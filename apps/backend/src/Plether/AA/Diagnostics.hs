module Plether.AA.Diagnostics (Diagnostic(..), DiagnosticSink, startDiagnostics, enqueueDiagnostic, validAttemptId, readDiagnostic, parseBrowserStage, BrowserFailure(..), recordBrowserStage, persistBrowserStage, persistAttemptStage) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Exception (SomeException, SomeAsyncException, try, fromException, throwIO)
import Control.Monad (forever, void, unless, when, forM_)
import Data.Char (isHexDigit)
import Data.Text (Text)
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Aeson (Value(..), object, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.Int (Int64)
import qualified Data.Text as T
import Database.PostgreSQL.Simple (Connection, execute, query, query_, Only(..))
import Plether.Database (DbPool, withDb)
import Plether.Config (Config)
import Plether.Ethereum.Client (EthClient)
import Plether.AA.OrderDiagnostics (recoverOrderDiagnostics)
import Plether.AA.ExecutionDiagnostics (recoverExecutionDiagnostics, gasUtilizationBps)
import Plether.Database.AaSponsorship (consumeAaRateLimit)
import Plether.Logging (field, logWarnEvery, logInfo)
import System.Timeout (timeout)

data Diagnostic = Diagnostic
  { diagnosticAttempt :: Text, diagnosticClient :: Text, diagnosticChain :: Integer
  , diagnosticDeployment :: Text, diagnosticPreparation :: Text, diagnosticHash :: Text
  , diagnosticSender :: Text
  }
  | RecoveryDiagnostic
    { diagnosticClient :: Text, diagnosticHash :: Text, diagnosticRecovered :: Bool }
  | SubmissionDiagnostic Text Text Text
    -- Client, exact operation hash, allowlisted backend stage.
type DiagnosticSink = TBQueue Diagnostic

validAttemptId :: Text -> Bool
validAttemptId value = case T.splitOn "-" value of
  [a,b,c,d,e] -> map T.length [a,b,c,d,e] == [8,4,4,4,12]
    && all (T.all isHexDigit) [a,b,c,d,e] && T.take 1 c == "4"
    && T.toLower (T.take 1 d) `elem` ["8","9","a","b"]
  _ -> False

startDiagnostics :: Config -> DbPool -> EthClient -> IO DiagnosticSink
startDiagnostics cfg pool client = do
  queue <- newTBQueueIO 256
  recoverySeen <- newIORef Map.empty
  void $ forkIO $ forever $ do
    result <- try $ timeout 20_000_000 $ recoverExecutionDiagnostics cfg pool client
    case result of
      Left (err :: SomeException) -> case fromException err :: Maybe SomeAsyncException of
        Just _ -> throwIO err
        Nothing -> dropped
      Right Nothing -> dropped
      Right (Just ()) -> pure ()
    threadDelay 10_000_000
  void $ forkIO $ forever $ do
    result <- try $ timeout 20_000_000 $ recoverOrderDiagnostics cfg pool client
    case result of
      Left (err :: SomeException) -> case fromException err :: Maybe SomeAsyncException of
        Just _ -> throwIO err
        Nothing -> dropped
      Right Nothing -> dropped
      Right (Just ()) -> pure ()
    threadDelay 10_000_000
  -- Recover durable references even if the API crashed before enqueueing, or
  -- the browser closed. Missing migration only disables this advisory exporter.
  void $ forkIO $ forever $ do
    result <- try $ timeout 2_000_000 $ withDb pool $ \conn -> do
      rows <- query_ conn
        "INSERT INTO aa_attempt_diagnostics(attempt_id,client_key,chain_id,deployment,preparation_id,operation_hash,sender,stage) SELECT p.diagnostic_attempt_id,p.client_key,p.diagnostic_chain_id,p.diagnostic_deployment,p.preparation_id,a.expected_user_operation_hash,p.sender,'prepared' FROM aa_preparations p JOIN aa_sponsorship_authorizations a ON a.digest=p.authorization_digest AND a.client_key=p.client_key AND a.sender=p.sender WHERE p.diagnostic_attempt_id IS NOT NULL AND p.diagnostic_chain_id IS NOT NULL AND p.diagnostic_deployment IS NOT NULL AND a.expected_user_operation_hash IS NOT NULL AND NOT EXISTS (SELECT 1 FROM aa_attempt_diagnostics d WHERE d.attempt_id=p.diagnostic_attempt_id) LIMIT 100 ON CONFLICT DO NOTHING RETURNING attempt_id::text" :: IO [Only Text]
      forM_ rows $ \(Only attempt) -> logInfo "aa_attempt_prepared" "Recovered prepared operation diagnostic"
        [field "attempt_id" attempt, field "stage" ("prepared" :: Text)]
      outcomes <- query_ conn
        "UPDATE aa_attempt_diagnostics d SET stage=CASE WHEN a.state='expired' THEN 'authorization_expired' WHEN e.success=false THEN 'user_operation_reverted' ELSE 'user_operation_confirmed' END,reason=CASE WHEN a.state='expired' THEN 'AUTHORIZATION_EXPIRED' WHEN e.success=false THEN 'USER_OPERATION_REVERTED' ELSE d.reason END,terminal_at=CASE WHEN a.state='expired' OR e.success=false THEN COALESCE(d.terminal_at,clock_timestamp()) ELSE d.terminal_at END,updated_at=clock_timestamp() FROM aa_sponsorship_authorizations a LEFT JOIN aa_user_operation_events e ON e.user_operation_hash=a.expected_user_operation_hash WHERE d.operation_hash=a.expected_user_operation_hash AND d.client_key=a.client_key AND d.sender=a.sender AND d.order_id IS NULL AND d.stage='prepared' AND (a.state='expired' OR (a.state='settled' AND e.finalized_at IS NOT NULL)) RETURNING d.attempt_id::text,d.stage,d.reason,a.operation,e.event_json" :: IO [(Text,Text,Maybe Text,Value,Maybe Value)]
      forM_ outcomes $ \(attempt,stage,reason,operation,event) -> logInfo "aa_recovery_outcome" "Safely reconciled sponsored operation"
        [field "attempt_id" attempt, field "stage" stage, field "reason_code" reason,
         field "gas_utilization_bps" $ event >>= gasUtilizationBps operation]
    case result of
      Left (err :: SomeException) -> case fromException err :: Maybe SomeAsyncException of
        Just _ -> throwIO err
        Nothing -> dropped
      Right Nothing -> dropped
      Right (Just ()) -> pure ()
    threadDelay 10_000_000
  void $ forkIO $ forever $ do
    diagnostic <- atomically $ readTBQueue queue
    result <- try $ timeout 2_000_000 $ withDb pool $ \conn -> case diagnostic of
      SubmissionDiagnostic clientKey operationHash stage ->
        persistAttemptStage conn clientKey operationHash "backend" stage
      RecoveryDiagnostic clientKey operationHash recovered -> do
        -- This lookup and export run off the recovery request's critical path.
        refs <- query conn "SELECT attempt_id::text FROM aa_attempt_diagnostics WHERE client_key=? AND operation_hash=? LIMIT 1"
          (clientKey, operationHash) :: IO [Only Text]
        now <- getPOSIXTime
        seen <- Map.filter (\timestamp -> now - timestamp < 60) <$> readIORef recoverySeen
        let reference = case refs of [Only ref] -> ref; _ -> clientKey <> ":" <> operationHash
            key = (reference,recovered)
        when (not (Map.member key seen) && Map.size seen >= 1024) dropped
        unless (Map.member key seen || Map.size seen >= 1024) $ do
          writeIORef recoverySeen $ Map.insert key now seen
          logInfo "aa_receipt_recovery" "Canonical recovery verification outcome"
            [field "attempt_id" $ case refs of [Only ref] -> Just ref; _ -> Nothing,
             field "stage" ("recovery" :: Text), field "recovery_source" ("finalized_record" :: Text),
             field "outcome" (if recovered then "confirmed" else "unknown" :: Text),
             field "reason_code" (if recovered then "RECOVERY_VERIFIED" else "RECOVERY_EVIDENCE_UNAVAILABLE" :: Text)]
        pure 0
      Diagnostic {} -> execute conn
        "INSERT INTO aa_attempt_diagnostics(attempt_id,client_key,chain_id,deployment,preparation_id,operation_hash,sender,stage) VALUES (?::uuid,?,?,?,?,?,?,'prepared') ON CONFLICT DO NOTHING"
        (diagnosticAttempt diagnostic, diagnosticClient diagnostic, diagnosticChain diagnostic,
          diagnosticDeployment diagnostic, diagnosticPreparation diagnostic, diagnosticHash diagnostic, diagnosticSender diagnostic)
    case result of
      Left (err :: SomeException) -> case fromException err :: Maybe SomeAsyncException of
        Just _ -> throwIO err
        Nothing -> dropped
      Right Nothing -> dropped
      Right (Just 0) -> pure ()
      Right (Just _) | SubmissionDiagnostic {} <- diagnostic -> pure ()
      Right (Just _) -> logInfo "aa_attempt_prepared" "Sponsored operation prepared"
        [field "attempt_id" $ diagnosticAttempt diagnostic, field "stage" ("prepared" :: Text)]
  pure queue
 where dropped = logWarnEvery 60 "aa_diagnostic_export_dropped" "Diagnostic persistence unavailable; authorization unaffected" []

enqueueDiagnostic :: DiagnosticSink -> Diagnostic -> IO ()
enqueueDiagnostic queue diagnostic = do
  accepted <- atomically $ do
    full <- isFullTBQueue queue
    if full then pure False else writeTBQueue queue diagnostic >> pure True
  unless accepted $ logWarnEvery 60 "aa_diagnostic_queue_full" "Diagnostic queue full; authorization unaffected" []

readDiagnostic :: DbPool -> Text -> Text -> IO Value
readDiagnostic pool client attempt = do
  result <- try $ timeout 1_000_000 $ withDb pool $ \conn -> do
    allowed <- consumeAaRateLimit conn "diagnostics" client client 120
    if not allowed then pure [] else query conn
      "SELECT stage,reason,(extract(epoch FROM updated_at)*1000)::bigint FROM aa_attempt_diagnostics WHERE client_key=? AND attempt_id=?::uuid"
      (client, attempt)
  rows <- case result of
    Right (Just found) -> pure found
    Right Nothing -> pure []
    Left (err :: SomeException) -> case fromException err :: Maybe SomeAsyncException of
      Just _ -> throwIO err
      Nothing -> pure []
  pure $ case rows of
    [(stage :: Text,reason :: Maybe Text,observed :: Integer)] -> object ["version" .= (1 :: Int), "stage" .= stage, "reason" .= reason,
      "observedAt" .= observed, "provenance" .= ("backend_observation" :: Text)]
    _ -> object ["version" .= (1 :: Int), "stage" .= ("unavailable" :: Text)]

-- Advisory reports cannot authorize recovery or overwrite canonical outcomes.
-- Old clients can omit details. New details must be a complete allowlisted pair.
data BrowserFailure = BrowserFailure { failureStep :: Text, failureReason :: Text }
  deriving stock (Eq, Show)

parseBrowserStage :: Value -> Maybe (Text, Text, Maybe BrowserFailure)
parseBrowserStage (Object fields)
  | Just (String attempt) <- KM.lookup "attemptId" fields
  , Just (String stage) <- KM.lookup "stage" fields
  , validAttemptId attempt, stage `elem` browserStages =
      if KM.size fields == 2 then Just (T.toLower attempt, stage, Nothing)
      else case (KM.size fields, KM.lookup "failureStep" fields, KM.lookup "reasonCode" fields) of
        (4, Just (String step), Just (String reason))
          | validBrowserFailure stage (BrowserFailure step reason) ->
              Just (T.toLower attempt, stage, Just $ BrowserFailure step reason)
        _ -> Nothing
parseBrowserStage _ = Nothing

validBrowserFailure :: Text -> BrowserFailure -> Bool
validBrowserFailure stage (BrowserFailure step reason) =
  stage `elem` ["execution_interrupted", "deadline_elapsed"]
    && step `elem` failureSteps && reason `elem` failureReasons

failureSteps, failureReasons :: [Text]
failureSteps =
  [ "preflight", "review_read", "preparation_journal", "review_clock"
  , "review_deadline", "recovery_check", "sponsorship", "prepared_payload_check"
  , "pre_sign_journal", "readiness_check", "review_revalidation", "signing_clock"
  , "signing_deadline", "wallet_approval", "signed_payload_check", "signed_journal"
  , "submission_clock", "submission_deadline", "submission_journal", "submission"
  , "confirmation"
  ]
failureReasons =
  [ "UNKNOWN", "REQUEST_ABORTED", "REQUEST_TIMEOUT", "WALLET_DECLINED"
  , "WALLET_DISCONNECTED", "NETWORK_ERROR", "REVIEW_CHANGED", "PREPARED_PAYLOAD_CHANGED"
  , "PREPARATION_UNUSABLE", "OPERATION_STORE_UNAVAILABLE", "INVALID_ORDER_DEADLINE", "DEADLINE_TOO_CLOSE"
  , "SPONSOR_UNAVAILABLE", "SPONSOR_REQUEST_TIMEOUT", "RATE_LIMITED", "SPONSOR_BUDGET_EXCEEDED"
  , "SIMULATION_FAILED", "POLICY_DENIED", "PAYMASTER_PAUSED", "ACCOUNT_NOT_TRUSTED"
  , "ACCOUNT_DEPLOYMENT_PENDING", "INSUFFICIENT_FREE_EQUITY", "MUST_CLOSE_OPPOSING", "EXECUTION_GAS_CAP_EXCEEDED"
  , "RESTART_ESTIMATION", "SECURITY_ATTESTATION_UNAVAILABLE", "SUBMISSION_PAUSED", "SPONSORSHIP_NOT_AUTHORIZED"
  , "DATABASE_UNAVAILABLE", "BUNDLER_UNAVAILABLE", "SUBMISSION_OUTCOME_UNKNOWN", "SUBMISSION_HASH_MISMATCH"
  , "RECEIPT_TIMEOUT", "USER_OPERATION_REVERTED", "READINESS_UNAVAILABLE", "OPEN_EXECUTION_UNAVAILABLE"
  , "PROTECTION_TRIGGER_UNAVAILABLE"
  ]

browserStages :: [Text]
browserStages = ["wallet_requested", "wallet_approved", "wallet_declined", "wallet_interrupted",
  "signed_operation_saved", "submission_requested", "submission_acknowledged", "submission_failed",
  "deadline_elapsed", "execution_interrupted", "safe_expiry_verified"]

backendStages :: [Text]
backendStages = ["submission_received", "rate_limited", "security_rejected", "identity_rejected",
  "policy_rejected", "runtime_rejected", "submission_paused", "authorization_rejected",
  "submission_journal_failed", "submission_journaled", "deadline_elapsed", "bundler_forwarded",
  "bundler_unavailable", "bundler_rejected", "bundler_acknowledged", "bundler_hash_mismatch"]

persistAttemptStage :: Connection -> Text -> Text -> Text -> Text -> IO Int64
persistAttemptStage conn client reference source stage
  | source == "browser" && stage `elem` browserStages && validAttemptId reference = execute conn
      "INSERT INTO aa_attempt_events(attempt_id,source,stage) SELECT diagnostic_attempt_id,?,? FROM aa_preparations WHERE diagnostic_attempt_id=?::uuid AND client_key=? ON CONFLICT DO NOTHING"
      (source, stage, reference, client)
  | source == "backend" && stage `elem` backendStages = execute conn
      "INSERT INTO aa_attempt_events(attempt_id,source,stage) SELECT p.diagnostic_attempt_id,?,? FROM aa_preparations p JOIN aa_sponsorship_authorizations a ON a.digest=p.authorization_digest WHERE a.expected_user_operation_hash=? AND a.client_key=? AND p.diagnostic_attempt_id IS NOT NULL ON CONFLICT DO NOTHING"
      (source, stage, reference, client)
  | otherwise = pure 0

-- Keep the first observation per stage, just like the existing timeline. Never
-- overwrite an earlier browser report or a canonical backend diagnostic.
persistBrowserStage :: Connection -> Text -> Text -> Text -> Maybe BrowserFailure -> IO Int64
persistBrowserStage conn client attempt stage Nothing = persistAttemptStage conn client attempt "browser" stage
persistBrowserStage conn client attempt stage (Just details@(BrowserFailure step reason))
  | validAttemptId attempt && validBrowserFailure stage details = execute conn
      "INSERT INTO aa_attempt_events(attempt_id,source,stage,failure_step,reason_code) SELECT diagnostic_attempt_id,'browser',?,?,? FROM aa_preparations WHERE diagnostic_attempt_id=?::uuid AND client_key=? ON CONFLICT DO NOTHING"
      (stage, step, reason, attempt, client)
  | otherwise = pure 0

recordBrowserStage :: DbPool -> Text -> Text -> Text -> Maybe BrowserFailure -> IO ()
recordBrowserStage pool client attempt stage details = do
  result <- try $ timeout 500_000 $ withDb pool $ \conn -> do
    allowed <- consumeAaRateLimit conn "attempt-events" client client 120
    when allowed $ do
      inserted <- persistBrowserStage conn client attempt stage details
      when (inserted > 0) $ forM_ details $ \(BrowserFailure step reason) ->
        logInfo "aa_browser_attempt_failure" "Browser reported an interrupted operation; advisory only"
          [field "attempt_id" attempt, field "stage" stage, field "failure_step" step,
           field "reason_code" reason, field "failure_source" ("browser" :: Text)]
  case result of
    Left (err :: SomeException) -> case fromException err :: Maybe SomeAsyncException of
      Just _ -> throwIO err
      Nothing -> dropped
    Right Nothing -> dropped
    Right (Just ()) -> pure ()
 where dropped = logWarnEvery 60 "aa_attempt_event_dropped" "Attempt timeline unavailable; trading unaffected" []
