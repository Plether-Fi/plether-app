module Plether.AA.Diagnostics (Diagnostic(..), DiagnosticSink, startDiagnostics, enqueueDiagnostic, validAttemptId, readDiagnostic) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Exception (SomeException, SomeAsyncException, try, fromException, throwIO)
import Control.Monad (forever, void, unless, forM_)
import Data.Char (isHexDigit)
import Data.Text (Text)
import Data.Aeson (Value, object, (.=))
import qualified Data.Text as T
import Database.PostgreSQL.Simple (execute, query, query_, Only(..))
import Plether.Database (DbPool, withDb)
import Plether.Config (Config)
import Plether.Ethereum.Client (EthClient)
import Plether.AA.OrderDiagnostics (recoverOrderDiagnostics)
import Plether.Database.AaSponsorship (consumeAaRateLimit)
import Plether.Logging (field, logWarnEvery, logInfo)
import System.Timeout (timeout)

data Diagnostic = Diagnostic
  { diagnosticAttempt :: Text, diagnosticClient :: Text, diagnosticChain :: Integer
  , diagnosticDeployment :: Text, diagnosticPreparation :: Text, diagnosticHash :: Text
  , diagnosticSender :: Text
  }
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
        "UPDATE aa_attempt_diagnostics d SET stage=CASE WHEN a.state='expired' THEN 'authorization_expired' WHEN e.success=false THEN 'user_operation_reverted' ELSE 'user_operation_confirmed' END,reason=CASE WHEN a.state='expired' THEN 'AUTHORIZATION_EXPIRED' WHEN e.success=false THEN 'USER_OPERATION_REVERTED' ELSE d.reason END,terminal_at=CASE WHEN a.state='expired' OR e.success=false THEN COALESCE(d.terminal_at,clock_timestamp()) ELSE d.terminal_at END,updated_at=clock_timestamp() FROM aa_sponsorship_authorizations a LEFT JOIN aa_user_operation_events e ON e.user_operation_hash=a.expected_user_operation_hash WHERE d.operation_hash=a.expected_user_operation_hash AND d.client_key=a.client_key AND d.sender=a.sender AND d.order_id IS NULL AND d.stage='prepared' AND (a.state='expired' OR (a.state='settled' AND e.finalized_at IS NOT NULL)) RETURNING d.attempt_id::text,d.stage,d.reason" :: IO [(Text,Text,Maybe Text)]
      forM_ outcomes $ \(attempt,stage,reason) -> logInfo "aa_recovery_outcome" "Safely reconciled sponsored operation"
        [field "attempt_id" attempt, field "stage" stage, field "reason_code" reason]
    case result of
      Left (err :: SomeException) -> case fromException err :: Maybe SomeAsyncException of
        Just _ -> throwIO err
        Nothing -> dropped
      Right Nothing -> dropped
      Right (Just ()) -> pure ()
    threadDelay 10_000_000
  void $ forkIO $ forever $ do
    diagnostic <- atomically $ readTBQueue queue
    result <- try $ timeout 2_000_000 $ withDb pool $ \conn -> execute conn
      "INSERT INTO aa_attempt_diagnostics(attempt_id,client_key,chain_id,deployment,preparation_id,operation_hash,sender,stage) VALUES (?::uuid,?,?,?,?,?,?,'prepared') ON CONFLICT DO NOTHING"
      (diagnosticAttempt diagnostic, diagnosticClient diagnostic, diagnosticChain diagnostic,
        diagnosticDeployment diagnostic, diagnosticPreparation diagnostic, diagnosticHash diagnostic, diagnosticSender diagnostic)
    case result of
      Left (err :: SomeException) -> case fromException err :: Maybe SomeAsyncException of
        Just _ -> throwIO err
        Nothing -> dropped
      Right Nothing -> dropped
      Right (Just 0) -> pure ()
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
