-- Advisory observations only: never consumed by authorization or reconciliation.
module Plether.AA.Readiness (newReadiness, Check(..), snapshotValue, classifyFunding, aggregateFunding, snapshotWithWorkers) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async (concurrently)
import Control.Concurrent.MVar
import Control.Exception (SomeException, SomeAsyncException, try, fromException, throwIO)
import Control.Monad (forever, void)
import Data.Aeson (Value(..), object, toJSON, (.=))
import qualified Data.ByteString as BS
import qualified Data.Vector as V
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.Clock (getMonotonicTimeNSec)
import Database.PostgreSQL.Simple (query)
import Plether.Config (Config(..), NativeAaConfig(..))
import Plether.Database (DbPool, withDb)
import Plether.Database.AaSponsorship (getAaIssuancePause, aaReconcilerIsFresh)
import Plether.Ethereum.Client (EthClient, CallParams(..), newClient, rpcCall, ethCall)
import Plether.Ethereum.Abi (encodeCall, decodeUint256)
import Plether.AA.OracleReadiness (oracleReadiness)
import System.Environment (lookupEnv)
import System.Timeout (timeout)

data Check = Check Text Text Text deriving stock (Eq, Show)
checkValue :: Check -> Value
checkValue (Check component state reason) = object ["component" .= component, "status" .= state, "reason" .= reason]

classifyFunding :: Integer -> Integer -> Integer -> Check
classifyFunding balance liability maximumCost
  | balance < 0 || liability < 0 || maximumCost <= 0 = Check "funding" "unknown" "FUNDING_UNVERIFIED"
  | balance - liability < maximumCost = Check "funding" "blocked" "KEEPER_INSUFFICIENT_FUNDS"
  | balance - liability < 10 * maximumCost = Check "funding" "ready" "FUNDING_LOW"
  | otherwise = Check "funding" "ready" "READY"

snapshotValue :: Integer -> Bool -> [Check] -> [Check] -> [Check] -> Value
snapshotValue observed enforced common opens closes = object
  ["version" .= (1 :: Int), "observedAt" .= observed, "expiresAt" .= (observed + 15000)
  ,"enforcementEnabled" .= enforced, "actions" .= object
    ["deposit" .= map checkValue common, "open" .= map checkValue (common ++ opens)
    ,"close" .= map checkValue (common ++ closes), "protection" .= map checkValue (common ++ closes)]]

-- Background dependencies are visible but never universal trading blockers.
-- Oracle updater funding is monitor-only: the keeper pays the execution-price
-- update itself, including frozen closes. LP/liquidation are not exit gates.
snapshotWithWorkers :: Integer -> Bool -> [Check] -> Check -> (Check,Check,Check) -> [Check] -> Value
snapshotWithWorkers observed enforced common keeper (opens,closes,protection) workers = object
  ["version" .= (1 :: Int), "observedAt" .= observed, "expiresAt" .= (observed+15000)
  ,"enforcementEnabled" .= enforced, "workers" .= map checkValue workers
  ,"actions" .= object ["deposit" .= checks []
    ,"open" .= checks (keeper : opens : funding "keeper")
    ,"close" .= checks (keeper : closes : funding "keeper")
    ,"protection" .= checks (map advisory (keeper : protection : funding "keeper" ++ funding "protection"))]]
 where
  funding component = filter (\(Check c _ _) -> c == component) workers
  checks extra = map checkValue $ common ++ funding "alto" ++ extra
  advisory (Check c "blocked" r) = Check c "unknown" r
  advisory c = c

aggregateFunding :: Text -> [(Text,Text)] -> Check
aggregateFunding component rows
  | null rows || any invalid rows = Check component "unknown" "FUNDING_UNVERIFIED"
  | all ((=="blocked") . fst) rows = Check component "blocked" "WORKER_INSUFFICIENT_FUNDS"
  | any ((=="blocked") . fst) rows = Check component "unknown" "FUNDING_LOW"
  | (state,reason):_ <- filter ((=="unknown") . fst) rows = Check component state reason
  | any ((=="FUNDING_LOW") . snd) rows = Check component "ready" "FUNDING_LOW"
  | otherwise = Check component "ready" "READY"
 where invalid (s,r) = (s,r) `notElem` [("ready","READY"),("ready","FUNDING_LOW"),("unknown","FUNDING_LOW"),("unknown","FUNDING_UNVERIFIED"),("blocked","WORKER_INSUFFICIENT_FUNDS")]

observe :: IO a -> IO (Maybe a)
observe action = do
  result <- try action
  case result of
    Right value -> pure $ Just value
    Left (err :: SomeException) -> case fromException err :: Maybe SomeAsyncException of
      Just _ -> throwIO err
      Nothing -> pure Nothing

newReadiness :: Config -> Maybe DbPool -> EthClient -> Bool -> IO (IO Value)
newReadiness cfg mPool client startupAvailable = do
  enforced <- (== Just "true") <$> lookupEnv "AA_READINESS_ENFORCEMENT_ENABLED"
  -- Public categorical configuration only; no signer secrets enter the API.
  inventory <- maybe [] (filter (`elem` ["alto","keeper","oracle","liquidation","protection","lp_settlement"]) . T.splitOn "," . T.pack) <$> lookupEnv "AA_FUNDING_COMPONENTS"
  inventoryId <- maybe "" T.pack <$> lookupEnv "AA_FUNDING_INVENTORY_ID"
  initial <- floor . (*1000) <$> getPOSIXTime
  cache <- newMVar $ snapshotValue initial False [Check "readiness" "unknown" "READINESS_UNAVAILABLE"] [] []
  case (cfgNativeAaConfig cfg, mPool) of
    (Just native, Just pool) -> do
      alto <- newClient $ naaAltoRpcUrl native
      void $ forkIO $ forever $ do
        tick <- getMonotonicTimeNSec
        started <- floor . (*1000) <$> getPOSIXTime
        observed <- timeout 5_000_000 $ observe $ do
          (((sponsor, worker), (bundler, oracle)), workers) <- concurrently
            (concurrently
            (concurrently (sponsorCheck native pool) (workerCheck ("keeper" `elem` inventory) pool))
            (concurrently (bundlerCheck alto) (oracleReadiness cfg pool client)))
            (fundingChecks inventory inventoryId pool)
          let ((os,orr),(cs,cr),(ps,pr)) = oracle
          pure $ snapshotWithWorkers started enforced (bundler : sponsor) worker (Check "oracle" os orr, Check "oracle" cs cr, Check "oracle" ps pr) workers
        let result = case observed of
              Just (Just value) -> value
              _ -> snapshotValue started False [Check "readiness" "unknown" "READINESS_UNAVAILABLE"] [] []
        modifyMVar_ cache $ const $ pure result
        end <- getMonotonicTimeNSec
        threadDelay $ max 0 $ 10_000_000 - fromIntegral ((end - tick) `div` 1000)
      pure $ readMVar cache
    _ -> pure $ readMVar cache
 where
  sponsorCheck native pool = do
    (result, onchain) <- concurrently
      (observe $ withDb pool $ \conn -> do
        pause <- getAaIssuancePause conn
        fresh <- aaReconcilerIsFresh conn native
        pure (pause, fresh))
      (ethCall client $ CallParams (naaPaymasterAddress native) (encodeCall "paused()" []))
    let enabled = naaSponsorshipEnabled native && naaSubmissionEnabled native && startupAvailable
        paused = case onchain of
          Right bytes | BS.length bytes == 32 && decodeUint256 bytes == 1 -> Check "paymaster" "blocked" "PAYMASTER_PAUSED"
          Right bytes | BS.length bytes == 32 && decodeUint256 bytes == 0 -> Check "paymaster" "ready" "READY"
          _ -> Check "paymaster" "unknown" "READINESS_UNAVAILABLE"
    pure $ [paused, Check "sponsorship" (if enabled then "ready" else "blocked") (if enabled then "READY" else "SPONSORSHIP_DISABLED")]
      ++ case result of
        Just (Just _, _) -> [Check "sponsorship" "blocked" "PAYMASTER_PAUSED"]
        Just (Nothing, False) -> [Check "reconciliation" "blocked" "RECONCILIATION_STALE"]
        Just (Nothing, True) -> [Check "reconciliation" "ready" "READY"]
        Nothing -> [Check "reconciliation" "unknown" "READINESS_UNAVAILABLE"]
  workerCheck fundingMonitored pool = do
    result <- observe (withDb pool $ \conn -> query conn
      "SELECT state,reason FROM aa_worker_readiness WHERE chain_id=? AND deployment=? AND component='keeper' AND observed_at > clock_timestamp()-interval '15 seconds'"
      (cfgPerpsChainId cfg, T.toLower $ cfgPerpsOrderRouter cfg) :: IO [(Text,Text)])
    pure $ case result of
      Just [(state, reason)] | state `elem` ["ready","blocked","unknown"] && reason `elem` ["READY","KEEPER_INSUFFICIENT_FUNDS","FUNDING_UNVERIFIED","FUNDING_LOW"] ->
        -- The worker row still establishes liveness. The shared observer owns
        -- funding once configured and understands journal/pending liabilities.
        if fundingMonitored then Check "keeper" "ready" "READY" else Check "keeper" state reason
      _ -> Check "keeper" "unknown" "WORKER_HEARTBEAT_STALE"
  bundlerCheck alto = do
    result <- rpcCall alto "eth_supportedEntryPoints" (toJSON ([] :: [Text]))
    pure $ case result of
      Right (Array entries) | String "0x4337084d9e255ff0702461cf8895ce9e3b5ff108" `elem` fmap lowerValue (V.toList entries) -> Check "bundler" "ready" "READY"
      _ -> Check "bundler" "unknown" "BUNDLER_UNAVAILABLE"
  lowerValue (String x) = String $ T.toLower x
  lowerValue x = x
  fundingChecks [] _ _ = pure []
  fundingChecks inventory inventoryId pool = do
    result <- observe (withDb pool $ \conn -> query conn
      "SELECT component,CASE WHEN observed_at > clock_timestamp()-interval '15 seconds' AND observed_at <= clock_timestamp() THEN state ELSE 'unknown' END,CASE WHEN observed_at > clock_timestamp()-interval '15 seconds' AND observed_at <= clock_timestamp() THEN reason ELSE 'FUNDING_UNVERIFIED' END FROM aa_funding_observations WHERE chain_id=? AND deployment=? AND inventory_id=? LIMIT 17"
      (cfgPerpsChainId cfg,T.toLower $ cfgPerpsOrderRouter cfg,inventoryId) :: IO [(Text,Text,Text)])
    pure [aggregateFunding component $ case result of
      Just rows | length rows <= 16 -> [(s,r) | (c,s,r) <- rows, c == component]
      _ -> [] | component <- inventory]
