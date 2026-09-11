-- Advisory observations only: never consumed by authorization or reconciliation.
module Plether.AA.Readiness (newReadiness, Check(..), snapshotValue, classifyFunding) where

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
import Plether.Perps.Release (perpsV2PublicLens)
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
  initial <- floor . (*1000) <$> getPOSIXTime
  cache <- newMVar $ snapshotValue initial False [Check "readiness" "unknown" "READINESS_UNAVAILABLE"] [] []
  case (cfgNativeAaConfig cfg, mPool) of
    (Just native, Just pool) -> do
      alto <- newClient $ naaAltoRpcUrl native
      void $ forkIO $ forever $ do
        tick <- getMonotonicTimeNSec
        started <- floor . (*1000) <$> getPOSIXTime
        observed <- timeout 5_000_000 $ observe $ do
          ((sponsor, worker), (bundler, oracle)) <- concurrently
            (concurrently (sponsorCheck native pool) (workerCheck pool))
            (concurrently (bundlerCheck alto) oracleCheck)
          let closes = case oracle of
                [Check "oracle" "ready" "READY"] -> oracle
                _ -> [Check "oracle" "unknown" "EXIT_MODE_REQUIRES_VALIDATION"]
          pure $ snapshotValue started enforced (bundler : sponsor) (worker : oracle) (worker : closes)
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
  workerCheck pool = do
    result <- observe (withDb pool $ \conn -> query conn
      "SELECT state,reason FROM aa_worker_readiness WHERE chain_id=? AND deployment=? AND component='keeper' AND observed_at > clock_timestamp()-interval '15 seconds'"
      (cfgPerpsChainId cfg, T.toLower $ cfgPerpsOrderRouter cfg) :: IO [(Text,Text)])
    pure $ case result of
      Just [(state, reason)] | state `elem` ["ready","blocked","unknown"] && reason `elem` ["READY","KEEPER_INSUFFICIENT_FUNDS","FUNDING_UNVERIFIED","FUNDING_LOW"] -> Check "keeper" state reason
      _ -> Check "keeper" "unknown" "WORKER_HEARTBEAT_STALE"
  bundlerCheck alto = do
    result <- rpcCall alto "eth_supportedEntryPoints" (toJSON ([] :: [Text]))
    pure $ case result of
      Right (Array entries) | String "0x4337084d9e255ff0702461cf8895ce9e3b5ff108" `elem` fmap lowerValue (V.toList entries) -> Check "bundler" "ready" "READY"
      _ -> Check "bundler" "unknown" "BUNDLER_UNAVAILABLE"
  lowerValue (String x) = String $ T.toLower x
  lowerValue x = x
  oracleCheck = do
    result <- rpcCall client "eth_call" $ toJSON
      [object ["to" .= perpsV2PublicLens, "data" .= ("0x5fd7f162" :: Text)], String "latest"]
    pure $ case result of
      Right (String value) | T.length value == 2 + 8 * 64 ->
        let active = T.take 64 $ T.drop (2 + 5 * 64) value
        in [if active == T.replicate 63 "0" <> "1" then Check "oracle" "ready" "READY" else Check "oracle" "blocked" "OPEN_EXECUTION_UNAVAILABLE"]
      _ -> [Check "oracle" "unknown" "ORACLE_UNAVAILABLE"]
