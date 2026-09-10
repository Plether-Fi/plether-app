module Main (main) where

import Control.Exception
  ( SomeException
  , displayException
  , fromException
  , throwIO
  , try
  )
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Plether.AA.Reconciler
  ( AaReconcilerConfig (..)
  , loadAaReconcilerConfig
  , runAaReconciler
  )
import Plether.Database (newDbPool, withDb)
import Plether.Database.AaSponsorship (ensureAaSponsorshipSchema)
import Plether.Ethereum.Client (newClient)
import Plether.Config (normalizeExternalSecurityRpcUrl, resolveAaSecurityRpc, aaRpcModeText, AaRpcMode (..))
import Plether.Logging (field, logError, logInfo, logWarn)
import System.Environment (lookupEnv)
import System.Exit (ExitCode, exitFailure)

main :: IO ()
main = runMain `catchUnexpected` handleUnexpectedFailure

runMain :: IO ()
runMain = do
  configResult <- loadAaReconcilerConfig
  cfg <- case configResult of
    Left err -> fatal "aa_reconciler_configuration_invalid" err
    Right value -> pure value
  databaseUrl <- requireEnv "DATABASE_URL"
  rpcUrlRaw <- requireRawEnv "PERPS_RPC_URL"
  secondaryRpcUrlRaw <- requireRawEnv "AA_RECONCILER_SECONDARY_RPC_URL"
  rpcModeRaw <- fromMaybe "dual-independent" <$> lookupEnv "AA_RPC_MODE"
  globalRaw <- fromMaybe "false" <$> lookupEnv "AA_NATIVE_GLOBAL_ROLLOUT_ENABLED"
  ownersRaw <- fromMaybe "" <$> lookupEnv "AA_NATIVE_CANARY_OWNERS"
  rpcUrl <- maybe
    (fatal "aa_reconciler_configuration_invalid" "PERPS_RPC_URL must be a normalized HTTPS/default-443 external provider URL")
    pure
    (normalizeExternalSecurityRpcUrl rpcUrlRaw)
  (rpcMode, secondaryRpcUrl) <- either
    (fatal "aa_reconciler_configuration_invalid" . T.pack)
    pure
    (resolveAaSecurityRpc rpcModeRaw (arcChainId cfg) globalRaw ownersRaw rpcUrlRaw secondaryRpcUrlRaw)
  case rpcMode of
    SingleProviderSepolia -> logWarn "aa_single_provider_canary"
      "Sepolia canary uses one RPC provider; repeated reads are not independent verification"
      [field "rpc_mode" $ aaRpcModeText rpcMode]
    DualIndependent -> pure ()
  pool <- newDbPool databaseUrl
  schemaResult <- try @SomeException $ withDb pool ensureAaSponsorshipSchema
  case schemaResult of
    Left err ->
      fatal
        "aa_reconciler_schema_invalid"
        ("Could not validate the AA sponsorship schema: " <> T.pack (displayException err))
    Right () -> pure ()
  primaryClient <- newClient rpcUrl
  secondaryClient <- newClient secondaryRpcUrl
  logInfo
    "aa_reconciler_started"
    "AA sponsorship reconciler started"
    [ field "chain_id" $ arcChainId cfg
    , field "rpc_mode" $ aaRpcModeText rpcMode
    , field "paymaster" $ arcPaymaster cfg
    , field "start_block" $ arcStartBlock cfg
    , field "poll_seconds" $ arcPollSeconds cfg
    , field "batch_blocks" $ arcBatchBlocks cfg
    , field "minimum_deposit_wei" $ arcMinDepositWei cfg
    , field "failure_pause_seconds" $ arcFailurePauseSeconds cfg
    , field "max_safe_lag_seconds" $ arcMaxSafeLagSeconds cfg
    ]
  runAaReconciler pool primaryClient secondaryClient cfg

requireEnv :: String -> IO Text
requireEnv name = do
  value <- fmap (T.strip . T.pack) <$> lookupEnv name
  case value of
    Just configured | not (T.null configured) -> pure configured
    _ -> fatal "aa_reconciler_configuration_invalid" $ T.pack name <> " is required"

requireRawEnv :: String -> IO Text
requireRawEnv name = do
  value <- fmap T.pack <$> lookupEnv name
  case value of
    Just configured | not (T.null configured) -> pure configured
    _ -> fatal "aa_reconciler_configuration_invalid" $ T.pack name <> " is required"

fatal :: Text -> Text -> IO a
fatal eventName reason = do
  logError eventName "AA sponsorship reconciler cannot start" [field "error" reason]
  exitFailure

catchUnexpected :: IO a -> (SomeException -> IO a) -> IO a
catchUnexpected action handler = do
  result <- try action
  either handler pure result

handleUnexpectedFailure :: SomeException -> IO ()
handleUnexpectedFailure err =
  case fromException err :: Maybe ExitCode of
    Just _ -> throwIO err
    Nothing -> do
      logError
        "aa_reconciler_crashed"
        "AA sponsorship reconciler terminated after an unexpected exception"
        [field "error" $ displayException err]
      throwIO err
