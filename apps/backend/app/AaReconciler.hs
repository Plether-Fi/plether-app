module Main (main) where

import qualified Plether.Perps.Manifest as Manifest

import Control.Exception
  ( SomeException
  , displayException
  , fromException
  , throwIO
  , try
  )
import Control.Monad (unless, when)
import Data.Char (isAscii, isDigit)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Plether.AA.Kernel (KernelProfile (..))
import Plether.AA.KernelGateway (KernelSponsorshipConfig (..), kernelScope, loadKernelPublicConfig)
import Plether.AA.KernelReconciler (KernelReconcilerConfig (..), ensureKernelReconcilerSchema, runKernelReconciler)
import Plether.AA.Reconciler
  ( AaReconcilerConfig (..)
  , loadAaReconcilerConfig
  , runAaReconciler
  )
import Plether.Database (newDbPool, withDb)
import Plether.Database.AaSponsorship (ensureAaSponsorshipSchema)
import Plether.Database.KernelOperation (ensureKernelOperationSchema)
import Plether.Ethereum.Client (RpcClientOptions (..), newClient, newClientWithOptions, withoutRpcFallback)
import Plether.Config (normalizeExternalSecurityRpcUrl, resolveAaSecurityRpc, aaRpcModeText, AaRpcMode (..), parseCanonicalAddressList, validateAaSafeLag)
import Plether.Logging (field, logError, logInfo, logWarn)
import System.Environment (lookupEnv)
import System.Exit (ExitCode, exitFailure)
import Text.Read (readMaybe)

main :: IO ()
main = runMain `catchUnexpected` handleUnexpectedFailure

runMain :: IO ()
runMain = do
  Manifest.requireDeployedRelease
  kernel <- loadKernelPublicConfig >>= either (fatal "aa_reconciler_configuration_invalid") pure
  case kernel of
    Just (profile, Just sponsor, safeLag) -> runKernel profile sponsor safeLag
    Just (_, Nothing, _) -> fatal "aa_reconciler_configuration_invalid" "Kernel reconciliation requires AA_KERNEL_SPONSORSHIP_JSON"
    Nothing -> runSimpleAccount

runSimpleAccount :: IO ()
runSimpleAccount = do
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
  either (fatal "aa_reconciler_configuration_invalid" . T.pack) pure $ do
    owners <- parseCanonicalAddressList "AA_NATIVE_CANARY_OWNERS" ownersRaw
    validateAaSafeLag (arcChainId cfg) (globalRaw /= "false") owners (arcMaxSafeLagSeconds cfg)
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
  token <- fmap T.pack <$> lookupEnv "PERPS_RPC_AUTH_TOKEN"
  primaryClient <- withoutRpcFallback <$> newClientWithOptions (RpcClientOptions rpcUrl token "aa-reconciler-primary")
  secondaryClient <- if rpcMode == SingleProviderSepolia
    then pure primaryClient
    else withoutRpcFallback <$> newClient secondaryRpcUrl
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

runKernel :: KernelProfile -> KernelSponsorshipConfig -> Integer -> IO ()
runKernel profile sponsor safeLag = do
  legacyIdentity <- traverse lookupEnv
    ["AA_PAYMASTER_ADDRESS", "AA_PAYMASTER_CODE_HASH", "AA_RECONCILER_START_BLOCK", "AA_RECONCILER_START_BLOCK_HASH"]
  when (any (maybe False (not . T.null . T.strip . T.pack)) legacyIdentity) $
    fatal "aa_reconciler_configuration_invalid" "Kernel and SimpleAccount reconciler identities are mutually exclusive"
  unless (Manifest.releaseChainId == 421614) $
    fatal "aa_reconciler_configuration_invalid" "Kernel AA requires an Arbitrum Sepolia Perps release"
  databaseUrl <- requireEnv "DATABASE_URL"
  primaryRaw <- requireRawEnv "PERPS_RPC_URL"
  secondarySetting <- lookupEnv "AA_SECURITY_RPC_URL"
  secondaryRaw <- case secondarySetting of
    Just _ -> requireRawEnv "AA_SECURITY_RPC_URL"
    Nothing -> requireRawEnv "AA_RECONCILER_SECONDARY_RPC_URL"
  let invalid = fatal "aa_reconciler_configuration_invalid"
  primary <- maybe (invalid "PERPS_RPC_URL must be normalized HTTPS/default-443") pure $
    normalizeExternalSecurityRpcUrl primaryRaw
  (_, secondary) <- either (invalid . T.pack) pure $
    resolveAaSecurityRpc "dual-independent" 421614 "false" "" primaryRaw secondaryRaw
  let authority = T.takeWhile (/= '/') . T.drop 8
  unless (authority primary /= authority secondary) $
    invalid "Kernel AA requires primary and secondary RPC providers on distinct hosts"
  primaryAuth <- fmap T.pack <$> lookupEnv "PERPS_RPC_AUTH_TOKEN"
  poll <- kernelIntSetting "AA_KERNEL_RECONCILER_POLL_SECONDS" 5 1 60
  batch <- kernelIntSetting "AA_KERNEL_RECONCILER_BATCH_BLOCKS" 250 1 2000
  pauseAfter <- kernelIntSetting "AA_KERNEL_RECONCILER_FAILURE_PAUSE_SECONDS" 60 (max 5 poll) 300
  pool <- newDbPool databaseUrl
  schemaResult <- try @SomeException $ withDb pool $ \conn -> do
    ensureKernelOperationSchema conn
    ensureKernelReconcilerSchema conn
  either (const $ fatal "kernel_aa_reconciler_schema_invalid" "Could not validate the Kernel sponsorship schema")
    pure schemaResult
  primaryClient <- withoutRpcFallback <$> newClientWithOptions (RpcClientOptions primary primaryAuth "kernel-reconciler-primary")
  secondaryClient <- withoutRpcFallback <$> newClientWithOptions (RpcClientOptions secondary Nothing "kernel-reconciler-secondary")
  logInfo "kernel_aa_reconciler_started" "Kernel sponsorship reconciler started"
    [ field "chain_id" $ kpChainId profile
    , field "rpc_mode" $ aaRpcModeText DualIndependent
    , field "paymaster" $ kscPaymaster sponsor
    , field "start_block" $ kscDeploymentBlock sponsor
    , field "poll_seconds" poll
    , field "batch_blocks" batch
    , field "minimum_deposit_wei" $ kscMinDepositWei sponsor
    , field "failure_pause_seconds" pauseAfter
    , field "max_safe_lag_seconds" safeLag
    ]
  runKernelReconciler pool primaryClient secondaryClient KernelReconcilerConfig
    { krcScope = kernelScope profile
    , krcEntryPointCodeHash = kpEntryPointCodeHash profile
    , krcPaymaster = kscPaymaster sponsor
    , krcPaymasterCodeHash = kscPaymasterCodeHash sponsor
    , krcStartBlock = kscDeploymentBlock sponsor
    , krcStartBlockHash = kscDeploymentBlockHash sponsor
    , krcPollSeconds = fromInteger poll
    , krcBatchBlocks = batch
    , krcMinDepositWei = kscMinDepositWei sponsor
    , krcFailurePauseSeconds = pauseAfter
    , krcMaxSafeLagSeconds = safeLag
    }

kernelIntSetting :: String -> Integer -> Integer -> Integer -> IO Integer
kernelIntSetting name fallback lower upper = do
  raw <- fromMaybe (show fallback) <$> lookupEnv name
  case readMaybe raw of
    Just value | all (\char -> isAscii char && isDigit char) raw
      , show value == raw, value >= lower, value <= upper -> pure value
    _ -> fatal "aa_reconciler_configuration_invalid" $ T.pack name <> " is out of range or malformed"

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
