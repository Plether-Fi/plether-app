module Main (main) where

import Control.Exception (SomeException, catch, displayException, fromException, throwIO)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Plether.Config (Config (..), loadConfig)
import Plether.Database (newDbPool, withDb)
import Plether.Database.Schema
  ( ensureBasketSnapshotSchema
  , ensurePerpsLiquidationSchema
  , seedPerpsLiquidationCandidatesFromHistory
  )
import Plether.Ethereum.Client (newClient)
import Plether.Ethereum.Rpc (ethGetBalance)
import Plether.Ethereum.Transaction (deriveAddress)
import Plether.LiquidationWorker
  ( LiquidationWorkerConfig (..)
  , LiquidationWorkerMode (..)
  , checkLiveSignerBalance
  , runLiquidationWorker
  )
import Plether.Logging (field, logError, logInfo)
import System.Environment (getArgs, lookupEnv)
import System.Exit (ExitCode, exitFailure)
import Text.Read (readMaybe)

data LiquidationWorkerArgs = LiquidationWorkerArgs
  { lwaMode :: LiquidationWorkerMode
  , lwaDryRun :: Bool
  }
  deriving (Show)

main :: IO ()
main = runMain `catch` handleUnexpectedFailure

runMain :: IO ()
runMain = do
  args <- parseArgs <$> getArgs
  eConfig <- loadConfig
  case eConfig of
    Left err -> fatal $ "Configuration error: " <> err
    Right cfg -> do
      dbUrl <- require "DATABASE_URL is required for plether-liquidation-worker" (cfgDatabaseUrl cfg)
      privateKey <- requireEnv "LIQUIDATION_KEEPER_PRIVATE_KEY is required for plether-liquidation-worker" "LIQUIDATION_KEEPER_PRIVATE_KEY"
      addressResult <- deriveAddress privateKey
      workerAddress <- case addressResult of
        Left err -> fatal $ "Invalid LIQUIDATION_KEEPER_PRIVATE_KEY: " <> T.unpack err
        Right derivedAddress -> pure derivedAddress

      workerCfg <- loadWorkerConfig cfg (cfgPerpsCfdEngine cfg) privateKey
      client <- newClient (cfgPerpsRpcUrl cfg)
      balanceResult <-
        checkLiveSignerBalance
          (lwaDryRun args)
          (ethGetBalance client workerAddress)
      signerBalance <- case balanceResult of
        Left err ->
          fatal $
            "Liquidation signer "
              <> T.unpack workerAddress
              <> " is not ready: "
              <> T.unpack err
        Right balance -> pure balance
      logInfo
        "liquidation_worker_started"
        "Liquidation worker started"
        ( [ field "worker_address" workerAddress
          , field "mode" $ show $ lwaMode args
          , field "dry_run" $ lwaDryRun args
          , field "chain_id" $ lwcChainId workerCfg
          , field "order_router" $ lwcOrderRouter workerCfg
          , field "cfd_engine" $ lwcCfdEngine workerCfg
          , field "poll_seconds" $ lwcPollSeconds workerCfg
          , field "confirmations" $ lwcIndexerConfirmations workerCfg
          ]
            <> maybe [] (\balance -> [field "signer_balance_wei" $ show balance]) signerBalance
        )
      pool <- newDbPool dbUrl
      withDb pool $ \conn -> do
        ensureBasketSnapshotSchema conn
        ensurePerpsLiquidationSchema conn
        seedPerpsLiquidationCandidatesFromHistory
          conn
          (lwcChainId workerCfg)
          (lwcOrderRouter workerCfg)
          (lwcCfdEngine workerCfg)
      runLiquidationWorker workerCfg pool client (lwaMode args) (lwaDryRun args)

loadWorkerConfig :: Config -> Text -> Text -> IO LiquidationWorkerConfig
loadWorkerConfig cfg cfdEngine privateKey = do
  pollSeconds <- readEnv "LIQUIDATION_WORKER_POLL_SECONDS" 1
  scanBatchSize <- readEnv "LIQUIDATION_WORKER_SCAN_BATCH_SIZE" 100
  indexerStartBlock <- readEnv "LIQUIDATION_WORKER_START_BLOCK" (cfgPerpsIndexerStartBlock cfg)
  indexerConfirmations <- readEnv "LIQUIDATION_WORKER_CONFIRMATIONS" 1
  indexerBatchSize <- readEnv "LIQUIDATION_WORKER_INDEX_BATCH_SIZE" 5_000
  indexerOverlapBlocks <- readEnv "LIQUIDATION_WORKER_REORG_OVERLAP_BLOCKS" 12
  pendingReplacementSeconds <- readEnv "LIQUIDATION_WORKER_PENDING_REPLACEMENT_SECONDS" 120
  gasBufferBps <- readEnv "LIQUIDATION_WORKER_GAS_BUFFER_BPS" (cfgKeeperGasBufferBps cfg)
  feeBufferBps <- readEnv "LIQUIDATION_WORKER_FEE_BUFFER_BPS" (cfgKeeperFeeBufferBps cfg)
  pure
    LiquidationWorkerConfig
      { lwcChainId = cfgPerpsChainId cfg
      , lwcOrderRouter = cfgPerpsOrderRouter cfg
      , lwcPletherOracle = cfgPerpsPletherOracle cfg
      , lwcCfdEngine = cfdEngine
      , lwcPrivateKey = privateKey
      , lwcPollSeconds = max 1 pollSeconds
      , lwcScanBatchSize = max 1 scanBatchSize
      , lwcIndexerStartBlock = max 0 indexerStartBlock
      , lwcIndexerConfirmations = max 0 indexerConfirmations
      , lwcIndexerBatchSize = max 1 indexerBatchSize
      , lwcIndexerOverlapBlocks = max 0 indexerOverlapBlocks
      , lwcPendingReplacementSeconds = max 1 pendingReplacementSeconds
      , lwcGasBufferBps = max 0 gasBufferBps
      , lwcFeeBufferBps = max 0 feeBufferBps
      }

parseArgs :: [String] -> LiquidationWorkerArgs
parseArgs args =
  LiquidationWorkerArgs
    { lwaMode = if "--once" `elem` args then LiquidationWorkerOnce else LiquidationWorkerLoop
    , lwaDryRun = "--dry-run" `elem` args
    }

readEnv :: (Read a) => String -> a -> IO a
readEnv name fallback = do
  value <- lookupEnv name
  pure $ fromMaybe fallback (value >>= readMaybe)

requireEnv :: String -> String -> IO Text
requireEnv message name = do
  value <- lookupEnv name
  require message $ fmap T.pack value

require :: String -> Maybe a -> IO a
require message value =
  case value of
    Just found -> pure found
    Nothing -> fatal message

fatal :: String -> IO a
fatal message = do
  logError
    "liquidation_worker_fatal"
    "Liquidation worker cannot start"
    [field "error" message]
  exitFailure

handleUnexpectedFailure :: SomeException -> IO ()
handleUnexpectedFailure err =
  case fromException err :: Maybe ExitCode of
    Just _ -> throwIO err
    Nothing -> do
      logError
        "liquidation_worker_crashed"
        "Liquidation worker terminated after an unexpected exception"
        [field "error" $ displayException err]
      throwIO err
