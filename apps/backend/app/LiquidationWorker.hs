module Main (main) where

import Control.Exception (SomeException, catch, displayException, fromException, throwIO)
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
  , loadLiquidationWorkerConfig
  , runLiquidationWorker
  )
import Plether.Logging (field, logError, logInfo)
import System.Environment (getArgs, lookupEnv)
import System.Exit (ExitCode, exitFailure)

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

      workerCfg <- loadLiquidationWorkerConfig cfg privateKey
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

parseArgs :: [String] -> LiquidationWorkerArgs
parseArgs args =
  LiquidationWorkerArgs
    { lwaMode = if "--once" `elem` args then LiquidationWorkerOnce else LiquidationWorkerLoop
    , lwaDryRun = "--dry-run" `elem` args
    }

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
