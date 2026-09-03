module Main (main) where

import Control.Exception
  ( SomeAsyncException
  , SomeException
  , displayException
  , fromException
  , throwIO
  , try
  )
import Data.Text (Text)
import qualified Data.Text as T
import Plether.Config
  ( Config (..)
  , LpSettlementMode (..)
  , loadConfig
  , lpSettlementModeText
  )
import Plether.Database (newDbPool, withDb)
import Plether.Database.Schema (ensureBasketSnapshotSchema, ensurePerpsKeeperSchema)
import Plether.Ethereum.Client (RpcClientOptions (..), newClientWithOptions)
import Plether.Ethereum.Transaction (deriveAddress)
import Plether.Keeper
  ( KeeperMode (..)
  , auditLpSettlementStartup
  , runKeeper
  , runLpSettlementPreflight
  )
import Plether.Logging (field, logError, logInfo)
import System.Environment (getArgs)
import System.Exit (exitFailure)

data KeeperArgs = KeeperArgs
  { kaMode :: KeeperMode
  , kaDryRun :: Bool
  , kaLpSettlementPreflight :: Bool
  }
  deriving (Show)

main :: IO ()
main = do
  rawArgs <- getArgs
  args <- either fatal pure $ parseKeeperArgs rawArgs
  eConfig <- loadConfig
  case eConfig of
    Left err -> fatal $ "Configuration error: " <> err
    Right cfg -> do
      dbUrl <- require "DATABASE_URL is required for plether-keeper" (cfgDatabaseUrl cfg)
      pool <- newDbPool dbUrl
      client <-
        newClientWithOptions $
          RpcClientOptions (cfgPerpsRpcUrl cfg) (cfgPerpsRpcAuthToken cfg) "keeper"
      if kaLpSettlementPreflight args
        then do
          logInfo
            "lp_settlement_preflight_started"
            "LP settlement read-only preflight started"
            [ field "lp_settlement_mode" $ lpSettlementModeText $ cfgLpSettlementMode cfg
            , field "settlement_monitor" $ cfgPerpsSettlementMonitorLens cfg
            , field "house_pool" $ cfgPerpsHousePool cfg
            , field "senior_vault" $ cfgLpSettlementSeniorVault cfg
            , field "junior_vault" $ cfgLpSettlementJuniorVault cfg
            ]
          preflightResult <- trySynchronous $ runLpSettlementPreflight cfg pool client
          case preflightResult of
            Left err -> do
              logError
                "lp_settlement_preflight_failed"
                "LP settlement read-only preflight failed"
                [ field "lp_settlement_mode" $ lpSettlementModeText $ cfgLpSettlementMode cfg
                , field "error" $ displayException err
                ]
              exitFailure
            Right () ->
              logInfo
                "lp_settlement_preflight_succeeded"
                "LP settlement read-only preflight succeeded"
                [field "lp_settlement_mode" $ lpSettlementModeText $ cfgLpSettlementMode cfg]
        else do
          privateKey <- require "KEEPER_PRIVATE_KEY is required for plether-keeper" (cfgKeeperPrivateKey cfg)
          keeperAddress <- deriveRequiredAddress "KEEPER_PRIVATE_KEY" privateKey
          lpSettlementSigner <- deriveLpSettlementSigner cfg
          logInfo
            "keeper_started"
            "Perps keeper started"
            [ field "keeper_address" keeperAddress
            , field "mode" $ show $ kaMode args
            , field "dry_run" $ kaDryRun args
            , field "poll_seconds" $ cfgKeeperPollSeconds cfg
            , field "idle_poll_seconds" $ cfgKeeperIdlePollSeconds cfg
            , field "lp_settlement_mode" $ lpSettlementModeText $ cfgLpSettlementMode cfg
            , field "lp_settlement_signer" lpSettlementSigner
            , field "lp_settlement_poll_seconds" $ cfgLpSettlementPollSeconds cfg
            , field "settlement_monitor" $ cfgPerpsSettlementMonitorLens cfg
            ]
          withDb pool $ \conn -> do
            ensureBasketSnapshotSchema conn
            ensurePerpsKeeperSchema conn
          whenLpSettlementActive cfg $
            auditLpSettlementStartup cfg client
          runKeeper cfg pool client (kaMode args) (kaDryRun args)

parseKeeperArgs :: [String] -> Either String KeeperArgs
parseKeeperArgs args =
  case [arg | arg <- args, arg `notElem` supportedArgs] of
    unknown : _ -> Left $ "Unknown plether-keeper argument: " <> unknown
    []
      | preflight && (once || dryRun) ->
          Left "--lp-settlement-preflight cannot be combined with --once or --dry-run"
      | otherwise ->
          Right
            KeeperArgs
              { kaMode = if once then KeeperOnce else KeeperLoop
              , kaDryRun = dryRun
              , kaLpSettlementPreflight = preflight
              }
  where
    supportedArgs = ["--once", "--dry-run", "--lp-settlement-preflight"]
    once = "--once" `elem` args
    dryRun = "--dry-run" `elem` args
    preflight = "--lp-settlement-preflight" `elem` args

deriveLpSettlementSigner :: Config -> IO (Maybe Text)
deriveLpSettlementSigner cfg =
  case cfgLpSettlementMode cfg of
    LpSettlementOff -> pure Nothing
    LpSettlementObserve -> deriveActiveSigner
    LpSettlementExecute -> deriveActiveSigner
  where
    deriveActiveSigner = do
      privateKey <-
        require
          "LP_SETTLEMENT_PRIVATE_KEY is required when LP settlement is active"
          (cfgLpSettlementPrivateKey cfg)
      Just <$> deriveRequiredAddress "LP_SETTLEMENT_PRIVATE_KEY" privateKey

whenLpSettlementActive :: Config -> IO () -> IO ()
whenLpSettlementActive cfg action =
  case cfgLpSettlementMode cfg of
    LpSettlementOff -> pure ()
    LpSettlementObserve -> action
    LpSettlementExecute -> action

deriveRequiredAddress :: String -> Text -> IO Text
deriveRequiredAddress variableName privateKey = do
  addressResult <- deriveAddress privateKey
  case addressResult of
    Left err -> fatal $ "Invalid " <> variableName <> ": " <> T.unpack err
    Right address -> pure address

trySynchronous :: IO a -> IO (Either SomeException a)
trySynchronous action = do
  result <- try action
  case result of
    Left err ->
      case fromException err :: Maybe SomeAsyncException of
        Just _ -> throwIO err
        Nothing -> pure result
    Right _ -> pure result

require :: String -> Maybe a -> IO a
require message value =
  case value of
    Just found -> pure found
    Nothing -> fatal message

fatal :: String -> IO a
fatal message = do
  logError "keeper_fatal" "Keeper cannot start" [field "error" message]
  exitFailure
