module Main (main) where

import Plether.Config (Config (..), loadConfig)
import Plether.Database (newDbPool, withDb)
import Plether.Database.Schema (ensureBasketSnapshotSchema, ensurePerpsKeeperSchema)
import Plether.Ethereum.Client (newClient)
import Plether.Ethereum.Transaction (deriveAddress)
import Plether.Keeper (KeeperMode (..), runKeeper)
import Plether.Logging (field, logError, logInfo)
import System.Environment (getArgs)
import System.Exit (exitFailure)

data KeeperArgs = KeeperArgs
  { kaMode :: KeeperMode
  , kaDryRun :: Bool
  }
  deriving (Show)

main :: IO ()
main = do
  args <- parseKeeperArgs <$> getArgs
  eConfig <- loadConfig
  case eConfig of
    Left err -> fatal $ "Configuration error: " <> err
    Right cfg -> do
      dbUrl <- require "DATABASE_URL is required for plether-keeper" (cfgDatabaseUrl cfg)
      privateKey <- require "KEEPER_PRIVATE_KEY is required for plether-keeper" (cfgKeeperPrivateKey cfg)
      addressResult <- deriveAddress privateKey
      case addressResult of
        Left err -> fatal $ "Invalid KEEPER_PRIVATE_KEY: " <> show err
        Right keeperAddress ->
          logInfo
            "keeper_started"
            "Perps keeper started"
            [ field "keeper_address" $ show keeperAddress
            , field "mode" $ show $ kaMode args
            , field "dry_run" $ kaDryRun args
            , field "poll_seconds" $ cfgKeeperPollSeconds cfg
            ]
      pool <- newDbPool dbUrl
      withDb pool $ \conn -> do
        ensureBasketSnapshotSchema conn
        ensurePerpsKeeperSchema conn
      client <- newClient (cfgPerpsRpcUrl cfg)
      runKeeper cfg pool client (kaMode args) (kaDryRun args)

parseKeeperArgs :: [String] -> KeeperArgs
parseKeeperArgs args =
  KeeperArgs
    { kaMode = if "--once" `elem` args then KeeperOnce else KeeperLoop
    , kaDryRun = "--dry-run" `elem` args
    }

require :: String -> Maybe a -> IO a
require message value =
  case value of
    Just found -> pure found
    Nothing -> fatal message

fatal :: String -> IO a
fatal message = do
  logError "keeper_fatal" "Keeper cannot start" [field "error" message]
  exitFailure
