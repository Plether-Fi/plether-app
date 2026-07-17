module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, displayException, try)
import Control.Monad (forever)
import Plether.Config (Config (..), loadConfig)
import Plether.Database (newDbPool, withDb)
import Plether.Database.Insights (ensureInsightsSchema)
import Plether.Database.Schema (ensurePerpsHistorySchema)
import Plether.Ethereum.Client (newClient)
import Plether.Insights.SnapshotWorker (runInsightsSnapshotCycle)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

main :: IO ()
main = do
  eConfig <- loadConfig
  case eConfig of
    Left err -> putStrLn $ "Configuration error: " <> err
    Right cfg ->
      case cfgDatabaseUrl cfg of
        Nothing -> putStrLn "DATABASE_URL is required for plether-insights-worker"
        Just databaseUrl -> do
          pool <- newDbPool databaseUrl
          withDb pool ensurePerpsHistorySchema
          withDb pool $ \conn ->
            ensureInsightsSchema
              conn
              (cfgPerpsChainId cfg)
              (cfgPerpsOrderRouter cfg)
              (cfgPerpsUsdc cfg)
              (cfgPerpsMarginClearinghouse cfg)
          client <- newClient $ cfgPerpsRpcUrl cfg
          pollSeconds <- loadPollSeconds
          putStrLn $
            "Starting Insights snapshot worker every "
              <> show pollSeconds
              <> " seconds"
          forever $ do
            result <-
              try @SomeException $
                runInsightsSnapshotCycle client pool cfg
            case result of
              Left err ->
                putStrLn $
                  "Insights snapshot cycle failed: "
                    <> displayException err
              Right () -> pure ()
            threadDelay $ pollSeconds * 1_000_000

loadPollSeconds :: IO Int
loadPollSeconds = do
  configured <- lookupEnv "INSIGHTS_SNAPSHOT_POLL_SECONDS"
  pure $ max 10 $ maybe 60 id $ configured >>= readMaybe
