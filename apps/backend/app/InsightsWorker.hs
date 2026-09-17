module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, SomeAsyncException, fromException, throwIO, try)
import Database.PostgreSQL.Simple (SqlError (..))
import qualified Data.Text.Encoding as TextEncoding
import qualified Plether.Logging as Log
import Control.Monad (forever)
import Plether.Config (Config (..), loadConfig)
import Plether.Database (newDbPool, withDb)
import Plether.Database.Insights (ensureInsightsSchema)
import Plether.Database.Schema (ensurePerpsHistorySchema, ensureTestnetFaucetSchema)
import Plether.Ethereum.Client (RpcClientOptions (..), newClientWithOptions)
import Plether.Insights.SnapshotWorker
  ( parseSnapshotMulticallSize
  , runInsightsSnapshotCycle
  )
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

main :: IO ()
main = do
  eConfig <- loadConfig
  case eConfig of
    Left err -> putStrLn $ "Configuration error: " <> err
    Right cfg -> do
      configuredMulticallSize <- lookupEnv "INSIGHTS_SNAPSHOT_MULTICALL_SIZE"
      case parseSnapshotMulticallSize configuredMulticallSize of
        Left err -> putStrLn $ "Configuration error: " <> err
        Right multicallSize ->
          case cfgDatabaseUrl cfg of
            Nothing -> putStrLn "DATABASE_URL is required for plether-insights-worker"
            Just databaseUrl -> do
              pool <- newDbPool databaseUrl
              withDb pool ensurePerpsHistorySchema
              withDb pool ensureTestnetFaucetSchema
              withDb pool $ \conn ->
                ensureInsightsSchema
                  conn
                  (cfgInsightsCompetitionRules cfg)
                  (cfgPerpsChainId cfg)
                  (cfgPerpsOrderRouter cfg)
                  (cfgPerpsUsdc cfg)
                  (cfgPerpsMarginClearinghouse cfg)
                  (cfgPerpsAccountLens cfg)
                  (cfgInsightsCompetitionReleaseManifest cfg)
              client <-
                newClientWithOptions $
                  RpcClientOptions (cfgPerpsRpcUrl cfg) (cfgPerpsRpcAuthToken cfg) "insights-worker"
              pollSeconds <- loadPollSeconds
              integrityEnabled <- (/= Just "false") <$> lookupEnv "INSIGHTS_INTEGRITY_REFRESH_ENABLED"
              putStrLn $
                "Starting Insights snapshot worker every "
                  <> show pollSeconds
                  <> " seconds using "
                  <> captureModeDescription multicallSize
              forever $ do
                result <-
                  try @SomeException $
                    runInsightsSnapshotCycle client pool cfg multicallSize (2 * pollSeconds) integrityEnabled
                case result of
                  Left err -> case fromException err :: Maybe SomeAsyncException of
                    Just _ -> throwIO err
                    Nothing -> Log.logWarn "insights_snapshot_cycle_failed" "Insights snapshot cycle failed"
                      [Log.field "sql_state" (case fromException err :: Maybe SqlError of
                        Just sqlError -> TextEncoding.decodeUtf8 $ sqlState sqlError
                        Nothing -> "not_database")]
                  Right () -> pure ()
                threadDelay $ pollSeconds * 1_000_000

loadPollSeconds :: IO Int
loadPollSeconds = do
  configured <- lookupEnv "INSIGHTS_SNAPSHOT_POLL_SECONDS"
  pure $ max 10 $ maybe 60 id $ configured >>= readMaybe

captureModeDescription :: Int -> String
captureModeDescription 0 = "direct account-lens calls"
captureModeDescription size = "Multicall3 chunks of " <> show size
