module Main (main) where

import Control.Concurrent (forkIO)
import Control.Monad (when)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Plether.AA.Pimlico (newPimlicoProxyState)
import Plether.Api (app)
import Plether.Cache (newAppCache)
import Plether.Config (Config (..), loadConfig)
import Plether.Database (newDbPool, withDb)
import Plether.Database.Insights (ensureInsightsSchema)
import Plether.Database.Protocol (ensureProtocolSchema)
import Plether.Database.Schema (ensureBasketSnapshotSchema, ensurePerpsHistorySchema, ensureTestnetFaucetSchema)
import Plether.Ethereum.Client (newClient)
import Plether.Indexer (IndexerConfig (..), startIndexer)
import Plether.Logging (field, logError, logInfo, logWarn)
import Plether.Pyth.History (BasketIngestorConfig (..), startBasketHistoryIngestor)
import Plether.Protocol.Release (currentProtocolRelease)
import Plether.RequestLogging (newRequestLoggingMiddleware)
import Web.Scotty (middleware, scotty)

main :: IO ()
main = do
  eConfig <- loadConfig

  case eConfig of
    Left err ->
      logError
        "api_configuration_invalid"
        "API configuration is invalid"
        [field "error" err]
    Right cfg -> do
      manager <- newManager tlsManagerSettings
      mPool <- case cfgDatabaseUrl cfg of
        Just dbUrl -> do
          pool <- newDbPool dbUrl
          withDb pool ensureBasketSnapshotSchema
          withDb pool ensurePerpsHistorySchema
          withDb pool $ \conn -> ensureProtocolSchema conn (currentProtocolRelease cfg)
          withDb pool ensureTestnetFaucetSchema
          withDb pool $ \conn ->
            ensureInsightsSchema
              conn
              (cfgPerpsChainId cfg)
              (cfgPerpsOrderRouter cfg)
              (cfgPerpsUsdc cfg)
              (cfgPerpsMarginClearinghouse cfg)
              (cfgPerpsAccountLens cfg)
          logInfo
            "api_database_ready"
            "Database schemas are ready"
            [field "history_enabled" True]
          let indexerCfg = IndexerConfig
                { icRpcUrl = cfgRpcUrl cfg
                , icDeployments = cfgDeployments cfg
                , icStartBlock = cfgIndexerStartBlock cfg
                , icBatchSize = 10000
                , icPollInterval = 12000000
                }
          _ <- forkIO $ startIndexer manager pool indexerCfg
          when (cfgPythIngestionEnabled cfg) $ do
            let basketCfg = BasketIngestorConfig
                  { bicBenchmarksUrl = cfgPythBenchmarksUrl cfg
                  , bicApiKey = cfgPythApiKey cfg
                  , bicBackfillDays = cfgPythBackfillDays cfg
                  , bicSampleIntervalSeconds = cfgPythSampleIntervalSeconds cfg
                  , bicPollSeconds = 15 * 60
                  }
            logInfo
              "pyth_history_ingestor_started"
              "Pyth basket history ingestor started"
              [ field "backfill_days" $ bicBackfillDays basketCfg
              , field "sample_interval_seconds" $ bicSampleIntervalSeconds basketCfg
              , field "poll_seconds" $ bicPollSeconds basketCfg
              ]
            _ <- forkIO $ startBasketHistoryIngestor manager pool basketCfg
            pure ()
          pure $ Just pool
        Nothing -> do
          logWarn
            "api_database_disabled"
            "Database is not configured; history endpoints are disabled"
            [field "history_enabled" False]
          pure Nothing

      client <- newClient (cfgRpcUrl cfg)
      perpsClient <- newClient (cfgPerpsRpcUrl cfg)
      cache <- newAppCache
      pimlicoProxyState <- newPimlicoProxyState
      requestLogging <- newRequestLoggingMiddleware
      logInfo
        "api_started"
        "Plether API is accepting requests"
        [ field "port" $ cfgPort cfg
        , field "chain_id" $ cfgChainId cfg
        , field "perps_chain_id" $ cfgPerpsChainId cfg
        , field "history_enabled" $ maybe False (const True) mPool
        ]
      scotty (cfgPort cfg) $ do
        middleware requestLogging
        app cache client perpsClient cfg mPool manager pimlicoProxyState
