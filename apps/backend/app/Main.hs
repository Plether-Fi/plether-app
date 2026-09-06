module Main (main) where

import Control.Concurrent (forkIO)
import Control.Monad (when)
import qualified Data.Text as T
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Plether.AA.Pimlico (newPimlicoProxyState)
import Plether.Api (app)
import Plether.Cache (newAppCache)
import Plether.Config (Config (..), loadConfig)
import Plether.Database (newDbPool, withDb)
import Plether.Database.Insights (ensureInsightsSchema)
import Plether.Database.Protection (ensureProtectionSchema)
import Plether.Database.Schema (ensureBasketSnapshotSchema, ensurePerpsHistorySchema, ensureTestnetFaucetSchema)
import Plether.Database.VaultActivity (ensureVaultActivitySchema)
import Plether.Database.VaultPerformance (ensureVaultPerformanceSchema)
import Plether.Ethereum.Client (RpcClientOptions (..), newClientWithOptions)
import Plether.Handlers.InsightsRegistration (initializeInsightsRegistration)
import Plether.Handlers.TestnetFaucetGuard (newFaucetGuardState)
import Plether.Indexer (IndexerConfig (..), startIndexer)
import Plether.Insights.Registration.Cleanup (startRegistrationCleanup)
import Plether.Logging (field, logError, logInfo, logWarn)
import Plether.Pyth.History (BasketIngestorConfig (..), startBasketHistoryIngestor)
import Plether.Perps.Release (verifyPerpsV2ReleaseBindings)
import Plether.RequestLogging (newRequestLoggingMiddleware)
import Plether.Vaults.PerformanceIndexer
  ( VaultPerformanceIndexerConfig (..)
  , startVaultPerformanceIndexer
  )
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
      perpsClient <-
        newClientWithOptions $
          RpcClientOptions (cfgPerpsRpcUrl cfg) (cfgPerpsRpcAuthToken cfg) "api-perps"
      case (cfgPerpsChainId cfg, cfgPerpsOrderLifecycleBook cfg) of
        (421614, Just _) -> do
          releaseVerification <-
            verifyPerpsV2ReleaseBindings
              perpsClient
              (cfgPerpsChainId cfg)
              (cfgPerpsOrderRouter cfg)
              (cfgPerpsOrderLifecycleBook cfg)
              (cfgPerpsCfdEngine cfg)
              (cfgPerpsMarginClearinghouse cfg)
              (cfgPerpsHousePool cfg)
              (cfgPerpsIndexerStartBlock cfg)
          case releaseVerification of
            Left failure ->
              ioError $ userError $ "Bounded V2 release verification failed: " <> T.unpack failure
            Right blockNumber ->
              logInfo
                "perps_v2_release_verified"
                "Bounded V2 contract bindings and runtime hashes are verified"
                [ field "block_number" blockNumber
                , field "order_router" $ cfgPerpsOrderRouter cfg
                , field "order_lifecycle_book" $ cfgPerpsOrderLifecycleBook cfg
                ]
        _ -> pure ()
      mPool <- case cfgDatabaseUrl cfg of
        Just dbUrl -> do
          pool <- newDbPool dbUrl
          withDb pool ensureBasketSnapshotSchema
          withDb pool ensurePerpsHistorySchema
          withDb pool ensureProtectionSchema
          withDb pool ensureTestnetFaucetSchema
          withDb pool ensureVaultPerformanceSchema
          withDb pool ensureVaultActivitySchema
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
          registrationInitialization <- initializeInsightsRegistration pool perpsClient cfg
          either (ioError . userError) pure registrationInitialization
          case cfgRegistrationConfig cfg of
            Just _ -> do
              _ <- forkIO $ startRegistrationCleanup pool
              pure ()
            Nothing -> pure ()
          logInfo
            "api_database_ready"
            "Database schemas are ready"
            [field "history_enabled" True]
          let indexerCfg = IndexerConfig
                { icRpcUrl = cfgRpcUrl cfg
                , icRpcAuthToken = cfgRpcAuthToken cfg
                , icDeployments = cfgDeployments cfg
                , icStartBlock = cfgIndexerStartBlock cfg
                , icBatchSize = 10000
                , icPollInterval = 12000000
                }
          _ <- forkIO $ startIndexer manager pool indexerCfg
          let vaultHistoryCfg =
                VaultPerformanceIndexerConfig
                  { vpicChainId = cfgPerpsChainId cfg
                  , vpicAssetAddress = cfgPerpsUsdc cfg
                  , vpicHousePoolAddress = cfgVaultHistoryHousePoolAddress cfg
                  , vpicSeniorVaultAddress = cfgVaultHistorySeniorVaultAddress cfg
                  , vpicJuniorVaultAddress = cfgVaultHistoryJuniorVaultAddress cfg
                  , vpicDeploymentBlock = cfgVaultHistoryDeploymentBlock cfg
                  , vpicConfirmations = cfgVaultHistoryConfirmations cfg
                  }
          logInfo
            "vault_performance_indexer_started"
            "Vault performance indexer started"
            [ field "chain_id" $ vpicChainId vaultHistoryCfg
            , field "house_pool" $ vpicHousePoolAddress vaultHistoryCfg
            , field "deployment_block" $ vpicDeploymentBlock vaultHistoryCfg
            , field "confirmations" $ vpicConfirmations vaultHistoryCfg
            ]
          vaultIndexerClient <-
            newClientWithOptions $
              RpcClientOptions
                (cfgPerpsRpcUrl cfg)
                (cfgPerpsRpcAuthToken cfg)
                "vault-indexer"
          _ <-
            forkIO $
              startVaultPerformanceIndexer
                vaultIndexerClient
                vaultIndexerClient
                pool
                vaultHistoryCfg
          when (cfgPythIngestionEnabled cfg) $ do
            let basketCfg = BasketIngestorConfig
                  { bicBenchmarksUrl = cfgPythBenchmarksUrl cfg
                  , bicHistoryUrl = cfgPythHistoryUrl cfg
                  , bicApiKey = cfgPythApiKey cfg
                  , bicChainId = cfgPerpsChainId cfg
                  , bicBackfillDays = cfgPythBackfillDays cfg
                  , bicOwnHistoryTargets = False
                  , bicSampleIntervalSeconds = cfgPythSampleIntervalSeconds cfg
                  , bicPollSeconds = 15 * 60
                  , bicCandleWriteMode = cfgPerpsCandleWriteMode cfg
                  , bicCandleLatenessSeconds = cfgPerpsCandleLatenessSeconds cfg
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

      client <-
        newClientWithOptions $
          RpcClientOptions (cfgRpcUrl cfg) (cfgRpcAuthToken cfg) "api-core"
      cache <- newAppCache
      pimlicoProxyState <- newPimlicoProxyState
      faucetGuardState <- newFaucetGuardState
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
        app cache client perpsClient cfg mPool manager pimlicoProxyState faucetGuardState
