module Main (main) where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, SomeAsyncException, try, fromException, throwIO)
import Control.Monad (unless, when)
import Data.Foldable (for_)
import Data.Maybe (isJust)
import qualified Data.Text as T
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Plether.AA.Gateway (nativeGatewayIssuanceError, newNativeGatewayState, initializeGatewayObservability)
import Plether.AA.KernelGateway (KernelGatewayConfig (kgcPrimaryRpcUrl), loadKernelGatewayConfig)
import Plether.AA.KernelReconciler (ensureKernelReconcilerSchema)
import Plether.AA.Pimlico (newPimlicoProxyState)
import Plether.Api (appWithKernelAa)
import Plether.Api.BackgroundWorkers (StartupPolicy (..), resolveStartupPolicy)
import Plether.Cache (newAppCache)
import Plether.Config (Config (..), loadConfig)
import Plether.Database (newDbPool, withDb)
import Plether.Database.Diagnostics (startDbDiagnostics)
import Plether.Database.AaSponsorship (ensureAaSponsorshipSchema)
import Plether.Database.KernelOperation (ensureKernelOperationSchema)
import Plether.Database.Insights (ensureInsightsSchema)
import Plether.Insights.DivisionsLive (ensureDivisionsLiveSchema)
import Plether.Database.Protection (ensureProtectionSchema)
import Plether.Database.Schema (ensureBasketSnapshotSchema, ensurePerpsHistorySchema, ensureTestnetFaucetSchema,ensurePerpsExecutionProgressSchema)
import Plether.Database.VaultActivity (ensureVaultActivitySchema)
import Plether.Database.VaultPerformance (ensureVaultPerformanceSchema)
import Plether.Ethereum.Client (RpcClientOptions (..), newClientWithOptions)
import Plether.Handlers.InsightsExecution (ensureTxSenderCacheTable)
import Plether.Handlers.InsightsRegistration (initializeInsightsRegistration)
import Plether.Handlers.DivisionsRegistration (ensureDivisionsSchema, loadDiscordConfig)
import Plether.Handlers.TestnetFaucetGuard (newFaucetGuardState)
import Plether.Database.TestnetFunding (ensureMarginFundingSchema, requireKernelMarginFundingSchema)
import qualified Plether.Passkeys.Names as PasskeyNames
import Plether.Handlers.TestnetMarginFunding (startMarginFundingProcessor)
import Plether.Indexer (IndexerConfig (..), startIndexer)
import Plether.Insights.Registration.Cleanup (startRegistrationCleanup)
import Plether.Logging (field, logError, logInfo, logWarn)
import Plether.Pyth.History (BasketIngestorConfig (..), startBasketHistoryIngestor)
import qualified Plether.Perps.Manifest as Manifest
import Plether.Perps.Release (verifyPerpsV3ReleaseBindings)
import Plether.RequestLogging (newRequestLoggingMiddleware)
import Plether.Server
  ( apiServerOptions
  , maxConcurrentForegroundRequests
  , maxConcurrentLongPollRequests
  , newConcurrencyLimitMiddleware
  )
import Plether.Vaults.PerformanceIndexer
  ( VaultPerformanceIndexerConfig (..)
  , startVaultPerformanceIndexer
  )
import System.Exit (exitFailure)
import System.Environment (lookupEnv)
import System.Timeout (timeout)
import VaultReturnWorker (startVaultReturnWorker)
import Web.Scotty (middleware, scottyOpts)

main :: IO ()
main = do
  eConfig <- loadConfig

  case eConfig of
    Left err ->
      logError
        "api_configuration_invalid"
        "API configuration is invalid"
        [field "error" err]
        >> exitFailure
    Right cfg -> do
      kernelAa <- loadKernelGatewayConfig >>= either (ioError . userError . T.unpack) pure
      canaryMode <- lookupEnv "KERNEL_CANARY_API_MODE"
      workersFlag <- lookupEnv "API_BACKGROUND_WORKERS_ENABLED"
      startupPolicy <- either (ioError . userError) pure $
        resolveStartupPolicy (isJust kernelAa) canaryMode workersFlag
      let canaryApiMode = spCanaryApiMode startupPolicy
          backgroundWorkersEnabled = spBackgroundWorkersEnabled startupPolicy
          genericSchemaEnabled = spGenericSchemaEnabled startupPolicy
      when (canaryApiMode && isJust (cfgRegistrationConfig cfg)) $
        ioError $ userError "Kernel canary API cannot initialize Insights registration against the shared database"
      when (canaryApiMode && cfgTestnetMarginFundingEnabled cfg) $
        ioError $ userError "Kernel canary API cannot enable testnet margin funding without its background processor"
      for_ kernelAa $ \_ -> do
        unless (Manifest.releaseIsDeployed && cfgPerpsChainId cfg == 421614) $
          ioError $ userError "Kernel AA requires verified Arbitrum Sepolia Perps release bindings"
        unless (isJust (cfgDatabaseUrl cfg)) $
          ioError $ userError "Kernel AA requires DATABASE_URL"
        when (isJust (cfgNativeAaConfig cfg) || isJust (cfgAaConfig cfg)) $
          ioError $ userError "Kernel AA requires a dedicated deployment without SimpleAccount AA"
      manager <- newManager tlsManagerSettings
      let perpsRpcUrl = maybe (cfgPerpsRpcUrl cfg) kgcPrimaryRpcUrl kernelAa
      perpsClient <-
        newClientWithOptions $
          RpcClientOptions perpsRpcUrl (cfgPerpsRpcAuthToken cfg) "api-perps"
      when Manifest.releaseIsDeployed $ do
        case (cfgPerpsChainId cfg, cfgPerpsOrderLifecycleBook cfg) of
          (421614, Just _) -> do
            releaseVerification <-
              verifyPerpsV3ReleaseBindings
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
          when Manifest.releaseIsDeployed $ do
            when genericSchemaEnabled $ do
              withDb pool ensureBasketSnapshotSchema
              withDb pool ensurePerpsHistorySchema
              progressSchema <- try $ timeout 2_000_000 $ withDb pool ensurePerpsExecutionProgressSchema
              case progressSchema of
                Right (Just ()) -> pure ()
                Left (err :: SomeException) | Just cancellation <- (fromException err :: Maybe SomeAsyncException) -> throwIO cancellation
                _ -> logWarn "execution_progress_schema_unavailable" "Optional execution progress schema unavailable; canonical history continues" []
              withDb pool ensureDivisionsLiveSchema
              withDb pool ensureProtectionSchema
              withDb pool ensureTestnetFaucetSchema
              withDb pool ensureMarginFundingSchema
              withDb pool PasskeyNames.ensureSchema
              withDb pool ensureVaultPerformanceSchema
              withDb pool ensureVaultActivitySchema
              withDb pool ensureAaSponsorshipSchema
              withDb pool ensureTxSenderCacheTable
            for_ kernelAa $ \_ -> withDb pool $ \conn -> do
              when canaryApiMode $ requireKernelMarginFundingSchema conn
              ensureKernelOperationSchema conn
              ensureKernelReconcilerSchema conn
            when genericSchemaEnabled $ do
              withDb pool $ \conn ->
                ensureInsightsSchema
                  conn
                  (cfgInsightsCompetitionRules cfg)
                  (cfgPerpsChainId cfg)
                  (cfgPerpsOrderRouter cfg)
                  (cfgPerpsSettlementToken cfg)
                  (cfgPerpsMarginClearinghouse cfg)
                  (cfgPerpsAccountLens cfg)
                  (cfgInsightsCompetitionReleaseManifest cfg)
              registrationInitialization <- initializeInsightsRegistration pool perpsClient cfg
              either (ioError . userError) pure registrationInitialization
          _ <- startDbDiagnostics pool dbUrl
          when (Manifest.releaseIsDeployed && backgroundWorkersEnabled) $ do
            case cfgRegistrationConfig cfg of
              Just _ -> do
                _ <- forkIO $ startRegistrationCleanup pool
                pure ()
              Nothing -> pure ()
          logInfo
            "api_database_ready"
            "Database schemas are ready"
            [ field "history_enabled" True
            , field "generic_schema_initialized" genericSchemaEnabled
            ]
          when backgroundWorkersEnabled $ do
            let indexerCfg = IndexerConfig
                  { icRpcUrl = cfgRpcUrl cfg
                  , icRpcAuthToken = cfgRpcAuthToken cfg
                  , icDeployments = cfgDeployments cfg
                  , icStartBlock = cfgIndexerStartBlock cfg
                  , icBatchSize = 10000
                  , icPollInterval = 12000000
                  }
            _ <- forkIO $ startIndexer manager pool indexerCfg
            pure ()
          when (Manifest.releaseIsDeployed && backgroundWorkersEnabled) $ do
            let vaultHistoryCfg =
                  VaultPerformanceIndexerConfig
                    { vpicChainId = cfgPerpsChainId cfg
                    , vpicAssetAddress = cfgPerpsSettlementToken cfg
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
            _ <- forkIO $ startVaultReturnWorker cfg
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

      case mPool of
        Just pool | Manifest.releaseIsDeployed && backgroundWorkersEnabled -> do
          _ <- forkIO $ startMarginFundingProcessor pool perpsClient cfg kernelAa
          pure ()
        _ -> pure ()

      client <-
        newClientWithOptions $
          RpcClientOptions (cfgRpcUrl cfg) (cfgRpcAuthToken cfg) "api-core"
      cache <- newAppCache
      pimlicoProxyState <- newPimlicoProxyState
      faucetGuardState <- newFaucetGuardState
      nativeGatewayState <- newNativeGatewayState manager cfg perpsClient
      when Manifest.releaseIsDeployed $
        initializeGatewayObservability nativeGatewayState cfg mPool perpsClient
      case nativeGatewayIssuanceError nativeGatewayState of
        Just _ ->
          logWarn
            "aa_native_issuance_unavailable"
            "Native AA startup attestation failed; affected methods remain fail-closed"
            [field "failure_class" ("startup-attestation" :: String)]
        Nothing -> pure ()
      concurrencyLimit <-
        newConcurrencyLimitMiddleware
          maxConcurrentForegroundRequests
          maxConcurrentLongPollRequests
      requestLogging <- newRequestLoggingMiddleware
      logInfo
        "api_started"
        "Plether API is accepting requests"
        [ field "port" $ cfgPort cfg
        , field "chain_id" $ cfgChainId cfg
        , field "perps_chain_id" $ cfgPerpsChainId cfg
        , field "history_enabled" $ maybe False (const True) mPool
        , field "kernel_canary_api_mode" canaryApiMode
        , field "background_workers_enabled" backgroundWorkersEnabled
        ]
      discordConfig <- if not Manifest.releaseIsDeployed || canaryApiMode then pure Nothing else case cfgRegistrationConfig cfg of
        Nothing -> do
          divisionsEnabled <- lookupEnv "INSIGHTS_DIVISIONS_REGISTRATION_ENABLED"
          when (divisionsEnabled == Just "true") $
            ioError $ userError "Divisions registration requires provisioned Insights registration configuration"
          pure Nothing
        Just registrationConfig -> do
          configured <- loadDiscordConfig registrationConfig
          either (ioError . userError) pure configured
      case (if Manifest.releaseIsDeployed && genericSchemaEnabled then mPool else Nothing, discordConfig) of
        (Just pool, _) -> withDb pool ensureDivisionsSchema
        (Nothing, Just _) -> ioError $ userError "Divisions registration requires DATABASE_URL"
        _ -> pure ()
      scottyOpts (apiServerOptions $ cfgPort cfg) $ do
        middleware concurrencyLimit
        middleware requestLogging
        appWithKernelAa kernelAa cache client perpsClient cfg mPool manager pimlicoProxyState faucetGuardState nativeGatewayState discordConfig
