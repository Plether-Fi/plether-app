module Plether.Protocol.ReleaseSpec (spec) where

import Control.Monad (filterM, forM_)
import Data.Aeson (Value (..), eitherDecodeFileStrict, withObject, (.:))
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (Parser, parseEither)
import Data.List (find, nub, (\\))
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import Paths_plether_api (getDataFileName)
import Plether.Config
  ( Config (..)
  , LpSettlementMode (..)
  , PerpsCandleReadMode (..)
  , PerpsCandleWriteMode (..)
  )
import Plether.Insights.Competition
  ( CompetitionReleaseManifest (..)
  , july2026Competition
  )
import Plether.Protocol.Governance
import Plether.Protocol.Parameters (ParameterDefinition (..), parameterCatalog)
import Plether.Protocol.Release
import Plether.Utils.Address (isValidAddress)
import System.Directory (doesFileExist, getCurrentDirectory)
import System.FilePath ((</>))
import Test.Hspec

spec :: Spec
spec = do
  describe "knownProtocolReleases" $ do
    it "pins the current Arbitrum Sepolia deployment metadata" $ do
      let release = currentSepoliaRelease

      prId release `shouldBe` "arbitrum-sepolia-2026-08-v1.2.0"
      prName release `shouldBe` "Plether Perps — August 2026 (v1.2.0)"
      prChainId release `shouldBe` 421614
      prDeploymentBlock release `shouldBe` 302257125
      prCalculationVersion release `shouldBe` "protocol-transparency-v1"
      prUsdc release `shouldBe` "0x1647e41f49ED6D688936092B5a291c4B28106343"
      prOrderLifecycleBook release
        `shouldBe` Just "0xa210928a7E0AE27626B8d0E67Bbd82305438aB9E"
      prCfdEngineLens release
        `shouldBe` "0x140067daAdd28bE4b04e649EEaCf6F5ECbEe8C79"
      prCfdEngineSettlementSidecar release
        `shouldBe` "0x288F70eC7cF0e16ae4FE4b91B5c266B047c83aFF"
      protocolReleaseId 421614 `shouldBe` prId release

    it "pins every monitored contract address for the current release" $ do
      releaseAddresses currentSepoliaRelease
        `shouldBe`
          [ "0x97A901dE2B267c307E264FD5F71403F8072F73e7"
          , "0xa210928a7E0AE27626B8d0E67Bbd82305438aB9E"
          , "0x3d0e430D670D74988C1B3e76b6ef018e79ab1E37"
          , "0x3dc9C0A1f9C745A4B08BD5C2E6c7aE613561c20D"
          , "0x140067daAdd28bE4b04e649EEaCf6F5ECbEe8C79"
          , "0x288F70eC7cF0e16ae4FE4b91B5c266B047c83aFF"
          , "0xda1240c36f3a4ddcAB3028F66B15Dfe91702dE2A"
          , "0x2f98787F6dCC3b1f2E4a2AFa5acf410159b9F211"
          , "0xC41e92F541cCF19FA203a96CecF3Ae4D2Ed7F60A"
          , "0x429DA61a7a616DeDD84d2a51eB6Dc1bD72427dC1"
          , "0x86939a377A78EDe8EEe5445765ac77c9016E35E2"
          , "0xB5A9a9d634197B8F0EA7c4042CF8d5701767D710"
          , "0xdf306B52eaC722D5994E2cc93D2818F391d68Adb"
          , "0xC69ec16EfB71F62984E9b2688396F34062277FdC"
          ]

    it "preserves the July deployment as a historical release" $ do
      prId julySepoliaRelease `shouldBe` "arbitrum-sepolia-2026-07"
      prName julySepoliaRelease `shouldBe` "Plether Perps — July 2026"
      prDeploymentBlock julySepoliaRelease `shouldBe` 288439939
      prUsdc julySepoliaRelease `shouldBe` "0xB15503d70B0eAa644dc6650d2A248762F7c5bCE3"
      prOrderRouter julySepoliaRelease
        `shouldBe` "0x04E3103752f623fBcDcD01f588590Af4c53E4c1E"
      prOrderLifecycleBook julySepoliaRelease `shouldBe` Nothing
      prCfdEngineLens julySepoliaRelease
        `shouldBe` "0xa9aa4097874e9622eaabee68f65ff5e3757728c5"
      prCfdEngineSettlementSidecar julySepoliaRelease
        `shouldBe` "0x0b652c4d4610234e221403076c116292f935b424"

    it "contains only complete, unique contract addresses in every release" $
      forM_ knownProtocolReleases $ \release -> do
        let addresses = prUsdc release : releaseAddresses release
            normalized = map T.toLower addresses

        addresses `shouldSatisfy` all isValidAddress
        normalized `shouldSatisfy` all (/= zeroAddress)
        length (nub normalized) `shouldBe` length normalized

    it "publishes only explicit non-secret operational addresses and labels permissionless roles" $ do
      let wallets = prOperationalWallets julySepoliaRelease
      prOperationalWallets currentSepoliaRelease `shouldBe` []
      map powRole wallets
        `shouldBe` ["oracle_updater", "order_keeper"]
      map powAddress wallets
        `shouldSatisfy` all isValidAddress
      map (T.toLower . powAddress) wallets
        `shouldSatisfy` all (/= zeroAddress)
      map powDescription wallets
        `shouldSatisfy` all (T.isInfixOf "permissionless")
      map (poweSelector . powRepresentativeEvidence) wallets
        `shouldBe` ["0x2efdaf14", "0xc700abdc"]
      map (poweBlockNumber . powRepresentativeEvidence) wallets
        `shouldBe` [292710937, 292710334]
      map
        (T.length . poweTransactionHash . powRepresentativeEvidence)
        wallets
        `shouldSatisfy` all (== 66)

    it "keeps every packaged release in parity with the compiled release table" $ do
      manifestPath <- resolveReleaseManifestPath
      decoded <- eitherDecodeFileStrict manifestPath
      case decoded >>= parseEither parseManifestReleases of
        Left decodeError ->
          expectationFailure $
            "Could not decode packaged protocol release manifest: " <> decodeError
        Right (currentReleaseId, manifestReleases) -> do
          let compiledCurrentRelease = currentProtocolRelease baseConfig
          currentReleaseId `shouldBe` prId compiledCurrentRelease
          manifestReleases
            `shouldBe` map protocolReleaseToJson knownProtocolReleases

    it "keeps every release ABI in parity with parameter and governance schema reads" $ do
      manifestPath <- resolveReleaseManifestPath
      decoded <- eitherDecodeFileStrict manifestPath
      case decoded >>= parseEither parseManifestAnalytics of
        Left decodeError ->
          expectationFailure $
            "Could not decode packaged protocol analytics ABI: " <> decodeError
        Right releaseAnalytics -> do
          let requiredReads =
                nub $
                  map pdGetter parameterCatalog
                    <> [ governanceFunctionSignature $ ggdFunction definition
                       | role <- [minBound .. maxBound]
                       , definition <- governanceRoleGetters role
                       ]
                    <> [ governanceFunctionSignature function
                       | definition <- governanceCategoryDefinitions
                       , function <-
                          [ gcdPendingGetter definition
                          , gcdActivationGetter definition
                          ]
                       ]
              requiredEvents =
                nub $
                  ledgerEventSignatures
                    <> [ gedSignature eventDefinition
                  | definition <- governanceCategoryDefinitions
                  , eventDefinition <- gcdEvents definition
                  ]
                    <> map gredSignature governanceRoleEvents

          map (\(releaseId, _, _) -> releaseId) releaseAnalytics
            `shouldBe` map prId knownProtocolReleases
          forM_ releaseAnalytics $ \(releaseId, manifestReads, manifestEvents) -> do
            requiredReads \\ manifestReads `shouldBe` []
            requiredEvents \\ manifestEvents `shouldBe` []
            if releaseId == prId currentSepoliaRelease
              then v2LifecycleEventSignatures \\ manifestEvents `shouldBe` []
              else pure ()
            "PoolConfigCancelled()" `elem` manifestEvents `shouldBe` False
            length (nub manifestReads) `shouldBe` length manifestReads
            length (nub manifestEvents) `shouldBe` length manifestEvents

  describe "currentProtocolRelease" $ do
    it "resolves the known release case-insensitively by chain and router" $ do
      let release =
            currentProtocolRelease
              baseConfig
                { cfgPerpsOrderRouter =
                    T.toLower (prOrderRouter currentSepoliaRelease)
                , cfgPerpsOrderLifecycleBook =
                    T.toLower <$> prOrderLifecycleBook currentSepoliaRelease
                , cfgPerpsIndexerStartBlock = 1
                }

      release `shouldBe` currentSepoliaRelease

    it "does not reuse the known release when the configured router differs" $ do
      let release =
            currentProtocolRelease
              baseConfig
                { cfgPerpsOrderRouter = alternateRouter
                , cfgPerpsIndexerStartBlock = 123
                }

      prId release `shouldBe` "chain-421614-block-123"
      prId release `shouldNotBe` prId currentSepoliaRelease
      prOrderRouter release `shouldBe` alternateRouter
      prDeploymentBlock release `shouldBe` 123

    it "does not reuse the known release when an auxiliary contract differs" $ do
      let alternatePublicLens =
            "0x2222222222222222222222222222222222222222"
          release =
            currentProtocolRelease
              baseConfig
                { cfgPerpsPublicLens = alternatePublicLens
                , cfgPerpsIndexerStartBlock = 123
                }

      prId release `shouldBe` "chain-421614-block-123"
      prPublicLens release `shouldBe` alternatePublicLens

    it "does not reuse the known release when the lifecycle book differs" $ do
      let alternateLifecycleBook =
            "0x3333333333333333333333333333333333333333"
          release =
            currentProtocolRelease
              baseConfig
                { cfgPerpsOrderLifecycleBook = Just alternateLifecycleBook
                , cfgPerpsIndexerStartBlock = 123
                }

      prId release `shouldBe` "chain-421614-block-123"
      prOrderLifecycleBook release `shouldBe` Just alternateLifecycleBook

    it "does not reuse the known release when an engine telemetry contract differs" $ do
      let alternateEngineLens =
            "0x4444444444444444444444444444444444444444"
          alternateSettlementSidecar =
            "0x5555555555555555555555555555555555555555"
          lensRelease =
            currentProtocolRelease
              baseConfig
                { cfgPerpsCfdEngineLens = alternateEngineLens
                , cfgPerpsIndexerStartBlock = 123
                }
          sidecarRelease =
            currentProtocolRelease
              baseConfig
                { cfgPerpsCfdEngineSettlementSidecar = alternateSettlementSidecar
                , cfgPerpsIndexerStartBlock = 124
                }

      prId lensRelease `shouldBe` "chain-421614-block-123"
      prCfdEngineLens lensRelease `shouldBe` alternateEngineLens
      prId sidecarRelease `shouldBe` "chain-421614-block-124"
      prCfdEngineSettlementSidecar sidecarRelease
        `shouldBe` alternateSettlementSidecar

    it "gives fallback releases distinct IDs across chains and deployment blocks" $ do
      let first =
            currentProtocolRelease
              baseConfig
                { cfgPerpsOrderRouter = alternateRouter
                , cfgPerpsIndexerStartBlock = 123
                }
          later =
            currentProtocolRelease
              baseConfig
                { cfgPerpsOrderRouter = alternateRouter
                , cfgPerpsIndexerStartBlock = 124
                }
          otherChain =
            currentProtocolRelease
              baseConfig
                { cfgPerpsChainId = 42161
                , cfgPerpsOrderRouter = alternateRouter
                , cfgPerpsIndexerStartBlock = 123
                }

      map prId [first, later, otherChain]
        `shouldSatisfy` \releaseIds ->
          length (nub releaseIds) == length releaseIds

  describe "protocolReleaseById" $ do
    it "resolves current and historical checked-in releases on the configured RPC chain" $ do
      protocolReleaseById baseConfig (prId currentSepoliaRelease)
        `shouldBe` Just currentSepoliaRelease
      protocolReleaseById baseConfig (prId julySepoliaRelease)
        `shouldBe` Just julySepoliaRelease

    it "resolves a configured fallback release without aliasing it to a manifest release" $ do
      let fallbackConfig =
            baseConfig
              { cfgPerpsOrderRouter = alternateRouter
              , cfgPerpsIndexerStartBlock = 123
              }
          fallback = currentProtocolRelease fallbackConfig

      protocolReleaseById fallbackConfig (prId fallback)
        `shouldBe` Just fallback
      protocolReleaseById fallbackConfig (prId currentSepoliaRelease)
        `shouldBe` Just currentSepoliaRelease
      protocolReleaseById fallbackConfig (prId julySepoliaRelease)
        `shouldBe` Just julySepoliaRelease

    it "rejects unknown release IDs" $
      protocolReleaseById baseConfig "unknown-release"
        `shouldBe` Nothing

parseManifestReleases :: Value -> Parser (Text, [Value])
parseManifestReleases =
  withObject "ProtocolReleaseManifest" $ \manifest -> do
    currentReleaseId <- manifest .: "currentReleaseId"
    releases <- manifest .: "releases" :: Parser [Value]
    case find (hasReleaseId currentReleaseId) releases of
      Nothing -> fail "currentReleaseId does not identify an entry in releases"
      Just _ -> do
        manifestReleases <- traverse withoutAnalyticsAbi releases
        pure (currentReleaseId, manifestReleases)
 where
  hasReleaseId releaseId (Object release) =
    KM.lookup "releaseId" release == Just (String releaseId)
  hasReleaseId _ _ = False
  withoutAnalyticsAbi (Object release) =
    pure $ Object $ KM.delete "analyticsAbi" release
  withoutAnalyticsAbi _ = fail "release entry is not an object"

parseManifestAnalytics :: Value -> Parser [(Text, [Text], [Text])]
parseManifestAnalytics =
  withObject "ProtocolReleaseManifest" $ \manifest -> do
    releases <- manifest .: "releases" :: Parser [Value]
    traverse (withObject "ProtocolRelease" parseRelease) releases
 where
  parseRelease release = do
    releaseId <- release .: "releaseId"
    analytics <- release .: "analyticsAbi"
    (manifestReads, manifestEvents) <-
      withObject "ProtocolAnalyticsAbi" parseAnalytics analytics
    pure (releaseId, manifestReads, manifestEvents)
  parseAnalytics analytics =
    (,) <$> analytics .: "reads" <*> analytics .: "events"

resolveReleaseManifestPath :: IO FilePath
resolveReleaseManifestPath = do
  packaged <- getDataFileName "config/protocol-releases.json"
  cwd <- getCurrentDirectory
  existing <-
    filterM
      doesFileExist
      [ packaged
      , cwd </> "config" </> "protocol-releases.json"
      , cwd </> "apps" </> "backend" </> "config" </> "protocol-releases.json"
      ]
  pure $ case existing of
    path : _ -> path
    [] -> packaged

currentSepoliaRelease :: ProtocolRelease
currentSepoliaRelease =
  fromJust $
    find
      ((== "arbitrum-sepolia-2026-08-v1.2.0") . prId)
      knownProtocolReleases

julySepoliaRelease :: ProtocolRelease
julySepoliaRelease =
  fromJust $
    find
      ((== "arbitrum-sepolia-2026-07") . prId)
      knownProtocolReleases

releaseAddresses :: ProtocolRelease -> [Text]
releaseAddresses release =
  [prOrderRouter release]
    <> maybe [] pure (prOrderLifecycleBook release)
    <> [ prOrderRouterAdmin release
       , prCfdEngine release
       , prCfdEngineLens release
       , prCfdEngineSettlementSidecar release
       , prCfdEngineAdmin release
       , prMarginClearinghouse release
       , prPublicLens release
       , prAccountLens release
       , prHousePool release
       , prSeniorVault release
       , prJuniorVault release
       , prPletherOracle release
       ]

ledgerEventSignatures :: [Text]
ledgerEventSignatures =
  [ "OrderCommitted(uint64,address,uint8)"
  , "OrderExecuted(uint64,uint256)"
  , "OrderFailed(uint64,uint8)"
  , "PositionOpened(address,uint8,uint256,uint256,uint256)"
  , "PositionClosed(address,uint8,uint256,uint256,int256)"
  , "PositionLiquidated(address,uint8,uint256,uint256,uint256)"
  , "MarginAdded(address,uint256)"
  , "Deposit(address,address,uint256)"
  , "Withdraw(address,address,uint256)"
  , "Deposit(address,address,uint256,uint256)"
  , "Withdraw(address,address,address,uint256,uint256)"
  , "Transfer(address,address,uint256)"
  ]

v2LifecycleEventSignatures :: [Text]
v2LifecycleEventSignatures =
  [ "IntentRegistered(uint64,address,bytes32,bytes32,uint256,(bytes32,uint8,uint256,uint256,uint256,bool,(uint64,uint8,bytes32,uint256,uint256,uint256,uint256,uint256,uint256,uint256,uint256,uint32)))"
  , "OrderFinalized(uint64,address,bytes32,bytes32,uint64,uint64,(uint64,address,bytes32,bytes32,bytes32,bytes32,uint8,uint8,uint8,address,uint8,uint256,uint256,uint256,uint64,bool,uint256,address,uint8,(bytes4,uint8,uint8,uint8,uint256,uint256,bytes32),(uint256,int256,int256,int256,uint256,uint256,uint256,uint256,uint256,uint256,uint256,uint256,uint256,uint256,uint256,int256,uint256)))"
  ]

zeroAddress :: Text
zeroAddress = "0x0000000000000000000000000000000000000000"

alternateRouter :: Text
alternateRouter = "0x1111111111111111111111111111111111111111"

baseConfig :: Config
baseConfig =
  Config
    { cfgRpcUrl = "https://eth-sepolia.example"
    , cfgChainId = 11155111
    , cfgPort = 3001
    , cfgCorsOrigins = []
    , cfgDeployments = []
    , cfgDatabaseUrl = Nothing
    , cfgIndexerStartBlock = 0
    , cfgPythBenchmarksUrl = "https://benchmarks.pyth.network"
    , cfgPythHistoryUrl = "https://history.pyth.example"
    , cfgPythHermesUrl = "https://hermes.pyth.network"
    , cfgPythApiKey = Nothing
    , cfgPythBackfillDays = 7
    , cfgPythSampleIntervalSeconds = 60
    , cfgPythLatestMaxAgeSeconds = 10
    , cfgPythIngestionEnabled = False
    , cfgProtocolExplorerEnabled = True
    , cfgPerpsCandleWriteMode = PerpsCandleWritesOff
    , cfgPerpsCandleReadMode = PerpsCandleReadsLegacy
    , cfgPerpsCandleReadIntervals = []
    , cfgPerpsCandleShadowSampleBps = 0
    , cfgPerpsCandleStrictCoverage = True
    , cfgPerpsCandleLatenessSeconds = 120
    , cfgPerpsCandleFinalizationGraceSeconds = 15
    , cfgPerpsRpcUrl = "https://arb-sepolia.example"
    , cfgPerpsChainId = 421614
    , cfgPerpsUsdc = "0x1647e41f49ED6D688936092B5a291c4B28106343"
    , cfgPerpsOrderRouter = "0x97A901dE2B267c307E264FD5F71403F8072F73e7"
    , cfgPerpsOrderLifecycleBook = Just "0xa210928a7E0AE27626B8d0E67Bbd82305438aB9E"
    , cfgPerpsCfdEngine = "0x3dc9C0A1f9C745A4B08BD5C2E6c7aE613561c20D"
    , cfgPerpsCfdEngineLens = "0x140067daAdd28bE4b04e649EEaCf6F5ECbEe8C79"
    , cfgPerpsCfdEngineSettlementSidecar = "0x288F70eC7cF0e16ae4FE4b91B5c266B047c83aFF"
    , cfgPerpsMarginClearinghouse = "0x2f98787F6dCC3b1f2E4a2AFa5acf410159b9F211"
    , cfgPerpsPletherOracle = "0xC69ec16EfB71F62984E9b2688396F34062277FdC"
    , cfgPerpsAccountLens = "0x429DA61a7a616DeDD84d2a51eB6Dc1bD72427dC1"
    , cfgPerpsPublicLens = "0xC41e92F541cCF19FA203a96CecF3Ae4D2Ed7F60A"
    , cfgPerpsHousePool = "0x86939a377A78EDe8EEe5445765ac77c9016E35E2"
    , cfgPerpsSeniorVault = "0xB5A9a9d634197B8F0EA7c4042CF8d5701767D710"
    , cfgPerpsJuniorVault = "0xdf306B52eaC722D5994E2cc93D2818F391d68Adb"
    , cfgPerpsOrderRouterAdmin = "0x3d0e430D670D74988C1B3e76b6ef018e79ab1E37"
    , cfgPerpsCfdEngineAdmin = "0xda1240c36f3a4ddcAB3028F66B15Dfe91702dE2A"
    , cfgPerpsSettlementMonitorLens = "0xd251AC0BD90780c48F31F575152808315200664E"
    , cfgPerpsIndexerStartBlock = 302257125
    , cfgVaultHistoryHousePoolAddress = "0x86939a377A78EDe8EEe5445765ac77c9016E35E2"
    , cfgVaultHistorySeniorVaultAddress = "0xB5A9a9d634197B8F0EA7c4042CF8d5701767D710"
    , cfgVaultHistoryJuniorVaultAddress = "0xdf306B52eaC722D5994E2cc93D2818F391d68Adb"
    , cfgVaultHistoryDeploymentBlock = 302257125
    , cfgVaultHistoryRpcUrl = "https://archive.example"
    , cfgVaultHistoryConfirmations = 12
    , cfgInsightsCompetitionRules = july2026Competition
    , cfgInsightsCompetitionReleaseManifest = currentCompetitionReleaseManifest
    , cfgRegistrationConfig = Nothing
    , cfgAaConfig = Nothing
    , cfgFaucetGuardConfig = Nothing
    , cfgFaucetPrivateKey = Nothing
    , cfgKeeperPrivateKey = Nothing
    , cfgKeeperPollSeconds = 1
    , cfgKeeperMaxBatchSize = 20
    , cfgKeeperConfirmations = 1
    , cfgKeeperGasBufferBps = 2000
    , cfgKeeperFeeBufferBps = 2500
    , cfgLpSettlementMode = LpSettlementOff
    , cfgLpSettlementPrivateKey = Nothing
    , cfgLpSettlementSeniorVault = "0xB5A9a9d634197B8F0EA7c4042CF8d5701767D710"
    , cfgLpSettlementJuniorVault = "0xdf306B52eaC722D5994E2cc93D2818F391d68Adb"
    , cfgLpSettlementPollSeconds = 15
    , cfgLpSettlementMaxDrainTransactions = 4
    , cfgLpSettlementPendingReplacementSeconds = 60
    , cfgLpSettlementMaxReplacements = 3
    , cfgLpSettlementMaxTxCostWei = 0
    }

currentCompetitionReleaseManifest :: CompetitionReleaseManifest
currentCompetitionReleaseManifest =
  CompetitionReleaseManifest
    { crmReleaseId = prId currentSepoliaRelease
    , crmChainId = prChainId currentSepoliaRelease
    , crmUsdc = prUsdc currentSepoliaRelease
    , crmOrderRouter = prOrderRouter currentSepoliaRelease
    , crmMarginClearinghouse = prMarginClearinghouse currentSepoliaRelease
    , crmAccountLens = prAccountLens currentSepoliaRelease
    , crmCfdEngine = prCfdEngine currentSepoliaRelease
    , crmCfdEngineLens = prCfdEngineLens currentSepoliaRelease
    , crmSettlementSidecar = prCfdEngineSettlementSidecar currentSepoliaRelease
    , crmPletherOracle = prPletherOracle currentSepoliaRelease
    , crmIndexerStartBlock = prDeploymentBlock currentSepoliaRelease
    }
