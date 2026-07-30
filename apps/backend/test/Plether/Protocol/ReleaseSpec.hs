module Plether.Protocol.ReleaseSpec (spec) where

import Control.Monad (filterM)
import Data.Aeson (Value (..), eitherDecodeFileStrict, withObject, (.:))
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (Parser, parseEither)
import Data.List (find, nub, (\\))
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import Paths_plether_api (getDataFileName)
import Plether.Config (Config (..))
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
      let release = sepoliaRelease

      prId release `shouldBe` "arbitrum-sepolia-2026-07"
      prName release `shouldBe` "Plether Perps — July 2026"
      prChainId release `shouldBe` 421614
      prDeploymentBlock release `shouldBe` 288439939
      prCalculationVersion release `shouldBe` "protocol-transparency-v1"
      prUsdc release `shouldBe` "0xB15503d70B0eAa644dc6650d2A248762F7c5bCE3"

    it "pins every monitored contract address for the release" $ do
      releaseAddresses sepoliaRelease
        `shouldBe`
          [ "0x04E3103752f623fBcDcD01f588590Af4c53E4c1E"
          , "0x3073d6D021eC20b95a8b7C780f5c30c07036ff6C"
          , "0x6A25eA1015b5f032d8a2D95d57AEfcB99219bF0a"
          , "0xb256d4E88d649b2A149aA8B8caa3159260eFBc39"
          , "0x19c2f60f6312EAF9acDE4C2b04551a05cA9bE76e"
          , "0x4E202C06e2C378d1a85577ac631e592AB66f23FB"
          , "0xC4C886A6F1D7CB22C833AC1b29f29Da43AfbcCd1"
          , "0xFA654f4c548130F09C3Fb962AbD4bE32c0357C18"
          , "0x4bAb5448C1BD9A48B978ABcb014F1a8F80F100A8"
          , "0x7258d6E91fbEFB8a16751575adbe9bBB3086D458"
          , "0xADfEd3bf768D810309B97b4dF9F9E77Eaa3a401c"
          ]

    it "contains only complete, unique contract addresses" $ do
      let addresses = prUsdc sepoliaRelease : releaseAddresses sepoliaRelease
          normalized = map T.toLower addresses

      addresses `shouldSatisfy` all isValidAddress
      normalized `shouldSatisfy` all (/= zeroAddress)
      length (nub normalized) `shouldBe` length normalized

    it "publishes only explicit non-secret operational addresses and labels permissionless roles" $ do
      let wallets = prOperationalWallets sepoliaRelease
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

    it "keeps the packaged release manifest in parity with the compiled release" $ do
      manifestPath <- resolveReleaseManifestPath
      decoded <- eitherDecodeFileStrict manifestPath
      case decoded >>= parseEither parseManifestRelease of
        Left decodeError ->
          expectationFailure $
            "Could not decode packaged protocol release manifest: " <> decodeError
        Right (currentReleaseId, manifestRelease) -> do
          let compiledCurrentRelease = currentProtocolRelease baseConfig
          currentReleaseId `shouldBe` prId compiledCurrentRelease
          manifestRelease `shouldBe` protocolReleaseToJson compiledCurrentRelease

    it "keeps the manifest ABI in parity with every parameter and governance schema read" $ do
      manifestPath <- resolveReleaseManifestPath
      decoded <- eitherDecodeFileStrict manifestPath
      case decoded >>= parseEither parseManifestAnalytics of
        Left decodeError ->
          expectationFailure $
            "Could not decode packaged protocol analytics ABI: " <> decodeError
        Right (manifestReads, manifestEvents) -> do
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
                  [ gedSignature eventDefinition
                  | definition <- governanceCategoryDefinitions
                  , eventDefinition <- gcdEvents definition
                  ]
                    <> map gredSignature governanceRoleEvents

          requiredReads \\ manifestReads `shouldBe` []
          requiredEvents \\ manifestEvents `shouldBe` []
          "PoolConfigCancelled()" `elem` manifestEvents `shouldBe` False
          length (nub manifestReads) `shouldBe` length manifestReads
          length (nub manifestEvents) `shouldBe` length manifestEvents

  describe "currentProtocolRelease" $ do
    it "resolves the known release case-insensitively by chain and router" $ do
      let release =
            currentProtocolRelease
              baseConfig
                { cfgPerpsOrderRouter =
                    T.toLower (prOrderRouter sepoliaRelease)
                , cfgPerpsIndexerStartBlock = 1
                }

      release `shouldBe` sepoliaRelease

    it "does not reuse the known release when the configured router differs" $ do
      let release =
            currentProtocolRelease
              baseConfig
                { cfgPerpsOrderRouter = alternateRouter
                , cfgPerpsIndexerStartBlock = 123
                }

      prId release `shouldBe` "chain-421614-block-123"
      prId release `shouldNotBe` prId sepoliaRelease
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
    it "resolves a checked-in release on the configured RPC chain" $
      protocolReleaseById baseConfig (prId sepoliaRelease)
        `shouldBe` Just sepoliaRelease

    it "resolves a configured fallback release without aliasing it to a manifest release" $ do
      let fallbackConfig =
            baseConfig
              { cfgPerpsOrderRouter = alternateRouter
              , cfgPerpsIndexerStartBlock = 123
              }
          fallback = currentProtocolRelease fallbackConfig

      protocolReleaseById fallbackConfig (prId fallback)
        `shouldBe` Just fallback
      protocolReleaseById fallbackConfig (prId sepoliaRelease)
        `shouldBe` Just sepoliaRelease

    it "rejects unknown release IDs" $
      protocolReleaseById baseConfig "unknown-release"
        `shouldBe` Nothing

parseManifestRelease :: Value -> Parser (Text, Value)
parseManifestRelease =
  withObject "ProtocolReleaseManifest" $ \manifest -> do
    currentReleaseId <- manifest .: "currentReleaseId"
    releases <- manifest .: "releases" :: Parser [Value]
    case find (hasReleaseId currentReleaseId) releases of
      Nothing -> fail "currentReleaseId does not identify an entry in releases"
      Just (Object release) ->
        pure
          ( currentReleaseId
          , Object $ KM.delete "analyticsAbi" release
          )
      Just _ -> fail "release entry is not an object"
 where
  hasReleaseId releaseId (Object release) =
    KM.lookup "releaseId" release == Just (String releaseId)
  hasReleaseId _ _ = False

parseManifestAnalytics :: Value -> Parser ([Text], [Text])
parseManifestAnalytics =
  withObject "ProtocolReleaseManifest" $ \manifest -> do
    currentReleaseId <- manifest .: "currentReleaseId"
    releases <- manifest .: "releases" :: Parser [Value]
    currentRelease <-
      case find (hasReleaseId currentReleaseId) releases of
        Just release -> pure release
        Nothing -> fail "currentReleaseId does not identify an entry in releases"
    withObject "ProtocolRelease" parseRelease currentRelease
 where
  parseRelease release = do
    analytics <- release .: "analyticsAbi"
    withObject "ProtocolAnalyticsAbi" parseAnalytics analytics
  parseAnalytics analytics =
    (,) <$> analytics .: "reads" <*> analytics .: "events"
  hasReleaseId releaseId (Object release) =
    KM.lookup "releaseId" release == Just (String releaseId)
  hasReleaseId _ _ = False

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

sepoliaRelease :: ProtocolRelease
sepoliaRelease =
  fromJust $
    find
      ((== "arbitrum-sepolia-2026-07") . prId)
      knownProtocolReleases

releaseAddresses :: ProtocolRelease -> [Text]
releaseAddresses release =
  [ prOrderRouter release
  , prOrderRouterAdmin release
  , prCfdEngine release
  , prCfdEngineAdmin release
  , prMarginClearinghouse release
  , prPublicLens release
  , prAccountLens release
  , prHousePool release
  , prSeniorVault release
  , prJuniorVault release
  , prPletherOracle release
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
    , cfgPythHermesUrl = "https://hermes.pyth.network"
    , cfgPythApiKey = Nothing
    , cfgPythBackfillDays = 7
    , cfgPythSampleIntervalSeconds = 60
    , cfgPythLatestMaxAgeSeconds = 10
    , cfgPythIngestionEnabled = False
    , cfgProtocolExplorerEnabled = True
    , cfgPerpsRpcUrl = "https://arb-sepolia.example"
    , cfgPerpsChainId = 421614
    , cfgPerpsUsdc = "0xB15503d70B0eAa644dc6650d2A248762F7c5bCE3"
    , cfgPerpsOrderRouter = "0x04E3103752f623fBcDcD01f588590Af4c53E4c1E"
    , cfgPerpsCfdEngine = "0x6A25eA1015b5f032d8a2D95d57AEfcB99219bF0a"
    , cfgPerpsMarginClearinghouse = "0x19c2f60f6312EAF9acDE4C2b04551a05cA9bE76e"
    , cfgPerpsPletherOracle = "0xADfEd3bf768D810309B97b4dF9F9E77Eaa3a401c"
    , cfgPerpsAccountLens = "0xC4C886A6F1D7CB22C833AC1b29f29Da43AfbcCd1"
    , cfgPerpsPublicLens = "0x4E202C06e2C378d1a85577ac631e592AB66f23FB"
    , cfgPerpsHousePool = "0xFA654f4c548130F09C3Fb962AbD4bE32c0357C18"
    , cfgPerpsSeniorVault = "0x4bAb5448C1BD9A48B978ABcb014F1a8F80F100A8"
    , cfgPerpsJuniorVault = "0x7258d6E91fbEFB8a16751575adbe9bBB3086D458"
    , cfgPerpsOrderRouterAdmin = "0x3073d6D021eC20b95a8b7C780f5c30c07036ff6C"
    , cfgPerpsCfdEngineAdmin = "0xb256d4E88d649b2A149aA8B8caa3159260eFBc39"
    , cfgPerpsIndexerStartBlock = 288439939
    , cfgAaConfig = Nothing
    , cfgFaucetPrivateKey = Nothing
    , cfgKeeperPrivateKey = Nothing
    , cfgKeeperPollSeconds = 1
    , cfgKeeperMaxBatchSize = 20
    , cfgKeeperConfirmations = 1
    , cfgKeeperGasBufferBps = 2000
    , cfgKeeperFeeBufferBps = 2500
    }
