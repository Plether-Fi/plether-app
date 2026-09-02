module Plether.ApiSpec (spec) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Text (Text)
import Network.HTTP.Types.Header (hCacheControl)
import Network.HTTP.Types.Status (status200, status302, status404, status500, status503)
import Network.Wai (Application, Middleware, responseLBS)
import Network.Wai.Test
  ( SResponse (..)
  , defaultRequest
  , request
  , runSession
  , setPath
  )
import Plether.Api
  ( noStoreErrorResponses
  , parseDatabaseBigInt
  , parseProtocolCursor
  , parseProtocolOrderId
  , parseTrancheHistoryCursor
  , protocolExplorerGate
  , protocolRpcChainGateWith
  )
import Plether.Config
  ( Config (..)
  , LpSettlementMode (..)
  , PerpsCandleReadMode (..)
  , PerpsCandleWriteMode (..)
  )
import Plether.Ethereum.Client (RpcError (..))
import Plether.Handlers.ProtocolInsights
  ( ProtocolCursor (..)
  , TrancheHistoryCursor (..)
  , encodeProtocolCursor
  , encodeTrancheHistoryCursor
  , getCurrentProtocolReleaseResponse
  )
import Plether.Insights.Competition
  ( CompetitionReleaseManifest (..)
  , july2026Competition
  )
import Plether.Types (ApiResponse (..))
import Test.Hspec

spec :: Spec
spec = do
  describe "noStoreErrorResponses" $ do
    it "overrides cacheable headers on every non-2xx response class" $
      mapM_
        ( \errorStatus -> do
            response <-
              runSession
                (request defaultRequest)
                ( noStoreErrorResponses $ \_ respond ->
                    respond $
                      responseLBS
                        errorStatus
                        [(hCacheControl, "public, max-age=3600")]
                        "error"
                )
            simpleHeaders response `shouldContain` [("Cache-Control", "no-store")]
            simpleHeaders response
              `shouldNotContain` [("Cache-Control", "public, max-age=3600")]
        )
        [status302, status404, status500]

    it "preserves successful response cache policy" $ do
      response <-
        runSession
          (request defaultRequest)
          ( noStoreErrorResponses $ \_ respond ->
              respond $
                responseLBS
                  status200
                  [(hCacheControl, "public, max-age=30")]
                  "ok"
          )
      simpleHeaders response
        `shouldContain` [("Cache-Control", "public, max-age=30")]

  describe "protocolExplorerGate" $ do
    it "passes protocol reads through when the explorer is enabled" $ do
      response <- requestPath True "/api/insights/v1/protocol/releases/release-1/overview"
      simpleStatus response `shouldBe` status200

    it "blocks release-scoped protocol reads when the explorer is disabled" $ do
      response <- requestPath False "/api/insights/v1/protocol/releases/release-1/overview"
      simpleStatus response `shouldBe` status404
      simpleHeaders response `shouldContain` [("Cache-Control", "no-store")]
      LBS8.unpack (simpleBody response) `shouldContain` "Protocol explorer is disabled"

    it "keeps the current-release bootstrap and competition API available" $ do
      current <- requestPath False "/api/insights/v1/protocol/releases/current"
      competition <- requestPath False "/api/insights/v1/competitions/testnet-trading-2026/leaderboard"
      simpleStatus current `shouldBe` status200
      simpleStatus competition `shouldBe` status200

    it "returns the disabled bootstrap flag without touching RPC" $ do
      result <-
        getCurrentProtocolReleaseResponse
          Nothing
          (error "disabled bootstrap must not evaluate the RPC client")
          disabledConfig
      case result of
        Right response ->
          case respData response of
            Object fields ->
              KM.lookup "explorerEnabled" fields `shouldBe` Just (Bool False)
            _ -> expectationFailure "current-release response was not an object"
        Left _ -> expectationFailure "disabled current-release bootstrap failed"

  describe "protocolRpcChainGateWith" $ do
    it "passes a protocol release read through on an exact chain match" $ do
      response <-
        requestPathWith
          (protocolRpcChainGateWith (pure $ Right 421614) 421614)
          "/api/insights/v1/protocol/releases/current"
      simpleStatus response `shouldBe` status200

    it "refuses a mismatching release RPC before the route handler runs" $ do
      response <-
        requestPathWith
          (protocolRpcChainGateWith (pure $ Right 1) 421614)
          "/api/insights/v1/protocol/releases/arbitrum-sepolia-2026-07/overview"
      simpleStatus response `shouldBe` status503
      simpleHeaders response `shouldContain` [("Cache-Control", "no-store")]
      LBS8.unpack (simpleBody response)
        `shouldContain` "rpc_chain_id_mismatch"

    it "redacts provider failures while exposing a stable availability reason" $ do
      response <-
        requestPathWith
          ( protocolRpcChainGateWith
              (pure $ Left $ RpcHttpError "https://private-rpc.invalid/secret")
              421614
          )
          "/api/insights/v1/protocol/releases/current"
      simpleStatus response `shouldBe` status503
      LBS8.unpack (simpleBody response)
        `shouldContain` "rpc_chain_id_unavailable"
      LBS8.unpack (simpleBody response)
        `shouldNotContain` "private-rpc"

    it "does not bind unrelated competition API reads to the perps RPC" $ do
      response <-
        requestPathWith
          (protocolRpcChainGateWith (pure $ Left $ RpcJsonError "unavailable") 421614)
          "/api/insights/v1/competitions/testnet-trading-2026/leaderboard"
      simpleStatus response `shouldBe` status200

  describe "protocol explorer identifiers" $ do
    it "bounds timestamp filters to PostgreSQL BIGINT" $ do
      parseDatabaseBigInt "9223372036854775807"
        `shouldBe` Just 9_223_372_036_854_775_807
      parseDatabaseBigInt "9223372036854775808"
        `shouldBe` Nothing

    it "accepts order IDs representable by both the contract and projection" $ do
      parseProtocolOrderId "0" `shouldBe` Just 0
      parseProtocolOrderId "9223372036854775807"
        `shouldBe` Just 9_223_372_036_854_775_807

    it "rejects order IDs that overflow the BIGINT projection or uint64 calldata identity" $ do
      parseProtocolOrderId "9223372036854775808" `shouldBe` Nothing
      parseProtocolOrderId "18446744073709551616" `shouldBe` Nothing

    it "round-trips an opaque cursor with its release, scope, and confirmed block anchor" $ do
      let cursor =
            ProtocolCursor
              { pcReleaseId = "arbitrum-sepolia-2026-07"
              , pcScope = "transactions:-:-:-:-:-:-:-:-:-"
              , pcConfirmedBlock = 123
              , pcConfirmedBlockHash =
                  "0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
              , pcItemBlock = 120
              , pcItemLogIndex = Just 4
              , pcItemId = Just "0xtx:4"
              }
      parseProtocolCursor (encodeProtocolCursor cursor) `shouldBe` Just cursor
      parseProtocolCursor (encodeProtocolCursor cursor {pcItemBlock = 124})
        `shouldBe` Nothing
      parseProtocolCursor (encodeProtocolCursor cursor {pcConfirmedBlockHash = "0x1234"})
        `shouldBe` Nothing

    it "rejects unanchored preview cursors and malformed opaque payloads" $ do
      parseProtocolCursor "pc1_3132333a34" `shouldBe` Nothing
      parseProtocolCursor "v1.123.4" `shouldBe` Nothing
      parseProtocolCursor "pc2_not-hex" `shouldBe` Nothing

    it "round-trips an anchored compound tranche cursor with independent stream positions" $ do
      let cursor =
            TrancheHistoryCursor
              { thcReleaseId = "arbitrum-sepolia-2026-07"
              , thcScope = "tranche-history:senior"
              , thcConfirmedBlock = 123
              , thcConfirmedBlockHash =
                  "0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
              , thcActionBlock = Just 121
              , thcActionLogIndex = Just 8
              , thcActionId = Just "0xtx:8"
              , thcActionsComplete = False
              , thcCheckpointBlock = Just 117
              , thcCheckpointsComplete = False
              , thcCheckpointContinuationUnavailable = False
              }
      parseTrancheHistoryCursor (encodeTrancheHistoryCursor cursor)
        `shouldBe` Just cursor
      let checkpointOnly =
            cursor
              { thcActionBlock = Nothing
              , thcActionLogIndex = Nothing
              , thcActionId = Nothing
              , thcActionsComplete = True
              }
          actionOnly =
            cursor
              { thcCheckpointBlock = Nothing
              , thcCheckpointsComplete = True
              , thcCheckpointContinuationUnavailable = True
              }
      parseTrancheHistoryCursor (encodeTrancheHistoryCursor checkpointOnly)
        `shouldBe` Just checkpointOnly
      parseTrancheHistoryCursor (encodeTrancheHistoryCursor actionOnly)
        `shouldBe` Just actionOnly
      parseTrancheHistoryCursor
        (encodeTrancheHistoryCursor cursor {thcActionBlock = Just 124})
        `shouldBe` Nothing
      parseTrancheHistoryCursor
        ( encodeTrancheHistoryCursor
            cursor
              { thcActionBlock = Nothing
              , thcActionLogIndex = Nothing
              , thcActionId = Nothing
              , thcActionsComplete = True
              , thcCheckpointBlock = Nothing
              , thcCheckpointsComplete = True
              }
        )
        `shouldBe` Nothing

    it "keeps legacy tranche action cursors usable without inventing a checkpoint position" $ do
      let legacy =
            ProtocolCursor
              { pcReleaseId = "arbitrum-sepolia-2026-07"
              , pcScope = "tranche-history:junior"
              , pcConfirmedBlock = 123
              , pcConfirmedBlockHash =
                  "0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
              , pcItemBlock = 120
              , pcItemLogIndex = Just 4
              , pcItemId = Just "0xtx:4"
              }
      case parseTrancheHistoryCursor $ encodeProtocolCursor legacy of
        Just decoded -> do
          thcActionBlock decoded `shouldBe` Just 120
          thcCheckpointBlock decoded `shouldBe` Nothing
          thcCheckpointsComplete decoded `shouldBe` True
          thcCheckpointContinuationUnavailable decoded `shouldBe` True
        Nothing -> expectationFailure "legacy tranche action cursor was rejected"

requestPath :: Bool -> ByteString -> IO SResponse
requestPath enabled path =
  runSession
    (request $ setPath defaultRequest path)
    (protocolExplorerGate enabled upstream)

requestPathWith :: Middleware -> ByteString -> IO SResponse
requestPathWith gate path =
  runSession
    (request $ setPath defaultRequest path)
    (gate upstream)

upstream :: Application
upstream _ respond = respond $ responseLBS status200 [] "upstream"

disabledConfig :: Config
disabledConfig =
  Config
    { cfgRpcUrl = ""
    , cfgChainId = 421614
    , cfgPort = 3001
    , cfgCorsOrigins = []
    , cfgDeployments = []
    , cfgDatabaseUrl = Nothing
    , cfgIndexerStartBlock = 0
    , cfgPythBenchmarksUrl = ""
    , cfgPythHistoryUrl = ""
    , cfgPythHermesUrl = ""
    , cfgPythApiKey = Nothing
    , cfgPythBackfillDays = 7
    , cfgPythSampleIntervalSeconds = 60
    , cfgPythLatestMaxAgeSeconds = 10
    , cfgPythIngestionEnabled = False
    , cfgProtocolExplorerEnabled = False
    , cfgPerpsCandleWriteMode = PerpsCandleWritesOff
    , cfgPerpsCandleReadMode = PerpsCandleReadsLegacy
    , cfgPerpsCandleReadIntervals = []
    , cfgPerpsCandleShadowSampleBps = 0
    , cfgPerpsCandleStrictCoverage = True
    , cfgPerpsCandleLatenessSeconds = 120
    , cfgPerpsCandleFinalizationGraceSeconds = 15
    , cfgPerpsRpcUrl = ""
    , cfgPerpsChainId = 421614
    , cfgPerpsUsdc = zeroAddress
    , cfgPerpsOrderRouter = zeroAddress
    , cfgPerpsOrderLifecycleBook = Nothing
    , cfgPerpsCfdEngine = zeroAddress
    , cfgPerpsCfdEngineLens = zeroAddress
    , cfgPerpsCfdEngineSettlementSidecar = zeroAddress
    , cfgPerpsMarginClearinghouse = zeroAddress
    , cfgPerpsPletherOracle = zeroAddress
    , cfgPerpsAccountLens = zeroAddress
    , cfgPerpsPublicLens = zeroAddress
    , cfgPerpsHousePool = zeroAddress
    , cfgPerpsSeniorVault = zeroAddress
    , cfgPerpsJuniorVault = zeroAddress
    , cfgPerpsOrderRouterAdmin = zeroAddress
    , cfgPerpsCfdEngineAdmin = zeroAddress
    , cfgPerpsSettlementMonitorLens = zeroAddress
    , cfgPerpsIndexerStartBlock = 0
    , cfgVaultHistoryHousePoolAddress = zeroAddress
    , cfgVaultHistorySeniorVaultAddress = zeroAddress
    , cfgVaultHistoryJuniorVaultAddress = zeroAddress
    , cfgVaultHistoryDeploymentBlock = 0
    , cfgVaultHistoryRpcUrl = ""
    , cfgVaultHistoryConfirmations = 0
    , cfgInsightsCompetitionRules = july2026Competition
    , cfgInsightsCompetitionReleaseManifest = disabledReleaseManifest
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
    , cfgLpSettlementSeniorVault = zeroAddress
    , cfgLpSettlementJuniorVault = zeroAddress
    , cfgLpSettlementPollSeconds = 15
    , cfgLpSettlementMaxDrainTransactions = 4
    , cfgLpSettlementPendingReplacementSeconds = 60
    , cfgLpSettlementMaxReplacements = 3
    , cfgLpSettlementMaxTxCostWei = 0
    }

zeroAddress :: Text
zeroAddress = "0x0000000000000000000000000000000000000000"

disabledReleaseManifest :: CompetitionReleaseManifest
disabledReleaseManifest =
  CompetitionReleaseManifest
    { crmReleaseId = "disabled-explorer-test"
    , crmChainId = 421614
    , crmUsdc = zeroAddress
    , crmOrderRouter = zeroAddress
    , crmMarginClearinghouse = zeroAddress
    , crmAccountLens = zeroAddress
    , crmCfdEngine = zeroAddress
    , crmCfdEngineLens = zeroAddress
    , crmSettlementSidecar = zeroAddress
    , crmPletherOracle = zeroAddress
    , crmIndexerStartBlock = 0
    }
