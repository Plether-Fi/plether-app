module Plether.Handlers.PerpsHistorySpec (spec) where

import Data.Aeson (object, (.=))
import Data.List (isInfixOf)
import Database.PostgreSQL.Simple (Query)
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
import Plether.Database.Schema
  ( PerpsIndexerStatusRow (..)
  , PerpsOrderRow (..)
  , executionModeOracleFrozen
  , pendingPerpsExecutionEvidenceSql
  , perpsExecutionEvidenceLaneLimits
  , perpsOrderBaseSelectSql
  , updatePerpsOrderLifecycleReceiptSql
  )
import Plether.Handlers.PerpsHistory
  ( orderRowToJson
  , perpsOrdersIndexedThroughBlock
  , perpsHistoryRouter
  , perpsMarketStatsChainId
  )
import Test.Hspec

spec :: Spec
spec = do
  describe "perpsMarketStatsChainId" $
    it "uses the Perps chain for market activity stats" $ do
      perpsMarketStatsChainId testConfig `shouldBe` 421614

  describe "perpsHistoryRouter" $ do
    it "defaults to the configured current router" $
      perpsHistoryRouter testConfig Nothing
        `shouldBe` "0xbd2f286efca5f761e21452673ab9b8c14e17aad7"

    it "normalizes an explicit release router" $
      perpsHistoryRouter testConfig (Just "  0x485703D16FE36369C134DEe2A61C057733E7830F  ")
        `shouldBe` "0x485703d16fe36369c134dee2a61c057733e7830f"

  describe "terminal execution evidence" $ do
    it "hydrates and verifies the account from a standalone lifecycle finalization" $ do
      queryContains updatePerpsOrderLifecycleReceiptSql "account = COALESCE(perps_orders.account, ?)"
      queryContains updatePerpsOrderLifecycleReceiptSql "AND (account IS NULL OR account = ?)"

    it "persists canonical oracle-frozen state from the lifecycle execution mode" $ do
      executionModeOracleFrozen "Live" `shouldBe` Just False
      executionModeOracleFrozen "FAD" `shouldBe` Just False
      executionModeOracleFrozen "Frozen" `shouldBe` Just True
      executionModeOracleFrozen "Unknown(0)" `shouldBe` Nothing
      queryContains updatePerpsOrderLifecycleReceiptSql "execution_oracle_frozen = ?"

    it "reserves two recent evidence slots and three fair-backlog slots" $ do
      perpsExecutionEvidenceLaneLimits 5 `shouldBe` (2, 3)
      queryContains pendingPerpsExecutionEvidenceSql "WHERE execution_evidence_last_attempt_at IS NULL"
      queryContains pendingPerpsExecutionEvidenceSql "ORDER BY terminal_block_number DESC, order_id DESC"
      queryContains pendingPerpsExecutionEvidenceSql "NOT EXISTS"
      queryContains pendingPerpsExecutionEvidenceSql "execution_evidence_last_attempt_at < NOW() - INTERVAL '5 minutes'"
      queryContains pendingPerpsExecutionEvidenceSql "ORDER BY lane ASC, lane_order ASC"

    it "projects exact execution VPI independently from activity data" $ do
      queryContains perpsOrderBaseSelectSql "o.execution_vpi_usdc"
      queryContains perpsOrderBaseSelectSql "data->>'vpiUsdc'"

    it "hydrates legacy receipt economics with the indexed execution bounty" $ do
      queryContains perpsOrderBaseSelectSql "data->>'executionBountyUsdc'"
      queryContains perpsOrderBaseSelectSql "jsonb_build_object('executionBountyUsdc'"
      queryContains perpsOrderBaseSelectSql "e.event_name = 'IntentRegistered'"

    it "projects the canonical terminal event block hash" $ do
      queryContains perpsOrderBaseSelectSql "terminal_event.block_hash"
      queryContains perpsOrderBaseSelectSql "e.block_number = o.terminal_block_number"
      queryContains perpsOrderBaseSelectSql "e.event_name = 'OrderFinalized'"

    it "correlates batched activity with the nearest preceding V2 finalization" $ do
      queryContains perpsOrderBaseSelectSql "e.order_id = o.order_id"
      queryContains perpsOrderBaseSelectSql "'OrderFinalized'"
      queryContains perpsOrderBaseSelectSql "o.terminal_status = 'Executed'"
      queryContains perpsOrderBaseSelectSql "a.activity_type IN ('Open', 'Close')"
      queryContains perpsOrderBaseSelectSql "a.log_index < terminal_event.log_index"
      queryContains perpsOrderBaseSelectSql "a.log_index > previous_terminal_event.log_index"
      queryContains perpsOrderBaseSelectSql "ORDER BY a.log_index DESC"

    it "excludes V1 orders that have no lifecycle client identity" $
      queryContains perpsOrderBaseSelectSql "o.client_order_id IS NOT NULL"

    it "serializes signed activity VPI as a lossless decimal string" $
      orderRowToJson
        executedOrderRow
        `shouldBe` object
          [ "orderId" .= ("9202" :: String)
          , "orderRouter" .= ("0xrouter" :: String)
          , "account" .= ("0xaccount" :: String)
          , "side" .= (1 :: Int)
          , "commitTxHash" .= ("0xcommit" :: String)
          , "commitBlockNumber" .= ("293014692" :: String)
          , "commitTimestamp" .= (1_785_437_833 :: Integer)
          , "terminalTxHash" .= ("0xreveal" :: String)
          , "terminalBlockNumber" .= ("293014724" :: String)
          , "terminalBlockHash" .= ("0xterminalblock" :: String)
          , "terminalTimestamp" .= (1_785_437_841 :: Integer)
          , "terminalStatus" .= ("Executed" :: String)
          , "executionPrice" .= ("98391251" :: String)
          , "vpiUsdc" .= ("182822887" :: String)
          , "frozenCloseSpreadUsdc" .= ("0" :: String)
          , "executionEconomicsVersion" .= (2 :: Int)
          , "executionOraclePrice" .= ("98391482" :: String)
          , "executionOracleFrozen" .= False
          , "oracleMinPublishTime" .= ("1785437834" :: String)
          , "oracleMaxPublishTime" .= ("1785437834" :: String)
          , "oracleDerivationVersion" .= (1 :: Int)
          , "clientOrderId" .= ("0xclient" :: String)
          , "receiptHash" .= ("0xreceipt" :: String)
          , "terminalReason" .= ("Executed" :: String)
          , "executionMode" .= ("Live" :: String)
          , "receiptEconomics" .= object
              [ "executionBountyUsdc" .= ("200000" :: String)
              , "vpiUsdc" .= ("182822887" :: String)
              , "frozenSpreadUsdc" .= ("0" :: String)
              ]
          , "activityType" .= ("Close" :: String)
          , "activitySizeDelta" .= ("98308614058332359914207" :: String)
          , "activityPrice" .= ("98391251" :: String)
          , "activityVpiUsdc" .= ("-182822887" :: String)
          , "activityPnlUsdc" .= ("104909054" :: String)
          ]

  describe "account order indexed-through proof" $ do
    it "exposes the indexer cursor when canonical indexer state exists" $
      perpsOrdersIndexedThroughBlock (Just indexerStatusRow)
        `shouldBe` Just 293_014_900

    it "does not manufacture an indexed-through proof from returned rows" $
      perpsOrdersIndexedThroughBlock Nothing
        `shouldBe` Nothing

queryContains :: Query -> String -> Expectation
queryContains sql fragment =
  show sql `shouldSatisfy` isInfixOf fragment

executedOrderRow :: PerpsOrderRow
executedOrderRow =
  PerpsOrderRow
    { porOrderId = 9202
    , porOrderRouter = "0xrouter"
    , porAccount = Just "0xaccount"
    , porSide = Just 1
    , porCommitTxHash = Just "0xcommit"
    , porCommitBlockNumber = Just 293_014_692
    , porCommitTimestamp = Just 1_785_437_833
    , porTerminalTxHash = Just "0xreveal"
    , porTerminalBlockNumber = Just 293_014_724
    , porTerminalBlockHash = Just "0xterminalblock"
    , porTerminalTimestamp = Just 1_785_437_841
    , porTerminalStatus = "Executed"
    , porFailureReason = Nothing
    , porExecutionPrice = Just 98_391_251
    , porExecutionVpiUsdc = Just 182_822_887
    , porExecutionFrozenCloseSpreadUsdc = Just 0
    , porExecutionEconomicsVersion = Just 2
    , porExecutionOraclePrice = Just 98_391_482
    , porExecutionOracleFrozen = Nothing
    , porOracleMinPublishTime = Just 1_785_437_834
    , porOracleMaxPublishTime = Just 1_785_437_834
    , porOracleDerivationVersion = Just 1
    , porClientOrderId = Just "0xclient"
    , porReceiptHash = Just "0xreceipt"
    , porTerminalReason = Just "Executed"
    , porPendingReason = Nothing
    , porExecutionMode = Just "Live"
    , porFailedConstraint = Nothing
    , porReceiptEconomics = Just $ object
        [ "executionBountyUsdc" .= ("200000" :: String)
        , "vpiUsdc" .= ("182822887" :: String)
        , "frozenSpreadUsdc" .= ("0" :: String)
        ]
    , porCleanupActor = Nothing
    , porActivityType = Just "Close"
    , porActivitySizeDelta = Just 98_308_614_058_332_359_914_207
    , porActivityPrice = Just 98_391_251
    , porActivityVpiUsdc = Just (-182_822_887)
    , porActivityPnlUsdc = Just 104_909_054
    , porSortBlock = 293_014_724
    }

indexerStatusRow :: PerpsIndexerStatusRow
indexerStatusRow =
  PerpsIndexerStatusRow
    { pisIndexerName = "perps-history-costs-v2"
    , pisChainId = 421614
    , pisReleaseRouter = "0xrouter"
    , pisLastIndexedBlock = 293_014_900
    , pisLastIndexedBlockHash = Just "0xindexedblock"
    }

testConfig :: Config
testConfig =
  Config
    { cfgRpcUrl = "https://eth-sepolia.example"
    , cfgRpcAuthToken = Nothing
    , cfgChainId = 11155111
    , cfgPort = 3001
    , cfgCorsOrigins = []
    , cfgDeployments = []
    , cfgDatabaseUrl = Nothing
    , cfgIndexerStartBlock = 0
    , cfgPythBenchmarksUrl = "https://benchmarks.pyth.network"
    , cfgPythHistoryUrl = "https://pyth.dourolabs.app/v1"
    , cfgPythHermesUrl = "https://hermes.pyth.network"
    , cfgPythApiKey = Nothing
    , cfgPythBackfillDays = 7
    , cfgPythSampleIntervalSeconds = 60
    , cfgPythLatestMaxAgeSeconds = 10
    , cfgPythIngestionEnabled = False
    , cfgPerpsCandleWriteMode = PerpsCandleWritesOff
    , cfgPerpsCandleReadMode = PerpsCandleReadsLegacy
    , cfgPerpsCandleReadIntervals = []
    , cfgPerpsCandleShadowSampleBps = 0
    , cfgPerpsCandleStrictCoverage = True
    , cfgPerpsCandleLatenessSeconds = 120
    , cfgPerpsCandleFinalizationGraceSeconds = 15
    , cfgPerpsRpcUrl = "https://arb-sepolia.example"
    , cfgPerpsRpcAuthToken = Nothing
    , cfgPerpsChainId = 421614
    , cfgPerpsUsdc = "0xc3CE8590B7EcDE7454f9D5b51a797bbDe96fe56B"
    , cfgPerpsOrderRouter = "0xbd2f286efca5F761E21452673ab9b8C14e17aad7"
    , cfgPerpsOrderLifecycleBook = Nothing
    , cfgPerpsCfdEngine = "0x9611E643aC4691E8fDeD8a0c2C22c56438B6f352"
    , cfgPerpsCfdEngineLens = "0xE004D20803B484fb62734b78d6144438669Bad18"
    , cfgPerpsCfdEngineSettlementSidecar = "0x5CBb5A2f75ea005753a6C0AcCE01f7bB02B668D7"
    , cfgPerpsMarginClearinghouse = "0xA863F985EedA8BF5BE2320693BB93d109EBB2dBd"
    , cfgPerpsPletherOracle = "0x0000000000000000000000000000000000000000"
    , cfgPerpsAccountLens = "0xd949E5987c3d33299dA4Da4d06b064729000d2EB"
    , cfgPerpsHousePool = "0x21D52509Bb9b9857DaBc8c7FD36dD7fed9118918"
    , cfgPerpsSettlementMonitorLens = "0xf799Be4f8B5142C052d821F0067ADdFBF9Ce5820"
    , cfgPerpsIndexerStartBlock = 0
    , cfgVaultHistoryHousePoolAddress = "0x0000000000000000000000000000000000000001"
    , cfgVaultHistorySeniorVaultAddress = "0x0000000000000000000000000000000000000002"
    , cfgVaultHistoryJuniorVaultAddress = "0x0000000000000000000000000000000000000003"
    , cfgVaultHistoryDeploymentBlock = 0
    , cfgVaultHistoryConfirmations = 12
    , cfgInsightsCompetitionRules = july2026Competition
    , cfgInsightsCompetitionReleaseManifest = testReleaseManifest
    , cfgRegistrationConfig = Nothing
    , cfgAaConfig = Nothing
    , cfgFaucetGuardConfig = Nothing
    , cfgFaucetPrivateKey = Nothing
    , cfgKeeperPrivateKey = Nothing
    , cfgKeeperPollSeconds = 1
    , cfgKeeperIdlePollSeconds = 5
    , cfgKeeperMaxBatchSize = 20
    , cfgKeeperConfirmations = 1
    , cfgKeeperGasBufferBps = 2000
    , cfgKeeperFeeBufferBps = 2500
    , cfgLpSettlementMode = LpSettlementOff
    , cfgLpSettlementPrivateKey = Nothing
    , cfgLpSettlementSeniorVault = "0x7Bf2B3d3912b5B8D367987C9ADfC6Bd1216E8129"
    , cfgLpSettlementJuniorVault = "0x41D785d3BcF4D0e306E491a66Ddb0d938135Cc1c"
    , cfgLpSettlementPollSeconds = 15
    , cfgLpSettlementMaxDrainTransactions = 4
    , cfgLpSettlementPendingReplacementSeconds = 60
    , cfgLpSettlementMaxReplacements = 3
    , cfgLpSettlementMaxTxCostWei = 0
    }

testReleaseManifest :: CompetitionReleaseManifest
testReleaseManifest =
  CompetitionReleaseManifest
    { crmReleaseId = "perps-history-test"
    , crmChainId = 421614
    , crmUsdc = "0xB15503d70B0eAa644dc6650d2A248762F7c5bCE3"
    , crmOrderRouter = "0x04E3103752f623fBcDcD01f588590Af4c53E4c1E"
    , crmMarginClearinghouse = "0x19c2f60f6312EAF9acDE4C2b04551a05cA9bE76e"
    , crmAccountLens = "0xC4C886A6F1D7CB22C833AC1b29f29Da43AfbcCd1"
    , crmCfdEngine = "0x6A25eA1015b5f032d8a2D95d57AEfcB99219bF0a"
    , crmCfdEngineLens = "0xa9aA3F66A88826C6856E1Fc915805784845A6b64"
    , crmSettlementSidecar = "0x0b65286A091266504502179558411935c339f8a6"
    , crmPletherOracle = "0x0000000000000000000000000000000000000000"
    , crmIndexerStartBlock = 0
    }
