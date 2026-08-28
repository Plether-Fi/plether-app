module Plether.Handlers.PerpsHistorySpec (spec) where

import Data.Aeson (object, (.=))
import Data.List (isInfixOf)
import Database.PostgreSQL.Simple (Query)
import Plether.Config
  ( Config (..)
  , PerpsCandleReadMode (..)
  , PerpsCandleWriteMode (..)
  )
import Plether.Database.Schema
  ( PerpsIndexerStatusRow (..)
  , PerpsKeeperTerminalOrderRow (..)
  , PerpsOrderRow (..)
  , pendingPerpsExecutionEvidenceSql
  , perpsExecutionEvidenceLaneLimits
  , perpsOrderBaseSelectSql
  )
import Plether.Handlers.PerpsHistory
  ( keeperTerminalIsCanonicallyRejected
  , orderRowToJson
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
        `shouldBe` "0x97a901de2b267c307e264fd5f71403f8072f73e7"

    it "normalizes an explicit release router" $
      perpsHistoryRouter testConfig (Just "  0x485703D16FE36369C134DEe2A61C057733E7830F  ")
        `shouldBe` "0x485703d16fe36369c134dee2a61c057733e7830f"

  describe "terminal execution evidence" $ do
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

    it "projects the canonical terminal event block hash" $ do
      queryContains perpsOrderBaseSelectSql "terminal_event.block_hash"
      queryContains perpsOrderBaseSelectSql "e.contract_address = o.order_router"
      queryContains perpsOrderBaseSelectSql "e.block_number = o.terminal_block_number"
      queryContains perpsOrderBaseSelectSql "CASE WHEN o.terminal_status = 'Executed' THEN 'OrderExecuted' ELSE 'OrderFailed' END"

    it "correlates batched activity with the nearest preceding OrderExecuted event" $ do
      queryContains perpsOrderBaseSelectSql "e.order_id = o.order_id"
      queryContains perpsOrderBaseSelectSql "e.contract_address = o.order_router"
      queryContains perpsOrderBaseSelectSql "o.terminal_status = 'Executed'"
      queryContains perpsOrderBaseSelectSql "a.activity_type IN ('Open', 'Close')"
      queryContains perpsOrderBaseSelectSql "a.log_index < terminal_event.log_index"
      queryContains perpsOrderBaseSelectSql "a.log_index > previous_terminal_event.log_index"
      queryContains perpsOrderBaseSelectSql "ORDER BY a.log_index DESC"

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
          , "executionEconomicsVersion" .= (1 :: Int)
          , "executionOraclePrice" .= ("98391482" :: String)
          , "executionOracleFrozen" .= False
          , "oracleMinPublishTime" .= ("1785437834" :: String)
          , "oracleMaxPublishTime" .= ("1785437834" :: String)
          , "oracleDerivationVersion" .= (1 :: Int)
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

  describe "keeper terminal canonical rejection" $ do
    it "suppresses a stale keeper terminal after indexed history proves the order is still committed" $
      keeperTerminalIsCanonicallyRejected
        0
        (Just 293_014_724)
        (Just committedOrderRow)
        keeperExecutedOrder
        `shouldBe` True

    it "keeps the fast keeper terminal while canonical history has not reached its block" $
      keeperTerminalIsCanonicallyRejected
        0
        (Just 293_014_723)
        (Just committedOrderRow)
        keeperExecutedOrder
        `shouldBe` False

    it "suppresses a stale keeper terminal after its canonical commit also disappears" $
      keeperTerminalIsCanonicallyRejected
        293_014_600
        (Just 293_014_724)
        Nothing
        keeperExecutedOrder
        `shouldBe` True

    it "does not infer an absent commit that predates the indexer's coverage" $
      keeperTerminalIsCanonicallyRejected
        293_014_700
        (Just 293_014_724)
        Nothing
        keeperExecutedOrder
        `shouldBe` False

    it "suppresses a stale keeper terminal when the canonical commit identity changed" $
      keeperTerminalIsCanonicallyRejected
        0
        (Just 293_014_724)
        (Just committedOrderRow {porCommitTxHash = Just "0xreplacement"})
        keeperExecutedOrder
        `shouldBe` True

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
    , porExecutionEconomicsVersion = Just 1
    , porExecutionOraclePrice = Just 98_391_482
    , porExecutionOracleFrozen = Just False
    , porOracleMinPublishTime = Just 1_785_437_834
    , porOracleMaxPublishTime = Just 1_785_437_834
    , porOracleDerivationVersion = Just 1
    , porCleanupActor = Nothing
    , porActivityType = Just "Close"
    , porActivitySizeDelta = Just 98_308_614_058_332_359_914_207
    , porActivityPrice = Just 98_391_251
    , porActivityVpiUsdc = Just (-182_822_887)
    , porActivityPnlUsdc = Just 104_909_054
    , porSortBlock = 293_014_724
    }

committedOrderRow :: PerpsOrderRow
committedOrderRow =
  executedOrderRow
    { porTerminalTxHash = Nothing
    , porTerminalBlockNumber = Nothing
    , porTerminalBlockHash = Nothing
    , porTerminalTimestamp = Nothing
    , porTerminalStatus = "Committed"
    , porExecutionPrice = Nothing
    }

keeperExecutedOrder :: PerpsKeeperTerminalOrderRow
keeperExecutedOrder =
  PerpsKeeperTerminalOrderRow
    { pktoOrderId = 9202
    , pktoOrderRouter = "0xrouter"
    , pktoAccount = "0xaccount"
    , pktoSide = 1
    , pktoCommitBlock = 293_014_692
    , pktoCommitEventBlock = Just 293_014_692
    , pktoCommitTime = 1_785_437_833
    , pktoCommitTxHash = "0xcommit"
    , pktoStatus = "Executed"
    , pktoExecutionTxHash = Just "0xreveal"
    , pktoExecutionBlock = Just 293_014_724
    , pktoExecutionPrice = Just 98_391_251
    , pktoFailureTxHash = Nothing
    , pktoFailureBlock = Nothing
    , pktoFailureReason = Nothing
    }

indexerStatusRow :: PerpsIndexerStatusRow
indexerStatusRow =
  PerpsIndexerStatusRow
    { pisIndexerName = "perps-history-costs-v1"
    , pisChainId = 421614
    , pisReleaseRouter = "0xrouter"
    , pisLastIndexedBlock = 293_014_900
    , pisLastIndexedBlockHash = Just "0xindexedblock"
    }

testConfig :: Config
testConfig =
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
    , cfgPerpsCfdEngine = "0x3dc9C0A1f9C745A4B08BD5C2E6c7aE613561c20D"
    , cfgPerpsMarginClearinghouse = "0x2f98787F6dCC3b1f2E4a2AFa5acf410159b9F211"
    , cfgPerpsPletherOracle = "0x0000000000000000000000000000000000000000"
    , cfgPerpsAccountLens = "0x429DA61a7a616DeDD84d2a51eB6Dc1bD72427dC1"
    , cfgPerpsHousePool = "0x86939a377A78EDe8EEe5445765ac77c9016E35E2"
    , cfgPerpsSettlementMonitorLens = "0xd251AC0BD90780c48F31F575152808315200664E"
    , cfgPerpsIndexerStartBlock = 0
    , cfgVaultHistoryHousePoolAddress = "0x0000000000000000000000000000000000000001"
    , cfgVaultHistorySeniorVaultAddress = "0x0000000000000000000000000000000000000002"
    , cfgVaultHistoryJuniorVaultAddress = "0x0000000000000000000000000000000000000003"
    , cfgVaultHistoryDeploymentBlock = 0
    , cfgVaultHistoryRpcUrl = "https://archive.example"
    , cfgVaultHistoryConfirmations = 12
    , cfgAaConfig = Nothing
    , cfgFaucetPrivateKey = Nothing
    , cfgKeeperPrivateKey = Nothing
    , cfgKeeperPollSeconds = 1
    , cfgKeeperMaxBatchSize = 20
    , cfgKeeperConfirmations = 1
    , cfgKeeperGasBufferBps = 2000
    , cfgKeeperFeeBufferBps = 2500
    , cfgLpSettlementEnabled = False
    , cfgLpSettlementPollSeconds = 15
    }
