{-# LANGUAGE OverloadedStrings #-}

module Plether.Handlers.InsightsSqlSmokeSpec (spec) where

import Control.Applicative ((<|>))
import Control.Exception (bracket, finally)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection, Only (..), close, connectPostgreSQL, execute, execute_, query)
import Plether.Database.Schema (ensureBasketSnapshotSchema, ensurePerpsHistorySchema)
import Plether.Database.VaultPerformance
  ( VaultPerformanceSnapshotRow (..)
  , ensureVaultPerformanceSchema
  , getVaultPerformanceSnapshots
  , upsertVaultPerformanceSnapshot
  )
import Plether.Handlers.InsightsExecution
  ( FinalizedOrderRow (..)
  , fetchOracleUpdateIdsForOrders
  , fetchRecentTrades
  , fetchTransactionFinalizedOrders
  , fetchTransactionTrades
  )
import Plether.Handlers.InsightsLiquidity
  ( OiDeltaBucketAgg (..)
  , OrderBucketAgg (..)
  , PoolEfficiencySummary (..)
  , buildPoolEfficiencyAnalytics
  , fetchEfficiencyOrderBuckets
  , fetchEfficiencyOrderWindowTotals
  , fetchLatestBasketMark
  , fetchOiDeltaBuckets
  , fetchPriorOiSizes
  )
import Plether.Handlers.InsightsOracle
  ( PayloadDetailRow (..)
  , RecentUpdateRow (..)
  , SettledOrderRow (..)
  , fetchAdmittedBucketsPair
  , fetchLatestPayload
  , fetchNeighborPayload
  , fetchOrdersSettledWithUpdate
  , fetchPayloadDetailRow
  , fetchRecentAdmittedUpdates
  , fetchRecentRejectionEvents
  , fetchRejectionBreakdown
  , fetchRejectionBucketsPair
  )
import System.Environment (lookupEnv)
import Test.Hspec

withSmokeDb :: (Connection -> IO ()) -> Expectation
withSmokeDb action = do
  mPrimary <- lookupEnv "INSIGHTS_SQL_TEST_DATABASE_URL"
  mFallback <- lookupEnv "PASSKEY_NAMES_TEST_DATABASE_URL"
  case mPrimary <|> mFallback of
    Nothing -> pendingWith "Set INSIGHTS_SQL_TEST_DATABASE_URL (or PASSKEY_NAMES_TEST_DATABASE_URL) to run schema-backed Insights SQL smoke tests"
    Just url ->
      bracket (connectPostgreSQL (BS8.pack url)) close $ \conn -> do
        _ <- execute_ conn "DROP SCHEMA IF EXISTS insights_sql_smoke CASCADE"
        _ <- execute_ conn "CREATE SCHEMA insights_sql_smoke"
        _ <- execute_ conn "SET search_path TO insights_sql_smoke, public"
        finally
          (action conn)
          ( do
              _ <- execute_ conn "SET search_path TO public"
              _ <- execute_ conn "DROP SCHEMA IF EXISTS insights_sql_smoke CASCADE"
              pure ()
          )

spec :: Spec
spec = describe "Insights schema-backed PostgreSQL SQL smoke tests" $ do
  it "executes liquidity efficiency, oracle update, and execution queries against migrated PostgreSQL tables with seeded rows" $
    withSmokeDb $ \conn -> do
      ensureBasketSnapshotSchema conn
      ensurePerpsHistorySchema conn
      ensureVaultPerformanceSchema conn

      let chainId = 421614 :: Integer
          router = "0x918663d6da8cb33b325a04c4a5e8c51d42f3b444" :: Text
          housePool = "0xeb5604c393843b2f3c347ec24fa9ddad6ab2131e" :: Text
          seniorVault = "0x5dc42ba5a1e8dd3e1b6868d10f66a9bf8fb5b83d" :: Text
          juniorVault = "0x4a8b67184a1e3224c11a191cc69f22c50eef734e" :: Text
          trader = "0x1111111111111111111111111111111111111111" :: Text
          txHash1 = "0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" :: Text
          txHash2 = "0xbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb" :: Text
          blockHash = "0xcccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc" :: Text
          bucketStart = 1795867200 :: Integer -- exact multiple of 21600 and 3600
          nowTs = bucketStart + 3600
          priorTs = bucketStart - 3600
          pubTime1 = bucketStart + 120
          pubTime2 = bucketStart + 180

      -- 1. Seed Pyth update payloads + oracle admission rejection event
      [Only payloadId1] <-
        query
          conn
          "INSERT INTO perps_pyth_update_payloads \
          \(min_publish_time, max_publish_time, publish_times, update_data, source, fetched_at, basket_price, component_prices) \
          \VALUES (?, ?, '{}'::jsonb, '[\"0x01\"]'::jsonb, 'backend_hermes_latest_v2', ?, 95000000, '[]'::jsonb) RETURNING id"
          (pubTime1, pubTime1, pubTime1 + 1)
          :: IO [Only Integer]
      [Only payloadId2] <-
        query
          conn
          "INSERT INTO perps_pyth_update_payloads \
          \(min_publish_time, max_publish_time, publish_times, update_data, source, fetched_at, basket_price, component_prices) \
          \VALUES (?, ?, '{}'::jsonb, '[\"0x02\"]'::jsonb, 'backend_hermes_latest_v2', ?, 95100000, '[]'::jsonb) RETURNING id"
          (pubTime2, pubTime2, pubTime2 + 1)
          :: IO [Only Integer]

      _ <-
        execute
          conn
          "INSERT INTO perps_oracle_admission_events \
          \(occurred_at, route, outcome, reason_code, min_publish_time, max_publish_time, spread_seconds, age_seconds, detail) \
          \VALUES (?, 'latest', 'rejected', 'stale_price', ?, ?, 0, 22, 'Pyth payload exceeded freshness staleness bound')"
          (nowTs - 30, pubTime1, pubTime1)

      -- 2. Seed perps_orders (covering both side=0 long and side=1 short, negative carry/VPI, and NULL account)
      let receipt1 :: Value
          receipt1 =
            object
              [ "executionNotionalAssets" .= ("40000000000000" :: Text)
              , "executionFeeAssets" .= ("16000000000" :: Text)
              , "vpiAssets" .= ("4000000000" :: Text)
              , "carryAssets" .= ("1000000000" :: Text)
              ]
          receipt2 :: Value
          receipt2 =
            object
              [ "executionNotionalAssets" .= ("10000000000000" :: Text)
              , "executionFeeAssets" .= ("5000000000" :: Text)
              , "vpiAssets" .= ("-2000000000" :: Text)
              , "carryAssets" .= ("-500000000" :: Text)
              ]
      _ <-
        execute
          conn
          "INSERT INTO perps_orders \
          \(chain_id, order_router, order_id, account, side, commit_tx_hash, commit_block_number, commit_timestamp, \
          \ terminal_tx_hash, terminal_block_number, terminal_timestamp, terminal_status, execution_price, \
          \ execution_oracle_price, execution_oracle_frozen, oracle_min_publish_time, oracle_max_publish_time, receipt_economics) \
          \VALUES (?, ?, 1001, ?, 0, ?, 100, ?, ?, 102, ?, 'Executed', 95000000, 95000000, FALSE, ?, ?, ?)"
          (chainId, router, trader, txHash1, pubTime1 - 2, txHash1, pubTime1 + 2, pubTime1, pubTime1, receipt1)
      _ <-
        execute
          conn
          "INSERT INTO perps_orders \
          \(chain_id, order_router, order_id, account, side, commit_tx_hash, commit_block_number, commit_timestamp, \
          \ terminal_tx_hash, terminal_block_number, terminal_timestamp, terminal_status, execution_price, \
          \ execution_oracle_price, execution_oracle_frozen, oracle_min_publish_time, oracle_max_publish_time, receipt_economics) \
          \VALUES (?, ?, 1002, NULL, 1, ?, 101, ?, ?, 102, ?, 'Executed', 95050000, 95000000, FALSE, ?, ?, ?)"
          (chainId, router, txHash2, pubTime1 - 1, txHash1, pubTime1 + 2, pubTime1, pubTime1, receipt2)

      -- 3. Seed perps_account_activity (prior OI + in-window OI delta)
      _ <-
        execute
          conn
          "INSERT INTO perps_account_activity \
          \(chain_id, release_router, event_key, account, activity_type, order_id, side, price, size_delta, \
          \ amount_assets, pnl_assets, tx_hash, block_number, block_hash, tx_index, log_index, timestamp, data) \
          \VALUES (?, ?, 'ev-prior-long', ?, 'Open', 900, 0, 100000000, 200000000000000000000000, 10000000000, 0, ?, 90, ?, 0, 1, ?, '{}'::jsonb)"
          (chainId, router, trader, txHash2, blockHash, priorTs)
      _ <-
        execute
          conn
          "INSERT INTO perps_account_activity \
          \(chain_id, release_router, event_key, account, activity_type, order_id, side, price, size_delta, \
          \ amount_assets, pnl_assets, tx_hash, block_number, block_hash, tx_index, log_index, timestamp, data) \
          \VALUES (?, ?, 'ev-win-short', ?, 'Open', 1001, 1, 95000000, 100000000000000000000000, 5000000000, 0, ?, 102, ?, 0, 2, ?, '{}'::jsonb)"
          (chainId, router, trader, txHash1, blockHash, pubTime1 + 2)

      -- 4. Seed vault_performance_snapshots
      upsertVaultPerformanceSnapshot
        conn
        VaultPerformanceSnapshotRow
          { vpsChainId = chainId
          , vpsHousePoolAddress = housePool
          , vpsSeniorVaultAddress = seniorVault
          , vpsJuniorVaultAddress = juniorVault
          , vpsEpochTimestamp = bucketStart
          , vpsBlockNumber = 102
          , vpsBlockHash = blockHash
          , vpsBlockTimestamp = bucketStart
          , vpsMarkFresh = Just True
          , vpsSeniorLockedAssets = Just 600000000000
          , vpsJuniorLockedAssets = Just 200000000000
          , vpsSeniorTotalAssets = 600000000000
          , vpsSeniorTotalSupply = 600000000000
          , vpsSeniorSharePriceWad = 1000000000000000000
          , vpsJuniorTotalAssets = 200000000000
          , vpsJuniorTotalSupply = 200000000000
          , vpsJuniorSharePriceWad = 1000000000000000000
          }

      -- 5. Verify InsightsLiquidity efficiency queries against live PostgreSQL
      snaps <- getVaultPerformanceSnapshots conn chainId housePool seniorVault juniorVault 721
      length snaps `shouldBe` 1

      orderBuckets <- fetchEfficiencyOrderBuckets conn chainId router bucketStart
      Map.lookup bucketStart orderBuckets
        `shouldBe` Just (OrderBucketAgg 2 50000000000000 26000000000)

      windowTotals <-
        fetchEfficiencyOrderWindowTotals
          conn
          chainId
          router
          (nowTs - 86400)
          (nowTs - 7 * 86400)
          (nowTs - 30 * 86400)
      windowTotals `shouldBe` (50000000000000, 50000000000000, 50000000000000, 26000000000, 26000000000)

      priorOi <- fetchPriorOiSizes conn chainId router bucketStart
      priorOi `shouldBe` (200000000000000000000000, 0)

      oiBuckets <- fetchOiDeltaBuckets conn chainId router bucketStart
      Map.lookup bucketStart oiBuckets
        `shouldBe` Just (OiDeltaBucketAgg 0 100000000000000000000000 (Just 95000000))

      mMark <- fetchLatestBasketMark conn
      mMark `shouldBe` Just 95100000

      let (effSummary, s7d, s30d) =
            buildPoolEfficiencyAnalytics nowTs snaps orderBuckets priorOi oiBuckets mMark windowTotals
      length s7d `shouldBe` 28
      length s30d `shouldBe` 30
      pesPoolTvlAssets effSummary `shouldBe` Just 800000000000
      pesVolume24hAssets effSummary `shouldBe` 50000000000000
      pesTurnover24hX100 effSummary `shouldBe` Just 6250

      -- 6. Verify InsightsOracle queries (including settledOrders integer side & NULL account decoding)
      mLatest <- fetchLatestPayload conn
      mLatest `shouldSatisfy` isJust

      recentUpdates <- fetchRecentAdmittedUpdates conn chainId router
      length recentUpdates `shouldBe` 2
      let mUpdate1 = filter (\r -> rurId r == payloadId1) recentUpdates
      map rurOrdersSettledCount mUpdate1 `shouldBe` [2]

      mDetail <- fetchPayloadDetailRow conn (Just payloadId1)
      fmap pdrId mDetail `shouldBe` Just payloadId1

      mNext <- fetchNeighborPayload conn False pubTime1 payloadId1
      fmap (\(pid, _, _) -> pid) mNext `shouldBe` Just payloadId2

      settledOrders <- fetchOrdersSettledWithUpdate conn chainId router pubTime1 pubTime1
      length settledOrders `shouldBe` 2
      map (\o -> (sorOrderId o, sorAccount o, sorSide o, sorStatus o)) settledOrders
        `shouldBe` [ ("1002", "", Just "short", "Executed")
                   , ("1001", trader, Just "long", "Executed")
                   ]

      (adm1h, adm24h) <- fetchAdmittedBucketsPair conn (nowTs - 3600) nowTs 120 (nowTs - 86400) nowTs 1800
      Map.size adm1h `shouldSatisfy` (> 0)
      Map.size adm24h `shouldSatisfy` (> 0)

      (rej1h, rej24h) <- fetchRejectionBucketsPair conn (nowTs - 3600) nowTs 120 (nowTs - 86400) nowTs 1800
      Map.size rej1h `shouldSatisfy` (> 0)
      Map.size rej24h `shouldSatisfy` (> 0)

      rejBreakdown <- fetchRejectionBreakdown conn (nowTs - 86400)
      length rejBreakdown `shouldBe` 1

      rejEvents <- fetchRecentRejectionEvents conn (nowTs - 86400)
      length rejEvents `shouldBe` 1

      -- 7. Verify InsightsExecution finalized orders & oracleUpdateId resolution
      finalized <- fetchTransactionFinalizedOrders conn chainId router txHash1
      length finalized `shouldBe` 2
      map forOrderId finalized `shouldBe` [1001, 1002]

      oracleIds <- fetchOracleUpdateIdsForOrders conn finalized
      Map.lookup (pubTime1, pubTime1) oracleIds `shouldBe` Just payloadId1

      recentTrades <- fetchRecentTrades conn chainId router 10
      length recentTrades `shouldBe` 2

      txTrades <- fetchTransactionTrades conn chainId router txHash1
      length txTrades `shouldBe` 1
