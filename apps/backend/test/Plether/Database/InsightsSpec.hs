module Plether.Database.InsightsSpec (spec) where

import Data.Aeson (object, (.=))
import Data.List (isInfixOf)
import Database.PostgreSQL.Simple (Query)
import Plether.Database.Insights
  ( InsightsActivityRow (..)
  , insightsDataStatusQuerySql
  , leaderboardQuerySql
  , leaderboardOrderBySql
  , leaderboardSearchPattern
  , snapshotBatchAccessIndexSql
  , walletActivityQuerySql
  )
import Plether.Handlers.Insights (activityRowToJson)
import Test.Hspec

spec :: Spec
spec = do
  describe "leaderboardSearchPattern" $ do
    it "builds a case-insensitive substring pattern" $
      leaderboardSearchPattern "Alice" `shouldBe` "%Alice%"

    it "escapes LIKE wildcards and the escape character" $
      leaderboardSearchPattern "team_100%!" `shouldBe` "%team!_100!%!!%"

  describe "competition result SQL" $ do
    it "ranks prizes only among reviewed, mechanically qualified participants" $ do
      queryContains leaderboardQuerySql "eligibility_status = 'eligible'"
      queryContains leaderboardQuerySql "active_days >= competition_minimum_active_days"
      queryContains leaderboardQuerySql "pc.prize_place <= 3"

    it "leaves zero-trade accounts unranked and orders them after active traders" $ do
      queryContains leaderboardQuerySql "final_pnl_usdc IS NULL OR executed_trades = 0 THEN NULL"
      queryContains leaderboardOrderBySql "CASE WHEN executed_trades > 0 THEN 0 ELSE 1 END"
      queryContains leaderboardOrderBySql "final_pnl_usdc DESC NULLS LAST"

    it "uses one published snapshot batch as the event projection upper bound" $ do
      queryContains leaderboardQuerySql "a.block_number <= cb.block_number"
      queryContains walletActivityQuerySql "a.block_number <= b.block_number"

    it "projects indexed execution fee and signed VPI into wallet activity" $ do
      queryContains walletActivityQuerySql "a.data->>'executionFeeUsdc'"
      queryContains walletActivityQuerySql "a.data->>'vpiUsdc'"
      queryContains insightsDataStatusQuerySql "perps-history-costs-v1:"

    it "serializes execution fee and signed VPI as lossless decimal strings" $ do
      activityRowToJson
        InsightsActivityRow
          { iarActivityType = "Close"
          , iarSide = Just 1
          , iarPrice = Just 96_866_388
          , iarSizeDelta = Just 4_513_034_696_886_011_329_166_042
          , iarAmountUsdc = Nothing
          , iarPnlUsdc = Just 3_424_490_727
          , iarExecutionFeeUsdc = Just 1_748_645_480
          , iarVpiUsdc = Just (-4_487_207_153)
          , iarTxHash = "0xabc"
          , iarBlockNumber = 290_862_399
          , iarTimestamp = 1_784_901_245
          , iarLogIndex = 16
          , iarSessionDay = Just "2026-07-24"
          }
        `shouldBe` object
          [ "activityType" .= ("Close" :: String)
          , "side" .= (1 :: Int)
          , "price" .= ("96866388" :: String)
          , "sizeDelta" .= ("4513034696886011329166042" :: String)
          , "pnlUsdc" .= ("3424490727" :: String)
          , "executionFeeUsdc" .= ("1748645480" :: String)
          , "vpiUsdc" .= ("-4487207153" :: String)
          , "txHash" .= ("0xabc" :: String)
          , "blockNumber" .= ("290862399" :: String)
          , "timestamp" .= (1_784_901_245 :: Integer)
          , "occurredAt" .= ("2026-07-24T13:54:05Z" :: String)
          , "logIndex" .= (16 :: Integer)
          , "sessionDay" .= ("2026-07-24" :: String)
          ]

    it "accepts legacy clearinghouse flows whose indexed JSON predates asset metadata" $ do
      queryContains leaderboardQuerySql "a.activity_type IN ('Deposit', 'Withdraw')"
      queryContains leaderboardQuerySql "OR NOT jsonb_exists(a.data, 'asset')"

    it "aggregates realized directional P&L for the public net-P&L reconciliation" $
      queryContains leaderboardQuerySql "SUM(a.pnl_usdc) FILTER (WHERE a.activity_type IN ('Close', 'Liquidated'))"

    it "ignores an all-zero regression after a stateful live snapshot" $ do
      queryContains leaderboardQuerySql "b.account_state_count > 0 OR NOT EXISTS"
      queryContains walletActivityQuerySql "b.account_state_count > 0 OR NOT EXISTS"

    it "reads snapshots only from the competition's recorded account lens" $ do
      queryContains leaderboardQuerySql "LOWER(b.account_lens_address) = LOWER(t.account_lens_address)"
      queryContains walletActivityQuerySql "LOWER(b.account_lens_address) = LOWER(t.account_lens_address)"

  describe "Insights load-control SQL" $ do
    it "derives data status from published batch metadata without scanning account snapshots" $ do
      queryContains insightsDataStatusQuerySql "FROM insights_snapshot_batches b"
      queryContains insightsDataStatusQuerySql "MAX(b.participant_count)"
      queryContains insightsDataStatusQuerySql "MAX(b.participant_count) FILTER (WHERE b.snapshot_kind = 'start'"
      queryContains insightsDataStatusQuerySql "MAX(b.participant_count) FILTER (WHERE b.snapshot_kind = 'final'"
      queryContains insightsDataStatusQuerySql "MAX(b.published_at)"
      queryContains insightsDataStatusQuerySql "b.chain_id = t.chain_id AND b.release_router = t.release_router"
      queryContains insightsDataStatusQuerySql "LOWER(b.account_lens_address) = LOWER(t.account_lens_address)"
      queryContains insightsDataStatusQuerySql "b.account_state_count > 0 OR NOT EXISTS"
      queryDoesNotContain insightsDataStatusQuerySql "insights_account_snapshots"

    it "creates an idempotent index matching batch-scoped snapshot reads" $ do
      queryContains snapshotBatchAccessIndexSql "CREATE INDEX IF NOT EXISTS"
      queryContains snapshotBatchAccessIndexSql
        "insights_account_snapshots(competition_slug, snapshot_kind, block_number, wallet)"

    it "keeps the static snapshot-batch schema aligned with runtime publication" $ do
      schema <- readFile "schema.sql"
      schema
        `shouldSatisfy` isInfixOf
          "release_router TEXT NOT NULL,\n    account_lens_address TEXT NOT NULL,\n    block_number BIGINT NOT NULL"
      schema `shouldSatisfy` isInfixOf "account_state_count INTEGER NOT NULL"

queryContains :: Query -> String -> Expectation
queryContains sql fragment =
  show sql `shouldSatisfy` isInfixOf fragment

queryDoesNotContain :: Query -> String -> Expectation
queryDoesNotContain sql fragment =
  show sql `shouldNotSatisfy` isInfixOf fragment
