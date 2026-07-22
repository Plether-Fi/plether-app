module Plether.Database.InsightsSpec (spec) where

import Data.List (isInfixOf)
import Database.PostgreSQL.Simple (Query)
import Plether.Database.Insights
  ( insightsDataStatusQuerySql
  , leaderboardQuerySql
  , leaderboardOrderBySql
  , leaderboardSearchPattern
  , snapshotBatchAccessIndexSql
  , walletActivityQuerySql
  )
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
