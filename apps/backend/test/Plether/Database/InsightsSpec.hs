module Plether.Database.InsightsSpec (spec) where

import Data.Aeson (object, (.=))
import Data.List (isInfixOf)
import Database.PostgreSQL.Simple (Query)
import Plether.Database.Insights
  ( InsightsActivityRow (..)
  , fundingIntegrityRefreshSql
  , hasCompleteAccountSnapshotBatchQuerySql
  , insightsDataStatusQuerySql
  , leaderboardQuerySql
  , leaderboardOrderBySql
  , leaderboardSearchPattern
  , manualRosterInsertionAllowed
  , snapshotBatchAccessIndexSql
  , walletActivityQuerySql
  )
import Plether.Handlers.Insights (activityRowToJson, prizeEligibleAfterIntegrityReview)
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
      queryContains leaderboardQuerySql "pc.prize_place <= competition_prize_places"
      queryContains leaderboardQuerySql "(t.fifth_prize_usdc > 0)::INT"

    it "uses the exact code-defined profit threshold instead of persisted basis points" $ do
      queryContains leaderboardQuerySql "?::NUMERIC AS code_minimum_profit_usdc"
      queryContains leaderboardQuerySql "final_pnl_usdc >= competition_minimum_profit_usdc"
      queryDoesNotContain leaderboardQuerySql
        "competition_starting_balance_usdc * competition_minimum_profit_bps"

    it "preserves July's zero-trade display while ranking every September participant" $ do
      queryContains leaderboardQuerySql
        "competition_slug = 'testnet-trading-2026' AND executed_trades = 0"
      queryContains leaderboardOrderBySql
        "competition_slug = 'testnet-trading-2026' AND executed_trades = 0"
      queryDoesNotContain leaderboardQuerySql
        "final_pnl_usdc IS NULL OR executed_trades = 0 THEN NULL"
      queryContains leaderboardOrderBySql "final_pnl_usdc DESC NULLS LAST"

    it "uses one published snapshot batch as the event projection upper bound" $ do
      queryContains leaderboardQuerySql "a.block_number <= cb.block_number"
      queryContains walletActivityQuerySql "a.block_number <= b.block_number"

    it "projects indexed execution fee and signed VPI into wallet activity" $ do
      queryContains walletActivityQuerySql "a.data->>'executionFeeUsdc'"
      queryContains walletActivityQuerySql "a.data->>'vpiUsdc'"
      queryContains insightsDataStatusQuerySql "perps-history-costs-v2:finalized-abi3:"

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
      queryContains leaderboardQuerySql
        "t.slug = 'testnet-trading-2026' AND NOT jsonb_exists(a.data, 'asset')"

    it "requires an explicit exact USDC asset for September cash flows" $ do
      queryContains leaderboardQuerySql
        "LOWER(COALESCE(a.data->>'asset', '')) = LOWER(t.usdc_address)"
      queryDoesNotContain leaderboardQuerySql
        "OR NOT jsonb_exists(a.data, 'asset')"

    it "aggregates realized directional P&L for the public net-P&L reconciliation" $
      queryContains leaderboardQuerySql "SUM(a.pnl_usdc) FILTER (WHERE a.activity_type IN ('Close', 'Liquidated'))"

    it "accepts a complete all-zero live or final account state" $ do
      queryDoesNotContain leaderboardQuerySql "b.account_state_count > 0 OR NOT EXISTS"
      queryDoesNotContain walletActivityQuerySql "b.account_state_count > 0 OR NOT EXISTS"

    it "reads snapshots only from the competition's recorded account lens" $ do
      queryContains leaderboardQuerySql "LOWER(b.account_lens_address) = LOWER(t.account_lens_address)"
      queryContains walletActivityQuerySql "LOWER(b.account_lens_address) = LOWER(t.account_lens_address)"

    it "derives FX days from each competition's stored UTC boundary" $ do
      queryContains leaderboardQuerySql "t.fx_session_boundary_utc_minutes"
      queryContains walletActivityQuerySql "c.fx_session_boundary_utc_minutes"

    it "keeps funding-capacity review separate from cash-flow-adjusted scoring" $ do
      queryContains fundingIntegrityRefreshSql "official_allocation_count_invalid"
      queryContains fundingIntegrityRefreshSql "unverified_funding_flow"
      queryContains fundingIntegrityRefreshSql "baseline_open_position"
      queryContains fundingIntegrityRefreshSql "baseline_pending_orders"
      queryContains fundingIntegrityRefreshSql "JOIN testnet_faucet_claims fc"
      queryContains fundingIntegrityRefreshSql "fc.status = 'success'"
      queryContains fundingIntegrityRefreshSql "fc.amount = p.starting_balance_usdc"
      queryContains fundingIntegrityRefreshSql "JOIN perps_usdc_transfers x"
      queryContains fundingIntegrityRefreshSql
        "ROW(f.block_number, f.tx_index, f.log_index) > ROW(m.block_number, m.tx_index, m.log_index)"
      queryContains fundingIntegrityRefreshSql "f.transfer_log_index = x.log_index"
      queryContains fundingIntegrityRefreshSql "official_funds_left_before_allocation"
      queryContains leaderboardQuerySql "jsonb_array_length(integrity_flags) = 0"
      queryContains leaderboardQuerySql "AS funding_integrity_clear"

    it "binds verified registration and faucet provenance to canonical block evidence" $ do
      queryContains fundingIntegrityRefreshSql "fc.mint_block_number IS NOT NULL"
      queryContains fundingIntegrityRefreshSql "x.block_number = fc.mint_block_number"
      queryContains fundingIntegrityRefreshSql "missing_verified_registration"
      queryDoesNotContain fundingIntegrityRefreshSql "pre_registration_activity"
      queryDoesNotContain fundingIntegrityRefreshSql "r.wallet_verification_block"
      queryDoesNotContain fundingIntegrityRefreshSql
        "e.timestamp <= FLOOR(EXTRACT(EPOCH FROM r.completed_at))"

    it "never reports a later integrity-flagged participant as prize eligible" $ do
      prizeEligibleAfterIntegrityReview True True False `shouldBe` False
      prizeEligibleAfterIntegrityReview True True True `shouldBe` True

    it "never recomputes funding flags for finalized historical standings" $ do
      queryContains fundingIntegrityRefreshSql "c.slug = ? AND c.finalized = FALSE"

    it "disables legacy admin registration for verified-registration competitions" $ do
      manualRosterInsertionAllowed Nothing `shouldBe` True
      manualRosterInsertionAllowed (Just 1_790_370_000) `shouldBe` False

  describe "Insights load-control SQL" $ do
    it "derives data status from published batch metadata without scanning account snapshots" $ do
      queryContains insightsDataStatusQuerySql "FROM insights_snapshot_batches b"
      queryContains insightsDataStatusQuerySql "MAX(b.participant_count)"
      queryContains insightsDataStatusQuerySql "MAX(b.participant_count) FILTER (WHERE b.snapshot_kind = 'start'"
      queryContains insightsDataStatusQuerySql "MAX(b.participant_count) FILTER (WHERE b.snapshot_kind = 'final'"
      queryContains insightsDataStatusQuerySql "MAX(b.published_at)"
      queryContains insightsDataStatusQuerySql "b.chain_id = t.chain_id AND b.release_router = t.release_router"
      queryContains insightsDataStatusQuerySql "LOWER(b.account_lens_address) = LOWER(t.account_lens_address)"
      queryDoesNotContain insightsDataStatusQuerySql "b.account_state_count > 0 OR NOT EXISTS"
      queryDoesNotContain insightsDataStatusQuerySql "insights_account_snapshots"

    it "creates an idempotent index matching batch-scoped snapshot reads" $ do
      queryContains snapshotBatchAccessIndexSql "CREATE INDEX IF NOT EXISTS"
      queryContains snapshotBatchAccessIndexSql
        "insights_account_snapshots(competition_slug, snapshot_kind, block_number, wallet)"

    it "rebuilds start/live/final batches when a late registration enlarges the roster" $ do
      queryContains hasCompleteAccountSnapshotBatchQuerySql
        "SELECT COUNT(*) FROM insights_competition_participants"
      queryContains hasCompleteAccountSnapshotBatchQuerySql
        "SELECT COUNT(DISTINCT s.wallet) FROM insights_account_snapshots"

    it "keeps the static snapshot-batch schema aligned with runtime publication" $ do
      schema <- readFile "schema.sql"
      schema
        `shouldSatisfy` isInfixOf
          "release_router TEXT NOT NULL,\n    account_lens_address TEXT NOT NULL,\n    block_number BIGINT NOT NULL"
      schema `shouldSatisfy` isInfixOf "account_state_count INTEGER NOT NULL"
      schema `shouldSatisfy` isInfixOf "release_bound_at TIMESTAMPTZ"
      schema `shouldSatisfy` isInfixOf "CREATE TABLE IF NOT EXISTS insights_finalized_standings"
      schema `shouldSatisfy` isInfixOf "CONSTRAINT perps_indexer_state_release_scope CHECK"
      schema `shouldSatisfy` isInfixOf "OR (release_router IS NOT NULL AND configured_start_block > 0)"

queryContains :: Query -> String -> Expectation
queryContains sql fragment =
  show sql `shouldSatisfy` isInfixOf fragment

queryDoesNotContain :: Query -> String -> Expectation
queryDoesNotContain sql fragment =
  show sql `shouldNotSatisfy` isInfixOf fragment
