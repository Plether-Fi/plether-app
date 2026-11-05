module Plether.Database.InsightsSpec (spec) where

import Data.Aeson (object, (.=))
import Data.List (isInfixOf)
import Database.PostgreSQL.Simple (Query)
import Plether.Database.Insights
  ( InsightsActivityRow (..)
  , integrityCalculationSql
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

  describe "competition result SQL source support (results are tested in Insights.DatabaseSpec)" $ do
    it "contains reviewed-participant and prize-qualification predicates" $ do
      queryContains leaderboardQuerySql "eligibility_status = 'eligible'"
      queryContains leaderboardQuerySql "active_days >= competition_minimum_active_days"
      queryContains leaderboardQuerySql "pc.prize_place <= competition_prize_places"
      queryContains leaderboardQuerySql "(t.fifth_prize_assets > 0)::INT"

    it "references the exact code-defined profit threshold" $ do
      queryContains leaderboardQuerySql "?::NUMERIC AS code_minimum_profit_assets"
      queryContains leaderboardQuerySql "final_pnl_assets >= competition_minimum_profit_assets"
      queryDoesNotContain leaderboardQuerySql
        "competition_starting_balance_assets * competition_minimum_profit_bps"

    it "contains separate July zero-trade and September ranking clauses" $ do
      queryContains leaderboardQuerySql
        "competition_slug = 'testnet-trading-2026' AND executed_trades = 0"
      queryContains leaderboardOrderBySql
        "competition_slug = 'testnet-trading-2026' AND executed_trades = 0"
      queryDoesNotContain leaderboardQuerySql
        "final_pnl_assets IS NULL OR executed_trades = 0 THEN NULL"
      queryContains leaderboardOrderBySql "final_pnl_assets DESC NULLS LAST"

    it "contains published-batch upper bounds for activity projections" $ do
      queryContains leaderboardQuerySql "a.block_number <= cb.block_number"
      queryContains walletActivityQuerySql "a.block_number <= b.block_number"

    it "selects execution fee and signed VPI expressions for wallet activity" $ do
      queryContains walletActivityQuerySql "a.data->>'executionFeeAssets'"
      queryContains walletActivityQuerySql "a.data->>'vpiAssets'"
      queryContains insightsDataStatusQuerySql "perps-history-costs-v2:finalized-abi3:"

    it "serializes execution fee and signed VPI as lossless decimal strings" $ do
      activityRowToJson
        InsightsActivityRow
          { iarActivityType = "Close"
          , iarSide = Just 1
          , iarPrice = Just 96_866_388
          , iarSizeDelta = Just 4_513_034_696_886_011_329_166_042
          , iarAmountAssets = Nothing
          , iarPnlAssets = Just 3_424_490_727
          , iarExecutionFeeAssets = Just 1_748_645_480
          , iarVpiAssets = Just (-4_487_207_153)
          , iarTxHash = "0xabc"
          , iarBlockNumber = 290_862_399
          , iarTimestamp = 1_784_901_245
          , iarLogIndex = 16
          , iarExecution = Nothing
          , iarSessionDay = Just "2026-07-24"
          }
        `shouldBe` object
          [ "activityType" .= ("Close" :: String)
          , "side" .= (1 :: Int)
          , "price" .= ("96866388" :: String)
          , "sizeDelta" .= ("4513034696886011329166042" :: String)
          , "pnlAssets" .= ("3424490727" :: String)
          , "executionFeeAssets" .= ("1748645480" :: String)
          , "vpiAssets" .= ("-4487207153" :: String)
          , "txHash" .= ("0xabc" :: String)
          , "blockNumber" .= ("290862399" :: String)
          , "timestamp" .= (1_784_901_245 :: Integer)
          , "occurredAt" .= ("2026-07-24T13:54:05Z" :: String)
          , "logIndex" .= (16 :: Integer)
          , "sessionDay" .= ("2026-07-24" :: String)
          ]

    it "contains the explicit legacy missing-asset exception" $ do
      queryContains leaderboardQuerySql "a.activity_type IN ('Deposit', 'Withdraw')"
      queryContains leaderboardQuerySql
        "t.slug = 'testnet-trading-2026' AND NOT jsonb_exists(a.data, 'asset')"

    it "contains exact USDG filtering without a generic missing-asset exception" $ do
      queryContains leaderboardQuerySql
        "LOWER(COALESCE(a.data->>'asset', '')) = LOWER(t.settlement_token_address)"
      queryDoesNotContain leaderboardQuerySql
        "OR NOT jsonb_exists(a.data, 'asset')"

    it "contains the realized directional P&L aggregate" $
      queryContains leaderboardQuerySql "SUM(a.pnl_assets) FILTER (WHERE a.activity_type IN ('Close', 'Liquidated'))"

    it "omits the obsolete nonzero-account-state predicate" $ do
      queryDoesNotContain leaderboardQuerySql "b.account_state_count > 0 OR NOT EXISTS"
      queryDoesNotContain walletActivityQuerySql "b.account_state_count > 0 OR NOT EXISTS"

    it "contains account-lens identity predicates" $ do
      queryContains leaderboardQuerySql "LOWER(b.account_lens_address) = LOWER(t.account_lens_address)"
      queryContains walletActivityQuerySql "LOWER(b.account_lens_address) = LOWER(t.account_lens_address)"

    it "references stored UTC session-boundary columns" $ do
      queryContains leaderboardQuerySql "t.fx_session_boundary_utc_minutes"
      queryContains walletActivityQuerySql "c.fx_session_boundary_utc_minutes"

    it "contains funding-integrity inputs and the separate eligibility predicate" $ do
      queryContains integrityCalculationSql "official_allocation_count_invalid"
      queryContains integrityCalculationSql "unverified_funding_flow"
      queryContains integrityCalculationSql "baseline_open_position"
      queryContains integrityCalculationSql "baseline_pending_orders"
      queryContains integrityCalculationSql "JOIN testnet_faucet_claims fc"
      queryContains integrityCalculationSql "fc.status = 'success'"
      queryContains integrityCalculationSql "fc.amount = p.starting_balance_assets"
      queryContains integrityCalculationSql "JOIN perps_asset_transfers x"
      queryContains integrityCalculationSql
        "ROW(f.block_number,f.tx_index,f.log_index) > ROW(f.mint_block,f.mint_tx,f.mint_log)"
      queryContains integrityCalculationSql "d.transfer_log_index = x.log_index"
      queryContains integrityCalculationSql "official_funds_left_before_allocation"
      queryContains leaderboardQuerySql "jsonb_array_length(integrity_flags) = 0"
      queryContains leaderboardQuerySql "AS funding_integrity_clear"

    it "contains canonical mint provenance predicates" $ do
      queryContains integrityCalculationSql "fc.mint_block_number IS NOT NULL"
      queryContains integrityCalculationSql "x.block_number = fc.mint_block_number"
      queryContains integrityCalculationSql "missing_verified_registration"
      queryDoesNotContain integrityCalculationSql "pre_registration_activity"
      queryDoesNotContain integrityCalculationSql "r.wallet_verification_block"
      queryDoesNotContain integrityCalculationSql
        "e.timestamp <= FLOOR(EXTRACT(EPOCH FROM r.completed_at))"

    it "never reports a later integrity-flagged participant as prize eligible" $ do
      prizeEligibleAfterIntegrityReview True True False `shouldBe` False
      prizeEligibleAfterIntegrityReview True True True `shouldBe` True

    it "contains the non-finalized integrity recalculation guard" $ do
      queryContains integrityCalculationSql "c.slug = ? AND c.finalized = FALSE"

    it "disables legacy admin registration for verified-registration competitions" $ do
      manualRosterInsertionAllowed Nothing `shouldBe` True
      manualRosterInsertionAllowed (Just 1_790_370_000) `shouldBe` False

  describe "Insights load-control SQL source support" $ do
    it "references batch metadata and omits account snapshot scans" $ do
      queryContains insightsDataStatusQuerySql "FROM insights_snapshot_batches b"
      queryContains insightsDataStatusQuerySql "MAX(b.participant_count)"
      queryContains insightsDataStatusQuerySql "MAX(b.participant_count) FILTER (WHERE b.snapshot_kind = 'start'"
      queryContains insightsDataStatusQuerySql "MAX(b.participant_count) FILTER (WHERE b.snapshot_kind = 'final'"
      queryContains insightsDataStatusQuerySql "MAX(b.published_at)"
      queryContains insightsDataStatusQuerySql "b.chain_id = t.chain_id AND b.release_router = t.release_router"
      queryContains insightsDataStatusQuerySql "LOWER(b.account_lens_address) = LOWER(t.account_lens_address)"
      queryDoesNotContain insightsDataStatusQuerySql "b.account_state_count > 0 OR NOT EXISTS"
      queryDoesNotContain insightsDataStatusQuerySql "insights_account_snapshots"

    it "declares an IF NOT EXISTS index over batch-scope columns" $ do
      queryContains snapshotBatchAccessIndexSql "CREATE INDEX IF NOT EXISTS"
      queryContains snapshotBatchAccessIndexSql
        "insights_account_snapshots(competition_slug, snapshot_kind, block_number, wallet)"

    it "includes roster and distinct snapshot-wallet counts in completeness SQL" $ do
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
