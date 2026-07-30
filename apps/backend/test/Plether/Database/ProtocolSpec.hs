module Plether.Database.ProtocolSpec (spec) where

import Data.List (isInfixOf)
import Database.PostgreSQL.Simple (Query)
import Plether.Database.Protocol
  ( keeperActionsQuerySql
  , keeperAggregatesQuerySql
  , keeperAggregatesPageQuerySql
  , keeperLatencySamplesQuerySql
  , keeperLatencyPercentilesQuerySql
  , keeperNativeCostSummaryQuerySql
  , keeperNativeCostsQuerySql
  , keeperNativeCostsForActorsQuerySql
  , keeperRewardLeadersQuerySql
  , keeperWindowSummaryQuerySql
  , listProtocolActionsQuerySql
  , operationalWalletActivityQuerySql
  , operationalWalletActionsQuerySql
  , operationalWalletCostsForActorsQuerySql
  , parameterChangesQuerySql
  , protocolActionsByOrderQuerySql
  , protocolActionsByTransactionQuerySql
  , protocolBlockCheckpointDeleteSql
  , protocolBlockCheckpointSchemaSql
  , protocolBlockCheckpointUpsertSql
  , protocolBlockCheckpointsDescendingSql
  , protocolEventsByTransactionQuerySql
  , protocolOverviewCountsQuerySql
  , protocolPendingOrderTimesQuerySql
  , protocolProjectionHeadQuerySql
  , protocolStateSnapshotsAtBlocksQuerySql
  , protocolStateSnapshotsPageQuerySql
  , protocolStateSnapshotsQuerySql
  , protocolTransactionQuerySql
  , trancheActionsQuerySql
  )
import Plether.Database.ProtocolParameterChanges
  ( parameterChangeAlreadyProjectedSql
  , parameterChangeRebuildActionsSql
  )
import Test.Hspec

spec :: Spec
spec = do
  describe "protocol activity feed SQL" $ do
    it "matches the global address filter against either account or actor" $
      queryContains
        listProtocolActionsQuerySql
        "(?::text IS NULL OR feed.account = LOWER(?) OR feed.actor = LOWER(?))"

    it "preserves dedicated account, keeper, contract, outcome, and cursor filters" $ do
      queryContains listProtocolActionsQuerySql "(?::text IS NULL OR feed.account = LOWER(?))"
      queryContains listProtocolActionsQuerySql "(?::text IS NULL OR feed.actor = LOWER(?))"
      queryContains listProtocolActionsQuerySql "(?::text IS NULL OR feed.contract_address = LOWER(?))"
      queryContains listProtocolActionsQuerySql "(?::text IS NULL OR feed.status = ?)"
      queryContains
        listProtocolActionsQuerySql
        "(?::bigint IS NULL OR feed.block_number < ?"

    it "caps the feed at the response confirmed block before applying its keyset" $ do
      queryContains listProtocolActionsQuerySql "feed.block_number <= ?"
      queryContains
        listProtocolActionsQuerySql
        "feed.block_number = ? AND feed.log_index < ?"
      queryContains
        listProtocolActionsQuerySql
        "ORDER BY feed.block_number DESC, feed.log_index DESC"

    it "surfaces monitored logs without typed actions as stable unavailable feed rows" $ do
      queryContains listProtocolActionsQuerySql "FROM protocol_events events"
      queryContains listProtocolActionsQuerySql "WHERE NOT EXISTS"
      queryContains
        listProtocolActionsQuerySql
        "events.tx_hash || ':' || events.log_index::text"
      queryContains listProtocolActionsQuerySql "'unclassified_event', 'unavailable'"
      queryContains listProtocolActionsQuerySql "'rawTopics', events.raw_topics"

  describe "confirmed-block protocol collection SQL" $ do
    it "caps transaction, order, transaction-detail, and event reads" $ do
      queryContains protocolActionsByTransactionQuerySql "actions.block_number <= ?"
      queryContains protocolActionsByOrderQuerySql "actions.block_number <= ?"
      queryContains protocolTransactionQuerySql "transactions.block_number <= ?"
      queryContains protocolEventsByTransactionQuerySql "events.block_number <= ?"

    it "caps overview and pending-order facts" $ do
      queryContains protocolOverviewCountsQuerySql "actions.block_number <= ?"
      queryContains protocolPendingOrderTimesQuerySql "orders.commit_block_number <= ?"
      queryContains
        protocolPendingOrderTimesQuerySql
        "orders.terminal_block_number IS NULL OR orders.terminal_block_number > ?"
      queryDoesNotContain
        protocolPendingOrderTimesQuerySql
        "orders.terminal_status = 'Pending'"

    it "caps state-snapshot history" $
      queryContains protocolStateSnapshotsQuerySql "snapshots.block_number <= ?"

    it "caps and keyset-pages snapshot history by an exclusive block cursor" $ do
      queryContains protocolStateSnapshotsPageQuerySql "snapshots.block_number <= ?"
      queryContains
        protocolStateSnapshotsPageQuerySql
        "(?::bigint IS NULL OR snapshots.block_number < ?)"
      queryContains
        protocolStateSnapshotsPageQuerySql
        "ORDER BY snapshots.block_number DESC LIMIT ?"

    it "reads companion scopes only at requested confirmed checkpoint blocks" $ do
      queryContains protocolStateSnapshotsAtBlocksQuerySql "snapshots.block_number <= ?"
      queryContains
        protocolStateSnapshotsAtBlocksQuerySql
        "snapshots.block_number IN ?"
      queryContains
        protocolStateSnapshotsAtBlocksQuerySql
        "ORDER BY snapshots.block_number DESC"

  describe "confirmed-block keeper SQL" $ do
    it "caps aggregates, latency endpoints, and transaction costs" $ do
      queryContains keeperAggregatesQuerySql "actions.block_number <= ?"
      queryContains keeperAggregatesPageQuerySql "actions.block_number <= ?"
      queryContains keeperWindowSummaryQuerySql "actions.block_number <= ?"
      queryContains keeperRewardLeadersQuerySql "actions.block_number <= ?"
      queryContains keeperLatencySamplesQuerySql "terminal.block_number <= ?"
      queryContains keeperLatencySamplesQuerySql "committed.block_number <= ?"
      queryContains keeperLatencyPercentilesQuerySql "terminal.block_number <= ?"
      queryContains keeperLatencyPercentilesQuerySql "committed.block_number <= ?"
      queryContains keeperNativeCostsQuerySql "actions.block_number <= ?"
      queryContains keeperNativeCostsQuerySql "transactions.block_number <= ?"
      queryContains keeperNativeCostsQuerySql "LEFT JOIN protocol_transactions transactions"
      queryContains keeperNativeCostSummaryQuerySql "actions.block_number <= ?"
      queryContains keeperNativeCostSummaryQuerySql "transactions.block_number <= ?"
      queryContains keeperNativeCostsForActorsQuerySql "actions.block_number <= ?"
      queryContains keeperNativeCostsForActorsQuerySql "transactions.block_number <= ?"

    it "defines keeper activity from successful onchain actions only" $ do
      queryContains keeperAggregatesQuerySql "actions.status = 'success'"
      queryContains keeperAggregatesPageQuerySql "actions.status = 'success'"
      queryContains keeperWindowSummaryQuerySql "actions.status = 'success'"
      queryContains keeperRewardLeadersQuerySql "actions.status = 'success'"
      queryContains keeperActionsQuerySql "actions.status = 'success'"
      queryContains keeperLatencySamplesQuerySql "terminal.status = 'success'"
      queryContains keeperLatencyPercentilesQuerySql "terminal.status = 'success'"
      queryContains keeperNativeCostsQuerySql "actions.status = 'success'"
      queryContains keeperNativeCostSummaryQuerySql "actions.status = 'success'"
      queryContains keeperNativeCostsForActorsQuerySql "actions.status = 'success'"

    it "uses a deterministic reward/action/address keyset for keeper list pages" $ do
      queryContains keeperAggregatesPageQuerySql "observed_rewards < ?"
      queryContains
        keeperAggregatesPageQuerySql
        "observed_rewards = ? AND action_count < ?"
      queryContains
        keeperAggregatesPageQuerySql
        "observed_rewards = ? AND action_count = ? AND actor > LOWER(?)"
      queryContains
        keeperAggregatesPageQuerySql
        "ORDER BY observed_rewards DESC, action_count DESC, actor ASC LIMIT ?"

    it "computes window-wide keeper metrics without materializing every keeper row" $ do
      queryContains keeperWindowSummaryQuerySql "SELECT COUNT(*)::BIGINT"
      queryContains keeperRewardLeadersQuerySql "LIMIT 8"
      queryContains keeperLatencyPercentilesQuerySql "PERCENTILE_DISC(0.50)"
      queryContains keeperLatencyPercentilesQuerySql "PERCENTILE_DISC(0.99)"
      queryContains keeperNativeCostsForActorsQuerySql "actions.actor IN ?"

    it "uses a descending block/log keyset for keeper action pages" $ do
      queryContains keeperActionsQuerySql "actions.block_number <= ?"
      queryContains keeperActionsQuerySql "actions.block_number < ?"
      queryContains
        keeperActionsQuerySql
        "actions.block_number = ? AND actions.log_index < ?"
      queryContains
        keeperActionsQuerySql
        "ORDER BY actions.block_number DESC, actions.log_index DESC"

  describe "confirmed-block operational-wallet SQL" $ do
    it "anchors observed roles and gross native-spend samples to successful confirmed activity" $ do
      queryContains operationalWalletActivityQuerySql "actions.block_number <= ?"
      queryContains operationalWalletActivityQuerySql "actions.status = 'success'"
      queryContains operationalWalletActivityQuerySql "'order_execution'"
      queryContains operationalWalletActivityQuerySql "'governance_execution'"
      queryDoesNotContain operationalWalletActivityQuerySql "'order_commitment'"
      queryContains operationalWalletCostsForActorsQuerySql "actions.block_number <= ?"
      queryContains operationalWalletCostsForActorsQuerySql "transactions.block_number <= ?"
      queryContains operationalWalletCostsForActorsQuerySql "actions.status = 'success'"

    it "samples only liveness/governance operations and includes transaction native value" $ do
      queryContains operationalWalletCostsForActorsQuerySql "'order_execution'"
      queryContains operationalWalletCostsForActorsQuerySql "'keeper_maintenance'"
      queryContains operationalWalletCostsForActorsQuerySql "'liquidation'"
      queryContains operationalWalletCostsForActorsQuerySql "'governance_execution'"
      queryDoesNotContain operationalWalletCostsForActorsQuerySql "'order_commitment'"
      queryContains
        operationalWalletCostsForActorsQuerySql
        "transactions.gas_used * transactions.effective_gas_price"
      queryContains
        operationalWalletCostsForActorsQuerySql
        "+ transactions.native_value"
      queryContains operationalWalletCostsForActorsQuerySql "PERCENTILE_DISC(0.50)"

    it "paginates only operational actions and excludes trader commitments" $ do
      queryContains operationalWalletActionsQuerySql "'order_execution'"
      queryContains operationalWalletActionsQuerySql "'governance_execution'"
      queryContains operationalWalletActionsQuerySql "'liquidation'"
      queryDoesNotContain operationalWalletActionsQuerySql "'order_commitment'"
      queryContains operationalWalletActionsQuerySql "actions.block_number < ?"
      queryContains
        operationalWalletActionsQuerySql
        "actions.block_number = ? AND actions.log_index < ?"
      queryContains
        operationalWalletActionsQuerySql
        "ORDER BY actions.block_number DESC, actions.log_index DESC"

  describe "confirmed-block tranche SQL" $ do
    it "caps tranche history and uses a descending block/log keyset" $ do
      queryContains trancheActionsQuerySql "feed.block_number <= ?"
      queryContains trancheActionsQuerySql "feed.block_number < ?"
      queryContains
        trancheActionsQuerySql
        "feed.block_number = ? AND feed.log_index < ?"
      queryContains
        trancheActionsQuerySql
        "ORDER BY feed.block_number DESC, feed.log_index DESC"

  describe "governance parameter history SQL" $ do
    it "returns provenance, lifecycle correlation, units, and projected status" $ do
      queryContains parameterChangesQuerySql "'category', category_key"
      queryContains parameterChangesQuerySql "'proposedValue', proposed_value"
      queryContains parameterChangesQuerySql "'proposalTxHash', proposal_tx_hash"
      queryContains parameterChangesQuerySql "'terminalTxHash', terminal_tx_hash"
      queryContains parameterChangesQuerySql "'availability', availability"
      queryContains parameterChangesQuerySql "'status', status"

    it "caps confirmed history and pages deterministically in mixed sort directions" $ do
      queryContains parameterChangesQuerySql "changes.block_number <= ?"
      queryContains parameterChangesQuerySql "changes.block_number < ?"
      queryContains
        parameterChangesQuerySql
        "changes.block_number = ? AND changes.change_id > ?"
      queryContains
        parameterChangesQuerySql
        "ORDER BY changes.block_number DESC, changes.change_id ASC"

    it "makes retries idempotent across proposal and terminal lifecycle actions" $ do
      queryContains parameterChangeAlreadyProjectedSql "source_action_id = ?"
      queryContains
        parameterChangeAlreadyProjectedSql
        "terminal_source_action_id = ?"

    it "rebuilds only governance actions in canonical ledger order" $ do
      queryContains parameterChangeRebuildActionsSql "FROM protocol_actions"
      queryContains parameterChangeRebuildActionsSql "'governance_proposal'"
      queryContains parameterChangeRebuildActionsSql "'ownership_transfer_started'"
      queryContains
        parameterChangeRebuildActionsSql
        "ORDER BY block_number ASC, log_index ASC"

  describe "protocol indexed block checkpoint SQL" $ do
    it "defines a release/indexer-scoped canonical block table" $ do
      queryContains protocolBlockCheckpointSchemaSql "CREATE TABLE IF NOT EXISTS protocol_indexed_blocks"
      queryContains
        protocolBlockCheckpointSchemaSql
        "PRIMARY KEY (release_id, indexer_name, block_number)"

    it "scopes canonical checkpoints by release, indexer, and block" $ do
      queryContains protocolBlockCheckpointUpsertSql "protocol_indexed_blocks"
      queryContains
        protocolBlockCheckpointUpsertSql
        "ON CONFLICT (release_id, indexer_name, block_number)"

    it "walks candidates newest-first without crossing the cursor" $ do
      queryContains
        protocolBlockCheckpointsDescendingSql
        "release_id = ? AND indexer_name = ? AND block_number <= ?"
      queryContains protocolBlockCheckpointsDescendingSql "ORDER BY block_number DESC"

    it "deletes every stale checkpoint at and above the rewind boundary" $
      queryContains
        protocolBlockCheckpointDeleteSql
        "release_id = ? AND indexer_name = ? AND block_number >= ?"

  describe "protocol projection completeness head SQL" $ do
    it "requires the release and the concrete contiguous indexer identity" $ do
      queryContains
        protocolProjectionHeadQuerySql
        "WHERE release_id = ? AND indexer_name = ?"
      queryDoesNotContain
        protocolProjectionHeadQuerySql
        "ORDER BY updated_at DESC"

queryContains :: Query -> String -> Expectation
queryContains sql fragment =
  show sql `shouldSatisfy` isInfixOf fragment

queryDoesNotContain :: Query -> String -> Expectation
queryDoesNotContain sql fragment =
  show sql `shouldNotSatisfy` isInfixOf fragment
