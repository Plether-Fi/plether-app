\set ON_ERROR_STOP on

-- Plether Transaction History Schema

CREATE TABLE IF NOT EXISTS transactions (
    id SERIAL PRIMARY KEY,
    tx_hash VARCHAR(66) NOT NULL,
    block_number BIGINT NOT NULL,
    timestamp BIGINT NOT NULL,
    user_address VARCHAR(42) NOT NULL,
    tx_type VARCHAR(32) NOT NULL,
    side VARCHAR(4),
    status VARCHAR(16) NOT NULL DEFAULT 'success',
    data JSONB NOT NULL,
    created_at TIMESTAMP DEFAULT NOW(),
    UNIQUE (tx_hash, tx_type)
);

CREATE INDEX IF NOT EXISTS idx_transactions_user ON transactions(user_address);
CREATE INDEX IF NOT EXISTS idx_transactions_type ON transactions(tx_type);
CREATE INDEX IF NOT EXISTS idx_transactions_block ON transactions(block_number DESC);
CREATE INDEX IF NOT EXISTS idx_transactions_user_block ON transactions(user_address, block_number DESC);

-- Track indexer state
CREATE TABLE IF NOT EXISTS indexer_state (
    id INTEGER PRIMARY KEY DEFAULT 1,
    last_indexed_block BIGINT NOT NULL,
    updated_at TIMESTAMP DEFAULT NOW(),
    CONSTRAINT single_row CHECK (id = 1)
);

-- Initialize indexer state if not exists
INSERT INTO indexer_state (last_indexed_block) VALUES (0) ON CONFLICT DO NOTHING;

-- Price snapshots for 24h change tracking
CREATE TABLE IF NOT EXISTS price_snapshots (
    id SERIAL PRIMARY KEY,
    block_number BIGINT NOT NULL,
    timestamp BIGINT NOT NULL,
    oracle_price BIGINT NOT NULL,
    UNIQUE (block_number)
);
CREATE INDEX IF NOT EXISTS idx_snapshots_timestamp ON price_snapshots(timestamp DESC);

-- Staking exchange rate snapshots for 7d APY tracking
CREATE TABLE IF NOT EXISTS staking_snapshots (
    id SERIAL PRIMARY KEY,
    block_number BIGINT NOT NULL,
    timestamp BIGINT NOT NULL,
    bear_exchange_rate BIGINT NOT NULL,
    bull_exchange_rate BIGINT NOT NULL,
    UNIQUE (block_number)
);
CREATE INDEX IF NOT EXISTS idx_staking_snapshots_timestamp ON staking_snapshots(timestamp DESC);

-- Coherent hourly Senior/Junior vault observations used for realized 7d APY.
-- epoch_timestamp is the UTC sampling boundary; block_timestamp is the actual
-- time of the last canonical block at or before that boundary.
CREATE TABLE IF NOT EXISTS vault_performance_snapshots (
    chain_id NUMERIC(78,0) NOT NULL,
    house_pool_address VARCHAR(42) NOT NULL,
    senior_vault_address VARCHAR(42) NOT NULL,
    junior_vault_address VARCHAR(42) NOT NULL,
    epoch_timestamp BIGINT NOT NULL,
    block_number NUMERIC(78,0) NOT NULL,
    block_hash VARCHAR(66) NOT NULL,
    block_timestamp BIGINT NOT NULL,
    senior_total_assets NUMERIC(78,0) NOT NULL,
    senior_total_supply NUMERIC(78,0) NOT NULL,
    senior_share_price_wad NUMERIC(78,0) NOT NULL,
    junior_total_assets NUMERIC(78,0) NOT NULL,
    junior_total_supply NUMERIC(78,0) NOT NULL,
    junior_share_price_wad NUMERIC(78,0) NOT NULL,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    PRIMARY KEY (
        chain_id,
        house_pool_address,
        senior_vault_address,
        junior_vault_address,
        epoch_timestamp
    ),
    CHECK (chain_id > 0),
    CHECK (house_pool_address ~ '^0x[0-9a-f]{40}$'),
    CHECK (senior_vault_address ~ '^0x[0-9a-f]{40}$'),
    CHECK (junior_vault_address ~ '^0x[0-9a-f]{40}$'),
    CHECK (epoch_timestamp >= 0 AND epoch_timestamp % 3600 = 0),
    CHECK (block_timestamp >= 0),
    CHECK (block_timestamp <= epoch_timestamp),
    CHECK (block_number >= 0),
    CHECK (block_hash ~ '^0x[0-9a-f]{64}$'),
    CHECK (senior_total_assets >= 0),
    CHECK (senior_total_supply >= 0),
    CHECK (senior_share_price_wad >= 0),
    CHECK (junior_total_assets >= 0),
    CHECK (junior_total_supply >= 0),
    CHECK (junior_share_price_wad >= 0)
);
CREATE INDEX IF NOT EXISTS idx_vault_performance_deployment_epoch
    ON vault_performance_snapshots(
        chain_id,
        house_pool_address,
        senior_vault_address,
        junior_vault_address,
        epoch_timestamp DESC
    );

-- Perps DXY basket snapshots built from historical Pyth benchmark values
CREATE TABLE IF NOT EXISTS perps_basket_snapshots (
    id SERIAL PRIMARY KEY,
    timestamp BIGINT NOT NULL,
    interval_seconds INTEGER NOT NULL,
    basket_price BIGINT NOT NULL,
    component_prices JSONB NOT NULL,
    source VARCHAR(32) NOT NULL DEFAULT 'pyth_benchmarks',
    created_at TIMESTAMP DEFAULT NOW(),
    UNIQUE (timestamp, interval_seconds)
);
CREATE INDEX IF NOT EXISTS idx_perps_basket_snapshots_timestamp ON perps_basket_snapshots(timestamp DESC);

-- Sepolia testnet mock USDC faucet claims
CREATE TABLE IF NOT EXISTS testnet_faucet_claims (
    address VARCHAR(42) NOT NULL,
    amount BIGINT NOT NULL,
    token_address VARCHAR(42) NOT NULL,
    tx_hash VARCHAR(66),
    raw_tx TEXT,
    mint_block_number BIGINT,
    status VARCHAR(16) NOT NULL,
    error TEXT,
    created_at TIMESTAMP DEFAULT NOW(),
    updated_at TIMESTAMP DEFAULT NOW(),
    PRIMARY KEY (address, token_address)
);
CREATE INDEX IF NOT EXISTS idx_testnet_faucet_claims_status ON testnet_faucet_claims(status);

-- Release-scoped, reorg-replayed history used to prove competition funding.
CREATE TABLE IF NOT EXISTS perps_usdc_transfers (
    chain_id BIGINT NOT NULL,
    release_router TEXT NOT NULL,
    token_address TEXT NOT NULL,
    from_address TEXT NOT NULL,
    to_address TEXT NOT NULL,
    amount NUMERIC(78,0) NOT NULL,
    tx_hash TEXT NOT NULL,
    block_number BIGINT NOT NULL,
    block_hash TEXT NOT NULL,
    tx_index BIGINT NOT NULL,
    log_index BIGINT NOT NULL,
    timestamp BIGINT NOT NULL,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    PRIMARY KEY (chain_id, release_router, tx_hash, log_index),
    CONSTRAINT perps_usdc_transfers_canonical_values
        CHECK (chain_id > 0 AND amount >= 0 AND block_number >= 0 AND tx_index >= 0 AND log_index >= 0 AND timestamp >= 0),
    CONSTRAINT perps_usdc_transfers_canonical_addresses
        CHECK (release_router ~ '^0x[0-9a-f]{40}$' AND token_address ~ '^0x[0-9a-f]{40}$'
            AND from_address ~ '^0x[0-9a-f]{40}$' AND to_address ~ '^0x[0-9a-f]{40}$'),
    CONSTRAINT perps_usdc_transfers_canonical_hashes
        CHECK (tx_hash ~ '^0x[0-9a-f]{64}$' AND block_hash ~ '^0x[0-9a-f]{64}$')
);
CREATE INDEX IF NOT EXISTS idx_perps_usdc_transfers_inbound
    ON perps_usdc_transfers(chain_id, release_router, token_address, to_address, block_number, tx_index, log_index);
CREATE INDEX IF NOT EXISTS idx_perps_usdc_transfers_outbound
    ON perps_usdc_transfers(chain_id, release_router, token_address, from_address, block_number, tx_index, log_index);

CREATE TABLE IF NOT EXISTS perps_indexer_state (
    indexer_name TEXT NOT NULL,
    chain_id BIGINT NOT NULL,
    release_router TEXT,
    configured_start_block BIGINT,
    last_indexed_block BIGINT NOT NULL,
    last_indexed_block_hash TEXT,
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    PRIMARY KEY (indexer_name, chain_id),
    CONSTRAINT perps_indexer_state_release_scope CHECK (
        indexer_name NOT LIKE 'perps-history-costs-v1:%'
        OR (release_router IS NOT NULL AND configured_start_block > 0)
    ),
    CONSTRAINT perps_indexer_state_v2_release_scope CHECK (
        indexer_name NOT LIKE 'perps-history-costs-v2:%'
        OR (release_router IS NOT NULL AND configured_start_block > 0)
    )
);

-- Cached six-feed Pyth update payloads used by reveal payload APIs and keeper execution
CREATE TABLE IF NOT EXISTS perps_pyth_update_payloads (
    id SERIAL PRIMARY KEY,
    min_publish_time BIGINT NOT NULL,
    max_publish_time BIGINT NOT NULL,
    publish_times JSONB NOT NULL,
    update_data JSONB NOT NULL,
    source VARCHAR(32) NOT NULL DEFAULT 'backend_hermes',
    fetched_at BIGINT NOT NULL,
    created_at TIMESTAMP DEFAULT NOW(),
    UNIQUE (min_publish_time, max_publish_time)
);
CREATE INDEX IF NOT EXISTS idx_perps_pyth_update_payloads_window
    ON perps_pyth_update_payloads(min_publish_time, max_publish_time);

CREATE INDEX IF NOT EXISTS idx_perps_pyth_update_payloads_admitted_latest
    ON perps_pyth_update_payloads(max_publish_time DESC)
    WHERE source = 'backend_hermes_latest_v2';

-- Perps keeper indexer state
CREATE TABLE IF NOT EXISTS perps_keeper_state (
    id INTEGER DEFAULT 1,
    order_router TEXT NOT NULL,
    last_indexed_block BIGINT NOT NULL,
    updated_at TIMESTAMP DEFAULT NOW(),
    PRIMARY KEY (order_router)
);

INSERT INTO perps_keeper_state (id, order_router, last_indexed_block)
VALUES (1, '0x0000000000000000000000000000000000000000', 0)
ON CONFLICT DO NOTHING;

-- Perps keeper pending/executed/failed order queue
CREATE TABLE IF NOT EXISTS perps_keeper_orders (
    order_id BIGINT NOT NULL,
    order_router TEXT NOT NULL,
    account VARCHAR(42) NOT NULL,
    side INTEGER NOT NULL,
    commit_block BIGINT NOT NULL,
    commit_event_block BIGINT,
    commit_time BIGINT NOT NULL,
    commit_tx_hash VARCHAR(66) NOT NULL,
    status VARCHAR(16) NOT NULL DEFAULT 'pending',
    execution_tx_hash VARCHAR(66),
    execution_block BIGINT,
    execution_price NUMERIC(78,0),
    failure_tx_hash VARCHAR(66),
    failure_block BIGINT,
    failure_reason INTEGER,
    attempt_count INTEGER NOT NULL DEFAULT 0,
    last_error TEXT,
    last_attempt_at TIMESTAMP,
    created_at TIMESTAMP DEFAULT NOW(),
    updated_at TIMESTAMP DEFAULT NOW(),
    PRIMARY KEY (order_router, order_id)
);
CREATE INDEX IF NOT EXISTS idx_perps_keeper_orders_pending ON perps_keeper_orders(order_router, order_id ASC) WHERE status = 'pending';
CREATE INDEX IF NOT EXISTS idx_perps_keeper_orders_commit_block ON perps_keeper_orders(commit_block DESC);

-- Plether Insights competitions, participants, canonical account snapshots,
-- and review audit data. Competition metadata is inserted once from runtime
-- Rules are immutable from insertion. A registration-only competition may
-- bind its reviewed release exactly once before its baseline is resolved;
-- later starts validate that release rather than rewriting history.
CREATE TABLE IF NOT EXISTS insights_competitions (
    slug TEXT PRIMARY KEY,
    name TEXT NOT NULL,
    chain_id BIGINT NOT NULL,
    release_router TEXT NOT NULL,
    usdc_address TEXT NOT NULL,
    margin_clearinghouse_address TEXT NOT NULL,
    account_lens_address TEXT NOT NULL,
    release_manifest TEXT NOT NULL,
    -- Registration may open before the reviewed contract release exists.
    -- This becomes non-null exactly once, before baseline resolution.
    release_bound_at TIMESTAMPTZ,
    start_timestamp BIGINT NOT NULL,
    new_risk_cutoff_timestamp BIGINT NOT NULL,
    score_cutoff_timestamp BIGINT NOT NULL,
    results_timestamp BIGINT NOT NULL,
    payment_deadline_timestamp BIGINT NOT NULL,
    registration_open_timestamp BIGINT,
    registration_close_timestamp BIGINT,
    minimum_x_account_age_days INTEGER,
    target_x_handle TEXT,
    privacy_notice_version TEXT,
    start_block BIGINT,
    start_block_hash TEXT,
    start_snapshot_block_hash TEXT,
    score_cutoff_block BIGINT,
    score_cutoff_block_hash TEXT,
    starting_balance_usdc NUMERIC(78,0) NOT NULL,
    minimum_profit_bps BIGINT NOT NULL,
    minimum_active_days INTEGER NOT NULL,
    fx_session_boundary_utc_minutes INTEGER NOT NULL DEFAULT 1320,
    scoring_version TEXT NOT NULL,
    rules_version TEXT NOT NULL,
    first_prize_usdc NUMERIC(78,0) NOT NULL,
    second_prize_usdc NUMERIC(78,0) NOT NULL,
    third_prize_usdc NUMERIC(78,0) NOT NULL,
    fourth_prize_usdc NUMERIC(78,0) NOT NULL DEFAULT 0,
    fifth_prize_usdc NUMERIC(78,0) NOT NULL DEFAULT 0,
    finalized BOOLEAN NOT NULL DEFAULT FALSE,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    CHECK (new_risk_cutoff_timestamp >= start_timestamp),
    CHECK (score_cutoff_timestamp >= new_risk_cutoff_timestamp),
    CHECK (minimum_profit_bps >= 0),
    CHECK (minimum_active_days >= 0),
    CONSTRAINT insights_competitions_fx_session_boundary_valid
        CHECK (fx_session_boundary_utc_minutes >= 0 AND fx_session_boundary_utc_minutes < 1440),
    CONSTRAINT insights_competitions_registration_metadata_consistent CHECK (
        (registration_open_timestamp IS NULL AND registration_close_timestamp IS NULL
            AND minimum_x_account_age_days IS NULL AND target_x_handle IS NULL)
        OR (registration_close_timestamp IS NOT NULL
            AND (registration_open_timestamp IS NULL OR registration_open_timestamp < registration_close_timestamp)
            AND minimum_x_account_age_days IS NOT NULL AND minimum_x_account_age_days >= 0
            AND NULLIF(BTRIM(target_x_handle), '') IS NOT NULL)
    ),
    CONSTRAINT insights_competitions_registration_privacy_version_consistent CHECK (
        registration_open_timestamp IS NULL OR NULLIF(BTRIM(privacy_notice_version), '') IS NOT NULL
    )
);

ALTER TABLE insights_competitions
    ADD COLUMN IF NOT EXISTS fourth_prize_usdc NUMERIC(78,0) NOT NULL DEFAULT 0;
ALTER TABLE insights_competitions
    ADD COLUMN IF NOT EXISTS fifth_prize_usdc NUMERIC(78,0) NOT NULL DEFAULT 0;

CREATE TABLE IF NOT EXISTS insights_competition_participants (
    competition_slug TEXT NOT NULL REFERENCES insights_competitions(slug) ON DELETE CASCADE,
    wallet VARCHAR(42) NOT NULL,
    -- Private opaque registration identifier used to enforce one beneficial
    -- trader per entry. This is intentionally absent from public API queries.
    -- It remains nullable only so pre-existing development rows can migrate by
    -- being re-registered before finalization.
    trader_reference TEXT,
    alias TEXT,
    eligibility_status TEXT NOT NULL DEFAULT 'pending',
    eligibility_reason TEXT,
    integrity_flags JSONB NOT NULL DEFAULT '[]'::jsonb,
    registered_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    reviewed_at TIMESTAMPTZ,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    PRIMARY KEY (competition_slug, wallet),
    CHECK (eligibility_status IN ('pending', 'eligible', 'under_review', 'ineligible'))
);
CREATE INDEX IF NOT EXISTS idx_insights_participants_wallet
    ON insights_competition_participants(wallet);
CREATE UNIQUE INDEX IF NOT EXISTS idx_insights_participants_trader_reference
    ON insights_competition_participants(competition_slug, trader_reference)
    WHERE trader_reference IS NOT NULL;

CREATE TABLE IF NOT EXISTS insights_participant_wallet_remaps (
    competition_slug TEXT NOT NULL REFERENCES insights_competitions(slug) ON DELETE CASCADE,
    trader_reference TEXT NOT NULL,
    old_wallet VARCHAR(42) NOT NULL,
    new_wallet VARCHAR(42) NOT NULL,
    staged_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    applied_at TIMESTAMPTZ,
    applied_by TEXT,
    PRIMARY KEY (competition_slug, trader_reference),
    UNIQUE (competition_slug, new_wallet)
);

CREATE TABLE IF NOT EXISTS insights_account_snapshots (
    competition_slug TEXT NOT NULL REFERENCES insights_competitions(slug) ON DELETE CASCADE,
    wallet VARCHAR(42) NOT NULL,
    snapshot_kind TEXT NOT NULL,
    chain_id BIGINT NOT NULL,
    release_router TEXT NOT NULL,
    block_number BIGINT NOT NULL,
    block_hash TEXT NOT NULL,
    timestamp BIGINT NOT NULL,
    has_open_position BOOLEAN NOT NULL,
    signed_net_equity_usdc NUMERIC(78,0) NOT NULL,
    terminal_reachable_usdc NUMERIC(78,0) NOT NULL,
    trader_claims_usdc NUMERIC(78,0) NOT NULL,
    raw_data JSONB NOT NULL,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    PRIMARY KEY (competition_slug, wallet, snapshot_kind, block_number),
    CHECK (snapshot_kind IN ('start', 'live', 'final'))
);
CREATE INDEX IF NOT EXISTS idx_insights_snapshots_latest
    ON insights_account_snapshots(competition_slug, wallet, block_number DESC);
CREATE INDEX IF NOT EXISTS idx_insights_snapshots_kind
    ON insights_account_snapshots(competition_slug, snapshot_kind, wallet);
CREATE INDEX IF NOT EXISTS idx_insights_snapshots_batch_wallet
    ON insights_account_snapshots(competition_slug, snapshot_kind, block_number, wallet);

CREATE TABLE IF NOT EXISTS insights_snapshot_batches (
    competition_slug TEXT NOT NULL REFERENCES insights_competitions(slug) ON DELETE CASCADE,
    snapshot_kind TEXT NOT NULL,
    chain_id BIGINT NOT NULL,
    release_router TEXT NOT NULL,
    account_lens_address TEXT NOT NULL,
    block_number BIGINT NOT NULL,
    block_hash TEXT NOT NULL,
    timestamp BIGINT NOT NULL,
    participant_count INTEGER NOT NULL,
    account_state_count INTEGER NOT NULL,
    published_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    PRIMARY KEY (competition_slug, snapshot_kind, block_number),
    CHECK (snapshot_kind IN ('start', 'live', 'final')),
    CHECK (participant_count > 0)
);
CREATE INDEX IF NOT EXISTS idx_insights_snapshot_batches_latest
    ON insights_snapshot_batches(competition_slug, block_number DESC, published_at DESC);

CREATE TABLE IF NOT EXISTS insights_manual_adjustments (
    id BIGSERIAL PRIMARY KEY,
    competition_slug TEXT NOT NULL,
    wallet VARCHAR(42) NOT NULL,
    amount_usdc NUMERIC(78,0) NOT NULL,
    reason TEXT NOT NULL,
    created_by TEXT NOT NULL,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    voided_at TIMESTAMPTZ,
    voided_by TEXT,
    void_reason TEXT,
    FOREIGN KEY (competition_slug, wallet)
        REFERENCES insights_competition_participants(competition_slug, wallet) ON DELETE CASCADE
);
CREATE INDEX IF NOT EXISTS idx_insights_adjustments_wallet
    ON insights_manual_adjustments(competition_slug, wallet) WHERE voided_at IS NULL;

CREATE TABLE IF NOT EXISTS insights_eligibility_audit (
    id BIGSERIAL PRIMARY KEY,
    competition_slug TEXT NOT NULL,
    wallet VARCHAR(42) NOT NULL,
    previous_status TEXT NOT NULL,
    new_status TEXT NOT NULL,
    reason TEXT,
    reviewed_by TEXT NOT NULL,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    FOREIGN KEY (competition_slug, wallet)
        REFERENCES insights_competition_participants(competition_slug, wallet) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS insights_competition_finalization_audit (
    id BIGSERIAL PRIMARY KEY,
    competition_slug TEXT NOT NULL REFERENCES insights_competitions(slug) ON DELETE CASCADE,
    finalized_by TEXT NOT NULL,
    participant_count BIGINT NOT NULL,
    final_snapshot_block BIGINT NOT NULL,
    final_snapshot_hash TEXT NOT NULL,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE TABLE IF NOT EXISTS insights_finalized_standings (
    competition_slug TEXT NOT NULL REFERENCES insights_competitions(slug) ON DELETE CASCADE,
    competition_rank BIGINT,
    prize_place BIGINT,
    prize_tie_count BIGINT,
    wallet VARCHAR(42) NOT NULL,
    alias TEXT,
    eligibility_status TEXT NOT NULL,
    eligibility_reason TEXT,
    funding_integrity_clear BOOLEAN NOT NULL,
    final_pnl_usdc NUMERIC(78,0),
    roi_bps BIGINT,
    starting_value_usdc NUMERIC(78,0),
    current_value_usdc NUMERIC(78,0),
    deposits_usdc NUMERIC(78,0) NOT NULL,
    withdrawals_usdc NUMERIC(78,0) NOT NULL,
    adjustment_usdc NUMERIC(78,0) NOT NULL,
    active_days INTEGER NOT NULL,
    volume_usdc NUMERIC(78,0) NOT NULL,
    executed_trades BIGINT NOT NULL,
    liquidations BIGINT NOT NULL,
    realized_pnl_usdc NUMERIC(78,0) NOT NULL,
    block_number BIGINT,
    timestamp BIGINT,
    has_open_position BOOLEAN,
    snapshot_kind TEXT,
    position_side TEXT,
    position_size_delta TEXT,
    position_margin_usdc TEXT,
    position_entry_price TEXT,
    position_unrealized_pnl_usdc TEXT,
    position_liquidatable BOOLEAN,
    materialized_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    PRIMARY KEY (competition_slug, wallet)
);

-- Dedicated liquidation worker discovery cursor
CREATE TABLE IF NOT EXISTS perps_liquidation_state (
    chain_id BIGINT NOT NULL,
    cfd_engine TEXT NOT NULL,
    last_indexed_block BIGINT NOT NULL DEFAULT 0,
    rejected_payload_key TEXT,
    rejected_payload_selector TEXT,
    rejected_payload_error TEXT,
    rejected_payload_at TIMESTAMP,
    signer_retry_required_balance NUMERIC(78,0),
    signer_retry_error TEXT,
    signer_retry_at TIMESTAMP,
    updated_at TIMESTAMP DEFAULT NOW(),
    PRIMARY KEY (chain_id, cfd_engine)
);
ALTER TABLE perps_liquidation_state
    ADD COLUMN IF NOT EXISTS rejected_payload_key TEXT;
ALTER TABLE perps_liquidation_state
    ADD COLUMN IF NOT EXISTS rejected_payload_selector TEXT;
ALTER TABLE perps_liquidation_state
    ADD COLUMN IF NOT EXISTS rejected_payload_error TEXT;
ALTER TABLE perps_liquidation_state
    ADD COLUMN IF NOT EXISTS rejected_payload_at TIMESTAMP;
ALTER TABLE perps_liquidation_state
    ADD COLUMN IF NOT EXISTS signer_retry_required_balance NUMERIC(78,0);
ALTER TABLE perps_liquidation_state
    ADD COLUMN IF NOT EXISTS signer_retry_error TEXT;
ALTER TABLE perps_liquidation_state
    ADD COLUMN IF NOT EXISTS signer_retry_at TIMESTAMP;

-- Monotonic candidate registry. On-chain position state remains authoritative.
CREATE TABLE IF NOT EXISTS perps_liquidation_candidates (
    chain_id BIGINT NOT NULL,
    cfd_engine TEXT NOT NULL,
    account VARCHAR(42) NOT NULL,
    first_seen_block BIGINT NOT NULL,
    last_seen_block BIGINT NOT NULL,
    attempt_count INTEGER NOT NULL DEFAULT 0,
    last_checked_at TIMESTAMP,
    last_error TEXT,
    pending_tx_hash VARCHAR(66),
    pending_nonce BIGINT,
    pending_sender VARCHAR(42),
    pending_raw_tx TEXT,
    pending_call_data TEXT,
    pending_value NUMERIC(78,0),
    pending_gas_limit BIGINT,
    pending_max_priority_fee_per_gas NUMERIC(78,0),
    pending_max_fee_per_gas NUMERIC(78,0),
    pending_since TIMESTAMP,
    pending_last_broadcast_at TIMESTAMP,
    updated_at TIMESTAMP DEFAULT NOW(),
    PRIMARY KEY (chain_id, cfd_engine, account)
);
ALTER TABLE perps_liquidation_candidates
    ADD COLUMN IF NOT EXISTS pending_last_broadcast_at TIMESTAMP;
CREATE INDEX IF NOT EXISTS idx_perps_liquidation_candidates_scan
    ON perps_liquidation_candidates(chain_id, cfd_engine, last_checked_at ASC NULLS FIRST);
CREATE INDEX IF NOT EXISTS idx_perps_liquidation_candidates_pending
    ON perps_liquidation_candidates(chain_id, cfd_engine, pending_since ASC)
    WHERE pending_tx_hash IS NOT NULL;

-- Incrementally maintained Perps basket OHLCV read model. These five tables are
-- safe to bootstrap before the Perps history indexer schema exists. Historical
-- population and the concurrent perps_events/perps_account_activity backfill
-- and block-rewind indexes are installed by `plether-candle-admin migrate` after
-- ensurePerpsHistorySchema has created the source tables. Keeping CREATE INDEX
-- CONCURRENTLY out of this fresh bootstrap prevents references to source tables
-- that do not exist yet.

CREATE TABLE IF NOT EXISTS perps_basket_definitions (
    series_id TEXT PRIMARY KEY,
    definition_version TEXT NOT NULL,
    configuration_hash TEXT NOT NULL,
    configuration JSONB NOT NULL,
    effective_from BIGINT NOT NULL,
    effective_to BIGINT,
    active BOOLEAN NOT NULL DEFAULT TRUE,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    CHECK (configuration_hash ~ '^sha256:[0-9a-f]{64}$'),
    CHECK (effective_from >= 0),
    CHECK (effective_to IS NULL OR effective_to > effective_from)
);
CREATE INDEX IF NOT EXISTS idx_perps_basket_definitions_effective
    ON perps_basket_definitions(active, effective_from DESC);

CREATE TABLE IF NOT EXISTS perps_basket_observations (
    series_id TEXT NOT NULL REFERENCES perps_basket_definitions(series_id),
    observation_id TEXT NOT NULL,
    publish_time BIGINT NOT NULL,
    basket_price BIGINT NOT NULL,
    component_prices JSONB NOT NULL,
    source TEXT NOT NULL,
    source_priority INTEGER NOT NULL,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    PRIMARY KEY (series_id, observation_id)
);
CREATE INDEX IF NOT EXISTS idx_perps_basket_observations_series_time
    ON perps_basket_observations(series_id, publish_time, source_priority DESC, observation_id);

CREATE TABLE IF NOT EXISTS perps_basket_candles (
    series_id TEXT NOT NULL REFERENCES perps_basket_definitions(series_id),
    interval_seconds BIGINT NOT NULL,
    bucket_start BIGINT NOT NULL,
    raw_open_price BIGINT NOT NULL,
    raw_high_price BIGINT NOT NULL,
    raw_low_price BIGINT NOT NULL,
    raw_close_price BIGINT NOT NULL,
    first_observation_time BIGINT NOT NULL,
    last_observation_time BIGINT NOT NULL,
    sample_count INTEGER NOT NULL,
    quality TEXT NOT NULL,
    revision BIGINT NOT NULL DEFAULT 1,
    finalized BOOLEAN NOT NULL DEFAULT FALSE,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    PRIMARY KEY (series_id, interval_seconds, bucket_start),
    CHECK (interval_seconds IN (60, 180, 300, 900, 1800, 3600, 86400)),
    CHECK (bucket_start % interval_seconds = 0),
    CHECK (sample_count > 0),
    CHECK (revision > 0),
    CHECK (quality IN ('observed', 'legacy_sampled', 'mixed')),
    CHECK (raw_high_price >= GREATEST(raw_open_price, raw_close_price)),
    CHECK (raw_low_price <= LEAST(raw_open_price, raw_close_price)),
    CHECK (last_observation_time >= first_observation_time)
);
CREATE INDEX IF NOT EXISTS idx_perps_basket_candles_page_cover
    ON perps_basket_candles(series_id, interval_seconds, bucket_start)
    INCLUDE (raw_open_price, raw_high_price, raw_low_price, raw_close_price,
             sample_count, quality, revision, finalized);

CREATE TABLE IF NOT EXISTS perps_market_volume_rollups (
    chain_id BIGINT NOT NULL,
    release_router TEXT NOT NULL,
    interval_seconds BIGINT NOT NULL,
    bucket_start BIGINT NOT NULL,
    -- Exact ABS(size_delta) * price numerator; division happens only at API output.
    volume_numerator NUMERIC(78,0) NOT NULL,
    trade_count BIGINT NOT NULL,
    first_source_block BIGINT NOT NULL,
    last_source_block BIGINT NOT NULL,
    revision BIGINT NOT NULL DEFAULT 1,
    finalized BOOLEAN NOT NULL DEFAULT FALSE,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    PRIMARY KEY (chain_id, release_router, interval_seconds, bucket_start),
    CHECK (interval_seconds IN (60, 180, 300, 900, 1800, 3600, 86400)),
    CHECK (bucket_start % interval_seconds = 0),
    CHECK (volume_numerator >= 0),
    CHECK (trade_count > 0),
    CHECK (revision > 0),
    CHECK (last_source_block >= first_source_block)
);
CREATE INDEX IF NOT EXISTS idx_perps_market_volume_rollups_page_cover
    ON perps_market_volume_rollups(chain_id, release_router, interval_seconds, bucket_start)
    INCLUDE (volume_numerator, trade_count, finalized);

CREATE TABLE IF NOT EXISTS perps_rollup_coverage (
    kind TEXT NOT NULL,
    series_id TEXT NOT NULL DEFAULT '',
    chain_id BIGINT NOT NULL DEFAULT 0,
    release_router TEXT NOT NULL DEFAULT '',
    interval_seconds BIGINT NOT NULL,
    coverage_start BIGINT,
    coverage_end BIGINT,
    finalized_through BIGINT,
    generation BIGINT NOT NULL DEFAULT 1,
    complete BOOLEAN NOT NULL DEFAULT FALSE,
    derivation_version TEXT NOT NULL,
    last_error TEXT,
    maintenance_from BIGINT,
    maintenance_to BIGINT,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    PRIMARY KEY (kind, series_id, chain_id, release_router, interval_seconds),
    CHECK (kind IN ('price', 'volume')),
    CHECK (interval_seconds IN (60, 180, 300, 900, 1800, 3600, 86400)),
    -- Two 26-bit source generations are packed into one JS-safe API integer.
    CHECK (generation > 0 AND generation < 67108864),
    CHECK (
        (kind = 'price' AND series_id <> '' AND chain_id = 0 AND release_router = '') OR
        (kind = 'volume' AND series_id = '' AND chain_id > 0 AND release_router <> '')
    ),
    CHECK ((coverage_start IS NULL) = (coverage_end IS NULL)),
    CHECK (coverage_start IS NULL OR coverage_start >= 0),
    CHECK (coverage_end IS NULL OR coverage_end >= 0),
    CHECK (finalized_through IS NULL OR finalized_through >= 0),
    CHECK (coverage_start IS NULL OR coverage_start % interval_seconds = 0),
    CHECK (coverage_end IS NULL OR coverage_end % interval_seconds = 0),
    CHECK (finalized_through IS NULL OR finalized_through % interval_seconds = 0),
    CHECK (coverage_start IS NULL OR coverage_end > coverage_start),
    CHECK (finalized_through IS NULL OR coverage_start IS NULL OR finalized_through >= coverage_start),
    CHECK (finalized_through IS NULL OR coverage_end IS NULL OR finalized_through <= coverage_end),
    CONSTRAINT perps_rollup_coverage_maintenance_state_check CHECK (
        (
            last_error IS NOT DISTINCT FROM 'bounded_admin_repair' AND
            NOT complete AND
            maintenance_from IS NOT NULL AND
            maintenance_to IS NOT NULL AND
            maintenance_from >= 0 AND
            maintenance_to > maintenance_from AND
            maintenance_from % 60 = 0 AND
            maintenance_to % 60 = 0
        ) OR (
            last_error IS DISTINCT FROM 'bounded_admin_repair' AND
            maintenance_from IS NULL AND
            maintenance_to IS NULL
        )
    )
);

-- Existing installations need the maintenance metadata added independently of
-- CREATE TABLE IF NOT EXISTS. These statements are additive and idempotent.
ALTER TABLE perps_rollup_coverage
    ADD COLUMN IF NOT EXISTS maintenance_from BIGINT;
ALTER TABLE perps_rollup_coverage
    ADD COLUMN IF NOT EXISTS maintenance_to BIGINT;
DO $maintenance_constraint$
BEGIN
    IF NOT EXISTS (
        SELECT 1
        FROM pg_constraint
        WHERE conname = 'perps_rollup_coverage_maintenance_state_check'
          AND conrelid = 'perps_rollup_coverage'::regclass
    ) THEN
        ALTER TABLE perps_rollup_coverage
            ADD CONSTRAINT perps_rollup_coverage_maintenance_state_check CHECK (
                (
                    last_error IS NOT DISTINCT FROM 'bounded_admin_repair' AND
                    NOT complete AND
                    maintenance_from IS NOT NULL AND
                    maintenance_to IS NOT NULL AND
                    maintenance_from >= 0 AND
                    maintenance_to > maintenance_from AND
                    maintenance_from % 60 = 0 AND
                    maintenance_to % 60 = 0
                ) OR (
                    last_error IS DISTINCT FROM 'bounded_admin_repair' AND
                    maintenance_from IS NULL AND
                    maintenance_to IS NULL
                )
            );
    END IF;
END
$maintenance_constraint$;

CREATE OR REPLACE FUNCTION protect_perps_rollup_generation_monotonic()
RETURNS TRIGGER
LANGUAGE plpgsql
AS $rollup_generation$
BEGIN
    IF NEW.generation < OLD.generation THEN
        RAISE EXCEPTION 'perps rollup generation cannot decrease'
            USING ERRCODE = '23514';
    END IF;
    IF NEW.generation = OLD.generation AND (
        OLD.derivation_version IS DISTINCT FROM NEW.derivation_version OR (
            OLD.complete AND
            OLD.coverage_start IS NOT NULL AND
            OLD.coverage_end IS NOT NULL AND
            OLD.finalized_through IS NOT NULL AND (
                NOT NEW.complete OR
                NEW.coverage_start IS NULL OR
                NEW.coverage_end IS NULL OR
                NEW.finalized_through IS NULL OR
                NEW.coverage_start > OLD.coverage_start OR
                NEW.coverage_end < OLD.coverage_end OR
                NEW.finalized_through < OLD.finalized_through
            )
        )
    ) THEN
        RAISE EXCEPTION 'perps rollup usability regression requires a new generation'
            USING ERRCODE = '23514';
    END IF;
    RETURN NEW;
END
$rollup_generation$;

DO $rollup_generation_trigger$
BEGIN
    IF NOT EXISTS (
        SELECT 1
        FROM pg_trigger
        WHERE tgname = 'perps_rollup_generation_monotonic'
          AND tgrelid = 'perps_rollup_coverage'::regclass
    ) THEN
        CREATE TRIGGER perps_rollup_generation_monotonic
            BEFORE UPDATE ON perps_rollup_coverage
            FOR EACH ROW
            EXECUTE FUNCTION protect_perps_rollup_generation_monotonic();
    END IF;
END
$rollup_generation_trigger$;

-- Operator-selected candle history, durable source proof, and independently
-- published logical-market price boundaries.
-- BEGIN PERPS CANDLE HISTORY FOUNDATION
CREATE TABLE IF NOT EXISTS perps_candle_markets (
    market_id TEXT PRIMARY KEY,
    chain_id BIGINT NOT NULL,
    price_series_id TEXT NOT NULL REFERENCES perps_basket_definitions(series_id),
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    UNIQUE (market_id, chain_id),
    CHECK (chain_id > 0),
    CHECK (market_id ~ '^[a-z0-9][a-z0-9-]{0,62}$')
);

CREATE OR REPLACE FUNCTION protect_perps_candle_market_identity()
RETURNS TRIGGER
LANGUAGE plpgsql
AS $candle_market_identity$
BEGIN
    IF TG_OP = 'INSERT' THEN
        NEW.created_at := NOW();
        RETURN NEW;
    END IF;
    RAISE EXCEPTION 'candle market identity is immutable'
        USING ERRCODE = '55000';
END
$candle_market_identity$;

DO $candle_market_triggers$
BEGIN
    IF NOT EXISTS (
        SELECT 1
          FROM pg_trigger
         WHERE tgname = 'perps_candle_market_immutable'
           AND tgrelid = 'perps_candle_markets'::regclass
    ) THEN
        CREATE TRIGGER perps_candle_market_immutable
            BEFORE INSERT OR UPDATE OR DELETE ON perps_candle_markets
            FOR EACH ROW
            EXECUTE FUNCTION protect_perps_candle_market_identity();
    END IF;
END
$candle_market_triggers$;

CREATE TABLE IF NOT EXISTS perps_candle_history_targets (
    market_id TEXT NOT NULL REFERENCES perps_candle_markets(market_id),
    revision BIGINT NOT NULL,
    requested_start_timestamp BIGINT NOT NULL,
    requested_by TEXT NOT NULL,
    request_reference TEXT NOT NULL,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    PRIMARY KEY (market_id, revision),
    UNIQUE (market_id, request_reference),
    CHECK (revision > 0),
    CHECK (requested_start_timestamp >= 0),
    CHECK (requested_by ~ '[^[:space:]]'),
    CHECK (request_reference ~ '[^[:space:]]')
);

CREATE OR REPLACE FUNCTION protect_perps_candle_history_target()
RETURNS TRIGGER
LANGUAGE plpgsql
AS $candle_history_target$
DECLARE
    current_revision BIGINT;
BEGIN
    IF TG_OP <> 'INSERT' THEN
        RAISE EXCEPTION 'candle history targets are immutable; append a revision'
            USING ERRCODE = '55000';
    END IF;

    PERFORM 1
      FROM perps_candle_markets
     WHERE market_id = NEW.market_id
       FOR UPDATE;
    IF NOT FOUND THEN
        RAISE EXCEPTION 'candle history target market does not exist'
            USING ERRCODE = '23503';
    END IF;

    SELECT COALESCE(MAX(revision), 0)
      INTO current_revision
      FROM perps_candle_history_targets
     WHERE market_id = NEW.market_id;
    IF NEW.revision <> current_revision + 1 THEN
        RAISE EXCEPTION 'candle history target must append the next revision'
            USING ERRCODE = '23514';
    END IF;

    NEW.created_at := NOW();
    RETURN NEW;
END
$candle_history_target$;

DO $candle_history_target_triggers$
BEGIN
    IF NOT EXISTS (
        SELECT 1
          FROM pg_trigger
         WHERE tgname = 'perps_candle_history_target_identity'
           AND tgrelid = 'perps_candle_history_targets'::regclass
    ) THEN
        CREATE TRIGGER perps_candle_history_target_identity
            BEFORE INSERT OR UPDATE OR DELETE ON perps_candle_history_targets
            FOR EACH ROW
            EXECUTE FUNCTION protect_perps_candle_history_target();
    END IF;
END
$candle_history_target_triggers$;

CREATE TABLE IF NOT EXISTS perps_candle_history_ingestions (
    market_id TEXT NOT NULL,
    target_revision BIGINT NOT NULL,
    start_timestamp BIGINT NOT NULL,
    end_timestamp_exclusive BIGINT NOT NULL,
    next_timestamp BIGINT NOT NULL,
    sample_interval_seconds BIGINT NOT NULL,
    complete BOOLEAN NOT NULL DEFAULT FALSE,
    last_error TEXT,
    published_generation BIGINT,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    PRIMARY KEY (market_id, target_revision),
    FOREIGN KEY (market_id, target_revision)
        REFERENCES perps_candle_history_targets(market_id, revision),
    CHECK (sample_interval_seconds > 0),
    CHECK (start_timestamp >= 0 AND end_timestamp_exclusive >= start_timestamp),
    CHECK (MOD(start_timestamp, sample_interval_seconds) = 0),
    CHECK (MOD(end_timestamp_exclusive, sample_interval_seconds) = 0),
    CHECK (next_timestamp >= start_timestamp AND next_timestamp <= end_timestamp_exclusive),
    CHECK (MOD(next_timestamp, sample_interval_seconds) = 0),
    CHECK (complete = (next_timestamp = end_timestamp_exclusive)),
    CHECK (last_error IS NULL OR (NOT complete AND last_error ~ '[^[:space:]]')),
    CONSTRAINT perps_candle_history_ingestions_publication_valid CHECK (
        published_generation IS NULL OR
        (published_generation > 0 AND complete AND last_error IS NULL)
    )
);

ALTER TABLE perps_candle_history_ingestions
    ADD COLUMN IF NOT EXISTS published_generation BIGINT;

DO $candle_history_publication_constraint$
BEGIN
    IF NOT EXISTS (
        SELECT 1
        FROM pg_constraint
        WHERE conname = 'perps_candle_history_ingestions_publication_valid'
          AND conrelid = 'perps_candle_history_ingestions'::regclass
    ) THEN
        ALTER TABLE perps_candle_history_ingestions
            ADD CONSTRAINT perps_candle_history_ingestions_publication_valid
            CHECK (
                published_generation IS NULL OR
                (published_generation > 0 AND complete AND last_error IS NULL)
            );
    END IF;
END
$candle_history_publication_constraint$;

CREATE OR REPLACE FUNCTION protect_perps_candle_history_publication()
RETURNS TRIGGER
LANGUAGE plpgsql
AS $candle_history_publication$
BEGIN
    IF OLD.published_generation IS NOT NULL
       AND NEW.published_generation IS DISTINCT FROM OLD.published_generation THEN
        RAISE EXCEPTION 'candle history publication is immutable'
            USING ERRCODE = '55000';
    END IF;
    RETURN NEW;
END
$candle_history_publication$;

DO $candle_history_publication_trigger$
BEGIN
    IF NOT EXISTS (
        SELECT 1
        FROM pg_trigger
        WHERE tgname = 'perps_candle_history_publication_immutable'
          AND tgrelid = 'perps_candle_history_ingestions'::regclass
    ) THEN
        CREATE TRIGGER perps_candle_history_publication_immutable
            BEFORE UPDATE ON perps_candle_history_ingestions
            FOR EACH ROW
            EXECUTE FUNCTION protect_perps_candle_history_publication();
    END IF;
END
$candle_history_publication_trigger$;

CREATE TABLE IF NOT EXISTS perps_candle_history_ingestion_windows (
    market_id TEXT NOT NULL,
    target_revision BIGINT NOT NULL,
    window_start BIGINT NOT NULL,
    window_end_exclusive BIGINT NOT NULL,
    sample_count BIGINT NOT NULL,
    completed_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    PRIMARY KEY (market_id, target_revision, window_start),
    FOREIGN KEY (market_id, target_revision)
        REFERENCES perps_candle_history_ingestions(market_id, target_revision),
    CHECK (window_start >= 0 AND window_end_exclusive > window_start),
    CHECK (sample_count >= 0)
);

CREATE TABLE IF NOT EXISTS perps_market_release_epochs (
    market_id TEXT NOT NULL,
    release_revision BIGINT NOT NULL,
    chain_id BIGINT NOT NULL,
    release_router TEXT NOT NULL,
    cfd_engine TEXT NOT NULL,
    margin_clearinghouse TEXT NOT NULL,
    deployment_block BIGINT NOT NULL,
    deployment_block_hash TEXT NOT NULL,
    deployment_tx_hash TEXT NOT NULL,
    activation_block BIGINT NOT NULL,
    activation_timestamp BIGINT NOT NULL,
    activation_block_hash TEXT NOT NULL,
    approval_reference TEXT NOT NULL,
    is_market_genesis BOOLEAN NOT NULL DEFAULT FALSE,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    PRIMARY KEY (market_id, release_revision),
    UNIQUE (market_id, activation_block),
    UNIQUE (chain_id, release_router),
    FOREIGN KEY (market_id, chain_id)
        REFERENCES perps_candle_markets(market_id, chain_id),
    CHECK (release_router ~ '^0x[0-9a-f]{40}$'),
    CHECK (cfd_engine ~ '^0x[0-9a-f]{40}$'),
    CHECK (margin_clearinghouse ~ '^0x[0-9a-f]{40}$'),
    CHECK (release_router <> '0x0000000000000000000000000000000000000000'),
    CHECK (cfd_engine <> '0x0000000000000000000000000000000000000000'),
    CHECK (margin_clearinghouse <> '0x0000000000000000000000000000000000000000'),
    CHECK (release_revision > 0),
    CHECK (is_market_genesis = (release_revision = 1)),
    CHECK (deployment_block > 0),
    CHECK (activation_block >= deployment_block),
    CHECK (activation_timestamp >= 0),
    CHECK (deployment_block_hash ~ '^0x[0-9a-f]{64}$'),
    CHECK (deployment_tx_hash ~ '^0x[0-9a-f]{64}$'),
    CHECK (activation_block_hash ~ '^0x[0-9a-f]{64}$'),
    CHECK (deployment_block_hash <> '0x0000000000000000000000000000000000000000000000000000000000000000'),
    CHECK (deployment_tx_hash <> '0x0000000000000000000000000000000000000000000000000000000000000000'),
    CHECK (activation_block_hash <> '0x0000000000000000000000000000000000000000000000000000000000000000'),
    CHECK (approval_reference ~ '[^[:space:]]')
);

CREATE OR REPLACE FUNCTION protect_perps_market_release_identity()
RETURNS TRIGGER
LANGUAGE plpgsql
AS $market_release_identity$
DECLARE
    market_chain_id BIGINT;
    current_revision BIGINT;
    latest_block BIGINT;
    latest_timestamp BIGINT;
BEGIN
    IF TG_OP <> 'INSERT' THEN
        RAISE EXCEPTION 'market release epochs are immutable; append a successor epoch'
            USING ERRCODE = '55000';
    END IF;

    SELECT chain_id
      INTO market_chain_id
      FROM perps_candle_markets
     WHERE market_id = NEW.market_id
       FOR UPDATE;
    IF NOT FOUND OR market_chain_id <> NEW.chain_id THEN
        RAISE EXCEPTION 'market release does not match the logical market and chain'
            USING ERRCODE = '23503';
    END IF;

    SELECT release_revision, activation_block, activation_timestamp
      INTO current_revision, latest_block, latest_timestamp
      FROM perps_market_release_epochs
     WHERE market_id = NEW.market_id
     ORDER BY release_revision DESC
     LIMIT 1;
    IF NOT FOUND THEN
        current_revision := 0;
    ELSE
        IF NEW.activation_block <= latest_block
           OR NEW.activation_timestamp < latest_timestamp THEN
            RAISE EXCEPTION 'market release epochs must append in activation order'
                USING ERRCODE = '23514';
        END IF;
    END IF;

    IF NEW.release_revision <> current_revision + 1 THEN
        RAISE EXCEPTION 'market release must append the next revision'
            USING ERRCODE = '23514';
    END IF;

    NEW.created_at := NOW();
    RETURN NEW;
END
$market_release_identity$;

DO $market_release_triggers$
BEGIN
    IF NOT EXISTS (
        SELECT 1
          FROM pg_trigger
         WHERE tgname = 'perps_market_release_immutable'
           AND tgrelid = 'perps_market_release_epochs'::regclass
    ) THEN
        CREATE TRIGGER perps_market_release_immutable
            BEFORE INSERT OR UPDATE OR DELETE ON perps_market_release_epochs
            FOR EACH ROW
            EXECUTE FUNCTION protect_perps_market_release_identity();
    END IF;
END
$market_release_triggers$;
-- END PERPS CANDLE HISTORY FOUNDATION

-- Private first-party competition registration state. These tables are never
-- queried by public leaderboard/wallet endpoints; only the registration
-- service and explicit key-rotation tooling may access encrypted identity
-- material.
CREATE TABLE IF NOT EXISTS insights_registration_competition_config (
    competition_slug TEXT PRIMARY KEY REFERENCES insights_competitions(slug) ON DELETE CASCADE,
    target_x_user_id_digest BYTEA NOT NULL,
    privacy_version TEXT NOT NULL,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    CHECK (octet_length(target_x_user_id_digest) = 32),
    CHECK (privacy_version ~ '^[A-Za-z0-9_.-]{1,64}$')
);

CREATE TABLE IF NOT EXISTS insights_registration_applications (
    registration_id UUID PRIMARY KEY,
    competition_slug TEXT NOT NULL REFERENCES insights_competitions(slug) ON DELETE CASCADE,
    status TEXT NOT NULL DEFAULT 'in_progress',
    turnstile_token_digest BYTEA NOT NULL CONSTRAINT insights_registration_turnstile_digest_unique UNIQUE,
    email_key_version TEXT,
    email_nonce BYTEA,
    email_ciphertext BYTEA,
    email_tag BYTEA,
    email_digest BYTEA,
    email_masked TEXT,
    x_user_id_key_version TEXT,
    x_user_id_nonce BYTEA,
    x_user_id_ciphertext BYTEA,
    x_user_id_tag BYTEA,
    x_user_id_digest BYTEA,
    x_username TEXT,
    x_created_timestamp BIGINT,
    x_identity_verified_at TIMESTAMPTZ,
    x_access_key_version TEXT,
    x_access_nonce BYTEA,
    x_access_ciphertext BYTEA,
    x_access_tag BYTEA,
    x_follow_attempt_id UUID,
    x_follow_attempt_started_at TIMESTAMPTZ,
    x_follow_verified_at TIMESTAMPTZ,
    owner_wallet VARCHAR(42),
    trading_account VARCHAR(42),
    wallet_verification_block BIGINT,
    wallet_verification_block_hash TEXT,
    wallet_verified_at TIMESTAMPTZ,
    rules_version TEXT,
    privacy_version TEXT,
    promotional_email_consent BOOLEAN NOT NULL DEFAULT FALSE,
    promotional_email_consent_at TIMESTAMPTZ,
    completed_at TIMESTAMPTZ,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    CHECK (status IN ('in_progress', 'completed')),
    CHECK (
        num_nonnulls(email_key_version, email_nonce, email_ciphertext, email_tag) IN (0, 4)
        AND (email_nonce IS NULL OR octet_length(email_nonce) = 12)
        AND (email_tag IS NULL OR octet_length(email_tag) = 16)
    ),
    CHECK (
        num_nonnulls(x_user_id_key_version, x_user_id_nonce, x_user_id_ciphertext, x_user_id_tag) IN (0, 4)
        AND (x_user_id_nonce IS NULL OR octet_length(x_user_id_nonce) = 12)
        AND (x_user_id_tag IS NULL OR octet_length(x_user_id_tag) = 16)
    ),
    CHECK (
        num_nonnulls(x_access_key_version, x_access_nonce, x_access_ciphertext, x_access_tag) IN (0, 4)
        AND (x_access_nonce IS NULL OR octet_length(x_access_nonce) = 12)
        AND (x_access_tag IS NULL OR octet_length(x_access_tag) = 16)
    ),
    CHECK (num_nonnulls(x_follow_attempt_id, x_follow_attempt_started_at) IN (0, 2)),
    CHECK (
        (owner_wallet IS NULL OR owner_wallet ~ '^0x[0-9a-f]{40}$')
        AND (trading_account IS NULL OR trading_account ~ '^0x[0-9a-f]{40}$')
    ),
    CHECK (
        num_nonnulls(wallet_verification_block, wallet_verification_block_hash) IN (0, 2)
        AND (wallet_verification_block IS NULL OR wallet_verification_block >= 0)
        AND (wallet_verification_block_hash IS NULL OR wallet_verification_block_hash ~ '^0x[0-9a-f]{64}$')
    ),
    CHECK (
        octet_length(turnstile_token_digest) = 32
        AND (email_digest IS NULL OR octet_length(email_digest) = 32)
        AND (x_user_id_digest IS NULL OR octet_length(x_user_id_digest) = 32)
    ),
    CONSTRAINT insights_registration_applications_promotional_email_consent_check CHECK (
        promotional_email_consent = (promotional_email_consent_at IS NOT NULL)
    ),
    CHECK (
        status <> 'completed'
        OR (
            completed_at IS NOT NULL
            AND rules_version IS NOT NULL
            AND privacy_version IS NOT NULL
            AND email_digest IS NOT NULL
            AND email_masked IS NOT NULL
            AND num_nonnulls(email_key_version, email_nonce, email_ciphertext, email_tag) = 4
            AND x_user_id_digest IS NOT NULL
            AND x_username IS NOT NULL
            AND x_created_timestamp IS NOT NULL
            AND x_identity_verified_at IS NOT NULL
            AND x_follow_verified_at IS NOT NULL
            AND x_follow_attempt_id IS NULL
            AND x_follow_attempt_started_at IS NULL
            AND owner_wallet IS NOT NULL
            AND trading_account IS NOT NULL
            AND wallet_verification_block IS NOT NULL
            AND wallet_verification_block_hash IS NOT NULL
            AND wallet_verified_at IS NOT NULL
            AND num_nonnulls(x_user_id_key_version, x_user_id_nonce, x_user_id_ciphertext, x_user_id_tag) = 0
            AND num_nonnulls(x_access_key_version, x_access_nonce, x_access_ciphertext, x_access_tag) = 0
        )
    )
);

CREATE UNIQUE INDEX IF NOT EXISTS idx_insights_registration_email_unique
    ON insights_registration_applications(competition_slug, email_digest)
    WHERE status = 'completed';
CREATE UNIQUE INDEX IF NOT EXISTS idx_insights_registration_x_unique
    ON insights_registration_applications(competition_slug, x_user_id_digest)
    WHERE status = 'completed';
CREATE UNIQUE INDEX IF NOT EXISTS idx_insights_registration_owner_unique
    ON insights_registration_applications(competition_slug, owner_wallet)
    WHERE status = 'completed';
CREATE UNIQUE INDEX IF NOT EXISTS idx_insights_registration_account_unique
    ON insights_registration_applications(competition_slug, trading_account)
    WHERE status = 'completed';
CREATE INDEX IF NOT EXISTS idx_insights_registration_applications_status_created
    ON insights_registration_applications(status, created_at);
CREATE INDEX IF NOT EXISTS idx_insights_registration_follow_attempt_lease
    ON insights_registration_applications(x_follow_attempt_started_at)
    WHERE x_follow_attempt_started_at IS NOT NULL;

CREATE TABLE IF NOT EXISTS insights_registration_sessions (
    session_digest BYTEA PRIMARY KEY,
    application_id UUID NOT NULL UNIQUE REFERENCES insights_registration_applications(registration_id) ON DELETE CASCADE,
    csrf_digest BYTEA NOT NULL,
    csrf_key_version TEXT NOT NULL,
    csrf_nonce BYTEA NOT NULL,
    csrf_ciphertext BYTEA NOT NULL,
    csrf_tag BYTEA NOT NULL,
    expires_at TIMESTAMPTZ NOT NULL,
    oauth_error_code TEXT,
    oauth_state_digest BYTEA,
    oauth_expires_at TIMESTAMPTZ,
    pkce_key_version TEXT,
    pkce_nonce BYTEA,
    pkce_ciphertext BYTEA,
    pkce_tag BYTEA,
    wallet_nonce_digest BYTEA,
    wallet_owner VARCHAR(42),
    wallet_expires_at TIMESTAMPTZ,
    wallet_message_key_version TEXT,
    wallet_message_nonce BYTEA,
    wallet_message_ciphertext BYTEA,
    wallet_message_tag BYTEA,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    CHECK (
        octet_length(session_digest) = 32
        AND octet_length(csrf_digest) = 32
        AND octet_length(csrf_nonce) = 12
        AND octet_length(csrf_tag) = 16
    ),
    CHECK (oauth_error_code IS NULL OR oauth_error_code ~ '^[A-Z_]{1,32}$'),
    CHECK (
        num_nonnulls(oauth_state_digest, oauth_expires_at, pkce_key_version, pkce_nonce, pkce_ciphertext, pkce_tag) IN (0, 6)
        AND (oauth_state_digest IS NULL OR octet_length(oauth_state_digest) = 32)
        AND (pkce_nonce IS NULL OR octet_length(pkce_nonce) = 12)
        AND (pkce_tag IS NULL OR octet_length(pkce_tag) = 16)
    ),
    CHECK (
        num_nonnulls(wallet_nonce_digest, wallet_owner, wallet_expires_at, wallet_message_key_version, wallet_message_nonce, wallet_message_ciphertext, wallet_message_tag) IN (0, 7)
        AND (wallet_owner IS NULL OR wallet_owner ~ '^0x[0-9a-f]{40}$')
        AND (wallet_nonce_digest IS NULL OR octet_length(wallet_nonce_digest) = 32)
        AND (wallet_message_nonce IS NULL OR octet_length(wallet_message_nonce) = 12)
        AND (wallet_message_tag IS NULL OR octet_length(wallet_message_tag) = 16)
    )
);

CREATE INDEX IF NOT EXISTS idx_insights_registration_sessions_expires
    ON insights_registration_sessions(expires_at);
CREATE INDEX IF NOT EXISTS idx_insights_registration_sessions_oauth_expires
    ON insights_registration_sessions(oauth_expires_at)
    WHERE oauth_expires_at IS NOT NULL;
CREATE INDEX IF NOT EXISTS idx_insights_registration_sessions_wallet_expires
    ON insights_registration_sessions(wallet_expires_at)
    WHERE wallet_expires_at IS NOT NULL;

CREATE TABLE IF NOT EXISTS insights_registration_rate_limits (
    scope_digest BYTEA NOT NULL,
    window_epoch_minute BIGINT NOT NULL,
    request_count INTEGER NOT NULL,
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    PRIMARY KEY(scope_digest, window_epoch_minute),
    CHECK (octet_length(scope_digest) = 32 AND window_epoch_minute >= 0 AND request_count > 0)
);

CREATE INDEX IF NOT EXISTS idx_insights_registration_rate_limits_window
    ON insights_registration_rate_limits(window_epoch_minute);
