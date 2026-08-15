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
    status VARCHAR(16) NOT NULL,
    error TEXT,
    created_at TIMESTAMP DEFAULT NOW(),
    updated_at TIMESTAMP DEFAULT NOW(),
    PRIMARY KEY (address, token_address)
);
CREATE INDEX IF NOT EXISTS idx_testnet_faucet_claims_status ON testnet_faucet_claims(status);

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
-- config; later starts validate it rather than rewriting historical rules.
CREATE TABLE IF NOT EXISTS insights_competitions (
    slug TEXT PRIMARY KEY,
    name TEXT NOT NULL,
    chain_id BIGINT NOT NULL,
    release_router TEXT NOT NULL,
    usdc_address TEXT NOT NULL,
    margin_clearinghouse_address TEXT NOT NULL,
    account_lens_address TEXT NOT NULL,
    start_timestamp BIGINT NOT NULL,
    new_risk_cutoff_timestamp BIGINT NOT NULL,
    score_cutoff_timestamp BIGINT NOT NULL,
    results_timestamp BIGINT NOT NULL,
    payment_deadline_timestamp BIGINT NOT NULL,
    start_block BIGINT,
    start_block_hash TEXT,
    score_cutoff_block BIGINT,
    score_cutoff_block_hash TEXT,
    starting_balance_usdc NUMERIC(78,0) NOT NULL,
    minimum_profit_bps BIGINT NOT NULL,
    minimum_active_days INTEGER NOT NULL,
    scoring_version TEXT NOT NULL,
    rules_version TEXT NOT NULL,
    first_prize_usdc NUMERIC(78,0) NOT NULL,
    second_prize_usdc NUMERIC(78,0) NOT NULL,
    third_prize_usdc NUMERIC(78,0) NOT NULL,
    finalized BOOLEAN NOT NULL DEFAULT FALSE,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    CHECK (new_risk_cutoff_timestamp >= start_timestamp),
    CHECK (score_cutoff_timestamp >= new_risk_cutoff_timestamp),
    CHECK (minimum_profit_bps >= 0),
    CHECK (minimum_active_days >= 0)
);

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

-- Operator-selected candle history and logical Perps market identity. These
-- tables are deliberately inert: no public read or writer uses them until a
-- separately gated logical-market rollout is implemented.
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
