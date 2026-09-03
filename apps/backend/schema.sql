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

-- Legacy LP settlement observation/status table. It remains available during
-- the keeper rollout, but new signed intents and broadcasts are never written
-- here because it cannot represent their complete transaction identity.
CREATE TABLE IF NOT EXISTS perps_lp_settlement_attempts (
    chain_id BIGINT NOT NULL,
    monitor_address TEXT NOT NULL,
    observation_digest VARCHAR(66) NOT NULL,
    epoch BIGINT NOT NULL,
    observed_block BIGINT NOT NULL,
    execution_path INTEGER NOT NULL,
    operational_blocker_mask TEXT NOT NULL,
    warning_mask TEXT NOT NULL,
    dependency_failure_mask TEXT NOT NULL,
    critical_fault_mask TEXT NOT NULL,
    transaction_hash VARCHAR(66),
    status VARCHAR(24) NOT NULL,
    last_error TEXT,
    created_at TIMESTAMP DEFAULT NOW(),
    updated_at TIMESTAMP DEFAULT NOW(),
    PRIMARY KEY (chain_id, monitor_address, observation_digest)
);
CREATE INDEX IF NOT EXISTS idx_perps_lp_settlement_submitted
    ON perps_lp_settlement_attempts(chain_id, monitor_address, updated_at)
    WHERE status = 'submitted';

-- Immutable settlement monitor observations. Detail fields are nullable only
-- for legacy rows, which did not persist the full monitor response.
CREATE TABLE IF NOT EXISTS perps_lp_settlement_observations (
    chain_id BIGINT NOT NULL CHECK (chain_id > 0),
    monitor_address VARCHAR(42) NOT NULL CHECK (monitor_address ~ '^0x[0-9a-f]{40}$'),
    observation_digest VARCHAR(66) NOT NULL CHECK (observation_digest ~ '^0x[0-9a-f]{64}$'),
    epoch BIGINT NOT NULL CHECK (epoch >= 0),
    observed_block BIGINT NOT NULL CHECK (observed_block >= 0),
    observed_block_hash VARCHAR(66) CHECK (observed_block_hash IS NULL OR observed_block_hash ~ '^0x[0-9a-f]{64}$'),
    execution_path INTEGER NOT NULL CHECK (execution_path >= 0),
    operational_blocker_mask NUMERIC(78,0) NOT NULL CHECK (operational_blocker_mask >= 0),
    warning_mask NUMERIC(78,0) NOT NULL CHECK (warning_mask >= 0),
    dependency_failure_mask NUMERIC(78,0) NOT NULL CHECK (dependency_failure_mask >= 0),
    critical_fault_mask NUMERIC(78,0) NOT NULL CHECK (critical_fault_mask >= 0),
    schema_version NUMERIC(78,0) CHECK (schema_version IS NULL OR schema_version >= 0),
    health_state NUMERIC(78,0) CHECK (health_state IS NULL OR health_state >= 0),
    execution_path_dependency_mask NUMERIC(78,0) CHECK (execution_path_dependency_mask IS NULL OR execution_path_dependency_mask >= 0),
    status_dependency_failure_mask NUMERIC(78,0) CHECK (status_dependency_failure_mask IS NULL OR status_dependency_failure_mask >= 0),
    health_dependency_failure_mask NUMERIC(78,0) CHECK (health_dependency_failure_mask IS NULL OR health_dependency_failure_mask >= 0),
    observation_complete BOOLEAN,
    has_matured_work BOOLEAN,
    lp_epoch_settlement_paused BOOLEAN,
    first_observed_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    last_observed_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    PRIMARY KEY (chain_id, monitor_address, observation_digest),
    UNIQUE (chain_id, monitor_address, observation_digest, epoch)
);

-- Migrate only canonical legacy observations. In particular, do not turn a
-- legacy transaction hash into a new attempt: the old table has no signer,
-- nonce, target, calldata, value, fee envelope, or signed raw transaction.
WITH canonical_legacy AS MATERIALIZED (
    SELECT
        chain_id,
        lower(trim(monitor_address)) AS monitor_address,
        lower(trim(observation_digest)) AS observation_digest,
        epoch,
        observed_block,
        execution_path,
        CASE
            WHEN trim(operational_blocker_mask) ~ '^[0-9]+$'
             AND length(trim(operational_blocker_mask)) <= 78
            THEN trim(operational_blocker_mask)::NUMERIC
        END AS operational_blocker_mask,
        CASE
            WHEN trim(warning_mask) ~ '^[0-9]+$'
             AND length(trim(warning_mask)) <= 78
            THEN trim(warning_mask)::NUMERIC
        END AS warning_mask,
        CASE
            WHEN trim(dependency_failure_mask) ~ '^[0-9]+$'
             AND length(trim(dependency_failure_mask)) <= 78
            THEN trim(dependency_failure_mask)::NUMERIC
        END AS dependency_failure_mask,
        CASE
            WHEN trim(critical_fault_mask) ~ '^[0-9]+$'
             AND length(trim(critical_fault_mask)) <= 78
            THEN trim(critical_fault_mask)::NUMERIC
        END AS critical_fault_mask,
        created_at AT TIME ZONE 'UTC' AS first_observed_at,
        updated_at AT TIME ZONE 'UTC' AS last_observed_at
    FROM perps_lp_settlement_attempts
    WHERE chain_id > 0
      AND epoch >= 0
      AND observed_block >= 0
      AND execution_path >= 0
      AND lower(trim(monitor_address)) ~ '^0x[0-9a-f]{40}$'
      AND lower(trim(observation_digest)) ~ '^0x[0-9a-f]{64}$'
      AND trim(operational_blocker_mask) ~ '^[0-9]+$'
      AND length(trim(operational_blocker_mask)) <= 78
      AND trim(warning_mask) ~ '^[0-9]+$'
      AND length(trim(warning_mask)) <= 78
      AND trim(dependency_failure_mask) ~ '^[0-9]+$'
      AND length(trim(dependency_failure_mask)) <= 78
      AND trim(critical_fault_mask) ~ '^[0-9]+$'
      AND length(trim(critical_fault_mask)) <= 78
), unambiguous AS (
    SELECT
        chain_id,
        monitor_address,
        observation_digest,
        MIN(epoch) AS epoch,
        MIN(observed_block) AS observed_block,
        MIN(execution_path) AS execution_path,
        MIN(operational_blocker_mask) AS operational_blocker_mask,
        MIN(warning_mask) AS warning_mask,
        MIN(dependency_failure_mask) AS dependency_failure_mask,
        MIN(critical_fault_mask) AS critical_fault_mask,
        COALESCE(MIN(first_observed_at), NOW()) AS first_observed_at,
        COALESCE(MAX(last_observed_at), NOW()) AS last_observed_at
    FROM canonical_legacy
    GROUP BY chain_id, monitor_address, observation_digest
    HAVING COUNT(DISTINCT (
        epoch,
        observed_block,
        execution_path,
        operational_blocker_mask,
        warning_mask,
        dependency_failure_mask,
        critical_fault_mask
    )) = 1
)
INSERT INTO perps_lp_settlement_observations (
    chain_id,
    monitor_address,
    observation_digest,
    epoch,
    observed_block,
    execution_path,
    operational_blocker_mask,
    warning_mask,
    dependency_failure_mask,
    critical_fault_mask,
    first_observed_at,
    last_observed_at
)
SELECT
    chain_id,
    monitor_address,
    observation_digest,
    epoch,
    observed_block,
    execution_path,
    operational_blocker_mask,
    warning_mask,
    dependency_failure_mask,
    critical_fault_mask,
    first_observed_at,
    last_observed_at
FROM unambiguous
ON CONFLICT (chain_id, monitor_address, observation_digest) DO NOTHING;

-- An observation digest is immutable. The only allowed update is filling
-- detail fields that were absent from a migrated legacy row and advancing its
-- last-seen timestamp.
CREATE OR REPLACE FUNCTION protect_lp_settlement_observation_identity()
RETURNS trigger AS $$
BEGIN
    IF TG_OP = 'DELETE' THEN
        RAISE EXCEPTION 'perps_lp_settlement_observations is append-only';
    END IF;
    IF ROW(
        OLD.chain_id,
        OLD.monitor_address,
        OLD.observation_digest,
        OLD.epoch,
        OLD.observed_block,
        OLD.execution_path,
        OLD.operational_blocker_mask,
        OLD.warning_mask,
        OLD.dependency_failure_mask,
        OLD.critical_fault_mask,
        OLD.first_observed_at
    ) IS DISTINCT FROM ROW(
        NEW.chain_id,
        NEW.monitor_address,
        NEW.observation_digest,
        NEW.epoch,
        NEW.observed_block,
        NEW.execution_path,
        NEW.operational_blocker_mask,
        NEW.warning_mask,
        NEW.dependency_failure_mask,
        NEW.critical_fault_mask,
        NEW.first_observed_at
    ) THEN
        RAISE EXCEPTION 'LP settlement observation identity is immutable';
    END IF;
    IF (OLD.observed_block_hash IS NOT NULL AND OLD.observed_block_hash IS DISTINCT FROM NEW.observed_block_hash)
       OR (OLD.schema_version IS NOT NULL AND OLD.schema_version IS DISTINCT FROM NEW.schema_version)
       OR (OLD.health_state IS NOT NULL AND OLD.health_state IS DISTINCT FROM NEW.health_state)
       OR (OLD.execution_path_dependency_mask IS NOT NULL AND OLD.execution_path_dependency_mask IS DISTINCT FROM NEW.execution_path_dependency_mask)
       OR (OLD.status_dependency_failure_mask IS NOT NULL AND OLD.status_dependency_failure_mask IS DISTINCT FROM NEW.status_dependency_failure_mask)
       OR (OLD.health_dependency_failure_mask IS NOT NULL AND OLD.health_dependency_failure_mask IS DISTINCT FROM NEW.health_dependency_failure_mask)
       OR (OLD.observation_complete IS NOT NULL AND OLD.observation_complete IS DISTINCT FROM NEW.observation_complete)
       OR (OLD.has_matured_work IS NOT NULL AND OLD.has_matured_work IS DISTINCT FROM NEW.has_matured_work)
       OR (OLD.lp_epoch_settlement_paused IS NOT NULL AND OLD.lp_epoch_settlement_paused IS DISTINCT FROM NEW.lp_epoch_settlement_paused)
    THEN
        RAISE EXCEPTION 'LP settlement observation detail is immutable once known';
    END IF;
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

DO $$
BEGIN
    IF NOT EXISTS (
        SELECT 1
        FROM pg_trigger
        WHERE tgname = 'trg_perps_lp_settlement_observation_identity'
          AND tgrelid = 'perps_lp_settlement_observations'::regclass
    ) THEN
        BEGIN
            CREATE TRIGGER trg_perps_lp_settlement_observation_identity
                BEFORE UPDATE OR DELETE ON perps_lp_settlement_observations
                FOR EACH ROW EXECUTE FUNCTION protect_lp_settlement_observation_identity();
        EXCEPTION WHEN duplicate_object THEN
            NULL;
        END;
    END IF;
END $$;

-- Every signed transaction (including a fee replacement) has its own durable
-- row. Semantic fields are copied from the predecessor by the atomic
-- replacement API; only fees, raw bytes, and the signed hash may change.
CREATE TABLE IF NOT EXISTS perps_lp_settlement_transactions (
    id BIGSERIAL PRIMARY KEY,
    chain_id BIGINT NOT NULL,
    monitor_address VARCHAR(42) NOT NULL,
    observation_digest VARCHAR(66) NOT NULL,
    epoch BIGINT NOT NULL CHECK (epoch >= 0),
    replacement_count INTEGER NOT NULL DEFAULT 0 CHECK (replacement_count >= 0),
    replaces_attempt_id BIGINT REFERENCES perps_lp_settlement_transactions(id) ON DELETE RESTRICT,
    signer_address VARCHAR(42) NOT NULL CHECK (signer_address ~ '^0x[0-9a-f]{40}$'),
    tx_nonce NUMERIC(78,0) NOT NULL CHECK (tx_nonce >= 0),
    target_address VARCHAR(42) NOT NULL CHECK (target_address ~ '^0x[0-9a-f]{40}$'),
    tx_value NUMERIC(78,0) NOT NULL CHECK (tx_value >= 0),
    calldata BYTEA NOT NULL CHECK (octet_length(calldata) >= 4),
    gas_limit NUMERIC(78,0) NOT NULL CHECK (gas_limit > 0),
    max_priority_fee_per_gas NUMERIC(78,0) NOT NULL CHECK (max_priority_fee_per_gas >= 0),
    max_fee_per_gas NUMERIC(78,0) NOT NULL CHECK (max_fee_per_gas >= max_priority_fee_per_gas),
    signed_raw_transaction BYTEA NOT NULL CHECK (octet_length(signed_raw_transaction) > 0),
    signed_transaction_hash VARCHAR(66) NOT NULL UNIQUE CHECK (signed_transaction_hash ~ '^0x[0-9a-f]{64}$'),
    status VARCHAR(32) NOT NULL CHECK (status IN (
        'prepared',
        'broadcast',
        'pending',
        'confirming',
        'manual_review',
        'replaced',
        'confirmed_success',
        'confirmed_revert',
        'failed',
        'abandoned',
        'superseded'
    )),
    last_error TEXT,
    receipt_transaction_hash VARCHAR(66) CHECK (receipt_transaction_hash IS NULL OR receipt_transaction_hash ~ '^0x[0-9a-f]{64}$'),
    receipt_block_number BIGINT CHECK (receipt_block_number IS NULL OR receipt_block_number >= 0),
    receipt_block_hash VARCHAR(66) CHECK (receipt_block_hash IS NULL OR receipt_block_hash ~ '^0x[0-9a-f]{64}$'),
    receipt_succeeded BOOLEAN,
    confirmed_at TIMESTAMPTZ,
    confirmation_depth INTEGER CHECK (confirmation_depth IS NULL OR confirmation_depth >= 0),
    settlement_event_log_index BIGINT CHECK (settlement_event_log_index IS NULL OR settlement_event_log_index >= 0),
    cutoff_epoch NUMERIC(78,0) CHECK (cutoff_epoch IS NULL OR cutoff_epoch >= 0),
    senior_redeem_assets NUMERIC(78,0) CHECK (senior_redeem_assets IS NULL OR senior_redeem_assets >= 0),
    junior_redeem_assets NUMERIC(78,0) CHECK (junior_redeem_assets IS NULL OR junior_redeem_assets >= 0),
    junior_deposit_assets NUMERIC(78,0) CHECK (junior_deposit_assets IS NULL OR junior_deposit_assets >= 0),
    senior_deposit_assets NUMERIC(78,0) CHECK (senior_deposit_assets IS NULL OR senior_deposit_assets >= 0),
    senior_backlog BOOLEAN,
    junior_backlog BOOLEAN,
    entries_deferred BOOLEAN,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    FOREIGN KEY (chain_id, monitor_address, observation_digest, epoch)
        REFERENCES perps_lp_settlement_observations(chain_id, monitor_address, observation_digest, epoch)
        ON DELETE RESTRICT,
    UNIQUE (replaces_attempt_id),
    UNIQUE (chain_id, signer_address, tx_nonce, replacement_count),
    CHECK (
        (replacement_count = 0 AND replaces_attempt_id IS NULL)
        OR (replacement_count > 0 AND replaces_attempt_id IS NOT NULL)
    ),
    CHECK (
        (
            receipt_transaction_hash IS NULL
            AND receipt_block_number IS NULL
            AND receipt_block_hash IS NULL
            AND receipt_succeeded IS NULL
        )
        OR (
            receipt_transaction_hash IS NOT NULL
            AND receipt_block_number IS NOT NULL
            AND receipt_block_hash IS NOT NULL
            AND receipt_succeeded IS NOT NULL
        )
    ),
    CHECK (
        (
            settlement_event_log_index IS NULL
            AND cutoff_epoch IS NULL
            AND senior_redeem_assets IS NULL
            AND junior_redeem_assets IS NULL
            AND junior_deposit_assets IS NULL
            AND senior_deposit_assets IS NULL
            AND senior_backlog IS NULL
            AND junior_backlog IS NULL
            AND entries_deferred IS NULL
        )
        OR (
            settlement_event_log_index IS NOT NULL
            AND cutoff_epoch IS NOT NULL
            AND senior_redeem_assets IS NOT NULL
            AND junior_redeem_assets IS NOT NULL
            AND junior_deposit_assets IS NOT NULL
            AND senior_deposit_assets IS NOT NULL
            AND senior_backlog IS NOT NULL
            AND junior_backlog IS NOT NULL
            AND entries_deferred IS NOT NULL
        )
    ),
    CONSTRAINT perps_lp_settlement_confirmation_state_check CHECK (
        (
            status IN ('confirmed_success', 'confirmed_revert')
            AND confirmed_at IS NOT NULL
            AND confirmation_depth IS NOT NULL
        )
        OR (
            status = 'superseded'
            AND (
                confirmed_at IS NULL
                OR (
                    receipt_transaction_hash IS NOT NULL
                    AND confirmation_depth IS NOT NULL
                )
            )
        )
        OR (
            status NOT IN ('confirmed_success', 'confirmed_revert', 'superseded')
            AND confirmed_at IS NULL
        )
    ),
    CHECK (
        status <> 'confirmed_success'
        OR (receipt_succeeded IS TRUE AND settlement_event_log_index IS NOT NULL)
    ),
    CHECK (
        status <> 'confirmed_revert'
        OR (receipt_succeeded IS FALSE AND settlement_event_log_index IS NULL)
    ),
    CONSTRAINT perps_lp_settlement_terminal_receipt_identity_check CHECK (
        (
            status NOT IN ('confirmed_success', 'confirmed_revert')
            AND (status <> 'superseded' OR confirmed_at IS NULL)
        )
        OR receipt_transaction_hash = signed_transaction_hash
    ),
    CONSTRAINT perps_lp_settlement_superseded_receipt_check CHECK (
        status <> 'superseded'
        OR confirmed_at IS NULL
        OR (
            receipt_transaction_hash IS NOT NULL
            AND
            receipt_succeeded IS FALSE
            AND settlement_event_log_index IS NULL
            AND confirmation_depth IS NOT NULL
        )
    ),
    CONSTRAINT perps_lp_settlement_success_epoch_check CHECK (
        status <> 'confirmed_success'
        OR cutoff_epoch = epoch
    ),
    CHECK (
        status <> 'confirming'
        OR (
            receipt_transaction_hash IS NOT NULL
            AND confirmed_at IS NULL
            AND settlement_event_log_index IS NULL
        )
    ),
    CHECK (
        status NOT IN ('prepared', 'broadcast', 'pending')
        OR (receipt_transaction_hash IS NULL AND settlement_event_log_index IS NULL)
    )
);

-- Additive migration for databases initialized by a pre-release keeper build.
DO $$
DECLARE
    old_constraint record;
BEGIN
    FOR old_constraint IN
        SELECT k.conname
        FROM pg_constraint k
        WHERE k.conrelid = 'perps_lp_settlement_transactions'::regclass
          AND k.contype = 'c'
          AND k.conkey = ARRAY[(
              SELECT a.attnum
              FROM pg_attribute a
              WHERE a.attrelid = k.conrelid
                AND a.attname = 'status'
          )]
          AND encode(sha256(convert_to(regexp_replace(lower(trim(pg_get_constraintdef(k.oid, true))), E'\\s+', ' ', 'g'), 'UTF8')), 'hex') <>
              '2bbd439a6b83336279526a7f336eb14aab3934929267a056211c35760228cbfa'
    LOOP
        EXECUTE format(
            'ALTER TABLE perps_lp_settlement_transactions DROP CONSTRAINT %I',
            old_constraint.conname
        );
    END LOOP;
    IF NOT EXISTS (
        SELECT 1
        FROM pg_constraint k
        WHERE k.conrelid = 'perps_lp_settlement_transactions'::regclass
          AND k.contype = 'c'
          AND k.convalidated
          AND encode(sha256(convert_to(regexp_replace(lower(trim(pg_get_constraintdef(k.oid, true))), E'\\s+', ' ', 'g'), 'UTF8')), 'hex') =
              '2bbd439a6b83336279526a7f336eb14aab3934929267a056211c35760228cbfa'
    ) THEN
        ALTER TABLE perps_lp_settlement_transactions
            ADD CONSTRAINT perps_lp_settlement_status_domain_check CHECK (
                status IN (
                    'prepared', 'broadcast', 'pending', 'confirming',
                    'manual_review', 'replaced', 'confirmed_success',
                    'confirmed_revert', 'failed', 'abandoned', 'superseded'
                )
            );
    END IF;
END
$$;

DO $$
DECLARE
    legacy_object record;
BEGIN
    FOR legacy_object IN
        SELECT k.conname
        FROM pg_constraint k
        WHERE k.conrelid = 'perps_lp_settlement_transactions'::regclass
          AND k.contype = 'u'
          AND k.conkey = ARRAY[
              (SELECT a.attnum FROM pg_attribute a WHERE a.attrelid = k.conrelid AND a.attname = 'chain_id'),
              (SELECT a.attnum FROM pg_attribute a WHERE a.attrelid = k.conrelid AND a.attname = 'signer_address'),
              (SELECT a.attnum FROM pg_attribute a WHERE a.attrelid = k.conrelid AND a.attname = 'tx_nonce')
          ]
    LOOP
        EXECUTE format(
            'ALTER TABLE perps_lp_settlement_transactions DROP CONSTRAINT %I',
            legacy_object.conname
        );
    END LOOP;
    FOR legacy_object IN
        SELECT idx.relname AS index_name
        FROM pg_index i
        JOIN pg_class idx ON idx.oid = i.indexrelid
        JOIN pg_class tbl ON tbl.oid = i.indrelid
        JOIN pg_namespace n ON n.oid = tbl.relnamespace
        LEFT JOIN pg_constraint k ON k.conindid = i.indexrelid
        WHERE n.nspname = current_schema()
          AND tbl.relname = 'perps_lp_settlement_transactions'
          AND k.oid IS NULL
          AND i.indisunique
          AND i.indpred IS NULL
          AND i.indnatts = 3 AND i.indnkeyatts = 3
          AND pg_get_indexdef(i.indexrelid, 1, true) = 'chain_id'
          AND pg_get_indexdef(i.indexrelid, 2, true) = 'signer_address'
          AND pg_get_indexdef(i.indexrelid, 3, true) = 'tx_nonce'
    LOOP
        EXECUTE format('DROP INDEX %I', legacy_object.index_name);
    END LOOP;
    IF NOT EXISTS (
        SELECT 1
        FROM pg_constraint k
        WHERE k.conrelid = 'perps_lp_settlement_transactions'::regclass
          AND k.contype = 'u'
          AND k.convalidated
          AND encode(sha256(convert_to(regexp_replace(lower(trim(pg_get_constraintdef(k.oid, true))), E'\\s+', ' ', 'g'), 'UTF8')), 'hex') =
              '4515d727d3995c3c2022d5ae8f7e8259e765718cd09d23db00ad64cc85a04b6f'
    ) THEN
        ALTER TABLE perps_lp_settlement_transactions
            DROP CONSTRAINT IF EXISTS perps_lp_settlement_nonce_replacement_unique;
        ALTER TABLE perps_lp_settlement_transactions
            ADD CONSTRAINT perps_lp_settlement_nonce_replacement_unique
            UNIQUE (chain_id, signer_address, tx_nonce, replacement_count);
    END IF;
END
$$;

DO $$
BEGIN
    IF NOT EXISTS (
        SELECT 1
        FROM pg_constraint k
        WHERE k.conrelid = 'perps_lp_settlement_transactions'::regclass
          AND k.contype = 'u'
          AND k.convalidated
          AND encode(sha256(convert_to(regexp_replace(lower(trim(pg_get_constraintdef(k.oid, true))), E'\\s+', ' ', 'g'), 'UTF8')), 'hex') =
              '74a720f18e3e0f47c41f7ab05c2b192f666d94ea418ad50b135aaa77570ec055'
    ) THEN
        ALTER TABLE perps_lp_settlement_transactions
            DROP CONSTRAINT IF EXISTS perps_lp_settlement_replaces_attempt_unique;
        ALTER TABLE perps_lp_settlement_transactions
            ADD CONSTRAINT perps_lp_settlement_replaces_attempt_unique
            UNIQUE (replaces_attempt_id);
    END IF;
END
$$;

DO $$
BEGIN
    IF NOT EXISTS (
        SELECT 1
        FROM pg_constraint k
        WHERE k.conrelid = 'perps_lp_settlement_transactions'::regclass
          AND k.conname = 'perps_lp_settlement_success_epoch_check'
          AND k.contype = 'c'
          AND k.convalidated
          AND encode(sha256(convert_to(regexp_replace(lower(trim(pg_get_constraintdef(k.oid, true))), E'\\s+', ' ', 'g'), 'UTF8')), 'hex') =
              '17853a3381c45cdd3a9bb4d0d4afc7722437c8aa09eb0e44ef9cb92eb26be9fa'
    ) THEN
        ALTER TABLE perps_lp_settlement_transactions
            DROP CONSTRAINT IF EXISTS perps_lp_settlement_success_epoch_check;
        ALTER TABLE perps_lp_settlement_transactions
            ADD CONSTRAINT perps_lp_settlement_success_epoch_check CHECK (
                status <> 'confirmed_success' OR cutoff_epoch = epoch
            );
    END IF;
END
$$;

DO $$
DECLARE
    old_constraint record;
BEGIN
    FOR old_constraint IN
        SELECT conname
        FROM pg_constraint
        WHERE conrelid = 'perps_lp_settlement_transactions'::regclass
          AND contype = 'c'
          AND conname <> 'perps_lp_settlement_confirmation_state_check'
          AND pg_get_constraintdef(oid) ILIKE '%confirmed_at%'
          AND pg_get_constraintdef(oid) ILIKE '%confirmation_depth%'
          AND pg_get_constraintdef(oid) ILIKE '%confirmed_success%'
          AND pg_get_constraintdef(oid) ILIKE '%confirmed_revert%'
    LOOP
        EXECUTE format(
            'ALTER TABLE perps_lp_settlement_transactions DROP CONSTRAINT %I',
            old_constraint.conname
        );
    END LOOP;
    IF NOT EXISTS (
        SELECT 1 FROM pg_constraint k
        WHERE k.conrelid = 'perps_lp_settlement_transactions'::regclass
          AND k.conname = 'perps_lp_settlement_confirmation_state_check'
          AND k.contype = 'c'
          AND k.convalidated
          AND encode(sha256(convert_to(regexp_replace(lower(trim(pg_get_constraintdef(k.oid, true))), E'\\s+', ' ', 'g'), 'UTF8')), 'hex') =
              '10a4e0fa933c2201f0e5525531dc709a69c33a7f041b08f299ea9db78a73a777'
    ) THEN
        ALTER TABLE perps_lp_settlement_transactions
            DROP CONSTRAINT IF EXISTS perps_lp_settlement_confirmation_state_check;
        ALTER TABLE perps_lp_settlement_transactions
            ADD CONSTRAINT perps_lp_settlement_confirmation_state_check CHECK (
                (
                    status IN ('confirmed_success', 'confirmed_revert')
                    AND confirmed_at IS NOT NULL
                    AND confirmation_depth IS NOT NULL
                )
                OR (
                    status = 'superseded'
                    AND (
                        confirmed_at IS NULL
                        OR (
                            receipt_transaction_hash IS NOT NULL
                            AND confirmation_depth IS NOT NULL
                        )
                    )
                )
                OR (
                    status NOT IN ('confirmed_success', 'confirmed_revert', 'superseded')
                    AND confirmed_at IS NULL
                )
            );
    END IF;
END
$$;

DO $$
DECLARE
    old_constraint record;
BEGIN
    FOR old_constraint IN
        SELECT conname
        FROM pg_constraint
        WHERE conrelid = 'perps_lp_settlement_transactions'::regclass
          AND contype = 'c'
          AND conname <> 'perps_lp_settlement_terminal_receipt_identity_check'
          AND pg_get_constraintdef(oid) ILIKE '%receipt_transaction_hash%'
          AND pg_get_constraintdef(oid) ILIKE '%signed_transaction_hash%'
    LOOP
        EXECUTE format(
            'ALTER TABLE perps_lp_settlement_transactions DROP CONSTRAINT %I',
            old_constraint.conname
        );
    END LOOP;
    IF NOT EXISTS (
        SELECT 1 FROM pg_constraint k
        WHERE k.conrelid = 'perps_lp_settlement_transactions'::regclass
          AND k.conname = 'perps_lp_settlement_terminal_receipt_identity_check'
          AND k.contype = 'c'
          AND k.convalidated
          AND encode(sha256(convert_to(regexp_replace(lower(trim(pg_get_constraintdef(k.oid, true))), E'\\s+', ' ', 'g'), 'UTF8')), 'hex') =
              'c2f90b0ccef94f85ebc3f3365ddfed88443aced61775661cf01b5db62babb079'
    ) THEN
        ALTER TABLE perps_lp_settlement_transactions
            DROP CONSTRAINT IF EXISTS perps_lp_settlement_terminal_receipt_identity_check;
        ALTER TABLE perps_lp_settlement_transactions
            ADD CONSTRAINT perps_lp_settlement_terminal_receipt_identity_check CHECK (
                (
                    status NOT IN ('confirmed_success', 'confirmed_revert')
                    AND (status <> 'superseded' OR confirmed_at IS NULL)
                )
                OR receipt_transaction_hash = signed_transaction_hash
            );
    END IF;
    IF NOT EXISTS (
        SELECT 1 FROM pg_constraint k
        WHERE k.conrelid = 'perps_lp_settlement_transactions'::regclass
          AND k.conname = 'perps_lp_settlement_superseded_receipt_check'
          AND k.contype = 'c'
          AND k.convalidated
          AND encode(sha256(convert_to(regexp_replace(lower(trim(pg_get_constraintdef(k.oid, true))), E'\\s+', ' ', 'g'), 'UTF8')), 'hex') =
              '5003d267946b1bfc7d66aeada89395bcb814829359bb7427c02ca0015c1f39b6'
    ) THEN
        ALTER TABLE perps_lp_settlement_transactions
            DROP CONSTRAINT IF EXISTS perps_lp_settlement_superseded_receipt_check;
        ALTER TABLE perps_lp_settlement_transactions
            ADD CONSTRAINT perps_lp_settlement_superseded_receipt_check CHECK (
                status <> 'superseded'
                OR confirmed_at IS NULL
                OR (
                    receipt_transaction_hash IS NOT NULL
                    AND
                    receipt_succeeded IS FALSE
                    AND settlement_event_log_index IS NULL
                    AND confirmation_depth IS NOT NULL
                )
            );
    END IF;
END
$$;

-- manual_review and confirming intentionally block both the monitor lane and
-- the signer's chain-wide nonce lane while work is unresolved.
DO $$
BEGIN
    IF NOT EXISTS (
        SELECT 1
        FROM pg_index i
        JOIN pg_class idx ON idx.oid = i.indexrelid
        JOIN pg_class tbl ON tbl.oid = i.indrelid
        JOIN pg_namespace n ON n.oid = tbl.relnamespace
        JOIN pg_am am ON am.oid = idx.relam
        WHERE n.nspname = current_schema()
          AND idx.relname = 'idx_perps_lp_settlement_one_active'
          AND tbl.relname = 'perps_lp_settlement_transactions'
          AND i.indisunique AND i.indisvalid AND i.indisready AND i.indislive
          AND NOT i.indisexclusion
          AND i.indnatts = 2 AND i.indnkeyatts = 2
          AND am.amname = 'btree'
          AND pg_get_indexdef(i.indexrelid, 1, true) = 'chain_id'
          AND pg_get_indexdef(i.indexrelid, 2, true) = 'monitor_address'
          AND i.indoption::text = '0 0'
          AND encode(sha256(convert_to(regexp_replace(lower(trim(COALESCE(pg_get_expr(i.indpred, i.indrelid, true), ''))), E'\\s+', ' ', 'g'), 'UTF8')), 'hex') =
              '11614e1c65ad7e4cbba8b221d476771aa63e0a0a85d2f0935f083e24ed1eff7a'
    ) THEN
        DROP INDEX IF EXISTS idx_perps_lp_settlement_one_active;
    END IF;
END
$$;
CREATE UNIQUE INDEX IF NOT EXISTS idx_perps_lp_settlement_one_active
    ON perps_lp_settlement_transactions(chain_id, monitor_address)
    WHERE status IN ('prepared', 'broadcast', 'pending', 'confirming', 'manual_review');
DO $$
BEGIN
    IF NOT EXISTS (
        SELECT 1
        FROM pg_index i
        JOIN pg_class idx ON idx.oid = i.indexrelid
        JOIN pg_class tbl ON tbl.oid = i.indrelid
        JOIN pg_namespace n ON n.oid = tbl.relnamespace
        JOIN pg_am am ON am.oid = idx.relam
        WHERE n.nspname = current_schema()
          AND idx.relname = 'idx_perps_lp_settlement_one_active_signer'
          AND tbl.relname = 'perps_lp_settlement_transactions'
          AND i.indisunique AND i.indisvalid AND i.indisready AND i.indislive
          AND NOT i.indisexclusion
          AND i.indnatts = 2 AND i.indnkeyatts = 2
          AND am.amname = 'btree'
          AND pg_get_indexdef(i.indexrelid, 1, true) = 'chain_id'
          AND pg_get_indexdef(i.indexrelid, 2, true) = 'signer_address'
          AND i.indoption::text = '0 0'
          AND encode(sha256(convert_to(regexp_replace(lower(trim(COALESCE(pg_get_expr(i.indpred, i.indrelid, true), ''))), E'\\s+', ' ', 'g'), 'UTF8')), 'hex') =
              '11614e1c65ad7e4cbba8b221d476771aa63e0a0a85d2f0935f083e24ed1eff7a'
    ) THEN
        DROP INDEX IF EXISTS idx_perps_lp_settlement_one_active_signer;
    END IF;
END
$$;
CREATE UNIQUE INDEX IF NOT EXISTS idx_perps_lp_settlement_one_active_signer
    ON perps_lp_settlement_transactions(chain_id, signer_address)
    WHERE status IN ('prepared', 'broadcast', 'pending', 'confirming', 'manual_review');
DO $$
BEGIN
    IF NOT EXISTS (
        SELECT 1
        FROM pg_index i
        JOIN pg_class idx ON idx.oid = i.indexrelid
        JOIN pg_class tbl ON tbl.oid = i.indrelid
        JOIN pg_namespace n ON n.oid = tbl.relnamespace
        JOIN pg_am am ON am.oid = idx.relam
        WHERE n.nspname = current_schema()
          AND idx.relname = 'idx_perps_lp_settlement_one_terminal_nonce'
          AND tbl.relname = 'perps_lp_settlement_transactions'
          AND i.indisunique AND i.indisvalid AND i.indisready AND i.indislive
          AND NOT i.indisexclusion
          AND i.indnatts = 3 AND i.indnkeyatts = 3
          AND am.amname = 'btree'
          AND pg_get_indexdef(i.indexrelid, 1, true) = 'chain_id'
          AND pg_get_indexdef(i.indexrelid, 2, true) = 'signer_address'
          AND pg_get_indexdef(i.indexrelid, 3, true) = 'tx_nonce'
          AND i.indoption::text = '0 0 0'
          AND encode(sha256(convert_to(regexp_replace(lower(trim(COALESCE(pg_get_expr(i.indpred, i.indrelid, true), ''))), E'\\s+', ' ', 'g'), 'UTF8')), 'hex') =
              '476c99ceb93e8b141f47c954d09b35e8e52b29e7dcf8cdf1bb1cc194aff580be'
    ) THEN
        DROP INDEX IF EXISTS idx_perps_lp_settlement_one_terminal_nonce;
    END IF;
END
$$;
CREATE UNIQUE INDEX IF NOT EXISTS idx_perps_lp_settlement_one_terminal_nonce
    ON perps_lp_settlement_transactions(chain_id, signer_address, tx_nonce)
    WHERE status IN ('confirmed_success', 'confirmed_revert')
       OR (status = 'superseded' AND confirmed_at IS NOT NULL);
CREATE INDEX IF NOT EXISTS idx_perps_lp_settlement_observation_history
    ON perps_lp_settlement_transactions(chain_id, monitor_address, observation_digest, replacement_count);
CREATE INDEX IF NOT EXISTS idx_perps_lp_settlement_receipt_recheck
    ON perps_lp_settlement_transactions(chain_id, monitor_address, updated_at)
    WHERE status IN ('broadcast', 'pending', 'confirming', 'manual_review');
CREATE INDEX IF NOT EXISTS idx_perps_lp_settlement_success_heartbeat
    ON perps_lp_settlement_transactions(chain_id, monitor_address, confirmed_at DESC)
    WHERE status = 'confirmed_success';

-- A replacement is a new prepared intent whose semantic transaction fields
-- exactly match its predecessor. Only its fee envelope, signed bytes, and hash
-- may change, and at least one fee component must increase.
CREATE OR REPLACE FUNCTION validate_lp_settlement_replacement_insert()
RETURNS trigger AS $$
DECLARE
    predecessor perps_lp_settlement_transactions%ROWTYPE;
BEGIN
    IF NEW.status <> 'prepared' THEN
        RAISE EXCEPTION 'LP settlement transactions must be inserted prepared';
    END IF;
    IF NEW.replaces_attempt_id IS NULL THEN
        RETURN NEW;
    END IF;
    SELECT *
    INTO predecessor
    FROM perps_lp_settlement_transactions
    WHERE id = NEW.replaces_attempt_id;
    IF NOT FOUND THEN
        RAISE EXCEPTION 'LP settlement replacement predecessor does not exist';
    END IF;
    IF predecessor.status <> 'replaced' THEN
        RAISE EXCEPTION 'LP settlement replacement predecessor is not marked replaced';
    END IF;
    IF NEW.replacement_count <> predecessor.replacement_count + 1 THEN
        RAISE EXCEPTION 'invalid LP settlement replacement_count';
    END IF;
    IF ROW(
        NEW.chain_id,
        NEW.monitor_address,
        NEW.observation_digest,
        NEW.epoch,
        NEW.signer_address,
        NEW.tx_nonce,
        NEW.target_address,
        NEW.tx_value,
        NEW.calldata,
        NEW.gas_limit
    ) IS DISTINCT FROM ROW(
        predecessor.chain_id,
        predecessor.monitor_address,
        predecessor.observation_digest,
        predecessor.epoch,
        predecessor.signer_address,
        predecessor.tx_nonce,
        predecessor.target_address,
        predecessor.tx_value,
        predecessor.calldata,
        predecessor.gas_limit
    ) THEN
        RAISE EXCEPTION 'LP settlement replacement changed signed transaction semantics';
    END IF;
    IF NEW.max_priority_fee_per_gas < predecessor.max_priority_fee_per_gas
       OR NEW.max_fee_per_gas < predecessor.max_fee_per_gas
       OR (
           NEW.max_priority_fee_per_gas = predecessor.max_priority_fee_per_gas
           AND NEW.max_fee_per_gas = predecessor.max_fee_per_gas
       ) THEN
        RAISE EXCEPTION 'LP settlement replacement fees did not increase';
    END IF;
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

DO $$
BEGIN
    IF NOT EXISTS (
        SELECT 1
        FROM pg_trigger
        WHERE tgname = 'trg_perps_lp_settlement_replacement_insert'
          AND tgrelid = 'perps_lp_settlement_transactions'::regclass
    ) THEN
        BEGIN
            CREATE TRIGGER trg_perps_lp_settlement_replacement_insert
                BEFORE INSERT ON perps_lp_settlement_transactions
                FOR EACH ROW EXECUTE FUNCTION validate_lp_settlement_replacement_insert();
        EXCEPTION WHEN duplicate_object THEN
            NULL;
        END;
    END IF;
END $$;

CREATE OR REPLACE FUNCTION require_lp_settlement_replacement_successor()
RETURNS trigger AS $$
BEGIN
    IF NEW.status = 'replaced'
       AND NOT EXISTS (
           SELECT 1
           FROM perps_lp_settlement_transactions successor
           WHERE successor.replaces_attempt_id = NEW.id
       ) THEN
        RAISE EXCEPTION 'replaced LP settlement transaction must retain a successor';
    END IF;
    RETURN NULL;
END;
$$ LANGUAGE plpgsql;

DO $$
BEGIN
    IF NOT EXISTS (
        SELECT 1
        FROM pg_trigger
        WHERE tgname = 'trg_perps_lp_settlement_replaced_successor'
          AND tgrelid = 'perps_lp_settlement_transactions'::regclass
    ) THEN
        BEGIN
            CREATE CONSTRAINT TRIGGER trg_perps_lp_settlement_replaced_successor
                AFTER INSERT OR UPDATE ON perps_lp_settlement_transactions
                DEFERRABLE INITIALLY DEFERRED
                FOR EACH ROW EXECUTE FUNCTION require_lp_settlement_replacement_successor();
        EXCEPTION WHEN duplicate_object THEN
            NULL;
        END;
    END IF;
END $$;

DO $$
BEGIN
    IF EXISTS (
        SELECT 1
        FROM perps_lp_settlement_transactions predecessor
        WHERE predecessor.status = 'replaced'
          AND NOT EXISTS (
              SELECT 1
              FROM perps_lp_settlement_transactions successor
              WHERE successor.replaces_attempt_id = predecessor.id
          )
    ) THEN
        RAISE EXCEPTION 'replaced LP settlement transaction exists without a successor';
    END IF;
END $$;

-- Lifecycle and receipt metadata may advance, but a persisted signed intent
-- can never be rewritten or deleted. Fee replacement therefore always creates
-- a new row and preserves the predecessor as history.
CREATE OR REPLACE FUNCTION protect_lp_settlement_transaction_intent()
RETURNS trigger AS $$
BEGIN
    IF TG_OP = 'DELETE' THEN
        RAISE EXCEPTION 'perps_lp_settlement_transactions is append-only';
    END IF;
    IF ROW(
        OLD.id,
        OLD.chain_id,
        OLD.monitor_address,
        OLD.observation_digest,
        OLD.epoch,
        OLD.replacement_count,
        OLD.replaces_attempt_id,
        OLD.signer_address,
        OLD.tx_nonce,
        OLD.target_address,
        OLD.tx_value,
        OLD.calldata,
        OLD.gas_limit,
        OLD.max_priority_fee_per_gas,
        OLD.max_fee_per_gas,
        OLD.signed_raw_transaction,
        OLD.signed_transaction_hash,
        OLD.created_at
    ) IS DISTINCT FROM ROW(
        NEW.id,
        NEW.chain_id,
        NEW.monitor_address,
        NEW.observation_digest,
        NEW.epoch,
        NEW.replacement_count,
        NEW.replaces_attempt_id,
        NEW.signer_address,
        NEW.tx_nonce,
        NEW.target_address,
        NEW.tx_value,
        NEW.calldata,
        NEW.gas_limit,
        NEW.max_priority_fee_per_gas,
        NEW.max_fee_per_gas,
        NEW.signed_raw_transaction,
        NEW.signed_transaction_hash,
        NEW.created_at
    ) THEN
        RAISE EXCEPTION 'LP settlement signed intent is immutable';
    END IF;
    IF NOT (
        OLD.status = NEW.status
        OR (
            OLD.status = 'prepared'
            AND NEW.status IN (
                'broadcast', 'pending', 'manual_review', 'replaced',
                'confirmed_success', 'superseded'
            )
        )
        OR (
            OLD.status IN ('broadcast', 'pending')
            AND NEW.status IN (
                'broadcast', 'pending', 'confirming', 'manual_review',
                'replaced', 'confirmed_success', 'superseded'
            )
        )
        OR (
            OLD.status = 'confirming'
            AND NEW.status IN (
                'pending', 'manual_review', 'confirmed_success', 'superseded'
            )
        )
        OR (
            OLD.status = 'manual_review'
            AND NEW.status IN ('confirmed_success', 'superseded')
        )
        OR (
            OLD.status = 'replaced'
            AND NEW.status IN ('manual_review', 'confirmed_success', 'superseded')
        )
    ) THEN
        RAISE EXCEPTION 'invalid LP settlement transaction status transition';
    END IF;
    IF OLD.status IN ('prepared', 'broadcast', 'pending', 'confirming', 'manual_review')
       AND NEW.status IN ('failed', 'abandoned') THEN
        RAISE EXCEPTION 'active LP settlement transaction cannot be released without canonical receipt evidence';
    END IF;
    IF OLD.status = 'manual_review'
       AND NEW.status NOT IN ('manual_review', 'confirmed_success', 'superseded') THEN
        RAISE EXCEPTION 'manual-review LP settlement transaction cannot be reopened without canonical terminal evidence';
    END IF;
    IF OLD.status IN ('prepared', 'broadcast', 'pending', 'confirming', 'manual_review')
       AND NEW.status = 'confirmed_revert' THEN
        RAISE EXCEPTION 'reverted LP settlement receipt must be recorded as manual review or receipt-backed superseded';
    END IF;
    IF OLD.status IN ('prepared', 'broadcast', 'pending', 'confirming', 'manual_review')
       AND NEW.status = 'superseded'
       AND NEW.confirmed_at IS NULL
       AND NOT EXISTS (
           SELECT 1
           FROM perps_lp_settlement_transactions winner
           WHERE winner.id <> OLD.id
             AND winner.chain_id = OLD.chain_id
             AND winner.signer_address = OLD.signer_address
             AND winner.tx_nonce = OLD.tx_nonce
             AND (
                 winner.status IN ('confirmed_success', 'confirmed_revert')
                 OR (winner.status = 'superseded' AND winner.confirmed_at IS NOT NULL)
             )
       ) THEN
        RAISE EXCEPTION 'active LP settlement transaction cannot be superseded without a terminal same-nonce receipt';
    END IF;
    IF (
           OLD.status IN ('confirmed_success', 'confirmed_revert')
           OR (OLD.status = 'superseded' AND OLD.confirmed_at IS NOT NULL)
       )
       AND ROW(
           OLD.status,
           OLD.last_error,
           OLD.receipt_transaction_hash,
           OLD.receipt_block_number,
           OLD.receipt_block_hash,
           OLD.receipt_succeeded,
           OLD.confirmed_at,
           OLD.confirmation_depth,
           OLD.settlement_event_log_index,
           OLD.cutoff_epoch,
           OLD.senior_redeem_assets,
           OLD.junior_redeem_assets,
           OLD.junior_deposit_assets,
           OLD.senior_deposit_assets,
           OLD.senior_backlog,
           OLD.junior_backlog,
           OLD.entries_deferred
       ) IS DISTINCT FROM ROW(
           NEW.status,
           NEW.last_error,
           NEW.receipt_transaction_hash,
           NEW.receipt_block_number,
           NEW.receipt_block_hash,
           NEW.receipt_succeeded,
           NEW.confirmed_at,
           NEW.confirmation_depth,
           NEW.settlement_event_log_index,
           NEW.cutoff_epoch,
           NEW.senior_redeem_assets,
           NEW.junior_redeem_assets,
           NEW.junior_deposit_assets,
           NEW.senior_deposit_assets,
           NEW.senior_backlog,
           NEW.junior_backlog,
           NEW.entries_deferred
       ) THEN
        RAISE EXCEPTION 'terminal LP settlement evidence is immutable';
    END IF;
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

DO $$
BEGIN
    IF NOT EXISTS (
        SELECT 1
        FROM pg_trigger
        WHERE tgname = 'trg_perps_lp_settlement_transaction_intent'
          AND tgrelid = 'perps_lp_settlement_transactions'::regclass
    ) THEN
        BEGIN
            CREATE TRIGGER trg_perps_lp_settlement_transaction_intent
                BEFORE UPDATE OR DELETE ON perps_lp_settlement_transactions
                FOR EACH ROW EXECUTE FUNCTION protect_lp_settlement_transaction_intent();
        EXCEPTION WHEN duplicate_object THEN
            NULL;
        END;
    END IF;
END $$;

-- Every RPC send has an immutable history row, including rejected and
-- ambiguous responses. A database trigger enforces append-only semantics.
CREATE TABLE IF NOT EXISTS perps_lp_settlement_broadcasts (
    id BIGSERIAL PRIMARY KEY,
    attempt_id BIGINT NOT NULL REFERENCES perps_lp_settlement_transactions(id) ON DELETE RESTRICT,
    broadcast_sequence INTEGER NOT NULL CHECK (broadcast_sequence > 0),
    outcome VARCHAR(24) NOT NULL CHECK (outcome IN ('accepted', 'already_known', 'rejected', 'ambiguous')),
    returned_transaction_hash VARCHAR(66) CHECK (returned_transaction_hash IS NULL OR returned_transaction_hash ~ '^0x[0-9a-f]{64}$'),
    rpc_error TEXT,
    broadcast_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    UNIQUE (attempt_id, broadcast_sequence)
);

CREATE OR REPLACE FUNCTION reject_lp_settlement_broadcast_mutation()
RETURNS trigger AS $$
BEGIN
    RAISE EXCEPTION 'perps_lp_settlement_broadcasts is append-only';
END;
$$ LANGUAGE plpgsql;

DO $$
BEGIN
    IF NOT EXISTS (
        SELECT 1
        FROM pg_trigger
        WHERE tgname = 'trg_perps_lp_settlement_broadcasts_append_only'
          AND tgrelid = 'perps_lp_settlement_broadcasts'::regclass
    ) THEN
        BEGIN
            CREATE TRIGGER trg_perps_lp_settlement_broadcasts_append_only
                BEFORE UPDATE OR DELETE ON perps_lp_settlement_broadcasts
                FOR EACH ROW EXECUTE FUNCTION reject_lp_settlement_broadcast_mutation();
        EXCEPTION WHEN duplicate_object THEN
            NULL;
        END;
    END IF;
END $$;

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

-- Canonical vault log index. The deployment identity scopes rebuilds so a
-- reorg cannot delete another release's data.
CREATE TABLE IF NOT EXISTS vault_activity_indexer_state (
    chain_id NUMERIC(78,0) NOT NULL,
    house_pool_address VARCHAR(42) NOT NULL,
    senior_vault_address VARCHAR(42) NOT NULL,
    junior_vault_address VARCHAR(42) NOT NULL,
    deployment_block NUMERIC(78,0) NOT NULL,
    last_indexed_block NUMERIC(78,0) NOT NULL,
    last_indexed_block_hash VARCHAR(66),
    last_indexed_block_timestamp BIGINT NOT NULL DEFAULT 0,
    safe_head_block NUMERIC(78,0) NOT NULL DEFAULT 0,
    safe_head_block_hash VARCHAR(66),
    safe_head_timestamp BIGINT NOT NULL DEFAULT 0,
    backfill_complete BOOLEAN NOT NULL DEFAULT FALSE,
    last_success_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    PRIMARY KEY (chain_id, house_pool_address, senior_vault_address, junior_vault_address, deployment_block),
    CHECK (chain_id > 0 AND deployment_block >= 0 AND last_indexed_block >= 0 AND safe_head_block >= 0),
    CHECK (house_pool_address ~ '^0x[0-9a-f]{40}$'),
    CHECK (senior_vault_address ~ '^0x[0-9a-f]{40}$'),
    CHECK (junior_vault_address ~ '^0x[0-9a-f]{40}$'),
    CHECK (last_indexed_block_hash IS NULL OR last_indexed_block_hash ~ '^0x[0-9a-f]{64}$'),
    CHECK (safe_head_block_hash IS NULL OR safe_head_block_hash ~ '^0x[0-9a-f]{64}$')
);
ALTER TABLE vault_activity_indexer_state
    ADD COLUMN IF NOT EXISTS last_indexed_block_timestamp BIGINT NOT NULL DEFAULT 0;

-- Public Lens snapshots that attribute finalized-but-unclaimed deposit shares
-- plus pending/refundable redeem shares to their request controller. This
-- cursor is independent from log ingestion so transient Lens failures cannot
-- stop canonical vault event indexing.
CREATE TABLE IF NOT EXISTS vault_deposit_attribution_state (
    chain_id NUMERIC(78,0) NOT NULL,
    house_pool_address VARCHAR(42) NOT NULL,
    senior_vault_address VARCHAR(42) NOT NULL,
    junior_vault_address VARCHAR(42) NOT NULL,
    deployment_block NUMERIC(78,0) NOT NULL,
    confirmed_through_block NUMERIC(78,0) NOT NULL,
    confirmed_through_block_hash VARCHAR(66),
    confirmed_through_block_timestamp BIGINT NOT NULL DEFAULT 0,
    backfill_complete BOOLEAN NOT NULL DEFAULT FALSE,
    last_success_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    PRIMARY KEY (chain_id, house_pool_address, senior_vault_address, junior_vault_address, deployment_block),
    CHECK (chain_id > 0 AND deployment_block >= 0 AND confirmed_through_block >= 0),
    CHECK (house_pool_address ~ '^0x[0-9a-f]{40}$'),
    CHECK (senior_vault_address ~ '^0x[0-9a-f]{40}$'),
    CHECK (junior_vault_address ~ '^0x[0-9a-f]{40}$'),
    CHECK (confirmed_through_block_hash IS NULL OR confirmed_through_block_hash ~ '^0x[0-9a-f]{64}$')
);

CREATE TABLE IF NOT EXISTS vault_deposit_request_states (
    chain_id NUMERIC(78,0) NOT NULL,
    house_pool_address VARCHAR(42) NOT NULL,
    deployment_block NUMERIC(78,0) NOT NULL,
    vault_address VARCHAR(42) NOT NULL,
    controller_address VARCHAR(42) NOT NULL,
    request_id NUMERIC(78,0) NOT NULL,
    pending_deposit_assets NUMERIC(78,0) NOT NULL,
    claimable_deposit_assets NUMERIC(78,0) NOT NULL,
    claimable_deposit_shares NUMERIC(78,0) NOT NULL,
    refundable_deposit_assets NUMERIC(78,0) NOT NULL,
    pending_redeem_shares NUMERIC(78,0) NOT NULL,
    refundable_redeem_shares NUMERIC(78,0) NOT NULL,
    redeem_refund_pending BOOLEAN NOT NULL,
    is_active BOOLEAN NOT NULL,
    observed_block NUMERIC(78,0) NOT NULL,
    observed_block_hash VARCHAR(66) NOT NULL,
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    PRIMARY KEY (chain_id, house_pool_address, deployment_block, vault_address, controller_address, request_id),
    CHECK (chain_id > 0 AND deployment_block >= 0 AND request_id >= 0 AND observed_block >= 0),
    CHECK (pending_deposit_assets >= 0 AND claimable_deposit_assets >= 0 AND claimable_deposit_shares >= 0 AND refundable_deposit_assets >= 0),
    CHECK (pending_redeem_shares >= 0 AND refundable_redeem_shares >= 0),
    CHECK (is_active = (pending_deposit_assets > 0 OR claimable_deposit_shares > 0 OR pending_redeem_shares > 0 OR refundable_redeem_shares > 0 OR redeem_refund_pending)),
    CHECK (house_pool_address ~ '^0x[0-9a-f]{40}$' AND vault_address ~ '^0x[0-9a-f]{40}$'),
    CHECK (controller_address ~ '^0x[0-9a-f]{40}$'),
    CHECK (observed_block_hash ~ '^0x[0-9a-f]{64}$')
);
CREATE INDEX IF NOT EXISTS idx_vault_deposit_request_states_active
    ON vault_deposit_request_states(chain_id, house_pool_address, deployment_block, is_active, vault_address, controller_address, request_id);
CREATE INDEX IF NOT EXISTS idx_vault_deposit_request_states_attribution
    ON vault_deposit_request_states(chain_id, house_pool_address, deployment_block, vault_address, controller_address, claimable_deposit_shares, pending_redeem_shares, refundable_redeem_shares);

CREATE TABLE IF NOT EXISTS vault_attributed_holder_balances (
    chain_id NUMERIC(78,0) NOT NULL,
    house_pool_address VARCHAR(42) NOT NULL,
    deployment_block NUMERIC(78,0) NOT NULL,
    vault_address VARCHAR(42) NOT NULL,
    holder_address VARCHAR(42) NOT NULL,
    share_balance NUMERIC(78,0) NOT NULL,
    unclaimed_deposit_shares NUMERIC(78,0) NOT NULL,
    withdrawal_escrow_shares NUMERIC(78,0) NOT NULL,
    total_attributed_shares NUMERIC(78,0) NOT NULL,
    observed_block NUMERIC(78,0) NOT NULL,
    observed_block_hash VARCHAR(66) NOT NULL,
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    PRIMARY KEY (chain_id, house_pool_address, deployment_block, vault_address, holder_address),
    CHECK (share_balance >= 0 AND unclaimed_deposit_shares >= 0 AND withdrawal_escrow_shares >= 0 AND total_attributed_shares > 0),
    CHECK (total_attributed_shares = share_balance + unclaimed_deposit_shares + withdrawal_escrow_shares),
    CHECK (house_pool_address ~ '^0x[0-9a-f]{40}$' AND vault_address ~ '^0x[0-9a-f]{40}$'),
    CHECK (holder_address ~ '^0x[0-9a-f]{40}$' AND observed_block_hash ~ '^0x[0-9a-f]{64}$')
);
CREATE INDEX IF NOT EXISTS idx_vault_attributed_holder_balances_rank
    ON vault_attributed_holder_balances(chain_id, house_pool_address, deployment_block, vault_address, total_attributed_shares DESC, holder_address);

CREATE TABLE IF NOT EXISTS vault_canonical_logs (
    chain_id NUMERIC(78,0) NOT NULL,
    house_pool_address VARCHAR(42) NOT NULL,
    deployment_block NUMERIC(78,0) NOT NULL,
    vault_address VARCHAR(42) NOT NULL,
    event_name TEXT NOT NULL,
    tx_hash VARCHAR(66) NOT NULL,
    block_number NUMERIC(78,0) NOT NULL,
    block_hash VARCHAR(66) NOT NULL,
    tx_index NUMERIC(78,0) NOT NULL,
    log_index NUMERIC(78,0) NOT NULL,
    block_timestamp BIGINT NOT NULL,
    PRIMARY KEY (chain_id, house_pool_address, deployment_block, vault_address, tx_hash, log_index),
    CHECK (event_name IN ('Transfer', 'DepositRequest', 'RedeemRequest', 'DepositRequested')),
    CHECK (chain_id > 0 AND deployment_block >= 0 AND block_number >= 0 AND tx_index >= 0 AND log_index >= 0),
    CHECK (house_pool_address ~ '^0x[0-9a-f]{40}$' AND vault_address ~ '^0x[0-9a-f]{40}$'),
    CHECK (tx_hash ~ '^0x[0-9a-f]{64}$' AND block_hash ~ '^0x[0-9a-f]{64}$')
);

CREATE TABLE IF NOT EXISTS vault_share_transfers (
    chain_id NUMERIC(78,0) NOT NULL,
    house_pool_address VARCHAR(42) NOT NULL,
    deployment_block NUMERIC(78,0) NOT NULL,
    vault_address VARCHAR(42) NOT NULL,
    from_address VARCHAR(42) NOT NULL,
    to_address VARCHAR(42) NOT NULL,
    amount NUMERIC(78,0) NOT NULL,
    tx_hash VARCHAR(66) NOT NULL,
    block_number NUMERIC(78,0) NOT NULL,
    block_hash VARCHAR(66) NOT NULL,
    tx_index NUMERIC(78,0) NOT NULL,
    log_index NUMERIC(78,0) NOT NULL,
    block_timestamp BIGINT NOT NULL,
    PRIMARY KEY (chain_id, house_pool_address, deployment_block, vault_address, tx_hash, log_index),
    CHECK (chain_id > 0 AND deployment_block >= 0 AND amount >= 0 AND block_number >= 0 AND tx_index >= 0 AND log_index >= 0),
    CHECK (house_pool_address ~ '^0x[0-9a-f]{40}$' AND vault_address ~ '^0x[0-9a-f]{40}$'),
    CHECK (from_address ~ '^0x[0-9a-f]{40}$' AND to_address ~ '^0x[0-9a-f]{40}$'),
    CHECK (tx_hash ~ '^0x[0-9a-f]{64}$' AND block_hash ~ '^0x[0-9a-f]{64}$')
);
CREATE INDEX IF NOT EXISTS idx_vault_share_transfers_holder
    ON vault_share_transfers(chain_id, house_pool_address, deployment_block, vault_address, from_address, to_address);

CREATE TABLE IF NOT EXISTS vault_holder_balances (
    chain_id NUMERIC(78,0) NOT NULL,
    house_pool_address VARCHAR(42) NOT NULL,
    deployment_block NUMERIC(78,0) NOT NULL,
    vault_address VARCHAR(42) NOT NULL,
    holder_address VARCHAR(42) NOT NULL,
    share_balance NUMERIC(78,0) NOT NULL,
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    PRIMARY KEY (chain_id, house_pool_address, deployment_block, vault_address, holder_address),
    CHECK (share_balance > 0),
    CHECK (house_pool_address ~ '^0x[0-9a-f]{40}$' AND vault_address ~ '^0x[0-9a-f]{40}$'),
    CHECK (holder_address ~ '^0x[0-9a-f]{40}$')
);
CREATE INDEX IF NOT EXISTS idx_vault_holder_balances_rank
    ON vault_holder_balances(chain_id, house_pool_address, deployment_block, vault_address, share_balance DESC, holder_address);

CREATE TABLE IF NOT EXISTS vault_request_events (
    chain_id NUMERIC(78,0) NOT NULL,
    house_pool_address VARCHAR(42) NOT NULL,
    deployment_block NUMERIC(78,0) NOT NULL,
    vault_address VARCHAR(42) NOT NULL,
    event_name TEXT NOT NULL,
    controller_address VARCHAR(42) NOT NULL,
    owner_address VARCHAR(42) NOT NULL,
    request_id NUMERIC(78,0) NOT NULL,
    raw_amount NUMERIC(78,0) NOT NULL,
    tx_hash VARCHAR(66) NOT NULL,
    block_number NUMERIC(78,0) NOT NULL,
    block_hash VARCHAR(66) NOT NULL,
    tx_index NUMERIC(78,0) NOT NULL,
    log_index NUMERIC(78,0) NOT NULL,
    block_timestamp BIGINT NOT NULL,
    PRIMARY KEY (chain_id, house_pool_address, deployment_block, vault_address, tx_hash, log_index),
    CHECK (event_name IN ('DepositRequest', 'RedeemRequest', 'DepositRequested')),
    CHECK (chain_id > 0 AND deployment_block >= 0 AND request_id >= 0 AND raw_amount >= 0 AND block_number >= 0 AND tx_index >= 0 AND log_index >= 0),
    CHECK (house_pool_address ~ '^0x[0-9a-f]{40}$' AND vault_address ~ '^0x[0-9a-f]{40}$'),
    CHECK (controller_address ~ '^0x[0-9a-f]{40}$' AND owner_address ~ '^0x[0-9a-f]{40}$'),
    CHECK (tx_hash ~ '^0x[0-9a-f]{64}$' AND block_hash ~ '^0x[0-9a-f]{64}$')
);
CREATE INDEX IF NOT EXISTS idx_vault_request_events_recent
    ON vault_request_events(chain_id, house_pool_address, deployment_block, vault_address, block_number DESC, tx_index DESC, log_index DESC);
CREATE INDEX IF NOT EXISTS idx_vault_request_events_controller
    ON vault_request_events(chain_id, house_pool_address, deployment_block, vault_address, controller_address, request_id DESC);
CREATE INDEX IF NOT EXISTS idx_vault_request_events_owner
    ON vault_request_events(chain_id, house_pool_address, deployment_block, vault_address, owner_address, request_id DESC);
