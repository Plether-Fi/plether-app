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
