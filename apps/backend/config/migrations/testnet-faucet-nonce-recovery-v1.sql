-- Additive audit only. No claim is released by this migration.
CREATE TABLE IF NOT EXISTS testnet_faucet_nonce_recoveries (
    original_tx_hash VARCHAR(66) PRIMARY KEY,
    address VARCHAR(42) NOT NULL,
    token_address VARCHAR(42) NOT NULL,
    amount BIGINT NOT NULL,
    original_raw_tx TEXT NOT NULL,
    chain_id BIGINT NOT NULL CHECK (chain_id = 421614),
    sender VARCHAR(42) NOT NULL,
    nonce BIGINT NOT NULL CHECK (nonce >= 0),
    replacement_tx_hash VARCHAR(66) NOT NULL,
    replacement_block_number BIGINT NOT NULL,
    replacement_block_hash VARCHAR(66) NOT NULL,
    verified_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    CHECK (original_tx_hash <> replacement_tx_hash)
);
