-- Additive. Existing wallet claims keep their allocation and mint provenance.
ALTER TABLE testnet_faucet_claims ADD COLUMN IF NOT EXISTS delivery_mode VARCHAR(16) NOT NULL DEFAULT 'wallet';
CREATE TABLE IF NOT EXISTS testnet_margin_funding (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    owner_address VARCHAR(42) NOT NULL,
    address VARCHAR(42) NOT NULL,
    token_address VARCHAR(42) NOT NULL,
    chain_id BIGINT NOT NULL CHECK (chain_id = 421614),
    clearinghouse VARCHAR(42) NOT NULL,
    signer VARCHAR(42) NOT NULL,
    release_id TEXT NOT NULL,
    amount BIGINT NOT NULL CHECK (amount = 100000000000),
    phase VARCHAR(16) NOT NULL DEFAULT 'mint' CHECK (phase IN ('mint','approval','deposit')),
    status VARCHAR(16) NOT NULL DEFAULT 'pending' CHECK (status IN ('pending','funded','failed','recovery')),
    reason TEXT,
    deposit_tx_hash VARCHAR(66),
    deposit_block_number BIGINT,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    -- Shared eligibility is written atomically by reservation. Avoid a foreign
    -- key here: older supported binaries recreate the legacy primary key at
    -- startup and must remain usable for rollback with this additive schema.
    UNIQUE (address, token_address)
);
CREATE TABLE IF NOT EXISTS testnet_margin_funding_transactions (
    id BIGSERIAL PRIMARY KEY,
    claim_id UUID NOT NULL REFERENCES testnet_margin_funding(id),
    phase VARCHAR(16) NOT NULL CHECK (phase IN ('mint','approval','deposit')),
    attempt INTEGER NOT NULL CHECK (attempt > 0),
    signer VARCHAR(42) NOT NULL,
    nonce BIGINT NOT NULL CHECK (nonce >= 0),
    target VARCHAR(42) NOT NULL,
    calldata TEXT NOT NULL,
    tx_hash VARCHAR(66) NOT NULL UNIQUE,
    raw_tx TEXT NOT NULL,
    status VARCHAR(16) NOT NULL DEFAULT 'submitted' CHECK (status IN ('submitted','confirmed','reverted','replaced')),
    block_number BIGINT,
    block_hash VARCHAR(66),
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    UNIQUE (claim_id, phase, attempt),
    UNIQUE (signer, nonce)
);
-- A durable reservation survives process death before the RPC sees the nonce.
CREATE UNIQUE INDEX IF NOT EXISTS testnet_margin_funding_one_submitted_signer
ON testnet_margin_funding_transactions(signer) WHERE status = 'submitted';
CREATE INDEX IF NOT EXISTS testnet_margin_funding_pending
ON testnet_margin_funding(created_at, id) WHERE status = 'pending';

-- Operator proof log. Original signed intent remains in the immutable journal.
CREATE TABLE IF NOT EXISTS testnet_margin_funding_nonce_recoveries (
    original_tx_hash VARCHAR(66) PRIMARY KEY REFERENCES testnet_margin_funding_transactions(tx_hash),
    claim_id UUID NOT NULL REFERENCES testnet_margin_funding(id),
    phase VARCHAR(16) NOT NULL,
    signer VARCHAR(42) NOT NULL,
    nonce BIGINT NOT NULL,
    replacement_tx_hash VARCHAR(66) NOT NULL,
    replacement_block_number BIGINT NOT NULL,
    replacement_block_hash VARCHAR(66) NOT NULL,
    recovered_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);
