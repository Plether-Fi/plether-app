CREATE TABLE IF NOT EXISTS vault_return_reports (
  chain_id NUMERIC(78,0) NOT NULL,
  house_pool_address VARCHAR(42) NOT NULL,
  senior_vault_address VARCHAR(42) NOT NULL,
  junior_vault_address VARCHAR(42) NOT NULL,
  start_block_number NUMERIC(78,0) NOT NULL,
  end_block_number NUMERIC(78,0) NOT NULL,
  start_block_hash VARCHAR(66) NOT NULL,
  end_block_hash VARCHAR(66) NOT NULL,
  start_share_price NUMERIC(78,0) NOT NULL,
  end_share_price NUMERIC(78,0) NOT NULL,
  schema_version INTEGER NOT NULL DEFAULT 2,
  source_commit TEXT NOT NULL,
  attribution JSONB NOT NULL,
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  PRIMARY KEY (chain_id, house_pool_address, senior_vault_address, junior_vault_address, end_block_number)
);
ALTER TABLE vault_return_reports ALTER COLUMN schema_version SET DEFAULT 2;
CREATE TABLE IF NOT EXISTS vault_return_index_state (
  deployment_key TEXT PRIMARY KEY,
  through_block NUMERIC(78,0) NOT NULL,
  through_hash VARCHAR(66) NOT NULL
);
CREATE TABLE IF NOT EXISTS vault_return_logs (
  deployment_key TEXT NOT NULL,
  block_number NUMERIC(78,0) NOT NULL,
  block_hash VARCHAR(66) NOT NULL,
  transaction_hash VARCHAR(66) NOT NULL,
  log_index INTEGER NOT NULL,
  payload JSONB NOT NULL,
  PRIMARY KEY (deployment_key, block_number, log_index)
);
CREATE TABLE IF NOT EXISTS vault_return_evidence (
  deployment_key TEXT NOT NULL,
  evidence_key TEXT NOT NULL,
  block_hash VARCHAR(66) NOT NULL,
  payload JSONB NOT NULL,
  PRIMARY KEY (deployment_key, evidence_key, block_hash)
);
