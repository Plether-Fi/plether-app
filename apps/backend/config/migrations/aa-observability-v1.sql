-- Additive, operator-applied migration. No authorization tables or invariants change.
BEGIN;
ALTER TABLE aa_preparations
  ADD COLUMN IF NOT EXISTS diagnostic_attempt_id UUID,
  ADD COLUMN IF NOT EXISTS diagnostic_chain_id BIGINT,
  ADD COLUMN IF NOT EXISTS diagnostic_deployment TEXT;
CREATE TABLE IF NOT EXISTS aa_worker_readiness (
  chain_id BIGINT NOT NULL,
  deployment TEXT NOT NULL,
  component TEXT NOT NULL,
  state TEXT NOT NULL CHECK (state IN ('ready','blocked','unknown')),
  reason TEXT NOT NULL,
  required_cost_wei NUMERIC(78,0),
  signer_address TEXT,
  observed_at TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp(),
  PRIMARY KEY(chain_id, deployment, component)
);
CREATE TABLE IF NOT EXISTS aa_attempt_diagnostics (
  attempt_id UUID PRIMARY KEY,
  client_key TEXT NOT NULL,
  chain_id BIGINT NOT NULL,
  deployment TEXT NOT NULL,
  preparation_id TEXT,
  operation_hash TEXT,
  sender TEXT,
  order_id BIGINT,
  stage TEXT NOT NULL,
  reason TEXT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp(),
  terminal_at TIMESTAMPTZ,
  updated_at TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp()
);
CREATE INDEX IF NOT EXISTS aa_attempt_diagnostics_operation ON aa_attempt_diagnostics(operation_hash);
CREATE INDEX IF NOT EXISTS aa_attempt_diagnostics_terminal ON aa_attempt_diagnostics(terminal_at) WHERE terminal_at IS NOT NULL;
-- Operator grants SELECT/INSERT/UPDATE to API/worker roles; the maintenance role
-- alone deletes mappings after terminal_at + 7 days. Unresolved rows never expire.
COMMIT;
