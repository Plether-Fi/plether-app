-- Additive advisory evidence only. Apply before starting the funding observer.
BEGIN;
CREATE TABLE IF NOT EXISTS aa_funding_observations (
  chain_id BIGINT NOT NULL,
  deployment TEXT NOT NULL,
  inventory_id TEXT NOT NULL CHECK (inventory_id ~ '^[0-9a-f]{64}$'),
  component TEXT NOT NULL CHECK (component IN ('alto','keeper','oracle','liquidation','protection','lp_settlement')),
  signer_address TEXT NOT NULL CHECK (signer_address ~ '^0x[0-9a-f]{40}$'),
  state TEXT NOT NULL CHECK (state IN ('ready','blocked','unknown')),
  reason TEXT NOT NULL CHECK (reason IN ('READY','FUNDING_LOW','FUNDING_UNVERIFIED','WORKER_INSUFFICIENT_FUNDS')),
  balance_wei NUMERIC(78,0) CHECK (balance_wei >= 0),
  liability_wei NUMERIC(78,0) CHECK (liability_wei >= 0),
  reserve_wei NUMERIC(78,0) CHECK (reserve_wei > 0),
  observed_at TIMESTAMPTZ NOT NULL,
  PRIMARY KEY(chain_id,deployment,signer_address)
);
-- API: SELECT only. Observer: SELECT on existing recovery journals and
-- SELECT/INSERT/DELETE on this table. No authorization/recovery-table writes.
COMMIT;
