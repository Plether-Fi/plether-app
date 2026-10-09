-- Apply after aa-preparation-recovery-v1.sql, before enabling Arbitrum mainnet
-- preparation recovery. Historical v1 evidence remains attributed to Sepolia;
-- do not replay its fixed-chain backfill for new mainnet preparations.
-- This changes only the supported-chain constraint, preserving every registry,
-- authorization, lease, generation and retirement record. Safe to apply again.
BEGIN;
SET LOCAL lock_timeout='5s';
SET LOCAL statement_timeout='15s';
SELECT pg_advisory_xact_lock(4338008421614);
ALTER TABLE aa_preparation_registry
  DROP CONSTRAINT IF EXISTS aa_preparation_registry_chain_id_check;
ALTER TABLE aa_preparation_registry
  ADD CONSTRAINT aa_preparation_registry_chain_id_check
  CHECK (chain_id IN (42161,421614));
COMMIT;
