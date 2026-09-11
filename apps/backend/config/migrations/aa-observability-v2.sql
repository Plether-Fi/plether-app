-- Apply after aa-observability-v1.sql, before the new API image. Additive only.
BEGIN;
ALTER TABLE aa_attempt_diagnostics
  ADD COLUMN IF NOT EXISTS correlation_checked_at TIMESTAMPTZ,
  ADD COLUMN IF NOT EXISTS correlation_resolved_at TIMESTAMPTZ,
  ADD COLUMN IF NOT EXISTS execution_observed_at TIMESTAMP;
CREATE INDEX IF NOT EXISTS aa_diagnostics_pending_correlation
  ON aa_attempt_diagnostics(chain_id,deployment,correlation_checked_at,created_at)
  WHERE correlation_resolved_at IS NULL AND order_id IS NULL AND terminal_at IS NULL;
COMMIT;
