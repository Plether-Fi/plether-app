-- Advisory only. Apply before the backend rollout. Missing table drops telemetry,
-- never blocks sponsorship/submission. Do not use browser reports as chain proof.
BEGIN;
CREATE INDEX IF NOT EXISTS aa_preparations_diagnostic_attempt
  ON aa_preparations(diagnostic_attempt_id) WHERE diagnostic_attempt_id IS NOT NULL;
CREATE TABLE IF NOT EXISTS aa_attempt_events (
  attempt_id UUID NOT NULL,
  source TEXT NOT NULL CHECK (source IN ('browser','backend')),
  stage TEXT NOT NULL CHECK (length(stage) BETWEEN 1 AND 64),
  observed_at TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp(),
  PRIMARY KEY (attempt_id, source, stage)
);
-- At most one row per allowlisted stage and source per attempt. No signatures,
-- operation payloads, provider URLs, error bodies or wallet addresses are stored.
-- Grant SELECT/INSERT to the API role; maintenance alone deletes terminal attempt
-- events after the same seven-day retention as aa_attempt_diagnostics.
COMMIT;
