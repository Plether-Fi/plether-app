BEGIN;
-- Advisory metadata only. Apply before the backend, then release the frontend.
-- Old timeline rows remain valid and are not retrospectively classified.
ALTER TABLE aa_attempt_events
  ADD COLUMN IF NOT EXISTS failure_step TEXT CHECK (length(failure_step) BETWEEN 1 AND 64),
  ADD COLUMN IF NOT EXISTS reason_code TEXT CHECK (length(reason_code) BETWEEN 1 AND 64);
COMMIT;
