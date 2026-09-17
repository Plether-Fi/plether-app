-- Backward compatible: existing writers still check immediately. Only the new
-- atomic publisher explicitly defers this FK until its short publication phase.
ALTER TABLE insights_account_snapshots
  ALTER CONSTRAINT insights_account_snapshots_competition_slug_fkey
  DEFERRABLE INITIALLY IMMEDIATE;
