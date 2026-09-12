-- Additive, operator-approved migration. No startup DDL or runtime grants.
-- Keep rows through rollback: reusing an ID must never silently issue new work.
BEGIN;
CREATE TABLE IF NOT EXISTS aa_preparations (
  client_key TEXT NOT NULL,
  sender TEXT NOT NULL,
  preparation_id TEXT NOT NULL,
  intent_hash TEXT NOT NULL,
  operation JSONB,
  authorization_digest TEXT REFERENCES aa_sponsorship_authorizations(digest),
  expires_at TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp()+interval '5 minutes',
  lease_token TEXT,
  lease_until TIMESTAMPTZ,
  updated_at TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp(),
  PRIMARY KEY (client_key,sender,preparation_id),
  CHECK (sender ~ '^0x[0-9a-f]{40}$'),
  CHECK (preparation_id ~ '^0x[0-9a-f]{64}$'),
  CHECK (intent_hash ~ '^0x[0-9a-f]{64}$'),
  CHECK (operation IS NULL OR jsonb_typeof(operation)='object'),
  CHECK ((lease_token IS NULL) = (lease_until IS NULL))
);
COMMIT;
