-- Additive bridge funding journal. Apply before enabling funding APIs/workers.
-- Quotes remain immutable; one quote can create at most one durable intent.
-- No automatic deletion: confirmed/failed intents stay recoverable for reorgs
-- and delayed or split destination transfers.
BEGIN;
SELECT pg_advisory_xact_lock(20261008, 3);
CREATE TABLE IF NOT EXISTS perps_funding_quotes (
  id TEXT PRIMARY KEY CHECK (id ~ '^0x[0-9a-f]{64}$'),
  payload JSONB NOT NULL CHECK (jsonb_typeof(payload) = 'object' AND payload->>'quoteId' IS NOT DISTINCT FROM id),
  expires_at TIMESTAMPTZ NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp()
);
CREATE TABLE IF NOT EXISTS perps_funding_intents (
  id TEXT PRIMARY KEY CHECK (id ~ '^0x[0-9a-f]{64}$'),
  idempotency_key TEXT NOT NULL UNIQUE CHECK (idempotency_key ~ '^[-a-zA-Z0-9_:]{16,128}$'),
  quote_id TEXT NOT NULL UNIQUE REFERENCES perps_funding_quotes(id),
  creation_payload JSONB NOT NULL CHECK (jsonb_typeof(creation_payload) = 'object'),
  payload JSONB NOT NULL CHECK (jsonb_typeof(payload) = 'object' AND payload->>'intentId' IS NOT DISTINCT FROM id AND payload->>'quoteId' IS NOT DISTINCT FROM quote_id),
  status TEXT NOT NULL CHECK (status IN ('awaiting-source','bridging','received','depositing','confirmed','retryable','failed')),
  created_at TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp(),
  last_polled_at TIMESTAMPTZ,
  CHECK (payload->>'status' IS NOT DISTINCT FROM status)
);
CREATE INDEX IF NOT EXISTS perps_funding_intents_poll_idx
  ON perps_funding_intents(last_polled_at NULLS FIRST, created_at, id);
CREATE TABLE IF NOT EXISTS perps_funding_worker_readiness (
  release_id TEXT NOT NULL CHECK (length(release_id)>0),
  chain_id NUMERIC(78,0) NOT NULL CHECK (chain_id>0),
  ready BOOLEAN NOT NULL,
  last_seen TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp(),
  PRIMARY KEY (release_id,chain_id)
);
COMMIT;
