-- Initial destination-action funding schema, not yet deployed.
-- Quotes, relay ownership and orphan history remain durable through rollback.
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
  status TEXT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp(),
  last_polled_at TIMESTAMPTZ,
  CHECK (payload->>'status' IS NOT DISTINCT FROM status)
);
-- Receiver-based development rows cannot be interpreted as destination actions.
-- This feature has not shipped; operators must explicitly reset/migrate such
-- local data rather than having startup rewrite its meaning or discard funds.
DO $$ BEGIN
  IF EXISTS (SELECT 1 FROM perps_funding_quotes WHERE NOT COALESCE(payload->>'destinationMessageHash' ~ '^0x[0-9a-f]{64}$' AND payload->>'destinationMessage' ~ '^0x([0-9a-f]{2})+$', FALSE))
    OR EXISTS (SELECT 1 FROM perps_funding_intents WHERE NOT COALESCE(payload->>'destinationMessageHash' ~ '^0x[0-9a-f]{64}$' AND payload->>'destinationMessage' ~ '^0x([0-9a-f]{2})+$', FALSE)) THEN
    RAISE EXCEPTION 'Legacy funding rows require an explicit migration or development reset';
  END IF;
END $$;
ALTER TABLE perps_funding_quotes DROP CONSTRAINT IF EXISTS perps_funding_quotes_destination_message_check;
ALTER TABLE perps_funding_quotes ADD CONSTRAINT perps_funding_quotes_destination_message_check
  CHECK (COALESCE(payload->>'destinationMessageHash' ~ '^0x[0-9a-f]{64}$' AND payload->>'destinationMessage' ~ '^0x([0-9a-f]{2})+$', FALSE));
ALTER TABLE perps_funding_intents DROP CONSTRAINT IF EXISTS perps_funding_intents_status_check;
ALTER TABLE perps_funding_intents ADD CONSTRAINT perps_funding_intents_status_check
  CHECK (status IN ('awaiting-source','bridging','confirmed','needs-deposit','retryable','failed'));
ALTER TABLE perps_funding_intents DROP CONSTRAINT IF EXISTS perps_funding_intents_destination_message_check;
ALTER TABLE perps_funding_intents ADD CONSTRAINT perps_funding_intents_destination_message_check
  CHECK (COALESCE(payload->>'destinationMessageHash' ~ '^0x[0-9a-f]{64}$' AND payload->>'destinationMessage' ~ '^0x([0-9a-f]{2})+$', FALSE));
CREATE INDEX IF NOT EXISTS perps_funding_intents_poll_idx
  ON perps_funding_intents(last_polled_at NULLS FIRST, created_at, id);
CREATE TABLE IF NOT EXISTS perps_funding_source_relays (
  relay_hash TEXT PRIMARY KEY CHECK (relay_hash ~ '^0x[0-9a-f]{64}$'),
  intent_id TEXT NOT NULL REFERENCES perps_funding_intents(id),
  origin_chain_id NUMERIC(78,0) NOT NULL CHECK (origin_chain_id>0),
  source_spoke_pool TEXT NOT NULL CHECK (source_spoke_pool ~ '^0x[0-9a-f]{40}$'),
  deposit_id NUMERIC(78,0) NOT NULL CHECK (deposit_id>=0),
  evidence JSONB NOT NULL CHECK (jsonb_typeof(evidence)='object' AND evidence->>'relayHash' IS NOT DISTINCT FROM relay_hash),
  canonical BOOLEAN NOT NULL DEFAULT TRUE,
  orphaned_evidence JSONB NOT NULL DEFAULT '[]'::jsonb CHECK (jsonb_typeof(orphaned_evidence)='array'),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp()
);
-- Ownership of a relay hash is permanent, even when its evidence is orphaned.
-- Deposit IDs may be reused after a positive source-chain reorg invalidation.
CREATE UNIQUE INDEX IF NOT EXISTS perps_funding_source_relays_active_intent
  ON perps_funding_source_relays(intent_id) WHERE canonical;
CREATE UNIQUE INDEX IF NOT EXISTS perps_funding_source_relays_active_deposit
  ON perps_funding_source_relays(origin_chain_id,source_spoke_pool,deposit_id) WHERE canonical;
CREATE TABLE IF NOT EXISTS perps_funding_worker_readiness (
  release_id TEXT NOT NULL CHECK (length(release_id)>0),
  chain_id NUMERIC(78,0) NOT NULL CHECK (chain_id>0),
  ready BOOLEAN NOT NULL,
  last_seen TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp(),
  PRIMARY KEY (release_id,chain_id)
);
COMMIT;
