-- Apply before deploying the fenced gateway. Retirement is separately gated
-- until ALL serving instances use this registry. Never drop retirement records.
BEGIN;
ALTER TABLE aa_preparations ADD COLUMN IF NOT EXISTS recovery_paymaster TEXT CHECK (recovery_paymaster ~ '^0x[0-9a-f]{40}$');
CREATE TABLE IF NOT EXISTS aa_preparation_registry (
  chain_id BIGINT NOT NULL CHECK (chain_id = 421614),
  paymaster TEXT NOT NULL CHECK (paymaster ~ '^0x[0-9a-f]{40}$'),
  sender TEXT NOT NULL CHECK (sender ~ '^0x[0-9a-f]{40}$'),
  preparation_id TEXT NOT NULL CHECK (preparation_id ~ '^0x[0-9a-f]{64}$'),
  generation BIGINT NOT NULL DEFAULT 0,
  retired BOOLEAN NOT NULL DEFAULT FALSE,
  lease_token TEXT,
  lease_until TIMESTAMPTZ,
  updated_at TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp(),
  PRIMARY KEY (chain_id,paymaster,sender,preparation_id)
);
CREATE TABLE IF NOT EXISTS aa_preparation_authorizations (
  chain_id BIGINT NOT NULL, paymaster TEXT NOT NULL, sender TEXT NOT NULL,
  preparation_id TEXT NOT NULL, digest TEXT NOT NULL REFERENCES aa_sponsorship_authorizations(digest),
  PRIMARY KEY (chain_id,paymaster,sender,preparation_id,digest),
  FOREIGN KEY (chain_id,paymaster,sender,preparation_id)
    REFERENCES aa_preparation_registry(chain_id,paymaster,sender,preparation_id)
);
CREATE TABLE IF NOT EXISTS aa_preparation_recovery_challenges (
  id TEXT PRIMARY KEY, chain_id BIGINT NOT NULL, paymaster TEXT NOT NULL,
  sender TEXT NOT NULL, preparation_id TEXT NOT NULL, owner TEXT NOT NULL,
  message TEXT NOT NULL, expires_at TIMESTAMPTZ NOT NULL, consumed BOOLEAN NOT NULL DEFAULT FALSE
);
CREATE TABLE IF NOT EXISTS aa_preparation_recovery_sessions (
  token_hash TEXT PRIMARY KEY, chain_id BIGINT NOT NULL, paymaster TEXT NOT NULL,
  sender TEXT NOT NULL, preparation_id TEXT NOT NULL, owner TEXT NOT NULL,
  expires_at TIMESTAMPTZ NOT NULL
);
CREATE INDEX IF NOT EXISTS aa_preparation_recovery_challenge_expiry ON aa_preparation_recovery_challenges(expires_at);
CREATE INDEX IF NOT EXISTS aa_preparation_recovery_session_expiry ON aa_preparation_recovery_sessions(expires_at);
CREATE INDEX IF NOT EXISTS aa_preparation_by_sender_id ON aa_preparations(sender,preparation_id);
UPDATE aa_preparations p SET recovery_paymaster=lower(a.operation->>'paymaster')
FROM aa_sponsorship_authorizations a WHERE a.digest=p.authorization_digest
AND a.operation->>'paymaster' ~ '^0x[0-9a-fA-F]{40}$' AND p.recovery_paymaster IS NULL;
-- A historical paymaster is derived only from durable authorization evidence.
-- Unbound records remain intact and are classified conservatively at recovery.
INSERT INTO aa_preparation_registry(chain_id,paymaster,sender,preparation_id)
SELECT DISTINCT 421614,lower(a.operation->>'paymaster'),p.sender,p.preparation_id
FROM aa_preparations p JOIN aa_sponsorship_authorizations a ON a.digest=p.authorization_digest
WHERE a.operation->>'paymaster' ~ '^0x[0-9a-fA-F]{40}$'
ON CONFLICT DO NOTHING;
INSERT INTO aa_preparation_authorizations(chain_id,paymaster,sender,preparation_id,digest)
SELECT 421614,lower(a.operation->>'paymaster'),p.sender,p.preparation_id,a.digest
FROM aa_preparations p JOIN aa_sponsorship_authorizations a ON a.digest=p.authorization_digest
WHERE a.operation->>'paymaster' ~ '^0x[0-9a-fA-F]{40}$'
ON CONFLICT DO NOTHING;
COMMIT;
