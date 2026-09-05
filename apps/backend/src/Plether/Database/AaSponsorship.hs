module Plether.Database.AaSponsorship
  ( SponsorshipDraft (..)
  , SponsorshipAuthorization (..)
  , SubmittedAuthorization (..)
  , AaReconcilerCursor (..)
  , ensureAaSponsorshipSchema
  , reserveSponsorship
  , storeSponsorshipSignature
  , isSponsorshipDeliveryAllowed
  , markSponsorshipSubmitted
  , getSponsorshipByDigest
  , getSponsorshipByRequestKey
  , getSponsorshipByUserOperationHash
  , listSubmittedSponsorships
  , settleSponsorship
  , recordRecoveryOperation
  , isRecoveryOperationAuthorized
  , consumeAaRateLimit
  , pruneAaRateWindows
  , pruneExpiredRecoveryOperations
  , getAaIssuancePause
  , pauseAaIssuance
  , resumeAaIssuance
  , getAaReconcilerCursor
  , initializeAaReconcilerCursor
  , aaSponsorshipStateIsEmpty
  , advanceAaReconcilerCursor
  , recordAaReconcilerHeartbeat
  , expireSponsorshipsThrough
  , cancelStaleUnsignedReservations
  , controlBootstrapReason
  ) where

import Control.Monad (unless, void, when)
import Data.Aeson (Value, encode)
import Data.Int (Int64)
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T
import Database.PostgreSQL.Simple
  ( Connection
  , Only (..)
  , Query
  , execute
  , execute_
  , query
  , query_
  , withTransaction
  )
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import Database.PostgreSQL.Simple.ToRow (ToRow)
import Plether.Config (NativeAaConfig (..))
import Text.Read (readMaybe)

-- | Immutable fields committed by a sponsorship digest.  The client key is an
-- HMAC pseudonym used only to bind recovery reads to the browser that created
-- the operation; raw IP addresses are never persisted.
data SponsorshipDraft = SponsorshipDraft
  { sdRequestKey :: Text
  , sdDigest :: Text
  , sdSender :: Text
  , sdOwner :: Text
  , sdNonce :: Integer
  , sdValidAfter :: Integer
  , sdValidUntil :: Integer
  , sdMaxCostWei :: Integer
  , sdClientKey :: Text
  , sdOperation :: Value
  }
  deriving stock (Eq, Show)

data SponsorshipAuthorization = SponsorshipAuthorization
  { saRequestKey :: Text
  , saDigest :: Text
  , saExpectedUserOperationHash :: Maybe Text
  , saSender :: Text
  , saOwner :: Text
  , saNonce :: Integer
  , saValidAfter :: Integer
  , saValidUntil :: Integer
  , saMaxCostWei :: Integer
  , saClientKey :: Text
  , saSignature :: Maybe Text
  , saState :: Text
  }
  deriving stock (Eq, Show)

instance FromRow SponsorshipAuthorization where
  fromRow =
    SponsorshipAuthorization
      <$> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field

data SubmittedAuthorization = SubmittedAuthorization
  { subDigest :: Text
  , subUserOperationHash :: Text
  , subValidUntil :: Integer
  }
  deriving stock (Eq, Show)

instance FromRow SubmittedAuthorization where
  fromRow = SubmittedAuthorization <$> field <*> field <*> field

data AaReconcilerCursor = AaReconcilerCursor
  { arcSafeBlock :: Integer
  , arcSafeBlockHash :: Text
  }
  deriving stock (Eq, Show)

instance FromRow AaReconcilerCursor where
  fromRow = AaReconcilerCursor <$> field <*> field

data AaSchemaKey = AaSchemaKey
  Text
  Text
  Text
  Text
  Text
  Text
  Text
  Text
  Bool
  Bool
  Bool
  Bool
  Bool
  Bool
  deriving stock (Eq, Ord, Show)

instance FromRow AaSchemaKey where
  fromRow =
    AaSchemaKey
      <$> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field

data AaSchemaIndex = AaSchemaIndex
  Text
  Text
  Bool
  Bool
  Bool
  Text
  Bool
  Int
  Int
  Bool
  Bool
  deriving stock (Eq, Ord, Show)

instance FromRow AaSchemaIndex where
  fromRow =
    AaSchemaIndex
      <$> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field

ensureAaSponsorshipSchema :: Connection -> IO ()
ensureAaSponsorshipSchema conn = withTransaction conn $ do
  void $ execute_ conn
    "CREATE TABLE IF NOT EXISTS aa_sponsorship_authorizations (\
    \request_key VARCHAR(66) NOT NULL,\
    \digest VARCHAR(66) PRIMARY KEY,\
    \expected_user_operation_hash VARCHAR(66) UNIQUE,\
    \user_operation_hash VARCHAR(66) UNIQUE,\
    \sender VARCHAR(42) NOT NULL,\
    \owner VARCHAR(42) NOT NULL,\
    \nonce NUMERIC(78,0) NOT NULL,\
    \valid_after BIGINT NOT NULL,\
    \valid_until BIGINT NOT NULL,\
    \max_cost_wei NUMERIC(78,0) NOT NULL,\
    \client_key VARCHAR(66) NOT NULL,\
    \operation JSONB NOT NULL,\
    \signature VARCHAR(132),\
    \state VARCHAR(16) NOT NULL,\
    \created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),\
    \signed_at TIMESTAMPTZ,\
    \submitted_at TIMESTAMPTZ,\
    \settled_at TIMESTAMPTZ,\
    \updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),\
    \CHECK (request_key ~ '^0x[0-9a-f]{64}$'),\
    \CHECK (digest ~ '^0x[0-9a-f]{64}$'),\
    \CHECK (expected_user_operation_hash IS NULL OR expected_user_operation_hash ~ '^0x[0-9a-f]{64}$'),\
    \CHECK (user_operation_hash IS NULL OR user_operation_hash = expected_user_operation_hash),\
    \CHECK (sender ~ '^0x[0-9a-f]{40}$'),\
    \CHECK (owner ~ '^0x[0-9a-f]{40}$'),\
    \CHECK (nonce >= 0 AND max_cost_wei > 0),\
    \CHECK (valid_after >= 0 AND valid_until > valid_after),\
    \CHECK (signature IS NULL OR signature ~ '^0x[0-9a-f]{130}$'),\
    \CHECK (state IN ('reserved','signed','submitted','settled','expired','cancelled')),\
    \CHECK ((state = 'reserved' AND signature IS NULL) OR state <> 'reserved')\
    \)"
  void $ execute_ conn
    "ALTER TABLE aa_sponsorship_authorizations \
    \ADD COLUMN IF NOT EXISTS request_key VARCHAR(66)"
  -- Rows from the pre-native development schema are conservatively made
  -- non-retryable by using their unique digest as the request key.
  void $ execute_ conn
    "UPDATE aa_sponsorship_authorizations SET request_key=digest WHERE request_key IS NULL"
  void $ execute_ conn
    "ALTER TABLE aa_sponsorship_authorizations ALTER COLUMN request_key SET NOT NULL"
  void $ execute_ conn
    "ALTER TABLE aa_sponsorship_authorizations \
    \DROP CONSTRAINT IF EXISTS aa_sponsorship_authorizations_request_key_key"
  void $ execute_ conn
    "DROP INDEX IF EXISTS idx_aa_sponsorship_request_key"
  void $ execute_ conn
    "CREATE UNIQUE INDEX IF NOT EXISTS idx_aa_sponsorship_active_request_key \
    \ON aa_sponsorship_authorizations(request_key) \
    \WHERE state IN ('reserved','signed','submitted')"
  void $ execute_ conn
    "CREATE INDEX IF NOT EXISTS idx_aa_sponsorship_sender_state \
    \ON aa_sponsorship_authorizations(sender, state)"
  void $ execute_ conn
    "CREATE INDEX IF NOT EXISTS idx_aa_sponsorship_client_state \
    \ON aa_sponsorship_authorizations(client_key, state)"
  void $ execute_ conn
    "ALTER TABLE aa_sponsorship_authorizations \
    \ALTER COLUMN expected_user_operation_hash DROP NOT NULL"
  void $ execute_ conn
    "CREATE INDEX IF NOT EXISTS idx_aa_sponsorship_submitted \
    \ON aa_sponsorship_authorizations(state, submitted_at)"
  void $ execute_ conn
    "CREATE TABLE IF NOT EXISTS aa_sponsorship_ledger (\
    \id BIGSERIAL PRIMARY KEY,\
    \digest VARCHAR(66) NOT NULL REFERENCES aa_sponsorship_authorizations(digest),\
    \entry_type VARCHAR(16) NOT NULL,\
    \amount_wei NUMERIC(78,0) NOT NULL,\
    \created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),\
    \UNIQUE (digest, entry_type),\
    \CHECK (entry_type IN ('reserve','release','actual_charge')),\
    \CHECK (amount_wei >= 0)\
    \)"
  void $ execute_ conn
    "CREATE TABLE IF NOT EXISTS aa_user_operation_events (\
    \user_operation_hash VARCHAR(66) PRIMARY KEY,\
    \digest VARCHAR(66) NOT NULL REFERENCES aa_sponsorship_authorizations(digest),\
    \transaction_hash VARCHAR(66) NOT NULL,\
    \block_number BIGINT NOT NULL,\
    \block_hash VARCHAR(66) NOT NULL,\
    \success BOOLEAN NOT NULL,\
    \actual_gas_cost_wei NUMERIC(78,0) NOT NULL,\
    \event_json JSONB NOT NULL,\
    \observed_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),\
    \finalized_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),\
    \CHECK (actual_gas_cost_wei >= 0),\
    \CHECK (block_number >= 0)\
    \)"
  void $ execute_ conn
    "CREATE INDEX IF NOT EXISTS idx_aa_user_operation_events_finalized_at \
    \ON aa_user_operation_events(finalized_at)"
  void $ execute_ conn
    "CREATE INDEX IF NOT EXISTS idx_aa_user_operation_events_digest \
    \ON aa_user_operation_events(digest)"
  void $ execute_ conn
    "CREATE TABLE IF NOT EXISTS aa_reconciler_cursor (\
    \chain_id BIGINT NOT NULL,\
    \paymaster VARCHAR(42) NOT NULL,\
    \safe_block BIGINT NOT NULL,\
    \safe_block_hash VARCHAR(66) NOT NULL,\
    \updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),\
    \PRIMARY KEY (chain_id, paymaster),\
    \CHECK (safe_block >= 0)\
    \)"
  void $ execute_ conn
    "CREATE TABLE IF NOT EXISTS aa_reconciler_health (\
    \chain_id BIGINT NOT NULL,\
    \paymaster VARCHAR(42) NOT NULL,\
    \safe_block BIGINT NOT NULL,\
    \safe_block_hash VARCHAR(66) NOT NULL,\
    \last_success_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),\
    \PRIMARY KEY (chain_id,paymaster),\
    \CHECK (safe_block >= 0)\
    \)"
  void $ execute_ conn
    "CREATE TABLE IF NOT EXISTS aa_recovery_operations (\
    \user_operation_hash VARCHAR(66) PRIMARY KEY,\
    \client_key VARCHAR(66) NOT NULL,\
    \provider VARCHAR(16) NOT NULL,\
    \created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),\
    \expires_at TIMESTAMPTZ NOT NULL,\
    \CHECK (provider IN ('pimlico','alto'))\
    \)"
  void $ execute_ conn
    "CREATE INDEX IF NOT EXISTS idx_aa_recovery_expiry \
    \ON aa_recovery_operations(expires_at)"
  void $ execute_ conn
    "CREATE TABLE IF NOT EXISTS aa_rate_windows (\
    \scope VARCHAR(24) NOT NULL,\
    \client_key VARCHAR(66) NOT NULL,\
    \account_key VARCHAR(66) NOT NULL,\
    \window_start TIMESTAMPTZ NOT NULL,\
    \request_count INTEGER NOT NULL,\
    \updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),\
    \PRIMARY KEY (scope,client_key,account_key,window_start),\
    \CHECK (client_key ~ '^0x[0-9a-f]{64}$'),\
    \CHECK (account_key ~ '^0x[0-9a-f]{64}$'),\
    \CHECK (request_count > 0)\
    \)"
  void $ execute_ conn
    "CREATE INDEX IF NOT EXISTS idx_aa_rate_window_expiry ON aa_rate_windows(window_start)"
  void $ execute_ conn
    "CREATE TABLE IF NOT EXISTS aa_sponsorship_control (\
    \singleton BOOLEAN PRIMARY KEY DEFAULT TRUE CHECK (singleton),\
    \issuance_paused BOOLEAN NOT NULL DEFAULT TRUE,\
    \paused_reason TEXT,\
    \updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),\
    \CHECK (issuance_paused OR paused_reason IS NULL)\
    \)"
  void $ execute_ conn
    "CREATE TABLE IF NOT EXISTS aa_sponsorship_control_events (\
    \id BIGSERIAL PRIMARY KEY,\
    \action VARCHAR(16) NOT NULL,\
    \reason TEXT NOT NULL,\
    \operator_note TEXT,\
    \created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),\
    \CHECK (action IN ('pause','resume')),\
    \CHECK (length(reason) BETWEEN 1 AND 512),\
    \CHECK (operator_note IS NULL OR length(operator_note) BETWEEN 1 AND 512)\
    \)"
  void $ execute_ conn
    "ALTER TABLE aa_sponsorship_control ALTER COLUMN issuance_paused SET DEFAULT TRUE"
  repaired <- execute conn
    "UPDATE aa_sponsorship_control SET issuance_paused=TRUE,paused_reason=?,updated_at=clock_timestamp() \
    \WHERE singleton=TRUE AND issuance_paused \
    \AND (paused_reason IS NULL OR length(btrim(paused_reason))=0)"
    (Only controlBootstrapReason)
  when (repaired == (1 :: Int64)) $
    void $ execute conn
      "INSERT INTO aa_sponsorship_control_events (action,reason,operator_note) \
      \VALUES ('pause',?,'automatic repair of an invalid paused control row')"
      (Only controlBootstrapReason)
  inserted <- query conn
    "INSERT INTO aa_sponsorship_control \
    \(singleton,issuance_paused,paused_reason) VALUES (TRUE,TRUE,?) \
    \ON CONFLICT DO NOTHING RETURNING singleton"
    (Only controlBootstrapReason) :: IO [Only Bool]
  unless (null inserted) $
    void $ execute conn
      "INSERT INTO aa_sponsorship_control_events (action,reason,operator_note) \
      \VALUES ('pause',?,'automatic fail-closed control row bootstrap')"
      (Only controlBootstrapReason)
  void $ execute_ conn
    "ALTER TABLE aa_sponsorship_control \
    \DROP CONSTRAINT IF EXISTS aa_sponsorship_control_reason_consistent, \
    \ADD CONSTRAINT aa_sponsorship_control_reason_consistent CHECK \
    \((issuance_paused AND paused_reason IS NOT NULL AND length(btrim(paused_reason)) BETWEEN 1 AND 512) \
    \OR (NOT issuance_paused AND paused_reason IS NULL))"
  mapM_ (void . execute_ conn) aaInvariantConstraintMigrations
  verifyAaSponsorshipSchema conn

-- These named, validated constraints form part of the runtime schema
-- fingerprint.  The older anonymous checks are deliberately left in place;
-- adding the composite constraints makes an existing development schema at
-- least as strict as a fresh schema without repeatedly dropping/revalidating
-- checks on every process start.
aaInvariantConstraintMigrations :: [Query]
aaInvariantConstraintMigrations =
  [ "DO $aa_schema$ BEGIN \
    \IF NOT EXISTS (SELECT 1 FROM pg_constraint WHERE conrelid='aa_sponsorship_authorizations'::regclass AND conname='aa_authorization_invariants_ck') THEN \
    \ALTER TABLE aa_sponsorship_authorizations ADD CONSTRAINT aa_authorization_invariants_ck CHECK (\
    \request_key ~ '^0x[0-9a-f]{64}$' AND digest ~ '^0x[0-9a-f]{64}$' \
    \AND (expected_user_operation_hash IS NULL OR expected_user_operation_hash ~ '^0x[0-9a-f]{64}$') \
    \AND (user_operation_hash IS NULL OR user_operation_hash = expected_user_operation_hash) \
    \AND sender ~ '^0x[0-9a-f]{40}$' AND owner ~ '^0x[0-9a-f]{40}$' \
    \AND nonce >= 0 AND max_cost_wei > 0 AND valid_after >= 0 AND valid_until > valid_after \
    \AND (signature IS NULL OR signature ~ '^0x[0-9a-f]{130}$') \
    \AND state IN ('reserved','signed','submitted','settled','expired','cancelled') \
    \AND ((state = 'reserved' AND signature IS NULL) OR state <> 'reserved')); \
    \END IF; END $aa_schema$"
  , "DO $aa_schema$ BEGIN \
    \IF NOT EXISTS (SELECT 1 FROM pg_constraint WHERE conrelid='aa_sponsorship_ledger'::regclass AND conname='aa_ledger_invariants_ck') THEN \
    \ALTER TABLE aa_sponsorship_ledger ADD CONSTRAINT aa_ledger_invariants_ck CHECK (\
    \entry_type IN ('reserve','release','actual_charge') AND amount_wei >= 0); \
    \END IF; END $aa_schema$"
  , "DO $aa_schema$ BEGIN \
    \IF NOT EXISTS (SELECT 1 FROM pg_constraint WHERE conrelid='aa_user_operation_events'::regclass AND conname='aa_event_invariants_ck') THEN \
    \ALTER TABLE aa_user_operation_events ADD CONSTRAINT aa_event_invariants_ck CHECK (\
    \actual_gas_cost_wei >= 0 AND block_number >= 0); \
    \END IF; END $aa_schema$"
  , "DO $aa_schema$ BEGIN \
    \IF NOT EXISTS (SELECT 1 FROM pg_constraint WHERE conrelid='aa_reconciler_cursor'::regclass AND conname='aa_cursor_invariants_ck') THEN \
    \ALTER TABLE aa_reconciler_cursor ADD CONSTRAINT aa_cursor_invariants_ck CHECK (safe_block >= 0); \
    \END IF; END $aa_schema$"
  , "DO $aa_schema$ BEGIN \
    \IF NOT EXISTS (SELECT 1 FROM pg_constraint WHERE conrelid='aa_reconciler_health'::regclass AND conname='aa_health_invariants_ck') THEN \
    \ALTER TABLE aa_reconciler_health ADD CONSTRAINT aa_health_invariants_ck CHECK (safe_block >= 0); \
    \END IF; END $aa_schema$"
  , "DO $aa_schema$ BEGIN \
    \IF NOT EXISTS (SELECT 1 FROM pg_constraint WHERE conrelid='aa_recovery_operations'::regclass AND conname='aa_recovery_invariants_ck') THEN \
    \ALTER TABLE aa_recovery_operations ADD CONSTRAINT aa_recovery_invariants_ck CHECK (\
    \provider IN ('pimlico','alto') AND expires_at > created_at); \
    \END IF; END $aa_schema$"
  , "DO $aa_schema$ BEGIN \
    \IF NOT EXISTS (SELECT 1 FROM pg_constraint WHERE conrelid='aa_rate_windows'::regclass AND conname='aa_rate_invariants_ck') THEN \
    \ALTER TABLE aa_rate_windows ADD CONSTRAINT aa_rate_invariants_ck CHECK (\
    \client_key ~ '^0x[0-9a-f]{64}$' AND account_key ~ '^0x[0-9a-f]{64}$' AND request_count > 0); \
    \END IF; END $aa_schema$"
  , "DO $aa_schema$ BEGIN \
    \IF NOT EXISTS (SELECT 1 FROM pg_constraint WHERE conrelid='aa_sponsorship_control'::regclass AND conname='aa_control_singleton_ck') THEN \
    \ALTER TABLE aa_sponsorship_control ADD CONSTRAINT aa_control_singleton_ck CHECK (singleton); \
    \END IF; END $aa_schema$"
  , "DO $aa_schema$ BEGIN \
    \IF NOT EXISTS (SELECT 1 FROM pg_constraint WHERE conrelid='aa_sponsorship_control_events'::regclass AND conname='aa_control_event_invariants_ck') THEN \
    \ALTER TABLE aa_sponsorship_control_events ADD CONSTRAINT aa_control_event_invariants_ck CHECK (\
    \action IN ('pause','resume') AND length(reason) BETWEEN 1 AND 512 \
    \AND (operator_note IS NULL OR length(operator_note) BETWEEN 1 AND 512)); \
    \END IF; END $aa_schema$"
  ]

-- Fail closed if an existing same-named table is weaker or structurally
-- different. CREATE TABLE/INDEX IF NOT EXISTS alone is not a migration
-- guarantee: PostgreSQL deliberately accepts a pre-existing object without
-- comparing its definition.
verifyAaSponsorshipSchema :: Connection -> IO ()
verifyAaSponsorshipSchema conn = do
  tables <- query_ conn
    "SELECT c.relname::TEXT,c.relpersistence::TEXT,c.relrowsecurity,c.relforcerowsecurity \
    \FROM pg_class c JOIN pg_namespace n ON n.oid=c.relnamespace \
    \WHERE n.nspname=current_schema() AND c.relname IN ('aa_sponsorship_authorizations',\
    \'aa_sponsorship_ledger','aa_user_operation_events','aa_reconciler_cursor',\
    \'aa_reconciler_health','aa_recovery_operations','aa_rate_windows',\
    \'aa_sponsorship_control','aa_sponsorship_control_events')" :: IO [(Text, Text, Bool, Bool)]
  unless (sort tables == sort expectedAaSchemaTables) $
    fail "native AA database table durability/row-security fingerprint mismatch"

  columns <- query_ conn aaSchemaColumnsQuery :: IO [(Text, Text, Text, Bool)]
  unless (sort columns == sort expectedAaSchemaColumns) $
    fail "native AA database schema column fingerprint mismatch"

  supportsNullsNotDistinctRows <- query_ conn aaSchemaNullsNotDistinctSupportQuery :: IO [Only Bool]
  supportsNullsNotDistinct <-
    case supportsNullsNotDistinctRows of
      [Only supported] -> pure supported
      _ -> fail "native AA database catalog capability fingerprint mismatch"
  keys <- query_ conn (aaSchemaKeysQuery supportsNullsNotDistinct) :: IO [AaSchemaKey]
  unless (sort keys == sort expectedAaSchemaKeys) $
    fail "native AA database schema key/foreign-key fingerprint mismatch"

  createExpectedAaConstraintFingerprint conn
  checks <- query_ conn aaSchemaChecksQuery :: IO [(Text, Text, Bool, Bool)]
  unless (sort checks == sort expectedAaSchemaChecks) $
    fail "native AA database schema check-constraint fingerprint mismatch"

  indexes <- query_ conn aaSchemaIndexesQuery :: IO [AaSchemaIndex]
  unless (sort indexes == sort expectedAaSchemaIndexes) $
    fail "native AA database schema index fingerprint mismatch"

  predicates <- query_ conn
    "SELECT pg_get_expr(actual.indpred,actual.indrelid,true) \
    \= pg_get_expr(expected.indpred,expected.indrelid,true) \
    \FROM pg_index actual JOIN pg_class actual_class ON actual_class.oid=actual.indexrelid \
    \CROSS JOIN pg_index expected JOIN pg_class expected_class ON expected_class.oid=expected.indexrelid \
    \WHERE actual_class.relname='idx_aa_sponsorship_active_request_key' \
    \AND expected_class.relname='aa_expected_active_request_key'" :: IO [Only Bool]
  unless (predicates == [Only True]) $
    fail "native AA active request-key index predicate mismatch"

  defaults <- query_ conn
    "SELECT table_name::TEXT,column_name::TEXT,lower(column_default)::TEXT \
    \FROM information_schema.columns WHERE table_schema=current_schema() \
    \AND table_name IN ('aa_sponsorship_authorizations','aa_sponsorship_ledger',\
    \'aa_user_operation_events','aa_reconciler_cursor','aa_reconciler_health',\
    \'aa_recovery_operations','aa_rate_windows','aa_sponsorship_control',\
    \'aa_sponsorship_control_events') AND column_default IS NOT NULL" :: IO [(Text, Text, Text)]
  unless (sort defaults == sort expectedAaSchemaDefaults) $
    fail "native AA database schema default fingerprint mismatch"
  void $ execute_ conn "DROP TABLE pg_temp.aa_expected_check_fingerprint"

-- PostgreSQL itself parses an expected copy of every security-critical CHECK
-- and the active request-key predicate.  Comparing pg_get_constraintdef /
-- pg_get_expr against that temporary object avoids version-sensitive hand
-- normalization while still rejecting a weaker same-named object.
createExpectedAaConstraintFingerprint :: Connection -> IO ()
createExpectedAaConstraintFingerprint conn = do
  void $ execute_ conn "DROP TABLE IF EXISTS pg_temp.aa_expected_check_fingerprint"
  void $ execute_ conn
    "CREATE TEMP TABLE aa_expected_check_fingerprint (\
    \request_key VARCHAR(66),digest VARCHAR(66),expected_user_operation_hash VARCHAR(66),\
    \user_operation_hash VARCHAR(66),sender VARCHAR(42),owner VARCHAR(42),nonce NUMERIC(78,0),\
    \valid_after BIGINT,valid_until BIGINT,max_cost_wei NUMERIC(78,0),signature VARCHAR(132),\
    \state VARCHAR(16),entry_type VARCHAR(16),amount_wei NUMERIC(78,0),\
    \actual_gas_cost_wei NUMERIC(78,0),block_number BIGINT,safe_block BIGINT,\
    \provider VARCHAR(16),created_at TIMESTAMPTZ,expires_at TIMESTAMPTZ,\
    \client_key VARCHAR(66),account_key VARCHAR(66),request_count INTEGER,\
    \submitted_at TIMESTAMPTZ,finalized_at TIMESTAMPTZ,window_start TIMESTAMPTZ,singleton BOOLEAN,\
    \issuance_paused BOOLEAN,paused_reason TEXT,action VARCHAR(16),reason TEXT,operator_note TEXT,\
    \CONSTRAINT aa_authorization_invariants_ck CHECK (\
    \request_key ~ '^0x[0-9a-f]{64}$' AND digest ~ '^0x[0-9a-f]{64}$' \
    \AND (expected_user_operation_hash IS NULL OR expected_user_operation_hash ~ '^0x[0-9a-f]{64}$') \
    \AND (user_operation_hash IS NULL OR user_operation_hash = expected_user_operation_hash) \
    \AND sender ~ '^0x[0-9a-f]{40}$' AND owner ~ '^0x[0-9a-f]{40}$' \
    \AND nonce >= 0 AND max_cost_wei > 0 AND valid_after >= 0 AND valid_until > valid_after \
    \AND (signature IS NULL OR signature ~ '^0x[0-9a-f]{130}$') \
    \AND state IN ('reserved','signed','submitted','settled','expired','cancelled') \
    \AND ((state = 'reserved' AND signature IS NULL) OR state <> 'reserved')),\
    \CONSTRAINT aa_ledger_invariants_ck CHECK (entry_type IN ('reserve','release','actual_charge') AND amount_wei >= 0),\
    \CONSTRAINT aa_event_invariants_ck CHECK (actual_gas_cost_wei >= 0 AND block_number >= 0),\
    \CONSTRAINT aa_cursor_invariants_ck CHECK (safe_block >= 0),\
    \CONSTRAINT aa_health_invariants_ck CHECK (safe_block >= 0),\
    \CONSTRAINT aa_recovery_invariants_ck CHECK (provider IN ('pimlico','alto') AND expires_at > created_at),\
    \CONSTRAINT aa_rate_invariants_ck CHECK (client_key ~ '^0x[0-9a-f]{64}$' AND account_key ~ '^0x[0-9a-f]{64}$' AND request_count > 0),\
    \CONSTRAINT aa_control_singleton_ck CHECK (singleton),\
    \CONSTRAINT aa_sponsorship_control_reason_consistent CHECK (\
    \(issuance_paused AND paused_reason IS NOT NULL AND length(btrim(paused_reason)) BETWEEN 1 AND 512) \
    \OR (NOT issuance_paused AND paused_reason IS NULL)),\
    \CONSTRAINT aa_control_event_invariants_ck CHECK (action IN ('pause','resume') \
    \AND length(reason) BETWEEN 1 AND 512 \
    \AND (operator_note IS NULL OR length(operator_note) BETWEEN 1 AND 512)))"
  void $ execute_ conn
    "CREATE UNIQUE INDEX aa_expected_active_request_key \
    \ON aa_expected_check_fingerprint(request_key) \
    \WHERE state IN ('reserved','signed','submitted')"
  mapM_ (void . execute_ conn)
    [ "CREATE INDEX aa_expected_sender_state ON aa_expected_check_fingerprint(sender,state)"
    , "CREATE INDEX aa_expected_client_state ON aa_expected_check_fingerprint(client_key,state)"
    , "CREATE INDEX aa_expected_submitted ON aa_expected_check_fingerprint(state,submitted_at)"
    , "CREATE INDEX aa_expected_finalized_at ON aa_expected_check_fingerprint(finalized_at)"
    , "CREATE INDEX aa_expected_digest ON aa_expected_check_fingerprint(digest)"
    , "CREATE INDEX aa_expected_recovery_expiry ON aa_expected_check_fingerprint(expires_at)"
    , "CREATE INDEX aa_expected_rate_expiry ON aa_expected_check_fingerprint(window_start)"
    ]

aaSchemaColumnsQuery :: Query
aaSchemaColumnsQuery =
  "SELECT c.relname::TEXT,a.attname::TEXT,format_type(a.atttypid,a.atttypmod)::TEXT,a.attnotnull \
  \FROM pg_class c JOIN pg_namespace n ON n.oid=c.relnamespace \
  \JOIN pg_attribute a ON a.attrelid=c.oid \
  \WHERE n.nspname=current_schema() AND c.relkind='r' AND a.attnum>0 AND NOT a.attisdropped \
  \AND c.relname IN ('aa_sponsorship_authorizations','aa_sponsorship_ledger',\
  \'aa_user_operation_events','aa_reconciler_cursor','aa_reconciler_health',\
  \'aa_recovery_operations','aa_rate_windows','aa_sponsorship_control',\
  \'aa_sponsorship_control_events')"

-- PostgreSQL 15 added pg_index.indnullsnotdistinct.  Schema initialization is
-- also used by supported PostgreSQL 14 deployments, so detect the catalog
-- capability using columns that exist on both versions and only parse the
-- PG15+ query when the column is present.  On PG14 UNIQUE necessarily has the
-- historical NULLS DISTINCT behavior, represented by the literal FALSE.
aaSchemaNullsNotDistinctSupportQuery :: Query
aaSchemaNullsNotDistinctSupportQuery =
  "SELECT EXISTS (SELECT 1 FROM pg_catalog.pg_attribute \
  \WHERE attrelid='pg_catalog.pg_index'::regclass \
  \AND attname='indnullsnotdistinct' AND NOT attisdropped)"

aaSchemaKeysQuery :: Bool -> Query
aaSchemaKeysQuery supportsNullsNotDistinct
  | supportsNullsNotDistinct = aaSchemaKeysQueryPg15
  | otherwise = aaSchemaKeysQueryPg14

aaSchemaKeysQueryPg15 :: Query
aaSchemaKeysQueryPg15 =
  "SELECT source.relname::TEXT,con.contype::TEXT,\
  \array_to_string(ARRAY(SELECT attribute.attname::TEXT \
  \FROM unnest(con.conkey) WITH ORDINALITY key_row(attnum,position) \
  \JOIN pg_attribute attribute ON attribute.attrelid=con.conrelid AND attribute.attnum=key_row.attnum \
  \ORDER BY key_row.position),',')::TEXT,\
  \COALESCE(target.relname::TEXT,''),\
  \CASE WHEN con.contype='f' THEN array_to_string(ARRAY(SELECT attribute.attname::TEXT \
  \FROM unnest(con.confkey) WITH ORDINALITY key_row(attnum,position) \
  \JOIN pg_attribute attribute ON attribute.attrelid=con.confrelid AND attribute.attnum=key_row.attnum \
  \ORDER BY key_row.position),',')::TEXT ELSE '' END,\
  \CASE WHEN con.contype='f' THEN con.confupdtype::TEXT ELSE '' END,\
  \CASE WHEN con.contype='f' THEN con.confdeltype::TEXT ELSE '' END,\
  \CASE WHEN con.contype='f' THEN con.confmatchtype::TEXT ELSE '' END,\
  \con.condeferrable,con.condeferred,con.convalidated,\
  \CASE WHEN con.contype IN ('p','u') \
  \THEN key_index.indisunique AND key_index.indisvalid AND key_index.indisready ELSE TRUE END,\
  \CASE WHEN con.contype='f' THEN target.relnamespace=source.relnamespace ELSE TRUE END,\
  \CASE WHEN con.contype IN ('p','u') THEN key_index.indnullsnotdistinct ELSE FALSE END \
  \FROM pg_constraint con JOIN pg_class source ON source.oid=con.conrelid \
  \JOIN pg_namespace ns ON ns.oid=source.relnamespace \
  \LEFT JOIN pg_class target ON target.oid=con.confrelid \
  \LEFT JOIN pg_index key_index ON key_index.indexrelid=con.conindid \
  \WHERE ns.nspname=current_schema() AND con.contype IN ('p','u','f') \
  \AND source.relname IN ('aa_sponsorship_authorizations','aa_sponsorship_ledger',\
  \'aa_user_operation_events','aa_reconciler_cursor','aa_reconciler_health',\
  \'aa_recovery_operations','aa_rate_windows','aa_sponsorship_control',\
  \'aa_sponsorship_control_events')"

aaSchemaKeysQueryPg14 :: Query
aaSchemaKeysQueryPg14 =
  "SELECT source.relname::TEXT,con.contype::TEXT,\
  \array_to_string(ARRAY(SELECT attribute.attname::TEXT \
  \FROM unnest(con.conkey) WITH ORDINALITY key_row(attnum,position) \
  \JOIN pg_attribute attribute ON attribute.attrelid=con.conrelid AND attribute.attnum=key_row.attnum \
  \ORDER BY key_row.position),',')::TEXT,\
  \COALESCE(target.relname::TEXT,''),\
  \CASE WHEN con.contype='f' THEN array_to_string(ARRAY(SELECT attribute.attname::TEXT \
  \FROM unnest(con.confkey) WITH ORDINALITY key_row(attnum,position) \
  \JOIN pg_attribute attribute ON attribute.attrelid=con.confrelid AND attribute.attnum=key_row.attnum \
  \ORDER BY key_row.position),',')::TEXT ELSE '' END,\
  \CASE WHEN con.contype='f' THEN con.confupdtype::TEXT ELSE '' END,\
  \CASE WHEN con.contype='f' THEN con.confdeltype::TEXT ELSE '' END,\
  \CASE WHEN con.contype='f' THEN con.confmatchtype::TEXT ELSE '' END,\
  \con.condeferrable,con.condeferred,con.convalidated,\
  \CASE WHEN con.contype IN ('p','u') \
  \THEN key_index.indisunique AND key_index.indisvalid AND key_index.indisready ELSE TRUE END,\
  \CASE WHEN con.contype='f' THEN target.relnamespace=source.relnamespace ELSE TRUE END,\
  \FALSE \
  \FROM pg_constraint con JOIN pg_class source ON source.oid=con.conrelid \
  \JOIN pg_namespace ns ON ns.oid=source.relnamespace \
  \LEFT JOIN pg_class target ON target.oid=con.confrelid \
  \LEFT JOIN pg_index key_index ON key_index.indexrelid=con.conindid \
  \WHERE ns.nspname=current_schema() AND con.contype IN ('p','u','f') \
  \AND source.relname IN ('aa_sponsorship_authorizations','aa_sponsorship_ledger',\
  \'aa_user_operation_events','aa_reconciler_cursor','aa_reconciler_health',\
  \'aa_recovery_operations','aa_rate_windows','aa_sponsorship_control',\
  \'aa_sponsorship_control_events')"

aaSchemaChecksQuery :: Query
aaSchemaChecksQuery =
  "SELECT table_class.relname::TEXT,con.conname::TEXT,con.convalidated,\
  \pg_get_constraintdef(con.oid,true)=pg_get_constraintdef(expected.oid,true) \
  \FROM pg_constraint con JOIN pg_class table_class ON table_class.oid=con.conrelid \
  \JOIN pg_namespace ns ON ns.oid=table_class.relnamespace \
  \JOIN pg_constraint expected ON expected.conname=con.conname \
  \JOIN pg_class expected_table ON expected_table.oid=expected.conrelid \
  \JOIN pg_namespace expected_ns ON expected_ns.oid=expected_table.relnamespace \
  \WHERE ns.nspname=current_schema() AND con.contype='c' \
  \AND expected_ns.nspname LIKE 'pg_temp_%' \
  \AND expected_table.relname='aa_expected_check_fingerprint' \
  \AND con.conname IN ('aa_authorization_invariants_ck','aa_ledger_invariants_ck',\
  \'aa_event_invariants_ck','aa_cursor_invariants_ck','aa_health_invariants_ck',\
  \'aa_recovery_invariants_ck','aa_rate_invariants_ck','aa_control_singleton_ck',\
  \'aa_sponsorship_control_reason_consistent','aa_control_event_invariants_ck')"

aaSchemaIndexesQuery :: Query
aaSchemaIndexesQuery =
  "SELECT index_class.relname::TEXT,table_class.relname::TEXT,idx.indisunique,\
  \idx.indisvalid,idx.indisready,\
  \array_to_string(ARRAY(SELECT attribute.attname::TEXT \
  \FROM unnest(idx.indkey) WITH ORDINALITY key_row(attnum,position) \
  \JOIN pg_attribute attribute ON attribute.attrelid=idx.indrelid AND attribute.attnum=key_row.attnum \
  \ORDER BY key_row.position),',')::TEXT,idx.indpred IS NOT NULL,\
  \idx.indnkeyatts::INTEGER,idx.indnatts::INTEGER,idx.indexprs IS NULL,\
  \idx.indclass=expected.indclass AND idx.indcollation=expected.indcollation \
  \AND idx.indoption=expected.indoption AND index_class.relam=expected_class.relam \
  \FROM pg_index idx JOIN pg_class index_class ON index_class.oid=idx.indexrelid \
  \JOIN pg_class table_class ON table_class.oid=idx.indrelid \
  \JOIN pg_namespace ns ON ns.oid=table_class.relnamespace \
  \JOIN pg_class expected_class ON expected_class.relname=CASE index_class.relname \
  \WHEN 'idx_aa_sponsorship_active_request_key' THEN 'aa_expected_active_request_key' \
  \WHEN 'idx_aa_sponsorship_sender_state' THEN 'aa_expected_sender_state' \
  \WHEN 'idx_aa_sponsorship_client_state' THEN 'aa_expected_client_state' \
  \WHEN 'idx_aa_sponsorship_submitted' THEN 'aa_expected_submitted' \
  \WHEN 'idx_aa_user_operation_events_finalized_at' THEN 'aa_expected_finalized_at' \
  \WHEN 'idx_aa_user_operation_events_digest' THEN 'aa_expected_digest' \
  \WHEN 'idx_aa_recovery_expiry' THEN 'aa_expected_recovery_expiry' \
  \WHEN 'idx_aa_rate_window_expiry' THEN 'aa_expected_rate_expiry' END \
  \JOIN pg_namespace expected_ns ON expected_ns.oid=expected_class.relnamespace \
  \JOIN pg_index expected ON expected.indexrelid=expected_class.oid \
  \WHERE ns.nspname=current_schema() AND index_class.relname IN (\
  \'idx_aa_sponsorship_active_request_key','idx_aa_sponsorship_sender_state',\
  \'idx_aa_sponsorship_client_state','idx_aa_sponsorship_submitted',\
  \'idx_aa_user_operation_events_finalized_at','idx_aa_user_operation_events_digest',\
  \'idx_aa_recovery_expiry','idx_aa_rate_window_expiry') \
  \AND expected_ns.nspname LIKE 'pg_temp_%'"

expectedAaSchemaColumns :: [(Text, Text, Text, Bool)]
expectedAaSchemaColumns =
  concat
    [ columns "aa_sponsorship_authorizations"
        [ ("request_key", "character varying(66)", True), ("digest", "character varying(66)", True)
        , ("expected_user_operation_hash", "character varying(66)", False), ("user_operation_hash", "character varying(66)", False)
        , ("sender", "character varying(42)", True), ("owner", "character varying(42)", True)
        , ("nonce", "numeric(78,0)", True), ("valid_after", "bigint", True), ("valid_until", "bigint", True)
        , ("max_cost_wei", "numeric(78,0)", True), ("client_key", "character varying(66)", True)
        , ("operation", "jsonb", True), ("signature", "character varying(132)", False), ("state", "character varying(16)", True)
        , ("created_at", "timestamp with time zone", True), ("signed_at", "timestamp with time zone", False)
        , ("submitted_at", "timestamp with time zone", False), ("settled_at", "timestamp with time zone", False)
        , ("updated_at", "timestamp with time zone", True)
        ]
    , columns "aa_sponsorship_ledger"
        [("id", "bigint", True), ("digest", "character varying(66)", True), ("entry_type", "character varying(16)", True), ("amount_wei", "numeric(78,0)", True), ("created_at", "timestamp with time zone", True)]
    , columns "aa_user_operation_events"
        [("user_operation_hash", "character varying(66)", True), ("digest", "character varying(66)", True), ("transaction_hash", "character varying(66)", True), ("block_number", "bigint", True), ("block_hash", "character varying(66)", True), ("success", "boolean", True), ("actual_gas_cost_wei", "numeric(78,0)", True), ("event_json", "jsonb", True), ("observed_at", "timestamp with time zone", True), ("finalized_at", "timestamp with time zone", True)]
    , columns "aa_reconciler_cursor"
        [("chain_id", "bigint", True), ("paymaster", "character varying(42)", True), ("safe_block", "bigint", True), ("safe_block_hash", "character varying(66)", True), ("updated_at", "timestamp with time zone", True)]
    , columns "aa_reconciler_health"
        [("chain_id", "bigint", True), ("paymaster", "character varying(42)", True), ("safe_block", "bigint", True), ("safe_block_hash", "character varying(66)", True), ("last_success_at", "timestamp with time zone", True)]
    , columns "aa_recovery_operations"
        [("user_operation_hash", "character varying(66)", True), ("client_key", "character varying(66)", True), ("provider", "character varying(16)", True), ("created_at", "timestamp with time zone", True), ("expires_at", "timestamp with time zone", True)]
    , columns "aa_rate_windows"
        [("scope", "character varying(24)", True), ("client_key", "character varying(66)", True), ("account_key", "character varying(66)", True), ("window_start", "timestamp with time zone", True), ("request_count", "integer", True), ("updated_at", "timestamp with time zone", True)]
    , columns "aa_sponsorship_control"
        [("singleton", "boolean", True), ("issuance_paused", "boolean", True), ("paused_reason", "text", False), ("updated_at", "timestamp with time zone", True)]
    , columns "aa_sponsorship_control_events"
        [("id", "bigint", True), ("action", "character varying(16)", True), ("reason", "text", True), ("operator_note", "text", False), ("created_at", "timestamp with time zone", True)]
    ]
 where
  columns tableName = map (\(name, columnType, required) -> (tableName, name, columnType, required))

expectedAaSchemaTables :: [(Text, Text, Bool, Bool)]
expectedAaSchemaTables =
  [ (tableName, "p", False, False)
  | tableName <-
      [ "aa_sponsorship_authorizations"
      , "aa_sponsorship_ledger"
      , "aa_user_operation_events"
      , "aa_reconciler_cursor"
      , "aa_reconciler_health"
      , "aa_recovery_operations"
      , "aa_rate_windows"
      , "aa_sponsorship_control"
      , "aa_sponsorship_control_events"
      ]
  ]

expectedAaSchemaDefaults :: [(Text, Text, Text)]
expectedAaSchemaDefaults =
  [ ("aa_sponsorship_authorizations", "created_at", "now()")
  , ("aa_sponsorship_authorizations", "updated_at", "now()")
  , ("aa_sponsorship_ledger", "id", "nextval('aa_sponsorship_ledger_id_seq'::regclass)")
  , ("aa_sponsorship_ledger", "created_at", "now()")
  , ("aa_user_operation_events", "observed_at", "now()")
  , ("aa_user_operation_events", "finalized_at", "now()")
  , ("aa_reconciler_cursor", "updated_at", "now()")
  , ("aa_reconciler_health", "last_success_at", "now()")
  , ("aa_recovery_operations", "created_at", "now()")
  , ("aa_rate_windows", "updated_at", "now()")
  , ("aa_sponsorship_control", "singleton", "true")
  , ("aa_sponsorship_control", "issuance_paused", "true")
  , ("aa_sponsorship_control", "updated_at", "now()")
  , ("aa_sponsorship_control_events", "id", "nextval('aa_sponsorship_control_events_id_seq'::regclass)")
  , ("aa_sponsorship_control_events", "created_at", "now()")
  ]

expectedAaSchemaKeys :: [AaSchemaKey]
expectedAaSchemaKeys =
  [ localKey "aa_sponsorship_authorizations" "p" "digest"
  , localKey "aa_sponsorship_authorizations" "u" "expected_user_operation_hash"
  , localKey "aa_sponsorship_authorizations" "u" "user_operation_hash"
  , localKey "aa_sponsorship_ledger" "p" "id"
  , localKey "aa_sponsorship_ledger" "u" "digest,entry_type"
  , foreignKey "aa_sponsorship_ledger" "digest" "aa_sponsorship_authorizations" "digest"
  , localKey "aa_user_operation_events" "p" "user_operation_hash"
  , foreignKey "aa_user_operation_events" "digest" "aa_sponsorship_authorizations" "digest"
  , localKey "aa_reconciler_cursor" "p" "chain_id,paymaster"
  , localKey "aa_reconciler_health" "p" "chain_id,paymaster"
  , localKey "aa_recovery_operations" "p" "user_operation_hash"
  , localKey "aa_rate_windows" "p" "scope,client_key,account_key,window_start"
  , localKey "aa_sponsorship_control" "p" "singleton"
  , localKey "aa_sponsorship_control_events" "p" "id"
  ]
 where
  localKey tableName keyType keyColumns =
    AaSchemaKey tableName keyType keyColumns "" "" "" "" "" False False True True True False
  foreignKey tableName keyColumns targetTable targetColumns =
    AaSchemaKey tableName "f" keyColumns targetTable targetColumns "a" "a" "s" False False True True True False

expectedAaSchemaChecks :: [(Text, Text, Bool, Bool)]
expectedAaSchemaChecks =
  [ ("aa_sponsorship_authorizations", "aa_authorization_invariants_ck", True, True)
  , ("aa_sponsorship_ledger", "aa_ledger_invariants_ck", True, True)
  , ("aa_user_operation_events", "aa_event_invariants_ck", True, True)
  , ("aa_reconciler_cursor", "aa_cursor_invariants_ck", True, True)
  , ("aa_reconciler_health", "aa_health_invariants_ck", True, True)
  , ("aa_recovery_operations", "aa_recovery_invariants_ck", True, True)
  , ("aa_rate_windows", "aa_rate_invariants_ck", True, True)
  , ("aa_sponsorship_control", "aa_control_singleton_ck", True, True)
  , ("aa_sponsorship_control", "aa_sponsorship_control_reason_consistent", True, True)
  , ("aa_sponsorship_control_events", "aa_control_event_invariants_ck", True, True)
  ]

expectedAaSchemaIndexes :: [AaSchemaIndex]
expectedAaSchemaIndexes =
  [ index "idx_aa_sponsorship_active_request_key" "aa_sponsorship_authorizations" True "request_key" True 1
  , index "idx_aa_sponsorship_sender_state" "aa_sponsorship_authorizations" False "sender,state" False 2
  , index "idx_aa_sponsorship_client_state" "aa_sponsorship_authorizations" False "client_key,state" False 2
  , index "idx_aa_sponsorship_submitted" "aa_sponsorship_authorizations" False "state,submitted_at" False 2
  , index "idx_aa_user_operation_events_finalized_at" "aa_user_operation_events" False "finalized_at" False 1
  , index "idx_aa_user_operation_events_digest" "aa_user_operation_events" False "digest" False 1
  , index "idx_aa_recovery_expiry" "aa_recovery_operations" False "expires_at" False 1
  , index "idx_aa_rate_window_expiry" "aa_rate_windows" False "window_start" False 1
  ]
 where
  index name tableName unique columns partial keyCount =
    AaSchemaIndex name tableName unique True True columns partial keyCount keyCount True True


-- | Atomically reserve the maximum liability.  A single transaction-scoped
-- advisory lock serializes all budget decisions across API replicas.  Exact
-- digest retries return the original row and never reserve twice.
reserveSponsorship
  :: Connection
  -> NativeAaConfig
  -> SponsorshipDraft
  -> IO (Either Text SponsorshipAuthorization)
reserveSponsorship conn cfg draft = withTransaction conn $ do
  acquireAaBudgetLock conn
  pauseReason <- getAaIssuancePause conn
  case pauseReason of
    Just _ -> pure $ Left "PAYMASTER_PAUSED"
    Nothing -> do
      fresh <- aaReconcilerIsFresh conn cfg
      wallClock <- currentWallClockSeconds conn
      if fresh && sdValidUntil draft > wallClock + signatureValiditySafetySeconds
        then reserveWhileUnpaused
        else if not fresh
          then pure $ Left "RECONCILER_STALE"
          else pure $ Left "SPONSORSHIP_VALIDITY_TOO_SHORT"
 where
  reserveWhileUnpaused = do
    existing <- getSponsorshipByRequestKey conn (sdRequestKey draft)
    case existing of
      Just authorization -> do
        wallClock <- currentWallClockSeconds conn
        if not (authorizationMatches draft authorization)
          then pure $ Left "SPONSORSHIP_REQUEST_CONFLICT"
          else
            if
              saState authorization `elem` ["reserved", "signed", "submitted"]
                && saValidUntil authorization > wallClock + signatureValiditySafetySeconds
              then pure $ Right authorization
              else pure $ Left "SPONSORSHIP_RETRY_EXPIRED"
      Nothing -> do
        accountOutstanding <- queryAmount conn
          "SELECT COALESCE(SUM(max_cost_wei), 0)::TEXT FROM aa_sponsorship_authorizations \
          \WHERE sender = ? AND state IN ('reserved','signed','submitted')"
          (Only $ T.toLower $ sdSender draft)
        clientOutstanding <- queryAmount conn
          "SELECT COALESCE(SUM(max_cost_wei), 0)::TEXT FROM aa_sponsorship_authorizations \
          \WHERE client_key = ? AND state IN ('reserved','signed','submitted')"
          (Only $ T.toLower $ sdClientKey draft)
        globalOutstanding <- queryAmount conn
          "SELECT COALESCE(SUM(max_cost_wei), 0)::TEXT FROM aa_sponsorship_authorizations \
          \WHERE state IN ('reserved','signed','submitted')"
          ()
        accountHourly <- queryAmount conn
          "SELECT COALESCE(SUM(e.actual_gas_cost_wei), 0)::TEXT \
          \FROM aa_user_operation_events e JOIN aa_sponsorship_authorizations a ON a.digest=e.digest \
          \WHERE a.sender = ? AND e.finalized_at >= clock_timestamp() - INTERVAL '1 hour'"
          (Only $ T.toLower $ sdSender draft)
        globalHourly <- queryAmount conn
          "SELECT COALESCE(SUM(actual_gas_cost_wei), 0)::TEXT FROM aa_user_operation_events \
          \WHERE finalized_at >= clock_timestamp() - INTERVAL '1 hour'"
          ()
        globalDaily <- queryAmount conn
          "SELECT COALESCE(SUM(actual_gas_cost_wei), 0)::TEXT FROM aa_user_operation_events \
          \WHERE finalized_at >= clock_timestamp() - INTERVAL '24 hours'"
          ()
        let cost = sdMaxCostWei draft
            denied
              | cost > naaMaxCostWei cfg = Just "PER_OPERATION_BUDGET_EXCEEDED"
              | accountOutstanding + cost > naaAccountOutstandingWei cfg = Just "ACCOUNT_OUTSTANDING_BUDGET_EXCEEDED"
              | clientOutstanding + cost > naaClientOutstandingWei cfg = Just "CLIENT_OUTSTANDING_BUDGET_EXCEEDED"
              | globalOutstanding + cost > naaGlobalOutstandingWei cfg = Just "GLOBAL_OUTSTANDING_BUDGET_EXCEEDED"
              | accountHourly + cost > naaAccountHourlyWei cfg = Just "ACCOUNT_HOURLY_BUDGET_EXCEEDED"
              | globalHourly + cost > naaGlobalHourlyWei cfg = Just "GLOBAL_HOURLY_BUDGET_EXCEEDED"
              | globalDaily + cost > naaGlobalDailyWei cfg = Just "GLOBAL_DAILY_BUDGET_EXCEEDED"
              | otherwise = Nothing
        case denied of
          Just reason -> pure $ Left reason
          Nothing -> do
            inserted <- execute conn
              "INSERT INTO aa_sponsorship_authorizations \
              \(request_key, digest, sender, owner, nonce, valid_after, valid_until, \
              \ max_cost_wei, client_key, operation, state, created_at, updated_at) \
              \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, 'reserved', clock_timestamp(), clock_timestamp())"
              ( T.toLower $ sdRequestKey draft
              , T.toLower $ sdDigest draft
              , T.toLower $ sdSender draft
              , T.toLower $ sdOwner draft
              , sdNonce draft
              , sdValidAfter draft
              , sdValidUntil draft
              , sdMaxCostWei draft
              , T.toLower $ sdClientKey draft
              , encode $ sdOperation draft
              )
            unless (inserted == (1 :: Int64)) $
              fail "aa sponsorship reservation insert did not affect one row"
            void $ execute conn
              "INSERT INTO aa_sponsorship_ledger (digest, entry_type, amount_wei, created_at) \
              \VALUES (?, 'reserve', ?, clock_timestamp())"
              (T.toLower $ sdDigest draft, sdMaxCostWei draft)
            maybe
              (fail "aa sponsorship reservation could not be read back")
              (pure . Right)
              =<< getSponsorshipByDigest conn (sdDigest draft)

storeSponsorshipSignature :: Connection -> NativeAaConfig -> Text -> Text -> Text -> IO Bool
storeSponsorshipSignature conn cfg digest signature expectedUserOperationHash = withTransaction conn $ do
  acquireAaBudgetLock conn
  pauseReason <- getAaIssuancePause conn
  case pauseReason of
    Just _ -> pure False
    Nothing -> do
      fresh <- aaReconcilerIsFresh conn cfg
      if not fresh
        then pure False
        else storeWhileFresh
 where
  storeWhileFresh = do
    affected <- execute conn
      "UPDATE aa_sponsorship_authorizations SET signature=?, expected_user_operation_hash=?, \
      \state='signed', signed_at=clock_timestamp(), updated_at=clock_timestamp() \
      \WHERE digest=? AND state='reserved' AND signature IS NULL \
      \AND valid_until > FLOOR(EXTRACT(EPOCH FROM clock_timestamp()))::BIGINT + ? \
      \AND EXISTS (SELECT 1 FROM aa_sponsorship_control \
      \WHERE singleton=TRUE AND NOT issuance_paused AND paused_reason IS NULL) \
      \AND EXISTS (SELECT 1 FROM aa_reconciler_health h \
      \JOIN aa_reconciler_cursor c USING (chain_id,paymaster) \
      \WHERE h.chain_id=421614 AND h.paymaster=? \
      \AND h.safe_block=c.safe_block AND h.safe_block_hash=c.safe_block_hash \
      \AND h.last_success_at >= clock_timestamp()-INTERVAL '120 seconds')"
      ( T.toLower signature
      , T.toLower expectedUserOperationHash
      , T.toLower digest
      , signatureValiditySafetySeconds
      , T.toLower $ naaPaymasterAddress cfg
      )
    if affected == (1 :: Int64)
      then pure True
      else do
        wallClock <- currentWallClockSeconds conn
        existing <- getSponsorshipByDigest conn digest
        pure $ maybe False
          (\current ->
            saSignature current == Just (T.toLower signature)
              && saExpectedUserOperationHash current == Just (T.toLower expectedUserOperationHash)
              && saValidUntil current > wallClock + signatureValiditySafetySeconds
          )
          existing

isSponsorshipDeliveryAllowed :: Connection -> NativeAaConfig -> Text -> IO Bool
isSponsorshipDeliveryAllowed conn cfg digest = withTransaction conn $ do
  acquireAaBudgetLock conn
  pauseReason <- getAaIssuancePause conn
  case pauseReason of
    Just _ -> pure False
    Nothing -> do
      rows <- query conn
        "SELECT EXISTS (SELECT 1 FROM aa_sponsorship_authorizations a \
        \WHERE a.digest=? AND a.state IN ('reserved','signed','submitted') \
        \AND a.valid_until > FLOOR(EXTRACT(EPOCH FROM clock_timestamp()))::BIGINT + ? \
        \AND EXISTS (SELECT 1 FROM aa_reconciler_health h \
        \JOIN aa_reconciler_cursor c USING (chain_id,paymaster) \
        \WHERE h.chain_id=421614 AND h.paymaster=? \
        \AND h.safe_block=c.safe_block AND h.safe_block_hash=c.safe_block_hash \
        \AND h.last_success_at >= clock_timestamp()-INTERVAL '120 seconds'))"
        ( T.toLower digest
        , signatureValiditySafetySeconds
        , T.toLower $ naaPaymasterAddress cfg
        ) :: IO [Only Bool]
      case rows of
        [Only allowed] -> pure allowed
        _ -> fail "sponsorship delivery authorization query returned an invalid row count"

markSponsorshipSubmitted :: Connection -> Text -> Text -> Text -> IO Bool
markSponsorshipSubmitted conn digest userOperationHash clientKey = withTransaction conn $ do
  acquireAaBudgetLock conn
  affected <- execute conn
    "UPDATE aa_sponsorship_authorizations SET user_operation_hash=?, state='submitted', \
    \submitted_at=COALESCE(submitted_at,clock_timestamp()), updated_at=clock_timestamp() \
    \WHERE digest=? AND expected_user_operation_hash=? AND client_key=? \
    \AND state IN ('signed','submitted')"
    ( normalizedHash
    , T.toLower digest
    , normalizedHash
    , T.toLower clientKey
    )
  if affected /= (1 :: Int64)
    then pure False
    else do
      recoveryRecorded <- recordRecoveryOperation conn normalizedHash clientKey "alto"
      unless recoveryRecorded $
        fail "recovery authorization conflicts with the submitted sponsorship"
      pure True
 where
  normalizedHash = T.toLower userOperationHash

getSponsorshipByDigest :: Connection -> Text -> IO (Maybe SponsorshipAuthorization)
getSponsorshipByDigest conn digest = do
  rows <- query conn authorizationSelect (Only $ T.toLower digest)
  pure $ one rows

getSponsorshipByRequestKey :: Connection -> Text -> IO (Maybe SponsorshipAuthorization)
getSponsorshipByRequestKey conn requestKey = do
  rows <- query conn authorizationByRequestKeySelect (Only $ T.toLower requestKey)
  pure $ one rows

getSponsorshipByUserOperationHash :: Connection -> Text -> IO (Maybe SponsorshipAuthorization)
getSponsorshipByUserOperationHash conn operationHash = do
  rows <- query conn
    authorizationByHashSelect
    (Only $ T.toLower operationHash)
  pure $ one rows

listSubmittedSponsorships :: Connection -> Int -> IO [SubmittedAuthorization]
listSubmittedSponsorships conn batchSize =
  query conn
    "SELECT digest, expected_user_operation_hash, valid_until FROM aa_sponsorship_authorizations \
    \WHERE state IN ('signed','submitted') AND expected_user_operation_hash IS NOT NULL \
    \ORDER BY COALESCE(submitted_at,signed_at) ASC LIMIT ?"
    (Only $ max 1 $ min 500 batchSize)

-- | Record canonical safe-chain evidence and settle the reservation exactly
-- once.  A failed user call still consumes gas and is therefore charged.
settleSponsorship
  :: Connection
  -> Text
  -> Text
  -> Text
  -> Integer
  -> Text
  -> Bool
  -> Integer
  -> Value
  -> IO (Either Text ())
settleSponsorship conn digest operationHash transactionHash blockNumber blockHash success actualCost eventValue =
  withTransaction conn $ do
    acquireAaBudgetLock conn
    authorization <- getSponsorshipByDigest conn digest
    case authorization of
      Nothing -> pure $ Left "UNKNOWN_SPONSORSHIP"
      Just current
        | Just (T.toLower operationHash) /= saExpectedUserOperationHash current ->
            pure $ Left "USER_OPERATION_HASH_MISMATCH"
        | actualCost < 0 || actualCost > saMaxCostWei current ->
            pure $ Left "ACTUAL_COST_EXCEEDS_RESERVATION"
        | otherwise -> do
            existingEvents <- query conn
              "SELECT digest,transaction_hash,block_number,block_hash,success,actual_gas_cost_wei::TEXT \
              \FROM aa_user_operation_events WHERE user_operation_hash=? FOR UPDATE"
              (Only $ T.toLower operationHash) :: IO [(Text, Text, Integer, Text, Bool, Text)]
            let eventMatches = case existingEvents of
                  [] -> True
                  [(eventDigest, eventTransactionHash, eventBlockNumber, eventBlockHash, eventSuccess, eventCost)] ->
                    T.toLower eventDigest == T.toLower digest
                      && T.toLower eventTransactionHash == T.toLower transactionHash
                      && eventBlockNumber == blockNumber
                      && T.toLower eventBlockHash == T.toLower blockHash
                      && eventSuccess == success
                      && readMaybe (T.unpack eventCost) == Just actualCost
                  _ -> False
            if not eventMatches
              then pure $ Left "USER_OPERATION_EVENT_CONFLICT"
              else do
                ledgerRows <- query conn
                  "SELECT entry_type,amount_wei::TEXT FROM aa_sponsorship_ledger \
                  \WHERE digest=? AND entry_type IN ('actual_charge','release') FOR UPDATE"
                  (Only $ T.toLower digest) :: IO [(Text, Text)]
                let expectedLedger =
                      [ ("actual_charge", actualCost)
                      , ("release", saMaxCostWei current - actualCost)
                      ]
                    ledgerMatches =
                      all
                        (\(entryType, rawAmount) ->
                          readMaybe (T.unpack rawAmount)
                            == lookup entryType expectedLedger
                        )
                        ledgerRows
                        && length ledgerRows <= 2
                if not ledgerMatches
                  then pure $ Left "SPONSORSHIP_LEDGER_CONFLICT"
                  else case saState current of
                    "settled"
                      | null existingEvents -> pure $ Left "SPONSORSHIP_STATE_CONFLICT"
                      | otherwise -> completeSettlement False expectedLedger
                    state
                      | state `elem` ["signed", "submitted"] -> do
                          -- Move the authorization first. Any later insert or
                          -- exact-readback failure throws, so withTransaction
                          -- rolls this mutation back rather than committing a
                          -- partial event/ledger settlement.
                          affected <- execute conn
                            "UPDATE aa_sponsorship_authorizations SET user_operation_hash=?, state='settled', \
                            \settled_at=clock_timestamp(), updated_at=clock_timestamp() \
                            \WHERE digest=? AND state IN ('signed','submitted')"
                            (T.toLower operationHash, T.toLower digest)
                          if affected /= (1 :: Int64)
                            then pure $ Left "SPONSORSHIP_STATE_CONFLICT"
                            else completeSettlement (null existingEvents) expectedLedger
                    _ -> pure $ Left "SPONSORSHIP_STATE_CONFLICT"
 where
  completeSettlement shouldInsertEvent expectedLedger = do
    when shouldInsertEvent $ do
      inserted <- execute conn
        "INSERT INTO aa_user_operation_events \
        \(user_operation_hash,digest,transaction_hash,block_number,block_hash,success,actual_gas_cost_wei,event_json,observed_at,finalized_at) \
        \VALUES (?,?,?,?,?,?,?,?,clock_timestamp(),clock_timestamp())"
        ( T.toLower operationHash
        , T.toLower digest
        , T.toLower transactionHash
        , blockNumber
        , T.toLower blockHash
        , success
        , actualCost
        , encode eventValue
        )
      unless (inserted == (1 :: Int64)) $
        fail "aa UserOperation event insert did not affect one row"
    mapM_
      (\(entryType, amount) -> ensureLedgerEntryExact conn digest entryType amount)
      expectedLedger
    pure $ Right ()

ensureLedgerEntryExact :: Connection -> Text -> Text -> Integer -> IO ()
ensureLedgerEntryExact conn digest entryType amount = do
  void $ execute conn
    "INSERT INTO aa_sponsorship_ledger (digest,entry_type,amount_wei,created_at) \
    \VALUES (?,?,?,clock_timestamp()) ON CONFLICT (digest,entry_type) DO NOTHING"
    (T.toLower digest, entryType, amount)
  rows <- query conn
    "SELECT amount_wei::TEXT FROM aa_sponsorship_ledger \
    \WHERE digest=? AND entry_type=? FOR UPDATE"
    (T.toLower digest, entryType) :: IO [Only Text]
  case rows of
    [Only rawAmount]
      | readMaybe (T.unpack rawAmount) == Just amount -> pure ()
      | otherwise -> fail "aa sponsorship ledger entry conflicts with the expected amount"
    _ -> fail "aa sponsorship ledger entry could not be read back exactly"

recordRecoveryOperation :: Connection -> Text -> Text -> Text -> IO Bool
recordRecoveryOperation conn operationHash clientKey provider = do
  rows <- query conn
    "INSERT INTO aa_recovery_operations (user_operation_hash,client_key,provider,expires_at) \
    \VALUES (?,?,?,clock_timestamp()+INTERVAL '7 days') ON CONFLICT (user_operation_hash) DO UPDATE SET \
    \expires_at=GREATEST(aa_recovery_operations.expires_at,EXCLUDED.expires_at) \
    \WHERE aa_recovery_operations.client_key=EXCLUDED.client_key \
    \AND aa_recovery_operations.provider=EXCLUDED.provider RETURNING TRUE"
    (T.toLower operationHash, T.toLower clientKey, provider) :: IO [Only Bool]
  pure $ rows == [Only True]

isRecoveryOperationAuthorized :: Connection -> Text -> Text -> Text -> IO Bool
isRecoveryOperationAuthorized conn operationHash clientKey provider = do
  rows <- query conn
    "SELECT EXISTS (SELECT 1 FROM aa_recovery_operations \
    \WHERE user_operation_hash=? AND client_key=? AND provider=? \
    \AND expires_at>clock_timestamp())"
    (T.toLower operationHash, T.toLower clientKey, provider)
  pure $ case rows of
    [Only allowed] -> allowed
    _ -> False

-- | Cross-replica fixed-window limiter. Both keys are HMAC pseudonyms created
-- by the gateway, so this table never contains a raw client IP.
consumeAaRateLimit
  :: Connection
  -> Text
  -> Text
  -> Text
  -> Int
  -> IO Bool
consumeAaRateLimit conn scope clientKey accountKey requestLimit = do
  rows <- query conn
    "INSERT INTO aa_rate_windows \
    \(scope,client_key,account_key,window_start,request_count) \
    \VALUES (?,?,?,date_trunc('minute',clock_timestamp()),1) \
    \ON CONFLICT (scope,client_key,account_key,window_start) DO UPDATE SET \
    \request_count=aa_rate_windows.request_count+1,updated_at=clock_timestamp() \
    \WHERE aa_rate_windows.request_count < ? RETURNING request_count"
    (scope, T.toLower clientKey, T.toLower accountKey, max 1 requestLimit) :: IO [Only Int]
  pure $ not $ null rows

-- | Bound the durable rate-limit table without weakening any active window.
-- Reconciliation calls this only after a complete dual-provider cycle.
pruneAaRateWindows :: Connection -> IO Int64
pruneAaRateWindows conn =
  execute_ conn
    "DELETE FROM aa_rate_windows \
    \WHERE window_start < clock_timestamp()-INTERVAL '48 hours'"

pruneExpiredRecoveryOperations :: Connection -> IO Int64
pruneExpiredRecoveryOperations conn =
  execute_ conn
    "DELETE FROM aa_recovery_operations WHERE expires_at < clock_timestamp()"

getAaIssuancePause :: Connection -> IO (Maybe Text)
getAaIssuancePause conn = do
  rows <- query_ conn
    "SELECT issuance_paused,paused_reason FROM aa_sponsorship_control WHERE singleton=TRUE" :: IO [(Bool, Maybe Text)]
  case rows of
    [(False, Nothing)] -> pure Nothing
    [(True, Just reason)]
      | not (T.null $ T.strip reason)
      , T.length reason <= 512 -> pure $ Just reason
    _ -> fail "aa sponsorship control row is missing or invalid"

pauseAaIssuance :: Connection -> Text -> IO ()
pauseAaIssuance conn reason = withTransaction conn $ do
  acquireAaBudgetLock conn
  affected <- execute conn
    "UPDATE aa_sponsorship_control SET issuance_paused=TRUE,paused_reason=?,updated_at=clock_timestamp() \
    \WHERE singleton=TRUE AND (NOT issuance_paused OR paused_reason IS DISTINCT FROM ?)"
    (normalizedReason, normalizedReason)
  when (affected == (1 :: Int64)) $
    void $ execute conn
      "INSERT INTO aa_sponsorship_control_events (action,reason,created_at) \
      \VALUES ('pause',?,clock_timestamp())"
      (Only normalizedReason)
  unless (affected == (1 :: Int64) || affected == 0) $
    fail "aa sponsorship control update affected an invalid row count"
  when (affected == 0) $ do
    current <- getAaIssuancePause conn
    when (current /= Just normalizedReason) $
      fail "aa sponsorship control row could not be paused"
 where
  normalizedReason = nonBlankLimited "unspecified circuit breaker" reason

-- | Clear the breaker only when an operator proves it observed the exact
-- current reason and supplies an audit note. There is intentionally no
-- automatic unpause path.
resumeAaIssuance :: Connection -> Text -> Text -> IO (Either Text ())
resumeAaIssuance conn expectedReason operatorNote = withTransaction conn $ do
  acquireAaBudgetLock conn
  current <- query_ conn
    "SELECT issuance_paused,paused_reason FROM aa_sponsorship_control \
    \WHERE singleton=TRUE FOR UPDATE" :: IO [(Bool, Maybe Text)]
  case current of
    [(False, _)] -> pure $ Left "AA issuance is not paused"
    [(True, Just actualReason)]
      | actualReason /= normalizedExpected ->
          pure $ Left "The expected pause reason does not match the current circuit breaker"
      | T.null normalizedNote ->
          pure $ Left "An operator note is required"
      | T.length normalizedNote > 512 ->
          pure $ Left "The operator note must not exceed 512 characters"
      | otherwise -> do
          affected <- execute conn
            "UPDATE aa_sponsorship_control SET issuance_paused=FALSE,paused_reason=NULL,updated_at=clock_timestamp() \
            \WHERE singleton=TRUE AND issuance_paused AND paused_reason=?"
            (Only normalizedExpected)
          if affected /= (1 :: Int64)
            then pure $ Left "The circuit breaker changed concurrently"
            else do
              void $ execute conn
                "INSERT INTO aa_sponsorship_control_events (action,reason,operator_note,created_at) \
                \VALUES ('resume',?,?,clock_timestamp())"
                (normalizedExpected, normalizedNote)
              pure $ Right ()
    _ -> pure $ Left "The AA sponsorship control row is missing or invalid"
 where
  normalizedExpected = expectedReason
  normalizedNote = T.strip operatorNote

getAaReconcilerCursor
  :: Connection
  -> Integer
  -> Text
  -> IO (Maybe AaReconcilerCursor)
getAaReconcilerCursor conn chainId paymaster = do
  rows <- query conn
    "SELECT safe_block,safe_block_hash FROM aa_reconciler_cursor \
    \WHERE chain_id=? AND paymaster=?"
    (chainId, T.toLower paymaster)
  pure $ one rows

initializeAaReconcilerCursor
  :: Connection
  -> Integer
  -> Text
  -> Integer
  -> Text
  -> IO AaReconcilerCursor
initializeAaReconcilerCursor conn chainId paymaster blockNumber blockHash = withTransaction conn $ do
  acquireAaBudgetLock conn
  before <- getAaReconcilerCursor conn chainId paymaster
  case before of
    Just cursor
      | cursor == expected -> pure ()
      | otherwise -> fail "aa reconciler cursor conflicts with the configured deployment anchor"
    Nothing -> do
      emptyState <- aaSponsorshipStateIsEmpty conn
      unless emptyState $
        fail "aa reconciler cursor cannot be bootstrapped over existing sponsorship state"
      inserted <- execute conn
        "INSERT INTO aa_reconciler_cursor (chain_id,paymaster,safe_block,safe_block_hash) \
        \VALUES (?,?,?,?) ON CONFLICT (chain_id,paymaster) DO NOTHING"
        (chainId, T.toLower paymaster, blockNumber, T.toLower blockHash)
      unless (inserted == (1 :: Int64)) $
        fail "aa reconciler cursor was concurrently initialized"
  current <- getAaReconcilerCursor conn chainId paymaster
  case current of
    Just cursor
      | cursor == expected -> pure cursor
      | otherwise -> fail "aa reconciler cursor conflicts with the configured deployment anchor"
    Nothing -> fail "aa reconciler cursor could not be initialized"
 where
  expected = AaReconcilerCursor blockNumber $ T.toLower blockHash

-- | A missing cursor may only be bootstrapped before any sponsorship economic
-- state exists. This prevents a partial restore from silently skipping history.
aaSponsorshipStateIsEmpty :: Connection -> IO Bool
aaSponsorshipStateIsEmpty conn = do
  rows <- query_ conn
    "SELECT \
    \(SELECT COUNT(*) FROM aa_sponsorship_authorizations), \
    \(SELECT COUNT(*) FROM aa_sponsorship_ledger), \
    \(SELECT COUNT(*) FROM aa_user_operation_events)" :: IO [(Int64, Int64, Int64)]
  case rows of
    [(authorizationCount, ledgerCount, eventCount)] ->
      pure $ authorizationCount == 0 && ledgerCount == 0 && eventCount == 0
    _ -> fail "aa sponsorship state inventory did not return exactly one row"

advanceAaReconcilerCursor
  :: Connection
  -> Integer
  -> Text
  -> AaReconcilerCursor
  -> AaReconcilerCursor
  -> IO Bool
advanceAaReconcilerCursor conn chainId paymaster previous next = withTransaction conn $ do
  affected <- execute conn
    "UPDATE aa_reconciler_cursor SET safe_block=?,safe_block_hash=?,updated_at=clock_timestamp() \
    \WHERE chain_id=? AND paymaster=? AND safe_block=? AND safe_block_hash=?"
    ( arcSafeBlock next
    , T.toLower $ arcSafeBlockHash next
    , chainId
    , T.toLower paymaster
    , arcSafeBlock previous
    , T.toLower $ arcSafeBlockHash previous
    )
  pure $ affected == (1 :: Int64)

recordAaReconcilerHeartbeat
  :: Connection
  -> Integer
  -> Text
  -> Integer
  -> Text
  -> IO ()
recordAaReconcilerHeartbeat conn chainId paymaster safeBlock safeBlockHash = do
  affected <- execute conn
    "INSERT INTO aa_reconciler_health \
    \(chain_id,paymaster,safe_block,safe_block_hash,last_success_at) \
    \VALUES (?,?,?,?,clock_timestamp()) ON CONFLICT (chain_id,paymaster) DO UPDATE SET \
    \safe_block=EXCLUDED.safe_block,safe_block_hash=EXCLUDED.safe_block_hash,last_success_at=clock_timestamp() \
    \WHERE EXCLUDED.safe_block > aa_reconciler_health.safe_block \
    \OR (EXCLUDED.safe_block = aa_reconciler_health.safe_block \
    \AND EXCLUDED.safe_block_hash = aa_reconciler_health.safe_block_hash)"
    (chainId, T.toLower paymaster, safeBlock, T.toLower safeBlockHash)
  unless (affected == (1 :: Int64)) $
    fail "aa reconciler heartbeat attempted to regress or rewrite the safe cursor"

-- | Release signed/submitted liabilities only after the caller has scanned a
-- continuous canonical range through a safe block whose timestamp is beyond
-- the signed validity window.
expireSponsorshipsThrough :: Connection -> Integer -> IO Int64
expireSponsorshipsThrough conn safeTimestamp = withTransaction conn $ do
  acquireAaBudgetLock conn
  expired <- query conn
    "UPDATE aa_sponsorship_authorizations SET state='expired',settled_at=clock_timestamp(),updated_at=clock_timestamp() \
    \WHERE state IN ('signed','submitted') AND valid_until < ? \
    \AND NOT EXISTS (SELECT 1 FROM aa_user_operation_events e \
    \WHERE e.digest=aa_sponsorship_authorizations.digest) \
    \RETURNING digest,max_cost_wei" (Only safeTimestamp) :: IO [(Text, Integer)]
  mapM_
    (\(digest, amount) ->
      ensureLedgerEntryExact conn digest "release" amount
    )
    expired
  pure $ fromIntegral $ length expired

cancelStaleUnsignedReservations :: Connection -> IO Int64
cancelStaleUnsignedReservations conn = withTransaction conn $ do
  acquireAaBudgetLock conn
  cancelled <- query_ conn
    "UPDATE aa_sponsorship_authorizations SET state='cancelled',settled_at=clock_timestamp(),updated_at=clock_timestamp() \
    \WHERE state='reserved' AND signature IS NULL AND created_at < clock_timestamp()-INTERVAL '10 minutes' \
    \RETURNING digest,max_cost_wei" :: IO [(Text, Integer)]
  mapM_
    (\(digest, amount) ->
      ensureLedgerEntryExact conn digest "release" amount
    )
    cancelled
  pure $ fromIntegral $ length cancelled

authorizationSelect :: Query
authorizationSelect =
  "SELECT request_key,digest,expected_user_operation_hash,sender,owner,nonce,valid_after,valid_until, \
  \max_cost_wei,client_key,signature,state FROM aa_sponsorship_authorizations WHERE digest = ?"

authorizationByRequestKeySelect :: Query
authorizationByRequestKeySelect =
  "SELECT request_key,digest,expected_user_operation_hash,sender,owner,nonce,valid_after,valid_until, \
  \max_cost_wei,client_key,signature,state FROM aa_sponsorship_authorizations \
  \WHERE request_key = ? AND state IN ('reserved','signed','submitted')"

authorizationByHashSelect :: Query
authorizationByHashSelect =
  "SELECT request_key,digest,expected_user_operation_hash,sender,owner,nonce,valid_after,valid_until, \
  \max_cost_wei,client_key,signature,state FROM aa_sponsorship_authorizations \
  \WHERE expected_user_operation_hash = ?"

authorizationMatches :: SponsorshipDraft -> SponsorshipAuthorization -> Bool
authorizationMatches draft authorization =
  and
    [ T.toLower (sdRequestKey draft) == saRequestKey authorization
    , T.toLower (sdSender draft) == saSender authorization
    , T.toLower (sdOwner draft) == saOwner authorization
    , sdNonce draft == saNonce authorization
    , T.toLower (sdClientKey draft) == saClientKey authorization
    ]

queryAmount
  :: ToRow q
  => Connection
  -> Query
  -> q
  -> IO Integer
queryAmount conn statement params = do
  rows <- query conn statement params :: IO [Only Text]
  case rows of
    [Only raw] ->
      maybe
        (fail "aa sponsorship budget aggregate is malformed")
        pure
        (readMaybe $ T.unpack raw)
    _ -> fail "aa sponsorship budget aggregate did not return exactly one row"

aaReconcilerIsFresh :: Connection -> NativeAaConfig -> IO Bool
aaReconcilerIsFresh conn cfg = do
  rows <- query conn
    "SELECT EXISTS (SELECT 1 FROM aa_reconciler_health h \
    \JOIN aa_reconciler_cursor c USING (chain_id,paymaster) \
    \WHERE h.chain_id=421614 AND h.paymaster=? \
    \AND h.safe_block=c.safe_block AND h.safe_block_hash=c.safe_block_hash \
    \AND h.last_success_at >= clock_timestamp()-INTERVAL '120 seconds')"
    (Only $ T.toLower $ naaPaymasterAddress cfg)
  case rows of
    [Only fresh] -> pure fresh
    _ -> fail "aa reconciler health query did not return exactly one row"

one :: [a] -> Maybe a
one [value] = Just value
one _ = Nothing

nonBlankLimited :: Text -> Text -> Text
nonBlankLimited fallback value =
  let normalized = T.take 512 $ T.strip value
   in if T.null normalized then fallback else normalized

aaBudgetLockId :: Integer
aaBudgetLockId = 4_338_008_421_614

acquireAaBudgetLock :: Connection -> IO ()
acquireAaBudgetLock conn = do
  void $ execute_ conn "SET LOCAL lock_timeout = '5s'"
  void $ execute_ conn "SET LOCAL statement_timeout = '15s'"
  rows <- query conn
    "SELECT 1::BIGINT FROM (SELECT pg_advisory_xact_lock(?)) AS locked"
    (Only aaBudgetLockId) :: IO [Only Integer]
  unless (rows == [Only 1]) $
    fail "aa sponsorship advisory lock was not acquired"

currentWallClockSeconds :: Connection -> IO Integer
currentWallClockSeconds conn = do
  rows <- query_ conn
    "SELECT FLOOR(EXTRACT(EPOCH FROM clock_timestamp()))::BIGINT" :: IO [Only Integer]
  case rows of
    [Only timestamp] -> pure timestamp
    _ -> fail "database wall-clock query did not return exactly one row"

signatureValiditySafetySeconds :: Integer
signatureValiditySafetySeconds = 30

controlBootstrapReason :: Text
controlBootstrapReason = "uninitialized or control row recreated"
