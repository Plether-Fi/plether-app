module Plether.Database.Insights.Registration
  ( RegistrationCompetition (..)
  , RegistrationSessionRow (..)
  , RegistrationEmailRow (..)
  , OAuthChallengeRow (..)
  , OAuthChallengeConsumeResult (..)
  , XFollowMaterialRow (..)
  , WalletChallengeRow (..)
  , ReleaseIndexerCursor (..)
  , RegistrationMutationResult (..)
  , XFollowClaimResult (..)
  , RegistrationKeyReferenceCounts (..)
  , RegistrationCleanupResult (..)
  , CompletionResult (..)
  , RegistrationCompletionState (..)
  , CreateSessionResult (..)
  , ensureRegistrationSchema
  , provisionRegistrationCompetitionConfig
  , openRegistrationIfConfigured
  , getRegistrationCompetition
  , createRegistrationSession
  , getRegistrationSession
  , registrationCompletionState
  , storeOAuthChallenge
  , consumeOAuthChallenge
  , recordOAuthCallbackError
  , storeXIdentityAndRefreshSession
  , claimXFollowMaterial
  , eraseXProviderSecrets
  , releaseXFollowAttempt
  , resetXIdentityAfterFollowFailure
  , confirmXFollow
  , storeWalletChallenge
  , consumeWalletChallenge
  , storeVerifiedWallet
  , clearVerifiedWallet
  , registrationRateLimitAllowed
  , cleanupExpiredRegistrationSecrets
  , tradingAccountHasReleaseActivity
  , getReleaseIndexerBlock
  , getReleaseIndexerCursor
  , completeRegistration
  , listRegistrationEmailsForRotation
  , countRegistrationEmailsByKeyVersion
  , countRegistrationKeyReferences
  , reencryptRegistrationEmails
  ) where

import Control.Exception (throwIO, try)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Database.PostgreSQL.Simple
  ( Connection
  , Only (..)
  , Query
  , SqlError (..)
  , execute
  , execute_
  , query
  , withTransaction
  )
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import Database.PostgreSQL.Simple.Types (Binary (..))
import Plether.Insights.Registration.Crypto (EncryptedValue (..))

bytea :: ByteString -> Binary ByteString
bytea = Binary

data RegistrationCompetition = RegistrationCompetition
  { rgcSlug :: Text
  , rgcChainId :: Integer
  , rgcReleaseRouter :: Text
  , rgcUsdcAddress :: Text
  , rgcReleaseManifest :: Text
  , rgcStartTimestamp :: Integer
  , rgcRegistrationOpenTimestamp :: Maybe Integer
  , rgcRegistrationCloseTimestamp :: Integer
  , rgcMinimumXAccountAgeDays :: Int
  , rgcTargetXHandle :: Text
  , rgcRulesVersion :: Text
  , rgcPrivacyNoticeVersion :: Maybe Text
  , rgcFinalized :: Bool
  }
  deriving stock (Show, Eq)

instance FromRow RegistrationCompetition where
  fromRow = RegistrationCompetition
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

data RegistrationSessionRow = RegistrationSessionRow
  { rsrApplicationId :: Text
  , rsrCompetitionSlug :: Text
  , rsrStatus :: Text
  , rsrSessionExpiresTimestamp :: Integer
  , rsrCsrfDigest :: ByteString
  , rsrCsrfEncrypted :: EncryptedValue
  , rsrOauthErrorCode :: Maybe Text
  , rsrEmailMasked :: Maybe Text
  , rsrXUsername :: Maybe Text
  , rsrXIdentityVerified :: Bool
  , rsrXFollowVerified :: Bool
  , rsrOwnerWallet :: Maybe Text
  , rsrTradingAccount :: Maybe Text
  , rsrWalletVerified :: Bool
  , rsrRulesVersion :: Text
  , rsrPrivacyVersion :: Text
  }
  deriving stock (Eq)

instance Show RegistrationSessionRow where
  show row =
    "RegistrationSessionRow {rsrApplicationId = "
      <> show (rsrApplicationId row)
      <> ", rsrCompetitionSlug = "
      <> show (rsrCompetitionSlug row)
      <> ", rsrStatus = "
      <> show (rsrStatus row)
      <> ", rsrSessionExpiresTimestamp = "
      <> show (rsrSessionExpiresTimestamp row)
      <> ", rsrCsrfDigest = <redacted>, rsrCsrfEncrypted = <redacted>, rsrOauthErrorCode = "
      <> show (rsrOauthErrorCode row)
      <> ", rsrEmailMasked = <redacted>, rsrXUsername = "
      <> show (rsrXUsername row)
      <> ", rsrXIdentityVerified = "
      <> show (rsrXIdentityVerified row)
      <> ", rsrXFollowVerified = "
      <> show (rsrXFollowVerified row)
      <> ", rsrOwnerWallet = <redacted>, rsrTradingAccount = <redacted>, rsrWalletVerified = "
      <> show (rsrWalletVerified row)
      <> ", rsrRulesVersion = "
      <> show (rsrRulesVersion row)
      <> ", rsrPrivacyVersion = "
      <> show (rsrPrivacyVersion row)
      <> "}"

instance FromRow RegistrationSessionRow where
  fromRow = do
    applicationId <- field
    competitionSlug <- field
    status <- field
    expiresTimestamp <- field
    csrfDigest <- field
    csrfKeyVersion <- field
    csrfNonce <- field
    csrfCiphertext <- field
    csrfTag <- field
    oauthErrorCode <- field
    emailMasked <- field
    xUsername <- field
    xIdentityVerified <- field
    xFollowVerified <- field
    ownerWallet <- field
    tradingAccount <- field
    walletVerified <- field
    rulesVersion <- field
    privacyVersion <- field
    pure
      RegistrationSessionRow
        { rsrApplicationId = applicationId
        , rsrCompetitionSlug = competitionSlug
        , rsrStatus = status
        , rsrSessionExpiresTimestamp = expiresTimestamp
        , rsrCsrfDigest = csrfDigest
        , rsrCsrfEncrypted = EncryptedValue csrfKeyVersion csrfNonce csrfCiphertext csrfTag
        , rsrOauthErrorCode = oauthErrorCode
        , rsrEmailMasked = emailMasked
        , rsrXUsername = xUsername
        , rsrXIdentityVerified = xIdentityVerified
        , rsrXFollowVerified = xFollowVerified
        , rsrOwnerWallet = ownerWallet
        , rsrTradingAccount = tradingAccount
        , rsrWalletVerified = walletVerified
        , rsrRulesVersion = rulesVersion
        , rsrPrivacyVersion = privacyVersion
        }

data RegistrationEmailRow = RegistrationEmailRow
  { rerApplicationId :: Text
  , rerCompetitionSlug :: Text
  , rerEncryptedEmail :: EncryptedValue
  }
  deriving stock (Eq)

instance Show RegistrationEmailRow where
  show row =
    "RegistrationEmailRow {rerApplicationId = "
      <> show (rerApplicationId row)
      <> ", rerCompetitionSlug = "
      <> show (rerCompetitionSlug row)
      <> ", rerEncryptedEmail = <redacted>}"

instance FromRow RegistrationEmailRow where
  fromRow = do
    applicationId <- field
    competitionSlug <- field
    keyVersion <- field
    nonce <- field
    ciphertext <- field
    tag <- field
    pure $ RegistrationEmailRow applicationId competitionSlug $ EncryptedValue keyVersion nonce ciphertext tag

data OAuthChallengeRow = OAuthChallengeRow
  { ocrApplicationId :: Text
  , ocrCompetitionSlug :: Text
  , ocrPkceVerifier :: EncryptedValue
  , ocrRegistrationOpen :: Bool
  , ocrChallengeUnexpired :: Bool
  }
  deriving stock (Eq)

data OAuthChallengeConsumeResult
  = OAuthChallengeConsumed OAuthChallengeRow
  | OAuthChallengeExpired
  | OAuthChallengeUnavailable
  deriving stock (Show, Eq)

instance Show OAuthChallengeRow where
  show row =
    "OAuthChallengeRow {ocrApplicationId = "
      <> show (ocrApplicationId row)
      <> ", ocrCompetitionSlug = "
      <> show (ocrCompetitionSlug row)
      <> ", ocrPkceVerifier = <redacted>, ocrRegistrationOpen = "
      <> show (ocrRegistrationOpen row)
      <> ", ocrChallengeUnexpired = "
      <> show (ocrChallengeUnexpired row)
      <> "}"

instance FromRow OAuthChallengeRow where
  fromRow = do
    applicationId <- field
    competitionSlug <- field
    keyVersion <- field
    nonce <- field
    ciphertext <- field
    tag <- field
    registrationOpen <- field
    challengeUnexpired <- field
    pure $ OAuthChallengeRow applicationId competitionSlug (EncryptedValue keyVersion nonce ciphertext tag) registrationOpen challengeUnexpired

data XFollowMaterialRow = XFollowMaterialRow
  { xfmrApplicationId :: Text
  , xfmrCompetitionSlug :: Text
  , xfmrXUserId :: EncryptedValue
  , xfmrAccessToken :: EncryptedValue
  }
  deriving stock (Eq)

instance Show XFollowMaterialRow where
  show row =
    "XFollowMaterialRow {xfmrApplicationId = "
      <> show (xfmrApplicationId row)
      <> ", xfmrCompetitionSlug = "
      <> show (xfmrCompetitionSlug row)
      <> ", xfmrXUserId = <redacted>, xfmrAccessToken = <redacted>}"

instance FromRow XFollowMaterialRow where
  fromRow = do
    applicationId <- field
    competitionSlug <- field
    userKeyVersion <- field
    userNonce <- field
    userCiphertext <- field
    userTag <- field
    tokenKeyVersion <- field
    tokenNonce <- field
    tokenCiphertext <- field
    tokenTag <- field
    pure $
      XFollowMaterialRow
        applicationId
        competitionSlug
        (EncryptedValue userKeyVersion userNonce userCiphertext userTag)
        (EncryptedValue tokenKeyVersion tokenNonce tokenCiphertext tokenTag)

data WalletChallengeRow = WalletChallengeRow
  { wchrApplicationId :: Text
  , wchrCompetitionSlug :: Text
  , wchrOwnerWallet :: Text
  , wchrExpiresTimestamp :: Integer
  , wchrMessage :: EncryptedValue
  , wchrRegistrationOpen :: Bool
  }
  deriving stock (Eq)

instance Show WalletChallengeRow where
  show row =
    "WalletChallengeRow {wchrApplicationId = "
      <> show (wchrApplicationId row)
      <> ", wchrCompetitionSlug = "
      <> show (wchrCompetitionSlug row)
      <> ", wchrOwnerWallet = <redacted>, wchrExpiresTimestamp = "
      <> show (wchrExpiresTimestamp row)
      <> ", wchrMessage = <redacted>, wchrRegistrationOpen = "
      <> show (wchrRegistrationOpen row)
      <> "}"

instance FromRow WalletChallengeRow where
  fromRow = do
    applicationId <- field
    competitionSlug <- field
    ownerWallet <- field
    expiresTimestamp <- field
    keyVersion <- field
    nonce <- field
    ciphertext <- field
    tag <- field
    registrationOpen <- field
    pure $
      WalletChallengeRow
        applicationId
        competitionSlug
        ownerWallet
        expiresTimestamp
        (EncryptedValue keyVersion nonce ciphertext tag)
        registrationOpen

data ReleaseIndexerCursor = ReleaseIndexerCursor
  { ricConfiguredStartBlock :: Integer
  , ricBlockNumber :: Integer
  , ricBlockHash :: Text
  }
  deriving stock (Show, Eq)

instance FromRow ReleaseIndexerCursor where
  fromRow = ReleaseIndexerCursor <$> field <*> field <*> field

data CompletionResult
  = CompletionSucceeded
  | CompletionAlreadySucceeded
  | CompletionClosed
  | CompletionIncomplete
  | CompletionDuplicate
  | CompletionTradingAccountUsed
  deriving stock (Show, Eq)

data RegistrationCompletionState
  = RegistrationCompletionOpen
  | RegistrationCompletionAlreadySucceeded
  | RegistrationCompletionClosed
  deriving stock (Show, Eq)

data CreateSessionResult
  = SessionCreated
  | SessionRegistrationClosed
  | SessionTurnstileReplay
  deriving stock (Show, Eq)

data RegistrationMutationResult
  = MutationApplied
  | MutationClosed
  | MutationIncomplete
  deriving stock (Show, Eq)

data XFollowClaimResult
  = XFollowClaimed XFollowMaterialRow
  | XFollowClaimClosed
  | XFollowClaimUnavailable
  deriving stock (Show, Eq)

data RegistrationKeyReferenceCounts = RegistrationKeyReferenceCounts
  { rkrcEmail :: Integer
  , rkrcXUserId :: Integer
  , rkrcXAccess :: Integer
  , rkrcCsrf :: Integer
  , rkrcPkce :: Integer
  , rkrcWalletMessage :: Integer
  }
  deriving stock (Show, Eq)

data RegistrationCleanupResult = RegistrationCleanupResult
  { rcrCleanedRecords :: Int64
  , rcrMayHaveMore :: Bool
  }
  deriving stock (Show, Eq)

ensureRegistrationSchema :: Connection -> IO ()
ensureRegistrationSchema connection = do
  _ <- execute_ connection
    "CREATE TABLE IF NOT EXISTS insights_registration_competition_config (\
    \ competition_slug TEXT PRIMARY KEY REFERENCES insights_competitions(slug) ON DELETE CASCADE,\
    \ target_x_user_id_digest BYTEA NOT NULL, privacy_version TEXT NOT NULL,\
    \ created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),\
    \ CHECK (octet_length(target_x_user_id_digest)=32),\
    \ CHECK (privacy_version ~ '^[A-Za-z0-9_.-]{1,64}$')\
    \ )"
  _ <- execute_ connection
    "CREATE TABLE IF NOT EXISTS insights_registration_applications (\
    \ registration_id UUID PRIMARY KEY,\
    \ competition_slug TEXT NOT NULL REFERENCES insights_competitions(slug) ON DELETE CASCADE,\
    \ status TEXT NOT NULL DEFAULT 'in_progress',\
    \ turnstile_token_digest BYTEA NOT NULL CONSTRAINT insights_registration_turnstile_digest_unique UNIQUE,\
    \ email_key_version TEXT, email_nonce BYTEA, email_ciphertext BYTEA, email_tag BYTEA,\
    \ email_digest BYTEA, email_masked TEXT,\
    \ x_user_id_key_version TEXT, x_user_id_nonce BYTEA, x_user_id_ciphertext BYTEA, x_user_id_tag BYTEA,\
    \ x_user_id_digest BYTEA, x_username TEXT, x_created_timestamp BIGINT, x_identity_verified_at TIMESTAMPTZ,\
    \ x_access_key_version TEXT, x_access_nonce BYTEA, x_access_ciphertext BYTEA, x_access_tag BYTEA,\
    \ x_follow_attempt_id UUID, x_follow_attempt_started_at TIMESTAMPTZ, x_follow_verified_at TIMESTAMPTZ,\
    \ owner_wallet VARCHAR(42), trading_account VARCHAR(42), wallet_verification_block BIGINT,\
    \ wallet_verification_block_hash TEXT, wallet_verified_at TIMESTAMPTZ,\
    \ rules_version TEXT, privacy_version TEXT, completed_at TIMESTAMPTZ,\
    \ created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(), updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),\
    \ CHECK (status IN ('in_progress', 'completed')),\
    \ CHECK (num_nonnulls(email_key_version,email_nonce,email_ciphertext,email_tag) IN (0,4)\
    \   AND (email_nonce IS NULL OR octet_length(email_nonce)=12) AND (email_tag IS NULL OR octet_length(email_tag)=16)),\
    \ CHECK (num_nonnulls(x_user_id_key_version,x_user_id_nonce,x_user_id_ciphertext,x_user_id_tag) IN (0,4)\
    \   AND (x_user_id_nonce IS NULL OR octet_length(x_user_id_nonce)=12) AND (x_user_id_tag IS NULL OR octet_length(x_user_id_tag)=16)),\
    \ CHECK (num_nonnulls(x_access_key_version,x_access_nonce,x_access_ciphertext,x_access_tag) IN (0,4)\
    \   AND (x_access_nonce IS NULL OR octet_length(x_access_nonce)=12) AND (x_access_tag IS NULL OR octet_length(x_access_tag)=16)),\
    \ CHECK (num_nonnulls(x_follow_attempt_id,x_follow_attempt_started_at) IN (0,2)),\
    \ CHECK ((owner_wallet IS NULL OR owner_wallet ~ '^0x[0-9a-f]{40}$') AND (trading_account IS NULL OR trading_account ~ '^0x[0-9a-f]{40}$')),\
    \ CHECK (num_nonnulls(wallet_verification_block,wallet_verification_block_hash) IN (0,2)\
    \   AND (wallet_verification_block IS NULL OR wallet_verification_block >= 0)\
    \   AND (wallet_verification_block_hash IS NULL OR wallet_verification_block_hash ~ '^0x[0-9a-f]{64}$')),\
    \ CHECK (octet_length(turnstile_token_digest)=32 AND (email_digest IS NULL OR octet_length(email_digest)=32) AND (x_user_id_digest IS NULL OR octet_length(x_user_id_digest)=32)),\
    \ CHECK (status <> 'completed' OR (completed_at IS NOT NULL AND rules_version IS NOT NULL AND privacy_version IS NOT NULL\
    \   AND email_digest IS NOT NULL AND email_masked IS NOT NULL\
    \   AND num_nonnulls(email_key_version,email_nonce,email_ciphertext,email_tag)=4\
    \   AND x_user_id_digest IS NOT NULL AND x_username IS NOT NULL AND x_created_timestamp IS NOT NULL\
    \   AND x_identity_verified_at IS NOT NULL AND x_follow_verified_at IS NOT NULL\
    \   AND x_follow_attempt_id IS NULL AND x_follow_attempt_started_at IS NULL\
    \   AND owner_wallet IS NOT NULL AND trading_account IS NOT NULL AND wallet_verification_block IS NOT NULL\
    \   AND wallet_verification_block_hash IS NOT NULL AND wallet_verified_at IS NOT NULL\
    \   AND num_nonnulls(x_user_id_key_version,x_user_id_nonce,x_user_id_ciphertext,x_user_id_tag)=0\
    \   AND num_nonnulls(x_access_key_version,x_access_nonce,x_access_ciphertext,x_access_tag)=0))\
    \ )"
  _ <- execute_ connection
    "CREATE UNIQUE INDEX IF NOT EXISTS idx_insights_registration_email_unique\
    \ ON insights_registration_applications(competition_slug, email_digest) WHERE status = 'completed'"
  _ <- execute_ connection
    "CREATE UNIQUE INDEX IF NOT EXISTS idx_insights_registration_x_unique\
    \ ON insights_registration_applications(competition_slug, x_user_id_digest) WHERE status = 'completed'"
  _ <- execute_ connection
    "CREATE UNIQUE INDEX IF NOT EXISTS idx_insights_registration_owner_unique\
    \ ON insights_registration_applications(competition_slug, owner_wallet) WHERE status = 'completed'"
  _ <- execute_ connection
    "CREATE UNIQUE INDEX IF NOT EXISTS idx_insights_registration_account_unique\
    \ ON insights_registration_applications(competition_slug, trading_account) WHERE status = 'completed'"
  _ <- execute_ connection
    "CREATE TABLE IF NOT EXISTS insights_registration_sessions (\
    \ session_digest BYTEA PRIMARY KEY,\
    \ application_id UUID NOT NULL UNIQUE REFERENCES insights_registration_applications(registration_id) ON DELETE CASCADE,\
    \ csrf_digest BYTEA NOT NULL, csrf_key_version TEXT NOT NULL, csrf_nonce BYTEA NOT NULL, csrf_ciphertext BYTEA NOT NULL, csrf_tag BYTEA NOT NULL,\
    \ expires_at TIMESTAMPTZ NOT NULL,\
    \ oauth_error_code TEXT,\
    \ oauth_state_digest BYTEA, oauth_expires_at TIMESTAMPTZ, pkce_key_version TEXT, pkce_nonce BYTEA, pkce_ciphertext BYTEA, pkce_tag BYTEA,\
    \ wallet_nonce_digest BYTEA, wallet_owner VARCHAR(42), wallet_expires_at TIMESTAMPTZ,\
    \ wallet_message_key_version TEXT, wallet_message_nonce BYTEA, wallet_message_ciphertext BYTEA, wallet_message_tag BYTEA,\
    \ created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(), updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),\
    \ CHECK (octet_length(session_digest)=32 AND octet_length(csrf_digest)=32 AND octet_length(csrf_nonce)=12 AND octet_length(csrf_tag)=16),\
    \ CHECK (oauth_error_code IS NULL OR oauth_error_code ~ '^[A-Z_]{1,32}$'),\
    \ CHECK (num_nonnulls(oauth_state_digest,oauth_expires_at,pkce_key_version,pkce_nonce,pkce_ciphertext,pkce_tag) IN (0,6)\
    \   AND (oauth_state_digest IS NULL OR octet_length(oauth_state_digest)=32)\
    \   AND (pkce_nonce IS NULL OR octet_length(pkce_nonce)=12) AND (pkce_tag IS NULL OR octet_length(pkce_tag)=16)),\
    \ CHECK (num_nonnulls(wallet_nonce_digest,wallet_owner,wallet_expires_at,wallet_message_key_version,wallet_message_nonce,wallet_message_ciphertext,wallet_message_tag) IN (0,7)\
    \   AND (wallet_owner IS NULL OR wallet_owner ~ '^0x[0-9a-f]{40}$')\
    \   AND (wallet_nonce_digest IS NULL OR octet_length(wallet_nonce_digest)=32)\
    \   AND (wallet_message_nonce IS NULL OR octet_length(wallet_message_nonce)=12)\
    \   AND (wallet_message_tag IS NULL OR octet_length(wallet_message_tag)=16))\
    \ )"
  _ <- execute_ connection
    "ALTER TABLE insights_registration_sessions ADD COLUMN IF NOT EXISTS oauth_error_code TEXT"
  _ <- execute_ connection
    "DO $$ BEGIN IF NOT EXISTS (SELECT 1 FROM pg_constraint WHERE conrelid='insights_registration_sessions'::regclass\
    \ AND conname='insights_registration_sessions_oauth_error_code_check') THEN\
    \ ALTER TABLE insights_registration_sessions ADD CONSTRAINT insights_registration_sessions_oauth_error_code_check\
    \ CHECK (oauth_error_code IS NULL OR oauth_error_code ~ '^[A-Z_]{1,32}$'); END IF; END $$"
  _ <- execute_ connection
    "CREATE TABLE IF NOT EXISTS insights_registration_rate_limits (\
    \ scope_digest BYTEA NOT NULL, window_epoch_minute BIGINT NOT NULL, request_count INTEGER NOT NULL,\
    \ updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(), PRIMARY KEY(scope_digest, window_epoch_minute),\
    \ CHECK (octet_length(scope_digest)=32 AND window_epoch_minute >= 0 AND request_count > 0)\
    \ )"
  _ <- execute_ connection
    "CREATE INDEX IF NOT EXISTS idx_insights_registration_applications_status_created\
    \ ON insights_registration_applications(status, created_at)"
  _ <- execute_ connection
    "CREATE INDEX IF NOT EXISTS idx_insights_registration_follow_attempt_lease\
    \ ON insights_registration_applications(x_follow_attempt_started_at)\
    \ WHERE x_follow_attempt_started_at IS NOT NULL"
  _ <- execute_ connection
    "CREATE INDEX IF NOT EXISTS idx_insights_registration_sessions_expires\
    \ ON insights_registration_sessions(expires_at)"
  _ <- execute_ connection
    "CREATE INDEX IF NOT EXISTS idx_insights_registration_sessions_oauth_expires\
    \ ON insights_registration_sessions(oauth_expires_at) WHERE oauth_expires_at IS NOT NULL"
  _ <- execute_ connection
    "CREATE INDEX IF NOT EXISTS idx_insights_registration_sessions_wallet_expires\
    \ ON insights_registration_sessions(wallet_expires_at) WHERE wallet_expires_at IS NOT NULL"
  _ <- execute_ connection
    "CREATE INDEX IF NOT EXISTS idx_insights_registration_rate_limits_window\
    \ ON insights_registration_rate_limits(window_epoch_minute)"
  pure ()

-- | The first provisioned deployment records only a keyed digest of the
-- reviewed numeric X target ID.  Later deployments may validate but never
-- overwrite it, preventing an environment drift from silently changing which
-- account entrants are asked to follow.
provisionRegistrationCompetitionConfig
  :: Connection
  -> Text
  -> ByteString
  -> Text
  -> IO Bool
provisionRegistrationCompetitionConfig connection competitionSlug targetXUserIdDigest privacyVersion =
  withTransaction connection $ do
    _ <- execute connection
      "INSERT INTO insights_registration_competition_config\
      \ (competition_slug, target_x_user_id_digest, privacy_version)\
      \ SELECT slug, ?, ? FROM insights_competitions WHERE slug = ?\
      \ ON CONFLICT (competition_slug) DO NOTHING"
      (bytea targetXUserIdDigest, privacyVersion, competitionSlug)
    rows <- query connection
      "SELECT EXISTS (SELECT 1 FROM insights_registration_competition_config\
      \ WHERE competition_slug = ? AND target_x_user_id_digest = ? AND privacy_version = ?)"
      (competitionSlug, bytea targetXUserIdDigest, privacyVersion)
    pure $ rows == [Only True]

openRegistrationIfConfigured :: Connection -> Text -> Text -> IO Bool
openRegistrationIfConfigured connection competitionSlug privacyVersion =
  withTransaction connection $ do
    affected <- execute connection
      "UPDATE insights_competitions\
      \ SET registration_open_timestamp = FLOOR(EXTRACT(EPOCH FROM NOW()))::BIGINT,\
      \ privacy_notice_version = ?, updated_at = NOW()\
      \ WHERE slug = ? AND registration_open_timestamp IS NULL AND registration_close_timestamp IS NOT NULL\
      \ AND (privacy_notice_version IS NULL OR privacy_notice_version = ?)\
      \ AND NOW() < TO_TIMESTAMP(registration_close_timestamp) AND finalized = FALSE"
      (privacyVersion, competitionSlug, privacyVersion)
    rows <- query connection
      "SELECT EXISTS (SELECT 1 FROM insights_competitions WHERE slug = ?\
      \ AND registration_open_timestamp IS NOT NULL AND registration_close_timestamp IS NOT NULL\
      \ AND privacy_notice_version = ?\
      \ AND NOW() >= TO_TIMESTAMP(registration_open_timestamp)\
      \ AND NOW() < TO_TIMESTAMP(registration_close_timestamp) AND finalized = FALSE)"
      (competitionSlug, privacyVersion)
    pure $ affected == (1 :: Int64) || rows == [Only True]

getRegistrationCompetition :: Connection -> Text -> IO (Maybe RegistrationCompetition)
getRegistrationCompetition connection competitionSlug = do
  rows <- query connection
    "SELECT slug, chain_id, release_router, usdc_address, release_manifest, start_timestamp, registration_open_timestamp, registration_close_timestamp,\
    \ minimum_x_account_age_days, target_x_handle, rules_version, privacy_notice_version, finalized\
    \ FROM insights_competitions WHERE slug = ? AND registration_close_timestamp IS NOT NULL\
    \ AND minimum_x_account_age_days IS NOT NULL AND target_x_handle IS NOT NULL LIMIT 1"
    (Only competitionSlug)
  pure $ case rows of
    [row] -> Just row
    _ -> Nothing

createRegistrationSession
  :: Connection
  -> Text
  -> Text
  -> ByteString
  -> ByteString
  -> ByteString
  -> EncryptedValue
  -> Integer
  -> IO CreateSessionResult
createRegistrationSession connection competitionSlug applicationId turnstileDigest sessionDigest csrfDigest csrfEncrypted ttlSeconds = do
  result <- try @SqlError $ withTransaction connection $ do
    applications <- query connection
      "INSERT INTO insights_registration_applications\
      \ (registration_id, competition_slug, turnstile_token_digest)\
      \ SELECT ?::uuid, slug, ? FROM insights_competitions\
      \ WHERE slug = ? AND registration_open_timestamp IS NOT NULL\
      \ AND NOW() >= TO_TIMESTAMP(registration_open_timestamp)\
      \ AND NOW() < TO_TIMESTAMP(registration_close_timestamp) AND finalized = FALSE\
      \ RETURNING registration_id::text"
      (applicationId, bytea turnstileDigest, competitionSlug) :: IO [Only Text]
    case applications of
      [Only _] -> do
        _ <- execute connection
          "INSERT INTO insights_registration_sessions\
          \ (session_digest, application_id, csrf_digest, csrf_key_version, csrf_nonce, csrf_ciphertext, csrf_tag, expires_at)\
          \ VALUES (?, ?::uuid, ?, ?, ?, ?, ?, NOW() + (? * INTERVAL '1 second'))"
          ( bytea sessionDigest
          , applicationId
          , bytea csrfDigest
          , evKeyVersion csrfEncrypted
          , bytea $ evNonce csrfEncrypted
          , bytea $ evCiphertext csrfEncrypted
          , bytea $ evTag csrfEncrypted
          , ttlSeconds
          )
        pure True
      _ -> pure False
  case result of
    Right True -> pure SessionCreated
    Right False -> pure SessionRegistrationClosed
    Left sqlError
      | isNamedUniqueViolation
          [ "insights_registration_turnstile_digest_unique"
          , "insights_registration_applications_turnstile_token_digest_key"
          ]
          sqlError -> pure SessionTurnstileReplay
      | otherwise -> throwIO sqlError

getRegistrationSession :: Connection -> ByteString -> IO (Maybe RegistrationSessionRow)
getRegistrationSession connection sessionDigest = do
  rows <- query connection (registrationSessionSelect <> " WHERE s.session_digest = ? AND s.expires_at > NOW() LIMIT 1") (Only $ bytea sessionDigest)
  pure $ case rows of
    [row] -> Just row
    _ -> Nothing

-- | Read the database-time half-open registration window before an expensive
-- provider proof. Every subsequent state transition repeats this predicate,
-- so this is an availability/error-code guard rather than an authorization
-- substitute.
registrationCompletionState :: Connection -> ByteString -> IO RegistrationCompletionState
registrationCompletionState connection sessionDigest = do
  rows <- query connection
    "SELECT a.status, c.registration_open_timestamp IS NOT NULL\
    \ AND NOW() >= TO_TIMESTAMP(c.registration_open_timestamp)\
    \ AND NOW() < TO_TIMESTAMP(c.registration_close_timestamp)\
    \ AND c.finalized=FALSE\
    \ FROM insights_registration_sessions s\
    \ JOIN insights_registration_applications a ON a.registration_id=s.application_id\
    \ JOIN insights_competitions c ON c.slug=a.competition_slug\
    \ WHERE s.session_digest=? AND s.expires_at>NOW()"
    (Only $ bytea sessionDigest) :: IO [(Text, Bool)]
  pure $ case rows of
    [("completed", _)] -> RegistrationCompletionAlreadySucceeded
    [("in_progress", True)] -> RegistrationCompletionOpen
    _ -> RegistrationCompletionClosed

storeOAuthChallenge
  :: Connection
  -> ByteString
  -> ByteString
  -> EncryptedValue
  -> Integer
  -> IO RegistrationMutationResult
storeOAuthChallenge connection sessionDigest stateDigest verifier sessionTtlSeconds = do
  affected <- execute connection
    "UPDATE insights_registration_sessions s SET oauth_error_code = NULL, oauth_state_digest = ?, oauth_expires_at = NOW() + INTERVAL '10 minutes', pkce_key_version = ?,\
    \ pkce_nonce = ?, pkce_ciphertext = ?, pkce_tag = ?, expires_at = NOW() + (? * INTERVAL '1 second'), updated_at = NOW()\
    \ FROM insights_registration_applications a, insights_competitions c\
    \ WHERE s.application_id = a.registration_id AND a.competition_slug = c.slug\
    \ AND s.session_digest = ? AND s.expires_at > NOW() AND a.status = 'in_progress'\
    \ AND a.x_follow_verified_at IS NULL AND a.wallet_verified_at IS NULL AND a.x_follow_attempt_id IS NULL\
    \ AND c.registration_open_timestamp IS NOT NULL\
    \ AND NOW() >= TO_TIMESTAMP(c.registration_open_timestamp)\
    \ AND NOW() < TO_TIMESTAMP(c.registration_close_timestamp) AND c.finalized = FALSE"
    ( bytea stateDigest
    , evKeyVersion verifier
    , bytea $ evNonce verifier
    , bytea $ evCiphertext verifier
    , bytea $ evTag verifier
    , sessionTtlSeconds
    , bytea sessionDigest
    )
  if affected == (1 :: Int64)
    then pure MutationApplied
    else classifySessionMutation connection sessionDigest

consumeOAuthChallenge :: Connection -> ByteString -> ByteString -> IO OAuthChallengeConsumeResult
consumeOAuthChallenge connection sessionDigest stateDigest =
  withTransaction connection $ do
    rows <- query connection
      "SELECT a.registration_id::text, a.competition_slug, s.pkce_key_version, s.pkce_nonce, s.pkce_ciphertext, s.pkce_tag,\
      \ c.registration_open_timestamp IS NOT NULL\
      \   AND NOW() >= TO_TIMESTAMP(c.registration_open_timestamp)\
      \   AND NOW() < TO_TIMESTAMP(c.registration_close_timestamp)\
      \   AND c.finalized = FALSE AS registration_open, s.oauth_expires_at > NOW() AS challenge_unexpired\
      \ FROM insights_registration_sessions s JOIN insights_registration_applications a ON a.registration_id = s.application_id\
      \ JOIN insights_competitions c ON c.slug = a.competition_slug\
      \ WHERE s.session_digest = ? AND s.oauth_state_digest = ? AND s.expires_at > NOW() AND s.oauth_expires_at IS NOT NULL\
      \ AND a.status = 'in_progress' AND s.pkce_key_version IS NOT NULL FOR UPDATE OF s"
      (bytea sessionDigest, bytea stateDigest)
    case rows of
      [row] -> do
        _ <- execute connection
          "UPDATE insights_registration_sessions SET oauth_state_digest = NULL, oauth_expires_at = NULL, pkce_key_version = NULL,\
          \ pkce_nonce = NULL, pkce_ciphertext = NULL, pkce_tag = NULL, updated_at = NOW()\
          \ WHERE session_digest = ?"
          (Only $ bytea sessionDigest)
        pure $
          if ocrChallengeUnexpired row
            then OAuthChallengeConsumed row
            else OAuthChallengeExpired
      _ -> pure OAuthChallengeUnavailable

-- | Persist only a bounded stable callback error on the owning session.  The
-- provider response, OAuth code, state, and identity details are never stored
-- or reflected to the browser.
recordOAuthCallbackError :: Connection -> ByteString -> Text -> IO Bool
recordOAuthCallbackError connection sessionDigest errorCode = do
  affected <- execute connection
    "UPDATE insights_registration_sessions s SET oauth_error_code=?, updated_at=NOW()\
    \ FROM insights_registration_applications a\
    \ WHERE s.application_id=a.registration_id AND s.session_digest=?\
    \ AND s.expires_at>NOW() AND a.status='in_progress'"
    (errorCode, bytea sessionDigest)
  pure $ affected == (1 :: Int64)

-- | Persist verified X identity and refresh CSRF/session expiry without
-- replacing the session digest.  Keeping the post-PKCE session token stable
-- means a committed callback whose 303/Set-Cookie response is lost remains
-- resumable through the owning-session endpoint.
storeXIdentityAndRefreshSession
  :: Connection
  -> ByteString
  -> ByteString
  -> EncryptedValue
  -> Integer
  -> Text
  -> ByteString
  -> EncryptedValue
  -> ByteString
  -> Text
  -> Integer
  -> Text
  -> EncryptedValue
  -> EncryptedValue
  -> IO RegistrationMutationResult
storeXIdentityAndRefreshSession connection sessionDigest nextCsrfDigest nextCsrfEncrypted ttlSeconds applicationId xUserDigest encryptedXUser emailDigest emailMasked xCreatedTimestamp xUsername encryptedEmail encryptedAccessToken =
  withTransaction connection $ do
    affected <- execute connection
      "UPDATE insights_registration_applications a SET\
      \ x_user_id_digest = ?, x_user_id_key_version = ?, x_user_id_nonce = ?, x_user_id_ciphertext = ?, x_user_id_tag = ?,\
      \ email_digest = ?, email_masked = ?, email_key_version = ?, email_nonce = ?, email_ciphertext = ?, email_tag = ?,\
      \ x_username = ?, x_created_timestamp = ?, x_identity_verified_at = NOW(),\
      \ x_access_key_version = ?, x_access_nonce = ?, x_access_ciphertext = ?, x_access_tag = ?, updated_at = NOW()\
      \ FROM insights_competitions c WHERE a.competition_slug = c.slug\
      \ AND a.registration_id = ?::uuid AND a.status = 'in_progress'\
      \ AND a.x_follow_verified_at IS NULL AND a.wallet_verified_at IS NULL AND a.x_follow_attempt_id IS NULL\
      \ AND c.registration_open_timestamp IS NOT NULL\
      \ AND NOW() >= TO_TIMESTAMP(c.registration_open_timestamp)\
      \ AND NOW() < TO_TIMESTAMP(c.registration_close_timestamp) AND c.finalized = FALSE"
      ( bytea xUserDigest
      , evKeyVersion encryptedXUser
      , bytea $ evNonce encryptedXUser
      , bytea $ evCiphertext encryptedXUser
      , bytea $ evTag encryptedXUser
      , bytea emailDigest
      , emailMasked
      , evKeyVersion encryptedEmail
      , bytea $ evNonce encryptedEmail
      , bytea $ evCiphertext encryptedEmail
      , bytea $ evTag encryptedEmail
      , xUsername
      , xCreatedTimestamp
      , evKeyVersion encryptedAccessToken
      , bytea $ evNonce encryptedAccessToken
      , bytea $ evCiphertext encryptedAccessToken
      , bytea $ evTag encryptedAccessToken
      , applicationId
      )
    if affected /= (1 :: Int64)
      then classifySessionMutation connection sessionDigest
      else do
        refreshed <- execute connection
          "UPDATE insights_registration_sessions SET csrf_digest=?, csrf_key_version=?,\
          \ csrf_nonce=?, csrf_ciphertext=?, csrf_tag=?, oauth_error_code=NULL,\
          \ expires_at=NOW() + (? * INTERVAL '1 second'), updated_at=NOW()\
          \ WHERE session_digest=? AND application_id=?::uuid AND expires_at>NOW()"
          ( bytea nextCsrfDigest
          , evKeyVersion nextCsrfEncrypted
          , bytea $ evNonce nextCsrfEncrypted
          , bytea $ evCiphertext nextCsrfEncrypted
          , bytea $ evTag nextCsrfEncrypted
          , ttlSeconds
          , bytea sessionDigest
          , applicationId
          )
        if refreshed == (1 :: Int64)
          then pure MutationApplied
          else fail "Registration OAuth session refresh failed"

claimXFollowMaterial
  :: Connection
  -> ByteString
  -> ByteString
  -> Text
  -> IO XFollowClaimResult
claimXFollowMaterial connection sessionDigest targetXUserIdDigest attemptId =
  withTransaction connection $ do
    rows <- query connection
      "SELECT a.registration_id::text, a.competition_slug,\
      \ a.x_user_id_key_version, a.x_user_id_nonce, a.x_user_id_ciphertext, a.x_user_id_tag,\
      \ a.x_access_key_version, a.x_access_nonce, a.x_access_ciphertext, a.x_access_tag\
      \ FROM insights_registration_sessions s JOIN insights_registration_applications a ON a.registration_id = s.application_id\
      \ JOIN insights_competitions c ON c.slug = a.competition_slug\
      \ JOIN insights_registration_competition_config rc ON rc.competition_slug = a.competition_slug\
      \ WHERE s.session_digest = ? AND s.expires_at > NOW() AND a.status = 'in_progress'\
      \ AND a.x_identity_verified_at IS NOT NULL AND a.x_follow_verified_at IS NULL\
      \ AND (a.x_follow_attempt_id IS NULL OR a.x_follow_attempt_started_at <= NOW() - INTERVAL '2 minutes')\
      \ AND a.x_user_id_key_version IS NOT NULL AND a.x_access_key_version IS NOT NULL\
      \ AND rc.target_x_user_id_digest = ?\
      \ AND c.registration_open_timestamp IS NOT NULL\
      \ AND NOW() >= TO_TIMESTAMP(c.registration_open_timestamp)\
      \ AND NOW() < TO_TIMESTAMP(c.registration_close_timestamp) AND c.finalized = FALSE\
      \ FOR UPDATE OF a"
      (bytea sessionDigest, bytea targetXUserIdDigest)
    case rows of
      [row] -> do
        affected <- execute connection
          "UPDATE insights_registration_applications SET x_follow_attempt_id=?::uuid,\
          \ x_follow_attempt_started_at=NOW(), updated_at=NOW()\
          \ WHERE registration_id=(SELECT application_id FROM insights_registration_sessions WHERE session_digest=?)\
          \ AND status='in_progress' AND x_follow_verified_at IS NULL\
          \ AND (x_follow_attempt_id IS NULL OR x_follow_attempt_started_at <= NOW() - INTERVAL '2 minutes')"
          (attemptId, bytea sessionDigest)
        if affected == (1 :: Int64)
          then pure $ XFollowClaimed row
          else classifyUnavailable
      _ -> classifyUnavailable
  where
    classifyUnavailable = do
      states <- query connection
        "SELECT c.registration_open_timestamp IS NOT NULL\
        \ AND NOW() >= TO_TIMESTAMP(c.registration_open_timestamp)\
        \ AND NOW() < TO_TIMESTAMP(c.registration_close_timestamp)\
        \ AND c.finalized = FALSE\
        \ FROM insights_registration_sessions s\
        \ JOIN insights_registration_applications a ON a.registration_id=s.application_id\
        \ JOIN insights_competitions c ON c.slug=a.competition_slug\
        \ WHERE s.session_digest=? AND s.expires_at>NOW() AND a.status='in_progress'"
        (Only $ bytea sessionDigest) :: IO [Only Bool]
      pure $ case states of
        [Only False] -> XFollowClaimClosed
        _ -> XFollowClaimUnavailable

eraseXProviderSecrets :: Connection -> Text -> IO ()
eraseXProviderSecrets connection applicationId = do
  _ <- execute connection
    "UPDATE insights_registration_applications SET\
    \ x_user_id_key_version = NULL, x_user_id_nonce = NULL, x_user_id_ciphertext = NULL, x_user_id_tag = NULL,\
    \ x_access_key_version = NULL, x_access_nonce = NULL, x_access_ciphertext = NULL, x_access_tag = NULL, updated_at = NOW()\
    \ WHERE registration_id = ?::uuid"
    (Only applicationId)
  pure ()

-- | Release the single-flight verification lease while keeping the encrypted,
-- short-lived OAuth material available for another read-only follow check.
-- This is used when X confirms that the user has not followed the target yet.
releaseXFollowAttempt :: Connection -> Text -> Text -> IO ()
releaseXFollowAttempt connection applicationId attemptId = do
  _ <- execute connection
    "UPDATE insights_registration_applications SET\
    \ x_follow_attempt_id=NULL, x_follow_attempt_started_at=NULL, updated_at=NOW()\
    \ WHERE registration_id=?::uuid AND status='in_progress' AND x_follow_attempt_id=?::uuid\
    \ AND x_follow_verified_at IS NULL"
    (applicationId, attemptId)
  pure ()

-- | A failed provider call may indicate that the short-lived bearer token is
-- unusable. Erase the partial identity so the resumable UI returns to X
-- authorization instead of entering a tokenless dead end. A clean
-- "not-following" response uses 'releaseXFollowAttempt' instead.
resetXIdentityAfterFollowFailure :: Connection -> Text -> Text -> IO ()
resetXIdentityAfterFollowFailure connection applicationId attemptId = do
  _ <- execute connection
    "UPDATE insights_registration_applications SET\
    \ email_key_version=NULL, email_nonce=NULL, email_ciphertext=NULL, email_tag=NULL, email_digest=NULL, email_masked=NULL,\
    \ x_user_id_key_version=NULL, x_user_id_nonce=NULL, x_user_id_ciphertext=NULL, x_user_id_tag=NULL, x_user_id_digest=NULL,\
    \ x_username=NULL, x_created_timestamp=NULL, x_identity_verified_at=NULL,\
    \ x_access_key_version=NULL, x_access_nonce=NULL, x_access_ciphertext=NULL, x_access_tag=NULL,\
    \ x_follow_attempt_id=NULL, x_follow_attempt_started_at=NULL, x_follow_verified_at=NULL, updated_at=NOW()\
    \ WHERE registration_id=?::uuid AND status='in_progress' AND x_follow_attempt_id=?::uuid\
    \ AND x_follow_verified_at IS NULL"
    (applicationId, attemptId)
  pure ()

confirmXFollow :: Connection -> Text -> Text -> IO Bool
confirmXFollow connection applicationId attemptId = do
  affected <- execute connection
    "UPDATE insights_registration_applications a SET x_follow_attempt_id=NULL, x_follow_attempt_started_at=NULL, x_follow_verified_at = NOW(),\
    \ x_user_id_key_version = NULL, x_user_id_nonce = NULL, x_user_id_ciphertext = NULL, x_user_id_tag = NULL,\
    \ x_access_key_version = NULL, x_access_nonce = NULL, x_access_ciphertext = NULL, x_access_tag = NULL, updated_at = NOW()\
    \ FROM insights_competitions c WHERE a.competition_slug = c.slug\
    \ AND a.registration_id = ?::uuid AND a.x_follow_attempt_id=?::uuid\
    \ AND a.status = 'in_progress' AND a.x_identity_verified_at IS NOT NULL AND a.x_follow_verified_at IS NULL\
    \ AND c.registration_open_timestamp IS NOT NULL\
    \ AND NOW() >= TO_TIMESTAMP(c.registration_open_timestamp)\
    \ AND NOW() < TO_TIMESTAMP(c.registration_close_timestamp) AND c.finalized = FALSE"
    (applicationId, attemptId)
  pure $ affected == (1 :: Int64)

storeWalletChallenge
  :: Connection
  -> ByteString
  -> ByteString
  -> Text
  -> Integer
  -> EncryptedValue
  -> IO RegistrationMutationResult
storeWalletChallenge connection sessionDigest nonceDigest ownerWallet expiresTimestamp encryptedMessage = do
  affected <- execute connection
    "UPDATE insights_registration_sessions s SET wallet_nonce_digest = ?, wallet_owner = ?,\
    \ wallet_expires_at = TO_TIMESTAMP(?), wallet_message_key_version = ?, wallet_message_nonce = ?,\
    \ wallet_message_ciphertext = ?, wallet_message_tag = ?, updated_at = NOW()\
    \ FROM insights_registration_applications a, insights_competitions c\
    \ WHERE s.application_id = a.registration_id AND a.competition_slug = c.slug\
    \ AND s.session_digest = ? AND s.expires_at > NOW() AND a.status = 'in_progress'\
    \ AND a.x_follow_verified_at IS NOT NULL AND c.registration_open_timestamp IS NOT NULL\
    \ AND NOW() >= TO_TIMESTAMP(c.registration_open_timestamp)\
    \ AND NOW() < TO_TIMESTAMP(c.registration_close_timestamp) AND c.finalized = FALSE"
    ( bytea nonceDigest
    , normalizeAddress ownerWallet
    , expiresTimestamp
    , evKeyVersion encryptedMessage
    , bytea $ evNonce encryptedMessage
    , bytea $ evCiphertext encryptedMessage
    , bytea $ evTag encryptedMessage
    , bytea sessionDigest
    )
  if affected == (1 :: Int64)
    then pure MutationApplied
    else classifySessionMutation connection sessionDigest

consumeWalletChallenge :: Connection -> ByteString -> Text -> IO (Maybe WalletChallengeRow)
consumeWalletChallenge connection sessionDigest ownerWallet =
  withTransaction connection $ do
    rows <- query connection
      "SELECT a.registration_id::text, a.competition_slug, s.wallet_owner,\
      \ EXTRACT(EPOCH FROM s.wallet_expires_at)::BIGINT, s.wallet_message_key_version,\
      \ s.wallet_message_nonce, s.wallet_message_ciphertext, s.wallet_message_tag,\
      \ c.registration_open_timestamp IS NOT NULL\
      \   AND NOW() >= TO_TIMESTAMP(c.registration_open_timestamp)\
      \   AND NOW() < TO_TIMESTAMP(c.registration_close_timestamp)\
      \   AND c.finalized = FALSE AS registration_open\
      \ FROM insights_registration_sessions s JOIN insights_registration_applications a ON a.registration_id = s.application_id\
      \ JOIN insights_competitions c ON c.slug = a.competition_slug\
      \ WHERE s.session_digest = ? AND s.expires_at > NOW() AND s.wallet_owner = ?\
      \ AND s.wallet_expires_at > NOW() AND s.wallet_nonce_digest IS NOT NULL\
      \ AND a.status = 'in_progress' AND s.wallet_message_key_version IS NOT NULL FOR UPDATE OF s"
      (bytea sessionDigest, normalizeAddress ownerWallet)
    _ <- execute connection
      "UPDATE insights_registration_sessions SET wallet_nonce_digest = NULL, wallet_owner = NULL,\
      \ wallet_expires_at = NULL, wallet_message_key_version = NULL, wallet_message_nonce = NULL,\
      \ wallet_message_ciphertext = NULL, wallet_message_tag = NULL, updated_at = NOW()\
      \ WHERE session_digest = ?"
      (Only $ bytea sessionDigest)
    pure $ case rows of
      [row] -> Just row
      _ -> Nothing

storeVerifiedWallet :: Connection -> Text -> Text -> Text -> Integer -> Text -> IO RegistrationMutationResult
storeVerifiedWallet connection applicationId ownerWallet tradingAccount verificationBlock verificationBlockHash = do
  affected <- execute connection
    "UPDATE insights_registration_applications a SET owner_wallet = ?, trading_account = ?,\
    \ wallet_verification_block = ?, wallet_verification_block_hash = ?, wallet_verified_at = NOW(), updated_at = NOW()\
    \ FROM insights_competitions c\
    \ WHERE a.competition_slug = c.slug AND a.registration_id = ?::uuid\
    \ AND a.status = 'in_progress' AND a.x_follow_verified_at IS NOT NULL\
    \ AND c.registration_open_timestamp IS NOT NULL\
    \ AND NOW() >= TO_TIMESTAMP(c.registration_open_timestamp)\
    \ AND NOW() < TO_TIMESTAMP(c.registration_close_timestamp) AND c.finalized = FALSE"
    ( normalizeAddress ownerWallet
    , normalizeAddress tradingAccount
    , verificationBlock
    , T.toLower verificationBlockHash
    , applicationId
    )
  if affected == (1 :: Int64)
    then pure MutationApplied
    else classifyApplicationMutation connection applicationId

clearVerifiedWallet :: Connection -> Text -> IO Bool
clearVerifiedWallet connection applicationId = do
  affected <- execute connection
    "UPDATE insights_registration_applications a SET owner_wallet=NULL, trading_account=NULL,\
    \ wallet_verification_block=NULL, wallet_verification_block_hash=NULL, wallet_verified_at=NULL, updated_at=NOW()\
    \ FROM insights_competitions c WHERE a.competition_slug=c.slug\
    \ AND a.registration_id=?::uuid AND a.status='in_progress'\
    \ AND c.registration_open_timestamp IS NOT NULL\
    \ AND NOW() >= TO_TIMESTAMP(c.registration_open_timestamp)\
    \ AND NOW() < TO_TIMESTAMP(c.registration_close_timestamp) AND c.finalized=FALSE"
    (Only applicationId)
  pure $ affected == (1 :: Int64)

classifySessionMutation :: Connection -> ByteString -> IO RegistrationMutationResult
classifySessionMutation connection sessionDigest = do
  rows <- query connection
    "SELECT c.registration_open_timestamp IS NOT NULL\
    \ AND NOW() >= TO_TIMESTAMP(c.registration_open_timestamp)\
    \ AND NOW() < TO_TIMESTAMP(c.registration_close_timestamp)\
    \ AND c.finalized=FALSE\
    \ FROM insights_registration_sessions s\
    \ JOIN insights_registration_applications a ON a.registration_id=s.application_id\
    \ JOIN insights_competitions c ON c.slug=a.competition_slug\
    \ WHERE s.session_digest=? AND s.expires_at>NOW() AND a.status='in_progress'"
    (Only $ bytea sessionDigest) :: IO [Only Bool]
  pure $ case rows of
    [Only False] -> MutationClosed
    _ -> MutationIncomplete

classifyApplicationMutation :: Connection -> Text -> IO RegistrationMutationResult
classifyApplicationMutation connection applicationId = do
  rows <- query connection
    "SELECT c.registration_open_timestamp IS NOT NULL\
    \ AND NOW() >= TO_TIMESTAMP(c.registration_open_timestamp)\
    \ AND NOW() < TO_TIMESTAMP(c.registration_close_timestamp)\
    \ AND c.finalized=FALSE\
    \ FROM insights_registration_applications a\
    \ JOIN insights_competitions c ON c.slug=a.competition_slug\
    \ WHERE a.registration_id=?::uuid AND a.status='in_progress'"
    (Only applicationId) :: IO [Only Bool]
  pure $ case rows of
    [Only False] -> MutationClosed
    _ -> MutationIncomplete

registrationRateLimitAllowed
  :: Connection
  -> ByteString
  -> Int
  -> IO Bool
registrationRateLimitAllowed connection scopeDigest maximumRequests = do
  rows <- query connection
    "INSERT INTO insights_registration_rate_limits(scope_digest, window_epoch_minute, request_count)\
    \ VALUES (?, FLOOR(EXTRACT(EPOCH FROM NOW()) / 60)::BIGINT, 1)\
    \ ON CONFLICT(scope_digest, window_epoch_minute) DO UPDATE SET\
    \ request_count = insights_registration_rate_limits.request_count + 1, updated_at = NOW()\
    \ RETURNING request_count"
    (Only $ bytea scopeDigest) :: IO [Only Int]
  pure $ case rows of
    [Only count] -> count <= maximumRequests
    _ -> False

-- | Perform one bounded cleanup pass.  Each statement owns a short transaction
-- and uses indexed expiry order plus SKIP LOCKED so startup and periodic work
-- cannot drain an unbounded abuse backlog or block another rolling task.
cleanupExpiredRegistrationSecrets :: Connection -> IO RegistrationCleanupResult
cleanupExpiredRegistrationSecrets connection = do
  followLeases <- execute_ connection
    "WITH expired AS (\
    \ SELECT registration_id FROM insights_registration_applications\
    \ WHERE status='in_progress' AND x_follow_attempt_started_at <= NOW() - INTERVAL '2 minutes'\
    \ ORDER BY x_follow_attempt_started_at LIMIT 500 FOR UPDATE SKIP LOCKED)\
    \ UPDATE insights_registration_applications a SET\
    \ email_key_version=NULL, email_nonce=NULL, email_ciphertext=NULL, email_tag=NULL, email_digest=NULL, email_masked=NULL,\
    \ x_user_id_key_version=NULL, x_user_id_nonce=NULL, x_user_id_ciphertext=NULL, x_user_id_tag=NULL, x_user_id_digest=NULL,\
    \ x_username=NULL, x_created_timestamp=NULL, x_identity_verified_at=NULL,\
    \ x_access_key_version=NULL, x_access_nonce=NULL, x_access_ciphertext=NULL, x_access_tag=NULL,\
    \ x_follow_attempt_id=NULL, x_follow_attempt_started_at=NULL, updated_at=NOW() FROM expired e\
    \ WHERE a.registration_id=e.registration_id"
  oauthSecrets <- execute_ connection
    "WITH expired AS (\
    \ SELECT session_digest FROM insights_registration_sessions WHERE oauth_expires_at <= NOW()\
    \ ORDER BY oauth_expires_at LIMIT 500 FOR UPDATE SKIP LOCKED)\
    \ UPDATE insights_registration_sessions s SET oauth_state_digest=NULL, oauth_expires_at=NULL,\
    \ pkce_key_version=NULL, pkce_nonce=NULL, pkce_ciphertext=NULL, pkce_tag=NULL, updated_at=NOW()\
    \ FROM expired e WHERE s.session_digest=e.session_digest"
  walletSecrets <- execute_ connection
    "WITH expired AS (\
    \ SELECT session_digest FROM insights_registration_sessions WHERE wallet_expires_at <= NOW()\
    \ ORDER BY wallet_expires_at LIMIT 500 FOR UPDATE SKIP LOCKED)\
    \ UPDATE insights_registration_sessions s SET wallet_nonce_digest=NULL, wallet_owner=NULL, wallet_expires_at=NULL,\
    \ wallet_message_key_version=NULL, wallet_message_nonce=NULL, wallet_message_ciphertext=NULL, wallet_message_tag=NULL, updated_at=NOW()\
    \ FROM expired e WHERE s.session_digest=e.session_digest"
  sessions <- execute_ connection
    "WITH expired AS (\
    \ SELECT session_digest FROM insights_registration_sessions WHERE expires_at <= NOW()\
    \ ORDER BY expires_at LIMIT 500 FOR UPDATE SKIP LOCKED)\
    \ DELETE FROM insights_registration_sessions s USING expired e WHERE s.session_digest=e.session_digest"
  applications <- execute_ connection
    "WITH orphaned AS (\
    \ SELECT a.registration_id FROM insights_registration_applications a\
    \ WHERE a.status='in_progress'\
    \ AND NOT EXISTS (SELECT 1 FROM insights_registration_sessions s WHERE s.application_id=a.registration_id)\
    \ ORDER BY a.created_at LIMIT 500 FOR UPDATE OF a SKIP LOCKED)\
    \ DELETE FROM insights_registration_applications a USING orphaned o WHERE a.registration_id=o.registration_id"
  rateWindows <- execute_ connection
    "WITH expired AS (\
    \ SELECT scope_digest, window_epoch_minute FROM insights_registration_rate_limits\
    \ WHERE window_epoch_minute < FLOOR(EXTRACT(EPOCH FROM NOW()) / 60)::BIGINT - 10\
    \ ORDER BY window_epoch_minute LIMIT 1000 FOR UPDATE SKIP LOCKED)\
    \ DELETE FROM insights_registration_rate_limits r USING expired e\
    \ WHERE r.scope_digest=e.scope_digest AND r.window_epoch_minute=e.window_epoch_minute"
  let counts = [followLeases, oauthSecrets, walletSecrets, sessions, applications, rateWindows]
  pure $
    RegistrationCleanupResult
      { rcrCleanedRecords = sum counts
      , rcrMayHaveMore = any (>= 500) counts
      }

tradingAccountHasReleaseActivity
  :: Connection
  -> Integer
  -> Text
  -> Text
  -> Text
  -> IO Bool
tradingAccountHasReleaseActivity connection chainId releaseRouter usdcAddress tradingAccount = do
  rows <- query connection
    "WITH target(chain_id, release_router, token_address, account) AS (VALUES (?, ?, ?, ?))\
    \ SELECT\
    \ EXISTS (SELECT 1 FROM perps_events e, target t WHERE e.chain_id=t.chain_id AND e.release_router=t.release_router AND e.account=t.account) OR\
    \ EXISTS (SELECT 1 FROM perps_orders o, target t WHERE o.chain_id=t.chain_id AND o.order_router=t.release_router AND o.account=t.account) OR\
    \ EXISTS (SELECT 1 FROM perps_account_activity a, target t WHERE a.chain_id=t.chain_id AND a.release_router=t.release_router AND a.account=t.account) OR\
    \ EXISTS (SELECT 1 FROM perps_usdc_transfers u, target t WHERE u.chain_id=t.chain_id AND u.release_router=t.release_router\
    \   AND u.token_address=t.token_address AND (u.from_address=t.account OR u.to_address=t.account))"
    ( chainId
    , normalizeAddress releaseRouter
    , normalizeAddress usdcAddress
    , normalizeAddress tradingAccount
    )
  pure $ rows == [Only True]

getReleaseIndexerBlock :: Connection -> Integer -> Text -> IO (Maybe Integer)
getReleaseIndexerBlock connection chainId releaseRouter = do
  fmap ricBlockNumber <$> getReleaseIndexerCursor connection chainId releaseRouter

getReleaseIndexerCursor :: Connection -> Integer -> Text -> IO (Maybe ReleaseIndexerCursor)
getReleaseIndexerCursor connection chainId releaseRouter = do
  rows <- query connection
    "SELECT configured_start_block, last_indexed_block, last_indexed_block_hash FROM perps_indexer_state\
    \ WHERE chain_id = ? AND release_router = ? AND indexer_name = ?\
    \ AND configured_start_block IS NOT NULL AND last_indexed_block_hash IS NOT NULL\
    \ ORDER BY updated_at DESC LIMIT 1"
    ( chainId
    , normalizeAddress releaseRouter
    , "perps-history-costs-v1:" <> normalizeAddress releaseRouter :: Text
    )
  pure $ case rows of
    [row] -> Just row
    _ -> Nothing

completeRegistration
  :: Connection
  -> ByteString
  -> Text
  -> Text
  -> Text
  -> Text
  -> Text
  -> Integer
  -> Text
  -> IO CompletionResult
completeRegistration connection sessionDigest requiredPrivacyVersion acceptedRulesVersion acceptedPrivacyVersion expectedOwner expectedAccount completionProofBlock completionProofHash = do
  result <- try @SqlError $ withTransaction connection $ do
    rows <- query connection
      "SELECT a.registration_id::text, a.competition_slug, a.status, a.x_username, a.owner_wallet, a.trading_account,\
      \ a.x_identity_verified_at IS NOT NULL, a.x_follow_verified_at IS NOT NULL, a.wallet_verified_at IS NOT NULL,\
      \ c.rules_version, rc.privacy_version, c.chain_id, c.release_router, c.usdc_address,\
      \ c.registration_open_timestamp IS NOT NULL\
      \   AND NOW() >= TO_TIMESTAMP(c.registration_open_timestamp)\
      \   AND NOW() < TO_TIMESTAMP(c.registration_close_timestamp)\
      \   AND NOT c.finalized AS registration_open\
      \ FROM insights_registration_sessions s\
      \ JOIN insights_registration_applications a ON a.registration_id = s.application_id\
      \ JOIN insights_competitions c ON c.slug = a.competition_slug\
      \ JOIN insights_registration_competition_config rc ON rc.competition_slug = a.competition_slug\
      \ WHERE s.session_digest = ? AND s.expires_at > NOW() FOR UPDATE OF s, a, c"
      (Only $ bytea sessionDigest)
      :: IO [(Text, Text, Text, Maybe Text, Maybe Text, Maybe Text, Bool, Bool, Bool, Text, Text, Integer, Text, Text, Bool)]
    case rows of
      [(applicationId, competitionSlug, status, maybeUsername, maybeOwner, maybeAccount, identityVerified, followVerified, walletVerified, requiredRules, storedPrivacyVersion, chainId, releaseRouter, usdcAddress, isOpen)]
        | status == "completed" -> pure CompletionAlreadySucceeded
        | not isOpen -> pure CompletionClosed
        | acceptedRulesVersion /= requiredRules
            || acceptedPrivacyVersion /= storedPrivacyVersion
            || requiredPrivacyVersion /= storedPrivacyVersion -> pure CompletionIncomplete
        | not identityVerified || not followVerified || not walletVerified -> pure CompletionIncomplete
        | (normalizeAddress <$> maybeOwner) /= Just (normalizeAddress expectedOwner)
            || (normalizeAddress <$> maybeAccount) /= Just (normalizeAddress expectedAccount) ->
            pure CompletionTradingAccountUsed
        | Just username <- maybeUsername
        , Just _owner <- maybeOwner
        , Just account <- maybeAccount -> do
            accountUsed <- tradingAccountHasReleaseActivity connection chainId releaseRouter usdcAddress account
            if accountUsed
              then pure CompletionTradingAccountUsed
              else do
                updated <- execute connection
                  "UPDATE insights_registration_applications SET status = 'completed', rules_version = ?,\
                  \ privacy_version = ?, wallet_verification_block=?, wallet_verification_block_hash=?,\
                  \ completed_at = NOW(), updated_at = NOW()\
                  \ WHERE registration_id = ?::uuid AND status = 'in_progress'"
                  ( acceptedRulesVersion
                  , acceptedPrivacyVersion
                  , completionProofBlock
                  , T.toLower completionProofHash
                  , applicationId
                  )
                if updated /= (1 :: Int64)
                  then fail "Registration application changed during completion"
                  else do
                    _ <- execute connection
                      "INSERT INTO insights_competition_participants\
                      \ (competition_slug, wallet, trader_reference, alias, eligibility_status)\
                      \ VALUES (?, ?, ?, ?, 'pending')"
                      (competitionSlug, normalizeAddress account, applicationId, username)
                    pure CompletionSucceeded
        | otherwise -> pure CompletionIncomplete
      _ -> pure CompletionIncomplete
  case result of
    Right completion -> pure completion
    Left sqlError
      | isNamedUniqueViolation
          [ "idx_insights_registration_email_unique"
          , "idx_insights_registration_x_unique"
          , "idx_insights_registration_owner_unique"
          , "idx_insights_registration_account_unique"
          , "insights_competition_participants_pkey"
          , "idx_insights_participants_trader_reference"
          ]
          sqlError -> pure CompletionDuplicate
      | otherwise -> throwIO sqlError

isNamedUniqueViolation :: [ByteString] -> SqlError -> Bool
isNamedUniqueViolation constraintNames sqlError =
  sqlState sqlError == "23505"
    && any (`BSC.isInfixOf` sqlErrorMsg sqlError) constraintNames

-- | Rotation is explicit and version-selective.  The caller decrypts each old
-- value, re-encrypts it with the active key/AAD, and writes it only while the
-- source version still matches, making retries safe.
listRegistrationEmailsForRotation
  :: Connection
  -> Text
  -> Int
  -> IO [RegistrationEmailRow]
listRegistrationEmailsForRotation connection oldVersion batchSize =
  query connection
    "SELECT registration_id::text, competition_slug, email_key_version, email_nonce, email_ciphertext, email_tag\
    \ FROM insights_registration_applications\
    \ WHERE email_key_version = ? AND email_nonce IS NOT NULL AND email_ciphertext IS NOT NULL AND email_tag IS NOT NULL\
    \ ORDER BY registration_id LIMIT ?"
    (oldVersion, max 1 $ min 1000 batchSize)

countRegistrationEmailsByKeyVersion :: Connection -> Text -> IO Integer
countRegistrationEmailsByKeyVersion connection keyVersion = do
  rows <- query connection
    "SELECT COUNT(*) FROM insights_registration_applications WHERE email_key_version = ?"
    (Only keyVersion)
  pure $ case rows of
    [Only count] -> count
    _ -> 0

-- | Count every live envelope that still references a key version.  Operators
-- use this count-only preflight after email rotation and expiry cleanup; old
-- key material must not be retired until every field is zero.
countRegistrationKeyReferences
  :: Connection
  -> Text
  -> IO RegistrationKeyReferenceCounts
countRegistrationKeyReferences connection keyVersion = do
  rows <- query connection
    "SELECT\
    \ (SELECT COUNT(*) FROM insights_registration_applications WHERE email_key_version=?),\
    \ (SELECT COUNT(*) FROM insights_registration_applications WHERE x_user_id_key_version=?),\
    \ (SELECT COUNT(*) FROM insights_registration_applications WHERE x_access_key_version=?),\
    \ (SELECT COUNT(*) FROM insights_registration_sessions WHERE csrf_key_version=?),\
    \ (SELECT COUNT(*) FROM insights_registration_sessions WHERE pkce_key_version=?),\
    \ (SELECT COUNT(*) FROM insights_registration_sessions WHERE wallet_message_key_version=?)"
    (keyVersion, keyVersion, keyVersion, keyVersion, keyVersion, keyVersion)
    :: IO [(Integer, Integer, Integer, Integer, Integer, Integer)]
  pure $ case rows of
    [(emailCount, xUserCount, xAccessCount, csrfCount, pkceCount, walletCount)] ->
      RegistrationKeyReferenceCounts
        emailCount
        xUserCount
        xAccessCount
        csrfCount
        pkceCount
        walletCount
    _ -> RegistrationKeyReferenceCounts 0 0 0 0 0 0

reencryptRegistrationEmails
  :: Connection
  -> Text
  -> [(Text, EncryptedValue)]
  -> IO Int64
reencryptRegistrationEmails connection oldVersion rows =
  withTransaction connection $ do
    counts <- mapM updateOne rows
    pure $ sum counts
  where
    updateOne (applicationId, encrypted) =
      execute connection
        "UPDATE insights_registration_applications SET email_key_version = ?, email_nonce = ?,\
        \ email_ciphertext = ?, email_tag = ?, updated_at = NOW()\
        \ WHERE registration_id = ?::uuid AND email_key_version = ?"
        ( evKeyVersion encrypted
        , bytea $ evNonce encrypted
        , bytea $ evCiphertext encrypted
        , bytea $ evTag encrypted
        , applicationId
        , oldVersion
        )

registrationSessionSelect :: Query
registrationSessionSelect =
  "SELECT a.registration_id::text, a.competition_slug, a.status,\
  \ EXTRACT(EPOCH FROM s.expires_at)::BIGINT, s.csrf_digest, s.csrf_key_version, s.csrf_nonce, s.csrf_ciphertext, s.csrf_tag,\
  \ s.oauth_error_code, a.email_masked, a.x_username, a.x_identity_verified_at IS NOT NULL, a.x_follow_verified_at IS NOT NULL,\
  \ a.owner_wallet, a.trading_account, a.wallet_verified_at IS NOT NULL, c.rules_version, rc.privacy_version\
  \ FROM insights_registration_sessions s JOIN insights_registration_applications a ON a.registration_id = s.application_id\
  \ JOIN insights_competitions c ON c.slug = a.competition_slug\
  \ JOIN insights_registration_competition_config rc ON rc.competition_slug = a.competition_slug"

normalizeAddress :: Text -> Text
normalizeAddress = T.toLower . T.strip
