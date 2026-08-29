{-# LANGUAGE LambdaCase #-}

module Plether.Insights.RegistrationDatabaseSpec
  ( registrationDatabaseSpec
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
  ( newEmptyMVar
  , putMVar
  , readMVar
  , takeMVar
  )
import Control.Exception (SomeException, bracket, finally, throwIO, try)
import Control.Monad (forM_, replicateM, replicateM_, void)
import qualified Data.ByteString as BS
import Data.Pool (destroyAllResources)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (addUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Database.PostgreSQL.Simple (Connection, Only (..), execute, query, query_, withTransaction)
import Database.PostgreSQL.Simple.Types (Binary (..))
import Plether.Database (DbPool, newDbPool, withDb)
import Plether.Database.Insights (ensureInsightsSchema)
import Plether.Database.Insights.Registration
  ( CompletionResult (..)
  , CreateSessionResult (..)
  , OAuthChallengeConsumeResult (..)
  , OAuthChallengeRow (..)
  , RegistrationCompletionState (..)
  , RegistrationEmailRow (..)
  , RegistrationMutationResult (..)
  , RegistrationSessionRow (..)
  , WalletChallengeRow (..)
  , XFollowClaimResult (..)
  , XFollowMaterialRow (..)
  , claimXFollowMaterial
  , cleanupExpiredRegistrationSecrets
  , completeRegistration
  , confirmXFollow
  , consumeOAuthChallenge
  , consumeWalletChallenge
  , createRegistrationSession
  , ensureRegistrationSchema
  , getRegistrationSession
  , listRegistrationEmailsForRotation
  , openRegistrationIfConfigured
  , provisionRegistrationCompetitionConfig
  , releaseXFollowAttempt
  , recordOAuthCallbackError
  , reencryptRegistrationEmails
  , registrationRateLimitAllowed
  , registrationCompletionState
  , storeOAuthChallenge
  , storeVerifiedWallet
  , storeWalletChallenge
  , storeXIdentityAndRefreshSession
  )
import Plether.Database.Schema (ensurePerpsHistorySchema)
import Plether.Insights.Competition
  ( CompetitionReleaseManifest (..)
  , CompetitionRules (..)
  , september2026Competition
  )
import Plether.Insights.Registration.Crypto (EncryptedValue (..))
import Test.Hspec

bytea :: BS.ByteString -> Binary BS.ByteString
bytea = Binary

registrationDatabaseSpec :: Text -> Spec
registrationDatabaseSpec databaseUrl =
  describe "Insights registration PostgreSQL completion" $ do
    it "keeps the current session authoritative after a lost completion response" $
      withRegistrationDatabase databaseUrl $ \pool -> do
        rules <- prepareVerifiedFixture pool
        withDb pool $ \conn -> do
          first <- completeWithRules conn rules
          first `shouldBe` CompletionSucceeded

          registrationCompletionState conn sessionDigest
            `shouldReturn` RegistrationCompletionAlreadySucceeded

          -- Model a committed response that never reaches the browser. The same
          -- pre-existing cookie must remain usable for both a retry and status.
          second <- completeWithRules conn rules
          third <- completeWithRules conn rules
          second `shouldBe` CompletionAlreadySucceeded
          third `shouldBe` CompletionAlreadySucceeded
        assertTerminalFixture pool rules

    it "serializes concurrent completion attempts into one roster entry" $
      withRegistrationDatabase databaseUrl $ \pool -> do
        rules <- prepareVerifiedFixture pool
        ready <- newEmptyMVar
        start <- newEmptyMVar
        outcomes <- newEmptyMVar
        replicateM_ 2 $ void $ forkIO $ do
          outcome <- try $ withDb pool $ \conn -> do
            putMVar ready ()
            readMVar start
            completeWithRules conn rules
          putMVar outcomes (outcome :: Either SomeException CompletionResult)
        replicateM_ 2 $ takeMVar ready
        putMVar start ()
        results <- replicateM 2 (takeMVar outcomes) >>= mapM (either throwIO pure)
        length (filter (== CompletionSucceeded) results) `shouldBe` 1
        length (filter (== CompletionAlreadySucceeded) results) `shouldBe` 1
        assertTerminalFixture pool rules

    it "cannot complete a concurrently replaced wallet with the stale external proof" $
      withRegistrationDatabase databaseUrl $ \pool -> do
        rules <- prepareVerifiedFixture pool
        writerReady <- newEmptyMVar
        releaseWriter <- newEmptyMVar
        writerDone <- newEmptyMVar
        completionStarted <- newEmptyMVar
        completionDone <- newEmptyMVar
        void $ forkIO $ do
          result <- try $ withDb pool $ \conn -> withTransaction conn $ do
            void $ execute conn
              "UPDATE insights_registration_applications SET owner_wallet=?,trading_account=?,updated_at=NOW() WHERE registration_id=?::uuid"
              (candidateOwner1, candidateAccount1, registrationId)
            putMVar writerReady ()
            takeMVar releaseWriter
          putMVar writerDone (result :: Either SomeException ())
        takeMVar writerReady
        void $ forkIO $ do
          result <- try $ withDb pool $ \conn -> do
            putMVar completionStarted ()
            completeWithRules conn rules
          putMVar completionDone (result :: Either SomeException CompletionResult)
        takeMVar completionStarted
        putMVar releaseWriter ()
        takeMVar writerDone >>= either throwIO pure
        outcome <- takeMVar completionDone >>= either throwIO pure
        outcome `shouldBe` CompletionWalletProofChanged
        withDb pool $ \conn -> do
          state <- query conn
            "SELECT status,owner_wallet,trading_account FROM insights_registration_applications WHERE registration_id=?::uuid"
            (Only registrationId) :: IO [(Text, Text, Text)]
          state `shouldBe` [("in_progress", candidateOwner1, candidateAccount1)]
          participants <- query conn
            "SELECT COUNT(*) FROM insights_competition_participants WHERE competition_slug=?"
            (Only $ crSlug rules) :: IO [Only Integer]
          participants `shouldBe` [Only 0]

    it "genericizes completed email, X ID, owner, and Trading Account conflicts per competition" $
      withRegistrationDatabase databaseUrl $ \pool -> do
        rules <- prepareVerifiedFixture pool
        withDb pool $ \conn -> do
          completeWithRules conn rules `shouldReturn` CompletionSucceeded
          let candidates =
                [ ("00000000-0000-4000-8000-000000000910", BS.replicate 32 110, BS.replicate 32 120, digestC, BS.replicate 32 130, candidateOwner1, candidateAccount1)
                , ("00000000-0000-4000-8000-000000000911", BS.replicate 32 111, BS.replicate 32 121, BS.replicate 32 131, digestD, candidateOwner2, candidateAccount2)
                , ("00000000-0000-4000-8000-000000000912", BS.replicate 32 112, BS.replicate 32 122, BS.replicate 32 132, BS.replicate 32 142, ownerWallet, candidateAccount3)
                , ("00000000-0000-4000-8000-000000000913", BS.replicate 32 113, BS.replicate 32 123, BS.replicate 32 133, BS.replicate 32 143, candidateOwner4, tradingAccount)
                ]
          forM_ candidates $ \(applicationId, candidateSession, turnstileDigest, emailDigest, xDigest, owner, account) -> do
            seedVerifiedCandidate
              conn
              (crSlug rules)
              applicationId
              candidateSession
              turnstileDigest
              emailDigest
              xDigest
              owner
              account
            completeRegistration
              conn candidateSession privacyVersion (crRulesVersion rules) privacyVersion
              owner account
              completionBlock completionHash
              `shouldReturn` CompletionDuplicate
          participants <- query conn
            "SELECT COUNT(*) FROM insights_competition_participants WHERE competition_slug=?"
            (Only $ crSlug rules) :: IO [Only Integer]
          participants `shouldBe` [Only 1]

    it "consumes OAuth state once, distinguishes exact-state expiry, and persists only a stable error" $
      withRegistrationDatabase databaseUrl $ \pool -> do
        _ <- prepareBareFixture pool
        withDb pool $ \conn -> do
          now <- floor <$> getPOSIXTime
          void $ execute conn
            "UPDATE insights_registration_sessions SET expires_at=NOW()+INTERVAL '1 second' WHERE session_digest=?"
            (Only $ bytea sessionDigest)
          storeOAuthChallenge conn sessionDigest oauthStateDigest envelope 1_800
            `shouldReturn` MutationApplied
          refreshed <- getRegistrationSession conn sessionDigest
          fmap rsrSessionExpiresTimestamp refreshed `shouldSatisfy` maybe False (> now + 1_700)
          first <- consumeOAuthChallenge conn sessionDigest oauthStateDigest
          first `shouldSatisfy` \case
            OAuthChallengeConsumed challenge -> ocrChallengeUnexpired challenge
            _ -> False
          consumeOAuthChallenge conn sessionDigest oauthStateDigest
            `shouldReturn` OAuthChallengeUnavailable

          storeOAuthChallenge conn sessionDigest oauthStateDigest envelope 1_800
            `shouldReturn` MutationApplied
          void $ execute conn
            "UPDATE insights_registration_sessions SET oauth_expires_at=NOW()-INTERVAL '1 millisecond' WHERE session_digest=?"
            (Only $ bytea sessionDigest)
          consumeOAuthChallenge conn sessionDigest oauthStateDigest
            `shouldReturn` OAuthChallengeExpired
          recordOAuthCallbackError conn sessionDigest "EXPIRED_CHALLENGE"
            `shouldReturn` True
          current <- getRegistrationSession conn sessionDigest
          fmap rsrOauthErrorCode current `shouldBe` Just (Just "EXPIRED_CHALLENGE")

    it "keeps the same session authoritative when an OAuth success response is lost" $
      withRegistrationDatabase databaseUrl $ \pool -> do
        _ <- prepareBareFixture pool
        withDb pool $ \conn -> do
          recordOAuthCallbackError conn sessionDigest "PROVIDER_UNAVAILABLE"
            `shouldReturn` True
          storeIdentityFixture conn `shouldReturn` MutationApplied

          -- The callback may commit and then lose its 303/Set-Cookie response.
          -- The original cookie digest remains authoritative and exposes the
          -- completed X step (and refreshed CSRF) on every resumed status read.
          first <- getRegistrationSession conn sessionDigest
          second <- getRegistrationSession conn sessionDigest
          fmap rsrXIdentityVerified first `shouldBe` Just True
          fmap rsrXIdentityVerified second `shouldBe` Just True
          fmap rsrCsrfDigest first `shouldBe` Just refreshedCsrfDigest
          fmap rsrOauthErrorCode first `shouldBe` Just Nothing

    it "releases a negative follow-check lease, erases bearer envelopes on confirmation, and recovers a crashed lease by forcing reauthorization" $ do
      withRegistrationDatabase databaseUrl $ \pool -> do
        _ <- prepareBareFixture pool
        withDb pool $ \conn -> do
          storeIdentityFixture conn `shouldReturn` MutationApplied
          claimed <- claimXFollowMaterial conn sessionDigest digestA firstFollowAttemptId
          claimed `shouldSatisfy` \case
            XFollowClaimed _ -> True
            _ -> False
          claimXFollowMaterial conn sessionDigest digestA secondFollowAttemptId
            `shouldReturn` XFollowClaimUnavailable
          releaseXFollowAttempt conn registrationId firstFollowAttemptId
          retried <- claimXFollowMaterial conn sessionDigest digestA secondFollowAttemptId
          retried `shouldSatisfy` \case
            XFollowClaimed _ -> True
            _ -> False
          confirmXFollow conn registrationId secondFollowAttemptId `shouldReturn` True
          confirmXFollow conn registrationId secondFollowAttemptId `shouldReturn` False
          confirmed <- getRegistrationSession conn sessionDigest
          fmap rsrXFollowVerified confirmed `shouldBe` Just True
          erased <- query conn
            "SELECT num_nonnulls(x_user_id_key_version,x_user_id_nonce,x_user_id_ciphertext,x_user_id_tag,\
            \ x_access_key_version,x_access_nonce,x_access_ciphertext,x_access_tag)=0\
            \ FROM insights_registration_applications WHERE registration_id=?::uuid"
            (Only registrationId) :: IO [Only Bool]
          erased `shouldBe` [Only True]

      -- A separate fixture models a worker crash after the claim. Cleanup
      -- must delete the temporary bearer and provisional identity, not make
      -- the claimed token reusable.
      withRegistrationDatabase databaseUrl $ \pool -> do
        _ <- prepareBareFixture pool
        withDb pool $ \conn -> do
          storeIdentityFixture conn `shouldReturn` MutationApplied
          claimed <- claimXFollowMaterial conn sessionDigest digestA firstFollowAttemptId
          claimed `shouldSatisfy` \case
            XFollowClaimed _ -> True
            _ -> False
          void $ execute conn
            "UPDATE insights_registration_applications SET x_follow_attempt_started_at=NOW()-INTERVAL '3 minutes' WHERE registration_id=?::uuid"
            (Only registrationId)
          _ <- cleanupExpiredRegistrationSecrets conn
          recovered <- getRegistrationSession conn sessionDigest
          fmap rsrXIdentityVerified recovered `shouldBe` Just False
          fmap rsrEmailMasked recovered `shouldBe` Just Nothing
          claimXFollowMaterial conn sessionDigest digestA secondFollowAttemptId
            `shouldReturn` XFollowClaimUnavailable
          storeOAuthChallenge conn sessionDigest oauthStateDigest envelope 1_800
            `shouldReturn` MutationApplied

    it "consumes wallet challenges once, rejects expiry, and rate-limits atomically within one window" $
      withRegistrationDatabase databaseUrl $ \pool -> do
        _ <- prepareBareFixture pool
        withDb pool $ \conn -> do
          storeIdentityFixture conn `shouldReturn` MutationApplied
          claimed <- claimXFollowMaterial conn sessionDigest digestA firstFollowAttemptId
          claimed `shouldSatisfy` \case
            XFollowClaimed _ -> True
            _ -> False
          confirmXFollow conn registrationId firstFollowAttemptId `shouldReturn` True
          now <- floor <$> getPOSIXTime
          storeWalletChallenge conn sessionDigest walletNonceDigest ownerWallet (now + 300) envelope
            `shouldReturn` MutationApplied
          first <- consumeWalletChallenge conn sessionDigest ownerWallet
          first `shouldSatisfy` maybe False (const True)
          consumeWalletChallenge conn sessionDigest ownerWallet `shouldReturn` Nothing

          storeWalletChallenge conn sessionDigest walletNonceDigest ownerWallet (now + 300) envelope
            `shouldReturn` MutationApplied
          void $ execute conn
            "UPDATE insights_registration_sessions SET wallet_expires_at=NOW()-INTERVAL '1 millisecond' WHERE session_digest=?"
            (Only $ bytea sessionDigest)
          consumeWalletChallenge conn sessionDigest ownerWallet `shouldReturn` Nothing

          registrationRateLimitAllowed conn rateScopeDigest 2 `shouldReturn` True
          registrationRateLimitAllowed conn rateScopeDigest 2 `shouldReturn` True
          registrationRateLimitAllowed conn rateScopeDigest 2 `shouldReturn` False

    it "round-trips high-bit BYTEA values through every registration state boundary" $
      withRegistrationDatabase databaseUrl $ \pool -> do
        rules <- prepareCompetitionFixture pool highTargetXUserIdDigest
        withDb pool $ \conn -> do
          createRegistrationSession
            conn
            (crSlug rules)
            highApplicationId
            highTurnstileDigest
            highSessionDigest
            highCsrfDigest
            highEnvelope
            3_600
            `shouldReturn` SessionCreated

          initial <- getRegistrationSession conn highSessionDigest
          fmap rsrCsrfDigest initial `shouldBe` Just highCsrfDigest
          fmap rsrCsrfEncrypted initial `shouldBe` Just highEnvelope

          storeOAuthChallenge conn highSessionDigest highOauthStateDigest highEnvelope 3_600
            `shouldReturn` MutationApplied
          consumeOAuthChallenge conn highSessionDigest highOauthStateDigest >>= \case
            OAuthChallengeConsumed challenge ->
              ocrPkceVerifier challenge `shouldBe` highEnvelope
            result -> expectationFailure $ "expected high-bit OAuth challenge, got " <> show result

          storeXIdentityAndRefreshSession
            conn
            highSessionDigest
            highRefreshedCsrfDigest
            highEnvelope
            3_600
            highApplicationId
            highXUserIdDigest
            highEnvelope
            highEmailDigest
            "h***@example.test"
            0
            "high_bit_trader"
            highEnvelope
            highEnvelope
            `shouldReturn` MutationApplied

          identity <- getRegistrationSession conn highSessionDigest
          fmap rsrCsrfDigest identity `shouldBe` Just highRefreshedCsrfDigest
          fmap rsrCsrfEncrypted identity `shouldBe` Just highEnvelope

          claimXFollowMaterial
            conn highSessionDigest highTargetXUserIdDigest highFollowAttemptId
            >>= \case
              XFollowClaimed material -> do
                xfmrXUserId material `shouldBe` highEnvelope
                xfmrAccessToken material `shouldBe` highEnvelope
              result -> expectationFailure $ "expected high-bit follow material, got " <> show result
          confirmXFollow conn highApplicationId highFollowAttemptId `shouldReturn` True

          now <- floor <$> getPOSIXTime
          storeWalletChallenge
            conn highSessionDigest highWalletNonceDigest highOwnerWallet (now + 300) highEnvelope
            `shouldReturn` MutationApplied
          walletChallenge <- consumeWalletChallenge conn highSessionDigest highOwnerWallet
          fmap wchrMessage walletChallenge `shouldBe` Just highEnvelope

          storeVerifiedWallet
            conn highApplicationId highOwnerWallet highTradingAccount verificationBlock verificationHash
            `shouldReturn` MutationApplied

          storedDigests <- query conn
            "SELECT x_user_id_digest,email_digest FROM insights_registration_applications WHERE registration_id=?::uuid"
            (Only highApplicationId) :: IO [(BS.ByteString, BS.ByteString)]
          storedDigests `shouldBe` [(highXUserIdDigest, highEmailDigest)]

          reencryptRegistrationEmails conn "v1" [(highApplicationId, rotatedHighEnvelope)]
            `shouldReturn` 1
          rotated <- listRegistrationEmailsForRotation conn "v2" 10
          map (\row -> (rerApplicationId row, rerEncryptedEmail row)) rotated
            `shouldContain` [(highApplicationId, rotatedHighEnvelope)]

          registrationRateLimitAllowed conn highRateScopeDigest 1 `shouldReturn` True
          registrationRateLimitAllowed conn highRateScopeDigest 1 `shouldReturn` False

          completeRegistration
            conn highSessionDigest privacyVersion (crRulesVersion rules) privacyVersion
            highOwnerWallet highTradingAccount completionBlock completionHash
            `shouldReturn` CompletionSucceeded
          completed <- getRegistrationSession conn highSessionDigest
          fmap rsrStatus completed `shouldBe` Just "completed"

          createRegistrationSession
            conn
            (crSlug rules)
            highReplayApplicationId
            highTurnstileDigest
            (BS.replicate 32 0xf8)
            (BS.replicate 32 0xf9)
            highEnvelope
            3_600
            `shouldReturn` SessionTurnstileReplay

prepareVerifiedFixture :: DbPool -> IO CompetitionRules
prepareVerifiedFixture pool = do
  rules <- prepareBareFixture pool
  withDb pool seedVerifiedIdentity
  pure rules

prepareBareFixture :: DbPool -> IO CompetitionRules
prepareBareFixture pool = do
  rules <- prepareCompetitionFixture pool digestA
  withDb pool $ \conn -> seedBareRegistration conn $ crSlug rules
  pure rules

prepareCompetitionFixture :: DbPool -> BS.ByteString -> IO CompetitionRules
prepareCompetitionFixture pool targetXUserIdDigest = withDb pool $ \conn -> do
  rules <- currentRegistrationRules
  let slug = crSlug rules
      manifest = fixtureManifest slug
  ensurePerpsHistorySchema conn
  ensureInsightsSchema
    conn rules fixtureChain fixtureRouter fixtureUsdc fixtureClearinghouse fixtureLens manifest
  ensureRegistrationSchema conn
  provisionRegistrationCompetitionConfig conn slug targetXUserIdDigest privacyVersion `shouldReturn` True
  openRegistrationIfConfigured conn slug privacyVersion `shouldReturn` True
  pure rules

storeIdentityFixture :: Connection -> IO RegistrationMutationResult
storeIdentityFixture conn =
  storeXIdentityAndRefreshSession
    conn
    sessionDigest
    refreshedCsrfDigest
    envelope
    3_600
    registrationId
    digestD
    envelope
    digestC
    "t***@example.test"
    0
    "fixture_trader"
    envelope
    envelope

completeWithRules :: Connection -> CompetitionRules -> IO CompletionResult
completeWithRules conn rules =
  completeRegistration
    conn sessionDigest privacyVersion (crRulesVersion rules) privacyVersion
    ownerWallet tradingAccount
    completionBlock completionHash

assertTerminalFixture :: DbPool -> CompetitionRules -> IO ()
assertTerminalFixture pool rules = withDb pool $ \conn -> do
  let slug = crSlug rules
  current <- getRegistrationSession conn sessionDigest
  fmap rsrStatus current `shouldBe` Just "completed"
  participants <- query conn
    "SELECT COUNT(*) FROM insights_competition_participants WHERE competition_slug = ? AND trader_reference = ?"
    (slug, registrationId) :: IO [Only Integer]
  participants `shouldBe` [Only 1]

withRegistrationDatabase :: Text -> (DbPool -> IO a) -> IO a
withRegistrationDatabase databaseUrl action =
  bracket (newDbPool databaseUrl) destroyAllResources $ \pool -> do
    assertDedicatedDatabase pool
    cleanupFixture pool
    action pool `finally` cleanupFixture pool

assertDedicatedDatabase :: DbPool -> IO ()
assertDedicatedDatabase pool = withDb pool $ \conn -> do
  names <- query_ conn "SELECT current_database()" :: IO [Only Text]
  case names of
    [Only name]
      | "critical_path" `T.isInfixOf` T.toLower name -> pure ()
    _ -> fail "Registration integration tests require a dedicated critical_path PostgreSQL database"

cleanupFixture :: DbPool -> IO ()
cleanupFixture pool = withDb pool $ \conn -> do
  tables <- query_ conn
    "SELECT TO_REGCLASS('public.insights_competitions') IS NOT NULL"
    :: IO [Only Bool]
  case tables of
    [Only True] ->
      void $ execute conn "DELETE FROM insights_competitions WHERE slug = ?" (Only fixtureSlug)
    _ -> pure ()
  rateTable <- query_ conn
    "SELECT TO_REGCLASS('public.insights_registration_rate_limits') IS NOT NULL"
    :: IO [Only Bool]
  case rateTable of
    [Only True] ->
      void $ execute conn
        "DELETE FROM insights_registration_rate_limits WHERE scope_digest IN (?, ?)"
        (bytea rateScopeDigest, bytea highRateScopeDigest)
    _ -> pure ()

currentRegistrationRules :: IO CompetitionRules
currentRegistrationRules = do
  now <- getCurrentTime
  pure
    september2026Competition
      { crSlug = fixtureSlug
      , crName = "Registration idempotency integration fixture"
      , crStartAt = addUTCTime 86_400 now
      , crNewRiskCutoffAt = addUTCTime 172_800 now
      , crScoreCutoffAt = addUTCTime 172_800 now
      , crResultsAt = addUTCTime 259_200 now
      , crPaymentDeadlineAt = addUTCTime 604_800 now
      , crRegistrationClosesAt = Just $ addUTCTime 3_600 now
      , crRulesVersion = "registration-idempotency-v1"
      }

seedBareRegistration :: Connection -> Text -> IO ()
seedBareRegistration conn slug = do
  void $ execute conn
    "INSERT INTO insights_registration_applications\
    \ (registration_id, competition_slug, turnstile_token_digest,\
    \ email_key_version, email_nonce, email_ciphertext, email_tag, email_digest, email_masked)\
    \ VALUES (?::uuid, ?, ?, ?, ?, ?, ?, ?, ?)"
    ( registrationId
    , slug
    , bytea digestB
    , "v1" :: Text
    , bytea nonce
    , bytea ciphertext
    , bytea tag
    , bytea digestC
    , "t***@example.test" :: Text
    )
  void $ execute conn
    "INSERT INTO insights_registration_sessions\
    \ (session_digest, application_id, csrf_digest, csrf_key_version, csrf_nonce, csrf_ciphertext, csrf_tag, expires_at)\
    \ VALUES (?, ?::uuid, ?, ?, ?, ?, ?, NOW() + INTERVAL '1 hour')"
    ( bytea sessionDigest
    , registrationId
    , bytea digestE
    , "v1" :: Text
    , bytea nonce
    , bytea ciphertext
    , bytea tag
    )

seedVerifiedIdentity :: Connection -> IO ()
seedVerifiedIdentity conn =
  void $ execute conn
    "UPDATE insights_registration_applications SET\
    \ x_user_id_digest=?, x_username=?, x_created_timestamp=0,\
    \ x_identity_verified_at=NOW(), x_follow_verified_at=NOW(),\
    \ owner_wallet=?, trading_account=?, wallet_verification_block=?,\
    \ wallet_verification_block_hash=?, wallet_verified_at=NOW()\
    \ WHERE registration_id=?::uuid"
    ( bytea digestD
    , "fixture_trader" :: Text
    , ownerWallet
    , tradingAccount
    , verificationBlock
    , verificationHash
    , registrationId
    )

seedVerifiedCandidate
  :: Connection
  -> Text
  -> Text
  -> BS.ByteString
  -> BS.ByteString
  -> BS.ByteString
  -> BS.ByteString
  -> Text
  -> Text
  -> IO ()
seedVerifiedCandidate conn slug applicationId candidateSession turnstileDigest emailDigest xDigest owner account = do
  void $ execute conn
    "INSERT INTO insights_registration_applications\
    \ (registration_id,competition_slug,turnstile_token_digest,\
    \ email_key_version,email_nonce,email_ciphertext,email_tag,email_digest,email_masked,\
    \ x_user_id_digest,x_username,x_created_timestamp,x_identity_verified_at,x_follow_verified_at,\
    \ owner_wallet,trading_account,wallet_verification_block,wallet_verification_block_hash,wallet_verified_at)\
    \ VALUES (?::uuid,?,?, ?,?,?,?,?,?, ?,?,0,NOW(),NOW(), ?,?,?,?,NOW())"
    ( applicationId
    , slug
    , bytea turnstileDigest
    , "v1" :: Text
    , bytea nonce
    , bytea ciphertext
    , bytea tag
    , bytea emailDigest
    , "d***@example.test" :: Text
    , bytea xDigest
    , "duplicate_fixture" :: Text
    , owner
    , account
    , verificationBlock
    , verificationHash
    )
  void $ execute conn
    "INSERT INTO insights_registration_sessions\
    \ (session_digest,application_id,csrf_digest,csrf_key_version,csrf_nonce,csrf_ciphertext,csrf_tag,expires_at)\
    \ VALUES (?,?::uuid,?,'v1',?,?,?,NOW()+INTERVAL '1 hour')"
    ( bytea candidateSession
    , applicationId
    , bytea $ BS.map (+ 1) candidateSession
    , bytea nonce
    , bytea ciphertext
    , bytea tag
    )

fixtureManifest :: Text -> CompetitionReleaseManifest
fixtureManifest slug =
  CompetitionReleaseManifest
    { crmReleaseId = slug
    , crmChainId = fixtureChain
    , crmUsdc = fixtureUsdc
    , crmOrderRouter = fixtureRouter
    , crmMarginClearinghouse = fixtureClearinghouse
    , crmAccountLens = fixtureLens
    , crmCfdEngine = "0xd100000000000000000000000000000000000001"
    , crmCfdEngineLens = "0xd200000000000000000000000000000000000002"
    , crmSettlementSidecar = "0xd300000000000000000000000000000000000003"
    , crmPletherOracle = "0xd400000000000000000000000000000000000004"
    , crmIndexerStartBlock = 1
    }

fixtureSlug, fixtureRouter, fixtureUsdc, fixtureClearinghouse, fixtureLens :: Text
fixtureSlug = "registration-idempotency-integration"
fixtureRouter = "0xe100000000000000000000000000000000000001"
fixtureUsdc = "0xe200000000000000000000000000000000000002"
fixtureClearinghouse = "0xe300000000000000000000000000000000000003"
fixtureLens = "0xe400000000000000000000000000000000000004"

ownerWallet, tradingAccount :: Text
ownerWallet = "0xf100000000000000000000000000000000000001"
tradingAccount = "0xf200000000000000000000000000000000000002"

candidateOwner1, candidateOwner2, candidateOwner4 :: Text
candidateOwner1 = "0xf100000000000000000000000000000000000010"
candidateOwner2 = "0xf100000000000000000000000000000000000020"
candidateOwner4 = "0xf100000000000000000000000000000000000040"

candidateAccount1, candidateAccount2, candidateAccount3 :: Text
candidateAccount1 = "0xf200000000000000000000000000000000000010"
candidateAccount2 = "0xf200000000000000000000000000000000000020"
candidateAccount3 = "0xf200000000000000000000000000000000000030"

fixtureChain, verificationBlock, completionBlock :: Integer
fixtureChain = 421_614
verificationBlock = 100
completionBlock = 101

verificationHash, completionHash :: Text
verificationHash = "0x" <> T.replicate 64 "a"
completionHash = "0x" <> T.replicate 64 "b"

registrationId :: Text
registrationId = "00000000-0000-4000-8000-000000000901"

firstFollowAttemptId, secondFollowAttemptId :: Text
firstFollowAttemptId = "00000000-0000-4000-8000-000000000902"
secondFollowAttemptId = "00000000-0000-4000-8000-000000000903"

privacyVersion :: Text
privacyVersion = "registration-privacy-v1"

sessionDigest, digestA, digestB, digestC, digestD, digestE, oauthStateDigest, refreshedCsrfDigest, walletNonceDigest, rateScopeDigest, nonce, ciphertext, tag :: BS.ByteString
sessionDigest = BS.replicate 32 1
digestA = BS.replicate 32 2
digestB = BS.replicate 32 3
digestC = BS.replicate 32 4
digestD = BS.replicate 32 5
digestE = BS.replicate 32 6
oauthStateDigest = BS.replicate 32 10
refreshedCsrfDigest = BS.replicate 32 11
walletNonceDigest = BS.replicate 32 12
rateScopeDigest = BS.replicate 32 13
nonce = BS.replicate 12 7
ciphertext = BS.replicate 16 8
tag = BS.replicate 16 9

envelope :: EncryptedValue
envelope = EncryptedValue "v1" nonce ciphertext tag

highApplicationId, highReplayApplicationId, highFollowAttemptId :: Text
highApplicationId = "00000000-0000-4000-8000-000000000950"
highReplayApplicationId = "00000000-0000-4000-8000-000000000951"
highFollowAttemptId = "00000000-0000-4000-8000-000000000952"

highOwnerWallet, highTradingAccount :: Text
highOwnerWallet = "0xf100000000000000000000000000000000000050"
highTradingAccount = "0xf200000000000000000000000000000000000050"

highTurnstileDigest, highSessionDigest, highCsrfDigest, highOauthStateDigest, highRefreshedCsrfDigest, highXUserIdDigest, highEmailDigest, highTargetXUserIdDigest, highWalletNonceDigest, highRateScopeDigest :: BS.ByteString
highTurnstileDigest = BS.replicate 32 0x80
highSessionDigest = BS.replicate 32 0x81
highCsrfDigest = BS.replicate 32 0x82
highOauthStateDigest = BS.replicate 32 0x83
highRefreshedCsrfDigest = BS.replicate 32 0x84
highXUserIdDigest = BS.replicate 32 0x85
highEmailDigest = BS.replicate 32 0x86
highTargetXUserIdDigest = BS.replicate 32 0x87
highWalletNonceDigest = BS.replicate 32 0x88
highRateScopeDigest = BS.replicate 32 0x89

highEnvelope, rotatedHighEnvelope :: EncryptedValue
highEnvelope =
  EncryptedValue
    "v1"
    (BS.replicate 12 0xe1)
    (BS.replicate 32 0xf2)
    (BS.replicate 16 0xd3)
rotatedHighEnvelope =
  EncryptedValue
    "v2"
    (BS.replicate 12 0xe4)
    (BS.replicate 48 0xf5)
    (BS.replicate 16 0xd6)
