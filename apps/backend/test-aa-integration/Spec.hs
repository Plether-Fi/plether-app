module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently, withAsync, wait)
import Control.Exception (bracket, finally)
import Control.Monad (void)
import Data.Aeson (Value(..), object, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.Int (Int64)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock.POSIX (getPOSIXTime)
import Database.PostgreSQL.Simple
  ( Connection
  , Only (..)
  , close
  , connectPostgreSQL
  , execute
  , execute_
  , query_
  )
import Plether.Config (NativeAaConfig (..), AaRpcMode (..))
import Plether.AA.OrderDiagnostics (claimOrderDiagnostics, completeOrderDiagnostic)
import qualified Plether.AA.RecoveryCapability as RecoveryCapability
import Plether.AA.ExecutionDiagnostics (claimExecutionDiagnostics, completeExecutionDiagnostic)
import Plether.Database.AaPreparation
import qualified Plether.Database.AaPreparationRecovery as Recovery
import Plether.Database.AaSponsorship
  ( AaReconcilerCursor (..)
  , SponsorshipAuthorization (..)
  , SponsorshipDraft (..)
  , advanceAaReconcilerCursor
  , publishAaReconcilerProgress
  , aaReconcilerIsFresh
  , getAaReconcilerCursor
  , cancelStaleUnsignedReservations
  , consumeAaRateLimit
  , controlBootstrapReason
  , ensureAaSponsorshipSchema
  , expireSponsorshipsThrough
  , getAaIssuancePause
  , getSponsorshipByDigest
  , getSponsorshipByUserOperationHash
  , getRecoveryReceiptLocator
  , ReceiptLocator(..)
  , initializeAaReconcilerCursor
  , isRecoveryOperationAuthorized
  , isSponsorshipDeliveryAllowed
  , listSubmittedSponsorships
  , markSponsorshipSubmitted
  , pauseAaIssuance
  , recordAaReconcilerHeartbeat
  , reserveSponsorship
  , reserveSponsorshipWithAssistance
  , reserveSponsorshipFenced
  , isSponsorshipDeliveryAllowedFenced
  , resumeAaIssuance
  , settleSponsorship
  , storeSponsorshipSignature
  , storeSponsorshipSignatureFenced
  )
import System.Environment (lookupEnv)
import Plether.Database.CloseAssistance
import System.Exit (die)
import Test.Hspec

main :: IO ()
main = do
  required <- (== Just "1") <$> lookupEnv "AA_INTEGRATION_REQUIRED"
  databaseUrl <- lookupEnv "AA_INTEGRATION_DATABASE_URL"
  case databaseUrl of
    Just value -> hspec $ aaIntegrationSpec $ T.pack value
    Nothing
      | required ->
          die
            "AA_INTEGRATION_DATABASE_URL is required when \
            \AA_INTEGRATION_REQUIRED=1"
      | otherwise ->
          putStrLn
            "Native AA integration test not requested. Set \
            \AA_INTEGRATION_DATABASE_URL to run it."

aaIntegrationSpec :: Text -> Spec
aaIntegrationSpec databaseUrl =
  describe "native AA PostgreSQL authorization lifecycle" $ do
    it "retires a missing attempt durably and rejects every delayed claim" $
      withFixture databaseUrl $ \conn -> do
        let scope = Recovery.Scope chainId paymasterAddress (addressOf '1') (hashOf '2')
        Recovery.retirePreparation conn scope (addressOf '8') `shouldReturn` Right ()
        Recovery.retirePreparation conn scope (addressOf '8') `shouldReturn` Right ()
        Recovery.beginPreparation conn scope "late" `shouldReturn` Left "PREPARATION_RETIRED"

    it "fences expired in-flight preparation before it can reserve sponsorship" $
      withFixture databaseUrl $ \conn -> do
        readyDatabase conn
        now <- currentEpochSeconds
        let candidate = draft '1' '2' '3' 7 1000 now
            scope = Recovery.Scope chainId paymasterAddress (sdSender candidate) (hashOf '4')
        Right fence <- Recovery.beginPreparation conn scope "worker"
        Recovery.retirePreparation conn scope (addressOf '8') `shouldReturn` Left "RECOVERY_LIABILITY_PENDING"
        void $ execute_ conn "UPDATE aa_preparation_registry SET lease_until=clock_timestamp()-interval '1 second'"
        Recovery.retirePreparation conn scope (addressOf '8') `shouldReturn` Right ()
        reserveSponsorshipFenced conn testConfig candidate Nothing (Just fence) `shouldReturn` Left "PREPARATION_LEASE_LOST"
        claimPreparationCompatibleFenced conn fence True (sdClientKey candidate) (sdSender candidate) (hashOf '4') (hashOf '5') [] "late-worker" `shouldReturn` PreparationFenceLost
        (query_ conn "SELECT count(*)::bigint FROM aa_preparations" :: IO [Only Int64]) `shouldReturn` [Only 0]
        (query_ conn "SELECT count(*)::bigint FROM aa_sponsorship_authorizations" :: IO [Only Int64]) `shouldReturn` [Only 0]

    it "finds unlinked liabilities and rejects retirement until safe reconciliation" $
      withFixture databaseUrl $ \conn -> do
        readyDatabase conn
        now <- currentEpochSeconds
        let candidate = draft '1' '2' '3' 7 1000 now
            scope = Recovery.Scope chainId paymasterAddress (sdSender candidate) (hashOf '4')
        Right fence <- Recovery.beginPreparation conn scope "worker"
        authorization <- reserveSponsorshipFenced conn testConfig candidate Nothing (Just fence) >>= expectAuthorization
        Recovery.releasePreparation conn fence
        -- No aa_preparations row/digest linkage exists; the reservation still blocks discard.
        Recovery.retirePreparation conn scope (addressOf '8') `shouldReturn` Left "RECOVERY_LIABILITY_PENDING"
        void $ execute conn "UPDATE aa_sponsorship_authorizations SET valid_after=0,valid_until=1 WHERE digest=?" (Only $ saDigest authorization)
        Recovery.retirePreparation conn scope (addressOf '8') `shouldReturn` Left "RECOVERY_LIABILITY_PENDING"
        void $ execute conn "UPDATE aa_sponsorship_authorizations SET state='cancelled',settled_at=clock_timestamp() WHERE digest=?" (Only $ saDigest authorization)
        Recovery.retirePreparation conn scope (addressOf '8') `shouldReturn` Right ()
        isSponsorshipDeliveryAllowedFenced conn testConfig (saDigest authorization) (Just fence) `shouldReturn` False

    it "recovers a legacy signed authorization only for its verified scope and exact hash" $
      withFixture databaseUrl $ \conn -> do
        readyDatabase conn
        now <- currentEpochSeconds
        let candidate = draft '1' '2' '3' 7 1000 now
            scope = Recovery.Scope chainId paymasterAddress (sdSender candidate) (hashOf '4')
            operationHash = hashOf '5'
        authorization <- reserveSponsorship conn testConfig candidate >>= expectAuthorization
        storeSponsorshipSignature conn testConfig (saDigest authorization) (signatureOf '6') operationHash `shouldReturn` True
        Right fence <- Recovery.beginPreparation conn scope "worker"
        _ <- claimPreparation conn True (sdClientKey candidate) (sdSender candidate) (hashOf '4') (hashOf '9') "preparation-worker"
        -- Delivery of an existing authorization must also acquire registry linkage.
        isSponsorshipDeliveryAllowedFenced conn testConfig (saDigest authorization) (Just fence) `shouldReturn` True
        Recovery.saveChallenge conn scope "challenge" (sdOwner candidate) "message"
        Recovery.consumeChallenge conn scope "challenge" (sdOwner candidate) "token" `shouldReturn` True
        Recovery.sessionSubmissionClient conn paymasterAddress "token" operationHash `shouldReturn` Just (sdClientKey candidate)
        Recovery.sessionSubmissionClient conn paymasterAddress "token" (hashOf '7') `shouldReturn` Nothing
        Recovery.sessionSubmissionClient conn (addressOf '8') "token" operationHash `shouldReturn` Nothing
        Recovery.saveChallenge conn (scope {Recovery.scopeId = hashOf '8'}) "wrong-attempt" (sdOwner candidate) "message"
        Recovery.consumeChallenge conn (scope {Recovery.scopeId = hashOf '8'}) "wrong-attempt" (sdOwner candidate) "other-token" `shouldReturn` True
        Recovery.sessionSubmissionClient conn paymasterAddress "other-token" operationHash `shouldReturn` Nothing
        _ <- claimPreparation conn True (clientKeyOf '9') (sdSender candidate) (hashOf '4') (hashOf '9') "historical-worker"
        Recovery.sessionSubmissionClient conn paymasterAddress "token" operationHash `shouldReturn` Nothing
        void $ execute_ conn "UPDATE aa_preparation_recovery_sessions SET expires_at=clock_timestamp()-interval '1 second'"
        Recovery.sessionSubmissionClient conn paymasterAddress "token" operationHash `shouldReturn` Nothing

    it "binds new unsigned rows to their paymaster and preserves cross-IP ambiguity" $
      withFixture databaseUrl $ \conn -> do
        let sender = addressOf '1'; identifier = hashOf '2'; client = clientKeyOf '3'
            scope = Recovery.Scope chainId paymasterAddress sender identifier
        Right fence <- Recovery.beginPreparation conn scope "registry-worker"
        _ <- claimPreparation conn True client sender identifier (hashOf '4') "worker"
        bindPreparationDeployment conn client sender identifier "worker" chainId (addressOf '8') `shouldReturn` True
        Recovery.matchingPreparations conn scope (addressOf '8') `shouldReturn` [(client,Nothing,False)]
        Recovery.bindDeployment conn fence client `shouldReturn` True
        Recovery.matchingPreparations conn scope (addressOf '8') `shouldReturn` [(client,Nothing,True)]
        let other = clientKeyOf '5'
        _ <- claimPreparation conn True other sender identifier (hashOf '4') "worker"
        bindPreparationDeployment conn other sender identifier "worker" chainId (addressOf '8') `shouldReturn` True
        Recovery.bindDeployment conn fence other `shouldReturn` True
        matches <- Recovery.matchingPreparations conn scope (addressOf '8')
        length matches `shouldBe` 2
        releasePreparation conn client sender identifier "worker"
        releasePreparation conn other sender identifier "worker"
        Recovery.releasePreparation conn fence
        Recovery.retirePreparation conn scope (addressOf '8') `shouldReturn` Right ()
        Recovery.bindDeployment conn fence client `shouldReturn` False

    it "serializes retirement with reservation and cannot clear issued liability" $
      withFixture databaseUrl $ \conn -> withPeerConnection databaseUrl $ \peer -> do
        readyDatabase conn
        now <- currentEpochSeconds
        let candidate = draft '1' '2' '3' 7 1000 now
            scope = Recovery.Scope chainId paymasterAddress (sdSender candidate) (hashOf '4')
        Right fence <- Recovery.beginPreparation conn scope "worker"
        (retired, reserved) <- concurrently
          (Recovery.retirePreparation conn scope (addressOf '8'))
          (reserveSponsorshipFenced peer testConfig candidate Nothing (Just fence))
        retired `shouldBe` Left "RECOVERY_LIABILITY_PENDING"
        authorization <- expectAuthorization reserved
        Recovery.releasePreparation conn fence
        (retirement, delivery) <- concurrently
          (Recovery.retirePreparation conn scope (addressOf '8'))
          (storeSponsorshipSignatureFenced peer testConfig (saDigest authorization) (signatureOf '5') (hashOf '6') (Just fence))
        retirement `shouldBe` Left "RECOVERY_LIABILITY_PENDING"
        delivery `shouldBe` False
        Recovery.registryRetired conn scope `shouldReturn` False

    it "serializes retirement against concurrent claims" $
      withFixture databaseUrl $ \conn -> withPeerConnection databaseUrl $ \peer -> do
        let scope = Recovery.Scope chainId paymasterAddress (addressOf '1') (hashOf '2')
        (retired,claimed) <- concurrently
          (Recovery.retirePreparation conn scope (addressOf '8'))
          (Recovery.beginPreparation peer scope "worker")
        case (retired,claimed) of
          (Right (),Left "PREPARATION_RETIRED") -> pure ()
          (Left "RECOVERY_LIABILITY_PENDING",Right _) -> pure ()
          other -> expectationFailure $ "Unsafe concurrent outcome: " <> show other

    it "keeps historical deployment ambiguity unresolved" $
      withFixture databaseUrl $ \conn -> do
        let sender = addressOf '1'; identifier = hashOf '2'
            scope = Recovery.Scope chainId paymasterAddress sender identifier
        _ <- claimPreparation conn True (clientKeyOf '3') sender identifier (hashOf '4') "worker"
        releasePreparation conn (clientKeyOf '3') sender identifier "worker"
        Recovery.retirePreparation conn scope (addressOf '8') `shouldReturn` Left "RECOVERY_BINDING_UNRESOLVED"
        Recovery.matchingPreparations conn scope (addressOf '8') `shouldReturn` [(clientKeyOf '3',Nothing,False)]

    it "consumes wallet challenges exactly once and scopes expiring sessions" $
      withFixture databaseUrl $ \conn -> withPeerConnection databaseUrl $ \peer -> do
        let scope = Recovery.Scope chainId paymasterAddress (addressOf '1') (hashOf '2')
            owner = addressOf '3'
        Recovery.saveChallenge conn scope "challenge" owner "message"
        Recovery.readChallenge conn scope "challenge" `shouldReturn` Just (owner,"message")
        (first,second) <- concurrently
          (Recovery.consumeChallenge conn scope "challenge" owner "token-a")
          (Recovery.consumeChallenge peer scope "challenge" owner "token-b")
        (first /= second) `shouldBe` True
        let token = if first then "token-a" else "token-b"
        Recovery.sessionOwner conn scope token `shouldReturn` Just owner
        Recovery.sessionOwner conn (scope { Recovery.scopeSender = addressOf '4' }) token `shouldReturn` Nothing
        Recovery.sessionOwner conn (scope { Recovery.scopePaymaster = addressOf '5' }) token `shouldReturn` Nothing
        Recovery.readChallenge conn scope "challenge" `shouldReturn` Nothing
        void $ execute_ conn "UPDATE aa_preparation_recovery_sessions SET expires_at=clock_timestamp()-interval '1 second'"
        Recovery.sessionOwner conn scope token `shouldReturn` Nothing

    it "does not consume expired challenges or accept the wrong owner" $
      withFixture databaseUrl $ \conn -> do
        let scope = Recovery.Scope chainId paymasterAddress (addressOf '1') (hashOf '2')
        Recovery.saveChallenge conn scope "challenge" (addressOf '3') "message"
        Recovery.consumeChallenge conn scope "challenge" (addressOf '4') "token" `shouldReturn` False
        void $ execute_ conn "UPDATE aa_preparation_recovery_challenges SET expires_at=clock_timestamp()-interval '1 second'"
        Recovery.consumeChallenge conn scope "challenge" (addressOf '3') "token" `shouldReturn` False

    it "serializes assistance across different requests and preserves failed-attempt retries" $
      withFixture databaseUrl $ \conn -> do
        readyDatabase conn
        now <- currentEpochSeconds
        let first = draft '1' '2' '3' 7 1000 now
            second = draft '4' '5' '3' 8 1000 now
            grant = CloseAssistanceReservation (addressOf '8') (addressOf '1') (hashOf '6') (hashOf '7') (addressOf '9') 198000
        _ <- reserveSponsorshipWithAssistance conn testConfig first (Just grant) >>= expectAuthorization
        getCloseAssistanceReservation conn (sdDigest first) `shouldReturn` Just grant
        reserveSponsorshipWithAssistance conn testConfig second (Just grant)
          `shouldReturn` Left "ASSISTANCE_RESERVATION_PENDING"
        void $ execute conn "UPDATE aa_sponsorship_authorizations SET state='cancelled' WHERE digest=?" (Only $ sdDigest first)
        _ <- reserveSponsorshipWithAssistance conn testConfig second (Just grant) >>= expectAuthorization
        pure ()

    it "permits only one of two concurrent assisted intents for the same account" $
      withFixture databaseUrl $ \conn -> do
        readyDatabase conn
        now <- currentEpochSeconds
        let first = draft '1' '2' '3' 7 1000 now
            second = draft '4' '5' '3' 8 1000 now
            grant = CloseAssistanceReservation (addressOf '8') (addressOf '1') (hashOf '6') (hashOf '7') (addressOf '9') 198000
        withPeerConnection databaseUrl $ \peer -> do
          (a,b) <- concurrently
            (reserveSponsorshipWithAssistance conn testConfig first (Just grant))
            (reserveSponsorshipWithAssistance peer testConfig second (Just $ grant { carClientOrderId = hashOf 'a' }))
          length [() | Right _ <- [a,b]] `shouldBe` 1
          length [() | Left "ASSISTANCE_RESERVATION_PENDING" <- [a,b]] `shouldBe` 1
        count <- query_ conn "SELECT COUNT(*) FROM aa_close_assistance" :: IO [Only Int64]
        count `shouldBe` [Only 1]

    it "boots paused and requires an exact audited operator resume" $
      withFixture databaseUrl $ \conn -> do
        getAaIssuancePause conn `shouldReturn` Just controlBootstrapReason
        initializeReconciler conn
        resumeAaIssuance conn "wrong reason" "integration test"
          `shouldReturn` Left "The expected pause reason does not match the current circuit breaker"
        resumeAaIssuance conn controlBootstrapReason "integration test setup"
          `shouldReturn` Right ()
        getAaIssuancePause conn `shouldReturn` Nothing

    it "serializes concurrent idempotent reservations across API connections" $
      withFixture databaseUrl $ \firstConnection -> do
        readyDatabase firstConnection
        now <- currentEpochSeconds
        let sponsorship = draft '1' '2' '3' 7 1_000 now
        withPeerConnection databaseUrl $ \secondConnection -> do
          (firstResult, secondResult) <-
            concurrently
              (reserveSponsorship firstConnection testConfig sponsorship)
              (reserveSponsorship secondConnection testConfig sponsorship)
          firstAuthorization <- expectAuthorization firstResult
          secondAuthorization <- expectAuthorization secondResult
          firstAuthorization `shouldBe` secondAuthorization

        authorizationCount <-
          query_ firstConnection
            "SELECT COUNT(*) FROM aa_sponsorship_authorizations" :: IO [Only Int64]
        reserveLedgerCount <-
          query_ firstConnection
            "SELECT COUNT(*) FROM aa_sponsorship_ledger WHERE entry_type='reserve'" :: IO [Only Int64]
        authorizationCount `shouldBe` [Only 1]
        reserveLedgerCount `shouldBe` [Only 1]

    it "persists the signature before submission and binds recovery to the exact client" $
      withFixture databaseUrl $ \conn -> do
        readyDatabase conn
        now <- currentEpochSeconds
        authorization <- reserveSponsorship conn testConfig (draft '4' '5' '6' 8 1_000 now)
          >>= expectAuthorization
        let digest = saDigest authorization
            operationHash = hashOf '7'
            clientKey = clientKeyOf '6'
            signature = signatureOf '8'

        storeSponsorshipSignature conn testConfig digest signature operationHash
          `shouldReturn` True
        storeSponsorshipSignature conn testConfig digest signature operationHash
          `shouldReturn` True
        storeSponsorshipSignature conn testConfig digest (signatureOf '9') operationHash
          `shouldReturn` False
        markSponsorshipSubmitted conn digest operationHash (clientKeyOf 'a')
          `shouldReturn` False
        markSponsorshipSubmitted conn digest operationHash clientKey
          `shouldReturn` True
        isRecoveryOperationAuthorized conn operationHash clientKey "alto"
          `shouldReturn` True
        isRecoveryOperationAuthorized conn operationHash (clientKeyOf 'a') "alto"
          `shouldReturn` False
        -- A read-only credential retains the original DB binding across IP
        -- changes. It cannot authorize another hash/provider or expired row.
        let token = RecoveryCapability.issue "secret" "deployment" now operationHash clientKey
        case RecoveryCapability.verify "secret" "deployment" (now+1) operationHash token of
          Nothing -> expectationFailure "credential rejected"
          Just originalClient -> do
            isRecoveryOperationAuthorized conn operationHash originalClient "alto" `shouldReturn` True
            isRecoveryOperationAuthorized conn (hashOf '0') originalClient "alto" `shouldReturn` False
            isRecoveryOperationAuthorized conn operationHash originalClient "other" `shouldReturn` False
            void $ execute conn "UPDATE aa_recovery_operations SET created_at=clock_timestamp()-INTERVAL '8 days', expires_at=clock_timestamp()-INTERVAL '1 second' WHERE user_operation_hash=?" (Only operationHash)
            isRecoveryOperationAuthorized conn operationHash originalClient "alto" `shouldReturn` False

        stored <- getSponsorshipByUserOperationHash conn operationHash
        fmap saState stored `shouldBe` Just "submitted"
        submitted <- listSubmittedSponsorships conn 10
        length submitted `shouldBe` 1

    it "settles once, records exact gas liability, and rejects conflicting replay" $
      withFixture databaseUrl $ \conn -> do
        readyDatabase conn
        now <- currentEpochSeconds
        authorization <- submittedAuthorization conn now
        let digest = saDigest authorization
            operationHash = maybe (hashOf '0') id $ saExpectedUserOperationHash authorization
            transactionHash = hashOf 'b'
            blockHash = hashOf 'c'
            event = object ["source" .= ("aa-integration" :: Text)]

        getRecoveryReceiptLocator conn operationHash (saClientKey authorization) `shouldReturn` Nothing

        settleSponsorship
          conn digest operationHash transactionHash 101 blockHash True 400 event
          `shouldReturn` Right ()
        settleSponsorship
          conn digest operationHash transactionHash 101 blockHash True 400 event
          `shouldReturn` Right ()
        settleSponsorship
          conn digest operationHash transactionHash 101 blockHash True 401 event
          `shouldReturn` Left "USER_OPERATION_EVENT_CONFLICT"

        stored <- getSponsorshipByDigest conn digest
        fmap saState stored `shouldBe` Just "settled"
        recovered <- getRecoveryReceiptLocator conn operationHash (saClientKey authorization)
        fmap rlTransactionHash recovered `shouldBe` Just transactionHash
        fmap rlNonce recovered `shouldBe` Just (saNonce authorization)
        fmap rlEvent recovered `shouldBe` Just event
        getRecoveryReceiptLocator conn operationHash (clientKeyOf '0') `shouldReturn` Nothing
        getRecoveryReceiptLocator conn (hashOf '0') (saClientKey authorization) `shouldReturn` Nothing
        pauseAaIssuance conn "integration recovery while paused"
        getRecoveryReceiptLocator conn operationHash (saClientKey authorization) `shouldReturn` recovered
        ledger <-
          query_ conn
            "SELECT entry_type,amount_wei::TEXT FROM aa_sponsorship_ledger ORDER BY entry_type" :: IO [(Text, Text)]
        ledger
          `shouldBe` [ ("actual_charge", "400")
                     , ("release", "600")
                     , ("reserve", "1000")
                     ]

    it "releases exact liability for stale unsigned and expired signed authorizations" $
      withFixture databaseUrl $ \conn -> do
        readyDatabase conn
        now <- currentEpochSeconds
        unsigned <- reserveSponsorship conn testConfig (draft '1' '2' '3' 12 1_000 now)
          >>= expectAuthorization
        aged <- execute conn
          "UPDATE aa_sponsorship_authorizations \
          \SET created_at=clock_timestamp()-INTERVAL '11 minutes' WHERE digest=?"
          (Only $ saDigest unsigned)
        aged `shouldBe` 1
        cancelStaleUnsignedReservations conn `shouldReturn` 1

        signed <- reserveSponsorship conn testConfig (draft '4' '5' '6' 13 1_000 now)
          >>= expectAuthorization
        storeSponsorshipSignature
          conn testConfig (saDigest signed) (signatureOf '7') (hashOf '8')
          `shouldReturn` True
        expireSponsorshipsThrough conn (now + 1_000) `shouldReturn` 1

        ledger <- query_ conn
          "SELECT digest,entry_type,amount_wei::TEXT FROM aa_sponsorship_ledger \
          \ORDER BY digest,entry_type" :: IO [(Text, Text, Text)]
        ledger
          `shouldBe` [ (hashOf '2', "release", "1000")
                     , (hashOf '2', "reserve", "1000")
                     , (hashOf '5', "release", "1000")
                     , (hashOf '5', "reserve", "1000")
                     ]

    it "atomically exposes completed cursor and health to concurrent readers" $
      withFixture databaseUrl $ \conn -> do
        readyDatabase conn
        now <- currentEpochSeconds
        void $ execute_ conn
          "CREATE FUNCTION hold_heartbeat() RETURNS trigger LANGUAGE plpgsql AS $$ \
          \BEGIN PERFORM pg_advisory_xact_lock(887766); RETURN NEW; END $$"
        void $ execute_ conn
          "CREATE TRIGGER hold_heartbeat BEFORE UPDATE ON aa_reconciler_health \
          \FOR EACH ROW EXECUTE FUNCTION hold_heartbeat()"
        let previous = AaReconcilerCursor 100 deploymentBlockHash
            next = AaReconcilerCursor 101 (hashOf '2')
        withPeerConnection databaseUrl $ \peer -> do
          void (query_ peer "SELECT 1::INT FROM pg_advisory_lock(887766)" :: IO [Only Int])
          withAsync (publishAaReconcilerProgress conn chainId paymasterAddress previous next now True) $ \publication -> do
            (do
              waitForPublicationLock peer 200
              -- The writer has advanced its cursor and is paused at health.
              -- A separate connection must still see the old consistent pair.
              getAaReconcilerCursor peer chainId paymasterAddress `shouldReturn` Just previous
              aaReconcilerIsFresh peer testConfig `shouldReturn` True
              ) `finally` void (query_ peer "SELECT pg_advisory_unlock(887766)" :: IO [Only Bool])
            wait publication `shouldReturn` True
          getAaReconcilerCursor peer chainId paymasterAddress `shouldReturn` Just next
          aaReconcilerIsFresh peer testConfig `shouldReturn` True
          authorization <- reserveSponsorship peer testConfig (draft 'd' 'e' 'f' 10 1_000 now) >>= expectAuthorization
          storeSponsorshipSignature peer testConfig (saDigest authorization) (signatureOf '1') (hashOf '3')
            `shouldReturn` True
          isSponsorshipDeliveryAllowed peer testConfig (saDigest authorization) `shouldReturn` True

    it "keeps partial progress stale and rejects a lost cursor CAS without refreshing health" $
      withFixture databaseUrl $ \conn -> do
        readyDatabase conn
        now <- currentEpochSeconds
        let previous = AaReconcilerCursor 100 deploymentBlockHash
            next = AaReconcilerCursor 101 (hashOf '2')
        publishAaReconcilerProgress conn chainId paymasterAddress previous next now False `shouldReturn` True
        aaReconcilerIsFresh conn testConfig `shouldReturn` False
        reserveSponsorship conn testConfig (draft 'd' 'e' 'f' 10 1_000 now) `shouldReturn` Left "RECONCILER_STALE"
        publishAaReconcilerProgress conn chainId paymasterAddress previous next now True `shouldReturn` False
        aaReconcilerIsFresh conn testConfig `shouldReturn` False
        publishAaReconcilerProgress conn chainId paymasterAddress next next now True `shouldReturn` True
        aaReconcilerIsFresh conn testConfig `shouldReturn` True
        void $ execute_ conn "UPDATE aa_reconciler_health SET last_success_at=clock_timestamp()-INTERVAL '121 seconds'"
        aaReconcilerIsFresh conn testConfig `shouldReturn` False

    it "rolls back cursor and liability cleanup if completed publication fails" $
      withFixture databaseUrl $ \conn -> do
        readyDatabase conn
        now <- currentEpochSeconds
        authorization <- submittedAuthorization conn now
        void $ execute_ conn
          "CREATE FUNCTION fail_pruning() RETURNS trigger LANGUAGE plpgsql AS $$ \
          \BEGIN RAISE EXCEPTION 'injected cleanup failure'; END $$"
        void $ execute_ conn
          "CREATE TRIGGER fail_pruning BEFORE DELETE ON aa_rate_windows \
          \FOR EACH STATEMENT EXECUTE FUNCTION fail_pruning()"
        let previous = AaReconcilerCursor 100 deploymentBlockHash
            next = AaReconcilerCursor 101 (hashOf '2')
        publishAaReconcilerProgress conn chainId paymasterAddress previous next (now + 10000) True
          `shouldThrow` anyException
        getAaReconcilerCursor conn chainId paymasterAddress `shouldReturn` Just previous
        aaReconcilerIsFresh conn testConfig `shouldReturn` True
        restored <- getSponsorshipByDigest conn (saDigest authorization)
        fmap saState restored `shouldBe` Just "submitted"

    it "fails closed when reconciliation is stale or issuance is paused" $
      withFixture databaseUrl $ \conn -> do
        readyDatabase conn
        now <- currentEpochSeconds
        void $ execute_ conn
          "UPDATE aa_reconciler_health SET last_success_at=clock_timestamp()-INTERVAL '5 minutes'"
        reserveSponsorship conn testConfig (draft 'd' 'e' 'f' 10 1_000 now)
          `shouldReturn` Left "RECONCILER_STALE"

        recordAaReconcilerHeartbeat conn chainId paymasterAddress 101 (hashOf '2')
        let previous = AaReconcilerCursor 100 deploymentBlockHash
            next = AaReconcilerCursor 101 (hashOf '2')
        advanceAaReconcilerCursor conn chainId paymasterAddress previous next
          `shouldReturn` True
        recordAaReconcilerHeartbeat conn chainId paymasterAddress 101 (hashOf '2')
        authorization <- reserveSponsorship conn testConfig (draft 'd' 'e' 'f' 10 1_000 now)
          >>= expectAuthorization
        storeSponsorshipSignature
          conn testConfig (saDigest authorization) (signatureOf '1') (hashOf '3')
          `shouldReturn` True
        isSponsorshipDeliveryAllowed conn testConfig (saDigest authorization)
          `shouldReturn` True
        pauseAaIssuance conn "integration incident"
        reserveSponsorship conn testConfig (draft '7' '8' '9' 11 1_000 now)
          `shouldReturn` Left "PAYMASTER_PAUSED"
        isSponsorshipDeliveryAllowed conn testConfig (saDigest authorization)
          `shouldReturn` False

    it "preserves budgets, stale-reconciler rejection and pause in single-provider mode" $
      withFixture databaseUrl $ \conn -> do
        readyDatabase conn
        now <- currentEpochSeconds
        let cfg = testConfig
              { naaRpcMode = SingleProviderSepolia
              , naaSecurityRpcUrl = "https://primary-rpc.invalid"
              }
        reserveSponsorship conn cfg (draft '1' '2' '3' 20 10_001 now)
          `shouldReturn` Left "PER_OPERATION_BUDGET_EXCEEDED"
        void $ reserveSponsorship conn cfg (draft '4' '5' '6' 21 1_000 now)
          >>= expectAuthorization
        void $ execute_ conn
          "UPDATE aa_reconciler_health SET last_success_at=clock_timestamp()-INTERVAL '5 minutes'"
        reserveSponsorship conn cfg (draft '7' '8' '9' 22 1_000 now)
          `shouldReturn` Left "RECONCILER_STALE"
        recordAaReconcilerHeartbeat conn chainId paymasterAddress 100 deploymentBlockHash
        pauseAaIssuance conn "single-provider test pause"
        reserveSponsorship conn cfg (draft 'a' 'b' 'c' 23 1_000 now)
          `shouldReturn` Left "PAYMASTER_PAUSED"

    it "shares rate limits across connections" $
      withFixture databaseUrl $ \firstConnection ->
        withPeerConnection databaseUrl $ \secondConnection -> do
          let clientKey = clientKeyOf '1'
              accountKey = clientKeyOf '2'
          (firstAllowed, secondAllowed) <-
            concurrently
              (consumeAaRateLimit firstConnection "final-issuance" clientKey accountKey 2)
              (consumeAaRateLimit secondConnection "final-issuance" clientKey accountKey 2)
          firstAllowed `shouldBe` True
          secondAllowed `shouldBe` True
          consumeAaRateLimit firstConnection "final-issuance" clientKey accountKey 2
            `shouldReturn` False

    it "reads an assisted preparation by client or hash without changing its authorization" $
      withFixture databaseUrl $ \conn -> do
        readyDatabase conn
        now <- currentEpochSeconds
        let first = draft '1' '2' '3' 7 1000 now
            grant = CloseAssistanceReservation (addressOf '8') (addressOf '1') (hashOf '6') (hashOf '7') (addressOf '9') 198000
            client = clientKeyOf '3'; sender = addressOf '1'; identifier = hashOf 'b'; intent = hashOf 'c'; operationHash = hashOf 'a'
            operation = object ["nonce" .= ("0x7" :: Text)]
        authorization <- reserveSponsorshipWithAssistance conn testConfig first (Just grant) >>= expectAuthorization
        storeSponsorshipSignature conn testConfig (saDigest authorization) (signatureOf '6') operationHash `shouldReturn` True
        claimPreparation conn True client sender identifier intent "lease" `shouldReturn` PreparationClaimed Nothing
        savePreparedOperation conn client sender identifier "lease" operation `shouldReturn` True
        bindPreparationDeployment conn client sender identifier "lease" chainId (addressOf '8') `shouldReturn` True
        linkPreparation conn client sender identifier "lease" (saDigest authorization) `shouldReturn` True
        byId <- getPreparationStatus conn client sender (Just identifier) Nothing
        byHash <- getPreparationStatus conn client sender Nothing (Just operationHash)
        byHash `shouldBe` byId
        case byId of
          Just (Object fields', Just saved) -> do
            saved `shouldBe` operation
            KM.lookup "authorizationState" fields' `shouldBe` Just (String "signed")
            KM.lookup "assistanceBlocked" fields' `shouldBe` Just (Bool True)
            KM.member "signature" fields' `shouldBe` False
            KM.member "clientKey" fields' `shouldBe` False
          _ -> expectationFailure "missing status projection"
        getPreparationStatus conn (clientKeyOf '4') sender (Just identifier) Nothing `shouldReturn` Nothing
        getPreparationStatus conn client (addressOf '4') Nothing (Just operationHash) `shouldReturn` Nothing
        void $ pauseAaIssuance conn "test pause"
        getPreparationStatus conn client sender (Just identifier) Nothing `shouldReturn` byId
        count <- query_ conn "SELECT COUNT(*) FROM aa_sponsorship_authorizations" :: IO [Only Int64]
        count `shouldBe` [Only 1]

    it "fences preparation leases across instances and persists immutable work" $
      withFixture databaseUrl $ \first -> withPeerConnection databaseUrl $ \second -> do
        let client = clientKeyOf 'a'; sender = addressOf '1'; identifier = hashOf 'b'; intent = hashOf 'c'
            operation = object ["nonce" .= ("0x1" :: Text),"callGasLimit" .= ("0x997fd" :: Text)]
        claimPreparation first False client sender identifier intent "worker-a" `shouldReturn` PreparationDisabled
        claimPreparation first True client sender identifier intent "worker-a" `shouldReturn` PreparationClaimed Nothing
        claimPreparation second True client sender identifier intent "worker-b" `shouldReturn` PreparationBusy
        claimPreparation second True client sender identifier (hashOf 'd') "worker-b" `shouldReturn` PreparationConflict
        claimPreparationCompatible second True client sender identifier (hashOf 'd') [intent] "worker-b"
          `shouldReturn` PreparationConflict
        savePreparedOperation second client sender identifier "worker-b" operation `shouldReturn` False
        savePreparedOperation first client sender identifier "worker-a" operation `shouldReturn` True
        void $ execute_ first "UPDATE aa_preparations SET lease_until=clock_timestamp()-interval '1 second'"
        claimPreparation second True client sender identifier intent "worker-b" `shouldReturn` PreparationClaimed (Just operation)
        releasePreparation first client sender identifier "worker-a"
        claimPreparation first True client sender identifier intent "worker-c" `shouldReturn` PreparationBusy
        -- A restarted worker must not apply the headroom multiplier a second time.
        savePreparedOperation second client sender identifier "worker-b"
          (object ["nonce" .= ("0x1" :: Text),"callGasLimit" .= ("0xe63fc" :: Text)]) `shouldReturn` False
        releasePreparation second client sender identifier "worker-b"
        -- A reviewed policy-only upgrade resumes the old exact padded payload;
        -- it never rewrites it or adopts a changed nonce/fee/intent.
        claimPreparationCompatible first False client sender identifier (hashOf 'd') [intent] "worker-c"
          `shouldReturn` PreparationClaimed (Just operation)
        savePreparedOperation first client sender identifier "worker-c"
          (object ["nonce" .= ("0x2" :: Text),"callGasLimit" .= ("0x2dc6c0" :: Text)]) `shouldReturn` False
        void $ execute_ first "UPDATE aa_preparations SET expires_at=clock_timestamp()-interval '1 second'"
        claimPreparation second True client sender identifier intent "worker-d" `shouldReturn` PreparationExpired

    it "persists nullable correlation with the lease-bound authorization and preserves it across retries" $
      withFixture databaseUrl $ \conn -> do
        readyDatabase conn
        now <- currentEpochSeconds
        authorization <- reserveSponsorship conn testConfig (draft '1' '2' '3' 0 100 now) >>= expectAuthorization
        let client = clientKeyOf '3'; sender = addressOf '1'; identifier = hashOf 'b'; intent = hashOf 'c'
            attempt = "12345678-1234-4123-8123-123456789abc"
        claimPreparation conn True client sender identifier intent "lease" `shouldReturn` PreparationClaimed Nothing
        linkPreparationDiagnostic conn client sender identifier "wrong-lease" (saDigest authorization) (Just attempt) chainId "deployment" `shouldReturn` False
        linkPreparationDiagnostic conn client sender identifier "lease" (saDigest authorization) (Just attempt) chainId "deployment" `shouldReturn` True
        releasePreparation conn client sender identifier "lease"
        claimPreparation conn False client sender identifier intent "retry" `shouldReturn` PreparationClaimed Nothing
        linkPreparationDiagnostic conn client sender identifier "retry" (saDigest authorization) (Just "12345678-1234-4123-8123-aaaaaaaaaaaa") chainId "changed" `shouldReturn` True
        rows <- query_ conn "SELECT diagnostic_attempt_id::text,diagnostic_deployment FROM aa_preparations" :: IO [(Text,Text)]
        rows `shouldBe` [(attempt,"deployment")]
        -- Rollback code still reads/releases the same immutable preparation.
        linkPreparation conn client sender identifier "retry" (saDigest authorization) `shouldReturn` True

    it "reapplies every additive release migration without changing existing signed liabilities" $
      withFixture databaseUrl $ \conn -> do
        readyDatabase conn
        now <- currentEpochSeconds
        authorization <- submittedAuthorization conn now
        before <- getSponsorshipByDigest conn (saDigest authorization)
        let migrations = ["aa-preparation-v1.sql", "aa-observability-v1.sql", "aa-observability-v2.sql", "aa-funding-v1.sql", "aa-preparation-recovery-v1.sql"]
        mapM_ (\name -> readFile ("config/migrations/" <> name) >>= execute_ conn . fromString >> pure ()) (migrations <> migrations)
        after <- getSponsorshipByDigest conn (saDigest authorization)
        after `shouldBe` before

    it "recovers late order diagnostics with a multi-instance fenced lease and no ledger changes" $
      withFixture databaseUrl $ \conn -> do
        readyDatabase conn
        now <- currentEpochSeconds
        authorization <- submittedAuthorization conn now
        let operationHash = maybe (hashOf '0') id $ saExpectedUserOperationHash authorization
            attempt = "12345678-1234-4123-8123-123456789abc" :: Text
            router = addressOf 'f'
        settleSponsorship conn (saDigest authorization) operationHash (hashOf 'b') 101 (hashOf 'c') True 400 (object [])
          `shouldReturn` Right ()
        -- Receipt/indexing may precede asynchronous diagnostic materialization.
        claimOrderDiagnostics conn chainId router `shouldReturn` []
        void $ execute conn
          "INSERT INTO aa_attempt_diagnostics(attempt_id,client_key,chain_id,deployment,operation_hash,sender,stage) VALUES (?::uuid,?,?,?,?,?,'prepared')"
          (attempt,clientKeyOf '5',chainId,router,operationHash,addressOf '1')
        claimOrderDiagnostics conn (chainId+1) router `shouldReturn` []
        withPeerConnection databaseUrl $ \peer -> do
          (first,second) <- concurrently (claimOrderDiagnostics conn chainId router) (claimOrderDiagnostics peer chainId router)
          length (first++second) `shouldBe` 1
          let (_,_,_,_,_,_,oldLease) = head $ first++second
          void $ execute_ conn "UPDATE aa_attempt_diagnostics SET correlation_checked_at=clock_timestamp()-interval '31 seconds'"
          recovered <- claimOrderDiagnostics peer chainId router
          length recovered `shouldBe` 1
          let (_,_,_,_,_,_,newLease) = head recovered
          completeOrderDiagnostic conn attempt oldLease (Just 8) `shouldReturn` False
          completeOrderDiagnostic peer attempt newLease (Just 8) `shouldReturn` True
          completeOrderDiagnostic conn attempt newLease (Just 9) `shouldReturn` False
          claimOrderDiagnostics conn chainId router `shouldReturn` []
        rows <- query_ conn "SELECT order_id,stage FROM aa_attempt_diagnostics" :: IO [(Integer,Text)]
        rows `shouldBe` [(8,"committed")]
        ledger <- query_ conn "SELECT entry_type,amount_wei::text FROM aa_sponsorship_ledger ORDER BY entry_type" :: IO [(Text,Text)]
        ledger `shouldBe` [("actual_charge","400"),("release","600"),("reserve","1000")]

    it "fences failed execution diagnostic claims and deduplicates completion without changing liabilities" $
      withFixture databaseUrl $ \conn -> withPeerConnection databaseUrl $ \peer -> do
        readyDatabase conn
        now <- currentEpochSeconds
        authorization <- submittedAuthorization conn now
        let operationHash = maybe (hashOf '0') id $ saExpectedUserOperationHash authorization
            attempt = "12345678-1234-4123-8123-123456789abc" :: Text
            router = addressOf 'f'; client = clientKeyOf '5'; sender = addressOf '1'; identifier = hashOf 'd'
        claimPreparation conn True client sender identifier (hashOf 'c') "prep" `shouldReturn` PreparationClaimed Nothing
        savePreparedOperation conn client sender identifier "prep" (object ["callData" .= ("0x34fcd5be" :: Text)]) `shouldReturn` True
        linkPreparation conn client sender identifier "prep" (saDigest authorization) `shouldReturn` True
        void $ execute conn
          "INSERT INTO aa_attempt_diagnostics(attempt_id,client_key,chain_id,deployment,preparation_id,operation_hash,sender,stage) VALUES (?::uuid,?,?,?,?,?,?,'user_operation_reverted')"
          (attempt,client,chainId,router,identifier,operationHash,sender)
        claimExecutionDiagnostics conn chainId router `shouldReturn` []
        settleSponsorship conn (saDigest authorization) operationHash (hashOf 'b') 101 (hashOf 'c') False 400 (object [])
          `shouldReturn` Right ()
        claimExecutionDiagnostics conn (chainId+1) router `shouldReturn` []
        (a,b) <- concurrently (claimExecutionDiagnostics conn chainId router) (claimExecutionDiagnostics peer chainId router)
        length (a++b) `shouldBe` 1
        let (_,_,_,_,_,_,oldLease) = head $ a++b
        void $ execute_ conn "UPDATE aa_attempt_diagnostics SET correlation_checked_at=clock_timestamp()-interval '6 minutes'"
        retried <- claimExecutionDiagnostics peer chainId router
        length retried `shouldBe` 1
        let (_,_,_,_,_,_,newLease) = head retried
        completeExecutionDiagnostic conn attempt oldLease "USER_OPERATION_OUT_OF_GAS" `shouldReturn` False
        completeExecutionDiagnostic peer attempt newLease "USER_OPERATION_OUT_OF_GAS" `shouldReturn` True
        completeExecutionDiagnostic conn attempt newLease "USER_OPERATION_OUT_OF_GAS" `shouldReturn` False
        claimExecutionDiagnostics conn chainId router `shouldReturn` []
        ledger <- query_ conn "SELECT entry_type,amount_wei::text FROM aa_sponsorship_ledger ORDER BY entry_type" :: IO [(Text,Text)]
        ledger `shouldBe` [("actual_charge","400"),("release","600"),("reserve","1000")]

    it "does not claim unfinalized, failed, terminal or wrong-client evidence" $
      withFixture databaseUrl $ \conn -> do
        readyDatabase conn
        now <- currentEpochSeconds
        authorization <- submittedAuthorization conn now
        let operationHash = maybe (hashOf '0') id $ saExpectedUserOperationHash authorization
            attempt = "12345678-1234-4123-8123-123456789abc" :: Text
            router = addressOf 'f'
        void $ execute conn
          "INSERT INTO aa_attempt_diagnostics(attempt_id,client_key,chain_id,deployment,operation_hash,sender,stage) VALUES (?::uuid,?,?,?,?,?,'prepared')"
          (attempt,clientKeyOf '5',chainId,router,operationHash,addressOf '1')
        claimOrderDiagnostics conn chainId router `shouldReturn` []
        settleSponsorship conn (saDigest authorization) operationHash (hashOf 'b') 101 (hashOf 'c') True 400 (object [])
          `shouldReturn` Right ()
        void $ execute_ conn "UPDATE aa_user_operation_events SET success=false"
        claimOrderDiagnostics conn chainId router `shouldReturn` []
        void $ execute_ conn "UPDATE aa_user_operation_events SET success=true"
        void $ execute_ conn "UPDATE aa_attempt_diagnostics SET terminal_at=clock_timestamp()"
        claimOrderDiagnostics conn chainId router `shouldReturn` []
        void $ execute conn "UPDATE aa_attempt_diagnostics SET terminal_at=NULL,client_key=?" (Only $ clientKeyOf '6')
        claimOrderDiagnostics conn chainId router `shouldReturn` []

waitForPublicationLock :: Connection -> Int -> IO ()
waitForPublicationLock conn attempts = do
  rows <- query_ conn
    "SELECT EXISTS (SELECT 1 FROM pg_locks WHERE locktype='advisory' AND objid=887766 AND NOT granted)" :: IO [Only Bool]
  if rows == [Only True]
    then pure ()
    else if attempts <= 0
      then expectationFailure "publication did not reach the heartbeat barrier"
      else threadDelay 10000 >> waitForPublicationLock conn (attempts - 1)

withFixture :: Text -> (Connection -> IO a) -> IO a
withFixture databaseUrl action =
  bracket
    (connectPostgreSQL $ TE.encodeUtf8 databaseUrl)
    close
    (\conn -> do
      resetSchema conn
      action conn `finally` cleanupSchema conn
    )

withPeerConnection :: Text -> (Connection -> IO a) -> IO a
withPeerConnection databaseUrl =
  bracket
    (do
      conn <- connectPostgreSQL $ TE.encodeUtf8 databaseUrl
      void $ execute_ conn "SET search_path TO aa_integration_spec, public"
      pure conn
    )
    close

resetSchema :: Connection -> IO ()
resetSchema conn = do
  void $ execute_ conn "DROP SCHEMA IF EXISTS aa_integration_spec CASCADE"
  void $ execute_ conn "CREATE SCHEMA aa_integration_spec"
  void $ execute_ conn "SET search_path TO aa_integration_spec, public"
  ensureAaSponsorshipSchema conn
  migration <- fromString <$> readFile "config/migrations/aa-preparation-v1.sql"
  void $ execute_ conn migration
  observability <- fromString <$> readFile "config/migrations/aa-observability-v1.sql"
  void $ execute_ conn observability
  correlation <- fromString <$> readFile "config/migrations/aa-observability-v2.sql"
  void $ execute_ conn correlation
  recovery <- fromString <$> readFile "config/migrations/aa-preparation-recovery-v1.sql"
  void $ execute_ conn recovery

cleanupSchema :: Connection -> IO ()
cleanupSchema conn = do
  void $ execute_ conn "SET search_path TO public"
  void $ execute_ conn "DROP SCHEMA IF EXISTS aa_integration_spec CASCADE"

readyDatabase :: Connection -> IO ()
readyDatabase conn = do
  initializeReconciler conn
  resumed <- resumeAaIssuance conn controlBootstrapReason "integration test setup"
  resumed `shouldBe` Right ()

initializeReconciler :: Connection -> IO ()
initializeReconciler conn = do
  _ <- initializeAaReconcilerCursor
    conn chainId paymasterAddress 100 deploymentBlockHash
  recordAaReconcilerHeartbeat
    conn chainId paymasterAddress 100 deploymentBlockHash

submittedAuthorization :: Connection -> Integer -> IO SponsorshipAuthorization
submittedAuthorization conn now = do
  authorization <- reserveSponsorship conn testConfig (draft '3' '4' '5' 9 1_000 now)
    >>= expectAuthorization
  let operationHash = hashOf 'a'
  stored <- storeSponsorshipSignature
    conn testConfig (saDigest authorization) (signatureOf '6') operationHash
  stored `shouldBe` True
  submitted <- markSponsorshipSubmitted
    conn (saDigest authorization) operationHash (clientKeyOf '5')
  submitted `shouldBe` True
  maybe (expectationFailure "submitted authorization disappeared" >> fail "missing authorization") pure
    =<< getSponsorshipByDigest conn (saDigest authorization)

expectAuthorization
  :: Either Text SponsorshipAuthorization
  -> IO SponsorshipAuthorization
expectAuthorization = \case
  Left reason -> do
    expectationFailure $ "expected sponsorship reservation, got: " <> T.unpack reason
    fail "sponsorship reservation failed"
  Right authorization -> pure authorization

draft :: Char -> Char -> Char -> Integer -> Integer -> Integer -> SponsorshipDraft
draft requestCharacter digestCharacter clientCharacter nonce maxCost now =
  SponsorshipDraft
    { sdRequestKey = hashOf requestCharacter
    , sdDigest = hashOf digestCharacter
    , sdSender = addressOf '1'
    , sdOwner = addressOf '2'
    , sdNonce = nonce
    , sdValidAfter = max 0 $ now - 30
    , sdValidUntil = now + 300
    , sdMaxCostWei = maxCost
    , sdClientKey = clientKeyOf clientCharacter
    , sdOperation = object ["nonce" .= nonce]
    }

currentEpochSeconds :: IO Integer
currentEpochSeconds = floor <$> getPOSIXTime

hashOf :: Char -> Text
hashOf character = "0x" <> T.replicate 64 (T.singleton character)

clientKeyOf :: Char -> Text
clientKeyOf = hashOf

addressOf :: Char -> Text
addressOf character = "0x" <> T.replicate 40 (T.singleton character)

signatureOf :: Char -> Text
signatureOf character = "0x" <> T.replicate 130 (T.singleton character)

chainId :: Integer
chainId = 421614

paymasterAddress :: Text
paymasterAddress = addressOf '3'

deploymentBlockHash :: Text
deploymentBlockHash = hashOf '1'

testConfig :: NativeAaConfig
testConfig =
  NativeAaConfig
    { naaProxyOriginToken = T.replicate 64 "1"
    , naaAltoRpcUrl = "http://alto.invalid"
    , naaSecurityRpcUrl = "https://secondary-rpc.invalid"
    , naaRpcMode = DualIndependent
    , naaMaxSafeLagSeconds = 600
    , naaPaymasterAddress = paymasterAddress
    , naaPaymasterCodeHash = hashOf '4'
    , naaPolicyId = hashOf '5'
    , naaSignerAddress = addressOf '4'
    , naaKmsKeyId = "alias/integration-test"
    , naaAccountCodeHash = hashOf '6'
    , naaSponsorshipEnabled = True
    , naaPreparationEnabled = False
    , naaSubmissionEnabled = True
    , naaIpRateLimitPerMinute = 120
    , naaFinalRateLimitPerMinute = 6
    , naaAccountRateLimitPerMinute = 30
    , naaMaxRequestBytes = 262_144
    , naaValiditySeconds = 300
    , naaVerificationGasLimit = 100_000
    , naaPostOpGasLimit = 40_000
    , naaMaxCostWei = 10_000
    , naaAccountOutstandingWei = 20_000
    , naaClientOutstandingWei = 20_000
    , naaGlobalOutstandingWei = 100_000
    , naaAccountHourlyWei = 30_000
    , naaGlobalHourlyWei = 100_000
    , naaGlobalDailyWei = 250_000
    , naaCanaryOwners = [addressOf '2']
    , naaGlobalRolloutEnabled = False
    }
