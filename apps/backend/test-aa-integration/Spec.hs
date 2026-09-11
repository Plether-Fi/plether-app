module Main (main) where

import Control.Concurrent.Async (concurrently)
import Control.Exception (bracket, finally)
import Control.Monad (void)
import Data.Aeson (object, (.=))
import Data.Int (Int64)
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
import Plether.Database.AaSponsorship
  ( AaReconcilerCursor (..)
  , SponsorshipAuthorization (..)
  , SponsorshipDraft (..)
  , advanceAaReconcilerCursor
  , cancelStaleUnsignedReservations
  , consumeAaRateLimit
  , controlBootstrapReason
  , ensureAaSponsorshipSchema
  , expireSponsorshipsThrough
  , getAaIssuancePause
  , getSponsorshipByDigest
  , getSponsorshipByUserOperationHash
  , initializeAaReconcilerCursor
  , isRecoveryOperationAuthorized
  , isSponsorshipDeliveryAllowed
  , listSubmittedSponsorships
  , markSponsorshipSubmitted
  , pauseAaIssuance
  , recordAaReconcilerHeartbeat
  , reserveSponsorship
  , resumeAaIssuance
  , settleSponsorship
  , storeSponsorshipSignature
  )
import System.Environment (lookupEnv)
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
