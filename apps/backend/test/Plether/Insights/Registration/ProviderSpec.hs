module Plether.Insights.Registration.ProviderSpec (spec) where

import qualified Data.ByteString as BS
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import qualified Data.Text as T
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import Plether.Insights.Registration.Config (RegistrationConfig (..))
import Plether.Insights.Registration.Provider
import Plether.Insights.Registration.Types
  ( RegistrationError (..)
  , RegistrationErrorCode (..)
  , XIdentity (..)
  )
import Test.Hspec

spec :: Spec
spec = do
  describe "XAccessToken reconstruction" $ do
    it "accepts only bounded visible-ASCII bearer material and stays redacted" $ do
      fmap xAccessTokenBytes (xAccessTokenFromBytes "valid-token_123")
        `shouldBe` Just "valid-token_123"
      xAccessTokenFromBytes "line\nbreak" `shouldSatisfy` isNothing
      xAccessTokenFromBytes (BS.pack [0xc3, 0xa9]) `shouldSatisfy` isNothing
      fmap show (xAccessTokenFromBytes "valid-token_123")
        `shouldBe` Just "XAccessToken <redacted>"

  describe "parseTurnstileResponseAt" $ do
    it "accepts a successful, fresh response for the exact action and hostname" $ do
      parseTurnstileResponseAt testConfig turnstileNow turnstileSuccess
        `shouldBe`
          Right
            TurnstileEvidence
              { teChallengeTimestamp = UTCTime (fromGregorian 2026 9 1) (secondsToDiffTime 1)
              , teHostname = "insights.plether.com"
              , teAction = "competition_registration"
              }

    it "uses a half-open 300-second freshness window" $ do
      leftCode
        ( parseTurnstileResponseAt
            testConfig
            turnstileNow
            (turnstileBody "2026-09-01T00:00:00Z" "INSIGHTS.PLETHER.COM" "competition_registration")
        )
        `shouldBe` Just TurnstileFailed
      parseTurnstileResponseAt
        testConfig
        turnstileNow
        (turnstileBody "2026-09-01T00:00:00.001Z" "INSIGHTS.PLETHER.COM" "competition_registration")
        `shouldSatisfy` isRight

    it "rejects failed, stale, future-dated, wrong-host, and wrong-action evidence" $ do
      mapM_
        (\body -> leftCode (parseTurnstileResponseAt testConfig turnstileNow body) `shouldBe` Just TurnstileFailed)
        [ "{\"success\":false}"
        , turnstileBody "2026-08-31T23:59:59Z" "INSIGHTS.PLETHER.COM" "competition_registration"
        , turnstileBody "2026-09-01T00:05:31Z" "INSIGHTS.PLETHER.COM" "competition_registration"
        , turnstileBody "2026-09-01T00:00:00Z" "evil.example" "competition_registration"
        , turnstileBody "2026-09-01T00:00:00Z" "INSIGHTS.PLETHER.COM" "login"
        , "{\"success\":true,\"challenge_ts\":\"2026-09-01T00:00:00Z\"}"
        ]

    it "maps malformed provider JSON to a provider outage without reflecting details" $ do
      let result = parseTurnstileResponseAt testConfig turnstileNow "not-json"
      leftCode result `shouldBe` Just ProviderUnavailable
      leftMessage result `shouldBe` Just "Identity provider is temporarily unavailable"

  describe "X OAuth authorization URL" $ do
    it "pins callback, PKCE S256, state, and least-privilege scopes" $ do
      let result = buildXAuthorizationUrl testConfig competitionSlug oauthState pkceChallenge
      case result of
        Left err -> expectationFailure $ show err
        Right url -> do
          url `shouldSatisfy` T.isPrefixOf "https://x.com/i/oauth2/authorize?"
          url `shouldSatisfy` T.isInfixOf "response_type=code"
          url `shouldSatisfy` T.isInfixOf ("state=" <> oauthState)
          url `shouldSatisfy` T.isInfixOf ("code_challenge=" <> pkceChallenge)
          url `shouldSatisfy` T.isInfixOf "code_challenge_method=S256"
          url `shouldSatisfy` T.isInfixOf "scope=tweet.read%20users.read%20users.email%20follows.read"
          url `shouldSatisfy` T.isInfixOf "redirect_uri=https%3A%2F%2Finsights.plether.com%2Fapi%2Finsights%2Fv1%2Fcompetitions%2Ftestnet-trading-2026-09%2Fregistrations%2Fx%2Fcallback"

    it "rejects cross-competition callbacks and empty state or challenge" $ do
      leftCode (buildXAuthorizationUrl testConfig "another-competition" oauthState pkceChallenge)
        `shouldBe` Just InvalidRequest
      leftCode (buildXAuthorizationUrl testConfig competitionSlug "" pkceChallenge)
        `shouldBe` Just InvalidRequest
      leftCode (buildXAuthorizationUrl testConfig competitionSlug oauthState "")
        `shouldBe` Just InvalidRequest

  describe "parseXIdentityResponse" $ do
    it "parses stable identity fields and normalizes the confirmed email" $ do
      parseXIdentityResponse validXProfile
        `shouldBe`
          Right
            XIdentity
              { xiUserId = "1234567890123456789"
              , xiUsername = "Alice_1"
              , xiConfirmedEmail = "alice@example.com"
              , xiCreatedAt = 1_781_557_200
              }

    it "preserves the account-age cutoff conservatively at sub-second precision" $ do
      fmap xiCreatedAt (parseXIdentityResponse $ xProfileAt "2026-06-15T21:00:00Z")
        `shouldBe` Right 1_781_557_200
      fmap xiCreatedAt (parseXIdentityResponse $ xProfileAt "2026-06-15T21:00:00.001Z")
        `shouldBe` Right 1_781_557_201

    it "requires a confirmed, syntactically bounded email" $ do
      leftCode
        ( parseXIdentityResponse
            "{\"data\":{\"id\":\"123\",\"username\":\"alice\",\"created_at\":\"2026-06-15T21:00:00Z\"}}"
        )
        `shouldBe` Just XEmailUnverified
      leftCode
        ( parseXIdentityResponse
            "{\"data\":{\"id\":\"123\",\"username\":\"alice\",\"created_at\":\"2026-06-15T21:00:00Z\",\"confirmed_email\":\"alice@example\"}}"
        )
        `shouldBe` Just XEmailUnverified

    it "treats malformed stable IDs, usernames, timestamps, and JSON as provider failures" $ do
      mapM_
        (\body -> leftCode (parseXIdentityResponse body) `shouldBe` Just ProviderUnavailable)
        [ "{\"data\":{\"id\":\"12x\",\"username\":\"alice\",\"created_at\":\"2026-06-15T21:00:00Z\",\"confirmed_email\":\"alice@example.com\"}}"
        , "{\"data\":{\"id\":\"123\",\"username\":\"alice-name-is-too-long\",\"created_at\":\"2026-06-15T21:00:00Z\",\"confirmed_email\":\"alice@example.com\"}}"
        , "{\"data\":{\"id\":\"123\",\"username\":\"alice\",\"created_at\":\"yesterday\",\"confirmed_email\":\"alice@example.com\"}}"
        , "not-json"
        ]

  describe "parseXFollowLookupResponse" $ do
    it "accepts only a confirmed, non-pending relationship with the configured target" $ do
      parseXFollowLookupResponse targetXUserId "{\"data\":{\"id\":\"1234567890123456789\",\"connection_status\":[\"following\"]}}"
        `shouldBe` Right ()
      leftCode (parseXFollowLookupResponse targetXUserId "{\"data\":{\"id\":\"1234567890123456789\",\"connection_status\":[]}}")
        `shouldBe` Just XFollowRequired
      leftCode (parseXFollowLookupResponse targetXUserId "{\"data\":{\"id\":\"1234567890123456789\",\"connection_status\":[\"following\",\"follow_request_sent\"]}}")
        `shouldBe` Just XFollowRequired

    it "maps incomplete or malformed follow responses to provider outages" $ do
      leftCode (parseXFollowLookupResponse targetXUserId "{\"data\":{\"id\":\"1234567890123456789\"}}")
        `shouldBe` Just ProviderUnavailable
      leftCode (parseXFollowLookupResponse targetXUserId "{\"data\":{\"id\":\"999\",\"connection_status\":[\"following\"]}}")
        `shouldBe` Just ProviderUnavailable
      leftCode (parseXFollowLookupResponse targetXUserId "not-json")
        `shouldBe` Just ProviderUnavailable

    it "classifies privacy-safe response validation reasons without retaining provider content" $ do
      parseXFollowLookupResponseDetailed targetXUserId "not-json"
        `shouldBe` XFollowLookupInvalid XFollowInvalidJson
      parseXFollowLookupResponseDetailed targetXUserId "[]"
        `shouldBe` XFollowLookupInvalid XFollowResponseNotObject
      parseXFollowLookupResponseDetailed targetXUserId "{\"errors\":{}}"
        `shouldBe` XFollowLookupInvalid XFollowErrorsInvalid
      parseXFollowLookupResponseDetailed
        targetXUserId
        "{\"data\":{\"id\":\"1234567890123456789\",\"connection_status\":[\"following\"]},\"errors\":[{\"detail\":\"sensitive provider detail\"}]}"
        `shouldBe` XFollowLookupInvalid XFollowErrorsPresent
      parseXFollowLookupResponseDetailed targetXUserId "{}"
        `shouldBe` XFollowLookupInvalid XFollowDataMissing
      parseXFollowLookupResponseDetailed targetXUserId "{\"data\":[] }"
        `shouldBe` XFollowLookupInvalid XFollowDataInvalid
      parseXFollowLookupResponseDetailed targetXUserId "{\"data\":{\"connection_status\":[]}}"
        `shouldBe` XFollowLookupInvalid XFollowTargetIdMissing
      parseXFollowLookupResponseDetailed targetXUserId "{\"data\":{\"id\":1,\"connection_status\":[]}}"
        `shouldBe` XFollowLookupInvalid XFollowTargetIdInvalid
      parseXFollowLookupResponseDetailed targetXUserId "{\"data\":{\"id\":\"999\",\"connection_status\":[]}}"
        `shouldBe` XFollowLookupInvalid XFollowTargetMismatch
      parseXFollowLookupResponseDetailed targetXUserId "{\"data\":{\"id\":\"1234567890123456789\"}}"
        `shouldBe` XFollowLookupInvalid XFollowConnectionStatusMissing
      parseXFollowLookupResponseDetailed targetXUserId "{\"data\":{\"id\":\"1234567890123456789\",\"connection_status\":null}}"
        `shouldBe` XFollowLookupInvalid XFollowConnectionStatusInvalid

  describe "X follow provider retry" $ do
    it "classifies HTTP status retry and identity-reset behavior at the response boundary" $ do
      mapM_
        (\(httpStatus, retryable, resetIdentity) ->
          attemptFailureFlags
            (classifyXFollowResponse targetXUserId httpStatus [] "ignored")
            `shouldBe` Just (retryable, resetIdentity)
        )
        [ (401, False, True)
        , (403, False, False)
        , (408, True, False)
        , (429, False, False)
        , (500, True, False)
        ]

    it "classifies a partial HTTP-200 response as retryable without invalidating identity" $ do
      attemptFailureFlags
        ( classifyXFollowResponse
            targetXUserId
            200
            [("x-transaction-id", "safe-request-id")]
            "{\"data\":{\"id\":\"1234567890123456789\"}}"
        )
        `shouldBe` Just (True, False)

    it "retries one response-validation failure and preserves the verified identity when the retry succeeds" $ do
      calls <- newIORef
        [ XFollowAttemptProviderFailed $ responseValidationFailure XFollowConnectionStatusMissing
        , XFollowAttemptVerified
        ]
      events <- newIORef []
      result <- runXFollowVerificationWith
        (pure ())
        (\eventName attemptNumber _ -> modifyIORef' events (<> [(eventName, attemptNumber)]))
        (nextAttempt calls)
      result `shouldBe` Right ()
      readIORef calls `shouldReturn` []
      readIORef events `shouldReturn` [("registration_x_provider_retry", 1)]

    it "stops after one retry and preserves identity for repeated transient failures" $ do
      calls <- newIORef
        [ XFollowAttemptProviderFailed $ responseValidationFailure XFollowErrorsPresent
        , XFollowAttemptProviderFailed $ responseValidationFailure XFollowDataMissing
        , XFollowAttemptVerified
        ]
      events <- newIORef []
      result <- runXFollowVerificationWith
        (pure ())
        (\eventName attemptNumber _ -> modifyIORef' events (<> [(eventName, attemptNumber)]))
        (nextAttempt calls)
      leftVerificationReset result `shouldBe` Just False
      leftVerificationCode result `shouldBe` Just ProviderUnavailable
      readIORef calls `shouldReturn` [XFollowAttemptVerified]
      readIORef events
        `shouldReturn`
          [ ("registration_x_provider_retry", 1)
          , ("registration_x_provider_failure", 2)
          ]

    it "does not retry a definite bearer rejection and requests X reauthorization" $ do
      calls <- newIORef
        [ XFollowAttemptProviderFailed
            XFollowProviderFailure
              { xfpfKind = "http_status"
              , xfpfHttpStatus = Just 401
              , xfpfValidationReason = Nothing
              , xfpfRequestId = Just "safe-request-id"
              , xfpfRetryable = False
              , xfpfInvalidatesIdentity = True
              }
        , XFollowAttemptVerified
        ]
      result <- runXFollowVerificationWith (pure ()) (\_ _ _ -> pure ()) $ nextAttempt calls
      leftVerificationReset result `shouldBe` Just True
      readIORef calls `shouldReturn` [XFollowAttemptVerified]

    it "keeps retry and cleanup decisions deterministic when structured logging fails" $ do
      retryCalls <- newIORef
        [ XFollowAttemptProviderFailed $ responseValidationFailure XFollowDataMissing
        , XFollowAttemptVerified
        ]
      retryResult <- runXFollowVerificationWith
        (pure ())
        (\_ _ _ -> ioError $ userError "closed log handle")
        (nextAttempt retryCalls)
      retryResult `shouldBe` Right ()
      readIORef retryCalls `shouldReturn` []

      rejectionCalls <- newIORef
        [ XFollowAttemptProviderFailed
            XFollowProviderFailure
              { xfpfKind = "http_status"
              , xfpfHttpStatus = Just 401
              , xfpfValidationReason = Nothing
              , xfpfRequestId = Nothing
              , xfpfRetryable = False
              , xfpfInvalidatesIdentity = True
              }
        ]
      rejectionResult <- runXFollowVerificationWith
        (pure ())
        (\_ _ _ -> ioError $ userError "closed log handle")
        (nextAttempt rejectionCalls)
      leftVerificationReset rejectionResult `shouldBe` Just True

    it "accepts bounded provider request identifiers and rejects unsafe values" $ do
      providerRequestIdFromHeaders [("x-transaction-id", "abc-123_DEF.9")]
        `shouldBe` Just "abc-123_DEF.9"
      providerRequestIdFromHeaders [("x-request-id", "fallback:123")]
        `shouldBe` Just "fallback:123"
      providerRequestIdFromHeaders [("x-transaction-id", "unsafe/request")]
        `shouldBe` Nothing
      providerRequestIdFromHeaders
        [ ("x-transaction-id", "unsafe/request")
        , ("x-request-id", "safe-fallback")
        ]
        `shouldBe` Just "safe-fallback"

nextAttempt :: IORef [XFollowAttemptResult] -> IO XFollowAttemptResult
nextAttempt attempts = atomicModifyIORef' attempts takeNext
  where
    takeNext [] = error "test exhausted X follow attempts"
    takeNext (attempt : remaining) = (remaining, attempt)

responseValidationFailure :: XFollowValidationReason -> XFollowProviderFailure
responseValidationFailure validationReason =
  XFollowProviderFailure
    { xfpfKind = "response_validation"
    , xfpfHttpStatus = Just 200
    , xfpfValidationReason = Just validationReason
    , xfpfRequestId = Just "safe-request-id"
    , xfpfRetryable = True
    , xfpfInvalidatesIdentity = False
    }

leftVerificationReset :: Either XFollowVerificationFailure () -> Maybe Bool
leftVerificationReset result = case result of
  Left failure -> Just $ xfvfResetIdentity failure
  Right () -> Nothing

leftVerificationCode :: Either XFollowVerificationFailure () -> Maybe RegistrationErrorCode
leftVerificationCode result = case result of
  Left failure -> Just $ reCode $ xfvfError failure
  Right () -> Nothing

attemptFailureFlags :: XFollowAttemptResult -> Maybe (Bool, Bool)
attemptFailureFlags result = case result of
  XFollowAttemptProviderFailed failure ->
    Just (xfpfRetryable failure, xfpfInvalidatesIdentity failure)
  XFollowAttemptVerified -> Nothing
  XFollowAttemptNotConfirmed -> Nothing

turnstileNow :: UTCTime
turnstileNow = UTCTime (fromGregorian 2026 9 1) (secondsToDiffTime 300)

turnstileSuccess :: BS.ByteString
turnstileSuccess = turnstileBody "2026-09-01T00:00:01Z" "INSIGHTS.PLETHER.COM" "competition_registration"

turnstileBody :: BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString
turnstileBody timestamp hostname action =
  "{\"success\":true,\"challenge_ts\":\""
    <> timestamp
    <> "\",\"hostname\":\""
    <> hostname
    <> "\",\"action\":\""
    <> action
    <> "\"}"

validXProfile :: BS.ByteString
validXProfile =
  "{\"data\":{\"id\":\"1234567890123456789\",\"username\":\"Alice_1\",\"created_at\":\"2026-06-15T21:00:00Z\",\"confirmed_email\":\" Alice@Example.COM \"}}"

xProfileAt :: BS.ByteString -> BS.ByteString
xProfileAt createdAt =
  "{\"data\":{\"id\":\"1234567890123456789\",\"username\":\"Alice_1\",\"created_at\":\""
    <> createdAt
    <> "\",\"confirmed_email\":\"alice@example.com\"}}"

competitionSlug :: T.Text
competitionSlug = "testnet-trading-2026-09"

targetXUserId :: T.Text
targetXUserId = "1234567890123456789"

oauthState :: T.Text
oauthState = T.replicate 43 "s"

pkceChallenge :: T.Text
pkceChallenge = T.replicate 43 "c"

testConfig :: RegistrationConfig
testConfig =
  RegistrationConfig
    { rcActivationEnabled = True
    , rcPublicOrigin = "https://insights.plether.com"
    , rcOriginToken = BS.replicate 32 1
    , rcOriginTokenNext = Nothing
    , rcTurnstileSecretKey = "turnstile-secret"
    , rcTurnstileExpectedHostname = "insights.plether.com"
    , rcTurnstileExpectedAction = "competition_registration"
    , rcXClientId = "x-client-id"
    , rcXClientSecret = "x-client-secret"
    , rcXCallbackUrl = "https://insights.plether.com/api/insights/v1/competitions/testnet-trading-2026-09/registrations/x/callback"
    , rcXCallbackCompetitionSlug = competitionSlug
    , rcXTargetUserId = "1234567890123456789"
    , rcXTargetHandle = "plether_fi"
    , rcEmailKeys = Map.singleton "v1" $ BS.replicate 32 2
    , rcActiveEmailKeyVersion = "v1"
    , rcLookupHmacKey = BS.replicate 32 3
    , rcSessionTtlSeconds = 1800
    , rcIpRateLimitPerMinute = 10
    , rcSessionRateLimitPerMinute = 30
    , rcRulesVersion = "2026-09-v1"
    , rcPrivacyVersion = "2026-09-v1"
    , rcMinimumXAccountAgeDays = 30
    }

leftCode :: Either RegistrationError a -> Maybe RegistrationErrorCode
leftCode result = case result of
  Left err -> Just $ reCode err
  Right _ -> Nothing

leftMessage :: Either RegistrationError a -> Maybe T.Text
leftMessage result = case result of
  Left err -> Just $ reMessage err
  Right _ -> Nothing

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight (Left _) = False
