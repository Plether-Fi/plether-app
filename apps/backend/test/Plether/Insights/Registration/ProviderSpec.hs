module Plether.Insights.Registration.ProviderSpec (spec) where

import qualified Data.ByteString as BS
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
