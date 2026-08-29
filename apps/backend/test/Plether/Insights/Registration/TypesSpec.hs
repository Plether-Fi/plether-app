module Plether.Insights.Registration.TypesSpec (spec) where

import Data.Aeson
  ( FromJSON
  , Value (..)
  , eitherDecode
  , encode
  , object
  , toJSON
  , (.=)
  )
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Char (isDigit)
import qualified Data.Text as T
import Plether.Insights.Registration.Types
import Test.Hspec

spec :: Spec
spec = do
  describe "registration error JSON and HTTP status mapping" $ do
    it "encodes every stable public error code exactly" $
      mapM_
        (\(code, encoded) -> do
          toJSON code `shouldBe` toJSON (encoded :: T.Text)
          registrationErrorCodeText code `shouldBe` encoded
          registrationErrorCodeFromText encoded `shouldBe` Just code
        )
        errorCodeVectors

    it "rejects unknown persisted callback error codes" $
      registrationErrorCodeFromText "X_PROVIDER_DETAIL_123" `shouldBe` Nothing

    it "maps every error class to its intended HTTP status" $
      mapM_
        (\(code, expectedStatus) -> statusNumber (registrationErrorStatus $ registrationError code "safe") `shouldBe` expectedStatus)
        errorStatusVectors

    it "wraps safe errors and omits absent details" $ do
      toJSON (registrationError DuplicateRegistration "Registration details are already in use")
        `shouldBe`
          object
            [ "error"
                .= object
                  [ "code" .= ("DUPLICATE_REGISTRATION" :: T.Text)
                  , "message" .= ("Registration details are already in use" :: T.Text)
                  ]
            ]

    it "includes explicitly supplied structured details" $ do
      let err =
            RegistrationError
              RateLimited
              "Please retry later"
              (Just $ object ["retryAfter" .= (60 :: Int)])
      toJSON err
        `shouldBe`
          object
            [ "error"
                .= object
                  [ "code" .= ("RATE_LIMITED" :: T.Text)
                  , "message" .= ("Please retry later" :: T.Text)
                  , "details" .= object ["retryAfter" .= (60 :: Int)]
                  ]
            ]

  describe "registration request field bounds" $ do
    it "accepts Turnstile tokens at both declared boundaries" $ do
      fmap srTurnstileToken (decodeJson $ object ["turnstileToken" .= ("x" :: T.Text)] :: Either String SessionRequest)
        `shouldBe` Right "x"
      fmap (T.length . srTurnstileToken)
        (decodeJson (object ["turnstileToken" .= T.replicate 2048 "x"]) :: Either String SessionRequest)
        `shouldBe` Right 2048

    it "rejects empty or oversized Turnstile tokens" $ do
      isLeft (decodeJson (object ["turnstileToken" .= ("" :: T.Text)]) :: Either String SessionRequest)
        `shouldBe` True
      isLeft (decodeJson (object ["turnstileToken" .= T.replicate 2049 "x"]) :: Either String SessionRequest)
        `shouldBe` True

    it "requires a 42-character owner address in challenge requests" $ do
      fmap wcrOwnerAddress
        (decodeJson (object ["ownerAddress" .= T.replicate 42 "a"]) :: Either String WalletChallengeRequest)
        `shouldBe` Right (T.replicate 42 "a")
      isLeft (decodeJson (object ["ownerAddress" .= T.replicate 41 "a"]) :: Either String WalletChallengeRequest)
        `shouldBe` True
      isLeft (decodeJson (object ["ownerAddress" .= T.replicate 43 "a"]) :: Either String WalletChallengeRequest)
        `shouldBe` True

    it "requires an exact 0x-prefixed 65-byte wallet proof signature length" $ do
      fmap (T.length . wvrSignature)
        ( decodeJson
            ( object
                [ "ownerAddress" .= T.replicate 42 "a"
                , "signature" .= T.replicate 132 "b"
                ]
            ) :: Either String WalletVerifyRequest
        )
        `shouldBe` Right 132
      mapM_
        (\lengthValue ->
          isLeft
            ( decodeJson
                ( object
                    [ "ownerAddress" .= T.replicate 42 "a"
                    , "signature" .= T.replicate lengthValue "b"
                    ]
                ) :: Either String WalletVerifyRequest
            )
            `shouldBe` True
        )
        [129, 130, 131, 133]

    it "requires explicit consent flags and versions bounded to 64 characters" $ do
      let valid =
            object
              [ "acceptRules" .= True
              , "acceptPrivacy" .= True
              , "rulesVersion" .= T.replicate 64 "r"
              , "privacyVersion" .= ("v1" :: T.Text)
              ]
      fmap crrAcceptRules (decodeJson valid :: Either String CompleteRegistrationRequest)
        `shouldBe` Right True
      isLeft
        ( decodeJson
            ( object
                [ "acceptRules" .= True
                , "acceptPrivacy" .= True
                , "rulesVersion" .= ("" :: T.Text)
                , "privacyVersion" .= ("v1" :: T.Text)
                ]
            ) :: Either String CompleteRegistrationRequest
        )
        `shouldBe` True
      isLeft
        ( decodeJson
            ( object
                [ "acceptRules" .= True
                , "acceptPrivacy" .= True
                , "rulesVersion" .= T.replicate 65 "r"
                , "privacyVersion" .= ("v1" :: T.Text)
                ]
            ) :: Either String CompleteRegistrationRequest
        )
        `shouldBe` True

  describe "owning-session response shape" $ do
    it "uses stable status, step, consent, and ISO-8601 timestamp fields" $ do
      toJSON sessionView
        `shouldBe`
          object
            [ "status" .= ("in_progress" :: T.Text)
            , "csrfToken" .= ("csrf-token" :: T.Text)
            , "expiresAt" .= ("2026-09-01T00:30:00Z" :: T.Text)
            , "steps"
                .= object
                  [ "xIdentity" .= ("verified" :: T.Text)
                  , "xFollow" .= ("pending" :: T.Text)
                  , "wallet" .= ("verified" :: T.Text)
                  , "completed" .= False
                  ]
            , "identity"
                .= object
                  [ "xHandle" .= ("alice" :: T.Text)
                  , "maskedEmail" .= ("a***@example.com" :: T.Text)
                  ]
            , "wallet"
                .= object
                  [ "ownerAddress" .= ("0x1111111111111111111111111111111111111111" :: T.Text)
                  , "tradingAccount" .= ("0x2222222222222222222222222222222222222222" :: T.Text)
                  ]
            , "oauthErrorCode" .= (Nothing :: Maybe RegistrationErrorCode)
            , "requiredConsents"
                .= object
                  [ "rulesVersion" .= ("2026-09-v1" :: T.Text)
                  , "privacyVersion" .= ("2026-09-v1" :: T.Text)
                  ]
            ]

    it "exposes only a stable OAuth callback error code to the owning session" $ do
      let failed = sessionView {rsvOauthErrorCode = Just XEmailUnverified}
      case toJSON failed of
        Object value ->
          lookupJson "oauthErrorCode" value `shouldBe` Just (String "X_EMAIL_UNVERIFIED")
        _ -> expectationFailure "session response was not a JSON object"

    it "wraps registration and authorization responses without private identity fields" $ do
      toJSON (RegistrationResponse sessionView)
        `shouldBe` object ["registration" .= sessionView]
      toJSON (AuthorizationResponse "https://x.com/i/oauth2/authorize?state=opaque")
        `shouldBe` object ["authorizationUrl" .= ("https://x.com/i/oauth2/authorize?state=opaque" :: T.Text)]

    it "emits wallet challenge expiry as an ISO-8601 string" $ do
      toJSON (WalletChallengeResponse "message" "2026-09-01T00:05:00Z")
        `shouldBe`
          object
            [ "message" .= ("message" :: T.Text)
            , "expiresAt" .= ("2026-09-01T00:05:00Z" :: T.Text)
            ]

    it "redacts session identity, wallet, CSRF, challenge, and X private fields from Show" $ do
      let sessionRendered = show sessionView
          identityRendered = show $ XIdentity "123456789" "alice" "alice@example.com" 1_781_557_200
          challengeRendered = show $ WalletChallengeResponse "secret challenge" "2026-09-01T00:05:00Z"
      sessionRendered `shouldNotContain` "csrf-token"
      sessionRendered `shouldNotContain` "alice"
      sessionRendered `shouldNotContain` "0x1111111111111111111111111111111111111111"
      identityRendered `shouldNotContain` "123456789"
      identityRendered `shouldNotContain` "alice@example.com"
      challengeRendered `shouldNotContain` "secret challenge"

sessionView :: RegistrationSessionView
sessionView =
  RegistrationSessionView
    { rsvStatus = RegistrationInProgress
    , rsvCsrfToken = "csrf-token"
    , rsvExpiresAt = "2026-09-01T00:30:00Z"
    , rsvSteps =
        RegistrationSteps
          { rsXIdentity = StepVerified
          , rsXFollow = StepPending
          , rsWallet = StepVerified
          , rsCompleted = False
          }
    , rsvIdentity = Just $ RegistrationIdentityView "alice" "a***@example.com"
    , rsvWallet =
        Just $
          RegistrationWalletView
            "0x1111111111111111111111111111111111111111"
            "0x2222222222222222222222222222222222222222"
    , rsvOauthErrorCode = Nothing
    , rsvRequiredConsents = RequiredConsents "2026-09-v1" "2026-09-v1"
    }

errorCodeVectors :: [(RegistrationErrorCode, T.Text)]
errorCodeVectors =
  [ (InvalidRequest, "INVALID_REQUEST")
  , (TurnstileFailed, "TURNSTILE_FAILED")
  , (OriginRejected, "ORIGIN_REJECTED")
  , (CsrfFailed, "CSRF_FAILED")
  , (ClosedRegistration, "CLOSED_REGISTRATION")
  , (ExpiredSession, "EXPIRED_SESSION")
  , (ExpiredChallenge, "EXPIRED_CHALLENGE")
  , (XEmailUnverified, "X_EMAIL_UNVERIFIED")
  , (XAccountTooNew, "X_ACCOUNT_TOO_NEW")
  , (XFollowRequired, "X_FOLLOW_REQUIRED")
  , (DuplicateRegistration, "DUPLICATE_REGISTRATION")
  , (TradingAccountExists, "TRADING_ACCOUNT_EXISTS")
  , (InvalidSignature, "INVALID_SIGNATURE")
  , (RegistrationIncomplete, "REGISTRATION_INCOMPLETE")
  , (ConsentMismatch, "CONSENT_MISMATCH")
  , (RateLimited, "RATE_LIMITED")
  , (ProviderUnavailable, "PROVIDER_UNAVAILABLE")
  , (RegistrationNotFound, "REGISTRATION_NOT_FOUND")
  , (RegistrationInternalError, "INTERNAL_ERROR")
  ]

errorStatusVectors :: [(RegistrationErrorCode, Int)]
errorStatusVectors =
  [ (InvalidRequest, 400)
  , (TurnstileFailed, 403)
  , (OriginRejected, 403)
  , (CsrfFailed, 403)
  , (ClosedRegistration, 409)
  , (ExpiredSession, 401)
  , (ExpiredChallenge, 400)
  , (XEmailUnverified, 403)
  , (XAccountTooNew, 403)
  , (XFollowRequired, 403)
  , (DuplicateRegistration, 409)
  , (TradingAccountExists, 409)
  , (InvalidSignature, 400)
  , (RegistrationIncomplete, 409)
  , (ConsentMismatch, 400)
  , (RateLimited, 429)
  , (ProviderUnavailable, 503)
  , (RegistrationNotFound, 404)
  , (RegistrationInternalError, 500)
  ]

decodeJson :: FromJSON a => Value -> Either String a
decodeJson = eitherDecode . encode

statusNumber :: Show status => status -> Int
statusNumber = read . takeWhile isDigit . dropWhile (not . isDigit) . show

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

lookupJson :: T.Text -> KeyMap.KeyMap Value -> Maybe Value
lookupJson key = KeyMap.lookup $ AesonKey.fromText key
