module Plether.Insights.Registration.Types
  ( RegistrationErrorCode (..)
  , RegistrationError (..)
  , registrationError
  , registrationErrorCodeText
  , registrationErrorCodeFromText
  , registrationErrorStatus
  , RegistrationStatus (..)
  , VerificationStep (..)
  , RegistrationSteps (..)
  , RegistrationIdentityView (..)
  , RegistrationWalletView (..)
  , RequiredConsents (..)
  , RegistrationSessionView (..)
  , SessionRequest (..)
  , WalletChallengeRequest (..)
  , WalletVerifyRequest (..)
  , CompleteRegistrationRequest (..)
  , WalletChallengeResponse (..)
  , RegistrationResponse (..)
  , AuthorizationResponse (..)
  , XIdentity (..)
  ) where

import Data.Aeson
  ( FromJSON (..)
  , ToJSON (..)
  , Value
  , object
  , withObject
  , (.:)
  , (.:?)
  , (.=)
  )
import Data.Aeson.Types (Parser, (.!=))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Network.HTTP.Types.Status
  ( Status
  , status400
  , status401
  , status403
  , status404
  , status409
  , status429
  , status500
  , status503
  )

data RegistrationErrorCode
  = InvalidRequest
  | TurnstileFailed
  | OriginRejected
  | CsrfFailed
  | ClosedRegistration
  | ExpiredSession
  | ExpiredChallenge
  | XEmailUnverified
  | XAccountTooNew
  | XFollowRequired
  | DuplicateRegistration
  | TradingAccountExists
  | InvalidSignature
  | RegistrationIncomplete
  | ConsentMismatch
  | RateLimited
  | ProviderUnavailable
  | RegistrationNotFound
  | RegistrationInternalError
  deriving stock (Show, Eq, Generic)

instance ToJSON RegistrationErrorCode where
  toJSON = toJSON . registrationErrorCodeText

registrationErrorCodeText :: RegistrationErrorCode -> Text
registrationErrorCodeText = \case
  InvalidRequest -> "INVALID_REQUEST"
  TurnstileFailed -> "TURNSTILE_FAILED"
  OriginRejected -> "ORIGIN_REJECTED"
  CsrfFailed -> "CSRF_FAILED"
  ClosedRegistration -> "CLOSED_REGISTRATION"
  ExpiredSession -> "EXPIRED_SESSION"
  ExpiredChallenge -> "EXPIRED_CHALLENGE"
  XEmailUnverified -> "X_EMAIL_UNVERIFIED"
  XAccountTooNew -> "X_ACCOUNT_TOO_NEW"
  XFollowRequired -> "X_FOLLOW_REQUIRED"
  DuplicateRegistration -> "DUPLICATE_REGISTRATION"
  TradingAccountExists -> "TRADING_ACCOUNT_EXISTS"
  InvalidSignature -> "INVALID_SIGNATURE"
  RegistrationIncomplete -> "REGISTRATION_INCOMPLETE"
  ConsentMismatch -> "CONSENT_MISMATCH"
  RateLimited -> "RATE_LIMITED"
  ProviderUnavailable -> "PROVIDER_UNAVAILABLE"
  RegistrationNotFound -> "REGISTRATION_NOT_FOUND"
  RegistrationInternalError -> "INTERNAL_ERROR"

registrationErrorCodeFromText :: Text -> Maybe RegistrationErrorCode
registrationErrorCodeFromText = \case
  "INVALID_REQUEST" -> Just InvalidRequest
  "TURNSTILE_FAILED" -> Just TurnstileFailed
  "ORIGIN_REJECTED" -> Just OriginRejected
  "CSRF_FAILED" -> Just CsrfFailed
  "CLOSED_REGISTRATION" -> Just ClosedRegistration
  "EXPIRED_SESSION" -> Just ExpiredSession
  "EXPIRED_CHALLENGE" -> Just ExpiredChallenge
  "X_EMAIL_UNVERIFIED" -> Just XEmailUnverified
  "X_ACCOUNT_TOO_NEW" -> Just XAccountTooNew
  "X_FOLLOW_REQUIRED" -> Just XFollowRequired
  "DUPLICATE_REGISTRATION" -> Just DuplicateRegistration
  "TRADING_ACCOUNT_EXISTS" -> Just TradingAccountExists
  "INVALID_SIGNATURE" -> Just InvalidSignature
  "REGISTRATION_INCOMPLETE" -> Just RegistrationIncomplete
  "CONSENT_MISMATCH" -> Just ConsentMismatch
  "RATE_LIMITED" -> Just RateLimited
  "PROVIDER_UNAVAILABLE" -> Just ProviderUnavailable
  "REGISTRATION_NOT_FOUND" -> Just RegistrationNotFound
  "INTERNAL_ERROR" -> Just RegistrationInternalError
  _ -> Nothing

data RegistrationError = RegistrationError
  { reCode :: RegistrationErrorCode
  , reMessage :: Text
  , reDetails :: Maybe Value
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON RegistrationError where
  toJSON err =
    object
      [ "error"
          .= object
            ( [ "code" .= reCode err
              , "message" .= reMessage err
              ]
                <> maybe [] (\details -> ["details" .= details]) (reDetails err)
            )
      ]

registrationError :: RegistrationErrorCode -> Text -> RegistrationError
registrationError code message = RegistrationError code message Nothing

registrationErrorStatus :: RegistrationError -> Status
registrationErrorStatus err = case reCode err of
  InvalidRequest -> status400
  TurnstileFailed -> status403
  OriginRejected -> status403
  CsrfFailed -> status403
  ClosedRegistration -> status409
  ExpiredSession -> status401
  ExpiredChallenge -> status400
  XEmailUnverified -> status403
  XAccountTooNew -> status403
  XFollowRequired -> status403
  DuplicateRegistration -> status409
  TradingAccountExists -> status409
  InvalidSignature -> status400
  RegistrationIncomplete -> status409
  ConsentMismatch -> status400
  RateLimited -> status429
  ProviderUnavailable -> status503
  RegistrationNotFound -> status404
  RegistrationInternalError -> status500

data RegistrationStatus = RegistrationInProgress | RegistrationCompleted
  deriving stock (Show, Eq, Generic)

instance ToJSON RegistrationStatus where
  toJSON RegistrationInProgress = "in_progress"
  toJSON RegistrationCompleted = "completed"

data VerificationStep = StepPending | StepVerified
  deriving stock (Show, Eq, Generic)

instance ToJSON VerificationStep where
  toJSON StepPending = "pending"
  toJSON StepVerified = "verified"

data RegistrationSteps = RegistrationSteps
  { rsXIdentity :: VerificationStep
  , rsXFollow :: VerificationStep
  , rsWallet :: VerificationStep
  , rsCompleted :: Bool
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON RegistrationSteps where
  toJSON steps =
    object
      [ "xIdentity" .= rsXIdentity steps
      , "xFollow" .= rsXFollow steps
      , "wallet" .= rsWallet steps
      , "completed" .= rsCompleted steps
      ]

data RegistrationIdentityView = RegistrationIdentityView
  { rivXHandle :: Text
  , rivMaskedEmail :: Text
  }
  deriving stock (Eq, Generic)

instance Show RegistrationIdentityView where
  show identity =
    "RegistrationIdentityView {rivXHandle = "
      <> show (rivXHandle identity)
      <> ", rivMaskedEmail = <redacted>}"

instance ToJSON RegistrationIdentityView where
  toJSON identity =
    object
      [ "xHandle" .= rivXHandle identity
      , "maskedEmail" .= rivMaskedEmail identity
      ]

data RegistrationWalletView = RegistrationWalletView
  { rwvOwnerAddress :: Text
  , rwvTradingAccount :: Text
  }
  deriving stock (Eq, Generic)

instance Show RegistrationWalletView where
  show _ = "RegistrationWalletView {rwvOwnerAddress = <redacted>, rwvTradingAccount = <redacted>}"

instance ToJSON RegistrationWalletView where
  toJSON wallet =
    object
      [ "ownerAddress" .= rwvOwnerAddress wallet
      , "tradingAccount" .= rwvTradingAccount wallet
      ]

data RequiredConsents = RequiredConsents
  { rcRules :: Text
  , rcPrivacy :: Text
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON RequiredConsents where
  toJSON consents =
    object
      [ "rulesVersion" .= rcRules consents
      , "privacyVersion" .= rcPrivacy consents
      ]

data RegistrationSessionView = RegistrationSessionView
  { rsvStatus :: RegistrationStatus
  , rsvCsrfToken :: Text
  , rsvExpiresAt :: Text
  , rsvSteps :: RegistrationSteps
  , rsvIdentity :: Maybe RegistrationIdentityView
  , rsvWallet :: Maybe RegistrationWalletView
  , rsvOauthErrorCode :: Maybe RegistrationErrorCode
  , rsvRequiredConsents :: RequiredConsents
  }
  deriving stock (Eq, Generic)

instance Show RegistrationSessionView where
  show session =
    "RegistrationSessionView {rsvStatus = "
      <> show (rsvStatus session)
      <> ", rsvCsrfToken = <redacted>, rsvExpiresAt = "
      <> show (rsvExpiresAt session)
      <> ", rsvSteps = "
      <> show (rsvSteps session)
      <> ", rsvIdentity = <redacted>, rsvWallet = <redacted>, rsvOauthErrorCode = "
      <> show (rsvOauthErrorCode session)
      <> ", rsvRequiredConsents = "
      <> show (rsvRequiredConsents session)
      <> "}"

instance ToJSON RegistrationSessionView where
  toJSON session =
    object
      [ "status" .= rsvStatus session
      , "csrfToken" .= rsvCsrfToken session
      , "expiresAt" .= rsvExpiresAt session
      , "steps" .= rsvSteps session
      , "identity" .= rsvIdentity session
      , "wallet" .= rsvWallet session
      , "oauthErrorCode" .= rsvOauthErrorCode session
      , "requiredConsents" .= rsvRequiredConsents session
      ]

newtype SessionRequest = SessionRequest {srTurnstileToken :: Text}

instance FromJSON SessionRequest where
  parseJSON = withObject "SessionRequest" $ \objectValue -> do
    token <- objectValue .: "turnstileToken"
    SessionRequest <$> boundedText "turnstileToken" 1 2048 token

newtype WalletChallengeRequest = WalletChallengeRequest {wcrOwnerAddress :: Text}

instance FromJSON WalletChallengeRequest where
  parseJSON = withObject "WalletChallengeRequest" $ \objectValue -> do
    owner <- objectValue .: "ownerAddress"
    WalletChallengeRequest <$> boundedText "ownerAddress" 42 42 owner

data WalletVerifyRequest = WalletVerifyRequest
  { wvrOwnerAddress :: Text
  , wvrSignature :: Text
  }

instance FromJSON WalletVerifyRequest where
  parseJSON = withObject "WalletVerifyRequest" $ \objectValue ->
    WalletVerifyRequest
      <$> (objectValue .: "ownerAddress" >>= boundedText "ownerAddress" 42 42)
      <*> (objectValue .: "signature" >>= boundedText "signature" 132 132)

data CompleteRegistrationRequest = CompleteRegistrationRequest
  { crrAcceptRules :: Bool
  , crrAcceptPrivacy :: Bool
  , crrAcceptPromotionalEmail :: Bool
  , crrRulesVersion :: Text
  , crrPrivacyVersion :: Text
  }

instance FromJSON CompleteRegistrationRequest where
  parseJSON = withObject "CompleteRegistrationRequest" $ \objectValue ->
    CompleteRegistrationRequest
      <$> objectValue .: "acceptRules"
      <*> objectValue .: "acceptPrivacy"
      <*> (objectValue .:? "acceptPromotionalEmail" .!= False)
      <*> (objectValue .: "rulesVersion" >>= boundedText "rulesVersion" 1 64)
      <*> (objectValue .: "privacyVersion" >>= boundedText "privacyVersion" 1 64)

data WalletChallengeResponse = WalletChallengeResponse
  { wcresMessage :: Text
  , wcresExpiresAt :: Text
  }
  deriving stock (Eq, Generic)

instance Show WalletChallengeResponse where
  show response =
    "WalletChallengeResponse {wcresMessage = <redacted>, wcresExpiresAt = "
      <> show (wcresExpiresAt response)
      <> "}"

instance ToJSON WalletChallengeResponse where
  toJSON challenge =
    object
      [ "message" .= wcresMessage challenge
      , "expiresAt" .= wcresExpiresAt challenge
      ]

newtype RegistrationResponse = RegistrationResponse
  { rrRegistration :: RegistrationSessionView
  }

instance ToJSON RegistrationResponse where
  toJSON response = object ["registration" .= rrRegistration response]

newtype AuthorizationResponse = AuthorizationResponse
  { arAuthorizationUrl :: Text
  }

instance ToJSON AuthorizationResponse where
  toJSON response = object ["authorizationUrl" .= arAuthorizationUrl response]

data XIdentity = XIdentity
  { xiUserId :: Text
  , xiUsername :: Text
  , xiConfirmedEmail :: Text
  , xiCreatedAt :: Integer
  }
  deriving stock (Eq, Generic)

instance Show XIdentity where
  show identity =
    "XIdentity {xiUserId = <redacted>, xiUsername = "
      <> show (xiUsername identity)
      <> ", xiConfirmedEmail = <redacted>, xiCreatedAt = "
      <> show (xiCreatedAt identity)
      <> "}"

boundedText :: String -> Int -> Int -> Text -> Parser Text
boundedText label minimumLength maximumLength value
  | T.length value < minimumLength || T.length value > maximumLength =
      fail $ label <> " length is invalid"
  | otherwise = pure value
