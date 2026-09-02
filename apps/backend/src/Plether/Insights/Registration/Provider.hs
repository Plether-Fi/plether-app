module Plether.Insights.Registration.Provider
  ( XAccessToken
  , xAccessTokenBytes
  , xAccessTokenFromBytes
  , TurnstileEvidence (..)
  , verifyTurnstile
  , buildXAuthorizationUrl
  , exchangeXAuthorizationCode
  , fetchXIdentity
  , XFollowAttemptResult (..)
  , XFollowLookupResult (..)
  , XFollowProviderFailure (..)
  , XFollowValidationReason (..)
  , XFollowVerificationFailure (..)
  , verifyXFollow
  , runXFollowVerificationWith
  , classifyXFollowResponse
  , parseTurnstileResponseAt
  , parseXIdentityResponse
  , parseXFollowLookupResponse
  , parseXFollowLookupResponseDetailed
  , providerRequestIdFromHeaders
  ) where

import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import Control.Exception
  ( SomeAsyncException
  , SomeException
  , fromException
  , throwIO
  , try
  )
import Data.Aeson
  ( FromJSON (..)
  , Value (..)
  , eitherDecodeStrict'
  , withObject
  , (.:)
  , (.:?)
  , (.!=)
  )
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import Data.Char (isAlphaNum, isAscii, isControl, isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime, diffUTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Network.HTTP.Client
  ( BodyReader
  , Manager
  , Request (..)
  , RequestBody (..)
  , applyBasicAuth
  , brRead
  , checkResponse
  , parseRequest
  , responseBody
  , responseHeaders
  , responseStatus
  , responseTimeoutMicro
  , withResponse
  )
import Network.HTTP.Types.Status (statusCode)
import Network.HTTP.Types.Header (Header)
import Network.HTTP.Types.URI (renderSimpleQuery)
import qualified Data.Vector as Vector
import Plether.Insights.Registration.Config (RegistrationConfig (..))
import Plether.Insights.Registration.Types
  ( RegistrationError (..)
  , RegistrationErrorCode (..)
  , XIdentity (..)
  , registrationError
  )
import Plether.Logging (field, logWarn)

newtype XAccessToken = XAccessToken BS.ByteString

instance Show XAccessToken where
  show _ = "XAccessToken <redacted>"

xAccessTokenBytes :: XAccessToken -> BS.ByteString
xAccessTokenBytes (XAccessToken token) = token

xAccessTokenFromBytes :: BS.ByteString -> Maybe XAccessToken
xAccessTokenFromBytes token
  | BS.null token || BS.length token > 8192 = Nothing
  | BS.any (\byte -> byte <= 32 || byte >= 127) token = Nothing
  | otherwise = Just $ XAccessToken token

data TurnstileEvidence = TurnstileEvidence
  { teChallengeTimestamp :: UTCTime
  , teHostname :: Text
  , teAction :: Text
  }
  deriving stock (Show, Eq)

data TurnstileResponse = TurnstileResponse
  { trSuccess :: Bool
  , trChallengeTimestamp :: Maybe UTCTime
  , trHostname :: Maybe Text
  , trAction :: Maybe Text
  }

instance FromJSON TurnstileResponse where
  parseJSON = withObject "TurnstileResponse" $ \value ->
    TurnstileResponse
      <$> value .: "success"
      <*> value .:? "challenge_ts"
      <*> value .:? "hostname"
      <*> value .:? "action"

verifyTurnstile
  :: Manager
  -> RegistrationConfig
  -> UTCTime
  -> Text
  -> Text
  -> Text
  -> IO (Either RegistrationError TurnstileEvidence)
verifyTurnstile manager config now remoteIp idempotencyKey token
  | token /= T.strip token
      || T.null token
      || T.length token > 2048
      || T.any (\character -> isControl character || isSpace character) token =
      pure $ Left $ registrationError InvalidRequest "Turnstile response is invalid"
  | not $ validUuid idempotencyKey =
      pure $ Left $ registrationError InvalidRequest "Turnstile request identity is invalid"
  | otherwise = do
      request <- parseRequest "https://challenges.cloudflare.com/turnstile/v0/siteverify"
      let form =
            renderSimpleQuery
              False
              [ ("secret", TE.encodeUtf8 $ rcTurnstileSecretKey config)
              , ("response", TE.encodeUtf8 token)
              , ("remoteip", TE.encodeUtf8 remoteIp)
              , ("idempotency_key", TE.encodeUtf8 idempotencyKey)
              ]
          prepared = secureRequest request
            { method = "POST"
            , requestHeaders =
                [ ("Content-Type", "application/x-www-form-urlencoded")
                , ("Accept", "application/json")
                ]
            , requestBody = RequestBodyBS form
            }
      outcome <- performBounded manager prepared
      pure $ do
        (httpStatus, body) <- outcome
        if httpStatus < 200 || httpStatus >= 300
          then Left providerUnavailable
          else parseTurnstileResponseAt config now body

parseTurnstileResponseAt
  :: RegistrationConfig
  -> UTCTime
  -> BS.ByteString
  -> Either RegistrationError TurnstileEvidence
parseTurnstileResponseAt config now body = do
  response <-
    either
      (const $ Left providerUnavailable)
      Right
      (eitherDecodeStrict' body)
  let failure = Left $ registrationError TurnstileFailed "Spam-protection verification failed"
  if not $ trSuccess response
    then failure
    else case (trChallengeTimestamp response, trHostname response, trAction response) of
      (Just challengeAt, Just hostname, Just action)
        | T.toLower hostname /= rcTurnstileExpectedHostname config -> failure
        | action /= rcTurnstileExpectedAction config -> failure
        | diffUTCTime now challengeAt < (-30) -> failure
        | diffUTCTime now challengeAt >= 300 -> failure
        | otherwise ->
            Right
              TurnstileEvidence
                { teChallengeTimestamp = challengeAt
                , teHostname = T.toLower hostname
                , teAction = action
                }
      _ -> failure

buildXAuthorizationUrl
  :: RegistrationConfig
  -> Text
  -> Text
  -> Text
  -> Either RegistrationError Text
buildXAuthorizationUrl config competitionSlug state codeChallenge
  | competitionSlug /= rcXCallbackCompetitionSlug config =
      Left $ registrationError InvalidRequest "OAuth callback competition does not match"
  | not (validBase64Url43 state) || not (validBase64Url43 codeChallenge) =
      Left $ registrationError InvalidRequest "OAuth state is invalid"
  | otherwise =
      Right $
        "https://x.com/i/oauth2/authorize"
          <> TE.decodeUtf8
            ( renderSimpleQuery
                True
                [ ("response_type", "code")
                , ("client_id", TE.encodeUtf8 $ rcXClientId config)
                , ("redirect_uri", TE.encodeUtf8 $ rcXCallbackUrl config)
                , ("scope", "tweet.read users.read users.email follows.read")
                , ("state", TE.encodeUtf8 state)
                , ("code_challenge", TE.encodeUtf8 codeChallenge)
                , ("code_challenge_method", "S256")
                ]
            )

data XTokenResponse = XTokenResponse
  { xtrAccessToken :: Text
  , xtrTokenType :: Text
  , xtrScope :: Text
  , xtrExpiresIn :: Maybe Integer
  , xtrRefreshToken :: Maybe Text
  }

instance FromJSON XTokenResponse where
  parseJSON = withObject "XTokenResponse" $ \value ->
    XTokenResponse
      <$> value .: "access_token"
      <*> value .: "token_type"
      <*> value .: "scope"
      <*> value .:? "expires_in"
      <*> value .:? "refresh_token"

exchangeXAuthorizationCode
  :: Manager
  -> RegistrationConfig
  -> Text
  -> Text
  -> IO (Either RegistrationError XAccessToken)
exchangeXAuthorizationCode manager config code verifier
  | not (validAuthorizationCode code) || not (validPkceVerifier verifier) =
      pure $ Left $ registrationError InvalidRequest "OAuth callback is invalid"
  | otherwise = do
      request <- parseRequest "https://api.x.com/2/oauth2/token"
      let form =
            renderSimpleQuery
              False
              [ ("grant_type", "authorization_code")
              , ("code", TE.encodeUtf8 code)
              , ("redirect_uri", TE.encodeUtf8 $ rcXCallbackUrl config)
              , ("code_verifier", TE.encodeUtf8 verifier)
              ]
          authenticated =
            applyBasicAuth
              (TE.encodeUtf8 $ rcXClientId config)
              (TE.encodeUtf8 $ rcXClientSecret config)
              request
          prepared = secureRequest authenticated
            { method = "POST"
            , requestHeaders =
                ("Content-Type", "application/x-www-form-urlencoded")
                  : ("Accept", "application/json")
                  : requestHeaders authenticated
            , requestBody = RequestBodyBS form
            }
      outcome <- performBounded manager prepared
      let result = do
            (httpStatus, body) <- outcome
            if httpStatus < 200 || httpStatus >= 300
              then Left providerUnavailable
              else do
                tokenResponse <- either (const $ Left providerUnavailable) Right $ eitherDecodeStrict' body
                let grantedScopes = T.words $ xtrScope tokenResponse
                    requiredScopes = ["tweet.read", "users.read", "users.email", "follows.read"]
                if T.toCaseFold (xtrTokenType tokenResponse) /= "bearer"
                    || T.null (xtrAccessToken tokenResponse)
                    || T.length (xtrAccessToken tokenResponse) > 8192
                    || any (`notElem` grantedScopes) requiredScopes
                    || "offline.access" `elem` grantedScopes
                    || xtrRefreshToken tokenResponse /= Nothing
                    || maybe False (\seconds -> seconds <= 0 || seconds > 86_400) (xtrExpiresIn tokenResponse)
                  then Left providerUnavailable
                  else maybe (Left providerUnavailable) Right $
                    xAccessTokenFromBytes $ TE.encodeUtf8 $ xtrAccessToken tokenResponse
      logXProviderFailure "oauth_token" outcome result
      pure result

data XProfileResponse = XProfileResponse (Maybe XProfile) [Value]

instance FromJSON XProfileResponse where
  parseJSON = withObject "XProfileResponse" $ \value ->
    XProfileResponse
      <$> value .:? "data"
      <*> (value .:? "errors" .!= [])

data XProfile = XProfile
  { xpId :: Text
  , xpUsername :: Text
  , xpCreatedAt :: UTCTime
  , xpConfirmedEmail :: Maybe Text
  }

instance FromJSON XProfile where
  parseJSON = withObject "XProfile" $ \value ->
    XProfile
      <$> value .: "id"
      <*> value .: "username"
      <*> value .: "created_at"
      <*> value .:? "confirmed_email"

fetchXIdentity
  :: Manager
  -> XAccessToken
  -> IO (Either RegistrationError XIdentity)
fetchXIdentity manager token = do
  request <- parseRequest "https://api.x.com/2/users/me?user.fields=confirmed_email%2Ccreated_at%2Cusername"
  let prepared = bearerRequest token request
  outcome <- performBounded manager prepared
  let result = do
        (httpStatus, body) <- outcome
        if httpStatus < 200 || httpStatus >= 300
          then Left providerUnavailable
          else parseXIdentityResponse body
  logXProviderFailure "authenticated_user" outcome result
  pure result

parseXIdentityResponse :: BS.ByteString -> Either RegistrationError XIdentity
parseXIdentityResponse body = do
  XProfileResponse maybeProfile apiErrors <-
    either (const $ Left providerUnavailable) Right $ eitherDecodeStrict' body
  if null apiErrors then pure () else Left providerUnavailable
  profile <- maybe (Left providerUnavailable) Right maybeProfile
  if not $ validXId $ xpId profile
    then Left providerUnavailable
    else pure ()
  if not $ validXUsername $ xpUsername profile
    then Left providerUnavailable
    else pure ()
  case T.toCaseFold . T.strip <$> xpConfirmedEmail profile of
    Just email | validEmail email ->
      Right
        XIdentity
          { xiUserId = xpId profile
          , xiUsername = xpUsername profile
          , xiConfirmedEmail = email
          -- Preserve the exact age boundary conservatively: a provider value
          -- even a fraction after the cutoff must not be rounded down into it.
          , xiCreatedAt = ceiling $ utcTimeToPOSIXSeconds $ xpCreatedAt profile
          }
    _ -> Left $ registrationError XEmailUnverified "X did not provide a confirmed email address"

data XFollowValidationReason
  = XFollowInvalidJson
  | XFollowResponseNotObject
  | XFollowErrorsInvalid
  | XFollowErrorsPresent
  | XFollowDataMissing
  | XFollowDataInvalid
  | XFollowTargetIdMissing
  | XFollowTargetIdInvalid
  | XFollowTargetMismatch
  | XFollowConnectionStatusMissing
  | XFollowConnectionStatusInvalid
  deriving stock (Show, Eq)

data XFollowLookupResult
  = XFollowLookupConfirmed
  | XFollowLookupNotConfirmed
  | XFollowLookupInvalid XFollowValidationReason
  deriving stock (Show, Eq)

data XFollowProviderFailure = XFollowProviderFailure
  { xfpfKind :: Text
  , xfpfHttpStatus :: Maybe Int
  , xfpfValidationReason :: Maybe XFollowValidationReason
  , xfpfRequestId :: Maybe Text
  , xfpfRetryable :: Bool
  , xfpfInvalidatesIdentity :: Bool
  }
  deriving stock (Show, Eq)

data XFollowAttemptResult
  = XFollowAttemptVerified
  | XFollowAttemptNotConfirmed
  | XFollowAttemptProviderFailed XFollowProviderFailure
  deriving stock (Show, Eq)

data XFollowVerificationFailure = XFollowVerificationFailure
  { xfvfError :: RegistrationError
  , xfvfResetIdentity :: Bool
  }
  deriving stock (Show, Eq)

verifyXFollow
  :: Manager
  -> RegistrationConfig
  -> Text
  -> XAccessToken
  -> IO (Either XFollowVerificationFailure ())
verifyXFollow manager config sourceUserId token
  | not (validXId sourceUserId) || not (validXId targetUserId) =
      pure $ Left $ XFollowVerificationFailure providerUnavailable False
  | otherwise = do
      request <- parseRequest $
        "https://api.x.com/2/users/"
          <> T.unpack targetUserId
          <> "?user.fields=connection_status"
      let prepared = bearerRequest token request
          attempt = performXFollowAttempt manager prepared targetUserId
      runXFollowVerificationWith
        (threadDelay 100_000)
        logXFollowProviderEvent
        attempt
  where
    targetUserId = rcXTargetUserId config

runXFollowVerificationWith
  :: IO ()
  -> (Text -> Int -> XFollowProviderFailure -> IO ())
  -> IO XFollowAttemptResult
  -> IO (Either XFollowVerificationFailure ())
runXFollowVerificationWith delayBeforeRetry logProviderEvent attempt = do
  first <- attempt
  case first of
    XFollowAttemptProviderFailed providerFailure
      | xfpfRetryable providerFailure -> do
          logProviderEventSafely "registration_x_provider_retry" 1 providerFailure
          delayBeforeRetry
          second <- attempt
          finish 2 second
    result -> finish 1 result
  where
    finish _ XFollowAttemptVerified = pure $ Right ()
    finish _ XFollowAttemptNotConfirmed =
      pure $ Left $ XFollowVerificationFailure followRequired False
    finish attemptNumber (XFollowAttemptProviderFailed providerFailure) = do
      logProviderEventSafely "registration_x_provider_failure" attemptNumber providerFailure
      pure $ Left $
        XFollowVerificationFailure
          providerUnavailable
          (xfpfInvalidatesIdentity providerFailure)

    followRequired = registrationError XFollowRequired "The X follow is not confirmed"

    logProviderEventSafely eventName attemptNumber providerFailure = do
      logResult <- try @SomeException $ logProviderEvent eventName attemptNumber providerFailure
      case logResult of
        Right () -> pure ()
        Left exception ->
          case fromException exception :: Maybe SomeAsyncException of
            Just _ -> throwIO exception
            Nothing -> pure ()

performXFollowAttempt :: Manager -> Request -> Text -> IO XFollowAttemptResult
performXFollowAttempt manager request targetUserId = do
  outcome <- performBoundedDetailed manager request
  pure $ case outcome of
    Left _ ->
      XFollowAttemptProviderFailed $
        XFollowProviderFailure
          { xfpfKind = "transport_or_response_size"
          , xfpfHttpStatus = Nothing
          , xfpfValidationReason = Nothing
          , xfpfRequestId = Nothing
          , xfpfRetryable = True
          , xfpfInvalidatesIdentity = False
          }
    Right response ->
      classifyXFollowResponse
        targetUserId
        (boundedStatus response)
        (boundedHeaders response)
        (boundedBody response)

classifyXFollowResponse
  :: Text
  -> Int
  -> [Header]
  -> BS.ByteString
  -> XFollowAttemptResult
classifyXFollowResponse targetUserId httpStatus headers body
  | httpStatus < 200 || httpStatus >= 300 =
      XFollowAttemptProviderFailed $
        XFollowProviderFailure
          { xfpfKind = "http_status"
          , xfpfHttpStatus = Just httpStatus
          , xfpfValidationReason = Nothing
          , xfpfRequestId = providerRequestIdFromHeaders headers
          , xfpfRetryable = retryableProviderStatus httpStatus
          -- A 401 is an unambiguous rejection of the stored bearer token.
          -- Preserve state for 403 responses because they can also mean
          -- an app entitlement or provider-policy failure.
          , xfpfInvalidatesIdentity = httpStatus == 401
          }
  | otherwise =
      case parseXFollowLookupResponseDetailed targetUserId body of
        XFollowLookupConfirmed -> XFollowAttemptVerified
        XFollowLookupNotConfirmed -> XFollowAttemptNotConfirmed
        XFollowLookupInvalid validationReason ->
          XFollowAttemptProviderFailed $
            XFollowProviderFailure
              { xfpfKind = "response_validation"
              , xfpfHttpStatus = Just httpStatus
              , xfpfValidationReason = Just validationReason
              , xfpfRequestId = providerRequestIdFromHeaders headers
              , xfpfRetryable = True
              , xfpfInvalidatesIdentity = False
              }

retryableProviderStatus :: Int -> Bool
retryableProviderStatus httpStatus =
  -- Retrying a 429 immediately would consume another request inside the same
  -- provider rate-limit window. Preserve the OAuth state and let the caller
  -- retry later instead.
  httpStatus `elem` [408, 425] || httpStatus >= 500

parseXFollowLookupResponse :: Text -> BS.ByteString -> Either RegistrationError ()
parseXFollowLookupResponse targetUserId body =
  case parseXFollowLookupResponseDetailed targetUserId body of
    XFollowLookupConfirmed -> Right ()
    XFollowLookupNotConfirmed ->
      Left $ registrationError XFollowRequired "The X follow is not confirmed"
    XFollowLookupInvalid _ -> Left providerUnavailable

parseXFollowLookupResponseDetailed :: Text -> BS.ByteString -> XFollowLookupResult
parseXFollowLookupResponseDetailed targetUserId body =
  case eitherDecodeStrict' body :: Either String Value of
    Left _ -> XFollowLookupInvalid XFollowInvalidJson
    Right (Object response) -> parseEnvelope response
    Right _ -> XFollowLookupInvalid XFollowResponseNotObject
  where
    parseEnvelope response =
      case KeyMap.lookup "errors" response of
        Just (Array errors)
          | not $ Vector.null errors -> XFollowLookupInvalid XFollowErrorsPresent
        Just Null -> parseData response
        Nothing -> parseData response
        Just (Array _) -> parseData response
        Just _ -> XFollowLookupInvalid XFollowErrorsInvalid

    parseData response =
      case KeyMap.lookup "data" response of
        Nothing -> XFollowLookupInvalid XFollowDataMissing
        Just Null -> XFollowLookupInvalid XFollowDataMissing
        Just (Object followData) -> parseFollowData followData
        Just _ -> XFollowLookupInvalid XFollowDataInvalid

    parseFollowData followData =
      case KeyMap.lookup "id" followData of
        Nothing -> XFollowLookupInvalid XFollowTargetIdMissing
        Just (String returnedUserId)
          | returnedUserId /= targetUserId -> XFollowLookupInvalid XFollowTargetMismatch
          | otherwise -> parseConnectionStatus followData
        Just _ -> XFollowLookupInvalid XFollowTargetIdInvalid

    parseConnectionStatus followData =
      case KeyMap.lookup "connection_status" followData of
        Nothing -> XFollowLookupInvalid XFollowConnectionStatusMissing
        Just (Array values) ->
          case traverse asText $ Vector.toList values of
            Nothing -> XFollowLookupInvalid XFollowConnectionStatusInvalid
            Just connectionStatus
              | "following" `elem` connectionStatus
                  && "follow_request_sent" `notElem` connectionStatus -> XFollowLookupConfirmed
              | otherwise -> XFollowLookupNotConfirmed
        Just _ -> XFollowLookupInvalid XFollowConnectionStatusInvalid

    asText (String value) = Just value
    asText _ = Nothing

logXFollowProviderEvent :: Text -> Int -> XFollowProviderFailure -> IO ()
logXFollowProviderEvent eventName attemptNumber failure =
  logWarn
    eventName
    (if eventName == "registration_x_provider_retry" then "Retrying X registration provider request" else "X registration provider request failed")
    [ field "provider_stage" ("follow_lookup" :: Text)
    , field "provider_attempt" attemptNumber
    , field "provider_failure_kind" $ xfpfKind failure
    , field "provider_http_status" $ xfpfHttpStatus failure
    , field "provider_validation_reason" $ validationReasonText <$> xfpfValidationReason failure
    , field "provider_request_id" $ xfpfRequestId failure
    , field "provider_retryable" $ xfpfRetryable failure
    ]

validationReasonText :: XFollowValidationReason -> Text
validationReasonText = \case
  XFollowInvalidJson -> "invalid_json"
  XFollowResponseNotObject -> "response_not_object"
  XFollowErrorsInvalid -> "errors_invalid"
  XFollowErrorsPresent -> "errors_present"
  XFollowDataMissing -> "data_missing"
  XFollowDataInvalid -> "data_invalid"
  XFollowTargetIdMissing -> "target_id_missing"
  XFollowTargetIdInvalid -> "target_id_invalid"
  XFollowTargetMismatch -> "target_mismatch"
  XFollowConnectionStatusMissing -> "connection_status_missing"
  XFollowConnectionStatusInvalid -> "connection_status_invalid"

bearerRequest :: XAccessToken -> Request -> Request
bearerRequest (XAccessToken token) request = (secureRequest request)
  { requestHeaders =
      [ ("Authorization", "Bearer " <> token)
      , ("Accept", "application/json")
      ]
  }

secureRequest :: Request -> Request
secureRequest request = request
  { redirectCount = 0
  , responseTimeout = responseTimeoutMicro 10_000_000
  , checkResponse = \_ _ -> pure ()
  }

performBounded
  :: Manager
  -> Request
  -> IO (Either RegistrationError (Int, BS.ByteString))
performBounded manager request =
  fmap
    (fmap $ \response -> (boundedStatus response, boundedBody response))
    (performBoundedDetailed manager request)

data BoundedResponse = BoundedResponse
  { boundedStatus :: Int
  , boundedHeaders :: [Header]
  , boundedBody :: BS.ByteString
  }

performBoundedDetailed
  :: Manager
  -> Request
  -> IO (Either RegistrationError BoundedResponse)
performBoundedDetailed manager request = do
  result <- try @SomeException $
    withResponse request manager $ \response -> do
      let httpStatus = statusCode $ responseStatus response
          headers = responseHeaders response
      -- Callers never inspect provider error bodies. Return the status and
      -- headers immediately so an oversized or interrupted error body cannot
      -- hide a definite bearer rejection such as HTTP 401.
      if httpStatus < 200 || httpStatus >= 300
        then pure (httpStatus, headers, Right BS.empty)
        else do
          body <- readBodyBounded 1_048_576 $ responseBody response
          pure (httpStatus, headers, body)
  case result of
    Left exception ->
      case fromException exception :: Maybe SomeAsyncException of
        Just _ -> throwIO exception
        Nothing -> pure $ Left providerUnavailable
    Right (_, _, Left ()) -> pure $ Left providerUnavailable
    Right (httpStatus, headers, Right body) ->
      pure $
        Right
          BoundedResponse
            { boundedStatus = httpStatus
            , boundedHeaders = headers
            , boundedBody = body
            }

providerRequestIdFromHeaders :: [Header] -> Maybe Text
providerRequestIdFromHeaders headers =
  (lookup "x-transaction-id" headers >>= sanitizeProviderRequestId)
    <|> (lookup "x-request-id" headers >>= sanitizeProviderRequestId)

sanitizeProviderRequestId :: BS.ByteString -> Maybe Text
sanitizeProviderRequestId raw =
  case TE.decodeUtf8' raw of
    Left _ -> Nothing
    Right value
      | T.null value || T.length value > 128 -> Nothing
      | T.all validRequestIdCharacter value -> Just value
      | otherwise -> Nothing
  where
    validRequestIdCharacter character =
      isAscii character
        && (isAlphaNum character || character `elem` ("-_.:" :: String))

readBodyBounded :: Int -> BodyReader -> IO (Either () BS.ByteString)
readBodyBounded maximumBytes reader = go 0 []
  where
    go total chunks = do
      chunk <- brRead reader
      if BS.null chunk
        then pure $ Right $ BS.concat $ reverse chunks
        else
          let nextTotal = total + BS.length chunk
           in if nextTotal > maximumBytes
                then pure $ Left ()
                else go nextTotal (chunk : chunks)

validAuthorizationCode :: Text -> Bool
validAuthorizationCode value =
  T.length value >= 1
    && T.length value <= 2048
    && not (T.any (\character -> isControl character || isSpace character) value)

validPkceVerifier :: Text -> Bool
validPkceVerifier value =
  T.length value >= 43
    && T.length value <= 128
    && T.all isPkceCharacter value

validBase64Url43 :: Text -> Bool
validBase64Url43 value =
  T.length value == 43
    && T.all
      (\character ->
        character == '-'
          || character == '_'
          || character `elem` ['0' .. '9']
          || character `elem` ['a' .. 'z']
          || character `elem` ['A' .. 'Z']
      )
      value

isPkceCharacter :: Char -> Bool
isPkceCharacter character =
  character == '-'
    || character == '.'
    || character == '_'
    || character == '~'
    || character `elem` ['0' .. '9']
    || character `elem` ['a' .. 'z']
    || character `elem` ['A' .. 'Z']

validXId :: Text -> Bool
validXId value =
  T.length value >= 1
    && T.length value <= 19
    && T.any (/= '0') value
    && T.all (`elem` ['0' .. '9']) value

validXUsername :: Text -> Bool
validXUsername value =
  T.length value >= 1
    && T.length value <= 15
    && T.all (\character -> character == '_' || character `elem` ['a' .. 'z'] || character `elem` ['A' .. 'Z'] || character `elem` ['0' .. '9']) value

validEmail :: Text -> Bool
validEmail value =
  T.length value >= 3
    && T.length value <= 320
    && T.count "@" value == 1
    && not (T.any (\character -> isControl character || isSpace character) value)
    && case T.splitOn "@" value of
      [localPart, domainPart] -> not (T.null localPart) && T.any (== '.') domainPart && not (T.null domainPart)
      _ -> False

providerUnavailable :: RegistrationError
providerUnavailable = registrationError ProviderUnavailable "Identity provider is temporarily unavailable"

-- OAuth credentials, response bodies, access tokens, and user data are
-- deliberately excluded. These fields are sufficient to distinguish network,
-- HTTP, and schema failures without exposing registration identities.
logXProviderFailure
  :: Text
  -> Either RegistrationError (Int, BS.ByteString)
  -> Either RegistrationError value
  -> IO ()
logXProviderFailure stage outcome result =
  case result of
    Left err | reCode err == ProviderUnavailable ->
      logWarn
        "registration_x_provider_failure"
        "X registration provider request failed"
        [ field "provider_stage" stage
        , field "provider_failure_kind" $ case outcome of
            Left _ -> ("transport_or_response_size" :: Text)
            Right (httpStatus, _)
              | httpStatus < 200 || httpStatus >= 300 -> "http_status"
              | otherwise -> "response_validation"
        , field "provider_http_status" $ case outcome of
            Left _ -> Nothing @Int
            Right (httpStatus, _) -> Just httpStatus
        ]
    _ -> pure ()

validUuid :: Text -> Bool
validUuid value =
  T.length value == 36
    && and
      [ T.index value 8 == '-'
      , T.index value 13 == '-'
      , T.index value 18 == '-'
      , T.index value 23 == '-'
      , T.index value 14 == '4'
      , T.index value 19 `elem` ['8', '9', 'a', 'b']
      ]
    && T.all (\character -> character == '-' || character `elem` ['0' .. '9'] || character `elem` ['a' .. 'f']) value
