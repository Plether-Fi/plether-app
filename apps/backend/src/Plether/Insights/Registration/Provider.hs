module Plether.Insights.Registration.Provider
  ( XAccessToken
  , xAccessTokenBytes
  , xAccessTokenFromBytes
  , TurnstileEvidence (..)
  , verifyTurnstile
  , buildXAuthorizationUrl
  , exchangeXAuthorizationCode
  , fetchXIdentity
  , verifyXFollow
  , parseTurnstileResponseAt
  , parseXIdentityResponse
  , parseXFollowLookupResponse
  ) where

import Control.Exception
  ( SomeAsyncException
  , SomeException
  , fromException
  , throwIO
  , try
  )
import Data.Aeson
  ( FromJSON (..)
  , Value
  , eitherDecodeStrict'
  , withObject
  , (.:)
  , (.:?)
  , (.!=)
  )
import qualified Data.ByteString as BS
import Data.Char (isControl, isSpace)
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
  , responseStatus
  , responseTimeoutMicro
  , withResponse
  )
import Network.HTTP.Types.Status (statusCode)
import Network.HTTP.Types.URI (renderSimpleQuery)
import Plether.Insights.Registration.Config (RegistrationConfig (..))
import Plether.Insights.Registration.Types
  ( RegistrationError
  , RegistrationErrorCode (..)
  , XIdentity (..)
  , registrationError
  )

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
      pure $ do
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
  pure $ do
    (httpStatus, body) <- outcome
    if httpStatus < 200 || httpStatus >= 300
      then Left providerUnavailable
      else parseXIdentityResponse body

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

data XFollowLookupResponse = XFollowLookupResponse (Maybe XFollowLookupData) [Value]

instance FromJSON XFollowLookupResponse where
  parseJSON = withObject "XFollowLookupResponse" $ \value ->
    XFollowLookupResponse
      <$> value .:? "data"
      <*> (value .:? "errors" .!= [])

data XFollowLookupData = XFollowLookupData
  { xfldUserId :: Text
  , xfldConnectionStatus :: [Text]
  }

instance FromJSON XFollowLookupData where
  parseJSON = withObject "XFollowLookupData" $ \value ->
    XFollowLookupData
      <$> value .: "id"
      <*> value .: "connection_status"

verifyXFollow
  :: Manager
  -> RegistrationConfig
  -> Text
  -> XAccessToken
  -> IO (Either RegistrationError ())
verifyXFollow manager config sourceUserId token
  | not (validXId sourceUserId) || not (validXId targetUserId) = pure $ Left providerUnavailable
  | otherwise = do
      request <- parseRequest $
        "https://api.x.com/2/users/"
          <> T.unpack targetUserId
          <> "?user.fields=connection_status"
      let prepared = bearerRequest token request
      outcome <- performBounded manager prepared
      pure $ do
        (httpStatus, body) <- outcome
        if httpStatus < 200 || httpStatus >= 300
          then Left providerUnavailable
          else parseXFollowLookupResponse targetUserId body
  where
    targetUserId = rcXTargetUserId config

parseXFollowLookupResponse :: Text -> BS.ByteString -> Either RegistrationError ()
parseXFollowLookupResponse targetUserId body = do
  XFollowLookupResponse maybeFollowData apiErrors <-
    either (const $ Left providerUnavailable) Right $ eitherDecodeStrict' body
  if null apiErrors then pure () else Left providerUnavailable
  followData <- maybe (Left providerUnavailable) Right maybeFollowData
  if xfldUserId followData /= targetUserId
    then Left providerUnavailable
    else pure ()
  let connectionStatus = xfldConnectionStatus followData
  if "following" `elem` connectionStatus && "follow_request_sent" `notElem` connectionStatus
    then Right ()
    else Left $ registrationError XFollowRequired "The X follow is not confirmed"

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
performBounded manager request = do
  result <- try @SomeException $
    withResponse request manager $ \response -> do
      body <- readBodyBounded 1_048_576 $ responseBody response
      pure (statusCode $ responseStatus response, body)
  case result of
    Left exception ->
      case fromException exception :: Maybe SomeAsyncException of
        Just _ -> throwIO exception
        Nothing -> pure $ Left providerUnavailable
    Right (_, Left ()) -> pure $ Left providerUnavailable
    Right (httpStatus, Right body) -> pure $ Right (httpStatus, body)

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
