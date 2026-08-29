module Plether.Insights.Registration.Config
  ( RegistrationConfig (..)
  , loadRegistrationConfig
  ) where

import Data.Aeson (eitherDecodeStrict')
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import Data.Char (isControl, isSpace)
import Data.List (nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.URI (URI (..), URIAuth (..), parseURI)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

-- | Secrets are deliberately kept out of the 'Show' instance.  The email
-- keyring retains old key versions so rows can be decrypted and re-encrypted
-- after rotation without ever falling back to an unversioned key.
data RegistrationConfig = RegistrationConfig
  { rcActivationEnabled :: Bool
  , rcPublicOrigin :: Text
  , rcOriginToken :: BS.ByteString
  , rcOriginTokenNext :: Maybe BS.ByteString
  , rcTurnstileSecretKey :: Text
  , rcTurnstileExpectedHostname :: Text
  , rcTurnstileExpectedAction :: Text
  , rcXClientId :: Text
  , rcXClientSecret :: Text
  , rcXCallbackUrl :: Text
  , rcXCallbackCompetitionSlug :: Text
  , rcXTargetUserId :: Text
  , rcXTargetHandle :: Text
  , rcEmailKeys :: Map Text BS.ByteString
  , rcActiveEmailKeyVersion :: Text
  , rcLookupHmacKey :: BS.ByteString
  , rcSessionTtlSeconds :: Integer
  , rcIpRateLimitPerMinute :: Int
  , rcSessionRateLimitPerMinute :: Int
  , rcRulesVersion :: Text
  , rcPrivacyVersion :: Text
  , rcMinimumXAccountAgeDays :: Integer
  }

instance Show RegistrationConfig where
  show cfg =
    "RegistrationConfig {rcActivationEnabled = "
      <> show (rcActivationEnabled cfg)
      <> ", rcPublicOrigin = "
      <> show (rcPublicOrigin cfg)
      <> ", rcOriginToken = <redacted>, rcOriginTokenNext = <redacted>, rcTurnstileSecretKey = <redacted>"
      <> ", rcTurnstileExpectedHostname = "
      <> show (rcTurnstileExpectedHostname cfg)
      <> ", rcTurnstileExpectedAction = "
      <> show (rcTurnstileExpectedAction cfg)
      <> ", rcXClientId = <redacted>, rcXClientSecret = <redacted>"
      <> ", rcXCallbackUrl = "
      <> show (rcXCallbackUrl cfg)
      <> ", rcXCallbackCompetitionSlug = "
      <> show (rcXCallbackCompetitionSlug cfg)
      <> ", rcXTargetUserId = <redacted>, rcXTargetHandle = "
      <> show (rcXTargetHandle cfg)
      <> ", rcEmailKeys = <redacted keyring>"
      <> ", rcActiveEmailKeyVersion = "
      <> show (rcActiveEmailKeyVersion cfg)
      <> ", rcLookupHmacKey = <redacted>"
      <> ", rcSessionTtlSeconds = "
      <> show (rcSessionTtlSeconds cfg)
      <> ", rcIpRateLimitPerMinute = "
      <> show (rcIpRateLimitPerMinute cfg)
      <> ", rcSessionRateLimitPerMinute = "
      <> show (rcSessionRateLimitPerMinute cfg)
      <> ", rcRulesVersion = "
      <> show (rcRulesVersion cfg)
      <> ", rcPrivacyVersion = "
      <> show (rcPrivacyVersion cfg)
      <> ", rcMinimumXAccountAgeDays = "
      <> show (rcMinimumXAccountAgeDays cfg)
      <> "}"

loadRegistrationConfig :: IO (Either String (Maybe RegistrationConfig))
loadRegistrationConfig = do
  enabledRaw <- fromMaybe "false" <$> lookupEnv "INSIGHTS_REGISTRATION_ENABLED"
  provisionedRaw <- fromMaybe "false" <$> lookupEnv "INSIGHTS_REGISTRATION_PROVISIONED"
  case (parseBoolStrict provisionedRaw, parseBoolStrict enabledRaw) of
    (Nothing, _) -> pure $ Left "INSIGHTS_REGISTRATION_PROVISIONED must be a boolean"
    (_, Nothing) -> pure $ Left "INSIGHTS_REGISTRATION_ENABLED must be a boolean"
    (Just False, Just True) ->
      pure $ Left "INSIGHTS_REGISTRATION_ENABLED=true requires INSIGHTS_REGISTRATION_PROVISIONED=true"
    (Just False, Just False) -> pure $ Right Nothing
    (Just True, Just activationEnabled) -> fmap Just <$> loadProvisioned activationEnabled

loadProvisioned :: Bool -> IO (Either String RegistrationConfig)
loadProvisioned activationEnabled = do
  publicOrigin <- required "INSIGHTS_REGISTRATION_PUBLIC_ORIGIN"
  originToken <- required "INSIGHTS_REGISTRATION_ORIGIN_TOKEN"
  originTokenNext <- lookupEnv "INSIGHTS_REGISTRATION_ORIGIN_TOKEN_NEXT"
  turnstileSecret <- required "TURNSTILE_SECRET_KEY"
  turnstileHostname <- required "TURNSTILE_EXPECTED_HOSTNAME"
  turnstileAction <- optional "TURNSTILE_EXPECTED_ACTION" "competition_registration"
  xClientId <- required "X_OAUTH_CLIENT_ID"
  xClientSecret <- required "X_OAUTH_CLIENT_SECRET"
  xCallback <- required "X_OAUTH_CALLBACK_URL"
  xTargetUserId <- required "X_TARGET_USER_ID"
  xTargetHandle <- optional "X_TARGET_HANDLE" "plether_fi"
  keyringJson <- required "INSIGHTS_REGISTRATION_EMAIL_KEYS_JSON"
  activeKeyVersion <- required "INSIGHTS_REGISTRATION_EMAIL_KEY_VERSION"
  hmacKey <- required "INSIGHTS_REGISTRATION_EMAIL_HMAC_KEY_BASE64"
  sessionTtlRaw <- lookupEnv "INSIGHTS_REGISTRATION_SESSION_TTL_SECONDS"
  ipRateRaw <- lookupEnv "INSIGHTS_REGISTRATION_IP_RATE_LIMIT_PER_MINUTE"
  sessionRateRaw <- lookupEnv "INSIGHTS_REGISTRATION_SESSION_RATE_LIMIT_PER_MINUTE"
  rulesVersion <- required "INSIGHTS_REGISTRATION_RULES_VERSION"
  privacyVersion <- required "INSIGHTS_REGISTRATION_PRIVACY_VERSION"
  pure $ do
    origin <- normalizeOrigin publicOrigin
    rejectPlaceholder "INSIGHTS_REGISTRATION_PUBLIC_ORIGIN" origin
    if origin == canonicalRegistrationOrigin
      then pure ()
      else Left "INSIGHTS_REGISTRATION_PUBLIC_ORIGIN must equal https://insights.plether.com"
    normalizedOriginToken <- requireTrimmedSecret "INSIGHTS_REGISTRATION_ORIGIN_TOKEN" originToken
    tokenBytes <- requireStrongBytes "INSIGHTS_REGISTRATION_ORIGIN_TOKEN" 32 $ TE.encodeUtf8 normalizedOriginToken
    nextTokenBytes <- traverse validateNextOriginToken $ case originTokenNext of
      Just "" -> Nothing
      other -> other
    if nextTokenBytes == Just tokenBytes
      then Left "INSIGHTS_REGISTRATION_ORIGIN_TOKEN_NEXT must differ from INSIGHTS_REGISTRATION_ORIGIN_TOKEN"
      else pure ()
    resolvedTurnstileSecret <- requireTrimmedSecret "TURNSTILE_SECRET_KEY" turnstileSecret
    rejectPlaceholder "TURNSTILE_SECRET_KEY" resolvedTurnstileSecret
    _ <- requireStrongBytes "TURNSTILE_SECRET_KEY" 16 $ TE.encodeUtf8 resolvedTurnstileSecret
    resolvedTurnstileHostname <- requireHostname "TURNSTILE_EXPECTED_HOSTNAME" turnstileHostname
    rejectPlaceholder "TURNSTILE_EXPECTED_HOSTNAME" resolvedTurnstileHostname
    if resolvedTurnstileHostname == canonicalRegistrationHostname
      then pure ()
      else Left "TURNSTILE_EXPECTED_HOSTNAME must equal insights.plether.com"
    resolvedTurnstileAction <- requireTurnstileAction turnstileAction
    resolvedXClientId <- requireTrimmedSecret "X_OAUTH_CLIENT_ID" xClientId
    rejectPlaceholder "X_OAUTH_CLIENT_ID" resolvedXClientId
    resolvedXClientSecret <- requireTrimmedSecret "X_OAUTH_CLIENT_SECRET" xClientSecret
    rejectPlaceholder "X_OAUTH_CLIENT_SECRET" resolvedXClientSecret
    _ <- requireStrongBytes "X_OAUTH_CLIENT_SECRET" 16 $ TE.encodeUtf8 resolvedXClientSecret
    keys <- decodeKeyring keyringJson
    activeVersion <- requireBoundedIdentifier "INSIGHTS_REGISTRATION_EMAIL_KEY_VERSION" 64 activeKeyVersion
    if Map.member activeVersion keys
      then pure ()
      else Left "INSIGHTS_REGISTRATION_EMAIL_KEY_VERSION is absent from INSIGHTS_REGISTRATION_EMAIL_KEYS_JSON"
    digestKey <- decodeFixedOrLonger "INSIGHTS_REGISTRATION_EMAIL_HMAC_KEY_BASE64" 32 hmacKey
    ensureDistinctEncryptionKeys keys
    if digestKey `elem` Map.elems keys
      then Left "INSIGHTS_REGISTRATION_EMAIL_HMAC_KEY_BASE64 must differ from every email encryption key"
      else pure ()
    (callback, callbackSlug) <- validateCallback origin xCallback
    targetUserId <- requireDigits "X_TARGET_USER_ID" xTargetUserId
    if targetUserId `elem` ["123456789", "1234567890", "1234567890123456789"]
      then Left "X_TARGET_USER_ID must not contain an example or placeholder value"
      else pure ()
    targetHandle <- normalizeHandle xTargetHandle
    rejectPlaceholder "X_TARGET_HANDLE" targetHandle
    resolvedRulesVersion <- requireBoundedIdentifier "INSIGHTS_REGISTRATION_RULES_VERSION" 64 rulesVersion
    resolvedPrivacyVersion <- requireBoundedIdentifier "INSIGHTS_REGISTRATION_PRIVACY_VERSION" 64 privacyVersion
    sessionTtl <- parseBoundedInteger "INSIGHTS_REGISTRATION_SESSION_TTL_SECONDS" 300 3600 1800 sessionTtlRaw
    ipRate <- fromInteger <$> parseBoundedInteger "INSIGHTS_REGISTRATION_IP_RATE_LIMIT_PER_MINUTE" 1 1000 10 ipRateRaw
    sessionRate <- fromInteger <$> parseBoundedInteger "INSIGHTS_REGISTRATION_SESSION_RATE_LIMIT_PER_MINUTE" 1 5000 30 sessionRateRaw
    Right
      RegistrationConfig
        { rcActivationEnabled = activationEnabled
        , rcPublicOrigin = origin
        , rcOriginToken = tokenBytes
        , rcOriginTokenNext = nextTokenBytes
        , rcTurnstileSecretKey = resolvedTurnstileSecret
        , rcTurnstileExpectedHostname = resolvedTurnstileHostname
        , rcTurnstileExpectedAction = resolvedTurnstileAction
        , rcXClientId = resolvedXClientId
        , rcXClientSecret = resolvedXClientSecret
        , rcXCallbackUrl = callback
        , rcXCallbackCompetitionSlug = callbackSlug
        , rcXTargetUserId = targetUserId
        , rcXTargetHandle = targetHandle
        , rcEmailKeys = keys
        , rcActiveEmailKeyVersion = activeVersion
        , rcLookupHmacKey = digestKey
        , rcSessionTtlSeconds = sessionTtl
        , rcIpRateLimitPerMinute = ipRate
        , rcSessionRateLimitPerMinute = sessionRate
        , rcRulesVersion = resolvedRulesVersion
        , rcPrivacyVersion = resolvedPrivacyVersion
        , rcMinimumXAccountAgeDays = 90
        }
  where
    validateNextOriginToken raw = do
      value <- requireTrimmedSecret "INSIGHTS_REGISTRATION_ORIGIN_TOKEN_NEXT" raw
      rejectPlaceholder "INSIGHTS_REGISTRATION_ORIGIN_TOKEN_NEXT" value
      requireStrongBytes "INSIGHTS_REGISTRATION_ORIGIN_TOKEN_NEXT" 32 $ TE.encodeUtf8 value

ensureDistinctEncryptionKeys :: Map Text BS.ByteString -> Either String ()
ensureDistinctEncryptionKeys keys =
  if length keyBytes == length (nub keyBytes)
    then Right ()
    else Left "INSIGHTS_REGISTRATION_EMAIL_KEYS_JSON must use distinct key material for every version"
  where
    keyBytes = Map.elems keys

required :: String -> IO String
required name = fromMaybe "" <$> lookupEnv name

optional :: String -> String -> IO String
optional name fallback = fromMaybe fallback <$> lookupEnv name

normalizeOrigin :: String -> Either String Text
normalizeOrigin raw =
  if raw /= trimmed
    then Left "INSIGHTS_REGISTRATION_PUBLIC_ORIGIN must not contain surrounding whitespace"
    else
      case parseURI trimmed of
        Just URI
          { uriScheme = "https:"
          , uriAuthority = Just URIAuth {uriUserInfo = "", uriRegName = host, uriPort = ""}
          , uriPath = path
          , uriQuery = ""
          , uriFragment = ""
          }
            | path `elem` ["", "/"]
            , validHostname host
            , host == map asciiLower host -> Right $ "https://" <> T.pack host
        _ -> Left "INSIGHTS_REGISTRATION_PUBLIC_ORIGIN must be a canonical lowercase HTTPS origin with no credentials, port, path, query, or fragment"
  where
    trimmed = T.unpack $ T.strip $ T.pack raw

validateCallback :: Text -> String -> Either String (Text, Text)
validateCallback origin raw =
  if raw /= trimmed
    then Left "X_OAUTH_CALLBACK_URL must not contain surrounding whitespace"
    else
      case (parseURI $ T.unpack origin, parseURI trimmed) of
        ( Just URI {uriAuthority = originAuthority}
          , Just callback@URI
              { uriScheme = "https:"
              , uriAuthority = callbackAuthority
              , uriPath = path
              , uriQuery = ""
              , uriFragment = ""
              }
          )
            | callbackAuthority == originAuthority
            , Just slug <- extractCallbackSlug path
            , validSlug slug -> Right (T.pack $ show callback, T.pack slug)
        _ -> Left "X_OAUTH_CALLBACK_URL must use the exact canonical Insights origin and /api/insights/v1/competitions/<slug>/registrations/x/callback path"
  where
    trimmed = T.unpack $ T.strip $ T.pack raw
    extractCallbackSlug path = do
      remainder <- stripStringPrefix "/api/insights/v1/competitions/" path
      let suffix = "/registrations/x/callback"
      if suffix `isStringSuffixOf` remainder
        then
          let slug = take (length remainder - length suffix) remainder
           in if null slug || '/' `elem` slug then Nothing else Just slug
        else Nothing
    validSlug value =
      not (null value)
        && all (\c -> c == '-' || c `elem` ['a' .. 'z'] || c `elem` ['0' .. '9']) value

validHostname :: String -> Bool
validHostname host =
  not (null host)
    && length host <= 253
    && all validLabel (splitLabels host)
  where
    validLabel label =
      not (null label)
        && length label <= 63
        && head label /= '-'
        && last label /= '-'
        && all (\c -> c == '-' || c `elem` ['a' .. 'z'] || c `elem` ['0' .. '9']) label
    splitLabels value = case break (== '.') value of
      (label, []) -> [label]
      (label, _dot : remaining) -> label : splitLabels remaining

requireHostname :: String -> String -> Either String Text
requireHostname name raw
  | raw == value && validHostname value && value == map asciiLower value = Right $ T.pack value
  | otherwise = Left $ name <> " must be a canonical lowercase hostname"
  where
    value = T.unpack $ T.strip $ T.pack raw

asciiLower :: Char -> Char
asciiLower c
  | c >= 'A' && c <= 'Z' = toEnum $ fromEnum c + 32
  | otherwise = c

parseBoundedInteger
  :: String
  -> Integer
  -> Integer
  -> Integer
  -> Maybe String
  -> Either String Integer
parseBoundedInteger _ _ _ fallback Nothing = Right fallback
parseBoundedInteger name lower upper _ (Just raw) =
  case readMaybe normalized of
    Just value
      | raw == normalized
      , show value == normalized
      , value >= lower
      , value <= upper -> Right value
    _ -> Left $ name <> " must be a whole number between " <> show lower <> " and " <> show upper
  where
    normalized = T.unpack $ T.strip $ T.pack raw

requireNonBlank :: String -> String -> Either String Text
requireNonBlank name raw
  | T.null value = Left $ name <> " is required when INSIGHTS_REGISTRATION_PROVISIONED=true"
  | otherwise = Right value
  where
    value = T.strip $ T.pack raw

requireTrimmedSecret :: String -> String -> Either String Text
requireTrimmedSecret name raw = do
  value <- requireNonBlank name raw
  if T.pack raw == value && not (T.any (\c -> isSpace c || isControl c) value)
    then Right value
    else Left $ name <> " must not contain whitespace"

requireBoundedIdentifier :: String -> Int -> String -> Either String Text
requireBoundedIdentifier name maximumLength raw = do
  value <- requireNonBlank name raw
  if T.pack raw == value
      && T.length value <= maximumLength
      && T.all (\c -> c == '-' || c == '_' || c == '.' || c `elem` ['a' .. 'z'] || c `elem` ['A' .. 'Z'] || c `elem` ['0' .. '9']) value
    then Right value
    else Left $ name <> " must be at most " <> show maximumLength <> " characters using only letters, digits, dot, dash, or underscore"

requireTurnstileAction :: String -> Either String Text
requireTurnstileAction raw = do
  value <- requireNonBlank "TURNSTILE_EXPECTED_ACTION" raw
  if T.pack raw == value && value == "competition_registration"
    then Right value
    else Left "TURNSTILE_EXPECTED_ACTION must equal competition_registration"

requireDigits :: String -> String -> Either String Text
requireDigits name raw = do
  value <- requireNonBlank name raw
  if T.pack raw == value
      && T.length value >= 1
      && T.length value <= 19
      && T.any (/= '0') value
      && T.all (`elem` ['0' .. '9']) value
    then Right value
    else Left $ name <> " must contain 1 to 19 decimal digits"

normalizeHandle :: String -> Either String Text
normalizeHandle raw = do
  value <- requireNonBlank "X_TARGET_HANDLE" raw
  let handle = T.dropWhile (== '@') value
  if T.pack raw == value && not (T.null handle) && T.length handle <= 15 && T.all (\c -> c == '_' || c `elem` ['a' .. 'z'] || c `elem` ['A' .. 'Z'] || c `elem` ['0' .. '9']) handle
    then Right handle
    else Left "X_TARGET_HANDLE is invalid"

decodeKeyring :: String -> Either String (Map Text BS.ByteString)
decodeKeyring raw = do
  encoded <- either (Left . ("INSIGHTS_REGISTRATION_EMAIL_KEYS_JSON is invalid JSON: " <>)) Right $
    eitherDecodeStrict' (TE.encodeUtf8 $ T.pack raw) :: Either String (Map Text Text)
  if Map.null encoded
    then Left "INSIGHTS_REGISTRATION_EMAIL_KEYS_JSON must contain at least one key"
    else Map.traverseWithKey decodeEntry encoded
  where
    decodeEntry version encodedKey = do
      _ <- requireBoundedIdentifier "INSIGHTS_REGISTRATION_EMAIL_KEYS_JSON key version" 64 $ T.unpack version
      decodeExactly "INSIGHTS_REGISTRATION_EMAIL_KEYS_JSON" 32 $ T.unpack encodedKey

decodeFixedOrLonger :: String -> Int -> String -> Either String BS.ByteString
decodeFixedOrLonger name minimumLength raw = do
  bytes <- decodeBase64 name raw
  requireStrongBytes name minimumLength bytes

decodeExactly :: String -> Int -> String -> Either String BS.ByteString
decodeExactly name lengthRequired raw = do
  bytes <- decodeBase64 name raw
  if BS.length bytes == lengthRequired
    then requireStrongBytes name lengthRequired bytes
    else Left $ name <> " values must decode to exactly " <> show lengthRequired <> " bytes"

decodeBase64 :: String -> String -> Either String BS.ByteString
decodeBase64 name raw =
  if T.pack raw /= T.strip (T.pack raw) || T.any (\c -> isSpace c || isControl c) (T.pack raw)
    then Left $ name <> " must not contain whitespace"
    else
      case B64.decode $ TE.encodeUtf8 $ T.pack raw of
        Left _ -> Left $ name <> " contains invalid base64"
        Right bytes -> Right bytes

requireMinBytes :: String -> Int -> BS.ByteString -> Either String BS.ByteString
requireMinBytes name minimumLength bytes
  | BS.length bytes >= minimumLength = Right bytes
  | otherwise = Left $ name <> " must contain at least " <> show minimumLength <> " bytes"

requireStrongBytes :: String -> Int -> BS.ByteString -> Either String BS.ByteString
requireStrongBytes name minimumLength bytes = do
  _ <- requireMinBytes name minimumLength bytes
  let distinctBytes = BS.foldl' (\seen byte -> if byte `elem` seen then seen else byte : seen) [] bytes
  if length distinctBytes >= 8
    then Right bytes
    else Left $ name <> " must be generated from strong random material"

rejectPlaceholder :: String -> Text -> Either String ()
rejectPlaceholder name value
  | any (`T.isInfixOf` normalized) placeholderMarkers =
      Left $ name <> " must not contain an example or placeholder value"
  | otherwise = Right ()
  where
    normalized = T.toCaseFold value
    placeholderMarkers =
      [ "example"
      , "placeholder"
      , "changeme"
      , "change-me"
      , "replace-me"
      , "replace_me"
      , "your-secret"
      , "your_secret"
      , "localhost"
      , ".invalid"
      ]

parseBoolStrict :: String -> Maybe Bool
parseBoolStrict raw =
  let value = T.pack raw
   in if value /= T.strip value
        then Nothing
        else case T.toLower value of
          "1" -> Just True
          "true" -> Just True
          "yes" -> Just True
          "on" -> Just True
          "0" -> Just False
          "false" -> Just False
          "no" -> Just False
          "off" -> Just False
          _ -> Nothing

stripStringPrefix :: String -> String -> Maybe String
stripStringPrefix [] value = Just value
stripStringPrefix _ [] = Nothing
stripStringPrefix (expected : restExpected) (actual : restActual)
  | expected == actual = stripStringPrefix restExpected restActual
  | otherwise = Nothing

isStringSuffixOf :: String -> String -> Bool
isStringSuffixOf suffix value = reverse suffix `isPrefix` reverse value
  where
    isPrefix [] _ = True
    isPrefix _ [] = False
    isPrefix (expected : restExpected) (actual : restActual) =
      expected == actual && isPrefix restExpected restActual

canonicalRegistrationOrigin, canonicalRegistrationHostname :: Text
canonicalRegistrationOrigin = "https://insights.plether.com"
canonicalRegistrationHostname = "insights.plether.com"
