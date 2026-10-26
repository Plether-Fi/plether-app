module Plether.Handlers.DivisionsRegistration
  ( DiscordConfig
  , loadDiscordConfig
  , ensureDivisionsSchema
  , registerDivisionsRegistrationRoutes
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON (..), Value, eitherDecode, object, withObject, (.:), (.:?), (.=))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isControl, isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as LT
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Database.PostgreSQL.Simple (Connection, Only (..), execute, execute_, query, withTransaction)
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import Database.PostgreSQL.Simple.Types (Binary (..))
import Network.HTTP.Client
  ( Manager, Request (..), RequestBody (..), applyBasicAuth, httpLbs
  , parseRequest, responseBody, responseStatus, responseTimeoutMicro
  )
import Network.HTTP.Types.Status (status303, statusCode)
import Network.HTTP.Types.URI (renderSimpleQuery)
import qualified Network.Wai as Wai
import Plether.Config (Config (..))
import Plether.Database (DbPool, withDb)
import qualified Plether.Database.Insights.Registration as LegacyDb
import Plether.Ethereum.Client (EthClient)
import Plether.Handlers.InsightsRegistration
  ( TradingAccountProof (..), readBoundedRequestBody, registrationHeaders
  , respondError, safeRegistrationIO, trustedClientIp, validateJsonRequest
  , verifyOwnedTradingAccount
  )
import Plether.Insights.Registration.Config (RegistrationConfig (..))
import Plether.Insights.Registration.Crypto
  ( base64UrlNoPad, constantTimeEqual, generateHexToken, generateOpaqueToken
  , generateUuidV4, secretDigest
  )
import Plether.Insights.Registration.Provider (verifyTurnstile)
import Plether.Insights.Registration.Types
  ( RegistrationError, RegistrationErrorCode (..), SessionRequest (..)
  , WalletChallengeRequest (..), WalletVerifyRequest (..), registrationError
  )
import Plether.Insights.Registration.Wallet
  ( recoverPersonalSignAddress, renderDivisionsWalletChallenge
  , walletChallengeLifetimeSeconds
  )
import Plether.Utils.Address (isValidAddress)
import System.Environment (lookupEnv)
import Web.Scotty
  ( ActionM, ScottyM, get, json, post, queryParamMaybe, request
  , setHeader, status
  )

data DiscordConfig = DiscordConfig
  { dcClientId :: Text
  , dcClientSecret :: Text
  , dcCallbackUrl :: Text
  , dcRulesVersion :: Text
  , dcPrivacyVersion :: Text
  }

-- Divisions linking is independent of the historical competition. It shares
-- the provisioned edge, Turnstile and HMAC keys, with distinct digest domains.
loadDiscordConfig :: RegistrationConfig -> IO (Either String (Maybe DiscordConfig))
loadDiscordConfig registrationConfig = do
  enabled <- lookupEnv "INSIGHTS_DIVISIONS_REGISTRATION_ENABLED"
  clientId <- lookupEnv "DISCORD_OAUTH_CLIENT_ID"
  clientSecret <- lookupEnv "DISCORD_OAUTH_CLIENT_SECRET"
  callback <- lookupEnv "DISCORD_OAUTH_CALLBACK_URL"
  rules <- lookupEnv "INSIGHTS_DIVISIONS_RULES_VERSION"
  privacy <- lookupEnv "INSIGHTS_DIVISIONS_PRIVACY_VERSION"
  pure $ case enabled of
    Nothing -> Right Nothing
    Just "false" -> Right Nothing
    Just "true" -> do
      cid <- required "DISCORD_OAUTH_CLIENT_ID" clientId
      secret <- required "DISCORD_OAUTH_CLIENT_SECRET" clientSecret
      uri <- required "DISCORD_OAUTH_CALLBACK_URL" callback
      rulesVersion <- required "INSIGHTS_DIVISIONS_RULES_VERSION" rules
      privacyVersion <- required "INSIGHTS_DIVISIONS_PRIVACY_VERSION" privacy
      if T.all (`elem` ['0'..'9']) cid && T.length cid >= 17 && T.length cid <= 22
        && T.length secret >= 16 && T.length secret <= 256
        && uri == rcPublicOrigin registrationConfig <> "/api/insights/v1/divisions/registrations/discord/callback"
        && validVersion rulesVersion && validVersion privacyVersion
        then Right $ Just $ DiscordConfig cid secret uri rulesVersion privacyVersion
        else Left "Divisions Discord OAuth or consent configuration is invalid"
    _ -> Left "INSIGHTS_DIVISIONS_REGISTRATION_ENABLED must be true or false"
  where
    required name = maybe (Left $ name <> " is required") $ \value ->
      let textValue = T.pack value
       in if T.null textValue || textValue /= T.strip textValue
            then Left $ name <> " is required without surrounding whitespace"
            else Right textValue
    validVersion value = T.length value <= 64 && T.all (\c -> c `elem` ['a'..'z'] || c `elem` ['A'..'Z'] || c `elem` ['0'..'9'] || c `elem` ['-', '_', '.']) value

data LinkSession = LinkSession
  { lsExpiresAt :: UTCTime
  , lsDiscordId :: Maybe Text
  , lsDiscordName :: Maybe Text
  , lsOwner :: Maybe Text
  , lsTradingAccount :: Maybe Text
  , lsCompleted :: Bool
  }

instance FromRow LinkSession where
  fromRow = LinkSession <$> field <*> field <*> field <*> field <*> field <*> field

data Authenticated = Authenticated
  { auToken :: Text
  , auDigest :: BS.ByteString
  , auSession :: LinkSession
  }

data CompleteRequest = CompleteRequest Bool Bool Text Text (Maybe Bool)
instance FromJSON CompleteRequest where
  parseJSON = withObject "CompleteRequest" $ \v ->
    CompleteRequest <$> v .: "acceptRules" <*> v .: "acceptPrivacy" <*> v .: "rulesVersion" <*> v .: "privacyVersion" <*> v .:? "showDiscordHandle"

data DiscordToken = DiscordToken Text Text Text
instance FromJSON DiscordToken where
  parseJSON = withObject "DiscordToken" $ \v -> DiscordToken <$> v .: "access_token" <*> v .: "token_type" <*> v .: "scope"

data DiscordUser = DiscordUser Text Text
instance FromJSON DiscordUser where
  parseJSON = withObject "DiscordUser" $ \v -> DiscordUser <$> v .: "id" <*> v .: "username"

registerDivisionsRegistrationRoutes :: DbPool -> EthClient -> Config -> Manager -> Maybe DiscordConfig -> ScottyM ()
registerDivisionsRegistrationRoutes pool perpsClient config manager maybeDiscord =
  case (cfgRegistrationConfig config, maybeDiscord) of
    (Just registrationConfig, Just discord) -> do

          post "/api/insights/v1/divisions/registrations/session" $ do
            checked <- guardRequest pool registrationConfig False
            case checked of
              Left err -> respondError err
              Right waiRequest -> do
                body <- decodeBody
                case body of
                  Left err -> respondError err
                  Right (SessionRequest turnstileToken) -> do
                    outcome <- safeRegistrationIO $ do
                      now <- getCurrentTime
                      nonce <- generateUuidV4
                      let trustedIp = either (const "") id $ trustedClientIp waiRequest
                      verified <- verifyTurnstile manager registrationConfig now trustedIp nonce turnstileToken
                      case verified of
                        Left err -> pure $ Left err
                        Right _ -> do
                          token <- generateOpaqueToken 32
                          let sessionDigest = tokenDigest registrationConfig token
                          withDb pool $ \connection -> do
                            _ <- execute_ connection "DELETE FROM insights_divisions_link_sessions WHERE expires_at<NOW()"
                            _ <- execute connection
                              "INSERT INTO insights_divisions_link_sessions (session_digest, expires_at) VALUES (?, NOW() + (? * INTERVAL '1 second'))"
                              (Binary sessionDigest, rcSessionTtlSeconds registrationConfig)
                            pure ()
                          pure $ Right token
                    case outcome of
                      Left err -> respondError err
                      Right token -> do
                        setSessionCookie registrationConfig token
                        loaded <- liftIO $ loadSession pool registrationConfig token
                        maybe (respondError expiredSession) (json . sessionResponse registrationConfig discord) loaded

          get "/api/insights/v1/divisions/registrations/session" $ do
            checked <- guardRequest pool registrationConfig False
            case checked of
              Left err -> respondError err
              Right waiRequest -> do
                loaded <- liftIO $ maybe (pure Nothing) (loadSession pool registrationConfig) $ sessionCookie waiRequest
                maybe (respondError expiredSession) (json . sessionResponse registrationConfig discord) loaded

          post "/api/insights/v1/divisions/registrations/discord/authorize" $ do
            authenticated <- guardMutation pool registrationConfig
            case authenticated of
              Left err -> respondError err
              Right auth -> do
                body <- decodeBody
                case body of
                  Left err -> respondError err
                  Right (_ :: EmptyObject) -> do
                    stateToken <- liftIO $ generateOpaqueToken 32
                    stored <- liftIO $ withDb pool $ \connection -> execute connection
                      "UPDATE insights_divisions_link_sessions SET oauth_state_digest=?, oauth_expires_at=NOW() + INTERVAL '10 minutes', discord_id=NULL, discord_username=NULL, owner_wallet=NULL, trading_account=NULL, challenge_message=NULL, challenge_owner=NULL, challenge_expires_at=NULL, updated_at=NOW() WHERE session_digest=? AND expires_at>NOW() AND completed_at IS NULL"
                      (Binary $ digest registrationConfig "divisions-oauth-state" stateToken, Binary $ auDigest auth)
                    if stored /= 1 then respondError expiredSession else
                      json $ object ["authorizationUrl" .= discordAuthorizationUrl discord stateToken]

          get "/api/insights/v1/divisions/registrations/discord/callback" $ do
            checked <- guardRequest pool registrationConfig True
            case checked of
              Left err -> respondError err
              Right waiRequest -> do
                maybeCode <- queryParamMaybe "code"
                maybeState <- queryParamMaybe "state"
                let token = sessionCookie waiRequest
                    validCode = maybeCode >>= boundedCode
                    validState = maybeState >>= boundedState
                case (token, validCode, validState) of
                  (Just sessionToken, Just code, Just stateToken) -> do
                    outcome <- safeRegistrationIO $ do
                      let sessionDigest = tokenDigest registrationConfig sessionToken
                      consumed <- withDb pool $ \connection -> execute connection
                        "UPDATE insights_divisions_link_sessions SET oauth_state_digest=NULL, oauth_expires_at=NULL, updated_at=NOW() WHERE session_digest=? AND oauth_state_digest=? AND oauth_expires_at>NOW() AND expires_at>NOW() AND completed_at IS NULL"
                        (Binary sessionDigest, Binary $ digest registrationConfig "divisions-oauth-state" stateToken)
                      if consumed /= 1 then pure $ Left invalidRequest else do
                        identified <- fetchDiscordIdentity manager discord code
                        case identified of
                          Left err -> pure $ Left err
                          Right (DiscordUser discordId username) -> do
                            updated <- withDb pool $ \connection -> execute connection
                              "UPDATE insights_divisions_link_sessions SET discord_id=?, discord_username=?, owner_wallet=NULL, trading_account=NULL, challenge_message=NULL, challenge_owner=NULL, challenge_expires_at=NULL, updated_at=NOW() WHERE session_digest=? AND expires_at>NOW() AND completed_at IS NULL"
                              (discordId, username, Binary sessionDigest)
                            pure $ if updated == 1 then Right () else Left expiredSession
                    case outcome of
                      Left _ -> pure ()
                      Right () -> setSessionCookie registrationConfig sessionToken
                  _ -> pure ()
                status status303
                setHeader "Location" $ LT.fromStrict $ rcPublicOrigin registrationConfig <> "/register"

          post "/api/insights/v1/divisions/registrations/wallet/challenge" $ do
            authenticated <- guardMutation pool registrationConfig
            case authenticated of
              Left err -> respondError err
              Right auth -> do
                body <- decodeBody
                case body of
                  Left err -> respondError err
                  Right (WalletChallengeRequest ownerAddress) -> do
                    outcome <- safeRegistrationIO $ do
                      let owner = T.toLower ownerAddress
                      if not $ validOwner ownerAddress then pure $ Left invalidRequest else
                        case lsDiscordId $ auSession auth of
                          Nothing -> pure $ Left incomplete
                          Just _ -> do
                            issuedAt <- floor <$> getPOSIXTime
                            nonce <- generateHexToken 16
                            let expiresAt = issuedAt + walletChallengeLifetimeSeconds
                            case renderDivisionsWalletChallenge (rcPublicOrigin registrationConfig) (cfgPerpsChainId config) owner nonce issuedAt expiresAt of
                              Left _ -> pure $ Left invalidRequest
                              Right message -> do
                                updated <- withDb pool $ \connection -> execute connection
                                  "UPDATE insights_divisions_link_sessions SET challenge_message=?, challenge_owner=?, challenge_expires_at=NOW() + INTERVAL '5 minutes', owner_wallet=NULL, trading_account=NULL, updated_at=NOW() WHERE session_digest=? AND discord_id IS NOT NULL AND expires_at>NOW() AND completed_at IS NULL"
                                  (message, owner, Binary $ auDigest auth)
                                pure $ if updated == 1 then Right message else Left expiredSession
                    either respondError (json . (\message -> object ["message" .= message])) outcome

          post "/api/insights/v1/divisions/registrations/wallet/verify" $ do
            authenticated <- guardMutation pool registrationConfig
            case authenticated of
              Left err -> respondError err
              Right auth -> do
                body <- decodeBody
                case body of
                  Left err -> respondError err
                  Right (WalletVerifyRequest ownerAddress signature) -> do
                    outcome <- safeRegistrationIO $ do
                      let owner = T.toLower ownerAddress
                      if not $ validOwner ownerAddress then pure $ Left invalidRequest else do
                        claimed <- withDb pool $ \connection -> do
                          rows <- query connection
                            "WITH claimed AS (SELECT session_digest, challenge_message FROM insights_divisions_link_sessions WHERE session_digest=? AND challenge_owner=? AND challenge_message IS NOT NULL AND challenge_expires_at>NOW() AND expires_at>NOW() AND completed_at IS NULL FOR UPDATE) UPDATE insights_divisions_link_sessions s SET challenge_message=NULL, challenge_owner=NULL, challenge_expires_at=NULL, updated_at=NOW() FROM claimed WHERE s.session_digest=claimed.session_digest RETURNING claimed.challenge_message"
                            (Binary $ auDigest auth, owner) :: IO [Only (Maybe Text)]
                          pure rows
                        -- Consume the challenge before signature verification.
                        case claimed of
                          [Only (Just message)] -> do
                            recovered <- recoverPersonalSignAddress message signature
                            case recovered of
                              Right signer | signer == owner -> do
                                proof <- verifyOwnedTradingAccount perpsClient owner
                                case proof of
                                  Left err -> pure $ Left err
                                  Right accountProof -> do
                                    updated <- withDb pool $ \connection -> execute connection
                                      "UPDATE insights_divisions_link_sessions SET owner_wallet=?, trading_account=?, updated_at=NOW() WHERE session_digest=? AND discord_id IS NOT NULL AND expires_at>NOW() AND completed_at IS NULL"
                                      (owner, tapTradingAccount accountProof, Binary $ auDigest auth)
                                    pure $ if updated == 1 then Right () else Left expiredSession
                              _ -> pure $ Left $ registrationError InvalidSignature "Wallet signature is invalid"
                          _ -> pure $ Left $ registrationError ExpiredChallenge "Wallet challenge expired or was already used"
                    case outcome of
                      Left err -> respondError err
                      Right () -> do
                        loaded <- liftIO $ loadSession pool registrationConfig $ auToken auth
                        maybe (respondError expiredSession) (json . sessionResponse registrationConfig discord) loaded

          post "/api/insights/v1/divisions/registrations/complete" $ do
            authenticated <- guardMutation pool registrationConfig
            case authenticated of
              Left err -> respondError err
              Right auth -> do
                body <- decodeBody
                case body of
                  Left err -> respondError err
                  Right (CompleteRequest acceptRules acceptPrivacy rulesVersion privacyVersion showDiscordHandle) -> do
                    outcome <- safeRegistrationIO $ do
                      let session = auSession auth
                      if not (acceptRules && acceptPrivacy) || rulesVersion /= dcRulesVersion discord || privacyVersion /= dcPrivacyVersion discord
                        then pure $ Left $ registrationError ConsentMismatch "Rules and privacy acceptance are required"
                        else case (lsDiscordId session, lsDiscordName session, lsOwner session, lsTradingAccount session) of
                          (Just discordId, Just username, Just owner, Just account) -> do
                            proof <- verifyOwnedTradingAccount perpsClient owner
                            case proof of
                              Right latest | tapTradingAccount latest == account -> do
                                completed <- withDb pool $ \connection -> withTransaction connection $ do
                                  current <- query connection
                                    "SELECT discord_id, discord_username, owner_wallet, trading_account, completed_at IS NOT NULL FROM insights_divisions_link_sessions WHERE session_digest=? AND expires_at>NOW() FOR UPDATE"
                                    (Only $ Binary $ auDigest auth) :: IO [(Maybe Text, Maybe Text, Maybe Text, Maybe Text, Bool)]
                                  if current /= [(Just discordId, Just username, Just owner, Just account, False)]
                                    then pure $ Left expiredSession
                                    else do
                                      inserted <- execute connection
                                        "INSERT INTO insights_divisions_links (discord_id, discord_username, owner_wallet, trading_account, rules_version, privacy_version, show_discord_handle) VALUES (?, ?, ?, ?, ?, ?, COALESCE(?, FALSE)) ON CONFLICT DO NOTHING"
                                        (discordId, username, owner, account, rulesVersion, privacyVersion, showDiscordHandle)
                                      same <- query connection
                                        "SELECT EXISTS (SELECT 1 FROM insights_divisions_links WHERE discord_id=? AND owner_wallet=? AND trading_account=?)"
                                        (discordId, owner, account) :: IO [Only Bool]
                                      if inserted /= 1 && same /= [Only True]
                                        then pure $ Left $ registrationError DuplicateRegistration "This Discord account or wallet is already linked"
                                        else do
                                          _ <- execute connection
                                            "UPDATE insights_divisions_links SET discord_username=?, rules_version=?, privacy_version=?, show_discord_handle=COALESCE(?, show_discord_handle) WHERE discord_id=? AND owner_wallet=? AND trading_account=?"
                                            (username, rulesVersion, privacyVersion, showDiscordHandle, discordId, owner, account)
                                          updated <- execute connection
                                            "UPDATE insights_divisions_link_sessions SET completed_at=NOW(), updated_at=NOW() WHERE session_digest=? AND completed_at IS NULL"
                                            (Only $ Binary $ auDigest auth)
                                          pure $ if updated == 1 then Right () else Left expiredSession
                                pure completed
                              Left err -> pure $ Left err
                              _ -> pure $ Left incomplete
                          _ -> pure $ Left incomplete
                    case outcome of
                      Left err -> respondError err
                      Right () -> do
                        loaded <- liftIO $ loadSession pool registrationConfig $ auToken auth
                        maybe (respondError expiredSession) (json . sessionResponse registrationConfig discord) loaded

    _ -> pure ()

data EmptyObject = EmptyObject
instance FromJSON EmptyObject where
  parseJSON = withObject "EmptyObject" $ \v -> if null v then pure EmptyObject else fail "Expected empty object"

ensureDivisionsSchema :: Connection -> IO ()
ensureDivisionsSchema connection = do
  _ <- execute_ connection
    "CREATE TABLE IF NOT EXISTS insights_divisions_links (discord_id TEXT PRIMARY KEY, discord_username TEXT NOT NULL, owner_wallet TEXT NOT NULL UNIQUE, trading_account TEXT NOT NULL UNIQUE, rules_version TEXT NOT NULL, privacy_version TEXT NOT NULL, created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(), CHECK (discord_id ~ '^[0-9]{17,22}$'), CHECK (owner_wallet ~ '^0x[0-9a-f]{40}$'), CHECK (trading_account ~ '^0x[0-9a-f]{40}$'))"
  _ <- execute_ connection
    "ALTER TABLE insights_divisions_links ADD COLUMN IF NOT EXISTS show_discord_handle BOOLEAN NOT NULL DEFAULT FALSE"
  _ <- execute_ connection
    "CREATE TABLE IF NOT EXISTS insights_divisions_link_sessions (session_digest BYTEA PRIMARY KEY, expires_at TIMESTAMPTZ NOT NULL, oauth_state_digest BYTEA, oauth_expires_at TIMESTAMPTZ, discord_id TEXT, discord_username TEXT, owner_wallet TEXT, trading_account TEXT, challenge_message TEXT, challenge_owner TEXT, challenge_expires_at TIMESTAMPTZ, completed_at TIMESTAMPTZ, updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(), CHECK (octet_length(session_digest)=32), CHECK (oauth_state_digest IS NULL OR octet_length(oauth_state_digest)=32), CHECK (num_nonnulls(oauth_state_digest,oauth_expires_at) IN (0,2)), CHECK (num_nonnulls(challenge_message,challenge_owner,challenge_expires_at) IN (0,3)))"
  _ <- execute_ connection "CREATE INDEX IF NOT EXISTS idx_insights_divisions_link_sessions_expiry ON insights_divisions_link_sessions(expires_at)"
  pure ()

loadSession :: DbPool -> RegistrationConfig -> Text -> IO (Maybe Authenticated)
loadSession pool config token = do
  let tokenHash = tokenDigest config token
  rows <- withDb pool $ \connection -> query connection
    "SELECT expires_at, discord_id, discord_username, owner_wallet, trading_account, completed_at IS NOT NULL FROM insights_divisions_link_sessions WHERE session_digest=? AND expires_at>NOW()"
    (Only $ Binary tokenHash)
  pure $ case rows of
    [session] -> Just $ Authenticated token tokenHash session
    _ -> Nothing

sessionResponse :: RegistrationConfig -> DiscordConfig -> Authenticated -> Value
sessionResponse config discord authenticated = object ["registration" .= object
  [ "status" .= (if lsCompleted session then "completed" else "in_progress" :: Text)
  , "csrfToken" .= csrfToken config (auToken authenticated)
  , "expiresAt" .= lsExpiresAt session
  , "steps" .= object ["discord" .= (if lsDiscordId session == Nothing then "pending" else "verified" :: Text), "wallet" .= (if lsOwner session == Nothing then "pending" else "verified" :: Text), "completed" .= lsCompleted session]
  , "identity" .= fmap (\username -> object ["username" .= username]) (lsDiscordName session)
  , "wallet" .= case (lsOwner session, lsTradingAccount session) of
      (Just owner, Just account) -> Just $ object ["ownerAddress" .= owner, "tradingAccount" .= account]
      _ -> Nothing
  , "capabilities" .= object ["publicDiscordHandle" .= True]
  , "requiredConsents" .= object ["rulesVersion" .= dcRulesVersion discord, "privacyVersion" .= dcPrivacyVersion discord]
  ]] where session = auSession authenticated

guardRequest :: DbPool -> RegistrationConfig -> Bool -> ActionM (Either RegistrationError Wai.Request)
guardRequest pool config callback = do
  registrationHeaders
  waiRequest <- request
  let headers = Wai.requestHeaders waiRequest
      edgeToken = lookup "X-Plether-Registration-Origin" headers
      edgeAllowed = maybe False (\supplied -> BS.length supplied <= 512 &&
        (constantTimeEqual supplied (rcOriginToken config) || maybe False (constantTimeEqual supplied) (rcOriginTokenNext config))) edgeToken
      originAllowed = lookup "Origin" headers == Just (TE.encodeUtf8 $ rcPublicOrigin config)
  if not edgeAllowed || not originAllowed then pure $ Left $ registrationError OriginRejected "Registration request origin was rejected"
    else if callback then pure $ Right waiRequest
    else case trustedClientIp waiRequest of
      Left err -> pure $ Left err
      Right ip -> do
        allowed <- safeRegistrationIO $ Right <$> withDb pool (\connection ->
          LegacyDb.registrationRateLimitAllowed connection
            (digest config "divisions-rate-ip" ip) (rcIpRateLimitPerMinute config))
        pure $ case allowed of
          Left err -> Left err
          Right False -> Left $ registrationError RateLimited "Too many registration requests"
          Right True -> Right waiRequest

guardMutation :: DbPool -> RegistrationConfig -> ActionM (Either RegistrationError Authenticated)
guardMutation pool config = do
  checked <- guardRequest pool config False
  case checked of
    Left err -> pure $ Left err
    Right waiRequest -> case (sessionCookie waiRequest, lookup "X-Registration-CSRF" $ Wai.requestHeaders waiRequest) of
      (Just token, Just supplied) -> do
        loaded <- liftIO $ loadSession pool config token
        pure $ case loaded of
          Nothing -> Left expiredSession
          Just auth | constantTimeEqual supplied (TE.encodeUtf8 $ csrfToken config token) -> Right auth
          _ -> Left $ registrationError CsrfFailed "Registration CSRF check failed"
      _ -> pure $ Left expiredSession

decodeBody :: FromJSON a => ActionM (Either RegistrationError a)
decodeBody = do
  waiRequest <- request
  case validateJsonRequest waiRequest of
    Left err -> pure $ Left err
    Right () -> do
      bounded <- liftIO $ readBoundedRequestBody (16 * 1024) waiRequest
      pure $ case bounded of
        Left () -> Left invalidRequest
        Right body -> either (const $ Left invalidRequest) Right $ eitherDecode body

sessionCookie :: Wai.Request -> Maybe Text
sessionCookie waiRequest = do
  raw <- lookup "Cookie" $ Wai.requestHeaders waiRequest
  let matches = [BS.drop 1 value | part <- BS.split 59 raw,
        let (name, value) = BS.break (== 61) $ BS.dropWhile (== 32) part,
        name == "__Host-plether_divisions", not $ BS.null value]
  case matches of
    [value] | BS.length value == 43 ->
      case TE.decodeUtf8' value of
        Left _ -> Nothing
        Right token -> token <$ if boundedState token == Just token then Just () else Nothing
    _ -> Nothing

setSessionCookie :: RegistrationConfig -> Text -> ActionM ()
setSessionCookie config token = setHeader "Set-Cookie" $ LT.fromStrict $
  "__Host-plether_divisions=" <> token <> "; Path=/; Max-Age="
    <> T.pack (show $ rcSessionTtlSeconds config) <> "; Secure; HttpOnly; SameSite=Lax"

csrfToken :: RegistrationConfig -> Text -> Text
csrfToken config token = base64UrlNoPad $ digest config "divisions-csrf" token

tokenDigest :: RegistrationConfig -> Text -> BS.ByteString
tokenDigest config = digest config "divisions-session"

digest :: RegistrationConfig -> Text -> Text -> BS.ByteString
digest config domain = secretDigest (rcLookupHmacKey config) domain . TE.encodeUtf8

discordAuthorizationUrl :: DiscordConfig -> Text -> Text
discordAuthorizationUrl config stateToken = "https://discord.com/oauth2/authorize" <>
  TE.decodeUtf8 (renderSimpleQuery True
    [("response_type", "code"), ("client_id", TE.encodeUtf8 $ dcClientId config),
     ("redirect_uri", TE.encodeUtf8 $ dcCallbackUrl config), ("scope", "identify"),
     ("state", TE.encodeUtf8 stateToken)])

fetchDiscordIdentity :: Manager -> DiscordConfig -> Text -> IO (Either RegistrationError DiscordUser)
fetchDiscordIdentity manager config code = do
  tokenRequest <- parseRequest "https://discord.com/api/v10/oauth2/token"
  let form = renderSimpleQuery False
        [("grant_type", "authorization_code"), ("code", TE.encodeUtf8 code),
         ("redirect_uri", TE.encodeUtf8 $ dcCallbackUrl config)]
      authenticated = applyBasicAuth (TE.encodeUtf8 $ dcClientId config) (TE.encodeUtf8 $ dcClientSecret config) tokenRequest
      prepared = authenticated { method = "POST", requestBody = RequestBodyBS form,
        requestHeaders = ("Content-Type", "application/x-www-form-urlencoded") : requestHeaders authenticated,
        redirectCount = 0, responseTimeout = responseTimeoutMicro 8000000 }
  tokenResponse <- httpLbs prepared manager
  case decodeProviderResponse tokenResponse of
    Left err -> pure $ Left err
    Right (DiscordToken accessToken tokenType scopes)
      | tokenType /= "Bearer" || "identify" `notElem` T.words scopes || T.null accessToken || T.length accessToken > 8192 -> pure $ Left providerError
      | otherwise -> do
          userRequest <- parseRequest "https://discord.com/api/v10/users/@me"
          userResponse <- httpLbs (userRequest { requestHeaders = [("Authorization", "Bearer " <> TE.encodeUtf8 accessToken)], redirectCount = 0, responseTimeout = responseTimeoutMicro 8000000 }) manager
          pure $ do
            user@(DiscordUser discordId username) <- decodeProviderResponse userResponse
            if T.length discordId >= 17 && T.length discordId <= 22 && T.all (`elem` ['0'..'9']) discordId
              && T.length username >= 1 && T.length username <= 64 && not (T.any isControl username)
              then Right user else Left providerError
  where
    decodeProviderResponse response =
      if statusCode (responseStatus response) /= 200 || LBS.length (responseBody response) > 16384
        then Left providerError
        else either (const $ Left providerError) Right $ eitherDecode $ responseBody response

boundedState :: Text -> Maybe Text
boundedState value = value <$ if T.length value == 43 && T.all (\c -> c `elem` ['a'..'z'] || c `elem` ['A'..'Z'] || c `elem` ['0'..'9'] || c == '-' || c == '_') value then Just () else Nothing

boundedCode :: Text -> Maybe Text
boundedCode value = value <$ if T.length value >= 1 && T.length value <= 2048 && not (T.any (\c -> isControl c || isSpace c) value) then Just () else Nothing

validOwner :: Text -> Bool
validOwner value = value == T.toLower value && value == T.strip value && value /= "0x0000000000000000000000000000000000000000" && isValidAddress value

invalidRequest, expiredSession, incomplete, providerError :: RegistrationError
invalidRequest = registrationError InvalidRequest "Registration request is invalid"
expiredSession = registrationError ExpiredSession "Registration session expired"
incomplete = registrationError RegistrationIncomplete "Complete Discord and wallet verification first"
providerError = registrationError ProviderUnavailable "Discord verification is temporarily unavailable"
