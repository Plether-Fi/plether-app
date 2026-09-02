module Plether.Handlers.InsightsRegistration
  ( registerInsightsRegistrationRoutes
  , initializeInsightsRegistration
  , registrationUiRedirect
  , maskEmail
  , maximumRegistrationBodyBytes
  , validateJsonRequest
  , readBoundedRequestBody
  , validateOrigin
  , csrfTokenFromRequest
  , sessionTokenFromRequest
  , parseCanonicalRpcQuantity
  , canonicalBlockLookupParams
  , completionResultDecision
  , ownedAccountDecision
  , xAccountAgeEligible
  , XFollowFailureDisposition (..)
  , xFollowFailureDisposition
  ) where

import Control.Exception
  ( SomeAsyncException
  , SomeException
  , fromException
  , onException
  , throwIO
  , try
  )
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
  ( FromJSON (..)
  , Value (..)
  , eitherDecode
  , toJSON
  , withObject
  )
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import qualified Data.Text.Lazy as LT
import Data.Time.Clock.POSIX
  ( getPOSIXTime
  , posixSecondsToUTCTime
  )
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Word (Word8)
import Network.HTTP.Client (Manager)
import Network.HTTP.Types.Status (status303)
import qualified Network.Wai as Wai
import Plether.AA.Pimlico
  ( OwnedTradingAccountFailure (..)
  , resolveOwnedTradingAccountAtBlock
  )
import Plether.Config (Config (..))
import Plether.Database (DbPool, withDb)
import qualified Plether.Database.Insights.Registration as Db
import Plether.Ethereum.Client
  ( EthClient
  , rpcCall
  )
import Plether.Insights.Registration.Config (RegistrationConfig (..))
import Plether.Insights.Registration.Crypto
  ( EncryptedValue (..)
  , constantTimeEqual
  , decryptValue
  , emailLookupDigest
  , encryptValue
  , generateHexToken
  , generateOpaqueToken
  , generatePkcePair
  , generateUuidV4
  , normalizeEmail
  , registrationFieldAad
  , secretDigest
  , uuidV4FromDigest
  )
import Plether.Insights.Registration.Provider
  ( XAccessToken
  , XFollowVerificationFailure (..)
  , buildXAuthorizationUrl
  , exchangeXAuthorizationCode
  , fetchXIdentity
  , verifyXFollow
  , verifyTurnstile
  , xAccessTokenBytes
  , xAccessTokenFromBytes
  )
import Plether.Insights.Registration.Types
  ( AuthorizationResponse (..)
  , CompleteRegistrationRequest (..)
  , RegistrationError (..)
  , RegistrationErrorCode (..)
  , RegistrationIdentityView (..)
  , RegistrationResponse (..)
  , RegistrationSessionView (..)
  , RegistrationStatus (..)
  , RegistrationSteps (..)
  , RegistrationWalletView (..)
  , RequiredConsents (..)
  , SessionRequest (..)
  , VerificationStep (..)
  , WalletChallengeRequest (..)
  , WalletChallengeResponse (..)
  , WalletVerifyRequest (..)
  , XIdentity (..)
  , registrationError
  , registrationErrorCodeFromText
  , registrationErrorCodeText
  , registrationErrorStatus
  )
import Plether.Insights.Registration.Wallet
  ( recoverPersonalSignAddress
  , renderWalletChallenge
  , walletChallengeLifetimeSeconds
  )
import Plether.Utils.Address (isValidAddress)
import Plether.Utils.Hex (hexToInteger, intToHex)
import Web.Scotty
  ( ActionM
  , ScottyM
  , get
  , json
  , pathParam
  , post
  , queryParamMaybe
  , request
  , setHeader
  , status
  )

registrationCookieName :: BS.ByteString
registrationCookieName = "__Host-plether_registration"

maximumRegistrationBodyBytes :: Int
maximumRegistrationBodyBytes = 16 * 1024

data EmptyMutation = EmptyMutation

instance FromJSON EmptyMutation where
  parseJSON = withObject "EmptyMutation" $ \objectValue ->
    if null objectValue
      then pure EmptyMutation
      else fail "request object must be empty"

data AuthenticatedSession = AuthenticatedSession
  { asDigest :: BS.ByteString
  , asRow :: Db.RegistrationSessionRow
  , asCsrfToken :: Text
  }

data TradingAccountProof = TradingAccountProof
  { tapTradingAccount :: Text
  , tapBlockNumber :: Integer
  , tapBlockHash :: Text
  }

registerInsightsRegistrationRoutes
  :: DbPool
  -> EthClient
  -> Config
  -> Manager
  -> ScottyM ()
registerInsightsRegistrationRoutes pool perpsClient config manager =
  case cfgRegistrationConfig config of
    Nothing -> pure ()
    Just registrationConfig -> do
      post "/api/insights/v1/competitions/:slug/registrations/session" $ do
        registrationHeaders
        slug <- pathParam "slug"
        guarded <- guardSessionCreationRequest pool registrationConfig slug
        case guarded of
          Left err -> respondError err
          Right (trustedIp, waiRequest) -> do
            decoded <- decodeBoundedJson waiRequest
            case decoded of
              Left err -> respondError err
              Right (SessionRequest turnstileToken) -> do
                outcome <- safeRegistrationIO $
                  createSession pool manager registrationConfig slug trustedIp turnstileToken
                case outcome of
                  Left err -> respondError err
                  Right (sessionToken, registrationView) -> do
                    setRegistrationCookie registrationConfig sessionToken
                    json $ RegistrationResponse registrationView

      get "/api/insights/v1/competitions/:slug/registrations/session" $ do
        registrationHeaders
        slug <- pathParam "slug"
        readResult <- guardAuthenticatedRead pool registrationConfig slug
        case readResult of
          Left err -> respondError err
          Right authenticated ->
            json $ RegistrationResponse $ sessionView registrationConfig authenticated

      post "/api/insights/v1/competitions/:slug/registrations/x/authorize" $ do
        registrationHeaders
        slug <- pathParam "slug"
        guarded <- guardAuthenticatedMutation pool registrationConfig slug
        case guarded of
          Left err -> respondError err
          Right (authenticated, waiRequest) -> do
            decoded <- decodeBoundedJson waiRequest
            case decoded of
              Left err -> respondError err
              Right EmptyMutation -> do
                outcome <- safeRegistrationIO $
                  beginXAuthorization pool registrationConfig slug authenticated
                case outcome of
                  Left err -> respondError err
                  Right authorizationUrl -> do
                    -- Refresh the same host-only cookie before leaving for X.
                    -- This prevents an authorization started near the original
                    -- expiry from losing resumability if the callback response
                    -- itself never reaches the browser.
                    case sessionTokenFromRequest waiRequest of
                      Just sessionToken -> setRegistrationCookie registrationConfig sessionToken
                      Nothing -> pure ()
                    json $ AuthorizationResponse authorizationUrl

      get "/api/insights/v1/competitions/:slug/registrations/x/callback" $ do
        registrationHeaders
        slug <- pathParam "slug"
        callbackGuard <- guardOAuthCallbackRequest pool registrationConfig slug
        maybeCode <- queryParamMaybe "code"
        maybeState <- queryParamMaybe "state"
        callbackOutcome <- case callbackGuard of
          Left _ -> pure Nothing
          Right waiRequest ->
            case (boundedState =<< maybeState, sessionTokenFromRequest waiRequest) of
              (Just oauthState, Just sessionToken) ->
                case boundedOAuthCode =<< maybeCode of
                  Just code -> do
                    result <- safeRegistrationIO $
                      completeXCallback pool manager registrationConfig slug sessionToken code oauthState
                    pure $ either (const Nothing) Just result
                  Nothing -> do
                    let sessionDigest = digestSecret registrationConfig "session" sessionToken
                    consumed <- safeRegistrationIO $ Right <$> withDb pool (\connection ->
                      Db.consumeOAuthChallenge connection sessionDigest $
                        digestSecret registrationConfig "oauth-state" oauthState)
                    case consumed of
                      Right (Db.OAuthChallengeConsumed challenge) ->
                        do
                          _ <- safeRegistrationIO $ Right <$> recordOAuthCallbackFailure
                            pool
                            sessionDigest
                            (if Db.ocrRegistrationOpen challenge then expiredChallengeError else closedError)
                          pure ()
                      Right Db.OAuthChallengeExpired ->
                        do
                          _ <- safeRegistrationIO $ Right <$> recordOAuthCallbackFailure pool sessionDigest expiredChallengeError
                          pure ()
                      _ -> pure ()
                    pure Nothing
              _ -> pure Nothing
        case callbackOutcome of
          Just (nextSessionToken, _) -> setRegistrationCookie registrationConfig nextSessionToken
          Nothing -> pure ()
        status status303
        setHeader "Location" $ LT.fromStrict $ registrationUiRedirect registrationConfig slug

      post "/api/insights/v1/competitions/:slug/registrations/x/follow" $ do
        registrationHeaders
        slug <- pathParam "slug"
        guarded <- guardAuthenticatedMutation pool registrationConfig slug
        case guarded of
          Left err -> respondError err
          Right (_, waiRequest) -> do
            decoded <- decodeBoundedJson waiRequest
            case decoded of
              Left err -> respondError err
              Right EmptyMutation -> do
                outcome <- safeRegistrationIO $
                  completeXFollow pool manager registrationConfig slug waiRequest
                either respondError (json . RegistrationResponse) outcome

      post "/api/insights/v1/competitions/:slug/registrations/wallet/challenge" $ do
        registrationHeaders
        slug <- pathParam "slug"
        guarded <- guardAuthenticatedMutation pool registrationConfig slug
        case guarded of
          Left err -> respondError err
          Right (authenticated, waiRequest) -> do
            decoded <- decodeBoundedJson waiRequest
            case decoded of
              Left err -> respondError err
              Right (WalletChallengeRequest ownerAddress) -> do
                outcome <- safeRegistrationIO $
                  createWalletChallenge pool registrationConfig config slug authenticated ownerAddress
                either respondError json outcome

      post "/api/insights/v1/competitions/:slug/registrations/wallet/verify" $ do
        registrationHeaders
        slug <- pathParam "slug"
        guarded <- guardAuthenticatedMutation pool registrationConfig slug
        case guarded of
          Left err -> respondError err
          Right (_, waiRequest) -> do
            decoded <- decodeBoundedJson waiRequest
            case decoded of
              Left err -> respondError err
              Right walletRequest -> do
                outcome <- safeRegistrationIO $
                  verifyWallet pool perpsClient registrationConfig config slug waiRequest walletRequest
                either respondError (json . RegistrationResponse) outcome

      post "/api/insights/v1/competitions/:slug/registrations/complete" $ do
        registrationHeaders
        slug <- pathParam "slug"
        guarded <- guardAuthenticatedMutation pool registrationConfig slug
        case guarded of
          Left err -> respondError err
          Right (authenticated, waiRequest) -> do
            decoded <- decodeBoundedJson waiRequest
            case decoded of
              Left err -> respondError err
              Right completionRequest -> do
                outcome <- safeRegistrationIO $
                  finishRegistration pool perpsClient registrationConfig config slug authenticated completionRequest
                either respondError (json . RegistrationResponse) outcome

initializeInsightsRegistration :: DbPool -> EthClient -> Config -> IO (Either String ())
initializeInsightsRegistration pool perpsClient config =
  case cfgRegistrationConfig config of
    Nothing -> pure $ Right ()
    Just registrationConfig -> do
      rpcValid <- validateRegistrationRpcChain perpsClient config
      valid <-
        if rpcValid
          then initializeRegistration pool perpsClient config registrationConfig
          else pure False
      pure $
        if valid
          then Right ()
          else Left "Provisioned Insights registration metadata or RPC chain does not match its configured competition"

initializeRegistration :: DbPool -> EthClient -> Config -> RegistrationConfig -> IO Bool
initializeRegistration pool _perpsClient config registrationConfig =
  withDb pool $ \connection -> do
    Db.ensureRegistrationSchema connection
    _ <- Db.cleanupExpiredRegistrationSecrets connection
    competition <- Db.getRegistrationCompetition connection $ rcXCallbackCompetitionSlug registrationConfig
    case competition of
      Just row | competitionMatchesConfig config registrationConfig row -> do
        privateConfigValid <-
          Db.provisionRegistrationCompetitionConfig
            connection
            (rcXCallbackCompetitionSlug registrationConfig)
            ( digestSecret
                registrationConfig
                "x-target-user-id"
                (rcXTargetUserId registrationConfig)
            )
            (rcPrivacyVersion registrationConfig)
        if not privateConfigValid
          then pure False
          else if not $ rcActivationEnabled registrationConfig
            then pure True
            else case Db.rgcRegistrationOpenTimestamp row of
              Just _ -> pure True
              Nothing -> do
                now <- getPOSIXSeconds
                if now >= Db.rgcRegistrationCloseTimestamp row
                  then pure True
                  else
                    Db.openRegistrationIfConfigured
                      connection
                      (rcXCallbackCompetitionSlug registrationConfig)
                      (rcPrivacyVersion registrationConfig)
      _ -> pure False

-- | Registration depends on the AA factory and the target chain, but not on
-- competition contracts that may still be awaiting deployment.
validateRegistrationRpcChain :: EthClient -> Config -> IO Bool
validateRegistrationRpcChain client config = do
  chainResponse <- rpcCall client "eth_chainId" $ toJSON ([] :: [Value])
  pure $ case chainResponse of
    Right chainValue
      | Right chainId <- parseCanonicalRpcQuantity chainValue ->
          chainId == cfgPerpsChainId config
    _ -> False

competitionMatchesConfig :: Config -> RegistrationConfig -> Db.RegistrationCompetition -> Bool
competitionMatchesConfig config registrationConfig competition =
  Db.rgcChainId competition == cfgPerpsChainId config
    && T.toCaseFold (Db.rgcTargetXHandle competition) == T.toCaseFold (rcXTargetHandle registrationConfig)
    && toInteger (Db.rgcMinimumXAccountAgeDays competition) == rcMinimumXAccountAgeDays registrationConfig
    && Db.rgcRulesVersion competition == rcRulesVersion registrationConfig
    && case Db.rgcRegistrationOpenTimestamp competition of
      Nothing -> maybe True (== rcPrivacyVersion registrationConfig) $ Db.rgcPrivacyNoticeVersion competition
      Just _ -> Db.rgcPrivacyNoticeVersion competition == Just (rcPrivacyVersion registrationConfig)

guardSessionCreationRequest
  :: DbPool
  -> RegistrationConfig
  -> Text
  -> ActionM (Either RegistrationError (Text, Wai.Request))
guardSessionCreationRequest pool config slug = do
  waiRequest <- request
  case validateEdge config slug waiRequest >> validateOrigin config waiRequest >> trustedClientIp waiRequest of
    Left err -> pure $ Left err
    Right trustedIp -> do
      allowed <- safeRegistrationIO $
        Right <$> checkRateLimit pool config "ip" (TE.encodeUtf8 trustedIp) (rcIpRateLimitPerMinute config)
      pure $ case allowed of
        Left err -> Left err
        Right False -> Left rateLimitedError
        Right True -> Right (trustedIp, waiRequest)

guardAuthenticatedMutation
  :: DbPool
  -> RegistrationConfig
  -> Text
  -> ActionM (Either RegistrationError (AuthenticatedSession, Wai.Request))
guardAuthenticatedMutation pool config slug = do
  waiRequest <- request
  case validateEdge config slug waiRequest >> validateOrigin config waiRequest >> trustedClientIp waiRequest of
    Left err -> pure $ Left err
    Right trustedIp -> do
      ipLimit <- safeRegistrationIO $
        Right <$> checkRateLimit pool config "ip" (TE.encodeUtf8 trustedIp) (rcIpRateLimitPerMinute config)
      case ipLimit of
        Left err -> pure $ Left err
        Right False -> pure $ Left rateLimitedError
        Right True ->
          case csrfTokenFromRequest waiRequest of
            Nothing -> pure $ Left csrfError
            Just suppliedCsrf -> do
              authenticated <- safeRegistrationIO $
                authenticateSession pool config slug waiRequest $ Just suppliedCsrf
              case authenticated of
                Left err -> pure $ Left err
                Right session -> do
                  sessionLimit <- safeRegistrationIO $
                    Right
                      <$> checkRateLimit
                        pool
                        config
                        "session"
                        (asDigest session)
                        (rcSessionRateLimitPerMinute config)
                  pure $ case sessionLimit of
                    Left err -> Left err
                    Right False -> Left rateLimitedError
                    Right True -> Right (session, waiRequest)

guardAuthenticatedRead
  :: DbPool
  -> RegistrationConfig
  -> Text
  -> ActionM (Either RegistrationError AuthenticatedSession)
guardAuthenticatedRead pool config slug = do
  waiRequest <- request
  case validateEdge config slug waiRequest >> validateOrigin config waiRequest >> trustedClientIp waiRequest of
    Left err -> pure $ Left err
    Right trustedIp -> do
      ipLimit <- safeRegistrationIO $
        Right <$> checkRateLimit pool config "ip" (TE.encodeUtf8 trustedIp) (rcIpRateLimitPerMinute config)
      case ipLimit of
        Left err -> pure $ Left err
        Right False -> pure $ Left rateLimitedError
        Right True -> do
          authenticated <- safeRegistrationIO $ authenticateSession pool config slug waiRequest Nothing
          case authenticated of
            Left err -> pure $ Left err
            Right session -> do
              sessionLimit <- safeRegistrationIO $
                Right
                  <$> checkRateLimit
                    pool
                    config
                    "session"
                    (asDigest session)
                    (rcSessionRateLimitPerMinute config)
              pure $ case sessionLimit of
                Left err -> Left err
                Right False -> Left rateLimitedError
                Right True -> Right session

guardOAuthCallbackRequest
  :: DbPool
  -> RegistrationConfig
  -> Text
  -> ActionM (Either RegistrationError Wai.Request)
guardOAuthCallbackRequest pool config slug = do
  waiRequest <- request
  case validateEdge config slug waiRequest >> trustedClientIp waiRequest of
    Left err -> pure $ Left err
    Right trustedIp -> do
      limits <- safeRegistrationIO $ Right <$> do
        ipAllowed <- checkRateLimit pool config "ip" (TE.encodeUtf8 trustedIp) (rcIpRateLimitPerMinute config)
        sessionAllowed <- case sessionTokenFromRequest waiRequest of
          Nothing -> pure True
          Just sessionToken ->
            checkRateLimit
              pool
              config
              "session"
              (digestSecret config "session" sessionToken)
              (rcSessionRateLimitPerMinute config)
        pure $ ipAllowed && sessionAllowed
      pure $ case limits of
        Left err -> Left err
        Right False -> Left rateLimitedError
        Right True -> Right waiRequest

validateEdge :: RegistrationConfig -> Text -> Wai.Request -> Either RegistrationError ()
validateEdge config slug waiRequest
  | slug /= rcXCallbackCompetitionSlug config =
      Left $ registrationError RegistrationNotFound "Registration is not available for this competition"
  | otherwise =
      case lookup "X-Plether-Registration-Origin" $ Wai.requestHeaders waiRequest of
        Just supplied
          | BS.length supplied <= 512
          , edgeOriginTokenAccepted config supplied -> Right ()
        _ -> Left $ registrationError OriginRejected "Registration request origin was rejected"

edgeOriginTokenAccepted :: RegistrationConfig -> BS.ByteString -> Bool
edgeOriginTokenAccepted config supplied =
  currentMatches `seq` nextMatches `seq` (currentMatches || nextMatches)
  where
    suppliedDigest = secretDigest (rcLookupHmacKey config) "edge-origin" supplied
    matches expected =
      constantTimeEqual
        suppliedDigest
        (secretDigest (rcLookupHmacKey config) "edge-origin" expected)
    currentMatches = matches $ rcOriginToken config
    nextMatches = maybe False matches $ rcOriginTokenNext config

validateOrigin :: RegistrationConfig -> Wai.Request -> Either RegistrationError ()
validateOrigin config waiRequest =
  case lookup "Origin" $ Wai.requestHeaders waiRequest of
    Just supplied
      | supplied == TE.encodeUtf8 (rcPublicOrigin config) -> Right ()
    _ -> Left $ registrationError OriginRejected "Registration request origin was rejected"

trustedClientIp :: Wai.Request -> Either RegistrationError Text
trustedClientIp waiRequest =
  case lookup "CF-Connecting-IP" $ Wai.requestHeaders waiRequest of
    Just raw
      | BS.length raw >= 2
      , BS.length raw <= 64
      , BS.all validIpByte raw -> Right $ TE.decodeUtf8With TEE.lenientDecode raw
    _ -> Left $ registrationError InvalidRequest "A trusted client address is required"
  where
    validIpByte byte =
      byte == 46
        || byte == 58
        || byte `elem` [48 .. 57]
        || byte `elem` [65 .. 70]
        || byte `elem` [97 .. 102]

authenticateSession
  :: DbPool
  -> RegistrationConfig
  -> Text
  -> Wai.Request
  -> Maybe Text
  -> IO (Either RegistrationError AuthenticatedSession)
authenticateSession pool config slug waiRequest maybeCsrf =
  case sessionTokenFromRequest waiRequest of
    Nothing -> pure $ Left expiredSessionError
    Just sessionToken -> do
      let sessionDigest = digestSecret config "session" sessionToken
      maybeRow <- withDb pool $ \connection -> Db.getRegistrationSession connection sessionDigest
      pure $ do
        row <- maybe (Left expiredSessionError) Right maybeRow
        unless (Db.rsrCompetitionSlug row == slug) $ Left expiredSessionError
        csrfToken <- decryptField config row "csrf" $ Db.rsrCsrfEncrypted row
        unless (isBase64Url43 csrfToken) $ Left internalError
        unless
          (constantTimeEqual (digestSecret config "csrf" csrfToken) $ Db.rsrCsrfDigest row)
          (Left internalError)
        case maybeCsrf of
          Nothing -> pure ()
          Just supplied -> do
            unless (isBase64Url43 supplied) $ Left csrfError
            unless
              (constantTimeEqual (digestSecret config "csrf" supplied) $ Db.rsrCsrfDigest row)
              (Left csrfError)
        pure $ AuthenticatedSession sessionDigest row csrfToken

createSession
  :: DbPool
  -> Manager
  -> RegistrationConfig
  -> Text
  -> Text
  -> Text
  -> IO (Either RegistrationError (Text, RegistrationSessionView))
createSession pool manager config slug trustedIp turnstileToken = do
  competition <- withDb pool $ \connection -> Db.getRegistrationCompetition connection slug
  now <- getPOSIXSeconds
  case competition of
    Nothing -> pure $ Left registrationNotFoundError
    Just row
      | not $ registrationOpenAt now row -> pure $ Left closedError
      | otherwise -> do
          applicationId <- generateUuidV4
          let turnstileDigest = digestSecret config "turnstile" turnstileToken
              turnstileIdempotency =
                uuidV4FromDigest $ digestSecret config "turnstile-idempotency" turnstileToken
          case turnstileIdempotency of
            Nothing -> pure $ Left internalError
            Just idempotencyKey -> do
              turnstile <- verifyTurnstile manager config (posixSecondsToUTCTime $ fromInteger now) trustedIp idempotencyKey turnstileToken
              case turnstile of
                Left err -> pure $ Left err
                Right _ -> do
                  sessionToken <- generateOpaqueToken 32
                  csrfToken <- generateOpaqueToken 32
                  encryptedCsrf <- encryptField config slug applicationId "csrf" $ TE.encodeUtf8 csrfToken
                  case encryptedCsrf of
                    Left err -> pure $ Left err
                    Right csrfEnvelope -> do
                      created <- withDb pool $ \connection ->
                        Db.createRegistrationSession
                          connection
                          slug
                          applicationId
                          turnstileDigest
                          (digestSecret config "session" sessionToken)
                          (digestSecret config "csrf" csrfToken)
                          csrfEnvelope
                          (rcSessionTtlSeconds config)
                      case created of
                        Db.SessionRegistrationClosed -> pure $ Left closedError
                        Db.SessionTurnstileReplay ->
                          pure $ Left $ registrationError TurnstileFailed "Spam-protection verification failed"
                        Db.SessionCreated -> do
                          maybeRow <- withDb pool $ \connection ->
                            Db.getRegistrationSession connection $ digestSecret config "session" sessionToken
                          pure $ do
                            sessionRow <- maybe (Left internalError) Right maybeRow
                            Right (sessionToken, sessionView config $ AuthenticatedSession (digestSecret config "session" sessionToken) sessionRow csrfToken)

beginXAuthorization
  :: DbPool
  -> RegistrationConfig
  -> Text
  -> AuthenticatedSession
  -> IO (Either RegistrationError Text)
beginXAuthorization pool config slug authenticated = do
  (verifier, challenge) <- generatePkcePair
  oauthState <- generateOpaqueToken 32
  encryptedVerifier <-
    encryptField config slug (Db.rsrApplicationId $ asRow authenticated) "pkce-verifier" $ TE.encodeUtf8 verifier
  case encryptedVerifier of
    Left err -> pure $ Left err
    Right verifierEnvelope -> do
      stored <- withDb pool $ \connection ->
        Db.storeOAuthChallenge
          connection
          (asDigest authenticated)
          (digestSecret config "oauth-state" oauthState)
          verifierEnvelope
          (rcSessionTtlSeconds config)
      pure $ case stored of
        Db.MutationApplied -> buildXAuthorizationUrl config slug oauthState challenge
        Db.MutationClosed -> Left closedError
        Db.MutationIncomplete ->
          Left $ registrationError RegistrationIncomplete "X authorization cannot be restarted after follow verification"

completeXCallback
  :: DbPool
  -> Manager
  -> RegistrationConfig
  -> Text
  -> Text
  -> Text
  -> Text
  -> IO (Either RegistrationError (Text, RegistrationSessionView))
completeXCallback pool manager config slug sessionToken authorizationCode oauthState = do
  let sessionDigest = digestSecret config "session" sessionToken
  challengeResult <- withDb pool $ \connection ->
    Db.consumeOAuthChallenge connection sessionDigest $ digestSecret config "oauth-state" oauthState
  case challengeResult of
    Db.OAuthChallengeUnavailable -> pure $ Left expiredChallengeError
    Db.OAuthChallengeExpired -> do
      recordOAuthCallbackFailure pool sessionDigest expiredChallengeError
      pure $ Left expiredChallengeError
    Db.OAuthChallengeConsumed challenge -> do
      outcome <-
        if not (Db.ocrRegistrationOpen challenge)
          then pure $ Left closedError
          else if Db.ocrCompetitionSlug challenge /= slug
            then pure $ Left expiredSessionError
            else
              case decryptEnvelope config (registrationFieldAad slug (Db.ocrApplicationId challenge) "pkce-verifier") $ Db.ocrPkceVerifier challenge of
                Left err -> pure $ Left err
                Right verifierBytes -> do
                  let verifier = TE.decodeUtf8With TEE.lenientDecode verifierBytes
                  exchanged <- exchangeXAuthorizationCode manager config authorizationCode verifier
                  case exchanged of
                    Left err -> pure $ Left err
                    Right accessToken -> do
                      identityResult <- fetchXIdentity manager accessToken
                      case identityResult of
                        Left err -> pure $ Left err
                        Right identity ->
                          storeIdentityAndRefresh pool config slug sessionToken sessionDigest challenge identity accessToken
      case outcome of
        Left err -> recordOAuthCallbackFailure pool sessionDigest err >> pure (Left err)
        Right result -> pure $ Right result

storeIdentityAndRefresh
  :: DbPool
  -> RegistrationConfig
  -> Text
  -> Text
  -> BS.ByteString
  -> Db.OAuthChallengeRow
  -> XIdentity
  -> XAccessToken
  -> IO (Either RegistrationError (Text, RegistrationSessionView))
storeIdentityAndRefresh pool config slug sessionToken sessionDigest challenge identity accessToken = do
  competition <- withDb pool $ \connection -> Db.getRegistrationCompetition connection slug
  case competition of
    Nothing -> pure $ Left registrationNotFoundError
    Just competitionRow
      | not $ xAccountAgeEligible competitionRow $ xiCreatedAt identity ->
          pure $ Left $ registrationError XAccountTooNew "The X account does not meet the minimum age requirement"
      | otherwise -> do
          let applicationId = Db.ocrApplicationId challenge
              normalizedConfirmedEmail = normalizeEmail $ xiConfirmedEmail identity
          encryptedXUser <- encryptField config slug applicationId "x-user-id" $ TE.encodeUtf8 $ xiUserId identity
          encryptedEmail <- encryptField config slug applicationId "email" $ TE.encodeUtf8 normalizedConfirmedEmail
          encryptedToken <- encryptField config slug applicationId "x-access-token" $ xAccessTokenBytes accessToken
          case (encryptedXUser, encryptedEmail, encryptedToken) of
            (Right xUserEnvelope, Right emailEnvelope, Right tokenEnvelope) -> do
              nextCsrfToken <- generateOpaqueToken 32
              encryptedCsrf <- encryptField config slug applicationId "csrf" $ TE.encodeUtf8 nextCsrfToken
              case encryptedCsrf of
                Left err -> pure $ Left err
                Right csrfEnvelope -> do
                  stored <- withDb pool $ \connection ->
                    Db.storeXIdentityAndRefreshSession
                      connection
                      sessionDigest
                      (digestSecret config "csrf" nextCsrfToken)
                      csrfEnvelope
                      (rcSessionTtlSeconds config)
                      applicationId
                      (digestSecret config "x-user-id" $ xiUserId identity)
                      xUserEnvelope
                      (emailLookupDigest (rcLookupHmacKey config) normalizedConfirmedEmail)
                      (maskEmail normalizedConfirmedEmail)
                      (xiCreatedAt identity)
                      (xiUsername identity)
                      emailEnvelope
                      tokenEnvelope
                  case stored of
                    Db.MutationClosed -> pure $ Left closedError
                    Db.MutationIncomplete ->
                      pure $ Left $ registrationError RegistrationIncomplete "X authorization state changed"
                    Db.MutationApplied -> do
                      loaded <- loadSessionView pool config slug sessionDigest
                      pure $ fmap (\registrationView -> (sessionToken, registrationView)) loaded
            _ -> pure $ Left internalError

recordOAuthCallbackFailure :: DbPool -> BS.ByteString -> RegistrationError -> IO ()
recordOAuthCallbackFailure pool sessionDigest err = do
  _ <- withDb pool $ \connection ->
    Db.recordOAuthCallbackError connection sessionDigest $
      registrationErrorCodeText $ reCode err
  pure ()

data XFollowFailureDisposition
  = ReleaseXFollowAttempt
  | ResetXIdentity
  deriving stock (Show, Eq)

xFollowFailureDisposition :: XFollowVerificationFailure -> XFollowFailureDisposition
xFollowFailureDisposition XFollowVerificationFailure {xfvfResetIdentity}
  | xfvfResetIdentity = ResetXIdentity
  | otherwise = ReleaseXFollowAttempt

completeXFollow
  :: DbPool
  -> Manager
  -> RegistrationConfig
  -> Text
  -> Wai.Request
  -> IO (Either RegistrationError RegistrationSessionView)
completeXFollow pool manager config slug waiRequest =
  case sessionTokenFromRequest waiRequest of
    Nothing -> pure $ Left expiredSessionError
    Just sessionToken -> do
      let sessionDigest = digestSecret config "session" sessionToken
      attemptId <- generateUuidV4
      claimed <- withDb pool $ \connection ->
        Db.claimXFollowMaterial
          connection
          sessionDigest
          (digestSecret config "x-target-user-id" $ rcXTargetUserId config)
          attemptId
      case claimed of
        Db.XFollowClaimClosed -> pure $ Left closedError
        Db.XFollowClaimUnavailable ->
          pure $ Left $ registrationError RegistrationIncomplete "X authorization must be completed first"
        Db.XFollowClaimed material
          | Db.xfmrCompetitionSlug material /= slug -> pure $ Left expiredSessionError
          | otherwise -> do
              let applicationId = Db.xfmrApplicationId material
                  resetIdentity = withDb pool $ \connection -> Db.resetXIdentityAfterFollowFailure connection applicationId attemptId
                  releaseAttempt = withDb pool $ \connection -> Db.releaseXFollowAttempt connection applicationId attemptId
                  providerCall = do
                    let xUser = decryptEnvelope config (registrationFieldAad slug applicationId "x-user-id") $ Db.xfmrXUserId material
                        access = decryptEnvelope config (registrationFieldAad slug applicationId "x-access-token") $ Db.xfmrAccessToken material
                    case (xUser, access >>= maybe (Left internalError) Right . xAccessTokenFromBytes) of
                      (Right xUserBytes, Right accessToken) ->
                        verifyXFollow
                          manager
                          config
                          (TE.decodeUtf8With TEE.lenientDecode xUserBytes)
                          accessToken
                      _ -> pure $ Left $ XFollowVerificationFailure internalError True
              -- Unexpected local exceptions must not destroy a still-usable
              -- OAuth identity. The two-minute lease is enough to prevent a
              -- concurrent verification while this attempt is in flight.
              followResult <- providerCall `onException` releaseAttempt
              case followResult of
                Left failure@XFollowVerificationFailure {xfvfError} ->
                  case xFollowFailureDisposition failure of
                    ResetXIdentity -> resetIdentity >> pure (Left xfvfError)
                    ReleaseXFollowAttempt -> releaseAttempt >> pure (Left xfvfError)
                Right () -> do
                  confirmed <-
                    (withDb pool $ \connection -> Db.confirmXFollow connection applicationId attemptId)
                      `onException` releaseAttempt
                  if not confirmed
                    then resetIdentity >> pure (Left closedError)
                    else loadSessionView pool config slug sessionDigest

createWalletChallenge
  :: DbPool
  -> RegistrationConfig
  -> Config
  -> Text
  -> AuthenticatedSession
  -> Text
  -> IO (Either RegistrationError WalletChallengeResponse)
createWalletChallenge pool registrationConfig config slug authenticated ownerAddress =
  case normalizeOwnerInput ownerAddress of
    Nothing -> pure $ Left invalidWalletError
    Just normalizedOwner
      | not $ Db.rsrXFollowVerified $ asRow authenticated ->
          pure $ Left $ registrationError RegistrationIncomplete "The X follow must be verified first"
      | otherwise -> do
          now <- getPOSIXSeconds
          nonce <- generateHexToken 16
          let expiresAt = now + walletChallengeLifetimeSeconds
          case
              renderWalletChallenge
                (rcPublicOrigin registrationConfig)
                slug
                (cfgPerpsChainId config)
                normalizedOwner
                nonce
                now
                expiresAt
            of
              Left _ -> pure $ Left invalidWalletError
              Right message -> do
                encryptedMessage <-
                  encryptField
                    registrationConfig
                    slug
                    (Db.rsrApplicationId $ asRow authenticated)
                    "wallet-message"
                    (TE.encodeUtf8 message)
                case encryptedMessage of
                  Left err -> pure $ Left err
                  Right messageEnvelope -> do
                    stored <- withDb pool $ \connection ->
                      Db.storeWalletChallenge
                        connection
                        (asDigest authenticated)
                        (digestSecret registrationConfig "wallet-nonce" nonce)
                        normalizedOwner
                        expiresAt
                        messageEnvelope
                    pure $ case stored of
                      Db.MutationApplied -> Right $ WalletChallengeResponse message $ renderTimestamp expiresAt
                      Db.MutationClosed -> Left closedError
                      Db.MutationIncomplete ->
                        Left $ registrationError RegistrationIncomplete "The X follow must be verified first"

verifyWallet
  :: DbPool
  -> EthClient
  -> RegistrationConfig
  -> Config
  -> Text
  -> Wai.Request
  -> WalletVerifyRequest
  -> IO (Either RegistrationError RegistrationSessionView)
verifyWallet pool perpsClient registrationConfig _config slug waiRequest walletRequest =
  case normalizeOwnerInput $ wvrOwnerAddress walletRequest of
    Nothing -> pure $ Left invalidWalletError
    Just normalizedOwner ->
      case sessionTokenFromRequest waiRequest of
        Nothing -> pure $ Left expiredSessionError
        Just sessionToken -> do
          let sessionDigest = digestSecret registrationConfig "session" sessionToken
          maybeChallenge <- withDb pool $ \connection ->
            Db.consumeWalletChallenge connection sessionDigest normalizedOwner
          case maybeChallenge of
            Nothing -> pure $ Left $ registrationError ExpiredChallenge "Wallet challenge expired or was already used"
            Just challenge
              | not (Db.wchrRegistrationOpen challenge) -> pure $ Left closedError
              | Db.wchrCompetitionSlug challenge /= slug -> pure $ Left expiredSessionError
              | otherwise ->
                  case decryptEnvelope registrationConfig (registrationFieldAad slug (Db.wchrApplicationId challenge) "wallet-message") $ Db.wchrMessage challenge of
                    Left err -> pure $ Left err
                    Right messageBytes -> do
                      recovered <-
                        recoverPersonalSignAddress
                          (TE.decodeUtf8With TEE.lenientDecode messageBytes)
                          (wvrSignature walletRequest)
                      case recovered of
                        Left _ -> pure $ Left $ registrationError InvalidSignature "Wallet signature is invalid"
                        Right signer
                          | signer /= Db.wchrOwnerWallet challenge ->
                              pure $ Left $ registrationError InvalidSignature "Wallet signature is invalid"
                          | otherwise -> do
                              verified <- verifyOwnedTradingAccount perpsClient signer
                              case verified of
                                Left err -> pure $ Left err
                                Right proof -> do
                                  stored <- withDb pool $ \connection ->
                                    Db.storeVerifiedWallet
                                      connection
                                      (Db.wchrApplicationId challenge)
                                      signer
                                      (tapTradingAccount proof)
                                      (tapBlockNumber proof)
                                      (tapBlockHash proof)
                                  case stored of
                                    Db.MutationApplied -> loadSessionView pool registrationConfig slug sessionDigest
                                    Db.MutationClosed -> pure $ Left closedError
                                    Db.MutationIncomplete ->
                                      pure $ Left $ registrationError RegistrationIncomplete "Wallet verification state changed"

finishRegistration
  :: DbPool
  -> EthClient
  -> RegistrationConfig
  -> Config
  -> Text
  -> AuthenticatedSession
  -> CompleteRegistrationRequest
  -> IO (Either RegistrationError RegistrationSessionView)
finishRegistration pool perpsClient registrationConfig _config slug authenticated requestValue
  | Db.rsrStatus row == "completed" = pure $ Right $ sessionView registrationConfig authenticated
  | otherwise = do
      completionState <- withDb pool $ \connection ->
        Db.registrationCompletionState connection $ asDigest authenticated
      case completionState of
        Db.RegistrationCompletionOpen -> completeOpenRegistration
        Db.RegistrationCompletionAlreadySucceeded ->
          loadSessionView pool registrationConfig slug $ asDigest authenticated
        Db.RegistrationCompletionClosed -> pure $ Left closedError
  where
    row = asRow authenticated
    completeOpenRegistration
      | not (crrAcceptRules requestValue && crrAcceptPrivacy requestValue) =
          pure $ Left $ registrationError ConsentMismatch "Rules and privacy notice acceptance are required"
      | crrRulesVersion requestValue /= Db.rsrRulesVersion row
          || crrPrivacyVersion requestValue /= Db.rsrPrivacyVersion row =
          pure $ Left $ registrationError ConsentMismatch "Rules or privacy notice version does not match"
      | otherwise =
          case (Db.rsrOwnerWallet row, Db.rsrTradingAccount row) of
            (Just owner, Just storedTradingAccount) -> do
              verified <- verifyOwnedTradingAccount perpsClient owner
              case verified of
                Left err -> pure $ Left err
                Right proof
                  | tapTradingAccount proof /= storedTradingAccount ->
                      clearWalletProof
                        >> pure
                          (Left $ registrationError RegistrationIncomplete "Wallet verification state changed; verify the wallet again")
                  | otherwise -> do
                      completed <- withDb pool $ \connection ->
                        Db.completeRegistration
                          connection
                          (asDigest authenticated)
                          (Db.rsrPrivacyVersion row)
                          (crrRulesVersion requestValue)
                          (crrPrivacyVersion requestValue)
                          (crrAcceptPromotionalEmail requestValue)
                          owner
                          storedTradingAccount
                          (tapBlockNumber proof)
                          (tapBlockHash proof)
                      case completionResultDecision completed of
                        Right () -> loadSessionView pool registrationConfig slug $ asDigest authenticated
                        Left err -> pure $ Left err
            _ -> pure $ Left $ registrationError RegistrationIncomplete "Wallet verification is incomplete"
    clearWalletProof = do
      _ <- withDb pool $ \connection ->
        Db.clearVerifiedWallet connection $ Db.rsrApplicationId row
      pure ()

completionResultDecision :: Db.CompletionResult -> Either RegistrationError ()
completionResultDecision = \case
  Db.CompletionSucceeded -> Right ()
  Db.CompletionAlreadySucceeded -> Right ()
  Db.CompletionClosed -> Left closedError
  Db.CompletionIncomplete ->
    Left $ registrationError RegistrationIncomplete "Registration steps are incomplete"
  Db.CompletionDuplicate -> Left duplicateError
  Db.CompletionWalletProofChanged ->
    Left $ registrationError RegistrationIncomplete "Wallet verification state changed; verify the wallet again"

ownedAccountDecision
  :: Either OwnedTradingAccountFailure Text
  -> Either RegistrationError Text
ownedAccountDecision = \case
  Left OwnerWalletIsContract -> Left invalidWalletError
  Left OwnedTradingAccountProofUnavailable -> Left providerUnavailableError
  Right account -> Right account

verifyOwnedTradingAccount
  :: EthClient
  -> Text
  -> IO (Either RegistrationError TradingAccountProof)
verifyOwnedTradingAccount client owner
  | not $ isCanonicalOwner owner = pure $ Left invalidWalletError
  | otherwise = do
      headResponse <- rpcCall client "eth_blockNumber" $ toJSON ([] :: [Value])
      let chainHead = case headResponse of
            Right value -> firstProviderError $ parseCanonicalRpcQuantity value
            Left _ -> Left providerUnavailableError
      case chainHead of
        Left _ -> pure $ Left providerUnavailableError
        Right verificationBlock -> do
          initialHeadHash <- canonicalBlockHash client verificationBlock
          derivedResult <- resolveOwnedTradingAccountAtBlock client owner verificationBlock
          finalHeadHash <- canonicalBlockHash client verificationBlock
          pure $ do
            verificationHash <- initialHeadHash
            rawDerived <- ownedAccountDecision derivedResult
            stableHeadHash <- finalHeadHash
            unless (stableHeadHash == verificationHash) $ Left providerUnavailableError
            let derived = T.toLower $ T.strip rawDerived
            unless (isCanonicalOwner derived) $ Left providerUnavailableError
            Right $
              TradingAccountProof
                { tapTradingAccount = derived
                , tapBlockNumber = verificationBlock
                , tapBlockHash = verificationHash
                }

canonicalBlockHash :: EthClient -> Integer -> IO (Either RegistrationError Text)
canonicalBlockHash client blockNumber = do
  response <-
    rpcCall client "eth_getBlockByNumber" $ canonicalBlockLookupParams blockNumber
  pure $ case response of
    Right (Object blockObject) ->
      case KM.lookup "hash" blockObject of
        Just (String blockHash)
          | isCanonicalHash blockHash -> Right blockHash
        _ -> Left providerUnavailableError
    _ -> Left providerUnavailableError
  where
    isCanonicalHash value =
      T.length value == 66
        && T.take 2 value == "0x"
        && T.all (\character -> character `elem` ['0' .. '9'] || character `elem` ['a' .. 'f']) (T.drop 2 value)

canonicalBlockLookupParams :: Integer -> Value
canonicalBlockLookupParams blockNumber =
  toJSON [String $ "0x" <> intToHex blockNumber, Bool False]

-- | Parse a canonical JSON-RPC quantity.  Security-sensitive registration
-- proofs must never use the permissive generic hex parser, which would turn
-- malformed input into zero.
parseCanonicalRpcQuantity :: Value -> Either Text Integer
parseCanonicalRpcQuantity (String "0x0") = Right 0
parseCanonicalRpcQuantity (String value)
  | T.take 2 value == "0x"
  , let digits = T.drop 2 value
  , not $ T.null digits
  , T.head digits /= '0'
  , T.all isLowerHex digits = Right $ hexToInteger digits
  where
    isLowerHex character =
      character `elem` ['0' .. '9'] || character `elem` ['a' .. 'f']
parseCanonicalRpcQuantity _ = Left "JSON-RPC quantity is not canonical"

firstProviderError :: Either Text a -> Either RegistrationError a
firstProviderError = either (const $ Left providerUnavailableError) Right

loadSessionView
  :: DbPool
  -> RegistrationConfig
  -> Text
  -> BS.ByteString
  -> IO (Either RegistrationError RegistrationSessionView)
loadSessionView pool config slug sessionDigest = do
  maybeRow <- withDb pool $ \connection -> Db.getRegistrationSession connection sessionDigest
  pure $ do
    row <- maybe (Left expiredSessionError) Right maybeRow
    unless (Db.rsrCompetitionSlug row == slug) $ Left expiredSessionError
    csrf <- decryptField config row "csrf" $ Db.rsrCsrfEncrypted row
    unless
      (constantTimeEqual (digestSecret config "csrf" csrf) $ Db.rsrCsrfDigest row)
      (Left internalError)
    Right $ sessionView config $ AuthenticatedSession sessionDigest row csrf

sessionView :: RegistrationConfig -> AuthenticatedSession -> RegistrationSessionView
sessionView _config authenticated =
  RegistrationSessionView
    { rsvStatus = if completed then RegistrationCompleted else RegistrationInProgress
    , rsvCsrfToken = asCsrfToken authenticated
    , rsvExpiresAt = renderTimestamp $ Db.rsrSessionExpiresTimestamp row
    , rsvSteps =
        RegistrationSteps
          { rsXIdentity = verifiedStep $ Db.rsrXIdentityVerified row
          , rsXFollow = verifiedStep $ Db.rsrXFollowVerified row
          , rsWallet = verifiedStep $ Db.rsrWalletVerified row
          , rsCompleted = completed
          }
    , rsvIdentity =
        RegistrationIdentityView
          <$> Db.rsrXUsername row
          <*> Db.rsrEmailMasked row
    , rsvWallet =
        RegistrationWalletView
          <$> Db.rsrOwnerWallet row
          <*> Db.rsrTradingAccount row
    , rsvOauthErrorCode = Db.rsrOauthErrorCode row >>= registrationErrorCodeFromText
    , rsvRequiredConsents = RequiredConsents (Db.rsrRulesVersion row) (Db.rsrPrivacyVersion row)
    }
  where
    row = asRow authenticated
    completed = Db.rsrStatus row == "completed"
    verifiedStep True = StepVerified
    verifiedStep False = StepPending

decodeBoundedJson :: (FromJSON a) => Wai.Request -> ActionM (Either RegistrationError a)
decodeBoundedJson waiRequest =
  case validateJsonRequest waiRequest of
    Left err -> pure $ Left err
    Right () -> do
      bodyResult <- liftIO $ readBoundedRequestBody maximumRegistrationBodyBytes waiRequest
      pure $ case bodyResult of
        Left () -> Left invalidRequestError
        Right body -> either (const $ Left invalidRequestError) Right $ eitherDecode body

validateJsonRequest :: Wai.Request -> Either RegistrationError ()
validateJsonRequest waiRequest = do
  case lookup "Content-Encoding" $ Wai.requestHeaders waiRequest of
    Nothing -> pure ()
    _ -> Left invalidRequestError
  case lookup "Content-Type" $ Wai.requestHeaders waiRequest of
    Just contentType
      | canonicalMediaType contentType == "application/json" -> pure ()
    _ -> Left invalidRequestError
  case Wai.requestBodyLength waiRequest of
    Wai.KnownLength lengthValue
      | lengthValue > fromIntegral maximumRegistrationBodyBytes -> Left invalidRequestError
    _ -> pure ()

canonicalMediaType :: BS.ByteString -> BS.ByteString
canonicalMediaType = BS.map asciiLower . BS.takeWhile (/= 59)
  where
    asciiLower byte
      | byte >= 65 && byte <= 90 = byte + 32
      | otherwise = byte

readBoundedRequestBody :: Int -> Wai.Request -> IO (Either () LBS.ByteString)
readBoundedRequestBody maximumBytes waiRequest = go 0 []
  where
    go total chunks = do
      chunk <- Wai.getRequestBodyChunk waiRequest
      if BS.null chunk
        then pure $ Right $ LBS.fromChunks $ reverse chunks
        else do
          let nextTotal = total + BS.length chunk
          if nextTotal > maximumBytes
            then pure $ Left ()
            else go nextTotal (chunk : chunks)

checkRateLimit
  :: DbPool
  -> RegistrationConfig
  -> Text
  -> BS.ByteString
  -> Int
  -> IO Bool
checkRateLimit pool config scope key maximumRequests =
  withDb pool $ \connection ->
    Db.registrationRateLimitAllowed
      connection
      (secretDigest (rcLookupHmacKey config) ("rate-" <> scope) key)
      maximumRequests

encryptField
  :: RegistrationConfig
  -> Text
  -> Text
  -> Text
  -> BS.ByteString
  -> IO (Either RegistrationError EncryptedValue)
encryptField config competitionSlug applicationId fieldName plaintext =
  case Map.lookup (rcActiveEmailKeyVersion config) $ rcEmailKeys config of
    Nothing -> pure $ Left internalError
    Just key -> do
      encrypted <-
        encryptValue
          (rcActiveEmailKeyVersion config)
          key
          (registrationFieldAad competitionSlug applicationId fieldName)
          plaintext
      pure $ either (const $ Left internalError) Right encrypted

decryptField
  :: RegistrationConfig
  -> Db.RegistrationSessionRow
  -> Text
  -> EncryptedValue
  -> Either RegistrationError Text
decryptField config row fieldName encrypted = do
  plaintext <-
    decryptEnvelope
      config
      (registrationFieldAad (Db.rsrCompetitionSlug row) (Db.rsrApplicationId row) fieldName)
      encrypted
  either (const $ Left internalError) Right $ TE.decodeUtf8' plaintext

decryptEnvelope
  :: RegistrationConfig
  -> BS.ByteString
  -> EncryptedValue
  -> Either RegistrationError BS.ByteString
decryptEnvelope config aad encrypted = do
  key <- maybe (Left internalError) Right $ Map.lookup (evKeyVersion encrypted) $ rcEmailKeys config
  either (const $ Left internalError) Right $ decryptValue key aad encrypted

digestSecret :: RegistrationConfig -> Text -> Text -> BS.ByteString
digestSecret config domain =
  secretDigest (rcLookupHmacKey config) domain . TE.encodeUtf8

sessionTokenFromRequest :: Wai.Request -> Maybe Text
sessionTokenFromRequest waiRequest = do
  cookieHeader <- lookup "Cookie" $ Wai.requestHeaders waiRequest
  let matches =
        [ BS.drop 1 value
        | component <- BS.split 59 cookieHeader
        , let trimmed = stripAsciiSpace component
        , let (name, value) = BS.break (== 61) trimmed
        , name == registrationCookieName
        , not $ BS.null value
        ]
  case matches of
    [rawToken]
      | BS.length rawToken == 43 ->
          let token = TE.decodeUtf8With TEE.lenientDecode rawToken
           in token <$ if isBase64Url43 token then Just () else Nothing
    _ -> Nothing

csrfTokenFromRequest :: Wai.Request -> Maybe Text
csrfTokenFromRequest waiRequest = do
  raw <- lookup "X-Registration-CSRF" $ Wai.requestHeaders waiRequest
  let token = TE.decodeUtf8With TEE.lenientDecode raw
  token <$ if BS.length raw == 43 && isBase64Url43 token then Just () else Nothing

isBase64Url43 :: Text -> Bool
isBase64Url43 value =
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

boundedState :: Text -> Maybe Text
boundedState value = value <$ if isBase64Url43 value then Just () else Nothing

boundedOAuthCode :: Text -> Maybe Text
boundedOAuthCode value =
  value
    <$ if T.length value >= 1
      && T.length value <= 2048
      && T.all (\character -> character > ' ' && character /= '\DEL') value
      then Just ()
      else Nothing

stripAsciiSpace :: BS.ByteString -> BS.ByteString
stripAsciiSpace = BS.dropWhile (== 32) . dropWhileEndByte (== 32)

dropWhileEndByte :: (Word8 -> Bool) -> BS.ByteString -> BS.ByteString
dropWhileEndByte predicate = BS.reverse . BS.dropWhile predicate . BS.reverse

registrationHeaders :: ActionM ()
registrationHeaders = do
  setHeader "Cache-Control" "no-store, private"
  setHeader "Pragma" "no-cache"
  setHeader "Referrer-Policy" "no-referrer"

setRegistrationCookie :: RegistrationConfig -> Text -> ActionM ()
setRegistrationCookie config sessionToken =
  setHeader "Set-Cookie" $
    LT.fromStrict $
      TE.decodeUtf8 registrationCookieName
        <> "="
        <> sessionToken
        <> "; Path=/; Max-Age="
        <> T.pack (show $ rcSessionTtlSeconds config)
        <> "; Secure; HttpOnly; SameSite=Lax"

respondError :: RegistrationError -> ActionM ()
respondError err = do
  registrationHeaders
  status $ registrationErrorStatus err
  if registrationErrorStatus err == registrationErrorStatus rateLimitedError
    then setHeader "Retry-After" "60"
    else pure ()
  json err

safeRegistrationIO
  :: IO (Either RegistrationError a)
  -> ActionM (Either RegistrationError a)
safeRegistrationIO operation = do
  result <- liftIO $ try @SomeException operation
  case result of
    Left exception ->
      case fromException exception :: Maybe SomeAsyncException of
        Just _ -> liftIO $ throwIO exception
        Nothing -> pure $ Left internalError
    Right outcome -> pure outcome

registrationUiRedirect :: RegistrationConfig -> Text -> Text
registrationUiRedirect config slug =
  rcPublicOrigin config <> "/competitions/" <> slug <> "/register"

registrationOpenAt :: Integer -> Db.RegistrationCompetition -> Bool
registrationOpenAt now competition =
  maybe False (<= now) (Db.rgcRegistrationOpenTimestamp competition)
    && now < Db.rgcRegistrationCloseTimestamp competition
    && not (Db.rgcFinalized competition)

xAccountCutoff :: Db.RegistrationCompetition -> Integer
xAccountCutoff competition =
  Db.rgcStartTimestamp competition
    - toInteger (Db.rgcMinimumXAccountAgeDays competition) * 86400

xAccountAgeEligible :: Db.RegistrationCompetition -> Integer -> Bool
xAccountAgeEligible competition createdTimestamp =
  createdTimestamp <= xAccountCutoff competition

maskEmail :: Text -> Text
maskEmail rawEmail =
  case T.splitOn "@" $ normalizeEmail rawEmail of
    [localPart, domainPart]
      | not (T.null localPart) && not (T.null domainPart) ->
          T.take 1 localPart <> "***@" <> domainPart
    _ -> "***"

renderTimestamp :: Integer -> Text
renderTimestamp =
  T.pack
    . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"
    . posixSecondsToUTCTime
    . fromInteger

getPOSIXSeconds :: IO Integer
getPOSIXSeconds = floor <$> getPOSIXTime

isCanonicalOwner :: Text -> Bool
isCanonicalOwner value =
  value == T.strip value
    && value == T.toLower value
    && T.length value == 42
    && T.take 2 value == "0x"
    && value /= "0x0000000000000000000000000000000000000000"
    && isValidAddress value

normalizeOwnerInput :: Text -> Maybe Text
normalizeOwnerInput raw
  | raw /= T.strip raw = Nothing
  | T.length raw /= 42 || T.take 2 raw /= "0x" = Nothing
  | not $ isValidAddress raw = Nothing
  | normalized == "0x0000000000000000000000000000000000000000" = Nothing
  | otherwise = Just normalized
  where
    normalized = T.toLower raw

closedError :: RegistrationError
closedError = registrationError ClosedRegistration "Competition registration is closed"

expiredSessionError :: RegistrationError
expiredSessionError = registrationError ExpiredSession "Registration session expired"

expiredChallengeError :: RegistrationError
expiredChallengeError =
  registrationError ExpiredChallenge "OAuth challenge expired or was already used"

csrfError :: RegistrationError
csrfError = registrationError CsrfFailed "CSRF validation failed"

registrationNotFoundError :: RegistrationError
registrationNotFoundError = registrationError RegistrationNotFound "Registration is not available for this competition"

invalidWalletError :: RegistrationError
invalidWalletError = registrationError InvalidRequest "Wallet address is invalid"

duplicateError :: RegistrationError
duplicateError = registrationError DuplicateRegistration "This registration cannot be completed"

invalidRequestError :: RegistrationError
invalidRequestError = registrationError InvalidRequest "Registration request is invalid"

rateLimitedError :: RegistrationError
rateLimitedError = registrationError RateLimited "Too many registration requests"

providerUnavailableError :: RegistrationError
providerUnavailableError = registrationError ProviderUnavailable "Registration verification is temporarily unavailable"

internalError :: RegistrationError
internalError = registrationError RegistrationInternalError "Registration is temporarily unavailable"
