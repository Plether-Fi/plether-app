module Plether.AA.Gateway
  ( NativeGatewayState
  , newNativeGatewayState
  , nativeGatewayIssuanceError
  , gatewayReadiness
  , initializeGatewayObservability
  , handleNativeAaRpc
  , attestNativePaymasterProfile
  , ownerAllowedForNativeCanary
  , isCanaryGated
  , validateHardEconomicCaps
  , nativeAccountRateClientKey
  , nativeMaxFeeAllowance
  , nativeStartupFailure
  , SecurityBlockHeader (..)
  , validateSecurityHeaderTime
  , advanceEvidenceSnapshots
  , buildPreparedOperation
  , revalidateSecuritySnapshot
  , agreeAccountIdentity
  , forwardAlto
  , closeAssistanceStatus
  , observePreparationInclusion
  ) where

import Control.Exception (SomeException, try)
import Control.Concurrent.Async (Concurrently (..), concurrently)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Monad (forever, void)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE)
import Data.Foldable (toList)
import Data.Aeson
  ( Value (..)
  , eitherDecode
  , eitherDecodeStrict'
  , encode
  , object
  , toJSON
  , (.=)
  )
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.Text (Text)
import Data.List (sortOn)
import Data.Ord (Down (..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.HTTP.Client
  ( BodyReader
  , HttpException
  , Manager
  , Request (..)
  , RequestBody (..)
  , brRead
  , parseRequest
  , responseBody
  , responseHeaders
  , responseStatus
  , responseTimeoutMicro
  , withResponse
  )
import Network.HTTP.Types.Header (hRetryAfter)
import Network.HTTP.Types.Status (status200, status400, status403, status413, statusCode)
import qualified Plether.AA.Paymaster as Paymaster
import qualified Plether.AA.Preparation as Preparation
import qualified Plether.AA.PreparationRecovery as Recovery
import qualified Plether.Database.AaPreparationRecovery as RecoveryDb
import Plether.Insights.Registration.Wallet (recoverPersonalSignAddress)
import qualified Plether.AA.Reconciler as Reconciler
import qualified Plether.AA.RecoveryReceipt as RecoveryReceipt
import qualified Plether.AA.RecoveryCapability as RecoveryCapability
import Plether.AA.Readiness (newReadiness)
import qualified Plether.AA.Diagnostics as Diagnostics
import qualified Plether.Database.AaPreparation as PreparationDb
import Plether.AA.Timing (Timing, newTiming, timingIdentifier, timed, timingCount, timingHeaders)
import qualified Plether.AA.EvidenceCache as Cache
import Plether.AA.ClientKey
  ( pseudonymousAccountKey
  , pseudonymousClientKey
  )
import Plether.AA.Kms
  ( PaymasterSigner (..)
  , newKmsPaymasterSigner
  )
import qualified Plether.AA.Pimlico as Legacy
import Plether.Config (Config (..), NativeAaConfig (..), AaRpcMode (..), aaRpcModeText)
import Plether.Database (DbPool, withDb)
import Plether.Database.AaSponsorship
  ( SponsorshipAuthorization (..)
  , SponsorshipDraft (..)
  , consumeAaRateLimit
  , AaReconcilerCursor (..)
  , getAaReconcilerCursor
  , getAaIssuancePause
  , getSponsorshipByDigest
  , getSponsorshipByRequestKey
  , getSponsorshipByUserOperationHash
  , getRecoveryReceiptLocator
  , isRecoveryOperationAuthorized
  , isSponsorshipDeliveryAllowedFenced
  , markSponsorshipSubmitted
  , reserveSponsorshipFenced
  , storeSponsorshipSignatureFenced
  )
import Plether.Ethereum.Abi
  ( decodeAddress
  , decodeUint256
  , encodeAddress
  , encodeUint256
  , encodeCall
  , keccak256
  )
import Plether.Ethereum.Client
  ( CallParams (..)
  , EthClient
  , ethCallAtBlock
  , newClient
  , rpcCall
  , withRpcObserver
  )
import Plether.Logging (field, logError, logErrorEvery, logInfo, logWarn)
import Web.Scotty
  ( ActionM
  , header
  , json
  , setHeader
  , status
  )
import qualified Web.Scotty as Scotty
import System.Timeout (timeout)
import System.Environment (lookupEnv)
import Data.Maybe (isJust)
import Plether.Database.CloseAssistance (CloseAssistanceReservation (..))
import qualified Plether.Perps.Manifest as Manifest

data NativeGatewayState = NativeGatewayState
  { ngsSigner :: Maybe PaymasterSigner
  , ngsIssuanceError :: Maybe Text
  , ngsSecurityClient :: Maybe EthClient
  , ngsProfileEvidence :: Cache.EvidenceCache Text ()
  , ngsAccountEvidence :: Cache.EvidenceCache Legacy.ProxyFailure Text
  , ngsSnapshots :: MVar [(Integer, Text)]
  , ngsTiming :: Maybe Timing
  , ngsReadiness :: MVar (Maybe (IO Value))
  , ngsDiagnostics :: MVar (Maybe Diagnostics.DiagnosticSink)
  , ngsAttemptId :: Maybe Text
  , ngsRecoveryOrigin :: Maybe Text
  , ngsRetirementEnabled :: Bool
  , ngsPreparationFence :: Maybe RecoveryDb.Fence
  , ngsCloseAssistance :: Maybe CloseAssistanceConfig
  }

data SecurityBlockHeader = SecurityBlockHeader
  { sbhNumber :: Integer
  , sbhHash :: Text
  , sbhTimestamp :: Integer
  , sbhBaseFeePerGas :: Integer
  }
  deriving stock (Eq, Show)

data NativeSecurityContext = NativeSecurityContext
  { nscPrimaryClient :: EthClient
  , nscSecondaryClient :: EthClient
  , nscHeader :: SecurityBlockHeader
  , nscMaxSafeLagSeconds :: Integer
  , nscRpcMode :: AaRpcMode
  , nscAccountEvidence :: Cache.EvidenceCache Legacy.ProxyFailure Text
  , nscPreparationFence :: Maybe RecoveryDb.Fence
  , nscTiming :: Maybe Timing
  }

nativeGatewayIssuanceError :: NativeGatewayState -> Maybe Text
nativeGatewayIssuanceError = ngsIssuanceError

newNativeGatewayState
  :: Manager
  -> Config
  -> EthClient
  -> IO NativeGatewayState
newNativeGatewayState manager cfg client = do
  base <- newNativeGatewayBaseState manager cfg client
  enabled <- lookupEnv "PERPS_CLOSE_ASSISTANCE_ENABLED"
  global <- lookupEnv "PERPS_CLOSE_ASSISTANCE_GLOBAL_ENABLED"
  lens <- fmap T.pack <$> lookupEnv "PERPS_CLOSE_ASSISTANCE_LENS"
  codeHash <- fmap T.pack <$> lookupEnv "PERPS_CLOSE_ASSISTANCE_LENS_CODE_HASH"
  unless (enabled `elem` [Nothing,Just "false",Just "true"] && global `elem` [Nothing,Just "false",Just "true"]) $
    fail "Close assistance flags must be true or false"
  assistance <- if lens `elem` [Nothing,Just ""] && codeHash `elem` [Nothing,Just ""] && enabled /= Just "true" then pure Nothing else case (lens,codeHash,cfgNativeAaConfig cfg) of
    (Just address,Just hash,Just native)
      | isFixedHex 20 address && isFixedHex 32 hash && (enabled /= Just "true" || naaRpcMode native == DualIndependent) ->
          pure $ Just $ CloseAssistanceConfig (T.toLower address) (T.toLower hash) (global == Just "true") (enabled == Just "true")
    _ -> fail "Enabled close assistance requires a lens, runtime hash, and independently verified native AA"
  origin <- fmap (\value -> if null value then Nothing else Just $ T.pack value) <$> lookupEnv "PERPS_AA_RECOVERY_ORIGIN"
  let recoveryOrigin = maybe Nothing id origin
  retirement <- lookupEnv "PERPS_AA_RECOVERY_RETIREMENT_ENABLED"
  unless (retirement `elem` [Nothing, Just "false", Just "true"]) $ fail "Invalid recovery retirement flag"
  unless (maybe True (\value -> "https://" `T.isPrefixOf` value && not (T.any (`elem` ['\n','\r',' ']) value) && not ("/" `T.isInfixOf` T.drop 8 value)) recoveryOrigin) $ fail "Recovery origin must be an exact HTTPS origin"
  when (retirement == Just "true" && recoveryOrigin == Nothing) $ fail "Retirement requires recovery origin"
  pure base { ngsCloseAssistance = assistance, ngsRecoveryOrigin = recoveryOrigin, ngsRetirementEnabled = retirement == Just "true" }

newNativeGatewayBaseState :: Manager -> Config -> EthClient -> IO NativeGatewayState
newNativeGatewayBaseState manager cfg client = do
  profiles <- Cache.newEvidenceCache 2
  accounts <- Cache.newEvidenceCache 1024
  snapshots <- newMVar []
  readiness <- newMVar Nothing
  diagnostics <- newMVar Nothing
  let make signer failure secondary = NativeGatewayState signer failure secondary profiles accounts snapshots Nothing readiness diagnostics Nothing Nothing False Nothing Nothing
      warm state nativeCfg = do
        when (naaPreparationEnabled nativeCfg && naaSponsorshipEnabled nativeCfg && ngsIssuanceError state == Nothing) $
          void $ forkIO $ forever $ do
            _ <- nativeSecurityContext nativeCfg state client
            threadDelay 1_000_000
        pure state
  case cfgNativeAaConfig cfg of
    Nothing -> pure $ make Nothing Nothing Nothing
    Just nativeCfg -> do
      logInfo "aa_rpc_mode_configured" "Native AA verification RPC mode configured"
        [field "rpc_mode" $ aaRpcModeText $ naaRpcMode nativeCfg]
      case naaRpcMode nativeCfg of
        SingleProviderSepolia -> logWarn "aa_single_provider_canary"
          "Sepolia canary uses one RPC provider; repeated reads are not independent verification" []
        DualIndependent -> pure ()
      securityClient <- newClient $ naaSecurityRpcUrl nativeCfg
      profile <- attestNativePaymasterProfile nativeCfg client securityClient
      case profile of
        Left err -> pure $ make Nothing (Just err) (Just securityClient)
        Right ()
          | not (naaSponsorshipEnabled nativeCfg) ->
              pure $ make Nothing Nothing $ Just securityClient
          | otherwise -> do
              signer <-
                newKmsPaymasterSigner
                  manager
                  (naaKmsKeyId nativeCfg)
                  (naaSignerAddress nativeCfg)
              warm (case signer of
                Left err -> make Nothing (Just err) $ Just securityClient
                Right resolved -> make (Just resolved) Nothing $ Just securityClient) nativeCfg

gatewayReadiness :: NativeGatewayState -> Config -> Maybe DbPool -> EthClient -> IO Value
gatewayReadiness state cfg pool client = do
  readSnapshot <- modifyMVar (ngsReadiness state) $ \current -> case current of
    Just reader -> pure (current, reader)
    Nothing -> do
      reader <- newReadiness cfg pool client (nativeGatewayIssuanceError state == Nothing)
      pure (Just reader, reader)
  readSnapshot

initializeGatewayObservability :: NativeGatewayState -> Config -> Maybe DbPool -> EthClient -> IO ()
initializeGatewayObservability state cfg pool client = case (cfgNativeAaConfig cfg, pool) of
  (Just _, Just database) -> do
    void $ gatewayReadiness state cfg pool client
    modifyMVar_ (ngsDiagnostics state) $ \current -> case current of
      Just _ -> pure current
      Nothing -> Just <$> Diagnostics.startDiagnostics cfg database client
  _ -> pure ()

data CloseAssistanceConfig = CloseAssistanceConfig
  { cacLens :: Text
  , cacCodeHash :: Text
  , cacGlobal :: Bool
  , cacEnabled :: Bool
  }

closeAssistanceStatus :: NativeGatewayState -> Config -> Value
closeAssistanceStatus state cfg = case (ngsCloseAssistance state,cfgNativeAaConfig cfg) of
  (Just assistance,Just native)
    | cacEnabled assistance && naaSponsorshipEnabled native && naaSubmissionEnabled native && isJust (ngsSigner state) ->
        object ["enabled" .= True,"chainId" .= (421614 :: Integer),"lens" .= cacLens assistance,
          "lensCodeHash" .= cacCodeHash assistance,"paymasterAddress" .= naaPaymasterAddress native,
          "canaryOwners" .= (if cacGlobal assistance then [] else naaCanaryOwners native)]
  _ -> object ["enabled" .= False]

assistanceLens :: NativeGatewayState -> Maybe Text
assistanceLens = fmap cacLens . ngsCloseAssistance

assistanceGlobal :: NativeGatewayState -> Maybe Legacy.CloseAssistanceIntent -> Bool
assistanceGlobal state intent = isJust intent && maybe False cacGlobal (ngsCloseAssistance state)

handleNativeAaRpc
  :: NativeGatewayState
  -> Config
  -> Maybe DbPool
  -> EthClient
  -> Manager
  -> ActionM ()
handleNativeAaRpc gatewayState cfg mPool perpsClient manager = do
  timing <- liftIO newTiming
  suppliedAttempt <- fmap TL.toStrict <$> header "X-Plether-Attempt-Id"
  let attempt = suppliedAttempt >>= \value -> if Diagnostics.validAttemptId value then Just (T.toLower value) else Nothing
  let observe = withRpcObserver $ timingCount timing "rpc_calls"
      scoped = gatewayState {ngsTiming = Just timing, ngsSecurityClient = observe <$> ngsSecurityClient gatewayState, ngsAttemptId = attempt}
  timed timing "http_total" $ handleNativeAaRpcTimed scoped cfg mPool (observe perpsClient) manager
  emitTiming timing

handleNativeAaRpcTimed :: NativeGatewayState -> Config -> Maybe DbPool -> EthClient -> Manager -> ActionM ()
handleNativeAaRpcTimed gatewayState cfg mPool perpsClient manager =
  case (cfgNativeAaConfig cfg, mPool) of
    (Nothing, _) ->
      Legacy.respondFailure Null $
        Legacy.unavailable "SPONSOR_UNAVAILABLE" "Self-hosted account abstraction is not configured"
    (_, Nothing) ->
      Legacy.respondFailure Null $
        Legacy.unavailable "SPONSOR_UNAVAILABLE" "The AA authorization database is unavailable"
    (Just nativeCfg, Just pool) -> do
      suppliedToken <- header "X-Plether-AA-Proxy-Token"
      if
        not $
          maybe
            False
            (Legacy.constantTimeTextEq $ naaProxyOriginToken nativeCfg)
            (TL.toStrict <$> suppliedToken)
        then
          Legacy.respondFailure Null $
            Legacy.ProxyFailure status403 (-32001) "Forbidden" "PROXY_AUTH_FAILED" False
        else do
          clientIp <- header "CF-Connecting-IP"
          case (TL.toStrict <$> clientIp) >>= Legacy.validateClientIp of
            Nothing ->
              Legacy.respondFailure Null $
                Legacy.invalidRequest "A trusted CF-Connecting-IP header is required"
            Just trustedIp -> do
              let clientKey =
                    pseudonymousClientKey (naaProxyOriginToken nativeCfg) trustedIp
                  emptyAccountKey =
                    pseudonymousAccountKey (naaProxyOriginToken nativeCfg) "prebody"
              preBodyRate <-
                timeGateway gatewayState "auth_prebody_rate" $ liftDb $
                  withDb pool $ \conn ->
                    consumeAaRateLimit
                      conn
                      "prebody"
                      clientKey
                      emptyAccountKey
                      (naaIpRateLimitPerMinute nativeCfg * 4)
              case preBodyRate of
                Left _ -> Legacy.respondFailure Null databaseUnavailable
                Right False -> Legacy.respondFailure Null Legacy.rateLimited
                Right True -> readAndHandle nativeCfg pool trustedIp clientKey
 where
  readAndHandle nativeCfg pool trustedIp clientKey = do
    waiRequest <- Scotty.request
    requestBody <-
      liftIO $
        Legacy.readBoundedRequestBody
          (naaMaxRequestBytes nativeCfg)
          waiRequest
    case requestBody of
      Left () ->
        Legacy.respondFailure Null $
          Legacy.ProxyFailure status413 (-32600) "Request body is too large" "INVALID_REQUEST" False
      Right boundedBody ->
        case eitherDecode boundedBody of
          Left _ ->
            Legacy.respondFailure Null $
              Legacy.ProxyFailure status400 (-32700) "Invalid JSON" "INVALID_REQUEST" False
          Right value ->
            case Legacy.parseRpcRequest value of
              Left failure -> Legacy.respondFailure Null failure
              Right request -> do
                let (rateScope, ipLimit) =
                      case Legacy.rrMethod request of
                        -- Final issuance is deliberately isolated from the
                        -- general RPC and stub buckets.  pm_getPaymasterData
                        -- is unsigned, so this durable low-volume fence is a
                        -- compensating control before any reservation or KMS
                        -- operation can occur.
                        Legacy.GetPaymasterData ->
                          ("final-issuance", naaFinalRateLimitPerMinute nativeCfg)
                        Legacy.PrepareUserOperation ->
                          ("final-issuance", naaFinalRateLimitPerMinute nativeCfg)
                        Legacy.GetPaymasterStubData ->
                          ("ip", naaIpRateLimitPerMinute nativeCfg)
                        _ -> ("ip", naaIpRateLimitPerMinute nativeCfg * 4)
                    emptyAccountKey =
                      pseudonymousAccountKey (naaProxyOriginToken nativeCfg) "no-account"
                ipRate <-
                  timeGateway gatewayState "ip_rate" $ liftDb $
                    withDb pool $ \conn ->
                      consumeAaRateLimit conn rateScope clientKey emptyAccountKey ipLimit
                case ipRate of
                  Left _ -> Legacy.respondFailure (Legacy.rrId request) databaseUnavailable
                  Right False -> Legacy.respondFailure (Legacy.rrId request) Legacy.rateLimited
                  Right True ->
                    dispatchNative
                      gatewayState
                      cfg
                      nativeCfg
                      pool
                      perpsClient
                      manager
                      trustedIp
                      clientKey
                      request

-- The standard ERC-7677 methods remain available. Only this authenticated,
-- explicitly enabled path can accept a preparation intent.
prepareNativeOperation
  :: NativeGatewayState -> Config -> NativeAaConfig -> DbPool -> EthClient
  -> Manager -> Text -> Legacy.RpcRequest -> ActionM ()
prepareNativeOperation gatewayState cfg nativeCfg pool client manager currentClient request =
  case Preparation.parsePreparationIntent $ Legacy.rrParams request of
    Left failure -> Legacy.respondFailure requestId failure
    Right intent -> do
      let scope = RecoveryDb.Scope (cfgPerpsChainId cfg) (T.toLower $ naaPaymasterAddress nativeCfg) (Preparation.piSender intent) (Preparation.piIdentifier intent)
      authorized <- preparationRecoveryClient cfg pool scope currentClient
      case authorized of
        Left failure -> Legacy.respondFailure requestId failure
        Right clientKey -> do
          token <- liftIO Recovery.randomToken
          claimed <- liftDb $ withDb pool $ \conn -> RecoveryDb.beginPreparation conn scope token
          case claimed of
            Left _ -> Legacy.respondFailure requestId databaseUnavailable
            Right (Left "PREPARATION_RETIRED") -> Legacy.respondFailure requestId $
              Legacy.ProxyFailure status403 (-32001) "This preparation was retired and cannot be reused" "PREPARATION_RETIRED" False
            Right (Left reason) -> Legacy.respondFailure requestId $ Legacy.unavailable reason "Preparation cannot currently be resumed"
            Right (Right fence) -> do
              prepareNativeOperationFenced (gatewayState { ngsPreparationFence = Just fence }) cfg nativeCfg pool client manager clientKey request
              _ <- liftDb $ withDb pool $ \conn -> RecoveryDb.releasePreparation conn fence
              pure ()
 where requestId = Legacy.rrId request

prepareNativeOperationFenced
  :: NativeGatewayState -> Config -> NativeAaConfig -> DbPool -> EthClient
  -> Manager -> Text -> Legacy.RpcRequest -> ActionM ()
prepareNativeOperationFenced gatewayState cfg nativeCfg pool client manager clientKey request = do
  timing <- maybe (liftIO newTiming) pure $ ngsTiming gatewayState
  let identifier = timingIdentifier timing
  if not (naaSponsorshipEnabled nativeCfg)
    then Legacy.respondFailure requestId $ Legacy.unavailable "PREPARATION_DISABLED" "Native preparation is disabled"
    else case Preparation.parsePreparationIntent $ Legacy.rrParams request of
      Left failure -> Legacy.respondFailure requestId failure
      Right intent -> do
        result <- timed timing "prepare" $ runExceptT $ do
          accountRate <- timed timing "account_rate" $ db $ consumeAaRateLimitFor intent
          unless accountRate $ throwE Legacy.rateLimited
          contextResult <- ioStage timing "security" $ nativeSecurityContext nativeCfg gatewayState client
          context <- maybe (throwE securityAttestationUnavailable) pure contextResult
          policyRequest <- checked $ Preparation.internalRequest "eth_estimateUserOperationGas"
            [Object $ Preparation.unsignedSkeleton intent, String nativeEntryPoint]
          policy <- checked (Legacy.validateMethodParams policyRequest) >>= maybe
            (throwE $ Legacy.invalidParams "Missing preparation account") pure
          owner <- ioStage timing "identity" $ verifyAccountIdentityDual context policy
          assistance <- checked $ Legacy.validateNativeActionSequence (assistanceLens gatewayState) cfg (Preparation.piSender intent) owner (Legacy.puoCalls policy)
          when (isJust assistance && not (maybe False cacEnabled $ ngsCloseAssistance gatewayState)) $ throwE paymasterPaused
          _ <- ioStage timing "close_assistance" $ validateCloseAssistanceDual gatewayState (Just context) (Preparation.piSender intent) assistance
          unless (case assistance of
            Nothing -> ownerAllowedForNativeCanary nativeCfg owner
            Just _ -> assistanceGlobal gatewayState assistance || T.toLower owner `elem` naaCanaryOwners nativeCfg) $
            throwE $ Legacy.policyDenied "Trading Account owner is not enabled for the native AA canary"
          _ <- ioStage timing "runtime" $ verifyNativeAccountRuntimeDual nativeCfg context policy
          let boundIntent profile = encodeHex $ keccak256 $ TE.encodeUtf8 $ Preparation.intentHash intent <> profile
              profile = preparationProfileFingerprint nativeCfg
              previous = T.replace Preparation.gasPolicyVersion "execution-headroom-v2-sepolia-cap2100000-150pct-min100000" profile
          fence <- maybe (throwE databaseUnavailable) pure $ ngsPreparationFence gatewayState
          claim <- db $ \conn -> PreparationDb.claimPreparationCompatibleFenced conn fence (naaPreparationEnabled nativeCfg && not (Preparation.piResumeOnly intent)) clientKey
            (Preparation.piSender intent) (Preparation.piIdentifier intent)
            (boundIntent profile) [boundIntent previous] identifier
          stored <- case claim of
            PreparationDb.PreparationFenceLost -> throwE $ Legacy.unavailable "PREPARATION_LEASE_LOST" "Retry the same preparation ID"
            PreparationDb.PreparationConflict -> throwE $ Legacy.invalidParams "Preparation ID is bound to another intent"
            PreparationDb.PreparationExpired -> throwE $ (Legacy.policyDenied "Preparation expired; review a new intent") {Legacy.pfReason = "PREPARATION_UNUSABLE"}
            PreparationDb.PreparationDisabled -> throwE $ Legacy.unavailable "PREPARATION_DISABLED" "Native preparation is disabled"
            PreparationDb.PreparationBusy -> throwE $ Legacy.unavailable "PREPARATION_BUSY" "Retry the same preparation ID"
            PreparationDb.PreparationClaimed operation -> pure operation
          bound <- db $ \conn -> PreparationDb.bindPreparationDeployment conn clientKey
            (Preparation.piSender intent) (Preparation.piIdentifier intent) identifier (cfgPerpsChainId cfg) (T.toLower $ cfgPerpsOrderRouter cfg)
          recoveryBound <- case ngsPreparationFence gatewayState of
            Nothing -> pure False
            Just fence -> db $ \conn -> RecoveryDb.bindDeployment conn fence clientKey
          unless (bound && recoveryBound) $ throwE $ Legacy.unavailable "PREPARATION_LEASE_LOST" "Retry the same preparation ID"
          operationObject <- case stored of
            Just (Object operation) -> do
              estimate <- ioStage timing "resume_estimation" $ buildPreparedOperation timing nativeCfg client manager intent
              unless (KM.lookup "nonce" estimate == KM.lookup "nonce" operation) $
                throwE $ (Legacy.policyDenied "The account nonce changed; review account activity") {Legacy.pfReason = "PREPARATION_UNUSABLE"}
              let quantityIn fields key = KM.lookup key fields >>= \case
                    String value -> parseRpcQuantity value
                    _ -> Nothing
                  fits key = case (quantityIn estimate key, quantityIn operation key) of
                    (Just needed, Just prepared) -> needed <= prepared
                    _ -> False
              unless (all fits ["callGasLimit","verificationGasLimit","preVerificationGas"]) $
                throwE $ (Legacy.policyDenied "The batch now requires more gas than the prepared limits") {Legacy.pfReason = "PREPARATION_UNUSABLE"}
              pure operation
            Just _ -> throwE databaseUnavailable
            Nothing -> do
              unless (naaPreparationEnabled nativeCfg) $ throwE $ Legacy.unavailable "PREPARATION_DISABLED" "New preparation is disabled"
              built <- ioStage timing "fees_nonce_estimation" $ buildPreparedOperation timing nativeCfg client manager intent
              saved <- db $ \conn -> PreparationDb.savePreparedOperation conn clientKey
                (Preparation.piSender intent) (Preparation.piIdentifier intent) identifier (Object built)
              unless saved $ throwE $ Legacy.unavailable "PREPARATION_LEASE_LOST" "Retry the same preparation ID"
              pure built
          unless (Preparation.matchesIntent intent operationObject) $ throwE databaseUnavailable
          operation <- checked $ firstInvalidParams $ Paymaster.parsePackedUserOperation operationObject
          when (isJust stored) $ ioStage timing "resume_fee_validation" $ validateLiveFeeCapDual context operation
          -- Validate the exact estimated payload again, not just the skeleton.
          finalPolicyRequest <- checked $ Preparation.internalRequest "eth_estimateUserOperationGas"
            [Object $ KM.insert "signature" (String Legacy.dummySignature) operationObject, String nativeEntryPoint]
          _ <- checked $ Legacy.validateMethodParams finalPolicyRequest
          pure (context, owner, operation, assistance)
        case result of
          Left failure -> do
            _ <- liftDb $ withDb pool $ \conn -> PreparationDb.releasePreparation conn clientKey
              (Preparation.piSender intent) (Preparation.piIdentifier intent) identifier
            Legacy.respondFailure requestId failure
          Right (context, owner, operation, assistance) ->
            deliverPreparation assistance gatewayState (if Preparation.piResumeOnly intent then nativeCfg { naaPreparationEnabled = False } else nativeCfg) pool context clientKey owner request operation $ \envelope -> do
              let finalOperation = Paymaster.applyPaymasterEnvelope operation envelope
              linked <- liftDb $ withDb pool $ \conn -> PreparationDb.linkPreparationDiagnostic conn clientKey
                (Preparation.piSender intent) (Preparation.piIdentifier intent) identifier
                (encodeHex $ Paymaster.sponsorshipDigest operation envelope)
                (ngsAttemptId gatewayState) (cfgPerpsChainId cfg) (T.toLower $ cfgPerpsOrderRouter cfg)
              _ <- liftDb $ withDb pool $ \conn -> PreparationDb.releasePreparation conn clientKey
                (Preparation.piSender intent) (Preparation.piIdentifier intent) identifier
              case linked of
                Right True -> do
                  case ngsAttemptId gatewayState of
                    Nothing -> pure ()
                    Just attempt -> liftIO $ do
                      sink <- modifyMVar (ngsDiagnostics gatewayState) $ \current -> case current of
                        Just queue -> pure (current, queue)
                        Nothing -> do
                          queue <- Diagnostics.startDiagnostics cfg pool client
                          pure (Just queue, queue)
                      Diagnostics.enqueueDiagnostic sink $ Diagnostics.Diagnostic attempt clientKey
                        (cfgPerpsChainId cfg) (T.toLower $ cfgPerpsOrderRouter cfg)
                        (Preparation.piIdentifier intent) (encodeHex $ Paymaster.userOperationHash finalOperation) (T.toLower $ Preparation.piSender intent)
                  respondSuccess requestId $ object
                    [ "version" .= (1 :: Int)
                    , "entryPoint" .= nativeEntryPoint
                    , "operation" .= Object (KM.delete "signature" $ Paymaster.puoObject finalOperation)
                    , "userOperationHash" .= encodeHex (Paymaster.userOperationHash finalOperation)
                    ]
                _ -> Legacy.respondFailure requestId $ Legacy.unavailable "PREPARATION_LEASE_LOST" "Retry the same preparation ID"
        -- Errors after reservation also release the work lease, not the budget.
        -- Lost/aborted requests leave an expiring lease for another instance.
        _ <- liftDb $ withDb pool $ \conn -> PreparationDb.releasePreparation conn clientKey
          (Preparation.piSender intent) (Preparation.piIdentifier intent) identifier
        pure ()
 where
  requestId = Legacy.rrId request
  checked = ExceptT . pure
  db action = ExceptT $ fmap (either (const $ Left databaseUnavailable) Right) $ liftDb $ withDb pool action
  ioStage timing stage action = ExceptT $ liftIO $ timed timing stage action
  consumeAaRateLimitFor intent conn = consumeAaRateLimit conn "account" nativeAccountRateClientKey
    (pseudonymousAccountKey (naaProxyOriginToken nativeCfg) $ Preparation.piSender intent)
    (naaAccountRateLimitPerMinute nativeCfg)

emitTiming :: Timing -> ActionM ()
emitTiming timing = do
  (identifier, stages) <- liftIO $ timingHeaders timing
  setHeader "X-Plether-Request-Id" $ TL.fromStrict identifier
  setHeader "Server-Timing" $ TL.fromStrict stages

-- Rollback disables new reservations but permits delivery of an already signed
-- authorization, subject to the same pause, expiry and canonical-state checks.
deliverPreparation :: Maybe Legacy.CloseAssistanceIntent -> NativeGatewayState -> NativeAaConfig -> DbPool -> NativeSecurityContext
  -> Text -> Text -> Legacy.RpcRequest -> Paymaster.PackedUserOperation
  -> (Paymaster.SponsorshipEnvelope -> ActionM ()) -> ActionM ()
deliverPreparation assistance state cfg pool context clientKey owner request operation deliver
  | naaPreparationEnabled cfg = issueSponsorship assistance state cfg pool (Just context) clientKey owner request operation deliver
  | otherwise = do
      now <- liftEpochSeconds
      existing <- liftDb $ withDb pool $ \conn -> getSponsorshipByRequestKey conn (sponsorshipRequestKey cfg clientKey owner operation)
      case (ngsSigner state, existing) of
        (Just signer, Right (Just authorization))
          | authorizationIsUsable now authorization, Just _ <- saSignature authorization ->
              finishSponsorship signer cfg pool context (Legacy.rrId request) operation authorization deliver
        _ -> Legacy.respondFailure (Legacy.rrId request) $ Legacy.unavailable "PREPARATION_DISABLED" "No signed preparation is available"

preparationProfileFingerprint :: NativeAaConfig -> Text
preparationProfileFingerprint cfg = T.intercalate ":"
  [naaPaymasterAddress cfg, naaPaymasterCodeHash cfg, naaPolicyId cfg,
   naaSignerAddress cfg, naaAccountCodeHash cfg, aaRpcModeText $ naaRpcMode cfg,
   T.pack $ show (naaVerificationGasLimit cfg, naaPostOpGasLimit cfg, naaMaxCostWei cfg, naaValiditySeconds cfg),
   Preparation.gasPolicyVersion]

timeContext :: MonadIO m => NativeSecurityContext -> Text -> m a -> m a
timeContext context stage action = maybe action (\timing -> timed timing stage action) $ nscTiming context

timeGateway :: MonadIO m => NativeGatewayState -> Text -> m a -> m a
timeGateway state stage action = maybe action (\timing -> timed timing stage action) $ ngsTiming state

buildPreparedOperation
  :: Timing -> NativeAaConfig -> EthClient -> Manager -> Preparation.PreparationIntent
  -> IO (Either Legacy.ProxyFailure (KM.KeyMap Value))
buildPreparedOperation timing cfg client manager intent = runExceptT $ do
  (feesResult, nonceResult) <- liftIO $ concurrently
    (altoTimed "fees" "pimlico_getUserOperationGasPrice" [])
    (timed timing "nonce" $ rpcCall client "eth_call" $ toJSON
      [ object ["to" .= nativeEntryPoint, "data" .= encodeHex (encodeCall "getNonce(address,uint192)" [encodeAddress $ Preparation.piSender intent, encodeUint256 0])]
      , String "latest"])
  fees <- ExceptT $ pure feesResult
  (maxFee, priority) <- case fees of
    Object tiers | Just (Object fast) <- KM.lookup "fast" tiers ->
      (,) <$> quantity fast "maxFeePerGas" <*> quantity fast "maxPriorityFeePerGas"
    _ -> throwE $ Legacy.unavailable "BUNDLER_UNAVAILABLE" "Alto returned invalid fees"
  nonce <- case nonceResult of
    Right (String word) | Just bytes <- decodeFixedHex 32 word -> pure $ decodeUint256 bytes
    _ -> throwE securityAttestationUnavailable
  unless (nonce < 2^(64 :: Int)) $ throwE $ Legacy.invalidParams "Unsupported nonce lane"
  let skeleton = foldr (uncurry KM.insert) (Preparation.unsignedSkeleton intent)
        [("nonce",String $ Paymaster.canonicalQuantity nonce), ("maxFeePerGas",String $ Paymaster.canonicalQuantity maxFee), ("maxPriorityFeePerGas",String $ Paymaster.canonicalQuantity priority)]
  packed <- ExceptT $ pure $ firstInvalidParams $ Paymaster.parsePackedUserOperation skeleton
  now <- liftIO $ floor <$> getPOSIXTime
  let stub = Paymaster.makeSponsorshipEnvelope cfg (max 0 $ now-30) (now+naaValiditySeconds cfg) (naaMaxCostWei cfg) Paymaster.dummyPaymasterSignature
      estimateObject = foldr KM.delete (Paymaster.puoObject $ Paymaster.applyPaymasterEnvelope packed stub)
        ["callGasLimit", "verificationGasLimit", "preVerificationGas"]
  estimates <- ExceptT $ altoTimed "estimation" "eth_estimateUserOperationGas" [Object estimateObject, String nativeEntryPoint]
  gas <- case estimates of
    Object values -> do
      estimated <- quantity values "callGasLimit"
      padded <- case Preparation.executionGasWithHeadroom estimated of
        Right value -> pure value
        Left reason | estimated > 2_000_000 -> throwE $
          (Legacy.policyDenied reason) {Legacy.pfReason = "EXECUTION_GAS_CAP_EXCEEDED"}
        Left reason -> throwE $ Legacy.invalidParams reason
      rest <- traverse (\name -> (name,) . String . Paymaster.canonicalQuantity <$> quantity values name)
        ["verificationGasLimit","preVerificationGas"]
      liftIO $ logInfo "aa_preparation_gas_headroom" "Applied bounded execution gas headroom"
        [field "request_id" $ timingIdentifier timing,
         field "estimated_call_gas" estimated, field "prepared_call_gas" padded,
         field "gas_headroom_bps" $ (padded - estimated) * 10_000 `div` estimated]
      -- Alto v1.2.7 sizes execution PVG with maximum-value fixed-width gas
      -- words; retain its Arbitrum data-fee estimate rather than re-estimating.
      pure $ ("callGasLimit", String $ Paymaster.canonicalQuantity padded) : rest
    _ -> throwE $ Legacy.unavailable "BUNDLER_UNAVAILABLE" "Alto returned invalid gas estimates"
  let finalObject = KM.delete "signature" $ foldr (uncurry KM.insert) skeleton $
        gas ++ [("paymasterVerificationGasLimit",String $ Paymaster.canonicalQuantity $ naaVerificationGasLimit cfg),
                ("paymasterPostOpGasLimit",String $ Paymaster.canonicalQuantity $ naaPostOpGasLimit cfg)]
  final <- ExceptT $ pure $ firstInvalidParams $ Paymaster.parsePackedUserOperation finalObject
  ExceptT $ pure $ firstInvalidParams $ validateHardEconomicCaps final
  pure finalObject
 where
  altoTimed stage method params = timed timing stage $ do
    timingCount timing "alto_calls"
    altoResult manager cfg method params
  quantity fields key = case KM.lookup key fields of
    Just (String value) | Just number <- parseRpcQuantity value, number >= 0, number < 2^(128 :: Int) -> pure number
    _ -> throwE $ Legacy.unavailable "BUNDLER_UNAVAILABLE" "Alto returned an invalid quantity"

altoResult :: Manager -> NativeAaConfig -> Text -> [Value] -> IO (Either Legacy.ProxyFailure Value)
altoResult manager cfg method params = case Preparation.internalRequest method params of
  Left failure -> pure $ Left failure
  Right request -> do
    response <- forwardAlto manager (naaAltoRpcUrl cfg) request
    pure $ case response of
      Right (Object fields, _) | Just result <- KM.lookup "result" fields -> Right result
      Left failure -> Left failure
      _ -> Left $ Legacy.unavailable "BUNDLER_UNAVAILABLE" "Alto preparation failed"

dispatchNative
  :: NativeGatewayState
  -> Config
  -> NativeAaConfig
  -> DbPool
  -> EthClient
  -> Manager
  -> Text
  -> Text
  -> Legacy.RpcRequest
  -> ActionM ()
dispatchNative gatewayState cfg nativeCfg pool perpsClient manager _trustedIp clientKey request
  | Legacy.rrMethod request `elem` [Legacy.GetRecoveryChallenge, Legacy.VerifyRecoveryChallenge, Legacy.GetRecoveryStatus, Legacy.RetirePreparation] =
      handlePreparationRecovery gatewayState cfg nativeCfg pool perpsClient manager clientKey request
  | Legacy.rrMethod request == Legacy.GetPreparationStatus =
      preparationStatus nativeCfg pool perpsClient manager clientKey request
  | Legacy.rrMethod request == Legacy.PrepareUserOperation =
      prepareNativeOperation gatewayState cfg nativeCfg pool perpsClient manager clientKey request
  | otherwise =
  case validateNativeParams request of
    Left failure -> Legacy.respondFailure (Legacy.rrId request) failure
    Right (mPolicyOperation, mPackedOperation) -> do
      recoveryClient <- authorizeRecoveryRead nativeCfg pool clientKey request
      if recoveryClient == Nothing
        then
          Legacy.respondFailure (Legacy.rrId request) $
            Legacy.ProxyFailure status403 (-32001) "Forbidden" "RECOVERY_HASH_NOT_AUTHORIZED" False
        else case (mPolicyOperation, mPackedOperation) of
          (Just policyOperation, Just packedOperation) -> do
            let accountKey =
                  pseudonymousAccountKey
                    (naaProxyOriginToken nativeCfg)
                    (Legacy.puoSender policyOperation)
            accountRate <-
              liftDb $
                withDb pool $ \conn ->
                  consumeAaRateLimit
                    conn
                    "account"
                    nativeAccountRateClientKey
                    accountKey
                    (naaAccountRateLimitPerMinute nativeCfg)
            case accountRate of
              Left _ -> Legacy.respondFailure (Legacy.rrId request) databaseUnavailable
              Right False -> Legacy.respondFailure (Legacy.rrId request) Legacy.rateLimited
              Right True -> do
                securityContext <-
                  if requiresDualSecurity request || length (Legacy.puoCalls policyOperation) == 5
                    then timeGateway gatewayState "security" $ liftIO $ nativeSecurityContext nativeCfg gatewayState perpsClient
                    else pure $ Right Nothing
                case securityContext of
                  Left _ -> respondSecurityAttestationFailure (Legacy.rrId request) "initial security context"
                  Right mSecurityContext -> do
                    identity <- timeGateway gatewayState "identity" $ liftIO $
                      maybe
                        (Legacy.verifyAccountIdentity perpsClient policyOperation)
                        (\context -> verifyAccountIdentityDual context policyOperation)
                        mSecurityContext
                    case identity of
                      Left failure ->
                        respondSecurityAwareFailure
                          (Legacy.rrId request)
                          mSecurityContext
                          failure
                      Right owner ->
                        case Legacy.validateNativeActionSequence (assistanceLens gatewayState)
                          cfg
                          (Legacy.puoSender policyOperation)
                          owner
                          (Legacy.puoCalls policyOperation) of
                          Left failure -> Legacy.respondFailure (Legacy.rrId request) failure
                          Right assistance
                            | (case assistance of
                                Nothing -> isCanaryGated nativeCfg request owner
                                Just _ -> Legacy.rrMethod request `elem` [Legacy.GetPaymasterStubData, Legacy.GetPaymasterData]
                                  && not (assistanceGlobal gatewayState assistance || T.toLower owner `elem` naaCanaryOwners nativeCfg)) ->
                                Legacy.respondFailure (Legacy.rrId request) $
                                  Legacy.policyDenied "Trading Account owner is not enabled for the native AA canary"
                            | otherwise -> do
                                runtimeTrusted <- timeGateway gatewayState "runtime" $ liftIO $
                                  maybe
                                    (verifyNativeAccountRuntime nativeCfg perpsClient policyOperation)
                                    (\context -> verifyNativeAccountRuntimeDual nativeCfg context policyOperation)
                                    mSecurityContext
                                case runtimeTrusted of
                                  Left failure ->
                                    respondSecurityAwareFailure
                                      (Legacy.rrId request)
                                      mSecurityContext
                                      failure
                                  Right () ->
                                    handleOperation
                                      assistance
                                      gatewayState
                                      nativeCfg
                                      pool
                                      manager
                                      mSecurityContext
                                      clientKey
                                      owner
                                      request
                                      packedOperation
          _ -> handleOperationless gatewayState perpsClient nativeCfg pool manager (maybe clientKey id recoveryClient) request

handleOperation
  :: Maybe Legacy.CloseAssistanceIntent
  -> NativeGatewayState
  -> NativeAaConfig
  -> DbPool
  -> Manager
  -> Maybe NativeSecurityContext
  -> Text
  -> Text
  -> Legacy.RpcRequest
  -> Paymaster.PackedUserOperation
  -> ActionM ()
handleOperation assistance gatewayState nativeCfg pool manager securityContext clientKey owner request operation = do
  -- Sending an already-authorized operation must remain recoverable after the
  -- intent commits or issuance is disabled. Submission verifies its durable
  -- authorization; the atomic lens guard enforces eligibility when it executes.
  eligible <- if Legacy.rrMethod request == Legacy.SendUserOperation then pure $ Right ()
    else if isJust assistance && Legacy.rrMethod request `elem` [Legacy.GetPaymasterStubData, Legacy.GetPaymasterData]
    && not (maybe False cacEnabled $ ngsCloseAssistance gatewayState)
    then pure $ Left paymasterPaused
    else liftIO $ validateCloseAssistanceDual gatewayState securityContext (Paymaster.puoSender operation) assistance
  case eligible of
    Left failure -> Legacy.respondFailure requestId failure
    Right () -> dispatch
 where
  requestId = Legacy.rrId request
  dispatch = case Legacy.rrMethod request of
    Legacy.GetPaymasterStubData ->
      if not (naaSponsorshipEnabled nativeCfg)
        then Legacy.respondFailure requestId paymasterPaused
        else do
          now <- liftEpochSeconds
          pauseReason <- liftDb $ withDb pool getAaIssuancePause
          case pauseReason of
            Left _ -> respondNativeDbFailure requestId (Legacy.rrMethod request) "circuit-read"
            Right (Just _) -> Legacy.respondFailure requestId paymasterPaused
            Right Nothing -> do
              case securityContext of
                Nothing -> respondSecurityAttestationFailure requestId "stub security context missing"
                Just context -> do
                  snapshotStillCanonical <- liftIO $ revalidateSecurityContext context
                  case snapshotStillCanonical of
                    Left reason -> respondSecurityAttestationFailure requestId reason
                    Right () -> do
                      let envelope =
                            Paymaster.makeSponsorshipEnvelope
                              nativeCfg
                              (max 0 $ now - 30)
                              (maybe (now + naaValiditySeconds nativeCfg) (min (now + naaValiditySeconds nativeCfg) . Legacy.caiValidUntil) assistance)
                              (naaMaxCostWei nativeCfg)
                              Paymaster.dummyPaymasterSignature
                      respondSuccess requestId $ paymasterResponse False envelope
    Legacy.GetPaymasterData ->
      issueSponsorship assistance gatewayState nativeCfg pool securityContext clientKey owner request operation
        (respondSuccess requestId . paymasterResponse True)
    Legacy.SendUserOperation ->
      submitSponsoredOperation nativeCfg pool manager securityContext clientKey request operation
    _ -> relayToAlto nativeCfg manager request Nothing

issueSponsorship
  :: Maybe Legacy.CloseAssistanceIntent
  -> NativeGatewayState
  -> NativeAaConfig
  -> DbPool
  -> Maybe NativeSecurityContext
  -> Text
  -> Text
  -> Legacy.RpcRequest
  -> Paymaster.PackedUserOperation
  -> (Paymaster.SponsorshipEnvelope -> ActionM ())
  -> ActionM ()
issueSponsorship assistance gatewayState nativeCfg pool securityContext clientKey owner request operation deliver
  | not (naaSponsorshipEnabled nativeCfg) =
      Legacy.respondFailure requestId paymasterPaused
  | Just _ <- ngsIssuanceError gatewayState =
      Legacy.respondFailure requestId nativeStartupFailure
  | Paymaster.puoPaymasterVerificationGasLimit operation /= Just (naaVerificationGasLimit nativeCfg)
      || Paymaster.puoPaymasterPostOpGasLimit operation /= Just (naaPostOpGasLimit nativeCfg) =
      Legacy.respondFailure requestId $
        Legacy.invalidParams "final paymaster request must preserve the issued paymaster gas limits"
  | Left reason <- validateHardEconomicCaps operation =
      Legacy.respondFailure requestId $ Legacy.policyDenied reason
  | otherwise =
      case (ngsSigner gatewayState, securityContext) of
        (Nothing, _) ->
          Legacy.respondFailure requestId $
            Legacy.unavailable "SIGNER_UNAVAILABLE" "Paymaster signer is unavailable"
        (_, Nothing) ->
          Legacy.respondFailure requestId $
            Legacy.unavailable "SECURITY_ATTESTATION_UNAVAILABLE" "Independent security RPC attestation is unavailable"
        (Just signer, Just context) -> do
          feeAttestation <- liftIO $ validateLiveFeeCapDual context operation
          case feeAttestation of
            Left failure -> Legacy.respondFailure requestId failure
            Right () -> do
              pauseReason <- liftDb $ withDb pool getAaIssuancePause
              case pauseReason of
                Left _ -> respondNativeDbFailure requestId (Legacy.rrMethod request) "circuit-read"
                Right (Just _) -> Legacy.respondFailure requestId paymasterPaused
                Right Nothing -> do
                  -- This key intentionally excludes the timestamp-derived validity
                  -- window. A retry therefore resolves the original durable row
                  -- before it can reserve a second liability.
                  let requestKey = sponsorshipRequestKey nativeCfg clientKey owner operation
                  existing <-
                    liftDb $
                      withDb pool $ \conn ->
                        getSponsorshipByRequestKey conn requestKey
                  case existing of
                    Left _ -> respondNativeDbFailure requestId (Legacy.rrMethod request) "idempotency-read"
                    Right (Just authorization) -> do
                      now <- liftEpochSeconds
                      if authorizationIsUsable now authorization
                        then finishSponsorship signer nativeCfg pool context requestId operation authorization deliver
                        else
                          Legacy.respondFailure requestId $
                            Legacy.policyDenied "This exact sponsorship request has already expired or completed"
                    Right Nothing -> reserveNew signer context requestKey
 where
  requestId = Legacy.rrId request
  reserveNew signer context requestKey = do
        now <- liftEpochSeconds
        let validAfter = max 0 $ now - 30
            validUntil = maybe (now + naaValiditySeconds nativeCfg) (min (now + naaValiditySeconds nativeCfg) . Legacy.caiValidUntil) assistance
            provisional =
              Paymaster.makeSponsorshipEnvelope
                nativeCfg validAfter validUntil (naaMaxCostWei nativeCfg) BS.empty
            maxCost = Paymaster.maximumUserOperationCost operation provisional
        if maxCost <= 0 || maxCost > naaMaxCostWei nativeCfg
          then
            Legacy.respondFailure requestId $
              Legacy.policyDenied "UserOperation maximum gas liability exceeds the sponsorship ceiling"
          else do
            let unsignedEnvelope =
                  Paymaster.makeSponsorshipEnvelope
                    nativeCfg validAfter validUntil maxCost BS.empty
                digestText = encodeHex $ Paymaster.sponsorshipDigest operation unsignedEnvelope
                draft =
                  SponsorshipDraft
                    { sdRequestKey = requestKey
                    , sdDigest = digestText
                    , sdSender = Paymaster.puoSender operation
                    , sdOwner = owner
                    , sdNonce = Paymaster.puoNonce operation
                    , sdValidAfter = validAfter
                    , sdValidUntil = validUntil
                    , sdMaxCostWei = maxCost
                    , sdClientKey = clientKey
                    , sdOperation = Object $ Paymaster.puoObject operation
                    }
            snapshotReady <- timeContext context "canonical_before_reservation" $ liftIO $ revalidateSecurityContext context
            case snapshotReady of
              Left reason -> respondSecurityAttestationFailure requestId reason
              Right () -> do
                reserved <- timeContext context "reservation_and_lock" $ liftDb $ withDb pool $ \conn -> reserveSponsorshipFenced conn nativeCfg draft (fmap (assistanceReservation (Paymaster.puoSender operation)) assistance) (nscPreparationFence context)
                case reserved of
                  Left _ -> respondNativeDbFailure requestId (Legacy.rrMethod request) "reservation"
                  Right (Left "PAYMASTER_PAUSED") -> Legacy.respondFailure requestId paymasterPaused
                  Right (Left "RECONCILER_STALE") ->
                    do
                      liftIO $
                        logErrorEvery
                          30
                          "aa_native_reconciler_stale"
                          "Native sponsorship was denied because reconciliation is stale"
                          [field "method" $ show $ Legacy.rrMethod request]
                      Legacy.respondFailure requestId $
                        Legacy.unavailable "RECONCILER_STALE" "Sponsorship reconciliation is not fresh"
                  Right (Left "SPONSORSHIP_RETRY_EXPIRED") ->
                    Legacy.respondFailure requestId $
                      Legacy.policyDenied "This exact sponsorship request has already expired or completed"
                  Right (Left reason) ->
                    Legacy.respondFailure requestId $
                      Legacy.ProxyFailure status200 (-32005) "Sponsorship budget exceeded" reason True
                  Right (Right authorization) ->
                    finishSponsorship signer nativeCfg pool context requestId operation authorization deliver

finishSponsorship
  :: PaymasterSigner
  -> NativeAaConfig
  -> DbPool
  -> NativeSecurityContext
  -> Value
  -> Paymaster.PackedUserOperation
  -> SponsorshipAuthorization
  -> (Paymaster.SponsorshipEnvelope -> ActionM ())
  -> ActionM ()
finishSponsorship signer nativeCfg pool securityContext requestId operation authorization deliver = do
  snapshotStillCanonical <- timeContext securityContext "canonical_before_delivery" $ liftIO $ revalidateSecurityContext securityContext
  case snapshotStillCanonical of
    Left reason -> respondSecurityAttestationFailure requestId reason
    Right () -> do
      deliveryAllowed <-
        liftDb $
          withDb pool $ \conn ->
            isSponsorshipDeliveryAllowedFenced conn nativeCfg (saDigest authorization) (nscPreparationFence securityContext)
      case deliveryAllowed of
        Left _ -> respondNativeDbFailure requestId Legacy.GetPaymasterData "delivery-authorization"
        Right False -> Legacy.respondFailure requestId paymasterPaused
        Right True -> finishVerified
 where
  unsignedEnvelope = envelopeFromAuthorization nativeCfg authorization BS.empty
  canonicalDigest = encodeHex $ Paymaster.sponsorshipDigest operation unsignedEnvelope

  finishVerified
    | canonicalDigest /= saDigest authorization =
        Legacy.respondFailure requestId databaseUnavailable
    | otherwise =
        case saSignature authorization of
          Just storedSignature ->
            case decodeFixedHex 65 storedSignature of
              Nothing -> Legacy.respondFailure requestId databaseUnavailable
              Just signature -> do
                let finalEnvelope = envelopeFromAuthorization nativeCfg authorization signature
                    expectedHash = encodeHex $
                      Paymaster.userOperationHash $
                        Paymaster.applyPaymasterEnvelope operation finalEnvelope
                if saExpectedUserOperationHash authorization /= Just expectedHash
                  then Legacy.respondFailure requestId databaseUnavailable
                  else do
                    issueRecoveryCapability nativeCfg expectedHash (saClientKey authorization)
                    deliver finalEnvelope
          Nothing -> do
            signatureResult <- timeContext securityContext "kms_sign" $ liftIO $ psSignDigest signer $ Paymaster.sponsorshipDigest operation unsignedEnvelope
            case signatureResult of
              Left _ -> do
                liftIO $
                  logError
                    "aa_native_signer_failure"
                    "AWS KMS could not sign a native sponsorship digest"
                    [field "method" ("pm_getPaymasterData" :: Text)]
                Legacy.respondFailure requestId $
                  Legacy.unavailable "SIGNER_UNAVAILABLE" "AWS KMS could not sign the sponsorship"
              Right signature -> do
                snapshotAfterSigning <- timeContext securityContext "canonical_after_signing" $ liftIO $ revalidateSecurityContext securityContext
                case snapshotAfterSigning of
                  Left reason -> respondSecurityAttestationFailure requestId reason
                  Right () -> do
                    let finalEnvelope = envelopeFromAuthorization nativeCfg authorization signature
                        signedOperation = Paymaster.applyPaymasterEnvelope operation finalEnvelope
                        expectedHash = encodeHex $ Paymaster.userOperationHash signedOperation
                        signatureText = encodeHex signature
                    stored <-
                      timeContext securityContext "signature_store_and_lock" $ liftDb $
                        withDb pool $ \conn ->
                          storeSponsorshipSignatureFenced
                            conn
                            nativeCfg
                            (saDigest authorization)
                            signatureText
                            expectedHash
                            (nscPreparationFence securityContext)
                    case stored of
                      Left _ -> respondNativeDbFailure requestId Legacy.GetPaymasterData "signature-store"
                      Right False -> respondNativeDbFailure requestId Legacy.GetPaymasterData "signature-store-rejected"
                      Right True -> do
                        canonical <-
                          liftDb $
                            withDb pool $ \conn ->
                              getSponsorshipByDigest conn (saDigest authorization)
                        case canonical of
                          Right (Just saved)
                            | Just _ <- saSignature saved ->
                                finishSponsorship signer nativeCfg pool securityContext requestId operation saved deliver
                          _ -> respondNativeDbFailure requestId Legacy.GetPaymasterData "signature-readback"

submitSponsoredOperation
  :: NativeAaConfig
  -> DbPool
  -> Manager
  -> Maybe NativeSecurityContext
  -> Text
  -> Legacy.RpcRequest
  -> Paymaster.PackedUserOperation
  -> ActionM ()
submitSponsoredOperation nativeCfg pool manager securityContext clientKey request operation
  | not (naaSubmissionEnabled nativeCfg) =
      Legacy.respondFailure requestId $
        Legacy.unavailable "SUBMISSION_PAUSED" "Native UserOperation submission is disabled"
  | BS.length (Paymaster.puoSignature operation) /= 65 =
      Legacy.respondFailure requestId $
        Legacy.invalidParams "submitted UserOperation must have a real 65-byte account signature"
  | Nothing <- securityContext =
      Legacy.respondFailure requestId $
        Legacy.unavailable "SECURITY_ATTESTATION_UNAVAILABLE" "Independent security RPC attestation is unavailable"
  | otherwise =
      case Paymaster.decodeSponsorshipEnvelope nativeCfg operation of
        Left message -> Legacy.respondFailure requestId $ Legacy.policyDenied message
        Right envelope
          | Paymaster.seSignature envelope == Paymaster.dummyPaymasterSignature ->
              Legacy.respondFailure requestId $ Legacy.policyDenied "dummy paymaster signatures cannot be submitted"
          | otherwise -> do
              securityVerified <-
                liftIO $
                  maybe
                    (pure $ Left "security context missing")
                    revalidateSecurityContext
                    securityContext
              case securityVerified of
                Left reason -> respondSecurityAttestationFailure requestId reason
                Right () -> submitVerified envelope
 where
  submitVerified envelope = do
              let digest = encodeHex $ Paymaster.sponsorshipDigest operation envelope
                  operationHash = encodeHex $ Paymaster.userOperationHash operation
                  signatureText = encodeHex $ Paymaster.seSignature envelope
              token <- header "X-Plether-AA-Preparation-Recovery"
              recovered <- case TL.toStrict <$> token of
                Just value | T.length value == 64 -> liftDb $ withDb pool $ \conn -> RecoveryDb.sessionSubmissionClient conn (T.toLower $ naaPaymasterAddress nativeCfg) (Recovery.tokenHash value) operationHash
                _ -> pure $ Right Nothing
              let recoveredClient = either (const Nothing) id recovered
              stored <- liftDb $ withDb pool $ \conn -> getSponsorshipByDigest conn digest
              case stored of
                Left _ -> respondNativeDbFailure requestId Legacy.SendUserOperation "authorization-read"
                Right Nothing ->
                  Legacy.respondFailure requestId $
                    Legacy.ProxyFailure status403 (-32001) "Forbidden" "SPONSORSHIP_NOT_AUTHORIZED" False
                Right (Just authorization)
                  | (saClientKey authorization /= T.toLower clientKey && Just (saClientKey authorization) /= recoveredClient)
                      || saExpectedUserOperationHash authorization /= Just operationHash
                      || saSignature authorization /= Just signatureText
                      || saState authorization `notElem` ["signed", "submitted"] ->
                      Legacy.respondFailure requestId $
                        Legacy.ProxyFailure status403 (-32001) "Forbidden" "SPONSORSHIP_NOT_AUTHORIZED" False
                  | otherwise -> do
                      marked <-
                        liftDb $
                          withDb pool $ \conn ->
                            markSponsorshipSubmitted conn digest operationHash (saClientKey authorization)
                      case marked of
                        Left _ -> respondNativeDbFailure requestId Legacy.SendUserOperation "submission-journal"
                        Right False -> respondNativeDbFailure requestId Legacy.SendUserOperation "submission-journal-rejected"
                        Right True -> do
                          finalSecurityCheck <-
                            liftIO $
                              maybe
                                (pure $ Left "security context missing")
                                revalidateSecurityContext
                                securityContext
                          case finalSecurityCheck of
                            Left reason -> respondSecurityAttestationFailure requestId reason
                            Right () -> relayToAlto nativeCfg manager request $ Just operationHash

  requestId = Legacy.rrId request

handleOperationless
  :: NativeGatewayState
  -> EthClient
  -> NativeAaConfig
  -> DbPool
  -> Manager
  -> Text
  -> Legacy.RpcRequest
  -> ActionM ()
handleOperationless gatewayState primary nativeCfg pool manager clientKey request
  | Legacy.rrMethod request == Legacy.GetUserOperationReceipt
  , String operationHash : _ <- Legacy.rrParams request = do
      upstream <- liftIO $ forwardAlto manager (naaAltoRpcUrl nativeCfg) request
      let forward = case upstream of
            Left failure -> Legacy.respondFailure requestId failure
            Right (value, retryAfter) -> do
              setHeader "Cache-Control" "no-store"
              maybe (pure ()) (setHeader "Retry-After" . TL.fromStrict) retryAfter
              status status200
              json value
      if not (RecoveryReceipt.needsReceiptFallback upstream) then forward else do
        located <- liftDb $ withDb pool $ \conn -> getRecoveryReceiptLocator conn operationHash clientKey
        case located of
          Left _ -> respondNativeDbFailure requestId (Legacy.rrMethod request) "recovery-receipt-locator"
          Right Nothing -> do
            authorization <- liftDb $ withDb pool $ \conn -> getSponsorshipByUserOperationHash conn operationHash
            case authorization of
              Left _ -> respondNativeDbFailure requestId (Legacy.rrMethod request) "recovery-receipt-authorization"
              Right (Just saved) | saClientKey saved == clientKey && saState saved /= "expired" ->
                respondRecoveryPending requestId
              _ -> forward
          Right (Just locator) -> do
            let recover client = RecoveryReceipt.recoverReceipt client 421614
                  (naaPaymasterAddress nativeCfg) operationHash locator
                secondary = case ngsSecurityClient gatewayState of
                  Just client -> recover client
                  Nothing -> pure $ Left "RECOVERY_PROVIDER_UNAVAILABLE"
            verified <- liftIO $ timeout 5_000_000 $
              providerPair (naaRpcMode nativeCfg) (recover primary) secondary
            let recovered = case verified of
                  Just (Right first, Right second) -> receiptIdentity first == receiptIdentity second
                  _ -> False
            liftIO $ do
              sink <- readMVar $ ngsDiagnostics gatewayState
              mapM_ (\queue -> Diagnostics.enqueueDiagnostic queue $
                Diagnostics.RecoveryDiagnostic clientKey operationHash recovered) sink
            case verified of
              Just (Right first, Right second) | receiptIdentity first == receiptIdentity second ->
                respondSuccess requestId first
              _ -> Legacy.respondFailure requestId $
                Legacy.unavailable "RECOVERY_EVIDENCE_UNAVAILABLE" "Canonical operation receipt could not be verified; retry recovery"
  | otherwise = relayToAlto nativeCfg manager request Nothing
 where
  requestId = Legacy.rrId request
  -- Provider-specific transaction receipt extensions are not operation identity.
  receiptIdentity (Object fields) = Object $ KM.mapWithKey normalizeField $ KM.delete "receipt" fields
  receiptIdentity value = value
  normalizeField "logs" (Array values) = Array $ fmap normalizeLog values
  normalizeField _ value = normalizeHex value
  normalizeLog (Object fields) = Object $ KM.map normalizeHex $ KM.filterWithKey
    (\key _ -> key `elem` ["address","topics","data","blockHash","blockNumber","transactionHash","logIndex"]) fields
  normalizeLog value = value
  normalizeHex (String value) = String $ T.toLower value
  normalizeHex (Array values) = Array $ fmap normalizeHex values
  normalizeHex value = value

requiresDualSecurity :: Legacy.RpcRequest -> Bool
requiresDualSecurity request =
  Legacy.rrMethod request
    `elem` [ Legacy.GetPaymasterStubData
           , Legacy.GetPaymasterData
           , Legacy.SendUserOperation
           ]

-- Never infer a provider mode from matching URLs; Config validates the mode.
providerPair :: AaRpcMode -> IO a -> IO a -> IO (a, a)
providerPair SingleProviderSepolia primary _ = do
  value <- primary
  pure (value, value)
providerPair DualIndependent primary secondary = concurrently primary secondary

nativeSecurityContext
  :: NativeAaConfig
  -> NativeGatewayState
  -> EthClient
  -> IO (Either Legacy.ProxyFailure (Maybe NativeSecurityContext))
nativeSecurityContext nativeCfg gatewayState primaryClient =
  case ngsSecurityClient gatewayState of
    Nothing -> pure $ Left securityAttestationUnavailable
    Just secondaryClient -> do
      -- Both are read-only evidence. No estimation, reservation or signing can
      -- start until chain identity AND the canonical snapshot have passed.
      ((primaryChain, secondaryChain), snapshot) <- concurrently
        (providerPair (naaRpcMode nativeCfg) (attestRpcChain primaryClient) (attestRpcChain secondaryClient))
        (readAgreedSecurityBlock (naaRpcMode nativeCfg) (naaMaxSafeLagSeconds nativeCfg) primaryClient secondaryClient)
      case (primaryChain, secondaryChain) of
        (Right (), Right ()) -> do
          case snapshot of
            Left _ -> pure $ Left securityAttestationUnavailable
            Right header -> do
              let blockNumber = sbhNumber header
              (trustedSnapshot, snapshots) <- modifyMVar (ngsSnapshots gatewayState) $ \previous -> do
                let (trusted, distinct) = advanceEvidenceSnapshots previous (blockNumber, sbhHash header)
                pure (distinct, (trusted, map snd distinct))
              Cache.retainSnapshots (ngsProfileEvidence gatewayState) snapshots
              Cache.retainSnapshots (ngsAccountEvidence gatewayState) snapshots
              let profile = do
                    (first, second) <- providerPair (naaRpcMode nativeCfg)
                      (attestProfileAt nativeCfg primaryClient blockNumber)
                      (attestProfileAt nativeCfg secondaryClient blockNumber)
                    pure $ first >> second
                  profileEvidence = if naaPreparationEnabled nativeCfg
                    then Cache.evidenceObserved (ngsProfileEvidence gatewayState)
                      (\event -> maybe (pure ()) (\timing -> timingCount timing $ "profile_cache_" <> event) $ ngsTiming gatewayState)
                      (sbhHash header) "profile" profile
                    else profile
              -- Permission is read afresh; only immutable block-specific profile
              -- evidence can be cached. The trailing header check brackets both.
              (profileResult, (primaryPause, secondaryPause)) <- concurrently profileEvidence $
                providerPair (naaRpcMode nativeCfg)
                  (readBoolAt primaryClient blockNumber (naaPaymasterAddress nativeCfg) "paused()" [])
                  (readBoolAt secondaryClient blockNumber (naaPaymasterAddress nativeCfg) "paused()" [])
              finalHeader <-
                readAgreedSecurityHeaderAt (naaRpcMode nativeCfg) primaryClient secondaryClient blockNumber
              case (trustedSnapshot, profileResult, primaryPause, secondaryPause, finalHeader) of
                (True, Right (), Right False, Right False, Right checkedHeader)
                  | checkedHeader == header ->
                      pure $ Right $ Just $ NativeSecurityContext primaryClient secondaryClient header (naaMaxSafeLagSeconds nativeCfg) (naaRpcMode nativeCfg) (ngsAccountEvidence gatewayState) (ngsPreparationFence gatewayState) (ngsTiming gatewayState)
                _ -> do
                  Cache.clearEvidence $ ngsProfileEvidence gatewayState
                  Cache.clearEvidence $ ngsAccountEvidence gatewayState
                  pure $ Left securityAttestationUnavailable
        _ -> pure $ Left securityAttestationUnavailable

-- Retain the two highest snapshots; reject a same-height replacement once.
advanceEvidenceSnapshots :: [(Integer, Text)] -> (Integer, Text) -> (Bool, [(Integer, Text)])
advanceEvidenceSnapshots previous current@(number, blockHash) =
  let trusted = all (\(oldNumber, oldHash) -> oldNumber <= number && (oldNumber /= number || oldHash == blockHash)) previous
      snapshots = take 2 $ sortOn (Down . fst) $ current : filter ((/= number) . fst) previous
  in (trusted, snapshots)

verifyAccountIdentityDual
  :: NativeSecurityContext
  -> Legacy.ParsedUserOperation
  -> IO (Either Legacy.ProxyFailure Text)
verifyAccountIdentityDual context operation = do
  let key = Legacy.puoSender operation <> ":" <> maybe "deployed" id (Legacy.puoFactoryOwner operation)
  timeContext context "identity_evidence" $ Cache.evidenceObserved (nscAccountEvidence context)
    (\event -> maybe (pure ()) (\timing -> timingCount timing $ "account_cache_" <> event) $ nscTiming context)
    (sbhHash $ nscHeader context) ("identity:" <> key) $
      verifyAccountIdentityUncached context operation

verifyAccountIdentityUncached :: NativeSecurityContext -> Legacy.ParsedUserOperation -> IO (Either Legacy.ProxyFailure Text)
verifyAccountIdentityUncached context operation = do
  let blockNumber = sbhNumber $ nscHeader context
  (primary, secondary) <- providerPair (nscRpcMode context)
    (Legacy.verifyAccountIdentityAtBlock (nscPrimaryClient context) blockNumber operation)
    (Legacy.verifyAccountIdentityAtBlock (nscSecondaryClient context) blockNumber operation)
  pure $ agreeAccountIdentity primary secondary

-- A pending deployment is advisory only and must agree across providers, just
-- like a definitive denial. Never let one provider's pending state hide a
-- disagreement or an unavailable proof from the other provider.
agreeAccountIdentity
  :: Either Legacy.ProxyFailure Text -> Either Legacy.ProxyFailure Text
  -> Either Legacy.ProxyFailure Text
agreeAccountIdentity primary secondary = case (primary, secondary) of
    (Right firstOwner, Right secondOwner)
      | T.toLower firstOwner == T.toLower secondOwner -> Right $ T.toLower firstOwner
      | otherwise -> Left securityAttestationUnavailable
    (Left firstFailure, Left secondFailure)
      | firstFailure == secondFailure
      , not (Legacy.pfRetryable firstFailure)
          || Legacy.pfReason firstFailure == "ACCOUNT_DEPLOYMENT_PENDING" -> Left firstFailure
      | otherwise -> Left securityAttestationUnavailable
    _ -> Left securityAttestationUnavailable

revalidateSecurityContext :: NativeSecurityContext -> IO (Either Text ())
revalidateSecurityContext context = revalidateSecuritySnapshot
  (nscRpcMode context) (nscMaxSafeLagSeconds context)
  (nscPrimaryClient context) (nscSecondaryClient context) (nscHeader context)

-- Keep every authorization-boundary check fresh. These two reads are independent
-- because the explicit block number is already captured, not derived from the
-- new safe-head response. No permission or canonical-header result is cached.
revalidateSecuritySnapshot
  :: AaRpcMode -> Integer -> EthClient -> EthClient -> SecurityBlockHeader
  -> IO (Either Text ())
revalidateSecuritySnapshot mode maxSafeLag primaryClient secondaryClient captured = do
  let blockNumber = sbhNumber captured
  ((primarySafe, secondarySafe), current) <- concurrently
    (providerPair mode (readSecurityHeader primaryClient "safe") (readSecurityHeader secondaryClient "safe"))
    (readAgreedSecurityHeaderAt mode primaryClient secondaryClient blockNumber)
  now <- floor <$> getPOSIXTime
  pure $ do
    firstSafe <- primarySafe
    secondSafe <- secondarySafe
    unless (sbhNumber firstSafe >= blockNumber && sbhNumber secondSafe >= blockNumber) $
      Left "a security provider's safe head moved behind the authorization snapshot"
    when (sbhNumber firstSafe == blockNumber && firstSafe /= captured) $
      Left "primary safe head disagrees with the authorization snapshot"
    when (sbhNumber secondSafe == blockNumber && secondSafe /= captured) $
      Left "secondary safe head disagrees with the authorization snapshot"
    header <- current
    unless (header == captured) $
      Left "the agreed security block changed during request authorization"
    validateSecurityHeaderTime maxSafeLag now header

respondSecurityAttestationFailure :: Value -> Text -> ActionM ()
respondSecurityAttestationFailure requestId _reason = do
  liftIO $
    logErrorEvery
      30
      "aa_native_security_attestation_failure"
      "Independent RPC security attestation failed closed"
      [field "method" ("native-aa" :: Text)]
  Legacy.respondFailure requestId securityAttestationUnavailable

respondSecurityAwareFailure
  :: Value
  -> Maybe NativeSecurityContext
  -> Legacy.ProxyFailure
  -> ActionM ()
respondSecurityAwareFailure requestId securityContext failure
  | Just _ <- securityContext
  , failure == securityAttestationUnavailable =
      respondSecurityAttestationFailure requestId "dual-provider account attestation"
  | otherwise = Legacy.respondFailure requestId failure

securityAttestationUnavailable :: Legacy.ProxyFailure
securityAttestationUnavailable =
  Legacy.unavailable
    "SECURITY_ATTESTATION_UNAVAILABLE"
    "Independent RPC providers could not attest the same canonical account state"

-- Never include the underlying KMS/provider error here: upstream text may
-- contain deployment identifiers such as a KMS KeyId or ARN.
nativeStartupFailure :: Legacy.ProxyFailure
nativeStartupFailure =
  Legacy.unavailable
    "SIGNER_UNAVAILABLE"
    "Native sponsorship startup attestation failed"

issueRecoveryCapability :: NativeAaConfig -> Text -> Text -> ActionM ()
issueRecoveryCapability cfg operation client = do
  now <- liftEpochSeconds
  setHeader "X-Plether-AA-Recovery" $ TL.fromStrict $
    RecoveryCapability.issue (naaProxyOriginToken cfg) (naaPaymasterAddress cfg) now operation client

-- Pending is deliberately NOT a null receipt: clients must not interpret lack
-- of finalized evidence as permission to release the lane or resubmit.
respondRecoveryPending :: Value -> ActionM ()
respondRecoveryPending requestId = do
  liftIO $ logInfo "aa_recovery_pending" "Awaiting verified recovery evidence" []
  setHeader "Cache-Control" "no-store"
  setHeader "Retry-After" "60"
  status status200
  json $ object ["jsonrpc" .= ("2.0" :: Text), "id" .= requestId,
    "error" .= object ["code" .= (-32001 :: Int), "message" .= ("Recovery is awaiting verified evidence" :: Text),
      "data" .= object ["reason" .= ("RECOVERY_PENDING" :: Text), "retryable" .= True, "retryAfter" .= (60 :: Int)]]]

authorizeRecoveryRead :: NativeAaConfig -> DbPool -> Text -> Legacy.RpcRequest -> ActionM (Maybe Text)
authorizeRecoveryRead cfg pool clientKey request =
  case Legacy.rrMethod request of
    method
      | method `elem`
          [ Legacy.GetUserOperationReceipt
          , Legacy.GetUserOperationByHash
          , Legacy.GetUserOperationStatus
          ] ->
          case Legacy.rrParams request of
            [String operationHash] -> do
              credential <- header "X-Plether-AA-Recovery"
              now <- liftEpochSeconds
              let capabilityClient = credential >>= RecoveryCapability.verify (naaProxyOriginToken cfg)
                    (naaPaymasterAddress cfg) now operationHash . TL.toStrict
                  authorizedClient = maybe clientKey id capabilityClient
              result <-
                liftDb $
                  withDb pool $ \conn ->
                    isRecoveryOperationAuthorized conn operationHash authorizedClient "alto"
              case result of
                Right True -> do
                  issueRecoveryCapability cfg operationHash authorizedClient
                  pure $ Just authorizedClient
                _ -> pure Nothing
            _ -> pure Nothing
    _ -> pure $ Just clientKey

validateNativeParams
  :: Legacy.RpcRequest
  -> Either Legacy.ProxyFailure
       (Maybe Legacy.ParsedUserOperation, Maybe Paymaster.PackedUserOperation)
validateNativeParams request =
  case Legacy.rrMethod request of
    Legacy.GetPaymasterStubData -> paymasterParams
    Legacy.GetPaymasterData -> paymasterParams
    method
      | method `elem` [Legacy.EstimateUserOperationGas, Legacy.SendUserOperation] -> do
          policyOperation <- Legacy.validateMethodParams request
          operationObject <- case Legacy.rrParams request of
            [Object operation, _] -> Right operation
            _ -> Left $ Legacy.invalidParams "UserOperation parameters are invalid"
          packed <- firstInvalidParams $ Paymaster.parsePackedUserOperation operationObject
          case method of
            Legacy.SendUserOperation -> do
              unless (BS.length (Paymaster.puoSignature packed) == 65) $
                Left $ Legacy.invalidParams "submitted UserOperation must have a 65-byte signature"
              pure ()
            _ -> pure ()
          pure (policyOperation, Just packed)
    _ -> do
      _ <- Legacy.validateMethodParams request
      pure (Nothing, Nothing)
 where
  paymasterParams =
    case Legacy.rrParams request of
      [Object operation, String entryPoint, String requestedChain, Object context]
        | normalizeAddress entryPoint == Just nativeEntryPoint
        , T.toLower requestedChain == "0x66eee"
        , KM.null context -> do
            packed <- firstInvalidParams $ Paymaster.parsePackedUserOperation operation
            unless (BS.null $ Paymaster.puoSignature packed) $
              Left $ Legacy.invalidParams "paymaster requests must omit the account signature"
            unless
              (Paymaster.puoPaymaster packed == Nothing && Paymaster.puoPaymasterData packed == Nothing)
              $ Left $ Legacy.invalidParams "paymaster requests must omit paymaster and paymasterData"
            let policyObject = KM.insert "signature" (String Legacy.dummySignature) operation
                policyRequest =
                  request
                    { Legacy.rrParams =
                        [Object policyObject, String entryPoint, String requestedChain, Object context]
                    }
            policyOperation <- Legacy.validateMethodParams policyRequest
            pure (policyOperation, Just packed)
      _ ->
        Left $
          Legacy.invalidParams
            "paymaster method requires [unsigned UserOperation, approved EntryPoint, Arbitrum Sepolia chain, empty context]"

relayToAlto
  :: NativeAaConfig
  -> Manager
  -> Legacy.RpcRequest
  -> Maybe Text
  -> ActionM ()
relayToAlto nativeCfg manager request expectedHash = do
  upstream <- liftIO $ forwardAlto manager (naaAltoRpcUrl nativeCfg) request
  case upstream of
    Left failure -> Legacy.respondFailure (Legacy.rrId request) failure
    Right (upstreamValue, retryAfter) ->
      case expectedHash of
        Nothing -> forwardResponse upstreamValue retryAfter
        Just localHash ->
          case responseOperationHash upstreamValue of
            Just upstreamHash | upstreamHash == localHash ->
              forwardResponse upstreamValue retryAfter
            Nothing | isRpcErrorResponse upstreamValue ->
              forwardResponse upstreamValue retryAfter
            returnedHash -> do
              liftIO $
                logError
                  "aa_native_bundler_hash_mismatch"
                  "Alto returned a successful result that did not match EntryPoint v0.8 hashing"
                  [ field "expected_user_operation_hash" localHash
                  , field "returned_user_operation_hash" $ fromMaybeText returnedHash
                  ]
              Legacy.respondFailure (Legacy.rrId request) $
                Legacy.unavailable "BUNDLER_HASH_MISMATCH" "Alto returned an unexpected UserOperation hash"
 where
  forwardResponse upstreamValue retryAfter = do
          setHeader "Content-Type" "application/json"
          setHeader "Cache-Control" "no-store"
          maybe (pure ()) (setHeader "Retry-After" . TL.fromStrict) retryAfter
          status status200
          json upstreamValue
  fromMaybeText = maybe "<invalid-success-result>" id

forwardAlto
  :: Manager
  -> Text
  -> Legacy.RpcRequest
  -> IO (Either Legacy.ProxyFailure (Value, Maybe Text))
forwardAlto manager rpcUrl request = do
  -- Alto 1.2.7 only accepts numeric IDs. Each HTTP call has one response, so
  -- a local numeric ID is sufficient; validate it BEFORE restoring caller ID.
  let upstreamRpc = request {Legacy.rrId = Number 1,
        Legacy.rrObject = KM.insert "id" (Number 1) $ Legacy.rrObject request}
  result <- try @HttpException $ timeout 20_000_000 $ do
    base <- parseRequest $ T.unpack rpcUrl
    let upstreamRequest =
          base
            { method = "POST"
            , requestHeaders =
                [ ("Content-Type", "application/json")
                , ("Accept", "application/json")
                ]
            , requestBody = RequestBodyLBS $ encode $ Object $ Legacy.rrObject upstreamRpc
            , responseTimeout = responseTimeoutMicro 20_000_000
            , redirectCount = 0
            , checkResponse = \_ _ -> pure ()
            }
    withResponse upstreamRequest manager $ \response -> do
      bounded <- readBoundedAltoBody maxAltoResponseBytes $ responseBody response
      pure
        ( statusCode $ responseStatus response
        , bounded
        , lookup hRetryAfter $ responseHeaders response
        )
  pure $ case result of
    Left _ -> Left $ Legacy.unavailable "BUNDLER_UNAVAILABLE" "Alto is temporarily unavailable"
    Right Nothing -> Left $ Legacy.unavailable "BUNDLER_UNAVAILABLE" "Alto request timed out"
    Right (Just (httpStatus, _, _))
      | httpStatus < 200 || httpStatus >= 300 ->
          Left $ Legacy.unavailable "BUNDLER_UNAVAILABLE" "Alto rejected the request at the HTTP layer"
    Right (Just (_, Left _, _)) ->
      Left $ Legacy.unavailable "BUNDLER_UNAVAILABLE" "Alto returned an oversized response"
    Right (Just (_, Right body, retryAfterBytes)) ->
      case eitherDecodeStrict' body of
        Left _ ->
          Left $ Legacy.unavailable "BUNDLER_UNAVAILABLE" "Alto returned an invalid response"
        Right (Object responseObject)
          | validAltoResponse upstreamRpc responseObject ->
              Right
                ( Object $ KM.insert "id" (Legacy.rrId request) responseObject
                , TE.decodeUtf8' <$> retryAfterBytes >>= either (const Nothing) Just
                )
        Right _ ->
          Left $ Legacy.unavailable "BUNDLER_UNAVAILABLE" "Alto returned a mismatched response"

validAltoResponse :: Legacy.RpcRequest -> KM.KeyMap Value -> Bool
validAltoResponse request responseObject =
  KM.lookup "jsonrpc" responseObject == Just (String "2.0")
    && KM.lookup "id" responseObject == Just (Legacy.rrId request)
    && case (KM.lookup "result" responseObject, KM.lookup "error" responseObject) of
      (Just _, Nothing) -> True
      (Nothing, Just (Object errorObject)) ->
        case (KM.lookup "code" errorObject, KM.lookup "message" errorObject) of
          (Just (Number _), Just (String message)) -> not $ T.null message
          _ -> False
      _ -> False

readBoundedAltoBody :: Int -> BodyReader -> IO (Either Text ByteString)
readBoundedAltoBody limit = go 0 []
 where
  go total chunks reader = do
    chunk <- brRead reader
    if BS.null chunk
      then pure $ Right $ BS.concat $ reverse chunks
      else
        let next = total + BS.length chunk
         in if next > limit
              then pure $ Left "Alto response exceeded the configured size limit"
              else go next (chunk : chunks) reader

paymasterResponse :: Bool -> Paymaster.SponsorshipEnvelope -> Value
paymasterResponse isFinal envelope =
  object
    [ "paymaster" .= T.toLower (Paymaster.sePaymaster envelope)
    , "paymasterData" .= Paymaster.paymasterDataHex envelope
    , "paymasterVerificationGasLimit"
        .= Paymaster.canonicalQuantity (Paymaster.seVerificationGasLimit envelope)
    , "paymasterPostOpGasLimit"
        .= Paymaster.canonicalQuantity (Paymaster.sePostOpGasLimit envelope)
    , "isFinal" .= isFinal
    ]

envelopeFromAuthorization
  :: NativeAaConfig
  -> SponsorshipAuthorization
  -> ByteString
  -> Paymaster.SponsorshipEnvelope
envelopeFromAuthorization cfg authorization signature =
  Paymaster.makeSponsorshipEnvelope
    cfg
    (saValidAfter authorization)
    (saValidUntil authorization)
    (saMaxCostWei authorization)
    signature

respondSuccess :: Value -> Value -> ActionM ()
respondSuccess requestId result = do
  setHeader "Content-Type" "application/json"
  setHeader "Cache-Control" "no-store"
  status status200
  json $
    object
      [ "jsonrpc" .= ("2.0" :: Text)
      , "id" .= requestId
      , "result" .= result
      ]

verifyNativeAccountRuntime
  :: NativeAaConfig
  -> EthClient
  -> Legacy.ParsedUserOperation
  -> IO (Either Legacy.ProxyFailure ())
verifyNativeAccountRuntime cfg client operation =
  case Legacy.puoFactoryOwner operation of
    Just _ -> pure $ Right ()
    Nothing -> do
      result <- rpcCall client "eth_getCode" $
        toJSON [String $ Legacy.puoSender operation, String "latest"]
      pure $ case result of
        Right (String codeText)
          | Just code <- decodeHex codeText
          , encodeHex (keccak256 code) == T.toLower (naaAccountCodeHash cfg) -> Right ()
        _ -> Left $ Legacy.policyDenied "Trading Account runtime code hash is not approved"

verifyNativeAccountRuntimeDual
  :: NativeAaConfig
  -> NativeSecurityContext
  -> Legacy.ParsedUserOperation
  -> IO (Either Legacy.ProxyFailure ())
verifyNativeAccountRuntimeDual cfg context operation =
  case Legacy.puoFactoryOwner operation of
    Just _ -> pure $ Right ()
    Nothing -> fmap (fmap $ const ()) $ Cache.evidenceObserved (nscAccountEvidence context)
      (\event -> maybe (pure ()) (\timing -> timingCount timing $ "runtime_cache_" <> event) $ nscTiming context)
      (sbhHash $ nscHeader context)
      ("runtime:" <> T.toLower (Legacy.puoSender operation) <> ":" <> T.toLower (naaAccountCodeHash cfg)) $ do
      let blockNumber = sbhNumber $ nscHeader context
          sender = Legacy.puoSender operation
      (primary, secondary) <- providerPair (nscRpcMode context)
        (readRuntimeCodeAt (nscPrimaryClient context) blockNumber sender)
        (readRuntimeCodeAt (nscSecondaryClient context) blockNumber sender)
      pure $ case (primary, secondary) of
        (Right firstCode, Right secondCode)
          | firstCode /= secondCode -> Left securityAttestationUnavailable
          | BS.null firstCode -> Left $ Legacy.policyDenied "Trading Account runtime code is missing"
          | encodeHex (keccak256 firstCode) == T.toLower (naaAccountCodeHash cfg) -> Right $ naaAccountCodeHash cfg
          | otherwise -> Left $ Legacy.policyDenied "Trading Account runtime code hash is not approved"
        _ -> Left securityAttestationUnavailable

readRuntimeCodeAt :: EthClient -> Integer -> Text -> IO (Either Text ByteString)
readRuntimeCodeAt client blockNumber account = do
  result <- rpcCall client "eth_getCode" $
    toJSON [String account, String $ Paymaster.canonicalQuantity blockNumber]
  pure $ case result of
    Right (String codeText)
      | Just code <- decodeHex codeText -> Right code
    _ -> Left "security RPC could not read account runtime code"

-- | Startup attestation prevents a syntactically valid but misconfigured
-- gateway from issuing signatures for a different paymaster profile.
attestNativePaymasterProfile
  :: NativeAaConfig
  -> EthClient
  -> EthClient
  -> IO (Either Text ())
attestNativePaymasterProfile cfg primaryClient secondaryClient = do
  (primaryChain, secondaryChain) <- providerPair (naaRpcMode cfg) (attestRpcChain primaryClient) (attestRpcChain secondaryClient)
  case (primaryChain, secondaryChain) of
    (Left err, _) -> pure $ Left $ "primary profile RPC: " <> err
    (_, Left err) -> pure $ Left $ "secondary profile RPC: " <> err
    (Right (), Right ()) -> do
      snapshot <- readAgreedSecurityBlock (naaRpcMode cfg) (naaMaxSafeLagSeconds cfg) primaryClient secondaryClient
      case snapshot of
        Left err -> pure $ Left err
        Right header -> do
          (primaryProfile, secondaryProfile) <- providerPair (naaRpcMode cfg) (attestProfileAt cfg primaryClient $ sbhNumber header) (attestProfileAt cfg secondaryClient $ sbhNumber header)
          verifiedHeader <-
            readAgreedSecurityHeaderAt (naaRpcMode cfg) primaryClient secondaryClient $ sbhNumber header
          pure $ do
            primaryProfile
            secondaryProfile
            finalHeader <- verifiedHeader
            unless (finalHeader == header) $
              Left "security snapshot changed during paymaster profile attestation"

attestProfileAt :: NativeAaConfig -> EthClient -> Integer -> IO (Either Text ())
attestProfileAt cfg client blockNumber = do
  let paymaster = naaPaymasterAddress cfg
  (configuredEntryPoint, configuredPaused, policy, accountHash, factory, factoryHash, implementation, implementationHash, configuredSigner, maxCost, maxValidity, factoryImplementation, liveEntryPointHash, livePaymasterHash, liveFactoryHash, liveImplementationHash) <- runConcurrently $
    (,,,,,,,,,,,,,,,) <$> Concurrently (readAddressAt client blockNumber paymaster "entryPoint()" [])
    <*> Concurrently (readBoolAt client blockNumber paymaster "paused()" [])
    <*> Concurrently (readBytes32At client blockNumber paymaster "policyId()" [])
    <*> Concurrently (readBytes32At client blockNumber paymaster "approvedAccountCodeHash()" [])
    <*> Concurrently (readAddressAt client blockNumber paymaster "accountFactory()" [])
    <*> Concurrently (readBytes32At client blockNumber paymaster "accountFactoryCodeHash()" [])
    <*> Concurrently (readAddressAt client blockNumber paymaster "accountImplementation()" [])
    <*> Concurrently (readBytes32At client blockNumber paymaster "accountImplementationCodeHash()" [])
    <*> Concurrently (readAddressAt client blockNumber paymaster "sponsorSigner()" [])
    <*> Concurrently (readUintAt client blockNumber paymaster "maxSponsoredCost()" [])
    <*> Concurrently (readUintAt client blockNumber paymaster "MAX_VALIDITY_WINDOW()" [])
    <*> Concurrently (readAddressAt client blockNumber canonicalFactory "accountImplementation()" [])
    <*> Concurrently (readCodeHashAt client blockNumber nativeEntryPoint)
    <*> Concurrently (readCodeHashAt client blockNumber paymaster)
    <*> Concurrently (readCodeHashAt client blockNumber canonicalFactory)
    <*> Concurrently (readCodeHashAt client blockNumber canonicalImplementation)
  pure $ do
    requireEqual "EntryPoint runtime code hash" reviewedEntryPointCodeHash =<< liveEntryPointHash
    requireEqual "paymaster runtime code hash" (naaPaymasterCodeHash cfg) =<< livePaymasterHash
    requireEqual "paymaster EntryPoint" nativeEntryPoint =<< configuredEntryPoint
    isPaused <- configuredPaused
    when isPaused $ Left "the reviewed paymaster is paused onchain"
    requireEqual "paymaster policy id" (T.toLower $ naaPolicyId cfg) =<< policy
    requireEqual "approved account code hash" reviewedAccountCodeHash =<< accountHash
    requireEqual "configured account code hash" reviewedAccountCodeHash $ naaAccountCodeHash cfg
    requireEqual "account factory" canonicalFactory =<< factory
    requireEqual "account implementation" canonicalImplementation =<< implementation
    requireEqual "factory-reported implementation" canonicalImplementation =<< factoryImplementation
    requireEqual "pinned factory runtime code hash" reviewedFactoryCodeHash =<< factoryHash
    requireEqual "live factory runtime code hash" reviewedFactoryCodeHash =<< liveFactoryHash
    requireEqual "pinned implementation runtime code hash" reviewedImplementationCodeHash =<< implementationHash
    requireEqual "live implementation runtime code hash" reviewedImplementationCodeHash =<< liveImplementationHash
    requireEqual "paymaster sponsor signer" (naaSignerAddress cfg) =<< configuredSigner
    configuredMaxCost <- maxCost
    unless (configuredMaxCost >= naaMaxCostWei cfg) $
      Left "backend maximum sponsorship cost exceeds the onchain paymaster ceiling"
    configuredValidity <- maxValidity
    unless (configuredValidity >= naaValiditySeconds cfg + 30) $
      Left "backend validity window exceeds the onchain paymaster ceiling"

attestRpcChain :: EthClient -> IO (Either Text ())
attestRpcChain client = do
  result <- rpcCall client "eth_chainId" $ toJSON ([] :: [Value])
  pure $ case result of
    Right (String chainValue) | T.toLower chainValue == "0x66eee" -> Right ()
    Right _ -> Left "PERPS_RPC_URL did not attest Arbitrum Sepolia chain id 421614"
    Left _ -> Left "could not attest PERPS_RPC_URL chain id"

readAgreedSecurityBlock
  :: AaRpcMode
  -> Integer
  -> EthClient
  -> EthClient
  -> IO (Either Text SecurityBlockHeader)
readAgreedSecurityBlock mode maxSafeLag primaryClient secondaryClient = do
  (primarySafe, secondarySafe) <- providerPair mode (readSecurityHeader primaryClient "safe") (readSecurityHeader secondaryClient "safe")
  case (primarySafe, secondarySafe) of
    (Left err, _) -> pure $ Left $ "primary security RPC: " <> err
    (_, Left err) -> pure $ Left $ "secondary security RPC: " <> err
    (Right firstSafe, Right secondSafe) -> do
      let agreedNumber = min (sbhNumber firstSafe) (sbhNumber secondSafe)
      agreed <- readAgreedSecurityHeaderAt mode primaryClient secondaryClient agreedNumber
      now <- floor <$> getPOSIXTime
      pure $ do
        header <- agreed
        when (sbhNumber firstSafe == agreedNumber && firstSafe /= header) $
          Left "primary safe header disagrees with its explicit numeric header"
        when (sbhNumber secondSafe == agreedNumber && secondSafe /= header) $
          Left "secondary safe header disagrees with its explicit numeric header"
        validateSecurityHeaderTime maxSafeLag now header
        Right header

validateSecurityHeaderTime :: Integer -> Integer -> SecurityBlockHeader -> Either Text ()
validateSecurityHeaderTime maxSafeLag now header = do
  when (sbhTimestamp header < now - maxSafeLag) $
    Left "the dual-provider security snapshot is stale"
  when (sbhTimestamp header > now + gatewayMaxFutureSkewSeconds) $
    Left "the dual-provider security snapshot timestamp is in the future"

readAgreedSecurityHeaderAt
  :: AaRpcMode
  -> EthClient
  -> EthClient
  -> Integer
  -> IO (Either Text SecurityBlockHeader)
readAgreedSecurityHeaderAt mode primaryClient secondaryClient blockNumber = do
  let blockTag = Paymaster.canonicalQuantity blockNumber
  (primary, secondary) <- providerPair mode (readSecurityHeader primaryClient blockTag) (readSecurityHeader secondaryClient blockTag)
  pure $ case (primary, secondary) of
    (Left err, _) -> Left $ "primary security RPC: " <> err
    (_, Left err) -> Left $ "secondary security RPC: " <> err
    (Right firstHeader, Right secondHeader)
      | firstHeader == secondHeader -> Right firstHeader
      | otherwise -> Left "security RPC providers disagree on the explicit block header"

readSecurityHeader :: EthClient -> Text -> IO (Either Text SecurityBlockHeader)
readSecurityHeader client blockTag = do
  result <- rpcCall client "eth_getBlockByNumber" $ toJSON [String blockTag, Bool False]
  pure $ case result of
    Right (Object blockObject) -> do
      number <- securityQuantity blockObject "number"
      case parseRpcQuantity blockTag of
        Just requested | number /= requested ->
          Left "security RPC returned a different explicit block number"
        _ -> Right ()
      blockHash <- case KM.lookup "hash" blockObject of
        Just (String value) | isFixedHex 32 value -> Right $ T.toLower value
        _ -> Left "security RPC returned an invalid block hash"
      timestamp <- securityQuantity blockObject "timestamp"
      baseFeePerGas <- securityQuantity blockObject "baseFeePerGas"
      Right $ SecurityBlockHeader number blockHash timestamp baseFeePerGas
    _ -> Left "security RPC could not read a block header"
 where
  securityQuantity :: KM.KeyMap Value -> Text -> Either Text Integer
  securityQuantity blockObject name =
    case KM.lookup (Key.fromText name) blockObject of
      Just (String value) ->
        maybe (Left $ "security RPC returned an invalid " <> name) Right $
          parseRpcQuantity value
      _ -> Left $ "security RPC omitted " <> name

readBytes32At :: EthClient -> Integer -> Text -> Text -> [ByteString] -> IO (Either Text Text)
readBytes32At client blockNumber target signature arguments = do
  result <- ethCallAtBlock client (CallParams target $ encodeCall signature arguments) blockNumber
  pure $ case result of
    Right bytes | BS.length bytes == 32 -> Right $ encodeHex bytes
    _ -> Left $ "could not attest " <> signature

readAddressAt :: EthClient -> Integer -> Text -> Text -> [ByteString] -> IO (Either Text Text)
readAddressAt client blockNumber target signature arguments = do
  result <- ethCallAtBlock client (CallParams target $ encodeCall signature arguments) blockNumber
  pure $ case result of
    Right bytes
      | BS.length bytes == 32
      , BS.take 12 bytes == BS.replicate 12 0 ->
          Right $ T.toLower $ decodeAddress bytes
    _ -> Left $ "could not attest " <> signature

readUintAt :: EthClient -> Integer -> Text -> Text -> [ByteString] -> IO (Either Text Integer)
readUintAt client blockNumber target signature arguments = do
  result <- ethCallAtBlock client (CallParams target $ encodeCall signature arguments) blockNumber
  pure $ case result of
    Right bytes | BS.length bytes == 32 -> Right $ decodeUint256 bytes
    _ -> Left $ "could not attest " <> signature

readBoolAt :: EthClient -> Integer -> Text -> Text -> [ByteString] -> IO (Either Text Bool)
readBoolAt client blockNumber target signature arguments = do
  result <- readUintAt client blockNumber target signature arguments
  pure $ do
    value <- result
    case value of
      0 -> Right False
      1 -> Right True
      _ -> Left $ "non-canonical boolean from " <> signature

readCodeHashAt :: EthClient -> Integer -> Text -> IO (Either Text Text)
readCodeHashAt client blockNumber address = do
  result <- rpcCall client "eth_getCode" $
    toJSON [String address, String $ Paymaster.canonicalQuantity blockNumber]
  pure $ case result of
    Right (String codeText)
      | Just code <- decodeHex codeText
      , not (BS.null code) -> Right $ encodeHex $ keccak256 code
    _ -> Left $ "could not read runtime code for " <> address

requireEqual :: Text -> Text -> Text -> Either Text ()
requireEqual label expected actual =
  unless (T.toLower expected == T.toLower actual) $
    Left $ label <> " does not match the reviewed deployment"

firstInvalidParams :: Either Text a -> Either Legacy.ProxyFailure a
firstInvalidParams = either (Left . Legacy.invalidParams) Right

responseOperationHash :: Value -> Maybe Text
responseOperationHash (Object responseObject) =
  case KM.lookup "result" responseObject of
    Just (String operationHash) | isFixedHex 32 operationHash -> Just $ T.toLower operationHash
    _ -> Nothing
responseOperationHash _ = Nothing

isRpcErrorResponse :: Value -> Bool
isRpcErrorResponse (Object responseObject) =
  case KM.lookup "error" responseObject of
    Just (Object _) -> True
    _ -> False
isRpcErrorResponse _ = False

normalizeAddress :: Text -> Maybe Text
normalizeAddress raw =
  let value = T.toLower $ T.strip raw
   in if isFixedHex 20 value then Just value else Nothing

decodeHex :: Text -> Maybe ByteString
decodeHex value
  | not (T.isPrefixOf "0x" value) || odd (T.length $ T.drop 2 value) = Nothing
  | otherwise = either (const Nothing) Just $ B16.decode $ TE.encodeUtf8 $ T.drop 2 $ T.toLower value

decodeFixedHex :: Int -> Text -> Maybe ByteString
decodeFixedHex bytes value = do
  decoded <- decodeHex value
  if BS.length decoded == bytes then Just decoded else Nothing

isFixedHex :: Int -> Text -> Bool
isFixedHex bytes value = maybe False ((== bytes) . BS.length) $ decodeHex value

encodeHex :: ByteString -> Text
encodeHex bytes = "0x" <> TE.decodeUtf8 (B16.encode bytes)

liftEpochSeconds :: ActionM Integer
liftEpochSeconds = liftIO $ floor <$> getPOSIXTime

liftDb :: IO a -> ActionM (Either SomeException a)
liftDb action = liftIO $ try action

databaseUnavailable :: Legacy.ProxyFailure
databaseUnavailable =
  Legacy.unavailable "SPONSOR_UNAVAILABLE" "The AA authorization database is temporarily unavailable"

respondNativeDbFailure :: Value -> Legacy.PimlicoMethod -> Text -> ActionM ()
respondNativeDbFailure requestId requestMethod reason = do
  liftIO $
    logErrorEvery
      30
      "aa_native_sponsorship_database_failure"
      "A native AA durable-state operation failed closed"
      [ field "method" $ show requestMethod
      , field "reason" reason
      ]
  Legacy.respondFailure requestId databaseUnavailable

paymasterPaused :: Legacy.ProxyFailure
paymasterPaused =
  Legacy.unavailable "PAYMASTER_PAUSED" "Native gas sponsorship is disabled or circuit-broken"

nativeEntryPoint :: Text
nativeEntryPoint = "0x4337084d9e255ff0702461cf8895ce9e3b5ff108"

canonicalFactory :: Text
canonicalFactory = "0x13e9ed32155810fdbd067d4522c492d6f68e5944"

canonicalImplementation :: Text
canonicalImplementation = "0x28426d752372d68d34340bd94390950dce3c9ec3"

reviewedEntryPointCodeHash :: Text
reviewedEntryPointCodeHash =
  "0xe3f30f78ae55058acdefea00952c8e44f2263215cf720fe1b27b6f148add0278"

reviewedFactoryCodeHash :: Text
reviewedFactoryCodeHash =
  "0xa2e635152a61e180383c7afc045620b7461ef6f43ba27d592262513106b991b7"

reviewedImplementationCodeHash :: Text
reviewedImplementationCodeHash =
  "0x689a90eff03926a12aedad2fc6d4fdbcbdd9ffac86e7d0d70ce6355961305c74"

reviewedAccountCodeHash :: Text
reviewedAccountCodeHash =
  "0x41ee894da413cc99e8dec0a1784470eceb736845ad1591e06ff0ecdf0aca26c9"

maxAltoResponseBytes :: Int
maxAltoResponseBytes = 1024 * 1024

gatewayMaxFutureSkewSeconds :: Integer
gatewayMaxFutureSkewSeconds = 60

-- The account bucket must not vary with IP/client identity; otherwise an
-- attacker can rotate source addresses to multiply one account's allowance.
nativeAccountRateClientKey :: Text
nativeAccountRateClientKey = "0x" <> T.replicate 64 "0"

isCanaryGated :: NativeAaConfig -> Legacy.RpcRequest -> Text -> Bool
isCanaryGated cfg request owner =
  not (ownerAllowedForNativeCanary cfg owner)
    && Legacy.rrMethod request
      `elem` [ Legacy.GetPaymasterStubData
             , Legacy.GetPaymasterData
             ]

-- Submission is authorized by the exact persisted client/digest/signature/hash,
-- not today's issuance cohort. A public-to-allowlisted rollback must still drain
-- already-issued operations through submitNativeOperation's unchanged checks.

ownerAllowedForNativeCanary :: NativeAaConfig -> Text -> Bool
ownerAllowedForNativeCanary cfg owner =
  naaGlobalRolloutEnabled cfg
    || T.toLower owner `elem` naaCanaryOwners cfg

validateHardEconomicCaps :: Paymaster.PackedUserOperation -> Either Text ()
validateHardEconomicCaps operation = do
  bounded "callGasLimit" 1 Preparation.sepoliaExecutionGasCap $ Paymaster.puoCallGasLimit operation
  bounded "verificationGasLimit" 1 1_000_000 $ Paymaster.puoVerificationGasLimit operation
  bounded "preVerificationGas" 1 1_000_000 $ Paymaster.puoPreVerificationGas operation
  bounded "aggregateGas" 1 5_000_000 $
    Paymaster.puoCallGasLimit operation + Paymaster.puoVerificationGasLimit operation
    + Paymaster.puoPreVerificationGas operation
    + maybe 100_000 id (Paymaster.puoPaymasterVerificationGasLimit operation)
    + maybe 0 id (Paymaster.puoPaymasterPostOpGasLimit operation)
  bounded "maxFeePerGas" 1 10_000_000_000 $ Paymaster.puoMaxFeePerGas operation
  bounded "maxPriorityFeePerGas" 0 2_000_000_000 $ Paymaster.puoMaxPriorityFeePerGas operation
 where
  bounded label minimum maximum value =
    unless (value >= minimum && value <= maximum) $
      Left $ "UserOperation " <> label <> " exceeds the reviewed sponsorship bounds"

validateLiveFeeCapDual
  :: NativeSecurityContext
  -> Paymaster.PackedUserOperation
  -> IO (Either Legacy.ProxyFailure ())
validateLiveFeeCapDual context operation = do
  let agreedBaseFee = sbhBaseFeePerGas $ nscHeader context
      allowedMaxFee = nativeMaxFeeAllowance agreedBaseFee
  pure $
    if Paymaster.puoMaxFeePerGas operation <= allowedMaxFee
      then Right ()
      else Left $ Legacy.policyDenied "UserOperation maxFeePerGas is above the agreed safe-block fee allowance"

-- | Both providers must return the exact same explicit safe-block header,
-- including baseFeePerGas. A fixed priority-fee cap is enforced separately by
-- 'validateHardEconomicCaps', so one provider cannot relax this allowance with
-- an untagged fee suggestion.
nativeMaxFeeAllowance :: Integer -> Integer
nativeMaxFeeAllowance agreedBaseFee =
  min 10_000_000_000 $ max 1_000_000_000 $ agreedBaseFee * 3

parseRpcQuantity :: Text -> Maybe Integer
parseRpcQuantity raw =
  let value = T.toLower raw
      digits = T.drop 2 value
      validDigit char =
        (char >= '0' && char <= '9') || (char >= 'a' && char <= 'f')
      digitValue char
        | char <= '9' = fromIntegral $ fromEnum char - fromEnum '0'
        | otherwise = fromIntegral $ fromEnum char - fromEnum 'a' + 10
   in if
        T.isPrefixOf "0x" value
          && not (T.null digits)
          && T.length digits <= 64
          && T.all validDigit digits
          && (T.length digits == 1 || T.head digits /= '0')
        then Just $ T.foldl' (\total digit -> total * 16 + digitValue digit) 0 digits
        else Nothing

authorizationIsUsable :: Integer -> SponsorshipAuthorization -> Bool
authorizationIsUsable now authorization =
  saState authorization `elem` ["reserved", "signed", "submitted"]
    && saValidUntil authorization > now + 30

sponsorshipRequestKey
  :: NativeAaConfig
  -> Text
  -> Text
  -> Paymaster.PackedUserOperation
  -> Text
sponsorshipRequestKey cfg clientKey owner operation =
  encodeHex $
    keccak256 $
      "PletherSponsorshipRequest/v1"
        <> Paymaster.userOperationHash operation
        <> TE.encodeUtf8 (T.toLower owner)
        <> TE.encodeUtf8 (T.toLower clientKey)
        <> TE.encodeUtf8
          ( T.intercalate
              "|"
              [ T.toLower $ naaPaymasterAddress cfg
              , T.toLower $ naaPaymasterCodeHash cfg
              , T.toLower $ naaPolicyId cfg
              , T.toLower $ naaSignerAddress cfg
              , T.toLower $ naaAccountCodeHash cfg
              , T.pack $ show $ naaVerificationGasLimit cfg
              , T.pack $ show $ naaPostOpGasLimit cfg
              , T.pack $ show $ naaMaxCostWei cfg
              , T.pack $ show $ naaValiditySeconds cfg
              ]
          )


assistanceReservation :: Text -> Legacy.CloseAssistanceIntent -> CloseAssistanceReservation
assistanceReservation sender intent = CloseAssistanceReservation
  Manifest.orderRouterAddress sender (encodeHex $ Legacy.caiClientOrderId intent)
  (encodeHex $ keccak256 $ Legacy.caiRequest intent) (Legacy.caiLens intent) (Legacy.caiAmountUsdc intent)

validateCloseAssistanceDual
  :: NativeGatewayState -> Maybe NativeSecurityContext -> Text -> Maybe Legacy.CloseAssistanceIntent
  -> IO (Either Legacy.ProxyFailure ())
validateCloseAssistanceDual _ _ _ Nothing = pure $ Right ()
validateCloseAssistanceDual state (Just context) sender (Just intent)
  | Just config <- ngsCloseAssistance state, nscRpcMode context == DualIndependent = do
      -- Eligibility is current state. The safe snapshot may predate this position or
      -- make a fresh order deadline appear too far in the future. Pin both live reads
      -- to the same explicit block and bracket them with independent header agreement.
      let primary = nscPrimaryClient context
          secondary = nscSecondaryClient context
      heads <- concurrently (readSecurityHeader primary "latest") (readSecurityHeader secondary "latest")
      case heads of
        (Right first, Right second) -> do
          let blockNumber = min (sbhNumber first) (sbhNumber second)
          before <- readAgreedSecurityHeaderAt DualIndependent primary secondary blockNumber
          now <- floor <$> getPOSIXTime
          case before of
            Right header | validateSecurityHeaderTime 30 now header == Right () -> do
              let call client = do
                    hash <- readCodeHashAt client blockNumber (cacLens config)
                    result <- rpcCall client "eth_call" $ toJSON
                      [object ["from" .= sender,"to" .= cacLens config,"data" .= encodeHex (Legacy.caiGuardData intent)],
                       String $ Paymaster.canonicalQuantity blockNumber]
                    pure $ hash == Right (cacCodeHash config) && result == Right (String "0x")
              (firstValid,secondValid) <- concurrently (call primary) (call secondary)
              after <- readAgreedSecurityHeaderAt DualIndependent primary secondary blockNumber
              canonical <- revalidateSecurityContext context
              pure $ if firstValid && secondValid && before == after && canonical == Right () then Right ()
                else Left $ Legacy.policyDenied "Close assistance eligibility or deployment changed; review again"
            _ -> pure $ Left securityAttestationUnavailable
        _ -> pure $ Left securityAttestationUnavailable
validateCloseAssistanceDual _ _ _ _ = pure $ Left securityAttestationUnavailable

-- Status remains available with issuance disabled. No security/signing/bundler
-- mutation is reachable from this handler; the reconciler alone releases liability.
preparationStatus :: NativeAaConfig -> DbPool -> EthClient -> Manager -> Text -> Legacy.RpcRequest -> ActionM ()
preparationStatus = preparationStatusWithRecovery Nothing

preparationStatusWithRecovery :: Maybe Bool -> NativeAaConfig -> DbPool -> EthClient -> Manager -> Text -> Legacy.RpcRequest -> ActionM ()
preparationStatusWithRecovery canRetire cfg pool client manager clientKey request = case Preparation.parsePreparationLocator $ Legacy.rrParams request of
  Left failure -> Legacy.respondFailure requestId failure
  Right locator -> do
    credential <- header "X-Plether-AA-Recovery"
    now <- liftEpochSeconds
    let hash = Preparation.plHash locator
        capabilityClient = hash >>= \h -> credential >>= RecoveryCapability.verify
          (naaProxyOriginToken cfg) (naaPaymasterAddress cfg) now h . TL.toStrict
        authorizedClient = maybe clientKey id capabilityClient
    status <- liftDb $ withDb pool $ \conn -> PreparationDb.getPreparationStatus conn
      authorizedClient (Preparation.plSender locator) (Preparation.plIdentifier locator) hash
    case status of
      Left _ -> Legacy.respondFailure requestId databaseUnavailable
      Right Nothing -> Legacy.respondFailure requestId $
        Legacy.ProxyFailure status403 (-32001) "Preparation is unavailable for this client" "PREPARATION_NOT_AUTHORIZED" False
      Right (Just (Object fields, storedOperation)) -> do
        cursor <- liftDb $ withDb pool $ \conn -> getAaReconcilerCursor conn 421614 (naaPaymasterAddress cfg)
        safeTimestamp <- case cursor of
          Right (Just position) -> do
            block <- liftIO $ readSecurityHeader client (Paymaster.canonicalQuantity $ arcSafeBlock position)
            pure $ case block of
              Right value | sbhHash value == arcSafeBlockHash position -> Just $ sbhTimestamp value
              _ -> Nothing
          _ -> pure Nothing
        let requestedAssistance = case storedOperation of
              Just (Object payload) -> case Preparation.internalRequest "eth_estimateUserOperationGas"
                [Object $ KM.insert "signature" (String Legacy.dummySignature) payload, String nativeEntryPoint]
                >>= Legacy.validateMethodParams of
                  Right (Just policy) -> length (Legacy.puoCalls policy) == 5
                  _ -> False
              _ -> False
        case KM.lookup "userOperationHash" fields of
          Just (String h) -> issueRecoveryCapability cfg h authorizedClient
          _ -> pure ()
        observed <- case (KM.lookup "authorizationState" fields, KM.lookup "userOperationHash" fields, storedOperation) of
          (Just (String state), Just (String h), Just (Object payload)) | state `elem` ["signed","submitted"] ->
            liftIO $ observePreparationInclusion cfg client manager h payload
          _ -> pure Nothing
        let withObservation = case observed of
              Just (tx, success) -> KM.insert "transactionHash" (String tx) $ KM.insert "executionSuccess" (Bool success) fields
              Nothing -> fields
        let recoveryFields = case canRetire of
              Nothing -> withObservation
              Just allowed -> KM.insert "recoveryVerified" (Bool True) $ KM.insert "canRetire" (Bool allowed) withObservation
        respondSuccess requestId $ Preparation.preparationStatusResponse now safeTimestamp requestedAssistance recoveryFields
      _ -> Legacy.respondFailure requestId databaseUnavailable
 where requestId = Legacy.rrId request

-- Alto supplies a locator only. Direct receipt/event and block-hash checks must
-- agree before reporting an observed (still unsafe) inclusion. No evidence is
-- retained across reads, so a reorg retracts this observation on the next poll.
observePreparationInclusion :: NativeAaConfig -> EthClient -> Manager -> Text -> KM.KeyMap Value -> IO (Maybe (Text, Bool))
observePreparationInclusion cfg client manager expectedHash payload = do
  locator <- altoResult manager cfg "eth_getUserOperationReceipt" [String expectedHash]
  case locator of
    Right (Object fields) | Just (Object receipt) <- KM.lookup "receipt" fields,
      Just (String tx) <- KM.lookup "transactionHash" receipt, isFixedHex 32 tx -> do
        direct <- rpcCall client "eth_getTransactionReceipt" $ toJSON [String tx]
        case direct of
          Right (Object canonicalReceipt) | Just (String numberText) <- KM.lookup "blockNumber" canonicalReceipt,
            Just number <- parseRpcQuantity numberText,
            Just (String blockHash) <- KM.lookup "blockHash" canonicalReceipt,
            KM.lookup "status" canonicalReceipt == Just (String "0x1"),
            KM.lookup "transactionHash" canonicalReceipt == Just (String tx),
            Just (Array logs) <- KM.lookup "logs" canonicalReceipt,
            Right operation <- Paymaster.parsePackedUserOperation payload -> do
              headerResult <- readSecurityHeader client numberText
              let events = [event | raw <- toList logs,
                    Right event <- [Reconciler.parseUserOperationEvent (naaPaymasterAddress cfg) number number raw],
                    Reconciler.uoeHash event == expectedHash,
                    Reconciler.uoeSender event == Paymaster.puoSender operation,
                    Reconciler.uoeNonce event == Paymaster.puoNonce operation,
                    Reconciler.uoeTransactionHash event == T.toLower tx,
                    Reconciler.uoeBlockHash event == T.toLower blockHash]
              pure $ case (headerResult, events) of
                (Right block, [event]) | sbhHash block == T.toLower blockHash -> Just (T.toLower tx, Reconciler.uoeSuccess event)
                _ -> Nothing
          _ -> pure Nothing
    _ -> pure Nothing

recoveryDenied :: Legacy.ProxyFailure
recoveryDenied = Legacy.ProxyFailure status403 (-32001) "Verify the owner wallet to recover this saved attempt" "RECOVERY_VERIFICATION_REQUIRED" False

recoverySessionOwner :: DbPool -> RecoveryDb.Scope -> ActionM (Either Legacy.ProxyFailure Text)
recoverySessionOwner pool scope = do
  supplied <- header "X-Plether-AA-Preparation-Recovery"
  case TL.toStrict <$> supplied of
    Just token | T.length token == 64 -> do
      found <- liftDb $ withDb pool $ \conn -> RecoveryDb.sessionOwner conn scope (Recovery.tokenHash token)
      pure $ case found of
        Right (Just owner) -> Right owner
        Left _ -> Left databaseUnavailable
        _ -> Left recoveryDenied
    _ -> pure $ Left recoveryDenied

preparationRecoveryClient :: Config -> DbPool -> RecoveryDb.Scope -> Text -> ActionM (Either Legacy.ProxyFailure Text)
preparationRecoveryClient cfg pool scope current = do
  matches <- liftDb $ withDb pool $ \conn -> RecoveryDb.matchingPreparations conn scope (T.toLower $ cfgPerpsOrderRouter cfg)
  supplied <- header "X-Plether-AA-Preparation-Recovery"
  case matches of
    Left _ -> pure $ Left databaseUnavailable
    Right [] | supplied == Nothing -> pure $ Right current
    Right [(client,_,_)] | client == current && supplied == Nothing -> pure $ Right current
    Right [(client,_,True)] -> do
      verified <- recoverySessionOwner pool scope
      pure $ client <$ verified
    _ -> pure $ Left recoveryDenied

handlePreparationRecovery
  :: NativeGatewayState -> Config -> NativeAaConfig -> DbPool -> EthClient
  -> Manager -> Text -> Legacy.RpcRequest -> ActionM ()
handlePreparationRecovery state cfg native pool client manager clientKey request = case ngsRecoveryOrigin state of
  Nothing -> Legacy.respondFailure requestId $ Legacy.unavailable "RECOVERY_DISABLED" "Wallet recovery is not enabled on this deployment"
  Just origin -> case Recovery.parseRecoveryRequest (naaPaymasterAddress native) extra (Legacy.rrParams request) of
    Left failure -> Legacy.respondFailure requestId failure
    Right (scope,fields) -> do
      rate <- liftDb $ withDb pool $ \conn -> consumeAaRateLimit conn "preparation-recovery" clientKey
        (pseudonymousAccountKey (naaProxyOriginToken native) $ RecoveryDb.scopeSender scope) 20
      case rate of
        Left _ -> Legacy.respondFailure requestId databaseUnavailable
        Right False -> Legacy.respondFailure requestId Legacy.rateLimited
        Right True -> case method of
          Legacy.GetRecoveryChallenge -> case (Recovery.requiredText "owner" 42 fields, Recovery.requiredText "origin" 256 fields) of
            (Right owner,Right requestedOrigin) | isFixedHex 20 owner && owner == T.toLower owner && requestedOrigin == origin -> do
              nonce <- liftIO Recovery.randomToken
              now <- liftEpochSeconds
              let expires = now+300
                  message = Recovery.renderChallenge origin scope owner nonce expires
              saved <- liftDb $ withDb pool $ \conn -> RecoveryDb.saveChallengeAt conn scope nonce owner message expires
              case saved of
                Left _ -> Legacy.respondFailure requestId databaseUnavailable
                Right () -> respondSuccess requestId $ object ["version" .= (1::Int),"challengeId" .= nonce,"message" .= message,"expiresAt" .= expires]
            _ -> Legacy.respondFailure requestId $ Legacy.invalidParams "Invalid recovery owner or origin"
          Legacy.VerifyRecoveryChallenge -> case (Recovery.requiredText "challengeId" 64 fields, Recovery.requiredText "signature" 132 fields) of
            (Right challenge,Right signature) -> do
              saved <- liftDb $ withDb pool $ \conn -> RecoveryDb.readChallenge conn scope challenge
              case saved of
                Right (Just (owner,message)) -> do
                  signer <- liftIO $ recoverPersonalSignAddress message signature
                  -- Recovery proves ownership only. It never substitutes latest
                  -- state for the safe-state checks required by preparation.
                  latest <- liftIO $ readSecurityHeader client "latest"
                  owned <- case latest of
                    Right block -> liftIO $ do
                      derived <- Legacy.resolveOwnedTradingAccountAtBlock client owner (sbhNumber block)
                      code <- Legacy.readCodeAtBlock client (RecoveryDb.scopeSender scope) (sbhNumber block)
                      case code of
                        Left _ -> pure $ Left Legacy.OwnedTradingAccountProofUnavailable
                        Right bytes -> do
                          identity <- Legacy.verifyAccountIdentityAtBlock client (sbhNumber block) $
                            Legacy.ParsedUserOperation (RecoveryDb.scopeSender scope) (if BS.null bytes then Just owner else Nothing) []
                          pure $ if identity == Right owner then derived else Left Legacy.OwnedTradingAccountProofUnavailable
                    _ -> pure $ Left Legacy.OwnedTradingAccountProofUnavailable
                  if signer /= Right owner || owned /= Right (RecoveryDb.scopeSender scope)
                    then Legacy.respondFailure requestId recoveryDenied
                    else do
                      token <- liftIO Recovery.randomToken
                      consumed <- liftDb $ withDb pool $ \conn -> RecoveryDb.consumeChallenge conn scope challenge owner (Recovery.tokenHash token)
                      case consumed of
                        Right True -> do
                          liftIO $ logInfo "aa_preparation_recovery_verified" "Owner verified preparation recovery" [field "stage" ("recovery"::Text),field "attempt_id" (ngsAttemptId state)]
                          respondSuccess requestId $ object ["version" .= (1::Int),"sessionToken" .= token,"expiresIn" .= (900::Int)]
                        Left _ -> Legacy.respondFailure requestId databaseUnavailable
                        _ -> Legacy.respondFailure requestId recoveryDenied
                Left _ -> Legacy.respondFailure requestId databaseUnavailable
                _ -> Legacy.respondFailure requestId recoveryDenied
            _ -> Legacy.respondFailure requestId $ Legacy.invalidParams "Invalid recovery proof"
          _ -> do
            authorized <- recoverySessionOwner pool scope
            case authorized of
              Left failure -> Legacy.respondFailure requestId failure
              Right _ -> case method of
                Legacy.RetirePreparation | ngsRetirementEnabled state -> do
                  result <- liftDb $ withDb pool $ \conn -> RecoveryDb.retirePreparation conn scope (T.toLower $ cfgPerpsOrderRouter cfg)
                  case result of
                    Left _ -> Legacy.respondFailure requestId databaseUnavailable
                    Right (Left reason) -> respondRecoveryState "unresolved" reason False []
                    Right (Right ()) -> do
                      liftIO $ logInfo "aa_preparation_retired" "Saved preparation safely retired" [field "stage" ("recovery"::Text),field "attempt_id" (ngsAttemptId state)]
                      respondRecoveryState "retired" "PREPARATION_RETIRED" False []
                Legacy.RetirePreparation -> respondRecoveryState "unresolved" "RECOVERY_RETIREMENT_DISABLED" False []
                _ -> do
                  result <- liftDb $ withDb pool $ \conn -> do
                    retired <- RecoveryDb.registryRetired conn scope
                    matches <- RecoveryDb.matchingPreparations conn scope (T.toLower $ cfgPerpsOrderRouter cfg)
                    reason <- RecoveryDb.retirementReason conn scope (T.toLower $ cfgPerpsOrderRouter cfg)
                    pure (retired,matches,reason)
                  case result of
                    Left _ -> Legacy.respondFailure requestId databaseUnavailable
                    Right (True,_,_) -> respondRecoveryState "retired" "PREPARATION_RETIRED" False []
                    Right (False,matches,reason) -> do
                      let canRetire = ngsRetirementEnabled state && reason == Nothing
                          hashes = [h | (_,Just h,_) <- take 20 matches]
                      case matches of
                        [] -> respondRecoveryState "missing" "PREPARATION_NOT_CREATED" canRetire []
                        [(originalClient,_,True)] -> preparationStatusWithRecovery (Just canRetire) native pool client manager originalClient request
                        _ | any (\(_,_,bound) -> not bound) matches -> respondRecoveryState "unresolved" "RECOVERY_BINDING_UNRESOLVED" False hashes
                          | otherwise -> respondRecoveryState "ambiguous" "RECOVERY_MULTIPLE_PREPARATIONS" canRetire hashes
 where
  requestId = Legacy.rrId request
  method = Legacy.rrMethod request
  extra | method == Legacy.GetRecoveryChallenge = ["owner","origin"]
        | method == Legacy.VerifyRecoveryChallenge = ["challengeId","signature"]
        | otherwise = []
  respondRecoveryState :: Text -> Text -> Bool -> [Text] -> ActionM ()
  respondRecoveryState recoveryState reason canRetire hashes = do
    outcomes <- case Recovery.parseRecoveryRequest (naaPaymasterAddress native) extra (Legacy.rrParams request) of
      Right (scope,_) | not (null hashes) -> liftDb $ withDb pool $ \conn -> RecoveryDb.operationOutcomes conn scope
      _ -> pure $ Right []
    liftIO $ logInfo "aa_preparation_recovery_result" "Preparation recovery result"
      [field "attempt_id" (ngsAttemptId state),field "reason" reason,field "outcome" recoveryState]
    case outcomes of
      Left _ -> Legacy.respondFailure requestId databaseUnavailable
      Right values -> respondSuccess requestId $ object
        ["version" .= (1::Int),"recoveryState" .= recoveryState,"reason" .= reason,"canRetire" .= canRetire,"operationHashes" .= hashes,"operationOutcomes" .= values]
