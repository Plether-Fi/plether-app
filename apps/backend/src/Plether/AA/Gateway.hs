module Plether.AA.Gateway
  ( NativeGatewayState
  , newNativeGatewayState
  , nativeGatewayIssuanceError
  , handleNativeAaRpc
  , attestNativePaymasterProfile
  , ownerAllowedForNativeCanary
  , validateHardEconomicCaps
  , nativeAccountRateClientKey
  , nativeMaxFeeAllowance
  , nativeStartupFailure
  ) where

import Control.Exception (SomeException, try)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
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
import Plether.AA.ClientKey
  ( pseudonymousAccountKey
  , pseudonymousClientKey
  )
import Plether.AA.Kms
  ( PaymasterSigner (..)
  , newKmsPaymasterSigner
  )
import qualified Plether.AA.Pimlico as Legacy
import Plether.Config (Config (..), NativeAaConfig (..))
import Plether.Database (DbPool, withDb)
import Plether.Database.AaSponsorship
  ( SponsorshipAuthorization (..)
  , SponsorshipDraft (..)
  , consumeAaRateLimit
  , getAaIssuancePause
  , getSponsorshipByDigest
  , getSponsorshipByRequestKey
  , isRecoveryOperationAuthorized
  , isSponsorshipDeliveryAllowed
  , markSponsorshipSubmitted
  , reserveSponsorship
  , storeSponsorshipSignature
  )
import Plether.Ethereum.Abi
  ( decodeAddress
  , decodeUint256
  , encodeCall
  , keccak256
  )
import Plether.Ethereum.Client
  ( CallParams (..)
  , EthClient
  , ethCallAtBlock
  , newClient
  , rpcCall
  )
import Plether.Logging (field, logError, logErrorEvery)
import Web.Scotty
  ( ActionM
  , header
  , json
  , setHeader
  , status
  )
import qualified Web.Scotty as Scotty
import System.Timeout (timeout)

data NativeGatewayState = NativeGatewayState
  { ngsSigner :: Maybe PaymasterSigner
  , ngsIssuanceError :: Maybe Text
  , ngsSecurityClient :: Maybe EthClient
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
  }

nativeGatewayIssuanceError :: NativeGatewayState -> Maybe Text
nativeGatewayIssuanceError = ngsIssuanceError

newNativeGatewayState
  :: Manager
  -> Config
  -> EthClient
  -> IO NativeGatewayState
newNativeGatewayState manager cfg client =
  case cfgNativeAaConfig cfg of
    Nothing -> pure $ NativeGatewayState Nothing Nothing Nothing
    Just nativeCfg -> do
      securityClient <- newClient $ naaSecurityRpcUrl nativeCfg
      profile <- attestNativePaymasterProfile nativeCfg client securityClient
      case profile of
        Left err -> pure $ NativeGatewayState Nothing (Just err) (Just securityClient)
        Right ()
          | not (naaSponsorshipEnabled nativeCfg) ->
              pure $ NativeGatewayState Nothing Nothing $ Just securityClient
          | otherwise -> do
              signer <-
                newKmsPaymasterSigner
                  manager
                  (naaKmsKeyId nativeCfg)
                  (naaSignerAddress nativeCfg)
              pure $ case signer of
                Left err -> NativeGatewayState Nothing (Just err) $ Just securityClient
                Right resolved -> NativeGatewayState (Just resolved) Nothing $ Just securityClient

handleNativeAaRpc
  :: NativeGatewayState
  -> Config
  -> Maybe DbPool
  -> EthClient
  -> Manager
  -> ActionM ()
handleNativeAaRpc gatewayState cfg mPool perpsClient manager =
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
                liftDb $
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
                        Legacy.GetPaymasterStubData ->
                          ("ip", naaIpRateLimitPerMinute nativeCfg)
                        _ -> ("ip", naaIpRateLimitPerMinute nativeCfg * 4)
                    emptyAccountKey =
                      pseudonymousAccountKey (naaProxyOriginToken nativeCfg) "no-account"
                ipRate <-
                  liftDb $
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
dispatchNative gatewayState cfg nativeCfg pool perpsClient manager _trustedIp clientKey request =
  case validateNativeParams request of
    Left failure -> Legacy.respondFailure (Legacy.rrId request) failure
    Right (mPolicyOperation, mPackedOperation) -> do
      authorizedRead <- authorizeRecoveryRead pool clientKey request
      if not authorizedRead
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
                  if requiresDualSecurity request
                    then liftIO $ nativeSecurityContext nativeCfg gatewayState perpsClient
                    else pure $ Right Nothing
                case securityContext of
                  Left _ -> respondSecurityAttestationFailure (Legacy.rrId request) "initial security context"
                  Right mSecurityContext -> do
                    identity <- liftIO $
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
                        case Legacy.validateActionSequence
                          cfg
                          (Legacy.puoSender policyOperation)
                          owner
                          (Legacy.puoCalls policyOperation) of
                          Left failure -> Legacy.respondFailure (Legacy.rrId request) failure
                          Right ()
                            | isCanaryGated nativeCfg request owner ->
                                Legacy.respondFailure (Legacy.rrId request) $
                                  Legacy.policyDenied "Trading Account owner is not enabled for the native AA canary"
                            | otherwise -> do
                                runtimeTrusted <- liftIO $
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
                                      gatewayState
                                      nativeCfg
                                      pool
                                      manager
                                      mSecurityContext
                                      clientKey
                                      owner
                                      request
                                      packedOperation
          _ -> handleOperationless nativeCfg pool manager clientKey request

handleOperation
  :: NativeGatewayState
  -> NativeAaConfig
  -> DbPool
  -> Manager
  -> Maybe NativeSecurityContext
  -> Text
  -> Text
  -> Legacy.RpcRequest
  -> Paymaster.PackedUserOperation
  -> ActionM ()
handleOperation gatewayState nativeCfg pool manager securityContext clientKey owner request operation =
  case Legacy.rrMethod request of
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
                              (now + naaValiditySeconds nativeCfg)
                              (naaMaxCostWei nativeCfg)
                              Paymaster.dummyPaymasterSignature
                      respondSuccess requestId $ paymasterResponse False envelope
    Legacy.GetPaymasterData ->
      issueSponsorship gatewayState nativeCfg pool securityContext clientKey owner request operation
    Legacy.SendUserOperation ->
      submitSponsoredOperation nativeCfg pool manager securityContext clientKey request operation
    _ -> relayToAlto nativeCfg manager request Nothing
 where
  requestId = Legacy.rrId request

issueSponsorship
  :: NativeGatewayState
  -> NativeAaConfig
  -> DbPool
  -> Maybe NativeSecurityContext
  -> Text
  -> Text
  -> Legacy.RpcRequest
  -> Paymaster.PackedUserOperation
  -> ActionM ()
issueSponsorship gatewayState nativeCfg pool securityContext clientKey owner request operation
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
                        then finishSponsorship signer nativeCfg pool context requestId operation authorization
                        else
                          Legacy.respondFailure requestId $
                            Legacy.policyDenied "This exact sponsorship request has already expired or completed"
                    Right Nothing -> reserveNew signer context requestKey
 where
  requestId = Legacy.rrId request
  reserveNew signer context requestKey = do
        now <- liftEpochSeconds
        let validAfter = max 0 $ now - 30
            validUntil = now + naaValiditySeconds nativeCfg
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
            snapshotReady <- liftIO $ revalidateSecurityContext context
            case snapshotReady of
              Left reason -> respondSecurityAttestationFailure requestId reason
              Right () -> do
                reserved <- liftDb $ withDb pool $ \conn -> reserveSponsorship conn nativeCfg draft
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
                    finishSponsorship signer nativeCfg pool context requestId operation authorization

finishSponsorship
  :: PaymasterSigner
  -> NativeAaConfig
  -> DbPool
  -> NativeSecurityContext
  -> Value
  -> Paymaster.PackedUserOperation
  -> SponsorshipAuthorization
  -> ActionM ()
finishSponsorship signer nativeCfg pool securityContext requestId operation authorization = do
  snapshotStillCanonical <- liftIO $ revalidateSecurityContext securityContext
  case snapshotStillCanonical of
    Left reason -> respondSecurityAttestationFailure requestId reason
    Right () -> do
      deliveryAllowed <-
        liftDb $
          withDb pool $ \conn ->
            isSponsorshipDeliveryAllowed conn nativeCfg $ saDigest authorization
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
                  else respondSuccess requestId $ paymasterResponse True finalEnvelope
          Nothing -> do
            signatureResult <- liftIO $ psSignDigest signer $ Paymaster.sponsorshipDigest operation unsignedEnvelope
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
                snapshotAfterSigning <- liftIO $ revalidateSecurityContext securityContext
                case snapshotAfterSigning of
                  Left reason -> respondSecurityAttestationFailure requestId reason
                  Right () -> do
                    let finalEnvelope = envelopeFromAuthorization nativeCfg authorization signature
                        signedOperation = Paymaster.applyPaymasterEnvelope operation finalEnvelope
                        expectedHash = encodeHex $ Paymaster.userOperationHash signedOperation
                        signatureText = encodeHex signature
                    stored <-
                      liftDb $
                        withDb pool $ \conn ->
                          storeSponsorshipSignature
                            conn
                            nativeCfg
                            (saDigest authorization)
                            signatureText
                            expectedHash
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
                                finishSponsorship signer nativeCfg pool securityContext requestId operation saved
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
              stored <- liftDb $ withDb pool $ \conn -> getSponsorshipByDigest conn digest
              case stored of
                Left _ -> respondNativeDbFailure requestId Legacy.SendUserOperation "authorization-read"
                Right Nothing ->
                  Legacy.respondFailure requestId $
                    Legacy.ProxyFailure status403 (-32001) "Forbidden" "SPONSORSHIP_NOT_AUTHORIZED" False
                Right (Just authorization)
                  | saClientKey authorization /= T.toLower clientKey
                      || saExpectedUserOperationHash authorization /= Just operationHash
                      || saSignature authorization /= Just signatureText
                      || saState authorization `notElem` ["signed", "submitted"] ->
                      Legacy.respondFailure requestId $
                        Legacy.ProxyFailure status403 (-32001) "Forbidden" "SPONSORSHIP_NOT_AUTHORIZED" False
                  | otherwise -> do
                      marked <-
                        liftDb $
                          withDb pool $ \conn ->
                            markSponsorshipSubmitted conn digest operationHash clientKey
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
  :: NativeAaConfig
  -> DbPool
  -> Manager
  -> Text
  -> Legacy.RpcRequest
  -> ActionM ()
handleOperationless nativeCfg _pool manager _clientKey request =
  relayToAlto nativeCfg manager request Nothing

requiresDualSecurity :: Legacy.RpcRequest -> Bool
requiresDualSecurity request =
  Legacy.rrMethod request
    `elem` [ Legacy.GetPaymasterStubData
           , Legacy.GetPaymasterData
           , Legacy.SendUserOperation
           ]

nativeSecurityContext
  :: NativeAaConfig
  -> NativeGatewayState
  -> EthClient
  -> IO (Either Legacy.ProxyFailure (Maybe NativeSecurityContext))
nativeSecurityContext nativeCfg gatewayState primaryClient =
  case ngsSecurityClient gatewayState of
    Nothing -> pure $ Left securityAttestationUnavailable
    Just secondaryClient -> do
      primaryChain <- attestRpcChain primaryClient
      secondaryChain <- attestRpcChain secondaryClient
      case (primaryChain, secondaryChain) of
        (Right (), Right ()) -> do
          snapshot <- readAgreedSecurityBlock primaryClient secondaryClient
          case snapshot of
            Left _ -> pure $ Left securityAttestationUnavailable
            Right header -> do
              let blockNumber = sbhNumber header
              primaryProfile <- attestProfileAt nativeCfg primaryClient blockNumber
              secondaryProfile <- attestProfileAt nativeCfg secondaryClient blockNumber
              finalHeader <-
                readAgreedSecurityHeaderAt primaryClient secondaryClient blockNumber
              case (primaryProfile, secondaryProfile, finalHeader) of
                (Right (), Right (), Right checkedHeader)
                  | checkedHeader == header ->
                      pure $ Right $ Just $ NativeSecurityContext primaryClient secondaryClient header
                _ -> pure $ Left securityAttestationUnavailable
        _ -> pure $ Left securityAttestationUnavailable

verifyAccountIdentityDual
  :: NativeSecurityContext
  -> Legacy.ParsedUserOperation
  -> IO (Either Legacy.ProxyFailure Text)
verifyAccountIdentityDual context operation = do
  let blockNumber = sbhNumber $ nscHeader context
  primary <-
    Legacy.verifyAccountIdentityAtBlock (nscPrimaryClient context) blockNumber operation
  secondary <-
    Legacy.verifyAccountIdentityAtBlock (nscSecondaryClient context) blockNumber operation
  pure $ case (primary, secondary) of
    (Right firstOwner, Right secondOwner)
      | T.toLower firstOwner == T.toLower secondOwner -> Right $ T.toLower firstOwner
      | otherwise -> Left securityAttestationUnavailable
    (Left firstFailure, Left secondFailure)
      | firstFailure == secondFailure
      , not (Legacy.pfRetryable firstFailure) -> Left firstFailure
      | otherwise -> Left securityAttestationUnavailable
    _ -> Left securityAttestationUnavailable

revalidateSecurityContext :: NativeSecurityContext -> IO (Either Text ())
revalidateSecurityContext context = do
  let blockNumber = sbhNumber $ nscHeader context
  primarySafe <- readSecurityHeader (nscPrimaryClient context) "safe"
  secondarySafe <- readSecurityHeader (nscSecondaryClient context) "safe"
  current <-
    readAgreedSecurityHeaderAt
      (nscPrimaryClient context)
      (nscSecondaryClient context)
      blockNumber
  now <- floor <$> getPOSIXTime
  pure $ do
    firstSafe <- primarySafe
    secondSafe <- secondarySafe
    let captured = nscHeader context
    unless (sbhNumber firstSafe >= blockNumber && sbhNumber secondSafe >= blockNumber) $
      Left "a security provider's safe head moved behind the authorization snapshot"
    when (sbhNumber firstSafe == blockNumber && firstSafe /= captured) $
      Left "primary safe head disagrees with the authorization snapshot"
    when (sbhNumber secondSafe == blockNumber && secondSafe /= captured) $
      Left "secondary safe head disagrees with the authorization snapshot"
    header <- current
    unless (header == captured) $
      Left "the agreed security block changed during request authorization"
    validateSecurityHeaderTime now header

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

authorizeRecoveryRead :: DbPool -> Text -> Legacy.RpcRequest -> ActionM Bool
authorizeRecoveryRead pool clientKey request =
  case Legacy.rrMethod request of
    method
      | method `elem`
          [ Legacy.GetUserOperationReceipt
          , Legacy.GetUserOperationByHash
          , Legacy.GetUserOperationStatus
          ] ->
          case Legacy.rrParams request of
            [String operationHash] -> do
              result <-
                liftDb $
                  withDb pool $ \conn ->
                    isRecoveryOperationAuthorized conn operationHash clientKey "alto"
              pure $ either (const False) id result
            _ -> pure False
    _ -> pure True

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
  result <- try @HttpException $ timeout 20_000_000 $ do
    base <- parseRequest $ T.unpack rpcUrl
    let upstreamRequest =
          base
            { method = "POST"
            , requestHeaders =
                [ ("Content-Type", "application/json")
                , ("Accept", "application/json")
                ]
            , requestBody = RequestBodyLBS $ encode $ Object $ Legacy.rrObject request
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
        Right value@(Object responseObject)
          | validAltoResponse request responseObject ->
              Right
                ( value
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
    Nothing -> do
      let blockNumber = sbhNumber $ nscHeader context
          sender = Legacy.puoSender operation
      primary <- readRuntimeCodeAt (nscPrimaryClient context) blockNumber sender
      secondary <- readRuntimeCodeAt (nscSecondaryClient context) blockNumber sender
      pure $ case (primary, secondary) of
        (Right firstCode, Right secondCode)
          | firstCode /= secondCode -> Left securityAttestationUnavailable
          | BS.null firstCode -> Left $ Legacy.policyDenied "Trading Account runtime code is missing"
          | encodeHex (keccak256 firstCode) == T.toLower (naaAccountCodeHash cfg) -> Right ()
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
  primaryChain <- attestRpcChain primaryClient
  secondaryChain <- attestRpcChain secondaryClient
  case (primaryChain, secondaryChain) of
    (Left err, _) -> pure $ Left $ "primary profile RPC: " <> err
    (_, Left err) -> pure $ Left $ "secondary profile RPC: " <> err
    (Right (), Right ()) -> do
      snapshot <- readAgreedSecurityBlock primaryClient secondaryClient
      case snapshot of
        Left err -> pure $ Left err
        Right header -> do
          primaryProfile <- attestProfileAt cfg primaryClient $ sbhNumber header
          secondaryProfile <- attestProfileAt cfg secondaryClient $ sbhNumber header
          verifiedHeader <-
            readAgreedSecurityHeaderAt primaryClient secondaryClient $ sbhNumber header
          pure $ do
            primaryProfile
            secondaryProfile
            finalHeader <- verifiedHeader
            unless (finalHeader == header) $
              Left "security snapshot changed during paymaster profile attestation"

attestProfileAt :: NativeAaConfig -> EthClient -> Integer -> IO (Either Text ())
attestProfileAt cfg client blockNumber = do
  let paymaster = naaPaymasterAddress cfg
  configuredEntryPoint <- readAddressAt client blockNumber paymaster "entryPoint()" []
  configuredPaused <- readBoolAt client blockNumber paymaster "paused()" []
  policy <- readBytes32At client blockNumber paymaster "policyId()" []
  accountHash <- readBytes32At client blockNumber paymaster "approvedAccountCodeHash()" []
  factory <- readAddressAt client blockNumber paymaster "accountFactory()" []
  factoryHash <- readBytes32At client blockNumber paymaster "accountFactoryCodeHash()" []
  implementation <- readAddressAt client blockNumber paymaster "accountImplementation()" []
  implementationHash <- readBytes32At client blockNumber paymaster "accountImplementationCodeHash()" []
  configuredSigner <- readAddressAt client blockNumber paymaster "sponsorSigner()" []
  maxCost <- readUintAt client blockNumber paymaster "maxSponsoredCost()" []
  maxValidity <- readUintAt client blockNumber paymaster "MAX_VALIDITY_WINDOW()" []
  factoryImplementation <-
    readAddressAt client blockNumber canonicalFactory "accountImplementation()" []
  liveEntryPointHash <- readCodeHashAt client blockNumber nativeEntryPoint
  livePaymasterHash <- readCodeHashAt client blockNumber paymaster
  liveFactoryHash <- readCodeHashAt client blockNumber canonicalFactory
  liveImplementationHash <- readCodeHashAt client blockNumber canonicalImplementation
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
  :: EthClient
  -> EthClient
  -> IO (Either Text SecurityBlockHeader)
readAgreedSecurityBlock primaryClient secondaryClient = do
  primarySafe <- readSecurityHeader primaryClient "safe"
  secondarySafe <- readSecurityHeader secondaryClient "safe"
  case (primarySafe, secondarySafe) of
    (Left err, _) -> pure $ Left $ "primary security RPC: " <> err
    (_, Left err) -> pure $ Left $ "secondary security RPC: " <> err
    (Right firstSafe, Right secondSafe) -> do
      let agreedNumber = min (sbhNumber firstSafe) (sbhNumber secondSafe)
      agreed <- readAgreedSecurityHeaderAt primaryClient secondaryClient agreedNumber
      now <- floor <$> getPOSIXTime
      pure $ do
        header <- agreed
        when (sbhNumber firstSafe == agreedNumber && firstSafe /= header) $
          Left "primary safe header disagrees with its explicit numeric header"
        when (sbhNumber secondSafe == agreedNumber && secondSafe /= header) $
          Left "secondary safe header disagrees with its explicit numeric header"
        validateSecurityHeaderTime now header
        Right header

validateSecurityHeaderTime :: Integer -> SecurityBlockHeader -> Either Text ()
validateSecurityHeaderTime now header = do
  when (sbhTimestamp header < now - gatewayMaxSafeLagSeconds) $
    Left "the dual-provider security snapshot is stale"
  when (sbhTimestamp header > now + gatewayMaxFutureSkewSeconds) $
    Left "the dual-provider security snapshot timestamp is in the future"

readAgreedSecurityHeaderAt
  :: EthClient
  -> EthClient
  -> Integer
  -> IO (Either Text SecurityBlockHeader)
readAgreedSecurityHeaderAt primaryClient secondaryClient blockNumber = do
  let blockTag = Paymaster.canonicalQuantity blockNumber
  primary <- readSecurityHeader primaryClient blockTag
  secondary <- readSecurityHeader secondaryClient blockTag
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

gatewayMaxSafeLagSeconds :: Integer
gatewayMaxSafeLagSeconds = 600

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
             , Legacy.SendUserOperation
             ]

ownerAllowedForNativeCanary :: NativeAaConfig -> Text -> Bool
ownerAllowedForNativeCanary cfg owner =
  naaGlobalRolloutEnabled cfg
    || T.toLower owner `elem` naaCanaryOwners cfg

validateHardEconomicCaps :: Paymaster.PackedUserOperation -> Either Text ()
validateHardEconomicCaps operation = do
  bounded "callGasLimit" 1 2_000_000 $ Paymaster.puoCallGasLimit operation
  bounded "verificationGasLimit" 1 1_000_000 $ Paymaster.puoVerificationGasLimit operation
  bounded "preVerificationGas" 1 1_000_000 $ Paymaster.puoPreVerificationGas operation
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
