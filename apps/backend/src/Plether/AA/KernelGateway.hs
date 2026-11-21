{-# LANGUAGE OverloadedStrings #-}

-- The fresh Kernel deployment has its own RPC surface. Only this server builds
-- the parameters passed to the private bundler and the KMS signer.
module Plether.AA.KernelGateway
  ( KernelGatewayConfig (..)
  , KernelSponsorshipConfig (..)
  , loadKernelPublicConfig
  , loadKernelGatewayConfig
  , parseKernelMaxSafeLag
  , kernelScope
  , handleKernelAaRpc
  , KernelTradingAccountProof (..)
  , resolveOwnedKernelTradingAccount
  , PrepareIntent (..)
  , parseKernelPrepareIntent
  , kernelCanFund
  , kernelFundingAdvice
  , classifyKernelEstimate
  , kernelCancellationCallData
  , minimumKernelReplacementFee
  , parseKernelCancellationOriginal
  , maxPrefund
  , validateKernelEconomicCaps
  , validateKernelWithdrawalMode
  , locateKernelNonceTransition
  , kernelNonceScanRange
  , kernelOperationDeadline
  , sponsorStakeReady
  , parseOwnerAdmission
  , kernelOwnerAdmitted
  , kernelOwnerMaySubmit
  ) where

import Control.Concurrent.Async (concurrently, mapConcurrently)
import Control.Concurrent.MVar (MVar, modifyMVar, newMVar)
import Control.Exception (try, SomeException)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE)
import Data.Aeson (FromJSON, Value (..), eitherDecode, eitherDecodeStrict', object, toJSON, withObject, (.:), (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Parser, parseEither)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (getPOSIXTime, posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import qualified Data.Vector as V
import Database.PostgreSQL.Simple (Connection)
import Network.HTTP.Client (Manager)
import Network.HTTP.Types.Status (status200, status400, status403, status413)
import Network.URI (URI (..), URIAuth (..), parseURI)
import qualified Plether.AA.ClientKey as ClientKey
import qualified Plether.AA.Gateway as Native
import qualified Plether.AA.Kernel as Kernel
import qualified Plether.AA.KernelReconciler as Reconciler
import qualified Plether.AA.Kms as Kms
import qualified Plether.AA.Paymaster as Paymaster
import qualified Plether.AA.Pimlico as Legacy
import qualified Plether.AA.Preparation as Preparation
import qualified Plether.AA.RecoveryCapability as Recovery
import qualified Plether.AA.Reconciler as Chain
import Plether.Config (AaRpcMode (..), Config (..), loadKernelSecurityRpc)
import Plether.Database (DbPool, withDb)
import Plether.Database.AaSponsorship (consumeAaRateLimit)
import qualified Plether.Database.KernelOperation as Db
import Plether.Ethereum.Abi (decodeAddress, decodeUint256, encodeAddress, encodeCall, encodeUint256, keccak256, selector)
import Plether.Ethereum.Client
  (CallParams (..), EthClient, RpcClientOptions (..), ethCallAtBlock, newClientWithOptions, rpcCall, withoutRpcFallback)
import qualified Plether.Perps.Manifest as Manifest
import System.Environment (lookupEnv)
import System.Timeout (timeout)
import Text.Read (readMaybe)
import Web.Scotty (ActionM, header, json, request, setHeader, status)

data KernelGatewayConfig = KernelGatewayConfig
  { kgcProfile :: Kernel.KernelProfile
  , kgcAltoRpcUrl :: Text
  , kgcOriginToken :: Text
  , kgcPrimaryRpcUrl :: Text
  , kgcSecondaryClient :: EthClient
  , kgcMaxSafeLagSeconds :: Integer
  , kgcEthModeEnabled :: Bool
  , kgcProtectionCommitsEnabled :: Bool
  , kgcSponsorship :: Maybe KernelSponsorshipConfig
  , kgcKmsSigner :: MVar (Maybe Kms.PaymasterSigner)
  , kgcAllowedOwners :: Maybe [Text]
  }

data KernelSponsorshipConfig = KernelSponsorshipConfig
  { kscPaymaster :: Text
  , kscPaymasterCodeHash :: Text
  , kscPolicyId :: Text
  , kscSignerAddress :: Text
  , kscKmsKeyId :: Text
  , kscDeploymentBlock :: Integer
  , kscDeploymentBlockHash :: Text
  , kscMinDepositWei :: Integer
  , kscVerificationGasLimit :: Integer
  , kscValiditySeconds :: Integer
  , kscBudget :: Db.KernelSponsorBudget
  , kscCanaryOwners :: [Text]
  , kscEnabled :: Bool
  }

data KernelTradingAccountProof = KernelTradingAccountProof
  { ktapAddress :: Text
  , ktapBlockNumber :: Integer
  , ktapBlockHash :: Text
  }
  deriving stock (Eq, Show)

type Gateway = ExceptT Legacy.ProxyFailure IO

data KernelRequest = KernelRequest
  { krId :: Value
  , krMethod :: Text
  , krParams :: [Value]
  }

data KernelRpcResult = KernelResult Value | KernelFundingRequired Value

data PrepareIntent = PrepareIntent
  { piId :: Text
  , piMode :: Db.PaymentMode
  , piSender :: Text
  , piCallData :: Text
  , piFactoryData :: Maybe Text
  }
  deriving stock (Eq)

loadKernelPublicConfig :: IO (Either Text (Maybe (Kernel.KernelProfile, Maybe KernelSponsorshipConfig, Integer)))
loadKernelPublicConfig = runExceptT $ do
  mProfile <- liftIO $ lookupEnv "AA_KERNEL_PROFILE_JSON"
  mSponsor <- liftIO $ lookupEnv "AA_KERNEL_SPONSORSHIP_JSON"
  sponsorEnabled <- ExceptT $ envBool "AA_KERNEL_SPONSORSHIP_ENABLED"
  lag <- liftIO $ lookupEnv "AA_KERNEL_MAX_SAFE_LAG_SECONDS"
  case mProfile of
    Nothing -> do
      when (sponsorEnabled || mSponsor /= Nothing) $
        throwE "Kernel sponsorship requires AA_KERNEL_PROFILE_JSON"
      pure Nothing
    Just profileJson -> do
      profile <- ExceptT $ pure $ decodeEnvironment "AA_KERNEL_PROFILE_JSON" profileJson
      maxLag <- maybe (pure 600) (ExceptT . pure . parseKernelMaxSafeLag) lag
      sponsor <- traverse (ExceptT . pure . parseSponsorship sponsorEnabled) mSponsor
      when (sponsorEnabled && maybe True (null . kscCanaryOwners) sponsor) $
        throwE "Enabled Kernel sponsorship requires a nonempty canary owner allowlist"
      pure $ Just (profile, sponsor, maxLag)

parseKernelMaxSafeLag :: String -> Either Text Integer
parseKernelMaxSafeLag raw = case readMaybe raw of
  Just value | value >= 60 && value <= 1_800 && raw == show value -> Right value
  _ -> Left "AA_KERNEL_MAX_SAFE_LAG_SECONDS must be between 60 and 1800"

loadKernelGatewayConfig :: IO (Either Text (Maybe KernelGatewayConfig))
loadKernelGatewayConfig = runExceptT $ do
  public <- ExceptT loadKernelPublicConfig
  ethEnabled <- ExceptT $ envBool "AA_KERNEL_ETH_ENABLED"
  protections <- ExceptT $ envBool "AA_KERNEL_PROTECTION_COMMITS_ENABLED"
  publicAdmission <- ExceptT $ envBool "AA_KERNEL_PUBLIC_ADMISSION"
  ownerList <- liftIO $ lookupEnv "AA_KERNEL_CANARY_OWNERS_JSON"
  case public of
    Nothing -> do
      when ethEnabled $ throwE "Kernel ETH payment requires AA_KERNEL_PROFILE_JSON"
      pure Nothing
    Just (profile, sponsor, maxLag) -> do
      allowedOwners <- ExceptT $ pure $ parseOwnerAdmission
        (ethEnabled || maybe False kscEnabled sponsor) publicAdmission ownerList
      origin <- requiredEnv "AA_PROXY_ORIGIN_TOKEN"
      unless (T.length origin == 64 && T.all lowerHex origin && T.any (/= '0') origin) $
        throwE "AA_PROXY_ORIGIN_TOKEN must be 64 nonzero lowercase hex characters"
      bundlerUrl <- requiredEnv "AA_KERNEL_ALTO_RPC_URL"
      case validBundlerTransportUri bundlerUrl of
        Just uri | null (uriQuery uri) -> pure ()
        _ -> throwE "AA_KERNEL_ALTO_RPC_URL must be an HTTP(S) URL without credentials or a query"
      (primary, secondary) <- ExceptT loadKernelSecurityRpc
      sponsorWithKey <- traverse
        (\s -> if kscEnabled s
          then (\key -> s { kscKmsKeyId = key }) <$> requiredEnv "AA_KERNEL_PAYMASTER_KMS_KEY_ID"
          else pure s)
        sponsor
      secondaryClient <- liftIO $ withoutRpcFallback <$>
        newClientWithOptions (RpcClientOptions secondary Nothing "kernel-gateway-secondary")
      signerCache <- liftIO $ newMVar Nothing
      pure $ Just $ KernelGatewayConfig profile bundlerUrl origin primary secondaryClient maxLag ethEnabled protections sponsorWithKey signerCache allowedOwners

-- A canary needs an explicit owner list for both gas modes. Broad admission
-- requires a separate deliberate switch and cannot coexist with a canary list.
parseOwnerAdmission :: Bool -> Bool -> Maybe String -> Either Text (Maybe [Text])
parseOwnerAdmission modesEnabled publicAdmission rawOwners =
  case (publicAdmission, rawOwners) of
    (True, Nothing) -> Right Nothing
    (True, Just _) -> Left "AA_KERNEL_PUBLIC_ADMISSION conflicts with AA_KERNEL_CANARY_OWNERS_JSON"
    (False, Nothing) | modesEnabled -> Left "Enabled Kernel payment requires AA_KERNEL_CANARY_OWNERS_JSON or public admission"
    (False, Nothing) -> Right $ Just []
    (False, Just raw) -> do
      supplied <- decodeEnvironment "AA_KERNEL_CANARY_OWNERS_JSON" raw :: Either Text [Text]
      owners <- traverse (maybe (Left "AA_KERNEL_CANARY_OWNERS_JSON contains an invalid address") Right . address) supplied
      unless (length owners <= 100 && (not modesEnabled || not (null owners))) $
        Left "AA_KERNEL_CANARY_OWNERS_JSON must contain 1 to 100 owners when payment is enabled"
      unless (length owners == length (Set.toList $ Set.fromList owners)) $
        Left "AA_KERNEL_CANARY_OWNERS_JSON contains duplicate owners"
      Right $ Just owners

kernelOwnerAdmitted :: Maybe [Text] -> Text -> Bool
kernelOwnerAdmitted Nothing _ = True
kernelOwnerAdmitted (Just owners) owner = owner `elem` owners

kernelOwnerMaySubmit :: Bool -> Maybe [Text] -> Text -> Bool
kernelOwnerMaySubmit alreadyAccepted owners owner =
  alreadyAccepted || kernelOwnerAdmitted owners owner

parseSponsorship :: Bool -> String -> Either Text KernelSponsorshipConfig
parseSponsorship enabled raw = do
  value <- decodeEnvironment "AA_KERNEL_SPONSORSHIP_JSON" raw
  either (const $ Left "AA_KERNEL_SPONSORSHIP_JSON is invalid") Right $ parseEither parser value
 where
  parser = withObject "Kernel sponsorship profile" $ \obj -> do
    exact obj ["paymaster","paymasterCodeHash","policyId","signerAddress","deploymentBlock","deploymentBlockHash","minDepositWei","verificationGasLimit","validitySeconds","budget","canaryOwners"]
    paymaster <- obj .: "paymaster" >>= validatedAddress
    codeHash <- obj .: "paymasterCodeHash" >>= validatedHash
    policy <- obj .: "policyId" >>= validatedHash
    signer <- obj .: "signerAddress" >>= validatedAddress
    block <- obj .: "deploymentBlock" >>= bounded 1 (2^(63::Int)-1)
    blockHash <- obj .: "deploymentBlockHash" >>= validatedHash
    deposit <- obj .: "minDepositWei" >>= bounded 1 (10^(18::Int))
    verification <- obj .: "verificationGasLimit" >>= bounded 1 1_000_000
    validity <- obj .: "validitySeconds" >>= bounded 60 570
    budget <- obj .: "budget" >>= parseBudget
    owners <- obj .: "canaryOwners" >>= traverse validatedAddress
    unless (length owners <= 100) $ fail "Too many canary owners"
    pure $ KernelSponsorshipConfig paymaster codeHash policy signer "" block blockHash deposit verification validity budget owners enabled
  parseBudget = withObject "Kernel sponsorship budget" $ \obj -> do
    exact obj ["maxCostWei","accountOutstandingWei","clientOutstandingWei","globalOutstandingWei","accountHourlyWei","globalHourlyWei","globalDailyWei"]
    cost <- obj .: "maxCostWei" >>= bounded 1 10_000_000_000_000_000
    account <- obj .: "accountOutstandingWei" >>= bounded cost 20_000_000_000_000_000
    client <- obj .: "clientOutstandingWei" >>= bounded cost 20_000_000_000_000_000
    global <- obj .: "globalOutstandingWei" >>= bounded (max account client) 100_000_000_000_000_000
    accountHourly <- obj .: "accountHourlyWei" >>= bounded cost 30_000_000_000_000_000
    globalHourly <- obj .: "globalHourlyWei" >>= bounded accountHourly 100_000_000_000_000_000
    globalDaily <- obj .: "globalDailyWei" >>= bounded globalHourly 250_000_000_000_000_000
    pure $ Db.KernelSponsorBudget cost account client global accountHourly globalHourly globalDaily
  exact values keys =
    unless (length values == length keys && all (`KM.member` values) keys) $ fail "Unexpected config fields"
  bounded :: Integer -> Integer -> Integer -> Parser Integer
  bounded lo hi n = if n >= lo && n <= hi then pure n else fail "Invalid value"
  validatedAddress rawAddress = maybe (fail "Invalid address") pure $ address rawAddress
  validatedHash rawHash = case fixedHex 32 rawHash of
    Just bytes | BS.any (/= 0) bytes -> pure $ toHex bytes
    _ -> fail "Invalid code or block hash"

requiredEnv :: String -> ExceptT Text IO Text
requiredEnv key = do
  value <- liftIO $ fmap (T.strip . T.pack) <$> lookupEnv key
  case value of
    Just trimmed | not (T.null trimmed) -> pure trimmed
    _ -> throwE $ T.pack key <> " is required"

decodeEnvironment :: FromJSON a => Text -> String -> Either Text a
decodeEnvironment name =
  either (const $ Left $ name <> " is invalid") Right . eitherDecodeStrict' . TE.encodeUtf8 . T.pack

envBool :: String -> IO (Either Text Bool)
envBool name = do
  value <- lookupEnv name
  pure $ case value of
    Nothing -> Right False
    Just "false" -> Right False
    Just "true" -> Right True
    _ -> Left $ T.pack name <> " must be true or false"

validBundlerTransportUri :: Text -> Maybe URI
validBundlerTransportUri value = do
  uri <- parseURI $ T.unpack value
  auth <- uriAuthority uri
  if uriScheme uri `elem` ["https:", "http:"]
     && null (uriUserInfo auth) && not (null $ uriRegName auth)
     && null (uriFragment uri)
    then Just uri
    else Nothing

kernelScope :: Kernel.KernelProfile -> Db.KernelScope
kernelScope profile = Db.KernelScope (Kernel.kpDeployment profile) (Kernel.kpChainId profile) (Kernel.kpEntryPoint profile)

handleKernelAaRpc :: KernelGatewayConfig -> Config -> Maybe DbPool -> EthClient -> Manager -> ActionM ()
handleKernelAaRpc gateway cfg pool ordinaryPrimary manager = do
  suppliedToken <- header "X-Plether-AA-Proxy-Token"
  ip <- header "CF-Connecting-IP"
  case (pool, (TL.toStrict <$> ip) >>= Legacy.validateClientIp) of
    _ | not (maybe False (Legacy.constantTimeTextEq $ kgcOriginToken gateway) (TL.toStrict <$> suppliedToken)) ->
      Legacy.respondFailure Null $ forbidden "PROXY_AUTH_FAILED" "Forbidden"
    (Nothing, _) -> Legacy.respondFailure Null databaseUnavailable
    (_, Nothing) -> Legacy.respondFailure Null $ Legacy.invalidRequest "A trusted CF-Connecting-IP header is required"
    (Just db, Just trustedIp) -> do
      let client = ClientKey.pseudonymousClientKey (kgcOriginToken gateway) trustedIp
      admitted <- liftIO $ runExceptT $ rateLimit gateway db "prebody" client "prebody" 120
      case admitted of
        Left failure -> Legacy.respondFailure Null failure
        Right () -> do
          wai <- request
          bounded <- liftIO $ Legacy.readBoundedRequestBody 65_536 wai
          case bounded of
            Left () -> Legacy.respondFailure Null $
              Legacy.ProxyFailure status413 (-32600) "Request body is too large" "INVALID_REQUEST" False
            Right bytes -> case eitherDecode bytes of
              Left _ -> Legacy.respondFailure Null $
                Legacy.ProxyFailure status400 (-32700) "Invalid JSON" "INVALID_REQUEST" False
              Right value -> case parseRequest value of
                Left failure -> Legacy.respondFailure Null failure
                Right rpc -> do
                  result <- liftIO $ runExceptT $ do
                    rateLimit gateway db "rpc" client "rpc" 90
                    unless (matchesPerpsRelease (kgcProfile gateway) cfg) $
                      throwE $ Legacy.unavailable "RELEASE_UNAVAILABLE" "Kernel trading is unavailable for this Perps release"
                    dispatch gateway cfg db (withoutRpcFallback ordinaryPrimary)
                      (withoutRpcFallback $ kgcSecondaryClient gateway) manager client rpc
                  case result of
                    Left failure -> Legacy.respondFailure (krId rpc) failure
                    Right (KernelFundingRequired advice) -> respondFundingAdvice (krId rpc) advice
                    Right (KernelResult value') -> do
                      setHeader "Cache-Control" "no-store"
                      status status200
                      json $ object ["jsonrpc" .= ("2.0" :: Text), "id" .= krId rpc, "result" .= value']

parseRequest :: Value -> Either Legacy.ProxyFailure KernelRequest
parseRequest (Object fields) = do
  unless (all (`elem` ["jsonrpc","id","method","params"]) (KM.keys fields) && KM.lookup "jsonrpc" fields == Just (String "2.0")) $
    Left $ Legacy.invalidRequest "Invalid JSON-RPC request"
  ident <- case KM.lookup "id" fields of
    Just value@(Number _) -> Right value
    Just value@(String t) | T.length t <= 100 -> Right value
    _ -> Left $ Legacy.invalidRequest "A JSON-RPC ID is required"
  method <- readText fields "method"
  params <- case KM.lookup "params" fields of
    Nothing -> pure []
    Just (Array items) -> pure $ V.toList items
    _ -> Left $ Legacy.invalidParams "params must be an array"
  pure $ KernelRequest ident method params
parseRequest _ = Left $ Legacy.invalidRequest "JSON-RPC request must be an object"

dispatch :: KernelGatewayConfig -> Config -> DbPool -> EthClient -> EthClient -> Manager -> Text -> KernelRequest -> Gateway KernelRpcResult
dispatch gateway cfg db primary secondary manager client rpc =
  case (krMethod rpc, krParams rpc) of
    ("eth_supportedEntryPoints", []) -> KernelResult <$> bundlerEntryPoint
    ("pimlico_getUserOperationGasPrice", []) -> KernelResult <$> gasPrice gateway manager
    ("plether_getKernelManifest", []) -> KernelResult <$> manifest
    ("plether_prepareUserOperation", [Object params]) -> do
      intent <- except $ parseKernelPrepareIntent profile params
      prepare gateway cfg db primary secondary manager client intent
    ("plether_sendUserOperation", [Object params]) -> KernelResult <$> submit gateway cfg db primary secondary manager client params
    ("plether_prepareUserOperationCancellation", [Object params]) ->
      KernelResult <$> prepareCancellation gateway db primary secondary manager client params
    ("plether_sendUserOperationCancellation", [Object params]) ->
      KernelResult <$> submitCancellation gateway db primary secondary manager client params
    ("plether_getUserOperationStatus", [Object params]) -> KernelResult <$> operationStatus gateway db primary secondary manager client params
    (method, _) | method `elem` ["eth_supportedEntryPoints", "pimlico_getUserOperationGasPrice",
        "plether_getKernelManifest","plether_prepareUserOperation", "plether_sendUserOperation","plether_getUserOperationStatus",
        "plether_prepareUserOperationCancellation", "plether_sendUserOperationCancellation"] ->
      throwE $ Legacy.invalidParams "Invalid Kernel request parameters"
    _ -> throwE $ Legacy.ProxyFailure status200 (-32601) "Method not found" "METHOD_NOT_ALLOWED" False
 where
  profile = kgcProfile gateway
  bundlerEntryPoint = do
    entries <- alto gateway manager "eth_supportedEntryPoints" []
    case entries of
      Array xs | String (Kernel.kpEntryPoint profile) `elem` fmap normalizeString xs ->
        pure $ toJSON [Kernel.kpEntryPoint profile]
      _ -> throwE $ Legacy.unavailable "ENTRY_POINT_UNAVAILABLE" "Alto does not support the pinned EntryPoint"
  manifest = do
    readiness <- liftIO $ runExceptT $ do
      _ <- bundlerEntryPoint
      snapshot <- agreedSnapshot gateway primary secondary
      attestBoth gateway primary secondary snapshot $ \rpcClient block ->
        runExceptT $ do
          ExceptT $ Kernel.attestKernelStackAtBlock profile rpcClient block
          ExceptT $ verifyPerpsTargetsAtBlock rpcClient block
      pure snapshot
    sponsorReady <- case (readiness, kgcSponsorship gateway) of
      (Right snapshot, Just sponsor) | kscEnabled sponsor -> do
        result <- liftIO $ runExceptT $
          verifySponsorAvailability gateway db primary secondary snapshot sponsor
        pure $ either (const False) (const True) result
      _ -> pure False
    pure $ object
      [ "version" .= (1 :: Int)
      , "profile" .= profile
      , "perpsRelease" .= Manifest.releaseVersion
      , "settlementToken" .= T.toLower (cfgPerpsSettlementToken cfg)
      , "marginClearinghouse" .= T.toLower (cfgPerpsMarginClearinghouse cfg)
      , "orderRouter" .= T.toLower (cfgPerpsOrderRouter cfg)
      , "cfdEngine" .= T.toLower (cfgPerpsCfdEngine cfg)
      , "positionProtectionBook" .= T.toLower Manifest.positionProtectionBookAddress
      , "protectionCommitsEnabled" .= kgcProtectionCommitsEnabled gateway
      , "paymaster" .= fmap kscPaymaster (kgcSponsorship gateway)
      , "readiness" .= object
          [ "eth" .= (kgcEthModeEnabled gateway && either (const False) (const True) readiness)
          , "sponsored" .= sponsorReady
          ]
      ]

parseKernelPrepareIntent :: Kernel.KernelProfile -> KM.KeyMap Value -> Either Legacy.ProxyFailure PrepareIntent
parseKernelPrepareIntent profile fields = do
  exactVersionedFields fields ["preparationId","mode","sender","callData"] ["factory","factoryData"]
  requirePinnedFactory profile fields
  ident <- readHash fields "preparationId"
  mode <- readMode fields
  sender <- readText fields "sender" >>= maybe (Left $ Legacy.invalidParams "Invalid sender") Right . address
  callData <- requireBytes fields "callData" 16_384
  factoryData <- case (KM.member "factory" fields, KM.member "factoryData" fields) of
    (False, False) -> pure Nothing
    (True, True) -> do
      _ <- readText fields "factory" >>= maybe (Left $ Legacy.invalidParams "Invalid factory") Right . address
      Just <$> requireBytes fields "factoryData" 2048
    _ -> Left $ Legacy.invalidParams "Factory fields must be paired"
  pure $ PrepareIntent ident mode sender callData factoryData

-- The factory comes from the reviewed profile. A client that supplies it must
-- agree with the pin, even though its value is never relayed directly.
requirePinnedFactory :: Kernel.KernelProfile -> KM.KeyMap Value -> Either Legacy.ProxyFailure ()
requirePinnedFactory profile fields =
  when (KM.member "factory" fields) $ do
    supplied <- readText fields "factory"
    unless (T.toLower supplied == Kernel.kpStaker profile) $
      Left $ Legacy.policyDenied "Transaction uses an unapproved Kernel Staker"

prepare :: KernelGatewayConfig -> Config -> DbPool -> EthClient -> EthClient -> Manager -> Text -> PrepareIntent -> Gateway KernelRpcResult
prepare gateway cfg db primary secondary manager client intent = do
  sponsor <- requireMode gateway (piMode intent)
  callData <- maybe (throwE $ Legacy.invalidParams "Invalid Kernel callData") pure $ hexBytes $ piCallData intent
  (calls, factoryOwner) <- except $
    Kernel.parseKernelCallIntent profile (piSender intent) callData
      (Kernel.kpStaker profile <$ piFactoryData intent) (piFactoryData intent)
  rateLimitAccount gateway db "account-prepare" (piSender intent) 12
  forMOptional sponsor $ \_ ->
    rateLimit gateway db "sponsored-prepare" client "sponsorship" 6
  snapshot <- agreedSnapshot gateway primary secondary
  owner <- attestBoth gateway primary secondary snapshot $
    verifyPerpsAccountAtBlock profile (piSender intent) factoryOwner
  unless (kernelOwnerAdmitted (kgcAllowedOwners gateway) owner) $
    throwE $ forbidden "KERNEL_CANARY_INELIGIBLE" "This Trading Account is not in the Kernel canary"
  except $ validateKernelActions gateway cfg (piMode intent) (piSender intent) owner calls
  forMOptional sponsor $ \s -> do
    unless (owner `elem` kscCanaryOwners s) $
      throwE $ forbidden "SPONSORSHIP_INELIGIBLE" "This Trading Account is not eligible for sponsorship"
    verifySponsorAvailability gateway db primary secondary snapshot s
  now <- seconds
  deadline <- except $ kernelOperationDeadline now calls
  existing <- dbAction db $ \conn ->
    Db.getKernelPreparationForClient conn scope (piId intent) client
  case existing of
    Just original -> do
      unless (originalMatches owner original) $ throwE preparationMismatch
      unless (Db.kpExpiresAt original > toUtc now) $ throwE preparationExpired
      operation <- except $ unpackStoredOperation $ Db.kpUnsignedOperation original
      pure $ KernelResult $ preparationResponse gateway now original operation
    Nothing -> case sponsor of
      Nothing -> do
        let expiry = min (now + 120) (maybe (now + 120) (subtract 30) deadline)
        (nonce, fees) <- quoteInputs snapshot
        (deposit, balance) <- readKernelFunds gateway primary secondary snapshot (piSender intent)
        result <- liftIO $ runExceptT $ quoteKernelOperation gateway manager snapshot intent nonce fees Nothing expiry
        let adviceFor operation
              | max 0 (deposit - maxPrefund operation) >= kernelDepositWithdrawal profile owner calls =
                  kernelFundingAdvice (maxPrefund operation) balance deposit (sum $ map Legacy.smartCallValue calls)
              | otherwise = Nothing
        case result of
          Left failure | failure == insufficientGasEth -> do
            -- A balance-only simulation can produce a top-up estimate. This branch
            -- never calls persistPreparation, even when the safe balance is large.
            estimated <- liftIO $ runExceptT $ quoteKernelOperationWithBalance gateway manager snapshot
              intent nonce fees Nothing expiry (Just $ max balance 200_000_000_000_000_000)
            revalidateSnapshot gateway primary secondary snapshot
            maybe (throwE failure) (pure . KernelFundingRequired) $ either (const Nothing) adviceFor estimated
          Left failure -> throwE failure
          Right operation -> case adviceFor operation of
            Just advice -> do
              revalidateSnapshot gateway primary secondary snapshot
              pure $ KernelFundingRequired advice
            Nothing -> do
              requireKernelFunds (maxPrefund operation) deposit balance (sum $ map Legacy.smartCallValue calls)
                (kernelDepositWithdrawal profile owner calls)
              persistPreparation snapshot owner now expiry operation
      Just s -> do
        reserved <- dbAction db $ \conn ->
          Db.getKernelSponsorshipForClient conn scope (kscPaymaster s) (piId intent) client
        authorization <- case reserved of
          Just existingAuth -> pure existingAuth
          Nothing -> do
            let expiry = min (now + kscValiditySeconds s) (fromMaybe (now + kscValiditySeconds s) deadline)
            (nonce, fees) <- quoteInputs snapshot
            packed <- quoteKernelOperation gateway manager snapshot intent nonce fees (Just s) expiry
            checkFunding gateway primary secondary snapshot (piMode intent) owner calls packed
            revalidateSnapshot gateway primary secondary snapshot
            let draft = Db.KernelSponsorshipDraft
                  scope (kscPaymaster s) (piId intent) (piId intent) client (piSender intent)
                  (Paymaster.puoNonce packed) (maxPrefund packed)
                  (max 0 (now - 30)) expiry (Object $ Paymaster.puoObject packed)
            result <- dbAction db $ \conn -> Db.reserveKernelSponsorship conn (kscBudget s) draft
            either (const $ throwE $ Legacy.unavailable "SPONSORSHIP_BUDGET_UNAVAILABLE" "Sponsorship capacity is unavailable") pure result
        let reservedNonce = Db.ksdNonce (Db.kspDraft authorization)
        liveNonce <- attestBoth gateway primary secondary snapshot $ \rpc block ->
          readNonce profile rpc block (piSender intent)
        unless (liveNonce == reservedNonce) $ throwE kernelNonceChanged
        (operation, expiresAt) <- signSponsorship gateway cfg db manager s owner intent authorization
        persistPreparation snapshot owner now expiresAt operation
 where
  profile = kgcProfile gateway
  scope = kernelScope profile
  originalMatches owner stored =
    Db.kpMode stored == piMode intent && Db.kpSender stored == piSender intent &&
    Db.kpOwner stored == owner && operationMatchesIntent intent (Db.kpUnsignedOperation stored)

  quoteInputs snapshot = do
    nonce <- attestBoth gateway primary secondary snapshot $ \rpc block ->
      readNonce profile rpc block (piSender intent)
    fees <- gasForPrepare gateway manager
    pure (nonce, fees)

  persistPreparation snapshot owner now expiresAt operation = do
    revalidateSnapshot gateway primary secondary snapshot
    persistedAt <- seconds
    unless (expiresAt > persistedAt) $ throwE preparationExpired
    let prepared = Db.KernelPreparation scope (piSender intent) owner (Paymaster.puoNonce operation) (piId intent)
          client (piMode intent) (Object $ Paymaster.puoObject operation) (toUtc expiresAt)
          (Just $ Native.sbhNumber snapshot)
    stored <- dbAction db $ \conn -> Db.storeKernelPreparation conn prepared
    case stored of
      Db.PreparationConflict -> throwE preparationMismatch
      _ -> pure $ KernelResult $ preparationResponse gateway now prepared operation

quoteKernelOperation :: KernelGatewayConfig -> Manager -> Native.SecurityBlockHeader -> PrepareIntent
  -> Integer -> (Integer, Integer) -> Maybe KernelSponsorshipConfig -> Integer -> Gateway Paymaster.PackedUserOperation
quoteKernelOperation gateway manager snapshot intent nonce fees maybeSponsor expiry =
  quoteKernelOperationWithBalance gateway manager snapshot intent nonce fees maybeSponsor expiry Nothing

quoteKernelOperationWithBalance :: KernelGatewayConfig -> Manager -> Native.SecurityBlockHeader -> PrepareIntent
  -> Integer -> (Integer, Integer) -> Maybe KernelSponsorshipConfig -> Integer -> Maybe Integer -> Gateway Paymaster.PackedUserOperation
quoteKernelOperationWithBalance gateway manager snapshot intent nonce (maxFee, priority) maybeSponsor expiry advisoryBalance = do
    unless (maxFee <= Native.nativeMaxFeeAllowance (Native.sbhBaseFeePerGas snapshot)) $
      throwE $ Legacy.policyDenied "Gas price exceeds the current reviewed allowance"
    let fields = baseFields profile intent nonce maxFee priority
    estimateFields <- case maybeSponsor of
      Nothing -> pure fields
      Just s -> do
        let fullFields = foldr (uncurry KM.insert) fields
              [ ("callGasLimit", String "0x1"), ("verificationGasLimit", String "0x1"), ("preVerificationGas", String "0x1") ]
        initial <- except $ mapParseFailure $ Paymaster.parsePackedUserOperation fullFields
        epoch <- seconds
        let envelope = sponsorEnvelope profile s (max 0 (epoch - 30)) expiry
              (Db.ksbMaxCostWei $ kscBudget s) Paymaster.dummyPaymasterSignature
            withStub = Paymaster.puoObject $ Paymaster.applyPaymasterEnvelope initial envelope
        pure $ foldr KM.delete withStub gasKeys
    let estimateMethod = "eth_estimateUserOperationGas"
        balanceOverride = case (maybeSponsor, advisoryBalance) of
          (Nothing, Just balance) -> [object [Key.fromText (piSender intent) .= object ["balance" .= Paymaster.canonicalQuantity balance]]]
          _ -> []
    request' <- except $ Preparation.internalRequest estimateMethod $
      [Object estimateFields, String (Kernel.kpEntryPoint profile)] <> balanceOverride
    (reply, _) <- ExceptT $ Native.forwardAlto manager (kgcAltoRpcUrl gateway) request'
    estimateResult <- except $ classifyKernelEstimate (piMode intent) reply
    estimates <- case estimateResult of
      Object values -> pure values
      _ -> throwE $ Legacy.unavailable "BUNDLER_UNAVAILABLE" "Alto did not return gas estimates"
    estimatedCallGas <- quoteQuantity estimates "callGasLimit"
    verificationGas <- quoteQuantity estimates "verificationGasLimit"
    preVerificationGas <- quoteQuantity estimates "preVerificationGas"
    callGas <- case maybeSponsor of
      Just _ -> except $ mapParseFailure $ Preparation.executionGasWithHeadroom estimatedCallGas
      Nothing -> pure $ estimatedCallGas + max 5_000 (estimatedCallGas `div` 10)
    let quotedUnsigned = KM.delete "signature" $ foldr (uncurry KM.insert) fields
          [("callGasLimit", String $ Paymaster.canonicalQuantity callGas),
           ("verificationGasLimit", String $ Paymaster.canonicalQuantity verificationGas),
           ("preVerificationGas", String $ Paymaster.canonicalQuantity preVerificationGas)]
    theQuote <- except $ mapParseFailure $ Paymaster.parsePackedUserOperation quotedUnsigned
    actualQuote <- case maybeSponsor of
      Nothing -> pure theQuote
      Just s -> do
        -- A stub envelope keeps the estimate and exact prefund protocols the
        -- same. Only the final signed envelope ever leaves the application.
        let cap = maxPrefund theQuote + kscVerificationGasLimit s * maxFee
            envelope = sponsorEnvelope profile s 0 expiry cap Paymaster.dummyPaymasterSignature
        pure $ Paymaster.applyPaymasterEnvelope theQuote envelope
    except $ mapParseFailure $ validateKernelEconomicCaps actualQuote
    forMOptional maybeSponsor $ \s ->
      unless (maxPrefund actualQuote <= Db.ksbMaxCostWei (kscBudget s)) $
        throwE $ Legacy.policyDenied "Estimated gas exceeds the reviewed sponsorship budget"
    pure actualQuote
 where profile = kgcProfile gateway

-- Only the exact pinned Alto prefund error can trigger an ETH advisory retry.
-- Preserve unrelated simulation/transport failures and all sponsored errors.
classifyKernelEstimate :: Db.PaymentMode -> Value -> Either Legacy.ProxyFailure Value
classifyKernelEstimate Db.PayWithEth (Object response)
  | not (KM.member "result" response)
  , Just (Object failure) <- KM.lookup "error" response
  , KM.lookup "code" failure == Just (Number (-32500))
  , KM.lookup "message" failure == Just (String "UserOperation reverted during simulation with reason: AA21 didn't pay prefund") =
      Left insufficientGasEth
classifyKernelEstimate _ response = Native.classifyAltoResult "eth_estimateUserOperationGas" response

baseFields :: Kernel.KernelProfile -> PrepareIntent -> Integer -> Integer -> Integer -> KM.KeyMap Value
baseFields profile intent nonce maxFee priority = KM.fromList $
  [("sender", String $ piSender intent), ("callData", String $ piCallData intent),
   ("nonce", String $ Paymaster.canonicalQuantity nonce),
   ("maxFeePerGas", String $ Paymaster.canonicalQuantity maxFee),
   ("maxPriorityFeePerGas", String $ Paymaster.canonicalQuantity priority),
   ("signature", String Legacy.dummySignature)] <>
  maybe [] (\factoryData -> [("factory", String $ Kernel.kpStaker profile), ("factoryData", String factoryData)]) (piFactoryData intent)

gasKeys :: [Key.Key]
gasKeys = ["callGasLimit","verificationGasLimit","preVerificationGas"]

signSponsorship :: KernelGatewayConfig -> Config -> DbPool -> Manager -> KernelSponsorshipConfig
  -> Text -> PrepareIntent -> Db.KernelSponsorship -> Gateway (Paymaster.PackedUserOperation, Integer)
signSponsorship gateway cfg db manager sponsor owner intent authorization = do
  let draft = Db.kspDraft authorization
      validAfter = Db.ksdValidAfter draft
      validUntil = Db.ksdValidUntil draft
      envelope = sponsorEnvelope profile sponsor validAfter validUntil
        (Db.ksdMaxCostWei draft) Paymaster.dummyPaymasterSignature
  unless (operationMatchesIntent intent (Db.ksdUnsignedOperation draft)) $ throwE preparationMismatch
  now <- seconds
  unless (validUntil - now > 30) $ throwE preparationExpired
  packed <- except $ unpackStoredOperation $ Db.ksdUnsignedOperation draft
  calls <- except $ Kernel.decodeKernelCalls $ Paymaster.puoCallData packed
  except $ validateKernelActions gateway cfg Db.Sponsored (piSender intent) owner calls
  except $ mapParseFailure $ validateKernelEconomicCaps packed
  unless (maxPrefund packed == Db.ksdMaxCostWei draft &&
          maxPrefund packed <= Db.ksbMaxCostWei (kscBudget sponsor)) $
    throwE sponsorshipNotAuthorized
  signature <- case Db.kspSignature authorization of
    Just stored -> do
      unless (Db.kspStatus authorization == "signed") $ throwE sponsorshipNotAuthorized
      maybe (throwE databaseUnavailable) pure $ fixedHex 65 stored
    Nothing -> do
      unless (Db.kspStatus authorization == "reserved") $ throwE sponsorshipNotAuthorized
      let digest = Paymaster.sponsorshipDigestFor (Kernel.kpChainId profile) (Kernel.kpEntryPoint profile) packed envelope
      signer <- liftIO cachedSigner
        >>= either (const $ throwE unavailableSigner) pure
      candidate <- liftIO (Kms.psSignDigest signer digest) >>= either (const $ throwE unavailableSigner) pure
      unless (BS.length candidate == 65) $ throwE unavailableSigner
      expectedHash <- except $ Kernel.kernelUserOperationHash profile $
        Paymaster.applyPaymasterEnvelope packed envelope {Paymaster.seSignature = candidate}
      saved <- dbAction db $ \conn -> Db.storeKernelSponsorshipSignature conn scope
        (kscPaymaster sponsor) (Db.ksdId draft) (Db.ksdClientKey draft) (toHex candidate) (toHex expectedHash)
      if saved then pure candidate else do
        -- Concurrent retries can obtain different KMS signatures for the same
        -- digest. Only the durably stored signature can be returned.
        existing <- dbAction db $ \conn ->
          Db.getKernelSponsorshipForClient conn scope (kscPaymaster sponsor) (Db.ksdId draft) (Db.ksdClientKey draft)
        case existing >>= signedSponsorshipSignature of
          Just exactSignature -> pure exactSignature
          Nothing -> throwE sponsorshipNotAuthorized
  let operation = Paymaster.applyPaymasterEnvelope packed envelope {Paymaster.seSignature = signature}
  nowAfterSigning <- seconds
  unless (validUntil - nowAfterSigning > 30) $ throwE preparationExpired
  pure (operation, validUntil - 30)
 where
  profile = kgcProfile gateway
  scope = kernelScope profile
  unavailableSigner = Legacy.unavailable "SPONSOR_SIGNER_UNAVAILABLE" "The sponsorship signer is unavailable"
  signedSponsorshipSignature record
    | Db.kspStatus record == "signed" = Db.kspSignature record >>= fixedHex 65
    | otherwise = Nothing
  -- Initialize only when signing is needed. The signer refreshes its ECS
  -- credentials, and a transient initialization failure can be retried.
  cachedSigner = modifyMVar (kgcKmsSigner gateway) $ \current -> case current of
    Just signer -> pure (current, Right signer)
    Nothing -> do
      result <- Kms.newKmsPaymasterSigner manager (kscKmsKeyId sponsor) (kscSignerAddress sponsor)
      pure (either (const Nothing) Just result, result)

sponsorEnvelope :: Kernel.KernelProfile -> KernelSponsorshipConfig -> Integer -> Integer -> Integer -> ByteString -> Paymaster.SponsorshipEnvelope
sponsorEnvelope profile sponsor validAfter validUntil cap signature =
  Paymaster.SponsorshipEnvelope
    (kscPaymaster sponsor) (kscVerificationGasLimit sponsor) 0 validUntil validAfter cap
    (fromMaybe BS.empty $ fixedHex 32 $ kscPolicyId sponsor)
    (fromMaybe BS.empty $ fixedHex 32 $ Kernel.kpAccountCodeHash profile)
    signature

maxPrefund :: Paymaster.PackedUserOperation -> Integer
maxPrefund op = Paymaster.puoMaxFeePerGas op *
  (Paymaster.puoCallGasLimit op + Paymaster.puoVerificationGasLimit op + Paymaster.puoPreVerificationGas op +
   fromMaybe 0 (Paymaster.puoPaymasterVerificationGasLimit op) + fromMaybe 0 (Paymaster.puoPaymasterPostOpGasLimit op))

-- Nitro charges L1 data in preVerificationGas. Keep the higher gas-unit cap
-- Kernel-specific, while retaining a fixed ETH prefund ceiling, the configured
-- sponsorship budget (checked by the sponsor path), and the live fee checks.
validateKernelEconomicCaps :: Paymaster.PackedUserOperation -> Either Text ()
validateKernelEconomicCaps operation = do
  bounded "callGasLimit" 1 Preparation.sepoliaExecutionGasCap callGas
  bounded "verificationGasLimit" 1 1_000_000 verificationGas
  bounded "preVerificationGas" 1 12_000_000 preVerificationGas
  bounded "paymasterVerificationGasLimit" 0 1_000_000 sponsorVerificationGas
  bounded "paymasterPostOpGasLimit" 0 0 sponsorPostOpGas
  bounded "aggregateGas" 1 17_000_000 (callGas + verificationGas + preVerificationGas +
    sponsorVerificationGas + sponsorPostOpGas)
  bounded "maxFeePerGas" 1 10_000_000_000 (Paymaster.puoMaxFeePerGas operation)
  bounded "maxPriorityFeePerGas" 0 2_000_000_000 (Paymaster.puoMaxPriorityFeePerGas operation)
  when (Paymaster.puoPaymaster operation == Nothing) $
    bounded "ETH prefund" 1 50_000_000_000_000_000 (maxPrefund operation)
 where
  callGas = Paymaster.puoCallGasLimit operation
  verificationGas = Paymaster.puoVerificationGasLimit operation
  preVerificationGas = Paymaster.puoPreVerificationGas operation
  sponsorVerificationGas = fromMaybe 0 (Paymaster.puoPaymasterVerificationGasLimit operation)
  sponsorPostOpGas = fromMaybe 0 (Paymaster.puoPaymasterPostOpGasLimit operation)
  bounded label lower upper value =
    unless (value >= lower && value <= upper) $
      Left $ "Kernel UserOperation " <> label <> " exceeds the reviewed gas bounds"

preparationResponse :: KernelGatewayConfig -> Integer -> Db.KernelPreparation -> Paymaster.PackedUserOperation -> Value
preparationResponse gateway now prepared operation = object $ preparationResponseFields gateway now prepared operation

preparationResponseFields :: KernelGatewayConfig -> Integer -> Db.KernelPreparation -> Paymaster.PackedUserOperation -> [(Key.Key, Value)]
preparationResponseFields gateway now prepared operation =
  [ "version" .= (1 :: Int)
  , "preparationId" .= Db.kpId prepared
  , "mode" .= renderMode (Db.kpMode prepared)
  , "entryPoint" .= Kernel.kpEntryPoint profile
  , "operation" .= Object (Paymaster.puoObject operation)
  , "expiresAt" .= fromUtc (Db.kpExpiresAt prepared)
  , "maxNetworkGasWei" .= Paymaster.canonicalQuantity (maxPrefund operation)
  , "recoveryToken" .= Recovery.issue (kgcOriginToken gateway) (recoveryDomain profile)
      now (Db.kpId prepared) (Db.kpClientKey prepared)
  ]
 where profile = kgcProfile gateway

operationMatchesIntent :: PrepareIntent -> Value -> Bool
operationMatchesIntent intent (Object fields) =
  KM.lookup "sender" fields == Just (String $ piSender intent) &&
  KM.lookup "callData" fields == Just (String $ piCallData intent) &&
  KM.lookup "factoryData" fields == fmap String (piFactoryData intent)
operationMatchesIntent _ _ = False

submit :: KernelGatewayConfig -> Config -> DbPool -> EthClient -> EthClient -> Manager -> Text -> KM.KeyMap Value -> Gateway Value
submit gateway cfg db primary secondary manager client fields = do
  except $ exactVersionedFields fields ["preparationId","mode","operation"] ["recoveryToken"]
  ident <- except $ readHash fields "preparationId"
  mode <- except $ readMode fields
  raw <- case KM.lookup "operation" fields of
    Just (Object values) -> pure values
    _ -> throwE $ Legacy.invalidParams "A signed UserOperation is required"
  parsed <- except $ Kernel.parseKernelOperation profile (toKernelMode mode) Kernel.KernelSubmit raw
  let operation = Kernel.koPacked parsed
      sender = Paymaster.puoSender operation
  expectedHash <- toHex <$> except (Kernel.kernelUserOperationHash profile operation)
  rateLimitAccount gateway db "account-submit" sender 20
  now <- seconds
  let replayClient = kernelRecoveryClient gateway now client ident fields
  existing <- dbAction db $ \conn -> Db.getKernelOperationForClient conn scope expectedHash replayClient
  case existing of
    Just saved | Db.koHash saved == expectedHash -> do
      unless (Db.koSignedOperation saved == Object raw && Db.kpId (Db.koPreparation saved) == ident &&
              Db.kpMode (Db.koPreparation saved) == mode) $
        throwE preparationMismatch
      when (Db.koCancellationOnly saved) $ throwE cancellationOnly
      -- The exact operation has already been durably accepted. Only a fresh,
      -- normal retry on the same mode can relay again. Status remains
      -- available independently of mode readiness or quote expiry.
      case Db.koTerminalEvidence saved of
        Just _ -> pure (String expectedHash)
        Nothing -> forward replayClient True ident mode sender now expectedHash parsed raw
    -- A recovery token locates accepted exact bytes. Fresh acceptance uses the
    -- trusted request's own client identity and its preparation.
    _ -> forward client False ident mode sender now expectedHash parsed raw
 where
  profile = kgcProfile gateway
  scope = kernelScope profile
  forward preparedClient alreadyAccepted ident mode sender now expectedHash parsed raw = do
    -- Pausing new issuance must leave already issued sponsor authorizations
    -- submit-capable whenever their pinned onchain paymaster is still live.
    case mode of
      Db.PayWithEth -> unless (kgcEthModeEnabled gateway) $ throwE modeDisabled
      Db.Sponsored -> pure ()
    preparation <- dbAction db (\conn -> Db.getKernelPreparationForClient conn scope ident preparedClient)
      >>= maybe (throwE $ forbidden "PREPARATION_NOT_AUTHORIZED" "Preparation is unavailable") pure
    let operation = Kernel.koPacked parsed
        calls = Kernel.koCalls parsed
    unless (Db.kpMode preparation == mode && Db.kpSender preparation == sender &&
            Db.kpNonce preparation == Paymaster.puoNonce operation &&
            Db.kpUnsignedOperation preparation == Object (KM.delete "signature" raw)) $
      throwE preparationMismatch
    unless (alreadyAccepted || Db.kpExpiresAt preparation > toUtc now) $ throwE preparationExpired
    snapshot <- agreedSnapshot gateway primary secondary
    owner <- attestBoth gateway primary secondary snapshot $
      verifyPerpsAccountAtBlock profile sender (Kernel.koFactoryOwner parsed)
    unless alreadyAccepted $ do
      nonce <- attestBoth gateway primary secondary snapshot $ \rpc block -> readNonce profile rpc block sender
      unless (nonce == Paymaster.puoNonce operation) $ throwE kernelNonceChanged
    unless (Db.kpOwner preparation == owner) $ throwE preparationMismatch
    unless (kernelOwnerMaySubmit alreadyAccepted (kgcAllowedOwners gateway) owner) $
      throwE $ forbidden "KERNEL_CANARY_INELIGIBLE" "This Trading Account is not in the Kernel canary"
    ExceptT $ Kernel.verifyKernelSignature profile owner operation
    except $ validateKernelActions gateway cfg mode sender owner calls
    _ <- except $ kernelOperationDeadline now calls
    except $ mapParseFailure $ validateKernelEconomicCaps operation
    unless (Paymaster.puoMaxFeePerGas operation <= Native.nativeMaxFeeAllowance (Native.sbhBaseFeePerGas snapshot)) $
      throwE $ Legacy.policyDenied "Gas price exceeds the current reviewed allowance"
    checkFunding gateway primary secondary snapshot mode owner calls operation
    case mode of
      Db.PayWithEth -> pure ()
      Db.Sponsored -> do
        sponsor <- maybe (throwE modeDisabled) pure $ kgcSponsorship gateway
        unless (Paymaster.puoPaymaster operation == Just (kscPaymaster sponsor)) $ throwE preparationMismatch
        attestBoth gateway primary secondary snapshot (attestSponsorAt profile sponsor)
        authorized <- dbAction db $ \conn -> Db.getKernelSponsorshipForClient conn scope (kscPaymaster sponsor) ident preparedClient
        case authorized of
          Just auth | Db.kspUserOperationHash auth == Just expectedHash
            , Db.kspStatus auth == "signed"
            , let draft = Db.kspDraft auth
            , Db.ksdValidAfter draft < now
            , Db.ksdValidUntil draft > now + 30
            , maxPrefund operation == Db.ksdMaxCostWei draft
            , maxPrefund operation <= Db.ksbMaxCostWei (kscBudget sponsor) -> pure ()
          _ -> throwE sponsorshipNotAuthorized
    revalidateSnapshot gateway primary secondary snapshot
    accepted <- dbAction db $ \conn -> Db.acceptKernelOperation conn scope ident preparedClient expectedHash (Object raw)
    case accepted of
      Db.OperationAlreadyFinal -> pure (String expectedHash)
      Db.OperationAccepted -> relayKernelOperation gateway manager expectedHash raw
      Db.OperationReplay -> relayKernelOperation gateway manager expectedHash raw
      Db.OperationPreparationExpired -> throwE preparationExpired
      Db.OperationLaneBusy -> throwE $ forbidden "OPERATION_PENDING" "Resolve the pending operation before submitting again"
      _ -> throwE preparationMismatch

relayKernelOperation :: KernelGatewayConfig -> Manager -> Text -> KM.KeyMap Value -> Gateway Value
relayKernelOperation gateway manager expectedHash raw = do
  forwarded <- alto gateway manager "eth_sendUserOperation"
    [Object raw, String (Kernel.kpEntryPoint $ kgcProfile gateway)]
  case forwarded of
    String actual | T.toLower actual == expectedHash -> pure $ String expectedHash
    _ -> throwE $ Legacy.unavailable "BUNDLER_HASH_MISMATCH" "Alto returned an unexpected UserOperation hash"

-- Only this dedicated route can quote a no-op. The original stays pending
-- even when a replacement is durably accepted or acknowledged by Alto.
prepareCancellation :: KernelGatewayConfig -> DbPool -> EthClient -> EthClient -> Manager -> Text -> KM.KeyMap Value -> Gateway Value
prepareCancellation gateway db primary secondary manager client fields = do
  except $ exactVersionedFields fields cancellationKeys ["recoveryToken", "originalOperation"]
  (original, preparationId) <- cancellationAnchor gateway db client fields $
    createCancellationAnchor gateway db primary secondary fields
  case KM.lookup "originalOperation" fields of
    Just prior | prior /= Db.koSignedOperation original -> throwE preparationMismatch
    _ -> pure ()
  let prepared = Db.koPreparation original
      sender = Db.kpSender prepared
      owner = Db.kpOwner prepared
      nonce = Db.kpNonce prepared
      calls = [Legacy.SmartCall sender 0 BS.empty]
  unless (Db.koState original == "pending") $ throwE cancellationUnavailable
  rateLimitAccount gateway db "account-cancellation-prepare" sender 6
  now <- seconds
  existing <- dbAction db $ \conn -> Db.getKernelCancellation conn original preparationId
  saved <- case existing of
    Just quote -> pure quote
    Nothing -> do
      prior <- except $ unpackStoredOperation $ Db.koSignedOperation original
      snapshot <- agreedSnapshot gateway primary secondary
      factoryData <- cancellationAccount gateway primary secondary snapshot prepared
      (networkMaxFee, networkPriority) <- gasForPrepare gateway manager
      let priority = max networkPriority $ minimumKernelReplacementFee $ Paymaster.puoMaxPriorityFeePerGas prior
          maxFee = maximum [priority, networkMaxFee, minimumKernelReplacementFee (Paymaster.puoMaxFeePerGas prior)]
          intent = PrepareIntent preparationId Db.PayWithEth sender (kernelCancellationCallData sender) factoryData
          expiry = now + 120
      operation <- quoteKernelOperation gateway manager snapshot intent nonce (maxFee, priority) Nothing expiry
      checkFunding gateway primary secondary snapshot Db.PayWithEth owner calls operation
      revalidateSnapshot gateway primary secondary snapshot
      let draft = Db.KernelCancellation preparationId (Object $ Paymaster.puoObject operation) (toUtc expiry) Nothing Nothing
      outcome <- dbAction db $ \conn -> Db.storeKernelCancellation conn original draft
      case outcome of
        Db.PreparationStored -> pure draft
        Db.PreparationAlreadyStored -> dbAction db (\conn -> Db.getKernelCancellation conn original preparationId)
          >>= maybe (throwE cancellationUnavailable) pure
        Db.PreparationConflict -> throwE cancellationUnavailable
  responseAt <- seconds
  unless (Db.kcExpiresAt saved > toUtc responseAt) $ throwE preparationExpired
  operation <- except $ unpackStoredOperation $ Db.kcUnsignedOperation saved
  let quote = prepared { Db.kpId = preparationId, Db.kpUnsignedOperation = Db.kcUnsignedOperation saved,
                        Db.kpExpiresAt = Db.kcExpiresAt saved }
  -- The quote carries its own capability to bind the published preparation.
  -- Cancellation submission and status recover the original account lane and
  -- therefore require the capability issued for the original preparation.
  pure $ object $ ("cancelsUserOperationHash" .= Db.koHash original) : preparationResponseFields gateway responseAt quote operation

submitCancellation :: KernelGatewayConfig -> DbPool -> EthClient -> EthClient -> Manager -> Text -> KM.KeyMap Value -> Gateway Value
submitCancellation gateway db primary secondary manager client fields = do
  except $ exactVersionedFields fields ("operation" : cancellationKeys) ["recoveryToken"]
  (original, preparationId) <- cancellationAnchor gateway db client fields $ \_ _ -> throwE operationNotAuthorized
  let prepared = Db.koPreparation original
      sender = Db.kpSender prepared
      owner = Db.kpOwner prepared
      profile = kgcProfile gateway
  rateLimitAccount gateway db "account-cancellation-submit" sender 10
  raw <- case KM.lookup "operation" fields of
    Just (Object values) -> pure values
    _ -> throwE $ Legacy.invalidParams "A signed cancellation UserOperation is required"
  parsed <- except $ Kernel.parseKernelOperation profile Kernel.KernelEthPaid Kernel.KernelSubmit raw
  let operation = Kernel.koPacked parsed
  saved <- dbAction db (\conn -> Db.getKernelCancellation conn original preparationId)
    >>= maybe (throwE cancellationUnavailable) pure
  unless (Paymaster.puoSender operation == sender && Paymaster.puoNonce operation == Db.kpNonce prepared &&
          toHex (Paymaster.puoCallData operation) == kernelCancellationCallData sender &&
          Db.kcUnsignedOperation saved == Object (KM.delete "signature" raw)) $
    throwE preparationMismatch
  expectedHash <- toHex <$> except (Kernel.kernelUserOperationHash profile operation)
  case Db.kcSignedOperation saved of
    Just previous -> unless (previous == Object raw && Db.kcHash saved == Just expectedHash) $ throwE preparationMismatch
    Nothing -> do
      now <- seconds
      unless (Db.kcExpiresAt saved > toUtc now) $ throwE preparationExpired
  if Db.koState original /= "pending"
    then case Db.kcHash saved of
      Just hash | hash == expectedHash -> pure $ String hash
      _ -> throwE cancellationUnavailable
    else do
      snapshot <- agreedSnapshot gateway primary secondary
      factoryData <- cancellationAccount gateway primary secondary snapshot prepared
      unless ((toHex <$> Paymaster.puoFactoryData operation) == factoryData) $ throwE preparationMismatch
      ExceptT $ Kernel.verifyKernelSignature profile owner operation
      except $ mapParseFailure $ validateKernelEconomicCaps operation
      unless (Paymaster.puoMaxFeePerGas operation <= Native.nativeMaxFeeAllowance (Native.sbhBaseFeePerGas snapshot)) $
        throwE $ Legacy.policyDenied "Gas price exceeds the current reviewed allowance"
      checkFunding gateway primary secondary snapshot Db.PayWithEth owner (Kernel.koCalls parsed) operation
      revalidateSnapshot gateway primary secondary snapshot
      accepted <- dbAction db $ \conn -> Db.acceptKernelCancellation conn original preparationId expectedHash (Object raw)
      case accepted of
        Db.OperationAlreadyFinal -> do
          final <- dbAction db $ \conn -> Db.getAcceptedKernelCancellation conn original
          case final of
            Just exact | Db.kcPreparationId exact == preparationId
              , Db.kcHash exact == Just expectedHash
              , Db.kcSignedOperation exact == Just (Object raw) -> pure $ String expectedHash
            _ -> throwE cancellationUnavailable
        Db.OperationAccepted -> relayKernelOperation gateway manager expectedHash raw
        Db.OperationReplay -> relayKernelOperation gateway manager expectedHash raw
        Db.OperationPreparationExpired -> throwE preparationExpired
        _ -> throwE cancellationUnavailable

cancellationKeys :: [Key.Key]
cancellationKeys = ["originalPreparationId", "originalUserOperationHash", "preparationId"]

cancellationAnchor :: KernelGatewayConfig -> DbPool -> Text -> KM.KeyMap Value
  -> (Text -> Text -> Gateway Db.KernelOperation) -> Gateway (Db.KernelOperation, Text)
cancellationAnchor gateway db client fields missing = do
  unless (kgcEthModeEnabled gateway) $ throwE modeDisabled
  ident <- except $ readHash fields "originalPreparationId"
  operationHash <- except $ readHash fields "originalUserOperationHash"
  preparationId <- except $ readHash fields "preparationId"
  unless (preparationId /= ident) $ throwE preparationMismatch
  found <- findAuthorizedKernelOperation gateway db client ident operationHash fields
  original <- maybe (missing ident operationHash) pure found
  unless (Db.kpMode (Db.koPreparation original) == Db.PayWithEth) $ throwE cancellationUnavailable
  pure (original, preparationId)

cancellationOnly :: Legacy.ProxyFailure
cancellationOnly = forbidden "CANCELLATION_PENDING" "The original UserOperation is locked for cancellation"

-- A recovery token locates a preparation. Only the owner's exact original
-- signed operation authorizes a new cancellation-only anchor for that quote.
-- This path never relays the original or settles the account nonce.
createCancellationAnchor :: KernelGatewayConfig -> DbPool -> EthClient -> EthClient -> KM.KeyMap Value
  -> Text -> Text -> Gateway Db.KernelOperation
createCancellationAnchor gateway db primary secondary fields ident operationHash = do
  now <- seconds
  recovered <- maybe (throwE operationNotAuthorized) pure $ kernelRecoverySubject gateway now ident fields
  raw <- case KM.lookup "originalOperation" fields of
    Just (Object operation) -> pure operation
    _ -> throwE operationNotAuthorized
  prepared <- dbAction db (\conn -> Db.getKernelPreparationForClient conn scope ident recovered)
    >>= maybe (throwE operationNotAuthorized) pure
  operation <- except $ parseKernelCancellationOriginal profile prepared operationHash raw
  rateLimitAccount gateway db "account-cancellation-anchor" (Db.kpSender prepared) 6
  ExceptT $ Kernel.verifyKernelSignature profile (Db.kpOwner prepared) operation
  snapshot <- agreedSnapshot gateway primary secondary
  _ <- cancellationAccount gateway primary secondary snapshot prepared
  revalidateSnapshot gateway primary secondary snapshot
  accepted <- dbAction db $ \conn -> Db.acceptKernelCancellationAnchor conn scope ident recovered operationHash (Object raw)
  let findExact = dbAction db (\conn -> Db.getKernelOperationForClient conn scope operationHash recovered)
        >>= maybe (throwE operationNotAuthorized) pure
  case accepted of
    Db.OperationAccepted -> findExact
    Db.OperationReplay -> findExact
    Db.OperationAlreadyFinal -> findExact
    Db.OperationLaneBusy -> throwE $ forbidden "OPERATION_PENDING" "Resolve the pending operation before submitting again"
    _ -> throwE preparationMismatch
 where
  profile = kgcProfile gateway
  scope = kernelScope profile

parseKernelCancellationOriginal :: Kernel.KernelProfile -> Db.KernelPreparation -> Text -> KM.KeyMap Value
  -> Either Legacy.ProxyFailure Paymaster.PackedUserOperation
parseKernelCancellationOriginal profile prepared operationHash raw = do
  operation <- Kernel.koPacked <$> Kernel.parseKernelOperation profile Kernel.KernelEthPaid Kernel.KernelSubmit raw
  unless (Db.kpMode prepared == Db.PayWithEth && Db.kpScope prepared == kernelScope profile &&
          Db.kpSender prepared == Paymaster.puoSender operation && Db.kpNonce prepared == Paymaster.puoNonce operation &&
          Db.kpUnsignedOperation prepared == Object (KM.delete "signature" raw)) $
    Left preparationMismatch
  expected <- toHex <$> Kernel.kernelUserOperationHash profile operation
  unless (operationHash == expected) $ Left preparationMismatch
  pure operation

cancellationAccount :: KernelGatewayConfig -> EthClient -> EthClient -> Native.SecurityBlockHeader
  -> Db.KernelPreparation -> Gateway (Maybe Text)
cancellationAccount gateway primary secondary snapshot prepared =
  attestBoth gateway primary secondary snapshot $ \rpc block -> runExceptT $ do
    let profile = kgcProfile gateway
        sender = Db.kpSender prepared
        rootOwner = Db.kpOwner prepared
    code <- liftIO $ rpcCall rpc "eth_getCode" $ toJSON [sender, Paymaster.canonicalQuantity block]
    undeployed <- case code of
      Right (String value) | Just bytecode <- hexBytes value -> pure $ BS.null bytecode
      _ -> throwE profileUnavailable
    let counterfactualOwner = if undeployed then Just rootOwner else Nothing
    owner <- ExceptT $ verifyPerpsAccountAtBlock profile sender counterfactualOwner rpc block
    nonce <- ExceptT $ readNonce profile rpc block sender
    unless (owner == rootOwner && nonce == Db.kpNonce prepared) $ throwE cancellationUnavailable
    traverse (except . Kernel.kernelFactoryArgs profile) counterfactualOwner

kernelCancellationCallData :: Text -> Text
kernelCancellationCallData sender = toHex $ encodeCall "execute(bytes32,bytes)"
  [encodeUint256 0, encodeUint256 64, encodeUint256 52, BS.drop 12 (encodeAddress sender),
   encodeUint256 0, BS.replicate 12 0]

-- Alto compares both gas fees to 110% of the outstanding operation. Round
-- up and also require an increase when the original fee was zero.
minimumKernelReplacementFee :: Integer -> Integer
minimumKernelReplacementFee fee = max (fee + 1) ((110 * fee + 99) `div` 100)

findAuthorizedKernelOperation :: KernelGatewayConfig -> DbPool -> Text -> Text -> Text -> KM.KeyMap Value -> Gateway (Maybe Db.KernelOperation)
findAuthorizedKernelOperation gateway db client ident operationHash fields = do
  now <- seconds
  let profile = kgcProfile gateway
      recoveryClient = kernelRecoveryClient gateway now client ident fields
  saved <- dbAction db $ \conn -> Db.getKernelOperationForClient conn (kernelScope profile) operationHash recoveryClient
  case saved of
    Just original | Db.kpId (Db.koPreparation original) /= ident -> throwE operationNotAuthorized
    _ -> pure saved

operationNotAuthorized :: Legacy.ProxyFailure
operationNotAuthorized = forbidden "RECOVERY_NOT_AUTHORIZED" "This UserOperation is unavailable"

kernelRecoveryClient :: KernelGatewayConfig -> Integer -> Text -> Text -> KM.KeyMap Value -> Text
kernelRecoveryClient gateway now client ident fields =
  fromMaybe client $ kernelRecoverySubject gateway now ident fields

kernelRecoverySubject :: KernelGatewayConfig -> Integer -> Text -> KM.KeyMap Value -> Maybe Text
kernelRecoverySubject gateway now ident fields = case KM.lookup "recoveryToken" fields of
  Just (String proof) | T.length proof <= 320 ->
    Recovery.verify (kgcOriginToken gateway) (recoveryDomain $ kgcProfile gateway) now ident proof
  _ -> Nothing

cancellationUnavailable :: Legacy.ProxyFailure
cancellationUnavailable = forbidden "CANCELLATION_UNAVAILABLE" "This UserOperation cannot be cancelled"

operationStatus :: KernelGatewayConfig -> DbPool -> EthClient -> EthClient -> Manager -> Text -> KM.KeyMap Value -> Gateway Value
operationStatus gateway db primary secondary manager client fields = do
  except $ exactVersionedFields fields ["preparationId","userOperationHash"] ["recoveryToken"]
  ident <- except $ readHash fields "preparationId"
  operationHash <- except $ readHash fields "userOperationHash"
  found <- findAuthorizedKernelOperation gateway db client ident operationHash fields
  case found of
    Just saved -> acceptedOperationStatus gateway db primary secondary manager saved
    Nothing -> do
      now <- seconds
      let recoveryClient = kernelRecoveryClient gateway now client ident fields
      unacceptedOperationStatus gateway db primary secondary manager recoveryClient ident operationHash

acceptedOperationStatus :: KernelGatewayConfig -> DbPool -> EthClient -> EthClient -> Manager -> Db.KernelOperation -> Gateway Value
acceptedOperationStatus gateway db primary secondary manager saved = do
  let preparation = Db.koPreparation saved
      operationHash = Db.koHash saved
  stored <- except $ unpackStoredOperation $ Db.koSignedOperation saved
  cancellation <- case Db.kpMode preparation of
    Db.PayWithEth -> dbAction db $ \conn -> Db.getAcceptedKernelCancellation conn saved
    Db.Sponsored -> pure Nothing
  let expectedPaymaster = fromMaybe zeroAddress $ Paymaster.puoPaymaster stored
      pending = kernelStatus "pending" Nothing
      consumed = kernelStatus "nonce_consumed" Nothing
      verify = canonicalKernelReceipt gateway primary secondary preparation
      inclusionResult (inclusion, receipt) =
        case Db.kpMode preparation of
          Db.Sponsored -> pure $ kernelStatus "included" (Just receipt)
          Db.PayWithEth -> do
            finalized <- dbAction db $ \conn -> Db.finalizeKernelOperation conn scope operationHash inclusion
            pure $ if finalized then kernelStatus "included" (Just receipt) else pending
      cancellationHash = cancellation >>= Db.kcHash
      cancellationResult hash (inclusion, receipt) = do
        finalized <- dbAction db $ \conn -> Db.finalizeKernelCancellation conn saved hash inclusion
        pure $ if finalized then kernelStatus "cancelled" (Just receipt) else pending
      storedCancellation = case Db.koTerminalEvidence saved of
        Just (Object evidence)
          | Just (String hash) <- KM.lookup "cancellationHash" evidence
          , Just hash == cancellationHash
          , Just (String txHash) <- KM.lookup "transactionHash" evidence -> Just (hash, kernelReceiptLocator txHash)
        _ -> Nothing
  case (Db.koState saved, storedCancellation) of
    ("sponsorship_expired", _) -> pure $ kernelStatus "sponsorship_expired" Nothing
    ("nonce_consumed", Just (hash, locator)) ->
      verify zeroAddress hash locator >>= maybe (pure pending) (cancellationResult hash)
    _ -> do
      locate <- case Db.koTerminalEvidence saved of
        Just (Object evidence) | Just (String txHash) <- KM.lookup "transactionHash" evidence ->
          pure $ kernelReceiptLocator txHash
        _ -> altoKernelReceiptLocator gateway manager operationHash
      receipt <- verify expectedPaymaster operationHash locate
      case receipt of
        Just evidence -> inclusionResult evidence
        Nothing | Db.kpMode preparation == Db.PayWithEth -> do
          direct <- case cancellationHash of
            Just hash -> do
              locator <- altoKernelReceiptLocator gateway manager hash
              fmap ((,) hash) <$> verify zeroAddress hash locator
            Nothing -> pure Nothing
          case direct of
            Just (hash, evidence) -> cancellationResult hash evidence
            Nothing | Db.koState saved == "nonce_consumed" -> pure consumed
            Nothing -> do
              resolved <- liftIO $ runExceptT $
                findCanonicalNonceEvent gateway db primary secondary preparation
              case resolved of
                Right (Just (snapshot, observedNonce, event, paymaster)) -> do
                  let foundHash = Chain.uoeHash event
                      locator = kernelReceiptLocator (Chain.uoeTransactionHash event)
                  case () of
                    _ | foundHash == operationHash ->
                          verify expectedPaymaster operationHash locator >>= maybe (pure pending) inclusionResult
                      | Just foundHash == cancellationHash ->
                          verify zeroAddress foundHash locator >>= maybe (pure pending) (cancellationResult foundHash)
                      | otherwise -> do
                          replacement <- verify paymaster foundHash locator
                          case replacement of
                            Nothing -> pure pending
                            Just _ -> do
                              revalidateSnapshot gateway primary secondary snapshot
                              retired <- dbAction db $ \conn -> Db.retireKernelOperation conn scope operationHash $
                                Db.KernelNonceConsumed (Db.kpSender preparation) (Db.kpNonce preparation)
                                  observedNonce (Native.sbhNumber snapshot) (Native.sbhHash snapshot)
                              pure $ if retired then consumed else pending
                _ -> pure pending
        Nothing -> pure pending
 where
  profile = kgcProfile gateway
  scope = kernelScope profile

-- A timeout or an unavailable gateway can leave a locally signed operation
-- without an acceptance row. A matching authorized preparation is enough to
-- look for canonical evidence. This path never creates an operation or relays.
unacceptedOperationStatus :: KernelGatewayConfig -> DbPool -> EthClient -> EthClient -> Manager
  -> Text -> Text -> Text -> Gateway Value
unacceptedOperationStatus gateway db primary secondary manager client ident operationHash = do
  prepared <- dbAction db (\conn -> Db.getKernelPreparationForClient conn scope ident client)
    >>= maybe (throwE operationNotAuthorized) pure
  operation <- except $ unpackStoredOperation $ Db.kpUnsignedOperation prepared
  expected <- toHex <$> except (Kernel.kernelUserOperationHash profile operation)
  unless (operationHash == expected) $ throwE operationNotAuthorized
  let expectedPaymaster = fromMaybe zeroAddress $ Paymaster.puoPaymaster operation
      verify = canonicalKernelReceipt gateway primary secondary prepared
      included = kernelStatus "included" . Just . snd
  locator <- altoKernelReceiptLocator gateway manager operationHash
  direct <- verify expectedPaymaster operationHash locator
  case direct of
    Just evidence -> pure $ included evidence
    Nothing -> do
      resolved <- liftIO $ runExceptT $
        findCanonicalNonceEvent gateway db primary secondary prepared
      outcome <- case resolved of
        Right (Just (_, _, event, paymaster)) -> do
          let foundHash = Chain.uoeHash event
              foundLocator = kernelReceiptLocator (Chain.uoeTransactionHash event)
          if foundHash == operationHash
            then fmap included <$> verify expectedPaymaster operationHash foundLocator
            else fmap (const $ kernelStatus "nonce_consumed" Nothing) <$> verify paymaster foundHash foundLocator
        _ -> pure Nothing
      maybe (unacceptedOperationWithoutReceipt prepared operation) pure outcome
 where
  profile = kgcProfile gateway
  scope = kernelScope profile
  unacceptedOperationWithoutReceipt prepared operation = do
    expired <- case (Db.kpMode prepared, Paymaster.puoPaymaster operation) of
      (Db.Sponsored, Just paymaster) -> do
        authorization <- dbAction db $ \conn -> Db.getKernelSponsorshipForClient conn scope paymaster ident client
        pure $ case authorization of
          Just record ->
            let draft = Db.kspDraft record
            in Db.kspUserOperationHash record == Just operationHash && Db.kspStatus record == "expired"
                && Db.ksdPreparationId draft == ident && Db.ksdSender draft == Db.kpSender prepared
                && Db.ksdNonce draft == Db.kpNonce prepared
          Nothing -> False
      _ -> pure False
    pure $ kernelStatus (if expired then "sponsorship_expired" else "pending") Nothing

kernelStatus :: Text -> Maybe Value -> Value
kernelStatus state receipt = object ["state" .= state, "receipt" .= receipt]

kernelReceiptLocator :: Text -> Value
kernelReceiptLocator txHash = object ["receipt" .= object ["transactionHash" .= txHash]]

altoKernelReceiptLocator :: KernelGatewayConfig -> Manager -> Text -> Gateway Value
altoKernelReceiptLocator gateway manager hash = do
  -- Give the status request time to check canonical evidence if Alto stalls.
  result <- liftIO $ timeout 4_000_000 $
    runExceptT $ alto gateway manager "eth_getUserOperationReceipt" [String hash]
  pure $ maybe Null (either (const Null) id) result

canonicalKernelReceipt :: KernelGatewayConfig -> EthClient -> EthClient -> Db.KernelPreparation
  -> Text -> Text -> Value -> Gateway (Maybe (Db.KernelInclusion, Value))
canonicalKernelReceipt _ _ _ _ _ _ Null = pure Nothing
canonicalKernelReceipt gateway primary secondary prepared paymaster hash locator = either (const Nothing) Just <$> liftIO
  (Reconciler.verifyKernelOperationReceipt (Db.kpScope prepared) primary secondary (kgcMaxSafeLagSeconds gateway)
    (Db.kpSender prepared) (Db.kpNonce prepared) paymaster hash locator)

-- EntryPoint lane nonces are monotonic. Use the preparation's safe block as
-- a starting point and find the first block whose lane nonce exceeds the
-- prepared nonce. This takes logarithmically many state reads and allows a
-- one-block log query even when the event has aged out of Alto's lookback.
-- An advanced nonce with no matching event is not evidence of replacement.
findCanonicalNonceEvent :: KernelGatewayConfig -> DbPool -> EthClient -> EthClient -> Db.KernelPreparation
  -> Gateway (Maybe (Native.SecurityBlockHeader, Integer, Chain.UserOperationEvent, Text))
findCanonicalNonceEvent gateway db primary secondary prepared = do
  snapshot <- agreedSnapshot gateway primary secondary
  observed <- attestBoth gateway primary secondary snapshot $ \client block ->
    readNonce profile client block sender
  if observed <= nonce then pure Nothing else do
    historical <- liftIO $ timeout 4_000_000 $
      runExceptT $ attestBoth gateway primary secondary snapshot readEvents
    found <- case historical of
      Just (Right events) -> pure events
      _ -> scanLogs snapshot
    pure $ (\(event, paymaster) -> (snapshot, observed, event, paymaster)) <$> found
 where
  profile = kgcProfile gateway
  entryPoint = Kernel.kpEntryPoint profile
  sender = Db.kpSender prepared
  nonce = Db.kpNonce prepared
  eventTopic = toHex $ keccak256 "UserOperationEvent(bytes32,address,address,uint256,bool,uint256,uint256)"
  readEvents client upper = runExceptT $ do
    transition <- locateKernelNonceTransition
      (\height -> ExceptT $ readNonce profile client height sender) (Db.kpSafeBlock prepared) upper nonce
    inclusionBlock <- maybe (throwE profileUnavailable) pure transition
    readEventRange client inclusionBlock inclusionBlock

  -- Some RPCs keep logs after pruning account state. On dual state failure,
  -- advance one bounded log batch per poll. The recorded end hash prevents a
  -- reorg from silently skipping the already-covered part of the range.
  scanLogs snapshot = case Db.kpSafeBlock prepared of
    Nothing -> attestBoth gateway primary secondary snapshot $ \client upper ->
      runExceptT $ readEventRange client (max 0 $ upper - 1_999) upper
    Just anchor -> do
      let upper = Native.sbhNumber snapshot
      cursor <- dbAction db $ \conn -> Db.getKernelNonceScanCursor conn prepared
      resumed <- case cursor of
        Just (covered, hash) | covered >= anchor && covered <= upper -> do
          coverage <- readCoverageHeader covered
          pure $ if Native.sbhHash coverage == hash then Just coverage else Nothing
        _ -> pure Nothing
      case kernelNonceScanRange anchor (Native.sbhNumber <$> resumed) upper of
        Nothing -> pure Nothing
        Just (lower, end) -> do
          endHeader <- readCoverageHeader end
          found <- attestBoth gateway primary secondary snapshot $ \client _ ->
            runExceptT $ readEventRange client lower end
          forMOptional resumed requireSameHeader
          requireSameHeader endHeader
          revalidateSnapshot gateway primary secondary snapshot
          when (found == Nothing) $ do
            _ <- dbAction db $ \conn -> Db.storeKernelNonceScanCursor conn prepared cursor
              (end, Native.sbhHash endHeader)
            pure ()
          pure found
  readCoverageHeader block = do
    coverage <- liftIO $ Native.readAgreedSecurityHeaderAt DualIndependent primary secondary block
    either (const $ throwE profileUnavailable) pure coverage
  requireSameHeader coverage = do
    current <- readCoverageHeader (Native.sbhNumber coverage)
    unless (current == coverage) $ throwE profileUnavailable

  readEventRange client lower upper = do
    reply <- liftIO $ rpcCall client "eth_getLogs" $ toJSON
      [object ["address" .= entryPoint, "fromBlock" .= Paymaster.canonicalQuantity lower,
               "toBlock" .= Paymaster.canonicalQuantity upper,
               "topics" .= [String eventTopic, Null, String (toHex $ encodeAddress sender)]]]
    logs <- case reply of
      Right (Array values) | V.length values <= 2000 -> pure $ V.toList values
      _ -> throwE profileUnavailable
    events <- traverse (parseEvent lower upper) logs
    case filter ((== nonce) . Chain.uoeNonce . fst) events of
      [] -> pure Nothing
      [(event, paymaster)] -> pure $ Just (event {Chain.uoeRaw = Null}, paymaster)
      _ -> throwE profileUnavailable
  parseEvent lower upper raw@(Object eventFields) = case KM.lookup "topics" eventFields of
    Just (Array topics)
      | V.length topics == 4
      , String paymasterTopic <- topics V.! 3
      , Just bytes <- fixedHex 32 paymasterTopic
      , BS.take 12 bytes == BS.replicate 12 0 -> do
          let paymaster = T.toLower $ decodeAddress bytes
          event <- except $ mapParseFailure $
            Chain.parseUserOperationEventAt entryPoint paymaster lower upper raw
          unless (Chain.uoeSender event == sender) $ throwE profileUnavailable
          pure (event, paymaster)
    _ -> throwE profileUnavailable
  parseEvent _ _ _ = throwE profileUnavailable

-- Accept only a cursor whose hash has been checked against both RPCs.
kernelNonceScanRange :: Integer -> Maybe Integer -> Integer -> Maybe (Integer, Integer)
kernelNonceScanRange anchor covered upper
  | lower <= upper = Just (lower, min upper (lower + 1_999))
  | otherwise = Nothing
 where
  lower = maybe anchor (+ 1) covered

-- Given a safe upper block where the lane has advanced, locate the first
-- block after the operation's nonce. A recorded preparation anchors the
-- search. For older rows, grow the range backward until it brackets the
-- transition. The caller still needs a canonical event and receipt.
locateKernelNonceTransition :: Monad m => (Integer -> m Integer) -> Maybe Integer -> Integer -> Integer -> m (Maybe Integer)
locateKernelNonceTransition nonceAt preparationBlock upper nonce = do
  range <- case preparationBlock of
    Just anchor | anchor >= 0 && anchor < upper -> do
      anchored <- nonceAt anchor
      if anchored <= nonce then pure $ Just (anchor, upper) else bracket anchor 1_000
    _ -> bracket upper 1_000
  traverse (uncurry findTransition) range
 where
  bracket changed distance
    | changed == 0 = pure Nothing
    | otherwise = do
        let earlier = max 0 (changed - distance)
        current <- nonceAt earlier
        if current <= nonce then pure $ Just (earlier, changed)
        else bracket earlier (distance * 2)
  findTransition lower changed
    | changed - lower == 1 = pure changed
    | otherwise = do
        let middle = lower + (changed - lower) `div` 2
        current <- nonceAt middle
        if current > nonce then findTransition lower middle else findTransition middle changed

verifySponsorAvailability :: KernelGatewayConfig -> DbPool -> EthClient -> EthClient -> Native.SecurityBlockHeader -> KernelSponsorshipConfig -> Gateway ()
verifySponsorAvailability gateway db primary secondary snapshot sponsor = do
  ready <- dbAction db $ \conn ->
    Db.kernelSponsorshipReady conn (kernelScope $ kgcProfile gateway) (kscPaymaster sponsor) 120
  unless ready $ throwE $ Legacy.unavailable "SPONSORSHIP_RECONCILIATION_UNAVAILABLE" "Sponsorship settlement is unavailable"
  attestBoth gateway primary secondary snapshot (attestSponsorAt (kgcProfile gateway) sponsor)

attestSponsorAt :: Kernel.KernelProfile -> KernelSponsorshipConfig -> EthClient -> Integer -> IO (Either Legacy.ProxyFailure ())
attestSponsorAt profile sponsor client block = runExceptT $ do
  rpcCode <- liftIO $ rpcCall client "eth_getCode" $ toJSON
    [String (kscPaymaster sponsor), String (Paymaster.canonicalQuantity block)]
  code <- case rpcCode of
    Right (String value) | Just bytes <- hexBytes value -> pure bytes
    _ -> throwE profileUnavailable
  unless (toHex (keccak256 code) == kscPaymasterCodeHash sponsor) $ throwE profileUnavailable
  -- Run two bounded streams per provider to reduce round-trip latency.
  (addresses, hashes) <- liftIO $ concurrently
    (runExceptT $ mapM_ (readExpected readContractAddress) addressPins)
    (runExceptT $ mapM_ (readExpected (fmap toHex . readConstant)) hashPins)
  except $ addresses >> hashes
  paused <- decodeUint256 <$> readConstant "paused()"
  unless (paused == 0) $ throwE $ Legacy.unavailable "PAYMASTER_PAUSED" "Sponsorship is paused"
  maxCost <- decodeUint256 <$> readConstant "maxSponsoredCost()"
  maxValidity <- decodeUint256 <$> readConstant "MAX_VALIDITY_WINDOW()"
  unless (maxCost >= Db.ksbMaxCostWei (kscBudget sponsor) && maxValidity >= kscValiditySeconds sponsor + 30) $
    throwE profileUnavailable
  stakeResult <- liftIO $ ethCallAtBlock client
    (CallParams (Kernel.kpEntryPoint profile) $
      encodeCall "getDepositInfo(address)" [encodeAddress $ kscPaymaster sponsor]) block
  unless (either (const False) sponsorStakeReady stakeResult) $ throwE profileUnavailable
  deposit <- ExceptT $ readAccountDeposit profile client block (kscPaymaster sponsor)
  unless (deposit >= max (kscMinDepositWei sponsor) (Db.ksbGlobalOutstandingWei $ kscBudget sponsor)) $
    throwE $ Legacy.unavailable "PAYMASTER_UNDERFUNDED" "The sponsor does not have enough ETH"
 where
  addressPins =
    [ ("entryPoint()", Kernel.kpEntryPoint profile)
    , ("accountFactory()", Kernel.kpFactory profile)
    , ("accountStaker()", Kernel.kpStaker profile)
    , ("accountImplementation()", Kernel.kpImplementation profile)
    , ("rootValidator()", Kernel.kpValidator profile)
    , ("sponsorSigner()", kscSignerAddress sponsor)
    ]
  hashPins =
    [ ("entryPointCodeHash()", Kernel.kpEntryPointCodeHash profile)
    , ("accountFactoryCodeHash()", Kernel.kpFactoryCodeHash profile)
    , ("accountStakerCodeHash()", Kernel.kpStakerCodeHash profile)
    , ("accountImplementationCodeHash()", Kernel.kpImplementationCodeHash profile)
    , ("rootValidatorCodeHash()", Kernel.kpValidatorCodeHash profile)
    , ("approvedAccountCodeHash()", Kernel.kpAccountCodeHash profile)
    , ("policyId()", kscPolicyId sponsor)
    ]
  readExpected readValue (method, expected) = do
    actual <- readValue method
    unless (actual == expected) $ throwE profileUnavailable
  readConstant method = ExceptT $ readWord client block (kscPaymaster sponsor) method []
  readContractAddress method = do
    word <- readConstant method
    unless (BS.take 12 word == BS.replicate 12 0) $ throwE profileUnavailable
    pure $ T.toLower $ decodeAddress word

-- EntryPoint returns deposit, staked, stake, unstakeDelaySec and withdrawTime.
-- Validate the entire fixed-width result before admitting sponsored operations.
sponsorStakeReady :: ByteString -> Bool
sponsorStakeReady bytes
  | BS.length bytes /= 160 = False
  | otherwise =
      let word index = decodeUint256 $ BS.take 32 $ BS.drop (32 * index) bytes
          deposit = word 0
          staked = word 1
          stake = word 2
          delay = word 3
          withdrawTime = word 4
      in deposit < 2 ^ (112 :: Int)
        && staked == 1
        && stake >= Kernel.minimumKernelStakeWei && stake < 2 ^ (112 :: Int)
        && delay >= 86400 && delay < 2 ^ (32 :: Int)
        && withdrawTime == 0

checkFunding :: KernelGatewayConfig -> EthClient -> EthClient -> Native.SecurityBlockHeader -> Db.PaymentMode
  -> Text -> [Legacy.SmartCall] -> Paymaster.PackedUserOperation -> Gateway ()
checkFunding gateway primary secondary snapshot mode owner calls operation = do
  let sender = Paymaster.puoSender operation
      requiredPrefund = if mode == Db.PayWithEth then maxPrefund operation else 0
      value = sum $ map Legacy.smartCallValue calls
      depositWithdrawal = kernelDepositWithdrawal (kgcProfile gateway) owner calls
  when (requiredPrefund > 0 || value > 0 || depositWithdrawal > 0) $ do
    (deposit, balance) <- readKernelFunds gateway primary secondary snapshot sender
    requireKernelFunds requiredPrefund deposit balance value depositWithdrawal

readKernelFunds :: KernelGatewayConfig -> EthClient -> EthClient -> Native.SecurityBlockHeader -> Text
  -> Gateway (Integer, Integer)
readKernelFunds gateway primary secondary snapshot sender =
  attestBoth gateway primary secondary snapshot $ \client block -> runExceptT $ do
    deposit <- ExceptT $ readAccountDeposit (kgcProfile gateway) client block sender
    result <- liftIO $ rpcCall client "eth_getBalance" $ toJSON [String sender, String $ Paymaster.canonicalQuantity block]
    case result of
      Right (String quantity) | Just amount <- parseQuantity quantity -> pure (deposit, amount)
      _ -> throwE profileUnavailable

kernelDepositWithdrawal :: Kernel.KernelProfile -> Text -> [Legacy.SmartCall] -> Integer
kernelDepositWithdrawal profile owner [call] = fromMaybe 0 $ readDepositWithdrawal profile owner call
kernelDepositWithdrawal _ _ _ = 0

requireKernelFunds :: Integer -> Integer -> Integer -> Integer -> Integer -> Gateway ()
requireKernelFunds prefund deposit liquid value withdrawal = do
  unless (max 0 (deposit - prefund) >= withdrawal) $ throwE $ Legacy.ProxyFailure status400 (-32001)
    "The EntryPoint deposit cannot cover this withdrawal after reserving network gas. Review a smaller amount." "INSUFFICIENT_ENTRYPOINT_DEPOSIT" False
  unless (kernelCanFund prefund liquid deposit value withdrawal) $ throwE insufficientGasEth

kernelCanFund :: Integer -> Integer -> Integer -> Integer -> Integer -> Bool
kernelCanFund prefund liquid deposit value withdrawal =
  liquid >= max 0 (prefund - deposit) + value &&
  max 0 (deposit - prefund) >= withdrawal

kernelFundingAdvice :: Integer -> Integer -> Integer -> Integer -> Maybe Value
kernelFundingAdvice prefund liquid deposit value
  | shortfall > 0 = Just $ object
      [ "balanceWei" .= Paymaster.canonicalQuantity liquid
      , "entryPointDepositWei" .= Paymaster.canonicalQuantity deposit
      , "maxNetworkGasWei" .= Paymaster.canonicalQuantity prefund
      , "callValueWei" .= Paymaster.canonicalQuantity value
      , "shortfallWei" .= Paymaster.canonicalQuantity shortfall
      ]
  | otherwise = Nothing
 where shortfall = max 0 (prefund - deposit) + value - liquid

insufficientGasEth :: Legacy.ProxyFailure
insufficientGasEth = Legacy.ProxyFailure status400 (-32001)
  "The Trading Account does not have enough native ETH for this operation" "INSUFFICIENT_GAS_ETH" False

respondFundingAdvice :: Value -> Value -> ActionM ()
respondFundingAdvice ident advice = do
  setHeader "Cache-Control" "no-store"
  status $ Legacy.pfStatus insufficientGasEth
  json $ object
    [ "jsonrpc" .= ("2.0" :: Text), "id" .= ident
    , "error" .= object
        [ "code" .= Legacy.pfCode insufficientGasEth
        , "message" .= Legacy.pfMessage insufficientGasEth
        , "data" .= object ["reason" .= Legacy.pfReason insufficientGasEth, "retryable" .= False, "funding" .= advice]
        ]
    ]

validateKernelActions :: KernelGatewayConfig -> Config -> Db.PaymentMode -> Text -> Text -> [Legacy.SmartCall]
  -> Either Legacy.ProxyFailure ()
validateKernelActions gateway cfg mode sender owner calls = do
  ownerWithdrawal <- validateKernelWithdrawalMode (kgcProfile gateway) mode owner calls
  unless ownerWithdrawal $
    Legacy.validateActionSequenceWithProtection (kgcProtectionCommitsEnabled gateway) cfg sender owner calls

-- Sending native ETH can execute owner code. Safe-block EOA attestation is
-- necessary but cannot stop a delegation made after sponsorship is signed.
-- Only permit these owner-directed withdrawals when the account pays gas.
validateKernelWithdrawalMode :: Kernel.KernelProfile -> Db.PaymentMode -> Text -> [Legacy.SmartCall]
  -> Either Legacy.ProxyFailure Bool
validateKernelWithdrawalMode profile mode owner calls = do
  when (mode == Db.Sponsored && any ownerWithdrawal calls) $
    Left $ Legacy.policyDenied "Native ETH withdrawals require ETH-paid gas"
  pure $ case calls of
    [call] -> ownerWithdrawal call
    _ -> False
 where
  ownerWithdrawal call =
    (Legacy.smartCallTarget call == owner && Legacy.smartCallValue call > 0 && BS.null (Legacy.smartCallData call))
      || maybe False (> 0) (readDepositWithdrawal profile owner call)

readDepositWithdrawal :: Kernel.KernelProfile -> Text -> Legacy.SmartCall -> Maybe Integer
readDepositWithdrawal profile owner call
  | Legacy.smartCallTarget call == Kernel.kpEntryPoint profile
  , Legacy.smartCallValue call == 0
  , BS.length bytes == 68
  , BS.take 36 bytes == selector "withdrawTo(address,uint256)" <> encodeAddress owner =
      Just $ decodeUint256 (BS.drop 36 bytes)
  | otherwise = Nothing
 where bytes = Legacy.smartCallData call

readNonce :: Kernel.KernelProfile -> EthClient -> Integer -> Text -> IO (Either Legacy.ProxyFailure Integer)
readNonce profile client block sender = runExceptT $ do
  word <- ExceptT $ readWord client block (Kernel.kpEntryPoint profile) "getNonce(address,uint192)"
    [encodeAddress sender, encodeUint256 0]
  let nonce = decodeUint256 word
  unless (nonce < 2^(64::Int)) $ throwE profileUnavailable
  pure nonce

readAccountDeposit :: Kernel.KernelProfile -> EthClient -> Integer -> Text -> IO (Either Legacy.ProxyFailure Integer)
readAccountDeposit profile client block sender = fmap decodeUint256 <$>
  readWord client block (Kernel.kpEntryPoint profile) "balanceOf(address)" [encodeAddress sender]

readWord :: EthClient -> Integer -> Text -> Text -> [ByteString] -> IO (Either Legacy.ProxyFailure ByteString)
readWord client block target function args = do
  result <- ethCallAtBlock client (CallParams target (encodeCall function args)) block
  pure $ case result of
    Right bytes | BS.length bytes == 32 -> Right bytes
    _ -> Left profileUnavailable

matchesPerpsRelease :: Kernel.KernelProfile -> Config -> Bool
matchesPerpsRelease profile cfg =
  Manifest.releaseIsDeployed
  && cfgPerpsChainId cfg == Manifest.releaseChainId
  && Manifest.releaseChainId == Kernel.kpChainId profile
  && all (\(configured, pinned) -> T.toLower configured == T.toLower pinned)
       [ (cfgPerpsSettlementToken cfg, Manifest.settlementTokenAddress)
       , (cfgPerpsMarginClearinghouse cfg, Manifest.marginClearinghouseAddress)
       , (cfgPerpsOrderRouter cfg, Manifest.orderRouterAddress)
       , (cfgPerpsCfdEngine cfg, Manifest.cfdEngineAddress)
       ]

verifyPerpsAccountAtBlock :: Kernel.KernelProfile -> Text -> Maybe Text -> EthClient -> Integer
  -> IO (Either Legacy.ProxyFailure Text)
verifyPerpsAccountAtBlock profile sender factoryOwner client block = do
  (targets, account) <- concurrently
    (verifyPerpsTargetsAtBlock client block)
    (Kernel.verifyKernelAccountAtBlock profile client block sender factoryOwner)
  pure (targets >> account)

verifyPerpsTargetsAtBlock :: EthClient -> Integer -> IO (Either Legacy.ProxyFailure ())
verifyPerpsTargetsAtBlock client block = do
  verified <- mapConcurrently verify
    [ (Manifest.settlementTokenAddress, Manifest.settlementTokenCodeHash)
    , (Manifest.marginClearinghouseAddress, Manifest.marginClearinghouseCodeHash)
    , (Manifest.orderRouterAddress, Manifest.orderRouterCodeHash)
    , (Manifest.cfdEngineAddress, Manifest.cfdEngineCodeHash)
    , (Manifest.positionProtectionBookAddress, Manifest.positionProtectionBookCodeHash)
    ]
  pure $ if and verified then Right () else Left profileUnavailable
 where
  verify (target, hash) = do
    reply <- rpcCall client "eth_getCode" $ toJSON [String target, String $ Paymaster.canonicalQuantity block]
    pure $ case reply of
      Right (String value) | Just code <- hexBytes value, not (BS.null code) ->
        toHex (keccak256 code) == T.toLower hash
      _ -> False

requireMode :: KernelGatewayConfig -> Db.PaymentMode -> Gateway (Maybe KernelSponsorshipConfig)
requireMode gateway mode = case mode of
  Db.PayWithEth | kgcEthModeEnabled gateway -> pure Nothing
  Db.Sponsored | Just sponsor <- kgcSponsorship gateway, kscEnabled sponsor -> pure $ Just sponsor
  _ -> throwE modeDisabled

kernelOperationDeadline :: Integer -> [Legacy.SmartCall] -> Either Legacy.ProxyFailure (Maybe Integer)
kernelOperationDeadline now calls = do
  deadline <- Legacy.orderSubmissionDeadlineForCalls calls
  when (maybe False (<= now+30) deadline) $
    Left $ Legacy.ProxyFailure status400 (-32001) "The order deadline is too close. Review a fresh quote." "DEADLINE_TOO_CLOSE" False
  pure deadline

agreedSnapshot :: KernelGatewayConfig -> EthClient -> EthClient -> Gateway Native.SecurityBlockHeader
agreedSnapshot gateway primary secondary = do
  let expected = Paymaster.canonicalQuantity (Kernel.kpChainId $ kgcProfile gateway)
      chain client = rpcCall client "eth_chainId" $ toJSON ([] :: [Value])
  (a, b) <- liftIO $ concurrently (chain primary) (chain secondary)
  case (a, b) of
    (Right (String first), Right (String second)) | normalizeString (String first) == String expected
      , normalizeString (String second) == String expected -> pure ()
    _ -> throwE profileUnavailable
  result <- liftIO $ Native.readAgreedSecurityBlock DualIndependent
    (kgcMaxSafeLagSeconds gateway) primary secondary
  either (const $ throwE profileUnavailable) pure result

-- Both onboarding consumers use the same block agreement and profile as
-- execution. Resolve identity even when transaction admission is disabled.
resolveOwnedKernelTradingAccount :: KernelGatewayConfig -> Config -> EthClient -> Text
  -> IO (Either Legacy.OwnedTradingAccountFailure KernelTradingAccountProof)
resolveOwnedKernelTradingAccount gateway cfg ordinaryPrimary owner = do
  verified <- runExceptT $ do
    unless (matchesPerpsRelease profile cfg) $ throwE profileUnavailable
    snapshot <- agreedSnapshot gateway primary secondary
    evidence <- attestBoth gateway primary secondary snapshot $ \client block ->
      Right <$> Kernel.resolveOwnedKernelAccountAtBlock profile client block owner
    pure $ (\sender -> KernelTradingAccountProof sender (Native.sbhNumber snapshot) (Native.sbhHash snapshot)) <$> evidence
  pure $ either (const $ Left Legacy.OwnedTradingAccountProofUnavailable) id verified
 where
  profile = kgcProfile gateway
  primary = withoutRpcFallback ordinaryPrimary
  secondary = withoutRpcFallback $ kgcSecondaryClient gateway

attestBoth :: Eq a => KernelGatewayConfig -> EthClient -> EthClient -> Native.SecurityBlockHeader
  -> (EthClient -> Integer -> IO (Either Legacy.ProxyFailure a)) -> Gateway a
attestBoth gateway primary secondary snapshot readAt = do
  let block = Native.sbhNumber snapshot
  (first, second) <- liftIO $ concurrently (readAt primary block) (readAt secondary block)
  revalidateSnapshot gateway primary secondary snapshot
  case (first, second) of
    (Right a, Right b) | a == b -> pure a
    (Left a, Left b) | a == b -> throwE a
    _ -> throwE profileUnavailable

revalidateSnapshot :: KernelGatewayConfig -> EthClient -> EthClient -> Native.SecurityBlockHeader -> Gateway ()
revalidateSnapshot gateway primary secondary snapshot = do
  result <- liftIO $ Native.revalidateSecuritySnapshot DualIndependent
    (kgcMaxSafeLagSeconds gateway) primary secondary snapshot
  unless (result == Right ()) $ throwE profileUnavailable

alto :: KernelGatewayConfig -> Manager -> Text -> [Value] -> Gateway Value
alto gateway manager method params = do
  request' <- except $ Preparation.internalRequest method params
  (response, _) <- ExceptT $ Native.forwardAlto manager (kgcAltoRpcUrl gateway) request'
  except $ Native.classifyAltoResult method response

gasPrice :: KernelGatewayConfig -> Manager -> Gateway Value
gasPrice gateway manager = do
  result <- alto gateway manager "pimlico_getUserOperationGasPrice" []
  case result of
    Object tiers -> object <$> traverse (priceTier tiers) ["slow", "standard", "fast"]
    _ -> throwE gasQuoteUnavailable
 where
  priceTier tiers name = case KM.lookup name tiers of
    Just (Object values) -> do
      fees <- quoteQuantity values "maxFeePerGas"
      priority <- quoteQuantity values "maxPriorityFeePerGas"
      unless (fees >= priority && fees > 0 && fees <= 10_000_000_000 && priority <= 2_000_000_000) $
        throwE gasQuoteUnavailable
      pure $ name .= object ["maxFeePerGas" .= Paymaster.canonicalQuantity fees,
                             "maxPriorityFeePerGas" .= Paymaster.canonicalQuantity priority]
    _ -> throwE gasQuoteUnavailable

gasForPrepare :: KernelGatewayConfig -> Manager -> Gateway (Integer, Integer)
gasForPrepare gateway manager = do
  result <- gasPrice gateway manager
  case result of
    Object fields | Just (Object fast) <- KM.lookup "fast" fields ->
      (,) <$> quoteQuantity fast "maxFeePerGas" <*> quoteQuantity fast "maxPriorityFeePerGas"
    _ -> throwE gasQuoteUnavailable

quoteQuantity :: KM.KeyMap Value -> Key.Key -> Gateway Integer
quoteQuantity fields key = case KM.lookup key fields of
  Just (String quantity) | Just result <- parseQuantity quantity, result < 2^(128::Int) -> pure result
  _ -> throwE gasQuoteUnavailable

parseQuantity :: Text -> Maybe Integer
parseQuantity text = do
  raw <- T.stripPrefix "0x" text
  if T.null raw || T.length raw > 64 || not (T.all lowerHex raw) || (T.length raw > 1 && T.head raw == '0')
    then Nothing
    else readMaybe $ T.unpack text

parseMode :: Text -> Maybe Db.PaymentMode
parseMode "sponsored" = Just Db.Sponsored
parseMode "eth" = Just Db.PayWithEth
parseMode _ = Nothing

readMode :: KM.KeyMap Value -> Either Legacy.ProxyFailure Db.PaymentMode
readMode fields = readText fields "mode" >>= maybe (Left $ Legacy.invalidParams "Unknown gas payment mode") Right . parseMode

renderMode :: Db.PaymentMode -> Text
renderMode Db.Sponsored = "sponsored"
renderMode Db.PayWithEth = "eth"

toKernelMode :: Db.PaymentMode -> Kernel.KernelGasMode
toKernelMode Db.Sponsored = Kernel.KernelSponsored
toKernelMode Db.PayWithEth = Kernel.KernelEthPaid

exactVersionedFields :: KM.KeyMap Value -> [Key.Key] -> [Key.Key] -> Either Legacy.ProxyFailure ()
exactVersionedFields fields required optional =
  unless (KM.lookup "version" fields == Just (Number 1) && all (`KM.member` fields) required
    && all (`elem` ("version" : required <> optional)) (KM.keys fields)) $
    Left $ Legacy.invalidParams "Invalid Kernel request version or fields"

readText :: KM.KeyMap Value -> Key.Key -> Either Legacy.ProxyFailure Text
readText fields name = case KM.lookup name fields of
  Just (String value) -> Right value
  _ -> Left $ Legacy.invalidParams $ Key.toText name <> " must be a string"

readHash :: KM.KeyMap Value -> Key.Key -> Either Legacy.ProxyFailure Text
readHash fields name = do
  raw <- readText fields name
  maybe (Left $ Legacy.invalidParams "Invalid operation or preparation identifier") (Right . toHex) $ fixedHex 32 raw

requireBytes :: KM.KeyMap Value -> Key.Key -> Int -> Either Legacy.ProxyFailure Text
requireBytes fields name maxSize = do
  raw <- readText fields name
  unless (T.length raw <= 2 + 2 * maxSize) $ Left $ Legacy.invalidParams "Kernel data exceeds the permitted length"
  case hexBytes raw of
    Just bytes | not (BS.null bytes) -> pure $ toHex bytes
    _ -> Left $ Legacy.invalidParams "Invalid Kernel transaction data"

address :: Text -> Maybe Text
address raw = do
  bytes <- fixedHex 20 raw
  if BS.all (==0) bytes then Nothing else Just $ toHex bytes

zeroAddress :: Text
zeroAddress = "0x0000000000000000000000000000000000000000"

hexBytes :: Text -> Maybe ByteString
hexBytes text = do
  raw <- T.stripPrefix "0x" text
  if T.all lowerHex (T.toLower raw)
    then either (const Nothing) Just $ B16.decode $ TE.encodeUtf8 raw
    else Nothing

fixedHex :: Int -> Text -> Maybe ByteString
fixedHex size text
  | T.length text /= 2 + 2*size = Nothing
  | otherwise = hexBytes text

toHex :: ByteString -> Text
toHex bytes = "0x" <> TE.decodeUtf8 (B16.encode bytes)

lowerHex :: Char -> Bool
lowerHex char = ('0' <= char && char <= '9') || ('a' <= char && char <= 'f')

normalizeString :: Value -> Value
normalizeString (String text) = String $ T.toLower text
normalizeString other = other

seconds :: Gateway Integer
seconds = liftIO $ floor <$> getPOSIXTime

toUtc :: Integer -> UTCTime
toUtc = posixSecondsToUTCTime . fromIntegral

fromUtc :: UTCTime -> Integer
fromUtc = floor . utcTimeToPOSIXSeconds

forMOptional :: Monad m => Maybe a -> (a -> m ()) -> m ()
forMOptional = flip (maybe (pure ()))

except :: Either Legacy.ProxyFailure a -> Gateway a
except = ExceptT . pure

unpackStoredOperation :: Value -> Either Legacy.ProxyFailure Paymaster.PackedUserOperation
unpackStoredOperation (Object value) = mapParseFailure $ Paymaster.parsePackedUserOperation value
unpackStoredOperation _ = Left databaseUnavailable

mapParseFailure :: Either Text a -> Either Legacy.ProxyFailure a
mapParseFailure = either (Left . Legacy.invalidParams) Right

recoveryDomain :: Kernel.KernelProfile -> Text
recoveryDomain profile = "kernel:" <> Kernel.kpDeployment profile <> ":" <> Kernel.kpEntryPoint profile

rateLimit :: KernelGatewayConfig -> DbPool -> Text -> Text -> Text -> Int -> Gateway ()
rateLimit gateway pool rateScope client account cap = do
  let scopedAccount = Kernel.kpDeployment (kgcProfile gateway) <> ":" <> account
      accountKey = ClientKey.pseudonymousAccountKey (kgcOriginToken gateway) scopedAccount
  allowed <- dbAction pool $ \conn -> consumeAaRateLimit conn ("kernel-" <> rateScope) client accountKey cap
  unless allowed $ throwE Legacy.rateLimited

rateLimitAccount :: KernelGatewayConfig -> DbPool -> Text -> Text -> Int -> Gateway ()
rateLimitAccount gateway pool rateScope =
  rateLimit gateway pool rateScope Native.nativeAccountRateClientKey

dbAction :: DbPool -> (Connection -> IO a) -> Gateway a
dbAction pool action = do
  result <- liftIO $ try $ withDb pool action
  case result of
    Left (_ :: SomeException) -> throwE databaseUnavailable
    Right v -> pure v

databaseUnavailable, profileUnavailable, gasQuoteUnavailable, modeDisabled, preparationMismatch, preparationExpired :: Legacy.ProxyFailure
databaseUnavailable = Legacy.unavailable "DATABASE_UNAVAILABLE" "The Kernel operation journal is unavailable"
profileUnavailable = Legacy.unavailable "KERNEL_EVIDENCE_UNAVAILABLE" "Kernel deployment evidence is unavailable"
gasQuoteUnavailable = Legacy.unavailable "BUNDLER_UNAVAILABLE" "Alto did not return a valid gas quote"
modeDisabled = Legacy.unavailable "PAYMENT_MODE_UNAVAILABLE" "The selected gas payment mode is unavailable"
preparationMismatch = Legacy.invalidParams "UserOperation does not match its reviewed preparation"
preparationExpired = Legacy.ProxyFailure status400 (-32001) "The preparation has expired. Review a fresh quote." "PREPARATION_EXPIRED" False

kernelNonceChanged, sponsorshipNotAuthorized :: Legacy.ProxyFailure
kernelNonceChanged = forbidden "NONCE_CHANGED" "The Trading Account nonce changed. Review a fresh quote."
sponsorshipNotAuthorized =
  forbidden "SPONSORSHIP_NOT_AUTHORIZED" "The signed UserOperation does not have a matching live sponsorship"

forbidden :: Text -> Text -> Legacy.ProxyFailure
forbidden reason message = Legacy.ProxyFailure status403 (-32001) message reason False
