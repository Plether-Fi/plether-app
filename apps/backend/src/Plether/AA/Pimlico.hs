module Plether.AA.Pimlico
  ( PimlicoProxyState
  , PimlicoMethod (..)
  , RpcRequest (..)
  , SmartCall (..)
  , newPimlicoProxyState
  , handlePimlicoProxy
  , parseRpcRequest
  , validateMethodParams
  , decodeSmartAccountCalls
  , validateActionSequence
  , injectSponsorshipPolicy
  , resolveTradingAccountAddress
  ) where

import Control.Concurrent.STM
  ( TVar
  , atomically
  , modifyTVar'
  , newTVarIO
  , readTVar
  , writeTVar
  )
import Control.Concurrent.Async (Concurrently (..), runConcurrently)
import Control.Exception (try)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
  ( Value (..)
  , eitherDecode
  , encode
  , object
  , toJSON
  , (.=)
  )
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.ByteArray (constEq)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import Data.Time.Clock
  ( NominalDiffTime
  , UTCTime
  , diffUTCTime
  , getCurrentTime
  )
import qualified Data.Vector as V
import Network.HTTP.Client
  ( HttpException
  , Manager
  , Request (..)
  , RequestBody (..)
  , httpLbs
  , parseRequest
  , responseBody
  , responseHeaders
  , responseStatus
  , responseTimeoutMicro
  , setQueryString
  )
import Network.HTTP.Types.Header (hRetryAfter)
import Network.HTTP.Types.Status
  ( Status
  , status200
  , status400
  , status403
  , status413
  )
import qualified Network.Wai as Wai
import Plether.Config (AaConfig (..), Config (..))
import Plether.Ethereum.Abi
  ( encodeAddress
  , encodeCall
  , encodeUint256
  )
import Plether.Ethereum.Client
  ( CallParams (..)
  , EthClient
  , ethCall
  , rpcCall
  )
import Web.Scotty
  ( ActionM
  , header
  , json
  , setHeader
  , status
  )
import qualified Web.Scotty as Scotty

entryPointAddress :: Text
entryPointAddress = "0x4337084d9e255ff0702461cf8895ce9e3b5ff108"

simpleAccountFactory :: Text
simpleAccountFactory = "0x13e9ed32155810fdbd067d4522c492d6f68e5944"

simpleAccountImplementation :: Text
simpleAccountImplementation = "0x28426d752372d68d34340bd94390950dce3c9ec3"

pimlicoRpcUrl :: String
pimlicoRpcUrl = "https://api.pimlico.io/v2/421614/rpc"

arbitrumSepoliaHexChainId :: Text
arbitrumSepoliaHexChainId = "0x66eee"

dummySignature :: Text
dummySignature =
  "0xfffffffffffffffffffffffffffffff000000000000000000000000000000000\
  \7aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa1c"

erc1967ImplementationSlot :: Text
erc1967ImplementationSlot =
  "0x360894a13ba1a3210667c828492db98dca3e2076cc3735a920a3ca505d382bbc"

erc1967BeaconSlot :: Text
erc1967BeaconSlot =
  "0xa3f0ad74e5423aebfd80d3ef4346578335a9a72aeaee59ff6cb3582b35133d50"

zeroWord :: Text
zeroWord = "0x" <> T.replicate 64 "0"

implementationWord :: Text
implementationWord = "0x" <> T.replicate 24 "0" <> T.drop 2 simpleAccountImplementation

data RateWindow = RateWindow
  { rwStartedAt :: UTCTime
  , rwCount :: Int
  }

data GasUsage = GasUsage
  { guStartedAt :: UTCTime
  , guActualGasCost :: Integer
  , guSubmittedOperations :: Map.Map Text UTCTime
  , guSeenReceipts :: Map.Map Text UTCTime
  , guAlerted :: Bool
  }

data PimlicoProxyState = PimlicoProxyState
  { ppsRateWindows :: TVar (Map.Map Text RateWindow)
  , ppsGasUsage :: TVar GasUsage
  }

data PimlicoMethod
  = GetGasPrice
  | GetPaymasterStubData
  | GetPaymasterData
  | EstimateUserOperationGas
  | SendUserOperation
  | GetUserOperationReceipt
  | GetUserOperationByHash
  | GetUserOperationStatus
  | GetSupportedEntryPoints
  deriving stock (Eq, Show)

data RpcRequest = RpcRequest
  { rrId :: Value
  , rrMethod :: PimlicoMethod
  , rrParams :: [Value]
  , rrObject :: KM.KeyMap Value
  }
  deriving stock (Eq, Show)

data ParsedUserOperation = ParsedUserOperation
  { puoSender :: Text
  , puoFactoryOwner :: Maybe Text
  , puoCalls :: [SmartCall]
  }
  deriving stock (Eq, Show)

data SmartCall = SmartCall
  { smartCallTarget :: Text
  , smartCallValue :: Integer
  , smartCallData :: ByteString
  }
  deriving stock (Eq, Show)

data ProxyFailure = ProxyFailure
  { pfStatus :: Status
  , pfCode :: Int
  , pfMessage :: Text
  , pfReason :: Text
  , pfRetryable :: Bool
  }
  deriving stock (Eq, Show)

newPimlicoProxyState :: IO PimlicoProxyState
newPimlicoProxyState = do
  now <- getCurrentTime
  rateWindows <- newTVarIO Map.empty
  gasUsage <- newTVarIO $ GasUsage now 0 Map.empty Map.empty False
  pure $ PimlicoProxyState rateWindows gasUsage

handlePimlicoProxy
  :: PimlicoProxyState
  -> Config
  -> EthClient
  -> Manager
  -> ActionM ()
handlePimlicoProxy proxyState cfg perpsClient manager =
  case cfgAaConfig cfg of
    Nothing ->
      respondFailure Null $
        unavailable "SPONSOR_UNAVAILABLE" "Managed gas sponsorship is not configured"
    Just aaCfg -> do
      suppliedToken <- header "X-Plether-AA-Proxy-Token"
      if
        not $
          maybe
            False
            (constantTimeTextEq $ aaProxyOriginToken aaCfg)
            (TL.toStrict <$> suppliedToken)
        then respondFailure Null $
          ProxyFailure status403 (-32001) "Forbidden" "PROXY_AUTH_FAILED" False
        else do
          clientIp <- header "CF-Connecting-IP"
          case (TL.toStrict <$> clientIp) >>= validateClientIp of
            Nothing ->
              respondFailure Null $
                invalidRequest "A trusted CF-Connecting-IP header is required"
            Just trustedIp -> do
              now <- liftCurrentTime
              preBodyAllowed <-
                liftRateCheck
                  proxyState
                  now
                  ("prebody-ip:" <> trustedIp)
                  (aaIpRateLimitPerMinute aaCfg * 4)
              if not preBodyAllowed
                then respondFailure Null rateLimited
                else do
                  waiRequest <- Scotty.request
                  requestBody <-
                    liftIO $
                      readBoundedRequestBody
                        (aaMaxRequestBytes aaCfg)
                        waiRequest
                  case requestBody of
                    Left () ->
                      respondFailure Null $
                        ProxyFailure status413 (-32600) "Request body is too large" "INVALID_REQUEST" False
                    Right boundedBody ->
                      case eitherDecode boundedBody of
                        Left _ ->
                          respondFailure Null $
                            ProxyFailure status400 (-32700) "Invalid JSON" "INVALID_REQUEST" False
                        Right value ->
                          case parseRpcRequest value of
                            Left failure -> respondFailure Null failure
                            Right rpcRequest -> do
                              let ipLimit =
                                    if isIssuanceMethod (rrMethod rpcRequest)
                                      then aaIpRateLimitPerMinute aaCfg
                                      else aaIpRateLimitPerMinute aaCfg * 4
                              ipAllowed <-
                                liftRateCheck proxyState now ("ip:" <> trustedIp) ipLimit
                              if not ipAllowed
                                then respondFailure (rrId rpcRequest) rateLimited
                                else
                                  handleAuthenticatedRequest
                                    proxyState
                                    cfg
                                    aaCfg
                                    perpsClient
                                    manager
                                    now
                                    trustedIp
                                    rpcRequest

handleAuthenticatedRequest
  :: PimlicoProxyState
  -> Config
  -> AaConfig
  -> EthClient
  -> Manager
  -> UTCTime
  -> Text
  -> RpcRequest
  -> ActionM ()
handleAuthenticatedRequest proxyState cfg aaCfg perpsClient manager now trustedIp rpcRequest
  | cfgPerpsChainId cfg /= 421614 =
      respondFailure (rrId rpcRequest) $
        unavailable "SPONSOR_UNAVAILABLE" "The backend Perps chain is not supported"
  | isIssuanceMethod (rrMethod rpcRequest) && not (aaSponsorshipEnabled aaCfg) =
      respondFailure (rrId rpcRequest) $
        unavailable "PAYMASTER_PAUSED" "Managed gas sponsorship is disabled"
  | otherwise =
      case validateMethodParams rpcRequest of
        Left failure -> respondFailure (rrId rpcRequest) failure
        Right mUserOperation ->
          case mUserOperation of
            Nothing -> relay rpcRequest
            Just parsed -> do
              accountAllowed <-
                liftRateCheck
                  proxyState
                  now
                  ("account-ip:" <> puoSender parsed <> ":" <> trustedIp)
                  (aaAccountRateLimitPerMinute aaCfg)
              if not accountAllowed
                then respondFailure (rrId rpcRequest) rateLimited
                else do
                  identity <- liftIdentityCheck perpsClient parsed
                  case identity of
                    Left failure -> respondFailure (rrId rpcRequest) failure
                    Right owner ->
                      case validateActionSequence cfg (puoSender parsed) owner (puoCalls parsed) of
                        Left failure -> respondFailure (rrId rpcRequest) failure
                        Right () -> relay rpcRequest
  where
    relay request =
      case injectSponsorshipPolicy aaCfg request of
        Left failure -> respondFailure (rrId request) failure
        Right rewritten -> do
          upstream <-
            liftUpstream
              manager
              (aaPimlicoApiKey aaCfg)
              rewritten
          case upstream of
            Left failure -> respondFailure (rrId request) failure
            Right (upstreamStatus, upstreamValue, retryAfter) -> do
              when (rrMethod request == SendUserOperation) $
                liftRecordSubmittedOperation proxyState now upstreamValue
              when (rrMethod request == GetUserOperationReceipt) $
                liftRecordReceiptCost proxyState aaCfg now request upstreamValue
              setHeader "Content-Type" "application/json"
              setHeader "Cache-Control" "no-store"
              maybe (pure ()) (setHeader "Retry-After" . TL.fromStrict) retryAfter
              status upstreamStatus
              json upstreamValue

parseRpcRequest :: Value -> Either ProxyFailure RpcRequest
parseRpcRequest (Array _) =
  Left $ invalidRequest "JSON-RPC batch requests are not supported"
parseRpcRequest (Object obj) = do
  let allowedKeys = Set.fromList $ map Key.fromText ["jsonrpc", "id", "method", "params"]
      unknownKeys = Set.fromList (KM.keys obj) `Set.difference` allowedKeys
  unless (Set.null unknownKeys) $
    Left $ invalidRequest "JSON-RPC request contains unsupported fields"
  case KM.lookup "jsonrpc" obj of
    Just (String "2.0") -> pure ()
    _ -> Left $ invalidRequest "jsonrpc must equal 2.0"
  requestId <-
    case KM.lookup "id" obj of
      Just value@(String _) -> Right value
      Just value@(Number _) -> Right value
      _ -> Left $ invalidRequest "JSON-RPC notifications are not supported"
  methodText <-
    case KM.lookup "method" obj of
      Just (String value) -> Right value
      _ -> Left $ invalidRequest "method must be a string"
  requestMethod <-
    maybe
      (Left $ ProxyFailure status200 (-32601) "Method not found" "METHOD_NOT_ALLOWED" False)
      Right
      (parseMethod methodText)
  requestParams <-
    case KM.lookup "params" obj of
      Nothing -> Right []
      Just (Array values) -> Right $ V.toList values
      _ -> Left $ invalidParams "params must be an array"
  pure $ RpcRequest requestId requestMethod requestParams obj
parseRpcRequest _ =
  Left $ invalidRequest "JSON-RPC request must be an object"

parseMethod :: Text -> Maybe PimlicoMethod
parseMethod = \case
  "pimlico_getUserOperationGasPrice" -> Just GetGasPrice
  "pm_getPaymasterStubData" -> Just GetPaymasterStubData
  "pm_getPaymasterData" -> Just GetPaymasterData
  "eth_estimateUserOperationGas" -> Just EstimateUserOperationGas
  "eth_sendUserOperation" -> Just SendUserOperation
  "eth_getUserOperationReceipt" -> Just GetUserOperationReceipt
  "eth_getUserOperationByHash" -> Just GetUserOperationByHash
  "pimlico_getUserOperationStatus" -> Just GetUserOperationStatus
  "eth_supportedEntryPoints" -> Just GetSupportedEntryPoints
  _ -> Nothing

validateMethodParams :: RpcRequest -> Either ProxyFailure (Maybe ParsedUserOperation)
validateMethodParams request =
  case rrMethod request of
    GetGasPrice -> emptyParams >> pure Nothing
    GetSupportedEntryPoints -> emptyParams >> pure Nothing
    GetUserOperationReceipt -> hashParams >> pure Nothing
    GetUserOperationByHash -> hashParams >> pure Nothing
    GetUserOperationStatus -> hashParams >> pure Nothing
    GetPaymasterStubData -> paymasterParams
    GetPaymasterData -> paymasterParams
    EstimateUserOperationGas -> operationParams
    SendUserOperation -> operationParams
  where
    emptyParams =
      unless (null $ rrParams request) $
        Left $ invalidParams "method does not accept parameters"
    hashParams =
      case rrParams request of
        [String userOperationHash]
          | isFixedHexBytes 32 userOperationHash -> Right ()
        _ -> Left $ invalidParams "method requires one 32-byte UserOperation hash"
    paymasterParams =
      case rrParams request of
        [Object operation, String entryPoint, String chainId, _]
          | normalizeAddress entryPoint == Just entryPointAddress
              && T.toLower chainId == arbitrumSepoliaHexChainId ->
              Just <$> parseUserOperation (rrMethod request) operation
        _ ->
          Left $
            invalidParams
              "paymaster method requires [userOperation, EntryPoint, Arbitrum Sepolia chain, context]"
    operationParams =
      case rrParams request of
        [Object operation, String entryPoint]
          | normalizeAddress entryPoint == Just entryPointAddress ->
              Just <$> parseUserOperation (rrMethod request) operation
        _ ->
          Left $
            invalidParams "method requires [userOperation, approved EntryPoint]"

parseUserOperation
  :: PimlicoMethod
  -> KM.KeyMap Value
  -> Either ProxyFailure ParsedUserOperation
parseUserOperation method operation = do
  let allowed =
        Set.fromList $
          map
            Key.fromText
            [ "sender"
            , "nonce"
            , "factory"
            , "factoryData"
            , "callData"
            , "callGasLimit"
            , "verificationGasLimit"
            , "preVerificationGas"
            , "maxFeePerGas"
            , "maxPriorityFeePerGas"
            , "paymaster"
            , "paymasterVerificationGasLimit"
            , "paymasterPostOpGasLimit"
            , "paymasterData"
            , "signature"
            ]
      unknown = Set.fromList (KM.keys operation) `Set.difference` allowed
  unless (Set.null unknown) $
    Left $ invalidParams "UserOperation contains unsupported fields"
  sender <- requiredAddress "sender"
  nonce <- requiredQuantity "nonce"
  when (nonce >= 2 ^ (64 :: Integer)) $
    Left $ invalidParams "UserOperation nonce must use nonce key zero"
  callDataText <- requiredText "callData"
  callData <- decodeHexField "callData" callDataText
  signature <- requiredText "signature"
  unless (isHex signature) $
    Left $ invalidParams "UserOperation signature must be hex bytes"
  if method == SendUserOperation
    then do
      unless (isFixedHexBytes 65 signature && T.toLower signature /= dummySignature) $
        Left $ invalidParams "submitted UserOperation must have a real 65-byte signature"
      _ <- requiredAddress "paymaster"
      _ <- requiredText "paymasterData" >>= decodeHexField "paymasterData"
      _ <- requiredQuantity "paymasterVerificationGasLimit"
      _ <- requiredQuantity "paymasterPostOpGasLimit"
      pure ()
    else
      unless (T.toLower signature == dummySignature) $
        Left $ invalidParams "prepared UserOperation must use the reviewed SimpleAccount stub signature"
  mapM_
    validateOptionalQuantity
    [ "callGasLimit"
    , "verificationGasLimit"
    , "preVerificationGas"
    , "maxFeePerGas"
    , "maxPriorityFeePerGas"
    , "paymasterVerificationGasLimit"
    , "paymasterPostOpGasLimit"
    ]
  validateOptionalAddress "paymaster"
  validateOptionalHex "paymasterData"
  priorityFee <- optionalQuantity "maxPriorityFeePerGas"
  maxFee <- optionalQuantity "maxFeePerGas"
  when (((>) <$> priorityFee <*> maxFee) == Just True) $
    Left $ invalidParams "maxPriorityFeePerGas cannot exceed maxFeePerGas"
  factoryOwner <- parseFactoryFields operation
  calls <- decodeSmartAccountCalls callData
  pure $ ParsedUserOperation sender factoryOwner calls
  where
    requiredText name =
      case KM.lookup (Key.fromText name) operation of
        Just (String value) -> Right value
        _ -> Left $ invalidParams $ "UserOperation." <> name <> " must be a string"
    requiredAddress name = do
      raw <- requiredText name
      maybe
        (Left $ invalidParams $ "UserOperation." <> name <> " must be an address")
        Right
        (normalizeAddress raw)
    requiredQuantity name = do
      raw <- requiredText name
      maybe
        (Left $ invalidParams $ "UserOperation." <> name <> " must be a canonical quantity")
        Right
        (parseQuantity raw)
    optionalQuantity name =
      case KM.lookup (Key.fromText name) operation of
        Nothing -> Right Nothing
        Just (String raw) ->
          maybe
            (Left $ invalidParams $ "UserOperation." <> name <> " must be a canonical quantity")
            (Right . Just)
            (parseQuantity raw)
        _ -> Left $ invalidParams $ "UserOperation." <> name <> " must be a string"
    validateOptionalQuantity name = optionalQuantity name >> pure ()
    validateOptionalAddress name =
      case KM.lookup (Key.fromText name) operation of
        Nothing -> Right ()
        Just (String raw)
          | isJust (normalizeAddress raw) -> Right ()
        _ -> Left $ invalidParams $ "UserOperation." <> name <> " must be an address"
    validateOptionalHex name =
      case KM.lookup (Key.fromText name) operation of
        Nothing -> Right ()
        Just (String raw)
          | isHex raw -> Right ()
        _ -> Left $ invalidParams $ "UserOperation." <> name <> " must be hex bytes"

parseFactoryFields :: KM.KeyMap Value -> Either ProxyFailure (Maybe Text)
parseFactoryFields operation =
  case (KM.lookup "factory" operation, KM.lookup "factoryData" operation) of
    (Nothing, Nothing) -> Right Nothing
    (Just (String factory), Just (String factoryData))
      | normalizeAddress factory == Just simpleAccountFactory -> do
          bytes <- decodeHexField "factoryData" factoryData
          if BS.length bytes /= 68 || BS.take 4 bytes /= selectorCreateAccount
            then Left $ invalidParams "factoryData must be canonical createAccount(owner, 0)"
            else do
              owner <- decodeAddressWord $ BS.take 32 $ BS.drop 4 bytes
              let index = bytesToInteger $ BS.drop 36 bytes
              if owner == zeroAddress || index /= 0
                then Left $ invalidParams "factoryData owner must be nonzero and index must be zero"
                else Right $ Just owner
    (Just _, Just _) ->
      Left $ invalidParams "factory and factoryData must use the reviewed SimpleAccount factory"
    _ ->
      Left $ invalidParams "factory and factoryData must be supplied together"

decodeSmartAccountCalls :: ByteString -> Either ProxyFailure [SmartCall]
decodeSmartAccountCalls callData
  | BS.take 4 callData == selectorExecute =
      (: []) <$> decodeExecute callData
  | BS.take 4 callData == selectorExecuteBatch =
      decodeExecuteBatch callData
  | otherwise =
      Left $ policyDenied "Smart-account callData is not an approved execute method"

decodeExecute :: ByteString -> Either ProxyFailure SmartCall
decodeExecute callData = do
  let payload = BS.drop 4 callData
  unless (BS.length payload >= 128) $
    Left $ policyDenied "execute calldata is truncated"
  target <- wordAddress payload 0
  value <- wordInteger payload 32
  offset <- wordInteger payload 64
  unless (offset == 96) $
    Left $ policyDenied "execute bytes offset is not canonical"
  (innerData, end) <- dynamicBytes payload 96
  unless (end == BS.length payload) $
    Left $ policyDenied "execute calldata has trailing bytes"
  pure $ SmartCall target value innerData

decodeExecuteBatch :: ByteString -> Either ProxyFailure [SmartCall]
decodeExecuteBatch callData = do
  let payload = BS.drop 4 callData
  rootOffset <- wordInteger payload 0
  unless (rootOffset == 32) $
    Left $ policyDenied "executeBatch root offset is not canonical"
  countInteger <- wordInteger payload 32
  unless (countInteger > 0 && countInteger <= 2) $
    Left $ policyDenied "executeBatch must contain one or two calls"
  let count = fromInteger countInteger
      elementsBase = 64
      tableEnd = elementsBase + count * 32
  unless (tableEnd <= BS.length payload) $
    Left $ policyDenied "executeBatch offset table is truncated"
  (calls, end) <- go payload elementsBase tableEnd 0 count []
  unless (end == BS.length payload) $
    Left $ policyDenied "executeBatch calldata has trailing bytes"
  pure $ reverse calls
  where
    go payload elementsBase expectedStart index count acc
      | index == count = Right (acc, expectedStart)
      | otherwise = do
          relativeOffset <- wordInteger payload (elementsBase + index * 32)
          let tupleStart = elementsBase + fromInteger relativeOffset
          unless (tupleStart == expectedStart) $
            Left $ policyDenied "executeBatch tuple offsets are not canonical"
          target <- wordAddress payload tupleStart
          value <- wordInteger payload (tupleStart + 32)
          bytesOffset <- wordInteger payload (tupleStart + 64)
          unless (bytesOffset == 96) $
            Left $ policyDenied "executeBatch tuple bytes offset is not canonical"
          (innerData, nextStart) <- dynamicBytes payload (tupleStart + 96)
          go
            payload
            elementsBase
            nextStart
            (index + 1)
            count
            (SmartCall target value innerData : acc)

validateActionSequence
  :: Config
  -> Text
  -> Text
  -> [SmartCall]
  -> Either ProxyFailure ()
validateActionSequence cfg sender owner calls = do
  unless
    ( all
        isJust
        [ approvedConfigAddress $ cfgPerpsUsdc cfg
        , approvedConfigAddress $ cfgPerpsMarginClearinghouse cfg
        , approvedConfigAddress $ cfgPerpsOrderRouter cfg
        , approvedConfigAddress $ cfgPerpsCfdEngine cfg
        ]
    )
    $ Left $
      unavailable "SPONSOR_UNAVAILABLE" "Managed sponsorship contract configuration is invalid"
  unless (all ((== 0) . smartCallValue) calls) $
    Left $ policyDenied "Sponsored calls must send zero native value"
  case calls of
    [first, second]
      | smartCallTarget first == usdc
          && smartCallTarget second == clearinghouse ->
          validateDeposit first second
      | smartCallTarget first == clearinghouse
          && smartCallTarget second == usdc ->
          validateWithdrawal first second
      | otherwise ->
          Left $ policyDenied "The two-call sequence is not an approved Plether action"
    [single] -> validateSingle single
    _ -> Left $ policyDenied "The call sequence is not an approved Plether action"
  where
    usdc = normalizedConfigAddress $ cfgPerpsUsdc cfg
    clearinghouse = normalizedConfigAddress $ cfgPerpsMarginClearinghouse cfg
    router = normalizedConfigAddress $ cfgPerpsOrderRouter cfg
    engine = normalizedConfigAddress $ cfgPerpsCfdEngine cfg

    validateDeposit approval deposit = do
      unless
        (smartCallTarget approval == usdc && smartCallTarget deposit == clearinghouse)
        $ Left $ policyDenied "Deposit targets are not approved"
      (spender, approvedAmount) <-
        decodeAddressUintCall selectorApprove (smartCallData approval)
      depositedAmount <-
        decodeUintCall selectorDepositMargin (smartCallData deposit)
      unless
        (spender == clearinghouse && approvedAmount > 0 && approvedAmount == depositedAmount)
        $ Left $ policyDenied "Deposit approval and margin amount must match"

    validateWithdrawal withdrawal transfer = do
      unless
        (smartCallTarget withdrawal == clearinghouse && smartCallTarget transfer == usdc)
        $ Left $ policyDenied "Withdrawal targets are not approved"
      withdrawnAmount <-
        decodeUintCall selectorWithdrawMargin (smartCallData withdrawal)
      (recipient, transferredAmount) <-
        decodeAddressUintCall selectorTransfer (smartCallData transfer)
      unless
        ( recipient == owner
            && withdrawnAmount > 0
            && withdrawnAmount == transferredAmount
        )
        $ Left $ policyDenied "Withdrawal must transfer the same amount to the verified owner"

    validateSingle single
      | smartCallTarget single == router =
          validateOrder $ smartCallData single
      | smartCallTarget single == engine =
          validateEngineCall $ smartCallData single
      | otherwise =
          Left $ policyDenied "Single-call action target is not approved"

    validateOrder dataBytes = do
      words' <- fixedWords selectorCommitOrder 5 dataBytes
      case map bytesToInteger words' of
        [side, sizeDelta, marginDelta, _targetPrice, closeWord] -> do
          unless
            ((side == 0 || side == 1) && sizeDelta > 0 && (closeWord == 0 || closeWord == 1))
            $ Left $ policyDenied "Order arguments are invalid"
          when (closeWord == 1 && marginDelta /= 0) $
            Left $ policyDenied "Close orders must have zero margin delta"
        _ -> Left $ policyDenied "Order calldata shape is invalid"

    validateEngineCall dataBytes
      | BS.take 4 dataBytes == selectorAddMargin = do
          (account, amount) <- decodeAddressUintCall selectorAddMargin dataBytes
          unless (account == sender && amount > 0) $
            Left $ policyDenied "addMargin account must equal the Trading Account"
      | BS.take 4 dataBytes == selectorSettleTraderClaim = do
          account <- decodeAddressCall selectorSettleTraderClaim dataBytes
          unless (account == sender) $
            Left $ policyDenied "claim account must equal the Trading Account"
      | otherwise =
          Left $ policyDenied "CFD engine selector is not approved"

injectSponsorshipPolicy
  :: AaConfig
  -> RpcRequest
  -> Either ProxyFailure RpcRequest
injectSponsorshipPolicy aaCfg request
  | rrMethod request `elem` [GetPaymasterStubData, GetPaymasterData] =
      case rrParams request of
        [operation, entryPoint, chainId, _] ->
          let context =
                object
                  [ "sponsorshipPolicyId" .= aaSponsorshipPolicyId aaCfg
                  ]
              rewrittenParams = [operation, entryPoint, chainId, context]
              rewrittenObject =
                KM.insert "params" (Array $ V.fromList rewrittenParams) (rrObject request)
           in Right
                request
                  { rrParams = rewrittenParams
                  , rrObject = rewrittenObject
                  }
        _ -> Left $ invalidParams "paymaster request parameters are invalid"
  | otherwise = Right request

verifyAccountIdentity
  :: EthClient
  -> ParsedUserOperation
  -> IO (Either ProxyFailure Text)
verifyAccountIdentity client operation =
  case puoFactoryOwner operation of
    Just owner -> verifyCounterfactual owner
    Nothing -> verifyDeployedAccountIdentity client sender
  where
    sender = puoSender operation

    verifyCounterfactual owner = do
      (factoryImplementation, code, expected) <-
        runConcurrently $
          (,,)
            <$> Concurrently
              ( readContractAddress
                  client
                  simpleAccountFactory
                  selectorAccountImplementation
              )
            <*> Concurrently (readCode client sender)
            <*> Concurrently (readFactoryAddress client owner)
      pure $ do
        implementation <- factoryImplementation
        unless (implementation == simpleAccountImplementation) $
          Left $ accountNotTrusted "SimpleAccount factory implementation drifted"
        accountCode <- code
        expectedSender <- expected
        unless (BS.null accountCode) $
          Left $ accountNotTrusted "Counterfactual UserOperation sender is already deployed"
        unless (expectedSender == sender) $
          Left $ accountNotTrusted "Factory owner/index does not derive the UserOperation sender"
        Right owner


-- | Resolve a submitted owner EOA to its deterministic index-0 Trading Account,
-- while preserving a submitted address only when it is an approved deployed
-- Trading Account. This keeps roster repair on the configured private RPC.
resolveTradingAccountAddress :: EthClient -> Text -> IO (Either Text Text)
resolveTradingAccountAddress client rawAddress =
  case normalizeAddress rawAddress of
    Nothing -> pure $ Left "Submitted wallet is not a valid Ethereum address"
    Just submitted -> do
      codeResult <- readCode client submitted
      case codeResult of
        Left failure -> pure $ Left $ pfMessage failure
        Right code
          | BS.null code -> do
              factoryImplementation <-
                readContractAddress
                  client
                  simpleAccountFactory
                  selectorAccountImplementation
              derived <- readFactoryAddress client submitted
              pure $ do
                implementation <- firstFailure factoryImplementation
                unless (implementation == simpleAccountImplementation) $
                  Left "SimpleAccount factory implementation drifted"
                firstFailure derived
          | otherwise -> do
              verified <- verifyDeployedAccountIdentity client submitted
              pure $ submitted <$ firstFailure verified
  where
    firstFailure = either (Left . pfMessage) Right

verifyDeployedAccountIdentity
  :: EthClient
  -> Text
  -> IO (Either ProxyFailure Text)
verifyDeployedAccountIdentity client sender = do
  ( factoryImplementation
    , code
    , ownerResult
    , accountEntryPoint
    , implementationSlot
    , beaconSlot
    ) <-
      runConcurrently $
        (,,,,,)
          <$> Concurrently
            ( readContractAddress
                client
                simpleAccountFactory
                selectorAccountImplementation
            )
          <*> Concurrently (readCode client sender)
          <*> Concurrently
            (readContractAddress client sender selectorOwner)
          <*> Concurrently
            (readContractAddress client sender selectorEntryPoint)
          <*> Concurrently
            (readStorageWord client sender erc1967ImplementationSlot)
          <*> Concurrently
            (readStorageWord client sender erc1967BeaconSlot)
  case factoryImplementation of
    Left failure -> pure $ Left failure
    Right implementation
      | implementation /= simpleAccountImplementation ->
          pure $ Left $ accountNotTrusted "SimpleAccount factory implementation drifted"
      | otherwise ->
          case ownerResult of
            Left failure -> pure $ Left failure
            Right owner -> do
              expected <- readFactoryAddress client owner
              pure $ do
                accountCode <- code
                expectedSender <- expected
                actualEntryPoint <- accountEntryPoint
                actualImplementation <- implementationSlot
                actualBeacon <- beaconSlot
                when (BS.null accountCode) $
                  Left $ accountNotTrusted "Deployed UserOperation sender has no code"
                when (owner == zeroAddress || expectedSender /= sender) $
                  Left $ accountNotTrusted "Trading Account owner does not derive the sender"
                unless (actualEntryPoint == entryPointAddress) $
                  Left $ accountNotTrusted "Trading Account EntryPoint is not approved"
                unless (T.toLower actualImplementation == implementationWord) $
                  Left $ accountNotTrusted "Trading Account implementation is not approved"
                unless (T.toLower actualBeacon == zeroWord) $
                  Left $ accountNotTrusted "Beacon-based Trading Accounts are not approved"
                Right owner

readFactoryAddress :: EthClient -> Text -> IO (Either ProxyFailure Text)
readFactoryAddress client owner =
  readContractAddress
    client
    simpleAccountFactory
    (encodeCall "getAddress(address,uint256)" [encodeAddress owner, encodeUint256 0])

readContractAddress
  :: EthClient
  -> Text
  -> ByteString
  -> IO (Either ProxyFailure Text)
readContractAddress client target calldata = do
  result <- ethCall client $ CallParams target calldata
  pure $ case result of
    Left _ -> Left $ accountValidationUnavailable
    Right word ->
      case decodeAddressWord word of
        Left _ -> Left $ accountValidationUnavailable
        Right value -> Right value

readCode :: EthClient -> Text -> IO (Either ProxyFailure ByteString)
readCode client account = do
  result <- rpcCall client "eth_getCode" $ toJSON [String account, String "latest"]
  pure $ case result of
    Left _ -> Left accountValidationUnavailable
    Right (String value) ->
      maybe
        (Left accountValidationUnavailable)
        Right
        (decodeHex value)
    Right _ -> Left accountValidationUnavailable

readStorageWord
  :: EthClient
  -> Text
  -> Text
  -> IO (Either ProxyFailure Text)
readStorageWord client account slot = do
  result <-
    rpcCall client "eth_getStorageAt" $
      toJSON [String account, String slot, String "latest"]
  pure $ case result of
    Left _ -> Left accountValidationUnavailable
    Right (String value)
      | isFixedHexBytes 32 value -> Right $ T.toLower value
    _ -> Left accountValidationUnavailable

forwardUpstream
  :: Manager
  -> Text
  -> RpcRequest
  -> IO (Either ProxyFailure (Status, Value, Maybe Text))
forwardUpstream manager apiKey request = do
  outcome <- try @HttpException $ do
    baseRequest <- parseRequest pimlicoRpcUrl
    let upstreamRequest =
          setQueryString
            [("apikey", Just $ TE.encodeUtf8 apiKey)]
            baseRequest
          { method = "POST"
          , requestHeaders =
              [ ("Content-Type", "application/json")
              , ("Accept", "application/json")
              ]
          , requestBody = RequestBodyLBS $ encode $ Object $ rrObject request
          , responseTimeout = responseTimeoutMicro 20_000_000
          , checkResponse = \_ _ -> pure ()
          }
    response <- httpLbs upstreamRequest manager
    pure
      ( responseStatus response
      , responseBody response
      , lookup hRetryAfter $ responseHeaders response
      )
  pure $ case outcome of
    Left _ ->
      Left $ unavailable "SPONSOR_UNAVAILABLE" "Pimlico is temporarily unavailable"
    Right (_upstreamStatus, upstreamBody, retryAfterBytes) ->
      case eitherDecode upstreamBody of
        Left _ ->
          Left $
            ProxyFailure
              status200
              (-32002)
              "Pimlico returned an invalid response"
              "SPONSOR_UNAVAILABLE"
              True
        Right upstreamValue@(Object upstreamObject)
          | KM.lookup "id" upstreamObject == Just (rrId request) ->
              Right
                ( status200
                , upstreamValue
                , TE.decodeUtf8' <$> retryAfterBytes >>= either (const Nothing) Just
                )
        Right _ ->
          Left $
            ProxyFailure
              status200
              (-32002)
              "Pimlico returned a mismatched response"
              "SPONSOR_UNAVAILABLE"
              True

recordReceiptCost
  :: PimlicoProxyState
  -> AaConfig
  -> UTCTime
  -> RpcRequest
  -> Value
  -> IO ()
recordReceiptCost proxyState aaCfg now request response =
  case (rrParams request, receiptActualGasCost response) of
    ([String userOperationHash], Just actualGasCost)
      | aaSponsoredGasAlertWeiPerHour aaCfg > 0 -> do
          let normalizedHash = T.toLower userOperationHash
          alertTotal <-
            atomically $ do
              current <- readTVar $ ppsGasUsage proxyState
              let withinHour = diffUTCTime now (guStartedAt current) < 3600
                  recentSubmissions =
                    Map.filter (\submittedAt -> diffUTCTime now submittedAt < 86400) $
                      guSubmittedOperations current
                  recentReceipts =
                    Map.filter (\seenAt -> diffUTCTime now seenAt < 86400) $
                      guSeenReceipts current
                  base =
                    if withinHour
                      then
                        current
                          { guSubmittedOperations = recentSubmissions
                          , guSeenReceipts = recentReceipts
                          }
                      else GasUsage now 0 recentSubmissions recentReceipts False
              if
                not (Map.member normalizedHash $ guSubmittedOperations base)
                  || Map.member normalizedHash (guSeenReceipts base)
                then pure Nothing
                else do
                  let total = guActualGasCost base + actualGasCost
                      shouldAlert =
                        not (guAlerted base)
                          && total >= aaSponsoredGasAlertWeiPerHour aaCfg
                      next =
                        base
                          { guActualGasCost = total
                          , guSubmittedOperations =
                              Map.delete normalizedHash $ guSubmittedOperations base
                          , guSeenReceipts = Map.insert normalizedHash now (guSeenReceipts base)
                          , guAlerted = guAlerted base || shouldAlert
                          }
                  writeTVar (ppsGasUsage proxyState) next
                  pure $ if shouldAlert then Just total else Nothing
          case alertTotal of
            Just total ->
              putStrLn $
                "AA sponsored gas alert: observed actualGasCost reached "
                  <> show total
                  <> " wei in the current hour"
            Nothing -> pure ()
    _ -> pure ()

recordSubmittedOperation
  :: PimlicoProxyState
  -> UTCTime
  -> Value
  -> IO ()
recordSubmittedOperation proxyState now response =
  case submittedUserOperationHash response of
    Nothing -> pure ()
    Just userOperationHash ->
      atomically $
        modifyTVar' (ppsGasUsage proxyState) $ \current ->
          current
            { guSubmittedOperations =
                Map.insert userOperationHash now $
                  Map.filter
                    (\submittedAt -> diffUTCTime now submittedAt < 86400)
                    (guSubmittedOperations current)
            }

submittedUserOperationHash :: Value -> Maybe Text
submittedUserOperationHash (Object response) = do
  String userOperationHash <- KM.lookup "result" response
  if isFixedHexBytes 32 userOperationHash
    then Just $ T.toLower userOperationHash
    else Nothing
submittedUserOperationHash _ = Nothing

receiptActualGasCost :: Value -> Maybe Integer
receiptActualGasCost (Object response) = do
  Object receipt <- KM.lookup "result" response
  String rawCost <- KM.lookup "actualGasCost" receipt
  parseQuantity rawCost
receiptActualGasCost _ = Nothing

readBoundedRequestBody
  :: Int
  -> Wai.Request
  -> IO (Either () LBS.ByteString)
readBoundedRequestBody maxBytes request' = go 0 []
  where
    go total chunks = do
      chunk <- Wai.getRequestBodyChunk request'
      if BS.null chunk
        then pure $ Right $ LBS.fromChunks $ reverse chunks
        else do
          let nextTotal = total + BS.length chunk
          if nextTotal > maxBytes
            then pure $ Left ()
            else go nextTotal (chunk : chunks)

checkRateLimit
  :: PimlicoProxyState
  -> UTCTime
  -> Text
  -> Int
  -> IO Bool
checkRateLimit proxyState now key limit =
  atomically $ do
    windows <- readTVar $ ppsRateWindows proxyState
    let activeWindows =
          Map.filter
            (\window -> diffUTCTime now (rwStartedAt window) < rateWindowSeconds)
            windows
    let current =
          case Map.lookup key activeWindows of
            Just window -> window
            _ -> RateWindow now 0
    if rwCount current >= limit
      then pure False
      else do
        let updated = current {rwCount = rwCount current + 1}
        writeTVar (ppsRateWindows proxyState) $ Map.insert key updated activeWindows
        pure True

rateWindowSeconds :: NominalDiffTime
rateWindowSeconds = 60

validateClientIp :: Text -> Maybe Text
validateClientIp raw =
  let value = T.strip raw
      validChar char =
        (char >= '0' && char <= '9')
          || (char >= 'a' && char <= 'f')
          || (char >= 'A' && char <= 'F')
          || char == '.'
          || char == ':'
   in if
        T.null value
          || T.length value > 45
          || T.any (not . validChar) value
        then Nothing
        else Just $ T.toLower value

constantTimeTextEq :: Text -> Text -> Bool
constantTimeTextEq expected supplied =
  constEq (TE.encodeUtf8 expected) (TE.encodeUtf8 supplied)

isIssuanceMethod :: PimlicoMethod -> Bool
isIssuanceMethod method =
  method
    `elem` [ GetPaymasterStubData
           , GetPaymasterData
           , EstimateUserOperationGas
           , SendUserOperation
           ]

respondFailure :: Value -> ProxyFailure -> ActionM ()
respondFailure requestId failure = do
  setHeader "Content-Type" "application/json"
  setHeader "Cache-Control" "no-store"
  status $ pfStatus failure
  json $
    object
      [ "jsonrpc" .= ("2.0" :: Text)
      , "id" .= requestId
      , "error"
          .= object
            [ "code" .= pfCode failure
            , "message" .= pfMessage failure
            , "data"
                .= object
                  [ "reason" .= pfReason failure
                  , "retryable" .= pfRetryable failure
                  ]
            ]
      ]

invalidRequest :: Text -> ProxyFailure
invalidRequest message =
  ProxyFailure status400 (-32600) message "INVALID_REQUEST" False

invalidParams :: Text -> ProxyFailure
invalidParams message =
  ProxyFailure status200 (-32602) message "POLICY_DENIED" False

policyDenied :: Text -> ProxyFailure
policyDenied message =
  ProxyFailure status200 (-32501) message "POLICY_DENIED" False

accountNotTrusted :: Text -> ProxyFailure
accountNotTrusted message =
  ProxyFailure status200 (-32501) message "ACCOUNT_NOT_TRUSTED" False

unavailable :: Text -> Text -> ProxyFailure
unavailable reason message =
  ProxyFailure status200 (-32002) message reason True

rateLimited :: ProxyFailure
rateLimited =
  ProxyFailure status200 (-32005) "Rate limit exceeded" "RATE_LIMITED" True

accountValidationUnavailable :: ProxyFailure
accountValidationUnavailable =
  unavailable "SPONSOR_UNAVAILABLE" "Trading Account validation is temporarily unavailable"

normalizeAddress :: Text -> Maybe Text
normalizeAddress raw =
  let value = T.toLower $ T.strip raw
      hexPart = T.drop 2 value
   in if
        T.isPrefixOf "0x" value
          && T.length hexPart == 40
          && T.all isHexChar hexPart
        then Just value
        else Nothing

normalizedConfigAddress :: Text -> Text
normalizedConfigAddress = maybe zeroAddress id . normalizeAddress

approvedConfigAddress :: Text -> Maybe Text
approvedConfigAddress raw = do
  address <- normalizeAddress raw
  if address == zeroAddress then Nothing else Just address

zeroAddress :: Text
zeroAddress = "0x0000000000000000000000000000000000000000"

isHex :: Text -> Bool
isHex value =
  T.isPrefixOf "0x" value
    && even (T.length $ T.drop 2 value)
    && T.all isHexChar (T.drop 2 value)

isFixedHexBytes :: Int -> Text -> Bool
isFixedHexBytes bytes value =
  isHex value && T.length (T.drop 2 value) == bytes * 2

decodeHex :: Text -> Maybe ByteString
decodeHex value
  | not (isHex value) = Nothing
  | otherwise =
      case B16.decode $ TE.encodeUtf8 $ T.toLower $ T.drop 2 value of
        Right bytes -> Just bytes
        Left _ -> Nothing

decodeHexField :: Text -> Text -> Either ProxyFailure ByteString
decodeHexField name value =
  maybe
    (Left $ invalidParams $ name <> " must be canonical hex bytes")
    Right
    (decodeHex value)

parseQuantity :: Text -> Maybe Integer
parseQuantity raw =
  let value = T.toLower raw
      digits = T.drop 2 value
   in if
        not (T.isPrefixOf "0x" value)
          || T.null digits
          || T.length digits > 64
          || T.any (not . isHexChar) digits
          || (T.length digits > 1 && T.head digits == '0')
        then Nothing
        else Just $ T.foldl' (\total digit -> total * 16 + hexDigit digit) 0 digits

isHexChar :: Char -> Bool
isHexChar char =
  (char >= '0' && char <= '9')
    || (char >= 'a' && char <= 'f')
    || (char >= 'A' && char <= 'F')

hexDigit :: Char -> Integer
hexDigit char
  | char >= '0' && char <= '9' = fromIntegral $ fromEnum char - fromEnum '0'
  | char >= 'a' && char <= 'f' = fromIntegral $ fromEnum char - fromEnum 'a' + 10
  | otherwise = fromIntegral $ fromEnum char - fromEnum 'A' + 10

wordInteger :: ByteString -> Int -> Either ProxyFailure Integer
wordInteger bytes offset = bytesToInteger <$> wordAt bytes offset

wordAddress :: ByteString -> Int -> Either ProxyFailure Text
wordAddress bytes offset = wordAt bytes offset >>= decodeAddressWord

wordAt :: ByteString -> Int -> Either ProxyFailure ByteString
wordAt bytes offset
  | offset < 0 || offset + 32 > BS.length bytes =
      Left $ policyDenied "ABI word is truncated"
  | otherwise = Right $ BS.take 32 $ BS.drop offset bytes

decodeAddressWord :: ByteString -> Either ProxyFailure Text
decodeAddressWord word
  | BS.length word /= 32 || BS.any (/= 0) (BS.take 12 word) =
      Left $ policyDenied "ABI address word is not canonical"
  | otherwise =
      Right $ "0x" <> TE.decodeUtf8 (B16.encode $ BS.drop 12 word)

dynamicBytes
  :: ByteString
  -> Int
  -> Either ProxyFailure (ByteString, Int)
dynamicBytes bytes lengthOffset = do
  lengthInteger <- wordInteger bytes lengthOffset
  when (lengthInteger > fromIntegral (maxBound :: Int)) $
    Left $ policyDenied "Dynamic ABI length overflows decoder bounds"
  let length' = fromInteger lengthInteger
      start = lengthOffset + 32
      paddedLength = ((length' + 31) `div` 32) * 32
      end = start + paddedLength
  when (end > BS.length bytes) $
    Left $ policyDenied "Dynamic ABI bytes are truncated"
  let value = BS.take length' $ BS.drop start bytes
      padding = BS.take (paddedLength - length') $ BS.drop (start + length') bytes
  unless (BS.all (== 0) padding) $
    Left $ policyDenied "Dynamic ABI bytes have nonzero padding"
  pure (value, end)

fixedWords
  :: ByteString
  -> Int
  -> ByteString
  -> Either ProxyFailure [ByteString]
fixedWords expectedSelector count calldata = do
  unless (BS.length calldata == 4 + count * 32 && BS.take 4 calldata == expectedSelector) $
    Left $ policyDenied "Nested calldata selector or length is not approved"
  traverse
    (\index -> wordAt (BS.drop 4 calldata) $ index * 32)
    [0 .. count - 1]

decodeUintCall :: ByteString -> ByteString -> Either ProxyFailure Integer
decodeUintCall expectedSelector calldata = do
  words' <- fixedWords expectedSelector 1 calldata
  case words' of
    [amountWord] -> pure $ bytesToInteger amountWord
    _ -> Left $ policyDenied "Nested calldata shape is invalid"

decodeAddressCall :: ByteString -> ByteString -> Either ProxyFailure Text
decodeAddressCall expectedSelector calldata = do
  words' <- fixedWords expectedSelector 1 calldata
  case words' of
    [addressWord] -> decodeAddressWord addressWord
    _ -> Left $ policyDenied "Nested calldata shape is invalid"

decodeAddressUintCall
  :: ByteString
  -> ByteString
  -> Either ProxyFailure (Text, Integer)
decodeAddressUintCall expectedSelector calldata = do
  words' <- fixedWords expectedSelector 2 calldata
  case words' of
    [addressWord, amountWord] -> do
      address <- decodeAddressWord addressWord
      pure (address, bytesToInteger amountWord)
    _ -> Left $ policyDenied "Nested calldata shape is invalid"

bytesToInteger :: ByteString -> Integer
bytesToInteger = BS.foldl' (\total byte -> total * 256 + fromIntegral byte) 0

selectorExecute :: ByteString
selectorExecute = decodeSelector "b61d27f6"

selectorExecuteBatch :: ByteString
selectorExecuteBatch = decodeSelector "34fcd5be"

selectorCreateAccount :: ByteString
selectorCreateAccount = decodeSelector "5fbfb9cf"

selectorOwner :: ByteString
selectorOwner = decodeSelector "8da5cb5b"

selectorEntryPoint :: ByteString
selectorEntryPoint = decodeSelector "b0d691fe"

selectorAccountImplementation :: ByteString
selectorAccountImplementation = decodeSelector "11464fbe"

selectorApprove :: ByteString
selectorApprove = decodeSelector "095ea7b3"

selectorTransfer :: ByteString
selectorTransfer = decodeSelector "a9059cbb"

selectorDepositMargin :: ByteString
selectorDepositMargin = decodeSelector "19bd1776"

selectorWithdrawMargin :: ByteString
selectorWithdrawMargin = decodeSelector "0cea7534"

selectorCommitOrder :: ByteString
selectorCommitOrder = decodeSelector "878f1de2"

selectorAddMargin :: ByteString
selectorAddMargin = decodeSelector "cf70cb69"

selectorSettleTraderClaim :: ByteString
selectorSettleTraderClaim = decodeSelector "258b1605"

decodeSelector :: ByteString -> ByteString
decodeSelector value =
  case B16.decode value of
    Right bytes -> bytes
    Left _ -> BS.empty

liftCurrentTime :: ActionM UTCTime
liftCurrentTime = liftIO getCurrentTime

liftRateCheck
  :: PimlicoProxyState
  -> UTCTime
  -> Text
  -> Int
  -> ActionM Bool
liftRateCheck state now key limit =
  liftIO $ checkRateLimit state now key limit

liftIdentityCheck
  :: EthClient
  -> ParsedUserOperation
  -> ActionM (Either ProxyFailure Text)
liftIdentityCheck client operation =
  liftIO $ verifyAccountIdentity client operation

liftUpstream
  :: Manager
  -> Text
  -> RpcRequest
  -> ActionM (Either ProxyFailure (Status, Value, Maybe Text))
liftUpstream manager apiKey request =
  liftIO $ forwardUpstream manager apiKey request

liftRecordReceiptCost
  :: PimlicoProxyState
  -> AaConfig
  -> UTCTime
  -> RpcRequest
  -> Value
  -> ActionM ()
liftRecordReceiptCost state aaCfg now request response =
  liftIO $ recordReceiptCost state aaCfg now request response

liftRecordSubmittedOperation
  :: PimlicoProxyState
  -> UTCTime
  -> Value
  -> ActionM ()
liftRecordSubmittedOperation state now response =
  liftIO $ recordSubmittedOperation state now response
