module Plether.Ethereum.Client
  ( EthClient
  , RpcClientOptions (..)
  , RpcError (..)
  , newClient
  , newClientWithOptions
  , newClientWithManager
  , rpcHttpExceptionText
  , rpcCall
  , ethCall
  , ethCallAt
  , ethCallWithValue
  , ethCallWithTransactionGas
  , ethCallAtBlock
  , ethBlockNumber
  , parseRpcQuantity
  , parseRpcData
  , CallParams (..)
  , BlockTag (..)
  , renderBlockTag
  ) where

import Control.Exception
  ( SomeAsyncException
  , SomeException
  , fromException
  , throwIO
  , try
  )
import Control.Monad (when)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object, withObject, (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word64)
import GHC.Generics (Generic)
import GHC.Clock (getMonotonicTimeNSec)
import Plether.Utils.Hex (hexToInteger, intToHex)
import Plether.Logging (field, logInfo, logWarnEvery)
import Network.HTTP.Client
  ( HttpException (..)
  , HttpExceptionContent (..)
  , Manager
  , Request (..)
  , RequestBody (..)
  , httpLbs
  , newManager
  , parseRequest
  , responseBody
  , responseStatus
  )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import System.IO.Unsafe (unsafePerformIO)

data EthClient = EthClient
  { clientManager :: Manager
  , clientRpcUrl :: Text
  , clientBearerToken :: Maybe Text
  , clientRole :: Text
  , clientRequestId :: IORef Integer
  }

data RpcClientOptions = RpcClientOptions
  { rcoEndpoint :: Text
  , rcoBearerToken :: Maybe Text
  , rcoRole :: Text
  }
  deriving stock (Eq)

instance Show RpcClientOptions where
  show options =
    "RpcClientOptions {rcoEndpoint = <redacted>, rcoBearerTokenConfigured = "
      <> show (maybe False (const True) $ rcoBearerToken options)
      <> ", rcoRole = "
      <> show (rcoRole options)
      <> "}"

data RpcMethodStats = RpcMethodStats
  { rmsRequestCount :: !Integer
  , rmsFailureCount :: !Integer
  , rmsTotalDurationNs :: !Word64
  , rmsMaxDurationNs :: !Word64
  }

data RpcMetricsState = RpcMetricsState
  { rmsWindowStartedAtNs :: !Word64
  , rmsMethods :: !(Map (Text, Text) RpcMethodStats)
  }

data RpcError
  = RpcHttpError Text
  | RpcJsonError Text
  | RpcNodeError Int Text (Maybe Text)
  deriving stock (Show, Eq)

data RpcRequest = RpcRequest
  { rpcMethod :: Text
  , rpcParams :: Value
  , rpcId :: Integer
  }
  deriving stock (Generic)

instance ToJSON RpcRequest where
  toJSON RpcRequest {..} =
    object
      [ "jsonrpc" .= ("2.0" :: Text)
      , "method" .= rpcMethod
      , "params" .= rpcParams
      , "id" .= rpcId
      ]

data RpcResponse = RpcResponse
  { rpcResult :: Maybe Value
  , rpcError :: Maybe RpcResponseError
  }
  deriving stock (Generic)

data RpcResponseError = RpcResponseError
  { rpcErrCode :: Int
  , rpcErrMessage :: Text
  , rpcErrData :: Maybe Value
  }
  deriving stock (Generic)

instance FromJSON RpcResponse where
  parseJSON = withObject "RpcResponse" $ \v -> do
    let result = KM.lookup (Key.fromText "result") v
    err <- case KM.lookup (Key.fromText "error") v of
      Just value -> Just <$> parseJSON value
      Nothing -> pure Nothing
    pure $ RpcResponse result err

instance FromJSON RpcResponseError where
  parseJSON = withObject "RpcResponseError" $ \v ->
    RpcResponseError
      <$> v .: "code"
      <*> v .: "message"
      <*> v .:? "data"

newClient :: Text -> IO EthClient
newClient rpcUrl =
  newClientWithOptions
    RpcClientOptions
      { rcoEndpoint = rpcUrl
      , rcoBearerToken = Nothing
      , rcoRole = "unattributed"
      }

newClientWithOptions :: RpcClientOptions -> IO EthClient
newClientWithOptions options = do
  manager <- newManager tlsManagerSettings
  reqId <- newIORef 1
  newClientWithManager manager reqId options

newClientWithManager :: Manager -> IORef Integer -> RpcClientOptions -> IO EthClient
newClientWithManager manager reqId RpcClientOptions {..} = do
  when (isLegacyAlchemyCredentialUrl rcoEndpoint) $
    logWarnEvery
      300
      "rpc_legacy_url_credential"
      "Alchemy RPC authentication is embedded in the endpoint URL; migrate to bearer authentication"
      [field "rpc_role" rcoRole]
  pure $
    EthClient
      { clientManager = manager
      , clientRpcUrl = rcoEndpoint
      , clientBearerToken = rcoBearerToken
      , clientRole = rcoRole
      , clientRequestId = reqId
      }

nextId :: EthClient -> IO Integer
nextId client = atomicModifyIORef' (clientRequestId client) $ \n -> (n + 1, n)

rpcCall :: EthClient -> Text -> Value -> IO (Either RpcError Value)
rpcCall client method params = do
  startedAt <- getMonotonicTimeNSec
  reqId <- nextId client
  let rpcReq =
        RpcRequest
          { rpcMethod = method
          , rpcParams = params
          , rpcId = reqId
          }

  eResult <- try @SomeException $ do
    req <- parseRequest $ T.unpack $ clientRpcUrl client
    let req' =
          req
            { method = "POST"
            , requestHeaders =
                ("Content-Type", "application/json")
                  : maybe
                    []
                    (\token -> [("Authorization", "Bearer " <> TE.encodeUtf8 token)])
                    (clientBearerToken client)
            , requestBody = RequestBodyLBS $ Aeson.encode rpcReq
            }
    response <- httpLbs req' (clientManager client)
    pure (statusCode $ responseStatus response, responseBody response)

  outcome <-
    case eResult of
      Left err ->
        case fromException err :: Maybe SomeAsyncException of
          Just _ -> throwIO err
          Nothing -> pure $ Left $ RpcHttpError $ rpcHttpExceptionText err
      Right (httpStatus, body)
        | httpStatus < 200 || httpStatus >= 300 ->
            pure $ Left $ RpcHttpError $ "statusCode = " <> T.pack (show httpStatus)
        | otherwise ->
            pure $ case Aeson.eitherDecode body of
              Left err -> Left $ RpcJsonError $ T.pack err
              Right RpcResponse {rpcResult = Just result, rpcError = Nothing} ->
                Right result
              Right RpcResponse {rpcError = Just RpcResponseError {..}} ->
                Left $ RpcNodeError rpcErrCode rpcErrMessage (renderErrorData <$> rpcErrData)
              Right _ ->
                Left $ RpcJsonError "No result or error in response"
  finishedAt <- getMonotonicTimeNSec
  recordRpcCall client method (finishedAt - startedAt) (either (const True) (const False) outcome)
  pure outcome

-- Keep provider diagnostics useful without ever rendering a Request, whose
-- headers may contain bearer credentials. Status codes are sufficient for
-- range-splitting and retry classification.
rpcHttpExceptionText :: SomeException -> Text
rpcHttpExceptionText exception =
  case fromException exception of
    Just (HttpExceptionRequest _ (StatusCodeException response _)) ->
      "statusCode = " <> T.pack (show $ statusCode $ responseStatus response)
    Just (InvalidUrlException _ _) -> "invalid RPC endpoint"
    Just (HttpExceptionRequest _ ResponseTimeout) -> "response timeout"
    Just (HttpExceptionRequest _ ConnectionTimeout) -> "connection timeout"
    Just (HttpExceptionRequest _ _) -> "RPC transport failure"
    Nothing -> "RPC request failed"

rpcMetricsWindowNs :: Word64
rpcMetricsWindowNs = 60 * 1_000_000_000

recordRpcCall :: EthClient -> Text -> Word64 -> Bool -> IO ()
recordRpcCall client methodName durationNs failed = do
  nowNs <- getMonotonicTimeNSec
  completedWindow <- atomicModifyIORef' rpcMetrics $ \state ->
    let metricKey = (clientRole client, methodName)
        previous = Map.findWithDefault emptyRpcMethodStats metricKey (rmsMethods state)
        updated =
          previous
            { rmsRequestCount = rmsRequestCount previous + 1
            , rmsFailureCount = rmsFailureCount previous + if failed then 1 else 0
            , rmsTotalDurationNs = rmsTotalDurationNs previous + durationNs
            , rmsMaxDurationNs = max (rmsMaxDurationNs previous) durationNs
            }
        updatedMethods = Map.insert metricKey updated (rmsMethods state)
     in if nowNs - rmsWindowStartedAtNs state >= rpcMetricsWindowNs
          then (RpcMetricsState nowNs Map.empty, Just updatedMethods)
          else (state {rmsMethods = updatedMethods}, Nothing)
  mapM_ emitRpcMethodSummary $ maybe [] Map.toList completedWindow

{-# NOINLINE rpcMetrics #-}
rpcMetrics :: IORef RpcMetricsState
rpcMetrics = unsafePerformIO $ do
  startedAt <- getMonotonicTimeNSec
  newIORef $ RpcMetricsState startedAt Map.empty

emptyRpcMethodStats :: RpcMethodStats
emptyRpcMethodStats = RpcMethodStats 0 0 0 0

emitRpcMethodSummary :: ((Text, Text), RpcMethodStats) -> IO ()
emitRpcMethodSummary ((role, methodName), RpcMethodStats {..}) =
  logInfo
    "rpc_request_summary"
    "Ethereum RPC request totals for the completed aggregation window"
    [ field "rpc_role" role
    , field "rpc_method" methodName
    , field "request_count" rmsRequestCount
    , field "failure_count" rmsFailureCount
    , field "total_duration_ms" $ nanosecondsToMilliseconds rmsTotalDurationNs
    , field "max_duration_ms" $ nanosecondsToMilliseconds rmsMaxDurationNs
    ]

nanosecondsToMilliseconds :: Word64 -> Double
nanosecondsToMilliseconds value = fromIntegral value / 1_000_000

isLegacyAlchemyCredentialUrl :: Text -> Bool
isLegacyAlchemyCredentialUrl endpoint =
  ".g.alchemy.com/v2/" `T.isInfixOf` endpoint
    && case T.splitOn "/v2/" endpoint of
      [_prefix, credential] ->
        let normalized = T.strip credential
         in not (T.null normalized)
              && normalized /= "YOUR_KEY"
              && not ("$" `T.isPrefixOf` normalized)
      _ -> False

renderErrorData :: Value -> Text
renderErrorData = \case
  String txt -> txt
  value -> TE.decodeUtf8 $ LBS.toStrict $ Aeson.encode value

data CallParams = CallParams
  { callTo :: Text
  , callData :: ByteString
  }
  deriving stock (Show)

-- | The block context in which an Ethereum RPC read should be evaluated.
-- A concrete 'BlockNumber' is encoded as a canonical JSON-RPC quantity.
data BlockTag
  = Latest
  | Earliest
  | Pending
  | Safe
  | Finalized
  | BlockNumber Integer
  deriving stock (Show, Eq)

renderBlockTag :: BlockTag -> Either RpcError Text
renderBlockTag = \case
  Latest -> Right "latest"
  Earliest -> Right "earliest"
  Pending -> Right "pending"
  Safe -> Right "safe"
  Finalized -> Right "finalized"
  BlockNumber number
    | number < 0 -> Left $ RpcJsonError "Block number cannot be negative"
    | otherwise -> Right $ "0x" <> intToHex number

ethCall :: EthClient -> CallParams -> IO (Either RpcError ByteString)
ethCall client params = ethCallAt client params Latest

-- | Evaluate an @eth_call@ using an explicit JSON-RPC block tag.
ethCallAt :: EthClient -> CallParams -> BlockTag -> IO (Either RpcError ByteString)
ethCallAt client params blockTag = ethCallAtTag client params Nothing Nothing Nothing blockTag

ethCallWithValue :: EthClient -> CallParams -> Integer -> IO (Either RpcError ByteString)
ethCallWithValue client params value
  | value < 0 = pure $ Left $ RpcJsonError "eth_call value cannot be negative"
  | otherwise = ethCallAtTag client params Nothing (Just value) Nothing Latest

-- | Evaluate an @eth_call@ with the same sender, value, and gas envelope that
-- will be used for a transaction. Some V2 contracts return a typed
-- "insufficient gas" result instead of reverting.
ethCallWithTransactionGas
  :: EthClient
  -> CallParams
  -> Text
  -> Integer
  -> Integer
  -> IO (Either RpcError ByteString)
ethCallWithTransactionGas client params fromAddr value gasLimit
  | T.null (T.strip fromAddr) = pure $ Left $ RpcJsonError "eth_call from address cannot be empty"
  | value < 0 = pure $ Left $ RpcJsonError "eth_call value cannot be negative"
  | gasLimit <= 0 = pure $ Left $ RpcJsonError "eth_call gas must be positive"
  | otherwise =
      ethCallAtTag client params (Just fromAddr) (Just value) (Just gasLimit) Latest

ethCallAtTag
  :: EthClient
  -> CallParams
  -> Maybe Text
  -> Maybe Integer
  -> Maybe Integer
  -> BlockTag
  -> IO (Either RpcError ByteString)
ethCallAtTag client CallParams {..} maybeFrom maybeValue maybeGas blockTag =
  case renderBlockTag blockTag of
    Left err -> pure $ Left err
    Right renderedBlockTag -> do
      let callObject =
            object $
              [ "to" .= callTo
              , "data" .= ("0x" <> TE.decodeUtf8 (B16.encode callData))
              ]
                <> maybe [] (\fromAddr -> ["from" .= fromAddr]) maybeFrom
                <> maybe [] (\value -> ["value" .= ("0x" <> intToHex value)]) maybeValue
                <> maybe [] (\gasLimit -> ["gas" .= ("0x" <> intToHex gasLimit)]) maybeGas
          params =
            Aeson.toJSON
              [ callObject
              , String renderedBlockTag
              ]
      result <- rpcCall client "eth_call" params
      pure $ case result of
        Left err -> Left err
        Right (String hex) -> parseRpcData "eth_call result" True hex
        Right _ -> Left $ RpcJsonError "Expected hex string from eth_call"

-- | Evaluate an @eth_call@ against the state at an exact block number.
ethCallAtBlock :: EthClient -> CallParams -> Integer -> IO (Either RpcError ByteString)
ethCallAtBlock client params = ethCallAt client params . BlockNumber

ethBlockNumber :: EthClient -> IO (Either RpcError Integer)
ethBlockNumber client = do
  result <- rpcCall client "eth_blockNumber" (Aeson.toJSON ([] :: [Value]))
  pure $ case result of
    Left err -> Left err
    Right (String hex) -> parseRpcQuantity "block number" hex
    Right _ -> Left $ RpcJsonError "Expected hex string from block number"

-- | Decode a canonical Ethereum JSON-RPC quantity. Quantities require a
-- lowercase @0x@ prefix, at least one hexadecimal digit, and no leading zero
-- unless the value itself is zero.
parseRpcQuantity :: Text -> Text -> Either RpcError Integer
parseRpcQuantity label value = do
  payload <- case T.stripPrefix "0x" value of
    Just stripped -> Right stripped
    Nothing -> Left $ RpcJsonError $ label <> " was not a 0x-prefixed hex quantity"
  if T.null payload
    then Left $ RpcJsonError $ label <> " had an empty hex quantity"
    else Right ()
  if T.all isHexDigit payload
    then Right ()
    else Left $ RpcJsonError $ label <> " contained non-hexadecimal characters"
  if T.length payload > 1 && T.head payload == '0'
    then Left $ RpcJsonError $ label <> " was not a canonical hex quantity"
    else Right $ hexToInteger payload

isHexDigit :: Char -> Bool
isHexDigit char =
  (char >= '0' && char <= '9')
    || (char >= 'a' && char <= 'f')
    || (char >= 'A' && char <= 'F')

-- | Decode 0x-prefixed JSON-RPC data without silently converting malformed
-- responses to empty bytes. Unlike quantities, byte data may be empty.
parseRpcData :: Text -> Bool -> Text -> Either RpcError ByteString
parseRpcData label allowEmpty value = do
  payload <- case T.stripPrefix "0x" value of
    Just stripped -> Right stripped
    Nothing -> Left $ RpcJsonError $ label <> " was not 0x-prefixed hex data"
  if not allowEmpty && T.null payload
    then Left $ RpcJsonError $ label <> " was empty"
    else Right ()
  case B16.decode (TE.encodeUtf8 $ T.toLower payload) of
    Right bytes -> Right bytes
    Left _ -> Left $ RpcJsonError $ label <> " contained invalid hex data"
