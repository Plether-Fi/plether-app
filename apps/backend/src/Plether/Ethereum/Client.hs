module Plether.Ethereum.Client
  ( EthClient (..)
  , RpcError (..)
  , RpcChainBindingError (..)
  , CanonicalBlockRef
  , canonicalBlockNumber
  , canonicalBlockHash
  , mkCanonicalBlockRef
  , decodeCanonicalBlockRef
  , renderCanonicalBlockIdentifier
  , canonicalEthCallParams
  , newClient
  , newClientWithManager
  , rpcCall
  , ethCall
  , ethCallAt
  , ethCallWithValue
  , ethCallAtBlock
  , ethCallAtCanonicalBlock
  , ethGetCanonicalBlockRef
  , ethBlockNumber
  , ethChainId
  , decodeChainIdResult
  , validateRpcChainId
  , selectRpcUrlsForChain
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
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object, withObject, (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import Plether.Utils.Hex (hexToInteger, intToHex)
import Network.HTTP.Client
  ( Manager
  , Request (..)
  , RequestBody (..)
  , httpLbs
  , newManager
  , parseRequest
  , responseBody
  )
import Network.HTTP.Client.TLS (tlsManagerSettings)

data EthClient = EthClient
  { clientManager :: Manager
  , clientRpcUrl :: Text
  , clientRequestId :: IORef Integer
  }

data RpcError
  = RpcHttpError Text
  | RpcJsonError Text
  | RpcNodeError Int Text (Maybe Text)
  deriving stock (Show, Eq)

-- | A deliberately redacted release-to-provider binding failure. Consumers
-- may expose this classification publicly without leaking provider URLs or
-- node error details.
data RpcChainBindingError
  = RpcChainIdUnavailable
  | RpcChainIdMismatch
  deriving stock (Show, Eq)

-- | A block number/hash pair that has passed strict local validation. EIP-1898
-- sends only the hash to @eth_call@; retaining the number in this opaque value
-- binds returned evidence and API envelopes to the header from which the hash
-- was resolved.
data CanonicalBlockRef = CanonicalBlockRef
  { canonicalBlockNumber :: Integer
  , canonicalBlockHash :: Text
  }
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
newClient rpcUrl = do
  manager <- newManager tlsManagerSettings
  newClientWithManager manager rpcUrl

newClientWithManager :: Manager -> Text -> IO EthClient
newClientWithManager manager rpcUrl = do
  reqId <- newIORef 1
  pure $
    EthClient
      { clientManager = manager
      , clientRpcUrl = rpcUrl
      , clientRequestId = reqId
      }

nextId :: EthClient -> IO Integer
nextId client = atomicModifyIORef' (clientRequestId client) $ \n -> (n + 1, n)

rpcCall :: EthClient -> Text -> Value -> IO (Either RpcError Value)
rpcCall client method params = do
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
                [ ("Content-Type", "application/json")
                ]
            , requestBody = RequestBodyLBS $ Aeson.encode rpcReq
            }
    response <- httpLbs req' (clientManager client)
    pure $ responseBody response

  case eResult of
    Left err ->
      case fromException err :: Maybe SomeAsyncException of
        Just _ -> throwIO err
        Nothing -> pure $ Left $ RpcHttpError $ T.pack $ show err
    Right body ->
      case Aeson.eitherDecode body of
        Left err -> pure $ Left $ RpcJsonError $ T.pack err
        Right RpcResponse {rpcResult = Just result, rpcError = Nothing} ->
          pure $ Right result
        Right RpcResponse {rpcError = Just RpcResponseError {..}} ->
          pure $ Left $ RpcNodeError rpcErrCode rpcErrMessage (renderErrorData <$> rpcErrData)
        Right _ ->
          pure $ Left $ RpcJsonError "No result or error in response"

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
ethCallAt client params blockTag = ethCallAtTag client params Nothing blockTag

ethCallWithValue :: EthClient -> CallParams -> Integer -> IO (Either RpcError ByteString)
ethCallWithValue client params value
  | value < 0 = pure $ Left $ RpcJsonError "eth_call value cannot be negative"
  | otherwise = ethCallAtTag client params (Just value) Latest

ethCallAtTag :: EthClient -> CallParams -> Maybe Integer -> BlockTag -> IO (Either RpcError ByteString)
ethCallAtTag client CallParams {..} maybeValue blockTag =
  case renderBlockTag blockTag of
    Left err -> pure $ Left err
    Right renderedBlockTag -> do
      let callObject =
            object $
              [ "to" .= callTo
              , "data" .= ("0x" <> TE.decodeUtf8 (B16.encode callData))
              ]
                <> maybe [] (\value -> ["value" .= ("0x" <> intToHex value)]) maybeValue
          params =
            Aeson.toJSON
              [ callObject
              , String renderedBlockTag
              ]
      result <- rpcCall client "eth_call" params
      pure $ case result of
        Left err -> Left err
        Right (String hex) -> Right $ decodeHex $ T.drop 2 hex
        Right _ -> Left $ RpcJsonError "Expected hex string result"

-- | Evaluate an @eth_call@ against the state at an exact block number.
ethCallAtBlock :: EthClient -> CallParams -> Integer -> IO (Either RpcError ByteString)
ethCallAtBlock client params = ethCallAt client params . BlockNumber

-- | Evaluate an @eth_call@ against one canonical block hash. Providers that
-- do not support EIP-1898, no longer consider the hash canonical, or cannot
-- serve its state return an RPC error; callers must not fall back to a numeric
-- block tag for evidence-bearing reads.
ethCallAtCanonicalBlock
  :: EthClient
  -> CallParams
  -> CanonicalBlockRef
  -> IO (Either RpcError ByteString)
ethCallAtCanonicalBlock client callParams blockRef = do
  result <-
    rpcCall
      client
      "eth_call"
      (canonicalEthCallParams callParams blockRef)
  pure $ result >>= decodeEthCallResult

-- | Resolve and strictly validate the exact header pair for a numeric block.
-- This is used before historical reads whose source data contains a number but
-- not a trusted hash.
ethGetCanonicalBlockRef
  :: EthClient
  -> Integer
  -> IO (Either RpcError CanonicalBlockRef)
ethGetCanonicalBlockRef client expectedBlockNumber
  | expectedBlockNumber < 0 =
      pure $ Left $ RpcJsonError "Block number cannot be negative"
  | otherwise = do
      result <-
        rpcCall
          client
          "eth_getBlockByNumber"
          ( Aeson.toJSON
              [ String $ "0x" <> intToHex expectedBlockNumber
              , Bool False
              ]
          )
      pure $ result >>= decodeCanonicalBlockRef expectedBlockNumber

decodeCanonicalBlockRef
  :: Integer
  -> Value
  -> Either RpcError CanonicalBlockRef
decodeCanonicalBlockRef expectedBlockNumber = \case
  Object fields -> do
    numberValue <-
      maybe
        (Left $ RpcJsonError "Block did not include number")
        Right
        (KM.lookup (Key.fromText "number") fields)
    returnedBlockNumber <- case numberValue of
      String quantity -> decodeRpcQuantity "block number" quantity
      _ -> Left $ RpcJsonError "Expected block number as a hex quantity string"
    if returnedBlockNumber /= expectedBlockNumber
      then Left $ RpcJsonError "Returned block number did not match the requested block"
      else do
        hashValue <-
          maybe
            (Left $ RpcJsonError "Block did not include hash")
            Right
            (KM.lookup (Key.fromText "hash") fields)
        case hashValue of
          String blockHash -> mkCanonicalBlockRef returnedBlockNumber blockHash
          _ -> Left $ RpcJsonError "Expected block hash as a hex string"
  Null -> Left $ RpcJsonError "Block was not found"
  _ -> Left $ RpcJsonError "Expected block object"

mkCanonicalBlockRef
  :: Integer
  -> Text
  -> Either RpcError CanonicalBlockRef
mkCanonicalBlockRef blockNumber blockHash
  | blockNumber < 0 =
      Left $ RpcJsonError "Block number cannot be negative"
  | T.length blockHash /= 66
      || not ("0x" `T.isPrefixOf` blockHash)
      || not (T.all isHexDigit $ T.drop 2 blockHash) =
      Left $ RpcJsonError "Expected a canonical 32-byte block hash"
  | otherwise =
      Right $
        CanonicalBlockRef
          { canonicalBlockNumber = blockNumber
          , canonicalBlockHash = T.toLower blockHash
          }
  where
    isHexDigit char =
      (char >= '0' && char <= '9')
        || (char >= 'a' && char <= 'f')
        || (char >= 'A' && char <= 'F')

renderCanonicalBlockIdentifier :: CanonicalBlockRef -> Value
renderCanonicalBlockIdentifier blockRef =
  object
    [ "blockHash" .= canonicalBlockHash blockRef
    , "requireCanonical" .= True
    ]

canonicalEthCallParams :: CallParams -> CanonicalBlockRef -> Value
canonicalEthCallParams CallParams {..} blockRef =
  Aeson.toJSON
    [ object
        [ "to" .= callTo
        , "data" .= ("0x" <> TE.decodeUtf8 (B16.encode callData))
        ]
    , renderCanonicalBlockIdentifier blockRef
    ]

ethBlockNumber :: EthClient -> IO (Either RpcError Integer)
ethBlockNumber client = do
  result <- rpcCall client "eth_blockNumber" (Aeson.toJSON ([] :: [Value]))
  pure $ case result of
    Left err -> Left err
    Right (String quantity) -> decodeRpcQuantity "block number" quantity
    Right _ -> Left $ RpcJsonError "Expected block number as a hex quantity string"

-- | Return the EIP-155 chain identifier reported by the configured provider.
-- JSON-RPC quantities are parsed strictly so malformed responses cannot be
-- mistaken for chain zero (the historical 'hexToInteger' helper is
-- intentionally permissive and is therefore unsuitable for this boundary).
ethChainId :: EthClient -> IO (Either RpcError Integer)
ethChainId client = do
  result <- rpcCall client "eth_chainId" (Aeson.toJSON ([] :: [Value]))
  pure $ result >>= decodeChainIdResult

decodeChainIdResult :: Value -> Either RpcError Integer
decodeChainIdResult = \case
  String quantity -> decodeRpcQuantity "chain ID" quantity
  _ -> Left $ RpcJsonError "Expected chain ID as a hex quantity string"

validateRpcChainId
  :: Integer
  -> Either RpcError Integer
  -> Either RpcChainBindingError ()
validateRpcChainId expectedChainId = \case
  Left _ -> Left RpcChainIdUnavailable
  Right observedChainId
    | observedChainId == expectedChainId -> Right ()
    | otherwise -> Left RpcChainIdMismatch

-- | Keep only providers that were positively bound to the expected chain.
-- Failed and mismatching probes are excluded from later fallback rotation.
selectRpcUrlsForChain
  :: Integer
  -> [(Text, Either RpcError Integer)]
  -> [Text]
selectRpcUrlsForChain expectedChainId =
  foldr
    ( \(rpcUrl, observedChainId) matching ->
        case validateRpcChainId expectedChainId observedChainId of
          Right () -> rpcUrl : matching
          Left _ -> matching
    )
    []

decodeRpcQuantity :: Text -> Text -> Either RpcError Integer
decodeRpcQuantity label quantity
  | not ("0x" `T.isPrefixOf` quantity) =
      malformed $ "Expected " <> label <> " with a 0x prefix"
  | T.null digits =
      malformed $ "Expected " <> label <> " with at least one hex digit"
  | T.length digits > 1 && T.head digits == '0' =
      malformed $ "Expected " <> label <> " as a canonical hex quantity"
  | not (T.all isHexDigit digits) =
      malformed $ "Expected " <> label <> " with only hex digits"
  | otherwise = Right $ hexToInteger digits
  where
    digits = T.drop 2 quantity
    malformed = Left . RpcJsonError
    isHexDigit char =
      (char >= '0' && char <= '9')
        || (char >= 'a' && char <= 'f')
        || (char >= 'A' && char <= 'F')

decodeEthCallResult :: Value -> Either RpcError ByteString
decodeEthCallResult = \case
  String value
    | not ("0x" `T.isPrefixOf` value) ->
        Left $ RpcJsonError "Expected eth_call result with a 0x prefix"
    | odd $ T.length digits ->
        Left $ RpcJsonError "Expected eth_call result with complete bytes"
    | not (T.all isHexDigit digits) ->
        Left $ RpcJsonError "Expected eth_call result with only hex digits"
    | otherwise ->
        case B16.decode $ TE.encodeUtf8 $ T.toLower digits of
          Right bytes -> Right bytes
          Left _ -> Left $ RpcJsonError "Could not decode eth_call result"
    where
      digits = T.drop 2 value
      isHexDigit char =
        (char >= '0' && char <= '9')
          || (char >= 'a' && char <= 'f')
          || (char >= 'A' && char <= 'F')
  _ -> Left $ RpcJsonError "Expected hex string result"

decodeHex :: Text -> ByteString
decodeHex txt = case B16.decode (TE.encodeUtf8 $ T.toLower txt) of
  Right bs -> bs
  Left _ -> mempty
