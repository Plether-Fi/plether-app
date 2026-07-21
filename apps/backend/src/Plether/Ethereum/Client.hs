module Plether.Ethereum.Client
  ( EthClient (..)
  , RpcError (..)
  , newClient
  , rpcCall
  , ethCall
  , ethCallAt
  , ethCallWithValue
  , ethCallAtBlock
  , ethBlockNumber
  , CallParams (..)
  , BlockTag (..)
  , renderBlockTag
  ) where

import Control.Exception (SomeException, try)
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
    Left err -> pure $ Left $ RpcHttpError $ T.pack $ show err
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

ethBlockNumber :: EthClient -> IO (Either RpcError Integer)
ethBlockNumber client = do
  result <- rpcCall client "eth_blockNumber" (Aeson.toJSON ([] :: [Value]))
  pure $ case result of
    Left err -> Left err
    Right (String hex) -> Right $ hexToInteger $ T.drop 2 hex
    Right _ -> Left $ RpcJsonError "Expected hex string result"

decodeHex :: Text -> ByteString
decodeHex txt = case B16.decode (TE.encodeUtf8 $ T.toLower txt) of
  Right bs -> bs
  Left _ -> mempty
