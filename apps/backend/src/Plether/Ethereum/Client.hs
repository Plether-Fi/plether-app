module Plether.Ethereum.Client
  ( EthClient (..)
  , RpcError (..)
  , newClient
  , ethCall
  , ethBlockNumber
  , CallParams (..)
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object, withObject, (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import Plether.Utils.Hex (hexToInteger)
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
  | RpcNodeError Int Text
  deriving stock (Show)

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
  }
  deriving stock (Generic)

instance FromJSON RpcResponse where
  parseJSON = withObject "RpcResponse" $ \v ->
    RpcResponse
      <$> v .:? "result"
      <*> v .:? "error"

instance FromJSON RpcResponseError where
  parseJSON = withObject "RpcResponseError" $ \v ->
    RpcResponseError
      <$> v .: "code"
      <*> v .: "message"

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
          pure $ Left $ RpcNodeError rpcErrCode rpcErrMessage
        Right _ ->
          pure $ Left $ RpcJsonError "No result or error in response"

data CallParams = CallParams
  { callTo :: Text
  , callData :: ByteString
  }
  deriving stock (Show)

ethCall :: EthClient -> CallParams -> IO (Either RpcError ByteString)
ethCall client CallParams {..} = do
  let params =
        Aeson.toJSON
          [ object
              [ "to" .= callTo
              , "data" .= ("0x" <> TE.decodeUtf8 (B16.encode callData))
              ]
          , String "latest"
          ]
  result <- rpcCall client "eth_call" params
  pure $ case result of
    Left err -> Left err
    Right (String hex) -> Right $ decodeHex $ T.drop 2 hex
    Right _ -> Left $ RpcJsonError "Expected hex string result"

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

