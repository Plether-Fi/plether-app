module Plether.Ethereum.Rpc
  ( RpcLog (..)
  , TxReceipt (..)
  , ethGetLogs
  , ethBlockTimestamp
  , ethLatestBlockTimestamp
  , ethGetTransactionCount
  , ethGetTransactionCountAtBlock
  , ethGetBalance
  , ethGasPrice
  , ethMaxPriorityFeePerGas
  , ethEstimateGas
  , ethSendRawTransaction
  , ethGetTransactionReceipt
  ) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as B16
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Plether.Ethereum.Client (EthClient, RpcError (..), rpcCall)
import Plether.Utils.Hex (hexToInteger, intToHex)

data RpcLog = RpcLog
  { rpcLogTxHash :: Text
  , rpcLogBlockNumber :: Integer
  , rpcLogAddress :: Text
  , rpcLogTopics :: [ByteString]
  , rpcLogData :: ByteString
  }
  deriving stock (Show, Eq)

data TxReceipt = TxReceipt
  { receiptTxHash :: Text
  , receiptBlockNumber :: Integer
  , receiptSucceeded :: Bool
  , receiptLogs :: [RpcLog]
  }
  deriving stock (Show, Eq)

ethGetLogs
  :: EthClient
  -> Text
  -> [ByteString]
  -> Integer
  -> Integer
  -> IO (Either RpcError [RpcLog])
ethGetLogs client address topics fromBlock toBlock = do
  let topicValues = map (String . ("0x" <>) . TE.decodeUtf8 . B16.encode) topics
      params =
        [ object
            [ "address" .= address
            , "topics" .= [topicValues]
            , "fromBlock" .= ("0x" <> intToHex fromBlock)
            , "toBlock" .= ("0x" <> intToHex toBlock)
            ]
        ]
  result <- rpcCall client "eth_getLogs" (Array $ fromList params)
  pure $ case result of
    Left err -> Left err
    Right (Array values) -> Right $ map parseLogEntry $ toList values
    Right _ -> Left $ RpcJsonError "Expected array from eth_getLogs"

ethBlockTimestamp :: EthClient -> Integer -> IO (Either RpcError Integer)
ethBlockTimestamp client blockNum =
  getBlockTimestamp client ("0x" <> intToHex blockNum)

ethLatestBlockTimestamp :: EthClient -> IO (Either RpcError Integer)
ethLatestBlockTimestamp client =
  getBlockTimestamp client "latest"

ethGetTransactionCount :: EthClient -> Text -> IO (Either RpcError Integer)
ethGetTransactionCount client address = do
  result <- rpcCall client "eth_getTransactionCount" (toJsonArray [String address, String "pending"])
  pure $ hexIntegerResult result "eth_getTransactionCount"

ethGetTransactionCountAtBlock :: EthClient -> Text -> Integer -> IO (Either RpcError Integer)
ethGetTransactionCountAtBlock client address blockNumber = do
  result <-
    rpcCall
      client
      "eth_getTransactionCount"
      (toJsonArray [String address, String $ "0x" <> intToHex (max 0 blockNumber)])
  pure $ hexIntegerResult result "eth_getTransactionCount"

ethGetBalance :: EthClient -> Text -> IO (Either RpcError Integer)
ethGetBalance client address = do
  result <- rpcCall client "eth_getBalance" (toJsonArray [String address, String "latest"])
  pure $ hexIntegerResult result "eth_getBalance"

ethGasPrice :: EthClient -> IO (Either RpcError Integer)
ethGasPrice client = do
  result <- rpcCall client "eth_gasPrice" (toJsonArray [])
  pure $ hexIntegerResult result "eth_gasPrice"

ethMaxPriorityFeePerGas :: EthClient -> IO (Either RpcError Integer)
ethMaxPriorityFeePerGas client = do
  result <- rpcCall client "eth_maxPriorityFeePerGas" (toJsonArray [])
  pure $ hexIntegerResult result "eth_maxPriorityFeePerGas"

ethEstimateGas
  :: EthClient
  -> Text
  -> Text
  -> Integer
  -> ByteString
  -> IO (Either RpcError Integer)
ethEstimateGas client fromAddr toAddr value callData = do
  let txObject =
        object
          [ "from" .= fromAddr
          , "to" .= toAddr
          , "value" .= ("0x" <> intToHex value)
          , "data" .= ("0x" <> TE.decodeUtf8 (B16.encode callData))
          ]
  result <- rpcCall client "eth_estimateGas" (toJsonArray [txObject])
  pure $ hexIntegerResult result "eth_estimateGas"

ethSendRawTransaction :: EthClient -> ByteString -> IO (Either RpcError Text)
ethSendRawTransaction client rawTx = do
  result <- rpcCall client "eth_sendRawTransaction" $
    toJsonArray [String $ "0x" <> TE.decodeUtf8 (B16.encode rawTx)]
  pure $ case result of
    Left err -> Left err
    Right (String txHash) -> Right txHash
    Right _ -> Left $ RpcJsonError "Expected transaction hash from eth_sendRawTransaction"

ethGetTransactionReceipt :: EthClient -> Text -> IO (Either RpcError (Maybe TxReceipt))
ethGetTransactionReceipt client txHash = do
  result <- rpcCall client "eth_getTransactionReceipt" (toJsonArray [String txHash])
  pure $ case result of
    Left err -> Left err
    Right Null -> Right Nothing
    Right (Object obj) -> Right $ Just $ parseReceipt obj
    Right _ -> Left $ RpcJsonError "Expected receipt object or null from eth_getTransactionReceipt"

getBlockTimestamp :: EthClient -> Text -> IO (Either RpcError Integer)
getBlockTimestamp client blockTag = do
  result <- rpcCall client "eth_getBlockByNumber" (toJsonArray [String blockTag, Bool False])
  pure $ case result of
    Left err -> Left err
    Right (Object obj) ->
      case lookupString "timestamp" obj of
        Just hex -> Right $ hexToInteger $ strip0x hex
        Nothing -> Left $ RpcJsonError "Block did not include timestamp"
    Right _ -> Left $ RpcJsonError "Expected block object"

parseReceipt :: KM.KeyMap Value -> TxReceipt
parseReceipt obj =
  TxReceipt
    { receiptTxHash = maybe "" id $ lookupString "transactionHash" obj
    , receiptBlockNumber = maybe 0 (hexToInteger . strip0x) $ lookupString "blockNumber" obj
    , receiptSucceeded = maybe False ((== 1) . hexToInteger . strip0x) $ lookupString "status" obj
    , receiptLogs =
        case KM.lookup (Key.fromText "logs") obj of
          Just (Array values) -> map parseLogEntry $ toList values
          _ -> []
    }

parseLogEntry :: Value -> RpcLog
parseLogEntry = \case
  Object obj ->
    RpcLog
      { rpcLogTxHash = maybe "" id $ lookupString "transactionHash" obj
      , rpcLogBlockNumber = maybe 0 (hexToInteger . strip0x) $ lookupString "blockNumber" obj
      , rpcLogAddress = maybe "" id $ lookupString "address" obj
      , rpcLogTopics =
          case KM.lookup (Key.fromText "topics") obj of
            Just (Array values) -> [decodeHex topic | String topic <- toList values]
            _ -> []
      , rpcLogData = maybe "" decodeHex $ lookupString "data" obj
      }
  _ ->
    RpcLog
      { rpcLogTxHash = ""
      , rpcLogBlockNumber = 0
      , rpcLogAddress = ""
      , rpcLogTopics = []
      , rpcLogData = ""
      }

hexIntegerResult :: Either RpcError Value -> Text -> Either RpcError Integer
hexIntegerResult = \case
  Left err -> const $ Left err
  Right (String hex) -> const $ Right $ hexToInteger $ strip0x hex
  Right _ -> \label -> Left $ RpcJsonError $ "Expected hex string from " <> label

lookupString :: Text -> KM.KeyMap Value -> Maybe Text
lookupString key obj =
  case KM.lookup (Key.fromText key) obj of
    Just (String value) -> Just value
    _ -> Nothing

decodeHex :: Text -> ByteString
decodeHex value =
  let stripped = strip0x value
   in case B16.decode (TE.encodeUtf8 $ T.toLower stripped) of
        Right bytes -> bytes
        Left _ -> ""

strip0x :: Text -> Text
strip0x value =
  case T.stripPrefix "0x" value of
    Just stripped -> stripped
    Nothing -> value

toJsonArray :: [Value] -> Value
toJsonArray = Array . fromList

fromList :: [Value] -> V.Vector Value
fromList = V.fromList

toList :: V.Vector Value -> [Value]
toList = V.toList
