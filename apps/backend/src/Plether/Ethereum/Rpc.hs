module Plether.Ethereum.Rpc
  ( RpcLog (..)
  , RpcBlock (..)
  , TxReceipt (..)
  , ethGetLogs
  , ethGetBlockByNumber
  , ethLatestBlock
  , ethBlockTimestamp
  , ethLatestBlockTimestamp
  , ethGetTransactionCount
  , ethGetTransactionCountAtBlock
  , ethGetBalance
  , ethChainId
  , ethGetCode
  , ethGasPrice
  , ethMaxPriorityFeePerGas
  , ethEstimateGas
  , ethEstimateGasAtBlock
  , ethSendRawTransaction
  , ethGetTransactionReceipt
  ) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.Foldable (traverse_)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Plether.Ethereum.Client (EthClient, RpcError (..), parseRpcQuantity, rpcCall)
import Plether.Utils.Hex (intToHex)

data RpcLog = RpcLog
  { rpcLogTxHash :: Text
  , rpcLogBlockNumber :: Integer
  , rpcLogBlockHash :: Text
  , rpcLogTransactionIndex :: Integer
  , rpcLogIndex :: Integer
  , rpcLogAddress :: Text
  , rpcLogTopics :: [ByteString]
  , rpcLogData :: ByteString
  }
  deriving stock (Show, Eq)

data RpcBlock = RpcBlock
  { rpcBlockNumber :: Integer
  -- Arbitrum includes the L1 block visible to Solidity's @block.number@.
  -- Standard Ethereum RPC responses omit this optional extension.
  , rpcBlockL1Number :: Maybe Integer
  , rpcBlockHash :: Text
  , rpcBlockTimestamp :: Integer
  }
  deriving stock (Show, Eq)

data TxReceipt = TxReceipt
  { receiptTxHash :: Text
  , receiptBlockNumber :: Integer
  , receiptBlockHash :: Text
  , receiptTransactionIndex :: Integer
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
    Right (Array values) -> traverse parseLogEntry $ toList values
    Right _ -> Left $ RpcJsonError "Expected array from eth_getLogs"

ethBlockTimestamp :: EthClient -> Integer -> IO (Either RpcError Integer)
ethBlockTimestamp client blockNum =
  fmap rpcBlockTimestamp <$> ethGetBlockByNumber client blockNum

ethLatestBlockTimestamp :: EthClient -> IO (Either RpcError Integer)
ethLatestBlockTimestamp client =
  fmap rpcBlockTimestamp <$> ethLatestBlock client

ethGetBlockByNumber :: EthClient -> Integer -> IO (Either RpcError RpcBlock)
ethGetBlockByNumber client blockNum
  | blockNum < 0 = pure $ Left $ RpcJsonError "Block number cannot be negative"
  | otherwise = getBlock client ("0x" <> intToHex blockNum) (Just blockNum)

ethLatestBlock :: EthClient -> IO (Either RpcError RpcBlock)
ethLatestBlock client = getBlock client "latest" Nothing

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

ethChainId :: EthClient -> IO (Either RpcError Integer)
ethChainId client = do
  result <- rpcCall client "eth_chainId" (toJsonArray [])
  pure $ hexIntegerResult result "eth_chainId"

ethGetCode :: EthClient -> Text -> IO (Either RpcError ByteString)
ethGetCode client address = do
  result <- rpcCall client "eth_getCode" (toJsonArray [String address, String "latest"])
  pure $ case result of
    Left err -> Left err
    Right (String value) -> parseHexBytes "eth_getCode bytecode" True value
    Right _ -> Left $ RpcJsonError "Expected hex string from eth_getCode"

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
  estimateGas client fromAddr toAddr value callData Nothing

ethEstimateGasAtBlock
  :: EthClient
  -> Text
  -> Text
  -> Integer
  -> ByteString
  -> Integer
  -> IO (Either RpcError Integer)
ethEstimateGasAtBlock client fromAddr toAddr value callData blockNumber
  | blockNumber < 0 = pure $ Left $ RpcJsonError "Gas estimate block number cannot be negative"
  | otherwise =
      estimateGas
        client
        fromAddr
        toAddr
        value
        callData
        (Just $ "0x" <> intToHex blockNumber)

estimateGas
  :: EthClient
  -> Text
  -> Text
  -> Integer
  -> ByteString
  -> Maybe Text
  -> IO (Either RpcError Integer)
estimateGas client fromAddr toAddr value callData blockTag
  | value < 0 = pure $ Left $ RpcJsonError "Gas estimate value cannot be negative"
  | otherwise = do
      let txObject = estimateGasTransaction fromAddr toAddr value callData
          params = txObject : maybe [] (pure . String) blockTag
      result <- rpcCall client "eth_estimateGas" (toJsonArray params)
      pure $ hexIntegerResult result "eth_estimateGas"

estimateGasTransaction :: Text -> Text -> Integer -> ByteString -> Value
estimateGasTransaction fromAddr toAddr value callData =
  object
    [ "from" .= fromAddr
    , "to" .= toAddr
    , "value" .= ("0x" <> intToHex value)
    , "data" .= ("0x" <> TE.decodeUtf8 (B16.encode callData))
    ]

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
    Right (Object obj) -> Just <$> parseReceipt txHash obj
    Right _ -> Left $ RpcJsonError "Expected receipt object or null from eth_getTransactionReceipt"

getBlock :: EthClient -> Text -> Maybe Integer -> IO (Either RpcError RpcBlock)
getBlock client blockTag expectedNumber = do
  result <- rpcCall client "eth_getBlockByNumber" (toJsonArray [String blockTag, Bool False])
  pure $ case result of
    Left err -> Left err
    Right (Object obj) -> do
      numberHex <- requiredString "block number" "number" obj
      number <- parseRpcQuantity "block number" numberHex
      blockHashText <- requiredString "block hash" "hash" obj
      blockHash <- parseHash "block hash" blockHashText
      timestampHex <- requiredString "block timestamp" "timestamp" obj
      timestamp <- parseRpcQuantity "block timestamp" timestampHex
      l1BlockNumber <- optionalQuantity "block L1 number" "l1BlockNumber" obj
      case expectedNumber of
        Just requested
          | number /= requested ->
              Left $
                RpcJsonError $
                  "eth_getBlockByNumber returned block "
                    <> T.pack (show number)
                    <> " for requested block "
                    <> T.pack (show requested)
        _ -> Right ()
      pure $
        RpcBlock
          { rpcBlockNumber = number
          , rpcBlockL1Number = l1BlockNumber
          , rpcBlockHash = blockHash
          , rpcBlockTimestamp = timestamp
          }
    Right Null -> Left $ RpcJsonError "Block was not found"
    Right _ -> Left $ RpcJsonError "Expected block object"

parseReceipt :: Text -> KM.KeyMap Value -> Either RpcError TxReceipt
parseReceipt expectedTxHash obj = do
  txHashText <- requiredString "receipt transaction hash" "transactionHash" obj
  txHash <- parseHash "receipt transaction hash" txHashText
  if T.toLower txHash == T.toLower expectedTxHash
    then Right ()
    else Left $ RpcJsonError "Receipt transaction hash did not match the requested transaction"
  blockNumberHex <- requiredString "receipt block number" "blockNumber" obj
  blockNumber <- parseRpcQuantity "receipt block number" blockNumberHex
  blockHashText <- requiredString "receipt block hash" "blockHash" obj
  blockHash <- parseHash "receipt block hash" blockHashText
  transactionIndexHex <- requiredString "receipt transaction index" "transactionIndex" obj
  transactionIndex <- parseRpcQuantity "receipt transaction index" transactionIndexHex
  statusHex <- requiredString "receipt status" "status" obj
  status <- parseRpcQuantity "receipt status" statusHex
  succeeded <- case status of
    0 -> Right False
    1 -> Right True
    _ -> Left $ RpcJsonError "Receipt status must be the canonical quantity 0x0 or 0x1"
  logsValue <- requiredArray "receipt logs" "logs" obj
  logs <- traverse parseLogEntry $ toList logsValue
  traverse_ (validateReceiptLogIdentity txHash blockNumber blockHash transactionIndex) logs
  pure
    TxReceipt
      { receiptTxHash = txHash
      , receiptBlockNumber = blockNumber
      , receiptBlockHash = blockHash
      , receiptTransactionIndex = transactionIndex
      , receiptSucceeded = succeeded
      , receiptLogs = logs
      }

parseLogEntry :: Value -> Either RpcError RpcLog
parseLogEntry = \case
  Object obj -> do
    txHashText <- requiredString "log transaction hash" "transactionHash" obj
    txHash <- parseHash "log transaction hash" txHashText
    blockNumberHex <- requiredString "log block number" "blockNumber" obj
    blockNumber <- parseRpcQuantity "log block number" blockNumberHex
    blockHashText <- requiredString "log block hash" "blockHash" obj
    blockHash <- parseHash "log block hash" blockHashText
    transactionIndexHex <- requiredString "log transaction index" "transactionIndex" obj
    transactionIndex <- parseRpcQuantity "log transaction index" transactionIndexHex
    logIndexHex <- requiredString "log index" "logIndex" obj
    logIndex <- parseRpcQuantity "log index" logIndexHex
    addressText <- requiredString "log address" "address" obj
    address <- parseAddress "log address" addressText
    topicsValue <- requiredArray "log topics" "topics" obj
    topics <- traverse parseTopic $ toList topicsValue
    dataText <- requiredString "log data" "data" obj
    eventData <- parseHexBytes "log data" True dataText
    pure
      RpcLog
        { rpcLogTxHash = txHash
        , rpcLogBlockNumber = blockNumber
        , rpcLogBlockHash = blockHash
        , rpcLogTransactionIndex = transactionIndex
        , rpcLogIndex = logIndex
        , rpcLogAddress = address
        , rpcLogTopics = topics
        , rpcLogData = eventData
        }
  _ -> Left $ RpcJsonError "Expected each JSON-RPC log entry to be an object"

validateReceiptLogIdentity :: Text -> Integer -> Text -> Integer -> RpcLog -> Either RpcError ()
validateReceiptLogIdentity txHash blockNumber blockHash transactionIndex entry
  | T.toLower (rpcLogTxHash entry) /= T.toLower txHash =
      Left $ RpcJsonError "Receipt log transaction hash did not match its receipt"
  | rpcLogBlockNumber entry /= blockNumber =
      Left $ RpcJsonError "Receipt log block number did not match its receipt"
  | T.toLower (rpcLogBlockHash entry) /= T.toLower blockHash =
      Left $ RpcJsonError "Receipt log block hash did not match its receipt"
  | rpcLogTransactionIndex entry /= transactionIndex =
      Left $ RpcJsonError "Receipt log transaction index did not match its receipt"
  | otherwise = Right ()

hexIntegerResult :: Either RpcError Value -> Text -> Either RpcError Integer
hexIntegerResult result label =
  case result of
    Left err -> Left err
    Right (String hex) -> parseRpcQuantity label hex
    Right _ -> Left $ RpcJsonError $ "Expected hex string from " <> label

requiredString :: Text -> Text -> KM.KeyMap Value -> Either RpcError Text
requiredString label key obj =
  case KM.lookup (Key.fromText key) obj of
    Just (String value) -> Right value
    Just _ -> Left $ RpcJsonError $ "Expected " <> label <> " to be a string"
    Nothing -> Left $ RpcJsonError $ "Missing " <> label

optionalQuantity :: Text -> Text -> KM.KeyMap Value -> Either RpcError (Maybe Integer)
optionalQuantity label key obj =
  case KM.lookup (Key.fromText key) obj of
    Nothing -> Right Nothing
    Just Null -> Right Nothing
    Just (String value) -> Just <$> parseRpcQuantity label value
    Just _ -> Left $ RpcJsonError $ "Expected " <> label <> " to be a string or null"

requiredArray :: Text -> Text -> KM.KeyMap Value -> Either RpcError (V.Vector Value)
requiredArray label key obj =
  case KM.lookup (Key.fromText key) obj of
    Just (Array values) -> Right values
    Just _ -> Left $ RpcJsonError $ "Expected " <> label <> " to be an array"
    Nothing -> Left $ RpcJsonError $ "Missing " <> label

parseHash :: Text -> Text -> Either RpcError Text
parseHash label value = parseFixedHexText label 32 value

parseAddress :: Text -> Text -> Either RpcError Text
parseAddress label value = parseFixedHexText label 20 value

parseFixedHexText :: Text -> Int -> Text -> Either RpcError Text
parseFixedHexText label expectedBytes value = do
  bytes <- parseHexBytes label False value
  if BS.length bytes == expectedBytes
    then Right value
    else
      Left $
        RpcJsonError $
          label
            <> " must contain exactly "
            <> T.pack (show expectedBytes)
            <> " bytes"

parseTopic :: Value -> Either RpcError ByteString
parseTopic = \case
  String value -> do
    bytes <- parseHexBytes "log topic" False value
    if BS.length bytes == 32
      then Right bytes
      else Left $ RpcJsonError "Each log topic must contain exactly 32 bytes"
  _ -> Left $ RpcJsonError "Expected each log topic to be a string"

parseHexBytes :: Text -> Bool -> Text -> Either RpcError ByteString
parseHexBytes label allowEmpty value = do
  payload <- case T.stripPrefix "0x" value of
    Just stripped -> Right stripped
    Nothing -> Left $ RpcJsonError $ label <> " was not 0x-prefixed hex data"
  if not allowEmpty && T.null payload
    then Left $ RpcJsonError $ label <> " was empty"
    else Right ()
  case B16.decode (TE.encodeUtf8 $ T.toLower payload) of
    Right bytes -> Right bytes
    Left _ -> Left $ RpcJsonError $ label <> " contained invalid hex data"

toJsonArray :: [Value] -> Value
toJsonArray = Array . fromList

fromList :: [Value] -> V.Vector Value
fromList = V.fromList

toList :: V.Vector Value -> [Value]
toList = V.toList
