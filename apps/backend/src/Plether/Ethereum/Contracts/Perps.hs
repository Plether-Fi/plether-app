module Plether.Ethereum.Contracts.Perps
  ( PerpsOrderEvent (..)
  , PendingOrderView (..)
  , OrderExecutionPolicy (..)
  , orderCommittedTopic
  , orderExecutedTopic
  , orderFailedTopic
  , perpsOrderTopics
  , decodePerpsOrderEvent
  , getPendingOrderView
  , maxOrderAge
  , orderSettlementWindow
  , orderExecutionStalenessLimit
  , isOracleFrozen
  , getOrderExecutionPolicy
  , getUpdateFee
  , executeOrderCall
  , executeOrderBatchCall
  , getUpdateFeeCall
  , orderFailureReasonText
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Plether.Ethereum.Abi
  ( decodeAddress
  , decodeBool
  , decodeUint256
  , encodeBool
  , encodeCall
  , encodeUint256
  , keccak256
  )
import Plether.Ethereum.Client (CallParams (..), EthClient, RpcError, ethCall)
import Plether.Ethereum.Rpc (RpcLog (..))

data PerpsOrderEvent
  = OrderCommitted
      { poeOrderId :: Integer
      , poeAccount :: Text
      , poeSide :: Integer
      , poeTxHash :: Text
      , poeBlockNumber :: Integer
      }
  | OrderExecuted
      { poeOrderId :: Integer
      , poeExecutionPrice :: Integer
      , poeTxHash :: Text
      , poeBlockNumber :: Integer
      }
  | OrderFailed
      { poeOrderId :: Integer
      , poeFailureReason :: Integer
      , poeTxHash :: Text
      , poeBlockNumber :: Integer
      }
  deriving stock (Show, Eq)

data PendingOrderView = PendingOrderView
  { povOrderId :: Integer
  , povIsClose :: Bool
  , povSide :: Integer
  , povSizeDelta :: Integer
  , povMarginDelta :: Integer
  , povTargetPrice :: Integer
  , povCommitTime :: Integer
  , povCommitBlock :: Integer
  , povCommittedMarginUsdc :: Integer
  , povExecutionBountyUsdc :: Integer
  , povNextAccountOrderId :: Integer
  }
  deriving stock (Show, Eq)

data OrderExecutionPolicy = OrderExecutionPolicy
  { oepCloseOnly :: Bool
  , oepRequireStoredMark :: Bool
  , oepAllowAnyStoredMark :: Bool
  , oepMaxStaleness :: Integer
  , oepOracleFrozen :: Bool
  , oepIsFadWindow :: Bool
  }
  deriving stock (Show, Eq)

orderCommittedTopic :: ByteString
orderCommittedTopic = keccak256 $ TE.encodeUtf8 "OrderCommitted(uint64,address,uint8)"

orderExecutedTopic :: ByteString
orderExecutedTopic = keccak256 $ TE.encodeUtf8 "OrderExecuted(uint64,uint256)"

orderFailedTopic :: ByteString
orderFailedTopic = keccak256 $ TE.encodeUtf8 "OrderFailed(uint64,uint8)"

perpsOrderTopics :: [ByteString]
perpsOrderTopics =
  [ orderCommittedTopic
  , orderExecutedTopic
  , orderFailedTopic
  ]

decodePerpsOrderEvent :: RpcLog -> Maybe PerpsOrderEvent
decodePerpsOrderEvent RpcLog {..} =
  case rpcLogTopics of
    topic : orderTopic : accountTopic : _
      | topic == orderCommittedTopic ->
          Just $
            OrderCommitted
              { poeOrderId = decodeUint256 orderTopic
              , poeAccount = decodeAddress accountTopic
              , poeSide = wordAt 0 rpcLogData
              , poeTxHash = rpcLogTxHash
              , poeBlockNumber = rpcLogBlockNumber
              }
    topic : orderTopic : _
      | topic == orderExecutedTopic ->
          Just $
            OrderExecuted
              { poeOrderId = decodeUint256 orderTopic
              , poeExecutionPrice = wordAt 0 rpcLogData
              , poeTxHash = rpcLogTxHash
              , poeBlockNumber = rpcLogBlockNumber
              }
      | topic == orderFailedTopic ->
          Just $
            OrderFailed
              { poeOrderId = decodeUint256 orderTopic
              , poeFailureReason = wordAt 0 rpcLogData
              , poeTxHash = rpcLogTxHash
              , poeBlockNumber = rpcLogBlockNumber
              }
    _ -> Nothing

getPendingOrderView :: EthClient -> Text -> Integer -> IO (Either RpcError PendingOrderView)
getPendingOrderView client orderRouter orderId = do
  result <- ethCall client (CallParams orderRouter (getPendingOrderViewCall orderId))
  pure $ fmap decodePendingOrderView result

maxOrderAge :: EthClient -> Text -> IO (Either RpcError Integer)
maxOrderAge client orderRouter = do
  result <- ethCall client (CallParams orderRouter (encodeCall "maxOrderAge()" []))
  pure $ fmap decodeUint256 result

orderSettlementWindow :: EthClient -> Text -> IO (Either RpcError Integer)
orderSettlementWindow client oracle = do
  result <- ethCall client (CallParams oracle (encodeCall "orderSettlementWindow()" []))
  pure $ fmap decodeUint256 result

orderExecutionStalenessLimit :: EthClient -> Text -> IO (Either RpcError Integer)
orderExecutionStalenessLimit client oracle = do
  result <- ethCall client (CallParams oracle (encodeCall "orderExecutionStalenessLimit()" []))
  pure $ fmap decodeUint256 result

isOracleFrozen :: EthClient -> Text -> IO (Either RpcError Bool)
isOracleFrozen client oracle = do
  result <- ethCall client (CallParams oracle (encodeCall "isOracleFrozen()" []))
  pure $ fmap decodeBool result

getOrderExecutionPolicy :: EthClient -> Text -> Bool -> IO (Either RpcError OrderExecutionPolicy)
getOrderExecutionPolicy client oracle isClose = do
  result <- ethCall client (CallParams oracle (getOrderExecutionPolicyCall isClose))
  pure $ fmap decodeOrderExecutionPolicy result

getUpdateFee :: EthClient -> Text -> [ByteString] -> IO (Either RpcError Integer)
getUpdateFee client oracle updateData = do
  result <- ethCall client (CallParams oracle (getUpdateFeeCall updateData))
  pure $ fmap decodeUint256 result

orderFailureReasonText :: Integer -> Text
orderFailureReasonText = \case
  0 -> "Expired"
  1 -> "CloseOnly"
  2 -> "SlippageExceeded"
  3 -> "EnginePanic"
  4 -> "AccountLiquidated"
  5 -> "EngineRevert"
  n -> "Unknown(" <> T.pack (show n) <> ")"

getPendingOrderViewCall :: Integer -> ByteString
getPendingOrderViewCall orderId =
  encodeCall "getPendingOrderView(uint64)" [encodeUint256 orderId]

getUpdateFeeCall :: [ByteString] -> ByteString
getUpdateFeeCall updateData =
  encodeCall "getUpdateFee(bytes[])" [encodeUint256 32, encodeBytesArray updateData]

getOrderExecutionPolicyCall :: Bool -> ByteString
getOrderExecutionPolicyCall isClose =
  encodeCall "getOrderExecutionPolicy(bool)" [encodeBool isClose]

executeOrderCall :: Integer -> [ByteString] -> ByteString
executeOrderCall orderId updateData =
  encodeCall
    "executeOrder(uint64,bytes[])"
    [ encodeUint256 orderId
    , encodeUint256 64
    , encodeBytesArray updateData
    ]

executeOrderBatchCall :: Integer -> [ByteString] -> ByteString
executeOrderBatchCall maxOrderId updateData =
  encodeCall
    "executeOrderBatch(uint64,bytes[])"
    [ encodeUint256 maxOrderId
    , encodeUint256 64
    , encodeBytesArray updateData
    ]

decodePendingOrderView :: ByteString -> PendingOrderView
decodePendingOrderView bytes =
  PendingOrderView
    { povOrderId = wordAt 0 bytes
    , povIsClose = decodeBool $ wordBytesAt 1 bytes
    , povSide = wordAt 2 bytes
    , povSizeDelta = wordAt 3 bytes
    , povMarginDelta = wordAt 4 bytes
    , povTargetPrice = wordAt 5 bytes
    , povCommitTime = wordAt 6 bytes
    , povCommitBlock = wordAt 7 bytes
    , povCommittedMarginUsdc = wordAt 8 bytes
    , povExecutionBountyUsdc = wordAt 9 bytes
    , povNextAccountOrderId = wordAt 10 bytes
    }

decodeOrderExecutionPolicy :: ByteString -> OrderExecutionPolicy
decodeOrderExecutionPolicy bytes =
  OrderExecutionPolicy
    { oepCloseOnly = decodeBool $ wordBytesAt 0 bytes
    , oepRequireStoredMark = decodeBool $ wordBytesAt 1 bytes
    , oepAllowAnyStoredMark = decodeBool $ wordBytesAt 2 bytes
    , oepMaxStaleness = wordAt 3 bytes
    , oepOracleFrozen = decodeBool $ wordBytesAt 4 bytes
    , oepIsFadWindow = decodeBool $ wordBytesAt 5 bytes
    }

encodeBytesArray :: [ByteString] -> ByteString
encodeBytesArray values =
  let headsLen = 32 * length values
      encodedValues = map encodeDynamicBytes values
      offsets =
        scanl
          (\offset encoded -> offset + fromIntegral (BS.length encoded))
          (fromIntegral headsLen)
          encodedValues
      offsetWords = map encodeUint256 $ take (length values) offsets
   in encodeUint256 (fromIntegral $ length values) <> mconcat offsetWords <> mconcat encodedValues

encodeDynamicBytes :: ByteString -> ByteString
encodeDynamicBytes value =
  encodeUint256 (fromIntegral $ BS.length value)
    <> value
    <> BS.replicate (paddingLength $ BS.length value) 0

paddingLength :: Int -> Int
paddingLength len =
  let remainder = len `mod` 32
   in if remainder == 0 then 0 else 32 - remainder

wordAt :: Int -> ByteString -> Integer
wordAt index = decodeUint256 . wordBytesAt index

wordBytesAt :: Int -> ByteString -> ByteString
wordBytesAt index = BS.take 32 . BS.drop (index * 32)
