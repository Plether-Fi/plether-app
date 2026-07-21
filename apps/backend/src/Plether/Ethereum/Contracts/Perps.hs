module Plether.Ethereum.Contracts.Perps
  ( PerpsOrderEvent (..)
  , PendingOrderView (..)
  , OrderExecutionPolicy (..)
  , orderCommittedTopic
  , orderExecutedTopic
  , orderFailedTopic
  , perpsOrderTopics
  , positionOpenedTopic
  , positionLiquidatedTopic
  , decodePerpsOrderEvent
  , decodePositionOpenedAccount
  , decodePositionLiquidatedAccount
  , getPendingOrderView
  , getPositionSize
  , getPositionSizes
  , getPositionSizeAtBlock
  , decodePositionSize
  , positionSizeCalls
  , decodePositionSizeBatch
  , maxOrderAge
  , orderSettlementWindow
  , orderExecutionStalenessLimit
  , adverseConfidenceMultiplierBps
  , isOracleFrozen
  , getOrderExecutionPolicy
  , getUpdateFee
  , getPythContract
  , decodePythContract
  , validatePythUpdateData
  , validateUniquePythUpdateData
  , executeOrderCall
  , executeOrderBatchCall
  , executeLiquidationCall
  , positionsCall
  , pythCall
  , getUpdateFeeCall
  , updatePriceFeedsCall
  , parsePriceFeedUpdatesCall
  , parsePriceFeedUpdatesUniqueCall
  , decodeParsedPriceFeedIds
  , adverseConfidenceMultiplierBpsCall
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
  , encodeAddress
  , encodeBool
  , encodeCall
  , encodeUint256
  , keccak256
  )
import Plether.Ethereum.Client
  ( CallParams (..)
  , EthClient
  , RpcError (..)
  , ethCall
  , ethCallAtBlock
  , ethCallWithValue
  )
import qualified Plether.Ethereum.Multicall as Multicall
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

positionOpenedTopic :: ByteString
positionOpenedTopic = keccak256 $ TE.encodeUtf8 "PositionOpened(address,uint8,uint256,uint256,uint256)"

positionLiquidatedTopic :: ByteString
positionLiquidatedTopic = keccak256 $ TE.encodeUtf8 "PositionLiquidated(address,uint8,uint256,uint256,uint256)"

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

decodePositionOpenedAccount :: RpcLog -> Maybe Text
decodePositionOpenedAccount = decodeIndexedPositionAccount positionOpenedTopic

decodePositionLiquidatedAccount :: RpcLog -> Maybe Text
decodePositionLiquidatedAccount = decodeIndexedPositionAccount positionLiquidatedTopic

decodeIndexedPositionAccount :: ByteString -> RpcLog -> Maybe Text
decodeIndexedPositionAccount eventTopic RpcLog {rpcLogTopics = topic : accountTopic : _}
  | topic == eventTopic && BS.length accountTopic == 32 = Just $ decodeAddress accountTopic
decodeIndexedPositionAccount _ _ = Nothing

getPendingOrderView :: EthClient -> Text -> Integer -> IO (Either RpcError PendingOrderView)
getPendingOrderView client orderRouter orderId = do
  result <- ethCall client (CallParams orderRouter (getPendingOrderViewCall orderId))
  pure $ fmap decodePendingOrderView result

getPositionSize :: EthClient -> Text -> Text -> IO (Either RpcError Integer)
getPositionSize client cfdEngine account = do
  result <- ethCall client (CallParams cfdEngine (positionsCall account))
  pure $ result >>= decodePositionSize

getPositionSizes
  :: EthClient
  -> Text
  -> [Text]
  -> IO (Either RpcError [Either RpcError Integer])
getPositionSizes _ _ [] = pure $ Right []
getPositionSizes client cfdEngine accounts = do
  result <- Multicall.multicall client $ positionSizeCalls cfdEngine accounts
  pure $ result >>= decodePositionSizeBatch (length accounts)

positionSizeCalls :: Text -> [Text] -> [Multicall.Call]
positionSizeCalls cfdEngine accounts =
  [ Multicall.Call
      { Multicall.callTarget = cfdEngine
      , Multicall.callAllowFailure = True
      , Multicall.callCalldata = positionsCall account
      }
  | account <- accounts
  ]

decodePositionSizeBatch
  :: Int
  -> [Multicall.CallResult]
  -> Either RpcError [Either RpcError Integer]
decodePositionSizeBatch expectedCount results
  | length results /= expectedCount =
      Left $
        RpcJsonError $
          "positions(address) multicall returned "
            <> T.pack (show $ length results)
            <> " results for "
            <> T.pack (show expectedCount)
            <> " accounts"
  | otherwise = Right $ map decodeResult results
  where
    decodeResult result
      | not $ Multicall.resultSuccess result =
          Left $ RpcJsonError "positions(address) multicall subcall failed"
      | otherwise = decodePositionSize $ Multicall.resultData result

getPositionSizeAtBlock :: EthClient -> Text -> Text -> Integer -> IO (Either RpcError Integer)
getPositionSizeAtBlock client cfdEngine account blockNumber = do
  result <- ethCallAtBlock client (CallParams cfdEngine (positionsCall account)) blockNumber
  pure $ result >>= decodePositionSize

decodePositionSize :: ByteString -> Either RpcError Integer
decodePositionSize bytes
  | BS.length bytes < 7 * 32 =
      Left $ RpcJsonError "positions(address) returned fewer than seven ABI words"
  | otherwise = Right $ wordAt 0 bytes

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

adverseConfidenceMultiplierBps :: EthClient -> Text -> IO (Either RpcError Integer)
adverseConfidenceMultiplierBps client oracle = do
  result <- ethCall client (CallParams oracle adverseConfidenceMultiplierBpsCall)
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
  pure $ do
    bytes <- result
    if BS.length bytes < 32
      then Left $ RpcJsonError "getUpdateFee(bytes[]) returned less than one ABI word"
      else Right $ decodeUint256 bytes

getPythContract :: EthClient -> Text -> IO (Either RpcError Text)
getPythContract client oracle = do
  result <- ethCall client (CallParams oracle pythCall)
  pure $ result >>= decodePythContract

decodePythContract :: ByteString -> Either RpcError Text
decodePythContract bytes
  | BS.length bytes < 32 =
      Left $ RpcJsonError "pyth() returned less than one ABI word"
  | address == "0x0000000000000000000000000000000000000000" =
      Left $ RpcJsonError "pyth() returned the zero address"
  | otherwise = Right address
  where
    address = decodeAddress bytes

validatePythUpdateData
  :: EthClient
  -> Text
  -> [ByteString]
  -> [ByteString]
  -> Integer
  -> Integer
  -> IO (Either RpcError ())
validatePythUpdateData client oracle updateData feedIds minPublishTime maxPublishTime =
  case parsePriceFeedUpdatesCall updateData feedIds minPublishTime maxPublishTime of
    Left err -> pure $ Left err
    Right calldata -> do
      pythResult <- getPythContract client oracle
      case pythResult of
        Left err -> pure $ Left err
        Right pyth -> do
          feeResult <- getUpdateFee client pyth updateData
          case feeResult of
            Left err -> pure $ Left err
            Right fee -> do
              result <-
                ethCallWithValue
                  client
                  (CallParams pyth calldata)
                  fee
              pure $ do
                bytes <- result
                _ <- decodeParsedPriceFeedIds feedIds bytes
                Right ()

validateUniquePythUpdateData
  :: EthClient
  -> Text
  -> [ByteString]
  -> [ByteString]
  -> Integer
  -> Integer
  -> IO (Either RpcError ())
validateUniquePythUpdateData client oracle updateData feedIds minPublishTime maxPublishTime =
  case parsePriceFeedUpdatesUniqueCall updateData feedIds minPublishTime maxPublishTime of
    Left err -> pure $ Left err
    Right calldata -> do
      pythResult <- getPythContract client oracle
      case pythResult of
        Left err -> pure $ Left err
        Right pyth -> do
          feeResult <- getUpdateFee client pyth updateData
          case feeResult of
            Left err -> pure $ Left err
            Right fee -> do
              result <- ethCallWithValue client (CallParams pyth calldata) fee
              pure $ do
                bytes <- result
                _ <- decodeParsedPriceFeedIds feedIds bytes
                Right ()

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

pythCall :: ByteString
pythCall = encodeCall "pyth()" []

getUpdateFeeCall :: [ByteString] -> ByteString
getUpdateFeeCall updateData =
  encodeCall "getUpdateFee(bytes[])" [encodeUint256 32, encodeBytesArray updateData]

updatePriceFeedsCall :: [ByteString] -> ByteString
updatePriceFeedsCall updateData =
  encodeCall "updatePriceFeeds(bytes[])" [encodeUint256 32, encodeBytesArray updateData]

parsePriceFeedUpdatesCall
  :: [ByteString]
  -> [ByteString]
  -> Integer
  -> Integer
  -> Either RpcError ByteString
parsePriceFeedUpdatesCall =
  parsePriceFeedUpdatesCallWith "parsePriceFeedUpdates(bytes[],bytes32[],uint64,uint64)"

parsePriceFeedUpdatesUniqueCall
  :: [ByteString]
  -> [ByteString]
  -> Integer
  -> Integer
  -> Either RpcError ByteString
parsePriceFeedUpdatesUniqueCall =
  parsePriceFeedUpdatesCallWith "parsePriceFeedUpdatesUnique(bytes[],bytes32[],uint64,uint64)"

parsePriceFeedUpdatesCallWith
  :: Text
  -> [ByteString]
  -> [ByteString]
  -> Integer
  -> Integer
  -> Either RpcError ByteString
parsePriceFeedUpdatesCallWith signature updateData feedIds minPublishTime maxPublishTime = do
  if null updateData
    then Left $ RpcJsonError "Pyth update data cannot be empty"
    else Right ()
  if null feedIds
    then Left $ RpcJsonError "Pyth feed IDs cannot be empty"
    else Right ()
  if any ((/= 32) . BS.length) feedIds
    then Left $ RpcJsonError "Pyth feed IDs must each be exactly 32 bytes"
    else Right ()
  if minPublishTime < 0 || maxPublishTime < minPublishTime || maxPublishTime > maxUint64
    then Left $ RpcJsonError "Pyth publish-time bounds must form a valid uint64 range"
    else Right ()
  let encodedUpdateData = encodeBytesArray updateData
      encodedFeedIds = encodeBytes32Array feedIds
      updateDataOffset = 4 * 32
      feedIdsOffset = updateDataOffset + BS.length encodedUpdateData
  Right $
    encodeCall
      signature
      [ encodeUint256 $ fromIntegral updateDataOffset
      , encodeUint256 $ fromIntegral feedIdsOffset
      , encodeUint256 minPublishTime
      , encodeUint256 maxPublishTime
      , encodedUpdateData
      , encodedFeedIds
      ]

decodeParsedPriceFeedIds :: [ByteString] -> ByteString -> Either RpcError [ByteString]
decodeParsedPriceFeedIds expectedFeedIds bytes = do
  actualFeedIds <- decodePriceFeedIds bytes
  if null actualFeedIds
    then Left $ RpcJsonError "parsePriceFeedUpdatesUnique returned no price feeds"
    else if length actualFeedIds /= length expectedFeedIds
      then Left $ RpcJsonError "parsePriceFeedUpdatesUnique returned an unexpected price-feed count"
      else if actualFeedIds /= expectedFeedIds
        then Left $ RpcJsonError "parsePriceFeedUpdatesUnique returned mismatched price-feed IDs"
        else Right actualFeedIds

decodePriceFeedIds :: ByteString -> Either RpcError [ByteString]
decodePriceFeedIds bytes
  | BS.length bytes < 32 =
      Left $ RpcJsonError "parsePriceFeedUpdatesUnique returned truncated ABI data"
  | arrayOffsetWord /= 32 =
      Left $ RpcJsonError "parsePriceFeedUpdatesUnique returned an invalid ABI array offset"
  | BS.length bytes < arrayOffset + 32 =
      Left $ RpcJsonError "parsePriceFeedUpdatesUnique returned a truncated ABI array"
  | priceFeedCount > fromIntegral maximumCompletePriceFeeds =
      Left $ RpcJsonError "parsePriceFeedUpdatesUnique returned truncated price-feed data"
  | otherwise =
      Right
        [ BS.take 32 $ BS.drop (arrayOffset + 32 + index * encodedPriceFeedSize) bytes
        | index <- [0 .. fromIntegral priceFeedCount - 1]
        ]
  where
    arrayOffsetWord = decodeUint256 $ BS.take 32 bytes
    arrayOffset = 32
    priceFeedCount = decodeUint256 $ BS.take 32 $ BS.drop arrayOffset bytes
    maximumCompletePriceFeeds =
      max 0 (BS.length bytes - arrayOffset - 32) `div` encodedPriceFeedSize

encodedPriceFeedSize :: Int
encodedPriceFeedSize = 9 * 32

maxUint64 :: Integer
maxUint64 = 2 ^ (64 :: Integer) - 1

getOrderExecutionPolicyCall :: Bool -> ByteString
getOrderExecutionPolicyCall isClose =
  encodeCall "getOrderExecutionPolicy(bool)" [encodeBool isClose]

adverseConfidenceMultiplierBpsCall :: ByteString
adverseConfidenceMultiplierBpsCall =
  encodeCall "adverseConfidenceMultiplierBps()" []

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

executeLiquidationCall :: Text -> [ByteString] -> ByteString
executeLiquidationCall account updateData =
  encodeCall
    "executeLiquidation(address,bytes[])"
    [ encodeAddress account
    , encodeUint256 64
    , encodeBytesArray updateData
    ]

positionsCall :: Text -> ByteString
positionsCall account =
  encodeCall "positions(address)" [encodeAddress account]

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

encodeBytes32Array :: [ByteString] -> ByteString
encodeBytes32Array values =
  encodeUint256 (fromIntegral $ length values) <> mconcat values

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
