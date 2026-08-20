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
  , getPositionSizeAtBlock
  , decodePositionSize
  , maxOrderAge
  , orderSettlementWindow
  , orderExecutionStalenessLimit
  , adverseConfidenceMultiplierBps
  , isOracleFrozen
  , getOrderExecutionPolicy
  , getUpdateFee
  , getPythContract
  , decodePythContract
  , parsePythUpdateData
  , parseUniquePythUpdateData
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
  , decodeParsedPriceFeeds
  , decodeParsedPriceFeedIds
  , adverseConfidenceMultiplierBpsCall
  , orderFailureReasonText
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Plether.Ethereum.Abi
  ( decodeAddress
  , decodeBool
  , decodeInt256
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
import Plether.Ethereum.Rpc (RpcLog (..))
import Plether.Pyth.Basket (PythPricePoint (..))

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
  fmap (fmap (const ())) $
    parsePythUpdateData client oracle updateData feedIds minPublishTime maxPublishTime

parsePythUpdateData
  :: EthClient
  -> Text
  -> [ByteString]
  -> [ByteString]
  -> Integer
  -> Integer
  -> IO (Either RpcError [PythPricePoint])
parsePythUpdateData =
  parsePythUpdateDataWith parsePriceFeedUpdatesCall

validateUniquePythUpdateData
  :: EthClient
  -> Text
  -> [ByteString]
  -> [ByteString]
  -> Integer
  -> Integer
  -> IO (Either RpcError ())
validateUniquePythUpdateData client oracle updateData feedIds minPublishTime maxPublishTime =
  fmap (fmap (const ())) $
    parseUniquePythUpdateData client oracle updateData feedIds minPublishTime maxPublishTime

parseUniquePythUpdateData
  :: EthClient
  -> Text
  -> [ByteString]
  -> [ByteString]
  -> Integer
  -> Integer
  -> IO (Either RpcError [PythPricePoint])
parseUniquePythUpdateData =
  parsePythUpdateDataWith parsePriceFeedUpdatesUniqueCall

parsePythUpdateDataWith
  :: ([ByteString] -> [ByteString] -> Integer -> Integer -> Either RpcError ByteString)
  -> EthClient
  -> Text
  -> [ByteString]
  -> [ByteString]
  -> Integer
  -> Integer
  -> IO (Either RpcError [PythPricePoint])
parsePythUpdateDataWith makeCalldata client oracle updateData feedIds minPublishTime maxPublishTime =
  case makeCalldata updateData feedIds minPublishTime maxPublishTime of
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
              pure $ result >>= decodeParsedPriceFeeds feedIds

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
  -- Pyth applies both EVM bounds inclusively. In particular, an exact-time
  -- query with minPublishTime == maxPublishTime is valid and must remain so.
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

decodeParsedPriceFeeds :: [ByteString] -> ByteString -> Either RpcError [PythPricePoint]
decodeParsedPriceFeeds expectedFeedIds bytes = do
  if null expectedFeedIds
    then Left $ RpcJsonError "Pyth price-feed parser was given no expected feed IDs"
    else Right ()
  if any ((/= 32) . BS.length) expectedFeedIds
    then Left $ RpcJsonError "Pyth price-feed parser expected a feed ID that was not 32 bytes"
    else Right ()
  if BS.length bytes < 2 * 32
    then Left $ RpcJsonError "Pyth price-feed parser returned truncated ABI data"
    else Right ()
  if arrayOffset /= 32
    then Left $ RpcJsonError "Pyth price-feed parser returned an invalid ABI array offset"
    else Right ()
  if priceFeedCount /= fromIntegral expectedCount
    then Left $ RpcJsonError "Pyth price-feed parser returned an unexpected price-feed count"
    else Right ()
  if BS.length bytes /= expectedEncodedLength
    then Left $ RpcJsonError "Pyth price-feed parser returned a non-canonical PriceFeed[] length"
    else Right ()
  traverse decodePriceFeed $ zip3 [0 :: Int ..] expectedFeedIds priceFeedBytes
  where
    arrayOffset = decodeUint256 $ BS.take 32 bytes
    priceFeedCount = decodeUint256 $ BS.take 32 $ BS.drop 32 bytes
    expectedCount = length expectedFeedIds
    expectedEncodedLength = 2 * 32 + expectedCount * encodedPriceFeedSize
    priceFeedBytes =
      [ BS.take encodedPriceFeedSize $
          BS.drop (2 * 32 + index * encodedPriceFeedSize) bytes
      | index <- [0 .. expectedCount - 1]
      ]

    decodePriceFeed (index, expectedFeedId, encoded) = do
      let actualFeedId = priceFeedWord 0 encoded
      if actualFeedId /= expectedFeedId
        then
          Left $
            RpcJsonError $
              "Pyth price-feed parser returned a mismatched feed ID at index "
                <> T.pack (show index)
        else Right ()
      (price, confidence, priceExponent, publishTime) <-
        decodePrice "price" index $ BS.drop 32 encoded
      -- A PriceFeed contains both price and emaPrice. The worker does not use
      -- emaPrice, but decoding it here prevents malformed trailing struct words
      -- from being admitted alongside an otherwise valid current price.
      _ <- decodePrice "EMA price" index $ BS.drop (5 * 32) encoded
      pure
        PythPricePoint
          { pppFeedId = "0x" <> TE.decodeUtf8 (B16.encode actualFeedId)
          , pppPrice = price
          , pppConfidence = confidence
          , pppExponent = priceExponent
          , pppPublishTime = publishTime
          }

    decodePrice label index encoded = do
      let price = decodeInt256 $ priceWord 0
          confidence = decodeUint256 $ priceWord 1
          exponentInteger = decodeInt256 $ priceWord 2
          publishTime = decodeUint256 $ priceWord 3
          fieldError fieldName =
            RpcJsonError $
              "Pyth price-feed parser returned an invalid "
                <> label
                <> " "
                <> fieldName
                <> " at index "
                <> T.pack (show index)
      if price < minInt64 || price > maxInt64
        then Left $ fieldError "int64 value"
        else Right ()
      if confidence > maxUint64
        then Left $ fieldError "uint64 confidence"
        else Right ()
      if exponentInteger < minInt32 || exponentInteger > maxInt32
        then Left $ fieldError "int32 exponent"
        else Right ()
      pure (price, confidence, fromInteger exponentInteger :: Int, publishTime)
      where
        priceWord wordIndex = BS.take 32 $ BS.drop (wordIndex * 32) encoded

    priceFeedWord wordIndex encoded =
      BS.take 32 $ BS.drop (wordIndex * 32) encoded

decodeParsedPriceFeedIds :: [ByteString] -> ByteString -> Either RpcError [ByteString]
decodeParsedPriceFeedIds expectedFeedIds bytes = do
  _ <- decodeParsedPriceFeeds expectedFeedIds bytes
  pure expectedFeedIds

encodedPriceFeedSize :: Int
encodedPriceFeedSize = 9 * 32

minInt32, maxInt32, minInt64, maxInt64 :: Integer
minInt32 = negate $ 2 ^ (31 :: Integer)
maxInt32 = 2 ^ (31 :: Integer) - 1
minInt64 = negate $ 2 ^ (63 :: Integer)
maxInt64 = 2 ^ (63 :: Integer) - 1

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
