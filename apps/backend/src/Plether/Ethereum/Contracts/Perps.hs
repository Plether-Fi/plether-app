module Plether.Ethereum.Contracts.Perps
  ( PerpsOrderEvent (..)
  , OrderExecutionResult (..)
  , OrderBatchResult (..)
  , OrderTerminalOutcome (..)
  , LiquidationBatchResult (..)
  , LiquidationBatchItem (..)
  , PendingOrderView (..)
  , OrderExecutionPolicy (..)
  , orderCommittedTopic
  , orderExecutedTopic
  , orderFailedTopic
  , intentRegisteredTopic
  , orderFinalizedTopic
  , perpsOrderTopics
  , positionOpenedTopic
  , positionLiquidatedTopic
  , liquidationBatchItemTopic
  , liquidationBatchStoppedTopic
  , decodePerpsOrderEvent
  , decodePositionOpenedAccount
  , decodePositionLiquidatedAccount
  , decodeLiquidationBatchItem
  , decodeLiquidationBatchStoppedIndex
  , getPendingOrderView
  , pendingPolicyValidUntil
  , lifecycleStatus
  , orderTerminalOutcome
  , decodeOrderTerminalOutcome
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
  , decodeOrderExecutionResult
  , decodeOrderBatchResult
  , executeLiquidationCall
  , executeLiquidationBatchCall
  , settleLpEpochRouterCall
  , settleLpEpochPoolCall
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
  | IntentRegistered
      { poeOrderId :: Integer
      , poeAccount :: Text
      , poeClientOrderId :: Text
      , poeSide :: Integer
      , poeTxHash :: Text
      , poeBlockNumber :: Integer
      }
  | OrderFinalized
      { poeOrderId :: Integer
      , poeAccount :: Text
      , poeClientOrderId :: Text
      , poeReceiptHash :: Text
      , poeLifecycleStatus :: Integer
      , poeTerminalReason :: Integer
      , poeExecutionMode :: Integer
      , poeFailedConstraint :: Integer
      , poeExecutionPrice :: Integer
      , poeTxHash :: Text
      , poeBlockNumber :: Integer
      }
  deriving stock (Show, Eq)

-- | Typed return value from OrderRouter.executeOrder(uint64,bytes[]).
data OrderExecutionResult = OrderExecutionResult
  { oerOrderId :: Integer
  , oerLifecycleStatus :: Integer
  , oerTerminalReason :: Integer
  , oerPendingReason :: Integer
  , oerReceiptHash :: ByteString
  }
  deriving stock (Show, Eq)

-- | Typed return value from OrderRouter.executeOrderBatch(uint64,bytes[]).
data OrderBatchResult = OrderBatchResult
  { obrNextOrderId :: Integer
  , obrTerminalCount :: Integer
  , obrStopReason :: Integer
  }
  deriving stock (Show, Eq)

-- | Canonical immutable terminal state returned by
-- OrderLifecycleBook.outcome(uint64). Transaction identity deliberately does
-- not live here: the receipt hash commits to the lifecycle receipt, but is not
-- an Ethereum transaction hash.
data OrderTerminalOutcome = OrderTerminalOutcome
  { otoLifecycleStatus :: Integer
  , otoTerminalReason :: Integer
  , otoExecutionMode :: Integer
  , otoTerminalBlock :: Integer
  , otoExecutionPrice :: Integer
  , otoFailedConstraint :: Integer
  , otoReceiptHash :: ByteString
  }
  deriving stock (Show, Eq)

data LiquidationBatchResult
  = LiquidationBatchLiquidated
  | LiquidationBatchSkippedNoPosition
  | LiquidationBatchSkippedSolvent
  | LiquidationBatchFailed
  deriving stock (Show, Eq)

data LiquidationBatchItem = LiquidationBatchItem
  { lbiIndex :: Integer
  , lbiAccount :: Text
  , lbiResult :: LiquidationBatchResult
  , lbiKeeperBountyUsdc :: Integer
  , lbiErrorSelector :: ByteString
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

intentRegisteredTopic :: ByteString
intentRegisteredTopic =
  keccak256 $ TE.encodeUtf8
    "IntentRegistered(uint64,address,bytes32,bytes32,uint256,(bytes32,uint8,uint256,uint256,uint256,bool,(uint64,uint8,bytes32,uint256,uint256,uint256,uint256,uint256,uint256,uint256,uint256,uint32)))"

orderFinalizedTopic :: ByteString
orderFinalizedTopic =
  keccak256 $ TE.encodeUtf8
    "OrderFinalized(uint64,address,bytes32,bytes32,uint64,uint64,(uint64,address,bytes32,bytes32,bytes32,bytes32,uint8,uint8,uint8,address,uint8,uint256,uint256,uint256,uint64,bool,uint256,address,uint8,(bytes4,uint8,uint8,uint8,uint256,uint256,bytes32),(uint256,int256,int256,int256,uint256,uint256,uint256,uint256,uint256,uint256,uint256,uint256,uint256,uint256,uint256,int256,uint256)))"

perpsOrderTopics :: [ByteString]
perpsOrderTopics =
  [ orderCommittedTopic
  , orderExecutedTopic
  , orderFailedTopic
  , intentRegisteredTopic
  , orderFinalizedTopic
  ]

positionOpenedTopic :: ByteString
positionOpenedTopic = keccak256 $ TE.encodeUtf8 "PositionOpened(address,uint8,uint256,uint256,uint256)"

positionLiquidatedTopic :: ByteString
positionLiquidatedTopic = keccak256 $ TE.encodeUtf8 "PositionLiquidated(address,uint8,uint256,uint256,uint256)"

liquidationBatchItemTopic :: ByteString
liquidationBatchItemTopic =
  keccak256 $ TE.encodeUtf8 "LiquidationBatchItem(uint256,address,uint8,uint256,bytes4)"

liquidationBatchStoppedTopic :: ByteString
liquidationBatchStoppedTopic = keccak256 $ TE.encodeUtf8 "LiquidationBatchStopped(uint256)"

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
    topic : orderTopic : accountTopic : clientOrderIdTopic : _
      | topic == intentRegisteredTopic
          && BS.length rpcLogData == 20 * 32 ->
          Just $
            IntentRegistered
              { poeOrderId = decodeUint256 orderTopic
              , poeAccount = decodeAddress accountTopic
              , poeClientOrderId = hexWord clientOrderIdTopic
              , poeSide = wordAt 3 rpcLogData
              , poeTxHash = rpcLogTxHash
              , poeBlockNumber = rpcLogBlockNumber
              }
      | topic == orderFinalizedTopic
          && BS.length rpcLogData == 46 * 32 ->
          Just $
            OrderFinalized
              { poeOrderId = decodeUint256 orderTopic
              , poeAccount = decodeAddress accountTopic
              , poeClientOrderId = hexWord clientOrderIdTopic
              , poeReceiptHash = hexWord $ wordBytes 0 rpcLogData
              , poeLifecycleStatus = wordAt 9 rpcLogData
              , poeTerminalReason = wordAt 10 rpcLogData
              , poeExecutionMode = wordAt 11 rpcLogData
              , poeFailedConstraint = wordAt 25 rpcLogData
              , poeExecutionPrice = wordAt 14 rpcLogData
              , poeTxHash = rpcLogTxHash
              , poeBlockNumber = rpcLogBlockNumber
              }
    _ -> Nothing

wordBytes :: Int -> ByteString -> ByteString
wordBytes index = BS.take 32 . BS.drop (index * 32)

hexWord :: ByteString -> Text
hexWord = ("0x" <>) . TE.decodeUtf8 . B16.encode

decodePositionOpenedAccount :: RpcLog -> Maybe Text
decodePositionOpenedAccount = decodeIndexedPositionAccount positionOpenedTopic

decodePositionLiquidatedAccount :: RpcLog -> Maybe Text
decodePositionLiquidatedAccount = decodeIndexedPositionAccount positionLiquidatedTopic

decodeLiquidationBatchItem :: RpcLog -> Maybe LiquidationBatchItem
decodeLiquidationBatchItem RpcLog {rpcLogTopics = topic : indexTopic : accountTopic : _, ..}
  | topic == liquidationBatchItemTopic
      && BS.length indexTopic == 32
      && BS.length accountTopic == 32
      && BS.length rpcLogData >= 96 = do
      result <- decodeLiquidationBatchResult $ wordAt 0 rpcLogData
      pure
        LiquidationBatchItem
          { lbiIndex = decodeUint256 indexTopic
          , lbiAccount = decodeAddress accountTopic
          , lbiResult = result
          , lbiKeeperBountyUsdc = wordAt 1 rpcLogData
          , lbiErrorSelector = BS.take 4 $ wordBytesAt 2 rpcLogData
          }
decodeLiquidationBatchItem _ = Nothing

decodeLiquidationBatchStoppedIndex :: RpcLog -> Maybe Integer
decodeLiquidationBatchStoppedIndex RpcLog {rpcLogTopics = topic : indexTopic : _}
  | topic == liquidationBatchStoppedTopic && BS.length indexTopic == 32 =
      Just $ decodeUint256 indexTopic
decodeLiquidationBatchStoppedIndex _ = Nothing

decodeLiquidationBatchResult :: Integer -> Maybe LiquidationBatchResult
decodeLiquidationBatchResult = \case
  0 -> Just LiquidationBatchLiquidated
  1 -> Just LiquidationBatchSkippedNoPosition
  2 -> Just LiquidationBatchSkippedSolvent
  3 -> Just LiquidationBatchFailed
  _ -> Nothing

decodeIndexedPositionAccount :: ByteString -> RpcLog -> Maybe Text
decodeIndexedPositionAccount eventTopic RpcLog {rpcLogTopics = topic : accountTopic : _}
  | topic == eventTopic && BS.length accountTopic == 32 = Just $ decodeAddress accountTopic
decodeIndexedPositionAccount _ _ = Nothing

getPendingOrderView :: EthClient -> Text -> Integer -> IO (Either RpcError PendingOrderView)
getPendingOrderView client orderRouter orderId = do
  result <- ethCall client (CallParams orderRouter (getPendingOrderViewCall orderId))
  pure $ fmap decodePendingOrderView result

pendingPolicyValidUntil :: EthClient -> Text -> Integer -> IO (Either RpcError Integer)
pendingPolicyValidUntil client lifecycleBook orderId = do
  result <-
    ethCall
      client
      (CallParams lifecycleBook $ encodeCall "pendingPolicy(uint64)" [encodeUint256 orderId])
  pure $ result >>= decodePendingPolicyValidUntil

lifecycleStatus :: EthClient -> Text -> Integer -> IO (Either RpcError Integer)
lifecycleStatus client lifecycleBook orderId = do
  result <-
    ethCall
      client
      (CallParams lifecycleBook $ encodeCall "lifecycleStatus(uint64)" [encodeUint256 orderId])
  pure $ result >>= decodeLifecycleStatus

orderTerminalOutcome :: EthClient -> Text -> Integer -> IO (Either RpcError OrderTerminalOutcome)
orderTerminalOutcome client lifecycleBook orderId = do
  result <-
    ethCall
      client
      (CallParams lifecycleBook $ encodeCall "outcome(uint64)" [encodeUint256 orderId])
  pure $ result >>= decodeOrderTerminalOutcome

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

maxUint32 :: Integer
maxUint32 = 2 ^ (32 :: Integer) - 1

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

decodeOrderExecutionResult :: ByteString -> Either RpcError OrderExecutionResult
decodeOrderExecutionResult bytes
  | BS.length bytes /= 5 * 32 =
      Left $ RpcJsonError "executeOrder returned an invalid ExecutionResult length"
  | orderId > maxUint64 =
      Left $ RpcJsonError "executeOrder returned an out-of-range order ID"
  | resultStatus > 3 =
      Left $ RpcJsonError "executeOrder returned an invalid lifecycle status"
  | terminalReason > 9 =
      Left $ RpcJsonError "executeOrder returned an invalid terminal reason"
  | pendingReason > 9 =
      Left $ RpcJsonError "executeOrder returned an invalid pending reason"
  | otherwise =
      Right
        OrderExecutionResult
          { oerOrderId = orderId
          , oerLifecycleStatus = resultStatus
          , oerTerminalReason = terminalReason
          , oerPendingReason = pendingReason
          , oerReceiptHash = wordBytesAt 4 bytes
          }
  where
    orderId = wordAt 0 bytes
    resultStatus = wordAt 1 bytes
    terminalReason = wordAt 2 bytes
    pendingReason = wordAt 3 bytes

decodeOrderBatchResult :: ByteString -> Either RpcError OrderBatchResult
decodeOrderBatchResult bytes
  | BS.length bytes /= 3 * 32 =
      Left $ RpcJsonError "executeOrderBatch returned an invalid BatchResult length"
  | nextOrderId > maxUint64 =
      Left $ RpcJsonError "executeOrderBatch returned an out-of-range next order ID"
  | terminalCount > maxUint32 =
      Left $ RpcJsonError "executeOrderBatch returned an out-of-range terminal count"
  | stopReason > 9 =
      Left $ RpcJsonError "executeOrderBatch returned an invalid pending reason"
  | otherwise =
      Right
        OrderBatchResult
          { obrNextOrderId = nextOrderId
          , obrTerminalCount = terminalCount
          , obrStopReason = stopReason
          }
  where
    nextOrderId = wordAt 0 bytes
    terminalCount = wordAt 1 bytes
    stopReason = wordAt 2 bytes

executeLiquidationCall :: Text -> [ByteString] -> ByteString
executeLiquidationCall account updateData =
  encodeCall
    "executeLiquidation(address,bytes[])"
    [ encodeAddress account
    , encodeUint256 64
    , encodeBytesArray updateData
    ]

executeLiquidationBatchCall :: [Text] -> [ByteString] -> ByteString
executeLiquidationBatchCall accounts updateData =
  let encodedAccounts = encodeAddressArray accounts
      encodedUpdateData = encodeBytesArray updateData
   in encodeCall
        "executeLiquidationBatch(address[],bytes[])"
        [ encodeUint256 64
        , encodeUint256 $ 64 + fromIntegral (BS.length encodedAccounts)
        , encodedAccounts
        , encodedUpdateData
        ]

settleLpEpochRouterCall :: [ByteString] -> ByteString
settleLpEpochRouterCall updateData =
  encodeCall
    "settleLpEpoch(bytes[])"
    [ encodeUint256 32
    , encodeBytesArray updateData
    ]

settleLpEpochPoolCall :: Integer -> Integer -> ByteString
settleLpEpochPoolCall cachedMarkPrice cachedMarkTime =
  encodeCall
    "settleLpEpoch(uint256,uint256)"
    [encodeUint256 cachedMarkPrice, encodeUint256 cachedMarkTime]

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

decodePendingPolicyValidUntil :: ByteString -> Either RpcError Integer
decodePendingPolicyValidUntil bytes
  | BS.length bytes < 32 =
      Left $ RpcJsonError "pendingPolicy(uint64) returned no ABI words"
  | validUntil > maxUint64 =
      Left $ RpcJsonError "pendingPolicy(uint64) returned an out-of-range validUntil"
  | otherwise = Right validUntil
  where
    validUntil = wordAt 0 bytes

decodeLifecycleStatus :: ByteString -> Either RpcError Integer
decodeLifecycleStatus bytes
  | BS.length bytes /= 32 =
      Left $ RpcJsonError "lifecycleStatus(uint64) returned an invalid ABI length"
  | status > 3 =
      Left $ RpcJsonError "lifecycleStatus(uint64) returned an invalid lifecycle status"
  | otherwise = Right status
  where
    status = wordAt 0 bytes

decodeOrderTerminalOutcome :: ByteString -> Either RpcError OrderTerminalOutcome
decodeOrderTerminalOutcome bytes
  | BS.length bytes /= 23 * 32 =
      Left $ RpcJsonError "outcome(uint64) returned an invalid terminal outcome length"
  | lifecycleStatus' < 2 || lifecycleStatus' > 3 =
      Left $ RpcJsonError "outcome(uint64) did not return a terminal lifecycle status"
  | terminalReason == 0 || terminalReason > 9 =
      Left $ RpcJsonError "outcome(uint64) returned an invalid terminal reason"
  | executionMode > 3 =
      Left $ RpcJsonError "outcome(uint64) returned an invalid execution mode"
  | terminalBlock == 0 || terminalBlock > maxUint64 =
      Left $ RpcJsonError "outcome(uint64) returned an invalid terminal block"
  | failedConstraint > 9 =
      Left $ RpcJsonError "outcome(uint64) returned an invalid failed constraint"
  | lifecycleStatus' == 2 && terminalReason /= 1 =
      Left $ RpcJsonError "outcome(uint64) returned an executed status without an executed reason"
  | lifecycleStatus' == 3 && terminalReason == 1 =
      Left $ RpcJsonError "outcome(uint64) returned a failed status with an executed reason"
  | otherwise =
      Right
        OrderTerminalOutcome
          { otoLifecycleStatus = lifecycleStatus'
          , otoTerminalReason = terminalReason
          , otoExecutionMode = executionMode
          , otoTerminalBlock = terminalBlock
          , otoExecutionPrice = wordAt 15 bytes
          , otoFailedConstraint = failedConstraint
          , otoReceiptHash = wordBytesAt 22 bytes
          }
  where
    lifecycleStatus' = wordAt 5 bytes
    terminalReason = wordAt 6 bytes
    executionMode = wordAt 7 bytes
    terminalBlock = wordAt 10 bytes
    failedConstraint = wordAt 20 bytes

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

encodeAddressArray :: [Text] -> ByteString
encodeAddressArray values =
  encodeUint256 (fromIntegral $ length values) <> mconcat (map encodeAddress values)

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
