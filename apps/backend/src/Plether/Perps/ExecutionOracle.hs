module Plether.Perps.ExecutionOracle
  ( ExecutionOracleSnapshot (..)
  , decodeExecutionUpdateData
  , deriveExecutionOracleSnapshot
  , executionOraclePublishTimeBounds
  ) where

import Control.Monad (unless)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List (foldl')
import Data.Text (Text)
import Plether.Ethereum.Abi (decodeUint256, selector)
import Plether.Ethereum.Contracts.Perps
  ( executeOrderBatchCall
  , executeOrderCall
  )
import Plether.Pyth.Basket
  ( BasketComponentPrice (..)
  , PythPricePoint
  , computeBasketSnapshot
  )

data ExecutionOracleSnapshot = ExecutionOracleSnapshot
  { eosMidpointPrice :: Integer
  , eosMinPublishTime :: Integer
  , eosMaxPublishTime :: Integer
  }
  deriving stock (Show, Eq)

data ExecutionCallKind
  = ExecuteSingle
  | ExecuteBatch

decodeExecutionUpdateData
  :: Integer
  -> ByteString
  -> Either Text [ByteString]
decodeExecutionUpdateData expectedOrderId calldata = do
  unless (expectedOrderId >= 0 && expectedOrderId <= maxUint64) $
    Left "expected order ID is outside the uint64 range"
  unless (BS.length calldata >= selectorLength) $
    Left "execution calldata is missing its selector"
  kind <-
    case BS.take selectorLength calldata of
      actualSelector
        | actualSelector == executeOrderSelector -> Right ExecuteSingle
        | actualSelector == executeOrderBatchSelector -> Right ExecuteBatch
        | otherwise -> Left "execution calldata has an unsupported selector"
  let arguments = BS.drop selectorLength calldata
  orderId <- uintAt "order ID" arguments 0
  unless (orderId <= maxUint64) $
    Left "execution calldata order ID is outside the uint64 range"
  arrayOffset <-
    uintAt
      "update-data array offset"
      arguments
      (toInteger abiWordLength)
  unless (arrayOffset == 2 * toInteger abiWordLength) $
    Left "execution calldata has a non-canonical update-data array offset"
  case kind of
    ExecuteSingle ->
      unless (orderId == expectedOrderId) $
        Left "executeOrder calldata is bound to a different order ID"
    ExecuteBatch ->
      unless (expectedOrderId <= orderId) $
        Left "executeOrderBatch calldata does not cover the expected order ID"
  updateData <- decodeCanonicalBytesArray arguments arrayOffset
  let canonicalCall =
        case kind of
          ExecuteSingle -> executeOrderCall orderId updateData
          ExecuteBatch -> executeOrderBatchCall orderId updateData
  unless (calldata == canonicalCall) $
    Left "execution calldata is not canonically ABI encoded"
  pure updateData

deriveExecutionOracleSnapshot
  :: [PythPricePoint]
  -> Either Text ExecutionOracleSnapshot
deriveExecutionOracleSnapshot pricePoints = do
  (midpointPrice, components) <- computeBasketSnapshot pricePoints
  case map bcpPublishTime components of
    [] -> Left "basket computation returned no component publish times"
    firstPublishTime : remainingPublishTimes ->
      let (minPublishTime, maxPublishTime) =
            foldl'
              (\(minimumTime, maximumTime) publishTime ->
                (min minimumTime publishTime, max maximumTime publishTime)
              )
              (firstPublishTime, firstPublishTime)
              remainingPublishTimes
       in Right
            ExecutionOracleSnapshot
              { eosMidpointPrice = midpointPrice
              , eosMinPublishTime = minPublishTime
              , eosMaxPublishTime = maxPublishTime
              }

executionOraclePublishTimeBounds
  :: Integer
  -> Either Text (Integer, Integer)
executionOraclePublishTimeBounds commitTimestamp
  | commitTimestamp < 0 =
      Left "order commit timestamp is negative"
  | commitTimestamp >= maxUint64 =
      Left "order commit timestamp cannot be advanced within the uint64 range"
  | otherwise =
      Right (commitTimestamp + 1, maxUint64)

decodeCanonicalBytesArray
  :: ByteString
  -> Integer
  -> Either Text [ByteString]
decodeCanonicalBytesArray arguments arrayOffset = do
  valueCount <- uintAt "update-data count" arguments arrayOffset
  let tableLength = valueCount * toInteger abiWordLength
      valuesStart = offsetTableStart + tableLength
  unless (valuesStart <= toInteger (BS.length arguments)) $
    Left "execution calldata has a truncated update-data offset table"
  go valueCount 0 tableLength []
  where
    offsetTableStart = arrayOffset + toInteger abiWordLength
    argumentLength = toInteger $ BS.length arguments

    go remaining index expectedRelativeOffset decoded
      | remaining == 0 = do
          unless (offsetTableStart + expectedRelativeOffset == argumentLength) $
            Left "execution calldata has trailing or missing update-data bytes"
          pure $ reverse decoded
      | otherwise = do
          relativeOffset <-
            uintAt
              "update-data value offset"
              arguments
              (offsetTableStart + index * toInteger abiWordLength)
          unless (relativeOffset == expectedRelativeOffset) $
            Left "execution calldata has non-canonical update-data value offsets"
          let valueStart = offsetTableStart + relativeOffset
          valueLength <- uintAt "update-data value length" arguments valueStart
          let dataStart = valueStart + toInteger abiWordLength
              paddedLength = paddedAbiLength valueLength
              valueEnd = dataStart + valueLength
              paddedEnd = dataStart + paddedLength
          value <- sliceAt "update-data value" arguments dataStart valueLength
          padding <-
            sliceAt
              "update-data value padding"
              arguments
              valueEnd
              (paddedEnd - valueEnd)
          unless (BS.all (== 0) padding) $
            Left "execution calldata has nonzero update-data padding"
          go
            (remaining - 1)
            (index + 1)
            (expectedRelativeOffset + toInteger abiWordLength + paddedLength)
            (value : decoded)

uintAt :: Text -> ByteString -> Integer -> Either Text Integer
uintAt label bytes offset =
  decodeUint256 <$> sliceAt label bytes offset (toInteger abiWordLength)

sliceAt
  :: Text
  -> ByteString
  -> Integer
  -> Integer
  -> Either Text ByteString
sliceAt label bytes offset length'
  | offset < 0 || length' < 0 =
      Left $ label <> " has a negative range"
  | offset > byteLength || length' > byteLength - offset =
      Left $ label <> " is truncated"
  | otherwise =
      Right $
        BS.take (fromInteger length') $
          BS.drop (fromInteger offset) bytes
  where
    byteLength = toInteger $ BS.length bytes

paddedAbiLength :: Integer -> Integer
paddedAbiLength valueLength =
  ((valueLength + toInteger abiWordLength - 1) `div` toInteger abiWordLength)
    * toInteger abiWordLength

executeOrderSelector, executeOrderBatchSelector :: ByteString
executeOrderSelector = selector "executeOrder(uint64,bytes[])"
executeOrderBatchSelector = selector "executeOrderBatch(uint64,bytes[])"

selectorLength, abiWordLength :: Int
selectorLength = 4
abiWordLength = 32

maxUint64 :: Integer
maxUint64 = 2 ^ (64 :: Integer) - 1
