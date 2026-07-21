module Plether.Ethereum.Multicall
  ( Call (..)
  , CallResult (..)
  , multicall
  , multicallAtBlock
  , multicallAddress
  , decodeResults
  , decodeResultsEither
  ) where

import Control.Monad (forM, unless, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import Plether.Ethereum.Abi (decodeUint256, encodeAddress, encodeBool, encodeCall, encodeUint256)
import Plether.Ethereum.Client
  ( CallParams (..)
  , EthClient
  , RpcError (..)
  , ethCall
  , ethCallAtBlock
  )

multicallAddress :: Text
multicallAddress = "0xcA11bde05977b3631167028862bE2a173976CA11"

data Call = Call
  { callTarget :: Text
  , callAllowFailure :: Bool
  , callCalldata :: ByteString
  }
  deriving stock (Show)

data CallResult = CallResult
  { resultSuccess :: Bool
  , resultData :: ByteString
  }
  deriving stock (Eq, Show)

multicall :: EthClient -> [Call] -> IO (Either RpcError [CallResult])
multicall client calls =
  runMulticall
    (ethCall client)
    calls

-- | Run Multicall3's @aggregate3@ against the state at one exact block.
-- Every subcall therefore observes the same numeric JSON-RPC block tag.
multicallAtBlock
  :: EthClient
  -> [Call]
  -> Integer
  -> IO (Either RpcError [CallResult])
multicallAtBlock client calls blockNumber =
  runMulticall
    (\params -> ethCallAtBlock client params blockNumber)
    calls

runMulticall
  :: (CallParams -> IO (Either RpcError ByteString))
  -> [Call]
  -> IO (Either RpcError [CallResult])
runMulticall executeCall calls = do
  result <-
    executeCall $
      CallParams multicallAddress (encodeAggregate3 calls)
  pure $ case result of
    Left err -> Left err
    Right bytes ->
      case decodeResultsEither bytes of
        Left err -> Left $ RpcJsonError $ "Malformed Multicall3 response: " <> err
        Right results -> Right results

encodeAggregate3 :: [Call] -> ByteString
encodeAggregate3 calls =
  let callsEncoded = map encodeCallStruct calls
      offsetToArray = encodeUint256 32
      arrayLength = encodeUint256 (fromIntegral $ length calls)
      callOffsets = calculateOffsets callsEncoded
      callsData = mconcat callsEncoded
   in encodeCall "aggregate3((address,bool,bytes)[])" []
        <> offsetToArray
        <> arrayLength
        <> mconcat (map encodeUint256 callOffsets)
        <> callsData

encodeCallStruct :: Call -> ByteString
encodeCallStruct Call {..} =
  let targetEncoded = encodeAddress callTarget
      allowFailureEncoded = encodeBool callAllowFailure
      calldataOffset = encodeUint256 96
      calldataLength = encodeUint256 (fromIntegral $ BS.length callCalldata)
      calldataPadded = callCalldata <> BS.replicate (padTo32 (BS.length callCalldata)) 0
   in targetEncoded <> allowFailureEncoded <> calldataOffset <> calldataLength <> calldataPadded

calculateOffsets :: [ByteString] -> [Integer]
calculateOffsets encodedCalls =
  let headerSize = 32 * length encodedCalls
      sizes = map (fromIntegral . BS.length) encodedCalls
   in scanl (+) (fromIntegral headerSize) (init sizes)

padTo32 :: Int -> Int
padTo32 n = (32 - (n `mod` 32)) `mod` 32

-- | Backwards-compatible permissive decoder. New RPC paths use
-- 'decodeResultsEither' so truncated or otherwise malformed ABI data cannot be
-- mistaken for a shorter successful response.
decodeResults :: ByteString -> [CallResult]
decodeResults = either (const []) id . decodeResultsEither

-- | Strictly decode the dynamic @Result[]@ returned by @aggregate3@.
decodeResultsEither :: ByteString -> Either Text [CallResult]
decodeResultsEither bytes = do
  arrayOffset <- wordAsOffset "result array offset" bytes 0
  requireAligned "result array offset" arrayOffset
  resultCountInteger <- wordAt "result count" bytes arrayOffset
  resultCount <- integerAsInt "result count" resultCountInteger
  headersStart <- checkedAdd "result headers start" arrayOffset abiWordLength
  headersLength <- checkedMultiply "result headers length" resultCount abiWordLength
  headersEnd <- checkedAdd "result headers end" headersStart headersLength
  requireAvailable "result headers" bytes headersStart headersEnd
  relativeOffsets <- forM [0 .. resultCount - 1] $ \index -> do
    offsetPosition <-
      checkedAdd
        "result offset position"
        headersStart
        (index * abiWordLength)
    relativeOffset <-
      wordAsOffset
        ("result " <> tshow index <> " offset")
        bytes
        offsetPosition
    requireAligned ("result " <> tshow index <> " offset") relativeOffset
    when (relativeOffset < headersLength) $
      Left $
        "result "
          <> tshow index
          <> " offset points inside the result headers"
    pure relativeOffset
  validateStrictlyIncreasingOffsets relativeOffsets
  decoded <- forM (zip [0 ..] relativeOffsets) $ \(index, relativeOffset) -> do
    resultStart <- checkedAdd ("result " <> tshow index <> " start") headersStart relativeOffset
    (result, resultEnd) <- decodeResultStruct bytes index resultStart
    pure (index, resultStart, resultEnd, result)
  validateNonOverlappingResults decoded
  pure [result | (_, _, _, result) <- decoded]

decodeResultStruct :: ByteString -> Int -> Int -> Either Text (CallResult, Int)
decodeResultStruct bytes index resultStart = do
  successWord <- wordAt label bytes resultStart
  success <- case successWord of
    0 -> Right False
    1 -> Right True
    _ -> Left $ label <> " has a non-boolean success value"
  dataOffsetPosition <- checkedAdd (label <> " data offset position") resultStart abiWordLength
  dataOffset <- wordAsOffset (label <> " data offset") bytes dataOffsetPosition
  requireAligned (label <> " data offset") dataOffset
  when (dataOffset < 2 * abiWordLength) $
    Left $ label <> " data offset points inside the result header"
  dataLengthPosition <- checkedAdd (label <> " data length position") resultStart dataOffset
  dataLengthInteger <- wordAt (label <> " data length") bytes dataLengthPosition
  dataLength <- integerAsInt (label <> " data length") dataLengthInteger
  dataStart <- checkedAdd (label <> " data start") dataLengthPosition abiWordLength
  dataEnd <- checkedAdd (label <> " data end") dataStart dataLength
  paddedDataEnd <- checkedAdd (label <> " padded data end") dataEnd (padTo32 dataLength)
  requireAvailable (label <> " padded data") bytes dataStart paddedDataEnd
  pure
    ( CallResult success $ BS.take dataLength $ BS.drop dataStart bytes
    , paddedDataEnd
    )
  where
    label = "result " <> tshow index

validateStrictlyIncreasingOffsets :: [Int] -> Either Text ()
validateStrictlyIncreasingOffsets (current : next : remaining)
  | current >= next = Left "result offsets must be strictly increasing"
  | otherwise = validateStrictlyIncreasingOffsets (next : remaining)
validateStrictlyIncreasingOffsets _ = Right ()

validateNonOverlappingResults :: [(Int, Int, Int, CallResult)] -> Either Text ()
validateNonOverlappingResults
  ((index, _, resultEnd, _) : next@(_, nextStart, _, _) : remaining)
    | resultEnd > nextStart =
        Left $ "result " <> tshow index <> " overlaps the next result"
    | otherwise = validateNonOverlappingResults (next : remaining)
validateNonOverlappingResults _ = Right ()

abiWordLength :: Int
abiWordLength = 32

wordAt :: Text -> ByteString -> Int -> Either Text Integer
wordAt label bytes offset = do
  end <- checkedAdd (label <> " end") offset abiWordLength
  requireAvailable label bytes offset end
  pure $ decodeUint256 $ BS.take abiWordLength $ BS.drop offset bytes

wordAsOffset :: Text -> ByteString -> Int -> Either Text Int
wordAsOffset label bytes offset =
  wordAt label bytes offset >>= integerAsInt label

integerAsInt :: Text -> Integer -> Either Text Int
integerAsInt label value
  | value < 0 = Left $ label <> " cannot be negative"
  | value > toInteger (maxBound :: Int) = Left $ label <> " exceeds the platform limit"
  | otherwise = Right $ fromInteger value

checkedAdd :: Text -> Int -> Int -> Either Text Int
checkedAdd label left right
  | left < 0 || right < 0 = Left $ label <> " cannot be negative"
  | left > maxBound - right = Left $ label <> " exceeds the platform limit"
  | otherwise = Right $ left + right

checkedMultiply :: Text -> Int -> Int -> Either Text Int
checkedMultiply label left right
  | left < 0 || right < 0 = Left $ label <> " cannot be negative"
  | left /= 0 && right > maxBound `div` left = Left $ label <> " exceeds the platform limit"
  | otherwise = Right $ left * right

requireAligned :: Text -> Int -> Either Text ()
requireAligned label offset =
  unless (offset `mod` abiWordLength == 0) $
    Left $ label <> " is not ABI-word aligned"

requireAvailable :: Text -> ByteString -> Int -> Int -> Either Text ()
requireAvailable label bytes start end =
  unless (start >= 0 && end >= start && end <= BS.length bytes) $
    Left $ label <> " extends past the response body"

tshow :: Show a => a -> Text
tshow = T.pack . show
