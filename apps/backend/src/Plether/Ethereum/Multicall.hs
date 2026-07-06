module Plether.Ethereum.Multicall
  ( Call (..)
  , CallResult (..)
  , multicall
  , multicallAddress
  , decodeResults
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import Plether.Ethereum.Abi (decodeUint256, encodeAddress, encodeBool, encodeCall, encodeUint256)
import Plether.Ethereum.Client (CallParams (..), EthClient, RpcError, ethCall)

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
multicall client calls = do
  let calldata = encodeAggregate3 calls
  result <- ethCall client (CallParams multicallAddress calldata)
  pure $ case result of
    Left err -> Left err
    Right bs -> Right $ decodeResults bs

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

decodeResults :: ByteString -> [CallResult]
decodeResults bs
  | BS.length bs < 64 = []
  | otherwise =
      let offset = fromIntegral $ decodeUint256 (BS.take 32 bs)
          arrayData = BS.drop offset bs
          numResults = fromIntegral $ decodeUint256 (BS.take 32 arrayData)
          resultsData = BS.drop 32 arrayData
       in decodeResultArray numResults resultsData

decodeResultArray :: Int -> ByteString -> [CallResult]
decodeResultArray 0 _ = []
decodeResultArray n bs
  | BS.length bs < 64 = []
  | otherwise =
      let offsets :: [Int]
          offsets = map (\i -> fromIntegral $ decodeUint256 (BS.take 32 $ BS.drop (i * 32) bs)) [0 .. n - 1]
          results = map (decodeResultStruct . flip BS.drop bs) offsets
       in results

decodeResultStruct :: ByteString -> CallResult
decodeResultStruct bs =
  let success = decodeUint256 (BS.take 32 bs) /= 0
      dataOffset = fromIntegral $ decodeUint256 (BS.take 32 $ BS.drop 32 bs)
      dataLen = fromIntegral $ decodeUint256 (BS.take 32 $ BS.drop dataOffset bs)
      resultData = BS.take dataLen $ BS.drop (dataOffset + 32) bs
   in CallResult success resultData
