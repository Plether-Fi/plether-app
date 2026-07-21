module Plether.Ethereum.Abi
  ( encodeCall
  , encodeUint256
  , encodeInt256
  , encodeAddress
  , encodeBytes32
  , encodeBool
  , decodeUint256
  , decodeInt256
  , decodeAddress
  , decodeBool
  , decodeBytes
  , decodeMultiple
  , selector
  , keccak256
  ) where

import Crypto.Hash (Digest, Keccak_256, hash)
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

keccak256 :: ByteString -> ByteString
keccak256 bs = convert (hash bs :: Digest Keccak_256)

selector :: Text -> ByteString
selector sig = BS.take 4 $ keccak256 $ TE.encodeUtf8 sig

encodeCall :: Text -> [ByteString] -> ByteString
encodeCall sig args = selector sig <> mconcat args

encodeUint256 :: Integer -> ByteString
encodeUint256 n =
  let bytes = integerToBytes n
      padding = BS.replicate (32 - BS.length bytes) 0
   in padding <> bytes

encodeInt256 :: Integer -> ByteString
encodeInt256 n
  | n >= 0 = encodeUint256 n
  | otherwise =
      let twosComplement = (2 ^ (256 :: Integer)) + n
       in encodeUint256 twosComplement

encodeAddress :: Text -> ByteString
encodeAddress addr =
  let stripped = stripPrefix addr
      bytes = decodeHexBS stripped
      padding = BS.replicate (32 - BS.length bytes) 0
   in padding <> bytes

encodeBytes32 :: ByteString -> ByteString
encodeBytes32 bs
  | BS.length bs >= 32 = BS.take 32 bs
  | otherwise = bs <> BS.replicate (32 - BS.length bs) 0

encodeBool :: Bool -> ByteString
encodeBool True = encodeUint256 1
encodeBool False = encodeUint256 0

decodeUint256 :: ByteString -> Integer
decodeUint256 bs = bytesToInteger $ BS.take 32 $ BS.drop (BS.length bs - 32) bs

decodeInt256 :: ByteString -> Integer
decodeInt256 bs =
  let unsigned = decodeUint256 bs
      signBit = 2 ^ (255 :: Integer)
   in if unsigned >= signBit
        then unsigned - (2 ^ (256 :: Integer))
        else unsigned

decodeAddress :: ByteString -> Text
decodeAddress bs =
  "0x" <> TE.decodeUtf8 (B16.encode $ BS.drop 12 $ BS.take 32 bs)

decodeBool :: ByteString -> Bool
decodeBool bs = decodeUint256 bs /= 0

decodeBytes :: ByteString -> ByteString
decodeBytes bs =
  let offset = fromIntegral $ decodeUint256 (BS.take 32 bs)
      len = fromIntegral $ decodeUint256 (BS.take 32 $ BS.drop offset bs)
   in BS.take len $ BS.drop (offset + 32) bs

decodeMultiple :: [ByteString -> a] -> ByteString -> [a]
decodeMultiple decoders bs = zipWith ($) decoders (chunk32 bs)

chunk32 :: ByteString -> [ByteString]
chunk32 bs
  | BS.length bs < 32 = []
  | otherwise = BS.take 32 bs : chunk32 (BS.drop 32 bs)

stripPrefix :: Text -> Text
stripPrefix txt
  | T.isPrefixOf "0x" txt = T.drop 2 txt
  | T.isPrefixOf "0X" txt = T.drop 2 txt
  | otherwise = txt

decodeHexBS :: Text -> ByteString
decodeHexBS txt = case B16.decode (TE.encodeUtf8 $ T.toLower txt) of
  Right bs -> bs
  Left _ -> BS.empty

integerToBytes :: Integer -> ByteString
integerToBytes 0 = BS.singleton 0
integerToBytes n = BS.pack $ reverse $ go n
  where
    go 0 = []
    go x = fromIntegral (x `mod` 256) : go (x `div` 256)

bytesToInteger :: ByteString -> Integer
bytesToInteger = BS.foldl' (\acc b -> acc * 256 + fromIntegral b) 0
