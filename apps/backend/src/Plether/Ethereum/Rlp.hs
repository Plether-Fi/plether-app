module Plether.Ethereum.Rlp
  ( Rlp (..)
  , rlpBytes
  , rlpInteger
  , rlpList
  , encodeRlp
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word8)

data Rlp
  = RlpBytes ByteString
  | RlpList [Rlp]
  deriving stock (Show, Eq)

rlpBytes :: ByteString -> Rlp
rlpBytes = RlpBytes

rlpInteger :: Integer -> Rlp
rlpInteger value
  | value <= 0 = RlpBytes BS.empty
  | otherwise = RlpBytes $ integerToBytes value

rlpList :: [Rlp] -> Rlp
rlpList = RlpList

encodeRlp :: Rlp -> ByteString
encodeRlp = \case
  RlpBytes bytes -> encodeBytes bytes
  RlpList items -> encodeList $ mconcat $ map encodeRlp items

encodeBytes :: ByteString -> ByteString
encodeBytes bytes
  | BS.length bytes == 1 && BS.head bytes < 0x80 = bytes
  | BS.length bytes <= 55 = BS.cons (0x80 + fromIntegral (BS.length bytes)) bytes
  | otherwise =
      let lenBytes = integerToBytes $ fromIntegral $ BS.length bytes
       in BS.cons (0xb7 + fromIntegral (BS.length lenBytes)) lenBytes <> bytes

encodeList :: ByteString -> ByteString
encodeList payload
  | BS.length payload <= 55 = BS.cons (0xc0 + fromIntegral (BS.length payload)) payload
  | otherwise =
      let lenBytes = integerToBytes $ fromIntegral $ BS.length payload
       in BS.cons (0xf7 + fromIntegral (BS.length lenBytes)) lenBytes <> payload

integerToBytes :: Integer -> ByteString
integerToBytes 0 = BS.empty
integerToBytes n = BS.pack $ reverse $ go n
  where
    go 0 = []
    go x = fromIntegral (x `mod` 256) : go (x `div` 256)
