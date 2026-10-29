module Plether.Ethereum.Rlp
  ( Rlp (..)
  , rlpBytes
  , rlpInteger
  , rlpList
  , encodeRlp
  , decodeRlp
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word8)
import Data.Text (Text)

-- Bound nesting and lengths before allocating or recursing. Round-tripping
-- rejects noncanonical length encodings as well as trailing bytes.
decodeRlp :: ByteString -> Either Text Rlp
decodeRlp bytes = do
  (value, rest) <- item (0 :: Int) bytes
  if BS.null rest && encodeRlp value == bytes then Right value
    else Left "Noncanonical RLP"
 where
  item depth input
    | depth > 16 = Left "RLP nesting limit"
    | otherwise = case BS.uncons input of
        Nothing -> Left "Truncated RLP"
        Just (prefix, rest)
          | prefix < 0x80 -> Right (RlpBytes $ BS.singleton prefix, rest)
          | prefix <= 0xb7 -> payload depth False (fromIntegral $ prefix - 0x80) rest
          | prefix <= 0xbf -> long depth False (fromIntegral $ prefix - 0xb7) rest
          | prefix <= 0xf7 -> payload depth True (fromIntegral $ prefix - 0xc0) rest
          | otherwise -> long depth True (fromIntegral $ prefix - 0xf7) rest
  long depth list count input
    | BS.length input < count = Left "Truncated RLP length"
    | otherwise =
        let (size, rest) = BS.splitAt count input
            len = BS.foldl' (\n b -> n * 256 + fromIntegral b) (0 :: Integer) size
         in if len > fromIntegral (BS.length rest) then Left "Truncated RLP payload"
              else payload depth list (fromInteger len) rest
  payload depth list len input
    | len > BS.length input = Left "Truncated RLP payload"
    | otherwise = do
        let (body, rest) = BS.splitAt len input
        value <- if list then RlpList <$> items (depth + 1) body else Right $ RlpBytes body
        Right (value, rest)
  items depth input
    | BS.null input = Right []
    | otherwise = do
        (first, rest) <- item depth input
        (first :) <$> items depth rest

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
