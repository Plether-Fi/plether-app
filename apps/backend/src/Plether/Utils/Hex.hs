module Plether.Utils.Hex
  ( hexToInteger
  , intToHex
  , hexToByteString
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as B16
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

hexToInteger :: Text -> Integer
hexToInteger = T.foldl' (\acc c -> acc * 16 + fromIntegral (hexDigit c)) 0
  where
    hexDigit c
      | c >= '0' && c <= '9' = fromEnum c - fromEnum '0'
      | c >= 'a' && c <= 'f' = fromEnum c - fromEnum 'a' + 10
      | c >= 'A' && c <= 'F' = fromEnum c - fromEnum 'A' + 10
      | otherwise = 0

intToHex :: Integer -> Text
intToHex n = T.pack $ go n ""
  where
    go 0 s = if null s then "0" else s
    go x s = go (x `div` 16) (hexChar (x `mod` 16) : s)
    hexChar d
      | d < 10 = toEnum (fromEnum '0' + fromIntegral d)
      | otherwise = toEnum (fromEnum 'a' + fromIntegral d - 10)

hexToByteString :: Text -> ByteString
hexToByteString txt =
  let stripped = if T.isPrefixOf "0x" txt || T.isPrefixOf "0X" txt then T.drop 2 txt else txt
   in case B16.decode (TE.encodeUtf8 $ T.toLower stripped) of
        Right bs -> bs
        Left _ -> ""
