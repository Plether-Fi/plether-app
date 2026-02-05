module Plether.Ethereum.AbiSpec (spec) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Plether.Ethereum.Abi

spec :: Spec
spec = do
  describe "selector" $ do
    it "computes transfer(address,uint256) selector" $
      toHex (selector "transfer(address,uint256)") `shouldBe` "a9059cbb"

    it "computes balanceOf(address) selector" $
      toHex (selector "balanceOf(address)") `shouldBe` "70a08231"

    it "computes approve(address,uint256) selector" $
      toHex (selector "approve(address,uint256)") `shouldBe` "095ea7b3"

    it "computes allowance(address,address) selector" $
      toHex (selector "allowance(address,address)") `shouldBe` "dd62ed3e"

    it "computes mint(uint256) selector" $
      toHex (selector "mint(uint256)") `shouldBe` "a0712d68"

    it "selector is always 4 bytes" $
      BS.length (selector "anyFunction(uint256,address,bool)") `shouldBe` 4

  describe "encodeUint256" $ do
    it "encodes 0 as 32 zero bytes" $
      encodeUint256 0 `shouldBe` BS.replicate 32 0

    it "encodes 1 correctly" $
      encodeUint256 1 `shouldBe` BS.replicate 31 0 <> BS.singleton 1

    it "encodes 256 correctly" $
      encodeUint256 256 `shouldBe` BS.replicate 30 0 <> BS.pack [1, 0]

    it "encodes max uint256" $
      encodeUint256 (2 ^ (256 :: Integer) - 1) `shouldBe` BS.replicate 32 0xff

    it "is always 32 bytes" $
      BS.length (encodeUint256 12345678901234567890) `shouldBe` 32

    prop "encoded value is always 32 bytes" prop_uint256Size

    prop "roundtrip through encode/decode" prop_uint256Roundtrip

  describe "decodeUint256" $ do
    it "decodes 32 zero bytes as 0" $
      decodeUint256 (BS.replicate 32 0) `shouldBe` 0

    it "decodes max uint256" $
      decodeUint256 (BS.replicate 32 0xff) `shouldBe` 2 ^ (256 :: Integer) - 1

    it "handles padded input" $
      decodeUint256 (BS.replicate 31 0 <> BS.singleton 42) `shouldBe` 42

  describe "decodeInt256" $ do
    it "decodes positive numbers" $
      decodeInt256 (encodeUint256 42) `shouldBe` 42

    it "decodes -1 (all 0xff)" $
      decodeInt256 (BS.replicate 32 0xff) `shouldBe` -1

    it "decodes min int256" $
      let minInt256 = BS.singleton 0x80 <> BS.replicate 31 0
      in decodeInt256 minInt256 `shouldBe` -(2 ^ (255 :: Integer))

    it "decodes max int256" $
      let maxInt256 = BS.singleton 0x7f <> BS.replicate 31 0xff
      in decodeInt256 maxInt256 `shouldBe` 2 ^ (255 :: Integer) - 1

  describe "encodeAddress" $ do
    it "encodes zero address" $
      encodeAddress "0x0000000000000000000000000000000000000000"
        `shouldBe` BS.replicate 32 0

    it "encodes address with left padding" $
      let addr = "0xdead000000000000000000000000000000000000"
          encoded = encodeAddress addr
      in BS.take 12 encoded `shouldBe` BS.replicate 12 0

    it "is always 32 bytes" $
      BS.length (encodeAddress "0x1234567890123456789012345678901234567890")
        `shouldBe` 32

    it "handles address without 0x prefix" $
      encodeAddress "dead000000000000000000000000000000000000"
        `shouldBe` encodeAddress "0xdead000000000000000000000000000000000000"

  describe "decodeAddress" $ do
    it "decodes zero address" $
      decodeAddress (BS.replicate 32 0)
        `shouldBe` "0x0000000000000000000000000000000000000000"

    it "decodes address with correct length" $
      let addr = decodeAddress (encodeAddress "0xdead000000000000000000000000000000000000")
      in T.length addr `shouldBe` 42

    prop "roundtrip encode/decode" prop_addressRoundtrip

  describe "encodeBool" $ do
    it "encodes True as 1" $
      encodeBool True `shouldBe` encodeUint256 1

    it "encodes False as 0" $
      encodeBool False `shouldBe` encodeUint256 0

  describe "decodeBool" $ do
    it "decodes 0 as False" $
      decodeBool (encodeUint256 0) `shouldBe` False

    it "decodes 1 as True" $
      decodeBool (encodeUint256 1) `shouldBe` True

    it "decodes any non-zero as True" $
      decodeBool (encodeUint256 42) `shouldBe` True

    prop "roundtrip encode/decode" prop_boolRoundtrip

  describe "encodeBytes32" $ do
    it "pads short input" $
      BS.length (encodeBytes32 "hello") `shouldBe` 32

    it "truncates long input" $
      BS.length (encodeBytes32 (BS.replicate 64 0)) `shouldBe` 32

    it "preserves 32-byte input" $
      encodeBytes32 (BS.replicate 32 0xab) `shouldBe` BS.replicate 32 0xab

  describe "encodeCall" $ do
    it "prepends selector to args" $
      let call = encodeCall "transfer(address,uint256)"
            [ encodeAddress "0x1234567890123456789012345678901234567890"
            , encodeUint256 1000
            ]
      in BS.length call `shouldBe` 4 + 32 + 32

    it "matches expected transfer calldata" $
      let call = encodeCall "transfer(address,uint256)"
            [ encodeAddress "0x0000000000000000000000000000000000000001"
            , encodeUint256 1
            ]
          expected = selector "transfer(address,uint256)"
            <> encodeAddress "0x0000000000000000000000000000000000000001"
            <> encodeUint256 1
      in call `shouldBe` expected

  describe "decodeMultiple" $ do
    it "decodes multiple uint256 values" $
      let encoded = encodeUint256 100 <> encodeUint256 200 <> encodeUint256 300
          [a, b, c] = decodeMultiple [decodeUint256, decodeUint256, decodeUint256] encoded
      in (a, b, c) `shouldBe` (100, 200, 300)

toHex :: ByteString -> Text
toHex = TE.decodeUtf8 . B16.encode

newtype ValidUint256 = ValidUint256 Integer
  deriving (Show)

instance Arbitrary ValidUint256 where
  arbitrary = ValidUint256 <$> choose (0, 2 ^ (256 :: Integer) - 1)

prop_uint256Size :: ValidUint256 -> Bool
prop_uint256Size (ValidUint256 n) = BS.length (encodeUint256 n) == 32

prop_uint256Roundtrip :: ValidUint256 -> Bool
prop_uint256Roundtrip (ValidUint256 n) = decodeUint256 (encodeUint256 n) == n

newtype ValidAddress = ValidAddress Text
  deriving (Show)

instance Arbitrary ValidAddress where
  arbitrary = do
    bytes <- vectorOf 20 (choose (0, 255) :: Gen Int)
    let hex = concatMap (padLeft . toHexStr) bytes
    pure $ ValidAddress $ T.pack $ "0x" ++ hex
    where
      toHexStr n = showHex' n ""
      showHex' n s
        | n < 16 = toHexChar n : s
        | otherwise = showHex' (n `div` 16) (toHexChar (n `mod` 16) : s)
      toHexChar n
        | n < 10 = toEnum (fromEnum '0' + n)
        | otherwise = toEnum (fromEnum 'a' + n - 10)
      padLeft [c] = ['0', c]
      padLeft cs = cs

prop_addressRoundtrip :: ValidAddress -> Bool
prop_addressRoundtrip (ValidAddress addr) =
  T.toLower (decodeAddress (encodeAddress addr)) == T.toLower addr

prop_boolRoundtrip :: Bool -> Bool
prop_boolRoundtrip b = decodeBool (encodeBool b) == b
