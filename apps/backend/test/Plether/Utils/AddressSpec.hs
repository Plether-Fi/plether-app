module Plether.Utils.AddressSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Plether.Utils.Address

spec :: Spec
spec = do
  describe "isValidAddress" $ do
    it "accepts valid checksummed address" $
      isValidAddress "0x5aAeb6053F3E94C9b9A09f33669435E7Ef1BeAed" `shouldBe` True

    it "accepts valid lowercase address" $
      isValidAddress "0x5aaeb6053f3e94c9b9a09f33669435e7ef1beaed" `shouldBe` True

    it "accepts valid uppercase address" $
      isValidAddress "0x5AAEB6053F3E94C9B9A09F33669435E7EF1BEAED" `shouldBe` True

    it "accepts zero address" $
      isValidAddress "0x0000000000000000000000000000000000000000" `shouldBe` True

    it "accepts address without 0x prefix" $
      isValidAddress "5aaeb6053f3e94c9b9a09f33669435e7ef1beaed" `shouldBe` True

    it "rejects empty string" $
      isValidAddress "" `shouldBe` False

    it "rejects too short address" $
      isValidAddress "0x5aaeb6053f3e94c9b9a09f33669435e7ef1beae" `shouldBe` False

    it "rejects too long address" $
      isValidAddress "0x5aaeb6053f3e94c9b9a09f33669435e7ef1beaed0" `shouldBe` False

    it "rejects invalid hex characters" $
      isValidAddress "0x5aaeb6053f3e94c9b9a09f33669435e7ef1beaeg" `shouldBe` False

    it "rejects non-hex string" $
      isValidAddress "0xhello_world_this_is_not_valid_hex_" `shouldBe` False

    prop "valid addresses validate" prop_validAddressValidates

    prop "case insensitive" prop_caseInsensitive

  describe "mkAddress" $ do
    it "creates Address from valid hex" $
      mkAddress "0x5aaeb6053f3e94c9b9a09f33669435e7ef1beaed" `shouldSatisfy` isSome

    it "creates Address without 0x prefix" $
      mkAddress "5aaeb6053f3e94c9b9a09f33669435e7ef1beaed" `shouldSatisfy` isSome

    it "returns Nothing for invalid address" $
      mkAddress "invalid" `shouldBe` Nothing

    it "returns Nothing for too short" $
      mkAddress "0x1234" `shouldBe` Nothing

  describe "toHex" $ do
    it "returns address with 0x prefix" $ do
      let Just addr = mkAddress "0x5aaeb6053f3e94c9b9a09f33669435e7ef1beaed"
      T.take 2 (toHex addr) `shouldBe` "0x"

    it "returns 42 character string" $ do
      let Just addr = mkAddress "0x5aaeb6053f3e94c9b9a09f33669435e7ef1beaed"
      T.length (toHex addr) `shouldBe` 42

    it "returns lowercase hex" $ do
      let Just addr = mkAddress "0x5AAEB6053F3E94C9B9A09F33669435E7EF1BEAED"
      toHex addr `shouldBe` "0x5aaeb6053f3e94c9b9a09f33669435e7ef1beaed"

  describe "fromHex" $ do
    it "is alias for mkAddress" $ do
      fromHex "0x5aaeb6053f3e94c9b9a09f33669435e7ef1beaed"
        `shouldBe` mkAddress "0x5aaeb6053f3e94c9b9a09f33669435e7ef1beaed"

  describe "zeroAddress" $ do
    it "is all zeros" $
      toHex zeroAddress `shouldBe` "0x0000000000000000000000000000000000000000"

  describe "roundtrip" $ do
    prop "mkAddress -> toHex preserves value" prop_mkAddressToHexRoundtrip

    prop "valid addresses roundtrip" prop_addressRoundtrip

isSome :: Maybe a -> Bool
isSome (Just _) = True
isSome Nothing = False

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

prop_validAddressValidates :: ValidAddress -> Bool
prop_validAddressValidates (ValidAddress addr) = isValidAddress addr

prop_caseInsensitive :: ValidAddress -> Bool
prop_caseInsensitive (ValidAddress addr) =
  isValidAddress (T.toLower addr) == isValidAddress (T.toUpper addr)

prop_mkAddressToHexRoundtrip :: ValidAddress -> Bool
prop_mkAddressToHexRoundtrip (ValidAddress addr) =
  case mkAddress addr of
    Just a -> T.toLower (toHex a) == T.toLower addr
    Nothing -> False

prop_addressRoundtrip :: ValidAddress -> Bool
prop_addressRoundtrip (ValidAddress addr) =
  case mkAddress addr of
    Just a -> mkAddress (toHex a) == Just a
    Nothing -> False
