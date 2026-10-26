module Plether.Perps.ManifestSpec (spec) where

import Data.Aeson (Value (..), object, (.=))
import Data.Either (isLeft)
import Plether.Perps.Manifest.Embed (settlementTokenField)
import qualified Plether.Perps.Manifest as Manifest
import Test.Hspec

spec :: Spec
spec = describe "settlement token release binding" $ do
  let token address = object ["address" .= (address :: String), "runtimeCodeHash" .= ("0xhash" :: String)]
      legacy = object ["schemaVersion" .= (2 :: Int), "contracts" .= object ["mockUsdc" .= token "0xlegacy"]]
      current = object ["schemaVersion" .= (4 :: Int), "contracts" .= object ["settlementToken" .= token "0xusdg"]]
  it "reads both historical USDC and canonical settlement token entries" $ do
    settlementTokenField legacy "address" `shouldBe` Right (String "0xlegacy")
    settlementTokenField current "address" `shouldBe` Right (String "0xusdg")
    settlementTokenField current "runtimeCodeHash" `shouldBe` Right (String "0xhash")
  it "does not hide an incomplete canonical token behind a legacy entry" $ do
    let malformed = object ["schemaVersion" .= (4 :: Int), "contracts" .= object ["settlementToken" .= Null, "mockUsdc" .= token "0xlegacy"]]
        absent = object ["schemaVersion" .= (4 :: Int), "contracts" .= object ["mockUsdc" .= token "0xlegacy"]]
        prepared = object ["schemaVersion" .= (4 :: Int), "contracts" .= object ["settlementToken" .= object ["address" .= Null]]]
    map (`settlementTokenField` "address") [malformed, absent, prepared] `shouldSatisfy` all isLeft
  it "rejects historical manifests relabeled with canonical metadata or entries" $ do
    let relabeled = object ["schemaVersion" .= (2 :: Int), "contracts" .= object ["mockUsdc" .= token "0xlegacy", "settlementToken" .= token "0xusdg"]]
        metadata = object ["schemaVersion" .= (2 :: Int), "contracts" .= object ["mockUsdc" .= token "0xlegacy"], "settlementToken" .= object ["symbol" .= ("USDG" :: String)]]
    map (`settlementTokenField` "address") [relabeled, metadata] `shouldSatisfy` all isLeft
  it "keeps the historical compatibility aliases bound to the same asset" $ do
    Manifest.mockUsdcAddress `shouldBe` Manifest.settlementTokenAddress
    Manifest.mockUsdcCodeHash `shouldBe` Manifest.settlementTokenCodeHash
