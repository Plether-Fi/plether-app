module Plether.Ethereum.Contracts.TrancheVaultSpec (spec) where

import qualified Data.ByteString as BS
import Data.Text (Text)
import Plether.Ethereum.Abi (encodeUint256)
import Plether.Ethereum.Contracts.TrancheVault
  ( TrancheVaultSnapshot (..)
  , decodeTrancheVaultSnapshotResults
  , decodeVaultDecimalsResults
  , sharePriceWadFromConvertedAssets
  , trancheVaultSnapshotCalls
  , vaultSharePriceProbe
  )
import Plether.Ethereum.Multicall (Call (..), CallResult (..))
import Test.Hspec

spec :: Spec
spec = do
  describe "trancheVaultSnapshotCalls" $ do
    it "reads pool freshness plus assets, supply, and the conversion probe for both tranches" $ do
      let calls = trancheVaultSnapshotCalls housePool senior junior
      length calls `shouldBe` 7
      map callTarget calls `shouldBe` [housePool] <> replicate 3 senior <> replicate 3 junior
      map callAllowFailure calls `shouldBe` replicate 7 True
      BS.length (callCalldata $ calls !! 3) `shouldBe` 36
      BS.drop 4 (callCalldata $ calls !! 3) `shouldBe` encodeUint256 vaultSharePriceProbe

  describe "decodeTrancheVaultSnapshotResults" $ do
    it "decodes an exact coherent seven-result response" $ do
      let converted = 1_007_500_000_000_000_000_000_000
          results = poolView True : map success [400, 300, converted, 100, 80, converted]
      decodeTrancheVaultSnapshotResults results
        `shouldBe` Right
          ( True
          , TrancheVaultSnapshot 400 300 1_007_500_000_000_000_000
          , TrancheVaultSnapshot 100 80 1_007_500_000_000_000_000
          )

    it "rejects failed, truncated, empty, and extra subcalls" $ do
      let valid = poolView False : map success [1 .. 6]
      decodeTrancheVaultSnapshotResults (CallResult False BS.empty : tail valid)
        `shouldSatisfy` isLeft
      decodeTrancheVaultSnapshotResults (CallResult True BS.empty : tail valid)
        `shouldSatisfy` isLeft
      decodeTrancheVaultSnapshotResults (CallResult True (BS.replicate 31 0) : tail valid)
        `shouldSatisfy` isLeft
      decodeTrancheVaultSnapshotResults (valid <> [success 7])
        `shouldSatisfy` isLeft

    it "rejects non-canonical pool freshness data" $ do
      let invalidPool = CallResult True $ mconcat $ replicate 9 (encodeUint256 0)
            <> [encodeUint256 2]
            <> replicate 2 (encodeUint256 0)
      decodeTrancheVaultSnapshotResults (invalidPool : map success [1 .. 6])
        `shouldSatisfy` isLeft

  describe "sharePriceWadFromConvertedAssets" $ do
    it "normalizes 6-decimal USDC and 9-decimal shares to 18 decimals" $
      sharePriceWadFromConvertedAssets 1_007_500_000_000_000_000_000_000
        `shouldBe` 1_007_500_000_000_000_000

  describe "decodeVaultDecimalsResults" $ do
    it "accepts only the configured 6-decimal asset and 9-decimal shares" $ do
      decodeVaultDecimalsResults (map success [6, 9, 9]) `shouldBe` Right ()
      decodeVaultDecimalsResults (map success [18, 9, 9]) `shouldSatisfy` isLeft
      decodeVaultDecimalsResults (map success [6, 18, 18]) `shouldSatisfy` isLeft

    it "rejects malformed decimals responses" $ do
      decodeVaultDecimalsResults [success 6, success 9] `shouldSatisfy` isLeft
      decodeVaultDecimalsResults [success 6, CallResult True BS.empty, success 9]
        `shouldSatisfy` isLeft

senior :: Text
senior = "0x0000000000000000000000000000000000000001"

junior :: Text
junior = "0x0000000000000000000000000000000000000002"

housePool :: Text
housePool = "0x0000000000000000000000000000000000000003"

poolView :: Bool -> CallResult
poolView fresh =
  CallResult True $ mconcat $
    replicate 9 (encodeUint256 0)
      <> [encodeUint256 $ if fresh then 1 else 0]
      <> replicate 2 (encodeUint256 0)

success :: Integer -> CallResult
success = CallResult True . encodeUint256

isLeft :: Either a b -> Bool
isLeft value = case value of
  Left _ -> True
  Right _ -> False
