module Plether.Ethereum.MulticallSpec (spec) where

import qualified Data.ByteString as BS
import Test.Hspec

import Plether.Ethereum.Abi (encodeUint256)
import Plether.Ethereum.Multicall

spec :: Spec
spec = do
  describe "decodeResults" $ do
    it "decodes returnData for aggregate3 results" $ do
      let balance = 50000000000
          result =
            encodeUint256 32
              <> encodeUint256 1
              <> encodeUint256 32
              <> encodeUint256 1
              <> encodeUint256 64
              <> encodeUint256 32
              <> encodeUint256 balance

      decodeResults result
        `shouldBe` [CallResult True (encodeUint256 balance)]

    it "decodes empty returnData" $ do
      let result =
            encodeUint256 32
              <> encodeUint256 1
              <> encodeUint256 32
              <> encodeUint256 1
              <> encodeUint256 64
              <> encodeUint256 0

      decodeResults result
        `shouldBe` [CallResult True BS.empty]

    it "strictly rejects truncated aggregate3 responses" $ do
      let result =
            encodeUint256 32
              <> encodeUint256 1
              <> encodeUint256 32
              <> encodeUint256 1
              <> encodeUint256 64
              <> encodeUint256 32
              <> encodeUint256 50000000000

      decodeResultsEither (BS.init result)
        `shouldSatisfy` isLeft

    it "strictly rejects non-boolean success words" $ do
      let result =
            encodeUint256 32
              <> encodeUint256 1
              <> encodeUint256 32
              <> encodeUint256 2
              <> encodeUint256 64
              <> encodeUint256 0

      decodeResultsEither result
        `shouldBe` Left "result 0 has a non-boolean success value"

    it "strictly rejects duplicate result offsets" $ do
      let result =
            encodeUint256 32
              <> encodeUint256 2
              <> encodeUint256 64
              <> encodeUint256 64
              <> encodeUint256 1
              <> encodeUint256 64
              <> encodeUint256 32
              <> encodeUint256 50000000000

      decodeResultsEither result
        `shouldBe` Left "result offsets must be strictly increasing"

    it "strictly rejects overlapping result tails" $ do
      let result =
            encodeUint256 32
              <> encodeUint256 2
              <> encodeUint256 64
              <> encodeUint256 192
              <> encodeUint256 1
              <> encodeUint256 160
              <> encodeUint256 0
              <> encodeUint256 0
              <> encodeUint256 1
              <> encodeUint256 64
              <> encodeUint256 0
              <> encodeUint256 0

      decodeResultsEither result
        `shouldBe` Left "result 0 overlaps the next result"

isLeft :: Either a b -> Bool
isLeft value = case value of
  Left _ -> True
  Right _ -> False
