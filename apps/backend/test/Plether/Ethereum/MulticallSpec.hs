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
