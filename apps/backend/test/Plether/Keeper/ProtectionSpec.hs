module Plether.Keeper.ProtectionSpec (spec) where

import qualified Data.ByteString as BS
import Data.Either (isLeft)
import Plether.Ethereum.Abi (encodeUint256, encodeAddress)
import Plether.Ethereum.Rpc (RpcLog (..))
import Plether.Keeper.Protection
import Test.Hspec

spec :: Spec
spec = do
  describe "latched protection retry admission" $ do
    let protection = Protection 7 11 "0x0000000000000000000000000000000000000001" 1 100 200000 999 8
        assess p reason disposition side size pending tailId age delay ready =
          assessRetry p reason disposition side size pending tailId age delay ready
        eligible = assess protection 2 4 1 100 0 0 60
    it "admits an expired retained attempt at the 45-second boundary" $
      eligible 45 True `shouldBe` Right ()
    it "refuses a late arrival, unavailable oracle or occupied FIFO" $ do
      eligible 46 True `shouldSatisfy` isLeft
      eligible 5 False `shouldSatisfy` isLeft
      assess protection 2 4 1 100 0 12 60 5 True `shouldSatisfy` isLeft
    it "does not hot-loop deterministic failure reasons" $
      mapM_ (\reason -> assess protection reason 4 1 100 0 0 60 5 True `shouldSatisfy` isLeft) [0, 1, 3, 4, 5, 6, 7, 8, 9]
    it "requires the exact position, active latch, no pending order and retained bounty" $ do
      assess protection 2 1 1 100 0 0 60 5 True `shouldSatisfy` isLeft
      assess protection 2 4 0 100 0 0 60 5 True `shouldSatisfy` isLeft
      assess protection 2 4 1 99 0 0 60 5 True `shouldSatisfy` isLeft
      assess protection 2 4 1 100 1 0 60 5 True `shouldSatisfy` isLeft
      assess protection {protectionStatus = 3} 2 4 1 100 0 0 60 5 True `shouldSatisfy` isLeft
      assess protection {retainedBounty = 0} 2 4 1 100 0 0 60 5 True `shouldSatisfy` isLeft
    it "requires all six fresh, synchronized feeds and nonempty payloads" $ do
      retryOracleReady 1000 10 2 (replicate 6 999) [BS.singleton 1] `shouldBe` True
      retryOracleReady 1000 10 2 (replicate 5 999) [BS.singleton 1] `shouldBe` False
      retryOracleReady 1000 10 2 (replicate 6 989) [BS.singleton 1] `shouldBe` False
      retryOracleReady 1000 10 2 (replicate 6 1001) [BS.singleton 1] `shouldBe` False
      retryOracleReady 1000 10 2 (995 : replicate 5 999) [BS.singleton 1] `shouldBe` False
      retryOracleReady 1000 10 2 (replicate 6 999) [] `shouldBe` False
  describe "protection bindings" $ do
    it "decodes latched views without changing their tuple shape" $ do
      let encoded = mconcat [encodeUint256 (case n of 0 -> 7; 2 -> 11; 4 -> 1; 5 -> 100; 9 -> 200000; 15 -> 8; _ -> 0) | n <- [0 .. 15 :: Int]]
      fmap protectionStatus (decodeProtection encoded) `shouldBe` Right 8
      decodeProtection (BS.take 480 encoded) `shouldSatisfy` isLeft
      decodeProtection (BS.take 480 encoded <> encodeUint256 9) `shouldSatisfy` isLeft
    it "decodes successive attempt links, failure-to-latch and permanent registration" $ do
      let account = "0x0000000000000000000000000000000000000001"
          logEntry topics bytes = RpcLog "0xtx" 10 "0xblock" 0 1 account topics bytes
          indexed = [encodeUint256 7, encodeAddress account, encodeUint256 19]
      decodeProtectionEvent (logEntry ((protectionTopics !! 0) : indexed) (encodeUint256 11))
        `shouldBe` Just (AttemptQueued 7 account 19 11)
      decodeProtectionEvent (logEntry ((protectionTopics !! 1) : indexed) (encodeUint256 2 <> encodeUint256 1))
        `shouldBe` Just (AttemptFailed 7 account 19 2 True)
      decodeProtectionEvent (logEntry [protectionTopics !! 2, encodeUint256 19] BS.empty)
        `shouldBe` Just (AttemptRegistered 19)
      decodeProtectionEvent (logEntry ((protectionTopics !! 1) : indexed) (encodeUint256 2 <> encodeUint256 2))
        `shouldBe` Nothing
