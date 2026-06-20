module Plether.KeeperSpec (spec) where

import Plether.Database.Schema (PerpsKeeperOrderRow (..))
import Plether.Keeper (isOrderExpired, isOrderRevealReady, selectBatchCandidates)
import Test.Hspec

spec :: Spec
spec = do
  describe "isOrderExpired" $ do
    it "does not expire at the exact max age boundary" $ do
      isOrderExpired 110 10 (order 1 100) `shouldBe` False

    it "expires after max age" $ do
      isOrderExpired 111 10 (order 1 100) `shouldBe` True

  describe "isOrderRevealReady" $ do
    it "accepts publish times inside the order reveal window" $ do
      isOrderRevealReady 15 [101, 102, 103, 104, 105, 106] (order 1 100)
        `shouldBe` True

    it "rejects publish times before the order reveal window" $ do
      isOrderRevealReady 15 [101, 102, 103, 104, 105, 106] (order 1 101)
        `shouldBe` False

  describe "selectBatchCandidates" $ do
    it "takes contiguous ready orders sharing the same payload" $ do
      let selected =
            selectBatchCandidates
              110
              11
              50
              15
              [101, 102, 103, 104, 105, 106]
              20
              [order 1 100, order 2 99, order 3 101]
      map pkorOrderId selected `shouldBe` [1, 2]

    it "includes expired terminal orders in a contiguous batch" $ do
      let selected =
            selectBatchCandidates
              120
              11
              10
              15
              [101, 102, 103, 104, 105, 106]
              20
              [order 1 100, order 2 99]
      map pkorOrderId selected `shouldBe` [1, 2]

    it "stops at the same-block guard" $ do
      let selected =
            selectBatchCandidates
              110
              10
              50
              15
              [101, 102, 103, 104, 105, 106]
              20
              [order 1 100]
      map pkorOrderId selected `shouldBe` []

    it "honors the max batch size" $ do
      let selected =
            selectBatchCandidates
              110
              11
              50
              15
              [101, 102, 103, 104, 105, 106]
              1
              [order 1 100, order 2 99]
      map pkorOrderId selected `shouldBe` [1]

order :: Integer -> Integer -> PerpsKeeperOrderRow
order orderId commitTime =
  PerpsKeeperOrderRow
    { pkorOrderId = orderId
    , pkorAccount = "0x1111111111111111111111111111111111111111"
    , pkorSide = 0
    , pkorCommitBlock = 10
    , pkorCommitTime = commitTime
    , pkorCommitTxHash = "0xcommit"
    , pkorStatus = "pending"
    , pkorAttemptCount = 0
    , pkorLastError = Nothing
    }
