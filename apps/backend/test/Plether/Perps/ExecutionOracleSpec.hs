module Plether.Perps.ExecutionOracleSpec (spec) where

import qualified Data.ByteString as BS
import Data.Either (isLeft)
import Plether.Ethereum.Abi (encodeUint256)
import Plether.Ethereum.Contracts.Perps
  ( executeOrderBatchCall
  , executeOrderCall
  )
import Plether.Perps.ExecutionOracle
import Plether.Pyth.Basket
  ( BasketComponent (..)
  , PythPricePoint (..)
  , basketComponents
  , computeBasketSnapshot
  )
import Test.Hspec

spec :: Spec
spec = do
  describe "decodeExecutionUpdateData" $ do
    it "returns the exact executeOrder bytes[] for the bound order" $ do
      let updateData =
            [ BS.pack [0x00, 0xff]
            , BS.empty
            , BS.pack [0 .. 32]
            ]
      decodeExecutionUpdateData 42 (executeOrderCall 42 updateData)
        `shouldBe` Right updateData

    it "accepts an executeOrderBatch call only when it covers the order" $ do
      let updateData = [BS.pack [0x01, 0x02, 0x03]]
          call = executeOrderBatchCall 50 updateData
      decodeExecutionUpdateData 42 call `shouldBe` Right updateData
      decodeExecutionUpdateData 51 call `shouldSatisfy` isLeft

    it "requires executeOrder to bind the order ID exactly" $ do
      decodeExecutionUpdateData 42 (executeOrderCall 43 [BS.singleton 0x01])
        `shouldSatisfy` isLeft

    it "decodes a canonical empty bytes[] but rejects empty calldata" $ do
      decodeExecutionUpdateData 42 (executeOrderCall 42 [])
        `shouldBe` Right []
      decodeExecutionUpdateData 42 BS.empty `shouldSatisfy` isLeft

    it "rejects truncated and trailing calldata" $ do
      let call = executeOrderCall 42 [BS.pack [0x01, 0x02, 0x03]]
      decodeExecutionUpdateData 42 (BS.init call) `shouldSatisfy` isLeft
      decodeExecutionUpdateData 42 (call <> BS.singleton 0x00)
        `shouldSatisfy` isLeft

    it "rejects non-canonical top-level and element offsets" $ do
      let call = executeOrderCall 42 [BS.singleton 0x01]
          wrongArrayOffset = replaceArgumentWord 1 (encodeUint256 96) call
          wrongElementOffset = replaceArgumentWord 3 (encodeUint256 64) call
      decodeExecutionUpdateData 42 wrongArrayOffset `shouldSatisfy` isLeft
      decodeExecutionUpdateData 42 wrongElementOffset `shouldSatisfy` isLeft

    it "rejects nonzero dynamic-bytes padding" $ do
      let call = executeOrderCall 42 [BS.singleton 0x01]
          nonzeroPadding = BS.init call <> BS.singleton 0x01
      decodeExecutionUpdateData 42 nonzeroPadding `shouldSatisfy` isLeft

    it "rejects non-uint64 calldata and expected order IDs" $ do
      let oversizedOrderId = 2 ^ (64 :: Integer)
          call = executeOrderCall oversizedOrderId [BS.singleton 0x01]
      decodeExecutionUpdateData 42 call `shouldSatisfy` isLeft
      decodeExecutionUpdateData (-1) (executeOrderCall 0 [])
        `shouldSatisfy` isLeft
      decodeExecutionUpdateData oversizedOrderId call
        `shouldSatisfy` isLeft

  describe "deriveExecutionOracleSnapshot" $ do
    it "uses the neutral basket midpoint and component publish-time bounds" $ do
      let pricePoints = neutralPricePoints [105, 101, 104, 100, 103, 102]
      (expectedMidpoint, _) <- expectRight $ computeBasketSnapshot pricePoints
      deriveExecutionOracleSnapshot pricePoints
        `shouldBe` Right
          ExecutionOracleSnapshot
            { eosMidpointPrice = expectedMidpoint
            , eosMinPublishTime = 100
            , eosMaxPublishTime = 105
            }

    it "derives bounds only from basket components" $ do
      let extraPoint =
            PythPricePoint
              { pppFeedId = "0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
              , pppPrice = 100_000_000
              , pppConfidence = 1
              , pppExponent = -8
              , pppPublishTime = 1
              }
          pricePoints = neutralPricePoints [205, 201, 204, 200, 203, 202]
      snapshot <- expectRight $ deriveExecutionOracleSnapshot (extraPoint : pricePoints)
      eosMinPublishTime snapshot `shouldBe` 200
      eosMaxPublishTime snapshot `shouldBe` 205

    it "rejects missing or empty basket price points" $ do
      deriveExecutionOracleSnapshot [] `shouldSatisfy` isLeft
      deriveExecutionOracleSnapshot (drop 1 $ neutralPricePoints [1 .. 6])
        `shouldSatisfy` isLeft

  describe "executionOraclePublishTimeBounds" $ do
    it "selects the first unique Pyth observation strictly after commit" $
      executionOraclePublishTimeBounds 1_785_437_833
        `shouldBe` Right (1_785_437_834, 18_446_744_073_709_551_615)

    it "rejects timestamps that cannot be represented as a post-commit uint64" $ do
      executionOraclePublishTimeBounds (-1) `shouldSatisfy` isLeft
      executionOraclePublishTimeBounds 18_446_744_073_709_551_615
        `shouldSatisfy` isLeft

replaceArgumentWord
  :: Int
  -> BS.ByteString
  -> BS.ByteString
  -> BS.ByteString
replaceArgumentWord wordIndex replacement calldata =
  let offset = 4 + wordIndex * 32
   in BS.take offset calldata
        <> replacement
        <> BS.drop (offset + 32) calldata

neutralPricePoints :: [Integer] -> [PythPricePoint]
neutralPricePoints publishTimes =
  zipWith point basketComponents publishTimes
  where
    point component publishTime =
      PythPricePoint
        { pppFeedId = bcFeedId component
        , pppPrice = 100_000_000
        , pppConfidence = 1
        , pppExponent = -8
        , pppPublishTime = publishTime
        }

expectRight :: Show a => Either a b -> IO b
expectRight value =
  case value of
    Left err ->
      expectationFailure ("unexpected Left: " <> show err)
        >> fail "unreachable"
    Right result -> pure result
