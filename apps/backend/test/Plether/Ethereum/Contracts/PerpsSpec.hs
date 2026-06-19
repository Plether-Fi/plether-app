module Plether.Ethereum.Contracts.PerpsSpec (spec) where

import qualified Data.ByteString as BS
import Plether.Ethereum.Abi (decodeUint256, encodeAddress, encodeUint256)
import Plether.Ethereum.Contracts.Perps
import Plether.Ethereum.Rpc (RpcLog (..))
import Test.Hspec

spec :: Spec
spec = do
  describe "bytes[] calldata encoding" $ do
    it "encodes getUpdateFee(bytes[]) with dynamic element offsets" $ do
      let call = getUpdateFeeCall [BS.pack [0x01, 0x02, 0x03], BS.pack [0x04]]
      word call 0 `shouldBe` 32
      word call 1 `shouldBe` 2
      word call 2 `shouldBe` 96
      word call 3 `shouldBe` 160
      word call 4 `shouldBe` 3
      BS.take 3 (wordBytes call 5) `shouldBe` BS.pack [0x01, 0x02, 0x03]
      word call 6 `shouldBe` 1
      BS.take 1 (wordBytes call 7) `shouldBe` BS.pack [0x04]

    it "encodes executeOrder(uint64,bytes[]) with the array at the second argument offset" $ do
      let call = executeOrderCall 7 [BS.pack [0x99]]
      word call 0 `shouldBe` 7
      word call 1 `shouldBe` 64
      word call 2 `shouldBe` 1
      word call 3 `shouldBe` 64
      word call 4 `shouldBe` 1
      BS.take 1 (wordBytes call 5) `shouldBe` BS.pack [0x99]

  describe "decodePerpsOrderEvent" $ do
    it "decodes OrderCommitted" $ do
      let logEntry =
            RpcLog
              { rpcLogTxHash = "0xabc"
              , rpcLogBlockNumber = 123
              , rpcLogAddress = "0xrouter"
              , rpcLogTopics =
                  [ orderCommittedTopic
                  , encodeUint256 42
                  , encodeAddress "0x1111111111111111111111111111111111111111"
                  ]
              , rpcLogData = encodeUint256 1
              }
      decodePerpsOrderEvent logEntry
        `shouldBe` Just
          OrderCommitted
            { poeOrderId = 42
            , poeAccount = "0x1111111111111111111111111111111111111111"
            , poeSide = 1
            , poeTxHash = "0xabc"
            , poeBlockNumber = 123
            }

    it "decodes OrderExecuted" $ do
      let logEntry =
            RpcLog
              { rpcLogTxHash = "0xdef"
              , rpcLogBlockNumber = 124
              , rpcLogAddress = "0xrouter"
              , rpcLogTopics = [orderExecutedTopic, encodeUint256 42]
              , rpcLogData = encodeUint256 123456
              }
      decodePerpsOrderEvent logEntry
        `shouldBe` Just
          OrderExecuted
            { poeOrderId = 42
            , poeExecutionPrice = 123456
            , poeTxHash = "0xdef"
            , poeBlockNumber = 124
            }

    it "decodes OrderFailed" $ do
      let logEntry =
            RpcLog
              { rpcLogTxHash = "0x456"
              , rpcLogBlockNumber = 125
              , rpcLogAddress = "0xrouter"
              , rpcLogTopics = [orderFailedTopic, encodeUint256 42]
              , rpcLogData = encodeUint256 3
              }
      decodePerpsOrderEvent logEntry
        `shouldBe` Just
          OrderFailed
            { poeOrderId = 42
            , poeFailureReason = 3
            , poeTxHash = "0x456"
            , poeBlockNumber = 125
            }

word :: BS.ByteString -> Int -> Integer
word call index = decodeUint256 $ wordBytes call index

wordBytes :: BS.ByteString -> Int -> BS.ByteString
wordBytes call index =
  BS.take 32 $ BS.drop (4 + index * 32) call
