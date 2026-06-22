module Plether.Ethereum.Contracts.PerpsSpec (spec) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Plether.Ethereum.Abi (decodeUint256, encodeAddress, encodeUint256)
import Plether.Ethereum.Contracts.Perps
import Plether.Ethereum.Rpc (RpcLog (..))
import Test.Hspec

spec :: Spec
spec = do
  describe "bytes[] calldata encoding" $ do
    it "encodes getUpdateFee(bytes[]) with an empty array" $ do
      getUpdateFeeCall []
        `shouldEncodeTo` "0xd47eed4500000000000000000000000000000000000000000000000000000000000000200000000000000000000000000000000000000000000000000000000000000000"

    it "encodes getUpdateFee(bytes[]) with one dynamic bytes value" $ do
      getUpdateFeeCall [BS.pack [0x01, 0x02, 0x03]]
        `shouldEncodeTo` "0xd47eed4500000000000000000000000000000000000000000000000000000000000000200000000000000000000000000000000000000000000000000000000000000001000000000000000000000000000000000000000000000000000000000000002000000000000000000000000000000000000000000000000000000000000000030102030000000000000000000000000000000000000000000000000000000000"

    it "encodes getUpdateFee(bytes[]) with two dynamic bytes values" $ do
      getUpdateFeeCall [BS.pack [0x01, 0x02, 0x03], BS.pack [0x04]]
        `shouldEncodeTo` "0xd47eed4500000000000000000000000000000000000000000000000000000000000000200000000000000000000000000000000000000000000000000000000000000002000000000000000000000000000000000000000000000000000000000000004000000000000000000000000000000000000000000000000000000000000000800000000000000000000000000000000000000000000000000000000000000003010203000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000010400000000000000000000000000000000000000000000000000000000000000"

    it "encodes executeOrder(uint64,bytes[]) with an empty array" $ do
      executeOrderCall 7 []
        `shouldEncodeTo` "0xc700abdc000000000000000000000000000000000000000000000000000000000000000700000000000000000000000000000000000000000000000000000000000000400000000000000000000000000000000000000000000000000000000000000000"

    it "encodes executeOrder(uint64,bytes[]) with one dynamic bytes value" $ do
      executeOrderCall 7 [BS.pack [0x99]]
        `shouldEncodeTo` "0xc700abdc000000000000000000000000000000000000000000000000000000000000000700000000000000000000000000000000000000000000000000000000000000400000000000000000000000000000000000000000000000000000000000000001000000000000000000000000000000000000000000000000000000000000002000000000000000000000000000000000000000000000000000000000000000019900000000000000000000000000000000000000000000000000000000000000"

    it "encodes executeOrderBatch(uint64,bytes[]) with one dynamic bytes value" $ do
      executeOrderBatchCall 7 [BS.pack [0x99]]
        `shouldEncodeTo` "0x8c3679bc000000000000000000000000000000000000000000000000000000000000000700000000000000000000000000000000000000000000000000000000000000400000000000000000000000000000000000000000000000000000000000000001000000000000000000000000000000000000000000000000000000000000002000000000000000000000000000000000000000000000000000000000000000019900000000000000000000000000000000000000000000000000000000000000"

    it "keeps an order-56-style reveal payload on the canonical bytes[] layout" $ do
      payload <-
        parseHex
          "0x504e41550100000003b801000000060d0078f076887d59cbc6abb6eab08ce271b1e2a8"
      expectedSelector <- parseHex "0xc700abdc"
      let call = executeOrderCall 56 [payload]
      BS.take 4 call `shouldBe` expectedSelector
      word call 0 `shouldBe` 56
      word call 1 `shouldBe` 64
      word call 2 `shouldBe` 1
      word call 3 `shouldBe` 32
      word call 4 `shouldBe` fromIntegral (BS.length payload)
      BS.take (BS.length payload) (bytesAtWord call 5) `shouldBe` payload

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

  describe "orderFailureReasonText" $ do
    it "decodes current router failure reason enum values" $ do
      map orderFailureReasonText [0 .. 5]
        `shouldBe` [ "Expired"
                   , "CloseOnly"
                   , "SlippageExceeded"
                   , "EnginePanic"
                   , "AccountLiquidated"
                   , "EngineRevert"
                   ]

    it "labels unknown future values without failing" $ do
      orderFailureReasonText 99 `shouldBe` "Unknown(99)"

word :: BS.ByteString -> Int -> Integer
word call index = decodeUint256 $ wordBytes call index

wordBytes :: BS.ByteString -> Int -> BS.ByteString
wordBytes call index =
  BS.take 32 $ BS.drop (4 + index * 32) call

bytesAtWord :: BS.ByteString -> Int -> BS.ByteString
bytesAtWord call index =
  BS.drop (4 + index * 32) call

shouldEncodeTo :: BS.ByteString -> BS.ByteString -> Expectation
shouldEncodeTo actual expectedHex = do
  expected <- parseHex expectedHex
  actual `shouldBe` expected

parseHex :: BS.ByteString -> IO BS.ByteString
parseHex value =
  case B16.decode $ strip0x value of
    Right bytes -> pure bytes
    Left err -> fail $ "invalid calldata hex fixture: " <> err

strip0x :: BS.ByteString -> BS.ByteString
strip0x value
  | "0x" `BS.isPrefixOf` value = BS.drop 2 value
  | otherwise = value
