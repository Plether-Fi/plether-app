module Plether.Ethereum.Contracts.PerpsSpec (spec) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.Text (Text)
import Plether.Ethereum.Abi (decodeUint256, encodeAddress, encodeUint256)
import Plether.Ethereum.Client (RpcError (..))
import Plether.Ethereum.Contracts.Perps
import qualified Plether.Ethereum.Multicall as Multicall
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

    it "encodes updatePriceFeeds(bytes[]) for latest-payload admission" $ do
      updatePriceFeedsCall [BS.pack [0x01, 0x02, 0x03]]
        `shouldEncodeTo` "0xef9e5e2800000000000000000000000000000000000000000000000000000000000000200000000000000000000000000000000000000000000000000000000000000001000000000000000000000000000000000000000000000000000000000000002000000000000000000000000000000000000000000000000000000000000000030102030000000000000000000000000000000000000000000000000000000000"

    it "encodes payable parsePriceFeedUpdatesUnique calldata" $ do
      let feedId = BS.replicate 32 0x11
      call <-
        expectRight $
          parsePriceFeedUpdatesUniqueCall
            [BS.pack [0x01, 0x02, 0x03]]
            [feedId]
            10
            20
      call
        `shouldEncodeTo` "0xaccca7f900000000000000000000000000000000000000000000000000000000000000800000000000000000000000000000000000000000000000000000000000000100000000000000000000000000000000000000000000000000000000000000000a0000000000000000000000000000000000000000000000000000000000000014000000000000000000000000000000000000000000000000000000000000000100000000000000000000000000000000000000000000000000000000000000200000000000000000000000000000000000000000000000000000000000000003010203000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000011111111111111111111111111111111111111111111111111111111111111111"

    it "encodes payable non-unique parsePriceFeedUpdates calldata" $ do
      let feedId = BS.replicate 32 0x11
      call <-
        expectRight $
          parsePriceFeedUpdatesCall
            [BS.pack [0x01, 0x02, 0x03]]
            [feedId]
            10
            20
      call
        `shouldEncodeTo` "0x4716e9c500000000000000000000000000000000000000000000000000000000000000800000000000000000000000000000000000000000000000000000000000000100000000000000000000000000000000000000000000000000000000000000000a0000000000000000000000000000000000000000000000000000000000000014000000000000000000000000000000000000000000000000000000000000000100000000000000000000000000000000000000000000000000000000000000200000000000000000000000000000000000000000000000000000000000000003010203000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000011111111111111111111111111111111111111111111111111111111111111111"

    it "rejects invalid parsePriceFeedUpdatesUnique inputs before RPC" $ do
      let feedId = BS.replicate 32 0x11
          payload = [BS.singleton 0x01]
      parsePriceFeedUpdatesUniqueCall [] [feedId] 10 20 `shouldSatisfy` isDecodeError
      parsePriceFeedUpdatesUniqueCall payload [] 10 20 `shouldSatisfy` isDecodeError
      parsePriceFeedUpdatesUniqueCall payload [BS.replicate 31 0x11] 10 20
        `shouldSatisfy` isDecodeError
      parsePriceFeedUpdatesUniqueCall payload [feedId] 20 10 `shouldSatisfy` isDecodeError

    it "encodes executeOrder(uint64,bytes[]) with an empty array" $ do
      executeOrderCall 7 []
        `shouldEncodeTo` "0xc700abdc000000000000000000000000000000000000000000000000000000000000000700000000000000000000000000000000000000000000000000000000000000400000000000000000000000000000000000000000000000000000000000000000"

    it "encodes executeOrder(uint64,bytes[]) with one dynamic bytes value" $ do
      executeOrderCall 7 [BS.pack [0x99]]
        `shouldEncodeTo` "0xc700abdc000000000000000000000000000000000000000000000000000000000000000700000000000000000000000000000000000000000000000000000000000000400000000000000000000000000000000000000000000000000000000000000001000000000000000000000000000000000000000000000000000000000000002000000000000000000000000000000000000000000000000000000000000000019900000000000000000000000000000000000000000000000000000000000000"

    it "encodes executeOrderBatch(uint64,bytes[]) with one dynamic bytes value" $ do
      executeOrderBatchCall 7 [BS.pack [0x99]]
        `shouldEncodeTo` "0x8c3679bc000000000000000000000000000000000000000000000000000000000000000700000000000000000000000000000000000000000000000000000000000000400000000000000000000000000000000000000000000000000000000000000001000000000000000000000000000000000000000000000000000000000000002000000000000000000000000000000000000000000000000000000000000000019900000000000000000000000000000000000000000000000000000000000000"

    it "encodes executeLiquidation(address,bytes[]) with one dynamic bytes value" $ do
      expectedSelector <- parseHex "0x4882af85"
      let account = "0x1111111111111111111111111111111111111111"
          call = executeLiquidationCall account [BS.pack [0x99]]
      BS.take 4 call `shouldBe` expectedSelector
      wordBytes call 0 `shouldBe` encodeAddress account
      word call 1 `shouldBe` 64
      word call 2 `shouldBe` 1
      word call 3 `shouldBe` 32
      word call 4 `shouldBe` 1
      BS.take 1 (bytesAtWord call 5) `shouldBe` BS.pack [0x99]

    it "encodes positions(address)" $ do
      positionsCall "0x1111111111111111111111111111111111111111"
        `shouldEncodeTo` "0x55f575100000000000000000000000001111111111111111111111111111111111111111"

    it "rejects truncated positions(address) return data" $ do
      decodePositionSize BS.empty `shouldSatisfy` isDecodeError

    it "decodes position size only after all seven ABI words are present" $ do
      let encodedPosition = encodeUint256 42 <> BS.replicate (6 * 32) 0
      case decodePositionSize encodedPosition of
        Right 42 -> pure ()
        result -> expectationFailure $ "unexpected decode result: " <> show result

    it "builds one failure-tolerant position call per account in order" $ do
      let cfdEngine = "0x3333333333333333333333333333333333333333"
          accounts =
            [ "0x1111111111111111111111111111111111111111"
            , "0x2222222222222222222222222222222222222222"
            ]
          calls = positionSizeCalls cfdEngine accounts
      map Multicall.callTarget calls `shouldBe` replicate 2 cfdEngine
      map Multicall.callAllowFailure calls `shouldBe` replicate 2 True
      map Multicall.callCalldata calls `shouldBe` map positionsCall accounts

    it "keeps batched position results aligned across failures" $ do
      let encodedPosition = encodeUint256 42 <> BS.replicate (6 * 32) 0
          results =
            [ Multicall.CallResult False BS.empty
            , Multicall.CallResult True encodedPosition
            , Multicall.CallResult True BS.empty
            ]
      decodePositionSizeBatch 3 results
        `shouldBe` Right
          [ Left $ RpcJsonError "positions(address) multicall subcall failed"
          , Right 42
          , Left $ RpcJsonError "positions(address) returned fewer than seven ABI words"
          ]

    it "rejects a position batch with the wrong result count" $ do
      decodePositionSizeBatch 2 [Multicall.CallResult True $ BS.replicate (7 * 32) 0]
        `shouldBe` Left
          (RpcJsonError "positions(address) multicall returned 1 results for 2 accounts")

    it "builds and decodes an empty position batch without work" $ do
      length (positionSizeCalls "0x3333333333333333333333333333333333333333" []) `shouldBe` 0
      decodePositionSizeBatch 0 [] `shouldBe` Right []

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

  describe "PletherOracle config calldata" $ do
    it "encodes adverseConfidenceMultiplierBps()" $ do
      adverseConfidenceMultiplierBpsCall `shouldEncodeTo` "0x6c6def16"

    it "encodes pyth()" $ do
      pythCall `shouldEncodeTo` "0xf98d06f0"

    it "strictly decodes a nonzero pyth() address" $ do
      let address = "0x1111111111111111111111111111111111111111"
      decodePythContract (encodeAddress address) `shouldSatisfy` isRightAddress address
      decodePythContract BS.empty `shouldSatisfy` isDecodeError
      decodePythContract (BS.replicate 32 0) `shouldSatisfy` isDecodeError

  describe "parsePriceFeedUpdatesUnique return decoding" $ do
    it "accepts the requested feed IDs in order" $ do
      let feedIds = [BS.replicate 32 0x11, BS.replicate 32 0x22]
      case decodeParsedPriceFeedIds feedIds (encodePriceFeeds feedIds) of
        Right actual -> actual `shouldBe` feedIds
        Left err -> expectationFailure $ "unexpected decode error: " <> show err

    it "rejects empty and truncated returns" $ do
      let feedId = BS.replicate 32 0x11
          emptyReturn = encodeUint256 32 <> encodeUint256 0
          truncatedReturn = encodeUint256 32 <> encodeUint256 1 <> feedId
      decodeParsedPriceFeedIds [feedId] BS.empty `shouldSatisfy` isDecodeError
      decodeParsedPriceFeedIds [feedId] emptyReturn `shouldSatisfy` isDecodeError
      decodeParsedPriceFeedIds [feedId] truncatedReturn `shouldSatisfy` isDecodeError

    it "rejects a wrong count or mismatched feed ID" $ do
      let requested = BS.replicate 32 0x11
          other = BS.replicate 32 0x22
      decodeParsedPriceFeedIds [requested] (encodePriceFeeds [requested, other])
        `shouldSatisfy` isDecodeError
      decodeParsedPriceFeedIds [requested] (encodePriceFeeds [other])
        `shouldSatisfy` isDecodeError

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

  describe "position event account decoding" $ do
    it "decodes PositionOpened and PositionLiquidated indexed accounts" $ do
      let account = "0x1111111111111111111111111111111111111111"
          positionLog topic =
            RpcLog
              { rpcLogTxHash = "0xabc"
              , rpcLogBlockNumber = 123
              , rpcLogAddress = "0xengine"
              , rpcLogTopics = [topic, encodeAddress account]
              , rpcLogData = ""
              }
      decodePositionOpenedAccount (positionLog positionOpenedTopic) `shouldBe` Just account
      decodePositionLiquidatedAccount (positionLog positionLiquidatedTopic) `shouldBe` Just account
      decodePositionOpenedAccount (positionLog positionLiquidatedTopic) `shouldBe` Nothing

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

isDecodeError :: Either a b -> Bool
isDecodeError value =
  case value of
    Left _ -> True
    Right _ -> False

expectRight :: Show a => Either a b -> IO b
expectRight value =
  case value of
    Left err -> expectationFailure ("unexpected Left: " <> show err) >> fail "unreachable"
    Right result -> pure result

encodePriceFeeds :: [BS.ByteString] -> BS.ByteString
encodePriceFeeds feedIds =
  encodeUint256 32
    <> encodeUint256 (fromIntegral $ length feedIds)
    <> mconcat [feedId <> BS.replicate (8 * 32) 0 | feedId <- feedIds]

isRightAddress :: Text -> Either a Text -> Bool
isRightAddress expected value =
  case value of
    Left _ -> False
    Right actual -> actual == expected
