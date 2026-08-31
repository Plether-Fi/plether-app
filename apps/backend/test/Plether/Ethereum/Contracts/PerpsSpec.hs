module Plether.Ethereum.Contracts.PerpsSpec (spec) where

import Data.Aeson (Value (..), decode, encode, object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (toList)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types (status200)
import Network.Wai (Application, responseLBS, strictRequestBody)
import Network.Wai.Handler.Warp (testWithApplication)
import Plether.Ethereum.Abi (decodeUint256, encodeAddress, encodeInt256, encodeUint256)
import Plether.Ethereum.Contracts.Perps
import Plether.Ethereum.Client (EthClient, RpcError (..), newClient)
import Plether.Ethereum.Rpc (RpcLog (..), TxReceipt (..))
import Plether.Pyth.Basket (PythPricePoint (..))
import Test.Hspec

hexText :: BS.ByteString -> Text
hexText = TE.decodeUtf8 . B16.encode

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

    it "accepts an exact inclusive Pyth publish-time bound" $ do
      let feedId = BS.replicate 32 0x11
      call <-
        expectRight $
          parsePriceFeedUpdatesUniqueCall
            [BS.singleton 0x01]
            [feedId]
            20
            20
      word call 2 `shouldBe` 20
      word call 3 `shouldBe` 20

    it "encodes executeOrder(uint64,bytes[]) with an empty array" $ do
      executeOrderCall 7 []
        `shouldEncodeTo` "0xc700abdc000000000000000000000000000000000000000000000000000000000000000700000000000000000000000000000000000000000000000000000000000000400000000000000000000000000000000000000000000000000000000000000000"

    it "encodes executeOrder(uint64,bytes[]) with one dynamic bytes value" $ do
      executeOrderCall 7 [BS.pack [0x99]]
        `shouldEncodeTo` "0xc700abdc000000000000000000000000000000000000000000000000000000000000000700000000000000000000000000000000000000000000000000000000000000400000000000000000000000000000000000000000000000000000000000000001000000000000000000000000000000000000000000000000000000000000002000000000000000000000000000000000000000000000000000000000000000019900000000000000000000000000000000000000000000000000000000000000"

    it "encodes executeOrderBatch(uint64,bytes[]) with one dynamic bytes value" $ do
      executeOrderBatchCall 7 [BS.pack [0x99]]
        `shouldEncodeTo` "0x8c3679bc000000000000000000000000000000000000000000000000000000000000000700000000000000000000000000000000000000000000000000000000000000400000000000000000000000000000000000000000000000000000000000000001000000000000000000000000000000000000000000000000000000000000002000000000000000000000000000000000000000000000000000000000000000019900000000000000000000000000000000000000000000000000000000000000"

    it "decodes the typed executeOrder result including InsufficientGas" $ do
      let receiptHash = BS.replicate 32 0x11
          encoded =
            encodeUint256 7
              <> encodeUint256 1
              <> encodeUint256 0
              <> encodeUint256 5
              <> receiptHash
      decodeOrderExecutionResult encoded
        `shouldBe` Right
          OrderExecutionResult
            { oerOrderId = 7
            , oerLifecycleStatus = 1
            , oerTerminalReason = 0
            , oerPendingReason = 5
            , oerReceiptHash = receiptHash
            }

    it "decodes a typed batch result with terminal progress" $ do
      let encoded = encodeUint256 8 <> encodeUint256 2 <> encodeUint256 0
      decodeOrderBatchResult encoded
        `shouldBe` Right
          OrderBatchResult
            { obrNextOrderId = 8
            , obrTerminalCount = 2
            , obrStopReason = 0
            }

    it "rejects malformed or out-of-range typed execution results" $ do
      decodeOrderExecutionResult BS.empty `shouldSatisfy` isDecodeError
      decodeOrderExecutionResult
        ( encodeUint256 7
            <> encodeUint256 4
            <> encodeUint256 0
            <> encodeUint256 0
            <> BS.replicate 32 0
        )
        `shouldSatisfy` isDecodeError
      decodeOrderBatchResult (encodeUint256 8 <> encodeUint256 (2 ^ (32 :: Integer)) <> encodeUint256 0)
        `shouldSatisfy` isDecodeError

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

    it "encodes executeLiquidationBatch(address[],bytes[]) with two accounts" $ do
      expectedSelector <- parseHex "0x07f4f6cf"
      let firstAccount = "0x1111111111111111111111111111111111111111"
          secondAccount = "0x2222222222222222222222222222222222222222"
          call = executeLiquidationBatchCall [firstAccount, secondAccount] [BS.pack [0x99]]
      BS.take 4 call `shouldBe` expectedSelector
      word call 0 `shouldBe` 64
      word call 1 `shouldBe` 160
      word call 2 `shouldBe` 2
      wordBytes call 3 `shouldBe` encodeAddress firstAccount
      wordBytes call 4 `shouldBe` encodeAddress secondAccount
      word call 5 `shouldBe` 1
      word call 6 `shouldBe` 32
      word call 7 `shouldBe` 1
      BS.take 1 (bytesAtWord call 8) `shouldBe` BS.pack [0x99]

    it "encodes the exact cached HousePool LP settlement calldata" $ do
      settleLpEpochPoolCall 123_456_789 1_700_000_000
        `shouldEncodeTo` "0x4c9bffad00000000000000000000000000000000000000000000000000000000075bcd15000000000000000000000000000000000000000000000000000000006553f100"

    it "encodes the exact six-feed atomic Router LP settlement calldata" $ do
      let sixFeedUpdateData =
            [ BS.pack [0x01]
            , BS.pack [0x02, 0x03]
            , BS.pack [0x03, 0x04, 0x05]
            , BS.pack [0x04, 0x05, 0x06, 0x07]
            , BS.pack [0x05, 0x06, 0x07, 0x08, 0x09]
            , BS.pack [0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b]
            ]
      settleLpEpochRouterCall sixFeedUpdateData
        `shouldEncodeTo` "0x0ad6dd2e0000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000000000000000000600000000000000000000000000000000000000000000000000000000000000c000000000000000000000000000000000000000000000000000000000000001000000000000000000000000000000000000000000000000000000000000000140000000000000000000000000000000000000000000000000000000000000018000000000000000000000000000000000000000000000000000000000000001c0000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000000000000000000010100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000202030000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000003030405000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000040405060700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000505060708090000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000006060708090a0b0000000000000000000000000000000000000000000000000000"

    it "quotes the exact fee for the same admitted six-feed payload" $ do
      let exactFee = 987_654
          oracle = "0x7777777777777777777777777777777777777777"
          sixFeedUpdateData = map BS.singleton [1 .. 6]
      captured <- newIORef Nothing
      withUpdateFeeRpc exactFee captured $ \client ->
        getUpdateFee client oracle sixFeedUpdateData `shouldReturn` Right exactFee
      capturedRequest <- readIORef captured
      capturedRequest
        `shouldBe` Just (oracle, bytesHexText $ getUpdateFeeCall sixFeedUpdateData)

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
    it "strictly decodes the signed current price from each nine-word PriceFeed" $ do
      let firstFeed = BS.replicate 32 0x11
          secondFeed = BS.replicate 32 0x22
          encoded =
            encodePriceFeedStructs
              [ encodePriceFeed firstFeed 123456 78 (-8) 1000 123400 80 (-8) 999
              , encodePriceFeed secondFeed (-42) 9 (-10) 1001 (-40) 10 (-10) 998
              ]
      decodeParsedPriceFeeds [firstFeed, secondFeed] encoded
        `shouldBe` Right
          [ PythPricePoint
              { pppFeedId = "0x1111111111111111111111111111111111111111111111111111111111111111"
              , pppPrice = 123456
              , pppConfidence = 78
              , pppExponent = -8
              , pppPublishTime = 1000
              }
          , PythPricePoint
              { pppFeedId = "0x2222222222222222222222222222222222222222222222222222222222222222"
              , pppPrice = -42
              , pppConfidence = 9
              , pppExponent = -10
              , pppPublishTime = 1001
              }
          ]

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

    it "rejects trailing ABI words and non-canonical typed fields" $ do
      let feedId = BS.replicate 32 0x11
          valid = encodePriceFeed feedId 100 2 (-8) 1000 99 3 (-8) 999
          invalidCurrentPrice =
            encodePriceFeed feedId (2 ^ (63 :: Integer)) 2 (-8) 1000 99 3 (-8) 999
          invalidCurrentConfidence =
            encodePriceFeed feedId 100 (2 ^ (64 :: Integer)) (-8) 1000 99 3 (-8) 999
          invalidCurrentExponent =
            encodePriceFeed feedId 100 2 (2 ^ (31 :: Integer)) 1000 99 3 (-8) 999
          invalidEmaConfidence =
            encodePriceFeed feedId 100 2 (-8) 1000 99 (2 ^ (64 :: Integer)) (-8) 999
      decodeParsedPriceFeeds [feedId] (encodePriceFeedStructs [valid] <> BS.replicate 32 0)
        `shouldSatisfy` isDecodeError
      decodeParsedPriceFeeds [feedId] (encodePriceFeedStructs [invalidCurrentPrice])
        `shouldSatisfy` isDecodeError
      decodeParsedPriceFeeds [feedId] (encodePriceFeedStructs [invalidCurrentConfidence])
        `shouldSatisfy` isDecodeError
      decodeParsedPriceFeeds [feedId] (encodePriceFeedStructs [invalidCurrentExponent])
        `shouldSatisfy` isDecodeError
      decodeParsedPriceFeeds [feedId] (encodePriceFeedStructs [invalidEmaConfidence])
        `shouldSatisfy` isDecodeError

  describe "bounded V2 lifecycle ABI" $ do
    it "pins the deployed OrderFinalized topic" $ do
      hexText orderFinalizedTopic
        `shouldBe` "449a7e19a9375343901f9775e5874784dc4e77750b1ee0f11e231f87cbe2f1af"

    it "decodes the canonical terminal outcome getter" $ do
      let receiptHash = BS.replicate 32 0xaa
          outcomeWord :: Int -> BS.ByteString
          outcomeWord index
            | index == 5 = encodeUint256 3
            | index == 6 = encodeUint256 2
            | index == 7 = encodeUint256 1
            | index == 10 = encodeUint256 303858802
            | index == 15 = encodeUint256 101250000
            | index == 20 = encodeUint256 4
            | index == 22 = receiptHash
            | otherwise = encodeUint256 0
      decodeOrderTerminalOutcome (mconcat $ map outcomeWord [0 .. 22])
        `shouldBe` Right
          OrderTerminalOutcome
            { otoLifecycleStatus = 3
            , otoTerminalReason = 2
            , otoExecutionMode = 1
            , otoTerminalBlock = 303858802
            , otoExecutionPrice = 101250000
            , otoFailedConstraint = 4
            , otoReceiptHash = receiptHash
            }

    it "rejects a non-terminal outcome" $ do
      decodeOrderTerminalOutcome (BS.replicate (23 * 32) 0)
        `shouldSatisfy` isDecodeError

  describe "decodePerpsOrderEvent" $ do
    it "decodes OrderCommitted" $ do
      let logEntry =
            RpcLog
              { rpcLogTxHash = "0xabc"
              , rpcLogBlockNumber = 123
              , rpcLogBlockHash = "0xblock"
              , rpcLogTransactionIndex = 0
              , rpcLogIndex = 0
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
              , rpcLogBlockHash = "0xblock"
              , rpcLogTransactionIndex = 0
              , rpcLogIndex = 0
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
              , rpcLogBlockHash = "0xblock"
              , rpcLogTransactionIndex = 0
              , rpcLogIndex = 0
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

    it "decodes V2 intent identity" $ do
      let account = "0x1111111111111111111111111111111111111111"
          clientOrderId = BS.replicate 32 0x11
          eventData =
            encodeUint256 999
              <> encodeUint256 7
              <> clientOrderId
              <> encodeUint256 1
              <> BS.replicate (16 * 32) 0
          logEntry =
            RpcLog
              { rpcLogTxHash = "0xintent"
              , rpcLogBlockNumber = 126
              , rpcLogAddress = "0xlifecycle"
              , rpcLogTopics =
                  [ intentRegisteredTopic
                  , encodeUint256 42
                  , encodeAddress account
                  , clientOrderId
                  ]
              , rpcLogData = eventData
              }
      decodePerpsOrderEvent logEntry
        `shouldBe` Just
          IntentRegistered
            { poeOrderId = 42
            , poeAccount = account
            , poeClientOrderId = "0x" <> hexText clientOrderId
            , poeSide = 1
            , poeTxHash = "0xintent"
            , poeBlockNumber = 126
            }

    it "decodes canonical V2 OrderFinalized receipt fields" $ do
      let account = "0x1111111111111111111111111111111111111111"
          clientOrderId = BS.replicate 32 0x11
          receiptHash = BS.replicate 32 0xaa
          eventWord :: Int -> BS.ByteString
          eventWord index
            | index == 0 = receiptHash
            | index == 9 = encodeUint256 2
            | index == 10 = encodeUint256 1
            | index == 11 = encodeUint256 1
            | index == 14 = encodeUint256 123456
            | index == 25 = encodeUint256 0
            | otherwise = encodeUint256 0
          logEntry =
            RpcLog
              { rpcLogTxHash = "0xreceipt"
              , rpcLogBlockNumber = 127
              , rpcLogAddress = "0xlifecycle"
              , rpcLogTopics =
                  [ orderFinalizedTopic
                  , encodeUint256 42
                  , encodeAddress account
                  , clientOrderId
                  ]
              , rpcLogData = mconcat $ map eventWord [0 .. 45]
              }
      decodePerpsOrderEvent logEntry
        `shouldBe` Just
          OrderFinalized
            { poeOrderId = 42
            , poeAccount = account
            , poeClientOrderId = "0x" <> hexText clientOrderId
            , poeReceiptHash = "0x" <> hexText receiptHash
            , poeLifecycleStatus = 2
            , poeTerminalReason = 1
            , poeExecutionMode = 1
            , poeFailedConstraint = 0
            , poeExecutionPrice = 123456
            , poeTxHash = "0xreceipt"
            , poeBlockNumber = 127
            }

  describe "position event account decoding" $ do
    it "decodes PositionOpened and PositionLiquidated indexed accounts" $ do
      let account = "0x1111111111111111111111111111111111111111"
          positionLog topic =
            RpcLog
              { rpcLogTxHash = "0xabc"
              , rpcLogBlockNumber = 123
              , rpcLogBlockHash = "0xblock"
              , rpcLogTransactionIndex = 0
              , rpcLogIndex = 0
              , rpcLogAddress = "0xengine"
              , rpcLogTopics = [topic, encodeAddress account]
              , rpcLogData = ""
              }
      decodePositionOpenedAccount (positionLog positionOpenedTopic) `shouldBe` Just account
      decodePositionLiquidatedAccount (positionLog positionLiquidatedTopic) `shouldBe` Just account
      decodePositionOpenedAccount (positionLog positionLiquidatedTopic) `shouldBe` Nothing

  describe "liquidation batch event decoding" $ do
    it "decodes an indexed batch item including its bytes4 error selector" $ do
      let account = "0x1111111111111111111111111111111111111111"
          selector = BS.pack [0xde, 0xad, 0xbe, 0xef]
          logEntry =
            RpcLog
              { rpcLogTxHash = "0xbatch"
              , rpcLogBlockNumber = 126
              , rpcLogBlockHash = "0xblock"
              , rpcLogTransactionIndex = 0
              , rpcLogIndex = 0
              , rpcLogAddress = "0xrouter"
              , rpcLogTopics =
                  [ liquidationBatchItemTopic
                  , encodeUint256 3
                  , encodeAddress account
                  ]
              , rpcLogData =
                  encodeUint256 3
                    <> encodeUint256 77
                    <> selector
                    <> BS.replicate 28 0
              }
      decodeLiquidationBatchItem logEntry
        `shouldBe` Just
          LiquidationBatchItem
            { lbiIndex = 3
            , lbiAccount = account
            , lbiResult = LiquidationBatchFailed
            , lbiKeeperBountyUsdc = 77
            , lbiErrorSelector = selector
            }

    it "decodes the indexed stop cursor and rejects unknown item results" $ do
      let stopLog =
            RpcLog
              { rpcLogTxHash = "0xbatch"
              , rpcLogBlockNumber = 126
              , rpcLogBlockHash = "0xblock"
              , rpcLogTransactionIndex = 0
              , rpcLogIndex = 0
              , rpcLogAddress = "0xrouter"
              , rpcLogTopics = [liquidationBatchStoppedTopic, encodeUint256 4]
              , rpcLogData = BS.empty
              }
          invalidItem =
            RpcLog
              { rpcLogTxHash = "0xbatch"
              , rpcLogBlockNumber = 126
              , rpcLogBlockHash = "0xblock"
              , rpcLogTransactionIndex = 0
              , rpcLogIndex = 0
              , rpcLogAddress = "0xrouter"
              , rpcLogTopics =
                  [ liquidationBatchItemTopic
                  , encodeUint256 0
                  , encodeAddress "0x1111111111111111111111111111111111111111"
                  ]
              , rpcLogData = encodeUint256 4 <> encodeUint256 0 <> BS.replicate 32 0
              }
      decodeLiquidationBatchStoppedIndex stopLog `shouldBe` Just 4
      decodeLiquidationBatchItem invalidItem `shouldBe` Nothing

  describe "LpEpochSettled decoding" $ do
    it "decodes and validates exactly one HousePool event with durable identity" $ do
      let event = lpSettlementLog 500_000 [10, 20, 30, 40, 1, 0, 1]
          receipt = lpSettlementReceipt [event]
      lpEpochSettledTopic
        `shouldBe` unsafeHex "cb683c928926f0e5d3426cec6288d011a54cc04072165b80b20be7b3ce9784a4"
      requireSingleLpEpochSettled housePool 500_000 receipt
        `shouldBe` Right
          LpEpochSettled
            { lpesCutoffEpoch = 500_000
            , lpesSeniorRedeemAssets = 10
            , lpesJuniorRedeemAssets = 20
            , lpesJuniorDepositAssets = 30
            , lpesSeniorDepositAssets = 40
            , lpesSeniorBacklog = True
            , lpesJuniorBacklog = False
            , lpesEntriesDeferred = True
            , lpesTxHash = settlementTxHash
            , lpesBlockNumber = 123
            , lpesBlockHash = settlementBlockHash
            , lpesTransactionIndex = 3
            , lpesLogIndex = 7
            }

    it "preserves a partial Senior withdrawal as funded assets plus FIFO backlog" $ do
      let event = lpSettlementLog 500_000 [10, 0, 0, 0, 1, 0, 0]
      case requireSingleLpEpochSettled housePool 500_000 (lpSettlementReceipt [event]) of
        Left err -> expectationFailure $ T.unpack err
        Right decoded -> do
          lpesSeniorRedeemAssets decoded `shouldBe` 10
          lpesSeniorBacklog decoded `shouldBe` True
          lpesJuniorBacklog decoded `shouldBe` False

    it "preserves frozen-oracle deposit deferral without inventing deposited assets" $ do
      let event = lpSettlementLog 500_000 [10, 20, 0, 0, 0, 0, 1]
      case requireSingleLpEpochSettled housePool 500_000 (lpSettlementReceipt [event]) of
        Left err -> expectationFailure $ T.unpack err
        Right decoded -> do
          lpesJuniorDepositAssets decoded `shouldBe` 0
          lpesSeniorDepositAssets decoded `shouldBe` 0
          lpesEntriesDeferred decoded `shouldBe` True

    it "rejects duplicate, wrong-cutoff, malformed, and non-canonical events" $ do
      let valid = lpSettlementLog 500_000 [10, 20, 30, 40, 0, 0, 0]
          nonCanonical = lpSettlementLog 500_000 [10, 20, 30, 40, 2, 0, 0]
          truncated = valid {rpcLogData = BS.take (6 * 32) $ rpcLogData valid}
          wrongTransactionIndex = valid {rpcLogTransactionIndex = 4}
      requireSingleLpEpochSettled housePool 499_999 (lpSettlementReceipt [valid])
        `shouldSatisfy` isDecodeError
      requireSingleLpEpochSettled housePool 500_000 (lpSettlementReceipt [valid, valid])
        `shouldSatisfy` isDecodeError
      requireSingleLpEpochSettled housePool 500_000 (lpSettlementReceipt [wrongTransactionIndex])
        `shouldSatisfy` isDecodeError
      decodeLpEpochSettled nonCanonical `shouldSatisfy` isDecodeError
      decodeLpEpochSettled truncated `shouldSatisfy` isDecodeError

    it "recognizes no-progress supersession from message or nested RPC data" $ do
      isNoLpEpochProgressRpcError
        (RpcNodeError 3 "execution reverted: HousePool__NoLpEpochProgress()" Nothing)
        `shouldBe` True
      isNoLpEpochProgressRpcError
        (RpcNodeError (-32000) "execution reverted" (Just "{\"data\":\"0x86cca6b8\"}"))
        `shouldBe` True
      isNoLpEpochProgressRpcError (RpcNodeError 3 "execution reverted" (Just "0xdeadbeef"))
        `shouldBe` False

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

housePool :: Text
housePool = "0x1111111111111111111111111111111111111111"

settlementTxHash :: Text
settlementTxHash = "0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

settlementBlockHash :: Text
settlementBlockHash = "0xbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"

lpSettlementLog :: Integer -> [Integer] -> RpcLog
lpSettlementLog cutoff values =
  RpcLog
    { rpcLogTxHash = settlementTxHash
    , rpcLogBlockNumber = 123
    , rpcLogBlockHash = settlementBlockHash
    , rpcLogTransactionIndex = 3
    , rpcLogIndex = 7
    , rpcLogAddress = housePool
    , rpcLogTopics = [lpEpochSettledTopic, encodeUint256 cutoff]
    , rpcLogData = mconcat $ map encodeUint256 values
    }

lpSettlementReceipt :: [RpcLog] -> TxReceipt
lpSettlementReceipt logs =
  TxReceipt
    { receiptTxHash = settlementTxHash
    , receiptBlockNumber = 123
    , receiptBlockHash = settlementBlockHash
    , receiptTransactionIndex = 3
    , receiptSucceeded = True
    , receiptLogs = logs
    }

unsafeHex :: BS.ByteString -> BS.ByteString
unsafeHex encoded =
  case B16.decode encoded of
    Right bytes -> bytes
    Left _ -> BS.empty

expectRight :: Show a => Either a b -> IO b
expectRight value =
  case value of
    Left err -> expectationFailure ("unexpected Left: " <> show err) >> fail "unreachable"
    Right result -> pure result

withUpdateFeeRpc
  :: Integer
  -> IORef (Maybe (Text, Text))
  -> (EthClient -> IO a)
  -> IO a
withUpdateFeeRpc exactFee captured action =
  testWithApplication (pure $ updateFeeRpcApplication exactFee captured) $ \port -> do
    client <- newClient $ "http://127.0.0.1:" <> T.pack (show port)
    action client

updateFeeRpcApplication :: Integer -> IORef (Maybe (Text, Text)) -> Application
updateFeeRpcApplication exactFee captured request respond = do
  body <- strictRequestBody request
  writeIORef captured $ rpcCallTargetAndData body
  respond $
    responseLBS
      status200
      [("Content-Type", "application/json")]
      ( encode $
          object
            [ "jsonrpc" .= ("2.0" :: Text)
            , "id" .= (1 :: Integer)
            , "result" .= bytesHexText (encodeUint256 exactFee)
            ]
      )

rpcCallTargetAndData :: LBS.ByteString -> Maybe (Text, Text)
rpcCallTargetAndData body = do
  Object request <- decode body
  Array params <- KeyMap.lookup (Key.fromText "params") request
  Object call <- case toList params of
    value : _ -> Just value
    [] -> Nothing
  String target <- KeyMap.lookup (Key.fromText "to") call
  String calldata <- KeyMap.lookup (Key.fromText "data") call
  pure (T.toLower target, calldata)

bytesHexText :: BS.ByteString -> Text
bytesHexText = ("0x" <>) . TE.decodeUtf8 . B16.encode

encodePriceFeeds :: [BS.ByteString] -> BS.ByteString
encodePriceFeeds feedIds =
  encodePriceFeedStructs [feedId <> BS.replicate (8 * 32) 0 | feedId <- feedIds]

encodePriceFeedStructs :: [BS.ByteString] -> BS.ByteString
encodePriceFeedStructs feeds =
  encodeUint256 32
    <> encodeUint256 (fromIntegral $ length feeds)
    <> mconcat feeds

encodePriceFeed
  :: BS.ByteString
  -> Integer
  -> Integer
  -> Integer
  -> Integer
  -> Integer
  -> Integer
  -> Integer
  -> Integer
  -> BS.ByteString
encodePriceFeed feedId price confidence priceExponent publishTime emaPrice emaConfidence emaExponent emaPublishTime =
  feedId
    <> encodeInt256 price
    <> encodeUint256 confidence
    <> encodeInt256 priceExponent
    <> encodeUint256 publishTime
    <> encodeInt256 emaPrice
    <> encodeUint256 emaConfidence
    <> encodeInt256 emaExponent
    <> encodeUint256 emaPublishTime

isRightAddress :: Text -> Either a Text -> Bool
isRightAddress expected value =
  case value of
    Left _ -> False
    Right actual -> actual == expected
