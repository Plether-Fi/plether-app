{-# LANGUAGE LambdaCase #-}

module Plether.Perps.HistoryIndexerSpec (spec) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Word (Word8)
import Plether.Indexer.Contracts (keccak256Text)
import Plether.Perps.HistoryIndexer
  ( BlockInfo (..)
  , ParsedPerpsLog (..)
  , RpcLog (..)
  , TradeCosts (..)
  , canCertifyIndexedRange
  , decodeCloseTradeCosts
  , decodeOpenTradeCosts
  , decodeReplayTradeCosts
  , isMarketVolumeActivity
  , orderFailReasonName
  , parsePerpsLog
  , terminalStatus
  , validateIndexedBoundary
  , validateReplayBounds
  , validateReplayLogScope
  , parseReplayLogEntry
  , parseReplayBlockInfo
  , parseReplayBlockNumber
  , parseReplayTransactionInfo
  , replayPreviewCallData
  , validateReplayStateUnchanged
  , validateRpcLogBlockHash
  )
import Plether.Perps.IndexerOptions (ReplayOptions (..))
import Test.Hspec

spec :: Spec
spec = do
  describe "bounded replay invariants" $ do
    let replay =
          ReplayOptions
            { roFromBlock = 1_100
            , roToBlock = 1_199
            , roStatementTimeoutMs = 30_000
            , roLockTimeoutMs = 5_000
            , roMaxRuntimeSeconds = 300
            }

    it "accepts only an exact range inside start, safe head, and cursor" $
      validateReplayBounds 1_000 1_300 1_250 replay `shouldBe` Right ()

    it "rejects a range below start, above safe head, or above the cursor" $ do
      validateReplayBounds 1_101 1_300 1_250 replay
        `shouldBe` Left "Replay range begins below the configured indexer start block"
      validateReplayBounds 1_000 1_198 1_250 replay
        `shouldBe` Left "Replay range extends above the current confirmed safe head"
      validateReplayBounds 1_000 1_300 1_198 replay
        `shouldBe` Left "Replay range extends above the persisted canonical cursor"

    it "requires cursor, coverage, history, and semantic rollup identity" $ do
      validateReplayStateUnchanged
        (1 :: Int) 1 [Just (2 :: Int)] [Just 2] ("history" :: String) "history" [3 :: Int] [3]
        `shouldBe` Right ()
      validateReplayStateUnchanged
        (1 :: Int) 1 [Just (2 :: Int)] [Just 2] ("before" :: String) "after" [3 :: Int] [3]
        `shouldBe` Left "Bounded replay was not an idempotent canonical-history ingestion"
      validateReplayStateUnchanged
        (1 :: Int) 1 [Just (2 :: Int)] [Just 2] ("history" :: String) "history" [3 :: Int] [4]
        `shouldBe` Left "Bounded replay was not an idempotent market-volume ingestion"

    it "rejects logs outside the exact replay range or address allowlist" $ do
      let scopedLog =
            (mkLog orderExecutedTopic [word 42] (word 101250000))
              { rlBlockNumber = 1_150
              , rlAddress = testAccount
              }
      validateReplayLogScope 1_100 1_199 [testAccount] scopedLog `shouldBe` Right ()
      validateReplayLogScope 1_151 1_199 [testAccount] scopedLog
        `shouldBe` Left "RPC returned a Perps replay log outside the exact requested range"
      validateReplayLogScope 1_100 1_199 ["0x0000000000000000000000000000000000000001"] scopedLog
        `shouldBe` Left "RPC returned a Perps replay log outside the configured address allowlist"

    it "strictly rejects malformed replay log array entries" $ do
      parseReplayLogEntry validReplayLogJson `shouldSatisfy` isRight
      parseReplayLogEntry (object [])
        `shouldBe` Left "Transaction response is missing string field address"
      parseReplayLogEntry (String "not-an-object")
        `shouldBe` Left "Replay log entry must be a JSON object"
      parseReplayLogEntry (replaceField "blockNumber" "0x01" validReplayLogJson)
        `shouldBe` Left "Replay log field is not a canonical hex quantity: blockNumber"
      parseReplayLogEntry (replaceField "data" "0x0" validReplayLogJson)
        `shouldBe` Left "Replay log field is not canonical hex bytes: data"
      parseReplayLogEntry (replaceField "blockHash" "0xdead" validReplayLogJson)
        `shouldBe` Left "Replay log transaction/block hash is not a canonical 32-byte hash"

    it "strictly decodes replay head and block responses" $ do
      parseReplayBlockNumber (String "0x2a") `shouldBe` Right 42
      parseReplayBlockNumber (String "0x02a")
        `shouldBe` Left "Replay block number is not a canonical hex quantity"
      parseReplayBlockInfo 42 validReplayBlockJson
        `shouldBe` Right (BlockInfo 42 canonicalHash 1_700_000_000)
      parseReplayBlockInfo 41 validReplayBlockJson
        `shouldBe` Left "Replay block response number does not match the requested block"
      parseReplayBlockInfo 42 (replaceField "hash" "0xdead" validReplayBlockJson)
        `shouldBe` Left "Replay block hash is not a canonical 32-byte hash"
      parseReplayBlockInfo 42 (object ["number" .= ("0x2a" :: Text), "hash" .= canonicalHash])
        `shouldBe` Left "Transaction response is missing string field timestamp"
      parseReplayBlockInfo 42 (replaceField "timestamp" "1700000000" validReplayBlockJson)
        `shouldBe` Left "Replay block timestamp is not a canonical hex quantity"

    it "strictly decodes canonical replay transactions" $ do
      parseReplayTransactionInfo validReplayTransactionJson `shouldSatisfy` isRight
      parseReplayTransactionInfo (replaceField "from" "0xdead" validReplayTransactionJson)
        `shouldBe` Left "Replay transaction sender/target is not a canonical 20-byte address"
      parseReplayTransactionInfo (replaceField "blockHash" "0xdead" validReplayTransactionJson)
        `shouldBe` Left "Replay transaction hash/block hash is not a canonical 32-byte hash"
      parseReplayTransactionInfo (replaceField "input" "0x0" validReplayTransactionJson)
        `shouldBe` Left "Replay log field is not canonical hex bytes: transaction input"

    it "requires exact replay trade-cost preview result lengths" $ do
      decodeReplayTradeCosts "Open" (BS.replicate (10 * 32) 0)
        `shouldBe` Right (TradeCosts 0 0)
      decodeReplayTradeCosts "Open" (BS.replicate (10 * 32 + 32) 0)
        `shouldBe` Left "Replay open preview result must contain exactly 10 ABI words"
      decodeReplayTradeCosts "Close" (BS.replicate (8 * 32 - 1) 0)
        `shouldBe` Left "Replay close preview result must contain exactly 8 ABI words"

    it "encodes the exact six Open preview arguments in canonical positions" $ do
      let parsed =
            ParsedPositionActivity
              "Open" testAccount 1 (Just 101) (Just 202) (Just 303) Nothing (object [])
      case replayPreviewCallData parsed of
        Nothing -> expectationFailure "expected Open preview calldata"
        Just encoded -> do
          BS.length encoded `shouldBe` 4 + 6 * 32
          BS.drop 4 encoded
            `shouldBe` BS.concat
              [ addressWord testAccount
              , word 1
              , word 202
              , word 303
              , word 101
              , word 0
              ]

  describe "canCertifyIndexedRange" $ do
    it "allows the configured first range and the exact range after a cursor" $ do
      canCertifyIndexedRange 1_000 0 1_000 1_099 `shouldBe` True
      canCertifyIndexedRange 1_000 1_099 1_100 1_199 `shouldBe` True

    it "does not certify a disjoint future backfill" $
      canCertifyIndexedRange 1_000 1_099 1_200 1_299 `shouldBe` False

    it "does not certify historical or overlapping replays" $ do
      canCertifyIndexedRange 1_000 1_299 1_000 1_099 `shouldBe` False
      canCertifyIndexedRange 1_000 1_099 1_050 1_199 `shouldBe` False

    it "rejects an empty or reversed range" $
      canCertifyIndexedRange 1_000 1_099 1_100 1_099 `shouldBe` False

  describe "validateIndexedBoundary" $ do
    let boundary =
          BlockInfo
            { biNumber = 1_099
            , biHash = "0xABCDEF"
            , biTimestamp = 1_785_437_841
            }

    it "accepts the persisted boundary block and normalized hash" $
      validateIndexedBoundary 1_099 "abcdef" boundary `shouldBe` Right ()

    it "rejects a different boundary block" $
      validateIndexedBoundary 1_098 "0xabcdef" boundary
        `shouldBe` Left "Canonical cursor boundary block number changed before commit"

    it "rejects a provider or fork switch at the boundary" $
      validateIndexedBoundary 1_099 "0xdifferent" boundary
        `shouldBe` Left "Canonical cursor boundary block hash changed before commit"

  describe "validateRpcLogBlockHash" $ do
    let logEntry = mkLog orderExecutedTopic [word 42] (word 101250000)
        canonicalBlock =
          BlockInfo
            { biNumber = rlBlockNumber logEntry
            , biHash = "0xBLOCK"
            , biTimestamp = 1_785_437_841
            }

    it "accepts a log from the fetched canonical block" $
      validateRpcLogBlockHash logEntry canonicalBlock
        `shouldBe` Right ()

    it "rejects a provider-fork log before it can be persisted" $
      validateRpcLogBlockHash
        logEntry
        canonicalBlock {biHash = "0xdifferent"}
        `shouldBe` Left "RPC log block hash does not match canonical block metadata"

  describe "parsePerpsLog" $ do
    it "parses OrderCommitted" $ do
      parsePerpsLog (mkLog orderCommittedTopic [word 42, addressTopic] (word 1))
        `shouldSatisfy` \case
          Just (ParsedOrderCommitted 42 account 1 _) -> account == testAccount
          _ -> False

    it "parses OrderExecuted" $ do
      parsePerpsLog (mkLog orderExecutedTopic [word 42] (word 101250000))
        `shouldBeParsedAs` \case
          ParsedOrderExecuted 42 101250000 _ -> True
          _ -> False

    it "parses OrderFailed and classifies expired cleanup state" $ do
      parsePerpsLog (mkLog orderFailedTopic [word 42] (word 0))
        `shouldBeParsedAs` \case
          ParsedOrderFailed 42 0 "Expired" _ -> True
          _ -> False
      terminalStatus "Expired" `shouldBe` "Expired / Cleaned up"
      terminalStatus "EngineRevert" `shouldBe` "Failed"

    it "parses position lifecycle activity" $ do
      parsePerpsLog (mkLog positionOpenedTopic [addressTopic] (words32 [0, 1_000, 101_000_000, 200_000_000]))
        `shouldBeParsedAs` \case
          ParsedPositionActivity "Open" account 0 (Just 101_000_000) (Just 1_000) (Just 200_000_000) Nothing _ ->
            account == testAccount
          _ -> False

      parsePerpsLog (mkLog positionClosedTopic [addressTopic] (words32 [1, 500, 99_000_000] <> signedWord (-75_000_000)))
        `shouldBeParsedAs` \case
          ParsedPositionActivity "Close" account 1 (Just 99_000_000) (Just 500) Nothing (Just (-75_000_000)) _ ->
            account == testAccount
          _ -> False

      parsePerpsLog (mkLog positionLiquidatedTopic [addressTopic] (words32 [1, 500, 99_000_000, 200_000]))
        `shouldBeParsedAs` \case
          ParsedPositionActivity "Liquidated" account 1 (Just 99_000_000) (Just 500) (Just 200_000) Nothing _ ->
            account == testAccount
          _ -> False

    it "parses margin account activity" $ do
      parsePerpsLog (mkLog marginAddedTopic [addressTopic] (word 5_000_000))
        `shouldBeParsedAs` \case
          ParsedMarginActivity "Add margin" account 5_000_000 _ -> account == testAccount
          _ -> False

      parsePerpsLog ((mkLog depositTopic [addressTopic, otherAddressTopic] (word 100_000_000)) {rlAddress = marginClearinghouse})
        `shouldBeParsedAs` \case
          ParsedMarginActivity "Deposit" account 100_000_000 payload ->
            account == testAccount
              && payload
                == object
                  [ "account" .= testAccount
                  , "asset" .= testAsset
                  , "contractAddress" .= testEmitter
                  , "amountUsdc" .= ("100000000" :: Text)
                  ]
          _ -> False

      parsePerpsLog ((mkLog withdrawTopic [addressTopic, otherAddressTopic] (word 25_000_000)) {rlAddress = marginClearinghouse})
        `shouldBeParsedAs` \case
          ParsedMarginActivity "Withdraw" account 25_000_000 _ -> account == testAccount
          _ -> False

  describe "orderFailReasonName" $ do
    it "matches deployed OrderFailReason ordinals" $ do
      map orderFailReasonName [0 .. 5]
        `shouldBe` ["Expired", "CloseOnly", "SlippageExceeded", "EnginePanic", "AccountLiquidated", "EngineRevert"]

  describe "isMarketVolumeActivity" $ do
    let payload = object []
        position kind price size =
          ParsedPositionActivity kind testAccount 0 price size Nothing Nothing payload

    it "accepts only canonical notional-bearing lifecycle events" $ do
      isMarketVolumeActivity (position "Open" (Just 101_000_000) (Just 1_000)) `shouldBe` True
      isMarketVolumeActivity (position "Close" (Just 99_000_000) (Just 500)) `shouldBe` True
      isMarketVolumeActivity (position "Liquidated" (Just 98_000_000) (Just 250)) `shouldBe` True

    it "rejects incomplete and non-volume activity" $ do
      isMarketVolumeActivity (position "Open" Nothing (Just 1_000)) `shouldBe` False
      isMarketVolumeActivity (position "Close" (Just 99_000_000) Nothing) `shouldBe` False
      isMarketVolumeActivity (position "Unknown" (Just 99_000_000) (Just 500)) `shouldBe` False
      isMarketVolumeActivity (ParsedMarginActivity "Deposit" testAccount 100 payload) `shouldBe` False

  describe "trade cost previews" $ do
    it "recovers an open fee from trade cost even when the preview is otherwise invalid" $ do
      let vpi = -3_916_326_394
          executionFee = 1_540_032_807
          preview = words32 [0, 5, 2, 96_915_422, 3_972_620_599_900_312_417_020_777, 3_850_082_018_852, 137_483_749_999]
            <> signedWord vpi
            <> word 0
            <> signedWord (vpi + executionFee)
      decodeOpenTradeCosts preview
        `shouldBe` Right (TradeCosts executionFee vpi)

    it "decodes signed close VPI and execution fee" $ do
      let vpi = -4_487_207_153
          executionFee = 1_748_645_480
          preview =
            words32 [1, 0, 96_866_388, 4_513_034_696_886_011_329_166_042, 3_424_490_727]
              <> signedWord vpi
              <> word 0
              <> word executionFee
      decodeCloseTradeCosts preview
        `shouldBe` Right (TradeCosts executionFee vpi)

validReplayLogJson :: Value
validReplayLogJson =
  object
    [ "address" .= testAccount
    , "topics" .= [canonicalHash, canonicalHash]
    , "data" .= ("0x" <> Text.replicate 64 "0")
    , "transactionHash" .= canonicalHash
    , "blockNumber" .= ("0x2a" :: Text)
    , "blockHash" .= canonicalHash
    , "transactionIndex" .= ("0x1" :: Text)
    , "logIndex" .= ("0x0" :: Text)
    ]

validReplayBlockJson :: Value
validReplayBlockJson =
  object
    [ "number" .= ("0x2a" :: Text)
    , "hash" .= canonicalHash
    , "timestamp" .= ("0x6553f100" :: Text)
    ]

validReplayTransactionJson :: Value
validReplayTransactionJson =
  object
    [ "hash" .= canonicalHash
    , "from" .= testAccount
    , "to" .= testAccount
    , "blockHash" .= canonicalHash
    , "input" .= ("0x" :: Text)
    ]

canonicalHash :: Text
canonicalHash = "0x" <> Text.replicate 64 "a"

replaceField :: Text -> Text -> Value -> Value
replaceField name value = \case
  Object fields -> Object $ KeyMap.insert (Key.fromText name) (String value) fields
  other -> other

isRight :: Either a b -> Bool
isRight = \case
  Right _ -> True
  Left _ -> False

addressWord :: Text -> ByteString
addressWord value =
  BS.replicate 12 0 <> addressBytes
 where
  addressBytes =
    case B16.decode $ TextEncoding.encodeUtf8 $ Text.drop 2 value of
      Right bytes -> bytes
      Left _ -> ""

shouldBeParsedAs :: Maybe ParsedPerpsLog -> (ParsedPerpsLog -> Bool) -> Expectation
shouldBeParsedAs parsed predicate =
  case parsed of
    Just event | predicate event -> pure ()
    _ -> expectationFailure $ "unexpected parsed event: " <> show parsed

mkLog :: ByteString -> [ByteString] -> ByteString -> RpcLog
mkLog topic indexedTopics eventData =
  RpcLog
    { rlAddress = "0x97A901dE2B267c307E264FD5F71403F8072F73e7"
    , rlTopics = topic : indexedTopics
    , rlData = eventData
    , rlTxHash = "0xabc"
    , rlBlockNumber = 123
    , rlBlockHash = "0xblock"
    , rlTxIndex = 1
    , rlLogIndex = 2
    }

word :: Integer -> ByteString
word n = BS.pack $ replicate (32 - length bytes) 0 <> bytes
  where
    bytes = toBytes n

signedWord :: Integer -> ByteString
signedWord n
  | n >= 0 = word n
  | otherwise = word (2 ^ (256 :: Int) + n)

words32 :: [Integer] -> ByteString
words32 = BS.concat . map word

toBytes :: Integer -> [Word8]
toBytes 0 = []
toBytes n = reverse $ go n
  where
    go 0 = []
    go value = fromInteger (value `mod` 256) : go (value `div` 256)

addressTopic :: ByteString
addressTopic = word 0x5a71a4094ec81165ada48aa4c27da48ec27e0d6b

otherAddressTopic :: ByteString
otherAddressTopic = word 0x55e007d79906572ccca8e75b1beb302787348d6e

testAccount :: Text
testAccount = "0x5a71a4094ec81165ada48aa4c27da48ec27e0d6b"

testAsset :: Text
testAsset = "0x55e007d79906572ccca8e75b1beb302787348d6e"

testEmitter :: Text
testEmitter = "0x731bb0939ce531728459394a277b28cbff8df049"

marginClearinghouse :: Text
marginClearinghouse = "0x731bb0939CE531728459394A277B28Cbff8df049"

orderCommittedTopic :: ByteString
orderCommittedTopic = keccak256Text "OrderCommitted(uint64,address,uint8)"

orderExecutedTopic :: ByteString
orderExecutedTopic = keccak256Text "OrderExecuted(uint64,uint256)"

orderFailedTopic :: ByteString
orderFailedTopic = keccak256Text "OrderFailed(uint64,uint8)"

positionOpenedTopic :: ByteString
positionOpenedTopic = keccak256Text "PositionOpened(address,uint8,uint256,uint256,uint256)"

positionClosedTopic :: ByteString
positionClosedTopic = keccak256Text "PositionClosed(address,uint8,uint256,uint256,int256)"

positionLiquidatedTopic :: ByteString
positionLiquidatedTopic = keccak256Text "PositionLiquidated(address,uint8,uint256,uint256,uint256)"

marginAddedTopic :: ByteString
marginAddedTopic = keccak256Text "MarginAdded(address,uint256)"

depositTopic :: ByteString
depositTopic = keccak256Text "Deposit(address,address,uint256)"

withdrawTopic :: ByteString
withdrawTopic = keccak256Text "Withdraw(address,address,uint256)"
