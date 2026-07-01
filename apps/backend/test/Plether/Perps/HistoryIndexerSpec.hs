{-# LANGUAGE LambdaCase #-}

module Plether.Perps.HistoryIndexerSpec (spec) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import Data.Word (Word8)
import Plether.Indexer.Contracts (keccak256Text)
import Plether.Perps.HistoryIndexer
  ( ParsedPerpsLog (..)
  , RpcLog (..)
  , orderFailReasonName
  , parsePerpsLog
  , terminalStatus
  )
import Test.Hspec

spec :: Spec
spec = do
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

      parsePerpsLog (mkLog depositTopic [addressTopic, otherAddressTopic] (word 100_000_000))
        `shouldBeParsedAs` \case
          ParsedMarginActivity "Deposit" account 100_000_000 _ -> account == testAccount
          _ -> False

      parsePerpsLog (mkLog withdrawTopic [addressTopic, otherAddressTopic] (word 25_000_000))
        `shouldBeParsedAs` \case
          ParsedMarginActivity "Withdraw" account 25_000_000 _ -> account == testAccount
          _ -> False

  describe "orderFailReasonName" $ do
    it "matches deployed OrderFailReason ordinals" $ do
      map orderFailReasonName [0 .. 5]
        `shouldBe` ["Expired", "CloseOnly", "SlippageExceeded", "EnginePanic", "AccountLiquidated", "EngineRevert"]

shouldBeParsedAs :: Maybe ParsedPerpsLog -> (ParsedPerpsLog -> Bool) -> Expectation
shouldBeParsedAs parsed predicate =
  case parsed of
    Just event | predicate event -> pure ()
    _ -> expectationFailure $ "unexpected parsed event: " <> show parsed

mkLog :: ByteString -> [ByteString] -> ByteString -> RpcLog
mkLog topic indexedTopics eventData =
  RpcLog
    { rlAddress = "0x4A0a6c028164A1254e10C3e39cc89Af45090069e"
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
