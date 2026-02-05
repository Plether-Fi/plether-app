module Plether.Indexer.EventsSpec (spec) where

import Data.Aeson (Value (..), decode, encode)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Test.Hspec

import Plether.Indexer.Contracts
import Plether.Indexer.Events

spec :: Spec
spec = do
  describe "parseEventLog" $ do
    it "returns Nothing for empty topics" $
      parseEventLog (emptyLog []) [] [] `shouldSatisfy` isNothing

    it "parses Mint event" $ do
      let log = mkLog (esTopic mintEvent)
            [ esTopic mintEvent
            , padAddress "0x1234567890123456789012345678901234567890"
            ]
            (encodeUint256 1000000 <> encodeUint256 2000000)
      let result = parseEventLog log [] []
      result `shouldSatisfy` isJust
      case result of
        Just pe -> do
          peTxType pe `shouldBe` "mint"
          T.toLower (peUserAddress pe) `shouldBe` "0x1234567890123456789012345678901234567890"
        Nothing -> expectationFailure "Expected Just"

    it "parses Burn event" $ do
      let log = mkLog (esTopic burnEvent)
            [ esTopic burnEvent
            , padAddress "0xabcdef0123456789abcdef0123456789abcdef01"
            ]
            (encodeUint256 500000 <> encodeUint256 1000000)
      let result = parseEventLog log [] []
      case result of
        Just pe -> do
          peTxType pe `shouldBe` "burn"
          T.toLower (peUserAddress pe) `shouldBe` "0xabcdef0123456789abcdef0123456789abcdef01"
        Nothing -> expectationFailure "Expected Just"

    it "parses TokenExchange event" $ do
      let log = mkLog (esTopic tokenExchangeEvent)
            [ esTopic tokenExchangeEvent
            , padAddress "0x1111111111111111111111111111111111111111"
            ]
            (encodeUint256 0 <> encodeUint256 1000 <> encodeUint256 1 <> encodeUint256 990)
      let result = parseEventLog log [] []
      case result of
        Just pe -> peTxType pe `shouldBe` "swap"
        Nothing -> expectationFailure "Expected Just"

    it "parses ZapMint event with bull side" $ do
      let log = mkLog (esTopic zapMintEvent)
            [ esTopic zapMintEvent
            , padAddress "0x2222222222222222222222222222222222222222"
            ]
            (encodeUint256 100 <> encodeUint256 200)
      let result = parseEventLog log [] []
      case result of
        Just pe -> do
          peTxType pe `shouldBe` "zap_buy"
          peSide pe `shouldBe` Just "bull"
        Nothing -> expectationFailure "Expected Just"

    it "parses ZapBurn event with bull side" $ do
      let log = mkLog (esTopic zapBurnEvent)
            [ esTopic zapBurnEvent
            , padAddress "0x3333333333333333333333333333333333333333"
            ]
            (encodeUint256 200 <> encodeUint256 100)
      let result = parseEventLog log [] []
      case result of
        Just pe -> do
          peTxType pe `shouldBe` "zap_sell"
          peSide pe `shouldBe` Just "bull"
        Nothing -> expectationFailure "Expected Just"

    it "parses Deposit (staking) event with bear side" $ do
      let bearContract = "0xbear000000000000000000000000000000000000"
          log = mkLogWithAddress bearContract (esTopic stakingDepositEvent)
            [ esTopic stakingDepositEvent
            , padAddress "0xsender00000000000000000000000000000000"
            , padAddress "0xowner000000000000000000000000000000000"
            ]
            (encodeUint256 1000 <> encodeUint256 1000)
      let result = parseEventLog log [bearContract] []
      case result of
        Just pe -> do
          peTxType pe `shouldBe` "stake"
          peSide pe `shouldBe` Just "bear"
        Nothing -> expectationFailure "Expected Just"

    it "parses Deposit (staking) event with bull side" $ do
      let bullContract = "0xbull000000000000000000000000000000000000"
          log = mkLogWithAddress bullContract (esTopic stakingDepositEvent)
            [ esTopic stakingDepositEvent
            , padAddress "0xsender00000000000000000000000000000000"
            , padAddress "0xowner000000000000000000000000000000000"
            ]
            (encodeUint256 1000 <> encodeUint256 1000)
      let result = parseEventLog log [] [bullContract]
      case result of
        Just pe -> do
          peTxType pe `shouldBe` "stake"
          peSide pe `shouldBe` Just "bull"
        Nothing -> expectationFailure "Expected Just"

    it "parses Withdraw (unstake) event" $ do
      let bearContract = "0xbear000000000000000000000000000000000000"
          log = mkLogWithAddress bearContract (esTopic stakingWithdrawEvent)
            [ esTopic stakingWithdrawEvent
            , padAddress "0xsender00000000000000000000000000000000"
            , padAddress "0xreceiver000000000000000000000000000000"
            , padAddress "0xowner000000000000000000000000000000000"
            ]
            (encodeUint256 500 <> encodeUint256 500)
      let result = parseEventLog log [bearContract] []
      case result of
        Just pe -> do
          peTxType pe `shouldBe` "unstake"
          peSide pe `shouldBe` Just "bear"
        Nothing -> expectationFailure "Expected Just"

    it "parses PositionOpened event" $ do
      let bullContract = "0xbull000000000000000000000000000000000000"
          log = mkLogWithAddress bullContract (esTopic positionOpenedEvent)
            [ esTopic positionOpenedEvent
            , padAddress "0xuser0000000000000000000000000000000000"
            ]
            (encodeUint256 1000 <> encodeUint256 200 <> encodeUint256 2000 <> encodeUint256 1000)
      let result = parseEventLog log [] [bullContract]
      case result of
        Just pe -> do
          peTxType pe `shouldBe` "leverage_open"
          peSide pe `shouldBe` Just "bull"
        Nothing -> expectationFailure "Expected Just"

    it "parses PositionClosed event" $ do
      let bearContract = "0xbear000000000000000000000000000000000000"
          log = mkLogWithAddress bearContract (esTopic positionClosedEvent)
            [ esTopic positionClosedEvent
            , padAddress "0xuser0000000000000000000000000000000000"
            ]
            (encodeUint256 1500 <> encodeUint256 1000)
      let result = parseEventLog log [bearContract] []
      case result of
        Just pe -> do
          peTxType pe `shouldBe` "leverage_close"
          peSide pe `shouldBe` Just "bear"
        Nothing -> expectationFailure "Expected Just"

    it "returns Nothing for unknown event" $ do
      let unknownTopic = keccak256Text "UnknownEvent(address,uint256)"
          log = mkLog unknownTopic [unknownTopic] (encodeUint256 100)
      parseEventLog log [] [] `shouldSatisfy` isNothing

  describe "ParsedEvent JSON serialization" $ do
    it "peData contains expected fields for mint" $ do
      let log = mkLog (esTopic mintEvent)
            [ esTopic mintEvent
            , padAddress "0x1234567890123456789012345678901234567890"
            ]
            (encodeUint256 1000000 <> encodeUint256 2000000)
      let result = parseEventLog log [] []
      case result of
        Just pe -> do
          let json = encode (peData pe)
          LBS.length json `shouldSatisfy` (> 0)
        Nothing -> expectationFailure "Expected Just"

  describe "determineSide" $ do
    it "identifies bear contracts" $ do
      let bearContract = "0xbear000000000000000000000000000000000000"
          log = mkLogWithAddress bearContract (esTopic stakingDepositEvent)
            [ esTopic stakingDepositEvent
            , padAddress "0x0000000000000000000000000000000000000000"
            , padAddress "0xowner000000000000000000000000000000000"
            ]
            (encodeUint256 100 <> encodeUint256 100)
      let result = parseEventLog log [bearContract] []
      case result of
        Just pe -> peSide pe `shouldBe` Just "bear"
        Nothing -> expectationFailure "Expected Just"

    it "identifies bull contracts case-insensitively" $ do
      let bullContract = "0xBULL000000000000000000000000000000000000"
          log = mkLogWithAddress (T.toLower bullContract) (esTopic stakingDepositEvent)
            [ esTopic stakingDepositEvent
            , padAddress "0x0000000000000000000000000000000000000000"
            , padAddress "0xowner000000000000000000000000000000000"
            ]
            (encodeUint256 100 <> encodeUint256 100)
      let result = parseEventLog log [] [bullContract]
      case result of
        Just pe -> peSide pe `shouldBe` Just "bull"
        Nothing -> expectationFailure "Expected Just"

    it "returns Nothing for unknown contract" $ do
      let unknownContract = "0xunknown0000000000000000000000000000000"
          log = mkLogWithAddress unknownContract (esTopic stakingDepositEvent)
            [ esTopic stakingDepositEvent
            , padAddress "0x0000000000000000000000000000000000000000"
            , padAddress "0xowner000000000000000000000000000000000"
            ]
            (encodeUint256 100 <> encodeUint256 100)
      let result = parseEventLog log ["0xbear"] ["0xbull"]
      case result of
        Just pe -> peSide pe `shouldBe` Nothing
        Nothing -> expectationFailure "Expected Just"

emptyLog :: [ByteString] -> EventLog
emptyLog topics = EventLog
  { elTxHash = "0x0000000000000000000000000000000000000000000000000000000000000000"
  , elBlockNumber = 0
  , elAddress = "0x0000000000000000000000000000000000000000"
  , elTopics = topics
  , elData = ""
  }

mkLog :: ByteString -> [ByteString] -> ByteString -> EventLog
mkLog _ topics logData = EventLog
  { elTxHash = "0x1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef"
  , elBlockNumber = 12345678
  , elAddress = "0xcontract000000000000000000000000000000000"
  , elTopics = topics
  , elData = logData
  }

mkLogWithAddress :: Text -> ByteString -> [ByteString] -> ByteString -> EventLog
mkLogWithAddress addr _ topics logData = EventLog
  { elTxHash = "0x1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef"
  , elBlockNumber = 12345678
  , elAddress = addr
  , elTopics = topics
  , elData = logData
  }

padAddress :: Text -> ByteString
padAddress addr =
  let stripped = if T.isPrefixOf "0x" addr then T.drop 2 addr else addr
      decoded = case B16.decode (TE.encodeUtf8 stripped) of
        Right bs -> bs
        Left _ -> BS.replicate 20 0
  in BS.replicate (32 - BS.length decoded) 0 <> decoded

encodeUint256 :: Integer -> ByteString
encodeUint256 n =
  let bytes = integerToBytes n
      padding = BS.replicate (32 - BS.length bytes) 0
  in padding <> bytes

integerToBytes :: Integer -> ByteString
integerToBytes 0 = BS.singleton 0
integerToBytes n = BS.pack $ reverse $ go n
  where
    go 0 = []
    go x = fromIntegral (x `mod` 256) : go (x `div` 256)
