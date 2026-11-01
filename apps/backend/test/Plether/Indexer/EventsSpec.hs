module Plether.Indexer.EventsSpec (spec) where

import Control.Monad (forM_)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
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
      parseEventLog (emptyLog []) [] [] noMorphoMarkets `shouldSatisfy` isNothing

    it "parses Minted event" $ do
      let log = mkLog (esTopic mintEvent)
            [ esTopic mintEvent
            , padAddress "0x1234567890123456789012345678901234567890"
            ]
            (encodeUint256 1000000)
      let result = parseEventLog log [] [] noMorphoMarkets
      result `shouldSatisfy` isJust
      case result of
        Just pe -> do
          peTxType pe `shouldBe` "mint"
          T.toLower (peUserAddress pe) `shouldBe` "0x1234567890123456789012345678901234567890"
        Nothing -> expectationFailure "Expected Just"

    it "parses Burned event" $ do
      let log = mkLog (esTopic burnEvent)
            [ esTopic burnEvent
            , padAddress "0xabcdef0123456789abcdef0123456789abcdef01"
            ]
            (encodeUint256 500000)
      let result = parseEventLog log [] [] noMorphoMarkets
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
            (encodeUint256 0 <> encodeUint256 1000 <> encodeUint256 1 <> encodeUint256 990 <> encodeUint256 10 <> encodeUint256 999)
      let result = parseEventLog log [] [] noMorphoMarkets
      case result of
        Just pe -> peTxType pe `shouldBe` "swap"
        Nothing -> expectationFailure "Expected Just"

    it "parses ZapMint event with bull side" $ do
      let log = mkLog (esTopic zapMintEvent)
            [ esTopic zapMintEvent
            , padAddress "0x2222222222222222222222222222222222222222"
            ]
            (encodeUint256 100 <> encodeUint256 200 <> encodeUint256 50 <> encodeUint256 195)
      let result = parseEventLog log [] [] noMorphoMarkets
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
      let result = parseEventLog log [] [] noMorphoMarkets
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
      let result = parseEventLog log [bearContract] [] noMorphoMarkets
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
      let result = parseEventLog log [] [bullContract] noMorphoMarkets
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
            , padAddress "0x00000000000000000000000000000000000000ec"
            , padAddress "0xowner000000000000000000000000000000000"
            ]
            (encodeUint256 500 <> encodeUint256 500)
      let result = parseEventLog log [bearContract] [] noMorphoMarkets
      case result of
        Just pe -> do
          peTxType pe `shouldBe` "unstake"
          peSide pe `shouldBe` Just "bear"
        Nothing -> expectationFailure "Expected Just"

    it "parses LeverageOpened event" $ do
      let bullContract = "0xbull000000000000000000000000000000000000"
          log = mkLogWithAddress bullContract (esTopic leverageOpenedEvent)
            [ esTopic leverageOpenedEvent
            , padAddress "0x0000000000000000000000000000000000aa0001"
            ]
            (encodeUint256 1000 <> encodeUint256 200 <> encodeUint256 2000 <> encodeUint256 1500 <> encodeUint256 1000 <> encodeUint256 50)
      let result = parseEventLog log [] [bullContract] noMorphoMarkets
      case result of
        Just pe -> do
          peTxType pe `shouldBe` "leverage_open"
          peSide pe `shouldBe` Just "bull"
        Nothing -> expectationFailure "Expected Just"

    it "parses LeverageClosed event" $ do
      let bearContract = "0xbear000000000000000000000000000000000000"
          log = mkLogWithAddress bearContract (esTopic leverageClosedEvent)
            [ esTopic leverageClosedEvent
            , padAddress "0x0000000000000000000000000000000000aa0001"
            ]
            (encodeUint256 1500 <> encodeUint256 1000 <> encodeUint256 800 <> encodeUint256 50)
      let result = parseEventLog log [bearContract] [] noMorphoMarkets
      case result of
        Just pe -> do
          peTxType pe `shouldBe` "leverage_close"
          peSide pe `shouldBe` Just "bear"
        Nothing -> expectationFailure "Expected Just"

    it "returns Nothing for unknown event" $ do
      let unknownTopic = keccak256Text "UnknownEvent(address,uint256)"
          log = mkLog unknownTopic [unknownTopic] (encodeUint256 100)
      parseEventLog log [] [] noMorphoMarkets `shouldSatisfy` isNothing

    it "parses Morpho Supply event with bear side" $ do
      let log = mkLog (esTopic morphoSupplyEvent)
            [ esTopic morphoSupplyEvent
            , hexToBytes testBearMarketId
            , padAddress "0xca11e40000000000000000000000000000000000"
            , padAddress "0x0000000000000000000000000000000000aa0001"
            ]
            (encodeUint256 5000000 <> encodeUint256 5000000000000000000)
      let result = parseEventLog log [] [] testMorphoMarkets
      case result of
        Just pe -> do
          peTxType pe `shouldBe` "lending_supply"
          peSide pe `shouldBe` Just "bear"
          T.toLower (peUserAddress pe) `shouldBe` "0x0000000000000000000000000000000000aa0001"
        Nothing -> expectationFailure "Expected Just"

    it "parses Morpho Supply event with bull side" $ do
      let log = mkLog (esTopic morphoSupplyEvent)
            [ esTopic morphoSupplyEvent
            , hexToBytes testBullMarketId
            , padAddress "0xca11e40000000000000000000000000000000000"
            , padAddress "0x0000000000000000000000000000000000aa0001"
            ]
            (encodeUint256 1000000 <> encodeUint256 1000000000000000000)
      let result = parseEventLog log [] [] testMorphoMarkets
      case result of
        Just pe -> do
          peTxType pe `shouldBe` "lending_supply"
          peSide pe `shouldBe` Just "bull"
        Nothing -> expectationFailure "Expected Just"

    it "parses Morpho Withdraw event with bear side" $ do
      let log = mkLog (esTopic morphoWithdrawEvent)
            [ esTopic morphoWithdrawEvent
            , hexToBytes testBearMarketId
            , padAddress "0xca11e40000000000000000000000000000000000"
            , padAddress "0x0000000000000000000000000000000000aa0001"
            ]
            (padAddress "0x00000000000000000000000000000000000000ec" <> encodeUint256 3000000 <> encodeUint256 3000000000000000000)
      let result = parseEventLog log [] [] testMorphoMarkets
      case result of
        Just pe -> do
          peTxType pe `shouldBe` "lending_withdraw"
          peSide pe `shouldBe` Just "bear"
          T.toLower (peUserAddress pe) `shouldBe` "0x0000000000000000000000000000000000aa0001"
        Nothing -> expectationFailure "Expected Just"

    it "parses Morpho Borrow event with bull side" $ do
      let log = mkLog (esTopic morphoBorrowEvent)
            [ esTopic morphoBorrowEvent
            , hexToBytes testBullMarketId
            , padAddress "0xca11e40000000000000000000000000000000000"
            , padAddress "0x0000000000000000000000000000000000aa0001"
            ]
            (padAddress "0x00000000000000000000000000000000000000ec" <> encodeUint256 2000000 <> encodeUint256 2000000000000000000)
      let result = parseEventLog log [] [] testMorphoMarkets
      case result of
        Just pe -> do
          peTxType pe `shouldBe` "lending_borrow"
          peSide pe `shouldBe` Just "bull"
          T.toLower (peUserAddress pe) `shouldBe` "0x0000000000000000000000000000000000aa0001"
        Nothing -> expectationFailure "Expected Just"

    it "parses Morpho Repay event with bear side" $ do
      let log = mkLog (esTopic morphoRepayEvent)
            [ esTopic morphoRepayEvent
            , hexToBytes testBearMarketId
            , padAddress "0xca11e40000000000000000000000000000000000"
            , padAddress "0x0000000000000000000000000000000000aa0001"
            ]
            (encodeUint256 4000000 <> encodeUint256 4000000000000000000)
      let result = parseEventLog log [] [] testMorphoMarkets
      case result of
        Just pe -> do
          peTxType pe `shouldBe` "lending_repay"
          peSide pe `shouldBe` Just "bear"
          T.toLower (peUserAddress pe) `shouldBe` "0x0000000000000000000000000000000000aa0001"
        Nothing -> expectationFailure "Expected Just"

    it "Morpho event returns Nothing side for unknown market ID" $ do
      let unknownMarketId = BS.replicate 32 0xff
          log = mkLog (esTopic morphoSupplyEvent)
            [ esTopic morphoSupplyEvent
            , unknownMarketId
            , padAddress "0xca11e40000000000000000000000000000000000"
            , padAddress "0x0000000000000000000000000000000000aa0001"
            ]
            (encodeUint256 1000 <> encodeUint256 1000)
      let result = parseEventLog log [] [] testMorphoMarkets
      case result of
        Just pe -> do
          peTxType pe `shouldBe` "lending_supply"
          peSide pe `shouldBe` Nothing
        Nothing -> expectationFailure "Expected Just"

    it "Morpho event uses onBehalf (topic 3) not caller (topic 2)" $ do
      let callerAddr = "0xca11e40000000000000000000000000000000000"
          onBehalfAddr = "0x0be4a1f000000000000000000000000000000000"
          log = mkLog (esTopic morphoSupplyEvent)
            [ esTopic morphoSupplyEvent
            , hexToBytes testBearMarketId
            , padAddress callerAddr
            , padAddress onBehalfAddr
            ]
            (encodeUint256 1000 <> encodeUint256 1000)
      let result = parseEventLog log [] [] testMorphoMarkets
      case result of
        Just pe -> T.toLower (peUserAddress pe) `shouldBe` onBehalfAddr
        Nothing -> expectationFailure "Expected Just"

    it "Morpho Withdraw skips receiver in data, reads assets correctly" $ do
      let log = mkLog (esTopic morphoWithdrawEvent)
            [ esTopic morphoWithdrawEvent
            , hexToBytes testBearMarketId
            , padAddress "0xca11e40000000000000000000000000000000000"
            , padAddress "0x0000000000000000000000000000000000aa0001"
            ]
            (padAddress "0x00000000000000000000000000000000000000ec" <> encodeUint256 42 <> encodeUint256 99)
      let result = parseEventLog log [] [] testMorphoMarkets
      case result of
        Just pe -> peData pe `shouldBe` economicPayload [("assets", 42), ("shares", 99)]
        Nothing -> expectationFailure "Expected decoded Morpho withdrawal"

  describe "ParsedEvent JSON serialization" $ do
    it "peData contains expected fields for mint" $ do
      let log = mkLog (esTopic mintEvent)
            [ esTopic mintEvent
            , padAddress "0x1234567890123456789012345678901234567890"
            ]
            (encodeUint256 1000000)
      let result = parseEventLog log [] [] noMorphoMarkets
      case result of
        Just pe -> do
          peData pe `shouldBe` economicPayload [("amount", 1000000)]
        Nothing -> expectationFailure "Expected Just"

  describe "complete economic event payloads" $
    forM_ economicFixtures $ \(name, event, indexed, dat, expected) -> do
      it ("preserves every economic field for " <> name) $
        case parseEventLog (mkLog (esTopic event) (esTopic event : indexed) dat) [] [] testMorphoMarkets of
          Just parsed -> peData parsed `shouldBe` economicPayload expected
          Nothing -> expectationFailure $ "Expected " <> name

      it ("rejects truncated economic data for " <> name) $
        parseEventLog (mkLog (esTopic event) (esTopic event : indexed) (BS.init dat)) [] [] testMorphoMarkets
          `shouldSatisfy` isNothing

  describe "determineSide" $ do
    it "identifies bear contracts" $ do
      let bearContract = "0xbear000000000000000000000000000000000000"
          log = mkLogWithAddress bearContract (esTopic stakingDepositEvent)
            [ esTopic stakingDepositEvent
            , padAddress "0x0000000000000000000000000000000000000000"
            , padAddress "0xowner000000000000000000000000000000000"
            ]
            (encodeUint256 100 <> encodeUint256 100)
      let result = parseEventLog log [bearContract] [] noMorphoMarkets
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
      let result = parseEventLog log [] [bullContract] noMorphoMarkets
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
      let result = parseEventLog log ["0xbear"] ["0xbull"] noMorphoMarkets
      case result of
        Just pe -> peSide pe `shouldBe` Nothing
        Nothing -> expectationFailure "Expected Just"

  describe "determineSideByMarketId" $ do
    it "matches bear market ID case-insensitively" $ do
      let bearId = "0xAABBCCDDEEFF00112233445566778899AABBCCDDEEFF00112233445566778899"
          markets = MorphoMarkets
            { mmBearMarketIds = [bearId]
            , mmBullMarketIds = []
            }
          log = mkLog (esTopic morphoSupplyEvent)
            [ esTopic morphoSupplyEvent
            , hexToBytes (T.toLower bearId)
            , padAddress "0xca11e40000000000000000000000000000000000"
            , padAddress "0x0000000000000000000000000000000000aa0001"
            ]
            (encodeUint256 100 <> encodeUint256 100)
      let result = parseEventLog log [] [] markets
      case result of
        Just pe -> peSide pe `shouldBe` Just "bear"
        Nothing -> expectationFailure "Expected Just"

    it "matches bull market ID" $ do
      let bullId = "0x1122334455667788990011223344556677889900112233445566778899001122"
          markets = MorphoMarkets
            { mmBearMarketIds = []
            , mmBullMarketIds = [bullId]
            }
          log = mkLog (esTopic morphoRepayEvent)
            [ esTopic morphoRepayEvent
            , hexToBytes bullId
            , padAddress "0xca11e40000000000000000000000000000000000"
            , padAddress "0x0000000000000000000000000000000000aa0001"
            ]
            (encodeUint256 100 <> encodeUint256 100)
      let result = parseEventLog log [] [] markets
      case result of
        Just pe -> peSide pe `shouldBe` Just "bull"
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

noMorphoMarkets :: MorphoMarkets
noMorphoMarkets = MorphoMarkets
  { mmBearMarketIds = []
  , mmBullMarketIds = []
  }

testBearMarketId :: Text
testBearMarketId = "0xaaaa000000000000000000000000000000000000000000000000000000000001"

testBullMarketId :: Text
testBullMarketId = "0xbbbb000000000000000000000000000000000000000000000000000000000002"

testMorphoMarkets :: MorphoMarkets
testMorphoMarkets = MorphoMarkets
  { mmBearMarketIds = [testBearMarketId]
  , mmBullMarketIds = [testBullMarketId]
  }

hexToBytes :: Text -> ByteString
hexToBytes txt =
  let stripped = if T.isPrefixOf "0x" txt then T.drop 2 txt else txt
  in case B16.decode (TE.encodeUtf8 stripped) of
    Right bs -> bs
    Left _ -> BS.replicate 32 0

economicPayload :: [(Text, Integer)] -> Value
economicPayload fields = object [Key.fromText name .= value | (name, value) <- fields]

economicFixtures :: [(String, EventSignature, [ByteString], ByteString, [(Text, Integer)])]
economicFixtures =
  [ fixture "Minted" mintEvent [owner] [101] [("amount",101)]
  , fixture "Burned" burnEvent [owner] [102] [("amount",102)]
  , fixture "TokenExchange" tokenExchangeEvent [owner] [0,103,1,104,105,106]
      [("soldId",0),("tokensSold",103),("boughtId",1),("tokensBought",104),("fee",105),("packedPriceScale",106)]
  , fixture "ZapMint" zapMintEvent [owner] [107,108,109,110]
      [("usdcIn",107),("tokensOut",108),("maxSlippageBps",109),("actualSwapOut",110)]
  , fixture "ZapBurn" zapBurnEvent [owner] [111,112] [("amountIn",111),("amountOut",112)]
  , fixture "staking Deposit" stakingDepositEvent [sender,owner] [113,114] [("assets",113),("shares",114)]
  , fixture "staking Withdraw" stakingWithdrawEvent [sender,receiver,owner] [115,116] [("assets",115),("shares",116)]
  , fixture "LeverageOpened" leverageOpenedEvent [owner] [117,118,119,120,121,122]
      [("principal",117),("leverage",118),("loanAmount",119),("tokensReceived",120),("debtIncurred",121),("maxSlippageBps",122)]
  , fixture "LeverageClosed" leverageClosedEvent [owner] [123,124,125,126]
      [("debtRepaid",123),("collateralWithdrawn",124),("usdcReturned",125),("maxSlippageBps",126)]
  , fixture "Morpho Supply" morphoSupplyEvent morphoTopics [127,128] [("assets",127),("shares",128)]
  , ("Morpho Withdraw",morphoWithdrawEvent,morphoTopics,receiver <> encodeUint256 129 <> encodeUint256 130,[("assets",129),("shares",130)])
  , ("Morpho Borrow",morphoBorrowEvent,morphoTopics,receiver <> encodeUint256 131 <> encodeUint256 132,[("assets",131),("shares",132)])
  , fixture "Morpho Repay" morphoRepayEvent morphoTopics [133,134] [("assets",133),("shares",134)]
  ]
 where
  owner = padAddress "0x1111111111111111111111111111111111111111"
  sender = padAddress "0x2222222222222222222222222222222222222222"
  receiver = padAddress "0x3333333333333333333333333333333333333333"
  morphoTopics = [hexToBytes testBearMarketId,sender,owner]
  fixture name event indexed values expected =
    (name,event,indexed,mconcat $ map encodeUint256 values,expected)
