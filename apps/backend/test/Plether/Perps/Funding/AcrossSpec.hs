module Plether.Perps.Funding.AcrossSpec (spec) where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Either (isLeft)
import Data.Text (Text)
import qualified Data.Text as T
import Numeric (showHex)
import Plether.Perps.Funding.Across
import Plether.Perps.Funding.Types
import Test.Hspec

spec :: Spec
spec = describe "Across stablecoin funding route validation" $ do
  it "accepts only the four documented advisory transfer states" $
    mapM_ (\status -> parseAcrossTransferStatus (object ["status" .= status]) `shouldBe` Right status)
      ["pending","filled","expired","refunded"]
  it "rejects malformed or missing transfer status" $
    mapM_ (\payload -> parseAcrossTransferStatus payload `shouldSatisfy` isLeft)
      [Null,String "filled",object [],object ["status" .= (1 :: Int)],object ["status" .= Null]]
  it "rejects unrecognized transfer states instead of treating them as complete" $
    mapM_ (\status -> parseAcrossTransferStatus (object ["status" .= (status :: Text)]) `shouldSatisfy` isLeft)
      ["complete","success","received","FILLED","filled ",""]
  it "ignores upstream balance and confirmation claims when parsing advisory status" $
    parseAcrossTransferStatus (object ["status" .= ("pending" :: Text),"confirmed" .= True
      ,"depositAmount" .= ("100000000" :: Text)]) `shouldBe` Right "pending"
  it "accepts the canonical simulated USDC route and replaces infinite approval with exact input" $ do
    let expectedApproval = SourceTransaction 1 "approval" ethUsdc
          ("0x095ea7b3" <> address spoke <> uint amount) "0"
    fmap pqTransactions (parse False $ fixture False)
      `shouldBe` Right [expectedApproval,SourceTransaction 1 "bridge" spoke (calldata False) "0"]
  it "accepts a strictly decoded USDT swap and four-call USDC transfer recipe" $ do
    fmap pqMinimumAmount (parse True $ fixture True) `shouldBe` Right (decimal minimumOutput)
  it "uses a zero reset before exact USDT approval when an insufficient nonzero allowance exists" $ do
    let payload = edit ["checks","allowance","actual"] (String "1") $ fixture True
    fmap (map stData . take 2 . pqTransactions) (parse True payload)
      `shouldBe` Right ["0x095ea7b3" <> address periphery <> uint 0,
        "0x095ea7b3" <> address periphery <> uint amount]
  it "does not request another approval when allowance already covers input" $ do
    let payload = edit ["checks","allowance","actual"] (String $ decimal amount) $ fixture False
    fmap (map stKind . pqTransactions) (parse False payload) `shouldBe` Right ["bridge"]
  it "caps usable quote lifetime even when the API supplies a longer expiry" $
    fmap pqExpiresAt (parse False $ fixture False) `shouldBe` Right (now+180)
  it "rejects an unsuccessful or missing source simulation" $ do
    parse False (edit ["swapTx","simulationSuccess"] (Bool False) $ fixture False) `shouldSatisfy` isLeft
    parse False (edit ["swapTx","simulationSuccess"] Null $ fixture False) `shouldSatisfy` isLeft
  it "rejects source chain, target, native value, input amount and input token tampering" $
    mapM_ (\(path,value) -> parse False (edit path value $ fixture False) `shouldSatisfy` isLeft)
      [(["swapTx","chainId"],Number 42161),(["swapTx","to"],String attacker)
      ,(["swapTx","value"],String "1"),(["inputAmount"],String "100000001")
      ,(["maxInputAmount"],String "100000001"),(["inputToken","address"],String ethUsdt)
      ,(["outputToken","address"],String ethUsdc),(["refundToken","chainId"],Number 42161)]
  it "rejects a malicious approval despite an otherwise valid bridge" $ do
    let malicious = object ["chainId" .= (1 :: Int),"to" .= ethUsdc
          ,"data" .= ("0x095ea7b3" <> address attacker <> uint amount)]
    parse False (edit ["approvalTxns"] (toJSON [malicious]) $ fixture False) `shouldSatisfy` isLeft
  it "rejects malformed or executable non-approve approval calldata" $ do
    let malformed = object ["chainId" .= (1 :: Int),"to" .= ethUsdc
          ,"data" .= ("0xa9059cbb" <> address spoke <> uint amount)]
    parse False (edit ["approvalTxns"] (toJSON [malformed]) $ fixture False) `shouldSatisfy` isLeft
  it "rejects zero and noncanonical numeric amounts" $
    mapM_ (\value -> parse False (edit ["minOutputAmount"] (String value) $ fixture False) `shouldSatisfy` isLeft)
      ["0","01","-1","1.0","100000000000000000000000000000000000000000000000000000000000000000000000000000000"]
  it "rejects output claims outside the stablecoin loss policy" $
    parse False (edit ["minOutputAmount"] (String "1") $ fixture False) `shouldSatisfy` isLeft
  it "rejects a recipient, refund address, chain, input or output amount changed only in calldata" $
    mapM_ (\(index,word) -> parse False (withData $ replaceWord index word $ calldata False) `shouldSatisfy` isLeft)
      [(0,address attacker),(1,address attacker),(2,address ethUsdt),(3,address ethUsdc)
      ,(4,uint $ amount+1),(5,uint $ minimumOutput-1),(6,uint 8453)]
  it "rejects stale and relative timestamps, excessive fill deadlines and expired API quotes" $ do
    mapM_ (\(index,word) -> parse False (withData $ replaceWord index word $ calldata False) `shouldSatisfy` isLeft)
      [(8,uint $ now-601),(8,uint 0),(9,uint $ now+86401),(9,uint 7200)]
    parse False (edit ["quoteExpiryTimestamp"] (toJSON now) $ fixture False) `shouldSatisfy` isLeft
  it "rejects noncanonical offsets, unexpected calldata suffixes and truncated ABI" $ do
    parse False (withData $ replaceWord 11 (uint 416) $ calldata False) `shouldSatisfy` isLeft
    parse False (withData $ calldata False <> "00") `shouldSatisfy` isLeft
    parse False (withData $ T.take 50 $ calldata False) `shouldSatisfy` isLeft
  it "accepts the configured integrator tag, but rejects a different tag" $ do
    let raw = T.dropEnd 16 $ calldata False
    parse False (withData $ raw <> "1dc0dedead73c0de") `shouldSatisfy` either (const False) (const True)
    parse False (withData $ raw <> "1dc0debeef73c0de") `shouldSatisfy` isLeft
  it "rejects forged USDT refund recipient, exchange and unbounded input" $
    mapM_ (\(index,word) -> parse True (withSwapData $ replaceWord index word $ calldata True) `shouldSatisfy` isLeft)
      [(5,address attacker),(7,uint $ amount+1),(16,address attacker),(17,address attacker)]
  it "rejects USDT drain recipient/token tampering even when the summary output still matches" $ do
    let drain = "ef8738d3" <> address arbUsdc <> address receiver
    parse True (withSwapData $ T.replace drain ("ef8738d3" <> address arbUsdc <> address attacker) $ calldata True)
      `shouldSatisfy` isLeft
    parse True (withSwapData $ T.replace drain ("ef8738d3" <> address ethUsdc <> address receiver) $ calldata True)
      `shouldSatisfy` isLeft
  it "rejects extra destination calls or arbitrary logger targets and methods" $ do
    parse True (withSwapData $ T.replace (address logger) (address attacker) $ calldata True) `shouldSatisfy` isLeft
    parse True (withSwapData $ T.replace "d836083e" "a9059cbb" $ calldata True) `shouldSatisfy` isLeft
    -- Message starts at outer word25; its array length is word28.
    parse True (withSwapData $ replaceWord 28 (uint 5) $ calldata True) `shouldSatisfy` isLeft
  it "rejects routing to a different destination release or unsupported source chain/token" $ do
    parseAcrossQuote now "0xdead" (deployment {fdChainId=421614}) (request False) receiver (fixture False) `shouldSatisfy` isLeft
    parseAcrossQuote now "0xdead" deployment ((request False) {qrSourceChainId=10}) receiver (fixture False) `shouldSatisfy` isLeft
    parseAcrossQuote now "0xdead" deployment ((request False) {qrSourceToken=attacker}) receiver (fixture False) `shouldSatisfy` isLeft

now, amount, minimumOutput :: Integer
now = 1791464000
amount = 100000000
minimumOutput = 99400000

ethUsdc, ethUsdt, arbUsdc, spoke, periphery, handler, logger, owner, receiver, attacker :: Text
ethUsdc = "0xa0b86991c6218b36c1d19d4a2e9eb0ce3606eb48"
ethUsdt = "0xdac17f958d2ee523a2206206994597c13d831ec7"
arbUsdc = "0xaf88d065e77c8cc2239327c5edb3a432268e5831"
spoke = "0x5c7bcd6e7de5423a257d81b442095a1a6ced35c5"
periphery = "0x97ccdbea4632140639ad5ea9b944aa034eb15fd4"
handler = "0x0f7ae28de1c8532170ad4ee566b5801485c13a0e"
logger = "0xbf75133b48b0a42ab9374027902e83c5e2949034"
owner = "0x1111111111111111111111111111111111111111"
receiver = "0x2222222222222222222222222222222222222222"
attacker = "0x3333333333333333333333333333333333333333"

deployment :: FundingDeployment
deployment = FundingDeployment
  { fdChainId=42161,fdReleaseId="release",fdClearinghouse=owner,fdToken=arbUsdc,fdFactory=receiver
  , fdFactoryCodeHash="0x" <> T.replicate 64 "1",fdConfirmations=12,fdStartBlock=0
  , fdClearinghouseCodeHash="0x" <> T.replicate 64 "2" }
request :: Bool -> QuoteRequest
request swap = QuoteRequest receiver owner 1 (if swap then ethUsdt else ethUsdc) (decimal amount)
parse :: Bool -> Value -> Either Text ProviderQuote
parse swap = parseAcrossQuote now "0xdead" deployment (request swap) receiver
withData, withSwapData :: Text -> Value
withData value = edit ["swapTx","data"] (String value) $ fixture False
withSwapData value = edit ["swapTx","data"] (String value) $ fixture True

fixture :: Bool -> Value
fixture swap = object
  [ "crossSwapType" .= (if swap then "anyToBridgeable" else "bridgeableToBridgeable" :: Text)
  , "amountType" .= ("exactInput" :: Text)
  , "checks" .= object
    [ "allowance" .= object ["token" .= token,"spender" .= target,"actual" .= ("0" :: Text),"expected" .= decimal amount]
    , "balance" .= object ["token" .= token,"actual" .= decimal amount,"expected" .= decimal amount] ]
  , "inputToken" .= tokenJson 1 token, "outputToken" .= tokenJson 42161 arbUsdc
  , "refundToken" .= tokenJson 1 ethUsdc
  , "inputAmount" .= decimal amount,"maxInputAmount" .= decimal amount
  , "expectedOutputAmount" .= (if swap then "99900000" else decimal minimumOutput),"minOutputAmount" .= decimal minimumOutput
  , "approvalTxns" .= [object ["chainId" .= (1 :: Int),"to" .= token
    ,"data" .= ("0x095ea7b3" <> address target <> uint (2^(256 :: Int)-1))]]
  , "swapTx" .= object ["chainId" .= (1 :: Int),"simulationSuccess" .= True
    ,"to" .= target,"data" .= calldata swap,"gas" .= ("500000" :: Text)]
  , "quoteExpiryTimestamp" .= (now+3600),"id" .= ("test-quote-id" :: Text) ]
 where
  token = if swap then ethUsdt else ethUsdc
  target = if swap then periphery else spoke
  tokenJson chain tokenAddress = object
    ["chainId" .= (chain :: Integer),"address" .= tokenAddress,"decimals" .= (6 :: Int)]

calldata :: Bool -> Text
calldata False = "0xad5425c6" <> T.concat
  [address owner,address receiver,address ethUsdc,address arbUsdc,uint amount,uint minimumOutput
  ,uint 42161,uint 0,uint now,uint (now+7200),uint 0,uint 384,uint 0] <> "1dc0dedead73c0de"
calldata True = "0x110560ad" <> uint 32 <> T.concat
  [uint 0,uint 0,uint 384,address ethUsdt,address "0x66a9893cc07d91d95644aedd05d03f95e1dba8af"
  ,uint 1,uint amount,uint amount,uint (384 + fromIntegral (T.length deposit `div` 2))
  ,uint 1,address spoke,uint 0] <> deposit <> bytes "24856bc3" <> "1dc0dedead73c0de"
 where
  deposit = T.concat [address ethUsdc,address arbUsdc,uint minimumOutput,address owner,address handler
    ,uint 42161,uint 0,uint now,uint (now+7200),uint 0,uint 352] <> bytes destinationMessage

destinationMessage :: Text
destinationMessage = uint 32 <> uint 64 <> uint 0 <> uint 4 <> T.concat offsets <> T.concat calls
 where
  drain = call handler $ "ef8738d3" <> address arbUsdc <> address receiver
  logCall = call logger $ "d836083e" <> uint 32 <> bytes "abcd"
  calls = [drain,drain,logCall,logCall]
  offsets = map uint $ take 4 $ scanl (+) 128 $ map (fromIntegral . (`div` 2) . T.length) calls
  call target callData = address target <> uint 96 <> uint 0 <> bytes callData

address :: Text -> Text
address value = T.replicate 24 "0" <> T.drop 2 value
uint :: Integer -> Text
uint value = let raw = T.pack $ showHex value "" in T.replicate (64-T.length raw) "0" <> raw
bytes :: Text -> Text
bytes raw = uint (fromIntegral $ T.length raw `div` 2) <> raw <> T.replicate ((64-T.length raw `mod` 64) `mod` 64) "0"
decimal :: Integer -> Text
decimal = T.pack . show
replaceWord :: Int -> Text -> Text -> Text
replaceWord index replacement value = T.take (10+64*index) value <> replacement <> T.drop (10+64*(index+1)) value
edit :: [Key] -> Value -> Value -> Value
edit [] replacement _ = replacement
edit [key] replacement (Object value) = Object $ KM.insert key replacement value
edit (key:rest) replacement (Object value) = Object $ case KM.lookup key value of
  Just previous -> KM.insert key (edit rest replacement previous) value
  Nothing -> value
edit _ _ value = value
