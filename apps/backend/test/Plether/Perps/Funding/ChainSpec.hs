module Plether.Perps.Funding.ChainSpec (spec) where

import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import Data.Text (Text)
import qualified Data.Text as T
import Plether.Ethereum.Abi
import Plether.Ethereum.Rpc
import Plether.Perps.Funding.Across (buildAcrossDestinationMessage)
import Plether.Perps.Funding.Chain
import Plether.Perps.Funding.Types
import Test.Hspec

spec :: Spec
spec = do
  describe "canonical Across source relay" $ do
    it "binds a confirmed deposit to its exact marker-bearing message" $ do
      fmap (>>= fieldText "depositId") sourceProof `shouldBe` Right (Just "7")
      fmap (>>= fieldText "recipient") sourceProof `shouldBe` Right (Just handler)
    it "matches an independent viem V3RelayData and destination-chain hash vector" $ do
      -- viem encodeAbiParameters((bytes32,bytes32,bytes32,bytes32,bytes32,
      -- uint256,uint256,uint256,uint256,uint32,uint32,bytes),uint256).
      fmap (>>= fieldText "messageHash") sourceProof `shouldBe` Right (Just "0x7f17cc4dc1d7f69ef8f9a7a5b3197433f227d248fa2f6293c2649d7d70c8196e")
      fmap (>>= fieldText "relayHash") sourceProof `shouldBe` Right (Just "0xcfa881d2faaf3dd55d76bb874c856820ed1d78d2a7b1d954b3f37766133f6c3f")
    it "does not accept a pending, reverted or noncanonical source receipt" $ do
      sourceDepositProof deployment intent 10 block sourceReceipt `shouldBe` Right Nothing
      sourceDepositProof deployment intent 11 block (sourceReceipt {receiptSucceeded=False}) `shouldBe` Right Nothing
      sourceDepositProof deployment intent 11 (block {rpcBlockHash=otherHash}) sourceReceipt `shouldSatisfy` isLeft
    it "cannot attribute the same beneficiary's other quoted message" $
      sourceDepositProof deployment (setFields [("destinationMessage",Data.Aeson.String "0x01")] intent) 11 block sourceReceipt `shouldBe` Right Nothing
    it "requires the allowlisted source pool, owner and destination chain" $
      mapM_ (\entry -> sourceDepositProof deployment intent 11 block (sourceReceipt {receiptLogs=[entry]}) `shouldBe` Right Nothing)
        [sourceLog {rpcLogAddress=handler},sourceLog {rpcLogTopics=[fundsDepositedTopic,encodeUint256 1,encodeUint256 7,encodeAddress owner]}
        ,sourceLog {rpcLogTopics=[fundsDepositedTopic,encodeUint256 42161,encodeUint256 7,encodeAddress account]}]
    it "rejects malformed dynamic data and under-minimum output" $ do
      sourceDepositProof deployment intent 11 block (sourceReceipt {receiptLogs=[sourceLog {rpcLogData=BS.take 400 $ rpcLogData sourceLog}]}) `shouldSatisfy` isLeft
      sourceDepositProof deployment intent 11 block (sourceReceipt {receiptLogs=[sourceLog {rpcLogData=replaceWord 3 (encodeUint256 1) $ rpcLogData sourceLog}]}) `shouldSatisfy` isLeft
    it "rejects duplicate source events instead of choosing a relay" $
      sourceDepositProof deployment intent 11 block (sourceReceipt {receiptLogs=[sourceLog,sourceLog {rpcLogIndex=1}]}) `shouldSatisfy` isLeft
  describe "canonical direct margin action" $ do
    it "accepts an exactly matched fill, deposit pair, transfer and quote marker" $
      fmap (>>= fieldText "creditedAmount") (proof successReceipt) `shouldBe` Right (Just "1000000")
    it "never credits a shared-handler deposit without its matching relay fill" $
      proof (successReceipt {receiptLogs=tail $ receiptLogs successReceipt}) `shouldBe` Right Nothing
    it "does not credit another deposit ID or origin chain" $ do
      proof (withFill $ fillLog {rpcLogTopics=[filledRelayTopic,encodeUint256 1,encodeUint256 8,encodeAddress owner]}) `shouldBe` Right Nothing
      proof (withFill $ fillLog {rpcLogTopics=[filledRelayTopic,encodeUint256 2,encodeUint256 7,encodeAddress owner]}) `shouldBe` Right Nothing
    it "ignores a historical relay with the same deposit ID but different original fields" $
      mapM_ (\index -> proof (withFill $ fillLog {rpcLogData=replaceWord index (BS.replicate 32 9) $ rpcLogData fillLog}) `shouldBe` Right Nothing)
        [0,1,2,3,5,6,7,8,9,10]
    it "rejects changed updated execution fields for the matching original relay" $
      mapM_ (\index -> proof (withFill $ fillLog {rpcLogData=replaceWord index (BS.replicate 32 9) $ rpcLogData fillLog}) `shouldSatisfy` isLeft)
        [11,12,13,14]
    it "accepts all three fill types with unchanged reviewed economics" $
      mapM_ (\kind -> fmap (>>= fieldText "status") (proof $ withFill $ fillLog {rpcLogData=replaceWord 14 (encodeUint256 kind) $ rpcLogData fillLog}) `shouldBe` Right (Just "confirmed")) [0,1,2]
    it "requires the canonical event pair, actual token transfer and exact marker" $ do
      mapM_ (\index -> proof (successReceipt {receiptLogs=filter ((/= index) . rpcLogIndex) $ receiptLogs successReceipt}) `shouldSatisfy` isLeft) [1,2,3,4]
      proof (successReceipt {receiptLogs=[fillLog,tokenTransfer 1 handler clearinghouse 999999,canonical 2 1000000,credit 3 1000000,marker 4]}) `shouldSatisfy` isLeft
    it "does not borrow another fill's callback events from the same transaction" $
      proof (successReceipt {receiptLogs=fillLog:(fillLog {rpcLogIndex=1,rpcLogTopics=[filledRelayTopic,encodeUint256 1,encodeUint256 8,encodeAddress owner]}):map (\entry -> entry {rpcLogIndex=rpcLogIndex entry+1}) (tail $ receiptLogs successReceipt)}) `shouldSatisfy` isLeft
    it "requires confirmation depth and canonical receipt identity" $ do
      depositProof deployment intent relay 10 block successReceipt `shouldBe` Right Nothing
      depositProof deployment intent relay 11 (block {rpcBlockHash=otherHash}) successReceipt `shouldSatisfy` isLeft
      proof (successReceipt {receiptSucceeded=False}) `shouldBe` Right Nothing
      proof (successReceipt {receiptTxHash=otherHash}) `shouldBe` Right Nothing
    it "rejects duplicate log indices and duplicate matching fills" $ do
      proof (successReceipt {receiptLogs=receiptLogs successReceipt <> [marker 4]}) `shouldSatisfy` isLeft
      proof (successReceipt {receiptLogs=receiptLogs successReceipt <> [fillLog {rpcLogIndex=5}]}) `shouldSatisfy` isLeft
  describe "canonical fallback delivery" $ do
    it "requires failed actions and actual USDC delivery to the beneficiary" $ do
      fmap (>>= fieldText "status") (proof fallbackReceipt) `shouldBe` Right (Just "needs-deposit")
      fmap (>>= fieldText "fallbackAmount") (proof fallbackReceipt) `shouldBe` Right (Just "1000000")
      fmap (>>= fieldText "creditedAmount") (proof fallbackReceipt) `shouldBe` Right (Just "0")
    it "records the actual fallback balance including unsolicited dust" $ do
      let receipt = fallbackReceipt {receiptLogs=[fillLog,failed 1,tokenTransfer 2 handler account 1000037,drained 3 account 1000037]}
      fmap (>>= fieldText "fallbackAmount") (proof receipt) `shouldBe` Right (Just "1000037")
      proof (receipt {receiptLogs=[fillLog,failed 1,tokenTransfer 2 handler account 1000000,drained 3 account 1000037]}) `shouldSatisfy` isLeft
    it "does not infer fallback merely from a fill without deposit events" $
      proof (fallbackReceipt {receiptLogs=[fillLog]}) `shouldSatisfy` isLeft
    it "rejects a wrong fallback recipient, missing transfer, or wrong failure message" $ do
      proof (fallbackReceipt {receiptLogs=[fillLog,failed 1,tokenTransfer 2 handler owner 1000000,drained 3 account 1000000]}) `shouldSatisfy` isLeft
      proof (fallbackReceipt {receiptLogs=[fillLog,failed 1,drained 3 account 1000000]}) `shouldSatisfy` isLeft
      proof (fallbackReceipt {receiptLogs=[fillLog,(failed 1) {rpcLogData="bad"},tokenTransfer 2 handler account 1000000,drained 3 account 1000000]}) `shouldSatisfy` isLeft
    it "does not reclassify fallback as ready after a later unrelated handler deposit" $
      fmap (>>= fieldText "status") (proof $ fallbackReceipt {receiptLogs=receiptLogs fallbackReceipt <>
        [tokenTransfer 4 handler clearinghouse 1000000,canonical 5 1000000,credit 6 1000000,marker 7]}) `shouldBe` Right (Just "needs-deposit")
  describe "funding public evidence freshness" $ do
    it "hides stale terminal outcomes on every API response path" $
      mapM_ (\status -> do
        let value = setFields [("status",Data.Aeson.String status),("creditedAmount",Data.Aeson.String "1000000"),("lastCheckedAt",Data.Aeson.Number 100)] intent
        fieldText "status" (publicObservedIntent 161 True value) `shouldBe` Just "bridging"
        fieldText "creditedAmount" (publicObservedIntent 161 True value) `shouldBe` Just "0"
        fieldText "status" (publicObservedIntent 101 False value) `shouldBe` Just "bridging"
        fieldText "status" (publicObservedIntent 101 True value) `shouldBe` Just status) ["confirmed","needs-deposit"]
    it "rejects missing or future observation timestamps" $
      mapM_ (\checked -> fieldText "status" (publicObservedIntent 100 True $ setFields
        [("status",Data.Aeson.String "confirmed"),("lastCheckedAt",checked)] intent) `shouldBe` Just "bridging")
        [Data.Aeson.Null,Data.Aeson.Number 101]
  describe "funding release/input boundaries" $ do
    it "matches the active trading release and isolates observer readiness" $ do
      validateFundingReleaseBinding 42161 clearinghouse token deployment `shouldBe` Right deployment
      validateFundingReleaseBinding 421614 clearinghouse token deployment `shouldSatisfy` isLeft
      validateFundingReleaseBinding 42161 account token deployment `shouldSatisfy` isLeft
      deploymentReadinessKey deployment `shouldNotBe` deploymentReadinessKey (deployment {fdDestinationSpokePoolImplementationCodeHash=otherHash})
    it "rejects noncanonical amounts and destination addresses" $ do
      mapM_ (\raw -> validateAmount raw `shouldSatisfy` isLeft) ["0","01","-1","1.0","1e6"," 1","0x1","115792089237316195423570985008687907853269984665640564039457584007913129639936"]
      mapM_ (\raw -> validateAddress raw `shouldSatisfy` isLeft) ["0x0000000000000000000000000000000000000000","1111111111111111111111111111111111111111","0x1234"]
  where
    proof = depositProof deployment intent relay 11 block
    sourceProof = sourceDepositProof deployment intent 11 block sourceReceipt
    withFill entry = successReceipt {receiptLogs=entry:tail (receiptLogs successReceipt)}

account, owner, handler, token, clearinghouse, spoke, implementation, txHash, blockHash, otherHash, quoteId, emitter, sourceToken :: Text
account="0x1111111111111111111111111111111111111111"
owner="0x2222222222222222222222222222222222222222"
handler="0x0f7ae28de1c8532170ad4ee566b5801485c13a0e"
token="0xaf88d065e77c8cc2239327c5edb3a432268e5831"
clearinghouse="0x4444444444444444444444444444444444444444"
spoke="0x5555555555555555555555555555555555555555"
implementation="0x6666666666666666666666666666666666666666"
emitter="0xbf75133b48b0a42ab9374027902e83c5e2949034"
sourceToken="0xa0b86991c6218b36c1d19d4a2e9eb0ce3606eb48"
txHash=encodeHex $ BS.replicate 32 1
blockHash=encodeHex $ BS.replicate 32 2
otherHash=encodeHex $ BS.replicate 32 3
quoteId=encodeHex $ BS.replicate 32 4
deployment :: FundingDeployment
deployment=FundingDeployment 42161 "test-release" clearinghouse token spoke blockHash implementation blockHash handler blockHash 2 0 blockHash
message :: BS.ByteString
message=either (error . T.unpack) id $ decodeHex $ buildAcrossDestinationMessage deployment (QuoteRequest account owner 1 sourceToken "1000000") quoteId
intent :: Value
intent=object ["beneficiary" .= account,"ownerAddress" .= owner,"quoteId" .= quoteId,"minimumAmount" .= ("1000000" :: Text),"destinationMessage" .= encodeHex message]
block :: RpcBlock
block=RpcBlock 10 Nothing blockHash 100
sourceReceipt, successReceipt, fallbackReceipt :: TxReceipt
sourceReceipt=TxReceipt txHash 10 blockHash 0 True [sourceLog]
successReceipt=TxReceipt txHash 10 blockHash 0 True [fillLog,tokenTransfer 1 handler clearinghouse 1000000,canonical 2 1000000,credit 3 1000000,marker 4]
fallbackReceipt=TxReceipt txHash 10 blockHash 0 True [fillLog,failed 1,tokenTransfer 2 handler account 1000000,drained 3 account 1000000]
relay :: Value
relay=case sourceDepositProof deployment intent 11 block sourceReceipt of Right (Just value) -> value; other -> error $ show other
logEntry :: Integer -> Text -> [BS.ByteString] -> BS.ByteString -> RpcLog
logEntry index address topics dat=RpcLog txHash 10 blockHash 0 index address topics dat
sourceLog, fillLog :: RpcLog
sourceLog=logEntry 0 originSpokePool [fundsDepositedTopic,encodeUint256 42161,encodeUint256 7,encodeAddress owner] $ BS.concat
  [encodeAddress sourceToken,encodeAddress token,encodeUint256 1000000,encodeUint256 1000000,encodeUint256 90
  ,encodeUint256 1000,encodeUint256 0,encodeAddress handler,BS.replicate 32 0,encodeUint256 320,dynamic message]
fillLog=logEntry 0 spoke [filledRelayTopic,encodeUint256 1,encodeUint256 7,encodeAddress owner] $ BS.concat
  [encodeAddress sourceToken,encodeAddress token,encodeUint256 1000000,encodeUint256 1000000,encodeUint256 1
  ,encodeUint256 1000,encodeUint256 0,BS.replicate 32 0,encodeAddress owner,encodeAddress handler,keccak256 message
  ,encodeAddress handler,keccak256 message,encodeUint256 1000000,encodeUint256 0]
canonical, credit :: Integer -> Integer -> RpcLog
canonical index amount=logEntry index clearinghouse [keccak256 "Deposit(address,address,uint256)",encodeAddress account,encodeAddress token] $ encodeUint256 amount
credit index amount=logEntry index clearinghouse [depositForTopic,encodeAddress handler,encodeAddress account] $ encodeUint256 amount
marker, failed :: Integer -> RpcLog
marker index=logEntry index emitter [keccak256 "MetadataEmitted(bytes)"] $ encodeUint256 32 <> dynamic (BS.replicate 32 4)
failed index=logEntry index handler [keccak256 "CallsFailed((address,bytes,uint256)[],address)",encodeAddress account] $ encodeUint256 32 <> BS.drop 96 message
tokenTransfer :: Integer -> Text -> Text -> Integer -> RpcLog
tokenTransfer index from to amount=logEntry index token [keccak256 "Transfer(address,address,uint256)",encodeAddress from,encodeAddress to] $ encodeUint256 amount
drained :: Integer -> Text -> Integer -> RpcLog
drained index recipient amount=logEntry index handler [keccak256 "DrainedTokens(address,address,uint256)",encodeAddress recipient,encodeAddress token,encodeUint256 amount] BS.empty
dynamic :: BS.ByteString -> BS.ByteString
dynamic bytes=encodeUint256 (fromIntegral $ BS.length bytes) <> bytes <> BS.replicate ((32-BS.length bytes `mod` 32) `mod` 32) 0
replaceWord :: Int -> BS.ByteString -> BS.ByteString -> BS.ByteString
replaceWord index replacement bytes=BS.take (index*32) bytes <> replacement <> BS.drop ((index+1)*32) bytes
