module Plether.Perps.Funding.ChainSpec (spec) where

import Data.Aeson (Value, object, (.=))
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import Data.Text (Text)
import Plether.AA.Kms (PaymasterSigner (..))
import Plether.Ethereum.Abi
import Plether.Ethereum.Rpc
import Plether.Perps.Funding.Chain
import Plether.Perps.Funding.Types
import Plether.Perps.Funding.Worker (shouldFlushBalance, verifyFundingSigner)
import Test.Hspec

spec :: Spec
spec = do
  describe "canonical bridge funding proof" $ do
    it "accepts a finalized, exactly bound event pair" $
      fmap (>>= fieldText "creditedAmount") (proof receipt) `shouldBe` Right (Just "1000000")
    it "does not confirm from a receipt before destination confirmations" $
      depositProof deployment intent 10 block receipt `shouldBe` Right Nothing
    it "retracts noncanonical block evidence" $
      depositProof deployment intent 11 (block {rpcBlockHash = otherHash}) receipt `shouldSatisfy` isLeft
    it "rejects a revert even if a node reports event-looking logs" $
      proof (receipt {receiptSucceeded = False}) `shouldBe` Right Nothing
    it "ignores an unrelated payer's deposit" $
      proof (receipt {receiptLogs = [canonical 0 1000000,metadata 1 account 1000000]}) `shouldBe` Right Nothing
    it "requires the canonical Deposit immediately before DepositFor" $
      proof (receipt {receiptLogs = [canonical 0 1000000,metadata 2 receiver 1000000]}) `shouldSatisfy` isLeft
    it "requires exact token and amount on the canonical Deposit" $ do
      proof (receipt {receiptLogs = [(canonical 0 999999),metadata 1 receiver 1000000]}) `shouldSatisfy` isLeft
      let wrongToken = (canonical 0 1000000) {rpcLogTopics = [keccak256 "Deposit(address,address,uint256)",encodeAddress account,encodeAddress receiver]}
      proof (receipt {receiptLogs = [wrongToken,metadata 1 receiver 1000000]}) `shouldSatisfy` isLeft
    it "aggregates multiple valid flushes in one transaction exactly once" $
      fmap (>>= fieldText "creditedAmount") (proof $ receipt {receiptLogs =
        [canonical 0 400000,metadata 1 receiver 400000,canonical 2 600000,metadata 3 receiver 600000]})
        `shouldBe` Right (Just "1000000")
    it "rejects duplicate log indices instead of counting twice" $
      proof (receipt {receiptLogs = receiptLogs receipt <> receiptLogs receipt}) `shouldSatisfy` isLeft
    it "does not credit a forged receipt identity or emitting address" $ do
      proof (receipt {receiptTxHash = otherHash}) `shouldBe` Right Nothing
      proof (receipt {receiptLogs = map (\entry -> entry {rpcLogAddress = receiver}) $ receiptLogs receipt}) `shouldBe` Right Nothing
  describe "receiver balance reconciliation" $ do
    it "waits for the route minimum on a first partial transfer" $
      shouldFlushBalance 100 0 40 `shouldBe` False
    it "counts already credited partial transfers toward the minimum" $
      shouldFlushBalance 100 40 60 `shouldBe` True
    it "sweeps late arrivals after the quote was already fulfilled" $
      shouldFlushBalance 100 100 1000000 `shouldBe` True
    it "does not sponsor repeated dust after fulfillment" $
      shouldFlushBalance 100 100 1 `shouldBe` False
    it "does not pay gas for an empty receiver" $
      shouldFlushBalance 100 100 0 `shouldBe` False
  describe "funding executor readiness" $ do
    it "rejects a KMS key with GetPublicKey but without Sign permission" $
      verifyFundingSigner (PaymasterSigner account (\_ -> pure $ Left "AccessDenied"))
        `shouldReturn` Left "DESTINATION_SIGNER_UNAVAILABLE"
    it "rejects a malformed Sign response" $
      verifyFundingSigner (PaymasterSigner account (\_ -> pure $ Right "not-a-signature"))
        `shouldReturn` Left "DESTINATION_SIGNER_UNAVAILABLE"
  describe "startup trading release binding" $ do
    it "accepts the exact application chain, clearinghouse, and token" $
      validateFundingReleaseBinding 42161 clearinghouse token deployment `shouldBe` Right deployment
    it "rejects funding into a different chain, clearinghouse, or token" $ do
      validateFundingReleaseBinding 421614 clearinghouse token deployment `shouldSatisfy` isLeft
      validateFundingReleaseBinding 42161 account token deployment `shouldSatisfy` isLeft
      validateFundingReleaseBinding 42161 clearinghouse account deployment `shouldSatisfy` isLeft
  describe "strict funding inputs" $ do
    it "binds executor readiness to exact deployment evidence, not only release label" $
      deploymentReadinessKey deployment `shouldNotBe` deploymentReadinessKey (deployment {fdClearinghouseCodeHash = otherHash})
    it "rejects noncanonical or overflowing amounts" $
      mapM_ (\raw -> validateAmount raw `shouldSatisfy` isLeft) ["0","01","-1","1.0","1e6"," 1","0x1","115792089237316195423570985008687907853269984665640564039457584007913129639936"]
    it "rejects zero/unprefixed/wrong-length destination addresses" $
      mapM_ (\raw -> validateAddress raw `shouldSatisfy` isLeft) ["0x0000000000000000000000000000000000000000","1111111111111111111111111111111111111111","0x1234"]
  where
    proof = depositProof deployment intent 11 block

account, receiver, token, clearinghouse, factory, txHash, blockHash, otherHash :: Text
account = "0x1111111111111111111111111111111111111111"
receiver = "0x2222222222222222222222222222222222222222"
token = "0x3333333333333333333333333333333333333333"
clearinghouse = "0x4444444444444444444444444444444444444444"
factory = "0x5555555555555555555555555555555555555555"
txHash = encodeHex $ BS.replicate 32 1
blockHash = encodeHex $ BS.replicate 32 2
otherHash = encodeHex $ BS.replicate 32 3
deployment :: FundingDeployment
deployment = FundingDeployment 42161 "test-release" clearinghouse token factory blockHash 2 0 blockHash
intent :: Value
intent = object ["beneficiary" .= account,"receiver" .= receiver]
block :: RpcBlock
block = RpcBlock 10 Nothing blockHash 100
receipt :: TxReceipt
receipt = TxReceipt txHash 10 blockHash 0 True [canonical 0 1000000,metadata 1 receiver 1000000]
canonical :: Integer -> Integer -> RpcLog
canonical index amount = RpcLog txHash 10 blockHash 0 index clearinghouse
  [keccak256 "Deposit(address,address,uint256)",encodeAddress account,encodeAddress token] (encodeUint256 amount)
metadata :: Integer -> Text -> Integer -> RpcLog
metadata index payer amount = RpcLog txHash 10 blockHash 0 index clearinghouse
  [depositForTopic,encodeAddress payer,encodeAddress account] (encodeUint256 amount)
