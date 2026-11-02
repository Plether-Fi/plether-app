module Plether.Handlers.TestnetMarginFundingSpec (spec) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Plether.Database.Schema (TestnetFaucetClaimRow (..))
import Plether.Ethereum.Transaction
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Plether.Database.TestnetFunding (FundingClaim (..))
import Plether.Ethereum.Abi (encodeAddress, encodeCall, encodeUint256, keccak256)
import Plether.Ethereum.Rpc (RpcLog (..), TxReceipt (..))
import Plether.Handlers.TestnetMarginFunding (fundingCalldata, fundingReceiptMatches, validFundingId, legacyTransactionMayUseSigner)
import Test.Hspec

spec :: Spec
spec = describe "direct margin funding intent and receipt evidence" $ do
  it "mints to the backend signer and deposits for the beneficiary" $ do
    fundingCalldata claim {fcPhase="mint"} `shouldBe`
      (fcToken claim,encodeCall "mint(address,uint256)" [encodeAddress $ fcSigner claim,encodeUint256 $ fcAmount claim])
    fundingCalldata claim {fcPhase="approval"} `shouldBe`
      (fcToken claim,encodeCall "approve(address,uint256)" [encodeAddress $ fcClearinghouse claim,encodeUint256 $ fcAmount claim])
    fundingCalldata claim {fcPhase="deposit"} `shouldBe`
      (fcClearinghouse claim,encodeCall "depositFor(address,uint256)" [encodeAddress $ fcAddress claim,encodeUint256 $ fcAmount claim])
  it "requires the exact backend mint and approval events" $ do
    fundingReceiptMatches claim "mint" receipt {receiptLogs=[mint]} `shouldBe` True
    fundingReceiptMatches claim "mint" receipt {receiptLogs=[mint {rpcLogTopics=[transferTopic,BS.replicate 32 0,encodeAddress $ fcAddress claim]}]} `shouldBe` False
    fundingReceiptMatches claim "approval" receipt {receiptLogs=[approval]} `shouldBe` True
    fundingReceiptMatches claim "approval" receipt {receiptLogs=[approval {rpcLogData=encodeUint256 $ fcAmount claim+1}]} `shouldBe` False
  it "requires matching custody transfer, beneficiary credit and payer metadata" $
    fundingReceiptMatches claim "deposit" receipt `shouldBe` True
  it "rejects wrong recipients, tokens, payers, amounts and duplicate credits" $ do
    let [deposit,payer,transfer] = receiptLogs receipt
    mapM_ (\logs -> fundingReceiptMatches claim "deposit" receipt {receiptLogs=logs} `shouldBe` False)
      [ [deposit,payer]
      , [deposit,transfer]
      , [deposit,payer,transfer,deposit]
      , [deposit {rpcLogTopics=[depositTopic,encodeAddress $ addr "9",encodeAddress $ fcToken claim]},payer,transfer]
      , [deposit,payer {rpcLogTopics=[payerTopic,encodeAddress $ addr "9",encodeAddress $ fcAddress claim]},transfer]
      , [deposit,payer,transfer {rpcLogAddress=addr "9"}]
      , [deposit {rpcLogData=encodeUint256 1},payer,transfer]
      , [deposit {rpcLogBlockHash="0xwrong"},payer,transfer]
      , [deposit {rpcLogBlockNumber=101},payer,transfer]
      , [deposit {rpcLogTransactionIndex=1},payer,transfer]
      ]
  it "blocks only legacy intents that may consume this signer and chain" $ do
    let key = "0x" <> T.replicate 63 "0" <> "1"
    Right signer <- deriveAddress key
    Right otherSigner <- deriveAddress $ "0x" <> T.replicate 63 "0" <> "2"
    Right signed <- signTransaction key $ Tx1559 421614 3 0 1 100_000 (fcToken claim) 0
      $ encodeCall "mint(address,uint256)" [encodeAddress $ fcAddress claim,encodeUint256 $ fcAmount claim]
    let raw = "0x" <> TE.decodeUtf8 (B16.encode $ signedRawTransaction signed)
        legacy = TestnetFaucetClaimRow (fcAddress claim) (fcAmount claim) (fcToken claim)
          (Just $ signedTransactionHash signed) (Just raw) Nothing "submitted" Nothing
    legacyTransactionMayUseSigner 421614 signer 0 legacy `shouldReturn` True
    legacyTransactionMayUseSigner 421614 otherSigner 0 legacy `shouldReturn` False
    legacyTransactionMayUseSigner 1 signer 0 legacy `shouldReturn` False
    legacyTransactionMayUseSigner 421614 signer 3 legacy `shouldReturn` True
    legacyTransactionMayUseSigner 421614 signer 4 legacy `shouldReturn` False
    legacyTransactionMayUseSigner 421614 signer 0 legacy {tfcRawTx=Nothing} `shouldReturn` True
    legacyTransactionMayUseSigner 421614 signer 0 legacy {tfcRawTx=Just "0x02bad"} `shouldReturn` True
    legacyTransactionMayUseSigner 421614 signer 0 legacy {tfcTxHash=Just txHash} `shouldReturn` True
  it "rejects malformed references before a PostgreSQL UUID cast" $ do
    validFundingId "a6415ba2-5668-4ba5-a058-f627bd1b938d" `shouldBe` True
    map validFundingId ["", "not-a-uuid", "a6415ba2-5668-4ba5-a058-f627bd1b938g", "' OR 1=1 --"] `shouldBe` replicate 4 False
  where
    addr digit = "0x" <> T.replicate 40 digit
    claim = FundingClaim "a6415ba2-5668-4ba5-a058-f627bd1b938d" (addr "1") (addr "2") (addr "3")
      421614 (addr "4") (addr "5") "v2.0.0" 100_000_000_000 "deposit" "pending" Nothing Nothing Nothing
    transferTopic = keccak256 $ TE.encodeUtf8 "Transfer(address,address,uint256)"
    depositTopic = keccak256 $ TE.encodeUtf8 "Deposit(address,address,uint256)"
    payerTopic = keccak256 $ TE.encodeUtf8 "DepositFor(address,address,uint256)"
    event target topics = RpcLog txHash 100 blockHash 0 0 target topics (encodeUint256 $ fcAmount claim)
    mint = event (fcToken claim) [transferTopic,BS.replicate 32 0,encodeAddress $ fcSigner claim]
    approval = event (fcToken claim) [keccak256 $ TE.encodeUtf8 "Approval(address,address,uint256)",encodeAddress $ fcSigner claim,encodeAddress $ fcClearinghouse claim]
    txHash = "0x" <> T.replicate 64 "a"
    blockHash = "0x" <> T.replicate 64 "b"
    receipt = TxReceipt txHash 100 blockHash 0 True
      [ event (fcClearinghouse claim) [depositTopic,encodeAddress $ fcAddress claim,encodeAddress $ fcToken claim]
      , event (fcClearinghouse claim) [payerTopic,encodeAddress $ fcSigner claim,encodeAddress $ fcAddress claim]
      , event (fcToken claim) [transferTopic,encodeAddress $ fcSigner claim,encodeAddress $ fcClearinghouse claim]
      ]
