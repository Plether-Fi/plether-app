module Plether.Insights.DivisionsFundingSpec (spec) where

import qualified Data.ByteString as BS
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Plether.Ethereum.Abi (encodeAddress, encodeUint256, keccak256)
import Plether.Ethereum.Rpc (RpcLog (..), TxReceipt (..))
import Plether.Insights.DivisionsFunding
import Test.Hspec

spec :: Spec
spec = describe "Divisions depositFor receipt proof" $ do
  it "binds the beneficiary's one deposit to the payer and custody transfer" $
    decodeDepositPayer flow receipt `shouldBe` Just (DepositPayerProof payer 4)
  it "supports self-funded depositFor without creating another cash flow" $ do
    let logs = creditLogs account account 4
    decodeDepositPayer flow receipt {receiptLogs=logs}
      `shouldBe` Just (DepositPayerProof account 4)
  it "binds equal deposits in the same receipt to separate transfer logs" $ do
    let twice = receipt {receiptLogs=creditLogs payer account 4 <> creditLogs other account 7}
    decodeDepositPayer flow twice `shouldBe` Just (DepositPayerProof payer 4)
    decodeDepositPayer flow {ffLogIndex=8} twice `shouldBe` Just (DepositPayerProof other 7)
    decodeDepositPayer flow twice {receiptLogs=creditLogs payer account 4 <> creditLogs other other 7}
      `shouldBe` Just (DepositPayerProof payer 4)
  it "rejects reverted receipts and different transaction, block or transaction-index identities" $ do
    mapM_ (\value -> decodeDepositPayer flow value `shouldSatisfy` isNothing)
      [ receipt {receiptSucceeded=False}
      , receipt {receiptTxHash=hash "c"}
      , receipt {receiptBlockHash=hash "c"}
      , receipt {receiptBlockNumber=101}
      , receipt {receiptTransactionIndex=1}
      ]
  it "rejects missing, duplicate and displaced evidence" $ do
    let logs = receiptLogs receipt
    mapM_ (\value -> decodeDepositPayer flow receipt {receiptLogs=value} `shouldSatisfy` isNothing)
      [drop 1 logs, take 2 logs, [head logs,last logs], logs <> [head logs],
       map (\l -> l {rpcLogIndex=rpcLogIndex l+1}) logs]
  it "rejects wrong beneficiaries, payers, tokens, clearinghouses, amounts and log identities" $ do
    let replace index f = receipt {receiptLogs=map (\l -> if rpcLogIndex l==index then f l else l) (receiptLogs receipt)}
        variants =
          [ replace 5 (\l -> l {rpcLogTopics=[topic "Deposit(address,address,uint256)",encodeAddress other,encodeAddress token]})
          , replace 5 (\l -> l {rpcLogTopics=[topic "Deposit(address,address,uint256)",encodeAddress account,encodeAddress other]})
          , replace 6 (\l -> l {rpcLogTopics=[topic "DepositFor(address,address,uint256)",encodeAddress other,encodeAddress account]})
          , replace 6 (\l -> l {rpcLogTopics=[topic "DepositFor(address,address,uint256)",encodeAddress payer,encodeAddress other]})
          , replace 6 (\l -> l {rpcLogTopics=[topic "DepositFor(address,address,uint256)",BS.replicate 32 0,encodeAddress account]})
          , replace 6 (\l -> l {rpcLogTopics=[topic "DepositFor(address,address,uint256)",BS.replicate 32 1,encodeAddress account]})
          , replace 4 (\l -> l {rpcLogTopics=[topic "Transfer(address,address,uint256)",encodeAddress payer,encodeAddress other]})
          ] <> [replace i f | i <- [4,5,6], f <-
            [ \l -> l {rpcLogAddress=other}
            , \l -> l {rpcLogData=encodeUint256 (amount+1)}
            , \l -> l {rpcLogTxHash=hash "c"}
            , \l -> l {rpcLogBlockHash=hash "c"}
            , \l -> l {rpcLogBlockNumber=101}
            , \l -> l {rpcLogTransactionIndex=1}
            ]]
    mapM_ (\value -> decodeDepositPayer flow value `shouldSatisfy` isNothing) variants
  it "never uses payer metadata to authorize a withdrawal, zero amount or wrong indexed credit" $ do
    mapM_ (\value -> decodeDepositPayer value receipt `shouldSatisfy` isNothing)
      [flow {ffKind="Withdraw"},flow {ffAmount="0"},flow {ffAmount="-1"},flow {ffAmount="1.5"},
       flow {ffAmount=T.pack $ show (2^(256 :: Int) :: Integer)},flow {ffLogIndex=0},
       flow {ffAccount=other},flow {ffEmitter=other},flow {ffAsset=other},flow {ffAmount="1"}]

flow :: FundingFlow
flow = FundingFlow account "Deposit" (T.pack $ show amount) clearinghouse token
  (hash "a") 100 (hash "b") 0 5 0 False False

receipt :: TxReceipt
receipt = TxReceipt (hash "a") 100 (hash "b") 0 True (creditLogs payer account 4)

creditLogs :: Text -> Text -> Integer -> [RpcLog]
creditLogs sender beneficiary index =
  [ event index token [topic "Transfer(address,address,uint256)",encodeAddress sender,encodeAddress clearinghouse]
  , event (index+1) clearinghouse [topic "Deposit(address,address,uint256)",encodeAddress beneficiary,encodeAddress token]
  , event (index+2) clearinghouse [topic "DepositFor(address,address,uint256)",encodeAddress sender,encodeAddress beneficiary]
  ]
  where
    event logIndex emitter topics = RpcLog (hash "a") 100 (hash "b") 0 logIndex emitter topics (encodeUint256 amount)

topic :: Text -> BS.ByteString
topic = keccak256 . TE.encodeUtf8

amount :: Integer
amount = 100_000_000_000

account, payer, clearinghouse, token, other :: Text
account = "0x" <> T.replicate 40 "1"
payer = "0x" <> T.replicate 40 "2"
clearinghouse = "0x" <> T.replicate 40 "3"
token = "0x" <> T.replicate 40 "4"
other = "0x" <> T.replicate 40 "5"

hash :: Text -> Text
hash digit = "0x" <> T.replicate 64 digit
