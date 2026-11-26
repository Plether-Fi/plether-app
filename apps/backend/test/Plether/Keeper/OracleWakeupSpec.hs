module Plether.Keeper.OracleWakeupSpec (spec) where

import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import Plether.Ethereum.Abi (encodeAddress, encodeUint256)
import Plether.Ethereum.Contracts.Perps (orderFinalizedTopic)
import Plether.Ethereum.Rpc (RpcLog (..), TxReceipt (..))
import Plether.Keeper.OracleWakeup (receiptMayAdvanceOracleMark)
import Test.Hspec

spec :: Spec
spec = describe "keeper oracle wake-up receipt" $ do
  it "accepts successful historical orders in live and FAD modes" $ do
    eligible receipt `shouldBe` True
    eligible (withEntry $ setWord 11 2 finalized) `shouldBe` True

  it "ignores reverted transactions, failed orders and frozen settlements" $ do
    eligible receipt {receiptSucceeded = False} `shouldBe` False
    eligible (withEntry $ setWord 9 3 finalized) `shouldBe` False
    eligible (withEntry $ setWord 10 2 finalized) `shouldBe` False
    eligible (withEntry $ setWord 11 3 finalized) `shouldBe` False

  it "requires a priced historical receipt that reached the engine" $ do
    eligible (withEntry $ setWord 13 2 finalized) `shouldBe` False
    eligible (withEntry $ setWord 17 0 finalized) `shouldBe` False
    eligible (withEntry $ setWord 18 0 finalized) `shouldBe` False
    eligible (withEntry $ setWord 18 2 finalized) `shouldBe` False

  it "matches the configured lifecycle book" $ do
    receiptMayAdvanceOracleMark (T.toUpper book) receipt `shouldBe` True
    eligible (withEntry finalized {rpcLogAddress = "0x" <> T.replicate 40 "c"}) `shouldBe` False

  it "ignores cleanup receipts and truncated terminal evidence" $ do
    eligible receipt {receiptLogs = []} `shouldBe` False
    eligible (withEntry finalized {rpcLogData = BS.take 32 (rpcLogData finalized)}) `shouldBe` False
  where
    eligible = receiptMayAdvanceOracleMark book
    withEntry entry = receipt {receiptLogs = [entry]}

book :: Text
book = "0x" <> T.replicate 40 "a"

receipt :: TxReceipt
receipt =
  TxReceipt
    { receiptTxHash = rpcLogTxHash finalized
    , receiptBlockNumber = rpcLogBlockNumber finalized
    , receiptBlockHash = rpcLogBlockHash finalized
    , receiptTransactionIndex = rpcLogTransactionIndex finalized
    , receiptSucceeded = True
    , receiptLogs = [finalized]
    }

finalized :: RpcLog
finalized =
  RpcLog
    { rpcLogTxHash = "0x" <> T.replicate 64 "1"
    , rpcLogBlockNumber = 123
    , rpcLogBlockHash = "0x" <> T.replicate 64 "2"
    , rpcLogTransactionIndex = 0
    , rpcLogIndex = 3
    , rpcLogAddress = book
    , rpcLogTopics =
        [ orderFinalizedTopic
        , encodeUint256 7
        , encodeAddress ("0x" <> T.replicate 40 "b")
        , BS.replicate 32 0
        ]
    , rpcLogData = mconcat
        [encodeUint256 $ maybe 0 id $ lookup i receiptFields | i <- [0 :: Int .. 70]]
    }
  where
    receiptFields =
      [(9, 2), (10, 1), (11, 1), (13, 1), (14, 100_000_000), (17, 100), (18, 1)]

setWord :: Int -> Integer -> RpcLog -> RpcLog
setWord index value entry = entry {rpcLogData = updated}
  where
    offset = index * 32
    bytes = rpcLogData entry
    updated = BS.take offset bytes <> encodeUint256 value <> BS.drop (offset + 32) bytes
