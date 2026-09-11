module Plether.AA.OrderDiagnosticsSpec (spec) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Plether.AA.OrderDiagnostics
import Plether.Ethereum.Abi (encodeUint256, keccak256)
import qualified Plether.Ethereum.Contracts.Perps as Perps
import Plether.Ethereum.Rpc
import Test.Hspec

spec :: Spec
spec = describe "Durable order diagnostic receipt correlation" $ do
  it "matches the operation interval, not the first event in a multi-operation bundle" $
    correlate [registration 1 10 sender, userEvent 2 otherHash sender, registration 3 20 sender, userEvent 4 operation sender]
      `shouldBe` Just (Just 20)
  it "does not assign the previous operation's order to a subsequent deposit" $
    correlate [registration 1 10 sender, userEvent 2 otherHash sender, userEvent 3 operation sender]
      `shouldBe` Just Nothing
  it "does not assign a later operation's order to a preceding deposit" $
    correlate [userEvent 1 operation sender, registration 2 10 sender, userEvent 3 otherHash sender]
      `shouldBe` Just Nothing
  it "requires the exact sender" $
    correlate [registration 1 10 stranger, userEvent 2 operation sender] `shouldBe` Nothing
  it "requires a successful EntryPoint event as well as a successful transaction" $
    correlate [(userEvent 1 operation sender){rpcLogData=BS.replicate 128 0}] `shouldBe` Nothing
  it "rejects duplicate or ambiguous UserOperation events" $
    correlate [userEvent 1 operation sender,userEvent 2 operation sender] `shouldBe` Nothing
  it "rejects duplicate log indices" $
    correlate [registration 1 10 sender,userEvent 1 operation sender] `shouldBe` Nothing
  it "does not classify a malformed registration as an ordinary non-order operation" $
    correlate [(registration 1 10 sender){rpcLogData=BS.empty},userEvent 2 operation sender] `shouldBe` Nothing
  it "rejects two order registrations instead of guessing which one to link" $
    correlate [registration 1 10 sender,registration 2 20 sender,userEvent 3 operation sender] `shouldBe` Nothing
  it "rejects mixed receipt context" $
    correlate [(registration 1 10 sender){rpcLogBlockHash=otherHash},userEvent 2 operation sender] `shouldBe` Nothing
  it "ignores registrations from a different release" $
    correlate [(registration 1 10 sender){rpcLogAddress=stranger},userEvent 2 operation sender] `shouldBe` Just Nothing
  it "does not treat raw errors as exportable reason codes" $ do
    executionFailureReason "provider secret raw payload" `shouldBe` "KEEPER_EXECUTION_FAILED"
    executionFailureReason "insufficient funds for gas" `shouldBe` "KEEPER_INSUFFICIENT_FUNDS"
    executionFailureReason "upstream Timeout" `shouldBe` "KEEPER_RPC_TIMEOUT"
 where
  correlate logs = receiptOrder lifecycle sender operation $ TxReceipt transaction 100 block 0 True logs

sender, stranger, lifecycle, operation, otherHash, transaction, block :: Text
sender = "0x" <> T.replicate 40 "1"
stranger = "0x" <> T.replicate 40 "2"
lifecycle = "0x" <> T.replicate 40 "3"
operation = "0x" <> T.replicate 64 "4"
otherHash = "0x" <> T.replicate 64 "5"
transaction = "0x" <> T.replicate 64 "6"
block = "0x" <> T.replicate 64 "7"

unhex :: Text -> BS.ByteString
unhex = either error id . B16.decode . TE.encodeUtf8 . T.drop 2
accountWord :: Text -> BS.ByteString
accountWord = (BS.replicate 12 0 <>) . unhex

registration :: Integer -> Integer -> Text -> RpcLog
registration index order account = RpcLog transaction 100 block 0 index lifecycle
  [Perps.intentRegisteredTopic,encodeUint256 order,accountWord account,BS.replicate 32 0] (BS.replicate 640 0)

userEvent :: Integer -> Text -> Text -> RpcLog
userEvent index hash account = RpcLog transaction 100 block 0 index "0x4337084d9e255ff0702461cf8895ce9e3b5ff108"
  [keccak256 "UserOperationEvent(bytes32,address,address,uint256,bool,uint256,uint256)",unhex hash,accountWord account,BS.replicate 32 0]
  (encodeUint256 0 <> encodeUint256 1 <> encodeUint256 100 <> encodeUint256 100)
