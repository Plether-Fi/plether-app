module Plether.Keeper.CommitmentHintsSpec (spec) where

import Data.Aeson (Value,object,(.=))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Plether.Ethereum.Abi
import qualified Plether.Ethereum.Contracts.Perps as P
import Plether.Ethereum.Rpc
import Plether.Keeper.CommitmentHints
import qualified Plether.Perps.Manifest as Manifest
import Test.Hspec hiding (pending)

spec :: Spec
spec = describe "Advisory commitment hint receipt binding" $ do
  it "accepts only a complete transaction hash" $ do
    validCommitmentHash operation `shouldBe` True
    mapM_ (\v -> validCommitmentHash v `shouldBe` False) ["",sender,T.drop 1 operation,operation<>"0","0x"<>T.replicate 64 "z"]
  it "binds original calldata clientOrderId to the configured router" $ do
    expectedClientOrderId router (operationFor router commitCall) `shouldBe` Just clientId
    expectedClientOrderId stranger (operationFor router commitCall) `shouldBe` Nothing
    expectedClientOrderId router (operationFor router $ BS.take 611 commitCall) `shouldBe` Nothing
    expectedClientOrderId router (operationFor router $ BS.pack [0xe4,0x27,0x58,0x66]<>BS.replicate 608 0) `shouldBe` Nothing
  it "recognizes the release-bound protected-open entry path" $ do
    let bytes=BS.pack [0x56,0xf7,0x50,0xf0]<>unhex clientId<>BS.replicate (24*32) 0
    expectedClientOrderId router (operationFor Manifest.positionProtectionBookAddress bytes) `shouldBe` Just clientId
    expectedClientOrderId router (operationFor stranger bytes) `shouldBe` Nothing
  it "accepts canonical successful inclusion before any execution decision" $
    verified logs canonical pending `shouldBe` Just (20,1000)
  it "does not associate another operation in a bundled transaction" $ do
    let bundle=[registration 1 10 otherClient,commit 2 10,userEvent 3 otherHash sender paymaster] <> map shift logs
        shift x=x{rpcLogIndex=rpcLogIndex x+3}
    verified bundle canonical pending `shouldBe` Just (20,1000)
    verified [registration 1 20 clientId,commit 2 20,userEvent 3 otherHash sender paymaster,userEvent 4 operation sender paymaster] canonical pending `shouldBe` Nothing
  it "requires the exact client order, account and sponsor" $ do
    verified [registration 1 20 otherClient,commit 2 20,userEvent 3 operation sender paymaster] canonical pending `shouldBe` Nothing
    verified [registration 1 20 clientId,commit 2 20,userEvent 3 operation stranger paymaster] canonical pending `shouldBe` Nothing
    verified [registration 1 20 clientId,commit 2 20,userEvent 3 operation sender stranger] canonical pending `shouldBe` Nothing
  it "rejects mismatched release targets and duplicate events" $ do
    verified ((head logs){rpcLogAddress=stranger}:tail logs) canonical pending `shouldBe` Nothing
    verified [registration 1 20 clientId,(commit 2 20){rpcLogAddress=stranger},userEvent 3 operation sender paymaster] canonical pending `shouldBe` Nothing
    verified (logs<>[userEvent 4 operation sender paymaster]) canonical pending `shouldBe` Nothing
    verified [registration 1 20 clientId,commit 1 20,userEvent 3 operation sender paymaster] canonical pending `shouldBe` Nothing
  it "rejects a reorged header or mixed receipt evidence" $ do
    verified logs canonical{rpcBlockHash=otherHash} pending `shouldBe` Nothing
    verified ((head logs){rpcLogBlockHash=otherHash}:tail logs) canonical pending `shouldBe` Nothing
    verified logs canonical{rpcBlockNumber=101} pending `shouldBe` Nothing
  it "requires pinned pending-order timing and the release's Arbitrum L2 commit block" $ do
    verified logs canonical pending{P.povOrderId=21} `shouldBe` Nothing
    verified logs canonical pending{P.povCommitTime=999} `shouldBe` Nothing
    verified logs canonical pending{P.povCommitBlock=50} `shouldBe` Nothing
    verified logs canonical pending{P.povSide=1} `shouldBe` Nothing
  it "rejects reverted or malformed operation outcomes" $ do
    verifyCommitmentReceipt router lifecycle paymaster sender operation clientId receipt{receiptSucceeded=False} canonical pending `shouldBe` Nothing
    verified [registration 1 20 clientId,commit 2 20,(userEvent 3 operation sender paymaster){rpcLogData=BS.replicate 128 0}] canonical pending `shouldBe` Nothing
    verified [registration 1 20 clientId,(commit 2 20){rpcLogData=BS.empty},userEvent 3 operation sender paymaster] canonical pending `shouldBe` Nothing
  it "an absent sink never blocks or validates a submission" $
    enqueueCommitmentHint Nothing "malformed" `shouldReturn` ()

router,lifecycle,sender,paymaster,stranger,operation,otherHash,transaction,blockHash,clientId,otherClient :: Text
router="0x"<>T.replicate 40 "1"
lifecycle="0x"<>T.replicate 40 "2"
sender="0x"<>T.replicate 40 "3"
paymaster="0x"<>T.replicate 40 "4"
stranger="0x"<>T.replicate 40 "5"
operation="0x"<>T.replicate 64 "6"
otherHash="0x"<>T.replicate 64 "7"
transaction="0x"<>T.replicate 64 "8"
blockHash="0x"<>T.replicate 64 "9"
clientId="0x"<>T.replicate 64 "a"
otherClient="0x"<>T.replicate 64 "b"
unhex :: Text -> BS.ByteString
unhex=either error id . B16.decode . TE.encodeUtf8 . T.drop 2
hex :: BS.ByteString -> Text
hex b="0x"<>TE.decodeUtf8 (B16.encode b)
wordAddress :: Text -> BS.ByteString
wordAddress a=BS.replicate 12 0<>unhex a
base :: Integer -> Text -> [BS.ByteString] -> BS.ByteString -> RpcLog
base index address topics bytes=RpcLog transaction 100 blockHash 0 index address topics bytes
registration :: Integer -> Integer -> Text -> RpcLog
registration index order client=base index lifecycle [P.intentRegisteredTopic,encodeUint256 order,wordAddress sender,unhex client] (BS.replicate 800 0)
commit :: Integer -> Integer -> RpcLog
commit index order=base index router [P.orderCommittedTopic,encodeUint256 order,wordAddress sender] (encodeUint256 0)
userEvent :: Integer -> Text -> Text -> Text -> RpcLog
userEvent index hash account sponsor=base index "0x4337084d9e255ff0702461cf8895ce9e3b5ff108"
  [keccak256 "UserOperationEvent(bytes32,address,address,uint256,bool,uint256,uint256)",unhex hash,wordAddress account,wordAddress sponsor]
  (encodeUint256 0<>encodeUint256 1<>encodeUint256 100<>encodeUint256 100)
logs :: [RpcLog]
logs=[registration 1 20 clientId,commit 2 20,userEvent 3 operation sender paymaster]
receipt :: TxReceipt
receipt=TxReceipt transaction 100 blockHash 0 True logs
canonical :: RpcBlock
canonical=RpcBlock 100 (Just 50) blockHash 1000
pending :: P.PendingOrderView
pending=P.PendingOrderView 20 False 0 1 1 1 1000 100 0 0 0
verified :: [RpcLog] -> RpcBlock -> P.PendingOrderView -> Maybe (Integer,Integer)
verified entries=verifyCommitmentReceipt router lifecycle paymaster sender operation clientId receipt{receiptLogs=entries}
commitCall :: BS.ByteString
commitCall=BS.pack [0xe4,0x27,0x58,0x66]<>unhex clientId<>BS.replicate (18*32) 0
operationFor :: Text -> BS.ByteString -> Value
operationFor target bytes=object ["callData".=hex calldata]
 where calldata=selector "execute(address,uint256,bytes)"<>encodeAddress target<>encodeUint256 0<>encodeUint256 96
         <>encodeUint256 (fromIntegral $ BS.length bytes)<>bytes<>BS.replicate ((32-BS.length bytes `mod` 32) `mod` 32) 0
