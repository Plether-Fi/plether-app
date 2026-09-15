module Plether.AA.CloseAssistanceEvidenceSpec (spec) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.Either (isLeft)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Plether.AA.CloseAssistanceEvidence
import Plether.Database.CloseAssistance
import Plether.Ethereum.Abi (encodeAddress, encodeUint256, keccak256)
import qualified Plether.Perps.Manifest as Manifest
import Test.Hspec

spec :: Spec
spec = describe "close assistance receipt provenance" $ do
  it "matches the exact safe operation's mint, deposit, intent and commit" $
    verify (receipt validLogs) `shouldBe` Right (2,7)
  it "rejects a matching deposit outside the operation boundary" $
    verify (receipt $ take 2 validLogs ++ [operationEvent 2 otherHash] ++ drop 2 validLogs) `shouldSatisfy` isLeft
  it "rejects a mint to a different account" $
    verify (receipt $ mint otherAccount : drop 1 validLogs) `shouldSatisfy` isLeft
  it "rejects duplicate deposit evidence" $
    verify (receipt $ take 2 validLogs ++ [deposit] ++ drop 2 validLogs) `shouldSatisfy` isLeft
  it "rejects missing fresh-intent evidence even when USDC was deposited" $
    verify (receipt $ take 2 validLogs ++ drop 3 validLogs) `shouldSatisfy` isLeft
 where
  verify = verifyCloseAssistanceReceipt grant opHash tx 100 blockHash 5

account, otherAccount, opHash, otherHash, tx, blockHash :: Text
account = "0x" <> T.replicate 40 "1"
otherAccount = "0x" <> T.replicate 40 "2"
opHash = "0x" <> T.replicate 64 "3"
otherHash = "0x" <> T.replicate 64 "4"
tx = "0x" <> T.replicate 64 "5"
blockHash = "0x" <> T.replicate 64 "6"

grant :: CloseAssistanceReservation
grant = CloseAssistanceReservation Manifest.orderRouterAddress account otherHash (hex $ keccak256 request) otherAccount 198000

request :: BS.ByteString
request = BS.replicate (18*32) 0

receipt :: [Value] -> Value
receipt logs = object ["transactionHash" .= tx,"blockHash" .= blockHash,"blockNumber" .= String "0x64","status" .= String "0x1","logs" .= logs]

validLogs :: [Value]
validLogs = [mint account,deposit,intent,commit,operationEvent 5 opHash]

mint :: Text -> Value
mint recipient = logEntry 1 Manifest.mockUsdcAddress
  [topic "Transfer(address,address,uint256)",hex $ BS.replicate 32 0,hex $ encodeAddress recipient] (encodeUint256 198000)

deposit, intent, commit :: Value
deposit = logEntry 2 Manifest.marginClearinghouseAddress
  [topic "Deposit(address,address,uint256)",hex $ encodeAddress account,hex $ encodeAddress Manifest.mockUsdcAddress] (encodeUint256 198000)
intent = logEntry 3 Manifest.orderLifecycleBookAddress
  [topic "IntentRegistered(uint64,address,bytes32,bytes32,uint256,(bytes32,uint8,uint256,uint256,uint256,bool,(uint64,uint8,bytes32,uint256,uint256,uint256,uint256,uint256,uint256,uint256,uint256,uint32)))",
   hex $ encodeUint256 7,hex $ encodeAddress account,otherHash] (BS.replicate 64 0 <> request)
commit = logEntry 4 Manifest.orderRouterAddress
  [topic "OrderCommitted(uint64,address,uint8)",hex $ encodeUint256 7,hex $ encodeAddress account] (encodeUint256 0)

operationEvent :: Int -> Text -> Value
operationEvent index operationHash = logEntry index "0x4337084d9e255ff0702461cf8895ce9e3b5ff108"
  [topic "UserOperationEvent(bytes32,address,address,uint256,bool,uint256,uint256)",operationHash] BS.empty

logEntry :: Int -> Text -> [Text] -> BS.ByteString -> Value
logEntry index address topics bytes = object ["address" .= address,"topics" .= topics,"data" .= hex bytes,"logIndex" .= ("0x" <> T.pack (show index))]

topic :: Text -> Text
topic = hex . keccak256 . TE.encodeUtf8
hex :: BS.ByteString -> Text
hex = ("0x" <>) . TE.decodeUtf8 . B16.encode
