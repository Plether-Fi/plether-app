module Plether.Perps.Funding.SourceSpec (spec) where

import Data.Aeson
import Data.Either (isLeft)
import Data.Text (Text)
import Plether.Perps.Funding.Source
import Plether.Perps.Funding.Types (setFields)
import Test.Hspec

spec :: Spec
spec = describe "source transaction binding" $ do
  it "accepts only the exact reviewed bridge transaction from the source owner" $
    sourceTransactionMatches intent hash transaction `shouldBe` Right ()
  it "does not bind a hash before the source RPC can see its transaction" $
    sourceTransactionMatches intent hash Null `shouldBe` Left "SOURCE_TRANSACTION_NOT_VISIBLE"
  it "rejects a different sender, target, calldata, value, or hash" $
    mapM_ (\change -> sourceTransactionMatches intent hash (setFields [change] transaction) `shouldSatisfy` isLeft)
      [("from",String target),("to",String owner),("input",String "0xdead"),("value",String "0x1"),("hash",String otherHash)]
  it "does not accept an approval transaction as evidence of bridge submission" $
    sourceTransactionMatches intent hash (setFields [("input",String "0x095ea7b3")] transaction) `shouldSatisfy` isLeft
  it "rejects an ambiguous source plan" $
    sourceTransactionMatches (setFields [("sourceTransactions",toJSON [bridge,bridge])] intent) hash transaction `shouldSatisfy` isLeft
  it "accepts a replacement hash only when its transaction bindings still match" $
    sourceTransactionMatches intent otherHash (setFields [("hash",String otherHash)] transaction) `shouldBe` Right ()

owner, target, hash, otherHash :: Text
owner = "0x1111111111111111111111111111111111111111"
target = "0x2222222222222222222222222222222222222222"
hash = "0x1111111111111111111111111111111111111111111111111111111111111111"
otherHash = "0x2222222222222222222222222222222222222222222222222222222222222222"
bridge, intent, transaction :: Value
bridge = object ["kind" .= ("bridge" :: Text),"to" .= target,"data" .= ("0x11223344" :: Text),"value" .= ("0" :: Text)]
intent = object ["ownerAddress" .= owner,"sourceTransactions" .= [bridge]]
transaction = object ["hash" .= hash,"from" .= owner,"to" .= target,"input" .= ("0x11223344" :: Text),"value" .= ("0x0" :: Text)]
