module Plether.Database.ExecutionProgressSpec (spec) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T
import Plether.Database.ExecutionProgress
import Test.Hspec

spec :: Spec
spec = describe "Execution locator cursors and confirmation policy" $ do
  it "uses the stricter keeper/indexer requirement with the indexer's default of one" $ do
    parseExecutionConfirmations 1 Nothing `shouldBe` Just 1
    parseExecutionConfirmations 3 (Just "1") `shouldBe` Just 3
    parseExecutionConfirmations 1 (Just "12") `shouldBe` Just 12
    parseExecutionConfirmations 0 (Just "0") `shouldBe` Just 1
  it "disables optional progress for malformed or out-of-range indexer configuration" $
    mapM_ (\raw -> parseExecutionConfirmations 1 (Just raw) `shouldBe` Nothing)
      ["", "-1", "1.5", "10001", "one", " 1", "18446744073709551617", "999999999999999999999999999999999999"]
  it "accepts bounded hash/state cursors and the explicit cleared cursor" $ do
    validExecutionVersion "none" `shouldBe` True
    validExecutionVersion (transaction <> ":submitting") `shouldBe` True
    validExecutionVersion (transaction <> ":submitted") `shouldBe` True
    mapM_ (\cursor -> validExecutionVersion cursor `shouldBe` False)
      ["", "null", "none:submitted", transaction <> ":executed", transaction <> ":submitted:extra", T.replicate 1000 "a"]
  it "changes cursor on acknowledgement, replacement and clearing" $ do
    let submitting = sample {epState = "submitting"}
    executionProgressVersion (Just submitting) `shouldBe` transaction <> ":submitting"
    executionProgressVersion (Just sample) `shouldBe` transaction <> ":submitted"
    executionProgressVersion (Just sample {epTransaction = replacement}) `shouldBe` replacement <> ":submitted"
    executionProgressVersion Nothing `shouldBe` "none"
  it "never represents a candidate hash as terminal history or economics" $
    case executionProgressJson 12 sample of
      Object fields -> do
        KM.lookup "requiredConfirmations" fields `shouldBe` Just (Number 12)
        KM.lookup "orderId" fields `shouldBe` Just (String "42")
        mapM_ (\key -> KM.member key fields `shouldBe` False)
          ["terminalStatus", "receiptHash", "executionPrice", "receiptEconomics", "raw_tx", "signature"]
      _ -> expectationFailure "Execution locator must be an object"

transaction, replacement :: T.Text
transaction = "0x" <> T.replicate 64 "a"
replacement = "0x" <> T.replicate 64 "b"

sample :: ExecutionProgress
sample = ExecutionProgress "submitted" 42
  ("0x" <> T.replicate 40 "1") ("0x" <> T.replicate 40 "2")
  ("0x" <> T.replicate 64 "3") ("0x" <> T.replicate 64 "4") transaction
