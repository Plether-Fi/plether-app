module Plether.Handlers.ProtectionHistorySpec (spec) where
import Test.Hspec
import Plether.Handlers.ProtectionHistory (parseProtectionCursor, validProtectionBook, protectionExecutionSql)
import Data.List (isInfixOf)

spec :: Spec
spec = describe "v1.2.1 protection history inputs" $ do
  it "accepts canonical IDs and rejects invalid pagination" $ do
    parseProtectionCursor "18446744073709551615" `shouldBe` Just 18446744073709551615
    mapM_ (\value -> parseProtectionCursor value `shouldBe` Nothing) ["0", "-1", "01", "1.2", "18446744073709551616"]
  it "rejects histories for other Book deployments" $ do
    validProtectionBook Nothing `shouldBe` True
    validProtectionBook (Just "0x63973Eb0B5a862dfc95348D4d575FC55C9546F04") `shouldBe` True
    validProtectionBook (Just "0x1111111111111111111111111111111111111111") `shouldBe` False
  it "scopes execution observations to the chain, Book and protection" $ do
    show protectionExecutionSql `shouldSatisfy` isInfixOf "o.chain_id=? AND o.book=? AND o.protection_id=?"
    show protectionExecutionSql `shouldSatisfy` isInfixOf "linked_order_id::text=o.observation->>'linkedOrderId'"
  it "reports freshness and public transaction references, never signed bytes" $ do
    show protectionExecutionSql `shouldSatisfy` isInfixOf "ageSeconds"
    show protectionExecutionSql `shouldSatisfy` isInfixOf "transactionHash"
    show protectionExecutionSql `shouldNotSatisfy` isInfixOf "raw_transaction"
