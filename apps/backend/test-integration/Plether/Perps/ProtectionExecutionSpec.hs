module Plether.Perps.ProtectionExecutionSpec (protectionExecutionSpec) where

import Control.Exception (bracket, bracket_)
import Control.Monad (void)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.PostgreSQL.Simple
import Plether.Database.Protection (ensureProtectionSchema)
import Plether.Handlers.ProtectionHistory (protectionExecutionSql)
import Test.Hspec

protectionExecutionSpec :: T.Text -> Spec
protectionExecutionSpec databaseUrl = describe "protection execution observations" $ do
  it "returns a scoped, timestamped report and no report for other deployments or IDs" $
    withFixture databaseUrl $ \conn -> do
      report <- readReport conn 421614 "book" 7
      length report `shouldBe` 1
      let value = snd $ head report
      field "protectionId" value `shouldBe` Just (String "7")
      field "reason" value `shouldBe` Just (String "queue-congested")
      field "checkedBlock" value `shouldBe` Just (String "120")
      field "ageSeconds" value `shouldSatisfy` maybe False isNumber
      field "transactionHash" value `shouldBe` Just Null
      readReport conn 1 "book" 7 `shouldReturn` []
      readReport conn 421614 "other-book" 7 `shouldReturn` []
      readReport conn 421614 "book" 8 `shouldReturn` []
  it "keeps ambiguous signed transactions visible only for their exact close attempt" $
    withFixture databaseUrl $ \conn -> do
      void $ execute_ conn "INSERT INTO perps_protection_transactions(chain_id,book,transaction_hash,raw_transaction,protection_id,action,linked_order_id) VALUES(421614,'book','public-hash','secret-signed-bytes',7,'retry',19)"
      [(_, pendingReport)] <- readReport conn 421614 "book" 7
      field "transactionHash" pendingReport `shouldBe` Just (String "public-hash")
      field "transactionAction" pendingReport `shouldBe` Just (String "retry")
      field "raw_transaction" pendingReport `shouldBe` Nothing
      -- A refreshed failure observation must not hide the already-journaled transaction.
      void $ execute_ conn "UPDATE perps_protection_observations SET checked_at=NOW(), observation=jsonb_set(observation,'{reason}','\"check-failed\"')"
      [(_, refreshed)] <- readReport conn 421614 "book" 7
      field "transactionHash" refreshed `shouldBe` Just (String "public-hash")
      void $ execute_ conn "UPDATE perps_protection_observations SET observation=jsonb_set(observation,'{linkedOrderId}','\"20\"')"
      [(_, nextAttempt)] <- readReport conn 421614 "book" 7
      field "transactionHash" nextAttempt `shouldBe` Just Null
  it "exposes the age of stale reports rather than refreshing them on reads" $
    withFixture databaseUrl $ \conn -> do
      void $ execute_ conn "UPDATE perps_protection_observations SET checked_at=NOW()-INTERVAL '2 minutes'"
      [(_, stale)] <- readReport conn 421614 "book" 7
      field "ageSeconds" stale `shouldSatisfy` maybe False (\v -> case v of Number n -> n >= 120; _ -> False)

field :: T.Text -> Value -> Maybe Value
field name (Object fields) = KM.lookup (Key.fromText name) fields
field _ _ = Nothing

isNumber :: Value -> Bool
isNumber (Number _) = True
isNumber _ = False

readReport :: Connection -> Integer -> T.Text -> Integer -> IO [(Integer, Value)]
readReport conn chain book protectionId = query conn protectionExecutionSql (chain, book, protectionId)

withFixture :: T.Text -> (Connection -> IO a) -> IO a
withFixture url work = bracket (connectPostgreSQL $ TE.encodeUtf8 url) close $ \conn -> do
  [Only name] <- query_ conn "SELECT current_database()" :: IO [Only T.Text]
  if not ("critical_path" `T.isInfixOf` name) then fail "Protection SQL tests require a dedicated critical_path database" else
    bracket_ (void $ execute_ conn "BEGIN") (void $ execute_ conn "ROLLBACK") $ do
      -- Transactional schema is rolled back even if an assertion fails.
      void $ execute_ conn "CREATE SCHEMA protection_execution_spec"
      void $ execute_ conn "SET LOCAL search_path TO protection_execution_spec"
      ensureProtectionSchema conn
      ensureProtectionSchema conn
      void $ execute conn "INSERT INTO perps_protection_observations(chain_id,book,protection_id,checked_block,checked_block_hash,observation) VALUES(421614,'book',7,120,'block-hash',?)"
        (Only $ object ["protectionId" .= ("7" :: T.Text), "linkedOrderId" .= ("19" :: T.Text), "reason" .= ("queue-congested" :: T.Text)])
      work conn
