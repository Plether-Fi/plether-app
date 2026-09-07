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
import Plether.Handlers.ProtectionHistory (parseProtectionCursor, protectionExecutionSql, readProtectionHistoryPage)
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

  describe "protection history pages" $ do
    it "decodes a populated NUMERIC protection ID and scopes history to its account and deployment" $
      withFixture databaseUrl $ \conn -> do
        insertHistory conn 1 100 0 "created"
        (rows, cursor) <- readProtectionHistoryPage conn 421614 "book" "ACCOUNT" 25 Nothing
        map (field "protectionId") rows `shouldBe` [Just (String "1")]
        cursor `shouldBe` Nothing
        readProtectionHistoryPage conn 1 "book" "account" 25 Nothing `shouldReturn` ([], Nothing)
        readProtectionHistoryPage conn 421614 "other-book" "account" 25 Nothing `shouldReturn` ([], Nothing)
        readProtectionHistoryPage conn 421614 "book" "other-account" 25 Nothing `shouldReturn` ([], Nothing)

    it "paginates numerically and preserves IDs across the full uint64 range" $
      withFixture databaseUrl $ \conn -> do
        let ids = [2, 10, 9223372036854775808, 18446744073709551615]
        mapM_ (\(i, pid) -> insertHistory conn pid 100 i "armed") $ zip [0..] ids
        (first, next) <- readProtectionHistoryPage conn 421614 "book" "account" 1 Nothing
        map (field "protectionId") first `shouldBe` [Just (String "18446744073709551615")]
        next `shouldBe` Just "18446744073709551615"
        (second, nextSecond) <- readProtectionHistoryPage conn 421614 "book" "account" 2 (next >>= parseProtectionCursor)
        map (field "protectionId") second `shouldBe` [Just (String "9223372036854775808"), Just (String "10")]
        nextSecond `shouldBe` Just "10"
        (lastPage, end) <- readProtectionHistoryPage conn 421614 "book" "account" 2 (nextSecond >>= parseProtectionCursor)
        map (field "protectionId") lastPage `shouldBe` [Just (String "2")]
        end `shouldBe` Nothing
        readProtectionHistoryPage conn 421614 "book" "account" 25 (Just 2) `shouldReturn` ([], Nothing)

    it "returns only the latest snapshot by block and log index" $
      withFixture databaseUrl $ \conn -> do
        insertHistory conn 1 100 9 "created"
        insertHistory conn 1 101 0 "armed"
        insertHistory conn 1 101 1 "cancelled"
        (rows, cursor) <- readProtectionHistoryPage conn 421614 "book" "account" 1 Nothing
        map (field "phase") rows `shouldBe` [Just (String "cancelled")]
        map (field "updatedBlock") rows `shouldBe` [Just (String "101")]
        cursor `shouldBe` Nothing

    it "handles empty history and clamps page size to the supported bounds" $
      withFixture databaseUrl $ \conn -> do
        readProtectionHistoryPage conn 421614 "book" "account" 25 Nothing `shouldReturn` ([], Nothing)
        mapM_ (\pid -> insertHistory conn pid 100 pid "armed") [1..101]
        (small, smallCursor) <- readProtectionHistoryPage conn 421614 "book" "account" 0 Nothing
        length small `shouldBe` 1
        smallCursor `shouldBe` Just "101"
        (large, largeCursor) <- readProtectionHistoryPage conn 421614 "book" "account" 200 Nothing
        length large `shouldBe` 100
        largeCursor `shouldBe` Just "2"

insertHistory :: Connection -> Integer -> Integer -> Integer -> T.Text -> IO ()
insertHistory conn protectionId block logIndex phase = void $ execute conn
  "INSERT INTO perps_protection_events(chain_id,book,protection_id,account,block_number,block_hash,log_index,transaction_hash,event_name,event_data,snapshot) VALUES(421614,'book',?,'account',?,'block-hash',?,'tx-hash','PositionProtectionCreated','{}',?)"
  (protectionId, block, logIndex, object ["protectionId" .= show protectionId, "phase" .= phase])

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
