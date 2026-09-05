module Plether.Keeper.ProtectionDatabaseSpec (protectionDatabaseSpec) where

import Control.Exception (bracket, finally)
import Control.Monad (void)
import qualified Data.ByteString as BS
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Time.Clock.POSIX (getPOSIXTime)
import Database.PostgreSQL.Simple
import Plether.Ethereum.Rpc (RpcLog (..))
import Plether.Keeper.Protection
import Test.Hspec

protectionDatabaseSpec :: Text -> Spec
protectionDatabaseSpec url = describe "protection attempt PostgreSQL durability" $ do
  it "preserves one-to-many links and consumed marker evidence across duplicate ingestion" $
    withDatabase url $ \conn -> do
      let events =
            [ (1, AttemptRegistered 11)
            , (2, AttemptQueued 7 account 11 0)
            , (3, AttemptFailed 7 account 11 2 True)
            , (4, AttemptRegistered 19)
            , (5, AttemptQueued 7 account 19 11)
            , (6, AttemptFailed 7 account 19 2 True)
            ]
      mapM_ (\(index, event) -> recordProtectionEvent conn book (logEntry index) event) (events <> events)
      rows <- query conn "SELECT order_id, previous_order_id FROM perps_protection_attempt_events WHERE book = ? AND protection_id = 7 AND event_kind = 'queued' ORDER BY order_id" (Only book) :: IO [(Integer, Integer)]
      rows `shouldBe` [(11, 0), (19, 11)]
      registered <- query conn "SELECT order_id FROM perps_protection_attempt_events WHERE book = ? AND event_kind = 'registered' ORDER BY order_id" (Only book) :: IO [Only Integer]
      registered `shouldBe` [Only 11, Only 19]
      candidates <- query_ conn "SELECT protection_id FROM perps_protection_retry_candidates" :: IO [Only Integer]
      candidates `shouldBe` [Only 7]
      count <- query_ conn "SELECT COUNT(*) FROM perps_protection_attempt_events" :: IO [Only Integer]
      count `shouldBe` [Only 6]
      -- Re-running schema setup must preserve permanent evidence.
      ensureProtectionSchema conn
      query_ conn "SELECT COUNT(*) FROM perps_protection_attempt_events" `shouldReturn` count
  it "keeps terminal failures out of retry candidates and scopes evidence by Book" $
    withDatabase url $ \conn -> do
      recordProtectionEvent conn book (logEntry 1) (AttemptFailed 7 account 11 4 False)
      recordProtectionEvent conn "another-book" (logEntry 1) (AttemptFailed 7 account 11 2 True)
      candidates <- query_ conn "SELECT book, protection_id FROM perps_protection_retry_candidates" :: IO [(Text, Integer)]
      candidates `shouldBe` [("another-book", 7)]

withDatabase :: Text -> (Connection -> IO a) -> IO a
withDatabase url action = bracket (connectPostgreSQL $ TE.encodeUtf8 url) close $ \conn -> do
  suffix <- (floor . (* 1000000) <$> getPOSIXTime) :: IO Integer
  let schema = "protection_test_" <> show suffix
  void $ execute_ conn $ fromString $ "CREATE SCHEMA " <> schema
  (do
    void $ execute_ conn $ fromString $ "SET search_path TO " <> schema
    ensureProtectionSchema conn
    action conn) `finally` void (execute_ conn $ fromString $ "DROP SCHEMA " <> schema <> " CASCADE")

book :: Text
book = "0x0000000000000000000000000000000000000002"

account :: Text
account = "0x0000000000000000000000000000000000000001"

logEntry :: Integer -> RpcLog
logEntry index = RpcLog "0xtx" 10 "0xblock" 0 index book [] BS.empty
