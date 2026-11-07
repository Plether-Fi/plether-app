module Plether.Keeper.WakeupDatabaseSpec (wakeupDatabaseSpec) where

import Control.Exception (bracket, try)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.PostgreSQL.Simple (Connection, Only (..), close, connectPostgreSQL, query, withTransaction)
import GHC.Clock (getMonotonicTimeNSec)
import Plether.Keeper.Wakeup
import Test.Hspec

wakeupDatabaseSpec :: Text -> Spec
wakeupDatabaseSpec databaseUrl = describe "PostgreSQL commitment wakeups" $ do
  it "LISTENs before startup rescan and delivers only committed matching-router hints" $ do
    wakeup <- newWakeup
    withWakeupListener databaseUrl router wakeup $
      bracket (connectPostgreSQL $ TE.encodeUtf8 databaseUrl) close $ \publisher -> do
        -- A startup signal is evidence that LISTEN has committed. Work which
        -- predates registration is recovered by the caller's durable rescan.
        waitWakeup wakeup 5_000_000 `shouldReturn` True
        withTransaction publisher $ do
          notifyCommitmentWakeup publisher router
          waitWakeup wakeup 50_000 `shouldReturn` False
        waitWakeup wakeup 2_000_000 `shouldReturn` True
        notifyCommitmentWakeup publisher "0xbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
        waitWakeup wakeup 100_000 `shouldReturn` False
        notifyCommitmentWakeup publisher " 0xAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA "
        waitWakeup wakeup 2_000_000 `shouldReturn` True

  it "does not publish a notification when its durable transaction rolls back" $ do
    wakeup <- newWakeup
    withWakeupListener databaseUrl router wakeup $
      bracket (connectPostgreSQL $ TE.encodeUtf8 databaseUrl) close $ \publisher -> do
        waitWakeup wakeup 5_000_000 `shouldReturn` True
        -- The notify shares the same transactional outcome as durable hint
        -- persistence, so a rollback cannot announce work which never existed.
        rolledBack <- rollbackNotification publisher router
        rolledBack `shouldBe` True
        waitWakeup wakeup 100_000 `shouldReturn` False

  it "reconnects its own terminated listener and rescans before receiving new hints" $ do
    wakeup <- newWakeup
    unique <- getMonotonicTimeNSec
    let tag = "plether-wakeup-test-" <> T.pack (show unique)
        taggedUrl
          | "postgresql://" `T.isPrefixOf` databaseUrl || "postgres://" `T.isPrefixOf` databaseUrl =
              databaseUrl <> (if "?" `T.isInfixOf` databaseUrl then "&" else "?") <> "application_name=" <> tag
          | otherwise = databaseUrl <> " application_name=" <> tag
        listenerPids publisher = query publisher
          "SELECT pid FROM pg_stat_activity WHERE datname=current_database() AND application_name=? AND pid<>pg_backend_pid()"
          (Only tag) :: IO [Only Int]
    -- Only the tagged connection created by this exact test is terminated.
    -- No fixture tables or other workers' database sessions are touched.
    withWakeupListener taggedUrl router wakeup $
      bracket (connectPostgreSQL $ TE.encodeUtf8 databaseUrl) close $ \publisher -> do
        waitWakeup wakeup 5_000_000 `shouldReturn` True
        before <- listenerPids publisher
        case before of
          [Only pid] -> do
            terminated <- query publisher "SELECT pg_terminate_backend(?)" (Only pid) :: IO [Only Bool]
            terminated `shouldBe` [Only True]
            waitWakeup wakeup 5_000_000 `shouldReturn` True
            after <- listenerPids publisher
            after `shouldSatisfy` (\pids -> length pids == 1 && pids /= before)
            notifyCommitmentWakeup publisher router
            waitWakeup wakeup 2_000_000 `shouldReturn` True
          _ -> expectationFailure "The test's dedicated listener session was not unique"
  where
    router = "0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

rollbackNotification :: Connection -> Text -> IO Bool
rollbackNotification publisher router = do
  result <- try $ withTransaction publisher $ do
    notifyCommitmentWakeup publisher router
    ioError $ userError "test rollback"
  pure $ case result :: Either IOError () of
    Left _ -> True
    Right () -> False
