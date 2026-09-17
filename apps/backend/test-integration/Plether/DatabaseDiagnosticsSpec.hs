module Plether.DatabaseDiagnosticsSpec (databaseDiagnosticsSpec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, cancel, wait, withAsync)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar, takeMVar)
import Control.Exception (IOException, bracket, finally, try)
import Control.Monad (replicateM, replicateM_, void)
import Data.Aeson (Value (..), encode)
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Int (Int64)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (isInfixOf)
import Data.Text (Text)
import qualified Data.Text as T
import Database.PostgreSQL.Simple
import Plether.Database
import Plether.Database.Diagnostics (readBlockingSnapshot, withDiagnosticConnection)
import Plether.Database.Observation
import System.Timeout (timeout)
import Test.Hspec hiding (after, pending)

databaseDiagnosticsSpec :: Text -> Spec
databaseDiagnosticsSpec url = before_ assertDedicatedDatabase $ describe "database diagnostics integration" $ do
  it "isolates registration exhaustion from oracle and general capacity" $
    bracket (newRegistrationDbPool url) destroyDbPool $ \registration ->
    bracket (newOracleDbPool url) destroyDbPool $ \oracle ->
    bracket (newApiDbPool url) destroyDbPool $ \general -> do
      entered <- newEmptyMVar
      gate <- newEmptyMVar
      let occupy = withDb registration $ \_ -> putMVar entered () >> readMVar gate
      withAsync occupy $ \first -> withAsync occupy $ \second -> do
        takeMVar entered
        takeMVar entered
        withDb registration (const $ pure ()) `shouldThrow` (== DbAcquireDeadline)
        withDb oracle (\conn -> query_ conn "SELECT 1" :: IO [Only Int]) `shouldReturn` [Only 1]
        withDb general (\conn -> query_ conn "SELECT 1" :: IO [Only Int]) `shouldReturn` [Only 1]
        putMVar gate ()
        wait first
        wait second
      snapshot <- readPoolSnapshot $ dbPoolObservation registration
      psWaiting snapshot `shouldBe` 0
      psActive snapshot `shouldBe` []

  it "discards a bounded connection after a SQL deadline" $
    bracket (newRegistrationDbPool url) destroyDbPool $ \pool -> do
      pidRef <- newIORef Nothing
      withDb pool (\conn -> do
        [Only pid] <- query_ conn "SELECT pg_backend_pid()" :: IO [Only Int]
        writeIORef pidRef $ Just pid
        query_ conn "SELECT 1 FROM pg_sleep(3)" :: IO [Only Int])
        `shouldThrow` (\err -> sqlState err == "57014")
      readIORef pidRef >>= assertDisconnected
      withDb pool (\conn -> query_ conn "SELECT 1" :: IO [Only Int]) `shouldReturn` [Only 1]

  it "bounds connection ownership even when no SQL is running" $
    bracket (newRegistrationDbPool url) destroyDbPool $ \pool -> do
      withDb pool (const $ threadDelay 3_000_000) `shouldThrow` (== DbOperationDeadline)
      snapshot <- readPoolSnapshot $ dbPoolObservation pool
      psActive snapshot `shouldBe` []

  it "isolates read-only settings and deadlines from application connections" $
    withConnection $ \application -> do
      let settings conn = query_ conn
            "SELECT current_setting('default_transaction_read_only'), current_setting('statement_timeout'), current_setting('lock_timeout')"
            :: IO [(Text, Text, Text)]
      originalSettings <- settings application
      observed <- withDiagnosticConnection url settings
      observed `shouldBe` Just [("on", "1500ms", "500ms")]
      settings application `shouldReturn` originalSettings
      writeResult <- try (withDiagnosticConnection url $ \conn ->
        execute_ conn "CREATE TABLE diagnostics_must_not_write (id integer)")
        :: IO (Either SqlError (Maybe Int64))
      fmap (const ()) writeResult `shouldSatisfy` either ((== "25006") . sqlState) (const False)

  it "bounds diagnostic SQL execution and closes the connection after timeout" $ do
    pidRef <- newIORef Nothing
    result <- try (withDiagnosticConnection url $ \conn -> do
      [Only pid] <- query_ conn "SELECT pg_backend_pid()" :: IO [Only Int]
      writeIORef pidRef $ Just pid
      query_ conn "SELECT 1 FROM pg_sleep(5)" :: IO [Only Int])
      :: IO (Either SqlError (Maybe [Only Int]))
    result `shouldSatisfy` either ((== "57014") . sqlState) (const False)
    readIORef pidRef >>= assertDisconnected

  it "closes its independent connection on cancellation" $ do
    entered <- newEmptyMVar
    gate <- newEmptyMVar
    withAsync (withDiagnosticConnection url $ \conn -> do
      [Only pid] <- query_ conn "SELECT pg_backend_pid()" :: IO [Only Int]
      putMVar entered pid
      takeMVar gate) $ \worker -> do
        pid <- takeMVar entered
        cancel worker
        assertDisconnected $ Just pid

  it "enforces the outer diagnostic deadline and closes the connection" $ do
    pidRef <- newIORef Nothing
    result <- withDiagnosticConnection url $ \conn -> do
      [Only pid] <- query_ conn "SELECT pg_backend_pid()" :: IO [Only Int]
      writeIORef pidRef $ Just pid
      threadDelay 5_000_000
    result `shouldBe` Nothing
    readIORef pidRef >>= assertDisconnected

  it "discards failed application connections without leaking observations" $
    bracket (newDbPool url) destroyDbPool $ \pool -> do
      pidRef <- newIORef Nothing
      result <- try (withDb pool $ \conn -> do
        [Only pid] <- query_ conn "SELECT pg_backend_pid()" :: IO [Only Int]
        writeIORef pidRef $ Just pid
        ioError $ userError "expected failure") :: IO (Either IOException ())
      result `shouldSatisfy` either (const True) (const False)
      readIORef pidRef >>= assertDisconnected
      snapshot <- readPoolSnapshot $ dbPoolObservation pool
      psActive snapshot `shouldBe` []
      psWaiting snapshot `shouldBe` 0
      psFailures snapshot `shouldBe` 1
      withDb pool (\conn -> query_ conn "SELECT 1" :: IO [Only Int]) `shouldReturn` [Only 1]

  it "captures actual PostgreSQL blockers without SQL or client metadata" $
    withConnection $ \blocker -> withConnection $ \blocked -> withConnection $ \observer -> do
      [Only blockerPid] <- query_ blocker "SELECT pg_backend_pid()" :: IO [Only Int]
      [Only blockedPid] <- query_ blocked "SELECT pg_backend_pid()" :: IO [Only Int]
      void $ execute_ blocked "SET application_name='PRIVATE_DIAGNOSTIC_SENTINEL'"
      void (query_ blocker "SELECT 1 FROM pg_advisory_lock(9071600123)" :: IO [Only Int])
      let unlock = void (query_ blocker "SELECT pg_advisory_unlock(9071600123)" :: IO [Only Bool])
      (withAsync (query_ blocked "SELECT 1 FROM pg_advisory_lock(9071600123) /* PRIVATE_DIAGNOSTIC_SENTINEL */" :: IO [Only Int]) $ \pending -> do
        snapshot <- waitUntil (hasBlocked blockedPid) $ readBlockingSnapshot observer []
        show snapshot `shouldSatisfy` isInfixOf (show blockerPid)
        LBS.unpack (encode snapshot) `shouldSatisfy` (not . isInfixOf "PRIVATE_DIAGNOSTIC_SENTINEL")
        unlock
        completed <- timeout 2_000_000 $ wait pending
        completed `shouldBe` Just [Only 1]) `finally` unlock

  it "observes a saturated application pool through an independent connection" $
    bracket (newDbPool url) destroyDbPool $ \pool -> withConnection $ \observer -> do
      entered <- newEmptyMVar
      release <- newEmptyMVar
      let hold = withDb pool $ \_ -> putMVar entered () >> readMVar release
      bracket (replicateM 10 $ async hold) (mapM_ cancel) $ \holders -> do
        replicateM_ 10 $ takeMVar entered
        withAsync (withDb pool $ \_ -> pure ()) $ \waiter -> do
          snapshot <- waitUntil ((== 1) . psWaiting) $ readPoolSnapshot $ dbPoolObservation pool
          length (psActive snapshot) `shouldBe` 10
          map acSource (psActive snapshot) `shouldSatisfy`
            all (T.isPrefixOf "Plether.DatabaseDiagnosticsSpec:")
          blocking <- readBlockingSnapshot observer $ map acBackendPid $ psActive snapshot
          case blocking of
            Object value -> case KeyMap.lookup "sessions" value of
              Just (Array sessions) -> length sessions `shouldBe` 10
              _ -> expectationFailure "Missing session evidence"
            _ -> expectationFailure "Missing blocking snapshot"
          putMVar release ()
          finished <- timeout 2_000_000 $ mapM_ wait holders >> wait waiter
          finished `shouldBe` Just ()
          after <- readPoolSnapshot $ dbPoolObservation pool
          psWaiting after `shouldBe` 0
          psActive after `shouldBe` []
  where
    withConnection = bracket (connectPostgreSQL $ BS.pack $ T.unpack url) close
    assertDedicatedDatabase = withConnection $ \conn -> do
      names <- query_ conn "SELECT current_database()" :: IO [Only Text]
      case names of
        [Only name] | "critical_path" `T.isInfixOf` T.toLower name -> pure ()
        _ -> fail "Diagnostics integration tests require a dedicated critical_path PostgreSQL database"
    assertDisconnected Nothing = expectationFailure "Connection PID was not captured"
    assertDisconnected (Just pid) = withConnection $ \observer -> do
      rows <- waitUntil (== [Only False]) $
        (query observer "SELECT EXISTS (SELECT 1 FROM pg_stat_activity WHERE pid=?)" (Only pid) :: IO [Only Bool])
      rows `shouldBe` [Only False]

hasBlocked :: Int -> Value -> Bool
hasBlocked pid (Object root) = case KeyMap.lookup "sessions" root of
  Just (Array rows) -> any matches rows
  _ -> False
  where
    matches (Object row) = KeyMap.lookup "backend_pid" row == Just (Number $ fromIntegral pid)
      && case KeyMap.lookup "blocking_pids" row of
        Just (Array blockers) -> not $ null blockers
        _ -> False
    matches _ = False
hasBlocked _ _ = False

waitUntil :: (a -> Bool) -> IO a -> IO a
waitUntil predicate action = loop (100 :: Int)
  where
    loop remaining = do
      result <- action
      if predicate result then pure result
      else if remaining <= 0 then fail "Diagnostic condition was not observed"
      else threadDelay 20_000 >> loop (remaining - 1)
