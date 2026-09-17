module Plether.Database.ObservationSpec (spec) where

import Control.Concurrent.Async (async, cancel, mapConcurrently_)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (IOException, onException, try)
import Control.Monad (replicateM_)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.IORef
import Data.List (isInfixOf)
import Plether.Database.Diagnostics (diagnosticConnectionString, snapshotValue)
import Plether.Database.Observation
import Test.Hspec hiding (after, pending)

spec :: Spec
spec = describe "database pool observation" $ do
  it "separates connection wait from hold time and records static source and PID" $ do
    clock <- newIORef 0
    tracker <- newObservation $ readIORef clock
    let allocator use = writeIORef clock 300_000_000 >> use (42 :: Int)
    result <- observeResource tracker "Module:12" allocator id $ \_ -> do
      writeIORef clock 2_300_000_000
      during <- readPoolSnapshot tracker
      psWaiting during `shouldBe` 0
      psActive during `shouldBe` [ActiveCheckout "Module:12" 42 2000]
      pure ("returned" :: String)
    result `shouldBe` "returned"
    after <- drainPoolSnapshot tracker
    psWaitTotalMs after `shouldBe` 300
    psHoldTotalMs after `shouldBe` 2000
    psAcquired after `shouldBe` 1
    psReleased after `shouldBe` 1
    psActive after `shouldBe` []
    poolUnderPressure after `shouldBe` True
    cleared <- readPoolSnapshot tracker
    psWaitMaxMs cleared `shouldBe` 0
    psReleased cleared `shouldBe` 0

  it "does not swallow action errors or bypass allocator discard semantics" $ do
    tracker <- newObservation $ pure 0
    discarded <- newIORef False
    let allocate use = use (1 :: Int) `onException` writeIORef discarded True
    result <- try (observeResource tracker "Module:1" allocate id $ \_ ->
      ioError $ userError "secret SQL and credentials") :: IO (Either IOException ())
    result `shouldSatisfy` either (const True) (const False)
    readIORef discarded `shouldReturn` True
    after <- readPoolSnapshot tracker
    psWaiting after `shouldBe` 0
    psActive after `shouldBe` []
    psFailures after `shouldBe` 1
    LBS.unpack (encode $ snapshotValue after) `shouldSatisfy` (not . isInfixOf "secret")

  it "cleans up queued counts after acquisition failure" $ do
    tracker <- newObservation $ pure 0
    _ <- try (observeResource tracker "Module:2" (\_ -> ioError $ userError "connection failed")
      (const 0) (\() -> pure ())) :: IO (Either IOException ())
    after <- readPoolSnapshot tracker
    psWaiting after `shouldBe` 0
    psAcquired after `shouldBe` 0
    psFailures after `shouldBe` 1

  it "preserves active checkout metadata across counter drains" $ do
    clock <- newIORef 0
    tracker <- newObservation $ readIORef clock
    observeResource tracker "Module:3" ($ ()) (const 7) $ \_ -> do
      _ <- drainPoolSnapshot tracker
      writeIORef clock 1_000_000_000
      after <- readPoolSnapshot tracker
      psActive after `shouldBe` [ActiveCheckout "Module:3" 7 1000]
      poolUnderPressure after `shouldBe` True
    after <- drainPoolSnapshot tracker
    psReleased after `shouldBe` 1
    psHoldMaxMs after `shouldBe` 1000

  it "preserves cancellation while queued without leaving phantom waiters" $ do
    clock <- newIORef 0
    tracker <- newObservation $ readIORef clock
    entered <- newEmptyMVar
    gate <- newEmptyMVar
    worker <- async $ observeResource tracker "Module:4"
      (\use -> putMVar entered () >> takeMVar gate >>= use) (const 1) (\() -> pure ())
    takeMVar entered
    pending <- drainPoolSnapshot tracker
    psWaiting pending `shouldBe` 1
    writeIORef clock 5_000_000_000
    cancel worker
    after <- readPoolSnapshot tracker
    psWaiting after `shouldBe` 0
    psFailures after `shouldBe` 1
    psAbandoned after `shouldBe` 1
    psWaitMaxMs after `shouldBe` 5000
    poolUnderPressure after `shouldBe` True

  it "preserves cancellation while holding a connection" $ do
    tracker <- newObservation $ pure 0
    entered <- newEmptyMVar
    gate <- newEmptyMVar
    worker <- async $ observeResource tracker "Module:5" ($ ()) (const 2) $ \_ ->
      putMVar entered () >> takeMVar gate
    takeMVar entered
    cancel worker
    after <- readPoolSnapshot tracker
    psActive after `shouldBe` []
    psReleased after `shouldBe` 1
    psFailures after `shouldBe` 1

  it "isolates separate pools" $ do
    first <- newObservation $ pure 0
    second <- newObservation $ pure 0
    observeResource first "Module:6" ($ ()) (const 1) $ \_ -> do
      other <- readPoolSnapshot second
      psActive other `shouldBe` []
      psAcquired other `shouldBe` 0

  it "accounts for concurrent acquisitions and failures without losing counters" $ do
    tracker <- newObservation $ pure 0
    mapConcurrently_ (\worker -> replicateM_ 50 $ do
      _ <- try (observeResource tracker "Module:7" ($ ()) (const worker) $ \_ ->
        if even worker then pure () else ioError $ userError "expected") :: IO (Either IOException ())
      pure ()) [1 .. 16]
    after <- drainPoolSnapshot tracker
    psAcquired after `shouldBe` 800
    psReleased after `shouldBe` 800
    psFailures after `shouldBe` 400
    psWaiting after `shouldBe` 0
    psActive after `shouldBe` []

  it "does not underflow duration if the injected clock goes backwards" $ do
    clock <- newIORef 2_000_000_000
    tracker <- newObservation $ readIORef clock
    observeResource tracker "Module:8" ($ ()) (const 1) $ \_ -> writeIORef clock 0
    after <- readPoolSnapshot tracker
    psHoldTotalMs after `shouldBe` 0
    psHoldMaxMs after `shouldBe` 0

  it "adds a libpq connect deadline to keyword strings and either URL form" $ do
    diagnosticConnectionString "host=localhost dbname=example" `shouldBe`
      "host=localhost dbname=example connect_timeout=2"
    diagnosticConnectionString "postgres://localhost/example" `shouldBe`
      "postgres://localhost/example?connect_timeout=2"
    diagnosticConnectionString "postgresql://localhost/example?sslmode=require" `shouldBe`
      "postgresql://localhost/example?sslmode=require&connect_timeout=2"
