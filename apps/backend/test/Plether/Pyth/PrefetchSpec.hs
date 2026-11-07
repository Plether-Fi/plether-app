module Plether.Pyth.PrefetchSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync)
import Control.Concurrent.Chan (newChan, readChan, writeChan)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar, readMVar, tryPutMVar)
import Control.Exception (bracket_)
import Control.Monad (replicateM, replicateM_, void)
import Data.IORef (atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.List (sort)
import qualified Data.Text
import Plether.Pyth.Prefetch (retryAfterSeconds, runPriorityPrefetch, runPriorityPrefetchWithWakeup,
  runPriorityPrefetchWithPriorities, PrefetchPriority (..), newProviderGate, withProviderGate)
import Plether.Keeper.Wakeup (newWakeup, signalWakeup)
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "pending historical-price prefetch" $ do
  it "fetches twenty unique queue entries with at most two concurrent requests" $ do
    completed <- newChan
    active <- newIORef (0 :: Int, 0 :: Int)
    let enter = atomicModifyIORef' active $ \(n, peak) -> ((n + 1, max peak (n + 1)), ())
        leave = atomicModifyIORef' active $ \(n, peak) -> ((n - 1, peak), ())
        action key = bracket_ enter leave $ do
          threadDelay 1_000
          writeChan completed key
          pure $ Right ()
        candidates = [(key, action key) | key <- [1..20 :: Int]]
    withAsync (runPriorityPrefetch $ pure $ take 1 candidates <> candidates) $ \_ -> do
      results <- timeout 5_000_000 $ replicateM 20 $ readChan completed
      fmap sort results `shouldBe` Just [1..20]
      readIORef active >>= \(_, peak) -> peak `shouldBe` 2
      timeout 20_000 (readChan completed) `shouldReturn` Nothing

  it "keeps the head in flight while another worker prepares later orders" $ do
    started <- newChan
    releaseHead <- newEmptyMVar
    let action key = do
          writeChan started key
          if key == (1 :: Int) then takeMVar releaseHead else pure ()
          pure $ Right ()
    withAsync (runPriorityPrefetch $ pure [(key, action key) | key <- [1..3]]) $ \_ -> do
      results <- timeout 2_000_000 $ replicateM 3 $ readChan started
      fmap sort results `shouldBe` Just [1,2,3]
      putMVar releaseHead ()

  it "invalidates the cached empty queue and wakes both price workers" $ do
    wakeup <- newWakeup
    loaded <- newEmptyMVar
    release <- newEmptyMVar
    started <- newChan
    candidates <- newIORef ([] :: [Int])
    let action key = do
          writeChan started key
          readMVar release
          pure $ Right ()
        load = do
          keys <- readIORef candidates
          void $ tryPutMVar loaded ()
          pure [(key, action key) | key <- keys]
    withAsync (runPriorityPrefetchWithWakeup (Just wakeup) load) $ \_ -> do
      timeout 2_000_000 (takeMVar loaded) `shouldReturn` Just ()
      threadDelay 50_000
      writeIORef candidates [1,2]
      signalWakeup wakeup
      -- Both original idle waits would last a second. The second worker must
      -- receive its own wake even while the first blocks on the head fetch.
      results <- timeout 500_000 $ replicateM 2 $ readChan started
      fmap sort results `shouldBe` Just [1,2]
      putMVar release ()

  it "keeps successful job cooldowns across duplicate notifications" $ do
    wakeup <- newWakeup
    completed <- newChan
    calls <- newIORef (0 :: Int)
    let action = do
          atomicModifyIORef' calls $ \n -> (n + 1, ())
          writeChan completed ()
          pure $ Right ()
    withAsync (runPriorityPrefetchWithWakeup (Just wakeup) $ pure [(1 :: Int, action)]) $ \_ -> do
      timeout 2_000_000 (readChan completed) `shouldReturn` Just ()
      threadDelay 50_000
      replicateM_ 100 $ signalWakeup wakeup
      timeout 300_000 (readChan completed) `shouldReturn` Nothing
      readIORef calls `shouldReturn` 1

  it "reserves a worker for a new canonical head while provisional preparation stalls" $ do
    wakeup <- newWakeup
    started <- newChan
    release <- newEmptyMVar
    candidates <- newIORef [(1 :: Int, ProvisionalPrefetch), (2, ProvisionalPrefetch)]
    let action key = do
          writeChan started key
          if key == 1 then readMVar release else pure ()
          pure $ Right ()
        load = readIORef candidates >>= \entries -> pure [(key, priority, action key) | (key, priority) <- entries]
    withAsync (runPriorityPrefetchWithPriorities (Just wakeup) load) $ \_ -> do
      timeout 2_000_000 (readChan started) `shouldReturn` Just 1
      timeout 100_000 (readChan started) `shouldReturn` Nothing
      -- Canonical work is deliberately appended after two provisional jobs;
      -- scheduler priority and its reserved capacity must still start it now.
      writeIORef candidates [(1, ProvisionalPrefetch), (2, ProvisionalPrefetch), (3, CanonicalPrefetch)]
      signalWakeup wakeup
      timeout 500_000 (readChan started) `shouldReturn` Just 3
      timeout 100_000 (readChan started) `shouldReturn` Nothing
      putMVar release ()
      -- Completing the first speculative request releases its reservation.
      timeout 1_500_000 (readChan started) `shouldReturn` Just 2

  it "retains a successful cooldown when an optional hint disappears and returns" $ do
    wakeup <- newWakeup
    completed <- newChan
    loaded <- newChan
    candidates <- newIORef [1 :: Int]
    let action = writeChan completed () >> pure (Right ())
        load = do
          keys <- readIORef candidates
          writeChan loaded keys
          pure [(key, action) | key <- keys]
    withAsync (runPriorityPrefetchWithWakeup (Just wakeup) load) $ \_ -> do
      timeout 2_000_000 (readChan completed) `shouldReturn` Just ()
      timeout 2_000_000 (readChan loaded) `shouldReturn` Just [1]
      threadDelay 50_000
      writeIORef candidates []
      signalWakeup wakeup
      timeout 500_000 (readChan loaded) `shouldReturn` Just []
      writeIORef candidates [1]
      signalWakeup wakeup
      timeout 500_000 (readChan loaded) `shouldReturn` Just [1]
      timeout 300_000 (readChan completed) `shouldReturn` Nothing

  it "does not bypass the provider cooldown when a wake adds another order" $ do
    wakeup <- newWakeup
    started <- newChan
    candidates <- newIORef [1 :: Int]
    let action key = do
          writeChan started key
          pure $ Left "Hermes returned HTTP 429; retry after 2"
        load = readIORef candidates >>= \keys -> pure [(key, action key) | key <- keys]
    withAsync (runPriorityPrefetchWithWakeup (Just wakeup) load) $ \_ -> do
      timeout 2_000_000 (readChan started) `shouldReturn` Just 1
      threadDelay 50_000
      writeIORef candidates [1,2]
      replicateM_ 10 $ signalWakeup wakeup
      timeout 300_000 (readChan started) `shouldReturn` Nothing

  it "shares provider throttling across latest and historical fetches" $ do
    gate <- newProviderGate
    calls <- newIORef (0 :: Int)
    let historical = modifyCount calls >> pure (Right ())
        modifyCount ref = atomicModifyIORef' ref $ \n -> (n + 1, ())
    withProviderGate gate (pure (Left "Hermes returned HTTP 429; retry after 2") :: IO (Either Data.Text.Text ()))
      `shouldReturn` Left "Hermes returned HTTP 429; retry after 2"
    timeout 50_000 (withProviderGate gate historical) `shouldReturn` Nothing
    readIORef calls `shouldReturn` 0

  it "pauses new reveal requests after provider throttling" $ do
    calls <- newIORef (0 :: Int)
    let action = do
          atomicModifyIORef' calls $ \n -> (n+1, ())
          pure $ Left "Hermes returned HTTP 429; retry after 2"
    withAsync (runPriorityPrefetch $ pure [(1 :: Int, action)]) $ \_ -> do
      threadDelay 1_100_000
      readIORef calls `shouldReturn` 1
    retryAfterSeconds "Hermes returned HTTP 429; retry after 2" `shouldBe` Just 2
    retryAfterSeconds "oracle unavailable" `shouldBe` Nothing
