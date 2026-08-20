module Plether.CacheSpec (spec) where

import Control.Concurrent
  ( forkIO
  , newEmptyMVar
  , putMVar
  , takeMVar
  , threadDelay
  , tryTakeMVar
  )
import Control.Exception (ErrorCall (..), throwIO, try)
import Data.Either (isLeft)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Word (Word64)
import Plether.Cache
  ( SingleFlightCache
  , SingleFlightSource (..)
  , newRefreshingSingleFlightCache
  , newSingleFlightCache
  , runSingleFlightCache
  , runSingleFlightCacheFresh
  )
import Test.Hspec

newIntCache :: Int -> Word64 -> IO (SingleFlightCache String Int)
newIntCache = newSingleFlightCache

newRefreshingIntCache
  :: Int -> Word64 -> Word64 -> IO (SingleFlightCache String Int)
newRefreshingIntCache = newRefreshingSingleFlightCache

tryErrorCall :: IO a -> IO (Either ErrorCall a)
tryErrorCall = try

awaitMemory :: SingleFlightCache String Int -> String -> Int -> IO ()
awaitMemory cache key expected = go (200 :: Int)
  where
    go 0 = expectationFailure "background cache refresh did not become visible"
    go attempts = do
      result <- runSingleFlightCache cache key (const True) (pure (-1))
      if result == (SingleFlightMemory, expected)
        then pure ()
        else threadDelay 1_000 >> go (attempts - 1)

spec :: Spec
spec = describe "runSingleFlightCache" $ do
  it "reuses an accepted value until its TTL expires" $ do
    cache <- newIntCache 4 20_000_000
    calls <- newIORef (0 :: Int)
    let load = atomicModifyIORef' calls $ \count -> (count + 1, count + 1)

    runSingleFlightCache cache "page" (const True) load
      `shouldReturn` (SingleFlightLoaded, 1)
    runSingleFlightCache cache "page" (const True) load
      `shouldReturn` (SingleFlightMemory, 1)
    threadDelay 30_000
    runSingleFlightCache cache "page" (const True) load
      `shouldReturn` (SingleFlightLoaded, 2)

  it "does not retain rejected loader results" $ do
    cache <- newIntCache 4 5_000_000_000
    calls <- newIORef (0 :: Int)
    let load = atomicModifyIORef' calls $ \count -> (count + 1, count + 1)

    runSingleFlightCache cache "page" even load
      `shouldReturn` (SingleFlightLoaded, 1)
    runSingleFlightCache cache "page" even load
      `shouldReturn` (SingleFlightLoaded, 2)
    runSingleFlightCache cache "page" even load
      `shouldReturn` (SingleFlightMemory, 2)

  it "coalesces concurrent misses into one loader call" $ do
    cache <- newIntCache 4 5_000_000_000
    calls <- newIORef (0 :: Int)
    started <- newEmptyMVar
    release <- newEmptyMVar
    firstResult <- newEmptyMVar
    secondResult <- newEmptyMVar
    let load = do
          atomicModifyIORef' calls $ \count -> (count + 1, ())
          putMVar started ()
          takeMVar release
          pure (7 :: Int)

    _ <- forkIO $ runSingleFlightCache cache "page" (const True) load >>= putMVar firstResult
    takeMVar started
    _ <- forkIO $ runSingleFlightCache cache "page" (const True) load >>= putMVar secondResult
    threadDelay 10_000
    readIORef calls `shouldReturn` 1
    putMVar release ()
    first <- takeMVar firstResult
    second <- takeMVar secondResult
    [first, second] `shouldMatchList`
      [ (SingleFlightLoaded, 7)
      , (SingleFlightCoalesced, 7)
      ]

  it "releases the key when a loader throws" $ do
    cache <- newIntCache 4 5_000_000_000

    failed <-
      tryErrorCall $
        runSingleFlightCache cache "page" (const True) $ throwIO $ ErrorCall "failed"
    failed `shouldSatisfy` isLeft
    runSingleFlightCache cache "page" (const True) (pure 9)
      `shouldReturn` (SingleFlightLoaded, 9)

  it "serves a bounded stale value while one background refresh runs" $ do
    cache <- newRefreshingIntCache 4 100_000_000 2_000_000_000
    refreshStarted <- newEmptyMVar
    releaseRefresh <- newEmptyMVar
    let refresh = do
          putMVar refreshStarted ()
          takeMVar releaseRefresh
          pure (2 :: Int)

    runSingleFlightCache cache "page" (const True) (pure 1)
      `shouldReturn` (SingleFlightLoaded, 1)
    threadDelay 120_000
    runSingleFlightCache cache "page" (const True) refresh
      `shouldReturn` (SingleFlightStale, 1)
    takeMVar refreshStarted
    runSingleFlightCache cache "page" (const True) (pure 3)
      `shouldReturn` (SingleFlightStale, 1)
    putMVar releaseRefresh ()
    awaitMemory cache "page" 2

  it "keeps stale data available when a background refresh throws" $ do
    cache <- newRefreshingIntCache 4 100_000_000 2_000_000_000
    failedRefreshStarted <- newEmptyMVar
    successfulRefreshStarted <- newEmptyMVar

    runSingleFlightCache cache "page" (const True) (pure 1)
      `shouldReturn` (SingleFlightLoaded, 1)
    threadDelay 120_000
    runSingleFlightCache cache "page" (const True) (do
      putMVar failedRefreshStarted ()
      throwIO $ ErrorCall "refresh failed")
      `shouldReturn` (SingleFlightStale, 1)
    takeMVar failedRefreshStarted
    threadDelay 10_000
    runSingleFlightCache cache "page" (const True) (do
      putMVar successfulRefreshStarted ()
      pure 2)
      `shouldReturn` (SingleFlightStale, 1)
    takeMVar successfulRefreshStarted
    awaitMemory cache "page" 2

  it "serializes stale refreshes across different keys" $ do
    cache <- newRefreshingIntCache 4 100_000_000 2_000_000_000
    firstStarted <- newEmptyMVar
    secondStarted <- newEmptyMVar
    releaseFirst <- newEmptyMVar
    let refreshFirst = do
          putMVar firstStarted ()
          takeMVar releaseFirst
          pure (11 :: Int)
        refreshSecond = putMVar secondStarted () >> pure (22 :: Int)

    runSingleFlightCache cache "first" (const True) (pure 1)
      `shouldReturn` (SingleFlightLoaded, 1)
    runSingleFlightCache cache "second" (const True) (pure 2)
      `shouldReturn` (SingleFlightLoaded, 2)
    threadDelay 120_000
    runSingleFlightCache cache "first" (const True) refreshFirst
      `shouldReturn` (SingleFlightStale, 1)
    takeMVar firstStarted
    runSingleFlightCache cache "second" (const True) refreshSecond
      `shouldReturn` (SingleFlightStale, 2)
    threadDelay 10_000
    tryTakeMVar secondStarted `shouldReturn` Nothing
    putMVar releaseFirst ()
    takeMVar secondStarted
    awaitMemory cache "first" 11
    awaitMemory cache "second" 22

  it "waits for an authoritative load when fresh data is explicitly required" $ do
    cache <- newRefreshingIntCache 4 5_000_000_000 10_000_000_000

    runSingleFlightCache cache "page" (const True) (pure 1)
      `shouldReturn` (SingleFlightLoaded, 1)
    runSingleFlightCacheFresh cache "page" (const True) (pure 2)
      `shouldReturn` (SingleFlightLoaded, 2)
    runSingleFlightCache cache "page" (const True) (pure 3)
      `shouldReturn` (SingleFlightMemory, 2)

  it "evicts the oldest entry when the bound is reached" $ do
    cache <- newIntCache 1 5_000_000_000
    calls <- newIORef (0 :: Int)
    let load = atomicModifyIORef' calls $ \count -> (count + 1, count + 1)

    runSingleFlightCache cache "first" (const True) load
      `shouldReturn` (SingleFlightLoaded, 1)
    runSingleFlightCache cache "second" (const True) load
      `shouldReturn` (SingleFlightLoaded, 2)
    runSingleFlightCache cache "first" (const True) load
      `shouldReturn` (SingleFlightLoaded, 3)
