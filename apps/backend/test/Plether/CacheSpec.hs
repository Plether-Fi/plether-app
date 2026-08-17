module Plether.CacheSpec (spec) where

import Control.Concurrent
  ( forkIO
  , newEmptyMVar
  , putMVar
  , takeMVar
  , threadDelay
  )
import Control.Exception (ErrorCall (..), throwIO, try)
import Data.Either (isLeft)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Word (Word64)
import Plether.Cache
  ( SingleFlightCache
  , SingleFlightSource (..)
  , newSingleFlightCache
  , runSingleFlightCache
  )
import Test.Hspec

newIntCache :: Int -> Word64 -> IO (SingleFlightCache String Int)
newIntCache = newSingleFlightCache

tryErrorCall :: IO a -> IO (Either ErrorCall a)
tryErrorCall = try

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
