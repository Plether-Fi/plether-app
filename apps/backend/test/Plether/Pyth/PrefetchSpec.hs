module Plether.Pyth.PrefetchSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync)
import Control.Concurrent.Chan (newChan, readChan, writeChan)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (bracket_)
import Control.Monad (replicateM)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.List (sort)
import qualified Data.Text
import Plether.Pyth.Prefetch (retryAfterSeconds, runPriorityPrefetch, newProviderGate, withProviderGate)
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
