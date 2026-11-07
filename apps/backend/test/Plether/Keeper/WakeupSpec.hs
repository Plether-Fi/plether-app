module Plether.Keeper.WakeupSpec (spec) where

import Control.Concurrent.Async (async, cancel, mapConcurrently_, wait, waitCatch)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeAsyncException, fromException)
import Control.Monad (replicateM_)
import Plether.Keeper.Wakeup
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "commitment wakeups" $ do
  it "defaults off and enables only reviewed Sepolia modes" $ do
    parseWakeupMode 421614 Nothing `shouldBe` WakeupsOff
    parseWakeupMode 421614 (Just "off") `shouldBe` WakeupsOff
    parseWakeupMode 421614 (Just "observe") `shouldBe` WakeupsObserve
    parseWakeupMode 421614 (Just "active") `shouldBe` WakeupsActive
    parseWakeupMode 421614 (Just " ACTIVE ") `shouldBe` WakeupsActive
    parseWakeupMode 421614 (Just "invalid") `shouldBe` WakeupsOff
    parseWakeupMode 421614 (Just "") `shouldBe` WakeupsOff
    parseWakeupMode 42161 (Just "active") `shouldBe` WakeupsOff
    parseWakeupMode 1 (Just "observe") `shouldBe` WakeupsOff

  it "coalesces repeated signals into one bounded pending wakeup" $ do
    wakeup <- newWakeup
    replicateM_ 1000 $ signalWakeup wakeup
    waitWakeup wakeup 0 `shouldReturn` True
    waitWakeup wakeup 0 `shouldReturn` False

  it "coalesces concurrent publishers without blocking any publisher" $ do
    wakeup <- newWakeup
    timeout 1_000_000 (mapConcurrently_ (const $ signalWakeup wakeup) [1..1000 :: Int])
      `shouldReturn` Just ()
    waitWakeup wakeup 0 `shouldReturn` True
    waitWakeup wakeup 0 `shouldReturn` False

  it "wakes an idle waiter before its polling timeout" $ do
    wakeup <- newWakeup
    started <- newEmptyMVar
    waiter <- async $ putMVar started () >> waitWakeup wakeup 5_000_000
    takeMVar started
    signalWakeup wakeup
    timeout 1_000_000 (wait waiter) `shouldReturn` Just True

  it "keeps timeout polling available without manufacturing notifications" $ do
    wakeup <- newWakeup
    waitWakeup wakeup 1000 `shouldReturn` False
    waitWakeup wakeup (-1) `shouldReturn` False
    signalWakeup wakeup
    waitWakeup wakeup 1000 `shouldReturn` True

  it "preserves asynchronous cancellation of an idle waiter" $ do
    wakeup <- newWakeup
    started <- newEmptyMVar
    waiter <- async $ putMVar started () >> waitWakeup wakeup 60_000_000
    takeMVar started
    timeout 1_000_000 (cancel waiter) `shouldReturn` Just ()
    result <- waitCatch waiter
    case result of
      Left err -> (fromException err :: Maybe SomeAsyncException) `shouldSatisfy` maybe False (const True)
      Right _ -> expectationFailure "Idle cancellation was swallowed"
    signalWakeup wakeup
    waitWakeup wakeup 0 `shouldReturn` True

  it "does not block the main action on an unavailable listener database" $ do
    wakeup <- newWakeup
    timeout 1_000_000 (withWakeupListener
      "host=127.0.0.1 port=1 dbname=wakeup-test connect_timeout=1"
      "0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" wakeup (pure (42 :: Int)))
      `shouldReturn` Just 42
