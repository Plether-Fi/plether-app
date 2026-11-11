module Plether.Keeper.SubmissionSpec (spec) where

import Control.Concurrent.Async (async, cancel, wait, waitCatch)
import Control.Concurrent.Chan (newChan, readChan, writeChan)
import Control.Concurrent.MVar
import Control.Exception (finally, throwIO)
import Control.Monad (replicateM, forM_)
import Data.IORef
import Data.Text (Text)
import Data.Word (Word64)
import Plether.Keeper.Submission
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = do
  describe "Sepolia preparation reads" $ do
    it "starts all inputs concurrently and waits for every required read before returning" $ do
      started <- newChan
      release <- replicateM 4 newEmptyMVar
      delivered <- newEmptyMVar
      let readInput n = writeChan started n >> takeMVar (release !! n) >> pure n
      worker <- async $ readPreparationInputs 421614 (readInput 0) (readInput 1) (readInput 2) (readInput 3) >>= putMVar delivered
      begun <- timeout 2_000_000 $ replicateM 4 (readChan started)
      fmap length begun `shouldBe` Just 4
      forM_ (take 3 release) (`putMVar` ())
      tryReadMVar delivered `shouldReturn` Nothing
      putMVar (release !! 3) ()
      timeout 2_000_000 (wait worker) `shouldReturn` Just ()
      takeMVar delivered `shouldReturn` (0,1,2,3)
    it "preserves failed required results and the optional priority result without substituting evidence" $ do
      result <- readPreparationInputs 421614
        (pure (Left "nonce_unavailable" :: Either Text Int)) (pure (Right 123 :: Either Text Int))
        (pure (Right 456 :: Either Text Int)) (pure (Left "priority_unavailable" :: Either Text Int))
      result `shouldBe` (Left "nonce_unavailable", Right 123, Right 456, Left "priority_unavailable")
    it "cancels and joins sibling reads if one throws" $ do
      begun <- newEmptyMVar
      stopped <- newEmptyMVar
      never <- newEmptyMVar :: IO (MVar ())
      let blocked = (putMVar begun () >> takeMVar never) `finally` putMVar stopped ()
          failed = takeMVar begun >> (throwIO (userError "fixture failure") :: IO ())
      worker <- async $ readPreparationInputs 421614 blocked failed (pure ()) (pure ())
      outcome <- timeout 2_000_000 $ waitCatch worker
      outcome `shouldSatisfy` maybe False (either (const True) (const False))
      timeout 2_000_000 (takeMVar stopped) `shouldReturn` Just ()
    it "reads fresh values for each preparation and retains sequential behavior outside Sepolia" $ do
      calls <- newIORef ([] :: [Int])
      let readInput n = modifyIORef' calls (++ [n]) >> pure n
      readPreparationInputs 42161 (readInput 1) (readInput 2) (readInput 3) (readInput 4) `shouldReturn` (1,2,3,4)
      readPreparationInputs 42161 (readInput 1) (readInput 2) (readInput 3) (readInput 4) `shouldReturn` (1,2,3,4)
      readIORef calls `shouldReturn` [1,2,3,4,1,2,3,4]
  describe "receipt observation without changing receipt acceptance" $ do
    it "observes a receipt available at 120ms at 150ms on Sepolia versus 250ms on mainnet" $ do
      forM_ [(421614,150_000_000),(42161,250_000_000)] $ \(chain,expected) -> do
        clock <- newIORef (0 :: Word64)
        let readReceipt = do
              now <- readIORef clock
              pure $ Right $ if now >= 120_000_000 then Just ("original receipt" :: Text) else Nothing
            sleep us = modifyIORef' clock (+ fromIntegral us * 1000)
        result <- pollReceiptWith chain (readIORef clock) sleep readReceipt :: IO (Either (ReceiptPollError Text) (Text,Int))
        fmap fst result `shouldBe` Right "original receipt"
        readIORef clock `shouldReturn` expected
    it "returns an already available receipt immediately without rewriting a failed status" $ do
      sleeps <- newIORef (0 :: Int)
      result <- pollReceiptWith 421614 (pure 0) (\_ -> modifyIORef' sleeps (+1))
        (pure $ Right $ Just False) :: IO (Either (ReceiptPollError Text) (Bool,Int))
      result `shouldBe` Right (False,1)
      readIORef sleeps `shouldReturn` 0
    it "preserves RPC errors without retrying, sleeping or authorizing a replacement" $ do
      calls <- newIORef (0 :: Int)
      sleeps <- newIORef (0 :: Int)
      let failed = modifyIORef' calls (+1) >> pure (Left ("rpc unavailable" :: Text))
      result <- pollReceiptWith 421614 (pure 0) (\_ -> modifyIORef' sleeps (+1)) failed :: IO (Either (ReceiptPollError Text) ((),Int))
      result `shouldBe` Left (ReceiptReadFailed "rpc unavailable")
      readIORef calls `shouldReturn` 1
      readIORef sleeps `shouldReturn` 0
    it "backs off at one and five seconds while keeping the 120-second deadline" $ do
      clock <- newIORef (0 :: Word64)
      reads <- newIORef ([] :: [Word64])
      sleeps <- newIORef ([] :: [Int])
      let readReceipt = do
            now <- readIORef clock
            modifyIORef' reads (++ [now])
            pure $ Right Nothing
          sleep us = modifyIORef' sleeps (++ [us]) >> modifyIORef' clock (+ fromIntegral us * 1000)
      result <- pollReceiptWith 421614 (readIORef clock) sleep readReceipt :: IO (Either (ReceiptPollError Text) ((),Int))
      result `shouldBe` Left ReceiptPollTimeout
      readIORef clock `shouldReturn` 120_000_000_000
      readIORef reads >>= (`shouldSatisfy` all (<120_000_000_000))
      schedule <- readIORef sleeps
      take 20 schedule `shouldBe` replicate 20 50_000
      take 16 (drop 20 schedule) `shouldBe` replicate 16 250_000
      last schedule `shouldBe` 1_000_000
    it "propagates cancellation instead of converting it to an available receipt" $ do
      begun <- newEmptyMVar
      stopped <- newEmptyMVar
      never <- newEmptyMVar :: IO (MVar ())
      let readReceipt = (putMVar begun () >> takeMVar never >> pure (Right Nothing)) `finally` putMVar stopped ()
      worker <- async (pollReceiptWith 421614 (pure 0) (\_ -> pure ()) readReceipt :: IO (Either (ReceiptPollError Text) ((),Int)))
      timeout 2_000_000 (takeMVar begun) `shouldReturn` Just ()
      cancel worker
      timeout 2_000_000 (takeMVar stopped) `shouldReturn` Just ()
