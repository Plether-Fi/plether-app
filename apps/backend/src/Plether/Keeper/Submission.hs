module Plether.Keeper.Submission
  ( readPreparationInputs
  , receiptPollDelayMicros
  , ReceiptPollError (..)
  , pollReceiptWith
  ) where

import Control.Concurrent.Async (Concurrently (..), runConcurrently)
import Data.Word (Word64)

-- Only read-only inputs overlap. The caller retains the singleton signing
-- lease, checks every required result, and journals before any broadcast.
-- Other deployments keep their existing sequential preparation behavior.
readPreparationInputs :: Integer -> IO a -> IO b -> IO c -> IO d -> IO (a, b, c, d)
readPreparationInputs chainId nonce gas fee priority
  | chainId == 421614 = runConcurrently $
      (,,,) <$> Concurrently nonce <*> Concurrently gas
        <*> Concurrently fee <*> Concurrently priority
  | otherwise = do
      a <- nonce
      b <- gas
      c <- fee
      d <- priority
      pure (a, b, c, d)

receiptPollDelayMicros :: Integer -> Word64 -> Int
receiptPollDelayMicros chainId elapsedNs
  | chainId == 421614 && elapsedNs < 1_000_000_000 = 50_000
  | elapsedNs < 5_000_000_000 = 250_000
  | otherwise = 1_000_000

data ReceiptPollError e = ReceiptReadFailed e | ReceiptPollTimeout
  deriving stock (Eq, Show)

-- Clock and sleep injection make deadline, backoff and error behavior testable
-- without waiting in tests. Faster observation never relaxes receipt checks or
-- turns an uncertain send into permission to submit another transaction.
pollReceiptWith
  :: Integer
  -> IO Word64
  -> (Int -> IO ())
  -> IO (Either e (Maybe a))
  -> IO (Either (ReceiptPollError e) (a, Int))
pollReceiptWith chainId clock sleep readReceipt = do
  started <- clock
  let poll count = do
        now <- clock
        if now - started >= 120_000_000_000
          then pure $ Left ReceiptPollTimeout
          else readReceipt >>= \result -> case result of
            Left err -> pure $ Left $ ReceiptReadFailed err
            Right (Just receipt) -> pure $ Right (receipt, count + 1)
            Right Nothing -> do
              sleep $ receiptPollDelayMicros chainId (now - started)
              poll (count + 1)
  poll 0
