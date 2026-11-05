-- Timings contain fixed stage names only, never operation or credential data.
module Plether.AA.Timing (Timing, newTiming, timingIdentifier, timed, timingCount, timingHeaders, observeLogTiming) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import Data.Unique (hashUnique, newUnique)
import Data.Word (Word64)
import qualified Data.Map.Strict as Map
import GHC.Clock (getMonotonicTimeNSec)
import Plether.Logging (LogTiming (..), field, logInfo)

data Timing = Timing Text Word64 (IORef [(Text, Double)]) (IORef (Map.Map Text Integer)) (IORef LogTiming)

timingIdentifier :: Timing -> Text
timingIdentifier (Timing identifier _ _ _ _) = identifier

newTiming :: IO Timing
newTiming = do
  identifier <- newUnique
  now <- getMonotonicTimeNSec
  Timing (T.pack $ show now <> "-" <> show (hashUnique identifier)) now <$> newIORef [] <*> newIORef Map.empty <*> newIORef (LogTiming 0 0)

observeLogTiming :: Timing -> LogTiming -> IO ()
observeLogTiming (Timing _ _ _ _ logs) (LogTiming wait write) =
  atomicModifyIORef' logs $ \(LogTiming previousWait previousWrite) ->
    (LogTiming (previousWait+wait) (previousWrite+write), ())

timingCount :: Timing -> Text -> IO ()
timingCount (Timing _ _ _ counts _) name = atomicModifyIORef' counts $ \values ->
  (Map.insertWith (+) name 1 values, ())

timed :: MonadIO m => Timing -> Text -> m a -> m a
timed (Timing _ _ values _ _) stage action = do
  start <- liftIO getMonotonicTimeNSec
  result <- action
  end <- liftIO getMonotonicTimeNSec
  liftIO $ atomicModifyIORef' values $ \xs ->
    ((stage, fromIntegral (end - start) / 1_000_000) : xs, ())
  pure result

timingHeaders :: Timing -> IO (Text, Text)
timingHeaders (Timing identifier started values counts logs) = do
  ended <- getMonotonicTimeNSec
  stages <- reverse <$> readIORef values
  counters <- readIORef counts
  LogTiming wait write <- readIORef logs
  let rendered = T.intercalate ", " [name <> ";dur=" <> T.pack (show (round duration :: Integer)) | (name, duration) <- stages <> [("log_lock_wait", wait), ("log_write", write)]]
  logInfo "aa_preparation_timing" "Native AA request timings"
    [field "request_id" identifier, field "stages" rendered, field "counts" $ T.pack $ show $ Map.toList counters,
     field "duration_ms" (fromIntegral (ended-started) / 1_000_000 :: Double),
     field "log_lock_wait_ms" wait, field "log_write_ms" write]
  pure (identifier, rendered)
