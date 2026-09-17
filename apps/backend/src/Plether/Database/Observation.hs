-- Pool-local, bounded bookkeeping. No SQL, parameters, URLs, or exception text.
module Plether.Database.Observation
  ( Observation
  , PoolSnapshot (..)
  , ActiveCheckout (..)
  , newObservation
  , observeResource
  , readPoolSnapshot
  , drainPoolSnapshot
  , poolUnderPressure
  ) where

import Control.Exception (bracket, finally, mask, onException)
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Word (Word64)

data ActiveCheckout = ActiveCheckout
  { acSource :: !Text
  , acBackendPid :: !Int
  , acHeldMs :: !Double
  } deriving stock (Eq, Show)

data PoolSnapshot = PoolSnapshot
  { psWaiting :: !Int
  , psActive :: ![ActiveCheckout]
  , psAcquired :: !Int
  , psReleased :: !Int
  , psFailures :: !Int
  , psAbandoned :: !Int
  , psWaitTotalMs :: !Double
  , psHoldTotalMs :: !Double
  , psWaitMaxMs :: !Double
  , psHoldMaxMs :: !Double
  , psSlowestWaitSource :: !Text
  , psSlowestHoldSource :: !Text
  } deriving stock (Eq, Show)

data State = State
  { nextId :: !Integer
  , waiting :: !Int
  , active :: !(Map.Map Integer (Text, Int, Word64))
  , totals :: !PoolSnapshot
  }

data Observation = Observation (IO Word64) (IORef State)

emptySnapshot :: PoolSnapshot
emptySnapshot = PoolSnapshot 0 [] 0 0 0 0 0 0 0 0 "none" "none"

newObservation :: IO Word64 -> IO Observation
newObservation clock = Observation clock <$> newIORef (State 0 0 Map.empty emptySnapshot)

milliseconds :: Word64 -> Word64 -> Double
milliseconds start end = fromIntegral (end - min start end) / 1_000_000

-- The allocator retains ownership of release/discard semantics. In particular,
-- exceptions still escape through resource-pool so broken connections are dropped.
observeResource
  :: Observation
  -> Text
  -> ((resource -> IO value) -> IO value)
  -> (resource -> Int)
  -> (resource -> IO value)
  -> IO value
observeResource (Observation clock ref) source allocate backendPid action = mask $ \restore -> do
  started <- clock
  queued <- newIORef True
  atomicModifyIORef' ref $ \s -> (s {waiting = waiting s + 1}, ())
  let leaveQueue = do
        stillQueued <- readIORef queued
        if stillQueued
          then do
            now <- clock
            atomicModifyIORef' ref $ \s ->
              let elapsed = milliseconds started now
                  t = totals s
                  t' = t
                    { psAbandoned = psAbandoned t + 1
                    , psWaitTotalMs = psWaitTotalMs t + elapsed
                    , psWaitMaxMs = max (psWaitMaxMs t) elapsed
                    , psSlowestWaitSource = if elapsed >= psWaitMaxMs t then source else psSlowestWaitSource t
                    }
               in (s {waiting = waiting s - 1, totals = t'}, ())
          else pure ()
      failed = atomicModifyIORef' ref $ \s ->
        (s {totals = (totals s) {psFailures = psFailures (totals s) + 1}}, ())
      acquired resource = do
        now <- clock
        writeIORef queued False
        atomicModifyIORef' ref $ \s ->
          let elapsed = milliseconds started now
              t = totals s
              t' = t
                { psAcquired = psAcquired t + 1
                , psWaitTotalMs = psWaitTotalMs t + elapsed
                , psWaitMaxMs = max (psWaitMaxMs t) elapsed
                , psSlowestWaitSource = if elapsed >= psWaitMaxMs t then source else psSlowestWaitSource t
                }
              identifier = nextId s
           in (s {nextId = identifier + 1, waiting = waiting s - 1,
                  active = Map.insert identifier (source, backendPid resource, now) (active s), totals = t'}, (identifier, now))
      released (identifier, began) = do
        now <- clock
        atomicModifyIORef' ref $ \s ->
          let elapsed = milliseconds began now
              t = totals s
              t' = t
                { psReleased = psReleased t + 1
                , psHoldTotalMs = psHoldTotalMs t + elapsed
                , psHoldMaxMs = max (psHoldMaxMs t) elapsed
                , psSlowestHoldSource = if elapsed >= psHoldMaxMs t then source else psSlowestHoldSource t
                }
           in (s {active = Map.delete identifier (active s), totals = t'}, ())
  (restore (allocate $ \resource -> bracket (acquired resource) released $ \_ -> action resource)
    `onException` failed) `finally` leaveQueue

snapshotAt :: Word64 -> State -> PoolSnapshot
snapshotAt now s = (totals s)
  { psWaiting = waiting s
  , psActive = [ActiveCheckout source pid (milliseconds started now) | (source, pid, started) <- Map.elems (active s)]
  }

readPoolSnapshot :: Observation -> IO PoolSnapshot
readPoolSnapshot (Observation clock ref) = do
  now <- clock
  snapshotAt now <$> readIORef ref

-- Draining counters never forgets current waiters or checked-out connections.
drainPoolSnapshot :: Observation -> IO PoolSnapshot
drainPoolSnapshot (Observation clock ref) = do
  now <- clock
  atomicModifyIORef' ref $ \s -> (s {totals = emptySnapshot}, snapshotAt now s)

poolUnderPressure :: PoolSnapshot -> Bool
poolUnderPressure s = psWaiting s > 0 || psWaitMaxMs s >= 250
  || psHoldMaxMs s >= 1000 || any ((>= 1000) . acHeldMs) (psActive s)
