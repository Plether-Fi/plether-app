-- Advisory only. CloudWatch retains per-order observations; this bounded cache
-- coalesces repeated observations and is never an execution/recovery authority.
module Plether.Keeper.Deferrals
  ( Deferral (..), observeDeferral, pendingReasonCode
  , recordDeferral, finishDeferrals
  ) where

import Data.IORef (IORef, atomicModifyIORef', newIORef)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Plether.Insights.Registration.Crypto (generateUuidV4)
import Plether.Logging (LogField, field, logInfo)
import System.IO.Unsafe (unsafePerformIO)

data Deferral = Deferral
  { firstAt :: Integer, lastAt :: Integer, emittedAt :: Integer
  , occurrences :: Integer, diagnosticRef :: Text
  , lastGasLimit :: Integer, deadlineAt :: Integer, lastPayloadTime :: Maybe Integer
  } deriving stock (Eq, Show)

-- Emit each order/reason's first observation, then a cumulative summary every
-- minute. Reason changes and other orders never share suppression state.
observeDeferral :: Integer -> Text -> Maybe Deferral -> (Deferral, Bool)
observeDeferral now ref previous = case previous of
  Nothing -> (Deferral now now now 1 ref 0 0 Nothing, True)
  Just old ->
    let emit = now - emittedAt old >= 60
    in (old { lastAt = now, occurrences = occurrences old + 1
            , emittedAt = if emit then now else emittedAt old }, emit)

pendingReasonCode :: Integer -> Text
pendingReasonCode n = case n of
  1 -> "KEEPER_CLOSE_ONLY"
  2 -> "KEEPER_SAME_BLOCK"
  3 -> "KEEPER_MEV_BOUNDARY"
  4 -> "KEEPER_HISTORICAL_PRICE_UNAVAILABLE"
  5 -> "KEEPER_INSUFFICIENT_GAS"
  6 -> "KEEPER_MARK_PRICE_OUT_OF_ORDER"
  7 -> "KEEPER_ENGINE_FAILURE"
  8 -> "KEEPER_RECEIPT_FAILURE"
  9 -> "KEEPER_CLEANUP_LIMIT"
  _ -> "KEEPER_PENDING_UNKNOWN"

type Key = (Text, Integer, Text)
{-# NOINLINE observations #-}
observations :: IORef (Map.Map Key Deferral)
observations = unsafePerformIO $ newIORef Map.empty

recordDeferral :: Text -> Integer -> Text -> Integer -> Integer -> Maybe Integer -> IO ()
recordDeferral router order reason gasLimit validUntil payloadTime = do
  now <- floor <$> getPOSIXTime
  ref <- generateUuidV4
  let key = (T.toLower router, order, reason)
  (entry, shouldEmit, evicted) <- atomicModifyIORef' observations $ \old ->
    let live = Map.filter (\v -> now - lastAt v < 3600) old
        -- Admission bound is independent of user-supplied identifiers.
        bounded = if Map.size live >= 4096 && Map.notMember key live
          then Map.delete (fst $ minimumByLast live) live else live
        (observed, emit) = observeDeferral now ref (Map.lookup key bounded)
        value = observed { lastGasLimit = gasLimit, deadlineAt = validUntil, lastPayloadTime = payloadTime }
    in (Map.insert key value bounded, (value, emit, Map.size old - Map.size bounded))
  if evicted > 0 then logInfo "keeper_deferral_cache_evicted" "Advisory deferral cache entries evicted"
    [field "occurrence_count" evicted] else pure ()
  if shouldEmit then emitObservation "keeper_transaction_deferred" router order reason entry [] else pure ()
 where
  minimumByLast entries = foldl1 (\a b -> if lastAt (snd a) <= lastAt (snd b) then a else b) $ Map.toList entries

finishDeferrals :: Text -> Integer -> IO ()
finishDeferrals router order = do
  entries <- atomicModifyIORef' observations $ \old ->
    let (finished, remaining) = Map.partitionWithKey (\(r,o,_) _ -> r == T.toLower router && o == order) old
    in (remaining, Map.toList finished)
  mapM_ (\((_,_,reason),entry) -> emitObservation "keeper_order_deferral_summary" router order reason entry []) entries

emitObservation :: Text -> Text -> Integer -> Text -> Deferral -> [LogField] -> IO ()
emitObservation event router order reason entry extra = logInfo event "Keeper preflight deferral observation"
  ([ field "order_router" router, field "order_id" order
   , field "attempt_id" $ diagnosticRef entry, field "reason_code" reason
   , field "stage" ("execution" :: Text), field "outcome" ("pending" :: Text)
   , field "first_observed_at" $ firstAt entry, field "last_observed_at" $ lastAt entry
   , field "occurrence_count" $ occurrences entry
   , field "duration_ms" $ max 0 (lastAt entry - firstAt entry) * 1000
   , field "gas_limit" $ lastGasLimit entry
   , field "remaining_deadline_seconds" $ max 0 (deadlineAt entry - lastAt entry)
   , field "payload_publish_time" $ lastPayloadTime entry, field "valid_until" $ deadlineAt entry
   ] <> extra)
