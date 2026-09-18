-- Safe, bounded-cardinality diagnostics. Never carry wallets, hashes, RPC
-- responses or exception messages into the logging envelope.
module Plether.Insights.SnapshotObservability
  ( SnapshotRejectionReason (..)
  , SnapshotRejection (..)
  , snapshotRejectionCode
  , snapshotRejectionIsDefect
  , logSnapshotRejection
  , logSnapshotCycleException
  , logSnapshotHealthException
  , SnapshotProgress (..)
  , initialSnapshotProgress
  , advanceSnapshotProgress
  , logSnapshotProgress
  ) where

import Control.Exception (Exception, SomeException, SomeAsyncException, fromException, throwIO)
import Data.Text (Text)
import qualified Data.Text.Encoding as TextEncoding
import Data.Time (UTCTime, diffUTCTime)
import Database.PostgreSQL.Simple (SqlError (..))
import qualified Plether.Logging as Log

data SnapshotRejectionReason
  = InputEpochChanged | ParticipantSetChanged | CompetitionFinalized
  | CompetitionMissing | HistoryCursorNotReady | AccountLensChanged
  | MixedBatchIdentity | DuplicateParticipant
  deriving stock (Show, Eq, Enum, Bounded)

data SnapshotRejection = SnapshotRejection
  { srReason :: SnapshotRejectionReason
  , srKind :: Text
  , srBlock :: Integer
  , srCapturedEpoch :: Maybe Integer
  , srCurrentEpoch :: Maybe Integer
  , srCapturedParticipants :: Int
  , srCurrentParticipants :: Maybe Int
  } deriving stock (Show, Eq)
instance Exception SnapshotRejection

snapshotRejectionCode :: SnapshotRejectionReason -> Text
snapshotRejectionCode = \case
  InputEpochChanged -> "input_epoch_changed"
  ParticipantSetChanged -> "participant_set_changed"
  CompetitionFinalized -> "competition_finalized"
  CompetitionMissing -> "competition_missing"
  HistoryCursorNotReady -> "history_cursor_not_ready"
  AccountLensChanged -> "account_lens_changed"
  MixedBatchIdentity -> "mixed_batch_identity"
  DuplicateParticipant -> "duplicate_participant"

snapshotRejectionIsDefect :: SnapshotRejectionReason -> Bool
snapshotRejectionIsDefect reason = reason `elem` [MixedBatchIdentity, DuplicateParticipant]

-- Caller must have rolled back before invoking this, including lock timing.
logSnapshotRejection :: Double -> Maybe Double -> Maybe Double -> SnapshotRejection -> IO ()
logSnapshotRejection elapsed wait held SnapshotRejection {..} =
  (if snapshotRejectionIsDefect srReason then Log.logError else Log.logInfo)
    "insights_snapshot_rejected" "Snapshot publication rejected"
    [ Log.field "reason" $ snapshotRejectionCode srReason
    , Log.field "snapshot_kind" srKind
    , Log.field "block_number" srBlock
    , Log.field "captured_epoch" srCapturedEpoch
    , Log.field "current_epoch" srCurrentEpoch
    , Log.field "captured_participant_count" srCapturedParticipants
    , Log.field "current_participant_count" srCurrentParticipants
    , Log.field "elapsed_ms" elapsed
    , Log.field "lock_wait_ms" wait
    , Log.field "lock_held_ms" held
    , Log.field "retryable" $ not $ snapshotRejectionIsDefect srReason
    ]

-- Typed rejections have already been logged after rollback. Cancellation must
-- retain its normal semantics; SQL errors retain SQLSTATE, never their payload.
logSnapshotCycleException :: SomeException -> IO ()
logSnapshotCycleException = logSnapshotException "insights_snapshot_cycle_failed"

logSnapshotHealthException :: SomeException -> IO ()
logSnapshotHealthException = logSnapshotException "insights_snapshot_health_failed"

logSnapshotException :: Text -> SomeException -> IO ()
logSnapshotException event err
  | Just async <- fromException err :: Maybe SomeAsyncException = throwIO async
  | Just (_ :: SnapshotRejection) <- fromException err = pure ()
  | Just sql <- fromException err :: Maybe SqlError =
      Log.logWarn event "Snapshot database operation failed"
        [Log.field "error_class" ("database" :: Text), Log.field "sql_state" $ TextEncoding.decodeUtf8 $ sqlState sql]
  | otherwise = Log.logError event "Unexpected snapshot operation failure"
      [Log.field "error_class" ("unexpected" :: Text)]

data SnapshotProgress = SnapshotProgress
  { spUnsuccessfulCycles :: Int
  , spStalled :: Bool
  , spLastPublication :: Maybe UTCTime
  } deriving stock (Show, Eq)

initialSnapshotProgress :: SnapshotProgress
initialSnapshotProgress = SnapshotProgress 0 False Nothing

-- Nothing means outside the mutable live competition window. Just Nothing
-- means an active competition has not published yet. A returning IO action is
-- not evidence of publication: RPC skips must not reset this counter.
advanceSnapshotProgress :: Int -> UTCTime -> UTCTime -> Maybe (Maybe UTCTime) -> SnapshotProgress -> SnapshotProgress
advanceSnapshotProgress pollSeconds workerStarted now publication previous =
  case publication of
    Nothing -> initialSnapshotProgress
    Just latest ->
      let published = maybe False (\t -> maybe True (t >) $ spLastPublication previous) latest
          failures = if published then 0 else spUnsuccessfulCycles previous + 1
          age = realToFrac (diffUTCTime now $ maybe workerStarted id latest) :: Double
      in SnapshotProgress failures (failures >= 3 || age > fromIntegral (2 * max 10 pollSeconds)) latest

logSnapshotProgress :: UTCTime -> UTCTime -> Maybe (Maybe UTCTime) -> SnapshotProgress -> SnapshotProgress -> IO ()
logSnapshotProgress workerStarted now publication previous current = do
  let latest = publication >>= id
      age = fmap (realToFrac . diffUTCTime now) latest :: Maybe Double
      fields =
        [ Log.field "active" $ maybe False (const True) publication
        , Log.field "consecutive_unsuccessful_cycles" $ spUnsuccessfulCycles current
        , Log.field "last_publication_age_seconds" age
        , Log.field "seconds_without_publication" (realToFrac (diffUTCTime now $ maybe workerStarted id latest) :: Double)
        , Log.field "stalled" $ spStalled current
        ]
  Log.logInfo "insights_snapshot_progress" "Snapshot publication progress" fields
  if spStalled current
    then Log.logWarnEvery 300 "insights_snapshot_stalled" "Snapshot publication needs attention" fields
    else if spStalled previous && maybe False (const True) publication
      then Log.logInfo "insights_snapshot_recovered" "Snapshot publication recovered" fields
      else pure ()
