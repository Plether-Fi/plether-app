module Plether.Database.CandleHistory
  ( CandleHistoryIngestionProgress (..)
  , CandleHistorySelection (..)
  , MarketReleaseEpoch (..)
  , appendCandleHistorySelection
  , candleHistorySchemaStatements
  , candleHistorySelectionIsAbsent
  , candleHistorySelectionIsLatest
  , completeCandleHistoryIngestionWindow
  , defaultCandleMarketId
  , effectiveHistoryStart
  , ensureCandleMarketIdentity
  , ensureCandleHistorySchema
  , getCandleHistoryIngestionProgress
  , getLatestCandleHistoryIngestionProgress
  , getLatestPublishedCandleHistoryIngestion
  , getLatestCandleHistorySelection
  , initializeCandleHistoryIngestionProgress
  , publishCandleHistoryIngestion
  , recordCandleHistoryIngestionError
  , releaseEpochAtBlock
  , validateCandleHistoryIngestionCompletion
  , validateCandleHistorySelection
  , validateMarketReleaseEpoch
  , validateMarketReleaseEpochSequence
  ) where

import Control.Monad (forM_, unless, when)
import Data.Char (isAsciiLower)
import Data.List (find, nub)
import Data.Text (Text)
import qualified Data.Text as T
import Database.PostgreSQL.Simple
  ( Connection
  , Only (..)
  , Query
  , execute
  , execute_
  , query
  )
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)

-- | The only logical Perps market currently served by the candle API. Keeping
-- this identity independent of any contract release lets price history begin
-- before the current router was deployed.
defaultCandleMarketId :: Text
defaultCandleMarketId = "dxy-perps-v1"

-- | An immutable operator request. The selected timestamp is intentionally not
-- bucket-aligned: every interval begins at its first complete canonical bucket.
data CandleHistorySelection = CandleHistorySelection
  { chsMarketId :: Text
  , chsRevision :: Integer
  , chsRequestedStartTimestamp :: Integer
  , chsRequestedBy :: Text
  , chsRequestReference :: Text
  }
  deriving stock (Eq, Show)

-- | Durable progress for one immutable target revision. The range is
-- half-open: @start <= timestamp < end@. 'chipNextTimestamp' is the beginning
-- of the first endpoint window which has not yet been proved and committed.
-- A completed row therefore has @next == end@ and no outstanding error.
data CandleHistoryIngestionProgress = CandleHistoryIngestionProgress
  { chipMarketId :: Text
  , chipTargetRevision :: Integer
  , chipStartTimestamp :: Integer
  , chipEndTimestampExclusive :: Integer
  , chipNextTimestamp :: Integer
  , chipSampleIntervalSeconds :: Integer
  , chipComplete :: Bool
  , chipLastError :: Maybe Text
  , chipPublishedGeneration :: Maybe Integer
  }
  deriving stock (Eq, Show)

instance FromRow CandleHistoryIngestionProgress where
  fromRow =
    CandleHistoryIngestionProgress
      <$> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field

-- | Immutable contract provenance for one release of a logical market. The
-- release owns blocks from its activation block through the block immediately
-- before the next release activation. The exclusive end is derived, not stored.
data MarketReleaseEpoch = MarketReleaseEpoch
  { mreMarketId :: Text
  , mreRevision :: Integer
  , mreChainId :: Integer
  , mreReleaseRouter :: Text
  , mreCfdEngine :: Text
  , mreMarginClearinghouse :: Text
  , mreDeploymentBlock :: Integer
  , mreDeploymentBlockHash :: Text
  , mreDeploymentTransactionHash :: Text
  , mreActivationBlock :: Integer
  , mreActivationTimestamp :: Integer
  , mreActivationBlockHash :: Text
  , mreApprovalReference :: Text
  , mreIsMarketGenesis :: Bool
  }
  deriving stock (Eq, Show)

ensureCandleHistorySchema :: Connection -> IO ()
ensureCandleHistorySchema conn =
  forM_ candleHistorySchemaStatements $ \statement -> do
    _ <- execute_ conn statement
    pure ()

-- | Create one immutable logical-market binding, or assert that the existing
-- binding exactly matches the caller's expected chain and price series.
ensureCandleMarketIdentity :: Connection -> Text -> Integer -> Text -> IO ()
ensureCandleMarketIdentity conn marketId chainId priceSeriesId = do
  either (fail . T.unpack) pure $
    validateCandleMarketIdentity marketId chainId priceSeriesId
  _ <-
    execute
      conn
      "INSERT INTO perps_candle_markets (market_id, chain_id, price_series_id) \
      \VALUES (?, ?, ?) ON CONFLICT (market_id) DO NOTHING"
      (marketId, chainId, priceSeriesId)
  assertCandleMarketIdentity conn False marketId chainId priceSeriesId

-- | Append an immutable history selection. Replaying the same request
-- reference with the same payload is idempotent and returns the original row;
-- reusing it for a different payload fails closed. The caller owns the
-- transaction boundary so this append can be composed atomically with other
-- administration operations. The market row lock serializes revision
-- allocation when the caller runs this operation in a transaction.
appendCandleHistorySelection
  :: Connection
  -> Text
  -> Integer
  -> Text
  -> Integer
  -> Text
  -> Text
  -> IO (CandleHistorySelection, Bool)
appendCandleHistorySelection
  conn
  marketId
  chainId
  priceSeriesId
  requestedStartTimestamp
  requestedBy
  requestReference = do
    either (fail . T.unpack) pure $
      validateCandleHistorySelection
        CandleHistorySelection
          { chsMarketId = marketId
          , chsRevision = 1
          , chsRequestedStartTimestamp = requestedStartTimestamp
          , chsRequestedBy = requestedBy
          , chsRequestReference = requestReference
          }
    ensureCandleMarketIdentity conn marketId chainId priceSeriesId
    assertCandleMarketIdentity conn True marketId chainId priceSeriesId
    existing <- queryHistorySelectionByReference conn marketId requestReference
    case existing of
      [] -> do
        nextRevisionRows <-
          query
            conn
            "SELECT COALESCE(MAX(revision), 0) + 1 \
            \FROM perps_candle_history_targets WHERE market_id = ?"
            (Only marketId) :: IO [Only Integer]
        nextRevision <- case nextRevisionRows of
          [Only revision] -> pure revision
          _ -> fail "Candle history target revision lookup was not unique"
        let selection =
              CandleHistorySelection
                { chsMarketId = marketId
                , chsRevision = nextRevision
                , chsRequestedStartTimestamp = requestedStartTimestamp
                , chsRequestedBy = requestedBy
                , chsRequestReference = requestReference
                }
        inserted <-
          query
            conn
            "INSERT INTO perps_candle_history_targets (\
            \market_id, revision, requested_start_timestamp, requested_by, request_reference) \
            \VALUES (?, ?, ?, ?, ?) \
            \RETURNING market_id, revision, requested_start_timestamp, requested_by, request_reference"
            ( marketId
            , nextRevision
            , requestedStartTimestamp
            , requestedBy
            , requestReference
            )
            :: IO [(Text, Integer, Integer, Text, Text)]
        case map historySelectionFromTuple inserted of
          [stored] | stored == selection -> pure (stored, True)
          [_] -> fail "Stored candle history target did not match the requested selection"
          _ -> fail "Candle history target insert did not return exactly one row"
      [stored]
        | sameHistorySelectionRequest
            stored
            requestedStartTimestamp
            requestedBy
            requestReference ->
            pure (stored, False)
        | otherwise ->
            fail "Candle history request reference is already bound to a different selection"
      _ -> fail "Candle history request reference lookup was not unique"

-- | Read the latest selection for one exact logical market. A missing target
-- is represented by 'Nothing'; a missing or mismatched market identity is an
-- invariant failure rather than an ambiguous lookup by chain or price series.
getLatestCandleHistorySelection
  :: Connection
  -> Text
  -> Integer
  -> Text
  -> IO (Maybe CandleHistorySelection)
getLatestCandleHistorySelection conn marketId chainId priceSeriesId = do
  either (fail . T.unpack) pure $
    validateCandleMarketIdentity marketId chainId priceSeriesId
  assertCandleMarketIdentity conn False marketId chainId priceSeriesId
  rows <-
    query
      conn
      "SELECT market_id, revision, requested_start_timestamp, requested_by, request_reference \
      \FROM perps_candle_history_targets WHERE market_id = ? \
      \ORDER BY revision DESC LIMIT 1"
      (Only marketId) :: IO [(Text, Integer, Integer, Text, Text)]
  case map historySelectionFromTuple rows of
    [] -> pure Nothing
    [selection] -> pure $ Just selection
    _ -> fail "Latest candle history target lookup was not unique"

-- | Lock the logical market and check that an immutable selection is still
-- the latest revision. Call this inside the same transaction which publishes
-- an ingestion window. The market lock serializes the check with target
-- replacement, so a superseded worker cannot commit source rows after it has
-- observed a newer target.
candleHistorySelectionIsLatest
  :: Connection
  -> Integer
  -> Text
  -> CandleHistorySelection
  -> IO Bool
candleHistorySelectionIsLatest conn chainId priceSeriesId selection = do
  assertCandleMarketIdentity
    conn
    True
    (chsMarketId selection)
    chainId
    priceSeriesId
  latest <-
    getLatestCandleHistorySelection
      conn
      (chsMarketId selection)
      chainId
      priceSeriesId
  pure $ latest == Just selection

-- | Lock the logical market and prove that legacy ingestion is still allowed.
-- Target insertion takes the same row lock, so a legacy fetch which began
-- before selection cannot persist after the selection commits.
candleHistorySelectionIsAbsent
  :: Connection
  -> Text
  -> Integer
  -> Text
  -> IO Bool
candleHistorySelectionIsAbsent conn marketId chainId priceSeriesId = do
  assertCandleMarketIdentity conn True marketId chainId priceSeriesId
  latest <-
    getLatestCandleHistorySelection
      conn
      marketId
      chainId
      priceSeriesId
  pure $ latest == Nothing

-- | Create the frozen half-open ingestion range for a target revision. A
-- retry must present the same aligned start, exclusive end, and sampling
-- interval; otherwise it fails instead of silently changing what "complete"
-- means for an existing revision.
initializeCandleHistoryIngestionProgress
  :: Connection
  -> CandleHistorySelection
  -> Integer
  -> Integer
  -> Integer
  -> IO CandleHistoryIngestionProgress
initializeCandleHistoryIngestionProgress
  conn
  selection
  startTimestamp
  endTimestampExclusive
  sampleIntervalSeconds = do
    validateIngestionRange
      selection
      startTimestamp
      endTimestampExclusive
      sampleIntervalSeconds
    inserted <-
      query
        conn
        "INSERT INTO perps_candle_history_ingestions (\
        \market_id, target_revision, start_timestamp, end_timestamp_exclusive, \
        \next_timestamp, sample_interval_seconds, complete, last_error, published_generation) \
        \VALUES (?, ?, ?, ?, ?, ?, ?, NULL, NULL) \
        \ON CONFLICT (market_id, target_revision) DO NOTHING \
        \RETURNING market_id, target_revision, start_timestamp, end_timestamp_exclusive, \
        \next_timestamp, sample_interval_seconds, complete, last_error, published_generation"
        ( chsMarketId selection
        , chsRevision selection
        , startTimestamp
        , endTimestampExclusive
        , startTimestamp
        , sampleIntervalSeconds
        , startTimestamp == endTimestampExclusive
        )
        :: IO [CandleHistoryIngestionProgress]
    stored <-
      case inserted of
        [progress] -> pure progress
        [] -> do
          existing <-
            getCandleHistoryIngestionProgress
              conn
              (chsMarketId selection)
              (chsRevision selection)
          maybe (fail "Candle history ingestion progress disappeared") pure existing
        _ -> fail "Candle history ingestion progress insert was not unique"
    unless
      ( chipMarketId stored == chsMarketId selection
          && chipTargetRevision stored == chsRevision selection
          && chipStartTimestamp stored == startTimestamp
          && chipEndTimestampExclusive stored == endTimestampExclusive
          && chipSampleIntervalSeconds stored == sampleIntervalSeconds
      ) $
      fail "Candle history ingestion progress conflicts with the frozen target range"
    pure stored

getCandleHistoryIngestionProgress
  :: Connection
  -> Text
  -> Integer
  -> IO (Maybe CandleHistoryIngestionProgress)
getCandleHistoryIngestionProgress conn marketId targetRevision = do
  rows <-
    query
      conn
      "SELECT market_id, target_revision, start_timestamp, end_timestamp_exclusive, \
      \next_timestamp, sample_interval_seconds, complete, last_error, published_generation \
      \FROM perps_candle_history_ingestions \
      \WHERE market_id = ? AND target_revision = ?"
      (marketId, targetRevision)
  case rows of
    [] -> pure Nothing
    [progress] -> pure $ Just progress
    _ -> fail "Candle history ingestion progress lookup was not unique"

-- | Read the latest target and its exact progress in one PostgreSQL statement.
-- The nested optional progress preserves the distinction between no target and
-- a selected target whose worker has not initialized a frozen range yet.
getLatestCandleHistoryIngestionProgress
  :: Connection
  -> Text
  -> Integer
  -> Text
  -> IO (Maybe (CandleHistorySelection, Maybe CandleHistoryIngestionProgress))
getLatestCandleHistoryIngestionProgress conn marketId chainId priceSeriesId = do
  either (fail . T.unpack) pure $
    validateCandleMarketIdentity marketId chainId priceSeriesId
  assertCandleMarketIdentity conn False marketId chainId priceSeriesId
  rows <-
    query
      conn
      "SELECT t.market_id, t.revision, t.requested_start_timestamp, \
      \t.requested_by, t.request_reference, \
      \p.market_id, p.target_revision, p.start_timestamp, p.end_timestamp_exclusive, \
      \p.next_timestamp, p.sample_interval_seconds, p.complete, p.last_error, \
      \p.published_generation \
      \FROM perps_candle_history_targets t \
      \LEFT JOIN perps_candle_history_ingestions p \
      \ON p.market_id = t.market_id AND p.target_revision = t.revision \
      \WHERE t.market_id = ? ORDER BY t.revision DESC LIMIT 1"
      (Only marketId)
      :: IO [LatestIngestionDbRow]
  case rows of
    [] -> pure Nothing
    [row] -> Just <$> latestIngestionFromDbRow row
    _ -> fail "Latest candle history ingestion lookup was not unique"

-- | Read the newest target which has completed the protected publication
-- transaction. This can intentionally lag the latest desired target while an
-- older extension is still ingesting.
getLatestPublishedCandleHistoryIngestion
  :: Connection
  -> Text
  -> Integer
  -> Text
  -> IO (Maybe (CandleHistorySelection, CandleHistoryIngestionProgress))
getLatestPublishedCandleHistoryIngestion conn marketId chainId priceSeriesId = do
  either (fail . T.unpack) pure $
    validateCandleMarketIdentity marketId chainId priceSeriesId
  assertCandleMarketIdentity conn False marketId chainId priceSeriesId
  rows <-
    query
      conn
      "SELECT t.market_id, t.revision, t.requested_start_timestamp, \
      \t.requested_by, t.request_reference, \
      \p.market_id, p.target_revision, p.start_timestamp, p.end_timestamp_exclusive, \
      \p.next_timestamp, p.sample_interval_seconds, p.complete, p.last_error, \
      \p.published_generation \
      \FROM perps_candle_history_targets t \
      \JOIN perps_candle_history_ingestions p \
      \ON p.market_id = t.market_id AND p.target_revision = t.revision \
      \WHERE t.market_id = ? AND p.published_generation IS NOT NULL \
      \ORDER BY t.revision DESC LIMIT 1"
      (Only marketId)
      :: IO [LatestIngestionDbRow]
  case rows of
    [] -> pure Nothing
    [row] -> do
      (selection, progress) <- latestIngestionFromDbRow row
      case progress of
        Just published -> pure $ Just (selection, published)
        Nothing -> fail "Published candle history target is missing ingestion progress"
    _ -> fail "Published candle history ingestion lookup was not unique"

-- | Atomically append one safely decoded endpoint window and advance its
-- contiguous cursor. The caller owns the transaction containing source writes.
-- Empty endpoint windows are valid and retain a sample count of zero; they
-- still prove that all six component requests were complete for the range.
completeCandleHistoryIngestionWindow
  :: Connection
  -> CandleHistorySelection
  -> Integer
  -> Integer
  -> Integer
  -> IO CandleHistoryIngestionProgress
completeCandleHistoryIngestionWindow
  conn
  selection
  windowStart
  windowEndExclusive
  sampleCount = do
    progress <- lockIngestionProgress conn selection
    let interval = chipSampleIntervalSeconds progress
        maximumSamples = (windowEndExclusive - windowStart) `div` interval
    when (chipComplete progress) $
      fail "Completed candle history ingestion cannot accept another window"
    when (windowStart /= chipNextTimestamp progress) $
      fail "Candle history ingestion window does not begin at the durable cursor"
    when
      ( windowEndExclusive <= windowStart
          || windowEndExclusive > chipEndTimestampExclusive progress
          || windowStart `mod` interval /= 0
          || windowEndExclusive `mod` interval /= 0
      ) $
      fail "Candle history ingestion window is outside the frozen aligned range"
    when (sampleCount < 0 || sampleCount > maximumSamples) $
      fail "Candle history ingestion window sample count is impossible"
    _ <-
      execute
        conn
        "INSERT INTO perps_candle_history_ingestion_windows (\
        \market_id, target_revision, window_start, window_end_exclusive, sample_count) \
        \VALUES (?, ?, ?, ?, ?)"
        ( chsMarketId selection
        , chsRevision selection
        , windowStart
        , windowEndExclusive
        , sampleCount
        )
    updated <-
      query
        conn
        "UPDATE perps_candle_history_ingestions SET next_timestamp = ?, \
        \complete = (? = end_timestamp_exclusive), last_error = NULL, updated_at = NOW() \
        \WHERE market_id = ? AND target_revision = ? AND next_timestamp = ? \
        \RETURNING market_id, target_revision, start_timestamp, end_timestamp_exclusive, \
        \next_timestamp, sample_interval_seconds, complete, last_error, published_generation"
        ( windowEndExclusive
        , windowEndExclusive
        , chsMarketId selection
        , chsRevision selection
        , windowStart
        )
        :: IO [CandleHistoryIngestionProgress]
    case updated of
      [stored] -> pure stored
      _ -> fail "Candle history ingestion cursor did not advance exactly once"

-- | Preserve the first incomplete window as an actionable retry point while
-- recording a bounded diagnostic. A later successful commit clears the error.
recordCandleHistoryIngestionError
  :: Connection
  -> CandleHistorySelection
  -> Integer
  -> Text
  -> IO ()
recordCandleHistoryIngestionError conn selection windowStart errorMessage = do
  let boundedError = T.take 2_000 $ T.strip errorMessage
  when (T.null boundedError) $
    fail "Candle history ingestion error cannot be blank"
  _ <- lockIngestionProgress conn selection
  affected <-
    execute
      conn
      "UPDATE perps_candle_history_ingestions SET last_error = ?, updated_at = NOW() \
      \WHERE market_id = ? AND target_revision = ? AND next_timestamp = ? AND NOT complete"
      (boundedError, chsMarketId selection, chsRevision selection, windowStart)
  unless (affected == 1) $
    fail "Candle history ingestion error did not bind the current durable cursor"

-- | Activate one completely proved target at the price dataset generation
-- allocated by the same publication transaction. A target can be published
-- only once; an exact retry is idempotent, while attempting to rebind it to a
-- different generation fails closed.
publishCandleHistoryIngestion
  :: Connection
  -> CandleHistorySelection
  -> Integer
  -> IO CandleHistoryIngestionProgress
publishCandleHistoryIngestion conn selection generation = do
  when (generation <= 0) $
    fail "Candle history publication generation must be positive"
  progress <- lockIngestionProgress conn selection
  either (fail . T.unpack) pure $
    validateCandleHistoryIngestionCompletion selection progress
  case chipPublishedGeneration progress of
    Just existing
      | existing == generation -> pure progress
      | otherwise ->
          fail "Published candle history ingestion cannot change generation"
    Nothing -> do
      published <-
        query
          conn
          "UPDATE perps_candle_history_ingestions SET published_generation = ?, \
          \updated_at = NOW() WHERE market_id = ? AND target_revision = ? \
          \AND complete AND next_timestamp = end_timestamp_exclusive \
          \AND last_error IS NULL AND published_generation IS NULL \
          \RETURNING market_id, target_revision, start_timestamp, end_timestamp_exclusive, \
          \next_timestamp, sample_interval_seconds, complete, last_error, published_generation"
          (generation, chsMarketId selection, chsRevision selection)
          :: IO [CandleHistoryIngestionProgress]
      case published of
        [stored] -> pure stored
        _ -> fail "Candle history ingestion was not published exactly once"

-- | The exact publication precondition for the canonical minute-grid source.
-- CandleAdmin can use the latest-target query above followed by this pure
-- validator; no wall clock or mutable environment input is involved.
validateCandleHistoryIngestionCompletion
  :: CandleHistorySelection
  -> CandleHistoryIngestionProgress
  -> Either Text ()
validateCandleHistoryIngestionCompletion selection progress = do
  let interval = chipSampleIntervalSeconds progress
  expectedStart <- effectiveHistoryStart interval $ chsRequestedStartTimestamp selection
  unless (chipMarketId progress == chsMarketId selection) $
    Left "Candle history ingestion market does not match the selected target"
  unless (chipTargetRevision progress == chsRevision selection) $
    Left "Candle history ingestion revision is not the latest selected target"
  unless (interval == 60) $
    Left "Candle history ingestion is not on the canonical 60-second grid"
  unless (chipStartTimestamp progress == expectedStart) $
    Left "Candle history ingestion start is not aligned from the selected target"
  unless (chipComplete progress) $
    Left "Candle history ingestion is incomplete"
  unless (chipNextTimestamp progress >= chipEndTimestampExclusive progress) $
    Left "Candle history ingestion cursor has not reached its exclusive end"
  unless (chipLastError progress == Nothing) $
    Left "Candle history ingestion has an outstanding source error"

data LatestIngestionDbRow = LatestIngestionDbRow
  { lidrMarketId :: Text
  , lidrRevision :: Integer
  , lidrRequestedStartTimestamp :: Integer
  , lidrRequestedBy :: Text
  , lidrRequestReference :: Text
  , lidrProgressMarketId :: Maybe Text
  , lidrProgressRevision :: Maybe Integer
  , lidrProgressStartTimestamp :: Maybe Integer
  , lidrProgressEndTimestampExclusive :: Maybe Integer
  , lidrProgressNextTimestamp :: Maybe Integer
  , lidrProgressSampleIntervalSeconds :: Maybe Integer
  , lidrProgressComplete :: Maybe Bool
  , lidrProgressLastError :: Maybe Text
  , lidrProgressPublishedGeneration :: Maybe Integer
  }

instance FromRow LatestIngestionDbRow where
  fromRow =
    LatestIngestionDbRow
      <$> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field

latestIngestionFromDbRow
  :: LatestIngestionDbRow
  -> IO (CandleHistorySelection, Maybe CandleHistoryIngestionProgress)
latestIngestionFromDbRow LatestIngestionDbRow {..} = do
  progress <-
    case
      ( lidrProgressMarketId
      , lidrProgressRevision
      , lidrProgressStartTimestamp
      , lidrProgressEndTimestampExclusive
      , lidrProgressNextTimestamp
      , lidrProgressSampleIntervalSeconds
      , lidrProgressComplete
      ) of
      (Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing) -> pure Nothing
      (Just progressMarketId, Just revision, Just start, Just end, Just next, Just interval, Just complete) ->
        pure $
          Just
            CandleHistoryIngestionProgress
              { chipMarketId = progressMarketId
              , chipTargetRevision = revision
              , chipStartTimestamp = start
              , chipEndTimestampExclusive = end
              , chipNextTimestamp = next
              , chipSampleIntervalSeconds = interval
              , chipComplete = complete
              , chipLastError = lidrProgressLastError
              , chipPublishedGeneration = lidrProgressPublishedGeneration
              }
      _ -> fail "Latest candle history ingestion progress has partial nullable fields"
  pure
    ( CandleHistorySelection
        { chsMarketId = lidrMarketId
        , chsRevision = lidrRevision
        , chsRequestedStartTimestamp = lidrRequestedStartTimestamp
        , chsRequestedBy = lidrRequestedBy
        , chsRequestReference = lidrRequestReference
        }
    , progress
    )

lockIngestionProgress
  :: Connection
  -> CandleHistorySelection
  -> IO CandleHistoryIngestionProgress
lockIngestionProgress conn selection = do
  rows <-
    query
      conn
      "SELECT market_id, target_revision, start_timestamp, end_timestamp_exclusive, \
      \next_timestamp, sample_interval_seconds, complete, last_error, published_generation \
      \FROM perps_candle_history_ingestions \
      \WHERE market_id = ? AND target_revision = ? FOR UPDATE"
      (chsMarketId selection, chsRevision selection)
  case rows of
    [progress] -> pure progress
    [] -> fail "Candle history ingestion progress does not exist"
    _ -> fail "Candle history ingestion progress lock was not unique"

validateIngestionRange
  :: CandleHistorySelection
  -> Integer
  -> Integer
  -> Integer
  -> IO ()
validateIngestionRange selection startTimestamp endTimestampExclusive interval = do
  expectedStart <-
    either (fail . T.unpack) pure $
      effectiveHistoryStart interval $ chsRequestedStartTimestamp selection
  when (startTimestamp /= expectedStart) $
    fail "Candle history ingestion start does not match the aligned target"
  when
    ( endTimestampExclusive < startTimestamp
        || endTimestampExclusive `mod` interval /= 0
    ) $
    fail "Candle history ingestion exclusive end is invalid or unaligned"

validateCandleMarketIdentity :: Text -> Integer -> Text -> Either Text ()
validateCandleMarketIdentity marketId chainId priceSeriesId = do
  validateMarketId marketId
  when (chainId <= 0) $ Left "Candle market chain id must be positive"
  requireNonBlank "Candle market price series id" priceSeriesId

assertCandleMarketIdentity
  :: Connection
  -> Bool
  -> Text
  -> Integer
  -> Text
  -> IO ()
assertCandleMarketIdentity conn lockIdentity marketId chainId priceSeriesId = do
  rows <-
    if lockIdentity
      then
        query
          conn
          "SELECT chain_id, price_series_id FROM perps_candle_markets \
          \WHERE market_id = ? FOR UPDATE"
          (Only marketId)
      else
        query
          conn
          "SELECT chain_id, price_series_id FROM perps_candle_markets \
          \WHERE market_id = ?"
          (Only marketId)
      :: IO [(Integer, Text)]
  case rows of
    [(storedChainId, storedPriceSeriesId)]
      | storedChainId == chainId && storedPriceSeriesId == priceSeriesId -> pure ()
      | otherwise ->
          fail "Candle market identity does not match the expected chain and price series"
    [] -> fail "Candle market identity does not exist"
    _ -> fail "Candle market identity lookup was not unique"

queryHistorySelectionByReference
  :: Connection
  -> Text
  -> Text
  -> IO [CandleHistorySelection]
queryHistorySelectionByReference conn marketId requestReference = do
  rows <-
    query
      conn
      "SELECT market_id, revision, requested_start_timestamp, requested_by, request_reference \
      \FROM perps_candle_history_targets \
      \WHERE market_id = ? AND request_reference = ?"
      (marketId, requestReference) :: IO [(Text, Integer, Integer, Text, Text)]
  pure $ map historySelectionFromTuple rows

historySelectionFromTuple
  :: (Text, Integer, Integer, Text, Text)
  -> CandleHistorySelection
historySelectionFromTuple
  (marketId, revision, requestedStartTimestamp, requestedBy, requestReference) =
    CandleHistorySelection
      { chsMarketId = marketId
      , chsRevision = revision
      , chsRequestedStartTimestamp = requestedStartTimestamp
      , chsRequestedBy = requestedBy
      , chsRequestReference = requestReference
      }

sameHistorySelectionRequest
  :: CandleHistorySelection
  -> Integer
  -> Text
  -> Text
  -> Bool
sameHistorySelectionRequest
  CandleHistorySelection {..}
  requestedStartTimestamp
  requestedBy
  requestReference =
    chsRequestedStartTimestamp == requestedStartTimestamp
      && chsRequestedBy == requestedBy
      && chsRequestReference == requestReference

-- | Align an arbitrary selected second to the first complete bucket which does
-- not begin before it. Source availability is deliberately not inferred here.
effectiveHistoryStart :: Integer -> Integer -> Either Text Integer
effectiveHistoryStart interval requestedStart
  | interval <= 0 = Left "Candle interval must be positive"
  | requestedStart < 0 = Left "Candle history start cannot precede the Unix epoch"
  | otherwise =
      Right $ ((requestedStart + interval - 1) `div` interval) * interval

validateCandleHistorySelection :: CandleHistorySelection -> Either Text ()
validateCandleHistorySelection CandleHistorySelection {..} = do
  validateMarketId chsMarketId
  when (chsRevision <= 0) $
    Left "Candle history request revision must be positive"
  when (chsRequestedStartTimestamp < 0) $
    Left "Candle history start cannot precede the Unix epoch"
  requireNonBlank "Candle history requester" chsRequestedBy
  requireNonBlank "Candle history request reference" chsRequestReference

validateMarketReleaseEpoch :: MarketReleaseEpoch -> Either Text ()
validateMarketReleaseEpoch MarketReleaseEpoch {..} = do
  validateMarketId mreMarketId
  when (mreRevision <= 0) $ Left "Market release revision must be positive"
  when (mreIsMarketGenesis /= (mreRevision == 1)) $
    Left "Only market release revision one may be the market genesis"
  when (mreChainId <= 0) $ Left "Market release chain id must be positive"
  validateCanonicalHex "Market release router" 40 mreReleaseRouter
  validateCanonicalHex "Market release CFD engine" 40 mreCfdEngine
  validateCanonicalHex
    "Market release margin clearinghouse"
    40
    mreMarginClearinghouse
  validateCanonicalHex "Deployment block hash" 64 mreDeploymentBlockHash
  validateCanonicalHex
    "Deployment transaction hash"
    64
    mreDeploymentTransactionHash
  validateCanonicalHex "Activation block hash" 64 mreActivationBlockHash
  requireNonBlank "Market release approval reference" mreApprovalReference
  when (mreDeploymentBlock <= 0) $
    Left "Market release deployment block must be positive"
  when (mreActivationBlock < mreDeploymentBlock) $
    Left "Market release activation cannot precede deployment"
  when (mreActivationTimestamp < 0) $
    Left "Market release activation timestamp cannot be negative"

-- | Validate a complete release registry in activation order. New releases are
-- appended in this order, and block ownership is derived from adjacent rows.
validateMarketReleaseEpochSequence :: [MarketReleaseEpoch] -> Either Text ()
validateMarketReleaseEpochSequence [] =
  Left "Market release sequence must contain a genesis epoch"
validateMarketReleaseEpochSequence epochs@(firstEpoch : laterEpochs) = do
  mapM_ validateMarketReleaseEpoch epochs
  unless (mreIsMarketGenesis firstEpoch) $
    Left "The first market release epoch must be the market genesis"
  when (any mreIsMarketGenesis laterEpochs) $
    Left "Only the first market release epoch may be the market genesis"
  unless (all ((== mreMarketId firstEpoch) . mreMarketId) laterEpochs) $
    Left "Market release epochs must belong to one logical market"
  unless (all ((== mreChainId firstEpoch) . mreChainId) laterEpochs) $
    Left "Market release epochs must belong to one chain"
  unless (map mreRevision epochs == [1 .. fromIntegral (length epochs)]) $
    Left "Market release revisions must be contiguous from one"
  unless (strictlyIncreasing $ map mreActivationBlock epochs) $
    Left "Market release activation blocks must be strictly increasing"
  unless (nondecreasing $ map mreActivationTimestamp epochs) $
    Left "Market release activation timestamps must be nondecreasing"
  unless (allUnique $ map mreReleaseRouter epochs) $
    Left "A release router cannot own more than one epoch in a logical market"

-- | Resolve half-open release ownership. At a transition block the successor
-- owns the block, preventing overlap or double counting.
releaseEpochAtBlock
  :: [MarketReleaseEpoch]
  -> Integer
  -> Either Text (Maybe MarketReleaseEpoch)
releaseEpochAtBlock epochs blockNumber = do
  when (blockNumber < 0) $ Left "Market release lookup block cannot be negative"
  validateMarketReleaseEpochSequence epochs
  pure $
    find
      ((<= blockNumber) . mreActivationBlock)
      (reverse epochs)

validateMarketId :: Text -> Either Text ()
validateMarketId marketId
  | T.null marketId || T.length marketId > 63 =
      Left "Market id must contain from 1 through 63 characters"
  | not (isMarketIdHead $ T.head marketId) =
      Left "Market id must begin with a lowercase letter or digit"
  | not (T.all isMarketIdCharacter marketId) =
      Left "Market id may contain only lowercase letters, digits, and hyphens"
  | otherwise = Right ()
 where
  isMarketIdHead char = isAsciiLower char || isAsciiDigit char
  isMarketIdCharacter char = isMarketIdHead char || char == '-'

validateCanonicalHex :: Text -> Int -> Text -> Either Text ()
validateCanonicalHex label digitCount value
  | T.length value /= digitCount + 2
      || T.take 2 value /= "0x"
      || not (T.all isLowerHexDigit $ T.drop 2 value)
      || T.all (== '0') (T.drop 2 value) =
      Left $ label <> " must be canonical lowercase 0x-prefixed hex"
  | otherwise = Right ()
 where
  isLowerHexDigit char = isAsciiDigit char || char >= 'a' && char <= 'f'

isAsciiDigit :: Char -> Bool
isAsciiDigit char = char >= '0' && char <= '9'

requireNonBlank :: Text -> Text -> Either Text ()
requireNonBlank label value =
  when (T.null $ T.strip value) $ Left $ label <> " cannot be blank"

strictlyIncreasing :: Ord value => [value] -> Bool
strictlyIncreasing values = and $ zipWith (<) values $ drop 1 values

nondecreasing :: Ord value => [value] -> Bool
nondecreasing values = and $ zipWith (<=) values $ drop 1 values

allUnique :: Eq value => [value] -> Bool
allUnique values = length values == length (nub values)

candleHistorySchemaStatements :: [Query]
candleHistorySchemaStatements =
  [ "CREATE TABLE IF NOT EXISTS perps_candle_markets (\
    \market_id TEXT PRIMARY KEY, chain_id BIGINT NOT NULL, \
    \price_series_id TEXT NOT NULL REFERENCES perps_basket_definitions(series_id), \
    \created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(), \
    \UNIQUE (market_id, chain_id), CHECK (chain_id > 0), \
    \CHECK (market_id ~ '^[a-z0-9][a-z0-9-]{0,62}$'))"
  , "CREATE OR REPLACE FUNCTION protect_perps_candle_market_identity() \
    \RETURNS TRIGGER LANGUAGE plpgsql AS $candle_market_identity$ BEGIN \
    \IF TG_OP = 'INSERT' THEN NEW.created_at := NOW(); RETURN NEW; END IF; \
    \RAISE EXCEPTION 'candle market identity is immutable' USING ERRCODE = '55000'; \
    \END $candle_market_identity$"
  , "DO $candle_market_triggers$ BEGIN \
    \IF NOT EXISTS (SELECT 1 FROM pg_trigger \
    \ WHERE tgname = 'perps_candle_market_immutable' \
    \ AND tgrelid = 'perps_candle_markets'::regclass) THEN \
    \ CREATE TRIGGER perps_candle_market_immutable BEFORE INSERT OR UPDATE OR DELETE \
    \ ON perps_candle_markets FOR EACH ROW \
    \ EXECUTE FUNCTION protect_perps_candle_market_identity(); \
    \END IF; END $candle_market_triggers$"
  , "CREATE TABLE IF NOT EXISTS perps_candle_history_targets (\
    \market_id TEXT NOT NULL REFERENCES perps_candle_markets(market_id), \
    \revision BIGINT NOT NULL, requested_start_timestamp BIGINT NOT NULL, \
    \requested_by TEXT NOT NULL, request_reference TEXT NOT NULL, \
    \created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(), \
    \PRIMARY KEY (market_id, revision), UNIQUE (market_id, request_reference), \
    \CHECK (revision > 0), CHECK (requested_start_timestamp >= 0), \
    \CHECK (requested_by ~ '[^[:space:]]'), \
    \CHECK (request_reference ~ '[^[:space:]]'))"
  , "CREATE OR REPLACE FUNCTION protect_perps_candle_history_target() \
    \RETURNS TRIGGER LANGUAGE plpgsql AS $candle_history_target$ \
    \DECLARE current_revision BIGINT; BEGIN \
    \IF TG_OP <> 'INSERT' THEN \
    \ RAISE EXCEPTION 'candle history targets are immutable; append a revision' \
    \ USING ERRCODE = '55000'; \
    \END IF; \
    \PERFORM 1 FROM perps_candle_markets WHERE market_id = NEW.market_id FOR UPDATE; \
    \IF NOT FOUND THEN \
    \ RAISE EXCEPTION 'candle history target market does not exist' USING ERRCODE = '23503'; \
    \END IF; \
    \SELECT COALESCE(MAX(revision), 0) INTO current_revision \
    \FROM perps_candle_history_targets WHERE market_id = NEW.market_id; \
    \IF NEW.revision <> current_revision + 1 THEN \
    \ RAISE EXCEPTION 'candle history target must append the next revision' \
    \ USING ERRCODE = '23514'; \
    \END IF; NEW.created_at := NOW(); RETURN NEW; END $candle_history_target$"
  , "DO $candle_history_target_triggers$ BEGIN \
    \IF NOT EXISTS (SELECT 1 FROM pg_trigger \
    \ WHERE tgname = 'perps_candle_history_target_identity' \
    \ AND tgrelid = 'perps_candle_history_targets'::regclass) THEN \
    \ CREATE TRIGGER perps_candle_history_target_identity BEFORE INSERT OR UPDATE OR DELETE \
    \ ON perps_candle_history_targets FOR EACH ROW \
    \ EXECUTE FUNCTION protect_perps_candle_history_target(); \
    \END IF; END $candle_history_target_triggers$"
  , "CREATE TABLE IF NOT EXISTS perps_candle_history_ingestions (\
    \market_id TEXT NOT NULL, target_revision BIGINT NOT NULL, \
    \start_timestamp BIGINT NOT NULL, end_timestamp_exclusive BIGINT NOT NULL, \
    \next_timestamp BIGINT NOT NULL, sample_interval_seconds BIGINT NOT NULL, \
    \complete BOOLEAN NOT NULL DEFAULT FALSE, last_error TEXT, \
    \published_generation BIGINT, \
    \created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(), \
    \updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(), \
    \PRIMARY KEY (market_id, target_revision), \
    \FOREIGN KEY (market_id, target_revision) \
    \REFERENCES perps_candle_history_targets(market_id, revision), \
    \CHECK (sample_interval_seconds > 0), \
    \CHECK (start_timestamp >= 0 AND end_timestamp_exclusive >= start_timestamp), \
    \CHECK (MOD(start_timestamp, sample_interval_seconds) = 0), \
    \CHECK (MOD(end_timestamp_exclusive, sample_interval_seconds) = 0), \
    \CHECK (next_timestamp >= start_timestamp AND next_timestamp <= end_timestamp_exclusive), \
    \CHECK (MOD(next_timestamp, sample_interval_seconds) = 0), \
    \CHECK (complete = (next_timestamp = end_timestamp_exclusive)), \
    \CHECK (last_error IS NULL OR (NOT complete AND last_error ~ '[^[:space:]]')), \
    \CONSTRAINT perps_candle_history_ingestions_publication_valid CHECK (\
    \ published_generation IS NULL OR \
    \ (published_generation > 0 AND complete AND last_error IS NULL)))"
  , "ALTER TABLE perps_candle_history_ingestions \
    \ADD COLUMN IF NOT EXISTS published_generation BIGINT"
  , "DO $candle_history_publication_constraint$ BEGIN \
    \IF NOT EXISTS (SELECT 1 FROM pg_constraint \
    \ WHERE conname = 'perps_candle_history_ingestions_publication_valid' \
    \ AND conrelid = 'perps_candle_history_ingestions'::regclass) THEN \
    \ ALTER TABLE perps_candle_history_ingestions ADD CONSTRAINT \
    \ perps_candle_history_ingestions_publication_valid CHECK (\
    \  published_generation IS NULL OR \
    \  (published_generation > 0 AND complete AND last_error IS NULL)); \
    \END IF; END $candle_history_publication_constraint$"
  , "CREATE OR REPLACE FUNCTION protect_perps_candle_history_publication() \
    \RETURNS TRIGGER LANGUAGE plpgsql AS $candle_history_publication$ BEGIN \
    \IF OLD.published_generation IS NOT NULL \
    \ AND NEW.published_generation IS DISTINCT FROM OLD.published_generation THEN \
    \ RAISE EXCEPTION 'candle history publication is immutable' \
    \ USING ERRCODE = '55000'; \
    \END IF; RETURN NEW; END $candle_history_publication$"
  , "DO $candle_history_publication_trigger$ BEGIN \
    \IF NOT EXISTS (SELECT 1 FROM pg_trigger \
    \ WHERE tgname = 'perps_candle_history_publication_immutable' \
    \ AND tgrelid = 'perps_candle_history_ingestions'::regclass) THEN \
    \ CREATE TRIGGER perps_candle_history_publication_immutable BEFORE UPDATE \
    \ ON perps_candle_history_ingestions FOR EACH ROW \
    \ EXECUTE FUNCTION protect_perps_candle_history_publication(); \
    \END IF; END $candle_history_publication_trigger$"
  , "CREATE TABLE IF NOT EXISTS perps_candle_history_ingestion_windows (\
    \market_id TEXT NOT NULL, target_revision BIGINT NOT NULL, \
    \window_start BIGINT NOT NULL, window_end_exclusive BIGINT NOT NULL, \
    \sample_count BIGINT NOT NULL, completed_at TIMESTAMPTZ NOT NULL DEFAULT NOW(), \
    \PRIMARY KEY (market_id, target_revision, window_start), \
    \FOREIGN KEY (market_id, target_revision) \
    \REFERENCES perps_candle_history_ingestions(market_id, target_revision), \
    \CHECK (window_start >= 0 AND window_end_exclusive > window_start), \
    \CHECK (sample_count >= 0))"
  , "CREATE TABLE IF NOT EXISTS perps_market_release_epochs (\
    \market_id TEXT NOT NULL, release_revision BIGINT NOT NULL, \
    \chain_id BIGINT NOT NULL, release_router TEXT NOT NULL, \
    \cfd_engine TEXT NOT NULL, margin_clearinghouse TEXT NOT NULL, \
    \deployment_block BIGINT NOT NULL, deployment_block_hash TEXT NOT NULL, \
    \deployment_tx_hash TEXT NOT NULL, activation_block BIGINT NOT NULL, \
    \activation_timestamp BIGINT NOT NULL, activation_block_hash TEXT NOT NULL, \
    \approval_reference TEXT NOT NULL, \
    \is_market_genesis BOOLEAN NOT NULL DEFAULT FALSE, \
    \created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(), \
    \PRIMARY KEY (market_id, release_revision), \
    \UNIQUE (market_id, activation_block), UNIQUE (chain_id, release_router), \
    \FOREIGN KEY (market_id, chain_id) REFERENCES perps_candle_markets(market_id, chain_id), \
    \CHECK (release_router ~ '^0x[0-9a-f]{40}$'), \
    \CHECK (cfd_engine ~ '^0x[0-9a-f]{40}$'), \
    \CHECK (margin_clearinghouse ~ '^0x[0-9a-f]{40}$'), \
    \CHECK (release_router <> '0x0000000000000000000000000000000000000000'), \
    \CHECK (cfd_engine <> '0x0000000000000000000000000000000000000000'), \
    \CHECK (margin_clearinghouse <> '0x0000000000000000000000000000000000000000'), \
    \CHECK (release_revision > 0), \
    \CHECK (is_market_genesis = (release_revision = 1)), \
    \CHECK (deployment_block > 0), CHECK (activation_block >= deployment_block), \
    \CHECK (activation_timestamp >= 0), \
    \CHECK (deployment_block_hash ~ '^0x[0-9a-f]{64}$'), \
    \CHECK (deployment_tx_hash ~ '^0x[0-9a-f]{64}$'), \
    \CHECK (activation_block_hash ~ '^0x[0-9a-f]{64}$'), \
    \CHECK (deployment_block_hash <> '0x0000000000000000000000000000000000000000000000000000000000000000'), \
    \CHECK (deployment_tx_hash <> '0x0000000000000000000000000000000000000000000000000000000000000000'), \
    \CHECK (activation_block_hash <> '0x0000000000000000000000000000000000000000000000000000000000000000'), \
    \CHECK (approval_reference ~ '[^[:space:]]'))"
  , "CREATE OR REPLACE FUNCTION protect_perps_market_release_identity() \
    \RETURNS TRIGGER LANGUAGE plpgsql AS $market_release_identity$ \
    \DECLARE market_chain_id BIGINT; current_revision BIGINT; \
    \latest_block BIGINT; latest_timestamp BIGINT; BEGIN \
    \IF TG_OP <> 'INSERT' THEN \
    \ RAISE EXCEPTION 'market release epochs are immutable; append a successor epoch' \
    \ USING ERRCODE = '55000'; \
    \END IF; \
    \SELECT chain_id INTO market_chain_id FROM perps_candle_markets \
    \WHERE market_id = NEW.market_id FOR UPDATE; \
    \IF NOT FOUND OR market_chain_id <> NEW.chain_id THEN \
    \ RAISE EXCEPTION 'market release does not match the logical market and chain' \
    \ USING ERRCODE = '23503'; \
    \END IF; \
    \SELECT release_revision, activation_block, activation_timestamp \
    \INTO current_revision, latest_block, latest_timestamp \
    \FROM perps_market_release_epochs WHERE market_id = NEW.market_id \
    \ORDER BY release_revision DESC LIMIT 1; \
    \IF NOT FOUND THEN \
    \ current_revision := 0; \
    \ELSE \
    \ IF NEW.activation_block <= latest_block \
    \  OR NEW.activation_timestamp < latest_timestamp THEN \
    \  RAISE EXCEPTION 'market release epochs must append in activation order' \
    \  USING ERRCODE = '23514'; \
    \ END IF; \
    \END IF; \
    \IF NEW.release_revision <> current_revision + 1 THEN \
    \ RAISE EXCEPTION 'market release must append the next revision' \
    \ USING ERRCODE = '23514'; \
    \END IF; NEW.created_at := NOW(); RETURN NEW; END $market_release_identity$"
  , "DO $market_release_triggers$ BEGIN \
    \IF NOT EXISTS (SELECT 1 FROM pg_trigger \
    \ WHERE tgname = 'perps_market_release_immutable' \
    \ AND tgrelid = 'perps_market_release_epochs'::regclass) THEN \
    \ CREATE TRIGGER perps_market_release_immutable BEFORE INSERT OR UPDATE OR DELETE \
    \ ON perps_market_release_epochs FOR EACH ROW \
    \ EXECUTE FUNCTION protect_perps_market_release_identity(); \
    \END IF; END $market_release_triggers$"
  ]
