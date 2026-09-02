module Main (main) where

import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, bracket_, displayException, try)
import Control.Monad (forM, forM_, join, unless, when)
import Data.Char (isDigit)
import Data.Maybe (fromMaybe, isJust, isNothing)
import qualified Data.Text as T
import Database.PostgreSQL.Simple
  ( Connection
  , Only (..)
  , execute
  , query
  , withTransaction
  )
import Database.PostgreSQL.Simple.Transaction
  ( IsolationLevel (ReadCommitted, RepeatableRead)
  , ReadWriteMode (ReadOnly, ReadWrite)
  , TransactionMode (..)
  , withTransactionMode
  )
import Plether.Database (newDbPool, withDb)
import Plether.Database.CandleHistory
  ( CandleHistoryIngestionProgress (..)
  , CandleHistorySelection (..)
  , appendCandleHistorySelection
  , candleHistorySelectionIsLatest
  , defaultCandleMarketId
  , getLatestCandleHistoryIngestionProgress
  , getLatestPublishedCandleHistoryIngestion
  , publishCandleHistoryIngestion
  , validateCandleHistoryIngestionCompletion
  )
import Plether.Database.Candles
  ( RollupCoverage (..)
  , RollupKind (..)
  , backfillLegacyBasketSnapshots
  , backfillMarketVolume
  , beginRollupMaintenance
  , bumpRollupDatasetGeneration
  , canonicalCandleIntervals
  , countBasketCandles
  , countMarketVolumeRollups
  , defaultBasketSeriesId
  , ensureCandleSchema
  , ensureCurrentBasketDefinition
  , getRollupCoverage
  , getActiveBasketSeriesId
  , lockBasketPriceDataset
  , markRollupCoverageIncomplete
  , upsertRollupCoverage
  , vacuumCandlePageTables
  )
import Plether.Logging
  ( field
  , logError
  , logInfo
  )
import Plether.Perps.CandleFinalizerProbe
  ( FinalizerProbeEnvironment (..)
  , FinalizerProbePlan (..)
  , finalizerProbeRecovered
  , planFinalizerProbe
  , validateFinalizerDatabaseUrl
  , validateFinalizerLibpqEnvironment
  , validateFinalizerProbeEnvironment
  , validateFinalizerProbePrestate
  )
import Plether.Perps.Release
  ( perpsV2DeploymentBlock
  , perpsV2OrderRouter
  , perpsV2VolumeHistoryStartTimestamp
  )
import System.Environment (getArgs, getEnvironment, lookupEnv)
import System.Exit (exitFailure)
import System.Timeout (timeout)
import Text.Read (readMaybe)

data RollupScope
  = PriceRollups
  | VolumeRollups
  | AllRollups
  deriving (Eq, Show)

data AdminAction
  = Estimate
  | Migrate
  | SetHistoryTarget
  | Backfill RollupScope
  | Status
  | Verify RollupScope
  | Repair RollupScope
  | FinalizerProbe
  deriving (Eq, Show)

data AdminOptions = AdminOptions
  { aoAction :: AdminAction
  , aoFrom :: Maybe Integer
  , aoTo :: Maybe Integer
  , aoBoundary :: Maybe Integer
  , aoHistoryStartTimestamp :: Maybe Integer
  , aoRequestedBy :: Maybe T.Text
  , aoRequestReference :: Maybe T.Text
  , aoChunkSeconds :: Integer
  , aoStatementTimeoutMs :: Int
  , aoLockTimeoutMs :: Int
  , aoThrottleMs :: Int
  , aoMaxRuntimeSeconds :: Int
  }
  deriving (Eq, Show)

data AdminRuntime = AdminRuntime
  { arChainId :: Integer
  , arReleaseRouter :: T.Text
  , arLatenessSeconds :: Integer
  }
  deriving (Eq, Show)

data SourceBounds = SourceBounds
  { sbFrom :: Integer
  , sbTo :: Integer
  , sbRows :: Integer
  }
  deriving (Eq, Show)

data CompletedHistoryTarget = CompletedHistoryTarget
  { chtSelection :: CandleHistorySelection
  , chtProgress :: CandleHistoryIngestionProgress
  }
  deriving (Eq, Show)

data BackfillOrder
  = OldestFirst
  | NewestFirst
  deriving (Eq, Show)

data BackfillRange = BackfillRange
  { brBounds :: SourceBounds
  , brOrder :: BackfillOrder
  }
  deriving (Eq, Show)

data RepairMaintenance = RepairMaintenance
  { rmKind :: RollupKind
  , rmPublicationCoverage :: [RollupCoverage]
  , rmGeneration :: Integer
  , rmRepairFrom :: Integer
  , rmRepairTo :: Integer
  }
  deriving (Eq, Show)

data MergedCoverageRange = MergedCoverageRange
  { mcrCoverageStart :: Integer
  , mcrPublishedEnd :: Integer
  , mcrFallbackCoverageEnd :: Integer
  , mcrFallbackFinalizedThrough :: Integer
  , mcrMergeTrustedEnvelope :: Bool
  }
  deriving (Eq, Show)

defaultOptions :: AdminAction -> AdminOptions
defaultOptions action =
  AdminOptions
    { aoAction = action
    , aoFrom = Nothing
    , aoTo = Nothing
    , aoBoundary = Nothing
    , aoHistoryStartTimestamp = Nothing
    , aoRequestedBy = Nothing
    , aoRequestReference = Nothing
    , aoChunkSeconds = 86_400
    , aoStatementTimeoutMs = 1_800_000
    , aoLockTimeoutMs =
        case action of
          Migrate -> 60_000
          _ -> 5_000
    , aoThrottleMs = 250
    , aoMaxRuntimeSeconds =
        case action of
          FinalizerProbe -> 2_100
          _ -> 21_600
    }

main :: IO ()
main = do
  args <- getArgs
  case parseAdminOptions args of
    Left err -> do
      unless (null err) $ putStrLn err
      putStrLn usage
      unless (null err) exitFailure
    Right options -> do
      writeMode <-
        if requiresDualWriteMode $ aoAction options
          then T.toLower . T.strip . T.pack <$> requireEnv "PERPS_CANDLE_WRITE_MODE"
          else pure ""
      when (requiresDualWriteMode $ aoAction options) $
        unless (writeMode == "dual") $
          failWith "This candle administration action requires PERPS_CANDLE_WRITE_MODE=dual"
      chainId <- requireIntegerEnv "PERPS_CHAIN_ID"
      latenessSeconds <- optionalIntegerEnv "PERPS_CANDLE_LATENESS_SECONDS" 120 0 86_400
      databaseUrl <- requireEnv "DATABASE_URL"
      when (aoAction options == FinalizerProbe) $ do
        deploymentEnvironment <- T.pack <$> requireEnv "DEPLOYMENT_ENVIRONMENT"
        expectedDatabaseHost <- T.pack <$> requireEnv "EXPECTED_DATABASE_HOST"
        processEnvironment <- map (T.pack . fst) <$> getEnvironment
        readMode <- T.pack <$> requireEnv "PERPS_CANDLE_READ_MODE"
        readIntervals <- T.pack <$> requireEnv "PERPS_CANDLE_READ_INTERVALS"
        strictCoverage <- T.pack <$> requireEnv "PERPS_CANDLE_STRICT_COVERAGE"
        finalizationGraceSeconds <- requireIntegerEnv "PERPS_CANDLE_FINALIZATION_GRACE_SECONDS"
        either (failWith . T.unpack) pure $
          validateFinalizerProbeEnvironment
            FinalizerProbeEnvironment
              { fpeDeploymentEnvironment = deploymentEnvironment
              , fpeChainId = chainId
              , fpeWriteMode = writeMode
              , fpeReadMode = readMode
              , fpeReadIntervals = readIntervals
              , fpeStrictCoverage = strictCoverage
              , fpeLatenessSeconds = latenessSeconds
              , fpeFinalizationGraceSeconds = finalizationGraceSeconds
              }
        either (failWith . T.unpack) pure $
          validateFinalizerDatabaseUrl expectedDatabaseHost $ T.pack databaseUrl
        either (failWith . T.unpack) pure $
          validateFinalizerLibpqEnvironment processEnvironment
      releaseRouter <-
        if aoAction options == SetHistoryTarget
          then pure ""
          else T.toLower . T.strip . T.pack <$> requireEnv "PERPS_ORDER_ROUTER"
      let runtime = AdminRuntime chainId releaseRouter latenessSeconds
      pool <- newDbPool $ T.pack databaseUrl
      result <- try @SomeException $ do
        completed <- timeout (aoMaxRuntimeSeconds options * 1_000_000) $
          withDb pool $ \conn -> do
            configureSession conn options
            if requiresAdvisoryLock $ aoAction options
              then withAdminLock conn (aoLockTimeoutMs options) $ runAdmin conn runtime options
              else runAdmin conn runtime options
        when (isNothing completed) $
          failWith "Candle administration exceeded its absolute maximum runtime"
      case result of
        Right () -> pure ()
        Left err -> do
          logError
            "perps_candle_admin_failed"
            "Perps candle administration failed"
            [ field "action" $ actionName $ aoAction options
            , field "scope" $ scopeNameForAction $ aoAction options
            , field "failure" ("operation_exception" :: T.Text)
            , field "error" $ sanitizeException databaseUrl err
            ]
          when (reportsBackfillFailure $ aoAction options) $
            logError
              "perps_candle_backfill_failed"
              "Perps candle administration failed"
              [ field "action" $ actionName $ aoAction options
              , field "scope" $ scopeNameForAction $ aoAction options
              , field "failure" ("operation_exception" :: T.Text)
              , field "error" $ sanitizeException databaseUrl err
              ]
          when (aoAction options == FinalizerProbe) $
            logError
              "perps_candle_finalizer_probe_failed"
              "Sepolia candle finalizer probe failed"
              [ field "action" $ actionName $ aoAction options
              , field "scope" $ scopeNameForAction $ aoAction options
              , field "failure" ("operation_exception" :: T.Text)
              , field "error" $ sanitizeException databaseUrl err
              ]
          exitFailure

runAdmin :: Connection -> AdminRuntime -> AdminOptions -> IO ()
runAdmin conn runtime options@AdminOptions {aoAction} =
  case aoAction of
    Estimate -> runEstimate conn runtime options
    Migrate -> do
      ensureCandleSchema conn
      vacuumCandlePageTables conn
      logInfo
        "perps_candle_migration_complete"
        "Perps candle schema migration completed"
        [field "derivation_version" candleDerivationVersion]
    SetHistoryTarget -> runSetHistoryTarget conn runtime options
    Backfill scope ->
      forM_ (rollupKinds scope) $ \kind ->
        runBackfill conn runtime options kind False
    Status -> withVerificationSnapshot conn $ runStatus conn runtime
    Verify scope -> withVerificationSnapshot conn $ do
      verified <- and <$> mapM (verifyKind conn runtime options) (rollupKinds scope)
      unless verified $ failWith "Candle rollup verification found mismatches"
    Repair scope -> do
      let selectedKinds = rollupKinds scope
      maintenance <- beginRepairMaintenance conn runtime options selectedKinds
      -- Every selected dataset remains incomplete while newest-first chunks
      -- commit independently. A failure leaves the preserved maintenance
      -- metadata in place so the same bounded repair can be retried safely.
      forM_ selectedKinds $ \kind ->
        runBackfill conn runtime options kind True
      withRepairDatasetLocks conn runtime selectedKinds $
        withRepairPublicationTransaction conn $ do
          -- The final repeatable-read transaction acquires each writer dataset
          -- lock in canonical kind order, then proves no worker/reorg changed
          -- the maintenance identity before exposing any selected kind.
          generations <-
            forM maintenance $ \state -> do
              assertRepairMaintenance conn runtime state
              generation <- bumpDatasetGeneration conn runtime $ rmKind state
              unless (generation == rmGeneration state + 1) $
                failWith "Candle repair generation changed while publication was locked"
              pure (rmKind state, generation)
          reconciled <- and <$> mapM (verifyKindValues conn runtime options) selectedKinds
          unless reconciled $ failWith "Repaired candle rollups did not reconcile with canonical sources"
          forM_ maintenance $ \state ->
            case lookup (rmKind state) generations of
              Just generation ->
                publishPreservedCoverage
                  conn
                  runtime
                  (rmKind state)
                  (rmPublicationCoverage state)
                  generation
              Nothing -> failWith "Repaired candle generation was not allocated"
          verified <- and <$> mapM (verifyKind conn runtime options) selectedKinds
          unless verified $ failWith "Repaired candle rollups did not pass verification"
      forM_ selectedKinds $ \kind ->
        logInfo
          "perps_candle_repair_complete"
          "Perps candle repair completed and passed verification"
          [ field "kind" $ rollupKindName kind
          , field "from_timestamp" $ aoFrom options
          , field "to_timestamp" $ aoTo options
          ]
    FinalizerProbe -> runFinalizerProbe conn runtime options

runSetHistoryTarget :: Connection -> AdminRuntime -> AdminOptions -> IO ()
runSetHistoryTarget
  conn
  AdminRuntime {arChainId}
  AdminOptions
    { aoHistoryStartTimestamp = Just requestedStartTimestamp
    , aoRequestedBy = Just requestedBy
    , aoRequestReference = Just requestReference
    } = do
    (selection, inserted) <-
      withTransaction conn $ do
        -- The logical-market row references the immutable price definition;
        -- seed/assert it here so this command also works on a freshly migrated
        -- database before any writer has started.
        ensureCurrentBasketDefinition conn defaultBasketSeriesId
        result <-
          appendCandleHistorySelection
            conn
            defaultCandleMarketId
            arChainId
            defaultBasketSeriesId
            requestedStartTimestamp
            requestedBy
            requestReference
        pure result
    logInfo
      "perps_candle_history_target_selected"
      "Perps candle history target selected"
      [ field "market_id" $ chsMarketId selection
      , field "revision" $ chsRevision selection
      , field "requested_start_timestamp" $ chsRequestedStartTimestamp selection
      , field "requested_by" $ chsRequestedBy selection
      , field "request_reference" $ chsRequestReference selection
      , field "inserted" inserted
      ]
runSetHistoryTarget _ _ _ =
  failWith "set-history-target requires --start-timestamp, --requested-by, and --request-reference"

-- A selected target is desired state only. Until its exact ingestion proof is
-- complete and CandleAdmin publishes it, native readers continue serving the
-- previously published target (or their existing physical coverage).
loadLatestHistoryTarget
  :: Connection
  -> AdminRuntime
  -> IO (Maybe (CandleHistorySelection, Maybe CandleHistoryIngestionProgress))
loadLatestHistoryTarget conn AdminRuntime {arChainId} = do
  marketRows <-
    query
      conn
      "SELECT chain_id, price_series_id FROM perps_candle_markets WHERE market_id = ?"
      (Only defaultCandleMarketId) :: IO [(Integer, T.Text)]
  case marketRows of
    [] -> pure Nothing
    [(storedChainId, storedSeriesId)]
      | storedChainId == arChainId && storedSeriesId == defaultBasketSeriesId ->
          getLatestCandleHistoryIngestionProgress
            conn
            defaultCandleMarketId
            arChainId
            defaultBasketSeriesId
      | otherwise ->
          failWith "Configured candle logical-market identity does not match this environment"
    _ -> failWith "Configured candle logical-market identity is not unique"

requireCompletedHistoryTarget
  :: Connection
  -> AdminRuntime
  -> IO (Maybe CompletedHistoryTarget)
requireCompletedHistoryTarget conn runtime = do
  latest <- loadLatestHistoryTarget conn runtime
  case latest of
    Nothing -> pure Nothing
    Just (_, Nothing) ->
      failWith "The latest candle history target has not initialized source ingestion"
    Just (selection, Just progress) -> do
      either (failWith . T.unpack) pure $
        validateCandleHistoryIngestionCompletion selection progress
      pure $ Just $ CompletedHistoryTarget selection progress

emitHistoryTargetStatus
  :: Maybe (CandleHistorySelection, Maybe CandleHistoryIngestionProgress)
  -> Maybe (CandleHistorySelection, CandleHistoryIngestionProgress)
  -> Bool
  -> IO ()
emitHistoryTargetStatus latest active publicationReady =
  case latest of
    Nothing ->
      logInfo
        "perps_candle_history_target_status"
        "No Perps candle history target is selected"
        [ field "market_id" defaultCandleMarketId
        , field "selected" False
        , field "active_published_target" False
        , field "publication_ready" False
        ]
    Just (selection, progress) -> do
      let publishedGeneration = progress >>= chipPublishedGeneration
          activeSelection = fst <$> active
          activeProgress = snd <$> active
      logInfo
        "perps_candle_history_target_status"
        "Reported desired Perps candle history target and ingestion proof"
        [ field "market_id" $ chsMarketId selection
        , field "selected" True
        , field "target_revision" $ chsRevision selection
        , field "requested_start_timestamp" $ chsRequestedStartTimestamp selection
        , field "requested_by" $ chsRequestedBy selection
        , field "request_reference" $ chsRequestReference selection
        , field "ingestion_start_timestamp" $ chipStartTimestamp <$> progress
        , field "ingestion_end_timestamp_exclusive" $ chipEndTimestampExclusive <$> progress
        , field "ingestion_next_timestamp" $ chipNextTimestamp <$> progress
        , field "sample_interval_seconds" $ chipSampleIntervalSeconds <$> progress
        , field "ingestion_complete" $ maybe False chipComplete progress
        , field "ingestion_last_error" $ progress >>= chipLastError
        , field "published" $ isJust publishedGeneration
        , field "published_generation" publishedGeneration
        , field "active_published_target" $ isJust active
        , field "active_target_revision" $ chsRevision <$> activeSelection
        , field "active_requested_start_timestamp" $
            chsRequestedStartTimestamp <$> activeSelection
        , field "active_published_generation" $
            activeProgress >>= chipPublishedGeneration
        , field "publication_ready" publicationReady
        ]

resolveCompletedHistoryTargetBounds
  :: Connection
  -> AdminOptions
  -> CompletedHistoryTarget
  -> IO SourceBounds
resolveCompletedHistoryTargetBounds
  conn
  AdminOptions {aoFrom, aoTo}
  CompletedHistoryTarget {chtProgress} = do
    let targetFrom = chipStartTimestamp chtProgress
        targetTo = chipEndTimestampExclusive chtProgress
    unless (maybe True (== targetFrom) aoFrom && maybe True (== targetTo) aoTo) $
      failWith
        "Target price backfill must use its complete frozen ingestion range; omit --from/--to or pass the exact bounds"
    unless (hasFullCanonicalRange targetFrom targetTo) $
      failWith
        "The completed history target does not contain a full aligned bucket for every canonical interval"
    sourceRows <- countPriceSourceRowsWithin conn targetFrom targetTo
    when (sourceRows <= 0) $
      failWith "The completed history target contains no canonical price observations"
    pure $ SourceBounds targetFrom targetTo sourceRows

countPriceSourceRowsWithin :: Connection -> Integer -> Integer -> IO Integer
countPriceSourceRowsWithin conn fromTimestamp toTimestamp = do
  observationTableRows <-
    query
      conn
      "SELECT to_regclass('perps_basket_observations') IS NOT NULL"
      () :: IO [Only Bool]
  rows <- case observationTableRows of
    [Only True] ->
      query
        conn
        "WITH observed_prioritized AS ( \
        \ SELECT publish_time, source_priority, \
        \   MAX(source_priority) OVER (PARTITION BY publish_time) AS max_source_priority \
        \ FROM perps_basket_observations WHERE series_id = ? \
        \   AND publish_time >= ? AND publish_time < ?), \
        \source_times AS ( \
        \ SELECT publish_time AS timestamp FROM observed_prioritized \
        \ WHERE source_priority = max_source_priority \
        \ UNION \
        \ SELECT timestamp FROM perps_basket_snapshots \
        \ WHERE timestamp >= ? AND timestamp < ?) \
        \SELECT COUNT(*)::BIGINT FROM source_times"
        ( defaultBasketSeriesId
        , fromTimestamp
        , toTimestamp
        , fromTimestamp
        , toTimestamp
        )
    [Only False] ->
      query
        conn
        "SELECT COUNT(DISTINCT timestamp)::BIGINT FROM perps_basket_snapshots \
        \WHERE timestamp >= ? AND timestamp < ?"
        (fromTimestamp, toTimestamp)
    _ -> failWith "Could not determine whether the basket observation ledger exists"
  case rows of
    [Only count] -> pure count
    _ -> failWith "Could not count canonical price observations in the history target"

priceSourceRowsExistWithin :: Connection -> Integer -> Integer -> IO Bool
priceSourceRowsExistWithin conn fromTimestamp toTimestamp = do
  observationTableRows <-
    query
      conn
      "SELECT to_regclass('perps_basket_observations') IS NOT NULL"
      () :: IO [Only Bool]
  rows <- case observationTableRows of
    [Only True] ->
      query
        conn
        "SELECT EXISTS (SELECT 1 FROM perps_basket_observations \
        \ WHERE series_id = ? AND publish_time >= ? AND publish_time < ?) \
        \OR EXISTS (SELECT 1 FROM perps_basket_snapshots \
        \ WHERE timestamp >= ? AND timestamp < ?)"
        ( defaultBasketSeriesId
        , fromTimestamp
        , toTimestamp
        , fromTimestamp
        , toTimestamp
        )
    [Only False] ->
      query
        conn
        "SELECT EXISTS (SELECT 1 FROM perps_basket_snapshots \
        \ WHERE timestamp >= ? AND timestamp < ?)"
        (fromTimestamp, toTimestamp)
    _ -> failWith "Could not determine whether the basket observation ledger exists"
  case rows of
    [Only available] -> pure available
    _ -> failWith "Could not determine canonical price source availability"

historyTargetPublicationReady
  :: Connection
  -> Maybe (CandleHistorySelection, Maybe CandleHistoryIngestionProgress)
  -> IO Bool
historyTargetPublicationReady conn = \case
  Just (selection, Just progress)
    | isNothing (chipPublishedGeneration progress)
    , Right () <- validateCandleHistoryIngestionCompletion selection progress
    , let fromTimestamp = chipStartTimestamp progress
    , let toTimestamp = chipEndTimestampExclusive progress
    , hasFullCanonicalRange fromTimestamp toTimestamp ->
        priceSourceRowsExistWithin conn fromTimestamp toTimestamp
  _ -> pure False

publishCompletedHistoryTarget
  :: Connection
  -> AdminRuntime
  -> CompletedHistoryTarget
  -> SourceBounds
  -> IO ()
publishCompletedHistoryTarget
  conn
  runtime
  CompletedHistoryTarget {chtSelection, chtProgress}
  SourceBounds {sbFrom, sbTo} =
    case chipPublishedGeneration chtProgress of
      Just generation ->
        logInfo
          "perps_candle_history_target_already_published"
          "The selected Perps candle history target is already published"
          [ field "market_id" $ chsMarketId chtSelection
          , field "target_revision" $ chsRevision chtSelection
          , field "price_generation" generation
          ]
      Nothing ->
        do
          generation <- withTransaction conn $ do
            isLatest <-
              candleHistorySelectionIsLatest
                conn
                (arChainId runtime)
                defaultBasketSeriesId
                chtSelection
            unless isLatest $
              failWith "Candle history target was replaced before publication"
            refreshed <-
              getLatestCandleHistoryIngestionProgress
                conn
                defaultCandleMarketId
                (arChainId runtime)
                defaultBasketSeriesId
            storedProgress <- case refreshed of
              Just (storedSelection, Just progress)
                | storedSelection == chtSelection && progress == chtProgress -> pure progress
              _ -> failWith "Candle history ingestion proof changed before publication"
            either (failWith . T.unpack) pure $
              validateCandleHistoryIngestionCompletion chtSelection storedProgress
            unless
              ( sbFrom == chipStartTimestamp storedProgress
                  && sbTo == chipEndTimestampExclusive storedProgress
              ) $
              failWith "Candle history publication bounds do not match its frozen ingestion proof"
            nextGeneration <- bumpDatasetGeneration conn runtime PriceRollup
            publishCoverage conn runtime PriceRollup sbFrom sbTo $ Just nextGeneration
            _ <- publishCandleHistoryIngestion conn chtSelection nextGeneration
            pure nextGeneration
          logInfo
            "perps_candle_history_target_published"
            "Published a completely ingested Perps candle history target"
            [ field "market_id" $ chsMarketId chtSelection
            , field "target_revision" $ chsRevision chtSelection
            , field "from_timestamp" sbFrom
            , field "to_timestamp" sbTo
            , field "price_generation" generation
            ]

candleDerivationVersion :: T.Text
candleDerivationVersion = "v1"

boundedRepairReason :: T.Text
boundedRepairReason = "bounded_admin_repair"

rollupKinds :: RollupScope -> [RollupKind]
rollupKinds = \case
  PriceRollups -> [PriceRollup]
  VolumeRollups -> [VolumeRollup]
  AllRollups -> [PriceRollup, VolumeRollup]

runEstimate :: Connection -> AdminRuntime -> AdminOptions -> IO ()
runEstimate conn runtime options =
  forM_ [PriceRollup, VolumeRollup] $ \kind -> do
    bounds <- resolveBounds conn runtime options kind
    case bounds of
      Nothing ->
        logInfo
          "perps_candle_backfill_estimate"
          "No source rows are available for candle backfill"
          [ field "kind" $ rollupKindName kind
          , field "source_rows" (0 :: Integer)
          , field "estimated_chunks" (0 :: Integer)
          , field "estimated_rollup_rows_upper_bound" (0 :: Integer)
          ]
      Just SourceBounds {sbFrom, sbTo, sbRows} -> do
        let spanSeconds = max 0 $ sbTo - sbFrom
            chunkCount = ceilingDiv spanSeconds $ aoChunkSeconds options
            rollupUpperBound = sum $ map (ceilingDiv spanSeconds) canonicalCandleIntervals
        logInfo
          "perps_candle_backfill_estimate"
          "Estimated Perps candle backfill work"
          [ field "kind" $ rollupKindName kind
          , field "from_timestamp" sbFrom
          , field "to_timestamp" sbTo
          , field "source_rows" sbRows
          , field "estimated_chunks" chunkCount
          , field "estimated_rollup_rows_upper_bound" rollupUpperBound
          ]

runStatus :: Connection -> AdminRuntime -> IO ()
runStatus conn runtime = do
  historyTarget <- loadLatestHistoryTarget conn runtime
  activeTarget <-
    case historyTarget of
      Nothing -> pure Nothing
      Just _ ->
        getLatestPublishedCandleHistoryIngestion
          conn
          defaultCandleMarketId
          (arChainId runtime)
          defaultBasketSeriesId
  publicationReady <- historyTargetPublicationReady conn historyTarget
  emitHistoryTargetStatus historyTarget activeTarget publicationReady
  forM_ [PriceRollup, VolumeRollup] $ \kind ->
    forM_ canonicalCandleIntervals $ \interval -> do
      coverage <- getCoverage conn runtime kind interval
      emitCoverage conn kind interval coverage Nothing

runBackfill
  :: Connection
  -> AdminRuntime
  -> AdminOptions
  -> RollupKind
  -> Bool
  -> IO ()
runBackfill conn runtime options kind isRepair = do
  historyTarget <-
    if kind == PriceRollup && not isRepair
      then requireCompletedHistoryTarget conn runtime
      else pure Nothing
  bounds <-
    case historyTarget of
      Just target -> Just <$> resolveCompletedHistoryTargetBounds conn options target
      Nothing -> resolveBounds conn runtime options kind
  case bounds of
    Nothing ->
      logInfo
        "perps_candle_backfill_complete"
        "No source rows matched the requested candle backfill"
        [field "kind" $ rollupKindName kind, field "affected_base_buckets" (0 :: Integer)]
    Just availableBounds -> do
      forM_ historyTarget $ \CompletedHistoryTarget {chtProgress} ->
        unless
          ( sbFrom availableBounds == chipStartTimestamp chtProgress
              && sbTo availableBounds == chipEndTimestampExclusive chtProgress
          ) $
          failWith
            "Target price backfill must cover the complete frozen ingestion range; omit --from/--to or pass its exact bounds"
      unless (isRepair || isJust historyTarget) $
        prepareBackfillCoverage conn runtime kind availableBounds
      ranges <-
        if isRepair
          then pure [BackfillRange availableBounds NewestFirst]
          else resumeRanges conn runtime kind availableBounds
      case ranges of
        [] -> do
          forM_ historyTarget $ \target ->
            publishCompletedHistoryTarget conn runtime target availableBounds
          coverage <- getCoverage conn runtime kind 60
          emitCoverage conn kind 60 coverage $ Just (0 :: Integer)
          logInfo
            "perps_candle_backfill_complete"
            "Requested candle range is already covered"
            [field "kind" $ rollupKindName kind, field "affected_base_buckets" (0 :: Integer)]
        _ -> do
          let chunks = concatMap (orderedChunks $ aoChunkSeconds options) ranges
          affectedCounts <- forM chunks $ \(chunkFrom, chunkTo) -> do
            affected <-
              if isRepair
                then backfillChunk conn runtime kind chunkFrom chunkTo False
                else
                  backfillChunk
                    conn
                    runtime
                    kind
                    chunkFrom
                    chunkTo
                    (isNothing historyTarget)
            coverage <- getCoverage conn runtime kind 60
            emitCoverage conn kind 60 coverage $ Just affected
            when (aoThrottleMs options > 0) $
              threadDelay $ aoThrottleMs options * 1_000
            pure affected
          forM_ historyTarget $ \target ->
            publishCompletedHistoryTarget conn runtime target availableBounds
          let processedFrom = minimum $ map (sbFrom . brBounds) ranges
              processedTo = maximum $ map (sbTo . brBounds) ranges
          finalCoverage <- getCoverage conn runtime kind 60
          emitCoverage conn kind 60 finalCoverage $ Just $ sum affectedCounts
          logInfo
            (if isRepair then "perps_candle_repair_data_written" else "perps_candle_backfill_complete")
            (if isRepair then "Perps candle repair rows were written pending verification" else "Perps candle backfill completed")
            [ field "kind" $ rollupKindName kind
            , field "from_timestamp" processedFrom
            , field "to_timestamp" processedTo
            , field "chunks" $ length chunks
            , field "affected_base_buckets" $ sum affectedCounts
            , field "repair" isRepair
            ]

backfillChunk
  :: Connection
  -> AdminRuntime
  -> RollupKind
  -> Integer
  -> Integer
  -> Bool
  -> IO Integer
backfillChunk conn runtime kind fromTimestamp toTimestamp publish =
  withTransaction conn $ do
    affected <- case kind of
      PriceRollup ->
        backfillLegacyBasketSnapshots
          conn
          defaultBasketSeriesId
          fromTimestamp
          toTimestamp
      VolumeRollup ->
        backfillMarketVolume
          conn
          (arChainId runtime)
          (arReleaseRouter runtime)
          fromTimestamp
          toTimestamp
    when publish $ do
      -- Extending coverage can change a formerly terminal page's rows or its
      -- has-earlier cursor. Allocate a new shared generation in the same
      -- transaction so generation-aware readers can reject the old terminal
      -- shape as this tranche becomes public.
      generation <- bumpDatasetGeneration conn runtime kind
      publishCoverage conn runtime kind fromTimestamp toTimestamp $ Just generation
    pure affected

publishCoverage
  :: Connection
  -> AdminRuntime
  -> RollupKind
  -> Integer
  -> Integer
  -> Maybe Integer
  -> IO ()
publishCoverage conn runtime kind fromTimestamp toTimestamp generation = do
  coverages <- mapM (getCoverage conn runtime kind) canonicalCandleIntervals
  let currentCoverage = join $ lookup 60 $ zip canonicalCandleIntervals coverages
      existingGenerations = [rcGeneration row | Just row <- coverages]
      publicationGeneration =
        fromMaybe (max 1 $ maximumOr 1 existingGenerations) generation
  let merged = mergeCoverageRange currentCoverage fromTimestamp toTimestamp
  forM_ (zip canonicalCandleIntervals coverages) $ \(interval, existing) -> do
    let intervalFrom = alignUp (mcrCoverageStart merged) interval
        (intervalEnd, intervalFinalizedThrough) =
          intervalCoverageEnvelope merged interval existing
    -- A coarse interval is publishable only after at least one whole bucket is
    -- finalized. coverage_end alone can be one aligned bucket ahead; exposing
    -- that zero-final envelope as complete would violate the trusted shape
    -- used by subsequent resumptions.
    when (intervalFrom < intervalFinalizedThrough) $ do
      finalizeCoveredRows
        conn runtime kind interval intervalFrom intervalFinalizedThrough
      upsertRollupCoverage conn $
        coverageRecord
          runtime
          kind
          interval
          intervalFrom
          intervalEnd
          intervalFinalizedThrough
          (Just publicationGeneration)
          existing

publishPreservedCoverage
  :: Connection
  -> AdminRuntime
  -> RollupKind
  -> [RollupCoverage]
  -> Integer
  -> IO ()
publishPreservedCoverage conn runtime kind preserved generation =
  forM_ preserved $ \coverage ->
    case
      ( rcCoverageStart coverage
      , rcCoverageEnd coverage
      , rcFinalizedThrough coverage
      ) of
      (Just coverageStart, Just _coverageEnd, Just finalizedThrough) -> do
        finalizeCoveredRows
          conn
          runtime
          kind
          (rcIntervalSeconds coverage)
          coverageStart
          finalizedThrough
        upsertRollupCoverage
          conn
          coverage
            { rcGeneration = generation
            , rcComplete = True
            , rcLastError = Nothing
            , rcMaintenanceFrom = Nothing
            , rcMaintenanceTo = Nothing
            }
      _ -> failWith "Preserved candle repair coverage is missing required bounds"

finalizeCoveredRows
  :: Connection
  -> AdminRuntime
  -> RollupKind
  -> Integer
  -> Integer
  -> Integer
  -> IO ()
finalizeCoveredRows conn runtime kind interval fromTimestamp toTimestamp = do
  _ <- case kind of
    PriceRollup ->
      execute
        conn
        "UPDATE perps_basket_candles SET finalized = TRUE, revision = revision + 1, \
        \updated_at = NOW() WHERE series_id = ? AND interval_seconds = ? \
        \AND bucket_start >= ? AND bucket_start < ? AND NOT finalized"
        (defaultBasketSeriesId, interval, fromTimestamp, toTimestamp)
    VolumeRollup ->
      execute
        conn
        "UPDATE perps_market_volume_rollups SET finalized = TRUE, revision = revision + 1, \
        \updated_at = NOW() WHERE chain_id = ? AND release_router = ? AND interval_seconds = ? \
        \AND bucket_start >= ? AND bucket_start < ? AND NOT finalized"
        ( arChainId runtime
        , arReleaseRouter runtime
        , interval
        , fromTimestamp
        , toTimestamp
        )
  pure ()

coverageRecord
  :: AdminRuntime
  -> RollupKind
  -> Integer
  -> Integer
  -> Integer
  -> Integer
  -> Maybe Integer
  -> Maybe RollupCoverage
  -> RollupCoverage
coverageRecord
  runtime kind interval fromTimestamp coverageEnd finalizedThrough generation existing =
  RollupCoverage
    { rcKind = kind
    , rcSeriesId = case kind of PriceRollup -> Just defaultBasketSeriesId; VolumeRollup -> Nothing
    , rcChainId = case kind of PriceRollup -> Nothing; VolumeRollup -> Just $ arChainId runtime
    , rcReleaseRouter = case kind of PriceRollup -> Nothing; VolumeRollup -> Just $ arReleaseRouter runtime
    , rcIntervalSeconds = interval
    , rcCoverageStart = Just fromTimestamp
    , rcCoverageEnd = Just coverageEnd
    , rcFinalizedThrough = Just finalizedThrough
    , rcGeneration = fromMaybe (maybe 1 (max 1 . rcGeneration) existing) generation
    , rcComplete = True
    , rcDerivationVersion = candleDerivationVersion
    , rcLastError = Nothing
    , rcMaintenanceFrom = Nothing
    , rcMaintenanceTo = Nothing
    }

mergeCoverageRange :: Maybe RollupCoverage -> Integer -> Integer -> MergedCoverageRange
mergeCoverageRange existing fromTimestamp toTimestamp =
  case (existing, trustedCoverageRange existing) of
    (Just RollupCoverage {rcCoverageEnd = Just oldCoverageEnd}, Just (oldFrom, oldFinalizedThrough))
      | oldFrom <= toTimestamp && oldFinalizedThrough >= fromTimestamp ->
          -- Merge each interval against its exact envelope below. The trusted
          -- read range ends at finalized_through, while coverage_end can carry
          -- a newer checked/indexed watermark that must never move backward.
          MergedCoverageRange
            (min oldFrom fromTimestamp)
            toTimestamp
            (max oldCoverageEnd toTimestamp)
            (max oldFinalizedThrough toTimestamp)
            True
    _ ->
      MergedCoverageRange
        fromTimestamp
        toTimestamp
        toTimestamp
        toTimestamp
        False

intervalCoverageEnvelope
  :: MergedCoverageRange
  -> Integer
  -> Maybe RollupCoverage
  -> (Integer, Integer)
intervalCoverageEnvelope merged interval existing
  | mcrMergeTrustedEnvelope merged =
      case (existing, trustedCoverageRange existing) of
        ( Just RollupCoverage
            { rcCoverageEnd = Just coverageEnd
            , rcFinalizedThrough = Just finalizedThrough
            }
          , Just _
          ) ->
            ( max coverageEnd alignedPublishedEnd
            , max finalizedThrough alignedPublishedEnd
            )
        _ -> alignedEnvelope
  | otherwise = alignedEnvelope
 where
  alignedPublishedEnd = alignDown (mcrPublishedEnd merged) interval
  alignedEnvelope =
    ( alignDown (mcrFallbackCoverageEnd merged) interval
    , alignDown (mcrFallbackFinalizedThrough merged) interval
    )

resumeRanges
  :: Connection
  -> AdminRuntime
  -> RollupKind
  -> SourceBounds
  -> IO [BackfillRange]
resumeRanges conn runtime kind bounds@SourceBounds {sbFrom, sbTo} = do
  coverage <- getCoverage conn runtime kind 60
  pure $ case trustedCoverageRange coverage of
    Just (coveredFrom, coveredTo)
      | coveredFrom <= sbTo && coveredTo >= sbFrom ->
            [ BackfillRange
                (bounds {sbFrom = max sbFrom coveredTo})
                OldestFirst
            | coveredTo < sbTo
            ]
              <> [ BackfillRange
                    (bounds {sbTo = min sbTo coveredFrom})
                    NewestFirst
                 | sbFrom < coveredFrom
                 ]
    _ -> [BackfillRange bounds NewestFirst]

trustedCoverageRange :: Maybe RollupCoverage -> Maybe (Integer, Integer)
trustedCoverageRange coverage =
  case coverage of
    Just row@RollupCoverage
      { rcCoverageStart = Just coverageStart
      , rcCoverageEnd = Just coverageEnd
      , rcFinalizedThrough = Just finalizedThrough
      , rcComplete = True
      }
        | rcDerivationVersion row == candleDerivationVersion
            && rcGeneration row > 0
            && rcGeneration row < generationRadix
            && coverageStart <= finalizedThrough
            && finalizedThrough <= coverageEnd ->
            Just (coverageStart, finalizedThrough)
    _ -> Nothing

prepareBackfillCoverage
  :: Connection
  -> AdminRuntime
  -> RollupKind
  -> SourceBounds
  -> IO ()
prepareBackfillCoverage conn runtime kind SourceBounds {sbFrom, sbTo} = do
  coverages <- mapM (getCoverage conn runtime kind) canonicalCandleIntervals
  let intervalCoverages = zip canonicalCandleIntervals coverages
      baseCoverage = case intervalCoverages of
        ((60, coverage) : _) -> coverage
        _ -> Nothing
      baseTrustedRange = trustedCoverageRange baseCoverage
      coverageShapeTrusted = case (baseCoverage, baseTrustedRange) of
        (Nothing, Nothing) -> not $ any isJust coverages
        (Just baseRow, Just (baseFrom, baseTo)) ->
          all (intervalCoverageMatches baseRow baseFrom baseTo) intervalCoverages
        _ -> False
      requestedIsDisjoint = case baseTrustedRange of
        Nothing -> False
        Just (coveredFrom, coveredTo) -> coveredTo < sbFrom || coveredFrom > sbTo
      requiresReplacement = not coverageShapeTrusted || requestedIsDisjoint
  when requiresReplacement $ do
    markKindIncomplete conn runtime kind "admin_backfill_replaced_untrusted_coverage"
    withTransaction conn $ do
      _ <- bumpDatasetGeneration conn runtime kind
      pure ()
 where
  intervalCoverageMatches baseRow baseFrom baseTo (interval, coverage) =
    let expectedFrom = alignUp baseFrom interval
        expectedTo = alignDown baseTo interval
     in if expectedFrom >= expectedTo
          then maybe True (not . rcComplete) coverage
          else case coverage of
            Just row ->
              trustedCoverageRange (Just row) == Just (expectedFrom, expectedTo)
                && rcGeneration row == rcGeneration baseRow
            Nothing -> False

maximumOr :: Ord a => a -> [a] -> a
maximumOr = foldr max

markKindIncomplete
  :: Connection
  -> AdminRuntime
  -> RollupKind
  -> T.Text
  -> IO ()
markKindIncomplete conn runtime kind reason = do
  withTransaction conn $
    markKindIncompleteWithinTransaction conn runtime kind reason

markKindIncompleteWithinTransaction
  :: Connection
  -> AdminRuntime
  -> RollupKind
  -> T.Text
  -> IO ()
markKindIncompleteWithinTransaction conn runtime kind reason =
  forM_ canonicalCandleIntervals $ \interval -> do
    _ <- case kind of
      PriceRollup ->
        markRollupCoverageIncomplete
          conn
          kind
          (Just defaultBasketSeriesId)
          Nothing
          Nothing
          interval
          reason
      VolumeRollup ->
        markRollupCoverageIncomplete
          conn
          kind
          Nothing
          (Just $ arChainId runtime)
          (Just $ arReleaseRouter runtime)
          interval
          reason
    pure ()

beginRepairMaintenance
  :: Connection
  -> AdminRuntime
  -> AdminOptions
  -> [RollupKind]
  -> IO [RepairMaintenance]
beginRepairMaintenance conn runtime options kinds = do
  (repairFrom, repairTo) <- repairBoundsFromOptions options
  withRepairDatasetLocks conn runtime kinds $
    withTransaction conn $ do
      prepared <- forM kinds $ \kind -> do
        coverages <- ensureRepairRangeCovered conn runtime options kind
        pure (kind, coverages)
      -- Invalidate every selected kind before rebuilding any data. This
      -- prevents `repair all` from briefly reopening combined reads with one
      -- old dataset. Session-level dataset locks are acquired before this
      -- transaction begins, so its reads and preserved bounds cannot race a
      -- live writer.
      maintenance <- forM prepared $ \(kind, preserved) -> do
        generation <- beginRepairMaintenanceKind conn runtime kind repairFrom repairTo
        pure $
          RepairMaintenance
            kind
            (map repairPublicationRecord preserved)
            generation
            repairFrom
            repairTo
      forM_ maintenance $ assertRepairMaintenance conn runtime
      pure maintenance

repairBoundsFromOptions :: AdminOptions -> IO (Integer, Integer)
repairBoundsFromOptions AdminOptions {aoFrom = Just repairFrom, aoTo = Just repairTo} =
  pure (repairFrom, repairTo)
repairBoundsFromOptions _ = failWith "repair requires both --from and --to"

beginRepairMaintenanceKind
  :: Connection
  -> AdminRuntime
  -> RollupKind
  -> Integer
  -> Integer
  -> IO Integer
beginRepairMaintenanceKind conn runtime kind repairFrom repairTo =
  case kind of
    PriceRollup ->
      beginRollupMaintenance
        conn
        PriceRollup
        (Just defaultBasketSeriesId)
        Nothing
        Nothing
        repairFrom
        repairTo
    VolumeRollup ->
      beginRollupMaintenance
        conn
        VolumeRollup
        Nothing
        (Just $ arChainId runtime)
        (Just $ arReleaseRouter runtime)
        repairFrom
        repairTo

repairPublicationRecord :: RollupCoverage -> RollupCoverage
repairPublicationRecord coverage =
  coverage
    { rcComplete = True
    , rcLastError = Nothing
    , rcMaintenanceFrom = Nothing
    , rcMaintenanceTo = Nothing
    }

assertRepairMaintenance
  :: Connection
  -> AdminRuntime
  -> RepairMaintenance
  -> IO ()
assertRepairMaintenance conn runtime RepairMaintenance {rmKind, rmPublicationCoverage, rmGeneration, rmRepairFrom, rmRepairTo} = do
  current <- mapM (getCoverage conn runtime rmKind) canonicalCandleIntervals
  let expectedByInterval =
        [ (rcIntervalSeconds row, row)
        | row <- rmPublicationCoverage
        ]
      currentMatches (interval, maybeRow) =
        case (lookup interval expectedByInterval, maybeRow) of
          (Just expected, Just actual) ->
            and
              [ rcCoverageStart actual == rcCoverageStart expected
              , rcCoverageEnd actual == rcCoverageEnd expected
              , rcFinalizedThrough actual == rcFinalizedThrough expected
              , rcGeneration actual == rmGeneration
              , not $ rcComplete actual
              , rcDerivationVersion actual == candleDerivationVersion
              , rcLastError actual == Just boundedRepairReason
              , rcMaintenanceFrom actual == Just rmRepairFrom
              , rcMaintenanceTo actual == Just rmRepairTo
              ]
          _ -> False
  unless (all currentMatches $ zip canonicalCandleIntervals current) $
    failWith $
      "Candle repair maintenance state changed concurrently for "
        <> T.unpack (rollupKindName rmKind)

verifyKind :: Connection -> AdminRuntime -> AdminOptions -> RollupKind -> IO Bool
verifyKind conn runtime options kind = do
  bounds <- resolveVerificationBounds conn runtime options kind
  case bounds of
    Nothing -> do
      logError
        "perps_candle_verification_failed"
        "No canonical source range is available for candle verification"
        [field "kind" $ rollupKindName kind]
      pure False
    Just SourceBounds {sbFrom, sbTo}
      | not $ hasFullCanonicalRange sbFrom sbTo -> do
          logError
            "perps_candle_verification_failed"
            "Requested range does not contain a full aligned bucket for every canonical interval"
            [ field "kind" $ rollupKindName kind
            , field "from_timestamp" sbFrom
            , field "to_timestamp" sbTo
            ]
          pure False
    Just SourceBounds {sbFrom, sbTo} -> do
      coverages <- mapM (getCoverage conn runtime kind) canonicalCandleIntervals
      let relevantGenerations =
            [ rcGeneration row
            | (interval, Just row) <- zip canonicalCandleIntervals coverages
            , alignUp sbFrom interval < alignDown sbTo interval
            ]
          generationConsistent = allEqual relevantGenerations
      results <- forM (zip canonicalCandleIntervals coverages) $ \(interval, coverage) -> do
        let verificationFrom = alignUp sbFrom interval
            verificationTo = alignDown sbTo interval
        expected <- expectedBucketCount conn runtime kind interval verificationFrom verificationTo
        actual <- actualBucketCount conn runtime kind interval verificationFrom verificationTo
        mismatched <- mismatchedBucketCount conn runtime kind interval verificationFrom verificationTo
        invalidRows <- invalidActualBucketCount conn runtime kind interval verificationFrom verificationTo
        let rangeCovered = coversRange coverage verificationFrom verificationTo
            finalizedCovered = coversFinalizedRange coverage verificationFrom verificationTo
            derivationMatches = hasCurrentDerivation coverage verificationFrom verificationTo
            generationValid = hasValidGeneration coverage verificationFrom verificationTo
            metadataConsistent = hasConsistentCoverageMetadata coverage verificationFrom verificationTo
            coverageValid =
              and
                [ rangeCovered
                , finalizedCovered
                , derivationMatches
                , generationValid
                , generationConsistent
                , metadataConsistent
                ]
            matches = actual == expected && mismatched == 0 && invalidRows == 0 && coverageValid
        logInfo
          "perps_candle_verification"
          "Compared rollup coverage with canonical source buckets"
          [ field "kind" $ rollupKindName kind
          , field "interval_seconds" interval
          , field "from_timestamp" verificationFrom
          , field "to_timestamp" verificationTo
          , field "expected_buckets" expected
          , field "actual_buckets" actual
          , field "mismatched_buckets" mismatched
          , field "invalid_rollup_rows" invalidRows
          , field "coverage_complete" rangeCovered
          , field "finalized_coverage_complete" finalizedCovered
          , field "derivation_version_matches" derivationMatches
          , field "generation_valid" generationValid
          , field "dataset_generation_consistent" generationConsistent
          , field "coverage_metadata_consistent" metadataConsistent
          , field "matches" matches
          ]
        pure matches
      pure $ and results

allEqual :: Eq a => [a] -> Bool
allEqual [] = True
allEqual (value : values) = all (== value) values

verifyKindValues :: Connection -> AdminRuntime -> AdminOptions -> RollupKind -> IO Bool
verifyKindValues conn runtime options kind = do
  bounds <- resolveVerificationBounds conn runtime options kind
  case bounds of
    Nothing -> pure False
    Just SourceBounds {sbFrom, sbTo} -> do
      results <- forM canonicalCandleIntervals $ \interval -> do
        let verificationFrom = alignUp sbFrom interval
            verificationTo = alignDown sbTo interval
        expected <- expectedBucketCount conn runtime kind interval verificationFrom verificationTo
        actual <- actualBucketCount conn runtime kind interval verificationFrom verificationTo
        mismatched <- mismatchedBucketCount conn runtime kind interval verificationFrom verificationTo
        invalidRows <- invalidActualBucketCount conn runtime kind interval verificationFrom verificationTo
        let matches = actual == expected && mismatched == 0 && invalidRows == 0
        logInfo
          "perps_candle_repair_reconciliation"
          "Reconciled repaired rollup rows before publishing coverage"
          [ field "kind" $ rollupKindName kind
          , field "interval_seconds" interval
          , field "from_timestamp" verificationFrom
          , field "to_timestamp" verificationTo
          , field "expected_buckets" expected
          , field "actual_buckets" actual
          , field "mismatched_buckets" mismatched
          , field "invalid_rollup_rows" invalidRows
          , field "matches" matches
          ]
        pure matches
      pure $ and results

hasFullCanonicalRange :: Integer -> Integer -> Bool
hasFullCanonicalRange fromTimestamp toTimestamp =
  all
    (\interval -> alignUp fromTimestamp interval < alignDown toTimestamp interval)
    canonicalCandleIntervals

coversRange :: Maybe RollupCoverage -> Integer -> Integer -> Bool
coversRange _ fromTimestamp toTimestamp | fromTimestamp >= toTimestamp = True
coversRange coverage fromTimestamp toTimestamp =
  case coverage of
    Just RollupCoverage
      { rcCoverageStart = Just coveredFrom
      , rcCoverageEnd = Just coveredTo
      , rcComplete = True
      , rcDerivationVersion = derivationVersion
      } ->
        derivationVersion == candleDerivationVersion
          && coveredFrom <= fromTimestamp
          && coveredTo >= toTimestamp
    _ -> False

coversFinalizedRange :: Maybe RollupCoverage -> Integer -> Integer -> Bool
coversFinalizedRange _ fromTimestamp toTimestamp | fromTimestamp >= toTimestamp = True
coversFinalizedRange coverage _fromTimestamp toTimestamp =
  case coverage >>= rcFinalizedThrough of
    Just finalizedThrough -> finalizedThrough >= toTimestamp
    Nothing -> False

hasCurrentDerivation :: Maybe RollupCoverage -> Integer -> Integer -> Bool
hasCurrentDerivation _ fromTimestamp toTimestamp | fromTimestamp >= toTimestamp = True
hasCurrentDerivation coverage _fromTimestamp _toTimestamp =
  maybe False ((== candleDerivationVersion) . rcDerivationVersion) coverage

hasValidGeneration :: Maybe RollupCoverage -> Integer -> Integer -> Bool
hasValidGeneration _ fromTimestamp toTimestamp | fromTimestamp >= toTimestamp = True
hasValidGeneration coverage _fromTimestamp _toTimestamp =
  maybe False (\row -> rcGeneration row > 0 && rcGeneration row < generationRadix) coverage

hasConsistentCoverageMetadata :: Maybe RollupCoverage -> Integer -> Integer -> Bool
hasConsistentCoverageMetadata _ fromTimestamp toTimestamp | fromTimestamp >= toTimestamp = True
hasConsistentCoverageMetadata coverage _fromTimestamp _toTimestamp =
  case coverage of
    Just RollupCoverage
      { rcCoverageStart = Just coverageStart
      , rcCoverageEnd = Just coverageEnd
      , rcFinalizedThrough = Just finalizedThrough
      } -> coverageStart <= finalizedThrough && finalizedThrough <= coverageEnd
    _ -> False

generationRadix :: Integer
generationRadix = 67_108_864

bumpDatasetGeneration :: Connection -> AdminRuntime -> RollupKind -> IO Integer
bumpDatasetGeneration conn runtime = \case
  PriceRollup ->
    bumpRollupDatasetGeneration
      conn
      PriceRollup
      (Just defaultBasketSeriesId)
      Nothing
      Nothing
  VolumeRollup ->
    bumpRollupDatasetGeneration
      conn
      VolumeRollup
      Nothing
      (Just $ arChainId runtime)
      (Just $ arReleaseRouter runtime)

runFinalizerProbe :: Connection -> AdminRuntime -> AdminOptions -> IO ()
runFinalizerProbe conn runtime AdminOptions {aoBoundary = Just boundary, aoMaxRuntimeSeconds} = do
  databaseTimestamp <- databaseClockNow conn
  plan <-
    either (failWith . T.unpack) pure $
      planFinalizerProbe databaseTimestamp boundary aoMaxRuntimeSeconds
  logInfo
    "perps_candle_finalizer_probe_scheduled"
    "Scheduled the bounded Sepolia hourly finalizer probe"
    [ field "boundary" $ fppBoundary plan
    , field "acquire_at" $ fppAcquireAt plan
    , field "grace_expires_at" $ fppGraceExpiresAt plan
    , field "release_at" $ fppReleaseAt plan
    , field "recovery_deadline" $ fppRecoveryDeadline plan
    , field "interval_seconds" (3_600 :: Integer)
    ]
  _ <- waitForDatabaseTime conn $ fppAcquireAt plan
  (priceGeneration, volumeGeneration) <-
    withTransactionMode
      ( TransactionMode
          { isolationLevel = ReadCommitted
          , readWriteMode = ReadOnly
          }
      )
      conn $ do
        lockBasketPriceDataset conn defaultBasketSeriesId
        acquiredAt <- databaseClockNow conn
        when (acquiredAt > fppAcquireAt plan + 5) $
          failWith "Finalizer probe did not acquire the price writer lock by boundary + 110 seconds"
        readOnlyRows <-
          query conn "SELECT current_setting('transaction_read_only')" () :: IO [Only T.Text]
        unless (readOnlyRows == [Only "on"]) $
          failWith "Finalizer probe transaction is not read only"
        requireFinalizerProbeSeriesWindow conn plan
        (priceCoverage, volumeCoverage) <- readFinalizerProbeCoverage conn runtime
        either (failWith . T.unpack) pure $
          validateFinalizerProbePrestate
            (fppBoundary plan)
            (arChainId runtime)
            (arReleaseRouter runtime)
            priceCoverage
            volumeCoverage
        logInfo
          "perps_candle_finalizer_probe_lock_acquired"
          "Acquired the read-only Sepolia price finalizer probe lock"
          [ field "boundary" $ fppBoundary plan
          , field "acquired_at" acquiredAt
          , field "release_at" $ fppReleaseAt plan
          , field "price_generation" $ rcGeneration priceCoverage
          , field "volume_generation" $ rcGeneration volumeCoverage
          , field "price_coverage_end" $ rcCoverageEnd priceCoverage
          , field "volume_coverage_end" $ rcCoverageEnd volumeCoverage
          , field "price_finalized_through" $ rcFinalizedThrough priceCoverage
          , field "volume_finalized_through" $ rcFinalizedThrough volumeCoverage
          , field "transaction_read_only" True
          ]
        graceObservedAt <- waitForDatabaseTime conn $ fppGraceExpiresAt plan
        logInfo
          "perps_candle_finalizer_probe_grace_expired"
          "Held the price writer lock through the hourly publication grace"
          [ field "boundary" $ fppBoundary plan
          , field "observed_at" graceObservedAt
          , field "release_at" $ fppReleaseAt plan
          ]
        _ <- waitForDatabaseTime conn $ fppReleaseAt plan
        pure (rcGeneration priceCoverage, rcGeneration volumeCoverage)
  releasedAt <- databaseClockNow conn
  when (releasedAt < fppReleaseAt plan) $
    failWith "Finalizer probe released the price writer lock before its bounded deadline"
  when (releasedAt > fppReleaseAt plan + 5) $
    failWith "Finalizer probe released the price writer lock after boundary + 155 seconds"
  logInfo
    "perps_candle_finalizer_probe_lock_released"
    "Released the read-only Sepolia price finalizer probe lock"
    [ field "boundary" $ fppBoundary plan
    , field "released_at" releasedAt
    , field "recovery_deadline" $ fppRecoveryDeadline plan
    ]
  waitForFinalizerProbeRecovery
    conn
    runtime
    plan
    priceGeneration
    volumeGeneration
runFinalizerProbe _ _ _ =
  failWith "Finalizer probe requires one aligned --boundary"

readFinalizerProbeCoverage
  :: Connection -> AdminRuntime -> IO (RollupCoverage, RollupCoverage)
readFinalizerProbeCoverage conn runtime = do
  price <-
    getRollupCoverage
      conn PriceRollup (Just defaultBasketSeriesId) Nothing Nothing 3_600
      >>= maybe (failWith "Finalizer probe price hourly coverage is unavailable") pure
  volume <-
    getRollupCoverage
      conn
      VolumeRollup
      Nothing
      (Just $ arChainId runtime)
      (Just $ arReleaseRouter runtime)
      3_600
      >>= maybe (failWith "Finalizer probe volume hourly coverage is unavailable") pure
  pure (price, volume)

waitForFinalizerProbeRecovery
  :: Connection
  -> AdminRuntime
  -> FinalizerProbePlan
  -> Integer
  -> Integer
  -> IO ()
waitForFinalizerProbeRecovery conn runtime plan priceGeneration volumeGeneration = go
 where
  go = do
    (priceCoverage, volumeCoverage, activeSeries, observedAt) <-
      withTransactionMode
        ( TransactionMode
            { isolationLevel = RepeatableRead
            , readWriteMode = ReadOnly
            }
        )
        conn $ do
          (priceCoverage, volumeCoverage) <- readFinalizerProbeCoverage conn runtime
          currentTimestamp <- databaseClockNow conn
          activeSeries <- getActiveBasketSeriesId conn currentTimestamp
          observedAt <- databaseClockNow conn
          pure (priceCoverage, volumeCoverage, activeSeries, observedAt)
    unless (activeSeries == Just defaultBasketSeriesId) $
      failWith "Finalizer probe active basket series changed before endpoint recovery"
    if observedAt > fppRecoveryDeadline plan
      then failWith "Hourly finalization did not recover by boundary + 165 seconds"
      else if finalizerProbeRecovered
        (fppBoundary plan)
        (arChainId runtime)
        (arReleaseRouter runtime)
        priceGeneration
        volumeGeneration
        priceCoverage
        volumeCoverage
      then
        logInfo
          "perps_candle_finalizer_probe_recovery_complete"
          "Hourly price and volume finalization recovered after the bounded probe"
          [ field "boundary" $ fppBoundary plan
          , field "recovered_at" observedAt
          , field "price_generation" $ rcGeneration priceCoverage
          , field "volume_generation" $ rcGeneration volumeCoverage
          , field "price_coverage_end" $ rcCoverageEnd priceCoverage
          , field "volume_coverage_end" $ rcCoverageEnd volumeCoverage
          , field "price_finalized_through" $ rcFinalizedThrough priceCoverage
          , field "volume_finalized_through" $ rcFinalizedThrough volumeCoverage
          ]
      else if observedAt == fppRecoveryDeadline plan
        then failWith "Hourly finalization did not recover by boundary + 165 seconds"
        else threadDelay 1_000_000 >> go

waitForDatabaseTime :: Connection -> Integer -> IO Integer
waitForDatabaseTime conn targetTimestamp = do
  currentTimestamp <- databaseClockNow conn
  if currentTimestamp >= targetTimestamp
    then pure currentTimestamp
    else do
      let remaining = targetTimestamp - currentTimestamp
          delayMicros =
            if remaining > 2
              then fromIntegral (min 5 $ remaining - 1) * 1_000_000
              else 100_000
      threadDelay delayMicros
      waitForDatabaseTime conn targetTimestamp

requireFinalizerProbeSeriesWindow :: Connection -> FinalizerProbePlan -> IO ()
requireFinalizerProbeSeriesWindow conn plan = do
  rows <-
    query conn
      "SELECT series_id, effective_from, effective_to \
      \FROM perps_basket_definitions \
      \WHERE active AND effective_from <= ? \
      \AND (effective_to IS NULL OR effective_to > ?) \
      \ORDER BY effective_from, series_id"
      (fppRecoveryDeadline plan, fppBoundary plan - 1)
      :: IO [(T.Text, Integer, Maybe Integer)]
  case rows of
    [(seriesId, effectiveFrom, effectiveTo)]
      | seriesId == defaultBasketSeriesId
      , effectiveFrom <= fppBoundary plan - 1
      , maybe True (> fppRecoveryDeadline plan) effectiveTo -> pure ()
    _ ->
      failWith
        "Finalizer probe requires dxy-v1 to remain the sole active basket series throughout the control window"

databaseClockNow :: Connection -> IO Integer
databaseClockNow conn = do
  rows <-
    query conn
      "SELECT FLOOR(EXTRACT(EPOCH FROM clock_timestamp()))::BIGINT"
      () :: IO [Only Integer]
  case rows of
    [Only timestamp] -> pure timestamp
    _ -> failWith "Could not read the advancing database clock"

withVerificationSnapshot :: Connection -> IO a -> IO a
withVerificationSnapshot =
  withTransactionMode $
    TransactionMode
      { isolationLevel = RepeatableRead
      , readWriteMode = ReadOnly
      }

withRepairPublicationTransaction :: Connection -> IO a -> IO a
withRepairPublicationTransaction =
  withTransactionMode $
    TransactionMode
      { isolationLevel = RepeatableRead
      , readWriteMode = ReadWrite
      }

withRepairDatasetLocks
  :: Connection
  -> AdminRuntime
  -> [RollupKind]
  -> IO a
  -> IO a
withRepairDatasetLocks conn runtime kinds action = do
  let lockKeys = map (repairDatasetLockKey runtime) kinds
  withLocks lockKeys action
 where
  -- Nest brackets so an exception or lock timeout while acquiring a later
  -- dataset still releases every earlier session-level lock before the pooled
  -- connection is returned.
  withLocks [] nestedAction = nestedAction
  withLocks (lockKey : remaining) nestedAction =
    bracket_ (acquire lockKey) (release lockKey) $
      withLocks remaining nestedAction

  acquire (namespace, scope, discriminator) = do
    _ <-
      query
        conn
        "SELECT 1::BIGINT FROM (SELECT pg_advisory_lock(hashtextextended(?, ?))) locked"
        (namespace <> ":" <> scope <> ":" <> T.pack (show discriminator), 0 :: Integer)
        :: IO [Only Integer]
    pure ()

  release (namespace, scope, discriminator) = do
    rows <-
      query
        conn
        "SELECT pg_advisory_unlock(hashtextextended(?, ?))"
        (namespace <> ":" <> scope <> ":" <> T.pack (show discriminator), 0 :: Integer)
        :: IO [Only Bool]
    unless (rows == [Only True]) $
      failWith "Could not release the candle writer dataset lock"

repairDatasetLockKey :: AdminRuntime -> RollupKind -> (T.Text, T.Text, Integer)
repairDatasetLockKey runtime = \case
  PriceRollup -> ("price-dataset", defaultBasketSeriesId, 0)
  VolumeRollup -> ("volume-dataset", arReleaseRouter runtime, arChainId runtime)

expectedBucketCount
  :: Connection
  -> AdminRuntime
  -> RollupKind
  -> Integer
  -> Integer
  -> Integer
  -> IO Integer
expectedBucketCount conn runtime kind interval fromTimestamp toTimestamp = do
  rows <- case kind of
    PriceRollup ->
      query
        conn
        "WITH input AS (SELECT ?::TEXT AS series_id, ?::BIGINT AS from_timestamp, \
        \ ?::BIGINT AS to_timestamp, ?::BIGINT AS interval_seconds), \
        \observed_prioritized AS ( \
        \ SELECT o.publish_time, o.source_priority, \
        \   MAX(o.source_priority) OVER (PARTITION BY o.publish_time) AS max_source_priority \
        \ FROM perps_basket_observations o CROSS JOIN input i \
        \ WHERE o.series_id = i.series_id AND o.publish_time >= i.from_timestamp \
        \   AND o.publish_time < i.to_timestamp), \
        \observed_ranked AS (SELECT publish_time FROM observed_prioritized \
        \ WHERE source_priority = max_source_priority), \
        \observed_minutes AS (SELECT DISTINCT (publish_time / 60) * 60 AS bucket_start \
        \ FROM observed_ranked), legacy_ranked AS ( \
        \ SELECT s.timestamp, ROW_NUMBER() OVER (PARTITION BY s.timestamp \
        \   ORDER BY s.interval_seconds ASC, s.id DESC) AS timestamp_rank \
        \ FROM perps_basket_snapshots s CROSS JOIN input i \
        \ WHERE s.timestamp >= i.from_timestamp AND s.timestamp < i.to_timestamp), \
        \legacy_minutes AS ( \
        \ SELECT DISTINCT (s.timestamp / 60) * 60 AS bucket_start \
        \ FROM legacy_ranked s \
        \ WHERE s.timestamp_rank = 1 \
        \   AND NOT EXISTS (SELECT 1 FROM observed_minutes o \
        \     WHERE o.bucket_start = (s.timestamp / 60) * 60)), \
        \canonical_minutes AS (SELECT * FROM observed_minutes UNION ALL SELECT * FROM legacy_minutes) \
        \SELECT COUNT(DISTINCT m.bucket_start / i.interval_seconds)::BIGINT \
        \FROM canonical_minutes m CROSS JOIN input i"
        (defaultBasketSeriesId, fromTimestamp, toTimestamp, interval)
    VolumeRollup ->
      query
        conn
        "SELECT COUNT(DISTINCT timestamp / ?)::BIGINT \
        \FROM perps_account_activity \
        \WHERE chain_id = ? AND release_router = ? \
        \AND timestamp >= ? AND timestamp < ? \
        \AND activity_type IN ('Open', 'Close', 'Liquidated') \
        \AND size_delta IS NOT NULL AND price IS NOT NULL"
        (interval, arChainId runtime, arReleaseRouter runtime, fromTimestamp, toTimestamp)
  case rows of
    [Only count] -> pure count
    _ -> failWith "Could not count canonical source buckets"

actualBucketCount
  :: Connection
  -> AdminRuntime
  -> RollupKind
  -> Integer
  -> Integer
  -> Integer
  -> IO Integer
actualBucketCount conn runtime kind interval fromTimestamp toTimestamp =
  case kind of
    PriceRollup ->
      countBasketCandles conn defaultBasketSeriesId interval fromTimestamp toTimestamp
    VolumeRollup ->
      countMarketVolumeRollups
        conn
        (arChainId runtime)
        (arReleaseRouter runtime)
        interval
        fromTimestamp
        toTimestamp

invalidActualBucketCount
  :: Connection
  -> AdminRuntime
  -> RollupKind
  -> Integer
  -> Integer
  -> Integer
  -> IO Integer
invalidActualBucketCount conn runtime kind interval fromTimestamp toTimestamp = do
  rows <- case kind of
    PriceRollup ->
      query
        conn
        "SELECT COUNT(*)::BIGINT FROM perps_basket_candles c \
        \JOIN perps_basket_definitions d ON d.series_id = c.series_id \
        \WHERE c.series_id = ? AND c.interval_seconds = ? \
        \AND c.bucket_start >= ? AND c.bucket_start < ? \
        \AND (NOT c.finalized OR c.revision <= 0 \
        \OR c.quality NOT IN ('observed', 'legacy_sampled', 'mixed') \
        \OR LEAST(c.raw_open_price, c.raw_high_price, c.raw_low_price, c.raw_close_price) <= 0 \
        \OR GREATEST(c.raw_open_price, c.raw_high_price, c.raw_low_price, c.raw_close_price) \
        \   >= (d.configuration ->> 'priceCap')::BIGINT)"
        (defaultBasketSeriesId, interval, fromTimestamp, toTimestamp)
    VolumeRollup ->
      query
        conn
        "SELECT COUNT(*)::BIGINT FROM perps_market_volume_rollups \
        \WHERE chain_id = ? AND release_router = ? AND interval_seconds = ? \
        \AND bucket_start >= ? AND bucket_start < ? \
        \AND (NOT finalized OR revision <= 0)"
        ( arChainId runtime
        , arReleaseRouter runtime
        , interval
        , fromTimestamp
        , toTimestamp
        )
  case rows of
    [Only count] -> pure count
    _ -> failWith "Could not validate candle rollup row flags"

mismatchedBucketCount
  :: Connection
  -> AdminRuntime
  -> RollupKind
  -> Integer
  -> Integer
  -> Integer
  -> IO Integer
mismatchedBucketCount conn runtime kind interval fromTimestamp toTimestamp = do
  rows <- case kind of
    PriceRollup ->
      query
        conn
        "WITH input AS (SELECT ?::TEXT AS series_id, ?::BIGINT AS from_timestamp, \
        \ ?::BIGINT AS to_timestamp, ?::BIGINT AS interval_seconds), \
        \observed_prioritized AS ( \
        \ SELECT o.observation_id, o.publish_time, o.basket_price, o.source, o.source_priority, \
        \   MAX(o.source_priority) OVER (PARTITION BY o.publish_time) AS max_source_priority \
        \ FROM perps_basket_observations o CROSS JOIN input i \
        \ WHERE o.series_id = i.series_id AND o.publish_time >= i.from_timestamp \
        \   AND o.publish_time < i.to_timestamp), \
        \observed_ranked AS ( \
        \ SELECT observation_id, publish_time, basket_price, source, source_priority \
        \ FROM observed_prioritized WHERE source_priority = max_source_priority), \
        \observed_minutes AS ( \
        \ SELECT (o.publish_time / 60) * 60 AS bucket_start, \
        \   (array_agg(o.basket_price ORDER BY o.publish_time ASC, o.source_priority DESC, \
        \     o.observation_id ASC))[1] AS raw_open_price, MAX(o.basket_price) AS raw_high_price, \
        \   MIN(o.basket_price) AS raw_low_price, \
        \   (array_agg(o.basket_price ORDER BY o.publish_time DESC, o.source_priority DESC, \
        \     o.observation_id DESC))[1] AS raw_close_price, \
        \   MIN(o.publish_time) AS first_observation_time, MAX(o.publish_time) AS last_observation_time, \
        \   COUNT(*)::INTEGER AS sample_count, \
        \   CASE WHEN bool_and(o.source = 'legacy_sampled') THEN 'legacy_sampled' \
        \        WHEN bool_or(o.source = 'legacy_sampled') THEN 'mixed' ELSE 'observed' END AS quality \
        \ FROM observed_ranked o GROUP BY o.publish_time / 60), \
        \legacy_ranked AS ( \
        \ SELECT s.id, s.timestamp, s.basket_price, \
        \   ROW_NUMBER() OVER (PARTITION BY s.timestamp \
        \     ORDER BY s.interval_seconds ASC, s.id DESC) AS timestamp_rank \
        \ FROM perps_basket_snapshots s CROSS JOIN input i \
        \ WHERE s.timestamp >= i.from_timestamp AND s.timestamp < i.to_timestamp), \
        \legacy_samples AS ( \
        \ SELECT id, timestamp, basket_price FROM legacy_ranked WHERE timestamp_rank = 1), \
        \legacy_minutes AS ( \
        \ SELECT (s.timestamp / 60) * 60 AS bucket_start, \
        \   (array_agg(s.basket_price ORDER BY s.timestamp ASC, s.id ASC))[1] AS raw_open_price, \
        \   MAX(s.basket_price) AS raw_high_price, MIN(s.basket_price) AS raw_low_price, \
        \   (array_agg(s.basket_price ORDER BY s.timestamp DESC, s.id DESC))[1] AS raw_close_price, \
        \   MIN(s.timestamp) AS first_observation_time, MAX(s.timestamp) AS last_observation_time, \
        \   COUNT(*)::INTEGER AS sample_count, 'legacy_sampled'::TEXT AS quality \
        \ FROM legacy_samples s \
        \ WHERE NOT EXISTS (SELECT 1 FROM observed_minutes o \
        \     WHERE o.bucket_start = (s.timestamp / 60) * 60) \
        \ GROUP BY s.timestamp / 60), \
        \canonical_minutes AS (SELECT * FROM observed_minutes UNION ALL SELECT * FROM legacy_minutes), \
        \expected AS ( \
        \ SELECT (m.bucket_start / i.interval_seconds) * i.interval_seconds AS bucket_start, \
        \   (array_agg(m.raw_open_price ORDER BY m.bucket_start ASC))[1] AS raw_open_price, \
        \   MAX(m.raw_high_price) AS raw_high_price, MIN(m.raw_low_price) AS raw_low_price, \
        \   (array_agg(m.raw_close_price ORDER BY m.bucket_start DESC))[1] AS raw_close_price, \
        \   MIN(m.first_observation_time) AS first_observation_time, \
        \   MAX(m.last_observation_time) AS last_observation_time, SUM(m.sample_count)::INTEGER AS sample_count, \
        \   CASE WHEN bool_and(m.quality = 'legacy_sampled') THEN 'legacy_sampled' \
        \        WHEN bool_and(m.quality = 'observed') THEN 'observed' ELSE 'mixed' END AS quality, \
        \   TRUE AS finalized \
        \ FROM canonical_minutes m CROSS JOIN input i \
        \ GROUP BY m.bucket_start / i.interval_seconds, i.interval_seconds), \
        \actual AS ( \
        \ SELECT c.bucket_start, c.raw_open_price, c.raw_high_price, c.raw_low_price, \
        \   c.raw_close_price, c.first_observation_time, c.last_observation_time, \
        \   c.sample_count, c.quality, c.finalized \
        \ FROM perps_basket_candles c CROSS JOIN input i \
        \ WHERE c.series_id = i.series_id AND c.interval_seconds = i.interval_seconds \
        \   AND c.bucket_start >= i.from_timestamp AND c.bucket_start < i.to_timestamp) \
        \SELECT COUNT(*)::BIGINT FROM expected e FULL OUTER JOIN actual a USING (bucket_start) \
        \WHERE (e.raw_open_price, e.raw_high_price, e.raw_low_price, e.raw_close_price, \
        \ e.first_observation_time, e.last_observation_time, e.sample_count, e.quality, e.finalized) \
        \ IS DISTINCT FROM \
        \ (a.raw_open_price, a.raw_high_price, a.raw_low_price, a.raw_close_price, \
        \ a.first_observation_time, a.last_observation_time, a.sample_count, a.quality, a.finalized)"
        (defaultBasketSeriesId, fromTimestamp, toTimestamp, interval)
    VolumeRollup ->
      query
        conn
        "WITH input AS (SELECT ?::BIGINT AS chain_id, ?::TEXT AS release_router, \
        \ ?::BIGINT AS interval_seconds, ?::BIGINT AS from_timestamp, ?::BIGINT AS to_timestamp), \
        \expected AS ( \
        \ SELECT (a.timestamp / i.interval_seconds) * i.interval_seconds AS bucket_start, \
        \   FLOOR(SUM(ABS(a.size_delta) * a.price)) AS volume_numerator, \
        \   COUNT(*)::BIGINT AS trade_count, MIN(a.block_number) AS first_source_block, \
        \   MAX(a.block_number) AS last_source_block \
        \ FROM perps_account_activity a CROSS JOIN input i \
        \ WHERE a.chain_id = i.chain_id AND a.release_router = i.release_router \
        \   AND a.timestamp >= i.from_timestamp AND a.timestamp < i.to_timestamp \
        \   AND a.activity_type IN ('Open', 'Close', 'Liquidated') \
        \   AND a.size_delta IS NOT NULL AND a.price IS NOT NULL \
        \ GROUP BY a.timestamp / i.interval_seconds, i.interval_seconds), \
        \actual AS ( \
        \ SELECT v.bucket_start, v.volume_numerator, v.trade_count, \
        \   v.first_source_block, v.last_source_block \
        \ FROM perps_market_volume_rollups v CROSS JOIN input i \
        \ WHERE v.chain_id = i.chain_id AND v.release_router = i.release_router \
        \   AND v.interval_seconds = i.interval_seconds \
        \   AND v.bucket_start >= i.from_timestamp AND v.bucket_start < i.to_timestamp) \
        \SELECT COUNT(*)::BIGINT FROM expected e FULL OUTER JOIN actual a USING (bucket_start) \
        \WHERE (e.volume_numerator, e.trade_count, e.first_source_block, e.last_source_block) \
        \ IS DISTINCT FROM \
        \ (a.volume_numerator, a.trade_count, a.first_source_block, a.last_source_block)"
        ( arChainId runtime
        , arReleaseRouter runtime
        , interval
        , fromTimestamp
        , toTimestamp
        )
  case rows of
    [Only count] -> pure count
    _ -> failWith "Could not reconcile candle rollup values"

resolveVerificationBounds
  :: Connection
  -> AdminRuntime
  -> AdminOptions
  -> RollupKind
  -> IO (Maybe SourceBounds)
resolveVerificationBounds conn runtime AdminOptions {aoFrom, aoTo} kind = do
  source <- sourceBounds conn runtime kind
  currentTimestamp <- databaseNow conn
  let finalizedCutoff = alignDown (currentTimestamp - arLatenessSeconds runtime) 60
      explicitBounds = SourceBounds <$> aoFrom <*> aoTo <*> pure 0
      inferredBounds = do
        available <- source
        let fromTimestamp = fromMaybe (sbFrom available) aoFrom
            defaultTo = min finalizedCutoff $ sbTo available
            toTimestamp = fromMaybe defaultTo aoTo
        pure available {sbFrom = fromTimestamp, sbTo = toTimestamp}
      -- Explicit bounds may select an empty subrange, but a globally empty
      -- canonical source domain is never valid evidence for publication.
      resolved = source >> (explicitBounds <|> inferredBounds)
  case resolved of
    Just bounds | sbFrom bounds >= sbTo bounds ->
      failWith "The resolved verification range is empty or reversed"
    _ -> pure resolved

ensureRepairRangeCovered
  :: Connection
  -> AdminRuntime
  -> AdminOptions
  -> RollupKind
  -> IO [RollupCoverage]
ensureRepairRangeCovered conn runtime AdminOptions {aoFrom, aoTo} kind =
  case (aoFrom, aoTo) of
    (Just requestedFrom, Just requestedTo) -> do
      source <- sourceBounds conn runtime kind
      when (isNothing source) $
        failWith "repair requires a non-empty canonical source domain"
      coverages <- mapM (getCoverage conn runtime kind) canonicalCandleIntervals
      let requestedRanges =
            [ (interval, alignUp requestedFrom interval, alignDown requestedTo interval, coverage)
            | (interval, coverage) <- zip canonicalCandleIntervals coverages
            ]
          relevantRanges =
            [ (interval, fromTimestamp, toTimestamp, coverage)
            | (interval, fromTimestamp, toTimestamp, coverage) <- requestedRanges
            , fromTimestamp < toTimestamp
            ]
      unless (repairCoverageSetEligible requestedFrom requestedTo relevantRanges) $
        failWith $
          "repair requires consistent finalized coverage or a resumable bounded repair for "
            <> T.unpack (rollupKindName kind)
      case sequence coverages of
        Just rows -> pure rows
        Nothing -> failWith "repair requires coverage metadata for every canonical interval"
    _ -> failWith "repair requires both --from and --to"

repairCoverageSetEligible
  :: Integer
  -> Integer
  -> [(Integer, Integer, Integer, Maybe RollupCoverage)]
  -> Bool
repairCoverageSetEligible _ _ [] = False
repairCoverageSetEligible requestedFrom requestedTo ranges =
  let generations =
        [ rcGeneration row
        | (_, _, _, Just row) <- ranges
        ]
      generationsAreConsistent =
        length generations == length ranges && allEqual generations
   in generationsAreConsistent
        && ( all completeRepairCoverageEligible ranges
              || all (incompleteRepairCoverageEligible requestedFrom requestedTo) ranges
           )

completeRepairCoverageEligible
  :: (Integer, Integer, Integer, Maybe RollupCoverage)
  -> Bool
completeRepairCoverageEligible (_, fromTimestamp, toTimestamp, coverage) =
  and
    [ coversRange coverage fromTimestamp toTimestamp
    , coversFinalizedRange coverage fromTimestamp toTimestamp
    , hasCurrentDerivation coverage fromTimestamp toTimestamp
    , hasValidGeneration coverage fromTimestamp toTimestamp
    , hasConsistentCoverageMetadata coverage fromTimestamp toTimestamp
    ]

incompleteRepairCoverageEligible
  :: Integer
  -> Integer
  -> (Integer, Integer, Integer, Maybe RollupCoverage)
  -> Bool
incompleteRepairCoverageEligible requestedFrom requestedTo (_, fromTimestamp, toTimestamp, coverage) =
  case coverage of
    Just RollupCoverage
      { rcCoverageStart = Just coveredFrom
      , rcCoverageEnd = Just coveredTo
      , rcFinalizedThrough = Just finalizedThrough
      , rcGeneration = generation
      , rcComplete = False
      , rcDerivationVersion = derivationVersion
      , rcLastError = Just lastError
      , rcMaintenanceFrom = maintenanceFrom
      , rcMaintenanceTo = maintenanceTo
      } ->
        and
          [ lastError == boundedRepairReason
          , derivationVersion == candleDerivationVersion
          , generation > 0
          , generation < generationRadix
          , coveredFrom <= fromTimestamp
          , finalizedThrough >= toTimestamp
          , coveredFrom <= finalizedThrough
          , finalizedThrough <= coveredTo
          , maintenanceFrom == Just requestedFrom
          , maintenanceTo == Just requestedTo
          ]
    _ -> False

resolveBounds
  :: Connection
  -> AdminRuntime
  -> AdminOptions
  -> RollupKind
  -> IO (Maybe SourceBounds)
resolveBounds conn runtime AdminOptions {aoAction, aoFrom, aoTo} kind =
  case (aoAction, aoFrom, aoTo) of
    (Repair _, Just requestedFrom, Just requestedTo) ->
      pure $
        Just $
          SourceBounds
            (alignDown requestedFrom 60)
            (alignUp requestedTo 60)
            0
    (Repair _, _, _) -> pure Nothing
    _ -> do
      source <- sourceBounds conn runtime kind
      currentTimestamp <- databaseNow conn
      let finalizedCutoff = alignDown (currentTimestamp - arLatenessSeconds runtime) 60
      pure $ case source of
        Nothing -> Nothing
        Just bounds ->
          let fromTimestamp =
                alignDown
                  (max (sbFrom bounds) $ fromMaybe (sbFrom bounds) aoFrom)
                  60
              requestedEnd = min (sbTo bounds) $ fromMaybe (sbTo bounds) aoTo
              toTimestamp = alignDown (min finalizedCutoff requestedEnd) 60
           in if fromTimestamp < toTimestamp
                then Just bounds {sbFrom = fromTimestamp, sbTo = toTimestamp}
                else Nothing

sourceBounds :: Connection -> AdminRuntime -> RollupKind -> IO (Maybe SourceBounds)
sourceBounds conn runtime kind = do
  rows <- case kind of
    PriceRollup -> do
      observationTableRows <-
        query
          conn
          "SELECT to_regclass('perps_basket_observations') IS NOT NULL"
          () :: IO [Only Bool]
      case observationTableRows of
        [Only True] ->
          query
            conn
            "WITH observed_prioritized AS ( \
            \ SELECT publish_time, source_priority, \
            \   MAX(source_priority) OVER (PARTITION BY publish_time) AS max_source_priority \
            \ FROM perps_basket_observations WHERE series_id = ?), \
            \observed_samples AS ( \
            \ SELECT publish_time AS timestamp FROM observed_prioritized \
            \ WHERE source_priority = max_source_priority), \
            \legacy_samples AS ( \
            \ SELECT DISTINCT ON (timestamp) timestamp FROM perps_basket_snapshots \
            \ ORDER BY timestamp, interval_seconds ASC, id DESC), \
            \source_times AS ( \
            \ SELECT timestamp FROM observed_samples \
            \ UNION ALL \
            \ SELECT timestamp FROM legacy_samples) \
            \SELECT MIN(timestamp), MAX(timestamp) + 1, COUNT(*)::BIGINT FROM source_times"
            (Only defaultBasketSeriesId)
        [Only False] ->
          query
            conn
            "WITH legacy_samples AS ( \
            \ SELECT DISTINCT ON (timestamp) timestamp FROM perps_basket_snapshots \
            \ ORDER BY timestamp, interval_seconds ASC, id DESC) \
            \SELECT MIN(timestamp), MAX(timestamp) + 1, COUNT(*)::BIGINT FROM legacy_samples"
            ()
        _ -> failWith "Could not determine whether the basket observation ledger exists"
    VolumeRollup ->
      let pinnedDeploymentStart =
            if arChainId runtime == 421614
              && T.toLower (arReleaseRouter runtime) == T.toLower perpsV2OrderRouter
              then Just perpsV2VolumeHistoryStartTimestamp
              else Nothing
       in query
            conn
            "WITH pinned_release AS ( \
        \ SELECT ?::BIGINT AS first_timestamp, ?::BIGINT AS deployment_block), \
        \release_candidate AS ( \
        \ SELECT pinned.first_timestamp, pinned.deployment_block AS proof_block, \
        \   ?::BIGINT AS chain_id, ?::TEXT AS release_router \
        \ FROM pinned_release pinned WHERE pinned.first_timestamp IS NOT NULL \
        \ UNION ALL \
        \ SELECT ((epoch.activation_timestamp + 59) / 60) * 60 AS first_timestamp, \
        \   epoch.activation_block AS proof_block, epoch.chain_id, epoch.release_router \
        \ FROM perps_market_release_epochs epoch CROSS JOIN pinned_release pinned \
        \ WHERE pinned.first_timestamp IS NULL AND epoch.market_id = ? \
        \   AND epoch.chain_id = ? AND epoch.release_router = ?), \
        \certified_release AS ( \
        \ SELECT candidate.first_timestamp \
        \ FROM release_candidate candidate \
        \ WHERE EXISTS ( \
        \   SELECT 1 FROM perps_indexer_state indexer_state \
        \   WHERE indexer_state.chain_id = candidate.chain_id \
        \   AND indexer_state.release_router = candidate.release_router \
        \   AND indexer_state.indexer_name \
        \     LIKE 'perps-history-costs-%:' || candidate.release_router \
        \   AND indexer_state.configured_start_block <= candidate.proof_block \
        \   AND indexer_state.last_indexed_block >= candidate.proof_block \
        \   AND indexer_state.last_indexed_block_hash ~ '^0x[0-9a-f]{64}$') \
        \ LIMIT 1), \
        \event_bounds AS ( \
        \ SELECT MIN(timestamp) AS first_timestamp, MAX(timestamp) + 1 AS end_timestamp \
        \ FROM perps_events WHERE chain_id = ? AND release_router = ?), \
        \activity_count AS ( \
        \ SELECT COUNT(*)::BIGINT AS row_count FROM perps_account_activity \
        \ WHERE chain_id = ? AND release_router = ? \
        \ AND activity_type IN ('Open', 'Close', 'Liquidated') \
        \ AND size_delta IS NOT NULL AND price IS NOT NULL) \
        \SELECT CASE \
        \   WHEN event_bounds.first_timestamp IS NULL THEN NULL \
        \   WHEN certified_release.first_timestamp IS NULL THEN event_bounds.first_timestamp \
        \   ELSE LEAST(certified_release.first_timestamp, event_bounds.first_timestamp) END, \
        \ event_bounds.end_timestamp, activity_count.row_count \
        \FROM event_bounds CROSS JOIN activity_count \
        \LEFT JOIN certified_release ON TRUE"
            ( pinnedDeploymentStart
            , perpsV2DeploymentBlock
            , arChainId runtime
            , arReleaseRouter runtime
            , defaultCandleMarketId
            , arChainId runtime
            , arReleaseRouter runtime
            , arChainId runtime
            , arReleaseRouter runtime
            , arChainId runtime
            , arReleaseRouter runtime
            )
  case rows of
    [(Just fromTimestamp, Just toTimestamp, rowCount)] ->
      pure $ Just $ SourceBounds (alignDown fromTimestamp 60) (alignUp toTimestamp 60) rowCount
    [(Nothing, Nothing, _)] -> pure Nothing
    _ -> failWith "Could not determine candle source bounds"

getCoverage
  :: Connection
  -> AdminRuntime
  -> RollupKind
  -> Integer
  -> IO (Maybe RollupCoverage)
getCoverage conn runtime kind interval =
  case kind of
    PriceRollup ->
      getRollupCoverage
        conn
        kind
        (Just defaultBasketSeriesId)
        Nothing
        Nothing
        interval
    VolumeRollup ->
      getRollupCoverage
        conn
        kind
        Nothing
        (Just $ arChainId runtime)
        (Just $ arReleaseRouter runtime)
        interval

emitCoverage
  :: Connection
  -> RollupKind
  -> Integer
  -> Maybe RollupCoverage
  -> Maybe Integer
  -> IO ()
emitCoverage conn kind interval coverage affected = do
  currentTimestamp <- databaseNow conn
  let finalizedThrough = coverage >>= rcFinalizedThrough
      -- A completed bucket is expected to trail wall-clock time by as much as
      -- its own interval. Report only excess lag so coarse intervals do not
      -- continuously trip the operational alarm. Missing coverage is emitted
      -- as JSON null and is diagnosed by the explicit complete/coverage fields.
      lagSeconds =
        (\finalized -> max 0 $ currentTimestamp - finalized - interval)
          <$> finalizedThrough
  logInfo
    "perps_candle_coverage"
    "Reported Perps candle rollup coverage"
    [ field "kind" $ rollupKindName kind
    , field "interval_seconds" interval
    , field "coverage_start" $ coverage >>= rcCoverageStart
    , field "coverage_end" $ coverage >>= rcCoverageEnd
    , field "finalized_through" finalizedThrough
    , field "generation" $ maybe 0 rcGeneration coverage
    , field "complete" $ maybe False rcComplete coverage
    , field "lag_seconds" lagSeconds
    , field "affected_base_buckets" affected
    ]

databaseNow :: Connection -> IO Integer
databaseNow conn = do
  rows <- query conn "SELECT EXTRACT(EPOCH FROM NOW())::BIGINT" () :: IO [Only Integer]
  case rows of
    [Only timestamp] -> pure timestamp
    _ -> failWith "Could not read the database clock"

newestFirstChunks :: Integer -> SourceBounds -> [(Integer, Integer)]
newestFirstChunks chunkSeconds SourceBounds {sbFrom, sbTo} = go sbTo
 where
  go cursor
    | cursor <= sbFrom = []
    | otherwise =
        let chunkFrom = max sbFrom $ cursor - chunkSeconds
         in (chunkFrom, cursor) : go chunkFrom

orderedChunks :: Integer -> BackfillRange -> [(Integer, Integer)]
orderedChunks chunkSeconds BackfillRange {brBounds, brOrder} =
  case brOrder of
    OldestFirst -> reverse $ newestFirstChunks chunkSeconds brBounds
    NewestFirst -> newestFirstChunks chunkSeconds brBounds

alignDown :: Integer -> Integer -> Integer
alignDown timestamp interval = timestamp - timestamp `mod` interval

alignUp :: Integer -> Integer -> Integer
alignUp timestamp interval =
  let remainder = timestamp `mod` interval
   in if remainder == 0 then timestamp else timestamp + interval - remainder

ceilingDiv :: Integer -> Integer -> Integer
ceilingDiv numerator denominator
  | numerator <= 0 = 0
  | otherwise = (numerator + denominator - 1) `div` denominator

rollupKindName :: RollupKind -> T.Text
rollupKindName = \case
  PriceRollup -> "price"
  VolumeRollup -> "volume"

configureSession :: Connection -> AdminOptions -> IO ()
configureSession conn AdminOptions {aoAction, aoStatementTimeoutMs, aoLockTimeoutMs} = do
  let statementTimeout = show aoStatementTimeoutMs <> "ms"
      lockTimeout = show aoLockTimeoutMs <> "ms"
  _ <- query conn "SELECT set_config('application_name', 'plether-candle-admin', false)" () :: IO [Only T.Text]
  _ <- query conn "SELECT set_config('statement_timeout', ?, false)" (Only statementTimeout) :: IO [Only T.Text]
  _ <- query conn "SELECT set_config('lock_timeout', ?, false)" (Only lockTimeout) :: IO [Only T.Text]
  when (aoAction == FinalizerProbe) $ do
    _ <-
      query conn
        "SELECT set_config('idle_in_transaction_session_timeout', '12000ms', false)"
        () :: IO [Only T.Text]
    pure ()
  pure ()

withAdminLock :: Connection -> Int -> IO a -> IO a
withAdminLock conn timeoutMs action = do
  acquired <- acquire 0
  if not acquired
    then failWith "Another plether-candle-admin mutation holds the advisory lock"
    else bracket_ (pure ()) release action
 where
  retryDelayMicros = 200_000
  maximumAttempts = max 1 $ (timeoutMs * 1_000 + retryDelayMicros - 1) `div` retryDelayMicros

  release = do
    _ <- query conn "SELECT pg_advisory_unlock(?)" (Only candleAdminLockId) :: IO [Only Bool]
    pure ()

  acquire attempt = do
    rows <- query conn "SELECT pg_try_advisory_lock(?)" (Only candleAdminLockId) :: IO [Only Bool]
    case rows of
      [Only True] -> pure True
      _
        | attempt + 1 >= maximumAttempts -> pure False
        | otherwise -> threadDelay retryDelayMicros >> acquire (attempt + 1)

candleAdminLockId :: Integer
candleAdminLockId = 4_278_619_031

requiresAdvisoryLock :: AdminAction -> Bool
requiresAdvisoryLock = \case
  Migrate -> True
  SetHistoryTarget -> True
  Backfill _ -> True
  Repair _ -> True
  Verify _ -> True
  FinalizerProbe -> True
  Estimate -> False
  Status -> False

requiresDualWriteMode :: AdminAction -> Bool
requiresDualWriteMode = \case
  Backfill _ -> True
  Repair _ -> True
  FinalizerProbe -> True
  _ -> False

reportsBackfillFailure :: AdminAction -> Bool
reportsBackfillFailure = \case
  Migrate -> False
  SetHistoryTarget -> False
  Backfill _ -> True
  Verify _ -> True
  Repair _ -> True
  FinalizerProbe -> False
  Estimate -> False
  Status -> False

parseAdminOptions :: [String] -> Either String AdminOptions
parseAdminOptions = \case
  [] -> Left "Missing command"
  ["--help"] -> Left ""
  ["help"] -> Left ""
  "estimate" : rest -> parseFlags (defaultOptions Estimate) rest >>= validateOptions
  "migrate" : rest -> parseFlags (defaultOptions Migrate) rest >>= validateOptions
  "set-history-target" : rest ->
    parseSetHistoryTargetFlags (defaultOptions SetHistoryTarget) rest >>= validateOptions
  "status" : rest -> parseFlags (defaultOptions Status) rest >>= validateOptions
  "verify" : rawScope : rest -> do
    scope <- parseScope rawScope
    parseFlags (defaultOptions $ Verify scope) rest >>= validateOptions
  "finalizer-probe" : rest ->
    parseFinalizerProbeFlags (defaultOptions FinalizerProbe) rest >>= validateOptions
  "backfill" : rawScope : rest -> do
    scope <- parseScope rawScope
    parseFlags (defaultOptions $ Backfill scope) rest >>= validateOptions
  "repair" : rawScope : rest -> do
    scope <- parseScope rawScope
    parseFlags (defaultOptions $ Repair scope) rest >>= validateOptions
  "backfill" : _ -> Left "backfill requires a scope: price, volume, or all"
  "verify" : _ -> Left "verify requires a scope: price, volume, or all"
  "repair" : _ -> Left "repair requires a scope: price, volume, or all"
  command : _ -> Left $ "Unknown command: " <> command

parseSetHistoryTargetFlags :: AdminOptions -> [String] -> Either String AdminOptions
parseSetHistoryTargetFlags options = \case
  [] -> Right options
  "--start-timestamp" : raw : rest -> do
    value <- parseBoundedInteger "--start-timestamp" 0 4_102_444_800 raw
    parseSetHistoryTargetFlags options {aoHistoryStartTimestamp = Just value} rest
  "--requested-by" : raw : rest ->
    parseSetHistoryTargetFlags options {aoRequestedBy = Just $ T.pack raw} rest
  "--request-reference" : raw : rest ->
    parseSetHistoryTargetFlags options {aoRequestReference = Just $ T.pack raw} rest
  "--statement-timeout-ms" : raw : rest -> do
    value <- parseBoundedInt "--statement-timeout-ms" 1_000 1_800_000 raw
    parseSetHistoryTargetFlags options {aoStatementTimeoutMs = value} rest
  "--lock-timeout-ms" : raw : rest -> do
    value <- parseBoundedInt "--lock-timeout-ms" 100 60_000 raw
    parseSetHistoryTargetFlags options {aoLockTimeoutMs = value} rest
  "--max-runtime-seconds" : raw : rest -> do
    value <- parseBoundedInt "--max-runtime-seconds" 60 21_600 raw
    parseSetHistoryTargetFlags options {aoMaxRuntimeSeconds = value} rest
  [flag] -> Left $ "Missing value for option: " <> flag
  flag : _ -> Left $ "Unknown set-history-target option: " <> flag

parseFinalizerProbeFlags :: AdminOptions -> [String] -> Either String AdminOptions
parseFinalizerProbeFlags options = \case
  [] -> Right options
  "--boundary" : raw : rest -> do
    value <- parseBoundedInteger "--boundary" 3_600 4_102_444_800 raw
    parseFinalizerProbeFlags options {aoBoundary = Just value} rest
  "--statement-timeout-ms" : raw : rest -> do
    value <- parseBoundedInt "--statement-timeout-ms" 1_000 10_000 raw
    parseFinalizerProbeFlags options {aoStatementTimeoutMs = value} rest
  "--lock-timeout-ms" : raw : rest -> do
    value <- parseBoundedInt "--lock-timeout-ms" 100 5_000 raw
    parseFinalizerProbeFlags options {aoLockTimeoutMs = value} rest
  "--max-runtime-seconds" : raw : rest -> do
    value <- parseBoundedInt "--max-runtime-seconds" 60 2_100 raw
    parseFinalizerProbeFlags options {aoMaxRuntimeSeconds = value} rest
  [flag] -> Left $ "Missing value for option: " <> flag
  flag : _ -> Left $ "Unknown finalizer-probe option: " <> flag

parseFlags :: AdminOptions -> [String] -> Either String AdminOptions
parseFlags options = \case
  [] -> Right options
  "--from" : raw : rest -> do
    value <- parseBoundedInteger "--from" 0 4_102_444_800 raw
    parseFlags options {aoFrom = Just value} rest
  "--to" : raw : rest -> do
    value <- parseBoundedInteger "--to" 1 4_102_444_800 raw
    parseFlags options {aoTo = Just value} rest
  "--chunk-seconds" : raw : rest -> do
    value <- parseBoundedInteger "--chunk-seconds" 3_600 604_800 raw
    parseFlags options {aoChunkSeconds = value} rest
  "--statement-timeout-ms" : raw : rest -> do
    value <- parseBoundedInt "--statement-timeout-ms" 1_000 1_800_000 raw
    parseFlags options {aoStatementTimeoutMs = value} rest
  "--lock-timeout-ms" : raw : rest -> do
    value <- parseBoundedInt "--lock-timeout-ms" 100 60_000 raw
    parseFlags options {aoLockTimeoutMs = value} rest
  "--throttle-ms" : raw : rest -> do
    value <- parseBoundedInt "--throttle-ms" 0 60_000 raw
    parseFlags options {aoThrottleMs = value} rest
  "--max-runtime-seconds" : raw : rest -> do
    value <- parseBoundedInt "--max-runtime-seconds" 60 21_600 raw
    parseFlags options {aoMaxRuntimeSeconds = value} rest
  [flag] -> Left $ "Missing value for option: " <> flag
  flag : _ -> Left $ "Unknown option: " <> flag

validateOptions :: AdminOptions -> Either String AdminOptions
validateOptions
  options@AdminOptions
    { aoAction
    , aoFrom
    , aoTo
    , aoBoundary
    , aoHistoryStartTimestamp
    , aoRequestedBy
    , aoRequestReference
    , aoChunkSeconds
    } = do
  whenEither
    (aoChunkSeconds `mod` 60 /= 0)
    "--chunk-seconds must align to a whole minute"
  case (aoFrom, aoTo) of
    (Just fromTimestamp, Just toTimestamp)
      | fromTimestamp >= toTimestamp ->
          Left "--from must be earlier than --to"
    _ -> Right ()
  case aoAction of
    SetHistoryTarget
      | isNothing aoHistoryStartTimestamp ->
          Left "set-history-target requires --start-timestamp"
      | maybe True (T.null . T.strip) aoRequestedBy ->
          Left "set-history-target requires a non-blank --requested-by"
      | maybe True (T.null . T.strip) aoRequestReference ->
          Left "set-history-target requires a non-blank --request-reference"
      | isJust aoFrom || isJust aoTo || isJust aoBoundary ->
          Left "set-history-target does not accept --from, --to, or --boundary"
      | otherwise -> Right ()
    Repair _
      | isNothing aoFrom || isNothing aoTo ->
          Left "repair requires both --from and --to"
      | any ((/= 0) . (`mod` maximum canonicalCandleIntervals)) [fromMaybe 1 aoFrom, fromMaybe 1 aoTo] ->
          Left "repair --from and --to must align to UTC day boundaries so every canonical parent interval is rebuilt"
    FinalizerProbe
      | maybe True ((/= 0) . (`mod` 3_600)) aoBoundary ->
          Left "finalizer-probe requires --boundary aligned to a UTC hour"
      | isJust aoFrom || isJust aoTo ->
          Left "finalizer-probe does not accept --from or --to"
      | otherwise -> Right ()
    _
      | isJust aoBoundary -> Left "--boundary is accepted only for finalizer-probe"
      | isJust aoHistoryStartTimestamp || isJust aoRequestedBy || isJust aoRequestReference ->
          Left "History-target options are accepted only for set-history-target"
    _ -> Right ()
  pure options

whenEither :: Bool -> String -> Either String ()
whenEither condition message = if condition then Left message else Right ()

parseScope :: String -> Either String RollupScope
parseScope = \case
  "price" -> Right PriceRollups
  "volume" -> Right VolumeRollups
  "all" -> Right AllRollups
  scope -> Left $ "Unknown rollup scope: " <> scope

parseBoundedInt :: String -> Int -> Int -> String -> Either String Int
parseBoundedInt label minValue maxValue raw = do
  value <- parseBoundedInteger label (fromIntegral minValue) (fromIntegral maxValue) raw
  pure $ fromIntegral value

parseBoundedInteger :: String -> Integer -> Integer -> String -> Either String Integer
parseBoundedInteger label minValue maxValue raw
  | null raw || not (all isDigit raw) = invalid
  | otherwise =
      case readMaybe raw of
        Just value | value >= minValue && value <= maxValue -> Right value
        _ -> invalid
 where
  invalid = Left $ label <> " must be an integer from " <> show minValue <> " through " <> show maxValue

requireEnv :: String -> IO String
requireEnv name = do
  value <- lookupEnv name
  case T.strip . T.pack <$> value of
    Just normalized | not $ T.null normalized -> pure $ T.unpack normalized
    _ -> failWith $ name <> " is required for plether-candle-admin"

requireIntegerEnv :: String -> IO Integer
requireIntegerEnv name = do
  raw <- requireEnv name
  case readMaybe raw of
    Just value | value > 0 -> pure value
    _ -> failWith $ name <> " must be a positive integer"

optionalIntegerEnv :: String -> Integer -> Integer -> Integer -> IO Integer
optionalIntegerEnv name defaultValue minValue maxValue = do
  raw <- lookupEnv name
  case raw of
    Nothing -> pure defaultValue
    Just value ->
      case readMaybe value of
        Just parsed | parsed >= minValue && parsed <= maxValue -> pure parsed
        _ ->
          failWith $
            name
              <> " must be an integer from "
              <> show minValue
              <> " through "
              <> show maxValue

actionName :: AdminAction -> T.Text
actionName = \case
  Estimate -> "estimate"
  Migrate -> "migrate"
  SetHistoryTarget -> "set_history_target"
  Backfill _ -> "backfill"
  Status -> "status"
  Verify _ -> "verify"
  Repair _ -> "repair"
  FinalizerProbe -> "finalizer_probe"

scopeNameForAction :: AdminAction -> T.Text
scopeNameForAction = \case
  Backfill scope -> scopeName scope
  Verify scope -> scopeName scope
  Repair scope -> scopeName scope
  FinalizerProbe -> "price"
  _ -> "none"

scopeName :: RollupScope -> T.Text
scopeName = \case
  PriceRollups -> "price"
  VolumeRollups -> "volume"
  AllRollups -> "all"

failWith :: String -> IO a
failWith message = ioError $ userError message

sanitizeException :: String -> SomeException -> T.Text
sanitizeException databaseUrl =
  T.take 500
    . T.replace (T.pack databaseUrl) "[redacted-database-url]"
    . T.replace "\n" " "
    . T.pack
    . displayException

usage :: String
usage =
  unlines
    [ "Usage:"
    , "  plether-candle-admin estimate [OPTIONS]"
    , "  plether-candle-admin migrate [OPTIONS]"
    , "  plether-candle-admin set-history-target --start-timestamp UNIX --requested-by TEXT --request-reference TEXT [OPTIONS]"
    , "  plether-candle-admin backfill price|volume|all [OPTIONS]"
    , "  plether-candle-admin status [OPTIONS]"
    , "  plether-candle-admin verify price|volume|all [OPTIONS]"
    , "  plether-candle-admin repair price|volume|all --from UNIX --to UNIX [OPTIONS]"
    , "  plether-candle-admin finalizer-probe --boundary UNIX [OPTIONS]"
    , ""
    , "Options:"
    , "  --from UNIX                  Inclusive source timestamp"
    , "  --to UNIX                    Exclusive source timestamp"
    , "  --boundary UNIX              Aligned UTC hour for the Sepolia finalizer probe"
    , "  --start-timestamp UNIX       Desired logical-market price-history start"
    , "  --requested-by TEXT          Identity recording the protected request"
    , "  --request-reference TEXT     Retry-stable protected workflow reference"
    , "  --chunk-seconds N            Transaction chunk size (default 86400)"
    , "  --statement-timeout-ms N     Per-statement timeout (default 1800000)"
    , "  --lock-timeout-ms N          Admin advisory-lock wait (default 5000)"
    , "  --throttle-ms N              Delay after committed chunks (default 250)"
    , "  --max-runtime-seconds N      Absolute process deadline (2100 for probe; otherwise 21600)"
    , ""
    , "Backfill, repair, and finalizer-probe require PERPS_CANDLE_WRITE_MODE=dual."
    , "set-history-target appends metadata only and does not publish candle coverage."
    , "Backfill and repair read only existing PostgreSQL data and never contact Pyth;"
    , "the Sepolia-only finalizer probe performs SELECTs and takes one transaction lock."
    ]
