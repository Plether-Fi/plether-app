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
  ( IsolationLevel (RepeatableRead)
  , ReadWriteMode (ReadOnly, ReadWrite)
  , TransactionMode (..)
  , withTransactionMode
  )
import Plether.Database (newDbPool, withDb)
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
  , getRollupCoverage
  , markRollupCoverageIncomplete
  , upsertRollupCoverage
  )
import Plether.Logging
  ( field
  , logError
  , logInfo
  )
import System.Environment (getArgs, lookupEnv)
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
  | Backfill RollupScope
  | Status
  | Verify
  | Repair RollupScope
  deriving (Eq, Show)

data AdminOptions = AdminOptions
  { aoAction :: AdminAction
  , aoFrom :: Maybe Integer
  , aoTo :: Maybe Integer
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

data RepairMaintenance = RepairMaintenance
  { rmKind :: RollupKind
  , rmPublicationCoverage :: [RollupCoverage]
  , rmGeneration :: Integer
  , rmRepairFrom :: Integer
  , rmRepairTo :: Integer
  }
  deriving (Eq, Show)

defaultOptions :: AdminAction -> AdminOptions
defaultOptions action =
  AdminOptions
    { aoAction = action
    , aoFrom = Nothing
    , aoTo = Nothing
    , aoChunkSeconds = 86_400
    , aoStatementTimeoutMs = 1_800_000
    , aoLockTimeoutMs = 5_000
    , aoThrottleMs = 250
    , aoMaxRuntimeSeconds = 21_600
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
      when (requiresDualWriteMode $ aoAction options) $ do
        writeMode <- T.toLower . T.strip . T.pack <$> requireEnv "PERPS_CANDLE_WRITE_MODE"
        unless (writeMode == "dual") $
          failWith "backfill and repair require PERPS_CANDLE_WRITE_MODE=dual"
      databaseUrl <- requireEnv "DATABASE_URL"
      chainId <- requireIntegerEnv "PERPS_CHAIN_ID"
      releaseRouter <- T.toLower . T.strip . T.pack <$> requireEnv "PERPS_ORDER_ROUTER"
      latenessSeconds <- optionalIntegerEnv "PERPS_CANDLE_LATENESS_SECONDS" 120 0 86_400
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
          exitFailure

runAdmin :: Connection -> AdminRuntime -> AdminOptions -> IO ()
runAdmin conn runtime options@AdminOptions {aoAction} =
  case aoAction of
    Estimate -> runEstimate conn runtime options
    Migrate -> do
      ensureCandleSchema conn
      logInfo
        "perps_candle_migration_complete"
        "Perps candle schema migration completed"
        [field "derivation_version" candleDerivationVersion]
    Backfill scope ->
      forM_ (rollupKinds scope) $ \kind ->
        runBackfill conn runtime options kind False
    Status -> runStatus conn runtime
    Verify -> withVerificationSnapshot conn $ do
      verified <- and <$> mapM (verifyKind conn runtime options) [PriceRollup, VolumeRollup]
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
runStatus conn runtime =
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
  bounds <- resolveBounds conn runtime options kind
  case bounds of
    Nothing ->
      logInfo
        "perps_candle_backfill_complete"
        "No source rows matched the requested candle backfill"
        [field "kind" $ rollupKindName kind, field "affected_base_buckets" (0 :: Integer)]
    Just availableBounds -> do
      unless isRepair $ prepareBackfillCoverage conn runtime kind availableBounds
      ranges <-
        if isRepair
          then pure [availableBounds]
          else resumeRanges conn runtime kind availableBounds
      case ranges of
        [] -> do
          coverage <- getCoverage conn runtime kind 60
          emitCoverage conn kind 60 coverage $ Just (0 :: Integer)
          logInfo
            "perps_candle_backfill_complete"
            "Requested candle range is already covered"
            [field "kind" $ rollupKindName kind, field "affected_base_buckets" (0 :: Integer)]
        _ -> do
          let chunks = concatMap (newestFirstChunks $ aoChunkSeconds options) ranges
          affectedCounts <- forM chunks $ \(chunkFrom, chunkTo) -> do
            affected <-
              if isRepair
                then backfillChunk conn runtime kind chunkFrom chunkTo False
                else backfillChunk conn runtime kind chunkFrom chunkTo True
            coverage <- getCoverage conn runtime kind 60
            emitCoverage conn kind 60 coverage $ Just affected
            when (aoThrottleMs options > 0) $
              threadDelay $ aoThrottleMs options * 1_000
            pure affected
          let processedFrom = minimum $ map sbFrom ranges
              processedTo = maximum $ map sbTo ranges
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
  let (mergedFrom, mergedTo) = mergeCoverageRange currentCoverage fromTimestamp toTimestamp
  forM_ (zip canonicalCandleIntervals coverages) $ \(interval, existing) -> do
    let intervalFrom = alignUp mergedFrom interval
        intervalTo = alignDown mergedTo interval
    when (intervalFrom < intervalTo) $ do
      finalizeCoveredRows conn runtime kind interval intervalFrom intervalTo
      upsertRollupCoverage conn $
        coverageRecord
          runtime
          kind
          interval
          intervalFrom
          intervalTo
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
  -> Maybe Integer
  -> Maybe RollupCoverage
  -> RollupCoverage
coverageRecord runtime kind interval fromTimestamp toTimestamp generation existing =
  RollupCoverage
    { rcKind = kind
    , rcSeriesId = case kind of PriceRollup -> Just defaultBasketSeriesId; VolumeRollup -> Nothing
    , rcChainId = case kind of PriceRollup -> Nothing; VolumeRollup -> Just $ arChainId runtime
    , rcReleaseRouter = case kind of PriceRollup -> Nothing; VolumeRollup -> Just $ arReleaseRouter runtime
    , rcIntervalSeconds = interval
    , rcCoverageStart = Just fromTimestamp
    , rcCoverageEnd = Just toTimestamp
    , rcFinalizedThrough = Just toTimestamp
    , rcGeneration = fromMaybe (maybe 1 (max 1 . rcGeneration) existing) generation
    , rcComplete = True
    , rcDerivationVersion = candleDerivationVersion
    , rcLastError = Nothing
    , rcMaintenanceFrom = Nothing
    , rcMaintenanceTo = Nothing
    }

mergeCoverageRange :: Maybe RollupCoverage -> Integer -> Integer -> (Integer, Integer)
mergeCoverageRange existing fromTimestamp toTimestamp =
  case trustedCoverageRange existing of
    Just (oldFrom, oldTo)
      | oldFrom <= toTimestamp && oldTo >= fromTimestamp ->
          (min oldFrom fromTimestamp, max oldTo toTimestamp)
    _ -> (fromTimestamp, toTimestamp)

resumeRanges
  :: Connection
  -> AdminRuntime
  -> RollupKind
  -> SourceBounds
  -> IO [SourceBounds]
resumeRanges conn runtime kind bounds@SourceBounds {sbFrom, sbTo} = do
  coverage <- getCoverage conn runtime kind 60
  pure $ case trustedCoverageRange coverage of
    Just (coveredFrom, coveredTo)
      | coveredFrom <= sbTo && coveredTo >= sbFrom ->
            [ bounds {sbFrom = max sbFrom coveredTo}
            | coveredTo < sbTo
            ]
              <> [ bounds {sbTo = min sbTo coveredFrom}
                 | sbFrom < coveredFrom
                 ]
    _ -> [bounds]

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
resolveBounds conn runtime AdminOptions {aoAction, aoFrom, aoTo} kind = do
  source <- sourceBounds conn runtime kind
  currentTimestamp <- databaseNow conn
  let finalizedCutoff = alignDown (currentTimestamp - arLatenessSeconds runtime) 60
  pure $ case (aoAction, source, aoFrom, aoTo) of
    (Repair _, Just _, Just requestedFrom, Just requestedTo) ->
      Just $ SourceBounds (alignDown requestedFrom 60) (alignUp requestedTo 60) 0
    (Repair _, Nothing, _, _) -> Nothing
    (_, Nothing, Nothing, Nothing) -> Nothing
    (_, Nothing, _, _) -> Nothing
    (_, Just bounds, requestedFrom, requestedTo) ->
      let fromTimestamp = alignDown (max (sbFrom bounds) $ fromMaybe (sbFrom bounds) requestedFrom) 60
          requestedEnd = min (sbTo bounds) $ fromMaybe (sbTo bounds) requestedTo
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
      query
        conn
        "WITH event_bounds AS ( \
        \ SELECT MIN(timestamp) AS first_timestamp, MAX(timestamp) + 1 AS end_timestamp \
        \ FROM perps_events WHERE chain_id = ? AND release_router = ?), \
        \activity_count AS ( \
        \ SELECT COUNT(*)::BIGINT AS row_count FROM perps_account_activity \
        \ WHERE chain_id = ? AND release_router = ? \
        \ AND activity_type IN ('Open', 'Close', 'Liquidated') \
        \ AND size_delta IS NOT NULL AND price IS NOT NULL) \
        \SELECT first_timestamp, end_timestamp, row_count \
        \FROM event_bounds CROSS JOIN activity_count"
        ( arChainId runtime
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
configureSession conn AdminOptions {aoStatementTimeoutMs, aoLockTimeoutMs} = do
  let statementTimeout = show aoStatementTimeoutMs <> "ms"
      lockTimeout = show aoLockTimeoutMs <> "ms"
  _ <- query conn "SELECT set_config('application_name', 'plether-candle-admin', false)" () :: IO [Only T.Text]
  _ <- query conn "SELECT set_config('statement_timeout', ?, false)" (Only statementTimeout) :: IO [Only T.Text]
  _ <- query conn "SELECT set_config('lock_timeout', ?, false)" (Only lockTimeout) :: IO [Only T.Text]
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
  Backfill _ -> True
  Repair _ -> True
  Verify -> True
  Estimate -> False
  Status -> False

requiresDualWriteMode :: AdminAction -> Bool
requiresDualWriteMode = \case
  Backfill _ -> True
  Repair _ -> True
  _ -> False

reportsBackfillFailure :: AdminAction -> Bool
reportsBackfillFailure = \case
  Migrate -> True
  Backfill _ -> True
  Verify -> True
  Repair _ -> True
  Estimate -> False
  Status -> False

parseAdminOptions :: [String] -> Either String AdminOptions
parseAdminOptions = \case
  [] -> Left "Missing command"
  ["--help"] -> Left ""
  ["help"] -> Left ""
  "estimate" : rest -> parseFlags (defaultOptions Estimate) rest >>= validateOptions
  "migrate" : rest -> parseFlags (defaultOptions Migrate) rest >>= validateOptions
  "status" : rest -> parseFlags (defaultOptions Status) rest >>= validateOptions
  "verify" : rest -> parseFlags (defaultOptions Verify) rest >>= validateOptions
  "backfill" : rawScope : rest -> do
    scope <- parseScope rawScope
    parseFlags (defaultOptions $ Backfill scope) rest >>= validateOptions
  "repair" : rawScope : rest -> do
    scope <- parseScope rawScope
    parseFlags (defaultOptions $ Repair scope) rest >>= validateOptions
  "backfill" : _ -> Left "backfill requires a scope: price, volume, or all"
  "repair" : _ -> Left "repair requires a scope: price, volume, or all"
  command : _ -> Left $ "Unknown command: " <> command

parseFlags :: AdminOptions -> [String] -> Either String AdminOptions
parseFlags options = \case
  [] -> Right options
  "--from" : raw : rest -> do
    value <- parseBoundedInteger "--from" 1 4_102_444_800 raw
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
validateOptions options@AdminOptions {aoAction, aoFrom, aoTo, aoChunkSeconds} = do
  whenEither
    (aoChunkSeconds `mod` 60 /= 0)
    "--chunk-seconds must align to a whole minute"
  case (aoFrom, aoTo) of
    (Just fromTimestamp, Just toTimestamp)
      | fromTimestamp >= toTimestamp ->
          Left "--from must be earlier than --to"
    _ -> Right ()
  case aoAction of
    Repair _
      | isNothing aoFrom || isNothing aoTo ->
          Left "repair requires both --from and --to"
      | any ((/= 0) . (`mod` maximum canonicalCandleIntervals)) [fromMaybe 1 aoFrom, fromMaybe 1 aoTo] ->
          Left "repair --from and --to must align to UTC day boundaries so every canonical parent interval is rebuilt"
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
  Backfill _ -> "backfill"
  Status -> "status"
  Verify -> "verify"
  Repair _ -> "repair"

scopeNameForAction :: AdminAction -> T.Text
scopeNameForAction = \case
  Backfill scope -> scopeName scope
  Repair scope -> scopeName scope
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
    , "  plether-candle-admin backfill price|volume|all [OPTIONS]"
    , "  plether-candle-admin status [OPTIONS]"
    , "  plether-candle-admin verify [OPTIONS]"
    , "  plether-candle-admin repair price|volume|all --from UNIX --to UNIX [OPTIONS]"
    , ""
    , "Options:"
    , "  --from UNIX                  Inclusive source timestamp"
    , "  --to UNIX                    Exclusive source timestamp"
    , "  --chunk-seconds N            Transaction chunk size (default 86400)"
    , "  --statement-timeout-ms N     Per-statement timeout (default 1800000)"
    , "  --lock-timeout-ms N          Admin advisory-lock wait (default 5000)"
    , "  --throttle-ms N              Delay after committed chunks (default 250)"
    , "  --max-runtime-seconds N      Absolute process deadline (default 21600)"
    , ""
    , "Backfill and repair require PERPS_CANDLE_WRITE_MODE=dual, read only existing"
    , "PostgreSQL data, and never contact Pyth."
    ]
