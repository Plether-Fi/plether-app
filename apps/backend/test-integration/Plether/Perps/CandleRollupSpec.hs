module Plether.Perps.CandleRollupSpec
  ( candleRollupSpec
  ) where

import Control.Concurrent
  ( forkFinally
  , forkIO
  , newEmptyMVar
  , putMVar
  , takeMVar
  , threadDelay
  , tryPutMVar
  )
import Control.Exception (IOException, SomeException, bracket, displayException, finally, try)
import Control.Monad (forM_, void, when)
import Data.Aeson (Value, object, withObject, (.:), (.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Char8 as BS8
import Data.Either (isLeft, isRight)
import Data.Pool (destroyAllResources)
import Data.Text (Text)
import qualified Data.Text as Text
import Database.PostgreSQL.Simple
  ( Connection
  , Only (..)
  , SqlError (..)
  , execute
  , execute_
  , query
  , query_
  , withTransaction
  )
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import Network.HTTP.Types.Header (RequestHeaders)
import Network.HTTP.Types.Status (status200, status503)
import Network.Wai (requestHeaders)
import Network.Wai.Test
  ( SResponse (..)
  , defaultRequest
  , request
  , runSession
  , setPath
  )
import Plether.Api (handleBasketCurrentCandleAt)
import Plether.Cache
  ( AppCache (..)
  , newAppCache
  , newConcurrentSingleFlightCache
  )
import Plether.Config
  ( Config (..)
  , LpSettlementMode (..)
  , PerpsCandleReadMode (PerpsCandleReadsRollup)
  , PerpsCandleWriteMode (PerpsCandleWritesDual)
  )
import Plether.Database (DbPool, newDbPool, withDb)
import Plether.Insights.Competition
  ( CompetitionReleaseManifest (..)
  , july2026Competition
  )
import Plether.Database.CandleHistory
  ( CandleHistoryIngestionProgress (..)
  , CandleHistorySelection (..)
  , appendCandleHistorySelection
  , candleHistorySelectionIsAbsent
  , candleHistorySelectionIsLatest
  , completeCandleHistoryIngestionWindow
  , defaultCandleMarketId
  , ensureCandleMarketIdentity
  , getCandleHistoryIngestionProgress
  , getLatestCandleHistoryIngestionProgress
  , getLatestPublishedCandleHistoryIngestion
  , getLatestCandleHistorySelection
  , initializeCandleHistoryIngestionProgress
  , publishCandleHistoryIngestion
  , recordCandleHistoryIngestionError
  , validateCandleHistoryIngestionCompletion
  )
import Plether.Database.Candles
  ( BasketCandleRow (..)
  , BasketObservationInput (..)
  , CandleCurrent (..)
  , CandlePage (..)
  , CandleRange (..)
  , RollupCoverage (..)
  , PriceGapRecoveryResult (..)
  , RollupKind (..)
  , advanceBasketPriceCoverage
  , advanceMarketVolumeCoverage
  , backfillLegacyBasketSnapshots
  , backfillMarketVolume
  , bumpRollupDatasetGeneration
  , canonicalCandleIntervals
  , defaultBasketSeriesId
  , ensureCandleSchema
  , ensureCurrentBasketDefinition
  , getActiveBasketDefinitionIdentity
  , getBasketCandlePage
  , getBasketCandlePageSnapshot
  , getBasketCurrentCandleSnapshot
  , getBasketCandleRange
  , getCurrentBasketCandle
  , getRollupCoverage
  , invalidateMarketVolumeFromBlock
  , lockBasketPriceDataset
  , markRollupCoverageIncomplete
  , recomputeBasketCandleHierarchy
  , recoverBasketPriceCoverageGap
  , recomputeMarketVolumeHierarchy
  , recomputeMarketVolumeHierarchyBatch
  , beginRollupMaintenance
  , upsertBasketObservation
  , upsertRollupCoverage
  , vacuumCandlePageTables
  )
import Plether.Database.Schema
  ( assertPerpsReplayEventExact
  , deletePerpsHistoryFromBlock
  , ensureBasketSnapshotSchema
  , ensurePerpsHistorySchema
  , insertBasketSnapshotWithSource
  , insertBasketSnapshotsWithSource
  , insertPerpsActivity
  , insertPerpsEvent
  , setPerpsIndexerState
  )
import Plether.Pyth.History (filterTradingViewHistorySamplesForPersistence)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.Process (CreateProcess (..), proc, readCreateProcessWithExitCode)
import System.Timeout (timeout)
import Database.PostgreSQL.Simple.Transaction
  ( IsolationLevel (ReadCommitted, RepeatableRead)
  , ReadWriteMode (ReadOnly)
  , TransactionMode (..)
  , withTransactionMode
  )
import Test.Hspec
  ( Spec
  , anyException
  , describe
  , expectationFailure
  , it
  , shouldBe
  , shouldContain
  , shouldReturn
  , shouldSatisfy
  , shouldThrow
  )
import Web.Scotty (get, scottyApp)

candleRollupSpec :: Text -> Spec
candleRollupSpec databaseUrl =
  describe "Perps candle rollup storage" $ do
    it "installs the additive schema idempotently" $
      withCandleDatabase databaseUrl $ \pool ->
        withDb pool $ \connection -> do
          ensureCandleSchema connection
          before <- candleRelationOids connection
          ensureCandleSchema connection
          after <- candleRelationOids connection
          after `shouldBe` before
          length after `shouldBe` 10
          candleActivityIndexValidity connection `shouldReturn` [Only True]
          candleEventIndexValidity connection `shouldReturn` [Only True]
          candleActivityReorgIndexValidity connection `shouldReturn` [Only True]
          candleEventReorgIndexValidity connection `shouldReturn` [Only True]
          canonicalCandleIntervals
            `shouldBe` [60, 180, 300, 900, 1800, 3600, 86_400]

    it "retains arbitrary earlier and later history targets as immutable revisions" $
      withCandleDatabase databaseUrl $ \pool ->
        withDb pool $ \connection ->
          withRollback connection $ do
            seedCandleHistoryMarket connection
            insertHistoryTarget connection 1 1_700_000_001 "history-target-1"
            insertHistoryTarget connection 2 946_684_801 "history-target-2"
            insertHistoryTarget connection 3 1_800_000_001 "history-target-3"
            targets <-
              query_
                connection
                "SELECT revision, requested_start_timestamp \
                \FROM perps_candle_history_targets \
                \WHERE market_id = 'dxy-history-integration' ORDER BY revision"
                :: IO [(Integer, Integer)]
            targets
              `shouldBe`
                [ (1, 1_700_000_001)
                , (2, 946_684_801)
                , (3, 1_800_000_001)
                ]

    it "rejects history target mutation and revision gaps" $
      withCandleDatabase databaseUrl $ \pool ->
        withDb pool $ \connection -> do
          withRollback connection $ do
            seedCandleHistoryMarket connection
            insertHistoryTarget connection 1 1_700_000_001 "history-target-1"
            execute_
              connection
              "UPDATE perps_candle_history_targets \
              \SET requested_start_timestamp = 946684801 \
              \WHERE market_id = 'dxy-history-integration' AND revision = 1"
              `shouldThrow` sqlErrorContaining "candle history targets are immutable"
          withRollback connection $ do
            seedCandleHistoryMarket connection
            insertHistoryTarget connection 2 946_684_801 "history-target-gap"
              `shouldThrow` sqlErrorContaining "must append the next revision"

    it "appends auditable history targets idempotently without requiring release rows" $
      withCandleDatabase databaseUrl $ \pool ->
        withDb pool $ \connection -> do
          ensureCurrentBasketDefinition connection testSeries
          ensureCandleMarketIdentity
            connection defaultCandleMarketId testChainId testSeries
          -- Repeating the exact identity is safe, while a conflicting runtime
          -- binding fails instead of silently selecting another market.
          ensureCandleMarketIdentity
            connection defaultCandleMarketId testChainId testSeries
          ensureCandleMarketIdentity
            connection defaultCandleMarketId (testChainId + 1) testSeries
            `shouldThrow` anyException
          let appendTarget requestedStart requestReference =
                appendCandleHistorySelection
                  connection defaultCandleMarketId testChainId testSeries
                  requestedStart "integration-test" requestReference
              readLatest =
                getLatestCandleHistorySelection
                  connection defaultCandleMarketId testChainId testSeries

          readLatest `shouldReturn` Nothing
          ( withTransaction connection $
              candleHistorySelectionIsAbsent
                connection defaultCandleMarketId testChainId testSeries
            )
            `shouldReturn` True

          (firstSelection, firstInserted) <-
            appendTarget 946_684_801 "history-append-1"
          firstInserted `shouldBe` True
          chsRevision firstSelection `shouldBe` 1
          ( withTransaction connection $
              candleHistorySelectionIsAbsent
                connection defaultCandleMarketId testChainId testSeries
            )
            `shouldReturn` False

          (retriedSelection, retryInserted) <-
            appendTarget 946_684_801 "history-append-1"
          retriedSelection `shouldBe` firstSelection
          retryInserted `shouldBe` False

          appendTarget 946_684_802 "history-append-1"
            `shouldThrow` anyException

          (latestSelection, latestInserted) <-
            appendTarget 1_700_000_001 "history-append-2"
          latestInserted `shouldBe` True
          chsRevision latestSelection `shouldBe` 2
          readLatest `shouldReturn` Just latestSelection

          marketRows <-
            query
              connection
              "SELECT chain_id, price_series_id FROM perps_candle_markets \
              \WHERE market_id = ?"
              (Only defaultCandleMarketId) :: IO [(Integer, Text)]
          marketRows `shouldBe` [(testChainId, testSeries)]
          releaseRows <-
            query
              connection
              "SELECT COUNT(*)::BIGINT FROM perps_market_release_epochs \
              \WHERE market_id = ?"
              (Only defaultCandleMarketId) :: IO [Only Integer]
          releaseRows `shouldBe` [Only 0]

    it "advances only contiguous proved ingestion windows and exposes the exact latest revision" $
      withCandleDatabase databaseUrl $ \pool ->
        withDb pool $ \connection -> do
          ensureCurrentBasketDefinition connection testSeries
          ensureCandleMarketIdentity
            connection defaultCandleMarketId testChainId testSeries
          (firstSelection, _) <-
            withTransaction connection $
              appendCandleHistorySelection
                connection defaultCandleMarketId testChainId testSeries
                1_700_000_001 "integration-test" "history-progress-1"
          let startTimestamp = 1_700_000_040
              middleTimestamp = startTimestamp + 120
              endTimestampExclusive = startTimestamp + 180
          initial <-
            withTransaction connection $ do
              isLatest <-
                candleHistorySelectionIsLatest
                  connection testChainId testSeries firstSelection
              isLatest `shouldBe` True
              initializeCandleHistoryIngestionProgress
                connection firstSelection startTimestamp endTimestampExclusive 60
          chipNextTimestamp initial `shouldBe` startTimestamp
          chipComplete initial `shouldBe` False

          withTransaction connection $
            recordCandleHistoryIngestionError
              connection firstSelection startTimestamp "partial component timestamp set"
          errored <-
            getCandleHistoryIngestionProgress
              connection defaultCandleMarketId (chsRevision firstSelection)
          chipLastError <$> errored
            `shouldBe` Just (Just "partial component timestamp set")

          firstWindow <-
            withTransaction connection $
              completeCandleHistoryIngestionWindow
                connection firstSelection startTimestamp middleTimestamp 0
          chipNextTimestamp firstWindow `shouldBe` middleTimestamp
          chipComplete firstWindow `shouldBe` False
          chipLastError firstWindow `shouldBe` Nothing

          completed <-
            withTransaction connection $
              completeCandleHistoryIngestionWindow
                connection firstSelection middleTimestamp endTimestampExclusive 1
          validateCandleHistoryIngestionCompletion firstSelection completed
            `shouldBe` Right ()
          completedWindows <-
            query
              connection
              "SELECT window_start, window_end_exclusive, sample_count \
              \FROM perps_candle_history_ingestion_windows \
              \WHERE market_id = ? AND target_revision = ? ORDER BY window_start"
              (defaultCandleMarketId, chsRevision firstSelection)
              :: IO [(Integer, Integer, Integer)]
          completedWindows
            `shouldBe`
              [ (startTimestamp, middleTimestamp, 0)
              , (middleTimestamp, endTimestampExclusive, 1)
              ]
          getLatestCandleHistoryIngestionProgress
            connection defaultCandleMarketId testChainId testSeries
            `shouldReturn` Just (firstSelection, Just completed)
          getLatestPublishedCandleHistoryIngestion
            connection defaultCandleMarketId testChainId testSeries
            `shouldReturn` Nothing
          published <-
            withTransaction connection $
              publishCandleHistoryIngestion connection firstSelection 7
          chipPublishedGeneration published `shouldBe` Just 7
          getLatestPublishedCandleHistoryIngestion
            connection defaultCandleMarketId testChainId testSeries
            `shouldReturn` Just (firstSelection, published)
          execute
            connection
            "UPDATE perps_candle_history_ingestions SET published_generation = 8 \
            \WHERE market_id = ? AND target_revision = ?"
            (defaultCandleMarketId, chsRevision firstSelection)
            `shouldThrow` sqlErrorContaining "candle history publication is immutable"

          (replacement, _) <-
            withTransaction connection $
              appendCandleHistorySelection
                connection defaultCandleMarketId testChainId testSeries
                946_684_801 "integration-test" "history-progress-2"
          ( withTransaction connection $
              candleHistorySelectionIsLatest
                connection testChainId testSeries firstSelection
            )
            `shouldReturn` False
          getLatestCandleHistoryIngestionProgress
            connection defaultCandleMarketId testChainId testSeries
            `shouldReturn` Just (replacement, Nothing)
          getLatestPublishedCandleHistoryIngestion
            connection defaultCandleMarketId testChainId testSeries
            `shouldReturn` Just (firstSelection, published)

    it "serializes a fetched legacy write with history-target selection" $
      withCandleDatabase databaseUrl $ \pool ->
        withDb pool $ \legacyConnection -> do
          ensureCurrentBasketDefinition legacyConnection testSeries
          ensureCandleMarketIdentity
            legacyConnection defaultCandleMarketId testChainId testSeries
          targetStarted <- newEmptyMVar
          targetDone <- newEmptyMVar
          let legacyTimestamp = baseTime
              legacyPrice = 99_000_000
          withTransaction legacyConnection $ do
            allowed <-
              candleHistorySelectionIsAbsent
                legacyConnection defaultCandleMarketId testChainId testSeries
            allowed `shouldBe` True
            _ <- forkIO $ do
              putMVar targetStarted ()
              result <- try @SomeException $
                withDb pool $ \targetConnection -> withTransaction targetConnection $
                  appendCandleHistorySelection
                    targetConnection defaultCandleMarketId testChainId testSeries
                    (legacyTimestamp - 86_400) "integration-test" "legacy-race-target"
              putMVar targetDone result
            takeMVar targetStarted
            -- Target selection takes the same market-row lock and therefore
            -- cannot commit until this already-authorized legacy write does.
            threadDelay 100_000
            blockedTargetResult <- timeout 100_000 (takeMVar targetDone)
            case blockedTargetResult of
              Nothing -> pure ()
              Just _ -> expectationFailure "History-target selection did not wait for the legacy write"
            insertBasketSnapshotWithSource
              legacyConnection legacyTimestamp 60 legacyPrice componentPayload legacySnapshotSource

          targetResult <- timeout 5_000_000 $ takeMVar targetDone
          case targetResult of
            Just (Right (_, True)) -> pure ()
            Just (Right _) -> expectationFailure "History-target race insert was not new"
            Just (Left err) -> expectationFailure $ "History-target race failed: " <> displayException err
            Nothing -> expectationFailure "History-target race remained blocked after legacy commit"

          persistedAfterSelection <- withTransaction legacyConnection $ do
            allowed <-
              candleHistorySelectionIsAbsent
                legacyConnection defaultCandleMarketId testChainId testSeries
            when allowed $
              insertBasketSnapshotWithSource
                legacyConnection legacyTimestamp 60 (legacyPrice + 1) componentPayload targetHistorySnapshotSource
            pure allowed
          persistedAfterSelection `shouldBe` False
          query
            legacyConnection
            "SELECT basket_price, source FROM perps_basket_snapshots \
            \WHERE timestamp = ? AND interval_seconds = 60"
            (Only legacyTimestamp)
            `shouldReturn` [(legacyPrice, legacySnapshotSource)]

    it "does not change published raw inputs while bounded repair disables coverage" $
      withCandleAdminDatabase databaseUrl $ \pool ->
        withDb pool $ \connection -> do
          ensureCurrentBasketDefinition connection testSeries
          let coverageStart = baseTime
              coverageEnd = baseTime + 86_400
              prefixTimestamp = coverageStart - 60
              publishedPrice = 100_000_000
              targetPrice = 101_000_000
          insertBasketSnapshotWithSource
            connection coverageStart 60 publishedPrice componentPayload legacySnapshotSource
          _ <-
            backfillLegacyBasketSnapshots
              connection defaultBasketSeriesId coverageStart coverageEnd
          forM_ canonicalCandleIntervals $ \interval ->
            upsertRollupCoverage connection $
              RollupCoverage
                { rcKind = PriceRollup
                , rcSeriesId = Just defaultBasketSeriesId
                , rcChainId = Nothing
                , rcReleaseRouter = Nothing
                , rcIntervalSeconds = interval
                , rcCoverageStart = Just coverageStart
                , rcCoverageEnd = Just coverageEnd
                , rcFinalizedThrough = Just coverageEnd
                , rcGeneration = 3
                , rcComplete = True
                , rcDerivationVersion = "v1"
                , rcLastError = Nothing
                , rcMaintenanceFrom = Nothing
                , rcMaintenanceTo = Nothing
                }
          _ <-
            beginRollupMaintenance
              connection
              PriceRollup
              (Just defaultBasketSeriesId)
              Nothing
              Nothing
              coverageStart
              coverageEnd
          coverage <-
            getRollupCoverage
              connection PriceRollup (Just defaultBasketSeriesId) Nothing Nothing 60
          fmap rcComplete coverage `shouldBe` Just False
          fmap rcLastError coverage `shouldBe` Just (Just "bounded_admin_repair")
          let fetchedTargetSamples =
                [ (prefixTimestamp, targetPrice, componentPayload)
                , (coverageStart, targetPrice, componentPayload)
                , (coverageEnd, targetPrice, componentPayload)
                ]
          safeTargetSamples <-
            either (fail . Text.unpack) pure $
              filterTradingViewHistorySamplesForPersistence
                coverage
                fetchedTargetSamples
          insertBasketSnapshotsWithSource
            connection
            [ (timestamp, 60, price, components)
            | (timestamp, price, components) <- safeTargetSamples
            ]
            targetHistorySnapshotSource

          storedSnapshots <-
            query
              connection
              "SELECT timestamp, basket_price, source FROM perps_basket_snapshots \
              \WHERE timestamp IN (?, ?, ?) ORDER BY timestamp"
              (prefixTimestamp, coverageStart, coverageEnd)
              :: IO [(Integer, Integer, Text)]
          storedSnapshots
            `shouldBe`
              [ (prefixTimestamp, targetPrice, targetHistorySnapshotSource)
              , (coverageStart, publishedPrice, legacySnapshotSource)
              ]

          -- The same source reconstruction CandleAdmin verifies remains
          -- unchanged because target inputs never enter the protected overlap.
          _ <-
            backfillLegacyBasketSnapshots
              connection defaultBasketSeriesId coverageStart coverageEnd
          reconstructed <-
            query
              connection
              "SELECT raw_close_price, sample_count FROM perps_basket_candles \
              \WHERE series_id = ? AND interval_seconds = 60 AND bucket_start = ?"
              (defaultBasketSeriesId, coverageStart) :: IO [(Integer, Int)]
          reconstructed `shouldBe` [(publishedPrice, 1)]

    it "keeps logical markets and release evidence immutable and ordered" $
      withCandleDatabase databaseUrl $ \pool ->
        withDb pool $ \connection -> do
          withRollback connection $ do
            seedCandleHistoryMarket connection
            execute_
              connection
              "UPDATE perps_candle_markets SET chain_id = 1 \
              \WHERE market_id = 'dxy-history-integration'"
              `shouldThrow` sqlErrorContaining "candle market identity is immutable"
          withRollback connection $ do
            seedCandleHistoryMarket connection
            insertHistoryGenesis connection
            insertHistorySuccessor connection
            releases <-
              query_
                connection
                "SELECT release_revision, activation_block, activation_timestamp \
                \FROM perps_market_release_epochs \
                \WHERE market_id = 'dxy-history-integration' \
                \ORDER BY release_revision"
                :: IO [(Integer, Integer, Integer)]
            releases `shouldBe` [(1, 100, 1_000), (2, 200, 2_000)]
            execute_
              connection
              "UPDATE perps_market_release_epochs SET activation_block = 201 \
              \WHERE market_id = 'dxy-history-integration' AND release_revision = 2"
              `shouldThrow` sqlErrorContaining "market release epochs are immutable"
          withRollback connection $ do
            seedCandleHistoryMarket connection
            insertHistorySuccessor connection
              `shouldThrow` sqlErrorContaining "must append the next revision"
          withRollback connection $ do
            seedCandleHistoryMarket connection
            insertHistoryGenesis connection
            insertHistoryRelease
              connection
              2
              "0x5555555555555555555555555555555555555555"
              90
              99
              2_000
              False
              "integration-out-of-order"
              `shouldThrow` sqlErrorContaining "must append in activation order"

    it "rejects whitespace-only target and release audit evidence" $
      withCandleDatabase databaseUrl $ \pool ->
        withDb pool $ \connection -> do
          withRollback connection $ do
            seedCandleHistoryMarket connection
            execute_
              connection
              "INSERT INTO perps_candle_history_targets (\
              \market_id, revision, requested_start_timestamp, requested_by, request_reference) \
              \VALUES ('dxy-history-integration', 1, 1700000001, E'\\t\\n', 'audit-target')"
              `shouldThrow` sqlErrorState "23514"
          withRollback connection $ do
            seedCandleHistoryMarket connection
            insertHistoryGenesisWithApproval connection "\t\n"
              `shouldThrow` sqlErrorState "23514"

    it "binds the exact current-candle validation clock to both grace-side responses" $
      withCandleDatabase databaseUrl $ \pool -> do
        let interval = 3_600
            boundary = baseTime + interval
            beforeGrace = boundary + 134
            atGraceExpiry = boundary + 135
            coverageStart = boundary - 2 * interval
            coverageEnd = boundary + interval
            finalizedThrough = boundary - interval
            config = candleApiConfig databaseUrl
        withDb pool $ \connection -> do
          ensureCurrentBasketDefinition connection testSeries
          putPriceCoverage
            connection interval coverageStart coverageEnd finalizedThrough 7 True
          putVolumeCoverage
            connection interval coverageStart coverageEnd finalizedThrough 11 True

        baseCache <- newAppCache
        testCurrentCache <- newConcurrentSingleFlightCache 32 5_000_000_000
        let cache =
              baseCache {cacheBasketCurrentCandles = testCurrentCache}
        beforeResponse <- currentCandleResponse cache pool config beforeGrace
        simpleStatus beforeResponse `shouldBe` status200
        filter
          ((== "X-Plether-Candle-Validated-At") . fst)
          (simpleHeaders beforeResponse)
          `shouldBe` [("X-Plether-Candle-Validated-At", BS8.pack $ show beforeGrace)]

        cachedResponse <- currentCandleResponse cache pool config beforeGrace
        shouldBe (simpleStatus cachedResponse) status200
        let serverTimingValues =
              map snd $
                filter
                  ((== "Server-Timing") . fst)
                  (simpleHeaders cachedResponse)
        shouldSatisfy
          serverTimingValues
          (any $ BS8.isInfixOf "plether_db_candles;dur=0.000")
        shouldSatisfy
          serverTimingValues
          (any $ BS8.isInfixOf "plether_singleflight_wait;dur=0.000")

        let initialGeneration = currentResponseDatasetGeneration cachedResponse
        initialGeneration `shouldBe` Just (7 * 134_217_728 + 11 * 2 + 1)
        withDb pool $ \connection -> do
          putPriceCoverage
            connection interval coverageStart coverageEnd finalizedThrough 8 True
          putVolumeCoverage
            connection interval coverageStart coverageEnd finalizedThrough 12 True

        -- A normal request still observes the live raw snapshot, while
        -- the worker/client no-cache contract must linearize through the DB.
        stillCachedResponse <- currentCandleResponse cache pool config beforeGrace
        currentResponseDatasetGeneration stillCachedResponse
          `shouldBe` initialGeneration
        revalidatedResponse <-
          currentCandleResponseFresh cache pool config beforeGrace
        simpleStatus revalidatedResponse `shouldBe` status200
        currentResponseDatasetGeneration revalidatedResponse
          `shouldBe` Just (8 * 134_217_728 + 12 * 2 + 1)

        -- The raw snapshot may be reused, but the later caller must still fail
        -- against its own validation second at the exact grace boundary.
        faultResponse <- currentCandleResponse cache pool config atGraceExpiry
        simpleStatus faultResponse `shouldBe` status503
        filter
          ((== "X-Plether-Candle-Validated-At") . fst)
          (simpleHeaders faultResponse)
          `shouldBe` [("X-Plether-Candle-Validated-At", BS8.pack $ show atGraceExpiry)]

        -- A validation failure from a still-fresh raw snapshot must trigger one
        -- authoritative reload, so a newly published generation is not hidden
        -- behind the bounded cache.
        withDb pool $ \connection -> do
          putPriceCoverage
            connection interval coverageStart coverageEnd boundary 8 True
          putVolumeCoverage
            connection interval coverageStart coverageEnd boundary 12 True
        recoveredResponse <-
          currentCandleResponse cache pool config atGraceExpiry
        shouldBe (simpleStatus recoveredResponse) status200

        recoveredCachedResponse <-
          currentCandleResponse cache pool config atGraceExpiry
        shouldBe (simpleStatus recoveredCachedResponse) status200
        let recoveredTimingValues =
              map snd $
                filter
                  ((== "Server-Timing") . fst)
                  (simpleHeaders recoveredCachedResponse)
        shouldSatisfy
          recoveredTimingValues
          (any $ BS8.isInfixOf "plether_db_candles;dur=0.000")

    it "rejects a conflicting replay event identity and rolls back its transaction" $
      withCandleDatabase databaseUrl $ \pool ->
        withDb pool $ \connection -> do
          let blockNumber = 70
              timestamp = baseTime + 70
              txHash = "candle-rollup-event-tx:replay-conflict"
              blockHash = "candle-rollup-event-block:70"
              contractAddress = "canonical-replay-engine"
              eventName = "PositionClosed"
              account = Just "canonical-replay-account"
              side = Just 1
              storedPayload = object ["canonicalReplay" .= False]
              replayPayload = object ["canonicalReplay" .= True]
              markerKey :: Text
              markerKey = "candle-rollup-integration:replay-rollback-marker"
          insertPerpsEvent
            connection
            testChainId
            testRouter
            contractAddress
            eventName
            txHash
            blockNumber
            blockHash
            0
            blockNumber
            timestamp
            account
            Nothing
            side
            storedPayload
          replayResult <-
            try
              ( withTransaction connection $ do
                  -- Every canonical field except payload matches the stored
                  -- row. The production conflict handler suppresses this
                  -- insert, reproducing the stale-row failure mode precisely.
                  insertPerpsEvent
                    connection
                    testChainId
                    testRouter
                    contractAddress
                    eventName
                    txHash
                    blockNumber
                    blockHash
                    0
                    blockNumber
                    timestamp
                    account
                    Nothing
                    side
                    replayPayload
                  insertActivity
                    connection
                    "replay-rollback-marker"
                    timestamp
                    (blockNumber + 1)
                    1
                    1
                    "Open"
                  markerCountInside <-
                    query
                      connection
                      "SELECT COUNT(*) FROM perps_account_activity WHERE event_key = ?"
                      (Only markerKey) :: IO [Only Integer]
                  markerCountInside `shouldBe` [Only 1]
                  assertPerpsReplayEventExact
                    connection
                    testChainId
                    testRouter
                    contractAddress
                    eventName
                    txHash
                    blockNumber
                    blockHash
                    0
                    blockNumber
                    timestamp
                    account
                    Nothing
                    side
                    replayPayload
              ) :: IO (Either IOException ())
          replayResult `shouldSatisfy` either (const True) (const False)
          either displayException (const "") replayResult
            `shouldBe` "user error (Bounded replay semantic assertion failed for event)"
          storedEventPayload <-
            query
              connection
              "SELECT data FROM perps_events \
              \WHERE chain_id = ? AND tx_hash = ? AND log_index = ?"
              (testChainId, txHash, blockNumber) :: IO [Only Value]
          storedEventPayload `shouldBe` [Only storedPayload]
          markerCount <-
            query
              connection
              "SELECT COUNT(*) FROM perps_account_activity WHERE event_key = ?"
              (Only markerKey) :: IO [Only Integer]
          markerCount `shouldBe` [Only 0]

    it "repairs the exact invalid index left by a failed concurrent build" $
      withCandleDatabase databaseUrl $ \pool ->
        withDb pool $ \connection -> do
          insertActivity connection "index-duplicate-a" baseTime 71 1 100 "Open"
          insertActivity connection "index-duplicate-b" (baseTime + 1) 72 1 100 "Open"
          void $
            execute_ connection
              "DROP INDEX idx_perps_account_activity_volume_rollup"
          let restoreExpectedIndex = ensureCandleSchema connection
          flip finally restoreExpectedIndex $ do
            failedBuild <-
              try
                ( void $
                    execute_ connection
                      "CREATE UNIQUE INDEX CONCURRENTLY \
                      \idx_perps_account_activity_volume_rollup \
                      \ON perps_account_activity(chain_id, release_router, activity_type)"
                ) :: IO (Either SqlError ())
            failedBuild `shouldSatisfy` either (const True) (const False)
            candleActivityIndexValidity connection `shouldReturn` [Only False]
            ensureCandleSchema connection
            candleActivityIndexValidity connection `shouldReturn` [Only True]

    it "repairs the exact invalid event-bounds index left by a failed build" $
      withCandleDatabase databaseUrl $ \pool ->
        withDb pool $ \connection -> do
          insertRawEvent connection "index-event-a" baseTime 81
          insertRawEvent connection "index-event-b" (baseTime + 1) 82
          void $
            execute_ connection
              "DROP INDEX idx_perps_events_candle_bounds"
          let restoreExpectedIndex = ensureCandleSchema connection
          flip finally restoreExpectedIndex $ do
            failedBuild <-
              try
                ( void $
                    execute_ connection
                      "CREATE UNIQUE INDEX CONCURRENTLY idx_perps_events_candle_bounds \
                      \ON perps_events(chain_id, release_router)"
                ) :: IO (Either SqlError ())
            failedBuild `shouldSatisfy` either (const True) (const False)
            candleEventIndexValidity connection `shouldReturn` [Only False]
            ensureCandleSchema connection
            candleEventIndexValidity connection `shouldReturn` [Only True]

    it "repairs exact invalid block-rewind indexes left by failed builds" $
      withCandleDatabase databaseUrl $ \pool ->
        withDb pool $ \connection -> do
          insertActivity connection "reorg-index-duplicate-a" baseTime 91 1 100 "Open"
          insertActivity connection "reorg-index-duplicate-b" (baseTime + 1) 92 1 100 "Open"
          insertRawEvent connection "reorg-index-event-a" baseTime 93
          insertRawEvent connection "reorg-index-event-b" (baseTime + 1) 94
          void $ execute_ connection "DROP INDEX idx_perps_account_activity_reorg_blocks"
          void $ execute_ connection "DROP INDEX idx_perps_events_reorg_blocks"
          let restoreExpectedIndexes = ensureCandleSchema connection
          flip finally restoreExpectedIndexes $ do
            failedActivityBuild <-
              try
                ( void $
                    execute_ connection
                      "CREATE UNIQUE INDEX CONCURRENTLY idx_perps_account_activity_reorg_blocks \
                      \ON perps_account_activity(chain_id, release_router, activity_type)"
                ) :: IO (Either SqlError ())
            failedActivityBuild `shouldSatisfy` either (const True) (const False)
            failedEventBuild <-
              try
                ( void $
                    execute_ connection
                      "CREATE UNIQUE INDEX CONCURRENTLY idx_perps_events_reorg_blocks \
                      \ON perps_events(chain_id, release_router)"
                ) :: IO (Either SqlError ())
            failedEventBuild `shouldSatisfy` either (const True) (const False)
            candleActivityReorgIndexValidity connection `shouldReturn` [Only False]
            candleEventReorgIndexValidity connection `shouldReturn` [Only False]
            ensureCandleSchema connection
            candleActivityReorgIndexValidity connection `shouldReturn` [Only True]
            candleEventReorgIndexValidity connection `shouldReturn` [Only True]

    it "derives deterministic OHLC at every canonical interval" $
      withCandleDatabase databaseUrl $ \pool ->
        withDb pool $ \connection -> do
          ensureCurrentBasketDefinition connection testSeries
          -- Insert out of timestamp order and add a lower-priority value at
          -- the same publish time. OHLC must use event time and discard the
          -- lower-priority tier at that publish time.
          insertObservation connection "close" (baseTime + 45) 110 "signed_pyth" 100
          insertObservation connection "low" (baseTime + 20) 90 "signed_pyth" 100
          insertObservation connection "open-fallback" (baseTime + 5) 999 "legacy_sampled" 10
          insertObservation connection "open" (baseTime + 5) 100 "signed_pyth" 100
          recomputeBasketCandleHierarchy connection testSeries (baseTime + 20) 0

          forM_ canonicalCandleIntervals $ \interval -> do
            stored <- requireStoredCandle connection interval baseTime
            stored
              `shouldBe` StoredCandle
                { scBucketStart = baseTime
                , scOpen = 100
                , scHigh = 110
                , scLow = 90
                , scClose = 110
                , scSamples = 3
                , scQuality = "observed"
                , scRevision = 1
                , scFinalized = True
                }

    it "retains every highest-priority signed update at the same publish time" $
      withCandleDatabase databaseUrl $ \pool ->
        withDb pool $ \connection -> do
          ensureCurrentBasketDefinition connection testSeries
          insertObservation connection "same-time-z" (baseTime + 5) 130 "signed_pyth" 100
          insertObservation connection "same-time-a" (baseTime + 5) 100 "signed_pyth" 100
          insertObservation connection "same-time-fallback" (baseTime + 5) 999 "legacy_sampled" 10
          recomputeBasketCandleHierarchy connection testSeries (baseTime + 5) 0

          forM_ canonicalCandleIntervals $ \interval -> do
            stored <- requireStoredCandle connection interval baseTime
            stored
              `shouldBe` StoredCandle
                { scBucketStart = baseTime
                , scOpen = 100
                , scHigh = 130
                , scLow = 100
                , scClose = 130
                , scSamples = 2
                , scQuality = "observed"
                , scRevision = 1
                , scFinalized = True
                }

    it "applies material equal-priority legacy corrections without allowing a downgrade" $
      withCandleDatabase databaseUrl $ \pool ->
        withDb pool $ \connection -> do
          ensureCurrentBasketDefinition connection testSeries
          let observationId = "legacy-snapshot:300:" <> Text.pack (show $ baseTime + 5)
              original = observation observationId (baseTime + 5) 100 "legacy_sampled" 10
              correction = observation observationId (baseTime + 5) 125 "legacy_sampled" 10
              downgrade = observation observationId (baseTime + 5) 999 "legacy_sampled" 9
          originalChanged <- upsertBasketObservation connection original
          originalChanged `shouldBe` True
          recomputeBasketCandleHierarchy connection testSeries (baseTime + 5) 0

          correctionChanged <- upsertBasketObservation connection correction
          correctionChanged `shouldBe` True
          recomputeBasketCandleHierarchy connection testSeries (baseTime + 5) 0

          duplicateChanged <- upsertBasketObservation connection correction
          duplicateChanged `shouldBe` False
          downgradeChanged <- upsertBasketObservation connection downgrade
          downgradeChanged `shouldBe` False

          forM_ canonicalCandleIntervals $ \interval -> do
            stored <- requireStoredCandle connection interval baseTime
            stored
              `shouldBe` StoredCandle
                { scBucketStart = baseTime
                , scOpen = 125
                , scHigh = 125
                , scLow = 125
                , scClose = 125
                , scSamples = 1
                , scQuality = "legacy_sampled"
                , scRevision = 2
                , scFinalized = True
                }

    it "rejects attempts to move an observation identity to another source time" $
      withCandleDatabase databaseUrl $ \pool ->
        withDb pool $ \connection -> do
          ensureCurrentBasketDefinition connection testSeries
          let originalTime = baseTime + 5
              movedTime = baseTime + 65
              observationId = "immutable-source-time"
          changed <-
            upsertBasketObservation connection $
              observation observationId originalTime 100 "signed_pyth" 100
          changed `shouldBe` True

          upsertBasketObservation
            connection
            (observation observationId movedTime 125 "signed_pyth" 100)
            `shouldThrow` anyException

          stored <-
            query
              connection
              "SELECT publish_time, basket_price FROM perps_basket_observations \
              \WHERE series_id = ? AND observation_id = ?"
              (testSeries, observationId) :: IO [(Integer, Integer)]
          stored `shouldBe` [(originalTime, 100)]

    it "does not let a historical reveal overwrite a newer latest snapshot" $
      withCandleDatabase databaseUrl $ \pool ->
        withDb pool $ \connection -> do
          let timestamp = baseTime + 5
              previousSourceTimestamp = baseTime + 65
          insertBasketSnapshotWithSource
            connection timestamp 60 90 componentPayload historicalSnapshotSource
          insertBasketSnapshotWithSource
            connection timestamp 60 120 componentPayload latestSnapshotSource
          insertBasketSnapshotWithSource
            connection timestamp 60 95 componentPayload historicalSnapshotSource

          snapshot <- legacySnapshotAt connection timestamp
          snapshot `shouldBe` [(120, latestSnapshotSource)]

          -- Same-tier latest corrections remain possible and retain their
          -- admitted provenance.
          insertBasketSnapshotWithSource
            connection timestamp 60 125 componentPayload latestSnapshotSource
          corrected <- legacySnapshotAt connection timestamp
          corrected `shouldBe` [(125, latestSnapshotSource)]

          -- Rows written before admitted source versioning receive the same
          -- latest-source protection during a rolling deployment.
          insertBasketSnapshotWithSource
            connection previousSourceTimestamp 60 130 componentPayload previousLatestSnapshotSource
          insertBasketSnapshotWithSource
            connection previousSourceTimestamp 60 80 componentPayload historicalSnapshotSource
          previous <- legacySnapshotAt connection previousSourceTimestamp
          previous `shouldBe` [(130, previousLatestSnapshotSource)]

    it "backfills mixed legacy intervals and prefers the finest row at each timestamp" $
      withCandleDatabase databaseUrl $ \pool ->
        withDb pool $ \connection -> do
          ensureCurrentBasketDefinition connection testSeries
          insertBasketSnapshotWithSource
            connection (baseTime + 5) 300 200 componentPayload legacySnapshotSource
          insertBasketSnapshotWithSource
            connection (baseTime + 65) 300 999 componentPayload legacySnapshotSource
          insertBasketSnapshotWithSource
            connection (baseTime + 65) 60 210 componentPayload legacySnapshotSource
          insertBasketSnapshotWithSource
            connection (baseTime + 125) 300 220 componentPayload legacySnapshotSource

          affected <-
            backfillLegacyBasketSnapshots connection testSeries baseTime (baseTime + 180)
          affected `shouldBe` 3
          minuteRows <- storedCandles connection 60 baseTime (baseTime + 180)
          minuteRows
            `shouldBe`
              [ StoredCandle baseTime 200 200 200 200 1 "legacy_sampled" 1 True
              , StoredCandle (baseTime + 60) 210 210 210 210 1 "legacy_sampled" 1 True
              , StoredCandle (baseTime + 120) 220 220 220 220 1 "legacy_sampled" 1 True
              ]
          parent <- requireStoredCandle connection 180 baseTime
          parent
            `shouldBe`
              StoredCandle baseTime 200 220 200 220 3 "legacy_sampled" 1 True

    it "preserves observed minutes while backfilling legacy sampled history" $
      withCandleDatabase databaseUrl $ \pool ->
        withDb pool $ \connection -> do
          ensureCurrentBasketDefinition connection testSeries
          insertObservation
            connection
            "observed-before-range"
            (baseTime - 55)
            150
            "signed_pyth"
            100
          recomputeBasketCandleHierarchy connection testSeries (baseTime - 55) 0
          insertObservation
            connection
            "observed-overlap"
            (baseTime + 125)
            320
            "signed_pyth"
            100
          recomputeBasketCandleHierarchy connection testSeries (baseTime + 125) 0
          insertBasketSnapshotWithSource
            connection
            (baseTime + 5)
            60
            200
            componentPayload
            legacySnapshotSource

          affected <-
            backfillLegacyBasketSnapshots
              connection
              testSeries
              baseTime
              (baseTime + 180)
          -- Both the legacy minute and the overlapping observed minute are
          -- rebuilt from their canonical sources inside the replacement
          -- chunk; the observed value must win for its minute.
          affected `shouldBe` 2

          beforeRange <- requireStoredCandle connection 60 (baseTime - 60)
          beforeRange
            `shouldBe`
              StoredCandle (baseTime - 60) 150 150 150 150 1 "observed" 1 True

          minuteRows <- storedCandles connection 60 baseTime (baseTime + 180)
          minuteRows
            `shouldBe`
              [ StoredCandle baseTime 200 200 200 200 1 "legacy_sampled" 1 True
              , StoredCandle (baseTime + 120) 320 320 320 320 1 "observed" 1 True
              ]
          parent <- requireStoredCandle connection 180 baseTime
          parent
            `shouldBe`
              StoredCandle baseTime 200 320 200 320 2 "mixed" 1 True

          -- Re-running a chunk is replacement-idempotent.
          _ <-
            backfillLegacyBasketSnapshots
              connection
              testSeries
              baseTime
              (baseTime + 180)
          replayedMinutes <-
            storedCandles connection 60 baseTime (baseTime + 180)
          replayedMinutes `shouldBe` minuteRows
          replayedParent <- requireStoredCandle connection 180 baseTime
          replayedParent `shouldBe` parent

    it "retains exact volume numerators and builds every parent interval" $
      withCandleDatabase databaseUrl $ \pool ->
        withDb pool $ \connection -> do
          let sizeOne = 2_000_000_000_000_000_003
              priceOne = 123_456_789
              sizeTwo = -3_000_000_000_000_000_007
              priceTwo = 222_222_223
              numeratorOne = abs sizeOne * priceOne
              numeratorTwo = abs sizeTwo * priceTwo
              totalNumerator = numeratorOne + numeratorTwo
          insertActivity connection "volume-one" (baseTime + 5) 100 sizeOne priceOne "Open"
          insertActivity connection "volume-two" (baseTime + 65) 101 sizeTwo priceTwo "Liquidated"
          insertActivity connection "ignored-funding" (baseTime + 10) 102 sizeTwo priceTwo "Funding"

          affected <-
            backfillMarketVolume
              connection
              testChainId
              testRouter
              baseTime
              (baseTime + 180)
          affected `shouldBe` 2

          minuteVolumes <-
            storedVolumes connection 60 baseTime (baseTime + 180)
          minuteVolumes
            `shouldBe`
              [ StoredVolume baseTime numeratorOne 1 100 100 1 True
              , StoredVolume (baseTime + 60) numeratorTwo 1 101 101 1 True
              ]
          forM_ (drop 1 canonicalCandleIntervals) $ \interval -> do
            parentVolume <- requireStoredVolume connection interval baseTime
            parentVolume
              `shouldBe`
                StoredVolume
                  baseTime
                  totalNumerator
                  2
                  100
                  101
                  1
                  (interval <= 180)

          -- Replacement backfill must not double count replays.
          _ <-
            backfillMarketVolume
              connection
              testChainId
              testRouter
              baseTime
              (baseTime + 180)
          replayedVolume <- requireStoredVolume connection 3600 baseTime
          replayedVolume
            `shouldBe`
              StoredVolume baseTime totalNumerator 2 100 101 1 False

    it "recomputes each shared volume parent once per batch" $
      withCandleDatabase databaseUrl $ \pool ->
        withDb pool $ \connection -> do
          insertActivity connection "batch-volume-a" (baseTime + 5) 100 2 10 "Open"
          insertActivity connection "batch-volume-b" (baseTime + 65) 101 3 10 "Close"
          insertActivity connection "batch-volume-c" (baseTime + 125) 102 4 10 "Liquidated"
          recomputeMarketVolumeHierarchyBatch
            connection testChainId testRouter
            [baseTime + 125, baseTime + 5, baseTime + 65, baseTime + 5]
            0

          firstParent <- requireStoredVolume connection 180 baseTime
          firstParent `shouldBe` StoredVolume baseTime 90 3 100 102 1 True
          forM_ canonicalCandleIntervals $ \interval ->
            putVolumeCoverage
              connection interval baseTime (baseTime + 86_400) (baseTime + 86_400) 7 True

          insertActivity connection "batch-volume-d" (baseTime + 10) 103 1 10 "Open"
          insertActivity connection "batch-volume-e" (baseTime + 70) 104 2 10 "Close"
          recomputeMarketVolumeHierarchyBatch
            connection testChainId testRouter
            [baseTime + 70, baseTime + 5, baseTime + 10, baseTime + 65, baseTime + 70]
            0

          firstMinute <- requireStoredVolume connection 60 baseTime
          secondMinute <- requireStoredVolume connection 60 (baseTime + 60)
          unchangedMinute <- requireStoredVolume connection 60 (baseTime + 120)
          secondParent <- requireStoredVolume connection 180 baseTime
          firstMinute `shouldBe` StoredVolume baseTime 30 2 100 103 2 True
          secondMinute `shouldBe` StoredVolume (baseTime + 60) 50 2 101 104 2 True
          unchangedMinute `shouldBe` StoredVolume (baseTime + 120) 40 1 102 102 1 True
          -- Both changed minutes share this parent. One batch causes one
          -- parent replacement/revision, not one replacement per minute.
          secondParent `shouldBe` StoredVolume baseTime 120 5 100 104 2 True
          forM_ canonicalCandleIntervals $ \interval -> do
            coverage <- requireVolumeCoverage connection interval
            rcGeneration coverage `shouldBe` 8

    it "serves price-led native pages while legacy compatibility remains volume-bounded" $
      withCandleDatabase databaseUrl $ \pool ->
        withDb pool $ \connection -> do
          let cursor = baseTime + pageSpan60
              priceCoverageStart = baseTime - pageSpan60
              volumeCoverageStart = baseTime + 900
              expectedBuckets = [baseTime, baseTime + 900, baseTime + 1800]
              emptyPage =
                CandlePage [] Nothing False Nothing Nothing Nothing 0 False
                  Nothing Nothing Nothing False
          ensureCurrentBasketDefinition connection testSeries
          (initialHistorySelection, _) <-
            appendCandleHistorySelection
              connection
              defaultCandleMarketId
              testChainId
              testSeries
              priceCoverageStart
              "integration-test"
              "native-price-led-history"
          _ <-
            initializeCandleHistoryIngestionProgress
              connection initialHistorySelection priceCoverageStart cursor 60
          _ <-
            completeCandleHistoryIngestionWindow
              connection initialHistorySelection priceCoverageStart cursor 0
          expectedDefinition <-
            getActiveBasketDefinitionIdentity connection (cursor - 1)
          let readNativePage pageCursor = do
                plainPage <-
                  getBasketCandlePage
                    connection testSeries testChainId testRouter 60 pageCursor
                (definition, snapshotPage) <-
                  getBasketCandlePageSnapshot
                    connection (cursor - 1) testChainId testRouter 60 pageCursor
                definition `shouldBe` expectedDefinition
                snapshotPage `shouldBe` plainPage
                pure snapshotPage
              readCurrent timestamp = do
                plainCurrent <-
                  getCurrentBasketCandle
                    connection testSeries testChainId testRouter 60 timestamp
                (definition, snapshotCurrent) <-
                  getBasketCurrentCandleSnapshot
                    connection timestamp testChainId testRouter 60
                definition `shouldBe` expectedDefinition
                snapshotCurrent `shouldBe` plainCurrent
                pure snapshotCurrent
              assertPriceRows page =
                map bcrBucketStart (cpCandles page) `shouldBe` expectedBuckets
              assertUnknownVolume page = do
                map bcrVolumeNumerator (cpCandles page)
                  `shouldBe` replicate 3 Nothing
                map bcrTradeCount (cpCandles page)
                  `shouldBe` replicate 3 Nothing
                map bcrVolumeComplete (cpCandles page)
                  `shouldBe` replicate 3 False

          pageWithoutCoverage <- readNativePage cursor
          pageWithoutCoverage `shouldBe` emptyPage

          insertObservation connection "earlier" (baseTime - 55) 80 "signed_pyth" 100
          insertObservation connection "pre-volume-price" (baseTime + 5) 100 "signed_pyth" 100
          insertObservation connection "covered-volume-price" (baseTime + 905) 120 "signed_pyth" 100
          insertObservation connection "covered-zero-price" (baseTime + 1805) 140 "signed_pyth" 100
          insertActivity connection "covered-page-volume" (baseTime + 905) 150 7 11 "Open"
          forM_ [baseTime - 55, baseTime + 5, baseTime + 905, baseTime + 1805] $ \timestamp ->
            recomputeBasketCandleHierarchy connection testSeries timestamp 0
          recomputeMarketVolumeHierarchy
            connection testChainId testRouter (baseTime + 905) 0
          putPriceCoverage
            connection 60 priceCoverageStart cursor cursor 2 True
          _ <-
            publishCandleHistoryIngestion
              connection initialHistorySelection 2

          -- Price coverage alone is sufficient for native history. Absence of
          -- current-router volume metadata is represented as unknown, not as a
          -- fabricated zero and not as a failed price page.
          noVolume <- readNativePage cursor
          assertPriceRows noVolume
          assertUnknownVolume noVolume
          cpCoverageStart noVolume `shouldBe` Just priceCoverageStart
          cpCoverageEnd noVolume `shouldBe` Just cursor
          cpFinalizedThrough noVolume `shouldBe` Just cursor
          cpCoverageComplete noVolume `shouldBe` True
          cpDatasetGeneration noVolume `shouldSatisfy` (> 0)
          cpVolumeCoverageStart noVolume `shouldBe` Nothing
          cpVolumeCoverageEnd noVolume `shouldBe` Nothing
          cpVolumeFinalizedThrough noVolume `shouldBe` Nothing
          cpVolumeCoverageComplete noVolume `shouldBe` False

          currentPriceOnly <- readCurrent (baseTime + 920)
          case ccCandle currentPriceOnly of
            Just row -> do
              bcrVolumeNumerator row `shouldBe` Nothing
              bcrTradeCount row `shouldBe` Nothing
              bcrVolumeComplete row `shouldBe` False
            Nothing -> fail "Expected the current price row without volume coverage"

          currentWithoutVolume <- readCurrent (baseTime + 1200)
          ccCandle currentWithoutVolume `shouldBe` Nothing
          ccCoverageStart currentWithoutVolume `shouldBe` Just priceCoverageStart
          ccCoverageEnd currentWithoutVolume `shouldBe` Just cursor
          ccFinalizedThrough currentWithoutVolume `shouldBe` Just cursor
          ccCoverageComplete currentWithoutVolume `shouldBe` True
          ccVolumeCoverageStart currentWithoutVolume `shouldBe` Nothing
          ccVolumeCoverageEnd currentWithoutVolume `shouldBe` Nothing
          ccVolumeFinalizedThrough currentWithoutVolume `shouldBe` Nothing
          ccVolumeCoverageComplete currentWithoutVolume `shouldBe` False
          ccDatasetGeneration currentWithoutVolume
            `shouldBe` cpDatasetGeneration noVolume

          -- A complete row written by an obsolete derivation cannot prove
          -- volume. Native price rows remain available and explicitly null.
          putVolumeCoverageVersion
            connection 60 volumeCoverageStart cursor cursor 1 True "v0"
          staleVolume <- readNativePage cursor
          assertPriceRows staleVolume
          assertUnknownVolume staleVolume
          cpCoverageComplete staleVolume `shouldBe` True
          cpDatasetGeneration staleVolume
            `shouldSatisfy` (/= cpDatasetGeneration noVolume)
          staleCurrentVolume <- readCurrent (baseTime + 920)
          case ccCandle staleCurrentVolume of
            Just row -> do
              bcrVolumeNumerator row `shouldBe` Nothing
              bcrTradeCount row `shouldBe` Nothing
              bcrVolumeComplete row `shouldBe` False
            Nothing -> fail "Expected the current price row with stale volume coverage"

          -- An incomplete current-derivation volume publication likewise does
          -- not suppress price or claim that any bucket has known volume.
          putVolumeCoverage
            connection 60 volumeCoverageStart cursor cursor 2 False
          incompleteVolume <- readNativePage cursor
          assertPriceRows incompleteVolume
          assertUnknownVolume incompleteVolume
          cpCoverageComplete incompleteVolume `shouldBe` True
          cpDatasetGeneration incompleteVolume
            `shouldSatisfy` (/= cpDatasetGeneration staleVolume)
          incompleteCurrentVolume <- readCurrent (baseTime + 920)
          case ccCandle incompleteCurrentVolume of
            Just row -> do
              bcrVolumeNumerator row `shouldBe` Nothing
              bcrTradeCount row `shouldBe` Nothing
              bcrVolumeComplete row `shouldBe` False
            Nothing -> fail "Expected the current price row with incomplete volume coverage"

          -- The live coverage terminal is aligned down to the active bucket.
          -- Equality deliberately exposes its provisional volume, while the
          -- finalized watermark keeps that mutable value incomplete.
          putVolumeCoverage
            connection 60 baseTime volumeCoverageStart volumeCoverageStart 3 True
          exactBoundaryCurrentVolume <- readCurrent (baseTime + 920)
          case ccCandle exactBoundaryCurrentVolume of
            Just row -> do
              bcrVolumeNumerator row `shouldBe` Just 77
              bcrTradeCount row `shouldBe` Just 1
              bcrVolumeComplete row `shouldBe` False
            Nothing -> fail "Expected provisional volume at the live coverage terminal"

          -- Once current-router coverage is usable, the pre-coverage candle
          -- remains unknown, the real trade remains exact, and absence of a
          -- rollup row inside proven coverage is a known zero.
          putVolumeCoverage
            connection 60 volumeCoverageStart cursor (baseTime + 1800) 4 True
          partialVolume <- readNativePage cursor
          map bcrVolumeNumerator (cpCandles partialVolume)
            `shouldBe` [Nothing, Just 77, Nothing]
          map bcrVolumeComplete (cpCandles partialVolume)
            `shouldBe` [False, True, False]
          cpVolumeCoverageStart partialVolume `shouldBe` Just volumeCoverageStart
          cpVolumeCoverageEnd partialVolume `shouldBe` Just cursor
          cpVolumeFinalizedThrough partialVolume `shouldBe` Just (baseTime + 1800)
          cpVolumeCoverageComplete partialVolume `shouldBe` True
          coveredCurrentVolume <- readCurrent (baseTime + 920)
          case ccCandle coveredCurrentVolume of
            Just row -> do
              bcrVolumeNumerator row `shouldBe` Just 77
              bcrTradeCount row `shouldBe` Just 1
              bcrVolumeComplete row `shouldBe` True
            Nothing -> fail "Expected the current price row inside usable volume coverage"

          -- The live finalizer advances this watermark without changing the
          -- dataset generation. The response metadata must still expose the
          -- transition so edge caches cannot retain nullable volume.
          putVolumeCoverage
            connection 60 volumeCoverageStart cursor cursor 4 True
          page <- readNativePage cursor
          assertPriceRows page
          map bcrVolumeNumerator (cpCandles page)
            `shouldBe` [Nothing, Just 77, Just 0]
          map bcrTradeCount (cpCandles page)
            `shouldBe` [Nothing, Just 1, Just 0]
          map bcrVolumeComplete (cpCandles page)
            `shouldBe` [False, True, True]
          cpPreviousCursor page `shouldBe` Just baseTime
          cpHasEarlier page `shouldBe` True
          cpCoverageStart page `shouldBe` Just priceCoverageStart
          cpCoverageEnd page `shouldBe` Just cursor
          cpFinalizedThrough page `shouldBe` Just cursor
          cpCoverageComplete page `shouldBe` True
          cpVolumeCoverageStart page `shouldBe` Just volumeCoverageStart
          cpVolumeCoverageEnd page `shouldBe` Just cursor
          cpVolumeFinalizedThrough page `shouldBe` Just cursor
          cpVolumeCoverageComplete page `shouldBe` True
          cpDatasetGeneration page `shouldBe` cpDatasetGeneration partialVolume

          current <- readCurrent (baseTime + 1200)
          ccCandle current `shouldBe` Nothing
          ccCoverageStart current `shouldBe` cpCoverageStart page
          ccCoverageEnd current `shouldBe` cpCoverageEnd page
          ccFinalizedThrough current `shouldBe` cpFinalizedThrough page
          ccDatasetGeneration current `shouldBe` cpDatasetGeneration page
          ccCoverageComplete current `shouldBe` True
          ccVolumeCoverageStart current `shouldBe` Just volumeCoverageStart
          ccVolumeCoverageEnd current `shouldBe` Just cursor
          ccVolumeFinalizedThrough current `shouldBe` Just cursor
          ccVolumeCoverageComplete current `shouldBe` True

          -- The legacy response cannot express unknown volume. Its range stays
          -- on the combined price/volume intersection and omits the earlier
          -- price-only candle instead of turning it into zero volume.
          range <-
            getBasketCandleRange
              connection testSeries testChainId testRouter 60
              baseTime (baseTime + 2400) 12_001
          map bcrBucketStart (crCandles range)
            `shouldBe` [baseTime + 900, baseTime + 1800]
          map bcrVolumeNumerator (crCandles range)
            `shouldBe` [Just 77, Just 0]
          map bcrTradeCount (crCandles range)
            `shouldBe` [Just 1, Just 0]
          map bcrVolumeComplete (crCandles range)
            `shouldBe` [True, True]
          crCoverageStart range `shouldBe` Just volumeCoverageStart
          crCoverageEnd range `shouldBe` Just cursor
          crFinalizedThrough range `shouldBe` Just cursor
          crDatasetGeneration range `shouldSatisfy` (> 0)
          crCoverageComplete range `shouldBe` True

          -- A complete page immediately beyond price coverage is empty but
          -- retains price metadata and points to the newest price page.
          let nextCursor = cursor + pageSpan60
          beyondCoverage <- readNativePage nextCursor
          cpCandles beyondCoverage `shouldBe` []
          cpPreviousCursor beyondCoverage `shouldBe` Just cursor
          cpHasEarlier beyondCoverage `shouldBe` True
          cpCoverageStart beyondCoverage `shouldBe` Just priceCoverageStart
          cpCoverageComplete beyondCoverage `shouldBe` True

          -- With volume made incomplete again, native price remains available
          -- as nullable volume while legacy compatibility fails closed.
          putVolumeCoverage
            connection 60 volumeCoverageStart cursor cursor 5 False
          nativeAfterVolumeFailure <- readNativePage cursor
          assertPriceRows nativeAfterVolumeFailure
          assertUnknownVolume nativeAfterVolumeFailure
          cpCoverageComplete nativeAfterVolumeFailure `shouldBe` True
          cpVolumeCoverageStart nativeAfterVolumeFailure `shouldBe` Nothing
          cpVolumeCoverageEnd nativeAfterVolumeFailure `shouldBe` Nothing
          cpVolumeFinalizedThrough nativeAfterVolumeFailure `shouldBe` Nothing
          cpVolumeCoverageComplete nativeAfterVolumeFailure `shouldBe` False
          incompleteRange <-
            getBasketCandleRange
              connection testSeries testChainId testRouter 60
              baseTime (baseTime + 2400) 12_001
          crCandles incompleteRange `shouldBe` []
          crCoverageComplete incompleteRange `shouldBe` False

          -- The latest published logical-market selection is the public price
          -- boundary. Merely selecting a replacement remains private.
          (laterHistorySelection, _) <-
            appendCandleHistorySelection
              connection
              defaultCandleMarketId
              testChainId
              testSeries
              volumeCoverageStart
              "integration-test"
              "native-price-led-history-later"
          _ <-
            initializeCandleHistoryIngestionProgress
              connection laterHistorySelection volumeCoverageStart cursor 60
          _ <-
            completeCandleHistoryIngestionWindow
              connection laterHistorySelection volumeCoverageStart cursor 0
          laterGeneration <-
            bumpRollupDatasetGeneration
              connection PriceRollup (Just testSeries) Nothing Nothing
          _ <-
            publishCandleHistoryIngestion
              connection laterHistorySelection laterGeneration
          laterTarget <- readNativePage cursor
          map bcrBucketStart (cpCandles laterTarget)
            `shouldBe` [baseTime + 900, baseTime + 1800]
          cpCoverageStart laterTarget `shouldBe` Just volumeCoverageStart
          cpCoverageComplete laterTarget `shouldBe` True
          cpDatasetGeneration laterTarget
            `shouldSatisfy` (> cpDatasetGeneration nativeAfterVolumeFailure)

          let unfilledEarlierTarget = priceCoverageStart - 60
          _ <-
            appendCandleHistorySelection
              connection
              defaultCandleMarketId
              testChainId
              testSeries
              unfilledEarlierTarget
              "integration-test"
              "native-price-led-history-earlier"
          earlierTarget <- readNativePage cursor
          map bcrBucketStart (cpCandles earlierTarget)
            `shouldBe` [baseTime + 900, baseTime + 1800]
          cpCoverageStart earlierTarget `shouldBe` Just volumeCoverageStart
          cpCoverageComplete earlierTarget `shouldBe` True
          cpDatasetGeneration earlierTarget
            `shouldBe` cpDatasetGeneration laterTarget
          currentDuringEarlierBackfill <- readCurrent (baseTime + 1200)
          ccCoverageStart currentDuringEarlierBackfill `shouldBe` Just volumeCoverageStart
          ccCoverageComplete currentDuringEarlierBackfill `shouldBe` True
          ccDatasetGeneration currentDuringEarlierBackfill
            `shouldBe` cpDatasetGeneration laterTarget

          -- Once the canonical logical market exists, a runtime chain/series
          -- mismatch is an invariant failure, not permission to ignore the
          -- published boundary and expose the full physical price table.
          let mismatchedChainId = testChainId + 1
          mismatchedPlain <-
            getBasketCandlePage
              connection testSeries mismatchedChainId testRouter 60 cursor
          (mismatchedDefinition, mismatchedSnapshot) <-
            getBasketCandlePageSnapshot
              connection (cursor - 1) mismatchedChainId testRouter 60 cursor
          mismatchedDefinition `shouldBe` expectedDefinition
          mismatchedSnapshot `shouldBe` mismatchedPlain
          cpCandles mismatchedPlain `shouldBe` []
          cpCoverageComplete mismatchedPlain `shouldBe` False
          mismatchedCurrent <-
            getCurrentBasketCandle
              connection testSeries mismatchedChainId testRouter 60 (baseTime + 1200)
          (mismatchedCurrentDefinition, mismatchedCurrentSnapshot) <-
            getBasketCurrentCandleSnapshot
              connection (baseTime + 1200) mismatchedChainId testRouter 60
          mismatchedCurrentDefinition `shouldBe` expectedDefinition
          mismatchedCurrentSnapshot `shouldBe` mismatchedCurrent
          ccCoverageComplete mismatchedCurrent `shouldBe` False

    it "requires a new generation before usable rollup coverage can regress" $
      withCandleDatabase databaseUrl $ \pool ->
        withDb pool $ \connection -> do
          putVolumeCoverage
            connection 60 baseTime (baseTime + 3600) (baseTime + 3600) 3 True
          putVolumeCoverage
            connection 60 baseTime (baseTime + 3600) (baseTime + 3600) 3 False
            `shouldThrow` sqlErrorContaining "usability regression requires a new generation"
          putVolumeCoverage
            connection 60 (baseTime + 60) (baseTime + 3600) (baseTime + 3600) 3 True
            `shouldThrow` sqlErrorContaining "usability regression requires a new generation"
          putVolumeCoverage
            connection 60 baseTime (baseTime + 3600) (baseTime + 3540) 3 True
            `shouldThrow` sqlErrorContaining "usability regression requires a new generation"
          putVolumeCoverage
            connection 60 baseTime (baseTime + 3600) (baseTime + 3600) 4 False
          coverage <- requireVolumeCoverage connection 60
          rcGeneration coverage `shouldBe` 4
          rcComplete coverage `shouldBe` False

    it "finalizes unchanged price and volume rows solely through watermarks" $
      withCandleDatabase databaseUrl $ \pool ->
        withDb pool $ \connection -> do
          ensureCurrentBasketDefinition connection testSeries
          nowRows <-
            query_ connection
              "SELECT EXTRACT(EPOCH FROM NOW())::bigint" :: IO [Only Integer]
          now <- case nowRows of
            [Only value] -> pure value
            _ -> fail "Expected one PostgreSQL clock row"
          let currentMinute = now - now `mod` 60
              bucketStart = currentMinute - 120
              publishTime = bucketStart + 5
              initialGeneration = 7
          insertObservation
            connection "watermark-price" publishTime 140 "signed_pyth" 100
          recomputeBasketCandleHierarchy
            connection testSeries publishTime 3600
          insertActivity
            connection "watermark-volume" publishTime 300 5 12 "Open"
          recomputeMarketVolumeHierarchy
            connection testChainId testRouter publishTime 3600

          priceBefore <- requireStoredCandle connection 60 bucketStart
          volumeBefore <- requireStoredVolume connection 60 bucketStart
          scFinalized priceBefore `shouldBe` False
          svFinalized volumeBefore `shouldBe` False
          scRevision priceBefore `shouldBe` 1
          svRevision volumeBefore `shouldBe` 1

          putPriceCoverage
            connection
            60
            bucketStart
            currentMinute
            bucketStart
            initialGeneration
            True
          putVolumeCoverage
            connection
            60
            bucketStart
            currentMinute
            bucketStart
            initialGeneration
            True
          before <-
            getCurrentBasketCandle
              connection testSeries testChainId testRouter 60 publishTime
          case ccCandle before of
            Just row -> do
              bcrPriceComplete row `shouldBe` False
              bcrVolumeNumerator row `shouldBe` Just 60
              bcrTradeCount row `shouldBe` Just 1
              bcrVolumeComplete row `shouldBe` False
            Nothing -> fail "Expected the mutable candle before watermark advance"

          -- No observation, activity, or recomputation occurs after this
          -- point. Advancing independently proven source watermarks must flip
          -- only finalization state, without revision or generation churn.
          advanceBasketPriceCoverage
            connection testSeries currentMinute 0
          advanceMarketVolumeCoverage
            connection testChainId testRouter currentMinute 0

          priceAfter <- requireStoredCandle connection 60 bucketStart
          volumeAfter <- requireStoredVolume connection 60 bucketStart
          scFinalized priceAfter `shouldBe` True
          svFinalized volumeAfter `shouldBe` True
          scRevision priceAfter `shouldBe` scRevision priceBefore
          svRevision volumeAfter `shouldBe` svRevision volumeBefore
          priceCoverage <- requirePriceCoverage connection 60
          volumeCoverage <- requireVolumeCoverage connection 60
          rcFinalizedThrough priceCoverage `shouldBe` Just currentMinute
          rcFinalizedThrough volumeCoverage `shouldBe` Just currentMinute
          rcGeneration priceCoverage `shouldBe` initialGeneration
          rcGeneration volumeCoverage `shouldBe` initialGeneration
          after <-
            getCurrentBasketCandle
              connection testSeries testChainId testRouter 60 publishTime
          case ccCandle after of
            Just row -> do
              bcrPriceComplete row `shouldBe` True
              bcrVolumeComplete row `shouldBe` True
            Nothing -> fail "Expected the finalized candle after watermark advance"

    it "invalidates an excessive price watermark gap only once" $
      withCandleDatabase databaseUrl $ \pool ->
        withDb pool $ \connection -> do
          ensureCurrentBasketDefinition connection testSeries
          let coverageStart = baseTime
              coverageEnd = baseTime + 86_400
              initialGeneration = 7
              checkedThrough = coverageEnd + 301
          forM_ canonicalCandleIntervals $ \interval ->
            putPriceCoverage
              connection
              interval
              coverageStart
              coverageEnd
              coverageEnd
              initialGeneration
              True

          advanceBasketPriceCoverage
            connection testSeries checkedThrough 120

          invalidated <-
            mapM (requirePriceCoverage connection) canonicalCandleIntervals
          forM_ invalidated $ \coverage -> do
            rcComplete coverage `shouldBe` False
            rcGeneration coverage `shouldBe` initialGeneration + 1
            rcLastError coverage `shouldBe` Just "price_watermark_gap"
            rcCoverageStart coverage `shouldBe` Just coverageStart
            rcCoverageEnd coverage `shouldBe` Just coverageEnd
            rcFinalizedThrough coverage `shouldBe` Just coverageEnd

          advanceBasketPriceCoverage
            connection testSeries (checkedThrough + 300) 120

          repeated <-
            mapM (requirePriceCoverage connection) canonicalCandleIntervals
          repeated `shouldBe` invalidated

    it "recovers only an exact price watermark gap backed by the stored signed state" $
      withCandleDatabase databaseUrl $ \pool ->
        withDb pool $ \connection -> do
          ensureCurrentBasketDefinition connection testSeries
          let coverageStart = baseTime
              coverageEnd = baseTime + 86_400
              lastObservationTime = coverageEnd - 3_600
              invalidatingPoll = coverageEnd + 301
              recoveredThrough = coverageEnd + 3_600
              initialGeneration = 17
          changed <-
            upsertBasketObservation connection $
              BasketObservationInput
                { boiSeriesId = testSeries
                , boiObservationId = "closed-price-gap-last-signed"
                , boiPublishTime = lastObservationTime
                , boiBasketPrice = 100
                , boiComponentPrices = componentPayload
                , boiSource = "backend_hermes_latest"
                , boiSourcePriority = 100
                }
          changed `shouldBe` True
          forM_ canonicalCandleIntervals $ \interval ->
            putPriceCoverage
              connection
              interval
              coverageStart
              coverageEnd
              coverageEnd
              initialGeneration
              True
          advanceBasketPriceCoverage connection testSeries invalidatingPoll 120

          recovery <-
            withTransaction connection $
              recoverBasketPriceCoverageGap
                connection
                testSeries
                coverageEnd
                recoveredThrough
                lastObservationTime
                100
                componentPayload
                120
          recovery
            `shouldBe`
              PriceGapRecoveryResult
                { pgrPreviousCoverageEnd = coverageEnd
                , pgrRecoveredThrough = recoveredThrough
                , pgrGeneration = initialGeneration + 2
                }
          forM_ canonicalCandleIntervals $ \interval -> do
            coverage <- requirePriceCoverage connection interval
            rcComplete coverage `shouldBe` True
            rcGeneration coverage `shouldBe` initialGeneration + 2
            rcLastError coverage `shouldBe` Nothing
            rcCoverageEnd coverage `shouldBe` Just (alignDownForTest recoveredThrough interval)

          recoverBasketPriceCoverageGap
            connection
            testSeries
            coverageEnd
            (recoveredThrough + 60)
            lastObservationTime
            100
            componentPayload
            120
            `shouldThrow` anyException

    it "invalidates complete coarser coverage despite a minute repair marker" $
      withCandleDatabase databaseUrl $ \pool ->
        withDb pool $ \connection -> do
          ensureCurrentBasketDefinition connection testSeries
          let coverageStart = baseTime
              coverageEnd = baseTime + 86_400
              initialGeneration = 9
              checkedThrough = coverageEnd + 301
          forM_ canonicalCandleIntervals $ \interval ->
            putPriceCoverage
              connection
              interval
              coverageStart
              coverageEnd
              coverageEnd
              initialGeneration
              True
          void $
            execute
              connection
              "UPDATE perps_rollup_coverage SET generation = generation + 1, complete = FALSE, \
              \last_error = 'bounded_admin_repair', maintenance_from = ?, maintenance_to = ? \
              \WHERE kind = 'price' AND series_id = ? AND interval_seconds = 60"
              (coverageStart, coverageEnd, testSeries)

          advanceBasketPriceCoverage
            connection testSeries checkedThrough 120

          invalidated <-
            mapM (requirePriceCoverage connection) canonicalCandleIntervals
          forM_ invalidated $ \coverage -> do
            rcComplete coverage `shouldBe` False
            rcGeneration coverage `shouldBe` initialGeneration + 2
            rcLastError coverage `shouldBe` Just "price_watermark_gap"

    it "rejects a trailing 24-hour verification range without a full UTC day" $
      withCandleAdminDatabase databaseUrl $ \pool -> do
        let verificationFrom = baseTime + 13 * 3_600 + 53 * 60
            verificationTo = verificationFrom + 86_400
        seedCandleAdminSources pool verificationFrom

        (exitCode, stdout, stderr) <-
          runCandleAdmin databaseUrl
            [ "verify"
            , "all"
            , "--from", show verificationFrom
            , "--to", show verificationTo
            ]

        exitCode `shouldSatisfy` (/= ExitSuccess)
        (stdout <> stderr)
          `shouldContain` "Requested range does not contain a full aligned bucket for every canonical interval"

    it "uses a release activation minute only after the matching indexer cursor is certified" $
      withCandleAdminDatabase databaseUrl $ \pool -> do
        let activationTimestamp = baseTime + 17
            activationMinute = baseTime + 60
            firstEventTimestamp = baseTime + 2 * 86_400 + 5
            firstEventMinute = firstEventTimestamp - 5
            requestedTo = firstEventMinute + 60
        seedCertifiedCandleAdminVolumeSource
          pool activationTimestamp firstEventTimestamp

        (uncertifiedExit, uncertifiedStdout, uncertifiedStderr) <-
          runCandleAdminWithRouter
            databaseUrl
            certifiedVolumeRouter
            [ "estimate"
            , "--from", show baseTime
            , "--to", show requestedTo
            ]
        uncertifiedExit `shouldBe` ExitSuccess
        uncertifiedStderr `shouldBe` ""
        uncertifiedStdout
          `shouldContain` ("\"from_timestamp\":" <> show firstEventMinute)

        withDb pool $ \connection ->
          setPerpsIndexerState
            connection
            testChainId
            certifiedVolumeIndexerName
            certifiedVolumeRouter
            certifiedVolumeActivationBlock
            (certifiedVolumeActivationBlock + 100)
            (Just $ "0x" <> Text.replicate 64 "d")

        (certifiedExit, certifiedStdout, certifiedStderr) <-
          runCandleAdminWithRouter
            databaseUrl
            certifiedVolumeRouter
            [ "estimate"
            , "--from", show baseTime
            , "--to", show requestedTo
            ]
        certifiedExit `shouldBe` ExitSuccess
        certifiedStderr `shouldBe` ""
        certifiedStdout
          `shouldContain` ("\"from_timestamp\":" <> show activationMinute)

    it "uses the exact writer lock and releases it with a read-only transaction" $
      withCandleAdminDatabase databaseUrl $ \pool -> do
        holderReady <- newEmptyMVar
        releaseHolder <- newEmptyMVar
        holderDone <- newEmptyMVar
        _ <-
          forkFinally
            ( withDb pool $ \holder ->
                withTransactionMode
                  ( TransactionMode
                      { isolationLevel = RepeatableRead
                      , readWriteMode = ReadOnly
                      }
                  )
                  holder $ do
                    lockBasketPriceDataset holder defaultBasketSeriesId
                    readOnlyRows <-
                      query holder "SELECT current_setting('transaction_read_only')" ()
                        :: IO [Only Text]
                    readOnlyRows `shouldBe` [Only "on"]
                    putMVar holderReady $ Right ()
                    takeMVar releaseHolder
                    fail "intentional rollback after releasing the probe lock"
            )
            ( \result -> do
                case result of
                  Left err -> void $ tryPutMVar holderReady $ Left err
                  Right () -> pure ()
                putMVar holderDone result
            )
        readyResult <- timeout 5_000_000 $ takeMVar holderReady
        case readyResult of
          Nothing -> do
            void $ tryPutMVar releaseHolder ()
            expectationFailure "Timed out waiting for the price dataset lock holder"
          Just result ->
            (result :: Either SomeException ()) `shouldSatisfy` isRight
        contended <-
          ( try
              ( withDb pool $ \contender ->
                  withTransaction contender $ do
                    void $ execute_ contender "SET LOCAL lock_timeout = '100ms'"
                    advanceBasketPriceCoverage
                      contender defaultBasketSeriesId baseTime 120
              ) :: IO (Either SqlError ())
          ) `finally` putMVar releaseHolder ()
        case contended of
          Left err -> sqlState err `shouldBe` "55P03"
          Right () -> expectationFailure "The live writer unexpectedly acquired the held price dataset lock"
        holderResult <- timeout 5_000_000 $ takeMVar holderDone
        holderResult `shouldSatisfy` maybe False isLeft
        reacquired <-
          try
            ( withDb pool $ \contender ->
                withTransaction contender $ do
                  void $ execute_ contender "SET LOCAL lock_timeout = '1s'"
                  lockBasketPriceDataset contender defaultBasketSeriesId
            ) :: IO (Either SomeException ())
        reacquired `shouldSatisfy` isRight

    it "observes a writer commit that completed while the probe waited for the lock" $
      withCandleAdminDatabase databaseUrl $ \pool -> do
        let initialGeneration = 31
            committedGeneration = initialGeneration + 1
        withDb pool $ \connection -> do
          ensureCurrentBasketDefinition connection defaultBasketSeriesId
          upsertRollupCoverage connection $
            RollupCoverage
              { rcKind = PriceRollup
              , rcSeriesId = Just defaultBasketSeriesId
              , rcChainId = Nothing
              , rcReleaseRouter = Nothing
              , rcIntervalSeconds = 3_600
              , rcCoverageStart = Just baseTime
              , rcCoverageEnd = Just $ baseTime + 7_200
              , rcFinalizedThrough = Just $ baseTime + 3_600
              , rcGeneration = initialGeneration
              , rcComplete = True
              , rcDerivationVersion = "v1"
              , rcLastError = Nothing
              , rcMaintenanceFrom = Nothing
              , rcMaintenanceTo = Nothing
              }

        writerReady <- newEmptyMVar
        releaseWriter <- newEmptyMVar
        writerDone <- newEmptyMVar
        _ <-
          forkFinally
            ( withDb pool $ \writer ->
                withTransaction writer $ do
                  lockBasketPriceDataset writer defaultBasketSeriesId
                  void $
                    execute writer
                      "UPDATE perps_rollup_coverage SET generation = ? \
                      \WHERE kind = 'price' AND series_id = ? \
                      \AND chain_id = 0 AND release_router = '' AND interval_seconds = 3600"
                      (committedGeneration, defaultBasketSeriesId)
                  putMVar writerReady $ Right ()
                  takeMVar releaseWriter
            )
            ( \result -> do
                case result of
                  Left err -> void $ tryPutMVar writerReady $ Left err
                  Right () -> pure ()
                putMVar writerDone result
            )
        readyResult <- timeout 5_000_000 $ takeMVar writerReady
        case readyResult of
          Nothing -> do
            void $ tryPutMVar releaseWriter ()
            expectationFailure "Timed out waiting for the simulated writer"
          Just result ->
            (result :: Either SomeException ()) `shouldSatisfy` isRight
        _ <- forkIO $ threadDelay 200_000 >> putMVar releaseWriter ()

        observedGeneration <-
          withDb pool $ \probe ->
            withTransactionMode
              ( TransactionMode
                  { isolationLevel = ReadCommitted
                  , readWriteMode = ReadOnly
                  }
              )
              probe $ do
                lockBasketPriceDataset probe defaultBasketSeriesId
                rows <-
                  query probe
                    "SELECT generation FROM perps_rollup_coverage \
                    \WHERE kind = 'price' AND series_id = ? \
                    \AND chain_id = 0 AND release_router = '' AND interval_seconds = 3600"
                    (Only defaultBasketSeriesId) :: IO [Only Integer]
                pure rows
        observedGeneration `shouldBe` [Only committedGeneration]
        writerResult <- timeout 5_000_000 $ takeMVar writerDone
        writerResult `shouldSatisfy` maybe False isRight

    it "rejects unsafe finalizer-probe CLI shapes before database access" $ do
      (missingCode, missingOut, missingErr) <-
        runCandleAdmin databaseUrl ["finalizer-probe"]
      missingCode `shouldSatisfy` (/= ExitSuccess)
      (missingOut <> missingErr) `shouldContain` "requires --boundary"

      (unalignedCode, unalignedOut, unalignedErr) <-
        runCandleAdmin databaseUrl ["finalizer-probe", "--boundary", "3601"]
      unalignedCode `shouldSatisfy` (/= ExitSuccess)
      (unalignedOut <> unalignedErr) `shouldContain` "aligned to a UTC hour"

      (unknownCode, unknownOut, unknownErr) <-
        runCandleAdmin
          databaseUrl
          ["finalizer-probe", "--boundary", "3600", "--from", "1"]
      unknownCode `shouldSatisfy` (/= ExitSuccess)
      (unknownOut <> unknownErr) `shouldContain` "Unknown finalizer-probe option"

    it "verifies a range containing one full UTC day" $
      withCandleAdminDatabase databaseUrl $ \pool -> do
        let verificationFrom = baseTime
            verificationTo = baseTime + 86_400
        seedCandleAdminVerificationRange pool verificationFrom verificationTo

        (exitCode, _, _) <-
          runCandleAdmin databaseUrl
            [ "verify"
            , "all"
            , "--from", show verificationFrom
            , "--to", show verificationTo
            ]

        exitCode `shouldBe` ExitSuccess

    it "preserves live price watermarks when admin backfill extends only backward" $
      withCandleAdminDatabase databaseUrl $ \pool -> do
        let backfillFrom = baseTime - 86_400
            coverageStart = baseTime
            finalizedThrough = baseTime + 86_400
            liveCoverageEnd = baseTime + 172_800
            initialGeneration = 7
            checkedThrough = liveCoverageEnd + 60
        withDb pool $ \connection -> do
          ensureCurrentBasketDefinition connection defaultBasketSeriesId
          insertCandleAdminPriceObservation
            connection "candle-admin-backward-oldest" (backfillFrom + 5)
          insertCandleAdminPriceObservation
            connection "candle-admin-backward-boundary" (coverageStart - 1)
          putCandleAdminPriceCoverage
            connection
            coverageStart
            liveCoverageEnd
            finalizedThrough
            initialGeneration

        (exitCode, _, _) <-
          runCandleAdmin databaseUrl
            [ "backfill"
            , "price"
            , "--from", show backfillFrom
            , "--to", show coverageStart
            , "--chunk-seconds", "86400"
            , "--throttle-ms", "0"
            ]
        exitCode `shouldBe` ExitSuccess

        withDb pool $ \connection -> do
          published <-
            mapM
              (requirePriceCoverageForSeries connection defaultBasketSeriesId)
              canonicalCandleIntervals
          forM_ published $ \coverage -> do
            rcCoverageStart coverage `shouldBe` Just backfillFrom
            rcCoverageEnd coverage `shouldBe` Just liveCoverageEnd
            rcFinalizedThrough coverage `shouldBe` Just finalizedThrough
            rcGeneration coverage `shouldBe` initialGeneration + 1
            rcComplete coverage `shouldBe` True

          -- The next successful poll is only one minute beyond the live
          -- watermark. Rewinding coverage_end to finalized_through would make
          -- this look like a one-day gap and disable the whole price dataset.
          advanceBasketPriceCoverage
            connection defaultBasketSeriesId checkedThrough 0
          afterPoll <-
            mapM
              (requirePriceCoverageForSeries connection defaultBasketSeriesId)
              canonicalCandleIntervals
          forM_ afterPoll $ \coverage -> do
            rcComplete coverage `shouldBe` True
            rcGeneration coverage `shouldBe` initialGeneration + 1
            rcLastError coverage `shouldBe` Nothing

    it "preserves coverage_end while publishing inside the unfinalized tail" $
      withCandleAdminDatabase databaseUrl $ \pool -> do
        let coverageStart = baseTime
            finalizedWatermark = baseTime + 90_000
            liveWatermark = baseTime + 180_000
            publishedThrough = finalizedWatermark + 3_600
            initialGeneration = 11
        withDb pool $ \connection -> do
          ensureCurrentBasketDefinition connection defaultBasketSeriesId
          insertCandleAdminPriceObservation
            connection "candle-admin-forward-tail-oldest" (finalizedWatermark + 5)
          insertCandleAdminPriceObservation
            connection "candle-admin-forward-tail-newest" (publishedThrough - 1)
          putCandleAdminPriceCoverage
            connection coverageStart liveWatermark finalizedWatermark initialGeneration

        (exitCode, _, _) <-
          runCandleAdmin databaseUrl
            [ "backfill"
            , "price"
            , "--from", show finalizedWatermark
            , "--to", show publishedThrough
            , "--chunk-seconds", "3600"
            , "--throttle-ms", "0"
            ]
        exitCode `shouldBe` ExitSuccess

        withDb pool $ \connection ->
          forM_ canonicalCandleIntervals $ \interval -> do
            coverage <-
              requirePriceCoverageForSeries
                connection defaultBasketSeriesId interval
            rcCoverageStart coverage
              `shouldBe` Just (alignUpForTest coverageStart interval)
            rcCoverageEnd coverage
              `shouldBe` Just (alignDownForTest liveWatermark interval)
            rcFinalizedThrough coverage
              `shouldBe` Just
                ( max
                    (alignDownForTest finalizedWatermark interval)
                    (alignDownForTest publishedThrough interval)
                )
            rcGeneration coverage `shouldBe` initialGeneration + 1
            rcComplete coverage `shouldBe` True

    it "creates missing coarse coverage from the merged minute envelope" $
      withCandleAdminDatabase databaseUrl $ \pool -> do
        let coverageStart = baseTime + 15 * 3_600
            backfillFrom = coverageStart - 172_800
            finalizedWatermark = coverageStart + 3_600
            liveWatermark = coverageStart + 5_400
            dailyStart = baseTime - 86_400
            dailyEnd = baseTime
            initialGeneration = 15
        withDb pool $ \connection -> do
          ensureCurrentBasketDefinition connection defaultBasketSeriesId
          insertCandleAdminPriceObservation
            connection "candle-admin-missing-coarse-oldest" (backfillFrom + 5)
          insertCandleAdminPriceObservation
            connection "candle-admin-missing-coarse-boundary" (coverageStart - 1)
          forM_ (init canonicalCandleIntervals) $ \interval ->
            upsertRollupCoverage connection $
              RollupCoverage
                { rcKind = PriceRollup
                , rcSeriesId = Just defaultBasketSeriesId
                , rcChainId = Nothing
                , rcReleaseRouter = Nothing
                , rcIntervalSeconds = interval
                , rcCoverageStart = Just $ alignUpForTest coverageStart interval
                , rcCoverageEnd = Just $ alignDownForTest liveWatermark interval
                , rcFinalizedThrough = Just $ alignDownForTest finalizedWatermark interval
                , rcGeneration = initialGeneration
                , rcComplete = True
                , rcDerivationVersion = "v1"
                , rcLastError = Nothing
                , rcMaintenanceFrom = Nothing
                , rcMaintenanceTo = Nothing
                }

        (exitCode, _, _) <-
          runCandleAdmin databaseUrl
            [ "backfill"
            , "price"
            , "--from", show backfillFrom
            , "--to", show coverageStart
            , "--chunk-seconds", "86400"
            , "--throttle-ms", "0"
            ]
        exitCode `shouldBe` ExitSuccess

        withDb pool $ \connection -> do
          daily <-
            requirePriceCoverageForSeries
              connection defaultBasketSeriesId 86_400
          rcCoverageStart daily `shouldBe` Just dailyStart
          rcCoverageEnd daily `shouldBe` Just dailyEnd
          rcFinalizedThrough daily `shouldBe` Just dailyEnd
          rcGeneration daily `shouldBe` initialGeneration + 2
          rcComplete daily `shouldBe` True

    it "keeps mixed multi-chunk forward and backward publication contiguous" $
      withCandleAdminDatabase databaseUrl $ \pool -> do
        let backfillFrom = baseTime - 172_800
            coverageStart = baseTime
            finalizedWatermark = baseTime + 172_800
            liveWatermark = finalizedWatermark + 90_000
            forwardTo = finalizedWatermark + 172_800
            initialGeneration = 19
            publishedGeneration = initialGeneration + 4
            checkedThrough = forwardTo + 60
        withDb pool $ \connection -> do
          ensureCurrentBasketDefinition connection defaultBasketSeriesId
          insertCandleAdminPriceObservation
            connection "candle-admin-mixed-oldest" (backfillFrom + 5)
          insertCandleAdminPriceObservation
            connection "candle-admin-mixed-newest" (forwardTo - 1)
          putCandleAdminPriceCoverage
            connection coverageStart liveWatermark finalizedWatermark initialGeneration

        (exitCode, _, _) <-
          runCandleAdmin databaseUrl
            [ "backfill"
            , "price"
            , "--from", show backfillFrom
            , "--to", show forwardTo
            , "--chunk-seconds", "86400"
            , "--throttle-ms", "0"
            ]
        exitCode `shouldBe` ExitSuccess

        withDb pool $ \connection -> do
          published <-
            mapM
              (requirePriceCoverageForSeries connection defaultBasketSeriesId)
              canonicalCandleIntervals
          forM_ published $ \coverage -> do
            let interval = rcIntervalSeconds coverage
            rcCoverageStart coverage
              `shouldBe` Just (alignUpForTest backfillFrom interval)
            rcCoverageEnd coverage
              `shouldBe` Just (alignDownForTest forwardTo interval)
            rcFinalizedThrough coverage
              `shouldBe` Just (alignDownForTest forwardTo interval)
            rcGeneration coverage `shouldBe` publishedGeneration
            rcComplete coverage `shouldBe` True

          advanceBasketPriceCoverage
            connection defaultBasketSeriesId checkedThrough 0
          afterPoll <-
            mapM
              (requirePriceCoverageForSeries connection defaultBasketSeriesId)
              canonicalCandleIntervals
          forM_ afterPoll $ \coverage -> do
            rcComplete coverage `shouldBe` True
            rcGeneration coverage `shouldBe` publishedGeneration
            rcLastError coverage `shouldBe` Nothing

    it "refuses a price watermark when the immutable basket definition conflicts" $
      withCandleDatabase databaseUrl $ \pool ->
        withDb pool $ \connection -> do
          ensureCurrentBasketDefinition connection testSeries
          putPriceCoverage
            connection 60 baseTime (baseTime + 60) (baseTime + 60) 7 True
          void $
            execute
              connection
              "UPDATE perps_basket_definitions \
              \SET configuration_hash = 'sha256:' || repeat('0', 64) \
              \WHERE series_id = ?"
              (Only testSeries)

          advanceBasketPriceCoverage
            connection testSeries (baseTime + 120) 0
            `shouldThrow` anyException

          coverage <- requirePriceCoverage connection 60
          rcCoverageEnd coverage `shouldBe` Just (baseTime + 60)
          rcFinalizedThrough coverage `shouldBe` Just (baseTime + 60)
          rcGeneration coverage `shouldBe` 7

    it "advances one shared generation when only the minute interval was finalized" $
      withCandleDatabase databaseUrl $ \pool ->
        withDb pool $ \connection -> do
          ensureCurrentBasketDefinition connection testSeries
          changed <-
            upsertBasketObservation connection $
              observation "correctable" (baseTime + 10) 100 "signed_pyth" 50
          changed `shouldBe` True
          recomputeBasketCandleHierarchy connection testSeries (baseTime + 10) 0
          forM_ canonicalCandleIntervals $ \interval -> do
            let finalizedThrough =
                  if interval == 60
                    then baseTime + 60
                    else baseTime
            putPriceCoverage
              connection interval baseTime (baseTime + interval) finalizedThrough 5 True
            coverage <- requirePriceCoverage connection interval
            rcFinalizedThrough coverage `shouldBe` Just finalizedThrough

          corrected <-
            upsertBasketObservation connection $
              observation "correctable" (baseTime + 10) 125 "signed_pyth_corrected" 100
          corrected `shouldBe` True
          recomputeBasketCandleHierarchy connection testSeries (baseTime + 10) 0
          forM_ canonicalCandleIntervals $ \interval -> do
            candle <- requireStoredCandle connection interval baseTime
            scClose candle `shouldBe` 125
            scRevision candle `shouldBe` 2
            coverage <- requirePriceCoverage connection interval
            rcGeneration coverage `shouldBe` 6

          replayed <-
            upsertBasketObservation connection $
              observation "correctable" (baseTime + 10) 125 "signed_pyth_corrected" 100
          replayed `shouldBe` False
          -- The recomputation primitive itself must also be idempotent, even
          -- if a defensive caller invokes it after a duplicate delivery.
          recomputeBasketCandleHierarchy connection testSeries (baseTime + 10) 0
          forM_ canonicalCandleIntervals $ \interval -> do
            candle <- requireStoredCandle connection interval baseTime
            scRevision candle `shouldBe` 2
            coverage <- requirePriceCoverage connection interval
            rcGeneration coverage `shouldBe` 6

    it "preserves pre-rewind volume and removes orphaned trades across every resolution" $
      withCandleDatabase databaseUrl $ \pool ->
        withDb pool $ \connection -> do
          let priorDay = baseTime - 86_400
          insertActivity connection "reorg-prior-day-kept" (priorDay + 5) 198 7 13 "Open"
          insertActivity connection "reorg-kept" (baseTime + 5) 199 2 10 "Open"
          insertActivity connection "reorg-minute-kept" (baseTime + 65) 200 4 12 "Open"
          insertActivity connection "reorg-removed" (baseTime + 65) 201 3 11 "Close"
          _ <-
            backfillMarketVolume
              connection testChainId testRouter priorDay (baseTime + 180)
          forM_ canonicalCandleIntervals $ \interval ->
            putVolumeCoverage
              connection interval priorDay (baseTime + 86_400) (baseTime + 86_400) 9 True

          affected <-
            invalidateMarketVolumeFromBlock
              connection testChainId testRouter 201
          affected `shouldBe` [baseTime + 60]
          -- The unaffected minute survives immediately. Every parent that
          -- mixed it with the orphaned trade is invalidated until rebuilt.
          keptMinute <- requireStoredVolume connection 60 baseTime
          keptMinute
            `shouldBe` StoredVolume baseTime 20 1 199 199 1 True
          forM_ canonicalCandleIntervals $ \interval -> do
            prior <- requireStoredVolume connection interval priorDay
            prior
              `shouldBe` StoredVolume priorDay 91 1 198 198 1 True
          storedVolumes connection 60 (baseTime + 60) (baseTime + 120)
            `shouldReturn` []
          forM_ (drop 1 canonicalCandleIntervals) $ \interval ->
            storedVolumes connection interval baseTime (baseTime + interval)
              `shouldReturn` []
          forM_ canonicalCandleIntervals $ \interval -> do
            coverage <- requireVolumeCoverage connection interval
            rcComplete coverage `shouldBe` False
            rcGeneration coverage `shouldBe` 10
            rcLastError coverage `shouldBe` Just "chain_reorg"
            rcFinalizedThrough coverage `shouldBe` Just priorDay

          -- This is the production rewind ordering: invalidate rollups first,
          -- delete orphaned canonical history, then rebuild each old affected
          -- minute. The affected minute and its parents are reconstructed from
          -- only the retained pre-rewind trades.
          deletePerpsHistoryFromBlock connection testChainId testRouter 201
          forM_ affected $ \minute ->
            recomputeMarketVolumeHierarchy
              connection testChainId testRouter minute 0
          forM_ (drop 1 canonicalCandleIntervals) $ \interval -> do
            rebuilt <- requireStoredVolume connection interval baseTime
            rebuilt
              `shouldBe` StoredVolume baseTime 68 2 199 200 1 True
          rebuiltMinute <- requireStoredVolume connection 60 (baseTime + 60)
          rebuiltMinute
            `shouldBe` StoredVolume (baseTime + 60) 48 1 200 200 1 True

          -- Replays commonly restart before the retained coverage inception.
          -- The recovery watermark must clamp to coverage_start instead of
          -- violating the metadata CHECK or prematurely republishing reads.
          advanceMarketVolumeCoverage
            connection testChainId testRouter (baseTime - 60) 0
          forM_ canonicalCandleIntervals $ \interval -> do
            coverage <- requireVolumeCoverage connection interval
            rcFinalizedThrough coverage `shouldBe` Just (baseTime - interval)
            rcComplete coverage `shouldBe` False
            rcGeneration coverage `shouldBe` 10

          advanceMarketVolumeCoverage
            connection testChainId testRouter (baseTime + 86_400) 0
          forM_ canonicalCandleIntervals $ \interval -> do
            coverage <- requireVolumeCoverage connection interval
            rcComplete coverage `shouldBe` True
            rcGeneration coverage `shouldBe` 10
            rcLastError coverage `shouldBe` Nothing
            retained <- requireStoredVolume connection interval baseTime
            let expectedNumerator = if interval == 60 then 20 else 68
                expectedTradeCount = if interval == 60 then 1 else 2
            svNumerator retained `shouldBe` expectedNumerator
            svTradeCount retained `shouldBe` expectedTradeCount
            svFirstBlock retained `shouldBe` 199
            svLastBlock retained `shouldBe` if interval == 60 then 199 else 200
            prior <- requireStoredVolume connection interval priorDay
            prior
              `shouldBe` StoredVolume priorDay 91 1 198 198 1 True

    it "marks the exact price and volume coverage identities incomplete" $
      withCandleDatabase databaseUrl $ \pool ->
        withDb pool $ \connection -> do
          putPriceCoverage connection 60 baseTime (baseTime + 60) (baseTime + 60) 7 True
          putVolumeCoverage connection 60 baseTime (baseTime + 60) (baseTime + 60) 11 True

          priceGeneration <-
            markRollupCoverageIncomplete
              connection PriceRollup (Just testSeries) Nothing Nothing 60 "price_repair"
          volumeGeneration <-
            markRollupCoverageIncomplete
              connection VolumeRollup Nothing (Just testChainId) (Just testRouter) 60 "volume_repair"

          priceGeneration `shouldBe` 8
          volumeGeneration `shouldBe` 12
          priceCoverage <- requirePriceCoverage connection 60
          volumeCoverage <- requireVolumeCoverage connection 60
          rcComplete priceCoverage `shouldBe` False
          rcComplete volumeCoverage `shouldBe` False
          rcLastError priceCoverage `shouldBe` Just "price_repair"
          rcLastError volumeCoverage `shouldBe` Just "volume_repair"

    it "persists exact maintenance bounds across every interval until publication" $
      withCandleDatabase databaseUrl $ \pool ->
        withDb pool $ \connection -> do
          forM_ canonicalCandleIntervals $ \interval ->
            putPriceCoverage
              connection interval baseTime (baseTime + 86_400) (baseTime + 86_400) 7 True

          let maintenanceFrom = baseTime + 3_600
              maintenanceTo = baseTime + 7_200
          generation <- beginRollupMaintenance
            connection PriceRollup (Just testSeries) Nothing Nothing
            maintenanceFrom maintenanceTo
          generation `shouldBe` 8

          forM_ canonicalCandleIntervals $ \interval -> do
            coverage <- requirePriceCoverage connection interval
            rcComplete coverage `shouldBe` False
            rcGeneration coverage `shouldBe` 8
            rcLastError coverage `shouldBe` Just "bounded_admin_repair"
            rcMaintenanceFrom coverage `shouldBe` Just maintenanceFrom
            rcMaintenanceTo coverage `shouldBe` Just maintenanceTo

          resumedGeneration <- beginRollupMaintenance
            connection PriceRollup (Just testSeries) Nothing Nothing
            maintenanceFrom maintenanceTo
          resumedGeneration `shouldBe` 8
          beforeDifferentRange <-
            mapM (requirePriceCoverage connection) canonicalCandleIntervals
          beginRollupMaintenance
            connection PriceRollup (Just testSeries) Nothing Nothing
            baseTime (baseTime + 60)
            `shouldThrow` anyException
          afterDifferentRange <-
            mapM (requirePriceCoverage connection) canonicalCandleIntervals
          afterDifferentRange `shouldBe` beforeDifferentRange

          -- A complete publication explicitly clears maintenance metadata.
          putPriceCoverage
            connection 60 baseTime (baseTime + 86_400) (baseTime + 86_400) 8 True
          published <- requirePriceCoverage connection 60
          rcMaintenanceFrom published `shouldBe` Nothing
          rcMaintenanceTo published `shouldBe` Nothing
          beforeMixedState <-
            mapM (requirePriceCoverage connection) canonicalCandleIntervals
          beginRollupMaintenance
            connection PriceRollup (Just testSeries) Nothing Nothing
            maintenanceFrom maintenanceTo
            `shouldThrow` anyException
          afterMixedState <-
            mapM (requirePriceCoverage connection) canonicalCandleIntervals
          afterMixedState `shouldBe` beforeMixedState
          mixedPublished <- requirePriceCoverage connection 60
          mixedRepairing <- requirePriceCoverage connection 180
          rcComplete mixedPublished `shouldBe` True
          rcGeneration mixedPublished `shouldBe` 8
          rcMaintenanceFrom mixedPublished `shouldBe` Nothing
          rcComplete mixedRepairing `shouldBe` False
          rcGeneration mixedRepairing `shouldBe` 8
          rcMaintenanceFrom mixedRepairing `shouldBe` Just maintenanceFrom
          rcMaintenanceTo mixedRepairing `shouldBe` Just maintenanceTo

    it "rejects partial coverage sets and invalid maintenance bounds atomically" $
      withCandleDatabase databaseUrl $ \pool ->
        withDb pool $ \connection -> do
          putVolumeCoverage connection 60 baseTime (baseTime + 60) (baseTime + 60) 3 True
          beginRollupMaintenance
            connection VolumeRollup Nothing (Just testChainId) (Just testRouter)
            baseTime (baseTime + 60)
            `shouldThrow` anyException
          partial <- requireVolumeCoverage connection 60
          rcMaintenanceFrom partial `shouldBe` Nothing
          rcMaintenanceTo partial `shouldBe` Nothing
          beginRollupMaintenance
            connection PriceRollup (Just testSeries) Nothing Nothing
            (baseTime + 1) (baseTime + 60)
            `shouldThrow` anyException

    it "rejects generation-skewed canonical coverage atomically" $
      withCandleDatabase databaseUrl $ \pool ->
        withDb pool $ \connection -> do
          forM_ canonicalCandleIntervals $ \interval ->
            putPriceCoverage
              connection interval baseTime (baseTime + 86_400) (baseTime + 86_400)
              (if interval == 900 then 8 else 7) True
          before <- mapM (requirePriceCoverage connection) canonicalCandleIntervals

          beginRollupMaintenance
            connection PriceRollup (Just testSeries) Nothing Nothing
            baseTime (baseTime + 60)
            `shouldThrow` anyException

          after <- mapM (requirePriceCoverage connection) canonicalCandleIntervals
          after `shouldBe` before

    it "enforces bounded-maintenance marker and bounds as one database state" $
      withCandleDatabase databaseUrl $ \pool ->
        withDb pool $ \connection -> do
          putPriceCoverage connection 60 baseTime (baseTime + 60) (baseTime + 60) 3 True
          execute
            connection
            "UPDATE perps_rollup_coverage SET complete = FALSE, last_error = NULL, \
            \maintenance_from = ?, maintenance_to = ? \
            \WHERE kind = 'price' AND series_id = ? AND interval_seconds = 60"
            (baseTime, baseTime + 60, testSeries)
            `shouldThrow` anyException
          execute
            connection
            "UPDATE perps_rollup_coverage SET complete = FALSE, \
            \last_error = 'bounded_admin_repair', \
            \maintenance_from = NULL, maintenance_to = NULL \
            \WHERE kind = 'price' AND series_id = ? AND interval_seconds = 60"
            (Only testSeries)
            `shouldThrow` anyException
          execute
            connection
            "UPDATE perps_rollup_coverage SET complete = TRUE, \
            \last_error = 'bounded_admin_repair', maintenance_from = ?, maintenance_to = ? \
            \WHERE kind = 'price' AND series_id = ? AND interval_seconds = 60"
            (baseTime, baseTime + 60, testSeries)
            `shouldThrow` anyException
          unchanged <- requirePriceCoverage connection 60
          rcComplete unchanged `shouldBe` True
          rcLastError unchanged `shouldBe` Nothing
          rcMaintenanceFrom unchanged `shouldBe` Nothing
          rcMaintenanceTo unchanged `shouldBe` Nothing

    it "allows exact maintenance resume at the maximum dataset generation" $
      withCandleDatabase databaseUrl $ \pool ->
        withDb pool $ \connection -> do
          forM_ canonicalCandleIntervals $ \interval ->
            putPriceCoverage
              connection interval baseTime (baseTime + 86_400) (baseTime + 86_400)
              67_108_862 True
          generation <-
            beginRollupMaintenance
              connection PriceRollup (Just testSeries) Nothing Nothing
              baseTime (baseTime + 60)
          generation `shouldBe` 67_108_863
          beginRollupMaintenance
            connection PriceRollup (Just testSeries) Nothing Nothing
            baseTime (baseTime + 60)
            `shouldReturn` 67_108_863

          forM_ canonicalCandleIntervals $ \interval ->
            putPriceCoverage
              connection interval baseTime (baseTime + 86_400) (baseTime + 86_400)
              67_108_863 True
          beginRollupMaintenance
            connection PriceRollup (Just testSeries) Nothing Nothing
            baseTime (baseTime + 60)
            `shouldThrow` anyException
          unchanged <- requirePriceCoverage connection 60
          rcComplete unchanged `shouldBe` True
          rcGeneration unchanged `shouldBe` 67_108_863

    it "uses bounded rollup indexes for compatibility range reads" $
      withCandleDatabase databaseUrl $ \pool ->
        withDb pool $ \connection -> do
          ensureCurrentBasketDefinition connection testSeries
          let pageRows = 12_001 :: Integer
              pageEnd = baseTime + pageRows * 60
          void $
            execute
              connection
              "INSERT INTO perps_basket_candles (\
              \ series_id, interval_seconds, bucket_start, raw_open_price, \
              \ raw_high_price, raw_low_price, raw_close_price, \
              \ first_observation_time, last_observation_time, sample_count, \
              \ quality, revision, finalized) \
              \SELECT ?, 60, bucket_start, 100, 100, 100, 100, \
              \ bucket_start + 5, bucket_start + 5, 1, 'observed', 1, TRUE \
              \FROM (SELECT ?::bigint + offset_number * 60 AS bucket_start \
              \ FROM generate_series(0, ?::bigint - 1) AS offsets(offset_number)) page"
              (testSeries, baseTime, pageRows)
          void $
            execute
              connection
              "INSERT INTO perps_market_volume_rollups (\
              \ chain_id, release_router, interval_seconds, bucket_start, \
              \ volume_numerator, trade_count, first_source_block, \
              \ last_source_block, revision, finalized) \
              \SELECT ?, ?, 60, bucket_start, 700, 1, offset_number + 1, \
              \ offset_number + 1, 1, TRUE \
              \FROM (SELECT offset_number, ?::bigint + offset_number * 60 AS bucket_start \
              \ FROM generate_series(0, ?::bigint - 1) AS offsets(offset_number)) page"
              (testChainId, normalizedTestRouter, baseTime, pageRows)
          vacuumCandlePageTables connection
          pageIndexes <-
            query_
              connection
              "SELECT index_relation.relname, index_state.indisvalid, \
              \ index_state.indisready, index_state.indislive, \
              \ index_state.indnkeyatts::int, \
              \ string_agg(attribute.attname, ',' ORDER BY ordinal.position) \
              \FROM pg_class index_relation \
              \JOIN pg_index index_state ON index_state.indexrelid = index_relation.oid \
              \JOIN pg_class target_relation ON target_relation.oid = index_state.indrelid \
              \CROSS JOIN LATERAL unnest(index_state.indkey) WITH ORDINALITY \
              \ ordinal(attribute_number, position) \
              \JOIN pg_attribute attribute \
              \ ON attribute.attrelid = target_relation.oid \
              \ AND attribute.attnum = ordinal.attribute_number \
              \WHERE index_relation.relname IN (\
              \ 'idx_perps_basket_candles_page_cover', \
              \ 'idx_perps_market_volume_rollups_page_cover') \
              \GROUP BY index_relation.relname, index_state.indisvalid, \
              \ index_state.indisready, index_state.indislive, index_state.indnkeyatts \
              \ORDER BY index_relation.relname"
              :: IO [(Text, Bool, Bool, Bool, Int, Text)]
          pageIndexes
            `shouldBe`
              [ ( "idx_perps_basket_candles_page_cover"
                , True
                , True
                , True
                , 3
                , "series_id,interval_seconds,bucket_start,raw_open_price,raw_high_price,raw_low_price,raw_close_price,sample_count,quality,revision,finalized"
                )
              , ( "idx_perps_market_volume_rollups_page_cover"
                , True
                , True
                , True
                , 4
                , "chain_id,release_router,interval_seconds,bucket_start,volume_numerator,trade_count,finalized"
                )
              ]
          plan <- withTransaction connection $ do
            _ <- execute_ connection "SET LOCAL enable_seqscan = off"
            query
              connection
              "EXPLAIN (COSTS OFF) \
              \SELECT c.bucket_start, c.raw_open_price, c.raw_high_price, \
              \ c.raw_low_price, c.raw_close_price, c.sample_count, c.quality, \
              \ c.revision, c.finalized, v.volume_numerator, v.trade_count, v.finalized \
              \FROM perps_basket_candles c \
              \LEFT JOIN perps_market_volume_rollups v \
              \ ON v.chain_id = ? AND v.release_router = ? \
              \ AND v.interval_seconds = c.interval_seconds AND v.bucket_start = c.bucket_start \
              \WHERE c.series_id = ? AND c.interval_seconds = ? \
              \AND c.bucket_start >= ? AND c.bucket_start < ? \
              \ORDER BY c.bucket_start LIMIT ?"
              ( testChainId, normalizedTestRouter, testSeries, 60 :: Integer
              , baseTime, pageEnd, 12_001 :: Int
              )
              :: IO [Only Text]
          let renderedPlan = Text.unlines [line | Only line <- plan]
              usesIndex names =
                any
                  (\name -> Text.isInfixOf ("using " <> name) renderedPlan)
                  names
          usesIndex
            [ "idx_perps_basket_candles_page_cover"
            , "perps_basket_candles_pkey"
            ]
            `shouldBe` True
          usesIndex
            [ "idx_perps_market_volume_rollups_page_cover"
            , "perps_market_volume_rollups_pkey"
            ]
            `shouldBe` True
          shouldBe
            ("Seq Scan on perps_basket_candles" `Text.isInfixOf` renderedPlan)
            False
          shouldBe
            ("Seq Scan on perps_market_volume_rollups" `Text.isInfixOf` renderedPlan)
            False

    it "refreshes the visibility map used by candle page covering indexes" $
      withCandleDatabase databaseUrl $ \pool ->
        withDb pool $ \connection -> do
          ensureCurrentBasketDefinition connection testSeries
          insertObservation connection "visibility-price" (baseTime + 5) 100 "signed_pyth" 100
          recomputeBasketCandleHierarchy connection testSeries (baseTime + 5) 0
          insertActivity connection "visibility-volume" (baseTime + 5) 701 2 10 "Open"
          _ <- backfillMarketVolume connection testChainId testRouter baseTime (baseTime + 60)
          vacuumCandlePageTables connection
          visibility <-
            query_
              connection
              "SELECT relname, relallvisible > 0 FROM pg_class \
              \WHERE relname IN ('perps_basket_candles','perps_market_volume_rollups') \
              \ORDER BY relname" :: IO [(Text, Bool)]
          visibility
            `shouldBe`
              [ ("perps_basket_candles", True)
              , ("perps_market_volume_rollups", True)
              ]

    it "uses block-number indexes for reorg discovery and history deletion" $
      withCandleDatabase databaseUrl $ \pool ->
        withDb pool $ \connection -> do
          insertActivity connection "reorg-plan-volume" baseTime 950 2 10 "Open"
          insertRawEvent connection "reorg-plan-event" baseTime 950
          (activityPlan, eventPlan) <- withTransaction connection $ do
            _ <- execute_ connection "SET LOCAL enable_seqscan = off"
            activityPlan <-
              query
                connection
                "EXPLAIN (COSTS OFF) SELECT id, timestamp FROM perps_account_activity \
                \WHERE chain_id = ? AND release_router = ? AND block_number >= ?"
                (testChainId, testRouter, 950 :: Integer) :: IO [Only Text]
            eventPlan <-
              query
                connection
                "EXPLAIN (COSTS OFF) SELECT id FROM perps_events \
                \WHERE chain_id = ? AND release_router = ? AND block_number >= ?"
                (testChainId, testRouter, 950 :: Integer) :: IO [Only Text]
            pure (activityPlan, eventPlan)
          let renderedActivityPlan = Text.unpack $ Text.unlines [line | Only line <- activityPlan]
              renderedEventPlan = Text.unpack $ Text.unlines [line | Only line <- eventPlan]
          renderedActivityPlan `shouldContain` "idx_perps_account_activity_reorg_blocks"
          renderedEventPlan `shouldContain` "idx_perps_events_reorg_blocks"

data StoredCandle = StoredCandle
  { scBucketStart :: Integer
  , scOpen :: Integer
  , scHigh :: Integer
  , scLow :: Integer
  , scClose :: Integer
  , scSamples :: Int
  , scQuality :: Text
  , scRevision :: Integer
  , scFinalized :: Bool
  }
  deriving (Eq, Show)

instance FromRow StoredCandle where
  fromRow =
    StoredCandle
      <$> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field

data StoredVolume = StoredVolume
  { svBucketStart :: Integer
  , svNumerator :: Integer
  , svTradeCount :: Integer
  , svFirstBlock :: Integer
  , svLastBlock :: Integer
  , svRevision :: Integer
  , svFinalized :: Bool
  }
  deriving (Eq, Show)

instance FromRow StoredVolume where
  fromRow = do
    bucketStart <- field
    numeratorText <- field
    tradeCount <- field
    firstBlock <- field
    lastBlock <- field
    revision <- field
    finalized <- field
    pure $
      StoredVolume
        bucketStart
        (read $ Text.unpack numeratorText)
        tradeCount
        firstBlock
        lastBlock
        revision
        finalized

withCandleDatabase :: Text -> (DbPool -> IO a) -> IO a
withCandleDatabase databaseUrl action =
  bracket (newDbPool databaseUrl) destroyAllResources $ \pool -> do
    assertDedicatedDatabase pool
    prepareCandleDatabase pool
    cleanupCandleRows pool
    action pool `finally` cleanupCandleRows pool

withRollback :: Connection -> IO a -> IO a
withRollback connection action = do
  void $ execute_ connection "BEGIN"
  action `finally` void (execute_ connection "ROLLBACK")

sqlErrorContaining :: BS8.ByteString -> SqlError -> Bool
sqlErrorContaining fragment sqlError =
  fragment `BS8.isInfixOf` sqlErrorMsg sqlError

sqlErrorState :: BS8.ByteString -> SqlError -> Bool
sqlErrorState expected sqlError = sqlState sqlError == expected

seedCandleHistoryMarket :: Connection -> IO ()
seedCandleHistoryMarket connection = do
  ensureCurrentBasketDefinition connection testSeries
  void $
    execute
      connection
      "INSERT INTO perps_candle_markets (market_id, chain_id, price_series_id) \
      \VALUES (?, ?, ?)"
      ("dxy-history-integration" :: Text, testChainId, testSeries)

insertHistoryTarget :: Connection -> Integer -> Integer -> Text -> IO ()
insertHistoryTarget connection revision requestedStart requestReference =
  void $
    execute
      connection
      "INSERT INTO perps_candle_history_targets \
      \(market_id, revision, requested_start_timestamp, requested_by, request_reference) \
      \VALUES ('dxy-history-integration', ?, ?, 'integration-test', ?)"
      (revision, requestedStart, requestReference)

insertHistoryGenesis :: Connection -> IO ()
insertHistoryGenesis connection =
  insertHistoryGenesisWithApproval connection "integration-genesis"

insertHistoryGenesisWithApproval :: Connection -> Text -> IO ()
insertHistoryGenesisWithApproval connection approvalReference =
  insertHistoryRelease
    connection
    1
    "0x1111111111111111111111111111111111111111"
    90
    100
    1_000
    True
    approvalReference

insertHistorySuccessor :: Connection -> IO ()
insertHistorySuccessor connection =
  insertHistoryRelease
    connection
    2
    "0x2222222222222222222222222222222222222222"
    190
    200
    2_000
    False
    "integration-successor"

insertHistoryRelease
  :: Connection
  -> Integer
  -> Text
  -> Integer
  -> Integer
  -> Integer
  -> Bool
  -> Text
  -> IO ()
insertHistoryRelease
  connection revision router deploymentBlock activationBlock activationTimestamp isGenesis approvalReference =
    void $
      execute
        connection
        "INSERT INTO perps_market_release_epochs (\
        \market_id, release_revision, chain_id, release_router, cfd_engine, \
        \margin_clearinghouse, deployment_block, deployment_block_hash, \
        \deployment_tx_hash, activation_block, activation_timestamp, \
        \activation_block_hash, approval_reference, is_market_genesis) VALUES (\
        \'dxy-history-integration', ?, 9999421614, ?, \
        \'0x3333333333333333333333333333333333333333', \
        \'0x4444444444444444444444444444444444444444', ?, \
        \'0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa', \
        \'0xbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb', \
        \?, ?, \
        \'0xcccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc', \
        \?, ?)"
        ( revision
        , router
        , deploymentBlock
        , activationBlock
        , activationTimestamp
        , approvalReference
        , isGenesis
        )

currentCandleResponse :: AppCache -> DbPool -> Config -> Integer -> IO SResponse
currentCandleResponse cache pool config validatedAt =
  currentCandleResponseWithHeaders cache pool config validatedAt []

currentCandleResponseFresh :: AppCache -> DbPool -> Config -> Integer -> IO SResponse
currentCandleResponseFresh cache pool config validatedAt =
  currentCandleResponseWithHeaders
    cache
    pool
    config
    validatedAt
    [("Cache-Control", "no-store"), ("Pragma", "no-cache")]

currentCandleResponseWithHeaders
  :: AppCache
  -> DbPool
  -> Config
  -> Integer
  -> RequestHeaders
  -> IO SResponse
currentCandleResponseWithHeaders cache pool config validatedAt headers = do
  application <-
    scottyApp $
      get "/api/perps/basket/candles/current" $
        handleBasketCurrentCandleAt cache config (Just pool) validatedAt
  let currentRequest =
        (setPath defaultRequest "/api/perps/basket/candles/current?interval=3600")
          { requestHeaders = headers
          }
  runSession
    (request currentRequest)
    application

currentResponseDatasetGeneration :: SResponse -> Maybe Integer
currentResponseDatasetGeneration response = do
  value <- Aeson.decode $ simpleBody response
  parseMaybe
    ( withObject "ApiResponse" $ \root -> do
        payload <- root .: "data"
        withObject "BasketCurrentCandle" (.: "datasetGeneration") payload
    )
    value

candleApiConfig :: Text -> Config
candleApiConfig databaseUrl =
  Config
    { cfgRpcUrl = ""
    , cfgChainId = 11_155_111
    , cfgPort = 3001
    , cfgCorsOrigins = []
    , cfgDeployments = []
    , cfgDatabaseUrl = Just databaseUrl
    , cfgIndexerStartBlock = 0
    , cfgPythBenchmarksUrl = ""
    , cfgPythHistoryUrl = ""
    , cfgPythHermesUrl = ""
    , cfgPythApiKey = Nothing
    , cfgPythBackfillDays = 1
    , cfgPythSampleIntervalSeconds = 60
    , cfgPythLatestMaxAgeSeconds = 10
    , cfgPythIngestionEnabled = False
    , cfgPerpsCandleWriteMode = PerpsCandleWritesDual
    , cfgPerpsCandleReadMode = PerpsCandleReadsRollup
    , cfgPerpsCandleReadIntervals = [3_600]
    , cfgPerpsCandleShadowSampleBps = 0
    , cfgPerpsCandleStrictCoverage = True
    , cfgPerpsCandleLatenessSeconds = 120
    , cfgPerpsCandleFinalizationGraceSeconds = 15
    , cfgPerpsRpcUrl = ""
    , cfgPerpsChainId = testChainId
    , cfgPerpsUsdc = ""
    , cfgPerpsOrderRouter = testRouter
    , cfgPerpsOrderLifecycleBook = Nothing
    , cfgPerpsCfdEngine = ""
    , cfgPerpsCfdEngineLens = ""
    , cfgPerpsCfdEngineSettlementSidecar = ""
    , cfgPerpsMarginClearinghouse = ""
    , cfgPerpsPletherOracle = ""
    , cfgPerpsAccountLens = ""
    , cfgPerpsHousePool = "0x86939a377A78EDe8EEe5445765ac77c9016E35E2"
    , cfgPerpsSettlementMonitorLens = "0xd251AC0BD90780c48F31F575152808315200664E"
    , cfgPerpsIndexerStartBlock = 0
    , cfgVaultHistoryHousePoolAddress = "0x86939a377A78EDe8EEe5445765ac77c9016E35E2"
    , cfgVaultHistorySeniorVaultAddress = "0xB5A9a9d634197B8F0EA7c4042CF8d5701767D710"
    , cfgVaultHistoryJuniorVaultAddress = "0xdf306B52eaC722D5994E2cc93D2818F391d68Adb"
    , cfgVaultHistoryDeploymentBlock = 0
    , cfgVaultHistoryRpcUrl = ""
    , cfgVaultHistoryConfirmations = 0
    , cfgInsightsCompetitionRules = july2026Competition
    , cfgInsightsCompetitionReleaseManifest = candleReleaseManifest
    , cfgRegistrationConfig = Nothing
    , cfgAaConfig = Nothing
    , cfgFaucetGuardConfig = Nothing
    , cfgFaucetPrivateKey = Nothing
    , cfgKeeperPrivateKey = Nothing
    , cfgKeeperPollSeconds = 1
    , cfgKeeperMaxBatchSize = 20
    , cfgKeeperConfirmations = 0
    , cfgKeeperGasBufferBps = 2_000
    , cfgKeeperFeeBufferBps = 2_500
    , cfgLpSettlementMode = LpSettlementOff
    , cfgLpSettlementPrivateKey = Nothing
    , cfgLpSettlementSeniorVault = "0xB5A9a9d634197B8F0EA7c4042CF8d5701767D710"
    , cfgLpSettlementJuniorVault = "0xdf306B52eaC722D5994E2cc93D2818F391d68Adb"
    , cfgLpSettlementPollSeconds = 15
    , cfgLpSettlementMaxDrainTransactions = 4
    , cfgLpSettlementPendingReplacementSeconds = 60
    , cfgLpSettlementMaxReplacements = 3
    , cfgLpSettlementMaxTxCostWei = 0
    }

candleReleaseManifest :: CompetitionReleaseManifest
candleReleaseManifest =
  CompetitionReleaseManifest
    { crmReleaseId = "candle-rollup-integration"
    , crmChainId = testChainId
    , crmUsdc = ""
    , crmOrderRouter = testRouter
    , crmMarginClearinghouse = ""
    , crmAccountLens = ""
    , crmCfdEngine = ""
    , crmCfdEngineLens = ""
    , crmSettlementSidecar = ""
    , crmPletherOracle = ""
    , crmIndexerStartBlock = 0
    }

withCandleAdminDatabase :: Text -> (DbPool -> IO a) -> IO a
withCandleAdminDatabase databaseUrl action =
  withCandleDatabase databaseUrl $ \pool -> do
    cleanupCandleAdminRows pool
    action pool `finally` cleanupCandleAdminRows pool

insertCandleAdminPriceObservation :: Connection -> Text -> Integer -> IO ()
insertCandleAdminPriceObservation connection observationId publishTime = do
  changed <-
    upsertBasketObservation connection $
      BasketObservationInput
        { boiSeriesId = defaultBasketSeriesId
        , boiObservationId = observationId
        , boiPublishTime = publishTime
        , boiBasketPrice = 100
        , boiComponentPrices = componentPayload
        , boiSource = "signed_pyth"
        , boiSourcePriority = 100
        }
  changed `shouldBe` True

putCandleAdminPriceCoverage
  :: Connection
  -> Integer
  -> Integer
  -> Integer
  -> Integer
  -> IO ()
putCandleAdminPriceCoverage
  connection coverageStart coverageEnd finalizedThrough generation =
    forM_ canonicalCandleIntervals $ \interval ->
      upsertRollupCoverage connection $
        RollupCoverage
          { rcKind = PriceRollup
          , rcSeriesId = Just defaultBasketSeriesId
          , rcChainId = Nothing
          , rcReleaseRouter = Nothing
          , rcIntervalSeconds = interval
          , rcCoverageStart = Just $ alignUpForTest coverageStart interval
          , rcCoverageEnd = Just $ alignDownForTest coverageEnd interval
          , rcFinalizedThrough = Just $ alignDownForTest finalizedThrough interval
          , rcGeneration = generation
          , rcComplete = True
          , rcDerivationVersion = "v1"
          , rcLastError = Nothing
          , rcMaintenanceFrom = Nothing
          , rcMaintenanceTo = Nothing
          }

seedCandleAdminSources :: DbPool -> Integer -> IO ()
seedCandleAdminSources pool rangeStart =
  withDb pool $ \connection -> do
    let sourceTimestamp = rangeStart + 60
    ensureCurrentBasketDefinition connection defaultBasketSeriesId
    changed <-
      upsertBasketObservation connection $
        BasketObservationInput
          { boiSeriesId = defaultBasketSeriesId
          , boiObservationId = "candle-admin-verification-price"
          , boiPublishTime = sourceTimestamp
          , boiBasketPrice = 100
          , boiComponentPrices = componentPayload
          , boiSource = "signed_pyth"
          , boiSourcePriority = 100
          }
    changed `shouldBe` True
    insertActivity
      connection "candle-admin-verification-volume" sourceTimestamp 1_100 2 10 "Open"
    insertRawEvent
      connection "candle-admin-verification-source-bound" sourceTimestamp 1_100

seedCertifiedCandleAdminVolumeSource :: DbPool -> Integer -> Integer -> IO ()
seedCertifiedCandleAdminVolumeSource pool activationTimestamp firstEventTimestamp =
  withDb pool $ \connection -> do
    ensureCurrentBasketDefinition connection defaultBasketSeriesId
    void $
      execute
        connection
        "INSERT INTO perps_candle_markets (market_id, chain_id, price_series_id) \
        \VALUES (?, ?, ?)"
        (defaultCandleMarketId, testChainId, defaultBasketSeriesId)
    void $
      execute
        connection
        "INSERT INTO perps_market_release_epochs (\
        \market_id, release_revision, chain_id, release_router, cfd_engine, \
        \margin_clearinghouse, deployment_block, deployment_block_hash, \
        \deployment_tx_hash, activation_block, activation_timestamp, \
        \activation_block_hash, approval_reference, is_market_genesis) VALUES (\
        \?, 1, ?, ?, \
        \'0x3333333333333333333333333333333333333333', \
        \'0x4444444444444444444444444444444444444444', ?, \
        \'0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa', \
        \'0xbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb', \
        \?, ?, \
        \'0xcccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc', \
        \'certified-volume-source-bound-integration', TRUE)"
        ( defaultCandleMarketId
        , testChainId
        , certifiedVolumeRouter
        , certifiedVolumeActivationBlock - 1
        , certifiedVolumeActivationBlock
        , activationTimestamp
        )
    insertPerpsEvent
      connection
      testChainId
      certifiedVolumeRouter
      "0x3333333333333333333333333333333333333333"
      "PositionOpened"
      ("0x" <> Text.replicate 64 "e")
      (certifiedVolumeActivationBlock + 50)
      ("0x" <> Text.replicate 64 "f")
      0
      0
      firstEventTimestamp
      Nothing
      Nothing
      Nothing
      (object ["integrationTest" .= True])

seedCandleAdminVerificationRange :: DbPool -> Integer -> Integer -> IO ()
seedCandleAdminVerificationRange pool rangeStart rangeEnd = do
  seedCandleAdminSources pool rangeStart
  withDb pool $ \connection ->
    withTransaction connection $ do
      _ <-
        backfillLegacyBasketSnapshots
          connection defaultBasketSeriesId rangeStart rangeEnd
      _ <-
        backfillMarketVolume
          connection testChainId testRouter rangeStart rangeEnd
      forM_ canonicalCandleIntervals $ \interval -> do
        upsertRollupCoverage connection $
          RollupCoverage
            { rcKind = PriceRollup
            , rcSeriesId = Just defaultBasketSeriesId
            , rcChainId = Nothing
            , rcReleaseRouter = Nothing
            , rcIntervalSeconds = interval
            , rcCoverageStart = Just rangeStart
            , rcCoverageEnd = Just rangeEnd
            , rcFinalizedThrough = Just rangeEnd
            , rcGeneration = 7
            , rcComplete = True
            , rcDerivationVersion = "v1"
            , rcLastError = Nothing
            , rcMaintenanceFrom = Nothing
            , rcMaintenanceTo = Nothing
            }
        upsertRollupCoverage connection $
          RollupCoverage
            { rcKind = VolumeRollup
            , rcSeriesId = Nothing
            , rcChainId = Just testChainId
            , rcReleaseRouter = Just testRouter
            , rcIntervalSeconds = interval
            , rcCoverageStart = Just rangeStart
            , rcCoverageEnd = Just rangeEnd
            , rcFinalizedThrough = Just rangeEnd
            , rcGeneration = 7
            , rcComplete = True
            , rcDerivationVersion = "v1"
            , rcLastError = Nothing
            , rcMaintenanceFrom = Nothing
            , rcMaintenanceTo = Nothing
            }

runCandleAdmin :: Text -> [String] -> IO (ExitCode, String, String)
runCandleAdmin databaseUrl =
  runCandleAdminWithRouter databaseUrl testRouter

runCandleAdminWithRouter :: Text -> Text -> [String] -> IO (ExitCode, String, String)
runCandleAdminWithRouter databaseUrl releaseRouter arguments = do
  inheritedEnvironment <- getEnvironment
  let overrides =
        [ ("DATABASE_URL", Text.unpack databaseUrl)
        , ("PERPS_CHAIN_ID", show testChainId)
        , ("PERPS_ORDER_ROUTER", Text.unpack releaseRouter)
        , ("PERPS_CANDLE_LATENESS_SECONDS", "0")
        , ("PERPS_CANDLE_WRITE_MODE", "dual")
        ]
      overriddenNames = map fst overrides
      command =
        (proc "plether-candle-admin" arguments)
          { env =
              Just $
                overrides
                  <> filter ((`notElem` overriddenNames) . fst) inheritedEnvironment
          }
  readCreateProcessWithExitCode command ""

assertDedicatedDatabase :: DbPool -> IO ()
assertDedicatedDatabase pool =
  withDb pool $ \connection -> do
    databaseNames <-
      query_ connection "SELECT current_database()" :: IO [Only Text]
    case databaseNames of
      [Only databaseName]
        | "critical_path" `Text.isInfixOf` Text.toLower databaseName ->
            pure ()
      [Only databaseName] ->
        fail $
          "Refusing to run candle integration tests against database "
            <> Text.unpack databaseName
            <> "; its name must contain critical_path"
      _ -> fail "PostgreSQL did not return exactly one current_database() row"

prepareCandleDatabase :: DbPool -> IO ()
prepareCandleDatabase pool =
  withDb pool $ \connection -> do
    ensureBasketSnapshotSchema connection
    ensurePerpsHistorySchema connection
    ensureCandleSchema connection

cleanupCandleRows :: DbPool -> IO ()
cleanupCandleRows pool =
  withDb pool $ \connection ->
    withTransaction connection $ do
      void $
        execute_
          connection
          "TRUNCATE TABLE perps_candle_history_ingestion_windows, \
          \perps_candle_history_ingestions, perps_candle_history_targets, \
          \perps_market_release_epochs, perps_candle_markets"
      void $
        execute
          connection
          "DELETE FROM perps_rollup_coverage \
          \WHERE series_id = ? OR (chain_id = ? AND release_router = ?)"
          (testSeries, testChainId, normalizedTestRouter)
      void $
        execute
          connection
          "DELETE FROM perps_market_volume_rollups \
          \WHERE chain_id = ? AND release_router = ?"
          (testChainId, normalizedTestRouter)
      void $
        execute
          connection
          "DELETE FROM perps_account_activity \
          \WHERE chain_id = ? AND release_router = ?"
          (testChainId, normalizedTestRouter)
      void $
        execute
          connection
          "DELETE FROM perps_events \
          \WHERE chain_id = ? AND release_router = ?"
          (testChainId, normalizedTestRouter)
      void $
        execute
          connection
          "DELETE FROM perps_indexer_state \
          \WHERE chain_id = ? AND release_router = ?"
          (testChainId, certifiedVolumeRouter)
      void $
        execute
          connection
          "DELETE FROM perps_account_activity \
          \WHERE chain_id = ? AND release_router = ?"
          (testChainId, certifiedVolumeRouter)
      void $
        execute
          connection
          "DELETE FROM perps_events \
          \WHERE chain_id = ? AND release_router = ?"
          (testChainId, certifiedVolumeRouter)
      void $
        execute
          connection
          "DELETE FROM perps_basket_candles WHERE series_id = ?"
          (Only testSeries)
      void $
        execute
          connection
          "DELETE FROM perps_basket_observations WHERE series_id = ?"
          (Only testSeries)
      void $
        execute
          connection
          "DELETE FROM perps_basket_definitions WHERE series_id = ?"
          (Only testSeries)
      void $
        execute
          connection
          "DELETE FROM perps_basket_snapshots WHERE source IN (?, ?, ?, ?, ?)"
          ( legacySnapshotSource
          , latestSnapshotSource
          , previousLatestSnapshotSource
          , historicalSnapshotSource
          , targetHistorySnapshotSource
          )

cleanupCandleAdminRows :: DbPool -> IO ()
cleanupCandleAdminRows pool =
  withDb pool $ \connection ->
    withTransaction connection $ do
      void $
        execute
          connection
          "DELETE FROM perps_rollup_coverage WHERE series_id = ?"
          (Only defaultBasketSeriesId)
      void $
        execute
          connection
          "DELETE FROM perps_basket_candles WHERE series_id = ?"
          (Only defaultBasketSeriesId)
      void $
        execute
          connection
          "DELETE FROM perps_basket_observations WHERE series_id = ?"
          (Only defaultBasketSeriesId)
      void $
        execute
          connection
          "DELETE FROM perps_basket_definitions WHERE series_id = ?"
          (Only defaultBasketSeriesId)

candleRelationOids :: Connection -> IO [Integer]
candleRelationOids connection = do
  rows <-
    query_ connection $
      "SELECT relation::regclass::oid::bigint FROM (VALUES \
      \('perps_basket_definitions'::regclass), \
      \('perps_basket_observations'::regclass), \
      \('perps_basket_candles'::regclass), \
      \('perps_candle_markets'::regclass), \
      \('perps_candle_history_targets'::regclass), \
      \('perps_candle_history_ingestions'::regclass), \
      \('perps_candle_history_ingestion_windows'::regclass), \
      \('perps_market_release_epochs'::regclass), \
      \('perps_market_volume_rollups'::regclass), \
      \('perps_rollup_coverage'::regclass)) AS expected(relation) \
      \ORDER BY relation::regclass::text"
  pure [oid | Only oid <- rows]

candleActivityIndexValidity :: Connection -> IO [Only Bool]
candleActivityIndexValidity connection =
  query_ connection $
    "SELECT index_state.indisvalid AND index_state.indisready AND index_state.indislive \
    \FROM pg_class index_relation \
    \JOIN pg_namespace index_namespace ON index_namespace.oid = index_relation.relnamespace \
    \JOIN pg_index index_state ON index_state.indexrelid = index_relation.oid \
    \JOIN pg_class target_relation ON target_relation.oid = index_state.indrelid \
    \WHERE index_namespace.nspname = current_schema() \
    \AND index_relation.relname = 'idx_perps_account_activity_volume_rollup' \
    \AND target_relation.relname = 'perps_account_activity'"

candleEventIndexValidity :: Connection -> IO [Only Bool]
candleEventIndexValidity connection =
  query_ connection $
    "SELECT index_state.indisvalid AND index_state.indisready AND index_state.indislive \
    \FROM pg_class index_relation \
    \JOIN pg_namespace index_namespace ON index_namespace.oid = index_relation.relnamespace \
    \JOIN pg_index index_state ON index_state.indexrelid = index_relation.oid \
    \JOIN pg_class target_relation ON target_relation.oid = index_state.indrelid \
    \WHERE index_namespace.nspname = current_schema() \
    \AND index_relation.relname = 'idx_perps_events_candle_bounds' \
    \AND target_relation.relname = 'perps_events'"

candleActivityReorgIndexValidity :: Connection -> IO [Only Bool]
candleActivityReorgIndexValidity connection =
  query_ connection $
    "SELECT index_state.indisvalid AND index_state.indisready AND index_state.indislive \
    \FROM pg_class index_relation \
    \JOIN pg_namespace index_namespace ON index_namespace.oid = index_relation.relnamespace \
    \JOIN pg_index index_state ON index_state.indexrelid = index_relation.oid \
    \JOIN pg_class target_relation ON target_relation.oid = index_state.indrelid \
    \WHERE index_namespace.nspname = current_schema() \
    \AND index_relation.relname = 'idx_perps_account_activity_reorg_blocks' \
    \AND target_relation.relname = 'perps_account_activity'"

candleEventReorgIndexValidity :: Connection -> IO [Only Bool]
candleEventReorgIndexValidity connection =
  query_ connection $
    "SELECT index_state.indisvalid AND index_state.indisready AND index_state.indislive \
    \FROM pg_class index_relation \
    \JOIN pg_namespace index_namespace ON index_namespace.oid = index_relation.relnamespace \
    \JOIN pg_index index_state ON index_state.indexrelid = index_relation.oid \
    \JOIN pg_class target_relation ON target_relation.oid = index_state.indrelid \
    \WHERE index_namespace.nspname = current_schema() \
    \AND index_relation.relname = 'idx_perps_events_reorg_blocks' \
    \AND target_relation.relname = 'perps_events'"

insertRawEvent :: Connection -> Text -> Integer -> Integer -> IO ()
insertRawEvent connection eventSuffix timestamp blockNumber =
  void $
    execute connection
      "INSERT INTO perps_events \
      \(chain_id, release_router, contract_address, event_name, tx_hash, block_number, \
      \ block_hash, tx_index, log_index, timestamp, data) \
      \VALUES (?, ?, ?, 'PositionOpened', ?, ?, ?, 0, ?, ?, '{}'::jsonb)"
      ( testChainId
      , normalizedTestRouter
      , "candle-rollup-integration-engine" :: Text
      , "candle-rollup-event-tx:" <> eventSuffix
      , blockNumber
      , "candle-rollup-event-block:" <> Text.pack (show blockNumber)
      , blockNumber
      , timestamp
      )

insertObservation
  :: Connection -> Text -> Integer -> Integer -> Text -> Int -> IO ()
insertObservation connection observationId publishTime price source priority = do
  changed <-
    upsertBasketObservation connection $
      observation observationId publishTime price source priority
  changed `shouldBe` True

observation :: Text -> Integer -> Integer -> Text -> Int -> BasketObservationInput
observation observationId publishTime price source priority =
  BasketObservationInput
    { boiSeriesId = testSeries
    , boiObservationId = observationId
    , boiPublishTime = publishTime
    , boiBasketPrice = price
    , boiComponentPrices = componentPayload
    , boiSource = source
    , boiSourcePriority = priority
    }

componentPayload :: Value
componentPayload =
  object
    [ "EURUSD" .= (100_000_000 :: Integer)
    , "USDJPY" .= (150_000_000 :: Integer)
    ]

legacySnapshotAt :: Connection -> Integer -> IO [(Integer, Text)]
legacySnapshotAt connection timestamp =
  query
    connection
    "SELECT basket_price, source FROM perps_basket_snapshots \
    \WHERE timestamp = ? AND interval_seconds = 60"
    (Only timestamp)

insertActivity
  :: Connection
  -> Text
  -> Integer
  -> Integer
  -> Integer
  -> Integer
  -> Text
  -> IO ()
insertActivity connection eventKey timestamp blockNumber sizeDelta price activityType =
  insertPerpsActivity
    connection
    testChainId
    testRouter
    "candle-rollup-integration-engine"
    ("candle-rollup-integration:" <> eventKey)
    "candle-rollup-integration-account"
    activityType
    Nothing
    Nothing
    Nothing
    (Just price)
    (Just sizeDelta)
    Nothing
    Nothing
    ("candle-rollup-integration-tx:" <> eventKey)
    blockNumber
    ("candle-rollup-integration-block:" <> Text.pack (show blockNumber))
    0
    blockNumber
    timestamp
    (object ["integrationTest" .= True])

storedCandles
  :: Connection -> Integer -> Integer -> Integer -> IO [StoredCandle]
storedCandles connection interval fromTimestamp toTimestamp =
  query
    connection
    "SELECT bucket_start, raw_open_price, raw_high_price, raw_low_price, \
    \raw_close_price, sample_count, quality, revision, finalized \
    \FROM perps_basket_candles WHERE series_id = ? AND interval_seconds = ? \
    \AND bucket_start >= ? AND bucket_start < ? ORDER BY bucket_start"
    (testSeries, interval, fromTimestamp, toTimestamp)

requireStoredCandle :: Connection -> Integer -> Integer -> IO StoredCandle
requireStoredCandle connection interval bucketStart = do
  rows <- storedCandles connection interval bucketStart (bucketStart + interval)
  rows `shouldSatisfy` ((== 1) . length)
  pure $ head rows

storedVolumes
  :: Connection -> Integer -> Integer -> Integer -> IO [StoredVolume]
storedVolumes connection interval fromTimestamp toTimestamp =
  query
    connection
    "SELECT bucket_start, volume_numerator::text, trade_count, first_source_block, \
    \last_source_block, revision, finalized \
    \FROM perps_market_volume_rollups \
    \WHERE chain_id = ? AND release_router = ? AND interval_seconds = ? \
    \AND bucket_start >= ? AND bucket_start < ? ORDER BY bucket_start"
    (testChainId, normalizedTestRouter, interval, fromTimestamp, toTimestamp)

requireStoredVolume :: Connection -> Integer -> Integer -> IO StoredVolume
requireStoredVolume connection interval bucketStart = do
  rows <- storedVolumes connection interval bucketStart (bucketStart + interval)
  rows `shouldSatisfy` ((== 1) . length)
  pure $ head rows

putPriceCoverage
  :: Connection
  -> Integer
  -> Integer
  -> Integer
  -> Integer
  -> Integer
  -> Bool
  -> IO ()
putPriceCoverage connection interval coverageStart coverageEnd finalized generation complete =
  putPriceCoverageVersion
    connection interval coverageStart coverageEnd finalized generation complete "v1"

putPriceCoverageVersion
  :: Connection
  -> Integer
  -> Integer
  -> Integer
  -> Integer
  -> Integer
  -> Bool
  -> Text
  -> IO ()
putPriceCoverageVersion
  connection interval coverageStart coverageEnd finalized generation complete derivationVersion =
  upsertRollupCoverage connection $
    RollupCoverage
      { rcKind = PriceRollup
      , rcSeriesId = Just testSeries
      , rcChainId = Nothing
      , rcReleaseRouter = Nothing
      , rcIntervalSeconds = interval
      , rcCoverageStart = Just coverageStart
      , rcCoverageEnd = Just coverageEnd
      , rcFinalizedThrough = Just finalized
      , rcGeneration = generation
      , rcComplete = complete
      , rcDerivationVersion = derivationVersion
      , rcLastError = Nothing
      , rcMaintenanceFrom = Nothing
      , rcMaintenanceTo = Nothing
      }

putVolumeCoverage
  :: Connection
  -> Integer
  -> Integer
  -> Integer
  -> Integer
  -> Integer
  -> Bool
  -> IO ()
putVolumeCoverage connection interval coverageStart coverageEnd finalized generation complete =
  putVolumeCoverageVersion
    connection interval coverageStart coverageEnd finalized generation complete "v1"

putVolumeCoverageVersion
  :: Connection
  -> Integer
  -> Integer
  -> Integer
  -> Integer
  -> Integer
  -> Bool
  -> Text
  -> IO ()
putVolumeCoverageVersion
  connection interval coverageStart coverageEnd finalized generation complete derivationVersion =
  upsertRollupCoverage connection $
    RollupCoverage
      { rcKind = VolumeRollup
      , rcSeriesId = Nothing
      , rcChainId = Just testChainId
      , rcReleaseRouter = Just testRouter
      , rcIntervalSeconds = interval
      , rcCoverageStart = Just coverageStart
      , rcCoverageEnd = Just coverageEnd
      , rcFinalizedThrough = Just finalized
      , rcGeneration = generation
      , rcComplete = complete
      , rcDerivationVersion = derivationVersion
      , rcLastError = Nothing
      , rcMaintenanceFrom = Nothing
      , rcMaintenanceTo = Nothing
      }

requirePriceCoverage :: Connection -> Integer -> IO RollupCoverage
requirePriceCoverage connection =
  requirePriceCoverageForSeries connection testSeries

requirePriceCoverageForSeries :: Connection -> Text -> Integer -> IO RollupCoverage
requirePriceCoverageForSeries connection seriesId interval = do
  coverage <-
    getRollupCoverage
      connection PriceRollup (Just seriesId) Nothing Nothing interval
  coverage `shouldSatisfy` maybe False (const True)
  case coverage of
    Just value -> pure value
    Nothing -> fail "Expected price coverage"

requireVolumeCoverage :: Connection -> Integer -> IO RollupCoverage
requireVolumeCoverage connection interval = do
  coverage <-
    getRollupCoverage
      connection VolumeRollup Nothing (Just testChainId) (Just testRouter) interval
  coverage `shouldSatisfy` maybe False (const True)
  case coverage of
    Just value -> pure value
    Nothing -> fail "Expected volume coverage"

testSeries :: Text
testSeries = "integration-candle-rollup-v1"

testChainId :: Integer
testChainId = 9_999_421_614

testRouter :: Text
testRouter = "CANDLE-ROLLUP-INTEGRATION-ROUTER"

normalizedTestRouter :: Text
normalizedTestRouter = Text.toLower testRouter

certifiedVolumeRouter :: Text
certifiedVolumeRouter = "0x9999999999999999999999999999999999999999"

certifiedVolumeIndexerName :: Text
certifiedVolumeIndexerName = "perps-history-costs-v2:certified-volume-bound"

certifiedVolumeActivationBlock :: Integer
certifiedVolumeActivationBlock = 10_000

legacySnapshotSource :: Text
legacySnapshotSource = "candle_rollup_integration_test"

targetHistorySnapshotSource :: Text
targetHistorySnapshotSource = "pyth_tradingview_history_v1"

latestSnapshotSource :: Text
latestSnapshotSource = "backend_hermes_latest_v2"

previousLatestSnapshotSource :: Text
previousLatestSnapshotSource = "pyth_hermes_latest"

historicalSnapshotSource :: Text
historicalSnapshotSource = "backend_hermes_historical_v2"

-- This value is aligned to UTC day boundaries and to the 60-second fixed-page
-- span (60 * 500), simplifying exact parent/page assertions.
baseTime :: Integer
baseTime = 1_699_920_000

alignDownForTest :: Integer -> Integer -> Integer
alignDownForTest timestamp interval = timestamp - timestamp `mod` interval

alignUpForTest :: Integer -> Integer -> Integer
alignUpForTest timestamp interval =
  let remainder = timestamp `mod` interval
   in if remainder == 0 then timestamp else timestamp + interval - remainder

pageSpan60 :: Integer
pageSpan60 = 60 * 500
