module Plether.Handlers.PerpsSpec (spec) where

import Control.Monad (forM_)
import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Either (isRight)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Plether.Cache (SingleFlightSource (..))
import Plether.Config
  ( Config (..)
  , LpSettlementMode (..)
  , PerpsCandleReadMode (..)
  , PerpsCandleWriteMode (..)
  )
import Plether.Insights.Competition
  ( CompetitionReleaseManifest (..)
  , july2026Competition
  )
import Plether.Database.Candles
  ( BasketCandleRow (..)
  , CandleCurrent (..)
  , CandlePage (..)
  , CandleRange (..)
  , CandleQuality (..)
  )
import Plether.Database.Schema
  ( BasketHistorySnapshotRow (..)
  , PerpsMarketVolumeBucketRow (..)
  )
import Plether.Handlers.Perps
  ( BasketHistoryTimings (..)
  , BasketCandleTimings (..)
  , PythUpdateAdmission (..)
  , basketHistoryPointsWithVolume
  , basketHistoryFromCandleRows
  , basketHistoryServerTiming
  , basketHistoryTimingMetrics
  , basketCandleServerTiming
  , basketCandleTimingMetrics
  , boundedBasketHistoryInterval
  , coverageLagSeconds
  , currentCandleCacheKey
  , currentCandleSnapshotNeedsAuthoritativeReload
  , decodePythUpdateForAdmission
  , getBasketHistoryWithSourcesTimed
  , getLegacyBasketHistoryVolumeRowsTimed
  , isBoundedComponentHistoryRequest
  , validateRollupHistoryRange
  , validateRollupHistoryRangeWithPolicy
  , validateBasketCandlePage
  , validateBasketCandlePageWithCap
  , validateBasketCandlePageWithPolicy
  , validateBasketCurrentCandle
  , validateBasketCurrentCandleWithCap
  , validateBasketCurrentCandleWithPolicy
  )
import Plether.Pyth.Basket (BasketComponent (..), basketComponents)
import Plether.Types
  ( ApiError (..)
  , ApiErrorCode (..)
  , BasketHistory (..)
  , BasketHistoryParams (..)
  , BasketHistoryPoint (..)
  , PythUpdateResponse (..)
  )
import Plether.Types.Perps (canonicalBasketCandleIntervals)
import Test.Hspec

spec :: Spec
spec = do
  describe "boundedBasketHistoryInterval" $ do
    it "preserves supported chart response shapes" $ do
      boundedBasketHistoryInterval (24 * 60 * 60) 60 `shouldBe` 60
      boundedBasketHistoryInterval (7 * 24 * 60 * 60) 60 `shouldBe` 60
      boundedBasketHistoryInterval (365 * 24 * 60 * 60) 3600 `shouldBe` 3600

    it "snaps expensive requests to the smallest sufficient canonical rollup" $ do
      boundedBasketHistoryInterval (30 * 24 * 60 * 60) 60 `shouldBe` 300
      boundedBasketHistoryInterval (365 * 24 * 60 * 60) 60 `shouldBe` 3600
      boundedBasketHistoryInterval (24 * 60 * 60) 120 `shouldBe` 180
      boundedBasketHistoryInterval (365 * 24 * 60 * 60) 999_999 `shouldBe` 86_400

    it "downsamples oversized direct history requests" $ do
      let yearSeconds = 365 * 24 * 60 * 60
          interval = boundedBasketHistoryInterval yearSeconds 60
      interval `shouldSatisfy` (> 60)
      (yearSeconds `div` interval) + 4 `shouldSatisfy` (<= 12_000)

    it "restricts raw component history to the bounded cached public shape" $ do
      isBoundedComponentHistoryRequest
        (historyParams {bhpRange = "24h", bhpIntervalSeconds = 3600, bhpIncludeComponents = True})
        `shouldBe` True
      isBoundedComponentHistoryRequest
        (historyParams {bhpRange = "24h", bhpIntervalSeconds = 300, bhpIncludeComponents = True})
        `shouldBe` False
      isBoundedComponentHistoryRequest
        (historyParams {bhpRange = "1y", bhpIntervalSeconds = 3600, bhpIncludeComponents = True})
        `shouldBe` False
      isBoundedComponentHistoryRequest
        (historyParams {bhpRange = "1y", bhpIntervalSeconds = 60, bhpIncludeComponents = False})
        `shouldBe` True

  describe "basket history request timings" $ do
    it "renders stable Server-Timing metrics with millisecond precision" $ do
      basketHistoryServerTiming sampleTimings
        `shouldBe` "plether_app;dur=50.000, plether_db_pool_wait;dur=7.000, plether_db_snapshots;dur=18.234, plether_db_volume;dur=20.000, plether_response_encode;dur=2.500, plether_other;dur=2.266"

    it "keeps the timing stages and unattributed remainder canonical" $ do
      basketHistoryTimingMetrics sampleTimings
        `shouldBe`
          [ ("plether_app", 50_000_000)
          , ("plether_db_pool_wait", 7_000_000)
          , ("plether_db_snapshots", 18_234_000)
          , ("plether_db_volume", 20_000_000)
          , ("plether_response_encode", 2_500_000)
          , ("plether_other", 2_266_000)
          ]

    it "renders sub-millisecond durations without scientific notation" $ do
      basketHistoryServerTiming zeroStageTimings {bhtBackendTotalNs = 1_234_000}
        `shouldBe` "plether_app;dur=1.234, plether_db_pool_wait;dur=0.000, plether_db_snapshots;dur=0.000, plether_db_volume;dur=0.000, plether_response_encode;dur=0.000, plether_other;dur=1.234"

    it "clamps the unattributed remainder when measured stages exceed app time" $ do
      last (basketHistoryTimingMetrics zeroStageTimings {bhtBackendTotalNs = 1, bhtDbPoolWaitNs = 2})
        `shouldBe` ("plether_other", 0)

  describe "legacy basket history volume query" $ do
    it "does not evaluate market-volume activity for component history" $ do
      calls <- newIORef (0 :: Int)
      (rows, queryNs, rowCount) <-
        getLegacyBasketHistoryVolumeRowsTimed True $ do
          modifyIORef' calls (+ 1)
          pure volumeRows
      rows `shouldBe` []
      queryNs `shouldBe` 0
      rowCount `shouldBe` 0
      readIORef calls `shouldReturn` 0

    it "still evaluates market-volume activity for non-component history" $ do
      calls <- newIORef (0 :: Int)
      (rows, _, rowCount) <-
        getLegacyBasketHistoryVolumeRowsTimed False $ do
          modifyIORef' calls (+ 1)
          pure volumeRows
      rows `shouldBe` volumeRows
      rowCount `shouldBe` length volumeRows
      readIORef calls `shouldReturn` 1

  describe "basket history rollup compatibility" $ do
    it "never evaluates the raw fetch in enabled rollup mode" $ do
      rawCalls <- newIORef (0 :: Int)
      rollupCalls <- newIORef (0 :: Int)
      let rawFetch _ = modifyIORef' rawCalls (+ 1) >> pure (Left testApiError)
          rollupFetch _ = modifyIORef' rollupCalls (+ 1) >> pure (Left testApiError)
      _ <-
        getBasketHistoryWithSourcesTimed
          rawFetch
          rollupFetch
          rollupConfig
          historyParams
      readIORef rawCalls `shouldReturn` 0
      readIORef rollupCalls `shouldReturn` 1

    it "routes the original 30-day hourly request to rollups" $ do
      rawCalls <- newIORef (0 :: Int)
      rollupCalls <- newIORef (0 :: Int)
      let rawFetch _ = modifyIORef' rawCalls (+ 1) >> pure (Left testApiError)
          rollupFetch _ = modifyIORef' rollupCalls (+ 1) >> pure (Left testApiError)
      _ <-
        getBasketHistoryWithSourcesTimed
          rawFetch
          rollupFetch
          rollupConfig
          historyParams {bhpRange = "30d", bhpIntervalSeconds = 3600}
      readIORef rawCalls `shouldReturn` 0
      readIORef rollupCalls `shouldReturn` 1

    it "routes oversized fine-grained requests through canonical rollups" $ do
      rawCalls <- newIORef (0 :: Int)
      rollupCalls <- newIORef (0 :: Int)
      let rawFetch _ = modifyIORef' rawCalls (+ 1) >> pure (Left testApiError)
          rollupFetch _ = modifyIORef' rollupCalls (+ 1) >> pure (Left testApiError)
          oneYearConfig = rollupConfig {cfgPerpsCandleReadIntervals = [3600]}
      _ <-
        getBasketHistoryWithSourcesTimed
          rawFetch
          rollupFetch
          oneYearConfig
          historyParams {bhpRange = "1y", bhpIntervalSeconds = 60}
      readIORef rawCalls `shouldReturn` 0
      readIORef rollupCalls `shouldReturn` 1

    it "keeps component history and legacy mode on the raw path" $ do
      rawCalls <- newIORef (0 :: Int)
      rollupCalls <- newIORef (0 :: Int)
      let rawFetch _ = modifyIORef' rawCalls (+ 1) >> pure (Left testApiError)
          rollupFetch _ = modifyIORef' rollupCalls (+ 1) >> pure (Left testApiError)
      _ <-
        getBasketHistoryWithSourcesTimed
          rawFetch
          rollupFetch
          rollupConfig
          historyParams {bhpIncludeComponents = True}
      _ <-
        getBasketHistoryWithSourcesTimed
          rawFetch
          rollupFetch
          rollupConfig {cfgPerpsCandleReadMode = PerpsCandleReadsLegacy}
          historyParams
      readIORef rawCalls `shouldReturn` 2
      readIORef rollupCalls `shouldReturn` 0

    it "projects finalized candles into the legacy response shape" $ do
      let history =
            basketHistoryFromCandleRows
              30_100
              historyParams
              [ sampleCandleRow
              , sampleCandleRow
                  { bcrBucketStart = 60
                  , bcrRawOpenPrice = 101_000_000
                  , bcrRawClosePrice = 102_000_000
                  , bcrVolumeNumerator = Just $ 3_000_000 * 10 ^ (20 :: Int)
                  }
              ]
      bhLatestPrice history `shouldBe` Just 102_000_000
      bhIntervalSeconds history `shouldBe` 60
      map bhpVolumeUsdc (bhPoints history) `shouldBe` [1_000_000, 3_000_000]
      map bhpComponents (bhPoints history) `shouldBe` [Nothing, Nothing]

    it "accepts one bounded sparse range and preserves inception clipping" $ do
      let candleRange =
            sampleCandleRange
              { crCandles =
                  [ sampleCandleRow {bcrBucketStart = 120}
                  , sampleCandleRow {bcrBucketStart = 600}
                  ]
              , crCoverageStart = Just 120
              , crCoverageEnd = Just 900
              , crFinalizedThrough = Just 900
              }
      validated <- expectRight $
        validateRollupHistoryRange 200_000_000 60 0 900 12_000 candleRange
      validated `shouldBe` crCandles candleRange

    it "rejects a compatibility range beyond its 12k response budget" $ do
      let oversized = sampleCandleRange {crCandles = replicate 12_001 sampleCandleRow}
      validateRollupHistoryRange 200_000_000 60 0 30_000 12_000 oversized
        `shouldFailWith` "bounded row budget"

    it "rejects incomplete compatibility metadata before serving sparse rows" $ do
      validateRollupHistoryRange
        200_000_000
        60
        0
        30_000
        12_000
        sampleCandleRange {crCoverageComplete = False}
        `shouldFailWith` "coverage is incomplete"

    it "keeps nullable volume out of the legacy compatibility shape" $ do
      let priceOnlyRow =
            sampleCandleRow
              { bcrVolumeNumerator = Nothing
              , bcrTradeCount = Nothing
              , bcrVolumeComplete = False
              }
      validateRollupHistoryRange
        200_000_000
        60
        0
        30_000
        12_000
        sampleCandleRange {crCandles = [priceOnlyRow]}
        `shouldFailWith` "compatibility candle has unknown volume"

    it "rejects a complete compatibility range with a stale source watermark" $ do
      let stale =
            sampleCandleRange
              { crCoverageEnd = Just 29_820
              , crFinalizedThrough = Just 29_820
              }
      validateRollupHistoryRangeWithPolicy
        200_000_000
        120
        0
        30_030
        60
        0
        30_000
        12_000
        stale
        `shouldFailWith` "watermark is stale"

    it "rejects fresh compatibility coverage with severely stale finalization" $ do
      let staleFinalization =
            sampleCandleRange
              { crCoverageEnd = Just 30_000
              , crFinalizedThrough = Just 60
              }
      validateRollupHistoryRangeWithPolicy
        200_000_000
        120
        0
        30_030
        60
        0
        30_000
        12_000
        staleFinalization
        `shouldFailWith` "finalized watermark is stale"

    it "accepts compatibility coverage and finalization at the exact freshness boundary" $ do
      let boundary =
            sampleCandleRange
              { crCoverageEnd = Just 29_880
              , crFinalizedThrough = Just 29_880
              }
      validated <- expectRight $
        validateRollupHistoryRangeWithPolicy
          200_000_000
          120
          0
          30_030
          60
          0
          30_000
          12_000
          boundary
      validated `shouldBe` crCandles boundary

    it "allows compatibility history through publication grace and fails at expiry" $ do
      let previousWatermark =
            sampleCandleRange
              { crCoverageEnd = Just 30_000
              , crFinalizedThrough = Just 29_940
              }
          validateAt now =
            validateRollupHistoryRangeWithPolicy
              200_000_000
              120
              15
              now
              60
              0
              30_000
              12_000
              previousWatermark
      validateAt 30_120 `shouldSatisfy` isRight
      validateAt 30_134 `shouldSatisfy` isRight
      validateAt 30_135 `shouldFailWith` "finalized watermark is stale"

  describe "current candle raw snapshot cache key" $ do
    it "isolates interval boundaries and runtime market identity" $ do
      let beforeBoundary = currentCandleCacheKey rollupConfig 179 60
          atBoundary = currentCandleCacheKey rollupConfig 180 60
          otherInterval = currentCandleCacheKey rollupConfig 180 180
          normalizedRouter =
            currentCandleCacheKey
              rollupConfig {cfgPerpsOrderRouter = " 0xRoUtEr "}
              179
              60
          otherChain =
            currentCandleCacheKey
              rollupConfig {cfgPerpsChainId = cfgPerpsChainId rollupConfig + 1}
              179
              60
      shouldBe
        (currentCandleCacheKey rollupConfig 120 60)
        (421614, "0xrouter", 60, 120)
      shouldBe beforeBoundary (421614, "0xrouter", 60, 120)
      shouldBe normalizedRouter beforeBoundary
      shouldBe (atBoundary == beforeBoundary) False
      shouldBe (otherInterval == atBoundary) False
      shouldBe (otherChain == beforeBoundary) False

    it "reloads only cached or cross-second coalesced snapshots" $ do
      currentCandleSnapshotNeedsAuthoritativeReload
        101 SingleFlightMemory 100
        `shouldBe` True
      currentCandleSnapshotNeedsAuthoritativeReload
        101 SingleFlightStale 100
        `shouldBe` True
      currentCandleSnapshotNeedsAuthoritativeReload
        101 SingleFlightCoalesced 100
        `shouldBe` True
      currentCandleSnapshotNeedsAuthoritativeReload
        101 SingleFlightCoalesced 101
        `shouldBe` False
      currentCandleSnapshotNeedsAuthoritativeReload
        101 SingleFlightLoaded 100
        `shouldBe` False

  describe "basket candle request timings" $ do
    it "attributes fixed-page reads to the rollup query" $ do
      basketCandleServerTiming sampleCandleTimings
        `shouldBe` "plether_app;dur=15.000, plether_db_pool_wait;dur=1.000, plether_db_candles;dur=9.250, plether_singleflight_wait;dur=0.750, plether_response_encode;dur=1.500, plether_other;dur=2.500"
      basketCandleTimingMetrics sampleCandleTimings
        `shouldBe`
          [ ("plether_app", 15_000_000)
          , ("plether_db_pool_wait", 1_000_000)
          , ("plether_db_candles", 9_250_000)
          , ("plether_singleflight_wait", 750_000)
          , ("plether_response_encode", 1_500_000)
          , ("plether_other", 2_500_000)
          ]

  describe "strict basket candle validation" $ do
    it "accepts an active page finalized behind the current boundary" $ do
      let page =
            sampleCandlePage
              { cpCandles = [sampleCandleRow {bcrBucketStart = 44_880}]
              , cpPreviousCursor = Just 30_000
              , cpHasEarlier = True
              , cpCoverageEnd = Just 45_000
              , cpFinalizedThrough = Just 44_940
              }
      validateBasketCandlePage 45_000 60 60_000 page `shouldBe` Right ()

    it "rejects a complete native page with a stale source watermark" $ do
      let stale =
            sampleCandlePage
              { cpCoverageEnd = Just 29_820
              , cpFinalizedThrough = Just 29_820
              }
      validateBasketCandlePageWithPolicy
        200_000_000
        120
        0
        30_030
        60
        30_000
        stale
        `shouldFailTextWith` "watermark is stale"

    it "rejects a native bridge page with fresh coverage but severely stale finalization" $ do
      let staleFinalization =
            sampleCandlePage
              { cpCandles = []
              , cpPreviousCursor = Just 30_000
              , cpHasEarlier = True
              , cpCoverageEnd = Just 30_000
              , cpFinalizedThrough = Just 60
              }
      validateBasketCandlePageWithPolicy
        200_000_000
        120
        0
        30_030
        60
        60_000
        staleFinalization
        `shouldFailTextWith` "finalized watermark is stale"

    it "accepts native-page coverage and finalization at the exact freshness boundary" $ do
      let boundary =
            sampleCandlePage
              { cpCoverageEnd = Just 29_880
              , cpFinalizedThrough = Just 29_880
              }
      validateBasketCandlePageWithPolicy
        200_000_000
        120
        0
        30_030
        60
        30_000
        boundary
        `shouldBe` Right ()

    it "allows an active native page through publication grace and fails at expiry" $ do
      let previousWatermark =
            sampleCandlePage
              { cpCandles = []
              , cpPreviousCursor = Just 30_000
              , cpHasEarlier = True
              , cpCoverageEnd = Just 45_000
              , cpFinalizedThrough = Just 44_940
              }
          validateAt now =
            validateBasketCandlePageWithPolicy
              200_000_000
              120
              15
              now
              60
              60_000
              previousWatermark
      validateAt 45_120 `shouldBe` Right ()
      validateAt 45_134 `shouldBe` Right ()
      validateAt 45_135 `shouldFailTextWith` "finalized watermark is stale"

    it "rejects mutable rows from an active historical page" $ do
      let page =
            sampleCandlePage
              { cpCandles = [sampleCandleRow {bcrBucketStart = 44_940}]
              , cpPreviousCursor = Just 30_000
              , cpHasEarlier = True
              , cpCoverageEnd = Just 45_000
              , cpFinalizedThrough = Just 44_940
              }
      validateBasketCandlePage 45_000 60 60_000 page
        `shouldFailTextWith` "finalized page window"

    it "rejects a page whose cursor is not a fixed-page boundary" $ do
      validateBasketCandlePage 30_060 60 30_060 sampleCandlePage
        `shouldFailTextWith` "cursor is not page-aligned"

    it "still requires a fully closed page to reach its cursor" $ do
      let page = sampleCandlePage {cpFinalizedThrough = Just 29_940}
      validateBasketCandlePage 30_060 60 30_000 page
        `shouldFailTextWith` "not finalized through its covered end"

    it "accepts a coverage-proven inception page clipped on the left" $ do
      let page =
            sampleCandlePage
              { cpCandles = [sampleCandleRow {bcrBucketStart = 12_000}]
              , cpCoverageStart = Just 12_000
              }
      validateBasketCandlePage 30_060 60 30_000 page `shouldBe` Right ()

    it "accepts a coverage-proven terminal page clipped on the right" $ do
      let page =
            sampleCandlePage
              { cpCandles = [sampleCandleRow {bcrBucketStart = 44_940}]
              , cpPreviousCursor = Just 30_000
              , cpHasEarlier = True
              , cpCoverageEnd = Just 45_000
              , cpFinalizedThrough = Just 45_000
              }
      validateBasketCandlePage 90_000 60 60_000 page `shouldBe` Right ()

    it "accepts a sparse previous cursor that skips empty fixed pages" $ do
      let page =
            sampleCandlePage
              { cpCandles = [sampleCandleRow {bcrBucketStart = 60_000}]
              , cpPreviousCursor = Just 30_000
              , cpHasEarlier = True
              , cpCoverageEnd = Just 90_000
              , cpFinalizedThrough = Just 90_000
              }
      validateBasketCandlePage 90_060 60 90_000 page `shouldBe` Right ()

    it "accepts an empty bridge page after the coverage terminal" $ do
      let page =
            sampleCandlePage
              { cpCandles = []
              , cpPreviousCursor = Just 120_000
              , cpHasEarlier = True
              , cpCoverageEnd = Just 120_000
              , cpFinalizedThrough = Just 120_000
              }
      validateBasketCandlePage 180_060 60 180_000 page `shouldBe` Right ()

    it "rejects inception pagination that claims nonexistent earlier coverage" $ do
      let page =
            sampleCandlePage
              { cpCandles = [sampleCandleRow {bcrBucketStart = 12_000}]
              , cpPreviousCursor = Just 0
              , cpHasEarlier = True
              , cpCoverageStart = Just 12_000
              }
      validateBasketCandlePage 30_060 60 30_000 page
        `shouldFailTextWith` "inception-clipped page"

    it "rejects duplicate or unordered rows" $ do
      let duplicateRows =
            sampleCandlePage
              { cpCandles = [sampleCandleRow, sampleCandleRow]
              }
      validateBasketCandlePage 30_060 60 30_000 duplicateRows
        `shouldFailTextWith` "not strictly ascending"

    it "accepts finalized price history before current-router volume coverage" $ do
      let priceOnly =
            sampleCandlePage
              { cpCandles =
                  [ sampleCandleRow
                      { bcrVolumeNumerator = Nothing
                      , bcrTradeCount = Nothing
                      , bcrVolumeComplete = False
                      }
                  ]
              }
      validateBasketCandlePage 30_060 60 30_000 priceOnly `shouldBe` Right ()

    it "rejects contradictory historical volume fields and incomplete prices" $ do
      let validate row =
            validateBasketCandlePage
              30_060
              60
              30_000
              sampleCandlePage {cpCandles = [row]}
      validate sampleCandleRow {bcrVolumeNumerator = Nothing}
        `shouldFailTextWith` "volume fields are inconsistent"
      validate sampleCandleRow {bcrVolumeComplete = False}
        `shouldFailTextWith` "volume fields are inconsistent"
      validate sampleCandleRow {bcrPriceComplete = False}
        `shouldFailTextWith` "price is incomplete"

    it "rejects historical prices at or above the immutable display cap" $ do
      let cap = 200_000_000
          atCap =
            sampleCandlePage
              { cpCandles =
                  [ sampleCandleRow
                      { bcrRawOpenPrice = cap
                      , bcrRawHighPrice = cap
                      , bcrRawLowPrice = cap
                      , bcrRawClosePrice = cap
                      }
                  ]
              }
          aboveCap =
            atCap
              { cpCandles =
                  [ sampleCandleRow
                      { bcrRawOpenPrice = cap + 1
                      , bcrRawHighPrice = cap + 1
                      , bcrRawLowPrice = cap + 1
                      , bcrRawClosePrice = cap + 1
                      }
                  ]
              }
      validateBasketCandlePageWithCap cap 30_060 60 30_000 atCap
        `shouldFailTextWith` "outside the display domain"
      validateBasketCandlePageWithCap cap 30_060 60 30_000 aboveCap
        `shouldFailTextWith` "outside the display domain"

    it "validates current rows but allows incomplete nullable volume" $ do
      let current =
            CandleCurrent
              { ccCandle =
                  Just $
                    sampleCandleRow
                    { bcrBucketStart = 30_000
                    , bcrVolumeNumerator = Nothing
                    , bcrTradeCount = Nothing
                    , bcrPriceComplete = False
                    , bcrVolumeComplete = False
                    }
              , ccCoverageStart = Just 0
              , ccCoverageEnd = Just 30_000
              , ccFinalizedThrough = Just 30_000
              , ccDatasetGeneration = 3
              , ccCoverageComplete = True
              , ccVolumeCoverageStart = Nothing
              , ccVolumeCoverageEnd = Nothing
              , ccVolumeFinalizedThrough = Nothing
              , ccVolumeCoverageComplete = False
              }
      validateBasketCurrentCandle 30_030 60 current `shouldBe` Right ()

    it "allows provisional current volume only with usable covering metadata" $ do
      let current =
            CandleCurrent
              { ccCandle =
                  Just $
                    sampleCandleRow
                      { bcrBucketStart = 30_000
                      , bcrPriceComplete = False
                      , bcrVolumeComplete = False
                      }
              , ccCoverageStart = Just 0
              , ccCoverageEnd = Just 30_000
              , ccFinalizedThrough = Just 30_000
              , ccDatasetGeneration = 3
              , ccCoverageComplete = True
              , ccVolumeCoverageStart = Just 0
              , ccVolumeCoverageEnd = Just 30_000
              , ccVolumeFinalizedThrough = Just 29_940
              , ccVolumeCoverageComplete = True
              }
      validateBasketCurrentCandle 30_030 60 current `shouldBe` Right ()

    it "rejects provisional current volume when coverage is unusable or has not reached the bucket" $ do
      let covered =
            CandleCurrent
              { ccCandle =
                  Just $
                    sampleCandleRow
                      { bcrBucketStart = 30_000
                      , bcrPriceComplete = False
                      , bcrVolumeComplete = False
                      }
              , ccCoverageStart = Just 0
              , ccCoverageEnd = Just 30_000
              , ccFinalizedThrough = Just 30_000
              , ccDatasetGeneration = 3
              , ccCoverageComplete = True
              , ccVolumeCoverageStart = Just 0
              , ccVolumeCoverageEnd = Just 30_000
              , ccVolumeFinalizedThrough = Just 29_940
              , ccVolumeCoverageComplete = True
              }
          unusable =
            covered
              { ccVolumeCoverageStart = Nothing
              , ccVolumeCoverageEnd = Nothing
              , ccVolumeFinalizedThrough = Nothing
              , ccVolumeCoverageComplete = False
              }
          notReached =
            covered
              { ccVolumeCoverageEnd = Just 29_940
              }
      validateBasketCurrentCandle 30_030 60 unusable
        `shouldFailTextWith` "without usable coverage"
      validateBasketCurrentCandle 30_030 60 notReached
        `shouldFailTextWith` "outside the checked coverage envelope"

    it "keeps current metadata valid when the active bucket has no row" $ do
      let current =
            CandleCurrent
              { ccCandle = Nothing
              , ccCoverageStart = Just 0
              , ccCoverageEnd = Just 30_000
              , ccFinalizedThrough = Just 30_000
              , ccDatasetGeneration = 3
              , ccCoverageComplete = True
              , ccVolumeCoverageStart = Nothing
              , ccVolumeCoverageEnd = Nothing
              , ccVolumeFinalizedThrough = Nothing
              , ccVolumeCoverageComplete = False
              }
      validateBasketCurrentCandle 30_030 60 current `shouldBe` Right ()

    it "rejects stale current coverage using the configured lateness and interval" $ do
      let current =
            CandleCurrent
              { ccCandle = Nothing
              , ccCoverageStart = Just 0
              , ccCoverageEnd = Just 29_820
              , ccFinalizedThrough = Just 29_820
              , ccDatasetGeneration = 3
              , ccCoverageComplete = True
              , ccVolumeCoverageStart = Nothing
              , ccVolumeCoverageEnd = Nothing
              , ccVolumeFinalizedThrough = Nothing
              , ccVolumeCoverageComplete = False
              }
      validateBasketCurrentCandleWithPolicy
        200_000_000
        120
        0
        30_030
        60
        current
        `shouldFailTextWith` "watermark is stale"

    it "rejects fresh current coverage with severely stale finalization" $ do
      let current =
            CandleCurrent
              { ccCandle = Nothing
              , ccCoverageStart = Just 0
              , ccCoverageEnd = Just 30_000
              , ccFinalizedThrough = Just 60
              , ccDatasetGeneration = 3
              , ccCoverageComplete = True
              , ccVolumeCoverageStart = Nothing
              , ccVolumeCoverageEnd = Nothing
              , ccVolumeFinalizedThrough = Nothing
              , ccVolumeCoverageComplete = False
              }
      validateBasketCurrentCandleWithPolicy
        200_000_000
        120
        0
        30_030
        60
        current
        `shouldFailTextWith` "finalized watermark is stale"

    it "accepts current coverage and finalization at the exact freshness boundary" $ do
      let current =
            CandleCurrent
              { ccCandle = Nothing
              , ccCoverageStart = Just 0
              , ccCoverageEnd = Just 29_880
              , ccFinalizedThrough = Just 29_880
              , ccDatasetGeneration = 3
              , ccCoverageComplete = True
              , ccVolumeCoverageStart = Nothing
              , ccVolumeCoverageEnd = Nothing
              , ccVolumeFinalizedThrough = Nothing
              , ccVolumeCoverageComplete = False
              }
      validateBasketCurrentCandleWithPolicy
        200_000_000
        120
        0
        30_030
        60
        current
        `shouldBe` Right ()

    it "applies publication grace at every canonical interval boundary" $ do
      forM_ canonicalBasketCandleIntervals $ \interval -> do
        let boundary = interval * 1_000
            previousWatermark =
              CandleCurrent
                { ccCandle = Nothing
                , ccCoverageStart = Just 0
                , ccCoverageEnd = Just boundary
                , ccFinalizedThrough = Just $ boundary - interval
                , ccDatasetGeneration = 3
                , ccCoverageComplete = True
                , ccVolumeCoverageStart = Nothing
                , ccVolumeCoverageEnd = Nothing
                , ccVolumeFinalizedThrough = Nothing
                , ccVolumeCoverageComplete = False
                }
            validateAt now =
              validateBasketCurrentCandleWithPolicy
                200_000_000
                120
                15
                now
                interval
                previousWatermark
        validateAt (boundary + 120) `shouldBe` Right ()
        validateAt (boundary + 134) `shouldBe` Right ()
        validateAt (boundary + 135)
          `shouldFailTextWith` "finalized watermark is stale"

    it "does not let publication grace hide stale coverage or older finalization" $ do
      let boundary = 30_000
          staleCoverage =
            CandleCurrent
              { ccCandle = Nothing
              , ccCoverageStart = Just 0
              , ccCoverageEnd = Just $ boundary - 60
              , ccFinalizedThrough = Just $ boundary - 60
              , ccDatasetGeneration = 3
              , ccCoverageComplete = True
              , ccVolumeCoverageStart = Nothing
              , ccVolumeCoverageEnd = Nothing
              , ccVolumeFinalizedThrough = Nothing
              , ccVolumeCoverageComplete = False
              }
          staleFinalization =
            staleCoverage
              { ccCoverageEnd = Just boundary
              , ccFinalizedThrough = Just $ boundary - 120
              }
          validate =
            validateBasketCurrentCandleWithPolicy
              200_000_000
              120
              15
              (boundary + 120)
              60
      validate staleCoverage `shouldFailTextWith` "coverage watermark is stale"
      validate staleFinalization `shouldFailTextWith` "finalized watermark is stale"

    it "accepts the newly published watermark when grace expires" $ do
      let boundary = 30_000
          current =
            CandleCurrent
              { ccCandle = Nothing
              , ccCoverageStart = Just 0
              , ccCoverageEnd = Just boundary
              , ccFinalizedThrough = Just boundary
              , ccDatasetGeneration = 3
              , ccCoverageComplete = True
              , ccVolumeCoverageStart = Nothing
              , ccVolumeCoverageEnd = Nothing
              , ccVolumeFinalizedThrough = Nothing
              , ccVolumeCoverageComplete = False
              }
      validateBasketCurrentCandleWithPolicy
        200_000_000
        120
        15
        (boundary + 135)
        60
        current
        `shouldBe` Right ()

    it "rejects a current-bucket row marked finalized" $ do
      let current =
            CandleCurrent
              { ccCandle =
                  Just $
                    sampleCandleRow
                      { bcrBucketStart = 30_000
                      , bcrVolumeNumerator = Nothing
                      , bcrTradeCount = Nothing
                      , bcrVolumeComplete = False
                      }
              , ccCoverageStart = Just 0
              , ccCoverageEnd = Just 30_000
              , ccFinalizedThrough = Just 30_000
              , ccDatasetGeneration = 3
              , ccCoverageComplete = True
              , ccVolumeCoverageStart = Nothing
              , ccVolumeCoverageEnd = Nothing
              , ccVolumeFinalizedThrough = Nothing
              , ccVolumeCoverageComplete = False
              }
      validateBasketCurrentCandle 30_030 60 current
        `shouldFailTextWith` "incorrectly marked finalized"

    it "rejects a mutable current price outside the display domain" $ do
      let cap = 200_000_000
          current =
            CandleCurrent
              { ccCandle =
                  Just $
                    sampleCandleRow
                      { bcrBucketStart = 30_000
                      , bcrRawOpenPrice = cap
                      , bcrRawHighPrice = cap
                      , bcrRawLowPrice = cap
                      , bcrRawClosePrice = cap
                      , bcrPriceComplete = False
                      , bcrVolumeNumerator = Nothing
                      , bcrTradeCount = Nothing
                      , bcrVolumeComplete = False
                      }
              , ccCoverageStart = Just 0
              , ccCoverageEnd = Just 30_000
              , ccFinalizedThrough = Just 30_000
              , ccDatasetGeneration = 3
              , ccCoverageComplete = True
              , ccVolumeCoverageStart = Nothing
              , ccVolumeCoverageEnd = Nothing
              , ccVolumeFinalizedThrough = Nothing
              , ccVolumeCoverageComplete = False
              }
      validateBasketCurrentCandleWithCap cap 30_030 60 current
        `shouldFailTextWith` "outside the display domain"

    it "fails closed when price coverage is marked incomplete" $ do
      validateBasketCandlePage
        30_060
        60
        30_000
        sampleCandlePage {cpCoverageComplete = False}
        `shouldFailTextWith` "price coverage is incomplete"

      let current =
            CandleCurrent
              { ccCandle = Nothing
              , ccCoverageStart = Just 0
              , ccCoverageEnd = Just 30_000
              , ccFinalizedThrough = Just 30_000
              , ccDatasetGeneration = 3
              , ccCoverageComplete = False
              , ccVolumeCoverageStart = Nothing
              , ccVolumeCoverageEnd = Nothing
              , ccVolumeFinalizedThrough = Nothing
              , ccVolumeCoverageComplete = False
              }
      validateBasketCurrentCandle 30_030 60 current
        `shouldFailTextWith` "price coverage is incomplete"

    it "treats missing finalization metadata as maximally unhealthy lag" $ do
      coverageLagSeconds 1_900_000_000 Nothing `shouldBe` 1_900_000_000
      coverageLagSeconds 1_900_000_000 (Just 1_899_999_880) `shouldBe` 120

  describe "basketHistoryPointsWithVolume" $ do
    it "matches activity volume by the requested interval bucket" $ do
      let points = basketHistoryPointsWithVolume 60 basketRows volumeRows
      map bhpVolumeUsdc points `shouldBe` [123_456, 789]

    it "zero-fills candles without activity in their interval bucket" $ do
      let points = basketHistoryPointsWithVolume 60 basketRows (take 1 volumeRows)
      map bhpVolumeUsdc points `shouldBe` [123_456, 0]

  describe "decodePythUpdateForAdmission" $ do
    it "prepares strict six-feed latest payload admission inputs" $ do
      admission <-
        expectRight $
          decodePythUpdateForAdmission
            Nothing
            105
            10
            (hermesResponse ["0102"] configuredFeedIds [100 .. 105])
      puaUpdateData admission `shouldBe` [BS.pack [0x01, 0x02]]
      length (puaFeedIds admission) `shouldBe` 6
      puaMinPublishTime admission `shouldBe` 100
      puaMaxPublishTime admission `shouldBe` 105
      purUpdateData (puaPayload admission) `shouldBe` ["0x0102"]
      purPublishTimes (puaPayload admission) `shouldBe` [100 .. 105]

    it "uses the requested historical timestamp as the on-chain lower bound" $ do
      admission <-
        expectRight $
          decodePythUpdateForAdmission
            (Just 100)
            200
            10
            (hermesResponse ["0x01"] configuredFeedIds [100 .. 105])
      puaMinPublishTime admission `shouldBe` 100
      puaMaxPublishTime admission `shouldBe` 105

    it "rejects historical metadata that predates the requested timestamp" $ do
      decodePythUpdateForAdmission
        (Just 101)
        200
        10
        (hermesResponse ["0x01"] configuredFeedIds [100 .. 105])
        `shouldFailWith` "predates the requested publish time"

    it "rejects stale latest metadata before any RPC admission call" $ do
      decodePythUpdateForAdmission
        Nothing
        200
        10
        (hermesResponse ["0x01"] configuredFeedIds [100 .. 105])
        `shouldFailWith` "latest payload is 100s old"

    it "rejects missing, duplicate, or unexpected requested feed IDs" $ do
      let wrongFeedIds = "0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff" : drop 1 configuredFeedIds
      decodePythUpdateForAdmission
        Nothing
        105
        10
        (hermesResponse ["0x01"] wrongFeedIds [100 .. 105])
        `shouldFailWith` "six requested basket feed IDs"

    it "rejects malformed raw Hermes bytes instead of caching an empty payload" $ do
      decodePythUpdateForAdmission
        Nothing
        105
        10
        (hermesResponse ["0xnot-hex"] configuredFeedIds [100 .. 105])
        `shouldFailWith` "update data item 0 is invalid"

sampleTimings :: BasketHistoryTimings
sampleTimings =
  BasketHistoryTimings
    { bhtBackendTotalNs = 50_000_000
    , bhtDbPoolWaitNs = 7_000_000
    , bhtSnapshotQueryNs = 18_234_000
    , bhtVolumeQueryNs = 20_000_000
    , bhtResponseEncodeNs = 2_500_000
    }

zeroStageTimings :: BasketHistoryTimings
zeroStageTimings =
  BasketHistoryTimings
    { bhtBackendTotalNs = 0
    , bhtDbPoolWaitNs = 0
    , bhtSnapshotQueryNs = 0
    , bhtVolumeQueryNs = 0
    , bhtResponseEncodeNs = 0
    }

sampleCandleTimings :: BasketCandleTimings
sampleCandleTimings =
  BasketCandleTimings
    { bctBackendTotalNs = 15_000_000
    , bctDbPoolWaitNs = 1_000_000
    , bctQueryNs = 9_250_000
    , bctSingleFlightWaitNs = 750_000
    , bctResponseEncodeNs = 1_500_000
    }

sampleCandlePage :: CandlePage
sampleCandlePage =
  CandlePage
    { cpCandles = [sampleCandleRow]
    , cpPreviousCursor = Nothing
    , cpHasEarlier = False
    , cpCoverageStart = Just 0
    , cpCoverageEnd = Just 30_000
    , cpFinalizedThrough = Just 30_000
    , cpDatasetGeneration = 1
    , cpCoverageComplete = True
    , cpVolumeCoverageStart = Just 0
    , cpVolumeCoverageEnd = Just 30_000
    , cpVolumeFinalizedThrough = Just 30_000
    , cpVolumeCoverageComplete = True
    }

sampleCandleRange :: CandleRange
sampleCandleRange =
  CandleRange
    { crCandles = [sampleCandleRow]
    , crCoverageStart = Just 0
    , crCoverageEnd = Just 30_000
    , crFinalizedThrough = Just 30_000
    , crDatasetGeneration = 1
    , crCoverageComplete = True
    }

sampleCandleRow :: BasketCandleRow
sampleCandleRow =
  BasketCandleRow
    { bcrBucketStart = 0
    , bcrRawOpenPrice = 100_000_000
    , bcrRawHighPrice = 102_000_000
    , bcrRawLowPrice = 99_000_000
    , bcrRawClosePrice = 101_000_000
    , bcrSampleCount = 60
    , bcrQuality = CandleObserved
    , bcrRevision = 1
    , bcrPriceComplete = True
    , bcrVolumeNumerator = Just $ 1_000_000 * 10 ^ (20 :: Int)
    , bcrTradeCount = Just 2
    , bcrVolumeComplete = True
    }

historyParams :: BasketHistoryParams
historyParams =
  BasketHistoryParams
    { bhpRange = "24h"
    , bhpIntervalSeconds = 60
    , bhpIncludeComponents = False
    }

rollupConfig :: Config
rollupConfig =
  Config
    { cfgRpcUrl = ""
    , cfgChainId = 11155111
    , cfgPort = 3001
    , cfgCorsOrigins = []
    , cfgDeployments = []
    , cfgDatabaseUrl = Nothing
    , cfgIndexerStartBlock = 0
    , cfgPythBenchmarksUrl = ""
    , cfgPythHistoryUrl = ""
    , cfgPythHermesUrl = ""
    , cfgPythApiKey = Nothing
    , cfgPythBackfillDays = 7
    , cfgPythSampleIntervalSeconds = 60
    , cfgPythLatestMaxAgeSeconds = 10
    , cfgPythIngestionEnabled = False
    , cfgPerpsCandleWriteMode = PerpsCandleWritesDual
    , cfgPerpsCandleReadMode = PerpsCandleReadsRollup
    , cfgPerpsCandleReadIntervals = [60, 3600]
    , cfgPerpsCandleShadowSampleBps = 0
    , cfgPerpsCandleStrictCoverage = True
    , cfgPerpsCandleLatenessSeconds = 120
    , cfgPerpsCandleFinalizationGraceSeconds = 15
    , cfgPerpsRpcUrl = ""
    , cfgPerpsChainId = 421614
    , cfgPerpsUsdc = ""
    , cfgPerpsOrderRouter = "0xrouter"
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
    , cfgVaultHistoryHousePoolAddress = "0x0000000000000000000000000000000000000001"
    , cfgVaultHistorySeniorVaultAddress = "0x0000000000000000000000000000000000000002"
    , cfgVaultHistoryJuniorVaultAddress = "0x0000000000000000000000000000000000000003"
    , cfgVaultHistoryDeploymentBlock = 0
    , cfgVaultHistoryRpcUrl = "https://archive.example"
    , cfgVaultHistoryConfirmations = 12
    , cfgInsightsCompetitionRules = july2026Competition
    , cfgInsightsCompetitionReleaseManifest = rollupReleaseManifest
    , cfgRegistrationConfig = Nothing
    , cfgAaConfig = Nothing
    , cfgFaucetGuardConfig = Nothing
    , cfgFaucetPrivateKey = Nothing
    , cfgKeeperPrivateKey = Nothing
    , cfgKeeperPollSeconds = 1
    , cfgKeeperMaxBatchSize = 20
    , cfgKeeperConfirmations = 1
    , cfgKeeperGasBufferBps = 2000
    , cfgKeeperFeeBufferBps = 2500
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

rollupReleaseManifest :: CompetitionReleaseManifest
rollupReleaseManifest =
  CompetitionReleaseManifest
    { crmReleaseId = "perps-rollup-test"
    , crmChainId = 421614
    , crmUsdc = ""
    , crmOrderRouter = "0xrouter"
    , crmMarginClearinghouse = ""
    , crmAccountLens = ""
    , crmCfdEngine = ""
    , crmCfdEngineLens = ""
    , crmSettlementSidecar = ""
    , crmPletherOracle = ""
    , crmIndexerStartBlock = 0
    }

testApiError :: ApiError
testApiError =
  ApiError
    { errCode = NetworkError
    , errMessage = "expected test sentinel"
    , errDetails = Nothing
    }

basketRows :: [BasketHistorySnapshotRow]
basketRows =
  [ BasketHistorySnapshotRow
      { bhsrTimestamp = 125
      , bhsrIntervalSeconds = 5
      , bhsrBasketPrice = 101_660_000
      , bhsrComponents = Nothing
      }
  , BasketHistorySnapshotRow
      { bhsrTimestamp = 181
      , bhsrIntervalSeconds = 5
      , bhsrBasketPrice = 101_670_000
      , bhsrComponents = Nothing
      }
  ]

volumeRows :: [PerpsMarketVolumeBucketRow]
volumeRows =
  [ PerpsMarketVolumeBucketRow
      { pmvbrBucket = 2
      , pmvbrVolumeUsdc = 123_456
      }
  , PerpsMarketVolumeBucketRow
      { pmvbrBucket = 3
      , pmvbrVolumeUsdc = 789
      }
  ]

configuredFeedIds :: [Text]
configuredFeedIds = bcFeedId <$> basketComponents

hermesResponse :: [Text] -> [Text] -> [Integer] -> LBS.ByteString
hermesResponse updateData feedIds publishTimes =
  encode $
    object
      [ "binary" .= object ["data" .= updateData]
      , "parsed"
          .= zipWith
            (\feedId publishTime ->
              object
                [ "id" .= feedId
                , "price" .= object ["publish_time" .= publishTime]
                ]
            )
            feedIds
            publishTimes
      ]

shouldFailWith :: Either ApiError value -> Text -> Expectation
shouldFailWith result expected =
  case result of
    Left err -> errMessage err `shouldSatisfy` T.isInfixOf expected
    Right _ -> expectationFailure "expected Pyth admission preparation to fail"

expectRight :: (Show err) => Either err value -> IO value
expectRight result =
  case result of
    Right value -> pure value
    Left err -> expectationFailure ("expected Right, got " <> show err) >> fail "unreachable"

shouldFailTextWith :: Either Text value -> Text -> Expectation
shouldFailTextWith result expected =
  case result of
    Left err -> err `shouldSatisfy` T.isInfixOf expected
    Right _ -> expectationFailure "expected strict candle validation to fail"
