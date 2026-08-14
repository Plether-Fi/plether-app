module Plether.Handlers.Perps
  ( getBasketHistory
  , getBasketHistoryTimed
  , getBasketHistoryWithSourcesTimed
  , getLegacyBasketHistoryVolumeRowsTimed
  , getBasketCandlePageTimed
  , getBasketCurrentCandleTimed
  , BasketHistoryFetch (..)
  , BasketHistoryTimings (..)
  , BasketCandleFetch (..)
  , BasketCandleTimings (..)
  , basketCandleServerTiming
  , basketCandleTimingMetrics
  , coverageLagSeconds
  , validateBasketCandlePage
  , validateBasketCandlePageWithCap
  , validateBasketCandlePageWithPolicy
  , validateBasketCurrentCandle
  , validateBasketCurrentCandleWithCap
  , validateBasketCurrentCandleWithPolicy
  , basketHistoryServerTiming
  , basketHistoryTimingMetrics
  , durationMilliseconds
  , basketHistoryPointsWithVolume
  , basketHistoryFromCandleRows
  , boundedBasketHistoryInterval
  , isBoundedComponentHistoryRequest
  , validateRollupHistoryRange
  , validateRollupHistoryRangeWithPolicy
  , getBasketLatest
  , getCachedLatestPythUpdate
  , getPythUpdate
  , getRevealPayload
  , PythUpdateAdmission (..)
  , decodePythUpdateForAdmission
  ) where

import Control.Exception (evaluate)
import Control.Concurrent.STM
  ( atomically
  , modifyTVar'
  , readTVar
  , writeTVar
  )
import Data.Aeson (FromJSON (..), Value, eitherDecode, withObject, (.:))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.List (sort)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Network.HTTP.Client
  ( Manager
  , httpLbs
  , parseRequest
  , requestHeaders
  , responseBody
  , responseHeaders
  , responseStatus
  , setQueryString
  )
import Network.HTTP.Types.Status (statusCode)
import Plether.Cache (AppCache (..))
import Plether.Config
  ( Config (..)
  , perpsCandleRollupReadEnabled
  )
import Plether.Database (DbPool, withDb)
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple.Transaction
  ( IsolationLevel (RepeatableRead)
  , ReadWriteMode (ReadOnly)
  , TransactionMode (..)
  , withTransactionMode
  )
import Plether.Database.Candles
  ( BasketCandleRow (..)
  , BasketDefinitionIdentity (..)
  , CandleCurrent (..)
  , CandlePage (..)
  , CandleRange (..)
  , CandleQuality (..)
  , defaultBasketSeriesId
  , getActiveBasketDefinitionIdentity
  , getBasketCandlePageSnapshot
  , getBasketCandleRange
  , getCurrentBasketCandle
  )
import Plether.Database.Schema
  ( BasketHistorySnapshotRow (..)
  , BasketSnapshotRow (..)
  , PerpsMarketVolumeBucketRow (..)
  , PythUpdatePayloadRow (..)
  , getBasketSnapshots
  , getPerpsMarketVolumeBuckets
  , getLatestBasketSnapshot
  , getLatestPythUpdatePayload
  , getPythUpdatePayloadForWindow
  , isHistoricalRevealPayload
  )
import Plether.Logging (field, logWarnEvery)
import Plether.Types
import qualified Plether.Types.Error as E
import Plether.Ethereum.Client (EthClient)
import Plether.Ethereum.Contracts.Perps (validatePythUpdateData, validateUniquePythUpdateData)
import Plether.Pyth.Basket (BasketComponent (..), basketComponents, normalizeFeedId)
import Plether.Pyth.Hermes (resolveHermesApiKey)
import Plether.Pyth.RevealPayload (validateLatestPublishTimes, validatePublishTimes)
import Plether.Utils.Hex (hexToByteStringEither)

data BasketHistoryFetch = BasketHistoryFetch
  { bhfResponse :: ApiResponse BasketHistory
  , bhfReadSource :: Text
  , bhfPoolWaitNs :: Word64
  , bhfSnapshotQueryNs :: Word64
  , bhfVolumeQueryNs :: Word64
  , bhfSnapshotRows :: Int
  , bhfVolumeRows :: Int
  }
  deriving stock (Show)

data BasketCandleFetch a = BasketCandleFetch
  { bcfResponse :: ApiResponse a
  , bcfReadSource :: Text
  , bcfPoolWaitNs :: Word64
  , bcfQueryNs :: Word64
  , bcfRowCount :: Int
  , bcfFinalizedThrough :: Maybe Integer
  , bcfDatasetGeneration :: Integer
  }
  deriving stock (Show)

defaultBasketDisplayPriceCap :: Integer
defaultBasketDisplayPriceCap = 200_000_000

data BasketCandleTimings = BasketCandleTimings
  { bctBackendTotalNs :: Word64
  , bctDbPoolWaitNs :: Word64
  , bctQueryNs :: Word64
  , bctResponseEncodeNs :: Word64
  }
  deriving stock (Eq, Show)

basketCandleTimingMetrics :: BasketCandleTimings -> [(Text, Word64)]
basketCandleTimingMetrics timings =
  [ ("plether_app", bctBackendTotalNs timings)
  , ("plether_db_pool_wait", bctDbPoolWaitNs timings)
  , ("plether_db_candles", bctQueryNs timings)
  , ("plether_response_encode", bctResponseEncodeNs timings)
  , ("plether_other", unattributedCandleDuration timings)
  ]

basketCandleServerTiming :: BasketCandleTimings -> Text
basketCandleServerTiming =
  T.intercalate ", "
    . map (\(metric, duration) -> metric <> ";dur=" <> renderDurationMilliseconds duration)
    . basketCandleTimingMetrics

unattributedCandleDuration :: BasketCandleTimings -> Word64
unattributedCandleDuration BasketCandleTimings {..} =
  bctBackendTotalNs
    - min
      bctBackendTotalNs
      (bctDbPoolWaitNs + bctQueryNs + bctResponseEncodeNs)

getBasketCandlePageTimed
  :: DbPool
  -> Config
  -> Integer
  -> Integer
  -> IO (Either ApiError (BasketCandleFetch BasketCandlePage))
getBasketCandlePageTimed pool cfg interval cursor = do
  now <- floor <$> getPOSIXTime
  poolStartedAt <- getMonotonicTimeNSec
  (mDefinition, page, poolWaitNs, queryNs) <- withDb pool $ \conn -> do
    connectionReadyAt <- getMonotonicTimeNSec
    let poolWaitNs = connectionReadyAt - poolStartedAt
    queryStartedAt <- getMonotonicTimeNSec
    -- A client may request the bounded page ahead of the wall clock. Never use
    -- that future timestamp to select a scheduled basket definition.
    (mDefinition, page) <-
      getBasketCandlePageSnapshot
        conn
        (min (cursor - 1) now)
        (cfgPerpsChainId cfg)
        (cfgPerpsOrderRouter cfg)
        interval
        cursor
    queryFinishedAt <- getMonotonicTimeNSec
    pure (mDefinition, page, poolWaitNs, queryFinishedAt - queryStartedAt)
  case mDefinition of
    Nothing ->
      pure $ Left $ E.networkError "Active basket definition identity is unavailable"
    Just definition ->
      case
          validateBasketCandlePageWithPolicy
            (bdiDisplayPriceCap definition)
            (cfgPerpsCandleLatenessSeconds cfg)
            (cfgPerpsCandleFinalizationGraceSeconds cfg)
            now
            interval
            cursor
            page
        of
        Left reason -> do
          logUnhealthyCandleCoverage "historical" now interval page reason
          pure $
            Left $
              E.networkError $
                "Candle rollup page failed strict coverage validation: " <> reason
        Right () ->
          pure $
            Right
              BasketCandleFetch
                { bcfResponse = mkResponse 0 (cfgPerpsChainId cfg) $ candlePageToResponse definition interval cursor page
                , bcfReadSource = "rollup"
                , bcfPoolWaitNs = poolWaitNs
                , bcfQueryNs = queryNs
                , bcfRowCount = length $ cpCandles page
                , bcfFinalizedThrough = cpFinalizedThrough page
                , bcfDatasetGeneration = cpDatasetGeneration page
                }

getBasketCurrentCandleTimed
  :: DbPool
  -> Config
  -> Integer
  -> IO (Either ApiError (BasketCandleFetch BasketCurrentCandle))
getBasketCurrentCandleTimed pool cfg interval = do
  now <- floor <$> getPOSIXTime
  poolStartedAt <- getMonotonicTimeNSec
  (mDefinition, current, poolWaitNs, queryNs) <- withDb pool $ \conn -> withCandleReadSnapshot conn $ do
    connectionReadyAt <- getMonotonicTimeNSec
    let poolWaitNs = connectionReadyAt - poolStartedAt
    queryStartedAt <- getMonotonicTimeNSec
    mDefinition <- getActiveBasketDefinitionIdentity conn now
    current <-
      getCurrentBasketCandle
        conn
        (maybe defaultBasketSeriesId bdiSeriesId mDefinition)
        (cfgPerpsChainId cfg)
        (cfgPerpsOrderRouter cfg)
        interval
        now
    queryFinishedAt <- getMonotonicTimeNSec
    pure (mDefinition, current, poolWaitNs, queryFinishedAt - queryStartedAt)
  case mDefinition of
    Nothing -> pure $ Left $ E.networkError "Active basket definition identity is unavailable"
    Just definition ->
      case
          validateBasketCurrentCandleWithPolicy
            (bdiDisplayPriceCap definition)
            (cfgPerpsCandleLatenessSeconds cfg)
            (cfgPerpsCandleFinalizationGraceSeconds cfg)
            now
            interval
            current
        of
        Left reason -> do
          logUnhealthyCurrentCoverage now interval current reason
          pure $
            Left $
              E.networkError $ "Current candle rollup failed strict validation: " <> reason
        Right () -> do
          let response =
                BasketCurrentCandle
                  { bccIntervalSeconds = interval
                  , bccSeriesId = bdiSeriesId definition
                  , bccConfigurationHash = bdiConfigurationHash definition
                  , bccDisplayPriceCap = bdiDisplayPriceCap definition
                  , bccDatasetGeneration = ccDatasetGeneration current
                  , bccCoverageStart = ccCoverageStart current
                  , bccCoverageEnd = ccCoverageEnd current
                  , bccFinalizedThrough = ccFinalizedThrough current
                  , bccCoverageComplete = ccCoverageComplete current
                  , bccCandle = candleRowToApi <$> ccCandle current
                  }
          pure $
            Right
              BasketCandleFetch
                { bcfResponse = mkResponse 0 (cfgPerpsChainId cfg) response
                , bcfReadSource = "rollup_current"
                , bcfPoolWaitNs = poolWaitNs
                , bcfQueryNs = queryNs
                , bcfRowCount = maybe 0 (const 1) $ ccCandle current
                , bcfFinalizedThrough = ccFinalizedThrough current
                , bcfDatasetGeneration = ccDatasetGeneration current
                }

candlePageToResponse :: BasketDefinitionIdentity -> Integer -> Integer -> CandlePage -> BasketCandlePage
candlePageToResponse definition interval cursor CandlePage {..} =
  BasketCandlePage
    { bcpIntervalSeconds = interval
    , bcpCursor = cursor
    , bcpSeriesId = bdiSeriesId definition
    , bcpConfigurationHash = bdiConfigurationHash definition
    , bcpDisplayPriceCap = bdiDisplayPriceCap definition
    , bcpPreviousCursor = cpPreviousCursor
    , bcpHasEarlier = cpHasEarlier
    , bcpCoverageStart = cpCoverageStart
    , bcpCoverageEnd = cpCoverageEnd
    , bcpFinalizedThrough = cpFinalizedThrough
    , bcpDatasetGeneration = cpDatasetGeneration
    , bcpCoverageComplete = cpCoverageComplete
    , bcpCandles = map candleRowToApi cpCandles
    }

candleRowToApi :: BasketCandleRow -> BasketCandle
candleRowToApi BasketCandleRow {..} =
  BasketCandle
    { bcTimestamp = bcrBucketStart
    , bcRawOpenPrice = bcrRawOpenPrice
    , bcRawHighPrice = bcrRawHighPrice
    , bcRawLowPrice = bcrRawLowPrice
    , bcRawClosePrice = bcrRawClosePrice
    , bcVolumeUsdc = (`div` 10 ^ (20 :: Int)) <$> bcrVolumeNumerator
    , bcTradeCount = bcrTradeCount
    , bcSampleCount = fromIntegral bcrSampleCount
    , bcQuality = candleQualityText bcrQuality
    , bcRevision = bcrRevision
    , bcPriceComplete = bcrPriceComplete
    , bcVolumeComplete = bcrVolumeComplete
    }

candleQualityText :: CandleQuality -> Text
candleQualityText = \case
  CandleObserved -> "observed"
  CandleLegacySampled -> "legacy_sampled"
  CandleMixed -> "mixed"

-- | Validate the storage page before exposing it through the public API. A
-- page that reaches the wall-clock boundary is intentionally allowed to stop
-- at its finalized watermark; it must never include the mutable bucket. Fully
-- historical pages, by contrast, must be finalized through their cursor.
-- Coverage may begin within the oldest page, but only when pagination metadata
-- proves that this is the inception page.
validateBasketCandlePage
  :: Integer
  -> Integer
  -> Integer
  -> CandlePage
  -> Either Text ()
validateBasketCandlePage = validateBasketCandlePageWithCap defaultBasketDisplayPriceCap

validateBasketCandlePageWithCap
  :: Integer
  -> Integer
  -> Integer
  -> Integer
  -> CandlePage
  -> Either Text ()
validateBasketCandlePageWithCap displayPriceCap now interval cursor CandlePage {..}
  | displayPriceCap <= 0 = Left "basket display price cap is not positive"
  | interval <= 0 = Left "interval must be positive"
  | now < 0 = Left "backend clock is before the Unix epoch"
  | cursor <= 0 || cursor `mod` (interval * 500) /= 0 = Left "cursor is not page-aligned"
  | not cpCoverageComplete = Left "combined price and volume coverage is incomplete"
  | cpDatasetGeneration <= 0 = Left "dataset generation is unavailable"
  | length cpCandles > 500 = Left "page contains more than 500 candles"
  | otherwise = do
      coverageStart <- maybe (Left "coverage start is unavailable") Right cpCoverageStart
      coverageEnd <- maybe (Left "coverage end is unavailable") Right cpCoverageEnd
      finalizedThrough <- maybe (Left "finalized watermark is unavailable") Right cpFinalizedThrough
      requireAligned "coverage start" coverageStart
      requireAligned "coverage end" coverageEnd
      requireAligned "finalized watermark" finalizedThrough
      if coverageStart >= coverageEnd
        then Left "coverage window is empty or reversed"
        else Right ()
      if finalizedThrough < coverageStart || finalizedThrough > coverageEnd
        then Left "finalized watermark is outside the coverage window"
        else Right ()
      let pageStart = cursor - interval * 500
          wallClockBoundary = (now `div` interval) * interval
          requestedClosedEnd = min cursor wallClockBoundary
          inceptionClipped = coverageStart > pageStart
          effectiveStart = max pageStart coverageStart
          effectiveEnd = minimum [requestedClosedEnd, coverageEnd, finalizedThrough]
          terminalClipped = coverageEnd < cursor
          validSparsePreviousCursor = case cpPreviousCursor of
            Just previousCursor ->
              previousCursor > 0
                && previousCursor `mod` (interval * 500) == 0
                && previousCursor < cursor
                && previousCursor <= pageStart
            Nothing -> False
          -- A request can land entirely after the latest covered bucket (for
          -- example during a weekend market gap). Storage deliberately
          -- returns an empty bridge page whose sparse cursor jumps directly
          -- to the most recent page that can contain data.
          sparseBridgePage =
            null cpCandles
              && cpHasEarlier
              && validSparsePreviousCursor
      if pageStart < 0
        then Left "page starts before the Unix epoch"
        else Right ()
      if effectiveStart >= effectiveEnd && not sparseBridgePage
        then Left "covered page window has no finalized buckets"
        else Right ()
      if cursor <= wallClockBoundary
          && finalizedThrough < min cursor coverageEnd
        then Left "closed page is not finalized through its covered end"
        else Right ()
      if inceptionClipped && (cpHasEarlier || isJust cpPreviousCursor)
        then Left "inception-clipped page claims an earlier page"
        else Right ()
      if cpHasEarlier && coverageStart >= pageStart
        then Left "page claims earlier data outside its coverage window"
        else Right ()
      if cpHasEarlier && pageStart <= 0
        then Left "page claims earlier data before the Unix epoch"
        else Right ()
      case (cpHasEarlier, cpPreviousCursor) of
        (False, Nothing) -> Right ()
        (False, Just _) -> Left "pagination cursor is present without earlier data"
        (True, Nothing) -> Left "pagination cursor is missing despite earlier data"
        (True, Just previousCursor)
          | previousCursor <= 0
              || previousCursor `mod` (interval * 500) /= 0 ->
              Left "previous cursor is not page-aligned"
          | previousCursor >= cursor || previousCursor > pageStart ->
              Left "previous cursor does not point to an earlier page"
          | otherwise -> Right ()
      if terminalClipped && coverageEnd <= pageStart && not sparseBridgePage
        then Left "requested page starts at or after the coverage terminal"
        else Right ()
      validateAscendingRows cpCandles
      mapM_
        (validateHistoricalRow displayPriceCap interval effectiveStart effectiveEnd finalizedThrough)
        cpCandles
  where
    requireAligned label timestamp
      | timestamp >= 0 && timestamp `mod` interval == 0 = Right ()
      | otherwise = Left $ label <> " is not interval-aligned"

-- | Apply deployment freshness policy in addition to the page-shape and
-- finalization checks. Historical rows can be immutable while the global
-- source watermark still goes stale, so every public native page must prove
-- that both writers have checked in recently.
validateBasketCandlePageWithPolicy
  :: Integer -- immutable display-price cap
  -> Integer -- configured candle lateness tolerance
  -> Integer -- bounded finalization-publication grace
  -> Integer -- backend clock
  -> Integer -- candle interval
  -> Integer -- fixed-page cursor
  -> CandlePage
  -> Either Text ()
validateBasketCandlePageWithPolicy displayPriceCap latenessSeconds finalizationGraceSeconds now interval cursor page = do
  validateBasketCandlePageWithCap displayPriceCap now interval cursor page
  coverageEnd <- maybe (Left "coverage end is unavailable") Right $ cpCoverageEnd page
  finalizedThrough <- maybe (Left "finalized watermark is unavailable") Right $ cpFinalizedThrough page
  validateCoverageFreshness latenessSeconds now interval coverageEnd
  validateFinalizationFreshness latenessSeconds finalizationGraceSeconds now interval finalizedThrough

-- | The current response is allowed to contain incomplete/nullable OHLCV,
-- but its metadata and row shape must still be coherent. Storage returns
-- metadata independently of the nullable row, so row checks apply only when an
-- active-bucket observation exists.
validateBasketCurrentCandle :: Integer -> Integer -> CandleCurrent -> Either Text ()
validateBasketCurrentCandle = validateBasketCurrentCandleWithCap defaultBasketDisplayPriceCap

validateBasketCurrentCandleWithCap
  :: Integer -> Integer -> Integer -> CandleCurrent -> Either Text ()
validateBasketCurrentCandleWithCap displayPriceCap =
  validateBasketCurrentCandleWithPolicy displayPriceCap 0 0

-- | Validate the mutable response against the source-watermark freshness
-- policy used by this deployment. Coverage timestamps are interval-aligned and
-- remain subject to the source-lateness floor. Finalization additionally gets
-- the bounded publication grace needed by the asynchronous writer loop, while
-- a stopped writer still makes an otherwise complete dataset fail closed.
validateBasketCurrentCandleWithPolicy
  :: Integer -- immutable display-price cap
  -> Integer -- configured candle lateness tolerance
  -> Integer -- bounded finalization-publication grace
  -> Integer -- backend clock
  -> Integer -- candle interval
  -> CandleCurrent
  -> Either Text ()
validateBasketCurrentCandleWithPolicy displayPriceCap latenessSeconds finalizationGraceSeconds now interval CandleCurrent {..}
  | displayPriceCap <= 0 = Left "basket display price cap is not positive"
  | interval <= 0 = Left "interval must be positive"
  | latenessSeconds < 0 = Left "candle lateness tolerance is negative"
  | finalizationGraceSeconds < 0 = Left "candle finalization grace is negative"
  | now < 0 = Left "backend clock is before the Unix epoch"
  | not ccCoverageComplete = Left "combined price and volume coverage is incomplete"
  | ccDatasetGeneration <= 0 = Left "dataset generation is unavailable"
  | otherwise = do
      coverageStart <- maybe (Left "coverage start is unavailable") Right ccCoverageStart
      coverageEnd <- maybe (Left "coverage end is unavailable") Right ccCoverageEnd
      finalizedThrough <- maybe (Left "finalized watermark is unavailable") Right ccFinalizedThrough
      let currentBucketStart = (now `div` interval) * interval
      if any (\timestamp -> timestamp < 0 || timestamp `mod` interval /= 0) [coverageStart, coverageEnd, finalizedThrough]
        then Left "current coverage metadata is not interval-aligned"
        else Right ()
      if coverageStart >= coverageEnd
        then Left "current coverage window is empty or reversed"
        else Right ()
      validateCoverageFreshness latenessSeconds now interval coverageEnd
      if finalizedThrough < coverageStart || finalizedThrough > coverageEnd
        then Left "finalized watermark is outside the coverage window"
        else Right ()
      validateFinalizationFreshness latenessSeconds finalizationGraceSeconds now interval finalizedThrough
      if finalizedThrough > currentBucketStart
        then Left "finalized watermark extends into the mutable bucket"
        else Right ()
      case ccCandle of
        Nothing -> Right ()
        Just candle
          | bcrBucketStart candle /= currentBucketStart ->
              Left "current candle timestamp does not match the mutable bucket"
          | bcrPriceComplete candle || bcrVolumeComplete candle ->
              Left "current candle is incorrectly marked finalized"
          | otherwise -> validateCandleRow displayPriceCap False candle

validateHistoricalRow
  :: Integer
  -> Integer
  -> Integer
  -> Integer
  -> Integer
  -> BasketCandleRow
  -> Either Text ()
validateHistoricalRow displayPriceCap interval effectiveStart effectiveEnd finalizedThrough row
  | bcrBucketStart row `mod` interval /= 0 = Left "candle timestamp is not interval-aligned"
  | bcrBucketStart row < effectiveStart = Left "candle precedes the covered page window"
  | bcrBucketStart row >= effectiveEnd = Left "candle exceeds the finalized page window"
  | bcrBucketStart row + interval > finalizedThrough = Left "candle extends past the finalized watermark"
  | not (bcrPriceComplete row && bcrVolumeComplete row) = Left "historical candle is incomplete"
  | not (isJust $ bcrVolumeNumerator row) || not (isJust $ bcrTradeCount row) =
      Left "historical candle has unknown volume"
  | otherwise = validateCandleRow displayPriceCap True row

validateAscendingRows :: [BasketCandleRow] -> Either Text ()
validateAscendingRows rows
  | and $ zipWith (<) timestamps (drop 1 timestamps) = Right ()
  | otherwise = Left "candle timestamps are not strictly ascending"
  where
    timestamps = map bcrBucketStart rows

validateCandleRow :: Integer -> Bool -> BasketCandleRow -> Either Text ()
validateCandleRow displayPriceCap requireComplete BasketCandleRow {..}
  | bcrBucketStart < 0 = Left "candle timestamp is negative"
  | any (<= 0) prices = Left "candle contains a non-positive price"
  | any (>= displayPriceCap) prices = Left "candle price is outside the display domain"
  | bcrRawLowPrice > minimum [bcrRawOpenPrice, bcrRawClosePrice] = Left "candle low exceeds open or close"
  | bcrRawHighPrice < maximum [bcrRawOpenPrice, bcrRawClosePrice] = Left "candle high is below open or close"
  | bcrRawLowPrice > bcrRawHighPrice = Left "candle low exceeds high"
  | bcrSampleCount <= 0 = Left "candle sample count is not positive"
  | bcrRevision <= 0 = Left "candle revision is not positive"
  | maybe False (< 0) bcrVolumeNumerator = Left "candle volume is negative"
  | maybe False (< 0) bcrTradeCount = Left "candle trade count is negative"
  | bcrVolumeComplete && (not (isJust bcrVolumeNumerator) || not (isJust bcrTradeCount)) =
      Left "complete candle has unknown volume"
  | requireComplete && not (bcrPriceComplete && bcrVolumeComplete) = Left "historical candle is incomplete"
  | otherwise = Right ()
  where
    prices = [bcrRawOpenPrice, bcrRawHighPrice, bcrRawLowPrice, bcrRawClosePrice]

coverageLagSeconds :: Integer -> Maybe Integer -> Integer
coverageLagSeconds now = maybe (max 0 now) (max 0 . (now -))

withCandleReadSnapshot :: Connection -> IO value -> IO value
withCandleReadSnapshot =
  withTransactionMode $
    TransactionMode
      { isolationLevel = RepeatableRead
      , readWriteMode = ReadOnly
      }

logUnhealthyCandleCoverage :: Text -> Integer -> Integer -> CandlePage -> Text -> IO ()
logUnhealthyCandleCoverage requestKind now interval CandlePage {..} reason =
  logWarnEvery
    30
    "perps_candle_coverage_unhealthy"
    "Perps candle rollup coverage failed public-read validation"
    [ field "request_kind" requestKind
    , field "interval_seconds" interval
    , field "coverage_available" $
        isJust cpCoverageStart && isJust cpCoverageEnd && isJust cpFinalizedThrough
    , field "coverage_start" cpCoverageStart
    , field "coverage_end" cpCoverageEnd
    , field "finalized_through" cpFinalizedThrough
    , field "lag_seconds" $ coverageLagSeconds now cpFinalizedThrough
    , field "dataset_generation" cpDatasetGeneration
    , field "complete" cpCoverageComplete
    , field "reason" reason
    ]

logUnhealthyCurrentCoverage :: Integer -> Integer -> CandleCurrent -> Text -> IO ()
logUnhealthyCurrentCoverage now interval current reason =
  logWarnEvery
    30
    "perps_candle_coverage_unhealthy"
    "Current Perps candle coverage failed public-read validation"
    [ field "request_kind" ("current" :: Text)
    , field "interval_seconds" interval
    , field "coverage_available" $
        isJust (ccCoverageStart current)
          && isJust (ccCoverageEnd current)
          && isJust (ccFinalizedThrough current)
    , field "coverage_start" $ ccCoverageStart current
    , field "coverage_end" $ ccCoverageEnd current
    , field "finalized_through" $ ccFinalizedThrough current
    , field "lag_seconds" $ coverageLagSeconds now $ ccFinalizedThrough current
    , field "dataset_generation" $ ccDatasetGeneration current
    , field "complete" $ ccCoverageComplete current
    , field "reason" reason
    ]

maxBasketHistoryPoints :: Integer
maxBasketHistoryPoints = 12_000

-- Preserve every normal chart shape (the largest is seven days of minute
-- bars) while preventing direct callers from requesting hundreds of thousands
-- of snapshots, for example one year at a one-minute interval.
boundedBasketHistoryInterval :: Integer -> Integer -> Integer
boundedBasketHistoryInterval rangeSeconds requestedInterval =
  case filter (>= requiredInterval) canonicalBasketCandleIntervals of
    interval : _ -> interval
    [] -> 86_400
  where
    normalizedInterval = max 60 requestedInterval
    targetBuckets = max 1 (maxBasketHistoryPoints - 4)
    minimumBoundedInterval =
      (max 0 rangeSeconds + targetBuckets - 1) `div` targetBuckets
    requiredInterval = max normalizedInterval minimumBoundedInterval

-- Component payloads are not part of the OHLCV read model. Keep the only
-- remaining raw-source public shape deliberately small and cacheable instead
-- of permitting arbitrary long-range snapshot/activity scans.
isBoundedComponentHistoryRequest :: BasketHistoryParams -> Bool
isBoundedComponentHistoryRequest params =
  not (bhpIncludeComponents params)
    || (bhpRange params == "24h" && bhpIntervalSeconds params == 3_600)

data BasketHistoryTimings = BasketHistoryTimings
  { bhtBackendTotalNs :: Word64
  , bhtDbPoolWaitNs :: Word64
  , bhtSnapshotQueryNs :: Word64
  , bhtVolumeQueryNs :: Word64
  , bhtResponseEncodeNs :: Word64
  }
  deriving stock (Eq, Show)

basketHistoryTimingMetrics :: BasketHistoryTimings -> [(Text, Word64)]
basketHistoryTimingMetrics timings =
  [ ("plether_app", bhtBackendTotalNs timings)
  , ("plether_db_pool_wait", bhtDbPoolWaitNs timings)
  , ("plether_db_snapshots", bhtSnapshotQueryNs timings)
  , ("plether_db_volume", bhtVolumeQueryNs timings)
  , ("plether_response_encode", bhtResponseEncodeNs timings)
  , ("plether_other", unattributedDuration timings)
  ]

basketHistoryServerTiming :: BasketHistoryTimings -> Text
basketHistoryServerTiming =
  T.intercalate ", "
    . map (\(metric, duration) -> metric <> ";dur=" <> renderDurationMilliseconds duration)
    . basketHistoryTimingMetrics

durationMilliseconds :: Word64 -> Double
durationMilliseconds durationNs = fromIntegral durationNs / 1_000_000

renderDurationMilliseconds :: Word64 -> Text
renderDurationMilliseconds durationNs =
  let durationMicros = durationNs `div` 1_000
      (wholeMilliseconds, fractionalMicros) = durationMicros `divMod` 1_000
   in T.pack (show wholeMilliseconds)
        <> "."
        <> T.justifyRight 3 '0' (T.pack $ show fractionalMicros)

unattributedDuration :: BasketHistoryTimings -> Word64
unattributedDuration BasketHistoryTimings {..} =
  bhtBackendTotalNs
    - min
      bhtBackendTotalNs
      ( bhtDbPoolWaitNs
          + bhtSnapshotQueryNs
          + bhtVolumeQueryNs
          + bhtResponseEncodeNs
      )

getBasketHistory
  :: DbPool
  -> Config
  -> BasketHistoryParams
  -> IO (Either ApiError (ApiResponse BasketHistory))
getBasketHistory pool cfg params = do
  result <- getBasketHistoryTimed pool cfg params
  pure $ bhfResponse <$> result

getBasketHistoryTimed
  :: DbPool
  -> Config
  -> BasketHistoryParams
  -> IO (Either ApiError BasketHistoryFetch)
getBasketHistoryTimed pool cfg params =
  getBasketHistoryWithSourcesTimed
    (getLegacyBasketHistoryTimed pool cfg)
    (getRollupBasketHistoryTimed pool cfg)
    cfg
    params

-- | Select a source before evaluating either fetch action. Besides avoiding raw
-- scans in rollup mode, this shape makes that property testable without a DB.
getBasketHistoryWithSourcesTimed
  :: (BasketHistoryParams -> IO (Either ApiError BasketHistoryFetch))
  -> (BasketHistoryParams -> IO (Either ApiError BasketHistoryFetch))
  -> Config
  -> BasketHistoryParams
  -> IO (Either ApiError BasketHistoryFetch)
getBasketHistoryWithSourcesTimed legacyFetch rollupFetch cfg params
  | historyRollupReadEnabled cfg params = rollupFetch params
  | otherwise = legacyFetch params

-- | Component history is a bounded price/composition compatibility payload,
-- not an OHLCV source. Skipping its independent activity scan also makes the
-- absence explicit in request telemetry instead of timing an empty action.
getLegacyBasketHistoryVolumeRowsTimed
  :: Bool
  -> IO [PerpsMarketVolumeBucketRow]
  -> IO ([PerpsMarketVolumeBucketRow], Word64, Int)
getLegacyBasketHistoryVolumeRowsTimed includeComponents fetchVolumeRows
  | includeComponents = pure ([], 0, 0)
  | otherwise = do
      queryStartedAt <- getMonotonicTimeNSec
      rows <- fetchVolumeRows
      rowCount <- evaluate $ length rows
      queryFinishedAt <- getMonotonicTimeNSec
      pure (rows, queryFinishedAt - queryStartedAt, rowCount)

historyRollupReadEnabled :: Config -> BasketHistoryParams -> Bool
historyRollupReadEnabled cfg params =
  let effectiveInterval =
        boundedBasketHistoryInterval
          (basketRangeSeconds $ bhpRange params)
          (bhpIntervalSeconds params)
   in not (bhpIncludeComponents params)
    && perpsCandleRollupReadEnabled
      (cfgPerpsCandleReadMode cfg)
      (cfgPerpsCandleStrictCoverage cfg)
      (cfgPerpsCandleReadIntervals cfg)
      effectiveInterval

getLegacyBasketHistoryTimed
  :: DbPool
  -> Config
  -> BasketHistoryParams
  -> IO (Either ApiError BasketHistoryFetch)
getLegacyBasketHistoryTimed pool cfg params = do
  now <- getPOSIXTime
  let nowUnix = round now
      rangeSeconds = basketRangeSeconds (bhpRange params)
      fromUnix = nowUnix - rangeSeconds
      interval = boundedBasketHistoryInterval rangeSeconds (bhpIntervalSeconds params)
      maxPoints = fromIntegral $ min maxBasketHistoryPoints ((rangeSeconds `div` interval) + 4)

  poolStartedAt <- getMonotonicTimeNSec
  (rows, volumeRows, poolWaitNs, snapshotQueryNs, volumeQueryNs, snapshotRows, volumeRowsCount) <- withDb pool $ \conn -> do
    connectionReadyAt <- getMonotonicTimeNSec
    let poolWaitNs = connectionReadyAt - poolStartedAt

    snapshotQueryStartedAt <- getMonotonicTimeNSec
    snapshots <-
      getBasketSnapshots conn fromUnix nowUnix interval maxPoints (bhpIncludeComponents params)
    snapshotRows <- evaluate $ length snapshots
    snapshotQueryFinishedAt <- getMonotonicTimeNSec

    (volumes, volumeQueryNs, volumeRowsCount) <-
      getLegacyBasketHistoryVolumeRowsTimed
        (bhpIncludeComponents params)
        ( getPerpsMarketVolumeBuckets
            conn
            (cfgPerpsChainId cfg)
            (cfgPerpsOrderRouter cfg)
            fromUnix
            nowUnix
            interval
        )

    pure
      ( snapshots
      , volumes
      , poolWaitNs
      , snapshotQueryFinishedAt - snapshotQueryStartedAt
      , volumeQueryNs
      , snapshotRows
      , volumeRowsCount
      )

  let points = basketHistoryPointsWithVolume interval rows volumeRows
      latest = case reverse rows of
        row : _ -> Just (bhsrBasketPrice row)
        [] -> Nothing
      changePct = computeChange rows
      history =
        BasketHistory
          { bhRange = bhpRange params
          , bhIntervalSeconds = interval
          , bhSource = "pyth_benchmarks"
          , bhGeneratedAt = now
          , bhLatestPrice = latest
          , bhChangePct = changePct
          , bhPoints = points
          }

  pure $
    Right $
      BasketHistoryFetch
        { bhfResponse = mkResponse 0 (cfgChainId cfg) history
        , bhfReadSource = "legacy_raw"
        , bhfPoolWaitNs = poolWaitNs
        , bhfSnapshotQueryNs = snapshotQueryNs
        , bhfVolumeQueryNs = volumeQueryNs
        , bhfSnapshotRows = snapshotRows
        , bhfVolumeRows = volumeRowsCount
        }

getRollupBasketHistoryTimed
  :: DbPool
  -> Config
  -> BasketHistoryParams
  -> IO (Either ApiError BasketHistoryFetch)
getRollupBasketHistoryTimed pool cfg params = do
  generatedAt <- getPOSIXTime
  let now = floor generatedAt
      rangeSeconds = basketRangeSeconds $ bhpRange params
      fromTimestamp = max 0 $ now - rangeSeconds
      interval = boundedBasketHistoryInterval rangeSeconds $ bhpIntervalSeconds params
      effectiveParams = params {bhpIntervalSeconds = interval}
      closedThrough = (now `div` interval) * interval
      maximumRows = fromIntegral maxBasketHistoryPoints
  poolStartedAt <- getMonotonicTimeNSec
  (result, poolWaitNs, queryNs) <- withDb pool $ \conn -> withCandleReadSnapshot conn $ do
    connectionReadyAt <- getMonotonicTimeNSec
    let poolWaitNs = connectionReadyAt - poolStartedAt
    queryStartedAt <- getMonotonicTimeNSec
    mDefinition <- getActiveBasketDefinitionIdentity conn now
    result <- case mDefinition of
      Nothing -> pure $ Left $ E.networkError "Active basket definition identity is unavailable"
      Just definition
        | bdiEffectiveFrom definition > fromTimestamp ->
            pure $ Left $ E.networkError "Requested history crosses a basket definition boundary"
        | otherwise ->
            validateRollupHistoryRangeWithPolicy
              (bdiDisplayPriceCap definition)
              (cfgPerpsCandleLatenessSeconds cfg)
              (cfgPerpsCandleFinalizationGraceSeconds cfg)
              now
              interval
              fromTimestamp
              closedThrough
              maximumRows
              <$> getBasketCandleRange
                conn
                (bdiSeriesId definition)
                (cfgPerpsChainId cfg)
                (cfgPerpsOrderRouter cfg)
                interval
                fromTimestamp
                closedThrough
                (maximumRows + 1)
    queryFinishedAt <- getMonotonicTimeNSec
    pure (result, poolWaitNs, queryFinishedAt - queryStartedAt)
  pure $ do
    rows <- result
    Right
      BasketHistoryFetch
        { bhfResponse =
            mkResponse 0 (cfgChainId cfg) $
              basketHistoryFromCandleRows generatedAt effectiveParams rows
        , bhfReadSource = "rollup_compat"
        , bhfPoolWaitNs = poolWaitNs
        , bhfSnapshotQueryNs = queryNs
        , bhfVolumeQueryNs = 0
        , bhfSnapshotRows = length rows
        , bhfVolumeRows = length rows
        }

validateRollupHistoryRange
  :: Integer
  -> Integer
  -> Integer
  -> Integer
  -> Int
  -> CandleRange
  -> Either ApiError [BasketCandleRow]
validateRollupHistoryRange displayPriceCap interval requestedStart requestedEnd maximumRows range
  | not $ crCoverageComplete range = unhealthy "combined price and volume coverage is incomplete"
  | crDatasetGeneration range <= 0 = unhealthy "dataset generation is unavailable"
  | length rows > maximumRows = unhealthy "compatibility result exceeds its bounded row budget"
  | otherwise = case validateMetadataAndRows of
      Left reason -> unhealthy reason
      Right () -> Right rows
 where
  rows = crCandles range
  unhealthy reason =
    Left $ E.networkError $ "Candle rollup compatibility range failed strict validation: " <> reason
  validateMetadataAndRows = do
    coverageStart <- maybe (Left "coverage start is unavailable") Right $ crCoverageStart range
    coverageEnd <- maybe (Left "coverage end is unavailable") Right $ crCoverageEnd range
    finalizedThrough <- maybe (Left "finalized watermark is unavailable") Right $ crFinalizedThrough range
    let effectiveStart = max requestedStart coverageStart
        effectiveEnd = minimum [requestedEnd, coverageEnd, finalizedThrough]
        aligned timestamp = timestamp >= 0 && timestamp `mod` interval == 0
    if interval <= 0 then Left "interval must be positive" else Right ()
    if not $ all aligned [coverageStart, coverageEnd, finalizedThrough]
      then Left "coverage metadata is not interval-aligned"
      else Right ()
    if coverageStart >= coverageEnd
      then Left "coverage window is empty or reversed"
      else Right ()
    if finalizedThrough < coverageStart || finalizedThrough > coverageEnd
      then Left "finalized watermark is outside the coverage window"
      else Right ()
    if effectiveStart >= effectiveEnd
      then Left "requested history has no finalized covered buckets"
      else Right ()
    validateAscendingRows rows
    mapM_
      (validateHistoricalRow displayPriceCap interval effectiveStart effectiveEnd finalizedThrough)
      rows

-- | The compatibility route is time-relative rather than a request for one
-- immutable closed page. Require a current global source watermark before
-- projecting rollups into the legacy response shape.
validateRollupHistoryRangeWithPolicy
  :: Integer
  -> Integer
  -> Integer
  -> Integer
  -> Integer
  -> Integer
  -> Integer
  -> Int
  -> CandleRange
  -> Either ApiError [BasketCandleRow]
validateRollupHistoryRangeWithPolicy displayPriceCap latenessSeconds finalizationGraceSeconds now interval requestedStart requestedEnd maximumRows range = do
  rows <-
    validateRollupHistoryRange
      displayPriceCap
      interval
      requestedStart
      requestedEnd
      maximumRows
      range
  coverageEnd <-
    maybe
      (Left $ E.networkError "Candle rollup compatibility range failed strict validation: coverage end is unavailable")
      Right
      (crCoverageEnd range)
  case validateCoverageFreshness latenessSeconds now interval coverageEnd of
    Left reason ->
      Left $
        E.networkError $
          "Candle rollup compatibility range failed strict validation: " <> reason
    Right () -> do
      finalizedThrough <-
        maybe
          (Left $ E.networkError "Candle rollup compatibility range failed strict validation: finalized watermark is unavailable")
          Right
          (crFinalizedThrough range)
      case validateFinalizationFreshness latenessSeconds finalizationGraceSeconds now interval finalizedThrough of
        Left reason ->
          Left $
            E.networkError $
              "Candle rollup compatibility range failed strict validation: " <> reason
        Right () -> Right rows

validateCoverageFreshness :: Integer -> Integer -> Integer -> Integer -> Either Text ()
validateCoverageFreshness latenessSeconds now interval coverageEnd
  | interval <= 0 = Left "interval must be positive"
  | latenessSeconds < 0 = Left "candle lateness tolerance is negative"
  | now < 0 = Left "backend clock is before the Unix epoch"
  | coverageEnd < freshnessFloor =
      Left "coverage watermark is stale for the configured lateness tolerance"
  | otherwise = Right ()
 where
  freshnessFloor = ((max 0 (now - latenessSeconds)) `div` interval) * interval

-- Coverage proves that both writers have checked in; finalization separately
-- proves that closed buckets are publishable. The writer may only advance once
-- source lateness has elapsed and publishes asynchronously, so readers allow a
-- small, bounded publication grace before requiring the next aligned watermark.
-- Rows remain clipped to the stored finalized watermark throughout the grace.
validateFinalizationFreshness :: Integer -> Integer -> Integer -> Integer -> Integer -> Either Text ()
validateFinalizationFreshness latenessSeconds finalizationGraceSeconds now interval finalizedThrough
  | interval <= 0 = Left "interval must be positive"
  | latenessSeconds < 0 = Left "candle lateness tolerance is negative"
  | finalizationGraceSeconds < 0 = Left "candle finalization grace is negative"
  | now < 0 = Left "backend clock is before the Unix epoch"
  | finalizedThrough < freshnessFloor =
      Left "finalized watermark is stale after the configured publication grace"
  | otherwise = Right ()
 where
  freshnessFloor =
    ((max 0 (now - latenessSeconds - finalizationGraceSeconds)) `div` interval) * interval

basketHistoryFromCandleRows
  :: POSIXTime
  -> BasketHistoryParams
  -> [BasketCandleRow]
  -> BasketHistory
basketHistoryFromCandleRows generatedAt params rows =
  BasketHistory
    { bhRange = bhpRange params
    , bhIntervalSeconds = bhpIntervalSeconds params
    -- Preserve the legacy discriminator until clients migrate to the native
    -- candle endpoint; the actual source is exposed in request telemetry.
    , bhSource = "pyth_benchmarks"
    , bhGeneratedAt = generatedAt
    , bhLatestPrice = bcrRawClosePrice <$> lastMaybe rows
    , bhChangePct = candleRowsChange rows
    , bhPoints = map candleRowToHistoryPoint rows
    }
  where
    candleRowToHistoryPoint row =
      BasketHistoryPoint
        { bhpTimestamp = bcrBucketStart row
        , bhpBasketPrice = bcrRawClosePrice row
        , bhpVolumeUsdc = maybe 0 (`div` 10 ^ (20 :: Int)) $ bcrVolumeNumerator row
        , bhpComponents = Nothing
        }

    candleRowsChange candleRows =
      case (candleRows, reverse candleRows) of
        (first : _, lastRow : _) | bcrRawOpenPrice first > 0 ->
          Just $
            fromIntegral (bcrRawClosePrice lastRow - bcrRawOpenPrice first)
              / fromIntegral (bcrRawOpenPrice first)
        _ -> Nothing

    lastMaybe values = case reverse values of
      value : _ -> Just value
      [] -> Nothing

basketHistoryPointsWithVolume
  :: Integer
  -> [BasketHistorySnapshotRow]
  -> [PerpsMarketVolumeBucketRow]
  -> [BasketHistoryPoint]
basketHistoryPointsWithVolume intervalSeconds rows volumeRows =
  map rowToPoint rows
  where
    interval = max 1 intervalSeconds
    volumeByBucket =
      Map.fromList
        [ (pmvbrBucket row, pmvbrVolumeUsdc row)
        | row <- volumeRows
        ]
    rowToPoint BasketHistorySnapshotRow {..} =
      BasketHistoryPoint
        { bhpTimestamp = bhsrTimestamp
        , bhpBasketPrice = bhsrBasketPrice
        , bhpVolumeUsdc = Map.findWithDefault 0 (bhsrTimestamp `div` interval) volumeByBucket
        , bhpComponents = bhsrComponents
        }

computeChange :: [BasketHistorySnapshotRow] -> Maybe Double
computeChange rows =
  case (rows, reverse rows) of
    (first : _, lastRow : _) | bhsrBasketPrice first > 0 ->
      Just $
        (fromIntegral (bhsrBasketPrice lastRow - bhsrBasketPrice first) / fromIntegral (bhsrBasketPrice first) :: Double)
    _ -> Nothing

getBasketLatest
  :: DbPool
  -> Config
  -> IO (Either ApiError (ApiResponse BasketLatest))
getBasketLatest pool cfg = do
  now <- getPOSIXTime
  mRow <- withDb pool getLatestBasketSnapshot
  pure $ case mRow of
    Nothing ->
      Left $ E.internalError "No perps basket snapshots are available yet. Start plether-basket-worker --once or --latest-loop."
    Just BasketSnapshotRow {..} ->
      Right $
        mkResponse 0 (cfgChainId cfg) $
          BasketLatest
            { blTimestamp = bsrTimestamp
            , blBasketPrice = bsrBasketPrice
            , blComponents = bsrComponents
            , blGeneratedAt = now
            , blSource = "database"
            }

getRevealPayload
  :: DbPool
  -> EthClient
  -> Config
  -> Integer
  -> Integer
  -> Integer
  -> IO (Either ApiError (ApiResponse RevealPayloadResponse))
getRevealPayload pool perpsClient cfg orderId minPublishTime maxPublishTime = do
  mRow <- withDb pool $ \conn ->
    getPythUpdatePayloadForWindow conn minPublishTime maxPublishTime
  case mRow of
    Nothing ->
      pure $ Left $
        E.networkError $
          "Reveal payload unavailable for order "
            <> T.pack (show orderId)
            <> ". The basket worker has not cached the first post-commit six-feed Pyth update starting at "
            <> T.pack (show minPublishTime)
            <> " within reveal window ending at "
            <> T.pack (show maxPublishTime)
            <> ". Keep plether-basket-worker --latest-loop running and retry before the order expires."
    Just row | not (isHistoricalRevealPayload row) ->
      pure $ Left $
        E.networkError $
          "Exact reveal payload unavailable for order "
            <> T.pack (show orderId)
            <> ". The cached row for the first post-commit tick came from "
            <> puprSource row
            <> ", so the app should retry with exact historical Pyth data."
    Just row ->
      case rowToRevealPayload orderId row of
        Left err -> pure $ Left $ E.internalError err
        Right payload -> do
          validation <-
            validateStoredPythUpdate
              perpsClient
              cfg
              (Just (minPublishTime, maxPublishTime))
              row
          pure $ case validation of
            Left err -> Left err
            Right _ -> Right $ mkResponse 0 (cfgChainId cfg) payload

rowToRevealPayload :: Integer -> PythUpdatePayloadRow -> Either Text RevealPayloadResponse
rowToRevealPayload orderId PythUpdatePayloadRow {..} = do
  publishTimes <- decodeValue "publish_times" puprPublishTimes
  updateData <- decodeValue "update_data" puprUpdateData
  pure
    RevealPayloadResponse
      { rprOrderId = orderId
      , rprUpdateData = updateData
      , rprFetchedAt = puprFetchedAt
      , rprPublishTimes = publishTimes
      , rprMinPublishTime = puprMinPublishTime
      , rprMaxPublishTime = puprMaxPublishTime
      , rprSource = puprSource
      }

decodeValue :: (FromJSON a) => Text -> Value -> Either Text a
decodeValue label value =
  case Aeson.fromJSON value of
    Aeson.Success parsed -> Right parsed
    Aeson.Error err -> Left $ "Could not decode cached reveal " <> label <> ": " <> T.pack err

getCachedLatestPythUpdate
  :: DbPool
  -> EthClient
  -> Config
  -> IO (Either ApiError (ApiResponse PythUpdateResponse))
getCachedLatestPythUpdate pool perpsClient cfg = do
  mRow <- withDb pool getLatestPythUpdatePayload
  case mRow of
    Nothing ->
      pure $ Left $
        E.networkError
          "No cached Pyth update payload is available yet. Keep plether-basket-worker --latest-loop running."
    Just row -> do
      validation <- validateStoredPythUpdate perpsClient cfg Nothing row
      pure $ case validation of
        Left err -> Left err
        Right admission -> Right $ mkResponse 0 (cfgChainId cfg) (puaPayload admission)

validateStoredPythUpdate
  :: EthClient
  -> Config
  -> Maybe (Integer, Integer)
  -> PythUpdatePayloadRow
  -> IO (Either ApiError PythUpdateAdmission)
validateStoredPythUpdate perpsClient cfg mHistoricalBounds row =
  case storedPythUpdateAdmission row of
    Left err -> pure $ Left err
    Right admission -> do
      validation <-
        case mHistoricalBounds of
          -- Unique parsing proves this is the first eligible update after the
          -- order's lower bound, so it must receive the full reveal window.
          Just (minPublishTime, maxPublishTime) ->
            validateUniquePythUpdateData
              perpsClient
              (cfgPerpsPletherOracle cfg)
              (puaUpdateData admission)
              (puaFeedIds admission)
              minPublishTime
              maxPublishTime
          Nothing ->
            validatePythUpdateData
              perpsClient
              (cfgPerpsPletherOracle cfg)
              (puaUpdateData admission)
              (puaFeedIds admission)
              (puaMinPublishTime admission)
              (puaMaxPublishTime admission)
      pure $ case validation of
        Left err -> Left $ rpcErrorToApiError err
        Right () -> Right admission

storedPythUpdateAdmission :: PythUpdatePayloadRow -> Either ApiError PythUpdateAdmission
storedPythUpdateAdmission PythUpdatePayloadRow {..} = do
  publishTimes <- mapLeft E.internalError $ decodeValue "publish_times" puprPublishTimes
  encodedUpdateData <- mapLeft E.internalError $ decodeValue "update_data" puprUpdateData
  if length publishTimes /= length basketComponents
    then Left $ E.internalError "Cached Pyth row does not include exactly six feed publish times"
    else Right ()
  (actualMinPublishTime, actualMaxPublishTime) <-
    mapAdmissionError $ validatePublishTimes publishTimes
  if actualMinPublishTime /= puprMinPublishTime || actualMaxPublishTime /= puprMaxPublishTime
    then Left $ E.internalError "Cached Pyth row publish-time metadata does not match its payload window"
    else Right ()
  (updateData, feedIds) <- decodeAdmissionByteStrings encodedUpdateData
  Right
    PythUpdateAdmission
      { puaPayload =
          PythUpdateResponse
            { purUpdateData = prefixHex <$> encodedUpdateData
            , purFetchedAt = puprFetchedAt
            , purPublishTimes = publishTimes
            , purSource = puprSource
            }
      , puaUpdateData = updateData
      , puaFeedIds = feedIds
      , puaMinPublishTime = puprMinPublishTime
      , puaMaxPublishTime = puprMaxPublishTime
      }
  where
    mapAdmissionError = mapLeft (E.internalError . ("Cached Pyth row failed admission checks: " <>))

data HermesBinary = HermesBinary
  { hbData :: [Text]
  }
  deriving stock (Show)

instance FromJSON HermesBinary where
  parseJSON = withObject "HermesBinary" $ \v ->
    HermesBinary <$> v .: "data"

data HermesPrice = HermesPrice
  { hpPublishTime :: Integer
  }
  deriving stock (Show)

instance FromJSON HermesPrice where
  parseJSON = withObject "HermesPrice" $ \v ->
    HermesPrice <$> v .: "publish_time"

data HermesParsedPrice = HermesParsedPrice
  { hppFeedId :: Text
  , hppPrice :: HermesPrice
  }
  deriving stock (Show)

instance FromJSON HermesParsedPrice where
  parseJSON = withObject "HermesParsedPrice" $ \v ->
    HermesParsedPrice
      <$> v .: "id"
      <*> v .: "price"

data HermesUpdateResponse = HermesUpdateResponse
  { hurBinary :: HermesBinary
  , hurParsed :: [HermesParsedPrice]
  }
  deriving stock (Show)

instance FromJSON HermesUpdateResponse where
  parseJSON = withObject "HermesUpdateResponse" $ \v ->
    HermesUpdateResponse
      <$> v .: "binary"
      <*> v .: "parsed"

data PythUpdateAdmission = PythUpdateAdmission
  { puaPayload :: PythUpdateResponse
  , puaUpdateData :: [ByteString]
  , puaFeedIds :: [ByteString]
  , puaMinPublishTime :: Integer
  , puaMaxPublishTime :: Integer
  }
  deriving stock (Show)

getPythUpdate
  :: AppCache
  -> Manager
  -> EthClient
  -> Config
  -> Maybe Integer
  -> IO (Either ApiError (ApiResponse PythUpdateResponse))
getPythUpdate cache manager perpsClient cfg mPublishTime =
  case resolveHermesApiKey (cfgPythHermesUrl cfg) (cfgPythApiKey cfg) of
    Left err -> pure $ Left $ E.internalError err
    Right apiKey -> runAuthenticated apiKey
  where
    runAuthenticated apiKey = do
      now <- getPOSIXTime
      mCached <- getCachedPyth now
      case mCached of
        Just cached -> pure $ Right $ mkResponse 0 (cfgChainId cfg) cached
        Nothing -> do
          mCooldown <- getRateLimitCooldown now
          case mCooldown of
            Just retryAfter -> pure $ Left $ E.rateLimitedWithDetails (Just $ BS8.pack $ show retryAfter)
            Nothing -> do
              requestBase <- parseRequest $ T.unpack requestUrl
              let request =
                    setQueryString queryParams requestBase
                      { requestHeaders = authHeaders apiKey <> requestHeaders requestBase
                      }
              response <- httpLbs request manager
              let code = statusCode (responseStatus response)
                  body = responseBody response
              if code == 429
                then do
                  setRateLimitCooldown now (retryAfterHeader response)
                  pure $ Left $ E.rateLimitedWithDetails (retryAfterHeader response)
                else
                  if code < 200 || code >= 300
                    then pure $ Left $ E.networkError $ "Hermes returned HTTP " <> T.pack (show code) <> ": " <> previewBody body
                    else do
                      fetchedAt <- round <$> getPOSIXTime
                      case decodePythUpdateForAdmission mPublishTime fetchedAt (cfgPythLatestMaxAgeSeconds cfg) body of
                        Left err -> pure $ Left err
                        Right admission -> do
                          validation <-
                            case mPublishTime of
                              Nothing ->
                                validatePythUpdateData
                                  perpsClient
                                  (cfgPerpsPletherOracle cfg)
                                  (puaUpdateData admission)
                                  (puaFeedIds admission)
                                  (puaMinPublishTime admission)
                                  (puaMaxPublishTime admission)
                              -- The historical endpoint has no separate maximum;
                              -- Hermes' returned maximum closes the requested window.
                              Just _ ->
                                validateUniquePythUpdateData
                                  perpsClient
                                  (cfgPerpsPletherOracle cfg)
                                  (puaUpdateData admission)
                                  (puaFeedIds admission)
                                  (puaMinPublishTime admission)
                                  (puaMaxPublishTime admission)
                          case validation of
                            Left err -> pure $ Left $ rpcErrorToApiError err
                            Right () -> do
                              let payload = puaPayload admission
                              setCachedPyth now payload
                              pure $ Right $ mkResponse 0 (cfgChainId cfg) payload

    cacheKey =
      maybe "latest" (T.pack . show) mPublishTime

    cacheTtlSeconds :: POSIXTime
    cacheTtlSeconds =
      case mPublishTime of
        Nothing -> 2
        Just _ -> 10 * 60

    getCachedPyth now =
      atomically $ do
        entries <- readTVar (cachePythUpdates cache)
        pure $ case Map.lookup cacheKey entries of
          Just (payload, cachedAt) | now - cachedAt <= cacheTtlSeconds -> Just payload
          _ -> Nothing

    setCachedPyth now payload =
      atomically $
        modifyTVar' (cachePythUpdates cache) $
          Map.insert cacheKey (payload, now)

    getRateLimitCooldown now =
      atomically $ do
        mUntil <- readTVar (cachePythRateLimitUntil cache)
        pure $ case mUntil of
          Just untilTime | untilTime > now -> Just (ceiling (untilTime - now) :: Int)
          _ -> Nothing

    setRateLimitCooldown now retryAfter =
      atomically $
        writeTVar (cachePythRateLimitUntil cache) $
          Just (now + fromIntegral (retryAfterSeconds retryAfter))

    retryAfterSeconds retryAfter =
      case retryAfter >>= BS8.readInteger of
        Just (seconds, _) | seconds > 0 -> fromInteger seconds
        _ -> 15 :: Int

    requestUrl =
      stripTrailingSlash (cfgPythHermesUrl cfg)
        <> "/v2/updates/price/"
        <> maybe "latest" (T.pack . show) mPublishTime

    queryParams =
      ("parsed", Just "true")
        : [("ids[]", Just (encodeUtf8 (bcFeedId component))) | component <- basketComponents]

    authHeaders = \case
      Nothing -> []
      Just key -> [("Authorization", encodeUtf8 $ "Bearer " <> key)]

    retryAfterHeader response =
      lookup "Retry-After" (responseHeaders response)

    previewBody body =
      T.take 180 . T.strip . T.pack $ show (LBS.take 180 body)

decodePythUpdateForAdmission
  :: Maybe Integer
  -> Integer
  -> Integer
  -> LBS.ByteString
  -> Either ApiError PythUpdateAdmission
decodePythUpdateForAdmission mRequestedPublishTime fetchedAt latestMaxAge body = do
  HermesUpdateResponse {..} <-
    case eitherDecode body of
      Left err -> Left $ E.internalError $ "Could not decode Hermes response: " <> T.pack err
      Right response -> Right response
  let encodedUpdateData = hbData hurBinary
      publishTimes = hpPublishTime . hppPrice <$> hurParsed
      actualFeedIds = sort $ normalizeFeedId . T.toLower . hppFeedId <$> hurParsed
      expectedFeedIdTexts = sort $ normalizeFeedId . T.toLower . bcFeedId <$> basketComponents
  if null encodedUpdateData
    then Left $ E.internalError "Hermes response did not include binary update data"
    else Right ()
  if actualFeedIds /= expectedFeedIdTexts
    then Left $ E.internalError "Hermes response did not include exactly the six requested basket feed IDs"
    else Right ()
  if length publishTimes /= length basketComponents
    then Left $ E.internalError "Hermes response did not include exactly six feed publish times"
    else Right ()
  (minPublishTime, maxPublishTime) <-
    mapAdmissionError $
      case mRequestedPublishTime of
        Nothing -> validateLatestPublishTimes fetchedAt latestMaxAge publishTimes
        Just requestedPublishTime -> do
          (returnedMin, returnedMax) <- validatePublishTimes publishTimes
          if returnedMin < requestedPublishTime
            then Left "Hermes historical payload predates the requested publish time"
            else Right (requestedPublishTime, returnedMax)
  (updateData, feedIds) <- decodeAdmissionByteStrings encodedUpdateData
  let payload =
        PythUpdateResponse
          { purUpdateData = prefixHex <$> encodedUpdateData
          , purFetchedAt = fetchedAt
          , purPublishTimes = publishTimes
          , purSource = "backend_hermes"
          }
  Right
    PythUpdateAdmission
      { puaPayload = payload
      , puaUpdateData = updateData
      , puaFeedIds = feedIds
      , puaMinPublishTime = minPublishTime
      , puaMaxPublishTime = maxPublishTime
      }
  where
    mapAdmissionError = mapLeft (E.internalError . ("Hermes payload failed admission checks: " <>))

decodeAdmissionByteStrings :: [Text] -> Either ApiError ([ByteString], [ByteString])
decodeAdmissionByteStrings encodedUpdateData = do
  updateData <-
    traverse
      (\(index, encoded) ->
        mapAdmissionError $
          mapLeft
            (\err -> "Pyth update data item " <> T.pack (show index) <> " is invalid: " <> err)
            (hexToByteStringEither encoded)
      )
      (zip [0 :: Int ..] encodedUpdateData)
  if null updateData
    then Left $ E.internalError "Pyth update data is empty"
    else Right ()
  feedIds <-
    traverse
      (\component -> do
        feedId <-
          mapAdmissionError $
            mapLeft
              (\err -> "configured feed " <> bcFeedId component <> " is invalid: " <> err)
              (hexToByteStringEither $ bcFeedId component)
        if BS.length feedId == 32
          then Right feedId
          else Left $ E.internalError $ "Configured Pyth feed ID is not 32 bytes: " <> bcFeedId component
      )
      basketComponents
  Right (updateData, feedIds)
  where
    mapAdmissionError = mapLeft (E.internalError . ("Pyth payload failed admission checks: " <>))

prefixHex :: Text -> Text
prefixHex value =
  if "0x" `T.isPrefixOf` T.toLower value then value else "0x" <> value

mapLeft :: (a -> b) -> Either a value -> Either b value
mapLeft f result =
  case result of
    Left err -> Left $ f err
    Right value -> Right value

stripTrailingSlash :: Text -> Text
stripTrailingSlash value =
  fromMaybe value (T.stripSuffix "/" value)
