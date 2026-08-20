module Plether.Pyth.History
  ( BasketIngestorConfig (..)
  , basketBackfillGridWindows
  , basketObservationId
  , decodeTradingViewCloseHistory
  , deriveEarliestBasketGridTimestamp
  , deriveBasketHistoryObservation
  , deriveTradingViewBasketHistory
  , filterTradingViewHistorySamplesForPersistence
  , minimumBasketHistoryPublicationEnd
  , fetchBasketSnapshotAt
  , legacyObservationId
  , runBasketBackfill
  , startBasketHistoryIngestor
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (SomeException, catch, displayException, finally, try)
import Control.Monad (foldM, forM, forM_, forever, unless, when)
import Data.Aeson
  ( FromJSON (..)
  , Value (..)
  , eitherDecode
  , toJSON
  , withObject
  , (.:)
  , (.:?)
  )
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as LBS
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import Data.Scientific (Scientific, floatingOrInteger)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Encoding as TE
import Data.Time.Clock.POSIX (getPOSIXTime)
import Database.PostgreSQL.Simple
  ( Connection
  , Only (..)
  , query
  , withTransaction
  )
import Network.HTTP.Client
  ( Manager
  , httpLbs
  , parseRequest
  , requestHeaders
  , responseBody
  , responseStatus
  , setQueryString
  )
import Network.HTTP.Types.Status (statusCode)
import Plether.Config (PerpsCandleWriteMode (..))
import Plether.Database (DbPool, withDb)
import Plether.Database.CandleHistory
  ( CandleHistoryIngestionProgress (..)
  , CandleHistorySelection (..)
  , candleHistorySelectionIsAbsent
  , candleHistorySelectionIsLatest
  , completeCandleHistoryIngestionWindow
  , defaultCandleMarketId
  , effectiveHistoryStart
  , ensureCandleMarketIdentity
  , getLatestCandleHistoryIngestionProgress
  , getLatestCandleHistorySelection
  , initializeCandleHistoryIngestionProgress
  , recordCandleHistoryIngestionError
  )
import Plether.Database.Candles
  ( BasketObservationInput (..)
  , RollupCoverage (..)
  , RollupKind (..)
  , canonicalCandleIntervals
  , defaultBasketSeriesId
  , ensureCurrentBasketDefinition
  , getRollupCoverage
  , lockBasketPriceDataset
  , recomputeBasketCandleHierarchy
  , upsertBasketObservation
  )
import Plether.Database.Schema
  ( getBasketSnapshotTimes
  , insertBasketSnapshot
  , insertBasketSnapshotsWithSource
  )
import Plether.Indexer.Contracts (keccak256Text)
import Plether.Logging (field, logErrorEvery, logInfo, logWarnEvery)
import Plether.Pyth.Basket
  ( BasketComponent (..)
  , PythPricePoint (..)
  , basketComponents
  , computeBasketSnapshot
  , normalizeFeedId
  )
import Plether.Pyth.RevealPayload (validatePublishTimes)
import Text.Read (readMaybe)

data BasketIngestorConfig = BasketIngestorConfig
  { bicBenchmarksUrl :: Text
  , bicApiKey :: Maybe Text
  , bicChainId :: Integer
  , bicBackfillDays :: Int
  , bicOwnHistoryTargets :: Bool
  , bicSampleIntervalSeconds :: Integer
  , bicPollSeconds :: Int
  , bicCandleWriteMode :: PerpsCandleWriteMode
  , bicCandleLatenessSeconds :: Integer
  }
  deriving stock (Show)

-- | Choose the first Pyth sampling-grid timestamp to ingest. A persisted
-- history target is authoritative when present; the relative-day window is
-- retained only as a compatibility fallback for environments which have not
-- selected a target yet.
deriveEarliestBasketGridTimestamp
  :: Integer
  -> Integer
  -> Int
  -> Maybe Integer
  -> Either Text Integer
deriveEarliestBasketGridTimestamp interval endTs fallbackDays targetStart =
  case targetStart of
    Just requestedStart -> effectiveHistoryStart interval requestedStart
    Nothing ->
      Right $
        endTs
          - fromIntegral (max 1 fallbackDays) * 86_400

-- | Earliest exclusive terminal which contains at least one fully aligned
-- bucket for every canonical interval. Since every canonical interval divides
-- the UTC day, one complete daily bucket proves the complete set.
minimumBasketHistoryPublicationEnd :: Integer -> Integer
minimumBasketHistoryPublicationEnd startTimestamp =
  let dailyInterval = maximum canonicalCandleIntervals
   in alignUp startTimestamp dailyInterval + dailyInterval

-- | Bound database result sets and in-memory sampling grids independently of
-- the operator-selected history span. Each pair is inclusive and adjacent
-- windows never repeat a sampling slot.
basketBackfillGridWindows :: Integer -> Integer -> Integer -> [(Integer, Integer)]
basketBackfillGridWindows interval startTs endTs
  | interval <= 0 || startTs > endTs = []
  | otherwise = go startTs
 where
  maximumSlotsPerWindow = 1_440
  windowSpan = interval * (maximumSlotsPerWindow - 1)
  go windowStart
    | windowStart > endTs = []
    | otherwise =
        let windowEnd = min endTs $ windowStart + windowSpan
         in (windowStart, windowEnd) : go (windowEnd + interval)

-- | Stable, order-independent identity for a signed basket observation. The
-- basket version is part of the digest so a future definition can never
-- deduplicate against the current series accidentally.
basketObservationId :: Text -> [PythPricePoint] -> Text
basketObservationId basketVersion points =
  "0x" <> TE.decodeUtf8 (B16.encode $ keccak256Text canonical)
  where
    canonical = T.intercalate "|" $ basketVersion : concatMap encodePoint ordered
    ordered = sortOn (normalizeFeedId . pppFeedId) points
    encodePoint point =
      [ normalizeFeedId $ pppFeedId point
      , T.pack $ show $ pppPrice point
      , T.pack $ show $ pppConfidence point
      , T.pack $ show $ pppExponent point
      , T.pack $ show $ pppPublishTime point
      ]

-- Legacy samples identify the underlying source event, not the requested
-- sampling slot. Repeated benchmark slots that resolve to the same six feed
-- publish times therefore deduplicate, while a material correction at those
-- same times updates the existing lower-priority observation.
legacyObservationId :: Text -> [PythPricePoint] -> Text
legacyObservationId basketVersion points =
  "legacy:0x" <> TE.decodeUtf8 (B16.encode $ keccak256Text canonical)
  where
    canonical = T.intercalate "|" $ basketVersion : concatMap encodePoint ordered
    ordered = sortOn (normalizeFeedId . pppFeedId) points
    encodePoint point =
      [ normalizeFeedId $ pppFeedId point
      , T.pack $ show $ pppPublishTime point
      ]

deriveBasketHistoryObservation
  :: Integer
  -> Integer
  -> [PythPricePoint]
  -> Either Text (Integer, Integer, Value, [PythPricePoint])
deriveBasketHistoryObservation requestedTimestamp benchmarkWindowSeconds points = do
  if benchmarkWindowSeconds > 0
    then pure ()
    else Left "Pyth Benchmarks admission window must be positive"
  if length points == length basketComponents
    then pure ()
    else Left "Pyth Benchmarks did not return exactly the configured six feeds"
  (canonicalPublishTime, maximumPublishTime) <-
    validatePublishTimes $ map pppPublishTime points
  let windowEnd = requestedTimestamp + benchmarkWindowSeconds
  -- Pyth Benchmarks defines the interval endpoint as inclusive, matching the
  -- EVM parser contract: minPublishTime <= publishTime <= maxPublishTime.
  -- Check both extrema so a divergent component cannot escape the requested
  -- signed window merely because the canonical (minimum) time is admissible.
  if
    canonicalPublishTime >= requestedTimestamp
      && maximumPublishTime <= windowEnd
    then pure ()
    else
      Left $
        "Pyth Benchmarks component publish times are outside requested window ["
          <> T.pack (show requestedTimestamp)
          <> ", "
          <> T.pack (show windowEnd)
          <> "]"
  (basketPrice, components) <- computeBasketSnapshot points
  pure (canonicalPublishTime, basketPrice, toJSON components, points)

data BenchmarkResponse = BenchmarkResponse
  { brParsed :: [PythPricePoint]
  }
  deriving stock (Show)

instance FromJSON BenchmarkResponse where
  parseJSON = withObject "BenchmarkResponse" $ \v -> do
    parsed <- v .: "parsed"
    pure $ BenchmarkResponse (map unBenchmarkFeed parsed)

newtype BenchmarkFeed = BenchmarkFeed
  { unBenchmarkFeed :: PythPricePoint
  }
  deriving stock (Show)

instance FromJSON BenchmarkFeed where
  parseJSON = withObject "PythPricePoint" $ \v -> do
    feedId <- v .: "id"
    priceValue <- v .: "price"
    BenchmarkFeed <$> parsePythPrice feedId priceValue

data TradingViewHistoryResponse
  = TradingViewHistoryOk [Integer] [Scientific]
  | TradingViewHistoryNoData
  deriving stock (Eq, Show)

instance FromJSON TradingViewHistoryResponse where
  parseJSON = withObject "TradingViewHistoryResponse" $ \v -> do
    status <- v .: "s"
    case status :: Text of
      "ok" -> TradingViewHistoryOk <$> v .: "t" <*> v .: "c"
      "no_data" -> pure TradingViewHistoryNoData
      _ -> do
        message <- v .:? "errmsg"
        fail $
          "Pyth TradingView history returned status "
            <> T.unpack status
            <> maybe "" ((": " <>) . T.unpack) message

-- | Decode only the two UDF columns used by price ingestion. Open/high/low
-- and volume cannot affect the immutable basket close series. A timestamp
-- without its matching close is rejected before any component series can be
-- combined or persisted.
decodeTradingViewCloseHistory
  :: LBS.ByteString
  -> Either Text [(Integer, Scientific)]
decodeTradingViewCloseHistory body = do
  response <-
    either (Left . ("could not decode Pyth TradingView history: " <>) . T.pack) Right $
      eitherDecode body
  case response of
    TradingViewHistoryNoData -> Right []
    TradingViewHistoryOk timestamps closes
      | length timestamps /= length closes ->
          Left "Pyth TradingView history timestamp and close arrays have different lengths"
      | otherwise -> Right $ zip timestamps closes

-- | Combine six independently updating close series on the canonical minute
-- grid. A component close may carry forward for at most five minutes. Missing
-- or stale components make that minute sparse rather than inventing a price;
-- malformed series still fail the entire endpoint window before persistence.
deriveTradingViewBasketHistory
  :: Integer
  -> Integer
  -> [(BasketComponent, [(Integer, Scientific)])]
  -> Either Text [(Integer, Integer, Value)]
deriveTradingViewBasketHistory windowStart windowEndExclusive componentSeries = do
  when
    ( windowStart < 0
        || windowEndExclusive <= windowStart
        || windowStart `mod` tradingViewSampleIntervalSeconds /= 0
        || windowEndExclusive `mod` tradingViewSampleIntervalSeconds /= 0
    ) $
    Left "Pyth TradingView history window must be a non-empty aligned half-open range"
  unless
    ( sortOn id (map (normalizeFeedId . bcFeedId . fst) componentSeries)
        == sortOn id (map (normalizeFeedId . bcFeedId) basketComponents)
    ) $
    Left "Pyth TradingView history does not contain exactly the configured six feeds"
  seriesByFeed <-
    pure $
      Map.fromList
        [ (normalizeFeedId $ bcFeedId component, closes)
        | (component, closes) <- componentSeries
        ]
  orderedSeries <-
    forM basketComponents $ \component ->
      maybe
        (Left $ "missing Pyth TradingView feed " <> bcFeedId component)
        Right
        (Map.lookup (normalizeFeedId $ bcFeedId component) seriesByFeed)
  let fetchStart = max 0 $ windowStart - tradingViewMaximumCarrySeconds
  forM_ orderedSeries $ validateCloseSeries fetchStart windowEndExclusive
  let closeMaps = map Map.fromList orderedSeries
      canonicalTimestamps =
        [ windowStart
        , windowStart + tradingViewSampleIntervalSeconds
        .. windowEndExclusive - tradingViewSampleIntervalSeconds
        ]
  catMaybes <$> forM canonicalTimestamps (deriveAt closeMaps)
 where
  deriveAt closeMaps timestamp =
    case traverse (asOfClose timestamp) closeMaps of
      Nothing -> Right Nothing
      Just closes -> do
        points <-
          forM (zip basketComponents closes) $ \(component, (publishTime, close)) -> do
            price <- scientificCloseToPythPrice close
            pure
              PythPricePoint
                { pppFeedId = bcFeedId component
                , pppPrice = price
                , pppConfidence = 0
                , pppExponent = -8
                , pppPublishTime = publishTime
                }
        (basketPrice, components) <- computeBasketSnapshot points
        pure $ Just (timestamp, basketPrice, toJSON components)

  asOfClose timestamp closes = do
    (publishTime, close) <- Map.lookupLE timestamp closes
    if timestamp - publishTime <= tradingViewMaximumCarrySeconds
      then Just (publishTime, close)
      else Nothing

-- | A published minute range remains a physical write boundary even while its
-- coverage health is temporarily disabled. Target ingestion may add a missing
-- historical prefix, but must not introduce raw snapshot inputs anywhere from
-- the protected start onward: that includes both its current overlap and a
-- tail which live coverage could advance over before target publication.
--
-- In particular, bounded repair preserves the published coverage bounds while
-- marking every interval incomplete between independently committed chunks.
-- Treating that maintenance state as untrusted would let a target window race
-- into the already-public source domain between repair chunks.
filterTradingViewHistorySamplesForPersistence
  :: Maybe RollupCoverage
  -> [(Integer, Integer, Value)]
  -> Either Text [(Integer, Integer, Value)]
filterTradingViewHistorySamplesForPersistence coverage samples =
  case coverage of
    Nothing -> Right samples
    Just row ->
      case protectedPriceCoverageStart row of
        Right (Just coverageStart) ->
          Right $ filter (\(timestamp, _, _) -> timestamp < coverageStart) samples
        Right Nothing -> Right samples
        Left err -> Left err

protectedPriceCoverageStart :: RollupCoverage -> Either Text (Maybe Integer)
protectedPriceCoverageStart row@RollupCoverage
    { rcKind = PriceRollup
    , rcSeriesId = Just seriesId
    , rcChainId = Nothing
    , rcReleaseRouter = Nothing
    , rcIntervalSeconds = 60
    , rcCoverageStart = Just coverageStart
    , rcCoverageEnd = Just coverageEnd
    , rcFinalizedThrough = Just finalizedThrough
    }
  | seriesId == defaultBasketSeriesId
      && rcDerivationVersion row == tradingViewCandleDerivationVersion
      && rcGeneration row > 0
      && rcGeneration row < tradingViewGenerationRadix
      && coverageStart <= finalizedThrough
      && finalizedThrough <= coverageEnd =
      Right $
        if coverageStart < finalizedThrough
          then Just coverageStart
          else Nothing
protectedPriceCoverageStart row@RollupCoverage
  { rcKind = PriceRollup
  , rcSeriesId = Just seriesId
  , rcChainId = Nothing
  , rcReleaseRouter = Nothing
  , rcIntervalSeconds = 60
  , rcCoverageStart = Nothing
  , rcCoverageEnd = Nothing
  , rcFinalizedThrough = Nothing
  }
    | seriesId == defaultBasketSeriesId
        && rcDerivationVersion row == tradingViewCandleDerivationVersion
        && rcGeneration row > 0
        && rcGeneration row < tradingViewGenerationRadix =
        Right Nothing
protectedPriceCoverageStart _ =
  Left "Existing price coverage has no safe target-ingestion write boundary"

validateCloseSeries
  :: Integer
  -> Integer
  -> [(Integer, Scientific)]
  -> Either Text ()
validateCloseSeries windowStart windowEndExclusive closes = do
  let timestamps = map fst closes
  unless (and $ zipWith (<) timestamps $ drop 1 timestamps) $
    Left "Pyth TradingView history timestamps are not strictly increasing"
  unless
    ( all
        (\timestamp ->
          timestamp >= windowStart
            && timestamp < windowEndExclusive
            && timestamp `mod` tradingViewSampleIntervalSeconds == 0
        )
        timestamps
    ) $
    Left "Pyth TradingView history timestamp is outside the aligned request window"

scientificCloseToPythPrice :: Scientific -> Either Text Integer
scientificCloseToPythPrice close
  | close <= 0 = Left "Pyth TradingView close must be positive"
  | otherwise =
      case floatingOrInteger (close * 100_000_000) :: Either Double Integer of
        Right scaled -> Right scaled
        Left _ -> Left "Pyth TradingView close has more than eight decimal places"

parsePythPrice :: Text -> Value -> Parser PythPricePoint
parsePythPrice feedId = withObject "PythPrice" $ \v -> do
  price <- v .: "price" >>= parseIntegerish
  conf <- v .: "conf" >>= parseIntegerish
  expo <- v .: "expo" >>= parseIntish
  publishTime <- v .: "publish_time" >>= parseIntegerish
  pure
    PythPricePoint
      { pppFeedId = feedId
      , pppPrice = price
      , pppConfidence = conf
      , pppExponent = expo
      , pppPublishTime = publishTime
      }

fetchBasketSnapshotAt
  :: Manager
  -> Text
  -> Maybe Text
  -> Integer
  -> Integer
  -> IO (Either Text (Integer, Integer, Value, [PythPricePoint]))
fetchBasketSnapshotAt manager benchmarksUrl apiKey intervalSeconds timestamp = do
  requestBase <- parseRequest $ T.unpack requestUrl
  let request =
        setQueryString queryParams requestBase
          { requestHeaders = authHeaders <> requestHeaders requestBase
          }
  response <- httpLbs request manager
  let code = statusCode (responseStatus response)
  if code < 200 || code >= 300
    then pure $ Left $ "Pyth Benchmarks returned HTTP " <> T.pack (show code)
    else pure $ decodeSnapshot (responseBody response)
  where
    requestUrl =
      stripTrailingSlash benchmarksUrl
        <> "/v1/updates/price/"
        <> T.pack (show timestamp)
        <> "/"
        <> T.pack (show benchmarkWindow)

    benchmarkWindow = min 60 (max 1 intervalSeconds)

    queryParams =
      ("parsed", Just "true")
        : [("ids", Just (encodeUtf8 (bcFeedId component))) | component <- basketComponents]

    authHeaders =
      case apiKey of
        Just key | not (T.null $ T.strip key) ->
          [("Authorization", encodeUtf8 $ "Bearer " <> T.strip key)]
        _ -> []

    decodeSnapshot
      :: LBS.ByteString
      -> Either Text (Integer, Integer, Value, [PythPricePoint])
    decodeSnapshot body = do
      benchmarks <-
        case eitherDecode body of
          Right parsed -> Right [parsed]
          Left objectErr ->
            case eitherDecode body of
              Left arrayErr ->
                Left $
                  "could not decode Pyth Benchmarks response: "
                    <> T.pack objectErr
                    <> "; interval response decode also failed: "
                    <> T.pack arrayErr
              Right (parsed :: [BenchmarkResponse]) -> Right $ reverse parsed
      case firstCompleteSnapshot benchmarks of
        Nothing -> Left "Pyth Benchmarks returned no complete six-feed basket snapshot"
        Just result -> Right result

    firstCompleteSnapshot
      :: [BenchmarkResponse]
      -> Maybe (Integer, Integer, Value, [PythPricePoint])
    firstCompleteSnapshot [] = Nothing
    firstCompleteSnapshot (benchmark : rest) =
      case deriveBasketHistoryObservation timestamp benchmarkWindow (brParsed benchmark) of
        Left _ -> firstCompleteSnapshot rest
        Right observation -> Just observation

fetchTradingViewCloseHistory
  :: Manager
  -> Text
  -> Maybe Text
  -> BasketComponent
  -> Integer
  -> Integer
  -> IO (Either Text [(Integer, Scientific)])
fetchTradingViewCloseHistory
  manager
  benchmarksUrl
  apiKey
  component
  windowStart
  windowEndExclusive = do
    requestBase <- parseRequest $ T.unpack requestUrl
    let request =
          setQueryString queryParams requestBase
            { requestHeaders = authHeaders <> requestHeaders requestBase
            }
    response <- httpLbs request manager
    let code = statusCode $ responseStatus response
    if code < 200 || code >= 300
      then
        pure $
          Left $
            "Pyth TradingView history returned HTTP "
              <> T.pack (show code)
              <> " for "
              <> bcFeedSymbol component
      else
        pure $ do
          closes <- decodeTradingViewCloseHistory $ responseBody response
          validateCloseSeries fetchStart windowEndExclusive closes
          pure closes
  where
    -- Pyth component feeds do not necessarily update on the same minute. Seed
    -- the first canonical minute from a recent close immediately before this
    -- endpoint window; derivation below still emits only the requested range.
    fetchStart = max 0 $ windowStart - tradingViewMaximumCarrySeconds
    requestUrl =
      stripTrailingSlash benchmarksUrl <> "/v1/shims/tradingview/history"
    queryParams =
      [ ("symbol", Just $ encodeUtf8 $ "FX." <> bcFeedSymbol component)
      , ("resolution", Just "1")
      , ("from", Just $ encodeUtf8 $ T.pack $ show fetchStart)
      -- The UDF endpoint treats @to@ as inclusive. Request one second before
      -- the frozen exclusive end and validate every returned timestamp again.
      , ("to", Just $ encodeUtf8 $ T.pack $ show $ windowEndExclusive - 1)
      ]
    authHeaders =
      case apiKey of
        Just key | not (T.null $ T.strip key) ->
          [("Authorization", encodeUtf8 $ "Bearer " <> T.strip key)]
        _ -> []

startBasketHistoryIngestor :: Manager -> DbPool -> BasketIngestorConfig -> IO ()
startBasketHistoryIngestor manager pool cfg = forever $ do
  runBasketBackfill manager pool cfg `catch` logException
  threadDelay (bicPollSeconds cfg * 1_000_000)

runBasketBackfill :: Manager -> DbPool -> BasketIngestorConfig -> IO ()
runBasketBackfill manager pool cfg
  | bicOwnHistoryTargets cfg =
      withTargetHistoryLeadership pool $ runBasketBackfillUnlocked manager pool cfg
  | otherwise = runBasketBackfillUnlocked manager pool cfg

runBasketBackfillUnlocked :: Manager -> DbPool -> BasketIngestorConfig -> IO ()
runBasketBackfillUnlocked manager pool cfg = do
  now <- round <$> getPOSIXTime
  latestTarget <-
    withDb pool $ \conn -> do
      ensureCurrentBasketDefinition conn defaultBasketSeriesId
      ensureCandleMarketIdentity
        conn
        defaultCandleMarketId
        (bicChainId cfg)
        defaultBasketSeriesId
      getLatestCandleHistoryIngestionProgress
        conn
        defaultCandleMarketId
        (bicChainId cfg)
        defaultBasketSeriesId
  case latestTarget of
    Nothing
      | bicOwnHistoryTargets cfg -> pure ()
      | otherwise -> runLegacyBasketBackfill manager pool cfg now
    Just (selection, existingProgress)
      | bicOwnHistoryTargets cfg ->
          runTargetBasketBackfill
            manager
            pool
            cfg
            now
            selection
            existingProgress
      | otherwise -> pure ()

-- Keep outbound target requests globally single-owner, not merely
-- transaction-idempotent. Multiple worker replicas may be healthy at once;
-- only the session holding this advisory lock may contact the six history
-- feeds. PostgreSQL releases the lock automatically if the owner dies.
withTargetHistoryLeadership :: DbPool -> IO () -> IO ()
withTargetHistoryLeadership pool action =
  withDb pool $ \lockConnection -> do
    rows <-
      query
        lockConnection
        "SELECT pg_try_advisory_lock(?)"
        (Only targetHistoryLeaderLockId) :: IO [Only Bool]
    case rows of
      [Only True] -> action `finally` release lockConnection
      [Only False] ->
        logInfo
          "pyth_history_target_leader_busy"
          "Another basket worker owns Pyth target-history ingestion"
          []
      _ -> fail "Pyth target-history leader lock lookup was not unique"
 where
  release connection = do
    _ <-
      query
        connection
        "SELECT pg_advisory_unlock(?)"
        (Only targetHistoryLeaderLockId) :: IO [Only Bool]
    pure ()

runTargetBasketBackfill
  :: Manager
  -> DbPool
  -> BasketIngestorConfig
  -> Integer
  -> CandleHistorySelection
  -> Maybe CandleHistoryIngestionProgress
  -> IO ()
runTargetBasketBackfill manager pool cfg now selection existingProgress = do
  let interval = tradingViewSampleIntervalSeconds
  selectedStart <-
    either (ioError . userError . T.unpack) pure $
      effectiveHistoryStart interval $ chsRequestedStartTimestamp selection
  progress <-
    case existingProgress of
      Just stored -> pure $ Just stored
      Nothing -> do
        let sourceSafeEnd =
              alignDown
                (max 0 $ now - max 0 (bicCandleLatenessSeconds cfg))
                interval
        let minimumPublicationEnd =
              minimumBasketHistoryPublicationEnd selectedStart
        sourceAvailable <-
          if sourceSafeEnd < minimumPublicationEnd
            then pure False
            else
              withDb pool $ \conn ->
                canonicalPriceSourceExists conn selectedStart sourceSafeEnd
        if sourceSafeEnd < minimumPublicationEnd || not sourceAvailable
          then do
            logInfo
              "pyth_history_target_waiting_for_safe_end"
              "Pyth TradingView target is waiting for a publishable source-safe range"
              [ field "market_id" $ chsMarketId selection
              , field "target_revision" $ chsRevision selection
              , field "selected_start_timestamp" selectedStart
              , field "source_safe_end_exclusive" sourceSafeEnd
              , field "minimum_publication_end_exclusive" minimumPublicationEnd
              , field "canonical_source_available" sourceAvailable
              ]
            pure Nothing
          else
            withDb pool $ \conn -> withTransaction conn $ do
              latest <-
                candleHistorySelectionIsLatest
                  conn
                  (bicChainId cfg)
                  defaultBasketSeriesId
                  selection
              if latest
                then
                  Just
                    <$> initializeCandleHistoryIngestionProgress
                      conn
                      selection
                      selectedStart
                      sourceSafeEnd
                      interval
                else pure Nothing
  forM_ progress $ ingestWindows
 where
  ingestWindows current
    | chipComplete current =
        logInfo
          "pyth_history_target_ingestion_complete"
          "Pyth TradingView target ingestion is complete"
          [ field "market_id" $ chipMarketId current
          , field "target_revision" $ chipTargetRevision current
          , field "start_timestamp" $ chipStartTimestamp current
          , field "end_timestamp_exclusive" $ chipEndTimestampExclusive current
          , field "next_timestamp" $ chipNextTimestamp current
          , field "sample_interval_seconds" $ chipSampleIntervalSeconds current
          ]
    | otherwise = do
        stillLatest <- latestSelectionMatches pool cfg selection
        if not stillLatest
          then
            logInfo
              "pyth_history_target_replaced"
              "Stopped Pyth TradingView ingestion because a newer target was selected"
              [ field "market_id" $ chsMarketId selection
              , field "target_revision" $ chsRevision selection
              ]
          else do
            let windowStart = chipNextTimestamp current
                windowEndExclusive =
                  min
                    (chipEndTimestampExclusive current)
                    (windowStart + tradingViewMaximumWindowSeconds)
            result <-
              try @SomeException $
                fetchTradingViewComponentWindow
                  manager
                  cfg
                  windowStart
                  windowEndExclusive
            case result of
              Left err ->
                stopWithError
                  current
                  windowStart
                  ("Pyth TradingView request failed: " <> T.pack (displayException err))
              Right componentResults ->
                case sequence componentResults >>= deriveWindow windowStart windowEndExclusive of
                  Left err -> stopWithError current windowStart err
                  Right samples -> do
                    committed <-
                      commitTargetWindow
                        pool
                        cfg
                        selection
                        current
                        windowStart
                        windowEndExclusive
                        samples
                    case committed of
                      Nothing ->
                        logInfo
                          "pyth_history_target_replaced"
                          "Discarded a fetched Pyth TradingView window after target replacement"
                          [ field "market_id" $ chsMarketId selection
                          , field "target_revision" $ chsRevision selection
                          , field "window_start" windowStart
                          , field "window_end_exclusive" windowEndExclusive
                          ]
                      Just advanced -> do
                        logInfo
                          "pyth_history_target_window_complete"
                          "Committed one complete Pyth TradingView endpoint window"
                          [ field "market_id" $ chsMarketId selection
                          , field "target_revision" $ chsRevision selection
                          , field "window_start" windowStart
                          , field "window_end_exclusive" windowEndExclusive
                          , field "sample_count" $ length samples
                          , field "next_timestamp" $ chipNextTimestamp advanced
                          , field "complete" $ chipComplete advanced
                          ]
                        unless (chipComplete advanced) $
                          threadDelay tradingViewBatchDelayMicroseconds
                        ingestWindows advanced

  deriveWindow windowStart windowEndExclusive closes =
    deriveTradingViewBasketHistory
      windowStart
      windowEndExclusive
      (zip basketComponents closes)

  stopWithError current windowStart err = do
    recorded <-
      recordTargetErrorIfLatest pool cfg selection windowStart err
    when recorded $
      logWarnEvery
        60
        "pyth_history_target_window_failed"
        "Pyth TradingView target ingestion stopped at an unproved endpoint window"
        [ field "market_id" $ chsMarketId selection
        , field "target_revision" $ chsRevision selection
        , field "window_start" windowStart
        , field "end_timestamp_exclusive" $ chipEndTimestampExclusive current
        , field "error" err
        ]
    when (recorded && "HTTP 429" `T.isInfixOf` err) $
      threadDelay 60_000_000

fetchTradingViewComponentWindow
  :: Manager
  -> BasketIngestorConfig
  -> Integer
  -> Integer
  -> IO [Either Text [(Integer, Scientific)]]
fetchTradingViewComponentWindow manager cfg windowStart windowEndExclusive =
  fetchBatches $ chunksOf two basketComponents
 where
  two = 2
  fetchBatches [] = pure []
  fetchBatches (componentBatch : remaining) = do
    results <-
      mapConcurrently
        ( \component ->
            fetchTradingViewCloseHistory
              manager
              (bicBenchmarksUrl cfg)
              (bicApiKey cfg)
              component
              windowStart
              windowEndExclusive
        )
        componentBatch
    if any isFailure results
      then pure results
      else do
        unless (null remaining) $
          -- Keep each two-request burst below one request/second on average.
          -- TradingView history and signed order reveal share Pyth's public
          -- IP quota, so endpoint-window spacing alone is insufficient.
          threadDelay tradingViewComponentBatchDelayMicroseconds
        (results <>) <$> fetchBatches remaining
  isFailure = \case Left _ -> True; Right _ -> False

chunksOf :: Int -> [value] -> [[value]]
chunksOf size values
  | size <= 0 = []
  | otherwise = go values
 where
  go [] = []
  go remaining =
    let (current, rest) = splitAt size remaining
     in current : go rest

commitTargetWindow
  :: DbPool
  -> BasketIngestorConfig
  -> CandleHistorySelection
  -> CandleHistoryIngestionProgress
  -> Integer
  -> Integer
  -> [(Integer, Integer, Value)]
  -> IO (Maybe CandleHistoryIngestionProgress)
commitTargetWindow
  pool
  cfg
  selection
  expectedProgress
  windowStart
  windowEndExclusive
  samples =
    withDb pool $ \conn -> withTransaction conn $ do
      latest <-
        candleHistorySelectionIsLatest
          conn
          (bicChainId cfg)
          defaultBasketSeriesId
          selection
      if not latest
        then pure Nothing
        else do
          progress <-
            getLatestCandleHistoryIngestionProgress
              conn
              defaultCandleMarketId
              (bicChainId cfg)
              defaultBasketSeriesId
          case progress of
            Just (latestSelection, Just stored)
              | latestSelection == selection
                  && stored == expectedProgress -> do
                    -- Serialize the protected-coverage snapshot with live price
                    -- writers and protected publication before deciding which
                    -- raw inputs are safe to add.
                    lockBasketPriceDataset conn defaultBasketSeriesId
                    publishedCoverage <-
                      getRollupCoverage
                        conn
                        PriceRollup
                        (Just defaultBasketSeriesId)
                        Nothing
                        Nothing
                        tradingViewSampleIntervalSeconds
                    case
                      filterTradingViewHistorySamplesForPersistence
                        publishedCoverage
                        samples
                      of
                        Left err -> fail $ T.unpack err
                        Right snapshotsToPersist -> do
                          insertBasketSnapshotsWithSource
                            conn
                            [ ( timestamp
                              , tradingViewSampleIntervalSeconds
                              , basketPrice
                              , components
                              )
                            | (timestamp, basketPrice, components) <- snapshotsToPersist
                            ]
                            tradingViewHistorySource
                          Just
                            <$> completeCandleHistoryIngestionWindow
                              conn
                              selection
                              windowStart
                              windowEndExclusive
                              (fromIntegral $ length samples)
              | latestSelection == selection
                  && chipNextTimestamp stored /= windowStart ->
                    -- A redundant worker committed first. Continue from the
                    -- durable cursor without replaying this endpoint window.
                    pure $ Just stored
            _ -> fail "Latest candle history ingestion state changed unexpectedly"

recordTargetErrorIfLatest
  :: DbPool
  -> BasketIngestorConfig
  -> CandleHistorySelection
  -> Integer
  -> Text
  -> IO Bool
recordTargetErrorIfLatest pool cfg selection windowStart err =
  withDb pool $ \conn -> withTransaction conn $ do
    latest <-
      candleHistorySelectionIsLatest
        conn
        (bicChainId cfg)
        defaultBasketSeriesId
        selection
    when latest $
      recordCandleHistoryIngestionError conn selection windowStart err
    pure latest

latestSelectionMatches
  :: DbPool
  -> BasketIngestorConfig
  -> CandleHistorySelection
  -> IO Bool
latestSelectionMatches pool cfg expected =
  withDb pool $ \conn -> do
    latest <-
      getLatestCandleHistorySelection
        conn
        defaultCandleMarketId
        (bicChainId cfg)
        defaultBasketSeriesId
    pure $ latest == Just expected

canonicalPriceSourceExists :: Connection -> Integer -> Integer -> IO Bool
canonicalPriceSourceExists conn fromTimestamp toTimestamp = do
  rows <-
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
      ) :: IO [Only Bool]
  case rows of
    [Only available] -> pure available
    _ -> fail "Canonical basket source availability lookup was not unique"

runLegacyBasketBackfill
  :: Manager
  -> DbPool
  -> BasketIngestorConfig
  -> Integer
  -> IO ()
runLegacyBasketBackfill manager pool cfg now = do
  let interval = max 60 $ bicSampleIntervalSeconds cfg
      endTs = alignDown now interval
  earliestTs <-
    either (ioError . userError . T.unpack) pure $
      deriveEarliestBasketGridTimestamp
        interval
        endTs
        (bicBackfillDays cfg)
        Nothing
  _ <-
    foldM
      (ingestLegacyWindow interval earliestTs)
      True
      (basketBackfillGridWindows interval earliestTs endTs)
  pure ()
 where
  ingestLegacyWindow _ _ False _ = pure False
  ingestLegacyWindow interval earliestTs True (windowStart, windowEnd) = do
    allowed <- legacyBackfillStillAllowed pool cfg
    if not allowed
      then logLegacyBackfillStopped >> pure False
      else do
        existingTimes <-
          withDb pool $ \conn ->
            getBasketSnapshotTimes conn windowStart windowEnd interval
        let existing = Set.fromList existingTimes
            missing =
              filter
                (`Set.notMember` existing)
                [windowStart, windowStart + interval .. windowEnd]
        when (not $ null missing) $
          logInfo
            "pyth_history_backfill_started"
            "Pyth basket history backfill window started"
            [ field "missing_snapshot_count" $ length missing
            , field "from_timestamp" windowStart
            , field "to_timestamp" windowEnd
            , field "selected_history_start_timestamp" earliestTs
            , field "sample_interval_seconds" interval
            ]
        foldM (ingestLegacyTimestamp interval) True missing

  ingestLegacyTimestamp _ False _ = pure False
  ingestLegacyTimestamp interval True ts = do
    allowed <- legacyBackfillStillAllowed pool cfg
    if not allowed
      then logLegacyBackfillStopped >> pure False
      else do
        result <-
          try @SomeException $
            fetchBasketSnapshotAt
              manager
              (bicBenchmarksUrl cfg)
              (bicApiKey cfg)
              interval
              ts
        continue <- case result of
          Left err -> do
            logWarnEvery
              60
              "pyth_history_snapshot_fetch_failed"
              "Pyth basket history snapshot fetch failed"
              [ field "snapshot_timestamp" ts
              , field "error" $ displayException err
              ]
            pure True
          Right (Left err) -> do
            logWarnEvery
              60
              "pyth_history_snapshot_fetch_failed"
              "Pyth basket history snapshot fetch failed"
              [ field "snapshot_timestamp" ts
              , field "error" err
              ]
            when ("429" `T.isInfixOf` err) $ threadDelay 60_000_000
            pure True
          Right (Right (canonicalPublishTime, basketPrice, components, pricePoints)) -> do
            persisted <-
              withDb pool $ \conn -> withTransaction conn $ do
                allowedAtCommit <-
                  candleHistorySelectionIsAbsent
                    conn
                    defaultCandleMarketId
                    (bicChainId cfg)
                    defaultBasketSeriesId
                if not allowedAtCommit
                  then pure False
                  else do
                    when (bicCandleWriteMode cfg == PerpsCandleWritesDual) $ do
                      ensureCurrentBasketDefinition conn defaultBasketSeriesId
                      changed <-
                        upsertBasketObservation
                          conn
                          BasketObservationInput
                            { boiSeriesId = defaultBasketSeriesId
                            , boiObservationId = legacyObservationId defaultBasketSeriesId pricePoints
                            , boiPublishTime = canonicalPublishTime
                            , boiBasketPrice = basketPrice
                            , boiComponentPrices = components
                            , boiSource = "legacy_sampled"
                            , boiSourcePriority = legacyObservationPriority
                            }
                      when changed $
                        recomputeBasketCandleHierarchy
                          conn
                          defaultBasketSeriesId
                          canonicalPublishTime
                          (bicCandleLatenessSeconds cfg)
                    -- The legacy table remains keyed by the requested sampling grid so
                    -- the existing raw-history API can detect completed backfill slots.
                    -- The observation ledger above carries the canonical source time.
                    insertBasketSnapshot conn ts interval basketPrice components
                    pure True
            unless persisted logLegacyBackfillStopped
            pure persisted
        -- Public Pyth endpoints are IP-rate-limited. Keep historical backfills
        -- below one request per second so chart ingestion cannot starve order reveal.
        when continue $ threadDelay 1_250_000
        pure continue

  logLegacyBackfillStopped =
    logInfo
      "pyth_legacy_backfill_stopped_for_history_target"
      "Stopped legacy point backfill because a history target is selected"
      [field "market_id" defaultCandleMarketId]

legacyBackfillStillAllowed :: DbPool -> BasketIngestorConfig -> IO Bool
legacyBackfillStillAllowed pool cfg =
  withDb pool $ \conn -> do
    latest <-
      getLatestCandleHistorySelection
        conn
        defaultCandleMarketId
        (bicChainId cfg)
        defaultBasketSeriesId
    pure $ latest == Nothing

alignDown :: Integer -> Integer -> Integer
alignDown timestamp interval = (timestamp `div` interval) * interval

alignUp :: Integer -> Integer -> Integer
alignUp timestamp interval = ((timestamp + interval - 1) `div` interval) * interval

tradingViewSampleIntervalSeconds :: Integer
tradingViewSampleIntervalSeconds = 60

tradingViewMaximumWindowSeconds :: Integer
tradingViewMaximumWindowSeconds = 2 * 86_400

tradingViewMaximumCarrySeconds :: Integer
tradingViewMaximumCarrySeconds = 300

tradingViewBatchDelayMicroseconds :: Int
tradingViewBatchDelayMicroseconds = 2_000_000

tradingViewComponentBatchDelayMicroseconds :: Int
tradingViewComponentBatchDelayMicroseconds = 2_000_000

tradingViewHistorySource :: Text
tradingViewHistorySource = "pyth_tradingview_history_v1"

tradingViewCandleDerivationVersion :: Text
tradingViewCandleDerivationVersion = "v1"

tradingViewGenerationRadix :: Integer
tradingViewGenerationRadix = 67_108_864

targetHistoryLeaderLockId :: Integer
targetHistoryLeaderLockId = 4_278_619_032

parseIntegerish :: Value -> Parser Integer
parseIntegerish = \case
  String txt ->
    case readMaybe (T.unpack txt) of
      Just value -> pure value
      Nothing -> fail $ "expected integer string, got " <> T.unpack txt
  Number n ->
    case floatingOrInteger n :: Either Double Integer of
      Right value -> pure value
      Left (_ :: Double) -> fail "expected integer number"
  other -> fail $ "expected integer, got " <> show other

parseIntish :: Value -> Parser Int
parseIntish value = fromInteger <$> parseIntegerish value

stripTrailingSlash :: Text -> Text
stripTrailingSlash = T.dropWhileEnd (== '/')

legacyObservationPriority :: Int
legacyObservationPriority = 10

logException :: SomeException -> IO ()
logException err =
  logErrorEvery
    60
    "pyth_history_ingestor_failed"
    "Pyth basket history ingestor failed"
    [field "error" $ displayException err]
