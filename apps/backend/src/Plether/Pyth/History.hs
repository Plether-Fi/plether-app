module Plether.Pyth.History
  ( BasketIngestorConfig (..)
  , basketObservationId
  , deriveBasketHistoryObservation
  , fetchBasketSnapshotAt
  , legacyObservationId
  , runBasketBackfill
  , startBasketHistoryIngestor
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, catch, displayException, try)
import Control.Monad (forM_, forever, when)
import Data.Aeson (FromJSON (..), Value (..), eitherDecode, toJSON, withObject, (.:))
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as LBS
import Data.List (sortOn)
import qualified Data.Set as Set
import Data.Scientific (floatingOrInteger)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Encoding as TE
import Data.Time.Clock.POSIX (getPOSIXTime)
import Database.PostgreSQL.Simple (withTransaction)
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
import Plether.Database.Candles
  ( BasketObservationInput (..)
  , defaultBasketSeriesId
  , ensureCurrentBasketDefinition
  , recomputeBasketCandleHierarchy
  , upsertBasketObservation
  )
import Plether.Database.Schema
  ( getBasketSnapshotTimes
  , insertBasketSnapshot
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
  , bicBackfillDays :: Int
  , bicSampleIntervalSeconds :: Integer
  , bicPollSeconds :: Int
  , bicCandleWriteMode :: PerpsCandleWriteMode
  , bicCandleLatenessSeconds :: Integer
  }
  deriving stock (Show)

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

startBasketHistoryIngestor :: Manager -> DbPool -> BasketIngestorConfig -> IO ()
startBasketHistoryIngestor manager pool cfg = forever $ do
  runBasketBackfill manager pool cfg `catch` logException
  threadDelay (bicPollSeconds cfg * 1_000_000)

runBasketBackfill :: Manager -> DbPool -> BasketIngestorConfig -> IO ()
runBasketBackfill manager pool cfg = do
  now <- round <$> getPOSIXTime
  let interval = max 60 (bicSampleIntervalSeconds cfg)
      endTs = (now `div` interval) * interval
      earliestTs = endTs - fromIntegral (max 1 (bicBackfillDays cfg)) * 86_400

  existingTimes <- withDb pool $ \conn -> getBasketSnapshotTimes conn earliestTs endTs interval
  let existing = Set.fromList existingTimes
      missing = filter (`Set.notMember` existing) [earliestTs, earliestTs + interval .. endTs]

  when (not (null missing)) $ do
    logInfo
      "pyth_history_backfill_started"
      "Pyth basket history backfill started"
      [ field "missing_snapshot_count" $ length missing
      , field "from_timestamp" earliestTs
      , field "to_timestamp" endTs
      , field "sample_interval_seconds" interval
      ]
    forM_ missing $ \ts -> do
      result <-
        try @SomeException $
          fetchBasketSnapshotAt
            manager
            (bicBenchmarksUrl cfg)
            (bicApiKey cfg)
            interval
            ts
      case result of
        Left err ->
          logWarnEvery
            60
            "pyth_history_snapshot_fetch_failed"
            "Pyth basket history snapshot fetch failed"
            [ field "snapshot_timestamp" ts
            , field "error" $ displayException err
            ]
        Right (Left err) ->
          do
            logWarnEvery
              60
              "pyth_history_snapshot_fetch_failed"
              "Pyth basket history snapshot fetch failed"
              [ field "snapshot_timestamp" ts
              , field "error" err
              ]
            when ("429" `T.isInfixOf` err) $ threadDelay 60_000_000
        Right (Right (canonicalPublishTime, basketPrice, components, pricePoints)) ->
          withDb pool $ \conn -> withTransaction conn $ do
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
      -- Public Pyth endpoints are IP-rate-limited. Keep historical backfills
      -- below one request per second so chart ingestion cannot starve order reveal.
      threadDelay 1_250_000

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
