module Plether.Types.Perps
  ( BasketHistory (..)
  , BasketHistoryParams (..)
  , BasketHistoryPoint (..)
  , BasketCandle (..)
  , BasketCandlePage (..)
  , BasketCurrentCandle (..)
  , canonicalBasketCandleIntervals
  , basketCandlePageSize
  , basketCandlePageSpan
  , isCanonicalBasketCandleInterval
  , isAlignedBasketCandleCursor
  , isBasketCandleCursorWithinFutureBound
  , hasExactBasketCandleQueryKeys
  , parseCanonicalPositiveInteger
  , parseBasketHistoryQueryParams
  , BasketLatest (..)
  , PythUpdateResponse (..)
  , RevealPayloadResponse (..)
  , basketRangeSeconds
  , defaultBasketHistoryParams
  ) where

import Data.Aeson (ToJSON (..), Value, object, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (POSIXTime)
import GHC.Generics (Generic)

-- | Candle resolutions supported by the persisted rollup read model. Keeping
-- this list closed makes storage, pagination and edge-cache keys deterministic.
canonicalBasketCandleIntervals :: [Integer]
canonicalBasketCandleIntervals = [60, 180, 300, 900, 1800, 3600, 86_400]

basketCandlePageSize :: Integer
basketCandlePageSize = 500

isCanonicalBasketCandleInterval :: Integer -> Bool
isCanonicalBasketCandleInterval interval = interval `elem` canonicalBasketCandleIntervals

basketCandlePageSpan :: Integer -> Maybe Integer
basketCandlePageSpan interval
  | isCanonicalBasketCandleInterval interval = Just $ interval * basketCandlePageSize
  | otherwise = Nothing

isAlignedBasketCandleCursor :: Integer -> Integer -> Bool
isAlignedBasketCandleCursor interval cursor =
  case basketCandlePageSpan interval of
    Just pageSpan -> cursor > 0 && cursor `mod` pageSpan == 0
    Nothing -> False

-- | Allow the fixed page containing the backend clock plus one page for modest
-- browser/backend clock skew, matching the edge proxy. The single-page grace
-- remains a strict bound against arbitrary future-window scans.
isBasketCandleCursorWithinFutureBound :: Integer -> Integer -> Integer -> Bool
isBasketCandleCursorWithinFutureBound now interval cursor =
  case basketCandlePageSpan interval of
    Just pageSpan
      | now >= 0
      , isAlignedBasketCandleCursor interval cursor ->
          let containingPageEnd = ((now + pageSpan - 1) `div` pageSpan) * pageSpan
           in cursor <= containingPageEnd + pageSpan
    _ -> False

hasExactBasketCandleQueryKeys :: [Text] -> [Text] -> Bool
hasExactBasketCandleQueryKeys required actual =
  length required == length actual
    && all (\key -> count key actual == 1) required
    && all (`elem` required) actual
  where
    count key = length . filter (== key)

-- | Parse the unique decimal representation of a positive integer. Public
-- candle URLs use the raw query text in cache keys, so accepting whitespace,
-- signs, or leading zeroes would let multiple spellings reach the same DB
-- query while bypassing the canonical request contract.
parseCanonicalPositiveInteger :: Text -> Maybe Integer
parseCanonicalPositiveInteger value =
  case T.uncons value of
    Just (first, rest)
      | first >= '1'
      , first <= '9'
      , T.compareLength value 20 /= GT
      , T.all (\c -> c >= '0' && c <= '9') rest ->
          Just $ read $ T.unpack value
    _ -> Nothing

-- | Public candle fields intentionally remain in the canonical/raw oracle
-- domain. Consumers applying a decreasing display transform must swap high
-- and low. Integer values are encoded as decimal strings to remain lossless in
-- JavaScript. Volume is nullable because price history may predate the current
-- router; null means unavailable, while zero means complete coverage proved no
-- activity in that bucket.
data BasketCandle = BasketCandle
  { bcTimestamp :: Integer
  , bcRawOpenPrice :: Integer
  , bcRawHighPrice :: Integer
  , bcRawLowPrice :: Integer
  , bcRawClosePrice :: Integer
  , bcVolumeUsdc :: Maybe Integer
  , bcTradeCount :: Maybe Integer
  , bcSampleCount :: Integer
  , bcQuality :: Text
  , bcRevision :: Integer
  , bcPriceComplete :: Bool
  , bcVolumeComplete :: Bool
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON BasketCandle where
  toJSON BasketCandle {..} =
    object
      [ "timestamp" .= bcTimestamp
      , "rawOpenPrice" .= show bcRawOpenPrice
      , "rawHighPrice" .= show bcRawHighPrice
      , "rawLowPrice" .= show bcRawLowPrice
      , "rawClosePrice" .= show bcRawClosePrice
      , "volumeUsdc" .= fmap show bcVolumeUsdc
      , "tradeCount" .= bcTradeCount
      , "sampleCount" .= bcSampleCount
      , "quality" .= bcQuality
      , "revision" .= bcRevision
      , "priceComplete" .= bcPriceComplete
      , "volumeComplete" .= bcVolumeComplete
      , "complete" .= (bcPriceComplete && bcVolumeComplete)
      ]

-- Page-level coverage describes the price dataset. Volume completeness is
-- reported independently on every candle.
data BasketCandlePage = BasketCandlePage
  { bcpIntervalSeconds :: Integer
  , bcpCursor :: Integer
  , bcpSeriesId :: Text
  , bcpConfigurationHash :: Text
  , bcpDisplayPriceCap :: Integer
  , bcpVolumeChainId :: Integer
  , bcpVolumeRouter :: Text
  , bcpVolumeCoverageStart :: Maybe Integer
  , bcpVolumeCoverageEnd :: Maybe Integer
  , bcpVolumeFinalizedThrough :: Maybe Integer
  , bcpVolumeCoverageComplete :: Bool
  , bcpPreviousCursor :: Maybe Integer
  , bcpHasEarlier :: Bool
  , bcpCoverageStart :: Maybe Integer
  , bcpCoverageEnd :: Maybe Integer
  , bcpFinalizedThrough :: Maybe Integer
  , bcpDatasetGeneration :: Integer
  , bcpCoverageComplete :: Bool
  , bcpCandles :: [BasketCandle]
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON BasketCandlePage where
  toJSON BasketCandlePage {..} =
    object
      [ "intervalSeconds" .= bcpIntervalSeconds
      , "cursor" .= bcpCursor
      , "seriesId" .= bcpSeriesId
      , "configurationHash" .= bcpConfigurationHash
      , "displayPriceCap" .= show bcpDisplayPriceCap
      , "volumeChainId" .= bcpVolumeChainId
      , "volumeRouter" .= bcpVolumeRouter
      , "volumeCoverageStart" .= bcpVolumeCoverageStart
      , "volumeCoverageEnd" .= bcpVolumeCoverageEnd
      , "volumeFinalizedThrough" .= bcpVolumeFinalizedThrough
      , "volumeCoverageComplete" .= bcpVolumeCoverageComplete
      , "previousCursor" .= bcpPreviousCursor
      , "hasEarlier" .= bcpHasEarlier
      , "coverageStart" .= bcpCoverageStart
      , "coverageEnd" .= bcpCoverageEnd
      , "finalizedThrough" .= bcpFinalizedThrough
      , "datasetGeneration" .= bcpDatasetGeneration
      , "coverageComplete" .= bcpCoverageComplete
      , "candles" .= bcpCandles
      ]

data BasketCurrentCandle = BasketCurrentCandle
  { bccIntervalSeconds :: Integer
  , bccSeriesId :: Text
  , bccConfigurationHash :: Text
  , bccDisplayPriceCap :: Integer
  , bccVolumeChainId :: Integer
  , bccVolumeRouter :: Text
  , bccVolumeCoverageStart :: Maybe Integer
  , bccVolumeCoverageEnd :: Maybe Integer
  , bccVolumeFinalizedThrough :: Maybe Integer
  , bccVolumeCoverageComplete :: Bool
  , bccDatasetGeneration :: Integer
  , bccCoverageStart :: Maybe Integer
  , bccCoverageEnd :: Maybe Integer
  , bccFinalizedThrough :: Maybe Integer
  , bccCoverageComplete :: Bool
  , bccCandle :: Maybe BasketCandle
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON BasketCurrentCandle where
  toJSON BasketCurrentCandle {..} =
    object
      [ "intervalSeconds" .= bccIntervalSeconds
      , "seriesId" .= bccSeriesId
      , "configurationHash" .= bccConfigurationHash
      , "displayPriceCap" .= show bccDisplayPriceCap
      , "volumeChainId" .= bccVolumeChainId
      , "volumeRouter" .= bccVolumeRouter
      , "volumeCoverageStart" .= bccVolumeCoverageStart
      , "volumeCoverageEnd" .= bccVolumeCoverageEnd
      , "volumeFinalizedThrough" .= bccVolumeFinalizedThrough
      , "volumeCoverageComplete" .= bccVolumeCoverageComplete
      , "datasetGeneration" .= bccDatasetGeneration
      , "coverageStart" .= bccCoverageStart
      , "coverageEnd" .= bccCoverageEnd
      , "finalizedThrough" .= bccFinalizedThrough
      , "coverageComplete" .= bccCoverageComplete
      , "candle" .= bccCandle
      ]

data BasketHistoryParams = BasketHistoryParams
  { bhpRange :: Text
  , bhpIntervalSeconds :: Integer
  , bhpIncludeComponents :: Bool
  }
  deriving stock (Eq, Show)

-- | Parse the compatibility history endpoint's deliberately small public
-- query surface. Requiring exact keys and canonical values keeps malformed
-- requests from being silently rewritten into a different database query.
parseBasketHistoryQueryParams
  :: [Text]
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Either Text BasketHistoryParams
parseBasketHistoryQueryParams queryKeys mRange mInterval mIncludeComponents = do
  includeComponents <-
    if hasExactBasketCandleQueryKeys ["range", "interval"] queryKeys
      then Right False
      else
        if hasExactBasketCandleQueryKeys ["range", "interval", "includeComponents"] queryKeys
          then
            case mIncludeComponents of
              Just "true" -> Right True
              Just "false" -> Right False
              _ -> Left "includeComponents must be true or false"
          else
            Left
              "exactly one range and one interval query parameter and at most one includeComponents query parameter are required"
  range <-
    case mRange of
      Just value | value `elem` ["24h", "7d", "30d", "1y"] -> Right value
      _ -> Left "range must be one of 24h, 7d, 30d, or 1y"
  interval <-
    case mInterval >>= parseCanonicalPositiveInteger of
      Just value -> Right value
      Nothing -> Left "interval must be a canonical positive integer"
  Right
    BasketHistoryParams
      { bhpRange = range
      , bhpIntervalSeconds = interval
      , bhpIncludeComponents = includeComponents
      }

defaultBasketHistoryParams :: BasketHistoryParams
defaultBasketHistoryParams =
  BasketHistoryParams
    { bhpRange = "7d"
    , bhpIntervalSeconds = 60
    , bhpIncludeComponents = False
    }

basketRangeSeconds :: Text -> Integer
basketRangeSeconds range =
  case T.toLower range of
    "24h" -> 24 * 60 * 60
    "30d" -> 30 * 24 * 60 * 60
    "1y" -> 365 * 24 * 60 * 60
    _ -> 7 * 24 * 60 * 60

data BasketHistoryPoint = BasketHistoryPoint
  { bhpTimestamp :: Integer
  , bhpBasketPrice :: Integer
  , bhpVolumeUsdc :: Integer
  , bhpComponents :: Maybe Value
  }
  deriving stock (Show, Generic)

instance ToJSON BasketHistoryPoint where
  toJSON BasketHistoryPoint {..} =
    object $
      [ "timestamp" .= bhpTimestamp
      , "basketPrice" .= show bhpBasketPrice
      , "volumeUsdc" .= show bhpVolumeUsdc
      ]
        <> maybe [] (\components -> ["components" .= components]) bhpComponents

data BasketHistory = BasketHistory
  { bhRange :: Text
  , bhIntervalSeconds :: Integer
  , bhSource :: Text
  , bhGeneratedAt :: POSIXTime
  , bhLatestPrice :: Maybe Integer
  , bhChangePct :: Maybe Double
  , bhPoints :: [BasketHistoryPoint]
  }
  deriving stock (Show, Generic)

instance ToJSON BasketHistory where
  toJSON BasketHistory {..} =
    object
      [ "range" .= bhRange
      , "intervalSeconds" .= bhIntervalSeconds
      , "source" .= bhSource
      , "generatedAt" .= (round bhGeneratedAt :: Integer)
      , "latestPrice" .= fmap show bhLatestPrice
      , "changePct" .= bhChangePct
      , "points" .= bhPoints
      ]

data BasketLatest = BasketLatest
  { blTimestamp :: Integer
  , blBasketPrice :: Integer
  , blComponents :: Value
  , blGeneratedAt :: POSIXTime
  , blSource :: Text
  }
  deriving stock (Show, Generic)

instance ToJSON BasketLatest where
  toJSON BasketLatest {..} =
    object
      [ "timestamp" .= blTimestamp
      , "basketPrice" .= show blBasketPrice
      , "components" .= blComponents
      , "generatedAt" .= (round blGeneratedAt :: Integer)
      , "source" .= blSource
      ]

data PythUpdateResponse = PythUpdateResponse
  { purUpdateData :: [Text]
  , purFetchedAt :: Integer
  , purPublishTimes :: [Integer]
  , purSource :: Text
  }
  deriving stock (Show, Generic)

instance ToJSON PythUpdateResponse where
  toJSON PythUpdateResponse {..} =
    object
      [ "updateData" .= purUpdateData
      , "fetchedAt" .= purFetchedAt
      , "publishTimes" .= purPublishTimes
      , "source" .= purSource
      ]

data RevealPayloadResponse = RevealPayloadResponse
  { rprOrderId :: Integer
  , rprUpdateData :: [Text]
  , rprFetchedAt :: Integer
  , rprPublishTimes :: [Integer]
  , rprMinPublishTime :: Integer
  , rprMaxPublishTime :: Integer
  , rprSource :: Text
  }
  deriving stock (Show, Generic)

instance ToJSON RevealPayloadResponse where
  toJSON RevealPayloadResponse {..} =
    object
      [ "orderId" .= show rprOrderId
      , "updateData" .= rprUpdateData
      , "fetchedAt" .= rprFetchedAt
      , "publishTimes" .= rprPublishTimes
      , "minPublishTime" .= rprMinPublishTime
      , "maxPublishTime" .= rprMaxPublishTime
      , "source" .= rprSource
      ]
