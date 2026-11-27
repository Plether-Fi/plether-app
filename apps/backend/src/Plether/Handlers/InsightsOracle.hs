{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Plether.Handlers.InsightsOracle
  ( AdmittedBucketAgg (..)
  , RejectionBucketAgg (..)
  , OracleHealthBucket (..)
  , getOracleHealthResponse
  , buildOracleHealthBuckets
  , classifyOracleSliceStatus
  , computeComponentConfidenceBps
  ) where

import Control.Applicative ((<|>))
import Control.Concurrent.Async (Concurrently (..), runConcurrently)
import Control.Exception (SomeAsyncException, SomeException, fromException, throwIO, try)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Scientific (floatingOrInteger)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (defaultTimeLocale, formatTime)
import Data.Time.Clock.POSIX (getPOSIXTime, posixSecondsToUTCTime)
import Database.PostgreSQL.Simple (Connection, Only (..), query, query_)
import Plether.AA.OracleReadiness (decodePolicy)
import Plether.Config (Config (..))
import Plether.Database (DbPool, withDb)
import Plether.Database.Schema (ensureOracleAdmissionEventsSchema)
import Plether.Ethereum.Abi (decodeUint256, encodeBool, encodeCall)
import Plether.Ethereum.Client (CallParams (..), EthClient, ethCall)
import Plether.Ethereum.Contracts.Perps (OrderExecutionPolicy (..))
import qualified Plether.Perps.Manifest as Manifest
import Plether.Pyth.Basket (basketDisplayPriceCap)
import Plether.Pyth.RevealPayload (maxComponentPublishTimeDivergence)
import Plether.Types (ApiError, ApiResponse, mkResponse)
import System.Timeout (timeout)
import Text.Read (readMaybe)

data AdmittedBucketAgg = AdmittedBucketAgg
  { abaTotal :: Integer
  , abaLatest :: Integer
  , abaReveal :: Integer
  , abaMaxSpreadSeconds :: Integer
  , abaMaxLagSeconds :: Integer
  , abaHighSpreadCount :: Integer
  }
  deriving stock (Eq, Show)

data RejectionBucketAgg = RejectionBucketAgg
  { rbaRejected :: Integer
  , rbaStale :: Integer
  }
  deriving stock (Eq, Show)

data OracleHealthBucket = OracleHealthBucket
  { ohbStartUnix :: Integer
  , ohbEndUnix :: Integer
  , ohbStatus :: Text
  , ohbAdmittedCount :: Integer
  , ohbLatestCount :: Integer
  , ohbRevealCount :: Integer
  , ohbRejectedCount :: Integer
  , ohbStaleCount :: Integer
  , ohbHighSpreadCount :: Integer
  , ohbMaxSpreadSeconds :: Maybe Integer
  , ohbMaxLagSeconds :: Maybe Integer
  }
  deriving stock (Eq, Show)

data LiveOracleOnChain = LiveOracleOnChain
  { looMaxConfidenceRatioBps :: Maybe Integer
  , looAdverseConfMultiplierBps :: Maybe Integer
  , looOrderStalenessLimit :: Maybe Integer
  , looLiqStalenessLimit :: Maybe Integer
  , looOpenPolicy :: Maybe OrderExecutionPolicy
  , looClosePolicy :: Maybe OrderExecutionPolicy
  , looLastMarkPrice :: Maybe Integer
  , looLastMarkTime :: Maybe Integer
  , looOracleFrozen :: Maybe Bool
  , looFadWindow :: Maybe Bool
  , looTradingActive :: Maybe Bool
  }

data ParsedComponent = ParsedComponent
  { pcSymbol :: Text
  , pcFeedSymbol :: Text
  , pcFeedId :: Text
  , pcPrice :: Text
  , pcRawPrice :: Text
  , pcConfidence :: Text
  , pcConfidenceBps :: Maybe Int
  , pcExponent :: Maybe Int
  , pcPublishTime :: Maybe Integer
  , pcWeightBps :: Maybe Integer
  , pcInverted :: Bool
  }

getOracleHealthResponse
  :: DbPool
  -> EthClient
  -> Config
  -> IO (Either ApiError (ApiResponse Value))
getOracleHealthResponse pool client cfg = do
  nowSec <- floor <$> getPOSIXTime
  let chainId = cfgPerpsChainId cfg
      bucket1hSec = 120 :: Integer
      bucket1hCount = 30 :: Int
      end1h = ((nowSec `div` bucket1hSec) + 1) * bucket1hSec
      start1h = end1h - fromIntegral bucket1hCount * bucket1hSec
      bucket24hSec = 1800 :: Integer
      bucket24hCount = 48 :: Int
      end24h = ((nowSec `div` bucket24hSec) + 1) * bucket24hSec
      start24h = end24h - fromIntegral bucket24hCount * bucket24hSec
      since24h = nowSec - 86400

  (onChain, dbRes) <-
    runConcurrently $
      (,)
        <$> Concurrently (fetchLiveOracleOnChain client)
        <*> Concurrently
          ( withDb pool $ \conn -> do
              _ <- observeIo (ensureOracleAdmissionEventsSchema conn)
              latestPayload <- fetchLatestPayload conn
              admitted1h <- fetchAdmittedBuckets conn start1h end1h bucket1hSec
              admitted24h <- fetchAdmittedBuckets conn start24h end24h bucket24hSec
              rejected1h <- fetchRejectionBuckets conn start1h end1h bucket1hSec
              rejected24h <- fetchRejectionBuckets conn start24h end24h bucket24hSec
              breakdown <- fetchRejectionBreakdown conn since24h
              recentRejections <- fetchRecentRejectionEvents conn since24h
              pure (latestPayload, admitted1h, admitted24h, rejected1h, rejected24h, breakdown, recentRejections)
          )

  let (mLatest, admitted1h, admitted24h, rejected1h, rejected24h, breakdown, recentRejections) = dbRes
      buckets1h = buildOracleHealthBuckets start1h bucket1hSec bucket1hCount admitted1h rejected1h
      buckets24h = buildOracleHealthBuckets start24h bucket24hSec bucket24hCount admitted24h rejected24h
      (mMinPub, mMaxPub, mFetchedAt, mSource, mBasketPrice, components) =
        case mLatest of
          Just (mn, mx, ft, src, bp, compsVal) ->
            (Just mn, Just mx, Just ft, Just src, bp, maybe [] parseComponentList compsVal)
          Nothing -> (Nothing, Nothing, Nothing, Nothing, Nothing, [])
      mCurrentSpread = (\mx mn -> max 0 (mx - mn)) <$> mMaxPub <*> mMinPub
      mCurrentAge = (\mx -> max 0 (nowSec - mx)) <$> mMaxPub
      mMaxConfBps =
        case mapMaybe pcConfidenceBps components of
          [] -> Nothing
          xs -> Just (maximum xs)
      reasonCountMap =
        Map.fromListWith (+) [(code, cnt) | (code, outcome, cnt, _) <- breakdown, outcome == "rejected"]
      admitted24hTotal = sum $ map ohbAdmittedCount buckets24h
      latest24hTotal = sum $ map ohbLatestCount buckets24h
      reveal24hTotal = sum $ map ohbRevealCount buckets24h
      rejected24hTotal = sum $ map ohbRejectedCount buckets24h
      stale24hTotal = sum $ map ohbStaleCount buckets24h
      maxSpread24h = maximumMaybe $ mapMaybe ohbMaxSpreadSeconds buckets24h
      maxLag24h = maximumMaybe $ mapMaybe ohbMaxLagSeconds buckets24h
      totalChecks24h = admitted24hTotal + rejected24hTotal
      admissionRateBps =
        if totalChecks24h > 0
          then Just (fromInteger ((admitted24hTotal * 10000 + totalChecks24h `div` 2) `div` totalChecks24h) :: Int)
          else Nothing
      stalenessLimit = fromMaybe 15 (looOrderStalenessLimit onChain)
      confLimit = looMaxConfidenceRatioBps onChain
      regimeTxt
        | looOracleFrozen onChain == Just True = "Frozen"
        | looFadWindow onChain == Just True = "FAD"
        | looTradingActive onChain == Just True = "Live"
        | otherwise = "Unknown"
      overallStatus :: Text
      overallStatus
        | looOracleFrozen onChain == Just True = "frozen"
        | rejected24hTotal > 0 && admitted24hTotal == 0 = "degraded"
        | maybe True (> stalenessLimit) mCurrentAge = "stale"
        | maybe False (> maxComponentPublishTimeDivergence) mCurrentSpread = "degraded"
        | otherwise = "healthy"
      checks =
        buildAdmissionChecks
          components
          mCurrentSpread
          mCurrentAge
          mMaxConfBps
          mBasketPrice
          stalenessLimit
          confLimit
          regimeTxt
          reasonCountMap

  pure $
    Right $
      mkResponse 0 chainId $
        object
          [ "network"
              .= object
                [ "chainId" .= chainId
                , "releaseVersion" .= Manifest.releaseVersion
                , "settlementAssetSymbol" .= ("USDG" :: Text)
                ]
          , "observedAt" .= isoTimestamp nowSec
          , "overallStatus" .= overallStatus
          , "regime" .= regimeTxt
          , "live"
              .= object
                [ "basketPrice" .= (showT <$> (mBasketPrice <|> looLastMarkPrice onChain))
                , "onChainMarkPrice" .= (showT <$> looLastMarkPrice onChain)
                , "onChainMarkTime" .= (isoTimestamp <$> looLastMarkTime onChain)
                , "latestPublishTime" .= (isoTimestamp <$> mMaxPub)
                , "minPublishTime" .= (isoTimestamp <$> mMinPub)
                , "fetchedAt" .= (isoTimestamp <$> mFetchedAt)
                , "source" .= mSource
                , "ageSeconds" .= mCurrentAge
                , "spreadSeconds" .= mCurrentSpread
                , "maxComponentConfidenceBps" .= mMaxConfBps
                , "oracleFrozen" .= looOracleFrozen onChain
                , "fadWindow" .= looFadWindow onChain
                , "tradingActive" .= looTradingActive onChain
                , "openPolicy" .= (policyToJson <$> looOpenPolicy onChain)
                , "closePolicy" .= (policyToJson <$> looClosePolicy onChain)
                ]
          , "thresholds"
              .= object
                [ "maxComponentDivergenceSeconds" .= maxComponentPublishTimeDivergence
                , "orderStalenessLimitSeconds" .= looOrderStalenessLimit onChain
                , "liquidationStalenessLimitSeconds" .= looLiqStalenessLimit onChain
                , "maxConfidenceRatioBps" .= looMaxConfidenceRatioBps onChain
                , "adverseConfidenceMultiplierBps" .= looAdverseConfMultiplierBps onChain
                , "displayPriceCap" .= showT basketDisplayPriceCap
                ]
          , "summary24h"
              .= object
                [ "admittedUpdates" .= admitted24hTotal
                , "latestAdmittedUpdates" .= latest24hTotal
                , "revealAdmittedUpdates" .= reveal24hTotal
                , "rejectedUpdates" .= rejected24hTotal
                , "staleMarketClosedPolls" .= stale24hTotal
                , "admissionRateBps" .= admissionRateBps
                , "maxSpreadSeconds" .= maxSpread24h
                , "maxLagSeconds" .= maxLag24h
                ]
          , "windows"
              .= object
                [ "window1h"
                    .= object
                      [ "windowSeconds" .= (3600 :: Integer)
                      , "bucketSeconds" .= bucket1hSec
                      , "timeline" .= map bucketToJson buckets1h
                      ]
                , "window24h"
                    .= object
                      [ "windowSeconds" .= (86400 :: Integer)
                      , "bucketSeconds" .= bucket24hSec
                      , "timeline" .= map bucketToJson buckets24h
                      ]
                ]
          , "admissionChecks" .= checks
          , "components" .= map (componentToJson mMinPub confLimit) components
          , "rejectionBreakdown"
              .= [ object
                    [ "reasonCode" .= code
                    , "outcome" .= outcome
                    , "count24h" .= cnt
                    , "lastOccurredAt" .= isoTimestamp lastTs
                    ]
                 | (code, outcome, cnt, lastTs) <- breakdown
                 ]
          , "recentRejections"
              .= [ object
                    [ "id" .= showT evId
                    , "occurredAt" .= isoTimestamp ts
                    , "route" .= route
                    , "outcome" .= outcome
                    , "reasonCode" .= code
                    , "minPublishTime" .= (isoTimestamp <$> mn)
                    , "maxPublishTime" .= (isoTimestamp <$> mx)
                    , "spreadSeconds" .= sp
                    , "ageSeconds" .= ag
                    , "detail" .= detail
                    ]
                 | (evId, ts, route, outcome, code, mn, mx, sp, ag, detail) <- recentRejections
                 ]
          ]

buildOracleHealthBuckets
  :: Integer
  -> Integer
  -> Int
  -> Map.Map Integer AdmittedBucketAgg
  -> Map.Map Integer RejectionBucketAgg
  -> [OracleHealthBucket]
buildOracleHealthBuckets startUnix bucketSec bucketCount admittedMap rejectedMap =
  [ let idx = toInteger i
        bStart = startUnix + idx * bucketSec
        bEnd = bStart + bucketSec
        mAdm = Map.lookup idx admittedMap
        mRej = Map.lookup idx rejectedMap
        admitted = maybe 0 abaTotal mAdm
        latestCnt = maybe 0 abaLatest mAdm
        revealCnt = maybe 0 abaReveal mAdm
        highSpread = maybe 0 abaHighSpreadCount mAdm
        rejected = maybe 0 rbaRejected mRej
        stale = maybe 0 rbaStale mRej
        st = classifyOracleSliceStatus admitted rejected stale highSpread
     in OracleHealthBucket
          { ohbStartUnix = bStart
          , ohbEndUnix = bEnd
          , ohbStatus = st
          , ohbAdmittedCount = admitted
          , ohbLatestCount = latestCnt
          , ohbRevealCount = revealCnt
          , ohbRejectedCount = rejected
          , ohbStaleCount = stale
          , ohbHighSpreadCount = highSpread
          , ohbMaxSpreadSeconds = abaMaxSpreadSeconds <$> mAdm
          , ohbMaxLagSeconds = abaMaxLagSeconds <$> mAdm
          }
  | i <- [0 .. bucketCount - 1]
  ]

classifyOracleSliceStatus :: Integer -> Integer -> Integer -> Integer -> Text
classifyOracleSliceStatus admitted rejected stale highSpread
  | rejected > 0 = "rejected"
  | stale > 0 && admitted == 0 = "stale"
  | highSpread > 0 = "degraded"
  | admitted > 0 = "healthy"
  | otherwise = "idle"

computeComponentConfidenceBps :: Integer -> Integer -> Maybe Int
computeComponentConfidenceBps rawPrice conf
  | rawPrice <= 0 || conf < 0 = Nothing
  | otherwise = Just $ fromInteger ((conf * 10000 + rawPrice `div` 2) `div` rawPrice)

buildAdmissionChecks
  :: [ParsedComponent]
  -> Maybe Integer
  -> Maybe Integer
  -> Maybe Int
  -> Maybe Integer
  -> Integer
  -> Maybe Integer
  -> Text
  -> Map.Map Text Integer
  -> [Value]
buildAdmissionChecks components mSpread mAge mMaxConf mBasketPrice stalenessLimit mConfLimit regimeTxt rejCounts =
  let rejFor keys = sum [Map.findWithDefault 0 k rejCounts | k <- keys]
      sixFeedRej = rejFor ["onchain_pyth_rejected", "metadata_mismatch", "hermes_fetch_error", "hermes_rate_limited"]
      divRej = rejFor ["publish_time_divergence", "future_skew"]
      staleRej = rejFor ["stale_price", "reveal_window_invalid"]
      confRej = rejFor ["confidence_too_wide"]
      domainRej = rejFor ["price_domain_invalid"]

      sixFeedStatus :: Text
      sixFeedStatus
        | length components == 6 = "passed"
        | null components = "unknown"
        | otherwise = "failed"

      divStatus :: Text
      divStatus = case mSpread of
        Nothing -> "unknown"
        Just s
          | s > maxComponentPublishTimeDivergence -> "failed"
          | s >= maxComponentPublishTimeDivergence - 1 -> "warning"
          | otherwise -> "passed"

      staleStatus :: Text
      staleStatus = case mAge of
        Nothing -> "unknown"
        Just a
          | a > stalenessLimit -> "failed"
          | a * 4 >= stalenessLimit * 3 -> "warning"
          | otherwise -> "passed"

      confStatus :: Text
      confStatus = case (mMaxConf, mConfLimit) of
        (Just c, Just lim)
          | toInteger c > lim -> "failed"
          | toInteger c * 4 >= lim * 3 -> "warning"
          | otherwise -> "passed"
        (Just _, Nothing) -> "passed"
        _ -> "unknown"

      domainStatus :: Text
      domainStatus = case mBasketPrice of
        Nothing -> "unknown"
        Just bp
          | bp > 0 && bp < basketDisplayPriceCap -> "passed"
          | otherwise -> "failed"

      regimeStatus :: Text
      regimeStatus
        | regimeTxt == "Live" = "passed"
        | regimeTxt == "FAD" = "warning"
        | regimeTxt == "Frozen" = "failed"
        | otherwise = "unknown"
   in [ checkObj
          "six_feed_signature"
          "6-feed Pyth signature & completeness"
          sixFeedStatus
          "6/6 FX feeds verified"
          (showT (length components) <> "/6 feeds")
          sixFeedRej
          "All 6 DXY FX feeds cryptographically verified via parsePythUpdateData"
      , checkObj
          "publish_time_divergence"
          "Component publish-time divergence"
          divStatus
          ("≤ " <> showT maxComponentPublishTimeDivergence <> "s spread")
          (maybe "—" (\s -> showT s <> "s") mSpread)
          divRej
          "Rejects updates where max(publishTime) - min(publishTime) exceeds PletherOracle__PublishTimeDivergence"
      , checkObj
          "staleness_bound"
          "Freshness / staleness guard"
          staleStatus
          ("≤ " <> showT stalenessLimit <> "s age")
          (maybe "—" (\a -> showT a <> "s") mAge)
          staleRej
          "Rejects updates older than orderExecutionStalenessLimit (PletherOracle__StalePrice)"
      , checkObj
          "confidence_ratio"
          "Basket confidence ratio"
          confStatus
          (maybe "On-chain limit" (\l -> "≤ " <> showT l <> " bps") mConfLimit)
          (maybe "—" (\c -> showT c <> " bps") mMaxConf)
          confRej
          "Rejects wide Pyth FX uncertainty intervals (PletherOracle__BasketConfidenceTooWide)"
      , checkObj
          "display_domain"
          "Display-domain & positivity bounds"
          domainStatus
          "0 < raw < 2.00"
          (maybe "—" (\bp -> if bp > 0 && bp < basketDisplayPriceCap then "Valid" else "Out of bounds") mBasketPrice)
          domainRej
          "Rejects non-positive component prices or baskets outside the v1 display cap"
      , checkObj
          "oracle_regime"
          "On-chain oracle execution regime"
          regimeStatus
          "Live (unfrozen)"
          regimeTxt
          0
          "PletherOracle.getOrderExecutionPolicy(open/close) and PerpsPublicLens status"
      ]
  where
    checkObj :: Text -> Text -> Text -> Text -> Text -> Integer -> Text -> Value
    checkObj key label st threshold current rej24h desc =
      object
        [ "key" .= key
        , "label" .= label
        , "status" .= st
        , "threshold" .= threshold
        , "currentValue" .= current
        , "rejections24h" .= rej24h
        , "description" .= desc
        ]

fetchLiveOracleOnChain :: EthClient -> IO LiveOracleOnChain
fetchLiveOracleOnChain client = do
  let oracle = Manifest.pletherOracleAddress
      publicLens = Manifest.perpsPublicLensAddress
      call target sig args = do
        res <- timeout 2500000 $ observeIo $ ethCall client (CallParams target (encodeCall sig args))
        pure $ case res of
          Just (Just (Right bytes)) -> Just bytes
          _ -> Nothing
      readUint target sig = do
        mBytes <- call target sig []
        pure $ case mBytes of
          Just b | BS.length b == 32 -> Just (decodeUint256 b)
          _ -> Nothing

  (maxConf, advMult, ordStale, liqStale, openPolRaw, closePolRaw, statusRaw) <-
    runConcurrently $
      (,,,,,,)
        <$> Concurrently (readUint oracle "basketMaxConfidenceRatioBps()")
        <*> Concurrently (readUint oracle "adverseConfidenceMultiplierBps()")
        <*> Concurrently (readUint oracle "orderExecutionStalenessLimit()")
        <*> Concurrently (readUint oracle "liquidationStalenessLimit()")
        <*> Concurrently (call oracle "getOrderExecutionPolicy(bool)" [encodeBool False])
        <*> Concurrently (call oracle "getOrderExecutionPolicy(bool)" [encodeBool True])
        <*> Concurrently (call publicLens "getProtocolStatus()" [])

  let (mMarkPrice, mMarkTime, mFrozen, mFad, mTrading) =
        case statusRaw of
          Just b | BS.length b >= 256 ->
            let wordAt n = decodeUint256 (BS.take 32 (BS.drop (n * 32) b))
                toBool w = if w == 0 then Just False else if w == 1 then Just True else Nothing
             in (Just (wordAt 1), Just (wordAt 2), toBool (wordAt 3), toBool (wordAt 4), toBool (wordAt 5))
          _ -> (Nothing, Nothing, Nothing, Nothing, Nothing)

  pure
    LiveOracleOnChain
      { looMaxConfidenceRatioBps = maxConf
      , looAdverseConfMultiplierBps = advMult
      , looOrderStalenessLimit = ordStale
      , looLiqStalenessLimit = liqStale
      , looOpenPolicy = openPolRaw >>= decodePolicy
      , looClosePolicy = closePolRaw >>= decodePolicy
      , looLastMarkPrice = mMarkPrice
      , looLastMarkTime = mMarkTime
      , looOracleFrozen = mFrozen
      , looFadWindow = mFad
      , looTradingActive = mTrading
      }

fetchLatestPayload :: Connection -> IO (Maybe (Integer, Integer, Integer, Text, Maybe Integer, Maybe Value))
fetchLatestPayload conn = do
  mRows <-
    observeIo $
      query_
        conn
        "SELECT min_publish_time, max_publish_time, fetched_at, source, basket_price, component_prices \
        \FROM perps_pyth_update_payloads \
        \WHERE source = 'backend_hermes_latest_v2' AND basket_price IS NOT NULL \
        \ORDER BY max_publish_time DESC LIMIT 1"
        :: IO (Maybe [(Integer, Integer, Integer, Text, Maybe Integer, Maybe Value)])
  pure (mRows >>= listToMaybe)

fetchAdmittedBuckets :: Connection -> Integer -> Integer -> Integer -> IO (Map.Map Integer AdmittedBucketAgg)
fetchAdmittedBuckets conn startUnix endUnix bucketSec = do
  mRows <-
    observeIo $
      query
        conn
        "SELECT \
        \  FLOOR((fetched_at - ?) / ?)::BIGINT, \
        \  COUNT(*)::BIGINT, \
        \  COUNT(*) FILTER (WHERE source = 'backend_hermes_latest_v2')::BIGINT, \
        \  COUNT(*) FILTER (WHERE source <> 'backend_hermes_latest_v2')::BIGINT, \
        \  COALESCE(MAX(GREATEST(0, max_publish_time - min_publish_time)), 0)::BIGINT, \
        \  COALESCE(MAX(GREATEST(0, fetched_at - min_publish_time)), 0)::BIGINT, \
        \  COUNT(*) FILTER (WHERE (max_publish_time - min_publish_time) >= 4)::BIGINT \
        \FROM perps_pyth_update_payloads \
        \WHERE fetched_at >= ? AND fetched_at < ? \
        \GROUP BY 1 ORDER BY 1 ASC"
        (startUnix, bucketSec, startUnix, endUnix)
        :: IO (Maybe [(Integer, Integer, Integer, Integer, Integer, Integer, Integer)])
  pure $
    Map.fromList
      [ (idx, AdmittedBucketAgg tot lat rev mxSp mxLg hiSp)
      | (idx, tot, lat, rev, mxSp, mxLg, hiSp) <- fromMaybe [] mRows
      ]

fetchRejectionBuckets :: Connection -> Integer -> Integer -> Integer -> IO (Map.Map Integer RejectionBucketAgg)
fetchRejectionBuckets conn startUnix endUnix bucketSec = do
  mRows <-
    observeIo $
      query
        conn
        "SELECT \
        \  FLOOR((occurred_at - ?) / ?)::BIGINT, \
        \  COUNT(*) FILTER (WHERE outcome = 'rejected')::BIGINT, \
        \  COUNT(*) FILTER (WHERE outcome = 'stale_market_closed')::BIGINT \
        \FROM perps_oracle_admission_events \
        \WHERE occurred_at >= ? AND occurred_at < ? \
        \GROUP BY 1 ORDER BY 1 ASC"
        (startUnix, bucketSec, startUnix, endUnix)
        :: IO (Maybe [(Integer, Integer, Integer)])
  pure $
    Map.fromList
      [ (idx, RejectionBucketAgg rej st)
      | (idx, rej, st) <- fromMaybe [] mRows
      ]

fetchRejectionBreakdown :: Connection -> Integer -> IO [(Text, Text, Integer, Integer)]
fetchRejectionBreakdown conn sinceUnix = do
  mRows <-
    observeIo $
      query
        conn
        "SELECT reason_code, outcome, COUNT(*)::BIGINT, MAX(occurred_at)::BIGINT \
        \FROM perps_oracle_admission_events \
        \WHERE occurred_at >= ? \
        \GROUP BY reason_code, outcome \
        \ORDER BY COUNT(*) DESC, MAX(occurred_at) DESC \
        \LIMIT 20"
        (Only sinceUnix)
        :: IO (Maybe [(Text, Text, Integer, Integer)])
  pure (fromMaybe [] mRows)

fetchRecentRejectionEvents
  :: Connection
  -> Integer
  -> IO [(Integer, Integer, Text, Text, Text, Maybe Integer, Maybe Integer, Maybe Int, Maybe Int, Text)]
fetchRecentRejectionEvents conn sinceUnix = do
  mRows <-
    observeIo $
      query
        conn
        "SELECT id, occurred_at, route, outcome, reason_code, \
        \  min_publish_time, max_publish_time, spread_seconds, age_seconds, detail \
        \FROM perps_oracle_admission_events \
        \WHERE occurred_at >= ? \
        \ORDER BY occurred_at DESC, id DESC \
        \LIMIT 30"
        (Only sinceUnix)
        :: IO (Maybe [(Integer, Integer, Text, Text, Text, Maybe Integer, Maybe Integer, Maybe Int, Maybe Int, Text)])
  pure (fromMaybe [] mRows)

parseComponentList :: Value -> [ParsedComponent]
parseComponentList (Array arr) = mapMaybe parseOne (toList arr)
  where
    parseOne (Object obj) = do
      sym <- lookupText "symbol" obj
      feedSym <- lookupText "feedSymbol" obj
      feedId <- lookupText "feedId" obj
      priceTxt <- lookupStringOrInt "price" obj
      rawTxt <- lookupStringOrInt "rawPrice" obj
      confTxt <- lookupStringOrInt "confidence" obj
      let mRawInt = readMaybe (T.unpack rawTxt) :: Maybe Integer
          mConfInt = readMaybe (T.unpack confTxt) :: Maybe Integer
          confBps = case (mRawInt, mConfInt) of
            (Just r, Just c) -> computeComponentConfidenceBps r c
            _ -> Nothing
      pure
        ParsedComponent
          { pcSymbol = sym
          , pcFeedSymbol = feedSym
          , pcFeedId = feedId
          , pcPrice = priceTxt
          , pcRawPrice = rawTxt
          , pcConfidence = confTxt
          , pcConfidenceBps = confBps
          , pcExponent = fromInteger <$> lookupInteger "exponent" obj
          , pcPublishTime = lookupInteger "publishTime" obj
          , pcWeightBps = lookupInteger "weightBps" obj
          , pcInverted = fromMaybe False (lookupBool "inverted" obj)
          }
    parseOne _ = Nothing
parseComponentList _ = []

componentToJson :: Maybe Integer -> Maybe Integer -> ParsedComponent -> Value
componentToJson mMinPub mConfLimit ParsedComponent {..} =
  let mDrift = (\pt mn -> max 0 (pt - mn)) <$> pcPublishTime <*> mMinPub
      confOver = case (pcConfidenceBps, mConfLimit) of
        (Just c, Just lim) -> toInteger c > lim
        _ -> False
   in object
        [ "symbol" .= pcSymbol
        , "feedSymbol" .= pcFeedSymbol
        , "feedId" .= pcFeedId
        , "price" .= pcPrice
        , "rawPrice" .= pcRawPrice
        , "confidence" .= pcConfidence
        , "confidenceBps" .= pcConfidenceBps
        , "confidenceExceeded" .= confOver
        , "exponent" .= pcExponent
        , "publishTime" .= (isoTimestamp <$> pcPublishTime)
        , "driftFromMinSeconds" .= mDrift
        , "weightBps" .= pcWeightBps
        , "inverted" .= pcInverted
        ]

bucketToJson :: OracleHealthBucket -> Value
bucketToJson OracleHealthBucket {..} =
  object
    [ "bucketStart" .= isoTimestamp ohbStartUnix
    , "bucketEnd" .= isoTimestamp ohbEndUnix
    , "status" .= ohbStatus
    , "admittedCount" .= ohbAdmittedCount
    , "latestCount" .= ohbLatestCount
    , "revealCount" .= ohbRevealCount
    , "rejectedCount" .= ohbRejectedCount
    , "staleCount" .= ohbStaleCount
    , "highSpreadCount" .= ohbHighSpreadCount
    , "maxSpreadSeconds" .= ohbMaxSpreadSeconds
    , "maxLagSeconds" .= ohbMaxLagSeconds
    ]

policyToJson :: OrderExecutionPolicy -> Value
policyToJson OrderExecutionPolicy {..} =
  object
    [ "closeOnly" .= oepCloseOnly
    , "oracleFrozen" .= oepOracleFrozen
    , "isFadWindow" .= oepIsFadWindow
    , "maxStalenessSeconds" .= oepMaxStaleness
    , "requireStoredMark" .= oepRequireStoredMark
    , "allowAnyStoredMark" .= oepAllowAnyStoredMark
    ]

maximumMaybe :: (Ord a) => [a] -> Maybe a
maximumMaybe [] = Nothing
maximumMaybe xs = Just (maximum xs)

observeIo :: IO a -> IO (Maybe a)
observeIo action = do
  res <- try action
  case res of
    Right val -> pure (Just val)
    Left (err :: SomeException) ->
      case fromException err :: Maybe SomeAsyncException of
        Just _ -> throwIO err
        Nothing -> pure Nothing

lookupText :: Text -> KM.KeyMap Value -> Maybe Text
lookupText k obj = case KM.lookup (Key.fromText k) obj of
  Just (String s) | not (T.null (T.strip s)) -> Just (T.strip s)
  _ -> Nothing

lookupBool :: Text -> KM.KeyMap Value -> Maybe Bool
lookupBool k obj = case KM.lookup (Key.fromText k) obj of
  Just (Bool b) -> Just b
  _ -> Nothing

lookupInteger :: Text -> KM.KeyMap Value -> Maybe Integer
lookupInteger k obj = case KM.lookup (Key.fromText k) obj of
  Just (Number n) ->
    case floatingOrInteger n :: Either Double Integer of
      Right i -> Just i
      Left _ -> Nothing
  Just (String s) -> readMaybe (T.unpack (T.strip s))
  _ -> Nothing

lookupStringOrInt :: Text -> KM.KeyMap Value -> Maybe Text
lookupStringOrInt k obj = case KM.lookup (Key.fromText k) obj of
  Just (String s) | not (T.null (T.strip s)) -> Just (T.strip s)
  Just (Number n) ->
    case floatingOrInteger n :: Either Double Integer of
      Right i -> Just (showT i)
      Left d -> Just (T.pack (show d))
  _ -> Nothing

showT :: (Show a) => a -> Text
showT = T.pack . show

isoTimestamp :: Integer -> Text
isoTimestamp = T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" . posixSecondsToUTCTime . fromIntegral
