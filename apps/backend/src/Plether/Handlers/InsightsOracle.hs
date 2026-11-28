{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Plether.Handlers.InsightsOracle
  ( AdmittedBucketAgg (..)
  , RejectionBucketAgg (..)
  , OracleHealthBucket (..)
  , RecentUpdateRow (..)
  , PayloadDetailRow (..)
  , SettledOrderRow (..)
  , getOracleHealthResponse
  , getOracleUpdateDetailResponse
  , fetchLatestPayload
  , fetchRecentAdmittedUpdates
  , fetchPayloadDetailRow
  , fetchNeighborPayload
  , fetchOrdersSettledWithUpdate
  , fetchAdmittedBucketsPair
  , fetchRejectionBucketsPair
  , fetchRejectionBreakdown
  , fetchRecentRejectionEvents
  , buildOracleHealthBuckets
  , classifyOracleSliceStatus
  , computeComponentConfidenceBps
  , computeComponentContribution
  , computeEffectiveShareBps
  , computeChangeFromBaseBps
  , computeDisplayDeltaBps
  , parseOracleUpdateIdParam
  , settledOrderSideLabel
  ) where

import Control.Applicative ((<|>))
import Control.Concurrent.Async (Concurrently (..), runConcurrently)
import Control.Exception (SomeAsyncException, SomeException, displayException, fromException, throwIO, try)
import Plether.Logging (field, logWarnEvery)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import Data.Either (isRight)
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
import Plether.Cache
  ( SingleFlightCache
  , newConcurrentRefreshingSingleFlightCache
  , newRefreshingSingleFlightCache
  , runSingleFlightCache
  )
import Plether.Config (Config (..))
import Plether.Database (DbPool, withDb)
import Plether.Ethereum.Abi (decodeUint256, encodeBool, encodeCall)
import Plether.Ethereum.Client (EthClient)
import Plether.Ethereum.Contracts.Perps (OrderExecutionPolicy (..))
import qualified Plether.Ethereum.Multicall as Multicall
import qualified Plether.Perps.Manifest as Manifest
import Plether.Pyth.Basket (BasketComponent (..), basketComponents, basketDisplayPriceCap, normalizeFeedId)
import Plether.Pyth.RevealPayload (maxComponentPublishTimeDivergence)
import Plether.Types (ApiError, ApiResponse, mkResponse)
import qualified Plether.Types.Error as E
import System.IO.Unsafe (unsafePerformIO)
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
  , pcBasePrice :: Maybe Integer
  , pcInverted :: Bool
  }

data PayloadDetailRow = PayloadDetailRow
  { pdrId :: Integer
  , pdrMinPublishTime :: Integer
  , pdrMaxPublishTime :: Integer
  , pdrFetchedAt :: Integer
  , pdrSource :: Text
  , pdrBasketPrice :: Maybe Integer
  , pdrComponentPrices :: Maybe Value
  , pdrVaaCount :: Int
  }

data RecentUpdateRow = RecentUpdateRow
  { rurId :: Integer
  , rurMinPublishTime :: Integer
  , rurMaxPublishTime :: Integer
  , rurFetchedAt :: Integer
  , rurSource :: Text
  , rurBasketPrice :: Maybe Integer
  , rurOrdersSettledCount :: Integer
  }

data SettledOrderRow = SettledOrderRow
  { sorOrderId :: Text
  , sorAccount :: Text
  , sorSide :: Maybe Text
  , sorStatus :: Text
  , sorTxHash :: Maybe Text
  , sorExecutionPrice :: Maybe Text
  , sorExecutionOraclePrice :: Maybe Text
  , sorCommitTimestamp :: Maybe Integer
  , sorTerminalTimestamp :: Maybe Integer
  }

{-# NOINLINE oracleHealthCache #-}
oracleHealthCache :: SingleFlightCache () (Either ApiError (ApiResponse Value))
oracleHealthCache =
  unsafePerformIO (newRefreshingSingleFlightCache 1 (15 * 1000000000) (300 * 1000000000))

{-# NOINLINE oracleUpdateDetailCache #-}
oracleUpdateDetailCache :: SingleFlightCache Text (Either ApiError (ApiResponse Value))
oracleUpdateDetailCache =
  unsafePerformIO (newConcurrentRefreshingSingleFlightCache 128 (30 * 1000000000) (600 * 1000000000))

{-# NOINLINE oracleThresholdsCache #-}
oracleThresholdsCache :: SingleFlightCache () LiveOracleOnChain
oracleThresholdsCache =
  unsafePerformIO (newRefreshingSingleFlightCache 1 (300 * 1000000000) (1800 * 1000000000))

getOracleHealthResponse
  :: DbPool
  -> EthClient
  -> Config
  -> IO (Either ApiError (ApiResponse Value))
getOracleHealthResponse pool client cfg =
  snd <$> runSingleFlightCache oracleHealthCache () isRight (computeOracleHealthResponse pool client cfg)

getOracleUpdateDetailResponse
  :: DbPool
  -> EthClient
  -> Config
  -> Text
  -> IO (Either ApiError (ApiResponse Value))
getOracleUpdateDetailResponse pool client cfg rawIdParam =
  let normalizedKey = T.toLower (T.strip rawIdParam)
   in case parseOracleUpdateIdParam normalizedKey of
        Nothing ->
          pure $ Left $ E.invalidAmount "oracle update id must be a positive integer or 'latest'"
        Just mTargetId ->
          snd
            <$> runSingleFlightCache
              oracleUpdateDetailCache
              normalizedKey
              isRight
              (computeOracleUpdateDetailResponse pool client cfg mTargetId)

parseOracleUpdateIdParam :: Text -> Maybe (Maybe Integer)
parseOracleUpdateIdParam raw =
  let cleaned = T.toLower (T.strip raw)
   in if cleaned == "latest"
        then Just Nothing
        else case readMaybe (T.unpack cleaned) :: Maybe Integer of
          Just n | n > 0 -> Just (Just n)
          _ -> Nothing

computeOracleHealthResponse
  :: DbPool
  -> EthClient
  -> Config
  -> IO (Either ApiError (ApiResponse Value))
computeOracleHealthResponse pool client cfg = do
  nowSec <- floor <$> getPOSIXTime
  let chainId = cfgPerpsChainId cfg
      router = T.toLower $ T.strip $ cfgPerpsOrderRouter cfg
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
              latestPayload <- fetchLatestPayload conn
              recentUpdates <- fetchRecentAdmittedUpdates conn chainId router
              (admitted1h, admitted24h) <-
                fetchAdmittedBucketsPair conn start1h end1h bucket1hSec start24h end24h bucket24hSec
              (rejected1h, rejected24h) <-
                fetchRejectionBucketsPair conn start1h end1h bucket1hSec start24h end24h bucket24hSec
              breakdown <- fetchRejectionBreakdown conn since24h
              recentRejections <- fetchRecentRejectionEvents conn since24h
              pure (latestPayload, recentUpdates, admitted1h, admitted24h, rejected1h, rejected24h, breakdown, recentRejections)
          )

  let (mLatest, recentUpdates, admitted1h, admitted24h, rejected1h, rejected24h, breakdown, recentRejections) = dbRes
      buckets1h = buildOracleHealthBuckets start1h bucket1hSec bucket1hCount admitted1h rejected1h
      buckets24h = buildOracleHealthBuckets start24h bucket24hSec bucket24hCount admitted24h rejected24h
      (mLatestId, mMinPub, mMaxPub, mFetchedAt, mSource, mBasketPrice, components) =
        case mLatest of
          Just (pid, mn, mx, ft, src, bp, compsVal) ->
            (Just pid, Just mn, Just mx, Just ft, Just src, bp, maybe [] parseComponentList compsVal)
          Nothing -> (Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, [])
      mCurrentSpread = (\mx mn -> max 0 (mx - mn)) <$> mMaxPub <*> mMinPub
      mCurrentAge = (\mx -> max 0 (nowSec - mx)) <$> mMaxPub
      mMaxConfBps =
        case mapMaybe pcConfidenceBps components of
          [] -> Nothing
          xs -> Just (maximum xs)
      totalContribution = computeTotalContribution components mBasketPrice
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
                [ "latestUpdateId" .= (showT <$> mLatestId)
                , "basketPrice" .= (showT <$> (mBasketPrice <|> looLastMarkPrice onChain))
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
          , "components" .= map (componentToJson mMinPub confLimit totalContribution) components
          , "recentUpdates" .= map recentUpdateToJson recentUpdates
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

computeOracleUpdateDetailResponse
  :: DbPool
  -> EthClient
  -> Config
  -> Maybe Integer
  -> IO (Either ApiError (ApiResponse Value))
computeOracleUpdateDetailResponse pool client cfg mTargetId = do
  nowSec <- floor <$> getPOSIXTime
  let chainId = cfgPerpsChainId cfg
      router = T.toLower $ T.strip $ cfgPerpsOrderRouter cfg

  (onChain, mDetailBundle) <-
    runConcurrently $
      (,)
        <$> Concurrently (getCachedOracleThresholds client)
        <*> Concurrently
          ( withDb pool $ \conn -> do
              mRow <- fetchPayloadDetailRow conn mTargetId
              case mRow of
                Nothing -> pure Nothing
                Just row -> do
                  mPrev <- fetchNeighborPayload conn True (pdrMaxPublishTime row) (pdrId row)
                  mNext <- fetchNeighborPayload conn False (pdrMaxPublishTime row) (pdrId row)
                  orders <-
                    fetchOrdersSettledWithUpdate
                      conn
                      chainId
                      router
                      (pdrMinPublishTime row)
                      (pdrMaxPublishTime row)
                  pure (Just (row, mPrev, mNext, orders))
          )

  case mDetailBundle of
    Nothing ->
      pure $ Left $ E.notFound "Oracle price update not found"
    Just (row, mPrev, mNext, settledOrders) -> do
      let components = maybe [] parseComponentList (pdrComponentPrices row)
          spreadSec = max 0 (pdrMaxPublishTime row - pdrMinPublishTime row)
          lagSec = max 0 (pdrFetchedAt row - pdrMinPublishTime row)
          mMaxConfBps =
            case mapMaybe pcConfidenceBps components of
              [] -> Nothing
              xs -> Just (maximum xs)
          stalenessLimit = fromMaybe 15 (looOrderStalenessLimit onChain)
          confLimit = looMaxConfidenceRatioBps onChain
          contribs = mapMaybe componentContributionMaybe components
          mReconstructed =
            if length contribs == length components && not (null contribs)
              then Just (sum contribs)
              else Nothing
          mReconDiff = (\recon bp -> abs (recon - bp)) <$> mReconstructed <*> pdrBasketPrice row
          totalContribution = computeTotalContribution components (pdrBasketPrice row)
          totalTargetWeightBps = sum (mapMaybe pcWeightBps components)
          totalEffectiveShareBps =
            sum
              [ s
              | comp <- components
              , Just c <- [componentContributionMaybe comp]
              , Just tot <- [totalContribution]
              , Just s <- [computeEffectiveShareBps c tot]
              ]
          mPrevBasketPrice = mPrev >>= \(_, bp, _) -> bp
          mDisplayDeltaBps =
            case (pdrBasketPrice row, mPrevBasketPrice) of
              (Just currBp, Just prevBp) -> computeDisplayDeltaBps currBp prevBp
              _ -> Nothing
          updateStatus :: Text
          updateStatus
            | spreadSec > maxComponentPublishTimeDivergence = "degraded"
            | maybe False (\c -> maybe False (toInteger c >) confLimit) mMaxConfBps = "degraded"
            | spreadSec >= maxComponentPublishTimeDivergence - 1 = "warning"
            | otherwise = "admitted"
          checks =
            buildUpdateDetailChecks
              components
              spreadSec
              lagSec
              mMaxConfBps
              (pdrBasketPrice row)
              mReconDiff
              stalenessLimit
              confLimit

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
              , "update"
                  .= object
                    [ "id" .= showT (pdrId row)
                    , "source" .= pdrSource row
                    , "status" .= updateStatus
                    , "minPublishTime" .= isoTimestamp (pdrMinPublishTime row)
                    , "maxPublishTime" .= isoTimestamp (pdrMaxPublishTime row)
                    , "fetchedAt" .= isoTimestamp (pdrFetchedAt row)
                    , "spreadSeconds" .= spreadSec
                    , "lagSeconds" .= lagSec
                    , "vaaCount" .= pdrVaaCount row
                    , "basketPrice" .= (showT <$> pdrBasketPrice row)
                    , "displayPriceCap" .= showT basketDisplayPriceCap
                    , "reconstructedBasketPrice" .= (showT <$> mReconstructed)
                    , "reconstructionDiffRaw" .= mReconDiff
                    , "previousBasketPrice" .= (showT <$> mPrevBasketPrice)
                    , "displayDeltaBps" .= mDisplayDeltaBps
                    , "maxComponentConfidenceBps" .= mMaxConfBps
                    , "totalTargetWeightBps" .= totalTargetWeightBps
                    , "totalEffectiveShareBps" .= totalEffectiveShareBps
                    ]
              , "navigation"
                  .= object
                    [ "previousId" .= ((\(pid, _, _) -> showT pid) <$> mPrev)
                    , "nextId" .= ((\(nid, _, _) -> showT nid) <$> mNext)
                    , "previousMaxPublishTime" .= ((\(_, _, ts) -> isoTimestamp ts) <$> mPrev)
                    , "nextMaxPublishTime" .= ((\(_, _, ts) -> isoTimestamp ts) <$> mNext)
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
              , "admissionChecks" .= checks
              , "components" .= map (componentToJson (Just (pdrMinPublishTime row)) confLimit totalContribution) components
              , "settledOrders" .= map settledOrderToJson settledOrders
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

-- | Compute a component's raw 8-decimal contribution to the DXY basket index,
-- matching Plether.Pyth.Basket.computeBasketSnapshot:
-- (normalizedPrice * (weightBps * 10^14)) `div` (basePrice * 10^10)
-- = (normalizedPrice * weightBps * 10_000) `div` basePrice
computeComponentContribution :: Integer -> Integer -> Integer -> Maybe Integer
computeComponentContribution normalizedPrice weightBps basePrice
  | normalizedPrice <= 0 || weightBps <= 0 || basePrice <= 0 = Nothing
  | otherwise = Just ((normalizedPrice * weightBps * 10_000) `div` basePrice)

computeEffectiveShareBps :: Integer -> Integer -> Maybe Int
computeEffectiveShareBps contribution totalContribution
  | contribution < 0 || totalContribution <= 0 = Nothing
  | otherwise =
      Just $
        fromInteger ((contribution * 10000 + totalContribution `div` 2) `div` totalContribution)

computeChangeFromBaseBps :: Integer -> Integer -> Maybe Int
computeChangeFromBaseBps normalizedPrice basePrice
  | normalizedPrice <= 0 || basePrice <= 0 = Nothing
  | otherwise =
      let diff = normalizedPrice - basePrice
          mag = (abs diff * 10000 + basePrice `div` 2) `div` basePrice
       in Just $ fromInteger (if diff < 0 then negate mag else mag)

computeDisplayDeltaBps :: Integer -> Integer -> Maybe Int
computeDisplayDeltaBps currRaw prevRaw =
  let currDisp = basketDisplayPriceCap - currRaw
      prevDisp = basketDisplayPriceCap - prevRaw
   in if currDisp <= 0 || prevDisp <= 0
        then Nothing
        else
          let diff = currDisp - prevDisp
              mag = (abs diff * 10000 + prevDisp `div` 2) `div` prevDisp
           in Just $ fromInteger (if diff < 0 then negate mag else mag)

componentContributionMaybe :: ParsedComponent -> Maybe Integer
componentContributionMaybe ParsedComponent {..} = do
  normPrice <- readMaybe (T.unpack pcPrice) :: Maybe Integer
  wBps <- pcWeightBps
  baseP <- pcBasePrice
  computeComponentContribution normPrice wBps baseP

computeTotalContribution :: [ParsedComponent] -> Maybe Integer -> Maybe Integer
computeTotalContribution components mBasketPrice =
  let contribs = mapMaybe componentContributionMaybe components
   in if not (null contribs) && length contribs == length components
        then Just (sum contribs)
        else mBasketPrice

buildUpdateDetailChecks
  :: [ParsedComponent]
  -> Integer
  -> Integer
  -> Maybe Int
  -> Maybe Integer
  -> Maybe Integer
  -> Integer
  -> Maybe Integer
  -> [Value]
buildUpdateDetailChecks components spreadSec lagSec mMaxConf mBasketPrice mReconDiff stalenessLimit mConfLimit =
  let sixFeedStatus :: Text
      sixFeedStatus
        | length components == 6 = "passed"
        | null components = "unknown"
        | otherwise = "failed"

      divStatus :: Text
      divStatus
        | spreadSec > maxComponentPublishTimeDivergence = "failed"
        | spreadSec >= maxComponentPublishTimeDivergence - 1 = "warning"
        | otherwise = "passed"

      lagStatus :: Text
      lagStatus
        | lagSec > stalenessLimit = "warning"
        | lagSec * 4 >= stalenessLimit * 3 = "warning"
        | otherwise = "passed"

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

      reconStatus :: Text
      reconStatus = case mReconDiff of
        Nothing -> "unknown"
        Just d
          | d <= 6 -> "passed"
          | d <= 100 -> "warning"
          | otherwise -> "failed"
   in [ checkObj
          "six_feed_signature"
          "6-feed Pyth signature & completeness"
          sixFeedStatus
          "6/6 FX feeds signed"
          (showT (length components) <> "/6 feeds")
          "All 6 DXY FX feeds verified and persisted together as an atomic basket payload"
      , checkObj
          "publish_time_divergence"
          "Component publish-time divergence"
          divStatus
          ("≤ " <> showT maxComponentPublishTimeDivergence <> "s spread")
          (showT spreadSec <> "s")
          "max(publishTime) - min(publishTime) across the 6 signed FX feeds in this update"
      , checkObj
          "ingestion_lag"
          "Hermes ingestion lag at admission"
          lagStatus
          ("≤ " <> showT stalenessLimit <> "s order bound")
          (showT lagSec <> "s")
          "Elapsed seconds between earliest component publishTime and backend ingestion (fetchedAt)"
      , checkObj
          "confidence_ratio"
          "Peak component confidence ratio"
          confStatus
          (maybe "On-chain limit" (\l -> "≤ " <> showT l <> " bps") mConfLimit)
          (maybe "—" (\c -> showT c <> " bps") mMaxConf)
          "Largest Pyth confidence-to-price ratio among the 6 FX components in this snapshot"
      , checkObj
          "display_domain"
          "Display-domain & positivity bounds"
          domainStatus
          "0 < raw < 2.00"
          (maybe "—" (\bp -> if bp > 0 && bp < basketDisplayPriceCap then "Valid" else "Out of bounds") mBasketPrice)
          "Verifies raw basket index is strictly within the immutable v1 display transform domain"
      , checkObj
          "basket_reconstruction"
          "Weighted sum reconstruction"
          reconStatus
          "Σ contributions = raw basket"
          (maybe "—" (\d -> if d == 0 then "Exact match" else "±" <> showT d <> " raw units") mReconDiff)
          "Recomputes Σ (price × weight / basePrice) across all 6 components against stored basketPrice"
      ]
  where
    checkObj :: Text -> Text -> Text -> Text -> Text -> Text -> Value
    checkObj key label st threshold current desc =
      object
        [ "key" .= key
        , "label" .= label
        , "status" .= st
        , "threshold" .= threshold
        , "currentValue" .= current
        , "rejections24h" .= (0 :: Integer)
        , "description" .= desc
        ]

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

getCachedOracleThresholds :: EthClient -> IO LiveOracleOnChain
getCachedOracleThresholds client =
  snd
    <$> runSingleFlightCache
      oracleThresholdsCache
      ()
      (\s -> maybe False (const True) (looOrderStalenessLimit s))
      (fetchLiveOracleOnChain client)

-- | Batch all 7 on-chain oracle reads into a single atomic Multicall3 call.
fetchLiveOracleOnChain :: EthClient -> IO LiveOracleOnChain
fetchLiveOracleOnChain client = do
  let oracle = Manifest.pletherOracleAddress
      publicLens = Manifest.perpsPublicLensAddress
      mkCall target sig args =
        Multicall.Call
          { Multicall.callTarget = target
          , Multicall.callAllowFailure = True
          , Multicall.callCalldata = encodeCall sig args
          }
      calls =
        [ mkCall oracle "basketMaxConfidenceRatioBps()" []
        , mkCall oracle "adverseConfidenceMultiplierBps()" []
        , mkCall oracle "orderExecutionStalenessLimit()" []
        , mkCall oracle "liquidationStalenessLimit()" []
        , mkCall oracle "getOrderExecutionPolicy(bool)" [encodeBool False]
        , mkCall oracle "getOrderExecutionPolicy(bool)" [encodeBool True]
        , mkCall publicLens "getProtocolStatus()" []
        ]
      decodeBytes r =
        if Multicall.resultSuccess r then Just (Multicall.resultData r) else Nothing
      decodeU r =
        case decodeBytes r of
          Just b | BS.length b == 32 -> Just (decodeUint256 b)
          _ -> Nothing

  mRes <- timeout 2500000 $ observeIo $ Multicall.multicall client calls
  let (maxConf, advMult, ordStale, liqStale, openPolRaw, closePolRaw, statusRaw) =
        case mRes of
          Just (Just (Right [r0, r1, r2, r3, r4, r5, r6])) ->
            ( decodeU r0
            , decodeU r1
            , decodeU r2
            , decodeU r3
            , decodeBytes r4
            , decodeBytes r5
            , decodeBytes r6
            )
          _ -> (Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)

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

fetchLatestPayload :: Connection -> IO (Maybe (Integer, Integer, Integer, Integer, Text, Maybe Integer, Maybe Value))
fetchLatestPayload conn = do
  mRows <-
    observeIo $
      query_
        conn
        "SELECT id, min_publish_time, max_publish_time, fetched_at, source, basket_price, component_prices \
        \FROM perps_pyth_update_payloads \
        \WHERE source = 'backend_hermes_latest_v2' AND basket_price IS NOT NULL \
        \ORDER BY max_publish_time DESC, id DESC LIMIT 1"
        :: IO (Maybe [(Integer, Integer, Integer, Integer, Text, Maybe Integer, Maybe Value)])
  pure (mRows >>= listToMaybe)

fetchRecentAdmittedUpdates :: Connection -> Integer -> Text -> IO [RecentUpdateRow]
fetchRecentAdmittedUpdates conn chainId router = do
  mRows <-
    observeIo $
      query
        conn
        "SELECT p.id, p.min_publish_time, p.max_publish_time, p.fetched_at, p.source, p.basket_price, \
        \  (SELECT COUNT(*)::BIGINT FROM perps_orders o \
        \   WHERE o.chain_id = ? AND o.order_router = ? \
        \     AND o.oracle_min_publish_time = p.min_publish_time \
        \     AND o.oracle_max_publish_time = p.max_publish_time) AS orders_settled \
        \FROM perps_pyth_update_payloads p \
        \WHERE p.basket_price IS NOT NULL \
        \ORDER BY p.max_publish_time DESC, p.id DESC \
        \LIMIT 25"
        (chainId, router)
        :: IO (Maybe [(Integer, Integer, Integer, Integer, Text, Maybe Integer, Integer)])
  pure
    [ RecentUpdateRow pid mn mx ft src bp cnt
    | (pid, mn, mx, ft, src, bp, cnt) <- fromMaybe [] mRows
    ]

fetchPayloadDetailRow :: Connection -> Maybe Integer -> IO (Maybe PayloadDetailRow)
fetchPayloadDetailRow conn mTargetId = do
  mRows <-
    case mTargetId of
      Just pid ->
        observeIo $
          query
            conn
            "SELECT id, min_publish_time, max_publish_time, fetched_at, source, basket_price, component_prices, \
            \  COALESCE(jsonb_array_length(update_data), 0)::INT \
            \FROM perps_pyth_update_payloads \
            \WHERE id = ? \
            \LIMIT 1"
            (Only pid)
            :: IO (Maybe [(Integer, Integer, Integer, Integer, Text, Maybe Integer, Maybe Value, Int)])
      Nothing ->
        observeIo $
          query_
            conn
            "SELECT id, min_publish_time, max_publish_time, fetched_at, source, basket_price, component_prices, \
            \  COALESCE(jsonb_array_length(update_data), 0)::INT \
            \FROM perps_pyth_update_payloads \
            \WHERE basket_price IS NOT NULL \
            \ORDER BY max_publish_time DESC, id DESC \
            \LIMIT 1"
            :: IO (Maybe [(Integer, Integer, Integer, Integer, Text, Maybe Integer, Maybe Value, Int)])
  pure $
    fmap
      (\(pid, mn, mx, ft, src, bp, comps, vaaCnt) -> PayloadDetailRow pid mn mx ft src bp comps vaaCnt)
      (mRows >>= listToMaybe)

fetchNeighborPayload :: Connection -> Bool -> Integer -> Integer -> IO (Maybe (Integer, Maybe Integer, Integer))
fetchNeighborPayload conn isPrev maxPub pid = do
  let sql =
        if isPrev
          then
            "SELECT id, basket_price, max_publish_time \
            \FROM perps_pyth_update_payloads \
            \WHERE basket_price IS NOT NULL \
            \  AND (max_publish_time, id) < (?, ?) \
            \ORDER BY max_publish_time DESC, id DESC \
            \LIMIT 1"
          else
            "SELECT id, basket_price, max_publish_time \
            \FROM perps_pyth_update_payloads \
            \WHERE basket_price IS NOT NULL \
            \  AND (max_publish_time, id) > (?, ?) \
            \ORDER BY max_publish_time ASC, id ASC \
            \LIMIT 1"
  mRows <-
    observeIo $
      query conn sql (maxPub, pid)
        :: IO (Maybe [(Integer, Maybe Integer, Integer)])
  pure (mRows >>= listToMaybe)

settledOrderSideLabel :: Maybe Int -> Maybe Text
settledOrderSideLabel (Just 0) = Just "long"
settledOrderSideLabel (Just 1) = Just "short"
settledOrderSideLabel _ = Nothing

fetchOrdersSettledWithUpdate :: Connection -> Integer -> Text -> Integer -> Integer -> IO [SettledOrderRow]
fetchOrdersSettledWithUpdate conn chainId router minPub maxPub = do
  mRows <-
    observeIoNamed "fetch_orders_settled_with_update" $
      query
        conn
        "SELECT order_id::text, COALESCE(account, ''), side, terminal_status, terminal_tx_hash, \
        \  execution_price::text, execution_oracle_price::text, commit_timestamp, terminal_timestamp \
        \FROM perps_orders \
        \WHERE chain_id = ? AND order_router = ? \
        \  AND oracle_min_publish_time = ? AND oracle_max_publish_time = ? \
        \ORDER BY order_id DESC \
        \LIMIT 50"
        (chainId, router, minPub, maxPub)
        :: IO (Maybe [(Text, Text, Maybe Int, Text, Maybe Text, Maybe Text, Maybe Text, Maybe Integer, Maybe Integer)])
  pure
    [ SettledOrderRow oid acct (settledOrderSideLabel side) st txHash execP execOracleP cTs tTs
    | (oid, acct, side, st, txHash, execP, execOracleP, cTs, tTs) <- fromMaybe [] mRows
    ]

-- | Fetch both 1h and 24h admitted buckets in a single index-only range scan
-- over idx_perps_pyth_update_payloads_fetched_at.
fetchAdmittedBucketsPair
  :: Connection
  -> Integer
  -> Integer
  -> Integer
  -> Integer
  -> Integer
  -> Integer
  -> IO (Map.Map Integer AdmittedBucketAgg, Map.Map Integer AdmittedBucketAgg)
fetchAdmittedBucketsPair conn start1h end1h bucket1hSec start24h end24h bucket24hSec = do
  let winMin = min start1h start24h
      winMax = max end1h end24h
  mRows <-
    observeIo $
      query
        conn
        "WITH scoped AS ( \
        \  SELECT fetched_at, min_publish_time, max_publish_time, source \
        \  FROM perps_pyth_update_payloads \
        \  WHERE fetched_at >= ? AND fetched_at < ? \
        \) \
        \SELECT '1h'::TEXT, \
        \  FLOOR((fetched_at - ?) / ?)::BIGINT, \
        \  COUNT(*)::BIGINT, \
        \  COUNT(*) FILTER (WHERE source = 'backend_hermes_latest_v2')::BIGINT, \
        \  COUNT(*) FILTER (WHERE source <> 'backend_hermes_latest_v2')::BIGINT, \
        \  COALESCE(MAX(GREATEST(0, max_publish_time - min_publish_time)), 0)::BIGINT, \
        \  COALESCE(MAX(GREATEST(0, fetched_at - min_publish_time)), 0)::BIGINT, \
        \  COUNT(*) FILTER (WHERE (max_publish_time - min_publish_time) >= 4)::BIGINT \
        \FROM scoped WHERE fetched_at >= ? AND fetched_at < ? \
        \GROUP BY 2 \
        \UNION ALL \
        \SELECT '24h'::TEXT, \
        \  FLOOR((fetched_at - ?) / ?)::BIGINT, \
        \  COUNT(*)::BIGINT, \
        \  COUNT(*) FILTER (WHERE source = 'backend_hermes_latest_v2')::BIGINT, \
        \  COUNT(*) FILTER (WHERE source <> 'backend_hermes_latest_v2')::BIGINT, \
        \  COALESCE(MAX(GREATEST(0, max_publish_time - min_publish_time)), 0)::BIGINT, \
        \  COALESCE(MAX(GREATEST(0, fetched_at - min_publish_time)), 0)::BIGINT, \
        \  COUNT(*) FILTER (WHERE (max_publish_time - min_publish_time) >= 4)::BIGINT \
        \FROM scoped WHERE fetched_at >= ? AND fetched_at < ? \
        \GROUP BY 2"
        (winMin, winMax, start1h, bucket1hSec, start1h, end1h, start24h, bucket24hSec, start24h, end24h)
        :: IO (Maybe [(Text, Integer, Integer, Integer, Integer, Integer, Integer, Integer)])
  let rows = fromMaybe [] mRows
      m1h =
        Map.fromList
          [ (idx, AdmittedBucketAgg tot lat rev mxSp mxLg hiSp)
          | ("1h", idx, tot, lat, rev, mxSp, mxLg, hiSp) <- rows
          ]
      m24h =
        Map.fromList
          [ (idx, AdmittedBucketAgg tot lat rev mxSp mxLg hiSp)
          | ("24h", idx, tot, lat, rev, mxSp, mxLg, hiSp) <- rows
          ]
  pure (m1h, m24h)

fetchRejectionBucketsPair
  :: Connection
  -> Integer
  -> Integer
  -> Integer
  -> Integer
  -> Integer
  -> Integer
  -> IO (Map.Map Integer RejectionBucketAgg, Map.Map Integer RejectionBucketAgg)
fetchRejectionBucketsPair conn start1h end1h bucket1hSec start24h end24h bucket24hSec = do
  let winMin = min start1h start24h
      winMax = max end1h end24h
  mRows <-
    observeIo $
      query
        conn
        "WITH scoped AS ( \
        \  SELECT occurred_at, outcome \
        \  FROM perps_oracle_admission_events \
        \  WHERE occurred_at >= ? AND occurred_at < ? \
        \) \
        \SELECT '1h'::TEXT, \
        \  FLOOR((occurred_at - ?) / ?)::BIGINT, \
        \  COUNT(*) FILTER (WHERE outcome = 'rejected')::BIGINT, \
        \  COUNT(*) FILTER (WHERE outcome = 'stale_market_closed')::BIGINT \
        \FROM scoped WHERE occurred_at >= ? AND occurred_at < ? \
        \GROUP BY 2 \
        \UNION ALL \
        \SELECT '24h'::TEXT, \
        \  FLOOR((occurred_at - ?) / ?)::BIGINT, \
        \  COUNT(*) FILTER (WHERE outcome = 'rejected')::BIGINT, \
        \  COUNT(*) FILTER (WHERE outcome = 'stale_market_closed')::BIGINT \
        \FROM scoped WHERE occurred_at >= ? AND occurred_at < ? \
        \GROUP BY 2"
        (winMin, winMax, start1h, bucket1hSec, start1h, end1h, start24h, bucket24hSec, start24h, end24h)
        :: IO (Maybe [(Text, Integer, Integer, Integer)])
  let rows = fromMaybe [] mRows
      m1h = Map.fromList [(idx, RejectionBucketAgg rej st) | ("1h", idx, rej, st) <- rows]
      m24h = Map.fromList [(idx, RejectionBucketAgg rej st) | ("24h", idx, rej, st) <- rows]
  pure (m1h, m24h)

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

canonicalComponentsByFeedId :: Map.Map Text BasketComponent
canonicalComponentsByFeedId =
  Map.fromList [(normalizeFeedId (bcFeedId bc), bc) | bc <- basketComponents]

canonicalComponentsBySymbol :: Map.Map Text BasketComponent
canonicalComponentsBySymbol =
  Map.fromList [(T.toUpper (bcSymbol bc), bc) | bc <- basketComponents]

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
      let mCanonical =
            Map.lookup (normalizeFeedId feedId) canonicalComponentsByFeedId
              <|> Map.lookup (T.toUpper sym) canonicalComponentsBySymbol
          mRawInt = readMaybe (T.unpack rawTxt) :: Maybe Integer
          mConfInt = readMaybe (T.unpack confTxt) :: Maybe Integer
          confBps = case (mRawInt, mConfInt) of
            (Just r, Just c) -> computeComponentConfidenceBps r c
            _ -> Nothing
          mWeight =
            lookupInteger "weightBps" obj
              <|> ((\bc -> bcWeight bc `div` (10 ^ (14 :: Int))) <$> mCanonical)
          mBasePrice =
            lookupInteger "basePrice" obj
              <|> (bcBasePrice <$> mCanonical)
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
          , pcWeightBps = mWeight
          , pcBasePrice = mBasePrice
          , pcInverted = fromMaybe (maybe False bcInverted mCanonical) (lookupBool "inverted" obj)
          }
    parseOne _ = Nothing
parseComponentList _ = []

componentToJson :: Maybe Integer -> Maybe Integer -> Maybe Integer -> ParsedComponent -> Value
componentToJson mMinPub mConfLimit mTotalContribution comp@ParsedComponent {..} =
  let mDrift = (\pt mn -> max 0 (pt - mn)) <$> pcPublishTime <*> mMinPub
      confOver = case (pcConfidenceBps, mConfLimit) of
        (Just c, Just lim) -> toInteger c > lim
        _ -> False
      mNormPrice = readMaybe (T.unpack pcPrice) :: Maybe Integer
      mContribution = componentContributionMaybe comp
      mEffShareBps = do
        c <- mContribution
        tot <- mTotalContribution
        computeEffectiveShareBps c tot
      mChangeFromBaseBps = do
        norm <- mNormPrice
        baseP <- pcBasePrice
        computeChangeFromBaseBps norm baseP
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
        , "basePrice" .= (showT <$> pcBasePrice)
        , "contribution" .= (showT <$> mContribution)
        , "effectiveShareBps" .= mEffShareBps
        , "changeFromBaseBps" .= mChangeFromBaseBps
        , "inverted" .= pcInverted
        ]

recentUpdateToJson :: RecentUpdateRow -> Value
recentUpdateToJson RecentUpdateRow {..} =
  object
    [ "id" .= showT rurId
    , "minPublishTime" .= isoTimestamp rurMinPublishTime
    , "maxPublishTime" .= isoTimestamp rurMaxPublishTime
    , "fetchedAt" .= isoTimestamp rurFetchedAt
    , "source" .= rurSource
    , "basketPrice" .= (showT <$> rurBasketPrice)
    , "spreadSeconds" .= max 0 (rurMaxPublishTime - rurMinPublishTime)
    , "lagSeconds" .= max 0 (rurFetchedAt - rurMinPublishTime)
    , "ordersSettledCount" .= rurOrdersSettledCount
    ]

settledOrderToJson :: SettledOrderRow -> Value
settledOrderToJson SettledOrderRow {..} =
  object
    [ "orderId" .= sorOrderId
    , "account" .= sorAccount
    , "side" .= sorSide
    , "status" .= sorStatus
    , "txHash" .= sorTxHash
    , "executionPrice" .= sorExecutionPrice
    , "executionOraclePrice" .= sorExecutionOraclePrice
    , "commitTimestamp" .= (isoTimestamp <$> sorCommitTimestamp)
    , "terminalTimestamp" .= (isoTimestamp <$> sorTerminalTimestamp)
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
observeIo = observeIoNamed "generic"

observeIoNamed :: Text -> IO a -> IO (Maybe a)
observeIoNamed opName action = do
  res <- try action
  case res of
    Right val -> pure (Just val)
    Left (err :: SomeException) ->
      case fromException err :: Maybe SomeAsyncException of
        Just _ -> throwIO err
        Nothing -> do
          logWarnEvery
            60
            ("insights.oracle." <> opName <> "_failed")
            "Suppressed non-async error in InsightsOracle observer"
            [ field "operation" opName
            , field "error" (T.take 300 (T.pack (displayException err)))
            ]
          pure Nothing

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
