module Plether.Handlers.InsightsLiquidity
  ( LpSettlementObservationRow (..)
  , LpSettlementTxRow (..)
  , ReturnReportAuditRow (..)
  , SnapshotFreshnessSummary (..)
  , ReturnReportResidual (..)
  , getLiquiditySettlementsResponse
  , getLiquidityForensicsResponse
  , getLiquidityEpochResponse
  , getLiquidityEfficiencyResponse
  , PoolEfficiencyBucket (..)
  , PoolEfficiencySummary (..)
  , OrderBucketAgg (..)
  , OiDeltaBucketAgg (..)
  , buildPoolEfficiencyAnalytics
  , computeRatioX100
  , computeRatioBps
  , computeTakeRateBpsX100
  , computeAnnualizedYieldBps
  , efficiencyOrderBucketsSql
  , efficiencyOrderWindowTotalsSql
  , priorOiSizesSql
  , oiDeltaBucketsSql
  , latestBasketMarkSql
  , fetchRecentReturnReports
  , executionPathLabel
  , healthStateLabel
  , decodeBitPositions
  , classifySettlementReadiness
  , summarizeSnapshotFreshness
  , auditReturnReportResidual
  , sharePriceDeltaBps
  ) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Bits (testBit)
import qualified Data.ByteString.Char8 as BS8
import Data.Either (isRight)
import Data.Int (Int64)
import Data.List (foldl')
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isNothing, listToMaybe)
import Data.Scientific (toRealFloat)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, defaultTimeLocale, diffUTCTime, formatTime)
import Data.Time.Clock.POSIX (getPOSIXTime, posixSecondsToUTCTime)
import Database.PostgreSQL.Simple (Connection, Only (..), Query, query, query_)
import Database.PostgreSQL.Simple.FromField (ResultError (ConversionFailed), returnError)
import Database.PostgreSQL.Simple.FromRow (FromRow (..), RowParser, field, fieldWith)
import Database.PostgreSQL.Simple.Transaction
  ( IsolationLevel (RepeatableRead)
  , ReadWriteMode (ReadOnly)
  , TransactionMode (..)
  , withTransactionMode
  )
import Plether.Cache
  ( SingleFlightCache
  , newConcurrentRefreshingSingleFlightCache
  , newRefreshingSingleFlightCache
  , runSingleFlightCache
  )
import Plether.Config (Config (..))
import System.IO.Unsafe (unsafePerformIO)
import Plether.Database (DbPool, withDb)
import Plether.Database.VaultActivity
  ( VaultActivityDeployment (..)
  , VaultActivityIndexerStateRow (..)
  , VaultDepositAttributionStateRow (..)
  , getVaultActivityIndexerState
  , getVaultDepositAttributionState
  )
import Plether.Database.VaultPerformance
  ( VaultPerformanceSnapshotRow (..)
  , getVaultPerformanceSnapshots
  )
import qualified Plether.Perps.Manifest as Manifest
import Plether.Types (ApiError, ApiResponse, mkResponse)
import qualified Plether.Types.Error as E
import Text.Read (readMaybe)

data LpSettlementObservationRow = LpSettlementObservationRow
  { lsorEpoch :: Integer
  , lsorObservedBlock :: Integer
  , lsorExecutionPath :: Int
  , lsorOperationalBlockerMask :: Integer
  , lsorWarningMask :: Integer
  , lsorDependencyFailureMask :: Integer
  , lsorCriticalFaultMask :: Integer
  , lsorHealthState :: Maybe Integer
  , lsorObservationComplete :: Maybe Bool
  , lsorHasMaturedWork :: Maybe Bool
  , lsorPaused :: Maybe Bool
  , lsorLastObservedAt :: UTCTime
  }
  deriving stock (Eq, Show)

instance FromRow LpSettlementObservationRow where
  fromRow =
    LpSettlementObservationRow
      <$> field
      <*> field
      <*> field
      <*> numericIntegerField
      <*> numericIntegerField
      <*> numericIntegerField
      <*> numericIntegerField
      <*> nullableNumericIntegerField
      <*> field
      <*> field
      <*> field
      <*> field

data LpSettlementTxRow = LpSettlementTxRow
  { lstrEpoch :: Integer
  , lstrCutoffEpoch :: Maybe Integer
  , lstrStatus :: Text
  , lstrExecutionPath :: Int
  , lstrReceiptTxHash :: Maybe Text
  , lstrReceiptBlock :: Maybe Integer
  , lstrCreatedAt :: UTCTime
  , lstrConfirmedAt :: Maybe UTCTime
  , lstrReplacementCount :: Int
  , lstrSeniorDepositAssets :: Maybe Integer
  , lstrSeniorRedeemAssets :: Maybe Integer
  , lstrJuniorDepositAssets :: Maybe Integer
  , lstrJuniorRedeemAssets :: Maybe Integer
  , lstrSeniorBacklog :: Maybe Bool
  , lstrJuniorBacklog :: Maybe Bool
  , lstrEntriesDeferred :: Maybe Bool
  }
  deriving stock (Eq, Show)

instance FromRow LpSettlementTxRow where
  fromRow =
    LpSettlementTxRow
      <$> field
      <*> nullableNumericIntegerField
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> nullableNumericIntegerField
      <*> nullableNumericIntegerField
      <*> nullableNumericIntegerField
      <*> nullableNumericIntegerField
      <*> field
      <*> field
      <*> field

data ReturnReportAuditRow = ReturnReportAuditRow
  { rrarStartBlock :: Integer
  , rrarEndBlock :: Integer
  , rrarSourceCommit :: Text
  , rrarSchemaVersion :: Int
  , rrarUpdatedAt :: UTCTime
  , rrarAttribution :: Value
  }
  deriving stock (Eq, Show)

instance FromRow ReturnReportAuditRow where
  fromRow =
    ReturnReportAuditRow
      <$> numericIntegerField
      <*> numericIntegerField
      <*> field
      <*> field
      <*> field
      <*> field

data SnapshotFreshnessSummary = SnapshotFreshnessSummary
  { sfsExpectedHours :: Int
  , sfsObservedHours :: Int
  , sfsFreshHours :: Int
  , sfsStaleHours :: Int
  , sfsLatestEpochTimestamp :: Maybe Integer
  , sfsLatestBlockNumber :: Maybe Integer
  , sfsLatestMarkFresh :: Maybe Bool
  }
  deriving stock (Eq, Show)

data ReturnReportResidual = ReturnReportResidual
  { rrrReportedReturnPp :: Maybe Double
  , rrrComponentsSumPp :: Maybe Double
  , rrrUnexplainedPp :: Maybe Double
  , rrrNetReturnUsdg :: Maybe Integer
  , rrrComponentsSumUsdg :: Maybe Integer
  , rrrUnexplainedUsdg :: Maybe Integer
  , rrrSourceBlockCount :: Maybe Integer
  , rrrNotesCount :: Int
  , rrrLiquidationFeesUsdg :: Maybe Integer
  }
  deriving stock (Eq, Show)

{-# NOINLINE liquiditySettlementsCache #-}
liquiditySettlementsCache :: SingleFlightCache () (Either ApiError (ApiResponse Value))
liquiditySettlementsCache =
  unsafePerformIO (newRefreshingSingleFlightCache 1 (15 * 1000000000) (300 * 1000000000))

{-# NOINLINE liquidityForensicsCache #-}
liquidityForensicsCache :: SingleFlightCache () (Either ApiError (ApiResponse Value))
liquidityForensicsCache =
  unsafePerformIO (newRefreshingSingleFlightCache 1 (30 * 1000000000) (300 * 1000000000))

{-# NOINLINE liquidityEpochCache #-}
liquidityEpochCache :: SingleFlightCache Integer (Either ApiError (ApiResponse Value))
liquidityEpochCache =
  unsafePerformIO (newConcurrentRefreshingSingleFlightCache 128 (30 * 1000000000) (600 * 1000000000))

{-# NOINLINE liquidityEfficiencyCache #-}
liquidityEfficiencyCache :: SingleFlightCache () (Either ApiError (ApiResponse Value))
liquidityEfficiencyCache =
  unsafePerformIO (newRefreshingSingleFlightCache 1 (15 * 1000000000) (300 * 1000000000))

getLiquiditySettlementsResponse
  :: DbPool
  -> Config
  -> IO (Either ApiError (ApiResponse Value))
getLiquiditySettlementsResponse pool cfg =
  snd
    <$> runSingleFlightCache
      liquiditySettlementsCache
      ()
      isRight
      (computeLiquiditySettlementsResponse pool cfg)

computeLiquiditySettlementsResponse
  :: DbPool
  -> Config
  -> IO (Either ApiError (ApiResponse Value))
computeLiquiditySettlementsResponse pool cfg = do
  let chainId = cfgPerpsChainId cfg
      monitor = T.toLower $ T.strip $ cfgPerpsSettlementMonitorLens cfg
  (observations, txs, summaryCounts) <- withDb pool $ \conn ->
    withReadSnapshot conn $ do
      obs <- fetchRecentSettlementObservations conn chainId monitor 1
      recentTxs <- fetchRecentSettlementTxs conn chainId monitor 30
      counts <- fetchSettlement24hSummary conn chainId monitor
      pure (obs, recentTxs, counts)
  let latestObs = case observations of
        row : _ -> Just row
        [] -> Nothing
      responseBlock = fromMaybe 0 $ lsorObservedBlock <$> latestObs
      (observedEpochs24h, confirmed24h, failed24h, replaced24h, deferred24h, backlog24h) = summaryCounts
  pure $
    Right $
      mkResponse responseBlock chainId $
        object
          [ "network" .= networkMetaJson cfg
          , "monitorAddress" .= monitor
          , "latestObservation" .= (observationToJson <$> latestObs)
          , "summary"
              .= object
                [ "observedEpochs24h" .= observedEpochs24h
                , "confirmedSettlements24h" .= confirmed24h
                , "failedOrReverted24h" .= failed24h
                , "replacedAttempts24h" .= replaced24h
                , "deferredEntryEpochs24h" .= deferred24h
                , "backlogEpochs24h" .= backlog24h
                ]
          , "recentSettlements" .= map settlementTxToJson txs
          ]

getLiquidityEpochResponse
  :: DbPool
  -> Config
  -> Integer
  -> IO (Either ApiError (ApiResponse Value))
getLiquidityEpochResponse pool cfg epoch =
  snd
    <$> runSingleFlightCache
      liquidityEpochCache
      epoch
      isRight
      (computeLiquidityEpochResponse pool cfg epoch)

computeLiquidityEpochResponse
  :: DbPool
  -> Config
  -> Integer
  -> IO (Either ApiError (ApiResponse Value))
computeLiquidityEpochResponse pool cfg epoch = do
  let chainId = cfgPerpsChainId cfg
      monitor = T.toLower $ T.strip $ cfgPerpsSettlementMonitorLens cfg
      housePool = T.toLower $ T.strip $ cfgVaultHistoryHousePoolAddress cfg
      seniorVault = T.toLower $ T.strip $ cfgVaultHistorySeniorVaultAddress cfg
      juniorVault = T.toLower $ T.strip $ cfgVaultHistoryJuniorVaultAddress cfg
      epochTs = epoch * 3600
      prevEpochTs = (epoch - 1) * 3600
  (observations, txs, mSnap, mPrevSnap, (mPrevEpoch, mNextEpoch), mReport) <- withDb pool $ \conn ->
    withReadSnapshot conn $ do
      obs <- fetchEpochObservations conn chainId monitor epoch
      settled <- fetchEpochSettlementTxs conn chainId monitor epoch
      snap <- fetchEpochVaultSnapshot conn chainId housePool seniorVault juniorVault epochTs
      prevSnap <-
        if epoch > 0
          then fetchEpochVaultSnapshot conn chainId housePool seniorVault juniorVault prevEpochTs
          else pure Nothing
      adjacent <- fetchAdjacentEpochs conn chainId monitor epoch
      let refBlock =
            case (listToMaybe obs, listToMaybe settled >>= lstrReceiptBlock, snap) of
              (Just o, _, _) -> Just (lsorObservedBlock o)
              (_, Just b, _) -> Just b
              (_, _, Just s) -> Just (vpsBlockNumber s)
              _ -> Nothing
      report <- case refBlock of
        Just blk -> fetchCoveringReturnReport conn chainId housePool seniorVault juniorVault blk
        Nothing -> pure Nothing
      pure (obs, settled, snap, prevSnap, adjacent, report)
  if null observations && null txs && isNothing mSnap
    then pure $ Left $ E.notFound ("No LP epoch data found for epoch #" <> T.pack (show epoch))
    else do
      let latestObs = listToMaybe observations
          responseBlock =
            maximum $
              0
                : concat
                    [ maybe [] (pure . lsorObservedBlock) latestObs
                    , maybe [] pure (listToMaybe txs >>= lstrReceiptBlock)
                    , maybe [] (pure . vpsBlockNumber) mSnap
                    ]
      pure $
        Right $
          mkResponse responseBlock chainId $
            object
              [ "network" .= networkMetaJson cfg
              , "monitorAddress" .= monitor
              , "epoch" .= epoch
              , "epochTimestamp" .= epochTs
              , "epochUtc" .= isoTimestamp epochTs
              , "previousEpoch" .= mPrevEpoch
              , "nextEpoch" .= mNextEpoch
              , "latestObservation" .= (observationToJson <$> latestObs)
              , "observations" .= map observationToJson observations
              , "settlements" .= map settlementTxToJson txs
              , "vaultSnapshot" .= ((\s -> vaultSnapshotToJson s mPrevSnap) <$> mSnap)
              , "coveringReport" .= (reportAuditToJson <$> mReport)
              ]

getLiquidityForensicsResponse
  :: DbPool
  -> Config
  -> IO (Either ApiError (ApiResponse Value))
getLiquidityForensicsResponse pool cfg =
  snd
    <$> runSingleFlightCache
      liquidityForensicsCache
      ()
      isRight
      (computeLiquidityForensicsResponse pool cfg)

computeLiquidityForensicsResponse
  :: DbPool
  -> Config
  -> IO (Either ApiError (ApiResponse Value))
computeLiquidityForensicsResponse pool cfg = do
  now <- floor <$> getPOSIXTime
  let chainId = cfgPerpsChainId cfg
      housePool = T.toLower $ T.strip $ cfgVaultHistoryHousePoolAddress cfg
      seniorVault = T.toLower $ T.strip $ cfgVaultHistorySeniorVaultAddress cfg
      juniorVault = T.toLower $ T.strip $ cfgVaultHistoryJuniorVaultAddress cfg
      deployment =
        VaultActivityDeployment
          { vadChainId = chainId
          , vadHousePool = housePool
          , vadSeniorVault = seniorVault
          , vadJuniorVault = juniorVault
          , vadDeploymentBlock = cfgVaultHistoryDeploymentBlock cfg
          }
  (snapshots, reports, returnIndexBlock, mActivityState, mAttrState) <- withDb pool $ \conn ->
    withReadSnapshot conn $ do
      snaps <- getVaultPerformanceSnapshots conn chainId housePool seniorVault juniorVault 721
      reps <- fetchRecentReturnReports conn chainId housePool seniorVault juniorVault 10
      idxBlock <- fetchLatestReturnIndexBlock conn
      actState <- getVaultActivityIndexerState conn deployment
      attrState <- getVaultDepositAttributionState conn deployment
      pure (snaps, reps, idxBlock, actState, attrState)
  let fresh7d = summarizeSnapshotFreshness 169 (takeLast 169 snapshots)
      fresh30d = summarizeSnapshotFreshness 721 snapshots
      responseBlock =
        maximum $
          0
            : concat
                [ maybe [] pure (sfsLatestBlockNumber fresh30d)
                , maybe [] pure returnIndexBlock
                , maybe [] (pure . vaisLastIndexedBlock) mActivityState
                ]
  pure $
    Right $
      mkResponse responseBlock chainId $
        object
          [ "network" .= networkMetaJson cfg
          , "observedAt" .= isoTimestamp now
          , "snapshotFreshness"
              .= object
                [ "window7d" .= freshnessToJson fresh7d
                , "window30d" .= freshnessToJson fresh30d
                ]
          , "attributionWorker"
              .= object
                [ "indexedThroughBlock" .= (show <$> returnIndexBlock)
                , "recentReports" .= map reportAuditToJson reports
                ]
          , "vaultIndexers"
              .= object
                [ "activity" .= (activityStateToJson now <$> mActivityState)
                , "depositAttribution" .= (depositAttrStateToJson now <$> mAttrState)
                ]
          ]

data OrderBucketAgg = OrderBucketAgg
  { obaTradesCount :: Int
  , obaVolumeAssets :: Integer
  , obaFeesAssets :: Integer
  }
  deriving stock (Eq, Show)

data OiDeltaBucketAgg = OiDeltaBucketAgg
  { odbLongSizeDelta :: Integer
  , odbShortSizeDelta :: Integer
  , odbLastRawPrice :: Maybe Integer
  }
  deriving stock (Eq, Show)

data PoolEfficiencyBucket = PoolEfficiencyBucket
  { pebBucketStart :: Integer
  , pebBucketEnd :: Integer
  , pebVolumeAssets :: Integer
  , pebFeesAssets :: Integer
  , pebTradesCount :: Int
  , pebPoolTvlAssets :: Maybe Integer
  , pebSeniorTvlAssets :: Maybe Integer
  , pebJuniorTvlAssets :: Maybe Integer
  , pebTurnoverX100 :: Maybe Int
  , pebAnnualizedTurnoverDailyX100 :: Maybe Int
  , pebTakeRateBpsX100 :: Maybe Int
  , pebLongOiAssets :: Integer
  , pebShortOiAssets :: Integer
  , pebGrossOiAssets :: Integer
  , pebNetSkewAssets :: Integer
  , pebGrossOiToPoolBps :: Maybe Int
  , pebNetSkewToPoolBps :: Maybe Int
  }
  deriving stock (Eq, Show)

data PoolEfficiencySummary = PoolEfficiencySummary
  { pesPoolTvlAssets :: Maybe Integer
  , pesSeniorTvlAssets :: Maybe Integer
  , pesJuniorTvlAssets :: Maybe Integer
  , pesVolume24hAssets :: Integer
  , pesVolume7dAssets :: Integer
  , pesVolume30dAssets :: Integer
  , pesFees24hAssets :: Integer
  , pesFees7dAssets :: Integer
  , pesTurnover24hX100 :: Maybe Int
  , pesTurnover7dAvgDailyX100 :: Maybe Int
  , pesTurnover30dAvgDailyX100 :: Maybe Int
  , pesEffectiveTakeRate7dBpsX100 :: Maybe Int
  , pesAnnualizedFeeYield7dBps :: Maybe Int
  , pesCurrentLongOiAssets :: Integer
  , pesCurrentShortOiAssets :: Integer
  , pesCurrentGrossOiAssets :: Integer
  , pesCurrentNetSkewAssets :: Integer
  , pesGrossOiToPoolBps :: Maybe Int
  , pesNetSkewToPoolBps :: Maybe Int
  , pesNetSkewToJuniorBps :: Maybe Int
  }
  deriving stock (Eq, Show)

computeRatioX100 :: Integer -> Integer -> Maybe Int
computeRatioX100 num denom
  | denom <= 0 = Nothing
  | otherwise = Just $ fromInteger ((num * 100 + denom `div` 2) `div` denom)

computeRatioBps :: Integer -> Integer -> Maybe Int
computeRatioBps num denom
  | denom <= 0 = Nothing
  | otherwise = Just $ fromInteger ((num * 10000 + denom `div` 2) `div` denom)

computeTakeRateBpsX100 :: Integer -> Integer -> Maybe Int
computeTakeRateBpsX100 fees volume
  | volume <= 0 = Nothing
  | otherwise = Just $ fromInteger ((fees * 1000000 + volume `div` 2) `div` volume)

computeAnnualizedYieldBps :: Integer -> Integer -> Integer -> Maybe Int
computeAnnualizedYieldBps fees windowDays avgPool
  | avgPool <= 0 || windowDays <= 0 = Nothing
  | otherwise =
      let denom = avgPool * windowDays
       in Just $ fromInteger ((fees * 365 * 10000 + denom `div` 2) `div` denom)

normalizeEfficiencyRawMark :: Integer -> Integer
normalizeEfficiencyRawMark raw
  | raw > 200000000 =
      let scaled = raw `div` 10000000000
       in if scaled > 0 && scaled <= 200000000 then scaled else 100000000
  | raw > 0 = raw
  | otherwise = 100000000

efficiencyNotionalFromSize :: Integer -> Integer -> Integer
efficiencyNotionalFromSize size rawPrice
  | size <= 0 || rawPrice <= 0 = 0
  | otherwise =
      let clamped = min 200000000 rawPrice
       in (size * clamped) `div` 100000000000000000000

resolvePoolSnapshotAt :: [VaultPerformanceSnapshotRow] -> Integer -> Maybe (Integer, Integer, Integer)
resolvePoolSnapshotAt [] _ = Nothing
resolvePoolSnapshotAt snaps targetTs =
  let earlier = filter (\s -> vpsEpochTimestamp s <= targetTs) snaps
      chosen = case reverse earlier of
        s : _ -> Just s
        [] -> case snaps of
          firstSnap : _ | vpsEpochTimestamp firstSnap - targetTs <= 86400 -> Just firstSnap
          _ -> Nothing
   in fmap
        (\s -> (max 0 (vpsSeniorTotalAssets s + vpsJuniorTotalAssets s), max 0 (vpsSeniorTotalAssets s), max 0 (vpsJuniorTotalAssets s)))
        chosen

buildPoolEfficiencyAnalytics
  :: Integer
  -> [VaultPerformanceSnapshotRow]
  -> Map.Map Integer OrderBucketAgg
  -> (Integer, Integer)
  -> Map.Map Integer OiDeltaBucketAgg
  -> Maybe Integer
  -> (Integer, Integer, Integer, Integer, Integer)
  -> (PoolEfficiencySummary, [PoolEfficiencyBucket], [PoolEfficiencyBucket])
buildPoolEfficiencyAnalytics now snaps orderBuckets6h (priorLongSize, priorShortSize) oiBuckets6h mFallbackMark (vol24h, vol7d, vol30d, fees24h, fees7d) =
  let bucket6h = 21600 :: Integer
      daySec = 86400 :: Integer
      end6h = ((now `div` bucket6h) + 1) * bucket6h
      start7d = end6h - 28 * bucket6h
      start30d = ((now `div` daySec) - 29) * daySec
      startAll = min start30d start7d
      all6hStarts = takeWhile (< end6h) [startAll, startAll + bucket6h ..]
      initialMark = fromMaybe 100000000 (normalizeEfficiencyRawMark <$> mFallbackMark)

      step6h (curLong, curShort, stepMark, acc) bStart =
        let bEnd = bStart + bucket6h
            ord = Map.findWithDefault (OrderBucketAgg 0 0 0) bStart orderBuckets6h
            oiD = Map.findWithDefault (OiDeltaBucketAgg 0 0 Nothing) bStart oiBuckets6h
            nextLong = max 0 (curLong + odbLongSizeDelta oiD)
            nextShort = max 0 (curShort + odbShortSizeDelta oiD)
            nextMark = maybe stepMark normalizeEfficiencyRawMark (odbLastRawPrice oiD)
            longOi = efficiencyNotionalFromSize nextLong nextMark
            shortOi = efficiencyNotionalFromSize nextShort nextMark
            grossOi = longOi + shortOi
            netSkew = longOi - shortOi
            mPool = resolvePoolSnapshotAt snaps bEnd
            mPoolTvl = (\(t, _, _) -> t) <$> mPool
            mSenTvl = (\(_, s, _) -> s) <$> mPool
            mJunTvl = (\(_, _, j) -> j) <$> mPool
            bucket =
              PoolEfficiencyBucket
                { pebBucketStart = bStart
                , pebBucketEnd = bEnd
                , pebVolumeAssets = obaVolumeAssets ord
                , pebFeesAssets = obaFeesAssets ord
                , pebTradesCount = obaTradesCount ord
                , pebPoolTvlAssets = mPoolTvl
                , pebSeniorTvlAssets = mSenTvl
                , pebJuniorTvlAssets = mJunTvl
                , pebTurnoverX100 = mPoolTvl >>= computeRatioX100 (obaVolumeAssets ord)
                , pebAnnualizedTurnoverDailyX100 = mPoolTvl >>= computeRatioX100 (obaVolumeAssets ord * 4)
                , pebTakeRateBpsX100 = computeTakeRateBpsX100 (obaFeesAssets ord) (obaVolumeAssets ord)
                , pebLongOiAssets = longOi
                , pebShortOiAssets = shortOi
                , pebGrossOiAssets = grossOi
                , pebNetSkewAssets = netSkew
                , pebGrossOiToPoolBps = mPoolTvl >>= computeRatioBps grossOi
                , pebNetSkewToPoolBps = mPoolTvl >>= computeRatioBps (abs netSkew)
                }
         in (nextLong, nextShort, nextMark, bucket : acc)

      (finalLongSize, finalShortSize, finalMark, rev6h) =
        foldl' step6h (max 0 priorLongSize, max 0 priorShortSize, initialMark, []) all6hStarts
      all6h = reverse rev6h
      series7d = filter (\b -> pebBucketStart b >= start7d) all6h

      dayStarts = [start30d + fromIntegral i * daySec | i <- [0 .. 29 :: Int]]
      byDayMap =
        Map.fromListWith
          (\new old -> old <> new)
          [( (pebBucketStart b `div` daySec) * daySec, [b] ) | b <- all6h]

      buildDayBucket dStart =
        let dEnd = dStart + daySec
            sub = Map.findWithDefault [] dStart byDayMap
            volSum = sum (map pebVolumeAssets sub)
            feeSum = sum (map pebFeesAssets sub)
            trdSum = sum (map pebTradesCount sub)
            mLast = case reverse sub of
              x : _ -> Just x
              [] -> Nothing
            mPool = resolvePoolSnapshotAt snaps dEnd
            mPoolTvl = (\(t, _, _) -> t) <$> mPool
            mSenTvl = (\(_, s, _) -> s) <$> mPool
            mJunTvl = (\(_, _, j) -> j) <$> mPool
            longOi = maybe 0 pebLongOiAssets mLast
            shortOi = maybe 0 pebShortOiAssets mLast
            grossOi = longOi + shortOi
            netSkew = longOi - shortOi
            turnover = mPoolTvl >>= computeRatioX100 volSum
         in PoolEfficiencyBucket
              { pebBucketStart = dStart
              , pebBucketEnd = dEnd
              , pebVolumeAssets = volSum
              , pebFeesAssets = feeSum
              , pebTradesCount = trdSum
              , pebPoolTvlAssets = mPoolTvl
              , pebSeniorTvlAssets = mSenTvl
              , pebJuniorTvlAssets = mJunTvl
              , pebTurnoverX100 = turnover
              , pebAnnualizedTurnoverDailyX100 = turnover
              , pebTakeRateBpsX100 = computeTakeRateBpsX100 feeSum volSum
              , pebLongOiAssets = longOi
              , pebShortOiAssets = shortOi
              , pebGrossOiAssets = grossOi
              , pebNetSkewAssets = netSkew
              , pebGrossOiToPoolBps = mPoolTvl >>= computeRatioBps grossOi
              , pebNetSkewToPoolBps = mPoolTvl >>= computeRatioBps (abs netSkew)
              }

      series30d = map buildDayBucket dayStarts

      mLatestPool = resolvePoolSnapshotAt snaps now
      mCurPoolTvl = (\(t, _, _) -> t) <$> mLatestPool
      mCurSenTvl = (\(_, s, _) -> s) <$> mLatestPool
      mCurJunTvl = (\(_, _, j) -> j) <$> mLatestPool

      avgPoolOf bs =
        let vals = [t | b <- bs, Just t <- [pebPoolTvlAssets b], t > 0]
         in if null vals then mCurPoolTvl else Just (sum vals `div` fromIntegral (length vals))

      mAvgPool7d = avgPoolOf series7d
      mAvgPool30d = avgPoolOf series30d

      curMark = maybe finalMark normalizeEfficiencyRawMark mFallbackMark
      curLongOi = efficiencyNotionalFromSize finalLongSize curMark
      curShortOi = efficiencyNotionalFromSize finalShortSize curMark
      curGrossOi = curLongOi + curShortOi
      curNetSkew = curLongOi - curShortOi

      summary =
        PoolEfficiencySummary
          { pesPoolTvlAssets = mCurPoolTvl
          , pesSeniorTvlAssets = mCurSenTvl
          , pesJuniorTvlAssets = mCurJunTvl
          , pesVolume24hAssets = vol24h
          , pesVolume7dAssets = vol7d
          , pesVolume30dAssets = vol30d
          , pesFees24hAssets = fees24h
          , pesFees7dAssets = fees7d
          , pesTurnover24hX100 = mCurPoolTvl >>= computeRatioX100 vol24h
          , pesTurnover7dAvgDailyX100 = mAvgPool7d >>= \p -> computeRatioX100 vol7d (p * 7)
          , pesTurnover30dAvgDailyX100 = mAvgPool30d >>= \p -> computeRatioX100 vol30d (p * 30)
          , pesEffectiveTakeRate7dBpsX100 = computeTakeRateBpsX100 fees7d vol7d
          , pesAnnualizedFeeYield7dBps = mAvgPool7d >>= computeAnnualizedYieldBps fees7d 7
          , pesCurrentLongOiAssets = curLongOi
          , pesCurrentShortOiAssets = curShortOi
          , pesCurrentGrossOiAssets = curGrossOi
          , pesCurrentNetSkewAssets = curNetSkew
          , pesGrossOiToPoolBps = mCurPoolTvl >>= computeRatioBps curGrossOi
          , pesNetSkewToPoolBps = mCurPoolTvl >>= computeRatioBps (abs curNetSkew)
          , pesNetSkewToJuniorBps = mCurJunTvl >>= computeRatioBps (abs curNetSkew)
          }
   in (summary, series7d, series30d)

getLiquidityEfficiencyResponse
  :: DbPool
  -> Config
  -> IO (Either ApiError (ApiResponse Value))
getLiquidityEfficiencyResponse pool cfg =
  snd
    <$> runSingleFlightCache
      liquidityEfficiencyCache
      ()
      isRight
      (computeLiquidityEfficiencyResponse pool cfg)

computeLiquidityEfficiencyResponse
  :: DbPool
  -> Config
  -> IO (Either ApiError (ApiResponse Value))
computeLiquidityEfficiencyResponse pool cfg = do
  now <- floor <$> getPOSIXTime
  let chainId = cfgPerpsChainId cfg
      router = T.toLower $ T.strip $ cfgPerpsOrderRouter cfg
      housePool = T.toLower $ T.strip $ cfgVaultHistoryHousePoolAddress cfg
      seniorVault = T.toLower $ T.strip $ cfgVaultHistorySeniorVaultAddress cfg
      juniorVault = T.toLower $ T.strip $ cfgVaultHistoryJuniorVaultAddress cfg
      bucket6h = 21600 :: Integer
      daySec = 86400 :: Integer
      end6h = ((now `div` bucket6h) + 1) * bucket6h
      start7d = end6h - 28 * bucket6h
      start30d = ((now `div` daySec) - 29) * daySec
      startAll = min start30d start7d
      since24h = now - daySec
      since7d = now - 7 * daySec
      since30d = now - 30 * daySec

  (snaps, orderBuckets, windowTotals, priorOi, oiBuckets, mLatestMark) <- withDb pool $ \conn ->
    withReadSnapshot conn $ do
      s <- getVaultPerformanceSnapshots conn chainId housePool seniorVault juniorVault 721
      oBuckets <- fetchEfficiencyOrderBuckets conn chainId router startAll
      wTotals <- fetchEfficiencyOrderWindowTotals conn chainId router since24h since7d since30d
      pOi <- fetchPriorOiSizes conn chainId router startAll
      oDeltas <- fetchOiDeltaBuckets conn chainId router startAll
      mMark <- fetchLatestBasketMark conn
      pure (s, oBuckets, wTotals, pOi, oDeltas, mMark)

  let (summary, series7d, series30d) =
        buildPoolEfficiencyAnalytics now snaps orderBuckets priorOi oiBuckets mLatestMark windowTotals
      latestBlock = case reverse snaps of
        s : _ -> vpsBlockNumber s
        [] -> 0

  pure $
    Right $
      mkResponse latestBlock chainId $
        object
          [ "network" .= networkMetaJson cfg
          , "observedAt" .= isoTimestamp now
          , "summary" .= efficiencySummaryToJson summary
          , "series7d" .= map efficiencyBucketToJson series7d
          , "series30d" .= map efficiencyBucketToJson series30d
          ]

efficiencySummaryToJson :: PoolEfficiencySummary -> Value
efficiencySummaryToJson PoolEfficiencySummary {..} =
  object
    [ "poolTvlAssets" .= (show <$> pesPoolTvlAssets)
    , "seniorTvlAssets" .= (show <$> pesSeniorTvlAssets)
    , "juniorTvlAssets" .= (show <$> pesJuniorTvlAssets)
    , "volume24hAssets" .= show pesVolume24hAssets
    , "volume7dAssets" .= show pesVolume7dAssets
    , "volume30dAssets" .= show pesVolume30dAssets
    , "fees24hAssets" .= show pesFees24hAssets
    , "fees7dAssets" .= show pesFees7dAssets
    , "turnover24hX100" .= pesTurnover24hX100
    , "turnover7dAvgDailyX100" .= pesTurnover7dAvgDailyX100
    , "turnover30dAvgDailyX100" .= pesTurnover30dAvgDailyX100
    , "effectiveTakeRate7dBpsX100" .= pesEffectiveTakeRate7dBpsX100
    , "annualizedFeeYield7dBps" .= pesAnnualizedFeeYield7dBps
    , "currentLongOiAssets" .= show pesCurrentLongOiAssets
    , "currentShortOiAssets" .= show pesCurrentShortOiAssets
    , "currentGrossOiAssets" .= show pesCurrentGrossOiAssets
    , "currentNetSkewAssets" .= show pesCurrentNetSkewAssets
    , "grossOiToPoolBps" .= pesGrossOiToPoolBps
    , "netSkewToPoolBps" .= pesNetSkewToPoolBps
    , "netSkewToJuniorBps" .= pesNetSkewToJuniorBps
    ]

efficiencyBucketToJson :: PoolEfficiencyBucket -> Value
efficiencyBucketToJson PoolEfficiencyBucket {..} =
  object
    [ "bucketStart" .= isoTimestamp pebBucketStart
    , "bucketEnd" .= isoTimestamp pebBucketEnd
    , "volumeAssets" .= show pebVolumeAssets
    , "feesAssets" .= show pebFeesAssets
    , "tradesCount" .= pebTradesCount
    , "poolTvlAssets" .= (show <$> pebPoolTvlAssets)
    , "seniorTvlAssets" .= (show <$> pebSeniorTvlAssets)
    , "juniorTvlAssets" .= (show <$> pebJuniorTvlAssets)
    , "turnoverX100" .= pebTurnoverX100
    , "annualizedTurnoverDailyX100" .= pebAnnualizedTurnoverDailyX100
    , "takeRateBpsX100" .= pebTakeRateBpsX100
    , "longOiAssets" .= show pebLongOiAssets
    , "shortOiAssets" .= show pebShortOiAssets
    , "grossOiAssets" .= show pebGrossOiAssets
    , "netSkewAssets" .= show pebNetSkewAssets
    , "grossOiToPoolBps" .= pebGrossOiToPoolBps
    , "netSkewToPoolBps" .= pebNetSkewToPoolBps
    ]

parseIntText :: Text -> Integer
parseIntText = fromMaybe 0 . readMaybe . T.unpack . T.strip

efficiencyOrderBucketsSql :: Query
efficiencyOrderBucketsSql =
  "SELECT \
  \  ((terminal_timestamp / 21600) * 21600)::BIGINT, \
  \  COUNT(*)::INT, \
  \  COALESCE(SUM(CASE WHEN receipt_economics->>'executionNotionalAssets' ~ '^[0-9]+$' THEN (receipt_economics->>'executionNotionalAssets')::NUMERIC ELSE 0 END), 0)::TEXT, \
  \  COALESCE(SUM( \
  \    (CASE WHEN receipt_economics->>'executionFeeAssets' ~ '^[0-9]+$' THEN (receipt_economics->>'executionFeeAssets')::NUMERIC ELSE 0 END) + \
  \    (CASE WHEN receipt_economics->>'vpiAssets' ~ '^[0-9]+$' THEN (receipt_economics->>'vpiAssets')::NUMERIC ELSE 0 END) + \
  \    (CASE WHEN receipt_economics->>'carryAssets' ~ '^-{0,1}[0-9]+$' THEN GREATEST(0, (receipt_economics->>'carryAssets')::NUMERIC) ELSE 0 END) \
  \  ), 0)::TEXT \
  \FROM perps_orders \
  \WHERE chain_id = ? AND order_router = ? AND terminal_status = 'Executed' \
  \  AND terminal_timestamp IS NOT NULL AND terminal_timestamp >= ? \
  \GROUP BY 1 \
  \ORDER BY 1 ASC"

fetchEfficiencyOrderBuckets :: Connection -> Integer -> Text -> Integer -> IO (Map.Map Integer OrderBucketAgg)
fetchEfficiencyOrderBuckets conn chainId router sinceUnix = do
  rows <-
    query
      conn
      efficiencyOrderBucketsSql
      (chainId, router, sinceUnix)
      :: IO [(Integer, Int, Text, Text)]
  pure $
    Map.fromList
      [ (ts, OrderBucketAgg cnt (parseIntText volT) (parseIntText feeT))
      | (ts, cnt, volT, feeT) <- rows
      ]

efficiencyOrderWindowTotalsSql :: Query
efficiencyOrderWindowTotalsSql =
  "SELECT \
  \  COALESCE(SUM(CASE WHEN receipt_economics->>'executionNotionalAssets' ~ '^[0-9]+$' THEN (receipt_economics->>'executionNotionalAssets')::NUMERIC ELSE 0 END) FILTER (WHERE terminal_timestamp >= ?), 0)::TEXT, \
  \  COALESCE(SUM(CASE WHEN receipt_economics->>'executionNotionalAssets' ~ '^[0-9]+$' THEN (receipt_economics->>'executionNotionalAssets')::NUMERIC ELSE 0 END) FILTER (WHERE terminal_timestamp >= ?), 0)::TEXT, \
  \  COALESCE(SUM(CASE WHEN receipt_economics->>'executionNotionalAssets' ~ '^[0-9]+$' THEN (receipt_economics->>'executionNotionalAssets')::NUMERIC ELSE 0 END) FILTER (WHERE terminal_timestamp >= ?), 0)::TEXT, \
  \  COALESCE(SUM( \
  \    (CASE WHEN receipt_economics->>'executionFeeAssets' ~ '^[0-9]+$' THEN (receipt_economics->>'executionFeeAssets')::NUMERIC ELSE 0 END) + \
  \    (CASE WHEN receipt_economics->>'vpiAssets' ~ '^[0-9]+$' THEN (receipt_economics->>'vpiAssets')::NUMERIC ELSE 0 END) + \
  \    (CASE WHEN receipt_economics->>'carryAssets' ~ '^-{0,1}[0-9]+$' THEN GREATEST(0, (receipt_economics->>'carryAssets')::NUMERIC) ELSE 0 END) \
  \  ) FILTER (WHERE terminal_timestamp >= ?), 0)::TEXT, \
  \  COALESCE(SUM( \
  \    (CASE WHEN receipt_economics->>'executionFeeAssets' ~ '^[0-9]+$' THEN (receipt_economics->>'executionFeeAssets')::NUMERIC ELSE 0 END) + \
  \    (CASE WHEN receipt_economics->>'vpiAssets' ~ '^[0-9]+$' THEN (receipt_economics->>'vpiAssets')::NUMERIC ELSE 0 END) + \
  \    (CASE WHEN receipt_economics->>'carryAssets' ~ '^-{0,1}[0-9]+$' THEN GREATEST(0, (receipt_economics->>'carryAssets')::NUMERIC) ELSE 0 END) \
  \  ) FILTER (WHERE terminal_timestamp >= ?), 0)::TEXT \
  \FROM perps_orders \
  \WHERE chain_id = ? AND order_router = ? AND terminal_status = 'Executed' \
  \  AND terminal_timestamp IS NOT NULL AND terminal_timestamp >= ?"

fetchEfficiencyOrderWindowTotals :: Connection -> Integer -> Text -> Integer -> Integer -> Integer -> IO (Integer, Integer, Integer, Integer, Integer)
fetchEfficiencyOrderWindowTotals conn chainId router since24h since7d since30d = do
  rows <-
    query
      conn
      efficiencyOrderWindowTotalsSql
      (since24h, since7d, since30d, since24h, since7d, chainId, router, since30d)
      :: IO [(Text, Text, Text, Text, Text)]
  pure $ case rows of
    (v24, v7, v30, f24, f7) : _ ->
      (parseIntText v24, parseIntText v7, parseIntText v30, parseIntText f24, parseIntText f7)
    [] -> (0, 0, 0, 0, 0)

priorOiSizesSql :: Query
priorOiSizesSql =
  "SELECT \
  \  COALESCE(SUM(CASE WHEN side = 0 THEN (CASE WHEN activity_type = 'Open' THEN COALESCE(size_delta, 0) ELSE -COALESCE(size_delta, 0) END) ELSE 0 END), 0)::TEXT, \
  \  COALESCE(SUM(CASE WHEN side = 1 THEN (CASE WHEN activity_type = 'Open' THEN COALESCE(size_delta, 0) ELSE -COALESCE(size_delta, 0) END) ELSE 0 END), 0)::TEXT \
  \FROM perps_account_activity \
  \WHERE chain_id = ? AND release_router = ? \
  \  AND activity_type IN ('Open', 'Close', 'Liquidated') \
  \  AND side IN (0, 1) \
  \  AND timestamp < ?"

fetchPriorOiSizes :: Connection -> Integer -> Text -> Integer -> IO (Integer, Integer)
fetchPriorOiSizes conn chainId router beforeUnix = do
  rows <-
    query
      conn
      priorOiSizesSql
      (chainId, router, beforeUnix)
      :: IO [(Text, Text)]
  pure $ case rows of
    (lT, sT) : _ -> (parseIntText lT, parseIntText sT)
    [] -> (0, 0)

oiDeltaBucketsSql :: Query
oiDeltaBucketsSql =
  "SELECT \
  \  ((timestamp / 21600) * 21600)::BIGINT, \
  \  COALESCE(SUM(CASE WHEN side = 0 THEN (CASE WHEN activity_type = 'Open' THEN COALESCE(size_delta, 0) ELSE -COALESCE(size_delta, 0) END) ELSE 0 END), 0)::TEXT, \
  \  COALESCE(SUM(CASE WHEN side = 1 THEN (CASE WHEN activity_type = 'Open' THEN COALESCE(size_delta, 0) ELSE -COALESCE(size_delta, 0) END) ELSE 0 END), 0)::TEXT, \
  \  (ARRAY_AGG(price::TEXT ORDER BY block_number DESC, log_index DESC) FILTER (WHERE price IS NOT NULL AND price > 0))[1] \
  \FROM perps_account_activity \
  \WHERE chain_id = ? AND release_router = ? \
  \  AND activity_type IN ('Open', 'Close', 'Liquidated') \
  \  AND side IN (0, 1) \
  \  AND timestamp >= ? \
  \GROUP BY 1 \
  \ORDER BY 1 ASC"

fetchOiDeltaBuckets :: Connection -> Integer -> Text -> Integer -> IO (Map.Map Integer OiDeltaBucketAgg)
fetchOiDeltaBuckets conn chainId router sinceUnix = do
  rows <-
    query
      conn
      oiDeltaBucketsSql
      (chainId, router, sinceUnix)
      :: IO [(Integer, Text, Text, Maybe Text)]
  pure $
    Map.fromList
      [ (ts, OiDeltaBucketAgg (parseIntText lT) (parseIntText sT) (mPT >>= readMaybe . T.unpack . T.strip))
      | (ts, lT, sT, mPT) <- rows
      ]

latestBasketMarkSql :: Query
latestBasketMarkSql =
  "SELECT basket_price::TEXT FROM perps_pyth_update_payloads \
  \WHERE basket_price IS NOT NULL \
  \ORDER BY max_publish_time DESC, id DESC LIMIT 1"

fetchLatestBasketMark :: Connection -> IO (Maybe Integer)
fetchLatestBasketMark conn = do
  rows <-
    query_
      conn
      latestBasketMarkSql
      :: IO [Only Text]
  pure $ case rows of
    Only t : _ -> readMaybe (T.unpack (T.strip t))
    [] -> Nothing

executionPathLabel :: Int -> Text
executionPathLabel = \case
  1 -> "no_matured_work"
  2 -> "cached_mark"
  3 -> "atomic_oracle_refresh"
  _ -> "unknown"

healthStateLabel :: Maybe Integer -> Text
healthStateLabel = \case
  Just 1 -> "healthy"
  Just 2 -> "degraded"
  Just 3 -> "faulted"
  _ -> "unknown"

decodeBitPositions :: Integer -> [Int]
decodeBitPositions mask
  | mask <= 0 = []
  | otherwise = [idx | idx <- [0 .. 255], testBit mask idx]

classifySettlementReadiness :: LpSettlementObservationRow -> Text
classifySettlementReadiness LpSettlementObservationRow {..}
  | lsorPaused == Just True = "paused"
  | lsorCriticalFaultMask > 0 = "faulted"
  | lsorOperationalBlockerMask > 0 || lsorDependencyFailureMask > 0 = "blocked"
  | lsorObservationComplete == Just False = "incomplete"
  | lsorWarningMask > 0 = "ready_with_warnings"
  | otherwise = "ready"

summarizeSnapshotFreshness :: Int -> [VaultPerformanceSnapshotRow] -> SnapshotFreshnessSummary
summarizeSnapshotFreshness expected rows =
  let freshCount = length $ filter ((== Just True) . vpsMarkFresh) rows
      staleCount = length $ filter ((== Just False) . vpsMarkFresh) rows
      latest = case reverse rows of
        row : _ -> Just row
        [] -> Nothing
   in SnapshotFreshnessSummary
        { sfsExpectedHours = expected
        , sfsObservedHours = length rows
        , sfsFreshHours = freshCount
        , sfsStaleHours = staleCount
        , sfsLatestEpochTimestamp = vpsEpochTimestamp <$> latest
        , sfsLatestBlockNumber = vpsBlockNumber <$> latest
        , sfsLatestMarkFresh = latest >>= vpsMarkFresh
        }

auditReturnReportResidual :: Value -> ReturnReportResidual
auditReturnReportResidual (Object obj) =
  let startPrice = lookupText "startDilutedSharePrice" obj >>= readMaybe @Integer . T.unpack
      endPrice = lookupText "endDilutedSharePrice" obj >>= readMaybe @Integer . T.unpack
      reportedReturnPp = case (startPrice, endPrice) of
        (Just s, Just e) | s > 0 -> Just $ (fromIntegral e / fromIntegral s - 1.0) * 100.0
        _ -> Nothing
      netUsdg = lookupText "netReturnUsdg" obj >>= readMaybe . T.unpack
      sourceBlocks = lookupInt "sourceBlockCount" obj
      notesCount = case KM.lookup (Key.fromText "notes") obj of
        Just (Array xs) -> length xs
        _ -> 0
      componentKeys =
        [ "netVpi"
        , "liquidationFees"
        , "realizedCarry"
        , "oracleSpread"
        , "settledTraderPnl"
        , "openPositionValuation"
        , "seniorCoupon"
        , "maintenanceDilution"
        ]
      componentsObj = case KM.lookup (Key.fromText "components") obj of
        Just (Object comps) -> Just comps
        _ -> Nothing
      compPps = traverse (\k -> componentsObj >>= lookupCompDouble k "contributionPp") componentKeys
      compUsdgs = traverse (\k -> componentsObj >>= lookupCompInteger k "amountUsdg") componentKeys
      sumPp = sum <$> compPps
      sumUsdg = sum <$> compUsdgs
      unexplainedPp = (-) <$> reportedReturnPp <*> sumPp
      unexplainedUsdg = (-) <$> netUsdg <*> sumUsdg
      liqFees = componentsObj >>= lookupCompInteger "liquidationFees" "amountUsdg"
   in ReturnReportResidual
        { rrrReportedReturnPp = reportedReturnPp
        , rrrComponentsSumPp = sumPp
        , rrrUnexplainedPp = unexplainedPp
        , rrrNetReturnUsdg = netUsdg
        , rrrComponentsSumUsdg = sumUsdg
        , rrrUnexplainedUsdg = unexplainedUsdg
        , rrrSourceBlockCount = sourceBlocks
        , rrrNotesCount = notesCount
        , rrrLiquidationFeesUsdg = liqFees
        }
auditReturnReportResidual _ =
  ReturnReportResidual Nothing Nothing Nothing Nothing Nothing Nothing Nothing 0 Nothing

lookupText :: Text -> KM.KeyMap Value -> Maybe Text
lookupText key obj = case KM.lookup (Key.fromText key) obj of
  Just (String txt) -> Just txt
  _ -> Nothing

lookupInt :: Text -> KM.KeyMap Value -> Maybe Integer
lookupInt key obj = case KM.lookup (Key.fromText key) obj of
  Just (Number sci) -> Just $ round (toRealFloat sci :: Double)
  Just (String txt) -> readMaybe (T.unpack txt)
  _ -> Nothing

lookupCompDouble :: Text -> Text -> KM.KeyMap Value -> Maybe Double
lookupCompDouble comp fieldName comps = case KM.lookup (Key.fromText comp) comps of
  Just (Object cObj) -> case KM.lookup (Key.fromText fieldName) cObj of
    Just (Number sci) -> Just $ toRealFloat sci
    _ -> Nothing
  _ -> Nothing

lookupCompInteger :: Text -> Text -> KM.KeyMap Value -> Maybe Integer
lookupCompInteger comp fieldName comps = case KM.lookup (Key.fromText comp) comps of
  Just (Object cObj) -> lookupText fieldName cObj >>= readMaybe . T.unpack
  _ -> Nothing

networkMetaJson :: Config -> Value
networkMetaJson cfg =
  object
    [ "chainId" .= cfgPerpsChainId cfg
    , "releaseVersion" .= Manifest.releaseVersion
    , "settlementAssetSymbol" .= ("USDG" :: Text)
    ]

maskJson :: Integer -> Value
maskJson mask =
  object
    [ "raw" .= show mask
    , "activeBits" .= decodeBitPositions mask
    ]

observationToJson :: LpSettlementObservationRow -> Value
observationToJson row@LpSettlementObservationRow {..} =
  object
    [ "epoch" .= lsorEpoch
    , "observedBlock" .= show lsorObservedBlock
    , "observedAt" .= formatUtcTime lsorLastObservedAt
    , "readiness" .= classifySettlementReadiness row
    , "executionPath" .= executionPathLabel lsorExecutionPath
    , "healthState" .= healthStateLabel lsorHealthState
    , "observationComplete" .= lsorObservationComplete
    , "hasMaturedWork" .= lsorHasMaturedWork
    , "paused" .= lsorPaused
    , "masks"
        .= object
          [ "operationalBlocker" .= maskJson lsorOperationalBlockerMask
          , "warning" .= maskJson lsorWarningMask
          , "dependencyFailure" .= maskJson lsorDependencyFailureMask
          , "criticalFault" .= maskJson lsorCriticalFaultMask
          ]
    ]

settlementTxToJson :: LpSettlementTxRow -> Value
settlementTxToJson LpSettlementTxRow {..} =
  let latencySeconds =
        (\confirmed -> max (0 :: Integer) $ round (diffUTCTime confirmed lstrCreatedAt))
          <$> lstrConfirmedAt
   in object
        [ "epoch" .= lstrEpoch
        , "cutoffEpoch" .= (show <$> lstrCutoffEpoch)
        , "status" .= lstrStatus
        , "executionPath" .= executionPathLabel lstrExecutionPath
        , "txHash" .= lstrReceiptTxHash
        , "receiptBlock" .= (show <$> lstrReceiptBlock)
        , "createdAt" .= formatUtcTime lstrCreatedAt
        , "confirmedAt" .= (formatUtcTime <$> lstrConfirmedAt)
        , "confirmationLatencySeconds" .= latencySeconds
        , "replacementCount" .= lstrReplacementCount
        , "seniorDepositAssets" .= (show <$> lstrSeniorDepositAssets)
        , "seniorRedeemAssets" .= (show <$> lstrSeniorRedeemAssets)
        , "juniorDepositAssets" .= (show <$> lstrJuniorDepositAssets)
        , "juniorRedeemAssets" .= (show <$> lstrJuniorRedeemAssets)
        , "seniorBacklog" .= lstrSeniorBacklog
        , "juniorBacklog" .= lstrJuniorBacklog
        , "entriesDeferred" .= lstrEntriesDeferred
        ]

freshnessToJson :: SnapshotFreshnessSummary -> Value
freshnessToJson SnapshotFreshnessSummary {..} =
  object
    [ "expectedHours" .= sfsExpectedHours
    , "observedHours" .= sfsObservedHours
    , "freshHours" .= sfsFreshHours
    , "staleHours" .= sfsStaleHours
    , "latestEpochTimestamp" .= sfsLatestEpochTimestamp
    , "latestBlockNumber" .= (show <$> sfsLatestBlockNumber)
    , "latestMarkFresh" .= sfsLatestMarkFresh
    ]

reportAuditToJson :: ReturnReportAuditRow -> Value
reportAuditToJson ReturnReportAuditRow {..} =
  let residual = auditReturnReportResidual rrarAttribution
   in object
        [ "startBlockNumber" .= show rrarStartBlock
        , "endBlockNumber" .= show rrarEndBlock
        , "sourceCommit" .= rrarSourceCommit
        , "schemaVersion" .= rrarSchemaVersion
        , "updatedAt" .= formatUtcTime rrarUpdatedAt
        , "sourceBlockCount" .= rrrSourceBlockCount residual
        , "reportedReturnPp" .= rrrReportedReturnPp residual
        , "unexplainedPp" .= rrrUnexplainedPp residual
        , "netReturnUsdg" .= (show <$> rrrNetReturnUsdg residual)
        , "unexplainedUsdg" .= (show <$> rrrUnexplainedUsdg residual)
        , "liquidationFeesUsdg" .= (show <$> rrrLiquidationFeesUsdg residual)
        , "notesCount" .= rrrNotesCount residual
        ]

activityStateToJson :: Integer -> VaultActivityIndexerStateRow -> Value
activityStateToJson now VaultActivityIndexerStateRow {..} =
  object
    [ "lastIndexedBlock" .= show vaisLastIndexedBlock
    , "safeHeadBlock" .= show vaisSafeHeadBlock
    , "lagBlocks" .= max 0 (vaisSafeHeadBlock - vaisLastIndexedBlock)
    , "lagSeconds" .= max 0 (vaisSafeHeadTimestamp - vaisLastIndexedBlockTimestamp)
    , "pollAgeSeconds" .= max 0 (now - vaisLastSuccessTimestamp)
    , "backfillComplete" .= vaisBackfillComplete
    ]

depositAttrStateToJson :: Integer -> VaultDepositAttributionStateRow -> Value
depositAttrStateToJson now VaultDepositAttributionStateRow {..} =
  object
    [ "confirmedThroughBlock" .= show vdasConfirmedThroughBlock
    , "pollAgeSeconds" .= max 0 (now - vdasLastSuccessTimestamp)
    , "backfillComplete" .= vdasBackfillComplete
    ]

fetchRecentSettlementObservations
  :: Connection
  -> Integer
  -> Text
  -> Int
  -> IO [LpSettlementObservationRow]
fetchRecentSettlementObservations conn chainId monitor limit =
  query
    conn
    "SELECT epoch, observed_block, execution_path, \
    \operational_blocker_mask, warning_mask, dependency_failure_mask, critical_fault_mask, \
    \health_state, observation_complete, has_matured_work, lp_epoch_settlement_paused, last_observed_at \
    \FROM perps_lp_settlement_observations \
    \WHERE chain_id = ? AND monitor_address = ? \
    \ORDER BY observed_block DESC, last_observed_at DESC \
    \LIMIT ?"
    (chainId, monitor, max 1 limit)

fetchRecentSettlementTxs
  :: Connection
  -> Integer
  -> Text
  -> Int
  -> IO [LpSettlementTxRow]
fetchRecentSettlementTxs conn chainId monitor limit =
  query
    conn
    "SELECT tx.epoch, tx.cutoff_epoch, tx.status, obs.execution_path, \
    \tx.receipt_transaction_hash, tx.receipt_block_number, tx.created_at, tx.confirmed_at, \
    \tx.replacement_count, tx.senior_deposit_assets, tx.senior_redeem_assets, \
    \tx.junior_deposit_assets, tx.junior_redeem_assets, \
    \tx.senior_backlog, tx.junior_backlog, tx.entries_deferred \
    \FROM perps_lp_settlement_transactions tx \
    \JOIN perps_lp_settlement_observations obs \
    \  ON obs.chain_id = tx.chain_id \
    \ AND obs.monitor_address = tx.monitor_address \
    \ AND obs.observation_digest = tx.observation_digest \
    \ AND obs.epoch = tx.epoch \
    \WHERE tx.chain_id = ? AND tx.monitor_address = ? \
    \ORDER BY tx.created_at DESC, tx.id DESC \
    \LIMIT ?"
    (chainId, monitor, max 1 limit)

fetchSettlement24hSummary
  :: Connection
  -> Integer
  -> Text
  -> IO (Int64, Int64, Int64, Int64, Int64, Int64)
fetchSettlement24hSummary conn chainId monitor = do
  obsCounts <-
    query
      conn
      "SELECT COUNT(DISTINCT epoch) \
      \FROM perps_lp_settlement_observations \
      \WHERE chain_id = ? AND monitor_address = ? \
      \  AND last_observed_at >= NOW() - INTERVAL '24 hours'"
      (chainId, monitor) :: IO [Only Int64]
  txCounts <-
    query
      conn
      "SELECT \
      \  COUNT(*) FILTER (WHERE status = 'confirmed_success'), \
      \  COUNT(*) FILTER (WHERE status IN ('confirmed_revert', 'failed', 'manual_review')), \
      \  COUNT(*) FILTER (WHERE replacement_count > 0), \
      \  COUNT(*) FILTER (WHERE entries_deferred = TRUE), \
      \  COUNT(*) FILTER (WHERE senior_backlog = TRUE OR junior_backlog = TRUE) \
      \FROM perps_lp_settlement_transactions \
      \WHERE chain_id = ? AND monitor_address = ? \
      \  AND created_at >= NOW() - INTERVAL '24 hours'"
      (chainId, monitor) :: IO [(Int64, Int64, Int64, Int64, Int64)]
  let obsEpochs = case obsCounts of
        [Only n] -> n
        _ -> 0
      (confirmed, failed, replaced, deferred, backlog) = case txCounts of
        [row] -> row
        _ -> (0, 0, 0, 0, 0)
  pure (obsEpochs, confirmed, failed, replaced, deferred, backlog)

sharePriceDeltaBps :: Integer -> Integer -> Maybe Int
sharePriceDeltaBps currentWad prevWad
  | prevWad <= 0 = Nothing
  | otherwise = Just $ fromInteger (((currentWad - prevWad) * 10000) `div` prevWad)

vaultSnapshotToJson :: VaultPerformanceSnapshotRow -> Maybe VaultPerformanceSnapshotRow -> Value
vaultSnapshotToJson cur mPrev =
  object
    [ "epochTimestamp" .= vpsEpochTimestamp cur
    , "blockNumber" .= show (vpsBlockNumber cur)
    , "blockTimestamp" .= vpsBlockTimestamp cur
    , "blockUtc" .= isoTimestamp (vpsBlockTimestamp cur)
    , "markFresh" .= vpsMarkFresh cur
    , "senior"
        .= object
          [ "totalAssets" .= show (vpsSeniorTotalAssets cur)
          , "lockedAssets" .= (show <$> vpsSeniorLockedAssets cur)
          , "totalSupply" .= show (vpsSeniorTotalSupply cur)
          , "sharePriceWad" .= show (vpsSeniorSharePriceWad cur)
          , "sharePriceDeltaBps" .= (mPrev >>= sharePriceDeltaBps (vpsSeniorSharePriceWad cur) . vpsSeniorSharePriceWad)
          ]
    , "junior"
        .= object
          [ "totalAssets" .= show (vpsJuniorTotalAssets cur)
          , "lockedAssets" .= (show <$> vpsJuniorLockedAssets cur)
          , "totalSupply" .= show (vpsJuniorTotalSupply cur)
          , "sharePriceWad" .= show (vpsJuniorSharePriceWad cur)
          , "sharePriceDeltaBps" .= (mPrev >>= sharePriceDeltaBps (vpsJuniorSharePriceWad cur) . vpsJuniorSharePriceWad)
          ]
    ]

fetchEpochObservations
  :: Connection
  -> Integer
  -> Text
  -> Integer
  -> IO [LpSettlementObservationRow]
fetchEpochObservations conn chainId monitor epoch =
  query
    conn
    "SELECT epoch, observed_block, execution_path, \
    \operational_blocker_mask, warning_mask, dependency_failure_mask, critical_fault_mask, \
    \health_state, observation_complete, has_matured_work, lp_epoch_settlement_paused, last_observed_at \
    \FROM perps_lp_settlement_observations \
    \WHERE chain_id = ? AND monitor_address = ? AND epoch = ? \
    \ORDER BY observed_block DESC, last_observed_at DESC \
    \LIMIT 20"
    (chainId, monitor, epoch)

fetchEpochSettlementTxs
  :: Connection
  -> Integer
  -> Text
  -> Integer
  -> IO [LpSettlementTxRow]
fetchEpochSettlementTxs conn chainId monitor epoch =
  query
    conn
    "SELECT tx.epoch, tx.cutoff_epoch, tx.status, obs.execution_path, \
    \tx.receipt_transaction_hash, tx.receipt_block_number, tx.created_at, tx.confirmed_at, \
    \tx.replacement_count, tx.senior_deposit_assets, tx.senior_redeem_assets, \
    \tx.junior_deposit_assets, tx.junior_redeem_assets, \
    \tx.senior_backlog, tx.junior_backlog, tx.entries_deferred \
    \FROM perps_lp_settlement_transactions tx \
    \JOIN perps_lp_settlement_observations obs \
    \  ON obs.chain_id = tx.chain_id \
    \ AND obs.monitor_address = tx.monitor_address \
    \ AND obs.observation_digest = tx.observation_digest \
    \ AND obs.epoch = tx.epoch \
    \WHERE tx.chain_id = ? AND tx.monitor_address = ? AND tx.epoch = ? \
    \ORDER BY tx.created_at DESC, tx.id DESC \
    \LIMIT 20"
    (chainId, monitor, epoch)

fetchEpochVaultSnapshot
  :: Connection
  -> Integer
  -> Text
  -> Text
  -> Text
  -> Integer
  -> IO (Maybe VaultPerformanceSnapshotRow)
fetchEpochVaultSnapshot conn chainId housePool seniorVault juniorVault epochTs = do
  rows <-
    query
      conn
      "SELECT chain_id, house_pool_address, senior_vault_address, junior_vault_address, \
      \epoch_timestamp, block_number, block_hash, block_timestamp, mark_fresh, \
      \senior_total_assets, senior_locked_assets, junior_locked_assets, senior_total_supply, senior_share_price_wad, \
      \junior_total_assets, junior_total_supply, junior_share_price_wad \
      \FROM vault_performance_snapshots \
      \WHERE chain_id = ? AND house_pool_address = ? AND senior_vault_address = ? \
      \  AND junior_vault_address = ? AND epoch_timestamp = ? \
      \LIMIT 1"
      (chainId, housePool, seniorVault, juniorVault, epochTs)
  pure (listToMaybe rows)

fetchAdjacentEpochs
  :: Connection
  -> Integer
  -> Text
  -> Integer
  -> IO (Maybe Integer, Maybe Integer)
fetchAdjacentEpochs conn chainId monitor epoch = do
  rows <-
    query
      conn
      "SELECT \
      \  (SELECT epoch FROM perps_lp_settlement_observations WHERE chain_id = ? AND monitor_address = ? AND epoch < ? ORDER BY epoch DESC LIMIT 1), \
      \  (SELECT epoch FROM perps_lp_settlement_observations WHERE chain_id = ? AND monitor_address = ? AND epoch > ? ORDER BY epoch ASC LIMIT 1)"
      (chainId, monitor, epoch, chainId, monitor, epoch)
      :: IO [(Maybe Integer, Maybe Integer)]
  pure $ case rows of
    [(mPrev, mNext)] -> (mPrev, mNext)
    _ -> (Nothing, Nothing)

fetchCoveringReturnReport
  :: Connection
  -> Integer
  -> Text
  -> Text
  -> Text
  -> Integer
  -> IO (Maybe ReturnReportAuditRow)
fetchCoveringReturnReport conn chainId housePool seniorVault juniorVault blk = do
  rows <-
    query
      conn
      "SELECT start_block_number, end_block_number, source_commit, schema_version, updated_at, attribution \
      \FROM vault_return_reports \
      \WHERE chain_id = ? AND house_pool_address = ? AND senior_vault_address = ? AND junior_vault_address = ? \
      \  AND schema_version = 3 \
      \  AND start_block_number <= ? AND end_block_number >= ? \
      \ORDER BY end_block_number DESC \
      \LIMIT 1"
      (chainId, housePool, seniorVault, juniorVault, blk, blk)
  pure (listToMaybe rows)

fetchRecentReturnReports
  :: Connection
  -> Integer
  -> Text
  -> Text
  -> Text
  -> Int
  -> IO [ReturnReportAuditRow]
fetchRecentReturnReports conn chainId housePool seniorVault juniorVault limit =
  query
    conn
    "SELECT start_block_number, end_block_number, source_commit, schema_version, updated_at, attribution \
    \FROM vault_return_reports \
    \WHERE chain_id = ? AND house_pool_address = ? AND senior_vault_address = ? AND junior_vault_address = ? \
    \  AND schema_version = 3 \
    \ORDER BY end_block_number DESC \
    \LIMIT ?"
    (chainId, housePool, seniorVault, juniorVault, max 1 limit)

fetchLatestReturnIndexBlock :: Connection -> IO (Maybe Integer)
fetchLatestReturnIndexBlock conn = do
  rows <-
    query_
      conn
      "SELECT through_block::text FROM vault_return_index_state ORDER BY through_block DESC LIMIT 1"
      :: IO [Only String]
  pure $ case rows of
    [Only raw] -> readMaybe raw
    _ -> Nothing

withReadSnapshot :: Connection -> IO a -> IO a
withReadSnapshot conn =
  withTransactionMode
    TransactionMode
      { isolationLevel = RepeatableRead
      , readWriteMode = ReadOnly
      }
    conn

numericIntegerField :: RowParser Integer
numericIntegerField =
  fieldWith $ \column raw ->
    case raw >>= readMaybe . BS8.unpack of
      Just integer -> pure integer
      Nothing -> returnError ConversionFailed column "Expected integer NUMERIC column"

nullableNumericIntegerField :: RowParser (Maybe Integer)
nullableNumericIntegerField =
  fieldWith $ \column raw ->
    case raw of
      Nothing -> pure Nothing
      Just bytes -> case readMaybe (BS8.unpack bytes) of
        Just integer -> pure $ Just integer
        Nothing -> returnError ConversionFailed column "Expected nullable integer NUMERIC column"

takeLast :: Int -> [a] -> [a]
takeLast n xs = drop (max 0 (length xs - n)) xs

formatUtcTime :: UTCTime -> Text
formatUtcTime = T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

isoTimestamp :: Integer -> Text
isoTimestamp = formatUtcTime . posixSecondsToUTCTime . fromIntegral
