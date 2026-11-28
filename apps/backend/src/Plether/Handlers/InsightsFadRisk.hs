module Plether.Handlers.InsightsFadRisk
  ( FadDangerStatus (..)
  , FadPositionRisk (..)
  , FadRiskSummary (..)
  , FadRiskThresholds (..)
  , fadDangerStatusText
  , fadDangerRank
  , computeProjectedCarryAssets
  , evaluatePositionFadRisk
  , aggregateFadRiskSummary
  , decodeMarketClosureTuple
  , decodePositionMaxProfitAssets
  , openAccountsForFadRiskSql
  , getMarketFadRiskResponse
  ) where

import Control.Concurrent.Async (concurrently)
import Control.Exception (SomeAsyncException, SomeException, fromException, throwIO, try)
import Data.Aeson (Value (..), object, (.=))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Either (isRight)
import Data.List (foldl', nub, sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Data.Time.Clock.POSIX (getPOSIXTime, posixSecondsToUTCTime)
import Database.PostgreSQL.Simple (Connection, In (..), Only (..), Query, query, query_)
import Database.PostgreSQL.Simple.Transaction
  ( IsolationLevel (RepeatableRead)
  , ReadWriteMode (ReadOnly)
  , TransactionMode (..)
  , withTransactionMode
  )
import Plether.Cache (SingleFlightCache, newRefreshingSingleFlightCache, runSingleFlightCache)
import Plether.Config (Config (..))
import Plether.Database (DbPool, withDb)
import Plether.Ethereum.Abi (decodeUint256, encodeAddress, encodeCall, encodeUint256, selector)
import Plether.Ethereum.Client (EthClient, ethBlockNumber)
import Plether.Ethereum.Contracts.CfdEngineAccountLens
  ( AccountLedgerSnapshot (..)
  , decodeAccountLedgerSnapshot
  , getAccountLedgerSnapshotCall
  )
import qualified Plether.Ethereum.Multicall as Multicall
import Plether.Handlers.InsightsLiquidity (latestBasketMarkSql)
import Plether.Handlers.InsightsPositions
  ( computeLeverageX100
  , computeNotionalAssets
  , computeUnrealizedPnl
  , defaultMaintenanceMarginBps
  , distanceToLiquidationBps
  , findLiquidationRawPrice
  , plDxyCapPrice
  )
import Plether.Handlers.InsightsProtocol
  ( LiveEngineRiskParams (..)
  , LiveProtocolStatus (..)
  , decodeEngineRiskParams
  , decodeProtocolStatus
  , decodeSingleUint
  )
import qualified Plether.Logging as Log
import qualified Plether.Perps.Manifest as Manifest
import Plether.Perps.MarketCalendar
  ( MarketCalendarSnapshot (..)
  , MarketClosure (..)
  , UpcomingMarketWindow (..)
  , buildMarketCalendarSnapshot
  , marketRegimeText
  )
import Plether.Types (ApiError, ApiResponse, mkResponse)
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout (timeout)
import Text.Read (readMaybe)

defaultFadMarginBps :: Integer
defaultFadMarginBps = 300

defaultBaseCarryBps :: Integer
defaultBaseCarryBps = 500

secondsPerYear :: Integer
secondsPerYear = 365 * 86400

carryIndexScale :: Integer
carryIndexScale = 1000000000000000000 -- 1e18

maxAccountsScanned :: Int
maxAccountsScanned = 200

accountChunkSize :: Int
accountChunkSize = 10

data FadDangerStatus
  = FadLiquidatableNow
  | FadWouldLiquidate
  | FadCritical
  | FadAtRisk
  | FadSafe
  deriving stock (Eq, Show)

fadDangerStatusText :: FadDangerStatus -> Text
fadDangerStatusText = \case
  FadLiquidatableNow -> "liquidatable_now"
  FadWouldLiquidate -> "would_liquidate_at_fad"
  FadCritical -> "critical"
  FadAtRisk -> "at_risk"
  FadSafe -> "safe"

fadDangerRank :: FadDangerStatus -> Int
fadDangerRank = \case
  FadLiquidatableNow -> 0
  FadWouldLiquidate -> 1
  FadCritical -> 2
  FadAtRisk -> 3
  FadSafe -> 4

data FadRiskThresholds = FadRiskThresholds
  { frtMaintenanceMarginBps :: Integer
  , frtFadMarginBps :: Integer
  , frtMaxFadLeverageX100 :: Integer
  , frtBaseCarryBps :: Integer
  , frtFadMaxStaleness :: Maybe Integer
  , frtEngineMarkStalenessLimit :: Maybe Integer
  }
  deriving stock (Eq, Show)

data FadPositionRisk = FadPositionRisk
  { fprAccount :: Text
  , fprDiscordUsername :: Maybe Text
  , fprSide :: Int
  , fprSize :: Integer
  , fprNotionalAssets :: Integer
  , fprMarginAssets :: Integer
  , fprTraderClaimAssets :: Integer
  , fprUnrealizedPnlAssets :: Integer
  , fprCurrentEquityAssets :: Integer
  , fprProjectedCarryToFadAssets :: Integer
  , fprProjectedEquityAtFadAssets :: Integer
  , fprNormalRequiredMarginAssets :: Integer
  , fprFadRequiredMarginAssets :: Integer
  , fprFadMarginBufferAssets :: Integer
  , fprMarginNeededToSurviveAssets :: Integer
  , fprEffectiveLeverageX100 :: Integer
  , fprEntryPrice :: Integer
  , fprMarkPrice :: Integer
  , fprNormalLiquidationPrice :: Maybe Integer
  , fprFadLiquidationPrice :: Maybe Integer
  , fprDistanceToFadLiquidationBps :: Maybe Integer
  , fprLiquidatableNow :: Bool
  , fprDangerStatus :: FadDangerStatus
  }
  deriving stock (Eq, Show)

data FadRiskSummary = FadRiskSummary
  { frsTotalOpenPositions :: Int
  , frsLiquidatableNowCount :: Int
  , frsWouldLiquidateCount :: Int
  , frsCriticalCount :: Int
  , frsAtRiskCount :: Int
  , frsSafeCount :: Int
  , frsEndangeredCount :: Int
  , frsImmediateLiquidationNotionalAssets :: Integer
  , frsImmediateLiquidationLongNotionalAssets :: Integer
  , frsImmediateLiquidationShortNotionalAssets :: Integer
  , frsTotalEndangeredNotionalAssets :: Integer
  , frsEndangeredLongNotionalAssets :: Integer
  , frsEndangeredShortNotionalAssets :: Integer
  , frsTotalMarginTopUpNeededAssets :: Integer
  }
  deriving stock (Eq, Show)

{-# NOINLINE marketFadRiskCache #-}
marketFadRiskCache :: SingleFlightCache () (Either ApiError (ApiResponse Value))
marketFadRiskCache =
  unsafePerformIO (newRefreshingSingleFlightCache 1 (15 * 1000000000) (300 * 1000000000))

-- | Compute flat annual carry accrual over @elapsedSeconds@ on @maxProfitAssets@,
-- matching PositionRiskAccountingLib.computeCurrentCarryIndex + computeIndexedCarryAssets.
computeProjectedCarryAssets :: Integer -> Integer -> Integer -> Integer
computeProjectedCarryAssets maxProfitAssets baseCarryBps elapsedSeconds
  | maxProfitAssets <= 0 || baseCarryBps <= 0 || elapsedSeconds <= 0 = 0
  | otherwise =
      let -- baseCarryBps / 10_000 * 1e18 = baseCarryBps * 1e14
          indexDelta = (elapsedSeconds * baseCarryBps * 100000000000000) `div` secondsPerYear
       in (maxProfitAssets * indexDelta) `div` carryIndexScale

-- | Evaluate a single live open account's FAD liquidation danger using exact
-- on-chain AccountLedgerSnapshot balances and fadMarginBps.
evaluatePositionFadRisk
  :: Text
  -> Maybe Text
  -> AccountLedgerSnapshot
  -> Maybe Integer -- ^ Exact CfdEngine.positions(acct).maxProfitAssets if read
  -> Integer -- ^ Live raw mark price (1e8)
  -> Integer -- ^ Normal maintenanceMarginBps
  -> Integer -- ^ FAD maintenance margin bps (e.g. 300)
  -> Integer -- ^ Annual baseCarryBps (e.g. 500)
  -> Integer -- ^ Seconds until next FAD starts (0 if FAD active)
  -> Maybe FadPositionRisk
evaluatePositionFadRisk account mDiscord snap mMaxProfit markRaw maintBps fadBps baseCarryBps secsUntilFad
  | not (alsHasPosition snap) || alsSize snap <= 0 = Nothing
  | otherwise =
      let side = fromInteger (alsSide snap)
          size = alsSize snap
          clampedMark = min plDxyCapPrice (max 1 markRaw)
          notional = computeNotionalAssets size clampedMark
          entryCost = computeNotionalAssets size (alsEntryPrice snap)
          currentEquity = alsNetEquityAssets snap
          fallbackMaxProfit =
            if side == 0
              then entryCost
              else max 0 (computeNotionalAssets size plDxyCapPrice - entryCost)
          maxProfit = max 0 (fromMaybe fallbackMaxProfit mMaxProfit)
          projectedCarry = computeProjectedCarryAssets maxProfit baseCarryBps (max 0 secsUntilFad)
          projectedEquityAtFad = currentEquity - projectedCarry
          normalReq = (notional * max 0 maintBps) `div` 10000
          fadReq = (notional * max 0 fadBps) `div` 10000
          fadBuffer = projectedEquityAtFad - fadReq
          marginNeeded = max 0 (-fadBuffer)
          -- Effective collateral backing before price PnL so findLiquidationRawPrice
          -- reproduces exact equity at markRaw: collateral + pnl(markRaw) == equity.
          pnlAtMark = computeUnrealizedPnl side size entryCost clampedMark
          collateralNow = max 0 (currentEquity - pnlAtMark)
          collateralAtFad = max 0 (projectedEquityAtFad - pnlAtMark)
          normalLiqPrice =
            findLiquidationRawPrice side size entryCost collateralNow 0 (max 0 maintBps)
          fadLiqPrice =
            if fadBuffer <= 0
              then Just clampedMark
              else findLiquidationRawPrice side size entryCost collateralAtFad 0 (max 0 fadBps)
          distToFadBps =
            if alsLiquidatable snap || fadBuffer <= 0
              then Just 0
              else distanceToLiquidationBps side clampedMark fadLiqPrice
          effLevX100 =
            if currentEquity > 0
              then computeLeverageX100 notional currentEquity
              else computeLeverageX100 notional (max 1 (alsMargin snap))
          danger
            | alsLiquidatable snap = FadLiquidatableNow
            | fadBuffer <= 0 = FadWouldLiquidate
            | maybe False (<= 50) distToFadBps = FadCritical
            | maybe False (<= 150) distToFadBps = FadAtRisk
            | otherwise = FadSafe
       in Just
            FadPositionRisk
              { fprAccount = T.toLower account
              , fprDiscordUsername = mDiscord
              , fprSide = side
              , fprSize = size
              , fprNotionalAssets = notional
              , fprMarginAssets = alsMargin snap
              , fprTraderClaimAssets = alsTraderClaimBalanceAssets snap
              , fprUnrealizedPnlAssets = alsUnrealizedPnlAssets snap
              , fprCurrentEquityAssets = currentEquity
              , fprProjectedCarryToFadAssets = projectedCarry
              , fprProjectedEquityAtFadAssets = projectedEquityAtFad
              , fprNormalRequiredMarginAssets = normalReq
              , fprFadRequiredMarginAssets = fadReq
              , fprFadMarginBufferAssets = fadBuffer
              , fprMarginNeededToSurviveAssets = marginNeeded
              , fprEffectiveLeverageX100 = effLevX100
              , fprEntryPrice = alsEntryPrice snap
              , fprMarkPrice = clampedMark
              , fprNormalLiquidationPrice = normalLiqPrice
              , fprFadLiquidationPrice = fadLiqPrice
              , fprDistanceToFadLiquidationBps = distToFadBps
              , fprLiquidatableNow = alsLiquidatable snap
              , fprDangerStatus = danger
              }

aggregateFadRiskSummary :: [FadPositionRisk] -> FadRiskSummary
aggregateFadRiskSummary = foldl' step emptySummary
  where
    emptySummary = FadRiskSummary 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    step acc p =
      let isImmediate = fprDangerStatus p `elem` [FadLiquidatableNow, FadWouldLiquidate]
          isEndangered = fprDangerStatus p /= FadSafe
          n = fprNotionalAssets p
          isLong = fprSide p == 0
       in acc
            { frsTotalOpenPositions = frsTotalOpenPositions acc + 1
            , frsLiquidatableNowCount =
                frsLiquidatableNowCount acc + if fprDangerStatus p == FadLiquidatableNow then 1 else 0
            , frsWouldLiquidateCount =
                frsWouldLiquidateCount acc + if fprDangerStatus p == FadWouldLiquidate then 1 else 0
            , frsCriticalCount =
                frsCriticalCount acc + if fprDangerStatus p == FadCritical then 1 else 0
            , frsAtRiskCount =
                frsAtRiskCount acc + if fprDangerStatus p == FadAtRisk then 1 else 0
            , frsSafeCount =
                frsSafeCount acc + if fprDangerStatus p == FadSafe then 1 else 0
            , frsEndangeredCount =
                frsEndangeredCount acc + if isEndangered then 1 else 0
            , frsImmediateLiquidationNotionalAssets =
                frsImmediateLiquidationNotionalAssets acc + if isImmediate then n else 0
            , frsImmediateLiquidationLongNotionalAssets =
                frsImmediateLiquidationLongNotionalAssets acc + if isImmediate && isLong then n else 0
            , frsImmediateLiquidationShortNotionalAssets =
                frsImmediateLiquidationShortNotionalAssets acc + if isImmediate && not isLong then n else 0
            , frsTotalEndangeredNotionalAssets =
                frsTotalEndangeredNotionalAssets acc + if isEndangered then n else 0
            , frsEndangeredLongNotionalAssets =
                frsEndangeredLongNotionalAssets acc + if isEndangered && isLong then n else 0
            , frsEndangeredShortNotionalAssets =
                frsEndangeredShortNotionalAssets acc + if isEndangered && not isLong then n else 0
            , frsTotalMarginTopUpNeededAssets =
                frsTotalMarginTopUpNeededAssets acc + fprMarginNeededToSurviveAssets p
            }

-- | Decode CfdEngine.marketClosures(uint256) -> (uint64 startTime, uint64 endTime).
decodeMarketClosureTuple :: ByteString -> Maybe MarketClosure
decodeMarketClosureTuple bytes
  | BS.length bytes < 64 = Nothing
  | otherwise =
      Just
        MarketClosure
          { mcStartTime = decodeUint256 (BS.take 32 bytes)
          , mcEndTime = decodeUint256 (BS.take 32 (BS.drop 32 bytes))
          }

-- | Decode maxProfitAssets (word 3) from CfdEngine.positions(address).
decodePositionMaxProfitAssets :: ByteString -> Maybe Integer
decodePositionMaxProfitAssets bytes
  | BS.length bytes < 4 * 32 = Nothing
  | otherwise = Just (decodeUint256 (BS.take 32 (BS.drop (3 * 32) bytes)))

openAccountsForFadRiskSql :: Query
openAccountsForFadRiskSql =
  "WITH activity_open AS ( \
  \  SELECT LOWER(account) AS acct, \
  \    COALESCE(SUM(CASE WHEN activity_type = 'Open' THEN COALESCE(size_delta, 0) ELSE -COALESCE(size_delta, 0) END), 0) AS net_size \
  \  FROM perps_account_activity \
  \  WHERE chain_id = ? AND release_router = ? \
  \    AND activity_type IN ('Open', 'Close', 'Liquidated') \
  \    AND side IN (0, 1) \
  \  GROUP BY LOWER(account), side \
  \  HAVING COALESCE(SUM(CASE WHEN activity_type = 'Open' THEN COALESCE(size_delta, 0) ELSE -COALESCE(size_delta, 0) END), 0) > 0 \
  \), \
  \candidate_open AS ( \
  \  SELECT LOWER(account) AS acct, 0::NUMERIC AS net_size \
  \  FROM perps_liquidation_candidates \
  \  WHERE chain_id = ? AND LOWER(cfd_engine) = ? \
  \), \
  \combined AS ( \
  \  SELECT acct, MAX(net_size) AS sort_size \
  \  FROM (SELECT * FROM activity_open UNION ALL SELECT * FROM candidate_open) u \
  \  GROUP BY acct \
  \) \
  \SELECT acct FROM combined ORDER BY sort_size DESC, acct ASC LIMIT ?"

getMarketFadRiskResponse
  :: DbPool
  -> EthClient
  -> Config
  -> IO (Either ApiError (ApiResponse Value))
getMarketFadRiskResponse pool client cfg =
  snd
    <$> runSingleFlightCache
      marketFadRiskCache
      ()
      isRight
      (computeMarketFadRiskResponse pool client cfg)

computeMarketFadRiskResponse
  :: DbPool
  -> EthClient
  -> Config
  -> IO (Either ApiError (ApiResponse Value))
computeMarketFadRiskResponse pool client cfg = do
  now <- floor <$> getPOSIXTime
  let chainId = cfgPerpsChainId cfg
      router = T.toLower $ T.strip $ cfgPerpsOrderRouter cfg
      engineLower = T.toLower $ T.strip $ cfgPerpsCfdEngine cfg
  ((accounts, handlesMap, mDbMark), (mBlockNum, mConfig)) <-
    concurrently
      (fetchCandidateAccountsFromDb pool chainId router engineLower)
      (fetchFadConfigFromChain client)
  let blockNum = fromMaybe 0 mBlockNum
      mStatus = mConfig >>= fcProtocolStatus
      mRisk = mConfig >>= fcRiskParams
      maintBps = maybe defaultMaintenanceMarginBps lerMaintenanceMarginBps mRisk
      fadBps = maybe defaultFadMarginBps lerFadMarginBps mRisk
      baseCarryBps = maybe defaultBaseCarryBps lerBaseCarryBps mRisk
      fadRunwaySecs = fromMaybe 0 (mConfig >>= fcFadRunwaySeconds)
      closures = maybe [] fcMarketClosures mConfig
      onChainCalFlags = (\s -> (lpsFadWindow s, lpsOracleFrozen s)) <$> mStatus
      calSnap = buildMarketCalendarSnapshot now closures fadRunwaySecs onChainCalFlags 14
      markRaw =
        case (lpsLastMarkPrice <$> mStatus, mDbMark) of
          (Just p, _) | p > 0 -> normalizeRawMark p
          (_, Just p) | p > 0 -> normalizeRawMark p
          _ -> 100000000
      secsUntilFad = fromMaybe 0 (mcsSecondsUntilNextFad calSnap)
  accountReads <-
    if null accounts || blockNum <= 0
      then pure []
      else fetchAccountSnapshotsBatched client blockNum accounts
  let evaluated =
        sortOn (\p -> (fadDangerRank (fprDangerStatus p), fprFadMarginBufferAssets p, Down (fprNotionalAssets p))) $
          catMaybes
            [ evaluatePositionFadRisk
                acct
                (Map.lookup (T.toLower acct) handlesMap)
                snap
                mMaxProfit
                markRaw
                maintBps
                fadBps
                baseCarryBps
                secsUntilFad
            | (acct, snap, mMaxProfit) <- accountReads
            ]
      summary = aggregateFadRiskSummary evaluated
      thresholds =
        FadRiskThresholds
          { frtMaintenanceMarginBps = maintBps
          , frtFadMarginBps = fadBps
          , frtMaxFadLeverageX100 = if fadBps > 0 then (1000000 :: Integer) `div` fadBps else 0
          , frtBaseCarryBps = baseCarryBps
          , frtFadMaxStaleness = mConfig >>= fcFadMaxStaleness
          , frtEngineMarkStalenessLimit = mConfig >>= fcEngineMarkStalenessLimit
          }
  pure $
    Right $
      mkResponse blockNum chainId $
        object
          [ "network"
              .= object
                [ "chainId" .= chainId
                , "releaseVersion" .= Manifest.releaseVersion
                , "settlementAssetSymbol" .= ("USDG" :: Text)
                ]
          , "observedAt" .= isoTimestamp now
          , "observedBlock" .= show blockNum
          , "markPrice" .= show markRaw
          , "accountsScanned" .= length accounts
          , "calendar" .= calendarSnapshotToJson calSnap
          , "thresholds" .= thresholdsToJson thresholds
          , "summary" .= summaryToJson summary
          , "positions" .= map positionRiskToJson evaluated
          ]

data OnChainFadConfig = OnChainFadConfig
  { fcProtocolStatus :: Maybe LiveProtocolStatus
  , fcRiskParams :: Maybe LiveEngineRiskParams
  , fcFadRunwaySeconds :: Maybe Integer
  , fcFadMaxStaleness :: Maybe Integer
  , fcEngineMarkStalenessLimit :: Maybe Integer
  , fcMarketClosures :: [MarketClosure]
  }

fetchFadConfigFromChain :: EthClient -> IO (Maybe Integer, Maybe OnChainFadConfig)
fetchFadConfigFromChain client = do
  mBlock <- timeout 3000000 $ observeFadIo "eth_blockNumber" (ethBlockNumber client)
  case mBlock of
    Just (Just (Right blockNum)) -> do
      let engine = Manifest.cfdEngineAddress
          publicLens = Manifest.perpsPublicLensAddress
          mk target sig =
            Multicall.Call
              { Multicall.callTarget = target
              , Multicall.callAllowFailure = True
              , Multicall.callCalldata = selector sig
              }
          calls =
            [ mk publicLens "getProtocolStatus()"
            , mk engine "riskParams()"
            , mk engine "fadRunwaySeconds()"
            , mk engine "fadMaxStaleness()"
            , mk engine "engineMarkStalenessLimit()"
            , mk engine "marketClosureCount()"
            ]
      mRes <- timeout 4000000 $ observeFadIo "multicall_fad_config" (Multicall.multicallAtBlock client calls blockNum)
      case mRes of
        Just (Just (Right [rStatus, rRisk, rRunway, rFadStale, rMarkStale, rCount])) -> do
          let decWith f r = if Multicall.resultSuccess r then f (Multicall.resultData r) else Nothing
              mStatus = decWith decodeProtocolStatus rStatus
              mRisk = decWith decodeEngineRiskParams rRisk
              mRunway = decWith decodeSingleUint rRunway
              mFadStale = decWith decodeSingleUint rFadStale
              mMarkStale = decWith decodeSingleUint rMarkStale
              closureCount = min 16 (fromInteger (fromMaybe 0 (decWith decodeSingleUint rCount)))
          closures <-
            if closureCount > 0
              then fetchMarketClosures client blockNum engine closureCount
              else pure []
          pure
            ( Just blockNum
            , Just
                OnChainFadConfig
                  { fcProtocolStatus = mStatus
                  , fcRiskParams = mRisk
                  , fcFadRunwaySeconds = mRunway
                  , fcFadMaxStaleness = mFadStale
                  , fcEngineMarkStalenessLimit = mMarkStale
                  , fcMarketClosures = closures
                  }
            )
        _ -> pure (Just blockNum, Nothing)
    _ -> pure (Nothing, Nothing)

fetchMarketClosures :: EthClient -> Integer -> Text -> Int -> IO [MarketClosure]
fetchMarketClosures client blockNum engine count = do
  let calls =
        [ Multicall.Call
            { Multicall.callTarget = engine
            , Multicall.callAllowFailure = True
            , Multicall.callCalldata = encodeCall "marketClosures(uint256)" [encodeUint256 (toInteger i)]
            }
        | i <- [0 .. count - 1]
        ]
  mRes <- timeout 3000000 $ observeFadIo "multicall_market_closures" (Multicall.multicallAtBlock client calls blockNum)
  pure $ case mRes of
    Just (Just (Right rs)) ->
      catMaybes
        [ if Multicall.resultSuccess r then decodeMarketClosureTuple (Multicall.resultData r) else Nothing
        | r <- rs
        ]
    _ -> []

fetchCandidateAccountsFromDb
  :: DbPool
  -> Integer
  -> Text
  -> Text
  -> IO ([Text], Map.Map Text Text, Maybe Integer)
fetchCandidateAccountsFromDb pool chainId router engineLower = do
  mAccts <- observeFadIo "db_open_accounts" $ withDb pool $ \conn ->
    withReadSnapshot conn $ do
      acctRows <-
        query
          conn
          openAccountsForFadRiskSql
          (chainId, router, chainId, engineLower, maxAccountsScanned)
          :: IO [Only Text]
      pure $ nub [T.toLower (T.strip (fromOnly r)) | r <- acctRows, not (T.null (T.strip (fromOnly r)))]
  let accts = fromMaybe [] mAccts
  mHandles <-
    if null accts
      then pure (Just Map.empty)
      else observeFadIo "db_discord_handles" $ withDb pool $ \conn ->
        withReadSnapshot conn $ do
          hRows <-
            query
              conn
              "SELECT trading_account, discord_username FROM insights_divisions_links WHERE trading_account IN ?"
              (Only (In accts))
              :: IO [(Text, Text)]
          pure $ Map.fromList [(T.toLower a, T.strip h) | (a, h) <- hRows, not (T.null (T.strip h))]
  mMark <- observeFadIo "db_latest_basket_mark" $ withDb pool $ \conn ->
    withReadSnapshot conn $ do
      markRows <- query_ conn latestBasketMarkSql :: IO [Only Text]
      pure (listToMaybe markRows >>= readMaybe . T.unpack . T.strip . fromOnly)
  pure (accts, fromMaybe Map.empty mHandles, fromMaybe Nothing mMark)

fetchAccountSnapshotsBatched
  :: EthClient
  -> Integer
  -> [Text]
  -> IO [(Text, AccountLedgerSnapshot, Maybe Integer)]
fetchAccountSnapshotsBatched client blockNum accounts = do
  let accountLens = Manifest.cfdEngineAccountLensAddress
      engine = Manifest.cfdEngineAddress
      chunks = chunkList accountChunkSize accounts
      runChunk accts = do
        let calls =
              concatMap
                ( \a ->
                    [ Multicall.Call
                        { Multicall.callTarget = accountLens
                        , Multicall.callAllowFailure = True
                        , Multicall.callCalldata = getAccountLedgerSnapshotCall a
                        }
                    , Multicall.Call
                        { Multicall.callTarget = engine
                        , Multicall.callAllowFailure = True
                        , Multicall.callCalldata = encodeCall "positions(address)" [encodeAddress a]
                        }
                    ]
                )
                accts
        mRes <- timeout 4000000 $ observeFadIo "multicall_account_snapshots" (Multicall.multicallAtBlock client calls blockNum)
        pure $ case mRes of
          Just (Just (Right rs)) -> pairResults accts rs
          _ -> []
  concat <$> traverse runChunk chunks
  where
    pairResults (a : as) (rSnap : rPos : rs) =
      let mSnap =
            if Multicall.resultSuccess rSnap
              then either (const Nothing) Just (decodeAccountLedgerSnapshot (Multicall.resultData rSnap))
              else Nothing
          mMaxProfit =
            if Multicall.resultSuccess rPos
              then decodePositionMaxProfitAssets (Multicall.resultData rPos)
              else Nothing
          rest = pairResults as rs
       in case mSnap of
            Just snap -> (a, snap, mMaxProfit) : rest
            Nothing -> rest
    pairResults _ _ = []

chunkList :: Int -> [a] -> [[a]]
chunkList n xs
  | n <= 0 || null xs = []
  | otherwise =
      let (h, t) = splitAt n xs
       in h : chunkList n t

normalizeRawMark :: Integer -> Integer
normalizeRawMark raw
  | raw > plDxyCapPrice =
      let scaled = raw `div` 10000000000
       in if scaled > 0 && scaled <= plDxyCapPrice then scaled else 100000000
  | raw > 0 = raw
  | otherwise = 100000000

calendarSnapshotToJson :: MarketCalendarSnapshot -> Value
calendarSnapshotToJson MarketCalendarSnapshot {..} =
  object
    [ "regime" .= marketRegimeText mcsRegime
    , "fadWindow" .= mcsFadWindow
    , "oracleFrozen" .= mcsOracleFrozen
    , "isNewYorkDst" .= mcsIsNewYorkDst
    , "nextFadStartsAt" .= mcsNextFadStartsAt
    , "nextFadStartsAtUtc" .= (isoTimestamp <$> mcsNextFadStartsAt)
    , "nextFreezeStartsAt" .= mcsNextFreezeStartsAt
    , "nextFreezeStartsAtUtc" .= (isoTimestamp <$> mcsNextFreezeStartsAt)
    , "nextReopenAt" .= mcsNextReopenAt
    , "nextReopenAtUtc" .= (isoTimestamp <$> mcsNextReopenAt)
    , "currentFadEndsAt" .= mcsCurrentFadEndsAt
    , "currentFadEndsAtUtc" .= (isoTimestamp <$> mcsCurrentFadEndsAt)
    , "secondsUntilNextFad" .= mcsSecondsUntilNextFad
    , "secondsUntilNextFreeze" .= mcsSecondsUntilNextFreeze
    , "fadRunwaySeconds" .= mcsFadRunwaySeconds
    , "scheduledClosures"
        .= map
          ( \c ->
              object
                [ "startTime" .= mcStartTime c
                , "startTimeUtc" .= isoTimestamp (mcStartTime c)
                , "endTime" .= mcEndTime c
                , "endTimeUtc" .= isoTimestamp (mcEndTime c)
                ]
          )
          mcsScheduledClosures
    , "upcomingWindows"
        .= map
          ( \w ->
              object
                [ "kind" .= umwKind w
                , "label" .= umwLabel w
                , "fadStartsAt" .= umwFadStartsAt w
                , "fadStartsAtUtc" .= isoTimestamp (umwFadStartsAt w)
                , "freezeStartsAt" .= umwFreezeStartsAt w
                , "freezeStartsAtUtc" .= isoTimestamp (umwFreezeStartsAt w)
                , "freezeEndsAt" .= umwFreezeEndsAt w
                , "freezeEndsAtUtc" .= isoTimestamp (umwFreezeEndsAt w)
                , "fadEndsAt" .= umwFadEndsAt w
                , "fadEndsAtUtc" .= isoTimestamp (umwFadEndsAt w)
                ]
          )
          mcsUpcomingWindows
    ]

thresholdsToJson :: FadRiskThresholds -> Value
thresholdsToJson FadRiskThresholds {..} =
  object
    [ "maintenanceMarginBps" .= frtMaintenanceMarginBps
    , "fadMarginBps" .= frtFadMarginBps
    , "maxFadLeverageX100" .= frtMaxFadLeverageX100
    , "baseCarryBps" .= frtBaseCarryBps
    , "fadMaxStaleness" .= frtFadMaxStaleness
    , "engineMarkStalenessLimit" .= frtEngineMarkStalenessLimit
    ]

summaryToJson :: FadRiskSummary -> Value
summaryToJson FadRiskSummary {..} =
  object
    [ "totalOpenPositions" .= frsTotalOpenPositions
    , "liquidatableNowCount" .= frsLiquidatableNowCount
    , "wouldLiquidateCount" .= frsWouldLiquidateCount
    , "criticalCount" .= frsCriticalCount
    , "atRiskCount" .= frsAtRiskCount
    , "safeCount" .= frsSafeCount
    , "endangeredCount" .= frsEndangeredCount
    , "immediateLiquidationNotionalAssets" .= show frsImmediateLiquidationNotionalAssets
    , "immediateLiquidationLongNotionalAssets" .= show frsImmediateLiquidationLongNotionalAssets
    , "immediateLiquidationShortNotionalAssets" .= show frsImmediateLiquidationShortNotionalAssets
    , "totalEndangeredNotionalAssets" .= show frsTotalEndangeredNotionalAssets
    , "endangeredLongNotionalAssets" .= show frsEndangeredLongNotionalAssets
    , "endangeredShortNotionalAssets" .= show frsEndangeredShortNotionalAssets
    , "totalMarginTopUpNeededAssets" .= show frsTotalMarginTopUpNeededAssets
    ]

positionRiskToJson :: FadPositionRisk -> Value
positionRiskToJson FadPositionRisk {..} =
  object
    [ "account" .= fprAccount
    , "discordUsername" .= fprDiscordUsername
    , "side" .= (if fprSide == 0 then ("long" :: Text) else "short")
    , "size" .= show fprSize
    , "notionalAssets" .= show fprNotionalAssets
    , "marginAssets" .= show fprMarginAssets
    , "traderClaimAssets" .= show fprTraderClaimAssets
    , "unrealizedPnlAssets" .= show fprUnrealizedPnlAssets
    , "currentEquityAssets" .= show fprCurrentEquityAssets
    , "projectedCarryToFadAssets" .= show fprProjectedCarryToFadAssets
    , "projectedEquityAtFadAssets" .= show fprProjectedEquityAtFadAssets
    , "normalRequiredMarginAssets" .= show fprNormalRequiredMarginAssets
    , "fadRequiredMarginAssets" .= show fprFadRequiredMarginAssets
    , "fadMarginBufferAssets" .= show fprFadMarginBufferAssets
    , "marginNeededToSurviveAssets" .= show fprMarginNeededToSurviveAssets
    , "effectiveLeverageX100" .= fprEffectiveLeverageX100
    , "entryPrice" .= show fprEntryPrice
    , "markPrice" .= show fprMarkPrice
    , "normalLiquidationPrice" .= (show <$> fprNormalLiquidationPrice)
    , "fadLiquidationPrice" .= (show <$> fprFadLiquidationPrice)
    , "distanceToFadLiquidationBps" .= fprDistanceToFadLiquidationBps
    , "liquidatableNow" .= fprLiquidatableNow
    , "dangerStatus" .= fadDangerStatusText fprDangerStatus
    ]

observeFadIo :: Text -> IO a -> IO (Maybe a)
observeFadIo opName action = do
  result <- try action
  case result of
    Right val -> pure (Just val)
    Left (err :: SomeException) ->
      case fromException @SomeAsyncException err of
        Just asyncErr -> throwIO asyncErr
        Nothing -> do
          Log.logWarnEvery
            60
            ("insights_fad_risk_" <> opName)
            "Insights FAD risk read degraded"
            [ Log.field "operation" opName
            , Log.field "error" (T.pack (show err))
            ]
          pure Nothing

withReadSnapshot :: Connection -> IO a -> IO a
withReadSnapshot conn =
  withTransactionMode
    TransactionMode
      { isolationLevel = RepeatableRead
      , readWriteMode = ReadOnly
      }
    conn

formatUtcTime :: UTCTime -> Text
formatUtcTime = T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

isoTimestamp :: Integer -> Text
isoTimestamp = formatUtcTime . posixSecondsToUTCTime . fromIntegral
