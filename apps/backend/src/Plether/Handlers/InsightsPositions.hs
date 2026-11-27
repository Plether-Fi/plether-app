{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Plether.Handlers.InsightsPositions
  ( PositionActivityRow (..)
  , PriceCandleRow (..)
  , PositionProtectionEventRow (..)
  , PositionStepEvent (..)
  , PositionLifecycle (..)
  , ExposureTimelinePoint (..)
  , WalletExposureSummary (..)
  , PositionPricePoint (..)
  , GlobalOpenPosition (..)
  , GlobalMarketExposure (..)
  , plDxyCapPrice
  , defaultMaintenanceMarginBps
  , computeNotionalAssets
  , computeUnrealizedPnl
  , computeLeverageX100
  , findLiquidationRawPrice
  , distanceToLiquidationBps
  , pickCandleIntervalSeconds
  , reconstructPositionLifecycles
  , enrichPositionWithCandles
  , buildExposureTimeline
  , summarizeWalletExposure
  , computeGlobalMarketExposure
  , getWalletExposureResponse
  , getWalletPositionDetailResponse
  , getMarketExposureResponse
  ) where

import Control.Applicative ((<|>))
import Control.Concurrent.Async (concurrently)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Char8 as BS8
import Data.Either (isRight)
import Data.List (sort, sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, listToMaybe, mapMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (defaultTimeLocale, formatTime)
import Data.Time.Clock.POSIX (getPOSIXTime, posixSecondsToUTCTime)
import Database.PostgreSQL.Simple (Connection, In (..), Only (..), query)
import Database.PostgreSQL.Simple.FromField (ResultError (ConversionFailed), returnError)
import Database.PostgreSQL.Simple.FromRow (FromRow (..), RowParser, field, fieldWith)
import Database.PostgreSQL.Simple.Transaction
  ( IsolationLevel (RepeatableRead)
  , ReadWriteMode (ReadOnly)
  , TransactionMode (..)
  , withTransactionMode
  )
import Plether.Cache (SingleFlightCache, newRefreshingSingleFlightCache, runSingleFlightCache)
import Plether.Config (Config (..))
import Plether.Database (DbPool, withDb)
import Plether.Database.Candles (getActiveBasketSeriesId)
import Plether.Ethereum.Client (EthClient)
import Plether.Ethereum.Contracts.CfdEngineAccountLens
  ( AccountLedgerSnapshot (..)
  , getAccountLedgerSnapshot
  )
import Plether.Handlers.InsightsOrderProtections
  ( OrderProtectionsBundle
  , buildOrderProtectionsForOrder
  , extractExecutionOrderId
  , fetchOrderProtectionsBundle
  )
import Plether.Handlers.InsightsWallets (fetchDiscordHandle)
import qualified Plether.Perps.Manifest as Manifest
import Plether.Types (ApiError, ApiResponse, mkResponse)
import qualified Plether.Types.Error as E
import System.IO.Unsafe (unsafePerformIO)
import Text.Read (readMaybe)

-- | Canonical plDXY price cap in 1e8 precision (2.00000000).
-- Display price is (plDxyCapPrice - rawPrice) / 1e8.
plDxyCapPrice :: Integer
plDxyCapPrice = 200000000

-- | Size (1e18) * rawPrice (1e8) / 1e20 = USDG assets (1e6).
sizeToAssetDivisor :: Integer
sizeToAssetDivisor = 100000000000000000000

-- | Canonical release maintenance margin (10 bps = 0.10%).
defaultMaintenanceMarginBps :: Integer
defaultMaintenanceMarginBps = 10

data PositionActivityRow = PositionActivityRow
  { parActivityType :: Text
  , parOrderId :: Maybe Integer
  , parSide :: Maybe Int
  , parPrice :: Maybe Integer
  , parSizeDelta :: Maybe Integer
  , parAmountAssets :: Maybe Integer
  , parPnlAssets :: Maybe Integer
  , parFeeAssets :: Maybe Integer
  , parVpiAssets :: Maybe Integer
  , parKeeperBountyAssets :: Maybe Integer
  , parTxHash :: Text
  , parBlockNumber :: Integer
  , parLogIndex :: Integer
  , parTimestamp :: Integer
  , parExecution :: Maybe Value
  }
  deriving stock (Eq, Show)

instance FromRow PositionActivityRow where
  fromRow =
    PositionActivityRow
      <$> field
      <*> field
      <*> field
      <*> nullableNumericIntegerField
      <*> nullableNumericIntegerField
      <*> nullableNumericIntegerField
      <*> nullableNumericIntegerField
      <*> nullableNumericIntegerField
      <*> nullableNumericIntegerField
      <*> nullableNumericIntegerField
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field

data PriceCandleRow = PriceCandleRow
  { pcrBucketStart :: Integer
  , pcrRawOpen :: Integer
  , pcrRawHigh :: Integer
  , pcrRawLow :: Integer
  , pcrRawClose :: Integer
  }
  deriving stock (Eq, Show)

instance FromRow PriceCandleRow where
  fromRow =
    PriceCandleRow
      <$> field
      <*> field
      <*> field
      <*> field
      <*> field

data PositionProtectionEventRow = PositionProtectionEventRow
  { pperProtectionId :: Text
  , pperBlockNumber :: Integer
  , pperLogIndex :: Integer
  , pperTxHash :: Text
  , pperEventName :: Text
  , pperEventData :: Value
  , pperSnapshot :: Value
  }
  deriving stock (Eq, Show)

instance FromRow PositionProtectionEventRow where
  fromRow =
    PositionProtectionEventRow
      <$> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field

data PositionStepEvent = PositionStepEvent
  { pseId :: Text
  , pseAction :: Text
  , pseRawActivityType :: Text
  , pseOrderId :: Maybe Integer
  , pseTxHash :: Text
  , pseBlockNumber :: Integer
  , pseLogIndex :: Integer
  , pseTimestamp :: Integer
  , psePrice :: Maybe Integer
  , pseSizeDelta :: Integer
  , pseResultingSize :: Integer
  , pseResultingEntryPrice :: Integer
  , pseResultingEntryCostAssets :: Integer
  , pseMarginDeltaAssets :: Integer
  , pseResultingMarginAssets :: Integer
  , pseResultingNotionalAssets :: Integer
  , pseResultingLeverageX100 :: Integer
  , pseResultingLiquidationPrice :: Maybe Integer
  , psePnlAssets :: Maybe Integer
  , pseFeeAssets :: Maybe Integer
  , pseVpiAssets :: Maybe Integer
  , pseKeeperBountyAssets :: Maybe Integer
  }
  deriving stock (Eq, Show)

data PositionLifecycle = PositionLifecycle
  { plPositionId :: Text
  , plStatus :: Text -- "open" | "closed" | "liquidated"
  , plSide :: Int -- 0 = long, 1 = short
  , plOpenedAt :: Integer
  , plClosedAt :: Maybe Integer
  , plOpenBlock :: Integer
  , plCloseBlock :: Maybe Integer
  , plOpenTxHash :: Text
  , plCloseTxHash :: Maybe Text
  , plOpenOrderId :: Maybe Integer
  , plEntryPrice :: Integer -- weighted avg raw entry price (1e8)
  , plExitPrice :: Maybe Integer -- weighted avg raw exit price (1e8)
  , plEntryCostAssets :: Integer
  , plCurrentSize :: Integer
  , plCurrentMarginAssets :: Integer
  , plPeakSize :: Integer
  , plPeakNotionalAssets :: Integer
  , plPeakMarginAssets :: Integer
  , plEntryLeverageX100 :: Integer
  , plMaxLeverageX100 :: Integer
  , plCurrentLeverageX100 :: Integer
  , plRealizedPnlAssets :: Integer
  , plUnrealizedPnlAssets :: Integer
  , plTotalFeesAssets :: Integer
  , plTotalVpiAssets :: Integer
  , plTotalKeeperBountyAssets :: Integer
  , plLiquidationPrice :: Maybe Integer
  , plLiquidatable :: Bool
  , plMfePnlAssets :: Integer
  , plMfeBps :: Integer
  , plMfePrice :: Maybe Integer
  , plMfeTimestamp :: Maybe Integer
  , plMaePnlAssets :: Integer
  , plMaeBps :: Integer
  , plMaePrice :: Maybe Integer
  , plMaeTimestamp :: Maybe Integer
  , plClosestLiquidationBps :: Maybe Integer
  , plClosestLiquidationAt :: Maybe Integer
  , plCurrentLiquidationBps :: Maybe Integer
  , plFillCount :: Int
  , plScaleInCount :: Int
  , plPartialCloseCount :: Int
  , plMarginAddCount :: Int
  , plEvents :: [PositionStepEvent]
  }
  deriving stock (Eq, Show)

data PositionPricePoint = PositionPricePoint
  { pppTimestamp :: Integer
  , pppRawPrice :: Integer
  , pppRawHigh :: Integer
  , pppRawLow :: Integer
  , pppUnrealizedPnlAssets :: Integer
  , pppPositionSize :: Integer
  , pppLeverageX100 :: Integer
  , pppLiquidationPrice :: Maybe Integer
  , pppDistanceToLiquidationBps :: Maybe Integer
  }
  deriving stock (Eq, Show)

data ExposureTimelinePoint = ExposureTimelinePoint
  { etpTimestamp :: Integer
  , etpBlockNumber :: Integer
  , etpSignedNotionalAssets :: Integer
  , etpAbsNotionalAssets :: Integer
  , etpMarginAssets :: Integer
  , etpLeverageX100 :: Integer
  , etpSide :: Text
  , etpEventType :: Text
  , etpPositionId :: Maybe Text
  }
  deriving stock (Eq, Show)

data WalletExposureSummary = WalletExposureSummary
  { wesTotalPositions :: Int
  , wesOpenPositions :: Int
  , wesClosedPositions :: Int
  , wesLiquidatedPositions :: Int
  , wesWinCount :: Int
  , wesLossCount :: Int
  , wesWinRateBps :: Maybe Integer
  , wesLongPositions :: Int
  , wesShortPositions :: Int
  , wesLongNotionalAssets :: Integer
  , wesShortNotionalAssets :: Integer
  , wesLongNotionalShareBps :: Maybe Integer
  , wesShortNotionalShareBps :: Maybe Integer
  , wesLongRealizedPnlAssets :: Integer
  , wesShortRealizedPnlAssets :: Integer
  , wesLongHoldSeconds :: Integer
  , wesShortHoldSeconds :: Integer
  , wesAvgEntryLeverageX100 :: Maybe Integer
  , wesMaxLeverageX100 :: Integer
  , wesCurrentLeverageX100 :: Integer
  , wesAvgHoldSeconds :: Maybe Integer
  , wesMedianHoldSeconds :: Maybe Integer
  , wesAvgNotionalAssets :: Maybe Integer
  , wesPeakNotionalAssets :: Integer
  , wesAvgMfeAssets :: Maybe Integer
  , wesAvgMaeAssets :: Maybe Integer
  , wesTightestLiquidationBps :: Maybe Integer
  }
  deriving stock (Eq, Show)

-- | Compute position notional in USDG 1e6 units from size (1e18) and raw price (1e8).
computeNotionalAssets :: Integer -> Integer -> Integer
computeNotionalAssets size rawPrice
  | size <= 0 || rawPrice <= 0 = 0
  | otherwise =
      let clamped = min plDxyCapPrice rawPrice
       in (size * clamped) `div` sizeToAssetDivisor

-- | Compute exact unrealized PnL in USDG 1e6 units using canonical inverted plDXY pricing.
computeUnrealizedPnl :: Int -> Integer -> Integer -> Integer -> Integer
computeUnrealizedPnl side size entryCostAssets rawPrice
  | size <= 0 = 0
  | otherwise =
      let curNotional = computeNotionalAssets size rawPrice
       in if side == 0
            then entryCostAssets - curNotional
            else curNotional - entryCostAssets

-- | Compute effective leverage in hundredths of x (e.g., 1250 = 12.50x).
computeLeverageX100 :: Integer -> Integer -> Integer
computeLeverageX100 notionalAssets marginAssets
  | notionalAssets <= 0 || marginAssets <= 0 = 0
  | otherwise = (notionalAssets * 100) `div` marginAssets

-- | Exact binary search for liquidation raw price (1e8), matching
-- PositionRiskAccountingLib.buildExactPriceRiskState and perpsRisk.ts.
findLiquidationRawPrice
  :: Int
  -> Integer
  -> Integer
  -> Integer
  -> Integer
  -> Integer
  -> Maybe Integer
findLiquidationRawPrice side size entryCostAssets marginAssets claimAssets maintBps
  | size <= 0 || (side /= 0 && side /= 1) || entryCostAssets < 0 || marginAssets < 0 || claimAssets < 0 = Nothing
  | maintBps < 0 || maintBps > 10000 = Nothing
  | not (if isLong then isLiq plDxyCapPrice else isLiq 0) = Nothing
  | if isLong then isLiq 0 else isLiq plDxyCapPrice =
      Just (if isLong then 0 else plDxyCapPrice)
  | otherwise = Just (go 0 plDxyCapPrice)
  where
    isLong = side == 0
    isLiq rawP =
      let curNotional = computeNotionalAssets size rawP
          pnl = if isLong then entryCostAssets - curNotional else curNotional - entryCostAssets
          equity = marginAssets + claimAssets + pnl
          maint = (curNotional * maintBps) `div` 10000
       in equity <= maint
    go lo hi
      | lo >= hi = lo
      | isLong =
          let mid = (lo + hi) `div` 2
           in if isLiq mid then go lo mid else go (mid + 1) hi
      | otherwise =
          let mid = (lo + hi + 1) `div` 2
           in if isLiq mid then go mid hi else go lo (mid - 1)

-- | Distance from a raw mark price to liquidation raw price, measured in display-price bps.
distanceToLiquidationBps :: Int -> Integer -> Maybe Integer -> Maybe Integer
distanceToLiquidationBps side markRaw mLiqRaw = do
  liqRaw <- mLiqRaw
  let markDisplay = max 0 (plDxyCapPrice - min plDxyCapPrice (max 0 markRaw))
      liqDisplay = max 0 (plDxyCapPrice - min plDxyCapPrice (max 0 liqRaw))
  if markDisplay <= 0
    then Just 0
    else
      let buffer =
            if side == 0
              then max 0 (markDisplay - liqDisplay)
              else max 0 (liqDisplay - markDisplay)
       in Just ((buffer * 10000) `div` markDisplay)

-- | Select an appropriate canonical candle interval (in seconds) for a time span.
pickCandleIntervalSeconds :: Integer -> Integer
pickCandleIntervalSeconds spanSeconds
  | spanSeconds <= 4 * 3600 = 60
  | spanSeconds <= 24 * 3600 = 300
  | spanSeconds <= 7 * 86400 = 900
  | spanSeconds <= 30 * 86400 = 3600
  | otherwise = 86400

data ActiveDraft = ActiveDraft
  { adPositionId :: Text
  , adSide :: Int
  , adOpenedAt :: Integer
  , adOpenBlock :: Integer
  , adOpenTxHash :: Text
  , adOpenOrderId :: Maybe Integer
  , adSize :: Integer
  , adEntryCostAssets :: Integer
  , adWeightedEntryRaw :: Integer
  , adMarginAssets :: Integer
  , adPeakSize :: Integer
  , adPeakNotionalAssets :: Integer
  , adPeakMarginAssets :: Integer
  , adEntryLeverageX100 :: Integer
  , adMaxLeverageX100 :: Integer
  , adRealizedPnlAssets :: Integer
  , adTotalFeesAssets :: Integer
  , adTotalVpiAssets :: Integer
  , adTotalKeeperBountyAssets :: Integer
  , adClosedSizeTotal :: Integer
  , adClosedNotionalTotal :: Integer
  , adFillCount :: Int
  , adScaleInCount :: Int
  , adPartialCloseCount :: Int
  , adMarginAddCount :: Int
  , adEventsRev :: [PositionStepEvent]
  }

-- | Reconstruct chronological position lifecycles (open -> scale-ins / margin adds /
-- partial closes -> close or liquidation) from ordered perps_account_activity rows,
-- reconciling any still-open position against the live on-chain AccountLedgerSnapshot.
reconstructPositionLifecycles
  :: Integer
  -> Maybe Integer
  -> Maybe AccountLedgerSnapshot
  -> [PositionActivityRow]
  -> [PositionLifecycle]
reconstructPositionLifecycles nowUnix mMarkRaw mLedger rows =
  let ordered = sortOn (\r -> (parBlockNumber r, parLogIndex r)) rows
      (finishedRev, mDraft) = foldl stepRow ([], Nothing) ordered
      finalList = case mDraft of
        Nothing -> finishedRev
        Just draft -> finalizeOpenDraft nowUnix mMarkRaw mLedger draft : finishedRev
      -- If the live ledger reports an open position that had no indexed Open row yet,
      -- synthesize a minimal live-only position so open exposure is never hidden.
      withLiveFallback =
        case (mDraft, mLedger) of
          (Nothing, Just snap)
            | alsHasPosition snap && alsSize snap > 0 ->
                synthesizeLiveOnlyPosition nowUnix mMarkRaw snap : finalList
          _ -> finalList
   in sortOn (\p -> Down (plOpenedAt p, plOpenBlock p)) withLiveFallback
  where
    stepRow :: ([PositionLifecycle], Maybe ActiveDraft) -> PositionActivityRow -> ([PositionLifecycle], Maybe ActiveDraft)
    stepRow (done, mCur) row =
      case parActivityType row of
        "Open" -> handleOpen done mCur row
        "Add margin" -> handleAddMargin done mCur row
        "Close" -> handleClose done mCur row
        "Liquidated" -> handleLiquidated done mCur row
        _ -> (done, mCur)

    effectiveOrderId row = parOrderId row <|> extractExecutionOrderId (parExecution row)
    rowEventId row = parTxHash row <> ":" <> T.pack (show (parLogIndex row))

    handleOpen done Nothing row =
      let side = fromMaybe 0 (parSide row)
          sizeDelta = max 0 (fromMaybe 0 (parSizeDelta row))
          rawPrice = max 0 (fromMaybe 0 (parPrice row))
          marginDelta = max 0 (fromMaybe 0 (parAmountAssets row))
          notional = computeNotionalAssets sizeDelta rawPrice
          levX100 = computeLeverageX100 notional marginDelta
          liqRaw = findLiquidationRawPrice side sizeDelta notional marginDelta 0 defaultMaintenanceMarginBps
          posId = "pos-" <> T.pack (show (parBlockNumber row)) <> "-" <> T.pack (show (parLogIndex row))
          ev =
            PositionStepEvent
              { pseId = rowEventId row
              , pseAction = "Open"
              , pseRawActivityType = "Open"
              , pseOrderId = effectiveOrderId row
              , pseTxHash = parTxHash row
              , pseBlockNumber = parBlockNumber row
              , pseLogIndex = parLogIndex row
              , pseTimestamp = parTimestamp row
              , psePrice = Just rawPrice
              , pseSizeDelta = sizeDelta
              , pseResultingSize = sizeDelta
              , pseResultingEntryPrice = rawPrice
              , pseResultingEntryCostAssets = notional
              , pseMarginDeltaAssets = marginDelta
              , pseResultingMarginAssets = marginDelta
              , pseResultingNotionalAssets = notional
              , pseResultingLeverageX100 = levX100
              , pseResultingLiquidationPrice = liqRaw
              , psePnlAssets = parPnlAssets row
              , pseFeeAssets = parFeeAssets row
              , pseVpiAssets = parVpiAssets row
              , pseKeeperBountyAssets = parKeeperBountyAssets row
              }
          draft =
            ActiveDraft
              { adPositionId = posId
              , adSide = side
              , adOpenedAt = parTimestamp row
              , adOpenBlock = parBlockNumber row
              , adOpenTxHash = parTxHash row
              , adOpenOrderId = effectiveOrderId row
              , adSize = sizeDelta
              , adEntryCostAssets = notional
              , adWeightedEntryRaw = rawPrice
              , adMarginAssets = marginDelta
              , adPeakSize = sizeDelta
              , adPeakNotionalAssets = notional
              , adPeakMarginAssets = marginDelta
              , adEntryLeverageX100 = levX100
              , adMaxLeverageX100 = levX100
              , adRealizedPnlAssets = fromMaybe 0 (parPnlAssets row)
              , adTotalFeesAssets = fromMaybe 0 (parFeeAssets row)
              , adTotalVpiAssets = fromMaybe 0 (parVpiAssets row)
              , adTotalKeeperBountyAssets = fromMaybe 0 (parKeeperBountyAssets row)
              , adClosedSizeTotal = 0
              , adClosedNotionalTotal = 0
              , adFillCount = 1
              , adScaleInCount = 0
              , adPartialCloseCount = 0
              , adMarginAddCount = 0
              , adEventsRev = [ev]
              }
       in if sizeDelta > 0 then (done, Just draft) else (done, Nothing)
    handleOpen done (Just cur) row =
      let rowSide = fromMaybe (adSide cur) (parSide row)
       in if rowSide /= adSide cur
            then
              -- Side flipped without an explicit close row (defensive boundary); finalize prior and start fresh.
              let closedPrev = finalizeDraft "closed" (Just (parTimestamp row)) (Just (parBlockNumber row)) (Just (parTxHash row)) Nothing cur
               in handleOpen (closedPrev : done) Nothing row
            else
              let sizeDelta = max 0 (fromMaybe 0 (parSizeDelta row))
                  rawPrice = max 0 (fromMaybe 0 (parPrice row))
                  marginDelta = max 0 (fromMaybe 0 (parAmountAssets row))
                  addNotional = computeNotionalAssets sizeDelta rawPrice
                  newSize = adSize cur + sizeDelta
                  newEntryCost = adEntryCostAssets cur + addNotional
                  newEntryRaw = weightedRawPrice newSize newEntryCost rawPrice
                  newMargin = adMarginAssets cur + marginDelta
                  curNotional = computeNotionalAssets newSize rawPrice
                  levX100 = computeLeverageX100 curNotional newMargin
                  liqRaw = findLiquidationRawPrice (adSide cur) newSize newEntryCost newMargin 0 defaultMaintenanceMarginBps
                  ev =
                    PositionStepEvent
                      { pseId = rowEventId row
                      , pseAction = "Scale-in"
                      , pseRawActivityType = "Open"
                      , pseOrderId = effectiveOrderId row
                      , pseTxHash = parTxHash row
                      , pseBlockNumber = parBlockNumber row
                      , pseLogIndex = parLogIndex row
                      , pseTimestamp = parTimestamp row
                      , psePrice = Just rawPrice
                      , pseSizeDelta = sizeDelta
                      , pseResultingSize = newSize
                      , pseResultingEntryPrice = newEntryRaw
                      , pseResultingEntryCostAssets = newEntryCost
                      , pseMarginDeltaAssets = marginDelta
                      , pseResultingMarginAssets = newMargin
                      , pseResultingNotionalAssets = curNotional
                      , pseResultingLeverageX100 = levX100
                      , pseResultingLiquidationPrice = liqRaw
                      , psePnlAssets = parPnlAssets row
                      , pseFeeAssets = parFeeAssets row
                      , pseVpiAssets = parVpiAssets row
                      , pseKeeperBountyAssets = parKeeperBountyAssets row
                      }
                  updated =
                    cur
                      { adSize = newSize
                      , adEntryCostAssets = newEntryCost
                      , adWeightedEntryRaw = newEntryRaw
                      , adMarginAssets = newMargin
                      , adPeakSize = max (adPeakSize cur) newSize
                      , adPeakNotionalAssets = max (adPeakNotionalAssets cur) curNotional
                      , adPeakMarginAssets = max (adPeakMarginAssets cur) newMargin
                      , adMaxLeverageX100 = max (adMaxLeverageX100 cur) levX100
                      , adTotalFeesAssets = adTotalFeesAssets cur + fromMaybe 0 (parFeeAssets row)
                      , adTotalVpiAssets = adTotalVpiAssets cur + fromMaybe 0 (parVpiAssets row)
                      , adFillCount = adFillCount cur + 1
                      , adScaleInCount = adScaleInCount cur + 1
                      , adEventsRev = ev : adEventsRev cur
                      }
               in (done, Just updated)

    handleAddMargin done Nothing _ = (done, Nothing)
    handleAddMargin done (Just cur) row =
      let marginDelta = max 0 (fromMaybe 0 (parAmountAssets row))
          newMargin = adMarginAssets cur + marginDelta
          entryRaw = weightedRawPrice (adSize cur) (adEntryCostAssets cur) 0
          curNotional = adEntryCostAssets cur
          levX100 = computeLeverageX100 curNotional newMargin
          liqRaw = findLiquidationRawPrice (adSide cur) (adSize cur) (adEntryCostAssets cur) newMargin 0 defaultMaintenanceMarginBps
          ev =
            PositionStepEvent
              { pseId = rowEventId row
              , pseAction = "Add margin"
              , pseRawActivityType = "Add margin"
              , pseOrderId = effectiveOrderId row
              , pseTxHash = parTxHash row
              , pseBlockNumber = parBlockNumber row
              , pseLogIndex = parLogIndex row
              , pseTimestamp = parTimestamp row
              , psePrice = Nothing
              , pseSizeDelta = 0
              , pseResultingSize = adSize cur
              , pseResultingEntryPrice = entryRaw
              , pseResultingEntryCostAssets = adEntryCostAssets cur
              , pseMarginDeltaAssets = marginDelta
              , pseResultingMarginAssets = newMargin
              , pseResultingNotionalAssets = curNotional
              , pseResultingLeverageX100 = levX100
              , pseResultingLiquidationPrice = liqRaw
              , psePnlAssets = Nothing
              , pseFeeAssets = Nothing
              , pseVpiAssets = Nothing
              , pseKeeperBountyAssets = Nothing
              }
          updated =
            cur
              { adMarginAssets = newMargin
              , adPeakMarginAssets = max (adPeakMarginAssets cur) newMargin
              , adMarginAddCount = adMarginAddCount cur + 1
              , adEventsRev = ev : adEventsRev cur
              }
       in (done, Just updated)

    handleClose done Nothing _ = (done, Nothing)
    handleClose done (Just cur) row =
      let rawPrice = max 0 (fromMaybe 0 (parPrice row))
          sizeDelta = max 0 (fromMaybe (adSize cur) (parSizeDelta row))
          closedSize = min (adSize cur) sizeDelta
          remainingSize = max 0 (adSize cur - closedSize)
          isFullClose = remainingSize == 0
          actionLabel = if isFullClose then "Close" else "Partial close"
          remainingEntryCost =
            if isFullClose || adSize cur <= 0
              then 0
              else (adEntryCostAssets cur * remainingSize) `div` adSize cur
          remainingMargin =
            if isFullClose || adSize cur <= 0
              then 0
              else (adMarginAssets cur * remainingSize) `div` adSize cur
          marginDelta = remainingMargin - adMarginAssets cur
          closedNotional = computeNotionalAssets closedSize rawPrice
          remNotional = computeNotionalAssets remainingSize rawPrice
          levX100 = computeLeverageX100 remNotional remainingMargin
          liqRaw =
            if isFullClose
              then Nothing
              else findLiquidationRawPrice (adSide cur) remainingSize remainingEntryCost remainingMargin 0 defaultMaintenanceMarginBps
          entryRaw = weightedRawPrice (adSize cur) (adEntryCostAssets cur) rawPrice
          ev =
            PositionStepEvent
              { pseId = rowEventId row
              , pseAction = actionLabel
              , pseRawActivityType = "Close"
              , pseOrderId = effectiveOrderId row
              , pseTxHash = parTxHash row
              , pseBlockNumber = parBlockNumber row
              , pseLogIndex = parLogIndex row
              , pseTimestamp = parTimestamp row
              , psePrice = Just rawPrice
              , pseSizeDelta = negate closedSize
              , pseResultingSize = remainingSize
              , pseResultingEntryPrice = if isFullClose then entryRaw else weightedRawPrice remainingSize remainingEntryCost entryRaw
              , pseResultingEntryCostAssets = remainingEntryCost
              , pseMarginDeltaAssets = marginDelta
              , pseResultingMarginAssets = remainingMargin
              , pseResultingNotionalAssets = remNotional
              , pseResultingLeverageX100 = levX100
              , pseResultingLiquidationPrice = liqRaw
              , psePnlAssets = parPnlAssets row
              , pseFeeAssets = parFeeAssets row
              , pseVpiAssets = parVpiAssets row
              , pseKeeperBountyAssets = parKeeperBountyAssets row
              }
          updated =
            cur
              { adSize = remainingSize
              , adEntryCostAssets = if isFullClose then adEntryCostAssets cur else remainingEntryCost
              , adMarginAssets = remainingMargin
              , adRealizedPnlAssets = adRealizedPnlAssets cur + fromMaybe 0 (parPnlAssets row)
              , adTotalFeesAssets = adTotalFeesAssets cur + fromMaybe 0 (parFeeAssets row)
              , adTotalVpiAssets = adTotalVpiAssets cur + fromMaybe 0 (parVpiAssets row)
              , adClosedSizeTotal = adClosedSizeTotal cur + closedSize
              , adClosedNotionalTotal = adClosedNotionalTotal cur + closedNotional
              , adFillCount = adFillCount cur + 1
              , adPartialCloseCount = adPartialCloseCount cur + (if isFullClose then 0 else 1)
              , adEventsRev = ev : adEventsRev cur
              }
       in if isFullClose
            then
              let avgExit = weightedRawPrice (adClosedSizeTotal updated) (adClosedNotionalTotal updated) rawPrice
                  closed = finalizeDraft "closed" (Just (parTimestamp row)) (Just (parBlockNumber row)) (Just (parTxHash row)) (Just avgExit) updated
               in (closed : done, Nothing)
            else (done, Just updated)

    handleLiquidated done Nothing _ = (done, Nothing)
    handleLiquidated done (Just cur) row =
      let rawPrice = max 0 (fromMaybe 0 (parPrice row))
          sizeDelta = max 0 (fromMaybe (adSize cur) (parSizeDelta row))
          closedSize = min (adSize cur) sizeDelta
          closedNotional = computeNotionalAssets closedSize rawPrice
          bounty = fromMaybe 0 (parKeeperBountyAssets row <|> parAmountAssets row)
          estLiqPnl = fromMaybe (negate (adMarginAssets cur)) (parPnlAssets row)
          entryRaw = weightedRawPrice (adSize cur) (adEntryCostAssets cur) rawPrice
          ev =
            PositionStepEvent
              { pseId = rowEventId row
              , pseAction = "Liquidated"
              , pseRawActivityType = "Liquidated"
              , pseOrderId = effectiveOrderId row
              , pseTxHash = parTxHash row
              , pseBlockNumber = parBlockNumber row
              , pseLogIndex = parLogIndex row
              , pseTimestamp = parTimestamp row
              , psePrice = Just rawPrice
              , pseSizeDelta = negate closedSize
              , pseResultingSize = 0
              , pseResultingEntryPrice = entryRaw
              , pseResultingEntryCostAssets = 0
              , pseMarginDeltaAssets = negate (adMarginAssets cur)
              , pseResultingMarginAssets = 0
              , pseResultingNotionalAssets = 0
              , pseResultingLeverageX100 = 0
              , pseResultingLiquidationPrice = Just rawPrice
              , psePnlAssets = Just estLiqPnl
              , pseFeeAssets = parFeeAssets row
              , pseVpiAssets = parVpiAssets row
              , pseKeeperBountyAssets = Just bounty
              }
          updated =
            cur
              { adSize = 0
              , adMarginAssets = 0
              , adRealizedPnlAssets = adRealizedPnlAssets cur + estLiqPnl
              , adTotalKeeperBountyAssets = adTotalKeeperBountyAssets cur + bounty
              , adClosedSizeTotal = adClosedSizeTotal cur + closedSize
              , adClosedNotionalTotal = adClosedNotionalTotal cur + closedNotional
              , adFillCount = adFillCount cur + 1
              , adEventsRev = ev : adEventsRev cur
              }
          avgExit = weightedRawPrice (adClosedSizeTotal updated) (adClosedNotionalTotal updated) rawPrice
          closed = finalizeDraft "liquidated" (Just (parTimestamp row)) (Just (parBlockNumber row)) (Just (parTxHash row)) (Just avgExit) updated
       in (closed : done, Nothing)

    finalizeDraft status mClosedAt mCloseBlock mCloseTx mExitRaw draft =
      let events = reverse (adEventsRev draft)
          entryRaw = adWeightedEntryRaw draft
          lastLiq = listToMaybe $ mapMaybe pseResultingLiquidationPrice (adEventsRev draft)
       in PositionLifecycle
            { plPositionId = adPositionId draft
            , plStatus = status
            , plSide = adSide draft
            , plOpenedAt = adOpenedAt draft
            , plClosedAt = mClosedAt
            , plOpenBlock = adOpenBlock draft
            , plCloseBlock = mCloseBlock
            , plOpenTxHash = adOpenTxHash draft
            , plCloseTxHash = mCloseTx
            , plOpenOrderId = adOpenOrderId draft
            , plEntryPrice = entryRaw
            , plExitPrice = mExitRaw
            , plEntryCostAssets = adEntryCostAssets draft
            , plCurrentSize = 0
            , plCurrentMarginAssets = 0
            , plPeakSize = adPeakSize draft
            , plPeakNotionalAssets = adPeakNotionalAssets draft
            , plPeakMarginAssets = adPeakMarginAssets draft
            , plEntryLeverageX100 = adEntryLeverageX100 draft
            , plMaxLeverageX100 = adMaxLeverageX100 draft
            , plCurrentLeverageX100 = 0
            , plRealizedPnlAssets = adRealizedPnlAssets draft
            , plUnrealizedPnlAssets = 0
            , plTotalFeesAssets = adTotalFeesAssets draft
            , plTotalVpiAssets = adTotalVpiAssets draft
            , plTotalKeeperBountyAssets = adTotalKeeperBountyAssets draft
            , plLiquidationPrice = lastLiq
            , plLiquidatable = False
            , plMfePnlAssets = 0
            , plMfeBps = 0
            , plMfePrice = Nothing
            , plMfeTimestamp = Nothing
            , plMaePnlAssets = 0
            , plMaeBps = 0
            , plMaePrice = Nothing
            , plMaeTimestamp = Nothing
            , plClosestLiquidationBps = if status == "liquidated" then Just 0 else Nothing
            , plClosestLiquidationAt = if status == "liquidated" then mClosedAt else Nothing
            , plCurrentLiquidationBps = Nothing
            , plFillCount = adFillCount draft
            , plScaleInCount = adScaleInCount draft
            , plPartialCloseCount = adPartialCloseCount draft
            , plMarginAddCount = adMarginAddCount draft
            , plEvents = events
            }

    finalizeOpenDraft _now mMark mSnap draft =
      let events = reverse (adEventsRev draft)
          (effSize, effMargin, effEntryRaw, effEntryCost, effUpnl, effLiquidatable, effClaim) =
            case mSnap of
              Just snap
                | alsHasPosition snap && alsSize snap > 0 ->
                    let eRaw = normalizeLedgerEntryPrice (alsEntryPrice snap) (weightedRawPrice (adSize draft) (adEntryCostAssets draft) 0)
                        eCost = computeNotionalAssets (alsSize snap) eRaw
                     in (alsSize snap, alsMargin snap, eRaw, eCost, alsUnrealizedPnlAssets snap, alsLiquidatable snap, alsTraderClaimBalanceAssets snap)
              _ ->
                let eRaw = weightedRawPrice (adSize draft) (adEntryCostAssets draft) (fromMaybe 0 (listToMaybe events >>= psePrice))
                    upnl = maybe 0 (computeUnrealizedPnl (adSide draft) (adSize draft) (adEntryCostAssets draft)) mMark
                 in (adSize draft, adMarginAssets draft, eRaw, adEntryCostAssets draft, upnl, False, 0)
          effMark = fromMaybe effEntryRaw mMark
          curNotional = computeNotionalAssets effSize effMark
          curLevX100 = computeLeverageX100 curNotional effMargin
          liqRaw = findLiquidationRawPrice (adSide draft) effSize effEntryCost effMargin effClaim defaultMaintenanceMarginBps
          curLiqBps = distanceToLiquidationBps (adSide draft) effMark liqRaw
       in PositionLifecycle
            { plPositionId = adPositionId draft
            , plStatus = "open"
            , plSide = adSide draft
            , plOpenedAt = adOpenedAt draft
            , plClosedAt = Nothing
            , plOpenBlock = adOpenBlock draft
            , plCloseBlock = Nothing
            , plOpenTxHash = adOpenTxHash draft
            , plCloseTxHash = Nothing
            , plOpenOrderId = adOpenOrderId draft
            , plEntryPrice = effEntryRaw
            , plExitPrice = Nothing
            , plEntryCostAssets = effEntryCost
            , plCurrentSize = effSize
            , plCurrentMarginAssets = effMargin
            , plPeakSize = max (adPeakSize draft) effSize
            , plPeakNotionalAssets = max (adPeakNotionalAssets draft) curNotional
            , plPeakMarginAssets = max (adPeakMarginAssets draft) effMargin
            , plEntryLeverageX100 = adEntryLeverageX100 draft
            , plMaxLeverageX100 = max (adMaxLeverageX100 draft) curLevX100
            , plCurrentLeverageX100 = curLevX100
            , plRealizedPnlAssets = adRealizedPnlAssets draft
            , plUnrealizedPnlAssets = effUpnl
            , plTotalFeesAssets = adTotalFeesAssets draft
            , plTotalVpiAssets = adTotalVpiAssets draft
            , plTotalKeeperBountyAssets = adTotalKeeperBountyAssets draft
            , plLiquidationPrice = liqRaw
            , plLiquidatable = effLiquidatable
            , plMfePnlAssets = max 0 effUpnl
            , plMfeBps = 0
            , plMfePrice = Just effMark
            , plMfeTimestamp = Just _now
            , plMaePnlAssets = min 0 effUpnl
            , plMaeBps = 0
            , plMaePrice = Just effMark
            , plMaeTimestamp = Just _now
            , plClosestLiquidationBps = curLiqBps
            , plClosestLiquidationAt = Just _now
            , plCurrentLiquidationBps = curLiqBps
            , plFillCount = adFillCount draft
            , plScaleInCount = adScaleInCount draft
            , plPartialCloseCount = adPartialCloseCount draft
            , plMarginAddCount = adMarginAddCount draft
            , plEvents = events
            }

    synthesizeLiveOnlyPosition nowTs mMark snap =
      let side = fromInteger (alsSide snap)
          eRaw = normalizeLedgerEntryPrice (alsEntryPrice snap) (fromMaybe 100000000 mMark)
          eCost = computeNotionalAssets (alsSize snap) eRaw
          effMark = fromMaybe eRaw mMark
          curNotional = computeNotionalAssets (alsSize snap) effMark
          curLevX100 = computeLeverageX100 curNotional (alsMargin snap)
          liqRaw = findLiquidationRawPrice side (alsSize snap) eCost (alsMargin snap) (alsTraderClaimBalanceAssets snap) defaultMaintenanceMarginBps
          curLiqBps = distanceToLiquidationBps side effMark liqRaw
       in PositionLifecycle
            { plPositionId = "pos-live"
            , plStatus = "open"
            , plSide = side
            , plOpenedAt = nowTs
            , plClosedAt = Nothing
            , plOpenBlock = 0
            , plCloseBlock = Nothing
            , plOpenTxHash = ""
            , plCloseTxHash = Nothing
            , plOpenOrderId = Nothing
            , plEntryPrice = eRaw
            , plExitPrice = Nothing
            , plEntryCostAssets = eCost
            , plCurrentSize = alsSize snap
            , plCurrentMarginAssets = alsMargin snap
            , plPeakSize = alsSize snap
            , plPeakNotionalAssets = curNotional
            , plPeakMarginAssets = alsMargin snap
            , plEntryLeverageX100 = curLevX100
            , plMaxLeverageX100 = curLevX100
            , plCurrentLeverageX100 = curLevX100
            , plRealizedPnlAssets = 0
            , plUnrealizedPnlAssets = alsUnrealizedPnlAssets snap
            , plTotalFeesAssets = 0
            , plTotalVpiAssets = 0
            , plTotalKeeperBountyAssets = 0
            , plLiquidationPrice = liqRaw
            , plLiquidatable = alsLiquidatable snap
            , plMfePnlAssets = max 0 (alsUnrealizedPnlAssets snap)
            , plMfeBps = 0
            , plMfePrice = Just effMark
            , plMfeTimestamp = Just nowTs
            , plMaePnlAssets = min 0 (alsUnrealizedPnlAssets snap)
            , plMaeBps = 0
            , plMaePrice = Just effMark
            , plMaeTimestamp = Just nowTs
            , plClosestLiquidationBps = curLiqBps
            , plClosestLiquidationAt = Just nowTs
            , plCurrentLiquidationBps = curLiqBps
            , plFillCount = 0
            , plScaleInCount = 0
            , plPartialCloseCount = 0
            , plMarginAddCount = 0
            , plEvents = []
            }

normalizeLedgerEntryPrice :: Integer -> Integer -> Integer
normalizeLedgerEntryPrice raw fallback
  | raw > 0 && raw <= plDxyCapPrice = raw
  | raw > plDxyCapPrice =
      -- Handle 1e18-scaled prices if an older lens returns WAD precision
      let scaled = raw `div` 10000000000
       in if scaled > 0 && scaled <= plDxyCapPrice then scaled else fallback
  | otherwise = fallback

weightedRawPrice :: Integer -> Integer -> Integer -> Integer
weightedRawPrice size costAssets fallback
  | size <= 0 || costAssets <= 0 = fallback
  | otherwise = (costAssets * sizeToAssetDivisor) `div` size

-- | Enrich a reconstructed position with true MFE, MAE, closest liquidation distance,
-- and an ordered price/PnL/leverage time series from historical basket candles.
enrichPositionWithCandles
  :: Integer
  -> Maybe Integer
  -> [PriceCandleRow]
  -> PositionLifecycle
  -> (PositionLifecycle, [PositionPricePoint])
enrichPositionWithCandles nowUnix mMarkRaw allCandles pos =
  let endTs = fromMaybe nowUnix (plClosedAt pos)
      -- Include candles overlapping [openedAt, endTs]
      relevant =
        filter (\c -> pcrBucketStart c >= plOpenedAt pos - 3600 && pcrBucketStart c <= endTs + 60) allCandles
      -- Build sample observations from candles + exact event fills + live mark
      candleSamples =
        [ ( max (plOpenedAt pos) (min endTs (pcrBucketStart c))
          , pcrRawClose c
          , pcrRawHigh c
          , pcrRawLow c
          )
        | c <- relevant
        , pcrBucketStart c >= plOpenedAt pos - 60 && pcrBucketStart c <= endTs
        ]
      fillSamples =
        [ (pseTimestamp ev, p, p, p)
        | ev <- plEvents pos
        , Just p <- [psePrice ev]
        , p > 0
        ]
      liveSample =
        case (plStatus pos, mMarkRaw) of
          ("open", Just mk) | mk > 0 -> [(nowUnix, mk, mk, mk)]
          _ -> []
      mergedSamples = sortOn (\(t, _, _, _) -> t) (candleSamples <> fillSamples <> liveSample)
      -- Active position state at timestamp t
      stateAt t =
        let prior = filter (\ev -> pseTimestamp ev <= t && pseResultingSize ev > 0) (plEvents pos)
         in case reverse prior of
              (ev : _) ->
                ( pseResultingSize ev
                , pseResultingEntryCostAssets ev
                , pseResultingMarginAssets ev
                , pseResultingEntryPrice ev
                , pseResultingLiquidationPrice ev
                )
              [] ->
                ( if plCurrentSize pos > 0 then plCurrentSize pos else plPeakSize pos
                , plEntryCostAssets pos
                , if plCurrentMarginAssets pos > 0 then plCurrentMarginAssets pos else plPeakMarginAssets pos
                , plEntryPrice pos
                , plLiquidationPrice pos
                )
      evalPoint (ts, rawClose, rawHigh, rawLow) =
        let (stSize, stEntryCost, stMargin, stEntryRaw, stLiq) = stateAt ts
            side = plSide pos
            -- Long benefits from lower raw price (higher display price); Short benefits from higher raw price
            bestRaw = if side == 0 then rawLow else rawHigh
            worstRaw = if side == 0 then rawHigh else rawLow
            favPnl = computeUnrealizedPnl side stSize stEntryCost bestRaw
            advPnl = computeUnrealizedPnl side stSize stEntryCost worstRaw
            closePnl = computeUnrealizedPnl side stSize stEntryCost rawClose
            notional = computeNotionalAssets stSize rawClose
            levX100 = computeLeverageX100 notional stMargin
            mDistWorst = distanceToLiquidationBps side worstRaw stLiq
            mDistClose = distanceToLiquidationBps side rawClose stLiq
            pt =
              PositionPricePoint
                { pppTimestamp = ts
                , pppRawPrice = rawClose
                , pppRawHigh = rawHigh
                , pppRawLow = rawLow
                , pppUnrealizedPnlAssets = closePnl
                , pppPositionSize = stSize
                , pppLeverageX100 = levX100
                , pppLiquidationPrice = stLiq
                , pppDistanceToLiquidationBps = mDistClose
                }
         in (pt, (favPnl, bestRaw, ts, stEntryRaw), (advPnl, worstRaw, ts, stEntryRaw), (mDistWorst, ts))
      evaluated = map evalPoint mergedSamples
      priceSeries = downsamplePricePoints 140 (map (\(pt, _, _, _) -> pt) evaluated)
      -- MFE (max favorable excursion >= 0)
      favCandidates = [(pnl, p, ts, eRaw) | (_, (pnl, p, ts, eRaw), _, _) <- evaluated]
      (mfePnl, mfeRaw, mfeTs, mfeBpsVal) =
        case favCandidates of
          [] -> (max 0 (plMfePnlAssets pos), plMfePrice pos, plMfeTimestamp pos, 0)
          (firstC : restC) ->
            let (bPnl, bRaw, bTs, bEntry) = foldl (\acc x@(p1, _, _, _) -> let (p0, _, _, _) = acc in if p1 > p0 then x else acc) firstC restC
             in if bPnl > 0
                  then (bPnl, Just bRaw, Just bTs, priceMoveBps (plSide pos) bEntry bRaw)
                  else (0, Just (plEntryPrice pos), Just (plOpenedAt pos), 0)
      -- MAE (max adverse excursion <= 0)
      advCandidates = [(pnl, p, ts, eRaw) | (_, _, (pnl, p, ts, eRaw), _) <- evaluated]
      (maePnl, maeRaw, maeTs, maeBpsVal) =
        case advCandidates of
          [] -> (min 0 (plMaePnlAssets pos), plMaePrice pos, plMaeTimestamp pos, 0)
          (firstA : restA) ->
            let (wPnl, wRaw, wTs, wEntry) = foldl (\acc x@(p1, _, _, _) -> let (p0, _, _, _) = acc in if p1 < p0 then x else acc) firstA restA
             in if wPnl < 0
                  then (wPnl, Just wRaw, Just wTs, priceMoveBps (plSide pos) wEntry wRaw)
                  else (0, Just (plEntryPrice pos), Just (plOpenedAt pos), 0)
      -- Closest liquidation proximity
      liqCandidates = [(d, ts) | (_, _, _, (Just d, ts)) <- evaluated]
      (closestLiqBps, closestLiqAt) =
        if plStatus pos == "liquidated"
          then (Just 0, plClosedAt pos)
          else case liqCandidates of
            [] -> (plClosestLiquidationBps pos, plClosestLiquidationAt pos)
            (c0 : cs) ->
              let (minD, minTs) = foldl (\acc x@(d1, _) -> let (d0, _) = acc in if d1 < d0 then x else acc) c0 cs
               in (Just minD, Just minTs)
      maxLevSeen =
        foldl max (plMaxLeverageX100 pos) (map pppLeverageX100 priceSeries)
      enriched =
        pos
          { plMaxLeverageX100 = maxLevSeen
          , plMfePnlAssets = mfePnl
          , plMfeBps = mfeBpsVal
          , plMfePrice = mfeRaw
          , plMfeTimestamp = mfeTs
          , plMaePnlAssets = maePnl
          , plMaeBps = maeBpsVal
          , plMaePrice = maeRaw
          , plMaeTimestamp = maeTs
          , plClosestLiquidationBps = closestLiqBps
          , plClosestLiquidationAt = closestLiqAt
          }
   in (enriched, priceSeries)

-- | Signed display-price excursion in bps relative to entry display price.
-- Positive = favorable move, Negative = adverse move.
priceMoveBps :: Int -> Integer -> Integer -> Integer
priceMoveBps side entryRaw targetRaw =
  let entryDisp = max 0 (plDxyCapPrice - min plDxyCapPrice (max 0 entryRaw))
      targetDisp = max 0 (plDxyCapPrice - min plDxyCapPrice (max 0 targetRaw))
   in if entryDisp <= 0
        then 0
        else
          let delta = if side == 0 then targetDisp - entryDisp else entryDisp - targetDisp
           in (delta * 10000) `div` entryDisp

downsamplePricePoints :: Int -> [PositionPricePoint] -> [PositionPricePoint]
downsamplePricePoints maxPts pts
  | length pts <= maxPts = pts
  | otherwise =
      let n = length pts
          stride = max 1 ((n - 1) `div` max 1 (maxPts - 1))
       in [p | (i, p) <- zip [0 :: Int ..] pts, i == 0 || i == n - 1 || i `mod` stride == 0]

-- | Build a chronological signed-notional and leverage timeline across all positions.
buildExposureTimeline :: Integer -> Maybe Integer -> [PositionLifecycle] -> [ExposureTimelinePoint]
buildExposureTimeline nowUnix mMarkRaw positions =
  let allEvents =
        sortOn
          (\(_, ev) -> (pseTimestamp ev, pseBlockNumber ev, pseLogIndex ev))
          [(pos, ev) | pos <- positions, ev <- plEvents pos]
      fromEvents =
        map
          ( \(pos, ev) ->
              let notional = pseResultingNotionalAssets ev
                  signedNotional =
                    if pseResultingSize ev <= 0
                      then 0
                      else if plSide pos == 0 then notional else negate notional
                  sideTxt =
                    if pseResultingSize ev <= 0
                      then "flat"
                      else sideText (plSide pos)
               in ExposureTimelinePoint
                    { etpTimestamp = pseTimestamp ev
                    , etpBlockNumber = pseBlockNumber ev
                    , etpSignedNotionalAssets = signedNotional
                    , etpAbsNotionalAssets = notional
                    , etpMarginAssets = pseResultingMarginAssets ev
                    , etpLeverageX100 = pseResultingLeverageX100 ev
                    , etpSide = sideTxt
                    , etpEventType = pseAction ev
                    , etpPositionId = Just (plPositionId pos)
                    }
          )
          allEvents
      openPos = listToMaybe $ filter (\p -> plStatus p == "open") positions
      withLive =
        case openPos of
          Just p ->
            let effMark = fromMaybe (plEntryPrice p) mMarkRaw
                notional = computeNotionalAssets (plCurrentSize p) effMark
                signedNotional = if plSide p == 0 then notional else negate notional
             in fromEvents
                  <> [ ExposureTimelinePoint
                        { etpTimestamp = nowUnix
                        , etpBlockNumber = 0
                        , etpSignedNotionalAssets = signedNotional
                        , etpAbsNotionalAssets = notional
                        , etpMarginAssets = plCurrentMarginAssets p
                        , etpLeverageX100 = plCurrentLeverageX100 p
                        , etpSide = sideText (plSide p)
                        , etpEventType = "Live"
                        , etpPositionId = Just (plPositionId p)
                        }
                     ]
          Nothing ->
            case reverse fromEvents of
              (lastPt : _)
                | etpTimestamp lastPt < nowUnix ->
                    fromEvents
                      <> [ ExposureTimelinePoint
                            { etpTimestamp = nowUnix
                            , etpBlockNumber = etpBlockNumber lastPt
                            , etpSignedNotionalAssets = 0
                            , etpAbsNotionalAssets = 0
                            , etpMarginAssets = 0
                            , etpLeverageX100 = 0
                            , etpSide = "flat"
                            , etpEventType = "Live"
                            , etpPositionId = Nothing
                            }
                         ]
              _ -> fromEvents
   in downsampleTimeline 160 withLive

downsampleTimeline :: Int -> [ExposureTimelinePoint] -> [ExposureTimelinePoint]
downsampleTimeline maxPts pts
  | length pts <= maxPts = pts
  | otherwise =
      let n = length pts
          stride = max 1 ((n - 1) `div` max 1 (maxPts - 1))
       in [p | (i, p) <- zip [0 :: Int ..] pts, i == 0 || i == n - 1 || i `mod` stride == 0]

-- | Aggregate wallet-level market exposure analytics across reconstructed positions.
summarizeWalletExposure :: Integer -> [PositionLifecycle] -> WalletExposureSummary
summarizeWalletExposure nowUnix positions =
  let totalPos = length positions
      openPos = filter (\p -> plStatus p == "open") positions
      closedPos = filter (\p -> plStatus p == "closed") positions
      liqPos = filter (\p -> plStatus p == "liquidated") positions
      terminated = closedPos <> liqPos
      wins = length $ filter (\p -> plRealizedPnlAssets p > 0) terminated
      losses = length $ filter (\p -> plRealizedPnlAssets p < 0) terminated
      winRate =
        if null terminated
          then Nothing
          else Just ((fromIntegral wins * 10000) `div` fromIntegral (length terminated))
      longs = filter (\p -> plSide p == 0) positions
      shorts = filter (\p -> plSide p == 1) positions
      longNotional = sum (map plPeakNotionalAssets longs)
      shortNotional = sum (map plPeakNotionalAssets shorts)
      totalNotional = longNotional + shortNotional
      (longShareBps, shortShareBps) =
        if totalNotional <= 0
          then (Nothing, Nothing)
          else
            let ls = (longNotional * 10000) `div` totalNotional
             in (Just ls, Just (10000 - ls))
      longPnl = sum (map plRealizedPnlAssets longs)
      shortPnl = sum (map plRealizedPnlAssets shorts)
      holdOf p = max 0 (fromMaybe nowUnix (plClosedAt p) - plOpenedAt p)
      longHold = sum (map holdOf longs)
      shortHold = sum (map holdOf shorts)
      entryLevs = filter (> 0) (map plEntryLeverageX100 positions)
      avgEntryLev =
        if null entryLevs
          then Nothing
          else Just (sum entryLevs `div` fromIntegral (length entryLevs))
      maxLev = foldl max 0 (map plMaxLeverageX100 positions)
      curLev = maybe 0 plCurrentLeverageX100 (listToMaybe openPos)
      holds = sort (map holdOf positions)
      avgHold =
        if null holds
          then Nothing
          else Just (sum holds `div` fromIntegral (length holds))
      medianHold =
        case holds of
          [] -> Nothing
          xs ->
            let n = length xs
                mid = n `div` 2
             in listToMaybe (drop mid xs)
      avgNotional =
        if totalPos == 0
          then Nothing
          else Just (totalNotional `div` fromIntegral totalPos)
      peakNotional = foldl max 0 (map plPeakNotionalAssets positions)
      avgMfe =
        if totalPos == 0
          then Nothing
          else Just (sum (map plMfePnlAssets positions) `div` fromIntegral totalPos)
      avgMae =
        if totalPos == 0
          then Nothing
          else Just (sum (map plMaePnlAssets positions) `div` fromIntegral totalPos)
      liqBpsList = mapMaybe plClosestLiquidationBps positions
      tightestLiq =
        case liqBpsList of
          [] -> Nothing
          (x : xs) -> Just (foldl min x xs)
   in WalletExposureSummary
        { wesTotalPositions = totalPos
        , wesOpenPositions = length openPos
        , wesClosedPositions = length closedPos
        , wesLiquidatedPositions = length liqPos
        , wesWinCount = wins
        , wesLossCount = losses
        , wesWinRateBps = winRate
        , wesLongPositions = length longs
        , wesShortPositions = length shorts
        , wesLongNotionalAssets = longNotional
        , wesShortNotionalAssets = shortNotional
        , wesLongNotionalShareBps = longShareBps
        , wesShortNotionalShareBps = shortShareBps
        , wesLongRealizedPnlAssets = longPnl
        , wesShortRealizedPnlAssets = shortPnl
        , wesLongHoldSeconds = longHold
        , wesShortHoldSeconds = shortHold
        , wesAvgEntryLeverageX100 = avgEntryLev
        , wesMaxLeverageX100 = maxLev
        , wesCurrentLeverageX100 = curLev
        , wesAvgHoldSeconds = avgHold
        , wesMedianHoldSeconds = medianHold
        , wesAvgNotionalAssets = avgNotional
        , wesPeakNotionalAssets = peakNotional
        , wesAvgMfeAssets = avgMfe
        , wesAvgMaeAssets = avgMae
        , wesTightestLiquidationBps = tightestLiq
        }

-- | GET /api/insights/v1/wallets/:address/exposure
getWalletExposureResponse
  :: DbPool
  -> EthClient
  -> Config
  -> Text
  -> IO (Either ApiError (ApiResponse Value))
getWalletExposureResponse pool client cfg rawAddress = do
  now <- floor <$> getPOSIXTime
  let account = T.toLower $ T.strip rawAddress
      chainId = cfgPerpsChainId cfg
      router = T.toLower $ T.strip $ cfgPerpsOrderRouter cfg
      accountLens = T.toLower $ T.strip $ cfgPerpsAccountLens cfg
  (ledgerRes, dbTuple) <-
    concurrently
      (getAccountLedgerSnapshot client accountLens account)
      ( withDb pool $ \conn ->
          withReadSnapshot conn $ do
            rows <- fetchWalletPositionActivity conn chainId router account
            mMark <- fetchLatestBasketMark conn now
            let minTs = case map parTimestamp rows of
                  [] -> Nothing
                  (x : xs) -> Just (foldl min x xs)
                maxTs = now
            candles <- case minTs of
              Nothing -> pure []
              Just startTs ->
                let interval = pickCandleIntervalSeconds (max 0 (maxTs - startTs))
                 in fetchBasketCandlesForWindow conn now interval (startTs - interval) (maxTs + interval) 2400
            pure (rows, mMark, candles)
      )
  let (rows, mMarkTuple, candles) = dbTuple
      mLedger = either (const Nothing) Just ledgerRes
      mMarkRaw = fst <$> mMarkTuple
      mMarkTs = snd <$> mMarkTuple
      latestBlock = foldl max 0 (map parBlockNumber rows)
      rawPositions = reconstructPositionLifecycles now mMarkRaw mLedger rows
      enrichedPositions = map (fst . enrichPositionWithCandles now mMarkRaw candles) rawPositions
      timeline = buildExposureTimeline now mMarkRaw enrichedPositions
      summary = summarizeWalletExposure now enrichedPositions
      activePos = listToMaybe $ filter (\p -> plStatus p == "open") enrichedPositions
  pure $
    Right $
      mkResponse latestBlock chainId $
        object
          [ "network"
              .= object
                [ "chainId" .= chainId
                , "releaseVersion" .= Manifest.releaseVersion
                , "settlementAssetSymbol" .= ("USDG" :: Text)
                ]
          , "account" .= account
          , "observedAt" .= isoTimestamp now
          , "currentMarkPrice" .= (show <$> mMarkRaw)
          , "markObservedAt" .= (isoTimestamp <$> mMarkTs)
          , "maintenanceMarginBps" .= defaultMaintenanceMarginBps
          , "activeExposure" .= (activeExposureToJson now mMarkRaw mLedger <$> activePos)
          , "summary" .= walletExposureSummaryToJson summary
          , "exposureTimeline" .= map exposureTimelinePointToJson timeline
          , "positions" .= map (positionSummaryToJson now) (take 50 enrichedPositions)
          ]

-- | GET /api/insights/v1/wallets/:address/positions/:positionId
getWalletPositionDetailResponse
  :: DbPool
  -> EthClient
  -> Config
  -> Text
  -> Text
  -> IO (Either ApiError (ApiResponse Value))
getWalletPositionDetailResponse pool client cfg rawAddress rawPositionId = do
  now <- floor <$> getPOSIXTime
  let account = T.toLower $ T.strip rawAddress
      targetId = T.strip rawPositionId
      chainId = cfgPerpsChainId cfg
      router = T.toLower $ T.strip $ cfgPerpsOrderRouter cfg
      accountLens = T.toLower $ T.strip $ cfgPerpsAccountLens cfg
      book = T.toLower Manifest.positionProtectionBookAddress
  (ledgerRes, dbInitial) <-
    concurrently
      (getAccountLedgerSnapshot client accountLens account)
      ( withDb pool $ \conn ->
          withReadSnapshot conn $ do
            handle <- fetchDiscordHandle conn account
            rows <- fetchWalletPositionActivity conn chainId router account
            mMark <- fetchLatestBasketMark conn now
            pure (handle, rows, mMark)
      )
  let (mHandle, rows, mMarkTuple) = dbInitial
      mLedger = either (const Nothing) Just ledgerRes
      mMarkRaw = fst <$> mMarkTuple
      mMarkTs = snd <$> mMarkTuple
      latestBlock = foldl max 0 (map parBlockNumber rows)
      rawPositions = reconstructPositionLifecycles now mMarkRaw mLedger rows
      matched =
        listToMaybe $
          filter
            ( \p ->
                plPositionId p == targetId
                  || fmap (T.pack . show) (plOpenOrderId p) == Just targetId
            )
            rawPositions
  case matched of
    Nothing ->
      pure $ Left $ E.notFound ("Position lifecycle not found: " <> targetId)
    Just pos -> do
      let startTs = plOpenedAt pos
          endTs = fromMaybe now (plClosedAt pos)
          interval = pickCandleIntervalSeconds (max 0 (endTs - startTs))
          openOrderIds = mapMaybe pseOrderId (filter (\ev -> pseRawActivityType ev == "Open") (plEvents pos))
      (candles, protEvents, protBundle) <-
        withDb pool $ \conn ->
          withReadSnapshot conn $ do
            cs <- fetchBasketCandlesForWindow conn now interval (startTs - interval) (endTs + interval) 1500
            pe <- fetchPositionProtectionEvents conn chainId book account (plOpenBlock pos) (plCloseBlock pos)
            pb <- fetchOrderProtectionsBundle conn chainId router book openOrderIds
            pure (cs, pe, pb)
      let (enriched, priceSeries) = enrichPositionWithCandles now mMarkRaw candles pos
          captureRatioBps =
            if plMfePnlAssets enriched > 0
              then
                let netPnl =
                      if plStatus enriched == "open"
                        then plRealizedPnlAssets enriched + plUnrealizedPnlAssets enriched
                        else plRealizedPnlAssets enriched
                 in Just ((netPnl * 10000) `div` plMfePnlAssets enriched)
              else Nothing
      pure $
        Right $
          mkResponse latestBlock chainId $
            object
              [ "network"
                  .= object
                    [ "chainId" .= chainId
                    , "releaseVersion" .= Manifest.releaseVersion
                    , "settlementAssetSymbol" .= ("USDG" :: Text)
                    ]
              , "account" .= account
              , "observedAt" .= isoTimestamp now
              , "identity"
                  .= object
                    [ "discordUsername" .= mHandle
                    ]
              , "currentMarkPrice" .= (show <$> mMarkRaw)
              , "markObservedAt" .= (isoTimestamp <$> mMarkTs)
              , "maintenanceMarginBps" .= defaultMaintenanceMarginBps
              , "candleIntervalSeconds" .= interval
              , "position" .= positionDetailToJson now captureRatioBps enriched
              , "priceSeries" .= map positionPricePointToJson priceSeries
              , "events" .= map (positionStepEventToJson protBundle) (plEvents enriched)
              , "protections" .= map protectionEventToJson protEvents
              ]

fetchWalletPositionActivity :: Connection -> Integer -> Text -> Text -> IO [PositionActivityRow]
fetchWalletPositionActivity conn chainId router account =
  query
    conn
    "SELECT a.activity_type, a.order_id, a.side, a.price, a.size_delta, \
    \a.amount_assets, a.pnl_assets, \
    \CASE WHEN COALESCE(o.receipt_economics->>'executionFeeAssets', a.data->>'executionFeeAssets') ~ '^-{0,1}[0-9]+$' \
    \  THEN (COALESCE(o.receipt_economics->>'executionFeeAssets', a.data->>'executionFeeAssets'))::NUMERIC ELSE NULL END, \
    \COALESCE(o.execution_vpi_assets, CASE WHEN a.data->>'vpiAssets' ~ '^-{0,1}[0-9]+$' THEN (a.data->>'vpiAssets')::NUMERIC ELSE NULL END), \
    \CASE WHEN a.data->>'keeperBountyAssets' ~ '^[0-9]+$' THEN (a.data->>'keeperBountyAssets')::NUMERIC ELSE NULL END, \
    \a.tx_hash, a.block_number, a.log_index, a.timestamp, execution.payload \
    \FROM perps_account_activity a \
    \LEFT JOIN perps_orders o \
    \  ON o.chain_id = a.chain_id AND o.order_router = a.release_router AND o.order_id = a.order_id \
    \LEFT JOIN LATERAL (\
    \  SELECT CASE WHEN COUNT(*) = 1 THEN (jsonb_agg(candidate.payload))->0 ELSE NULL END AS payload \
    \  FROM (\
    \    SELECT jsonb_build_object('orderId', o2.order_id::text, 'receipt', o2.receipt_economics) AS payload \
    \    FROM perps_events terminal \
    \    JOIN perps_orders o2 ON o2.chain_id = terminal.chain_id AND o2.order_router = terminal.release_router \
    \      AND o2.order_id = terminal.order_id AND o2.terminal_status = 'Executed' \
    \      AND o2.account = a.account AND o2.terminal_tx_hash = a.tx_hash AND o2.terminal_block_number = a.block_number \
    \      AND o2.receipt_hash = terminal.data->>'receiptHash' \
    \    WHERE terminal.chain_id = a.chain_id AND terminal.release_router = a.release_router \
    \      AND terminal.tx_hash = a.tx_hash AND terminal.block_hash = a.block_hash AND terminal.block_number = a.block_number \
    \      AND terminal.account = a.account AND terminal.event_name = 'OrderFinalized' \
    \      AND terminal.data->>'status' = '2' AND terminal.data->>'terminalReason' = 'Executed' \
    \      AND terminal.log_index > a.log_index AND a.activity_type IN ('Open', 'Close') \
    \      AND NOT EXISTS (\
    \        SELECT 1 FROM perps_events prior WHERE prior.chain_id = a.chain_id AND prior.release_router = a.release_router \
    \          AND prior.tx_hash = a.tx_hash AND prior.block_hash = a.block_hash \
    \          AND prior.event_name = 'OrderFinalized' AND prior.log_index > a.log_index AND prior.log_index < terminal.log_index\
    \      )\
    \  ) candidate\
    \) execution ON a.order_id IS NULL AND a.activity_type IN ('Open', 'Close') \
    \WHERE a.chain_id = ? AND a.release_router = ? AND a.account = ? \
    \  AND a.activity_type IN ('Open', 'Close', 'Liquidated', 'Add margin') \
    \ORDER BY a.block_number ASC, a.log_index ASC \
    \LIMIT 2000"
    (chainId, router, account)

fetchLatestBasketMark :: Connection -> Integer -> IO (Maybe (Integer, Integer))
fetchLatestBasketMark conn nowUnix = do
  mSeries <- getActiveBasketSeriesId conn nowUnix
  case mSeries of
    Just seriesId -> do
      obs <-
        query
          conn
          "SELECT basket_price, publish_time FROM perps_basket_observations \
          \WHERE series_id = ? ORDER BY publish_time DESC LIMIT 1"
          (Only seriesId) :: IO [(Integer, Integer)]
      case obs of
        (row : _) -> pure (Just row)
        [] -> do
          candles <-
            query
              conn
              "SELECT raw_close_price, bucket_start FROM perps_basket_candles \
              \WHERE series_id = ? AND interval_seconds = 60 ORDER BY bucket_start DESC LIMIT 1"
              (Only seriesId) :: IO [(Integer, Integer)]
          pure (listToMaybe candles)
    Nothing -> do
      snaps <-
        query
          conn
          "SELECT basket_price, timestamp FROM perps_basket_snapshots ORDER BY timestamp DESC LIMIT 1"
          () :: IO [(Integer, Integer)]
      pure (listToMaybe snaps)

fetchBasketCandlesForWindow
  :: Connection
  -> Integer
  -> Integer
  -> Integer
  -> Integer
  -> Int
  -> IO [PriceCandleRow]
fetchBasketCandlesForWindow conn nowUnix intervalSeconds startTs endTs maxRows = do
  mSeries <- getActiveBasketSeriesId conn nowUnix
  case mSeries of
    Just seriesId -> do
      rows <-
        query
          conn
          "SELECT bucket_start, raw_open_price, raw_high_price, raw_low_price, raw_close_price \
          \FROM perps_basket_candles \
          \WHERE series_id = ? AND interval_seconds = ? AND bucket_start >= ? AND bucket_start <= ? \
          \ORDER BY bucket_start ASC LIMIT ?"
          (seriesId, intervalSeconds, startTs, endTs, maxRows)
      if null rows
        then fetchLegacySnapshots conn startTs endTs maxRows
        else pure rows
    Nothing -> fetchLegacySnapshots conn startTs endTs maxRows

fetchLegacySnapshots :: Connection -> Integer -> Integer -> Int -> IO [PriceCandleRow]
fetchLegacySnapshots conn startTs endTs maxRows = do
  rows <-
    query
      conn
      "SELECT timestamp, basket_price FROM perps_basket_snapshots \
      \WHERE timestamp >= ? AND timestamp <= ? \
      \ORDER BY timestamp ASC LIMIT ?"
      (startTs, endTs, maxRows) :: IO [(Integer, Integer)]
  pure [PriceCandleRow ts p p p p | (ts, p) <- rows]

fetchPositionProtectionEvents
  :: Connection
  -> Integer
  -> Text
  -> Text
  -> Integer
  -> Maybe Integer
  -> IO [PositionProtectionEventRow]
fetchPositionProtectionEvents conn chainId book account openBlock mCloseBlock =
  query
    conn
    "SELECT protection_id::text, block_number, log_index, transaction_hash, event_name, event_data, snapshot \
    \FROM perps_protection_events \
    \WHERE chain_id = ? AND book = ? AND account = ? \
    \  AND block_number >= ? \
    \  AND (?::bigint IS NULL OR block_number <= ?) \
    \ORDER BY block_number ASC, log_index ASC \
    \LIMIT 50"
    (chainId, book, account, openBlock, mCloseBlock, mCloseBlock)

activeExposureToJson :: Integer -> Maybe Integer -> Maybe AccountLedgerSnapshot -> PositionLifecycle -> Value
activeExposureToJson nowUnix mMarkRaw mLedger pos =
  let effMark = fromMaybe (plEntryPrice pos) mMarkRaw
      notional = computeNotionalAssets (plCurrentSize pos) effMark
      marginUtilBps =
        case mLedger of
          Just snap | alsNetEquityAssets snap > 0 ->
            Just ((alsActivePositionMarginAssets snap * 10000) `div` alsNetEquityAssets snap)
          _ -> Nothing
   in object
        [ "positionId" .= plPositionId pos
        , "market" .= ("plDXY-PERP" :: Text)
        , "side" .= sideText (plSide pos)
        , "size" .= show (plCurrentSize pos)
        , "notionalAssets" .= show notional
        , "marginAssets" .= show (plCurrentMarginAssets pos)
        , "entryPrice" .= show (plEntryPrice pos)
        , "markPrice" .= show effMark
        , "liquidationPrice" .= (show <$> plLiquidationPrice pos)
        , "distanceToLiquidationBps" .= plCurrentLiquidationBps pos
        , "closestLiquidationBps" .= plClosestLiquidationBps pos
        , "effectiveLeverageX100" .= plCurrentLeverageX100 pos
        , "unrealizedPnlAssets" .= show (plUnrealizedPnlAssets pos)
        , "marginUtilizationBps" .= marginUtilBps
        , "liquidatable" .= plLiquidatable pos
        , "openedAt" .= isoTimestamp (plOpenedAt pos)
        , "durationSeconds" .= max 0 (nowUnix - plOpenedAt pos)
        , "mfePnlAssets" .= show (plMfePnlAssets pos)
        , "maePnlAssets" .= show (plMaePnlAssets pos)
        ]

walletExposureSummaryToJson :: WalletExposureSummary -> Value
walletExposureSummaryToJson WalletExposureSummary {..} =
  object
    [ "totalPositions" .= wesTotalPositions
    , "openPositions" .= wesOpenPositions
    , "closedPositions" .= wesClosedPositions
    , "liquidatedPositions" .= wesLiquidatedPositions
    , "winCount" .= wesWinCount
    , "lossCount" .= wesLossCount
    , "winRateBps" .= wesWinRateBps
    , "longPositions" .= wesLongPositions
    , "shortPositions" .= wesShortPositions
    , "longNotionalAssets" .= show wesLongNotionalAssets
    , "shortNotionalAssets" .= show wesShortNotionalAssets
    , "longNotionalShareBps" .= wesLongNotionalShareBps
    , "shortNotionalShareBps" .= wesShortNotionalShareBps
    , "longRealizedPnlAssets" .= show wesLongRealizedPnlAssets
    , "shortRealizedPnlAssets" .= show wesShortRealizedPnlAssets
    , "longHoldSeconds" .= wesLongHoldSeconds
    , "shortHoldSeconds" .= wesShortHoldSeconds
    , "avgEntryLeverageX100" .= wesAvgEntryLeverageX100
    , "maxLeverageX100" .= wesMaxLeverageX100
    , "currentLeverageX100" .= wesCurrentLeverageX100
    , "avgHoldSeconds" .= wesAvgHoldSeconds
    , "medianHoldSeconds" .= wesMedianHoldSeconds
    , "avgNotionalAssets" .= (show <$> wesAvgNotionalAssets)
    , "peakNotionalAssets" .= show wesPeakNotionalAssets
    , "avgMfeAssets" .= (show <$> wesAvgMfeAssets)
    , "avgMaeAssets" .= (show <$> wesAvgMaeAssets)
    , "tightestLiquidationBps" .= wesTightestLiquidationBps
    ]

exposureTimelinePointToJson :: ExposureTimelinePoint -> Value
exposureTimelinePointToJson ExposureTimelinePoint {..} =
  object
    [ "timestamp" .= etpTimestamp
    , "occurredAt" .= isoTimestamp etpTimestamp
    , "blockNumber" .= show etpBlockNumber
    , "signedNotionalAssets" .= show etpSignedNotionalAssets
    , "absNotionalAssets" .= show etpAbsNotionalAssets
    , "marginAssets" .= show etpMarginAssets
    , "leverageX100" .= etpLeverageX100
    , "side" .= etpSide
    , "eventType" .= etpEventType
    , "positionId" .= etpPositionId
    ]

positionSummaryToJson :: Integer -> PositionLifecycle -> Value
positionSummaryToJson nowUnix pos =
  let duration = max 0 (fromMaybe nowUnix (plClosedAt pos) - plOpenedAt pos)
      netPnl =
        if plStatus pos == "open"
          then plRealizedPnlAssets pos + plUnrealizedPnlAssets pos
          else plRealizedPnlAssets pos
      roiBps =
        if plPeakMarginAssets pos > 0
          then Just ((netPnl * 10000) `div` plPeakMarginAssets pos)
          else Nothing
   in object
        [ "positionId" .= plPositionId pos
        , "market" .= ("plDXY-PERP" :: Text)
        , "status" .= plStatus pos
        , "side" .= sideText (plSide pos)
        , "openedAt" .= isoTimestamp (plOpenedAt pos)
        , "closedAt" .= (isoTimestamp <$> plClosedAt pos)
        , "durationSeconds" .= duration
        , "entryPrice" .= show (plEntryPrice pos)
        , "exitPrice" .= (show <$> plExitPrice pos)
        , "currentSize" .= show (plCurrentSize pos)
        , "peakSize" .= show (plPeakSize pos)
        , "peakNotionalAssets" .= show (plPeakNotionalAssets pos)
        , "peakMarginAssets" .= show (plPeakMarginAssets pos)
        , "entryLeverageX100" .= plEntryLeverageX100 pos
        , "maxLeverageX100" .= plMaxLeverageX100 pos
        , "currentLeverageX100" .= plCurrentLeverageX100 pos
        , "realizedPnlAssets" .= show (plRealizedPnlAssets pos)
        , "unrealizedPnlAssets" .= show (plUnrealizedPnlAssets pos)
        , "netPnlAssets" .= show netPnl
        , "roiBps" .= roiBps
        , "totalFeesAssets" .= show (plTotalFeesAssets pos)
        , "totalVpiAssets" .= show (plTotalVpiAssets pos)
        , "totalKeeperBountyAssets" .= show (plTotalKeeperBountyAssets pos)
        , "mfePnlAssets" .= show (plMfePnlAssets pos)
        , "mfeBps" .= plMfeBps pos
        , "mfePrice" .= (show <$> plMfePrice pos)
        , "mfeOccurredAt" .= (isoTimestamp <$> plMfeTimestamp pos)
        , "maePnlAssets" .= show (plMaePnlAssets pos)
        , "maeBps" .= plMaeBps pos
        , "maePrice" .= (show <$> plMaePrice pos)
        , "maeOccurredAt" .= (isoTimestamp <$> plMaeTimestamp pos)
        , "liquidationPrice" .= (show <$> plLiquidationPrice pos)
        , "closestLiquidationBps" .= plClosestLiquidationBps pos
        , "closestLiquidationAt" .= (isoTimestamp <$> plClosestLiquidationAt pos)
        , "currentLiquidationBps" .= plCurrentLiquidationBps pos
        , "liquidatable" .= plLiquidatable pos
        , "fillCount" .= plFillCount pos
        , "scaleInCount" .= plScaleInCount pos
        , "partialCloseCount" .= plPartialCloseCount pos
        , "marginAddCount" .= plMarginAddCount pos
        , "openTxHash" .= plOpenTxHash pos
        , "closeTxHash" .= plCloseTxHash pos
        , "openOrderId" .= (show <$> plOpenOrderId pos)
        ]

positionDetailToJson :: Integer -> Maybe Integer -> PositionLifecycle -> Value
positionDetailToJson nowUnix captureRatioBps pos =
  case positionSummaryToJson nowUnix pos of
    Object base ->
      Object $
        KM.insert (Key.fromText "captureRatioBps") (maybe Null (\v -> toJsonInt v) captureRatioBps) $
          KM.insert (Key.fromText "remainingMarginAssets") (String (T.pack (show (plCurrentMarginAssets pos)))) $
            KM.insert (Key.fromText "entryCostAssets") (String (T.pack (show (plEntryCostAssets pos)))) base
    other -> other
  where
    toJsonInt :: Integer -> Value
    toJsonInt = Number . fromInteger

positionPricePointToJson :: PositionPricePoint -> Value
positionPricePointToJson PositionPricePoint {..} =
  object
    [ "timestamp" .= pppTimestamp
    , "occurredAt" .= isoTimestamp pppTimestamp
    , "rawPrice" .= show pppRawPrice
    , "rawHigh" .= show pppRawHigh
    , "rawLow" .= show pppRawLow
    , "unrealizedPnlAssets" .= show pppUnrealizedPnlAssets
    , "positionSize" .= show pppPositionSize
    , "leverageX100" .= pppLeverageX100
    , "liquidationPrice" .= (show <$> pppLiquidationPrice)
    , "distanceToLiquidationBps" .= pppDistanceToLiquidationBps
    ]

positionStepEventToJson :: OrderProtectionsBundle -> PositionStepEvent -> Value
positionStepEventToJson protBundle PositionStepEvent {..} =
  let mOrderProtections =
        if pseRawActivityType == "Open"
          then pseOrderId >>= buildOrderProtectionsForOrder protBundle
          else Nothing
   in object $
        catMaybes
          [ Just $ "id" .= pseId
          , Just $ "action" .= pseAction
          , Just $ "activityType" .= pseRawActivityType
          , ("orderId" .=) . show <$> pseOrderId
          , Just $ "txHash" .= pseTxHash
          , Just $ "blockNumber" .= show pseBlockNumber
          , Just $ "timestamp" .= pseTimestamp
          , Just $ "occurredAt" .= isoTimestamp pseTimestamp
          , ("price" .=) . show <$> psePrice
          , Just $ "sizeDelta" .= show pseSizeDelta
          , Just $ "resultingSize" .= show pseResultingSize
          , Just $ "resultingEntryPrice" .= show pseResultingEntryPrice
          , Just $ "marginDeltaAssets" .= show pseMarginDeltaAssets
          , Just $ "resultingMarginAssets" .= show pseResultingMarginAssets
          , Just $ "resultingNotionalAssets" .= show pseResultingNotionalAssets
          , Just $ "resultingLeverageX100" .= pseResultingLeverageX100
          , ("resultingLiquidationPrice" .=) . show <$> pseResultingLiquidationPrice
          , ("pnlAssets" .=) . show <$> psePnlAssets
          , ("feeAssets" .=) . show <$> pseFeeAssets
          , ("vpiAssets" .=) . show <$> pseVpiAssets
          , ("keeperBountyAssets" .=) . show <$> pseKeeperBountyAssets
          , ("orderProtections" .=) <$> mOrderProtections
          ]

protectionEventToJson :: PositionProtectionEventRow -> Value
protectionEventToJson PositionProtectionEventRow {..} =
  object
    [ "protectionId" .= pperProtectionId
    , "blockNumber" .= show pperBlockNumber
    , "logIndex" .= pperLogIndex
    , "txHash" .= pperTxHash
    , "eventName" .= pperEventName
    , "eventData" .= pperEventData
    , "snapshot" .= pperSnapshot
    ]

sideText :: Int -> Text
sideText 0 = "long"
sideText 1 = "short"
sideText _ = "unknown"

withReadSnapshot :: Connection -> IO a -> IO a
withReadSnapshot conn =
  withTransactionMode
    TransactionMode
      { isolationLevel = RepeatableRead
      , readWriteMode = ReadOnly
      }
    conn

nullableNumericIntegerField :: RowParser (Maybe Integer)
nullableNumericIntegerField =
  fieldWith $ \column raw ->
    case raw of
      Nothing -> pure Nothing
      Just bytes -> case readMaybe (BS8.unpack bytes) of
        Just integer -> pure $ Just integer
        Nothing -> returnError ConversionFailed column "Expected nullable integer NUMERIC column"

isoTimestamp :: Integer -> Text
isoTimestamp =
  T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" . posixSecondsToUTCTime . fromIntegral

data GlobalOpenPosition = GlobalOpenPosition
  { gopAccount :: Text
  , gopDiscordUsername :: Maybe Text
  , gopPositionId :: Text
  , gopSide :: Int
  , gopSize :: Integer
  , gopNotionalAssets :: Integer
  , gopMarginAssets :: Integer
  , gopEntryPrice :: Integer
  , gopMarkPrice :: Integer
  , gopLiquidationPrice :: Maybe Integer
  , gopDistanceToLiquidationBps :: Maybe Integer
  , gopEffectiveLeverageX100 :: Integer
  , gopUnrealizedPnlAssets :: Integer
  , gopOpenedAt :: Integer
  , gopDurationSeconds :: Integer
  }
  deriving stock (Eq, Show)

data GlobalMarketExposure = GlobalMarketExposure
  { gmeLongOpenInterestAssets :: Integer
  , gmeShortOpenInterestAssets :: Integer
  , gmeTotalOpenInterestAssets :: Integer
  , gmeNetSkewAssets :: Integer
  , gmeLongShareBps :: Maybe Integer
  , gmeShortShareBps :: Maybe Integer
  , gmeOpenPositionCount :: Int
  , gmeLongCount :: Int
  , gmeShortCount :: Int
  , gmeAvgLeverageX100 :: Maybe Integer
  , gmeTopPositions :: [GlobalOpenPosition]
  }
  deriving stock (Eq, Show)

data AccountTaggedActivityRow = AccountTaggedActivityRow
  { atarAccount :: Text
  , atarActivityType :: Text
  , atarOrderId :: Maybe Integer
  , atarSide :: Maybe Int
  , atarPrice :: Maybe Integer
  , atarSizeDelta :: Maybe Integer
  , atarAmountAssets :: Maybe Integer
  , atarPnlAssets :: Maybe Integer
  , atarTxHash :: Text
  , atarBlockNumber :: Integer
  , atarLogIndex :: Integer
  , atarTimestamp :: Integer
  }

instance FromRow AccountTaggedActivityRow where
  fromRow =
    AccountTaggedActivityRow
      <$> field
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
      <*> field

{-# NOINLINE marketExposureCache #-}
marketExposureCache :: SingleFlightCache () (Either ApiError (ApiResponse Value))
marketExposureCache =
  unsafePerformIO (newRefreshingSingleFlightCache 1 (15 * 1000000000) (300 * 1000000000))

computeGlobalMarketExposure
  :: Integer
  -> Maybe Integer
  -> Map.Map Text Text
  -> Map.Map Text [PositionActivityRow]
  -> GlobalMarketExposure
computeGlobalMarketExposure nowUnix mMarkRaw handlesMap byAccount =
  let openPositions =
        sortOn
          (Down . gopNotionalAssets)
          [ GlobalOpenPosition
              { gopAccount = acct
              , gopDiscordUsername = Map.lookup acct handlesMap
              , gopPositionId = plPositionId pos
              , gopSide = plSide pos
              , gopSize = plCurrentSize pos
              , gopNotionalAssets = notional
              , gopMarginAssets = plCurrentMarginAssets pos
              , gopEntryPrice = plEntryPrice pos
              , gopMarkPrice = effMark
              , gopLiquidationPrice = plLiquidationPrice pos
              , gopDistanceToLiquidationBps = plCurrentLiquidationBps pos
              , gopEffectiveLeverageX100 = plCurrentLeverageX100 pos
              , gopUnrealizedPnlAssets = plUnrealizedPnlAssets pos
              , gopOpenedAt = plOpenedAt pos
              , gopDurationSeconds = max 0 (nowUnix - plOpenedAt pos)
              }
          | (acct, rows) <- Map.toList byAccount
          , let lifecycles = reconstructPositionLifecycles nowUnix mMarkRaw Nothing rows
          , Just pos <- [listToMaybe $ filter (\p -> plStatus p == "open" && plCurrentSize p > 0) lifecycles]
          , let effMark = fromMaybe (plEntryPrice pos) mMarkRaw
          , let notional = computeNotionalAssets (plCurrentSize pos) effMark
          , notional > 0
          ]
      longs = filter (\p -> gopSide p == 0) openPositions
      shorts = filter (\p -> gopSide p == 1) openPositions
      longOi = sum (map gopNotionalAssets longs)
      shortOi = sum (map gopNotionalAssets shorts)
      totalOi = longOi + shortOi
      (longShare, shortShare) =
        if totalOi <= 0
          then (Nothing, Nothing)
          else
            let ls = (longOi * 10000) `div` totalOi
             in (Just ls, Just (10000 - ls))
      levs = filter (> 0) (map gopEffectiveLeverageX100 openPositions)
      avgLev =
        if null levs
          then Nothing
          else Just (sum levs `div` fromIntegral (length levs))
   in GlobalMarketExposure
        { gmeLongOpenInterestAssets = longOi
        , gmeShortOpenInterestAssets = shortOi
        , gmeTotalOpenInterestAssets = totalOi
        , gmeNetSkewAssets = longOi - shortOi
        , gmeLongShareBps = longShare
        , gmeShortShareBps = shortShare
        , gmeOpenPositionCount = length openPositions
        , gmeLongCount = length longs
        , gmeShortCount = length shorts
        , gmeAvgLeverageX100 = avgLev
        , gmeTopPositions = take 10 openPositions
        }

-- | GET /api/insights/v1/market/exposure
getMarketExposureResponse
  :: DbPool
  -> Config
  -> IO (Either ApiError (ApiResponse Value))
getMarketExposureResponse pool cfg =
  snd <$> runSingleFlightCache marketExposureCache () isRight (computeMarketExposureResponse pool cfg)

computeMarketExposureResponse
  :: DbPool
  -> Config
  -> IO (Either ApiError (ApiResponse Value))
computeMarketExposureResponse pool cfg = do
  now <- floor <$> getPOSIXTime
  let chainId = cfgPerpsChainId cfg
      router = T.toLower $ T.strip $ cfgPerpsOrderRouter cfg
  (taggedRows, mMarkTuple, handlesMap, latestBlock) <-
    withDb pool $ \conn ->
      withReadSnapshot conn $ do
        mMark <- fetchLatestBasketMark conn now
        -- Identify accounts with net positive open size in the active release,
        -- then pull their position activity to reconstruct live open positions.
        activeAcctRows <-
          query
            conn
            "SELECT account \
            \FROM perps_account_activity \
            \WHERE chain_id = ? AND release_router = ? \
            \  AND activity_type IN ('Open', 'Close', 'Liquidated') \
            \GROUP BY account \
            \HAVING COALESCE(SUM(CASE WHEN activity_type = 'Open' THEN COALESCE(size_delta, 0) ELSE -COALESCE(size_delta, 0) END), 0) > 0 \
            \ORDER BY MAX(block_number) DESC \
            \LIMIT 150"
            (chainId, router) :: IO [Only Text]
        let accounts = map fromOnly activeAcctRows
        if null accounts
          then pure ([], mMark, Map.empty, 0)
          else do
            rows <-
              query
                conn
                "SELECT account, activity_type, order_id, side, price, size_delta, \
                \amount_assets, pnl_assets, tx_hash, block_number, log_index, timestamp \
                \FROM perps_account_activity \
                \WHERE chain_id = ? AND release_router = ? \
                \  AND account IN ? \
                \  AND activity_type IN ('Open', 'Close', 'Liquidated', 'Add margin') \
                \ORDER BY block_number ASC, log_index ASC"
                (chainId, router, In accounts) :: IO [AccountTaggedActivityRow]
            handleRows <-
              query
                conn
                "SELECT trading_account, discord_username \
                \FROM insights_divisions_links \
                \WHERE trading_account IN ?"
                (Only (In accounts)) :: IO [(Text, Text)]
            let hMap = Map.fromList [(T.toLower a, T.strip h) | (a, h) <- handleRows, not (T.null (T.strip h))]
                maxBlk = foldl max 0 (map atarBlockNumber rows)
            pure (rows, mMark, hMap, maxBlk)
  let mMarkRaw = fst <$> mMarkTuple
      mMarkTs = snd <$> mMarkTuple
      byAccount =
        foldl
          ( \acc row ->
              let pRow =
                    PositionActivityRow
                      { parActivityType = atarActivityType row
                      , parOrderId = atarOrderId row
                      , parSide = atarSide row
                      , parPrice = atarPrice row
                      , parSizeDelta = atarSizeDelta row
                      , parAmountAssets = atarAmountAssets row
                      , parPnlAssets = atarPnlAssets row
                      , parFeeAssets = Nothing
                      , parVpiAssets = Nothing
                      , parKeeperBountyAssets = Nothing
                      , parTxHash = atarTxHash row
                      , parBlockNumber = atarBlockNumber row
                      , parLogIndex = atarLogIndex row
                      , parTimestamp = atarTimestamp row
                      , parExecution = Nothing
                      }
               in Map.insertWith (\new old -> old <> new) (T.toLower (atarAccount row)) [pRow] acc
          )
          Map.empty
          taggedRows
      exposure = computeGlobalMarketExposure now mMarkRaw handlesMap byAccount
  pure $
    Right $
      mkResponse latestBlock chainId $
        object
          [ "network"
              .= object
                [ "chainId" .= chainId
                , "releaseVersion" .= Manifest.releaseVersion
                , "settlementAssetSymbol" .= ("USDG" :: Text)
                ]
          , "observedAt" .= isoTimestamp now
          , "currentMarkPrice" .= (show <$> mMarkRaw)
          , "markObservedAt" .= (isoTimestamp <$> mMarkTs)
          , "summary"
              .= object
                [ "longOpenInterestAssets" .= show (gmeLongOpenInterestAssets exposure)
                , "shortOpenInterestAssets" .= show (gmeShortOpenInterestAssets exposure)
                , "totalOpenInterestAssets" .= show (gmeTotalOpenInterestAssets exposure)
                , "netSkewAssets" .= show (gmeNetSkewAssets exposure)
                , "longShareBps" .= gmeLongShareBps exposure
                , "shortShareBps" .= gmeShortShareBps exposure
                , "openPositionCount" .= gmeOpenPositionCount exposure
                , "longCount" .= gmeLongCount exposure
                , "shortCount" .= gmeShortCount exposure
                , "avgLeverageX100" .= gmeAvgLeverageX100 exposure
                ]
          , "positions" .= map globalOpenPositionToJson (gmeTopPositions exposure)
          ]

globalOpenPositionToJson :: GlobalOpenPosition -> Value
globalOpenPositionToJson GlobalOpenPosition {..} =
  object
    [ "account" .= gopAccount
    , "discordUsername" .= gopDiscordUsername
    , "positionId" .= gopPositionId
    , "market" .= ("plDXY-PERP" :: Text)
    , "side" .= sideText gopSide
    , "size" .= show gopSize
    , "notionalAssets" .= show gopNotionalAssets
    , "marginAssets" .= show gopMarginAssets
    , "entryPrice" .= show gopEntryPrice
    , "markPrice" .= show gopMarkPrice
    , "liquidationPrice" .= (show <$> gopLiquidationPrice)
    , "distanceToLiquidationBps" .= gopDistanceToLiquidationBps
    , "effectiveLeverageX100" .= gopEffectiveLeverageX100
    , "unrealizedPnlAssets" .= show gopUnrealizedPnlAssets
    , "openedAt" .= isoTimestamp gopOpenedAt
    , "durationSeconds" .= gopDurationSeconds
    ]
