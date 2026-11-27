{-# LANGUAGE OverloadedStrings #-}

module Plether.Handlers.InsightsPositionsSpec (spec) where

import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Plether.Handlers.InsightsPositions
  ( ExposureTimelinePoint (..)
  , GlobalMarketExposure (..)
  , GlobalOpenPosition (..)
  , PositionActivityRow (..)
  , PositionLifecycle (..)
  , PositionStepEvent (..)
  , PriceCandleRow (..)
  , WalletExposureSummary (..)
  , buildExposureTimeline
  , computeGlobalMarketExposure
  , computeLeverageX100
  , computeNotionalAssets
  , computeUnrealizedPnl
  , defaultMaintenanceMarginBps
  , distanceToLiquidationBps
  , enrichPositionWithCandles
  , findLiquidationRawPrice
  , pickCandleIntervalSeconds
  , reconstructPositionLifecycles
  , summarizeWalletExposure
  )
import Test.Hspec

mkRow :: PositionActivityRow
mkRow =
  PositionActivityRow
    { parActivityType = "Open"
    , parOrderId = Nothing
    , parSide = Just 0
    , parPrice = Just 95000000
    , parSizeDelta = Just (10000 * 10 ^ (18 :: Int))
    , parAmountAssets = Just 500000000
    , parPnlAssets = Nothing
    , parFeeAssets = Just 3800000
    , parVpiAssets = Just 1200000
    , parKeeperBountyAssets = Nothing
    , parTxHash = "0x1111111111111111111111111111111111111111111111111111111111111111"
    , parBlockNumber = 100
    , parLogIndex = 1
    , parTimestamp = 1800000000
    , parExecution = Nothing
    }

spec :: Spec
spec = describe "Plether.Handlers.InsightsPositions" $ do
  it "computes canonical inverted plDXY notional, PnL, leverage, and liquidation threshold" $ do
    let size = 10000 * 10 ^ (18 :: Int) -- 10,000 plDXY
        entryRaw = 95000000 -- display 1.0500
        entryCost = computeNotionalAssets size entryRaw
    entryCost `shouldBe` 9500000000 -- 9,500.00 USDG
    -- Display price up to 1.0600 -> rawPrice drops to 94,000,000 -> +100 USDG for Long, -100 USDG for Short
    computeUnrealizedPnl 0 size entryCost 94000000 `shouldBe` 100000000
    computeUnrealizedPnl 1 size entryCost 94000000 `shouldBe` (-100000000)
    -- Leverage: 9,500 USDG notional on 500 USDG margin = 19.00x (1900)
    computeLeverageX100 entryCost 500000000 `shouldBe` 1900
    -- Liquidation price for Long should be above entryRaw (lower display price)
    let mLiqLong = findLiquidationRawPrice 0 size entryCost 500000000 0 defaultMaintenanceMarginBps
    mLiqLong `shouldSatisfy` isJust
    case mLiqLong of
      Just liqRaw -> do
        liqRaw `shouldSatisfy` (> entryRaw)
        liqRaw `shouldSatisfy` (< 101000000)
        case distanceToLiquidationBps 0 entryRaw (Just liqRaw) of
          Just bps -> do
            bps `shouldSatisfy` (> 400)
            bps `shouldSatisfy` (< 500)
          Nothing -> expectationFailure "Expected non-Nothing liquidation distance"
      Nothing -> expectationFailure "Expected liquidation boundary"

  it "reconstructs multi-fill lifecycles (open -> scale-in -> margin add -> partial close -> close)" $ do
    let s1 = 10000 * 10 ^ (18 :: Int)
        rows =
          [ mkRow
              { parActivityType = "Open"
              , parOrderId = Just 10
              , parSide = Just 0
              , parPrice = Just 95000000
              , parSizeDelta = Just s1
              , parAmountAssets = Just 500000000
              , parBlockNumber = 100
              , parLogIndex = 1
              , parTimestamp = 1800000000
              }
          , mkRow
              { parActivityType = "Open"
              , parOrderId = Just 11
              , parSide = Just 0
              , parPrice = Just 94000000
              , parSizeDelta = Just s1
              , parAmountAssets = Just 500000000
              , parBlockNumber = 110
              , parLogIndex = 2
              , parTimestamp = 1800000600
              }
          , mkRow
              { parActivityType = "Add margin"
              , parOrderId = Nothing
              , parSide = Nothing
              , parPrice = Nothing
              , parSizeDelta = Nothing
              , parAmountAssets = Just 250000000
              , parFeeAssets = Nothing
              , parVpiAssets = Nothing
              , parBlockNumber = 115
              , parLogIndex = 1
              , parTimestamp = 1800000900
              }
          , mkRow
              { parActivityType = "Close"
              , parOrderId = Just 12
              , parSide = Just 0
              , parPrice = Just 93000000
              , parSizeDelta = Just s1
              , parAmountAssets = Nothing
              , parPnlAssets = Just 150000000
              , parBlockNumber = 120
              , parLogIndex = 3
              , parTimestamp = 1800001200
              }
          , mkRow
              { parActivityType = "Close"
              , parOrderId = Just 13
              , parSide = Just 0
              , parPrice = Just 92000000
              , parSizeDelta = Just s1
              , parAmountAssets = Nothing
              , parPnlAssets = Just 250000000
              , parBlockNumber = 130
              , parLogIndex = 4
              , parTimestamp = 1800001800
              }
          ]
        lifecycles = reconstructPositionLifecycles 1800003600 (Just 92000000) Nothing rows
    length lifecycles `shouldBe` 1
    case lifecycles of
      [pos] -> do
        plPositionId pos `shouldBe` "pos-100-1"
        plStatus pos `shouldBe` "closed"
        plSide pos `shouldBe` 0
        plEntryPrice pos `shouldBe` 94500000 -- exact average of 95M and 94M preserved across partial close!
        plExitPrice pos `shouldBe` Just 92500000 -- exact average of 93M and 92M
        plRealizedPnlAssets pos `shouldBe` 400000000
        plScaleInCount pos `shouldBe` 1
        plPartialCloseCount pos `shouldBe` 1
        plMarginAddCount pos `shouldBe` 1
        plFillCount pos `shouldBe` 4
        map pseAction (plEvents pos) `shouldBe` ["Open", "Scale-in", "Add margin", "Partial close", "Close"]
      _ -> expectationFailure "Expected exactly one reconstructed position"

  it "enriches MFE, MAE, closest liquidation distance, timeline, and directional bias summary" $ do
    let s1 = 10000 * 10 ^ (18 :: Int)
        rows =
          [ mkRow
              { parActivityType = "Open"
              , parSide = Just 0
              , parPrice = Just 95000000
              , parSizeDelta = Just s1
              , parAmountAssets = Just 500000000
              , parBlockNumber = 200
              , parLogIndex = 1
              , parTimestamp = 1800000000
              }
          , mkRow
              { parActivityType = "Close"
              , parSide = Just 0
              , parPrice = Just 94000000
              , parSizeDelta = Just s1
              , parAmountAssets = Nothing
              , parPnlAssets = Just 100000000
              , parBlockNumber = 220
              , parLogIndex = 1
              , parTimestamp = 1800001800
              }
          , mkRow
              { parActivityType = "Open"
              , parSide = Just 1
              , parPrice = Just 94000000
              , parSizeDelta = Just s1
              , parAmountAssets = Just 500000000
              , parBlockNumber = 240
              , parLogIndex = 1
              , parTimestamp = 1800003600
              }
          ]
        -- Candles dip adversely to rawHigh = 97,000,000 (-200 USDG for Long) and rally to rawLow = 92,000,000 (+300 USDG)
        candles =
          [ PriceCandleRow 1800000300 95000000 97000000 94500000 96000000
          , PriceCandleRow 1800000900 96000000 96200000 92000000 93500000
          ]
        rawPositions = reconstructPositionLifecycles 1800005000 (Just 94500000) Nothing rows
        enriched = map (fst . enrichPositionWithCandles 1800005000 (Just 94500000) candles) rawPositions
        summary = summarizeWalletExposure 1800005000 enriched
        timeline = buildExposureTimeline 1800005000 (Just 94500000) enriched
    wesTotalPositions summary `shouldBe` 2
    wesOpenPositions summary `shouldBe` 1
    wesClosedPositions summary `shouldBe` 1
    wesLongPositions summary `shouldBe` 1
    wesShortPositions summary `shouldBe` 1
    wesWinCount summary `shouldBe` 1
    wesWinRateBps summary `shouldBe` Just 10000
    -- Verify the closed long captured MFE (+300 USDG) and MAE (-200 USDG)
    case filter (\p -> plStatus p == "closed") enriched of
      [closedLong] -> do
        plMfePnlAssets closedLong `shouldBe` 300000000
        plMaePnlAssets closedLong `shouldBe` (-200000000)
        plMfePrice closedLong `shouldBe` Just 92000000
        plMaePrice closedLong `shouldBe` Just 97000000
        plClosestLiquidationBps closedLong `shouldSatisfy` isJust
      _ -> expectationFailure "Expected one closed long position"
    -- Verify signed timeline flips from positive (long) -> 0 (flat) -> negative (short)
    any (\pt -> etpSignedNotionalAssets pt > 0) timeline `shouldBe` True
    any (\pt -> etpSignedNotionalAssets pt == 0) timeline `shouldBe` True
    any (\pt -> etpSignedNotionalAssets pt < 0) timeline `shouldBe` True

  it "selects appropriate canonical candle intervals for different holding durations" $ do
    pickCandleIntervalSeconds 1800 `shouldBe` 60
    pickCandleIntervalSeconds 36000 `shouldBe` 300
    pickCandleIntervalSeconds (3 * 86400) `shouldBe` 900
    pickCandleIntervalSeconds (14 * 86400) `shouldBe` 3600
    pickCandleIntervalSeconds (45 * 86400) `shouldBe` 86400

  it "computes global long/short market exposure leaderboard sorted by notional" $ do
    let sLarge = 20000 * 10 ^ (18 :: Int)
        sSmall = 10000 * 10 ^ (18 :: Int)
        byAccount =
          Map.fromList
            [ ( "0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
              , [ mkRow {parSide = Just 0, parPrice = Just 95000000, parSizeDelta = Just sLarge, parAmountAssets = Just 1000000000} ]
              )
            , ( "0xbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
              , [ mkRow {parSide = Just 1, parPrice = Just 95000000, parSizeDelta = Just sSmall, parAmountAssets = Just 500000000} ]
              )
            ]
        handles = Map.singleton "0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" "whale_alice"
        globalExp = computeGlobalMarketExposure 1800003600 (Just 94500000) handles byAccount
    gmeOpenPositionCount globalExp `shouldBe` 2
    gmeLongCount globalExp `shouldBe` 1
    gmeShortCount globalExp `shouldBe` 1
    gmeLongShareBps globalExp `shouldBe` Just 6666
    gmeShortShareBps globalExp `shouldBe` Just 3334
    map gopAccount (gmeTopPositions globalExp)
      `shouldBe` ["0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", "0xbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"]
    map gopDiscordUsername (gmeTopPositions globalExp)
      `shouldBe` [Just "whale_alice", Nothing]
