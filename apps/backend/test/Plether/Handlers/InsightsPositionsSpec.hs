{-# LANGUAGE OverloadedStrings #-}

module Plether.Handlers.InsightsPositionsSpec (spec) where

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Plether.Ethereum.Abi (encodeUint256)
import Plether.Handlers.InsightsPositions
  ( ExposureTimelinePoint (..)
  , GlobalMarketExposure (..)
  , GlobalOpenAggregate (..)
  , GlobalOpenPosition (..)
  , OnChainMarketSides (..)
  , PositionActivityRow (..)
  , PositionLifecycle (..)
  , PositionStepEvent (..)
  , PriceCandleRow (..)
  , WalletDirectoryEntry (..)
  , WalletExposureSummary (..)
  , buildExposureTimeline
  , computeGlobalMarketExposure
  , computeLeverageX100
  , computeNotionalAssets
  , computeUnrealizedPnl
  , decodeMarketSideTuple
  , defaultMaintenanceMarginBps
  , distanceToLiquidationBps
  , enrichPositionWithCandles
  , filterSortWalletsDirectory
  , findLiquidationRawPrice
  , pickCandleIntervalSeconds
  , reconcileGlobalMarketExposure
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

  it "decodes CfdEngine.sides() ABI tuples and reconciles canonical on-chain OI + full counts" $ do
    let bullOi = 228935000 * 10 ^ (18 :: Int)
        bearOi = 183764200 * 10 ^ (18 :: Int)
        bullMargin = 162053408260000
        bearMargin = 170711969440000
        encodedBull = BS.concat [encodeUint256 999, encodeUint256 bullOi, encodeUint256 1234, encodeUint256 bullMargin]
    decodeMarketSideTuple encodedBull `shouldBe` Just (bullOi, bullMargin)
    decodeMarketSideTuple (BS.replicate 64 0) `shouldBe` Nothing

    let markRaw = 94605602
        base = computeGlobalMarketExposure 1800003600 (Just markRaw) Map.empty Map.empty
        sides =
          OnChainMarketSides
            { ocmsLongOpenInterest = bullOi
            , ocmsShortOpenInterest = bearOi
            , ocmsLongTotalMarginAssets = bullMargin
            , ocmsShortTotalMarginAssets = bearMargin
            }
        agg =
          GlobalOpenAggregate
            { goaLongCount = 620
            , goaShortCount = 480
            , goaLongSize = bullOi
            , goaShortSize = bearOi
            }
        reconciled = reconcileGlobalMarketExposure (Just markRaw) (Just sides) (Just agg) base
    gmeLongOpenInterestAssets reconciled `shouldBe` computeNotionalAssets bullOi markRaw
    gmeShortOpenInterestAssets reconciled `shouldBe` computeNotionalAssets bearOi markRaw
    gmeNetSkewAssets reconciled `shouldSatisfy` (> 0)
    gmeOpenPositionCount reconciled `shouldBe` 1100
    gmeLongCount reconciled `shouldBe` 620
    gmeShortCount reconciled `shouldBe` 480
    gmeLongShareBps reconciled `shouldBe` Just 5547
    gmeShortShareBps reconciled `shouldBe` Just 4453
    gmeAvgLeverageX100 reconciled `shouldBe` Just 117

    -- Fallback to full SQL aggregate sizes when on-chain RPC is unavailable
    let fallback = reconcileGlobalMarketExposure (Just markRaw) Nothing (Just agg) base
    gmeLongOpenInterestAssets fallback `shouldBe` computeNotionalAssets bullOi markRaw
    gmeShortOpenInterestAssets fallback `shouldBe` computeNotionalAssets bearOi markRaw
    gmeOpenPositionCount fallback `shouldBe` 1100

  it "filters, searches, sorts, and paginates Trading Accounts directory entries" $ do
    let entries =
          [ WalletDirectoryEntry "0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" (Just "macro_alice") (Just "Gold") "long" 100 9500000000 12 6 6 0 120000000000 500000000 (Just 1700000000) (Just 1800000200)
          , WalletDirectoryEntry "0xbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb" Nothing Nothing "short" 200 19000000000 4 2 2 0 40000000000 (-150000000) (Just 1700000100) (Just 1800000100)
          , WalletDirectoryEntry "0xcccccccccccccccccccccccccccccccccccccccc" (Just "fx_carol") (Just "Silver") "flat" 0 0 25 12 13 0 250000000000 900000000 (Just 1700000200) (Just 1800000300)
          ]
    -- Default sort (notional DESC)
    let (totAll, byNotional) = filterSortWalletsDirectory Nothing Nothing Nothing 50 0 entries
    totAll `shouldBe` 3
    map wdeAccount byNotional
      `shouldBe` [ "0xbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
                 , "0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                 , "0xcccccccccccccccccccccccccccccccccccccccc"
                 ]
    -- Search by @handle (leading @ stripped, case-insensitive)
    let (totSearch, matchedSearch) = filterSortWalletsDirectory (Just "@MACRO_ALICE") Nothing Nothing 50 0 entries
    totSearch `shouldBe` 1
    map wdeAccount matchedSearch `shouldBe` ["0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"]
    -- Filter open + sort by volume
    let (totOpen, openByVol) = filterSortWalletsDirectory Nothing (Just "open") (Just "volume") 50 0 entries
    totOpen `shouldBe` 2
    map wdeAccount openByVol
      `shouldBe` [ "0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                 , "0xbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
                 ]
    -- Filter profitable + sort by pnl + pagination
    let (totProf, profPage) = filterSortWalletsDirectory Nothing (Just "profitable") (Just "pnl") 1 0 entries
    totProf `shouldBe` 2
    map wdeAccount profPage `shouldBe` ["0xcccccccccccccccccccccccccccccccccccccccc"]
