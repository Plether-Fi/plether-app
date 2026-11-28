module Plether.Handlers.InsightsFadRiskSpec (spec) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Database.PostgreSQL.Simple.Types (Query (..))
import Plether.Ethereum.Abi (encodeUint256)
import Plether.Ethereum.Contracts.CfdEngineAccountLens (AccountLedgerSnapshot (..))
import Plether.Handlers.InsightsFadRisk
  ( FadDangerStatus (..)
  , FadPositionRisk (..)
  , FadRiskSummary (..)
  , aggregateFadRiskSummary
  , computeProjectedCarryAssets
  , decodeMarketClosureTuple
  , decodePositionMaxProfitAssets
  , evaluatePositionFadRisk
  , openAccountsForFadRiskSql
  )
import Plether.Perps.MarketCalendar (MarketClosure (..))
import Test.Hspec

mkSnap :: Integer -> Integer -> Integer -> Integer -> Integer -> Bool -> AccountLedgerSnapshot
mkSnap side size margin entryPrice netEquity liquidatable =
  AccountLedgerSnapshot
    { alsSettlementBalanceAssets = margin
    , alsFreeSettlementAssets = 0
    , alsActivePositionMarginAssets = margin
    , alsOtherLockedMarginAssets = 0
    , alsPositionMarginBucketAssets = margin
    , alsCommittedOrderMarginBucketAssets = 0
    , alsReservedSettlementBucketAssets = 0
    , alsExecutionBountyReserveAssets = 0
    , alsCommittedMarginAssets = 0
    , alsTraderClaimBalanceAssets = 0
    , alsPendingOrderCount = 0
    , alsCloseReachableAssets = margin
    , alsLiquidationReachableSettlementAssets = margin
    , alsTerminalPriceCollectibleCapAssets = margin
    , alsAccountEquityAssets = margin
    , alsFreeBuyingPowerAssets = 0
    , alsHasPosition = size > 0
    , alsSide = side
    , alsSize = size
    , alsMargin = margin
    , alsEntryPrice = entryPrice
    , alsUnrealizedPnlAssets = netEquity - margin
    , alsNetEquityAssets = netEquity
    , alsLiquidatable = liquidatable
    }

spec :: Spec
spec = describe "Plether.Handlers.InsightsFadRisk" $ do
  it "computes flat annual carry projection matching PositionRiskAccountingLib" $ do
    -- 1,000,000 USDG (1e12) maxProfit at 500 bps (5%) for 1 full year -> 50,000 USDG (50_000_000_000)
    computeProjectedCarryAssets 1000000000000 500 (365 * 86400) `shouldBe` 50000000000
    -- 0 seconds -> 0 carry
    computeProjectedCarryAssets 1000000000000 500 0 `shouldBe` 0
    -- 1 day at 5% on 365,000 USDG: floor(500e14 / 365) * 365e9 / 1e18 = 49,999,999 (rounds down at index increment)
    computeProjectedCarryAssets 365000000000 500 86400 `shouldBe` 49999999

  it "classifies 45x leveraged position as would_liquidate_at_fad with exact top-up needed" $ do
    -- Size = 100,000 plDXY (1e23) at rawPrice 1.00 (1e8) -> notional = 100,000 USDG (100_000_000_000)
    -- Equity = 2,200 USDG (~45.45x leverage): safe under 10 bps maint (100 USDG req),
    -- but under 300 bps FAD (3,000 USDG req) it is short by 800 USDG (+ projected carry).
    let size = 100000 * 1000000000000000000
        markRaw = 100000000
        equity = 2200000000
        snap = mkSnap 0 size equity markRaw equity False
        mRisk = evaluatePositionFadRisk "0xAAA" (Just "alice") snap (Just 100000000000) markRaw 10 300 500 0
    case mRisk of
      Nothing -> expectationFailure "expected evaluated position"
      Just r -> do
        fprDangerStatus r `shouldBe` FadWouldLiquidate
        fprNotionalAssets r `shouldBe` 100000000000
        fprFadRequiredMarginAssets r `shouldBe` 3000000000
        fprFadMarginBufferAssets r `shouldBe` (-800000000)
        fprMarginNeededToSurviveAssets r `shouldBe` 800000000
        fprDistanceToFadLiquidationBps r `shouldBe` Just 0
        fprEffectiveLeverageX100 r `shouldBe` 4545

  it "accounts for projected carry accrual pushing a borderline position into would_liquidate_at_fad" $ do
    -- Notional = 100,000 USDG, FAD req = 3,000 USDG.
    -- Current equity = 3,010 USDG (barely above 3,000 USDG right now),
    -- but 3 days of 5% carry on 100,000 maxProfit (~41.09 USDG) pushes projected equity to ~2,968.9 USDG.
    let size = 100000 * 1000000000000000000
        markRaw = 100000000
        equity = 3010000000
        snap = mkSnap 0 size equity markRaw equity False
        mRisk = evaluatePositionFadRisk "0xBBB" Nothing snap (Just 100000000000) markRaw 10 300 500 (3 * 86400)
    case mRisk of
      Nothing -> expectationFailure "expected evaluated position"
      Just r -> do
        fprProjectedCarryToFadAssets r `shouldSatisfy` (> 40000000)
        fprDangerStatus r `shouldBe` FadWouldLiquidate
        fprMarginNeededToSurviveAssets r `shouldSatisfy` (> 30000000)

  it "distinguishes critical (<=50 bps), at_risk (<=150 bps), safe (>150 bps), and liquidatable_now" $ do
    let size = 100000 * 1000000000000000000
        markRaw = 100000000
        -- Critical: 3,300 USDG equity (300 USDG = ~30 bps buffer above 3,000 USDG FAD req)
        critSnap = mkSnap 0 size 3300000000 markRaw 3300000000 False
        -- At risk: 4,000 USDG equity (1,000 USDG = ~100 bps buffer)
        riskSnap = mkSnap 1 size 4000000000 markRaw 4000000000 False
        -- Safe: 10,000 USDG equity (10x leverage -> ~700 bps buffer)
        safeSnap = mkSnap 1 size 10000000000 markRaw 10000000000 False
        -- Already liquidatable now
        liqSnap = mkSnap 0 size 50000000 markRaw 50000000 True

        mAll =
          (,,,)
            <$> evaluatePositionFadRisk "0x1" Nothing critSnap Nothing markRaw 10 300 500 0
            <*> evaluatePositionFadRisk "0x2" Nothing riskSnap Nothing markRaw 10 300 500 0
            <*> evaluatePositionFadRisk "0x3" Nothing safeSnap Nothing markRaw 10 300 500 0
            <*> evaluatePositionFadRisk "0x4" Nothing liqSnap Nothing markRaw 10 300 500 0

    case mAll of
      Nothing -> expectationFailure "expected all four positions to evaluate"
      Just (crit, risk, safe, liq) -> do
        fprDangerStatus crit `shouldBe` FadCritical
        fprDangerStatus risk `shouldBe` FadAtRisk
        fprDangerStatus safe `shouldBe` FadSafe
        fprDangerStatus liq `shouldBe` FadLiquidatableNow

        let summary = aggregateFadRiskSummary [liq, crit, risk, safe]
        frsTotalOpenPositions summary `shouldBe` 4
        frsLiquidatableNowCount summary `shouldBe` 1
        frsCriticalCount summary `shouldBe` 1
        frsAtRiskCount summary `shouldBe` 1
        frsSafeCount summary `shouldBe` 1
        frsEndangeredCount summary `shouldBe` 3
        frsImmediateLiquidationLongNotionalAssets summary `shouldBe` 100000000000
        frsImmediateLiquidationShortNotionalAssets summary `shouldBe` 0
        frsEndangeredLongNotionalAssets summary `shouldBe` 200000000000
        frsEndangeredShortNotionalAssets summary `shouldBe` 100000000000

  it "decodes marketClosures(uint256) and positions(address).maxProfitAssets ABI payloads" $ do
    let closureBytes = BS.concat [encodeUint256 1796000000, encodeUint256 1796086400]
    decodeMarketClosureTuple closureBytes `shouldBe` Just (MarketClosure 1796000000 1796086400)
    let posBytes = BS.concat [encodeUint256 1, encodeUint256 2, encodeUint256 3, encodeUint256 987654321]
    decodePositionMaxProfitAssets posBytes `shouldBe` Just 987654321

  it "uses exact postgresql-simple placeholder count in openAccountsForFadRiskSql" $ do
    let countPlaceholders (Query bs) = BS8.count '?' bs
    countPlaceholders openAccountsForFadRiskSql `shouldBe` 5
