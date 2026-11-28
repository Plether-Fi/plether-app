module Plether.Handlers.InsightsLiquiditySpec (spec) where

import Data.Aeson (object, (.=))
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Database.PostgreSQL.Simple.Types (Query (..))
import Plether.Database.VaultPerformance (VaultPerformanceSnapshotRow (..))
import Plether.Handlers.InsightsLiquidity
  ( LpSettlementObservationRow (..)
  , OiDeltaBucketAgg (..)
  , OrderBucketAgg (..)
  , PoolEfficiencyBucket (..)
  , PoolEfficiencySummary (..)
  , ReturnReportResidual (..)
  , SnapshotFreshnessSummary (..)
  , auditReturnReportResidual
  , buildPoolEfficiencyAnalytics
  , classifySettlementReadiness
  , computeAnnualizedYieldBps
  , computeRatioBps
  , computeRatioX100
  , computeTakeRateBpsX100
  , decodeBitPositions
  , efficiencyOrderBucketsSql
  , efficiencyOrderWindowTotalsSql
  , executionPathLabel
  , healthStateLabel
  , latestBasketMarkSql
  , oiDeltaBucketsSql
  , priorOiSizesSql
  , sharePriceDeltaBps
  , summarizeSnapshotFreshness
  )
import Test.Hspec

baseObservation :: LpSettlementObservationRow
baseObservation =
  LpSettlementObservationRow
    { lsorEpoch = 496900
    , lsorObservedBlock = 325000000
    , lsorExecutionPath = 2
    , lsorOperationalBlockerMask = 0
    , lsorWarningMask = 0
    , lsorDependencyFailureMask = 0
    , lsorCriticalFaultMask = 0
    , lsorHealthState = Just 1
    , lsorObservationComplete = Just True
    , lsorHasMaturedWork = Just True
    , lsorPaused = Just False
    , lsorLastObservedAt = posixSecondsToUTCTime 1795694400
    }

sampleSnapshot :: Integer -> Maybe Bool -> VaultPerformanceSnapshotRow
sampleSnapshot epoch fresh =
  VaultPerformanceSnapshotRow
    { vpsChainId = 421614
    , vpsHousePoolAddress = "0xeb5604c393843b2f3c347ec24fa9ddad6ab2131e"
    , vpsSeniorVaultAddress = "0x5dc42ba5a1e8dd3e1b6868d10f66a9bf8fb5b83d"
    , vpsJuniorVaultAddress = "0x4a8b67184a1e3224c11a191cc69f22c50eef734e"
    , vpsEpochTimestamp = epoch
    , vpsBlockNumber = epoch
    , vpsBlockHash = "0x" <> T.replicate 64 "a"
    , vpsBlockTimestamp = epoch
    , vpsMarkFresh = fresh
    , vpsSeniorTotalAssets = 1000000
    , vpsSeniorLockedAssets = Just 1000000
    , vpsJuniorLockedAssets = Just 500000
    , vpsSeniorTotalSupply = 1000000
    , vpsSeniorSharePriceWad = 1000000000000000000
    , vpsJuniorTotalAssets = 500000
    , vpsJuniorTotalSupply = 500000
    , vpsJuniorSharePriceWad = 1000000000000000000
    }

spec :: Spec
spec = describe "Plether.Handlers.InsightsLiquidity" $ do
  it "maps execution path and health state enums cleanly" $ do
    executionPathLabel 1 `shouldBe` "no_matured_work"
    executionPathLabel 2 `shouldBe` "cached_mark"
    executionPathLabel 3 `shouldBe` "atomic_oracle_refresh"
    executionPathLabel 99 `shouldBe` "unknown"
    healthStateLabel (Just 0) `shouldBe` "unknown"
    healthStateLabel (Just 1) `shouldBe` "healthy"
    healthStateLabel (Just 2) `shouldBe` "degraded"
    healthStateLabel (Just 3) `shouldBe` "faulted"
    healthStateLabel Nothing `shouldBe` "unknown"

  it "decodes active bit positions from settlement masks" $ do
    decodeBitPositions 0 `shouldBe` []
    decodeBitPositions 1 `shouldBe` [0]
    decodeBitPositions 13 `shouldBe` [0, 2, 3]

  it "classifies LP settlement readiness in strict severity order" $ do
    classifySettlementReadiness baseObservation `shouldBe` "ready"
    classifySettlementReadiness baseObservation {lsorWarningMask = 2} `shouldBe` "ready_with_warnings"
    classifySettlementReadiness baseObservation {lsorObservationComplete = Just False} `shouldBe` "incomplete"
    classifySettlementReadiness baseObservation {lsorOperationalBlockerMask = 1} `shouldBe` "blocked"
    classifySettlementReadiness baseObservation {lsorDependencyFailureMask = 4} `shouldBe` "blocked"
    classifySettlementReadiness baseObservation {lsorCriticalFaultMask = 1} `shouldBe` "faulted"
    classifySettlementReadiness baseObservation {lsorPaused = Just True, lsorCriticalFaultMask = 1} `shouldBe` "paused"

  it "summarizes fresh vs stale vault performance snapshots accurately" $ do
    let rows =
          [ sampleSnapshot 3600 (Just True)
          , sampleSnapshot 7200 (Just False)
          , sampleSnapshot 10800 (Just True)
          ]
        summary = summarizeSnapshotFreshness 169 rows
    sfsExpectedHours summary `shouldBe` 169
    sfsObservedHours summary `shouldBe` 3
    sfsFreshHours summary `shouldBe` 2
    sfsStaleHours summary `shouldBe` 1
    sfsLatestEpochTimestamp summary `shouldBe` Just 10800
    sfsLatestMarkFresh summary `shouldBe` Just True

  it "audits Junior return attribution residuals exactly in USDG and percentage points" $ do
    let comp pp amt = object ["contributionPp" .= (pp :: Double), "amountUsdg" .= (amt :: String)]
        report =
          object
            [ "startDilutedSharePrice" .= ("1000000000000000000" :: String)
            , "endDilutedSharePrice" .= ("1010000000000000000" :: String)
            , "netReturnUsdg" .= ("1000000" :: String)
            , "sourceBlockCount" .= (42 :: Int)
            , "notes" .= (["clean window"] :: [String])
            , "components"
                .= object
                  [ "netVpi" .= comp 0.5 "500000"
                  , "liquidationFees" .= comp 0.2 "200000"
                  , "realizedCarry" .= comp 0.3 "300000"
                  , "oracleSpread" .= comp 0.0 "0"
                  , "settledTraderPnl" .= comp 0.0 "0"
                  , "openPositionValuation" .= comp 0.0 "0"
                  , "seniorCoupon" .= comp 0.0 "0"
                  , "maintenanceDilution" .= comp 0.0 "0"
                  ]
            ]
        residual = auditReturnReportResidual report
    rrrNetReturnUsdg residual `shouldBe` Just 1000000
    rrrUnexplainedUsdg residual `shouldBe` Just 0
    rrrLiquidationFeesUsdg residual `shouldBe` Just 200000
    rrrSourceBlockCount residual `shouldBe` Just 42
    rrrNotesCount residual `shouldBe` 1

  it "computes hourly share price deltas in bps and fails closed on non-positive baselines" $ do
    sharePriceDeltaBps 1010000000000000000 1000000000000000000 `shouldBe` Just 100
    sharePriceDeltaBps 995000000000000000 1000000000000000000 `shouldBe` Just (-50)
    sharePriceDeltaBps 1000000000000000000 0 `shouldBe` Nothing

  it "computes pool turnover, OI utilization, net skew, take-rate, and annualized fee yield cleanly" $ do
    computeRatioX100 200000000000000 800000000000 `shouldBe` Just 25000 -- 250.00x
    computeRatioX100 100 0 `shouldBe` Nothing
    computeRatioBps 600000 1000000 `shouldBe` Just 6000 -- 60.00%
    computeTakeRateBpsX100 700000000 1000000000000 `shouldBe` Just 700 -- 7.00 bps
    computeAnnualizedYieldBps 7000000000 7 1000000000000 `shouldBe` Just 3650 -- 36.50% APR

  it "reconstructs 7d (6h) and 30d (daily) pool efficiency & open interest series accurately" $ do
    let nowTs = 1795867200 :: Integer -- aligned UTC timestamp
        bucket6h = 21600 :: Integer
        end6h = ((nowTs `div` bucket6h) + 1) * bucket6h
        lastBucketStart = end6h - bucket6h
        snap =
          (sampleSnapshot (nowTs - 3600) (Just True))
            { vpsSeniorTotalAssets = 600000000000 -- 600k USDG
            , vpsJuniorTotalAssets = 200000000000 -- 200k USDG -> 800k total pool
            }
        -- Prior OI: 200k plDXY long, 100k plDXY short @ raw price 1.00 (1e8)
        priorOi = (200000000000000000000000, 100000000000000000000000)
        -- Last 6h bucket adds +100k long, +50k short -> 300k long, 150k short -> 450k gross, +150k net skew
        oiBuckets =
          Map.singleton
            lastBucketStart
            (OiDeltaBucketAgg 100000000000000000000000 50000000000000000000000 (Just 100000000))
        orderBuckets =
          Map.singleton
            lastBucketStart
            (OrderBucketAgg 120 40000000000000 28000000000) -- 40M vol, 28k fees in 6h
        windowTotals =
          ( 160000000000000 -- 160M 24h vol -> 200.00x on 800k pool
          , 800000000000000 -- 800M 7d vol
          , 2400000000000000 -- 2.4B 30d vol
          , 112000000000 -- 112k 24h fees
          , 560000000000 -- 560k 7d fees -> 7.00 bps take rate
          )
        (summary, s7d, s30d) =
          buildPoolEfficiencyAnalytics
            nowTs
            [snap]
            orderBuckets
            priorOi
            oiBuckets
            (Just 100000000)
            windowTotals

    length s7d `shouldBe` 28
    length s30d `shouldBe` 30
    pesPoolTvlAssets summary `shouldBe` Just 800000000000
    pesTurnover24hX100 summary `shouldBe` Just 20000 -- 200.00x
    pesEffectiveTakeRate7dBpsX100 summary `shouldBe` Just 700 -- 7.00 bps
    pesCurrentLongOiAssets summary `shouldBe` 300000000000
    pesCurrentShortOiAssets summary `shouldBe` 150000000000
    pesCurrentGrossOiAssets summary `shouldBe` 450000000000
    pesCurrentNetSkewAssets summary `shouldBe` 150000000000
    pesGrossOiToPoolBps summary `shouldBe` Just 5625 -- 450k / 800k = 56.25%
    pesNetSkewToPoolBps summary `shouldBe` Just 1875 -- 150k / 800k = 18.75%
    pesNetSkewToJuniorBps summary `shouldBe` Just 7500 -- 150k / 200k = 75.00%
    -- 6h bucket annualized daily turnover = 4 * 40M / 800k = 200.00x
    pebAnnualizedTurnoverDailyX100 (last s7d) `shouldBe` Just 20000
    pebGrossOiAssets (last s30d) `shouldBe` 450000000000

  it "uses exact postgresql-simple parameter placeholder counts in efficiency SQL queries" $ do
    let countPlaceholders (Query bs) = BS8.count '?' bs
    countPlaceholders efficiencyOrderBucketsSql `shouldBe` 3
    countPlaceholders efficiencyOrderWindowTotalsSql `shouldBe` 8
    countPlaceholders priorOiSizesSql `shouldBe` 3
    countPlaceholders oiDeltaBucketsSql `shouldBe` 3
    countPlaceholders latestBasketMarkSql `shouldBe` 0
