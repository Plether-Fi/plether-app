module Plether.Handlers.InsightsLiquiditySpec (spec) where

import Data.Aeson (object, (.=))
import qualified Data.Text as T
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Plether.Database.VaultPerformance (VaultPerformanceSnapshotRow (..))
import Plether.Handlers.InsightsLiquidity
  ( LpSettlementObservationRow (..)
  , ReturnReportResidual (..)
  , SnapshotFreshnessSummary (..)
  , auditReturnReportResidual
  , classifySettlementReadiness
  , decodeBitPositions
  , executionPathLabel
  , healthStateLabel
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
    , lsorHealthState = Just 0
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
    healthStateLabel (Just 0) `shouldBe` "healthy"
    healthStateLabel (Just 1) `shouldBe` "degraded"
    healthStateLabel (Just 2) `shouldBe` "faulted"
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

  it "computes hourly share price deltas in bps and fails closed on non-positive baselines" $ do
    sharePriceDeltaBps 1010000000000000000 1000000000000000000 `shouldBe` Just 100
    sharePriceDeltaBps 995000000000000000 1000000000000000000 `shouldBe` Just (-50)
    sharePriceDeltaBps 1000000000000000000 0 `shouldBe` Nothing
    rrrNotesCount residual `shouldBe` 1
