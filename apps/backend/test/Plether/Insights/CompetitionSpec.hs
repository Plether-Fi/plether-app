module Plether.Insights.CompetitionSpec (spec) where

import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import Plether.Database.Insights
  ( CompetitionSeedMetadata (..)
  , CompetitionSeedMismatch (..)
  , competitionSeedMetadataFor
  , competitionSeedMismatches
  , isLegacyPaymentDeadlineOnlyMismatch
  )
import Plether.Insights.Competition
import Test.Hspec

spec :: Spec
spec = do
  describe "prizeAllocation" $ do
    let prizes = map (* usdcScale) [600, 300, 100]

    it "assigns an untied eligible participant its occupied prize" $ do
      prizeAllocation prizes (Just 2) (Just 1)
        `shouldBe` Just (PrizeAllocation 2 [2] $ 300 * usdcScale)

    it "splits the combined occupied prizes equally across an exact tie" $ do
      prizeAllocation prizes (Just 1) (Just 2)
        `shouldBe` Just (PrizeAllocation 1 [1, 2] $ 450 * usdcScale)

    it "shares the last paid place across a tie that extends beyond the podium" $ do
      prizeAllocation prizes (Just 3) (Just 2)
        `shouldBe` Just (PrizeAllocation 3 [3] $ 50 * usdcScale)

    it "does not create an award outside the paid places" $ do
      prizeAllocation prizes (Just 4) (Just 1) `shouldBe` Nothing
      prizeAllocation prizes Nothing Nothing `shouldBe` Nothing

  describe "finalizationBlockers" $ do
    let ready =
          FinalizationReadiness
            { frNowTimestamp = 200
            , frScoreCutoffTimestamp = 100
            , frResultsTimestamp = 150
            , frStartBlock = Just 100
            , frScoreCutoffBlock = Just 123
            , frParticipantCount = 2
            , frMissingTraderReferences = 0
            , frUnresolvedReviews = 0
            , frStartSnapshotCount = 2
            , frFinalSnapshotCount = 2
            , frFinalSnapshotHashCount = 1
            }

    it "allows finalization only when the closeout data and reviews are complete" $ do
      finalizationBlockers ready `shouldBe` []

    it "reports every unresolved closeout prerequisite" $ do
      finalizationBlockers
        (ready
          { frNowTimestamp = 99
          , frScoreCutoffBlock = Nothing
          , frMissingTraderReferences = 1
          , frUnresolvedReviews = 2
          , frFinalSnapshotCount = 1
          })
        `shouldBe`
          [ "the scoring cutoff has not passed"
          , "the scheduled results publication time has not arrived"
          , "the canonical final block has not been resolved"
          , "1 participant registration(s) are missing a private trader reference"
          , "2 participant review(s) are still pending or under review"
          , "final snapshots are incomplete: 1 of 2 registered wallets are snapshotted at the final block"
          ]

    it "requires one common canonical hash for the complete final batch" $ do
      finalizationBlockers (ready {frFinalSnapshotHashCount = 2})
        `shouldBe` ["the final snapshots do not share one canonical block hash"]

  describe "July 2026 competition defaults" $ do
    it "uses the published bankroll, threshold, active-day minimum, and prizes" $ do
      crSlug july2026Competition `shouldBe` "testnet-trading-2026"
      crStartingBalanceUsdc july2026Competition `shouldBe` 100_000_000_000
      minimumProfitUsdc july2026Competition `shouldBe` 1_000_000_000
      crMinimumActiveDays july2026Competition `shouldBe` 5
      crPrizeUsdc july2026Competition `shouldBe` [600_000_000, 300_000_000, 100_000_000]
      crPaymentDeadlineAt july2026Competition
        `shouldBe` UTCTime (fromGregorian 2026 8 7) (secondsToDiffTime $ 22 * 60 * 60)

  describe "immutable competition seed metadata" $ do
    let expected = competitionSeedMetadataFor
          july2026Competition
          421_614
          " 0xAa00000000000000000000000000000000000001 "
          "0xBb00000000000000000000000000000000000002"
          "0xCc00000000000000000000000000000000000003"

    it "normalizes deployment addresses before persistence and comparison" $ do
      csmReleaseRouter expected `shouldBe` "0xaa00000000000000000000000000000000000001"
      csmUsdcAddress expected `shouldBe` "0xbb00000000000000000000000000000000000002"
      csmMarginClearinghouseAddress expected `shouldBe` "0xcc00000000000000000000000000000000000003"

    it "identifies deployment and scoring changes instead of silently accepting them" $ do
      let stored = expected
            { csmChainId = 1
            , csmMinimumProfitBps = 200
            }
      competitionSeedMismatches expected stored
        `shouldBe`
          [ CompetitionSeedMismatch "chain_id" "1" "421614"
          , CompetitionSeedMismatch "minimum_profit_bps" "200" "100"
          ]

    it "recognizes only the exact pre-launch payout-deadline correction" $ do
      let legacy = expected {csmPaymentDeadlineTimestamp = 1_786_319_999}
      isLegacyPaymentDeadlineOnlyMismatch expected legacy `shouldBe` True
      isLegacyPaymentDeadlineOnlyMismatch
        expected
        legacy {csmRulesVersion = "different"}
        `shouldBe` False

    it "reports no mismatches for an idempotent restart" $ do
      competitionSeedMismatches expected expected `shouldBe` []

  describe "economicAccountValue" $ do
    it "uses terminal reachable collateral for a flat account and includes claims" $ do
      economicAccountValue (snapshot False 80_000_000 100_000_000 5_000_000)
        `shouldBe` 105_000_000

    it "uses signed net equity for an open position and includes claims" $ do
      economicAccountValue (snapshot True 90_000_000 100_000_000 5_000_000)
        `shouldBe` 95_000_000

    it "floors limited-liability account value at zero" $ do
      economicAccountValue (snapshot True (-10_000_000) 100_000_000 2_000_000)
        `shouldBe` 0

  describe "calculateScore" $ do
    it "neutralizes deposits and withdrawals and applies audited adjustments" $ do
      let score = calculateScore $
            ScoreInput
              { siStartingSnapshot = snapshot False 0 100_000_000_000 0
              , siCurrentSnapshot = snapshot False 0 111_500_000_000 0
              , siDepositsSinceStartUsdc = 10_000_000_000
              , siWithdrawalsSinceStartUsdc = 500_000_000
              , siManualAdjustmentsUsdc = 250_000_000
              }
      sbFinalPnlUsdc score `shouldBe` 2_250_000_000

  describe "qualification" $ do
    it "treats exactly +1% and five active days as mechanically qualified" $ do
      let result = qualification july2026Competition EligibilityPending 1_000_000_000 5
      qMeetsProfitRequirement result `shouldBe` True
      qMeetsActiveDaysRequirement result `shouldBe` True
      qMechanicallyQualified result `shouldBe` True
      qPrizeEligible result `shouldBe` False

    it "requires an eligible integrity review before awarding a prize" $ do
      qPrizeEligible (qualification july2026Competition EligibilityEligible 1_000_000_000 5)
        `shouldBe` True
      qPrizeEligible (qualification july2026Competition EligibilityIneligible 5_000_000_000 10)
        `shouldBe` False

    it "does not round a sub-threshold score up to +1%" $ do
      qMeetsProfitRequirement (qualification july2026Competition EligibilityEligible 999_999_999 5)
        `shouldBe` False

  describe "FX active sessions" $ do
    it "maps the Sunday 22:00 UTC reopen to Monday" $ do
      fxSessionDay 1_784_498_400 `shouldBe` Just (fromGregorian 2026 7 20)

    it "moves to the next named session at 22:00 UTC" $ do
      fxSessionDay 1_784_584_799 `shouldBe` Just (fromGregorian 2026 7 20)
      fxSessionDay 1_784_584_800 `shouldBe` Just (fromGregorian 2026 7 21)

    it "rejects the closed weekend interval" $ do
      fxSessionDay 1_784_930_400 `shouldBe` Nothing

    it "counts only executed voluntary opens and closes inside the competition window" $ do
      activeSessionDays july2026Competition
        [ ("Open", 1_784_498_400)
        , ("Close", 1_784_498_500)
        , ("Liquidated", 1_784_584_800)
        , ("Deposit", 1_784_671_200)
        , ("Open", 1_784_671_200)
        , ("Open", 1_785_535_200)
        ]
        `shouldBe` [fromGregorian 2026 7 20, fromGregorian 2026 7 22]

    it "excludes executions at the exact score cutoff" $ do
      activeSessionDay july2026Competition "Close" 1_785_535_200 `shouldBe` Nothing

snapshot :: Bool -> Integer -> Integer -> Integer -> EquitySnapshot
snapshot = EquitySnapshot
