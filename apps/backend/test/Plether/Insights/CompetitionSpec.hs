module Plether.Insights.CompetitionSpec (spec) where

import Data.Time (UTCTime (..), addUTCTime, fromGregorian, secondsToDiffTime)
import Plether.Database.Insights
  ( CompetitionSeedMetadata (..)
  , CompetitionSeedMismatch (..)
  , competitionSeedMetadataFor
  , competitionSeedMismatches
  , isLegacyPaymentDeadlineOnlyMismatch
  , isLegacySeptemberPrizeAndXAccountAgeMismatch
  , isLegacySeptemberPrizeOnlyMismatch
  , isLegacySeptemberXAccountAgeOnlyMismatch
  )
import Plether.Insights.Competition
import Test.Hspec

spec :: Spec
spec = do
  describe "prizeAllocation" $ do
    let prizes = map (* usdcScale) [600, 500, 400, 300, 200]

    it "assigns an untied eligible participant its occupied prize" $ do
      prizeAllocation prizes (Just 2) (Just 1)
        `shouldBe` Just (PrizeAllocation 2 [2] $ 500 * usdcScale)

    it "splits the combined occupied prizes equally across an exact tie" $ do
      prizeAllocation prizes (Just 1) (Just 2)
        `shouldBe` Just (PrizeAllocation 1 [1, 2] $ 550 * usdcScale)

    it "shares the last paid place across a tie that extends beyond the awards" $ do
      prizeAllocation prizes (Just 5) (Just 2)
        `shouldBe` Just (PrizeAllocation 5 [5] $ 100 * usdcScale)

    it "does not create an award outside the paid places" $ do
      prizeAllocation prizes (Just 6) (Just 1) `shouldBe` Nothing
      prizeAllocation prizes Nothing Nothing `shouldBe` Nothing

  describe "finalizationBlockers" $ do
    let ready =
          FinalizationReadiness
            { frNowTimestamp = 200
            , frScoreCutoffTimestamp = 100
            , frResultsTimestamp = 150
            , frStartBlock = Just 100
            , frStartBlockHash = Just "0xstart"
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
          , frStartBlockHash = Nothing
          , frScoreCutoffBlock = Nothing
          , frMissingTraderReferences = 1
          , frUnresolvedReviews = 2
          , frFinalSnapshotCount = 1
          })
        `shouldBe`
          [ "the scoring cutoff has not passed"
          , "the scheduled results publication time has not arrived"
          , "the canonical start block hash has not been resolved"
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
      crStartAt july2026Competition
        `shouldBe` UTCTime (fromGregorian 2026 7 20) (secondsToDiffTime $ 16 * 60 * 60)
      crScoreCutoffAt july2026Competition
        `shouldBe` UTCTime (fromGregorian 2026 8 3) (secondsToDiffTime $ 16 * 60 * 60)
      crScoreCutoffAt july2026Competition
        `shouldBe` addUTCTime (14 * 24 * 60 * 60) (crStartAt july2026Competition)
      crStartingBalanceUsdc july2026Competition `shouldBe` 100_000_000_000
      minimumProfitUsdc july2026Competition `shouldBe` 1_000_000_000
      crMinimumActiveDays july2026Competition `shouldBe` 5
      fxSessionBoundaryUtcText july2026Competition `shouldBe` "22:00"
      crPrizeUsdc july2026Competition `shouldBe` [600_000_000, 300_000_000, 100_000_000]
      crPaymentDeadlineAt july2026Competition
        `shouldBe` UTCTime (fromGregorian 2026 8 10) (secondsToDiffTime $ 16 * 60 * 60)

  describe "September 2026 competition defaults" $ do
    it "uses the versioned September schedule with no close-only period" $ do
      crSlug september2026Competition `shouldBe` "testnet-trading-2026-09"
      crStartAt september2026Competition
        `shouldBe` UTCTime (fromGregorian 2026 9 13) (secondsToDiffTime $ 21 * 60 * 60)
      crNewRiskCutoffAt september2026Competition
        `shouldBe` UTCTime (fromGregorian 2026 9 25) (secondsToDiffTime $ 21 * 60 * 60)
      crScoreCutoffAt september2026Competition `shouldBe` crNewRiskCutoffAt september2026Competition
      crRegistrationClosesAt september2026Competition
        `shouldBe` Just (UTCTime (fromGregorian 2026 9 20) (secondsToDiffTime $ 21 * 60 * 60))
      crResultsAt september2026Competition
        `shouldBe` UTCTime (fromGregorian 2026 9 28) (secondsToDiffTime $ 12 * 60 * 60)
      crPaymentDeadlineAt september2026Competition
        `shouldBe` UTCTime (fromGregorian 2026 10 3) 0

    it "retains the bankroll and qualification thresholds with the expanded prize pool" $ do
      crStartingBalanceUsdc september2026Competition `shouldBe` 100_000_000_000
      minimumProfitUsdc september2026Competition `shouldBe` 1_000_000
      crMinimumActiveDays september2026Competition `shouldBe` 5
      crPrizeUsdc september2026Competition
        `shouldBe` [600_000_000, 500_000_000, 400_000_000, 300_000_000, 200_000_000]
      crScoringVersion september2026Competition `shouldBe` "cash-flow-adjusted-v1"
      crScoringVersion july2026Competition `shouldBe` "account-value-v1"
      fxSessionBoundaryUtcText september2026Competition `shouldBe` "21:00"
      crMinimumXAccountAgeDays september2026Competition `shouldBe` Just 30
      crTargetXHandle september2026Competition `shouldBe` Just "plether_fi"

    it "is selected only through its exact versioned slug" $ do
      competitionRulesForSlug september2026CompetitionSlug `shouldBe` Just september2026Competition
      competitionRulesForSlug "testnet-trading-2026-09-typo" `shouldBe` Nothing

    it "distinguishes registration-only state from the reviewed release binding" $ do
      competitionReleaseIsBound september2026Competition fixtureManifest `shouldBe` False
      competitionReleaseIsBound
        september2026Competition
        (fixtureManifest {crmReleaseId = september2026CompetitionSlug})
        `shouldBe` True
      pendingCompetitionReleaseManifestText september2026Competition 421_614
        `shouldBe` "release-pending-v1|testnet-trading-2026-09|421614"

    it "requires the registration opener to run strictly before close" $ do
      canInitiallySeedCompetitionAt
        september2026Competition
        (UTCTime (fromGregorian 2026 9 20) (secondsToDiffTime $ 20 * 60 * 60 + 59 * 60 + 59))
        `shouldBe` True
      canInitiallySeedCompetitionAt
        september2026Competition
        (UTCTime (fromGregorian 2026 9 20) (secondsToDiffTime $ 21 * 60 * 60))
        `shouldBe` False

    it "still permits an existing immutable competition row to restart after close" $ do
      let afterClose = UTCTime (fromGregorian 2026 9 21) 0
      canSeedCompetitionRowAt False september2026Competition afterClose `shouldBe` False
      canSeedCompetitionRowAt True september2026Competition afterClose `shouldBe` True

  describe "competition registration metadata states" $ do
    it "accepts historical, staged, and atomically opened metadata" $ do
      competitionRegistrationState Nothing Nothing Nothing Nothing
        `shouldBe` RegistrationUnconfigured
      competitionRegistrationState Nothing (Just 200) (Just 90) (Just "plether_fi")
        `shouldBe` RegistrationConfiguredUnopened
      competitionRegistrationState (Just 100) (Just 200) (Just 90) (Just "plether_fi")
        `shouldBe` RegistrationOpened 100 200

    it "rejects partial, blank, negative-age, and empty registration windows" $ do
      competitionRegistrationState Nothing (Just 200) Nothing (Just "plether_fi")
        `shouldBe` RegistrationMetadataInvalid
      competitionRegistrationState Nothing (Just 200) (Just 90) (Just " ")
        `shouldBe` RegistrationMetadataInvalid
      competitionRegistrationState Nothing (Just 200) (Just (-1)) (Just "plether_fi")
        `shouldBe` RegistrationMetadataInvalid
      competitionRegistrationState (Just 200) (Just 200) (Just 90) (Just "plether_fi")
        `shouldBe` RegistrationMetadataInvalid

  describe "immutable competition seed metadata" $ do
    let expected = competitionSeedMetadataFor
          july2026Competition
          421_614
          " 0xAa00000000000000000000000000000000000001 "
          "0xBb00000000000000000000000000000000000002"
          "0xCc00000000000000000000000000000000000003"
          "0xDd00000000000000000000000000000000000004"
          fixtureManifest

    it "normalizes deployment addresses before persistence and comparison" $ do
      csmReleaseRouter expected `shouldBe` "0xaa00000000000000000000000000000000000001"
      csmUsdcAddress expected `shouldBe` "0xbb00000000000000000000000000000000000002"
      csmMarginClearinghouseAddress expected `shouldBe` "0xcc00000000000000000000000000000000000003"
      csmAccountLensAddress expected `shouldBe` "0xdd00000000000000000000000000000000000004"

    it "identifies deployment and scoring changes instead of silently accepting them" $ do
      let stored = expected
            { csmChainId = 1
            , csmMinimumProfitBps = 200
            , csmAccountLensAddress = "0xee00000000000000000000000000000000000005"
            }
      competitionSeedMismatches expected stored
        `shouldBe`
          [ CompetitionSeedMismatch "chain_id" "1" "421614"
          , CompetitionSeedMismatch "account_lens_address" "0xee00000000000000000000000000000000000005" "0xdd00000000000000000000000000000000000004"
          , CompetitionSeedMismatch "minimum_profit_bps" "200" "100"
          ]

    it "treats every paid place as immutable competition metadata" $ do
      let septemberExpected = competitionSeedMetadataFor
            september2026Competition
            421_614
            "0xAa00000000000000000000000000000000000001"
            "0xBb00000000000000000000000000000000000002"
            "0xCc00000000000000000000000000000000000003"
            "0xDd00000000000000000000000000000000000004"
            fixtureManifest
          stored = septemberExpected {csmFifthPrizeUsdc = 100_000_000}
      competitionSeedMismatches septemberExpected stored
        `shouldBe` [CompetitionSeedMismatch "fifth_prize_usdc" "100000000" "200000000"]

    it "recognizes only the exact pre-launch payout-deadline correction" $ do
      let legacy = expected {csmPaymentDeadlineTimestamp = 1_786_319_999}
      isLegacyPaymentDeadlineOnlyMismatch expected legacy `shouldBe` True
      isLegacyPaymentDeadlineOnlyMismatch
        expected
        legacy {csmRulesVersion = "different"}
        `shouldBe` False

    it "never applies the July payout migration to a newer competition" $ do
      let septemberExpected = competitionSeedMetadataFor
            september2026Competition
            421_614
            "0xAa00000000000000000000000000000000000001"
            "0xBb00000000000000000000000000000000000002"
            "0xCc00000000000000000000000000000000000003"
            "0xDd00000000000000000000000000000000000004"
            fixtureManifest
          deceptiveLegacy = septemberExpected {csmPaymentDeadlineTimestamp = 1_786_319_999}
      isLegacyPaymentDeadlineOnlyMismatch septemberExpected deceptiveLegacy `shouldBe` False

    it "recognizes only the exact pre-launch September prize expansion" $ do
      let septemberExpected = competitionSeedMetadataFor
            september2026Competition
            421_614
            "0xAa00000000000000000000000000000000000001"
            "0xBb00000000000000000000000000000000000002"
            "0xCc00000000000000000000000000000000000003"
            "0xDd00000000000000000000000000000000000004"
            fixtureManifest
          legacyPrizes = septemberExpected
            { csmSecondPrizeUsdc = 300_000_000
            , csmThirdPrizeUsdc = 100_000_000
            , csmFourthPrizeUsdc = 0
            , csmFifthPrizeUsdc = 0
            }
      isLegacySeptemberPrizeOnlyMismatch septemberExpected legacyPrizes
        `shouldBe` True
      isLegacySeptemberPrizeOnlyMismatch
        septemberExpected
        legacyPrizes {csmMinimumProfitBps = 200}
        `shouldBe` False
      isLegacySeptemberPrizeOnlyMismatch expected legacyPrizes
        `shouldBe` False

    it "recognizes only the exact pre-launch September 90-to-30 age relaxation" $ do
      let septemberExpected = competitionSeedMetadataFor
            september2026Competition
            421_614
            "0xAa00000000000000000000000000000000000001"
            "0xBb00000000000000000000000000000000000002"
            "0xCc00000000000000000000000000000000000003"
            "0xDd00000000000000000000000000000000000004"
            fixtureManifest
          legacyAge = septemberExpected {csmMinimumXAccountAgeDays = Just 90}
      isLegacySeptemberXAccountAgeOnlyMismatch septemberExpected legacyAge
        `shouldBe` True
      isLegacySeptemberXAccountAgeOnlyMismatch
        septemberExpected
        legacyAge {csmMinimumProfitBps = 200}
        `shouldBe` False
      isLegacySeptemberXAccountAgeOnlyMismatch
        (septemberExpected {csmMinimumXAccountAgeDays = Just 90})
        septemberExpected
        `shouldBe` False
      isLegacySeptemberXAccountAgeOnlyMismatch
        (septemberExpected {csmMinimumXAccountAgeDays = Just 60})
        legacyAge
        `shouldBe` False
      isLegacySeptemberXAccountAgeOnlyMismatch expected legacyAge
        `shouldBe` False

    it "recognizes the exact combined pre-launch September corrections" $ do
      let septemberExpected = competitionSeedMetadataFor
            september2026Competition
            421_614
            "0xAa00000000000000000000000000000000000001"
            "0xBb00000000000000000000000000000000000002"
            "0xCc00000000000000000000000000000000000003"
            "0xDd00000000000000000000000000000000000004"
            fixtureManifest
          legacy = septemberExpected
            { csmMinimumXAccountAgeDays = Just 90
            , csmSecondPrizeUsdc = 300_000_000
            , csmThirdPrizeUsdc = 100_000_000
            , csmFourthPrizeUsdc = 0
            , csmFifthPrizeUsdc = 0
            }
      isLegacySeptemberPrizeAndXAccountAgeMismatch septemberExpected legacy
        `shouldBe` True
      isLegacySeptemberPrizeAndXAccountAgeMismatch
        septemberExpected
        legacy {csmMinimumProfitBps = 200}
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

    it "scores a prefunded bankroll and one official post-baseline allocation identically" $ do
      let prefunded = calculateScore $
            ScoreInput
              { siStartingSnapshot = snapshot False 0 100_000_000_000 0
              , siCurrentSnapshot = snapshot False 0 105_000_000_000 0
              , siDepositsSinceStartUsdc = 0
              , siWithdrawalsSinceStartUsdc = 0
              , siManualAdjustmentsUsdc = 0
              }
          allocatedAfterBaseline = calculateScore $
            ScoreInput
              { siStartingSnapshot = snapshot False 0 0 0
              , siCurrentSnapshot = snapshot False 0 105_000_000_000 0
              , siDepositsSinceStartUsdc = 100_000_000_000
              , siWithdrawalsSinceStartUsdc = 0
              , siManualAdjustmentsUsdc = 0
              }
      sbFinalPnlUsdc prefunded `shouldBe` 5_000_000_000
      sbFinalPnlUsdc allocatedAfterBaseline `shouldBe` sbFinalPnlUsdc prefunded

    it "scores a legitimate complete zero terminal state instead of freezing an older snapshot" $ do
      let score = calculateScore $
            ScoreInput
              { siStartingSnapshot = snapshot False 0 100_000_000_000 0
              , siCurrentSnapshot = snapshot False 0 0 0
              , siDepositsSinceStartUsdc = 0
              , siWithdrawalsSinceStartUsdc = 0
              , siManualAdjustmentsUsdc = 0
              }
      sbCurrentAccountValueUsdc score `shouldBe` 0
      sbFinalPnlUsdc score `shouldBe` (-100_000_000_000)

  describe "fundingIntegrityFlags" $ do
    it "accepts the two permitted 100,000 bankroll paths" $ do
      fundingIntegrityFlags (FundingIntegrityInput (Just 100_000_000_000) False 0 100_000_000_000 [100_000_000_000] 100_000_000_000 [] 0 100_000_000_000)
        `shouldBe` []
      fundingIntegrityFlags (FundingIntegrityInput (Just 0) False 0 100_000_000_000 [] 0 [(100_000_000_000, True)] 0 100_000_000_000)
        `shouldBe` []

    it "flags excess, late, and unverifiable funding for private review" $ do
      fundingIntegrityFlags
        (FundingIntegrityInput (Just 0) False 0 100_000_000_000 [] 0 [(50_000_000_000, False)] 1 50_000_000_000)
        `shouldBe`
          [ "official_allocation_amount_invalid"
          , "official_allocation_not_before_trading"
          , "unverified_deposit_provenance"
          ]

    it "flags positions and pending orders already present at the canonical baseline" $ do
      fundingIntegrityFlags
        (FundingIntegrityInput (Just 100_000_000_000) True 2 100_000_000_000 [100_000_000_000] 100_000_000_000 [] 0 100_000_000_000)
        `shouldBe` ["baseline_open_position", "baseline_pending_orders"]

    it "rejects an unproven prefunded bankroll, prebaseline round trips, and excess capacity" $ do
      fundingIntegrityFlags
        (FundingIntegrityInput (Just 100_000_000_000) False 0 100_000_000_000 [] 100_000_000_000 [] 0 100_000_000_000)
        `shouldBe` ["baseline_official_allocation_count_invalid"]
      fundingIntegrityFlags
        (FundingIntegrityInput (Just 0) False 0 100_000_000_000 [100_000_000_000] 0 [(100_000_000_000, True)] 0 200_000_000_000)
        `shouldBe`
          [ "unexpected_prebaseline_official_allocation"
          , "funding_capacity_exceeded"
          ]

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

    it "uses the exact +1.00 USDC threshold for September" $ do
      qMeetsProfitRequirement (qualification september2026Competition EligibilityEligible 1_000_000 5)
        `shouldBe` True
      qMeetsProfitRequirement (qualification september2026Competition EligibilityEligible 999_999 5)
        `shouldBe` False

  describe "FX active sessions" $ do
    it "maps the Sunday 22:00 UTC reopen to Monday" $ do
      fxSessionDay july2026Competition 1_784_498_400 `shouldBe` Just (fromGregorian 2026 7 20)

    it "moves to the next named session at 22:00 UTC" $ do
      fxSessionDay july2026Competition 1_784_584_799 `shouldBe` Just (fromGregorian 2026 7 20)
      fxSessionDay july2026Competition 1_784_584_800 `shouldBe` Just (fromGregorian 2026 7 21)

    it "rejects the closed weekend interval" $ do
      fxSessionDay july2026Competition 1_784_930_400 `shouldBe` Nothing

    it "counts only executed voluntary opens and closes inside the competition window" $ do
      activeSessionDays july2026Competition
        [ ("Open", 1_784_563_199)
        , ("Open", 1_784_563_200)
        , ("Close", 1_784_563_300)
        , ("Liquidated", 1_784_584_800)
        , ("Deposit", 1_784_671_200)
        , ("Open", 1_784_671_200)
        , ("Open", 1_785_535_200)
        ]
        `shouldBe` [fromGregorian 2026 7 20, fromGregorian 2026 7 22]

    it "excludes executions at the exact score cutoff" $ do
      activeSessionDay july2026Competition "Close" 1_785_535_200 `shouldBe` Nothing

    it "uses the September 21:00 UTC boundary" $ do
      fxSessionDay september2026Competition 1_789_419_599
        `shouldBe` Just (fromGregorian 2026 9 14)
      fxSessionDay september2026Competition 1_789_419_600
        `shouldBe` Just (fromGregorian 2026 9 15)

    it "offers exactly the ten September weekday sessions inside the window" $ do
      activeSessionDays september2026Competition
        [ ("Open", 1_789_387_200)
        , ("Open", 1_789_473_600)
        , ("Open", 1_789_560_000)
        , ("Open", 1_789_646_400)
        , ("Open", 1_789_732_800)
        , ("Open", 1_789_992_000)
        , ("Open", 1_790_078_400)
        , ("Open", 1_790_164_800)
        , ("Open", 1_790_251_200)
        , ("Open", 1_790_337_600)
        ]
        `shouldBe` map (fromGregorian 2026 9) [14, 15, 16, 17, 18, 21, 22, 23, 24, 25]

    it "treats the September scoring interval as half-open" $ do
      activeSessionDay september2026Competition "Open" 1_789_333_199 `shouldBe` Nothing
      activeSessionDay september2026Competition "Open" 1_789_333_200
        `shouldBe` Just (fromGregorian 2026 9 14)
      activeSessionDay september2026Competition "Close" 1_790_369_999
        `shouldBe` Just (fromGregorian 2026 9 25)
      activeSessionDay september2026Competition "Close" 1_790_370_000 `shouldBe` Nothing

snapshot :: Bool -> Integer -> Integer -> Integer -> EquitySnapshot
snapshot = EquitySnapshot

fixtureManifest :: CompetitionReleaseManifest
fixtureManifest =
  CompetitionReleaseManifest
    { crmReleaseId = "fixture-release"
    , crmChainId = 421_614
    , crmUsdc = "0xa200000000000000000000000000000000000002"
    , crmOrderRouter = "0xa100000000000000000000000000000000000001"
    , crmMarginClearinghouse = "0xa300000000000000000000000000000000000003"
    , crmAccountLens = "0xa400000000000000000000000000000000000004"
    , crmCfdEngine = "0xd100000000000000000000000000000000000001"
    , crmCfdEngineLens = "0xd200000000000000000000000000000000000002"
    , crmSettlementSidecar = "0xd300000000000000000000000000000000000003"
    , crmPletherOracle = "0xd400000000000000000000000000000000000004"
    , crmIndexerStartBlock = 1
    }
