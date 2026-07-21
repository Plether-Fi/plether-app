module Plether.Insights.Competition
  ( CompetitionRules (..)
  , CompetitionPhase (..)
  , ParticipantEligibility (..)
  , EquitySnapshot (..)
  , ScoreInput (..)
  , ScoreBreakdown (..)
  , Qualification (..)
  , PrizeAllocation (..)
  , FinalizationReadiness (..)
  , july2026Competition
  , july2026CompetitionSlug
  , usdcScale
  , economicAccountValue
  , calculateScore
  , minimumProfitUsdc
  , qualification
  , prizeAllocation
  , finalizationBlockers
  , participantEligibilityText
  , participantEligibilityFromText
  , competitionPhaseAt
  , competitionPhaseText
  , fxSessionDay
  , activeSessionDay
  , activeSessionDays
  , isVoluntaryPositionChange
  ) where

import Data.List (nub, sort)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
  ( Day
  , UTCTime (..)
  , addUTCTime
  , dayOfWeek
  , fromGregorian
  , secondsToDiffTime
  , utctDay
  , DayOfWeek (..)
  )
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

-- | All monetary values in this module are integer USDC base units (six
-- decimals). Keeping the scoring core integral makes the +1% boundary and ties
-- exact and reproducible.
usdcScale :: Integer
usdcScale = 1_000_000

july2026CompetitionSlug :: Text
july2026CompetitionSlug = "testnet-trading-2026"

data CompetitionRules = CompetitionRules
  { crSlug :: Text
  , crName :: Text
  , crStartAt :: UTCTime
  , crNewRiskCutoffAt :: UTCTime
  , crScoreCutoffAt :: UTCTime
  , crResultsAt :: UTCTime
  , crPaymentDeadlineAt :: UTCTime
  , crStartingBalanceUsdc :: Integer
  , crMinimumProfitBps :: Integer
  , crMinimumActiveDays :: Int
  , crPrizeUsdc :: [Integer]
  , crScoringVersion :: Text
  , crRulesVersion :: Text
  }
  deriving stock (Show, Eq)

-- | The July 2026 competition follows FX sessions. Scoring begins Monday July
-- 20 at 16:00 UTC (18:00 in Warsaw) and runs for exactly 14 days. The final
-- cutoff is Monday August 3 at 16:00 UTC (18:00 in Warsaw).
july2026Competition :: CompetitionRules
july2026Competition =
  CompetitionRules
    { crSlug = july2026CompetitionSlug
    , crName = "Plether Testnet Trading Competition"
    , crStartAt = utc 2026 7 20 16 0 0
    , crNewRiskCutoffAt = utc 2026 8 3 13 0 0
    , crScoreCutoffAt = utc 2026 8 3 16 0 0
    , crResultsAt = utc 2026 8 5 12 0 0
    , crPaymentDeadlineAt = utc 2026 8 10 16 0 0
    , crStartingBalanceUsdc = 100_000 * usdcScale
    , crMinimumProfitBps = 100
    , crMinimumActiveDays = 5
    , crPrizeUsdc = map (* usdcScale) [600, 300, 100]
    , crScoringVersion = "account-value-v1"
    , crRulesVersion = "2026-07-20"
    }

data CompetitionPhase
  = CompetitionUpcoming
  | CompetitionLive
  | CompetitionReview
  | CompetitionFinal
  deriving stock (Show, Eq, Ord)

competitionPhaseAt :: CompetitionRules -> UTCTime -> CompetitionPhase
competitionPhaseAt rules now
  | now < crStartAt rules = CompetitionUpcoming
  | now < crScoreCutoffAt rules = CompetitionLive
  | now < crResultsAt rules = CompetitionReview
  | otherwise = CompetitionFinal

competitionPhaseText :: CompetitionPhase -> Text
competitionPhaseText = \case
  CompetitionUpcoming -> "upcoming"
  CompetitionLive -> "live"
  CompetitionReview -> "review"
  CompetitionFinal -> "final"

-- | Review state is deliberately separate from mechanical qualification. A
-- participant can meet the P&L/day thresholds while still awaiting the
-- one-wallet and anti-manipulation review.
data ParticipantEligibility
  = EligibilityPending
  | EligibilityEligible
  | EligibilityUnderReview
  | EligibilityIneligible
  deriving stock (Show, Eq, Ord)

-- | A reviewed, mechanically qualified participant's share of the public
-- prize table. Exact P&L ties occupy consecutive prize places and split the
-- combined awards for those places. Any indivisible single-micro-USDC dust is
-- deliberately left undistributed so every tied participant receives exactly
-- the same integer amount.
data PrizeAllocation = PrizeAllocation
  { paPlace :: Integer
  , paPlaces :: [Integer]
  , paAmountUsdc :: Integer
  }
  deriving stock (Show, Eq)

prizeAllocation
  :: [Integer]
  -> Maybe Integer
  -> Maybe Integer
  -> Maybe PrizeAllocation
prizeAllocation prizes maybePlace maybeTieCount = do
  place <- maybePlace
  tieCount <- maybeTieCount
  let lastPrizePlace = toInteger $ length prizes
  if place < 1 || place > lastPrizePlace || tieCount < 1
    then Nothing
    else
      let occupiedPlaces = [place .. min lastPrizePlace (place + tieCount - 1)]
          occupiedPrizes =
            [ prizes !! fromInteger (occupiedPlace - 1)
            | occupiedPlace <- occupiedPlaces
            ]
       in Just
            PrizeAllocation
              { paPlace = place
              , paPlaces = occupiedPlaces
              , paAmountUsdc = sum occupiedPrizes `div` tieCount
              }

-- | Closed-world facts checked immediately before the one-way transition to
-- final standings. Keeping the policy pure makes both the CLI error and tests
-- describe the same fail-closed gate.
data FinalizationReadiness = FinalizationReadiness
  { frNowTimestamp :: Integer
  , frScoreCutoffTimestamp :: Integer
  , frResultsTimestamp :: Integer
  , frStartBlock :: Maybe Integer
  , frScoreCutoffBlock :: Maybe Integer
  , frParticipantCount :: Integer
  , frMissingTraderReferences :: Integer
  , frUnresolvedReviews :: Integer
  , frStartSnapshotCount :: Integer
  , frFinalSnapshotCount :: Integer
  , frFinalSnapshotHashCount :: Integer
  }
  deriving stock (Show, Eq)

finalizationBlockers :: FinalizationReadiness -> [Text]
finalizationBlockers FinalizationReadiness {..} =
  concat
    [ ["the scoring cutoff has not passed" | frNowTimestamp < frScoreCutoffTimestamp]
    , ["the scheduled results publication time has not arrived" | frNowTimestamp < frResultsTimestamp]
    , ["the canonical start block has not been resolved" | frStartBlock == Nothing]
    , ["the canonical final block has not been resolved" | frScoreCutoffBlock == Nothing]
    , ["no participants are registered" | frParticipantCount < 1]
    , countBlocker frMissingTraderReferences "participant registration(s) are missing a private trader reference"
    , countBlocker frUnresolvedReviews "participant review(s) are still pending or under review"
    , [ "baseline snapshots are incomplete: "
          <> tshow frStartSnapshotCount
          <> " of "
          <> tshow frParticipantCount
          <> " registered wallets are snapshotted at the baseline block"
      | frStartSnapshotCount /= frParticipantCount
      ]
    , [ "final snapshots are incomplete: "
          <> tshow frFinalSnapshotCount
          <> " of "
          <> tshow frParticipantCount
          <> " registered wallets are snapshotted at the final block"
      | frFinalSnapshotCount /= frParticipantCount
      ]
    , [ "the final snapshots do not share one canonical block hash"
      | frFinalSnapshotCount == frParticipantCount
          && frParticipantCount > 0
          && frFinalSnapshotHashCount /= 1
      ]
    ]
  where
    countBlocker count label =
      [tshow count <> " " <> label | count > 0]
    tshow = T.pack . show

participantEligibilityText :: ParticipantEligibility -> Text
participantEligibilityText = \case
  EligibilityPending -> "pending"
  EligibilityEligible -> "eligible"
  EligibilityUnderReview -> "under_review"
  EligibilityIneligible -> "ineligible"

participantEligibilityFromText :: Text -> Maybe ParticipantEligibility
participantEligibilityFromText value =
  case T.toLower $ T.strip value of
    "pending" -> Just EligibilityPending
    "pending_review" -> Just EligibilityPending
    "eligible" -> Just EligibilityEligible
    "under_review" -> Just EligibilityUnderReview
    "ineligible" -> Just EligibilityIneligible
    "disqualified" -> Just EligibilityIneligible
    _ -> Nothing

-- | Raw account-ledger outputs captured at one finalized block. A flat account
-- uses terminalReachable; an account with exposure uses signed netEquity.
-- Trader claims are added in both cases because they remain economically owned
-- by the trader but sit outside those ledger values.
data EquitySnapshot = EquitySnapshot
  { esHasOpenPosition :: Bool
  , esSignedNetEquityUsdc :: Integer
  , esTerminalReachableUsdc :: Integer
  , esTraderClaimsUsdc :: Integer
  }
  deriving stock (Show, Eq)

economicAccountValue :: EquitySnapshot -> Integer
economicAccountValue EquitySnapshot {..} =
  max 0 $ ledgerValue + esTraderClaimsUsdc
  where
    ledgerValue
      | esHasOpenPosition = esSignedNetEquityUsdc
      | otherwise = esTerminalReachableUsdc

data ScoreInput = ScoreInput
  { siStartingSnapshot :: EquitySnapshot
  , siCurrentSnapshot :: EquitySnapshot
  , siDepositsSinceStartUsdc :: Integer
  , siWithdrawalsSinceStartUsdc :: Integer
  , siManualAdjustmentsUsdc :: Integer
  }
  deriving stock (Show, Eq)

data ScoreBreakdown = ScoreBreakdown
  { sbStartingAccountValueUsdc :: Integer
  , sbCurrentAccountValueUsdc :: Integer
  , sbDepositsSinceStartUsdc :: Integer
  , sbWithdrawalsSinceStartUsdc :: Integer
  , sbManualAdjustmentsUsdc :: Integer
  , sbFinalPnlUsdc :: Integer
  }
  deriving stock (Show, Eq)

calculateScore :: ScoreInput -> ScoreBreakdown
calculateScore ScoreInput {..} =
  ScoreBreakdown
    { sbStartingAccountValueUsdc = startingValue
    , sbCurrentAccountValueUsdc = currentValue
    , sbDepositsSinceStartUsdc = siDepositsSinceStartUsdc
    , sbWithdrawalsSinceStartUsdc = siWithdrawalsSinceStartUsdc
    , sbManualAdjustmentsUsdc = siManualAdjustmentsUsdc
    , sbFinalPnlUsdc =
        currentValue
          - startingValue
          - siDepositsSinceStartUsdc
          + siWithdrawalsSinceStartUsdc
          + siManualAdjustmentsUsdc
    }
  where
    startingValue = economicAccountValue siStartingSnapshot
    currentValue = economicAccountValue siCurrentSnapshot

minimumProfitUsdc :: CompetitionRules -> Integer
minimumProfitUsdc rules =
  crStartingBalanceUsdc rules * crMinimumProfitBps rules `div` 10_000

data Qualification = Qualification
  { qMeetsProfitRequirement :: Bool
  , qMeetsActiveDaysRequirement :: Bool
  , qMechanicallyQualified :: Bool
  , qPrizeEligible :: Bool
  }
  deriving stock (Show, Eq)

qualification
  :: CompetitionRules
  -> ParticipantEligibility
  -> Integer
  -> Int
  -> Qualification
qualification rules reviewStatus pnlUsdc activeDayCount =
  Qualification
    { qMeetsProfitRequirement = profitOk
    , qMeetsActiveDaysRequirement = daysOk
    , qMechanicallyQualified = profitOk && daysOk
    , qPrizeEligible =
        profitOk
          && daysOk
          && reviewStatus == EligibilityEligible
    }
  where
    profitOk = pnlUsdc >= minimumProfitUsdc rules
    daysOk = activeDayCount >= crMinimumActiveDays rules

-- | Convert an execution timestamp to its named FX session. Adding two hours
-- maps the Sunday 22:00 UTC reopen to Monday and the Friday 22:00 UTC close to
-- Saturday. Weekend dates are rejected.
fxSessionDay :: Integer -> Maybe Day
fxSessionDay epochSeconds =
  let sessionDay = utctDay $ addUTCTime (2 * 60 * 60) $ posixSecondsToUTCTime $ fromInteger epochSeconds
   in case dayOfWeek sessionDay of
        Monday -> Just sessionDay
        Tuesday -> Just sessionDay
        Wednesday -> Just sessionDay
        Thursday -> Just sessionDay
        Friday -> Just sessionDay
        Saturday -> Nothing
        Sunday -> Nothing

isVoluntaryPositionChange :: Text -> Bool
isVoluntaryPositionChange activityType =
  activityType == "Open" || activityType == "Close"

activeSessionDay :: CompetitionRules -> Text -> Integer -> Maybe Day
activeSessionDay rules activityType timestamp
  | not (isVoluntaryPositionChange activityType) = Nothing
  | activityTime < crStartAt rules = Nothing
  | activityTime >= crScoreCutoffAt rules = Nothing
  | otherwise = fxSessionDay timestamp
  where
    activityTime = posixSecondsToUTCTime $ fromInteger timestamp

activeSessionDays :: CompetitionRules -> [(Text, Integer)] -> [Day]
activeSessionDays rules =
  sort . nub . mapMaybe (uncurry $ activeSessionDay rules)

utc :: Integer -> Int -> Int -> Integer -> Integer -> Integer -> UTCTime
utc year month day hour minute second =
  UTCTime
    (fromGregorian year month day)
    (secondsToDiffTime $ hour * 60 * 60 + minute * 60 + second)
