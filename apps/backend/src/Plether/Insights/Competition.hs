module Plether.Insights.Competition
  ( CompetitionRules (..)
  , CompetitionReleaseManifest (..)
  , competitionReleaseManifestText
  , competitionReleaseIsBound
  , pendingCompetitionReleaseManifestText
  , CompetitionPhase (..)
  , CompetitionRegistrationState (..)
  , ParticipantEligibility (..)
  , EquitySnapshot (..)
  , ScoreInput (..)
  , ScoreBreakdown (..)
  , Qualification (..)
  , PrizeAllocation (..)
  , FinalizationReadiness (..)
  , FundingIntegrityInput (..)
  , july2026Competition
  , july2026CompetitionSlug
  , september2026Competition
  , september2026CompetitionSlug
  , defaultCompetitionSlug
  , competitionRules
  , competitionRulesForSlug
  , usdcScale
  , economicAccountValue
  , calculateScore
  , minimumProfitUsdc
  , qualification
  , prizeAllocation
  , finalizationBlockers
  , fundingIntegrityFlags
  , participantEligibilityText
  , participantEligibilityFromText
  , competitionPhaseAt
  , competitionPhaseText
  , canInitiallySeedCompetitionAt
  , canSeedCompetitionRowAt
  , competitionRegistrationState
  , fxSessionBoundaryUtcText
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
-- decimals). Keeping the scoring core integral makes profit boundaries and ties
-- exact and reproducible.
usdcScale :: Integer
usdcScale = 1_000_000

july2026CompetitionSlug :: Text
july2026CompetitionSlug = "testnet-trading-2026"

september2026CompetitionSlug :: Text
september2026CompetitionSlug = "testnet-trading-2026-09"

-- | Keep the historical competition as the local-development default. A new
-- competition is activated only when its versioned slug is configured
-- explicitly, so upgrading the binary cannot silently seed a new event.
defaultCompetitionSlug :: Text
defaultCompetitionSlug = july2026CompetitionSlug

data CompetitionRules = CompetitionRules
  { crSlug :: Text
  , crName :: Text
  , crStartAt :: UTCTime
  , crNewRiskCutoffAt :: UTCTime
  , crScoreCutoffAt :: UTCTime
  , crResultsAt :: UTCTime
  , crPaymentDeadlineAt :: UTCTime
  , crStartingBalanceUsdc :: Integer
  -- Canonical prize-qualification threshold. This stays in the code-defined
  -- rule set because sub-basis-point amounts cannot be represented exactly by
  -- the legacy persisted percentage metadata below.
  , crMinimumProfitUsdc :: Integer
  , crLegacyMinimumProfitBps :: Integer
  , crMinimumActiveDays :: Int
  , crFxSessionBoundaryUtcMinutes :: Int
  , crRegistrationClosesAt :: Maybe UTCTime
  , crMinimumXAccountAgeDays :: Maybe Int
  , crTargetXHandle :: Maybe Text
  , crPrizeUsdc :: [Integer]
  , crScoringVersion :: Text
  , crRulesVersion :: Text
  }
  deriving stock (Show, Eq)

-- | The deployment inputs that affect canonical account snapshots and indexed
-- activity but are not already stored as first-class competition columns.
-- Their canonical rendering is persisted with the immutable rule row and is
-- also checked by the history indexer before it writes.
data CompetitionReleaseManifest = CompetitionReleaseManifest
  { crmReleaseId :: Text
  , crmChainId :: Integer
  , crmUsdc :: Text
  , crmOrderRouter :: Text
  , crmMarginClearinghouse :: Text
  , crmAccountLens :: Text
  , crmCfdEngine :: Text
  , crmCfdEngineLens :: Text
  , crmSettlementSidecar :: Text
  , crmPletherOracle :: Text
  , crmIndexerStartBlock :: Integer
  }
  deriving stock (Show, Eq)

competitionReleaseManifestText :: CompetitionReleaseManifest -> Text
competitionReleaseManifestText CompetitionReleaseManifest {..} =
  T.intercalate
    "|"
    [ "release-manifest-v2"
    , T.strip crmReleaseId
    , T.pack $ show crmChainId
    , normalize crmUsdc
    , normalize crmOrderRouter
    , normalize crmMarginClearinghouse
    , normalize crmAccountLens
    , normalize crmCfdEngine
    , normalize crmCfdEngineLens
    , normalize crmSettlementSidecar
    , normalize crmPletherOracle
    , T.pack $ show crmIndexerStartBlock
    ]
  where
    normalize = T.toLower . T.strip

-- | A competition may accept registrations before its on-chain release has
-- been deployed.  The release becomes immutable only when its reviewed
-- manifest uses the competition slug as its release identifier.
competitionReleaseIsBound :: CompetitionRules -> CompetitionReleaseManifest -> Bool
competitionReleaseIsBound rules manifest =
  T.strip (crmReleaseId manifest) == crSlug rules

-- | Stable sentinel persisted while registration is open but the competition
-- contracts have not yet been bound.  It is deliberately not a valid release
-- manifest and must never be consumed by indexers or snapshot workers.
pendingCompetitionReleaseManifestText :: CompetitionRules -> Integer -> Text
pendingCompetitionReleaseManifestText rules chainId =
  T.intercalate
    "|"
    [ "release-pending-v1"
    , crSlug rules
    , T.pack $ show chainId
    ]

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
    , crMinimumProfitUsdc = 1_000 * usdcScale
    , crLegacyMinimumProfitBps = 100
    , crMinimumActiveDays = 5
    , crFxSessionBoundaryUtcMinutes = 22 * 60
    , crRegistrationClosesAt = Nothing
    , crMinimumXAccountAgeDays = Nothing
    , crTargetXHandle = Nothing
    , crPrizeUsdc = map (* usdcScale) [600, 300, 100]
    , crScoringVersion = "account-value-v1"
    , crRulesVersion = "2026-07-20"
    }

-- | The September event opens at the Sunday 21:00 UTC FX-session boundary and
-- ends at the Friday 21:00 boundary. There is deliberately no close-only
-- interval: the new-risk and scoring cutoffs are identical.
september2026Competition :: CompetitionRules
september2026Competition =
  CompetitionRules
    { crSlug = september2026CompetitionSlug
    , crName = "Plether September 2026 Testnet Trading Competition"
    , crStartAt = utc 2026 9 13 21 0 0
    , crNewRiskCutoffAt = utc 2026 9 25 21 0 0
    , crScoreCutoffAt = utc 2026 9 25 21 0 0
    , crResultsAt = utc 2026 9 28 12 0 0
    , crPaymentDeadlineAt = utc 2026 10 3 0 0 0
    , crStartingBalanceUsdc = 100_000 * usdcScale
    , crMinimumProfitUsdc = 1 * usdcScale
    , crLegacyMinimumProfitBps = 100
    , crMinimumActiveDays = 5
    , crFxSessionBoundaryUtcMinutes = 21 * 60
    , crRegistrationClosesAt = Just $ utc 2026 9 20 21 0 0
    , crMinimumXAccountAgeDays = Just 30
    , crTargetXHandle = Just "plether_fi"
    , crPrizeUsdc = map (* usdcScale) [600, 500, 400, 300, 200]
    , crScoringVersion = "cash-flow-adjusted-v1"
    , crRulesVersion = "2026-09-13"
    }

competitionRules :: [CompetitionRules]
competitionRules = [july2026Competition, september2026Competition]

competitionRulesForSlug :: Text -> Maybe CompetitionRules
competitionRulesForSlug requestedSlug =
  case filter ((== T.strip requestedSlug) . crSlug) competitionRules of
    [rules] -> Just rules
    _ -> Nothing

data CompetitionPhase
  = CompetitionUpcoming
  | CompetitionLive
  | CompetitionReview
  | CompetitionFinal
  deriving stock (Show, Eq, Ord)

data CompetitionRegistrationState
  = RegistrationUnconfigured
  | RegistrationConfiguredUnopened
  | RegistrationOpened Integer Integer
  | RegistrationMetadataInvalid
  deriving stock (Show, Eq, Ord)

-- | Mirror the database constraint in a pure form for API fail-closed
-- handling and regression tests. The configured-unopened state permits schema
-- and worker startup before the enabled registration API atomically records
-- the real opening time.
competitionRegistrationState
  :: Maybe Integer
  -> Maybe Integer
  -> Maybe Int
  -> Maybe Text
  -> CompetitionRegistrationState
competitionRegistrationState openTimestamp closeTimestamp minimumAge targetHandle =
  case (openTimestamp, closeTimestamp, minimumAge, normalizedHandle) of
    (Nothing, Nothing, Nothing, Nothing) -> RegistrationUnconfigured
    (Nothing, Just _, Just age, Just _)
      | age >= 0 -> RegistrationConfiguredUnopened
    (Just opened, Just closed, Just age, Just _)
      | age >= 0 && opened < closed -> RegistrationOpened opened closed
    _ -> RegistrationMetadataInvalid
  where
    normalizedHandle = case T.strip <$> targetHandle of
      Just value | not (T.null value) -> Just value
      _ -> Nothing

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

-- | A registration-enabled competition may be restarted after close once its
-- immutable row exists, but it must never be created with an empty registration
-- window. Database seeding applies this predicate only to the first insert.
canInitiallySeedCompetitionAt :: CompetitionRules -> UTCTime -> Bool
canInitiallySeedCompetitionAt rules now =
  maybe True (now <) $ crRegistrationClosesAt rules

canSeedCompetitionRowAt :: Bool -> CompetitionRules -> UTCTime -> Bool
canSeedCompetitionRowAt rowAlreadyExists rules now =
  rowAlreadyExists || canInitiallySeedCompetitionAt rules now

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
  , frStartBlockHash :: Maybe Text
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
    , ["the canonical start block hash has not been resolved" | frStartBlockHash == Nothing]
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

data FundingIntegrityInput = FundingIntegrityInput
  { fiiBaselineValueUsdc :: Maybe Integer
  , fiiBaselineHasOpenPosition :: Bool
  , fiiBaselinePendingOrderCount :: Int
  , fiiRequiredBankrollUsdc :: Integer
  , fiiBaselineOfficialAllocations :: [Integer]
  , fiiBaselineFundingNetUsdc :: Integer
  , fiiOfficialAllocations :: [(Integer, Bool)]
  , fiiUnverifiedDepositCount :: Int
  , fiiMaximumFundingNetUsdc :: Integer
  }
  deriving stock (Show, Eq)

-- | Funding is eligible either when the baseline already contains the exact
-- bankroll and no later allocation occurs, or when a zero baseline receives
-- exactly one official allocation before the participant's first trade.
-- These flags are private review signals; public scoring still neutralizes all
-- verified cash flows independently.
fundingIntegrityFlags :: FundingIntegrityInput -> [Text]
fundingIntegrityFlags FundingIntegrityInput {..} =
  concat
    [ ["baseline_unavailable" | fiiBaselineValueUsdc == Nothing]
    , ["baseline_open_position" | fiiBaselineHasOpenPosition]
    , ["baseline_pending_orders" | fiiBaselinePendingOrderCount > 0]
    , [ "invalid_starting_bankroll"
      | maybe False (`notElem` [0, fiiRequiredBankrollUsdc]) fiiBaselineValueUsdc
      ]
    , case fiiBaselineValueUsdc of
        Just baseline
          | baseline == fiiRequiredBankrollUsdc ->
              concat
                [ ["baseline_official_allocation_count_invalid" | length fiiBaselineOfficialAllocations /= 1]
                , [ "baseline_official_allocation_amount_invalid"
                  | [amount] <- [fiiBaselineOfficialAllocations]
                  , amount /= fiiRequiredBankrollUsdc
                  ]
                , ["baseline_funding_flow_mismatch" | fiiBaselineFundingNetUsdc /= fiiRequiredBankrollUsdc]
                , ["unexpected_official_deposit" | not $ null fiiOfficialAllocations]
                ]
          | baseline == 0 ->
              concat
                [ ["unexpected_prebaseline_official_allocation" | not $ null fiiBaselineOfficialAllocations]
                , ["zero_baseline_funding_flow_mismatch" | fiiBaselineFundingNetUsdc /= 0]
                , ["official_allocation_count_invalid" | length fiiOfficialAllocations /= 1]
                , [ "official_allocation_amount_invalid"
                  | [(amount, _)] <- [fiiOfficialAllocations]
                  , amount /= fiiRequiredBankrollUsdc
                  ]
                , [ "official_allocation_not_before_trading"
                  | [(_, beforeFirstTrade)] <- [fiiOfficialAllocations]
                  , not beforeFirstTrade
                  ]
                ]
        _ -> []
    , ["unverified_deposit_provenance" | fiiUnverifiedDepositCount > 0]
    , ["funding_capacity_exceeded" | fiiMaximumFundingNetUsdc > fiiRequiredBankrollUsdc]
    ]

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
minimumProfitUsdc = crMinimumProfitUsdc

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

fxSessionBoundaryUtcText :: CompetitionRules -> Text
fxSessionBoundaryUtcText rules =
  twoDigits hours <> ":" <> twoDigits minutes
  where
    (hours, minutes) = crFxSessionBoundaryUtcMinutes rules `divMod` 60
    twoDigits value = T.justifyRight 2 '0' $ T.pack $ show value

-- | Convert an execution timestamp to its named FX session. Shifting forward
-- from the configured UTC boundary to midnight maps the Sunday reopen to
-- Monday and the Friday close to Saturday. Weekend dates are rejected.
fxSessionDay :: CompetitionRules -> Integer -> Maybe Day
fxSessionDay rules epochSeconds =
  let boundaryMinutes = crFxSessionBoundaryUtcMinutes rules `mod` (24 * 60)
      shiftSeconds = ((24 * 60 - boundaryMinutes) `mod` (24 * 60)) * 60
      sessionDay = utctDay $ addUTCTime (fromIntegral shiftSeconds) $ posixSecondsToUTCTime $ fromInteger epochSeconds
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
  | otherwise = fxSessionDay rules timestamp
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
