module Plether.Handlers.Insights
  ( getCurrentCompetitionResponse
  , getCompetitionLeaderboardResponse
  , getCompetitionWalletResponse
  , getInsightsDataStatusResponse
  , competitionRowToJson
  , leaderboardRowToJson
  , prizeEligibleAfterIntegrityReview
  , walletRowToJson
  , activityRowToJson
  ) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (defaultTimeLocale, formatTime)
import Data.Time.Clock.POSIX (getPOSIXTime, posixSecondsToUTCTime)
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple.Transaction
  ( IsolationLevel (RepeatableRead)
  , ReadWriteMode (ReadOnly)
  , TransactionMode (..)
  , withTransactionMode
  )
import Plether.Config (Config (..))
import Plether.Database (DbPool, withDb)
import Plether.Database.Insights
  ( CompetitionRow (..)
  , InsightsActivityRow (..)
  , InsightsDataStatusRow (..)
  , LeaderboardRow (..)
  , getCompetitionBySlug
  , getCompetitionLeaderboard
  , getCompetitionWallet
  , getCompetitionWalletActivity
  , getCurrentCompetition
  , getInsightsDataStatus
  )
import Plether.Insights.Competition
  ( CompetitionRegistrationState (..)
  , CompetitionRules (..)
  , ParticipantEligibility (..)
  , PrizeAllocation (..)
  , competitionRegistrationState
  , participantEligibilityFromText
  , prizeAllocation
  )
import Plether.Insights.Registration.Config (RegistrationConfig)
import Plether.Types (ApiError, ApiResponse, mkResponse)
import qualified Plether.Types.Error as E
import Plether.Utils.Address (isValidAddress)

getCurrentCompetitionResponse
  :: DbPool
  -> Config
  -> IO (Either ApiError (ApiResponse Value))
getCurrentCompetitionResponse pool cfg = do
  now <- floor <$> getPOSIXTime
  competition <- withDb pool $ \conn ->
    getCurrentCompetition conn (crSlug $ cfgInsightsCompetitionRules cfg)
  pure $ case competition of
    Nothing -> Left $ E.internalError "Insights competition metadata has not been initialized"
    Just row ->
      Right $
        mkResponse (maybe 0 id $ icrScoreCutoffBlock row) (cfgPerpsChainId cfg) $
          object
            [ "competition" .= competitionRowToJson now (cfgRegistrationConfig cfg) row
            , "generatedAt" .= isoTimestamp now
            , "scoringVersion" .= icrScoringVersion row
            , "provisional" .= not (icrFinalized row)
            ]

getCompetitionLeaderboardResponse
  :: DbPool
  -> Config
  -> Text
  -> Maybe Text
  -> Int
  -> Int
  -> IO (Either ApiError (ApiResponse Value))
getCompetitionLeaderboardResponse pool cfg slug search requestedLimit requestedOffset = do
  now <- floor <$> getPOSIXTime
  let pageLimit = clampLimit requestedLimit
      pageOffset = max 0 requestedOffset
  result <- withDb pool $ \conn -> withInsightsReadSnapshot conn $ do
    competition <- getCompetitionBySlug conn slug
    rows <- case competition of
      Nothing -> pure []
      Just _ -> getCompetitionLeaderboard conn slug search (pageLimit + 1) pageOffset
    pure (competition, rows)
  pure $ case result of
    (Nothing, _) -> Left $ E.notFound $ "Unknown Insights competition: " <> slug
    (Just competition, rows) ->
      let visibleRows = take pageLimit rows
          nextCursor
            | length rows > pageLimit = Just $ T.pack $ show $ pageOffset + pageLimit
            | otherwise = Nothing
          latestBlock = maximum $ maybe 0 id (icrStartBlock competition) : map (maybe 0 id . ilrLatestSnapshotBlock) visibleRows
       in Right $
            mkResponse latestBlock (cfgPerpsChainId cfg) $
              object
                [ "competition" .= competitionRowToJson now (cfgRegistrationConfig cfg) competition
                , "standings" .= map (leaderboardRowToJson competition) visibleRows
                , "nextCursor" .= nextCursor
                , "generatedAt" .= isoTimestamp now
                , "scoringVersion" .= icrScoringVersion competition
                , "provisional" .= not (icrFinalized competition)
                ]

getCompetitionWalletResponse
  :: DbPool
  -> Config
  -> Text
  -> Text
  -> Int
  -> IO (Either ApiError (ApiResponse Value))
getCompetitionWalletResponse pool cfg slug wallet requestedActivityLimit
  | not $ isValidAddress wallet = pure $ Left $ E.invalidAddress wallet
  | otherwise = do
      now <- floor <$> getPOSIXTime
      result <- withDb pool $ \conn -> withInsightsReadSnapshot conn $ do
        competition <- getCompetitionBySlug conn slug
        walletRow <- getCompetitionWallet conn slug wallet
        -- Final standings are immutable.  Do not pair that frozen summary with
        -- reorg-replayable activity rows; callers receive an explicit marker
        -- instead of a temporarily inconsistent history.
        activity <- case competition of
          Just row | icrFinalized row -> pure []
          _ -> getCompetitionWalletActivity conn slug wallet (clampActivityLimit requestedActivityLimit)
        pure (competition, walletRow, activity)
      pure $ case result of
        (Nothing, _, _) -> Left $ E.notFound $ "Unknown Insights competition: " <> slug
        (Just _, Nothing, _) -> Left $ E.notFound "This wallet is not registered for the competition"
        (Just competition, Just walletRow, activity) ->
          let latestBlock =
                maybe
                  (maybe 0 id $ icrStartBlock competition)
                  id
                  (ilrLatestSnapshotBlock walletRow)
           in Right $
                mkResponse latestBlock (cfgPerpsChainId cfg) $
                  object
                    [ "competition" .= competitionRowToJson now (cfgRegistrationConfig cfg) competition
                    , "wallet" .= walletRowToJson competition walletRow
                    , "activity" .= map activityRowToJson activity
                    , "activityStatus" .= if icrFinalized competition
                        then ("omitted_after_finalization" :: Text)
                        else "live"
                    , "generatedAt" .= isoTimestamp now
                    , "scoringVersion" .= icrScoringVersion competition
                    , "provisional" .= not (icrFinalized competition)
                    ]

getInsightsDataStatusResponse
  :: DbPool
  -> Config
  -> IO (Either ApiError (ApiResponse Value))
getInsightsDataStatusResponse pool cfg = do
  now <- floor <$> getPOSIXTime
  result <- withDb pool $ \conn -> withInsightsReadSnapshot conn $ do
    competition <- getCurrentCompetition conn (crSlug $ cfgInsightsCompetitionRules cfg)
    status <- case competition of
      Nothing -> pure Nothing
      Just row -> getInsightsDataStatus conn (icrSlug row)
    pure (competition, status)
  pure $ case result of
    (Nothing, _) -> Left $ E.internalError "Insights competition metadata has not been initialized"
    (Just _, Nothing) -> Left $ E.internalError "Insights data status is unavailable"
    (Just competition, Just status) ->
      let indexedBlock = maybe 0 id $ idsrIndexerBlock status
       in Right $
            mkResponse indexedBlock (cfgPerpsChainId cfg) $
              object
                [ "competition" .= competitionRowToJson now (cfgRegistrationConfig cfg) competition
                , "status" .= dataStatusRowToJson competition status
                , "generatedAt" .= isoTimestamp now
                , "scoringVersion" .= icrScoringVersion competition
                , "provisional" .= not (icrFinalized competition)
                ]

withInsightsReadSnapshot :: Connection -> IO value -> IO value
withInsightsReadSnapshot =
  withTransactionMode $
    TransactionMode
      { isolationLevel = RepeatableRead
      , readWriteMode = ReadOnly
      }

competitionRowToJson :: Integer -> Maybe RegistrationConfig -> CompetitionRow -> Value
competitionRowToJson now _registrationConfig competition@CompetitionRow {..} =
  object $
    catMaybes
      [ Just $ "slug" .= icrSlug
      , Just $ "name" .= icrName
      , Just $ "chainId" .= show icrChainId
      , if icrReleaseReady then Just $ "releaseRouter" .= icrReleaseRouter else Nothing
      , Just $ "releaseReady" .= icrReleaseReady
      , Just $ "phase" .= competitionPhase
      , Just $ "startAt" .= isoTimestamp icrStartTimestamp
      , Just $ "newRiskCutoffAt" .= isoTimestamp icrNewRiskCutoffTimestamp
      , Just $ "scoreCutoffAt" .= isoTimestamp icrScoreCutoffTimestamp
      , Just $ "resultsAt" .= isoTimestamp icrResultsTimestamp
      , Just $ "paymentDeadlineAt" .= isoTimestamp icrPaymentDeadlineTimestamp
      , ("startBlock" .=) . show <$> icrStartBlock
      , ("scoreCutoffBlock" .=) . show <$> icrScoreCutoffBlock
      , Just $ "startingBalanceUsdc" .= show icrStartingBalanceUsdc
      , Just $ "minimumProfitUsdc" .= show minimumProfit
      , Just $ "minimumProfitBps" .= icrMinimumProfitBps
      , Just $ "minimumActiveDays" .= icrMinimumActiveDays
      , Just $ "fxSessionBoundaryUtc" .= formatBoundary icrFxSessionBoundaryUtcMinutes
      , Just $
          "prizes"
            .= [ object ["place" .= place, "amountUsdc" .= show amount]
               | (place, amount) <- zip [(1 :: Int) ..] $ competitionPrizeAmounts competition
               ]
      , Just $ "scoringVersion" .= icrScoringVersion
      , Just $ "rulesVersion" .= icrRulesVersion
      , Just $ "participantCount" .= icrParticipantCount
      , Just $ "finalized" .= icrFinalized
      , Just $ "updatedAt" .= isoTimestamp icrUpdatedTimestamp
      , ("registration" .=) <$> registrationMetadata
      ]
  where
    minimumProfit = icrStartingBalanceUsdc * icrMinimumProfitBps `div` 10_000
    competitionPhase
      | icrFinalized = "final" :: Text
      | now < icrStartTimestamp = "upcoming"
      | now < icrScoreCutoffTimestamp = "live"
      | now < icrResultsTimestamp = "review"
      | otherwise = "provisional_results"
    registrationMetadata = do
      RegistrationOpened opensAt closesAt <-
        pure $
          competitionRegistrationState
            icrRegistrationOpenTimestamp
            icrRegistrationCloseTimestamp
            icrMinimumXAccountAgeDays
            icrTargetXHandle
      minimumAge <- icrMinimumXAccountAgeDays
      targetHandle <- icrTargetXHandle
      privacyVersion <- icrPrivacyNoticeVersion
      pure $
        object
          [ "status" .= registrationStatus opensAt closesAt
          , "opensAt" .= isoTimestamp opensAt
          , "closesAt" .= isoTimestamp closesAt
          , "minimumXAccountAgeDays" .= minimumAge
          , "targetXHandle" .= targetHandle
          , "rulesVersion" .= icrRulesVersion
          , "privacyVersion" .= privacyVersion
          ]
    registrationStatus opensAt closesAt
      | now < opensAt = "upcoming" :: Text
      | now < closesAt = "open"
      | otherwise = "closed"
    formatBoundary minutes =
      let (hours, remainingMinutes) = minutes `divMod` 60
          twoDigits value = T.justifyRight 2 '0' $ T.pack $ show value
       in twoDigits hours <> ":" <> twoDigits remainingMinutes

leaderboardRowToJson :: CompetitionRow -> LeaderboardRow -> Value
leaderboardRowToJson competition LeaderboardRow {..} =
  object $
    catMaybes
      [ ("rank" .=) <$> ilrRank
      , ("prizePlace" .=) . paPlace <$> allocation
      , ("prizePlaces" .=) . paPlaces <$> allocation
      , ("prizeAmountUsdc" .=) . show . paAmountUsdc <$> allocation
      , Just $ "wallet" .= ilrWallet
      , ("alias" .=) <$> ilrAlias
      , Just $ "eligibilityStatus" .= ilrEligibilityStatus
      , ("eligibilityReason" .=) <$> ilrEligibilityReason
      , ("finalPnlUsdc" .=) . show <$> ilrFinalPnlUsdc
      , ("roiBps" .=) <$> ilrRoiBps
      , ("startingAccountValueUsdc" .=) . show <$> ilrStartingAccountValueUsdc
      , ("currentAccountValueUsdc" .=) . show <$> ilrCurrentAccountValueUsdc
      , Just $ "depositsUsdc" .= show ilrDepositsUsdc
      , Just $ "withdrawalsUsdc" .= show ilrWithdrawalsUsdc
      , Just $ "manualAdjustmentsUsdc" .= show ilrManualAdjustmentsUsdc
      , Just $ "activeDays" .= ilrActiveDays
      , Just $ "volumeUsdc" .= show ilrVolumeUsdc
      , Just $ "executedTrades" .= ilrExecutedTrades
      , Just $ "liquidations" .= ilrLiquidations
      , Just $ "realizedPnlUsdc" .= show ilrRealizedPnlUsdc
      , ("snapshotBlock" .=) . show <$> ilrLatestSnapshotBlock
      , ("snapshotAt" .=) . isoTimestamp <$> ilrLatestSnapshotTimestamp
      , ("hasOpenPosition" .=) <$> ilrHasOpenPosition
      , ("snapshotKind" .=) <$> ilrLatestSnapshotKind
      , Just $ "meetsProfitRequirement" .= meetsProfit
      , Just $ "meetsActiveDaysRequirement" .= meetsDays
      , Just $ "mechanicallyQualified" .= mechanicallyQualified
      , Just $ "prizeEligible" .= prizeEligible
      , Just $ "scoreAvailable" .= scoreAvailable
      ]
  where
    minimumProfit = icrStartingBalanceUsdc competition * icrMinimumProfitBps competition `div` 10_000
    meetsProfit = maybe False (>= minimumProfit) ilrFinalPnlUsdc
    meetsDays = ilrActiveDays >= icrMinimumActiveDays competition
    mechanicallyQualified = meetsProfit && meetsDays
    scoreAvailable = maybe False (const True) ilrFinalPnlUsdc
    reviewedEligible =
      participantEligibilityFromText ilrEligibilityStatus == Just EligibilityEligible
    prizeEligible =
      prizeEligibleAfterIntegrityReview
        mechanicallyQualified
        reviewedEligible
        ilrFundingIntegrityClear
    allocation =
      prizeAllocation
        (competitionPrizeAmounts competition)
        ilrPrizePlace
        ilrPrizeTieCount

competitionPrizeAmounts :: CompetitionRow -> [Integer]
competitionPrizeAmounts CompetitionRow {..} =
  filter
    (> 0)
    [ icrFirstPrizeUsdc
    , icrSecondPrizeUsdc
    , icrThirdPrizeUsdc
    , icrFourthPrizeUsdc
    , icrFifthPrizeUsdc
    ]

prizeEligibleAfterIntegrityReview :: Bool -> Bool -> Bool -> Bool
prizeEligibleAfterIntegrityReview mechanicallyQualified reviewedEligible integrityClear =
  mechanicallyQualified && reviewedEligible && integrityClear

walletRowToJson :: CompetitionRow -> LeaderboardRow -> Value
walletRowToJson competition row =
  case leaderboardRowToJson competition row of
    Object fields -> Object $ KeyMap.insert "position" (walletPositionToJson row) fields
    value -> value

walletPositionToJson :: LeaderboardRow -> Value
walletPositionToJson LeaderboardRow {..}
  | ilrHasOpenPosition /= Just True = Null
  | otherwise =
      object $
        catMaybes
          [ Just $ "market" .= ("plDXY Perp" :: Text)
          , ("side" .=) <$> positionSide ilrPositionSide
          , ("sideCode" .=) <$> ilrPositionSide
          , ("sizeDelta" .=) <$> ilrPositionSizeDelta
          , ("marginUsdc" .=) <$> ilrPositionMarginUsdc
          , ("entryPrice" .=) <$> ilrPositionEntryPrice
          , ("unrealizedPnlUsdc" .=) <$> ilrPositionUnrealizedPnlUsdc
          , ("liquidatable" .=) <$> ilrPositionLiquidatable
          ]
  where
    positionSide = \case
      Just "0" -> Just ("long" :: Text)
      Just "1" -> Just "short"
      _ -> Nothing

activityRowToJson :: InsightsActivityRow -> Value
activityRowToJson InsightsActivityRow {..} =
  object $
    catMaybes
      [ Just $ "activityType" .= iarActivityType
      , ("side" .=) <$> iarSide
      , ("price" .=) . show <$> iarPrice
      , ("sizeDelta" .=) . show <$> iarSizeDelta
      , ("amountUsdc" .=) . show <$> iarAmountUsdc
      , ("pnlUsdc" .=) . show <$> iarPnlUsdc
      , ("executionFeeUsdc" .=) . show <$> iarExecutionFeeUsdc
      , ("vpiUsdc" .=) . show <$> iarVpiUsdc
      , Just $ "txHash" .= iarTxHash
      , Just $ "blockNumber" .= show iarBlockNumber
      , Just $ "timestamp" .= iarTimestamp
      , Just $ "occurredAt" .= isoTimestamp iarTimestamp
      , Just $ "logIndex" .= iarLogIndex
      , ("sessionDay" .=) <$> iarSessionDay
      ]

dataStatusRowToJson :: CompetitionRow -> InsightsDataStatusRow -> Value
dataStatusRowToJson competition InsightsDataStatusRow {..} =
  object $
    catMaybes
      [ Just $ "participantCount" .= idsrParticipantCount
      , Just $ "snapshottedWalletCount" .= idsrSnapshottedWalletCount
      , Just $ "startSnapshotCount" .= idsrStartSnapshotCount
      , Just $ "finalSnapshotCount" .= idsrFinalSnapshotCount
      , ("snapshotThroughBlock" .=) . show <$> idsrLatestSnapshotBlock
      , ("latestSnapshotAt" .=) . isoTimestamp <$> idsrLatestSnapshotTimestamp
      , ("indexedThroughBlock" .=) . show <$> idsrIndexerBlock
      , ("indexedThroughBlockHash" .=) <$> idsrIndexerBlockHash
      , ("indexerUpdatedAt" .=) . isoTimestamp <$> idsrIndexerUpdatedTimestamp
      , ("snapshotWorkerUpdatedAt" .=) . isoTimestamp <$> idsrSnapshotWorkerUpdatedTimestamp
      , Just $ "startSnapshotsComplete" .= startComplete
      , Just $ "finalSnapshotsComplete" .= finalComplete
      , Just $ "provisional" .= not (icrFinalized competition)
      ]
  where
    hasParticipants = idsrParticipantCount > 0
    startComplete = hasParticipants && idsrStartSnapshotCount >= idsrParticipantCount
    finalComplete = hasParticipants && idsrFinalSnapshotCount >= idsrParticipantCount

isoTimestamp :: Integer -> Text
isoTimestamp timestamp =
  T.pack $
    formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" $
      posixSecondsToUTCTime $ fromInteger timestamp

clampLimit :: Int -> Int
clampLimit = min 100 . max 1

clampActivityLimit :: Int -> Int
clampActivityLimit = min 500 . max 1
