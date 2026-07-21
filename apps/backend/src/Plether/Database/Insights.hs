module Plether.Database.Insights
  ( CompetitionRow (..)
  , CompetitionSeedMetadata (..)
  , CompetitionSeedMismatch (..)
  , ParticipantRow (..)
  , SnapshotKind (..)
  , AccountSnapshotInput (..)
  , LeaderboardRow (..)
  , InsightsActivityRow (..)
  , InsightsDataStatusRow (..)
  , ensureInsightsSchema
  , seedJuly2026Competition
  , competitionSeedMetadataFor
  , competitionSeedMismatches
  , isLegacyPaymentDeadlineOnlyMismatch
  , setCompetitionBoundaryBlocks
  , upsertCompetitionParticipant
  , stageCompetitionParticipantWalletRemap
  , applyCompetitionParticipantWalletRemaps
  , setParticipantEligibility
  , finalizeCompetition
  , listCompetitionParticipants
  , publishAccountSnapshotBatch
  , hasCompleteAccountSnapshotBatch
  , invalidateSnapshotBatchesAfter
  , insertManualAdjustment
  , voidManualAdjustment
  , getCurrentCompetition
  , getCompetitionBySlug
  , getCompetitionLeaderboard
  , getCompetitionWallet
  , getCompetitionWalletActivity
  , getInsightsDataStatus
  , getLatestIndexedSafeBlock
  , leaderboardSearchPattern
  , leaderboardQuerySql
  , insightsDataStatusQuerySql
  , snapshotBatchAccessIndexSql
  , walletActivityQuerySql
  , snapshotKindText
  ) where

import Control.Monad (unless, when)
import Data.Aeson (Value, encode)
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int64)
import Data.List (nub, sort)
import Data.Scientific (Scientific, base10Exponent, coefficient)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (getPOSIXTime, utcTimeToPOSIXSeconds)
import Database.PostgreSQL.Simple
  ( Connection
  , Only (..)
  , Query
  , execute
  , executeMany
  , execute_
  , query
  , query_
  , withTransaction
  )
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import Database.PostgreSQL.Simple.Internal (RowParser)
import Plether.Insights.Competition
  ( CompetitionRules (..)
  , EquitySnapshot (..)
  , FinalizationReadiness (..)
  , ParticipantEligibility
  , finalizationBlockers
  , july2026Competition
  , july2026CompetitionSlug
  , participantEligibilityText
  )
import Plether.Utils.Address (isValidAddress)

-- | Immutable identity, schedule, scoring, and prize metadata for a seeded
-- competition. Operational state such as resolved boundary blocks and
-- finalization deliberately lives outside this record and changes only through
-- its explicit update paths.
data CompetitionSeedMetadata = CompetitionSeedMetadata
  { csmSlug :: Text
  , csmName :: Text
  , csmChainId :: Integer
  , csmReleaseRouter :: Text
  , csmUsdcAddress :: Text
  , csmMarginClearinghouseAddress :: Text
  , csmStartTimestamp :: Integer
  , csmNewRiskCutoffTimestamp :: Integer
  , csmScoreCutoffTimestamp :: Integer
  , csmResultsTimestamp :: Integer
  , csmPaymentDeadlineTimestamp :: Integer
  , csmStartingBalanceUsdc :: Integer
  , csmMinimumProfitBps :: Integer
  , csmMinimumActiveDays :: Int
  , csmScoringVersion :: Text
  , csmRulesVersion :: Text
  , csmFirstPrizeUsdc :: Integer
  , csmSecondPrizeUsdc :: Integer
  , csmThirdPrizeUsdc :: Integer
  }
  deriving stock (Show, Eq)

instance FromRow CompetitionSeedMetadata where
  fromRow = CompetitionSeedMetadata
    <$> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> numericIntegerFieldRequired
    <*> field
    <*> field
    <*> field
    <*> field
    <*> numericIntegerFieldRequired
    <*> numericIntegerFieldRequired
    <*> numericIntegerFieldRequired

data CompetitionSeedMismatch = CompetitionSeedMismatch
  { csmmField :: Text
  , csmmStored :: Text
  , csmmExpected :: Text
  }
  deriving stock (Show, Eq)

data CompetitionRow = CompetitionRow
  { icrSlug :: Text
  , icrName :: Text
  , icrChainId :: Integer
  , icrReleaseRouter :: Text
  , icrUsdcAddress :: Text
  , icrMarginClearinghouseAddress :: Text
  , icrAccountLensAddress :: Text
  , icrStartTimestamp :: Integer
  , icrNewRiskCutoffTimestamp :: Integer
  , icrScoreCutoffTimestamp :: Integer
  , icrResultsTimestamp :: Integer
  , icrPaymentDeadlineTimestamp :: Integer
  , icrStartBlock :: Maybe Integer
  , icrStartBlockHash :: Maybe Text
  , icrScoreCutoffBlock :: Maybe Integer
  , icrScoreCutoffBlockHash :: Maybe Text
  , icrStartingBalanceUsdc :: Integer
  , icrMinimumProfitBps :: Integer
  , icrMinimumActiveDays :: Int
  , icrScoringVersion :: Text
  , icrRulesVersion :: Text
  , icrFirstPrizeUsdc :: Integer
  , icrSecondPrizeUsdc :: Integer
  , icrThirdPrizeUsdc :: Integer
  , icrFinalized :: Bool
  , icrUpdatedTimestamp :: Integer
  }
  deriving stock (Show, Eq)

instance FromRow CompetitionRow where
  fromRow = CompetitionRow
    <$> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> numericIntegerFieldRequired
    <*> field
    <*> field
    <*> field
    <*> field
    <*> numericIntegerFieldRequired
    <*> numericIntegerFieldRequired
    <*> numericIntegerFieldRequired
    <*> field
    <*> field

data ParticipantRow = ParticipantRow
  { iprCompetitionSlug :: Text
  , iprWallet :: Text
  , iprAlias :: Maybe Text
  , iprEligibilityStatus :: Text
  , iprEligibilityReason :: Maybe Text
  , iprIntegrityFlags :: Value
  , iprRegisteredTimestamp :: Integer
  , iprReviewedTimestamp :: Maybe Integer
  }
  deriving stock (Show, Eq)

instance FromRow ParticipantRow where
  fromRow = ParticipantRow
    <$> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field

data SnapshotKind
  = SnapshotStart
  | SnapshotLive
  | SnapshotFinal
  deriving stock (Show, Eq, Ord)

snapshotKindText :: SnapshotKind -> Text
snapshotKindText = \case
  SnapshotStart -> "start"
  SnapshotLive -> "live"
  SnapshotFinal -> "final"

data AccountSnapshotInput = AccountSnapshotInput
  { asiCompetitionSlug :: Text
  , asiWallet :: Text
  , asiKind :: SnapshotKind
  , asiChainId :: Integer
  , asiReleaseRouter :: Text
  , asiAccountLensAddress :: Text
  , asiBlockNumber :: Integer
  , asiBlockHash :: Text
  , asiTimestamp :: Integer
  , asiEquity :: EquitySnapshot
  , asiRawData :: Value
  }
  deriving stock (Show, Eq)

data LeaderboardRow = LeaderboardRow
  { ilrRank :: Maybe Integer
  , ilrPrizePlace :: Maybe Integer
  , ilrPrizeTieCount :: Maybe Integer
  , ilrWallet :: Text
  , ilrAlias :: Maybe Text
  , ilrEligibilityStatus :: Text
  , ilrEligibilityReason :: Maybe Text
  , ilrFinalPnlUsdc :: Maybe Integer
  , ilrRoiBps :: Maybe Integer
  , ilrStartingAccountValueUsdc :: Maybe Integer
  , ilrCurrentAccountValueUsdc :: Maybe Integer
  , ilrDepositsUsdc :: Integer
  , ilrWithdrawalsUsdc :: Integer
  , ilrManualAdjustmentsUsdc :: Integer
  , ilrActiveDays :: Int
  , ilrVolumeUsdc :: Integer
  , ilrExecutedTrades :: Integer
  , ilrLiquidations :: Integer
  , ilrRealizedPnlUsdc :: Integer
  , ilrLatestSnapshotBlock :: Maybe Integer
  , ilrLatestSnapshotTimestamp :: Maybe Integer
  , ilrHasOpenPosition :: Maybe Bool
  , ilrLatestSnapshotKind :: Maybe Text
  , ilrPositionSide :: Maybe Text
  , ilrPositionSizeDelta :: Maybe Text
  , ilrPositionMarginUsdc :: Maybe Text
  , ilrPositionEntryPrice :: Maybe Text
  , ilrPositionUnrealizedPnlUsdc :: Maybe Text
  , ilrPositionLiquidatable :: Maybe Bool
  }
  deriving stock (Show, Eq)

instance FromRow LeaderboardRow where
  fromRow = LeaderboardRow
    <$> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> numericIntegerField
    <*> field
    <*> numericIntegerField
    <*> numericIntegerField
    <*> numericIntegerFieldRequired
    <*> numericIntegerFieldRequired
    <*> numericIntegerFieldRequired
    <*> field
    <*> numericIntegerFieldRequired
    <*> field
    <*> field
    <*> numericIntegerFieldRequired
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field

data InsightsActivityRow = InsightsActivityRow
  { iarActivityType :: Text
  , iarSide :: Maybe Int
  , iarPrice :: Maybe Integer
  , iarSizeDelta :: Maybe Integer
  , iarAmountUsdc :: Maybe Integer
  , iarPnlUsdc :: Maybe Integer
  , iarTxHash :: Text
  , iarBlockNumber :: Integer
  , iarTimestamp :: Integer
  , iarLogIndex :: Integer
  , iarSessionDay :: Maybe Text
  }
  deriving stock (Show, Eq)

instance FromRow InsightsActivityRow where
  fromRow = InsightsActivityRow
    <$> field
    <*> field
    <*> numericIntegerField
    <*> numericIntegerField
    <*> numericIntegerField
    <*> numericIntegerField
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field

data InsightsDataStatusRow = InsightsDataStatusRow
  { idsrParticipantCount :: Integer
  , idsrSnapshottedWalletCount :: Integer
  , idsrStartSnapshotCount :: Integer
  , idsrFinalSnapshotCount :: Integer
  , idsrLatestSnapshotBlock :: Maybe Integer
  , idsrLatestSnapshotTimestamp :: Maybe Integer
  , idsrIndexerBlock :: Maybe Integer
  , idsrIndexerBlockHash :: Maybe Text
  , idsrIndexerUpdatedTimestamp :: Maybe Integer
  , idsrSnapshotWorkerUpdatedTimestamp :: Maybe Integer
  }
  deriving stock (Show, Eq)

instance FromRow InsightsDataStatusRow where
  fromRow = InsightsDataStatusRow
    <$> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field

data FinalizationDatabaseRow = FinalizationDatabaseRow
  { fdrAlreadyFinalized :: Bool
  , fdrScoreCutoffTimestamp :: Integer
  , fdrResultsTimestamp :: Integer
  , fdrStartBlock :: Maybe Integer
  , fdrScoreCutoffBlock :: Maybe Integer
  , fdrParticipantCount :: Integer
  , fdrMissingTraderReferences :: Integer
  , fdrUnresolvedReviews :: Integer
  , fdrStartSnapshotCount :: Integer
  , fdrFinalSnapshotCount :: Integer
  , fdrFinalSnapshotHashCount :: Integer
  , fdrFinalSnapshotHash :: Maybe Text
  }

instance FromRow FinalizationDatabaseRow where
  fromRow = FinalizationDatabaseRow
    <$> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field

snapshotBatchAccessIndexSql :: Query
snapshotBatchAccessIndexSql =
  "CREATE INDEX IF NOT EXISTS idx_insights_snapshots_batch_wallet \
  \ ON insights_account_snapshots(competition_slug, snapshot_kind, block_number, wallet)"

ensureInsightsSchema :: Connection -> Integer -> Text -> Text -> Text -> Text -> IO ()
ensureInsightsSchema conn chainId releaseRouter usdcAddress marginClearinghouseAddress accountLensAddress = do
  validateOfficialAddress "PERPS_ORDER_ROUTER" releaseRouter
  validateOfficialAddress "PERPS_USDC" usdcAddress
  validateOfficialAddress "PERPS_MARGIN_CLEARINGHOUSE" marginClearinghouseAddress
  validateOfficialAddress "PERPS_ACCOUNT_LENS" accountLensAddress
  _ <- execute_ conn
    "CREATE TABLE IF NOT EXISTS insights_competitions (\
    \ slug TEXT PRIMARY KEY,\
    \ name TEXT NOT NULL,\
    \ chain_id BIGINT NOT NULL,\
    \ release_router TEXT NOT NULL,\
    \ usdc_address TEXT NOT NULL,\
    \ margin_clearinghouse_address TEXT NOT NULL,\
    \ account_lens_address TEXT,\
    \ start_timestamp BIGINT NOT NULL,\
    \ new_risk_cutoff_timestamp BIGINT NOT NULL,\
    \ score_cutoff_timestamp BIGINT NOT NULL,\
    \ results_timestamp BIGINT NOT NULL,\
    \ payment_deadline_timestamp BIGINT NOT NULL,\
    \ start_block BIGINT,\
    \ start_block_hash TEXT,\
    \ score_cutoff_block BIGINT,\
    \ score_cutoff_block_hash TEXT,\
    \ starting_balance_usdc NUMERIC(78,0) NOT NULL,\
    \ minimum_profit_bps BIGINT NOT NULL,\
    \ minimum_active_days INTEGER NOT NULL,\
    \ scoring_version TEXT NOT NULL,\
    \ rules_version TEXT NOT NULL,\
    \ first_prize_usdc NUMERIC(78,0) NOT NULL,\
    \ second_prize_usdc NUMERIC(78,0) NOT NULL,\
    \ third_prize_usdc NUMERIC(78,0) NOT NULL,\
    \ finalized BOOLEAN NOT NULL DEFAULT FALSE,\
    \ created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),\
    \ updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),\
    \ CHECK (new_risk_cutoff_timestamp >= start_timestamp),\
    \ CHECK (score_cutoff_timestamp >= new_risk_cutoff_timestamp),\
    \ CHECK (minimum_profit_bps >= 0),\
    \ CHECK (minimum_active_days >= 0)\
    \ )"
  _ <- execute_ conn
    "ALTER TABLE insights_competitions ADD COLUMN IF NOT EXISTS usdc_address TEXT"
  _ <- execute_ conn
    "ALTER TABLE insights_competitions ADD COLUMN IF NOT EXISTS margin_clearinghouse_address TEXT"
  _ <- execute_ conn
    "ALTER TABLE insights_competitions ADD COLUMN IF NOT EXISTS account_lens_address TEXT"
  _ <- execute_ conn
    "ALTER TABLE insights_competitions ADD COLUMN IF NOT EXISTS start_block_hash TEXT"
  _ <- execute_ conn
    "ALTER TABLE insights_competitions ADD COLUMN IF NOT EXISTS score_cutoff_block_hash TEXT"
  _ <- execute conn
    "UPDATE insights_competitions SET usdc_address = ? WHERE usdc_address IS NULL"
    (Only $ normalizeAddress usdcAddress)
  _ <- execute conn
    "UPDATE insights_competitions SET margin_clearinghouse_address = ? WHERE margin_clearinghouse_address IS NULL"
    (Only $ normalizeAddress marginClearinghouseAddress)
  _ <- execute_ conn
    "ALTER TABLE insights_competitions ALTER COLUMN usdc_address SET NOT NULL"
  _ <- execute_ conn
    "ALTER TABLE insights_competitions ALTER COLUMN margin_clearinghouse_address SET NOT NULL"
  _ <- execute_ conn
    "CREATE TABLE IF NOT EXISTS insights_competition_participants (\
    \ competition_slug TEXT NOT NULL REFERENCES insights_competitions(slug) ON DELETE CASCADE,\
    \ wallet VARCHAR(42) NOT NULL,\
    \ trader_reference TEXT,\
    \ alias TEXT,\
    \ eligibility_status TEXT NOT NULL DEFAULT 'pending',\
    \ eligibility_reason TEXT,\
    \ integrity_flags JSONB NOT NULL DEFAULT '[]'::jsonb,\
    \ registered_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),\
    \ reviewed_at TIMESTAMPTZ,\
    \ created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),\
    \ updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),\
    \ PRIMARY KEY (competition_slug, wallet),\
    \ CHECK (eligibility_status IN ('pending', 'eligible', 'under_review', 'ineligible'))\
    \ )"
  _ <- execute_ conn
    "ALTER TABLE insights_competition_participants ADD COLUMN IF NOT EXISTS trader_reference TEXT"
  _ <- execute_ conn
    "CREATE INDEX IF NOT EXISTS idx_insights_participants_wallet \
    \ ON insights_competition_participants(wallet)"
  _ <- execute_ conn
    "CREATE UNIQUE INDEX IF NOT EXISTS idx_insights_participants_trader_reference \
    \ ON insights_competition_participants(competition_slug, trader_reference) \
    \ WHERE trader_reference IS NOT NULL"
  _ <- execute_ conn
    "CREATE TABLE IF NOT EXISTS insights_participant_wallet_remaps (\
    \ competition_slug TEXT NOT NULL REFERENCES insights_competitions(slug) ON DELETE CASCADE,\
    \ trader_reference TEXT NOT NULL,\
    \ old_wallet VARCHAR(42) NOT NULL,\
    \ new_wallet VARCHAR(42) NOT NULL,\
    \ staged_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),\
    \ applied_at TIMESTAMPTZ,\
    \ applied_by TEXT,\
    \ PRIMARY KEY (competition_slug, trader_reference),\
    \ UNIQUE (competition_slug, new_wallet)\
    \ )"
  _ <- execute_ conn
    "CREATE TABLE IF NOT EXISTS insights_account_snapshots (\
    \ competition_slug TEXT NOT NULL REFERENCES insights_competitions(slug) ON DELETE CASCADE,\
    \ wallet VARCHAR(42) NOT NULL,\
    \ snapshot_kind TEXT NOT NULL,\
    \ chain_id BIGINT NOT NULL,\
    \ release_router TEXT NOT NULL,\
    \ block_number BIGINT NOT NULL,\
    \ block_hash TEXT NOT NULL,\
    \ timestamp BIGINT NOT NULL,\
    \ has_open_position BOOLEAN NOT NULL,\
    \ signed_net_equity_usdc NUMERIC(78,0) NOT NULL,\
    \ terminal_reachable_usdc NUMERIC(78,0) NOT NULL,\
    \ trader_claims_usdc NUMERIC(78,0) NOT NULL,\
    \ raw_data JSONB NOT NULL,\
    \ created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),\
    \ updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),\
    \ PRIMARY KEY (competition_slug, wallet, snapshot_kind, block_number),\
    \ CHECK (snapshot_kind IN ('start', 'live', 'final'))\
    \ )"
  _ <- execute_ conn
    "CREATE INDEX IF NOT EXISTS idx_insights_snapshots_latest \
    \ ON insights_account_snapshots(competition_slug, wallet, block_number DESC)"
  _ <- execute_ conn
    "CREATE INDEX IF NOT EXISTS idx_insights_snapshots_kind \
    \ ON insights_account_snapshots(competition_slug, snapshot_kind, wallet)"
  _ <- execute_ conn snapshotBatchAccessIndexSql
  _ <- execute_ conn
    "CREATE TABLE IF NOT EXISTS insights_snapshot_batches (\
    \ competition_slug TEXT NOT NULL REFERENCES insights_competitions(slug) ON DELETE CASCADE,\
    \ snapshot_kind TEXT NOT NULL,\
    \ chain_id BIGINT NOT NULL,\
    \ release_router TEXT NOT NULL,\
    \ account_lens_address TEXT,\
    \ block_number BIGINT NOT NULL,\
    \ block_hash TEXT NOT NULL,\
    \ timestamp BIGINT NOT NULL,\
    \ participant_count INTEGER NOT NULL,\
    \ account_state_count INTEGER,\
    \ published_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),\
    \ PRIMARY KEY (competition_slug, snapshot_kind, block_number),\
    \ CHECK (snapshot_kind IN ('start', 'live', 'final')),\
    \ CHECK (participant_count > 0)\
    \ )"
  _ <- execute_ conn
    "CREATE INDEX IF NOT EXISTS idx_insights_snapshot_batches_latest \
    \ ON insights_snapshot_batches(competition_slug, block_number DESC, published_at DESC)"
  _ <- execute_ conn
    "CREATE TABLE IF NOT EXISTS insights_manual_adjustments (\
    \ id BIGSERIAL PRIMARY KEY,\
    \ competition_slug TEXT NOT NULL,\
    \ wallet VARCHAR(42) NOT NULL,\
    \ amount_usdc NUMERIC(78,0) NOT NULL,\
    \ reason TEXT NOT NULL,\
    \ created_by TEXT NOT NULL,\
    \ created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),\
    \ voided_at TIMESTAMPTZ,\
    \ voided_by TEXT,\
    \ void_reason TEXT,\
    \ FOREIGN KEY (competition_slug, wallet) REFERENCES insights_competition_participants(competition_slug, wallet) ON DELETE CASCADE\
    \ )"
  _ <- execute_ conn
    "CREATE INDEX IF NOT EXISTS idx_insights_adjustments_wallet \
    \ ON insights_manual_adjustments(competition_slug, wallet) WHERE voided_at IS NULL"
  _ <- execute_ conn
    "CREATE TABLE IF NOT EXISTS insights_eligibility_audit (\
    \ id BIGSERIAL PRIMARY KEY,\
    \ competition_slug TEXT NOT NULL,\
    \ wallet VARCHAR(42) NOT NULL,\
    \ previous_status TEXT NOT NULL,\
    \ new_status TEXT NOT NULL,\
    \ reason TEXT,\
    \ reviewed_by TEXT NOT NULL,\
    \ created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),\
    \ FOREIGN KEY (competition_slug, wallet) REFERENCES insights_competition_participants(competition_slug, wallet) ON DELETE CASCADE\
    \ )"
  _ <- execute_ conn
    "CREATE TABLE IF NOT EXISTS insights_competition_finalization_audit (\
    \ id BIGSERIAL PRIMARY KEY,\
    \ competition_slug TEXT NOT NULL REFERENCES insights_competitions(slug) ON DELETE CASCADE,\
    \ finalized_by TEXT NOT NULL,\
    \ participant_count BIGINT NOT NULL,\
    \ final_snapshot_block BIGINT NOT NULL,\
    \ final_snapshot_hash TEXT NOT NULL,\
    \ created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()\
    \ )"
  seedJuly2026Competition conn chainId releaseRouter usdcAddress marginClearinghouseAddress accountLensAddress
  reconcileCompetitionAccountLens conn july2026CompetitionSlug accountLensAddress
  _ <- execute_ conn
    "ALTER TABLE insights_competitions ALTER COLUMN account_lens_address SET NOT NULL"
  _ <- execute_ conn
    "ALTER TABLE insights_snapshot_batches ADD COLUMN IF NOT EXISTS account_lens_address TEXT"
  _ <- execute_ conn
    "ALTER TABLE insights_snapshot_batches ADD COLUMN IF NOT EXISTS account_state_count INTEGER"
  _ <- execute_ conn
    "UPDATE insights_snapshot_batches b SET account_lens_address = c.account_lens_address\
    \ FROM insights_competitions c WHERE c.slug = b.competition_slug\
    \ AND b.account_lens_address IS NULL"
  _ <- execute_ conn
    "UPDATE insights_snapshot_batches b SET account_state_count = (\
    \ SELECT COUNT(*)::integer FROM insights_account_snapshots s\
    \ WHERE s.competition_slug = b.competition_slug AND s.snapshot_kind = b.snapshot_kind\
    \ AND s.block_number = b.block_number AND LOWER(s.block_hash) = LOWER(b.block_hash)\
    \ AND (s.has_open_position OR s.signed_net_equity_usdc <> 0\
    \   OR s.terminal_reachable_usdc <> 0 OR s.trader_claims_usdc <> 0))\
    \ WHERE b.account_state_count IS NULL"
  _ <- execute_ conn
    "ALTER TABLE insights_snapshot_batches ALTER COLUMN account_lens_address SET NOT NULL"
  _ <- execute_ conn
    "ALTER TABLE insights_snapshot_batches ALTER COLUMN account_state_count SET NOT NULL"
  pure ()

-- Snapshot values are only meaningful for the exact lens implementation that
-- produced them. A configured lens change invalidates every mutable snapshot
-- atomically so the worker rebuilds both the historical baseline and live data.
-- Finalized competition results are never rewritten automatically.
reconcileCompetitionAccountLens :: Connection -> Text -> Text -> IO ()
reconcileCompetitionAccountLens conn slug accountLensAddress =
  withTransaction conn $ do
    rows <- query conn
      "SELECT account_lens_address, finalized FROM insights_competitions\
      \ WHERE slug = ? FOR UPDATE"
      (Only slug)
    case rows of
      [(storedAddress, finalized)]
        | fmap normalizeAddress storedAddress == Just normalizedAddress -> pure ()
        | finalized ->
            ioError $ userError $
              "PERPS_ACCOUNT_LENS changed for finalized Insights competition "
                <> T.unpack slug
                <> "; refusing to invalidate final results"
        | otherwise -> do
            _ <- execute conn
              "DELETE FROM insights_account_snapshots WHERE competition_slug = ?"
              (Only slug)
            _ <- execute conn
              "DELETE FROM insights_snapshot_batches WHERE competition_slug = ?"
              (Only slug)
            _ <- execute conn
              "UPDATE insights_competitions\
              \ SET account_lens_address = ?, updated_at = NOW() WHERE slug = ?"
              (normalizedAddress, slug)
            pure ()
      _ -> ioError $ userError $
        "Plether Insights could not uniquely identify competition " <> T.unpack slug
  where
    normalizedAddress = normalizeAddress accountLensAddress

seedJuly2026Competition :: Connection -> Integer -> Text -> Text -> Text -> Text -> IO ()
seedJuly2026Competition conn chainId releaseRouter usdcAddress marginClearinghouseAddress accountLensAddress =
  withTransaction conn $ do
    let expected =
          competitionSeedMetadataFor
            july2026Competition
            chainId
            releaseRouter
            usdcAddress
            marginClearinghouseAddress
    _ <- execute conn
      "INSERT INTO insights_competitions (\
      \ slug, name, chain_id, release_router, usdc_address, margin_clearinghouse_address, account_lens_address,\
      \ start_timestamp, new_risk_cutoff_timestamp, score_cutoff_timestamp,\
      \ results_timestamp, payment_deadline_timestamp, starting_balance_usdc,\
      \ minimum_profit_bps, minimum_active_days, scoring_version, rules_version,\
      \ first_prize_usdc, second_prize_usdc, third_prize_usdc)\
      \ VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)\
      \ ON CONFLICT (slug) DO NOTHING"
      ( csmSlug expected
      , csmName expected
      , csmChainId expected
      , csmReleaseRouter expected
      , csmUsdcAddress expected
      , csmMarginClearinghouseAddress expected
      , normalizeAddress accountLensAddress
      , csmStartTimestamp expected
      , csmNewRiskCutoffTimestamp expected
      , csmScoreCutoffTimestamp expected
      , csmResultsTimestamp expected
      , csmPaymentDeadlineTimestamp expected
      , csmStartingBalanceUsdc expected
      , csmMinimumProfitBps expected
      , csmMinimumActiveDays expected
      , csmScoringVersion expected
      , csmRulesVersion expected
      , csmFirstPrizeUsdc expected
      , csmSecondPrizeUsdc expected
      , csmThirdPrizeUsdc expected
      )
    storedRows <- query conn
      (competitionSeedMetadataSelect <> " WHERE slug = ? FOR UPDATE")
      (Only $ csmSlug expected)
    case storedRows of
      [stored] -> validateOrMigrateCompetitionSeed conn expected stored
      _ -> ioError $ userError $
        "Plether Insights could not read the competition row immediately after seeding slug "
          <> T.unpack (csmSlug expected)
          <> ". Check database constraints and transaction logs."

competitionSeedMetadataFor
  :: CompetitionRules
  -> Integer
  -> Text
  -> Text
  -> Text
  -> CompetitionSeedMetadata
competitionSeedMetadataFor rules chainId releaseRouter usdcAddress marginClearinghouseAddress =
  CompetitionSeedMetadata
    { csmSlug = crSlug rules
    , csmName = crName rules
    , csmChainId = chainId
    , csmReleaseRouter = normalizeAddress releaseRouter
    , csmUsdcAddress = normalizeAddress usdcAddress
    , csmMarginClearinghouseAddress = normalizeAddress marginClearinghouseAddress
    , csmStartTimestamp = epoch $ crStartAt rules
    , csmNewRiskCutoffTimestamp = epoch $ crNewRiskCutoffAt rules
    , csmScoreCutoffTimestamp = epoch $ crScoreCutoffAt rules
    , csmResultsTimestamp = epoch $ crResultsAt rules
    , csmPaymentDeadlineTimestamp = epoch $ crPaymentDeadlineAt rules
    , csmStartingBalanceUsdc = crStartingBalanceUsdc rules
    , csmMinimumProfitBps = crMinimumProfitBps rules
    , csmMinimumActiveDays = crMinimumActiveDays rules
    , csmScoringVersion = crScoringVersion rules
    , csmRulesVersion = crRulesVersion rules
    , csmFirstPrizeUsdc = prizeAt 0 rules
    , csmSecondPrizeUsdc = prizeAt 1 rules
    , csmThirdPrizeUsdc = prizeAt 2 rules
    }

competitionSeedMismatches
  :: CompetitionSeedMetadata
  -> CompetitionSeedMetadata
  -> [CompetitionSeedMismatch]
competitionSeedMismatches expected stored = concat
  [ mismatch "slug" csmSlug
  , mismatch "name" csmName
  , mismatchShow "chain_id" csmChainId
  , mismatch "release_router" csmReleaseRouter
  , mismatch "usdc_address" csmUsdcAddress
  , mismatch "margin_clearinghouse_address" csmMarginClearinghouseAddress
  , mismatchShow "start_timestamp" csmStartTimestamp
  , mismatchShow "new_risk_cutoff_timestamp" csmNewRiskCutoffTimestamp
  , mismatchShow "score_cutoff_timestamp" csmScoreCutoffTimestamp
  , mismatchShow "results_timestamp" csmResultsTimestamp
  , mismatchShow "payment_deadline_timestamp" csmPaymentDeadlineTimestamp
  , mismatchShow "starting_balance_usdc" csmStartingBalanceUsdc
  , mismatchShow "minimum_profit_bps" csmMinimumProfitBps
  , mismatchShow "minimum_active_days" csmMinimumActiveDays
  , mismatch "scoring_version" csmScoringVersion
  , mismatch "rules_version" csmRulesVersion
  , mismatchShow "first_prize_usdc" csmFirstPrizeUsdc
  , mismatchShow "second_prize_usdc" csmSecondPrizeUsdc
  , mismatchShow "third_prize_usdc" csmThirdPrizeUsdc
  ]
  where
    mismatch fieldName getter
      | getter expected == getter stored = []
      | otherwise = [CompetitionSeedMismatch fieldName (getter stored) (getter expected)]
    mismatchShow fieldName getter
      | getter expected == getter stored = []
      | otherwise =
          [ CompetitionSeedMismatch
              fieldName
              (T.pack $ show $ getter stored)
              (T.pack $ show $ getter expected)
          ]

-- This is the single payout timestamp written by the pre-launch development
-- seed before the "within one week of close" deadline was corrected. It may be
-- migrated only when the database has no resolved boundaries or snapshots.
legacyPaymentDeadlineTimestamp :: Integer
legacyPaymentDeadlineTimestamp = 1_786_319_999 -- 2026-08-09T23:59:59Z

isLegacyPaymentDeadlineOnlyMismatch
  :: CompetitionSeedMetadata
  -> CompetitionSeedMetadata
  -> Bool
isLegacyPaymentDeadlineOnlyMismatch expected stored =
  csmPaymentDeadlineTimestamp stored == legacyPaymentDeadlineTimestamp
    && competitionSeedMismatches expected stored
      == [ CompetitionSeedMismatch
             "payment_deadline_timestamp"
             (T.pack $ show legacyPaymentDeadlineTimestamp)
             (T.pack $ show $ csmPaymentDeadlineTimestamp expected)
         ]

validateOrMigrateCompetitionSeed
  :: Connection
  -> CompetitionSeedMetadata
  -> CompetitionSeedMetadata
  -> IO ()
validateOrMigrateCompetitionSeed conn expected stored =
  case competitionSeedMismatches expected stored of
    [] -> pure ()
    mismatches
      | isLegacyPaymentDeadlineOnlyMismatch expected stored -> do
          safeRows <- query conn
            "SELECT start_block IS NULL AND score_cutoff_block IS NULL AND NOT finalized\
            \ AND NOT EXISTS (SELECT 1 FROM insights_account_snapshots WHERE competition_slug = ?)\
            \ FROM insights_competitions WHERE slug = ?"
            (csmSlug expected, csmSlug expected)
          case safeRows of
            [Only True] -> do
              _ <- execute conn
                "UPDATE insights_competitions SET payment_deadline_timestamp = ?, updated_at = NOW()\
                \ WHERE slug = ?"
                (csmPaymentDeadlineTimestamp expected, csmSlug expected)
              putStrLn $
                "Migrated the pre-launch Plether Insights payout deadline for "
                  <> T.unpack (csmSlug expected)
                  <> " from Unix timestamp "
                  <> show legacyPaymentDeadlineTimestamp
                  <> " to "
                  <> show (csmPaymentDeadlineTimestamp expected)
                  <> "."
            _ -> seedMismatchError expected mismatches $
              Just "The known pre-launch payout-deadline correction was detected, but automatic migration is allowed only before boundary blocks, snapshots, or finalization exist."
      | otherwise -> seedMismatchError expected mismatches Nothing

seedMismatchError
  :: CompetitionSeedMetadata
  -> [CompetitionSeedMismatch]
  -> Maybe String
  -> IO a
seedMismatchError expected mismatches extra = ioError $ userError $ unlines $
  [ "Plether Insights competition metadata mismatch for slug " <> T.unpack (csmSlug expected) <> "."
  , "Stored identity and scoring metadata is immutable; startup refused to reinterpret existing competition history."
  ]
    <> maybe [] pure extra
    <> ("Mismatched fields:" : map renderMismatch mismatches)
    <> [ "Restore the deployment config/code that originally seeded this slug, or create a deliberately versioned competition with a new slug."
       , "For a disposable pre-launch database only, remove the competition data explicitly and restart; never delete a live competition to bypass this check."
       ]
  where
    renderMismatch CompetitionSeedMismatch {..} =
      "  - "
        <> T.unpack csmmField
        <> ": stored="
        <> T.unpack csmmStored
        <> ", expected="
        <> T.unpack csmmExpected

setCompetitionBoundaryBlocks
  :: Connection
  -> Text
  -> Maybe (Integer, Text)
  -> Maybe (Integer, Text)
  -> IO ()
setCompetitionBoundaryBlocks conn slug startBlock cutoffBlock = do
  let startBlockNumber = fst <$> startBlock
      startBlockHash = normalizeAddress . snd <$> startBlock
      cutoffBlockNumber = fst <$> cutoffBlock
      cutoffBlockHash = normalizeAddress . snd <$> cutoffBlock
  _ <- execute conn
    "UPDATE insights_competitions SET\
    \ start_block = COALESCE(start_block, ?),\
    \ start_block_hash = CASE WHEN ? IS NULL THEN start_block_hash\
    \   WHEN start_block IS NULL OR start_block = ? THEN ? ELSE start_block_hash END,\
    \ score_cutoff_block = COALESCE(score_cutoff_block, ?),\
    \ score_cutoff_block_hash = CASE WHEN ? IS NULL THEN score_cutoff_block_hash\
    \   WHEN score_cutoff_block IS NULL OR score_cutoff_block = ? THEN ? ELSE score_cutoff_block_hash END,\
    \ updated_at = NOW() WHERE slug = ? AND finalized = FALSE"
    ( startBlockNumber
    , startBlockHash
    , startBlockNumber
    , startBlockHash
    , cutoffBlockNumber
    , cutoffBlockHash
    , cutoffBlockNumber
    , cutoffBlockHash
    , slug
    )
  pure ()

upsertCompetitionParticipant
  :: Connection
  -> Text
  -> Text
  -> Text
  -> Maybe Text
  -> IO (Either Text ())
upsertCompetitionParticipant conn slug traderReference wallet alias =
  withTransaction conn $ do
    let normalizedWallet = normalizeAddress wallet
        normalizedReference = T.strip traderReference
    competitions <- query conn
      "SELECT finalized FROM insights_competitions WHERE slug = ? FOR UPDATE"
      (Only slug)
    case competitions of
      [] -> pure $ Left $ "Unknown Insights competition: " <> slug
      [Only True] -> pure $ Left "The competition is already finalized; registrations are locked"
      [Only False]
        | T.null normalizedReference ->
            pure $ Left "TRADER_REFERENCE must be a non-empty opaque registration identifier"
        | otherwise -> do
            referenceOwners <- query conn
              "SELECT wallet FROM insights_competition_participants\
              \ WHERE competition_slug = ? AND trader_reference = ? AND wallet <> ?\
              \ FOR UPDATE"
              (slug, normalizedReference, normalizedWallet)
            walletReferences <- query conn
              "SELECT trader_reference FROM insights_competition_participants\
              \ WHERE competition_slug = ? AND wallet = ? FOR UPDATE"
              (slug, normalizedWallet)
            case (referenceOwners, walletReferences) of
              (Only owner : _, _) ->
                pure $ Left $
                  "TRADER_REFERENCE is already assigned to trading account " <> owner
              (_, [Only (Just existingReference)])
                | existingReference /= normalizedReference ->
                    pure $ Left "This trading account is already assigned to another TRADER_REFERENCE"
              _ -> do
                _ <- execute conn
                  "INSERT INTO insights_competition_participants\
                  \ (competition_slug, wallet, trader_reference, alias)\
                  \ VALUES (?, ?, ?, ?)\
                  \ ON CONFLICT (competition_slug, wallet) DO UPDATE SET\
                  \ trader_reference = EXCLUDED.trader_reference, alias = EXCLUDED.alias, updated_at = NOW()"
                  (slug, normalizedWallet, normalizedReference, normalizeAlias alias)
                pure $ Right ()
      _ -> pure $ Left "Competition registration state is ambiguous"

stageCompetitionParticipantWalletRemap
  :: Connection
  -> Text
  -> Text
  -> Text
  -> Text
  -> IO (Either Text ())
stageCompetitionParticipantWalletRemap conn slug traderReference oldWallet newWallet =
  withTransaction conn $ do
    let normalizedReference = T.strip traderReference
        normalizedOldWallet = normalizeAddress oldWallet
        normalizedNewWallet = normalizeAddress newWallet
    mutable <- competitionIsMutableForUpdate conn slug
    if not mutable
      then pure $ Left "The competition is missing or finalized; wallet remaps are locked"
      else if T.null normalizedReference
        then pure $ Left "TRADER_REFERENCE must be a non-empty opaque registration identifier"
        else do
          participants <- query conn
            "SELECT wallet FROM insights_competition_participants\
            \ WHERE competition_slug = ? AND trader_reference = ? FOR UPDATE"
            (slug, normalizedReference)
          destinationOwners <- query conn
            "SELECT trader_reference FROM insights_participant_wallet_remaps\
            \ WHERE competition_slug = ? AND new_wallet = ? AND trader_reference <> ?\
            \ FOR UPDATE"
            (slug, normalizedNewWallet, normalizedReference)
          case (participants, destinationOwners) of
            ([], _) -> pure $ Left "TRADER_REFERENCE is not registered for this competition"
            ([Only currentWallet], _)
              | normalizeAddress currentWallet /= normalizedOldWallet ->
                  pure $ Left "The registered wallet does not match OLD_WALLET"
            ([_], Only owner : _) ->
              pure $ Left $ "NEW_WALLET is already staged for TRADER_REFERENCE " <> owner
            ([_], []) -> do
              _ <- execute conn
                "INSERT INTO insights_participant_wallet_remaps\
                \ (competition_slug, trader_reference, old_wallet, new_wallet)\
                \ VALUES (?, ?, ?, ?)\
                \ ON CONFLICT (competition_slug, trader_reference) DO UPDATE SET\
                \ old_wallet = EXCLUDED.old_wallet, new_wallet = EXCLUDED.new_wallet,\
                \ staged_at = NOW(), applied_at = NULL, applied_by = NULL"
                (slug, normalizedReference, normalizedOldWallet, normalizedNewWallet)
              pure $ Right ()
            _ -> pure $ Left "Participant wallet remap state is ambiguous"

applyCompetitionParticipantWalletRemaps
  :: Connection
  -> Text
  -> Integer
  -> Text
  -> IO (Either Text ())
applyCompetitionParticipantWalletRemaps conn slug expectedCount appliedBy =
  withTransaction conn $ do
    let normalizedAppliedBy = T.strip appliedBy
    mutable <- competitionIsMutableForUpdate conn slug
    if not mutable
      then pure $ Left "The competition is missing or finalized; wallet remaps are locked"
      else if expectedCount <= 0
        then pure $ Left "EXPECTED_COUNT must be positive"
        else if T.null normalizedAppliedBy
          then pure $ Left "APPLIED_BY must not be empty"
          else do
            counts <- query conn
              "SELECT\
              \ (SELECT COUNT(*) FROM insights_competition_participants WHERE competition_slug = ?),\
              \ (SELECT COUNT(*) FROM insights_participant_wallet_remaps WHERE competition_slug = ? AND applied_at IS NULL),\
              \ (SELECT COUNT(*) FROM insights_competition_participants p\
              \   JOIN insights_participant_wallet_remaps m\
              \   ON m.competition_slug = p.competition_slug AND m.trader_reference = p.trader_reference\
              \   AND m.old_wallet = p.wallet\
              \   WHERE p.competition_slug = ? AND m.applied_at IS NULL),\
              \ (SELECT COUNT(DISTINCT new_wallet) FROM insights_participant_wallet_remaps\
              \   WHERE competition_slug = ? AND applied_at IS NULL),\
              \ (SELECT COUNT(*) FROM insights_manual_adjustments WHERE competition_slug = ?),\
              \ (SELECT COUNT(*) FROM insights_eligibility_audit WHERE competition_slug = ?)"
              (slug, slug, slug, slug, slug, slug)
              :: IO [(Integer, Integer, Integer, Integer, Integer, Integer)]
            case counts of
              [(participantCount, remapCount, matchingCount, destinationCount, adjustmentCount, auditCount)]
                | participantCount /= expectedCount ->
                    pure $ Left "EXPECTED_COUNT does not match the registered participant count"
                | remapCount /= expectedCount ->
                    pure $ Left "The staged wallet remap set is incomplete"
                | matchingCount /= expectedCount ->
                    pure $ Left "One or more staged remaps do not match the registered roster"
                | destinationCount /= expectedCount ->
                    pure $ Left "The staged destination wallet set contains duplicates"
                | adjustmentCount /= 0 ->
                    pure $ Left "Wallet remapping is blocked after manual adjustments exist"
                | auditCount /= 0 ->
                    pure $ Left "Wallet remapping is blocked after eligibility review has started"
                | otherwise -> do
                    _ <- execute conn
                      "CREATE TEMP TABLE insights_roster_replacement ON COMMIT DROP AS\
                      \ SELECT p.competition_slug, m.new_wallet AS wallet, p.trader_reference, p.alias,\
                      \ p.eligibility_status, p.eligibility_reason, p.integrity_flags, p.registered_at,\
                      \ p.reviewed_at, p.created_at, NOW() AS updated_at\
                      \ FROM insights_competition_participants p\
                      \ JOIN insights_participant_wallet_remaps m\
                      \ ON m.competition_slug = p.competition_slug AND m.trader_reference = p.trader_reference\
                      \ AND m.old_wallet = p.wallet\
                      \ WHERE p.competition_slug = ? AND m.applied_at IS NULL"
                      (Only slug)
                    _ <- execute conn
                      "DELETE FROM insights_account_snapshots WHERE competition_slug = ?"
                      (Only slug)
                    _ <- execute conn
                      "DELETE FROM insights_snapshot_batches WHERE competition_slug = ?"
                      (Only slug)
                    _ <- execute conn
                      "DELETE FROM insights_competition_participants WHERE competition_slug = ?"
                      (Only slug)
                    _ <- execute_ conn
                      "INSERT INTO insights_competition_participants\
                      \ (competition_slug, wallet, trader_reference, alias, eligibility_status, eligibility_reason,\
                      \ integrity_flags, registered_at, reviewed_at, created_at, updated_at)\
                      \ SELECT competition_slug, wallet, trader_reference, alias, eligibility_status, eligibility_reason,\
                      \ integrity_flags, registered_at, reviewed_at, created_at, updated_at\
                      \ FROM insights_roster_replacement"
                    _ <- execute conn
                      "UPDATE insights_participant_wallet_remaps SET applied_at = NOW(), applied_by = ?\
                      \ WHERE competition_slug = ? AND applied_at IS NULL"
                      (normalizedAppliedBy, slug)
                    _ <- execute conn
                      "UPDATE insights_competitions SET updated_at = NOW() WHERE slug = ?"
                      (Only slug)
                    pure $ Right ()
              _ -> pure $ Left "Participant wallet remap validation state is ambiguous"

setParticipantEligibility
  :: Connection
  -> Text
  -> Text
  -> ParticipantEligibility
  -> Maybe Text
  -> Text
  -> IO Bool
setParticipantEligibility conn slug wallet status reason reviewedBy = do
  withTransaction conn $ do
    mutable <- competitionIsMutableForUpdate conn slug
    if not mutable
      then pure False
      else do
        previous <- query conn
          "SELECT eligibility_status FROM insights_competition_participants\
          \ WHERE competition_slug = ? AND wallet = ? FOR UPDATE"
          (slug, normalizeAddress wallet)
        case previous of
          [Only (previousStatus :: Text)] -> do
            let newStatus = participantEligibilityText status
            _ <- execute conn
              "UPDATE insights_competition_participants SET eligibility_status = ?,\
              \ eligibility_reason = ?, reviewed_at = NOW(), updated_at = NOW()\
              \ WHERE competition_slug = ? AND wallet = ?"
              (newStatus, reason, slug, normalizeAddress wallet)
            _ <- execute conn
              "INSERT INTO insights_eligibility_audit\
              \ (competition_slug, wallet, previous_status, new_status, reason, reviewed_by)\
              \ VALUES (?, ?, ?, ?, ?, ?)"
              (slug, normalizeAddress wallet, previousStatus, newStatus, reason, reviewedBy)
            pure True
          _ -> pure False

finalizeCompetition :: Connection -> Text -> Text -> IO (Either Text ())
finalizeCompetition _ _ finalizedBy
  | T.null (T.strip finalizedBy) = pure $ Left "REVIEWER must not be empty"
finalizeCompetition conn slug finalizedBy =
  withTransaction conn $ do
    rows <- query conn finalizationReadinessQuery (Only slug)
    now <- round <$> getPOSIXTime
    case rows of
      [] -> pure $ Left $ "unknown competition: " <> slug
      [FinalizationDatabaseRow {..}]
        | fdrAlreadyFinalized -> pure $ Left "standings are already finalized"
        | otherwise -> do
            let readiness =
                  FinalizationReadiness
                    { frNowTimestamp = now
                    , frScoreCutoffTimestamp = fdrScoreCutoffTimestamp
                    , frResultsTimestamp = fdrResultsTimestamp
                    , frStartBlock = fdrStartBlock
                    , frScoreCutoffBlock = fdrScoreCutoffBlock
                    , frParticipantCount = fdrParticipantCount
                    , frMissingTraderReferences = fdrMissingTraderReferences
                    , frUnresolvedReviews = fdrUnresolvedReviews
                    , frStartSnapshotCount = fdrStartSnapshotCount
                    , frFinalSnapshotCount = fdrFinalSnapshotCount
                    , frFinalSnapshotHashCount = fdrFinalSnapshotHashCount
                    }
            case finalizationBlockers readiness of
              blockers@(_ : _) -> pure $ Left $ T.intercalate "; " blockers
              [] -> case (fdrScoreCutoffBlock, fdrFinalSnapshotHash) of
                (Just finalBlock, Just finalHash) -> do
                  affected <- execute conn
                    "UPDATE insights_competitions SET finalized = TRUE, updated_at = NOW()\
                    \ WHERE slug = ? AND finalized = FALSE"
                    (Only slug)
                  if affected /= 1
                    then pure $ Left "standings changed while finalization was in progress; retry"
                    else do
                      _ <- execute conn
                        "INSERT INTO insights_competition_finalization_audit\
                        \ (competition_slug, finalized_by, participant_count, final_snapshot_block, final_snapshot_hash)\
                        \ VALUES (?, ?, ?, ?, ?)"
                        (slug, T.strip finalizedBy, fdrParticipantCount, finalBlock, finalHash)
                      pure $ Right ()
                _ -> pure $ Left "canonical final snapshot identity is unavailable"
      _ -> pure $ Left "competition finalization state is ambiguous"

listCompetitionParticipants :: Connection -> Text -> IO [ParticipantRow]
listCompetitionParticipants conn slug =
  query conn participantSelect (Only slug)

type AccountSnapshotParameters =
  ( Text
  , Text
  , Text
  , Integer
  , Text
  , Integer
  , Text
  , Integer
  , Bool
  , Integer
  , Integer
  , Integer
  , LBS.ByteString
  )

accountSnapshotParameters :: AccountSnapshotInput -> AccountSnapshotParameters
accountSnapshotParameters AccountSnapshotInput {..} =
  let EquitySnapshot {..} = asiEquity
   in ( asiCompetitionSlug
      , normalizeAddress asiWallet
      , snapshotKindText asiKind
      , asiChainId
      , normalizeAddress asiReleaseRouter
      , asiBlockNumber
      , normalizeAddress asiBlockHash
      , asiTimestamp
      , esHasOpenPosition
      , esSignedNetEquityUsdc
      , esTerminalReachableUsdc
      , esTraderClaimsUsdc
      , encode asiRawData
      )

accountSnapshotUpsertQuery :: Query
accountSnapshotUpsertQuery =
  "INSERT INTO insights_account_snapshots (\
  \ competition_slug, wallet, snapshot_kind, chain_id, release_router, block_number,\
  \ block_hash, timestamp, has_open_position, signed_net_equity_usdc,\
  \ terminal_reachable_usdc, trader_claims_usdc, raw_data)\
  \ VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)\
  \ ON CONFLICT (competition_slug, wallet, snapshot_kind, block_number) DO UPDATE SET\
  \ chain_id = EXCLUDED.chain_id,\
  \ release_router = EXCLUDED.release_router, block_hash = EXCLUDED.block_hash,\
  \ timestamp = EXCLUDED.timestamp, has_open_position = EXCLUDED.has_open_position,\
  \ signed_net_equity_usdc = EXCLUDED.signed_net_equity_usdc,\
  \ terminal_reachable_usdc = EXCLUDED.terminal_reachable_usdc,\
  \ trader_claims_usdc = EXCLUDED.trader_claims_usdc, raw_data = EXCLUDED.raw_data,\
  \ updated_at = NOW()"

upsertAccountSnapshotsUnchecked :: Connection -> [AccountSnapshotInput] -> IO ()
upsertAccountSnapshotsUnchecked _ [] = pure ()
upsertAccountSnapshotsUnchecked conn snapshots = do
  _ <- executeMany conn accountSnapshotUpsertQuery $ map accountSnapshotParameters snapshots
  pure ()

publishAccountSnapshotBatch :: Connection -> [AccountSnapshotInput] -> IO ()
publishAccountSnapshotBatch _ [] = pure ()
publishAccountSnapshotBatch conn snapshots@(firstSnapshot : _) =
  withTransaction conn $ do
    let slug = asiCompetitionSlug firstSnapshot
        kind = asiKind firstSnapshot
        chainId = asiChainId firstSnapshot
        releaseRouter = normalizeAddress $ asiReleaseRouter firstSnapshot
        accountLensAddress = normalizeAddress $ asiAccountLensAddress firstSnapshot
        blockNumber = asiBlockNumber firstSnapshot
        blockHash = normalizeAddress $ asiBlockHash firstSnapshot
        timestamp = asiTimestamp firstSnapshot
        accountStateCount = length $ filter (equityHasAccountState . asiEquity) snapshots
        sameIdentity snapshot =
          asiCompetitionSlug snapshot == slug
            && asiKind snapshot == kind
            && asiChainId snapshot == chainId
            && normalizeAddress (asiReleaseRouter snapshot) == releaseRouter
            && normalizeAddress (asiAccountLensAddress snapshot) == accountLensAddress
            && asiBlockNumber snapshot == blockNumber
            && normalizeAddress (asiBlockHash snapshot) == blockHash
            && asiTimestamp snapshot == timestamp
        inputWallets = sort $ map (normalizeAddress . asiWallet) snapshots
    unless (all sameIdentity snapshots) $
      fail "Cannot publish a mixed-block or mixed-competition Insights snapshot batch"
    unless (length inputWallets == length (nub inputWallets)) $
      fail "Cannot publish an Insights snapshot batch with duplicate wallets"
    mutable <- competitionIsMutableForUpdate conn slug
    unless mutable $
      fail "Cannot publish an account snapshot batch: the competition is missing or finalized"
    configuredLens <- query conn
      "SELECT account_lens_address FROM insights_competitions WHERE slug = ?"
      (Only slug)
    unless (configuredLens == [Only accountLensAddress]) $
      fail "Cannot publish an Insights snapshot batch from a stale account lens"
    previousStatefulBatches <- query conn
      "SELECT EXISTS (SELECT 1 FROM insights_snapshot_batches\
      \ WHERE competition_slug = ? AND snapshot_kind IN ('live', 'final')\
      \ AND LOWER(account_lens_address) = LOWER(?) AND account_state_count > 0)"
      (slug, accountLensAddress)
    when
      ( kind /= SnapshotStart
          && accountStateCount == 0
          && previousStatefulBatches == [Only True]
      ) $
      fail "Cannot publish an all-zero Insights snapshot after a stateful live batch"
    registered <- query conn
      "SELECT wallet FROM insights_competition_participants\
      \ WHERE competition_slug = ? ORDER BY wallet ASC"
      (Only slug)
    let registeredWallets = [wallet | Only wallet <- registered]
    unless (inputWallets == registeredWallets) $
      fail "Cannot publish an incomplete Insights snapshot batch: registered participant set changed"
    _ <- execute conn
      "DELETE FROM insights_account_snapshots\
      \ WHERE competition_slug = ? AND snapshot_kind = ? AND block_number = ?"
      (slug, snapshotKindText kind, blockNumber)
    upsertAccountSnapshotsUnchecked conn snapshots
    _ <- execute conn
      "INSERT INTO insights_snapshot_batches\
      \ (competition_slug, snapshot_kind, chain_id, release_router, account_lens_address, block_number, block_hash, timestamp, participant_count, account_state_count)\
      \ VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)\
      \ ON CONFLICT (competition_slug, snapshot_kind, block_number) DO UPDATE SET\
      \ chain_id = EXCLUDED.chain_id, release_router = EXCLUDED.release_router,\
      \ account_lens_address = EXCLUDED.account_lens_address,\
      \ block_hash = EXCLUDED.block_hash, timestamp = EXCLUDED.timestamp,\
      \ participant_count = EXCLUDED.participant_count,\
      \ account_state_count = EXCLUDED.account_state_count, published_at = NOW()"
      ( slug
      , snapshotKindText kind
      , chainId
      , releaseRouter
      , accountLensAddress
      , blockNumber
      , blockHash
      , timestamp
      , length snapshots
      , accountStateCount
      )
    pure ()

equityHasAccountState :: EquitySnapshot -> Bool
equityHasAccountState EquitySnapshot {..} =
  esHasOpenPosition
    || esSignedNetEquityUsdc /= 0
    || esTerminalReachableUsdc /= 0
    || esTraderClaimsUsdc /= 0

hasCompleteAccountSnapshotBatch
  :: Connection
  -> Text
  -> SnapshotKind
  -> Integer
  -> Text
  -> IO Bool
hasCompleteAccountSnapshotBatch conn slug kind blockNumber blockHash = do
  rows <- query conn
    "SELECT EXISTS (\
    \ SELECT 1 FROM insights_snapshot_batches b\
    \ JOIN insights_competitions c ON c.slug = b.competition_slug\
    \ WHERE b.competition_slug = ? AND b.snapshot_kind = ? AND b.block_number = ?\
    \ AND LOWER(b.block_hash) = LOWER(?) AND b.participant_count > 0\
    \ AND b.chain_id = c.chain_id AND b.release_router = c.release_router\
    \ AND LOWER(b.account_lens_address) = LOWER(c.account_lens_address)\
    \ AND (b.snapshot_kind = 'start' OR b.account_state_count > 0 OR NOT EXISTS (\
    \   SELECT 1 FROM insights_snapshot_batches prior\
    \   WHERE prior.competition_slug = b.competition_slug\
    \   AND prior.snapshot_kind IN ('live', 'final')\
    \   AND LOWER(prior.account_lens_address) = LOWER(c.account_lens_address)\
    \   AND prior.account_state_count > 0))\
    \ AND b.participant_count = (\
    \   SELECT COUNT(*) FROM insights_competition_participants p WHERE p.competition_slug = b.competition_slug\
    \ )\
    \ AND b.participant_count = (\
    \   SELECT COUNT(DISTINCT s.wallet) FROM insights_account_snapshots s\
    \   JOIN insights_competition_participants p ON p.competition_slug = s.competition_slug AND p.wallet = s.wallet\
    \   WHERE s.competition_slug = b.competition_slug AND s.snapshot_kind = b.snapshot_kind\
    \   AND s.block_number = b.block_number AND LOWER(s.block_hash) = LOWER(b.block_hash)\
    \   AND s.chain_id = b.chain_id AND s.release_router = b.release_router\
    \ ))"
    (slug, snapshotKindText kind, blockNumber, normalizeAddress blockHash)
  pure $ case rows of
    [Only found] -> found
    _ -> False

invalidateSnapshotBatchesAfter :: Connection -> Text -> Integer -> IO ()
invalidateSnapshotBatchesAfter conn slug safeBlock =
  withTransaction conn $ do
    mutable <- competitionIsMutableForUpdate conn slug
    when mutable $ do
      _ <- execute conn
        "DELETE FROM insights_account_snapshots\
        \ WHERE competition_slug = ? AND block_number > ?"
        (slug, safeBlock)
      _ <- execute conn
        "DELETE FROM insights_snapshot_batches\
        \ WHERE competition_slug = ? AND block_number > ?"
        (slug, safeBlock)
      _ <- execute conn
        "UPDATE insights_competitions SET\
        \ start_block = CASE WHEN start_block > ? THEN NULL ELSE start_block END,\
        \ start_block_hash = CASE WHEN start_block > ? THEN NULL ELSE start_block_hash END,\
        \ score_cutoff_block = CASE WHEN score_cutoff_block > ? THEN NULL ELSE score_cutoff_block END,\
        \ score_cutoff_block_hash = CASE WHEN score_cutoff_block > ? THEN NULL ELSE score_cutoff_block_hash END,\
        \ updated_at = CASE WHEN start_block > ? OR score_cutoff_block > ? THEN NOW() ELSE updated_at END\
        \ WHERE slug = ?"
        (safeBlock, safeBlock, safeBlock, safeBlock, safeBlock, safeBlock, slug)
      pure ()

competitionIsMutableForUpdate :: Connection -> Text -> IO Bool
competitionIsMutableForUpdate conn slug = do
  rows <- query conn
    "SELECT NOT finalized FROM insights_competitions WHERE slug = ? FOR UPDATE"
    (Only slug)
  pure $ case rows of
    [Only mutable] -> mutable
    _ -> False

insertManualAdjustment
  :: Connection
  -> Text
  -> Text
  -> Integer
  -> Text
  -> Text
  -> IO (Maybe Integer)
insertManualAdjustment conn slug wallet amountUsdc reason createdBy = do
  withTransaction conn $ do
    mutable <- competitionIsMutableForUpdate conn slug
    if not mutable
      then pure Nothing
      else do
        rows <- query conn
          "INSERT INTO insights_manual_adjustments\
          \ (competition_slug, wallet, amount_usdc, reason, created_by)\
          \ VALUES (?, ?, ?, ?, ?) RETURNING id"
          (slug, normalizeAddress wallet, amountUsdc, reason, createdBy)
        pure $ case rows of
          [Only adjustmentId] -> Just adjustmentId
          _ -> Nothing

voidManualAdjustment :: Connection -> Integer -> Text -> Text -> IO Bool
voidManualAdjustment conn adjustmentId voidedBy reason = do
  withTransaction conn $ do
    competitions <- query conn
      "SELECT c.slug FROM insights_manual_adjustments m\
      \ JOIN insights_competitions c ON c.slug = m.competition_slug\
      \ WHERE m.id = ? AND m.voided_at IS NULL AND c.finalized = FALSE\
      \ FOR UPDATE OF c, m"
      (Only adjustmentId)
    case competitions of
      [Only (_ :: Text)] -> do
        affected <- execute conn
          "UPDATE insights_manual_adjustments SET voided_at = NOW(), voided_by = ?, void_reason = ?\
          \ WHERE id = ? AND voided_at IS NULL"
          (voidedBy, reason, adjustmentId)
        pure $ affected > (0 :: Int64)
      _ -> pure False

getCurrentCompetition :: Connection -> IO (Maybe CompetitionRow)
getCurrentCompetition conn = do
  rows <- query_ conn $
    competitionSelect
      <> " ORDER BY CASE WHEN results_timestamp >= EXTRACT(EPOCH FROM NOW())::bigint THEN 0 ELSE 1 END,\
         \ ABS(results_timestamp - EXTRACT(EPOCH FROM NOW())::bigint) ASC LIMIT 1"
  pure $ firstRow rows

getCompetitionBySlug :: Connection -> Text -> IO (Maybe CompetitionRow)
getCompetitionBySlug conn slug = do
  rows <- query conn (competitionSelect <> " WHERE slug = ? LIMIT 1") (Only slug)
  pure $ firstRow rows

getCompetitionLeaderboard
  :: Connection
  -> Text
  -> Maybe Text
  -> Int
  -> Int
  -> IO [LeaderboardRow]
getCompetitionLeaderboard conn slug requestedSearch limitRows offsetRows =
  case normalizeLeaderboardSearch requestedSearch of
    Nothing ->
      query conn
        (leaderboardQuery <> " ORDER BY final_pnl_usdc DESC NULLS LAST, wallet ASC LIMIT ? OFFSET ?")
        (slug, limitRows, offsetRows)
    Just search ->
      let pattern = leaderboardSearchPattern search
       in query conn
            (leaderboardQuery
              <> " WHERE wallet ILIKE ? ESCAPE '!' OR COALESCE(alias, '') ILIKE ? ESCAPE '!'\
                 \ ORDER BY final_pnl_usdc DESC NULLS LAST, wallet ASC LIMIT ? OFFSET ?")
            (slug, pattern, pattern, limitRows, offsetRows)

getCompetitionWallet :: Connection -> Text -> Text -> IO (Maybe LeaderboardRow)
getCompetitionWallet conn slug wallet = do
  rows <- query conn
    (leaderboardQuery <> " WHERE wallet = ? LIMIT 1")
    (slug, normalizeAddress wallet)
  pure $ firstRow rows

getCompetitionWalletActivity
  :: Connection
  -> Text
  -> Text
  -> Int
  -> IO [InsightsActivityRow]
getCompetitionWalletActivity conn slug wallet limitRows =
  query conn
    walletActivityQuery
    (slug, normalizeAddress wallet, limitRows)

getInsightsDataStatus :: Connection -> Text -> IO (Maybe InsightsDataStatusRow)
getInsightsDataStatus conn slug = do
  rows <- query conn insightsDataStatusQuerySql (Only slug)
  pure $ firstRow rows

-- A published batch is the completeness marker for its participant set. The
-- maximum published count also preserves the old across-history wallet-count
-- behavior when the roster grows, without revisiting every account row.
-- Snapshot rows have no independent mutation path: publication writes every
-- registered wallet and its batch metadata in one transaction, while each
-- invalidation path deletes both. Status can therefore use the small batch
-- table as the durable completeness summary instead of rescanning history.
insightsDataStatusQuerySql :: Query
insightsDataStatusQuerySql =
  "WITH target AS (SELECT * FROM insights_competitions WHERE slug = ?),\
  \ participant_stats AS (\
  \ SELECT COUNT(*) AS participant_count FROM insights_competition_participants p\
  \ JOIN target t ON t.slug = p.competition_slug\
  \ ), snapshot_stats AS (\
  \ SELECT COALESCE(MAX(b.participant_count), 0) AS wallet_count,\
  \ COALESCE(MAX(b.participant_count) FILTER (WHERE b.snapshot_kind = 'start'\
  \   AND t.start_block IS NOT NULL AND b.block_number = t.start_block - 1), 0) AS start_count,\
  \ COALESCE(MAX(b.participant_count) FILTER (WHERE b.snapshot_kind = 'final'\
  \   AND t.score_cutoff_block IS NOT NULL AND b.block_number = t.score_cutoff_block\
  \   AND LOWER(b.block_hash) = LOWER(t.score_cutoff_block_hash)), 0) AS final_count,\
  \ MAX(b.block_number) AS latest_block, MAX(b.timestamp) AS latest_timestamp,\
  \ EXTRACT(EPOCH FROM MAX(b.published_at))::bigint AS updated_timestamp\
  \ FROM insights_snapshot_batches b JOIN target t ON t.slug = b.competition_slug\
  \   AND b.chain_id = t.chain_id AND b.release_router = t.release_router\
  \ WHERE LOWER(b.account_lens_address) = LOWER(t.account_lens_address)\
  \ AND (b.snapshot_kind = 'start' OR b.account_state_count > 0 OR NOT EXISTS (\
  \   SELECT 1 FROM insights_snapshot_batches prior\
  \   WHERE prior.competition_slug = b.competition_slug\
  \   AND prior.snapshot_kind IN ('live', 'final')\
  \   AND LOWER(prior.account_lens_address) = LOWER(t.account_lens_address)\
  \   AND prior.account_state_count > 0))\
  \ ), indexer AS (\
  \ SELECT i.last_indexed_block, i.last_indexed_block_hash,\
  \   EXTRACT(EPOCH FROM i.updated_at)::bigint AS updated_timestamp\
  \ FROM perps_indexer_state i\
  \ JOIN target t ON t.chain_id = i.chain_id AND t.release_router = i.release_router\
  \ WHERE i.indexer_name = ('perps-history:' || t.release_router) LIMIT 1\
  \ )\
  \ SELECT COALESCE(p.participant_count, 0), s.wallet_count, s.start_count, s.final_count,\
  \ s.latest_block, s.latest_timestamp, i.last_indexed_block, i.last_indexed_block_hash,\
  \ i.updated_timestamp, s.updated_timestamp\
  \ FROM participant_stats p CROSS JOIN snapshot_stats s LEFT JOIN indexer i ON TRUE"

getLatestIndexedSafeBlock
  :: Connection
  -> Integer
  -> Text
  -> IO (Maybe (Integer, Maybe Text))
getLatestIndexedSafeBlock conn chainId releaseRouter = do
  rows <- query conn
    "SELECT last_indexed_block, last_indexed_block_hash FROM perps_indexer_state\
    \ WHERE chain_id = ? AND release_router = ?\
    \ AND indexer_name = ('perps-history:' || ?) LIMIT 1"
    (chainId, normalizeAddress releaseRouter, normalizeAddress releaseRouter)
  pure $ firstRow rows

participantSelect :: Query
participantSelect =
  "SELECT competition_slug, wallet, alias, eligibility_status, eligibility_reason, integrity_flags,\
  \ EXTRACT(EPOCH FROM registered_at)::bigint, EXTRACT(EPOCH FROM reviewed_at)::bigint\
  \ FROM insights_competition_participants WHERE competition_slug = ? ORDER BY wallet ASC"

competitionSeedMetadataSelect :: Query
competitionSeedMetadataSelect =
  "SELECT slug, name, chain_id, release_router, usdc_address, margin_clearinghouse_address,\
  \ start_timestamp, new_risk_cutoff_timestamp, score_cutoff_timestamp, results_timestamp,\
  \ payment_deadline_timestamp, starting_balance_usdc, minimum_profit_bps, minimum_active_days,\
  \ scoring_version, rules_version, first_prize_usdc, second_prize_usdc, third_prize_usdc\
  \ FROM insights_competitions"

competitionSelect :: Query
competitionSelect =
  "SELECT slug, name, chain_id, release_router, usdc_address, margin_clearinghouse_address, account_lens_address, start_timestamp, new_risk_cutoff_timestamp, score_cutoff_timestamp,\
  \ results_timestamp, payment_deadline_timestamp, start_block, start_block_hash, score_cutoff_block, score_cutoff_block_hash,\
  \ starting_balance_usdc, minimum_profit_bps, minimum_active_days, scoring_version, rules_version,\
  \ first_prize_usdc, second_prize_usdc, third_prize_usdc, finalized,\
  \ EXTRACT(EPOCH FROM updated_at)::bigint FROM insights_competitions"

-- The start snapshot is the common finalized baseline at start_block - 1;
-- start_block itself is the first canonical block at or after the opening time.
-- External flows are therefore counted over [start_block, snapshot_block]. At and after cutoff, current snapshot
-- selection is pinned to the configured cutoff block/timestamp.
leaderboardQuery :: Query
leaderboardQuery =
  "WITH target AS (\
  \ SELECT * FROM insights_competitions WHERE slug = ?\
  \ ), start_batch AS (\
  \ SELECT b.* FROM insights_snapshot_batches b JOIN target t ON t.slug = b.competition_slug\
  \ WHERE b.snapshot_kind = 'start' AND (t.start_block IS NULL OR b.block_number = t.start_block - 1)\
  \ AND LOWER(b.account_lens_address) = LOWER(t.account_lens_address)\
  \ AND b.participant_count = (SELECT COUNT(*) FROM insights_competition_participants p WHERE p.competition_slug = t.slug)\
  \ ORDER BY b.block_number DESC, b.published_at DESC LIMIT 1\
  \ ), current_batch AS (\
  \ SELECT b.* FROM insights_snapshot_batches b JOIN target t ON t.slug = b.competition_slug\
  \ WHERE b.snapshot_kind IN ('live', 'final') AND b.timestamp < t.score_cutoff_timestamp\
  \ AND LOWER(b.account_lens_address) = LOWER(t.account_lens_address)\
  \ AND (b.account_state_count > 0 OR NOT EXISTS (\
  \   SELECT 1 FROM insights_snapshot_batches prior\
  \   WHERE prior.competition_slug = b.competition_slug\
  \   AND prior.snapshot_kind IN ('live', 'final')\
  \   AND LOWER(prior.account_lens_address) = LOWER(t.account_lens_address)\
  \   AND prior.account_state_count > 0))\
  \ AND b.participant_count = (SELECT COUNT(*) FROM insights_competition_participants p WHERE p.competition_slug = t.slug)\
  \ AND (t.score_cutoff_block IS NULL OR b.block_number <= t.score_cutoff_block)\
  \ AND (NOT t.finalized OR (b.snapshot_kind = 'final' AND b.block_number = t.score_cutoff_block\
  \   AND LOWER(b.block_hash) = LOWER(t.score_cutoff_block_hash)))\
  \ ORDER BY b.block_number DESC, CASE WHEN b.snapshot_kind = 'final' THEN 0 ELSE 1 END, b.published_at DESC LIMIT 1\
  \ ), start_snapshots AS (\
  \ SELECT s.wallet, s.has_open_position, s.signed_net_equity_usdc,\
  \ s.terminal_reachable_usdc, s.trader_claims_usdc, s.block_number, s.timestamp\
  \ FROM insights_account_snapshots s JOIN start_batch b ON b.competition_slug = s.competition_slug\
  \   AND b.snapshot_kind = s.snapshot_kind AND b.block_number = s.block_number\
  \   AND LOWER(b.block_hash) = LOWER(s.block_hash) AND b.chain_id = s.chain_id AND b.release_router = s.release_router\
  \ ), current_snapshots AS (\
  \ SELECT s.wallet, s.snapshot_kind, s.has_open_position, s.signed_net_equity_usdc,\
  \ s.terminal_reachable_usdc, s.trader_claims_usdc, s.block_number, s.timestamp, s.raw_data\
  \ FROM insights_account_snapshots s JOIN current_batch b ON b.competition_slug = s.competition_slug\
  \   AND b.snapshot_kind = s.snapshot_kind AND b.block_number = s.block_number\
  \   AND LOWER(b.block_hash) = LOWER(s.block_hash) AND b.chain_id = s.chain_id AND b.release_router = s.release_router\
  \ ), flows AS (\
  \ SELECT a.account AS wallet,\
  \ COALESCE(SUM(a.amount_usdc) FILTER (WHERE a.activity_type = 'Deposit'), 0) AS deposits_usdc,\
  \ COALESCE(SUM(a.amount_usdc) FILTER (WHERE a.activity_type = 'Withdraw'), 0) AS withdrawals_usdc\
  \ FROM perps_account_activity a JOIN target t ON t.chain_id = a.chain_id AND t.release_router = a.release_router\
  \ CROSS JOIN current_batch cb\
  \ WHERE a.timestamp >= t.start_timestamp AND a.timestamp < t.score_cutoff_timestamp\
  \ AND a.activity_type IN ('Deposit', 'Withdraw')\
  \ AND LOWER(COALESCE(a.contract_address, '')) = LOWER(t.margin_clearinghouse_address)\
  \ AND (LOWER(COALESCE(a.data->>'asset', '')) = LOWER(t.usdc_address)\
  \   OR NOT jsonb_exists(a.data, 'asset'))\
  \ AND (t.start_block IS NULL OR a.block_number >= t.start_block)\
  \ AND a.block_number <= cb.block_number\
  \ GROUP BY a.account\
  \ ), activity_stats AS (\
  \ SELECT a.account AS wallet,\
  \ COUNT(DISTINCT (((to_timestamp(a.timestamp) AT TIME ZONE 'UTC') + INTERVAL '2 hours')::date))\
  \   FILTER (WHERE a.activity_type IN ('Open', 'Close') AND COALESCE(a.size_delta, 0) <> 0\
  \     AND EXTRACT(ISODOW FROM ((to_timestamp(a.timestamp) AT TIME ZONE 'UTC') + INTERVAL '2 hours')) BETWEEN 1 AND 5) AS active_days,\
  \ FLOOR(COALESCE(SUM(CASE WHEN a.activity_type IN ('Open', 'Close', 'Liquidated')\
  \   AND a.size_delta IS NOT NULL AND a.price IS NOT NULL\
  \   THEN ABS(a.size_delta) * a.price / 100000000000000000000 ELSE 0 END), 0)) AS volume_usdc,\
  \ COUNT(*) FILTER (WHERE a.activity_type IN ('Open', 'Close') AND COALESCE(a.size_delta, 0) <> 0) AS executed_trades,\
  \ COUNT(*) FILTER (WHERE a.activity_type = 'Liquidated') AS liquidations,\
  \ COALESCE(SUM(a.pnl_usdc) FILTER (WHERE a.activity_type IN ('Close', 'Liquidated')), 0) AS realized_pnl_usdc\
  \ FROM perps_account_activity a JOIN target t ON t.chain_id = a.chain_id AND t.release_router = a.release_router\
  \ CROSS JOIN current_batch cb\
  \ WHERE a.timestamp >= t.start_timestamp AND a.timestamp < t.score_cutoff_timestamp\
  \ AND (t.start_block IS NULL OR a.block_number >= t.start_block)\
  \ AND a.block_number <= cb.block_number\
  \ GROUP BY a.account\
  \ ), adjustments AS (\
  \ SELECT m.wallet, COALESCE(SUM(m.amount_usdc), 0) AS amount_usdc\
  \ FROM insights_manual_adjustments m JOIN target t ON t.slug = m.competition_slug\
  \ WHERE m.voided_at IS NULL GROUP BY m.wallet\
  \ ), raw AS (\
  \ SELECT p.wallet, p.alias, p.eligibility_status, p.eligibility_reason,\
  \ t.starting_balance_usdc AS competition_starting_balance_usdc,\
  \ t.minimum_profit_bps AS competition_minimum_profit_bps,\
  \ t.minimum_active_days AS competition_minimum_active_days,\
  \ CASE WHEN ss.wallet IS NULL THEN NULL ELSE GREATEST(0,\
  \   CASE WHEN ss.has_open_position THEN ss.signed_net_equity_usdc ELSE ss.terminal_reachable_usdc END + ss.trader_claims_usdc) END AS starting_value_usdc,\
  \ CASE WHEN cs.wallet IS NULL THEN NULL ELSE GREATEST(0,\
  \   CASE WHEN cs.has_open_position THEN cs.signed_net_equity_usdc ELSE cs.terminal_reachable_usdc END + cs.trader_claims_usdc) END AS current_value_usdc,\
  \ COALESCE(f.deposits_usdc, 0) AS deposits_usdc, COALESCE(f.withdrawals_usdc, 0) AS withdrawals_usdc,\
  \ COALESCE(adj.amount_usdc, 0) AS adjustment_usdc, COALESCE(ast.active_days, 0) AS active_days,\
  \ COALESCE(ast.volume_usdc, 0) AS volume_usdc, COALESCE(ast.executed_trades, 0) AS executed_trades,\
  \ COALESCE(ast.liquidations, 0) AS liquidations, COALESCE(ast.realized_pnl_usdc, 0) AS realized_pnl_usdc,\
  \ cs.block_number, cs.timestamp, cs.has_open_position, cs.snapshot_kind,\
  \ CASE WHEN cs.has_open_position THEN cs.raw_data->>'side' ELSE NULL END AS position_side,\
  \ CASE WHEN cs.has_open_position THEN cs.raw_data->>'size' ELSE NULL END AS position_size_delta,\
  \ CASE WHEN cs.has_open_position THEN cs.raw_data->>'margin' ELSE NULL END AS position_margin_usdc,\
  \ CASE WHEN cs.has_open_position THEN cs.raw_data->>'entryPrice' ELSE NULL END AS position_entry_price,\
  \ CASE WHEN cs.has_open_position THEN cs.raw_data->>'unrealizedPnlUsdc' ELSE NULL END AS position_unrealized_pnl_usdc,\
  \ CASE WHEN cs.has_open_position THEN (cs.raw_data->>'liquidatable')::boolean ELSE NULL END AS position_liquidatable\
  \ FROM insights_competition_participants p JOIN target t ON t.slug = p.competition_slug\
  \ LEFT JOIN start_snapshots ss ON ss.wallet = p.wallet LEFT JOIN current_snapshots cs ON cs.wallet = p.wallet\
  \ LEFT JOIN flows f ON f.wallet = p.wallet LEFT JOIN activity_stats ast ON ast.wallet = p.wallet\
  \ LEFT JOIN adjustments adj ON adj.wallet = p.wallet\
  \ ), scored AS (\
  \ SELECT raw.*, CASE WHEN starting_value_usdc IS NULL OR current_value_usdc IS NULL THEN NULL\
  \ ELSE current_value_usdc - starting_value_usdc - deposits_usdc + withdrawals_usdc + adjustment_usdc END AS final_pnl_usdc\
  \ FROM raw\
  \ ), ranked AS (\
  \ SELECT scored.*, CASE WHEN final_pnl_usdc IS NULL THEN NULL ELSE RANK() OVER (ORDER BY final_pnl_usdc DESC NULLS LAST) END AS competition_rank\
  \ FROM scored\
  \ ), prize_candidates AS (\
  \ SELECT wallet, RANK() OVER (ORDER BY final_pnl_usdc DESC) AS prize_place,\
  \ COUNT(*) OVER (PARTITION BY final_pnl_usdc) AS prize_tie_count\
  \ FROM ranked WHERE final_pnl_usdc IS NOT NULL AND eligibility_status = 'eligible'\
  \ AND final_pnl_usdc >= competition_starting_balance_usdc * competition_minimum_profit_bps / 10000\
  \ AND active_days >= competition_minimum_active_days\
  \ ), with_prizes AS (\
  \ SELECT ranked.*, CASE WHEN pc.prize_place <= 3 THEN pc.prize_place ELSE NULL END AS prize_place,\
  \ CASE WHEN pc.prize_place <= 3 THEN pc.prize_tie_count ELSE NULL END AS prize_tie_count\
  \ FROM ranked LEFT JOIN prize_candidates pc ON pc.wallet = ranked.wallet\
  \ )\
  \ SELECT competition_rank, prize_place, prize_tie_count, wallet, alias, eligibility_status, eligibility_reason, final_pnl_usdc,\
  \ CASE WHEN final_pnl_usdc IS NULL OR competition_starting_balance_usdc = 0 THEN NULL\
  \ ELSE TRUNC(final_pnl_usdc * 10000 / competition_starting_balance_usdc)::bigint END AS roi_bps,\
  \ starting_value_usdc, current_value_usdc, deposits_usdc, withdrawals_usdc, adjustment_usdc,\
  \ active_days, volume_usdc, executed_trades, liquidations, realized_pnl_usdc, block_number, timestamp, has_open_position, snapshot_kind,\
  \ position_side, position_size_delta, position_margin_usdc, position_entry_price,\
  \ position_unrealized_pnl_usdc, position_liquidatable\
  \ FROM with_prizes"

leaderboardQuerySql :: Query
leaderboardQuerySql = leaderboardQuery

walletActivityQuery :: Query
walletActivityQuery =
  "WITH target AS (\
  \ SELECT * FROM insights_competitions WHERE slug = ?\
  \ ), current_batch AS (\
  \ SELECT b.* FROM insights_snapshot_batches b JOIN target t ON t.slug = b.competition_slug\
  \ WHERE b.snapshot_kind IN ('live', 'final') AND b.timestamp < t.score_cutoff_timestamp\
  \ AND LOWER(b.account_lens_address) = LOWER(t.account_lens_address)\
  \ AND (b.account_state_count > 0 OR NOT EXISTS (\
  \   SELECT 1 FROM insights_snapshot_batches prior\
  \   WHERE prior.competition_slug = b.competition_slug\
  \   AND prior.snapshot_kind IN ('live', 'final')\
  \   AND LOWER(prior.account_lens_address) = LOWER(t.account_lens_address)\
  \   AND prior.account_state_count > 0))\
  \ AND b.participant_count = (SELECT COUNT(*) FROM insights_competition_participants p WHERE p.competition_slug = t.slug)\
  \ AND (t.score_cutoff_block IS NULL OR b.block_number <= t.score_cutoff_block)\
  \ AND (NOT t.finalized OR (b.snapshot_kind = 'final' AND b.block_number = t.score_cutoff_block\
  \   AND LOWER(b.block_hash) = LOWER(t.score_cutoff_block_hash)))\
  \ ORDER BY b.block_number DESC, CASE WHEN b.snapshot_kind = 'final' THEN 0 ELSE 1 END, b.published_at DESC LIMIT 1\
  \ )\
  \ SELECT a.activity_type, a.side, a.price, a.size_delta, a.amount_usdc, a.pnl_usdc,\
  \ a.tx_hash, a.block_number, a.timestamp, a.log_index,\
  \ CASE WHEN EXTRACT(ISODOW FROM ((to_timestamp(a.timestamp) AT TIME ZONE 'UTC') + INTERVAL '2 hours')) BETWEEN 1 AND 5\
  \ THEN (((to_timestamp(a.timestamp) AT TIME ZONE 'UTC') + INTERVAL '2 hours')::date)::text ELSE NULL END\
  \ FROM perps_account_activity a JOIN target c ON c.chain_id = a.chain_id AND c.release_router = a.release_router\
  \ CROSS JOIN current_batch b\
  \ WHERE a.account = ? AND a.timestamp >= c.start_timestamp AND a.timestamp < c.score_cutoff_timestamp\
  \ AND (c.start_block IS NULL OR a.block_number >= c.start_block) AND a.block_number <= b.block_number\
  \ ORDER BY a.block_number DESC, a.log_index DESC LIMIT ?"

walletActivityQuerySql :: Query
walletActivityQuerySql = walletActivityQuery

finalizationReadinessQuery :: Query
finalizationReadinessQuery =
  "SELECT c.finalized, c.score_cutoff_timestamp, c.results_timestamp, c.start_block, c.score_cutoff_block,\
  \ (SELECT COUNT(*) FROM insights_competition_participants p WHERE p.competition_slug = c.slug),\
  \ (SELECT COUNT(*) FROM insights_competition_participants p WHERE p.competition_slug = c.slug\
  \   AND NULLIF(BTRIM(p.trader_reference), '') IS NULL),\
  \ (SELECT COUNT(*) FROM insights_competition_participants p WHERE p.competition_slug = c.slug\
  \   AND p.eligibility_status NOT IN ('eligible', 'ineligible')),\
  \ (SELECT COUNT(DISTINCT s.wallet) FROM insights_competition_participants p\
  \   JOIN insights_account_snapshots s ON s.competition_slug = p.competition_slug AND s.wallet = p.wallet\
  \   JOIN insights_snapshot_batches b ON b.competition_slug = s.competition_slug\
  \     AND b.snapshot_kind = s.snapshot_kind AND b.block_number = s.block_number\
  \     AND LOWER(b.block_hash) = LOWER(s.block_hash)\
  \   WHERE p.competition_slug = c.slug AND b.snapshot_kind = 'start'\
  \     AND c.start_block IS NOT NULL AND b.block_number = c.start_block - 1\
  \     AND b.chain_id = c.chain_id AND b.release_router = c.release_router\
  \     AND LOWER(b.account_lens_address) = LOWER(c.account_lens_address)\
  \     AND s.chain_id = b.chain_id AND s.release_router = b.release_router\
  \     AND b.participant_count = (SELECT COUNT(*) FROM insights_competition_participants p0 WHERE p0.competition_slug = c.slug)),\
  \ (SELECT COUNT(DISTINCT s.wallet) FROM insights_competition_participants p\
  \   JOIN insights_account_snapshots s ON s.competition_slug = p.competition_slug AND s.wallet = p.wallet\
  \   JOIN insights_snapshot_batches b ON b.competition_slug = s.competition_slug\
  \     AND b.snapshot_kind = s.snapshot_kind AND b.block_number = s.block_number\
  \     AND LOWER(b.block_hash) = LOWER(s.block_hash)\
  \   WHERE p.competition_slug = c.slug AND b.snapshot_kind = 'final'\
  \     AND b.block_number = c.score_cutoff_block AND LOWER(b.block_hash) = LOWER(c.score_cutoff_block_hash)\
  \     AND b.chain_id = c.chain_id AND b.release_router = c.release_router\
  \     AND LOWER(b.account_lens_address) = LOWER(c.account_lens_address)\
  \     AND (b.account_state_count > 0 OR NOT EXISTS (\
  \       SELECT 1 FROM insights_snapshot_batches prior\
  \       WHERE prior.competition_slug = c.slug AND prior.snapshot_kind IN ('live', 'final')\
  \       AND LOWER(prior.account_lens_address) = LOWER(c.account_lens_address)\
  \       AND prior.account_state_count > 0))\
  \     AND s.chain_id = b.chain_id AND s.release_router = b.release_router\
  \     AND b.participant_count = (SELECT COUNT(*) FROM insights_competition_participants p0 WHERE p0.competition_slug = c.slug)),\
  \ (SELECT COUNT(DISTINCT b.block_hash) FROM insights_snapshot_batches b\
  \   WHERE b.competition_slug = c.slug AND b.snapshot_kind = 'final'\
  \     AND b.block_number = c.score_cutoff_block AND LOWER(b.block_hash) = LOWER(c.score_cutoff_block_hash)\
  \     AND LOWER(b.account_lens_address) = LOWER(c.account_lens_address)\
  \     AND (b.account_state_count > 0 OR NOT EXISTS (\
  \       SELECT 1 FROM insights_snapshot_batches prior\
  \       WHERE prior.competition_slug = c.slug AND prior.snapshot_kind IN ('live', 'final')\
  \       AND LOWER(prior.account_lens_address) = LOWER(c.account_lens_address)\
  \       AND prior.account_state_count > 0))),\
  \ (SELECT MIN(b.block_hash) FROM insights_snapshot_batches b\
  \   WHERE b.competition_slug = c.slug AND b.snapshot_kind = 'final'\
  \     AND b.block_number = c.score_cutoff_block AND LOWER(b.block_hash) = LOWER(c.score_cutoff_block_hash)\
  \     AND LOWER(b.account_lens_address) = LOWER(c.account_lens_address)\
  \     AND (b.account_state_count > 0 OR NOT EXISTS (\
  \       SELECT 1 FROM insights_snapshot_batches prior\
  \       WHERE prior.competition_slug = c.slug AND prior.snapshot_kind IN ('live', 'final')\
  \       AND LOWER(prior.account_lens_address) = LOWER(c.account_lens_address)\
  \       AND prior.account_state_count > 0)))\
  \ FROM insights_competitions c WHERE c.slug = ? FOR UPDATE OF c"

normalizeLeaderboardSearch :: Maybe Text -> Maybe Text
normalizeLeaderboardSearch requestedSearch = do
  search <- requestedSearch
  let trimmed = T.take 100 $ T.strip search
  if T.null trimmed then Nothing else Just trimmed

-- PostgreSQL LIKE metacharacters are escaped so search is a literal,
-- case-insensitive substring match rather than a user-controlled pattern.
leaderboardSearchPattern :: Text -> Text
leaderboardSearchPattern search =
  "%" <> T.concatMap escapeLikeCharacter search <> "%"
  where
    escapeLikeCharacter '!' = "!!"
    escapeLikeCharacter '%' = "!%"
    escapeLikeCharacter '_' = "!_"
    escapeLikeCharacter character = T.singleton character

normalizeAddress :: Text -> Text
normalizeAddress value =
  let normalized = T.toLower $ T.strip value
   in if "0x" `T.isPrefixOf` normalized then normalized else "0x" <> normalized

validateOfficialAddress :: String -> Text -> IO ()
validateOfficialAddress variableName address
  | not (isValidAddress normalized) =
      fail $ variableName <> " must be a 20-byte Ethereum address"
  | normalized == "0x0000000000000000000000000000000000000000" =
      fail $ variableName <> " must not be the zero address"
  | otherwise = pure ()
  where
    normalized = normalizeAddress address

normalizeAlias :: Maybe Text -> Maybe Text
normalizeAlias = (\value -> if T.null value then Nothing else Just value) . maybe "" T.strip

epoch :: UTCTime -> Integer
epoch = round . utcTimeToPOSIXSeconds

numericIntegerField :: RowParser (Maybe Integer)
numericIntegerField = fmap scientificToInteger <$> (field :: RowParser (Maybe Scientific))

numericIntegerFieldRequired :: RowParser Integer
numericIntegerFieldRequired = scientificToInteger <$> (field :: RowParser Scientific)

scientificToInteger :: Scientific -> Integer
scientificToInteger value
  | scale >= 0 = coeff * (10 ^ scale)
  | otherwise = coeff `div` (10 ^ negate scale)
  where
    coeff = coefficient value
    scale = base10Exponent value

firstRow :: [a] -> Maybe a
firstRow = \case
  row : _ -> Just row
  [] -> Nothing

prizeAt :: Int -> CompetitionRules -> Integer
prizeAt index rules =
  case drop index $ crPrizeUsdc rules of
    prize : _ -> prize
    [] -> 0
