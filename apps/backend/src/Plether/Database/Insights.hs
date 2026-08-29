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
  , FinalizationCanonicalityTarget (..)
  , ensureInsightsSchema
  , validateCompetitionReleaseManifest
  , seedCompetition
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
  , refreshCompetitionIntegrityFlags
  , publishAccountSnapshotBatch
  , hasCompleteAccountSnapshotBatch
  , invalidateSnapshotBatchesAfter
  , invalidateCompetitionSnapshotsForReleaseRebuild
  , insertManualAdjustment
  , voidManualAdjustment
  , getCurrentCompetition
  , getCompetitionBySlug
  , getCompetitionLeaderboard
  , getCompetitionWallet
  , getCompetitionWalletActivity
  , materializeFinalizedStandings
  , getInsightsDataStatus
  , getLatestIndexedSafeBlock
  , leaderboardSearchPattern
  , leaderboardQuerySql
  , leaderboardOrderBySql
  , insightsDataStatusQuerySql
  , snapshotBatchAccessIndexSql
  , hasCompleteAccountSnapshotBatchQuerySql
  , walletActivityQuerySql
  , fundingIntegrityRefreshSql
  , manualRosterInsertionAllowed
  , snapshotKindText
  ) where

import Control.Monad (forM_, unless, when)
import Data.Aeson (Value, encode)
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int64)
import Data.List (nub, sort)
import Data.Scientific (Scientific, base10Exponent, coefficient)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (getPOSIXTime, posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
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
import Database.PostgreSQL.Simple.ToField (toField)
import Database.PostgreSQL.Simple.ToRow (ToRow (..))
import Plether.Insights.Competition
  ( CompetitionReleaseManifest (..)
  , CompetitionRules (..)
  , EquitySnapshot (..)
  , FinalizationReadiness (..)
  , ParticipantEligibility (..)
  , canSeedCompetitionRowAt
  , competitionReleaseManifestText
  , finalizationBlockers
  , july2026CompetitionSlug
  , participantEligibilityText
  )
import qualified Plether.Database.Insights.Registration as RegistrationDb
import Plether.Utils.Address (isValidAddress)

-- Immutable values used only to migrate the already-published July row. A
-- startup for a newer competition must never copy its active deployment into
-- historical rows that predate these columns.
julyUsdcAddress, julyReleaseRouter, julyMarginClearinghouseAddress, julyAccountLensAddress :: Text
julyUsdcAddress = "0xb15503d70b0eaa644dc6650d2a248762f7c5bce3"
julyReleaseRouter = "0x04e3103752f623fbcdcd01f588590af4c53e4c1e"
julyMarginClearinghouseAddress = "0x19c2f60f6312eaf9acde4c2b04551a05ca9be76e"
julyAccountLensAddress = "0xc4c886a6f1d7cb22c833ac1b29f29da43afbccd1"

julyReleaseManifest :: CompetitionReleaseManifest
julyReleaseManifest =
  CompetitionReleaseManifest
    { crmReleaseId = july2026CompetitionSlug
    , crmChainId = 421_614
    , crmUsdc = julyUsdcAddress
    , crmOrderRouter = julyReleaseRouter
    , crmMarginClearinghouse = julyMarginClearinghouseAddress
    , crmAccountLens = julyAccountLensAddress
    , crmCfdEngine = "0x6a25ea1015b5f032d8a2d95d57aefcb99219bf0a"
    , crmCfdEngineLens = "0xa9aa4097874e9622eaabee68f65ff5e3757728c5"
    , crmSettlementSidecar = "0x0b652c4d4610234e221403076c116292f935b424"
    , crmPletherOracle = "0xadfed3bf768d810309b97b4df9f9e77eaa3a401c"
    , crmIndexerStartBlock = 288_439_939
    }

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
  , csmAccountLensAddress :: Text
  , csmReleaseManifest :: Text
  , csmStartTimestamp :: Integer
  , csmNewRiskCutoffTimestamp :: Integer
  , csmScoreCutoffTimestamp :: Integer
  , csmResultsTimestamp :: Integer
  , csmPaymentDeadlineTimestamp :: Integer
  , csmStartingBalanceUsdc :: Integer
  , csmMinimumProfitBps :: Integer
  , csmMinimumActiveDays :: Int
  , csmFxSessionBoundaryUtcMinutes :: Int
  , csmRegistrationCloseTimestamp :: Maybe Integer
  , csmMinimumXAccountAgeDays :: Maybe Int
  , csmTargetXHandle :: Maybe Text
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
    <*> numericIntegerFieldRequired
    <*> numericIntegerFieldRequired
    <*> numericIntegerFieldRequired

data CompetitionSeedMismatch = CompetitionSeedMismatch
  { csmmField :: Text
  , csmmStored :: Text
  , csmmExpected :: Text
  }
  deriving stock (Show, Eq)

data CompetitionSeedInsert = CompetitionSeedInsert
  { csiMetadata :: CompetitionSeedMetadata
  , csiAccountLensAddress :: Text
  }

instance ToRow CompetitionSeedInsert where
  toRow CompetitionSeedInsert {csiMetadata = CompetitionSeedMetadata {..}, ..} =
    [ toField csmSlug
    , toField csmName
    , toField csmChainId
    , toField csmReleaseRouter
    , toField csmUsdcAddress
    , toField csmMarginClearinghouseAddress
    , toField csiAccountLensAddress
    , toField csmReleaseManifest
    , toField csmStartTimestamp
    , toField csmNewRiskCutoffTimestamp
    , toField csmScoreCutoffTimestamp
    , toField csmResultsTimestamp
    , toField csmPaymentDeadlineTimestamp
    , toField csmRegistrationCloseTimestamp
    , toField csmMinimumXAccountAgeDays
    , toField csmTargetXHandle
    , toField csmStartingBalanceUsdc
    , toField csmMinimumProfitBps
    , toField csmMinimumActiveDays
    , toField csmFxSessionBoundaryUtcMinutes
    , toField csmScoringVersion
    , toField csmRulesVersion
    , toField csmFirstPrizeUsdc
    , toField csmSecondPrizeUsdc
    , toField csmThirdPrizeUsdc
    ]

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
  , icrRegistrationOpenTimestamp :: Maybe Integer
  , icrRegistrationCloseTimestamp :: Maybe Integer
  , icrMinimumXAccountAgeDays :: Maybe Int
  , icrTargetXHandle :: Maybe Text
  , icrPrivacyNoticeVersion :: Maybe Text
  , icrStartBlock :: Maybe Integer
  , icrStartBlockHash :: Maybe Text
  , icrScoreCutoffBlock :: Maybe Integer
  , icrScoreCutoffBlockHash :: Maybe Text
  , icrStartingBalanceUsdc :: Integer
  , icrMinimumProfitBps :: Integer
  , icrMinimumActiveDays :: Int
  , icrFxSessionBoundaryUtcMinutes :: Int
  , icrScoringVersion :: Text
  , icrRulesVersion :: Text
  , icrFirstPrizeUsdc :: Integer
  , icrSecondPrizeUsdc :: Integer
  , icrThirdPrizeUsdc :: Integer
  , icrFinalized :: Bool
  , icrUpdatedTimestamp :: Integer
  , icrParticipantCount :: Integer
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
    <*> field
    <*> numericIntegerFieldRequired
    <*> numericIntegerFieldRequired
    <*> numericIntegerFieldRequired
    <*> field
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
  , ilrFundingIntegrityClear :: Bool
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
  , iarExecutionFeeUsdc :: Maybe Integer
  , iarVpiUsdc :: Maybe Integer
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
  , fdrStartBlockHash :: Maybe Text
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
    <*> field

-- | The complete on-chain identity that an operator must refetch while the
-- release history advisory lock is held immediately before standings freeze.
-- This closes the worker-poll/admin-finalize gap: stored hashes alone are not
-- a proof that the RPC's canonical fork still contains them.
data FinalizationCanonicalityTarget = FinalizationCanonicalityTarget
  { fctStartBlock :: Integer
  , fctStartBlockHash :: Text
  , fctBaselineBlock :: Integer
  , fctBaselineBlockHash :: Text
  , fctScoreCutoffBlock :: Integer
  , fctScoreCutoffBlockHash :: Text
  , fctIndexerBlock :: Integer
  , fctIndexerBlockHash :: Text
  }
  deriving stock (Show, Eq)

instance FromRow FinalizationCanonicalityTarget where
  fromRow = FinalizationCanonicalityTarget
    <$> field
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

ensureInsightsSchema :: Connection -> CompetitionRules -> Integer -> Text -> Text -> Text -> Text -> CompetitionReleaseManifest -> IO ()
ensureInsightsSchema conn rules chainId releaseRouter usdcAddress marginClearinghouseAddress accountLensAddress releaseManifest = do
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
    \ release_manifest TEXT,\
    \ start_timestamp BIGINT NOT NULL,\
    \ new_risk_cutoff_timestamp BIGINT NOT NULL,\
    \ score_cutoff_timestamp BIGINT NOT NULL,\
    \ results_timestamp BIGINT NOT NULL,\
    \ payment_deadline_timestamp BIGINT NOT NULL,\
    \ registration_open_timestamp BIGINT,\
    \ registration_close_timestamp BIGINT,\
    \ minimum_x_account_age_days INTEGER,\
    \ target_x_handle TEXT,\
    \ privacy_notice_version TEXT,\
    \ start_block BIGINT,\
    \ start_block_hash TEXT,\
    \ start_snapshot_block_hash TEXT,\
    \ score_cutoff_block BIGINT,\
    \ score_cutoff_block_hash TEXT,\
    \ starting_balance_usdc NUMERIC(78,0) NOT NULL,\
    \ minimum_profit_bps BIGINT NOT NULL,\
    \ minimum_active_days INTEGER NOT NULL,\
    \ fx_session_boundary_utc_minutes INTEGER NOT NULL DEFAULT 1320,\
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
    \ CHECK (minimum_active_days >= 0),\
    \ CONSTRAINT insights_competitions_fx_session_boundary_valid\
    \ CHECK (fx_session_boundary_utc_minutes >= 0 AND fx_session_boundary_utc_minutes < 1440),\
    \ CONSTRAINT insights_competitions_registration_metadata_consistent CHECK (\
    \   (registration_open_timestamp IS NULL AND registration_close_timestamp IS NULL\
    \     AND minimum_x_account_age_days IS NULL AND target_x_handle IS NULL)\
    \   OR (registration_close_timestamp IS NOT NULL\
    \     AND (registration_open_timestamp IS NULL OR registration_open_timestamp < registration_close_timestamp)\
    \     AND minimum_x_account_age_days IS NOT NULL AND minimum_x_account_age_days >= 0\
    \     AND NULLIF(BTRIM(target_x_handle), '') IS NOT NULL))\
    \ , CONSTRAINT insights_competitions_registration_privacy_version_consistent CHECK (\
    \   registration_open_timestamp IS NULL OR NULLIF(BTRIM(privacy_notice_version), '') IS NOT NULL)\
    \ )"
  _ <- execute_ conn
    "ALTER TABLE insights_competitions ADD COLUMN IF NOT EXISTS usdc_address TEXT"
  _ <- execute_ conn
    "ALTER TABLE insights_competitions ADD COLUMN IF NOT EXISTS margin_clearinghouse_address TEXT"
  _ <- execute_ conn
    "ALTER TABLE insights_competitions ADD COLUMN IF NOT EXISTS account_lens_address TEXT"
  _ <- execute_ conn
    "ALTER TABLE insights_competitions ADD COLUMN IF NOT EXISTS release_manifest TEXT"
  _ <- execute_ conn
    "ALTER TABLE insights_competitions ADD COLUMN IF NOT EXISTS start_block_hash TEXT"
  _ <- execute_ conn
    "ALTER TABLE insights_competitions ADD COLUMN IF NOT EXISTS start_snapshot_block_hash TEXT"
  _ <- execute_ conn
    "ALTER TABLE insights_competitions ADD COLUMN IF NOT EXISTS score_cutoff_block_hash TEXT"
  _ <- execute_ conn
    "ALTER TABLE insights_competitions ADD COLUMN IF NOT EXISTS registration_open_timestamp BIGINT"
  _ <- execute_ conn
    "ALTER TABLE insights_competitions ADD COLUMN IF NOT EXISTS registration_close_timestamp BIGINT"
  _ <- execute_ conn
    "ALTER TABLE insights_competitions ADD COLUMN IF NOT EXISTS minimum_x_account_age_days INTEGER"
  _ <- execute_ conn
    "ALTER TABLE insights_competitions ADD COLUMN IF NOT EXISTS target_x_handle TEXT"
  _ <- execute_ conn
    "ALTER TABLE insights_competitions ADD COLUMN IF NOT EXISTS privacy_notice_version TEXT"
  _ <- execute_ conn
    "ALTER TABLE insights_competitions ADD COLUMN IF NOT EXISTS fx_session_boundary_utc_minutes INTEGER"
  _ <- execute conn
    "UPDATE insights_competitions SET fx_session_boundary_utc_minutes = 1320\
    \ WHERE slug = ? AND fx_session_boundary_utc_minutes IS NULL"
    (Only july2026CompetitionSlug)
  _ <- execute conn
    "UPDATE insights_competitions SET fx_session_boundary_utc_minutes = ?\
    \ WHERE slug = ? AND fx_session_boundary_utc_minutes IS NULL"
    (crFxSessionBoundaryUtcMinutes rules, crSlug rules)
  unresolvedFxRows <- query_ conn
    "SELECT slug FROM insights_competitions WHERE fx_session_boundary_utc_minutes IS NULL"
  unless (null (unresolvedFxRows :: [Only Text])) $
    ioError $ userError "Insights competition FX boundary migration found an unknown historical slug; migrate it explicitly before startup"
  _ <- execute_ conn
    "ALTER TABLE insights_competitions ALTER COLUMN fx_session_boundary_utc_minutes SET NOT NULL"
  _ <- execute_ conn
    "DO $$ BEGIN\
    \ IF NOT EXISTS (SELECT 1 FROM pg_constraint\
    \   WHERE conrelid = 'insights_competitions'::regclass\
    \   AND conname = 'insights_competitions_fx_session_boundary_valid') THEN\
    \   ALTER TABLE insights_competitions ADD CONSTRAINT insights_competitions_fx_session_boundary_valid\
    \   CHECK (fx_session_boundary_utc_minutes >= 0 AND fx_session_boundary_utc_minutes < 1440);\
    \ END IF;\
    \ IF NOT EXISTS (SELECT 1 FROM pg_constraint\
    \   WHERE conrelid = 'insights_competitions'::regclass\
    \   AND conname = 'insights_competitions_registration_metadata_consistent') THEN\
    \   ALTER TABLE insights_competitions ADD CONSTRAINT insights_competitions_registration_metadata_consistent CHECK (\
    \     (registration_open_timestamp IS NULL AND registration_close_timestamp IS NULL\
    \       AND minimum_x_account_age_days IS NULL AND target_x_handle IS NULL)\
    \     OR (registration_close_timestamp IS NOT NULL\
    \       AND (registration_open_timestamp IS NULL OR registration_open_timestamp < registration_close_timestamp)\
    \       AND minimum_x_account_age_days IS NOT NULL AND minimum_x_account_age_days >= 0\
    \       AND NULLIF(BTRIM(target_x_handle), '') IS NOT NULL));\
    \ END IF;\
    \ IF NOT EXISTS (SELECT 1 FROM pg_constraint\
    \   WHERE conrelid = 'insights_competitions'::regclass\
    \   AND conname = 'insights_competitions_registration_privacy_version_consistent') THEN\
    \   ALTER TABLE insights_competitions ADD CONSTRAINT insights_competitions_registration_privacy_version_consistent\
    \   CHECK (registration_open_timestamp IS NULL OR NULLIF(BTRIM(privacy_notice_version), '') IS NOT NULL);\
    \ END IF; END $$"
  _ <- execute conn
    "UPDATE insights_competitions SET usdc_address = COALESCE(usdc_address, ?),\
    \ margin_clearinghouse_address = COALESCE(margin_clearinghouse_address, ?)\
    \ WHERE slug = ? AND (usdc_address IS NULL OR margin_clearinghouse_address IS NULL)"
    (julyUsdcAddress, julyMarginClearinghouseAddress, july2026CompetitionSlug)
  _ <- execute conn
    "UPDATE insights_competitions SET usdc_address = COALESCE(usdc_address, ?),\
    \ margin_clearinghouse_address = COALESCE(margin_clearinghouse_address, ?)\
    \ WHERE slug = ? AND LOWER(release_router) = LOWER(?)\
    \ AND (usdc_address IS NULL OR margin_clearinghouse_address IS NULL)"
    ( normalizeAddress usdcAddress
    , normalizeAddress marginClearinghouseAddress
    , crSlug rules
    , normalizeAddress releaseRouter
    )
  unresolvedAddressRows <- query_ conn
    "SELECT slug FROM insights_competitions\
    \ WHERE usdc_address IS NULL OR margin_clearinghouse_address IS NULL"
  unless (null (unresolvedAddressRows :: [Only Text])) $
    ioError $ userError "Insights competition address migration found an unknown release; migrate that immutable row explicitly before startup"
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
  _ <- execute_ conn
    "CREATE TABLE IF NOT EXISTS insights_finalized_standings (\
    \ competition_slug TEXT NOT NULL REFERENCES insights_competitions(slug) ON DELETE CASCADE,\
    \ competition_rank BIGINT, prize_place BIGINT, prize_tie_count BIGINT,\
    \ wallet VARCHAR(42) NOT NULL, alias TEXT, eligibility_status TEXT NOT NULL, eligibility_reason TEXT,\
    \ funding_integrity_clear BOOLEAN NOT NULL, final_pnl_usdc NUMERIC(78,0), roi_bps BIGINT,\
    \ starting_value_usdc NUMERIC(78,0), current_value_usdc NUMERIC(78,0),\
    \ deposits_usdc NUMERIC(78,0) NOT NULL, withdrawals_usdc NUMERIC(78,0) NOT NULL, adjustment_usdc NUMERIC(78,0) NOT NULL,\
    \ active_days INTEGER NOT NULL, volume_usdc NUMERIC(78,0) NOT NULL, executed_trades BIGINT NOT NULL,\
    \ liquidations BIGINT NOT NULL, realized_pnl_usdc NUMERIC(78,0) NOT NULL, block_number BIGINT, timestamp BIGINT,\
    \ has_open_position BOOLEAN, snapshot_kind TEXT, position_side TEXT, position_size_delta TEXT,\
    \ position_margin_usdc TEXT, position_entry_price TEXT, position_unrealized_pnl_usdc TEXT, position_liquidatable BOOLEAN,\
    \ materialized_at TIMESTAMPTZ NOT NULL DEFAULT NOW(), PRIMARY KEY (competition_slug, wallet)\
    \ )"
  -- Funding-integrity review consumes the immutable wallet-completion proof
  -- from the private registration schema.  Every core consumer (API, worker,
  -- admin, and integration harness) runs this initializer, so a rolling start
  -- cannot reach the strict query before the proof table exists.
  RegistrationDb.ensureRegistrationSchema conn
  _ <- execute conn
    "UPDATE insights_competitions SET account_lens_address = COALESCE(account_lens_address, ?),\
    \ release_manifest = COALESCE(release_manifest, ?)\
    \ WHERE slug = ? AND (account_lens_address IS NULL OR release_manifest IS NULL)"
    ( julyAccountLensAddress
    , competitionReleaseManifestText julyReleaseManifest
    , july2026CompetitionSlug
    )
  seedCompetition conn rules chainId releaseRouter usdcAddress marginClearinghouseAddress accountLensAddress releaseManifest
  unresolvedLensRows <- query_ conn
    "SELECT slug FROM insights_competitions WHERE account_lens_address IS NULL OR release_manifest IS NULL"
  unless (null (unresolvedLensRows :: [Only Text])) $
    ioError $ userError "Insights competition account-lens migration found an unknown release; migrate that immutable row explicitly before startup"
  _ <- execute_ conn
    "ALTER TABLE insights_competitions ALTER COLUMN account_lens_address SET NOT NULL"
  _ <- execute_ conn
    "ALTER TABLE insights_competitions ALTER COLUMN release_manifest SET NOT NULL"
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
  finalizedWithoutMaterialization <- query_ conn
    "SELECT c.slug FROM insights_competitions c WHERE c.finalized = TRUE\
    \ AND EXISTS (SELECT 1 FROM insights_competition_participants p WHERE p.competition_slug = c.slug)\
    \ AND NOT EXISTS (SELECT 1 FROM insights_finalized_standings s WHERE s.competition_slug = c.slug)"
    :: IO [Only Text]
  forM_ finalizedWithoutMaterialization $ \(Only finalizedSlug) -> do
    result <- withTransaction conn $ do
      advisoryRows <- query conn
        "SELECT 1::BIGINT FROM (SELECT pg_advisory_xact_lock(hashtextextended(\
        \ 'perps-indexer:perps-history-costs-v1:' || c.release_router, c.chain_id))\
        \ FROM insights_competitions c WHERE c.slug = ?) locked"
        (Only finalizedSlug) :: IO [Only Integer]
      unless (length advisoryRows == 1) $
        fail "Could not acquire the release history lock for finalized standings migration"
      lockedRows <- query conn
        "SELECT finalized FROM insights_competitions WHERE slug = ? FOR UPDATE"
        (Only finalizedSlug) :: IO [Only Bool]
      case lockedRows of
        [Only True] -> materializeFinalizedStandings conn finalizedSlug
        _ -> pure $ Left "competition is no longer finalized during standings migration"
    case result of
      Right _ -> pure ()
      Left err -> ioError $ userError $
        "Could not freeze historical finalized Insights standings for "
          <> T.unpack finalizedSlug <> ": " <> T.unpack err
  pure ()

seedCompetition :: Connection -> CompetitionRules -> Integer -> Text -> Text -> Text -> Text -> CompetitionReleaseManifest -> IO ()
seedCompetition conn rules chainId releaseRouter usdcAddress marginClearinghouseAddress accountLensAddress releaseManifest =
  withTransaction conn $ do
    seedStateRows <-
      query conn
        "SELECT EXISTS (SELECT 1 FROM insights_competitions WHERE slug = ?),\
        \ FLOOR(EXTRACT(EPOCH FROM NOW()))::BIGINT"
        (Only $ crSlug rules) :: IO [(Bool, Integer)]
    case seedStateRows of
      [(rowAlreadyExists, databaseNow)]
        | not $ canSeedCompetitionRowAt rowAlreadyExists rules $ posixSecondsToUTCTime $ fromInteger databaseNow ->
            ioError $ userError $
              "Refusing to seed Insights competition "
                <> T.unpack (crSlug rules)
                <> " because its registration window has already closed"
      [(_existing, _databaseNow)] -> pure ()
      _ -> ioError $ userError "Plether Insights could not read database time before competition seeding"
    let expected =
          competitionSeedMetadataFor
            rules
            chainId
            releaseRouter
            usdcAddress
            marginClearinghouseAddress
            accountLensAddress
            releaseManifest
    _ <- execute conn
      "INSERT INTO insights_competitions (\
      \ slug, name, chain_id, release_router, usdc_address, margin_clearinghouse_address, account_lens_address, release_manifest,\
      \ start_timestamp, new_risk_cutoff_timestamp, score_cutoff_timestamp,\
      \ results_timestamp, payment_deadline_timestamp, registration_open_timestamp, registration_close_timestamp,\
      \ minimum_x_account_age_days, target_x_handle, starting_balance_usdc,\
      \ minimum_profit_bps, minimum_active_days, fx_session_boundary_utc_minutes, scoring_version, rules_version,\
      \ first_prize_usdc, second_prize_usdc, third_prize_usdc)\
      \ VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?,\
      \ NULL,\
      \ ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)\
      \ ON CONFLICT (slug) DO NOTHING"
      CompetitionSeedInsert
        { csiMetadata = expected
        , csiAccountLensAddress = normalizeAddress accountLensAddress
        }
    storedRows <- query conn
      (competitionSeedMetadataSelect <> " WHERE slug = ? FOR UPDATE")
      (Only $ csmSlug expected)
    case storedRows of
      [stored] -> validateOrMigrateCompetitionSeed conn expected stored
      _ -> ioError $ userError $
        "Plether Insights could not read the competition row immediately after seeding slug "
          <> T.unpack (csmSlug expected)
          <> ". Check database constraints and transaction logs."

validateCompetitionReleaseManifest
  :: Connection
  -> Text
  -> CompetitionReleaseManifest
  -> IO ()
validateCompetitionReleaseManifest conn slug releaseManifest = do
  rows <- query conn
    "SELECT release_manifest FROM insights_competitions WHERE slug = ?"
    (Only slug)
    :: IO [Only Text]
  case rows of
    [Only stored]
      | stored == competitionReleaseManifestText releaseManifest -> pure ()
      | otherwise ->
          ioError $ userError $
            "Immutable Insights release manifest mismatch for slug "
              <> T.unpack slug
              <> "; refusing to read or write mixed-release history"
    [] ->
      ioError $ userError $
        "Insights competition release manifest is not seeded for slug " <> T.unpack slug
    _ -> ioError $ userError "Insights competition release manifest state is ambiguous"

competitionSeedMetadataFor
  :: CompetitionRules
  -> Integer
  -> Text
  -> Text
  -> Text
  -> Text
  -> CompetitionReleaseManifest
  -> CompetitionSeedMetadata
competitionSeedMetadataFor rules chainId releaseRouter usdcAddress marginClearinghouseAddress accountLensAddress releaseManifest =
  CompetitionSeedMetadata
    { csmSlug = crSlug rules
    , csmName = crName rules
    , csmChainId = chainId
    , csmReleaseRouter = normalizeAddress releaseRouter
    , csmUsdcAddress = normalizeAddress usdcAddress
    , csmMarginClearinghouseAddress = normalizeAddress marginClearinghouseAddress
    , csmAccountLensAddress = normalizeAddress accountLensAddress
    , csmReleaseManifest = competitionReleaseManifestText releaseManifest
    , csmStartTimestamp = epoch $ crStartAt rules
    , csmNewRiskCutoffTimestamp = epoch $ crNewRiskCutoffAt rules
    , csmScoreCutoffTimestamp = epoch $ crScoreCutoffAt rules
    , csmResultsTimestamp = epoch $ crResultsAt rules
    , csmPaymentDeadlineTimestamp = epoch $ crPaymentDeadlineAt rules
    , csmStartingBalanceUsdc = crStartingBalanceUsdc rules
    , csmMinimumProfitBps = crMinimumProfitBps rules
    , csmMinimumActiveDays = crMinimumActiveDays rules
    , csmFxSessionBoundaryUtcMinutes = crFxSessionBoundaryUtcMinutes rules
    , csmRegistrationCloseTimestamp = epoch <$> crRegistrationClosesAt rules
    , csmMinimumXAccountAgeDays = crMinimumXAccountAgeDays rules
    , csmTargetXHandle = crTargetXHandle rules
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
  , mismatch "account_lens_address" csmAccountLensAddress
  , mismatch "release_manifest" csmReleaseManifest
  , mismatchShow "start_timestamp" csmStartTimestamp
  , mismatchShow "new_risk_cutoff_timestamp" csmNewRiskCutoffTimestamp
  , mismatchShow "score_cutoff_timestamp" csmScoreCutoffTimestamp
  , mismatchShow "results_timestamp" csmResultsTimestamp
  , mismatchShow "payment_deadline_timestamp" csmPaymentDeadlineTimestamp
  , mismatchShow "starting_balance_usdc" csmStartingBalanceUsdc
  , mismatchShow "minimum_profit_bps" csmMinimumProfitBps
  , mismatchShow "minimum_active_days" csmMinimumActiveDays
  , mismatchShow "fx_session_boundary_utc_minutes" csmFxSessionBoundaryUtcMinutes
  , mismatchShow "registration_close_timestamp" csmRegistrationCloseTimestamp
  , mismatchShow "minimum_x_account_age_days" csmMinimumXAccountAgeDays
  , mismatchShow "target_x_handle" csmTargetXHandle
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
  csmSlug expected == july2026CompetitionSlug
    && csmSlug stored == july2026CompetitionSlug
    && csmPaymentDeadlineTimestamp stored == legacyPaymentDeadlineTimestamp
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
  -> Maybe (Integer, Text, Text)
  -> Maybe (Integer, Text)
  -> IO ()
setCompetitionBoundaryBlocks conn slug startBlock cutoffBlock = do
  let startBlockNumber = (\(number, _, _) -> number) <$> startBlock
      startBlockHash = normalizeAddress . (\(_, hash, _) -> hash) <$> startBlock
      startSnapshotBlockHash = normalizeAddress . (\(_, _, hash) -> hash) <$> startBlock
      cutoffBlockNumber = fst <$> cutoffBlock
      cutoffBlockHash = normalizeAddress . snd <$> cutoffBlock
  withTransaction conn $ do
    rows <- query conn
      "SELECT finalized, start_block, start_block_hash, start_snapshot_block_hash,\
      \ score_cutoff_block, score_cutoff_block_hash\
      \ FROM insights_competitions WHERE slug = ? FOR UPDATE"
      (Only slug)
      :: IO [(Bool, Maybe Integer, Maybe Text, Maybe Text, Maybe Integer, Maybe Text)]
    case rows of
      [(False, storedStart, storedStartHash, storedStartSnapshotHash, storedCutoff, storedCutoffHash)] -> do
        let startChanged = case (startBlockNumber, startBlockHash, startSnapshotBlockHash) of
              (Just number, Just boundaryHash, Just snapshotHash) ->
                storedStart /= Just number
                  || fmap normalizeAddress storedStartHash /= Just boundaryHash
                  || fmap normalizeAddress storedStartSnapshotHash /= Just snapshotHash
              _ -> False
            cutoffChanged = case (cutoffBlockNumber, cutoffBlockHash) of
              (Just number, Just hash) ->
                storedCutoff /= Just number || fmap normalizeAddress storedCutoffHash /= Just hash
              _ -> False
        when (startChanged || cutoffChanged) $ do
          _ <- execute conn
            "DELETE FROM insights_account_snapshots WHERE competition_slug = ?"
            (Only slug)
          _ <- execute conn
            "DELETE FROM insights_snapshot_batches WHERE competition_slug = ?"
            (Only slug)
          pure ()
        _ <- execute conn
          "UPDATE insights_competitions SET\
          \ start_block = CASE WHEN ? IS NULL THEN start_block ELSE ? END,\
          \ start_block_hash = CASE WHEN ? IS NULL THEN start_block_hash ELSE ? END,\
          \ start_snapshot_block_hash = CASE WHEN ? IS NULL THEN start_snapshot_block_hash ELSE ? END,\
          \ score_cutoff_block = CASE WHEN ? IS NULL THEN score_cutoff_block ELSE ? END,\
          \ score_cutoff_block_hash = CASE WHEN ? IS NULL THEN score_cutoff_block_hash ELSE ? END,\
          \ updated_at = CASE WHEN ? OR ? THEN NOW() ELSE updated_at END\
          \ WHERE slug = ? AND finalized = FALSE"
          ( startBlockNumber
          , startBlockNumber
          , startBlockHash
          , startBlockHash
          , startSnapshotBlockHash
          , startSnapshotBlockHash
          , cutoffBlockNumber
          , cutoffBlockNumber
          , cutoffBlockHash
          , cutoffBlockHash
          , startChanged
          , cutoffChanged
          , slug
          )
        pure ()
      _ -> pure ()

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
      "SELECT finalized, registration_close_timestamp FROM insights_competitions WHERE slug = ? FOR UPDATE"
      (Only slug) :: IO [(Bool, Maybe Integer)]
    case competitions of
      [] -> pure $ Left $ "Unknown Insights competition: " <> slug
      [(True, _)] -> pure $ Left "The competition is already finalized; registrations are locked"
      [(False, registrationClose)]
        | not $ manualRosterInsertionAllowed registrationClose ->
            pure $ Left "This competition uses verified first-party registration; manual roster insertion is disabled"
      [(False, _)]
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

manualRosterInsertionAllowed :: Maybe Integer -> Bool
manualRosterInsertionAllowed = maybe True (const False)

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
    mutable <- competitionAllowsManualRosterMutationForUpdate conn slug
    if not mutable
      then pure $ Left "The competition is missing, finalized, or uses verified first-party registration; wallet remaps are locked"
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
    mutable <- competitionAllowsManualRosterMutationForUpdate conn slug
    if not mutable
      then pure $ Left "The competition is missing, finalized, or uses verified first-party registration; wallet remaps are locked"
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
        refreshCompetitionIntegrityFlags conn slug
        previous <- query conn
          "SELECT eligibility_status, jsonb_array_length(integrity_flags) FROM insights_competition_participants\
          \ WHERE competition_slug = ? AND wallet = ? FOR UPDATE"
          (slug, normalizeAddress wallet)
        case previous of
          [(_previousStatus :: Text, integrityFlagCount :: Int)]
            | status == EligibilityEligible && integrityFlagCount > 0 -> pure False
          [(previousStatus :: Text, _integrityFlagCount :: Int)] -> do
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

finalizeCompetition
  :: Connection
  -> Text
  -> Text
  -> (FinalizationCanonicalityTarget -> IO (Either Text ()))
  -> IO (Either Text ())
finalizeCompetition _ _ finalizedBy _verifyCanonicality
  | T.null (T.strip finalizedBy) = pure $ Left "REVIEWER must not be empty"
finalizeCompetition conn slug finalizedBy verifyCanonicality =
  withTransaction conn $ do
    advisoryRows <- query conn
      "SELECT 1::BIGINT FROM (SELECT pg_advisory_xact_lock(hashtextextended(\
      \ 'perps-indexer:perps-history-costs-v1:' || c.release_router, c.chain_id))\
      \ FROM insights_competitions c WHERE c.slug = ?) locked"
      (Only slug) :: IO [Only Integer]
    unless (length advisoryRows == 1) $
      fail "Could not acquire the release history lock for competition finalization"
    locked <- query conn
      "SELECT finalized FROM insights_competitions WHERE slug = ? FOR UPDATE"
      (Only slug)
    case locked of
      [] -> pure $ Left $ "unknown competition: " <> slug
      [Only True] -> pure $ Left "standings are already finalized"
      [Only False] -> do
        refreshCompetitionIntegrityFlags conn slug
        rows <- query conn finalizationReadinessQuery (Only slug)
        now <- floor <$> getPOSIXTime
        case rows of
          [FinalizationDatabaseRow {..}] -> do
            let readiness =
                  FinalizationReadiness
                    { frNowTimestamp = now
                    , frScoreCutoffTimestamp = fdrScoreCutoffTimestamp
                    , frResultsTimestamp = fdrResultsTimestamp
                    , frStartBlock = fdrStartBlock
                    , frStartBlockHash = fdrStartBlockHash
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
              [] -> case (fdrStartBlockHash, fdrScoreCutoffBlock, fdrFinalSnapshotHash) of
                (Just _, Just finalBlock, Just finalHash) -> do
                  canonicalTargets <- query conn
                    "SELECT c.start_block, c.start_block_hash, c.start_block - 1, c.start_snapshot_block_hash,\
                    \ c.score_cutoff_block, c.score_cutoff_block_hash, i.last_indexed_block, i.last_indexed_block_hash\
                    \ FROM insights_competitions c JOIN perps_indexer_state i\
                    \ ON i.chain_id = c.chain_id AND i.release_router = c.release_router\
                    \ AND i.indexer_name = ('perps-history-costs-v1:' || c.release_router)\
                    \ WHERE c.slug = ? AND c.start_block IS NOT NULL AND c.start_block_hash IS NOT NULL\
                    \ AND c.start_snapshot_block_hash IS NOT NULL AND c.score_cutoff_block IS NOT NULL\
                    \ AND c.score_cutoff_block_hash IS NOT NULL AND i.last_indexed_block_hash IS NOT NULL"
                    (Only slug)
                  case canonicalTargets of
                    [target] -> verifyCanonicality target >>= \case
                      Left err -> pure $ Left $ "on-chain canonicality proof failed: " <> err
                      Right () -> do
                        _ <- execute conn
                          "DELETE FROM insights_finalized_standings WHERE competition_slug = ?"
                          (Only slug)
                        affected <- execute conn
                          "UPDATE insights_competitions SET finalized = TRUE, updated_at = NOW()\
                          \ WHERE slug = ? AND finalized = FALSE"
                          (Only slug)
                        if affected /= 1
                          then pure $ Left "standings changed while finalization was in progress; retry"
                          else do
                            materialized <- materializeFinalizedStandings conn slug
                            case materialized of
                              Right count | count == fdrParticipantCount -> do
                                _ <- execute conn
                                  "INSERT INTO insights_competition_finalization_audit\
                                  \ (competition_slug, finalized_by, participant_count, final_snapshot_block, final_snapshot_hash)\
                                  \ VALUES (?, ?, ?, ?, ?)"
                                  (slug, T.strip finalizedBy, fdrParticipantCount, finalBlock, finalHash)
                                pure $ Right ()
                              Left err -> do
                                _ <- execute conn
                                  "UPDATE insights_competitions SET finalized = FALSE WHERE slug = ?"
                                  (Only slug)
                                pure $ Left err
                              Right _ -> do
                                _ <- execute conn
                                  "UPDATE insights_competitions SET finalized = FALSE WHERE slug = ?"
                                  (Only slug)
                                pure $ Left "materialized standings count changed during finalization"
                    _ -> pure $ Left "canonical release cursor identity is unavailable"
                _ -> pure $ Left "canonical final snapshot identity is unavailable"
          _ -> pure $ Left "competition finalization state is ambiguous"
      _ -> pure $ Left "competition finalization state is ambiguous"

listCompetitionParticipants :: Connection -> Text -> IO [ParticipantRow]
listCompetitionParticipants conn slug = do
  refreshCompetitionIntegrityFlags conn slug
  query conn participantSelect (Only slug)

refreshCompetitionIntegrityFlags :: Connection -> Text -> IO ()
refreshCompetitionIntegrityFlags conn slug = do
  let refreshSql
        | slug == july2026CompetitionSlug = fundingIntegrityRefreshSqlLegacy
        | otherwise = fundingIntegrityRefreshSql
  _ <- execute conn refreshSql (Only slug)
  pure ()

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
    cursorReady <- query conn
      "SELECT EXISTS (SELECT 1 FROM perps_indexer_state i\
      \ WHERE i.chain_id = ? AND i.release_router = ?\
      \ AND i.indexer_name = ('perps-history-costs-v1:' || ?)\
      \ AND i.last_indexed_block_hash IS NOT NULL AND i.last_indexed_block >= ?)"
      (chainId, releaseRouter, releaseRouter, blockNumber)
    unless (cursorReady == [Only True]) $
      fail "Cannot publish an Insights snapshot ahead of a canonical perps-history cursor"
    configuredLens <- query conn
      "SELECT account_lens_address FROM insights_competitions WHERE slug = ?"
      (Only slug)
    unless (configuredLens == [Only accountLensAddress]) $
      fail "Cannot publish an Insights snapshot batch from a stale account lens"
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
    refreshCompetitionIntegrityFlags conn slug
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
    hasCompleteAccountSnapshotBatchQuerySql
    (slug, snapshotKindText kind, blockNumber, normalizeAddress blockHash)
  pure $ case rows of
    [Only found] -> found
    _ -> False

hasCompleteAccountSnapshotBatchQuerySql :: Query
hasCompleteAccountSnapshotBatchQuerySql =
    "SELECT EXISTS (\
    \ SELECT 1 FROM insights_snapshot_batches b\
    \ JOIN insights_competitions c ON c.slug = b.competition_slug\
    \ WHERE b.competition_slug = ? AND b.snapshot_kind = ? AND b.block_number = ?\
    \ AND LOWER(b.block_hash) = LOWER(?) AND b.participant_count > 0\
    \ AND b.chain_id = c.chain_id AND b.release_router = c.release_router\
    \ AND LOWER(b.account_lens_address) = LOWER(c.account_lens_address)\
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

invalidateSnapshotBatchesAfter :: Connection -> Text -> Integer -> Text -> IO ()
invalidateSnapshotBatchesAfter conn slug safeBlock safeBlockHash =
  withTransaction conn $ do
    mutable <- competitionIsMutableForUpdate conn slug
    when mutable $ do
      let canonicalHash = normalizeAddress safeBlockHash
      _ <- execute conn
        "DELETE FROM insights_account_snapshots\
        \ WHERE competition_slug = ? AND (block_number > ?\
        \ OR (block_number = ? AND LOWER(block_hash) <> LOWER(?)))"
        (slug, safeBlock, safeBlock, canonicalHash)
      _ <- execute conn
        "DELETE FROM insights_snapshot_batches\
        \ WHERE competition_slug = ? AND (block_number > ?\
        \ OR (block_number = ? AND LOWER(block_hash) <> LOWER(?)))"
        (slug, safeBlock, safeBlock, canonicalHash)
      _ <- execute conn
        "WITH args(safe_block, safe_hash) AS (VALUES (?::BIGINT, ?::TEXT))\
        \ UPDATE insights_competitions SET\
        \ start_block = CASE WHEN start_block > args.safe_block OR (start_block = args.safe_block\
        \   AND LOWER(COALESCE(start_block_hash, '')) <> LOWER(args.safe_hash)) THEN NULL ELSE start_block END,\
        \ start_block_hash = CASE WHEN start_block > args.safe_block OR (start_block = args.safe_block\
        \   AND LOWER(COALESCE(start_block_hash, '')) <> LOWER(args.safe_hash)) THEN NULL ELSE start_block_hash END,\
        \ start_snapshot_block_hash = CASE WHEN start_block > args.safe_block OR (start_block = args.safe_block\
        \   AND LOWER(COALESCE(start_block_hash, '')) <> LOWER(args.safe_hash)) THEN NULL ELSE start_snapshot_block_hash END,\
        \ score_cutoff_block = CASE WHEN score_cutoff_block > args.safe_block OR (score_cutoff_block = args.safe_block\
        \   AND LOWER(COALESCE(score_cutoff_block_hash, '')) <> LOWER(args.safe_hash)) THEN NULL ELSE score_cutoff_block END,\
        \ score_cutoff_block_hash = CASE WHEN score_cutoff_block > args.safe_block OR (score_cutoff_block = args.safe_block\
        \   AND LOWER(COALESCE(score_cutoff_block_hash, '')) <> LOWER(args.safe_hash)) THEN NULL ELSE score_cutoff_block_hash END,\
        \ updated_at = CASE WHEN start_block > args.safe_block OR score_cutoff_block > args.safe_block\
        \   OR (start_block = args.safe_block AND LOWER(COALESCE(start_block_hash, '')) <> LOWER(args.safe_hash))\
        \   OR (score_cutoff_block = args.safe_block AND LOWER(COALESCE(score_cutoff_block_hash, '')) <> LOWER(args.safe_hash))\
        \   THEN NOW() ELSE updated_at END\
        \ FROM args WHERE slug = ?"
        (safeBlock, canonicalHash, slug)
      pure ()

-- | A history-indexer rewind makes every event-derived Insights projection
-- provisional. Remove mutable release snapshots in the same transaction that
-- clears the canonical cursor so public reads cannot combine old account state
-- with a partially rebuilt activity history.
invalidateCompetitionSnapshotsForReleaseRebuild :: Connection -> Integer -> Text -> IO ()
invalidateCompetitionSnapshotsForReleaseRebuild conn chainId releaseRouter = do
  schemaState <- query_ conn
    "SELECT to_regclass('public.insights_competitions') IS NOT NULL"
    :: IO [Only Bool]
  case schemaState of
    [Only True] -> do
      rows <- query conn
        "SELECT slug FROM insights_competitions\
        \ WHERE chain_id = ? AND release_router = ? AND finalized = FALSE FOR UPDATE"
        (chainId, normalizeAddress releaseRouter)
        :: IO [Only Text]
      let slugs = [slug | Only slug <- rows]
      mapM_ invalidate slugs
    _ -> pure ()
  where
    invalidate slug = do
      _ <- execute conn
        "DELETE FROM insights_account_snapshots WHERE competition_slug = ?"
        (Only slug)
      _ <- execute conn
        "DELETE FROM insights_snapshot_batches WHERE competition_slug = ?"
        (Only slug)
      _ <- execute conn
        "UPDATE insights_competitions SET start_block = NULL, start_block_hash = NULL,\
        \ start_snapshot_block_hash = NULL, score_cutoff_block = NULL,\
        \ score_cutoff_block_hash = NULL, updated_at = NOW() WHERE slug = ?"
        (Only slug)
      pure ()

competitionIsMutableForUpdate :: Connection -> Text -> IO Bool
competitionIsMutableForUpdate conn slug = do
  rows <- query conn
    "SELECT NOT finalized FROM insights_competitions WHERE slug = ? FOR UPDATE"
    (Only slug)
  pure $ case rows of
    [Only mutable] -> mutable
    _ -> False

competitionAllowsManualRosterMutationForUpdate :: Connection -> Text -> IO Bool
competitionAllowsManualRosterMutationForUpdate conn slug = do
  rows <- query conn
    "SELECT NOT finalized AND registration_close_timestamp IS NULL\
    \ FROM insights_competitions WHERE slug = ? FOR UPDATE"
    (Only slug)
  pure $ case rows of
    [Only allowed] -> allowed
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

getCurrentCompetition :: Connection -> Text -> IO (Maybe CompetitionRow)
getCurrentCompetition conn activeSlug = do
  rows <- query conn (competitionSelect <> " WHERE slug = ? LIMIT 1") (Only activeSlug)
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
  competitionUsesMaterializedStandings conn slug >>= \case
    True -> case normalizeLeaderboardSearch requestedSearch of
      Nothing ->
        query conn
          (finalizedStandingsSelect <> " WHERE competition_slug = ?" <> leaderboardOrderBy)
          (slug, limitRows, offsetRows)
      Just search ->
        let pattern = leaderboardSearchPattern search
         in query conn
              (finalizedStandingsSelect
                <> " WHERE competition_slug = ? AND (wallet ILIKE ? ESCAPE '!' OR COALESCE(alias, '') ILIKE ? ESCAPE '!')"
                <> leaderboardOrderBy)
              (slug, pattern, pattern, limitRows, offsetRows)
    False -> case normalizeLeaderboardSearch requestedSearch of
      Nothing ->
        query conn
          (leaderboardQuery <> leaderboardOrderBy)
          (slug, limitRows, offsetRows)
      Just search ->
        let pattern = leaderboardSearchPattern search
         in query conn
              (leaderboardQuery
                <> " WHERE wallet ILIKE ? ESCAPE '!' OR COALESCE(alias, '') ILIKE ? ESCAPE '!'\
                   \"
                <> leaderboardOrderBy)
              (slug, pattern, pattern, limitRows, offsetRows)

getCompetitionWallet :: Connection -> Text -> Text -> IO (Maybe LeaderboardRow)
getCompetitionWallet conn slug wallet = do
  finalized <- competitionUsesMaterializedStandings conn slug
  rows <- if finalized
    then query conn
      (finalizedStandingsSelect <> " WHERE competition_slug = ? AND wallet = ? LIMIT 1")
      (slug, normalizeAddress wallet)
    else query conn
      (leaderboardQuery <> " WHERE wallet = ? LIMIT 1")
      (slug, normalizeAddress wallet)
  pure $ firstRow rows

competitionUsesMaterializedStandings :: Connection -> Text -> IO Bool
competitionUsesMaterializedStandings conn slug = do
  rows <- query conn
    "SELECT c.finalized, EXISTS (SELECT 1 FROM insights_finalized_standings s WHERE s.competition_slug = c.slug)\
    \ FROM insights_competitions c WHERE c.slug = ?"
    (Only slug) :: IO [(Bool, Bool)]
  pure $ case rows of
    [(True, True)] -> True
    -- Finalized data is never served from reorg-mutable history.  A missing
    -- materialization therefore produces an empty/fail-closed result; startup
    -- separately refuses to proceed until historical rows are frozen.
    [(True, False)] -> True
    _ -> False

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
  \ ), indexer AS (\
  \ SELECT i.last_indexed_block, i.last_indexed_block_hash,\
  \   EXTRACT(EPOCH FROM i.updated_at)::bigint AS updated_timestamp\
  \ FROM perps_indexer_state i\
  \ JOIN target t ON t.chain_id = i.chain_id AND t.release_router = i.release_router\
  \ WHERE i.indexer_name = ('perps-history-costs-v1:' || t.release_router) LIMIT 1\
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
    \ AND indexer_name = ('perps-history-costs-v1:' || ?) LIMIT 1"
    (chainId, normalizeAddress releaseRouter, normalizeAddress releaseRouter)
  pure $ firstRow rows

participantSelect :: Query
participantSelect =
  "SELECT competition_slug, wallet, alias, eligibility_status, eligibility_reason, integrity_flags,\
  \ EXTRACT(EPOCH FROM registered_at)::bigint, EXTRACT(EPOCH FROM reviewed_at)::bigint\
  \ FROM insights_competition_participants WHERE competition_slug = ? ORDER BY wallet ASC"

-- Funding review is deliberately separate from cash-flow-adjusted scoring.
-- Additional capital cannot increase displayed P&L, but it can increase risk
-- capacity and therefore blocks an eligible review state.
fundingIntegrityRefreshSql :: Query
fundingIntegrityRefreshSql =
  "WITH target AS (\
  \ SELECT c.*, i.configured_start_block FROM insights_competitions c\
  \ JOIN perps_indexer_state i ON i.chain_id = c.chain_id AND i.release_router = c.release_router\
  \  AND i.indexer_name = ('perps-history-costs-v1:' || c.release_router)\
  \  AND i.configured_start_block IS NOT NULL AND i.last_indexed_block_hash IS NOT NULL\
  \  AND split_part(c.release_manifest, '|', 12) ~ '^[1-9][0-9]*$'\
  \  AND i.configured_start_block = split_part(c.release_manifest, '|', 12)::bigint\
  \ WHERE c.slug = ? AND c.finalized = FALSE\
  \ ), start_batch AS (\
  \ SELECT b.* FROM insights_snapshot_batches b JOIN target t ON t.slug = b.competition_slug\
  \ WHERE b.snapshot_kind = 'start' AND t.start_block IS NOT NULL AND b.block_number = t.start_block - 1\
  \ AND t.start_snapshot_block_hash IS NOT NULL AND LOWER(b.block_hash) = LOWER(t.start_snapshot_block_hash)\
  \ AND b.participant_count = (SELECT COUNT(*) FROM insights_competition_participants p WHERE p.competition_slug = t.slug)\
  \ ORDER BY b.published_at DESC LIMIT 1\
  \ ), baseline AS (\
  \ SELECT s.wallet, GREATEST(0, CASE WHEN s.has_open_position THEN s.signed_net_equity_usdc\
  \  ELSE s.terminal_reachable_usdc END + s.trader_claims_usdc) AS value_usdc,\
  \ s.has_open_position, COALESCE(NULLIF(s.raw_data->>'pendingOrderCount', '')::integer, 0) AS pending_order_count,\
  \ s.block_number AS baseline_block FROM insights_account_snapshots s\
  \ JOIN start_batch b ON b.competition_slug = s.competition_slug AND b.snapshot_kind = s.snapshot_kind\
  \  AND b.block_number = s.block_number AND LOWER(b.block_hash) = LOWER(s.block_hash)\
  \ ), participants AS (\
  \ SELECT p.wallet, p.trader_reference, b.value_usdc, COALESCE(b.has_open_position, FALSE) AS has_open_position,\
  \ COALESCE(b.pending_order_count, 0) AS pending_order_count, b.baseline_block, t.*\
  \ FROM insights_competition_participants p JOIN target t ON t.slug = p.competition_slug\
  \ LEFT JOIN baseline b ON b.wallet = p.wallet\
  \ ), canonical_mints AS (\
  \ SELECT p.wallet, x.tx_hash, x.block_number, x.block_hash, x.tx_index, x.log_index\
  \ FROM participants p JOIN testnet_faucet_claims fc\
  \  ON LOWER(fc.address) = LOWER(p.wallet) AND LOWER(fc.token_address) = LOWER(p.usdc_address)\
  \ JOIN perps_usdc_transfers x ON x.chain_id = p.chain_id AND x.release_router = p.release_router\
  \  AND LOWER(x.token_address) = LOWER(p.usdc_address) AND LOWER(x.tx_hash) = LOWER(fc.tx_hash)\
  \  AND x.block_number = fc.mint_block_number AND x.from_address = '0x0000000000000000000000000000000000000000'\
  \  AND LOWER(x.to_address) = LOWER(p.wallet) AND x.amount = p.starting_balance_usdc\
  \ WHERE fc.status = 'success' AND fc.amount = p.starting_balance_usdc\
  \  AND NULLIF(BTRIM(fc.tx_hash), '') IS NOT NULL AND fc.mint_block_number IS NOT NULL\
  \  AND x.block_number >= p.configured_start_block AND x.timestamp < p.score_cutoff_timestamp\
  \ ), mint_counts AS (\
  \ SELECT p.wallet, COUNT(m.wallet) AS mint_count FROM participants p\
  \ LEFT JOIN canonical_mints m ON m.wallet = p.wallet GROUP BY p.wallet\
  \ ), first_mint AS (\
  \ SELECT DISTINCT ON (wallet) wallet, block_number, tx_index, log_index FROM canonical_mints\
  \ ORDER BY wallet, block_number, tx_index, log_index\
  \ ), flow_rows AS (\
  \ SELECT p.wallet, a.activity_type, a.amount_usdc, a.tx_hash, a.block_number, a.block_hash,\
  \ a.tx_index, a.log_index, a.timestamp, (SELECT MIN(x.log_index) FROM perps_usdc_transfers x\
  \  WHERE x.chain_id = p.chain_id AND x.release_router = p.release_router\
  \   AND LOWER(x.token_address) = LOWER(p.usdc_address) AND x.tx_hash = LOWER(a.tx_hash)\
  \   AND x.block_number = a.block_number AND x.block_hash = LOWER(a.block_hash) AND x.amount = a.amount_usdc\
  \   AND ((a.activity_type = 'Deposit' AND LOWER(x.from_address) = LOWER(p.wallet)\
  \    AND LOWER(x.to_address) = LOWER(p.margin_clearinghouse_address)) OR (a.activity_type = 'Withdraw'\
  \    AND LOWER(x.from_address) = LOWER(p.margin_clearinghouse_address) AND LOWER(x.to_address) = LOWER(p.wallet))))\
  \  AS transfer_log_index,\
  \ (LOWER(COALESCE(a.contract_address, '')) = LOWER(p.margin_clearinghouse_address)\
  \  AND LOWER(COALESCE(a.data->>'asset', '')) = LOWER(p.usdc_address) AND a.amount_usdc > 0\
  \  AND (SELECT COUNT(*) FROM perps_usdc_transfers x WHERE x.chain_id = p.chain_id\
  \    AND x.release_router = p.release_router AND LOWER(x.token_address) = LOWER(p.usdc_address)\
  \    AND x.tx_hash = LOWER(a.tx_hash) AND x.block_number = a.block_number AND x.block_hash = LOWER(a.block_hash)\
  \    AND x.amount = a.amount_usdc AND ((a.activity_type = 'Deposit' AND LOWER(x.from_address) = LOWER(p.wallet)\
  \      AND LOWER(x.to_address) = LOWER(p.margin_clearinghouse_address)) OR (a.activity_type = 'Withdraw'\
  \      AND LOWER(x.from_address) = LOWER(p.margin_clearinghouse_address) AND LOWER(x.to_address) = LOWER(p.wallet)))) = 1\
  \  AND (SELECT COUNT(*) FROM perps_account_activity peer WHERE peer.chain_id = p.chain_id\
  \    AND peer.release_router = p.release_router AND peer.account = p.wallet AND peer.activity_type = a.activity_type\
  \    AND peer.tx_hash = a.tx_hash AND peer.block_number = a.block_number AND peer.block_hash = a.block_hash\
  \    AND peer.amount_usdc = a.amount_usdc AND LOWER(COALESCE(peer.contract_address, '')) = LOWER(p.margin_clearinghouse_address)\
  \    AND LOWER(COALESCE(peer.data->>'asset', '')) = LOWER(p.usdc_address)) = 1) AS verified\
  \ FROM participants p JOIN perps_account_activity a ON a.chain_id = p.chain_id\
  \  AND a.release_router = p.release_router AND a.account = p.wallet\
  \ WHERE a.activity_type IN ('Deposit', 'Withdraw') AND a.block_number >= p.configured_start_block\
  \  AND a.timestamp < p.score_cutoff_timestamp\
  \ ), flow_summary AS (\
  \ SELECT p.wallet,\
  \ COUNT(*) FILTER (WHERE f.block_number <= p.baseline_block) AS baseline_flow_count,\
  \ COUNT(*) FILTER (WHERE f.activity_type = 'Deposit') AS deposit_count,\
  \ COUNT(*) FILTER (WHERE f.activity_type = 'Deposit' AND f.block_number <= p.baseline_block) AS baseline_deposit_count,\
  \ COALESCE(SUM(CASE WHEN f.activity_type = 'Deposit' THEN f.amount_usdc ELSE -f.amount_usdc END)\
  \  FILTER (WHERE f.verified AND f.block_number <= p.baseline_block), 0) AS baseline_flow_net,\
  \ COUNT(*) FILTER (WHERE NOT f.verified) AS unverified_flow_count,\
  \ COUNT(*) FILTER (WHERE f.verified AND f.activity_type = 'Deposit' AND f.amount_usdc = p.starting_balance_usdc\
  \  AND mc.mint_count = 1 AND ROW(f.block_number, f.tx_index, f.log_index) > ROW(m.block_number, m.tx_index, m.log_index)\
  \  AND f.block_number <= p.baseline_block) AS baseline_official_count,\
  \ COALESCE(SUM(f.amount_usdc) FILTER (WHERE f.verified AND f.activity_type = 'Deposit'\
  \  AND f.amount_usdc = p.starting_balance_usdc AND mc.mint_count = 1\
  \  AND ROW(f.block_number, f.tx_index, f.log_index) > ROW(m.block_number, m.tx_index, m.log_index)\
  \  AND f.block_number <= p.baseline_block), 0) AS baseline_official_amount,\
  \ COUNT(*) FILTER (WHERE f.verified AND f.activity_type = 'Deposit' AND f.amount_usdc = p.starting_balance_usdc\
  \  AND mc.mint_count = 1 AND ROW(f.block_number, f.tx_index, f.log_index) > ROW(m.block_number, m.tx_index, m.log_index)\
  \  AND f.block_number > p.baseline_block) AS post_official_count,\
  \ COALESCE(SUM(f.amount_usdc) FILTER (WHERE f.verified AND f.activity_type = 'Deposit'\
  \  AND f.amount_usdc = p.starting_balance_usdc AND mc.mint_count = 1\
  \  AND ROW(f.block_number, f.tx_index, f.log_index) > ROW(m.block_number, m.tx_index, m.log_index)\
  \  AND f.block_number > p.baseline_block), 0) AS post_official_amount,\
  \ COUNT(*) FILTER (WHERE f.activity_type = 'Deposit' AND mc.mint_count = 1\
  \  AND ROW(f.block_number, f.tx_index, f.log_index) <= ROW(m.block_number, m.tx_index, m.log_index)) AS pre_mint_deposit_count\
  \ FROM participants p LEFT JOIN flow_rows f ON f.wallet = p.wallet\
  \ LEFT JOIN mint_counts mc ON mc.wallet = p.wallet LEFT JOIN first_mint m ON m.wallet = p.wallet\
  \ GROUP BY p.wallet, p.baseline_block, p.starting_balance_usdc, mc.mint_count,\
  \  m.block_number, m.tx_index, m.log_index\
  \ ), running_funding AS (\
  \ SELECT wallet, SUM(CASE WHEN activity_type = 'Deposit' THEN amount_usdc ELSE -amount_usdc END)\
  \  OVER (PARTITION BY wallet ORDER BY block_number, tx_index, log_index) AS running_net\
  \ FROM flow_rows WHERE verified\
  \ ), funding_cap AS (SELECT wallet, COALESCE(MAX(running_net), 0) AS max_net_amount\
  \ FROM running_funding GROUP BY wallet), allocation AS (\
  \ SELECT DISTINCT ON (f.wallet) f.wallet, f.block_number, f.tx_index, f.log_index\
  \ FROM flow_rows f JOIN participants p ON p.wallet = f.wallet JOIN mint_counts mc ON mc.wallet = f.wallet\
  \ JOIN first_mint m ON m.wallet = f.wallet WHERE f.verified AND f.activity_type = 'Deposit'\
  \  AND f.amount_usdc = p.starting_balance_usdc AND mc.mint_count = 1\
  \  AND ROW(f.block_number, f.tx_index, f.log_index) > ROW(m.block_number, m.tx_index, m.log_index)\
  \ ORDER BY f.wallet, f.block_number, f.tx_index, f.log_index\
  \ ), first_trade AS (\
  \ SELECT DISTINCT ON (p.wallet) p.wallet, a.block_number, a.tx_index, a.log_index\
  \ FROM participants p JOIN perps_account_activity a ON a.chain_id = p.chain_id AND a.release_router = p.release_router\
  \  AND a.account = p.wallet AND a.activity_type IN ('Open', 'Close') AND COALESCE(a.size_delta, 0) <> 0\
  \ WHERE a.timestamp >= p.start_timestamp AND a.timestamp < p.score_cutoff_timestamp\
  \ ORDER BY p.wallet, a.block_number, a.tx_index, a.log_index\
  \ ), registration_audit AS (\
  \ SELECT p.wallet, (p.registration_close_timestamp IS NULL OR r.registration_id IS NOT NULL) AS verified_registration,\
  \ CASE WHEN p.registration_close_timestamp IS NULL OR r.registration_id IS NULL THEN FALSE ELSE (\
  \  EXISTS (SELECT 1 FROM perps_events e WHERE e.chain_id = p.chain_id AND e.release_router = p.release_router\
  \   AND e.account = p.wallet AND e.block_number <= r.wallet_verification_block) OR\
  \  EXISTS (SELECT 1 FROM perps_orders o WHERE o.chain_id = p.chain_id AND o.order_router = p.release_router\
  \   AND o.account = p.wallet AND o.commit_block_number <= r.wallet_verification_block) OR\
  \  EXISTS (SELECT 1 FROM perps_account_activity a WHERE a.chain_id = p.chain_id AND a.release_router = p.release_router\
  \   AND a.account = p.wallet AND a.block_number <= r.wallet_verification_block) OR\
  \  EXISTS (SELECT 1 FROM perps_usdc_transfers x WHERE x.chain_id = p.chain_id AND x.release_router = p.release_router\
  \   AND (x.from_address = p.wallet OR x.to_address = p.wallet)\
  \   AND x.block_number <= r.wallet_verification_block)) END AS prior_activity\
  \ FROM participants p LEFT JOIN insights_registration_applications r ON r.competition_slug = p.slug\
  \  AND r.registration_id::text = p.trader_reference AND r.status = 'completed' AND r.trading_account = p.wallet\
  \ ), invalid_inbound AS (\
  \ SELECT p.wallet, COUNT(*) AS invalid_count FROM participants p JOIN perps_usdc_transfers x\
  \  ON x.chain_id = p.chain_id AND x.release_router = p.release_router\
  \  AND LOWER(x.token_address) = LOWER(p.usdc_address) AND LOWER(x.to_address) = LOWER(p.wallet)\
  \ WHERE x.amount > 0 AND x.block_number >= p.configured_start_block AND x.timestamp < p.score_cutoff_timestamp\
  \  AND NOT (EXISTS (SELECT 1 FROM canonical_mints m WHERE m.wallet = p.wallet AND m.tx_hash = x.tx_hash\
  \    AND m.block_number = x.block_number AND m.block_hash = x.block_hash AND m.log_index = x.log_index)\
  \   OR (SELECT COUNT(*) FROM flow_rows f WHERE f.wallet = p.wallet AND f.verified\
  \    AND f.activity_type = 'Withdraw' AND f.tx_hash = x.tx_hash AND f.block_number = x.block_number\
  \    AND f.block_hash = x.block_hash AND f.amount_usdc = x.amount AND f.transfer_log_index = x.log_index\
  \    AND LOWER(x.from_address) = LOWER(p.margin_clearinghouse_address)) = 1)\
  \ GROUP BY p.wallet\
  \ ), premature_outbound AS (\
  \ SELECT p.wallet, COUNT(*) AS invalid_count FROM participants p JOIN allocation al ON al.wallet = p.wallet\
  \ JOIN first_mint m ON m.wallet = p.wallet\
  \ JOIN perps_usdc_transfers x ON x.chain_id = p.chain_id AND x.release_router = p.release_router\
  \  AND LOWER(x.token_address) = LOWER(p.usdc_address) AND LOWER(x.from_address) = LOWER(p.wallet)\
  \  AND ROW(x.block_number, x.tx_index, x.log_index) > ROW(m.block_number, m.tx_index, m.log_index)\
  \  AND ROW(x.block_number, x.tx_index, x.log_index) < ROW(al.block_number, al.tx_index, al.log_index)\
  \ WHERE x.amount > 0 AND NOT EXISTS (SELECT 1 FROM flow_rows f WHERE f.wallet = p.wallet AND f.verified\
  \  AND f.activity_type = 'Deposit' AND f.tx_hash = x.tx_hash AND f.block_number = x.block_number\
  \  AND f.block_hash = x.block_hash AND f.amount_usdc = x.amount AND f.transfer_log_index = x.log_index\
  \  AND LOWER(x.to_address) = LOWER(p.margin_clearinghouse_address)) GROUP BY p.wallet\
  \ ), facts AS (\
  \ SELECT p.wallet, p.value_usdc, p.has_open_position, p.pending_order_count, p.starting_balance_usdc,\
  \ COALESCE(mc.mint_count, 0) AS mint_count, COALESCE(fs.baseline_flow_count, 0) AS baseline_flow_count,\
  \ COALESCE(fs.deposit_count, 0) AS deposit_count, COALESCE(fs.baseline_deposit_count, 0) AS baseline_deposit_count,\
  \ COALESCE(fs.baseline_flow_net, 0) AS baseline_flow_net,\
  \ COALESCE(fs.unverified_flow_count, 0) AS unverified_flow_count, COALESCE(fs.baseline_official_count, 0) AS baseline_official_count,\
  \ COALESCE(fs.baseline_official_amount, 0) AS baseline_official_amount, COALESCE(fs.post_official_count, 0) AS post_official_count,\
  \ COALESCE(fs.post_official_amount, 0) AS post_official_amount, COALESCE(fs.pre_mint_deposit_count, 0) AS pre_mint_deposit_count,\
  \ COALESCE(cap.max_net_amount, 0) AS max_net_amount, COALESCE(ii.invalid_count, 0) AS invalid_inbound_count,\
  \ COALESCE(po.invalid_count, 0) AS premature_outbound_count, al.block_number AS allocation_block,\
  \ al.tx_index AS allocation_tx, al.log_index AS allocation_log, tr.block_number AS trade_block,\
  \ tr.tx_index AS trade_tx, tr.log_index AS trade_log, COALESCE(ra.verified_registration, FALSE) AS verified_registration,\
  \ COALESCE(ra.prior_activity, FALSE) AS prior_registration_activity FROM participants p\
  \ LEFT JOIN mint_counts mc ON mc.wallet = p.wallet LEFT JOIN flow_summary fs ON fs.wallet = p.wallet\
  \ LEFT JOIN funding_cap cap ON cap.wallet = p.wallet LEFT JOIN invalid_inbound ii ON ii.wallet = p.wallet\
  \ LEFT JOIN premature_outbound po ON po.wallet = p.wallet LEFT JOIN allocation al ON al.wallet = p.wallet\
  \ LEFT JOIN first_trade tr ON tr.wallet = p.wallet LEFT JOIN registration_audit ra ON ra.wallet = p.wallet\
  \ ), computed AS (SELECT wallet, TO_JSONB(ARRAY_REMOVE(ARRAY[\
  \ CASE WHEN value_usdc IS NULL THEN 'baseline_unavailable' END,\
  \ CASE WHEN NOT verified_registration THEN 'missing_verified_registration' END,\
  \ CASE WHEN prior_registration_activity THEN 'pre_registration_activity' END,\
  \ CASE WHEN has_open_position THEN 'baseline_open_position' END,\
  \ CASE WHEN pending_order_count > 0 THEN 'baseline_pending_orders' END,\
  \ CASE WHEN value_usdc IS NOT NULL AND value_usdc NOT IN (0, starting_balance_usdc) THEN 'invalid_starting_bankroll' END,\
  \ CASE WHEN mint_count <> 1 THEN 'official_mint_count_invalid' END,\
  \ CASE WHEN deposit_count <> 1 THEN 'unofficial_capital_used' END,\
  \ CASE WHEN pre_mint_deposit_count > 0 THEN 'deposit_before_official_mint' END,\
  \ CASE WHEN value_usdc = starting_balance_usdc AND baseline_official_count <> 1 THEN 'baseline_official_allocation_count_invalid' END,\
  \ CASE WHEN value_usdc = starting_balance_usdc AND baseline_official_amount <> starting_balance_usdc THEN 'baseline_official_allocation_amount_invalid' END,\
  \ CASE WHEN value_usdc = starting_balance_usdc AND baseline_flow_net <> starting_balance_usdc THEN 'baseline_funding_flow_mismatch' END,\
  \ CASE WHEN value_usdc = starting_balance_usdc AND post_official_count > 0 THEN 'unexpected_official_deposit' END,\
  \ CASE WHEN value_usdc = 0 AND (baseline_flow_count <> 0 OR baseline_deposit_count <> 0) THEN 'unexpected_prebaseline_funding' END,\
  \ CASE WHEN value_usdc = 0 AND baseline_flow_net <> 0 THEN 'zero_baseline_funding_flow_mismatch' END,\
  \ CASE WHEN value_usdc = 0 AND post_official_count <> 1 THEN 'official_allocation_count_invalid' END,\
  \ CASE WHEN value_usdc = 0 AND post_official_amount <> starting_balance_usdc THEN 'official_allocation_amount_invalid' END,\
  \ CASE WHEN value_usdc = 0 AND trade_block IS NOT NULL\
  \  AND ROW(allocation_block, allocation_tx, allocation_log) >= ROW(trade_block, trade_tx, trade_log)\
  \  THEN 'official_allocation_not_before_trading' END,\
  \ CASE WHEN unverified_flow_count > 0 THEN 'unverified_funding_flow' END,\
  \ CASE WHEN premature_outbound_count > 0 THEN 'official_funds_left_before_allocation' END,\
  \ CASE WHEN max_net_amount > starting_balance_usdc THEN 'funding_capacity_exceeded' END\
  \ ]::TEXT[], NULL)) AS flags FROM facts)\
  \ UPDATE insights_competition_participants p SET integrity_flags = c.flags, updated_at = NOW()\
  \ FROM computed c, target t WHERE p.competition_slug = t.slug AND p.wallet = c.wallet"

fundingIntegrityRefreshSqlLegacy :: Query
fundingIntegrityRefreshSqlLegacy =
  "WITH target AS (SELECT * FROM insights_competitions WHERE slug = ? AND finalized = FALSE),\
  \ start_batch AS (\
  \ SELECT b.* FROM insights_snapshot_batches b JOIN target t ON t.slug = b.competition_slug\
  \ JOIN perps_indexer_state i ON i.chain_id = t.chain_id AND i.release_router = t.release_router\
  \   AND i.indexer_name = ('perps-history-costs-v1:' || t.release_router)\
  \   AND i.last_indexed_block_hash IS NOT NULL AND i.last_indexed_block >= b.block_number\
  \ WHERE b.snapshot_kind = 'start' AND t.start_block IS NOT NULL AND b.block_number = t.start_block - 1\
  \ AND t.start_snapshot_block_hash IS NOT NULL\
  \ AND LOWER(b.block_hash) = LOWER(t.start_snapshot_block_hash)\
  \ AND b.participant_count = (SELECT COUNT(*) FROM insights_competition_participants p WHERE p.competition_slug = t.slug)\
  \ ORDER BY b.published_at DESC LIMIT 1\
  \ ), baseline AS (\
  \ SELECT s.wallet, GREATEST(0, CASE WHEN s.has_open_position THEN s.signed_net_equity_usdc\
  \   ELSE s.terminal_reachable_usdc END + s.trader_claims_usdc) AS value_usdc,\
  \ s.has_open_position, COALESCE(NULLIF(s.raw_data->>'pendingOrderCount', '')::integer, 0) AS pending_order_count,\
  \ s.block_number AS baseline_block\
  \ FROM insights_account_snapshots s JOIN start_batch b ON b.competition_slug = s.competition_slug\
  \ AND b.snapshot_kind = s.snapshot_kind AND b.block_number = s.block_number\
  \ AND LOWER(b.block_hash) = LOWER(s.block_hash)\
  \ ), facts AS (\
  \ SELECT p.wallet, b.value_usdc, COALESCE(b.has_open_position, FALSE) AS has_open_position,\
  \ COALESCE(b.pending_order_count, 0) AS pending_order_count, t.starting_balance_usdc,\
  \ COALESCE(pre.official_count, 0) AS baseline_official_count,\
  \ COALESCE(pre.official_amount, 0) AS baseline_official_amount,\
  \ COALESCE(pre.unverified_count, 0) AS baseline_unverified_count,\
  \ COALESCE(pre.net_amount, 0) AS baseline_flow_net,\
  \ COALESCE(d.official_count, 0) AS official_count, COALESCE(d.official_amount, 0) AS official_amount,\
  \ COALESCE(d.unverified_count, 0) AS unverified_count,\
  \ allocation.allocation_block, allocation.allocation_tx, allocation.allocation_log,\
  \ tr.trade_block, tr.trade_tx, tr.trade_log, COALESCE(cap.max_net_amount, 0) AS max_net_amount,\
  \ COALESCE(provenance.unverified_count, 0) AS any_unverified_count\
  \ FROM insights_competition_participants p JOIN target t ON t.slug = p.competition_slug\
  \ LEFT JOIN baseline b ON b.wallet = p.wallet\
  \ LEFT JOIN LATERAL (\
  \   SELECT COUNT(*) FILTER (WHERE a.activity_type = 'Deposit' AND a.amount_usdc = t.starting_balance_usdc\
  \     AND EXISTS (SELECT 1 FROM testnet_faucet_claims fc\
  \       WHERE LOWER(fc.address) = LOWER(p.wallet) AND LOWER(fc.token_address) = LOWER(t.usdc_address)\
  \       AND fc.status = 'success' AND fc.amount = t.starting_balance_usdc AND fc.tx_hash IS NOT NULL\
  \       AND fc.mint_block_number IS NOT NULL AND fc.mint_block_number < a.block_number)) AS official_count,\
  \   COALESCE(SUM(a.amount_usdc) FILTER (WHERE a.activity_type = 'Deposit' AND a.amount_usdc = t.starting_balance_usdc\
  \     AND EXISTS (SELECT 1 FROM testnet_faucet_claims fc\
  \       WHERE LOWER(fc.address) = LOWER(p.wallet) AND LOWER(fc.token_address) = LOWER(t.usdc_address)\
  \       AND fc.status = 'success' AND fc.amount = t.starting_balance_usdc AND fc.tx_hash IS NOT NULL\
  \       AND fc.mint_block_number IS NOT NULL AND fc.mint_block_number < a.block_number)), 0) AS official_amount,\
  \   COUNT(*) FILTER (WHERE a.activity_type = 'Deposit' AND NOT (a.amount_usdc = t.starting_balance_usdc\
  \     AND EXISTS (SELECT 1 FROM testnet_faucet_claims fc\
  \       WHERE LOWER(fc.address) = LOWER(p.wallet) AND LOWER(fc.token_address) = LOWER(t.usdc_address)\
  \       AND fc.status = 'success' AND fc.amount = t.starting_balance_usdc AND fc.tx_hash IS NOT NULL\
  \       AND fc.mint_block_number IS NOT NULL AND fc.mint_block_number < a.block_number))) AS unverified_count,\
  \   COALESCE(SUM(CASE WHEN a.activity_type = 'Deposit' THEN a.amount_usdc ELSE -a.amount_usdc END), 0) AS net_amount\
  \   FROM perps_account_activity a WHERE a.chain_id = t.chain_id AND a.release_router = t.release_router\
  \   AND a.account = p.wallet AND a.activity_type IN ('Deposit', 'Withdraw')\
  \   AND LOWER(COALESCE(a.contract_address, '')) = LOWER(t.margin_clearinghouse_address)\
  \   AND jsonb_exists(a.data, 'asset') AND LOWER(a.data->>'asset') = LOWER(t.usdc_address)\
  \   AND b.baseline_block IS NOT NULL AND a.block_number <= b.baseline_block\
  \ ) pre ON TRUE\
  \ LEFT JOIN LATERAL (\
  \   SELECT COUNT(*) FILTER (WHERE a.amount_usdc = t.starting_balance_usdc\
  \     AND EXISTS (SELECT 1 FROM testnet_faucet_claims fc\
  \       WHERE LOWER(fc.address) = LOWER(p.wallet) AND LOWER(fc.token_address) = LOWER(t.usdc_address)\
  \       AND fc.status = 'success' AND fc.amount = t.starting_balance_usdc AND fc.tx_hash IS NOT NULL\
  \       AND fc.mint_block_number IS NOT NULL AND fc.mint_block_number < a.block_number)) AS official_count,\
  \   COALESCE(SUM(a.amount_usdc) FILTER (WHERE a.amount_usdc = t.starting_balance_usdc\
  \     AND EXISTS (SELECT 1 FROM testnet_faucet_claims fc\
  \       WHERE LOWER(fc.address) = LOWER(p.wallet) AND LOWER(fc.token_address) = LOWER(t.usdc_address)\
  \       AND fc.status = 'success' AND fc.amount = t.starting_balance_usdc AND fc.tx_hash IS NOT NULL\
  \       AND fc.mint_block_number IS NOT NULL AND fc.mint_block_number < a.block_number)), 0) AS official_amount,\
  \   COUNT(*) FILTER (WHERE NOT (a.amount_usdc = t.starting_balance_usdc\
  \     AND EXISTS (SELECT 1 FROM testnet_faucet_claims fc\
  \       WHERE LOWER(fc.address) = LOWER(p.wallet) AND LOWER(fc.token_address) = LOWER(t.usdc_address)\
  \       AND fc.status = 'success' AND fc.amount = t.starting_balance_usdc AND fc.tx_hash IS NOT NULL\
  \       AND fc.mint_block_number IS NOT NULL AND fc.mint_block_number < a.block_number))) AS unverified_count\
  \   FROM perps_account_activity a WHERE a.chain_id = t.chain_id AND a.release_router = t.release_router\
  \   AND a.account = p.wallet AND a.activity_type = 'Deposit'\
  \   AND LOWER(COALESCE(a.contract_address, '')) = LOWER(t.margin_clearinghouse_address)\
  \   AND jsonb_exists(a.data, 'asset') AND LOWER(a.data->>'asset') = LOWER(t.usdc_address)\
  \   AND b.baseline_block IS NOT NULL AND a.block_number > b.baseline_block\
  \   AND a.timestamp < t.score_cutoff_timestamp\
  \ ) d ON TRUE\
  \ LEFT JOIN LATERAL (\
  \   SELECT a.block_number AS allocation_block, a.tx_index AS allocation_tx, a.log_index AS allocation_log\
  \   FROM perps_account_activity a WHERE a.chain_id = t.chain_id AND a.release_router = t.release_router\
  \   AND a.account = p.wallet AND a.activity_type = 'Deposit' AND a.amount_usdc = t.starting_balance_usdc\
  \   AND LOWER(COALESCE(a.contract_address, '')) = LOWER(t.margin_clearinghouse_address)\
  \   AND jsonb_exists(a.data, 'asset') AND LOWER(a.data->>'asset') = LOWER(t.usdc_address)\
  \   AND b.baseline_block IS NOT NULL AND a.block_number > b.baseline_block\
  \   AND a.timestamp < t.score_cutoff_timestamp\
  \   AND EXISTS (SELECT 1 FROM testnet_faucet_claims fc WHERE LOWER(fc.address) = LOWER(p.wallet)\
  \     AND LOWER(fc.token_address) = LOWER(t.usdc_address) AND fc.status = 'success'\
  \     AND fc.amount = t.starting_balance_usdc AND fc.tx_hash IS NOT NULL\
  \     AND fc.mint_block_number IS NOT NULL AND fc.mint_block_number < a.block_number)\
  \   ORDER BY a.block_number, a.tx_index, a.log_index LIMIT 1\
  \ ) allocation ON TRUE\
  \ LEFT JOIN LATERAL (\
  \   SELECT COUNT(*) AS unverified_count FROM perps_account_activity a\
  \   WHERE a.chain_id = t.chain_id AND a.release_router = t.release_router AND a.account = p.wallet\
  \   AND a.activity_type = 'Deposit' AND a.timestamp < t.score_cutoff_timestamp\
  \   AND NOT COALESCE((LOWER(COALESCE(a.contract_address, '')) = LOWER(t.margin_clearinghouse_address)\
  \     AND jsonb_exists(a.data, 'asset') AND LOWER(a.data->>'asset') = LOWER(t.usdc_address)\
  \     AND a.amount_usdc = t.starting_balance_usdc\
  \     AND EXISTS (SELECT 1 FROM testnet_faucet_claims fc WHERE LOWER(fc.address) = LOWER(p.wallet)\
  \       AND LOWER(fc.token_address) = LOWER(t.usdc_address) AND fc.status = 'success'\
  \       AND fc.amount = t.starting_balance_usdc AND NULLIF(BTRIM(fc.tx_hash), '') IS NOT NULL\
  \       AND fc.mint_block_number IS NOT NULL AND fc.mint_block_number < a.block_number)), FALSE)\
  \ ) provenance ON TRUE\
  \ LEFT JOIN LATERAL (\
  \   SELECT a.block_number AS trade_block, a.tx_index AS trade_tx, a.log_index AS trade_log FROM perps_account_activity a\
  \   WHERE a.chain_id = t.chain_id AND a.release_router = t.release_router AND a.account = p.wallet\
  \   AND a.activity_type IN ('Open', 'Close') AND COALESCE(a.size_delta, 0) <> 0\
  \   AND a.timestamp >= t.start_timestamp AND a.timestamp < t.score_cutoff_timestamp\
  \   ORDER BY a.block_number, a.tx_index, a.log_index LIMIT 1\
  \ ) tr ON TRUE\
  \ LEFT JOIN LATERAL (\
  \   SELECT COALESCE(MAX(running_net), 0) AS max_net_amount FROM (\
  \     SELECT SUM(CASE WHEN a.activity_type = 'Deposit' THEN a.amount_usdc ELSE -a.amount_usdc END)\
  \       OVER (ORDER BY a.block_number, a.tx_index, a.log_index) AS running_net\
  \     FROM perps_account_activity a WHERE a.chain_id = t.chain_id AND a.release_router = t.release_router\
  \     AND a.account = p.wallet AND a.activity_type IN ('Deposit', 'Withdraw')\
  \     AND LOWER(COALESCE(a.contract_address, '')) = LOWER(t.margin_clearinghouse_address)\
  \     AND jsonb_exists(a.data, 'asset') AND LOWER(a.data->>'asset') = LOWER(t.usdc_address)\
  \     AND a.timestamp < t.score_cutoff_timestamp\
  \   ) ordered_flows\
  \ ) cap ON TRUE\
  \ ), computed AS (\
  \ SELECT wallet, TO_JSONB(ARRAY_REMOVE(ARRAY[\
  \   CASE WHEN value_usdc IS NULL THEN 'baseline_unavailable' END,\
  \   CASE WHEN has_open_position THEN 'baseline_open_position' END,\
  \   CASE WHEN pending_order_count > 0 THEN 'baseline_pending_orders' END,\
  \   CASE WHEN value_usdc IS NOT NULL AND value_usdc NOT IN (0, starting_balance_usdc) THEN 'invalid_starting_bankroll' END,\
  \   CASE WHEN value_usdc = starting_balance_usdc AND baseline_official_count <> 1 THEN 'baseline_official_allocation_count_invalid' END,\
  \   CASE WHEN value_usdc = starting_balance_usdc AND baseline_official_count = 1\
  \     AND baseline_official_amount <> starting_balance_usdc THEN 'baseline_official_allocation_amount_invalid' END,\
  \   CASE WHEN value_usdc = starting_balance_usdc AND baseline_flow_net <> starting_balance_usdc\
  \     THEN 'baseline_funding_flow_mismatch' END,\
  \   CASE WHEN value_usdc = 0 AND baseline_official_count <> 0 THEN 'unexpected_prebaseline_official_allocation' END,\
  \   CASE WHEN value_usdc = 0 AND baseline_flow_net <> 0 THEN 'zero_baseline_funding_flow_mismatch' END,\
  \   CASE WHEN baseline_unverified_count > 0 THEN 'unverified_prebaseline_deposit_provenance' END,\
  \   CASE WHEN value_usdc = starting_balance_usdc AND official_count > 0 THEN 'unexpected_official_deposit' END,\
  \   CASE WHEN value_usdc = 0 AND official_count <> 1 THEN 'official_allocation_count_invalid' END,\
  \   CASE WHEN value_usdc = 0 AND official_count = 1 AND official_amount <> starting_balance_usdc THEN 'official_allocation_amount_invalid' END,\
  \   CASE WHEN value_usdc = 0 AND official_count = 1 AND trade_block IS NOT NULL\
  \     AND ROW(allocation_block, allocation_tx, allocation_log) >= ROW(trade_block, trade_tx, trade_log)\
  \     THEN 'official_allocation_not_before_trading' END,\
  \   CASE WHEN unverified_count > 0 OR any_unverified_count > 0 THEN 'unverified_deposit_provenance' END,\
  \   CASE WHEN max_net_amount > starting_balance_usdc THEN 'funding_capacity_exceeded' END\
  \ ]::TEXT[], NULL)) AS flags FROM facts\
  \ ) UPDATE insights_competition_participants p SET integrity_flags = c.flags, updated_at = NOW()\
  \ FROM computed c, target t WHERE p.competition_slug = t.slug AND p.wallet = c.wallet"

competitionSeedMetadataSelect :: Query
competitionSeedMetadataSelect =
  "SELECT slug, name, chain_id, release_router, usdc_address, margin_clearinghouse_address, account_lens_address, release_manifest,\
  \ start_timestamp, new_risk_cutoff_timestamp, score_cutoff_timestamp, results_timestamp,\
  \ payment_deadline_timestamp, starting_balance_usdc, minimum_profit_bps, minimum_active_days,\
  \ fx_session_boundary_utc_minutes, registration_close_timestamp, minimum_x_account_age_days, target_x_handle,\
  \ scoring_version, rules_version, first_prize_usdc, second_prize_usdc, third_prize_usdc\
  \ FROM insights_competitions"

competitionSelect :: Query
competitionSelect =
  "SELECT slug, name, chain_id, release_router, usdc_address, margin_clearinghouse_address, account_lens_address, start_timestamp, new_risk_cutoff_timestamp, score_cutoff_timestamp,\
  \ results_timestamp, payment_deadline_timestamp, registration_open_timestamp, registration_close_timestamp, minimum_x_account_age_days, target_x_handle, privacy_notice_version,\
  \ start_block, start_block_hash, score_cutoff_block, score_cutoff_block_hash,\
  \ starting_balance_usdc, minimum_profit_bps, minimum_active_days, fx_session_boundary_utc_minutes, scoring_version, rules_version,\
  \ first_prize_usdc, second_prize_usdc, third_prize_usdc, finalized,\
  \ EXTRACT(EPOCH FROM updated_at)::bigint,\
  \ (SELECT COUNT(*) FROM insights_competition_participants p WHERE p.competition_slug = insights_competitions.slug)::bigint\
  \ FROM insights_competitions"

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
  \ JOIN perps_indexer_state i ON i.chain_id = t.chain_id AND i.release_router = t.release_router\
  \   AND i.indexer_name = ('perps-history-costs-v1:' || t.release_router)\
  \   AND i.last_indexed_block_hash IS NOT NULL AND i.last_indexed_block >= b.block_number\
  \ WHERE b.snapshot_kind = 'start' AND t.start_block IS NOT NULL AND b.block_number = t.start_block - 1\
  \ AND (LOWER(b.block_hash) = LOWER(t.start_snapshot_block_hash)\
  \   OR (t.slug = 'testnet-trading-2026' AND t.start_snapshot_block_hash IS NULL))\
  \ AND LOWER(b.account_lens_address) = LOWER(t.account_lens_address)\
  \ AND b.participant_count = (SELECT COUNT(*) FROM insights_competition_participants p WHERE p.competition_slug = t.slug)\
  \ ORDER BY b.block_number DESC, b.published_at DESC LIMIT 1\
  \ ), current_batch AS (\
  \ SELECT b.* FROM insights_snapshot_batches b JOIN target t ON t.slug = b.competition_slug\
  \ JOIN perps_indexer_state i ON i.chain_id = t.chain_id AND i.release_router = t.release_router\
  \   AND i.indexer_name = ('perps-history-costs-v1:' || t.release_router)\
  \   AND i.last_indexed_block_hash IS NOT NULL AND i.last_indexed_block >= b.block_number\
  \ WHERE b.snapshot_kind IN ('live', 'final') AND b.timestamp < t.score_cutoff_timestamp\
  \ AND LOWER(b.account_lens_address) = LOWER(t.account_lens_address)\
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
  \   OR (t.slug = 'testnet-trading-2026' AND NOT jsonb_exists(a.data, 'asset')))\
  \ AND (t.start_block IS NULL OR a.block_number >= t.start_block)\
  \ AND a.block_number <= cb.block_number\
  \ GROUP BY a.account\
  \ ), activity_stats AS (\
  \ SELECT a.account AS wallet,\
  \ COUNT(DISTINCT (((to_timestamp(a.timestamp) AT TIME ZONE 'UTC')\
  \   + MOD(1440 - t.fx_session_boundary_utc_minutes, 1440) * INTERVAL '1 minute')::date))\
  \   FILTER (WHERE a.activity_type IN ('Open', 'Close') AND COALESCE(a.size_delta, 0) <> 0\
  \     AND EXTRACT(ISODOW FROM ((to_timestamp(a.timestamp) AT TIME ZONE 'UTC')\
  \       + MOD(1440 - t.fx_session_boundary_utc_minutes, 1440) * INTERVAL '1 minute')) BETWEEN 1 AND 5) AS active_days,\
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
  \ SELECT p.wallet, p.alias, p.eligibility_status, p.eligibility_reason, p.integrity_flags,\
  \ t.slug AS competition_slug,\
  \ t.starting_balance_usdc AS competition_starting_balance_usdc,\
  \ t.minimum_profit_bps AS competition_minimum_profit_bps,\
  \ t.minimum_active_days AS competition_minimum_active_days,\
  \ CASE WHEN ss.wallet IS NULL THEN NULL ELSE GREATEST(0,\
  \   CASE WHEN ss.has_open_position THEN ss.signed_net_equity_usdc ELSE ss.terminal_reachable_usdc END + ss.trader_claims_usdc) END AS starting_value_usdc,\
  \ CASE WHEN cs.wallet IS NULL THEN NULL ELSE GREATEST(0,\
  \   CASE WHEN cs.has_open_position THEN cs.signed_net_equity_usdc ELSE cs.terminal_reachable_usdc END + cs.trader_claims_usdc) END AS current_value_usdc,\
  \ COALESCE(f.deposits_usdc, 0) AS deposits_usdc, COALESCE(f.withdrawals_usdc, 0) AS withdrawals_usdc,\
  \ CASE WHEN t.slug = 'testnet-trading-2026' THEN COALESCE(adj.amount_usdc, 0) ELSE 0 END AS adjustment_usdc,\
  \ COALESCE(ast.active_days, 0) AS active_days,\
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
  \ SELECT scored.*, CASE WHEN final_pnl_usdc IS NULL\
  \   OR (competition_slug = 'testnet-trading-2026' AND executed_trades = 0) THEN NULL\
  \ ELSE RANK() OVER (ORDER BY CASE WHEN competition_slug = 'testnet-trading-2026'\
  \   AND executed_trades = 0 THEN 1 ELSE 0 END, final_pnl_usdc DESC NULLS LAST) END AS competition_rank\
  \ FROM scored\
  \ ), prize_candidates AS (\
  \ SELECT wallet, RANK() OVER (ORDER BY final_pnl_usdc DESC) AS prize_place,\
  \ COUNT(*) OVER (PARTITION BY final_pnl_usdc) AS prize_tie_count\
  \ FROM ranked WHERE final_pnl_usdc IS NOT NULL AND eligibility_status = 'eligible'\
  \ AND jsonb_array_length(integrity_flags) = 0\
  \ AND final_pnl_usdc >= competition_starting_balance_usdc * competition_minimum_profit_bps / 10000\
  \ AND active_days >= competition_minimum_active_days\
  \ ), with_prizes AS (\
  \ SELECT ranked.*, CASE WHEN pc.prize_place <= 3 THEN pc.prize_place ELSE NULL END AS prize_place,\
  \ CASE WHEN pc.prize_place <= 3 THEN pc.prize_tie_count ELSE NULL END AS prize_tie_count\
  \ FROM ranked LEFT JOIN prize_candidates pc ON pc.wallet = ranked.wallet\
  \ )\
  \ SELECT competition_rank, prize_place, prize_tie_count, wallet, alias, eligibility_status, eligibility_reason,\
  \ jsonb_array_length(integrity_flags) = 0 AS funding_integrity_clear, final_pnl_usdc,\
  \ CASE WHEN final_pnl_usdc IS NULL OR competition_starting_balance_usdc = 0 THEN NULL\
  \ ELSE TRUNC(final_pnl_usdc * 10000 / competition_starting_balance_usdc)::bigint END AS roi_bps,\
  \ starting_value_usdc, current_value_usdc, deposits_usdc, withdrawals_usdc, adjustment_usdc,\
  \ active_days, volume_usdc, executed_trades, liquidations, realized_pnl_usdc, block_number, timestamp, has_open_position, snapshot_kind,\
  \ position_side, position_size_delta, position_margin_usdc, position_entry_price,\
  \ position_unrealized_pnl_usdc, position_liquidatable\
  \ FROM with_prizes"

leaderboardQuerySql :: Query
leaderboardQuerySql = leaderboardQuery

finalizedStandingsSelect :: Query
finalizedStandingsSelect =
  "SELECT competition_rank, prize_place, prize_tie_count, wallet, alias, eligibility_status, eligibility_reason,\
  \ funding_integrity_clear, final_pnl_usdc, roi_bps, starting_value_usdc, current_value_usdc,\
  \ deposits_usdc, withdrawals_usdc, adjustment_usdc, active_days, volume_usdc, executed_trades,\
  \ liquidations, realized_pnl_usdc, block_number, timestamp, has_open_position, snapshot_kind,\
  \ position_side, position_size_delta, position_margin_usdc, position_entry_price,\
  \ position_unrealized_pnl_usdc, position_liquidatable FROM insights_finalized_standings"

materializeFinalizedStandings :: Connection -> Text -> IO (Either Text Integer)
materializeFinalizedStandings conn slug = do
  counts <- query conn
    "SELECT (SELECT COUNT(*) FROM insights_competition_participants WHERE competition_slug = ?),\
    \ (SELECT COUNT(*) FROM insights_finalized_standings WHERE competition_slug = ?)"
    (slug, slug) :: IO [(Integer, Integer)]
  case counts of
    [(participantCount, existingCount)]
      | participantCount <= 0 -> pure $ Left "cannot materialize empty competition standings"
      | existingCount == participantCount -> validateComplete participantCount
      | existingCount /= 0 -> pure $ Left "finalized standings are partially materialized"
      | otherwise -> do
          inserted <- execute conn
            ("INSERT INTO insights_finalized_standings (competition_slug, competition_rank, prize_place, prize_tie_count,\
             \ wallet, alias, eligibility_status, eligibility_reason, funding_integrity_clear, final_pnl_usdc, roi_bps,\
             \ starting_value_usdc, current_value_usdc, deposits_usdc, withdrawals_usdc, adjustment_usdc, active_days,\
             \ volume_usdc, executed_trades, liquidations, realized_pnl_usdc, block_number, timestamp, has_open_position,\
             \ snapshot_kind, position_side, position_size_delta, position_margin_usdc, position_entry_price,\
             \ position_unrealized_pnl_usdc, position_liquidatable) SELECT ?, standings.* FROM ("
              <> leaderboardQuery
              <> ") standings")
            (slug, slug)
          if toInteger inserted == participantCount
            then validateComplete participantCount >>= \case
              success@(Right _) -> pure success
              failure@(Left _) -> do
                _ <- execute conn "DELETE FROM insights_finalized_standings WHERE competition_slug = ?" (Only slug)
                pure failure
            else do
              _ <- execute conn "DELETE FROM insights_finalized_standings WHERE competition_slug = ?" (Only slug)
              pure $ Left "canonical standings did not cover the complete participant roster"
    _ -> pure $ Left "competition standings materialization state is ambiguous"
  where
    validateComplete expectedCount = do
      rows <- query conn
        "SELECT COUNT(*) FROM insights_finalized_standings s\
        \ JOIN insights_competitions c ON c.slug = s.competition_slug\
        \ WHERE s.competition_slug = ? AND s.final_pnl_usdc IS NOT NULL\
        \ AND s.starting_value_usdc IS NOT NULL AND s.current_value_usdc IS NOT NULL\
        \ AND s.block_number IS NOT NULL AND s.timestamp IS NOT NULL\
        \ AND s.snapshot_kind = 'final' AND s.block_number = c.score_cutoff_block\
        \ AND c.score_cutoff_block_hash IS NOT NULL"
        (Only slug) :: IO [Only Integer]
      pure $ case rows of
        [Only completeCount]
          | completeCount == expectedCount -> Right expectedCount
        _ -> Left "canonical standings contain missing or non-final score evidence"

leaderboardOrderBy :: Query
leaderboardOrderBy =
  " ORDER BY CASE WHEN competition_slug = 'testnet-trading-2026' AND executed_trades = 0 THEN 1 ELSE 0 END,\
  \ final_pnl_usdc DESC NULLS LAST, wallet ASC LIMIT ? OFFSET ?"

leaderboardOrderBySql :: Query
leaderboardOrderBySql = leaderboardOrderBy

walletActivityQuery :: Query
walletActivityQuery =
  "WITH target AS (\
  \ SELECT * FROM insights_competitions WHERE slug = ?\
  \ ), current_batch AS (\
  \ SELECT b.* FROM insights_snapshot_batches b JOIN target t ON t.slug = b.competition_slug\
  \ JOIN perps_indexer_state i ON i.chain_id = t.chain_id AND i.release_router = t.release_router\
  \   AND i.indexer_name = ('perps-history-costs-v1:' || t.release_router)\
  \   AND i.last_indexed_block_hash IS NOT NULL AND i.last_indexed_block >= b.block_number\
  \ WHERE b.snapshot_kind IN ('live', 'final') AND b.timestamp < t.score_cutoff_timestamp\
  \ AND LOWER(b.account_lens_address) = LOWER(t.account_lens_address)\
  \ AND b.participant_count = (SELECT COUNT(*) FROM insights_competition_participants p WHERE p.competition_slug = t.slug)\
  \ AND (t.score_cutoff_block IS NULL OR b.block_number <= t.score_cutoff_block)\
  \ AND (NOT t.finalized OR (b.snapshot_kind = 'final' AND b.block_number = t.score_cutoff_block\
  \   AND LOWER(b.block_hash) = LOWER(t.score_cutoff_block_hash)))\
  \ ORDER BY b.block_number DESC, CASE WHEN b.snapshot_kind = 'final' THEN 0 ELSE 1 END, b.published_at DESC LIMIT 1\
  \ )\
  \ SELECT a.activity_type, a.side, a.price, a.size_delta, a.amount_usdc, a.pnl_usdc,\
  \ (a.data->>'executionFeeUsdc')::numeric, (a.data->>'vpiUsdc')::numeric,\
  \ a.tx_hash, a.block_number, a.timestamp, a.log_index,\
  \ CASE WHEN EXTRACT(ISODOW FROM ((to_timestamp(a.timestamp) AT TIME ZONE 'UTC')\
  \   + MOD(1440 - c.fx_session_boundary_utc_minutes, 1440) * INTERVAL '1 minute')) BETWEEN 1 AND 5\
  \ THEN (((to_timestamp(a.timestamp) AT TIME ZONE 'UTC')\
  \   + MOD(1440 - c.fx_session_boundary_utc_minutes, 1440) * INTERVAL '1 minute')::date)::text ELSE NULL END\
  \ FROM perps_account_activity a JOIN target c ON c.chain_id = a.chain_id AND c.release_router = a.release_router\
  \ CROSS JOIN current_batch b\
  \ WHERE a.account = ? AND a.timestamp >= c.start_timestamp AND a.timestamp < c.score_cutoff_timestamp\
  \ AND (c.start_block IS NULL OR a.block_number >= c.start_block) AND a.block_number <= b.block_number\
  \ ORDER BY a.block_number DESC, a.log_index DESC LIMIT ?"

walletActivityQuerySql :: Query
walletActivityQuerySql = walletActivityQuery

finalizationReadinessQuery :: Query
finalizationReadinessQuery =
  "SELECT c.finalized, c.score_cutoff_timestamp, c.results_timestamp, c.start_block, c.start_block_hash, c.score_cutoff_block,\
  \ (SELECT COUNT(*) FROM insights_competition_participants p WHERE p.competition_slug = c.slug),\
  \ (SELECT COUNT(*) FROM insights_competition_participants p WHERE p.competition_slug = c.slug\
  \   AND NULLIF(BTRIM(p.trader_reference), '') IS NULL),\
  \ (SELECT COUNT(*) FROM insights_competition_participants p WHERE p.competition_slug = c.slug\
  \   AND (p.eligibility_status NOT IN ('eligible', 'ineligible')\
  \     OR (p.eligibility_status = 'eligible' AND jsonb_array_length(p.integrity_flags) > 0))),\
  \ (SELECT COUNT(DISTINCT s.wallet) FROM insights_competition_participants p\
  \   JOIN insights_account_snapshots s ON s.competition_slug = p.competition_slug AND s.wallet = p.wallet\
  \   JOIN insights_snapshot_batches b ON b.competition_slug = s.competition_slug\
  \     AND b.snapshot_kind = s.snapshot_kind AND b.block_number = s.block_number\
  \     AND LOWER(b.block_hash) = LOWER(s.block_hash)\
  \   WHERE p.competition_slug = c.slug AND b.snapshot_kind = 'start'\
  \     AND c.start_block IS NOT NULL AND b.block_number = c.start_block - 1\
  \     AND c.start_block_hash IS NOT NULL AND c.start_snapshot_block_hash IS NOT NULL\
  \     AND LOWER(b.block_hash) = LOWER(c.start_snapshot_block_hash)\
  \     AND EXISTS (SELECT 1 FROM perps_indexer_state i WHERE i.chain_id = c.chain_id\
  \       AND i.release_router = c.release_router\
  \       AND i.indexer_name = ('perps-history-costs-v1:' || c.release_router)\
  \       AND i.last_indexed_block_hash IS NOT NULL AND i.last_indexed_block >= b.block_number)\
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
  \     AND EXISTS (SELECT 1 FROM perps_indexer_state i WHERE i.chain_id = c.chain_id\
  \       AND i.release_router = c.release_router\
  \       AND i.indexer_name = ('perps-history-costs-v1:' || c.release_router)\
  \       AND i.last_indexed_block_hash IS NOT NULL AND i.last_indexed_block >= b.block_number)\
  \     AND b.chain_id = c.chain_id AND b.release_router = c.release_router\
  \     AND LOWER(b.account_lens_address) = LOWER(c.account_lens_address)\
  \     AND s.chain_id = b.chain_id AND s.release_router = b.release_router\
  \     AND b.participant_count = (SELECT COUNT(*) FROM insights_competition_participants p0 WHERE p0.competition_slug = c.slug)),\
  \ (SELECT COUNT(DISTINCT b.block_hash) FROM insights_snapshot_batches b\
  \   WHERE b.competition_slug = c.slug AND b.snapshot_kind = 'final'\
  \     AND b.block_number = c.score_cutoff_block AND LOWER(b.block_hash) = LOWER(c.score_cutoff_block_hash)\
  \     AND EXISTS (SELECT 1 FROM perps_indexer_state i WHERE i.chain_id = c.chain_id\
  \       AND i.release_router = c.release_router\
  \       AND i.indexer_name = ('perps-history-costs-v1:' || c.release_router)\
  \       AND i.last_indexed_block_hash IS NOT NULL AND i.last_indexed_block >= b.block_number)\
  \     AND LOWER(b.account_lens_address) = LOWER(c.account_lens_address)),\
  \ (SELECT MIN(b.block_hash) FROM insights_snapshot_batches b\
  \   WHERE b.competition_slug = c.slug AND b.snapshot_kind = 'final'\
  \     AND b.block_number = c.score_cutoff_block AND LOWER(b.block_hash) = LOWER(c.score_cutoff_block_hash)\
  \     AND EXISTS (SELECT 1 FROM perps_indexer_state i WHERE i.chain_id = c.chain_id\
  \       AND i.release_router = c.release_router\
  \       AND i.indexer_name = ('perps-history-costs-v1:' || c.release_router)\
  \       AND i.last_indexed_block_hash IS NOT NULL AND i.last_indexed_block >= b.block_number)\
  \     AND LOWER(b.account_lens_address) = LOWER(c.account_lens_address))\
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
epoch = floor . utcTimeToPOSIXSeconds

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
