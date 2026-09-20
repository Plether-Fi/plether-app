module Plether.LiquidationWorker.Monitoring
  ( BacklogHealth (..)
  , ensureMonitoringSchema
  , observeRisk
  , recordConfirmedProgress
  , readBacklogHealth
  , emitBacklogHealth
  , runBacklogWatchdog
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeAsyncException, SomeException, fromException, throwIO, try)
import Control.Monad (forever, unless, void)
import Data.Text (Text)
import qualified Data.Text as T
import Database.PostgreSQL.Simple (Connection, In (..), execute, execute_, query)
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import Plether.Database (DbPool, withDb)
import qualified Plether.Logging as Log

-- Risk includes the conservative preflight buffer. A skipped-solvent receipt
-- resolves it. Unknown reads never erase an outstanding risk observation.
data BacklogHealth = BacklogHealth
  { bhRiskCount :: Int
  , bhOldestRiskSeconds :: Double
  , bhWithoutProgressSeconds :: Double
  , bhOldestUncheckedSeconds :: Double
  , bhPendingCount :: Int
  } deriving stock (Show, Eq)

instance FromRow BacklogHealth where
  fromRow = BacklogHealth <$> field <*> field <*> field <*> field <*> field

ensureMonitoringSchema :: Connection -> IO ()
ensureMonitoringSchema conn = do
  void $ execute_ conn
    "ALTER TABLE perps_liquidation_candidates \
    \ADD COLUMN IF NOT EXISTS monitoring_first_seen_at TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp(), \
    \ADD COLUMN IF NOT EXISTS risk_first_observed_at TIMESTAMPTZ, \
    \ADD COLUMN IF NOT EXISTS risk_checked_at TIMESTAMPTZ"
  void $ execute_ conn
    "ALTER TABLE perps_liquidation_state \
    \ADD COLUMN IF NOT EXISTS last_liquidation_progress_at TIMESTAMPTZ, \
    \ADD COLUMN IF NOT EXISTS last_liquidation_progress_block BIGINT"

-- Bulk updates preserve first detection across scans, failures and restarts.
-- Callers pass Nothing for unknown observations, which must not reset age.
observeRisk :: Connection -> Integer -> Text -> [(Text, Maybe Bool)] -> IO ()
observeRisk conn chainId engine observations = do
  let risky = [T.toLower account | (account, Just True) <- observations]
      resolved = [T.toLower account | (account, Just False) <- observations]
  unless (null risky) $ void $ execute conn
    "UPDATE perps_liquidation_candidates SET \
    \risk_first_observed_at = COALESCE(risk_first_observed_at, clock_timestamp()), \
    \risk_checked_at = clock_timestamp() \
    \WHERE chain_id = ? AND cfd_engine = ? AND account IN ?"
    (chainId, T.toLower engine, In risky)
  unless (null resolved) $ void $ execute conn
    "UPDATE perps_liquidation_candidates SET risk_first_observed_at = NULL, risk_checked_at = clock_timestamp() \
    \WHERE chain_id = ? AND cfd_engine = ? AND account IN ?"
    (chainId, T.toLower engine, In resolved)

-- Only reconciled receipts with matching PositionLiquidated events call this.
-- Re-reading the same/older receipt cannot manufacture recent progress.
recordConfirmedProgress :: Connection -> Integer -> Text -> Integer -> IO ()
recordConfirmedProgress conn chainId engine blockNumber = void $ execute conn
  "INSERT INTO perps_liquidation_state \
  \(chain_id, cfd_engine, last_liquidation_progress_at, last_liquidation_progress_block) \
  \VALUES (?, ?, clock_timestamp(), ?) ON CONFLICT (chain_id, cfd_engine) DO UPDATE SET \
  \last_liquidation_progress_at = EXCLUDED.last_liquidation_progress_at, \
  \last_liquidation_progress_block = EXCLUDED.last_liquidation_progress_block \
  \WHERE perps_liquidation_state.last_liquidation_progress_block IS NULL \
  \OR perps_liquidation_state.last_liquidation_progress_block < EXCLUDED.last_liquidation_progress_block"
  (chainId, T.toLower engine, blockNumber)

readBacklogHealth :: Connection -> Integer -> Text -> IO BacklogHealth
readBacklogHealth conn chainId engine = do
  rows <- query conn
    "WITH candidates AS (SELECT * FROM perps_liquidation_candidates WHERE chain_id = ? AND cfd_engine = ?), \
    \ages AS (SELECT count(*) FILTER (WHERE risk_first_observed_at IS NOT NULL) AS risk_count, \
    \min(risk_first_observed_at) AS oldest_risk, \
    \min(COALESCE(risk_checked_at, monitoring_first_seen_at)) AS oldest_check, \
    \count(*) FILTER (WHERE pending_tx_hash IS NOT NULL) AS pending_count FROM candidates), \
    \progress AS (SELECT max(last_liquidation_progress_at) AS last_progress FROM perps_liquidation_state \
    \WHERE chain_id = ? AND cfd_engine = ?) \
    \SELECT risk_count, \
    \GREATEST(0, COALESCE(EXTRACT(EPOCH FROM clock_timestamp() - oldest_risk), 0))::double precision, \
    \CASE WHEN risk_count = 0 THEN 0 ELSE \
    \GREATEST(0, EXTRACT(EPOCH FROM clock_timestamp() - GREATEST(oldest_risk, COALESCE(last_progress, oldest_risk)))) END::double precision, \
    \GREATEST(0, COALESCE(EXTRACT(EPOCH FROM clock_timestamp() - oldest_check), 0))::double precision, \
    \pending_count FROM ages CROSS JOIN progress"
    (chainId, T.toLower engine, chainId, T.toLower engine)
  case rows of
    [health] -> pure health
    _ -> fail "Unexpected liquidation monitoring aggregate result"

emitBacklogHealth :: Integer -> Text -> BacklogHealth -> IO ()
emitBacklogHealth chainId engine health =
  Log.logInfo "liquidation_backlog_health" "Durable liquidation backlog and progress snapshot"
    [ Log.field "chain_id" chainId
    , Log.field "cfd_engine" engine
    , Log.field "risk_backlog_count" $ bhRiskCount health
    , Log.field "oldest_risk_seconds" $ bhOldestRiskSeconds health
    , Log.field "seconds_without_progress" $ bhWithoutProgressSeconds health
    , Log.field "oldest_unchecked_seconds" $ bhOldestUncheckedSeconds health
    , Log.field "pending_account_count" $ bhPendingCount health
    , Log.field "heartbeat" (1 :: Int)
    ]

-- Separate DB lease and thread: waits in the executor cannot hide a stall.
-- If this process/DB itself stops reporting, CloudWatch alarms on missing heartbeats.
runBacklogWatchdog :: DbPool -> Integer -> Text -> IO ()
runBacklogWatchdog pool chainId engine = forever $ do
  outcome <- try $ withDb pool $ \conn -> readBacklogHealth conn chainId engine >>= emitBacklogHealth chainId engine
  case outcome of
    Right () -> pure ()
    Left err -> case fromException (err :: SomeException) :: Maybe SomeAsyncException of
      Just _ -> throwIO err
      Nothing -> Log.logError "liquidation_monitor_unavailable" "Cannot read liquidation backlog health"
        [Log.field "chain_id" chainId, Log.field "cfd_engine" engine]
  threadDelay 10_000_000
