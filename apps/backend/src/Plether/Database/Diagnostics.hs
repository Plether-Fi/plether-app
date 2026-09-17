module Plether.Database.Diagnostics
  ( startDbDiagnostics
  , diagnosticConnectionString
  , readBlockingSnapshot
  , withDiagnosticConnection
  , snapshotValue
  ) where

import Control.Concurrent (ThreadId, forkIO, threadDelay)
import Control.Exception (SomeAsyncException, SomeException, bracket, fromException, throwIO, try)
import Control.Monad (void, when)
import Data.Aeson (Value, object, (.=))
import qualified Data.ByteString.Char8 as BS
import Data.Text (Text)
import qualified Data.Text as T
import Database.PostgreSQL.Simple (Connection, Only (..), close, connectPostgreSQL, execute_, query, query_)
import Database.PostgreSQL.Simple.Types (PGArray (..))
import Plether.Database (DbPool, dbPoolObservation)
import Plether.Database.Observation
import Plether.Logging (field, logInfo, logWarn, logWarnEvery)
import System.Timeout (timeout)

-- Override libpq's connection timeout without rendering the credential anywhere.
diagnosticConnectionString :: Text -> Text
diagnosticConnectionString original
  | "postgresql://" `T.isPrefixOf` original || "postgres://" `T.isPrefixOf` original =
      original <> (if "?" `T.isInfixOf` original then "&" else "?") <> "connect_timeout=2"
  | otherwise = original <> " connect_timeout=2"

snapshotValue :: PoolSnapshot -> Value
snapshotValue s = object
  [ "waiting" .= psWaiting s
  , "in_use" .= length (psActive s)
  , "acquired" .= psAcquired s
  , "released" .= psReleased s
  , "failures" .= psFailures s
  , "abandoned_waits" .= psAbandoned s
  , "wait_total_ms" .= psWaitTotalMs s
  , "hold_total_ms" .= psHoldTotalMs s
  , "wait_max_ms" .= psWaitMaxMs s
  , "hold_max_ms" .= psHoldMaxMs s
  , "slowest_wait_source" .= psSlowestWaitSource s
  , "slowest_hold_source" .= psSlowestHoldSource s
  , "active" .= map activeValue (take 10 $ psActive s)
  ]
  where
    activeValue a = object
      [ "source" .= acSource a, "backend_pid" .= acBackendPid a, "held_ms" .= acHeldMs a ]

-- This deliberately does not select query text, role names, client addresses,
-- application_name (which clients can choose), or bind parameters.
readBlockingSnapshot :: Connection -> [Int] -> IO Value
readBlockingSnapshot conn ownPids = do
  visibility <- query_ conn
    "SELECT pg_has_role(current_user, 'pg_read_all_stats', 'USAGE')"
    :: IO [Only Bool]
  rows <- query conn
    "WITH activity AS (\
    \ SELECT pid, state, wait_event_type, wait_event,\
    \ GREATEST(0, EXTRACT(EPOCH FROM (clock_timestamp()-query_start))*1000)::double precision AS query_age_ms,\
    \ GREATEST(0, EXTRACT(EPOCH FROM (clock_timestamp()-xact_start))*1000)::double precision AS transaction_age_ms,\
    \ pg_blocking_pids(pid) AS blockers\
    \ FROM pg_stat_activity WHERE datname=current_database()\
    \ AND backend_type='client backend' AND pid<>pg_backend_pid())\
    \ SELECT pid,state,wait_event_type,wait_event,query_age_ms,transaction_age_ms,blockers\
    \ FROM activity WHERE cardinality(blockers)>0\
    \ OR pid IN (SELECT unnest(blockers) FROM activity)\
    \ OR state LIKE 'idle in transaction%'\
    \ OR (state='active' AND query_age_ms>=1000) OR pid=ANY(?)\
    \ ORDER BY (pid=ANY(?)) DESC, cardinality(blockers) DESC, transaction_age_ms DESC, pid LIMIT 21"
    (PGArray ownPids, PGArray ownPids)
    :: IO [(Int, Maybe Text, Maybe Text, Maybe Text, Double, Double, PGArray Int)]
  pure $ object
    [ "cross_role_visibility" .= (visibility == [Only True])
    , "truncated" .= (length rows > 20)
    , "sessions" .= map sessionValue (take 20 rows)
    ]
  where
    sessionValue (pid, state, waitType, waitEvent, queryAge, transactionAge, PGArray blockers) = object
      [ "backend_pid" .= pid, "state" .= state, "wait_event_type" .= waitType
      , "wait_event" .= waitEvent, "query_age_ms" .= queryAge
      , "transaction_age_ms" .= transactionAge, "blocking_pids" .= take 20 blockers
      , "blocking_pids_truncated" .= (length blockers > 20)
      ]

-- This helper is also exercised against PostgreSQL in integration tests. All
-- settings are confined to this short-lived connection, never the request pool.
withDiagnosticConnection :: Text -> (Connection -> IO a) -> IO (Maybe a)
withDiagnosticConnection connectionString action = timeout 3_000_000 $
  bracket
    (connectPostgreSQL $ BS.pack $ T.unpack $ diagnosticConnectionString connectionString)
    close
    $ \conn -> do
      void $ execute_ conn "SET statement_timeout='1500ms'"
      void $ execute_ conn "SET lock_timeout='500ms'"
      void $ execute_ conn "SET default_transaction_read_only=on"
      action conn

-- One API-owned observer. It never checks out an application-pool connection,
-- never changes that pool's session settings, and never kills blocking queries.
-- Normally it emits once per minute; under pressure it emits every five seconds.
startDbDiagnostics :: DbPool -> Text -> IO ThreadId
startDbDiagnostics pool connectionString = forkIO $ loop (0 :: Int)
  where
    observation = dbPoolObservation pool
    loop ticks = do
      threadDelay 5_000_000
      result <- try @SomeException $ do
        snapshot <- readPoolSnapshot observation
        let pressure = poolUnderPressure snapshot
            emitNow = pressure || ticks >= 11
        when emitNow $ do
          drained <- drainPoolSnapshot observation
          (if pressure then logWarn else logInfo)
            "db_pool_observation" "Database connection pool observation"
            [field "pool" ("api" :: Text), field "pressure" pressure, field "observation" $ snapshotValue drained]
          capture drained
        pure emitNow
      case result of
        Right emitted -> loop $ if emitted then 0 else ticks + 1
        Left exception -> do
          rethrowAsync exception
          -- Exception text can include the database URL or SQL. Never emit it.
          logWarnEvery 60 "db_diagnostics_failed" "Database diagnostics unavailable" []
          loop 0
    capture snapshot = do
      result <- withDiagnosticConnection connectionString $ \conn ->
        readBlockingSnapshot conn $ map acBackendPid $ psActive snapshot
      case result of
        Nothing -> logWarnEvery 60 "db_diagnostics_timeout" "Database diagnostics reached their deadline" []
        Just blocking -> logInfo "db_blocking_snapshot" "Database blocking and wait snapshot"
          [field "pool" ("api" :: Text), field "snapshot" blocking]

rethrowAsync :: SomeException -> IO ()
rethrowAsync exception = case fromException exception :: Maybe SomeAsyncException of
  Just _ -> throwIO exception
  Nothing -> pure ()
