-- Counts remain in PostgreSQL: no wallet or order identifiers leave this module.
module Plether.Keeper.Reliability
  ( OrderReliability (..)
  , readOrderReliability
  , runOrderReliabilityObserver
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeAsyncException, SomeException, fromException, throwIO, try)
import Control.Monad (forever, forM_)
import Data.Text (Text)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Database.PostgreSQL.Simple (Connection, query)
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import Plether.Database.Diagnostics (withDiagnosticConnection)
import qualified Plether.Logging as Log

data OrderReliability = OrderReliability
  { windowSeconds :: Integer
  , totalOrders :: Integer
  , executedOrders :: Integer
  , expiredOrders :: Integer
  , otherFailedOrders :: Integer
  , pendingOrders :: Integer
  , activeAccounts :: Integer
  , affectedAccounts :: Integer
  , repeatAffectedAccounts :: Integer
  } deriving stock (Eq, Show)

instance FromRow OrderReliability where
  fromRow = OrderReliability <$> field <*> field <*> field <*> field <*> field
    <*> field <*> field <*> field <*> field

-- Half-open commitment cohorts end two minutes before observation, allowing
-- normal settlement time. Pending rows remain explicit, never treated as success.
-- Router scoping prevents mixing deployments whose order IDs overlap.
readOrderReliability :: Connection -> Text -> Integer -> IO [OrderReliability]
readOrderReliability conn router cohortEnd = query conn
  "WITH cohort AS MATERIALIZED (\
  \ SELECT lower(account) AS account, commit_time, status, failure_reason\
  \ FROM perps_keeper_orders WHERE order_router=? AND commit_time>=?-86400 AND commit_time<?),\
  \ windows(seconds) AS (VALUES (3600::bigint),(86400::bigint)),\
  \ accounts AS (SELECT w.seconds,c.account,count(*) AS orders,\
  \ count(*) FILTER (WHERE status='executed') AS executed,\
  \ count(*) FILTER (WHERE status='failed' AND failure_reason=2) AS expired,\
  \ count(*) FILTER (WHERE status='failed' AND failure_reason IS DISTINCT FROM 2) AS other_failed,\
  \ count(*) FILTER (WHERE status NOT IN ('executed','failed')) AS pending\
  \ FROM windows w JOIN cohort c ON c.commit_time>=?-w.seconds\
  \ GROUP BY w.seconds,c.account)\
  \ SELECT w.seconds,coalesce(sum(a.orders),0)::bigint,coalesce(sum(a.executed),0)::bigint,\
  \ coalesce(sum(a.expired),0)::bigint,coalesce(sum(a.other_failed),0)::bigint,\
  \ coalesce(sum(a.pending),0)::bigint,count(a.account),\
  \ count(*) FILTER (WHERE a.expired>0),count(*) FILTER (WHERE a.expired>1)\
  \ FROM windows w LEFT JOIN accounts a ON a.seconds=w.seconds\
  \ GROUP BY w.seconds ORDER BY w.seconds"
  (router, cohortEnd, cohortEnd, cohortEnd)

-- Independent, short-lived read-only connections and bounded SQL keep reporting
-- off both the keeper execution connection and its application connection pool.
-- Only the keeper holding the session lock starts this observer.
runOrderReliabilityObserver :: Text -> Text -> IO ()
runOrderReliabilityObserver databaseUrl router = forever $ do
  result <- try @SomeException $ do
    cohortEnd <- subtract 120 . floor <$> getPOSIXTime
    snapshot <- withDiagnosticConnection databaseUrl $ \conn ->
      readOrderReliability conn router cohortEnd
    case snapshot of
      Nothing -> unavailable
      Just rows -> forM_ rows $ \s ->
        Log.logInfo "keeper_order_reliability_snapshot" "Canonical order reliability aggregates"
          [ Log.field "window_seconds" $ windowSeconds s
          , Log.field "cohort_end_unix" cohortEnd
          , Log.field "total_orders" $ totalOrders s
          , Log.field "executed_orders" $ executedOrders s
          , Log.field "expired_orders" $ expiredOrders s
          , Log.field "other_failed_orders" $ otherFailedOrders s
          , Log.field "pending_orders" $ pendingOrders s
          , Log.field "active_accounts" $ activeAccounts s
          , Log.field "affected_accounts" $ affectedAccounts s
          , Log.field "repeat_affected_accounts" $ repeatAffectedAccounts s
          ]
  case result of
    Left exception -> case fromException exception :: Maybe SomeAsyncException of
      Just _ -> throwIO exception
      Nothing -> unavailable
    Right () -> pure ()
  threadDelay 60_000_000
 where
  -- Exception text may contain database credentials or bind parameters.
  unavailable = Log.logWarn "keeper_order_reliability_unavailable" "Order reliability snapshot unavailable" []
