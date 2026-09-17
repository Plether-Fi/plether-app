module Plether.Database
  ( DbPool
  , newDbPool
  , newApiDbPool
  , newRegistrationDbPool
  , newOracleDbPool
  , dbPoolName
  , DbDeadline (..)
  , destroyDbPool
  , dbPoolObservation
  , withDb
  , withDbAdvisoryLock
  ) where

import Control.Exception (Exception, bracket_, bracketOnError, mask, onException, throwIO)
import Control.Monad (void)
import Data.Pool (Pool, newPool, defaultPoolConfig, withResource, destroyAllResources, takeResource, putResource, destroyResource)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import Database.PostgreSQL.Simple (Connection, Only (..), close, connectPostgreSQL, query, execute_)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stack (HasCallStack, callStack, getCallStack, SrcLoc (..))
import Plether.Database.Observation (Observation, newObservation, observeResource)
import System.Timeout (timeout)

data DbPool = DbPool (Pool (Connection, Int)) Observation Text Bool

data DbDeadline = DbAcquireDeadline | DbOperationDeadline deriving stock (Show, Eq)
instance Exception DbDeadline

dbPoolName :: DbPool -> Text
dbPoolName (DbPool _ _ name _) = name

dbPoolObservation :: DbPool -> Observation
dbPoolObservation (DbPool _ observation _ _) = observation

destroyDbPool :: DbPool -> IO ()
destroyDbPool (DbPool pool _ _ _) = destroyAllResources pool

newDbPool :: Text -> IO DbPool
newDbPool = newNamedDbPool "worker" 10 False

newApiDbPool, newRegistrationDbPool, newOracleDbPool :: Text -> IO DbPool
newApiDbPool = newNamedDbPool "api-general" 7 False
newRegistrationDbPool = newNamedDbPool "api-registration" 2 True
newOracleDbPool = newNamedDbPool "api-oracle" 1 True

newNamedDbPool :: Text -> Int -> Bool -> Text -> IO DbPool
newNamedDbPool name size bounded connStr =
  DbPool <$> newPool poolConfig <*> newObservation getMonotonicTimeNSec <*> pure name <*> pure bounded
  where
    connect = bracketOnError
      (connectPostgreSQL $ BS.pack $ T.unpack connStr)
      close
      $ \conn -> do
        -- These dedicated pools never perform schema work or external RPC calls.
        if bounded then void $ execute_ conn "SET statement_timeout='1000ms'; SET lock_timeout='250ms'" else pure ()
        rows <- query conn "SELECT pg_backend_pid()" () :: IO [Only Int]
        case rows of
          [Only pid] -> pure (conn, pid)
          _ -> fail "Database backend PID unavailable"
    poolConfig = defaultPoolConfig
      connect
      (close . fst)
      60.0   -- idle timeout (seconds)
      size

withDb :: HasCallStack => DbPool -> (Connection -> IO a) -> IO a
withDb (DbPool pool observation _ bounded) action =
  observeResource observation source allocate snd (action . fst)
  where
    allocate callback
      | not bounded = withResource pool callback
      | otherwise = mask $ \restore -> do
          acquired <- timeout 250_000 $ takeResource pool
          (resource, local) <- maybe (throwIO DbAcquireDeadline) pure acquired
          value <- (restore (timeout 2_000_000 $ callback resource) >>= maybe (throwIO DbOperationDeadline) pure)
            `onException` destroyResource pool local resource
          putResource local resource
          pure value
    -- Compiler-supplied location only, never a caller-controlled request label.
    source = case getCallStack callStack of
      (_, location) : _ -> T.pack (srcLocModule location <> ":" <> show (srcLocStartLine location))
      [] -> "unknown"

withDbAdvisoryLock :: Connection -> Integer -> IO a -> IO a
withDbAdvisoryLock conn lockId =
  bracket_
    (void (query conn "SELECT 1::BIGINT FROM pg_advisory_lock(?)" (Only lockId) :: IO [Only Integer]))
    (void (query conn "SELECT pg_advisory_unlock(?)" (Only lockId) :: IO [Only Bool]))
