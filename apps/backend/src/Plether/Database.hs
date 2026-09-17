module Plether.Database
  ( DbPool
  , newDbPool
  , destroyDbPool
  , dbPoolObservation
  , withDb
  , withDbAdvisoryLock
  ) where

import Control.Exception (bracket_, bracketOnError)
import Control.Monad (void)
import Data.Pool (Pool, newPool, defaultPoolConfig, withResource, destroyAllResources)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import Database.PostgreSQL.Simple (Connection, Only (..), close, connectPostgreSQL, query)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stack (HasCallStack, callStack, getCallStack, SrcLoc (..))
import Plether.Database.Observation (Observation, newObservation, observeResource)

data DbPool = DbPool (Pool (Connection, Int)) Observation

dbPoolObservation :: DbPool -> Observation
dbPoolObservation (DbPool _ observation) = observation

destroyDbPool :: DbPool -> IO ()
destroyDbPool (DbPool pool _) = destroyAllResources pool

newDbPool :: Text -> IO DbPool
newDbPool connStr = DbPool <$> newPool poolConfig <*> newObservation getMonotonicTimeNSec
  where
    connect = bracketOnError
      (connectPostgreSQL $ BS.pack $ T.unpack connStr)
      close
      $ \conn -> do
        rows <- query conn "SELECT pg_backend_pid()" () :: IO [Only Int]
        case rows of
          [Only pid] -> pure (conn, pid)
          _ -> fail "Database backend PID unavailable"
    poolConfig = defaultPoolConfig
      connect
      (close . fst)
      60.0   -- idle timeout (seconds)
      10     -- max connections

withDb :: HasCallStack => DbPool -> (Connection -> IO a) -> IO a
withDb (DbPool pool observation) action =
  observeResource observation source (withResource pool) snd (action . fst)
  where
    -- Compiler-supplied location only, never a caller-controlled request label.
    source = case getCallStack callStack of
      (_, location) : _ -> T.pack (srcLocModule location <> ":" <> show (srcLocStartLine location))
      [] -> "unknown"

withDbAdvisoryLock :: Connection -> Integer -> IO a -> IO a
withDbAdvisoryLock conn lockId =
  bracket_
    (void (query conn "SELECT 1::BIGINT FROM pg_advisory_lock(?)" (Only lockId) :: IO [Only Integer]))
    (void (query conn "SELECT pg_advisory_unlock(?)" (Only lockId) :: IO [Only Bool]))
