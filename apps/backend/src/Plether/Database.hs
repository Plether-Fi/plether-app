module Plether.Database
  ( DbPool
  , newDbPool
  , withDb
  , withDbAdvisoryLock
  ) where

import Control.Exception (bracket_)
import Control.Monad (void)
import Data.Pool (Pool, newPool, defaultPoolConfig, withResource)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import Database.PostgreSQL.Simple (Connection, Only (..), close, connectPostgreSQL, query)

type DbPool = Pool Connection

newDbPool :: Text -> IO DbPool
newDbPool connStr = newPool poolConfig
  where
    poolConfig = defaultPoolConfig
      (connectPostgreSQL (BS.pack $ T.unpack connStr))
      close
      60.0   -- idle timeout (seconds)
      10     -- max connections

withDb :: DbPool -> (Connection -> IO a) -> IO a
withDb = withResource

withDbAdvisoryLock :: Connection -> Integer -> IO a -> IO a
withDbAdvisoryLock conn lockId =
  bracket_
    (void (query conn "SELECT 1::BIGINT FROM pg_advisory_lock(?)" (Only lockId) :: IO [Only Integer]))
    (void (query conn "SELECT pg_advisory_unlock(?)" (Only lockId) :: IO [Only Bool]))
