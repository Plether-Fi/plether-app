module Plether.Database
  ( DbPool
  , newDbPool
  , withDb
  ) where

import Control.Exception (bracket)
import Data.Pool (Pool, newPool, defaultPoolConfig, withResource)
import Data.Text (Text)
import qualified Data.Text as T
import Database.PostgreSQL.Simple (Connection, close, connectPostgreSQL)
import qualified Data.ByteString.Char8 as BS

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
