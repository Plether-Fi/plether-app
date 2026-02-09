module Plether.Cache
  ( CacheEntry (..)
  , AppCache (..)
  , newAppCache
  , isValid
  , getCached
  , setCached
  , getCachedFor
  , setCachedFor
  , evictStale
  ) where

import Control.Concurrent.STM
  ( STM
  , TVar
  , newTVarIO
  , readTVar
  , writeTVar
  )
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Time.Clock.POSIX (POSIXTime)
import Plether.Types.Protocol (ProtocolStatus)
import Plether.Types.User (UserAllowances, UserDashboard)

data CacheEntry a = CacheEntry
  { ceValue :: !a
  , ceBlock :: !Integer
  , ceCachedAt :: !POSIXTime
  }

data AppCache = AppCache
  { cacheProtocolStatus :: !(TVar (Maybe (CacheEntry ProtocolStatus)))
  , cacheUserDashboards :: !(TVar (Map Text (CacheEntry UserDashboard)))
  , cacheUserAllowances :: !(TVar (Map Text (CacheEntry UserAllowances)))
  }

newAppCache :: IO AppCache
newAppCache =
  AppCache
    <$> newTVarIO Nothing
    <*> newTVarIO Map.empty
    <*> newTVarIO Map.empty

isValid :: Integer -> CacheEntry a -> Bool
isValid currentBlock entry = ceBlock entry >= currentBlock

getCached :: TVar (Maybe (CacheEntry a)) -> Integer -> STM (Maybe (CacheEntry a))
getCached cacheVar currentBlock = do
  mEntry <- readTVar cacheVar
  pure $ case mEntry of
    Just entry | isValid currentBlock entry -> Just entry
    _ -> Nothing

setCached :: TVar (Maybe (CacheEntry a)) -> a -> Integer -> POSIXTime -> STM ()
setCached cacheVar value blockNum cachedAt =
  writeTVar cacheVar $ Just CacheEntry
    { ceValue = value
    , ceBlock = blockNum
    , ceCachedAt = cachedAt
    }

getCachedFor :: Ord k => TVar (Map k (CacheEntry a)) -> k -> Integer -> STM (Maybe (CacheEntry a))
getCachedFor cacheVar key currentBlock = do
  cache <- readTVar cacheVar
  pure $ case Map.lookup key cache of
    Just entry | isValid currentBlock entry -> Just entry
    _ -> Nothing

setCachedFor :: Ord k => TVar (Map k (CacheEntry a)) -> k -> a -> Integer -> POSIXTime -> STM ()
setCachedFor cacheVar key value blockNum cachedAt = do
  cache <- readTVar cacheVar
  let entry = CacheEntry
        { ceValue = value
        , ceBlock = blockNum
        , ceCachedAt = cachedAt
        }
  writeTVar cacheVar $ Map.insert key entry cache

evictStale :: Integer -> TVar (Map k (CacheEntry a)) -> STM ()
evictStale currentBlock cacheVar = do
  cache <- readTVar cacheVar
  let fresh = Map.filter (\e -> ceBlock e >= currentBlock - 10) cache
  writeTVar cacheVar fresh
