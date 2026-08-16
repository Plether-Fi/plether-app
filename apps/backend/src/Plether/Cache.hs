module Plether.Cache
  ( CacheEntry (..)
  , CandlePageCacheValue (..)
  , SingleFlightCache
  , SingleFlightSource (..)
  , AppCache (..)
  , newAppCache
  , newSingleFlightCache
  , runSingleFlightCache
  , isValid
  , getCached
  , setCached
  , getCachedFor
  , setCachedFor
  , evictStale
  ) where

import Control.Exception (SomeException, mask, throwIO, try)
import Control.Concurrent.STM
  ( STM
  , TMVar
  , TVar
  , atomically
  , modifyTVar'
  , newEmptyTMVar
  , newTVarIO
  , putTMVar
  , readTMVar
  , readTVar
  , writeTVar
  )
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Plether.Types.Api (ApiResponse)
import Plether.Types.Error (ApiError)
import Plether.Types.Protocol (ProtocolStatus)
import Plether.Types.Perps (BasketCandlePage, PythUpdateResponse)
import Plether.Types.User (UserAllowances, UserDashboard)

data CacheEntry a = CacheEntry
  { ceValue :: !a
  , ceBlock :: !Integer
  , ceCachedAt :: !POSIXTime
  }

-- Historical candle pages contain only finalized rows, while their mutable
-- successor is served by /candles/current. Retaining a page briefly therefore
-- absorbs duplicate TradingView requests without hiding the live candle.
data CandlePageCacheValue = CandlePageCacheValue
  { cpcvResponse :: !(ApiResponse BasketCandlePage)
  , cpcvPoolWaitNs :: !Word64
  , cpcvQueryNs :: !Word64
  , cpcvRowCount :: !Int
  , cpcvFinalizedThrough :: !(Maybe Integer)
  , cpcvDatasetGeneration :: !Integer
  , cpcvCachedAt :: !POSIXTime
  }

data SingleFlightEntry value = SingleFlightEntry
  { sfeValue :: !value
  , sfeExpiresAtNs :: !Word64
  }

data SingleFlightCache key value = SingleFlightCache
  { sfcEntries :: !(TVar (Map key (SingleFlightEntry value)))
  , sfcInFlight :: !(TVar (Map key (TMVar (Either SomeException value))))
  , sfcMaxEntries :: !Int
  , sfcTtlNs :: !Word64
  }

data SingleFlightSource
  = SingleFlightLoaded
  | SingleFlightMemory
  | SingleFlightCoalesced
  deriving stock (Eq, Show)

data SingleFlightDecision value
  = UseMemory !value
  | WaitForLoad !(TMVar (Either SomeException value))
  | StartLoad !(TMVar (Either SomeException value))

data AppCache = AppCache
  { cacheProtocolStatus :: !(TVar (Maybe (CacheEntry ProtocolStatus)))
  , cacheUserDashboards :: !(TVar (Map Text (CacheEntry UserDashboard)))
  , cacheUserAllowances :: !(TVar (Map Text (CacheEntry UserAllowances)))
  , cachePythUpdates :: !(TVar (Map Text (PythUpdateResponse, POSIXTime)))
  , cachePythRateLimitUntil :: !(TVar (Maybe POSIXTime))
  , cacheBasketCandlePages
      :: !(SingleFlightCache (Integer, Integer) (Either ApiError CandlePageCacheValue))
  }

candlePageCacheMaxEntries :: Int
candlePageCacheMaxEntries = 64

candlePageCacheTtlNs :: Word64
candlePageCacheTtlNs = 5_000_000_000

newAppCache :: IO AppCache
newAppCache = do
  candlePages <- newSingleFlightCache candlePageCacheMaxEntries candlePageCacheTtlNs
  AppCache
    <$> newTVarIO Nothing
    <*> newTVarIO Map.empty
    <*> newTVarIO Map.empty
    <*> newTVarIO Map.empty
    <*> newTVarIO Nothing
    <*> pure candlePages

newSingleFlightCache :: Int -> Word64 -> IO (SingleFlightCache key value)
newSingleFlightCache maxEntries ttlNs =
  SingleFlightCache
    <$> newTVarIO Map.empty
    <*> newTVarIO Map.empty
    <*> pure (max 0 maxEntries)
    <*> pure ttlNs

-- Run at most one loader for a key. Waiting callers share its result, and only
-- values accepted by shouldCache survive as short-lived memory hits. Exceptions
-- are published to waiters before being rethrown so a cancelled loader cannot
-- strand an in-flight key forever.
runSingleFlightCache
  :: Ord key
  => SingleFlightCache key value
  -> key
  -> (value -> Bool)
  -> IO value
  -> IO (SingleFlightSource, value)
runSingleFlightCache cache key shouldCache load = mask $ \restore -> do
  nowNs <- getMonotonicTimeNSec
  decision <- atomically $ claimSingleFlight cache key nowNs
  case decision of
    UseMemory value -> pure (SingleFlightMemory, value)
    WaitForLoad gate -> do
      outcome <- restore $ atomically $ readTMVar gate
      value <- either throwIO pure outcome
      pure (SingleFlightCoalesced, value)
    StartLoad gate -> do
      outcome <- try $ restore load
      completedAtNs <- getMonotonicTimeNSec
      atomically $ completeSingleFlight cache key gate completedAtNs shouldCache outcome
      value <- either throwIO pure outcome
      pure (SingleFlightLoaded, value)

claimSingleFlight
  :: Ord key
  => SingleFlightCache key value
  -> key
  -> Word64
  -> STM (SingleFlightDecision value)
claimSingleFlight SingleFlightCache {..} key nowNs = do
  entries <- readTVar sfcEntries
  let fresh = Map.filter ((> nowNs) . sfeExpiresAtNs) entries
  writeTVar sfcEntries fresh
  case Map.lookup key fresh of
    Just entry -> pure $ UseMemory $ sfeValue entry
    Nothing -> do
      inFlight <- readTVar sfcInFlight
      case Map.lookup key inFlight of
        Just gate -> pure $ WaitForLoad gate
        Nothing -> do
          gate <- newEmptyTMVar
          writeTVar sfcInFlight $ Map.insert key gate inFlight
          pure $ StartLoad gate

completeSingleFlight
  :: Ord key
  => SingleFlightCache key value
  -> key
  -> TMVar (Either SomeException value)
  -> Word64
  -> (value -> Bool)
  -> Either SomeException value
  -> STM ()
completeSingleFlight SingleFlightCache {..} key gate completedAtNs shouldCache outcome = do
  modifyTVar' sfcInFlight $ Map.delete key
  case outcome of
    Right value | shouldCache value ->
      modifyTVar' sfcEntries $
        trimSingleFlightEntries sfcMaxEntries
          . Map.insert
            key
            SingleFlightEntry
              { sfeValue = value
              , sfeExpiresAtNs = completedAtNs + sfcTtlNs
              }
    _ -> pure ()
  putTMVar gate outcome

trimSingleFlightEntries
  :: Ord key
  => Int
  -> Map key (SingleFlightEntry value)
  -> Map key (SingleFlightEntry value)
trimSingleFlightEntries maxEntries entries
  | Map.size entries <= maxEntries = entries
  | otherwise =
      case oldestSingleFlightKey entries of
        Nothing -> entries
        Just key -> trimSingleFlightEntries maxEntries $ Map.delete key entries

oldestSingleFlightKey :: Map key (SingleFlightEntry value) -> Maybe key
oldestSingleFlightKey = fmap fst . Map.foldlWithKey' choose Nothing
  where
    choose Nothing key entry = Just (key, sfeExpiresAtNs entry)
    choose current@(Just (_, oldestExpiry)) key entry
      | sfeExpiresAtNs entry < oldestExpiry = Just (key, sfeExpiresAtNs entry)
      | otherwise = current

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
