module Plether.Cache
  ( CacheEntry (..)
  , CandlePageCacheValue (..)
  , CurrentCandleCacheValue (..)
  , SingleFlightCache
  , SingleFlightSource (..)
  , AppCache (..)
  , newAppCache
  , newConcurrentSingleFlightCache
  , newSingleFlightCache
  , newRefreshingSingleFlightCache
  , runSingleFlightCache
  , runSingleFlightCacheFresh
  , runSingleFlightCacheTimed
  , runSingleFlightCacheFreshTimed
  , isValid
  , getCached
  , setCached
  , getCachedFor
  , setCachedFor
  , evictStale
  ) where

import Control.Concurrent (MVar, forkIO, newMVar, withMVar)
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
import Control.Exception (SomeException, mask, throwIO, try)
import Control.Monad (void)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Plether.Database.Candles
  ( BasketDefinitionIdentity
  , CandleCurrent
  )
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

-- The mutable current response is composed and revalidated per request. Cache
-- only the coherent raw database snapshot so a reused value cannot bypass the
-- caller's own validation clock.
data CurrentCandleCacheValue = CurrentCandleCacheValue
  { cccvDefinition :: !(Maybe BasketDefinitionIdentity)
  , cccvCurrent :: !CandleCurrent
  , cccvDefinitionAt :: !Integer
  , cccvPoolWaitNs :: !Word64
  , cccvQueryNs :: !Word64
  , cccvCachedAt :: !POSIXTime
  }
  deriving stock (Show)

data SingleFlightEntry value = SingleFlightEntry
  { sfeValue :: !value
  , sfeFreshUntilNs :: !Word64
  , sfeStaleUntilNs :: !Word64
  }

data SingleFlightCache key value = SingleFlightCache
  { sfcEntries :: !(TVar (Map key (SingleFlightEntry value)))
  , sfcInFlight :: !(TVar (Map key (TMVar (Either SomeException value))))
  , sfcLoadLock :: !(Maybe (MVar ()))
  , sfcTtlAnchor :: !SingleFlightTtlAnchor
  , sfcMaxEntries :: !Int
  , sfcFreshTtlNs :: !Word64
  , sfcStaleTtlNs :: !Word64
  }

data SingleFlightTtlAnchor
  = TtlFromLoadStart
  | TtlFromCompletion

data SingleFlightSource
  = SingleFlightLoaded
  | SingleFlightMemory
  | SingleFlightCoalesced
  | SingleFlightStale
  deriving stock (Eq, Show)

data SingleFlightDecision value
  = UseMemory !value
  | UseStale !value
  | WaitForLoad !(TMVar (Either SomeException value))
  | StartLoad !(TMVar (Either SomeException value))
  | StartRefresh !value !(TMVar (Either SomeException value))

data AppCache = AppCache
  { cacheProtocolStatus :: !(TVar (Maybe (CacheEntry ProtocolStatus)))
  , cacheUserDashboards :: !(TVar (Map Text (CacheEntry UserDashboard)))
  , cacheUserAllowances :: !(TVar (Map Text (CacheEntry UserAllowances)))
  , cachePythUpdates :: !(TVar (Map Text (PythUpdateResponse, POSIXTime)))
  , cachePythRateLimitUntil :: !(TVar (Maybe POSIXTime))
  , cacheBasketCandlePages
      :: !(SingleFlightCache (Integer, Integer) (Either ApiError CandlePageCacheValue))
  , cacheBasketCurrentCandles
      :: !(SingleFlightCache (Integer, Text, Integer, Integer) CurrentCandleCacheValue)
  }

candlePageCacheMaxEntries :: Int
candlePageCacheMaxEntries = 64

candlePageCacheFreshTtlNs :: Word64
candlePageCacheFreshTtlNs = 20_000_000_000

candlePageCacheStaleTtlNs :: Word64
candlePageCacheStaleTtlNs = 60_000_000_000

currentCandleCacheMaxEntries :: Int
currentCandleCacheMaxEntries = 32

-- Age this from the beginning of the database load. Together with the edge
-- worker's four-second reuse and five-second browser poll, 850ms leaves a
-- bounded margin inside the current-candle ten-second freshness budget.
currentCandleCacheTtlNs :: Word64
currentCandleCacheTtlNs = 850_000_000

newAppCache :: IO AppCache
newAppCache = do
  candlePages <-
    newRefreshingSingleFlightCache
      candlePageCacheMaxEntries
      candlePageCacheFreshTtlNs
      candlePageCacheStaleTtlNs
  currentCandles <-
    newConcurrentSingleFlightCache
      currentCandleCacheMaxEntries
      currentCandleCacheTtlNs
  AppCache
    <$> newTVarIO Nothing
    <*> newTVarIO Map.empty
    <*> newTVarIO Map.empty
    <*> newTVarIO Map.empty
    <*> newTVarIO Nothing
    <*> pure candlePages
    <*> pure currentCandles

-- Current buckets for different intervals may turn over together. They still
-- single-flight independently by key, but should not form a cross-key convoy
-- behind one slow database refresh.
newConcurrentSingleFlightCache
  :: Int
  -> Word64
  -> IO (SingleFlightCache key value)
newConcurrentSingleFlightCache maxEntries ttlNs =
  SingleFlightCache
    <$> newTVarIO Map.empty
    <*> newTVarIO Map.empty
    <*> pure Nothing
    <*> pure TtlFromLoadStart
    <*> pure (max 0 maxEntries)
    <*> pure ttlNs
    <*> pure ttlNs

newSingleFlightCache :: Int -> Word64 -> IO (SingleFlightCache key value)
newSingleFlightCache maxEntries ttlNs =
  newRefreshingSingleFlightCache maxEntries ttlNs ttlNs

newRefreshingSingleFlightCache
  :: Int
  -> Word64
  -> Word64
  -> IO (SingleFlightCache key value)
newRefreshingSingleFlightCache maxEntries freshTtlNs staleTtlNs =
  SingleFlightCache
    <$> newTVarIO Map.empty
    <*> newTVarIO Map.empty
    <*> (Just <$> newMVar ())
    <*> pure TtlFromCompletion
    <*> pure (max 0 maxEntries)
    <*> pure freshTtlNs
    <*> pure (max freshTtlNs staleTtlNs)

-- Run at most one loader for a key. Waiting cold callers share its result;
-- callers with a bounded stale value return it immediately while one serialized
-- background refresh runs. Only values accepted by shouldCache survive in
-- memory. Exceptions are published to cold waiters before being rethrown, while
-- a failed refresh leaves the previous value available until its hard expiry.
runSingleFlightCache
  :: Ord key
  => SingleFlightCache key value
  -> key
  -> (value -> Bool)
  -> IO value
  -> IO (SingleFlightSource, value)
runSingleFlightCache cache key shouldCache load = mask $ \restore -> do
  (source, value, _) <-
    runSingleFlightCacheWithPolicy False cache key shouldCache load restore
  pure (source, value)

runSingleFlightCacheTimed
  :: Ord key
  => SingleFlightCache key value
  -> key
  -> (value -> Bool)
  -> IO value
  -> IO (SingleFlightSource, value, Word64)
runSingleFlightCacheTimed cache key shouldCache load = mask $ \restore ->
  runSingleFlightCacheWithPolicy False cache key shouldCache load restore

-- Ignore a cached value and wait for the authoritative loader result. This is
-- used for explicit HTTP revalidation so a stale-while-revalidate response
-- cannot hide a newly published dataset generation from the client.
runSingleFlightCacheFresh
  :: Ord key
  => SingleFlightCache key value
  -> key
  -> (value -> Bool)
  -> IO value
  -> IO (SingleFlightSource, value)
runSingleFlightCacheFresh cache key shouldCache load = mask $ \restore -> do
  (source, value, _) <-
    runSingleFlightCacheWithPolicy True cache key shouldCache load restore
  pure (source, value)

runSingleFlightCacheFreshTimed
  :: Ord key
  => SingleFlightCache key value
  -> key
  -> (value -> Bool)
  -> IO value
  -> IO (SingleFlightSource, value, Word64)
runSingleFlightCacheFreshTimed cache key shouldCache load = mask $ \restore ->
  runSingleFlightCacheWithPolicy True cache key shouldCache load restore

runSingleFlightCacheWithPolicy
  :: Ord key
  => Bool
  -> SingleFlightCache key value
  -> key
  -> (value -> Bool)
  -> IO value
  -> (forall result. IO result -> IO result)
  -> IO (SingleFlightSource, value, Word64)
runSingleFlightCacheWithPolicy requireFresh cache key shouldCache load restore = do
  nowNs <- getMonotonicTimeNSec
  decision <- atomically $ claimSingleFlight requireFresh cache key nowNs
  case decision of
    UseMemory value -> pure (SingleFlightMemory, value, 0)
    UseStale value -> pure (SingleFlightStale, value, 0)
    WaitForLoad gate -> do
      waitStartedAtNs <- getMonotonicTimeNSec
      outcome <- restore $ atomically $ readTMVar gate
      waitFinishedAtNs <- getMonotonicTimeNSec
      value <- either throwIO pure outcome
      pure (SingleFlightCoalesced, value, waitFinishedAtNs - waitStartedAtNs)
    StartLoad gate -> do
      startedAtNs <- getMonotonicTimeNSec
      outcome <- try $ restore $ runSingleFlightLoad cache load
      completedAtNs <- getMonotonicTimeNSec
      atomically $
        completeSingleFlight
          cache key gate startedAtNs completedAtNs shouldCache outcome
      value <- either throwIO pure outcome
      pure (SingleFlightLoaded, value, 0)
    StartRefresh staleValue gate -> do
      void $ forkIO $ do
        startedAtNs <- getMonotonicTimeNSec
        outcome <- try $ restore $ runSingleFlightLoad cache load
        completedAtNs <- getMonotonicTimeNSec
        atomically $
          completeSingleFlight
            cache key gate startedAtNs completedAtNs shouldCache outcome
      pure (SingleFlightStale, staleValue, 0)

runSingleFlightLoad :: SingleFlightCache key value -> IO value -> IO value
runSingleFlightLoad SingleFlightCache {sfcLoadLock = Nothing} load = load
runSingleFlightLoad SingleFlightCache {sfcLoadLock = Just loadLock} load =
  withMVar loadLock $ const load

claimSingleFlight
  :: Ord key
  => Bool
  -> SingleFlightCache key value
  -> key
  -> Word64
  -> STM (SingleFlightDecision value)
claimSingleFlight requireFresh SingleFlightCache {..} key nowNs = do
  entries <- readTVar sfcEntries
  let retained = Map.filter ((> nowNs) . sfeStaleUntilNs) entries
  writeTVar sfcEntries retained
  inFlight <- readTVar sfcInFlight
  case (requireFresh, Map.lookup key inFlight) of
    (True, Just gate) -> pure $ WaitForLoad gate
    (True, Nothing) -> do
      gate <- newEmptyTMVar
      writeTVar sfcInFlight $ Map.insert key gate inFlight
      pure $ StartLoad gate
    _ -> case Map.lookup key retained of
      Just entry
        | sfeFreshUntilNs entry > nowNs -> pure $ UseMemory $ sfeValue entry
        | otherwise ->
            case Map.lookup key inFlight of
              Just _ -> pure $ UseStale $ sfeValue entry
              Nothing -> do
                gate <- newEmptyTMVar
                writeTVar sfcInFlight $ Map.insert key gate inFlight
                pure $ StartRefresh (sfeValue entry) gate
      Nothing -> do
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
  -> Word64
  -> (value -> Bool)
  -> Either SomeException value
  -> STM ()
completeSingleFlight
  SingleFlightCache {..}
  key
  gate
  startedAtNs
  completedAtNs
  shouldCache
  outcome = do
  modifyTVar' sfcInFlight $ Map.delete key
  case outcome of
    Right value | shouldCache value -> do
      let ttlStartedAtNs =
            case sfcTtlAnchor of
              TtlFromLoadStart -> startedAtNs
              TtlFromCompletion -> completedAtNs
      modifyTVar' sfcEntries $
        trimSingleFlightEntries sfcMaxEntries
          . Map.insert
            key
            SingleFlightEntry
              { sfeValue = value
              , sfeFreshUntilNs = ttlStartedAtNs + sfcFreshTtlNs
              , sfeStaleUntilNs = ttlStartedAtNs + sfcStaleTtlNs
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
    choose Nothing key entry = Just (key, sfeStaleUntilNs entry)
    choose current@(Just (_, oldestExpiry)) key entry
      | sfeStaleUntilNs entry < oldestExpiry = Just (key, sfeStaleUntilNs entry)
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
