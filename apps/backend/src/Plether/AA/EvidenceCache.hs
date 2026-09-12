-- Bounded successful-evidence cache. Failed reads and exceptions are not cached.
-- A cache belongs to one gateway/configuration/provider set, and every key also
-- names the exact canonical block hash. Permission is never stored here.
module Plether.AA.EvidenceCache
  ( EvidenceCache, newEvidenceCache, evidence, evidenceObserved, retainSnapshots, clearEvidence ) where

import Control.Concurrent.MVar
import Control.Exception (SomeException, mask, throwIO, try)
import Control.Monad (filterM)
import Data.Maybe (isJust)
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

type Key = (Text, Text)
data Entry e a = Entry Integer (MVar (Either SomeException (Either e a)))
data EvidenceCache e a = EvidenceCache Int (MVar (Integer, Map.Map Key (Entry e a)))

newEvidenceCache :: Int -> IO (EvidenceCache e a)
newEvidenceCache capacity = EvidenceCache (max 1 capacity) <$> newMVar (0, Map.empty)

clearEvidence :: EvidenceCache e a -> IO ()
clearEvidence (EvidenceCache _ state) = modifyMVar_ state $ \(tick, _) -> pure (tick, Map.empty)

retainSnapshots :: EvidenceCache e a -> [Text] -> IO ()
retainSnapshots (EvidenceCache _ state) snapshots = modifyMVar_ state $ \(tick, entries) ->
  pure (tick, Map.filterWithKey (\(blockHash, _) _ -> blockHash `elem` snapshots) entries)

evidence :: EvidenceCache e a -> Text -> Text -> IO (Either e a) -> IO (Either e a)
evidence cache = evidenceObserved cache (const $ pure ())

evidenceObserved :: EvidenceCache e a -> (Text -> IO ()) -> Text -> Text -> IO (Either e a) -> IO (Either e a)
evidenceObserved (EvidenceCache capacity state) observe blockHash identity action = mask $ \restore -> do
  let key = (blockHash, identity)
  selected <- modifyMVar state $ \(tick, entries) -> case Map.lookup key entries of
    Just (Entry _ cell) -> pure ((tick + 1, Map.insert key (Entry tick cell) entries), Right (False, cell))
    Nothing -> do
      let ordered = sortOn (\(_, Entry stamp _) -> stamp) $ Map.toList entries
      complete <- filterM (\(_, Entry _ cell) -> isJust <$> tryReadMVar cell) ordered
      case ordered of
        (_, Entry _ waiting) : _ | Map.size entries >= capacity && null complete ->
          pure ((tick, entries), Left waiting)
        _ -> do
          cell <- newEmptyMVar
          let trimmed = foldr (Map.delete . fst) entries $ take (max 0 $ Map.size entries - capacity + 1) complete
          pure ((tick + 1, Map.insert key (Entry tick cell) trimmed), Right (True, cell))
  case selected of
    Left waiting -> do
      -- Never evict an in-flight leader and duplicate its upstream work.
      observe "capacity_wait"
      _ <- restore $ readMVar waiting
      restore $ evidenceObserved (EvidenceCache capacity state) observe blockHash identity action
    Right (leader, cell) -> do
      ready <- tryReadMVar cell
      observe $ if leader then "miss" else maybe "wait" (const "hit") ready
      if not leader then restore (readMVar cell) >>= either throwIO pure else do
        result <- try @SomeException $ restore action
        putMVar cell result
        case result of
          Right (Right value) -> pure $ Right value
          _ -> do
            modifyMVar_ state $ \(tick, entries) -> pure (tick, case Map.lookup key entries of
              Just (Entry _ current) | current == cell -> Map.delete key entries
              _ -> entries)
            either throwIO pure result
