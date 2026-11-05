module Plether.Pyth.Prefetch
  ( runPriorityPrefetch, retryAfterSeconds, ProviderGate, newProviderGate, withProviderGate ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently_)
import Control.Concurrent.MVar (MVar, modifyMVar, newMVar, readMVar)
import Control.Exception (SomeAsyncException, SomeException, fromException, throwIO, try)
import Control.Monad (forever)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Text.Read (readMaybe)
import System.Timeout (timeout)
import Plether.Logging (logWarnEvery)

retryAfterSeconds :: Text -> Maybe Int
retryAfterSeconds message = do
  suffix <- T.stripPrefix "Hermes returned HTTP 429; retry after " message
  value <- readMaybe $ T.unpack suffix
  pure $ max 1 value

-- A provider-wide 429 applies to latest and historical requests alike. Ordinary
-- update delays are not shared and cannot suspend historical-price retrieval.
newtype ProviderGate = ProviderGate (MVar Integer)

newProviderGate :: IO ProviderGate
newProviderGate = ProviderGate <$> newMVar 0

withProviderGate :: ProviderGate -> IO (Either Text a) -> IO (Either Text a)
withProviderGate gate@(ProviderGate blocked) action = do
  now <- floor <$> getPOSIXTime
  untilTime <- readMVar blocked
  if now < untilTime
    then threadDelay 1_000_000 >> withProviderGate gate action
    else do
      result <- action
      finished <- ceiling <$> getPOSIXTime
      case result of
        Left err | Just seconds <- retryAfterSeconds err ->
          modifyMVar blocked $ \prior -> pure (max prior (finished + fromIntegral seconds), ())
        _ -> pure ()
      pure result

-- Two workers reserve jobs atomically in priority order. A stalled tail fetch
-- cannot hold the head's next retry until the whole queue has been traversed.
-- Keys identify an exact feed set / first reveal tick / settlement window.
runPriorityPrefetch :: Ord key => IO [(key, IO (Either Text ()))] -> IO ()
runPriorityPrefetch load = do
  state <- newMVar (Map.empty, 0 :: Integer)
  snapshot <- newMVar (0 :: Integer, [])
  let now = floor <$> getPOSIXTime :: IO Integer
      worker = forever $ do
        at <- now
        candidates <- modifyMVar snapshot $ \(refreshAt, prior) ->
          if at < refreshAt then pure ((refreshAt, prior), prior)
          else do
            loaded <- try @SomeException $ timeout 4_000_000 load
            value <- case loaded of
              Right (Just fresh) -> pure fresh
              Left err | Just (_ :: SomeAsyncException) <- fromException err -> throwIO err
              _ -> do
                logWarnEvery 30 "reveal_queue_load_failed" "Could not load pending historical-price requests" []
                pure []
            pure ((at + 1, value), value)
        selected <- modifyMVar state $ \(jobs, blockedUntil) -> do
          let live = Set.fromList $ map fst candidates
              current = Map.filterWithKey (\key value -> value == Nothing || Set.member key live) jobs
              eligible (key, _) = case Map.lookup key current of
                Nothing -> True
                Just (Just due) -> at >= due
                Just Nothing -> False
          case if at < blockedUntil then [] else filter eligible candidates of
            [] -> pure ((current, blockedUntil), Nothing)
            (key, action):_ -> pure ((Map.insert key Nothing current, blockedUntil), Just (key, action))
        case selected of
          Nothing -> threadDelay 1_000_000
          Just (key, action) -> do
            attempted <- try @SomeException $ timeout 10_000_000 action
            result <- case attempted of
              Left err -> case fromException err :: Maybe SomeAsyncException of
                Just _ -> throwIO err
                Nothing -> pure $ Left "Reveal request failed"
              Right Nothing -> pure $ Left "Reveal request timed out"
              Right (Just value) -> pure value
            finished <- now
            modifyMVar state $ \(jobs, blockedUntil) -> do
              let cooldown = either (maybe 1 fromIntegral . retryAfterSeconds) (const 60) result
                  providerPause = either (maybe 0 ((finished +) . fromIntegral) . retryAfterSeconds) (const 0) result
              pure ((Map.insert key (Just $ finished + cooldown) jobs, max blockedUntil providerPause), ())
  concurrently_ worker worker
