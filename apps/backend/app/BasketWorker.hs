module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, displayException, try)
import Control.Monad (forM_, when)
import Data.Aeson (toJSON)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Plether.Config (Config (..), loadConfig)
import Plether.Database (DbPool, newDbPool, withDb)
import Plether.Database.Schema
  ( PerpsKeeperOrderRow (..)
  , ensureBasketSnapshotSchema
  , ensurePerpsKeeperSchema
  , getPendingPerpsKeeperOrders
  , getPythUpdatePayloadForWindow
  , insertBasketSnapshotWithSource
  , insertPythUpdatePayload
  , isHistoricalRevealPayload
  )
import Plether.Logging (field, logError, logErrorEvery, logInfo, logInfoEvery, logWarnEvery)
import Plether.Pyth.Hermes (HermesBasketUpdate (..), fetchBasketUpdateAt, fetchLatestBasketUpdate)
import Plether.Pyth.History (BasketIngestorConfig (..), runBasketBackfill)
import Plether.Pyth.RevealPayload (validatePublishTimes, validateRevealWindow)
import System.Environment (getArgs)
import Text.Read (readMaybe)

data WorkerMode
  = RunOnce
  | LatestLoop
  | BackfillOnce
  deriving (Eq, Show)

data WorkerArgs = WorkerArgs
  { waMode :: WorkerMode
  , waPollSeconds :: Int
  , waBackfillDays :: Maybe Int
  }
  deriving (Show)

defaultPollSeconds :: Int
defaultPollSeconds = 5

defaultOrderSettlementWindow :: Integer
defaultOrderSettlementWindow = 15

main :: IO ()
main = do
  args <- parseWorkerArgs <$> getArgs
  eConfig <- loadConfig
  case eConfig of
    Left err ->
      logError
        "basket_worker_configuration_invalid"
        "Basket worker configuration is invalid"
        [field "error" err]
    Right cfg ->
      case cfgDatabaseUrl cfg of
        Nothing ->
          logError
            "basket_worker_database_missing"
            "Basket worker requires a database"
            []
        Just dbUrl -> do
          manager <- newManager tlsManagerSettings
          pool <- newDbPool dbUrl
          withDb pool $ \conn -> do
            ensureBasketSnapshotSchema conn
            ensurePerpsKeeperSchema conn
          logInfo
            "basket_worker_started"
            "Pyth basket worker started"
            [ field "mode" $ show $ waMode args
            , field "poll_seconds" $ waPollSeconds args
            ]
          case waMode args of
            RunOnce -> do
              result <- runLatestOnce manager pool cfg
              case result of
                Left err ->
                  logError
                    "basket_update_failed"
                    "Latest Pyth basket update failed"
                    [field "error" err]
                Right () -> pure ()
            LatestLoop ->
              latestLoop manager pool cfg (waPollSeconds args)
            BackfillOnce -> do
              let backfillDays = fromMaybe (cfgPythBackfillDays cfg) (waBackfillDays args)
              runBasketBackfill manager pool BasketIngestorConfig
                { bicBenchmarksUrl = cfgPythBenchmarksUrl cfg
                , bicBackfillDays = backfillDays
                , bicSampleIntervalSeconds = cfgPythSampleIntervalSeconds cfg
                , bicPollSeconds = 0
                }

latestLoop :: Manager -> DbPool -> Config -> Int -> IO ()
latestLoop manager pool cfg pollSeconds = do
  result <- try (runLatestCycle manager pool cfg) :: IO (Either SomeException (Either T.Text ()))
  delaySeconds <- case result of
    Left err -> do
      logErrorEvery
        60
        "basket_worker_iteration_failed"
        "Basket worker iteration failed"
        [field "error" $ displayException err]
      pure pollSeconds
    Right (Left err) -> do
      logWarnEvery
        60
        "basket_update_skipped"
        "Latest Pyth basket update was skipped"
        [ field "rate_limited" $ "429" `T.isInfixOf` err
        , field "error" err
        ]
      pure $ if "429" `T.isInfixOf` err then 60 else pollSeconds
    Right (Right ()) ->
      pure pollSeconds
  threadDelay (max 1 delaySeconds * 1_000_000)
  latestLoop manager pool cfg pollSeconds

runLatestCycle :: Manager -> DbPool -> Config -> IO (Either T.Text ())
runLatestCycle manager pool cfg = do
  latestResult <- runLatestOnce manager pool cfg
  backfillResult <- backfillPendingOrderRevealPayloads manager pool cfg
  pure $ case (latestResult, backfillResult) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (), Right ()) -> Right ()

runLatestOnce :: Manager -> DbPool -> Config -> IO (Either T.Text ())
runLatestOnce manager pool cfg = do
  result <- fetchLatestBasketUpdate manager cfg
  case result of
    Left err -> pure $ Left err
    Right update -> cacheBasketUpdate pool update

backfillPendingOrderRevealPayloads :: Manager -> DbPool -> Config -> IO (Either T.Text ())
backfillPendingOrderRevealPayloads manager pool cfg = do
  pending <- withDb pool $ \conn -> getPendingPerpsKeeperOrders conn (cfgPerpsOrderRouter cfg) 20
  forM_ pending $ \order -> do
    let firstRevealTick = pkorCommitTime order + 1
        maxRevealTick = pkorCommitTime order + defaultOrderSettlementWindow
    mExisting <- withDb pool $ \conn ->
      getPythUpdatePayloadForWindow conn firstRevealTick maxRevealTick
    when (maybe True (not . isHistoricalRevealPayload) mExisting) $ do
      result <- fetchBasketUpdateAt manager cfg firstRevealTick
      case result of
        Left err ->
          logWarnEvery
            60
            "reveal_payload_backfill_fetch_failed"
            "Reveal payload backfill fetch failed"
            [ field "order_id" $ pkorOrderId order
            , field "error" err
            ]
        Right update ->
          case validateRevealWindow (pkorCommitTime order) defaultOrderSettlementWindow (hbuPublishTimes update) of
            Left err ->
              logWarnEvery
                60
                "reveal_payload_backfill_invalid"
                "Reveal payload backfill returned an unusable payload"
                [ field "order_id" $ pkorOrderId order
                , field "error" err
                ]
            Right _ -> do
              cacheResult <- cacheBasketUpdate pool update
              case cacheResult of
                Left err ->
                  logWarnEvery
                    60
                    "reveal_payload_backfill_cache_failed"
                    "Reveal payload backfill could not be cached"
                    [ field "order_id" $ pkorOrderId order
                    , field "error" err
                    ]
                Right () ->
                  logInfo
                    "reveal_payload_backfilled"
                    "First reveal payload was backfilled for an order"
                    [ field "order_id" $ pkorOrderId order
                    , field "publish_time" firstRevealTick
                    ]
  pure $ Right ()

cacheBasketUpdate :: DbPool -> HermesBasketUpdate -> IO (Either T.Text ())
cacheBasketUpdate pool update =
  case validatePublishTimes (hbuPublishTimes update) of
    Left err -> pure $ Left err
    Right (minPublishTime, maxPublishTime) -> do
      let minuteBucket = (minPublishTime `div` 60) * 60
      withDb pool $ \conn -> do
        insertBasketSnapshotWithSource
          conn
          minuteBucket
          60
          (hbuBasketPrice update)
          (hbuComponents update)
          "pyth_hermes_latest"
        insertPythUpdatePayload
          conn
          minPublishTime
          maxPublishTime
          (toJSON $ hbuPublishTimes update)
          (toJSON $ hbuUpdateData update)
          (hbuFetchedAt update)
          (hbuSource update)
      logInfoEvery
        300
        "basket_cache_progress"
        "Latest Pyth basket update was cached"
        [ field "min_publish_time" minPublishTime
        , field "max_publish_time" maxPublishTime
        , field "minute_bucket" minuteBucket
        ]
      pure $ Right ()

parseWorkerArgs :: [String] -> WorkerArgs
parseWorkerArgs args =
  WorkerArgs
    { waMode =
        if "--backfill-once" `elem` args
          then BackfillOnce
          else if "--latest-loop" `elem` args
            then LatestLoop
            else RunOnce
    , waPollSeconds = readFlag "--poll-seconds" defaultPollSeconds args
    , waBackfillDays =
        case lookupFlag "--backfill-days" args of
          Just value -> readMaybe value
          Nothing -> Nothing
    }

readFlag :: (Read a) => String -> a -> [String] -> a
readFlag name fallback args =
  case lookupFlag name args >>= readMaybe of
    Just value -> value
    Nothing -> fallback

lookupFlag :: String -> [String] -> Maybe String
lookupFlag _ [] = Nothing
lookupFlag name (flag : value : rest)
  | flag == name = Just value
  | otherwise = lookupFlag name (value : rest)
lookupFlag _ [_] = Nothing
