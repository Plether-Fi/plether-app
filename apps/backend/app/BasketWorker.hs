module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, displayException, try)
import Control.Monad (forM_, when)
import Data.Aeson (toJSON)
import Data.Maybe (fromMaybe, isNothing)
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
  )
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
    Left err -> do
      putStrLn $ "Configuration error: " <> err
      putStrLn "Required: RPC_URL and DATABASE_URL. Optional: PYTH_HERMES_URL, PYTH_API_KEY."
    Right cfg ->
      case cfgDatabaseUrl cfg of
        Nothing ->
          putStrLn "DATABASE_URL is required for plether-basket-worker"
        Just dbUrl -> do
          manager <- newManager tlsManagerSettings
          pool <- newDbPool dbUrl
          withDb pool $ \conn -> do
            ensureBasketSnapshotSchema conn
            ensurePerpsKeeperSchema conn
          case waMode args of
            RunOnce -> do
              putStrLn "Fetching one latest six-feed Pyth basket update..."
              result <- runLatestOnce manager pool cfg
              case result of
                Left err -> putStrLn $ "Latest basket update failed: " <> T.unpack err
                Right () -> pure ()
            LatestLoop -> do
              putStrLn $
                "Starting latest six-feed Pyth basket loop every "
                  <> show (waPollSeconds args)
                  <> "s"
              latestLoop manager pool cfg (waPollSeconds args)
            BackfillOnce -> do
              let backfillDays = fromMaybe (cfgPythBackfillDays cfg) (waBackfillDays args)
              putStrLn $ "Running historical basket backfill for " <> show backfillDays <> "d"
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
      putStrLn $ "Latest basket worker exception: " <> displayException err
      pure pollSeconds
    Right (Left err) -> do
      putStrLn $ "Latest basket update skipped: " <> T.unpack err
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
  pending <- withDb pool $ \conn -> getPendingPerpsKeeperOrders conn 20
  forM_ pending $ \order -> do
    let firstRevealTick = pkorCommitTime order + 1
        maxRevealTick = pkorCommitTime order + defaultOrderSettlementWindow
    mExisting <- withDb pool $ \conn ->
      getPythUpdatePayloadForWindow conn firstRevealTick maxRevealTick
    when (isNothing mExisting) $ do
      result <- fetchBasketUpdateAt manager cfg firstRevealTick
      case result of
        Left err ->
          putStrLn $
            "Reveal payload backfill skipped for order "
              <> show (pkorOrderId order)
              <> ": "
              <> T.unpack err
        Right update ->
          case validateRevealWindow (pkorCommitTime order) defaultOrderSettlementWindow (hbuPublishTimes update) of
            Left err ->
              putStrLn $
                "Reveal payload backfill returned unusable payload for order "
                  <> show (pkorOrderId order)
                  <> ": "
                  <> T.unpack err
            Right _ -> do
              cacheResult <- cacheBasketUpdate pool update
              case cacheResult of
                Left err ->
                  putStrLn $
                    "Reveal payload backfill cache failed for order "
                      <> show (pkorOrderId order)
                      <> ": "
                      <> T.unpack err
                Right () ->
                  putStrLn $
                    "Backfilled first reveal payload for order "
                      <> show (pkorOrderId order)
                      <> " at publish time "
                      <> show firstRevealTick
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
      putStrLn $
        "Cached basket update publish window "
          <> show minPublishTime
          <> ".."
          <> show maxPublishTime
          <> " into minute bucket "
          <> show minuteBucket
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
