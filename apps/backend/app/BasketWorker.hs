module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, displayException, try)
import Control.Monad (forM_, when)
import Data.Aeson (toJSON)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
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
  , promotePythPayloadSource
  )
import Plether.Ethereum.Client (EthClient, RpcError (..), newClient)
import Plether.Ethereum.Contracts.Perps
  ( orderSettlementWindow
  , parsePythUpdateData
  , parseUniquePythUpdateData
  )
import Plether.Logging (field, logError, logErrorEvery, logInfo, logInfoEvery, logWarnEvery)
import Plether.Pyth.Basket
  ( BasketComponent (..)
  , BasketComponentPrice
  , PythPricePoint (..)
  , basketComponents
  , computeBasketSnapshot
  )
import Plether.Pyth.Hermes
  ( HermesBasketUpdate (..)
  , fetchBasketUpdateAt
  , fetchLatestBasketUpdate
  , isPermanentHermesConfigurationError
  , resolveHermesApiKey
  )
import Plether.Pyth.History (BasketIngestorConfig (..), runBasketBackfill)
import Plether.Pyth.RevealPayload
  ( validateLatestPublishTimes
  , validatePublishTimes
  , validateRevealWindow
  )
import Plether.Utils.Hex (hexToByteStringEither)
import System.Environment (getArgs)
import System.Exit (exitFailure)
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

main :: IO ()
main = do
  args <- parseWorkerArgs <$> getArgs
  eConfig <- loadConfig
  case eConfig of
    Left err -> do
      logError
        "basket_worker_configuration_invalid"
        "Basket worker configuration is invalid"
        [field "error" err]
      exitFailure
    Right cfg ->
      case resolveHermesApiKey (cfgPythHermesUrl cfg) (cfgPythApiKey cfg) of
        Left err -> do
          logError
            "basket_worker_configuration_invalid"
            "Basket worker Pyth configuration is invalid"
            [field "error" err]
          exitFailure
        Right _ -> case cfgDatabaseUrl cfg of
          Nothing -> do
            logError
              "basket_worker_database_missing"
              "Basket worker requires a database"
              []
            exitFailure
          Just dbUrl -> do
            manager <- newManager tlsManagerSettings
            ethClient <- newClient (cfgPerpsRpcUrl cfg)
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
                result <- runLatestOnce manager ethClient pool cfg
                case result of
                  Left err -> do
                    logError
                      "basket_update_failed"
                      "Latest Pyth basket update failed"
                      [field "error" err]
                    exitFailure
                  Right () -> pure ()
              LatestLoop ->
                latestLoop manager ethClient pool cfg (waPollSeconds args)
              BackfillOnce -> do
                let backfillDays = fromMaybe (cfgPythBackfillDays cfg) (waBackfillDays args)
                runBasketBackfill manager pool BasketIngestorConfig
                  { bicBenchmarksUrl = cfgPythBenchmarksUrl cfg
                  , bicApiKey = cfgPythApiKey cfg
                  , bicBackfillDays = backfillDays
                  , bicSampleIntervalSeconds = cfgPythSampleIntervalSeconds cfg
                  , bicPollSeconds = 0
                  }

latestLoop :: Manager -> EthClient -> DbPool -> Config -> Int -> IO ()
latestLoop manager ethClient pool cfg pollSeconds = do
  result <- try (runLatestCycle manager ethClient pool cfg) :: IO (Either SomeException (Either T.Text ()))
  delaySeconds <- case result of
    Left err -> do
      logErrorEvery
        60
        "basket_worker_iteration_failed"
        "Basket worker iteration failed"
        [field "error" $ displayException err]
      pure pollSeconds
    Right (Left err)
      | isPermanentHermesConfigurationError err -> do
          logError
            "basket_worker_configuration_rejected"
            "Basket worker stopped because Hermes rejected its credentials or endpoint"
            [field "error" err]
          exitFailure
      | otherwise -> do
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
  latestLoop manager ethClient pool cfg pollSeconds

runLatestCycle :: Manager -> EthClient -> DbPool -> Config -> IO (Either T.Text ())
runLatestCycle manager ethClient pool cfg = do
  latestResult <- runLatestOnce manager ethClient pool cfg
  backfillResult <- backfillPendingOrderRevealPayloads manager ethClient pool cfg
  pure $ case (latestResult, backfillResult) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (), Right ()) -> Right ()

runLatestOnce :: Manager -> EthClient -> DbPool -> Config -> IO (Either T.Text ())
runLatestOnce manager ethClient pool cfg = do
  result <- fetchLatestBasketUpdate manager cfg
  case result of
    Left err -> pure $ Left err
    Right update -> cacheBasketUpdate ethClient pool cfg Nothing update

backfillPendingOrderRevealPayloads :: Manager -> EthClient -> DbPool -> Config -> IO (Either T.Text ())
backfillPendingOrderRevealPayloads manager ethClient pool cfg = do
  pending <- withDb pool $ \conn -> getPendingPerpsKeeperOrders conn (cfgPerpsOrderRouter cfg) 20
  case pending of
    [] -> pure $ Right ()
    _ -> do
      settlementWindowResult <- orderSettlementWindow ethClient (cfgPerpsPletherOracle cfg)
      case settlementWindowResult of
        Left err ->
          pure $ Left $ "could not read the on-chain order settlement window: " <> T.pack (show err)
        Right settlementWindow -> do
          forM_ pending $ \order -> do
            let firstRevealTick = pkorCommitTime order + 1
                maxRevealTick = pkorCommitTime order + settlementWindow
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
                  case validateRevealWindow (pkorCommitTime order) settlementWindow (hbuPublishTimes update) of
                    Left err ->
                      logWarnEvery
                        60
                        "reveal_payload_backfill_invalid"
                        "Reveal payload backfill returned an unusable payload"
                        [ field "order_id" $ pkorOrderId order
                        , field "error" err
                        ]
                    Right _ -> do
                      cacheResult <-
                        cacheBasketUpdate
                          ethClient
                          pool
                          cfg
                          (Just (firstRevealTick, maxRevealTick))
                          update
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

cacheBasketUpdate
  :: EthClient
  -> DbPool
  -> Config
  -> Maybe (Integer, Integer) -- exact on-chain order bounds for historical updates
  -> HermesBasketUpdate
  -> IO (Either T.Text ())
cacheBasketUpdate ethClient pool cfg historicalBounds update =
  case promotePythPayloadSource (hbuSource update) of
    Nothing -> pure $ Left $ "unsupported Hermes payload source: " <> hbuSource update
    Just admittedSource ->
      case validateCachePublishTimes cfg update of
        Left err -> pure $ Left err
        Right (minPublishTime, maxPublishTime) -> do
          case decodeAdmissionInputs update of
            Left err -> pure $ Left err
            Right (updateData, feedIds) -> do
              admission <- admitCachePayload
                ethClient
                cfg
                historicalBounds
                update
                updateData
                feedIds
                minPublishTime
                maxPublishTime
              case admission of
                Left err ->
                  pure $
                    Left $
                      "Pyth rejected Hermes payload before cache promotion: "
                        <> T.pack (show err)
                Right signedPoints ->
                  case basketSnapshotFromSignedPrices update signedPoints of
                    Left err ->
                      pure $
                        Left $
                          "Pyth signed prices did not match Hermes metadata: " <> err
                    Right (signedBasketPrice, signedComponents) -> do
                      let minuteBucket = (minPublishTime `div` 60) * 60
                          signedPublishTimes = map pppPublishTime signedPoints
                      withDb pool $ \conn -> do
                        insertBasketSnapshotWithSource
                          conn
                          minuteBucket
                          60
                          signedBasketPrice
                          (toJSON signedComponents)
                          "pyth_hermes_latest"
                        insertPythUpdatePayload
                          conn
                          minPublishTime
                          maxPublishTime
                          (toJSON signedPublishTimes)
                          (toJSON $ hbuUpdateData update)
                          (hbuFetchedAt update)
                          admittedSource
                      logInfoEvery
                        300
                        "basket_cache_progress"
                        "Pyth basket update was decoded on-chain and cached"
                        [ field "min_publish_time" minPublishTime
                        , field "max_publish_time" maxPublishTime
                        , field "minute_bucket" minuteBucket
                        , field "source" admittedSource
                        ]
                      pure $ Right ()

admitCachePayload
  :: EthClient
  -> Config
  -> Maybe (Integer, Integer)
  -> HermesBasketUpdate
  -> [ByteString]
  -> [ByteString]
  -> Integer
  -> Integer
  -> IO (Either RpcError [PythPricePoint])
admitCachePayload ethClient cfg historicalBounds update updateData feedIds minPublishTime maxPublishTime
  | hbuSource update == "backend_hermes_latest" =
      parsePythUpdateData
        ethClient
        (cfgPerpsPletherOracle cfg)
        updateData
        feedIds
        minPublishTime
        maxPublishTime
  | otherwise =
      case historicalBounds of
        Nothing ->
          pure $ Left $ RpcJsonError "historical Pyth admission requires the on-chain order reveal bounds"
        Just (routeMinPublishTime, routeMaxPublishTime) ->
          parseUniquePythUpdateData
            ethClient
            (cfgPerpsPletherOracle cfg)
            updateData
            feedIds
            routeMinPublishTime
            routeMaxPublishTime

basketSnapshotFromSignedPrices
  :: HermesBasketUpdate
  -> [PythPricePoint]
  -> Either T.Text (Integer, [BasketComponentPrice])
basketSnapshotFromSignedPrices update signedPoints
  | map pppPublishTime signedPoints /= hbuPublishTimes update =
      Left "signed PriceFeed[] publish times differed from Hermes parsed publish times"
  | otherwise = computeBasketSnapshot signedPoints

validateCachePublishTimes :: Config -> HermesBasketUpdate -> Either T.Text (Integer, Integer)
validateCachePublishTimes cfg update
  | hbuSource update == "backend_hermes_latest" =
      validateLatestPublishTimes
        (hbuFetchedAt update)
        (cfgPythLatestMaxAgeSeconds cfg)
        (hbuPublishTimes update)
  | otherwise = validatePublishTimes (hbuPublishTimes update)

decodeAdmissionInputs :: HermesBasketUpdate -> Either T.Text ([ByteString], [ByteString])
decodeAdmissionInputs update = do
  updateData <- traverse decodeUpdateData (zip [0 :: Int ..] $ hbuUpdateData update)
  whenNull updateData "Hermes payload did not include update data"
  feedIds <- traverse decodeFeedId basketComponents
  pure (updateData, feedIds)
  where
    decodeUpdateData (index, encoded) =
      mapLeft
        (\err -> "Hermes update data item " <> T.pack (show index) <> " is invalid: " <> err)
        (hexToByteStringEither encoded)

    decodeFeedId component = do
      feedId <-
        mapLeft
          (\err -> "configured feed " <> bcFeedId component <> " is invalid: " <> err)
          (hexToByteStringEither $ bcFeedId component)
      if BS.length feedId == 32
        then Right feedId
        else Left $ "configured feed " <> bcFeedId component <> " is not 32 bytes"

whenNull :: [a] -> T.Text -> Either T.Text ()
whenNull [] err = Left err
whenNull _ _ = Right ()

mapLeft :: (a -> b) -> Either a value -> Either b value
mapLeft f result =
  case result of
    Left err -> Left $ f err
    Right value -> Right value

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
