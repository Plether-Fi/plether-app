module Main (main) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (SomeException, displayException, try)
import Control.Monad (forM_, when)
import Data.Aeson (toJSON)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Database.PostgreSQL.Simple (withTransaction)
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Plether.Config (Config (..), PerpsCandleWriteMode (..), loadConfig)
import Plether.Database (DbPool, newDbPool, withDb)
import Plether.Database.Candles
  ( BasketObservationInput (..)
  , PriceGapRecoveryResult (..)
  , RollupCoverage (..)
  , RollupKind (PriceRollup)
  , advanceBasketPriceCoverage
  , defaultBasketSeriesId
  , ensureCurrentBasketDefinition
  , getRollupCoverage
  , recomputeBasketCandleHierarchy
  , recoverBasketPriceCoverageGap
  , upsertBasketObservation
  )
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
import Plether.Pyth.History
  ( BasketHistoryActivity (..)
  , BasketIngestorConfig (..)
  , basketObservationId
  , fetchBasketHistoryActivity
  , runBasketBackfill
  , startBasketHistoryIngestor
  )
import Plether.Pyth.RevealPayload
  ( PythPayloadAdmission (..)
  , classifyPythPayloadAdmission
  , maxComponentPublishTimeDivergence
  , validatePublishTimes
  , validateRevealWindow
  )
import Plether.Perps.ClosedPriceGap (validateClosedPriceGapEvidence)
import Plether.Utils.Hex (hexToByteStringEither)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.Read (readMaybe)

data WorkerMode
  = RunOnce
  | LatestLoop
  | BackfillOnce
  | RecoverClosedPriceGap
  deriving (Eq, Show)

data WorkerArgs = WorkerArgs
  { waMode :: WorkerMode
  , waPollSeconds :: Int
  , waBackfillDays :: Maybe Int
  , waExpectedCoverageEnd :: Maybe Integer
  , waRecoverBefore :: Maybe Integer
  , waRequestedBy :: Maybe T.Text
  , waRequestReference :: Maybe T.Text
  }
  deriving (Show)

defaultPollSeconds :: Int
defaultPollSeconds = 5

main :: IO ()
main = do
  rawArgs <- getArgs
  case parseWorkerArgs rawArgs of
    Left err -> do
      logError
        "basket_worker_arguments_invalid"
        "Basket worker arguments are invalid"
        [field "error" err]
      exitFailure
    Right args -> runWorker args

runWorker :: WorkerArgs -> IO ()
runWorker args = do
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
              LatestLoop -> do
                when (candleWritesEnabled cfg) $ do
                  _ <-
                    forkIO $
                      startBasketHistoryIngestor
                        manager
                        pool
                        BasketIngestorConfig
                          { bicBenchmarksUrl = cfgPythBenchmarksUrl cfg
                          , bicApiKey = cfgPythApiKey cfg
                          , bicChainId = cfgPerpsChainId cfg
                          , bicBackfillDays = cfgPythBackfillDays cfg
                          , bicOwnHistoryTargets = True
                          , bicSampleIntervalSeconds = cfgPythSampleIntervalSeconds cfg
                          , bicPollSeconds = 15 * 60
                          , bicCandleWriteMode = cfgPerpsCandleWriteMode cfg
                          , bicCandleLatenessSeconds = cfgPerpsCandleLatenessSeconds cfg
                          }
                  pure ()
                latestLoop manager ethClient pool cfg (waPollSeconds args)
              BackfillOnce -> do
                let backfillDays = fromMaybe (cfgPythBackfillDays cfg) (waBackfillDays args)
                runBasketBackfill manager pool BasketIngestorConfig
                  { bicBenchmarksUrl = cfgPythBenchmarksUrl cfg
                  , bicApiKey = cfgPythApiKey cfg
                  , bicChainId = cfgPerpsChainId cfg
                  , bicBackfillDays = backfillDays
                  , bicOwnHistoryTargets = False
                  , bicSampleIntervalSeconds = cfgPythSampleIntervalSeconds cfg
                  , bicPollSeconds = 0
                  , bicCandleWriteMode = cfgPerpsCandleWriteMode cfg
                  , bicCandleLatenessSeconds = cfgPerpsCandleLatenessSeconds cfg
                  }
              RecoverClosedPriceGap ->
                case
                    ( waExpectedCoverageEnd args
                    , waRecoverBefore args
                    , waRequestedBy args
                    , waRequestReference args
                    )
                  of
                    (Just coverageEnd, Just recoverBefore, Just requestedBy, Just requestReference) -> do
                      result <-
                        runClosedPriceGapRecovery
                          manager
                          ethClient
                          pool
                          cfg
                          coverageEnd
                          recoverBefore
                          requestedBy
                          requestReference
                      case result of
                        Left err -> do
                          logError
                            "basket_price_gap_recovery_failed"
                            "Closed-market basket price coverage recovery failed"
                            [ field "expected_coverage_end" coverageEnd
                            , field "recover_before" recoverBefore
                            , field "requested_by" requestedBy
                            , field "request_reference" requestReference
                            , field "error" err
                            ]
                          exitFailure
                        Right recovery ->
                          logInfo
                            "basket_price_gap_recovered"
                            "Closed-market basket price coverage was safely republished"
                            [ field "previous_coverage_end" $ pgrPreviousCoverageEnd recovery
                            , field "recovered_through" $ pgrRecoveredThrough recovery
                            , field "generation" $ pgrGeneration recovery
                            , field "requested_by" requestedBy
                            , field "request_reference" requestReference
                            ]
                    _ -> do
                      logError
                        "basket_worker_arguments_invalid"
                        "Closed price-gap recovery arguments are incomplete"
                        []
                      exitFailure

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

runClosedPriceGapRecovery
  :: Manager
  -> EthClient
  -> DbPool
  -> Config
  -> Integer
  -> Integer
  -> T.Text
  -> T.Text
  -> IO (Either T.Text PriceGapRecoveryResult)
runClosedPriceGapRecovery manager ethClient pool cfg expectedCoverageEnd recoverBefore requestedBy requestReference
  | cfgPerpsChainId cfg /= 421_614 =
      pure $ Left "Closed price-gap recovery is restricted to Sepolia chain 421614"
  | not $ candleWritesEnabled cfg =
      pure $ Left "Closed price-gap recovery requires PERPS_CANDLE_WRITE_MODE=dual"
  | otherwise = do
      latestResult <- fetchLatestBasketUpdate manager cfg
      case latestResult of
        Left err -> pure $ Left $ "could not fetch latest signed Pyth evidence: " <> err
        Right update -> do
          signedResult <- verifyLatestRecoveryPayload ethClient cfg update
          case signedResult of
            Left err -> pure $ Left err
            Right (minPublishTime, maxPublishTime, signedBasketPrice, signedComponents) -> do
              historyResult <-
                fetchBasketHistoryActivity
                  manager
                  (cfgPythBenchmarksUrl cfg)
                  (cfgPythApiKey cfg)
                  expectedCoverageEnd
                  (hbuFetchedAt update)
              case historyResult of
                Left err -> pure $ Left $ "could not fetch Pyth closed-gap history evidence: " <> err
                Right activity ->
                  case
                      validateClosedPriceGapEvidence
                        expectedCoverageEnd
                        (hbuFetchedAt update)
                        recoverBefore
                        maxPublishTime
                        (map bhaTimestamps activity)
                    of
                      Left err -> pure $ Left err
                      Right () -> do
                        databaseResult <-
                          try $
                            withDb pool $ \conn ->
                              withTransaction conn $
                                recoverBasketPriceCoverageGap
                                  conn
                                  defaultBasketSeriesId
                                  expectedCoverageEnd
                                  (hbuFetchedAt update)
                                  minPublishTime
                                  signedBasketPrice
                                  (toJSON signedComponents)
                                  (cfgPerpsCandleLatenessSeconds cfg)
                        case databaseResult of
                          Left err ->
                            pure $
                              Left $
                                "closed price-gap database publication failed: "
                                  <> T.pack (displayException (err :: SomeException))
                          Right recovery -> do
                            logInfo
                              "basket_price_gap_evidence_verified"
                              "Verified signed and historical Pyth evidence for a closed-market coverage gap"
                              [ field "expected_coverage_end" expectedCoverageEnd
                              , field "checked_through" $ hbuFetchedAt update
                              , field "recover_before" recoverBefore
                              , field "min_publish_time" minPublishTime
                              , field "max_publish_time" maxPublishTime
                              , field "history_feed_count" $ length activity
                              , field "history_update_count" $ sum $ map (length . bhaTimestamps) activity
                              , field "history_feed_symbols" $ map bhaFeedSymbol activity
                              , field "requested_by" requestedBy
                              , field "request_reference" requestReference
                              ]
                            pure $ Right recovery

verifyLatestRecoveryPayload
  :: EthClient
  -> Config
  -> HermesBasketUpdate
  -> IO (Either T.Text (Integer, Integer, Integer, [BasketComponentPrice]))
verifyLatestRecoveryPayload ethClient cfg update
  | not $ isLatestUpdate update = pure $ Left "Closed price-gap evidence is not a latest-source payload"
  | otherwise =
      case validateCachePublishTimes update of
        Left err -> pure $ Left err
        Right (minPublishTime, maxPublishTime) ->
          case decodeAdmissionInputs update of
            Left err -> pure $ Left err
            Right (updateData, feedIds) -> do
              admission <-
                admitCachePayload
                  ethClient
                  cfg
                  AdmitLatestPayload
                  updateData
                  feedIds
                  minPublishTime
                  maxPublishTime
              case admission of
                Left err ->
                  pure $
                    Left $
                      "Pyth rejected latest recovery evidence on-chain: "
                        <> T.pack (show err)
                Right signedPoints ->
                  pure $ do
                    (signedBasketPrice, signedComponents) <-
                      basketSnapshotFromSignedPrices update signedPoints
                    pure
                      ( minPublishTime
                      , maxPublishTime
                      , signedBasketPrice
                      , signedComponents
                      )

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
  case
      ( promotePythPayloadSource $ hbuSource update
      , classifyPythPayloadAdmission historicalBounds $ hbuSource update
      )
    of
      (Nothing, _) -> pure $ Left $ "unsupported Hermes payload source: " <> hbuSource update
      (_, Left err) -> pure $ Left err
      (Just admittedSource, Right admissionMode) ->
        case validateCachePublishTimes update of
          Left err -> pure $ Left err
          Right (minPublishTime, maxPublishTime) -> case decodeAdmissionInputs update of
            Left err -> pure $ Left err
            Right (updateData, feedIds) -> do
              admission <- admitCachePayload
                ethClient
                cfg
                admissionMode
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
                      let latestSourcePoll = admissionMode == AdmitLatestPayload
                      if
                        latestSourcePoll
                          && isStaleLatestUpdate
                            (cfgPythLatestMaxAgeSeconds cfg)
                            update
                            minPublishTime
                        then do
                          -- A stale but signed latest response is expected when
                          -- the underlying markets are closed. It proves that
                          -- Hermes had no newer update as of the fetch time,
                          -- but it must not be promoted as a new observation or
                          -- reveal payload.
                          when (candleWritesEnabled cfg) $ do
                            withDb pool $ \conn -> withTransaction conn $ do
                              -- Coverage is meaningful only for the exact
                              -- compiled immutable definition. A stale/closed
                              -- market poll must fail closed on configuration
                              -- drift just like an admitted fresh observation;
                              -- the coverage primitive performs that check.
                              advanceBasketPriceCoverage
                                conn
                                defaultBasketSeriesId
                                (hbuFetchedAt update)
                                (cfgPerpsCandleLatenessSeconds cfg)
                            emitPriceWriterHeartbeat
                              pool
                              cfg
                              update
                              minPublishTime
                              maxPublishTime
                              admittedSource
                              "stale_latest_no_update"
                              "A signed stale latest response completed a basket source watermark poll"
                          pure $ Right ()
                        else do
                          let minuteBucket = (minPublishTime `div` 60) * 60
                              signedPublishTimes = map pppPublishTime signedPoints
                          withDb pool $ \conn -> withTransaction conn $ do
                            -- Persist the full admitted observation before
                            -- writing the legacy minute snapshot, which
                            -- overwrites intra-minute information. The latest
                            -- source watermark shares this transaction so
                            -- neither rollup coverage nor dual-write data can
                            -- advance independently.
                            when (candleWritesEnabled cfg) $ do
                              ensureCurrentBasketDefinition conn defaultBasketSeriesId
                              changed <-
                                upsertBasketObservation
                                  conn
                                  BasketObservationInput
                                    { boiSeriesId = defaultBasketSeriesId
                                    , boiObservationId = basketObservationId defaultBasketSeriesId signedPoints
                                    , boiPublishTime = minPublishTime
                                    , boiBasketPrice = signedBasketPrice
                                    , boiComponentPrices = toJSON signedComponents
                                    , boiSource = admittedSource
                                    , boiSourcePriority = signedObservationPriority
                                    }
                              when changed $
                                recomputeBasketCandleHierarchy
                                  conn
                                  defaultBasketSeriesId
                                  minPublishTime
                                  (cfgPerpsCandleLatenessSeconds cfg)
                            when (candleWritesEnabled cfg && latestSourcePoll) $
                              advanceBasketPriceCoverage
                                conn
                                defaultBasketSeriesId
                                (hbuFetchedAt update)
                                (cfgPerpsCandleLatenessSeconds cfg)
                            insertBasketSnapshotWithSource
                              conn
                              minuteBucket
                              60
                              signedBasketPrice
                              (toJSON signedComponents)
                              admittedSource
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
                          when (candleWritesEnabled cfg && latestSourcePoll) $
                            emitPriceWriterHeartbeat
                              pool
                              cfg
                              update
                              minPublishTime
                              maxPublishTime
                              admittedSource
                              "admitted_latest"
                              "An admitted latest Pyth update completed a basket source watermark poll"
                          pure $ Right ()

signedObservationPriority :: Int
signedObservationPriority = 100

candleWritesEnabled :: Config -> Bool
candleWritesEnabled cfg = cfgPerpsCandleWriteMode cfg == PerpsCandleWritesDual

-- Keep operational reads outside the ingestion transaction. A monitoring
-- query must never roll back an already validated observation or watermark;
-- its own failure suppresses the heartbeat and is surfaced separately.
emitPriceWriterHeartbeat
  :: DbPool
  -> Config
  -> HermesBasketUpdate
  -> Integer
  -> Integer
  -> T.Text
  -> T.Text
  -> T.Text
  -> IO ()
emitPriceWriterHeartbeat pool cfg update minPublishTime maxPublishTime source reason message = do
  coverageResult <-
    ( try $
        withDb pool $ \conn ->
          getRollupCoverage
            conn
            PriceRollup
            (Just defaultBasketSeriesId)
            Nothing
            Nothing
            60
    ) :: IO (Either SomeException (Maybe RollupCoverage))
  case coverageResult of
    Left err ->
      logErrorEvery
        60
        "basket_price_writer_heartbeat_failed"
        "Basket price candle writer could not read its coverage heartbeat"
        [field "error" $ displayException err]
    Right priceCoverage ->
      logInfoEvery
        300
        "basket_price_watermark_advanced"
        message
        [ field "checked_through" $ hbuFetchedAt update
        , field "min_publish_time" minPublishTime
        , field "max_publish_time" maxPublishTime
        , field "source" source
        , field "watermark_reason" reason
        , field "writer_kind" ("price" :: T.Text)
        , field "service" ("plether-basket-worker" :: T.Text)
        , field "coverage_interval_seconds" (60 :: Integer)
        , field "coverage_expected_lateness_seconds" $
            cfgPerpsCandleLatenessSeconds cfg
        , field "coverage_state" $ coverageState priceCoverage
        , field "coverage_finalized_through" $ priceCoverage >>= rcFinalizedThrough
        , field "coverage_lag_seconds" $
            normalizedCoverageLag
              (hbuFetchedAt update)
              60
              (cfgPerpsCandleLatenessSeconds cfg)
              (priceCoverage >>= rcFinalizedThrough)
        , field "coverage_error" $ priceCoverage >>= rcLastError
        ]

-- Coverage can legitimately be absent between the additive migration and the
-- first backfill. Once present, an incomplete row is actionable, while lag is
-- normalized by the base bucket so normal alignment never looks stale.
coverageState :: Maybe RollupCoverage -> T.Text
coverageState coverageResult = case coverageResult of
  Nothing -> "uninitialized"
  Just coverage
    | rcComplete coverage -> "complete"
    | otherwise -> "incomplete"

normalizedCoverageLag :: Integer -> Integer -> Integer -> Maybe Integer -> Maybe Integer
normalizedCoverageLag now interval expectedLateness =
  fmap $ \finalizedThrough ->
    max 0 (now - finalizedThrough - interval - max 0 expectedLateness)

admitCachePayload
  :: EthClient
  -> Config
  -> PythPayloadAdmission
  -> [ByteString]
  -> [ByteString]
  -> Integer
  -> Integer
  -> IO (Either RpcError [PythPricePoint])
admitCachePayload ethClient cfg admissionMode updateData feedIds minPublishTime maxPublishTime =
  case admissionMode of
    AdmitLatestPayload ->
      parsePythUpdateData
        ethClient
        (cfgPerpsPletherOracle cfg)
        updateData
        feedIds
        minPublishTime
        maxPublishTime
    AdmitHistoricalPayload routeMinPublishTime routeMaxPublishTime ->
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
  | otherwise = do
      snapshot@(signedBasketPrice, signedComponents) <- computeBasketSnapshot signedPoints
      if signedBasketPrice /= hbuBasketPrice update
        then Left "signed basket price differed from Hermes parsed basket price"
        else
          if toJSON signedComponents /= hbuComponents update
            then Left "signed component prices differed from Hermes parsed component prices"
            else Right snapshot

validateCachePublishTimes :: HermesBasketUpdate -> Either T.Text (Integer, Integer)
validateCachePublishTimes update = do
  bounds@(_, maximumTs) <- validatePublishTimes $ hbuPublishTimes update
  let futureSkew = maximumTs - hbuFetchedAt update
  if isLatestUpdate update && futureSkew > maxComponentPublishTimeDivergence
    then
      Left $
        "latest payload publish time is "
          <> T.pack (show futureSkew)
          <> "s in the future"
    else Right bounds

isLatestUpdate :: HermesBasketUpdate -> Bool
isLatestUpdate update = hbuSource update == "backend_hermes_latest"

isStaleLatestUpdate :: Integer -> HermesBasketUpdate -> Integer -> Bool
isStaleLatestUpdate maximumAge update minimumTs =
  isLatestUpdate update
    && hbuFetchedAt update - minimumTs > max 0 maximumAge

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

parseWorkerArgs :: [String] -> Either T.Text WorkerArgs
parseWorkerArgs args
  | "--recover-closed-price-gap" `elem` args = parseRecoveryArgs args
  | otherwise =
      Right
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
          , waExpectedCoverageEnd = Nothing
          , waRecoverBefore = Nothing
          , waRequestedBy = Nothing
          , waRequestReference = Nothing
          }

parseRecoveryArgs :: [String] -> Either T.Text WorkerArgs
parseRecoveryArgs args = do
  when
    (length args /= 9 || any (`elem` args) ["--latest-loop", "--backfill-once"])
    (Left "Closed price-gap recovery accepts exactly four named values and no other worker mode")
  expectedCoverageEnd <- requireRecoveryInteger "--expected-coverage-end" args
  recoverBefore <- requireRecoveryInteger "--recover-before" args
  requestedBy <- requireRecoveryText "--requested-by" args
  requestReference <- requireRecoveryText "--request-reference" args
  pure
    WorkerArgs
      { waMode = RecoverClosedPriceGap
      , waPollSeconds = defaultPollSeconds
      , waBackfillDays = Nothing
      , waExpectedCoverageEnd = Just expectedCoverageEnd
      , waRecoverBefore = Just recoverBefore
      , waRequestedBy = Just requestedBy
      , waRequestReference = Just requestReference
      }

requireRecoveryInteger :: String -> [String] -> Either T.Text Integer
requireRecoveryInteger name args =
  case lookupFlag name args >>= readMaybe of
    Just value | value >= 0 && value <= 4_102_444_800 -> Right value
    _ -> Left $ T.pack name <> " must be a Unix timestamp from 0 through 4102444800"

requireRecoveryText :: String -> [String] -> Either T.Text T.Text
requireRecoveryText name args =
  case T.strip . T.pack <$> lookupFlag name args of
    Just value | not (T.null value) && T.length value <= 200 -> Right value
    _ -> Left $ T.pack name <> " must be non-blank and at most 200 characters"

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
