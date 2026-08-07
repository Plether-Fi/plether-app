module Plether.Handlers.Perps
  ( getBasketHistory
  , basketHistoryPointsWithVolume
  , getBasketLatest
  , getCachedLatestPythUpdate
  , getPythUpdate
  , getRevealPayload
  , PythUpdateAdmission (..)
  , decodePythUpdateForAdmission
  ) where

import Data.Aeson (FromJSON (..), Value, eitherDecode, withObject, (.:))
import qualified Data.Aeson as Aeson
import Control.Concurrent.STM
  ( atomically
  , modifyTVar'
  , readTVar
  , writeTVar
  )
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.List (sort)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Network.HTTP.Client
  ( Manager
  , httpLbs
  , parseRequest
  , requestHeaders
  , responseBody
  , responseHeaders
  , responseStatus
  , setQueryString
  )
import Network.HTTP.Types.Status (statusCode)
import Plether.Cache (AppCache (..))
import Plether.Config (Config (..))
import Plether.Database (DbPool, withDb)
import Plether.Database.Schema
  ( BasketHistorySnapshotRow (..)
  , BasketSnapshotRow (..)
  , PerpsMarketVolumeBucketRow (..)
  , PythUpdatePayloadRow (..)
  , getBasketSnapshots
  , getPerpsMarketVolumeBuckets
  , getLatestBasketSnapshot
  , getLatestPythUpdatePayload
  , getPythUpdatePayloadForWindow
  , isHistoricalRevealPayload
  )
import Plether.Types
import qualified Plether.Types.Error as E
import Plether.Ethereum.Client (EthClient)
import Plether.Ethereum.Contracts.Perps (validatePythUpdateData, validateUniquePythUpdateData)
import Plether.Pyth.Basket (BasketComponent (..), basketComponents, normalizeFeedId)
import Plether.Pyth.Hermes (resolveHermesApiKey)
import Plether.Pyth.RevealPayload (validateLatestPublishTimes, validatePublishTimes)
import Plether.Utils.Hex (hexToByteStringEither)

getBasketHistory
  :: DbPool
  -> Config
  -> BasketHistoryParams
  -> IO (Either ApiError (ApiResponse BasketHistory))
getBasketHistory pool cfg params = do
  now <- getPOSIXTime
  let nowUnix = round now
      fromUnix = nowUnix - basketRangeSeconds (bhpRange params)
      interval = max 60 (bhpIntervalSeconds params)
      maxPoints = fromIntegral ((basketRangeSeconds (bhpRange params) `div` interval) + 4)

  (rows, volumeRows) <- withDb pool $ \conn -> do
    snapshots <-
      getBasketSnapshots conn fromUnix nowUnix interval maxPoints (bhpIncludeComponents params)
    volumes <-
      getPerpsMarketVolumeBuckets
        conn
        (cfgPerpsChainId cfg)
        (cfgPerpsOrderRouter cfg)
        fromUnix
        nowUnix
        interval
    pure (snapshots, volumes)

  let points = basketHistoryPointsWithVolume interval rows volumeRows
      latest = case reverse rows of
        row : _ -> Just (bhsrBasketPrice row)
        [] -> Nothing
      changePct = computeChange rows
      history =
        BasketHistory
          { bhRange = bhpRange params
          , bhIntervalSeconds = interval
          , bhSource = "pyth_benchmarks"
          , bhGeneratedAt = now
          , bhLatestPrice = latest
          , bhChangePct = changePct
          , bhPoints = points
          }

  pure $ Right $ mkResponse 0 (cfgChainId cfg) history

basketHistoryPointsWithVolume
  :: Integer
  -> [BasketHistorySnapshotRow]
  -> [PerpsMarketVolumeBucketRow]
  -> [BasketHistoryPoint]
basketHistoryPointsWithVolume intervalSeconds rows volumeRows =
  map rowToPoint rows
  where
    interval = max 1 intervalSeconds
    volumeByBucket =
      Map.fromList
        [ (pmvbrBucket row, pmvbrVolumeUsdc row)
        | row <- volumeRows
        ]
    rowToPoint BasketHistorySnapshotRow {..} =
      BasketHistoryPoint
        { bhpTimestamp = bhsrTimestamp
        , bhpBasketPrice = bhsrBasketPrice
        , bhpVolumeUsdc = Map.findWithDefault 0 (bhsrTimestamp `div` interval) volumeByBucket
        , bhpComponents = bhsrComponents
        }

computeChange :: [BasketHistorySnapshotRow] -> Maybe Double
computeChange rows =
  case (rows, reverse rows) of
    (first : _, lastRow : _) | bhsrBasketPrice first > 0 ->
      Just $
        (fromIntegral (bhsrBasketPrice lastRow - bhsrBasketPrice first) / fromIntegral (bhsrBasketPrice first) :: Double)
    _ -> Nothing

getBasketLatest
  :: DbPool
  -> Config
  -> IO (Either ApiError (ApiResponse BasketLatest))
getBasketLatest pool cfg = do
  now <- getPOSIXTime
  mRow <- withDb pool getLatestBasketSnapshot
  pure $ case mRow of
    Nothing ->
      Left $ E.internalError "No perps basket snapshots are available yet. Start plether-basket-worker --once or --latest-loop."
    Just BasketSnapshotRow {..} ->
      Right $
        mkResponse 0 (cfgChainId cfg) $
          BasketLatest
            { blTimestamp = bsrTimestamp
            , blBasketPrice = bsrBasketPrice
            , blComponents = bsrComponents
            , blGeneratedAt = now
            , blSource = "database"
            }

getRevealPayload
  :: DbPool
  -> EthClient
  -> Config
  -> Integer
  -> Integer
  -> Integer
  -> IO (Either ApiError (ApiResponse RevealPayloadResponse))
getRevealPayload pool perpsClient cfg orderId minPublishTime maxPublishTime = do
  mRow <- withDb pool $ \conn ->
    getPythUpdatePayloadForWindow conn minPublishTime maxPublishTime
  case mRow of
    Nothing ->
      pure $ Left $
        E.networkError $
          "Reveal payload unavailable for order "
            <> T.pack (show orderId)
            <> ". The basket worker has not cached the first post-commit six-feed Pyth update starting at "
            <> T.pack (show minPublishTime)
            <> " within reveal window ending at "
            <> T.pack (show maxPublishTime)
            <> ". Keep plether-basket-worker --latest-loop running and retry before the order expires."
    Just row | not (isHistoricalRevealPayload row) ->
      pure $ Left $
        E.networkError $
          "Exact reveal payload unavailable for order "
            <> T.pack (show orderId)
            <> ". The cached row for the first post-commit tick came from "
            <> puprSource row
            <> ", so the app should retry with exact historical Pyth data."
    Just row ->
      case rowToRevealPayload orderId row of
        Left err -> pure $ Left $ E.internalError err
        Right payload -> do
          validation <-
            validateStoredPythUpdate
              perpsClient
              cfg
              (Just (minPublishTime, maxPublishTime))
              row
          pure $ case validation of
            Left err -> Left err
            Right _ -> Right $ mkResponse 0 (cfgChainId cfg) payload

rowToRevealPayload :: Integer -> PythUpdatePayloadRow -> Either Text RevealPayloadResponse
rowToRevealPayload orderId PythUpdatePayloadRow {..} = do
  publishTimes <- decodeValue "publish_times" puprPublishTimes
  updateData <- decodeValue "update_data" puprUpdateData
  pure
    RevealPayloadResponse
      { rprOrderId = orderId
      , rprUpdateData = updateData
      , rprFetchedAt = puprFetchedAt
      , rprPublishTimes = publishTimes
      , rprMinPublishTime = puprMinPublishTime
      , rprMaxPublishTime = puprMaxPublishTime
      , rprSource = puprSource
      }

decodeValue :: (FromJSON a) => Text -> Value -> Either Text a
decodeValue label value =
  case Aeson.fromJSON value of
    Aeson.Success parsed -> Right parsed
    Aeson.Error err -> Left $ "Could not decode cached reveal " <> label <> ": " <> T.pack err

getCachedLatestPythUpdate
  :: DbPool
  -> EthClient
  -> Config
  -> IO (Either ApiError (ApiResponse PythUpdateResponse))
getCachedLatestPythUpdate pool perpsClient cfg = do
  mRow <- withDb pool getLatestPythUpdatePayload
  case mRow of
    Nothing ->
      pure $ Left $
        E.networkError
          "No cached Pyth update payload is available yet. Keep plether-basket-worker --latest-loop running."
    Just row -> do
      validation <- validateStoredPythUpdate perpsClient cfg Nothing row
      pure $ case validation of
        Left err -> Left err
        Right admission -> Right $ mkResponse 0 (cfgChainId cfg) (puaPayload admission)

validateStoredPythUpdate
  :: EthClient
  -> Config
  -> Maybe (Integer, Integer)
  -> PythUpdatePayloadRow
  -> IO (Either ApiError PythUpdateAdmission)
validateStoredPythUpdate perpsClient cfg mHistoricalBounds row =
  case storedPythUpdateAdmission row of
    Left err -> pure $ Left err
    Right admission -> do
      validation <-
        case mHistoricalBounds of
          -- Unique parsing proves this is the first eligible update after the
          -- order's lower bound, so it must receive the full reveal window.
          Just (minPublishTime, maxPublishTime) ->
            validateUniquePythUpdateData
              perpsClient
              (cfgPerpsPletherOracle cfg)
              (puaUpdateData admission)
              (puaFeedIds admission)
              minPublishTime
              maxPublishTime
          Nothing ->
            validatePythUpdateData
              perpsClient
              (cfgPerpsPletherOracle cfg)
              (puaUpdateData admission)
              (puaFeedIds admission)
              (puaMinPublishTime admission)
              (puaMaxPublishTime admission)
      pure $ case validation of
        Left err -> Left $ rpcErrorToApiError err
        Right () -> Right admission

storedPythUpdateAdmission :: PythUpdatePayloadRow -> Either ApiError PythUpdateAdmission
storedPythUpdateAdmission PythUpdatePayloadRow {..} = do
  publishTimes <- mapLeft E.internalError $ decodeValue "publish_times" puprPublishTimes
  encodedUpdateData <- mapLeft E.internalError $ decodeValue "update_data" puprUpdateData
  if length publishTimes /= length basketComponents
    then Left $ E.internalError "Cached Pyth row does not include exactly six feed publish times"
    else Right ()
  (actualMinPublishTime, actualMaxPublishTime) <-
    mapAdmissionError $ validatePublishTimes publishTimes
  if actualMinPublishTime /= puprMinPublishTime || actualMaxPublishTime /= puprMaxPublishTime
    then Left $ E.internalError "Cached Pyth row publish-time metadata does not match its payload window"
    else Right ()
  (updateData, feedIds) <- decodeAdmissionByteStrings encodedUpdateData
  Right
    PythUpdateAdmission
      { puaPayload =
          PythUpdateResponse
            { purUpdateData = prefixHex <$> encodedUpdateData
            , purFetchedAt = puprFetchedAt
            , purPublishTimes = publishTimes
            , purSource = puprSource
            }
      , puaUpdateData = updateData
      , puaFeedIds = feedIds
      , puaMinPublishTime = puprMinPublishTime
      , puaMaxPublishTime = puprMaxPublishTime
      }
  where
    mapAdmissionError = mapLeft (E.internalError . ("Cached Pyth row failed admission checks: " <>))

data HermesBinary = HermesBinary
  { hbData :: [Text]
  }
  deriving stock (Show)

instance FromJSON HermesBinary where
  parseJSON = withObject "HermesBinary" $ \v ->
    HermesBinary <$> v .: "data"

data HermesPrice = HermesPrice
  { hpPublishTime :: Integer
  }
  deriving stock (Show)

instance FromJSON HermesPrice where
  parseJSON = withObject "HermesPrice" $ \v ->
    HermesPrice <$> v .: "publish_time"

data HermesParsedPrice = HermesParsedPrice
  { hppFeedId :: Text
  , hppPrice :: HermesPrice
  }
  deriving stock (Show)

instance FromJSON HermesParsedPrice where
  parseJSON = withObject "HermesParsedPrice" $ \v ->
    HermesParsedPrice
      <$> v .: "id"
      <*> v .: "price"

data HermesUpdateResponse = HermesUpdateResponse
  { hurBinary :: HermesBinary
  , hurParsed :: [HermesParsedPrice]
  }
  deriving stock (Show)

instance FromJSON HermesUpdateResponse where
  parseJSON = withObject "HermesUpdateResponse" $ \v ->
    HermesUpdateResponse
      <$> v .: "binary"
      <*> v .: "parsed"

data PythUpdateAdmission = PythUpdateAdmission
  { puaPayload :: PythUpdateResponse
  , puaUpdateData :: [ByteString]
  , puaFeedIds :: [ByteString]
  , puaMinPublishTime :: Integer
  , puaMaxPublishTime :: Integer
  }
  deriving stock (Show)

getPythUpdate
  :: AppCache
  -> Manager
  -> EthClient
  -> Config
  -> Maybe Integer
  -> IO (Either ApiError (ApiResponse PythUpdateResponse))
getPythUpdate cache manager perpsClient cfg mPublishTime =
  case resolveHermesApiKey (cfgPythHermesUrl cfg) (cfgPythApiKey cfg) of
    Left err -> pure $ Left $ E.internalError err
    Right apiKey -> runAuthenticated apiKey
  where
    runAuthenticated apiKey = do
      now <- getPOSIXTime
      mCached <- getCachedPyth now
      case mCached of
        Just cached -> pure $ Right $ mkResponse 0 (cfgChainId cfg) cached
        Nothing -> do
          mCooldown <- getRateLimitCooldown now
          case mCooldown of
            Just retryAfter -> pure $ Left $ E.rateLimitedWithDetails (Just $ BS8.pack $ show retryAfter)
            Nothing -> do
              requestBase <- parseRequest $ T.unpack requestUrl
              let request =
                    setQueryString queryParams requestBase
                      { requestHeaders = authHeaders apiKey <> requestHeaders requestBase
                      }
              response <- httpLbs request manager
              let code = statusCode (responseStatus response)
                  body = responseBody response
              if code == 429
                then do
                  setRateLimitCooldown now (retryAfterHeader response)
                  pure $ Left $ E.rateLimitedWithDetails (retryAfterHeader response)
                else
                  if code < 200 || code >= 300
                    then pure $ Left $ E.networkError $ "Hermes returned HTTP " <> T.pack (show code) <> ": " <> previewBody body
                    else do
                      fetchedAt <- round <$> getPOSIXTime
                      case decodePythUpdateForAdmission mPublishTime fetchedAt (cfgPythLatestMaxAgeSeconds cfg) body of
                        Left err -> pure $ Left err
                        Right admission -> do
                          validation <-
                            case mPublishTime of
                              Nothing ->
                                validatePythUpdateData
                                  perpsClient
                                  (cfgPerpsPletherOracle cfg)
                                  (puaUpdateData admission)
                                  (puaFeedIds admission)
                                  (puaMinPublishTime admission)
                                  (puaMaxPublishTime admission)
                              -- The historical endpoint has no separate maximum;
                              -- Hermes' returned maximum closes the requested window.
                              Just _ ->
                                validateUniquePythUpdateData
                                  perpsClient
                                  (cfgPerpsPletherOracle cfg)
                                  (puaUpdateData admission)
                                  (puaFeedIds admission)
                                  (puaMinPublishTime admission)
                                  (puaMaxPublishTime admission)
                          case validation of
                            Left err -> pure $ Left $ rpcErrorToApiError err
                            Right () -> do
                              let payload = puaPayload admission
                              setCachedPyth now payload
                              pure $ Right $ mkResponse 0 (cfgChainId cfg) payload

    cacheKey =
      maybe "latest" (T.pack . show) mPublishTime

    cacheTtlSeconds :: POSIXTime
    cacheTtlSeconds =
      case mPublishTime of
        Nothing -> 2
        Just _ -> 10 * 60

    getCachedPyth now =
      atomically $ do
        entries <- readTVar (cachePythUpdates cache)
        pure $ case Map.lookup cacheKey entries of
          Just (payload, cachedAt) | now - cachedAt <= cacheTtlSeconds -> Just payload
          _ -> Nothing

    setCachedPyth now payload =
      atomically $
        modifyTVar' (cachePythUpdates cache) $
          Map.insert cacheKey (payload, now)

    getRateLimitCooldown now =
      atomically $ do
        mUntil <- readTVar (cachePythRateLimitUntil cache)
        pure $ case mUntil of
          Just untilTime | untilTime > now -> Just (ceiling (untilTime - now) :: Int)
          _ -> Nothing

    setRateLimitCooldown now retryAfter =
      atomically $
        writeTVar (cachePythRateLimitUntil cache) $
          Just (now + fromIntegral (retryAfterSeconds retryAfter))

    retryAfterSeconds retryAfter =
      case retryAfter >>= BS8.readInteger of
        Just (seconds, _) | seconds > 0 -> fromInteger seconds
        _ -> 15 :: Int

    requestUrl =
      stripTrailingSlash (cfgPythHermesUrl cfg)
        <> "/v2/updates/price/"
        <> maybe "latest" (T.pack . show) mPublishTime

    queryParams =
      ("parsed", Just "true")
        : [("ids[]", Just (encodeUtf8 (bcFeedId component))) | component <- basketComponents]

    authHeaders = \case
      Nothing -> []
      Just key -> [("Authorization", encodeUtf8 $ "Bearer " <> key)]

    retryAfterHeader response =
      lookup "Retry-After" (responseHeaders response)

    previewBody body =
      T.take 180 . T.strip . T.pack $ show (LBS.take 180 body)

decodePythUpdateForAdmission
  :: Maybe Integer
  -> Integer
  -> Integer
  -> LBS.ByteString
  -> Either ApiError PythUpdateAdmission
decodePythUpdateForAdmission mRequestedPublishTime fetchedAt latestMaxAge body = do
  HermesUpdateResponse {..} <-
    case eitherDecode body of
      Left err -> Left $ E.internalError $ "Could not decode Hermes response: " <> T.pack err
      Right response -> Right response
  let encodedUpdateData = hbData hurBinary
      publishTimes = hpPublishTime . hppPrice <$> hurParsed
      actualFeedIds = sort $ normalizeFeedId . T.toLower . hppFeedId <$> hurParsed
      expectedFeedIdTexts = sort $ normalizeFeedId . T.toLower . bcFeedId <$> basketComponents
  if null encodedUpdateData
    then Left $ E.internalError "Hermes response did not include binary update data"
    else Right ()
  if actualFeedIds /= expectedFeedIdTexts
    then Left $ E.internalError "Hermes response did not include exactly the six requested basket feed IDs"
    else Right ()
  if length publishTimes /= length basketComponents
    then Left $ E.internalError "Hermes response did not include exactly six feed publish times"
    else Right ()
  (minPublishTime, maxPublishTime) <-
    mapAdmissionError $
      case mRequestedPublishTime of
        Nothing -> validateLatestPublishTimes fetchedAt latestMaxAge publishTimes
        Just requestedPublishTime -> do
          (returnedMin, returnedMax) <- validatePublishTimes publishTimes
          if returnedMin < requestedPublishTime
            then Left "Hermes historical payload predates the requested publish time"
            else Right (requestedPublishTime, returnedMax)
  (updateData, feedIds) <- decodeAdmissionByteStrings encodedUpdateData
  let payload =
        PythUpdateResponse
          { purUpdateData = prefixHex <$> encodedUpdateData
          , purFetchedAt = fetchedAt
          , purPublishTimes = publishTimes
          , purSource = "backend_hermes"
          }
  Right
    PythUpdateAdmission
      { puaPayload = payload
      , puaUpdateData = updateData
      , puaFeedIds = feedIds
      , puaMinPublishTime = minPublishTime
      , puaMaxPublishTime = maxPublishTime
      }
  where
    mapAdmissionError = mapLeft (E.internalError . ("Hermes payload failed admission checks: " <>))

decodeAdmissionByteStrings :: [Text] -> Either ApiError ([ByteString], [ByteString])
decodeAdmissionByteStrings encodedUpdateData = do
  updateData <-
    traverse
      (\(index, encoded) ->
        mapAdmissionError $
          mapLeft
            (\err -> "Pyth update data item " <> T.pack (show index) <> " is invalid: " <> err)
            (hexToByteStringEither encoded)
      )
      (zip [0 :: Int ..] encodedUpdateData)
  if null updateData
    then Left $ E.internalError "Pyth update data is empty"
    else Right ()
  feedIds <-
    traverse
      (\component -> do
        feedId <-
          mapAdmissionError $
            mapLeft
              (\err -> "configured feed " <> bcFeedId component <> " is invalid: " <> err)
              (hexToByteStringEither $ bcFeedId component)
        if BS.length feedId == 32
          then Right feedId
          else Left $ E.internalError $ "Configured Pyth feed ID is not 32 bytes: " <> bcFeedId component
      )
      basketComponents
  Right (updateData, feedIds)
  where
    mapAdmissionError = mapLeft (E.internalError . ("Pyth payload failed admission checks: " <>))

prefixHex :: Text -> Text
prefixHex value =
  if "0x" `T.isPrefixOf` T.toLower value then value else "0x" <> value

mapLeft :: (a -> b) -> Either a value -> Either b value
mapLeft f result =
  case result of
    Left err -> Left $ f err
    Right value -> Right value

stripTrailingSlash :: Text -> Text
stripTrailingSlash value =
  fromMaybe value (T.stripSuffix "/" value)
