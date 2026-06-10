module Plether.Handlers.Perps
  ( getBasketHistory
  , getPythUpdate
  ) where

import Data.Aeson (FromJSON (..), eitherDecode, withObject, (.:), (.:?), (.!=))
import Control.Concurrent.STM
  ( atomically
  , modifyTVar'
  , readTVar
  , writeTVar
  )
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
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
import Plether.Database.Schema (BasketSnapshotRow (..), getBasketSnapshots)
import Plether.Types
import qualified Plether.Types.Error as E
import Plether.Pyth.Basket (BasketComponent (..), basketComponents)

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

  rows <- withDb pool $ \conn ->
    getBasketSnapshots conn fromUnix nowUnix interval maxPoints

  let points = map rowToPoint rows
      latest = case reverse rows of
        row : _ -> Just (bsrBasketPrice row)
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

rowToPoint :: BasketSnapshotRow -> BasketHistoryPoint
rowToPoint BasketSnapshotRow {..} =
  BasketHistoryPoint
    { bhpTimestamp = bsrTimestamp
    , bhpBasketPrice = bsrBasketPrice
    , bhpComponents = bsrComponents
    }

computeChange :: [BasketSnapshotRow] -> Maybe Double
computeChange rows =
  case (rows, reverse rows) of
    (first : _, lastRow : _) | bsrBasketPrice first > 0 ->
      Just $
        (fromIntegral (bsrBasketPrice lastRow - bsrBasketPrice first) / fromIntegral (bsrBasketPrice first) :: Double)
    _ -> Nothing

data HermesBinary = HermesBinary
  { hbData :: [Text]
  }
  deriving stock (Show)

instance FromJSON HermesBinary where
  parseJSON = withObject "HermesBinary" $ \v ->
    HermesBinary <$> v .: "data"

data HermesPrice = HermesPrice
  { hpPublishTime :: Maybe Integer
  }
  deriving stock (Show)

instance FromJSON HermesPrice where
  parseJSON = withObject "HermesPrice" $ \v ->
    HermesPrice <$> v .:? "publish_time"

data HermesParsedPrice = HermesParsedPrice
  { hppPrice :: Maybe HermesPrice
  }
  deriving stock (Show)

instance FromJSON HermesParsedPrice where
  parseJSON = withObject "HermesParsedPrice" $ \v ->
    HermesParsedPrice <$> v .:? "price"

data HermesUpdateResponse = HermesUpdateResponse
  { hurBinary :: HermesBinary
  , hurParsed :: [HermesParsedPrice]
  }
  deriving stock (Show)

instance FromJSON HermesUpdateResponse where
  parseJSON = withObject "HermesUpdateResponse" $ \v ->
    HermesUpdateResponse
      <$> v .: "binary"
      <*> v .:? "parsed" .!= []

getPythUpdate
  :: AppCache
  -> Manager
  -> Config
  -> Maybe Integer
  -> IO (Either ApiError (ApiResponse PythUpdateResponse))
getPythUpdate cache manager cfg mPublishTime = do
  now <- getPOSIXTime
  let nowUnix = round now
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
                  { requestHeaders = authHeaders <> requestHeaders requestBase
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
                else case decodePythUpdate nowUnix body of
                  Left err -> pure $ Left err
                  Right payload -> do
                    setCachedPyth now payload
                    pure $ Right $ mkResponse 0 (cfgChainId cfg) payload
  where
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

    authHeaders =
      case cfgPythApiKey cfg of
        Nothing -> []
        Just key | T.null (T.strip key) -> []
        Just key -> [("Authorization", encodeUtf8 ("Bearer " <> T.strip key))]

    retryAfterHeader response =
      lookup "Retry-After" (responseHeaders response)

    decodePythUpdate now body =
      case eitherDecode body of
        Left err -> Left $ E.internalError $ "Could not decode Hermes response: " <> T.pack err
        Right HermesUpdateResponse {..} ->
          if null (hbData hurBinary)
            then Left $ E.internalError "Hermes response did not include binary update data"
            else
              Right $
                PythUpdateResponse
                  { purUpdateData = prefixHex <$> hbData hurBinary
                  , purFetchedAt = now
                  , purPublishTimes = publishTimes hurParsed
                  , purSource = "backend_hermes"
                  }

    publishTimes =
      map hpPublishTimeValue
        . filter hasPublishTime
      where
        hasPublishTime HermesParsedPrice { hppPrice = Just HermesPrice { hpPublishTime = Just _ } } = True
        hasPublishTime _ = False
        hpPublishTimeValue HermesParsedPrice { hppPrice = Just HermesPrice { hpPublishTime = Just ts } } = ts
        hpPublishTimeValue _ = 0

    prefixHex value =
      if "0x" `T.isPrefixOf` value then value else "0x" <> value

    previewBody body =
      T.take 180 . T.strip . T.pack $ show (LBS.take 180 body)

stripTrailingSlash :: Text -> Text
stripTrailingSlash value =
  fromMaybe value (T.stripSuffix "/" value)
