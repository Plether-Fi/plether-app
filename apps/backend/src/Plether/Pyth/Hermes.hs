module Plether.Pyth.Hermes
  ( HermesBasketUpdate (..)
  , fetchBasketUpdateAt
  , fetchLatestBasketUpdate
  , resolveHermesApiKey
  , isPermanentHermesConfigurationError
  ) where

import Data.Aeson (FromJSON (..), Value (..), eitherDecode, toJSON, withObject, (.:), (.:?), (.!=))
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Lazy as LBS
import Data.Scientific (floatingOrInteger)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock.POSIX (getPOSIXTime)
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
import Plether.Config (Config (..), defaultPythHermesUrl)
import Plether.Pyth.Basket
  ( BasketComponent (..)
  , PythPricePoint (..)
  , basketComponents
  , computeBasketSnapshot
  )
import Text.Read (readMaybe)

data HermesBasketUpdate = HermesBasketUpdate
  { hbuBasketPrice :: Integer
  , hbuComponents :: Value
  , hbuUpdateData :: [Text]
  , hbuPublishTimes :: [Integer]
  , hbuFetchedAt :: Integer
  , hbuSource :: Text
  }
  deriving stock (Show)

data HermesBinary = HermesBinary
  { hbData :: [Text]
  }
  deriving stock (Show)

instance FromJSON HermesBinary where
  parseJSON = withObject "HermesBinary" $ \v ->
    HermesBinary <$> v .: "data"

data HermesResponse = HermesResponse
  { hrBinary :: HermesBinary
  , hrParsed :: [PythPricePoint]
  }
  deriving stock (Show)

instance FromJSON HermesResponse where
  parseJSON = withObject "HermesResponse" $ \v -> do
    parsed <- v .:? "parsed" .!= []
    HermesResponse
      <$> v .: "binary"
      <*> pure (map unHermesParsedFeed parsed)

newtype HermesParsedFeed = HermesParsedFeed
  { unHermesParsedFeed :: PythPricePoint
  }
  deriving stock (Show)

instance FromJSON HermesParsedFeed where
  parseJSON = withObject "HermesParsedFeed" $ \v -> do
    feedId <- v .: "id"
    priceValue <- v .: "price"
    HermesParsedFeed <$> parsePythPrice feedId priceValue

parsePythPrice :: Text -> Value -> Parser PythPricePoint
parsePythPrice feedId = withObject "HermesPrice" $ \v -> do
  price <- v .: "price" >>= parseIntegerish
  conf <- v .: "conf" >>= parseIntegerish
  expo <- v .: "expo" >>= parseIntish
  publishTime <- v .: "publish_time" >>= parseIntegerish
  pure
    PythPricePoint
      { pppFeedId = feedId
      , pppPrice = price
      , pppConfidence = conf
      , pppExponent = expo
      , pppPublishTime = publishTime
      }

fetchLatestBasketUpdate :: Manager -> Config -> IO (Either Text HermesBasketUpdate)
fetchLatestBasketUpdate manager cfg =
  fetchBasketUpdate manager cfg "latest" "backend_hermes_latest"

fetchBasketUpdateAt :: Manager -> Config -> Integer -> IO (Either Text HermesBasketUpdate)
fetchBasketUpdateAt manager cfg publishTime =
  fetchBasketUpdate manager cfg (T.pack $ show publishTime) "backend_hermes_historical"

fetchBasketUpdate :: Manager -> Config -> Text -> Text -> IO (Either Text HermesBasketUpdate)
fetchBasketUpdate manager cfg pathSegment source =
  case resolveHermesApiKey (cfgPythHermesUrl cfg) (cfgPythApiKey cfg) of
    Left err -> pure $ Left err
    Right apiKey -> do
      requestBase <- parseRequest $ T.unpack requestUrl
      let request =
            setQueryString queryParams requestBase
              { requestHeaders = authHeaders apiKey <> requestHeaders requestBase
              }
      response <- httpLbs request manager
      nowUnix <- round <$> getPOSIXTime
      let code = statusCode (responseStatus response)
          body = responseBody response
      if code == 429
        then pure $ Left $ "Hermes returned HTTP 429; retry after " <> retryAfterText response
        else
          if code < 200 || code >= 300
            then pure $ Left $ "Hermes returned HTTP " <> T.pack (show code) <> ": " <> previewBody body
            else pure $ decodeBasket nowUnix body
  where
    requestUrl =
      stripTrailingSlash (cfgPythHermesUrl cfg)
        <> "/v2/updates/price/"
        <> pathSegment

    queryParams =
      ("parsed", Just "true")
        : [("ids[]", Just (encodeUtf8 (bcFeedId component))) | component <- basketComponents]

    authHeaders = \case
      Nothing -> []
      Just key -> [("Authorization", encodeUtf8 $ "Bearer " <> key)]

    retryAfterText response =
      maybe "60s" (T.pack . show) (lookup "Retry-After" (responseHeaders response))

    decodeBasket now body = do
      HermesResponse {..} <-
        case eitherDecode body of
          Left err -> Left $ "could not decode Hermes response: " <> T.pack err
          Right value -> Right value
      if null (hbData hrBinary)
        then Left "Hermes response did not include binary update data"
        else case computeBasketSnapshot hrParsed of
          Left err -> Left err
          Right (basketPrice, components) ->
            let publishTimes = pppPublishTime <$> hrParsed
             in if length publishTimes /= length basketComponents
                  then Left "Hermes response did not include all six parsed feed publish times"
                  else
                    Right
                      HermesBasketUpdate
                        { hbuBasketPrice = basketPrice
                        , hbuComponents = toJSON components
                        , hbuUpdateData = prefixHex <$> hbData hrBinary
                        , hbuPublishTimes = publishTimes
                        , hbuFetchedAt = now
                        , hbuSource = source
                        }

parseIntegerish :: Value -> Parser Integer
parseIntegerish = \case
  String txt ->
    case readMaybe (T.unpack txt) of
      Just value -> pure value
      Nothing -> fail $ "expected integer string, got " <> T.unpack txt
  Number n ->
    case floatingOrInteger n :: Either Double Integer of
      Right value -> pure value
      Left (_ :: Double) -> fail "expected integer number"
  other -> fail $ "expected integer, got " <> show other

parseIntish :: Value -> Parser Int
parseIntish value = fromInteger <$> parseIntegerish value

prefixHex :: Text -> Text
prefixHex value =
  if "0x" `T.isPrefixOf` value then value else "0x" <> value

previewBody :: LBS.ByteString -> Text
previewBody body =
  T.take 180 . T.strip . T.pack $ show (LBS.take 180 body)

stripTrailingSlash :: Text -> Text
stripTrailingSlash = T.dropWhileEnd (== '/')

resolveHermesApiKey :: Text -> Maybe Text -> Either Text (Maybe Text)
resolveHermesApiKey hermesUrl apiKey
  | normalizedUrl == knownLegacyHermesUrl =
      Left
        "PYTH_HERMES_URL points to the legacy Pyth Hermes endpoint, whose payloads are incompatible with the deployed upgraded Pyth contract; use https://pyth.dourolabs.app/hermes"
  | otherwise =
      case apiKey >>= nonBlank of
        Just key -> Right $ Just key
        Nothing
          | normalizedUrl == normalizeUrl defaultPythHermesUrl ->
              Left
                "PYTH_API_KEY is required for the upgraded Pyth Hermes endpoint; configure a server-side key with access to all six basket feeds"
          | otherwise -> Right Nothing
  where
    normalizedUrl = normalizeUrl hermesUrl
    knownLegacyHermesUrl = normalizeUrl "https://hermes.pyth.network"

    nonBlank value =
      let stripped = T.strip value
       in if T.null stripped then Nothing else Just stripped

    normalizeUrl = T.toLower . stripTrailingSlash . T.strip

isPermanentHermesConfigurationError :: Text -> Bool
isPermanentHermesConfigurationError err =
  any
    (`T.isPrefixOf` T.strip err)
    [ "PYTH_API_KEY is required"
    , "PYTH_HERMES_URL points to the legacy"
    , "Hermes returned HTTP 401"
    , "Hermes returned HTTP 403"
    ]
