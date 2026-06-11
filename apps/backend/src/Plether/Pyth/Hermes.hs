module Plether.Pyth.Hermes
  ( HermesBasketUpdate (..)
  , fetchLatestBasketUpdate
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
import Plether.Config (Config (..))
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
fetchLatestBasketUpdate manager cfg = do
  nowUnix <- round <$> getPOSIXTime
  requestBase <- parseRequest $ T.unpack requestUrl
  let request =
        setQueryString queryParams requestBase
          { requestHeaders = authHeaders <> requestHeaders requestBase
          }
  response <- httpLbs request manager
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
        <> "/v2/updates/price/latest"

    queryParams =
      ("parsed", Just "true")
        : [("ids[]", Just (encodeUtf8 (bcFeedId component))) | component <- basketComponents]

    authHeaders =
      case cfgPythApiKey cfg of
        Nothing -> []
        Just key | T.null (T.strip key) -> []
        Just key -> [("Authorization", encodeUtf8 ("Bearer " <> T.strip key))]

    retryAfterText response =
      maybe "60s" (T.pack . show) (lookup "Retry-After" (responseHeaders response))

    decodeBasket now body = do
      HermesResponse {..} <-
        case eitherDecode body of
          Left err -> Left $ "could not decode Hermes latest response: " <> T.pack err
          Right value -> Right value
      if null (hbData hrBinary)
        then Left "Hermes latest response did not include binary update data"
        else case computeBasketSnapshot hrParsed of
          Left err -> Left err
          Right (basketPrice, components) ->
            let publishTimes = pppPublishTime <$> hrParsed
             in if length publishTimes /= length basketComponents
                  then Left "Hermes latest response did not include all six parsed feed publish times"
                  else
                    Right
                      HermesBasketUpdate
                        { hbuBasketPrice = basketPrice
                        , hbuComponents = toJSON components
                        , hbuUpdateData = prefixHex <$> hbData hrBinary
                        , hbuPublishTimes = publishTimes
                        , hbuFetchedAt = now
                        , hbuSource = "backend_hermes"
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
