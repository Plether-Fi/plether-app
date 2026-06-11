module Plether.Pyth.History
  ( BasketIngestorConfig (..)
  , fetchBasketSnapshotAt
  , runBasketBackfill
  , startBasketHistoryIngestor
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, catch, displayException, try)
import Control.Monad (forM_, forever, when)
import Data.Aeson (FromJSON (..), Value (..), eitherDecode, toJSON, withObject, (.:))
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Set as Set
import Data.Scientific (floatingOrInteger)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.HTTP.Client
  ( Manager
  , httpLbs
  , parseRequest
  , responseBody
  , responseStatus
  , setQueryString
  )
import Network.HTTP.Types.Status (statusCode)
import Plether.Database (DbPool, withDb)
import Plether.Database.Schema
  ( getBasketSnapshotTimes
  , insertBasketSnapshot
  )
import Plether.Pyth.Basket
  ( BasketComponent (..)
  , PythPricePoint (..)
  , basketComponents
  , computeBasketSnapshot
  )
import Text.Read (readMaybe)

data BasketIngestorConfig = BasketIngestorConfig
  { bicBenchmarksUrl :: Text
  , bicBackfillDays :: Int
  , bicSampleIntervalSeconds :: Integer
  , bicPollSeconds :: Int
  }
  deriving stock (Show)

data BenchmarkResponse = BenchmarkResponse
  { brParsed :: [PythPricePoint]
  }
  deriving stock (Show)

instance FromJSON BenchmarkResponse where
  parseJSON = withObject "BenchmarkResponse" $ \v -> do
    parsed <- v .: "parsed"
    pure $ BenchmarkResponse (map unBenchmarkFeed parsed)

newtype BenchmarkFeed = BenchmarkFeed
  { unBenchmarkFeed :: PythPricePoint
  }
  deriving stock (Show)

instance FromJSON BenchmarkFeed where
  parseJSON = withObject "PythPricePoint" $ \v -> do
    feedId <- v .: "id"
    priceValue <- v .: "price"
    BenchmarkFeed <$> parsePythPrice feedId priceValue

parsePythPrice :: Text -> Value -> Parser PythPricePoint
parsePythPrice feedId = withObject "PythPrice" $ \v -> do
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

fetchBasketSnapshotAt :: Manager -> Text -> Integer -> Integer -> IO (Either Text (Integer, Value))
fetchBasketSnapshotAt manager benchmarksUrl intervalSeconds timestamp = do
  requestBase <- parseRequest $ T.unpack requestUrl
  let request = setQueryString queryParams requestBase
  response <- httpLbs request manager
  let code = statusCode (responseStatus response)
  if code < 200 || code >= 300
    then pure $ Left $ "Pyth Benchmarks returned HTTP " <> T.pack (show code)
    else pure $ decodeSnapshot (responseBody response)
  where
    requestUrl =
      stripTrailingSlash benchmarksUrl
        <> "/v1/updates/price/"
        <> T.pack (show timestamp)
        <> "/"
        <> T.pack (show benchmarkWindow)

    benchmarkWindow = min 60 (max 1 intervalSeconds)

    queryParams =
      ("parsed", Just "true")
        : [("ids", Just (encodeUtf8 (bcFeedId component))) | component <- basketComponents]

    decodeSnapshot :: LBS.ByteString -> Either Text (Integer, Value)
    decodeSnapshot body = do
      benchmarks <-
        case eitherDecode body of
          Right parsed -> Right [parsed]
          Left objectErr ->
            case eitherDecode body of
              Left arrayErr ->
                Left $
                  "could not decode Pyth Benchmarks response: "
                    <> T.pack objectErr
                    <> "; interval response decode also failed: "
                    <> T.pack arrayErr
              Right (parsed :: [BenchmarkResponse]) -> Right $ reverse parsed
      case firstCompleteSnapshot benchmarks of
        Nothing -> Left "Pyth Benchmarks returned no complete six-feed basket snapshot"
        Just result -> Right result

    firstCompleteSnapshot :: [BenchmarkResponse] -> Maybe (Integer, Value)
    firstCompleteSnapshot [] = Nothing
    firstCompleteSnapshot (benchmark : rest) =
      case computeBasketSnapshot (brParsed benchmark) of
        Left _ -> firstCompleteSnapshot rest
        Right (basketPrice, components) -> Just (basketPrice, toJSON components)

startBasketHistoryIngestor :: Manager -> DbPool -> BasketIngestorConfig -> IO ()
startBasketHistoryIngestor manager pool cfg = forever $ do
  runBasketBackfill manager pool cfg `catch` logException
  threadDelay (bicPollSeconds cfg * 1_000_000)

runBasketBackfill :: Manager -> DbPool -> BasketIngestorConfig -> IO ()
runBasketBackfill manager pool cfg = do
  now <- round <$> getPOSIXTime
  let interval = max 60 (bicSampleIntervalSeconds cfg)
      endTs = (now `div` interval) * interval
      earliestTs = endTs - fromIntegral (max 1 (bicBackfillDays cfg)) * 86_400

  existingTimes <- withDb pool $ \conn -> getBasketSnapshotTimes conn earliestTs endTs interval
  let existing = Set.fromList existingTimes
      missing = filter (`Set.notMember` existing) [earliestTs, earliestTs + interval .. endTs]

  when (not (null missing)) $ do
    putStrLn $
      "Backfilling "
        <> show (length missing)
        <> " missing Pyth basket snapshots from "
        <> show earliestTs
        <> " to "
        <> show endTs
    forM_ missing $ \ts -> do
      result <- try @SomeException $ fetchBasketSnapshotAt manager (bicBenchmarksUrl cfg) interval ts
      case result of
        Left err ->
          putStrLn $ "Pyth basket fetch failed at " <> show ts <> ": " <> displayException err
        Right (Left err) ->
          do
            putStrLn $ "Pyth basket fetch failed at " <> show ts <> ": " <> T.unpack err
            when ("429" `T.isInfixOf` err) $ threadDelay 60_000_000
        Right (Right (basketPrice, components)) ->
          withDb pool $ \conn ->
            insertBasketSnapshot conn ts interval basketPrice components
      -- Public Pyth endpoints are IP-rate-limited. Keep historical backfills
      -- below one request per second so chart ingestion cannot starve order reveal.
      threadDelay 1_250_000

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

stripTrailingSlash :: Text -> Text
stripTrailingSlash = T.dropWhileEnd (== '/')

logException :: SomeException -> IO ()
logException err =
  putStrLn $ "Pyth basket ingestor failed: " <> displayException err
