module Plether.RequestLogging
  ( newRequestLoggingMiddleware
  ) where

import Control.Concurrent.MVar (MVar, modifyMVar, newMVar)
import Control.Monad (when)
import Data.Char (isDigit)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Text.Encoding.Error (lenientDecode)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Network.HTTP.Types.Status (statusCode)
import Network.Wai
  ( Middleware
  , Request
  , pathInfo
  , requestMethod
  , responseStatus
  )
import Plether.Logging (field, logErrorEvery, logInfo, logWarnEvery)

data RequestStats = RequestStats
  { rqsWindowStartedNs :: Word64
  , rqsRequestCount :: Int
  , rqsSuccessCount :: Int
  , rqsRedirectCount :: Int
  , rqsClientErrorCount :: Int
  , rqsServerErrorCount :: Int
  , rqsTotalDurationMs :: Double
  , rqsMaxDurationMs :: Double
  }

requestSummaryIntervalNs :: Word64
requestSummaryIntervalNs = 60 * 1_000_000_000

slowRequestThresholdMs :: Double
slowRequestThresholdMs = 2_000

newRequestLoggingMiddleware :: IO Middleware
newRequestLoggingMiddleware = do
  startedAt <- getMonotonicTimeNSec
  statsVar <- newMVar $ emptyStats startedAt
  pure $ logRequests statsVar

logRequests :: MVar RequestStats -> Middleware
logRequests statsVar application request respond = do
  startedAt <- getMonotonicTimeNSec
  application request $ \response -> do
    finishedAt <- getMonotonicTimeNSec
    let durationMs = fromIntegral (finishedAt - startedAt) / 1_000_000
        status = statusCode $ responseStatus response
        method = TextEncoding.decodeUtf8With lenientDecode $ requestMethod request
        route = normalizedPath request
    mSummary <- recordRequest statsVar finishedAt durationMs status
    mapM_ emitSummary mSummary
    when (status >= 500) $
      logErrorEvery
        10
        "api_request_failed"
        "API request returned a server error"
        [ field "http_method" method
        , field "http_route" route
        , field "http_status_code" status
        , field "duration_ms" (round durationMs :: LogFieldValue)
        ]
    when (status < 500 && durationMs >= slowRequestThresholdMs) $
      logWarnEvery
        60
        "api_request_slow"
        "API request exceeded the slow-request threshold"
        [ field "http_method" method
        , field "http_route" route
        , field "http_status_code" status
        , field "duration_ms" (round durationMs :: LogFieldValue)
        , field "slow_threshold_ms" (round slowRequestThresholdMs :: LogFieldValue)
        ]
    respond response

type LogFieldValue = Integer

recordRequest :: MVar RequestStats -> Word64 -> Double -> Int -> IO (Maybe RequestStats)
recordRequest statsVar now durationMs status =
  modifyMVar statsVar $ \stats -> do
    let updated = addRequest durationMs status stats
    if now - rqsWindowStartedNs stats >= requestSummaryIntervalNs
      then pure (emptyStats now, Just updated)
      else pure (updated, Nothing)

addRequest :: Double -> Int -> RequestStats -> RequestStats
addRequest durationMs status stats =
  stats
    { rqsRequestCount = rqsRequestCount stats + 1
    , rqsSuccessCount = rqsSuccessCount stats + inRange 200 299
    , rqsRedirectCount = rqsRedirectCount stats + inRange 300 399
    , rqsClientErrorCount = rqsClientErrorCount stats + inRange 400 499
    , rqsServerErrorCount = rqsServerErrorCount stats + inRange 500 599
    , rqsTotalDurationMs = rqsTotalDurationMs stats + durationMs
    , rqsMaxDurationMs = max (rqsMaxDurationMs stats) durationMs
    }
  where
    inRange lower upper = if status >= lower && status <= upper then 1 else 0

emitSummary :: RequestStats -> IO ()
emitSummary stats =
  logInfo
    "api_request_summary"
    "API request activity since the previous summary"
    [ field "request_count" $ rqsRequestCount stats
    , field "http_2xx_count" $ rqsSuccessCount stats
    , field "http_3xx_count" $ rqsRedirectCount stats
    , field "http_4xx_count" $ rqsClientErrorCount stats
    , field "http_5xx_count" $ rqsServerErrorCount stats
    , field "average_duration_ms" averageDurationMs
    , field "max_duration_ms" (round (rqsMaxDurationMs stats) :: LogFieldValue)
    ]
  where
    averageDurationMs =
      if rqsRequestCount stats == 0
        then 0
        else round (rqsTotalDurationMs stats / fromIntegral (rqsRequestCount stats)) :: LogFieldValue

emptyStats :: Word64 -> RequestStats
emptyStats startedAt =
  RequestStats
    { rqsWindowStartedNs = startedAt
    , rqsRequestCount = 0
    , rqsSuccessCount = 0
    , rqsRedirectCount = 0
    , rqsClientErrorCount = 0
    , rqsServerErrorCount = 0
    , rqsTotalDurationMs = 0
    , rqsMaxDurationMs = 0
    }

normalizedPath :: Request -> Text
normalizedPath request =
  "/" <> Text.intercalate "/" (map normalizeSegment $ pathInfo request)
  where
    normalizeSegment segment
      | isAddress segment = ":address"
      | not (Text.null segment) && Text.all isDigit segment = ":id"
      | otherwise = Text.take 64 segment

    isAddress segment =
      Text.length segment == 42
        && "0x" `Text.isPrefixOf` Text.toLower segment
