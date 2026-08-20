module Plether.RequestLogging
  ( newRequestLoggingMiddleware
  , RequestClass (..)
  , classifyNormalizedRoute
  , afterResponseHandoff
  , normalizeRouteSegments
  , shouldEmitForegroundSample
  , shouldEmitSlowWarning
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

data RequestClass = Foreground | LongPoll | HealthCheck
  deriving stock (Eq, Show)

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
        requestClass = classifyNormalizedRoute route
        requestClassName = requestClassText requestClass
    afterResponseHandoff (respond response) $ do
      mSummary <-
        if shouldEmitForegroundSample requestClass
          then recordRequest statsVar durationMs status
          else pure Nothing
      mapM_ emitSummary mSummary
      when (shouldEmitForegroundSample requestClass) $
        logInfo
          "api_foreground_request_completed"
          "Foreground API request completed"
          [ field "http_method" method
          , field "http_route" route
          , field "http_status_code" status
          , field "request_class" requestClassName
          , field "duration_ms" durationMs
          ]
      when (status >= 500) $
        logErrorEvery
          10
          "api_request_failed"
          "API request returned a server error"
          [ field "http_method" method
          , field "http_route" route
          , field "http_status_code" status
          , field "request_class" requestClassName
          , field "duration_ms" (round durationMs :: LogFieldValue)
          ]
      when (shouldEmitSlowWarning requestClass status durationMs) $
        logWarnEvery
          60
          "api_request_slow"
          "API request exceeded the slow-request threshold"
          [ field "http_method" method
          , field "http_route" route
          , field "http_status_code" status
          , field "request_class" requestClassName
          , field "duration_ms" (round durationMs :: LogFieldValue)
          , field "slow_threshold_ms" (round slowRequestThresholdMs :: LogFieldValue)
          ]

-- Capture request duration before response handoff, then run completion effects
-- only after the server's response callback returns. This prevents synchronous
-- logging from inflating either the sampled duration or response latency.
afterResponseHandoff :: IO result -> IO () -> IO result
afterResponseHandoff handoff completion = do
  responseReceived <- handoff
  completion
  pure responseReceived

type LogFieldValue = Integer

recordRequest :: MVar RequestStats -> Double -> Int -> IO (Maybe RequestStats)
recordRequest statsVar durationMs status =
  modifyMVar statsVar $ \stats -> do
    -- Response callbacks may return out of order. Read the summary clock only
    -- after taking the MVar so an older callback cannot underflow Word64 when
    -- compared with a window already advanced by a newer callback.
    now <- getMonotonicTimeNSec
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
    "api_foreground_request_summary"
    "Foreground API request activity since the previous summary"
    [ field "request_count" $ rqsRequestCount stats
    , field "request_class" ("foreground" :: Text)
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
normalizedPath = normalizeRouteSegments . pathInfo

normalizeRouteSegments :: [Text] -> Text
normalizeRouteSegments segments =
  let normalized = normalizeCompetitionSlug $ map normalizeSegment segments
   in if normalized `elem` knownNormalizedRoutes
        then "/" <> Text.intercalate "/" normalized
        else "/:unmatched"
  where
    normalizeSegment segment
      | isAddress segment = ":address"
      | not (Text.null segment) && Text.all isDigit segment = ":id"
      | otherwise = Text.take 64 segment

    -- Competition slugs are operator-defined but still dynamic route values.
    -- Normalize the two public slug positions so request logs do not create a
    -- new route label for each competition.
    normalizeCompetitionSlug = \case
      ["api", "insights", "v1", "competitions", _, "leaderboard"] ->
        ["api", "insights", "v1", "competitions", ":slug", "leaderboard"]
      ["api", "insights", "v1", "competitions", _, "wallets", address] ->
        ["api", "insights", "v1", "competitions", ":slug", "wallets", address]
      normalized -> normalized

    isAddress segment =
      Text.length segment == 42
        && "0x" `Text.isPrefixOf` Text.toLower segment

-- This is intentionally an exact, low-cardinality mirror of the public Scotty
-- routes. Unknown paths collapse to /:unmatched instead of placing attacker-
-- controlled path fragments in a per-request latency log.
knownNormalizedRoutes :: [[Text]]
knownNormalizedRoutes =
  [ ["api", "health"]
  , ["api", "testnet", "faucet"]
  , ["api", "aa", "pimlico"]
  , ["api", "protocol", "status"]
  , ["api", "protocol", "config"]
  , ["api", "user", ":address", "dashboard"]
  , ["api", "user", ":address", "balances"]
  , ["api", "user", ":address", "positions"]
  , ["api", "user", ":address", "allowances"]
  , ["api", "user", ":address", "history"]
  , ["api", "user", ":address", "history", "leverage"]
  , ["api", "user", ":address", "history", "lending"]
  , ["api", "quotes", "mint"]
  , ["api", "quotes", "burn"]
  , ["api", "quotes", "zap"]
  , ["api", "quotes", "trade"]
  , ["api", "quotes", "leverage"]
  , ["api", "insights", "v1", "competitions", "current"]
  , ["api", "insights", "v1", "competitions", ":slug", "leaderboard"]
  , ["api", "insights", "v1", "competitions", ":slug", "wallets", ":address"]
  , ["api", "insights", "v1", "status"]
  , ["api", "perps", "accounts", ":address", "orders"]
  , ["api", "perps", "accounts", ":address", "activity"]
  , ["api", "perps", "indexer", "status"]
  , ["api", "perps", "orders", ":id", "wait"]
  , ["api", "perps", "orders", ":id", "reveal-payload"]
  , ["api", "perps", "market", "stats"]
  , ["api", "perps", "basket", "history"]
  , ["api", "perps", "basket", "candles"]
  , ["api", "perps", "basket", "candles", "current"]
  , ["api", "perps", "basket", "latest"]
  , ["api", "perps", "pyth", "update"]
  , ["api", "perps", "pyth", "cached-latest"]
  ]

classifyNormalizedRoute :: Text -> RequestClass
classifyNormalizedRoute route
  | route == "/api/perps/orders/:id/wait" = LongPoll
  | route == "/api/health" = HealthCheck
  | otherwise = Foreground

requestClassText :: RequestClass -> Text
requestClassText = \case
  Foreground -> "foreground"
  LongPoll -> "long_poll"
  HealthCheck -> "health_check"

shouldEmitForegroundSample :: RequestClass -> Bool
shouldEmitForegroundSample = (== Foreground)

shouldEmitSlowWarning :: RequestClass -> Int -> Double -> Bool
shouldEmitSlowWarning requestClass status durationMs =
  requestClass == Foreground
    && status < 500
    && durationMs >= slowRequestThresholdMs
