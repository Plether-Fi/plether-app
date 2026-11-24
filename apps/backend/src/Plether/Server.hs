module Plether.Server
  ( apiServerOptions
  , apiConnectionTimeoutSeconds
  , maxConcurrentForegroundRequests
  , maxConcurrentLongPollRequests
  , newConcurrencyLimitMiddleware
  ) where

import Control.Exception (bracket)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Text (Text)
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types.Status (status503)
import Network.Wai (Middleware, pathInfo, responseLBS)
import Network.Wai.Handler.Warp (setPort, setTimeout)
import Plether.Logging (field, logWarnEvery)
import Plether.RequestLogging
  ( RequestClass (..)
  , classifyNormalizedRoute
  , normalizeRouteSegments
  )
import Web.Scotty (Options (..), defaultOptions)

-- Must exceed the API ALB's 75-second idle timeout. This is a connection
-- timeout, NOT a request deadline; Warp pauses it while the application runs.
-- Keep the 60-second long-poll and faucet route deadlines independent.
apiConnectionTimeoutSeconds :: Int
apiConnectionTimeoutSeconds = 120

maxConcurrentForegroundRequests :: Int
maxConcurrentForegroundRequests = 256

maxConcurrentLongPollRequests :: Int
maxConcurrentLongPollRequests = 256

apiServerOptions :: Int -> Options
apiServerOptions port = defaultOptions
  { settings = setPort port $ setTimeout apiConnectionTimeoutSeconds $ settings defaultOptions }

-- Shed excess concurrent requests in O(1) memory before they enter Scotty or
-- request logging. Health checks are never shed so ALB target health stays up
-- under traffic floods, and long polls use a separate bucket so they cannot
-- starve foreground requests (or vice versa).
newConcurrencyLimitMiddleware :: Int -> Int -> IO Middleware
newConcurrencyLimitMiddleware foregroundLimit longPollLimit = do
  foregroundInFlight <- newIORef (0 :: Int)
  longPollInFlight <- newIORef (0 :: Int)
  pure $ \application req respond -> do
    let route = normalizeRouteSegments $ pathInfo req
        requestClass = classifyNormalizedRoute route
    case requestClass of
      HealthCheck -> application req respond
      Foreground ->
        withBoundedSlot foregroundInFlight (max 1 foregroundLimit) "foreground" route
          (application req respond)
          (respond overloadResponse)
      LongPoll ->
        withBoundedSlot longPollInFlight (max 1 longPollLimit) "long_poll" route
          (application req respond)
          (respond overloadResponse)
  where
    withBoundedSlot :: IORef Int -> Int -> String -> Text -> IO a -> IO a -> IO a
    withBoundedSlot counterRef limit className route runAllowed runRejected =
      bracket acquire release $ \acquired ->
        if acquired
          then runAllowed
          else do
            logWarnEvery
              10
              "api_concurrency_limit_exceeded"
              "API concurrency limit exceeded; shedding excess request"
              [ field "request_class" className
              , field "http_route" route
              , field "limit" limit
              ]
            runRejected
      where
        acquire = atomicModifyIORef' counterRef $ \current ->
          if current < limit
            then (current + 1, True)
            else (current, False)
        release acquired =
          if acquired
            then atomicModifyIORef' counterRef $ \current -> (max 0 (current - 1), ())
            else pure ()

    overloadResponse =
      responseLBS
        status503
        [ (hContentType, "application/json")
        , ("Cache-Control", "no-store")
        , ("Retry-After", "1")
        ]
        "{\"error\":{\"code\":\"RATE_LIMITED\",\"message\":\"Server is temporarily overloaded; retry shortly\"}}"
