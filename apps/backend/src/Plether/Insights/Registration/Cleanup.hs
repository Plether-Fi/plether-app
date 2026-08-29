module Plether.Insights.Registration.Cleanup
  ( startRegistrationCleanup
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception
  ( SomeAsyncException
  , SomeException
  , fromException
  , throwIO
  , try
  )
import Control.Monad (forever)
import Plether.Database (DbPool, withDb)
import Plether.Database.Insights.Registration
  ( RegistrationCleanupResult (..)
  , cleanupExpiredRegistrationSecrets
  )
import Plether.Logging (field, logInfo, logWarn)

cleanupIntervalMicroseconds :: Int
cleanupIntervalMicroseconds = 5 * 60 * 1_000_000

-- | Purge expired cookies, one-time challenges, provider credentials, and
-- abandoned in-progress applications throughout a long-running API process.
-- Logs contain aggregate status/counts only; no registration identifiers or
-- encrypted values are ever rendered.
startRegistrationCleanup :: DbPool -> IO ()
startRegistrationCleanup pool = forever $ do
  threadDelay cleanupIntervalMicroseconds
  outcome <- try @SomeException $ withDb pool cleanupExpiredRegistrationSecrets
  case outcome of
    Right cleanupResult ->
      logInfo
        "insights_registration_cleanup_completed"
        "Expired registration secrets were cleaned"
        [ field "cleaned_records" $ rcrCleanedRecords cleanupResult
        , field "more_records_may_remain" $ rcrMayHaveMore cleanupResult
        ]
    Left exception ->
      case fromException exception :: Maybe SomeAsyncException of
        Just _ -> throwIO exception
        Nothing ->
          logWarn
            "insights_registration_cleanup_failed"
            "Expired registration secret cleanup failed"
            []
