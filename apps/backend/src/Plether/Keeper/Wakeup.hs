module Plether.Keeper.Wakeup
  ( WakeupMode (..)
  , parseWakeupMode
  , readWakeupMode
  , Wakeup
  , newWakeup
  , signalWakeup
  , waitWakeup
  , withWakeupListener
  , notifyCommitmentWakeup
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, tryPutMVar, tryTakeMVar)
import Control.Exception (SomeAsyncException, SomeException, bracket, fromException, throwIO, try)
import Control.Monad (forever, void, when)
import Data.Char (isHexDigit)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.PostgreSQL.Simple (Connection, Only (..), close, connectPostgreSQL, execute_, query)
import Database.PostgreSQL.Simple.Notification (Notification (..), getNotification)
import Plether.Config (Config (cfgPerpsChainId))
import Plether.Database.Diagnostics (diagnosticConnectionString)
import Plether.Logging (logInfoEvery, logWarnEvery)
import System.Environment (lookupEnv)
import System.Timeout (timeout)

data WakeupMode = WakeupsOff | WakeupsObserve | WakeupsActive
  deriving stock (Eq, Show)

-- Enabling another deployment requires a separate reviewed rollout. Unknown
-- configuration always preserves the ordinary polling path.
parseWakeupMode :: Integer -> Maybe Text -> WakeupMode
parseWakeupMode chainId raw
  | chainId /= 421614 = WakeupsOff
  | otherwise = case normalizeMode <$> raw of
      Just "observe" -> WakeupsObserve
      Just "active" -> WakeupsActive
      _ -> WakeupsOff

readWakeupMode :: Config -> IO WakeupMode
readWakeupMode cfg = do
  raw <- fmap T.pack <$> lookupEnv "PERPS_COMMITMENT_WAKEUPS_MODE"
  let normalized = normalizeMode <$> raw
      valid = normalized `elem` [Nothing, Just "off", Just "observe", Just "active"]
      enabledRequested = normalized `elem` [Just "observe", Just "active"]
  if not valid
    then logWarnEvery 60 "commitment_wakeup_invalid_mode"
      "Unknown commitment wakeup mode; ordinary polling remains enabled" []
    else when (cfgPerpsChainId cfg /= 421614 && enabledRequested) $
      logWarnEvery 60 "commitment_wakeup_wrong_chain"
        "Commitment wakeups are enabled only on the reviewed Sepolia deployment" []
  pure $ parseWakeupMode (cfgPerpsChainId cfg) raw

normalizeMode :: Text -> Text
normalizeMode = T.toLower . T.strip

-- A single pending signal bounds memory and coalesces duplicate notifications.
-- This interrupts idle waiting only; callers must not cancel active execution.
newtype Wakeup = Wakeup (MVar ())

newWakeup :: IO Wakeup
newWakeup = Wakeup <$> newEmptyMVar

signalWakeup :: Wakeup -> IO ()
signalWakeup (Wakeup signal) = void $ tryPutMVar signal ()

waitWakeup :: Wakeup -> Int -> IO Bool
waitWakeup (Wakeup signal) delayMicros
  | delayMicros <= 0 = isJust <$> tryTakeMVar signal
  | otherwise = isJust <$> timeout delayMicros (takeMVar signal)

normalizeRouter :: Text -> Text
normalizeRouter = T.toLower . T.strip

validRouter :: Text -> Bool
validRouter router = T.length router == 42 && "0x" `T.isPrefixOf` router
  && T.all isHexDigit (T.drop 2 router)

-- NOTIFY is delivered only after the caller's transaction commits. It is a
-- disposable hint, never the durable work record or authorization to execute.
-- Database failures propagate so callers choose their own best-effort boundary.
notifyCommitmentWakeup :: Connection -> Text -> IO ()
notifyCommitmentWakeup conn router
  | not (validRouter normalized) =
      logWarnEvery 60 "commitment_wakeup_invalid_router"
        "Commitment wakeup skipped for an invalid deployment address" []
  | otherwise = void (query conn
      "SELECT 1::integer FROM pg_notify('plether_perps_commitment_wakeup', ?)"
      (Only normalized) :: IO [Only Int])
  where normalized = normalizeRouter router

-- The listener never borrows the keeper's advisory-lock connection or a request
-- pool slot. Listener failures cannot fail the signing loop: synchronous errors
-- reconnect in this child while the caller continues its normal polling path.
withWakeupListener :: Text -> Text -> Wakeup -> IO a -> IO a
withWakeupListener databaseUrl router wakeup action
  | not (validRouter normalized) = do
      logWarnEvery 60 "commitment_wakeup_invalid_router"
        "Commitment listener skipped for an invalid deployment address" []
      action
  | otherwise = withAsync (supervise 1) $ \_ -> action
  where
    normalized = normalizeRouter router
    expected = TE.encodeUtf8 normalized
    session = bracket
      (connectPostgreSQL $ TE.encodeUtf8 $ diagnosticConnectionString databaseUrl)
      close
      $ \conn -> do
        void $ execute_ conn "SET statement_timeout='1500ms'"
        void $ execute_ conn "SET lock_timeout='500ms'"
        -- Dedicated sessions use autocommit: LISTEN has committed when this
        -- statement returns. The initial signal triggers a durable rescan after
        -- every connect, closing the startup/reconnect notification-loss window.
        void $ execute_ conn "LISTEN plether_perps_commitment_wakeup"
        signalWakeup wakeup
        logInfoEvery 60 "commitment_wakeup_listener_connected"
          "Commitment wakeup listener connected; durable work will be rescanned" []
        forever $ do
          notification <- getNotification conn
          when (notificationChannel notification == "plether_perps_commitment_wakeup"
              && notificationData notification == expected) $ do
            signalWakeup wakeup
            logInfoEvery 60 "commitment_wakeup_received"
              "Commitment work notification received" []
    supervise backoff = do
      result <- try session
      case result of
        Left err -> case fromException err :: Maybe SomeAsyncException of
          Just _ -> throwIO (err :: SomeException)
          Nothing -> logWarnEvery 60 "commitment_wakeup_listener_retry"
            "Commitment wakeup listener unavailable; ordinary polling continues" []
        Right () -> pure ()
      threadDelay $ backoff * 1_000_000
      supervise $ min 30 (backoff * 2)
