module Plether.Logging
  ( LogField
  , field
  , logDebug
  , logInfo
  , logWarn
  , logError
  , logInfoEvery
  , logWarnEvery
  , logErrorEvery
  ) where

import Data.Aeson (ToJSON, Value (..), encode, toJSON)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy.Char8 as LazyByteString
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import qualified Data.Vector as Vector
import System.IO (Handle, hFlush, stderr, stdout)
import System.IO.Unsafe (unsafePerformIO)

data LogLevel = Debug | Info | Warn | Error
  deriving stock (Eq, Ord, Show)

data LogField = LogField Key.Key Value

data RateState = RateState
  { rsLastEmittedAt :: POSIXTime
  , rsSuppressedCount :: Int
  }

field :: (ToJSON value) => Key.Key -> value -> LogField
field key = LogField key . sanitizeValue . toJSON

logDebug :: Text -> Text -> [LogField] -> IO ()
logDebug = emit Debug

logInfo :: Text -> Text -> [LogField] -> IO ()
logInfo = emit Info

logWarn :: Text -> Text -> [LogField] -> IO ()
logWarn = emit Warn

logError :: Text -> Text -> [LogField] -> IO ()
logError = emit Error

logInfoEvery :: Int -> Text -> Text -> [LogField] -> IO ()
logInfoEvery = emitEvery Info

logWarnEvery :: Int -> Text -> Text -> [LogField] -> IO ()
logWarnEvery = emitEvery Warn

logErrorEvery :: Int -> Text -> Text -> [LogField] -> IO ()
logErrorEvery = emitEvery Error

emitEvery :: LogLevel -> Int -> Text -> Text -> [LogField] -> IO ()
emitEvery level intervalSeconds eventName message fields = do
  now <- getPOSIXTime
  mSuppressed <- atomicModifyIORef' rateStates $ decide now
  case mSuppressed of
    Nothing -> pure ()
    Just suppressed ->
      emit level eventName message $
        if suppressed > 0
          then field "suppressed_count" suppressed : fields
          else fields
  where
    key = (level, sanitizeEvent eventName)
    interval = fromIntegral $ max 0 intervalSeconds
    decide now states =
      case Map.lookup key states of
        Nothing ->
          (Map.insert key (RateState now 0) states, Just 0)
        Just RateState {rsLastEmittedAt, rsSuppressedCount}
          | interval == 0 || now - rsLastEmittedAt >= interval ->
              (Map.insert key (RateState now 0) states, Just rsSuppressedCount)
          | otherwise ->
              (Map.insert key (RateState rsLastEmittedAt $ rsSuppressedCount + 1) states, Nothing)

emit :: LogLevel -> Text -> Text -> [LogField] -> IO ()
emit level eventName message fields = do
  let (severityText, severityNumber, target) = levelMetadata level
      baseFields =
        [ ("log_schema_version", toJSON (1 :: Int))
        , ("event", String $ sanitizeEvent eventName)
        , ("message", String $ sanitizeText 4096 message)
        , ("level", String severityText)
        , ("SeverityText", String severityText)
        , ("SeverityNumber", toJSON severityNumber)
        ]
      extraFields = [(key, value) | LogField key value <- fields]
      -- Reserved envelope fields win if a call site accidentally reuses one.
      payload = Object $ KeyMap.fromList baseFields <> KeyMap.fromList extraFields
  LazyByteString.hPutStrLn target $ encode payload
  hFlush target

levelMetadata :: LogLevel -> (Text, Int, Handle)
levelMetadata = \case
  Debug -> ("DEBUG", 5, stdout)
  Info -> ("INFO", 9, stdout)
  Warn -> ("WARN", 13, stderr)
  Error -> ("ERROR", 17, stderr)

sanitizeValue :: Value -> Value
sanitizeValue = \case
  String value -> String $ sanitizeText 2048 value
  Array values -> Array $ Vector.map sanitizeValue $ Vector.take 20 values
  Object values -> Object $ KeyMap.map sanitizeValue values
  value -> value

sanitizeEvent :: Text -> Text
sanitizeEvent = sanitizeText 128

sanitizeText :: Int -> Text -> Text
sanitizeText limit = Text.take limit . Text.unwords . map redactWord . Text.words
  where
    redactWord word
      | "://" `Text.isInfixOf` word = redactUrl word
      | otherwise = word

    redactUrl word =
      let (scheme, withSeparator) = Text.breakOn "://" word
          remainder = Text.drop 3 withSeparator
          authority = Text.takeWhile (`notElem` ['/', '?', '#']) remainder
          (credentialsPrefix, authorityWithoutCredentials) = Text.breakOnEnd "@" authority
          safeAuthority =
            if Text.null credentialsPrefix
              then authority
              else authorityWithoutCredentials
       in if Text.null scheme || Text.null safeAuthority
            then "<redacted-url>"
            else scheme <> "://" <> safeAuthority <> "/<redacted>"

{-# NOINLINE rateStates #-}
rateStates :: IORef (Map.Map (LogLevel, Text) RateState)
rateStates = unsafePerformIO $ newIORef Map.empty
