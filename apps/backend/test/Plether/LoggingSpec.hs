module Plether.LoggingSpec (spec) where

import Control.Exception (bracket, finally)
import Data.Aeson (Value (..), eitherDecodeStrict')
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import Plether.Logging (field, logInfo, logWarnEvery)
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO
  ( Handle
  , hClose
  , hFlush
  , openBinaryTempFile
  , stderr
  , stdout
  )
import Test.Hspec

spec :: Spec
spec = do
  describe "structured logging" $ do
    it "emits one JSON line with reserved fields and redacted URL paths" $ do
      output <- captureHandle stdout $
        logInfo
          "test_event"
          "Useful message"
          [ field "rpc_url" ("https://rpc.example/v2/secret-key" :: String)
          , field "message" ("must not override" :: String)
          ]
      record <- decodeOnly output
      lookupField "event" record `shouldBe` Just (String "test_event")
      lookupField "message" record `shouldBe` Just (String "Useful message")
      lookupField "SeverityText" record `shouldBe` Just (String "INFO")
      lookupField "rpc_url" record `shouldBe` Just (String "https://rpc.example/<redacted>")

    it "suppresses repeated events and reports how many lines were omitted" $ do
      output <- captureHandle stderr $ do
        logWarnEvery 3600 "rate_limit_test" "First warning" []
        logWarnEvery 3600 "rate_limit_test" "Suppressed warning" []
        logWarnEvery 0 "rate_limit_test" "Next warning" []
      let records = traverse eitherDecodeStrict' $ filter (not . ByteString.null) $ Char8.lines output
      case records of
        Left err -> expectationFailure err
        Right [_, finalRecord] ->
          lookupField "suppressed_count" finalRecord `shouldBe` Just (Number 1)
        Right values ->
          expectationFailure $ "expected two emitted records, got " <> show (length values)

captureHandle :: Handle -> IO () -> IO ByteString.ByteString
captureHandle target action = do
  temporaryDirectory <- getTemporaryDirectory
  bracket
    (openBinaryTempFile temporaryDirectory "plether-logging-test")
    (\(path, handle) -> hClose handle `finally` removeFile path)
    $ \(path, temporaryHandle) ->
      bracket (hDuplicate target) hClose $ \originalHandle -> do
        hFlush target
        hDuplicateTo temporaryHandle target
        action `finally` hDuplicateTo originalHandle target
        hFlush temporaryHandle
        hClose temporaryHandle
        ByteString.readFile path

decodeOnly :: ByteString.ByteString -> IO Value
decodeOnly output =
  case filter (not . ByteString.null) $ Char8.lines output of
    [line] ->
      case eitherDecodeStrict' line of
        Left err -> expectationFailure err >> pure Null
        Right value -> pure value
    linesFound ->
      expectationFailure ("expected one log line, got " <> show (length linesFound)) >> pure Null

lookupField :: Key.Key -> Value -> Maybe Value
lookupField key (Object values) = KeyMap.lookup key values
lookupField _ _ = Nothing
