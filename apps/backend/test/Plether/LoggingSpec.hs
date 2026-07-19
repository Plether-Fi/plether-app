module Plether.LoggingSpec (spec) where

import Control.Exception (bracket, finally)
import Data.Aeson (Value (..), eitherDecodeStrict')
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import Plether.LiquidationWorker
  ( LiquidationWorkerConfig (..)
  , LiquidationWorkerMode (LiquidationWorkerOnce)
  , runLiquidationWorker
  )
import Plether.Logging (field, logError, logInfo, logWarnEvery)
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

    it "emits errors with OTLP severity and removes URL credentials" $ do
      output <- captureHandle stderr $
        logError
          "test_error"
          "Operation failed"
          [field "database_url" ("postgresql://alice:secret@db.example:5432/plether" :: String)]
      record <- decodeOnly output
      lookupField "level" record `shouldBe` Just (String "ERROR")
      lookupField "SeverityText" record `shouldBe` Just (String "ERROR")
      lookupField "SeverityNumber" record `shouldBe` Just (Number 17)
      lookupField "database_url" record `shouldBe` Just (String "postgresql://db.example:5432/<redacted>")

      malformedOutput <- captureHandle stderr $
        logError
          "test_malformed_url"
          "Operation failed"
          [field "database_url" ("postgresql://alice:secret@" :: String)]
      malformedRecord <- decodeOnly malformedOutput
      lookupField "database_url" malformedRecord `shouldBe` Just (String "<redacted-url>")

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

    it "emits a structured liquidation-worker startup failure without key material" $ do
      output <- captureHandle stderr $
        runLiquidationWorker
          invalidSignerConfig
          (error "invalid signer must not evaluate the database pool")
          (error "invalid signer must not evaluate the Ethereum client")
          LiquidationWorkerOnce
          False
      record <- decodeOnly output
      lookupField "event" record `shouldBe` Just (String "liquidation_worker_signer_invalid")
      lookupField "level" record `shouldBe` Just (String "ERROR")
      lookupField "SeverityNumber" record `shouldBe` Just (Number 17)
      lookupField "chain_id" record `shouldBe` Just (Number 1)
      output `shouldNotSatisfy` Char8.isInfixOf "super-secret-not-a-valid-private-key"

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

invalidSignerConfig :: LiquidationWorkerConfig
invalidSignerConfig =
  LiquidationWorkerConfig
    { lwcChainId = 1
    , lwcOrderRouter = "0x0000000000000000000000000000000000000001"
    , lwcPletherOracle = "0x0000000000000000000000000000000000000002"
    , lwcCfdEngine = "0x0000000000000000000000000000000000000003"
    , lwcPrivateKey = "super-secret-not-a-valid-private-key"
    , lwcPollSeconds = 1
    , lwcScanBatchSize = 1
    , lwcIndexerStartBlock = 0
    , lwcIndexerConfirmations = 1
    , lwcIndexerBatchSize = 1
    , lwcIndexerOverlapBlocks = 0
    , lwcPendingReplacementSeconds = 60
    , lwcGasBufferBps = 0
    , lwcFeeBufferBps = 0
    }
