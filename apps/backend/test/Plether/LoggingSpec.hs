module Plether.LoggingSpec (spec) where

import Control.Concurrent.Async (mapConcurrently_)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar, takeMVar)
import Control.Exception (bracket, finally, toException, AsyncException (ThreadKilled))
import Plether.Insights.SnapshotObservability
import Database.PostgreSQL.Simple (SqlError (..), ExecStatus (FatalError))
import Control.Monad (forM_, replicateM_)
import Data.Aeson (Value (..), eitherDecodeStrict', toJSON)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import Data.List (sort)
import Data.IORef
import qualified Data.Text as Text
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import Plether.LiquidationWorker
  ( LiquidationWorkerConfig (..)
  , LiquidationWorkerMode (LiquidationWorkerOnce)
  , runLiquidationWorker
  )
import Plether.Logging (field, logDebug, logError, logInfo, logWarn, logWarnEvery, LogTiming (..), withLogTiming)
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
  describe "snapshot publication logging" $ do
    it "logs expected races as safe structured information, and defects as errors" $ do
      forM_ [minBound .. maxBound] $ \reason -> do
        let rejection = SnapshotRejection reason "live" 123 (Just 7) (Just 8) 10 (Just 11)
            target = if snapshotRejectionIsDefect reason then stderr else stdout
        output <- captureHandle target $ logSnapshotRejection 25 (Just 1) (Just 2) rejection
        record <- decodeOnly output
        lookupField "event" record `shouldBe` Just (String "insights_snapshot_rejected")
        lookupField "reason" record `shouldBe` Just (String $ snapshotRejectionCode reason)
        lookupField "level" record `shouldBe` Just (String $ if snapshotRejectionIsDefect reason then "ERROR" else "INFO")
        lookupField "current_participant_count" record `shouldBe` Just (Number 11)
        lookupField "lock_held_ms" record `shouldBe` Just (Number 2)
        case record of
          Object fields -> sort (map Key.toText $ KeyMap.keys fields) `shouldBe` sort
            ["event","message","level","SeverityText","SeverityNumber","log_schema_version",
             "reason","snapshot_kind","block_number","captured_epoch","current_epoch",
             "captured_participant_count","current_participant_count","elapsed_ms",
             "lock_wait_ms","lock_held_ms","retryable"]
          _ -> expectationFailure "Expected a structured rejection"

    it "does not duplicate typed rejections or swallow cancellation" $ do
      output <- captureHandle stderr $ logSnapshotCycleException $ toException $
        SnapshotRejection ParticipantSetChanged "live" 123 Nothing Nothing 10 Nothing
      output `shouldBe` ByteString.empty
      logSnapshotCycleException (toException ThreadKilled) `shouldThrow` (== ThreadKilled)

    it "retains SQLSTATE and excludes database and unexpected exception payloads" $ do
      let sql = SqlError "55P03" FatalError "private-wallet-and-token" "secret-detail" "secret-hint"
      output <- captureHandle stderr $ logSnapshotCycleException $ toException sql
      record <- decodeOnly output
      lookupField "sql_state" record `shouldBe` Just (String "55P03")
      Char8.isInfixOf "private" output `shouldBe` False
      Char8.isInfixOf "secret" output `shouldBe` False
      unexpected <- captureHandle stderr $ logSnapshotCycleException $ toException $ userError "private-wallet-and-token"
      unexpectedRecord <- decodeOnly unexpected
      lookupField "error_class" unexpectedRecord `shouldBe` Just (String "unexpected")
      Char8.isInfixOf "private" unexpected `shouldBe` False

  describe "structured logging" $ do
    it "measures request-local output and restores observers after nesting and exceptions" $ do
      outer <- newIORef ([] :: [LogTiming])
      inner <- newIORef ([] :: [LogTiming])
      let observe ref sample = atomicModifyIORef' ref $ \xs -> (sample:xs, ())
          emitOne = logInfo "timing_fixture" "fixed test message" []
      _ <- captureHandle stdout $ do
        withLogTiming (observe outer) $ do
          emitOne
          withLogTiming (observe inner) emitOne
          withLogTiming (observe inner) (ioError $ userError "fixture") `shouldThrow` anyIOException
          emitOne
        emitOne
      length <$> readIORef outer `shouldReturn` 2
      length <$> readIORef inner `shouldReturn` 1
      samples <- readIORef outer
      forM_ samples $ \sample -> do
        logLockWaitMs sample `shouldSatisfy` (>= 0)
        logWriteMs sample `shouldSatisfy` (>= 0)

    it "does not attribute concurrent threads' writes to another request" $ do
      first <- newIORef (0 :: Int)
      second <- newIORef (0 :: Int)
      let observe ref _ = atomicModifyIORef' ref $ \n -> (n+1, ())
          run ref n = withLogTiming (observe ref) $ replicateM_ n $ logInfo "timing_fixture" "fixed" []
      _ <- captureHandle stdout $ mapConcurrently_ id [run first 7, run second 11]
      readIORef first `shouldReturn` 7
      readIORef second `shouldReturn` 11

    forM_ [("stdout", stdout, logInfo, logDebug), ("stderr", stderr, logWarn, logError)] $
      \(name, target, firstLevel, secondLevel) ->
        it ("keeps concurrent small and multi-chunk records intact on " <> name) $ do
          ready <- newEmptyMVar
          start <- newEmptyMVar
          let workers = 16
              perWorker = 32
              emitWorker worker = do
                putMVar ready ()
                readMVar start
                forM_ [1 .. perWorker] $ \sequenceNumber -> do
                  let identifier = worker * perWorker + sequenceNumber
                      emitLog = if even identifier then firstLevel else secondLevel
                      -- Exceed a lazy bytestring chunk and the handle buffer.
                      payload = if sequenceNumber `mod` 8 == 0
                        then replicate 20 (Text.replicate 2048 "x") else ["small"]
                  emitLog "concurrent_record" "Concurrent log test"
                    [field "identifier" identifier, field "payload" payload]
          output <- captureHandle target $ mapConcurrently_ id
            [ mapConcurrently_ emitWorker [0 .. workers - 1 :: Int]
            , replicateM_ workers (takeMVar ready) >> putMVar start ()
            ]
          let linesFound = Char8.lines output
          length linesFound `shouldBe` workers * perWorker
          case traverse eitherDecodeStrict' linesFound of
            Left err -> expectationFailure err
            Right records -> do
              sort (map (lookupField "identifier") records) `shouldBe`
                [Just (Number $ fromIntegral n) | n <- [1 .. workers * perWorker]]
              forM_ records $ \record -> do
                lookupField "event" record `shouldBe` Just (String "concurrent_record")
                lookupField "message" record `shouldBe` Just (String "Concurrent log test")
                case lookupField "identifier" record of
                  Just (Number n) -> lookupField "payload" record `shouldBe` Just (toJSON $
                    if (floor n :: Int) `mod` 8 == 0
                      then replicate 20 (Text.replicate 2048 "x") else ["small"])
                  _ -> expectationFailure "Missing record identifier"

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
    , lwcAccountLens = "0x0000000000000000000000000000000000000004"
    , lwcPrivateKey = "super-secret-not-a-valid-private-key"
    , lwcPollSeconds = 1
    , lwcScanBatchSize = 1
    , lwcMulticallSize = 1
    , lwcExecutionBatchSize = 1
    , lwcIndexerStartBlock = 0
    , lwcIndexerConfirmations = 1
    , lwcIndexerBatchSize = 1
    , lwcIndexerOverlapBlocks = 0
    , lwcPendingReplacementSeconds = 60
    , lwcMaxTransactionGas = 25_000_000
    , lwcGasBufferBps = 0
    , lwcFeeBufferBps = 0
    , lwcFuturePublishMaxRetries = 2
    , lwcFuturePublishRetryMaxSeconds = 10
    , lwcPythLatestMaxAgeSeconds = 10
    }
