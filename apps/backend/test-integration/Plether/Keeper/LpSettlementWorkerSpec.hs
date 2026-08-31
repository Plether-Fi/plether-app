module Plether.Keeper.LpSettlementWorkerSpec
  ( lpSettlementWorkerSpec
  ) where

import Control.Concurrent
  ( MVar
  , forkFinally
  , killThread
  , newEmptyMVar
  , putMVar
  , takeMVar
  , threadDelay
  , tryPutMVar
  )
import Control.Exception (bracket, finally)
import Control.Monad (foldM, unless, void, when)
import Data.Aeson (Value (..), decode, encode, object, toJSON, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (toList)
import Data.IORef
  ( IORef
  , atomicModifyIORef'
  , modifyIORef'
  , newIORef
  , readIORef
  , writeIORef
  )
import Data.Pool (destroyAllResources)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word8)
import Database.PostgreSQL.Simple
  ( Connection
  , Only (..)
  , execute
  , execute_
  , query
  , query_
  , withTransaction
  )
import Database.PostgreSQL.Simple.Types (Binary (..))
import Network.HTTP.Types (status200)
import Network.Wai (Application, responseLBS, strictRequestBody)
import Network.Wai.Handler.Warp (testWithApplication)
import Numeric (showHex)
import Plether.Config
  ( Config (..)
  , LpSettlementMode (..)
  , PerpsCandleReadMode (..)
  , PerpsCandleWriteMode (..)
  )
import Plether.Database (DbPool, newDbPool, withDb)
import Plether.Database.Schema
  ( LpSettlementBroadcastInput (..)
  , LpSettlementObservationInput (..)
  , LpSettlementSignedIntent (..)
  , LpSettlementTransactionRow (..)
  , appendLpSettlementBroadcast
  , ensurePerpsKeeperSchema
  , getActiveLpSettlementTransaction
  , getLpSettlementTransactionFamily
  , getLpSettlementTransactionById
  , getPerpsKeeperLastIndexedBlock
  , markLpSettlementTransactionConfirming
  , prepareLpSettlementTransaction
  , recordLpSettlementObservationV2
  , setPerpsKeeperLastIndexedBlock
  , tryPerpsKeeperLock
  , unlockPerpsKeeperLock
  , verifyLpSettlementSchema
  )
import Plether.Ethereum.Abi (encodeAddress, encodeCall, encodeUint256, keccak256)
import Plether.Ethereum.Client (EthClient, newClient)
import Plether.Ethereum.Contracts.Perps
  ( lpEpochSettledTopic
  , settleLpEpochPoolCall
  )
import Plether.Ethereum.Contracts.SettlementMonitor
  ( SettlementCodeHashes (..)
  , supportedConfigSchemaVersion
  , supportedObservationSchemaVersion
  )
import Plether.Ethereum.Transaction
  ( SignedTransaction (..)
  , Tx1559 (..)
  , rawTransactionHash
  , signTransaction
  )
import Plether.Insights.Competition
  ( CompetitionReleaseManifest (..)
  , july2026Competition
  )
import Plether.Keeper
  ( KeeperMode (..)
  , processLpSettlementCycleWithCodeHashes
  , runKeeperWithCodeHashes
  )
import System.Timeout (timeout)
import Test.Hspec

lpSettlementWorkerSpec :: Text -> Spec
lpSettlementWorkerSpec databaseUrl =
  describe "LP settlement worker scripted-RPC integration" $ do
    it "observe mode pins and simulates the exact cached call without signing or sending" $
      withWorkerDatabase databaseUrl $ \pool ->
        withRpcFixture False $ \fixture client ->
          withDb pool $ \conn -> do
            processLpSettlementCycle (workerConfig LpSettlementObserve) conn client

            transactionCount <- tableCount conn "perps_lp_settlement_transactions"
            observationCount <- tableCount conn "perps_lp_settlement_observations"
            sends <- readIORef $ rfSentRawTransactions fixture
            estimates <- readIORef $ rfEstimateCalls fixture
            transactionCount `shouldBe` 0
            observationCount `shouldBe` 1
            sends `shouldBe` []
            estimates
              `shouldBe` [ EstimateCall
                             { ecFrom = fixtureSigner
                             , ecTo = housePool
                             , ecValue = "0x0"
                             , ecData = bytesHex $ settleLpEpochPoolCall cachedMark cachedMarkTime
                             }
                         ]
            assertNoUnexpectedRequests fixture

    it "reconciles a lost broadcast response after restart by rebroadcasting identical raw bytes" $
      withWorkerDatabase databaseUrl $ \pool ->
        withRpcFixture False $ \fixture client ->
          withDb pool $ \conn -> do
            signed <- fixtureSignedTransaction 7
            attemptId <- seedStaleAmbiguousTransaction conn signed 45

            processLpSettlementCycle (workerConfig LpSettlementExecute) conn client

            sends <- readIORef $ rfSentRawTransactions fixture
            broadcasts <- query conn
              "SELECT outcome FROM perps_lp_settlement_broadcasts WHERE attempt_id = ? ORDER BY broadcast_sequence"
              (Only attemptId) :: IO [Only Text]
            persisted <- getLpSettlementTransactionById conn attemptId
            sends `shouldBe` [bytesHex $ signedRawTransaction signed]
            broadcasts `shouldBe` [Only "ambiguous", Only "accepted"]
            fmap lstrReplacementCount persisted `shouldBe` Just 0
            fmap lstrSignedTransactionHash persisted `shouldBe` Just (signedTransactionHash signed)
            assertNoUnexpectedRequests fixture

    it "fails closed when the configured cost cap is reduced below a persisted rebroadcast" $
      withWorkerDatabase databaseUrl $ \pool ->
        withRpcFixture False $ \fixture client ->
          withDb pool $ \conn -> do
            signed <- fixtureSignedTransaction 7
            attemptId <- seedStaleAmbiguousTransaction conn signed 45
            let reducedCap = 125_000 * 2_000_000_000 - 1

            processLpSettlementCycle
              ((workerConfig LpSettlementExecute) {cfgLpSettlementMaxTxCostWei = reducedCap})
              conn
              client

            reviewed <- getLpSettlementTransactionById conn attemptId
            broadcasts <- query conn
              "SELECT outcome FROM perps_lp_settlement_broadcasts WHERE attempt_id = ? ORDER BY broadcast_sequence"
              (Only attemptId) :: IO [Only Text]
            fmap lstrStatus reviewed `shouldBe` Just "manual_review"
            broadcasts `shouldBe` [Only "ambiguous"]
            readIORef (rfSentRawTransactions fixture) `shouldReturn` []
            assertNoUnexpectedRequests fixture

    it "does not rebroadcast a stale intent when startup bytecode attestation drifts" $
      withWorkerDatabase databaseUrl $ \pool ->
        withRpcFixture False $ \fixture client ->
          withDb pool $ \conn -> do
            signed <- fixtureSignedTransaction 7
            attemptId <- seedStaleAmbiguousTransaction conn signed 45
            writeIORef (rfCodeDrift fixture) True

            processLpSettlementCycle (workerConfig LpSettlementExecute) conn client

            pendingTx <- getLpSettlementTransactionById conn attemptId
            broadcasts <- query conn
              "SELECT outcome FROM perps_lp_settlement_broadcasts WHERE attempt_id = ? ORDER BY broadcast_sequence"
              (Only attemptId) :: IO [Only Text]
            fmap lstrStatus pendingTx `shouldBe` Just "pending"
            broadcasts `shouldBe` [Only "ambiguous"]
            readIORef (rfSentRawTransactions fixture) `shouldReturn` []
            assertNoUnexpectedRequests fixture

    it "replaces a sixty-second-old durable intent at the same nonce and semantics" $
      withWorkerDatabase databaseUrl $ \pool ->
        withRpcFixture False $ \fixture client ->
          withDb pool $ \conn -> do
            originalSigned <- fixtureSignedTransaction 7
            originalId <- seedStaleAmbiguousTransaction conn originalSigned 61

            processLpSettlementCycle (workerConfig LpSettlementExecute) conn client

            active <- getActiveLpSettlementTransaction conn fixtureChainId monitor fixtureSigner
            replacement <- case active of
              Just row -> pure row
              Nothing -> expectationFailure "expected active replacement" >> fail "missing replacement"
            family <- getLpSettlementTransactionFamily conn $ lstrId replacement
            sends <- readIORef $ rfSentRawTransactions fixture
            lstrReplacementCount replacement `shouldBe` 1
            lstrReplacesAttemptId replacement `shouldBe` Just originalId
            lstrNonce replacement `shouldBe` 7
            lstrTargetAddress replacement `shouldBe` housePool
            lstrValue replacement `shouldBe` 0
            lstrCalldata replacement
              `shouldBe` settleLpEpochPoolCall cachedMark cachedMarkTime
            lstrMaxPriorityFeePerGas replacement `shouldSatisfy` (> 1_000_000_000)
            lstrMaxFeePerGas replacement `shouldSatisfy` (> 2_000_000_000)
            map lstrReplacementCount family `shouldBe` [1, 0]
            sends `shouldSatisfy` \case
              [replacementRaw] -> replacementRaw /= bytesHex (signedRawTransaction originalSigned)
              _ -> False
            assertNoUnexpectedRequests fixture

    it "fails closed at the configured replacement cap without signing or broadcasting again" $
      withWorkerDatabase databaseUrl $ \pool ->
        withRpcFixture False $ \fixture client ->
          withDb pool $ \conn -> do
            attemptId <- seedReplacementCapLineage conn 61

            processLpSettlementCycle
              (workerConfig LpSettlementExecute)
              conn
              client

            reviewed <- getLpSettlementTransactionById conn attemptId
            family <- getLpSettlementTransactionFamily conn attemptId
            broadcasts <- query conn
              "SELECT outcome FROM perps_lp_settlement_broadcasts WHERE attempt_id = ? ORDER BY broadcast_sequence"
              (Only attemptId) :: IO [Only Text]
            fmap lstrStatus reviewed `shouldBe` Just "manual_review"
            fmap lstrReplacementCount reviewed `shouldBe` Just 3
            map lstrReplacementCount family `shouldBe` [3, 2, 1, 0]
            broadcasts `shouldBe` [Only "ambiguous"]
            readIORef (rfSentRawTransactions fixture) `shouldReturn` []
            assertNoUnexpectedRequests fixture

    it "observe rollback reconciles a persisted receipt but never rebroadcasts or replaces" $
      withWorkerDatabase databaseUrl $ \pool ->
        withRpcFixture False $ \fixture client ->
          withDb pool $ \conn -> do
            signed <- fixtureSignedTransaction 9
            pendingTx <- seedPendingTransaction conn signed

            processLpSettlementCycle (workerConfig LpSettlementObserve) conn client
            stillPending <- getLpSettlementTransactionById conn $ lstrId pendingTx
            fmap lstrStatus stillPending `shouldBe` Just "pending"
            readIORef (rfSentRawTransactions fixture) `shouldReturn` []

            let receiptBlock = 911
                receiptHash = blockHash receiptBlock
            modifyIORef' (rfReceipts fixture) $
              insertAssociation
                (T.toLower $ signedTransactionHash signed)
                (successfulReceipt (signedTransactionHash signed) receiptBlock receiptHash)
            writeIORef (rfHasMaturedWork fixture) False
            processLpSettlementCycle (workerConfig LpSettlementObserve) conn client

            confirmed <- getLpSettlementTransactionById conn $ lstrId pendingTx
            fmap lstrStatus confirmed `shouldBe` Just "confirmed_success"
            fmap lstrReceiptBlockHash confirmed `shouldBe` Just (Just receiptHash)
            readIORef (rfSentRawTransactions fixture) `shouldReturn` []
            assertNoUnexpectedRequests fixture

    it "recovers a manual nonce-consumed lane when its late persisted receipt appears" $
      withWorkerDatabase databaseUrl $ \pool ->
        withRpcFixture False $ \fixture client ->
          withDb pool $ \conn -> do
            signed <- fixtureSignedTransaction 9
            pendingTx <- seedPendingTransaction conn signed
            writeIORef (rfConfirmedNonce fixture) 10

            processLpSettlementCycle (workerConfig LpSettlementExecute) conn client
            reviewed <- getLpSettlementTransactionById conn $ lstrId pendingTx
            fmap lstrStatus reviewed `shouldBe` Just "manual_review"

            let receiptBlock = 912
                receiptHash = blockHash receiptBlock
            modifyIORef' (rfReceipts fixture) $
              insertAssociation
                (T.toLower $ signedTransactionHash signed)
                (successfulReceipt (signedTransactionHash signed) receiptBlock receiptHash)
            writeIORef (rfHasMaturedWork fixture) False
            processLpSettlementCycle (workerConfig LpSettlementExecute) conn client

            recovered <- getLpSettlementTransactionById conn $ lstrId pendingTx
            fmap lstrStatus recovered `shouldBe` Just "confirmed_success"
            fmap lstrReceiptBlockHash recovered `shouldBe` Just (Just receiptHash)
            readIORef (rfSentRawTransactions fixture) `shouldReturn` []
            assertNoUnexpectedRequests fixture

    it "retains malformed success-receipt evidence and enters manual review" $
      withWorkerDatabase databaseUrl $ \pool ->
        withRpcFixture False $ \fixture client ->
          withDb pool $ \conn -> do
            signed <- fixtureSignedTransaction 9
            pendingTx <- seedPendingTransaction conn signed
            let receiptBlock = 913
                receiptHash = blockHash receiptBlock
            modifyIORef' (rfReceipts fixture) $
              insertAssociation
                (T.toLower $ signedTransactionHash signed)
                (receiptWithoutSettlementEvent (signedTransactionHash signed) receiptBlock receiptHash)

            processLpSettlementCycle (workerConfig LpSettlementExecute) conn client

            reviewed <- getLpSettlementTransactionById conn $ lstrId pendingTx
            fmap lstrStatus reviewed `shouldBe` Just "manual_review"
            fmap lstrReceiptBlockHash reviewed `shouldBe` Just (Just receiptHash)
            fmap lstrReceiptSucceeded reviewed `shouldBe` Just (Just True)
            readIORef (rfSentRawTransactions fixture) `shouldReturn` []
            assertNoUnexpectedRequests fixture

    it "accepts only an earlier same-block, backlog-free competitor plus exact no-progress replay" $
      withWorkerDatabase databaseUrl $ \pool ->
        withRpcFixture False $ \fixture client ->
          withDb pool $ \conn -> do
            signed <- fixtureSignedTransaction 9
            let earlyObservation =
                  fixtureObservation
                    { lsoiObservedBlock = 100
                    , lsoiObservedBlockHash = Just $ blockHash 100
                    }
            recordLpSettlementObservationV2 conn earlyObservation
            prepared <- prepareLpSettlementTransaction conn $ fixtureIntent signed
            _ <- appendLpSettlementBroadcast conn $
              LpSettlementBroadcastInput
                { lsbiAttemptId = lstrId prepared
                , lsbiOutcome = "ambiguous"
                , lsbiReturnedTransactionHash = Nothing
                , lsbiRpcError = Just "lost response"
                }
            let receiptBlock = 914
                receiptHash = blockHash receiptBlock
                reverted = revertedReceipt (signedTransactionHash signed) receiptBlock receiptHash 1
                competitor = competitorSettlementLog receiptBlock receiptHash 0
            modifyIORef' (rfReceipts fixture) $
              insertAssociation (T.toLower $ signedTransactionHash signed) reverted
            writeIORef (rfLogs fixture) [competitor]
            writeIORef (rfReplayNoProgress fixture) True
            writeIORef (rfHasMaturedWork fixture) False

            processLpSettlementCycle (workerConfig LpSettlementExecute) conn client

            superseded <- getLpSettlementTransactionById conn $ lstrId prepared
            fmap lstrStatus superseded `shouldBe` Just "superseded"
            fmap lstrReceiptSucceeded superseded `shouldBe` Just (Just False)
            readIORef (rfSentRawTransactions fixture) `shouldReturn` []
            assertNoUnexpectedRequests fixture

    it "retains the active nonce lane when a previously seen receipt block is reorged" $
      withWorkerDatabase databaseUrl $ \pool ->
        withRpcFixture False $ \fixture client ->
          withDb pool $ \conn -> do
            signed <- fixtureSignedTransaction 9
            prepared <- seedPreparedTransaction conn signed
            let receiptBlock = 910
                receiptHash = blockHash receiptBlock
                replacementCanonicalHash = hexText 64 "f"
                receipt = successfulReceipt (signedTransactionHash signed) receiptBlock receiptHash
            _ <- appendLpSettlementBroadcast conn $
              LpSettlementBroadcastInput
                { lsbiAttemptId = lstrId prepared
                , lsbiOutcome = "accepted"
                , lsbiReturnedTransactionHash = Just $ signedTransactionHash signed
                , lsbiRpcError = Nothing
                }
            markLpSettlementTransactionConfirming
              conn
              (lstrId prepared)
              (signedTransactionHash signed)
              receiptBlock
              receiptHash
              True
              1
            modifyIORef' (rfReceipts fixture) $
              insertAssociation (T.toLower $ signedTransactionHash signed) receipt
            modifyIORef' (rfCanonicalBlockHashes fixture) $
              insertAssociation receiptBlock replacementCanonicalHash

            processLpSettlementCycle (workerConfig LpSettlementExecute) conn client

            active <- getActiveLpSettlementTransaction conn fixtureChainId monitor fixtureSigner
            fmap lstrStatus active `shouldBe` Just "pending"
            fmap lstrReceiptBlockHash active `shouldBe` Just Nothing
            readIORef (rfSentRawTransactions fixture) `shouldReturn` []
            assertNoUnexpectedRequests fixture

    it "confirms and immediately drains exactly four transactions even while safe work remains" $
      withWorkerDatabase databaseUrl $ \pool ->
        withRpcFixture True $ \fixture client ->
          withDb pool $ \conn -> do
            processLpSettlementCycle (workerConfig LpSettlementExecute) conn client

            sends <- readIORef $ rfSentRawTransactions fixture
            estimates <- readIORef $ rfEstimateCalls fixture
            statuses <- query_ conn
              "SELECT status FROM perps_lp_settlement_transactions ORDER BY id"
              :: IO [Only Text]
            active <- getActiveLpSettlementTransaction conn fixtureChainId monitor fixtureSigner
            length sends `shouldBe` 4
            length estimates `shouldBe` 4
            statuses `shouldBe` replicate 4 (Only "confirmed_success")
            active `shouldBe` Nothing
            assertNoUnexpectedRequests fixture

    it "keeps LP progress independent when the order worker advisory lock is unavailable" $
      withWorkerDatabase databaseUrl $ \pool ->
        withRpcFixture True $ \fixture client ->
          withDb pool $ \orderLockConnection ->
            bracket
              (tryPerpsKeeperLock orderLockConnection)
              (\acquired -> when acquired $ unlockPerpsKeeperLock orderLockConnection)
              $ \acquired -> do
                acquired `shouldBe` True
                stopped <- newEmptyMVar
                workerThread <-
                  forkFinally
                    (runKeeper (workerConfig LpSettlementExecute) pool client KeeperLoop False)
                    (putMVar stopped)

                progressed <- timeout 5_000_000 $ waitForSentTransactions fixture 4
                confirmed <- timeout 5_000_000 $ waitForConfirmedLpTransactions orderLockConnection 4
                killThread workerThread
                terminated <- timeout 5_000_000 $ takeMVar stopped

                progressed `shouldBe` Just ()
                confirmed `shouldBe` Just ()
                terminated `shouldSatisfy` maybe False (const True)
                sends <- readIORef $ rfSentRawTransactions fixture
                length sends `shouldBe` 4
                assertNoUnexpectedRequests fixture

    it "keeps LP progress independent while the order worker is blocked fetching logs" $
      withWorkerDatabase databaseUrl $ \pool ->
        withRpcFixture True $ \fixture client ->
          withDb pool $ \conn -> do
            setPerpsKeeperLastIndexedBlock conn router 0
            writeIORef (rfBlockOrderLogs fixture) True
            stopped <- newEmptyMVar
            workerThread <-
              forkFinally
                (runKeeper (workerConfig LpSettlementExecute) pool client KeeperLoop False)
                (putMVar stopped)

            orderBlocked <- timeout 5_000_000 $ takeMVar $ rfOrderLogBlocked fixture
            progressed <- timeout 5_000_000 $ waitForSentTransactions fixture 4
            writeIORef (rfBlockOrderLogs fixture) False
            _ <- tryPutMVar (rfReleaseOrderLogs fixture) ()
            orderResumed <- timeout 5_000_000 $ waitForOrderIndex conn pinnedBlockNumber
            threadDelay 50_000
            killThread workerThread
            terminated <- timeout 5_000_000 $ takeMVar stopped

            orderBlocked `shouldBe` Just ()
            progressed `shouldBe` Just ()
            orderResumed `shouldBe` Just ()
            terminated `shouldSatisfy` maybe False (const True)
            sends <- readIORef $ rfSentRawTransactions fixture
            length sends `shouldBe` 4
            assertNoUnexpectedRequests fixture

processLpSettlementCycle :: Config -> Connection -> EthClient -> IO ()
processLpSettlementCycle = processLpSettlementCycleWithCodeHashes fixtureCodeHashes

runKeeper :: Config -> DbPool -> EthClient -> KeeperMode -> Bool -> IO ()
runKeeper = runKeeperWithCodeHashes fixtureCodeHashes

data EstimateCall = EstimateCall
  { ecFrom :: Text
  , ecTo :: Text
  , ecValue :: Text
  , ecData :: Text
  }
  deriving (Eq, Show)

data RpcFixture = RpcFixture
  { rfAutoMine :: Bool
  , rfSentRawTransactions :: IORef [Text]
  , rfEstimateCalls :: IORef [EstimateCall]
  , rfReceipts :: IORef [(Text, Value)]
  , rfCanonicalBlockHashes :: IORef [(Integer, Text)]
  , rfCodeDrift :: IORef Bool
  , rfBlockOrderLogs :: IORef Bool
  , rfOrderLogBlocked :: MVar ()
  , rfReleaseOrderLogs :: MVar ()
  , rfConfirmedNonce :: IORef Integer
  , rfHasMaturedWork :: IORef Bool
  , rfLogs :: IORef [Value]
  , rfReplayNoProgress :: IORef Bool
  , rfUnexpectedRequests :: IORef [Text]
  }

withRpcFixture :: Bool -> (RpcFixture -> EthClient -> IO a) -> IO a
withRpcFixture autoMine action = do
  sent <- newIORef []
  estimates <- newIORef []
  receipts <- newIORef []
  canonicalHashes <- newIORef []
  codeDrift <- newIORef False
  blockOrderLogs <- newIORef False
  orderLogBlocked <- newEmptyMVar
  releaseOrderLogs <- newEmptyMVar
  confirmedNonce <- newIORef 0
  hasMaturedWork <- newIORef True
  logs <- newIORef []
  replayNoProgress <- newIORef False
  unexpectedRequests <- newIORef []
  let fixture =
        RpcFixture
          { rfAutoMine = autoMine
          , rfSentRawTransactions = sent
          , rfEstimateCalls = estimates
          , rfReceipts = receipts
          , rfCanonicalBlockHashes = canonicalHashes
          , rfCodeDrift = codeDrift
          , rfBlockOrderLogs = blockOrderLogs
          , rfOrderLogBlocked = orderLogBlocked
          , rfReleaseOrderLogs = releaseOrderLogs
          , rfConfirmedNonce = confirmedNonce
          , rfHasMaturedWork = hasMaturedWork
          , rfLogs = logs
          , rfReplayNoProgress = replayNoProgress
          , rfUnexpectedRequests = unexpectedRequests
          }
  testWithApplication (pure $ rpcApplication fixture) $ \port -> do
    client <- newClient $ "http://127.0.0.1:" <> T.pack (show port)
    action fixture client

rpcApplication :: RpcFixture -> Application
rpcApplication fixture request respond = do
  body <- strictRequestBody request
  let requestId = rpcRequestId body
      replayRequest = case rpcMethodAndParams body of
        Just ("eth_estimateGas", [_call, String _blockTag]) -> True
        _ -> False
  replayNoProgress <- readIORef $ rfReplayNoProgress fixture
  responseValue <-
    if replayRequest && replayNoProgress
      then
        pure $
          object
            [ "jsonrpc" .= ("2.0" :: Text)
            , "id" .= requestId
            , "error"
                .= object
                  [ "code" .= (-32000 :: Int)
                  , "message" .= ("execution reverted" :: Text)
                  , "data" .= ("0x86cca6b8" :: Text)
                  ]
            ]
      else do
        result <- rpcResult fixture body
        pure $
          object
            [ "jsonrpc" .= ("2.0" :: Text)
            , "id" .= requestId
            , "result" .= result
            ]
  respond $ responseLBS status200 [("Content-Type", "application/json")] $ encode responseValue

rpcResult :: RpcFixture -> LBS.ByteString -> IO Value
rpcResult fixture body =
  case rpcMethodAndParams body of
    Just ("eth_chainId", _) -> pure $ String $ quantity fixtureChainId
    Just ("eth_getCode", params) ->
      case params of
        [String target, String _blockTag] -> do
          drifted <- readIORef $ rfCodeDrift fixture
          case fixtureContractCode target of
            Just code
              | drifted && normalize target == normalize monitor ->
                  pure $ String $ bytesHex $ runtimeCode 255
              | otherwise -> pure $ String $ bytesHex code
            Nothing -> unexpected fixture ("eth_getCode for unknown address " <> target) params
        _ -> unexpected fixture "malformed eth_getCode" params
    Just ("eth_getBalance", _) -> pure $ String "0x56bc75e2d63100000"
    Just ("eth_blockNumber", _) -> pure $ String $ quantity chainHead
    Just ("eth_gasPrice", _) -> pure $ String "0x3b9aca00"
    Just ("eth_maxPriorityFeePerGas", _) -> pure $ String "0x77359400"
    Just ("eth_getTransactionCount", params) ->
      case params of
        [String _, String "pending"] -> do
          sentCount <- length <$> readIORef (rfSentRawTransactions fixture)
          pure $ String $ quantity $ fromIntegral sentCount
        [String _, String _] -> do
          nonce <- readIORef $ rfConfirmedNonce fixture
          pure $ String $ quantity nonce
        _ -> unexpected fixture "malformed eth_getTransactionCount" params
    Just ("eth_getBlockByNumber", params) ->
      case params of
        [String blockTag, Bool False] -> do
          let number = parseQuantity blockTag
          overrides <- readIORef $ rfCanonicalBlockHashes fixture
          pure $ blockValue number $ maybe (blockHash number) id $ lookup number overrides
        _ -> unexpected fixture "malformed eth_getBlockByNumber" params
    Just ("eth_call", params) -> handleEthCall fixture params
    Just ("eth_estimateGas", params) -> do
      case estimateCall params of
        Just estimate -> modifyIORef' (rfEstimateCalls fixture) (<> [estimate])
        Nothing -> void $ unexpected fixture "malformed eth_estimateGas" params
      pure $ String "0x186a0"
    Just ("eth_sendRawTransaction", params) -> handleSend fixture params
    Just ("eth_getLogs", params) -> do
      blockOrderLogs <- readIORef $ rfBlockOrderLogs fixture
      when (blockOrderLogs && isOrderLogQuery params) $ do
        _ <- tryPutMVar (rfOrderLogBlocked fixture) ()
        takeMVar $ rfReleaseOrderLogs fixture
      toJSON <$> readIORef (rfLogs fixture)
    Just ("eth_getTransactionReceipt", params) ->
      case params of
        [String txHash] -> do
          receipts <- readIORef $ rfReceipts fixture
          pure $ maybe Null id $ lookup (T.toLower txHash) receipts
        _ -> unexpected fixture "malformed eth_getTransactionReceipt" params
    Just (method, params) -> unexpected fixture ("unexpected method " <> method) params
    Nothing -> do
      modifyIORef' (rfUnexpectedRequests fixture) (<> ["malformed JSON-RPC request"])
      pure Null

handleEthCall :: RpcFixture -> [Value] -> IO Value
handleEthCall fixture params =
  case params of
    [Object call, String _blockTag] -> do
      hasMaturedWork <- readIORef $ rfHasMaturedWork fixture
      case (textField "to" call, textField "data" call) of
        (Just target, Just calldata)
          | normalize target == normalize monitor
              && calldata == callHex "CONFIG_SCHEMA_VERSION()" [] ->
              pure $ String $ bytesHex $ encodeUint256 supportedConfigSchemaVersion
          | normalize target == normalize monitor
              && calldata == callHex "ROUTER()" [] ->
              addressResult router
          | normalize target == normalize monitor
              && calldata == callHex "ENGINE()" [] ->
              addressResult engine
          | normalize target == normalize monitor
              && calldata == callHex "HOUSE_POOL()" [] ->
              addressResult housePool
          | normalize target == normalize monitor
              && calldata == callHex "SENIOR_VAULT()" [] ->
              addressResult seniorVault
          | normalize target == normalize monitor
              && calldata == callHex "JUNIOR_VAULT()" [] ->
              addressResult juniorVault
          | normalize target == normalize router
              && calldata == callHex "engine()" [] ->
              addressResult engine
          | normalize target == normalize router
              && calldata == callHex "pletherOracle()" [] ->
              addressResult oracle
          | normalize target == normalize engine
              && calldata == callHex "orderRouter()" [] ->
              addressResult router
          | normalize target == normalize housePool
              && calldata == callHex "ENGINE()" [] ->
              addressResult engine
          | normalize target == normalize housePool
              && calldata == callHex "seniorVault()" [] ->
              addressResult seniorVault
          | normalize target == normalize housePool
              && calldata == callHex "juniorVault()" [] ->
              addressResult juniorVault
          | normalize target == normalize seniorVault
              && calldata == callHex "POOL()" [] ->
              addressResult housePool
          | normalize target == normalize seniorVault
              && calldata == callHex "IS_SENIOR()" [] ->
              pure $ String $ bytesHex $ encodeUint256 1
          | normalize target == normalize juniorVault
              && calldata == callHex "POOL()" [] ->
              addressResult housePool
          | normalize target == normalize juniorVault
              && calldata == callHex "IS_SENIOR()" [] ->
              pure $ String $ bytesHex $ encodeUint256 0
          | normalize target == normalize housePool
              && calldata == callHex "currentLpEpoch()" [] ->
              pure $ String $ bytesHex $ encodeUint256 fixtureEpoch
          | normalize target == normalize monitor
              && calldata == callHex "getSettlementStatus(uint256)" [encodeUint256 fixtureEpoch] ->
              pure $ String $ bytesHex $ settlementStatusBytes hasMaturedWork
          | normalize target == normalize monitor
              && calldata == callHex "getSettlementObservation(uint256)" [encodeUint256 fixtureEpoch] ->
              pure $ String $ bytesHex $ settlementObservationBytes hasMaturedWork
          | otherwise -> unexpected fixture ("unexpected eth_call " <> target <> " " <> calldata) params
        _ -> unexpected fixture "malformed eth_call object" params
    _ -> unexpected fixture "malformed eth_call" params

addressResult :: Text -> IO Value
addressResult = pure . String . bytesHex . encodeAddress

handleSend :: RpcFixture -> [Value] -> IO Value
handleSend fixture params =
  case params of
    [String rawText] -> do
      raw <- case decodeHexText rawText of
        Right value -> pure value
        Left err -> do
          modifyIORef' (rfUnexpectedRequests fixture) (<> [err])
          pure BS.empty
      let txHash = rawTransactionHash raw
      sendIndex <- atomicModifyIORef' (rfSentRawTransactions fixture) $ \sent ->
        let next = sent <> [rawText]
         in (next, length next)
      if rfAutoMine fixture
        then do
          let receiptBlock = 900 + fromIntegral sendIndex
              receiptHash = blockHash receiptBlock
          modifyIORef' (rfReceipts fixture) $
            insertAssociation
              (T.toLower txHash)
              (successfulReceipt txHash receiptBlock receiptHash)
        else pure ()
      pure $ String txHash
    _ -> unexpected fixture "malformed eth_sendRawTransaction" params

unexpected :: RpcFixture -> Text -> [Value] -> IO Value
unexpected fixture label params = do
  modifyIORef' (rfUnexpectedRequests fixture) $
    (<> [label <> ": " <> TE.decodeUtf8 (LBS.toStrict $ encode params)])
  pure Null

assertNoUnexpectedRequests :: RpcFixture -> Expectation
assertNoUnexpectedRequests fixture = do
  requests <- readIORef $ rfUnexpectedRequests fixture
  unless (null requests) $
    expectationFailure $ "unexpected scripted RPC requests: " <> show requests

waitForSentTransactions :: RpcFixture -> Int -> IO ()
waitForSentTransactions fixture expectedCount = do
  sentCount <- length <$> readIORef (rfSentRawTransactions fixture)
  if sentCount >= expectedCount
    then pure ()
    else threadDelay 10_000 >> waitForSentTransactions fixture expectedCount

waitForConfirmedLpTransactions :: Connection -> Integer -> IO ()
waitForConfirmedLpTransactions conn expectedCount = do
  rows <-
    query_
      conn
      "SELECT COUNT(*) FROM perps_lp_settlement_transactions WHERE status = 'confirmed_success'"
      :: IO [Only Integer]
  case rows of
    [Only confirmedCount]
      | confirmedCount >= expectedCount -> pure ()
      | otherwise -> threadDelay 10_000 >> waitForConfirmedLpTransactions conn expectedCount
    _ -> expectationFailure "could not count confirmed LP settlement transactions"

waitForOrderIndex :: Connection -> Integer -> IO ()
waitForOrderIndex conn expectedBlock = do
  indexedBlock <- getPerpsKeeperLastIndexedBlock conn router
  if indexedBlock >= expectedBlock
    then pure ()
    else threadDelay 10_000 >> waitForOrderIndex conn expectedBlock

successfulReceipt :: Text -> Integer -> Text -> Value
successfulReceipt txHash receiptBlock receiptHash =
  object
    [ "transactionHash" .= txHash
    , "transactionIndex" .= ("0x0" :: Text)
    , "blockNumber" .= quantity receiptBlock
    , "blockHash" .= receiptHash
    , "status" .= ("0x1" :: Text)
    , "logs"
        .= [ object
              [ "transactionHash" .= txHash
              , "transactionIndex" .= ("0x0" :: Text)
              , "blockNumber" .= quantity receiptBlock
              , "blockHash" .= receiptHash
              , "logIndex" .= ("0x0" :: Text)
              , "address" .= housePool
              , "topics"
                  .= [ bytesHex lpEpochSettledTopic
                     , bytesHex $ encodeUint256 fixtureEpoch
                     ]
              , "data"
                  .= bytesHex
                    ( mconcat $
                        map encodeUint256 [11, 12, 13, 14, 1, 0, 1]
                    )
              ]
           ]
    ]

receiptWithoutSettlementEvent :: Text -> Integer -> Text -> Value
receiptWithoutSettlementEvent txHash receiptBlock receiptHash =
  object
    [ "transactionHash" .= txHash
    , "transactionIndex" .= ("0x0" :: Text)
    , "blockNumber" .= quantity receiptBlock
    , "blockHash" .= receiptHash
    , "status" .= ("0x1" :: Text)
    , "logs" .= ([] :: [Value])
    ]

revertedReceipt :: Text -> Integer -> Text -> Integer -> Value
revertedReceipt txHash receiptBlock receiptHash transactionIndex =
  object
    [ "transactionHash" .= txHash
    , "transactionIndex" .= quantity transactionIndex
    , "blockNumber" .= quantity receiptBlock
    , "blockHash" .= receiptHash
    , "status" .= ("0x0" :: Text)
    , "logs" .= ([] :: [Value])
    ]

competitorSettlementLog :: Integer -> Text -> Integer -> Value
competitorSettlementLog eventBlock eventBlockHash transactionIndex =
  object
    [ "transactionHash" .= hexText 64 "d"
    , "transactionIndex" .= quantity transactionIndex
    , "blockNumber" .= quantity eventBlock
    , "blockHash" .= eventBlockHash
    , "logIndex" .= ("0x0" :: Text)
    , "address" .= housePool
    , "topics"
        .= [ bytesHex lpEpochSettledTopic
           , bytesHex $ encodeUint256 fixtureEpoch
           ]
    , "data"
        .= bytesHex
          (mconcat $ map encodeUint256 [11, 12, 13, 14, 0, 0, 0])
    ]

settlementStatusBytes :: Bool -> BS.ByteString
settlementStatusBytes hasMaturedWork =
  setWords
    109
    [ (1, pinnedBlockNumber)
    , (4, fixtureEpoch)
    , (5, fixtureEpoch)
    , (11, cachedMarkTime)
    , (25, fixtureEpoch - 1)
    , (26, 1)
    , (86, 2)
    , (87, cachedMark)
    , (88, cachedMarkTime)
    , (99, if hasMaturedWork then 1 else 0)
    ]

settlementObservationBytes :: Bool -> BS.ByteString
settlementObservationBytes hasMaturedWork =
  replaceWord 191 (hexBytes 64 "c") $
    setWords
      194
      [ (0, supportedObservationSchemaVersion)
      , (2, pinnedBlockNumber)
      , (5, fixtureEpoch)
      , (6, fixtureEpoch)
      , (12, cachedMarkTime)
      , (26, fixtureEpoch - 1)
      , (27, 1)
      , (87, 2)
      , (88, cachedMark)
      , (89, cachedMarkTime)
      , (100, if hasMaturedWork then 1 else 0)
      , (159, 1)
      , (193, 1)
      ]

setWords :: Int -> [(Int, Integer)] -> BS.ByteString
setWords wordCount values =
  foldr (uncurry replaceIntegerWord) (BS.replicate (wordCount * 32) 0) values

replaceIntegerWord :: Int -> Integer -> BS.ByteString -> BS.ByteString
replaceIntegerWord index value = replaceWord index (encodeUint256 value)

replaceWord :: Int -> BS.ByteString -> BS.ByteString -> BS.ByteString
replaceWord index value bytes =
  BS.take (index * 32) bytes
    <> BS.take 32 value
    <> BS.drop ((index + 1) * 32) bytes

estimateCall :: [Value] -> Maybe EstimateCall
estimateCall params = do
  Object call <- case params of
    value : _ -> Just value
    [] -> Nothing
  EstimateCall
    <$> textField "from" call
    <*> textField "to" call
    <*> textField "value" call
    <*> textField "data" call

isOrderLogQuery :: [Value] -> Bool
isOrderLogQuery = \case
  [Object queryObject] ->
    maybe False ((== normalize router) . normalize) $ textField "address" queryObject
  _ -> False

rpcMethodAndParams :: LBS.ByteString -> Maybe (Text, [Value])
rpcMethodAndParams body = do
  Object request <- decode body
  String method <- KeyMap.lookup (Key.fromText "method") request
  Array params <- KeyMap.lookup (Key.fromText "params") request
  pure (method, toList params)

rpcRequestId :: LBS.ByteString -> Value
rpcRequestId body =
  case decode body of
    Just (Object request) ->
      maybe Null id $ KeyMap.lookup (Key.fromText "id") request
    _ -> Null

textField :: Text -> KeyMap.KeyMap Value -> Maybe Text
textField name value = do
  String result <- KeyMap.lookup (Key.fromText name) value
  pure result

callHex :: Text -> [BS.ByteString] -> Text
callHex signature arguments = bytesHex $ encodeCall signature arguments

bytesHex :: BS.ByteString -> Text
bytesHex = ("0x" <>) . TE.decodeUtf8 . B16.encode

decodeHexText :: Text -> Either Text BS.ByteString
decodeHexText value =
  case B16.decode $ TE.encodeUtf8 $ fromMaybePrefix $ normalize value of
    Right bytes -> Right bytes
    Left err -> Left $ "invalid raw transaction hex: " <> T.pack err
 where
  fromMaybePrefix normalized =
    case T.stripPrefix "0x" normalized of
      Just stripped -> stripped
      Nothing -> normalized

normalize :: Text -> Text
normalize = T.toLower . T.strip

insertAssociation :: (Eq key) => key -> value -> [(key, value)] -> [(key, value)]
insertAssociation key value existing =
  (key, value) : filter ((/= key) . fst) existing

quantity :: Integer -> Text
quantity value = "0x" <> T.pack (showHex value "")

parseQuantity :: Text -> Integer
parseQuantity value =
  case readHexInteger $ T.drop 2 value of
    Just number -> number
    Nothing -> 0

readHexInteger :: Text -> Maybe Integer
readHexInteger value =
  case reads $ "0x" <> T.unpack value of
    [(number, "")] -> Just number
    _ -> Nothing

blockValue :: Integer -> Text -> Value
blockValue number hash =
  object
    [ "number" .= quantity number
    , "hash" .= hash
    , "timestamp" .= ("0x6553f100" :: Text)
    ]

blockHash :: Integer -> Text
blockHash number =
  "0x" <> T.justifyRight 64 '0' (T.pack $ showHex number "")

hexText :: Int -> Text -> Text
hexText count digit = "0x" <> T.replicate count digit

hexBytes :: Int -> Text -> BS.ByteString
hexBytes count digit =
  case B16.decode $ TE.encodeUtf8 $ T.replicate count digit of
    Right value -> value
    Left _ -> BS.empty

withWorkerDatabase :: Text -> (DbPool -> IO a) -> IO a
withWorkerDatabase databaseUrl action =
  bracket (newDbPool databaseUrl) destroyAllResources $ \pool -> do
    withDb pool $ \conn -> do
      assertDedicatedDatabase conn
      ensurePerpsKeeperSchema conn
      verifyLpSettlementSchema conn `shouldReturn` Right ()
    cleanupWorkerTables pool
    action pool `finally` cleanupWorkerTables pool

assertDedicatedDatabase :: Connection -> IO ()
assertDedicatedDatabase conn = do
  rows <- query_ conn "SELECT current_database()" :: IO [Only Text]
  case rows of
    [Only databaseName]
      | "critical_path" `T.isInfixOf` T.toLower databaseName -> pure ()
    [Only databaseName] ->
      expectationFailure $
        "Refusing destructive LP worker cleanup against database "
          <> T.unpack databaseName
          <> "; its name must contain critical_path"
    _ -> expectationFailure "PostgreSQL did not return exactly one database name"

cleanupWorkerTables :: DbPool -> IO ()
cleanupWorkerTables pool =
  withDb pool $ \conn ->
    void $ execute_ conn
      "TRUNCATE TABLE perps_lp_settlement_broadcasts, \
      \perps_lp_settlement_transactions, perps_lp_settlement_observations \
      \RESTART IDENTITY"

tableCount :: Connection -> Text -> IO Integer
tableCount conn tableName = do
  rows <- query_ conn $ fromString $ "SELECT COUNT(*) FROM " <> T.unpack tableName
  case rows of
    [Only count] -> pure count
    _ -> expectationFailure "could not count LP settlement table" >> pure (-1)

seedPreparedTransaction :: Connection -> SignedTransaction -> IO LpSettlementTransactionRow
seedPreparedTransaction conn signed = do
  recordLpSettlementObservationV2 conn fixtureObservation
  prepareLpSettlementTransaction conn $ fixtureIntent signed

seedPendingTransaction :: Connection -> SignedTransaction -> IO LpSettlementTransactionRow
seedPendingTransaction conn signed = do
  prepared <- seedPreparedTransaction conn signed
  _ <- appendLpSettlementBroadcast conn $
    LpSettlementBroadcastInput
      { lsbiAttemptId = lstrId prepared
      , lsbiOutcome = "ambiguous"
      , lsbiReturnedTransactionHash = Nothing
      , lsbiRpcError = Just "lost response"
      }
  refreshed <- getLpSettlementTransactionById conn $ lstrId prepared
  case refreshed of
    Just row -> pure row
    Nothing -> expectationFailure "missing pending transaction" >> fail "missing transaction"

seedStaleAmbiguousTransaction :: Connection -> SignedTransaction -> Integer -> IO Integer
seedStaleAmbiguousTransaction conn signed ageSeconds = do
  recordLpSettlementObservationV2 conn fixtureObservation
  rows <- query conn
    "INSERT INTO perps_lp_settlement_transactions \
    \(chain_id, monitor_address, observation_digest, epoch, replacement_count, replaces_attempt_id, \
    \signer_address, tx_nonce, target_address, tx_value, calldata, gas_limit, \
    \max_priority_fee_per_gas, max_fee_per_gas, signed_raw_transaction, \
    \signed_transaction_hash, status, created_at, updated_at) \
    \VALUES (?, ?, ?, ?, 0, NULL, ?, 7, ?, 0, ?, 125000, 1000000000, 2000000000, ?, ?, \
    \'prepared', NOW() - (? * INTERVAL '1 second'), NOW() - INTERVAL '31 seconds') RETURNING id"
    ( fixtureChainId
    , monitor
    , fixtureDigest
    , fixtureEpoch
    , fixtureSigner
    , housePool
    , Binary $ settleLpEpochPoolCall cachedMark cachedMarkTime
    , Binary $ signedRawTransaction signed
    , signedTransactionHash signed
    , ageSeconds
    ) :: IO [Only Integer]
  attemptId <- case rows of
    [Only value] -> pure value
    _ -> expectationFailure "could not seed stale LP transaction" >> fail "missing transaction"
  seedAgedAmbiguousBroadcast conn attemptId
  pure attemptId

seedReplacementCapLineage :: Connection -> Integer -> IO Integer
seedReplacementCapLineage conn ageSeconds = do
  original <- fixtureSignedTransactionWithFees 7 1_000_000_000 2_000_000_000
  originalId <- seedStaleAmbiguousTransaction conn original ageSeconds
  replacements <-
    mapM
      (\(priorityFee, maxFee) -> do
        signed <- fixtureSignedTransactionWithFees 7 priorityFee maxFee
        pure (priorityFee, maxFee, signed)
      )
      [ (1_100_000_000, 2_100_000_000)
      , (1_200_000_000, 2_200_000_000)
      , (1_300_000_000, 2_300_000_000)
      ]
  finalId <- foldM (insertAgedReplacement conn ageSeconds) originalId replacements
  seedAgedAmbiguousBroadcast conn finalId
  pure finalId

insertAgedReplacement
  :: Connection
  -> Integer
  -> Integer
  -> (Integer, Integer, SignedTransaction)
  -> IO Integer
insertAgedReplacement conn ageSeconds predecessorId (priorityFee, maxFee, signed) =
  withTransaction conn $ do
    replaced <- execute conn
      "UPDATE perps_lp_settlement_transactions \
      \SET status = 'replaced', last_error = NULL, updated_at = NOW() \
      \WHERE id = ? AND status IN ('prepared', 'broadcast', 'pending')"
      (Only predecessorId)
    replaced `shouldBe` 1
    rows <- query conn
      "INSERT INTO perps_lp_settlement_transactions \
      \(chain_id, monitor_address, observation_digest, epoch, replacement_count, replaces_attempt_id, \
      \signer_address, tx_nonce, target_address, tx_value, calldata, gas_limit, \
      \max_priority_fee_per_gas, max_fee_per_gas, signed_raw_transaction, \
      \signed_transaction_hash, status, created_at, updated_at) \
      \SELECT chain_id, monitor_address, observation_digest, epoch, replacement_count + 1, id, \
      \signer_address, tx_nonce, target_address, tx_value, calldata, gas_limit, ?, ?, ?, ?, \
      \'prepared', NOW() - (? * INTERVAL '1 second'), NOW() \
      \FROM perps_lp_settlement_transactions WHERE id = ? RETURNING id"
      ( priorityFee
      , maxFee
      , Binary $ signedRawTransaction signed
      , signedTransactionHash signed
      , ageSeconds
      , predecessorId
      ) :: IO [Only Integer]
    case rows of
      [Only replacementId] -> pure replacementId
      _ -> expectationFailure "could not seed LP replacement lineage" >> fail "missing replacement"

seedAgedAmbiguousBroadcast :: Connection -> Integer -> IO ()
seedAgedAmbiguousBroadcast conn attemptId = do
  _ <- execute conn
    "INSERT INTO perps_lp_settlement_broadcasts \
    \(attempt_id, broadcast_sequence, outcome, rpc_error, broadcast_at) \
    \VALUES (?, 1, 'ambiguous', 'lost response', NOW() - INTERVAL '31 seconds')"
    (Only attemptId)
  _ <- execute conn
    "UPDATE perps_lp_settlement_transactions \
    \SET status = 'pending', last_error = 'lost response', updated_at = NOW() WHERE id = ?"
    (Only attemptId)
  pure ()

fixtureSignedTransaction :: Integer -> IO SignedTransaction
fixtureSignedTransaction nonce =
  fixtureSignedTransactionWithFees nonce 1_000_000_000 2_000_000_000

fixtureSignedTransactionWithFees :: Integer -> Integer -> Integer -> IO SignedTransaction
fixtureSignedTransactionWithFees nonce priorityFee maxFee = do
  result <- signTransaction fixturePrivateKey $
    Tx1559
      { txChainId = fixtureChainId
      , txNonce = nonce
      , txMaxPriorityFeePerGas = priorityFee
      , txMaxFeePerGas = maxFee
      , txGasLimit = 125_000
      , txTo = housePool
      , txValue = 0
      , txData = settleLpEpochPoolCall cachedMark cachedMarkTime
      }
  case result of
    Right signed -> pure signed
    Left err -> expectationFailure (T.unpack err) >> fail "could not sign fixture"

fixtureObservation :: LpSettlementObservationInput
fixtureObservation =
  LpSettlementObservationInput
    { lsoiChainId = fixtureChainId
    , lsoiMonitorAddress = monitor
    , lsoiObservationDigest = fixtureDigest
    , lsoiEpoch = fixtureEpoch
    , lsoiObservedBlock = pinnedBlockNumber
    , lsoiObservedBlockHash = Just $ blockHash pinnedBlockNumber
    , lsoiExecutionPath = 2
    , lsoiOperationalBlockerMask = 0
    , lsoiWarningMask = 0
    , lsoiDependencyFailureMask = 0
    , lsoiCriticalFaultMask = 0
    , lsoiSchemaVersion = supportedObservationSchemaVersion
    , lsoiHealthState = 1
    , lsoiExecutionPathDependencyMask = 0
    , lsoiStatusDependencyFailureMask = 0
    , lsoiHealthDependencyFailureMask = 0
    , lsoiObservationComplete = True
    , lsoiHasMaturedWork = True
    , lsoiLpEpochSettlementPaused = False
    }

fixtureIntent :: SignedTransaction -> LpSettlementSignedIntent
fixtureIntent signed =
  LpSettlementSignedIntent
    { lssiChainId = fixtureChainId
    , lssiMonitorAddress = monitor
    , lssiObservationDigest = fixtureDigest
    , lssiEpoch = fixtureEpoch
    , lssiSignerAddress = fixtureSigner
    , lssiNonce = 9
    , lssiTargetAddress = housePool
    , lssiValue = 0
    , lssiCalldata = settleLpEpochPoolCall cachedMark cachedMarkTime
    , lssiGasLimit = 125_000
    , lssiMaxPriorityFeePerGas = 1_000_000_000
    , lssiMaxFeePerGas = 2_000_000_000
    , lssiSignedRawTransaction = signedRawTransaction signed
    , lssiSignedTransactionHash = signedTransactionHash signed
    }

workerConfig :: LpSettlementMode -> Config
workerConfig mode =
  Config
    { cfgRpcUrl = ""
    , cfgChainId = 11155111
    , cfgPort = 0
    , cfgCorsOrigins = []
    , cfgDeployments = []
    , cfgDatabaseUrl = Nothing
    , cfgIndexerStartBlock = 0
    , cfgPythBenchmarksUrl = ""
    , cfgPythHistoryUrl = ""
    , cfgPythHermesUrl = ""
    , cfgPythApiKey = Nothing
    , cfgPythBackfillDays = 0
    , cfgPythSampleIntervalSeconds = 60
    , cfgPythLatestMaxAgeSeconds = 10
    , cfgPythIngestionEnabled = False
    , cfgPerpsCandleWriteMode = PerpsCandleWritesOff
    , cfgPerpsCandleReadMode = PerpsCandleReadsLegacy
    , cfgPerpsCandleReadIntervals = []
    , cfgPerpsCandleShadowSampleBps = 0
    , cfgPerpsCandleStrictCoverage = True
    , cfgPerpsCandleLatenessSeconds = 120
    , cfgPerpsCandleFinalizationGraceSeconds = 15
    , cfgPerpsRpcUrl = ""
    , cfgPerpsChainId = fixtureChainId
    , cfgPerpsUsdc = usdc
    , cfgPerpsOrderRouter = router
    , cfgPerpsOrderLifecycleBook = Nothing
    , cfgPerpsCfdEngine = engine
    , cfgPerpsCfdEngineLens = lens
    , cfgPerpsCfdEngineSettlementSidecar = sidecar
    , cfgPerpsMarginClearinghouse = clearinghouse
    , cfgPerpsPletherOracle = oracle
    , cfgPerpsAccountLens = accountLens
    , cfgPerpsHousePool = housePool
    , cfgPerpsSettlementMonitorLens = monitor
    , cfgPerpsIndexerStartBlock = 0
    , cfgVaultHistoryHousePoolAddress = housePool
    , cfgVaultHistorySeniorVaultAddress = seniorVault
    , cfgVaultHistoryJuniorVaultAddress = juniorVault
    , cfgVaultHistoryDeploymentBlock = 0
    , cfgVaultHistoryRpcUrl = ""
    , cfgVaultHistoryConfirmations = 1
    , cfgInsightsCompetitionRules = july2026Competition
    , cfgInsightsCompetitionReleaseManifest = releaseManifest
    , cfgRegistrationConfig = Nothing
    , cfgAaConfig = Nothing
    , cfgFaucetGuardConfig = Nothing
    , cfgFaucetPrivateKey = Nothing
    , cfgKeeperPrivateKey = Nothing
    , cfgKeeperPollSeconds = 15
    , cfgKeeperMaxBatchSize = 20
    , cfgKeeperConfirmations = 1
    , cfgKeeperGasBufferBps = 2_000
    , cfgKeeperFeeBufferBps = 2_500
    , cfgLpSettlementMode = mode
    , cfgLpSettlementPrivateKey = Just fixturePrivateKey
    , cfgLpSettlementSeniorVault = seniorVault
    , cfgLpSettlementJuniorVault = juniorVault
    , cfgLpSettlementPollSeconds = 15
    , cfgLpSettlementMaxDrainTransactions = 4
    , cfgLpSettlementPendingReplacementSeconds = 60
    , cfgLpSettlementMaxReplacements = 3
    , cfgLpSettlementMaxTxCostWei = 1_000_000_000_000_000_000
    }

releaseManifest :: CompetitionReleaseManifest
releaseManifest =
  CompetitionReleaseManifest
    { crmReleaseId = "lp-worker-test"
    , crmChainId = fixtureChainId
    , crmUsdc = usdc
    , crmOrderRouter = router
    , crmMarginClearinghouse = clearinghouse
    , crmAccountLens = accountLens
    , crmCfdEngine = engine
    , crmCfdEngineLens = lens
    , crmSettlementSidecar = sidecar
    , crmPletherOracle = oracle
    , crmIndexerStartBlock = 0
    }

fixtureChainId, fixtureEpoch, chainHead, pinnedBlockNumber, cachedMark, cachedMarkTime :: Integer
fixtureChainId = 421614
fixtureEpoch = 500_000
chainHead = 1_000
pinnedBlockNumber = chainHead - 1
cachedMark = 100_000_000
cachedMarkTime = 1_800_000_000

fixturePrivateKey, fixtureSigner, fixtureDigest :: Text
fixturePrivateKey = "0x0000000000000000000000000000000000000000000000000000000000000001"
fixtureSigner = "0x7e5f4552091a69125d5dfcb7b8c2659029395bdf"
fixtureDigest = hexText 64 "c"

fixtureCodeHashes :: SettlementCodeHashes
fixtureCodeHashes =
  SettlementCodeHashes
    { schMonitor = runtimeCodeHash 1
    , schRouter = runtimeCodeHash 2
    , schEngine = runtimeCodeHash 3
    , schHousePool = runtimeCodeHash 4
    , schSeniorVault = runtimeCodeHash 5
    , schJuniorVault = runtimeCodeHash 6
    , schPletherOracle = runtimeCodeHash 7
    }

fixtureContractCode :: Text -> Maybe BS.ByteString
fixtureContractCode target
  | normalized == normalize monitor = Just $ runtimeCode 1
  | normalized == normalize router = Just $ runtimeCode 2
  | normalized == normalize engine = Just $ runtimeCode 3
  | normalized == normalize housePool = Just $ runtimeCode 4
  | normalized == normalize seniorVault = Just $ runtimeCode 5
  | normalized == normalize juniorVault = Just $ runtimeCode 6
  | normalized == normalize oracle = Just $ runtimeCode 7
  | otherwise = Nothing
 where
  normalized = normalize target

runtimeCodeHash :: Word8 -> Text
runtimeCodeHash = bytesHex . keccak256 . runtimeCode

runtimeCode :: Word8 -> BS.ByteString
runtimeCode tag = BS.pack [0x60, tag, 0x60, 0x00, 0xf3]

usdc, router, engine, lens, sidecar, clearinghouse, oracle, accountLens :: Text
housePool, monitor, seniorVault, juniorVault :: Text
usdc = "0x1111111111111111111111111111111111111111"
router = "0x2222222222222222222222222222222222222222"
engine = "0x3333333333333333333333333333333333333333"
lens = "0x4444444444444444444444444444444444444444"
sidecar = "0x5555555555555555555555555555555555555555"
clearinghouse = "0x6666666666666666666666666666666666666666"
oracle = "0x7777777777777777777777777777777777777777"
accountLens = "0x8888888888888888888888888888888888888888"
housePool = "0x9999999999999999999999999999999999999999"
monitor = "0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
seniorVault = "0xbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
juniorVault = "0xcccccccccccccccccccccccccccccccccccccccc"
