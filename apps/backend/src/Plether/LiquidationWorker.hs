module Plether.LiquidationWorker
  ( LiquidationWorkerMode (..)
  , LiquidationWorkerConfig (..)
  , loadLiquidationWorkerConfig
  , runLiquidationWorker
  , decodeCachedPythPayload
  , LiquidationPayloadCircuitDecision (..)
  , liquidationPayloadCircuitDecision
  , LiquidationSignerCircuitDecision (..)
  , liquidationSignerCircuitDecision
  , LiquidationPendingSignerAction (..)
  , liquidationPendingSignerAction
  , isInsufficientFundsRpcError
  , liquidationPayloadFingerprint
  , payloadGlobalSimulationRevertSelector
  , isLiquidationReceiptFor
  , isExpectedLiquidationSimulationRevert
  , liquidationIndexRange
  , sameNonceReplacementFees
  , checkLiveSignerBalance
  , transactionMaximumCost
  , canAffordTransaction
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (forM_, unless, when)
import Data.Aeson (FromJSON, Result (..), Value, fromJSON)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.PostgreSQL.Simple (Connection)
import Plether.Config (Config (..))
import Plether.Database (DbPool, withDb)
import Plether.Database.Schema
  ( PerpsLiquidationCandidateRow (..)
  , PerpsLiquidationRejectedPayloadRow (..)
  , PerpsLiquidationSignerRetryRow (..)
  , PythUpdatePayloadRow (..)
  , clearPerpsLiquidationCandidatePending
  , clearPerpsLiquidationRejectedPayload
  , clearPerpsLiquidationSignerRetry
  , deletePerpsLiquidationCandidate
  , getLatestPythUpdatePayload
  , getPerpsLiquidationCandidates
  , getPerpsLiquidationLastIndexedBlock
  , getPerpsLiquidationRejectedPayload
  , getPerpsLiquidationSignerRetry
  , getPendingPerpsLiquidationCandidate
  , markPerpsLiquidationCandidateChecked
  , recordPerpsLiquidationCandidateError
  , recordPerpsLiquidationCandidateBroadcastAttempt
  , recordPerpsLiquidationCandidatePending
  , recordPerpsLiquidationRejectedPayload
  , recordPerpsLiquidationSignerRetry
  , setPerpsLiquidationLastIndexedBlock
  , tryPerpsLiquidationLock
  , unlockPerpsLiquidationLock
  , upsertPerpsLiquidationCandidate
  )
import Plether.Ethereum.Abi (encodeUint256, keccak256)
import Plether.Ethereum.Client (EthClient, RpcError (..), ethBlockNumber)
import qualified Plether.Ethereum.Contracts.Perps as Perps
import Plether.Ethereum.Rpc
  ( RpcLog (..)
  , TxReceipt (..)
  , ethEstimateGas
  , ethGasPrice
  , ethGetLogs
  , ethGetBalance
  , ethGetTransactionCount
  , ethGetTransactionCountAtBlock
  , ethGetTransactionReceipt
  , ethMaxPriorityFeePerGas
  , ethSendRawTransaction
  )
import Plether.Ethereum.Transaction
  ( SignedTransaction (..)
  , Tx1559 (..)
  , deriveAddress
  , signTransaction
  )
import Plether.Logging
  ( LogField
  , field
  , logError
  , logErrorEvery
  , logInfo
  , logInfoEvery
  , logWarn
  , logWarnEvery
  )
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

data LiquidationWorkerMode
  = LiquidationWorkerLoop
  | LiquidationWorkerOnce
  deriving stock (Show, Eq)

data LiquidationPayloadCircuitDecision
  = ProcessLiquidationPayload
  | ClearRejectedLiquidationPayload
  | SuppressRejectedLiquidationPayload
  deriving stock (Show, Eq)

data LiquidationSignerCircuitDecision
  = SignerTransactionReady
  | RecheckSignerTransaction
  | SuppressSignerTransaction
  deriving stock (Show, Eq)

data LiquidationPendingSignerAction
  = ReplacePendingSignerTransaction
  | RebroadcastPendingSignerTransaction
  | WaitForPendingSignerTransaction
  deriving stock (Show, Eq)

data LiquidationWorkerConfig = LiquidationWorkerConfig
  { lwcChainId :: Integer
  , lwcOrderRouter :: Text
  , lwcPletherOracle :: Text
  , lwcCfdEngine :: Text
  , lwcPrivateKey :: Text
  , lwcPollSeconds :: Int
  , lwcScanBatchSize :: Int
  , lwcIndexerStartBlock :: Integer
  , lwcIndexerConfirmations :: Int
  , lwcIndexerBatchSize :: Integer
  , lwcIndexerOverlapBlocks :: Integer
  , lwcPendingReplacementSeconds :: Int
  , lwcGasBufferBps :: Integer
  , lwcFeeBufferBps :: Integer
  }
  deriving stock (Show)

loadLiquidationWorkerConfig :: Config -> Text -> IO LiquidationWorkerConfig
loadLiquidationWorkerConfig cfg privateKey = do
  pollSeconds <- readEnv "LIQUIDATION_WORKER_POLL_SECONDS" 1
  scanBatchSize <- readEnv "LIQUIDATION_WORKER_SCAN_BATCH_SIZE" 100
  indexerStartBlock <- readEnv "LIQUIDATION_WORKER_START_BLOCK" (cfgPerpsIndexerStartBlock cfg)
  indexerConfirmations <- readEnv "LIQUIDATION_WORKER_CONFIRMATIONS" 1
  indexerBatchSize <- readEnv "LIQUIDATION_WORKER_INDEX_BATCH_SIZE" 5_000
  indexerOverlapBlocks <- readEnv "LIQUIDATION_WORKER_REORG_OVERLAP_BLOCKS" 12
  pendingReplacementSeconds <- readEnv "LIQUIDATION_WORKER_PENDING_REPLACEMENT_SECONDS" 120
  gasBufferBps <- readEnv "LIQUIDATION_WORKER_GAS_BUFFER_BPS" (cfgKeeperGasBufferBps cfg)
  feeBufferBps <- readEnv "LIQUIDATION_WORKER_FEE_BUFFER_BPS" (cfgKeeperFeeBufferBps cfg)
  pure
    LiquidationWorkerConfig
      { lwcChainId = cfgPerpsChainId cfg
      , lwcOrderRouter = cfgPerpsOrderRouter cfg
      , lwcPletherOracle = cfgPerpsPletherOracle cfg
      , lwcCfdEngine = cfgPerpsCfdEngine cfg
      , lwcPrivateKey = privateKey
      , lwcPollSeconds = max 1 pollSeconds
      , lwcScanBatchSize = max 1 scanBatchSize
      , lwcIndexerStartBlock = max 0 indexerStartBlock
      , lwcIndexerConfirmations = max 0 indexerConfirmations
      , lwcIndexerBatchSize = max 1 indexerBatchSize
      , lwcIndexerOverlapBlocks = max 0 indexerOverlapBlocks
      , lwcPendingReplacementSeconds = max 1 pendingReplacementSeconds
      , lwcGasBufferBps = max 0 gasBufferBps
      , lwcFeeBufferBps = max 0 feeBufferBps
      }

readEnv :: (Read a) => String -> a -> IO a
readEnv name fallback = do
  value <- lookupEnv name
  pure $ fromMaybe fallback (value >>= readMaybe)

runLiquidationWorker :: LiquidationWorkerConfig -> DbPool -> EthClient -> LiquidationWorkerMode -> Bool -> IO ()
runLiquidationWorker cfg pool client mode dryRun =
  deriveAddress (lwcPrivateKey cfg) >>= \case
    Left err ->
      logError
        "liquidation_worker_signer_invalid"
        "Liquidation worker signer is invalid"
        (workerLogFields cfg <> [field "error" err])
    Right workerAddress ->
      withDb pool $ \conn ->
        bracket
          (tryPerpsLiquidationLock conn (lwcChainId cfg) (lwcCfdEngine cfg))
          (\acquired ->
              when acquired $
                unlockPerpsLiquidationLock conn (lwcChainId cfg) (lwcCfdEngine cfg)
          )
          $ \acquired ->
            if not acquired
              then
                logWarn
                  "liquidation_worker_lock_unavailable"
                  "Another liquidation worker instance already holds the advisory lock"
                  (workerLogFields cfg)
              else do
                logInfo
                  "liquidation_worker_lock_acquired"
                  "Liquidation worker acquired the advisory lock"
                  ( workerLogFields cfg
                      <> [ field "worker_address" workerAddress
                         , field "mode" $ show mode
                         , field "dry_run" dryRun
                         ]
                  )
                case mode of
                  LiquidationWorkerOnce -> runIteration cfg conn client workerAddress dryRun
                  LiquidationWorkerLoop -> loop conn workerAddress
  where
    loop conn workerAddress = do
      runIteration cfg conn client workerAddress dryRun
      threadDelay (lwcPollSeconds cfg * 1_000_000)
      loop conn workerAddress

runIteration :: LiquidationWorkerConfig -> Connection -> EthClient -> Text -> Bool -> IO ()
runIteration cfg conn client workerAddress dryRun = do
  indexNewCandidates cfg conn client
  processCandidates cfg conn client workerAddress dryRun
  logInfoEvery
    300
    "liquidation_worker_heartbeat"
    "Liquidation worker completed an iteration"
    (workerLogFields cfg <> [field "dry_run" dryRun])

indexNewCandidates :: LiquidationWorkerConfig -> Connection -> EthClient -> IO ()
indexNewCandidates cfg conn client = do
  latestResult <- ethBlockNumber client
  case latestResult of
    Left err ->
      logWarnEvery
        60
        "liquidation_chain_head_fetch_failed"
        "Liquidation worker could not fetch the chain head"
        (workerLogFields cfg <> [field "error" $ rpcErrorText err])
    Right latestBlock -> do
      lastIndexed <- getPerpsLiquidationLastIndexedBlock conn (lwcChainId cfg) (lwcCfdEngine cfg)
      let indexRange =
            liquidationIndexRange
              (lwcIndexerStartBlock cfg)
              (lwcIndexerConfirmations cfg)
              (lwcIndexerBatchSize cfg)
              (lwcIndexerOverlapBlocks cfg)
              lastIndexed
              latestBlock
      case indexRange of
        Nothing -> pure ()
        Just (startBlock, endBlock) -> do
          logsResult <-
            ethGetLogs
              client
              (lwcCfdEngine cfg)
              [Perps.positionOpenedTopic]
              startBlock
              endBlock
          case logsResult of
            Left err ->
              logWarnEvery
                60
                "liquidation_candidate_logs_fetch_failed"
                "Liquidation worker could not fetch position-opening logs"
                ( workerLogFields cfg
                    <> [ field "from_block" startBlock
                       , field "to_block" endBlock
                       , field "error" $ rpcErrorText err
                       ]
                )
            Right logs -> do
              let discovered = mapMaybePositionOpened logs
              forM_ discovered $ \(account, blockNumber) ->
                upsertPerpsLiquidationCandidate
                  conn
                  (lwcChainId cfg)
                  (lwcCfdEngine cfg)
                  account
                  blockNumber
              setPerpsLiquidationLastIndexedBlock conn (lwcChainId cfg) (lwcCfdEngine cfg) endBlock
              unless (null logs) $
                logInfoEvery
                  300
                  "liquidation_candidates_indexed"
                  "Liquidation worker indexed a position-opening log batch"
                  ( workerLogFields cfg
                      <> [ field "from_block" startBlock
                         , field "to_block" endBlock
                         , field "event_count" $ length logs
                         , field "candidate_count" $ length discovered
                         ]
                  )

liquidationIndexRange
  :: Integer -- configured start block
  -> Int -- confirmations
  -> Integer -- maximum batch span
  -> Integer -- reorg overlap
  -> Integer -- last indexed block
  -> Integer -- latest chain block
  -> Maybe (Integer, Integer)
liquidationIndexRange configuredStart confirmations batchSize overlapBlocks lastIndexed latestBlock =
  if startBlock > confirmedLatest
    then Nothing
    else Just (startBlock, min confirmedLatest $ startBlock + safeBatchSize - 1)
  where
    safeStart = max 0 configuredStart
    safeBatchSize = max 1 batchSize
    safeOverlap = min (max 0 overlapBlocks) (safeBatchSize - 1)
    confirmedLatest = max 0 $ latestBlock - fromIntegral (max 0 confirmations)
    startBlock
      | lastIndexed < safeStart = safeStart
      | otherwise = max safeStart (lastIndexed + 1 - safeOverlap)

processCandidates :: LiquidationWorkerConfig -> Connection -> EthClient -> Text -> Bool -> IO ()
processCandidates cfg conn client workerAddress dryRun = do
  pending <-
    getPendingPerpsLiquidationCandidate
      conn
      (lwcChainId cfg)
      (lwcCfdEngine cfg)
      (lwcPendingReplacementSeconds cfg)
      pendingBroadcastRetrySeconds
  case pending of
    Just candidate -> reconcilePendingCandidate cfg conn client workerAddress candidate
    Nothing -> do
      signerReady <-
        if dryRun
          then pure True
          else checkSignerTransactionReadiness cfg conn
      when signerReady processAvailableCandidates
  where
    processAvailableCandidates = do
      candidates <-
        getPerpsLiquidationCandidates
          conn
          (lwcChainId cfg)
          (lwcCfdEngine cfg)
          (lwcScanBatchSize cfg)
      unless (null candidates) $ do
        mPayload <- getLatestPythUpdatePayload conn
        case mPayload of
          Nothing ->
            logWarnEvery
              60
              "liquidation_pyth_payload_missing"
              "Liquidation scan is waiting for a cached latest Pyth payload"
              (workerLogFields cfg <> [field "candidate_count" $ length candidates])
          Just payload ->
            case decodeCachedPythPayload payload of
              Left err ->
                logErrorEvery
                  60
                  "liquidation_pyth_payload_invalid"
                  "Latest cached Pyth payload could not be decoded"
                  (workerLogFields cfg <> [field "error" err])
              Right (_, updateData) -> do
                let payloadKey =
                      liquidationPayloadFingerprint
                        (lwcPletherOracle cfg)
                        (lwcOrderRouter cfg)
                        updateData
                rejectedPayload <-
                  getPerpsLiquidationRejectedPayload
                    conn
                    (lwcChainId cfg)
                    (lwcCfdEngine cfg)
                case
                    liquidationPayloadCircuitDecision
                      (plrprPayloadKey <$> rejectedPayload)
                      payloadKey
                  of
                  SuppressRejectedLiquidationPayload ->
                    case rejectedPayload of
                      Just rejected ->
                        logWarnEvery
                          60
                          "liquidation_pyth_payload_suppressed"
                          "Liquidation scan is waiting for a new Pyth payload after a deterministic oracle rejection"
                          ( workerLogFields cfg
                              <> [ field "candidate_count" $ length candidates
                                 , field "payload_key" payloadKey
                                 , field "revert_selector" $ plrprSelector rejected
                                 , field "rejected_at" $ plrprRejectedAt rejected
                                 , field "error" $ plrprError rejected
                                 ]
                          )
                      Nothing -> processPayload candidates payloadKey updateData
                  ClearRejectedLiquidationPayload -> do
                    clearPerpsLiquidationRejectedPayload
                      conn
                      (lwcChainId cfg)
                      (lwcCfdEngine cfg)
                    logInfo
                      "liquidation_pyth_payload_changed"
                      "Liquidation scan resumed with a new Pyth payload"
                      (workerLogFields cfg <> [field "payload_key" payloadKey])
                    processPayload candidates payloadKey updateData
                  ProcessLiquidationPayload ->
                    processPayload candidates payloadKey updateData

    processPayload candidates payloadKey updateData = do
      feeResult <- Perps.getUpdateFee client (lwcPletherOracle cfg) updateData
      case feeResult of
        Left err ->
          logWarnEvery
            60
            "liquidation_update_fee_fetch_failed"
            "Liquidation worker could not fetch the Pyth update fee"
            ( workerLogFields cfg
                <> [ field "candidate_count" $ length candidates
                   , field "payload_key" payloadKey
                   , field "error" $ rpcErrorText err
                   ]
            )
        Right updateFee ->
          processCandidateBatch candidates payloadKey updateData updateFee

    processCandidateBatch [] _ _ _ = pure ()
    processCandidateBatch (candidate : rest) payloadKey updateData updateFee = do
      canContinue <-
        processCandidate cfg conn client workerAddress dryRun payloadKey updateData updateFee candidate
      when canContinue $ processCandidateBatch rest payloadKey updateData updateFee

processCandidate
  :: LiquidationWorkerConfig
  -> Connection
  -> EthClient
  -> Text
  -> Bool
  -> Text
  -> [ByteString]
  -> Integer
  -> PerpsLiquidationCandidateRow
  -> IO Bool
processCandidate cfg conn client workerAddress dryRun payloadKey updateData updateFee candidate = do
  let account = plcrAccount candidate
  positionResult <- Perps.getPositionSize client (lwcCfdEngine cfg) account
  case positionResult of
    Left err -> do
      recordCandidateError cfg conn candidate "position_read" $ "position read failed: " <> rpcErrorText err
      pure False
    Right 0 -> do
      confirmedPosition <- getConfirmedPositionSize cfg client account
      case confirmedPosition of
        Left err -> do
          recordCandidateError cfg conn candidate "confirmed_position_read" $
            "confirmed position read failed: " <> rpcErrorText err
          pure False
        Right 0 -> do
          deletePerpsLiquidationCandidate conn (lwcChainId cfg) (lwcCfdEngine cfg) account
          pure True
        Right _ -> do
          -- A close/liquidation visible only at latest may still be reorged out.
          -- Keep the original opening candidate until zero size is confirmed.
          markPerpsLiquidationCandidateChecked conn (lwcChainId cfg) (lwcCfdEngine cfg) account
          pure True
    Right _ -> do
      let callData = Perps.executeLiquidationCall account updateData
      gasResult <- ethEstimateGas client workerAddress (lwcOrderRouter cfg) updateFee callData
      case gasResult of
        Left err
          | isExpectedLiquidationSimulationRevert err -> do
              -- A healthy position and a liquidation race both revert during simulation.
              -- The next sweep re-reads authoritative on-chain position state.
              markPerpsLiquidationCandidateChecked conn (lwcChainId cfg) (lwcCfdEngine cfg) account
              pure True
          | Just selectorText <- payloadGlobalSimulationRevertSelector err -> do
              let failure = "liquidation simulation rejected Pyth payload: " <> rpcErrorText err
              recordPerpsLiquidationRejectedPayload
                conn
                (lwcChainId cfg)
                (lwcCfdEngine cfg)
                payloadKey
                selectorText
                failure
              recordCandidateError cfg conn candidate "simulation" failure
              logError
                "liquidation_pyth_payload_rejected"
                "Liquidation worker suppressed a deterministic Pyth payload until the cache changes"
                ( candidateLogFields cfg candidate
                    <> [ field "payload_key" payloadKey
                       , field "revert_selector" selectorText
                       , field "error" failure
                       ]
                )
              pure False
          | otherwise -> do
              recordCandidateError cfg conn candidate "simulation" $ "liquidation simulation failed: " <> rpcErrorText err
              pure False
        Right estimatedGas -> do
          logInfo
            "liquidation_opportunity_detected"
            "Liquidation opportunity passed transaction simulation"
            ( candidateLogFields cfg candidate
                <> [ field "estimated_gas" estimatedGas
                   , field "update_fee_wei" $ show updateFee
                   , field "dry_run" dryRun
                   ]
            )
          if dryRun
            then do
              markPerpsLiquidationCandidateChecked conn (lwcChainId cfg) (lwcCfdEngine cfg) account
              pure True
            else do
              prepared <-
                prepareLiquidationTransaction cfg client workerAddress estimatedGas updateFee callData
              case prepared of
                Left err -> do
                  recordCandidateError cfg conn candidate "transaction_prepare" err
                  pure False
                Right (tx, signed) -> do
                  affordabilityResult <- checkTransactionAffordability client workerAddress tx
                  case affordabilityResult of
                    Left err -> do
                      recordSignerTransactionRetry cfg conn tx err
                      recordCandidateError cfg conn candidate "transaction_affordability" err
                      pure False
                    Right _ -> do
                      let rawTx = signedRawTransaction signed
                          txHash = signedTransactionHash signed
                          pendingCandidate =
                            candidate
                              { plcrAttemptCount = plcrAttemptCount candidate + 1
                              , plcrPendingTxHash = Just txHash
                              , plcrPendingNonce = Just $ txNonce tx
                              }
                      -- Persist the deterministic signed hash before broadcast. If the
                      -- RPC response is lost, the next iteration reconciles this nonce
                      -- instead of creating a transaction behind it.
                      persistPendingTransaction cfg conn workerAddress account tx signed
                      recordPendingBroadcastAttempt cfg conn account
                      sendResult <- ethSendRawTransaction client rawTx
                      case sendResult of
                        Left err -> do
                          when (isInsufficientFundsRpcError err) $
                            recordSignerTransactionRetry cfg conn tx (rpcErrorText err)
                          recordCandidateError cfg conn pendingCandidate "transaction_broadcast" $
                            "broadcast result uncertain for " <> txHash <> ": " <> rpcErrorText err
                          pure False
                        Right returnedHash
                          | normalizeAddress returnedHash /= normalizeAddress txHash -> do
                              recordCandidateErrorWith
                                cfg
                                conn
                                pendingCandidate
                                "broadcast_hash_mismatch"
                                [field "returned_transaction_hash" returnedHash]
                                "RPC returned a transaction hash that did not match the signed transaction hash"
                              pure False
                          | otherwise -> do
                              logInfo
                                "liquidation_transaction_submitted"
                                "Liquidation transaction was submitted"
                                ( candidateLogFields cfg pendingCandidate
                                    <> [ field "transaction_hash" txHash
                                       , field "nonce" $ txNonce tx
                                       , field "gas_limit" $ txGasLimit tx
                                       , field "value_wei" $ show $ txValue tx
                                       , field "max_priority_fee_per_gas_wei" $ show $ txMaxPriorityFeePerGas tx
                                       , field "max_fee_per_gas_wei" $ show $ txMaxFeePerGas tx
                                       ]
                                )
                              receiptResult <- waitForReceipt client txHash 60
                              case receiptResult of
                                Left err -> do
                                  recordCandidateError cfg conn pendingCandidate "receipt_wait" err
                                  pure False
                                Right receipt -> handleLiquidationReceipt cfg conn client pendingCandidate receipt

prepareLiquidationTransaction
  :: LiquidationWorkerConfig
  -> EthClient
  -> Text
  -> Integer
  -> Integer
  -> ByteString
  -> IO (Either Text (Tx1559, SignedTransaction))
prepareLiquidationTransaction cfg client workerAddress estimatedGas value callData = do
  nonceResult <- ethGetTransactionCount client workerAddress
  gasPriceResult <- ethGasPrice client
  priorityResult <- ethMaxPriorityFeePerGas client
  case (nonceResult, gasPriceResult) of
    (Right nonce, Right gasPrice) -> do
      let priorityBase = either (const gasPrice) id priorityResult
          maxFeeBase = max gasPrice priorityBase
          gasLimit = max 21_000 $ applyBuffer estimatedGas (lwcGasBufferBps cfg)
          maxPriorityFee = applyBuffer priorityBase (lwcFeeBufferBps cfg)
          maxFee = max maxPriorityFee $ applyBuffer maxFeeBase (lwcFeeBufferBps cfg)
          tx =
            Tx1559
              { txChainId = lwcChainId cfg
              , txNonce = nonce
              , txMaxPriorityFeePerGas = maxPriorityFee
              , txMaxFeePerGas = maxFee
              , txGasLimit = gasLimit
              , txTo = lwcOrderRouter cfg
              , txValue = value
              , txData = callData
              }
      signResult <- signTransaction (lwcPrivateKey cfg) tx
      pure $ fmap (\signed -> (tx, signed)) signResult
    _ ->
      pure $
        Left $
          T.intercalate
            "; "
            [ rpcErrorText err
            | Left err <- [nonceResult, gasPriceResult]
            ]

persistPendingTransaction
  :: LiquidationWorkerConfig
  -> Connection
  -> Text
  -> Text
  -> Tx1559
  -> SignedTransaction
  -> IO ()
persistPendingTransaction cfg conn sender account tx signed =
  recordPerpsLiquidationCandidatePending
    conn
    (lwcChainId cfg)
    (lwcCfdEngine cfg)
    account
    (txNonce tx)
    sender
    (signedTransactionHash signed)
    (encodeHex $ signedRawTransaction signed)
    (encodeHex $ txData tx)
    (txValue tx)
    (txGasLimit tx)
    (txMaxPriorityFeePerGas tx)
    (txMaxFeePerGas tx)

recordPendingBroadcastAttempt :: LiquidationWorkerConfig -> Connection -> Text -> IO ()
recordPendingBroadcastAttempt cfg conn account =
  recordPerpsLiquidationCandidateBroadcastAttempt
    conn
    (lwcChainId cfg)
    (lwcCfdEngine cfg)
    account

reconcilePendingCandidate
  :: LiquidationWorkerConfig
  -> Connection
  -> EthClient
  -> Text
  -> PerpsLiquidationCandidateRow
  -> IO ()
reconcilePendingCandidate cfg conn client workerAddress candidate =
  case
      ( plcrPendingTxHash candidate
      , plcrPendingNonce candidate
      , plcrPendingSender candidate
      , plcrPendingRawTx candidate
      )
    of
    (Just txHash, Just nonce, Just pendingSender, Just rawTxHex) -> do
      receiptResult <- ethGetTransactionReceipt client txHash
      case receiptResult of
        Left err ->
          recordCandidateError cfg conn candidate "pending_receipt_lookup" $
            "pending receipt lookup failed for " <> txHash <> ": " <> rpcErrorText err
        Right (Just receipt) -> do
          _ <- handleLiquidationReceipt cfg conn client candidate receipt
          pure ()
        Right Nothing
          | normalizeAddress pendingSender /= normalizeAddress workerAddress ->
              recordCandidateCritical cfg conn candidate "liquidation_signer_mismatch" $
                "pending liquidation was signed by "
                  <> pendingSender
                  <> " but the configured key resolves to "
                  <> workerAddress
                  <> "; refusing automatic rebroadcast or replacement until manually reconciled"
          | otherwise ->
              reconcileMissingReceipt cfg conn client pendingSender candidate nonce txHash rawTxHex
    _ ->
      -- Never clear a partially persisted pending transaction automatically: it
      -- may still be live on-chain, and doing so could create a second nonce lane.
      recordCandidateCritical cfg conn candidate "liquidation_pending_state_invalid" $
        "incomplete pending liquidation state requires manual reconciliation"

reconcileMissingReceipt
  :: LiquidationWorkerConfig
  -> Connection
  -> EthClient
  -> Text
  -> PerpsLiquidationCandidateRow
  -> Integer
  -> Text
  -> Text
  -> IO ()
reconcileMissingReceipt cfg conn client pendingSender candidate nonce txHash rawTxHex = do
  latestResult <- ethBlockNumber client
  confirmedNonceResult <-
    case latestResult of
      Left err -> pure $ Left err
      Right latestBlock ->
        ethGetTransactionCountAtBlock
          client
          pendingSender
          (max 0 $ latestBlock - fromIntegral (lwcIndexerConfirmations cfg))
  case confirmedNonceResult of
    Right confirmedNonce
      | confirmedNonce > nonce ->
          resolveConsumedPendingNonce cfg conn client candidate txHash nonce
    _
      | plcrPendingStale candidate -> do
          signerReady <- checkSignerTransactionReadiness cfg conn
          case liquidationPendingSignerAction signerReady (plcrPendingBroadcastDue candidate) of
            ReplacePendingSignerTransaction ->
              replacePendingTransaction cfg conn client pendingSender candidate nonce txHash rawTxHex
            RebroadcastPendingSignerTransaction ->
              rebroadcastPendingTransaction cfg conn client candidate nonce txHash rawTxHex
            WaitForPendingSignerTransaction -> pure ()
      | otherwise ->
          when (plcrPendingBroadcastDue candidate) $
            rebroadcastPendingTransaction cfg conn client candidate nonce txHash rawTxHex

rebroadcastPendingTransaction
  :: LiquidationWorkerConfig
  -> Connection
  -> EthClient
  -> PerpsLiquidationCandidateRow
  -> Integer
  -> Text
  -> Text
  -> IO ()
rebroadcastPendingTransaction cfg conn client candidate nonce txHash rawTxHex =
  case decodeHexUpdate rawTxHex of
    Left err ->
      recordCandidateCritical cfg conn candidate "liquidation_pending_transaction_invalid" $
        "pending raw transaction could not be decoded for " <> txHash <> ": " <> err
    Right rawTx -> do
      recordPendingBroadcastAttempt cfg conn (plcrAccount candidate)
      rebroadcastResult <- ethSendRawTransaction client rawTx
      case rebroadcastResult of
        Left err -> do
          when (isInsufficientFundsRpcError err) $
            forM_ (pendingCandidateMaximumCost candidate) $ \requiredBalance ->
              recordSignerReadinessFailure cfg conn requiredBalance (rpcErrorText err)
          recordCandidateError cfg conn candidate "transaction_rebroadcast" $
            "waiting for pending transaction " <> txHash <> " after rebroadcast: " <> rpcErrorText err
        Right returnedHash
          | normalizeAddress returnedHash == normalizeAddress txHash ->
              logInfoEvery
                60
                "liquidation_transaction_rebroadcast"
                "Liquidation worker rebroadcast the persisted transaction"
                ( candidateLogFields cfg candidate
                    <> [ field "transaction_hash" txHash
                       , field "nonce" nonce
                       ]
                )
          | otherwise ->
              recordCandidateErrorWith
                cfg
                conn
                candidate
                "rebroadcast_hash_mismatch"
                [field "returned_transaction_hash" returnedHash]
                "Rebroadcast RPC hash did not match the persisted transaction hash"

replacePendingTransaction
  :: LiquidationWorkerConfig
  -> Connection
  -> EthClient
  -> Text
  -> PerpsLiquidationCandidateRow
  -> Integer
  -> Text
  -> Text
  -> IO ()
replacePendingTransaction cfg conn client pendingSender candidate nonce txHash rawTxHex =
  case
      ( plcrPendingCallData candidate
      , plcrPendingValue candidate
      , plcrPendingGasLimit candidate
      , plcrPendingMaxPriorityFeePerGas candidate
      , plcrPendingMaxFeePerGas candidate
      )
    of
    (Just callDataHex, Just value, Just gasLimit, Just oldPriorityFee, Just oldMaxFee) ->
      case decodeHexUpdate callDataHex of
        Left err ->
          recordCandidateCritical cfg conn candidate "liquidation_replacement_state_invalid" $
            "pending calldata could not be decoded for same-nonce replacement: " <> err
        Right callData -> do
          gasPriceResult <- ethGasPrice client
          priorityResult <- ethMaxPriorityFeePerGas client
          case gasPriceResult of
            Left err ->
              recordCandidateError cfg conn candidate "replacement_fee_quote" $
                "could not price same-nonce replacement: " <> rpcErrorText err
            Right gasPrice -> do
              let priorityBase = either (const gasPrice) id priorityResult
                  (replacementPriorityFee, replacementMaxFee) =
                    sameNonceReplacementFees
                      (lwcFeeBufferBps cfg)
                      gasPrice
                      priorityBase
                      oldPriorityFee
                      oldMaxFee
                  replacementTx =
                    Tx1559
                      { txChainId = lwcChainId cfg
                      , txNonce = nonce
                      , txMaxPriorityFeePerGas = replacementPriorityFee
                      , txMaxFeePerGas = replacementMaxFee
                      , txGasLimit = gasLimit
                      , txTo = lwcOrderRouter cfg
                      , txValue = value
                      , txData = callData
                      }
              signResult <- signTransaction (lwcPrivateKey cfg) replacementTx
              case signResult of
                Left err -> recordCandidateError cfg conn candidate "replacement_sign" err
                Right signed -> do
                  affordabilityResult <- checkTransactionAffordability client pendingSender replacementTx
                  case affordabilityResult of
                    Left err -> do
                      recordSignerTransactionRetry cfg conn replacementTx err
                      recordCandidateError cfg conn candidate "replacement_affordability" err
                      when (plcrPendingBroadcastDue candidate) $
                        rebroadcastPendingTransaction cfg conn client candidate nonce txHash rawTxHex
                    Right _ -> do
                      let replacementHash = signedTransactionHash signed
                          replacementCandidate =
                            candidate
                              { plcrAttemptCount = plcrAttemptCount candidate + 1
                              , plcrPendingTxHash = Just replacementHash
                              , plcrPendingNonce = Just nonce
                              }
                      persistPendingTransaction
                        cfg
                        conn
                        pendingSender
                        (plcrAccount candidate)
                        replacementTx
                        signed
                      recordPendingBroadcastAttempt cfg conn (plcrAccount candidate)
                      sendResult <- ethSendRawTransaction client (signedRawTransaction signed)
                      case sendResult of
                        Left err -> do
                          when (isInsufficientFundsRpcError err) $
                            recordSignerTransactionRetry cfg conn replacementTx (rpcErrorText err)
                          recordCandidateError cfg conn replacementCandidate "replacement_broadcast" $
                            "same-nonce replacement broadcast is uncertain for "
                              <> replacementHash
                              <> ": "
                              <> rpcErrorText err
                        Right returnedHash
                          | normalizeAddress returnedHash == normalizeAddress replacementHash ->
                              logWarn
                                "liquidation_transaction_replaced"
                                "Liquidation worker replaced a stale transaction at the same nonce"
                                ( candidateLogFields cfg replacementCandidate
                                    <> maybe
                                      []
                                      (\previousHash -> [field "previous_transaction_hash" previousHash])
                                      (plcrPendingTxHash candidate)
                                    <> [ field "transaction_hash" returnedHash
                                       , field "nonce" nonce
                                       , field "max_priority_fee_per_gas_wei" $ show replacementPriorityFee
                                       , field "max_fee_per_gas_wei" $ show replacementMaxFee
                                       ]
                                )
                          | otherwise ->
                              recordCandidateErrorWith
                                cfg
                                conn
                                replacementCandidate
                                "replacement_hash_mismatch"
                                [field "returned_transaction_hash" returnedHash]
                                "Replacement RPC hash did not match the signed transaction hash"
    _ ->
      recordCandidateCritical cfg conn candidate "liquidation_replacement_state_incomplete" $
        "pending liquidation lacks fee or calldata fields required for same-nonce replacement"

resolveConsumedPendingNonce
  :: LiquidationWorkerConfig
  -> Connection
  -> EthClient
  -> PerpsLiquidationCandidateRow
  -> Text
  -> Integer
  -> IO ()
resolveConsumedPendingNonce cfg conn client candidate txHash nonce = do
  confirmedPosition <- getConfirmedPositionSize cfg client (plcrAccount candidate)
  case confirmedPosition of
    Left err ->
      recordCandidateError cfg conn candidate "consumed_nonce_position_read" $
        "nonce "
          <> T.pack (show nonce)
          <> " was consumed but confirmed position verification failed: "
          <> rpcErrorText err
    Right 0 -> do
      deletePerpsLiquidationCandidate
        conn
        (lwcChainId cfg)
        (lwcCfdEngine cfg)
        (plcrAccount candidate)
      logInfo
        "liquidation_nonce_reconciled"
        "Consumed liquidation nonce resolved with no remaining position"
        ( candidateLogFields cfg candidate
            <> [ field "transaction_hash" txHash
               , field "nonce" nonce
               , field "position_open" False
               ]
        )
    Right _ -> do
      clearPerpsLiquidationCandidatePending
        conn
        (lwcChainId cfg)
        (lwcCfdEngine cfg)
        (plcrAccount candidate)
      markPerpsLiquidationCandidateChecked
        conn
        (lwcChainId cfg)
        (lwcCfdEngine cfg)
        (plcrAccount candidate)
      logWarn
        "liquidation_nonce_reconciled"
        "Consumed liquidation nonce resolved while the account still has an open position"
        ( candidateLogFields cfg candidate
            <> [ field "transaction_hash" txHash
               , field "nonce" nonce
               , field "position_open" True
               ]
        )

handleLiquidationReceipt
  :: LiquidationWorkerConfig
  -> Connection
  -> EthClient
  -> PerpsLiquidationCandidateRow
  -> TxReceipt
  -> IO Bool
handleLiquidationReceipt cfg conn client candidate receipt = do
  latestResult <- ethBlockNumber client
  case latestResult of
    Left err -> do
      recordCandidateErrorWith
        cfg
        conn
        candidate
        "confirmation_depth_read"
        (receiptLogFields receipt)
        ("could not verify confirmation depth: " <> rpcErrorText err)
      pure False
    Right latestBlock
      | latestBlock < receiptBlockNumber receipt + fromIntegral (lwcIndexerConfirmations cfg) -> do
          logInfoEvery
            60
            "liquidation_receipt_confirmations_pending"
            "Liquidation receipt is waiting for confirmation depth"
            ( candidateLogFields cfg candidate
                <> [ field "transaction_hash" $ receiptTxHash receipt
                   , field "receipt_block_number" $ receiptBlockNumber receipt
                   , field "chain_head_block" latestBlock
                   , field "required_confirmations" $ lwcIndexerConfirmations cfg
                   ]
            )
          pure False
      | otherwise -> handleConfirmedLiquidationReceipt cfg conn client candidate receipt

handleConfirmedLiquidationReceipt
  :: LiquidationWorkerConfig
  -> Connection
  -> EthClient
  -> PerpsLiquidationCandidateRow
  -> TxReceipt
  -> IO Bool
handleConfirmedLiquidationReceipt cfg conn client candidate receipt
  | isLiquidationReceiptFor (lwcCfdEngine cfg) (plcrAccount candidate) receipt = do
      postReceiptPosition <- getConfirmedPositionSize cfg client (plcrAccount candidate)
      case postReceiptPosition of
        Left err -> do
          -- Keep the confirmed hash until post-state can be verified. This
          -- prevents deleting a newer PositionOpened that was indexed while the
          -- liquidation transaction was pending.
          recordCandidateErrorWith
            cfg
            conn
            candidate
            "post_receipt_position_read"
            (receiptLogFields receipt)
            ("liquidation post-state verification failed: " <> rpcErrorText err)
          pure False
        Right 0 -> do
          deletePerpsLiquidationCandidate
            conn
            (lwcChainId cfg)
            (lwcCfdEngine cfg)
            (plcrAccount candidate)
          logInfo
            "liquidation_confirmed"
            "Liquidation transaction confirmed and the position is closed"
            ( candidateLogFields cfg candidate
                <> [ field "transaction_hash" $ receiptTxHash receipt
                   , field "receipt_block_number" $ receiptBlockNumber receipt
                   , field "position_reopened" False
                   ]
            )
          pure True
        Right _ -> do
          clearPerpsLiquidationCandidatePending
            conn
            (lwcChainId cfg)
            (lwcCfdEngine cfg)
            (plcrAccount candidate)
          markPerpsLiquidationCandidateChecked
            conn
            (lwcChainId cfg)
            (lwcCfdEngine cfg)
            (plcrAccount candidate)
          logWarn
            "liquidation_confirmed_position_reopened"
            "Liquidation confirmed but the account has a newer open position"
            ( candidateLogFields cfg candidate
                <> [ field "transaction_hash" $ receiptTxHash receipt
                   , field "receipt_block_number" $ receiptBlockNumber receipt
                   , field "position_reopened" True
                   ]
            )
          pure True
  | receiptSucceeded receipt = do
      -- Keep the confirmed hash as a circuit breaker. A successful call without
      -- the engine event indicates a router/ABI invariant failure; automatically
      -- resubmitting would burn gas indefinitely.
      recordCandidateCriticalWith
        cfg
        conn
        candidate
        "liquidation_receipt_invariant_failed"
        (receiptLogFields receipt)
        "Confirmed transaction omitted the expected CFD-engine PositionLiquidated event"
      pure False
  | otherwise = do
      clearPerpsLiquidationCandidatePending
        conn
        (lwcChainId cfg)
        (lwcCfdEngine cfg)
        (plcrAccount candidate)
      let err = "liquidation transaction reverted: " <> receiptTxHash receipt
      persistCandidateError cfg conn candidate err
      logError
        "liquidation_transaction_reverted"
        "Liquidation transaction reverted on-chain"
        (candidateLogFields cfg candidate <> receiptLogFields receipt <> [field "error" err])
      pure True

getConfirmedPositionSize
  :: LiquidationWorkerConfig
  -> EthClient
  -> Text
  -> IO (Either RpcError Integer)
getConfirmedPositionSize cfg client account = do
  latestResult <- ethBlockNumber client
  case latestResult of
    Left err -> pure $ Left err
    Right latestBlock ->
      Perps.getPositionSizeAtBlock
        client
        (lwcCfdEngine cfg)
        account
        (max 0 $ latestBlock - fromIntegral (lwcIndexerConfirmations cfg))

recordCandidateError
  :: LiquidationWorkerConfig
  -> Connection
  -> PerpsLiquidationCandidateRow
  -> Text
  -> Text
  -> IO ()
recordCandidateError cfg conn candidate failureStage =
  recordCandidateErrorWith cfg conn candidate failureStage []

recordCandidateErrorWith
  :: LiquidationWorkerConfig
  -> Connection
  -> PerpsLiquidationCandidateRow
  -> Text
  -> [LogField]
  -> Text
  -> IO ()
recordCandidateErrorWith cfg conn candidate failureStage contextFields err = do
  persistCandidateError cfg conn candidate err
  logErrorEvery
    60
    ("liquidation_candidate_" <> failureStage <> "_failed")
    "Liquidation candidate processing failed"
    ( candidateLogFields cfg candidate
        <> contextFields
        <> [field "failure_stage" failureStage, field "error" err]
    )

recordCandidateCritical
  :: LiquidationWorkerConfig
  -> Connection
  -> PerpsLiquidationCandidateRow
  -> Text
  -> Text
  -> IO ()
recordCandidateCritical cfg conn candidate eventName =
  recordCandidateCriticalWith cfg conn candidate eventName []

recordCandidateCriticalWith
  :: LiquidationWorkerConfig
  -> Connection
  -> PerpsLiquidationCandidateRow
  -> Text
  -> [LogField]
  -> Text
  -> IO ()
recordCandidateCriticalWith cfg conn candidate eventName contextFields err = do
  persistCandidateError cfg conn candidate err
  logError
    eventName
    "Liquidation worker stopped automatic processing for a pending candidate"
    ( candidateLogFields cfg candidate
        <> contextFields
        <> [field "error" err, field "manual_intervention_required" True]
    )

persistCandidateError
  :: LiquidationWorkerConfig
  -> Connection
  -> PerpsLiquidationCandidateRow
  -> Text
  -> IO ()
persistCandidateError cfg conn candidate err =
  recordPerpsLiquidationCandidateError
    conn
    (lwcChainId cfg)
    (lwcCfdEngine cfg)
    (plcrAccount candidate)
    err

workerLogFields :: LiquidationWorkerConfig -> [LogField]
workerLogFields cfg =
  [ field "chain_id" $ lwcChainId cfg
  , field "order_router" $ lwcOrderRouter cfg
  , field "cfd_engine" $ lwcCfdEngine cfg
  ]

candidateLogFields :: LiquidationWorkerConfig -> PerpsLiquidationCandidateRow -> [LogField]
candidateLogFields cfg candidate =
  workerLogFields cfg
    <> [ field "account" $ plcrAccount candidate
       , field "attempt_count" $ plcrAttemptCount candidate
       ]
    <> maybe
      []
      (\transactionHash -> [field "pending_transaction_hash" transactionHash])
      (plcrPendingTxHash candidate)
    <> maybe [] (\nonce -> [field "pending_nonce" nonce]) (plcrPendingNonce candidate)

receiptLogFields :: TxReceipt -> [LogField]
receiptLogFields receipt =
  [ field "transaction_hash" $ receiptTxHash receipt
  , field "receipt_block_number" $ receiptBlockNumber receipt
  ]

waitForReceipt :: EthClient -> Text -> Int -> IO (Either Text TxReceipt)
waitForReceipt _ txHash 0 = pure $ Left $ "timed out waiting for receipt " <> txHash
waitForReceipt client txHash attempts = do
  receiptResult <- ethGetTransactionReceipt client txHash
  case receiptResult of
    Left err -> pure $ Left $ rpcErrorText err
    Right (Just receipt) -> pure $ Right receipt
    Right Nothing -> do
      threadDelay 2_000_000
      waitForReceipt client txHash (attempts - 1)

decodeCachedPythPayload :: PythUpdatePayloadRow -> Either Text ([Integer], [ByteString])
decodeCachedPythPayload PythUpdatePayloadRow {puprPublishTimes, puprUpdateData} = do
  publishTimes <- parseValue "publish_times" puprPublishTimes
  updateHex <- parseValue "update_data" puprUpdateData
  updateData <- traverse decodeHexUpdate updateHex
  pure (publishTimes, updateData)

parseValue :: (FromJSON a) => Text -> Value -> Either Text a
parseValue label value =
  case fromJSON value of
    Success decoded -> Right decoded
    Error err -> Left $ label <> " JSON decode failed: " <> T.pack err

decodeHexUpdate :: Text -> Either Text ByteString
decodeHexUpdate value =
  case B16.decode (TE.encodeUtf8 $ T.toLower $ strip0x value) of
    Right bytes -> Right bytes
    Left err -> Left $ "invalid updateData hex: " <> T.pack err

encodeHex :: ByteString -> Text
encodeHex value = "0x" <> TE.decodeUtf8 (B16.encode value)

isLiquidationReceiptFor :: Text -> Text -> TxReceipt -> Bool
isLiquidationReceiptFor cfdEngine account receipt =
  receiptSucceeded receipt
    && any
      ( \logEntry ->
          normalizeAddress (rpcLogAddress logEntry) == normalizedEngine
            && fmap normalizeAddress (Perps.decodePositionLiquidatedAccount logEntry) == Just normalizedAccount
      )
      (receiptLogs receipt)
  where
    normalizedEngine = normalizeAddress cfdEngine
    normalizedAccount = normalizeAddress account

isExpectedLiquidationSimulationRevert :: RpcError -> Bool
isExpectedLiquidationSimulationRevert = \case
  RpcNodeError _ message revertData ->
    let normalizedError = normalizedNodeError message revertData
     in any
          (`T.isInfixOf` normalizedError)
          [ "0x451cebb2" -- CfdEngine__PositionIsSolvent()
          , "0x4565ea0c" -- CfdEngine__NoPositionToLiquidate()
          ]
  _ -> False

payloadGlobalSimulationRevertSelector :: RpcError -> Maybe Text
payloadGlobalSimulationRevertSelector = \case
  RpcNodeError _ message revertData ->
    findKnownSelector $ normalizedNodeError message revertData
  _ -> Nothing
  where
    findKnownSelector revertData =
      case filter (`T.isInfixOf` revertData) payloadGlobalRevertSelectors of
        selectorText : _ -> Just selectorText
        [] -> Nothing

    payloadGlobalRevertSelectors =
      [ "0x2acbe915" -- InvalidWormholeVaa()
      , "0xf4a25e0f" -- PletherOracle__StalePrice()
      ]

normalizedNodeError :: Text -> Maybe Text -> Text
normalizedNodeError message revertData =
  T.toLower $ message <> maybe "" (" " <>) revertData

isInsufficientFundsRpcError :: RpcError -> Bool
isInsufficientFundsRpcError = \case
  RpcNodeError _ message errData ->
    let normalizedError = normalizedNodeError message errData
     in any
          (`T.isInfixOf` normalizedError)
          [ "insufficient funds"
          , "insufficient balance for transfer"
          ]
  _ -> False

liquidationPayloadFingerprint :: Text -> Text -> [ByteString] -> Text
liquidationPayloadFingerprint pletherOracle orderRouter updateData =
  encodeHex $
    keccak256 $
      framed "plether:liquidation-pyth-payload:v1"
        <> labelled "plether-oracle" (TE.encodeUtf8 $ normalizeAddress pletherOracle)
        <> labelled "order-router" (TE.encodeUtf8 $ normalizeAddress orderRouter)
        <> encodeUint256 (fromIntegral $ length updateData)
        <> mconcat (map framed updateData)
  where
    labelled label value = framed label <> framed value
    framed value = encodeUint256 (fromIntegral $ BS.length value) <> value

liquidationPayloadCircuitDecision :: Maybe Text -> Text -> LiquidationPayloadCircuitDecision
liquidationPayloadCircuitDecision maybeRejectedKey payloadKey =
  case normalizeAddress <$> maybeRejectedKey of
    Nothing -> ProcessLiquidationPayload
    Just rejectedKey
      | rejectedKey == normalizeAddress payloadKey -> SuppressRejectedLiquidationPayload
      | otherwise -> ClearRejectedLiquidationPayload

mapMaybePositionOpened :: [RpcLog] -> [(Text, Integer)]
mapMaybePositionOpened =
  foldr
    (\logEntry found -> case Perps.decodePositionOpenedAccount logEntry of
        Just account -> (account, rpcLogBlockNumber logEntry) : found
        Nothing -> found
    )
    []

applyBuffer :: Integer -> Integer -> Integer
applyBuffer value bufferBps =
  ((value * (10_000 + bufferBps)) + 9_999) `div` 10_000

sameNonceReplacementFees
  :: Integer -- current fee buffer bps
  -> Integer -- current gas price
  -> Integer -- current priority fee
  -> Integer -- previous priority fee
  -> Integer -- previous max fee
  -> (Integer, Integer)
sameNonceReplacementFees feeBufferBps gasPrice priorityBase oldPriorityFee oldMaxFee =
  (replacementPriorityFee, replacementMaxFee)
  where
    currentPriorityFee = applyBuffer priorityBase feeBufferBps
    currentMaxFee =
      max currentPriorityFee $
        applyBuffer (max gasPrice priorityBase) feeBufferBps
    replacementPriorityFee =
      max currentPriorityFee $ applyBuffer oldPriorityFee 1_250
    replacementMaxFee =
      max replacementPriorityFee $
        max currentMaxFee (applyBuffer oldMaxFee 1_250)

liquidationSignerCircuitDecision :: Maybe Bool -> LiquidationSignerCircuitDecision
liquidationSignerCircuitDecision = \case
  Nothing -> SignerTransactionReady
  Just True -> RecheckSignerTransaction
  Just False -> SuppressSignerTransaction

liquidationPendingSignerAction :: Bool -> Bool -> LiquidationPendingSignerAction
liquidationPendingSignerAction signerReady broadcastDue
  | signerReady = ReplacePendingSignerTransaction
  | broadcastDue = RebroadcastPendingSignerTransaction
  | otherwise = WaitForPendingSignerTransaction

-- Keep the cooldown in PostgreSQL so restarts and repeated --once invocations
-- cannot turn an unfunded signer into a simulation/replacement RPC storm.
signerTransactionRetrySeconds :: Int
signerTransactionRetrySeconds = 60

pendingBroadcastRetrySeconds :: Int
pendingBroadcastRetrySeconds = 60

checkSignerTransactionReadiness
  :: LiquidationWorkerConfig
  -> Connection
  -> IO Bool
checkSignerTransactionReadiness cfg conn = do
  retry <-
    getPerpsLiquidationSignerRetry
      conn
      (lwcChainId cfg)
      (lwcCfdEngine cfg)
      signerTransactionRetrySeconds
  case liquidationSignerCircuitDecision (plrsrRetryDue <$> retry) of
    SignerTransactionReady -> pure True
    SuppressSignerTransaction -> do
      forM_ retry $ \blocked ->
        logWarnEvery
          signerTransactionRetrySeconds
          "liquidation_signer_transaction_suppressed"
          "Liquidation worker is waiting before rechecking signer transaction affordability"
          ( signerRetryLogFields cfg blocked
              <> [field "retry_seconds" signerTransactionRetrySeconds]
          )
      pure False
    RecheckSignerTransaction -> do
      clearPerpsLiquidationSignerRetry
        conn
        (lwcChainId cfg)
        (lwcCfdEngine cfg)
      forM_ retry $ \blocked ->
        logInfo
          "liquidation_signer_transaction_retrying"
          "Liquidation worker is allowing one freshly priced signer transaction attempt"
          (signerRetryLogFields cfg blocked)
      pure True

recordSignerTransactionRetry :: LiquidationWorkerConfig -> Connection -> Tx1559 -> Text -> IO ()
recordSignerTransactionRetry cfg conn tx =
  recordSignerReadinessFailure cfg conn (transactionMaximumCost tx)

recordSignerReadinessFailure :: LiquidationWorkerConfig -> Connection -> Integer -> Text -> IO ()
recordSignerReadinessFailure cfg conn requiredBalance err = do
  recordPerpsLiquidationSignerRetry
    conn
    (lwcChainId cfg)
    (lwcCfdEngine cfg)
    requiredBalance
    err
  logErrorEvery
    signerTransactionRetrySeconds
    "liquidation_signer_transaction_unready"
    "Liquidation worker paused new signer transaction attempts"
    ( workerLogFields cfg
        <> [ field "required_balance_wei" $ show requiredBalance
           , field "retry_seconds" signerTransactionRetrySeconds
           , field "error" err
           ]
    )

signerRetryLogFields :: LiquidationWorkerConfig -> PerpsLiquidationSignerRetryRow -> [LogField]
signerRetryLogFields cfg retry =
  workerLogFields cfg
    <> [ field "required_balance_wei" $ maybe "unknown" (T.pack . show) $ plrsrRequiredBalance retry
       , field "retry_recorded_at" $ plrsrRecordedAt retry
       , field "error" $ plrsrError retry
       ]

-- | Skip live-balance readiness entirely in dry-run mode. In live mode, make
-- startup fail closed when the signer balance cannot be read or is zero.
checkLiveSignerBalance
  :: Bool
  -> IO (Either RpcError Integer)
  -> IO (Either Text (Maybe Integer))
checkLiveSignerBalance dryRun fetchBalance
  | dryRun = pure $ Right Nothing
  | otherwise =
      fetchBalance >>= \case
        Left err ->
          pure $ Left $ "could not read liquidation signer balance: " <> rpcErrorText err
        Right balance
          | balance <= 0 -> pure $ Left "liquidation signer has zero ETH balance"
          | otherwise -> pure $ Right $ Just balance

transactionMaximumCost :: Tx1559 -> Integer
transactionMaximumCost tx =
  txValue tx + txGasLimit tx * txMaxFeePerGas tx

pendingCandidateMaximumCost :: PerpsLiquidationCandidateRow -> Maybe Integer
pendingCandidateMaximumCost candidate = do
  value <- plcrPendingValue candidate
  gasLimit <- plcrPendingGasLimit candidate
  maxFee <- plcrPendingMaxFeePerGas candidate
  pure $ value + gasLimit * maxFee

canAffordTransaction :: Integer -> Tx1559 -> Bool
canAffordTransaction balance tx = balance >= transactionMaximumCost tx

checkTransactionAffordability :: EthClient -> Text -> Tx1559 -> IO (Either Text Integer)
checkTransactionAffordability client signer tx =
  ethGetBalance client signer >>= \case
    Left err ->
      pure $ Left $ "could not recheck liquidation signer balance: " <> rpcErrorText err
    Right balance
      | canAffordTransaction balance tx -> pure $ Right balance
      | otherwise ->
          pure $
            Left $
              "liquidation signer balance "
                <> T.pack (show balance)
                <> " wei is below the transaction maximum cost "
                <> T.pack (show $ transactionMaximumCost tx)
                <> " wei"

rpcErrorText :: RpcError -> Text
rpcErrorText = \case
  RpcHttpError err -> "RPC HTTP error: " <> err
  RpcJsonError err -> "RPC JSON error: " <> err
  RpcNodeError code message mData ->
    "RPC node error "
      <> T.pack (show code)
      <> ": "
      <> message
      <> maybe "" ("; data: " <>) mData

normalizeAddress :: Text -> Text
normalizeAddress = T.toLower . T.strip

strip0x :: Text -> Text
strip0x value =
  fromMaybe value $ T.stripPrefix "0x" value
