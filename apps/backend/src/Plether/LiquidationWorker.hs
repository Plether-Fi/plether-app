module Plether.LiquidationWorker
  ( LiquidationWorkerMode (..)
  , LiquidationWorkerConfig (..)
  , loadLiquidationWorkerConfig
  , runLiquidationWorker
  , decodeCachedPythPayload
  , isLiquidationReceiptFor
  , isExpectedLiquidationSimulationRevert
  , liquidationIndexRange
  , sameNonceReplacementFees
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (forM_, unless, when)
import Data.Aeson (FromJSON, Result (..), Value, fromJSON)
import Data.ByteString (ByteString)
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
  , PythUpdatePayloadRow (..)
  , clearPerpsLiquidationCandidatePending
  , deletePerpsLiquidationCandidate
  , getLatestPythUpdatePayload
  , getPerpsLiquidationCandidates
  , getPerpsLiquidationLastIndexedBlock
  , getPendingPerpsLiquidationCandidate
  , markPerpsLiquidationCandidateChecked
  , recordPerpsLiquidationCandidateError
  , recordPerpsLiquidationCandidatePending
  , setPerpsLiquidationLastIndexedBlock
  , tryPerpsLiquidationLock
  , unlockPerpsLiquidationLock
  , upsertPerpsLiquidationCandidate
  )
import Plether.Ethereum.Client (EthClient, RpcError (..), ethBlockNumber)
import qualified Plether.Ethereum.Contracts.Perps as Perps
import Plether.Ethereum.Rpc
  ( RpcLog (..)
  , TxReceipt (..)
  , ethEstimateGas
  , ethGasPrice
  , ethGetLogs
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
  case pending of
    Just candidate -> reconcilePendingCandidate cfg conn client workerAddress candidate
    Nothing -> do
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
                feeResult <- Perps.getUpdateFee client (lwcPletherOracle cfg) updateData
                case feeResult of
                  Left err ->
                    logWarnEvery
                      60
                      "liquidation_update_fee_fetch_failed"
                      "Liquidation worker could not fetch the Pyth update fee"
                      ( workerLogFields cfg
                          <> [ field "candidate_count" $ length candidates
                             , field "error" $ rpcErrorText err
                             ]
                      )
                  Right updateFee ->
                    processCandidateBatch candidates updateData updateFee
  where
    processCandidateBatch [] _ _ = pure ()
    processCandidateBatch (candidate : rest) updateData updateFee = do
      canContinue <-
        processCandidate cfg conn client workerAddress dryRun updateData updateFee candidate
      when canContinue $ processCandidateBatch rest updateData updateFee

processCandidate
  :: LiquidationWorkerConfig
  -> Connection
  -> EthClient
  -> Text
  -> Bool
  -> [ByteString]
  -> Integer
  -> PerpsLiquidationCandidateRow
  -> IO Bool
processCandidate cfg conn client workerAddress dryRun updateData updateFee candidate = do
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
                  sendResult <- ethSendRawTransaction client rawTx
                  case sendResult of
                    Left err -> do
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
      | plcrPendingStale candidate ->
          replacePendingTransaction cfg conn client pendingSender candidate nonce
      | otherwise ->
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
      rebroadcastResult <- ethSendRawTransaction client rawTx
      case rebroadcastResult of
        Left err ->
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
  -> IO ()
replacePendingTransaction cfg conn client pendingSender candidate nonce =
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
                  sendResult <- ethSendRawTransaction client (signedRawTransaction signed)
                  case sendResult of
                    Left err ->
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
  RpcNodeError _ _ (Just revertData) ->
    let normalizedData = T.toLower revertData
     in any
          (`T.isInfixOf` normalizedData)
          [ "0x451cebb2" -- CfdEngine__PositionIsSolvent()
          , "0x4565ea0c" -- CfdEngine__NoPositionToLiquidate()
          ]
  _ -> False

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
