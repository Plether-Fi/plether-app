module Plether.LiquidationWorker
  ( LiquidationWorkerMode (..)
  , LiquidationWorkerConfig (..)
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

runLiquidationWorker :: LiquidationWorkerConfig -> DbPool -> EthClient -> LiquidationWorkerMode -> Bool -> IO ()
runLiquidationWorker cfg pool client mode dryRun =
  deriveAddress (lwcPrivateKey cfg) >>= \case
    Left err -> putStrLn $ "Invalid LIQUIDATION_KEEPER_PRIVATE_KEY: " <> T.unpack err
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
              then putStrLn "Another plether-liquidation-worker instance already holds the advisory lock"
              else do
                putStrLn $ "plether-liquidation-worker acquired advisory lock as " <> T.unpack workerAddress
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

indexNewCandidates :: LiquidationWorkerConfig -> Connection -> EthClient -> IO ()
indexNewCandidates cfg conn client = do
  latestResult <- ethBlockNumber client
  case latestResult of
    Left err -> putStrLn $ "liquidation candidate indexing skipped: " <> T.unpack (rpcErrorText err)
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
            Left err -> putStrLn $ "liquidation candidate indexing failed: " <> T.unpack (rpcErrorText err)
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
              unless (null discovered) $
                putStrLn $
                  "indexed "
                    <> show (length discovered)
                    <> " liquidation candidate openings through block "
                    <> show endBlock

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
          Nothing -> putStrLn "liquidation scan is waiting for a cached latest Pyth payload"
          Just payload ->
            case decodeCachedPythPayload payload of
              Left err -> putStrLn $ "latest cached Pyth payload could not be decoded: " <> T.unpack err
              Right (_, updateData) -> do
                feeResult <- Perps.getUpdateFee client (lwcPletherOracle cfg) updateData
                case feeResult of
                  Left err -> putStrLn $ "liquidation update-fee lookup failed: " <> T.unpack (rpcErrorText err)
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
      recordCandidateError cfg conn candidate $ "position read failed: " <> rpcErrorText err
      pure False
    Right 0 -> do
      confirmedPosition <- getConfirmedPositionSize cfg client account
      case confirmedPosition of
        Left err -> do
          recordCandidateError cfg conn candidate $
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
              recordCandidateError cfg conn candidate $ "liquidation simulation failed: " <> rpcErrorText err
              pure False
        Right estimatedGas -> do
          putStrLn $ "liquidation opportunity found for " <> T.unpack account
          if dryRun
            then do
              putStrLn $
                "dry-run: would submit liquidation with value "
                  <> show updateFee
                  <> " and estimated gas "
                  <> show estimatedGas
              markPerpsLiquidationCandidateChecked conn (lwcChainId cfg) (lwcCfdEngine cfg) account
              pure True
            else do
              prepared <-
                prepareLiquidationTransaction cfg client workerAddress estimatedGas updateFee callData
              case prepared of
                Left err -> do
                  recordCandidateError cfg conn candidate err
                  pure False
                Right (tx, signed) -> do
                  let rawTx = signedRawTransaction signed
                      txHash = signedTransactionHash signed
                  -- Persist the deterministic signed hash before broadcast. If the
                  -- RPC response is lost, the next iteration reconciles this nonce
                  -- instead of creating a transaction behind it.
                  persistPendingTransaction cfg conn workerAddress account tx signed
                  sendResult <- ethSendRawTransaction client rawTx
                  case sendResult of
                    Left err -> do
                      recordCandidateError cfg conn candidate $
                        "broadcast result uncertain for " <> txHash <> ": " <> rpcErrorText err
                      pure False
                    Right returnedHash
                      | normalizeAddress returnedHash /= normalizeAddress txHash -> do
                          recordCandidateError cfg conn candidate $
                            "RPC returned transaction hash " <> returnedHash <> " but signed hash is " <> txHash
                          pure False
                      | otherwise -> do
                          receiptResult <- waitForReceipt client txHash 60
                          case receiptResult of
                            Left err -> do
                              recordCandidateError cfg conn candidate err
                              pure False
                            Right receipt -> handleLiquidationReceipt cfg conn client candidate receipt

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
          recordCandidateError cfg conn candidate $
            "pending receipt lookup failed for " <> txHash <> ": " <> rpcErrorText err
        Right (Just receipt) -> do
          _ <- handleLiquidationReceipt cfg conn client candidate receipt
          pure ()
        Right Nothing
          | normalizeAddress pendingSender /= normalizeAddress workerAddress ->
              recordCandidateError cfg conn candidate $
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
      recordCandidateError cfg conn candidate $
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
      recordCandidateError cfg conn candidate $
        "pending raw transaction could not be decoded for " <> txHash <> ": " <> err
    Right rawTx -> do
      rebroadcastResult <- ethSendRawTransaction client rawTx
      case rebroadcastResult of
        Left err ->
          recordCandidateError cfg conn candidate $
            "waiting for pending transaction " <> txHash <> " after rebroadcast: " <> rpcErrorText err
        Right returnedHash
          | normalizeAddress returnedHash == normalizeAddress txHash ->
              putStrLn $
                "rebroadcast liquidation transaction "
                  <> T.unpack txHash
                  <> " at nonce "
                  <> show nonce
          | otherwise ->
              recordCandidateError cfg conn candidate $
                "rebroadcast returned transaction hash " <> returnedHash <> " but signed hash is " <> txHash

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
          recordCandidateError cfg conn candidate $
            "pending calldata could not be decoded for same-nonce replacement: " <> err
        Right callData -> do
          gasPriceResult <- ethGasPrice client
          priorityResult <- ethMaxPriorityFeePerGas client
          case gasPriceResult of
            Left err ->
              recordCandidateError cfg conn candidate $
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
                Left err -> recordCandidateError cfg conn candidate err
                Right signed -> do
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
                      recordCandidateError cfg conn candidate $
                        "same-nonce replacement broadcast is uncertain for "
                          <> signedTransactionHash signed
                          <> ": "
                          <> rpcErrorText err
                    Right returnedHash
                      | normalizeAddress returnedHash == normalizeAddress (signedTransactionHash signed) ->
                          putStrLn $
                            "replaced stale liquidation transaction at nonce "
                              <> show nonce
                              <> " with "
                              <> T.unpack returnedHash
                      | otherwise ->
                          recordCandidateError cfg conn candidate $
                            "replacement returned transaction hash "
                              <> returnedHash
                              <> " but signed hash is "
                              <> signedTransactionHash signed
    _ ->
      recordCandidateError cfg conn candidate $
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
      recordCandidateError cfg conn candidate $
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
      putStrLn $
        "resolved consumed liquidation nonce "
          <> show nonce
          <> " with no remaining position; last tracked hash "
          <> T.unpack txHash
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
      putStrLn $
        "resolved consumed liquidation nonce "
          <> show nonce
          <> "; retained open account "
          <> T.unpack (plcrAccount candidate)

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
      recordCandidateError cfg conn candidate $
        "could not verify confirmation depth for "
          <> receiptTxHash receipt
          <> ": "
          <> rpcErrorText err
      pure False
    Right latestBlock
      | latestBlock < receiptBlockNumber receipt + fromIntegral (lwcIndexerConfirmations cfg) -> do
          putStrLn $
            "waiting for liquidation receipt "
              <> T.unpack (receiptTxHash receipt)
              <> " to reach "
              <> show (lwcIndexerConfirmations cfg)
              <> " confirmations"
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
          recordCandidateError cfg conn candidate $
            "liquidation confirmed in "
              <> receiptTxHash receipt
              <> " but post-state verification failed: "
              <> rpcErrorText err
          pure False
        Right 0 -> do
          deletePerpsLiquidationCandidate
            conn
            (lwcChainId cfg)
            (lwcCfdEngine cfg)
            (plcrAccount candidate)
          putStrLn $
            "liquidated "
              <> T.unpack (plcrAccount candidate)
              <> " in "
              <> T.unpack (receiptTxHash receipt)
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
          putStrLn $
            "liquidated prior position in "
              <> T.unpack (receiptTxHash receipt)
              <> "; retained reopened account "
              <> T.unpack (plcrAccount candidate)
          pure True
  | receiptSucceeded receipt = do
      -- Keep the confirmed hash as a circuit breaker. A successful call without
      -- the engine event indicates a router/ABI invariant failure; automatically
      -- resubmitting would burn gas indefinitely.
      recordCandidateError cfg conn candidate $
        "confirmed in "
          <> receiptTxHash receipt
          <> " without matching CFD-engine PositionLiquidated event; manual intervention required"
      pure False
  | otherwise = do
      clearPerpsLiquidationCandidatePending
        conn
        (lwcChainId cfg)
        (lwcCfdEngine cfg)
        (plcrAccount candidate)
      recordCandidateError cfg conn candidate $
        "liquidation transaction reverted: " <> receiptTxHash receipt
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
  -> IO ()
recordCandidateError cfg conn candidate err = do
  recordPerpsLiquidationCandidateError
    conn
    (lwcChainId cfg)
    (lwcCfdEngine cfg)
    (plcrAccount candidate)
    err
  putStrLn $
    "liquidation attempt for "
      <> T.unpack (plcrAccount candidate)
      <> " failed: "
      <> T.unpack err

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
