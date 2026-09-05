module Plether.Keeper
  ( KeeperMode (..)
  , KeeperIterationActivity (..)
  , LpSettlementDecision (..)
  , FreshPendingOrder (..)
  , LifecycleRefreshAction (..)
  , runKeeper
  , runKeeperWithCodeHashes
  , auditLpSettlementStartup
  , runLpSettlementPreflight
  , processLpSettlementCycle
  , processLpSettlementCycleWithCodeHashes
  , assessLpSettlementStatus
  , isLpSettlementObservationSafe
  , isLpSettlementObservationConsistent
  , validateAtomicSettlementPayload
  , validateLpSettlementCost
  , lpSettlementRequiredBalance
  , isOrderPastValidUntil
  , isOrderRevealReady
  , isFrozenClosePayloadReady
  , isSameBlockMevGuardError
  , selectBatchCandidates
  , nextV2GasLimit
  , assessLifecycleRefresh
  , V2PreflightAction (..)
  , assessSingleOrderPreflight
  , assessBatchOrderPreflight
  , keeperPollDelayMicros
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently_)
import Control.Exception
  ( SomeAsyncException
  , SomeException
  , displayException
  , fromException
  , throwIO
  , try
  )
import Control.Monad (foldM, forM_, unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Data.Aeson (FromJSON, Result (..), Value, fromJSON)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.Bits ((.|.))
import Data.List (nub, sortOn)
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import Database.PostgreSQL.Simple (Connection)
import Plether.Config (Config (..), LpSettlementMode (..), lpSettlementModeText)
import Plether.Database (DbPool, withDb)
import Plether.Database.Schema
  ( PerpsKeeperOrderRow (..)
  , LpSettlementBroadcastInput (..)
  , LpSettlementBroadcastRow (..)
  , LpSettlementEventOutcome (..)
  , LpSettlementObservationInput (..)
  , LpSettlementReceiptInput (..)
  , LpSettlementSignedIntent (..)
  , LpSettlementTransactionRow (..)
  , PythUpdatePayloadRow (..)
  , appendLpSettlementBroadcast
  , clearLpSettlementReorgedReceiptEvidence
  , getActiveLpSettlementTransaction
  , getLatestSuccessfulLpSettlementAt
  , getLpSettlementBroadcasts
  , getLpSettlementObservationObservedBlock
  , getLpSettlementTransactionFamily
  , getPendingPerpsKeeperOrders
  , getPerpsKeeperLastIndexedBlock
  , getLatestPythUpdatePayload
  , getLatestPythUpdatePayloadAtOrAfter
  , getPythUpdatePayloadForWindow
  , isHistoricalRevealPayload
  , markPerpsKeeperOrderExecuted
  , markPerpsKeeperOrderFailed
  , markLpSettlementTransactionConfirming
  , markLpSettlementTransactionManualReview
  , markLpSettlementTransactionPending
  , prepareLpSettlementTransaction
  , recordPerpsKeeperOrderAttempt
  , recordPerpsKeeperOrderError
  , recordPerpsKeeperOrderImmediateRetryError
  , reconcilePerpsKeeperOrderExecuted
  , reconcilePerpsKeeperOrderFailed
  , recordLpSettlementObservationV2
  , recordLpSettlementReceipt
  , recordLpSettlementReceiptForManualReview
  , recordLpSettlementSupersededReceipt
  , replaceLpSettlementTransaction
  , setPerpsKeeperLastIndexedBlock
  , tryPerpsKeeperLock
  , tryLpSettlementKeeperLock
  , unlockPerpsKeeperLock
  , unlockLpSettlementKeeperLock
  , upsertPerpsKeeperOrderCommitted
  , verifyLpSettlementSchema
  , verifyNoLegacySubmittedLpSettlementAttempts
  )
import Plether.Ethereum.Client
  ( CallParams (..)
  , EthClient
  , RpcError (..)
  , ethBlockNumber
  , ethCallWithTransactionGas
  )
import qualified Plether.Ethereum.Contracts.Perps as Perps
import qualified Plether.Ethereum.Contracts.SettlementMonitor as SettlementMonitor
import Plether.Ethereum.Rpc
  ( TxReceipt (..)
  , RpcBlock (..)
  , RpcLog (..)
  , ethBlockTimestamp
  , ethChainId
  , ethEstimateGas
  , ethEstimateGasAtBlock
  , ethGasPrice
  , ethGetBalance
  , ethGetBlockByNumber
  , ethGetLogs
  , ethGetLogsForAddresses
  , ethGetTransactionCount
  , ethGetTransactionCountAtBlock
  , ethGetTransactionReceipt
  , ethLatestBlockTimestamp
  , ethMaxPriorityFeePerGas
  , ethSendRawTransaction
  )
import Plether.Ethereum.Transaction
  ( SignedTransaction (..)
  , Tx1559 (..)
  , applyBpsBuffer
  , deriveAddress
  , rawTransactionHash
  , sameNonceReplacementFees
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
import Plether.Pyth.RevealPayload (validateRevealWindow)

data KeeperMode
  = KeeperLoop
  | KeeperOnce
  deriving stock (Show, Eq)

data KeeperIterationActivity
  = KeeperIdle
  | KeeperPending
  deriving stock (Show, Eq)

keeperPollDelayMicros :: Int -> Int -> KeeperIterationActivity -> Int
keeperPollDelayMicros activeSeconds idleSeconds activity =
  max 1 selectedSeconds * 1_000_000
 where
  selectedSeconds =
    case activity of
      KeeperIdle -> idleSeconds
      KeeperPending -> activeSeconds

data LpSettlementDecision
  = LpSettlementHeld
  | LpSettlementNoMaturedWork
  | LpSettlementDependenciesUnknown
  | LpSettlementOperationallyBlocked
  | LpSettlementReady SettlementMonitor.ExecutionPath
  deriving stock (Show, Eq)

data ExecutionIntent
  = CleanupExpired PerpsKeeperOrderRow
  | ExecuteReady [PerpsKeeperOrderRow] PythUpdatePayloadRow [Integer] [ByteString]

data FreshPendingOrder = FreshPendingOrder
  { fpoOrder :: PerpsKeeperOrderRow
  , fpoIsClose :: Bool
  , fpoValidUntil :: Integer
  }
  deriving stock (Show)

data LifecycleRefreshAction
  = RefreshPendingLifecycle
  | ReconcileTerminalLifecycle
  deriving stock (Show, Eq)

data PendingOrderRefresh
  = RefreshedPendingOrder FreshPendingOrder
  | RefreshedTerminalOrder Perps.OrderTerminalOutcome

data OrderCallKind
  = SingleOrderCall Integer
  | BatchOrderCall
  deriving stock (Show, Eq)

data V2PreflightResult
  = V2PreflightReady Integer
  | V2PreflightDeferred Text

data V2PreflightAction
  = V2PreflightSubmit
  | V2PreflightIncreaseGas
  | V2PreflightDefer Text
  | V2PreflightReject Text
  deriving stock (Show, Eq)

data LpPreparedWork = LpPreparedWork
  { lpwObservation :: SettlementMonitor.SettlementObservation
  , lpwObservedBlockHash :: Text
  , lpwSignerAddress :: Text
  , lpwSignerBalance :: Integer
  , lpwTarget :: Text
  , lpwValue :: Integer
  , lpwCallData :: ByteString
  , lpwTransaction :: Tx1559
  }

data LpReconcileOutcome
  = LpReconciledSuccess
  | LpReconciledSuperseded
  | LpReconcilePending
  | LpReconcileManualReview
  deriving stock (Eq, Show)

runKeeper :: Config -> DbPool -> EthClient -> KeeperMode -> Bool -> IO ()
runKeeper = runKeeperWithCodeHashes SettlementMonitor.reviewedSettlementCodeHashes

runKeeperWithCodeHashes
  :: SettlementMonitor.SettlementCodeHashes
  -> Config
  -> DbPool
  -> EthClient
  -> KeeperMode
  -> Bool
  -> IO ()
runKeeperWithCodeHashes codeHashes cfg pool client mode dryRun =
  case mode of
    KeeperOnce -> do
      runOrderKeeper cfg pool client KeeperOnce dryRun
      when lpSettlementActive $ runLpSettlementWorker codeHashes lpCfg pool client KeeperOnce
    KeeperLoop
      | lpSettlementActive ->
          concurrently_
            (runOrderKeeper cfg pool client KeeperLoop dryRun)
            (runLpSettlementWorker codeHashes lpCfg pool client KeeperLoop)
      | otherwise -> runOrderKeeper cfg pool client KeeperLoop dryRun
 where
  lpSettlementActive = cfgLpSettlementMode cfg /= LpSettlementOff
  lpCfg
    | dryRun && cfgLpSettlementMode cfg == LpSettlementExecute =
        cfg {cfgLpSettlementMode = LpSettlementObserve}
    | otherwise = cfg

runOrderKeeper :: Config -> DbPool -> EthClient -> KeeperMode -> Bool -> IO ()
runOrderKeeper cfg pool client KeeperOnce dryRun =
  runOrderKeeperSession cfg pool client KeeperOnce dryRun
runOrderKeeper cfg pool client KeeperLoop dryRun = supervise
 where
  supervise = do
    result <- trySynchronous $ runOrderKeeperSession cfg pool client KeeperLoop dryRun
    case result of
      Left (err :: SomeException) ->
        logError
          "order_keeper_worker_restarting"
          "Order keeper worker session failed; its dedicated database connection and advisory lock will be reacquired"
          [field "error" $ displayException err]
      Right () ->
        logWarn
          "order_keeper_worker_stopped"
          "Order keeper worker session stopped unexpectedly and will be restarted"
          []
    threadDelay (cfgKeeperPollSeconds cfg * 1_000_000)
    supervise

runOrderKeeperSession :: Config -> DbPool -> EthClient -> KeeperMode -> Bool -> IO ()
runOrderKeeperSession cfg pool client mode dryRun =
  withKeeperSessionLock pool tryPerpsKeeperLock unlockPerpsKeeperLock $ \conn acquired ->
        if not acquired
          then case mode of
            KeeperOnce ->
              logWarn
                "keeper_lock_unavailable"
                "Another keeper instance already holds the advisory lock"
                []
            KeeperLoop ->
              fail "Another order keeper already holds the advisory lock"
          else do
            logInfo
              "keeper_lock_acquired"
              "Order keeper acquired its advisory lock"
              []
            case mode of
              KeeperOnce -> void $ runKeeperIteration cfg conn client dryRun
              KeeperLoop -> loop conn
  where
    loop conn = do
      activity <- runKeeperIteration cfg conn client dryRun
      threadDelay $
        keeperPollDelayMicros
          (cfgKeeperPollSeconds cfg)
          (cfgKeeperIdlePollSeconds cfg)
          activity
      loop conn

runKeeperIteration :: Config -> Connection -> EthClient -> Bool -> IO KeeperIterationActivity
runKeeperIteration cfg conn client dryRun = do
  indexNewLogs cfg conn client
  processQueueHead cfg conn client dryRun

runLpSettlementWorker
  :: SettlementMonitor.SettlementCodeHashes
  -> Config
  -> DbPool
  -> EthClient
  -> KeeperMode
  -> IO ()
runLpSettlementWorker codeHashes cfg pool client KeeperOnce =
  runLpSettlementWorkerSession codeHashes cfg pool client KeeperOnce
runLpSettlementWorker codeHashes cfg pool client KeeperLoop = supervise
 where
  supervise = do
    result <- trySynchronous $ runLpSettlementWorkerSession codeHashes cfg pool client KeeperLoop
    case result of
      Left (err :: SomeException) ->
        logError
          "lp_settlement_worker_restarting"
          "LP settlement worker session failed; its dedicated database connection and advisory lock will be reacquired"
          [field "error" $ displayException err]
      Right () ->
        logWarn
          "lp_settlement_worker_stopped"
          "LP settlement worker session stopped unexpectedly and will be restarted"
          []
    threadDelay (cfgLpSettlementPollSeconds cfg * 1_000_000)
    supervise

runLpSettlementWorkerSession
  :: SettlementMonitor.SettlementCodeHashes
  -> Config
  -> DbPool
  -> EthClient
  -> KeeperMode
  -> IO ()
runLpSettlementWorkerSession codeHashes cfg pool client mode =
  withKeeperSessionLock pool tryLpSettlementKeeperLock unlockLpSettlementKeeperLock $ \conn acquired ->
        if not acquired
          then
            fail "Another LP settlement worker already holds the advisory lock"
          else do
            logInfo
              "lp_settlement_lock_acquired"
              "LP settlement worker acquired its independent advisory lock"
              []
            case mode of
              KeeperOnce -> void $ runLpSettlementIteration codeHashes cfg conn client
              KeeperLoop -> lpLoop conn
 where
  lpLoop conn = do
    continue <- runLpSettlementIteration codeHashes cfg conn client
    when continue $ do
      threadDelay (cfgLpSettlementPollSeconds cfg * 1_000_000)
      lpLoop conn

runLpSettlementIteration
  :: SettlementMonitor.SettlementCodeHashes
  -> Config
  -> Connection
  -> EthClient
  -> IO Bool
runLpSettlementIteration codeHashes cfg conn client = do
  processLpSettlementCycleWithCodeHashes codeHashes cfg conn client
  emitLpSettlementHeartbeat cfg conn client
  pure True

-- On failure (including cancellation), withDb destroys the connection, which
-- releases its session locks. Do not issue an unlock query on that path: an
-- interrupted libpq query can leave the connection busy, and an unlock error
-- would replace the cancellation and cause the supervisor to restart.
withKeeperSessionLock
  :: DbPool
  -> (Connection -> IO Bool)
  -> (Connection -> IO ())
  -> (Connection -> Bool -> IO a)
  -> IO a
withKeeperSessionLock pool acquire release action =
  withDb pool $ \conn -> do
    acquired <- acquire conn
    result <- action conn acquired
    when acquired $ release conn
    pure result

trySynchronous :: IO a -> IO (Either SomeException a)
trySynchronous action = do
  result <- try action
  case result of
    Left err
      | Just (_ :: SomeAsyncException) <- fromException err -> throwIO err
    _ -> pure result

runLpSettlementPreflight :: Config -> DbPool -> EthClient -> IO ()
runLpSettlementPreflight cfg pool client = do
  startupResult <- verifyLpSettlementStartup cfg client
  case startupResult of
    Left err -> preflightFailure "startup" err
    Right signerAddress -> do
      signerBalanceResult <- rpcStep "LP settlement signer balance" $ ethGetBalance client signerAddress
      signerBalance <- either (preflightFailure "signer_balance") pure signerBalanceResult
      when (signerBalance <= 0) $
        preflightFailure "signer_balance" "LP settlement signer has zero native-token balance"
      withDb pool $ \conn -> do
        verifyLpSettlementSchema conn >>= \case
          Left err -> preflightFailure "database_schema" err
          Right () -> pure ()
        verifyNoLegacySubmittedLpSettlementAttempts
          conn
          (cfgPerpsChainId cfg)
          >>= \case
            Left err -> preflightFailure "legacy_pending_transaction" err
            Right () -> pure ()
        preparedResult <- prepareLpSettlementWork cfg conn client False
        case preparedResult of
          Left err -> preflightFailure "simulation" err
          Right Nothing -> do
            when
              ( cfgLpSettlementMode cfg == LpSettlementExecute
                  && signerBalance < lpSettlementRequiredBalance cfg
              )
              $ preflightFailure
                "signer_reserve"
                "LP settlement signer balance is below the configured drain-cycle reserve"
            logInfo
              "lp_settlement_preflight_no_ready_work"
              "LP settlement preflight found no safe, ready matured work"
              [ field "signer" signerAddress
              , field "signer_balance_wei" signerBalance
              ]
          Right (Just work) -> do
            let maximumCost = transactionMaximumCost $ lpwTransaction work
                requiredExecuteReserve = lpSettlementRequiredBalance cfg
            case cfgLpSettlementMode cfg of
              LpSettlementExecute -> do
                case validateLpTransactionAffordability cfg work of
                  Left err -> preflightFailure "affordability" err
                  Right () -> pure ()
                when (lpwSignerBalance work < requiredExecuteReserve) $
                  preflightFailure
                    "signer_reserve"
                    ( "LP settlement signer balance is below the configured drain-cycle reserve: balance="
                        <> tshow (lpwSignerBalance work)
                        <> ", required_balance="
                        <> tshow requiredExecuteReserve
                    )
              mode ->
                case
                  validateLpSettlementCost
                    mode
                    (cfgLpSettlementMaxTxCostWei cfg)
                    (lpwSignerBalance work)
                    maximumCost
                of
                  Left err -> preflightFailure "affordability" err
                  Right () -> pure ()
            logInfo
              "lp_settlement_preflight_would_submit"
              "LP settlement preflight selected and simulated the exact canonical transaction"
              (lpPreparedWorkLogFields work <> [field "maximum_transaction_cost_wei" maximumCost])
 where
  preflightFailure :: Text -> Text -> IO a
  preflightFailure category err = do
    logError
      "lp_settlement_invariant_failure"
      "LP settlement preflight failed closed"
      [field "category" category, field "error" err]
    fail $ T.unpack err

-- | Verify the execution-critical deployment as soon as the process starts,
-- while deliberately leaving the worker alive to reconcile an already signed
-- nonce lane. Every operation that can create or resend work repeats the same
-- checks and remains fail-closed.
auditLpSettlementStartup :: Config -> EthClient -> IO ()
auditLpSettlementStartup cfg client =
  verifyLpSettlementStartup cfg client >>= \case
    Left err -> logInvariantFailure "startup" err
    Right signerAddress ->
      logInfo
        "lp_settlement_startup_audit_succeeded"
        "LP settlement startup chain, bytecode, bindings, and signer audit succeeded"
        [ field "mode" $ lpSettlementModeText $ cfgLpSettlementMode cfg
        , field "signer" signerAddress
        , field "monitor" $ cfgPerpsSettlementMonitorLens cfg
        , field "house_pool" $ cfgPerpsHousePool cfg
        ]

verifyLpSettlementStartup
  :: Config
  -> EthClient
  -> IO (Either Text Text)
verifyLpSettlementStartup =
  verifyLpSettlementStartupWithCodeHashes SettlementMonitor.reviewedSettlementCodeHashes

verifyLpSettlementStartupWithCodeHashes
  :: SettlementMonitor.SettlementCodeHashes
  -> Config
  -> EthClient
  -> IO (Either Text Text)
verifyLpSettlementStartupWithCodeHashes codeHashes cfg client = runExceptT $ do
  observedChainId <- ExceptT $ rpcStep "RPC chain id" $ ethChainId client
  ExceptT $ pure $
    unlessEither
      (observedChainId == cfgPerpsChainId cfg)
      ( "RPC chain id mismatch: expected "
          <> tshow (cfgPerpsChainId cfg)
          <> ", observed "
          <> tshow observedChainId
      )
  ExceptT $
    SettlementMonitor.verifySettlementDeployment
      client
      SettlementMonitor.SettlementDeployment
        { SettlementMonitor.sdConfigSchemaVersion = SettlementMonitor.supportedConfigSchemaVersion
        , SettlementMonitor.sdMonitor = cfgPerpsSettlementMonitorLens cfg
        , SettlementMonitor.sdRouter = cfgPerpsOrderRouter cfg
        , SettlementMonitor.sdEngine = cfgPerpsCfdEngine cfg
        , SettlementMonitor.sdHousePool = cfgPerpsHousePool cfg
        , SettlementMonitor.sdSeniorVault = cfgLpSettlementSeniorVault cfg
        , SettlementMonitor.sdJuniorVault = cfgLpSettlementJuniorVault cfg
        , SettlementMonitor.sdPletherOracle = cfgPerpsPletherOracle cfg
        }
      codeHashes
  privateKey <- ExceptT $ pure $ maybe (Left "LP_SETTLEMENT_PRIVATE_KEY is not configured") Right $ cfgLpSettlementPrivateKey cfg
  signerAddress <- ExceptT $ ioTextStep "LP settlement signer" $ deriveAddress privateKey
  pure signerAddress

rpcStep :: Text -> IO (Either RpcError a) -> IO (Either Text a)
rpcStep label action = do
  result <- action
  pure $ either (Left . ((label <> ": ") <>) . rpcErrorText) Right result

ioTextStep :: Text -> IO (Either Text a) -> IO (Either Text a)
ioTextStep label action = do
  result <- action
  pure $ either (Left . ((label <> ": ") <>)) Right result

unlessEither :: Bool -> Text -> Either Text ()
unlessEither condition message = if condition then Right () else Left message

prepareLpSettlementWork
  :: Config
  -> Connection
  -> EthClient
  -> Bool
  -> IO (Either Text (Maybe LpPreparedWork))
prepareLpSettlementWork cfg conn client persistObservation =
  fmap normalizeResult $ runExceptT $ do
  privateKey <- ExceptT $ pure $ maybe (Left "LP_SETTLEMENT_PRIVATE_KEY is not configured") Right $ cfgLpSettlementPrivateKey cfg
  signerAddress <- ExceptT $ ioTextStep "LP settlement signer" $ deriveAddress privateKey
  signerBalance <- ExceptT $ rpcStep "LP settlement signer balance" $ ethGetBalance client signerAddress
  latestEpoch <- ExceptT $ rpcStep "latest HousePool epoch" $
    SettlementMonitor.getCurrentEpoch client (cfgPerpsHousePool cfg)
  latestStatus <- ExceptT $ rpcStep "latest settlement status" $
    SettlementMonitor.getSettlementStatus
      client
      (cfgPerpsSettlementMonitorLens cfg)
      latestEpoch
  case assessLpSettlementStatus latestStatus of
    LpSettlementReady _ -> pure ()
    decision -> do
      liftIO $ logLpSettlementDecision latestEpoch latestStatus decision
      ExceptT $ pure $ Left "LP_SETTLEMENT_NOT_READY"
  latestBlock <- ExceptT $ rpcStep "chain head" $ ethBlockNumber client
  let observedBlock = max 0 $ latestBlock - fromIntegral (cfgKeeperConfirmations cfg)
  pinnedBlockBefore <- ExceptT $ rpcStep "pinned block" $ ethGetBlockByNumber client observedBlock
  ExceptT $ pure $
    unlessEither
      (rpcBlockNumber pinnedBlockBefore == observedBlock)
      "Pinned block response does not match the requested block number"
  -- Arbitrum's Solidity block.number is the L1 block number, while the RPC
  -- block tag and hash identify the L2 block. Compare the facade observation
  -- against the former, but retain the latter as the durable audit identity.
  let monitorObservedBlock = fromMaybe observedBlock $ rpcBlockL1Number pinnedBlockBefore
  pinnedEpoch <- ExceptT $ rpcStep "pinned HousePool epoch" $
    SettlementMonitor.getCurrentEpochAtBlock client (cfgPerpsHousePool cfg) observedBlock
  if pinnedEpoch /= latestEpoch
    then do
      liftIO $
        logInfoEvery
          15
          "lp_settlement_epoch_confirmation_pending"
          "LP settlement is waiting for the confirmed observation to cross the epoch boundary"
          [field "latest_epoch" latestEpoch, field "pinned_epoch" pinnedEpoch, field "observed_block" observedBlock]
      ExceptT $ pure $ Left "LP_SETTLEMENT_NOT_READY"
    else pure ()
  observation <- ExceptT $ rpcStep "pinned settlement observation" $
    SettlementMonitor.getSettlementObservationAtBlock
      client
      (cfgPerpsSettlementMonitorLens cfg)
      pinnedEpoch
      observedBlock
  ExceptT $ pure $
    unlessEither
      (isLpSettlementObservationConsistent pinnedEpoch monitorObservedBlock observation)
      "Settlement observation epoch/block fields do not match the pinned request"
  ExceptT $ pure $
    unlessEither
      (isLpSettlementObservationSafe observation)
      "Settlement observation is incomplete, unhealthy, blocked, or uses an unsupported schema/path"
  pinnedBlockAfterObservation <- ExceptT $ rpcStep "pinned block recheck" $ ethGetBlockByNumber client observedBlock
  ExceptT $ pure $
    unlessEither
      (samePinnedBlock pinnedBlockBefore pinnedBlockAfterObservation)
      "Pinned settlement observation was invalidated by a block reorganization"
  let status = SettlementMonitor.soStatus observation
      dependencyMask =
        SettlementMonitor.ssDependencyFailureMask status
          .|. SettlementMonitor.soHealthDependencyFailureMask observation
  liftIO $
    logInfoEvery
      60
      "lp_settlement_ready_backlog"
      "Safe, ready matured LP settlement work remains"
      [ field "epoch" $ SettlementMonitor.ssSettlementCutoffEpoch status
      , field "oldest_matured_head" $ SettlementMonitor.ssOldestMaturedHead status
      , field "execution_path" $ show $ SettlementMonitor.ssRequiredExecutionPath status
      ]
  when persistObservation $
    ExceptT $
      Right <$> recordLpSettlementObservationV2
        conn
        LpSettlementObservationInput
          { lsoiChainId = cfgPerpsChainId cfg
          , lsoiMonitorAddress = cfgPerpsSettlementMonitorLens cfg
          , lsoiObservationDigest = SettlementMonitor.soObservationDigest observation
          , lsoiEpoch = SettlementMonitor.ssSettlementCutoffEpoch status
          , lsoiObservedBlock = observedBlock
          , lsoiObservedBlockHash = Just $ rpcBlockHash pinnedBlockAfterObservation
          , lsoiExecutionPath = executionPathNumber $ SettlementMonitor.ssRequiredExecutionPath status
          , lsoiOperationalBlockerMask = SettlementMonitor.ssOperationalBlockerMask status
          , lsoiWarningMask = SettlementMonitor.ssWarningMask status
          , lsoiDependencyFailureMask = dependencyMask
          , lsoiCriticalFaultMask = SettlementMonitor.soCriticalFaultMask observation
          , lsoiSchemaVersion = SettlementMonitor.soSchemaVersion observation
          , lsoiHealthState = SettlementMonitor.soHealthState observation
          , lsoiExecutionPathDependencyMask = SettlementMonitor.ssExecutionPathDependencyMask status
          , lsoiStatusDependencyFailureMask = SettlementMonitor.ssDependencyFailureMask status
          , lsoiHealthDependencyFailureMask = SettlementMonitor.soHealthDependencyFailureMask observation
          , lsoiObservationComplete = SettlementMonitor.soObservationComplete observation
          , lsoiHasMaturedWork = SettlementMonitor.ssHasMaturedWork status
          , lsoiLpEpochSettlementPaused = SettlementMonitor.ssLpEpochSettlementPaused status
          }
  (target, value, callData) <- ExceptT $ buildLpSettlementTransaction cfg conn client status
  estimatedGasResult <- liftIO $ ethEstimateGas client signerAddress target value callData
  estimatedGas <-
    case estimatedGasResult of
      Right gas -> pure gas
      Left simulationError -> do
        superseded <- ExceptT $ Right <$> lpSettlementWasSuperseded cfg client simulationError
        if superseded
          then do
            liftIO $
              logInfo
                "lp_settlement_benign_supersession"
                "Another permissionless caller cleared the observed LP work before submission"
                [ field "epoch" pinnedEpoch
                , field "error" $ rpcErrorText simulationError
                ]
            ExceptT $ pure $ Left "LP_SETTLEMENT_NOT_READY"
          else ExceptT $ pure $ Left $ "exact LP settlement simulation failed: " <> rpcErrorText simulationError
  nonce <- ExceptT $ rpcStep "LP settlement pending nonce" $ ethGetTransactionCount client signerAddress
  gasPrice <- ExceptT $ rpcStep "LP settlement gas price" $ ethGasPrice client
  priorityResult <- ExceptT $ Right <$> ethMaxPriorityFeePerGas client
  let priorityBase = fromRight gasPrice priorityResult
      maxFeeBase = max gasPrice priorityBase
      gasLimit = max 21_000 $ applyBpsBuffer estimatedGas (cfgKeeperGasBufferBps cfg)
      maxPriorityFee = applyBpsBuffer priorityBase (cfgKeeperFeeBufferBps cfg)
      maxFee = max maxPriorityFee $ applyBpsBuffer maxFeeBase (cfgKeeperFeeBufferBps cfg)
      transaction =
        Tx1559
          { txChainId = cfgPerpsChainId cfg
          , txNonce = nonce
          , txMaxPriorityFeePerGas = maxPriorityFee
          , txMaxFeePerGas = maxFee
          , txGasLimit = gasLimit
          , txTo = target
          , txValue = value
          , txData = callData
          }
  pinnedBlockBeforeSubmission <- ExceptT $ rpcStep "pinned block final recheck" $ ethGetBlockByNumber client observedBlock
  ExceptT $ pure $
    unlessEither
      (samePinnedBlock pinnedBlockAfterObservation pinnedBlockBeforeSubmission)
      "Pinned settlement observation was invalidated before transaction preparation completed"
  let work =
        LpPreparedWork
          { lpwObservation = observation
          , lpwObservedBlockHash = rpcBlockHash pinnedBlockBeforeSubmission
          , lpwSignerAddress = signerAddress
          , lpwSignerBalance = signerBalance
          , lpwTarget = target
          , lpwValue = value
          , lpwCallData = callData
          , lpwTransaction = transaction
          }
  pure $ Just work
 where
  -- Expected non-ready decisions are represented as an internal sentinel so
  -- the ExceptT setup remains compact; callers turn it into 'Nothing'.
  normalizeResult = \case
    Left "LP_SETTLEMENT_NOT_READY" -> Right Nothing
    other -> other

  samePinnedBlock left right =
    rpcBlockNumber left == rpcBlockNumber right
      && normalizeHex (rpcBlockHash left) == normalizeHex (rpcBlockHash right)

logLpSettlementDecision
  :: Integer
  -> SettlementMonitor.SettlementStatus
  -> LpSettlementDecision
  -> IO ()
logLpSettlementDecision epoch status = \case
  LpSettlementHeld ->
    logWarnEvery 60 "lp_settlement_held" "Governance has paused LP epoch settlement" fields
  LpSettlementNoMaturedWork ->
    logInfoEvery 300 "lp_settlement_no_matured_work" "No matured LP settlement work is visible" fields
  LpSettlementDependenciesUnknown ->
    logWarnEvery 60 "lp_settlement_dependency_unknown" "LP settlement dependencies are incomplete" fields
  LpSettlementOperationallyBlocked ->
    logWarnEvery 60 "lp_settlement_operationally_blocked" "LP settlement is blocked by protocol health" fields
  LpSettlementReady _ -> pure ()
 where
  fields =
    [ field "epoch" epoch
    , field "execution_path" $ show $ SettlementMonitor.ssRequiredExecutionPath status
    , field "operational_blocker_mask" $ SettlementMonitor.ssOperationalBlockerMask status
    , field "dependency_failure_mask" $ SettlementMonitor.ssDependencyFailureMask status
    ]

lpSettlementWasSuperseded :: Config -> EthClient -> RpcError -> IO Bool
lpSettlementWasSuperseded cfg client simulationError
  | not $ Perps.isNoLpEpochProgressRpcError simulationError = pure False
  | otherwise = lpSettlementWorkIsNowAbsent cfg client

lpPreparedWorkLogFields :: LpPreparedWork -> [LogField]
lpPreparedWorkLogFields work =
  let observation = lpwObservation work
      status = SettlementMonitor.soStatus observation
      tx = lpwTransaction work
   in [ field "epoch" $ SettlementMonitor.ssSettlementCutoffEpoch status
      , field "observed_block" $ SettlementMonitor.ssObservedBlock status
      , field "observation_digest" $ SettlementMonitor.soObservationDigest observation
      , field "execution_path" $ show $ SettlementMonitor.ssRequiredExecutionPath status
      , field "signer" $ lpwSignerAddress work
      , field "signer_balance_wei" $ lpwSignerBalance work
      , field "target" $ lpwTarget work
      , field "value_wei" $ lpwValue work
      , field "nonce" $ txNonce tx
      , field "gas_limit" $ txGasLimit tx
      , field "max_priority_fee_per_gas_wei" $ txMaxPriorityFeePerGas tx
      , field "max_fee_per_gas_wei" $ txMaxFeePerGas tx
      ]

transactionMaximumCost :: Tx1559 -> Integer
transactionMaximumCost tx = txValue tx + txGasLimit tx * txMaxFeePerGas tx

processLpSettlementCycle :: Config -> Connection -> EthClient -> IO ()
processLpSettlementCycle =
  processLpSettlementCycleWithCodeHashes SettlementMonitor.reviewedSettlementCodeHashes

processLpSettlementCycleWithCodeHashes
  :: SettlementMonitor.SettlementCodeHashes
  -> Config
  -> Connection
  -> EthClient
  -> IO ()
processLpSettlementCycleWithCodeHashes codeHashes cfg conn client = do
  active <- getActiveLpSettlementForConfiguredSigner cfg conn
  case active of
    Nothing -> beginNewWork 0
    Just transaction ->
      verifyLpSettlementRecoveryChain cfg client >>= \case
        Left err -> logInvariantFailure "recovery_chain" err
        Right () ->
          reconcileLpSettlementTransaction codeHashes cfg conn client transaction >>= \case
            LpReconciledSuccess -> beginNewWork 1
            LpReconciledSuperseded -> beginNewWork 0
            LpReconcilePending -> pure ()
            LpReconcileManualReview -> pure ()
 where
  beginNewWork confirmedCount =
    verifyLpSettlementNewWorkSafety codeHashes cfg conn client >>= \case
      Left err -> logInvariantFailure "new_work_safety" err
      Right signerAddress -> do
        logInfoEvery
          300
          "lp_settlement_startup_verified"
          "LP settlement chain, code, bindings, schema, legacy state, and signer were verified before preparing new work"
          [ field "monitor" $ cfgPerpsSettlementMonitorLens cfg
          , field "order_router" $ cfgPerpsOrderRouter cfg
          , field "house_pool" $ cfgPerpsHousePool cfg
          , field "senior_vault" $ cfgLpSettlementSeniorVault cfg
          , field "junior_vault" $ cfgLpSettlementJuniorVault cfg
          , field "plether_oracle" $ cfgPerpsPletherOracle cfg
          , field "signer" signerAddress
          , field "mode" $ lpSettlementModeText $ cfgLpSettlementMode cfg
          ]
        drainLpSettlementBacklog codeHashes cfg conn client confirmedCount

verifyLpSettlementNewWorkSafety
  :: SettlementMonitor.SettlementCodeHashes
  -> Config
  -> Connection
  -> EthClient
  -> IO (Either Text Text)
verifyLpSettlementNewWorkSafety codeHashes cfg conn client = runExceptT $ do
  ExceptT $ verifyLpSettlementSchema conn
  ExceptT $
    verifyNoLegacySubmittedLpSettlementAttempts
      conn
      (cfgPerpsChainId cfg)
  ExceptT $ verifyLpSettlementStartupWithCodeHashes codeHashes cfg client

verifyLpSettlementRecoveryChain :: Config -> EthClient -> IO (Either Text ())
verifyLpSettlementRecoveryChain cfg client = do
  observed <- rpcStep "RPC chain id" $ ethChainId client
  pure $ do
    observedChainId <- observed
    unlessEither
      (observedChainId == cfgPerpsChainId cfg)
      ( "RPC chain id mismatch during durable reconciliation: expected "
          <> tshow (cfgPerpsChainId cfg)
          <> ", observed "
          <> tshow observedChainId
      )

drainLpSettlementBacklog
  :: SettlementMonitor.SettlementCodeHashes
  -> Config
  -> Connection
  -> EthClient
  -> Int
  -> IO ()
drainLpSettlementBacklog codeHashes cfg conn client confirmedCount
  | confirmedCount >= cfgLpSettlementMaxDrainTransactions cfg =
      logInfo
        "lp_settlement_drain_cap_reached"
        "LP settlement stopped at the configured per-cycle confirmed transaction cap"
        [ field "confirmed_transaction_count" confirmedCount
        , field "max_drain_transactions" $ cfgLpSettlementMaxDrainTransactions cfg
        ]
  | otherwise =
      prepareLpSettlementWork cfg conn client True >>= \case
        Left err -> do
          logInvariantFailure "observation_or_simulation" err
          logWarnEvery
            60
            "lp_settlement_transaction_unavailable"
            "LP settlement could not prepare its canonical transaction"
            [field "error" err]
        Right Nothing -> pure ()
        Right (Just work) -> do
          let maximumCost = transactionMaximumCost $ lpwTransaction work
          logLpSignerBalance cfg (lpwSignerAddress work) (lpwSignerBalance work)
          case cfgLpSettlementMode cfg of
            LpSettlementOff -> pure ()
            LpSettlementObserve ->
              case
                validateLpSettlementCost
                  LpSettlementObserve
                  (cfgLpSettlementMaxTxCostWei cfg)
                  (lpwSignerBalance work)
                  maximumCost
              of
                Left err -> do
                  logWarnEvery
                    60
                    "lp_settlement_transaction_unaffordable"
                    "LP settlement observe mode simulated work that fails its affordability gates"
                    (lpPreparedWorkLogFields work <> [field "error" err])
                  when (lpwSignerBalance work < maximumCost) $
                    logLpLowBalance cfg (lpwSignerBalance work)
                Right () ->
                  logInfo
                    "lp_settlement_observe_would_submit"
                    "LP settlement observe mode selected, simulated, and passed affordability without signing or broadcasting"
                    ( lpPreparedWorkLogFields work
                        <> [field "maximum_transaction_cost_wei" maximumCost]
                    )
            LpSettlementExecute ->
              case validateLpTransactionAffordability cfg work of
                Left err -> do
                  logWarnEvery
                    60
                    "lp_settlement_transaction_unaffordable"
                    "LP settlement transaction exceeded its signer balance or configured cost cap"
                    (lpPreparedWorkLogFields work <> [field "error" err])
                  when (lpwSignerBalance work < maximumCost) $
                    logLpLowBalance cfg (lpwSignerBalance work)
                Right () -> do
                  submitted <- persistAndBroadcastLpSettlement cfg conn client work
                  waitForSubmittedLpSettlement codeHashes cfg conn client confirmedCount submitted

validateLpTransactionAffordability :: Config -> LpPreparedWork -> Either Text ()
validateLpTransactionAffordability cfg work =
  validateLpSettlementCost
    LpSettlementExecute
    (cfgLpSettlementMaxTxCostWei cfg)
    (lpwSignerBalance work)
    (transactionMaximumCost $ lpwTransaction work)

validateLpSettlementCost
  :: LpSettlementMode
  -> Integer
  -> Integer
  -> Integer
  -> Either Text ()
validateLpSettlementCost mode configuredCap signerBalance maximumCost
  | mode == LpSettlementExecute && configuredCap <= 0 =
      Left "LP_SETTLEMENT_MAX_TX_COST_WEI must be positive in execute mode"
  | configuredCap > 0 && maximumCost > configuredCap =
      Left $
        "transaction maximum cost "
          <> tshow maximumCost
          <> " wei exceeds configured cap "
          <> tshow configuredCap
          <> " wei"
  | signerBalance < maximumCost =
      Left $
        "signer balance "
          <> tshow signerBalance
          <> " wei is below transaction maximum cost "
          <> tshow maximumCost
          <> " wei"
  | otherwise = Right ()

persistAndBroadcastLpSettlement
  :: Config
  -> Connection
  -> EthClient
  -> LpPreparedWork
  -> IO LpSettlementTransactionRow
persistAndBroadcastLpSettlement cfg conn client work = do
  privateKey <- requireLpSettlementPrivateKey cfg
  signedResult <- signTransaction privateKey (lpwTransaction work)
  signed <- either (fail . T.unpack) pure signedResult
  let observation = lpwObservation work
      status = SettlementMonitor.soStatus observation
      tx = lpwTransaction work
  persisted <-
    prepareLpSettlementTransaction
      conn
      LpSettlementSignedIntent
        { lssiChainId = cfgPerpsChainId cfg
        , lssiMonitorAddress = cfgPerpsSettlementMonitorLens cfg
        , lssiObservationDigest = SettlementMonitor.soObservationDigest observation
        , lssiEpoch = SettlementMonitor.ssSettlementCutoffEpoch status
        , lssiSignerAddress = lpwSignerAddress work
        , lssiNonce = txNonce tx
        , lssiTargetAddress = txTo tx
        , lssiValue = txValue tx
        , lssiCalldata = txData tx
        , lssiGasLimit = txGasLimit tx
        , lssiMaxPriorityFeePerGas = txMaxPriorityFeePerGas tx
        , lssiMaxFeePerGas = txMaxFeePerGas tx
        , lssiSignedRawTransaction = signedRawTransaction signed
        , lssiSignedTransactionHash = signedTransactionHash signed
        }
  broadcastLpSettlementTransaction conn client persisted
  refreshed <- getActiveLpSettlementForConfiguredSigner cfg conn
  case refreshed of
    Just active -> pure active
    Nothing -> pure persisted

broadcastLpSettlementTransaction
  :: Connection
  -> EthClient
  -> LpSettlementTransactionRow
  -> IO ()
broadcastLpSettlementTransaction conn client transaction = do
  sendResult <- ethSendRawTransaction client (lstrSignedRawTransaction transaction)
  let signedHash = lstrSignedTransactionHash transaction
      broadcastInput =
        case sendResult of
          Left err ->
            LpSettlementBroadcastInput
              { lsbiAttemptId = lstrId transaction
              , lsbiOutcome = "ambiguous"
              , lsbiReturnedTransactionHash = Nothing
              , lsbiRpcError = Just $ rpcErrorText err
              }
          Right returnedHash ->
            LpSettlementBroadcastInput
              { lsbiAttemptId = lstrId transaction
              , lsbiOutcome = "accepted"
              , lsbiReturnedTransactionHash = Just returnedHash
              , lsbiRpcError = Nothing
              }
  _ <- appendLpSettlementBroadcast conn broadcastInput
  case sendResult of
    Left err ->
      logWarn
        "lp_settlement_broadcast_uncertain"
        "LP settlement broadcast response was uncertain; the persisted hash will be reconciled"
        [ field "transaction_hash" signedHash
        , field "nonce" $ lstrNonce transaction
        , field "error" $ rpcErrorText err
        ]
    Right returnedHash
      | normalizeHex returnedHash == normalizeHex signedHash ->
          logInfo
            "lp_settlement_broadcast"
            "LP settlement transaction was broadcast from its durable signed intent"
            [ field "transaction_hash" signedHash
            , field "nonce" $ lstrNonce transaction
            , field "replacement_count" $ lstrReplacementCount transaction
            ]
      | otherwise ->
          logInvariantFailure
            "broadcast_hash_mismatch"
            ("RPC returned " <> returnedHash <> " for signed transaction " <> signedHash)

waitForSubmittedLpSettlement
  :: SettlementMonitor.SettlementCodeHashes
  -> Config
  -> Connection
  -> EthClient
  -> Int
  -> LpSettlementTransactionRow
  -> IO ()
waitForSubmittedLpSettlement codeHashes cfg conn client confirmedCount submitted = do
  startedAt <- getCurrentTime
  go startedAt submitted
 where
  go startedAt transaction = do
    outcome <- reconcileLpSettlementTransaction codeHashes cfg conn client transaction
    case outcome of
      LpReconciledSuccess -> drainLpSettlementBacklog codeHashes cfg conn client (confirmedCount + 1)
      LpReconciledSuperseded -> pure ()
      LpReconcileManualReview -> pure ()
      LpReconcilePending -> do
        now <- getCurrentTime
        if diffUTCTime now startedAt >= 120
          then logLpPendingStuck cfg transaction
          else do
            emitLpSettlementHeartbeat cfg conn client
            threadDelay 2_000_000
            getActiveLpSettlementForConfiguredSigner cfg conn
              >>= \case
                Nothing -> pure ()
                Just active -> go startedAt active

reconcileLpSettlementTransaction
  :: SettlementMonitor.SettlementCodeHashes
  -> Config
  -> Connection
  -> EthClient
  -> LpSettlementTransactionRow
  -> IO LpReconcileOutcome
reconcileLpSettlementTransaction codeHashes cfg conn client transaction = do
  if lstrStatus transaction == "manual_review"
    then do
      logLpPendingStuck cfg transaction
      reconcileLpTransactionFamily codeHashes cfg conn client transaction
    else
      if lstrChainId transaction /= cfgPerpsChainId cfg
        then
          enterManualReviewAndReconcile
            "chain_mismatch"
            "persisted LP transaction chain does not match PERPS_CHAIN_ID"
        else
          if normalizeHex (lstrMonitorAddress transaction)
              /= normalizeHex (cfgPerpsSettlementMonitorLens cfg)
            then
              enterManualReviewAndReconcile
                "monitor_mismatch"
                "persisted active LP transaction monitor does not match PERPS_SETTLEMENT_MONITOR_LENS"
            else do
              privateKey <- requireLpSettlementPrivateKey cfg
              deriveAddress privateKey >>= \case
                Left err -> enterManualReviewAndReconcile "signer_derivation" err
                Right configuredSigner
                  | normalizeHex (lstrSignerAddress transaction) /= normalizeHex configuredSigner ->
                      enterManualReviewAndReconcile
                        "signer_mismatch"
                        "persisted LP signer does not match LP_SETTLEMENT_PRIVATE_KEY"
                  | normalizeHex (rawTransactionHash $ lstrSignedRawTransaction transaction)
                      /= normalizeHex (lstrSignedTransactionHash transaction) ->
                      enterManualReviewAndReconcile
                        "signed_intent_hash"
                        "persisted LP raw transaction hash does not match its signed hash"
                  | otherwise ->
                      reconcileLpTransactionFamily codeHashes cfg conn client transaction
 where
  enterManualReviewAndReconcile category err = do
    markLpSettlementTransactionManualReview conn (lstrId transaction) err
    logInvariantFailure category err
    reconcileLpTransactionFamily codeHashes cfg conn client transaction

reconcileLpTransactionFamily
  :: SettlementMonitor.SettlementCodeHashes
  -> Config
  -> Connection
  -> EthClient
  -> LpSettlementTransactionRow
  -> IO LpReconcileOutcome
reconcileLpTransactionFamily codeHashes cfg conn client active = do
  family <- getLpSettlementTransactionFamily conn (lstrId active)
  let currentActive =
        fromMaybe active $
          listToMaybe [row | row <- family, lstrId row == lstrId active]
  receiptResults <-
    traverse
      (\row -> fmap ((,) row) <$> ethGetTransactionReceipt client (lstrSignedTransactionHash row))
      family
  forM_
    [ row
    | Right (row, Nothing) <- receiptResults
    , isJust (lstrReceiptTransactionHash row)
    , lstrStatus row `elem` ["confirming", "manual_review", "replaced"]
    ]
    (clearLpSettlementReorgedReceiptEvidence conn . lstrId)
  case [err | Left err <- receiptResults] of
    RpcJsonError err : _ -> do
      let message = "invalid LP settlement receipt response: " <> err
      markLpSettlementTransactionManualReview conn (lstrId active) message
      logInvariantFailure "receipt_shape" message
      pure LpReconcileManualReview
    err : _ -> do
      logWarnEvery
        60
        "lp_settlement_reconciliation_failed"
        "LP settlement transaction-family receipt lookup failed; the nonce lane remains active"
        [ field "transaction_hash" $ lstrSignedTransactionHash active
        , field "error" $ rpcErrorText err
        ]
      pure LpReconcilePending
    [] ->
      case [(row, receipt) | Right (row, Just receipt) <- receiptResults] of
        [] -> reconcileMissingLpReceipt codeHashes cfg conn client currentActive
        [(row, receipt)] -> reconcilePresentLpReceipt cfg conn client row receipt
        _ -> do
          let err = "multiple same-nonce LP transaction hashes returned receipts"
          markLpSettlementTransactionManualReview conn (lstrId active) err
          logInvariantFailure "same_nonce_receipts" err
          pure LpReconcileManualReview

reconcilePresentLpReceipt
  :: Config
  -> Connection
  -> EthClient
  -> LpSettlementTransactionRow
  -> TxReceipt
  -> IO LpReconcileOutcome
reconcilePresentLpReceipt cfg conn client transaction receipt
  | normalizeHex (receiptTxHash receipt) /= normalizeHex (lstrSignedTransactionHash transaction) =
      enterFamilyManualReview
        "receipt_transaction_hash"
        "LP settlement receipt transaction hash differs from the persisted signed intent"
  | otherwise = do
  headResult <- ethBlockNumber client
  case headResult of
    Left err -> do
      logWarnEvery
        60
        "lp_settlement_confirmation_head_failed"
        "LP settlement receipt was found, but confirmation depth could not be determined"
        [ field "transaction_hash" $ receiptTxHash receipt
        , field "error" $ rpcErrorText err
        ]
      pure LpReconcilePending
    Right latestBlock -> do
      let confirmationDepth = max 0 $ latestBlock - receiptBlockNumber receipt
          requiredDepth = fromIntegral $ cfgKeeperConfirmations cfg
          confirmationDepthInt = fromInteger confirmationDepth
      if confirmationDepth < requiredDepth
        then do
          case lstrStatus transaction of
            "replaced" -> pure ()
            "manual_review" ->
              recordLpSettlementReceiptForManualReview
                conn
                (lpSettlementReceiptWithoutEvent transaction receipt confirmationDepthInt)
                (fromMaybe "LP settlement transaction requires manual review" $ lstrLastError transaction)
            _ ->
              markLpSettlementTransactionConfirming
                conn
                (lstrId transaction)
                (receiptTxHash receipt)
                (receiptBlockNumber receipt)
                (receiptBlockHash receipt)
                (receiptSucceeded receipt)
                confirmationDepthInt
          logInfoEvery
            15
            "lp_settlement_confirmation_pending"
            "LP settlement receipt is waiting for the configured confirmation depth"
            [ field "transaction_hash" $ receiptTxHash receipt
            , field "confirmation_depth" confirmationDepth
            , field "required_confirmation_depth" requiredDepth
            ]
          pure LpReconcilePending
        else
          ethGetBlockByNumber client (receiptBlockNumber receipt) >>= \case
            Left err -> do
              logWarnEvery
                60
                "lp_settlement_receipt_block_failed"
                "LP settlement receipt block could not be checked for canonicality"
                [ field "transaction_hash" $ receiptTxHash receipt
                , field "block_number" $ receiptBlockNumber receipt
                , field "error" $ rpcErrorText err
                ]
              pure LpReconcilePending
            Right canonicalBlock
              | normalizeHex (rpcBlockHash canonicalBlock) /= normalizeHex (receiptBlockHash receipt) -> do
                  when
                    (lstrStatus transaction `elem` ["confirming", "manual_review", "replaced"])
                    $ clearLpSettlementReorgedReceiptEvidence conn (lstrId transaction)
                  logWarn
                    "lp_settlement_receipt_reorged"
                    "LP settlement receipt disappeared from the canonical chain; its nonce lane remains active"
                    [ field "transaction_hash" $ receiptTxHash receipt
                    , field "receipt_block_hash" $ receiptBlockHash receipt
                    , field "canonical_block_hash" $ rpcBlockHash canonicalBlock
                    ]
                  pure LpReconcilePending
              | receiptSucceeded receipt ->
                  case
                    Perps.requireSingleLpEpochSettled
                      (cfgPerpsHousePool cfg)
                      (lstrEpoch transaction)
                      receipt
                  of
                    Left err -> do
                      recordLpSettlementReceiptForManualReview
                        conn
                        (lpSettlementReceiptWithoutEvent transaction receipt confirmationDepthInt)
                        err
                      enterFamilyManualReview "settlement_event" err
                    Right event -> do
                      recordLpSettlementReceipt
                        conn
                        LpSettlementReceiptInput
                          { lsriAttemptId = lstrId transaction
                          , lsriTransactionHash = receiptTxHash receipt
                          , lsriBlockNumber = receiptBlockNumber receipt
                          , lsriBlockHash = receiptBlockHash receipt
                          , lsriSucceeded = True
                          , lsriConfirmationDepth = confirmationDepthInt
                          , lsriEventOutcome = Just $ lpSettlementEventOutcome event
                          }
                      logInfo
                        "lp_settlement_confirmed"
                        "LP epoch settlement was confirmed with exactly one valid HousePool event"
                        [ field "transaction_hash" $ receiptTxHash receipt
                        , field "block_number" $ receiptBlockNumber receipt
                        , field "confirmation_depth" confirmationDepth
                        , field "cutoff_epoch" $ Perps.lpesCutoffEpoch event
                        , field "senior_redeem_assets" $ Perps.lpesSeniorRedeemAssets event
                        , field "junior_redeem_assets" $ Perps.lpesJuniorRedeemAssets event
                        , field "junior_deposit_assets" $ Perps.lpesJuniorDepositAssets event
                        , field "senior_deposit_assets" $ Perps.lpesSeniorDepositAssets event
                        , field "senior_backlog" $ Perps.lpesSeniorBacklog event
                        , field "junior_backlog" $ Perps.lpesJuniorBacklog event
                        , field "entries_deferred" $ Perps.lpesEntriesDeferred event
                        ]
                      pure LpReconciledSuccess
              | otherwise -> do
                  competingSettlement <-
                    hasCompetingLpSettlementEvidence cfg conn client transaction receipt
                  if competingSettlement
                    then do
                      recordLpSettlementSupersededReceipt
                        conn
                        (lpSettlementReceiptWithoutEvent transaction receipt confirmationDepthInt)
                        "a prior canonical permissionless transaction settled the observed cutoff"
                      logInfo
                        "lp_settlement_benign_supersession"
                        "The LP transaction reverted after another permissionless caller cleared the work"
                        [ field "transaction_hash" $ receiptTxHash receipt
                        , field "cutoff_epoch" $ lstrEpoch transaction
                        ]
                      pure LpReconciledSuperseded
                    else
                      let err =
                            "LP settlement transaction reverted without a prior canonical competing LpEpochSettled event for its cutoff"
                       in do
                            recordLpSettlementReceiptForManualReview
                              conn
                              (lpSettlementReceiptWithoutEvent transaction receipt confirmationDepthInt)
                              err
                            enterFamilyManualReview "unexpected_revert" err
 where
  enterFamilyManualReview category err = do
    active <- getActiveLpSettlementForConfiguredSigner cfg conn
    case active of
      Just activeTransaction ->
        markLpSettlementTransactionManualReview conn (lstrId activeTransaction) err
      Nothing -> pure ()
    logInvariantFailure category err
    pure LpReconcileManualReview

lpSettlementEventOutcome :: Perps.LpEpochSettled -> LpSettlementEventOutcome
lpSettlementEventOutcome event =
  LpSettlementEventOutcome
    { lseoLogIndex = Perps.lpesLogIndex event
    , lseoCutoffEpoch = Perps.lpesCutoffEpoch event
    , lseoSeniorRedeemAssets = Perps.lpesSeniorRedeemAssets event
    , lseoJuniorRedeemAssets = Perps.lpesJuniorRedeemAssets event
    , lseoJuniorDepositAssets = Perps.lpesJuniorDepositAssets event
    , lseoSeniorDepositAssets = Perps.lpesSeniorDepositAssets event
    , lseoSeniorBacklog = Perps.lpesSeniorBacklog event
    , lseoJuniorBacklog = Perps.lpesJuniorBacklog event
    , lseoEntriesDeferred = Perps.lpesEntriesDeferred event
    }

lpSettlementReceiptWithoutEvent
  :: LpSettlementTransactionRow
  -> TxReceipt
  -> Int
  -> LpSettlementReceiptInput
lpSettlementReceiptWithoutEvent transaction receipt confirmationDepth =
  LpSettlementReceiptInput
    { lsriAttemptId = lstrId transaction
    , lsriTransactionHash = receiptTxHash receipt
    , lsriBlockNumber = receiptBlockNumber receipt
    , lsriBlockHash = receiptBlockHash receipt
    , lsriSucceeded = receiptSucceeded receipt
    , lsriConfirmationDepth = confirmationDepth
    , lsriEventOutcome = Nothing
    }

hasCompetingLpSettlementEvidence
  :: Config
  -> Connection
  -> EthClient
  -> LpSettlementTransactionRow
  -> TxReceipt
  -> IO Bool
hasCompetingLpSettlementEvidence cfg conn client transaction receipt = do
  observedBlock <- getLpSettlementObservationObservedBlock conn (lstrId transaction)
  case observedBlock of
    Nothing -> pure False
    Just fromBlock
      | receiptBlockNumber receipt < fromBlock -> pure False
      | otherwise ->
          ethGetLogs
            client
            (cfgPerpsHousePool cfg)
            [Perps.lpEpochSettledTopic]
            fromBlock
            (receiptBlockNumber receipt)
            >>= \case
              Left err -> do
                logWarn
                  "lp_settlement_competing_event_lookup_failed"
                  "A reverted LP transaction could not be matched to canonical competing settlement evidence"
                  [ field "transaction_hash" $ receiptTxHash receipt
                  , field "error" $ rpcErrorText err
                  ]
                pure False
              Right logs ->
                case traverse Perps.decodeLpEpochSettled logs of
                  Left err -> do
                    logInvariantFailure "competing_settlement_event" err
                    pure False
                  Right events ->
                    let qualifyingEvents = filter (isQualifyingCompetitor fromBlock) events
                     in if null qualifyingEvents
                          then pure False
                          else
                            ethEstimateGasAtBlock
                              client
                              (lstrSignerAddress transaction)
                              (lstrTargetAddress transaction)
                              (lstrValue transaction)
                              (lstrCalldata transaction)
                              (receiptBlockNumber receipt)
                              >>= \case
                                Left replayError
                                  | Perps.isNoLpEpochProgressRpcError replayError -> pure True
                                  | otherwise -> do
                                      logWarn
                                        "lp_settlement_supersession_replay_failed"
                                        "Competing settlement evidence existed, but exact post-block replay did not prove the no-progress revert"
                                        [ field "transaction_hash" $ receiptTxHash receipt
                                        , field "error" $ rpcErrorText replayError
                                        ]
                                      pure False
                                Right _ -> pure False
 where
  isQualifyingCompetitor fromBlock event =
    Perps.lpesCutoffEpoch event == lstrEpoch transaction
      && normalizeHex (Perps.lpesTxHash event)
        /= normalizeHex (receiptTxHash receipt)
      && not (Perps.lpesSeniorBacklog event)
      && not (Perps.lpesJuniorBacklog event)
      && not (Perps.lpesEntriesDeferred event)
      && Perps.lpesBlockNumber event > fromBlock
      && ( Perps.lpesBlockNumber event < receiptBlockNumber receipt
            || ( Perps.lpesBlockNumber event == receiptBlockNumber receipt
                  && Perps.lpesTransactionIndex event
                    < receiptTransactionIndex receipt
               )
         )

lpSettlementWorkIsNowAbsent :: Config -> EthClient -> IO Bool
lpSettlementWorkIsNowAbsent cfg client = do
  epochResult <- SettlementMonitor.getCurrentEpoch client (cfgPerpsHousePool cfg)
  case epochResult of
    Left _ -> pure False
    Right epoch ->
      SettlementMonitor.getSettlementStatus
        client
        (cfgPerpsSettlementMonitorLens cfg)
        epoch
        >>= \case
          Right status -> pure $ assessLpSettlementStatus status == LpSettlementNoMaturedWork
          Left _ -> pure False

reconcileMissingLpReceipt
  :: SettlementMonitor.SettlementCodeHashes
  -> Config
  -> Connection
  -> EthClient
  -> LpSettlementTransactionRow
  -> IO LpReconcileOutcome
reconcileMissingLpReceipt codeHashes cfg conn client transaction = do
  case lstrStatus transaction of
    "confirming" -> markLpSettlementTransactionPending conn (lstrId transaction)
    _ -> pure ()
  latestResult <- ethBlockNumber client
  confirmedNonceResult <-
    case latestResult of
      Left err -> pure $ Left err
      Right latestBlock ->
        ethGetTransactionCountAtBlock
          client
          (lstrSignerAddress transaction)
          (max 0 $ latestBlock - fromIntegral (cfgKeeperConfirmations cfg))
  case confirmedNonceResult of
    Right confirmedNonce
      | confirmedNonce > lstrNonce transaction -> do
          let err =
                "LP signer nonce "
                  <> tshow (lstrNonce transaction)
                  <> " was consumed without a verifiable persisted receipt"
          markLpSettlementTransactionManualReview conn (lstrId transaction) err
          logInvariantFailure "nonce_consumed" err
          pure LpReconcileManualReview
    Left err -> do
      logWarnEvery
        60
        "lp_settlement_nonce_reconciliation_failed"
        "LP settlement confirmed nonce could not be read"
        [ field "transaction_hash" $ lstrSignedTransactionHash transaction
        , field "error" $ rpcErrorText err
        ]
      pure LpReconcilePending
    _
      | lstrStatus transaction == "manual_review" -> do
          logLpPendingStuck cfg transaction
          pure LpReconcileManualReview
      | cfgLpSettlementMode cfg /= LpSettlementExecute -> pure LpReconcilePending
      | otherwise -> reconcileStaleLpTransaction codeHashes cfg conn client transaction

reconcileStaleLpTransaction
  :: SettlementMonitor.SettlementCodeHashes
  -> Config
  -> Connection
  -> EthClient
  -> LpSettlementTransactionRow
  -> IO LpReconcileOutcome
reconcileStaleLpTransaction codeHashes cfg conn client transaction = do
  now <- getCurrentTime
  broadcasts <- getLpSettlementBroadcasts conn (lstrId transaction)
  family <- getLpSettlementTransactionFamily conn (lstrId transaction)
  let transactionAge = ageSeconds now $ lstrCreatedAt transaction
      laneAge =
        maximum $
          transactionAge : map (ageSeconds now . lstrCreatedAt) family
      lastBroadcastAge =
        maybe transactionAge (ageSeconds now . lsbrBroadcastAt) $
          listToMaybe $ reverse $ sortOn lsbrBroadcastSequence broadcasts
      replacementDue = transactionAge >= cfgLpSettlementPendingReplacementSeconds cfg
      rebroadcastDue = lastBroadcastAge >= 30
      atReplacementCap = lstrReplacementCount transaction >= cfgLpSettlementMaxReplacements cfg
  when (laneAge >= 120 || atReplacementCap) $ logLpPendingStuckAt cfg transaction laneAge
  if replacementDue && atReplacementCap
    then do
      let err = "LP settlement transaction remained unconfirmed after reaching the replacement cap"
      markLpSettlementTransactionManualReview conn (lstrId transaction) err
      logInvariantFailure "replacement_cap" err
      pure LpReconcileManualReview
    else
      if replacementDue || rebroadcastDue
        then
          verifyLpSettlementNewWorkSafety codeHashes cfg conn client >>= \case
            Left err -> do
              logInvariantFailure
                "recovery_send_safety"
                ("recovery broadcast/replacement was blocked: " <> err)
              pure LpReconcilePending
            Right _
              | replacementDue ->
                  replaceStaleLpSettlementTransaction cfg conn client transaction
              | persistedLpTransactionMaximumCost transaction > cfgLpSettlementMaxTxCostWei cfg -> do
                  let err = "persisted LP settlement transaction exceeds the current LP_SETTLEMENT_MAX_TX_COST_WEI"
                  markLpSettlementTransactionManualReview conn (lstrId transaction) err
                  logInvariantFailure "rebroadcast_cost_cap" err
                  pure LpReconcileManualReview
              | otherwise -> do
                  broadcastLpSettlementTransaction conn client transaction
                  pure LpReconcilePending
        else pure LpReconcilePending

replaceStaleLpSettlementTransaction
  :: Config
  -> Connection
  -> EthClient
  -> LpSettlementTransactionRow
  -> IO LpReconcileOutcome
replaceStaleLpSettlementTransaction cfg conn client transaction = do
  gasPriceResult <- ethGasPrice client
  priorityResult <- ethMaxPriorityFeePerGas client
  case gasPriceResult of
    Left err -> do
      logWarnEvery
        60
        "lp_settlement_replacement_fee_failed"
        "LP settlement replacement fee quote failed"
        [field "error" $ rpcErrorText err]
      pure LpReconcilePending
    Right gasPrice -> do
      let priorityBase = fromRight gasPrice priorityResult
          (replacementPriorityFee, replacementMaxFee) =
            sameNonceReplacementFees
              (cfgKeeperFeeBufferBps cfg)
              gasPrice
              priorityBase
              (lstrMaxPriorityFeePerGas transaction)
              (lstrMaxFeePerGas transaction)
          replacementTx =
            Tx1559
              { txChainId = lstrChainId transaction
              , txNonce = lstrNonce transaction
              , txMaxPriorityFeePerGas = replacementPriorityFee
              , txMaxFeePerGas = replacementMaxFee
              , txGasLimit = lstrGasLimit transaction
              , txTo = lstrTargetAddress transaction
              , txValue = lstrValue transaction
              , txData = lstrCalldata transaction
              }
          maximumCost = transactionMaximumCost replacementTx
      balanceResult <- ethGetBalance client (lstrSignerAddress transaction)
      case balanceResult of
        Left err -> do
          logWarnEvery
            60
            "lp_settlement_replacement_balance_failed"
            "LP settlement replacement signer balance could not be read"
            [field "error" $ rpcErrorText err]
          pure LpReconcilePending
        Right balance
          | maximumCost > cfgLpSettlementMaxTxCostWei cfg -> do
              let err = "same-nonce replacement would exceed LP_SETTLEMENT_MAX_TX_COST_WEI"
              markLpSettlementTransactionManualReview conn (lstrId transaction) err
              logInvariantFailure "replacement_cost_cap" err
              pure LpReconcileManualReview
          | balance < maximumCost -> do
              logLpLowBalance cfg balance
              pure LpReconcilePending
          | otherwise -> do
              privateKey <- requireLpSettlementPrivateKey cfg
              signTransaction privateKey replacementTx >>= \case
                Left err -> do
                  markLpSettlementTransactionManualReview conn (lstrId transaction) err
                  logInvariantFailure "replacement_signing" err
                  pure LpReconcileManualReview
                Right signed -> do
                  replacement <-
                    replaceLpSettlementTransaction
                      conn
                      (lstrId transaction)
                      replacementPriorityFee
                      replacementMaxFee
                      (signedRawTransaction signed)
                      (signedTransactionHash signed)
                  broadcastLpSettlementTransaction conn client replacement
                  logWarn
                    "lp_settlement_transaction_replaced"
                    "LP settlement replaced a stale transaction at the same nonce"
                    [ field "previous_transaction_hash" $ lstrSignedTransactionHash transaction
                    , field "transaction_hash" $ lstrSignedTransactionHash replacement
                    , field "nonce" $ lstrNonce replacement
                    , field "replacement_count" $ lstrReplacementCount replacement
                    , field "maximum_transaction_cost_wei" maximumCost
                    ]
                  pure LpReconcilePending

ageSeconds :: UTCTime -> UTCTime -> Int
ageSeconds now thenTime = max 0 $ floor $ diffUTCTime now thenTime

persistedLpTransactionMaximumCost :: LpSettlementTransactionRow -> Integer
persistedLpTransactionMaximumCost transaction =
  lstrValue transaction
    + lstrGasLimit transaction * lstrMaxFeePerGas transaction

emitLpSettlementHeartbeat :: Config -> Connection -> EthClient -> IO ()
emitLpSettlementHeartbeat cfg conn client = do
  now <- getCurrentTime
  active <- getActiveLpSettlementForConfiguredSigner cfg conn
  activeFamily <-
    maybe (pure []) (getLpSettlementTransactionFamily conn . lstrId) active
  lastSuccess <-
    getLatestSuccessfulLpSettlementAt
      conn
      (cfgPerpsChainId cfg)
      (cfgPerpsSettlementMonitorLens cfg)
  epochResult <- SettlementMonitor.getCurrentEpoch client (cfgPerpsHousePool cfg)
  statusResult <-
    case epochResult of
      Left err -> pure $ Left err
      Right epoch ->
        SettlementMonitor.getSettlementStatus
          client
          (cfgPerpsSettlementMonitorLens cfg)
          epoch
  signerResult <- lpSettlementSignerAndBalance cfg client
  let epoch = either (const Nothing) Just epochResult
      status = either (const Nothing) Just statusResult
      decision = maybe "unavailable" (lpSettlementDecisionText . assessLpSettlementStatus) status
      executionPath = maybe "unavailable" (T.pack . show . SettlementMonitor.ssRequiredExecutionPath) status
      oldestHead = status >>= SettlementMonitor.ssOldestMaturedHead
      signerBalance = either (const Nothing) (Just . snd) signerResult
      pendingAge =
        case active of
          Nothing -> Nothing
          Just transaction ->
            Just $
              maximum $
                ageSeconds now (lstrCreatedAt transaction)
                  : map (ageSeconds now . lstrCreatedAt) activeFamily
  logInfoEvery
    60
    "lp_settlement_heartbeat"
    "LP settlement worker heartbeat"
    [ field "mode" $ lpSettlementModeText $ cfgLpSettlementMode cfg
    , field "epoch" epoch
    , field "oldest_matured_head" oldestHead
    , field "decision" decision
    , field "execution_path" executionPath
    , field "signer_balance_wei" signerBalance
    , field "pending_transaction_age_seconds" pendingAge
    , field "last_successful_settlement_at" lastSuccess
    ]
  forM_ active $ \transaction ->
    when
      ( ageSeconds now (lstrCreatedAt transaction) >= 120
          || lstrReplacementCount transaction >= cfgLpSettlementMaxReplacements cfg
      )
      $ logLpPendingStuckAt cfg transaction $ fromMaybe 0 pendingAge
  forM_ signerBalance $ logLpLowBalance cfg

lpSettlementSignerAndBalance
  :: Config
  -> EthClient
  -> IO (Either Text (Text, Integer))
lpSettlementSignerAndBalance cfg client = runExceptT $ do
  privateKey <- ExceptT $ pure $ maybe (Left "LP_SETTLEMENT_PRIVATE_KEY is not configured") Right $ cfgLpSettlementPrivateKey cfg
  signer <- ExceptT $ ioTextStep "LP settlement signer" $ deriveAddress privateKey
  balance <- ExceptT $ rpcStep "LP settlement signer balance" $ ethGetBalance client signer
  pure (signer, balance)

lpSettlementDecisionText :: LpSettlementDecision -> Text
lpSettlementDecisionText = \case
  LpSettlementHeld -> "held"
  LpSettlementNoMaturedWork -> "no_matured_work"
  LpSettlementDependenciesUnknown -> "dependencies_unknown"
  LpSettlementOperationallyBlocked -> "operationally_blocked"
  LpSettlementReady SettlementMonitor.CachedMark -> "ready_cached_mark"
  LpSettlementReady SettlementMonitor.AtomicOracleRefresh -> "ready_atomic_oracle_refresh"
  LpSettlementReady _ -> "ready_unsupported_path"

logLpSignerBalance :: Config -> Text -> Integer -> IO ()
logLpSignerBalance cfg _signer balance = logLpLowBalance cfg balance

logLpLowBalance :: Config -> Integer -> IO ()
logLpLowBalance cfg balance =
  when (requiredBalance > 0 && balance < requiredBalance) $
    logWarnEvery
      60
      "lp_settlement_low_balance"
      "LP settlement signer balance is below twice the four-transaction cost budget"
      [ field "signer_balance_wei" balance
      , field "required_balance_wei" requiredBalance
      , field "max_tx_cost_wei" $ cfgLpSettlementMaxTxCostWei cfg
      ]
 where
  requiredBalance = lpSettlementRequiredBalance cfg

lpSettlementRequiredBalance :: Config -> Integer
lpSettlementRequiredBalance cfg =
  8 * cfgLpSettlementMaxTxCostWei cfg

logLpPendingStuck :: Config -> LpSettlementTransactionRow -> IO ()
logLpPendingStuck cfg transaction = do
  now <- getCurrentTime
  logLpPendingStuckAt cfg transaction $ ageSeconds now $ lstrCreatedAt transaction

logLpPendingStuckAt :: Config -> LpSettlementTransactionRow -> Int -> IO ()
logLpPendingStuckAt cfg transaction pendingAge =
  logWarnEvery
    60
    "lp_settlement_pending_stuck"
    "LP settlement transaction exceeded the pending SLO or reached its replacement cap"
    [ field "transaction_hash" $ lstrSignedTransactionHash transaction
    , field "nonce" $ lstrNonce transaction
    , field "pending_age_seconds" pendingAge
    , field "replacement_count" $ lstrReplacementCount transaction
    , field "max_replacements" $ cfgLpSettlementMaxReplacements cfg
    ]

logInvariantFailure :: Text -> Text -> IO ()
logInvariantFailure category err =
  logError
    "lp_settlement_invariant_failure"
    "LP settlement failed closed on an invariant"
    [field "category" category, field "error" err]

requireLpSettlementPrivateKey :: Config -> IO Text
requireLpSettlementPrivateKey cfg =
  maybe
    (fail "LP_SETTLEMENT_PRIVATE_KEY is not configured")
    pure
    (cfgLpSettlementPrivateKey cfg)

getActiveLpSettlementForConfiguredSigner
  :: Config
  -> Connection
  -> IO (Maybe LpSettlementTransactionRow)
getActiveLpSettlementForConfiguredSigner cfg conn = do
  privateKey <- requireLpSettlementPrivateKey cfg
  signerAddress <- deriveAddress privateKey >>= either (fail . T.unpack) pure
  getActiveLpSettlementTransaction
    conn
    (cfgPerpsChainId cfg)
    (cfgPerpsSettlementMonitorLens cfg)
    signerAddress

normalizeHex :: Text -> Text
normalizeHex = T.toLower . T.strip

tshow :: (Show a) => a -> Text
tshow = T.pack . show

buildLpSettlementTransaction
  :: Config
  -> Connection
  -> EthClient
  -> SettlementMonitor.SettlementStatus
  -> IO (Either Text (Text, Integer, ByteString))
buildLpSettlementTransaction cfg conn client status =
  case SettlementMonitor.ssRequiredExecutionPath status of
    SettlementMonitor.CachedMark ->
      pure $
        Right
          ( cfgPerpsHousePool cfg
          , 0
          , Perps.settleLpEpochPoolCall
              (SettlementMonitor.ssCachedMarkPrice status)
              (SettlementMonitor.ssCachedMarkTime status)
          )
    SettlementMonitor.AtomicOracleRefresh -> do
      payload <-
        getLatestPythUpdatePayloadAtOrAfter
          conn
          (SettlementMonitor.ssMinimumAtomicPublishTime status)
      case payload of
        Nothing -> pure $ Left "no admitted latest six-feed Pyth payload is cached"
        Just cachedPayload ->
          case decodePayload cachedPayload of
            Left err -> pure $ Left err
            Right (publishTimes, updateData) ->
              case validateAtomicSettlementPayload
                (SettlementMonitor.ssMinimumAtomicPublishTime status)
                publishTimes
                updateData of
                Left err -> pure $ Left err
                Right () -> do
                  feeResult <- Perps.getUpdateFee client (cfgPerpsPletherOracle cfg) updateData
                  pure $ case feeResult of
                    Left err -> Left $ rpcErrorText err
                    Right exactFee ->
                      Right
                        ( cfgPerpsOrderRouter cfg
                        , exactFee
                        , Perps.settleLpEpochRouterCall updateData
                        )
    _ -> pure $ Left "the Settlement Monitor did not select an executable path"

assessLpSettlementStatus
  :: SettlementMonitor.SettlementStatus
  -> LpSettlementDecision
assessLpSettlementStatus status
  | SettlementMonitor.ssLpEpochSettlementPaused status = LpSettlementHeld
  | not (SettlementMonitor.ssHasMaturedWork status)
      || SettlementMonitor.ssRequiredExecutionPath status == SettlementMonitor.NoMaturedWork =
      LpSettlementNoMaturedWork
  | SettlementMonitor.ssRequiredExecutionPath status == SettlementMonitor.UnknownPath
      || SettlementMonitor.ssExecutionPathDependencyMask status /= 0
      || SettlementMonitor.ssDependencyFailureMask status /= 0 =
      LpSettlementDependenciesUnknown
  | SettlementMonitor.ssOperationalBlockerMask status /= 0 =
      LpSettlementOperationallyBlocked
  | otherwise = LpSettlementReady $ SettlementMonitor.ssRequiredExecutionPath status

isLpSettlementObservationSafe :: SettlementMonitor.SettlementObservation -> Bool
isLpSettlementObservationSafe observation =
  SettlementMonitor.soSchemaVersion observation == SettlementMonitor.supportedObservationSchemaVersion
    && SettlementMonitor.soObservationComplete observation
    && SettlementMonitor.soHealthState observation == 1
    && SettlementMonitor.soCriticalFaultMask observation == 0
    && SettlementMonitor.soHealthDependencyFailureMask observation == 0
    && SettlementMonitor.ssDependencyFailureMask status == 0
    && SettlementMonitor.ssOperationalBlockerMask status == 0
    && SettlementMonitor.ssExecutionPathDependencyMask status == 0
    && SettlementMonitor.ssHasMaturedWork status
    && not (SettlementMonitor.ssLpEpochSettlementPaused status)
    && SettlementMonitor.ssRequiredExecutionPath status
      `elem` [SettlementMonitor.CachedMark, SettlementMonitor.AtomicOracleRefresh]
 where
  status = SettlementMonitor.soStatus observation

isLpSettlementObservationConsistent
  :: Integer
  -> Integer
  -> SettlementMonitor.SettlementObservation
  -> Bool
isLpSettlementObservationConsistent expectedEpoch expectedBlock observation =
  SettlementMonitor.ssCurrentEpoch status == expectedEpoch
    && SettlementMonitor.ssSettlementCutoffEpoch status == expectedEpoch
    && SettlementMonitor.ssObservedBlock status == expectedBlock
 where
  status = SettlementMonitor.soStatus observation

validateAtomicSettlementPayload
  :: Integer
  -> [Integer]
  -> [ByteString]
  -> Either Text ()
validateAtomicSettlementPayload minimumPublishTime publishTimes updateData
  | length publishTimes /= 6 =
      Left "the latest admitted Pyth payload does not contain exactly six feed publish times"
  | null updateData || any BS.null updateData =
      Left "the latest admitted Pyth payload does not contain non-empty binary update data"
  | any (< minimumPublishTime) publishTimes =
      Left "the latest Pyth payload predates the minimum atomic publish time"
  | otherwise = Right ()

executionPathNumber :: SettlementMonitor.ExecutionPath -> Integer
executionPathNumber = \case
  SettlementMonitor.UnknownPath -> 0
  SettlementMonitor.NoMaturedWork -> 1
  SettlementMonitor.CachedMark -> 2
  SettlementMonitor.AtomicOracleRefresh -> 3

indexNewLogs :: Config -> Connection -> EthClient -> IO ()
indexNewLogs cfg conn client = do
  latestResult <- ethBlockNumber client
  case latestResult of
    Left err ->
      logWarnEvery
        60
        "keeper_chain_head_fetch_failed"
        "Keeper could not fetch the chain head"
        [field "error" $ rpcErrorText err]
    Right latestBlock -> do
      lastIndexed <- getPerpsKeeperLastIndexedBlock conn (cfgPerpsOrderRouter cfg)
      let confirmedLatest = max 0 $ latestBlock - fromIntegral (cfgKeeperConfirmations cfg)
          startBlock = max (cfgPerpsIndexerStartBlock cfg) (lastIndexed + 1)
          endBlock = min confirmedLatest (startBlock + 1_999)
      if startBlock > confirmedLatest
        then pure ()
        else do
          let router = cfgPerpsOrderRouter cfg
              lifecycleTopics = [Perps.intentRegisteredTopic, Perps.orderFinalizedTopic]
              addresses = router : maybe [] pure (cfgPerpsOrderLifecycleBook cfg)
              topics = nub $ Perps.perpsOrderTopics <> lifecycleTopics
          logsResult <-
            ethGetLogsForAddresses client addresses topics startBlock endBlock
          case logsResult of
            Left err ->
              logWarnEvery
                60
                "keeper_order_logs_fetch_failed"
                "Keeper could not fetch order logs"
                [ field "from_block" startBlock
                , field "to_block" endBlock
                , field "error" $ rpcErrorText err
                ]
            Right unscopedLogs -> do
              let logs = filter (isExpectedKeeperLog cfg) unscopedLogs
              forM_ (mapMaybe Perps.decodePerpsOrderEvent logs) (applyOrderEvent cfg conn client)
              setPerpsKeeperLastIndexedBlock conn (cfgPerpsOrderRouter cfg) endBlock
              unless (null logs) $
                logInfoEvery
                  300
                  "keeper_order_index_progress"
                  "Keeper indexed new order logs"
                  [ field "from_block" startBlock
                  , field "to_block" endBlock
                  , field "event_count" $ length logs
                  ]

isExpectedKeeperLog :: Config -> RpcLog -> Bool
isExpectedKeeperLog cfg logEntry =
  case rpcLogTopics logEntry of
    [] -> False
    topic0 : _
      | normalizedAddress == normalizeAddress (cfgPerpsOrderRouter cfg) ->
          topic0 `elem` Perps.perpsOrderTopics
      | Just lifecycleBook <- cfgPerpsOrderLifecycleBook cfg
      , normalizedAddress == normalizeAddress lifecycleBook ->
          topic0 `elem` [Perps.intentRegisteredTopic, Perps.orderFinalizedTopic]
      | otherwise -> False
 where
  normalizedAddress = normalizeAddress $ rpcLogAddress logEntry
  normalizeAddress = T.toLower . T.strip

applyOrderEvent :: Config -> Connection -> EthClient -> Perps.PerpsOrderEvent -> IO ()
applyOrderEvent cfg conn client = \case
  Perps.OrderCommitted {..} -> do
    metadataResult <- readCommitMetadata cfg client poeOrderId poeBlockNumber
    case metadataResult of
      Nothing -> pure ()
      Just (commitBlock, commitTime) ->
        upsertPerpsKeeperOrderCommitted
          conn
          (cfgPerpsOrderRouter cfg)
          poeOrderId
          poeAccount
          poeSide
          commitBlock
          poeBlockNumber
          commitTime
          poeTxHash
  Perps.OrderExecuted {..} ->
    markPerpsKeeperOrderExecuted conn (cfgPerpsOrderRouter cfg) poeOrderId poeTxHash poeBlockNumber poeExecutionPrice
  Perps.OrderFailed {..} ->
    markPerpsKeeperOrderFailed conn (cfgPerpsOrderRouter cfg) poeOrderId poeTxHash poeBlockNumber poeFailureReason
  Perps.IntentRegistered {..} -> do
    metadataResult <- readCommitMetadata cfg client poeOrderId poeBlockNumber
    case metadataResult of
      Nothing -> pure ()
      Just (commitBlock, commitTime) ->
        upsertPerpsKeeperOrderCommitted
          conn
          (cfgPerpsOrderRouter cfg)
          poeOrderId
          poeAccount
          poeSide
          commitBlock
          poeBlockNumber
          commitTime
          poeTxHash
  Perps.OrderFinalized {..}
    | poeLifecycleStatus == 2 && poeTerminalReason == 1 ->
        markPerpsKeeperOrderExecuted
          conn
          (cfgPerpsOrderRouter cfg)
          poeOrderId
          poeTxHash
          poeBlockNumber
          poeExecutionPrice
    | otherwise ->
        markPerpsKeeperOrderFailed
          conn
          (cfgPerpsOrderRouter cfg)
          poeOrderId
          poeTxHash
          poeBlockNumber
          poeTerminalReason

readCommitMetadata :: Config -> EthClient -> Integer -> Integer -> IO (Maybe (Integer, Integer))
readCommitMetadata cfg client orderId fallbackBlock = do
  viewResult <- Perps.getPendingOrderView client (cfgPerpsOrderRouter cfg) orderId
  case viewResult of
    Right view | Perps.povOrderId view == orderId ->
      pure $ Just (Perps.povCommitBlock view, Perps.povCommitTime view)
    _ -> do
      timestampResult <- ethBlockTimestamp client fallbackBlock
      case timestampResult of
        Left err -> do
          logWarnEvery
            60
            "keeper_commit_metadata_fetch_failed"
            "Keeper could not fetch order commit metadata"
            [ field "order_id" orderId
            , field "fallback_block" fallbackBlock
            , field "error" $ rpcErrorText err
            ]
          pure Nothing
        Right commitTime -> pure $ Just (fallbackBlock, commitTime)

processQueueHead :: Config -> Connection -> EthClient -> Bool -> IO KeeperIterationActivity
processQueueHead cfg conn client dryRun = do
  pending <- getPendingPerpsKeeperOrders conn (cfgPerpsOrderRouter cfg) (cfgKeeperMaxBatchSize cfg)
  case pending of
    [] -> pure KeeperIdle
    headOrder : _ -> do
      settlementWindowResult <- Perps.orderSettlementWindow client (cfgPerpsPletherOracle cfg)
      chainNowResult <- ethLatestBlockTimestamp client
      latestBlockResult <- ethBlockNumber client
      case (settlementWindowResult, chainNowResult, latestBlockResult) of
        (Right settlementWindow, Right chainNow, Right latestBlock) ->
          decideExecution cfg conn client dryRun pending headOrder settlementWindow chainNow latestBlock
        _ -> do
          let errors =
                [ either (Just . rpcErrorText) (const Nothing) settlementWindowResult
                , either (Just . rpcErrorText) (const Nothing) chainNowResult
                , either (Just . rpcErrorText) (const Nothing) latestBlockResult
                ]
          logWarnEvery
            60
            "keeper_queue_context_fetch_failed"
            "Keeper could not load the chain context required to process its queue"
            [ field "pending_order_count" $ length pending
            , field "error" $ T.intercalate "; " $ catMaybes errors
            ]
      pure KeeperPending

decideExecution
  :: Config
  -> Connection
  -> EthClient
  -> Bool
  -> [PerpsKeeperOrderRow]
  -> PerpsKeeperOrderRow
  -> Integer
  -> Integer
  -> Integer
  -> IO ()
decideExecution cfg conn client dryRun pending headOrder settlementWindow chainNow latestBlock = do
  freshHeadResult <- refreshPendingOrder cfg client headOrder
  case freshHeadResult of
    Left err -> do
      recordPerpsKeeperOrderError conn (cfgPerpsOrderRouter cfg) (pkorOrderId headOrder) err
      logWarnEvery
        60
        "keeper_queue_head_refresh_failed"
        "Keeper could not refresh the queue head"
        [ field "order_id" $ pkorOrderId headOrder
        , field "error" err
        ]
    Right (RefreshedTerminalOrder outcome) ->
      reconcileTerminalOrder cfg conn headOrder outcome
    Right (RefreshedPendingOrder FreshPendingOrder {fpoOrder = freshHead, fpoIsClose = freshHeadIsClose, fpoValidUntil})
      | not (isPastCommitBlock latestBlock freshHead) ->
          logInfoEvery
            300
            "keeper_waiting_for_post_commit_block"
            "Queue head is waiting for a post-commit block"
            [ field "order_id" $ pkorOrderId freshHead
            , field "commit_block" $ pkorCommitBlock freshHead
            , field "chain_head_block" latestBlock
            ]
      | isOrderPastValidUntil chainNow fpoValidUntil ->
          submitIntent cfg conn client dryRun $ CleanupExpired freshHead
      | chainNow < pkorCommitTime freshHead + 1 ->
          logInfoEvery
            300
            "keeper_waiting_for_reveal_window"
            "Queue head is waiting for its reveal window"
            [ field "order_id" $ pkorOrderId freshHead
            , field "commit_time" $ pkorCommitTime freshHead
            , field "chain_time" chainNow
            ]
      | otherwise ->
          executeReadyHead (drop 1 pending) freshHead freshHeadIsClose fpoValidUntil
  where
    executeReadyHead remainingPending freshHead freshHeadIsClose validUntil = do
      frozenCloseResult <- tryFrozenClosePayload freshHead freshHeadIsClose
      case frozenCloseResult of
        Left err -> do
          recordPerpsKeeperOrderError conn (cfgPerpsOrderRouter cfg) (pkorOrderId freshHead) err
          logWarnEvery
            60
            "keeper_frozen_close_payload_failed"
            "Keeper could not select a frozen-close payload"
            [ field "order_id" $ pkorOrderId freshHead
            , field "error" err
            ]
        Right (Just (payload, publishTimes, updateData)) ->
          submitIntent cfg conn client dryRun $
            ExecuteReady [freshHead] payload publishTimes updateData
        Right Nothing ->
          executeHistoricalReadyHead remainingPending freshHead freshHeadIsClose validUntil

    executeHistoricalReadyHead remainingPending freshHead freshHeadIsClose validUntil = do
      mPayload <-
        getPythUpdatePayloadForWindow
          conn
          (pkorCommitTime freshHead + 1)
          (pkorCommitTime freshHead + settlementWindow)
      case mPayload of
        Nothing ->
          logInfoEvery
            300
            "keeper_waiting_for_cached_payload"
            "Queue head is waiting for its first post-commit Pyth payload"
            [field "order_id" $ pkorOrderId freshHead]
        Just payload
          | not (isHistoricalRevealPayload payload) ->
              logInfoEvery
                300
                "keeper_waiting_for_historical_payload"
                "Queue head is waiting for an exact historical Pyth payload"
                [ field "order_id" $ pkorOrderId freshHead
                , field "cached_payload_source" $ puprSource payload
                ]
        Just payload ->
          case decodePayload payload of
            Left err -> do
              recordPerpsKeeperOrderError conn (cfgPerpsOrderRouter cfg) (pkorOrderId freshHead) err
              logWarnEvery
                60
                "keeper_cached_payload_decode_failed"
                "Keeper could not decode a cached Pyth payload"
                [ field "order_id" $ pkorOrderId freshHead
                , field "error" err
                ]
            Right (publishTimes, updateData) ->
              case validateRevealWindow (pkorCommitTime freshHead) settlementWindow publishTimes of
                Left err -> do
                  logWarnEvery
                    60
                    "keeper_cached_payload_invalid"
                    "Cached Pyth payload is invalid for the queue head"
                    [ field "order_id" $ pkorOrderId freshHead
                    , field "error" err
                    ]
                Right _ -> do
                  refreshedTail <-
                    refreshContiguousOrders
                      cfg
                      client
                      (take (cfgKeeperMaxBatchSize cfg - 1) remainingPending)
                  let refreshed =
                        FreshPendingOrder
                          { fpoOrder = freshHead
                          , fpoIsClose = freshHeadIsClose
                          , fpoValidUntil = validUntil
                          }
                          : refreshedTail
                  let selected =
                        selectBatchCandidates
                          chainNow
                          latestBlock
                          settlementWindow
                          publishTimes
                          (cfgKeeperMaxBatchSize cfg)
                          refreshed
                  case selected of
                    [] ->
                      logInfoEvery
                        300
                        "keeper_waiting_for_first_payload"
                        "Cached Pyth payload is not the first post-commit payload for the queue head"
                        [field "order_id" $ pkorOrderId freshHead]
                    orders ->
                      submitIntent cfg conn client dryRun $
                        ExecuteReady orders payload publishTimes updateData

    tryFrozenClosePayload freshHead freshHeadIsClose
      | not freshHeadIsClose = pure $ Right Nothing
      | otherwise = do
          policyResult <- Perps.getOrderExecutionPolicy client (cfgPerpsPletherOracle cfg) True
          divergenceResult <- Perps.orderExecutionStalenessLimit client (cfgPerpsPletherOracle cfg)
          case (policyResult, divergenceResult) of
            (Right policy, Right maxDivergence)
              | not (Perps.oepOracleFrozen policy) -> pure $ Right Nothing
              | otherwise -> do
                  mPayload <- getLatestPythUpdatePayload conn
                  case mPayload of
                    Nothing -> do
                      logInfoEvery
                        300
                        "keeper_frozen_close_waiting_for_payload"
                        "Frozen close order is waiting for the latest cached Pyth payload"
                        [field "order_id" $ pkorOrderId freshHead]
                      pure $ Right Nothing
                    Just payload ->
                      case decodePayload payload of
                        Left err -> pure $ Left err
                        Right (publishTimes, updateData)
                          | isFrozenClosePayloadReady chainNow (Perps.oepMaxStaleness policy) maxDivergence publishTimes ->
                              pure $ Right $ Just (payload, publishTimes, updateData)
                          | otherwise -> do
                              logInfoEvery
                                300
                                "keeper_frozen_close_payload_not_ready"
                                "Frozen close order is waiting for a policy-compliant Pyth payload"
                                [field "order_id" $ pkorOrderId freshHead]
                              pure $ Right Nothing
            _ ->
              pure $
                Left $
                  T.intercalate
                    "; "
                    $ catMaybes
                      [ either (Just . rpcErrorText) (const Nothing) policyResult
                      , either (Just . rpcErrorText) (const Nothing) divergenceResult
                      ]

reconcileTerminalOrder
  :: Config
  -> Connection
  -> PerpsKeeperOrderRow
  -> Perps.OrderTerminalOutcome
  -> IO ()
reconcileTerminalOrder cfg conn order outcome = do
  case Perps.otoLifecycleStatus outcome of
    2 ->
      reconcilePerpsKeeperOrderExecuted
        conn
        (cfgPerpsOrderRouter cfg)
        orderId
        (Perps.otoTerminalBlock outcome)
        (Perps.otoExecutionPrice outcome)
    3 ->
      reconcilePerpsKeeperOrderFailed
        conn
        (cfgPerpsOrderRouter cfg)
        orderId
        (Perps.otoTerminalBlock outcome)
        (Perps.otoTerminalReason outcome)
    _ -> pure ()
  logInfo
    "keeper_queue_head_reconciled_terminal"
    "Keeper reconciled a stale queue head from canonical lifecycle state"
    [ field "order_id" orderId
    , field "lifecycle_status" $ Perps.otoLifecycleStatus outcome
    , field "terminal_reason_code" $ Perps.otoTerminalReason outcome
    , field "terminal_block" $ Perps.otoTerminalBlock outcome
    , field "execution_mode" $ Perps.otoExecutionMode outcome
    , field "failed_constraint_code" $ Perps.otoFailedConstraint outcome
    , field "receipt_hash" $ "0x" <> TE.decodeUtf8 (B16.encode $ Perps.otoReceiptHash outcome)
    ]
  where
    orderId = pkorOrderId order

refreshPendingOrder :: Config -> EthClient -> PerpsKeeperOrderRow -> IO (Either Text PendingOrderRefresh)
refreshPendingOrder cfg client order =
  case cfgPerpsOrderLifecycleBook cfg of
    Nothing ->
      pure $ Left "PERPS_ORDER_LIFECYCLE_BOOK is required for bounded V2 keeper execution"
    Just lifecycleBook -> do
      statusResult <- Perps.lifecycleStatus client lifecycleBook orderId
      case statusResult of
        Left err -> pure $ Left $ lifecycleReadError err
        Right status ->
          case assessLifecycleRefresh status of
            Left err -> pure $ Left $ orderError err
            Right RefreshPendingLifecycle -> refreshPending lifecycleBook
            Right ReconcileTerminalLifecycle -> refreshTerminal lifecycleBook status
  where
    orderId = pkorOrderId order

    refreshPending lifecycleBook = do
      viewResult <- Perps.getPendingOrderView client (cfgPerpsOrderRouter cfg) orderId
      policyResult <- Perps.pendingPolicyValidUntil client lifecycleBook orderId
      case policyResult of
        Left err ->
          pure $
            Left $
              "could not read immutable pending policy for order "
                <> T.pack (show orderId)
                <> ": "
                <> rpcErrorText err
        Right 0 -> do
          -- Another executor may have finalized the order between the status
          -- and policy reads. Re-read the immutable lifecycle state rather
          -- than wedging the FIFO queue on the now-cleared pending policy.
          statusResult <- Perps.lifecycleStatus client lifecycleBook orderId
          case statusResult of
            Left err -> pure $ Left $ lifecycleReadError err
            Right status ->
              case assessLifecycleRefresh status of
                Left err -> pure $ Left $ orderError err
                Right RefreshPendingLifecycle ->
                  pure $
                    Left $
                      "lifecycle book returned a zero validUntil for pending order "
                        <> T.pack (show orderId)
                Right ReconcileTerminalLifecycle -> refreshTerminal lifecycleBook status
        Right validUntil ->
          pure $ case viewResult of
            Left err ->
              Left $
                "could not re-read pending order "
                  <> T.pack (show orderId)
                  <> ": "
                  <> rpcErrorText err
            Right view
              | Perps.povOrderId view /= orderId ->
                  Left $
                    "router returned pending order "
                      <> T.pack (show $ Perps.povOrderId view)
                      <> " while re-reading order "
                      <> T.pack (show orderId)
              | otherwise ->
                  Right $
                    RefreshedPendingOrder
                      FreshPendingOrder
                        { fpoOrder =
                            order
                              { pkorSide = Perps.povSide view
                              , pkorCommitBlock = Perps.povCommitBlock view
                              , pkorCommitTime = Perps.povCommitTime view
                              }
                        , fpoIsClose = Perps.povIsClose view
                        , fpoValidUntil = validUntil
                        }

    refreshTerminal lifecycleBook status = do
      outcomeResult <- Perps.orderTerminalOutcome client lifecycleBook orderId
      pure $ case outcomeResult of
        Left err ->
          Left $
            "could not read terminal outcome for order "
              <> T.pack (show orderId)
              <> ": "
              <> rpcErrorText err
        Right outcome
          | Perps.otoLifecycleStatus outcome /= status ->
              Left $
                "lifecycle outcome status changed while re-reading order "
                  <> T.pack (show orderId)
          | otherwise -> Right $ RefreshedTerminalOrder outcome

    lifecycleReadError err =
      "could not read lifecycle status for order "
        <> T.pack (show orderId)
        <> ": "
        <> rpcErrorText err

    orderError err = err <> " " <> T.pack (show orderId)

refreshContiguousOrders :: Config -> EthClient -> [PerpsKeeperOrderRow] -> IO [FreshPendingOrder]
refreshContiguousOrders _ _ [] = pure []
refreshContiguousOrders cfg client (order : orders) = do
  result <- refreshPendingOrder cfg client order
  case result of
    Left err -> do
      logWarnEvery
        60
        "keeper_batch_refresh_failed"
        "Keeper stopped refreshing a candidate batch"
        [ field "order_id" $ pkorOrderId order
        , field "error" err
        ]
      pure []
    Right (RefreshedPendingOrder freshOrder) ->
      (freshOrder :) <$> refreshContiguousOrders cfg client orders
    Right (RefreshedTerminalOrder _) -> pure []

submitIntent :: Config -> Connection -> EthClient -> Bool -> ExecutionIntent -> IO ()
submitIntent cfg conn client dryRun intent = do
  let targetOrders = intentOrders intent
      targetIds = map pkorOrderId targetOrders
      (callKind, callData) =
        case intent of
          CleanupExpired order ->
            (SingleOrderCall $ pkorOrderId order, Perps.executeOrderCall (pkorOrderId order) [])
          ExecuteReady [order] _ _ updateData ->
            ( SingleOrderCall $ pkorOrderId order
            , Perps.executeOrderCall (pkorOrderId order) updateData
            )
          ExecuteReady orders _ _ updateData ->
            (BatchOrderCall, Perps.executeOrderBatchCall (maximum $ map pkorOrderId orders) updateData)
  valueResult <- intentValue cfg client intent
  case valueResult of
    Left err -> recordAllErrors cfg conn targetIds err
    Right value -> do
      preflight <- preflightV2OrderTransaction cfg client value callKind callData
      case preflight of
        Left err ->
          if dryRun
            then
              logWarn
                "keeper_transaction_dry_run_failed"
                "Keeper dry-run V2 preflight failed"
                [ field "intent" $ describeIntent intent
                , field "order_ids" targetIds
                , field "error" err
                ]
            else recordAllErrors cfg conn targetIds err
        Right (V2PreflightDeferred reason) ->
          logInfoEvery
            60
            "keeper_transaction_deferred"
            "V2 execution preflight made no terminal progress; no transaction was broadcast"
            [ field "intent" $ describeIntent intent
            , field "order_ids" targetIds
            , field "reason" reason
            ]
        Right (V2PreflightReady gasLimit) ->
          if dryRun
            then
              logInfo
                "keeper_transaction_dry_run"
                "Keeper dry-run passed typed V2 execution preflight"
                [ field "intent" $ describeIntent intent
                , field "order_ids" targetIds
                , field "value_wei" $ show value
                , field "gas_limit" gasLimit
                ]
            else do
              forM_ targetIds (recordPerpsKeeperOrderAttempt conn (cfgPerpsOrderRouter cfg))
              sent <- submitKeeperTransaction cfg client value callData gasLimit
              case sent of
                Left err -> recordAllErrors cfg conn targetIds err
                Right receipt -> applyReceipt cfg conn targetIds receipt

intentValue :: Config -> EthClient -> ExecutionIntent -> IO (Either Text Integer)
intentValue _ _ (CleanupExpired _) = pure $ Right 0
intentValue cfg client (ExecuteReady orders _ _ updateData) = do
  feeResult <- Perps.getUpdateFee client (cfgPerpsPletherOracle cfg) updateData
  pure $
    case feeResult of
      Left err -> Left $ rpcErrorText err
      Right updateFee -> Right $ updateFee * fromIntegral (length orders)

submitKeeperTransaction :: Config -> EthClient -> Integer -> ByteString -> Integer -> IO (Either Text TxReceipt)
submitKeeperTransaction cfg client value callData gasLimit = do
  result <-
    submitKeeperTransactionTo
      cfg
      client
      (cfgPerpsOrderRouter cfg)
      value
      callData
      (Just gasLimit)
      (const $ pure ())
  pure $ either (Left . snd) Right result

preflightV2OrderTransaction
  :: Config
  -> EthClient
  -> Integer
  -> OrderCallKind
  -> ByteString
  -> IO (Either Text V2PreflightResult)
preflightV2OrderTransaction cfg client value callKind callData =
  case cfgKeeperPrivateKey cfg of
    Nothing -> pure $ Left "KEEPER_PRIVATE_KEY is not configured"
    Just privateKey ->
      deriveAddress privateKey >>= \case
        Left err -> pure $ Left err
        Right fromAddr -> do
          estimatedResult <-
            ethEstimateGas client fromAddr (cfgPerpsOrderRouter cfg) value callData
          case estimatedResult of
            Left err -> pure $ Left $ rpcErrorText err
            Right estimatedGas ->
              let initialGas = max 21_000 $ applyBuffer estimatedGas (cfgKeeperGasBufferBps cfg)
               in if initialGas > v2OrderGasLimitCap
                    then
                      pure $
                        Left $
                          "estimated V2 order gas exceeds the keeper safety cap of "
                            <> T.pack (show v2OrderGasLimitCap)
                    else runPreflight fromAddr initialGas
  where
    runPreflight fromAddr gasLimit = do
      callResult <-
        ethCallWithTransactionGas
          client
          (CallParams (cfgPerpsOrderRouter cfg) callData)
          fromAddr
          value
          gasLimit
      case callResult of
        Left err -> pure $ Left $ rpcErrorText err
        Right bytes ->
          case callKind of
            SingleOrderCall expectedOrderId ->
              case Perps.decodeOrderExecutionResult bytes of
                Left err -> pure $ Left $ rpcErrorText err
                Right result ->
                  applyAction fromAddr gasLimit False $ assessSingleOrderPreflight expectedOrderId result
            BatchOrderCall ->
              case Perps.decodeOrderBatchResult bytes of
                Left err -> pure $ Left $ rpcErrorText err
                Right result ->
                  applyAction
                    fromAddr
                    gasLimit
                    (Perps.obrTerminalCount result > 0)
                    (assessBatchOrderPreflight result)

    applyAction fromAddr gasLimit hasProgress = \case
      V2PreflightSubmit -> pure $ Right $ V2PreflightReady gasLimit
      V2PreflightDefer reason -> pure $ Right $ V2PreflightDeferred reason
      V2PreflightReject err -> pure $ Left err
      V2PreflightIncreaseGas
        | gasLimit < v2OrderGasLimitCap -> retryWithMoreGas fromAddr gasLimit
        | hasProgress -> pure $ Right $ V2PreflightReady gasLimit
        | otherwise ->
            pure $
              Left $
                "V2 order execution still reports insufficient gas at the keeper safety cap of "
                  <> T.pack (show v2OrderGasLimitCap)

    retryWithMoreGas fromAddr gasLimit =
      case nextV2GasLimit gasLimit v2OrderGasLimitCap of
        Nothing ->
          pure $
            Left $
              "V2 order execution still reports insufficient gas at the keeper safety cap of "
                <> T.pack (show v2OrderGasLimitCap)
        Just nextGas -> runPreflight fromAddr nextGas

v2InsufficientGasReason :: Integer
v2InsufficientGasReason = 5

v2OrderGasLimitCap :: Integer
v2OrderGasLimitCap = 30_000_000

nextV2GasLimit :: Integer -> Integer -> Maybe Integer
nextV2GasLimit currentGas maximumGas
  | currentGas <= 0 || maximumGas <= 0 || currentGas >= maximumGas = Nothing
  | otherwise = Just $ min maximumGas (currentGas * 2)

assessSingleOrderPreflight :: Integer -> Perps.OrderExecutionResult -> V2PreflightAction
assessSingleOrderPreflight expectedOrderId result
  | Perps.oerOrderId result /= expectedOrderId =
      V2PreflightReject "executeOrder preflight returned a different order ID"
  | Perps.oerLifecycleStatus result == 1
      && Perps.oerPendingReason result == v2InsufficientGasReason =
      V2PreflightIncreaseGas
  | Perps.oerLifecycleStatus result == 1 =
      V2PreflightDefer $
        "order remains pending with reason "
          <> T.pack (show $ Perps.oerPendingReason result)
  | Perps.oerLifecycleStatus result == 2
      || Perps.oerLifecycleStatus result == 3 =
      V2PreflightSubmit
  | otherwise =
      V2PreflightReject "executeOrder preflight returned no lifecycle outcome"

assessBatchOrderPreflight :: Perps.OrderBatchResult -> V2PreflightAction
assessBatchOrderPreflight result
  | Perps.obrStopReason result == v2InsufficientGasReason = V2PreflightIncreaseGas
  | Perps.obrTerminalCount result > 0 = V2PreflightSubmit
  | otherwise =
      V2PreflightDefer $
        "batch made no terminal progress and stopped with reason "
          <> T.pack (show $ Perps.obrStopReason result)

submitKeeperTransactionTo
  :: Config
  -> EthClient
  -> Text
  -> Integer
  -> ByteString
  -> Maybe Integer
  -> (Text -> IO ())
  -> IO (Either (Bool, Text) TxReceipt)
submitKeeperTransactionTo cfg client target value callData gasLimitOverride onBroadcast =
  case cfgKeeperPrivateKey cfg of
    Nothing -> pure $ Left (False, "KEEPER_PRIVATE_KEY is not configured")
    Just privateKey ->
      deriveAddress privateKey >>= \case
        Left err -> pure $ Left (False, err)
        Right fromAddr -> do
          nonceResult <- ethGetTransactionCount client fromAddr
          gasResult <-
            case gasLimitOverride of
              Nothing -> ethEstimateGas client fromAddr target value callData
              Just gasLimit -> pure $ Right gasLimit
          gasPriceResult <- ethGasPrice client
          priorityResult <- ethMaxPriorityFeePerGas client
          case (nonceResult, gasResult, gasPriceResult) of
            (Right nonce, Right estimatedGas, Right gasPrice) -> do
              let priorityBase = fromRight gasPrice priorityResult
                  maxFeeBase = max gasPrice priorityBase
                  gasLimit =
                    case gasLimitOverride of
                      Nothing -> max 21_000 $ applyBuffer estimatedGas (cfgKeeperGasBufferBps cfg)
                      Just explicitGasLimit -> explicitGasLimit
                  maxPriorityFee = applyBuffer priorityBase (cfgKeeperFeeBufferBps cfg)
                  maxFee = max maxPriorityFee $ applyBuffer maxFeeBase (cfgKeeperFeeBufferBps cfg)
                  tx =
                    Tx1559
                      { txChainId = cfgPerpsChainId cfg
                      , txNonce = nonce
                      , txMaxPriorityFeePerGas = maxPriorityFee
                      , txMaxFeePerGas = maxFee
                      , txGasLimit = gasLimit
                      , txTo = target
                      , txValue = value
                      , txData = callData
                      }
              signResult <- signTransaction privateKey tx
              case signResult of
                Left err -> pure $ Left (False, err)
                Right signed -> do
                  sendResult <- ethSendRawTransaction client (signedRawTransaction signed)
                  case sendResult of
                    Left err -> pure $ Left (False, rpcErrorText err)
                    Right txHash -> do
                      onBroadcast txHash
                      receiptResult <- waitForReceipt client txHash 60
                      pure $ either (\err -> Left (True, err)) Right receiptResult
            _ ->
              pure $
                Left
                  ( False
                  , T.intercalate
                      "; "
                      $ catMaybes
                        [ either (Just . rpcErrorText) (const Nothing) nonceResult
                        , either (Just . rpcErrorText) (const Nothing) gasResult
                        , either (Just . rpcErrorText) (const Nothing) gasPriceResult
                        ]
                  )

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

applyReceipt :: Config -> Connection -> [Integer] -> TxReceipt -> IO ()
applyReceipt cfg conn targetIds receipt = do
  let decodedEvents = mapMaybe Perps.decodePerpsOrderEvent (receiptLogs receipt)
      finalizedOrderIds =
        [ poeOrderId
        | Perps.OrderFinalized {..} <- decodedEvents
        ]
      orderEvents = filter (notSuperseded finalizedOrderIds) decodedEvents
  seenIds <- foldM applyEvent [] orderEvents
  let missingIds = filter (`notElem` seenIds) targetIds
  if receiptSucceeded receipt
    then
      forM_ missingIds $ \orderId ->
        recordPerpsKeeperOrderError
          conn
          (cfgPerpsOrderRouter cfg)
          orderId
          ("confirmed in " <> receiptTxHash receipt <> " without target order event")
    else
      forM_ targetIds $ \orderId ->
        recordPerpsKeeperOrderError conn (cfgPerpsOrderRouter cfg) orderId ("transaction reverted: " <> receiptTxHash receipt)
  let transactionLogger = if receiptSucceeded receipt then logInfo else logWarn
  transactionLogger
    "keeper_transaction_mined"
    "Keeper transaction was mined"
    [ field "transaction_hash" $ receiptTxHash receipt
    , field "block_number" $ receiptBlockNumber receipt
    , field "transaction_succeeded" $ receiptSucceeded receipt
    , field "target_order_count" $ length targetIds
    , field "decoded_order_event_count" $ length seenIds
    , field "missing_order_event_count" $ length missingIds
    ]
  where
    notSuperseded finalizedOrderIds = \case
      Perps.OrderExecuted {..} -> poeOrderId `notElem` finalizedOrderIds
      Perps.OrderFailed {..} -> poeOrderId `notElem` finalizedOrderIds
      _ -> True

    applyEvent seen = \case
      Perps.OrderExecuted {..} -> do
        markPerpsKeeperOrderExecuted conn (cfgPerpsOrderRouter cfg) poeOrderId poeTxHash poeBlockNumber poeExecutionPrice
        pure $ poeOrderId : seen
      Perps.OrderFailed {..} -> do
        markPerpsKeeperOrderFailed conn (cfgPerpsOrderRouter cfg) poeOrderId poeTxHash poeBlockNumber poeFailureReason
        logWarn
          "keeper_order_failed"
          "Perps order execution failed"
          [ field "order_id" poeOrderId
          , field "transaction_hash" poeTxHash
          , field "block_number" poeBlockNumber
          , field "failure_reason" $ Perps.orderFailureReasonText poeFailureReason
          , field "failure_reason_code" poeFailureReason
          ]
        pure $ poeOrderId : seen
      Perps.OrderCommitted {} -> pure seen
      Perps.IntentRegistered {} -> pure seen
      Perps.OrderFinalized {..}
        | poeLifecycleStatus == 2 && poeTerminalReason == 1 -> do
            markPerpsKeeperOrderExecuted
              conn
              (cfgPerpsOrderRouter cfg)
              poeOrderId
              poeTxHash
              poeBlockNumber
              poeExecutionPrice
            pure $ poeOrderId : seen
        | otherwise -> do
            markPerpsKeeperOrderFailed
              conn
              (cfgPerpsOrderRouter cfg)
              poeOrderId
              poeTxHash
              poeBlockNumber
              poeTerminalReason
            logWarn
              "keeper_order_finalized_failed"
              "Perps order reached a canonical failed lifecycle outcome"
              [ field "order_id" poeOrderId
              , field "transaction_hash" poeTxHash
              , field "block_number" poeBlockNumber
              , field "terminal_reason_code" poeTerminalReason
              , field "failed_constraint_code" poeFailedConstraint
              , field "execution_mode" poeExecutionMode
              , field "receipt_hash" poeReceiptHash
              ]
            pure $ poeOrderId : seen

recordAllErrors :: Config -> Connection -> [Integer] -> Text -> IO ()
recordAllErrors cfg conn orderIds err = do
  let retryable = isSameBlockMevGuardError err
  forM_ orderIds $ \orderId ->
    if retryable
      then recordPerpsKeeperOrderImmediateRetryError conn (cfgPerpsOrderRouter cfg) orderId err
      else recordPerpsKeeperOrderError conn (cfgPerpsOrderRouter cfg) orderId err
  let failureLogger = if retryable then logWarnEvery else logErrorEvery
  failureLogger
    60
    "keeper_transaction_failed"
    "Keeper transaction was not submitted or confirmed"
    [ field "order_ids" orderIds
    , field "order_count" $ length orderIds
    , field "retryable" retryable
    , field "error" err
    ]

decodePayload :: PythUpdatePayloadRow -> Either Text ([Integer], [ByteString])
decodePayload PythUpdatePayloadRow {puprPublishTimes, puprUpdateData} = do
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

assessLifecycleRefresh :: Integer -> Either Text LifecycleRefreshAction
assessLifecycleRefresh = \case
  1 -> Right RefreshPendingLifecycle
  2 -> Right ReconcileTerminalLifecycle
  3 -> Right ReconcileTerminalLifecycle
  0 -> Left "lifecycle book reports that the indexed order is unused"
  status -> Left $ "lifecycle book returned unsupported status " <> T.pack (show status)

isOrderPastValidUntil :: Integer -> Integer -> Bool
isOrderPastValidUntil chainNow validUntil =
  validUntil > 0 && chainNow > validUntil

isOrderRevealReady :: Integer -> [Integer] -> PerpsKeeperOrderRow -> Bool
isOrderRevealReady settlementWindow publishTimes order =
  either (const False) (const True) $
    validateRevealWindow (pkorCommitTime order) settlementWindow publishTimes

isFrozenClosePayloadReady :: Integer -> Integer -> Integer -> [Integer] -> Bool
isFrozenClosePayloadReady chainNow maxStaleness maxDivergence publishTimes =
  case publishTimes of
    [] -> False
    _ ->
      all (\publishTime -> publishTime <= chainNow && chainNow - publishTime <= maxStaleness) publishTimes
        && maximum publishTimes <= minimum publishTimes + maxDivergence

isSameBlockMevGuardError :: Text -> Bool
isSameBlockMevGuardError err =
  "0x7abb32d5" `T.isInfixOf` T.toLower err

selectBatchCandidates
  :: Integer -- chain now
  -> Integer -- current block
  -> Integer -- settlement window
  -> [Integer] -- payload publish times
  -> Int -- max batch size
  -> [FreshPendingOrder]
  -> [PerpsKeeperOrderRow]
selectBatchCandidates chainNow currentBlock settlementWindow publishTimes maxBatchSize =
  map fpoOrder
    . take maxBatchSize
    . takeWhile isReady
  where
    isReady FreshPendingOrder {..} =
      isPastCommitBlock currentBlock fpoOrder
        && ( isOrderPastValidUntil chainNow fpoValidUntil
              || isOrderRevealReady settlementWindow publishTimes fpoOrder
           )

isPastCommitBlock :: Integer -> PerpsKeeperOrderRow -> Bool
isPastCommitBlock currentBlock order =
  currentBlock > pkorCommitBlock order

intentOrders :: ExecutionIntent -> [PerpsKeeperOrderRow]
intentOrders = \case
  CleanupExpired order -> [order]
  ExecuteReady orders _ _ _ -> orders

describeIntent :: ExecutionIntent -> String
describeIntent = \case
  CleanupExpired order -> "expired-order cleanup for " <> show (pkorOrderId order)
  ExecuteReady [order] _ _ _ -> "single-order execution for " <> show (pkorOrderId order)
  ExecuteReady orders _ _ _ ->
    "batch execution through order "
      <> show (maximum $ map pkorOrderId orders)
      <> " ("
      <> show (length orders)
      <> " orders)"

applyBuffer :: Integer -> Integer -> Integer
applyBuffer value bufferBps =
  ((value * (10_000 + bufferBps)) + 9_999) `div` 10_000

fromRight :: a -> Either e a -> a
fromRight fallback = \case
  Left _ -> fallback
  Right value -> value

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

strip0x :: Text -> Text
strip0x value =
  fromMaybe value $ T.stripPrefix "0x" value

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f =
  foldr
    ( \value acc ->
        case f value of
          Just result -> result : acc
          Nothing -> acc
    )
    []

catMaybes :: [Maybe a] -> [a]
catMaybes =
  foldr
    ( \value acc ->
        case value of
          Just result -> result : acc
          Nothing -> acc
    )
    []
