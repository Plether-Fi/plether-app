module Plether.Keeper
  ( KeeperMode (..)
  , LpSettlementDecision (..)
  , runKeeper
  , assessLpSettlementStatus
  , isLpSettlementObservationSafe
  , validateAtomicSettlementPayload
  , isOrderExpired
  , isOrderRevealReady
  , isFrozenClosePayloadReady
  , isSameBlockMevGuardError
  , selectBatchCandidates
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, bracket, displayException, try)
import Control.Monad (foldM, forM_, unless, void, when)
import Data.Aeson (FromJSON, Result (..), Value, fromJSON)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as B16
import Data.Bits ((.|.))
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock.POSIX (getPOSIXTime)
import Database.PostgreSQL.Simple (Connection)
import Plether.Config (Config (..))
import Plether.Database (DbPool, withDb)
import Plether.Database.Schema
  ( PerpsKeeperOrderRow (..)
  , LpSettlementAttemptRow (..)
  , PythUpdatePayloadRow (..)
  , getPendingPerpsKeeperOrders
  , getPerpsKeeperLastIndexedBlock
  , getLatestPythUpdatePayload
  , getLatestPythUpdatePayloadAtOrAfter
  , getSubmittedLpSettlementAttempts
  , getPythUpdatePayloadForWindow
  , isHistoricalRevealPayload
  , markPerpsKeeperOrderExecuted
  , markPerpsKeeperOrderFailed
  , markLpSettlementAttemptStatus
  , markLpSettlementAttemptSubmitted
  , recordPerpsKeeperOrderAttempt
  , recordPerpsKeeperOrderError
  , recordPerpsKeeperOrderImmediateRetryError
  , recordLpSettlementObservation
  , setPerpsKeeperLastIndexedBlock
  , tryPerpsKeeperLock
  , unlockPerpsKeeperLock
  , upsertPerpsKeeperOrderCommitted
  )
import Plether.Ethereum.Client (EthClient, RpcError (..), ethBlockNumber)
import qualified Plether.Ethereum.Contracts.Perps as Perps
import qualified Plether.Ethereum.Contracts.SettlementMonitor as SettlementMonitor
import Plether.Ethereum.Rpc
  ( TxReceipt (..)
  , ethBlockTimestamp
  , ethEstimateGas
  , ethGasPrice
  , ethGetLogs
  , ethGetTransactionCount
  , ethGetTransactionReceipt
  , ethLatestBlockTimestamp
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
  ( field
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
  }
  deriving stock (Show)

runKeeper :: Config -> DbPool -> EthClient -> KeeperMode -> Bool -> IO ()
runKeeper cfg pool client mode dryRun =
  withDb pool $ \conn ->
    bracket
      (tryPerpsKeeperLock conn)
      (\acquired -> when acquired $ unlockPerpsKeeperLock conn)
      $ \acquired ->
        if not acquired
          then
            logWarn
              "keeper_lock_unavailable"
              "Another keeper instance already holds the advisory lock"
              []
          else do
            when (cfgLpSettlementEnabled cfg || dryRun) $ do
              bindingResult <-
                SettlementMonitor.verifyBindings
                  client
                  (cfgPerpsSettlementMonitorLens cfg)
                  (cfgPerpsOrderRouter cfg)
                  (cfgPerpsHousePool cfg)
              case bindingResult of
                Left err -> fail $ T.unpack err
                Right () ->
                  logInfo
                    "lp_settlement_monitor_bindings_verified"
                    "Settlement Monitor facade bindings match the configured Router and HousePool"
                    [ field "monitor" $ cfgPerpsSettlementMonitorLens cfg
                    , field "order_router" $ cfgPerpsOrderRouter cfg
                    , field "house_pool" $ cfgPerpsHousePool cfg
                    ]
            logInfo
              "keeper_lock_acquired"
              "Keeper acquired the advisory lock"
              []
            lpPollRef <- newIORef 0
            case mode of
              KeeperOnce -> void $ runKeeperIteration cfg conn client dryRun lpPollRef
              KeeperLoop -> loop conn lpPollRef
  where
    loop conn lpPollRef = do
      continue <- runKeeperIteration cfg conn client dryRun lpPollRef
      when continue $ do
        threadDelay (cfgKeeperPollSeconds cfg * 1_000_000)
        loop conn lpPollRef

runKeeperIteration :: Config -> Connection -> EthClient -> Bool -> IORef Integer -> IO Bool
runKeeperIteration cfg conn client dryRun lpPollRef = do
  result <- try $ do
    indexNewLogs cfg conn client
    processQueueHead cfg conn client dryRun
    lpPollDue <- claimLpSettlementPoll cfg dryRun lpPollRef
    when lpPollDue $ processLpSettlement cfg conn client dryRun
  case result of
    Left (err :: SomeException) -> do
      logErrorEvery
        60
        "keeper_iteration_failed"
        "Keeper iteration failed"
        [field "error" $ displayException err]
      pure True
    Right () -> pure True

claimLpSettlementPoll :: Config -> Bool -> IORef Integer -> IO Bool
claimLpSettlementPoll cfg dryRun pollRef
  | not (cfgLpSettlementEnabled cfg || dryRun) = pure False
  | otherwise = do
      now <- floor <$> getPOSIXTime
      atomicModifyIORef' pollRef $ \lastPoll ->
        if lastPoll == 0 || now - lastPoll >= fromIntegral (cfgLpSettlementPollSeconds cfg)
          then (now, True)
          else (lastPoll, False)

processLpSettlement :: Config -> Connection -> EthClient -> Bool -> IO ()
processLpSettlement cfg conn client dryRun = do
  pendingSubmission <- reconcileLpSettlementAttempts cfg conn client
  unless pendingSubmission $ do
    epochResult <- SettlementMonitor.getCurrentEpoch client (cfgPerpsHousePool cfg)
    case epochResult of
      Left err ->
        logWarnEvery
          60
          "lp_settlement_epoch_read_failed"
          "LP settlement could not read the current HousePool epoch"
          [field "error" $ rpcErrorText err]
      Right currentEpoch -> do
        statusResult <-
          SettlementMonitor.getSettlementStatus
            client
            (cfgPerpsSettlementMonitorLens cfg)
            currentEpoch
        case statusResult of
          Left err ->
            logWarnEvery
              60
              "lp_settlement_status_failed"
              "LP settlement status could not be read"
              [ field "epoch" currentEpoch
              , field "error" $ rpcErrorText err
              ]
          Right status -> processLpSettlementStatus cfg conn client dryRun currentEpoch status

processLpSettlementStatus
  :: Config
  -> Connection
  -> EthClient
  -> Bool
  -> Integer
  -> SettlementMonitor.SettlementStatus
  -> IO ()
processLpSettlementStatus cfg conn client dryRun currentEpoch status
  = case assessLpSettlementStatus status of
    LpSettlementHeld ->
      logWarnEvery
        60
        "lp_settlement_held"
        "Governance has paused LP epoch settlement"
        [ field "epoch" currentEpoch
        , field "operational_blocker_mask" $ SettlementMonitor.ssOperationalBlockerMask status
        ]
    LpSettlementNoMaturedWork ->
      logInfoEvery
        300
        "lp_settlement_no_matured_work"
        "No matured LP settlement work is visible"
        [field "epoch" currentEpoch]
    LpSettlementDependenciesUnknown ->
      logWarnEvery
        60
        "lp_settlement_dependency_unknown"
        "LP settlement stopped because one or more dependencies are unknown"
        [ field "epoch" currentEpoch
        , field "execution_path_dependency_mask" $
            SettlementMonitor.ssExecutionPathDependencyMask status
        , field "dependency_failure_mask" $ SettlementMonitor.ssDependencyFailureMask status
        ]
    LpSettlementOperationallyBlocked ->
      logWarnEvery
        60
        "lp_settlement_operationally_blocked"
        "LP settlement is blocked by current protocol health or runtime gates"
        [ field "epoch" currentEpoch
        , field "operational_blocker_mask" $ SettlementMonitor.ssOperationalBlockerMask status
        , field "warning_mask" $ SettlementMonitor.ssWarningMask status
        ]
    LpSettlementReady _ -> do
      blockResult <- ethBlockNumber client
      case blockResult of
        Left err ->
          logWarnEvery
            60
            "lp_settlement_block_read_failed"
            "LP settlement could not pin an observation block"
            [field "error" $ rpcErrorText err]
        Right latestBlock -> do
          let observedBlock = max 0 $ latestBlock - fromIntegral (cfgKeeperConfirmations cfg)
          observationResult <-
            SettlementMonitor.getSettlementObservationAtBlock
              client
              (cfgPerpsSettlementMonitorLens cfg)
              currentEpoch
              observedBlock
          case observationResult of
            Left err ->
              logWarnEvery
                60
                "lp_settlement_observation_failed"
                "The block-pinned LP settlement observation could not be read"
                [ field "epoch" currentEpoch
                , field "observed_block" observedBlock
                , field "error" $ rpcErrorText err
                ]
            Right observation -> processLpObservation cfg conn client dryRun observation

processLpObservation
  :: Config
  -> Connection
  -> EthClient
  -> Bool
  -> SettlementMonitor.SettlementObservation
  -> IO ()
processLpObservation cfg conn client dryRun observation = do
  let status = SettlementMonitor.soStatus observation
      digest = SettlementMonitor.soObservationDigest observation
      dependencyMask =
        SettlementMonitor.ssDependencyFailureMask status
          .|. SettlementMonitor.soHealthDependencyFailureMask observation
      healthCritical = SettlementMonitor.soCriticalFaultMask observation
      safeObservation = isLpSettlementObservationSafe observation
  if not safeObservation
    then
      logWarnEvery
        60
        "lp_settlement_observation_unsafe"
        "LP settlement stopped because the pinned observation was incomplete or unhealthy"
        [ field "epoch" $ SettlementMonitor.ssCurrentEpoch status
        , field "observed_block" $ SettlementMonitor.ssObservedBlock status
        , field "observation_complete" $ SettlementMonitor.soObservationComplete observation
        , field "health_state" $ SettlementMonitor.soHealthState observation
        , field "critical_fault_mask" healthCritical
        , field "dependency_failure_mask" dependencyMask
        , field "operational_blocker_mask" $ SettlementMonitor.ssOperationalBlockerMask status
        ]
    else do
      recordLpSettlementObservation
        conn
        (cfgPerpsChainId cfg)
        (cfgPerpsSettlementMonitorLens cfg)
        digest
        (SettlementMonitor.ssCurrentEpoch status)
        (SettlementMonitor.ssObservedBlock status)
        (executionPathNumber $ SettlementMonitor.ssRequiredExecutionPath status)
        (SettlementMonitor.ssOperationalBlockerMask status)
        (SettlementMonitor.ssWarningMask status)
        dependencyMask
        healthCritical
      transactionResult <- buildLpSettlementTransaction cfg conn client status
      case transactionResult of
        Left err -> do
          markLpSettlementAttemptStatus
            conn
            (cfgPerpsChainId cfg)
            (cfgPerpsSettlementMonitorLens cfg)
            digest
            "blocked"
            (Just err)
          logWarnEvery
            60
            "lp_settlement_transaction_unavailable"
            "LP settlement could not prepare its canonical transaction"
            [ field "epoch" $ SettlementMonitor.ssCurrentEpoch status
            , field "execution_path" $ show $ SettlementMonitor.ssRequiredExecutionPath status
            , field "error" err
            ]
        Right (target, value, callData) ->
          executeLpSettlementTransaction cfg conn client dryRun observation target value callData

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
  SettlementMonitor.soObservationComplete observation
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

validateAtomicSettlementPayload
  :: Integer
  -> [Integer]
  -> [ByteString]
  -> Either Text ()
validateAtomicSettlementPayload minimumPublishTime publishTimes updateData
  | length publishTimes /= 6 || length updateData /= 6 =
      Left "the latest Pyth payload does not contain exactly six feeds"
  | any (< minimumPublishTime) publishTimes =
      Left "the latest Pyth payload predates the minimum atomic publish time"
  | otherwise = Right ()

executeLpSettlementTransaction
  :: Config
  -> Connection
  -> EthClient
  -> Bool
  -> SettlementMonitor.SettlementObservation
  -> Text
  -> Integer
  -> ByteString
  -> IO ()
executeLpSettlementTransaction cfg conn client dryRun observation target value callData = do
  let digest = SettlementMonitor.soObservationDigest observation
      status = SettlementMonitor.soStatus observation
  simulation <- simulateKeeperTransaction cfg client target value callData
  case simulation of
    Left err -> do
      markLpSettlementAttemptStatus
        conn
        (cfgPerpsChainId cfg)
        (cfgPerpsSettlementMonitorLens cfg)
        digest
        "simulation_failed"
        (Just err)
      logWarnEvery
        60
        "lp_settlement_simulation_failed"
        "The exact LP settlement transaction failed simulation"
        [ field "epoch" $ SettlementMonitor.ssCurrentEpoch status
        , field "target" target
        , field "value_wei" value
        , field "error" err
        ]
    Right estimatedGas ->
      if dryRun
        then do
          markLpSettlementAttemptStatus
            conn
            (cfgPerpsChainId cfg)
            (cfgPerpsSettlementMonitorLens cfg)
            digest
            "dry_run"
            Nothing
          logInfo
            "lp_settlement_dry_run_complete"
            "LP settlement dry-run completed all reads, payload selection, fee quoting, and simulation"
            [ field "epoch" $ SettlementMonitor.ssCurrentEpoch status
            , field "observed_block" $ SettlementMonitor.ssObservedBlock status
            , field "execution_path" $ show $ SettlementMonitor.ssRequiredExecutionPath status
            , field "target" target
            , field "value_wei" value
            , field "estimated_gas" estimatedGas
            , field "observation_digest" digest
            ]
        else do
          result <-
            submitKeeperTransactionTo cfg client target value callData $ \txHash -> do
              markLpSettlementAttemptSubmitted
                conn
                (cfgPerpsChainId cfg)
                (cfgPerpsSettlementMonitorLens cfg)
                digest
                txHash
              logInfo
                "lp_settlement_broadcast"
                "LP settlement transaction was broadcast"
                [ field "epoch" $ SettlementMonitor.ssCurrentEpoch status
                , field "transaction_hash" txHash
                , field "observation_digest" digest
                ]
          case result of
            Left (wasBroadcast, err) -> do
              unless wasBroadcast $
                markLpSettlementAttemptStatus
                  conn
                  (cfgPerpsChainId cfg)
                  (cfgPerpsSettlementMonitorLens cfg)
                  digest
                  "failed"
                  (Just err)
              logWarnEvery
                60
                "lp_settlement_submission_failed"
                "LP settlement was not confirmed"
                [ field "epoch" $ SettlementMonitor.ssCurrentEpoch status
                , field "broadcast" wasBroadcast
                , field "error" err
                ]
            Right receipt -> do
              let terminalStatus = if receiptSucceeded receipt then "success" else "reverted"
              markLpSettlementAttemptStatus
                conn
                (cfgPerpsChainId cfg)
                (cfgPerpsSettlementMonitorLens cfg)
                digest
                terminalStatus
                (if receiptSucceeded receipt then Nothing else Just "settlement transaction reverted")
              logInfo
                "lp_settlement_receipt"
                "LP settlement transaction reached a terminal receipt"
                [ field "epoch" $ SettlementMonitor.ssCurrentEpoch status
                , field "transaction_hash" $ receiptTxHash receipt
                , field "block_number" $ receiptBlockNumber receipt
                , field "succeeded" $ receiptSucceeded receipt
                ]
              logRemainingLpBacklog cfg client

reconcileLpSettlementAttempts :: Config -> Connection -> EthClient -> IO Bool
reconcileLpSettlementAttempts cfg conn client = do
  submitted <-
    getSubmittedLpSettlementAttempts
      conn
      (cfgPerpsChainId cfg)
      (cfgPerpsSettlementMonitorLens cfg)
  pendingFlags <- traverse reconcile submitted
  pure $ or pendingFlags
 where
  reconcile row =
    case lsarTransactionHash row of
      Nothing -> do
        markLpSettlementAttemptStatus
          conn
          (cfgPerpsChainId cfg)
          (cfgPerpsSettlementMonitorLens cfg)
          (lsarObservationDigest row)
          "failed"
          (Just "submitted settlement attempt has no transaction hash")
        pure False
      Just txHash -> do
        receiptResult <- ethGetTransactionReceipt client txHash
        case receiptResult of
          Left err -> do
            logWarnEvery
              60
              "lp_settlement_reconciliation_failed"
              "A submitted LP settlement receipt could not be reconciled"
              [ field "transaction_hash" txHash
              , field "error" $ rpcErrorText err
              ]
            pure True
          Right Nothing -> pure True
          Right (Just receipt) -> do
            markLpSettlementAttemptStatus
              conn
              (cfgPerpsChainId cfg)
              (cfgPerpsSettlementMonitorLens cfg)
              (lsarObservationDigest row)
              (if receiptSucceeded receipt then "success" else "reverted")
              (if receiptSucceeded receipt then Nothing else Just "settlement transaction reverted")
            logInfo
              "lp_settlement_reconciled"
              "A previously submitted LP settlement reached a terminal receipt"
              [ field "transaction_hash" txHash
              , field "succeeded" $ receiptSucceeded receipt
              , field "block_number" $ receiptBlockNumber receipt
              ]
            pure False

logRemainingLpBacklog :: Config -> EthClient -> IO ()
logRemainingLpBacklog cfg client = do
  epochResult <- SettlementMonitor.getCurrentEpoch client (cfgPerpsHousePool cfg)
  case epochResult of
    Left _ -> pure ()
    Right epoch -> do
      statusResult <-
        SettlementMonitor.getSettlementStatus
          client
          (cfgPerpsSettlementMonitorLens cfg)
          epoch
      case statusResult of
        Right status | SettlementMonitor.ssHasMaturedWork status ->
          logInfoEvery
            15
            "lp_settlement_backlog_remaining"
            "Matured LP work remains and will be handled on a later poll"
            [ field "epoch" epoch
            , field "execution_path" $ show $ SettlementMonitor.ssRequiredExecutionPath status
            ]
        _ -> pure ()

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
          routerLogsResult <-
            ethGetLogs
              client
              (cfgPerpsOrderRouter cfg)
              Perps.perpsOrderTopics
              startBlock
              endBlock
          lifecycleLogsResult <-
            case cfgPerpsOrderLifecycleBook cfg of
              Nothing -> pure $ Right []
              Just lifecycleBook ->
                ethGetLogs
                  client
                  lifecycleBook
                  [Perps.intentRegisteredTopic, Perps.orderFinalizedTopic]
                  startBlock
                  endBlock
          case (routerLogsResult, lifecycleLogsResult) of
            (Left err, _) ->
              logWarnEvery
                60
                "keeper_order_logs_fetch_failed"
                "Keeper could not fetch order logs"
                [ field "from_block" startBlock
                , field "to_block" endBlock
                , field "error" $ rpcErrorText err
                ]
            (_, Left err) ->
              logWarnEvery
                60
                "keeper_order_lifecycle_logs_fetch_failed"
                "Keeper could not fetch lifecycle-book logs"
                [ field "from_block" startBlock
                , field "to_block" endBlock
                , field "error" $ rpcErrorText err
                ]
            (Right routerLogs, Right lifecycleLogs) -> do
              let logs = routerLogs <> lifecycleLogs
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

processQueueHead :: Config -> Connection -> EthClient -> Bool -> IO ()
processQueueHead cfg conn client dryRun = do
  pending <- getPendingPerpsKeeperOrders conn (cfgPerpsOrderRouter cfg) (cfgKeeperMaxBatchSize cfg)
  case pending of
    [] -> pure ()
    headOrder : _ -> do
      maxAgeResult <- Perps.maxOrderAge client (cfgPerpsOrderRouter cfg)
      settlementWindowResult <- Perps.orderSettlementWindow client (cfgPerpsPletherOracle cfg)
      chainNowResult <- ethLatestBlockTimestamp client
      latestBlockResult <- ethBlockNumber client
      case (maxAgeResult, settlementWindowResult, chainNowResult, latestBlockResult) of
        (Right maxAge, Right settlementWindow, Right chainNow, Right latestBlock) ->
          decideExecution cfg conn client dryRun pending headOrder maxAge settlementWindow chainNow latestBlock
        _ -> do
          let errors =
                [ either (Just . rpcErrorText) (const Nothing) maxAgeResult
                , either (Just . rpcErrorText) (const Nothing) settlementWindowResult
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
  -> Integer
  -> IO ()
decideExecution cfg conn client dryRun pending headOrder maxAge settlementWindow chainNow latestBlock = do
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
    Right FreshPendingOrder {fpoOrder = freshHead, fpoIsClose = freshHeadIsClose}
      | not (isPastCommitBlock latestBlock freshHead) ->
          logInfoEvery
            300
            "keeper_waiting_for_post_commit_block"
            "Queue head is waiting for a post-commit block"
            [ field "order_id" $ pkorOrderId freshHead
            , field "commit_block" $ pkorCommitBlock freshHead
            , field "chain_head_block" latestBlock
            ]
      | isOrderExpired chainNow maxAge freshHead ->
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
          executeReadyHead (freshHead : drop 1 pending) freshHead freshHeadIsClose
  where
    executeReadyHead pendingWithFreshHead freshHead freshHeadIsClose = do
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
          executeHistoricalReadyHead pendingWithFreshHead freshHead

    executeHistoricalReadyHead pendingWithFreshHead freshHead = do
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
                  let candidates =
                        selectBatchCandidates
                          chainNow
                          latestBlock
                          maxAge
                          settlementWindow
                          publishTimes
                          (cfgKeeperMaxBatchSize cfg)
                          pendingWithFreshHead
                  refreshed <- refreshContiguousOrders cfg client candidates
                  let selected =
                        selectBatchCandidates
                          chainNow
                          latestBlock
                          maxAge
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

refreshPendingOrder :: Config -> EthClient -> PerpsKeeperOrderRow -> IO (Either Text FreshPendingOrder)
refreshPendingOrder cfg client order = do
  viewResult <- Perps.getPendingOrderView client (cfgPerpsOrderRouter cfg) (pkorOrderId order)
  pure $ case viewResult of
    Right view | Perps.povOrderId view == pkorOrderId order ->
      Right
        FreshPendingOrder
          { fpoOrder =
              order
                { pkorSide = Perps.povSide view
                , pkorCommitBlock = Perps.povCommitBlock view
                , pkorCommitTime = Perps.povCommitTime view
                }
          , fpoIsClose = Perps.povIsClose view
          }
    Right view ->
      Left $
        "router returned pending order "
          <> T.pack (show $ Perps.povOrderId view)
          <> " while re-reading order "
          <> T.pack (show $ pkorOrderId order)
    Left err ->
      Left $
        "could not re-read pending order "
          <> T.pack (show $ pkorOrderId order)
          <> ": "
          <> rpcErrorText err

refreshContiguousOrders :: Config -> EthClient -> [PerpsKeeperOrderRow] -> IO [PerpsKeeperOrderRow]
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
    Right freshOrder -> (fpoOrder freshOrder :) <$> refreshContiguousOrders cfg client orders

submitIntent :: Config -> Connection -> EthClient -> Bool -> ExecutionIntent -> IO ()
submitIntent cfg conn client dryRun intent = do
  let targetOrders = intentOrders intent
      targetIds = map pkorOrderId targetOrders
      callData =
        case intent of
          CleanupExpired order -> Perps.executeOrderCall (pkorOrderId order) []
          ExecuteReady [order] _ _ updateData -> Perps.executeOrderCall (pkorOrderId order) updateData
          ExecuteReady orders _ _ updateData ->
            Perps.executeOrderBatchCall (maximum $ map pkorOrderId orders) updateData
  valueResult <- intentValue cfg client intent
  case valueResult of
    Left err -> recordAllErrors cfg conn targetIds err
    Right value ->
      if dryRun
        then
          logInfo
            "keeper_transaction_dry_run"
            "Keeper dry-run prepared a transaction"
            [ field "intent" $ describeIntent intent
            , field "order_ids" targetIds
            , field "value_wei" $ show value
            ]
        else do
          forM_ targetIds (recordPerpsKeeperOrderAttempt conn (cfgPerpsOrderRouter cfg))
          sent <- submitKeeperTransaction cfg client value callData
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

submitKeeperTransaction :: Config -> EthClient -> Integer -> ByteString -> IO (Either Text TxReceipt)
submitKeeperTransaction cfg client value callData = do
  result <-
    submitKeeperTransactionTo
      cfg
      client
      (cfgPerpsOrderRouter cfg)
      value
      callData
      (const $ pure ())
  pure $ either (Left . snd) Right result

simulateKeeperTransaction
  :: Config
  -> EthClient
  -> Text
  -> Integer
  -> ByteString
  -> IO (Either Text Integer)
simulateKeeperTransaction cfg client target value callData =
  case cfgKeeperPrivateKey cfg of
    Nothing -> pure $ Left "KEEPER_PRIVATE_KEY is not configured"
    Just privateKey ->
      deriveAddress privateKey >>= \case
        Left err -> pure $ Left err
        Right fromAddr -> do
          gasResult <- ethEstimateGas client fromAddr target value callData
          pure $ either (Left . rpcErrorText) Right gasResult

submitKeeperTransactionTo
  :: Config
  -> EthClient
  -> Text
  -> Integer
  -> ByteString
  -> (Text -> IO ())
  -> IO (Either (Bool, Text) TxReceipt)
submitKeeperTransactionTo cfg client target value callData onBroadcast =
  case cfgKeeperPrivateKey cfg of
    Nothing -> pure $ Left (False, "KEEPER_PRIVATE_KEY is not configured")
    Just privateKey ->
      deriveAddress privateKey >>= \case
        Left err -> pure $ Left (False, err)
        Right fromAddr -> do
          nonceResult <- ethGetTransactionCount client fromAddr
          gasResult <- ethEstimateGas client fromAddr target value callData
          gasPriceResult <- ethGasPrice client
          priorityResult <- ethMaxPriorityFeePerGas client
          case (nonceResult, gasResult, gasPriceResult) of
            (Right nonce, Right estimatedGas, Right gasPrice) -> do
              let priorityBase = fromRight gasPrice priorityResult
                  maxFeeBase = max gasPrice priorityBase
                  gasLimit = max 21_000 $ applyBuffer estimatedGas (cfgKeeperGasBufferBps cfg)
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

isOrderExpired :: Integer -> Integer -> PerpsKeeperOrderRow -> Bool
isOrderExpired chainNow maxAge order =
  maxAge > 0 && chainNow > pkorCommitTime order + maxAge

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
  -> Integer -- max order age
  -> Integer -- settlement window
  -> [Integer] -- payload publish times
  -> Int -- max batch size
  -> [PerpsKeeperOrderRow]
  -> [PerpsKeeperOrderRow]
selectBatchCandidates chainNow currentBlock maxAge settlementWindow publishTimes maxBatchSize =
  take maxBatchSize
    . takeWhile
      ( \order ->
          isPastCommitBlock currentBlock order
            && ( isOrderExpired chainNow maxAge order
                  || isOrderRevealReady settlementWindow publishTimes order
               )
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
