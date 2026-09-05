module Plether.AA.Reconciler
  ( AaReconcilerConfig (..)
  , BlockHeader (..)
  , UserOperationEvent (..)
  , loadAaReconcilerConfig
  , runAaReconciler
  , parseUserOperationEvent
  , agreeUserOperationLogs
  , validateTargetTimestamp
  , validateSafeHeadFreshness
  , boundariesRemainCanonical
  , validateDeploymentAnchor
  ) where

import Control.Concurrent (threadDelay)
import Control.Monad (foldM, forever, unless, when)
import Data.Aeson (Value (..), object, toJSON, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.IORef (IORef, atomicModifyIORef', newIORef, writeIORef)
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.Vector as V
import Database.PostgreSQL.Simple (Connection, Only (..), query_)
import Plether.Database (DbPool, withDb, withDbAdvisoryLock)
import Plether.Database.AaSponsorship
  ( AaReconcilerCursor (..)
  , SponsorshipAuthorization (..)
  , aaSponsorshipStateIsEmpty
  , advanceAaReconcilerCursor
  , cancelStaleUnsignedReservations
  , expireSponsorshipsThrough
  , getAaReconcilerCursor
  , getSponsorshipByUserOperationHash
  , initializeAaReconcilerCursor
  , recordAaReconcilerHeartbeat
  , pauseAaIssuance
  , pruneAaRateWindows
  , pruneExpiredRecoveryOperations
  , settleSponsorship
  )
import Plether.Ethereum.Abi
  ( decodeUint256
  , encodeAddress
  , encodeCall
  , keccak256
  )
import Plether.Ethereum.Client
  ( CallParams (..)
  , EthClient
  , ethCall
  , rpcCall
  )
import Plether.Logging
  ( LogField
  , field
  , logError
  , logInfo
  , logInfoEvery
  , logWarnEvery
  )
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

data AaReconcilerConfig = AaReconcilerConfig
  { arcChainId :: Integer
  , arcPaymaster :: Text
  , arcPaymasterCodeHash :: Text
  , arcStartBlock :: Integer
  , arcStartBlockHash :: Text
  , arcPollSeconds :: Int
  , arcBatchBlocks :: Integer
  , arcMinDepositWei :: Integer
  , arcFailurePauseSeconds :: Integer
  , arcMaxSafeLagSeconds :: Integer
  }
  deriving stock (Eq, Show)

data BlockHeader = BlockHeader
  { bhNumber :: Integer
  , bhHash :: Text
  , bhTimestamp :: Integer
  }
  deriving stock (Eq, Show)

data UserOperationEvent = UserOperationEvent
  { uoeHash :: Text
  , uoeSender :: Text
  , uoeNonce :: Integer
  , uoeTransactionHash :: Text
  , uoeBlockNumber :: Integer
  , uoeBlockHash :: Text
  , uoeLogIndex :: Integer
  , uoeSuccess :: Bool
  , uoeActualGasCost :: Integer
  , uoeActualGasUsed :: Integer
  , uoeRaw :: Value
  }
  deriving stock (Eq, Show)

data FatalFailure
  = UnknownOperation Text
  | CursorDiscontinuity Text
  | CostExceedsReservation Text Integer Integer
  | InvalidOwnPaymasterEvent Text
  | RpcChainMismatch Integer
  | ProviderDisagreement Text
  | InvalidSafeTimestamp Text
  deriving stock (Eq, Show)

data StepResult
  = StepAdvanced BlockHeader Int
  | StepCaughtUp BlockHeader
  | StepRetry Text
  | StepFatal FatalFailure

data ProviderResult a
  = ProviderUnavailable Text
  | ProviderMismatch Text
  | ProviderAgreed a

loadAaReconcilerConfig :: IO (Either Text AaReconcilerConfig)
loadAaReconcilerConfig = do
  mChainId <- lookupEnv "PERPS_CHAIN_ID"
  mPaymaster <- lookupEnv "AA_PAYMASTER_ADDRESS"
  mPaymasterCodeHash <- lookupEnv "AA_PAYMASTER_CODE_HASH"
  mStartBlock <- lookupEnv "AA_RECONCILER_START_BLOCK"
  mStartBlockHash <- lookupEnv "AA_RECONCILER_START_BLOCK_HASH"
  pollRaw <- fromMaybe "5" <$> lookupEnv "AA_RECONCILER_POLL_SECONDS"
  batchRaw <- fromMaybe "1000" <$> lookupEnv "AA_RECONCILER_BATCH_BLOCKS"
  minDepositRaw <-
    fromMaybe "50000000000000000" <$> lookupEnv "AA_PAYMASTER_MIN_DEPOSIT_WEI"
  failurePauseRaw <-
    fromMaybe "30" <$> lookupEnv "AA_RECONCILER_FAILURE_PAUSE_SECONDS"
  maxSafeLagRaw <-
    fromMaybe "600" <$> lookupEnv "AA_RECONCILER_MAX_SAFE_LAG_SECONDS"
  pure $ do
    chain <- maybe (Left "PERPS_CHAIN_ID is required") (parseDecimal "PERPS_CHAIN_ID") mChainId
    unless (chain == 421614) $ Left "AA reconciler supports only PERPS_CHAIN_ID=421614"
    paymaster <- maybe (Left "AA_PAYMASTER_ADDRESS is required") (parseAddress . T.pack) mPaymaster
    paymasterCodeHash <- maybe (Left "AA_PAYMASTER_CODE_HASH is required") (parseHash "AA_PAYMASTER_CODE_HASH" . T.pack) mPaymasterCodeHash
    startBlock <- maybe (Left "AA_RECONCILER_START_BLOCK is required") (parseDecimal "AA_RECONCILER_START_BLOCK") mStartBlock
    unless (startBlock > 0) $ Left "AA_RECONCILER_START_BLOCK must be greater than zero"
    startBlockHash <- maybe (Left "AA_RECONCILER_START_BLOCK_HASH is required") (parseHash "AA_RECONCILER_START_BLOCK_HASH" . T.pack) mStartBlockHash
    poll <- parseDecimal "AA_RECONCILER_POLL_SECONDS" pollRaw
    unless (poll >= 1 && poll <= 60) $ Left "AA_RECONCILER_POLL_SECONDS must be between 1 and 60"
    batch <- parseDecimal "AA_RECONCILER_BATCH_BLOCKS" batchRaw
    unless (batch >= 1 && batch <= 10_000) $ Left "AA_RECONCILER_BATCH_BLOCKS must be between 1 and 10000"
    minDeposit <- parseDecimal "AA_PAYMASTER_MIN_DEPOSIT_WEI" minDepositRaw
    unless (minDeposit > 0) $ Left "AA_PAYMASTER_MIN_DEPOSIT_WEI must be positive"
    failurePause <- parseDecimal "AA_RECONCILER_FAILURE_PAUSE_SECONDS" failurePauseRaw
    unless (failurePause >= 5 && failurePause <= 300) $
      Left "AA_RECONCILER_FAILURE_PAUSE_SECONDS must be between 5 and 300"
    maxSafeLag <- parseDecimal "AA_RECONCILER_MAX_SAFE_LAG_SECONDS" maxSafeLagRaw
    unless (maxSafeLag >= 60 && maxSafeLag <= 3600) $
      Left "AA_RECONCILER_MAX_SAFE_LAG_SECONDS must be between 60 and 3600"
    Right $
      AaReconcilerConfig
        chain
        paymaster
        paymasterCodeHash
        startBlock
        startBlockHash
        (fromInteger poll)
        batch
        minDeposit
        failurePause
        maxSafeLag

runAaReconciler :: DbPool -> EthClient -> EthClient -> AaReconcilerConfig -> IO ()
runAaReconciler pool primaryClient secondaryClient cfg = do
  failureSince <- newIORef Nothing
  withDb pool $ \leaderConnection ->
    withDbAdvisoryLock leaderConnection reconcilerLeaderLock $
      forever $ do
        assertLeaderLockHeld leaderConnection
        cycleResult <- reconcileCycle pool primaryClient secondaryClient cfg
        case cycleResult of
          Right () -> writeIORef failureSince Nothing
          Left err -> handleTransientFailure pool cfg failureSince err
        threadDelay $ arcPollSeconds cfg * 1_000_000

assertLeaderLockHeld :: Connection -> IO ()
assertLeaderLockHeld connection = do
  rows <- query_ connection
    "SELECT EXISTS (SELECT 1 FROM pg_locks \
    \WHERE locktype='advisory' AND pid=pg_backend_pid() AND granted)" :: IO [Only Bool]
  unless (rows == [Only True]) $
    fail "AA reconciler leadership advisory lock is no longer held"

reconcileCycle
  :: DbPool
  -> EthClient
  -> EthClient
  -> AaReconcilerConfig
  -> IO (Either Text ())
reconcileCycle pool primaryClient secondaryClient cfg = do
  primaryChain <- readRpcChainId primaryClient
  secondaryChain <- readRpcChainId secondaryClient
  case (primaryChain, secondaryChain) of
    (Left err, _) -> pure $ Left $ "primary provider: " <> err
    (_, Left err) -> pure $ Left $ "secondary provider: " <> err
    (Right actualChain, _)
      | actualChain /= arcChainId cfg -> handleFatal pool $ RpcChainMismatch actualChain
    (_, Right actualChain)
      | actualChain /= arcChainId cfg -> handleFatal pool $ RpcChainMismatch actualChain
    (Right _, Right _) -> reconcileOnAttestedChain pool primaryClient secondaryClient cfg

reconcileOnAttestedChain
  :: DbPool
  -> EthClient
  -> EthClient
  -> AaReconcilerConfig
  -> IO (Either Text ())
reconcileOnAttestedChain pool primaryClient secondaryClient cfg = do
  primaryHealth <- paymasterHealth primaryClient cfg
  secondaryHealth <- paymasterHealth secondaryClient cfg
  case (primaryHealth, secondaryHealth) of
    (Left err, _) -> pure $ Left $ "primary provider paymaster health: " <> err
    (_, Left err) -> pure $ Left $ "secondary provider paymaster health: " <> err
    (Right firstHealth, Right secondHealth) -> do
      -- Deposit/stake calls are made against each provider's current head and
      -- can legitimately straddle a block.  Use the conservative intersection
      -- instead of treating a moving deposit as a chain-integrity mismatch.
      let (firstDeposit, firstStaked, firstStake) = firstHealth
          (secondDeposit, secondStaked, secondStake) = secondHealth
          healthTuple =
            ( min firstDeposit secondDeposit
            , firstStaked && secondStaked
            , min firstStake secondStake
            )
          health = Right healthTuple
          (deposit, staked, stake) = healthTuple
      when (deposit < arcMinDepositWei cfg) $ do
        withDb pool $ \conn ->
          pauseAaIssuance conn "paymaster EntryPoint deposit is below the configured minimum"
        logError
          "aa_reconciler_paymaster_low_deposit"
          "Paymaster deposit is below the issuance threshold"
          [ field "deposit_wei" deposit
          , field "minimum_deposit_wei" $ arcMinDepositWei cfg
          ]
      unless staked $
        logWarnEvery
          60
          "aa_reconciler_paymaster_unstaked"
          "Paymaster is not currently staked in both reconciliation providers"
          [field "stake_wei" stake]
      step <- reconcileStep pool primaryClient secondaryClient cfg
      case step of
        StepRetry err -> pure $ Left err
        StepCaughtUp safeHeader -> do
          -- A prior process may have committed the cursor and crashed before
          -- releasing expired liabilities.  Re-run idempotent housekeeping at
          -- the verified boundary before claiming a fresh healthy heartbeat.
          _ <- withDb pool $ \conn ->
            expireSponsorshipsThrough conn $ bhTimestamp safeHeader
          _ <- withDb pool cancelStaleUnsignedReservations
          _ <- withDb pool pruneAaRateWindows
          _ <- withDb pool pruneExpiredRecoveryOperations
          withDb pool $ \conn ->
            recordAaReconcilerHeartbeat
              conn
              (arcChainId cfg)
              (arcPaymaster cfg)
              (bhNumber safeHeader)
              (bhHash safeHeader)
          logInfoEvery
            60
            "aa_reconciler_heartbeat"
            "AA reconciler is caught up to the dual-provider safe boundary"
            (healthFields health <> [field "safe_block" $ bhNumber safeHeader])
          pure $ Right ()
        StepAdvanced header eventCount -> do
          -- Advancing one historical batch is progress, not proof that the
          -- entire advertised safe range has been scanned. Only StepCaughtUp
          -- may refresh the issuance-authorizing heartbeat.
          logInfo
            "aa_reconciler_safe_block_advanced"
            "AA reconciler advanced its dual-provider verified safe cursor"
            [ field "safe_block" $ bhNumber header
            , field "safe_block_hash" $ bhHash header
            , field "event_count" eventCount
            ]
          pure $ Right ()
        StepFatal failure -> handleFatal pool failure

handleTransientFailure
  :: DbPool
  -> AaReconcilerConfig
  -> IORef (Maybe Integer)
  -> Text
  -> IO ()
handleTransientFailure pool cfg failureSince reason = do
  now <- floor <$> getPOSIXTime
  started <- atomicModifyIORef' failureSince $ \existing ->
    let firstFailure = fromMaybe now existing
     in (Just firstFailure, firstFailure)
  let unavailableSeconds = max 0 $ now - started
  logWarnEvery
    30
    "aa_reconciler_rpc_unavailable"
    "Dual-provider reconciliation could not complete"
    [ field "reason" reason
    , field "unavailable_seconds" unavailableSeconds
    ]
  when (unavailableSeconds >= arcFailurePauseSeconds cfg) $ do
    withDb pool $ \conn ->
      pauseAaIssuance conn "dual-provider reconciliation unavailable beyond configured threshold"
    logError
      "aa_reconciler_failure_threshold_exceeded"
      "Native sponsorship issuance was durably paused after reconciliation failures"
      [ field "unavailable_seconds" unavailableSeconds
      , field "threshold_seconds" $ arcFailurePauseSeconds cfg
      ]

reconcileStep :: DbPool -> EthClient -> EthClient -> AaReconcilerConfig -> IO StepResult
reconcileStep pool primaryClient secondaryClient cfg = do
  safeResult <- readAgreedSafeBoundary primaryClient secondaryClient
  case safeResult of
    ProviderUnavailable err -> pure $ StepRetry err
    ProviderMismatch err -> pure $ StepFatal $ ProviderDisagreement err
    ProviderAgreed safeHeader -> do
      now <- floor <$> getPOSIXTime
      case validateSafeHeadFreshness (arcMaxSafeLagSeconds cfg) now safeHeader of
        Left reason -> pure $ StepFatal $ InvalidSafeTimestamp reason
        Right () -> do
          cursorResult <- ensureCursor pool primaryClient secondaryClient cfg
          case cursorResult of
            ProviderUnavailable err -> pure $ StepRetry err
            ProviderMismatch err -> pure $ StepFatal $ ProviderDisagreement err
            ProviderAgreed cursor
              | bhNumber safeHeader < arcSafeBlock cursor ->
                  pure $ StepFatal $ CursorDiscontinuity "safe boundary moved behind the stored cursor"
              | otherwise ->
                  reconcileFromCursor pool primaryClient secondaryClient cfg safeHeader cursor

reconcileFromCursor
  :: DbPool
  -> EthClient
  -> EthClient
  -> AaReconcilerConfig
  -> BlockHeader
  -> AaReconcilerCursor
  -> IO StepResult
reconcileFromCursor pool primaryClient secondaryClient cfg safeHeader cursor = do
  cursorBlockResult <-
    readAgreedBlock primaryClient secondaryClient $ quantity $ arcSafeBlock cursor
  case cursorBlockResult of
    ProviderUnavailable err -> pure $ StepRetry err
    ProviderMismatch err -> pure $ StepFatal $ ProviderDisagreement err
    ProviderAgreed canonicalCursor
      | T.toLower (bhHash canonicalCursor) /= T.toLower (arcSafeBlockHash cursor) ->
          pure $ StepFatal $ CursorDiscontinuity "stored cursor block hash is no longer canonical"
      | arcSafeBlock cursor == bhNumber safeHeader
          && T.toLower (bhHash safeHeader) /= T.toLower (arcSafeBlockHash cursor) ->
          pure $ StepFatal $ CursorDiscontinuity "safe boundary hash disagrees with the stored cursor"
      | arcSafeBlock cursor == bhNumber safeHeader -> do
          now <- floor <$> getPOSIXTime
          pure $ case validateTargetTimestamp now canonicalCursor safeHeader of
            Left reason -> StepFatal $ InvalidSafeTimestamp reason
            Right () -> StepCaughtUp safeHeader
      | otherwise -> do
          let targetNumber =
                min (bhNumber safeHeader) $
                  arcSafeBlock cursor + arcBatchBlocks cfg
          targetResult <- readAgreedBlock primaryClient secondaryClient $ quantity targetNumber
          case targetResult of
            ProviderUnavailable err -> pure $ StepRetry err
            ProviderMismatch err -> pure $ StepFatal $ ProviderDisagreement err
            ProviderAgreed targetHeader
              | targetNumber == bhNumber safeHeader && targetHeader /= safeHeader ->
                  pure $ StepFatal $ ProviderDisagreement "numeric target header disagrees with the attested safe boundary"
              | otherwise -> do
                  now <- floor <$> getPOSIXTime
                  case validateTargetTimestamp now canonicalCursor targetHeader of
                    Left reason -> pure $ StepFatal $ InvalidSafeTimestamp reason
                    Right () -> do
                      logsResult <-
                        readAgreedUserOperationLogs
                          primaryClient
                          secondaryClient
                          cfg
                          (arcSafeBlock cursor + 1)
                          targetNumber
                      case logsResult of
                        ProviderUnavailable err -> pure $ StepRetry err
                        ProviderMismatch err -> pure $ StepFatal $ ProviderDisagreement err
                        ProviderAgreed logs -> do
                          eventBlocks <-
                            verifyEventBlockHeaders primaryClient secondaryClient logs
                          case eventBlocks of
                            ProviderUnavailable err -> pure $ StepRetry err
                            ProviderMismatch err -> pure $ StepFatal $ ProviderDisagreement err
                            ProviderAgreed () -> do
                              verifiedSafe <- readAgreedSafeBoundary primaryClient secondaryClient
                              verifiedCursor <-
                                readAgreedBlock primaryClient secondaryClient $ quantity $ bhNumber canonicalCursor
                              verifiedTarget <-
                                readAgreedBlock primaryClient secondaryClient $ quantity targetNumber
                              case (verifiedSafe, verifiedCursor, verifiedTarget) of
                                (ProviderUnavailable err, _, _) -> pure $ StepRetry err
                                (_, ProviderUnavailable err, _) -> pure $ StepRetry err
                                (_, _, ProviderUnavailable err) -> pure $ StepRetry err
                                (ProviderMismatch err, _, _) -> pure $ StepFatal $ ProviderDisagreement err
                                (_, ProviderMismatch err, _) -> pure $ StepFatal $ ProviderDisagreement err
                                (_, _, ProviderMismatch err) -> pure $ StepFatal $ ProviderDisagreement err
                                (ProviderAgreed secondSafe, ProviderAgreed secondCursor, ProviderAgreed secondHeader)
                                  | bhNumber secondSafe < targetNumber ->
                                      pure $ StepFatal $ ProviderDisagreement "a provider safe boundary moved behind the verified target"
                                  | Left reason <- boundariesRemainCanonical canonicalCursor targetHeader secondCursor secondHeader ->
                                      pure $ StepFatal $ ProviderDisagreement reason
                                  | otherwise -> do
                                      processed <- processEvents pool logs
                                      case processed of
                                        Left fatal -> pure $ StepFatal fatal
                                        Right eventCount -> do
                                          advanced <- withDb pool $ \conn ->
                                            advanceAaReconcilerCursor
                                              conn
                                              (arcChainId cfg)
                                              (arcPaymaster cfg)
                                              cursor
                                              (AaReconcilerCursor targetNumber $ bhHash targetHeader)
                                          if not advanced
                                            then pure $ StepFatal $ CursorDiscontinuity "cursor compare-and-swap failed"
                                            else do
                                              _ <- withDb pool $ \conn ->
                                                expireSponsorshipsThrough conn $ bhTimestamp targetHeader
                                              _ <- withDb pool cancelStaleUnsignedReservations
                                              _ <- withDb pool pruneAaRateWindows
                                              _ <- withDb pool pruneExpiredRecoveryOperations
                                              pure $ StepAdvanced targetHeader eventCount

boundariesRemainCanonical
  :: BlockHeader
  -> BlockHeader
  -> BlockHeader
  -> BlockHeader
  -> Either Text ()
boundariesRemainCanonical expectedCursor expectedTarget actualCursor actualTarget = do
  unless (actualCursor == expectedCursor) $
    Left "cursor block changed while its event range was being read"
  unless (actualTarget == expectedTarget) $
    Left "target block changed while its event range was being read"

validateDeploymentAnchor
  :: Text
  -> Text
  -> BlockHeader
  -> ByteString
  -> ByteString
  -> Either Text ()
validateDeploymentAnchor expectedBlockHash expectedCodeHash deploymentHeader beforeCode deployedCode = do
  unless (T.toLower (bhHash deploymentHeader) == T.toLower expectedBlockHash) $
    Left "deployment anchor block hash does not match AA_RECONCILER_START_BLOCK_HASH"
  unless (BS.null beforeCode) $
    Left "paymaster runtime code existed before the configured deployment block"
  when (BS.null deployedCode || encodeHash deployedCode /= T.toLower expectedCodeHash) $
    Left "paymaster runtime code does not match the configured deployment hash"

ensureCursor
  :: DbPool
  -> EthClient
  -> EthClient
  -> AaReconcilerConfig
  -> IO (ProviderResult AaReconcilerCursor)
ensureCursor pool primaryClient secondaryClient cfg = do
  existing <- withDb pool $ \conn ->
    getAaReconcilerCursor conn (arcChainId cfg) (arcPaymaster cfg)
  case existing of
    Just cursor -> pure $ ProviderAgreed cursor
    Nothing -> do
      emptyState <- withDb pool aaSponsorshipStateIsEmpty
      if not emptyState
        then pure $ ProviderMismatch "reconciler cursor is missing while sponsorship economic state is nonempty"
        else do
          deployment <- readAgreedBlock primaryClient secondaryClient $ quantity $ arcStartBlock cfg
          initial <- readAgreedBlock primaryClient secondaryClient $ quantity $ arcStartBlock cfg - 1
          priorCode <-
            readAgreedRuntimeCode
              primaryClient
              secondaryClient
              (arcStartBlock cfg - 1)
              (arcPaymaster cfg)
          deployedCode <-
            readAgreedRuntimeCode
              primaryClient
              secondaryClient
              (arcStartBlock cfg)
              (arcPaymaster cfg)
          case (deployment, initial, priorCode, deployedCode) of
            (ProviderUnavailable err, _, _, _) -> pure $ ProviderUnavailable err
            (ProviderMismatch err, _, _, _) -> pure $ ProviderMismatch err
            (_, ProviderUnavailable err, _, _) -> pure $ ProviderUnavailable err
            (_, ProviderMismatch err, _, _) -> pure $ ProviderMismatch err
            (_, _, ProviderUnavailable err, _) -> pure $ ProviderUnavailable err
            (_, _, ProviderMismatch err, _) -> pure $ ProviderMismatch err
            (_, _, _, ProviderUnavailable err) -> pure $ ProviderUnavailable err
            (_, _, _, ProviderMismatch err) -> pure $ ProviderMismatch err
            (ProviderAgreed deploymentHeader, ProviderAgreed initialHeader, ProviderAgreed beforeCode, ProviderAgreed code)
              | Left reason <-
                  validateDeploymentAnchor
                    (arcStartBlockHash cfg)
                    (arcPaymasterCodeHash cfg)
                    deploymentHeader
                    beforeCode
                    code -> pure $ ProviderMismatch reason
              | otherwise ->
                  ProviderAgreed <$> withDb pool (\conn ->
                    initializeAaReconcilerCursor
                      conn
                      (arcChainId cfg)
                      (arcPaymaster cfg)
                      (arcStartBlock cfg - 1)
                      (bhHash initialHeader))

readAgreedSafeBoundary
  :: EthClient
  -> EthClient
  -> IO (ProviderResult BlockHeader)
readAgreedSafeBoundary primaryClient secondaryClient = do
  primarySafe <- readBlock primaryClient "safe"
  secondarySafe <- readBlock secondaryClient "safe"
  case (primarySafe, secondarySafe) of
    (Left err, _) -> pure $ ProviderUnavailable $ "primary safe head: " <> err
    (_, Left err) -> pure $ ProviderUnavailable $ "secondary safe head: " <> err
    (Right firstHeader, Right secondHeader) -> do
      let boundaryNumber = min (bhNumber firstHeader) (bhNumber secondHeader)
      boundary <- readAgreedBlock primaryClient secondaryClient $ quantity boundaryNumber
      pure $ case boundary of
        ProviderAgreed header
          | bhNumber firstHeader == boundaryNumber && firstHeader /= header ->
              ProviderMismatch "primary advertised safe header disagrees with its numeric header"
          | bhNumber secondHeader == boundaryNumber && secondHeader /= header ->
              ProviderMismatch "secondary advertised safe header disagrees with its numeric header"
          | otherwise -> ProviderAgreed header
        other -> other

readAgreedBlock
  :: EthClient
  -> EthClient
  -> Text
  -> IO (ProviderResult BlockHeader)
readAgreedBlock primaryClient secondaryClient blockTag = do
  primary <- readBlock primaryClient blockTag
  secondary <- readBlock secondaryClient blockTag
  pure $ case (primary, secondary) of
    (Left err, _) -> ProviderUnavailable $ "primary block read: " <> err
    (_, Left err) -> ProviderUnavailable $ "secondary block read: " <> err
    (Right firstHeader, Right secondHeader)
      | firstHeader == secondHeader -> ProviderAgreed firstHeader
      | otherwise -> ProviderMismatch $ "providers disagree on block header " <> blockTag

readAgreedRuntimeCode
  :: EthClient
  -> EthClient
  -> Integer
  -> Text
  -> IO (ProviderResult ByteString)
readAgreedRuntimeCode primaryClient secondaryClient blockNumber address = do
  primary <- readRuntimeCode primaryClient
  secondary <- readRuntimeCode secondaryClient
  pure $ case (primary, secondary) of
    (Left err, _) -> ProviderUnavailable $ "primary deployment code read: " <> err
    (_, Left err) -> ProviderUnavailable $ "secondary deployment code read: " <> err
    (Right firstCode, Right secondCode)
      | firstCode == secondCode -> ProviderAgreed firstCode
      | otherwise -> ProviderMismatch "providers disagree on deployment runtime code"
 where
  readRuntimeCode client = do
    result <- rpcCall client "eth_getCode" $
      toJSON [String address, String $ quantity blockNumber]
    pure $ case result of
      Right (String codeText) ->
        maybe (Left "eth_getCode returned invalid hex") Right $ decodeHex codeText
      _ -> Left "eth_getCode RPC failed"

readAgreedUserOperationLogs
  :: EthClient
  -> EthClient
  -> AaReconcilerConfig
  -> Integer
  -> Integer
  -> IO (ProviderResult [UserOperationEvent])
readAgreedUserOperationLogs primaryClient secondaryClient cfg fromBlock toBlock = do
  primary <- readUserOperationLogs primaryClient cfg fromBlock toBlock
  secondary <- readUserOperationLogs secondaryClient cfg fromBlock toBlock
  pure $ case (primary, secondary) of
    (Left err, _) -> ProviderUnavailable $ "primary log scan: " <> err
    (_, Left err) -> ProviderUnavailable $ "secondary log scan: " <> err
    (Right firstEvents, Right secondEvents) ->
      either ProviderMismatch ProviderAgreed $
        agreeUserOperationLogs firstEvents secondEvents

-- | Compare the complete canonical event identity while deliberately ignoring
-- provider order and provider-specific, non-consensus JSON object fields.
-- Duplicate log identities or duplicate UserOperation hashes are rejected
-- instead of being collapsed into a set.
agreeUserOperationLogs
  :: [UserOperationEvent]
  -> [UserOperationEvent]
  -> Either Text [UserOperationEvent]
agreeUserOperationLogs firstEvents secondEvents = do
  rejectDuplicates "primary" firstEvents
  rejectDuplicates "secondary" secondEvents
  let firstSorted = sortOn eventKey firstEvents
      secondSorted = sortOn eventKey secondEvents
  unless (map eventKey firstSorted == map eventKey secondSorted) $
    Left "reconciliation providers returned different own-paymaster event sets"
  Right firstSorted
 where
  rejectDuplicates label events = do
    unless (allUnique $ map eventIdentity events) $
      Left $ label <> " provider returned a duplicate log identity"
    unless (allUnique $ map (T.toLower . uoeHash) events) $
      Left $ label <> " provider returned a duplicate UserOperation hash"

  allUnique values = Set.size (Set.fromList values) == length values

  eventIdentity event = (T.toLower $ uoeBlockHash event, uoeLogIndex event)

eventKey
  :: UserOperationEvent
  -> (Text, Integer, Text, Integer, Text, Text, Integer, Bool, Integer, Integer)
eventKey event =
  ( T.toLower $ uoeBlockHash event
  , uoeLogIndex event
  , T.toLower $ uoeTransactionHash event
  , uoeBlockNumber event
  , T.toLower $ uoeHash event
  , T.toLower $ uoeSender event
  , uoeNonce event
  , uoeSuccess event
  , uoeActualGasCost event
  , uoeActualGasUsed event
  )

verifyEventBlockHeaders
  :: EthClient
  -> EthClient
  -> [UserOperationEvent]
  -> IO (ProviderResult ())
verifyEventBlockHeaders primaryClient secondaryClient events =
  foldM verifyOne (ProviderAgreed ()) $ Set.toList eventBlocks
 where
  eventBlocks =
    Set.fromList $
      map (\event -> (uoeBlockNumber event, T.toLower $ uoeBlockHash event)) events

  verifyOne failure@(ProviderUnavailable _) _ = pure failure
  verifyOne failure@(ProviderMismatch _) _ = pure failure
  verifyOne (ProviderAgreed ()) (blockNumber, expectedHash) = do
    headerResult <-
      readAgreedBlock primaryClient secondaryClient $ quantity blockNumber
    pure $ case headerResult of
      ProviderAgreed header
        | T.toLower (bhHash header) == expectedHash -> ProviderAgreed ()
        | otherwise ->
            ProviderMismatch "event block hash disagrees with the canonical numeric header"
      ProviderUnavailable err -> ProviderUnavailable err
      ProviderMismatch err -> ProviderMismatch err

validateTargetTimestamp :: Integer -> BlockHeader -> BlockHeader -> Either Text ()
validateTargetTimestamp wallClockSeconds cursorHeader targetHeader = do
  unless (bhTimestamp targetHeader >= bhTimestamp cursorHeader) $
    Left "safe target timestamp moved behind the canonical cursor timestamp"
  unless (bhTimestamp targetHeader <= wallClockSeconds + maxFutureBlockSkewSeconds) $
    Left "safe target timestamp is implausibly ahead of the reconciler clock"

validateSafeHeadFreshness :: Integer -> Integer -> BlockHeader -> Either Text ()
validateSafeHeadFreshness maxSafeLagSeconds wallClockSeconds safeHeader =
  do
    unless (bhTimestamp safeHeader >= wallClockSeconds - maxSafeLagSeconds) $
      Left "safe boundary timestamp is older than the configured maximum safe-head lag"
    unless (bhTimestamp safeHeader <= wallClockSeconds + maxFutureBlockSkewSeconds) $
      Left "safe boundary timestamp is implausibly ahead of the reconciler clock"

processEvents :: DbPool -> [UserOperationEvent] -> IO (Either FatalFailure Int)
processEvents pool = foldM processOne $ Right 0
 where
  processOne (Left failure) _ = pure $ Left failure
  processOne (Right count) event = do
    authorization <- withDb pool $ \conn ->
      getSponsorshipByUserOperationHash conn $ uoeHash event
    case authorization of
      Nothing -> pure $ Left $ UnknownOperation $ uoeHash event
      Just expected
        | T.toLower (uoeSender event) /= saSender expected
            || uoeNonce event /= saNonce expected ->
            pure $ Left $ InvalidOwnPaymasterEvent "UserOperationEvent sender or nonce does not match its authorization"
        | uoeActualGasCost event > saMaxCostWei expected ->
            pure $
              Left $
                CostExceedsReservation
                  (uoeHash event)
                  (uoeActualGasCost event)
                  (saMaxCostWei expected)
        | otherwise -> do
            settled <- withDb pool $ \conn ->
              settleSponsorship
                conn
                (saDigest expected)
                (uoeHash event)
                (uoeTransactionHash event)
                (uoeBlockNumber event)
                (uoeBlockHash event)
                (uoeSuccess event)
                (uoeActualGasCost event)
                (uoeRaw event)
            pure $ case settled of
              Left reason -> Left $ InvalidOwnPaymasterEvent reason
              Right () -> Right $ count + 1

handleFatal :: DbPool -> FatalFailure -> IO a
handleFatal pool failure = do
  let (eventName, message, fields) = case failure of
        UnknownOperation operationHash ->
          ( "aa_reconciler_unknown_operation"
          , "Observed an own-paymaster UserOperation without a durable authorization"
          , [field "user_operation_hash" operationHash]
          )
        CursorDiscontinuity reason ->
          ( "aa_reconciler_cursor_discontinuity"
          , "Safe-chain cursor continuity verification failed"
          , [field "reason" reason]
          )
        CostExceedsReservation operationHash actualCost reservedCost ->
          ( "aa_reconciler_cost_exceeds_reservation"
          , "Observed UserOperation gas cost above its signed reservation"
          , [ field "user_operation_hash" operationHash
            , field "actual_cost_wei" actualCost
            , field "reserved_cost_wei" reservedCost
            ]
          )
        InvalidOwnPaymasterEvent reason ->
          ( "aa_reconciler_unknown_operation"
          , "Own-paymaster event failed durable authorization validation"
          , [field "reason" reason]
          )
        RpcChainMismatch actualChain ->
          ( "aa_reconciler_chain_mismatch"
          , "Reconciliation RPC returned the wrong chain id"
          , [field "actual_chain_id" actualChain]
          )
        ProviderDisagreement reason ->
          ( "aa_reconciler_provider_disagreement"
          , "Independent reconciliation providers disagreed on canonical chain data"
          , [field "reason" reason]
          )
        InvalidSafeTimestamp reason ->
          ( "aa_reconciler_timestamp_invalid"
          , "Safe-chain timestamp failed monotonicity or clock validation"
          , [field "reason" reason]
          )
  withDb pool $ \conn -> pauseAaIssuance conn message
  logError eventName message fields
  ioError $ userError $ T.unpack message

readBlock :: EthClient -> Text -> IO (Either Text BlockHeader)
readBlock client blockTag = do
  result <- rpcCall client "eth_getBlockByNumber" $ toJSON [String blockTag, Bool False]
  pure $ case result of
    Left _ -> Left "eth_getBlockByNumber RPC failed"
    Right (Object blockObject) -> do
      number <- requiredQuantity blockObject "number"
      case parseQuantity blockTag of
        Just requested | number /= requested ->
          Left "eth_getBlockByNumber returned a different block than requested"
        _ -> Right ()
      blockHash <- requiredHash blockObject "hash"
      timestamp <- requiredQuantity blockObject "timestamp"
      Right $ BlockHeader number blockHash timestamp
    _ -> Left "eth_getBlockByNumber returned an invalid block"

readUserOperationLogs
  :: EthClient
  -> AaReconcilerConfig
  -> Integer
  -> Integer
  -> IO (Either Text [UserOperationEvent])
readUserOperationLogs client cfg fromBlock toBlock = do
  result <- rpcCall client "eth_getLogs" $
    toJSON
      [ object
          [ "address" .= entryPointAddress
          , "fromBlock" .= quantity fromBlock
          , "toBlock" .= quantity toBlock
          , "topics"
              .= [ String userOperationEventTopic
                 , Null
                 , Null
                 , String $ addressTopic $ arcPaymaster cfg
                 ]
          ]
      ]
  pure $ case result of
    Left _ -> Left "eth_getLogs RPC failed"
    Right (Array values) ->
      traverse
        (parseUserOperationEvent (arcPaymaster cfg) fromBlock toBlock)
        (V.toList values)
    _ -> Left "eth_getLogs returned an invalid result"

parseUserOperationEvent :: Text -> Integer -> Integer -> Value -> Either Text UserOperationEvent
parseUserOperationEvent expectedPaymaster fromBlock toBlock raw@(Object logObject) = do
  emittingAddress <- requiredText logObject "address"
  unless (T.toLower emittingAddress == entryPointAddress) $
    Left "UserOperationEvent was not emitted by the reviewed EntryPoint"
  topics <- case KM.lookup "topics" logObject of
    Just (Array values) -> Right $ V.toList values
    _ -> Left "UserOperationEvent topics are missing"
  topicTexts <- traverse expectTopic topics
  unless
    ( length topicTexts == 4
        && head topicTexts == userOperationEventTopic
        && topicTexts !! 3 == addressTopic expectedPaymaster
    ) $
    Left "UserOperationEvent topic shape is invalid"
  unless (isAddressTopic $ topicTexts !! 2) $
    Left "UserOperationEvent sender topic is not a canonical address"
  operationHash <- case topicTexts of
    _ : hashValue : _ | isFixedHex 32 hashValue -> Right $ T.toLower hashValue
    _ -> Left "UserOperationEvent operation hash is invalid"
  sender <- case topicTexts of
    _ : _ : senderTopic : _ -> topicAddress senderTopic
    _ -> Left "UserOperationEvent sender topic is invalid"
  dataText <- requiredText logObject "data"
  dataBytes <- maybe (Left "UserOperationEvent data is invalid") Right $ decodeHex dataText
  unless (BS.length dataBytes == 128) $
    Left "UserOperationEvent data must contain four ABI words"
  success <- case decodeUint256 $ word 1 dataBytes of
    0 -> Right False
    1 -> Right True
    _ -> Left "UserOperationEvent success flag is non-canonical"
  transactionHash <- requiredHash logObject "transactionHash"
  blockNumber <- requiredQuantity logObject "blockNumber"
  unless (blockNumber >= fromBlock && blockNumber <= toBlock) $
    Left "UserOperationEvent block number is outside the requested safe range"
  blockHash <- requiredHash logObject "blockHash"
  logIndex <- requiredQuantity logObject "logIndex"
  removed <- case KM.lookup "removed" logObject of
    Nothing -> Right False
    Just (Bool value) -> Right value
    _ -> Left "UserOperationEvent removed flag is invalid"
  when removed $ Left "removed UserOperationEvent appeared in a safe range"
  Right $
    UserOperationEvent
      operationHash
      sender
      (decodeUint256 $ word 0 dataBytes)
      transactionHash
      blockNumber
      blockHash
      logIndex
      success
      (decodeUint256 $ word 2 dataBytes)
      (decodeUint256 $ word 3 dataBytes)
      raw
parseUserOperationEvent _ _ _ _ = Left "UserOperationEvent must be an object"

paymasterHealth :: EthClient -> AaReconcilerConfig -> IO (Either Text (Integer, Bool, Integer))
paymasterHealth client cfg = do
  result <- ethCall client $
    CallParams entryPointAddress $
      encodeCall "getDepositInfo(address)" [encodeAddress $ arcPaymaster cfg]
  pure $ case result of
    Right bytes | BS.length bytes == 160 -> do
      let deposit = decodeUint256 $ word 0 bytes
          stakedWord = decodeUint256 $ word 1 bytes
          stake = decodeUint256 $ word 2 bytes
      staked <- case stakedWord of
        0 -> Right False
        1 -> Right True
        _ -> Left "EntryPoint returned a non-canonical staked flag"
      Right (deposit, staked, stake)
    _ -> Left "EntryPoint getDepositInfo failed"

readRpcChainId :: EthClient -> IO (Either Text Integer)
readRpcChainId client = do
  result <- rpcCall client "eth_chainId" $ toJSON ([] :: [Value])
  pure $ case result of
    Left _ -> Left "eth_chainId RPC failed"
    Right (String value) ->
      maybe (Left "eth_chainId returned an invalid quantity") Right $ parseQuantity value
    _ -> Left "eth_chainId returned an invalid result"

healthFields :: Either Text (Integer, Bool, Integer) -> [LogField]
healthFields = \case
  Left _ -> []
  Right (deposit, staked, stake) ->
    [ field "paymaster_deposit_wei" deposit
    , field "paymaster_staked" staked
    , field "paymaster_stake_wei" stake
    ]

requiredText :: KM.KeyMap Value -> Text -> Either Text Text
requiredText objectValue keyName =
  case KM.lookup (Key.fromText keyName) objectValue of
    Just (String value) -> Right value
    _ -> Left $ "missing " <> keyName

requiredHash :: KM.KeyMap Value -> Text -> Either Text Text
requiredHash objectValue keyName = do
  value <- requiredText objectValue keyName
  if isFixedHex 32 value
    then Right $ T.toLower value
    else Left $ keyName <> " is not a 32-byte hash"

requiredQuantity :: KM.KeyMap Value -> Text -> Either Text Integer
requiredQuantity objectValue keyName = do
  value <- requiredText objectValue keyName
  maybe (Left $ keyName <> " is not a canonical quantity") Right $ parseQuantity value

expectTopic :: Value -> Either Text Text
expectTopic (String value) | isFixedHex 32 value = Right $ T.toLower value
expectTopic _ = Left "event topic is not a 32-byte value"

word :: Int -> ByteString -> ByteString
word index = BS.take 32 . BS.drop (index * 32)

addressTopic :: Text -> Text
addressTopic address = "0x" <> T.replicate 24 "0" <> T.drop 2 (T.toLower address)

isAddressTopic :: Text -> Bool
isAddressTopic topic =
  isFixedHex 32 topic
    && T.take 26 (T.toLower topic) == "0x" <> T.replicate 24 "0"

topicAddress :: Text -> Either Text Text
topicAddress topic
  | isAddressTopic topic = Right $ "0x" <> T.drop 26 (T.toLower topic)
  | otherwise = Left "event topic is not a canonical address"

parseAddress :: Text -> Either Text Text
parseAddress raw =
  let value = T.toLower $ T.strip raw
   in if isFixedHex 20 value && value /= zeroAddress
        then Right value
        else Left "AA_PAYMASTER_ADDRESS must be a nonzero 20-byte address"

parseHash :: Text -> Text -> Either Text Text
parseHash name raw =
  let value = T.toLower $ T.strip raw
   in if raw == value && isFixedHex 32 value && value /= zeroHash
        then Right value
        else Left $ name <> " must be a canonical nonzero 32-byte hash"

parseDecimal :: String -> String -> Either Text Integer
parseDecimal name raw =
  let normalized = T.unpack $ T.strip $ T.pack raw
   in case readMaybe normalized of
        Just value | value >= 0 && show value == normalized -> Right value
        _ -> Left $ T.pack name <> " must be a canonical nonnegative decimal integer"

parseQuantity :: Text -> Maybe Integer
parseQuantity raw =
  let value = T.toLower raw
      digits = T.drop 2 value
   in if
        T.isPrefixOf "0x" value
          && not (T.null digits)
          && T.length digits <= 64
          && T.all isHexChar digits
          && (T.length digits == 1 || T.head digits /= '0')
        then Just $ T.foldl' (\total digit -> total * 16 + hexDigit digit) 0 digits
        else Nothing

quantity :: Integer -> Text
quantity value = "0x" <> T.pack (showHex value "")

decodeHex :: Text -> Maybe ByteString
decodeHex value
  | not (T.isPrefixOf "0x" value) || odd (T.length $ T.drop 2 value) = Nothing
  | otherwise = either (const Nothing) Just $ B16.decode $ TE.encodeUtf8 $ T.drop 2 $ T.toLower value

encodeHash :: ByteString -> Text
encodeHash bytes = "0x" <> TE.decodeUtf8 (B16.encode $ keccak256 bytes)

isFixedHex :: Int -> Text -> Bool
isFixedHex bytes value = maybe False ((== bytes) . BS.length) $ decodeHex value

isHexChar :: Char -> Bool
isHexChar char =
  (char >= '0' && char <= '9')
    || (char >= 'a' && char <= 'f')
    || (char >= 'A' && char <= 'F')

hexDigit :: Char -> Integer
hexDigit char
  | char >= '0' && char <= '9' = fromIntegral $ fromEnum char - fromEnum '0'
  | char >= 'a' && char <= 'f' = fromIntegral $ fromEnum char - fromEnum 'a' + 10
  | otherwise = fromIntegral $ fromEnum char - fromEnum 'A' + 10

zeroAddress :: Text
zeroAddress = "0x0000000000000000000000000000000000000000"

zeroHash :: Text
zeroHash = "0x" <> T.replicate 64 "0"

entryPointAddress :: Text
entryPointAddress = "0x4337084d9e255ff0702461cf8895ce9e3b5ff108"

userOperationEventTopic :: Text
userOperationEventTopic =
  "0x" <> TE.decodeUtf8 (B16.encode $ keccak256 eventSignature)
 where
  eventSignature =
    "UserOperationEvent(bytes32,address,address,uint256,bool,uint256,uint256)"

reconcilerLeaderLock :: Integer
reconcilerLeaderLock = 4_338_008_421_615

maxFutureBlockSkewSeconds :: Integer
maxFutureBlockSkewSeconds = 60

showHex :: Integer -> String -> String
showHex value suffix
  | value < 16 = ["0123456789abcdef" !! fromInteger value] <> suffix
  | otherwise = showHex (value `div` 16) (["0123456789abcdef" !! fromInteger (value `mod` 16)] <> suffix)
