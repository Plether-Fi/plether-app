module Plether.Perps.HistoryIndexer
  ( PerpsAddresses (..)
  , PerpsIndexerConfig (..)
  , PerpsIndexerMode (..)
  , IndexerIterationOutcome (..)
  , defaultPerpsAddresses
  , applyPerpsAddressEnvironment
  , perpsContractAddressesFor
  , validatePerpsIndexerReleaseConfig
  , perpsIndexerName
  , perpsV2IndexerName
  , perpsIndexerNameForLifecycleBook
  , runPerpsIndexer
  , indexerIterationDelayMicros
  , perpsEventTopics
  , parsePerpsLog
  , parseUsdcTransfer
  , transferTopic
  , RpcLog (..)
  , BlockInfo (..)
  , ParsedPerpsLog (..)
  , TradeCosts (..)
  , validateRpcLogBlockHash
  , decodeOpenTradeCosts
  , decodeCloseTradeCosts
  , decodeReplayTradeCosts
  , replayPreviewCallData
  , orderFailReasonName
  , terminalStatus
  , isMarketVolumeActivity
  , canCertifyIndexedRange
  , validateIndexedBoundary
  , validateReplayBounds
  , validateReplayLogScope
  , parseReplayLogEntry
  , parseReplayBlockNumber
  , parseReplayBlockInfo
  , parseReplayTransactionInfo
  , validateReplayStateUnchanged
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (SomeException, throwIO, try)
import qualified Plether.Perps.Manifest as Manifest
import Control.Monad (forM, forM_, forever, unless, when)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.Foldable (toList)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, isJust)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word (Word64)
import Database.PostgreSQL.Simple (Connection, withTransaction)
import GHC.Clock (getMonotonicTimeNSec)
import Network.HTTP.Client
  ( Manager
  , Request (..)
  , RequestBody (..)
  , httpLbs
  , parseRequest
  , responseBody
  , responseTimeoutMicro
  )
import Plether.Config (PerpsCandleWriteMode (..))
import Plether.Database (DbPool, withDb)
import Plether.Database.Insights (invalidateCompetitionSnapshotsForReleaseRebuild)
import Plether.Database.Candles
  ( RollupCoverage (..)
  , RollupKind (VolumeRollup)
  , advanceMarketVolumeCoverage
  , getMarketVolumeCoverageSnapshot
  , getMarketVolumeRollupSnapshot
  , getRollupCoverage
  , invalidateMarketVolumeFromBlock
  , lockMarketVolumeDataset
  , recomputeMarketVolumeHierarchyBatch
  )
import Plether.Database.Schema
  ( PerpsExecutionEvidenceRow (..)
  , assertPerpsReplayActivityExact
  , assertPerpsReplayUsdcTransferExact
  , assertPerpsReplayEventExact
  , assertPerpsReplayExpiredCleanupExact
  , assertPerpsReplayExpiredCleanupIfReadyExact
  , assertPerpsReplayOrderCommittedExact
  , assertPerpsReplayOrderTerminalExact
  , configurePerpsReplayTransaction
  , deletePerpsHistoryFromBlock
  , getPendingPerpsExecutionEvidence
  , getPerpsReplayHistorySnapshot
  , getPerpsIndexerLastBlock
  , insertPerpsExpiredCleanupActivityIfReady
  , insertPerpsActivity
  , insertPerpsEvent
  , insertPerpsUsdcTransfer
  , lockPerpsIndexerTransaction
  , lockPerpsReplayOrders
  , markPerpsExecutionEvidenceAttempt
  , setPerpsIndexerState
  , updatePerpsOrderEconomicsEvidence
  , updatePerpsOrderLifecycleIdentity
  , updatePerpsOrderLifecycleReceipt
  , updatePerpsOrderOracleEvidence
  , upsertPerpsOrderCommitted
  , upsertPerpsOrderTerminal
  )
import Plether.Ethereum.Abi (encodeAddress, encodeCall, encodeUint256)
import Plether.Ethereum.Client
  ( RpcClientOptions (..)
  , RpcError
  , newClientWithManager
  , rpcHttpExceptionText
  )
import Plether.Ethereum.Contracts.Perps (parseUniquePythUpdateData)
import Plether.Indexer.Contracts (keccak256Text)
import Plether.Logging (LogField, field, logError, logErrorEvery, logInfo, logInfoEvery, logWarn, logWarnEvery)
import Plether.Perps.IndexerOptions (ReplayOptions (..), validateReplayOptions)
import qualified Plether.Perps.IndexerFormat as IndexerFormat
import Plether.Perps.Release (validatePerpsV2ReleaseConfig)
import Plether.Perps.ExecutionOracle
  ( ExecutionOracleSnapshot (..)
  , decodeExecutionUpdateData
  , deriveExecutionOracleSnapshot
  , executionOraclePublishTimeBounds
  )
import Plether.Perps.ExecutionTrace
  ( TradeExecutionEvidence (..)
  , decodeTradeExecutionEvidence
  )
import Plether.Pyth.Basket (BasketComponent (..), basketComponents)
import Plether.Utils.Hex (hexToInteger, intToHex)
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout (timeout)

data PerpsAddresses = PerpsAddresses
  { paUsdc :: Text
  , paOrderRouter :: Text
  , paOrderLifecycleBook :: Maybe Text
  , paCfdEngine :: Text
  , paCfdEngineLens :: Text
  , paCfdEngineSettlementSidecar :: Text
  , paMarginClearinghouse :: Text
  , paPletherOracle :: Text
  }
  deriving stock (Show, Eq)

defaultPerpsAddresses :: PerpsAddresses
defaultPerpsAddresses =
  PerpsAddresses
    { paUsdc = Manifest.mockUsdcAddress
    , paOrderRouter = Manifest.orderRouterAddress
    , paOrderLifecycleBook = Just Manifest.orderLifecycleBookAddress
    , paCfdEngine = Manifest.cfdEngineAddress
    , paCfdEngineLens = Manifest.cfdEngineLensAddress
    , paCfdEngineSettlementSidecar = Manifest.cfdEngineSettlementSidecarAddress
    , paMarginClearinghouse = Manifest.marginClearinghouseAddress
    , paPletherOracle = Manifest.pletherOracleAddress
    }

perpsIndexerName :: Text
perpsIndexerName = IndexerFormat.indexerName IndexerFormat.LegacyV1

perpsV2IndexerName :: Text
perpsV2IndexerName = IndexerFormat.indexerName IndexerFormat.BoundedV2

perpsIndexerNameForLifecycleBook :: Maybe Text -> Text
perpsIndexerNameForLifecycleBook =
  IndexerFormat.indexerName . IndexerFormat.indexerFormatForLifecycleBook

-- | Apply process-level address overrides without erasing addresses already
-- resolved by the shared application configuration. The previous worker
-- overlay replaced LifecycleBook with 'Nothing' whenever its local env
-- allowlist omitted the key, even though 'loadConfig' had resolved it.
applyPerpsAddressEnvironment
  :: PerpsAddresses
  -> [(String, String)]
  -> PerpsAddresses
applyPerpsAddressEnvironment addressDefaults env =
  addressDefaults
    { paUsdc = addressOverride "PERPS_USDC" $ paUsdc addressDefaults
    , paOrderRouter = addressOverride "PERPS_ORDER_ROUTER" $ paOrderRouter addressDefaults
    , paOrderLifecycleBook =
        case lookup "PERPS_ORDER_LIFECYCLE_BOOK" env of
          Nothing -> paOrderLifecycleBook addressDefaults
          Just value -> Just $ T.pack value
    , paCfdEngine = addressOverride "PERPS_CFD_ENGINE" $ paCfdEngine addressDefaults
    , paCfdEngineLens = addressOverride "PERPS_CFD_ENGINE_LENS" $ paCfdEngineLens addressDefaults
    , paCfdEngineSettlementSidecar =
        addressOverride
          "PERPS_CFD_ENGINE_SETTLEMENT_SIDECAR"
          (paCfdEngineSettlementSidecar addressDefaults)
    , paMarginClearinghouse =
        addressOverride
          "PERPS_MARGIN_CLEARINGHOUSE"
          (paMarginClearinghouse addressDefaults)
    , paPletherOracle = addressOverride "PERPS_PLETHER_ORACLE" $ paPletherOracle addressDefaults
    }
 where
  addressOverride name fallback =
    T.pack $ fromMaybe (T.unpack fallback) $ lookup name env

-- | Sepolia is bounded-V2-only. Validate the effective worker configuration,
-- not the shared configuration that existed before worker-local overlays.
validatePerpsIndexerReleaseConfig
  :: Integer
  -> PerpsAddresses
  -> Text
  -> Integer
  -> Either Text ()
validatePerpsIndexerReleaseConfig chainId addresses housePool startBlock
  | chainId == 421614 =
      validatePerpsV2ReleaseConfig
        chainId
        (paOrderRouter addresses)
        (paOrderLifecycleBook addresses)
        (paCfdEngine addresses)
        (paMarginClearinghouse addresses)
        housePool
        startBlock
  | otherwise = Right ()

data PerpsIndexerMode
  = PerpsIndexerLoop
  | PerpsIndexerOnce
  | PerpsIndexerReplay ReplayOptions
  deriving stock (Show, Eq)

data IndexerIterationOutcome
  = IndexerProcessed
  | IndexerCaughtUp
  | IndexerFailed
  deriving stock (Show, Eq)

indexerIterationDelayMicros :: Int -> IndexerIterationOutcome -> Int
indexerIterationDelayMicros configuredDelay outcome =
  case outcome of
    IndexerFailed -> boundedDelay * 2
    IndexerProcessed -> boundedDelay
    IndexerCaughtUp -> boundedDelay
 where
  boundedDelay = max 1 configuredDelay

data PerpsIndexerConfig = PerpsIndexerConfig
  { picRpcUrls :: [Text]
  , picRpcAuthToken :: Maybe Text
  , picChainId :: Integer
  , picAddresses :: PerpsAddresses
  , picStartBlock :: Integer
  , picConfirmations :: Integer
  , picBatchSize :: Integer
  , picPollIntervalMicros :: Int
  , picIndexerName :: Text
  , picMode :: PerpsIndexerMode
  , picCandleWriteMode :: PerpsCandleWriteMode
  , picCandleLatenessSeconds :: Integer
  , picDeploymentEnvironment :: Maybe Text
  }
  deriving stock (Eq)

instance Show PerpsIndexerConfig where
  show cfg =
    "PerpsIndexerConfig {picRpcProviderCount = "
      <> show (length $ picRpcUrls cfg)
      <> ", picRpcAuthTokenConfigured = "
      <> show (isJust $ picRpcAuthToken cfg)
      <> ", picChainId = "
      <> show (picChainId cfg)
      <> ", picStartBlock = "
      <> show (picStartBlock cfg)
      <> ", picMode = "
      <> show (picMode cfg)
      <> "}"

data RpcEndpoint = RpcEndpoint
  { reUrl :: !Text
  , reBearerToken :: !(Maybe Text)
  }

indexerRpcEndpoints :: PerpsIndexerConfig -> [RpcEndpoint]
indexerRpcEndpoints cfg =
  case picRpcUrls cfg of
    [] -> []
    primary : fallbacks ->
      RpcEndpoint primary (picRpcAuthToken cfg)
        : map (`RpcEndpoint` Nothing) fallbacks

data RpcLog = RpcLog
  { rlAddress :: Text
  , rlTopics :: [ByteString]
  , rlData :: ByteString
  , rlTxHash :: Text
  , rlBlockNumber :: Integer
  , rlBlockHash :: Text
  , rlTxIndex :: Integer
  , rlLogIndex :: Integer
  }
  deriving stock (Show, Eq)

data BlockInfo = BlockInfo
  { biNumber :: Integer
  , biHash :: Text
  , biTimestamp :: Integer
  }
  deriving stock (Show, Eq)

data TransactionInfo = TransactionInfo
  { tiHash :: Text
  , tiFrom :: Text
  , tiTo :: Text
  , tiBlockHash :: Text
  , tiInput :: ByteString
  }
  deriving stock (Show, Eq)

data ParsedPerpsLog
  = ParsedOrderCommitted Integer Text Int Value
  | ParsedIntentRegistered Integer Text Text Int Value
  | ParsedOrderFinalized Integer Text Text Text Text Text Text (Maybe Text) Integer Value Value
  | ParsedOrderExecuted Integer Integer Value
  | ParsedOrderFailed Integer Int Text Value
  | ParsedPositionActivity Text Text Int (Maybe Integer) (Maybe Integer) (Maybe Integer) (Maybe Integer) Value
  | ParsedMarginActivity Text Text Integer Value
  | ParsedUsdcTransfer Text Text Integer Value
  deriving stock (Show, Eq)

data TradeCosts = TradeCosts
  { tcExecutionFeeUsdc :: Integer
  , tcVpiUsdc :: Integer
  }
  deriving stock (Show, Eq)

data PreparedReplayLog = PreparedReplayLog
  { prlLog :: RpcLog
  , prlBlockInfo :: BlockInfo
  , prlParsed :: ParsedPerpsLog
  , prlTransactionFrom :: Text
  , prlTradeCosts :: Maybe TradeCosts
  }

orderCommittedTopic :: ByteString
orderCommittedTopic = keccak256Text "OrderCommitted(uint64,address,uint8)"

orderExecutedTopic :: ByteString
orderExecutedTopic = keccak256Text "OrderExecuted(uint64,uint256)"

orderFailedTopic :: ByteString
orderFailedTopic = keccak256Text "OrderFailed(uint64,uint8)"

intentRegisteredTopic :: ByteString
intentRegisteredTopic =
  keccak256Text
    "IntentRegistered(uint64,address,bytes32,bytes32,uint256,(bytes32,uint8,uint256,uint256,uint256,bool,(uint64,uint8,bytes32,uint256,uint256,uint256,uint256,uint256,uint256,uint256,uint256,uint32)))"

orderFinalizedTopic :: ByteString
orderFinalizedTopic =
  keccak256Text
    "OrderFinalized(uint64,address,bytes32,bytes32,uint64,uint64,(uint64,address,bytes32,bytes32,bytes32,bytes32,uint8,uint8,uint8,address,uint8,uint256,uint256,uint256,uint64,bool,uint256,address,uint8,(bytes4,uint8,uint8,uint8,uint256,uint256,bytes32),(uint256,int256,int256,int256,uint256,uint256,uint256,uint256,uint256,uint256,uint256,uint256,uint256,uint256,uint256,int256,uint256)))"

positionOpenedTopic :: ByteString
positionOpenedTopic = keccak256Text "PositionOpened(address,uint8,uint256,uint256,uint256)"

positionClosedTopic :: ByteString
positionClosedTopic = keccak256Text "PositionClosed(address,uint8,uint256,uint256,int256)"

positionLiquidatedTopic :: ByteString
positionLiquidatedTopic = keccak256Text "PositionLiquidated(address,uint8,uint256,uint256,uint256)"

marginAddedTopic :: ByteString
marginAddedTopic = keccak256Text "MarginAdded(address,uint256)"

depositTopic :: ByteString
depositTopic = keccak256Text "Deposit(address,address,uint256)"

withdrawTopic :: ByteString
withdrawTopic = keccak256Text "Withdraw(address,address,uint256)"

transferTopic :: ByteString
transferTopic = keccak256Text "Transfer(address,address,uint256)"

perpsEventTopics :: [ByteString]
perpsEventTopics =
  [ orderCommittedTopic
  , intentRegisteredTopic
  , orderFinalizedTopic
  , orderExecutedTopic
  , orderFailedTopic
  , positionOpenedTopic
  , positionClosedTopic
  , positionLiquidatedTopic
  , marginAddedTopic
  , depositTopic
  , withdrawTopic
  ]

runPerpsIndexer :: Manager -> DbPool -> PerpsIndexerConfig -> IO ()
runPerpsIndexer manager pool cfg =
  case picMode cfg of
    PerpsIndexerLoop -> do
      _ <- forkIO runEvidenceLoop
      runIndexerLoop
    PerpsIndexerOnce -> do
      _ <- runOneRange manager pool cfg Nothing Nothing
      runEvidenceBatch
    PerpsIndexerReplay replayOptions -> runReplayWithAudit replayOptions
  where
    runIndexerLoop = forever $ do
      result <- try @SomeException $ runOneRange manager pool cfg Nothing Nothing
      case result of
        Left err -> do
          logErrorEvery
            60
            "perps_indexer_iteration_failed"
            "Perps indexer iteration failed"
            [field "error" $ show err]
          threadDelay $ indexerIterationDelayMicros (picPollIntervalMicros cfg) IndexerFailed
        Right indexed -> do
          -- A successful poll is the volume-writer liveness primitive. Emit it
          -- even when the indexer is already caught up: quiet chains and
          -- zero-trade ranges must not look like a dead candle writer.
          when (picCandleWriteMode cfg == PerpsCandleWritesDual) $ do
            coverageResult <-
              try @SomeException $
                withDb pool $ \conn ->
                  getRollupCoverage
                    conn
                    VolumeRollup
                    Nothing
                    (Just $ picChainId cfg)
                    (Just $ paOrderRouter $ picAddresses cfg)
                    60
            case coverageResult of
              Left err ->
                logErrorEvery
                  60
                  "perps_volume_writer_heartbeat_failed"
                  "Perps volume candle writer could not read its coverage heartbeat"
                  [field "error" $ show err]
              Right volumeCoverage -> do
                now <- round <$> getPOSIXTime
                logInfoEvery
                  300
                  "perps_volume_writer_heartbeat"
                  "Perps volume candle writer completed an indexer poll"
                  [ field "writer_kind" ("volume" :: Text)
                  , field "service" ("plether-perps-indexer" :: Text)
                  , field "processed_range" indexed
                  , field "coverage_interval_seconds" (60 :: Integer)
                  , field "coverage_expected_lateness_seconds" (0 :: Integer)
                  , field "coverage_state" $ coverageState volumeCoverage
                  , field "coverage_finalized_through" $ volumeCoverage >>= rcFinalizedThrough
                  , field "coverage_lag_seconds" $
                      normalizedCoverageLag
                        now
                        60
                        0
                        (volumeCoverage >>= rcFinalizedThrough)
                  , field "coverage_error" $ volumeCoverage >>= rcLastError
                  ]
          threadDelay $
            indexerIterationDelayMicros
              (picPollIntervalMicros cfg)
              (if indexed then IndexerProcessed else IndexerCaughtUp)

    runEvidenceLoop = forever $ do
      runEvidenceBatch
      threadDelay (max 1_000_000 $ picPollIntervalMicros cfg)

    runEvidenceBatch = do
      reqIdRef <- newIORef 1
      result <-
        try @SomeException $
          enrichPendingExecutionEvidence manager pool cfg reqIdRef
      case result of
        Left err ->
          logErrorEvery
            60
            "perps_indexer_execution_evidence_iteration_failed"
            "Optional execution-evidence enrichment failed"
            [field "error" $ show err]
        Right () -> pure ()

    runReplayWithAudit replayOptions = do
      let baseFields = replayAuditFields replayOptions
      logInfo
        "perps_indexer_replay_started"
        "Bounded Perps duplicate-ingestion replay started"
        baseFields
      result <-
        try @SomeException $
          timeout
            (roMaxRuntimeSeconds replayOptions * 1_000_000)
            (runBoundedReplay manager pool cfg replayOptions)
      case result of
        Right (Just (safeBlock, eventCount, cursorBlock)) ->
          logInfo
            "perps_indexer_replay_complete"
            "Bounded Perps duplicate-ingestion replay completed"
            ( [ field "safe_head_block" safeBlock
              , field "event_count" eventCount
              , field "cursor_block" cursorBlock
              ]
                <> baseFields
            )
        Right Nothing -> replayFailed baseFields "runtime_limit_exceeded"
        -- Never render an arbitrary exception here: PostgreSQL/network
        -- exceptions can contain connection strings. Detailed diagnosis stays
        -- in trusted task metadata; this public audit event is credential-safe.
        Left _ -> replayFailed baseFields "bounded_replay_rejected"
      where
        replayFailed :: [LogField] -> Text -> IO ()
        replayFailed baseFields err = do
          logError
            "perps_indexer_replay_failed"
            "Bounded Perps duplicate-ingestion replay failed"
            (field "error" err : baseFields)
          throwIO $ userError "Bounded Perps duplicate-ingestion replay failed"

    replayAuditFields replayOptions =
      [ field "from_block" $ roFromBlock replayOptions
      , field "to_block" $ roToBlock replayOptions
      , field "canonical_progress_certified" False
      ]

    coverageState = \case
      Nothing -> "uninitialized" :: Text
      Just coverage
        | rcComplete coverage -> "complete"
        | otherwise -> "incomplete"

    normalizedCoverageLag now interval expectedLateness =
      fmap $ \finalizedThrough ->
        max 0 (now - finalizedThrough - interval - max 0 expectedLateness)

-- Execute one exact, already bounded range as a duplicate-ingestion proof.
-- Unlike normal indexing this path cannot clip, recurse, enrich evidence,
-- advance/rewind the cursor, or publish/invalidate coverage. It commits only
-- when canonical rows were already present and recomputing every affected
-- volume bucket is a semantic no-op (including revision identity).
runBoundedReplay
  :: Manager
  -> DbPool
  -> PerpsIndexerConfig
  -> ReplayOptions
  -> IO (Integer, Int, Integer)
runBoundedReplay manager pool cfg replayOptions = do
  validatedOptions <-
    either fail pure $ validateReplayOptions replayOptions
  unless
    (picDeploymentEnvironment cfg == Just "sepolia")
    (fail "Bounded Perps replay is restricted to the Sepolia deployment")
  unless
    (picCandleWriteMode cfg == PerpsCandleWritesDual)
    (fail "Bounded Perps replay requires PERPS_CANDLE_WRITE_MODE=dual")

  reqIdRef <- newIORef 1
  currentBlock <-
    requireRpc "eth_blockNumber(replay)" $
      getReplayCurrentBlockNumber manager (indexerRpcEndpoints cfg) reqIdRef
  let safeBlock = max 0 (currentBlock - picConfirmations cfg)
      fromBlock = roFromBlock validatedOptions
      toBlock = roToBlock validatedOptions
      releaseRouter = paOrderRouter $ picAddresses cfg
  preliminaryCursor <-
    fst
      <$> withDb pool
        ( \conn ->
            getPerpsIndexerLastBlock
              conn
              (picChainId cfg)
              (picIndexerName cfg)
              releaseRouter
        )
  either (fail . T.unpack) pure $
    validateReplayBounds (picStartBlock cfg) safeBlock preliminaryCursor validatedOptions

  logs <-
    requireRpc "eth_getLogs(replay)" $
      getReplayLogs
        manager
        (indexerRpcEndpoints cfg)
        reqIdRef
        cfg
        fromBlock
        toBlock
  forM_ logs $ \logEntry ->
    either (fail . T.unpack) pure $
      validateReplayLogScope
        fromBlock
        toBlock
        (perpsAddresses cfg)
        logEntry
  let orderedLogs =
        sortOn
          (\logEntry -> (rlBlockNumber logEntry, rlTxIndex logEntry, rlLogIndex logEntry))
          logs
      logBlockNumbers =
        Map.keys $
          Map.fromList
            [ (rlBlockNumber logEntry, ())
            | logEntry <- orderedLogs
            ]

  endInfoBefore <-
    requireRpc "eth_getBlockByNumber(replay-end-before)" $
      getReplayBlockByNumber manager (indexerRpcEndpoints cfg) reqIdRef toBlock
  blockInfos <- forM logBlockNumbers $ \blockNumber -> do
    blockInfo <-
      requireRpc "eth_getBlockByNumber(replay-log)" $
        getReplayBlockByNumber manager (indexerRpcEndpoints cfg) reqIdRef blockNumber
    pure (blockNumber, blockInfo)
  let blockInfoByNumber = Map.fromList blockInfos
  validatedLogs <- forM orderedLogs $ \logEntry ->
    case Map.lookup (rlBlockNumber logEntry) blockInfoByNumber of
      Nothing ->
        fail $
          "Missing canonical block metadata for replay log block "
            <> show (rlBlockNumber logEntry)
      Just blockInfo ->
        case validateRpcLogBlockHash logEntry blockInfo of
          Left err -> fail $ T.unpack err
          Right () -> pure (logEntry, blockInfo)
  endInfo <-
    requireRpc "eth_getBlockByNumber(replay-end-after)" $
      getReplayBlockByNumber manager (indexerRpcEndpoints cfg) reqIdRef toBlock
  unless
    (normalizeHex (biHash endInfoBefore) == normalizeHex (biHash endInfo))
    (fail "Canonical replay end block changed while validating the exact range")

  preparedLogs <- forM validatedLogs $ \(logEntry, blockInfo) -> do
    either (fail . T.unpack) pure $ validateReplayLogAbi logEntry
    parsed <-
      maybe
        (fail "Allowlisted Perps replay log could not be decoded")
        pure
        (parseConfiguredLog cfg logEntry)
    txInfo <-
      requireRpc "eth_getTransactionByHash(replay)" $
        getReplayTransactionInfo manager (indexerRpcEndpoints cfg) reqIdRef (rlTxHash logEntry)
    unless
      (normalizeHex (tiHash txInfo) == normalizeHex (rlTxHash logEntry))
      (fail "Replay transaction hash does not match its canonical log")
    unless
      (normalizeHex (tiBlockHash txInfo) == normalizeHex (rlBlockHash logEntry))
      (fail "Replay transaction block hash does not match its canonical log")
    tradeCosts <- case parsed of
      ParsedPositionActivity kind _ _ _ _ _ _ _
        | kind == "Open" || kind == "Close" -> do
            costs <- getReplayTradeCosts manager cfg reqIdRef logEntry parsed
            either (fail . T.unpack) (pure . Just) costs
      _ -> pure Nothing
    pure
      PreparedReplayLog
        { prlLog = logEntry
        , prlBlockInfo = blockInfo
        , prlParsed = parsed
        , prlTransactionFrom = tiFrom txInfo
        , prlTradeCosts = tradeCosts
        }
  let affectedOrderIds =
        Set.toAscList $
          Set.fromList $
            catMaybes $
              map (parsedOrderId . prlParsed) preparedLogs
      affectedVolumeTimes =
        [ biTimestamp (prlBlockInfo prepared)
        | prepared <- preparedLogs
        , isMarketVolumeActivity (prlParsed prepared)
        ]
  committedCursor <- withDb pool $ \conn -> withTransaction conn $ do
    configurePerpsReplayTransaction
      conn
      (roStatementTimeoutMs validatedOptions)
      (roLockTimeoutMs validatedOptions)
    lockPerpsIndexerTransaction
      conn
      (picChainId cfg)
      (picIndexerName cfg)
      releaseRouter
    -- The volume dataset lock must always follow the indexer lock, matching
    -- the live writer and reorg path. It also protects explicit absence in the
    -- semantic rollup snapshot.
    lockMarketVolumeDataset conn (picChainId cfg) releaseRouter
    lockPerpsReplayOrders
      conn
      (picChainId cfg)
      releaseRouter
      affectedOrderIds

    cursorBefore@(cursorBlock, cursorHash) <-
      getPerpsIndexerLastBlock
        conn
        (picChainId cfg)
        (picIndexerName cfg)
        releaseRouter
    either (fail . T.unpack) pure $
      validateReplayBounds (picStartBlock cfg) safeBlock cursorBlock validatedOptions
    persistedCursorHash <-
      maybe
        (fail "Bounded Perps replay requires a persisted canonical cursor hash")
        pure
        cursorHash
    cursorInfo <-
      requireRpc "eth_getBlockByNumber(replay-cursor)" $
        getReplayBlockByNumber manager (indexerRpcEndpoints cfg) reqIdRef cursorBlock
    case validateIndexedBoundary cursorBlock persistedCursorHash cursorInfo of
      Left err -> fail $ T.unpack err
      Right () -> pure ()

    coverageBefore <-
      getMarketVolumeCoverageSnapshot conn (picChainId cfg) releaseRouter
    historyBefore <-
      getPerpsReplayHistorySnapshot
        conn
        (picChainId cfg)
        releaseRouter
        fromBlock
        toBlock
        affectedOrderIds
    rollupsBefore <-
      getMarketVolumeRollupSnapshot
        conn
        (picChainId cfg)
        releaseRouter
        affectedVolumeTimes

    -- Sender and trade-cost metadata were prepared fail-closed before opening
    -- the transaction. Every canonical event/order/activity write and all
    -- volume recomputations share this transaction; the independent evidence
    -- worker is deliberately not run by replay.
    forM_ preparedLogs $ \prepared -> do
      _ <- processParsedLog
        conn
        cfg
        (prlBlockInfo prepared)
        (Just $ prlTransactionFrom prepared)
        (prlTradeCosts prepared)
        (prlLog prepared)
        (prlParsed prepared)
      assertPreparedReplayLogExact conn cfg prepared
    recomputeMarketVolumeHierarchyBatch
      conn
      (picChainId cfg)
      releaseRouter
      affectedVolumeTimes
      (picCandleLatenessSeconds cfg)

    cursorAfter <-
      getPerpsIndexerLastBlock
        conn
        (picChainId cfg)
        (picIndexerName cfg)
        releaseRouter
    coverageAfter <-
      getMarketVolumeCoverageSnapshot conn (picChainId cfg) releaseRouter
    historyAfter <-
      getPerpsReplayHistorySnapshot
        conn
        (picChainId cfg)
        releaseRouter
        fromBlock
        toBlock
        affectedOrderIds
    rollupsAfter <-
      getMarketVolumeRollupSnapshot
        conn
        (picChainId cfg)
        releaseRouter
        affectedVolumeTimes
    either (fail . T.unpack) pure $
      validateReplayStateUnchanged
        cursorBefore
        cursorAfter
        coverageBefore
        coverageAfter
        historyBefore
        historyAfter
        rollupsBefore
        rollupsAfter
    pure cursorBlock
  pure (safeBlock, length orderedLogs, committedCursor)

validateReplayBounds
  :: Integer -> Integer -> Integer -> ReplayOptions -> Either Text ()
validateReplayBounds configuredStart safeBlock cursorBlock options = do
  _ <- firstText $ validateReplayOptions options
  unless
    (roFromBlock options >= configuredStart)
    (Left "Replay range begins below the configured indexer start block")
  unless
    (roToBlock options <= safeBlock)
    (Left "Replay range extends above the current confirmed safe head")
  unless
    (roToBlock options <= cursorBlock)
    (Left "Replay range extends above the persisted canonical cursor")
 where
  firstText = either (Left . T.pack) Right

validateReplayLogScope :: Integer -> Integer -> [Text] -> RpcLog -> Either Text ()
validateReplayLogScope fromBlock toBlock allowedAddresses logEntry = do
  unless
    (rlBlockNumber logEntry >= fromBlock && rlBlockNumber logEntry <= toBlock)
    (Left "RPC returned a Perps replay log outside the exact requested range")
  unless
    ( normalizeHex (rlAddress logEntry)
        `elem` map normalizeHex allowedAddresses
    )
    (Left "RPC returned a Perps replay log outside the configured address allowlist")

validateReplayLogAbi :: RpcLog -> Either Text ()
validateReplayLogAbi logEntry =
  case rlTopics logEntry of
    topic : indexedTopics
      | topic == orderCommittedTopic -> requireShape 3 32 indexedTopics
      | topic == intentRegisteredTopic -> requireShape 4 (20 * 32) indexedTopics
      | topic == orderFinalizedTopic -> requireShape 4 (46 * 32) indexedTopics
      | topic == orderExecutedTopic -> requireShape 2 32 indexedTopics
      | topic == orderFailedTopic -> requireShape 2 32 indexedTopics
      | topic == positionOpenedTopic -> requireShape 2 128 indexedTopics
      | topic == positionClosedTopic -> requireShape 2 128 indexedTopics
      | topic == positionLiquidatedTopic -> requireShape 2 128 indexedTopics
      | topic == marginAddedTopic -> requireShape 2 32 indexedTopics
      | topic == depositTopic -> requireShape 3 32 indexedTopics
      | topic == withdrawTopic -> requireShape 3 32 indexedTopics
      | topic == transferTopic -> requireShape 3 32 indexedTopics
      | otherwise -> Left "Replay log topic is not in the allowlisted release ABI"
    [] -> Left "Replay log has no event topic"
 where
  requireShape totalTopicCount dataBytes indexedTopics = do
    unless (length (rlTopics logEntry) == totalTopicCount) $
      Left "Replay log topic count does not match the allowlisted Perps ABI"
    unless (all ((== 32) . BS.length) indexedTopics) $
      Left "Replay log indexed topic is not exactly 32 bytes"
    unless (BS.length (rlData logEntry) == dataBytes) $
      Left "Replay log data length does not match the allowlisted Perps ABI"


validateReplayStateUnchanged
  :: (Eq cursor, Eq coverage, Eq history, Eq rollup)
  => cursor
  -> cursor
  -> [coverage]
  -> [coverage]
  -> history
  -> history
  -> [rollup]
  -> [rollup]
  -> Either Text ()
validateReplayStateUnchanged cursorBefore cursorAfter coverageBefore coverageAfter historyBefore historyAfter rollupsBefore rollupsAfter = do
  unless
    (cursorAfter == cursorBefore)
    (Left "Bounded replay changed the canonical indexer cursor")
  unless
    (coverageAfter == coverageBefore)
    (Left "Bounded replay changed market-volume coverage state")
  unless
    (historyAfter == historyBefore)
    (Left "Bounded replay was not an idempotent canonical-history ingestion")
  unless
    (rollupsAfter == rollupsBefore)
    (Left "Bounded replay was not an idempotent market-volume ingestion")

runOneRange :: Manager -> DbPool -> PerpsIndexerConfig -> Maybe Integer -> Maybe Integer -> IO Bool
runOneRange manager pool cfg explicitFrom explicitTo = do
  reqIdRef <- newIORef 1
  currentBlock <- requireRpc "eth_blockNumber" $ getCurrentBlockNumber manager (indexerRpcEndpoints cfg) reqIdRef
  let safeBlock = max 0 (currentBlock - picConfirmations cfg)
  (storedLastBlock, storedLastHash) <- withDb pool $ \conn ->
    getPerpsIndexerLastBlock conn (picChainId cfg) (picIndexerName cfg) (paOrderRouter $ picAddresses cfg)
  verifyCursor manager pool cfg reqIdRef storedLastBlock storedLastHash
  (lastBlock, _) <- withDb pool $ \conn ->
    getPerpsIndexerLastBlock conn (picChainId cfg) (picIndexerName cfg) (paOrderRouter $ picAddresses cfg)
  let startBlock = fromMaybe (max (picStartBlock cfg) (lastBlock + 1)) explicitFrom
      cappedToBlock = maybe safeBlock (min safeBlock) explicitTo
      endBlock = min cappedToBlock (startBlock + picBatchSize cfg - 1)
  if startBlock > endBlock
    then pure False
    else do
      logs <- requireRpc "eth_getLogs" $
        getLogs manager (indexerRpcEndpoints cfg) reqIdRef cfg startBlock endBlock
      forM_ logs $ \logEntry -> do
        either (fail . T.unpack) pure $
          validateReplayLogScope startBlock endBlock (perpsAddresses cfg) logEntry
        either (fail . T.unpack) pure $ validateReplayLogAbi logEntry
        unless (maybe False (const True) $ parseConfiguredLog cfg logEntry) $
          fail "Allowlisted release log could not be decoded for its configured contract"
      let orderedLogs =
            sortOn
              (\logEntry -> (rlBlockNumber logEntry, rlTxIndex logEntry, rlLogIndex logEntry))
              logs
          logBlockNumbers =
            Map.keys $
              Map.fromList
                [ (rlBlockNumber logEntry, ())
                | logEntry <- orderedLogs
                ]
      -- Resolve and validate the complete range before the first database
      -- write. Reading the end block on both sides of the per-block lookups
      -- detects a provider/fork switch during this snapshot.
      endInfoBefore <- requireRpc "eth_getBlockByNumber" $
        getBlockByNumber manager (indexerRpcEndpoints cfg) reqIdRef endBlock
      blockInfos <- forM logBlockNumbers $ \blockNumber -> do
        blockInfo <- requireRpc "eth_getBlockByNumber" $
          getBlockByNumber manager (indexerRpcEndpoints cfg) reqIdRef blockNumber
        pure (blockNumber, blockInfo)
      let blockInfoByNumber = Map.fromList blockInfos
      validatedLogs <- forM orderedLogs $ \logEntry ->
        case Map.lookup (rlBlockNumber logEntry) blockInfoByNumber of
          Nothing ->
            fail $
              "Missing canonical block metadata for log block "
                <> show (rlBlockNumber logEntry)
          Just blockInfo ->
            case validateRpcLogBlockHash logEntry blockInfo of
              Left err -> fail $ T.unpack err
              Right () -> pure (logEntry, blockInfo)
      endInfo <- requireRpc "eth_getBlockByNumber" $
        getBlockByNumber manager (indexerRpcEndpoints cfg) reqIdRef endBlock
      unless
        (normalizeHex (biHash endInfoBefore) == normalizeHex (biHash endInfo))
        (fail "Canonical end block changed while validating the fetched log range")

      enrichedLogs <- forM validatedLogs $ \(logEntry, blockInfo) -> do
        mTxFrom <- getTransactionFrom manager (indexerRpcEndpoints cfg) reqIdRef (rlTxHash logEntry)
        let parsedLog = parseConfiguredLog cfg logEntry
        tradeCosts <- case parsedLog of
          Just parsed@(ParsedPositionActivity kind _ _ _ _ _ _ _)
            | kind == "Open" || kind == "Close" -> do
            result <- getTradeCosts manager cfg reqIdRef logEntry parsed
            case result of
              Right costs -> pure $ Just costs
              Left err -> do
                logWarnEvery
                  60
                  "perps_indexer_trade_cost_preview_failed"
                  "Perps history indexer could not reconstruct optional activity cost metadata"
                  [ field "tx_hash" $ rlTxHash logEntry
                  , field "log_index" $ rlLogIndex logEntry
                  , field "error" err
                  ]
                pure Nothing
          _ -> pure Nothing
        pure (logEntry, blockInfo, mTxFrom, tradeCosts)
      -- Persist the whole canonical range, its volume rollups, and the cursor
      -- in one transaction. A failure cannot advance the cursor past missing
      -- events or leave rollups only partly rebuilt.
      certifiedCanonicalContinuity <- withDb pool $ \conn -> withTransaction conn $ do
        lockPerpsIndexerTransaction
          conn
          (picChainId cfg)
          (picIndexerName cfg)
          (paOrderRouter $ picAddresses cfg)
        (currentCursor, currentCursorHash) <-
          getPerpsIndexerLastBlock
            conn
            (picChainId cfg)
            (picIndexerName cfg)
            (paOrderRouter $ picAddresses cfg)
        let certifiesCanonicalContinuity =
              canCertifyIndexedRange
                (picStartBlock cfg)
                currentCursor
                startBlock
                endBlock
        -- Close the gap between the initial cursor verification and commit.
        -- A certifying append with a persisted boundary hash must still be on
        -- that same canonical chain immediately before the first write.
        when certifiesCanonicalContinuity $
          forM_ currentCursorHash $ \persistedHash -> do
            boundaryInfo <- requireRpc "eth_getBlockByNumber(cursor-boundary)" $
              getBlockByNumber manager (indexerRpcEndpoints cfg) reqIdRef (startBlock - 1)
            case validateIndexedBoundary (startBlock - 1) persistedHash boundaryInfo of
              Left err -> fail $ T.unpack err
              Right () -> pure ()
        affectedVolumeTimes <- fmap catMaybes $ forM enrichedLogs $ \(logEntry, blockInfo, mTxFrom, tradeCosts) ->
          processLog conn cfg blockInfo mTxFrom tradeCosts logEntry
        -- Rollups are recomputed from the canonical activity table after all
        -- writes in this range. The batch primitive deduplicates both minutes
        -- and their overlapping parent buckets, bounding write amplification
        -- while preserving idempotence when an RPC provider replays a range.
        let affectedVolumeMinutes =
              Set.toAscList $
                Set.fromList $
                  map (\timestamp -> (timestamp `div` 60) * 60) affectedVolumeTimes
        when (picCandleWriteMode cfg == PerpsCandleWritesDual) $ do
          recomputeMarketVolumeHierarchyBatch
            conn
            (picChainId cfg)
            (paOrderRouter $ picAddresses cfg)
            affectedVolumeMinutes
            (picCandleLatenessSeconds cfg)
          -- Only a range contiguous with the persisted block cursor proves
          -- that every intervening block was inspected. Explicit historical
          -- or disjoint replays may repair canonical rows and rollups, but
          -- must not certify skipped blocks as zero-volume.
          when certifiesCanonicalContinuity $
            advanceMarketVolumeCoverage
              conn
              (picChainId cfg)
              (paOrderRouter $ picAddresses cfg)
              (biTimestamp endInfo)
              0
        when certifiesCanonicalContinuity $
          setPerpsIndexerState conn (picChainId cfg) (picIndexerName cfg) (paOrderRouter $ picAddresses cfg)
            (picStartBlock cfg) endBlock (Just $ biHash endInfo)
        pure certifiesCanonicalContinuity
      logInfoEvery
        300
        "perps_indexer_progress"
        "Perps history indexer processed a block range"
        [ field "from_block" startBlock
        , field "to_block" endBlock
        , field "safe_head_block" safeBlock
        , field "event_count" $ length orderedLogs
        , field "canonical_progress_certified" certifiedCanonicalContinuity
        , field "indexed_through_timestamp" $ biTimestamp endInfo
        ]
      pure True

-- A processed block range may advance canonical state only when it begins at
-- the exact next block implied by the persisted cursor. The configured start
-- block is the trusted lower boundary for a fresh or rewound indexer. This
-- intentionally treats overlapping and disjoint explicit replays as
-- non-certifying even though their rows can still be written idempotently.
canCertifyIndexedRange :: Integer -> Integer -> Integer -> Integer -> Bool
canCertifyIndexedRange configuredStart storedCursor rangeFrom rangeTo =
  rangeFrom == expectedFrom && rangeTo >= rangeFrom
  where
    expectedFrom
      | storedCursor < configuredStart = configuredStart
      | otherwise = storedCursor + 1

validateIndexedBoundary :: Integer -> Text -> BlockInfo -> Either Text ()
validateIndexedBoundary expectedBlock persistedHash blockInfo = do
  unless
    (biNumber blockInfo == expectedBlock)
    (Left "Canonical cursor boundary block number changed before commit")
  unless
    (normalizeHex (biHash blockInfo) == normalizeHex persistedHash)
    (Left "Canonical cursor boundary block hash changed before commit")

validateRpcLogBlockHash :: RpcLog -> BlockInfo -> Either Text ()
validateRpcLogBlockHash logEntry blockInfo = do
  unless
    (rlBlockNumber logEntry == biNumber blockInfo)
    (Left "RPC log block number does not match canonical block metadata")
  unless
    (normalizeHex (rlBlockHash logEntry) == normalizeHex (biHash blockInfo))
    (Left "RPC log block hash does not match canonical block metadata")

verifyCursor :: Manager -> DbPool -> PerpsIndexerConfig -> IORef Integer -> Integer -> Maybe Text -> IO ()
verifyCursor _ _ _ _ 0 _ = pure ()
verifyCursor _ _ _ _ _ Nothing = pure ()
verifyCursor manager pool cfg reqIdRef lastBlock (Just storedHash) = do
  eBlock <- getBlockByNumber manager (indexerRpcEndpoints cfg) reqIdRef lastBlock
  case eBlock of
    Right blockInfo | normalizeHex (biHash blockInfo) == normalizeHex storedHash -> pure ()
    Right _ -> rewind
    Left err ->
      fail $
        "Perps indexer could not verify canonical cursor block "
          <> show lastBlock
          <> ": "
          <> show err
  where
    rewind = do
      -- A mismatch at the cursor proves that some ancestor may also have been
      -- replaced. We only persist the cursor hash, so the sole correctness-safe
      -- recovery is to rebuild this release from its configured start block.
      let rewindBlock = picStartBlock cfg
          newCursor = max 0 (rewindBlock - 1)
      logWarn
        "perps_indexer_reorg_detected"
        "Perps indexer detected a block hash mismatch and is rebuilding the release history"
        [ field "mismatch_block" lastBlock
        , field "rewind_to_block" newCursor
        ]
      withDb pool $ \conn -> withTransaction conn $ do
        lockPerpsIndexerTransaction
          conn
          (picChainId cfg)
          (picIndexerName cfg)
          (paOrderRouter $ picAddresses cfg)
        affectedVolumeMinutes <-
          if picCandleWriteMode cfg == PerpsCandleWritesDual
            then
              invalidateMarketVolumeFromBlock
                conn
                (picChainId cfg)
                (paOrderRouter $ picAddresses cfg)
                rewindBlock
            else pure []
        invalidateCompetitionSnapshotsForReleaseRebuild
          conn
          (picChainId cfg)
          (paOrderRouter $ picAddresses cfg)
        deletePerpsHistoryFromBlock conn (picChainId cfg) (paOrderRouter $ picAddresses cfg) rewindBlock
        -- Rebuild while the dataset is still coverage-gated. A minute may now
        -- be empty because its sole trade was orphaned; recomputing it also
        -- reconstructs affected parent buckets from retained pre-rewind rows.
        recomputeMarketVolumeHierarchyBatch
          conn
          (picChainId cfg)
          (paOrderRouter $ picAddresses cfg)
          affectedVolumeMinutes
          (picCandleLatenessSeconds cfg)
        setPerpsIndexerState conn (picChainId cfg) (picIndexerName cfg) (paOrderRouter $ picAddresses cfg)
          (picStartBlock cfg) newCursor Nothing

enrichPendingExecutionEvidence
  :: Manager
  -> DbPool
  -> PerpsIndexerConfig
  -> IORef Integer
  -> IO ()
enrichPendingExecutionEvidence manager pool cfg reqIdRef = do
  let releaseRouter = paOrderRouter $ picAddresses cfg
  candidates <- withDb pool $ \conn ->
    getPendingPerpsExecutionEvidence
      conn
      (picChainId cfg)
      releaseRouter
      executionOracleDerivationVersion
      executionEconomicsDerivationVersion
      executionEvidenceBatchSize
  transactionCacheRef <- newIORef Map.empty
  traceCacheRef <- newIORef Map.empty
  oracleCacheRef <- newIORef Map.empty
  forM_ candidates $ \candidate -> do
    withDb pool $ \conn ->
      markPerpsExecutionEvidenceAttempt
        conn
        (picChainId cfg)
        releaseRouter
        (peerOrderId candidate)
        (peerTerminalTxHash candidate)
        (peerTerminalBlockNumber candidate)
        (peerTerminalBlockHash candidate)
    txResult <-
      cachedBy
        transactionCacheRef
        (normalizeHex $ peerTerminalTxHash candidate)
        (getTransactionInfo manager (indexerRpcEndpoints cfg) reqIdRef $ peerTerminalTxHash candidate)
    case txResult >>= validateExecutionTransaction candidate of
      Left err ->
        logExecutionEvidenceFailure candidate "transaction" err
      Right txInfo -> do
        let needsOracle =
              peerOracleDerivationVersion candidate
                /= Just executionOracleDerivationVersion
            needsEconomics =
              maybe
                True
                (< executionEconomicsDerivationVersion)
                (peerExecutionEconomicsVersion candidate)
        traceResult <-
          cachedBy
            traceCacheRef
            (normalizeHex $ peerTerminalTxHash candidate)
            (deriveTransactionExecutionEconomics manager cfg reqIdRef txInfo)
        case traceResult >>= evidenceForOrder (peerOrderId candidate) of
          Right TradeExecutionEvidence {..} -> do
            when needsEconomics $
              withDb pool $ \conn ->
                updatePerpsOrderEconomicsEvidence
                  conn
                  (picChainId cfg)
                  releaseRouter
                  (peerOrderId candidate)
                  (peerTerminalTxHash candidate)
                  (peerTerminalBlockNumber candidate)
                  (peerTerminalBlockHash candidate)
                  teeVpiUsdc
                  teeFrozenCloseSpreadUsdc
                  executionEconomicsDerivationVersion
            when needsOracle $
              withDb pool $ \conn ->
                updatePerpsOrderOracleEvidence
                  conn
                  (picChainId cfg)
                  releaseRouter
                  (peerOrderId candidate)
                  (peerTerminalTxHash candidate)
                  (peerTerminalBlockNumber candidate)
                  (peerTerminalBlockHash candidate)
                  (Just teeExecutionOraclePrice)
                  (Just teeOracleFrozen)
                  (Just teeOraclePublishTime)
                  (Just teeOraclePublishTime)
                  executionOracleDerivationVersion
          Left traceErr -> do
            when needsEconomics $
              logExecutionEvidenceFailure candidate "economics" traceErr
            when needsOracle $ do
              oracleResult <-
                case peerCommitTimestamp candidate of
                  Nothing -> pure $ Left "Executed order is missing its commit timestamp"
                  Just commitTimestamp ->
                    cachedBy
                      oracleCacheRef
                      (normalizeHex $ peerTerminalTxHash candidate, commitTimestamp)
                      (deriveOrderExecutionOracle manager cfg reqIdRef candidate txInfo commitTimestamp)
              case oracleResult of
                Left err ->
                  logExecutionEvidenceFailure candidate "oracle" err
                Right snapshot ->
                  withDb pool $ \conn ->
                    updatePerpsOrderOracleEvidence
                      conn
                      (picChainId cfg)
                      releaseRouter
                      (peerOrderId candidate)
                      (peerTerminalTxHash candidate)
                      (peerTerminalBlockNumber candidate)
                      (peerTerminalBlockHash candidate)
                      (eosMidpointPrice <$> snapshot)
                      Nothing
                      (eosMinPublishTime <$> snapshot)
                      (eosMaxPublishTime <$> snapshot)
                      executionOraclePayloadDerivationVersion

cachedBy :: Ord key => IORef (Map.Map key value) -> key -> IO value -> IO value
cachedBy cacheRef key action = do
  cache <- readIORef cacheRef
  case Map.lookup key cache of
    Just cached -> pure cached
    Nothing -> do
      value <- action
      writeIORef cacheRef $ Map.insert key value cache
      pure value

validateExecutionTransaction
  :: PerpsExecutionEvidenceRow
  -> TransactionInfo
  -> Either Text TransactionInfo
validateExecutionTransaction candidate txInfo = do
  unless
    (normalizeHex (tiHash txInfo) == normalizeHex (peerTerminalTxHash candidate))
    (Left "Transaction hash does not match the indexed terminal event")
  unless
    (normalizeHex (tiBlockHash txInfo) == normalizeHex (peerTerminalBlockHash candidate))
    (Left "Transaction block hash does not match the indexed terminal event")
  pure txInfo

deriveOrderExecutionOracle
  :: Manager
  -> PerpsIndexerConfig
  -> IORef Integer
  -> PerpsExecutionEvidenceRow
  -> TransactionInfo
  -> Integer
  -> IO (Either Text (Maybe ExecutionOracleSnapshot))
deriveOrderExecutionOracle manager cfg reqIdRef candidate txInfo commitTimestamp =
  case executionUpdateData
    cfg
    (peerTerminalTxHash candidate)
    (peerTerminalBlockHash candidate)
    txInfo
    (peerOrderId candidate) of
      Left err -> pure $ Left err
      Right [] -> pure $ Right Nothing
      Right updateData -> do
        case executionOraclePublishTimeBounds commitTimestamp of
          Left err -> pure $ Left err
          Right publishTimeBounds ->
            fmap Just
              <$> deriveExecutionOracleMidpoint
                manager
                cfg
                reqIdRef
                updateData
                publishTimeBounds

deriveTransactionExecutionEconomics
  :: Manager
  -> PerpsIndexerConfig
  -> IORef Integer
  -> TransactionInfo
  -> IO (Either Text (Map.Map Integer TradeExecutionEvidence))
deriveTransactionExecutionEconomics manager cfg reqIdRef txInfo = do
  rpcTrace <-
    rpcCallAny
      manager
      (indexerRpcEndpoints cfg)
      reqIdRef
      "debug_traceTransaction"
      [ String $ tiHash txInfo
      , object
          [ "tracer" .= ("callTracer" :: Text)
          , "timeout" .= ("20s" :: Text)
          ]
      ]
  case rpcTrace >>= decodeTrace of
    Right evidence -> pure $ Right evidence
    Left rpcErr -> pure $ Left $ "Alchemy RPC call trace failed: " <> rpcErr
  where
    decodeTrace trace = do
      validateCanonicalTraceRoot txInfo trace
      decodeTradeExecutionEvidence
        (paOrderRouter $ picAddresses cfg)
        (paCfdEngine $ picAddresses cfg)
        (paCfdEngineSettlementSidecar $ picAddresses cfg)
        (paPletherOracle $ picAddresses cfg)
        trace

validateCanonicalTraceRoot :: TransactionInfo -> Value -> Either Text ()
validateCanonicalTraceRoot txInfo = \case
  Object trace -> do
    traceFrom <- requiredString "from" trace
    traceTo <- requiredString "to" trace
    traceInput <- requiredString "input" trace
    unless (normalizeHex traceFrom == normalizeHex (tiFrom txInfo)) $
      Left "Trace root sender does not match the canonical transaction"
    unless (normalizeHex traceTo == normalizeHex (tiTo txInfo)) $
      Left "Trace root target does not match the canonical transaction"
    unless (decodeHex traceInput == tiInput txInfo) $
      Left "Trace root calldata does not match the canonical transaction"
  _ -> Left "Trace root must be an object"

evidenceForOrder
  :: Integer
  -> Map.Map Integer TradeExecutionEvidence
  -> Either Text TradeExecutionEvidence
evidenceForOrder orderId evidence =
  maybe
    (Left $ "Transaction trace contains no exact execution economics for order " <> T.pack (show orderId))
    Right
    (Map.lookup orderId evidence)

logExecutionEvidenceFailure
  :: PerpsExecutionEvidenceRow
  -> Text
  -> Text
  -> IO ()
logExecutionEvidenceFailure candidate component err =
  logWarnEvery
    60
    ("perps_indexer_execution_evidence_" <> component <> "_failed")
    "Perps history indexer could not derive optional exact execution evidence"
    [ field "component" component
    , field "order_id" $ peerOrderId candidate
    , field "tx_hash" $ peerTerminalTxHash candidate
    , field "error" err
    ]

executionEvidenceBatchSize :: Int
executionEvidenceBatchSize = 5

executionOracleDerivationVersion, executionOraclePayloadDerivationVersion, executionEconomicsDerivationVersion :: Int
executionOracleDerivationVersion = 2
executionOraclePayloadDerivationVersion = 1
executionEconomicsDerivationVersion = 1

processLog
  :: Connection
  -> PerpsIndexerConfig
  -> BlockInfo
  -> Maybe Text
  -> Maybe TradeCosts
  -> RpcLog
  -> IO (Maybe Integer)
processLog conn cfg blockInfo txFrom tradeCosts logEntry =
  case parseConfiguredLog cfg logEntry of
    Nothing -> pure Nothing
    Just parsed ->
      processParsedLog conn cfg blockInfo txFrom tradeCosts logEntry parsed

processParsedLog
  :: Connection
  -> PerpsIndexerConfig
  -> BlockInfo
  -> Maybe Text
  -> Maybe TradeCosts
  -> RpcLog
  -> ParsedPerpsLog
  -> IO (Maybe Integer)
processParsedLog conn cfg blockInfo txFrom tradeCosts logEntry parsed
  | ParsedUsdcTransfer fromAddress toAddress amount _ <- parsed = do
      insertPerpsUsdcTransfer
        conn
        (picChainId cfg)
        (paOrderRouter $ picAddresses cfg)
        (paUsdc $ picAddresses cfg)
        fromAddress
        toAddress
        amount
        (rlTxHash logEntry)
        (rlBlockNumber logEntry)
        (rlBlockHash logEntry)
        (rlTxIndex logEntry)
        (rlLogIndex logEntry)
        (biTimestamp blockInfo)
      pure Nothing
  | otherwise = do
      let eventName = parsedEventName parsed
          account = parsedAccount parsed
          orderId = parsedOrderId parsed
          side = parsedSide parsed
          eventPayload = parsedPayload parsed
          releaseRouter = paOrderRouter $ picAddresses cfg
      insertPerpsEvent conn (picChainId cfg) releaseRouter (rlAddress logEntry) eventName (rlTxHash logEntry)
        (rlBlockNumber logEntry) (rlBlockHash logEntry) (rlTxIndex logEntry) (rlLogIndex logEntry)
        (biTimestamp blockInfo) account orderId side eventPayload
      case parsed of
        ParsedOrderCommitted oid account' side' _ ->
          do
            upsertPerpsOrderCommitted conn (picChainId cfg) releaseRouter oid account' side' (rlTxHash logEntry)
              (rlBlockNumber logEntry) (biTimestamp blockInfo)
            insertPerpsExpiredCleanupActivityIfReady conn (picChainId cfg) releaseRouter oid
        ParsedIntentRegistered oid account' clientOrderId side' _ -> do
          upsertPerpsOrderCommitted conn (picChainId cfg) releaseRouter oid account' side' (rlTxHash logEntry)
            (rlBlockNumber logEntry) (biTimestamp blockInfo)
          updatePerpsOrderLifecycleIdentity
            conn
            (picChainId cfg)
            releaseRouter
            oid
            clientOrderId
        ParsedOrderFinalized oid account' clientOrderId receiptHash status terminalReason' mode failedConstraint executionPrice economics _ -> do
          upsertPerpsOrderTerminal
            conn
            (picChainId cfg)
            releaseRouter
            oid
            status
            (if status == "Executed" then Nothing else Just terminalReason')
            (Just executionPrice)
            Nothing
            (rlTxHash logEntry)
            (rlBlockNumber logEntry)
            (biTimestamp blockInfo)
          updatePerpsOrderLifecycleReceipt
            conn
            (picChainId cfg)
            releaseRouter
            oid
            account'
            clientOrderId
            receiptHash
            terminalReason'
            mode
            failedConstraint
            economics
        ParsedOrderExecuted _ _ _
          | isJust (paOrderLifecycleBook $ picAddresses cfg) -> pure ()
        ParsedOrderExecuted oid executionPrice _ ->
          upsertPerpsOrderTerminal
            conn
            (picChainId cfg)
            releaseRouter
            oid
            "Executed"
            Nothing
            (Just executionPrice)
            Nothing
            (rlTxHash logEntry)
            (rlBlockNumber logEntry)
            (biTimestamp blockInfo)
        ParsedOrderFailed _ _ _ _
          | isJust (paOrderLifecycleBook $ picAddresses cfg) -> pure ()
        ParsedOrderFailed oid reason reasonName _ -> do
          upsertPerpsOrderTerminal
            conn
            (picChainId cfg)
            releaseRouter
            oid
            (terminalStatus reasonName)
            (Just reasonName)
            Nothing
            txFrom
            (rlTxHash logEntry)
            (rlBlockNumber logEntry)
            (biTimestamp blockInfo)
          when (reason == 0) $
            insertPerpsExpiredCleanupActivityIfReady conn (picChainId cfg) releaseRouter oid
        ParsedPositionActivity kind account' side' price sizeDelta amountUsdc pnl payload ->
          insertPerpsActivity conn (picChainId cfg) releaseRouter (rlAddress logEntry) (activityKey logEntry kind Nothing) account'
            kind Nothing Nothing (Just side') price sizeDelta amountUsdc pnl (rlTxHash logEntry)
            (rlBlockNumber logEntry) (rlBlockHash logEntry) (rlTxIndex logEntry) (rlLogIndex logEntry)
            (biTimestamp blockInfo) (addTradeCosts tradeCosts payload)
        ParsedMarginActivity kind account' amount payload ->
          insertPerpsActivity conn (picChainId cfg) releaseRouter (rlAddress logEntry) (activityKey logEntry kind Nothing) account'
            kind Nothing Nothing Nothing Nothing Nothing (Just amount) Nothing (rlTxHash logEntry)
            (rlBlockNumber logEntry) (rlBlockHash logEntry) (rlTxIndex logEntry) (rlLogIndex logEntry)
            (biTimestamp blockInfo) payload
      pure $
        if isMarketVolumeActivity parsed
          then Just $ biTimestamp blockInfo
          else Nothing

-- Conflict handlers make live ingestion idempotent, but they can also leave a
-- pre-existing row untouched when its identity collides with different
-- semantics. Replay therefore verifies the exact canonical projection after
-- every prepared log. Any mismatch aborts and rolls back the whole range.
assertPreparedReplayLogExact :: Connection -> PerpsIndexerConfig -> PreparedReplayLog -> IO ()
assertPreparedReplayLogExact conn cfg prepared =
  case prlParsed prepared of
    ParsedUsdcTransfer fromAddress toAddress amount _ ->
      let logEntry = prlLog prepared
       in assertPerpsReplayUsdcTransferExact
            conn
            (picChainId cfg)
            (paOrderRouter $ picAddresses cfg)
            (paUsdc $ picAddresses cfg)
            fromAddress
            toAddress
            amount
            (rlTxHash logEntry)
            (rlBlockNumber logEntry)
            (rlBlockHash logEntry)
            (rlTxIndex logEntry)
            (rlLogIndex logEntry)
            (biTimestamp $ prlBlockInfo prepared)
    _ -> assertPreparedPerpsLogExact conn cfg prepared

assertPreparedPerpsLogExact :: Connection -> PerpsIndexerConfig -> PreparedReplayLog -> IO ()
assertPreparedPerpsLogExact conn cfg prepared = do
  let logEntry = prlLog prepared
      blockInfo = prlBlockInfo prepared
      parsed = prlParsed prepared
      releaseRouter = paOrderRouter $ picAddresses cfg
      chainId = picChainId cfg
      timestamp = biTimestamp blockInfo
      txHash = rlTxHash logEntry
      blockNumber = rlBlockNumber logEntry
      blockHash = rlBlockHash logEntry
      txIndex = rlTxIndex logEntry
      logIndex = rlLogIndex logEntry
  assertPerpsReplayEventExact
    conn
    chainId
    releaseRouter
    (rlAddress logEntry)
    (parsedEventName parsed)
    txHash
    blockNumber
    blockHash
    txIndex
    logIndex
    timestamp
    (parsedAccount parsed)
    (parsedOrderId parsed)
    (parsedSide parsed)
    (parsedPayload parsed)
  case parsed of
    ParsedOrderCommitted orderId account side _ -> do
      assertPerpsReplayOrderCommittedExact
        conn
        chainId
        releaseRouter
        orderId
        account
        side
        txHash
        blockNumber
        timestamp
      assertPerpsReplayExpiredCleanupIfReadyExact conn chainId releaseRouter orderId
    ParsedIntentRegistered {} -> pure ()
    ParsedOrderFinalized {} -> pure ()
    ParsedOrderExecuted {} | isJust (paOrderLifecycleBook $ picAddresses cfg) -> pure ()
    ParsedOrderExecuted orderId executionPrice _ ->
      assertPerpsReplayOrderTerminalExact
        conn
        chainId
        releaseRouter
        orderId
        "Executed"
        Nothing
        (Just executionPrice)
        Nothing
        txHash
        blockNumber
        timestamp
    ParsedOrderFailed {} | isJust (paOrderLifecycleBook $ picAddresses cfg) -> pure ()
    ParsedOrderFailed orderId reason reasonName _ -> do
      assertPerpsReplayOrderTerminalExact
        conn
        chainId
        releaseRouter
        orderId
        (terminalStatus reasonName)
        (Just reasonName)
        Nothing
        (Just $ prlTransactionFrom prepared)
        txHash
        blockNumber
        timestamp
      when (reason == 0) $
        assertPerpsReplayExpiredCleanupExact conn chainId releaseRouter orderId
    ParsedPositionActivity kind account side price sizeDelta amountUsdc pnl payload ->
      assertPerpsReplayActivityExact
        conn
        chainId
        releaseRouter
        (rlAddress logEntry)
        (activityKey logEntry kind Nothing)
        account
        kind
        Nothing
        Nothing
        (Just side)
        price
        sizeDelta
        amountUsdc
        pnl
        txHash
        blockNumber
        blockHash
        txIndex
        logIndex
        timestamp
        (addTradeCosts (prlTradeCosts prepared) payload)
    ParsedMarginActivity kind account amount payload ->
      assertPerpsReplayActivityExact
        conn
        chainId
        releaseRouter
        (rlAddress logEntry)
        (activityKey logEntry kind Nothing)
        account
        kind
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        (Just amount)
        Nothing
        txHash
        blockNumber
        blockHash
        txIndex
        logIndex
        timestamp
        payload
    ParsedUsdcTransfer {} -> pure ()

-- Keep this predicate aligned with the canonical volume query. Only position
-- lifecycle events that contain both notional inputs contribute to OHLCV.
isMarketVolumeActivity :: ParsedPerpsLog -> Bool
isMarketVolumeActivity = \case
  ParsedPositionActivity kind _ _ (Just _) (Just _) _ _ _ ->
    kind `elem` ["Open", "Close", "Liquidated"]
  _ -> False

parseConfiguredLog :: PerpsIndexerConfig -> RpcLog -> Maybe ParsedPerpsLog
parseConfiguredLog cfg logEntry
  | normalizeHex (rlAddress logEntry) == normalizeHex (paUsdc $ picAddresses cfg) =
      case rlTopics logEntry of
        topic : _ | topic == transferTopic -> parseUsdcTransfer logEntry
        _ -> Nothing
  | otherwise = parsePerpsLog logEntry

parsePerpsLog :: RpcLog -> Maybe ParsedPerpsLog
parsePerpsLog logEntry =
  case rlTopics logEntry of
    topic : _
      | topic == orderCommittedTopic -> parseOrderCommitted logEntry
      | topic == intentRegisteredTopic -> parseIntentRegistered logEntry
      | topic == orderFinalizedTopic -> parseOrderFinalized logEntry
      | topic == orderExecutedTopic -> parseOrderExecuted logEntry
      | topic == orderFailedTopic -> parseOrderFailed logEntry
      | topic == positionOpenedTopic -> parsePositionOpened logEntry
      | topic == positionClosedTopic -> parsePositionClosed logEntry
      | topic == positionLiquidatedTopic -> parsePositionLiquidated logEntry
      | topic == marginAddedTopic -> parseMarginAdded logEntry
      | topic == depositTopic -> parseDepositWithdraw "Deposit" logEntry
      | topic == withdrawTopic -> parseDepositWithdraw "Withdraw" logEntry
    _ -> Nothing

parseUsdcTransfer :: RpcLog -> Maybe ParsedPerpsLog
parseUsdcTransfer logEntry =
  case rlTopics logEntry of
    [topic, _, _]
      | topic == transferTopic
      , all ((== 32) . BS.length) (rlTopics logEntry)
      , BS.length (rlData logEntry) == 32 -> do
          fromAddress <- indexedAddress (rlTopics logEntry) 1
          toAddress <- indexedAddress (rlTopics logEntry) 2
          let amount = wordAt (rlData logEntry) 0
          pure $ ParsedUsdcTransfer fromAddress toAddress amount $
            object
              [ "from" .= fromAddress
              , "to" .= toAddress
              , "amount" .= show amount
              ]
    _ -> Nothing

parseOrderCommitted :: RpcLog -> Maybe ParsedPerpsLog
parseOrderCommitted logEntry = do
  oid <- indexedUint (rlTopics logEntry) 1
  account <- indexedAddress (rlTopics logEntry) 2
  let side = fromInteger $ wordAt (rlData logEntry) 0
  pure $ ParsedOrderCommitted oid account side $
    object ["orderId" .= show oid, "account" .= account, "side" .= side]

parseIntentRegistered :: RpcLog -> Maybe ParsedPerpsLog
parseIntentRegistered logEntry = do
  unless (BS.length (rlData logEntry) == 20 * 32) Nothing
  oid <- indexedUint (rlTopics logEntry) 1
  account <- indexedAddress (rlTopics logEntry) 2
  clientOrderId <- indexedBytes32 (rlTopics logEntry) 3
  let side = fromInteger $ wordAt (rlData logEntry) 3
      payload = object
        [ "orderId" .= show oid
        , "account" .= account
        , "clientOrderId" .= clientOrderId
        , "intentHash" .= hexWordAt (rlData logEntry) 0
        , "executionBountyUsdc" .= show (wordAt (rlData logEntry) 1)
        , "side" .= side
        , "validUntil" .= show (wordAt (rlData logEntry) 8)
        , "allowedExecutionModes" .= wordAt (rlData logEntry) 9
        , "expectedConfigHash" .= hexWordAt (rlData logEntry) 10
        ]
  pure $ ParsedIntentRegistered oid account clientOrderId side payload

parseOrderFinalized :: RpcLog -> Maybe ParsedPerpsLog
parseOrderFinalized logEntry = do
  unless (BS.length (rlData logEntry) == 46 * 32) Nothing
  oid <- indexedUint (rlTopics logEntry) 1
  account <- indexedAddress (rlTopics logEntry) 2
  clientOrderId <- indexedBytes32 (rlTopics logEntry) 3
  let bytes = rlData logEntry
      receiptHash = hexWordAt bytes 0
      lifecycleStatus = fromInteger (wordAt bytes 9) :: Int
      terminalReasonCode = fromInteger (wordAt bytes 10) :: Int
      executionModeCode = fromInteger (wordAt bytes 11) :: Int
      executionPrice = wordAt bytes 14
      failedConstraintCode = fromInteger (wordAt bytes 25) :: Int
      terminalReason = terminalReasonName terminalReasonCode
      status
        | lifecycleStatus == 2 && terminalReasonCode == 1 = "Executed"
        | terminalReasonCode == 2 = "Expired / Cleaned up"
        | otherwise = "Failed"
      mode = executionModeName executionModeCode
      failedConstraint =
        if failedConstraintCode == 0
          then Nothing
          else Just $ failedConstraintName failedConstraintCode
      economics = object
        [ "executionNotionalUsdc" .= show (wordAt bytes 29)
        , "executionBountyUsdc" .= show (wordAt bytes 19)
        , "realizedPnlUsdc" .= show (intWordAt bytes 30)
        , "vpiUsdc" .= show (intWordAt bytes 31)
        , "carryUsdc" .= show (intWordAt bytes 32)
        , "executionFeeUsdc" .= show (wordAt bytes 33)
        , "frozenSpreadUsdc" .= show (wordAt bytes 34)
        , "actionChargeAssessedUsdc" .= show (wordAt bytes 35)
        , "actionChargeCollectedUsdc" .= show (wordAt bytes 36)
        , "grossAccountDebitUsdc" .= show (wordAt bytes 37)
        , "preSettlementBalanceUsdc" .= show (wordAt bytes 38)
        , "postSettlementBalanceUsdc" .= show (wordAt bytes 39)
        , "preTraderClaimBalanceUsdc" .= show (wordAt bytes 40)
        , "postTraderClaimBalanceUsdc" .= show (wordAt bytes 41)
        , "postPositionSize" .= show (wordAt bytes 42)
        , "postPositionMarginUsdc" .= show (wordAt bytes 43)
        , "postPositionEquityUsdc" .= show (intWordAt bytes 44)
        , "postLeverageBps" .= show (wordAt bytes 45)
        ]
      payload = object
        [ "orderId" .= show oid
        , "account" .= account
        , "clientOrderId" .= clientOrderId
        , "receiptHash" .= receiptHash
        , "terminalBlock" .= show (wordAt bytes 1)
        , "terminalTime" .= show (wordAt bytes 2)
        , "status" .= lifecycleStatus
        , "terminalReason" .= terminalReason
        , "executionMode" .= mode
        , "failedConstraint" .= failedConstraint
        , "executionPrice" .= show executionPrice
        , "executionBountyUsdc" .= show (wordAt bytes 19)
        , "economics" .= economics
        ]
  pure $
    ParsedOrderFinalized
      oid account clientOrderId receiptHash status terminalReason mode failedConstraint
      executionPrice economics payload

parseOrderExecuted :: RpcLog -> Maybe ParsedPerpsLog
parseOrderExecuted logEntry = do
  oid <- indexedUint (rlTopics logEntry) 1
  let executionPrice = wordAt (rlData logEntry) 0
  pure $ ParsedOrderExecuted oid executionPrice $
    object ["orderId" .= show oid, "executionPrice" .= show executionPrice]

parseOrderFailed :: RpcLog -> Maybe ParsedPerpsLog
parseOrderFailed logEntry = do
  oid <- indexedUint (rlTopics logEntry) 1
  let reason = fromInteger $ wordAt (rlData logEntry) 0
      reasonName = orderFailReasonName reason
  pure $ ParsedOrderFailed oid reason reasonName $
    object ["orderId" .= show oid, "reason" .= reason, "reasonName" .= reasonName]

parsePositionOpened :: RpcLog -> Maybe ParsedPerpsLog
parsePositionOpened logEntry = do
  account <- indexedAddress (rlTopics logEntry) 1
  let side = fromInteger $ wordAt (rlData logEntry) 0
      sizeDelta = wordAt (rlData logEntry) 1
      price = wordAt (rlData logEntry) 2
      marginDelta = wordAt (rlData logEntry) 3
      payload = object
        [ "account" .= account
        , "side" .= side
        , "sizeDelta" .= show sizeDelta
        , "price" .= show price
        , "marginDelta" .= show marginDelta
        ]
  pure $ ParsedPositionActivity "Open" account side (Just price) (Just sizeDelta) (Just marginDelta) Nothing payload

parsePositionClosed :: RpcLog -> Maybe ParsedPerpsLog
parsePositionClosed logEntry = do
  account <- indexedAddress (rlTopics logEntry) 1
  let side = fromInteger $ wordAt (rlData logEntry) 0
      sizeDelta = wordAt (rlData logEntry) 1
      price = wordAt (rlData logEntry) 2
      pnl = intWordAt (rlData logEntry) 3
      payload = object
        [ "account" .= account
        , "side" .= side
        , "sizeDelta" .= show sizeDelta
        , "price" .= show price
        , "pnl" .= show pnl
        ]
  pure $ ParsedPositionActivity "Close" account side (Just price) (Just sizeDelta) Nothing (Just pnl) payload

getTradeCosts
  :: Manager
  -> PerpsIndexerConfig
  -> IORef Integer
  -> RpcLog
  -> ParsedPerpsLog
  -> IO (Either Text TradeCosts)
getTradeCosts manager cfg reqIdRef logEntry parsed
  | rlBlockNumber logEntry <= 0 = pure $ Left "Cannot preview trade costs before genesis"
  | otherwise = do
      let callData = replayPreviewCallData parsed
      case callData of
        Nothing -> pure $ Left "Unsupported position activity for trade-cost preview"
        Just encoded -> do
          result <-
            rpcCallAny
              manager
              (indexerRpcEndpoints cfg)
              reqIdRef
              "eth_call"
              [ object
                  [ "to" .= paCfdEngineLens (picAddresses cfg)
                  , "data" .= ("0x" <> bytesToHex encoded)
                  ]
              , String $ "0x" <> intToHex (rlBlockNumber logEntry - 1)
              ]
          pure $ do
            response <- case result of
              Left err -> Left err
              Right (String value) -> Right $ decodeHex value
              Right _ -> Left "Expected eth_call hex result"
            case parsed of
              ParsedPositionActivity "Open" _ _ _ _ _ _ _ -> decodeOpenTradeCosts response
              ParsedPositionActivity "Close" _ _ _ _ _ _ _ -> decodeCloseTradeCosts response
              _ -> Left "Unsupported position activity for trade-cost preview"

getReplayTradeCosts
  :: Manager
  -> PerpsIndexerConfig
  -> IORef Integer
  -> RpcLog
  -> ParsedPerpsLog
  -> IO (Either Text TradeCosts)
getReplayTradeCosts manager cfg reqIdRef logEntry parsed
  | rlBlockNumber logEntry <= 0 = pure $ Left "Cannot preview replay trade costs before genesis"
  | otherwise =
      case replayPreviewCallData parsed of
        Nothing -> pure $ Left "Unsupported position activity for replay trade-cost preview"
        Just encoded -> do
          result <-
            rpcCallAny
              manager
              (indexerRpcEndpoints cfg)
              reqIdRef
              "eth_call"
              [ object
                  [ "to" .= paCfdEngineLens (picAddresses cfg)
                  , "data" .= ("0x" <> bytesToHex encoded)
                  ]
              , String $ "0x" <> intToHex (rlBlockNumber logEntry - 1)
              ]
          pure $ do
            response <- case result of
              Left err -> Left err
              Right (String value) -> requiredHexBytes "trade-cost preview" value
              Right _ -> Left "Replay eth_call result must be canonical hex bytes"
            case parsed of
              ParsedPositionActivity kind _ _ _ _ _ _ _ ->
                decodeReplayTradeCosts kind response
              _ -> Left "Unsupported position activity for replay trade-cost preview"

replayPreviewCallData :: ParsedPerpsLog -> Maybe ByteString
replayPreviewCallData = \case
  ParsedPositionActivity "Open" account side (Just price) (Just sizeDelta) (Just marginDelta) _ _ ->
    Just $
      encodeCall
        "previewOpen(address,uint8,uint256,uint256,uint256,uint64)"
        [ encodeAddress account
        , encodeUint256 $ toInteger side
        , encodeUint256 sizeDelta
        , encodeUint256 marginDelta
        , encodeUint256 price
        , encodeUint256 0
        ]
  ParsedPositionActivity "Close" account _ (Just price) (Just sizeDelta) _ _ _ ->
    Just $
      encodeCall
        "previewClose(address,uint256,uint256)"
        [encodeAddress account, encodeUint256 sizeDelta, encodeUint256 price]
  _ -> Nothing

decodeOpenTradeCosts :: ByteString -> Either Text TradeCosts
decodeOpenTradeCosts bytes
  | BS.length bytes < 10 * 32 = Left "Open preview result is shorter than 10 ABI words"
  | executionFee < 0 = Left "Open preview returned a negative execution fee"
  | otherwise = Right $ TradeCosts executionFee vpi
  where
    vpi = intWordAt bytes 7
    tradeCost = intWordAt bytes 9
    executionFee = tradeCost - vpi

decodeCloseTradeCosts :: ByteString -> Either Text TradeCosts
decodeCloseTradeCosts bytes
  | BS.length bytes < 8 * 32 = Left "Close preview result is shorter than 8 ABI words"
  | otherwise = Right $ TradeCosts (wordAt bytes 7) (intWordAt bytes 5)

decodeReplayTradeCosts :: Text -> ByteString -> Either Text TradeCosts
decodeReplayTradeCosts "Open" bytes
  | BS.length bytes /= 10 * 32 =
      Left "Replay open preview result must contain exactly 10 ABI words"
  | otherwise = decodeOpenTradeCosts bytes
decodeReplayTradeCosts "Close" bytes
  | BS.length bytes /= 8 * 32 =
      Left "Replay close preview result must contain exactly 8 ABI words"
  | otherwise = decodeCloseTradeCosts bytes
decodeReplayTradeCosts _ _ =
  Left "Unsupported position activity for replay trade-cost preview"

addTradeCosts :: Maybe TradeCosts -> Value -> Value
addTradeCosts Nothing payload = payload
addTradeCosts (Just TradeCosts {..}) payloadValue = addToPayload payloadValue
  where
    addToPayload = \case
      Object payload ->
        Object $
          KM.insert (Key.fromText "vpiUsdc") (String $ T.pack $ show tcVpiUsdc) $
            KM.insert
              (Key.fromText "executionFeeUsdc")
              (String $ T.pack $ show tcExecutionFeeUsdc)
              payload
      payload -> payload

parsePositionLiquidated :: RpcLog -> Maybe ParsedPerpsLog
parsePositionLiquidated logEntry = do
  account <- indexedAddress (rlTopics logEntry) 1
  let side = fromInteger $ wordAt (rlData logEntry) 0
      sizeDelta = wordAt (rlData logEntry) 1
      price = wordAt (rlData logEntry) 2
      keeperBounty = wordAt (rlData logEntry) 3
      payload = object
        [ "account" .= account
        , "side" .= side
        , "sizeDelta" .= show sizeDelta
        , "price" .= show price
        , "keeperBountyUsdc" .= show keeperBounty
        ]
  pure $ ParsedPositionActivity "Liquidated" account side (Just price) (Just sizeDelta) (Just keeperBounty) Nothing payload

parseMarginAdded :: RpcLog -> Maybe ParsedPerpsLog
parseMarginAdded logEntry = do
  account <- indexedAddress (rlTopics logEntry) 1
  let amount = wordAt (rlData logEntry) 0
  pure $ ParsedMarginActivity "Add margin" account amount $
    object ["account" .= account, "amountUsdc" .= show amount]

parseDepositWithdraw :: Text -> RpcLog -> Maybe ParsedPerpsLog
parseDepositWithdraw kind logEntry = do
  account <- indexedAddress (rlTopics logEntry) 1
  asset <- indexedAddress (rlTopics logEntry) 2
  let amount = wordAt (rlData logEntry) 0
      contractAddress = normalizeHex $ rlAddress logEntry
  pure $ ParsedMarginActivity kind account amount $
    object
      [ "account" .= account
      , "asset" .= asset
      , "contractAddress" .= contractAddress
      , "amountUsdc" .= show amount
      ]

parsedEventName :: ParsedPerpsLog -> Text
parsedEventName = \case
  ParsedOrderCommitted {} -> "OrderCommitted"
  ParsedIntentRegistered {} -> "IntentRegistered"
  ParsedOrderFinalized {} -> "OrderFinalized"
  ParsedOrderExecuted {} -> "OrderExecuted"
  ParsedOrderFailed {} -> "OrderFailed"
  ParsedPositionActivity kind _ _ _ _ _ _ _
    | kind == "Open" -> "PositionOpened"
    | kind == "Close" -> "PositionClosed"
    | kind == "Liquidated" -> "PositionLiquidated"
    | otherwise -> kind
  ParsedMarginActivity kind _ _ _
    | kind == "Add margin" -> "MarginAdded"
    | otherwise -> kind
  ParsedUsdcTransfer {} -> "Transfer"

parsedAccount :: ParsedPerpsLog -> Maybe Text
parsedAccount = \case
  ParsedOrderCommitted _ account _ _ -> Just account
  ParsedIntentRegistered _ account _ _ _ -> Just account
  ParsedOrderFinalized _ account _ _ _ _ _ _ _ _ _ -> Just account
  ParsedPositionActivity _ account _ _ _ _ _ _ -> Just account
  ParsedMarginActivity _ account _ _ -> Just account
  ParsedUsdcTransfer _ toAddress _ _ -> Just toAddress
  _ -> Nothing

parsedOrderId :: ParsedPerpsLog -> Maybe Integer
parsedOrderId = \case
  ParsedOrderCommitted oid _ _ _ -> Just oid
  ParsedIntentRegistered oid _ _ _ _ -> Just oid
  ParsedOrderFinalized oid _ _ _ _ _ _ _ _ _ _ -> Just oid
  ParsedOrderExecuted oid _ _ -> Just oid
  ParsedOrderFailed oid _ _ _ -> Just oid
  _ -> Nothing

parsedSide :: ParsedPerpsLog -> Maybe Int
parsedSide = \case
  ParsedOrderCommitted _ _ side _ -> Just side
  ParsedIntentRegistered _ _ _ side _ -> Just side
  ParsedPositionActivity _ _ side _ _ _ _ _ -> Just side
  _ -> Nothing

parsedPayload :: ParsedPerpsLog -> Value
parsedPayload = \case
  ParsedOrderCommitted _ _ _ payload -> payload
  ParsedIntentRegistered _ _ _ _ payload -> payload
  ParsedOrderFinalized _ _ _ _ _ _ _ _ _ _ payload -> payload
  ParsedOrderExecuted _ _ payload -> payload
  ParsedOrderFailed _ _ _ payload -> payload
  ParsedPositionActivity _ _ _ _ _ _ _ payload -> payload
  ParsedMarginActivity _ _ _ payload -> payload
  ParsedUsdcTransfer _ _ _ payload -> payload

terminalStatus :: Text -> Text
terminalStatus "Expired" = "Expired / Cleaned up"
terminalStatus _ = "Failed"

orderFailReasonName :: Int -> Text
orderFailReasonName = \case
  0 -> "Expired"
  1 -> "CloseOnly"
  2 -> "SlippageExceeded"
  3 -> "EnginePanic"
  4 -> "AccountLiquidated"
  5 -> "EngineRevert"
  n -> "Unknown(" <> T.pack (show n) <> ")"

terminalReasonName :: Int -> Text
terminalReasonName = \case
  0 -> "None"
  1 -> "Executed"
  2 -> "Expired"
  3 -> "Slippage"
  4 -> "Config mismatch"
  5 -> "Mode disallowed"
  6 -> "Risk off"
  7 -> "Planner rejected"
  8 -> "Constraint violation"
  9 -> "Account liquidated"
  n -> "Unknown(" <> T.pack (show n) <> ")"

executionModeName :: Int -> Text
executionModeName = \case
  1 -> "Live"
  2 -> "FAD"
  3 -> "Frozen"
  n -> "Unknown(" <> T.pack (show n) <> ")"

failedConstraintName :: Int -> Text
failedConstraintName = \case
  1 -> "Execution bounty"
  2 -> "Execution notional"
  3 -> "Gross account debit"
  4 -> "Action charge"
  5 -> "Explicit fees"
  6 -> "Post-position size"
  7 -> "Post-settlement balance"
  8 -> "Post-position equity"
  9 -> "Post leverage"
  n -> "Unknown(" <> T.pack (show n) <> ")"

activityKey :: RpcLog -> Text -> Maybe Integer -> Text
activityKey logEntry kind orderId =
  T.intercalate ":"
    [ normalizeHex (rlTxHash logEntry)
    , T.pack $ show (rlLogIndex logEntry)
    , T.replace " " "_" kind
    , maybe "" (T.pack . show) orderId
    ]

perpsAddresses :: PerpsIndexerConfig -> [Text]
perpsAddresses cfg =
  paUsdc (picAddresses cfg) : perpsContractAddresses cfg

perpsContractAddresses :: PerpsIndexerConfig -> [Text]
perpsContractAddresses = perpsContractAddressesFor . picAddresses

perpsContractAddressesFor :: PerpsAddresses -> [Text]
perpsContractAddressesFor addresses =
  [ paOrderRouter addresses
  , paCfdEngine addresses
  , paMarginClearinghouse addresses
  ] <> maybe [] pure (paOrderLifecycleBook addresses)

requireRpc :: Text -> IO (Either Text a) -> IO a
requireRpc label action = do
  result <- action
  case result of
    Right value -> pure value
    Left err -> fail $ T.unpack $ label <> " failed: " <> err

getCurrentBlockNumber :: Manager -> [RpcEndpoint] -> IORef Integer -> IO (Either Text Integer)
getCurrentBlockNumber manager rpcUrls reqIdRef = do
  result <- rpcCallAny manager rpcUrls reqIdRef "eth_blockNumber" ([] :: [Value])
  pure $ case result of
    Left err -> Left err
    Right (String hex) -> Right $ hexToInteger $ strip0x hex
    Right _ -> Left "Expected hex string"

getReplayCurrentBlockNumber :: Manager -> [RpcEndpoint] -> IORef Integer -> IO (Either Text Integer)
getReplayCurrentBlockNumber manager rpcUrls reqIdRef = do
  result <- rpcCallAny manager rpcUrls reqIdRef "eth_blockNumber" ([] :: [Value])
  pure $ result >>= parseReplayBlockNumber

parseReplayBlockNumber :: Value -> Either Text Integer
parseReplayBlockNumber = \case
  String quantity -> parseCanonicalHexQuantity "block number" quantity
  _ -> Left "Replay block number response must be a canonical hex string"

getBlockByNumber :: Manager -> [RpcEndpoint] -> IORef Integer -> Integer -> IO (Either Text BlockInfo)
getBlockByNumber manager rpcUrls reqIdRef blockNumber = do
  result <- rpcCallAny manager rpcUrls reqIdRef "eth_getBlockByNumber" [String $ "0x" <> intToHex blockNumber, Bool False]
  pure $ case result of
    Left err -> Left err
    Right (Object obj) -> Right $
      BlockInfo
        { biNumber = blockNumber
        , biHash = getString "hash" obj
        , biTimestamp = hexToInteger $ strip0x $ getString "timestamp" obj
        }
    Right _ -> Left "Expected block object"

getReplayBlockByNumber
  :: Manager -> [RpcEndpoint] -> IORef Integer -> Integer -> IO (Either Text BlockInfo)
getReplayBlockByNumber manager rpcUrls reqIdRef expectedBlock = do
  result <-
    rpcCallAny
      manager
      rpcUrls
      reqIdRef
      "eth_getBlockByNumber"
      [String $ "0x" <> intToHex expectedBlock, Bool False]
  pure $ result >>= parseReplayBlockInfo expectedBlock

parseReplayBlockInfo :: Integer -> Value -> Either Text BlockInfo
parseReplayBlockInfo expectedBlock = \case
  Object obj -> do
    numberText <- requiredString "number" obj
    number <- parseCanonicalHexQuantity "block number" numberText
    unless (number == expectedBlock) $
      Left "Replay block response number does not match the requested block"
    blockHash <- requiredString "hash" obj
    unless (isCanonicalHash blockHash) $
      Left "Replay block hash is not a canonical 32-byte hash"
    timestampText <- requiredString "timestamp" obj
    timestamp <- parseCanonicalHexQuantity "block timestamp" timestampText
    pure
      BlockInfo
        { biNumber = number
        , biHash = blockHash
        , biTimestamp = timestamp
        }
  _ -> Left "Replay block response must be a JSON object"

parseCanonicalHexQuantity :: Text -> Text -> Either Text Integer
parseCanonicalHexQuantity label value = do
  unless
    ( T.length value >= 3
        && "0x" `T.isPrefixOf` value
        && T.all isLowerHexDigit (T.drop 2 value)
        && (T.length value == 3 || T.index value 2 /= '0')
    )
    (Left $ "Replay " <> label <> " is not a canonical hex quantity")
  pure $ hexToInteger $ T.drop 2 value

getTransactionInfo :: Manager -> [RpcEndpoint] -> IORef Integer -> Text -> IO (Either Text TransactionInfo)
getTransactionInfo manager rpcUrls reqIdRef txHash = do
  result <- rpcCallAny manager rpcUrls reqIdRef "eth_getTransactionByHash" [String txHash]
  pure $ case result of
    Left err -> Left err
    Right (Object obj) -> do
      transactionHash <- requiredString "hash" obj
      fromAddress <- requiredString "from" obj
      toAddress <- requiredString "to" obj
      blockHash <- requiredString "blockHash" obj
      input <- requiredString "input" obj
      Right
        TransactionInfo
          { tiHash = transactionHash
          , tiFrom = fromAddress
          , tiTo = toAddress
          , tiBlockHash = blockHash
          , tiInput = decodeHex input
          }
    Right Null -> Left $ "Transaction not found: " <> txHash
    Right _ -> Left "Expected transaction object"

getReplayTransactionInfo
  :: Manager -> [RpcEndpoint] -> IORef Integer -> Text -> IO (Either Text TransactionInfo)
getReplayTransactionInfo manager rpcUrls reqIdRef txHash = do
  result <- rpcCallAny manager rpcUrls reqIdRef "eth_getTransactionByHash" [String txHash]
  pure $ result >>= parseReplayTransactionInfo

parseReplayTransactionInfo :: Value -> Either Text TransactionInfo
parseReplayTransactionInfo = \case
  Object obj -> do
    transactionHash <- requiredString "hash" obj
    fromAddress <- requiredString "from" obj
    toAddress <- requiredString "to" obj
    blockHash <- requiredString "blockHash" obj
    inputText <- requiredString "input" obj
    unless (isCanonicalHash transactionHash && isCanonicalHash blockHash) $
      Left "Replay transaction hash/block hash is not a canonical 32-byte hash"
    unless (isCanonicalAddress fromAddress && isCanonicalAddress toAddress) $
      Left "Replay transaction sender/target is not a canonical 20-byte address"
    input <- requiredHexBytes "transaction input" inputText
    pure
      TransactionInfo
        { tiHash = transactionHash
        , tiFrom = fromAddress
        , tiTo = toAddress
        , tiBlockHash = blockHash
        , tiInput = input
        }
  Null -> Left "Replay transaction was not found"
  _ -> Left "Replay transaction response must be a JSON object"

getTransactionFrom :: Manager -> [RpcEndpoint] -> IORef Integer -> Text -> IO (Maybe Text)
getTransactionFrom manager rpcUrls reqIdRef txHash = do
  result <- getTransactionInfo manager rpcUrls reqIdRef txHash
  case result of
    Right txInfo -> pure $ Just $ tiFrom txInfo
    Left err -> do
      logWarnEvery
        60
        "perps_indexer_transaction_sender_unavailable"
        "Perps history indexer could not read optional transaction sender metadata"
        [ field "tx_hash" txHash
        , field "error" err
        ]
      pure Nothing

executionUpdateData
  :: PerpsIndexerConfig
  -> Text
  -> Text
  -> TransactionInfo
  -> Integer
  -> Either Text [ByteString]
executionUpdateData cfg expectedTxHash expectedBlockHash txInfo orderId = do
  if normalizeHex (tiHash txInfo) == normalizeHex expectedTxHash
    then Right ()
    else Left "Execution transaction hash does not match the indexed terminal event"
  if normalizeHex (tiBlockHash txInfo) == normalizeHex expectedBlockHash
    then Right ()
    else Left "Execution transaction block hash does not match the indexed terminal event"
  if normalizeHex (tiTo txInfo) == normalizeHex (paOrderRouter $ picAddresses cfg)
    then Right ()
    else Left "Execution transaction target does not match the configured order router"
  decodeExecutionUpdateData orderId (tiInput txInfo)

deriveExecutionOracleMidpoint
  :: Manager
  -> PerpsIndexerConfig
  -> IORef Integer
  -> [ByteString]
  -> (Integer, Integer)
  -> IO (Either Text ExecutionOracleSnapshot)
deriveExecutionOracleMidpoint manager cfg reqIdRef updateData (minPublishTime, maxPublishTime) =
  case executionFeedIds of
    Left err -> pure $ Left err
    Right feedIds -> tryRpcUrls (indexerRpcEndpoints cfg) feedIds []
  where
    tryRpcUrls [] _ errors =
      pure $
        Left $
          "All RPC providers failed to parse the signed execution oracle payload"
            <> if null errors then "" else ": " <> T.intercalate "; " (reverse errors)
    tryRpcUrls (RpcEndpoint {..} : remaining) feedIds errors = do
      client <-
        newClientWithManager
          manager
          reqIdRef
          (RpcClientOptions reUrl reBearerToken "history-indexer")
      parsed <-
        parseUniquePythUpdateData
          client
          (paPletherOracle $ picAddresses cfg)
          updateData
          feedIds
          minPublishTime
          maxPublishTime
      case parsed of
        Right pricePoints ->
          pure $ deriveExecutionOracleSnapshot pricePoints
        Left err ->
          tryRpcUrls remaining feedIds (renderRpcError err : errors)

executionFeedIds :: Either Text [ByteString]
executionFeedIds = traverse decodeFeedId basketComponents
  where
    decodeFeedId component =
      let decoded = decodeHex $ bcFeedId component
       in if BS.length decoded == 32
            then Right decoded
            else Left $ "Configured Pyth feed ID is not 32 bytes: " <> bcFeedId component

renderRpcError :: RpcError -> Text
renderRpcError = T.pack . show

getLogs :: Manager -> [RpcEndpoint] -> IORef Integer -> PerpsIndexerConfig -> Integer -> Integer -> IO (Either Text [RpcLog])
getLogs manager rpcUrls reqIdRef cfg fromBlock toBlock = do
  perpsResult <- getLogsFor manager rpcUrls reqIdRef (perpsContractAddresses cfg) perpsEventTopics fromBlock toBlock
  transferResult <- getLogsFor manager rpcUrls reqIdRef [paUsdc $ picAddresses cfg] [transferTopic] fromBlock toBlock
  pure $ (<>) <$> perpsResult <*> transferResult

getLogsFor :: Manager -> [RpcEndpoint] -> IORef Integer -> [Text] -> [ByteString] -> Integer -> Integer -> IO (Either Text [RpcLog])
getLogsFor manager rpcUrls reqIdRef addresses eventTopics fromBlock toBlock = do
  let topics = map (String . ("0x" <>) . bytesToHex) eventTopics
      filterObject = object
        [ "address" .= addresses
        , "topics" .= [topics]
        , "fromBlock" .= ("0x" <> intToHex fromBlock)
        , "toBlock" .= ("0x" <> intToHex toBlock)
        ]
  result <- rpcCallAny manager rpcUrls reqIdRef "eth_getLogs" [filterObject]
  pure $ case result of
    Left err -> Left err
    Right (Array arr) -> traverse parseReplayLogEntry $ toList arr
    Right _ -> Left "Expected logs array"

getReplayLogs
  :: Manager -> [RpcEndpoint] -> IORef Integer -> PerpsIndexerConfig -> Integer -> Integer -> IO (Either Text [RpcLog])
getReplayLogs = getLogs

parseReplayLogEntry :: Value -> Either Text RpcLog
parseReplayLogEntry = \case
  Object obj -> do
    address <- requiredString "address" obj
    topics <- requiredStringArray "topics" obj
    dataText <- requiredString "data" obj
    txHash <- requiredString "transactionHash" obj
    blockNumber <- requiredHexQuantity "blockNumber" obj
    blockHash <- requiredString "blockHash" obj
    txIndex <- requiredHexQuantity "transactionIndex" obj
    logIndex <- requiredHexQuantity "logIndex" obj
    decodedTopics <- traverse (requiredHexBytes "topic") topics
    decodedData <- requiredHexBytes "data" dataText
    unless (isCanonicalAddress address) $
      Left "Replay log address is not a canonical 20-byte hex address"
    unless (isCanonicalHash txHash && isCanonicalHash blockHash) $
      Left "Replay log transaction/block hash is not a canonical 32-byte hash"
    unless (not $ null decodedTopics) $
      Left "Replay log has no event topic"
    pure
      RpcLog
        { rlAddress = address
        , rlTopics = decodedTopics
        , rlData = decodedData
        , rlTxHash = txHash
        , rlBlockNumber = blockNumber
        , rlBlockHash = blockHash
        , rlTxIndex = txIndex
        , rlLogIndex = logIndex
        }
  _ -> Left "Replay log entry must be a JSON object"

requiredStringArray :: Text -> KM.KeyMap Value -> Either Text [Text]
requiredStringArray name obj =
  case KM.lookup (Key.fromText name) obj of
    Just (Array values) -> traverse requireString $ toList values
    _ -> Left $ "Missing or invalid replay log field: " <> name
 where
  requireString = \case
    String value -> Right value
    _ -> Left $ "Replay log array field contains a non-string: " <> name

requiredHexQuantity :: Text -> KM.KeyMap Value -> Either Text Integer
requiredHexQuantity name obj = do
  value <- requiredString name obj
  case parseCanonicalHexQuantity ("log field " <> name) value of
    Left _ -> Left $ "Replay log field is not a canonical hex quantity: " <> name
    Right parsed -> Right parsed

requiredHexBytes :: Text -> Text -> Either Text ByteString
requiredHexBytes name value = do
  unless
    ( "0x" `T.isPrefixOf` value
        && even (T.length $ T.drop 2 value)
        && T.all isLowerHexDigit (T.drop 2 value)
    )
    (Left $ "Replay log field is not canonical hex bytes: " <> name)
  case B16.decode (TE.encodeUtf8 $ T.drop 2 value) of
    Right bytes -> Right bytes
    Left _ -> Left $ "Replay log field could not be decoded: " <> name

isCanonicalAddress :: Text -> Bool
isCanonicalAddress value =
  T.length value == 42
    && "0x" `T.isPrefixOf` value
    && T.all isHexDigit (T.drop 2 value)

isCanonicalHash :: Text -> Bool
isCanonicalHash value =
  T.length value == 66
    && "0x" `T.isPrefixOf` value
    && T.all isHexDigit (T.drop 2 value)

isHexDigit :: Char -> Bool
isHexDigit value =
  (value >= '0' && value <= '9')
    || (value >= 'a' && value <= 'f')
    || (value >= 'A' && value <= 'F')

isLowerHexDigit :: Char -> Bool
isLowerHexDigit value =
  (value >= '0' && value <= '9') || (value >= 'a' && value <= 'f')

rpcCallAny :: (Aeson.ToJSON params) => Manager -> [RpcEndpoint] -> IORef Integer -> Text -> params -> IO (Either Text Value)
rpcCallAny manager rpcUrls reqIdRef method params = tryUrls rpcUrls
  where
    tryUrls [] = pure $ Left "No RPC URLs configured"
    tryUrls [endpoint] = rpcCall manager endpoint reqIdRef method params
    tryUrls (endpoint : rest) = do
      result <- rpcCall manager endpoint reqIdRef method params
      case result of
        Right value -> pure $ Right value
        Left err -> do
          logWarnEvery
            60
            "perps_indexer_rpc_fallback"
            "Perps indexer RPC request failed; trying a fallback provider"
            [ field "rpc_method" method
            , field "remaining_provider_count" $ length rest
            , field "error" err
            ]
          tryUrls rest

rpcCall :: (Aeson.ToJSON params) => Manager -> RpcEndpoint -> IORef Integer -> Text -> params -> IO (Either Text Value)
rpcCall manager RpcEndpoint {..} reqIdRef methodName params = do
  startedAt <- getMonotonicTimeNSec
  reqId <- nextId reqIdRef
  let payload = object
        [ "jsonrpc" .= ("2.0" :: Text)
        , "method" .= methodName
        , "params" .= params
        , "id" .= reqId
        ]
  eResult <- try @SomeException $ do
    req <- parseRequest $ T.unpack reUrl
    let req' = req
          { method = "POST"
          , requestHeaders =
              ("Content-Type", "application/json")
                : maybe [] (\token -> [("Authorization", "Bearer " <> TE.encodeUtf8 token)]) reBearerToken
          , requestBody = RequestBodyLBS $ Aeson.encode payload
          , responseTimeout = responseTimeoutMicro rpcRequestTimeoutMicros
          }
    responseBody <$> httpLbs req' manager
  let outcome =
        case eResult of
          Left err -> Left $ rpcHttpExceptionText err
          Right body ->
            case Aeson.decode body of
              Just (Object obj) ->
                case KM.lookup (Key.fromText "result") obj of
                  Just value -> Right value
                  Nothing -> Left $ "RPC error: " <> T.pack (show $ KM.lookup (Key.fromText "error") obj)
              _ -> Left "Invalid JSON-RPC response"
  finishedAt <- getMonotonicTimeNSec
  recordIndexerRpcCall methodName (finishedAt - startedAt) (either (const True) (const False) outcome)
  pure outcome

data IndexerRpcMethodStats = IndexerRpcMethodStats
  { irmsRequestCount :: !Integer
  , irmsFailureCount :: !Integer
  , irmsTotalDurationNs :: !Word64
  , irmsMaxDurationNs :: !Word64
  }

data IndexerRpcMetrics = IndexerRpcMetrics
  { irmWindowStartedAtNs :: !Word64
  , irmMethods :: !(Map.Map Text IndexerRpcMethodStats)
  }

{-# NOINLINE indexerRpcMetrics #-}
indexerRpcMetrics :: IORef IndexerRpcMetrics
indexerRpcMetrics = unsafePerformIO $ do
  startedAt <- getMonotonicTimeNSec
  newIORef $ IndexerRpcMetrics startedAt Map.empty

recordIndexerRpcCall :: Text -> Word64 -> Bool -> IO ()
recordIndexerRpcCall methodName durationNs failed = do
  nowNs <- getMonotonicTimeNSec
  completedWindow <- atomicModifyIORef' indexerRpcMetrics $ \state ->
    let previous =
          Map.findWithDefault
            (IndexerRpcMethodStats 0 0 0 0)
            methodName
            (irmMethods state)
        updated =
          previous
            { irmsRequestCount = irmsRequestCount previous + 1
            , irmsFailureCount = irmsFailureCount previous + if failed then 1 else 0
            , irmsTotalDurationNs = irmsTotalDurationNs previous + durationNs
            , irmsMaxDurationNs = max (irmsMaxDurationNs previous) durationNs
            }
        methods = Map.insert methodName updated (irmMethods state)
     in if nowNs - irmWindowStartedAtNs state >= 60 * 1_000_000_000
          then (IndexerRpcMetrics nowNs Map.empty, Just methods)
          else (state {irmMethods = methods}, Nothing)
  mapM_ emitSummary $ maybe [] Map.toList completedWindow
 where
  emitSummary (rpcMethodName, IndexerRpcMethodStats {..}) =
    logInfo
      "rpc_request_summary"
      "Ethereum RPC request totals for the completed aggregation window"
      [ field "rpc_role" ("history-indexer" :: Text)
      , field "rpc_method" rpcMethodName
      , field "request_count" irmsRequestCount
      , field "failure_count" irmsFailureCount
      , field "total_duration_ms" $ nsToMs irmsTotalDurationNs
      , field "max_duration_ms" $ nsToMs irmsMaxDurationNs
      ]
  nsToMs value = (fromIntegral value :: Double) / 1_000_000

nextId :: IORef Integer -> IO Integer
nextId ref = atomicModifyIORef' ref $ \n -> (n + 1, n)

rpcRequestTimeoutMicros :: Int
rpcRequestTimeoutMicros = 25_000_000

indexedUint :: [ByteString] -> Int -> Maybe Integer
indexedUint topics idx
  | idx < length topics = Just $ bytesToInteger (topics !! idx)
  | otherwise = Nothing

indexedAddress :: [ByteString] -> Int -> Maybe Text
indexedAddress topics idx
  | idx < length topics = Just $ "0x" <> T.drop 24 (bytesToHex (topics !! idx))
  | otherwise = Nothing

indexedBytes32 :: [ByteString] -> Int -> Maybe Text
indexedBytes32 topics idx
  | idx < length topics && BS.length (topics !! idx) == 32 =
      Just $ "0x" <> bytesToHex (topics !! idx)
  | otherwise = Nothing

hexWordAt :: ByteString -> Int -> Text
hexWordAt bytes index =
  "0x" <> bytesToHex (BS.take 32 $ BS.drop (index * 32) bytes)

wordAt :: ByteString -> Int -> Integer
wordAt bytes index = bytesToInteger $ BS.take 32 $ BS.drop (index * 32) bytes

intWordAt :: ByteString -> Int -> Integer
intWordAt bytes index =
  let unsigned = wordAt bytes index
      signThreshold = 2 ^ (255 :: Int)
      modulo = 2 ^ (256 :: Int)
  in if unsigned >= signThreshold then unsigned - modulo else unsigned

bytesToInteger :: ByteString -> Integer
bytesToInteger = BS.foldl' (\acc byte -> acc * 256 + fromIntegral byte) 0

bytesToHex :: ByteString -> Text
bytesToHex = TE.decodeUtf8 . B16.encode

decodeHex :: Text -> ByteString
decodeHex txt =
  case B16.decode (TE.encodeUtf8 $ T.toLower $ strip0x txt) of
    Right bs -> bs
    Left _ -> ""

strip0x :: Text -> Text
strip0x txt
  | "0x" `T.isPrefixOf` txt = T.drop 2 txt
  | "0X" `T.isPrefixOf` txt = T.drop 2 txt
  | otherwise = txt

normalizeHex :: Text -> Text
normalizeHex txt = "0x" <> T.toLower (strip0x txt)

getString :: Text -> Aeson.Object -> Text
getString key obj = case KM.lookup (Key.fromText key) obj of
  Just (String s) -> s
  _ -> ""

requiredString :: Text -> Aeson.Object -> Either Text Text
requiredString key obj =
  case KM.lookup (Key.fromText key) obj of
    Just (String value) | not (T.null value) -> Right value
    _ -> Left $ "Transaction response is missing string field " <> key
