module Plether.Perps.HistoryIndexer
  ( PerpsAddresses (..)
  , PerpsIndexerConfig (..)
  , PerpsIndexerMode (..)
  , defaultPerpsAddresses
  , perpsIndexerName
  , runPerpsIndexer
  , perpsEventTopics
  , parsePerpsLog
  , parsePerpsLogForAddresses
  , parseUsdcTransfer
  , transferTopic
  , RpcLog (..)
  , BlockInfo (..)
  , TransactionIdentity (..)
  , TransactionInfo (..)
  , bindTransactionInfoToLog
  , transactionInfoFromRpcResults
  , ParsedPerpsLog (..)
  , ProtocolLogClassification (..)
  , classifyProtocolLog
  , classifyProtocolLogForAddresses
  , snapshotContractAddress
  , snapshotCallData
  , snapshotEthCallParams
  , snapshotReadFromRpcResult
  , accountSnapshotTargets
  , orderFailReasonName
  , terminalStatus
  , findNewestCommonCheckpoint
  , cursorBlockMatchesCanonical
  , canAdvanceCompletenessCursor
  , checkpointBlockNumbers
  , TradeCosts (..)
  , validateRpcLogBlockHash
  , decodeOpenTradeCosts
  , decodeCloseTradeCosts
  , decodeReplayTradeCosts
  , replayPreviewCallData
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

import Control.Applicative ((<|>))
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (SomeException, throwIO, try)
import Control.Monad (forM, forM_, forever, unless, when)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Pair)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.Char (isHexDigit)
import Data.Foldable (toList)
import Data.Function (on)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.List (find, nubBy, sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, isJust)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Ord (Down (..))
import Data.Time.Clock.POSIX (getPOSIXTime)
import Database.PostgreSQL.Simple (Connection, withTransaction)
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
import Plether.Database.Protocol
  ( deleteProtocolBlockCheckpointsFromBlock
  , deleteProtocolLedgerFromBlock
  , getProtocolBlockCheckpointsDescending
  , getProtocolIndexerCursor
  , insertProtocolLedgerEntry
  , setProtocolIndexerCursor
  , upsertProtocolBlockCheckpoints
  , upsertProtocolStateSnapshot
  )
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
  , updatePerpsOrderOracleEvidence
  , upsertPerpsOrderCommitted
  , upsertPerpsOrderTerminal
  )
import Plether.Ethereum.Abi (encodeAddress, encodeCall, encodeUint256)
import Plether.Ethereum.Client (EthClient (..), RpcError)
import Plether.Ethereum.Contracts.Perps (parseUniquePythUpdateData)
import Plether.Indexer.Contracts (keccak256Text)
import Plether.Logging (LogField, field, logError, logErrorEvery, logInfo, logInfoEvery, logWarn, logWarnEvery)
import Plether.Protocol.Governance
  ( DecodedGovernanceField (..)
  , DecodedGovernanceEvent (..)
  , GovernanceCategory (..)
  , GovernanceCategoryDefinition (..)
  , GovernanceContractRole (..)
  , GovernanceDecodeError (..)
  , GovernanceDecodedValue (..)
  , GovernanceEventDefinition (..)
  , GovernanceField (..)
  , GovernanceFieldType (..)
  , GovernanceLifecycle (..)
  , GovernanceRoleEventDefinition (..)
  , decodeGovernanceEvent
  , governanceCategoryDefinitions
  , governanceContractRoleKey
  , governanceRoleEvents
  )
import Plether.Protocol.Snapshots
  ( SnapshotArgument (..)
  , SnapshotBuildContext (..)
  , SnapshotCallPlan (..)
  , SnapshotContract (..)
  , SnapshotDocument (..)
  , SnapshotPlan (..)
  , SnapshotRead (..)
  , SnapshotSourceBlock (..)
  , SnapshotUnavailable (..)
  , buildSnapshot
  , accountLedgerSnapshotPlan
  , globalSnapshotPlans
  , snapshotAvailabilityToJson
  , snapshotDocumentToJson
  )
import Plether.Utils.Address (isValidAddress)
import Plether.Perps.IndexerOptions (ReplayOptions (..), validateReplayOptions)
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
import System.Timeout (timeout)

data PerpsAddresses = PerpsAddresses
  { paUsdc :: Text
  , paOrderRouter :: Text
  , paOrderRouterAdmin :: Text
  , paCfdEngine :: Text
  , paCfdEngineAdmin :: Text
  , paCfdEngineLens :: Text
  , paCfdEngineSettlementSidecar :: Text
  , paMarginClearinghouse :: Text
  , paPletherOracle :: Text
  , paAccountLens :: Text
  , paPublicLens :: Text
  , paHousePool :: Text
  , paSeniorVault :: Text
  , paJuniorVault :: Text
  }
  deriving stock (Show, Eq)

defaultPerpsAddresses :: PerpsAddresses
defaultPerpsAddresses =
  PerpsAddresses
    { paUsdc = "0x1647e41f49ED6D688936092B5a291c4B28106343"
    , paOrderRouter = "0x97A901dE2B267c307E264FD5F71403F8072F73e7"
    , paOrderRouterAdmin = "0x3073d6D021eC20b95a8b7C780f5c30c07036ff6C"
    , paCfdEngine = "0x3dc9C0A1f9C745A4B08BD5C2E6c7aE613561c20D"
    , paCfdEngineAdmin = "0xb256d4E88d649b2A149aA8B8caa3159260eFBc39"
    , paCfdEngineLens = "0x140067daAdd28bE4b04e649EEaCf6F5ECbEe8C79"
    , paCfdEngineSettlementSidecar = "0x288F70eC7cF0e16ae4FE4b91B5c266B047c83aFF"
    , paMarginClearinghouse = "0x2f98787F6dCC3b1f2E4a2AFa5acf410159b9F211"
    , paPletherOracle = "0xC69ec16EfB71F62984E9b2688396F34062277FdC"
    , paAccountLens = "0xC4C886A6F1D7CB22C833AC1b29f29Da43AfbcCd1"
    , paPublicLens = "0x4E202C06e2C378d1a85577ac631e592AB66f23FB"
    , paHousePool = "0xFA654f4c548130F09C3Fb962AbD4bE32c0357C18"
    , paSeniorVault = "0x4bAb5448C1BD9A48B978ABcb014F1a8F80F100A8"
    , paJuniorVault = "0x7258d6E91fbEFB8a16751575adbe9bBB3086D458"
    }

perpsIndexerName :: Text
perpsIndexerName = "perps-history-costs-v1"

data PerpsIndexerMode
  = PerpsIndexerLoop
  | PerpsIndexerOnce
  | PerpsIndexerReplay ReplayOptions
  deriving stock (Show, Eq)

data PerpsIndexerConfig = PerpsIndexerConfig
  { picRpcUrls :: [Text]
  , picTraceApiUrl :: Maybe Text
  , picChainId :: Integer
  , picReleaseId :: Text
  , picCalculationVersion :: Text
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
  deriving stock (Show, Eq)

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

-- | The fields that bind a transaction or receipt response to one canonical
-- block. They are kept separate from the transaction facts so unbound RPC
-- payloads can never be mistaken for exact ledger evidence.
data TransactionIdentity = TransactionIdentity
  { txiHash :: Text
  , txiBlockNumber :: Integer
  , txiBlockHash :: Text
  , txiTransactionIndex :: Integer
  }
  deriving stock (Show, Eq)

data TransactionInfo = TransactionInfo
  { tiFrom :: Maybe Text
  , tiTo :: Maybe Text
  , tiSelector :: Maybe Text
  , tiInput :: Maybe Text
  , tiNativeValue :: Maybe Integer
  , tiStatus :: Text
  , tiGasUsed :: Maybe Integer
  , tiEffectiveGasPrice :: Maybe Integer
  , tiTransactionIdentity :: Maybe TransactionIdentity
  , tiReceiptIdentity :: Maybe TransactionIdentity
  , tiReceiptLogs :: Maybe [RpcLog]
  , tiAvailability :: [Value]
  , tiTransactionAvailable :: Bool
  , tiReceiptAvailable :: Bool
  , tiEvidence :: Value
  }
  deriving stock (Show, Eq)

-- | Minimal canonical transaction facts used by bounded replay and optional
-- execution-evidence enrichment. Protocol ledger ingestion uses the richer
-- 'TransactionInfo' above so missing receipt fields remain explicit.
data ExecutionTransactionInfo = ExecutionTransactionInfo
  { etiHash :: Text
  , etiFrom :: Text
  , etiTo :: Text
  , etiBlockHash :: Text
  , etiInput :: ByteString
  }
  deriving stock (Show, Eq)

data ParsedPerpsLog
  = ParsedOrderCommitted Integer Text Int Value
  | ParsedOrderExecuted Integer Integer Value
  | ParsedOrderFailed Integer Int Text Value
  | ParsedPositionActivity Text Text Int (Maybe Integer) (Maybe Integer) (Maybe Integer) (Maybe Integer) Value
  | ParsedMarginActivity Text Text Integer Value
  | ParsedUsdcTransfer Text Text Integer Value
  deriving stock (Show, Eq)

-- | Release-wide events that can be decoded without relying on a
-- deployment-specific ABI. Any log not matching one of these signatures is
-- still retained in the immutable ledger as an explicit unclassified event.
data ProtocolLogClassification = ProtocolLogClassification
  { plcEventName :: Text
  , plcActionType :: Text
  , plcAccount :: Maybe Text
  , plcPayload :: Value
  , plcDecoded :: Bool
  , plcAvailability :: [Value]
  }
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

erc4626DepositTopic :: ByteString
erc4626DepositTopic = keccak256Text "Deposit(address,address,uint256,uint256)"

erc4626WithdrawTopic :: ByteString
erc4626WithdrawTopic = keccak256Text "Withdraw(address,address,address,uint256,uint256)"

-- | The Solidity event signature alone does not prove that the remaining log
-- shape matches the deployed ABI. Keep these shapes next to the topics so
-- every current-release decoder rejects truncated and extended payloads,
-- non-canonical indexed words, and narrow integers with dirty high bits.
data StaticLogShape = StaticLogShape
  { slsTopicCount :: Int
  , slsDataWordCount :: Int
  , slsIndexedAddresses :: [Int]
  , slsIndexedUintWidths :: [(Int, Int)]
  , slsDataUintWidths :: [(Int, Int)]
  }

orderCommittedShape :: StaticLogShape
orderCommittedShape =
  StaticLogShape
    { slsTopicCount = 3
    , slsDataWordCount = 1
    , slsIndexedAddresses = [2]
    , slsIndexedUintWidths = [(1, 64)]
    , slsDataUintWidths = [(0, 8)]
    }

orderExecutedShape :: StaticLogShape
orderExecutedShape =
  StaticLogShape
    { slsTopicCount = 2
    , slsDataWordCount = 1
    , slsIndexedAddresses = []
    , slsIndexedUintWidths = [(1, 64)]
    , slsDataUintWidths = []
    }

orderFailedShape :: StaticLogShape
orderFailedShape =
  StaticLogShape
    { slsTopicCount = 2
    , slsDataWordCount = 1
    , slsIndexedAddresses = []
    , slsIndexedUintWidths = [(1, 64)]
    , slsDataUintWidths = [(0, 8)]
    }

positionShape :: StaticLogShape
positionShape =
  StaticLogShape
    { slsTopicCount = 2
    , slsDataWordCount = 4
    , slsIndexedAddresses = [1]
    , slsIndexedUintWidths = []
    , slsDataUintWidths = [(0, 8)]
    }

singleAccountAmountShape :: StaticLogShape
singleAccountAmountShape =
  StaticLogShape
    { slsTopicCount = 2
    , slsDataWordCount = 1
    , slsIndexedAddresses = [1]
    , slsIndexedUintWidths = []
    , slsDataUintWidths = []
    }

marginTransferShape :: StaticLogShape
marginTransferShape =
  StaticLogShape
    { slsTopicCount = 3
    , slsDataWordCount = 1
    , slsIndexedAddresses = [1, 2]
    , slsIndexedUintWidths = []
    , slsDataUintWidths = []
    }

erc4626DepositShape :: StaticLogShape
erc4626DepositShape =
  StaticLogShape
    { slsTopicCount = 3
    , slsDataWordCount = 2
    , slsIndexedAddresses = [1, 2]
    , slsIndexedUintWidths = []
    , slsDataUintWidths = []
    }

erc4626WithdrawShape :: StaticLogShape
erc4626WithdrawShape =
  StaticLogShape
    { slsTopicCount = 4
    , slsDataWordCount = 2
    , slsIndexedAddresses = [1, 2, 3]
    , slsIndexedUintWidths = []
    , slsDataUintWidths = []
    }

transferTopic :: ByteString
transferTopic = keccak256Text "Transfer(address,address,uint256)"

perpsEventTopics :: [ByteString]
perpsEventTopics =
  [ orderCommittedTopic
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
          threadDelay (picPollIntervalMicros cfg * 2)
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
          when (not indexed) $ threadDelay (picPollIntervalMicros cfg)

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
      getReplayCurrentBlockNumber manager (picRpcUrls cfg) reqIdRef
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
        (picRpcUrls cfg)
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
      getReplayBlockByNumber manager (picRpcUrls cfg) reqIdRef toBlock
  blockInfos <- forM logBlockNumbers $ \blockNumber -> do
    blockInfo <-
      requireRpc "eth_getBlockByNumber(replay-log)" $
        getReplayBlockByNumber manager (picRpcUrls cfg) reqIdRef blockNumber
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
      getReplayBlockByNumber manager (picRpcUrls cfg) reqIdRef toBlock
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
        getReplayExecutionTransactionInfo manager (picRpcUrls cfg) reqIdRef (rlTxHash logEntry)
    unless
      (normalizeHex (etiHash txInfo) == normalizeHex (rlTxHash logEntry))
      (fail "Replay transaction hash does not match its canonical log")
    unless
      (normalizeHex (etiBlockHash txInfo) == normalizeHex (rlBlockHash logEntry))
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
        , prlTransactionFrom = etiFrom txInfo
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
        getReplayBlockByNumber manager (picRpcUrls cfg) reqIdRef cursorBlock
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
  currentBlock <-
    requireRpc "eth_blockNumber" $
      getCurrentBlockNumber manager (picRpcUrls cfg) reqIdRef
  let safeBlock = max 0 (currentBlock - picConfirmations cfg)
  (storedLastBlock, storedLastHash) <- withDb pool $ \conn ->
    getProtocolIndexerCursor conn (picReleaseId cfg) (picIndexerName cfg)
  verifyCursor manager pool cfg reqIdRef storedLastBlock storedLastHash
  (lastBlock, _) <- withDb pool $ \conn ->
    getProtocolIndexerCursor conn (picReleaseId cfg) (picIndexerName cfg)
  let startBlock =
        fromMaybe
          (max (picStartBlock cfg) (lastBlock + 1))
          explicitFrom
      cappedToBlock = maybe safeBlock (min safeBlock) explicitTo
      endBlock = min cappedToBlock (startBlock + picBatchSize cfg - 1)
  if startBlock > endBlock
    then pure False
    else do
      -- Resolve both boundaries plus every event block before the first write.
      -- This preserves exact log/block identity while retaining checkpoints for
      -- empty ranges and efficient common-ancestor recovery.
      rangeEndInfo <-
        requireRpc "eth_getBlockByNumber(range-end-before)" $
          getBlockByNumber manager (picRpcUrls cfg) reqIdRef endBlock
      logs <-
        requireRpc "eth_getLogs" $
          getLogs manager (picRpcUrls cfg) reqIdRef cfg startBlock endBlock
      forM_ logs $ \logEntry ->
        either (fail . T.unpack) pure $
          validateReplayLogScope
            startBlock
            endBlock
            (perpsAddresses cfg)
            logEntry
      let orderedLogs =
            sortOn
              (\logEntry ->
                ( rlBlockNumber logEntry
                , rlTxIndex logEntry
                , rlLogIndex logEntry
                )
              )
              logs
          blockNumbers =
            checkpointBlockNumbers
              startBlock
              endBlock
              (map rlBlockNumber orderedLogs)
          txHashesByKey =
            Map.fromList
              [ (normalizeHex $ rlTxHash logEntry, rlTxHash logEntry)
              | logEntry <- orderedLogs
              ]
      blockPairs <- forM blockNumbers $ \blockNumber -> do
        blockInfo <-
          if blockNumber == endBlock
            then pure rangeEndInfo
            else
              requireRpc "eth_getBlockByNumber(log-or-checkpoint)" $
                getBlockByNumber
                  manager
                  (picRpcUrls cfg)
                  reqIdRef
                  blockNumber
        pure (blockNumber, blockInfo)
      let blockInfoByNumber = Map.fromList blockPairs
      forM_ orderedLogs $ \logEntry -> do
        blockInfo <-
          maybe
            (fail "Missing canonical block metadata for monitored log block")
            pure
            (Map.lookup (rlBlockNumber logEntry) blockInfoByNumber)
        either (fail . T.unpack) pure $
          validateRpcLogBlockHash logEntry blockInfo

      transactionPairs <- forM (Map.toList txHashesByKey) $ \(txKey, txHash) -> do
        transactionInfo <-
          getTransactionInfo manager (picRpcUrls cfg) reqIdRef txHash
        pure (txKey, transactionInfo)
      let transactionByHash = Map.fromList transactionPairs
      boundTransactionPairs <- forM orderedLogs $ \logEntry -> do
        blockInfo <-
          maybe
            (fail "Missing canonical block metadata for transaction evidence")
            pure
            (Map.lookup (rlBlockNumber logEntry) blockInfoByNumber)
        transactionInfo <-
          maybe
            (fail "Missing transaction evidence for monitored log")
            pure
            (Map.lookup (normalizeHex $ rlTxHash logEntry) transactionByHash)
        boundInfo <-
          either (fail . T.unpack) pure $
            bindTransactionInfoToLog blockInfo logEntry transactionInfo
        pure (logEvidenceKey logEntry, boundInfo)
      let boundTransactionByLog = Map.fromList boundTransactionPairs

      endInfo <-
        requireRpc "eth_getBlockByNumber(range-end-after)" $
          getBlockByNumber manager (picRpcUrls cfg) reqIdRef endBlock
      unless
        (normalizeHex (biHash rangeEndInfo) == normalizeHex (biHash endInfo))
        (fail "Canonical end block changed while validating the fetched log range")

      enrichedLogs <- forM orderedLogs $ \logEntry -> do
        blockInfo <-
          maybe
            (fail "Missing canonical block metadata for enriched log")
            pure
            (Map.lookup (rlBlockNumber logEntry) blockInfoByNumber)
        transactionInfo <-
          maybe
            (fail "Missing bound transaction evidence for enriched log")
            pure
            (Map.lookup (logEvidenceKey logEntry) boundTransactionByLog)
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
        pure (logEntry, blockInfo, transactionInfo, tradeCosts)

      globalSnapshots <-
        collectGlobalSnapshots manager cfg reqIdRef endInfo
      accountSnapshots <-
        collectAccountSnapshots
          manager
          cfg
          reqIdRef
          blockInfoByNumber
          orderedLogs

      (certifiedProtocolContinuity, certifiedLegacyContinuity) <-
        withDb pool $ \conn -> withTransaction conn $ do
          let releaseRouter = paOrderRouter $ picAddresses cfg
          lockPerpsIndexerTransaction
            conn
            (picChainId cfg)
            (picIndexerName cfg)
            releaseRouter
          (protocolCursor, protocolCursorHash) <-
            getProtocolIndexerCursor
              conn
              (picReleaseId cfg)
              (picIndexerName cfg)
          (legacyCursor, legacyCursorHash) <-
            getPerpsIndexerLastBlock
              conn
              (picChainId cfg)
              (picIndexerName cfg)
              releaseRouter
          let certifiesProtocolContinuity =
                canAdvanceCompletenessCursor
                  (picStartBlock cfg)
                  protocolCursor
                  startBlock
                  endBlock
              certifiesLegacyContinuity =
                canCertifyIndexedRange
                  (picStartBlock cfg)
                  legacyCursor
                  startBlock
                  endBlock

          -- Close the gap between the initial cursor verification and commit.
          -- A certifying append must still share the persisted boundary hash.
          when certifiesProtocolContinuity $
            forM_ protocolCursorHash $ \persistedHash -> do
              boundaryInfo <-
                requireRpc "eth_getBlockByNumber(protocol-cursor-boundary)" $
                  getBlockByNumber
                    manager
                    (picRpcUrls cfg)
                    reqIdRef
                    (startBlock - 1)
              either (fail . T.unpack) pure $
                validateIndexedBoundary
                  (startBlock - 1)
                  persistedHash
                  boundaryInfo
          when certifiesLegacyContinuity $
            forM_ legacyCursorHash $ \persistedHash -> do
              boundaryInfo <-
                requireRpc "eth_getBlockByNumber(legacy-cursor-boundary)" $
                  getBlockByNumber
                    manager
                    (picRpcUrls cfg)
                    reqIdRef
                    (startBlock - 1)
              either (fail . T.unpack) pure $
                validateIndexedBoundary
                  (startBlock - 1)
                  persistedHash
                  boundaryInfo

          affectedVolumeTimes <-
            fmap catMaybes $
              forM enrichedLogs $
                \(logEntry, blockInfo, transactionInfo, tradeCosts) -> do
                  persistProtocolLog
                    conn
                    cfg
                    blockInfo
                    transactionInfo
                    logEntry
                  processLog
                    conn
                    cfg
                    blockInfo
                    (tiFrom transactionInfo)
                    tradeCosts
                    logEntry

          persistGlobalSnapshots conn cfg endInfo globalSnapshots
          persistAccountSnapshots conn cfg accountSnapshots
          upsertProtocolBlockCheckpoints
            conn
            (picReleaseId cfg)
            (picIndexerName cfg)
            [ (blockNumber, biHash blockInfo)
            | (blockNumber, blockInfo) <- blockPairs
            ]

          let affectedVolumeMinutes =
                Set.toAscList $
                  Set.fromList $
                    map (\timestamp -> (timestamp `div` 60) * 60) affectedVolumeTimes
          when (picCandleWriteMode cfg == PerpsCandleWritesDual) $ do
            recomputeMarketVolumeHierarchyBatch
              conn
              (picChainId cfg)
              releaseRouter
              affectedVolumeMinutes
              (picCandleLatenessSeconds cfg)
            when certifiesLegacyContinuity $
              advanceMarketVolumeCoverage
                conn
                (picChainId cfg)
                releaseRouter
                (biTimestamp endInfo)
                0

          when certifiesProtocolContinuity $
            setProtocolIndexerCursor
              conn
              (picReleaseId cfg)
              (picIndexerName cfg)
              endBlock
              (Just $ biHash endInfo)
          when certifiesLegacyContinuity $
            setPerpsIndexerState
              conn
              (picChainId cfg)
              (picIndexerName cfg)
              releaseRouter
              (picStartBlock cfg)
              endBlock
              (Just $ biHash endInfo)
          pure (certifiesProtocolContinuity, certifiesLegacyContinuity)

      logInfoEvery
        300
        "perps_indexer_progress"
        "Perps history indexer processed a confirmed block range"
        [ field "from_block" startBlock
        , field "to_block" endBlock
        , field "safe_head_block" safeBlock
        , field "event_count" $ length orderedLogs
        , field "canonical_progress_certified" certifiedProtocolContinuity
        , field "legacy_progress_certified" certifiedLegacyContinuity
        , field "indexed_through_timestamp" $ biTimestamp endInfo
        ]
      pure True

-- | Collect every release-global snapshot at the confirmed range-end block.
-- Calls are deliberately independent: an unavailable archive read affects only
-- its own fields and never prevents event ingestion or the remaining reads.
collectGlobalSnapshots
  :: Manager
  -> PerpsIndexerConfig
  -> IORef Integer
  -> BlockInfo
  -> IO [SnapshotDocument]
collectGlobalSnapshots manager cfg reqIdRef blockInfo =
  forM globalSnapshotPlans $
    collectSnapshotDocument manager cfg reqIdRef blockInfo

-- | Collect one account-ledger snapshot for each distinct trading account and
-- confirmed block affected by this range. Targets are planned before any RPC
-- reads, so multiple logs for the same account in one block still issue only
-- one AccountLens call.
collectAccountSnapshots
  :: Manager
  -> PerpsIndexerConfig
  -> IORef Integer
  -> Map.Map Integer BlockInfo
  -> [RpcLog]
  -> IO [(BlockInfo, SnapshotDocument)]
collectAccountSnapshots manager cfg reqIdRef blockCache logs =
  forM (accountSnapshotTargets (picAddresses cfg) logs) $
    \(blockNumber, account) -> do
      blockInfo <-
        maybe
          (fail "Indexer block cache is missing an account snapshot block")
          pure
          (Map.lookup blockNumber blockCache)
      document <-
        collectSnapshotDocument
          manager
          cfg
          reqIdRef
          blockInfo
          (accountLedgerSnapshotPlan account)
      pure (blockInfo, document)

collectSnapshotDocument
  :: Manager
  -> PerpsIndexerConfig
  -> IORef Integer
  -> BlockInfo
  -> SnapshotPlan
  -> IO SnapshotDocument
collectSnapshotDocument manager cfg reqIdRef blockInfo plan = do
  snapshotReads <-
    forM (spCalls plan) $
      collectSnapshotRead manager cfg reqIdRef blockInfo
  pure $
    buildSnapshot
      SnapshotBuildContext
        { sbcReleaseId = picReleaseId cfg
        , sbcCalculationVersion = picCalculationVersion cfg
        , sbcSourceBlock =
            SnapshotSourceBlock
              { ssbNumber = biNumber blockInfo
              , ssbHash = Just $ biHash blockInfo
              , ssbTimestamp = Just $ biTimestamp blockInfo
              }
        }
      plan
      snapshotReads

collectSnapshotRead
  :: Manager
  -> PerpsIndexerConfig
  -> IORef Integer
  -> BlockInfo
  -> SnapshotCallPlan
  -> IO SnapshotRead
collectSnapshotRead manager cfg reqIdRef blockInfo callPlan =
  case snapshotContractAddress (picAddresses cfg) (scpContract callPlan) of
    Nothing ->
      pure $
        unavailableSnapshotRead callPlan "release_contract_unavailable"
    Just contractAddress ->
      case snapshotCallData callPlan of
        Left failure ->
          pure SnapshotRead
            { srCallId = scpId callPlan
            , srResult = Left failure
            }
        Right callData -> do
          rpcResult <-
            rpcCallAny
              manager
              (picRpcUrls cfg)
              reqIdRef
              "eth_call"
              (snapshotEthCallParams contractAddress callData blockInfo)
          pure $ snapshotReadFromRpcResult callPlan rpcResult

-- | Build an EIP-1898 canonical block-hash request for a snapshot read.
-- Pinning the call to the already-resolved range-end hash prevents a provider
-- from resolving a numeric block tag against a different fork.
snapshotEthCallParams :: Text -> ByteString -> BlockInfo -> [Value]
snapshotEthCallParams contractAddress callData blockInfo =
  [ object
      [ "to" .= contractAddress
      , "data" .= ("0x" <> bytesToHex callData)
      ]
  , object
      [ "blockHash" .= biHash blockInfo
      , "requireCanonical" .= True
      ]
  ]

persistGlobalSnapshots
  :: Connection
  -> PerpsIndexerConfig
  -> BlockInfo
  -> [SnapshotDocument]
  -> IO ()
persistGlobalSnapshots conn cfg blockInfo =
  mapM_ $ persistSnapshotAtBlock conn cfg blockInfo

persistAccountSnapshots
  :: Connection
  -> PerpsIndexerConfig
  -> [(BlockInfo, SnapshotDocument)]
  -> IO ()
persistAccountSnapshots conn cfg =
  mapM_ $ \(blockInfo, document) ->
    persistSnapshotAtBlock conn cfg blockInfo document

persistSnapshotAtBlock
  :: Connection
  -> PerpsIndexerConfig
  -> BlockInfo
  -> SnapshotDocument
  -> IO ()
persistSnapshotAtBlock conn cfg blockInfo document =
  upsertProtocolStateSnapshot
    conn
    (picReleaseId cfg)
    (sdScope document)
    (biNumber blockInfo)
    (biHash blockInfo)
    (biTimestamp blockInfo)
    (snapshotDocumentToJson document)
    (Aeson.toJSON $ map snapshotAvailabilityToJson $ sdAvailability document)
    (picCalculationVersion cfg)

-- | Plan one AccountLens read per canonical @(block, trading account)@ pair.
-- The account field on governance and tranche actions has a different meaning,
-- so only actions that can mutate the trading ledger are eligible.
accountSnapshotTargets
  :: PerpsAddresses
  -> [RpcLog]
  -> [(Integer, Text)]
accountSnapshotTargets addresses =
  Set.toAscList . Set.fromList . catMaybes . map targetForLog
  where
    targetForLog logEntry = do
      (actionType, account) <-
        case parsePerpsLogForAddresses addresses logEntry of
          Just parsedEvent -> do
            affectedAccount <- parsedAccount parsedEvent
            pure (fst $ parsedAction parsedEvent, affectedAccount)
          Nothing -> do
            classified <- classifyProtocolLogForAddresses addresses logEntry
            affectedAccount <- plcAccount classified
            pure (plcActionType classified, affectedAccount)
      if actionType `Set.member` accountLedgerActionTypes
          && isValidAddress account
        then Just (rlBlockNumber logEntry, T.toLower account)
        else Nothing

accountLedgerActionTypes :: Set.Set Text
accountLedgerActionTypes =
  Set.fromList
    [ "order_commitment"
    , "order_execution"
    , "order_cleanup"
    , "position_open"
    , "position_close"
    , "position_change"
    , "liquidation"
    , "margin_add"
    , "margin_deposit"
    , "margin_withdraw"
    ]

-- | Resolve a versioned snapshot plan's symbolic contract against the release
-- addresses supplied to this indexer.
snapshotContractAddress
  :: PerpsAddresses
  -> SnapshotContract
  -> Maybe Text
snapshotContractAddress addresses (ReleaseContract contractName) =
  case contractName of
    "orderRouter" -> present $ paOrderRouter addresses
    "orderRouterAdmin" -> present $ paOrderRouterAdmin addresses
    "cfdEngine" -> present $ paCfdEngine addresses
    "cfdEngineAdmin" -> present $ paCfdEngineAdmin addresses
    "marginClearinghouse" -> present $ paMarginClearinghouse addresses
    "pletherOracle" -> present $ paPletherOracle addresses
    "accountLens" -> present $ paAccountLens addresses
    "publicLens" -> present $ paPublicLens addresses
    "housePool" -> present $ paHousePool addresses
    "seniorVault" -> present $ paSeniorVault addresses
    "juniorVault" -> present $ paJuniorVault addresses
    _ -> Nothing
  where
    present address
      | isValidAddress address = Just address
      | otherwise = Nothing

-- | Encode a snapshot call without permitting malformed addresses or values to
-- be silently coerced into valid-looking ABI words.
snapshotCallData
  :: SnapshotCallPlan
  -> Either SnapshotUnavailable ByteString
snapshotCallData callPlan =
  encodeCall (scpSignature callPlan) <$> traverse encodeArgument (scpArguments callPlan)
  where
    encodeArgument = \case
      UintArgument value
        | value >= 0 && value < twoTo256 ->
            Right $ encodeUint256 value
        | otherwise ->
            Left $
              snapshotUnavailable "invalid_snapshot_uint_argument"
      AddressArgument address
        | isValidAddress address ->
            Right $ encodeAddress address
        | otherwise ->
            Left $
              snapshotUnavailable "invalid_snapshot_address_argument"

-- | Convert a raw provider response into the evidence model. Provider errors,
-- archive gaps, and malformed JSON-RPC payloads intentionally collapse to one
-- public reason with no provider URL, node message, or internal error detail.
snapshotReadFromRpcResult
  :: SnapshotCallPlan
  -> Either Text Value
  -> SnapshotRead
snapshotReadFromRpcResult callPlan rpcResult =
  SnapshotRead
    { srCallId = scpId callPlan
    , srResult =
        case rpcResult of
          Right (String hexResult) ->
            maybe
              (Left archiveUnavailable)
              Right
              (decodeRpcHexResult hexResult)
          _ -> Left archiveUnavailable
    }
  where
    archiveUnavailable =
      snapshotUnavailable "archive_state_unavailable"

unavailableSnapshotRead :: SnapshotCallPlan -> Text -> SnapshotRead
unavailableSnapshotRead callPlan reason =
  SnapshotRead
    { srCallId = scpId callPlan
    , srResult = Left $ snapshotUnavailable reason
    }

snapshotUnavailable :: Text -> SnapshotUnavailable
snapshotUnavailable reason =
  SnapshotUnavailable
    { suReason = reason
    , suDetail = Nothing
    }

decodeRpcHexResult :: Text -> Maybe ByteString
decodeRpcHexResult value
  | not (T.isPrefixOf "0x" value || T.isPrefixOf "0X" value) = Nothing
  | odd $ T.length digits = Nothing
  | not $ T.all isHexDigit digits = Nothing
  | otherwise =
      either (const Nothing) Just $
        B16.decode $
          TE.encodeUtf8 $
            T.toLower digits
  where
    digits = T.drop 2 value

twoTo256 :: Integer
twoTo256 = 2 ^ (256 :: Int)

-- | A completeness cursor may certify only a range that starts at the release
-- floor or directly after the previously certified block. Bounded repair
-- backfills can still populate idempotent projections and checkpoints, but a
-- gap can never advance either the protocol or legacy cursor.
canAdvanceCompletenessCursor
  :: Integer
  -> Integer
  -> Integer
  -> Integer
  -> Bool
canAdvanceCompletenessCursor deploymentBlock currentCursor rangeStart rangeEnd =
  rangeEnd >= rangeStart
    && rangeStart == max deploymentBlock (currentCursor + 1)

-- | Persist both range boundaries plus every block that emitted a monitored
-- log. Boundaries keep empty ranges recoverable; event blocks make the common
-- ancestor walk precise around protocol activity.
checkpointBlockNumbers :: Integer -> Integer -> [Integer] -> [Integer]
checkpointBlockNumbers rangeStart rangeEnd observedLogBlocks =
  Set.toAscList $
    Set.insert rangeStart $
      Set.insert rangeEnd $
        Set.fromList observedLogBlocks

-- | Walk stored checkpoints newest-first and return the newest one whose hash
-- still matches the canonical chain. Resolver failures are skipped so one
-- unavailable archive block does not hide an older verifiable ancestor.
findNewestCommonCheckpoint
  :: Monad m
  => (Integer -> m (Either e Text))
  -> [(Integer, Text)]
  -> m (Maybe (Integer, Text))
findNewestCommonCheckpoint resolve =
  go . sortOn (Down . fst)
  where
    go [] = pure Nothing
    go ((blockNumber, storedHash) : remaining) = do
      canonicalHash <- resolve blockNumber
      case canonicalHash of
        Right currentHash
          | normalizeHex currentHash == normalizeHex storedHash ->
              pure $ Just (blockNumber, storedHash)
        _ -> go remaining
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
  eBlock <- getBlockByNumber manager (picRpcUrls cfg) reqIdRef lastBlock
  case cursorBlockMatchesCanonical storedHash eBlock of
    Right True -> pure ()
    Right False -> rewindToCommonAncestor
    Left reason -> do
      logWarnEvery
        60
        "perps_indexer_cursor_verification_failed"
        "Perps indexer could not verify its cursor block hash; aborting this range"
        [ field "cursor_block" lastBlock
        , field "reason" reason
        ]
      fail $ T.unpack reason
  where
    rewindToCommonAncestor = do
      checkpoints <- withDb pool $ \conn ->
        getProtocolBlockCheckpointsDescending
          conn
          (picReleaseId cfg)
          (picIndexerName cfg)
          (lastBlock - 1)
      commonAncestor <-
        findNewestCommonCheckpoint
          ( \blockNumber ->
              fmap (fmap biHash) $
                getBlockByNumber manager (picRpcUrls cfg) reqIdRef blockNumber
          )
          (filter ((>= picStartBlock cfg) . fst) checkpoints)
      let rewindBlock =
            maybe
              (picStartBlock cfg)
              ((+ 1) . fst)
              commonAncestor
          newCursor =
            maybe
              (max 0 $ picStartBlock cfg - 1)
              fst
              commonAncestor
          newCursorHash = snd <$> commonAncestor
          releaseRouter = paOrderRouter $ picAddresses cfg
      logWarn
        "perps_indexer_reorg_detected"
        "Perps indexer detected a block hash mismatch and rewound to its newest verified checkpoint"
        [ field "mismatch_block" lastBlock
        , field "rewind_to_block" newCursor
        , field "common_ancestor_found" $ isJust commonAncestor
        ]
      withDb pool $ \conn -> withTransaction conn $ do
        lockPerpsIndexerTransaction
          conn
          (picChainId cfg)
          (picIndexerName cfg)
          releaseRouter
        affectedVolumeMinutes <-
          if picCandleWriteMode cfg == PerpsCandleWritesDual
            then
              invalidateMarketVolumeFromBlock
                conn
                (picChainId cfg)
                releaseRouter
                rewindBlock
            else pure []
        invalidateCompetitionSnapshotsForReleaseRebuild
          conn
          (picChainId cfg)
          releaseRouter
        deletePerpsHistoryFromBlock
          conn
          (picChainId cfg)
          releaseRouter
          rewindBlock
        deleteProtocolLedgerFromBlock conn (picReleaseId cfg) rewindBlock
        deleteProtocolBlockCheckpointsFromBlock
          conn
          (picReleaseId cfg)
          (picIndexerName cfg)
          rewindBlock
        recomputeMarketVolumeHierarchyBatch
          conn
          (picChainId cfg)
          releaseRouter
          affectedVolumeMinutes
          (picCandleLatenessSeconds cfg)
        setProtocolIndexerCursor
          conn
          (picReleaseId cfg)
          (picIndexerName cfg)
          newCursor
          newCursorHash
        setPerpsIndexerState
          conn
          (picChainId cfg)
          (picIndexerName cfg)
          releaseRouter
          (picStartBlock cfg)
          newCursor
          newCursorHash

-- | Treat an unavailable cursor header as a hard verification failure. The
-- caller must not append a new range until the existing cursor's canonical
-- identity has been proven.
cursorBlockMatchesCanonical
  :: Text
  -> Either e BlockInfo
  -> Either Text Bool
cursorBlockMatchesCanonical storedHash = \case
  Left _ -> Left "cursor_block_unavailable"
  Right blockInfo ->
    Right $
      normalizeHex (biHash blockInfo) == normalizeHex storedHash

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
        (getExecutionTransactionInfo manager (picRpcUrls cfg) reqIdRef $ peerTerminalTxHash candidate)
    case txResult >>= validateExecutionTransaction candidate of
      Left err ->
        logExecutionEvidenceFailure candidate "transaction" err
      Right txInfo -> do
        let needsOracle =
              peerOracleDerivationVersion candidate
                /= Just executionOracleDerivationVersion
            needsEconomics =
              peerExecutionEconomicsVersion candidate
                /= Just executionEconomicsDerivationVersion
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
  -> ExecutionTransactionInfo
  -> Either Text ExecutionTransactionInfo
validateExecutionTransaction candidate txInfo = do
  unless
    (normalizeHex (etiHash txInfo) == normalizeHex (peerTerminalTxHash candidate))
    (Left "Transaction hash does not match the indexed terminal event")
  unless
    (normalizeHex (etiBlockHash txInfo) == normalizeHex (peerTerminalBlockHash candidate))
    (Left "Transaction block hash does not match the indexed terminal event")
  pure txInfo

deriveOrderExecutionOracle
  :: Manager
  -> PerpsIndexerConfig
  -> IORef Integer
  -> PerpsExecutionEvidenceRow
  -> ExecutionTransactionInfo
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
  -> ExecutionTransactionInfo
  -> IO (Either Text (Map.Map Integer TradeExecutionEvidence))
deriveTransactionExecutionEconomics manager cfg reqIdRef txInfo = do
  rpcTrace <-
    rpcCallAny
      manager
      (picRpcUrls cfg)
      reqIdRef
      "debug_traceTransaction"
      [ String $ etiHash txInfo
      , object
          [ "tracer" .= ("callTracer" :: Text)
          , "timeout" .= ("20s" :: Text)
          ]
      ]
  case rpcTrace >>= decodeTrace of
    Right evidence -> pure $ Right evidence
    Left rpcErr ->
      case picTraceApiUrl cfg of
        Nothing -> pure $ Left rpcErr
        Just traceApiUrl -> do
          explorerTrace <- fetchBlockscoutTrace manager traceApiUrl (etiHash txInfo)
          pure $
            case explorerTrace >>= decodeTrace of
              Right evidence -> Right evidence
              Left explorerErr ->
                Left $
                  "RPC call trace failed ("
                    <> rpcErr
                    <> "); trace API fallback failed ("
                    <> explorerErr
                    <> ")"
  where
    decodeTrace trace = do
      validateCanonicalTraceRoot txInfo trace
      decodeTradeExecutionEvidence
        (paOrderRouter $ picAddresses cfg)
        (paCfdEngine $ picAddresses cfg)
        (paCfdEngineSettlementSidecar $ picAddresses cfg)
        (paPletherOracle $ picAddresses cfg)
        trace

validateCanonicalTraceRoot :: ExecutionTransactionInfo -> Value -> Either Text ()
validateCanonicalTraceRoot txInfo = \case
  Object trace -> do
    traceFrom <- requiredString "from" trace
    traceTo <- requiredString "to" trace
    traceInput <- requiredString "input" trace
    unless (normalizeHex traceFrom == normalizeHex (etiFrom txInfo)) $
      Left "Trace root sender does not match the canonical transaction"
    unless (normalizeHex traceTo == normalizeHex (etiTo txInfo)) $
      Left "Trace root target does not match the canonical transaction"
    unless (decodeHex traceInput == etiInput txInfo) $
      Left "Trace root calldata does not match the canonical transaction"
  _ -> Left "Trace root must be an object"

fetchBlockscoutTrace :: Manager -> Text -> Text -> IO (Either Text Value)
fetchBlockscoutTrace manager traceApiUrl txHash = do
  let url =
        T.dropWhileEnd (== '/') traceApiUrl
          <> "/transactions/"
          <> normalizeHex txHash
          <> "/raw-trace"
  eResult <- try @SomeException $ do
    baseRequest <- parseRequest $ T.unpack url
    let request =
          baseRequest
            { responseTimeout = responseTimeoutMicro traceRequestTimeoutMicros
            }
    responseBody <$> httpLbs request manager
  pure $ case eResult of
    Left err -> Left $ T.pack $ show err
    Right body ->
      case Aeson.eitherDecode body of
        Left err -> Left $ "Invalid trace API JSON response: " <> T.pack err
        Right value -> Right value

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

traceRequestTimeoutMicros :: Int
traceRequestTimeoutMicros = 20_000_000

executionOracleDerivationVersion, executionOraclePayloadDerivationVersion, executionEconomicsDerivationVersion :: Int
executionOracleDerivationVersion = 2
executionOraclePayloadDerivationVersion = 1
executionEconomicsDerivationVersion = 1

-- | Persist every monitored release log with transaction/receipt evidence.
-- The legacy Perps projection below remains deliberately narrower; generic
-- governance, tranche, oracle, and dependency events live in this immutable
-- release-scoped ledger even when no typed projection exists yet.
persistProtocolLog
  :: Connection
  -> PerpsIndexerConfig
  -> BlockInfo
  -> TransactionInfo
  -> RpcLog
  -> IO ()
persistProtocolLog conn cfg blockInfo txInfo logEntry = do
  let addresses = picAddresses cfg
      parsed = parseConfiguredLog cfg logEntry
      common = classifyProtocolLogForAddresses addresses logEntry
      eventName =
        maybe
          (maybe "Unclassified" plcEventName common)
          parsedEventName
          parsed
      account = maybe (common >>= plcAccount) parsedAccount parsed
      orderId = parsed >>= parsedOrderId
      eventPayload =
        maybe
          (maybe unclassifiedPayload plcPayload common)
          parsedPayload
          parsed
      (actionType, actionStatus) =
        maybe
          ( maybe
              ("unclassified_event", "unavailable")
              ( \entry ->
                  ( plcActionType entry
                  , if plcActionType entry == "unclassified_event"
                      then "unavailable"
                      else "success"
                  )
              )
              common
          )
          parsedAction
          parsed
      classificationAvailability =
        maybe [] plcAvailability common
          <> case (parsed, common) of
            (Nothing, Nothing) ->
              [ object
                  [ "field" .= ("decodedData" :: Text)
                  , "reason" .= ("unknown_event_signature" :: Text)
                  ]
              ]
            _ -> []
      decoded = isJust parsed || maybe False plcDecoded common
      evidence = object $
        [ "level" .=
            if null classificationAvailability
              then ("exact" :: Text)
              else ("unavailable" :: Text)
        , "source" .= ("confirmed_log" :: Text)
        , "identityLevel" .= ("exact" :: Text)
        , "eventName" .= eventName
        , "sourceBlock" .= show (rlBlockNumber logEntry)
        , "decoded" .= decoded
        , "calculationVersion" .= picCalculationVersion cfg
        , "formulaIdentifier" .= ("protocol.action.log_projection.v1" :: Text)
        , "transactionEvidence" .= tiEvidence txInfo
        ]
          <> [ "availability" .= classificationAvailability
             | not (null classificationAvailability)
             ]
      rawTopics = Aeson.toJSON $ map (("0x" <>) . bytesToHex) (rlTopics logEntry)
      rawData = "0x" <> bytesToHex (rlData logEntry)
      unclassifiedPayload =
        object
          [ "classification" .= ("unavailable" :: Text)
          , "reason" .= ("unknown_event_signature" :: Text)
          ]
  insertProtocolLedgerEntry
    conn
    (picReleaseId cfg)
    (picChainId cfg)
    (rlTxHash logEntry)
    (rlAddress logEntry)
    (rlBlockNumber logEntry)
    (rlBlockHash logEntry)
    (rlTxIndex logEntry)
    (rlLogIndex logEntry)
    (biTimestamp blockInfo)
    eventName
    actionType
    (tiStatus txInfo)
    actionStatus
    (tiFrom txInfo)
    (tiTo txInfo)
    (tiSelector txInfo)
    (tiNativeValue txInfo)
    (tiGasUsed txInfo)
    (tiEffectiveGasPrice txInfo)
    rawTopics
    (tiInput txInfo)
    rawData
    account
    orderId
    eventPayload
    (tiEvidence txInfo)
    evidence

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
  | otherwise = parsePerpsLogForAddresses (picAddresses cfg) logEntry

parsedAction :: ParsedPerpsLog -> (Text, Text)
parsedAction = \case
  ParsedOrderCommitted {} -> ("order_commitment", "pending")
  ParsedOrderExecuted {} -> ("order_execution", "success")
  -- OrderFailed is an order-level terminal outcome emitted by a successful
  -- cleanup transaction. The decoded reason remains in the action payload.
  ParsedOrderFailed {} -> ("order_cleanup", "success")
  ParsedPositionActivity kind _ _ _ _ _ _ _
    | kind == "Open" -> ("position_open", "success")
    | kind == "Close" -> ("position_close", "success")
    | kind == "Liquidated" -> ("liquidation", "success")
    | otherwise -> ("position_change", "success")
  ParsedMarginActivity kind _ _ _
    | kind == "Deposit" -> ("margin_deposit", "success")
    | kind == "Withdraw" -> ("margin_withdraw", "success")
    | otherwise -> ("margin_add", "success")
  ParsedUsdcTransfer {} -> ("usdc_transfer", "success")

parsePerpsLog :: RpcLog -> Maybe ParsedPerpsLog
parsePerpsLog = parsePerpsLogForAddresses defaultPerpsAddresses

parsePerpsLogForAddresses ::
  PerpsAddresses ->
  RpcLog ->
  Maybe ParsedPerpsLog
parsePerpsLogForAddresses addresses logEntry = do
  topic <- case rlTopics logEntry of
    firstTopic : _ -> Just firstTopic
    [] -> Nothing
  expectedAddress <- perpsEventExpectedAddress addresses topic
  if protocolAddressMatches expectedAddress (rlAddress logEntry)
    then parsePerpsLogAbi logEntry
    else Nothing

parsePerpsLogAbi :: RpcLog -> Maybe ParsedPerpsLog
parsePerpsLogAbi logEntry =
  case rlTopics logEntry of
    topic : _
      | topic == orderCommittedTopic -> parseOrderCommitted logEntry
      | topic == orderExecutedTopic -> parseOrderExecuted logEntry
      | topic == orderFailedTopic -> parseOrderFailed logEntry
      | topic == positionOpenedTopic -> parsePositionOpened logEntry
      | topic == positionClosedTopic -> parsePositionClosed logEntry
      | topic == positionLiquidatedTopic -> parsePositionLiquidated logEntry
      | topic == marginAddedTopic -> parseMarginAdded logEntry
      | topic == depositTopic -> parseDepositWithdraw "Deposit" logEntry
      | topic == withdrawTopic -> parseDepositWithdraw "Withdraw" logEntry
    _ -> Nothing

classifyProtocolLog :: RpcLog -> Maybe ProtocolLogClassification
classifyProtocolLog =
  classifyProtocolLogForAddresses defaultPerpsAddresses

classifyProtocolLogForAddresses ::
  PerpsAddresses ->
  RpcLog ->
  Maybe ProtocolLogClassification
classifyProtocolLogForAddresses addresses logEntry =
  case rlTopics logEntry of
    topic : _
      | topic == erc4626DepositTopic ->
          classifyErc4626Deposit addresses logEntry
      | topic == erc4626WithdrawTopic ->
          classifyErc4626Withdraw addresses logEntry
      | otherwise ->
          classifyMalformedPerpsEvent addresses topic logEntry
            <|> classifyGovernanceConfigEvent addresses topic logEntry
            <|> classifyGovernanceRoleEvent addresses topic logEntry
    _ -> Nothing

classifyErc4626Deposit ::
  PerpsAddresses ->
  RpcLog ->
  Maybe ProtocolLogClassification
classifyErc4626Deposit addresses logEntry =
  Just $
    case validateVaultEmitter addresses logEntry of
      Left reason ->
        unavailableKnownEvent
          "Deposit"
          "tranche_deposit"
          reason
      Right () ->
        classifyShape
  where
    classifyShape =
      case validateStaticLogShape erc4626DepositShape logEntry of
        Left reason ->
          unavailableKnownEvent
            "Deposit"
            "tranche_deposit"
            reason
        Right () ->
          let sender = requiredIndexedAddress (rlTopics logEntry) 1
              owner = requiredIndexedAddress (rlTopics logEntry) 2
              assets = wordAt (rlData logEntry) 0
              shares = wordAt (rlData logEntry) 1
           in ProtocolLogClassification
                { plcEventName = "Deposit"
                , plcActionType = "tranche_deposit"
                , plcAccount = Just owner
                , plcPayload =
                    object
                      [ "sender" .= sender
                      , "owner" .= owner
                      , "assets" .= show assets
                      , "shares" .= show shares
                      , "assetsUnit" .= ("USDC:6" :: Text)
                      , "sharesUnit" .= ("shares:18" :: Text)
                      ]
                , plcDecoded = True
                , plcAvailability = []
                }

classifyErc4626Withdraw ::
  PerpsAddresses ->
  RpcLog ->
  Maybe ProtocolLogClassification
classifyErc4626Withdraw addresses logEntry =
  Just $
    case validateVaultEmitter addresses logEntry of
      Left reason ->
        unavailableKnownEvent
          "Withdraw"
          "tranche_withdraw"
          reason
      Right () ->
        classifyShape
  where
    classifyShape =
      case validateStaticLogShape erc4626WithdrawShape logEntry of
        Left reason ->
          unavailableKnownEvent
            "Withdraw"
            "tranche_withdraw"
            reason
        Right () ->
          let sender = requiredIndexedAddress (rlTopics logEntry) 1
              receiver = requiredIndexedAddress (rlTopics logEntry) 2
              owner = requiredIndexedAddress (rlTopics logEntry) 3
              assets = wordAt (rlData logEntry) 0
              shares = wordAt (rlData logEntry) 1
           in ProtocolLogClassification
                { plcEventName = "Withdraw"
                , plcActionType = "tranche_withdraw"
                , plcAccount = Just owner
                , plcPayload =
                    object
                      [ "sender" .= sender
                      , "receiver" .= receiver
                      , "owner" .= owner
                      , "assets" .= show assets
                      , "shares" .= show shares
                      , "assetsUnit" .= ("USDC:6" :: Text)
                      , "sharesUnit" .= ("shares:18" :: Text)
                      ]
                , plcDecoded = True
                , plcAvailability = []
                }

-- | A matching topic proves only the event identity. If the remaining ABI
-- shape is malformed, retain that identity and the raw log while explicitly
-- preventing it from entering any typed action projection.
classifyMalformedPerpsEvent ::
  PerpsAddresses ->
  ByteString ->
  RpcLog ->
  Maybe ProtocolLogClassification
classifyMalformedPerpsEvent addresses topic logEntry = do
  (eventName, intendedActionType, shape) <- perpsEventMetadata topic
  expectedAddress <- perpsEventExpectedAddress addresses topic
  if not $ protocolAddressMatches expectedAddress (rlAddress logEntry)
    then
      Just $
        unavailableKnownEvent
          eventName
          intendedActionType
          "event_contract_address_mismatch"
    else
      case (validateStaticLogShape shape logEntry, parsePerpsLogAbi logEntry) of
        (Right (), Just _) -> Nothing
        (Right (), Nothing) ->
          Just $
            unavailableKnownEvent
              eventName
              intendedActionType
              "event_decode_unavailable"
        (Left reason, _) ->
          Just $
            unavailableKnownEvent eventName intendedActionType reason

unavailableKnownEvent ::
  Text ->
  Text ->
  Text ->
  ProtocolLogClassification
unavailableKnownEvent eventName intendedActionType reason =
  ProtocolLogClassification
    { plcEventName = eventName
    , plcActionType = "unclassified_event"
    , plcAccount = Nothing
    , plcPayload =
        object
          [ "classification" .= ("unavailable" :: Text)
          , "reason" .= reason
          , "intendedActionType" .= intendedActionType
          ]
    , plcDecoded = False
    , plcAvailability =
        [ object
            [ "field" .= ("decodedData" :: Text)
            , "reason" .= reason
            ]
        ]
    }

perpsEventMetadata ::
  ByteString ->
  Maybe (Text, Text, StaticLogShape)
perpsEventMetadata topic
  | topic == orderCommittedTopic =
      Just ("OrderCommitted", "order_commitment", orderCommittedShape)
  | topic == orderExecutedTopic =
      Just ("OrderExecuted", "order_execution", orderExecutedShape)
  | topic == orderFailedTopic =
      Just ("OrderFailed", "order_cleanup", orderFailedShape)
  | topic == positionOpenedTopic =
      Just ("PositionOpened", "position_open", positionShape)
  | topic == positionClosedTopic =
      Just ("PositionClosed", "position_close", positionShape)
  | topic == positionLiquidatedTopic =
      Just ("PositionLiquidated", "liquidation", positionShape)
  | topic == marginAddedTopic =
      Just ("MarginAdded", "margin_add", singleAccountAmountShape)
  | topic == depositTopic =
      Just ("Deposit", "margin_deposit", marginTransferShape)
  | topic == withdrawTopic =
      Just ("Withdraw", "margin_withdraw", marginTransferShape)
  | otherwise = Nothing

perpsEventExpectedAddress ::
  PerpsAddresses ->
  ByteString ->
  Maybe Text
perpsEventExpectedAddress addresses topic
  | topic `elem` [orderCommittedTopic, orderExecutedTopic, orderFailedTopic] =
      Just $ paOrderRouter addresses
  | topic `elem` [positionOpenedTopic, positionClosedTopic, positionLiquidatedTopic] =
      Just $ paCfdEngine addresses
  | topic `elem` [marginAddedTopic, depositTopic, withdrawTopic] =
      Just $ paMarginClearinghouse addresses
  | otherwise = Nothing

validateVaultEmitter ::
  PerpsAddresses ->
  RpcLog ->
  Either Text ()
validateVaultEmitter addresses logEntry
  | any
      ( \candidate ->
          protocolAddressMatches candidate (rlAddress logEntry)
      )
      [paSeniorVault addresses, paJuniorVault addresses] =
      Right ()
  | otherwise =
      Left "event_contract_address_mismatch"

protocolAddressMatches :: Text -> Text -> Bool
protocolAddressMatches expected actual =
  isValidAddress expected
    && isValidAddress actual
    && normalizeHex expected == normalizeHex actual

governanceRoleAddress ::
  PerpsAddresses ->
  GovernanceContractRole ->
  Text
governanceRoleAddress addresses = \case
  OrderRouterAdminRole -> paOrderRouterAdmin addresses
  CfdEngineAdminRole -> paCfdEngineAdmin addresses
  HousePoolRole -> paHousePool addresses
  OrderRouterRole -> paOrderRouter addresses
  CfdEngineRole -> paCfdEngine addresses
  PletherOracleRole -> paPletherOracle addresses

classifyGovernanceConfigEvent ::
  PerpsAddresses ->
  ByteString ->
  RpcLog ->
  Maybe ProtocolLogClassification
classifyGovernanceConfigEvent addresses topic logEntry = do
  (categoryDefinition, eventDefinition) <-
    find
      ((== topic) . gedTopic . snd)
      [ (definition, eventDefinition)
      | definition <- governanceCategoryDefinitions
      , eventDefinition <- gcdEvents definition
      ]
  let eventName = eventNameFromSignature $ gedSignature eventDefinition
      actionType = governanceLifecycleActionType $ gedLifecycle eventDefinition
      basePayload =
        [ "category" .= governanceCategoryName (gcdCategory categoryDefinition)
        , "lifecycle" .= governanceLifecycleName (gedLifecycle eventDefinition)
        , "eventSignature" .= gedSignature eventDefinition
        ]
      unavailable malformed reason =
        ProtocolLogClassification
          { plcEventName = eventName
          , plcActionType =
              if malformed
                then "unclassified_event"
                else actionType
          , plcAccount = Nothing
          , plcPayload =
              object $
                basePayload
                  <> [ "classification" .= ("unavailable" :: Text)
                     , "reason" .= reason
                     ]
                  <> [ "intendedActionType" .= actionType
                     | malformed
                     ]
          , plcDecoded = False
          , plcAvailability =
              [ object
                  [ "field" .= ("governanceFields" :: Text)
                  , "reason" .= reason
                  ]
              ]
          }
  if
    not $
      protocolAddressMatches
        (governanceRoleAddress addresses $ gcdContractRole categoryDefinition)
        (rlAddress logEntry)
    then
      Just $
        unavailable True ("event_contract_address_mismatch" :: Text)
    else if length (rlTopics logEntry) /= 1
      then Just $ unavailable True ("governance_event_topics_invalid" :: Text)
    else
      case decodeGovernanceEvent categoryDefinition topic (rlData logEntry) of
        Right decodedEvent ->
          Just
            ProtocolLogClassification
              { plcEventName = eventName
              , plcActionType = actionType
              , plcAccount = Nothing
              , plcPayload =
                  object $
                    basePayload
                      <> [ "fields" .= map governanceFieldJson (dgeFields decodedEvent)
                         ]
              , plcDecoded = True
              , plcAvailability = []
              }
        Left decodeError ->
          Just $
            unavailable
              (governanceDecodeIsMalformed decodeError)
              (governanceDecodeReason decodeError)

classifyGovernanceRoleEvent ::
  PerpsAddresses ->
  ByteString ->
  RpcLog ->
  Maybe ProtocolLogClassification
classifyGovernanceRoleEvent addresses topic logEntry = do
  definition <- find ((== topic) . gredTopic) governanceRoleEvents
  let matchingRole =
        find
          ( \role ->
              protocolAddressMatches
                (governanceRoleAddress addresses role)
                (rlAddress logEntry)
          )
          (gredContractRoles definition)
      eventName = eventNameFromSignature $ gredSignature definition
      actionType = governanceRoleActionType $ gredKey definition
      basePayload =
        [ "governanceKey" .= gredKey definition
        , "eventSignature" .= gredSignature definition
        ]
          <> maybe
            []
            (\role -> ["contractRole" .= governanceContractRoleKey role])
            matchingRole
      unavailable reason =
        ProtocolLogClassification
          { plcEventName = eventName
          , plcActionType = "unclassified_event"
          , plcAccount = Nothing
          , plcPayload =
              object $
                basePayload
                  <> [ "classification" .= ("unavailable" :: Text)
                     , "reason" .= reason
                     , "intendedActionType" .= actionType
                     ]
          , plcDecoded = False
          , plcAvailability =
              [ object
                  [ "field" .= ("governanceRoleFields" :: Text)
                  , "reason" .= reason
                  ]
              ]
          }
  if matchingRole == Nothing
    then
      Just $
        unavailable ("event_contract_address_mismatch" :: Text)
    else
      case decodeGovernanceRolePayload (gredKey definition) logEntry of
        Right (account, fields) ->
          Just
            ProtocolLogClassification
              { plcEventName = eventName
              , plcActionType = actionType
              , plcAccount = account
              , plcPayload = object $ basePayload <> fields
              , plcDecoded = True
              , plcAvailability = []
              }
        Left reason -> Just $ unavailable reason

decodeGovernanceRolePayload ::
  Text ->
  RpcLog ->
  Either Text (Maybe Text, [Pair])
decodeGovernanceRolePayload governanceKey logEntry
  | governanceKey == "governance.ownership_transfer_started" = do
      (previousOwner, newOwner) <- twoIndexedAddresses logEntry
      pure
        ( Just newOwner
        , [ "previousOwner" .= previousOwner
          , "newOwner" .= newOwner
          ]
        )
  | governanceKey == "governance.ownership_transferred" = do
      (previousOwner, newOwner) <- twoIndexedAddresses logEntry
      pure
        ( Just newOwner
        , [ "previousOwner" .= previousOwner
          , "newOwner" .= newOwner
          ]
        )
  | governanceKey == "governance.pauser_updated" = do
      (previousPauser, newPauser) <- twoIndexedAddresses logEntry
      pure
        ( Just newPauser
        , [ "previousPauser" .= previousPauser
          , "newPauser" .= newPauser
          ]
        )
  | governanceKey == "governance.paused" = do
      account <- singleAddressEvent logEntry
      pure (Just account, ["account" .= account])
  | governanceKey == "governance.unpaused" = do
      account <- singleAddressEvent logEntry
      pure (Just account, ["account" .= account])
  | governanceKey == "governance.protocol_treasury_updated" = do
      treasury <- singleIndexedAddress logEntry
      pure
        ( Just treasury
        , ["protocolTreasury" .= treasury]
        )
  | otherwise = Left "governance_role_event_schema_unavailable"

twoIndexedAddresses :: RpcLog -> Either Text (Text, Text)
twoIndexedAddresses logEntry =
  case rlTopics logEntry of
    [_eventTopic, firstWord, secondWord]
      | BS.null (rlData logEntry) -> do
          firstAddress <- canonicalAddressWord firstWord
          secondAddress <- canonicalAddressWord secondWord
          pure (firstAddress, secondAddress)
    _ -> Left "governance_role_event_shape_invalid"

singleIndexedAddress :: RpcLog -> Either Text Text
singleIndexedAddress logEntry =
  case rlTopics logEntry of
    [_eventTopic, addressWord]
      | BS.null (rlData logEntry) ->
          canonicalAddressWord addressWord
    _ -> Left "governance_role_event_shape_invalid"

-- OpenZeppelin Pausable emits its account in event data. The indexed form is
-- retained as a compatibility path for earlier deployments and fixtures.
singleAddressEvent :: RpcLog -> Either Text Text
singleAddressEvent logEntry =
  case rlTopics logEntry of
    [_eventTopic]
      | BS.length (rlData logEntry) == 32 ->
          canonicalAddressWord (rlData logEntry)
    [_eventTopic, addressWord]
      | BS.null (rlData logEntry) ->
          canonicalAddressWord addressWord
    _ -> Left "governance_role_event_shape_invalid"

canonicalAddressWord :: ByteString -> Either Text Text
canonicalAddressWord word
  | BS.length word /= 32 =
      Left "governance_address_word_length_invalid"
  | BS.take 12 word /= BS.replicate 12 0 =
      Left "governance_address_not_canonical"
  | otherwise =
      Right $ "0x" <> bytesToHex (BS.drop 12 word)

governanceFieldJson :: DecodedGovernanceField -> Value
governanceFieldJson DecodedGovernanceField {dgfDefinition, dgfValue} =
  object
    [ "key" .= gfKey dgfDefinition
    , "rawValue" .= governanceDecodedRawValue dgfValue
    , "scale" .= gfScale dgfDefinition
    , "unit" .= gfUnit dgfDefinition
    , "valueType" .= governanceFieldTypeName (gfType dgfDefinition)
    , "evidence" .=
        object
          [ "level" .= ("exact" :: Text)
          , "source" .= ("confirmed_log" :: Text)
          ]
    ]

governanceDecodedRawValue :: GovernanceDecodedValue -> Value
governanceDecodedRawValue = \case
  GovernanceUint value -> String $ T.pack $ show value
  GovernanceAddress value -> String value
  GovernanceBool value -> Bool value

governanceFieldTypeName :: GovernanceFieldType -> Text
governanceFieldTypeName = \case
  Uint256Field -> "uint256"
  AddressField -> "address"
  BoolField -> "bool"
  Uint256ArrayField -> "uint256[]"

governanceCategoryName :: GovernanceCategory -> Text
governanceCategoryName = \case
  RouterConfigCategory -> "router_config"
  OracleConfigCategory -> "oracle_config"
  EngineRiskConfigCategory -> "engine_risk_config"
  EngineCalendarConfigCategory -> "engine_calendar_config"
  EngineFreshnessConfigCategory -> "engine_freshness_config"
  HousePoolConfigCategory -> "house_pool_config"

governanceLifecycleName :: GovernanceLifecycle -> Text
governanceLifecycleName = \case
  GovernanceProposed -> "proposed"
  GovernanceFinalized -> "finalized"
  GovernanceCancelled -> "cancelled"

governanceLifecycleActionType :: GovernanceLifecycle -> Text
governanceLifecycleActionType = \case
  GovernanceProposed -> "governance_proposal"
  GovernanceFinalized -> "governance_execution"
  GovernanceCancelled -> "governance_cancellation"

governanceRoleActionType :: Text -> Text
governanceRoleActionType governanceKey
  | governanceKey == "governance.ownership_transfer_started" = "ownership_transfer_started"
  | governanceKey == "governance.ownership_transferred" = "ownership_transfer"
  | governanceKey == "governance.pauser_updated" = "pauser_update"
  | governanceKey == "governance.paused" = "pause"
  | governanceKey == "governance.unpaused" = "unpause"
  | governanceKey == "governance.protocol_treasury_updated" = "protocol_treasury_update"
  | otherwise = "governance_role_change"

governanceDecodeReason :: GovernanceDecodeError -> Text
governanceDecodeReason = \case
  GovernanceDynamicPayloadUnavailable reason -> reason
  GovernancePayloadLengthMismatch {} -> "governance_payload_length_mismatch"
  GovernanceSchemaWordCountMismatch {} -> "governance_schema_word_count_mismatch"
  GovernanceNonCanonicalAddress {} -> "governance_address_not_canonical"
  GovernanceInvalidBool {} -> "governance_bool_not_canonical"
  GovernanceUnsupportedStaticField {} -> "governance_static_field_unsupported"
  GovernanceUnknownEventTopic {} -> "governance_event_topic_unknown"

governanceDecodeIsMalformed :: GovernanceDecodeError -> Bool
governanceDecodeIsMalformed = \case
  -- A correctly-shaped dynamic calendar event has an exact identity even
  -- though the current calculation version deliberately cannot decode it.
  GovernanceDynamicPayloadUnavailable {} -> False
  GovernancePayloadLengthMismatch {} -> True
  GovernanceSchemaWordCountMismatch {} -> True
  GovernanceNonCanonicalAddress {} -> True
  GovernanceInvalidBool {} -> True
  GovernanceUnsupportedStaticField {} -> True
  GovernanceUnknownEventTopic {} -> True

eventNameFromSignature :: Text -> Text
eventNameFromSignature = T.takeWhile (/= '(')

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
  requireStaticLogShape orderCommittedShape logEntry
  oid <- indexedUint (rlTopics logEntry) 1
  account <- indexedAddress (rlTopics logEntry) 2
  let side = fromInteger $ wordAt (rlData logEntry) 0
  pure $ ParsedOrderCommitted oid account side $
    object ["orderId" .= show oid, "account" .= account, "side" .= side]

parseOrderExecuted :: RpcLog -> Maybe ParsedPerpsLog
parseOrderExecuted logEntry = do
  requireStaticLogShape orderExecutedShape logEntry
  oid <- indexedUint (rlTopics logEntry) 1
  let executionPrice = wordAt (rlData logEntry) 0
  pure $ ParsedOrderExecuted oid executionPrice $
    object ["orderId" .= show oid, "executionPrice" .= show executionPrice]

parseOrderFailed :: RpcLog -> Maybe ParsedPerpsLog
parseOrderFailed logEntry = do
  requireStaticLogShape orderFailedShape logEntry
  oid <- indexedUint (rlTopics logEntry) 1
  let reason = fromInteger $ wordAt (rlData logEntry) 0
      reasonName = orderFailReasonName reason
  pure $ ParsedOrderFailed oid reason reasonName $
    object ["orderId" .= show oid, "reason" .= reason, "reasonName" .= reasonName]

parsePositionOpened :: RpcLog -> Maybe ParsedPerpsLog
parsePositionOpened logEntry = do
  requireStaticLogShape positionShape logEntry
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
  requireStaticLogShape positionShape logEntry
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
              (picRpcUrls cfg)
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
              (picRpcUrls cfg)
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
  requireStaticLogShape positionShape logEntry
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
  requireStaticLogShape singleAccountAmountShape logEntry
  account <- indexedAddress (rlTopics logEntry) 1
  let amount = wordAt (rlData logEntry) 0
  pure $ ParsedMarginActivity "Add margin" account amount $
    object ["account" .= account, "amountUsdc" .= show amount]

parseDepositWithdraw :: Text -> RpcLog -> Maybe ParsedPerpsLog
parseDepositWithdraw kind logEntry = do
  requireStaticLogShape marginTransferShape logEntry
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
  ParsedPositionActivity _ account _ _ _ _ _ _ -> Just account
  ParsedMarginActivity _ account _ _ -> Just account
  ParsedUsdcTransfer _ toAddress _ _ -> Just toAddress
  _ -> Nothing

parsedOrderId :: ParsedPerpsLog -> Maybe Integer
parsedOrderId = \case
  ParsedOrderCommitted oid _ _ _ -> Just oid
  ParsedOrderExecuted oid _ _ -> Just oid
  ParsedOrderFailed oid _ _ _ -> Just oid
  _ -> Nothing

parsedSide :: ParsedPerpsLog -> Maybe Int
parsedSide = \case
  ParsedOrderCommitted _ _ side _ -> Just side
  ParsedPositionActivity _ _ side _ _ _ _ _ -> Just side
  _ -> Nothing

parsedPayload :: ParsedPerpsLog -> Value
parsedPayload = \case
  ParsedOrderCommitted _ _ _ payload -> payload
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

activityKey :: RpcLog -> Text -> Maybe Integer -> Text
activityKey logEntry kind orderId =
  T.intercalate ":"
    [ normalizeHex (rlTxHash logEntry)
    , T.pack $ show (rlLogIndex logEntry)
    , T.replace " " "_" kind
    , maybe "" (T.pack . show) orderId
    ]

logEvidenceKey :: RpcLog -> (Text, Integer, Integer)
logEvidenceKey logEntry =
  ( normalizeHex $ rlTxHash logEntry
  , rlBlockNumber logEntry
  , rlLogIndex logEntry
  )

perpsAddresses :: PerpsIndexerConfig -> [Text]
perpsAddresses cfg =
  nubBy ((==) `on` normalizeHex) $
    filter (not . T.null . T.strip)
      [ paUsdc addresses
      , paOrderRouter addresses
      , paOrderRouterAdmin addresses
      , paCfdEngine addresses
      , paCfdEngineAdmin addresses
      , paCfdEngineLens addresses
      , paCfdEngineSettlementSidecar addresses
      , paMarginClearinghouse addresses
      , paPletherOracle addresses
      , paAccountLens addresses
      , paPublicLens addresses
      , paHousePool addresses
      , paSeniorVault addresses
      , paJuniorVault addresses
      ]
  where
    addresses = picAddresses cfg

perpsContractAddresses :: PerpsIndexerConfig -> [Text]
perpsContractAddresses cfg =
  filter
    ((/= normalizeHex (paUsdc $ picAddresses cfg)) . normalizeHex)
    (perpsAddresses cfg)

replayPerpsContractAddresses :: PerpsIndexerConfig -> [Text]
replayPerpsContractAddresses cfg =
  [ paOrderRouter addresses
  , paCfdEngine addresses
  , paMarginClearinghouse addresses
  ]
  where
    addresses = picAddresses cfg

requireRpc :: Text -> IO (Either Text a) -> IO a
requireRpc label action = do
  result <- action
  case result of
    Right value -> pure value
    Left err -> fail $ T.unpack $ label <> " failed: " <> err

getCurrentBlockNumber :: Manager -> [Text] -> IORef Integer -> IO (Either Text Integer)
getCurrentBlockNumber manager rpcUrls reqIdRef = do
  result <- rpcCallAny manager rpcUrls reqIdRef "eth_blockNumber" ([] :: [Value])
  pure $ case result of
    Left err -> Left err
    Right (String hex) ->
      maybe
        (Left "Expected canonical block quantity")
        Right
        (parseRpcQuantity hex)
    Right _ -> Left "Expected canonical block quantity"

getReplayCurrentBlockNumber :: Manager -> [Text] -> IORef Integer -> IO (Either Text Integer)
getReplayCurrentBlockNumber manager rpcUrls reqIdRef = do
  result <- rpcCallAny manager rpcUrls reqIdRef "eth_blockNumber" ([] :: [Value])
  pure $ result >>= parseReplayBlockNumber

parseReplayBlockNumber :: Value -> Either Text Integer
parseReplayBlockNumber = \case
  String quantity -> parseCanonicalHexQuantity "block number" quantity
  _ -> Left "Replay block number response must be a canonical hex string"

getBlockByNumber :: Manager -> [Text] -> IORef Integer -> Integer -> IO (Either Text BlockInfo)
getBlockByNumber manager rpcUrls reqIdRef blockNumber = do
  result <- rpcCallAny manager rpcUrls reqIdRef "eth_getBlockByNumber" [String $ "0x" <> intToHex blockNumber, Bool False]
  pure $ case result of
    Left err -> Left err
    Right value -> parseBlockInfo blockNumber value

getTransactionInfo :: Manager -> [Text] -> IORef Integer -> Text -> IO TransactionInfo
getTransactionInfo manager rpcUrls reqIdRef txHash = do
  txResult <- rpcCallAny manager rpcUrls reqIdRef "eth_getTransactionByHash" [String txHash]
  receiptResult <- rpcCallAny manager rpcUrls reqIdRef "eth_getTransactionReceipt" [String txHash]
  pure $ transactionInfoFromRpcResults txResult receiptResult

transactionInfoFromRpcResults :: Either Text Value -> Either Text Value -> TransactionInfo
transactionInfoFromRpcResults txResult receiptResult =
  let (txObject, txUnavailable) = rpcObjectResult "transaction" txResult
      (receiptObject, receiptUnavailable) = rpcObjectResult "receipt" receiptResult
      txIdentityResult = maybe (Left "transaction_identity_unavailable") (parseTransactionIdentity "transaction") txObject
      receiptIdentityResult = maybe (Left "receipt_identity_unavailable") (parseTransactionIdentity "receipt") receiptObject
      receiptLogsResult = maybe (Left "receipt_logs_unavailable") parseReceiptLogs receiptObject
      senderResult =
        maybe (Left "transaction_sender_unavailable")
          (mapLeft (const "transaction_sender_unavailable") . requiredCanonicalAddress "from")
          txObject
      recipientResult =
        maybe (Left "transaction_recipient_unavailable")
          (mapLeft (const "transaction_recipient_unavailable") . nullableCanonicalAddress "to")
          txObject
      inputResult =
        maybe (Left "transaction_input_unavailable")
          (mapLeft (const "transaction_input_unavailable") . requiredCanonicalData "input")
          txObject
      nativeValueResult =
        maybe (Left "transaction_native_value_unavailable")
          (mapLeft (const "transaction_native_value_unavailable") . requiredCanonicalQuantity "value")
          txObject
      statusResult =
        maybe (Left "receipt_status_unavailable")
          (mapLeft (const "receipt_status_unavailable") . parseReceiptStatus)
          receiptObject
      gasUsedResult =
        maybe (Left "receipt_gas_used_unavailable")
          (mapLeft (const "receipt_gas_used_unavailable") . requiredCanonicalQuantity "gasUsed")
          receiptObject
      effectiveGasPriceResult =
        maybe
          (Left "receipt_effective_gas_price_unavailable")
          (mapLeft (const "receipt_effective_gas_price_unavailable") . requiredCanonicalQuantity "effectiveGasPrice")
          receiptObject
      sender = either (const Nothing) Just senderResult
      recipient = either (const Nothing) id recipientResult
      input = either (const Nothing) Just inputResult
      nativeValue = either (const Nothing) Just nativeValueResult
      status = either (const "unavailable") id statusResult
      gasUsed = either (const Nothing) Just gasUsedResult
      effectiveGasPrice = either (const Nothing) Just effectiveGasPriceResult
      transactionIdentity = either (const Nothing) Just txIdentityResult
      receiptIdentity = either (const Nothing) Just receiptIdentityResult
      receiptLogs = either (const Nothing) Just receiptLogsResult
      selector =
        input >>= \value ->
          if T.length value >= 10
            then Just $ T.take 10 value
            else Nothing
      availability =
        catMaybes [txUnavailable, receiptUnavailable]
          <> eitherAvailability "transactionIdentity" txIdentityResult
          <> eitherAvailability "receiptIdentity" receiptIdentityResult
          <> eitherAvailability "receiptLogs" receiptLogsResult
          <> eitherAvailability "sender" senderResult
          <> eitherAvailability "recipient" recipientResult
          <> eitherAvailability "input" inputResult
          <> eitherAvailability "nativeValue" nativeValueResult
          <> eitherAvailability "status" statusResult
          <> eitherAvailability "gasUsed" gasUsedResult
          <> eitherAvailability "effectiveGasPrice" effectiveGasPriceResult
      evidence =
        transactionEvidence
          "unbound"
          (isJust txObject)
          (isJust receiptObject)
          availability
          Nothing
  in TransactionInfo
    { tiFrom = sender
    , tiTo = recipient
    , tiSelector = selector
    , tiInput = input
    , tiNativeValue = nativeValue
    , tiStatus = status
    , tiGasUsed = gasUsed
    , tiEffectiveGasPrice = effectiveGasPrice
    , tiTransactionIdentity = transactionIdentity
    , tiReceiptIdentity = receiptIdentity
    , tiReceiptLogs = receiptLogs
    , tiAvailability = availability
    , tiTransactionAvailable = isJust txObject
    , tiReceiptAvailable = isJust receiptObject
    , tiEvidence = evidence
    }

-- | Bind parsed transaction/receipt payloads to the exact confirmed log and
-- canonical block selected for this range. Any disagreement is retryable
-- provider/fork evidence and must abort the range before a ledger write.
bindTransactionInfoToLog
  :: BlockInfo
  -> RpcLog
  -> TransactionInfo
  -> Either Text TransactionInfo
bindTransactionInfoToLog blockInfo logEntry txInfo = do
  validateCanonicalRpcLog logEntry
  unlessEither
    (biNumber blockInfo == rlBlockNumber logEntry)
    "transaction_evidence_block_number_mismatch"
  unlessEither
    (normalizeHex (biHash blockInfo) == normalizeHex (rlBlockHash logEntry))
    "transaction_evidence_block_hash_mismatch"
  transactionIdentity <-
    maybe
      (Left "transaction_identity_unavailable")
      Right
      (tiTransactionIdentity txInfo)
  receiptIdentity <-
    maybe
      (Left "receipt_identity_unavailable")
      Right
      (tiReceiptIdentity txInfo)
  receiptLogs <-
    maybe
      (Left "receipt_logs_unavailable")
      Right
      (tiReceiptLogs txInfo)
  validateExpectedIdentity "transaction" logEntry transactionIdentity
  validateExpectedIdentity "receipt" logEntry receiptIdentity
  unlessEither
    (transactionIdentity == receiptIdentity)
    "transaction_receipt_identity_mismatch"
  unlessEither
    (all (receiptLogBelongsTo receiptIdentity) receiptLogs)
    "receipt_log_parent_identity_mismatch"
  receiptLog <-
    case filter ((== rlLogIndex logEntry) . rlLogIndex) receiptLogs of
      [matchedLog] -> Right matchedLog
      [] -> Left "receipt_log_identity_missing"
      _ -> Left "receipt_log_identity_ambiguous"
  unlessEither
    (receiptLogExactlyMatches logEntry receiptLog)
    "receipt_log_identity_mismatch"
  let binding =
        object
          [ "transactionHash" .= normalizeHex (rlTxHash logEntry)
          , "blockNumber" .= show (rlBlockNumber logEntry)
          , "blockHash" .= normalizeHex (rlBlockHash logEntry)
          , "transactionIndex" .= show (rlTxIndex logEntry)
          , "receiptLogIndex" .= show (rlLogIndex logEntry)
          , "receiptLogAddress" .= normalizeHex (rlAddress receiptLog)
          , "receiptLogMatched" .= True
          ]
      evidence =
        transactionEvidence
          "exact"
          (tiTransactionAvailable txInfo)
          (tiReceiptAvailable txInfo)
          (tiAvailability txInfo)
          (Just binding)
  pure txInfo {tiEvidence = evidence}

transactionEvidence
  :: Text
  -> Bool
  -> Bool
  -> [Value]
  -> Maybe Value
  -> Value
transactionEvidence identityLevel transactionAvailable receiptAvailable availability binding =
  object $
    [ "level" .=
        if identityLevel == "exact" && null availability
          then ("exact" :: Text)
          else ("unavailable" :: Text)
    , "source" .= ("transaction_and_receipt" :: Text)
    , "identityLevel" .= identityLevel
    , "transactionAvailable" .= transactionAvailable
    , "receiptAvailable" .= receiptAvailable
    , "availability" .= availability
    ]
      <> maybe [] (\value -> ["binding" .= value]) binding

validateExpectedIdentity :: Text -> RpcLog -> TransactionIdentity -> Either Text ()
validateExpectedIdentity sourceName logEntry identity = do
  unlessEither
    (normalizeHex (txiHash identity) == normalizeHex (rlTxHash logEntry))
    (sourceName <> "_hash_mismatch")
  unlessEither
    (txiBlockNumber identity == rlBlockNumber logEntry)
    (sourceName <> "_block_number_mismatch")
  unlessEither
    (normalizeHex (txiBlockHash identity) == normalizeHex (rlBlockHash logEntry))
    (sourceName <> "_block_hash_mismatch")
  unlessEither
    (txiTransactionIndex identity == rlTxIndex logEntry)
    (sourceName <> "_transaction_index_mismatch")

receiptLogBelongsTo :: TransactionIdentity -> RpcLog -> Bool
receiptLogBelongsTo identity receiptLog =
  normalizeHex (rlTxHash receiptLog) == normalizeHex (txiHash identity)
    && rlBlockNumber receiptLog == txiBlockNumber identity
    && normalizeHex (rlBlockHash receiptLog) == normalizeHex (txiBlockHash identity)
    && rlTxIndex receiptLog == txiTransactionIndex identity

receiptLogExactlyMatches :: RpcLog -> RpcLog -> Bool
receiptLogExactlyMatches expected actual =
  normalizeHex (rlAddress expected) == normalizeHex (rlAddress actual)
    && rlTopics expected == rlTopics actual
    && rlData expected == rlData actual
    && normalizeHex (rlTxHash expected) == normalizeHex (rlTxHash actual)
    && rlBlockNumber expected == rlBlockNumber actual
    && normalizeHex (rlBlockHash expected) == normalizeHex (rlBlockHash actual)
    && rlTxIndex expected == rlTxIndex actual
    && rlLogIndex expected == rlLogIndex actual

unlessEither :: Bool -> Text -> Either Text ()
unlessEither condition reason =
  if condition then Right () else Left reason

eitherAvailability :: Text -> Either Text a -> [Value]
eitherAvailability fieldName =
  either (\reason -> [unavailableField fieldName reason]) (const [])

rpcObjectResult :: Text -> Either Text Value -> (Maybe Aeson.Object, Maybe Value)
rpcObjectResult sourceName = \case
  Right (Object obj) -> (Just obj, Nothing)
  Right Null ->
    ( Nothing
    , Just $ unavailableField sourceName (sourceName <> "_not_returned")
    )
  Right _ ->
    ( Nothing
    , Just $ unavailableField sourceName (sourceName <> "_response_invalid")
    )
  Left _ ->
    ( Nothing
    , Just $ unavailableField sourceName (sourceName <> "_rpc_unavailable")
    )

unavailableField :: Text -> Text -> Value
unavailableField fieldName reason =
  object
    [ "field" .= fieldName
    , "reason" .= reason
    ]

parseRpcQuantity :: Text -> Maybe Integer
parseRpcQuantity value =
  if not ("0x" `T.isPrefixOf` value)
    || T.null digits
    || not (T.all isHexDigit digits)
    || (T.length digits > 1 && T.head digits == '0')
    then Nothing
    else Just $ hexToInteger digits
  where
    digits = T.drop 2 value

getReplayBlockByNumber
  :: Manager -> [Text] -> IORef Integer -> Integer -> IO (Either Text BlockInfo)
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

getExecutionTransactionInfo :: Manager -> [Text] -> IORef Integer -> Text -> IO (Either Text ExecutionTransactionInfo)
getExecutionTransactionInfo manager rpcUrls reqIdRef txHash = do
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
        ExecutionTransactionInfo
          { etiHash = transactionHash
          , etiFrom = fromAddress
          , etiTo = toAddress
          , etiBlockHash = blockHash
          , etiInput = decodeHex input
          }
    Right Null -> Left $ "Transaction not found: " <> txHash
    Right _ -> Left "Expected transaction object"

getReplayExecutionTransactionInfo
  :: Manager -> [Text] -> IORef Integer -> Text -> IO (Either Text ExecutionTransactionInfo)
getReplayExecutionTransactionInfo manager rpcUrls reqIdRef txHash = do
  result <- rpcCallAny manager rpcUrls reqIdRef "eth_getTransactionByHash" [String txHash]
  pure $ result >>= parseReplayTransactionInfo

parseReplayTransactionInfo :: Value -> Either Text ExecutionTransactionInfo
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
      ExecutionTransactionInfo
        { etiHash = transactionHash
        , etiFrom = fromAddress
        , etiTo = toAddress
        , etiBlockHash = blockHash
        , etiInput = input
        }
  Null -> Left "Replay transaction was not found"
  _ -> Left "Replay transaction response must be a JSON object"

executionUpdateData
  :: PerpsIndexerConfig
  -> Text
  -> Text
  -> ExecutionTransactionInfo
  -> Integer
  -> Either Text [ByteString]
executionUpdateData cfg expectedTxHash expectedBlockHash txInfo orderId = do
  if normalizeHex (etiHash txInfo) == normalizeHex expectedTxHash
    then Right ()
    else Left "Execution transaction hash does not match the indexed terminal event"
  if normalizeHex (etiBlockHash txInfo) == normalizeHex expectedBlockHash
    then Right ()
    else Left "Execution transaction block hash does not match the indexed terminal event"
  if normalizeHex (etiTo txInfo) == normalizeHex (paOrderRouter $ picAddresses cfg)
    then Right ()
    else Left "Execution transaction target does not match the configured order router"
  decodeExecutionUpdateData orderId (etiInput txInfo)

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
    Right feedIds -> tryRpcUrls (picRpcUrls cfg) feedIds []
  where
    tryRpcUrls [] _ errors =
      pure $
        Left $
          "All RPC providers failed to parse the signed execution oracle payload"
            <> if null errors then "" else ": " <> T.intercalate "; " (reverse errors)
    tryRpcUrls (rpcUrl : remaining) feedIds errors = do
      let client =
            EthClient
              { clientManager = manager
              , clientRpcUrl = rpcUrl
              , clientRequestId = reqIdRef
              }
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

getLogs
  :: Manager
  -> [Text]
  -> IORef Integer
  -> PerpsIndexerConfig
  -> Integer
  -> Integer
  -> IO (Either Text [RpcLog])
getLogs manager rpcUrls reqIdRef cfg fromBlock toBlock = do
  protocolResult <-
    getLogsFor
      manager
      rpcUrls
      reqIdRef
      (perpsContractAddresses cfg)
      Nothing
      parseLogEntry
      fromBlock
      toBlock
  transferResult <-
    getLogsFor
      manager
      rpcUrls
      reqIdRef
      [paUsdc $ picAddresses cfg]
      (Just [transferTopic])
      parseLogEntry
      fromBlock
      toBlock
  pure $ (<>) <$> protocolResult <*> transferResult

-- Bounded replay intentionally remains narrower than the immutable protocol
-- ledger: it proves duplicate ingestion only for the canonical Perps
-- projection and USDC transfer rows that existed before the explorer.
getReplayLogs
  :: Manager
  -> [Text]
  -> IORef Integer
  -> PerpsIndexerConfig
  -> Integer
  -> Integer
  -> IO (Either Text [RpcLog])
getReplayLogs manager rpcUrls reqIdRef cfg fromBlock toBlock = do
  perpsResult <-
    getLogsFor
      manager
      rpcUrls
      reqIdRef
      (replayPerpsContractAddresses cfg)
      (Just perpsEventTopics)
      parseReplayLogEntry
      fromBlock
      toBlock
  transferResult <-
    getLogsFor
      manager
      rpcUrls
      reqIdRef
      [paUsdc $ picAddresses cfg]
      (Just [transferTopic])
      parseReplayLogEntry
      fromBlock
      toBlock
  pure $ (<>) <$> perpsResult <*> transferResult

getLogsFor
  :: Manager
  -> [Text]
  -> IORef Integer
  -> [Text]
  -> Maybe [ByteString]
  -> (Value -> Either Text RpcLog)
  -> Integer
  -> Integer
  -> IO (Either Text [RpcLog])
getLogsFor manager rpcUrls reqIdRef addresses eventTopics parseEntry fromBlock toBlock = do
  let filterFields =
        [ "address" .= addresses
        , "fromBlock" .= ("0x" <> intToHex fromBlock)
        , "toBlock" .= ("0x" <> intToHex toBlock)
        ]
          <> maybe
            []
            (\topics ->
              [ "topics"
                  .= [map (String . ("0x" <>) . bytesToHex) topics]
              ]
            )
            eventTopics
  result <-
    rpcCallAny
      manager
      rpcUrls
      reqIdRef
      "eth_getLogs"
      [object filterFields]
  pure $ case result of
    Left err -> Left err
    Right (Array entries) -> traverse parseEntry $ toList entries
    Right _ -> Left "Expected logs array"


parseLogEntry :: Value -> Either Text RpcLog
parseLogEntry = \case
  Object obj -> do
    address <- requiredCanonicalAddress "address" obj
    topics <- requiredCanonicalTopics obj
    eventData <- requiredCanonicalDataBytes "data" obj
    txHash <- requiredCanonicalHash "transactionHash" obj
    blockNumber <- requiredCanonicalQuantity "blockNumber" obj
    blockHash <- requiredCanonicalHash "blockHash" obj
    txIndex <- requiredCanonicalQuantity "transactionIndex" obj
    logIndex <- requiredCanonicalQuantity "logIndex" obj
    case KM.lookup "removed" obj of
      Just (Bool True) -> Left "log_removed"
      Just (Bool False) -> pure ()
      Nothing -> pure ()
      _ -> Left "log_removed_flag_invalid"
    pure
      RpcLog
        { rlAddress = address
        , rlTopics = topics
        , rlData = eventData
        , rlTxHash = txHash
        , rlBlockNumber = blockNumber
        , rlBlockHash = blockHash
        , rlTxIndex = txIndex
        , rlLogIndex = logIndex
        }
  _ -> Left "log_response_invalid"

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

parseBlockInfo :: Integer -> Value -> Either Text BlockInfo
parseBlockInfo requestedBlock = \case
  Object obj -> do
    returnedBlock <- requiredCanonicalQuantity "number" obj
    unlessEither (returnedBlock == requestedBlock) "block_number_mismatch"
    blockHash <- requiredCanonicalHash "hash" obj
    timestamp <- requiredCanonicalQuantity "timestamp" obj
    pure
      BlockInfo
        { biNumber = returnedBlock
        , biHash = blockHash
        , biTimestamp = timestamp
        }
  Null -> Left "block_not_returned"
  _ -> Left "block_response_invalid"

parseTransactionIdentity :: Text -> Aeson.Object -> Either Text TransactionIdentity
parseTransactionIdentity sourceName obj = do
  transactionHash <-
    mapLeft
      (const $ sourceName <> "_hash_invalid")
      ( requiredCanonicalHash
          (if sourceName == "receipt" then "transactionHash" else "hash")
          obj
      )
  blockNumber <-
    mapLeft
      (const $ sourceName <> "_block_number_invalid")
      (requiredCanonicalQuantity "blockNumber" obj)
  blockHash <-
    mapLeft
      (const $ sourceName <> "_block_hash_invalid")
      (requiredCanonicalHash "blockHash" obj)
  transactionIndex <-
    mapLeft
      (const $ sourceName <> "_transaction_index_invalid")
      (requiredCanonicalQuantity "transactionIndex" obj)
  pure
    TransactionIdentity
      { txiHash = transactionHash
      , txiBlockNumber = blockNumber
      , txiBlockHash = blockHash
      , txiTransactionIndex = transactionIndex
      }

parseReceiptLogs :: Aeson.Object -> Either Text [RpcLog]
parseReceiptLogs obj =
  case KM.lookup "logs" obj of
    Just (Array values) ->
      mapLeft
        (const "receipt_logs_invalid")
        (traverse parseLogEntry $ toList values)
    _ -> Left "receipt_logs_invalid"

parseReceiptStatus :: Aeson.Object -> Either Text Text
parseReceiptStatus obj = do
  status <- requiredCanonicalQuantity "status" obj
  case status of
    0 -> Right "reverted"
    1 -> Right "success"
    _ -> Left "receipt_status_invalid"

requiredCanonicalAddress :: Text -> Aeson.Object -> Either Text Text
requiredCanonicalAddress key obj = do
  value <- requiredText key obj
  if isCanonicalAddress value
    then Right value
    else Left $ key <> "_invalid"

nullableCanonicalAddress :: Text -> Aeson.Object -> Either Text (Maybe Text)
nullableCanonicalAddress key obj =
  case KM.lookup (Key.fromText key) obj of
    Just Null -> Right Nothing
    Just (String value)
      | isCanonicalAddress value -> Right $ Just value
    _ -> Left $ key <> "_invalid"

requiredCanonicalHash :: Text -> Aeson.Object -> Either Text Text
requiredCanonicalHash key obj = do
  value <- requiredText key obj
  if isFixedHexText 32 value
    then Right value
    else Left $ key <> "_invalid"

requiredCanonicalQuantity :: Text -> Aeson.Object -> Either Text Integer
requiredCanonicalQuantity key obj = do
  value <- requiredText key obj
  maybe
    (Left $ key <> "_invalid")
    Right
    (parseRpcQuantity value)

requiredCanonicalData :: Text -> Aeson.Object -> Either Text Text
requiredCanonicalData key obj = do
  value <- requiredText key obj
  _ <- decodeCanonicalHex value
  pure value

requiredCanonicalDataBytes :: Text -> Aeson.Object -> Either Text ByteString
requiredCanonicalDataBytes key obj =
  requiredText key obj >>= decodeCanonicalHex

requiredCanonicalTopics :: Aeson.Object -> Either Text [ByteString]
requiredCanonicalTopics obj =
  case KM.lookup "topics" obj of
    Just (Array values) -> traverse decodeTopic $ toList values
    _ -> Left "topics_invalid"
  where
    decodeTopic = \case
      String value
        | isFixedHexText 32 value ->
            decodeCanonicalHex value
      _ -> Left "topics_invalid"

requiredText :: Text -> Aeson.Object -> Either Text Text
requiredText key obj =
  case KM.lookup (Key.fromText key) obj of
    Just (String value) | not (T.null value) -> Right value
    _ -> Left $ key <> "_unavailable"

decodeCanonicalHex :: Text -> Either Text ByteString
decodeCanonicalHex value
  | not ("0x" `T.isPrefixOf` value) = Left "hex_prefix_invalid"
  | odd (T.length digits) = Left "hex_length_invalid"
  | not (T.all isHexDigit digits) = Left "hex_data_invalid"
  | otherwise =
      mapLeft
        (const "hex_data_invalid")
        (B16.decode $ TE.encodeUtf8 $ T.toLower digits)
  where
    digits = T.drop 2 value

isCanonicalAddress :: Text -> Bool
isCanonicalAddress value =
  "0x" `T.isPrefixOf` value
    && T.length value == 42
    && isValidAddress value

isFixedHexText :: Int -> Text -> Bool
isFixedHexText byteLength value =
  "0x" `T.isPrefixOf` value
    && T.length value == 2 + byteLength * 2
    && T.all isHexDigit (T.drop 2 value)

validateCanonicalRpcLog :: RpcLog -> Either Text ()
validateCanonicalRpcLog logEntry = do
  unlessEither (isCanonicalAddress $ rlAddress logEntry) "log_address_invalid"
  unlessEither (isFixedHexText 32 $ rlTxHash logEntry) "log_transaction_hash_invalid"
  unlessEither (isFixedHexText 32 $ rlBlockHash logEntry) "log_block_hash_invalid"
  unlessEither (rlBlockNumber logEntry >= 0) "log_block_number_invalid"
  unlessEither (rlTxIndex logEntry >= 0) "log_transaction_index_invalid"
  unlessEither (rlLogIndex logEntry >= 0) "log_index_invalid"
  unlessEither (all ((== 32) . BS.length) $ rlTopics logEntry) "log_topics_invalid"

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f =
  either (Left . f) Right
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

isCanonicalHash :: Text -> Bool
isCanonicalHash value =
  T.length value == 66
    && "0x" `T.isPrefixOf` value
    && T.all isHexDigit (T.drop 2 value)

isLowerHexDigit :: Char -> Bool
isLowerHexDigit value =
  (value >= '0' && value <= '9') || (value >= 'a' && value <= 'f')


rpcCallAny :: (Aeson.ToJSON params) => Manager -> [Text] -> IORef Integer -> Text -> params -> IO (Either Text Value)
rpcCallAny manager rpcUrls reqIdRef method params = tryUrls rpcUrls
  where
    tryUrls [] = pure $ Left "No RPC URLs configured"
    tryUrls [url] = rpcCall manager url reqIdRef method params
    tryUrls (url : rest) = do
      result <- rpcCall manager url reqIdRef method params
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

rpcCall :: (Aeson.ToJSON params) => Manager -> Text -> IORef Integer -> Text -> params -> IO (Either Text Value)
rpcCall manager rpcUrl reqIdRef methodName params = do
  reqId <- nextId reqIdRef
  let payload = object
        [ "jsonrpc" .= ("2.0" :: Text)
        , "method" .= methodName
        , "params" .= params
        , "id" .= reqId
        ]
  eResult <- try @SomeException $ do
    req <- parseRequest $ T.unpack rpcUrl
    let req' = req
          { method = "POST"
          , requestHeaders = [("Content-Type", "application/json")]
          , requestBody = RequestBodyLBS $ Aeson.encode payload
          , responseTimeout = responseTimeoutMicro rpcRequestTimeoutMicros
          }
    responseBody <$> httpLbs req' manager
  case eResult of
    Left err -> pure $ Left $ T.pack $ show err
    Right body ->
      case Aeson.decode body of
        Just (Object obj) ->
          case KM.lookup (Key.fromText "result") obj of
            Just value -> pure $ Right value
            Nothing -> pure $ Left $ "RPC error: " <> T.pack (show $ KM.lookup (Key.fromText "error") obj)
        _ -> pure $ Left "Invalid JSON-RPC response"

nextId :: IORef Integer -> IO Integer
nextId ref = atomicModifyIORef' ref $ \n -> (n + 1, n)

validateStaticLogShape :: StaticLogShape -> RpcLog -> Either Text ()
validateStaticLogShape shape logEntry
  | length topics /= slsTopicCount shape =
      Left "event_topic_count_invalid"
  | any ((/= 32) . BS.length) topics =
      Left "event_topic_word_length_invalid"
  | BS.length eventData /= slsDataWordCount shape * 32 =
      Left "event_data_length_invalid"
  | Just _ <- find (not . canonicalAddressAt) (slsIndexedAddresses shape) =
      Left "event_indexed_address_not_canonical"
  | Just (_, width) <-
      find
        (not . indexedUintFits)
        (slsIndexedUintWidths shape) =
      Left $ narrowUintReason width
  | Just (_, width) <-
      find
        (not . dataUintFits)
        (slsDataUintWidths shape) =
      Left $ narrowUintReason width
  | otherwise = Right ()
  where
    topics = rlTopics logEntry
    eventData = rlData logEntry

    canonicalAddressAt index =
      case atIndex topics index >>= eitherToMaybe . canonicalAddressWord of
        Just _ -> True
        Nothing -> False

    indexedUintFits (index, width) =
      maybe False (fitsUnsignedWidth width . bytesToInteger) $
        atIndex topics index

    dataUintFits (index, width)
      | index < 0 || index >= slsDataWordCount shape = False
      | otherwise = fitsUnsignedWidth width $ wordAt eventData index

requireStaticLogShape :: StaticLogShape -> RpcLog -> Maybe ()
requireStaticLogShape shape =
  either (const Nothing) (const $ Just ()) . validateStaticLogShape shape

fitsUnsignedWidth :: Int -> Integer -> Bool
fitsUnsignedWidth width value =
  width > 0
    && width <= 256
    && value >= 0
    && value < 2 ^ width

narrowUintReason :: Int -> Text
narrowUintReason 1 = "event_bool_not_canonical"
narrowUintReason width =
  "event_uint"
    <> T.pack (show width)
    <> "_not_canonical"

atIndex :: [a] -> Int -> Maybe a
atIndex values index
  | index < 0 = Nothing
  | otherwise =
      case drop index values of
        value : _ -> Just value
        [] -> Nothing

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

rpcRequestTimeoutMicros :: Int
rpcRequestTimeoutMicros = 25_000_000

indexedUint :: [ByteString] -> Int -> Maybe Integer
indexedUint topics idx = do
  topicWord <- atIndex topics idx
  if BS.length topicWord == 32
    then Just $ bytesToInteger topicWord
    else Nothing

indexedAddress :: [ByteString] -> Int -> Maybe Text
indexedAddress topics idx =
  atIndex topics idx >>= eitherToMaybe . canonicalAddressWord

requiredIndexedAddress :: [ByteString] -> Int -> Text
requiredIndexedAddress topics index =
  fromMaybe
    (error "validated indexed address is unavailable")
    (indexedAddress topics index)

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
    Right bytes -> bytes
    Left _ -> ""

strip0x :: Text -> Text
strip0x txt
  | "0x" `T.isPrefixOf` txt = T.drop 2 txt
  | "0X" `T.isPrefixOf` txt = T.drop 2 txt
  | otherwise = txt

normalizeHex :: Text -> Text
normalizeHex txt = "0x" <> T.toLower (strip0x txt)

requiredString :: Text -> Aeson.Object -> Either Text Text
requiredString key obj =
  case KM.lookup (Key.fromText key) obj of
    Just (String value) | not (T.null value) -> Right value
    _ -> Left $ "Transaction response is missing string field " <> key
