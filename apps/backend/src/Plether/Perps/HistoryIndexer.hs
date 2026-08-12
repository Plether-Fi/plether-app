module Plether.Perps.HistoryIndexer
  ( PerpsAddresses (..)
  , PerpsIndexerConfig (..)
  , PerpsIndexerMode (..)
  , defaultPerpsAddresses
  , perpsIndexerName
  , runPerpsIndexer
  , perpsEventTopics
  , parsePerpsLog
  , RpcLog (..)
  , BlockInfo (..)
  , ParsedPerpsLog (..)
  , TradeCosts (..)
  , validateRpcLogBlockHash
  , decodeOpenTradeCosts
  , decodeCloseTradeCosts
  , orderFailReasonName
  , terminalStatus
  , isMarketVolumeActivity
  , canCertifyIndexedRange
  , validateIndexedBoundary
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (SomeException, try)
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
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
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
import Plether.Database.Candles
  ( RollupCoverage (..)
  , RollupKind (VolumeRollup)
  , advanceMarketVolumeCoverage
  , getRollupCoverage
  , invalidateMarketVolumeFromBlock
  , recomputeMarketVolumeHierarchyBatch
  )
import Plether.Database.Schema
  ( PerpsExecutionEvidenceRow (..)
  , deletePerpsHistoryFromBlock
  , getPendingPerpsExecutionEvidence
  , getPerpsIndexerLastBlock
  , insertPerpsExpiredCleanupActivityIfReady
  , insertPerpsActivity
  , insertPerpsEvent
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
import Plether.Logging (field, logErrorEvery, logInfoEvery, logWarn, logWarnEvery)
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

data PerpsAddresses = PerpsAddresses
  { paOrderRouter :: Text
  , paCfdEngine :: Text
  , paCfdEngineLens :: Text
  , paCfdEngineSettlementSidecar :: Text
  , paMarginClearinghouse :: Text
  , paPletherOracle :: Text
  }
  deriving stock (Show)

defaultPerpsAddresses :: PerpsAddresses
defaultPerpsAddresses =
  PerpsAddresses
    { paOrderRouter = "0x04E3103752f623fBcDcD01f588590Af4c53E4c1E"
    , paCfdEngine = "0x6A25eA1015b5f032d8a2D95d57AEfcB99219bF0a"
    , paCfdEngineLens = "0xa9aA4097874e9622eAABeE68f65Ff5e3757728C5"
    , paCfdEngineSettlementSidecar = "0x0b652c4d4610234e221403076c116292f935b424"
    , paMarginClearinghouse = "0x19c2f60f6312EAF9acDE4C2b04551a05cA9bE76e"
    , paPletherOracle = "0xADfEd3bf768D810309B97b4dF9F9E77Eaa3a401c"
    }

perpsIndexerName :: Text
perpsIndexerName = "perps-history-costs-v1"

data PerpsIndexerMode
  = PerpsIndexerLoop
  | PerpsIndexerOnce
  | PerpsIndexerBackfill Integer Integer
  deriving stock (Show, Eq)

data PerpsIndexerConfig = PerpsIndexerConfig
  { picRpcUrls :: [Text]
  , picTraceApiUrl :: Maybe Text
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
  }
  deriving stock (Show)

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
  deriving stock (Show)

data BlockInfo = BlockInfo
  { biNumber :: Integer
  , biHash :: Text
  , biTimestamp :: Integer
  }
  deriving stock (Show)

data TransactionInfo = TransactionInfo
  { tiHash :: Text
  , tiFrom :: Text
  , tiTo :: Text
  , tiBlockHash :: Text
  , tiInput :: ByteString
  }
  deriving stock (Show)

data ParsedPerpsLog
  = ParsedOrderCommitted Integer Text Int Value
  | ParsedOrderExecuted Integer Integer Value
  | ParsedOrderFailed Integer Int Text Value
  | ParsedPositionActivity Text Text Int (Maybe Integer) (Maybe Integer) (Maybe Integer) (Maybe Integer) Value
  | ParsedMarginActivity Text Text Integer Value
  deriving stock (Show, Eq)

data TradeCosts = TradeCosts
  { tcExecutionFeeUsdc :: Integer
  , tcVpiUsdc :: Integer
  }
  deriving stock (Show, Eq)

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
    PerpsIndexerBackfill fromBlock toBlock -> do
      runBackfill fromBlock toBlock
      runEvidenceBatch
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

    runBackfill fromBlock toBlock
      | fromBlock > toBlock = pure ()
      | otherwise = do
          let endBlock = min toBlock (fromBlock + picBatchSize cfg - 1)
          _ <- runOneRange manager pool cfg (Just fromBlock) (Just endBlock)
          runBackfill (endBlock + 1) toBlock

    coverageState = \case
      Nothing -> "uninitialized" :: Text
      Just coverage
        | rcComplete coverage -> "complete"
        | otherwise -> "incomplete"

    normalizedCoverageLag now interval expectedLateness =
      fmap $ \finalizedThrough ->
        max 0 (now - finalizedThrough - interval - max 0 expectedLateness)

runOneRange :: Manager -> DbPool -> PerpsIndexerConfig -> Maybe Integer -> Maybe Integer -> IO Bool
runOneRange manager pool cfg explicitFrom explicitTo = do
  reqIdRef <- newIORef 1
  currentBlock <- requireRpc "eth_blockNumber" $ getCurrentBlockNumber manager (picRpcUrls cfg) reqIdRef
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
        getLogs manager (picRpcUrls cfg) reqIdRef (perpsAddresses cfg) startBlock endBlock
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
        getBlockByNumber manager (picRpcUrls cfg) reqIdRef endBlock
      blockInfos <- forM logBlockNumbers $ \blockNumber -> do
        blockInfo <- requireRpc "eth_getBlockByNumber" $
          getBlockByNumber manager (picRpcUrls cfg) reqIdRef blockNumber
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
        getBlockByNumber manager (picRpcUrls cfg) reqIdRef endBlock
      unless
        (normalizeHex (biHash endInfoBefore) == normalizeHex (biHash endInfo))
        (fail "Canonical end block changed while validating the fetched log range")

      enrichedLogs <- forM validatedLogs $ \(logEntry, blockInfo) -> do
        mTxFrom <- getTransactionFrom manager (picRpcUrls cfg) reqIdRef (rlTxHash logEntry)
        let parsedLog = parsePerpsLog logEntry
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
              getBlockByNumber manager (picRpcUrls cfg) reqIdRef (startBlock - 1)
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
          setPerpsIndexerState conn (picChainId cfg) (picIndexerName cfg) (paOrderRouter $ picAddresses cfg) endBlock (Just $ biHash endInfo)
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
  eBlock <- getBlockByNumber manager (picRpcUrls cfg) reqIdRef lastBlock
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
        affectedVolumeMinutes <-
          if picCandleWriteMode cfg == PerpsCandleWritesDual
            then
              invalidateMarketVolumeFromBlock
                conn
                (picChainId cfg)
                (paOrderRouter $ picAddresses cfg)
                rewindBlock
            else pure []
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
        setPerpsIndexerState conn (picChainId cfg) (picIndexerName cfg) (paOrderRouter $ picAddresses cfg) newCursor Nothing

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
        (getTransactionInfo manager (picRpcUrls cfg) reqIdRef $ peerTerminalTxHash candidate)
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
      (picRpcUrls cfg)
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
    Left rpcErr ->
      case picTraceApiUrl cfg of
        Nothing -> pure $ Left rpcErr
        Just traceApiUrl -> do
          explorerTrace <- fetchBlockscoutTrace manager traceApiUrl (tiHash txInfo)
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

processLog
  :: Connection
  -> PerpsIndexerConfig
  -> BlockInfo
  -> Maybe Text
  -> Maybe TradeCosts
  -> RpcLog
  -> IO (Maybe Integer)
processLog conn cfg blockInfo txFrom tradeCosts logEntry =
  case parsePerpsLog logEntry of
    Nothing -> pure Nothing
    Just parsed -> do
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

-- Keep this predicate aligned with the canonical volume query. Only position
-- lifecycle events that contain both notional inputs contribute to OHLCV.
isMarketVolumeActivity :: ParsedPerpsLog -> Bool
isMarketVolumeActivity = \case
  ParsedPositionActivity kind _ _ (Just _) (Just _) _ _ _ ->
    kind `elem` ["Open", "Close", "Liquidated"]
  _ -> False

parsePerpsLog :: RpcLog -> Maybe ParsedPerpsLog
parsePerpsLog logEntry =
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

parseOrderCommitted :: RpcLog -> Maybe ParsedPerpsLog
parseOrderCommitted logEntry = do
  oid <- indexedUint (rlTopics logEntry) 1
  account <- indexedAddress (rlTopics logEntry) 2
  let side = fromInteger $ wordAt (rlData logEntry) 0
  pure $ ParsedOrderCommitted oid account side $
    object ["orderId" .= show oid, "account" .= account, "side" .= side]

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
      let callData = case parsed of
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

parsedAccount :: ParsedPerpsLog -> Maybe Text
parsedAccount = \case
  ParsedOrderCommitted _ account _ _ -> Just account
  ParsedPositionActivity _ account _ _ _ _ _ _ -> Just account
  ParsedMarginActivity _ account _ _ -> Just account
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

perpsAddresses :: PerpsIndexerConfig -> [Text]
perpsAddresses cfg =
  [ paOrderRouter (picAddresses cfg)
  , paCfdEngine (picAddresses cfg)
  , paMarginClearinghouse (picAddresses cfg)
  ]

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
    Right (String hex) -> Right $ hexToInteger $ strip0x hex
    Right _ -> Left "Expected hex string"

getBlockByNumber :: Manager -> [Text] -> IORef Integer -> Integer -> IO (Either Text BlockInfo)
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

getTransactionInfo :: Manager -> [Text] -> IORef Integer -> Text -> IO (Either Text TransactionInfo)
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

getTransactionFrom :: Manager -> [Text] -> IORef Integer -> Text -> IO (Maybe Text)
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

getLogs :: Manager -> [Text] -> IORef Integer -> [Text] -> Integer -> Integer -> IO (Either Text [RpcLog])
getLogs manager rpcUrls reqIdRef addresses fromBlock toBlock = do
  let topics = map (String . ("0x" <>) . bytesToHex) perpsEventTopics
      filterObject = object
        [ "address" .= addresses
        , "topics" .= [topics]
        , "fromBlock" .= ("0x" <> intToHex fromBlock)
        , "toBlock" .= ("0x" <> intToHex toBlock)
        ]
  result <- rpcCallAny manager rpcUrls reqIdRef "eth_getLogs" [filterObject]
  pure $ case result of
    Left err -> Left err
    Right (Array arr) -> Right $ catMaybes $ map parseLogEntry (toList arr)
    Right _ -> Left "Expected logs array"

parseLogEntry :: Value -> Maybe RpcLog
parseLogEntry = \case
  Object obj -> Just $
    RpcLog
      { rlAddress = getString "address" obj
      , rlTopics = map decodeHex $ getStringArray "topics" obj
      , rlData = decodeHex $ getString "data" obj
      , rlTxHash = getString "transactionHash" obj
      , rlBlockNumber = hexToInteger $ strip0x $ getString "blockNumber" obj
      , rlBlockHash = getString "blockHash" obj
      , rlTxIndex = hexToInteger $ strip0x $ getString "transactionIndex" obj
      , rlLogIndex = hexToInteger $ strip0x $ getString "logIndex" obj
      }
  _ -> Nothing

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

getStringArray :: Text -> Aeson.Object -> [Text]
getStringArray key obj = case KM.lookup (Key.fromText key) obj of
  Just (Array arr) -> [s | String s <- toList arr]
  _ -> []
