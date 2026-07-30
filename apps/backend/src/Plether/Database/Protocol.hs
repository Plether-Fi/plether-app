module Plether.Database.Protocol
  ( ProtocolActionRow (..)
  , ProtocolTransactionRow (..)
  , ProtocolEventRow (..)
  , ProtocolStateSnapshotRow (..)
  , KeeperAggregateRow (..)
  , KeeperWindowSummaryRow (..)
  , KeeperNativeCostRow (..)
  , KeeperNativeCostSummaryRow (..)
  , OperationalWalletActivityRow (..)
  , OperationalWalletCostRow (..)
  , ensureProtocolSchema
  , getProtocolIndexerCursor
  , setProtocolIndexerCursor
  , upsertProtocolBlockCheckpoints
  , getProtocolBlockCheckpointsDescending
  , deleteProtocolBlockCheckpointsFromBlock
  , protocolBlockCheckpointSchemaSql
  , protocolBlockCheckpointUpsertSql
  , protocolBlockCheckpointsDescendingSql
  , protocolBlockCheckpointDeleteSql
  , insertProtocolLedgerEntry
  , upsertProtocolStateSnapshot
  , deleteProtocolLedgerFromBlock
  , listProtocolActionsQuerySql
  , listProtocolActions
  , protocolActionsByTransactionQuerySql
  , getProtocolActionsByTransaction
  , protocolActionsByOrderQuerySql
  , getProtocolActionsByOrder
  , protocolTransactionQuerySql
  , getProtocolTransaction
  , getProtocolTransactionsByHashes
  , protocolEventsByTransactionQuerySql
  , getProtocolEventsByTransaction
  , protocolOverviewCountsQuerySql
  , getProtocolOverviewCounts
  , getProtocolIndexedHead
  , protocolProjectionHeadQuerySql
  , getProtocolProjectionHead
  , protocolPendingOrderTimesQuerySql
  , getProtocolPendingOrderTimes
  , keeperAggregatesQuerySql
  , getKeeperAggregates
  , keeperAggregatesPageQuerySql
  , getKeeperAggregatesPage
  , keeperWindowSummaryQuerySql
  , getKeeperWindowSummary
  , keeperRewardLeadersQuerySql
  , getKeeperRewardLeaders
  , keeperActionsQuerySql
  , getKeeperActions
  , keeperLatencySamplesQuerySql
  , getKeeperLatencySamples
  , keeperLatencyPercentilesQuerySql
  , getKeeperLatencyPercentiles
  , keeperNativeCostsQuerySql
  , getKeeperNativeCosts
  , keeperNativeCostSummaryQuerySql
  , getKeeperNativeCostSummary
  , keeperNativeCostsForActorsQuerySql
  , getKeeperNativeCostsForActors
  , operationalWalletActivityQuerySql
  , getOperationalWalletActivity
  , operationalWalletActionsQuerySql
  , getOperationalWalletActions
  , operationalWalletCostsForActorsQuerySql
  , getOperationalWalletCostsForActors
  , trancheActionsQuerySql
  , getTrancheActions
  , protocolStateSnapshotsQuerySql
  , getProtocolStateSnapshots
  , protocolStateSnapshotsPageQuerySql
  , getProtocolStateSnapshotsPage
  , protocolStateSnapshotsAtBlocksQuerySql
  , getProtocolStateSnapshotsAtBlocks
  , getParameterChanges
  , parameterChangesQuerySql
  ) where

import Control.Monad (forM_, when)
import Data.Aeson (Value, encode, object, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import Database.PostgreSQL.Simple
  ( Connection
  , Only (..)
  , Query
  , execute
  , executeMany
  , execute_
  , query
  )
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import Database.PostgreSQL.Simple.ToField (toField)
import Database.PostgreSQL.Simple.Types (In (..))
import Plether.Database.ProtocolParameterChanges
  ( ensureParameterChangeProjectionSchema
  , projectParameterChangeAction
  , rebuildParameterChangeProjection
  )
import Plether.Protocol.ParameterChanges (parameterProjectionActionTypes)
import Plether.Protocol.Release (ProtocolRelease (..))

data ProtocolActionRow = ProtocolActionRow
  { parActionId :: Text
  , parTxHash :: Text
  , parBlockNumber :: Integer
  , parBlockHash :: Text
  , parTxIndex :: Integer
  , parLogIndex :: Integer
  , parTimestamp :: Integer
  , parActionType :: Text
  , parStatus :: Text
  , parAccount :: Maybe Text
  , parActor :: Maybe Text
  , parOrderId :: Maybe Integer
  , parContractAddress :: Text
  , parData :: Value
  , parEvidence :: Value
  }
  deriving stock (Show, Eq)

instance FromRow ProtocolActionRow where
  fromRow =
    ProtocolActionRow
      <$> field <*> field <*> field <*> field <*> field <*> field <*> field
      <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

data ProtocolTransactionRow = ProtocolTransactionRow
  { ptrTxHash :: Text
  , ptrBlockNumber :: Integer
  , ptrBlockHash :: Text
  , ptrTxIndex :: Integer
  , ptrTimestamp :: Integer
  , ptrSender :: Maybe Text
  , ptrRecipient :: Maybe Text
  , ptrSelector :: Maybe Text
  , ptrStatus :: Text
  , ptrGasUsed :: Maybe Integer
  , ptrEffectiveGasPrice :: Maybe Integer
  , ptrNativeValue :: Maybe Integer
  , ptrInputData :: Maybe Text
  , ptrEvidence :: Value
  }
  deriving stock (Show, Eq)

instance FromRow ProtocolTransactionRow where
  fromRow =
    ProtocolTransactionRow
      <$> field <*> field <*> field <*> field <*> field <*> field <*> field
      <*> field <*> field <*> field <*> field <*> field <*> field <*> field

data ProtocolEventRow = ProtocolEventRow
  { perLogIndex :: Integer
  , perContractAddress :: Text
  , perEventName :: Text
  , perRawTopics :: Value
  , perRawData :: Text
  , perDecodedData :: Value
  , perEvidence :: Value
  }
  deriving stock (Show, Eq)

instance FromRow ProtocolEventRow where
  fromRow =
    ProtocolEventRow
      <$> field <*> field <*> field <*> field <*> field <*> field <*> field

data ProtocolStateSnapshotRow = ProtocolStateSnapshotRow
  { pssScope :: Text
  , pssBlockNumber :: Integer
  , pssBlockHash :: Text
  , pssTimestamp :: Integer
  , pssState :: Value
  , pssAvailability :: Value
  , pssCalculationVersion :: Text
  }
  deriving stock (Show, Eq)

instance FromRow ProtocolStateSnapshotRow where
  fromRow =
    ProtocolStateSnapshotRow
      <$> field <*> field <*> field <*> field <*> field <*> field <*> field

data KeeperAggregateRow = KeeperAggregateRow
  { karActor :: Text
  , karActionCount :: Integer
  , karExecutionCount :: Integer
  , karCleanupCount :: Integer
  , karLiquidationCount :: Integer
  , karGrossRewardsUsdc :: Integer
  , karFirstActionAt :: Integer
  , karLastActionAt :: Integer
  }
  deriving stock (Show, Eq)

instance FromRow KeeperAggregateRow where
  fromRow =
    KeeperAggregateRow
      <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

data KeeperWindowSummaryRow = KeeperWindowSummaryRow
  { kwsrActiveKeeperCount :: Integer
  , kwsrActionCount :: Integer
  , kwsrExecutionCount :: Integer
  , kwsrCleanupCount :: Integer
  , kwsrLiquidationCount :: Integer
  , kwsrGrossRewardsUsdc :: Integer
  }
  deriving stock (Show, Eq)

instance FromRow KeeperWindowSummaryRow where
  fromRow =
    KeeperWindowSummaryRow
      <$> field <*> field <*> field <*> field <*> field <*> field

data KeeperNativeCostRow = KeeperNativeCostRow
  { kncActor :: Text
  , kncGasCostWei :: Integer
  , kncTransactionNativeValueWei :: Integer
  , kncMissingGasReceiptCount :: Integer
  , kncMissingNativeValueCount :: Integer
  }
  deriving stock (Show, Eq)

instance FromRow KeeperNativeCostRow where
  fromRow =
    KeeperNativeCostRow
      <$> field <*> field <*> field <*> field <*> field

data KeeperNativeCostSummaryRow = KeeperNativeCostSummaryRow
  { kncsrGasCostWei :: Integer
  , kncsrTransactionNativeValueWei :: Integer
  , kncsrMissingGasReceiptCount :: Integer
  , kncsrMissingNativeValueCount :: Integer
  }
  deriving stock (Show, Eq)

instance FromRow KeeperNativeCostSummaryRow where
  fromRow =
    KeeperNativeCostSummaryRow
      <$> field <*> field <*> field <*> field

-- | Successful actor-attributed protocol activity in a confirmed time window.
-- Counts remain action- and transaction-distinct so batched logs are visible
-- without inflating the native-cost sample count.
data OperationalWalletActivityRow = OperationalWalletActivityRow
  { owaAddress :: Text
  , owaActionCount :: Integer
  , owaTransactionCount :: Integer
  , owaExecutionCount :: Integer
  , owaCleanupCount :: Integer
  , owaLiquidationCount :: Integer
  , owaMaintenanceCount :: Integer
  , owaGovernanceCount :: Integer
  , owaFirstActivityAt :: Integer
  , owaLastActivityAt :: Integer
  }
  deriving stock (Show, Eq)

instance FromRow OperationalWalletActivityRow where
  fromRow =
    OperationalWalletActivityRow
      <$> field <*> field <*> field <*> field <*> field
      <*> field <*> field <*> field <*> field <*> field

-- | Exact native-denominated cost observations for distinct successful
-- protocol transactions. A runway sample exists only when receipt gas fields
-- and transaction native value are all present.
data OperationalWalletCostRow = OperationalWalletCostRow
  { owcAddress :: Text
  , owcObservedGasCostWei :: Integer
  , owcObservedTransactionNativeValueWei :: Integer
  , owcAvailableGrossNativeSpendSampleCount :: Integer
  , owcMedianSuccessfulOperationalTransactionGrossNativeSpendWei :: Maybe Integer
  , owcMissingGasReceiptCount :: Integer
  , owcMissingNativeValueCount :: Integer
  }
  deriving stock (Show, Eq)

instance FromRow OperationalWalletCostRow where
  fromRow =
    OperationalWalletCostRow
      <$> field <*> field <*> field <*> field <*> field <*> field <*> field

ensureProtocolSchema :: Connection -> ProtocolRelease -> IO ()
ensureProtocolSchema conn ProtocolRelease {..} = do
  _ <- execute_ conn
    "CREATE TABLE IF NOT EXISTS protocol_releases (\
    \release_id TEXT PRIMARY KEY,\
    \chain_id BIGINT NOT NULL,\
    \name TEXT NOT NULL,\
    \deployment_block BIGINT NOT NULL,\
    \calculation_version TEXT NOT NULL,\
    \contracts JSONB NOT NULL,\
    \created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()\
    \)"
  _ <- execute_ conn
    "CREATE TABLE IF NOT EXISTS protocol_indexer_state (\
    \release_id TEXT NOT NULL REFERENCES protocol_releases(release_id),\
    \indexer_name TEXT NOT NULL,\
    \last_block BIGINT NOT NULL DEFAULT 0,\
    \last_block_hash TEXT,\
    \updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),\
    \PRIMARY KEY (release_id, indexer_name)\
    \)"
  _ <- execute_ conn protocolBlockCheckpointSchemaSql
  _ <- execute_ conn
    "CREATE TABLE IF NOT EXISTS protocol_transactions (\
    \release_id TEXT NOT NULL REFERENCES protocol_releases(release_id),\
    \chain_id BIGINT NOT NULL,\
    \tx_hash TEXT NOT NULL,\
    \block_number BIGINT NOT NULL,\
    \block_hash TEXT NOT NULL,\
    \tx_index BIGINT NOT NULL,\
    \timestamp BIGINT NOT NULL,\
    \sender TEXT,\
    \recipient TEXT,\
    \selector TEXT,\
    \status TEXT NOT NULL,\
    \gas_used NUMERIC,\
    \effective_gas_price NUMERIC,\
    \native_value NUMERIC,\
    \input_data TEXT,\
    \evidence JSONB NOT NULL,\
    \created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),\
    \updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),\
    \PRIMARY KEY (release_id, tx_hash)\
    \)"
  _ <- execute_ conn
    "CREATE TABLE IF NOT EXISTS protocol_events (\
    \release_id TEXT NOT NULL REFERENCES protocol_releases(release_id),\
    \chain_id BIGINT NOT NULL,\
    \tx_hash TEXT NOT NULL,\
    \log_index BIGINT NOT NULL,\
    \block_number BIGINT NOT NULL,\
    \block_hash TEXT NOT NULL,\
    \tx_index BIGINT NOT NULL,\
    \timestamp BIGINT NOT NULL,\
    \contract_address TEXT NOT NULL,\
    \event_name TEXT NOT NULL,\
    \raw_topics JSONB NOT NULL,\
    \raw_data TEXT NOT NULL,\
    \decoded_data JSONB NOT NULL,\
    \evidence JSONB NOT NULL,\
    \created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),\
    \PRIMARY KEY (release_id, tx_hash, log_index)\
    \)"
  _ <- execute_ conn
    "CREATE TABLE IF NOT EXISTS protocol_actions (\
    \release_id TEXT NOT NULL REFERENCES protocol_releases(release_id),\
    \action_id TEXT NOT NULL,\
    \tx_hash TEXT NOT NULL,\
    \block_number BIGINT NOT NULL,\
    \block_hash TEXT NOT NULL,\
    \tx_index BIGINT NOT NULL,\
    \log_index BIGINT NOT NULL,\
    \timestamp BIGINT NOT NULL,\
    \action_type TEXT NOT NULL,\
    \status TEXT NOT NULL,\
    \account TEXT,\
    \actor TEXT,\
    \order_id BIGINT,\
    \contract_address TEXT NOT NULL,\
    \data JSONB NOT NULL,\
    \evidence JSONB NOT NULL,\
    \created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),\
    \PRIMARY KEY (release_id, action_id)\
    \)"
  _ <- execute_ conn
    "CREATE TABLE IF NOT EXISTS protocol_state_snapshots (\
    \release_id TEXT NOT NULL REFERENCES protocol_releases(release_id),\
    \snapshot_scope TEXT NOT NULL,\
    \block_number BIGINT NOT NULL,\
    \block_hash TEXT NOT NULL,\
    \timestamp BIGINT NOT NULL,\
    \state JSONB NOT NULL,\
    \availability JSONB NOT NULL,\
    \calculation_version TEXT NOT NULL,\
    \created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),\
    \PRIMARY KEY (release_id, snapshot_scope, block_number)\
    \)"
  _ <- execute_ conn
    "CREATE TABLE IF NOT EXISTS protocol_parameter_changes (\
    \release_id TEXT NOT NULL REFERENCES protocol_releases(release_id),\
    \change_id TEXT NOT NULL,\
    \parameter_key TEXT NOT NULL,\
    \status TEXT NOT NULL,\
    \old_value JSONB,\
    \new_value JSONB,\
    \proposer TEXT,\
    \executor TEXT,\
    \proposed_at BIGINT,\
    \eta BIGINT,\
    \executed_at BIGINT,\
    \tx_hash TEXT NOT NULL,\
    \block_number BIGINT NOT NULL,\
    \evidence JSONB NOT NULL,\
    \PRIMARY KEY (release_id, change_id)\
    \)"
  _ <- execute_ conn
    "ALTER TABLE protocol_transactions ADD COLUMN IF NOT EXISTS input_data TEXT"
  _ <- execute_ conn
    "ALTER TABLE protocol_events ADD COLUMN IF NOT EXISTS raw_topics JSONB NOT NULL DEFAULT '[]'::jsonb"
  _ <- execute_ conn
    "ALTER TABLE protocol_events ADD COLUMN IF NOT EXISTS raw_data TEXT NOT NULL DEFAULT '0x'"
  ensureParameterChangeProjectionSchema conn
  _ <- execute_ conn
    "CREATE INDEX IF NOT EXISTS idx_protocol_actions_feed \
    \ON protocol_actions(release_id, block_number DESC, log_index DESC)"
  _ <- execute_ conn
    "CREATE INDEX IF NOT EXISTS idx_protocol_actions_actor \
    \ON protocol_actions(release_id, actor, timestamp DESC) WHERE actor IS NOT NULL"
  _ <- execute_ conn
    "CREATE INDEX IF NOT EXISTS idx_protocol_actions_order \
    \ON protocol_actions(release_id, order_id) WHERE order_id IS NOT NULL"
  let contracts =
        object
          [ "usdc" .= prUsdc
          , "orderRouter" .= prOrderRouter
          , "orderRouterAdmin" .= prOrderRouterAdmin
          , "cfdEngine" .= prCfdEngine
          , "cfdEngineAdmin" .= prCfdEngineAdmin
          , "marginClearinghouse" .= prMarginClearinghouse
          , "publicLens" .= prPublicLens
          , "accountLens" .= prAccountLens
          , "housePool" .= prHousePool
          , "seniorVault" .= prSeniorVault
          , "juniorVault" .= prJuniorVault
          , "pletherOracle" .= prPletherOracle
          ]
  _ <- execute conn
    "INSERT INTO protocol_releases \
    \(release_id, chain_id, name, deployment_block, calculation_version, contracts) \
    \VALUES (?, ?, ?, ?, ?, ?) \
    \ON CONFLICT (release_id) DO NOTHING"
    (prId, prChainId, prName, prDeploymentBlock, prCalculationVersion, encode contracts)
  stored <-
    query conn
      "SELECT chain_id, name, deployment_block, calculation_version, contracts \
      \FROM protocol_releases WHERE release_id = ?"
      (Only prId)
      :: IO [(Integer, Text, Integer, Text, Value)]
  let manifestMatches =
        case stored of
          [(chainId, name, deploymentBlock, calculationVersion, storedContracts)] ->
            chainId == prChainId
              && name == prName
              && deploymentBlock == prDeploymentBlock
              && calculationVersion == prCalculationVersion
              && storedContracts == contracts
          _ -> False
  when (not manifestMatches) $
    fail $ "Immutable protocol release manifest drift detected for " <> T.unpack prId
  pure ()

getProtocolIndexerCursor :: Connection -> Text -> Text -> IO (Integer, Maybe Text)
getProtocolIndexerCursor conn releaseId indexerName = do
  rows <-
    query conn
      "SELECT last_block, last_block_hash FROM protocol_indexer_state \
      \WHERE release_id = ? AND indexer_name = ?"
      (releaseId, indexerName)
  pure $ case rows of
    (lastBlock, lastHash) : _ -> (lastBlock, lastHash)
    [] -> (0, Nothing)

setProtocolIndexerCursor :: Connection -> Text -> Text -> Integer -> Maybe Text -> IO ()
setProtocolIndexerCursor conn releaseId indexerName lastBlock lastHash = do
  _ <-
    execute conn
      "INSERT INTO protocol_indexer_state \
      \(release_id, indexer_name, last_block, last_block_hash) VALUES (?, ?, ?, ?) \
      \ON CONFLICT (release_id, indexer_name) DO UPDATE SET \
      \last_block = EXCLUDED.last_block, last_block_hash = EXCLUDED.last_block_hash, updated_at = NOW()"
      (releaseId, indexerName, lastBlock, T.toLower <$> lastHash)
  pure ()

-- | Canonical release/indexer-scoped block hashes retained as reorg
-- checkpoints. Callers persist these in the same transaction as the range
-- projections and cursor advancement.
upsertProtocolBlockCheckpoints
  :: Connection
  -> Text
  -> Text
  -> [(Integer, Text)]
  -> IO ()
upsertProtocolBlockCheckpoints conn releaseId indexerName checkpoints = do
  _ <-
    executeMany conn
      protocolBlockCheckpointUpsertSql
      [ (releaseId, indexerName, blockNumber, T.toLower blockHash)
      | (blockNumber, blockHash) <- checkpoints
      ]
  pure ()

getProtocolBlockCheckpointsDescending
  :: Connection
  -> Text
  -> Text
  -> Integer
  -> IO [(Integer, Text)]
getProtocolBlockCheckpointsDescending conn releaseId indexerName maxBlock =
  query conn
    protocolBlockCheckpointsDescendingSql
    (releaseId, indexerName, maxBlock)

deleteProtocolBlockCheckpointsFromBlock
  :: Connection
  -> Text
  -> Text
  -> Integer
  -> IO ()
deleteProtocolBlockCheckpointsFromBlock conn releaseId indexerName blockNumber = do
  _ <-
    execute conn
      protocolBlockCheckpointDeleteSql
      (releaseId, indexerName, blockNumber)
  pure ()

protocolBlockCheckpointUpsertSql :: Query
protocolBlockCheckpointUpsertSql =
  "INSERT INTO protocol_indexed_blocks \
  \(release_id, indexer_name, block_number, block_hash) VALUES (?, ?, ?, ?) \
  \ON CONFLICT (release_id, indexer_name, block_number) DO UPDATE SET \
  \block_hash = EXCLUDED.block_hash, updated_at = NOW()"

protocolBlockCheckpointSchemaSql :: Query
protocolBlockCheckpointSchemaSql =
  "CREATE TABLE IF NOT EXISTS protocol_indexed_blocks (\
  \release_id TEXT NOT NULL REFERENCES protocol_releases(release_id),\
  \indexer_name TEXT NOT NULL,\
  \block_number BIGINT NOT NULL,\
  \block_hash TEXT NOT NULL,\
  \created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),\
  \updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),\
  \PRIMARY KEY (release_id, indexer_name, block_number)\
  \)"

protocolBlockCheckpointsDescendingSql :: Query
protocolBlockCheckpointsDescendingSql =
  "SELECT block_number, block_hash FROM protocol_indexed_blocks \
  \WHERE release_id = ? AND indexer_name = ? AND block_number <= ? \
  \ORDER BY block_number DESC"

protocolBlockCheckpointDeleteSql :: Query
protocolBlockCheckpointDeleteSql =
  "DELETE FROM protocol_indexed_blocks \
  \WHERE release_id = ? AND indexer_name = ? AND block_number >= ?"

insertProtocolLedgerEntry
  :: Connection
  -> Text
  -> Integer
  -> Text
  -> Text
  -> Integer
  -> Text
  -> Integer
  -> Integer
  -> Integer
  -> Text
  -> Text
  -> Text
  -> Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Integer
  -> Maybe Integer
  -> Maybe Integer
  -> Value
  -> Maybe Text
  -> Text
  -> Maybe Text
  -> Maybe Integer
  -> Value
  -> Value
  -> Value
  -> IO ()
insertProtocolLedgerEntry conn releaseId chainId txHash contractAddress blockNumber blockHash txIndex logIndex timestamp eventName actionType transactionStatus actionStatus sender recipient selector nativeValue gasUsed effectiveGasPrice rawTopics txInput rawEventData account orderId payload transactionEvidence eventEvidence = do
  let normalizedTx = T.toLower txHash
      normalizedContract = T.toLower contractAddress
      actionId = normalizedTx <> ":" <> T.pack (show logIndex)
      actor =
        if actionType `elem`
          [ "order_execution"
          , "order_cleanup"
          , "liquidation"
          , "keeper_maintenance"
          , "governance_proposal"
          , "governance_execution"
          , "governance_cancellation"
          , "ownership_transfer_started"
          , "ownership_transfer"
          , "pauser_update"
          , "pause"
          , "unpause"
          , "protocol_treasury_update"
          , "governance_role_change"
          ]
          then T.toLower <$> sender
          else Nothing
  _ <- execute conn
    "INSERT INTO protocol_transactions \
    \(release_id, chain_id, tx_hash, block_number, block_hash, tx_index, timestamp, sender, recipient, selector, status, gas_used, effective_gas_price, native_value, input_data, evidence) \
    \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) \
    \ON CONFLICT (release_id, tx_hash) DO UPDATE SET \
    \sender = COALESCE(protocol_transactions.sender, EXCLUDED.sender), \
    \recipient = COALESCE(protocol_transactions.recipient, EXCLUDED.recipient), \
    \selector = COALESCE(protocol_transactions.selector, EXCLUDED.selector), \
    \status = CASE \
    \  WHEN protocol_transactions.status <> 'unavailable' AND EXCLUDED.status = 'unavailable' \
    \  THEN protocol_transactions.status ELSE EXCLUDED.status END, \
    \gas_used = COALESCE(protocol_transactions.gas_used, EXCLUDED.gas_used), \
    \effective_gas_price = COALESCE(protocol_transactions.effective_gas_price, EXCLUDED.effective_gas_price), \
    \native_value = COALESCE(protocol_transactions.native_value, EXCLUDED.native_value), \
    \input_data = COALESCE(protocol_transactions.input_data, EXCLUDED.input_data), \
    \evidence = CASE \
    \  WHEN protocol_transactions.status <> 'unavailable' AND EXCLUDED.status = 'unavailable' \
    \  THEN protocol_transactions.evidence ELSE EXCLUDED.evidence END, \
    \updated_at = NOW()"
    ( releaseId, chainId, normalizedTx, blockNumber, T.toLower blockHash, txIndex
    , timestamp, fmap T.toLower sender, fmap T.toLower recipient, selector, transactionStatus
    , gasUsed, effectiveGasPrice, nativeValue, txInput, encode transactionEvidence
    )
  _ <- execute conn
    "INSERT INTO protocol_events \
    \(release_id, chain_id, tx_hash, log_index, block_number, block_hash, tx_index, timestamp, contract_address, event_name, raw_topics, raw_data, decoded_data, evidence) \
    \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) \
    \ON CONFLICT (release_id, tx_hash, log_index) DO NOTHING"
    ( releaseId, chainId, normalizedTx, logIndex, blockNumber, T.toLower blockHash
    , txIndex, timestamp, normalizedContract, eventName, encode rawTopics, rawEventData, encode payload, encode eventEvidence
    )
  when (actionType /= "unclassified_event") $ do
    _ <- execute conn
      "INSERT INTO protocol_actions \
      \(release_id, action_id, tx_hash, block_number, block_hash, tx_index, log_index, timestamp, action_type, status, account, actor, order_id, contract_address, data, evidence) \
      \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) \
      \ON CONFLICT (release_id, action_id) DO NOTHING"
      ( releaseId, actionId, normalizedTx, blockNumber, T.toLower blockHash, txIndex, logIndex
      , timestamp, actionType, actionStatus, fmap T.toLower account, actor, orderId
      , normalizedContract, encode payload, encode eventEvidence
      )
    when (actionType `elem` parameterProjectionActionTypes) $ do
        versions <-
          query conn
            "SELECT calculation_version FROM protocol_releases WHERE release_id = ?"
            (Only releaseId)
            :: IO [Only Text]
        forM_ versions $ \(Only calculationVersion) ->
          projectParameterChangeAction
            conn
            releaseId
            calculationVersion
            actionId
            normalizedTx
            blockNumber
            logIndex
            timestamp
            actionType
            actor
            normalizedContract
            payload
            eventEvidence
    pure ()
  pure ()

-- | Persist one release-scoped snapshot without allowing a partial retry to
-- overwrite a more complete result for the same canonical block. A block-hash
-- mismatch is deliberately left untouched for the reorg verifier to rewind.
upsertProtocolStateSnapshot
  :: Connection
  -> Text
  -> Text
  -> Integer
  -> Text
  -> Integer
  -> Value
  -> Value
  -> Text
  -> IO ()
upsertProtocolStateSnapshot conn releaseId scope blockNumber blockHash timestamp state availability calculationVersion = do
  _ <- execute conn
    "INSERT INTO protocol_state_snapshots \
    \(release_id, snapshot_scope, block_number, block_hash, timestamp, state, availability, calculation_version) \
    \VALUES (?, ?, ?, ?, ?, ?, ?, ?) \
    \ON CONFLICT (release_id, snapshot_scope, block_number) DO UPDATE SET \
    \block_hash = EXCLUDED.block_hash, \
    \timestamp = EXCLUDED.timestamp, \
    \state = EXCLUDED.state, \
    \availability = EXCLUDED.availability, \
    \calculation_version = EXCLUDED.calculation_version \
    \WHERE protocol_state_snapshots.block_hash = EXCLUDED.block_hash \
    \AND jsonb_array_length(protocol_state_snapshots.availability) \
    \  > jsonb_array_length(EXCLUDED.availability)"
    ( releaseId
    , scope
    , blockNumber
    , T.toLower blockHash
    , timestamp
    , encode state
    , encode availability
    , calculationVersion
    )
  pure ()

deleteProtocolLedgerFromBlock :: Connection -> Text -> Integer -> IO ()
deleteProtocolLedgerFromBlock conn releaseId blockNumber = do
  _ <- execute conn "DELETE FROM protocol_actions WHERE release_id = ? AND block_number >= ?" (releaseId, blockNumber)
  _ <- execute conn "DELETE FROM protocol_events WHERE release_id = ? AND block_number >= ?" (releaseId, blockNumber)
  _ <- execute conn "DELETE FROM protocol_transactions WHERE release_id = ? AND block_number >= ?" (releaseId, blockNumber)
  _ <- execute conn "DELETE FROM protocol_state_snapshots WHERE release_id = ? AND block_number >= ?" (releaseId, blockNumber)
  versions <-
    query conn
      "SELECT calculation_version FROM protocol_releases WHERE release_id = ?"
      (Only releaseId)
      :: IO [Only Text]
  case versions of
    Only calculationVersion : _ ->
      rebuildParameterChangeProjection conn releaseId calculationVersion
    [] -> do
      _ <- execute conn
        "DELETE FROM protocol_parameter_changes WHERE release_id = ?"
        (Only releaseId)
      pure ()
  pure ()

listProtocolActions
  :: Connection -> Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text
  -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Integer -> Maybe Integer
  -> Integer -> Int -> Maybe (Integer, Integer) -> IO [ProtocolActionRow]
listProtocolActions conn releaseId mType mOutcome mAddress mAccount mActor mContract mTxHash mFromTimestamp mToTimestamp maxBlock limitRows cursor =
  query conn
    listProtocolActionsQuerySql
    [ toField releaseId
    , toField mType, toField mType
    , toField mAddress, toField mAddress, toField mAddress
    , toField mAccount, toField mAccount
    , toField mActor, toField mActor
    , toField mContract, toField mContract
    , toField mTxHash, toField mTxHash
    , toField mFromTimestamp, toField mFromTimestamp
    , toField mToTimestamp, toField mToTimestamp
    , toField mOutcome, toField mOutcome
    , toField maxBlock
    , toField (fst <$> cursor), toField (fst <$> cursor)
    , toField (fst <$> cursor), toField (snd <$> cursor)
    , toField limitRows
    ]

listProtocolActionsQuerySql :: Query
listProtocolActionsQuerySql =
  "WITH feed AS (\
  \SELECT release_id, action_id, tx_hash, block_number, block_hash, tx_index, log_index, timestamp, \
  \action_type, status, account, actor, order_id, contract_address, data, evidence \
  \FROM protocol_actions \
  \UNION ALL \
  \SELECT events.release_id, events.tx_hash || ':' || events.log_index::text, \
  \events.tx_hash, events.block_number, events.block_hash, events.tx_index, \
  \events.log_index, events.timestamp, 'unclassified_event', 'unavailable', \
  \NULL, NULL, NULL, events.contract_address, \
  \jsonb_build_object(\
  \  'eventName', events.event_name, 'decodedData', events.decoded_data, \
  \  'rawTopics', events.raw_topics, 'rawData', events.raw_data), \
  \events.evidence || jsonb_build_object(\
  \  'level', 'unavailable', 'source', 'confirmed_log', \
  \  'virtualAction', true, 'reason', 'typed_action_unavailable') \
  \FROM protocol_events events \
  \WHERE NOT EXISTS (\
  \  SELECT 1 FROM protocol_actions actions \
  \  WHERE actions.release_id = events.release_id \
  \  AND actions.action_id = events.tx_hash || ':' || events.log_index::text)) \
  \SELECT action_id, tx_hash, block_number, block_hash, tx_index, log_index, timestamp, \
  \action_type, status, account, actor, order_id, contract_address, data, evidence \
  \FROM feed WHERE feed.release_id = ? \
  \AND (?::text IS NULL OR feed.action_type = ?) \
  \AND (?::text IS NULL OR feed.account = LOWER(?) OR feed.actor = LOWER(?)) \
  \AND (?::text IS NULL OR feed.account = LOWER(?)) \
  \AND (?::text IS NULL OR feed.actor = LOWER(?)) \
  \AND (?::text IS NULL OR feed.contract_address = LOWER(?)) \
  \AND (?::text IS NULL OR feed.tx_hash = LOWER(?)) \
  \AND (?::bigint IS NULL OR feed.timestamp >= ?) \
  \AND (?::bigint IS NULL OR feed.timestamp <= ?) \
  \AND (?::text IS NULL OR feed.status = ?) \
  \AND feed.block_number <= ? \
  \AND (?::bigint IS NULL OR feed.block_number < ? \
  \  OR (feed.block_number = ? AND feed.log_index < ?)) \
  \ORDER BY feed.block_number DESC, feed.log_index DESC LIMIT ?"

getProtocolActionsByTransaction :: Connection -> Text -> Text -> Integer -> IO [ProtocolActionRow]
getProtocolActionsByTransaction conn releaseId txHash maxBlock =
  query conn
    protocolActionsByTransactionQuerySql
    (releaseId, txHash, maxBlock)

protocolActionsByTransactionQuerySql :: Query
protocolActionsByTransactionQuerySql =
  "SELECT actions.action_id, actions.tx_hash, actions.block_number, actions.block_hash, \
  \actions.tx_index, actions.log_index, actions.timestamp, actions.action_type, \
  \actions.status, actions.account, actions.actor, actions.order_id, \
  \actions.contract_address, actions.data, actions.evidence \
  \FROM protocol_actions actions \
  \WHERE actions.release_id = ? AND actions.tx_hash = LOWER(?) \
  \AND actions.block_number <= ? \
  \ORDER BY actions.log_index ASC, actions.action_id ASC"

getProtocolActionsByOrder :: Connection -> Text -> Integer -> Integer -> IO [ProtocolActionRow]
getProtocolActionsByOrder conn releaseId orderId maxBlock =
  query conn
    protocolActionsByOrderQuerySql
    (releaseId, orderId, maxBlock)

protocolActionsByOrderQuerySql :: Query
protocolActionsByOrderQuerySql =
  "SELECT actions.action_id, actions.tx_hash, actions.block_number, actions.block_hash, \
  \actions.tx_index, actions.log_index, actions.timestamp, actions.action_type, \
  \actions.status, actions.account, actions.actor, actions.order_id, \
  \actions.contract_address, actions.data, actions.evidence \
  \FROM protocol_actions actions \
  \WHERE actions.release_id = ? AND actions.order_id = ? \
  \AND actions.block_number <= ? \
  \ORDER BY actions.block_number ASC, actions.log_index ASC, actions.action_id ASC"

getProtocolTransaction :: Connection -> Text -> Text -> Integer -> IO (Maybe ProtocolTransactionRow)
getProtocolTransaction conn releaseId txHash maxBlock = do
  rows <- query conn
    protocolTransactionQuerySql
    (releaseId, txHash, maxBlock)
  pure $ case rows of
    row : _ -> Just row
    [] -> Nothing

protocolTransactionQuerySql :: Query
protocolTransactionQuerySql =
  "SELECT transactions.tx_hash, transactions.block_number, transactions.block_hash, \
  \transactions.tx_index, transactions.timestamp, transactions.sender, \
  \transactions.recipient, transactions.selector, transactions.status, \
  \transactions.gas_used, transactions.effective_gas_price, transactions.native_value, \
  \transactions.input_data, transactions.evidence \
  \FROM protocol_transactions transactions \
  \WHERE transactions.release_id = ? AND transactions.tx_hash = LOWER(?) \
  \AND transactions.block_number <= ?"

getProtocolTransactionsByHashes
  :: Connection
  -> Text
  -> [Text]
  -> Integer
  -> IO [ProtocolTransactionRow]
getProtocolTransactionsByHashes _ _ [] _ = pure []
getProtocolTransactionsByHashes conn releaseId txHashes maxBlock =
  query conn
    "SELECT transactions.tx_hash, transactions.block_number, transactions.block_hash, \
    \transactions.tx_index, transactions.timestamp, transactions.sender, \
    \transactions.recipient, transactions.selector, transactions.status, \
    \transactions.gas_used, transactions.effective_gas_price, transactions.native_value, \
    \transactions.input_data, transactions.evidence \
    \FROM protocol_transactions transactions \
    \WHERE transactions.release_id = ? AND transactions.tx_hash IN ? \
    \AND transactions.block_number <= ?"
    (releaseId, In $ map T.toLower txHashes, maxBlock)

getProtocolEventsByTransaction :: Connection -> Text -> Text -> Integer -> IO [ProtocolEventRow]
getProtocolEventsByTransaction conn releaseId txHash maxBlock =
  query conn
    protocolEventsByTransactionQuerySql
    (releaseId, txHash, maxBlock)

protocolEventsByTransactionQuerySql :: Query
protocolEventsByTransactionQuerySql =
  "SELECT events.log_index, events.contract_address, events.event_name, \
  \events.raw_topics, events.raw_data, events.decoded_data, events.evidence \
  \FROM protocol_events events \
  \WHERE events.release_id = ? AND events.tx_hash = LOWER(?) \
  \AND events.block_number <= ? \
  \ORDER BY events.log_index ASC"

getProtocolOverviewCounts :: Connection -> Text -> Integer -> Integer -> IO (Integer, Integer, Integer, Integer)
getProtocolOverviewCounts conn releaseId sinceTimestamp maxBlock = do
  rows <- query conn
    protocolOverviewCountsQuerySql
    (releaseId, sinceTimestamp, maxBlock)
  pure $ case rows of
    [(actions, liquidations, keepers, failures)] -> (actions, liquidations, keepers, failures)
    _ -> (0, 0, 0, 0)

protocolOverviewCountsQuerySql :: Query
protocolOverviewCountsQuerySql =
  "SELECT COUNT(*)::BIGINT, \
  \COUNT(*) FILTER (WHERE actions.action_type = 'liquidation')::BIGINT, \
  \COUNT(DISTINCT actions.actor) FILTER (WHERE actions.actor IS NOT NULL \
  \AND actions.action_type IN ('order_execution', 'order_cleanup', 'liquidation', 'keeper_maintenance'))::BIGINT, \
  \COUNT(*) FILTER (WHERE actions.status IN ('failed', 'reverted') \
  \OR (actions.action_type = 'order_cleanup' \
  \AND COALESCE(actions.data->>'reasonName', '') NOT IN ('', 'Expired')))::BIGINT \
  \FROM protocol_actions actions \
  \WHERE actions.release_id = ? AND actions.timestamp >= ? \
  \AND actions.block_number <= ?"

getProtocolIndexedHead :: Connection -> Text -> IO (Maybe (Integer, Text, Integer))
getProtocolIndexedHead conn releaseId = do
  rows <- query conn
    "SELECT last_block, COALESCE(last_block_hash, ''), \
    \FLOOR(EXTRACT(EPOCH FROM updated_at))::BIGINT \
    \FROM protocol_indexer_state \
    \WHERE release_id = ? ORDER BY updated_at DESC LIMIT 1"
    (Only releaseId)
  pure $ case rows of
    row : _ -> Just row
    [] -> Nothing

-- | Read the completeness cursor for one concrete projection. Unlike
-- 'getProtocolIndexedHead', this never selects whichever release indexer
-- happened to update most recently: callers name the projection whose
-- contiguous coverage they require.
getProtocolProjectionHead
  :: Connection
  -> Text
  -> Text
  -> IO (Maybe (Integer, Text, Integer))
getProtocolProjectionHead conn releaseId indexerName = do
  rows <-
    query
      conn
      protocolProjectionHeadQuerySql
      (releaseId, indexerName)
  pure $ case rows of
    row : _ -> Just row
    [] -> Nothing

protocolProjectionHeadQuerySql :: Query
protocolProjectionHeadQuerySql =
  "SELECT last_block, COALESCE(last_block_hash, ''), \
  \FLOOR(EXTRACT(EPOCH FROM updated_at))::BIGINT \
  \FROM protocol_indexer_state \
  \WHERE release_id = ? AND indexer_name = ? \
  \LIMIT 1"

getProtocolPendingOrderTimes :: Connection -> Integer -> Text -> Integer -> IO [Integer]
getProtocolPendingOrderTimes conn chainId orderRouter maxBlock = do
  rows <- query conn
    protocolPendingOrderTimesQuerySql
    (chainId, orderRouter, maxBlock, maxBlock)
  pure [timestamp | Only timestamp <- rows]

protocolPendingOrderTimesQuerySql :: Query
protocolPendingOrderTimesQuerySql =
  "SELECT orders.commit_timestamp FROM perps_orders orders \
  \WHERE orders.chain_id = ? AND orders.order_router = LOWER(?) \
  \AND orders.commit_timestamp IS NOT NULL \
  \AND orders.commit_block_number <= ? \
  \AND (orders.terminal_block_number IS NULL OR orders.terminal_block_number > ?) \
  \ORDER BY orders.commit_timestamp ASC, orders.order_id ASC"

getKeeperAggregates :: Connection -> Text -> Integer -> Integer -> IO [KeeperAggregateRow]
getKeeperAggregates conn releaseId sinceTimestamp maxBlock =
  query conn
    keeperAggregatesQuerySql
    (releaseId, sinceTimestamp, maxBlock)

keeperAggregatesQuerySql :: Query
keeperAggregatesQuerySql =
  "SELECT actions.actor, COUNT(*)::BIGINT, \
  \COUNT(*) FILTER (WHERE actions.action_type = 'order_execution')::BIGINT, \
  \COUNT(*) FILTER (WHERE actions.action_type = 'order_cleanup')::BIGINT, \
  \COUNT(*) FILTER (WHERE actions.action_type = 'liquidation')::BIGINT, \
  \COALESCE(SUM(CASE WHEN actions.action_type = 'liquidation' \
  \THEN NULLIF(actions.data->>'keeperBountyUsdc','')::NUMERIC ELSE 0 END),0)::BIGINT, \
  \MIN(actions.timestamp), MAX(actions.timestamp) \
  \FROM protocol_actions actions \
  \WHERE actions.release_id = ? AND actions.actor IS NOT NULL \
  \AND actions.timestamp >= ? AND actions.block_number <= ? \
  \AND actions.status = 'success' \
  \AND actions.action_type IN ('order_execution', 'order_cleanup', 'liquidation', 'keeper_maintenance') \
  \GROUP BY actions.actor ORDER BY 6 DESC, 2 DESC, actions.actor ASC"

-- | Page keeper aggregates with the same deterministic reward/action/address
-- ordering used by the concentration leaders. The cursor tuple is the last
-- visible (reward, action count, actor) row.
getKeeperAggregatesPage
  :: Connection
  -> Text
  -> Integer
  -> Integer
  -> Int
  -> Maybe (Integer, Integer, Text)
  -> IO [KeeperAggregateRow]
getKeeperAggregatesPage conn releaseId sinceTimestamp maxBlock limitRows cursor =
  query conn
    keeperAggregatesPageQuerySql
    ( releaseId
    , sinceTimestamp
    , maxBlock
    , cursorReward
    , cursorReward
    , cursorReward
    , cursorActions
    , cursorReward
    , cursorActions
    , cursorActor
    , limitRows
    )
  where
    cursorReward = (\(reward, _, _) -> reward) <$> cursor
    cursorActions = (\(_, actions, _) -> actions) <$> cursor
    cursorActor = (\(_, _, actor) -> actor) <$> cursor

keeperAggregatesPageQuerySql :: Query
keeperAggregatesPageQuerySql =
  "WITH keeper_aggregates AS (\
  \SELECT actions.actor, COUNT(*)::BIGINT AS action_count, \
  \COUNT(*) FILTER (WHERE actions.action_type = 'order_execution')::BIGINT AS execution_count, \
  \COUNT(*) FILTER (WHERE actions.action_type = 'order_cleanup')::BIGINT AS cleanup_count, \
  \COUNT(*) FILTER (WHERE actions.action_type = 'liquidation')::BIGINT AS liquidation_count, \
  \COALESCE(SUM(CASE WHEN actions.action_type = 'liquidation' \
  \THEN NULLIF(actions.data->>'keeperBountyUsdc','')::NUMERIC ELSE 0 END),0)::BIGINT AS observed_rewards, \
  \MIN(actions.timestamp) AS first_action_at, MAX(actions.timestamp) AS last_action_at \
  \FROM protocol_actions actions \
  \WHERE actions.release_id = ? AND actions.actor IS NOT NULL \
  \AND actions.timestamp >= ? AND actions.block_number <= ? \
  \AND actions.status = 'success' \
  \AND actions.action_type IN ('order_execution', 'order_cleanup', 'liquidation', 'keeper_maintenance') \
  \GROUP BY actions.actor) \
  \SELECT actor, action_count, execution_count, cleanup_count, liquidation_count, \
  \observed_rewards, first_action_at, last_action_at FROM keeper_aggregates \
  \WHERE (?::numeric IS NULL OR observed_rewards < ? \
  \OR (observed_rewards = ? AND action_count < ?) \
  \OR (observed_rewards = ? AND action_count = ? AND actor > LOWER(?))) \
  \ORDER BY observed_rewards DESC, action_count DESC, actor ASC LIMIT ?"

getKeeperWindowSummary
  :: Connection -> Text -> Integer -> Integer -> IO KeeperWindowSummaryRow
getKeeperWindowSummary conn releaseId sinceTimestamp maxBlock = do
  rows <- query conn keeperWindowSummaryQuerySql (releaseId, sinceTimestamp, maxBlock)
  pure $ case rows of
    row : _ -> row
    [] -> KeeperWindowSummaryRow 0 0 0 0 0 0

keeperWindowSummaryQuerySql :: Query
keeperWindowSummaryQuerySql =
  "WITH keeper_aggregates AS (\
  \SELECT actions.actor, COUNT(*)::BIGINT AS action_count, \
  \COUNT(*) FILTER (WHERE actions.action_type = 'order_execution')::BIGINT AS execution_count, \
  \COUNT(*) FILTER (WHERE actions.action_type = 'order_cleanup')::BIGINT AS cleanup_count, \
  \COUNT(*) FILTER (WHERE actions.action_type = 'liquidation')::BIGINT AS liquidation_count, \
  \COALESCE(SUM(CASE WHEN actions.action_type = 'liquidation' \
  \THEN NULLIF(actions.data->>'keeperBountyUsdc','')::NUMERIC ELSE 0 END),0)::BIGINT AS observed_rewards \
  \FROM protocol_actions actions \
  \WHERE actions.release_id = ? AND actions.actor IS NOT NULL \
  \AND actions.timestamp >= ? AND actions.block_number <= ? \
  \AND actions.status = 'success' \
  \AND actions.action_type IN ('order_execution', 'order_cleanup', 'liquidation', 'keeper_maintenance') \
  \GROUP BY actions.actor) \
  \SELECT COUNT(*)::BIGINT, COALESCE(SUM(action_count),0)::BIGINT, \
  \COALESCE(SUM(execution_count),0)::BIGINT, COALESCE(SUM(cleanup_count),0)::BIGINT, \
  \COALESCE(SUM(liquidation_count),0)::BIGINT, COALESCE(SUM(observed_rewards),0)::BIGINT \
  \FROM keeper_aggregates"

getKeeperRewardLeaders
  :: Connection -> Text -> Integer -> Integer -> IO [KeeperAggregateRow]
getKeeperRewardLeaders conn releaseId sinceTimestamp maxBlock =
  query conn keeperRewardLeadersQuerySql (releaseId, sinceTimestamp, maxBlock)

keeperRewardLeadersQuerySql :: Query
keeperRewardLeadersQuerySql =
  "SELECT actions.actor, COUNT(*)::BIGINT, \
  \COUNT(*) FILTER (WHERE actions.action_type = 'order_execution')::BIGINT, \
  \COUNT(*) FILTER (WHERE actions.action_type = 'order_cleanup')::BIGINT, \
  \COUNT(*) FILTER (WHERE actions.action_type = 'liquidation')::BIGINT, \
  \COALESCE(SUM(CASE WHEN actions.action_type = 'liquidation' \
  \THEN NULLIF(actions.data->>'keeperBountyUsdc','')::NUMERIC ELSE 0 END),0)::BIGINT, \
  \MIN(actions.timestamp), MAX(actions.timestamp) \
  \FROM protocol_actions actions \
  \WHERE actions.release_id = ? AND actions.actor IS NOT NULL \
  \AND actions.timestamp >= ? AND actions.block_number <= ? \
  \AND actions.status = 'success' \
  \AND actions.action_type IN ('order_execution', 'order_cleanup', 'liquidation', 'keeper_maintenance') \
  \GROUP BY actions.actor ORDER BY 6 DESC, 2 DESC, actions.actor ASC LIMIT 8"

getKeeperActions
  :: Connection
  -> Text
  -> Text
  -> Integer
  -> Integer
  -> Int
  -> Maybe (Integer, Integer)
  -> IO [ProtocolActionRow]
getKeeperActions conn releaseId actor sinceTimestamp maxBlock limitRows cursor =
  query conn
    keeperActionsQuerySql
    ( releaseId
    , actor
    , sinceTimestamp
    , maxBlock
    , fst <$> cursor
    , fst <$> cursor
    , fst <$> cursor
    , snd <$> cursor
    , limitRows
    )

keeperActionsQuerySql :: Query
keeperActionsQuerySql =
  "SELECT actions.action_id, actions.tx_hash, actions.block_number, actions.block_hash, \
  \actions.tx_index, actions.log_index, actions.timestamp, actions.action_type, \
  \actions.status, actions.account, actions.actor, actions.order_id, \
  \actions.contract_address, actions.data, actions.evidence \
  \FROM protocol_actions actions \
  \WHERE actions.release_id = ? AND actions.actor = LOWER(?) \
  \AND actions.timestamp >= ? AND actions.block_number <= ? \
  \AND actions.status = 'success' \
  \AND actions.action_type IN ('order_execution', 'order_cleanup', 'liquidation', 'keeper_maintenance') \
  \AND (?::bigint IS NULL OR actions.block_number < ? \
  \  OR (actions.block_number = ? AND actions.log_index < ?)) \
  \ORDER BY actions.block_number DESC, actions.log_index DESC LIMIT ?"

getKeeperLatencySamples :: Connection -> Text -> Integer -> Integer -> IO [Integer]
getKeeperLatencySamples conn releaseId sinceTimestamp maxBlock = do
  rows <- query conn
    keeperLatencySamplesQuerySql
    (releaseId, sinceTimestamp, maxBlock, maxBlock)
  pure [sample | Only sample <- rows]

keeperLatencySamplesQuerySql :: Query
keeperLatencySamplesQuerySql =
  "SELECT GREATEST(0, terminal.timestamp - committed.timestamp)::BIGINT \
  \FROM protocol_actions terminal \
  \JOIN protocol_actions committed \
  \ON committed.release_id = terminal.release_id \
  \AND committed.order_id = terminal.order_id \
  \AND committed.action_type = 'order_commitment' \
  \WHERE terminal.release_id = ? AND terminal.actor IS NOT NULL \
  \AND terminal.timestamp >= ? \
  \AND terminal.block_number <= ? AND committed.block_number <= ? \
  \AND terminal.status = 'success' \
  \AND terminal.action_type IN ('order_execution', 'order_cleanup')"

getKeeperLatencyPercentiles
  :: Connection
  -> Text
  -> Integer
  -> Integer
  -> IO (Maybe Integer, Maybe Integer, Maybe Integer)
getKeeperLatencyPercentiles conn releaseId sinceTimestamp maxBlock = do
  rows <-
    query
      conn
      keeperLatencyPercentilesQuerySql
      (releaseId, sinceTimestamp, maxBlock, maxBlock)
  pure $ case rows of
    row : _ -> row
    [] -> (Nothing, Nothing, Nothing)

keeperLatencyPercentilesQuerySql :: Query
keeperLatencyPercentilesQuerySql =
  "WITH latency_samples AS (\
  \SELECT GREATEST(0, terminal.timestamp - committed.timestamp)::BIGINT AS latency_seconds \
  \FROM protocol_actions terminal \
  \JOIN protocol_actions committed \
  \ON committed.release_id = terminal.release_id \
  \AND committed.order_id = terminal.order_id \
  \AND committed.action_type = 'order_commitment' \
  \WHERE terminal.release_id = ? AND terminal.actor IS NOT NULL \
  \AND terminal.timestamp >= ? \
  \AND terminal.block_number <= ? AND committed.block_number <= ? \
  \AND terminal.status = 'success' \
  \AND terminal.action_type IN ('order_execution', 'order_cleanup')) \
  \SELECT PERCENTILE_DISC(0.50) WITHIN GROUP (ORDER BY latency_seconds)::BIGINT, \
  \PERCENTILE_DISC(0.90) WITHIN GROUP (ORDER BY latency_seconds)::BIGINT, \
  \PERCENTILE_DISC(0.99) WITHIN GROUP (ORDER BY latency_seconds)::BIGINT \
  \FROM latency_samples"

-- | Aggregate exact native-denominated transaction costs without converting
-- them to USDC or assuming that all transaction value is a Pyth fee. The
-- distinct action/transaction pair prevents batch logs from double-counting a
-- single keeper transaction.
getKeeperNativeCosts :: Connection -> Text -> Integer -> Integer -> IO [KeeperNativeCostRow]
getKeeperNativeCosts conn releaseId sinceTimestamp maxBlock =
  query conn
    keeperNativeCostsQuerySql
    (releaseId, sinceTimestamp, maxBlock, releaseId, maxBlock)

keeperNativeCostsQuerySql :: Query
keeperNativeCostsQuerySql =
  "WITH keeper_transactions AS (\
  \SELECT DISTINCT actions.actor, actions.tx_hash FROM protocol_actions actions \
  \WHERE actions.release_id = ? AND actions.actor IS NOT NULL \
  \AND actions.timestamp >= ? AND actions.block_number <= ? \
  \AND actions.status = 'success' \
  \AND actions.action_type IN ('order_execution', 'order_cleanup', 'liquidation', 'keeper_maintenance')) \
  \SELECT keeper_transactions.actor, \
  \COALESCE(SUM(CASE \
  \  WHEN transactions.gas_used IS NOT NULL AND transactions.effective_gas_price IS NOT NULL \
  \  THEN transactions.gas_used * transactions.effective_gas_price ELSE 0 END), 0)::NUMERIC, \
  \COALESCE(SUM(COALESCE(transactions.native_value, 0)), 0)::NUMERIC, \
  \COUNT(*) FILTER (WHERE transactions.gas_used IS NULL \
  \  OR transactions.effective_gas_price IS NULL)::BIGINT, \
  \COUNT(*) FILTER (WHERE transactions.native_value IS NULL)::BIGINT \
  \FROM keeper_transactions \
  \LEFT JOIN protocol_transactions transactions \
  \ON transactions.release_id = ? \
  \AND transactions.tx_hash = keeper_transactions.tx_hash \
  \AND transactions.block_number <= ? \
  \GROUP BY keeper_transactions.actor ORDER BY keeper_transactions.actor ASC"

getKeeperNativeCostSummary
  :: Connection
  -> Text
  -> Integer
  -> Integer
  -> IO KeeperNativeCostSummaryRow
getKeeperNativeCostSummary conn releaseId sinceTimestamp maxBlock = do
  rows <-
    query
      conn
      keeperNativeCostSummaryQuerySql
      (releaseId, sinceTimestamp, maxBlock, releaseId, maxBlock)
  pure $ case rows of
    row : _ -> row
    [] -> KeeperNativeCostSummaryRow 0 0 0 0

keeperNativeCostSummaryQuerySql :: Query
keeperNativeCostSummaryQuerySql =
  "WITH keeper_transactions AS (\
  \SELECT DISTINCT actions.actor, actions.tx_hash FROM protocol_actions actions \
  \WHERE actions.release_id = ? AND actions.actor IS NOT NULL \
  \AND actions.timestamp >= ? AND actions.block_number <= ? \
  \AND actions.status = 'success' \
  \AND actions.action_type IN ('order_execution', 'order_cleanup', 'liquidation', 'keeper_maintenance')) \
  \SELECT COALESCE(SUM(CASE \
  \  WHEN transactions.gas_used IS NOT NULL AND transactions.effective_gas_price IS NOT NULL \
  \  THEN transactions.gas_used * transactions.effective_gas_price ELSE 0 END), 0)::NUMERIC, \
  \COALESCE(SUM(COALESCE(transactions.native_value, 0)), 0)::NUMERIC, \
  \COUNT(*) FILTER (WHERE transactions.gas_used IS NULL \
  \  OR transactions.effective_gas_price IS NULL)::BIGINT, \
  \COUNT(*) FILTER (WHERE transactions.native_value IS NULL)::BIGINT \
  \FROM keeper_transactions \
  \LEFT JOIN protocol_transactions transactions \
  \ON transactions.release_id = ? \
  \AND transactions.tx_hash = keeper_transactions.tx_hash \
  \AND transactions.block_number <= ?"

getKeeperNativeCostsForActors
  :: Connection
  -> Text
  -> Integer
  -> Integer
  -> [Text]
  -> IO [KeeperNativeCostRow]
getKeeperNativeCostsForActors _ _ _ _ [] = pure []
getKeeperNativeCostsForActors conn releaseId sinceTimestamp maxBlock actors =
  query
    conn
    keeperNativeCostsForActorsQuerySql
    (releaseId, sinceTimestamp, maxBlock, In actors, releaseId, maxBlock)

keeperNativeCostsForActorsQuerySql :: Query
keeperNativeCostsForActorsQuerySql =
  "WITH keeper_transactions AS (\
  \SELECT DISTINCT actions.actor, actions.tx_hash FROM protocol_actions actions \
  \WHERE actions.release_id = ? AND actions.actor IS NOT NULL \
  \AND actions.timestamp >= ? AND actions.block_number <= ? \
  \AND actions.status = 'success' \
  \AND actions.action_type IN ('order_execution', 'order_cleanup', 'liquidation', 'keeper_maintenance') \
  \AND actions.actor IN ?) \
  \SELECT keeper_transactions.actor, \
  \COALESCE(SUM(CASE \
  \  WHEN transactions.gas_used IS NOT NULL AND transactions.effective_gas_price IS NOT NULL \
  \  THEN transactions.gas_used * transactions.effective_gas_price ELSE 0 END), 0)::NUMERIC, \
  \COALESCE(SUM(COALESCE(transactions.native_value, 0)), 0)::NUMERIC, \
  \COUNT(*) FILTER (WHERE transactions.gas_used IS NULL \
  \  OR transactions.effective_gas_price IS NULL)::BIGINT, \
  \COUNT(*) FILTER (WHERE transactions.native_value IS NULL)::BIGINT \
  \FROM keeper_transactions \
  \LEFT JOIN protocol_transactions transactions \
  \ON transactions.release_id = ? \
  \AND transactions.tx_hash = keeper_transactions.tx_hash \
  \AND transactions.block_number <= ? \
  \GROUP BY keeper_transactions.actor ORDER BY keeper_transactions.actor ASC"

getOperationalWalletActivity
  :: Connection
  -> Text
  -> Integer
  -> Integer
  -> IO [OperationalWalletActivityRow]
getOperationalWalletActivity conn releaseId sinceTimestamp maxBlock =
  query
    conn
    operationalWalletActivityQuerySql
    (releaseId, sinceTimestamp, maxBlock)

operationalWalletActivityQuerySql :: Query
operationalWalletActivityQuerySql =
  "SELECT actions.actor, COUNT(*)::BIGINT, \
  \COUNT(DISTINCT actions.tx_hash)::BIGINT, \
  \COUNT(*) FILTER (WHERE actions.action_type = 'order_execution')::BIGINT, \
  \COUNT(*) FILTER (WHERE actions.action_type = 'order_cleanup')::BIGINT, \
  \COUNT(*) FILTER (WHERE actions.action_type = 'liquidation')::BIGINT, \
  \COUNT(*) FILTER (WHERE actions.action_type = 'keeper_maintenance')::BIGINT, \
  \COUNT(*) FILTER (WHERE actions.action_type IN (\
  \  'governance_proposal', 'governance_execution', 'governance_cancellation', \
  \  'ownership_transfer_started', 'ownership_transfer', 'pauser_update', \
  \  'pause', 'unpause', 'protocol_treasury_update', 'governance_role_change'))::BIGINT, \
  \MIN(actions.timestamp), MAX(actions.timestamp) \
  \FROM protocol_actions actions \
  \WHERE actions.release_id = ? AND actions.actor IS NOT NULL \
  \AND actions.timestamp >= ? AND actions.block_number <= ? \
  \AND actions.status = 'success' \
  \AND actions.action_type IN (\
  \  'order_execution', 'order_cleanup', 'liquidation', 'keeper_maintenance', \
  \  'governance_proposal', 'governance_execution', 'governance_cancellation', \
  \  'ownership_transfer_started', 'ownership_transfer', 'pauser_update', \
  \  'pause', 'unpause', 'protocol_treasury_update', 'governance_role_change') \
  \GROUP BY actions.actor ORDER BY actions.actor ASC"

getOperationalWalletActions
  :: Connection
  -> Text
  -> Text
  -> Integer
  -> Integer
  -> Int
  -> Maybe (Integer, Integer)
  -> IO [ProtocolActionRow]
getOperationalWalletActions conn releaseId actor sinceTimestamp maxBlock limitRows cursor =
  query
    conn
    operationalWalletActionsQuerySql
    ( releaseId
    , actor
    , sinceTimestamp
    , maxBlock
    , fst <$> cursor
    , fst <$> cursor
    , fst <$> cursor
    , snd <$> cursor
    , limitRows
    )

operationalWalletActionsQuerySql :: Query
operationalWalletActionsQuerySql =
  "SELECT actions.action_id, actions.tx_hash, actions.block_number, actions.block_hash, \
  \actions.tx_index, actions.log_index, actions.timestamp, actions.action_type, \
  \actions.status, actions.account, actions.actor, actions.order_id, \
  \actions.contract_address, actions.data, actions.evidence \
  \FROM protocol_actions actions \
  \WHERE actions.release_id = ? AND actions.actor = LOWER(?) \
  \AND actions.timestamp >= ? AND actions.block_number <= ? \
  \AND actions.status = 'success' \
  \AND actions.action_type IN (\
  \  'order_execution', 'order_cleanup', 'liquidation', 'keeper_maintenance', \
  \  'governance_proposal', 'governance_execution', 'governance_cancellation', \
  \  'ownership_transfer_started', 'ownership_transfer', 'pauser_update', \
  \  'pause', 'unpause', 'protocol_treasury_update', 'governance_role_change') \
  \AND (?::bigint IS NULL OR actions.block_number < ? \
  \  OR (actions.block_number = ? AND actions.log_index < ?)) \
  \ORDER BY actions.block_number DESC, actions.log_index DESC LIMIT ?"

getOperationalWalletCostsForActors
  :: Connection
  -> Text
  -> Integer
  -> Integer
  -> [Text]
  -> IO [OperationalWalletCostRow]
getOperationalWalletCostsForActors _ _ _ _ [] = pure []
getOperationalWalletCostsForActors conn releaseId sinceTimestamp maxBlock actors =
  query
    conn
    operationalWalletCostsForActorsQuerySql
    (releaseId, sinceTimestamp, maxBlock, In $ map T.toLower actors, releaseId, maxBlock)

operationalWalletCostsForActorsQuerySql :: Query
operationalWalletCostsForActorsQuerySql =
  "WITH wallet_transactions AS (\
  \SELECT DISTINCT actions.actor, actions.tx_hash FROM protocol_actions actions \
  \WHERE actions.release_id = ? AND actions.actor IS NOT NULL \
  \AND actions.timestamp >= ? AND actions.block_number <= ? \
  \AND actions.status = 'success' AND actions.actor IN ? \
  \AND actions.action_type IN (\
  \  'order_execution', 'order_cleanup', 'liquidation', 'keeper_maintenance', \
  \  'governance_proposal', 'governance_execution', 'governance_cancellation', \
  \  'ownership_transfer_started', 'ownership_transfer', 'pauser_update', \
  \  'pause', 'unpause', 'protocol_treasury_update', 'governance_role_change')), \
  \cost_samples AS (\
  \SELECT wallet_transactions.actor, transactions.gas_used, \
  \transactions.effective_gas_price, transactions.native_value, \
  \CASE WHEN transactions.gas_used IS NOT NULL \
  \  AND transactions.effective_gas_price IS NOT NULL \
  \  AND transactions.native_value IS NOT NULL \
  \THEN transactions.gas_used * transactions.effective_gas_price \
  \  + transactions.native_value ELSE NULL END AS gross_native_spend \
  \FROM wallet_transactions \
  \LEFT JOIN protocol_transactions transactions \
  \ON transactions.release_id = ? \
  \AND transactions.tx_hash = wallet_transactions.tx_hash \
  \AND transactions.block_number <= ?) \
  \SELECT actor, \
  \COALESCE(SUM(CASE WHEN gas_used IS NOT NULL AND effective_gas_price IS NOT NULL \
  \THEN gas_used * effective_gas_price ELSE 0 END), 0)::NUMERIC, \
  \COALESCE(SUM(COALESCE(native_value, 0)), 0)::NUMERIC, \
  \COUNT(gross_native_spend)::BIGINT, \
  \PERCENTILE_DISC(0.50) WITHIN GROUP (ORDER BY gross_native_spend) \
  \FILTER (WHERE gross_native_spend IS NOT NULL)::NUMERIC, \
  \COUNT(*) FILTER (WHERE gas_used IS NULL OR effective_gas_price IS NULL)::BIGINT, \
  \COUNT(*) FILTER (WHERE native_value IS NULL)::BIGINT \
  \FROM cost_samples GROUP BY actor ORDER BY actor ASC"

getTrancheActions
  :: Connection
  -> Text
  -> Text
  -> Text
  -> Integer
  -> Int
  -> Maybe (Integer, Integer)
  -> IO [ProtocolActionRow]
getTrancheActions conn releaseId vaultAddress housePoolAddress maxBlock limitRows cursor =
  query conn
    trancheActionsQuerySql
    ( releaseId
    , vaultAddress
    , housePoolAddress
    , maxBlock
    , fst <$> cursor
    , fst <$> cursor
    , fst <$> cursor
    , snd <$> cursor
    , limitRows
    )

trancheActionsQuerySql :: Query
trancheActionsQuerySql =
  "WITH feed AS (\
  \SELECT release_id, action_id, tx_hash, block_number, block_hash, tx_index, log_index, timestamp, \
  \action_type, status, account, actor, order_id, contract_address, data, evidence \
  \FROM protocol_actions \
  \UNION ALL \
  \SELECT events.release_id, events.tx_hash || ':' || events.log_index::text, \
  \events.tx_hash, events.block_number, events.block_hash, events.tx_index, \
  \events.log_index, events.timestamp, 'unclassified_event', 'unavailable', \
  \NULL, NULL, NULL, events.contract_address, \
  \jsonb_build_object(\
  \  'eventName', events.event_name, 'decodedData', events.decoded_data, \
  \  'rawTopics', events.raw_topics, 'rawData', events.raw_data), \
  \events.evidence || jsonb_build_object(\
  \  'level', 'unavailable', 'source', 'confirmed_log', \
  \  'virtualAction', true, 'reason', 'typed_action_unavailable') \
  \FROM protocol_events events \
  \WHERE NOT EXISTS (\
  \  SELECT 1 FROM protocol_actions actions \
  \  WHERE actions.release_id = events.release_id \
  \  AND actions.action_id = events.tx_hash || ':' || events.log_index::text)) \
  \SELECT action_id, tx_hash, block_number, block_hash, tx_index, log_index, timestamp, \
  \action_type, status, account, actor, order_id, contract_address, data, evidence \
  \FROM feed WHERE feed.release_id = ? \
  \AND feed.contract_address IN (LOWER(?), LOWER(?)) \
  \AND feed.block_number <= ? \
  \AND (?::bigint IS NULL OR feed.block_number < ? \
  \  OR (feed.block_number = ? AND feed.log_index < ?)) \
  \ORDER BY feed.block_number DESC, feed.log_index DESC LIMIT ?"

getProtocolStateSnapshots
  :: Connection
  -> Text
  -> Text
  -> Integer
  -> Int
  -> IO [ProtocolStateSnapshotRow]
getProtocolStateSnapshots conn releaseId scope maxBlock limitRows =
  query conn
    protocolStateSnapshotsQuerySql
    (releaseId, scope, maxBlock, limitRows)

protocolStateSnapshotsQuerySql :: Query
protocolStateSnapshotsQuerySql =
  "SELECT snapshots.snapshot_scope, snapshots.block_number, snapshots.block_hash, \
  \snapshots.timestamp, snapshots.state, snapshots.availability, snapshots.calculation_version \
  \FROM protocol_state_snapshots snapshots \
  \WHERE snapshots.release_id = ? AND snapshots.snapshot_scope = ? \
  \AND snapshots.block_number <= ? \
  \ORDER BY snapshots.block_number DESC LIMIT ?"

-- | Read one stable, newest-first page of snapshots. The cursor is the
-- exclusive block-number boundary from the final row of the previous page.
-- Snapshot block numbers are unique within a release and scope.
getProtocolStateSnapshotsPage
  :: Connection
  -> Text
  -> Text
  -> Integer
  -> Int
  -> Maybe Integer
  -> IO [ProtocolStateSnapshotRow]
getProtocolStateSnapshotsPage conn releaseId scope maxBlock limitRows cursorBlock =
  query conn
    protocolStateSnapshotsPageQuerySql
    ( releaseId
    , scope
    , maxBlock
    , cursorBlock
    , cursorBlock
    , limitRows
    )

protocolStateSnapshotsPageQuerySql :: Query
protocolStateSnapshotsPageQuerySql =
  "SELECT snapshots.snapshot_scope, snapshots.block_number, snapshots.block_hash, \
  \snapshots.timestamp, snapshots.state, snapshots.availability, snapshots.calculation_version \
  \FROM protocol_state_snapshots snapshots \
  \WHERE snapshots.release_id = ? AND snapshots.snapshot_scope = ? \
  \AND snapshots.block_number <= ? \
  \AND (?::bigint IS NULL OR snapshots.block_number < ?) \
  \ORDER BY snapshots.block_number DESC LIMIT ?"

-- | Read a companion scope at exactly the checkpoint blocks selected by a
-- bounded snapshot page. The confirmed-block cap remains authoritative even
-- if a caller supplies a future block in the requested list.
getProtocolStateSnapshotsAtBlocks
  :: Connection
  -> Text
  -> Text
  -> Integer
  -> [Integer]
  -> IO [ProtocolStateSnapshotRow]
getProtocolStateSnapshotsAtBlocks _ _ _ _ [] = pure []
getProtocolStateSnapshotsAtBlocks conn releaseId scope maxBlock blockNumbers =
  query conn
    protocolStateSnapshotsAtBlocksQuerySql
    (releaseId, scope, maxBlock, In blockNumbers)

protocolStateSnapshotsAtBlocksQuerySql :: Query
protocolStateSnapshotsAtBlocksQuerySql =
  "SELECT snapshots.snapshot_scope, snapshots.block_number, snapshots.block_hash, \
  \snapshots.timestamp, snapshots.state, snapshots.availability, snapshots.calculation_version \
  \FROM protocol_state_snapshots snapshots \
  \WHERE snapshots.release_id = ? AND snapshots.snapshot_scope = ? \
  \AND snapshots.block_number <= ? \
  \AND snapshots.block_number IN ? \
  \ORDER BY snapshots.block_number DESC"

getParameterChanges
  :: Connection
  -> Text
  -> Integer
  -> Int
  -> Maybe (Integer, Text)
  -> IO [Value]
getParameterChanges conn releaseId maxBlock limitRows cursor = do
  rows <-
    query
      conn
      parameterChangesQuerySql
      ( releaseId
      , maxBlock
      , fst <$> cursor
      , fst <$> cursor
      , fst <$> cursor
      , snd <$> cursor
      , limitRows
      )
  pure [value | Only value <- rows]

parameterChangesQuerySql :: Query
parameterChangesQuerySql =
  "SELECT jsonb_build_object(\
    \'changeId', change_id, 'parameterKey', parameter_key,\
    \'category', category_key, 'lifecycle', lifecycle,\
    \'status', status,\
    \'oldValue', old_value, 'newValue', new_value,\
    \'proposedValue', proposed_value, 'proposer', proposer,\
    \'executor', executor, 'proposedAt', proposed_at, 'eta', eta,\
    \'executedAt', executed_at, 'terminalAt', terminal_at,\
    \'txHash', tx_hash, 'blockNumber', block_number,\
    \'proposalTxHash', proposal_tx_hash,\
    \'proposalBlockNumber', proposal_block_number,\
    \'terminalTxHash', terminal_tx_hash,\
    \'terminalBlockNumber', terminal_block_number,\
    \'sourceContract', source_contract, 'sourceActionId', source_action_id,\
    \'terminalSourceActionId', terminal_source_action_id,\
    \'rawScale', raw_scale, 'unit', display_unit, 'valueType', value_type,\
    \'calculationVersion', calculation_version,\
    \'availability', availability, 'evidence', evidence) \
    \FROM protocol_parameter_changes changes \
    \WHERE changes.release_id = ? \
    \AND changes.block_number <= ? \
    \AND (?::bigint IS NULL OR changes.block_number < ? \
    \  OR (changes.block_number = ? AND changes.change_id > ?)) \
    \ORDER BY changes.block_number DESC, changes.change_id ASC LIMIT ?"
