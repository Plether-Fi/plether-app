module Plether.Database.Schema
  ( insertTransaction
  , getTransactionsByUser
  , getTransactionCount
  , getLastIndexedBlock
  , setLastIndexedBlock
  , TransactionRow (..)
  , insertPriceSnapshot
  , getPriceAt
  , insertStakingSnapshot
  , getStakingRatesAt
  , ensureBasketSnapshotSchema
  , insertBasketSnapshot
  , insertBasketSnapshotWithSource
  , insertBasketSnapshotsWithSource
  , getBasketSnapshots
  , getBasketSnapshotTimes
  , getLatestBasketSnapshot
  , getLatestBasketSnapshotTime
  , BasketHistorySnapshotRow (..)
  , BasketSnapshotRow (..)
  , insertPythUpdatePayload
  , getPythUpdatePayloadForWindow
  , getLatestPythUpdatePayload
  , getLatestPythUpdatePayloadAtOrAfter
  , PythUpdatePayloadRow (..)
  , promotePythPayloadSource
  , isAdmittedPythPayloadSource
  , isHistoricalRevealPayload
  , isHistoricalRevealPayloadSource
  , ensurePerpsKeeperSchema
  , verifyLpSettlementSchema
  , verifyNoLegacySubmittedLpSettlementAttempts
  , tryPerpsKeeperLock
  , unlockPerpsKeeperLock
  , tryLpSettlementKeeperLock
  , unlockLpSettlementKeeperLock
  , getPerpsKeeperLastIndexedBlock
  , setPerpsKeeperLastIndexedBlock
  , upsertPerpsKeeperOrderCommitted
  , markPerpsKeeperOrderExecuted
  , markPerpsKeeperOrderFailed
  , reconcilePerpsKeeperOrderExecuted
  , reconcilePerpsKeeperOrderFailed
  , recordPerpsKeeperOrderAttempt
  , recordPerpsKeeperOrderError
  , recordPerpsKeeperOrderImmediateRetryError
  , getPendingPerpsKeeperOrders
  , PerpsKeeperOrderRow (..)
  , PerpsKeeperTerminalOrderRow (..)
  , getPerpsKeeperOrderById
  , LpSettlementAttemptRow (..)
  , recordLpSettlementObservation
  , markLpSettlementAttemptStatus
  , markLpSettlementAttemptSubmitted
  , getSubmittedLpSettlementAttempts
  , LpSettlementObservationInput (..)
  , recordLpSettlementObservationV2
  , LpSettlementSignedIntent (..)
  , LpSettlementTransactionRow (..)
  , prepareLpSettlementTransaction
  , replaceLpSettlementTransaction
  , getActiveLpSettlementTransaction
  , getLpSettlementTransactionById
  , getLpSettlementTransactionFamily
  , getLpSettlementObservationObservedBlock
  , getLatestSuccessfulLpSettlementAt
  , LpSettlementBroadcastInput (..)
  , LpSettlementBroadcastRow (..)
  , appendLpSettlementBroadcast
  , getLpSettlementBroadcasts
  , markLpSettlementTransactionPending
  , markLpSettlementTransactionConfirming
  , clearLpSettlementReorgedReceiptEvidence
  , markLpSettlementTransactionManualReview
  , LpSettlementEventOutcome (..)
  , LpSettlementReceiptInput (..)
  , recordLpSettlementReceipt
  , recordLpSettlementReceiptForManualReview
  , recordLpSettlementSupersededReceipt
  , ensurePerpsLiquidationSchema
  , tryPerpsLiquidationLock
  , unlockPerpsLiquidationLock
  , getPerpsLiquidationLastIndexedBlock
  , setPerpsLiquidationLastIndexedBlock
  , getPerpsLiquidationRejectedPayload
  , recordPerpsLiquidationRejectedPayload
  , clearPerpsLiquidationRejectedPayload
  , getPerpsLiquidationSignerRetry
  , recordPerpsLiquidationSignerRetry
  , clearPerpsLiquidationSignerRetry
  , upsertPerpsLiquidationCandidate
  , seedPerpsLiquidationCandidatesFromHistory
  , getPerpsLiquidationCandidates
  , getPendingPerpsLiquidationCandidates
  , markPerpsLiquidationCandidateChecked
  , recordPerpsLiquidationCandidatePending
  , recordPerpsLiquidationCandidateBroadcastAttempt
  , clearPerpsLiquidationCandidatePending
  , recordPerpsLiquidationCandidateError
  , recordPerpsLiquidationCandidateRetryableError
  , deletePerpsLiquidationCandidate
  , PerpsLiquidationCandidateRow (..)
  , PerpsLiquidationRejectedPayloadRow (..)
  , PerpsLiquidationSignerRetryRow (..)
  , ensurePerpsHistorySchema
  , ensureTestnetFaucetSchema
  , TestnetFaucetClaimRow (..)
  , getTestnetFaucetClaim
  , beginTestnetFaucetClaim
  , beginTestnetFaucetClaimSql
  , markTestnetFaucetClaimSubmitted
  , markTestnetFaucetClaimSubmittedSql
  , markTestnetFaucetClaimSuccess
  , markTestnetFaucetClaimSuccessSql
  , markTestnetFaucetClaimReconciled
  , markTestnetFaucetClaimFailed
  , markTestnetFaucetClaimReverted
  , markTestnetFaucetClaimRevertedSql
  , PerpsOrderRow (..)
  , PerpsExecutionEvidenceRow (..)
  , PerpsActivityRow (..)
  , PerpsIndexerStatusRow (..)
  , insertPerpsEvent
  , upsertPerpsOrderCommitted
  , upsertPerpsOrderTerminal
  , getPendingPerpsExecutionEvidence
  , pendingPerpsExecutionEvidenceSql
  , perpsExecutionEvidenceLaneLimits
  , markPerpsExecutionEvidenceAttempt
  , updatePerpsOrderOracleEvidence
  , updatePerpsOrderEconomicsEvidence
  , updatePerpsOrderLifecycleIdentity
  , updatePerpsOrderLifecycleReceipt
  , updatePerpsOrderLifecycleReceiptSql
  , executionModeOracleFrozen
  , insertPerpsActivity
  , insertPerpsUsdcTransfer
  , perpsOrderBaseSelectSql
  , getPerpsOrdersByAccount
  , getPerpsOrderById
  , getPerpsActivityByAccount
  , PerpsMarketVolumeBucketRow (..)
  , getPerpsMarketVolumeBuckets
  , getPerpsMarketVolumeSince
  , getPerpsOrderAccountSide
  , insertPerpsExpiredCleanupActivityIfReady
  , getPerpsIndexerStatus
  , getPerpsIndexerLastBlock
  , PerpsReplayHistorySnapshot (..)
  , getPerpsReplayHistorySnapshot
  , lockPerpsReplayOrders
  , assertPerpsReplayEventExact
  , assertPerpsReplayOrderCommittedExact
  , assertPerpsReplayOrderTerminalExact
  , assertPerpsReplayActivityExact
  , assertPerpsReplayUsdcTransferExact
  , assertPerpsReplayExpiredCleanupExact
  , assertPerpsReplayExpiredCleanupIfReadyExact
  , configurePerpsReplayTransaction
  , lockPerpsIndexerTransaction
  , setPerpsIndexerState
  , setPerpsIndexerStateWithTimestamp
  , deletePerpsHistoryFromBlock
  ) where

import Control.Monad (unless, when)
import Data.Aeson (Value, encode, object, (.=))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Scientific (Scientific, base10Exponent, coefficient)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Int (Int64)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple
  ( Connection
  , In (..)
  , Only (..)
  , Query
  , execute
  , executeMany
  , execute_
  , query
  , query_
  , withTransaction
  )
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import Database.PostgreSQL.Simple.Internal (RowParser)
import Database.PostgreSQL.Simple.ToField (ToField (..))
import Database.PostgreSQL.Simple.ToRow (ToRow (..))
import Database.PostgreSQL.Simple.Types (Binary (..))
import GHC.Generics (Generic)

data TransactionRow = TransactionRow
  { trId :: Integer
  , trTxHash :: Text
  , trBlockNumber :: Integer
  , trTimestamp :: Integer
  , trUserAddress :: Text
  , trTxType :: Text
  , trSide :: Maybe Text
  , trStatus :: Text
  , trData :: Value
  }
  deriving stock (Show, Generic)

instance FromRow TransactionRow where
  fromRow = TransactionRow
    <$> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field

data TestnetFaucetClaimRow = TestnetFaucetClaimRow
  { tfcAddress :: Text
  , tfcAmount :: Integer
  , tfcTokenAddress :: Text
  , tfcTxHash :: Maybe Text
  , tfcRawTx :: Maybe Text
  , tfcMintBlockNumber :: Maybe Integer
  , tfcStatus :: Text
  , tfcError :: Maybe Text
  }
  deriving stock (Show, Generic)

instance FromRow TestnetFaucetClaimRow where
  fromRow = TestnetFaucetClaimRow
    <$> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field

ensureTestnetFaucetSchema :: Connection -> IO ()
ensureTestnetFaucetSchema conn = do
  _ <- execute_ conn
    "CREATE TABLE IF NOT EXISTS testnet_faucet_claims (\
    \address VARCHAR(42) NOT NULL,\
    \amount BIGINT NOT NULL,\
    \token_address VARCHAR(42) NOT NULL,\
    \tx_hash VARCHAR(66),\
    \raw_tx TEXT,\
    \mint_block_number BIGINT,\
    \status VARCHAR(16) NOT NULL,\
    \error TEXT,\
    \created_at TIMESTAMP DEFAULT NOW(),\
    \updated_at TIMESTAMP DEFAULT NOW(),\
    \PRIMARY KEY (address, token_address)\
    \)"
  _ <- execute_ conn "ALTER TABLE testnet_faucet_claims ADD COLUMN IF NOT EXISTS raw_tx TEXT"
  _ <- execute_ conn "ALTER TABLE testnet_faucet_claims ADD COLUMN IF NOT EXISTS mint_block_number BIGINT"
  _ <- execute_ conn "ALTER TABLE testnet_faucet_claims DROP CONSTRAINT IF EXISTS testnet_faucet_claims_pkey"
  _ <- execute_ conn
    "ALTER TABLE testnet_faucet_claims \
    \ADD CONSTRAINT testnet_faucet_claims_pkey PRIMARY KEY (address, token_address)"
  _ <- execute_ conn
    "CREATE INDEX IF NOT EXISTS idx_testnet_faucet_claims_status \
    \ON testnet_faucet_claims(status)"
  pure ()

getTestnetFaucetClaim :: Connection -> Text -> Text -> IO (Maybe TestnetFaucetClaimRow)
getTestnetFaucetClaim conn address tokenAddress = do
  rows <- query conn
    "SELECT address, amount, token_address, tx_hash, raw_tx, mint_block_number, status, error \
    \FROM testnet_faucet_claims WHERE address = ? AND token_address = ?"
    (T.toLower address, T.toLower tokenAddress)
  pure $ case rows of
    [row] -> Just row
    _ -> Nothing

beginTestnetFaucetClaim :: Connection -> Text -> Integer -> Text -> IO Bool
beginTestnetFaucetClaim conn address amount tokenAddress = do
  affected <- execute conn
    beginTestnetFaucetClaimSql
    (T.toLower address, amount, T.toLower tokenAddress)
  pure $ affected > (0 :: Int64)

beginTestnetFaucetClaimSql :: Query
beginTestnetFaucetClaimSql =
  "INSERT INTO testnet_faucet_claims \
  \(address, amount, token_address, tx_hash, raw_tx, status, error, updated_at) \
  \VALUES (?, ?, ?, NULL, NULL, 'preparing', NULL, NOW()) \
  \ON CONFLICT (address, token_address) DO UPDATE SET \
  \amount = EXCLUDED.amount,\
  \tx_hash = NULL,\
  \raw_tx = NULL,\
  \mint_block_number = NULL,\
  \status = 'preparing',\
  \error = NULL,\
  \updated_at = NOW() \
  \WHERE testnet_faucet_claims.status = 'failed' \
  \OR (testnet_faucet_claims.status = 'preparing' \
  \AND testnet_faucet_claims.updated_at <= NOW() - INTERVAL '5 minutes')"

markTestnetFaucetClaimSubmitted :: Connection -> Text -> Text -> Text -> Text -> IO Bool
markTestnetFaucetClaimSubmitted conn address tokenAddress txHash rawTx = do
  affected <- execute conn
    markTestnetFaucetClaimSubmittedSql
    (T.toLower txHash, T.toLower rawTx, T.toLower address, T.toLower tokenAddress)
  pure $ affected > (0 :: Int64)

markTestnetFaucetClaimSubmittedSql :: Query
markTestnetFaucetClaimSubmittedSql =
  "UPDATE testnet_faucet_claims SET \
  \tx_hash = ?, raw_tx = ?, status = 'submitted', error = NULL, updated_at = NOW() \
  \WHERE address = ? AND token_address = ? AND status = 'preparing'"

markTestnetFaucetClaimSuccess :: Connection -> Text -> Text -> Text -> Integer -> IO Bool
markTestnetFaucetClaimSuccess conn address tokenAddress txHash mintBlockNumber = do
  affected <- execute conn markTestnetFaucetClaimSuccessSql
    (T.toLower txHash, mintBlockNumber, T.toLower address, T.toLower tokenAddress, T.toLower txHash)
  pure $ affected > (0 :: Int64)

markTestnetFaucetClaimSuccessSql :: Query
markTestnetFaucetClaimSuccessSql =
  "UPDATE testnet_faucet_claims SET \
  \tx_hash = ?, raw_tx = NULL, mint_block_number = ?, status = 'success', error = NULL, updated_at = NOW() \
  \WHERE address = ? AND token_address = ? AND tx_hash = ? \
  \AND status IN ('submitted', 'success')"

markTestnetFaucetClaimReconciled :: Connection -> Text -> Text -> IO ()
markTestnetFaucetClaimReconciled conn address tokenAddress = do
  _ <- execute conn
    "UPDATE testnet_faucet_claims SET \
    \tx_hash = NULL, raw_tx = NULL, status = 'success', error = NULL, updated_at = NOW() \
    \WHERE address = ? AND token_address = ? AND status = 'pending'"
    (T.toLower address, T.toLower tokenAddress)
  pure ()

markTestnetFaucetClaimFailed :: Connection -> Text -> Text -> Text -> IO ()
markTestnetFaucetClaimFailed conn address tokenAddress err = do
  _ <- execute conn
    "UPDATE testnet_faucet_claims SET \
    \status = 'failed', error = ?, updated_at = NOW() \
    \WHERE address = ? AND token_address = ? AND status = 'preparing'"
    (err, T.toLower address, T.toLower tokenAddress)
  pure ()

markTestnetFaucetClaimReverted :: Connection -> Text -> Text -> Text -> Text -> IO Bool
markTestnetFaucetClaimReverted conn address tokenAddress txHash err = do
  affected <- execute conn markTestnetFaucetClaimRevertedSql
    (err, T.toLower address, T.toLower tokenAddress, T.toLower txHash)
  pure $ affected > (0 :: Int64)

markTestnetFaucetClaimRevertedSql :: Query
markTestnetFaucetClaimRevertedSql =
  "UPDATE testnet_faucet_claims SET \
  \raw_tx = NULL, mint_block_number = NULL, status = 'failed', error = ?, updated_at = NOW() \
  \WHERE address = ? AND token_address = ? AND tx_hash = ? AND status = 'submitted'"

data InsertRow = InsertRow
  { irTxHash :: Text
  , irBlockNumber :: Integer
  , irTimestamp :: Integer
  , irUserAddress :: Text
  , irTxType :: Text
  , irSide :: Maybe Text
  , irStatus :: Text
  , irData :: LBS.ByteString
  }

instance ToRow InsertRow where
  toRow InsertRow {..} =
    [ toField irTxHash
    , toField irBlockNumber
    , toField irTimestamp
    , toField (T.toLower irUserAddress)
    , toField irTxType
    , toField irSide
    , toField irStatus
    , toField irData
    ]

insertTransaction
  :: Connection
  -> Text       -- tx_hash
  -> Integer    -- block_number
  -> Integer    -- timestamp
  -> Text       -- user_address
  -> Text       -- tx_type
  -> Maybe Text -- side
  -> Text       -- status
  -> Value      -- data
  -> IO ()
insertTransaction conn txHash blockNum timestamp userAddr txType side status txData = do
  let row = InsertRow
        { irTxHash = txHash
        , irBlockNumber = blockNum
        , irTimestamp = timestamp
        , irUserAddress = userAddr
        , irTxType = txType
        , irSide = side
        , irStatus = status
        , irData = encode txData
        }
  _ <- execute conn insertQuery row
  pure ()
  where
    insertQuery :: Query
    insertQuery = "INSERT INTO transactions \
      \(tx_hash, block_number, timestamp, user_address, tx_type, side, status, data) \
      \VALUES (?, ?, ?, ?, ?, ?, ?, ?) \
      \ON CONFLICT (tx_hash, tx_type) DO NOTHING"

getTransactionsByUser
  :: Connection
  -> Text        -- user_address
  -> Maybe Text  -- tx_type filter
  -> Maybe Text  -- side filter
  -> [Text]      -- tx_types filter (for leverage/lending views)
  -> Int         -- limit
  -> Int         -- offset
  -> IO [TransactionRow]
getTransactionsByUser conn userAddr mTxType mSide txTypes limit offset = do
  let addr = T.toLower userAddr
  case (mTxType, mSide, txTypes) of
    (Nothing, Nothing, []) ->
      query conn baseQuery (addr, limit, offset)
    (Just txType, Nothing, []) ->
      query conn typeQuery (addr, txType, limit, offset)
    (Nothing, Just side, []) ->
      query conn sideQuery (addr, side, limit, offset)
    (Just txType, Just side, []) ->
      query conn typeSideQuery (addr, txType, side, limit, offset)
    (_, _, types) | not (null types) ->
      query conn (typesQuery $ length types) (toRow (Only addr) ++ map toField types ++ [toField limit, toField offset])
    _ ->
      query conn baseQuery (addr, limit, offset)
  where
    baseQuery :: Query
    baseQuery = "SELECT id, tx_hash, block_number, timestamp, user_address, \
      \tx_type, side, status, data FROM transactions \
      \WHERE user_address = ? ORDER BY block_number DESC LIMIT ? OFFSET ?"

    typeQuery :: Query
    typeQuery = "SELECT id, tx_hash, block_number, timestamp, user_address, \
      \tx_type, side, status, data FROM transactions \
      \WHERE user_address = ? AND tx_type = ? ORDER BY block_number DESC LIMIT ? OFFSET ?"

    sideQuery :: Query
    sideQuery = "SELECT id, tx_hash, block_number, timestamp, user_address, \
      \tx_type, side, status, data FROM transactions \
      \WHERE user_address = ? AND side = ? ORDER BY block_number DESC LIMIT ? OFFSET ?"

    typeSideQuery :: Query
    typeSideQuery = "SELECT id, tx_hash, block_number, timestamp, user_address, \
      \tx_type, side, status, data FROM transactions \
      \WHERE user_address = ? AND tx_type = ? AND side = ? ORDER BY block_number DESC LIMIT ? OFFSET ?"

    typesQuery :: Int -> Query
    typesQuery n =
      let placeholders = T.intercalate "," (replicate n "?")
      in fromString $ T.unpack $ "SELECT id, tx_hash, block_number, timestamp, user_address, \
        \tx_type, side, status, data FROM transactions \
        \WHERE user_address = ? AND tx_type IN (" <> placeholders <> ") ORDER BY block_number DESC LIMIT ? OFFSET ?"

    fromString :: String -> Query
    fromString s = read $ show s

getTransactionCount
  :: Connection
  -> Text        -- user_address
  -> Maybe Text  -- tx_type filter
  -> Maybe Text  -- side filter
  -> [Text]      -- tx_types filter
  -> IO Int
getTransactionCount conn userAddr mTxType mSide txTypes = do
  let addr = T.toLower userAddr
  result <- case (mTxType, mSide, txTypes) of
    (Nothing, Nothing, []) ->
      query conn baseCountQuery (Only addr) :: IO [Only Int]
    (Just txType, Nothing, []) ->
      query conn typeCountQuery (addr, txType) :: IO [Only Int]
    (Nothing, Just side, []) ->
      query conn sideCountQuery (addr, side) :: IO [Only Int]
    (Just txType, Just side, []) ->
      query conn typeSideCountQuery (addr, txType, side) :: IO [Only Int]
    (_, _, types) | not (null types) ->
      query conn (typesCountQuery $ length types) (toRow (Only addr) ++ map toField types)
    _ ->
      query conn baseCountQuery (Only addr) :: IO [Only Int]
  case result of
    [Only count] -> pure count
    _ -> pure 0
  where
    baseCountQuery :: Query
    baseCountQuery = "SELECT COUNT(*) FROM transactions WHERE user_address = ?"

    typeCountQuery :: Query
    typeCountQuery = "SELECT COUNT(*) FROM transactions WHERE user_address = ? AND tx_type = ?"

    sideCountQuery :: Query
    sideCountQuery = "SELECT COUNT(*) FROM transactions WHERE user_address = ? AND side = ?"

    typeSideCountQuery :: Query
    typeSideCountQuery = "SELECT COUNT(*) FROM transactions WHERE user_address = ? AND tx_type = ? AND side = ?"

    typesCountQuery :: Int -> Query
    typesCountQuery n =
      let placeholders = T.intercalate "," (replicate n "?")
      in fromString $ T.unpack $ "SELECT COUNT(*) FROM transactions WHERE user_address = ? AND tx_type IN (" <> placeholders <> ")"

    fromString :: String -> Query
    fromString s = read $ show s

getLastIndexedBlock :: Connection -> IO Integer
getLastIndexedBlock conn = do
  result <- query_ conn "SELECT last_indexed_block FROM indexer_state WHERE id = 1" :: IO [Only Integer]
  case result of
    [Only block] -> pure block
    _ -> pure 0

setLastIndexedBlock :: Connection -> Integer -> IO ()
setLastIndexedBlock conn block = do
  _ <- execute conn "UPDATE indexer_state SET last_indexed_block = ?, updated_at = NOW() WHERE id = 1" (Only block)
  pure ()

insertPriceSnapshot
  :: Connection
  -> Integer    -- block_number
  -> Integer    -- timestamp
  -> Integer    -- oracle_price
  -> IO ()
insertPriceSnapshot conn blockNum timestamp oraclePrice' = do
  _ <- execute conn
    "INSERT INTO price_snapshots (block_number, timestamp, oracle_price) \
    \VALUES (?, ?, ?) ON CONFLICT (block_number) DO NOTHING"
    (blockNum, timestamp, oraclePrice')
  pure ()

getPriceAt :: Connection -> Integer -> IO (Maybe Integer)
getPriceAt conn timestamp = do
  result <- query conn
    "SELECT oracle_price FROM price_snapshots \
    \WHERE timestamp <= ? ORDER BY timestamp DESC LIMIT 1"
    (Only timestamp) :: IO [Only Integer]
  case result of
    [Only p] -> pure $ Just p
    _ -> pure Nothing

insertStakingSnapshot
  :: Connection
  -> Integer    -- block_number
  -> Integer    -- timestamp
  -> Integer    -- bear_exchange_rate
  -> Integer    -- bull_exchange_rate
  -> IO ()
insertStakingSnapshot conn blockNum timestamp bearRate bullRate = do
  _ <- execute conn
    "INSERT INTO staking_snapshots (block_number, timestamp, bear_exchange_rate, bull_exchange_rate) \
    \VALUES (?, ?, ?, ?) ON CONFLICT (block_number) DO NOTHING"
    (blockNum, timestamp, bearRate, bullRate)
  pure ()

getStakingRatesAt :: Connection -> Integer -> IO (Maybe (Integer, Integer))
getStakingRatesAt conn timestamp = do
  result <- query conn
    "SELECT bear_exchange_rate, bull_exchange_rate FROM staking_snapshots \
    \WHERE timestamp <= ? ORDER BY timestamp DESC LIMIT 1"
    (Only timestamp) :: IO [(Integer, Integer)]
  case result of
    [(bear, bull)] -> pure $ Just (bear, bull)
    _ -> pure Nothing

data BasketSnapshotRow = BasketSnapshotRow
  { bsrTimestamp :: Integer
  , bsrIntervalSeconds :: Integer
  , bsrBasketPrice :: Integer
  , bsrComponents :: Value
  }
  deriving stock (Show, Generic)

instance FromRow BasketSnapshotRow where
  fromRow = BasketSnapshotRow
    <$> field
    <*> field
    <*> field
    <*> field

data BasketHistorySnapshotRow = BasketHistorySnapshotRow
  { bhsrTimestamp :: Integer
  , bhsrIntervalSeconds :: Integer
  , bhsrBasketPrice :: Integer
  , bhsrComponents :: Maybe Value
  }
  deriving stock (Show, Generic)

instance FromRow BasketHistorySnapshotRow where
  fromRow = BasketHistorySnapshotRow
    <$> field
    <*> field
    <*> field
    <*> field

ensureBasketSnapshotSchema :: Connection -> IO ()
ensureBasketSnapshotSchema conn = do
  _ <- execute_ conn
    "CREATE TABLE IF NOT EXISTS perps_basket_snapshots (\
    \id SERIAL PRIMARY KEY,\
    \timestamp BIGINT NOT NULL,\
    \interval_seconds INTEGER NOT NULL,\
    \basket_price BIGINT NOT NULL,\
    \component_prices JSONB NOT NULL,\
    \source VARCHAR(32) NOT NULL DEFAULT 'pyth_benchmarks',\
    \created_at TIMESTAMP DEFAULT NOW(),\
    \UNIQUE (timestamp, interval_seconds)\
    \)"
  _ <- execute_ conn
    "CREATE INDEX IF NOT EXISTS idx_perps_basket_snapshots_timestamp \
    \ON perps_basket_snapshots(timestamp DESC)"
  _ <- execute_ conn
    "CREATE INDEX IF NOT EXISTS idx_perps_basket_snapshots_interval_timestamp \
    \ON perps_basket_snapshots(interval_seconds, timestamp ASC)"
  _ <- execute_ conn
    "CREATE TABLE IF NOT EXISTS perps_pyth_update_payloads (\
    \id SERIAL PRIMARY KEY,\
    \min_publish_time BIGINT NOT NULL,\
    \max_publish_time BIGINT NOT NULL,\
    \publish_times JSONB NOT NULL,\
    \update_data JSONB NOT NULL,\
    \source VARCHAR(32) NOT NULL DEFAULT 'backend_hermes',\
    \fetched_at BIGINT NOT NULL,\
    \created_at TIMESTAMP DEFAULT NOW(),\
    \UNIQUE (min_publish_time, max_publish_time)\
    \)"
  _ <- execute_ conn
    "CREATE INDEX IF NOT EXISTS idx_perps_pyth_update_payloads_window \
    \ON perps_pyth_update_payloads(min_publish_time, max_publish_time)"
  _ <- execute_ conn
    "CREATE INDEX IF NOT EXISTS idx_perps_pyth_update_payloads_admitted_latest \
    \ON perps_pyth_update_payloads(max_publish_time DESC) \
    \WHERE source = 'backend_hermes_latest_v2'"
  pure ()

insertBasketSnapshot
  :: Connection
  -> Integer -- timestamp
  -> Integer -- interval_seconds
  -> Integer -- basket_price
  -> Value   -- component_prices
  -> IO ()
insertBasketSnapshot conn timestamp intervalSeconds basketPrice components = do
  insertBasketSnapshotWithSource conn timestamp intervalSeconds basketPrice components "pyth_benchmarks"

insertBasketSnapshotWithSource
  :: Connection
  -> Integer -- timestamp
  -> Integer -- interval_seconds
  -> Integer -- basket_price
  -> Value   -- component_prices
  -> Text    -- source
  -> IO ()
insertBasketSnapshotWithSource conn timestamp intervalSeconds basketPrice components source = do
  insertBasketSnapshotsWithSource
    conn
    [(timestamp, intervalSeconds, basketPrice, components)]
    source

-- | Persist a bounded endpoint response with one prepared batch operation.
-- The upsert policy is identical to the single-row writer, including source
-- priority, so historical bulk ingestion cannot overwrite a stronger live
-- observation for the same minute.
insertBasketSnapshotsWithSource
  :: Connection
  -> [(Integer, Integer, Integer, Value)]
  -> Text
  -> IO ()
insertBasketSnapshotsWithSource _ [] _ = pure ()
insertBasketSnapshotsWithSource conn snapshots source = do
  _ <- executeMany conn
    "INSERT INTO perps_basket_snapshots \
    \(timestamp, interval_seconds, basket_price, component_prices, source) \
    \VALUES (?, ?, ?, ?, ?) \
    \ON CONFLICT (timestamp, interval_seconds) DO UPDATE SET \
    \basket_price = EXCLUDED.basket_price, \
    \component_prices = EXCLUDED.component_prices, \
    \source = EXCLUDED.source \
    \WHERE (CASE EXCLUDED.source \
    \  WHEN 'backend_hermes_latest_v2' THEN 30 \
    \  WHEN 'pyth_hermes_latest' THEN 30 \
    \  WHEN 'backend_hermes_historical_v2' THEN 20 \
    \  WHEN 'backend_hermes_reveal_v2' THEN 20 \
    \  ELSE 10 END) >= \
    \ (CASE perps_basket_snapshots.source \
    \  WHEN 'backend_hermes_latest_v2' THEN 30 \
    \  WHEN 'pyth_hermes_latest' THEN 30 \
    \  WHEN 'backend_hermes_historical_v2' THEN 20 \
    \  WHEN 'backend_hermes_reveal_v2' THEN 20 \
    \  ELSE 10 END)"
    [ (timestamp, intervalSeconds, basketPrice, encode components, source)
    | (timestamp, intervalSeconds, basketPrice, components) <- snapshots
    ]
  pure ()

getBasketSnapshots
  :: Connection
  -> Integer -- from timestamp
  -> Integer -- to timestamp
  -> Integer -- interval seconds
  -> Int     -- limit
  -> Bool    -- include component prices
  -> IO [BasketHistorySnapshotRow]
getBasketSnapshots conn fromTimestamp toTimestamp intervalSeconds limit includeComponents = do
  if includeComponents
    then query conn
      "WITH selected_interval AS (\
      \  SELECT COALESCE(\
      \    (SELECT interval_seconds FROM perps_basket_snapshots \
      \     WHERE interval_seconds = ? AND timestamp >= ? AND timestamp <= ? LIMIT 1),\
      \    (SELECT MIN(interval_seconds) FROM perps_basket_snapshots \
      \     WHERE timestamp >= ? AND timestamp <= ?)\
      \  ) AS interval_seconds\
      \) \
      \SELECT timestamp, interval_seconds, basket_price, component_prices \
      \FROM (\
      \  SELECT DISTINCT ON (bucket) \
      \    timestamp, interval_seconds, basket_price, component_prices \
      \  FROM (\
      \    SELECT perps_basket_snapshots.timestamp, perps_basket_snapshots.interval_seconds, basket_price, component_prices, perps_basket_snapshots.timestamp / ? AS bucket \
      \    FROM perps_basket_snapshots \
      \    CROSS JOIN selected_interval \
      \    WHERE perps_basket_snapshots.timestamp >= ? AND perps_basket_snapshots.timestamp <= ? \
      \      AND perps_basket_snapshots.interval_seconds = selected_interval.interval_seconds \
      \  ) candidates \
      \  ORDER BY bucket, timestamp DESC\
      \) sampled \
      \ORDER BY timestamp ASC LIMIT ?"
      (intervalSeconds, fromTimestamp, toTimestamp, fromTimestamp, toTimestamp, intervalSeconds, fromTimestamp, toTimestamp, limit)
    else query conn
      "WITH selected_interval AS (\
      \  SELECT COALESCE(\
      \    (SELECT interval_seconds FROM perps_basket_snapshots \
      \     WHERE interval_seconds = ? AND timestamp >= ? AND timestamp <= ? LIMIT 1),\
      \    (SELECT MIN(interval_seconds) FROM perps_basket_snapshots \
      \     WHERE timestamp >= ? AND timestamp <= ?)\
      \  ) AS interval_seconds\
      \) \
      \SELECT timestamp, interval_seconds, basket_price, NULL::jsonb AS component_prices \
      \FROM (\
      \  SELECT DISTINCT ON (bucket) \
      \    timestamp, interval_seconds, basket_price \
      \  FROM (\
      \    SELECT perps_basket_snapshots.timestamp, perps_basket_snapshots.interval_seconds, basket_price, perps_basket_snapshots.timestamp / ? AS bucket \
      \    FROM perps_basket_snapshots \
      \    CROSS JOIN selected_interval \
      \    WHERE perps_basket_snapshots.timestamp >= ? AND perps_basket_snapshots.timestamp <= ? \
      \      AND perps_basket_snapshots.interval_seconds = selected_interval.interval_seconds \
      \  ) candidates \
      \  ORDER BY bucket, timestamp DESC\
      \) sampled \
      \ORDER BY timestamp ASC LIMIT ?"
      (intervalSeconds, fromTimestamp, toTimestamp, fromTimestamp, toTimestamp, intervalSeconds, fromTimestamp, toTimestamp, limit)

getBasketSnapshotTimes
  :: Connection
  -> Integer -- from timestamp
  -> Integer -- to timestamp
  -> Integer -- interval seconds
  -> IO [Integer]
getBasketSnapshotTimes conn fromTimestamp toTimestamp intervalSeconds = do
  rows <-
    query conn
      "SELECT timestamp FROM perps_basket_snapshots \
      \WHERE timestamp >= ? AND timestamp <= ? AND interval_seconds = ? \
      \ORDER BY timestamp ASC"
      (fromTimestamp, toTimestamp, intervalSeconds)
      :: IO [Only Integer]
  pure [timestamp | Only timestamp <- rows]

getLatestBasketSnapshot :: Connection -> IO (Maybe BasketSnapshotRow)
getLatestBasketSnapshot conn = do
  rows <- query_ conn
    "SELECT timestamp, interval_seconds, basket_price, component_prices \
    \FROM perps_basket_snapshots \
    \ORDER BY timestamp DESC LIMIT 1"
  case rows of
    [row] -> pure $ Just row
    _ -> pure Nothing

getLatestBasketSnapshotTime :: Connection -> Integer -> IO (Maybe Integer)
getLatestBasketSnapshotTime conn intervalSeconds = do
  result <- query conn
    "SELECT timestamp FROM perps_basket_snapshots \
    \WHERE interval_seconds = ? \
    \ORDER BY timestamp DESC LIMIT 1"
    (Only intervalSeconds)
    :: IO [Only Integer]
  case result of
    [Only timestamp] -> pure $ Just timestamp
    _ -> pure Nothing

data PythUpdatePayloadRow = PythUpdatePayloadRow
  { puprMinPublishTime :: Integer
  , puprMaxPublishTime :: Integer
  , puprPublishTimes :: Value
  , puprUpdateData :: Value
  , puprFetchedAt :: Integer
  , puprSource :: Text
  }
  deriving stock (Show, Generic)

instance FromRow PythUpdatePayloadRow where
  fromRow = PythUpdatePayloadRow
    <$> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field

isHistoricalRevealPayload :: PythUpdatePayloadRow -> Bool
isHistoricalRevealPayload =
  isHistoricalRevealPayloadSource . puprSource

isHistoricalRevealPayloadSource :: Text -> Bool
isHistoricalRevealPayloadSource source =
  source
    `elem` [ "backend_hermes_historical_v2"
           , "backend_hermes_reveal_v2"
           ]

-- Only source-v2 rows have passed the upgraded Pyth contract's payable parser.
-- Keeping that admission version in the persisted source makes pre-deployment
-- Hermes rows fail closed without deleting potentially useful audit data.
promotePythPayloadSource :: Text -> Maybe Text
promotePythPayloadSource source =
  case source of
    "backend_hermes_latest" -> Just "backend_hermes_latest_v2"
    "backend_hermes_historical" -> Just "backend_hermes_historical_v2"
    "backend_hermes_reveal_backfill" -> Just "backend_hermes_reveal_v2"
    _ -> Nothing

isAdmittedPythPayloadSource :: Text -> Bool
isAdmittedPythPayloadSource source =
  source
    `elem` [ "backend_hermes_latest_v2"
           , "backend_hermes_historical_v2"
           , "backend_hermes_reveal_v2"
           ]

insertPythUpdatePayload
  :: Connection
  -> Integer -- min publish time
  -> Integer -- max publish time
  -> Value   -- publish_times
  -> Value   -- update_data
  -> Integer -- fetched_at
  -> Text    -- source
  -> IO ()
insertPythUpdatePayload _ _ _ _ _ _ source
  | not (isAdmittedPythPayloadSource source) =
      fail $ "refusing to persist a Pyth payload without on-chain-admitted source v2: " <> T.unpack source
insertPythUpdatePayload conn minPublishTime maxPublishTime publishTimes updateData fetchedAt source = do
  _ <- execute conn
    "INSERT INTO perps_pyth_update_payloads \
    \(min_publish_time, max_publish_time, publish_times, update_data, source, fetched_at) \
    \VALUES (?, ?, ?, ?, ?, ?) \
    \ON CONFLICT (min_publish_time, max_publish_time) DO UPDATE SET \
    \publish_times = EXCLUDED.publish_times, \
    \update_data = EXCLUDED.update_data, \
    \source = EXCLUDED.source, \
    \fetched_at = EXCLUDED.fetched_at \
    \WHERE perps_pyth_update_payloads.source NOT IN ('backend_hermes_historical_v2', 'backend_hermes_reveal_v2') \
    \OR EXCLUDED.source IN ('backend_hermes_historical_v2', 'backend_hermes_reveal_v2')"
    (minPublishTime, maxPublishTime, encode publishTimes, encode updateData, source, fetchedAt)
  pure ()

getPythUpdatePayloadForWindow
  :: Connection
  -> Integer -- min publish time
  -> Integer -- max publish time
  -> IO (Maybe PythUpdatePayloadRow)
getPythUpdatePayloadForWindow conn minPublishTime maxPublishTime = do
  rows <- query conn
    "SELECT min_publish_time, max_publish_time, publish_times, update_data, fetched_at, source \
    \FROM perps_pyth_update_payloads \
    \WHERE min_publish_time = ? AND max_publish_time <= ? \
    \AND source IN ('backend_hermes_historical_v2', 'backend_hermes_reveal_v2') \
    \ORDER BY min_publish_time ASC LIMIT 1"
    (minPublishTime, maxPublishTime)
  case rows of
    [row] -> pure $ Just row
    _ -> pure Nothing

getLatestPythUpdatePayload :: Connection -> IO (Maybe PythUpdatePayloadRow)
getLatestPythUpdatePayload conn = do
  rows <- query_ conn
    "SELECT min_publish_time, max_publish_time, publish_times, update_data, fetched_at, source \
    \FROM perps_pyth_update_payloads \
    \WHERE source = 'backend_hermes_latest_v2' \
    \ORDER BY max_publish_time DESC LIMIT 1"
  case rows of
    [row] -> pure $ Just row
    _ -> pure Nothing

getLatestPythUpdatePayloadAtOrAfter
  :: Connection
  -> Integer
  -> IO (Maybe PythUpdatePayloadRow)
getLatestPythUpdatePayloadAtOrAfter conn minimumPublishTime = do
  rows <- query conn
    "SELECT min_publish_time, max_publish_time, publish_times, update_data, fetched_at, source \
    \FROM perps_pyth_update_payloads \
    \WHERE source = 'backend_hermes_latest_v2' AND min_publish_time >= ? \
    \ORDER BY max_publish_time DESC LIMIT 1"
    (Only minimumPublishTime)
  case rows of
    [row] -> pure $ Just row
    _ -> pure Nothing

data PerpsKeeperOrderRow = PerpsKeeperOrderRow
  { pkorOrderId :: Integer
  , pkorOrderRouter :: Text
  , pkorAccount :: Text
  , pkorSide :: Integer
  , pkorCommitBlock :: Integer
  , pkorCommitTime :: Integer
  , pkorCommitTxHash :: Text
  , pkorStatus :: Text
  , pkorAttemptCount :: Int
  , pkorLastError :: Maybe Text
  }
  deriving stock (Show, Generic)

instance FromRow PerpsKeeperOrderRow where
  fromRow =
    PerpsKeeperOrderRow
      <$> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field

data PerpsKeeperTerminalOrderRow = PerpsKeeperTerminalOrderRow
  { pktoOrderId :: Integer
  , pktoOrderRouter :: Text
  , pktoAccount :: Text
  , pktoSide :: Integer
  , pktoCommitBlock :: Integer
  , pktoCommitEventBlock :: Maybe Integer
  , pktoCommitTime :: Integer
  , pktoCommitTxHash :: Text
  , pktoStatus :: Text
  , pktoExecutionTxHash :: Maybe Text
  , pktoExecutionBlock :: Maybe Integer
  , pktoExecutionPrice :: Maybe Integer
  , pktoFailureTxHash :: Maybe Text
  , pktoFailureBlock :: Maybe Integer
  , pktoFailureReason :: Maybe Int
  }
  deriving stock (Show, Generic)

instance FromRow PerpsKeeperTerminalOrderRow where
  fromRow =
    PerpsKeeperTerminalOrderRow
      <$> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> numericIntegerField
      <*> field
      <*> field
      <*> field

ensurePerpsKeeperSchema :: Connection -> IO ()
ensurePerpsKeeperSchema conn = do
  _ <- execute_ conn
    "CREATE TABLE IF NOT EXISTS perps_keeper_state (\
    \id INTEGER PRIMARY KEY DEFAULT 1,\
    \order_router TEXT,\
    \last_indexed_block BIGINT NOT NULL,\
    \updated_at TIMESTAMP DEFAULT NOW(),\
    \CONSTRAINT perps_keeper_state_single_row CHECK (id = 1)\
    \)"
  _ <- execute_ conn
    "ALTER TABLE perps_keeper_state ADD COLUMN IF NOT EXISTS order_router TEXT"
  _ <- execute_ conn
    "UPDATE perps_keeper_state SET order_router = '0x0000000000000000000000000000000000000000' WHERE order_router IS NULL"
  _ <- execute_ conn
    "DO $$ \
    \BEGIN \
    \  IF EXISTS (SELECT 1 FROM information_schema.columns WHERE table_schema = current_schema() AND table_name = 'perps_keeper_state' AND column_name = 'order_router' AND is_nullable = 'YES') THEN \
    \    ALTER TABLE perps_keeper_state ALTER COLUMN order_router SET NOT NULL; \
    \  END IF; \
    \END $$"
  _ <- execute_ conn
    "ALTER TABLE perps_keeper_state DROP CONSTRAINT IF EXISTS perps_keeper_state_single_row"
  _ <- execute_ conn
    "DO $$ \
    \DECLARE pk_cols text[]; \
    \BEGIN \
    \  SELECT COALESCE(array_agg(a.attname ORDER BY cols.ordinality), ARRAY[]::text[]) INTO pk_cols \
    \  FROM pg_constraint c \
    \  JOIN pg_class t ON t.oid = c.conrelid \
    \  JOIN pg_namespace n ON n.oid = t.relnamespace \
    \  JOIN unnest(c.conkey) WITH ORDINALITY AS cols(attnum, ordinality) ON TRUE \
    \  JOIN pg_attribute a ON a.attrelid = t.oid AND a.attnum = cols.attnum \
    \  WHERE c.contype = 'p' AND n.nspname = current_schema() AND t.relname = 'perps_keeper_state'; \
    \  IF pk_cols <> ARRAY['order_router']::text[] THEN \
    \    ALTER TABLE perps_keeper_state DROP CONSTRAINT IF EXISTS perps_keeper_state_pkey; \
    \    ALTER TABLE perps_keeper_state ADD CONSTRAINT perps_keeper_state_pkey PRIMARY KEY (order_router); \
    \  END IF; \
    \END $$"
  _ <- execute_ conn
    "INSERT INTO perps_keeper_state (id, order_router, last_indexed_block) \
    \VALUES (1, '0x0000000000000000000000000000000000000000', 0) ON CONFLICT (order_router) DO NOTHING"
  _ <- execute_ conn
    "CREATE TABLE IF NOT EXISTS perps_keeper_orders (\
    \order_id BIGINT NOT NULL,\
    \order_router TEXT,\
    \account VARCHAR(42) NOT NULL,\
    \side INTEGER NOT NULL,\
    \commit_block BIGINT NOT NULL,\
    \commit_event_block BIGINT,\
    \commit_time BIGINT NOT NULL,\
    \commit_tx_hash VARCHAR(66) NOT NULL,\
    \status VARCHAR(16) NOT NULL DEFAULT 'pending',\
    \execution_tx_hash VARCHAR(66),\
    \execution_block BIGINT,\
    \execution_price NUMERIC(78,0),\
    \failure_tx_hash VARCHAR(66),\
    \failure_block BIGINT,\
    \failure_reason INTEGER,\
    \attempt_count INTEGER NOT NULL DEFAULT 0,\
    \last_error TEXT,\
    \last_attempt_at TIMESTAMP,\
    \created_at TIMESTAMP DEFAULT NOW(),\
    \updated_at TIMESTAMP DEFAULT NOW(),\
    \PRIMARY KEY (order_router, order_id)\
    \)"
  _ <- execute_ conn
    "ALTER TABLE perps_keeper_orders ADD COLUMN IF NOT EXISTS order_router TEXT"
  _ <- execute_ conn
    "UPDATE perps_keeper_orders SET order_router = '0x0000000000000000000000000000000000000000' WHERE order_router IS NULL"
  _ <- execute_ conn
    "DO $$ \
    \BEGIN \
    \  IF EXISTS (SELECT 1 FROM information_schema.columns WHERE table_schema = current_schema() AND table_name = 'perps_keeper_orders' AND column_name = 'order_router' AND is_nullable = 'YES') THEN \
    \    ALTER TABLE perps_keeper_orders ALTER COLUMN order_router SET NOT NULL; \
    \  END IF; \
    \END $$"
  _ <- execute_ conn
    "DO $$ \
    \BEGIN \
    \  IF EXISTS (SELECT 1 FROM information_schema.columns WHERE table_schema = current_schema() AND table_name = 'perps_keeper_orders' AND column_name = 'order_id' AND data_type <> 'bigint') THEN \
    \    ALTER TABLE perps_keeper_orders ALTER COLUMN order_id TYPE BIGINT USING order_id::bigint; \
    \  END IF; \
    \END $$"
  _ <- execute_ conn
    "DO $$ \
    \DECLARE pk_cols text[]; \
    \BEGIN \
    \  SELECT COALESCE(array_agg(a.attname ORDER BY cols.ordinality), ARRAY[]::text[]) INTO pk_cols \
    \  FROM pg_constraint c \
    \  JOIN pg_class t ON t.oid = c.conrelid \
    \  JOIN pg_namespace n ON n.oid = t.relnamespace \
    \  JOIN unnest(c.conkey) WITH ORDINALITY AS cols(attnum, ordinality) ON TRUE \
    \  JOIN pg_attribute a ON a.attrelid = t.oid AND a.attnum = cols.attnum \
    \  WHERE c.contype = 'p' AND n.nspname = current_schema() AND t.relname = 'perps_keeper_orders'; \
    \  IF pk_cols <> ARRAY['order_router', 'order_id']::text[] THEN \
    \    ALTER TABLE perps_keeper_orders DROP CONSTRAINT IF EXISTS perps_keeper_orders_pkey; \
    \    ALTER TABLE perps_keeper_orders ADD CONSTRAINT perps_keeper_orders_pkey PRIMARY KEY (order_router, order_id); \
    \  END IF; \
    \END $$"
  _ <- execute_ conn
    "ALTER TABLE perps_keeper_orders \
    \ADD COLUMN IF NOT EXISTS commit_event_block BIGINT"
  _ <- execute_ conn
    "CREATE INDEX IF NOT EXISTS idx_perps_keeper_orders_pending \
    \ON perps_keeper_orders(order_router, order_id ASC) WHERE status = 'pending'"
  _ <- execute_ conn
    "CREATE INDEX IF NOT EXISTS idx_perps_keeper_orders_commit_block \
    \ON perps_keeper_orders(commit_block DESC)"
  _ <- execute_ conn
    "CREATE TABLE IF NOT EXISTS perps_lp_settlement_attempts (\
    \chain_id BIGINT NOT NULL,\
    \monitor_address TEXT NOT NULL,\
    \observation_digest VARCHAR(66) NOT NULL,\
    \epoch BIGINT NOT NULL,\
    \observed_block BIGINT NOT NULL,\
    \execution_path INTEGER NOT NULL,\
    \operational_blocker_mask TEXT NOT NULL,\
    \warning_mask TEXT NOT NULL,\
    \dependency_failure_mask TEXT NOT NULL,\
    \critical_fault_mask TEXT NOT NULL,\
    \transaction_hash VARCHAR(66),\
    \status VARCHAR(24) NOT NULL,\
    \last_error TEXT,\
    \created_at TIMESTAMP DEFAULT NOW(),\
    \updated_at TIMESTAMP DEFAULT NOW(),\
    \PRIMARY KEY (chain_id, monitor_address, observation_digest)\
    \)"
  _ <- execute_ conn
    "CREATE INDEX IF NOT EXISTS idx_perps_lp_settlement_submitted \
    \ON perps_lp_settlement_attempts(chain_id, monitor_address, updated_at) \
    \WHERE status = 'submitted'"
  _ <- execute_ conn
    "CREATE TABLE IF NOT EXISTS perps_lp_settlement_observations (\
    \chain_id BIGINT NOT NULL CHECK (chain_id > 0),\
    \monitor_address VARCHAR(42) NOT NULL CHECK (monitor_address ~ '^0x[0-9a-f]{40}$'),\
    \observation_digest VARCHAR(66) NOT NULL CHECK (observation_digest ~ '^0x[0-9a-f]{64}$'),\
    \epoch BIGINT NOT NULL CHECK (epoch >= 0),\
    \observed_block BIGINT NOT NULL CHECK (observed_block >= 0),\
    \observed_block_hash VARCHAR(66) CHECK (observed_block_hash IS NULL OR observed_block_hash ~ '^0x[0-9a-f]{64}$'),\
    \execution_path INTEGER NOT NULL CHECK (execution_path >= 0),\
    \operational_blocker_mask NUMERIC(78,0) NOT NULL CHECK (operational_blocker_mask >= 0),\
    \warning_mask NUMERIC(78,0) NOT NULL CHECK (warning_mask >= 0),\
    \dependency_failure_mask NUMERIC(78,0) NOT NULL CHECK (dependency_failure_mask >= 0),\
    \critical_fault_mask NUMERIC(78,0) NOT NULL CHECK (critical_fault_mask >= 0),\
    \schema_version NUMERIC(78,0) CHECK (schema_version IS NULL OR schema_version >= 0),\
    \health_state NUMERIC(78,0) CHECK (health_state IS NULL OR health_state >= 0),\
    \execution_path_dependency_mask NUMERIC(78,0) CHECK (execution_path_dependency_mask IS NULL OR execution_path_dependency_mask >= 0),\
    \status_dependency_failure_mask NUMERIC(78,0) CHECK (status_dependency_failure_mask IS NULL OR status_dependency_failure_mask >= 0),\
    \health_dependency_failure_mask NUMERIC(78,0) CHECK (health_dependency_failure_mask IS NULL OR health_dependency_failure_mask >= 0),\
    \observation_complete BOOLEAN,\
    \has_matured_work BOOLEAN,\
    \lp_epoch_settlement_paused BOOLEAN,\
    \first_observed_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),\
    \last_observed_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),\
    \PRIMARY KEY (chain_id, monitor_address, observation_digest),\
    \UNIQUE (chain_id, monitor_address, observation_digest, epoch)\
    \)"
  -- Legacy rows contain enough information to retain observation history, but
  -- not enough to fabricate a signed transaction intent. Only canonical,
  -- parseable observations are copied; the legacy table remains untouched.
  _ <- execute_ conn
    "WITH canonical_legacy AS MATERIALIZED (\
    \ SELECT chain_id, lower(trim(monitor_address)) AS monitor_address, \
    \ lower(trim(observation_digest)) AS observation_digest, epoch, observed_block, execution_path, \
    \ CASE WHEN trim(operational_blocker_mask) ~ '^[0-9]+$' AND length(trim(operational_blocker_mask)) <= 78 THEN trim(operational_blocker_mask)::NUMERIC END AS operational_blocker_mask, \
    \ CASE WHEN trim(warning_mask) ~ '^[0-9]+$' AND length(trim(warning_mask)) <= 78 THEN trim(warning_mask)::NUMERIC END AS warning_mask, \
    \ CASE WHEN trim(dependency_failure_mask) ~ '^[0-9]+$' AND length(trim(dependency_failure_mask)) <= 78 THEN trim(dependency_failure_mask)::NUMERIC END AS dependency_failure_mask, \
    \ CASE WHEN trim(critical_fault_mask) ~ '^[0-9]+$' AND length(trim(critical_fault_mask)) <= 78 THEN trim(critical_fault_mask)::NUMERIC END AS critical_fault_mask, \
    \ created_at AT TIME ZONE 'UTC' AS first_observed_at, \
    \ updated_at AT TIME ZONE 'UTC' AS last_observed_at \
    \ FROM perps_lp_settlement_attempts \
    \ WHERE chain_id > 0 AND epoch >= 0 AND observed_block >= 0 AND execution_path >= 0 \
    \ AND lower(trim(monitor_address)) ~ '^0x[0-9a-f]{40}$' \
    \ AND lower(trim(observation_digest)) ~ '^0x[0-9a-f]{64}$' \
    \ AND trim(operational_blocker_mask) ~ '^[0-9]+$' AND length(trim(operational_blocker_mask)) <= 78 \
    \ AND trim(warning_mask) ~ '^[0-9]+$' AND length(trim(warning_mask)) <= 78 \
    \ AND trim(dependency_failure_mask) ~ '^[0-9]+$' AND length(trim(dependency_failure_mask)) <= 78 \
    \ AND trim(critical_fault_mask) ~ '^[0-9]+$' AND length(trim(critical_fault_mask)) <= 78\
    \), unambiguous AS (\
    \ SELECT chain_id, monitor_address, observation_digest, MIN(epoch) AS epoch, \
    \ MIN(observed_block) AS observed_block, MIN(execution_path) AS execution_path, \
    \ MIN(operational_blocker_mask) AS operational_blocker_mask, MIN(warning_mask) AS warning_mask, \
    \ MIN(dependency_failure_mask) AS dependency_failure_mask, MIN(critical_fault_mask) AS critical_fault_mask, \
    \ COALESCE(MIN(first_observed_at), NOW()) AS first_observed_at, \
    \ COALESCE(MAX(last_observed_at), NOW()) AS last_observed_at \
    \ FROM canonical_legacy GROUP BY chain_id, monitor_address, observation_digest \
    \ HAVING COUNT(DISTINCT (epoch, observed_block, execution_path, operational_blocker_mask, warning_mask, dependency_failure_mask, critical_fault_mask)) = 1\
    \) INSERT INTO perps_lp_settlement_observations \
    \(chain_id, monitor_address, observation_digest, epoch, observed_block, execution_path, \
    \ operational_blocker_mask, warning_mask, dependency_failure_mask, critical_fault_mask, \
    \ first_observed_at, last_observed_at) \
    \SELECT chain_id, monitor_address, observation_digest, epoch, observed_block, execution_path, \
    \ operational_blocker_mask, warning_mask, dependency_failure_mask, critical_fault_mask, \
    \ first_observed_at, last_observed_at FROM unambiguous \
    \ON CONFLICT (chain_id, monitor_address, observation_digest) DO NOTHING"
  _ <- execute_ conn
    "CREATE OR REPLACE FUNCTION protect_lp_settlement_observation_identity() RETURNS trigger AS $$ \
    \BEGIN \
    \IF TG_OP = 'DELETE' THEN RAISE EXCEPTION 'perps_lp_settlement_observations is append-only'; END IF; \
    \IF ROW(OLD.chain_id, OLD.monitor_address, OLD.observation_digest, OLD.epoch, OLD.observed_block, OLD.execution_path, OLD.operational_blocker_mask, OLD.warning_mask, OLD.dependency_failure_mask, OLD.critical_fault_mask, OLD.first_observed_at) \
    \ IS DISTINCT FROM ROW(NEW.chain_id, NEW.monitor_address, NEW.observation_digest, NEW.epoch, NEW.observed_block, NEW.execution_path, NEW.operational_blocker_mask, NEW.warning_mask, NEW.dependency_failure_mask, NEW.critical_fault_mask, NEW.first_observed_at) \
    \THEN RAISE EXCEPTION 'LP settlement observation identity is immutable'; END IF; \
    \IF (OLD.observed_block_hash IS NOT NULL AND OLD.observed_block_hash IS DISTINCT FROM NEW.observed_block_hash) \
    \ OR (OLD.schema_version IS NOT NULL AND OLD.schema_version IS DISTINCT FROM NEW.schema_version) \
    \ OR (OLD.health_state IS NOT NULL AND OLD.health_state IS DISTINCT FROM NEW.health_state) \
    \ OR (OLD.execution_path_dependency_mask IS NOT NULL AND OLD.execution_path_dependency_mask IS DISTINCT FROM NEW.execution_path_dependency_mask) \
    \ OR (OLD.status_dependency_failure_mask IS NOT NULL AND OLD.status_dependency_failure_mask IS DISTINCT FROM NEW.status_dependency_failure_mask) \
    \ OR (OLD.health_dependency_failure_mask IS NOT NULL AND OLD.health_dependency_failure_mask IS DISTINCT FROM NEW.health_dependency_failure_mask) \
    \ OR (OLD.observation_complete IS NOT NULL AND OLD.observation_complete IS DISTINCT FROM NEW.observation_complete) \
    \ OR (OLD.has_matured_work IS NOT NULL AND OLD.has_matured_work IS DISTINCT FROM NEW.has_matured_work) \
    \ OR (OLD.lp_epoch_settlement_paused IS NOT NULL AND OLD.lp_epoch_settlement_paused IS DISTINCT FROM NEW.lp_epoch_settlement_paused) \
    \THEN RAISE EXCEPTION 'LP settlement observation detail is immutable once known'; END IF; \
    \RETURN NEW; END; \
    \$$ LANGUAGE plpgsql"
  _ <- execute_ conn
    "DO $$ BEGIN \
    \IF NOT EXISTS (SELECT 1 FROM pg_trigger WHERE tgname = 'trg_perps_lp_settlement_observation_identity' AND tgrelid = 'perps_lp_settlement_observations'::regclass) THEN \
    \BEGIN CREATE TRIGGER trg_perps_lp_settlement_observation_identity BEFORE UPDATE OR DELETE ON perps_lp_settlement_observations FOR EACH ROW EXECUTE FUNCTION protect_lp_settlement_observation_identity(); \
    \EXCEPTION WHEN duplicate_object THEN NULL; END; \
    \END IF; END $$"
  _ <- execute_ conn
    "CREATE TABLE IF NOT EXISTS perps_lp_settlement_transactions (\
    \id BIGSERIAL PRIMARY KEY,\
    \chain_id BIGINT NOT NULL,\
    \monitor_address VARCHAR(42) NOT NULL,\
    \observation_digest VARCHAR(66) NOT NULL,\
    \epoch BIGINT NOT NULL CHECK (epoch >= 0),\
    \replacement_count INTEGER NOT NULL DEFAULT 0 CHECK (replacement_count >= 0),\
    \replaces_attempt_id BIGINT REFERENCES perps_lp_settlement_transactions(id) ON DELETE RESTRICT,\
    \signer_address VARCHAR(42) NOT NULL CHECK (signer_address ~ '^0x[0-9a-f]{40}$'),\
    \tx_nonce NUMERIC(78,0) NOT NULL CHECK (tx_nonce >= 0),\
    \target_address VARCHAR(42) NOT NULL CHECK (target_address ~ '^0x[0-9a-f]{40}$'),\
    \tx_value NUMERIC(78,0) NOT NULL CHECK (tx_value >= 0),\
    \calldata BYTEA NOT NULL CHECK (octet_length(calldata) >= 4),\
    \gas_limit NUMERIC(78,0) NOT NULL CHECK (gas_limit > 0),\
    \max_priority_fee_per_gas NUMERIC(78,0) NOT NULL CHECK (max_priority_fee_per_gas >= 0),\
    \max_fee_per_gas NUMERIC(78,0) NOT NULL CHECK (max_fee_per_gas >= max_priority_fee_per_gas),\
    \signed_raw_transaction BYTEA NOT NULL CHECK (octet_length(signed_raw_transaction) > 0),\
    \signed_transaction_hash VARCHAR(66) NOT NULL UNIQUE CHECK (signed_transaction_hash ~ '^0x[0-9a-f]{64}$'),\
    \status VARCHAR(32) NOT NULL CHECK (status IN ('prepared', 'broadcast', 'pending', 'confirming', 'manual_review', 'replaced', 'confirmed_success', 'confirmed_revert', 'failed', 'abandoned', 'superseded')),\
    \last_error TEXT,\
    \receipt_transaction_hash VARCHAR(66) CHECK (receipt_transaction_hash IS NULL OR receipt_transaction_hash ~ '^0x[0-9a-f]{64}$'),\
    \receipt_block_number BIGINT CHECK (receipt_block_number IS NULL OR receipt_block_number >= 0),\
    \receipt_block_hash VARCHAR(66) CHECK (receipt_block_hash IS NULL OR receipt_block_hash ~ '^0x[0-9a-f]{64}$'),\
    \receipt_succeeded BOOLEAN,\
    \confirmed_at TIMESTAMPTZ,\
    \confirmation_depth INTEGER CHECK (confirmation_depth IS NULL OR confirmation_depth >= 0),\
    \settlement_event_log_index BIGINT CHECK (settlement_event_log_index IS NULL OR settlement_event_log_index >= 0),\
    \cutoff_epoch NUMERIC(78,0) CHECK (cutoff_epoch IS NULL OR cutoff_epoch >= 0),\
    \senior_redeem_assets NUMERIC(78,0) CHECK (senior_redeem_assets IS NULL OR senior_redeem_assets >= 0),\
    \junior_redeem_assets NUMERIC(78,0) CHECK (junior_redeem_assets IS NULL OR junior_redeem_assets >= 0),\
    \junior_deposit_assets NUMERIC(78,0) CHECK (junior_deposit_assets IS NULL OR junior_deposit_assets >= 0),\
    \senior_deposit_assets NUMERIC(78,0) CHECK (senior_deposit_assets IS NULL OR senior_deposit_assets >= 0),\
    \senior_backlog BOOLEAN,\
    \junior_backlog BOOLEAN,\
    \entries_deferred BOOLEAN,\
    \created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),\
    \updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),\
    \FOREIGN KEY (chain_id, monitor_address, observation_digest, epoch) REFERENCES perps_lp_settlement_observations(chain_id, monitor_address, observation_digest, epoch) ON DELETE RESTRICT,\
    \UNIQUE (replaces_attempt_id),\
    \UNIQUE (chain_id, signer_address, tx_nonce, replacement_count),\
    \CHECK ((replacement_count = 0 AND replaces_attempt_id IS NULL) OR (replacement_count > 0 AND replaces_attempt_id IS NOT NULL)),\
    \CHECK ((receipt_transaction_hash IS NULL AND receipt_block_number IS NULL AND receipt_block_hash IS NULL AND receipt_succeeded IS NULL) OR (receipt_transaction_hash IS NOT NULL AND receipt_block_number IS NOT NULL AND receipt_block_hash IS NOT NULL AND receipt_succeeded IS NOT NULL)),\
    \CHECK ((settlement_event_log_index IS NULL AND cutoff_epoch IS NULL AND senior_redeem_assets IS NULL AND junior_redeem_assets IS NULL AND junior_deposit_assets IS NULL AND senior_deposit_assets IS NULL AND senior_backlog IS NULL AND junior_backlog IS NULL AND entries_deferred IS NULL) OR (settlement_event_log_index IS NOT NULL AND cutoff_epoch IS NOT NULL AND senior_redeem_assets IS NOT NULL AND junior_redeem_assets IS NOT NULL AND junior_deposit_assets IS NOT NULL AND senior_deposit_assets IS NOT NULL AND senior_backlog IS NOT NULL AND junior_backlog IS NOT NULL AND entries_deferred IS NOT NULL)),\
    \CONSTRAINT perps_lp_settlement_confirmation_state_check CHECK ((status IN ('confirmed_success', 'confirmed_revert') AND confirmed_at IS NOT NULL AND confirmation_depth IS NOT NULL) OR (status = 'superseded' AND (confirmed_at IS NULL OR (receipt_transaction_hash IS NOT NULL AND confirmation_depth IS NOT NULL))) OR (status NOT IN ('confirmed_success', 'confirmed_revert', 'superseded') AND confirmed_at IS NULL)),\
    \CHECK (status <> 'confirmed_success' OR (receipt_succeeded IS TRUE AND settlement_event_log_index IS NOT NULL)),\
    \CHECK (status <> 'confirmed_revert' OR (receipt_succeeded IS FALSE AND settlement_event_log_index IS NULL)),\
    \CONSTRAINT perps_lp_settlement_terminal_receipt_identity_check CHECK ((status NOT IN ('confirmed_success', 'confirmed_revert') AND (status <> 'superseded' OR confirmed_at IS NULL)) OR receipt_transaction_hash = signed_transaction_hash),\
    \CONSTRAINT perps_lp_settlement_superseded_receipt_check CHECK (status <> 'superseded' OR confirmed_at IS NULL OR (receipt_transaction_hash IS NOT NULL AND receipt_succeeded IS FALSE AND settlement_event_log_index IS NULL AND confirmation_depth IS NOT NULL)),\
    \CONSTRAINT perps_lp_settlement_success_epoch_check CHECK (status <> 'confirmed_success' OR cutoff_epoch = epoch),\
    \CHECK (status <> 'confirming' OR (receipt_transaction_hash IS NOT NULL AND confirmed_at IS NULL AND settlement_event_log_index IS NULL)),\
    \CHECK (status NOT IN ('prepared', 'broadcast', 'pending') OR (receipt_transaction_hash IS NULL AND settlement_event_log_index IS NULL))\
    \)"
  -- Converge pre-release transaction tables on the complete status domain.
  -- A single-column status check with a narrower set can otherwise make fee
  -- replacement impossible even when a second canonical check is installed.
  _ <- execute_ conn
    "DO $$ DECLARE old_constraint record; BEGIN \
    \FOR old_constraint IN SELECT k.conname FROM pg_constraint k WHERE k.conrelid = 'perps_lp_settlement_transactions'::regclass AND k.contype = 'c' \
    \ AND k.conkey = ARRAY[(SELECT a.attnum FROM pg_attribute a WHERE a.attrelid = k.conrelid AND a.attname = 'status')] \
    \ AND encode(sha256(convert_to(regexp_replace(lower(trim(pg_get_constraintdef(k.oid, true))), E'\\\\s+', ' ', 'g'), 'UTF8')), 'hex') <> '2bbd439a6b83336279526a7f336eb14aab3934929267a056211c35760228cbfa' \
    \ LOOP EXECUTE format('ALTER TABLE perps_lp_settlement_transactions DROP CONSTRAINT %I', old_constraint.conname); END LOOP; \
    \IF NOT EXISTS (SELECT 1 FROM pg_constraint k WHERE k.conrelid = 'perps_lp_settlement_transactions'::regclass AND k.contype = 'c' AND k.convalidated \
    \ AND encode(sha256(convert_to(regexp_replace(lower(trim(pg_get_constraintdef(k.oid, true))), E'\\\\s+', ' ', 'g'), 'UTF8')), 'hex') = '2bbd439a6b83336279526a7f336eb14aab3934929267a056211c35760228cbfa') THEN \
    \ ALTER TABLE perps_lp_settlement_transactions ADD CONSTRAINT perps_lp_settlement_status_domain_check CHECK (status IN ('prepared', 'broadcast', 'pending', 'confirming', 'manual_review', 'replaced', 'confirmed_success', 'confirmed_revert', 'failed', 'abandoned', 'superseded')); \
    \END IF; END $$"
  -- Some pre-release databases created the table before signer/nonce lineage
  -- and the one-successor rule were constrained. Match exact ordered UNIQUE
  -- definitions rather than accepting an index/constraint with extra keys.
  _ <- execute_ conn
    "DO $$ DECLARE legacy_object record; BEGIN \
    \FOR legacy_object IN SELECT k.conname FROM pg_constraint k WHERE k.conrelid = 'perps_lp_settlement_transactions'::regclass AND k.contype = 'u' \
    \ AND k.conkey = ARRAY[ \
    \  (SELECT a.attnum FROM pg_attribute a WHERE a.attrelid = k.conrelid AND a.attname = 'chain_id'), \
    \  (SELECT a.attnum FROM pg_attribute a WHERE a.attrelid = k.conrelid AND a.attname = 'signer_address'), \
    \  (SELECT a.attnum FROM pg_attribute a WHERE a.attrelid = k.conrelid AND a.attname = 'tx_nonce')] \
    \ LOOP EXECUTE format('ALTER TABLE perps_lp_settlement_transactions DROP CONSTRAINT %I', legacy_object.conname); END LOOP; \
    \FOR legacy_object IN SELECT idx.relname AS index_name FROM pg_index i JOIN pg_class idx ON idx.oid = i.indexrelid JOIN pg_class tbl ON tbl.oid = i.indrelid JOIN pg_namespace n ON n.oid = tbl.relnamespace \
    \ LEFT JOIN pg_constraint k ON k.conindid = i.indexrelid WHERE n.nspname = current_schema() AND tbl.relname = 'perps_lp_settlement_transactions' AND k.oid IS NULL \
    \ AND i.indisunique AND i.indpred IS NULL AND i.indnatts = 3 AND i.indnkeyatts = 3 \
    \ AND pg_get_indexdef(i.indexrelid, 1, true) = 'chain_id' AND pg_get_indexdef(i.indexrelid, 2, true) = 'signer_address' AND pg_get_indexdef(i.indexrelid, 3, true) = 'tx_nonce' \
    \ LOOP EXECUTE format('DROP INDEX %I', legacy_object.index_name); END LOOP; \
    \IF NOT EXISTS (SELECT 1 FROM pg_constraint k WHERE k.conrelid = 'perps_lp_settlement_transactions'::regclass AND k.contype = 'u' AND k.convalidated \
    \ AND encode(sha256(convert_to(regexp_replace(lower(trim(pg_get_constraintdef(k.oid, true))), E'\\\\s+', ' ', 'g'), 'UTF8')), 'hex') = '4515d727d3995c3c2022d5ae8f7e8259e765718cd09d23db00ad64cc85a04b6f') THEN \
    \ ALTER TABLE perps_lp_settlement_transactions DROP CONSTRAINT IF EXISTS perps_lp_settlement_nonce_replacement_unique; \
    \ ALTER TABLE perps_lp_settlement_transactions ADD CONSTRAINT perps_lp_settlement_nonce_replacement_unique UNIQUE (chain_id, signer_address, tx_nonce, replacement_count); \
    \END IF; END $$"
  _ <- execute_ conn
    "DO $$ BEGIN \
    \IF NOT EXISTS (SELECT 1 FROM pg_constraint k WHERE k.conrelid = 'perps_lp_settlement_transactions'::regclass AND k.contype = 'u' AND k.convalidated \
    \ AND encode(sha256(convert_to(regexp_replace(lower(trim(pg_get_constraintdef(k.oid, true))), E'\\\\s+', ' ', 'g'), 'UTF8')), 'hex') = '74a720f18e3e0f47c41f7ab05c2b192f666d94ea418ad50b135aaa77570ec055') THEN \
    \ ALTER TABLE perps_lp_settlement_transactions DROP CONSTRAINT IF EXISTS perps_lp_settlement_replaces_attempt_unique; \
    \ ALTER TABLE perps_lp_settlement_transactions ADD CONSTRAINT perps_lp_settlement_replaces_attempt_unique UNIQUE (replaces_attempt_id); \
    \END IF; END $$"
  -- Install the successful receipt/intent epoch binding for pre-release
  -- transaction tables as well as newly created ones.
  _ <- execute_ conn
    "DO $$ BEGIN \
    \IF NOT EXISTS (SELECT 1 FROM pg_constraint k WHERE k.conrelid = 'perps_lp_settlement_transactions'::regclass \
    \ AND k.conname = 'perps_lp_settlement_success_epoch_check' AND k.contype = 'c' AND k.convalidated \
    \ AND encode(sha256(convert_to(regexp_replace(lower(trim(pg_get_constraintdef(k.oid, true))), E'\\\\s+', ' ', 'g'), 'UTF8')), 'hex') = '17853a3381c45cdd3a9bb4d0d4afc7722437c8aa09eb0e44ef9cb92eb26be9fa') THEN \
    \ ALTER TABLE perps_lp_settlement_transactions DROP CONSTRAINT IF EXISTS perps_lp_settlement_success_epoch_check; \
    \ ALTER TABLE perps_lp_settlement_transactions ADD CONSTRAINT perps_lp_settlement_success_epoch_check CHECK (status <> 'confirmed_success' OR cutoff_epoch = epoch); \
    \END IF; END $$"
  -- Replace the pre-release confirmation checks whose non-terminal branch
  -- prohibited receipt-backed benign supersession.
  _ <- execute_ conn
    "DO $$ DECLARE old_constraint record; BEGIN \
    \FOR old_constraint IN SELECT conname FROM pg_constraint WHERE conrelid = 'perps_lp_settlement_transactions'::regclass AND contype = 'c' \
    \ AND conname <> 'perps_lp_settlement_confirmation_state_check' \
    \ AND pg_get_constraintdef(oid) ILIKE '%confirmed_at%' AND pg_get_constraintdef(oid) ILIKE '%confirmation_depth%' \
    \ AND pg_get_constraintdef(oid) ILIKE '%confirmed_success%' AND pg_get_constraintdef(oid) ILIKE '%confirmed_revert%' \
    \LOOP EXECUTE format('ALTER TABLE perps_lp_settlement_transactions DROP CONSTRAINT %I', old_constraint.conname); END LOOP; \
    \IF NOT EXISTS (SELECT 1 FROM pg_constraint k WHERE k.conrelid = 'perps_lp_settlement_transactions'::regclass AND k.conname = 'perps_lp_settlement_confirmation_state_check' AND k.contype = 'c' AND k.convalidated \
    \ AND encode(sha256(convert_to(regexp_replace(lower(trim(pg_get_constraintdef(k.oid, true))), E'\\\\s+', ' ', 'g'), 'UTF8')), 'hex') = '10a4e0fa933c2201f0e5525531dc709a69c33a7f041b08f299ea9db78a73a777') THEN \
    \ ALTER TABLE perps_lp_settlement_transactions DROP CONSTRAINT IF EXISTS perps_lp_settlement_confirmation_state_check; \
    \ ALTER TABLE perps_lp_settlement_transactions ADD CONSTRAINT perps_lp_settlement_confirmation_state_check CHECK ((status IN ('confirmed_success', 'confirmed_revert') AND confirmed_at IS NOT NULL AND confirmation_depth IS NOT NULL) OR (status = 'superseded' AND (confirmed_at IS NULL OR (receipt_transaction_hash IS NOT NULL AND confirmation_depth IS NOT NULL))) OR (status NOT IN ('confirmed_success', 'confirmed_revert', 'superseded') AND confirmed_at IS NULL)); \
    \END IF; END $$"
  _ <- execute_ conn
    "DO $$ DECLARE old_constraint record; BEGIN \
    \FOR old_constraint IN SELECT conname FROM pg_constraint WHERE conrelid = 'perps_lp_settlement_transactions'::regclass AND contype = 'c' \
    \ AND conname <> 'perps_lp_settlement_terminal_receipt_identity_check' \
    \ AND pg_get_constraintdef(oid) ILIKE '%receipt_transaction_hash%' AND pg_get_constraintdef(oid) ILIKE '%signed_transaction_hash%' \
    \LOOP EXECUTE format('ALTER TABLE perps_lp_settlement_transactions DROP CONSTRAINT %I', old_constraint.conname); END LOOP; \
    \IF NOT EXISTS (SELECT 1 FROM pg_constraint k WHERE k.conrelid = 'perps_lp_settlement_transactions'::regclass AND k.conname = 'perps_lp_settlement_terminal_receipt_identity_check' AND k.contype = 'c' AND k.convalidated \
    \ AND encode(sha256(convert_to(regexp_replace(lower(trim(pg_get_constraintdef(k.oid, true))), E'\\\\s+', ' ', 'g'), 'UTF8')), 'hex') = 'c2f90b0ccef94f85ebc3f3365ddfed88443aced61775661cf01b5db62babb079') THEN \
    \ ALTER TABLE perps_lp_settlement_transactions DROP CONSTRAINT IF EXISTS perps_lp_settlement_terminal_receipt_identity_check; \
    \ ALTER TABLE perps_lp_settlement_transactions ADD CONSTRAINT perps_lp_settlement_terminal_receipt_identity_check CHECK ((status NOT IN ('confirmed_success', 'confirmed_revert') AND (status <> 'superseded' OR confirmed_at IS NULL)) OR receipt_transaction_hash = signed_transaction_hash); \
    \END IF; \
    \IF NOT EXISTS (SELECT 1 FROM pg_constraint k WHERE k.conrelid = 'perps_lp_settlement_transactions'::regclass AND k.conname = 'perps_lp_settlement_superseded_receipt_check' AND k.contype = 'c' AND k.convalidated \
    \ AND encode(sha256(convert_to(regexp_replace(lower(trim(pg_get_constraintdef(k.oid, true))), E'\\\\s+', ' ', 'g'), 'UTF8')), 'hex') = '5003d267946b1bfc7d66aeada89395bcb814829359bb7427c02ca0015c1f39b6') THEN \
    \ ALTER TABLE perps_lp_settlement_transactions DROP CONSTRAINT IF EXISTS perps_lp_settlement_superseded_receipt_check; \
    \ ALTER TABLE perps_lp_settlement_transactions ADD CONSTRAINT perps_lp_settlement_superseded_receipt_check CHECK (status <> 'superseded' OR confirmed_at IS NULL OR (receipt_transaction_hash IS NOT NULL AND receipt_succeeded IS FALSE AND settlement_event_log_index IS NULL AND confirmation_depth IS NOT NULL)); \
    \END IF; END $$"
  -- Retain the plan's monitor lane and add a second chain-wide signer lane.
  -- If a pre-release build reused the monitor index name for signer scope,
  -- restore its canonical definition before adding the signer index.
  _ <- execute_ conn
    "DO $$ BEGIN \
    \IF NOT EXISTS (SELECT 1 FROM pg_index i JOIN pg_class idx ON idx.oid = i.indexrelid JOIN pg_class tbl ON tbl.oid = i.indrelid JOIN pg_namespace n ON n.oid = tbl.relnamespace JOIN pg_am am ON am.oid = idx.relam \
    \ WHERE n.nspname = current_schema() AND idx.relname = 'idx_perps_lp_settlement_one_active' AND tbl.relname = 'perps_lp_settlement_transactions' \
    \ AND i.indisunique AND i.indisvalid AND i.indisready AND i.indislive AND NOT i.indisexclusion AND i.indnatts = 2 AND i.indnkeyatts = 2 AND am.amname = 'btree' \
    \ AND pg_get_indexdef(i.indexrelid, 1, true) = 'chain_id' AND pg_get_indexdef(i.indexrelid, 2, true) = 'monitor_address' AND i.indoption::text = '0 0' \
    \ AND encode(sha256(convert_to(regexp_replace(lower(trim(COALESCE(pg_get_expr(i.indpred, i.indrelid, true), ''))), E'\\\\s+', ' ', 'g'), 'UTF8')), 'hex') = '11614e1c65ad7e4cbba8b221d476771aa63e0a0a85d2f0935f083e24ed1eff7a') THEN \
    \ DROP INDEX IF EXISTS idx_perps_lp_settlement_one_active; \
    \END IF; END $$"
  _ <- execute_ conn
    "CREATE UNIQUE INDEX IF NOT EXISTS idx_perps_lp_settlement_one_active \
    \ON perps_lp_settlement_transactions(chain_id, monitor_address) \
    \WHERE status IN ('prepared', 'broadcast', 'pending', 'confirming', 'manual_review')"
  _ <- execute_ conn
    "DO $$ BEGIN \
    \IF NOT EXISTS (SELECT 1 FROM pg_index i JOIN pg_class idx ON idx.oid = i.indexrelid JOIN pg_class tbl ON tbl.oid = i.indrelid JOIN pg_namespace n ON n.oid = tbl.relnamespace JOIN pg_am am ON am.oid = idx.relam \
    \ WHERE n.nspname = current_schema() AND idx.relname = 'idx_perps_lp_settlement_one_active_signer' AND tbl.relname = 'perps_lp_settlement_transactions' \
    \ AND i.indisunique AND i.indisvalid AND i.indisready AND i.indislive AND NOT i.indisexclusion AND i.indnatts = 2 AND i.indnkeyatts = 2 AND am.amname = 'btree' \
    \ AND pg_get_indexdef(i.indexrelid, 1, true) = 'chain_id' AND pg_get_indexdef(i.indexrelid, 2, true) = 'signer_address' AND i.indoption::text = '0 0' \
    \ AND encode(sha256(convert_to(regexp_replace(lower(trim(COALESCE(pg_get_expr(i.indpred, i.indrelid, true), ''))), E'\\\\s+', ' ', 'g'), 'UTF8')), 'hex') = '11614e1c65ad7e4cbba8b221d476771aa63e0a0a85d2f0935f083e24ed1eff7a') THEN \
    \ DROP INDEX IF EXISTS idx_perps_lp_settlement_one_active_signer; \
    \END IF; END $$"
  _ <- execute_ conn
    "CREATE UNIQUE INDEX IF NOT EXISTS idx_perps_lp_settlement_one_active_signer \
    \ON perps_lp_settlement_transactions(chain_id, signer_address) \
    \WHERE status IN ('prepared', 'broadcast', 'pending', 'confirming', 'manual_review')"
  _ <- execute_ conn
    "DO $$ BEGIN \
    \IF NOT EXISTS (SELECT 1 FROM pg_index i JOIN pg_class idx ON idx.oid = i.indexrelid JOIN pg_class tbl ON tbl.oid = i.indrelid JOIN pg_namespace n ON n.oid = tbl.relnamespace JOIN pg_am am ON am.oid = idx.relam \
    \ WHERE n.nspname = current_schema() AND idx.relname = 'idx_perps_lp_settlement_one_terminal_nonce' AND tbl.relname = 'perps_lp_settlement_transactions' \
    \ AND i.indisunique AND i.indisvalid AND i.indisready AND i.indislive AND NOT i.indisexclusion AND i.indnatts = 3 AND i.indnkeyatts = 3 AND am.amname = 'btree' \
    \ AND pg_get_indexdef(i.indexrelid, 1, true) = 'chain_id' AND pg_get_indexdef(i.indexrelid, 2, true) = 'signer_address' AND pg_get_indexdef(i.indexrelid, 3, true) = 'tx_nonce' AND i.indoption::text = '0 0 0' \
    \ AND encode(sha256(convert_to(regexp_replace(lower(trim(COALESCE(pg_get_expr(i.indpred, i.indrelid, true), ''))), E'\\\\s+', ' ', 'g'), 'UTF8')), 'hex') = '476c99ceb93e8b141f47c954d09b35e8e52b29e7dcf8cdf1bb1cc194aff580be') THEN \
    \ DROP INDEX IF EXISTS idx_perps_lp_settlement_one_terminal_nonce; \
    \END IF; END $$"
  _ <- execute_ conn
    "CREATE UNIQUE INDEX IF NOT EXISTS idx_perps_lp_settlement_one_terminal_nonce \
    \ON perps_lp_settlement_transactions(chain_id, signer_address, tx_nonce) \
    \WHERE status IN ('confirmed_success', 'confirmed_revert') OR (status = 'superseded' AND confirmed_at IS NOT NULL)"
  _ <- execute_ conn
    "CREATE INDEX IF NOT EXISTS idx_perps_lp_settlement_observation_history \
    \ON perps_lp_settlement_transactions(chain_id, monitor_address, observation_digest, replacement_count)"
  _ <- execute_ conn
    "CREATE INDEX IF NOT EXISTS idx_perps_lp_settlement_receipt_recheck \
    \ON perps_lp_settlement_transactions(chain_id, monitor_address, updated_at) \
    \WHERE status IN ('broadcast', 'pending', 'confirming', 'manual_review')"
  _ <- execute_ conn
    "CREATE INDEX IF NOT EXISTS idx_perps_lp_settlement_success_heartbeat \
    \ON perps_lp_settlement_transactions(chain_id, monitor_address, confirmed_at DESC) \
    \WHERE status = 'confirmed_success'"
  _ <- execute_ conn
    "CREATE OR REPLACE FUNCTION validate_lp_settlement_replacement_insert() RETURNS trigger AS $$ \
    \DECLARE predecessor perps_lp_settlement_transactions%ROWTYPE; \
    \BEGIN \
    \IF NEW.status <> 'prepared' THEN RAISE EXCEPTION 'LP settlement transactions must be inserted prepared'; END IF; \
    \IF NEW.replaces_attempt_id IS NULL THEN RETURN NEW; END IF; \
    \SELECT * INTO predecessor FROM perps_lp_settlement_transactions WHERE id = NEW.replaces_attempt_id; \
    \IF NOT FOUND THEN RAISE EXCEPTION 'LP settlement replacement predecessor does not exist'; END IF; \
    \IF predecessor.status <> 'replaced' THEN RAISE EXCEPTION 'LP settlement replacement predecessor is not marked replaced'; END IF; \
    \IF NEW.replacement_count <> predecessor.replacement_count + 1 THEN RAISE EXCEPTION 'invalid LP settlement replacement_count'; END IF; \
    \IF ROW(NEW.chain_id, NEW.monitor_address, NEW.observation_digest, NEW.epoch, NEW.signer_address, NEW.tx_nonce, NEW.target_address, NEW.tx_value, NEW.calldata, NEW.gas_limit) \
    \ IS DISTINCT FROM ROW(predecessor.chain_id, predecessor.monitor_address, predecessor.observation_digest, predecessor.epoch, predecessor.signer_address, predecessor.tx_nonce, predecessor.target_address, predecessor.tx_value, predecessor.calldata, predecessor.gas_limit) \
    \THEN RAISE EXCEPTION 'LP settlement replacement changed signed transaction semantics'; END IF; \
    \IF NEW.max_priority_fee_per_gas < predecessor.max_priority_fee_per_gas OR NEW.max_fee_per_gas < predecessor.max_fee_per_gas OR (NEW.max_priority_fee_per_gas = predecessor.max_priority_fee_per_gas AND NEW.max_fee_per_gas = predecessor.max_fee_per_gas) \
    \THEN RAISE EXCEPTION 'LP settlement replacement fees did not increase'; END IF; \
    \RETURN NEW; END; \
    \$$ LANGUAGE plpgsql"
  _ <- execute_ conn
    "DO $$ BEGIN \
    \IF NOT EXISTS (SELECT 1 FROM pg_trigger WHERE tgname = 'trg_perps_lp_settlement_replacement_insert' AND tgrelid = 'perps_lp_settlement_transactions'::regclass) THEN \
    \BEGIN CREATE TRIGGER trg_perps_lp_settlement_replacement_insert BEFORE INSERT ON perps_lp_settlement_transactions FOR EACH ROW EXECUTE FUNCTION validate_lp_settlement_replacement_insert(); \
    \EXCEPTION WHEN duplicate_object THEN NULL; END; \
    \END IF; END $$"
  _ <- execute_ conn
    "CREATE OR REPLACE FUNCTION require_lp_settlement_replacement_successor() RETURNS trigger AS $$ \
    \BEGIN \
    \IF NEW.status = 'replaced' AND NOT EXISTS (SELECT 1 FROM perps_lp_settlement_transactions successor WHERE successor.replaces_attempt_id = NEW.id) \
    \THEN RAISE EXCEPTION 'replaced LP settlement transaction must retain a successor'; END IF; \
    \RETURN NULL; END; \
    \$$ LANGUAGE plpgsql"
  _ <- execute_ conn
    "DO $$ BEGIN \
    \IF NOT EXISTS (SELECT 1 FROM pg_trigger WHERE tgname = 'trg_perps_lp_settlement_replaced_successor' AND tgrelid = 'perps_lp_settlement_transactions'::regclass) THEN \
    \BEGIN CREATE CONSTRAINT TRIGGER trg_perps_lp_settlement_replaced_successor AFTER INSERT OR UPDATE ON perps_lp_settlement_transactions DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE FUNCTION require_lp_settlement_replacement_successor(); \
    \EXCEPTION WHEN duplicate_object THEN NULL; END; \
    \END IF; END $$"
  _ <- execute_ conn
    "DO $$ BEGIN \
    \IF EXISTS (SELECT 1 FROM perps_lp_settlement_transactions predecessor WHERE predecessor.status = 'replaced' \
    \ AND NOT EXISTS (SELECT 1 FROM perps_lp_settlement_transactions successor WHERE successor.replaces_attempt_id = predecessor.id)) \
    \THEN RAISE EXCEPTION 'replaced LP settlement transaction exists without a successor'; END IF; END $$"
  _ <- execute_ conn
    "CREATE OR REPLACE FUNCTION protect_lp_settlement_transaction_intent() RETURNS trigger AS $$ \
    \BEGIN \
    \IF TG_OP = 'DELETE' THEN RAISE EXCEPTION 'perps_lp_settlement_transactions is append-only'; END IF; \
    \IF ROW(OLD.id, OLD.chain_id, OLD.monitor_address, OLD.observation_digest, OLD.epoch, OLD.replacement_count, OLD.replaces_attempt_id, OLD.signer_address, OLD.tx_nonce, OLD.target_address, OLD.tx_value, OLD.calldata, OLD.gas_limit, OLD.max_priority_fee_per_gas, OLD.max_fee_per_gas, OLD.signed_raw_transaction, OLD.signed_transaction_hash, OLD.created_at) \
    \ IS DISTINCT FROM ROW(NEW.id, NEW.chain_id, NEW.monitor_address, NEW.observation_digest, NEW.epoch, NEW.replacement_count, NEW.replaces_attempt_id, NEW.signer_address, NEW.tx_nonce, NEW.target_address, NEW.tx_value, NEW.calldata, NEW.gas_limit, NEW.max_priority_fee_per_gas, NEW.max_fee_per_gas, NEW.signed_raw_transaction, NEW.signed_transaction_hash, NEW.created_at) \
    \THEN RAISE EXCEPTION 'LP settlement signed intent is immutable'; END IF; \
    \IF NOT (OLD.status = NEW.status \
    \ OR (OLD.status = 'prepared' AND NEW.status IN ('broadcast', 'pending', 'manual_review', 'replaced', 'confirmed_success', 'superseded')) \
    \ OR (OLD.status IN ('broadcast', 'pending') AND NEW.status IN ('broadcast', 'pending', 'confirming', 'manual_review', 'replaced', 'confirmed_success', 'superseded')) \
    \ OR (OLD.status = 'confirming' AND NEW.status IN ('pending', 'manual_review', 'confirmed_success', 'superseded')) \
    \ OR (OLD.status = 'manual_review' AND NEW.status IN ('confirmed_success', 'superseded')) \
    \ OR (OLD.status = 'replaced' AND NEW.status IN ('manual_review', 'confirmed_success', 'superseded'))) \
    \THEN RAISE EXCEPTION 'invalid LP settlement transaction status transition'; END IF; \
    \IF OLD.status IN ('prepared', 'broadcast', 'pending', 'confirming', 'manual_review') AND NEW.status IN ('failed', 'abandoned') \
    \THEN RAISE EXCEPTION 'active LP settlement transaction cannot be released without canonical receipt evidence'; END IF; \
    \IF OLD.status = 'manual_review' AND NEW.status NOT IN ('manual_review', 'confirmed_success', 'superseded') \
    \THEN RAISE EXCEPTION 'manual-review LP settlement transaction cannot be reopened without canonical terminal evidence'; END IF; \
    \IF OLD.status IN ('prepared', 'broadcast', 'pending', 'confirming', 'manual_review') AND NEW.status = 'confirmed_revert' \
    \THEN RAISE EXCEPTION 'reverted LP settlement receipt must be recorded as manual review or receipt-backed superseded'; END IF; \
    \IF OLD.status IN ('prepared', 'broadcast', 'pending', 'confirming', 'manual_review') AND NEW.status = 'superseded' AND NEW.confirmed_at IS NULL \
    \ AND NOT EXISTS (SELECT 1 FROM perps_lp_settlement_transactions winner WHERE winner.id <> OLD.id AND winner.chain_id = OLD.chain_id AND winner.signer_address = OLD.signer_address AND winner.tx_nonce = OLD.tx_nonce \
    \ AND (winner.status IN ('confirmed_success', 'confirmed_revert') OR (winner.status = 'superseded' AND winner.confirmed_at IS NOT NULL))) \
    \THEN RAISE EXCEPTION 'active LP settlement transaction cannot be superseded without a terminal same-nonce receipt'; END IF; \
    \IF (OLD.status IN ('confirmed_success', 'confirmed_revert') OR (OLD.status = 'superseded' AND OLD.confirmed_at IS NOT NULL)) AND ROW(OLD.status, OLD.last_error, OLD.receipt_transaction_hash, OLD.receipt_block_number, OLD.receipt_block_hash, OLD.receipt_succeeded, OLD.confirmed_at, OLD.confirmation_depth, OLD.settlement_event_log_index, OLD.cutoff_epoch, OLD.senior_redeem_assets, OLD.junior_redeem_assets, OLD.junior_deposit_assets, OLD.senior_deposit_assets, OLD.senior_backlog, OLD.junior_backlog, OLD.entries_deferred) \
    \ IS DISTINCT FROM ROW(NEW.status, NEW.last_error, NEW.receipt_transaction_hash, NEW.receipt_block_number, NEW.receipt_block_hash, NEW.receipt_succeeded, NEW.confirmed_at, NEW.confirmation_depth, NEW.settlement_event_log_index, NEW.cutoff_epoch, NEW.senior_redeem_assets, NEW.junior_redeem_assets, NEW.junior_deposit_assets, NEW.senior_deposit_assets, NEW.senior_backlog, NEW.junior_backlog, NEW.entries_deferred) \
    \THEN RAISE EXCEPTION 'terminal LP settlement evidence is immutable'; END IF; \
    \RETURN NEW; END; \
    \$$ LANGUAGE plpgsql"
  _ <- execute_ conn
    "DO $$ BEGIN \
    \IF NOT EXISTS (SELECT 1 FROM pg_trigger WHERE tgname = 'trg_perps_lp_settlement_transaction_intent' AND tgrelid = 'perps_lp_settlement_transactions'::regclass) THEN \
    \BEGIN CREATE TRIGGER trg_perps_lp_settlement_transaction_intent BEFORE UPDATE OR DELETE ON perps_lp_settlement_transactions FOR EACH ROW EXECUTE FUNCTION protect_lp_settlement_transaction_intent(); \
    \EXCEPTION WHEN duplicate_object THEN NULL; END; \
    \END IF; END $$"
  _ <- execute_ conn
    "CREATE TABLE IF NOT EXISTS perps_lp_settlement_broadcasts (\
    \id BIGSERIAL PRIMARY KEY,\
    \attempt_id BIGINT NOT NULL REFERENCES perps_lp_settlement_transactions(id) ON DELETE RESTRICT,\
    \broadcast_sequence INTEGER NOT NULL CHECK (broadcast_sequence > 0),\
    \outcome VARCHAR(24) NOT NULL CHECK (outcome IN ('accepted', 'already_known', 'rejected', 'ambiguous')),\
    \returned_transaction_hash VARCHAR(66) CHECK (returned_transaction_hash IS NULL OR returned_transaction_hash ~ '^0x[0-9a-f]{64}$'),\
    \rpc_error TEXT,\
    \broadcast_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),\
    \UNIQUE (attempt_id, broadcast_sequence)\
    \)"
  _ <- execute_ conn
    "CREATE OR REPLACE FUNCTION reject_lp_settlement_broadcast_mutation() RETURNS trigger AS $$ \
    \BEGIN RAISE EXCEPTION 'perps_lp_settlement_broadcasts is append-only'; END; \
    \$$ LANGUAGE plpgsql"
  _ <- execute_ conn
    "DO $$ BEGIN \
    \IF NOT EXISTS (SELECT 1 FROM pg_trigger WHERE tgname = 'trg_perps_lp_settlement_broadcasts_append_only' AND tgrelid = 'perps_lp_settlement_broadcasts'::regclass) THEN \
    \BEGIN CREATE TRIGGER trg_perps_lp_settlement_broadcasts_append_only BEFORE UPDATE OR DELETE ON perps_lp_settlement_broadcasts FOR EACH ROW EXECUTE FUNCTION reject_lp_settlement_broadcast_mutation(); \
    \EXCEPTION WHEN duplicate_object THEN NULL; END; \
    \END IF; END $$"
  pure ()

-- | Read-only catalog verification used by deployment preflight. This checks
-- the complete v1 persistence surface rather than merely testing that the
-- tables can be resolved. Database connectivity/query errors still throw;
-- schema drift is returned as a descriptive 'Left'.
verifyLpSettlementSchema :: Connection -> IO (Either Text ())
verifyLpSettlementSchema conn = do
  tableRows <- query_ conn
    "SELECT table_name::text FROM information_schema.tables \
    \WHERE table_schema = current_schema() AND table_type = 'BASE TABLE' \
    \AND table_name IN ('perps_lp_settlement_observations', 'perps_lp_settlement_transactions', 'perps_lp_settlement_broadcasts')"
    :: IO [Only Text]
  columnShapeRows <- query_ conn
    "SELECT c.relname::text, a.attname::text, \
    \pg_catalog.format_type(a.atttypid, a.atttypmod)::text, a.attnotnull \
    \FROM pg_catalog.pg_attribute a \
    \JOIN pg_catalog.pg_class c ON c.oid = a.attrelid \
    \JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace \
    \WHERE n.nspname = current_schema() AND a.attnum > 0 AND NOT a.attisdropped \
    \AND c.relname IN ('perps_lp_settlement_observations', 'perps_lp_settlement_transactions', 'perps_lp_settlement_broadcasts')"
    :: IO [(Text, Text, Text, Bool)]
  columnDefaultRows <- query_ conn
    "SELECT c.relname::text, a.attname::text, pg_get_expr(d.adbin, d.adrelid, true)::text \
    \FROM pg_catalog.pg_attribute a \
    \JOIN pg_catalog.pg_class c ON c.oid = a.attrelid \
    \JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace \
    \JOIN pg_catalog.pg_attrdef d ON d.adrelid = a.attrelid AND d.adnum = a.attnum \
    \WHERE n.nspname = current_schema() AND a.attnum > 0 AND NOT a.attisdropped \
    \AND c.relname IN ('perps_lp_settlement_observations', 'perps_lp_settlement_transactions', 'perps_lp_settlement_broadcasts')"
    :: IO [(Text, Text, Text)]
  indexRows <- query_ conn
    "SELECT idx.relname::text, tbl.relname::text, i.indisunique, i.indisexclusion, \
    \(i.indisvalid AND i.indisready AND i.indislive AND NOT i.indisexclusion \
    \ AND i.indnatts = i.indnkeyatts AND am.amname = 'btree') AS canonical_shape, \
    \(SELECT string_agg(pg_get_indexdef(i.indexrelid, key_no, true), ', ' ORDER BY key_no) \
    \ FROM generate_series(1, i.indnkeyatts) AS keys(key_no))::text AS ordered_keys, \
    \(SELECT string_agg(option::text, ',' ORDER BY ordinality) \
    \ FROM unnest(i.indoption) WITH ORDINALITY AS options(option, ordinality) \
    \ WHERE ordinality <= i.indnkeyatts)::text AS key_options, \
    \encode(sha256(convert_to(regexp_replace(lower(trim(COALESCE(pg_get_expr(i.indpred, i.indrelid, true), ''))), E'\\\\s+', ' ', 'g'), 'UTF8')), 'hex')::text \
    \FROM pg_catalog.pg_index i \
    \JOIN pg_catalog.pg_class idx ON idx.oid = i.indexrelid \
    \JOIN pg_catalog.pg_class tbl ON tbl.oid = i.indrelid \
    \JOIN pg_catalog.pg_namespace n ON n.oid = tbl.relnamespace \
    \JOIN pg_catalog.pg_am am ON am.oid = idx.relam \
    \WHERE n.nspname = current_schema() AND tbl.relname IN \
    \('perps_lp_settlement_observations', 'perps_lp_settlement_transactions', 'perps_lp_settlement_broadcasts')"
    :: IO [(Text, Text, Bool, Bool, Bool, Text, Text, Text)]
  triggerRows <- query_ conn
    "SELECT t.tgname::text, c.relname::text, t.tgtype::integer, \
    \t.tgdeferrable, t.tginitdeferred, (t.tgconstraint <> 0), p.proname::text, \
    \l.lanname::text, \
    \(t.tgnargs = 0 AND p.pronargs = 0 AND p.prorettype = 'trigger'::regtype \
    \ AND p.provolatile = 'v' AND p.prokind = 'f' AND NOT p.prosecdef AND NOT p.proleakproof) AS canonical_function, \
    \encode(sha256(convert_to(regexp_replace(lower(p.prosrc), E'\\\\s+', '', 'g'), 'UTF8')), 'hex')::text \
    \FROM pg_trigger t \
    \JOIN pg_class c ON c.oid = t.tgrelid \
    \JOIN pg_namespace n ON n.oid = c.relnamespace \
    \JOIN pg_proc p ON p.oid = t.tgfoid \
    \JOIN pg_language l ON l.oid = p.prolang \
    \WHERE n.nspname = current_schema() AND NOT t.tgisinternal AND t.tgenabled = 'O' \
    \AND c.relname IN ('perps_lp_settlement_observations', 'perps_lp_settlement_transactions', 'perps_lp_settlement_broadcasts')"
    :: IO [(Text, Text, Int, Bool, Bool, Bool, Text, Text, Bool, Text)]
  constraintRows <- query_ conn
    "SELECT c.relname::text, k.contype::text, \
    \encode(sha256(convert_to(regexp_replace(lower(trim(pg_get_constraintdef(k.oid, true))), E'\\\\s+', ' ', 'g'), 'UTF8')), 'hex')::text \
    \FROM pg_constraint k \
    \JOIN pg_class c ON c.oid = k.conrelid \
    \JOIN pg_namespace n ON n.oid = c.relnamespace \
    \WHERE n.nspname = current_schema() AND k.convalidated \
    \AND c.relname IN ('perps_lp_settlement_observations', 'perps_lp_settlement_transactions', 'perps_lp_settlement_broadcasts')"
    :: IO [(Text, Text, Text)]
  -- Preserve multiplicity and include unvalidated constraints so restrictive
  -- drift cannot hide. The canonical digest is defined in C byte order so it
  -- is also stable across databases initialized with different collations.
  constraintSetFingerprintRows <- query_ conn
    "WITH definitions AS (SELECT c.relname::text AS table_name, k.contype::text AS constraint_type, \
    \regexp_replace(lower(trim(pg_get_constraintdef(k.oid, true))), E'\\\\s+', ' ', 'g') AS definition \
    \FROM pg_constraint k JOIN pg_class c ON c.oid = k.conrelid \
    \JOIN pg_namespace n ON n.oid = c.relnamespace \
    \WHERE n.nspname = current_schema() \
    \AND c.relname IN ('perps_lp_settlement_observations', 'perps_lp_settlement_transactions', 'perps_lp_settlement_broadcasts')) \
    \SELECT encode(sha256(convert_to(string_agg(table_name || chr(31) || constraint_type || chr(31) || definition, \
    \chr(30) ORDER BY table_name COLLATE \"C\", constraint_type COLLATE \"C\", definition COLLATE \"C\"), 'UTF8')), 'hex')::text FROM definitions"
    :: IO [Only (Maybe Text)]
  let observationsTable = "perps_lp_settlement_observations"
      transactionsTable = "perps_lp_settlement_transactions"
      broadcastsTable = "perps_lp_settlement_broadcasts"
      requiredTables = [observationsTable, transactionsTable, broadcastsTable]
      actualTables = [tableName | Only tableName <- tableRows]
      requiredColumnShapes =
        shapesFor observationsTable
          [ ("chain_id", "bigint", True)
          , ("monitor_address", "character varying(42)", True)
          , ("observation_digest", "character varying(66)", True)
          , ("epoch", "bigint", True)
          , ("observed_block", "bigint", True)
          , ("observed_block_hash", "character varying(66)", False)
          , ("execution_path", "integer", True)
          , ("operational_blocker_mask", "numeric(78,0)", True)
          , ("warning_mask", "numeric(78,0)", True)
          , ("dependency_failure_mask", "numeric(78,0)", True)
          , ("critical_fault_mask", "numeric(78,0)", True)
          , ("schema_version", "numeric(78,0)", False)
          , ("health_state", "numeric(78,0)", False)
          , ("execution_path_dependency_mask", "numeric(78,0)", False)
          , ("status_dependency_failure_mask", "numeric(78,0)", False)
          , ("health_dependency_failure_mask", "numeric(78,0)", False)
          , ("observation_complete", "boolean", False)
          , ("has_matured_work", "boolean", False)
          , ("lp_epoch_settlement_paused", "boolean", False)
          , ("first_observed_at", "timestamp with time zone", True)
          , ("last_observed_at", "timestamp with time zone", True)
          ]
          <> shapesFor transactionsTable
            [ ("id", "bigint", True)
            , ("chain_id", "bigint", True)
            , ("monitor_address", "character varying(42)", True)
            , ("observation_digest", "character varying(66)", True)
            , ("epoch", "bigint", True)
            , ("replacement_count", "integer", True)
            , ("replaces_attempt_id", "bigint", False)
            , ("signer_address", "character varying(42)", True)
            , ("tx_nonce", "numeric(78,0)", True)
            , ("target_address", "character varying(42)", True)
            , ("tx_value", "numeric(78,0)", True)
            , ("calldata", "bytea", True)
            , ("gas_limit", "numeric(78,0)", True)
            , ("max_priority_fee_per_gas", "numeric(78,0)", True)
            , ("max_fee_per_gas", "numeric(78,0)", True)
            , ("signed_raw_transaction", "bytea", True)
            , ("signed_transaction_hash", "character varying(66)", True)
            , ("status", "character varying(32)", True)
            , ("last_error", "text", False)
            , ("receipt_transaction_hash", "character varying(66)", False)
            , ("receipt_block_number", "bigint", False)
            , ("receipt_block_hash", "character varying(66)", False)
            , ("receipt_succeeded", "boolean", False)
            , ("confirmed_at", "timestamp with time zone", False)
            , ("confirmation_depth", "integer", False)
            , ("settlement_event_log_index", "bigint", False)
            , ("cutoff_epoch", "numeric(78,0)", False)
            , ("senior_redeem_assets", "numeric(78,0)", False)
            , ("junior_redeem_assets", "numeric(78,0)", False)
            , ("junior_deposit_assets", "numeric(78,0)", False)
            , ("senior_deposit_assets", "numeric(78,0)", False)
            , ("senior_backlog", "boolean", False)
            , ("junior_backlog", "boolean", False)
            , ("entries_deferred", "boolean", False)
            , ("created_at", "timestamp with time zone", True)
            , ("updated_at", "timestamp with time zone", True)
            ]
          <> shapesFor broadcastsTable
            [ ("id", "bigint", True)
            , ("attempt_id", "bigint", True)
            , ("broadcast_sequence", "integer", True)
            , ("outcome", "character varying(24)", True)
            , ("returned_transaction_hash", "character varying(66)", False)
            , ("rpc_error", "text", False)
            , ("broadcast_at", "timestamp with time zone", True)
            ]
      columnRows = [(tableName, columnName) | (tableName, columnName, _, _) <- columnShapeRows]
      requiredColumns = [(tableName, columnName) | (tableName, columnName, _, _) <- requiredColumnShapes]
      missingTables = filter (`notElem` actualTables) requiredTables
      missingColumns = filter (`notElem` columnRows) requiredColumns
      missingColumnShapes = filter (`notElem` columnShapeRows) requiredColumnShapes
      requiredColumnDefaults =
        [ (observationsTable, "first_observed_at", "now()")
        , (observationsTable, "last_observed_at", "now()")
        , (transactionsTable, "id", "nextval('perps_lp_settlement_transactions_id_seq'::regclass)")
        , (transactionsTable, "replacement_count", "0")
        , (transactionsTable, "created_at", "now()")
        , (transactionsTable, "updated_at", "now()")
        , (broadcastsTable, "id", "nextval('perps_lp_settlement_broadcasts_id_seq'::regclass)")
        , (broadcastsTable, "broadcast_at", "now()")
        ]
      missingColumnDefaults = filter (`notElem` columnDefaultRows) requiredColumnDefaults
      -- Predicate and definition fingerprints are SHA-256 over PostgreSQL 16's
      -- lower-cased, whitespace-normalized catalog rendering. Infrastructure
      -- pins PostgreSQL 16. Exact fingerprints prevent a definition that only
      -- retains the expected words (for example, by adding AND FALSE) from
      -- passing deployment preflight.
      requiredIndexes =
        [ ( "idx_perps_lp_settlement_one_active"
          , transactionsTable
          , True
          , False
          , True
          , "chain_id, monitor_address"
          , "0,0"
          , "11614e1c65ad7e4cbba8b221d476771aa63e0a0a85d2f0935f083e24ed1eff7a"
          )
        , ( "idx_perps_lp_settlement_one_active_signer"
          , transactionsTable
          , True
          , False
          , True
          , "chain_id, signer_address"
          , "0,0"
          , "11614e1c65ad7e4cbba8b221d476771aa63e0a0a85d2f0935f083e24ed1eff7a"
          )
        , ( "idx_perps_lp_settlement_one_terminal_nonce"
          , transactionsTable
          , True
          , False
          , True
          , "chain_id, signer_address, tx_nonce"
          , "0,0,0"
          , "476c99ceb93e8b141f47c954d09b35e8e52b29e7dcf8cdf1bb1cc194aff580be"
          )
        , ( "idx_perps_lp_settlement_observation_history"
          , transactionsTable
          , False
          , False
          , True
          , "chain_id, monitor_address, observation_digest, replacement_count"
          , "0,0,0,0"
          , "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
          )
        , ( "idx_perps_lp_settlement_receipt_recheck"
          , transactionsTable
          , False
          , False
          , True
          , "chain_id, monitor_address, updated_at"
          , "0,0,0"
          , "7f9616d762da6f038f51e895cc2a9ffba18278e50ff4431dfed2f23652f08ad9"
          )
        , ( "idx_perps_lp_settlement_success_heartbeat"
          , transactionsTable
          , False
          , False
          , True
          , "chain_id, monitor_address, confirmed_at"
          , "0,0,3"
          , "efb1eb47914a122cc3a60e5613f734d68dab6d14985a0ff54a0b1edb68a55bc1"
          )
        ]
      missingIndexes =
        [ indexName
        | expected@(indexName, _, _, _, _, _, _, _) <- requiredIndexes
        , expected `notElem` indexRows
        ]
      allowedRestrictiveIndexShapes =
        [ (observationsTable, True, False, True, "chain_id, monitor_address, observation_digest", "0,0,0", emptyDefinitionFingerprint)
        , (observationsTable, True, False, True, "chain_id, monitor_address, observation_digest, epoch", "0,0,0,0", emptyDefinitionFingerprint)
        , (transactionsTable, True, False, True, "id", "0", emptyDefinitionFingerprint)
        , (transactionsTable, True, False, True, "signed_transaction_hash", "0", emptyDefinitionFingerprint)
        , (transactionsTable, True, False, True, "replaces_attempt_id", "0", emptyDefinitionFingerprint)
        , (transactionsTable, True, False, True, "chain_id, signer_address, tx_nonce, replacement_count", "0,0,0,0", emptyDefinitionFingerprint)
        , (transactionsTable, True, False, True, "chain_id, monitor_address", "0,0", "11614e1c65ad7e4cbba8b221d476771aa63e0a0a85d2f0935f083e24ed1eff7a")
        , (transactionsTable, True, False, True, "chain_id, signer_address", "0,0", "11614e1c65ad7e4cbba8b221d476771aa63e0a0a85d2f0935f083e24ed1eff7a")
        , (transactionsTable, True, False, True, "chain_id, signer_address, tx_nonce", "0,0,0", "476c99ceb93e8b141f47c954d09b35e8e52b29e7dcf8cdf1bb1cc194aff580be")
        , (broadcastsTable, True, False, True, "id", "0", emptyDefinitionFingerprint)
        , (broadcastsTable, True, False, True, "attempt_id, broadcast_sequence", "0,0", emptyDefinitionFingerprint)
        ]
      unexpectedRestrictiveIndexes =
        [ indexName
        | (indexName, tableName, isUnique, isExclusion, canonicalShape, orderedKeys, keyOptions, predicateFingerprint) <- indexRows
        , isUnique || isExclusion
        , (tableName, isUnique, isExclusion, canonicalShape, orderedKeys, keyOptions, predicateFingerprint)
            `notElem` allowedRestrictiveIndexShapes
        ]
      requiredTriggers =
        [ ( "trg_perps_lp_settlement_observation_identity"
          , observationsTable
          , 27
          , False
          , False
          , False
          , "protect_lp_settlement_observation_identity"
          , "plpgsql"
          , True
          , "e73d09ff27932ba8243e75591e7e296f43a626168145aa7e664ee69a3a7a3bf0"
          )
        , ( "trg_perps_lp_settlement_replacement_insert"
          , transactionsTable
          , 7
          , False
          , False
          , False
          , "validate_lp_settlement_replacement_insert"
          , "plpgsql"
          , True
          , "04c93fee1c7239f3d16ea53b01ea771805b77b0838fa9b788cee8441db71a2f1"
          )
        , ( "trg_perps_lp_settlement_replaced_successor"
          , transactionsTable
          , 21
          , True
          , True
          , True
          , "require_lp_settlement_replacement_successor"
          , "plpgsql"
          , True
          , "48e4cfed9d255e35a062ab571a6b1d2094a1a0cf3926ea70c9e282e175ff3336"
          )
        , ( "trg_perps_lp_settlement_transaction_intent"
          , transactionsTable
          , 27
          , False
          , False
          , False
          , "protect_lp_settlement_transaction_intent"
          , "plpgsql"
          , True
          , "d869130b2195fcf6ed2f1463132a38660f58d8ead7f5ba8eae07a717da19c252"
          )
        , ( "trg_perps_lp_settlement_broadcasts_append_only"
          , broadcastsTable
          , 27
          , False
          , False
          , False
          , "reject_lp_settlement_broadcast_mutation"
          , "plpgsql"
          , True
          , "12bd6bd853153bf8ace60d4bcc538a56d9ce47c4d56d97c82259dcf5b6492a47"
          )
        ]
      missingTriggers =
        [ triggerName
        | expected@(triggerName, _, _, _, _, _, _, _, _, _) <- requiredTriggers
        , expected `notElem` triggerRows
        ]
      requiredConstraints =
        [ ("observation primary key", observationsTable, "p", "b327d24bbff85ad8b44fc14422311152c302d0adf766133600df6f480c361c20")
        , ("observation epoch identity", observationsTable, "u", "c06a86e9c949fe9b98aae6028b0076e8c7499b0a73e9ab27a9a93da4821f44ac")
        , ("transaction primary key", transactionsTable, "p", "4dc891945e16ea7d6f277a8b51132b1f8a751408a8e57d36fe86cfa59f8640f4")
        , ("transaction observation foreign key", transactionsTable, "f", "679812a5d560a93582bccf75e465ab643af2c5fd45c9c4d1678585773c081c83")
        , ("signed transaction hash uniqueness", transactionsTable, "u", "661abcffb98055705020399df746b1cfbfd3f3227902e0290edf1e783a286ef2")
        , ("single replacement successor", transactionsTable, "u", "74a720f18e3e0f47c41f7ab05c2b192f666d94ea418ad50b135aaa77570ec055")
        , ("signer nonce replacement lineage", transactionsTable, "u", "4515d727d3995c3c2022d5ae8f7e8259e765718cd09d23db00ad64cc85a04b6f")
        , ("transaction status domain", transactionsTable, "c", "2bbd439a6b83336279526a7f336eb14aab3934929267a056211c35760228cbfa")
        , ("replacement linkage", transactionsTable, "c", "bf8296a116d21db099dc95f42647b2a5901eeb49a871b5738db51ecd87012c96")
        , ("receipt completeness", transactionsTable, "c", "f220c9f734f3b397d119cacf6630c8e0b02c7d2221b822aa4039a6ff90dc05cb")
        , ("settlement event completeness", transactionsTable, "c", "768f310e393ea3751a628abbf79d618932c2403b9e2144117b7947013fcb10e2")
        , ("confirmation outcome", transactionsTable, "c", "10a4e0fa933c2201f0e5525531dc709a69c33a7f041b08f299ea9db78a73a777")
        , ("terminal receipt identity", transactionsTable, "c", "c2f90b0ccef94f85ebc3f3365ddfed88443aced61775661cf01b5db62babb079")
        , ("superseded receipt outcome", transactionsTable, "c", "5003d267946b1bfc7d66aeada89395bcb814829359bb7427c02ca0015c1f39b6")
        , ("successful cutoff epoch identity", transactionsTable, "c", "17853a3381c45cdd3a9bb4d0d4afc7722437c8aa09eb0e44ef9cb92eb26be9fa")
        , ("broadcast primary key", broadcastsTable, "p", "4dc891945e16ea7d6f277a8b51132b1f8a751408a8e57d36fe86cfa59f8640f4")
        , ("broadcast transaction foreign key", broadcastsTable, "f", "659cdea12440e8438cab34d4a4720a13c254cfe8788e307dbd2fe328331bd9f8")
        , ("broadcast sequence uniqueness", broadcastsTable, "u", "62e05a83b11fcde6a031fd59d5f28439feb9730b53c657b5a50b4e804ea74ceb")
        , ("broadcast outcome domain", broadcastsTable, "c", "e5127d27d11c9d4af42c5a5365bf93717699e33996da3088ebb25e58a703ee36")
        ]
      missingConstraints =
        [ label
        | (label, tableName, constraintType, definitionFingerprint) <- requiredConstraints
        , (tableName, constraintType, definitionFingerprint) `notElem` constraintRows
        ]
      canonicalConstraintSetFingerprint =
        "7f7ced2f88daced3a97bcc99757610100280c705b4bcb097ed83c9682deee467"
      constraintSetDrift =
        if constraintSetFingerprintRows == [Only (Just canonicalConstraintSetFingerprint)]
          then []
          else ["complete LP constraint definition set"]
      failures =
        failure "missing tables" missingTables
          <> failure "missing columns" (map renderColumn missingColumns)
          <> failure "missing or drifted column shapes" (map renderColumnShape missingColumnShapes)
          <> failure "missing or drifted column defaults" (map renderColumnDefault missingColumnDefaults)
          <> failure "missing or drifted indexes" missingIndexes
          <> failure "unexpected restrictive indexes" unexpectedRestrictiveIndexes
          <> failure "missing, disabled, or drifted triggers" missingTriggers
          <> failure "missing or unvalidated constraints" missingConstraints
          <> failure "unexpected or drifted constraints" constraintSetDrift
  pure $
    if null failures
      then Right ()
      else Left ("LP settlement schema verification failed: " <> T.intercalate "; " failures)
  where
    shapesFor tableName = map (\(columnName, columnType, isNotNull) -> (tableName, columnName, columnType, isNotNull))
    renderColumn (tableName, columnName) = tableName <> "." <> columnName
    renderColumnShape (tableName, columnName, columnType, isNotNull) =
      renderColumn (tableName, columnName)
        <> " expected "
        <> columnType
        <> if isNotNull then " NOT NULL" else " NULL"
    renderColumnDefault (tableName, columnName, expectedDefault) =
      renderColumn (tableName, columnName) <> " expected DEFAULT " <> expectedDefault
    failure _ [] = []
    failure label values = [label <> " [" <> T.intercalate ", " values <> "]"]
    emptyDefinitionFingerprint = "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"

-- | Fail closed when an unreconcilable pre-v2 submission exists. The legacy
-- table never stored signed bytes, signer, or nonce, so a submitted row cannot
-- safely be adopted or replaced by the durable keeper.
verifyNoLegacySubmittedLpSettlementAttempts
  :: Connection
  -> Integer
  -> IO (Either Text ())
verifyNoLegacySubmittedLpSettlementAttempts conn chainId = do
  rows <- query conn
    "SELECT EXISTS (SELECT 1 FROM perps_lp_settlement_attempts \
    \WHERE chain_id = ? AND status = 'submitted')"
    (Only chainId) :: IO [Only Bool]
  pure $ case rows of
    [Only False] -> Right ()
    [Only True] ->
      Left
        "legacy submitted LP settlement attempt exists without complete signed transaction identity; manual reconciliation is required"
    _ -> Left "legacy LP settlement submission verification returned an unexpected result"

data LpSettlementAttemptRow = LpSettlementAttemptRow
  { lsarObservationDigest :: Text
  , lsarEpoch :: Integer
  , lsarObservedBlock :: Integer
  , lsarTransactionHash :: Maybe Text
  , lsarStatus :: Text
  }
  deriving stock (Show, Generic)

instance FromRow LpSettlementAttemptRow where
  fromRow =
    LpSettlementAttemptRow
      <$> field
      <*> field
      <*> field
      <*> field
      <*> field

recordLpSettlementObservation
  :: Connection
  -> Integer
  -> Text
  -> Text
  -> Integer
  -> Integer
  -> Integer
  -> Integer
  -> Integer
  -> Integer
  -> Integer
  -> IO ()
recordLpSettlementObservation conn chainId monitor digest epoch observedBlock executionPath operationalMask warningMask dependencyMask criticalMask = do
  _ <- execute conn
    "INSERT INTO perps_lp_settlement_attempts \
    \(chain_id, monitor_address, observation_digest, epoch, observed_block, execution_path, \
    \operational_blocker_mask, warning_mask, dependency_failure_mask, critical_fault_mask, status) \
    \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, 'observed') \
    \ON CONFLICT (chain_id, monitor_address, observation_digest) DO UPDATE SET \
    \epoch = EXCLUDED.epoch, observed_block = EXCLUDED.observed_block, \
    \execution_path = EXCLUDED.execution_path, \
    \operational_blocker_mask = EXCLUDED.operational_blocker_mask, \
    \warning_mask = EXCLUDED.warning_mask, \
    \dependency_failure_mask = EXCLUDED.dependency_failure_mask, \
    \critical_fault_mask = EXCLUDED.critical_fault_mask, updated_at = NOW()"
    ( chainId
    , T.toLower monitor
    , T.toLower digest
    , epoch
    , observedBlock
    , executionPath
    , show operationalMask
    , show warningMask
    , show dependencyMask
    , show criticalMask
    )
  pure ()

markLpSettlementAttemptStatus
  :: Connection
  -> Integer
  -> Text
  -> Text
  -> Text
  -> Maybe Text
  -> IO ()
markLpSettlementAttemptStatus conn chainId monitor digest status lastError = do
  _ <- execute conn
    "UPDATE perps_lp_settlement_attempts SET status = ?, last_error = ?, updated_at = NOW() \
    \WHERE chain_id = ? AND monitor_address = ? AND observation_digest = ?"
    (status, lastError, chainId, T.toLower monitor, T.toLower digest)
  pure ()

markLpSettlementAttemptSubmitted
  :: Connection
  -> Integer
  -> Text
  -> Text
  -> Text
  -> IO ()
markLpSettlementAttemptSubmitted conn chainId monitor digest txHash = do
  _ <- execute conn
    "UPDATE perps_lp_settlement_attempts SET transaction_hash = ?, status = 'submitted', \
    \last_error = NULL, updated_at = NOW() \
    \WHERE chain_id = ? AND monitor_address = ? AND observation_digest = ?"
    (T.toLower txHash, chainId, T.toLower monitor, T.toLower digest)
  pure ()

getSubmittedLpSettlementAttempts
  :: Connection
  -> Integer
  -> Text
  -> IO [LpSettlementAttemptRow]
getSubmittedLpSettlementAttempts conn chainId monitor =
  query conn
    "SELECT observation_digest, epoch, observed_block, transaction_hash, status \
    \FROM perps_lp_settlement_attempts \
    \WHERE chain_id = ? AND monitor_address = ? AND status = 'submitted' \
    \ORDER BY updated_at ASC"
    (chainId, T.toLower monitor)

-- | An immutable monitor observation. The digest is the observation identity;
-- recording the same digest with different contents is rejected. Nullable
-- columns in the table exist only so verifiable legacy observations can be
-- retained without inventing fields that the old keeper never stored.
data LpSettlementObservationInput = LpSettlementObservationInput
  { lsoiChainId :: Integer
  , lsoiMonitorAddress :: Text
  , lsoiObservationDigest :: Text
  , lsoiEpoch :: Integer
  , lsoiObservedBlock :: Integer
  , lsoiObservedBlockHash :: Maybe Text
  , lsoiExecutionPath :: Integer
  , lsoiOperationalBlockerMask :: Integer
  , lsoiWarningMask :: Integer
  , lsoiDependencyFailureMask :: Integer
  , lsoiCriticalFaultMask :: Integer
  , lsoiSchemaVersion :: Integer
  , lsoiHealthState :: Integer
  , lsoiExecutionPathDependencyMask :: Integer
  , lsoiStatusDependencyFailureMask :: Integer
  , lsoiHealthDependencyFailureMask :: Integer
  , lsoiObservationComplete :: Bool
  , lsoiHasMaturedWork :: Bool
  , lsoiLpEpochSettlementPaused :: Bool
  }
  deriving stock (Eq, Show, Generic)

instance ToRow LpSettlementObservationInput where
  toRow LpSettlementObservationInput{..} =
    [ toField lsoiChainId
    , toField lsoiMonitorAddress
    , toField lsoiObservationDigest
    , toField lsoiEpoch
    , toField lsoiObservedBlock
    , toField lsoiObservedBlockHash
    , toField lsoiExecutionPath
    , toField lsoiOperationalBlockerMask
    , toField lsoiWarningMask
    , toField lsoiDependencyFailureMask
    , toField lsoiCriticalFaultMask
    , toField lsoiSchemaVersion
    , toField lsoiHealthState
    , toField lsoiExecutionPathDependencyMask
    , toField lsoiStatusDependencyFailureMask
    , toField lsoiHealthDependencyFailureMask
    , toField lsoiObservationComplete
    , toField lsoiHasMaturedWork
    , toField lsoiLpEpochSettlementPaused
    ]

recordLpSettlementObservationV2 :: Connection -> LpSettlementObservationInput -> IO ()
recordLpSettlementObservationV2 conn input = do
  affected <- execute conn
    "INSERT INTO perps_lp_settlement_observations \
    \(chain_id, monitor_address, observation_digest, epoch, observed_block, observed_block_hash, \
    \ execution_path, operational_blocker_mask, warning_mask, dependency_failure_mask, \
    \ critical_fault_mask, schema_version, health_state, execution_path_dependency_mask, \
    \ status_dependency_failure_mask, health_dependency_failure_mask, observation_complete, \
    \ has_matured_work, lp_epoch_settlement_paused) \
    \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) \
    \ON CONFLICT (chain_id, monitor_address, observation_digest) DO UPDATE SET \
    \ observed_block_hash = COALESCE(perps_lp_settlement_observations.observed_block_hash, EXCLUDED.observed_block_hash), \
    \ schema_version = COALESCE(perps_lp_settlement_observations.schema_version, EXCLUDED.schema_version), \
    \ health_state = COALESCE(perps_lp_settlement_observations.health_state, EXCLUDED.health_state), \
    \ execution_path_dependency_mask = COALESCE(perps_lp_settlement_observations.execution_path_dependency_mask, EXCLUDED.execution_path_dependency_mask), \
    \ status_dependency_failure_mask = COALESCE(perps_lp_settlement_observations.status_dependency_failure_mask, EXCLUDED.status_dependency_failure_mask), \
    \ health_dependency_failure_mask = COALESCE(perps_lp_settlement_observations.health_dependency_failure_mask, EXCLUDED.health_dependency_failure_mask), \
    \ observation_complete = COALESCE(perps_lp_settlement_observations.observation_complete, EXCLUDED.observation_complete), \
    \ has_matured_work = COALESCE(perps_lp_settlement_observations.has_matured_work, EXCLUDED.has_matured_work), \
    \ lp_epoch_settlement_paused = COALESCE(perps_lp_settlement_observations.lp_epoch_settlement_paused, EXCLUDED.lp_epoch_settlement_paused), \
    \ last_observed_at = NOW() \
    \WHERE perps_lp_settlement_observations.epoch = EXCLUDED.epoch \
    \ AND perps_lp_settlement_observations.observed_block = EXCLUDED.observed_block \
    \ AND perps_lp_settlement_observations.execution_path = EXCLUDED.execution_path \
    \ AND perps_lp_settlement_observations.operational_blocker_mask = EXCLUDED.operational_blocker_mask \
    \ AND perps_lp_settlement_observations.warning_mask = EXCLUDED.warning_mask \
    \ AND perps_lp_settlement_observations.dependency_failure_mask = EXCLUDED.dependency_failure_mask \
    \ AND perps_lp_settlement_observations.critical_fault_mask = EXCLUDED.critical_fault_mask \
    \ AND (perps_lp_settlement_observations.observed_block_hash IS NULL OR perps_lp_settlement_observations.observed_block_hash IS NOT DISTINCT FROM EXCLUDED.observed_block_hash) \
    \ AND (perps_lp_settlement_observations.schema_version IS NULL OR perps_lp_settlement_observations.schema_version = EXCLUDED.schema_version) \
    \ AND (perps_lp_settlement_observations.health_state IS NULL OR perps_lp_settlement_observations.health_state = EXCLUDED.health_state) \
    \ AND (perps_lp_settlement_observations.execution_path_dependency_mask IS NULL OR perps_lp_settlement_observations.execution_path_dependency_mask = EXCLUDED.execution_path_dependency_mask) \
    \ AND (perps_lp_settlement_observations.status_dependency_failure_mask IS NULL OR perps_lp_settlement_observations.status_dependency_failure_mask = EXCLUDED.status_dependency_failure_mask) \
    \ AND (perps_lp_settlement_observations.health_dependency_failure_mask IS NULL OR perps_lp_settlement_observations.health_dependency_failure_mask = EXCLUDED.health_dependency_failure_mask) \
    \ AND (perps_lp_settlement_observations.observation_complete IS NULL OR perps_lp_settlement_observations.observation_complete = EXCLUDED.observation_complete) \
    \ AND (perps_lp_settlement_observations.has_matured_work IS NULL OR perps_lp_settlement_observations.has_matured_work = EXCLUDED.has_matured_work) \
    \ AND (perps_lp_settlement_observations.lp_epoch_settlement_paused IS NULL OR perps_lp_settlement_observations.lp_epoch_settlement_paused = EXCLUDED.lp_epoch_settlement_paused)"
    (normalizeLpSettlementObservation input)
  unless (affected == 1) $
    ioError (userError "LP settlement observation digest collision: stored observation differs")

normalizeLpSettlementObservation :: LpSettlementObservationInput -> LpSettlementObservationInput
normalizeLpSettlementObservation input =
  input
    { lsoiMonitorAddress = normalizeHexText (lsoiMonitorAddress input)
    , lsoiObservationDigest = normalizeHexText (lsoiObservationDigest input)
    , lsoiObservedBlockHash = normalizeHexText <$> lsoiObservedBlockHash input
    }

-- | Everything necessary to deterministically rebroadcast a transaction is
-- committed before the first RPC send.
data LpSettlementSignedIntent = LpSettlementSignedIntent
  { lssiChainId :: Integer
  , lssiMonitorAddress :: Text
  , lssiObservationDigest :: Text
  , lssiEpoch :: Integer
  , lssiSignerAddress :: Text
  , lssiNonce :: Integer
  , lssiTargetAddress :: Text
  , lssiValue :: Integer
  , lssiCalldata :: ByteString
  , lssiGasLimit :: Integer
  , lssiMaxPriorityFeePerGas :: Integer
  , lssiMaxFeePerGas :: Integer
  , lssiSignedRawTransaction :: ByteString
  , lssiSignedTransactionHash :: Text
  }
  deriving stock (Eq, Show, Generic)

instance ToRow LpSettlementSignedIntent where
  toRow LpSettlementSignedIntent{..} =
    [ toField lssiChainId
    , toField lssiMonitorAddress
    , toField lssiObservationDigest
    , toField lssiEpoch
    , toField lssiSignerAddress
    , toField lssiNonce
    , toField lssiTargetAddress
    , toField lssiValue
    , toField $ Binary lssiCalldata
    , toField lssiGasLimit
    , toField lssiMaxPriorityFeePerGas
    , toField lssiMaxFeePerGas
    , toField $ Binary lssiSignedRawTransaction
    , toField lssiSignedTransactionHash
    ]

data LpSettlementTransactionRow = LpSettlementTransactionRow
  { lstrId :: Integer
  , lstrChainId :: Integer
  , lstrMonitorAddress :: Text
  , lstrObservationDigest :: Text
  , lstrEpoch :: Integer
  , lstrReplacementCount :: Int
  , lstrReplacesAttemptId :: Maybe Integer
  , lstrSignerAddress :: Text
  , lstrNonce :: Integer
  , lstrTargetAddress :: Text
  , lstrValue :: Integer
  , lstrCalldata :: ByteString
  , lstrGasLimit :: Integer
  , lstrMaxPriorityFeePerGas :: Integer
  , lstrMaxFeePerGas :: Integer
  , lstrSignedRawTransaction :: ByteString
  , lstrSignedTransactionHash :: Text
  , lstrStatus :: Text
  , lstrLastError :: Maybe Text
  , lstrReceiptTransactionHash :: Maybe Text
  , lstrReceiptBlockNumber :: Maybe Integer
  , lstrReceiptBlockHash :: Maybe Text
  , lstrReceiptSucceeded :: Maybe Bool
  , lstrConfirmedAt :: Maybe UTCTime
  , lstrConfirmationDepth :: Maybe Int
  , lstrSettlementEventLogIndex :: Maybe Integer
  , lstrCutoffEpoch :: Maybe Integer
  , lstrSeniorRedeemAssets :: Maybe Integer
  , lstrJuniorRedeemAssets :: Maybe Integer
  , lstrJuniorDepositAssets :: Maybe Integer
  , lstrSeniorDepositAssets :: Maybe Integer
  , lstrSeniorBacklog :: Maybe Bool
  , lstrJuniorBacklog :: Maybe Bool
  , lstrEntriesDeferred :: Maybe Bool
  , lstrCreatedAt :: UTCTime
  , lstrUpdatedAt :: UTCTime
  }
  deriving stock (Eq, Show, Generic)

instance FromRow LpSettlementTransactionRow where
  fromRow =
    LpSettlementTransactionRow
      <$> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> numericRequiredIntegerField
      <*> field
      <*> numericRequiredIntegerField
      <*> field
      <*> numericRequiredIntegerField
      <*> numericRequiredIntegerField
      <*> numericRequiredIntegerField
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> numericIntegerField
      <*> numericIntegerField
      <*> numericIntegerField
      <*> numericIntegerField
      <*> numericIntegerField
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field

lpSettlementTransactionSelectSql :: Query
lpSettlementTransactionSelectSql =
  "SELECT id, chain_id, monitor_address, observation_digest, epoch, replacement_count, \
  \replaces_attempt_id, signer_address, tx_nonce, target_address, tx_value, calldata, \
  \gas_limit, max_priority_fee_per_gas, max_fee_per_gas, signed_raw_transaction, \
  \signed_transaction_hash, status, last_error, receipt_transaction_hash, \
  \receipt_block_number, receipt_block_hash, receipt_succeeded, confirmed_at, \
  \confirmation_depth, settlement_event_log_index, cutoff_epoch, senior_redeem_assets, \
  \junior_redeem_assets, junior_deposit_assets, senior_deposit_assets, senior_backlog, \
  \junior_backlog, entries_deferred, created_at, updated_at \
  \FROM perps_lp_settlement_transactions"

prepareLpSettlementTransaction
  :: Connection
  -> LpSettlementSignedIntent
  -> IO LpSettlementTransactionRow
prepareLpSettlementTransaction conn signedIntent = do
  rows <- query conn
    "INSERT INTO perps_lp_settlement_transactions \
    \(chain_id, monitor_address, observation_digest, epoch, signer_address, tx_nonce, target_address, \
    \ tx_value, calldata, gas_limit, max_priority_fee_per_gas, max_fee_per_gas, \
    \ signed_raw_transaction, signed_transaction_hash, status) \
    \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, 'prepared') \
    \RETURNING id, chain_id, monitor_address, observation_digest, epoch, replacement_count, \
    \ replaces_attempt_id, signer_address, tx_nonce, target_address, tx_value, calldata, \
    \ gas_limit, max_priority_fee_per_gas, max_fee_per_gas, signed_raw_transaction, \
    \ signed_transaction_hash, status, last_error, receipt_transaction_hash, \
    \ receipt_block_number, receipt_block_hash, receipt_succeeded, confirmed_at, \
    \ confirmation_depth, settlement_event_log_index, cutoff_epoch, senior_redeem_assets, \
    \ junior_redeem_assets, junior_deposit_assets, senior_deposit_assets, senior_backlog, \
    \ junior_backlog, entries_deferred, created_at, updated_at"
    (normalizeLpSettlementSignedIntent signedIntent)
  requireExactlyOne "preparing LP settlement transaction" rows

normalizeLpSettlementSignedIntent :: LpSettlementSignedIntent -> LpSettlementSignedIntent
normalizeLpSettlementSignedIntent signedIntent =
  signedIntent
    { lssiMonitorAddress = normalizeHexText (lssiMonitorAddress signedIntent)
    , lssiObservationDigest = normalizeHexText (lssiObservationDigest signedIntent)
    , lssiSignerAddress = normalizeHexText (lssiSignerAddress signedIntent)
    , lssiTargetAddress = normalizeHexText (lssiTargetAddress signedIntent)
    , lssiSignedTransactionHash = normalizeHexText (lssiSignedTransactionHash signedIntent)
    }

replaceLpSettlementTransaction
  :: Connection
  -> Integer
  -> Integer
  -> Integer
  -> ByteString
  -> Text
  -> IO LpSettlementTransactionRow
replaceLpSettlementTransaction conn attemptId newPriorityFee newMaxFee newSignedRaw newSignedHash =
  withTransaction conn $ do
    lockLpSettlementNonceFamily conn attemptId
    existingRows <- query conn
      (lpSettlementTransactionSelectSql <> " WHERE id = ? AND status IN ('prepared', 'broadcast', 'pending') FOR UPDATE")
      (Only attemptId)
    existing <- requireExactlyOne "locking replaceable LP settlement transaction" existingRows
    unless
      ( newPriorityFee >= lstrMaxPriorityFeePerGas existing
          && newMaxFee >= lstrMaxFeePerGas existing
          && (newPriorityFee > lstrMaxPriorityFeePerGas existing || newMaxFee > lstrMaxFeePerGas existing)
      ) $
      ioError (userError "replacement fees must monotonically increase, with at least one strict increase")
    replaced <- execute conn
      "UPDATE perps_lp_settlement_transactions SET status = 'replaced', last_error = NULL, updated_at = NOW() \
      \WHERE id = ? AND status IN ('prepared', 'broadcast', 'pending')"
      (Only attemptId)
    unless (replaced == 1) $
      ioError (userError "LP settlement transaction was no longer replaceable")
    rows <- query conn
      "INSERT INTO perps_lp_settlement_transactions \
      \(chain_id, monitor_address, observation_digest, epoch, replacement_count, replaces_attempt_id, \
      \ signer_address, tx_nonce, target_address, tx_value, calldata, gas_limit, \
      \ max_priority_fee_per_gas, max_fee_per_gas, signed_raw_transaction, \
      \ signed_transaction_hash, status) \
      \SELECT chain_id, monitor_address, observation_digest, epoch, replacement_count + 1, id, \
      \ signer_address, tx_nonce, target_address, tx_value, calldata, gas_limit, ?, ?, ?, ?, 'prepared' \
      \FROM perps_lp_settlement_transactions WHERE id = ? \
      \RETURNING id, chain_id, monitor_address, observation_digest, epoch, replacement_count, \
      \ replaces_attempt_id, signer_address, tx_nonce, target_address, tx_value, calldata, \
      \ gas_limit, max_priority_fee_per_gas, max_fee_per_gas, signed_raw_transaction, \
      \ signed_transaction_hash, status, last_error, receipt_transaction_hash, \
      \ receipt_block_number, receipt_block_hash, receipt_succeeded, confirmed_at, \
      \ confirmation_depth, settlement_event_log_index, cutoff_epoch, senior_redeem_assets, \
      \ junior_redeem_assets, junior_deposit_assets, senior_deposit_assets, senior_backlog, \
      \ junior_backlog, entries_deferred, created_at, updated_at"
      (newPriorityFee, newMaxFee, Binary newSignedRaw, normalizeHexText newSignedHash, attemptId)
    requireExactlyOne "inserting LP settlement replacement" rows

getActiveLpSettlementTransaction
  :: Connection
  -> Integer
  -> Text
  -> Text
  -> IO (Maybe LpSettlementTransactionRow)
getActiveLpSettlementTransaction conn chainId monitorAddress signerAddress = do
  rows <- query conn
    (lpSettlementTransactionSelectSql <> " WHERE chain_id = ? AND (monitor_address = ? OR signer_address = ?) AND status IN ('prepared', 'broadcast', 'pending', 'confirming', 'manual_review')")
    (chainId, normalizeHexText monitorAddress, normalizeHexText signerAddress)
  requireAtMostOne "loading active LP settlement transaction" rows

getLpSettlementTransactionById
  :: Connection
  -> Integer
  -> IO (Maybe LpSettlementTransactionRow)
getLpSettlementTransactionById conn attemptId = do
  rows <- query conn
    (lpSettlementTransactionSelectSql <> " WHERE id = ?")
    (Only attemptId)
  requireAtMostOne "loading LP settlement transaction" rows

-- | All signed hashes that could consume the same signer nonce, newest first.
-- Keepers reconcile the whole family because a predecessor may be mined while
-- its higher-fee replacement is being propagated.
getLpSettlementTransactionFamily
  :: Connection
  -> Integer
  -> IO [LpSettlementTransactionRow]
getLpSettlementTransactionFamily conn attemptId =
  query conn
    ( lpSettlementTransactionSelectSql
        <> " WHERE (chain_id, signer_address, tx_nonce) = \
           \(SELECT chain_id, signer_address, tx_nonce \
           \ FROM perps_lp_settlement_transactions WHERE id = ?) \
           \ORDER BY id DESC"
    )
    (Only attemptId)

-- | Resolve the immutable block at which an attempt's safety observation was
-- pinned. This is intentionally keyed by attempt id so callers cannot
-- accidentally combine evidence from a different observation digest.
getLpSettlementObservationObservedBlock
  :: Connection
  -> Integer
  -> IO (Maybe Integer)
getLpSettlementObservationObservedBlock conn attemptId = do
  rows <- query conn
    "SELECT observation.observed_block \
    \FROM perps_lp_settlement_transactions transaction \
    \JOIN perps_lp_settlement_observations observation \
    \ ON observation.chain_id = transaction.chain_id \
    \ AND observation.monitor_address = transaction.monitor_address \
    \ AND observation.observation_digest = transaction.observation_digest \
    \ AND observation.epoch = transaction.epoch \
    \WHERE transaction.id = ?"
    (Only attemptId) :: IO [Only Integer]
  case rows of
    [] -> pure Nothing
    [Only observedBlock] -> pure $ Just observedBlock
    _ -> ioError $ userError "multiple observations found for LP settlement transaction"

getLatestSuccessfulLpSettlementAt
  :: Connection
  -> Integer
  -> Text
  -> IO (Maybe UTCTime)
getLatestSuccessfulLpSettlementAt conn chainId monitorAddress = do
  rows <- query conn
    "SELECT MAX(confirmed_at) FROM perps_lp_settlement_transactions \
    \WHERE chain_id = ? AND monitor_address = ? AND status = 'confirmed_success'"
    (chainId, normalizeHexText monitorAddress) :: IO [Only (Maybe UTCTime)]
  case rows of
    [Only confirmedAt] -> pure confirmedAt
    _ -> ioError (userError "unexpected row count while loading latest successful LP settlement")

data LpSettlementBroadcastInput = LpSettlementBroadcastInput
  { lsbiAttemptId :: Integer
  , lsbiOutcome :: Text
  , lsbiReturnedTransactionHash :: Maybe Text
  , lsbiRpcError :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)

data LpSettlementBroadcastRow = LpSettlementBroadcastRow
  { lsbrId :: Integer
  , lsbrAttemptId :: Integer
  , lsbrBroadcastSequence :: Int
  , lsbrOutcome :: Text
  , lsbrReturnedTransactionHash :: Maybe Text
  , lsbrRpcError :: Maybe Text
  , lsbrBroadcastAt :: UTCTime
  }
  deriving stock (Eq, Show, Generic)

instance FromRow LpSettlementBroadcastRow where
  fromRow =
    LpSettlementBroadcastRow
      <$> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field

appendLpSettlementBroadcast
  :: Connection
  -> LpSettlementBroadcastInput
  -> IO LpSettlementBroadcastRow
appendLpSettlementBroadcast conn LpSettlementBroadcastInput{..} =
  withTransaction conn $ do
    unless (lsbiOutcome `elem` ["accepted", "already_known", "rejected", "ambiguous"]) $
      ioError (userError "invalid LP settlement broadcast outcome")
    attempts <- query conn
      "SELECT signed_transaction_hash FROM perps_lp_settlement_transactions \
      \WHERE id = ? AND status IN ('prepared', 'broadcast', 'pending') FOR UPDATE"
      (Only lsbiAttemptId) :: IO [Only Text]
    signedHash <- case attempts of
      [Only value] -> pure value
      _ -> ioError (userError "LP settlement transaction is not active")
    sequences <- query conn
      "SELECT COALESCE(MAX(broadcast_sequence), 0) + 1 \
      \FROM perps_lp_settlement_broadcasts WHERE attempt_id = ?"
      (Only lsbiAttemptId) :: IO [Only Int]
    sequenceNumber <- case sequences of
      [Only value] -> pure value
      _ -> ioError (userError "could not allocate LP settlement broadcast sequence")
    let normalizedReturnedHash = normalizeHexText <$> lsbiReturnedTransactionHash
        hashMismatch = maybe False (/= signedHash) normalizedReturnedHash
        (nextStatus, nextError)
          | hashMismatch = ("manual_review", Just "RPC returned a transaction hash different from the signed intent")
          | lsbiOutcome `elem` ["accepted", "already_known"] = ("broadcast", Nothing)
          | lsbiOutcome == "rejected" = ("pending", lsbiRpcError)
          | otherwise = ("pending", lsbiRpcError)
    rows <- query conn
      "INSERT INTO perps_lp_settlement_broadcasts \
      \(attempt_id, broadcast_sequence, outcome, returned_transaction_hash, rpc_error) \
      \VALUES (?, ?, ?, ?, ?) \
      \RETURNING id, attempt_id, broadcast_sequence, outcome, returned_transaction_hash, rpc_error, broadcast_at"
      (lsbiAttemptId, sequenceNumber, lsbiOutcome, normalizedReturnedHash, lsbiRpcError)
    _ <- execute conn
      "UPDATE perps_lp_settlement_transactions SET status = ?, last_error = ?, updated_at = NOW() WHERE id = ?"
      (nextStatus :: Text, nextError, lsbiAttemptId)
    requireExactlyOne "appending LP settlement broadcast" rows

getLpSettlementBroadcasts :: Connection -> Integer -> IO [LpSettlementBroadcastRow]
getLpSettlementBroadcasts conn attemptId =
  query conn
    "SELECT id, attempt_id, broadcast_sequence, outcome, returned_transaction_hash, rpc_error, broadcast_at \
    \FROM perps_lp_settlement_broadcasts WHERE attempt_id = ? ORDER BY broadcast_sequence ASC"
    (Only attemptId)

markLpSettlementTransactionPending :: Connection -> Integer -> IO ()
markLpSettlementTransactionPending conn attemptId = do
  affected <- execute conn
    "UPDATE perps_lp_settlement_transactions SET status = 'pending', last_error = NULL, \
    \receipt_transaction_hash = NULL, receipt_block_number = NULL, receipt_block_hash = NULL, \
    \receipt_succeeded = NULL, confirmed_at = NULL, confirmation_depth = NULL, \
    \settlement_event_log_index = NULL, cutoff_epoch = NULL, senior_redeem_assets = NULL, \
    \junior_redeem_assets = NULL, junior_deposit_assets = NULL, senior_deposit_assets = NULL, \
    \senior_backlog = NULL, junior_backlog = NULL, entries_deferred = NULL, updated_at = NOW() \
    \WHERE id = ? AND status IN ('broadcast', 'pending', 'confirming')"
    (Only attemptId)
  unless (affected == 1) $
    ioError (userError "LP settlement transaction is not eligible to become pending")

markLpSettlementTransactionConfirming
  :: Connection
  -> Integer
  -> Text
  -> Integer
  -> Text
  -> Bool
  -> Int
  -> IO ()
markLpSettlementTransactionConfirming conn attemptId receiptTxHash receiptBlockNumber receiptBlockHash receiptSucceeded confirmationDepth = do
  affected <- execute conn
    "UPDATE perps_lp_settlement_transactions SET \
    \status = CASE WHEN status = 'manual_review' THEN 'manual_review' WHEN signed_transaction_hash = ? THEN 'confirming' ELSE 'manual_review' END, \
    \last_error = CASE WHEN status = 'manual_review' THEN last_error WHEN signed_transaction_hash = ? THEN NULL ELSE 'receipt transaction hash differs from signed intent' END, \
    \receipt_transaction_hash = ?, receipt_block_number = ?, receipt_block_hash = ?, \
    \receipt_succeeded = ?, confirmed_at = NULL, confirmation_depth = ?, \
    \settlement_event_log_index = NULL, cutoff_epoch = NULL, senior_redeem_assets = NULL, \
    \junior_redeem_assets = NULL, junior_deposit_assets = NULL, senior_deposit_assets = NULL, \
    \senior_backlog = NULL, junior_backlog = NULL, entries_deferred = NULL, updated_at = NOW() \
    \WHERE id = ? AND status IN ('broadcast', 'pending', 'confirming', 'manual_review')"
    ( normalizedReceiptHash
    , normalizedReceiptHash
    , normalizedReceiptHash
    , receiptBlockNumber
    , normalizeHexText receiptBlockHash
    , receiptSucceeded
    , confirmationDepth
    , attemptId
    )
  unless (affected == 1) $
    ioError (userError "LP settlement transaction is not active")
  where
    normalizedReceiptHash = normalizeHexText receiptTxHash

-- | Clear receipt evidence invalidated by a canonical block-hash mismatch.
-- Manual-review and replaced family history retain their status/diagnostic;
-- only a normal confirming row returns to pending reconciliation.
clearLpSettlementReorgedReceiptEvidence :: Connection -> Integer -> IO ()
clearLpSettlementReorgedReceiptEvidence conn attemptId = do
  affected <- execute conn
    "UPDATE perps_lp_settlement_transactions SET \
    \status = CASE WHEN status = 'confirming' THEN 'pending' ELSE status END, \
    \last_error = CASE WHEN status = 'confirming' THEN NULL ELSE last_error END, \
    \receipt_transaction_hash = NULL, receipt_block_number = NULL, receipt_block_hash = NULL, \
    \receipt_succeeded = NULL, confirmed_at = NULL, confirmation_depth = NULL, \
    \settlement_event_log_index = NULL, cutoff_epoch = NULL, senior_redeem_assets = NULL, \
    \junior_redeem_assets = NULL, junior_deposit_assets = NULL, senior_deposit_assets = NULL, \
    \senior_backlog = NULL, junior_backlog = NULL, entries_deferred = NULL, updated_at = NOW() \
    \WHERE id = ? AND status IN ('confirming', 'manual_review', 'replaced')"
    (Only attemptId)
  unless (affected == 1) $
    ioError (userError "LP settlement transaction has no reorgable receipt evidence")

markLpSettlementTransactionManualReview :: Connection -> Integer -> Text -> IO ()
markLpSettlementTransactionManualReview conn attemptId reason = do
  affected <- execute conn
    "UPDATE perps_lp_settlement_transactions SET status = 'manual_review', last_error = ?, updated_at = NOW() \
    \WHERE id = ? AND status IN ('prepared', 'broadcast', 'pending', 'confirming', 'manual_review')"
    (reason, attemptId)
  unless (affected == 1) $
    ioError (userError "LP settlement transaction is not active")

data LpSettlementEventOutcome = LpSettlementEventOutcome
  { lseoLogIndex :: Integer
  , lseoCutoffEpoch :: Integer
  , lseoSeniorRedeemAssets :: Integer
  , lseoJuniorRedeemAssets :: Integer
  , lseoJuniorDepositAssets :: Integer
  , lseoSeniorDepositAssets :: Integer
  , lseoSeniorBacklog :: Bool
  , lseoJuniorBacklog :: Bool
  , lseoEntriesDeferred :: Bool
  }
  deriving stock (Eq, Show, Generic)

data LpSettlementReceiptInput = LpSettlementReceiptInput
  { lsriAttemptId :: Integer
  , lsriTransactionHash :: Text
  , lsriBlockNumber :: Integer
  , lsriBlockHash :: Text
  , lsriSucceeded :: Bool
  , lsriConfirmationDepth :: Int
  , lsriEventOutcome :: Maybe LpSettlementEventOutcome
  }
  deriving stock (Eq, Show, Generic)

lockLpSettlementReceiptFamily
  :: Connection
  -> Integer
  -> IO [LpSettlementReceiptTarget]
lockLpSettlementReceiptFamily conn attemptId =
  query conn
    "SELECT member.id, member.signed_transaction_hash, member.status, \
    \member.receipt_transaction_hash, member.receipt_block_number, member.receipt_block_hash, \
    \member.receipt_succeeded, member.confirmation_depth, member.settlement_event_log_index, \
    \member.confirmed_at \
    \FROM perps_lp_settlement_transactions seed \
    \JOIN perps_lp_settlement_transactions member \
    \ ON member.chain_id = seed.chain_id \
    \ AND member.signer_address = seed.signer_address \
    \ AND member.tx_nonce = seed.tx_nonce \
    \WHERE seed.id = ? ORDER BY member.id FOR UPDATE OF member"
    (Only attemptId)

-- | Serialize every mutation that can change membership or terminal evidence
-- for a signer/nonce family. Row locks alone are insufficient here: under
-- READ COMMITTED a receipt query can wait on a predecessor while retaining a
-- statement snapshot that predates a concurrently inserted replacement.
-- This helper must be called from inside the surrounding SQL transaction.
lockLpSettlementNonceFamily :: Connection -> Integer -> IO ()
lockLpSettlementNonceFamily conn attemptId = do
  rows <- query conn
    "SELECT 1::BIGINT FROM (SELECT pg_advisory_xact_lock(hashtextextended(\
    \ concat_ws(':', chain_id::text, signer_address, tx_nonce::text), 0)) \
    \ FROM perps_lp_settlement_transactions WHERE id = ?) locked"
    (Only attemptId) :: IO [Only Integer]
  case rows of
    [Only 1] -> pure ()
    [_] -> ioError (userError "failed to acquire LP settlement signer/nonce transaction lock")
    [] -> ioError (userError "LP settlement transaction does not exist")
    _ -> ioError (userError "multiple LP settlement transactions matched signer/nonce lock seed")

requireLpSettlementReceiptTarget
  :: Integer
  -> [LpSettlementReceiptTarget]
  -> IO LpSettlementReceiptTarget
requireLpSettlementReceiptTarget attemptId family =
  maybe
    (ioError $ userError "LP settlement transaction is not receipt-reconcilable")
    pure
    (find ((== attemptId) . lsrtId) family)

getActiveLpSettlementSibling
  :: Integer
  -> [LpSettlementReceiptTarget]
  -> IO (Maybe LpSettlementReceiptTarget)
getActiveLpSettlementSibling attemptId family =
  case filter (\row -> lsrtId row /= attemptId && isActiveLpSettlementStatus (lsrtStatus row)) family of
    [] -> pure Nothing
    [row] -> pure $ Just row
    _ -> ioError (userError "multiple active LP settlement replacements found")

receiptEvidenceIsEmptyOrMatches
  :: LpSettlementReceiptTarget
  -> LpSettlementReceiptInput
  -> Bool
receiptEvidenceIsEmptyOrMatches target LpSettlementReceiptInput{..} =
  case lsrtReceiptTransactionHash target of
    Nothing ->
      lsrtReceiptBlockNumber target == Nothing
        && lsrtReceiptBlockHash target == Nothing
        && lsrtReceiptSucceeded target == Nothing
        && lsrtConfirmationDepth target == Nothing
        && lsrtSettlementEventLogIndex target == Nothing
    Just existingHash ->
      existingHash == normalizeHexText lsriTransactionHash
        && lsrtReceiptBlockNumber target == Just lsriBlockNumber
        && lsrtReceiptBlockHash target == Just (normalizeHexText lsriBlockHash)
        && lsrtReceiptSucceeded target == Just lsriSucceeded
        && maybe False (<= lsriConfirmationDepth) (lsrtConfirmationDepth target)
        && lsrtSettlementEventLogIndex target == Nothing

ensureNonTerminalCanonicalReceipt
  :: LpSettlementReceiptTarget
  -> [LpSettlementReceiptTarget]
  -> LpSettlementReceiptInput
  -> IO ()
ensureNonTerminalCanonicalReceipt target family receipt@LpSettlementReceiptInput{..} = do
  when (maybe False (const True) lsriEventOutcome) $
    ioError (userError "manual or superseded LP settlement receipt must not include a settlement event")
  unless (normalizeHexText lsriTransactionHash == lsrtSignedTransactionHash target) $
    ioError (userError "LP settlement receipt transaction hash differs from signed intent")
  unless (receiptEvidenceIsEmptyOrMatches target receipt) $
    ioError (userError "LP settlement receipt conflicts with previously persisted evidence")
  unless (isActiveLpSettlementStatus (lsrtStatus target) || isHistoricalLpSettlementStatus (lsrtStatus target)) $
    ioError (userError "LP settlement transaction already has terminal receipt evidence")
  when
    (any (\row -> lsrtId row /= lsriAttemptId && isTerminalLpSettlementReceipt row) family)
    (ioError $ userError "another same-nonce LP settlement transaction already has terminal receipt evidence")

recordLpSettlementReceipt :: Connection -> LpSettlementReceiptInput -> IO ()
recordLpSettlementReceipt conn receipt@LpSettlementReceiptInput{..}
  | not (lsriSucceeded && maybe False (const True) lsriEventOutcome) =
      ioError (userError "canonical LP settlement receipt must be successful and include its settlement event")
  | otherwise = recordSuccessfulLpSettlementReceipt conn receipt

recordSuccessfulLpSettlementReceipt :: Connection -> LpSettlementReceiptInput -> IO ()
recordSuccessfulLpSettlementReceipt conn LpSettlementReceiptInput{..} =
  withTransaction conn $ do
    lockLpSettlementNonceFamily conn lsriAttemptId
    family <- lockLpSettlementReceiptFamily conn lsriAttemptId
    target <- requireLpSettlementReceiptTarget lsriAttemptId family
    activeSibling <- getActiveLpSettlementSibling lsriAttemptId family
    let normalizedReceiptHash = normalizeHexText lsriTransactionHash
        hashMatches = normalizedReceiptHash == lsrtSignedTransactionHash target
        eventShapeMatches = lsriSucceeded == maybe False (const True) lsriEventOutcome
        terminalWinnerExists =
          any
            (\row -> lsrtId row /= lsriAttemptId && isTerminalLpSettlementReceipt row)
            family
        terminal = hashMatches && eventShapeMatches && not terminalWinnerExists
        nextStatus
          | not terminal = "manual_review"
          | lsriSucceeded = "confirmed_success"
          | otherwise = "confirmed_revert"
        persistedStatus
          | isHistoricalLpSettlementStatus (lsrtStatus target)
              && not terminal
              && maybe False (const True) activeSibling = lsrtStatus target
          | otherwise = nextStatus
        nextError
          | terminalWinnerExists = Just "another same-nonce transaction is already terminal"
          | not hashMatches = Just "receipt transaction hash differs from signed intent"
          | not eventShapeMatches = Just "receipt success/event outcome mismatch"
          | otherwise = Nothing
        eventValues = maybe emptyLpSettlementEventOutcome id lsriEventOutcome
        params = LpSettlementReceiptUpdate
          { lsruTransactionHash = normalizedReceiptHash
          , lsruBlockNumber = lsriBlockNumber
          , lsruBlockHash = normalizeHexText lsriBlockHash
          , lsruSucceeded = lsriSucceeded
          , lsruConfirmationDepth = lsriConfirmationDepth
          , lsruEventOutcome = eventValues
          , lsruHasEvent = maybe False (const True) lsriEventOutcome
          , lsruStatus = persistedStatus
          , lsruLastError = nextError
          , lsruIsTerminal = terminal
          , lsruAttemptId = lsriAttemptId
          }
    affected <- execute conn
      "UPDATE perps_lp_settlement_transactions SET receipt_transaction_hash = ?, \
      \receipt_block_number = ?, receipt_block_hash = ?, receipt_succeeded = ?, \
      \confirmation_depth = ?, settlement_event_log_index = CASE WHEN ? THEN ? ELSE NULL END, \
      \cutoff_epoch = CASE WHEN ? THEN ? ELSE NULL END, \
      \senior_redeem_assets = CASE WHEN ? THEN ? ELSE NULL END, \
      \junior_redeem_assets = CASE WHEN ? THEN ? ELSE NULL END, \
      \junior_deposit_assets = CASE WHEN ? THEN ? ELSE NULL END, \
      \senior_deposit_assets = CASE WHEN ? THEN ? ELSE NULL END, \
      \senior_backlog = CASE WHEN ? THEN ? ELSE NULL END, \
      \junior_backlog = CASE WHEN ? THEN ? ELSE NULL END, \
      \entries_deferred = CASE WHEN ? THEN ? ELSE NULL END, \
      \status = ?, last_error = ?, confirmed_at = CASE WHEN ? THEN COALESCE(confirmed_at, NOW()) ELSE NULL END, updated_at = NOW() \
      \WHERE id = ?"
      params
    unless (affected == 1) $
      ioError (userError "LP settlement transaction disappeared while recording receipt")
    when (isHistoricalLpSettlementStatus $ lsrtStatus target) $
      case activeSibling of
        Nothing -> pure ()
        Just sibling -> do
          let successorStatus = if terminal then "superseded" else "manual_review"
              successorReason
                | terminal = "same-nonce predecessor reached a terminal receipt"
                | otherwise = fromMaybe "same-nonce predecessor receipt requires manual review" nextError
          successorCount <- execute conn
            "UPDATE perps_lp_settlement_transactions SET status = ?, last_error = ?, updated_at = NOW() \
            \WHERE id = ? AND status IN ('prepared', 'broadcast', 'pending', 'confirming', 'manual_review')"
            (successorStatus :: Text, successorReason, lsrtId sibling)
          unless (successorCount == 1) $
            ioError (userError "active LP settlement replacement changed during receipt reconciliation")

-- | Preserve an invalid or unexpected canonical receipt without releasing the
-- signer/monitor lane. If the receipt belongs to a replaced predecessor, its
-- active replacement becomes the blocking manual-review row atomically.
recordLpSettlementReceiptForManualReview
  :: Connection
  -> LpSettlementReceiptInput
  -> Text
  -> IO ()
recordLpSettlementReceiptForManualReview conn receipt@LpSettlementReceiptInput{..} reason =
  withTransaction conn $ do
    lockLpSettlementNonceFamily conn lsriAttemptId
    family <- lockLpSettlementReceiptFamily conn lsriAttemptId
    target <- requireLpSettlementReceiptTarget lsriAttemptId family
    activeSibling <- getActiveLpSettlementSibling lsriAttemptId family
    ensureNonTerminalCanonicalReceipt target family receipt
    let targetWasHistorical = isHistoricalLpSettlementStatus $ lsrtStatus target
        targetStatus =
          if targetWasHistorical && maybe False (const True) activeSibling
            then lsrtStatus target
            else "manual_review"
    affected <- execute conn
      "UPDATE perps_lp_settlement_transactions SET receipt_transaction_hash = ?, \
      \receipt_block_number = ?, receipt_block_hash = ?, receipt_succeeded = ?, \
      \confirmation_depth = ?, settlement_event_log_index = NULL, cutoff_epoch = NULL, \
      \senior_redeem_assets = NULL, junior_redeem_assets = NULL, junior_deposit_assets = NULL, \
      \senior_deposit_assets = NULL, senior_backlog = NULL, junior_backlog = NULL, \
      \entries_deferred = NULL, status = ?, last_error = ?, confirmed_at = NULL, updated_at = NOW() \
      \WHERE id = ?"
      ( normalizeHexText lsriTransactionHash
      , lsriBlockNumber
      , normalizeHexText lsriBlockHash
      , lsriSucceeded
      , lsriConfirmationDepth
      , targetStatus
      , reason
      , lsriAttemptId
      )
    unless (affected == 1) $
      ioError (userError "LP settlement transaction disappeared while preserving manual-review receipt")
    when targetWasHistorical $
      case activeSibling of
        Nothing -> pure ()
        Just sibling -> do
          successorCount <- execute conn
            "UPDATE perps_lp_settlement_transactions SET status = 'manual_review', last_error = ?, updated_at = NOW() \
            \WHERE id = ? AND status IN ('prepared', 'broadcast', 'pending', 'confirming', 'manual_review')"
            (reason, lsrtId sibling)
          unless (successorCount == 1) $
            ioError (userError "active LP settlement replacement changed during manual-review receipt reconciliation")

-- | Record a canonical reverted receipt whose revert is known to be benign
-- because permissionless settlement already cleared the work. The mined hash
-- is the receipt-backed superseded terminal row; an active same-nonce
-- replacement is made inactive in the same database transaction.
recordLpSettlementSupersededReceipt
  :: Connection
  -> LpSettlementReceiptInput
  -> Text
  -> IO ()
recordLpSettlementSupersededReceipt conn receipt@LpSettlementReceiptInput{..} reason =
  withTransaction conn $ do
    when lsriSucceeded $
      ioError (userError "superseded LP settlement receipt must be a revert")
    lockLpSettlementNonceFamily conn lsriAttemptId
    family <- lockLpSettlementReceiptFamily conn lsriAttemptId
    target <- requireLpSettlementReceiptTarget lsriAttemptId family
    activeSibling <- getActiveLpSettlementSibling lsriAttemptId family
    ensureNonTerminalCanonicalReceipt target family receipt
    affected <- execute conn
      "UPDATE perps_lp_settlement_transactions SET receipt_transaction_hash = ?, \
      \receipt_block_number = ?, receipt_block_hash = ?, receipt_succeeded = FALSE, \
      \confirmation_depth = ?, settlement_event_log_index = NULL, cutoff_epoch = NULL, \
      \senior_redeem_assets = NULL, junior_redeem_assets = NULL, junior_deposit_assets = NULL, \
      \senior_deposit_assets = NULL, senior_backlog = NULL, junior_backlog = NULL, \
      \entries_deferred = NULL, status = 'superseded', last_error = ?, \
      \confirmed_at = COALESCE(confirmed_at, NOW()), updated_at = NOW() WHERE id = ?"
      ( normalizeHexText lsriTransactionHash
      , lsriBlockNumber
      , normalizeHexText lsriBlockHash
      , lsriConfirmationDepth
      , reason
      , lsriAttemptId
      )
    unless (affected == 1) $
      ioError (userError "LP settlement transaction disappeared while preserving superseded receipt")
    case activeSibling of
      Nothing -> pure ()
      Just sibling -> do
        successorCount <- execute conn
          "UPDATE perps_lp_settlement_transactions SET status = 'superseded', last_error = ?, updated_at = NOW() \
          \WHERE id = ? AND status IN ('prepared', 'broadcast', 'pending', 'confirming', 'manual_review')"
          (reason, lsrtId sibling)
        unless (successorCount == 1) $
          ioError (userError "active LP settlement replacement changed during superseded receipt reconciliation")

isActiveLpSettlementStatus :: Text -> Bool
isActiveLpSettlementStatus status =
  status `elem` ["prepared", "broadcast", "pending", "confirming", "manual_review"]

isConfirmedLpSettlementStatus :: Text -> Bool
isConfirmedLpSettlementStatus status =
  status `elem` ["confirmed_success", "confirmed_revert"]

isTerminalLpSettlementReceipt :: LpSettlementReceiptTarget -> Bool
isTerminalLpSettlementReceipt row =
  isConfirmedLpSettlementStatus (lsrtStatus row)
    || (lsrtStatus row == "superseded" && maybe False (const True) (lsrtConfirmedAt row))

isHistoricalLpSettlementStatus :: Text -> Bool
isHistoricalLpSettlementStatus status =
  status `elem` ["replaced", "failed", "abandoned", "superseded"]

data LpSettlementReceiptTarget = LpSettlementReceiptTarget
  { lsrtId :: Integer
  , lsrtSignedTransactionHash :: Text
  , lsrtStatus :: Text
  , lsrtReceiptTransactionHash :: Maybe Text
  , lsrtReceiptBlockNumber :: Maybe Integer
  , lsrtReceiptBlockHash :: Maybe Text
  , lsrtReceiptSucceeded :: Maybe Bool
  , lsrtConfirmationDepth :: Maybe Int
  , lsrtSettlementEventLogIndex :: Maybe Integer
  , lsrtConfirmedAt :: Maybe UTCTime
  }

instance FromRow LpSettlementReceiptTarget where
  fromRow =
    LpSettlementReceiptTarget
      <$> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field

-- Internal flattened update payload. Dummy event values are never persisted
-- when 'lsruHasEvent' is false, keeping the all-or-none event constraint true.
data LpSettlementReceiptUpdate = LpSettlementReceiptUpdate
  { lsruTransactionHash :: Text
  , lsruBlockNumber :: Integer
  , lsruBlockHash :: Text
  , lsruSucceeded :: Bool
  , lsruConfirmationDepth :: Int
  , lsruEventOutcome :: LpSettlementEventOutcome
  , lsruHasEvent :: Bool
  , lsruStatus :: Text
  , lsruLastError :: Maybe Text
  , lsruIsTerminal :: Bool
  , lsruAttemptId :: Integer
  }

instance ToRow LpSettlementReceiptUpdate where
  toRow LpSettlementReceiptUpdate{lsruEventOutcome = LpSettlementEventOutcome{..}, ..} =
    [ toField lsruTransactionHash
    , toField lsruBlockNumber
    , toField lsruBlockHash
    , toField lsruSucceeded
    , toField lsruConfirmationDepth
    , toField lsruHasEvent
    , toField lseoLogIndex
    , toField lsruHasEvent
    , toField lseoCutoffEpoch
    , toField lsruHasEvent
    , toField lseoSeniorRedeemAssets
    , toField lsruHasEvent
    , toField lseoJuniorRedeemAssets
    , toField lsruHasEvent
    , toField lseoJuniorDepositAssets
    , toField lsruHasEvent
    , toField lseoSeniorDepositAssets
    , toField lsruHasEvent
    , toField lseoSeniorBacklog
    , toField lsruHasEvent
    , toField lseoJuniorBacklog
    , toField lsruHasEvent
    , toField lseoEntriesDeferred
    , toField lsruStatus
    , toField lsruLastError
    , toField lsruIsTerminal
    , toField lsruAttemptId
    ]

emptyLpSettlementEventOutcome :: LpSettlementEventOutcome
emptyLpSettlementEventOutcome =
  LpSettlementEventOutcome
    { lseoLogIndex = 0
    , lseoCutoffEpoch = 0
    , lseoSeniorRedeemAssets = 0
    , lseoJuniorRedeemAssets = 0
    , lseoJuniorDepositAssets = 0
    , lseoSeniorDepositAssets = 0
    , lseoSeniorBacklog = False
    , lseoJuniorBacklog = False
    , lseoEntriesDeferred = False
    }

requireExactlyOne :: String -> [a] -> IO a
requireExactlyOne _ [row] = pure row
requireExactlyOne context _ = ioError (userError ("unexpected row count while " <> context))

requireAtMostOne :: String -> [a] -> IO (Maybe a)
requireAtMostOne _ [] = pure Nothing
requireAtMostOne _ [row] = pure (Just row)
requireAtMostOne context _ = ioError (userError ("multiple rows while " <> context))

normalizeHexText :: Text -> Text
normalizeHexText = T.toLower . T.strip

keeperLockId :: Int
keeperLockId = 421614485

lpSettlementKeeperLockId :: Int
lpSettlementKeeperLockId = 421614486

tryPerpsKeeperLock :: Connection -> IO Bool
tryPerpsKeeperLock conn = do
  rows <- query conn "SELECT pg_try_advisory_lock(?)" (Only keeperLockId) :: IO [Only Bool]
  pure $ case rows of
    [Only acquired] -> acquired
    _ -> False

unlockPerpsKeeperLock :: Connection -> IO ()
unlockPerpsKeeperLock conn = do
  _ <- query conn "SELECT pg_advisory_unlock(?)" (Only keeperLockId) :: IO [Only Bool]
  pure ()

tryLpSettlementKeeperLock :: Connection -> IO Bool
tryLpSettlementKeeperLock conn = do
  rows <- query conn "SELECT pg_try_advisory_lock(?)" (Only lpSettlementKeeperLockId) :: IO [Only Bool]
  pure $ case rows of
    [Only acquired] -> acquired
    _ -> False

unlockLpSettlementKeeperLock :: Connection -> IO ()
unlockLpSettlementKeeperLock conn = do
  _ <- query conn "SELECT pg_advisory_unlock(?)" (Only lpSettlementKeeperLockId) :: IO [Only Bool]
  pure ()

getPerpsKeeperLastIndexedBlock :: Connection -> Text -> IO Integer
getPerpsKeeperLastIndexedBlock conn orderRouter = do
  result <- query conn
    "SELECT last_indexed_block FROM perps_keeper_state WHERE order_router = ?"
    (Only $ normalizeRouter orderRouter) :: IO [Only Integer]
  case result of
    [Only block] -> pure block
    _ -> pure 0

setPerpsKeeperLastIndexedBlock :: Connection -> Text -> Integer -> IO ()
setPerpsKeeperLastIndexedBlock conn orderRouter block = do
  _ <- execute conn
    "INSERT INTO perps_keeper_state (order_router, last_indexed_block, updated_at) \
    \VALUES (?, ?, NOW()) \
    \ON CONFLICT (order_router) DO UPDATE SET \
    \last_indexed_block = EXCLUDED.last_indexed_block, \
    \updated_at = NOW()"
    (normalizeRouter orderRouter, block)
  pure ()

upsertPerpsKeeperOrderCommitted
  :: Connection
  -> Text    -- order_router
  -> Integer -- order_id
  -> Text    -- account
  -> Integer -- side
  -> Integer -- commit_block
  -> Integer -- commit_event_block
  -> Integer -- commit_time
  -> Text    -- commit_tx_hash
  -> IO ()
upsertPerpsKeeperOrderCommitted conn orderRouter orderId account side commitBlock commitEventBlock commitTime commitTxHash = do
  _ <- execute conn
    "INSERT INTO perps_keeper_orders \
    \(order_router, order_id, account, side, commit_block, commit_event_block, commit_time, commit_tx_hash, status) \
    \VALUES (?, ?, ?, ?, ?, ?, ?, ?, 'pending') \
    \ON CONFLICT (order_router, order_id) DO UPDATE SET \
    \commit_event_block = COALESCE(perps_keeper_orders.commit_event_block, EXCLUDED.commit_event_block)"
    (normalizeRouter orderRouter, orderId, T.toLower account, side, commitBlock, commitEventBlock, commitTime, T.toLower commitTxHash)
  pure ()

markPerpsKeeperOrderExecuted
  :: Connection
  -> Text    -- order_router
  -> Integer -- order_id
  -> Text    -- tx_hash
  -> Integer -- block_number
  -> Integer -- execution_price
  -> IO ()
markPerpsKeeperOrderExecuted conn orderRouter orderId txHash blockNumber executionPrice = do
  _ <- execute conn
    "UPDATE perps_keeper_orders SET \
    \status = 'executed', \
    \execution_tx_hash = ?, \
    \execution_block = ?, \
    \execution_price = ?, \
    \last_error = NULL, \
    \updated_at = NOW() \
    \WHERE order_router = ? AND order_id = ?"
    (T.toLower txHash, blockNumber, executionPrice, normalizeRouter orderRouter, orderId)
  pure ()

markPerpsKeeperOrderFailed
  :: Connection
  -> Text    -- order_router
  -> Integer -- order_id
  -> Text    -- tx_hash
  -> Integer -- block_number
  -> Integer -- failure_reason
  -> IO ()
markPerpsKeeperOrderFailed conn orderRouter orderId txHash blockNumber failureReason = do
  _ <- execute conn
    "UPDATE perps_keeper_orders SET \
    \status = 'failed', \
    \failure_tx_hash = ?, \
    \failure_block = ?, \
    \failure_reason = ?, \
    \last_error = NULL, \
    \updated_at = NOW() \
    \WHERE order_router = ? AND order_id = ?"
    (T.toLower txHash, blockNumber, failureReason, normalizeRouter orderRouter, orderId)
  pure ()

-- | Reconcile canonical lifecycle state when the keeper missed the terminal
-- event. The lifecycle outcome does not contain the Ethereum transaction hash,
-- so these updates intentionally leave the transaction-evidence columns
-- untouched instead of storing the receipt hash in them.
reconcilePerpsKeeperOrderExecuted
  :: Connection
  -> Text    -- order_router
  -> Integer -- order_id
  -> Integer -- terminal_block
  -> Integer -- execution_price
  -> IO ()
reconcilePerpsKeeperOrderExecuted conn orderRouter orderId blockNumber executionPrice = do
  _ <- execute conn
    "UPDATE perps_keeper_orders SET \
    \status = 'executed', \
    \execution_block = ?, \
    \execution_price = ?, \
    \last_error = NULL, \
    \updated_at = NOW() \
    \WHERE order_router = ? AND order_id = ?"
    (blockNumber, executionPrice, normalizeRouter orderRouter, orderId)
  pure ()

reconcilePerpsKeeperOrderFailed
  :: Connection
  -> Text    -- order_router
  -> Integer -- order_id
  -> Integer -- terminal_block
  -> Integer -- failure_reason
  -> IO ()
reconcilePerpsKeeperOrderFailed conn orderRouter orderId blockNumber failureReason = do
  _ <- execute conn
    "UPDATE perps_keeper_orders SET \
    \status = 'failed', \
    \failure_block = ?, \
    \failure_reason = ?, \
    \last_error = NULL, \
    \updated_at = NOW() \
    \WHERE order_router = ? AND order_id = ?"
    (blockNumber, failureReason, normalizeRouter orderRouter, orderId)
  pure ()

recordPerpsKeeperOrderAttempt :: Connection -> Text -> Integer -> IO ()
recordPerpsKeeperOrderAttempt conn orderRouter orderId = do
  _ <- execute conn
    "UPDATE perps_keeper_orders SET \
    \attempt_count = attempt_count + 1, \
    \last_attempt_at = NOW(), \
    \updated_at = NOW() \
    \WHERE order_router = ? AND order_id = ?"
    (normalizeRouter orderRouter, orderId)
  pure ()

recordPerpsKeeperOrderError :: Connection -> Text -> Integer -> Text -> IO ()
recordPerpsKeeperOrderError conn orderRouter orderId err = do
  _ <- execute conn
    "UPDATE perps_keeper_orders SET \
    \last_error = ?, \
    \updated_at = NOW() \
    \WHERE order_router = ? AND order_id = ?"
    (err, normalizeRouter orderRouter, orderId)
  pure ()

recordPerpsKeeperOrderImmediateRetryError :: Connection -> Text -> Integer -> Text -> IO ()
recordPerpsKeeperOrderImmediateRetryError conn orderRouter orderId err = do
  _ <- execute conn
    "UPDATE perps_keeper_orders SET \
    \last_error = ?, \
    \last_attempt_at = NULL, \
    \updated_at = NOW() \
    \WHERE order_router = ? AND order_id = ?"
    (err, normalizeRouter orderRouter, orderId)
  pure ()

getPendingPerpsKeeperOrders :: Connection -> Text -> Int -> IO [PerpsKeeperOrderRow]
getPendingPerpsKeeperOrders conn orderRouter limitRows =
  query conn
    "SELECT order_id, order_router, account, side, commit_block, commit_time, commit_tx_hash, \
    \status, attempt_count, last_error \
    \FROM perps_keeper_orders \
    \WHERE order_router = ? AND status = 'pending' \
    \AND (last_attempt_at IS NULL OR last_attempt_at < NOW() - INTERVAL '5 seconds') \
    \ORDER BY order_id ASC LIMIT ?"
    (normalizeRouter orderRouter, limitRows)

getPerpsKeeperOrderById :: Connection -> Text -> Integer -> Maybe Text -> IO (Maybe PerpsKeeperTerminalOrderRow)
getPerpsKeeperOrderById conn orderRouter orderId mAccount = do
  rows <- case mAccount of
    Nothing ->
      query conn baseQuery (normalizeRouter orderRouter, orderId)
    Just account ->
      query conn accountQuery (normalizeRouter orderRouter, orderId, T.toLower account)
  pure $ case rows of
    row : _ -> Just row
    [] -> Nothing
  where
    baseSelect :: Query
    baseSelect =
      "SELECT order_id, order_router, account, side, commit_block, commit_event_block, commit_time, commit_tx_hash, status, \
      \execution_tx_hash, execution_block, execution_price, failure_tx_hash, failure_block, failure_reason \
      \FROM perps_keeper_orders \
      \WHERE order_router = ? AND order_id = ?"

    baseQuery :: Query
    baseQuery =
      baseSelect <> " LIMIT 1"

    accountQuery :: Query
    accountQuery =
      baseSelect <> " AND account = ? LIMIT 1"

data PerpsLiquidationCandidateRow = PerpsLiquidationCandidateRow
  { plcrAccount :: Text
  , plcrAttemptCount :: Int
  , plcrLastError :: Maybe Text
  , plcrPendingTxHash :: Maybe Text
  , plcrPendingNonce :: Maybe Integer
  , plcrPendingSender :: Maybe Text
  , plcrPendingRawTx :: Maybe Text
  , plcrPendingCallData :: Maybe Text
  , plcrPendingValue :: Maybe Integer
  , plcrPendingGasLimit :: Maybe Integer
  , plcrPendingMaxPriorityFeePerGas :: Maybe Integer
  , plcrPendingMaxFeePerGas :: Maybe Integer
  , plcrPendingStale :: Bool
  , plcrPendingBroadcastDue :: Bool
  }
  deriving stock (Show, Generic)

data PerpsLiquidationRejectedPayloadRow = PerpsLiquidationRejectedPayloadRow
  { plrprPayloadKey :: Text
  , plrprSelector :: Text
  , plrprError :: Text
  , plrprRejectedAt :: Text
  }
  deriving stock (Show, Generic)

data PerpsLiquidationSignerRetryRow = PerpsLiquidationSignerRetryRow
  { plrsrRequiredBalance :: Maybe Integer
  , plrsrError :: Text
  , plrsrRetryDue :: Bool
  , plrsrRecordedAt :: Text
  }
  deriving stock (Show, Generic)

instance FromRow PerpsLiquidationRejectedPayloadRow where
  fromRow =
    PerpsLiquidationRejectedPayloadRow
      <$> field
      <*> field
      <*> field
      <*> field

instance FromRow PerpsLiquidationSignerRetryRow where
  fromRow =
    PerpsLiquidationSignerRetryRow
      <$> numericIntegerField
      <*> field
      <*> field
      <*> field

instance FromRow PerpsLiquidationCandidateRow where
  fromRow =
    PerpsLiquidationCandidateRow
      <$> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> numericIntegerField
      <*> field
      <*> numericIntegerField
      <*> numericIntegerField
      <*> field
      <*> field

ensurePerpsLiquidationSchema :: Connection -> IO ()
ensurePerpsLiquidationSchema conn = do
  _ <- execute_ conn
    "CREATE TABLE IF NOT EXISTS perps_liquidation_state (\
    \chain_id BIGINT NOT NULL,\
    \cfd_engine TEXT NOT NULL,\
    \last_indexed_block BIGINT NOT NULL DEFAULT 0,\
    \rejected_payload_key TEXT,\
    \rejected_payload_selector TEXT,\
    \rejected_payload_error TEXT,\
    \rejected_payload_at TIMESTAMP,\
    \signer_retry_required_balance NUMERIC(78,0),\
    \signer_retry_error TEXT,\
    \signer_retry_at TIMESTAMP,\
    \updated_at TIMESTAMP DEFAULT NOW(),\
    \PRIMARY KEY (chain_id, cfd_engine)\
    \)"
  _ <- execute_ conn
    "ALTER TABLE perps_liquidation_state ADD COLUMN IF NOT EXISTS rejected_payload_key TEXT"
  _ <- execute_ conn
    "ALTER TABLE perps_liquidation_state ADD COLUMN IF NOT EXISTS rejected_payload_selector TEXT"
  _ <- execute_ conn
    "ALTER TABLE perps_liquidation_state ADD COLUMN IF NOT EXISTS rejected_payload_error TEXT"
  _ <- execute_ conn
    "ALTER TABLE perps_liquidation_state ADD COLUMN IF NOT EXISTS rejected_payload_at TIMESTAMP"
  _ <- execute_ conn
    "ALTER TABLE perps_liquidation_state ADD COLUMN IF NOT EXISTS signer_retry_required_balance NUMERIC(78,0)"
  _ <- execute_ conn
    "ALTER TABLE perps_liquidation_state ADD COLUMN IF NOT EXISTS signer_retry_error TEXT"
  _ <- execute_ conn
    "ALTER TABLE perps_liquidation_state ADD COLUMN IF NOT EXISTS signer_retry_at TIMESTAMP"
  _ <- execute_ conn
    "CREATE TABLE IF NOT EXISTS perps_liquidation_candidates (\
    \chain_id BIGINT NOT NULL,\
    \cfd_engine TEXT NOT NULL,\
    \account TEXT NOT NULL,\
    \first_seen_block BIGINT NOT NULL,\
    \last_seen_block BIGINT NOT NULL,\
    \attempt_count INTEGER NOT NULL DEFAULT 0,\
    \last_checked_at TIMESTAMP,\
    \last_error TEXT,\
    \pending_tx_hash TEXT,\
    \pending_nonce BIGINT,\
    \pending_sender TEXT,\
    \pending_raw_tx TEXT,\
    \pending_call_data TEXT,\
    \pending_value NUMERIC(78,0),\
    \pending_gas_limit BIGINT,\
    \pending_max_priority_fee_per_gas NUMERIC(78,0),\
    \pending_max_fee_per_gas NUMERIC(78,0),\
    \pending_since TIMESTAMP,\
    \pending_last_broadcast_at TIMESTAMP,\
    \updated_at TIMESTAMP DEFAULT NOW(),\
    \PRIMARY KEY (chain_id, cfd_engine, account)\
    \)"
  _ <- execute_ conn
    "ALTER TABLE perps_liquidation_candidates ADD COLUMN IF NOT EXISTS pending_tx_hash TEXT"
  _ <- execute_ conn
    "ALTER TABLE perps_liquidation_candidates ADD COLUMN IF NOT EXISTS pending_nonce BIGINT"
  _ <- execute_ conn
    "ALTER TABLE perps_liquidation_candidates ADD COLUMN IF NOT EXISTS pending_sender TEXT"
  _ <- execute_ conn
    "ALTER TABLE perps_liquidation_candidates ADD COLUMN IF NOT EXISTS pending_raw_tx TEXT"
  _ <- execute_ conn
    "ALTER TABLE perps_liquidation_candidates ADD COLUMN IF NOT EXISTS pending_call_data TEXT"
  _ <- execute_ conn
    "ALTER TABLE perps_liquidation_candidates ADD COLUMN IF NOT EXISTS pending_value NUMERIC(78,0)"
  _ <- execute_ conn
    "ALTER TABLE perps_liquidation_candidates ADD COLUMN IF NOT EXISTS pending_gas_limit BIGINT"
  _ <- execute_ conn
    "ALTER TABLE perps_liquidation_candidates ADD COLUMN IF NOT EXISTS pending_max_priority_fee_per_gas NUMERIC(78,0)"
  _ <- execute_ conn
    "ALTER TABLE perps_liquidation_candidates ADD COLUMN IF NOT EXISTS pending_max_fee_per_gas NUMERIC(78,0)"
  _ <- execute_ conn
    "ALTER TABLE perps_liquidation_candidates ADD COLUMN IF NOT EXISTS pending_since TIMESTAMP"
  _ <- execute_ conn
    "ALTER TABLE perps_liquidation_candidates ADD COLUMN IF NOT EXISTS pending_last_broadcast_at TIMESTAMP"
  _ <- execute_ conn
    "CREATE INDEX IF NOT EXISTS idx_perps_liquidation_candidates_scan \
    \ON perps_liquidation_candidates(chain_id, cfd_engine, last_checked_at ASC NULLS FIRST)"
  _ <- execute_ conn
    "CREATE INDEX IF NOT EXISTS idx_perps_liquidation_candidates_pending \
    \ON perps_liquidation_candidates(chain_id, cfd_engine, pending_since ASC) \
    \WHERE pending_tx_hash IS NOT NULL"
  pure ()

perpsLiquidationLockNamespace :: Int
perpsLiquidationLockNamespace = 421614486

tryPerpsLiquidationLock :: Connection -> Integer -> Text -> IO Bool
tryPerpsLiquidationLock conn chainId cfdEngine = do
  rows <-
    query
      conn
      "SELECT pg_try_advisory_lock(?, hashtext(?))"
      (perpsLiquidationLockNamespace, liquidationLockKey chainId cfdEngine) :: IO [Only Bool]
  pure $ case rows of
    [Only acquired] -> acquired
    _ -> False

unlockPerpsLiquidationLock :: Connection -> Integer -> Text -> IO ()
unlockPerpsLiquidationLock conn chainId cfdEngine = do
  _ <-
    query
      conn
      "SELECT pg_advisory_unlock(?, hashtext(?))"
      (perpsLiquidationLockNamespace, liquidationLockKey chainId cfdEngine) :: IO [Only Bool]
  pure ()

liquidationLockKey :: Integer -> Text -> Text
liquidationLockKey chainId cfdEngine =
  T.pack (show chainId) <> ":" <> normalizeRouter cfdEngine

getPerpsLiquidationLastIndexedBlock :: Connection -> Integer -> Text -> IO Integer
getPerpsLiquidationLastIndexedBlock conn chainId cfdEngine = do
  rows <- query conn
    "SELECT last_indexed_block FROM perps_liquidation_state WHERE chain_id = ? AND cfd_engine = ?"
    (chainId, normalizeRouter cfdEngine) :: IO [Only Integer]
  pure $ case rows of
    [Only blockNumber] -> blockNumber
    _ -> 0

setPerpsLiquidationLastIndexedBlock :: Connection -> Integer -> Text -> Integer -> IO ()
setPerpsLiquidationLastIndexedBlock conn chainId cfdEngine blockNumber = do
  _ <- execute conn
    "INSERT INTO perps_liquidation_state (chain_id, cfd_engine, last_indexed_block, updated_at) \
    \VALUES (?, ?, ?, NOW()) \
    \ON CONFLICT (chain_id, cfd_engine) DO UPDATE SET \
    \last_indexed_block = EXCLUDED.last_indexed_block, updated_at = NOW()"
    (chainId, normalizeRouter cfdEngine, blockNumber)
  pure ()

getPerpsLiquidationRejectedPayload
  :: Connection
  -> Integer
  -> Text
  -> IO (Maybe PerpsLiquidationRejectedPayloadRow)
getPerpsLiquidationRejectedPayload conn chainId cfdEngine = do
  rows <-
    query
      conn
      "SELECT rejected_payload_key, \
      \COALESCE(rejected_payload_selector, ''), \
      \COALESCE(rejected_payload_error, ''), \
      \COALESCE(rejected_payload_at, updated_at, NOW())::TEXT \
      \FROM perps_liquidation_state \
      \WHERE chain_id = ? AND cfd_engine = ? AND rejected_payload_key IS NOT NULL"
      (chainId, normalizeRouter cfdEngine)
  pure $ case rows of
    rejected : _ -> Just rejected
    [] -> Nothing

recordPerpsLiquidationRejectedPayload
  :: Connection
  -> Integer
  -> Text
  -> Text
  -> Text
  -> Text
  -> IO ()
recordPerpsLiquidationRejectedPayload conn chainId cfdEngine payloadKey selectorText err = do
  _ <-
    execute
      conn
      "INSERT INTO perps_liquidation_state \
      \(chain_id, cfd_engine, last_indexed_block, rejected_payload_key, \
      \rejected_payload_selector, rejected_payload_error, rejected_payload_at, updated_at) \
      \VALUES (?, ?, 0, ?, ?, ?, NOW(), NOW()) \
      \ON CONFLICT (chain_id, cfd_engine) DO UPDATE SET \
      \rejected_payload_key = EXCLUDED.rejected_payload_key, \
      \rejected_payload_selector = EXCLUDED.rejected_payload_selector, \
      \rejected_payload_error = EXCLUDED.rejected_payload_error, \
      \rejected_payload_at = NOW(), updated_at = NOW()"
      ( chainId
      , normalizeRouter cfdEngine
      , T.toLower payloadKey
      , T.toLower selectorText
      , err
      )
  pure ()

clearPerpsLiquidationRejectedPayload :: Connection -> Integer -> Text -> IO ()
clearPerpsLiquidationRejectedPayload conn chainId cfdEngine = do
  _ <-
    execute
      conn
      "UPDATE perps_liquidation_state SET \
      \rejected_payload_key = NULL, rejected_payload_selector = NULL, \
      \rejected_payload_error = NULL, rejected_payload_at = NULL, updated_at = NOW() \
      \WHERE chain_id = ? AND cfd_engine = ?"
      (chainId, normalizeRouter cfdEngine)
  pure ()

getPerpsLiquidationSignerRetry
  :: Connection
  -> Integer
  -> Text
  -> Int
  -> IO (Maybe PerpsLiquidationSignerRetryRow)
getPerpsLiquidationSignerRetry conn chainId cfdEngine retrySeconds = do
  rows <-
    query
      conn
      "SELECT signer_retry_required_balance, \
      \COALESCE(signer_retry_error, ''), \
      \signer_retry_at <= NOW() - (? * INTERVAL '1 second'), \
      \COALESCE(signer_retry_at, updated_at, NOW())::TEXT \
      \FROM perps_liquidation_state \
      \WHERE chain_id = ? AND cfd_engine = ? AND signer_retry_at IS NOT NULL"
      (max 1 retrySeconds, chainId, normalizeRouter cfdEngine)
  pure $ case rows of
    retry : _ -> Just retry
    [] -> Nothing

recordPerpsLiquidationSignerRetry
  :: Connection
  -> Integer
  -> Text
  -> Integer
  -> Text
  -> IO ()
recordPerpsLiquidationSignerRetry conn chainId cfdEngine requiredBalance err = do
  _ <-
    execute
      conn
      "INSERT INTO perps_liquidation_state \
      \(chain_id, cfd_engine, last_indexed_block, signer_retry_required_balance, \
      \signer_retry_error, signer_retry_at, updated_at) \
      \VALUES (?, ?, 0, ?, ?, NOW(), NOW()) \
      \ON CONFLICT (chain_id, cfd_engine) DO UPDATE SET \
      \signer_retry_required_balance = EXCLUDED.signer_retry_required_balance, \
      \signer_retry_error = EXCLUDED.signer_retry_error, \
      \signer_retry_at = NOW(), updated_at = NOW()"
      (chainId, normalizeRouter cfdEngine, max 0 requiredBalance, err)
  pure ()

clearPerpsLiquidationSignerRetry :: Connection -> Integer -> Text -> IO ()
clearPerpsLiquidationSignerRetry conn chainId cfdEngine = do
  _ <-
    execute
      conn
      "UPDATE perps_liquidation_state SET \
      \signer_retry_required_balance = NULL, signer_retry_error = NULL, \
      \signer_retry_at = NULL, updated_at = NOW() \
      \WHERE chain_id = ? AND cfd_engine = ?"
      (chainId, normalizeRouter cfdEngine)
  pure ()

upsertPerpsLiquidationCandidate :: Connection -> Integer -> Text -> Text -> Integer -> IO ()
upsertPerpsLiquidationCandidate conn chainId cfdEngine account blockNumber = do
  _ <- execute conn
    "INSERT INTO perps_liquidation_candidates \
    \(chain_id, cfd_engine, account, first_seen_block, last_seen_block) \
    \VALUES (?, ?, ?, ?, ?) \
    \ON CONFLICT (chain_id, cfd_engine, account) DO UPDATE SET \
    \last_seen_block = GREATEST(perps_liquidation_candidates.last_seen_block, EXCLUDED.last_seen_block), \
    \last_checked_at = CASE \
    \  WHEN EXCLUDED.last_seen_block > perps_liquidation_candidates.last_seen_block THEN NULL \
    \  ELSE perps_liquidation_candidates.last_checked_at \
    \END, updated_at = NOW()"
    (chainId, normalizeRouter cfdEngine, T.toLower account, blockNumber, blockNumber)
  pure ()

seedPerpsLiquidationCandidatesFromHistory :: Connection -> Integer -> Text -> Text -> IO ()
seedPerpsLiquidationCandidatesFromHistory conn chainId orderRouter cfdEngine = do
  tableRows <- query_ conn
    "SELECT to_regclass('perps_account_activity') IS NOT NULL" :: IO [Only Bool]
  case tableRows of
    [Only True] -> do
      _ <- execute conn
        "INSERT INTO perps_liquidation_candidates \
        \(chain_id, cfd_engine, account, first_seen_block, last_seen_block) \
        \SELECT chain_id, ?, account, MIN(block_number), MAX(block_number) \
        \FROM perps_account_activity \
        \WHERE chain_id = ? AND release_router = ? AND activity_type = 'Open' \
        \GROUP BY chain_id, account \
        \ON CONFLICT (chain_id, cfd_engine, account) DO UPDATE SET \
        \first_seen_block = LEAST(perps_liquidation_candidates.first_seen_block, EXCLUDED.first_seen_block), \
        \last_seen_block = GREATEST(perps_liquidation_candidates.last_seen_block, EXCLUDED.last_seen_block), \
        \last_checked_at = CASE \
        \  WHEN EXCLUDED.last_seen_block > perps_liquidation_candidates.last_seen_block THEN NULL \
        \  ELSE perps_liquidation_candidates.last_checked_at \
        \END, updated_at = NOW() \
        \WHERE EXCLUDED.first_seen_block < perps_liquidation_candidates.first_seen_block \
        \OR EXCLUDED.last_seen_block > perps_liquidation_candidates.last_seen_block"
        (normalizeRouter cfdEngine, chainId, normalizeRouter orderRouter)
      pure ()
    _ -> pure ()

getPerpsLiquidationCandidates :: Connection -> Integer -> Text -> Int -> IO [PerpsLiquidationCandidateRow]
getPerpsLiquidationCandidates conn chainId cfdEngine limitRows =
  query conn
    "SELECT account, attempt_count, last_error, pending_tx_hash, pending_nonce, pending_sender, pending_raw_tx, \
    \pending_call_data, pending_value, pending_gas_limit, pending_max_priority_fee_per_gas, \
    \pending_max_fee_per_gas, FALSE, FALSE \
    \FROM perps_liquidation_candidates \
    \WHERE chain_id = ? AND cfd_engine = ? \
    \ORDER BY last_checked_at ASC NULLS FIRST, first_seen_block ASC, account ASC \
    \LIMIT ?"
    (chainId, normalizeRouter cfdEngine, limitRows)

getPendingPerpsLiquidationCandidates :: Connection -> Integer -> Text -> Int -> Int -> IO [PerpsLiquidationCandidateRow]
getPendingPerpsLiquidationCandidates conn chainId cfdEngine replacementSeconds broadcastRetrySeconds =
  query
    conn
    "WITH oldest AS (\
    \  SELECT pending_tx_hash FROM perps_liquidation_candidates \
    \  WHERE chain_id = ? AND cfd_engine = ? AND pending_tx_hash IS NOT NULL \
    \  ORDER BY pending_since ASC, account ASC LIMIT 1\
    \) \
    \SELECT account, attempt_count, last_error, pending_tx_hash, pending_nonce, pending_sender, pending_raw_tx, \
    \pending_call_data, pending_value, pending_gas_limit, pending_max_priority_fee_per_gas, \
    \pending_max_fee_per_gas, \
    \COALESCE(pending_since <= NOW() - (? * INTERVAL '1 second'), FALSE), \
    \COALESCE(pending_last_broadcast_at <= NOW() - (? * INTERVAL '1 second'), TRUE) \
    \FROM perps_liquidation_candidates \
    \WHERE chain_id = ? AND cfd_engine = ? \
    \AND pending_tx_hash = (SELECT pending_tx_hash FROM oldest) \
    \ORDER BY account ASC"
    ( chainId
    , normalizeRouter cfdEngine
    , max 1 replacementSeconds
    , max 1 broadcastRetrySeconds
    , chainId
    , normalizeRouter cfdEngine
    )

markPerpsLiquidationCandidateChecked :: Connection -> Integer -> Text -> Text -> IO ()
markPerpsLiquidationCandidateChecked conn chainId cfdEngine account = do
  _ <- execute conn
    "UPDATE perps_liquidation_candidates SET \
    \last_checked_at = NOW(), last_error = NULL, updated_at = NOW() \
    \WHERE chain_id = ? AND cfd_engine = ? AND account = ?"
    (chainId, normalizeRouter cfdEngine, T.toLower account)
  pure ()

recordPerpsLiquidationCandidatePending
  :: Connection
  -> Integer
  -> Text
  -> Text
  -> Integer
  -> Text
  -> Text
  -> Text
  -> Text
  -> Integer
  -> Integer
  -> Integer
  -> Integer
  -> IO ()
recordPerpsLiquidationCandidatePending conn chainId cfdEngine account nonce sender txHash rawTx callData value gasLimit maxPriorityFee maxFee = do
  _ <- execute conn
    "UPDATE perps_liquidation_candidates SET \
    \attempt_count = attempt_count + 1, last_checked_at = NOW(), last_error = NULL, \
    \pending_nonce = ?, pending_sender = ?, pending_tx_hash = ?, pending_raw_tx = ?, pending_call_data = ?, \
    \pending_value = ?, pending_gas_limit = ?, pending_max_priority_fee_per_gas = ?, \
    \pending_max_fee_per_gas = ?, pending_since = NOW(), pending_last_broadcast_at = NULL, updated_at = NOW() \
    \WHERE chain_id = ? AND cfd_engine = ? AND account = ?"
    ( nonce
    , normalizeRouter sender
    , T.toLower txHash
    , T.toLower rawTx
    , T.toLower callData
    , value
    , gasLimit
    , maxPriorityFee
    , maxFee
    , chainId
    , normalizeRouter cfdEngine
    , T.toLower account
    )
  pure ()

recordPerpsLiquidationCandidateBroadcastAttempt :: Connection -> Integer -> Text -> Text -> IO ()
recordPerpsLiquidationCandidateBroadcastAttempt conn chainId cfdEngine account = do
  _ <- execute conn
    "UPDATE perps_liquidation_candidates SET \
    \pending_last_broadcast_at = NOW(), updated_at = NOW() \
    \WHERE chain_id = ? AND cfd_engine = ? AND account = ? AND pending_tx_hash IS NOT NULL"
    (chainId, normalizeRouter cfdEngine, T.toLower account)
  pure ()

clearPerpsLiquidationCandidatePending :: Connection -> Integer -> Text -> Text -> IO ()
clearPerpsLiquidationCandidatePending conn chainId cfdEngine account = do
  _ <- execute conn
    "UPDATE perps_liquidation_candidates SET \
    \pending_nonce = NULL, pending_sender = NULL, pending_tx_hash = NULL, pending_raw_tx = NULL, pending_call_data = NULL, \
    \pending_value = NULL, pending_gas_limit = NULL, pending_max_priority_fee_per_gas = NULL, \
    \pending_max_fee_per_gas = NULL, pending_since = NULL, pending_last_broadcast_at = NULL, updated_at = NOW() \
    \WHERE chain_id = ? AND cfd_engine = ? AND account = ?"
    (chainId, normalizeRouter cfdEngine, T.toLower account)
  pure ()

recordPerpsLiquidationCandidateError :: Connection -> Integer -> Text -> Text -> Text -> IO ()
recordPerpsLiquidationCandidateError conn chainId cfdEngine account err = do
  _ <- execute conn
    "UPDATE perps_liquidation_candidates SET \
    \last_checked_at = NOW(), last_error = ?, updated_at = NOW() \
    \WHERE chain_id = ? AND cfd_engine = ? AND account = ?"
    (err, chainId, normalizeRouter cfdEngine, T.toLower account)
  pure ()

-- | Records a retryable batch-item outcome and places it at the front of the
-- next candidate sweep. This is used for an unattempted suffix or an isolated
-- item failure after the shared batch transaction itself succeeded.
recordPerpsLiquidationCandidateRetryableError :: Connection -> Integer -> Text -> Text -> Text -> IO ()
recordPerpsLiquidationCandidateRetryableError conn chainId cfdEngine account err = do
  _ <- execute conn
    "UPDATE perps_liquidation_candidates SET \
    \last_checked_at = NULL, last_error = ?, updated_at = NOW() \
    \WHERE chain_id = ? AND cfd_engine = ? AND account = ?"
    (err, chainId, normalizeRouter cfdEngine, T.toLower account)
  pure ()

deletePerpsLiquidationCandidate :: Connection -> Integer -> Text -> Text -> IO ()
deletePerpsLiquidationCandidate conn chainId cfdEngine account = do
  _ <- execute conn
    "DELETE FROM perps_liquidation_candidates \
    \WHERE chain_id = ? AND cfd_engine = ? AND account = ?"
    (chainId, normalizeRouter cfdEngine, T.toLower account)
  pure ()

data PerpsOrderRow = PerpsOrderRow
  { porOrderId :: Integer
  , porOrderRouter :: Text
  , porAccount :: Maybe Text
  , porSide :: Maybe Int
  , porCommitTxHash :: Maybe Text
  , porCommitBlockNumber :: Maybe Integer
  , porCommitTimestamp :: Maybe Integer
  , porTerminalTxHash :: Maybe Text
  , porTerminalBlockNumber :: Maybe Integer
  , porTerminalBlockHash :: Maybe Text
  , porTerminalTimestamp :: Maybe Integer
  , porTerminalStatus :: Text
  , porFailureReason :: Maybe Text
  , porExecutionPrice :: Maybe Integer
  , porExecutionVpiUsdc :: Maybe Integer
  , porExecutionFrozenCloseSpreadUsdc :: Maybe Integer
  , porExecutionEconomicsVersion :: Maybe Int
  , porExecutionOraclePrice :: Maybe Integer
  , porExecutionOracleFrozen :: Maybe Bool
  , porOracleMinPublishTime :: Maybe Integer
  , porOracleMaxPublishTime :: Maybe Integer
  , porOracleDerivationVersion :: Maybe Int
  , porClientOrderId :: Maybe Text
  , porReceiptHash :: Maybe Text
  , porTerminalReason :: Maybe Text
  , porPendingReason :: Maybe Text
  , porExecutionMode :: Maybe Text
  , porFailedConstraint :: Maybe Text
  , porReceiptEconomics :: Maybe Value
  , porCleanupActor :: Maybe Text
  , porActivityType :: Maybe Text
  , porActivitySizeDelta :: Maybe Integer
  , porActivityPrice :: Maybe Integer
  , porActivityVpiUsdc :: Maybe Integer
  , porActivityPnlUsdc :: Maybe Integer
  , porSortBlock :: Integer
  }
  deriving stock (Show, Generic)

instance FromRow PerpsOrderRow where
  fromRow = PerpsOrderRow
    <$> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> numericIntegerField
    <*> numericIntegerField
    <*> numericIntegerField
    <*> field
    <*> numericIntegerField
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> numericIntegerField
    <*> numericIntegerField
    <*> numericIntegerField
    <*> numericIntegerField
    <*> field

data PerpsExecutionEvidenceRow = PerpsExecutionEvidenceRow
  { peerOrderId :: Integer
  , peerCommitTimestamp :: Maybe Integer
  , peerTerminalTxHash :: Text
  , peerTerminalBlockNumber :: Integer
  , peerTerminalBlockHash :: Text
  , peerOracleDerivationVersion :: Maybe Int
  , peerExecutionEconomicsVersion :: Maybe Int
  }
  deriving stock (Show, Generic)

instance FromRow PerpsExecutionEvidenceRow where
  fromRow = PerpsExecutionEvidenceRow
    <$> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field

data PerpsActivityRow = PerpsActivityRow
  { parActivityType :: Text
  , parOrderRouter :: Text
  , parContractAddress :: Maybe Text
  , parAccount :: Text
  , parActor :: Maybe Text
  , parOrderId :: Maybe Integer
  , parSide :: Maybe Int
  , parPrice :: Maybe Integer
  , parSizeDelta :: Maybe Integer
  , parAmountUsdc :: Maybe Integer
  , parPnlUsdc :: Maybe Integer
  , parTxHash :: Text
  , parBlockNumber :: Integer
  , parTimestamp :: Integer
  , parData :: Value
  , parLogIndex :: Integer
  }
  deriving stock (Show, Generic)

instance FromRow PerpsActivityRow where
  fromRow = PerpsActivityRow
    <$> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> numericIntegerField
    <*> numericIntegerField
    <*> numericIntegerField
    <*> numericIntegerField
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field

data PerpsMarketVolumeBucketRow = PerpsMarketVolumeBucketRow
  { pmvbrBucket :: Integer
  , pmvbrVolumeUsdc :: Integer
  }
  deriving stock (Eq, Show, Generic)

instance FromRow PerpsMarketVolumeBucketRow where
  fromRow = PerpsMarketVolumeBucketRow
    <$> field
    <*> (scientificToInteger <$> (field :: RowParser Scientific))

numericIntegerField :: RowParser (Maybe Integer)
numericIntegerField = fmap scientificToInteger <$> (field :: RowParser (Maybe Scientific))

numericRequiredIntegerField :: RowParser Integer
numericRequiredIntegerField = scientificToInteger <$> (field :: RowParser Scientific)

normalizeRouter :: Text -> Text
normalizeRouter = T.toLower . T.strip

scientificToInteger :: Scientific -> Integer
scientificToInteger value
  | scale >= 0 = coeff * (10 ^ scale)
  | otherwise = coeff `div` (10 ^ negate scale)
  where
    coeff = coefficient value
    scale = base10Exponent value

data PerpsIndexerStatusRow = PerpsIndexerStatusRow
  { pisIndexerName :: Text
  , pisChainId :: Integer
  , pisReleaseRouter :: Text
  , pisLastIndexedBlock :: Integer
  , pisLastIndexedBlockHash :: Maybe Text
  }
  deriving stock (Show, Generic)

instance FromRow PerpsIndexerStatusRow where
  fromRow = PerpsIndexerStatusRow
    <$> field
    <*> field
    <*> field
    <*> field
    <*> field

-- Stable semantic rows touched by bounded duplicate ingestion. Surrogate IDs
-- and volatile created/updated timestamps are excluded; every field that the
-- replay write path can insert or update is retained in PostgreSQL's canonical
-- JSON text representation and deterministic order.
data PerpsReplayHistorySnapshot = PerpsReplayHistorySnapshot
  { prhsEvents :: [Text]
  , prhsOrders :: [Text]
  , prhsActivity :: [Text]
  , prhsUsdcTransfers :: [Text]
  }
  deriving stock (Eq, Show)

-- Acquire affected order rows in deterministic order after the indexer and
-- volume locks. This prevents the independent evidence worker from mutating
-- enrichment columns between replay's semantic before/after snapshots.
lockPerpsReplayOrders :: Connection -> Integer -> Text -> [Integer] -> IO ()
lockPerpsReplayOrders _ _ _ [] = pure ()
lockPerpsReplayOrders conn chainId releaseRouter orderIds = do
  rows <- query conn
    "SELECT order_id FROM perps_orders \
    \WHERE chain_id = ? AND order_router = ? AND order_id IN ? \
    \ORDER BY order_id FOR UPDATE"
    (chainId, normalizeRouter releaseRouter, In orderIds) :: IO [Only Integer]
  -- Missing rows are deliberately allowed here; the semantic snapshot will
  -- detect an insertion and force rollback.
  rows `seq` pure ()

assertExactlyOneReplayRow :: Text -> [Only Integer] -> IO ()
assertExactlyOneReplayRow label rows =
  unless (length rows == 1) $
    fail $ "Bounded replay semantic assertion failed for " <> T.unpack label

assertPerpsReplayEventExact
  :: Connection
  -> Integer
  -> Text
  -> Text
  -> Text
  -> Text
  -> Integer
  -> Text
  -> Integer
  -> Integer
  -> Integer
  -> Maybe Text
  -> Maybe Integer
  -> Maybe Int
  -> Value
  -> IO ()
assertPerpsReplayEventExact conn chainId releaseRouter contractAddress eventName txHash blockNumber blockHash txIndex logIndex timestamp account orderId side payload = do
  rows <- query conn
    "SELECT 1::BIGINT FROM perps_events WHERE chain_id = ? AND tx_hash = ? AND log_index = ? \
    \AND release_router = ? AND contract_address = ? AND event_name = ? AND block_number = ? \
    \AND block_hash = ? AND tx_index = ? AND timestamp = ? \
    \AND account IS NOT DISTINCT FROM ? AND order_id IS NOT DISTINCT FROM ? \
    \AND side IS NOT DISTINCT FROM ? AND data = ?::jsonb"
    ( chainId
    , T.toLower txHash
    , logIndex
    , normalizeRouter releaseRouter
    , T.toLower contractAddress
    , eventName
    , blockNumber
    , T.toLower blockHash
    , txIndex
    , timestamp
    , fmap T.toLower account
    , orderId
    , side
    , encode payload
    ) :: IO [Only Integer]
  assertExactlyOneReplayRow "event" rows

assertPerpsReplayOrderCommittedExact
  :: Connection
  -> Integer
  -> Text
  -> Integer
  -> Text
  -> Int
  -> Text
  -> Integer
  -> Integer
  -> IO ()
assertPerpsReplayOrderCommittedExact conn chainId orderRouter orderId account side txHash blockNumber timestamp = do
  rows <- query conn
    "SELECT 1::BIGINT FROM perps_orders WHERE chain_id = ? AND order_router = ? AND order_id = ? \
    \AND account = ? AND side = ? AND commit_tx_hash = ? AND commit_block_number = ? \
    \AND commit_timestamp = ?"
    ( chainId
    , normalizeRouter orderRouter
    , orderId
    , T.toLower account
    , side
    , T.toLower txHash
    , blockNumber
    , timestamp
    ) :: IO [Only Integer]
  assertExactlyOneReplayRow "committed order" rows

assertPerpsReplayOrderTerminalExact
  :: Connection
  -> Integer
  -> Text
  -> Integer
  -> Text
  -> Maybe Text
  -> Maybe Integer
  -> Maybe Text
  -> Text
  -> Integer
  -> Integer
  -> IO ()
assertPerpsReplayOrderTerminalExact conn chainId orderRouter orderId status failureReason executionPrice cleanupActor txHash blockNumber timestamp = do
  rows <- query conn
    "SELECT 1::BIGINT FROM perps_orders WHERE chain_id = ? AND order_router = ? AND order_id = ? \
    \AND terminal_status = ? AND failure_reason IS NOT DISTINCT FROM ? \
    \AND execution_price IS NOT DISTINCT FROM ? AND cleanup_actor IS NOT DISTINCT FROM ? \
    \AND terminal_tx_hash = ? AND terminal_block_number = ? AND terminal_timestamp = ?"
    ( chainId
    , normalizeRouter orderRouter
    , orderId
    , status
    , failureReason
    , executionPrice
    , fmap T.toLower cleanupActor
    , T.toLower txHash
    , blockNumber
    , timestamp
    ) :: IO [Only Integer]
  assertExactlyOneReplayRow "terminal order" rows

assertPerpsReplayActivityExact
  :: Connection
  -> Integer
  -> Text
  -> Text
  -> Text
  -> Text
  -> Text
  -> Maybe Text
  -> Maybe Integer
  -> Maybe Int
  -> Maybe Integer
  -> Maybe Integer
  -> Maybe Integer
  -> Maybe Integer
  -> Text
  -> Integer
  -> Text
  -> Integer
  -> Integer
  -> Integer
  -> Value
  -> IO ()
assertPerpsReplayActivityExact conn chainId releaseRouter contractAddress eventKey account activityType actor orderId side price sizeDelta amountUsdc pnlUsdc txHash blockNumber blockHash txIndex logIndex timestamp payload = do
  rows <- query conn
    "SELECT 1::BIGINT FROM perps_account_activity WHERE event_key = ? \
    \AND chain_id = ? AND release_router = ? AND contract_address = ? AND account = ? \
    \AND actor IS NOT DISTINCT FROM ? AND activity_type = ? AND order_id IS NOT DISTINCT FROM ? \
    \AND side IS NOT DISTINCT FROM ? AND price IS NOT DISTINCT FROM ? \
    \AND size_delta IS NOT DISTINCT FROM ? AND amount_usdc IS NOT DISTINCT FROM ? \
    \AND pnl_usdc IS NOT DISTINCT FROM ? AND tx_hash = ? AND block_number = ? \
    \AND block_hash = ? AND tx_index = ? AND log_index = ? AND timestamp = ? AND data = ?::jsonb"
    ( eventKey
    , chainId
    , normalizeRouter releaseRouter
    , normalizeRouter contractAddress
    , T.toLower account
    , fmap T.toLower actor
    , activityType
    , orderId
    , side
    , price
    , sizeDelta
    , amountUsdc
    , pnlUsdc
    , T.toLower txHash
    , blockNumber
    , T.toLower blockHash
    , txIndex
    , logIndex
    , timestamp
    , encode payload
    ) :: IO [Only Integer]
  assertExactlyOneReplayRow "account activity" rows

assertPerpsReplayUsdcTransferExact
  :: Connection
  -> Integer
  -> Text
  -> Text
  -> Text
  -> Text
  -> Integer
  -> Text
  -> Integer
  -> Text
  -> Integer
  -> Integer
  -> Integer
  -> IO ()
assertPerpsReplayUsdcTransferExact conn chainId releaseRouter tokenAddress fromAddress toAddress amount txHash blockNumber blockHash txIndex logIndex timestamp = do
  rows <- query conn
    "SELECT 1::BIGINT FROM perps_usdc_transfers \
    \WHERE chain_id = ? AND release_router = ? AND token_address = ? \
    \AND from_address = ? AND to_address = ? AND amount = ? AND tx_hash = ? \
    \AND block_number = ? AND block_hash = ? AND tx_index = ? AND log_index = ? AND timestamp = ?"
    ( chainId
    , normalizeRouter releaseRouter
    , normalizeRouter tokenAddress
    , T.toLower fromAddress
    , T.toLower toAddress
    , amount
    , T.toLower txHash
    , blockNumber
    , T.toLower blockHash
    , txIndex
    , logIndex
    , timestamp
    ) :: IO [Only Integer]
  assertExactlyOneReplayRow "USDC transfer" rows

assertPerpsReplayExpiredCleanupExact :: Connection -> Integer -> Text -> Integer -> IO ()
assertPerpsReplayExpiredCleanupExact conn chainId orderRouter orderId =
  assertPerpsReplayExpiredCleanupWithRequirement conn chainId orderRouter orderId True

assertPerpsReplayExpiredCleanupIfReadyExact :: Connection -> Integer -> Text -> Integer -> IO ()
assertPerpsReplayExpiredCleanupIfReadyExact conn chainId orderRouter orderId =
  assertPerpsReplayExpiredCleanupWithRequirement conn chainId orderRouter orderId False

assertPerpsReplayExpiredCleanupWithRequirement
  :: Connection -> Integer -> Text -> Integer -> Bool -> IO ()
assertPerpsReplayExpiredCleanupWithRequirement conn chainId orderRouter orderId requireExpired = do
  rows <- query conn
    "WITH order_state AS (\
    \ SELECT account, side, cleanup_actor, terminal_status, terminal_tx_hash, terminal_block_number \
    \ FROM perps_orders WHERE chain_id = ? AND order_router = ? AND order_id = ?), \
    \expected AS (\
    \ SELECT o.account, o.side, o.cleanup_actor, e.contract_address, e.tx_hash, e.block_number, \
    \ e.block_hash, e.tx_index, e.log_index, e.timestamp \
    \ FROM order_state o JOIN perps_events e ON e.chain_id = ? \
    \ AND e.release_router = ? AND e.order_id = ? AND e.event_name = 'OrderFailed' \
    \ AND e.tx_hash = o.terminal_tx_hash AND e.block_number = o.terminal_block_number \
    \ WHERE o.terminal_status = 'Expired / Cleaned up' AND o.account IS NOT NULL \
    \ AND o.side IS NOT NULL AND o.cleanup_actor IS NOT NULL \
    \ ORDER BY e.log_index DESC LIMIT 1), \
    \exact_cleanup AS (\
    \ SELECT 1 FROM expected e JOIN perps_account_activity a ON \
    \ a.event_key = LOWER(e.tx_hash) || ':' || e.log_index::text || ':cleanup:' || ?::text \
    \ WHERE a.chain_id = ? AND a.release_router = ? AND a.contract_address = LOWER(e.contract_address) \
    \ AND a.account = LOWER(e.account) AND a.actor = LOWER(e.cleanup_actor) \
    \ AND a.activity_type = 'Cleaned up expired order' AND a.order_id = ? \
    \ AND a.side = e.side AND a.price IS NULL AND a.size_delta IS NULL \
    \ AND a.amount_usdc IS NULL AND a.pnl_usdc IS NULL AND a.tx_hash = LOWER(e.tx_hash) \
    \ AND a.block_number = e.block_number AND a.block_hash = LOWER(e.block_hash) \
    \ AND a.tx_index = e.tx_index AND a.log_index = e.log_index AND a.timestamp = e.timestamp \
    \ AND a.data = jsonb_build_object('orderId', ?::text, 'reason', 'Expired', 'actor', e.cleanup_actor)) \
    \SELECT 1::BIGINT WHERE (NOT ? AND EXISTS (\
    \ SELECT 1 FROM order_state WHERE terminal_status <> 'Expired / Cleaned up')) \
    \ OR EXISTS (SELECT 1 FROM exact_cleanup)"
    ( chainId
    , normalizeRouter orderRouter
    , orderId
    , chainId
    , normalizeRouter orderRouter
    , orderId
    , orderId
    , chainId
    , normalizeRouter orderRouter
    , orderId
    , orderId
    , requireExpired
    ) :: IO [Only Integer]
  assertExactlyOneReplayRow "expired cleanup activity" rows

getPerpsReplayHistorySnapshot
  :: Connection
  -> Integer
  -> Text
  -> Integer
  -> Integer
  -> [Integer]
  -> IO PerpsReplayHistorySnapshot
getPerpsReplayHistorySnapshot conn chainId releaseRouter fromBlock toBlock orderIds = do
  let router = normalizeRouter releaseRouter
  events <- query conn
    "SELECT row_to_json(snapshot)::text FROM (\
    \ SELECT chain_id, release_router, contract_address, event_name, tx_hash, block_number, \
    \ block_hash, tx_index, log_index, timestamp, account, order_id, side, data \
    \ FROM perps_events WHERE chain_id = ? AND release_router = ? \
    \ AND block_number BETWEEN ? AND ? ORDER BY block_number, tx_index, log_index) snapshot"
    (chainId, router, fromBlock, toBlock) :: IO [Only Text]
  orders <-
    if null orderIds
      then pure []
      else
        query conn
          "SELECT row_to_json(snapshot)::text FROM (\
          \ SELECT chain_id, order_router, order_id, account, side, commit_tx_hash, commit_block_number, \
          \ commit_timestamp, terminal_tx_hash, terminal_block_number, terminal_timestamp, terminal_status, \
          \ failure_reason, execution_price, execution_vpi_usdc, execution_frozen_close_spread_usdc, \
          \ execution_economics_version, execution_oracle_price, execution_oracle_frozen, \
          \ oracle_min_publish_time, oracle_max_publish_time, oracle_derivation_version, \
          \ execution_evidence_last_attempt_at, cleanup_actor \
          \ FROM perps_orders WHERE chain_id = ? AND order_router = ? AND order_id IN ? \
          \ ORDER BY order_id) snapshot"
          (chainId, router, In orderIds) :: IO [Only Text]
  activity <-
    if null orderIds
      then
        query conn
          "SELECT row_to_json(snapshot)::text FROM (\
          \ SELECT chain_id, release_router, contract_address, event_key, account, actor, activity_type, \
          \ order_id, side, price, size_delta, amount_usdc, pnl_usdc, tx_hash, block_number, block_hash, \
          \ tx_index, log_index, timestamp, data FROM perps_account_activity \
          \ WHERE chain_id = ? AND release_router = ? AND block_number BETWEEN ? AND ? \
          \ ORDER BY block_number, tx_index, log_index, event_key) snapshot"
          (chainId, router, fromBlock, toBlock)
      else
        query conn
          "SELECT row_to_json(snapshot)::text FROM (\
          \ SELECT chain_id, release_router, contract_address, event_key, account, actor, activity_type, \
          \ order_id, side, price, size_delta, amount_usdc, pnl_usdc, tx_hash, block_number, block_hash, \
          \ tx_index, log_index, timestamp, data FROM perps_account_activity \
          \ WHERE chain_id = ? AND release_router = ? \
          \ AND (block_number BETWEEN ? AND ? OR order_id IN ?) \
          \ ORDER BY block_number, tx_index, log_index, event_key) snapshot"
          (chainId, router, fromBlock, toBlock, In orderIds)
    :: IO [Only Text]
  transfers <- query conn
    "SELECT row_to_json(snapshot)::text FROM (\
    \ SELECT chain_id, release_router, token_address, from_address, to_address, amount, tx_hash, \
    \ block_number, block_hash, tx_index, log_index, timestamp FROM perps_usdc_transfers \
    \ WHERE chain_id = ? AND release_router = ? AND block_number BETWEEN ? AND ? \
    \ ORDER BY block_number, tx_index, log_index) snapshot"
    (chainId, router, fromBlock, toBlock) :: IO [Only Text]
  pure $
    PerpsReplayHistorySnapshot
      { prhsEvents = map fromOnly events
      , prhsOrders = map fromOnly orders
      , prhsActivity = map fromOnly activity
      , prhsUsdcTransfers = map fromOnly transfers
      }

ensurePerpsHistorySchema :: Connection -> IO ()
ensurePerpsHistorySchema conn = do
  _ <- execute_ conn
    "CREATE TABLE IF NOT EXISTS perps_events (\
    \id SERIAL PRIMARY KEY,\
    \chain_id BIGINT NOT NULL,\
    \release_router TEXT,\
    \contract_address TEXT NOT NULL,\
    \event_name TEXT NOT NULL,\
    \tx_hash TEXT NOT NULL,\
    \block_number BIGINT NOT NULL,\
    \block_hash TEXT NOT NULL,\
    \tx_index BIGINT NOT NULL,\
    \log_index BIGINT NOT NULL,\
    \timestamp BIGINT NOT NULL,\
    \account TEXT,\
    \order_id BIGINT,\
    \side INTEGER,\
    \data JSONB NOT NULL,\
    \created_at TIMESTAMP DEFAULT NOW(),\
    \UNIQUE (chain_id, tx_hash, log_index)\
    \)"
  _ <- execute_ conn
    "ALTER TABLE perps_events ADD COLUMN IF NOT EXISTS release_router TEXT"
  _ <- execute_ conn
    "UPDATE perps_events SET release_router = '0x0000000000000000000000000000000000000000' WHERE release_router IS NULL"
  _ <- execute_ conn
    "DO $$ \
    \BEGIN \
    \  IF EXISTS (SELECT 1 FROM information_schema.columns WHERE table_schema = current_schema() AND table_name = 'perps_events' AND column_name = 'release_router' AND is_nullable = 'YES') THEN \
    \    ALTER TABLE perps_events ALTER COLUMN release_router SET NOT NULL; \
    \  END IF; \
    \END $$"
  _ <- execute_ conn
    "CREATE INDEX IF NOT EXISTS idx_perps_events_account_block \
    \ON perps_events(account, block_number DESC, log_index DESC)"
  _ <- execute_ conn
    "CREATE INDEX IF NOT EXISTS idx_perps_events_order_id \
    \ON perps_events(chain_id, release_router, order_id)"
  _ <- execute_ conn
    "CREATE TABLE IF NOT EXISTS perps_orders (\
    \chain_id BIGINT NOT NULL,\
    \order_router TEXT,\
    \order_id BIGINT NOT NULL,\
    \account TEXT,\
    \side INTEGER,\
    \commit_tx_hash TEXT,\
    \commit_block_number BIGINT,\
    \commit_timestamp BIGINT,\
    \terminal_tx_hash TEXT,\
    \terminal_block_number BIGINT,\
    \terminal_timestamp BIGINT,\
    \terminal_status TEXT NOT NULL DEFAULT 'Committed',\
    \failure_reason TEXT,\
    \execution_price NUMERIC,\
    \execution_vpi_usdc NUMERIC,\
    \execution_frozen_close_spread_usdc NUMERIC,\
    \execution_economics_version INTEGER,\
    \execution_oracle_price NUMERIC,\
    \execution_oracle_frozen BOOLEAN,\
    \oracle_min_publish_time BIGINT,\
    \oracle_max_publish_time BIGINT,\
    \oracle_derivation_version INTEGER,\
    \execution_evidence_last_attempt_at TIMESTAMP,\
    \cleanup_actor TEXT,\
    \updated_at TIMESTAMP DEFAULT NOW(),\
    \PRIMARY KEY (chain_id, order_router, order_id)\
    \)"
  _ <- execute_ conn
    "ALTER TABLE perps_orders ADD COLUMN IF NOT EXISTS order_router TEXT"
  _ <- execute_ conn
    "ALTER TABLE perps_orders ADD COLUMN IF NOT EXISTS execution_vpi_usdc NUMERIC"
  _ <- execute_ conn
    "ALTER TABLE perps_orders ADD COLUMN IF NOT EXISTS execution_frozen_close_spread_usdc NUMERIC"
  _ <- execute_ conn
    "ALTER TABLE perps_orders ADD COLUMN IF NOT EXISTS execution_economics_version INTEGER"
  _ <- execute_ conn
    "ALTER TABLE perps_orders ADD COLUMN IF NOT EXISTS execution_oracle_price NUMERIC"
  _ <- execute_ conn
    "ALTER TABLE perps_orders ADD COLUMN IF NOT EXISTS execution_oracle_frozen BOOLEAN"
  _ <- execute_ conn
    "ALTER TABLE perps_orders ADD COLUMN IF NOT EXISTS oracle_min_publish_time BIGINT"
  _ <- execute_ conn
    "ALTER TABLE perps_orders ADD COLUMN IF NOT EXISTS oracle_max_publish_time BIGINT"
  _ <- execute_ conn
    "ALTER TABLE perps_orders ADD COLUMN IF NOT EXISTS oracle_derivation_version INTEGER"
  _ <- execute_ conn
    "ALTER TABLE perps_orders ADD COLUMN IF NOT EXISTS execution_evidence_last_attempt_at TIMESTAMP"
  _ <- execute_ conn
    "ALTER TABLE perps_orders ADD COLUMN IF NOT EXISTS client_order_id TEXT"
  _ <- execute_ conn
    "ALTER TABLE perps_orders ADD COLUMN IF NOT EXISTS receipt_hash TEXT"
  _ <- execute_ conn
    "ALTER TABLE perps_orders ADD COLUMN IF NOT EXISTS terminal_reason TEXT"
  _ <- execute_ conn
    "ALTER TABLE perps_orders ADD COLUMN IF NOT EXISTS pending_reason TEXT"
  _ <- execute_ conn
    "ALTER TABLE perps_orders ADD COLUMN IF NOT EXISTS execution_mode TEXT"
  _ <- execute_ conn
    "ALTER TABLE perps_orders ADD COLUMN IF NOT EXISTS failed_constraint TEXT"
  _ <- execute_ conn
    "ALTER TABLE perps_orders ADD COLUMN IF NOT EXISTS receipt_economics JSONB"
  _ <- execute_ conn
    "UPDATE perps_orders SET order_router = '0x0000000000000000000000000000000000000000' WHERE order_router IS NULL"
  _ <- execute_ conn
    "DO $$ \
    \BEGIN \
    \  IF EXISTS (SELECT 1 FROM information_schema.columns WHERE table_schema = current_schema() AND table_name = 'perps_orders' AND column_name = 'order_router' AND is_nullable = 'YES') THEN \
    \    ALTER TABLE perps_orders ALTER COLUMN order_router SET NOT NULL; \
    \  END IF; \
    \END $$"
  _ <- execute_ conn
    "DO $$ \
    \DECLARE pk_cols text[]; \
    \BEGIN \
    \  SELECT COALESCE(array_agg(a.attname ORDER BY cols.ordinality), ARRAY[]::text[]) INTO pk_cols \
    \  FROM pg_constraint c \
    \  JOIN pg_class t ON t.oid = c.conrelid \
    \  JOIN pg_namespace n ON n.oid = t.relnamespace \
    \  JOIN unnest(c.conkey) WITH ORDINALITY AS cols(attnum, ordinality) ON TRUE \
    \  JOIN pg_attribute a ON a.attrelid = t.oid AND a.attnum = cols.attnum \
    \  WHERE c.contype = 'p' AND n.nspname = current_schema() AND t.relname = 'perps_orders'; \
    \  IF pk_cols <> ARRAY['chain_id', 'order_router', 'order_id']::text[] THEN \
    \    ALTER TABLE perps_orders DROP CONSTRAINT IF EXISTS perps_orders_pkey; \
    \    ALTER TABLE perps_orders ADD CONSTRAINT perps_orders_pkey PRIMARY KEY (chain_id, order_router, order_id); \
    \  END IF; \
    \END $$"
  _ <- execute_ conn
    "CREATE INDEX IF NOT EXISTS idx_perps_orders_account_block \
    \ON perps_orders(chain_id, order_router, account, COALESCE(terminal_block_number, commit_block_number) DESC)"
  _ <- execute_ conn
    "CREATE TABLE IF NOT EXISTS perps_account_activity (\
    \id SERIAL PRIMARY KEY,\
    \chain_id BIGINT NOT NULL,\
    \release_router TEXT,\
    \contract_address TEXT,\
    \event_key TEXT NOT NULL UNIQUE,\
    \account TEXT NOT NULL,\
    \actor TEXT,\
    \activity_type TEXT NOT NULL,\
    \order_id BIGINT,\
    \side INTEGER,\
    \price NUMERIC,\
    \size_delta NUMERIC,\
    \amount_usdc NUMERIC,\
    \pnl_usdc NUMERIC,\
    \tx_hash TEXT NOT NULL,\
    \block_number BIGINT NOT NULL,\
    \block_hash TEXT NOT NULL,\
    \tx_index BIGINT NOT NULL,\
    \log_index BIGINT NOT NULL,\
    \timestamp BIGINT NOT NULL,\
    \data JSONB NOT NULL,\
    \created_at TIMESTAMP DEFAULT NOW()\
    \)"
  _ <- execute_ conn
    "ALTER TABLE perps_account_activity ADD COLUMN IF NOT EXISTS release_router TEXT"
  _ <- execute_ conn
    "ALTER TABLE perps_account_activity ADD COLUMN IF NOT EXISTS contract_address TEXT"
  _ <- execute_ conn
    "UPDATE perps_account_activity SET release_router = '0x0000000000000000000000000000000000000000' WHERE release_router IS NULL"
  _ <- execute_ conn
    "DO $$ \
    \BEGIN \
    \  IF EXISTS (SELECT 1 FROM information_schema.columns WHERE table_schema = current_schema() AND table_name = 'perps_account_activity' AND column_name = 'release_router' AND is_nullable = 'YES') THEN \
    \    ALTER TABLE perps_account_activity ALTER COLUMN release_router SET NOT NULL; \
    \  END IF; \
    \END $$"
  _ <- execute_ conn
    "UPDATE perps_account_activity SET contract_address = LOWER(TRIM(contract_address)) \
    \WHERE contract_address IS NOT NULL AND contract_address <> LOWER(TRIM(contract_address))"
  -- Only exact, already-indexed event matches are safe to migrate. Rows without
  -- independently retained emitter provenance intentionally remain NULL.
  _ <- execute_ conn
    "UPDATE perps_account_activity a SET contract_address = LOWER(e.contract_address) \
    \FROM perps_events e \
    \WHERE a.contract_address IS NULL \
    \AND e.chain_id = a.chain_id AND e.release_router = a.release_router \
    \AND e.tx_hash = a.tx_hash AND e.log_index = a.log_index \
    \AND e.block_number = a.block_number AND e.block_hash = a.block_hash"
  _ <- execute_ conn
    "CREATE INDEX IF NOT EXISTS idx_perps_account_activity_account_block \
    \ON perps_account_activity(chain_id, release_router, account, block_number DESC, log_index DESC)"
  _ <- execute_ conn
    "CREATE INDEX IF NOT EXISTS idx_perps_account_activity_chain_timestamp \
    \ON perps_account_activity(chain_id, release_router, timestamp DESC)"
  _ <- execute_ conn
    "CREATE INDEX IF NOT EXISTS idx_perps_account_activity_flow_source \
    \ON perps_account_activity(chain_id, release_router, contract_address, activity_type, block_number)"
  _ <- execute_ conn
    "CREATE INDEX IF NOT EXISTS idx_perps_account_activity_open_accounts \
    \ON perps_account_activity(chain_id, release_router, account, block_number) \
    \WHERE activity_type = 'Open'"
  _ <- execute_ conn
    "CREATE TABLE IF NOT EXISTS perps_usdc_transfers (\
    \chain_id BIGINT NOT NULL,\
    \release_router TEXT NOT NULL,\
    \token_address TEXT NOT NULL,\
    \from_address TEXT NOT NULL,\
    \to_address TEXT NOT NULL,\
    \amount NUMERIC(78,0) NOT NULL,\
    \tx_hash TEXT NOT NULL,\
    \block_number BIGINT NOT NULL,\
    \block_hash TEXT NOT NULL,\
    \tx_index BIGINT NOT NULL,\
    \log_index BIGINT NOT NULL,\
    \timestamp BIGINT NOT NULL,\
    \created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),\
    \PRIMARY KEY (chain_id, release_router, tx_hash, log_index),\
    \CHECK (chain_id > 0 AND amount >= 0 AND block_number >= 0 AND tx_index >= 0 AND log_index >= 0 AND timestamp >= 0),\
    \CHECK (release_router ~ '^0x[0-9a-f]{40}$' AND token_address ~ '^0x[0-9a-f]{40}$'),\
    \CHECK (from_address ~ '^0x[0-9a-f]{40}$' AND to_address ~ '^0x[0-9a-f]{40}$'),\
    \CHECK (tx_hash ~ '^0x[0-9a-f]{64}$' AND block_hash ~ '^0x[0-9a-f]{64}$')\
    \)"
  _ <- execute_ conn
    "CREATE INDEX IF NOT EXISTS idx_perps_usdc_transfers_inbound \
    \ON perps_usdc_transfers(chain_id, release_router, token_address, to_address, block_number, tx_index, log_index)"
  _ <- execute_ conn
    "CREATE INDEX IF NOT EXISTS idx_perps_usdc_transfers_outbound \
    \ON perps_usdc_transfers(chain_id, release_router, token_address, from_address, block_number, tx_index, log_index)"
  _ <- execute_ conn
    "DO $$ BEGIN \
    \ IF NOT EXISTS (SELECT 1 FROM pg_constraint WHERE conrelid = 'perps_usdc_transfers'::regclass \
    \  AND conname = 'perps_usdc_transfers_canonical_values') THEN \
    \  ALTER TABLE perps_usdc_transfers ADD CONSTRAINT perps_usdc_transfers_canonical_values \
    \  CHECK (chain_id > 0 AND amount >= 0 AND block_number >= 0 AND tx_index >= 0 AND log_index >= 0 AND timestamp >= 0); \
    \ END IF; \
    \ IF NOT EXISTS (SELECT 1 FROM pg_constraint WHERE conrelid = 'perps_usdc_transfers'::regclass \
    \  AND conname = 'perps_usdc_transfers_canonical_addresses') THEN \
    \  ALTER TABLE perps_usdc_transfers ADD CONSTRAINT perps_usdc_transfers_canonical_addresses \
    \  CHECK (release_router ~ '^0x[0-9a-f]{40}$' AND token_address ~ '^0x[0-9a-f]{40}$' \
    \   AND from_address ~ '^0x[0-9a-f]{40}$' AND to_address ~ '^0x[0-9a-f]{40}$'); \
    \ END IF; \
    \ IF NOT EXISTS (SELECT 1 FROM pg_constraint WHERE conrelid = 'perps_usdc_transfers'::regclass \
    \  AND conname = 'perps_usdc_transfers_canonical_hashes') THEN \
    \  ALTER TABLE perps_usdc_transfers ADD CONSTRAINT perps_usdc_transfers_canonical_hashes \
    \  CHECK (tx_hash ~ '^0x[0-9a-f]{64}$' AND block_hash ~ '^0x[0-9a-f]{64}$'); \
    \ END IF; END $$"
  _ <- execute_ conn
    "CREATE TABLE IF NOT EXISTS perps_indexer_state (\
    \indexer_name TEXT NOT NULL,\
    \chain_id BIGINT NOT NULL,\
    \release_router TEXT,\
    \configured_start_block BIGINT,\
    \last_indexed_block BIGINT NOT NULL,\
    \last_indexed_block_hash TEXT,\
    \updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),\
    \PRIMARY KEY (indexer_name, chain_id),\
    \CONSTRAINT perps_indexer_state_release_scope CHECK (\
    \ indexer_name NOT LIKE 'perps-history-costs-v1:%' OR\
    \ (release_router IS NOT NULL AND configured_start_block > 0)),\
    \CONSTRAINT perps_indexer_state_v2_release_scope CHECK (\
    \ indexer_name NOT LIKE 'perps-history-costs-v2:%' OR\
    \ (release_router IS NOT NULL AND configured_start_block > 0))\
    \)"
  _ <- execute_ conn
    "ALTER TABLE perps_indexer_state ADD COLUMN IF NOT EXISTS release_router TEXT"
  _ <- execute_ conn
    "ALTER TABLE perps_indexer_state ADD COLUMN IF NOT EXISTS configured_start_block BIGINT"
  _ <- execute_ conn
    "ALTER TABLE perps_indexer_state ADD COLUMN IF NOT EXISTS last_indexed_timestamp BIGINT"
  _ <- execute_ conn
    "DO $$ BEGIN\
    \ IF EXISTS (SELECT 1 FROM information_schema.columns WHERE table_schema = current_schema()\
    \   AND table_name = 'perps_indexer_state' AND column_name = 'updated_at'\
    \   AND data_type = 'timestamp without time zone') THEN\
    \   ALTER TABLE perps_indexer_state ALTER COLUMN updated_at TYPE TIMESTAMPTZ\
    \     USING updated_at AT TIME ZONE 'UTC';\
    \ END IF; END $$"
  _ <- execute_ conn
    "UPDATE perps_indexer_state SET updated_at = NOW() WHERE updated_at IS NULL"
  _ <- execute_ conn
    "ALTER TABLE perps_indexer_state ALTER COLUMN updated_at SET DEFAULT NOW(),\
    \ ALTER COLUMN updated_at SET NOT NULL"
  _ <- execute_ conn
    "DO $$ BEGIN\
    \ IF NOT EXISTS (SELECT 1 FROM pg_constraint WHERE conrelid = 'perps_indexer_state'::regclass\
    \   AND conname = 'perps_indexer_state_release_scope') THEN\
    \   ALTER TABLE perps_indexer_state ADD CONSTRAINT perps_indexer_state_release_scope CHECK (\
    \     indexer_name NOT LIKE 'perps-history-costs-v1:%' OR\
    \     (release_router IS NOT NULL AND configured_start_block > 0)) NOT VALID;\
    \ END IF;\
    \ IF NOT EXISTS (SELECT 1 FROM pg_constraint WHERE conrelid = 'perps_indexer_state'::regclass\
    \   AND conname = 'perps_indexer_state_v2_release_scope') THEN\
    \   ALTER TABLE perps_indexer_state ADD CONSTRAINT perps_indexer_state_v2_release_scope CHECK (\
    \     indexer_name NOT LIKE 'perps-history-costs-v2:%' OR\
    \     (release_router IS NOT NULL AND configured_start_block > 0)) NOT VALID;\
    \ END IF; END $$"
  pure ()

insertPerpsEvent
  :: Connection
  -> Integer
  -> Text
  -> Text
  -> Text
  -> Text
  -> Integer
  -> Text
  -> Integer
  -> Integer
  -> Integer
  -> Maybe Text
  -> Maybe Integer
  -> Maybe Int
  -> Value
  -> IO ()
insertPerpsEvent conn chainId releaseRouter contractAddress eventName txHash blockNumber blockHash txIndex logIndex timestamp account orderId side payload = do
  _ <- execute conn
    "INSERT INTO perps_events \
    \(chain_id, release_router, contract_address, event_name, tx_hash, block_number, block_hash, tx_index, log_index, timestamp, account, order_id, side, data) \
    \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) \
    \ON CONFLICT (chain_id, tx_hash, log_index) DO NOTHING"
    ( chainId
    , normalizeRouter releaseRouter
    , T.toLower contractAddress
    , eventName
    , T.toLower txHash
    , blockNumber
    , T.toLower blockHash
    , txIndex
    , logIndex
    , timestamp
    , fmap T.toLower account
    , orderId
    , side
    , encode payload
    )
  pure ()

upsertPerpsOrderCommitted
  :: Connection
  -> Integer
  -> Text
  -> Integer
  -> Text
  -> Int
  -> Text
  -> Integer
  -> Integer
  -> IO ()
upsertPerpsOrderCommitted conn chainId orderRouter orderId account side txHash blockNumber timestamp = do
  _ <- execute conn
    "INSERT INTO perps_orders \
    \(chain_id, order_router, order_id, account, side, commit_tx_hash, commit_block_number, commit_timestamp, terminal_status) \
    \VALUES (?, ?, ?, ?, ?, ?, ?, ?, 'Committed') \
    \ON CONFLICT (chain_id, order_router, order_id) DO UPDATE SET \
    \account = COALESCE(perps_orders.account, EXCLUDED.account),\
    \side = COALESCE(perps_orders.side, EXCLUDED.side),\
    \commit_tx_hash = COALESCE(perps_orders.commit_tx_hash, EXCLUDED.commit_tx_hash),\
    \commit_block_number = COALESCE(perps_orders.commit_block_number, EXCLUDED.commit_block_number),\
    \commit_timestamp = COALESCE(perps_orders.commit_timestamp, EXCLUDED.commit_timestamp),\
    \updated_at = NOW()"
    (chainId, normalizeRouter orderRouter, orderId, T.toLower account, side, T.toLower txHash, blockNumber, timestamp)
  pure ()

updatePerpsOrderLifecycleIdentity
  :: Connection
  -> Integer
  -> Text
  -> Integer
  -> Text
  -> IO ()
updatePerpsOrderLifecycleIdentity conn chainId orderRouter orderId clientOrderId = do
  _ <- execute conn
    "UPDATE perps_orders SET client_order_id = ?, updated_at = NOW() \
    \WHERE chain_id = ? AND order_router = ? AND order_id = ?"
    (T.toLower clientOrderId, chainId, normalizeRouter orderRouter, orderId)
  pure ()

updatePerpsOrderLifecycleReceipt
  :: Connection
  -> Integer
  -> Text
  -> Integer
  -> Text
  -> Text
  -> Text
  -> Text
  -> Text
  -> Maybe Text
  -> Value
  -> IO ()
updatePerpsOrderLifecycleReceipt
  conn chainId orderRouter orderId account clientOrderId receiptHash terminalReason
  executionMode failedConstraint economics = do
  let normalizedAccount = T.toLower account
      executionOracleFrozen = executionModeOracleFrozen executionMode
  affected <- execute conn
    updatePerpsOrderLifecycleReceiptSql
    ( normalizedAccount
    , T.toLower clientOrderId
    , T.toLower receiptHash
    , terminalReason
    , executionMode
    , executionOracleFrozen
    , failedConstraint
    , encode economics
    , chainId
    , normalizeRouter orderRouter
    , orderId
    , normalizedAccount
    )
  unless (affected == 1) $
    fail "Lifecycle receipt account conflicts with the canonical order identity"

updatePerpsOrderLifecycleReceiptSql :: Query
updatePerpsOrderLifecycleReceiptSql =
  "UPDATE perps_orders SET \
  \account = COALESCE(perps_orders.account, ?), client_order_id = ?, \
  \receipt_hash = ?, terminal_reason = ?, pending_reason = NULL, \
  \execution_mode = ?, execution_oracle_frozen = ?, \
  \failed_constraint = ?, receipt_economics = ?, \
  \execution_economics_version = 2, updated_at = NOW() \
  \WHERE chain_id = ? AND order_router = ? AND order_id = ? \
  \AND (account IS NULL OR account = ?)"

executionModeOracleFrozen :: Text -> Maybe Bool
executionModeOracleFrozen mode =
  case T.toLower mode of
    "live" -> Just False
    "fad" -> Just False
    "frozen" -> Just True
    _ -> Nothing

upsertPerpsOrderTerminal
  :: Connection
  -> Integer
  -> Text
  -> Integer
  -> Text
  -> Maybe Text
  -> Maybe Integer
  -> Maybe Text
  -> Text
  -> Integer
  -> Integer
  -> IO ()
upsertPerpsOrderTerminal
  conn
  chainId
  orderRouter
  orderId
  status
  failureReason
  executionPrice
  cleanupActor
  txHash
  blockNumber
  timestamp = do
  _ <- execute conn
    "INSERT INTO perps_orders \
    \(chain_id, order_router, order_id, terminal_tx_hash, terminal_block_number, terminal_timestamp, terminal_status, failure_reason, execution_price, cleanup_actor) \
    \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?) \
    \ON CONFLICT (chain_id, order_router, order_id) DO UPDATE SET \
    \terminal_tx_hash = EXCLUDED.terminal_tx_hash,\
    \terminal_block_number = EXCLUDED.terminal_block_number,\
    \terminal_timestamp = EXCLUDED.terminal_timestamp,\
    \terminal_status = EXCLUDED.terminal_status,\
    \failure_reason = EXCLUDED.failure_reason,\
    \execution_price = EXCLUDED.execution_price,\
    \execution_vpi_usdc = CASE WHEN \
    \  perps_orders.terminal_status = EXCLUDED.terminal_status \
    \  AND perps_orders.terminal_tx_hash = EXCLUDED.terminal_tx_hash \
    \  AND perps_orders.terminal_block_number = EXCLUDED.terminal_block_number \
    \  THEN perps_orders.execution_vpi_usdc ELSE NULL END,\
    \execution_frozen_close_spread_usdc = CASE WHEN \
    \  perps_orders.terminal_status = EXCLUDED.terminal_status \
    \  AND perps_orders.terminal_tx_hash = EXCLUDED.terminal_tx_hash \
    \  AND perps_orders.terminal_block_number = EXCLUDED.terminal_block_number \
    \  THEN perps_orders.execution_frozen_close_spread_usdc ELSE NULL END,\
    \execution_economics_version = CASE WHEN \
    \  perps_orders.terminal_status = EXCLUDED.terminal_status \
    \  AND perps_orders.terminal_tx_hash = EXCLUDED.terminal_tx_hash \
    \  AND perps_orders.terminal_block_number = EXCLUDED.terminal_block_number \
    \  THEN perps_orders.execution_economics_version ELSE NULL END,\
    \execution_oracle_price = CASE WHEN \
    \  perps_orders.terminal_status = EXCLUDED.terminal_status \
    \  AND perps_orders.terminal_tx_hash = EXCLUDED.terminal_tx_hash \
    \  AND perps_orders.terminal_block_number = EXCLUDED.terminal_block_number \
    \  THEN perps_orders.execution_oracle_price ELSE NULL END,\
    \execution_oracle_frozen = CASE WHEN \
    \  perps_orders.terminal_status = EXCLUDED.terminal_status \
    \  AND perps_orders.terminal_tx_hash = EXCLUDED.terminal_tx_hash \
    \  AND perps_orders.terminal_block_number = EXCLUDED.terminal_block_number \
    \  THEN perps_orders.execution_oracle_frozen ELSE NULL END,\
    \oracle_min_publish_time = CASE WHEN \
    \  perps_orders.terminal_status = EXCLUDED.terminal_status \
    \  AND perps_orders.terminal_tx_hash = EXCLUDED.terminal_tx_hash \
    \  AND perps_orders.terminal_block_number = EXCLUDED.terminal_block_number \
    \  THEN perps_orders.oracle_min_publish_time ELSE NULL END,\
    \oracle_max_publish_time = CASE WHEN \
    \  perps_orders.terminal_status = EXCLUDED.terminal_status \
    \  AND perps_orders.terminal_tx_hash = EXCLUDED.terminal_tx_hash \
    \  AND perps_orders.terminal_block_number = EXCLUDED.terminal_block_number \
    \  THEN perps_orders.oracle_max_publish_time ELSE NULL END,\
    \oracle_derivation_version = CASE WHEN \
    \  perps_orders.terminal_status = EXCLUDED.terminal_status \
    \  AND perps_orders.terminal_tx_hash = EXCLUDED.terminal_tx_hash \
    \  AND perps_orders.terminal_block_number = EXCLUDED.terminal_block_number \
    \  THEN perps_orders.oracle_derivation_version ELSE NULL END,\
    \execution_evidence_last_attempt_at = CASE WHEN \
    \  perps_orders.terminal_status = EXCLUDED.terminal_status \
    \  AND perps_orders.terminal_tx_hash = EXCLUDED.terminal_tx_hash \
    \  AND perps_orders.terminal_block_number = EXCLUDED.terminal_block_number \
    \  THEN perps_orders.execution_evidence_last_attempt_at ELSE NULL END,\
    \cleanup_actor = CASE WHEN \
    \  perps_orders.terminal_status = EXCLUDED.terminal_status \
    \  AND perps_orders.terminal_tx_hash = EXCLUDED.terminal_tx_hash \
    \  AND perps_orders.terminal_block_number = EXCLUDED.terminal_block_number \
    \  AND EXCLUDED.cleanup_actor IS NULL \
    \  THEN perps_orders.cleanup_actor ELSE EXCLUDED.cleanup_actor END,\
    \updated_at = NOW()"
    ( chainId
    , normalizeRouter orderRouter
    , orderId
    , T.toLower txHash
    , blockNumber
    , timestamp
    , status
    , failureReason
    , executionPrice
    , fmap T.toLower cleanupActor
    )
  pure ()

getPendingPerpsExecutionEvidence
  :: Connection
  -> Integer
  -> Text
  -> Int
  -> Int
  -> Int
  -> IO [PerpsExecutionEvidenceRow]
getPendingPerpsExecutionEvidence
  conn chainId orderRouter oracleDerivationVersion economicsDerivationVersion limit = do
  let (recentLimit, backlogLimit) = perpsExecutionEvidenceLaneLimits limit
  query conn
    pendingPerpsExecutionEvidenceSql
    ( chainId
    , normalizeRouter orderRouter
    , oracleDerivationVersion
    , economicsDerivationVersion
    , recentLimit
    , backlogLimit
    )

pendingPerpsExecutionEvidenceSql :: Query
pendingPerpsExecutionEvidenceSql =
  "WITH eligible AS (\
  \  SELECT o.order_id, o.commit_timestamp, o.terminal_tx_hash, o.terminal_block_number, e.block_hash, \
  \    o.oracle_derivation_version, o.execution_economics_version, o.execution_evidence_last_attempt_at \
  \  FROM perps_orders o \
  \  JOIN perps_events e ON \
  \    e.chain_id = o.chain_id \
  \    AND e.release_router = o.order_router \
  \    AND e.contract_address = o.order_router \
  \    AND e.tx_hash = o.terminal_tx_hash \
  \    AND e.block_number = o.terminal_block_number \
  \    AND e.order_id = o.order_id \
  \    AND e.event_name = 'OrderExecuted' \
  \  WHERE o.chain_id = ? AND o.order_router = ? \
  \    AND o.terminal_status = 'Executed' \
  \    AND (o.oracle_derivation_version IS DISTINCT FROM ? \
  \      OR o.execution_economics_version IS NULL \
  \      OR o.execution_economics_version < ?) \
  \    AND (o.execution_evidence_last_attempt_at IS NULL \
  \      OR o.execution_evidence_last_attempt_at < NOW() - INTERVAL '5 minutes')\
  \), recent AS (\
  \  SELECT eligible.*, 0 AS lane, \
  \    ROW_NUMBER() OVER (ORDER BY terminal_block_number DESC, order_id DESC) AS lane_order \
  \  FROM eligible \
  \  WHERE execution_evidence_last_attempt_at IS NULL \
  \  ORDER BY terminal_block_number DESC, order_id DESC \
  \  LIMIT ?\
  \), backlog AS (\
  \  SELECT eligible.*, 1 AS lane, \
  \    ROW_NUMBER() OVER (ORDER BY execution_evidence_last_attempt_at ASC NULLS FIRST, terminal_block_number ASC, order_id ASC) AS lane_order \
  \  FROM eligible \
  \  WHERE NOT EXISTS (\
  \    SELECT 1 FROM recent \
  \    WHERE recent.order_id = eligible.order_id \
  \      AND recent.terminal_tx_hash = eligible.terminal_tx_hash \
  \      AND recent.terminal_block_number = eligible.terminal_block_number\
  \  ) \
  \  ORDER BY execution_evidence_last_attempt_at ASC NULLS FIRST, terminal_block_number ASC, order_id ASC \
  \  LIMIT ?\
  \) \
  \SELECT order_id, commit_timestamp, terminal_tx_hash, terminal_block_number, block_hash, \
  \  oracle_derivation_version, execution_economics_version \
  \FROM (\
  \  SELECT * FROM recent \
  \  UNION ALL \
  \  SELECT * FROM backlog\
  \) queued \
  \ORDER BY lane ASC, lane_order ASC"

perpsExecutionEvidenceLaneLimits :: Int -> (Int, Int)
perpsExecutionEvidenceLaneLimits requestedLimit =
  (recentLimit, batchLimit - recentLimit)
  where
    batchLimit = max 0 requestedLimit
    recentLimit = min 2 batchLimit

markPerpsExecutionEvidenceAttempt
  :: Connection
  -> Integer
  -> Text
  -> Integer
  -> Text
  -> Integer
  -> Text
  -> IO ()
markPerpsExecutionEvidenceAttempt
  conn chainId orderRouter orderId txHash blockNumber blockHash = do
  _ <- execute conn
    "UPDATE perps_orders SET execution_evidence_last_attempt_at = NOW(), updated_at = NOW() \
    \WHERE chain_id = ? AND order_router = ? AND order_id = ? \
    \  AND terminal_status = 'Executed' AND terminal_tx_hash = ? AND terminal_block_number = ? \
    \  AND EXISTS (SELECT 1 FROM perps_events e \
    \    WHERE e.chain_id = perps_orders.chain_id \
    \      AND e.release_router = perps_orders.order_router \
    \      AND e.contract_address = perps_orders.order_router \
    \      AND e.order_id = perps_orders.order_id \
    \      AND e.event_name = 'OrderExecuted' \
    \      AND e.tx_hash = perps_orders.terminal_tx_hash \
    \      AND e.block_number = perps_orders.terminal_block_number \
    \      AND e.block_hash = ?)"
    ( chainId
    , normalizeRouter orderRouter
    , orderId
    , T.toLower txHash
    , blockNumber
    , T.toLower blockHash
    )
  pure ()

updatePerpsOrderOracleEvidence
  :: Connection
  -> Integer
  -> Text
  -> Integer
  -> Text
  -> Integer
  -> Text
  -> Maybe Integer
  -> Maybe Bool
  -> Maybe Integer
  -> Maybe Integer
  -> Int
  -> IO ()
updatePerpsOrderOracleEvidence
  conn chainId orderRouter orderId txHash blockNumber blockHash
  executionOraclePrice executionOracleFrozen oracleMinPublishTime
  oracleMaxPublishTime derivationVersion = do
    _ <- execute conn
      "UPDATE perps_orders SET execution_oracle_price = ?, execution_oracle_frozen = ?, \
      \oracle_min_publish_time = ?, oracle_max_publish_time = ?, \
      \oracle_derivation_version = ?, updated_at = NOW() \
      \WHERE chain_id = ? AND order_router = ? AND order_id = ? \
      \  AND terminal_status = 'Executed' AND terminal_tx_hash = ? AND terminal_block_number = ? \
      \  AND EXISTS (SELECT 1 FROM perps_events e \
      \    WHERE e.chain_id = perps_orders.chain_id \
      \      AND e.release_router = perps_orders.order_router \
      \      AND e.contract_address = perps_orders.order_router \
      \      AND e.order_id = perps_orders.order_id \
      \      AND e.event_name = 'OrderExecuted' \
      \      AND e.tx_hash = perps_orders.terminal_tx_hash \
      \      AND e.block_number = perps_orders.terminal_block_number \
      \      AND e.block_hash = ?)"
      ( executionOraclePrice
      , executionOracleFrozen
      , oracleMinPublishTime
      , oracleMaxPublishTime
      , derivationVersion
      , chainId
      , normalizeRouter orderRouter
      , orderId
      , T.toLower txHash
      , blockNumber
      , T.toLower blockHash
      )
    pure ()

updatePerpsOrderEconomicsEvidence
  :: Connection
  -> Integer
  -> Text
  -> Integer
  -> Text
  -> Integer
  -> Text
  -> Integer
  -> Maybe Integer
  -> Int
  -> IO ()
updatePerpsOrderEconomicsEvidence
  conn chainId orderRouter orderId txHash blockNumber blockHash
  vpiUsdc frozenCloseSpreadUsdc derivationVersion = do
    _ <- execute conn
      "UPDATE perps_orders SET execution_vpi_usdc = ?, execution_frozen_close_spread_usdc = ?, \
      \execution_economics_version = ?, updated_at = NOW() \
      \WHERE chain_id = ? AND order_router = ? AND order_id = ? \
      \  AND terminal_status = 'Executed' AND terminal_tx_hash = ? AND terminal_block_number = ? \
      \  AND EXISTS (SELECT 1 FROM perps_events e \
      \    WHERE e.chain_id = perps_orders.chain_id \
      \      AND e.release_router = perps_orders.order_router \
      \      AND e.contract_address = perps_orders.order_router \
      \      AND e.order_id = perps_orders.order_id \
      \      AND e.event_name = 'OrderExecuted' \
      \      AND e.tx_hash = perps_orders.terminal_tx_hash \
      \      AND e.block_number = perps_orders.terminal_block_number \
      \      AND e.block_hash = ?)"
      ( vpiUsdc
      , frozenCloseSpreadUsdc
      , derivationVersion
      , chainId
      , normalizeRouter orderRouter
      , orderId
      , T.toLower txHash
      , blockNumber
      , T.toLower blockHash
      )
    pure ()

insertPerpsActivity
  :: Connection
  -> Integer
  -> Text
  -> Text
  -> Text
  -> Text
  -> Text
  -> Maybe Text
  -> Maybe Integer
  -> Maybe Int
  -> Maybe Integer
  -> Maybe Integer
  -> Maybe Integer
  -> Maybe Integer
  -> Text
  -> Integer
  -> Text
  -> Integer
  -> Integer
  -> Integer
  -> Value
  -> IO ()
insertPerpsActivity conn chainId releaseRouter contractAddress eventKey account activityType actor orderId side price sizeDelta amountUsdc pnlUsdc txHash blockNumber blockHash txIndex logIndex timestamp payload = do
  _ <- execute conn
    "INSERT INTO perps_account_activity \
    \(chain_id, release_router, contract_address, event_key, account, actor, activity_type, order_id, side, price, size_delta, amount_usdc, pnl_usdc, tx_hash, block_number, block_hash, tx_index, log_index, timestamp, data) \
    \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) \
    \ON CONFLICT (event_key) DO UPDATE SET \
    \contract_address = EXCLUDED.contract_address, data = EXCLUDED.data \
    \WHERE perps_account_activity.chain_id = EXCLUDED.chain_id \
    \AND perps_account_activity.release_router = EXCLUDED.release_router \
    \AND perps_account_activity.tx_hash = EXCLUDED.tx_hash \
    \AND perps_account_activity.log_index = EXCLUDED.log_index"
    ( chainId
    , normalizeRouter releaseRouter
    , normalizeRouter contractAddress
    , eventKey
    , T.toLower account
    , fmap T.toLower actor
    , activityType
    , orderId
    , side
    , price
    , sizeDelta
    , amountUsdc
    , pnlUsdc
    , T.toLower txHash
    , blockNumber
    , T.toLower blockHash
    , txIndex
    , logIndex
    , timestamp
    , encode payload
    )
  pure ()

insertPerpsUsdcTransfer
  :: Connection
  -> Integer
  -> Text
  -> Text
  -> Text
  -> Text
  -> Integer
  -> Text
  -> Integer
  -> Text
  -> Integer
  -> Integer
  -> Integer
  -> IO ()
insertPerpsUsdcTransfer conn chainId releaseRouter tokenAddress fromAddress toAddress amount txHash blockNumber blockHash txIndex logIndex timestamp = do
  affected <- execute conn
    "INSERT INTO perps_usdc_transfers \
    \(chain_id, release_router, token_address, from_address, to_address, amount, tx_hash, block_number, block_hash, tx_index, log_index, timestamp) \
    \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) \
    \ON CONFLICT (chain_id, release_router, tx_hash, log_index) DO UPDATE SET timestamp = EXCLUDED.timestamp \
    \WHERE perps_usdc_transfers.token_address = EXCLUDED.token_address \
    \AND perps_usdc_transfers.from_address = EXCLUDED.from_address AND perps_usdc_transfers.to_address = EXCLUDED.to_address \
    \AND perps_usdc_transfers.amount = EXCLUDED.amount AND perps_usdc_transfers.block_number = EXCLUDED.block_number \
    \AND perps_usdc_transfers.block_hash = EXCLUDED.block_hash AND perps_usdc_transfers.tx_index = EXCLUDED.tx_index \
    \AND perps_usdc_transfers.timestamp = EXCLUDED.timestamp"
    ( chainId
    , normalizeRouter releaseRouter
    , normalizeRouter tokenAddress
    , T.toLower fromAddress
    , T.toLower toAddress
    , amount
    , T.toLower txHash
    , blockNumber
    , T.toLower blockHash
    , txIndex
    , logIndex
    , timestamp
    )
  unless (affected == 1) $
    fail "Canonical USDC transfer conflicts with a previously indexed event identity"

perpsOrderBaseSelectSql :: Query
perpsOrderBaseSelectSql =
  "SELECT o.order_id, o.order_router, o.account, o.side, o.commit_tx_hash, o.commit_block_number, o.commit_timestamp, \
  \o.terminal_tx_hash, o.terminal_block_number, terminal_event.block_hash, o.terminal_timestamp, o.terminal_status, o.failure_reason, \
  \o.execution_price, o.execution_vpi_usdc, o.execution_frozen_close_spread_usdc, o.execution_economics_version, \
  \o.execution_oracle_price, o.execution_oracle_frozen, o.oracle_min_publish_time, o.oracle_max_publish_time, \
  \o.oracle_derivation_version, o.client_order_id, o.receipt_hash, o.terminal_reason, o.pending_reason, \
  \o.execution_mode, o.failed_constraint, CASE \
  \  WHEN o.receipt_economics IS NULL THEN NULL \
  \  WHEN o.receipt_economics->>'executionBountyUsdc' IS NOT NULL THEN o.receipt_economics \
  \  WHEN intent_event.execution_bounty_usdc IS NULL THEN o.receipt_economics \
  \  ELSE o.receipt_economics || jsonb_build_object('executionBountyUsdc', intent_event.execution_bounty_usdc) \
  \END, o.cleanup_actor, \
  \a.activity_type, a.size_delta, a.price, a.vpi_usdc, a.pnl_usdc, \
  \COALESCE(o.terminal_block_number, o.commit_block_number, 0) AS sort_block \
  \FROM perps_orders o \
  \LEFT JOIN LATERAL (\
  \  SELECT e.log_index, e.block_hash \
  \  FROM perps_events e \
  \  WHERE e.chain_id = o.chain_id AND e.release_router = o.order_router \
  \    AND e.tx_hash = o.terminal_tx_hash AND e.block_number = o.terminal_block_number AND e.order_id = o.order_id \
  \    AND e.event_name = 'OrderFinalized' \
  \  ORDER BY e.log_index ASC LIMIT 1\
  \) terminal_event ON TRUE \
  \LEFT JOIN LATERAL (\
  \  SELECT e.log_index \
  \  FROM perps_events e \
  \  WHERE e.chain_id = o.chain_id AND e.release_router = o.order_router \
  \    AND e.tx_hash = o.terminal_tx_hash AND e.event_name = 'OrderFinalized' \
  \    AND e.log_index < terminal_event.log_index \
  \  ORDER BY e.log_index DESC LIMIT 1\
  \) previous_terminal_event ON terminal_event.log_index IS NOT NULL \
  \LEFT JOIN LATERAL (\
  \  SELECT e.data->>'executionBountyUsdc' AS execution_bounty_usdc \
  \  FROM perps_events e \
  \  WHERE e.chain_id = o.chain_id AND e.release_router = o.order_router AND e.order_id = o.order_id \
  \    AND e.event_name = 'IntentRegistered' AND e.data->>'executionBountyUsdc' IS NOT NULL \
  \  ORDER BY e.block_number DESC, e.tx_index DESC, e.log_index DESC LIMIT 1\
  \) intent_event ON TRUE \
  \LEFT JOIN LATERAL (\
  \  SELECT activity_type, size_delta, price, (data->>'vpiUsdc')::numeric AS vpi_usdc, pnl_usdc \
  \  FROM perps_account_activity a \
  \  WHERE a.chain_id = o.chain_id AND a.release_router = o.order_router AND a.account = o.account AND a.tx_hash = o.terminal_tx_hash \
  \    AND o.terminal_status = 'Executed' AND terminal_event.log_index IS NOT NULL \
  \    AND a.activity_type IN ('Open', 'Close') \
  \    AND a.log_index < terminal_event.log_index \
  \    AND (previous_terminal_event.log_index IS NULL OR a.log_index > previous_terminal_event.log_index) \
  \  ORDER BY a.log_index DESC LIMIT 1\
  \) a ON TRUE \
  \WHERE o.chain_id = ? AND o.order_router = ? AND o.client_order_id IS NOT NULL"

getPerpsOrdersByAccount :: Connection -> Integer -> Text -> Text -> Int -> Maybe (Integer, Integer) -> IO [PerpsOrderRow]
getPerpsOrdersByAccount conn chainId orderRouter account limit cursor = do
  case cursor of
    Nothing ->
      query conn baseQuery (chainId, normalizeRouter orderRouter, T.toLower account, limit)
    Just (cursorBlock, cursorOrderId) ->
      query conn cursorQuery (chainId, normalizeRouter orderRouter, T.toLower account, cursorBlock, cursorBlock, cursorOrderId, limit)
  where
    baseSelect :: Query
    baseSelect = perpsOrderBaseSelectSql <> " AND o.account = ?"

    baseQuery :: Query
    baseQuery =
      baseSelect <> " \
      \ORDER BY COALESCE(o.terminal_block_number, o.commit_block_number) DESC, o.order_id DESC \
      \LIMIT ?"

    cursorQuery :: Query
    cursorQuery =
      baseSelect <> " \
      \AND (COALESCE(o.terminal_block_number, o.commit_block_number, 0) < ? \
      \  OR (COALESCE(o.terminal_block_number, o.commit_block_number, 0) = ? AND o.order_id < ?)) \
      \ORDER BY COALESCE(o.terminal_block_number, o.commit_block_number) DESC, o.order_id DESC \
      \LIMIT ?"

getPerpsOrderById :: Connection -> Integer -> Text -> Integer -> Maybe Text -> IO (Maybe PerpsOrderRow)
getPerpsOrderById conn chainId orderRouter orderId mAccount = do
  rows <- case mAccount of
    Nothing ->
      query conn baseQuery (chainId, normalizeRouter orderRouter, orderId)
    Just account ->
      query conn accountQuery (chainId, normalizeRouter orderRouter, orderId, T.toLower account)
  pure $ case rows of
    row : _ -> Just row
    [] -> Nothing
  where
    baseSelect :: Query
    baseSelect = perpsOrderBaseSelectSql <> " AND o.order_id = ?"

    baseQuery :: Query
    baseQuery =
      baseSelect <> " LIMIT 1"

    accountQuery :: Query
    accountQuery =
      baseSelect <> " AND o.account = ? LIMIT 1"

getPerpsActivityByAccount :: Connection -> Integer -> Text -> Text -> Integer -> Int -> Maybe (Integer, Integer) -> IO [PerpsActivityRow]
getPerpsActivityByAccount conn chainId releaseRouter account startBlock limit cursor = do
  case cursor of
    Nothing ->
      query conn baseQuery (chainId, normalizeRouter releaseRouter, T.toLower account, startBlock, limit)
    Just (cursorBlock, cursorLogIndex) ->
      query conn cursorQuery (chainId, normalizeRouter releaseRouter, T.toLower account, startBlock, cursorBlock, cursorBlock, cursorLogIndex, limit)
  where
    baseQuery :: Query
    baseQuery =
      "SELECT activity_type, release_router, contract_address, account, actor, order_id, side, price, size_delta, amount_usdc, pnl_usdc, \
      \tx_hash, block_number, timestamp, data, log_index \
      \FROM perps_account_activity \
      \WHERE chain_id = ? AND release_router = ? AND account = ? AND block_number >= ? \
      \ORDER BY block_number DESC, log_index DESC LIMIT ?"

    cursorQuery :: Query
    cursorQuery =
      "SELECT activity_type, release_router, contract_address, account, actor, order_id, side, price, size_delta, amount_usdc, pnl_usdc, \
      \tx_hash, block_number, timestamp, data, log_index \
      \FROM perps_account_activity \
      \WHERE chain_id = ? AND release_router = ? AND account = ? AND block_number >= ? \
      \AND (block_number < ? OR (block_number = ? AND log_index < ?)) \
      \ORDER BY block_number DESC, log_index DESC LIMIT ?"

perpsMarketVolumeExpressionSql :: Query
perpsMarketVolumeExpressionSql =
  "ABS(size_delta) * price / 100000000000000000000"

perpsMarketVolumeFilterSql :: Query
perpsMarketVolumeFilterSql =
  "FROM perps_account_activity \
  \WHERE chain_id = ? \
  \AND release_router = ? \
  \AND timestamp >= ? \
  \AND activity_type IN ('Open', 'Close', 'Liquidated') \
  \AND size_delta IS NOT NULL \
  \AND price IS NOT NULL"

perpsMarketVolumeSinceSql :: Query
perpsMarketVolumeSinceSql =
  "SELECT FLOOR(COALESCE(SUM("
    <> perpsMarketVolumeExpressionSql
    <> "), 0)) "
    <> perpsMarketVolumeFilterSql

perpsMarketVolumeBucketsSql :: Query
perpsMarketVolumeBucketsSql =
  "SELECT timestamp / ? AS bucket, FLOOR(COALESCE(SUM("
    <> perpsMarketVolumeExpressionSql
    <> "), 0)) "
    <> perpsMarketVolumeFilterSql
    <> " AND timestamp <= ? GROUP BY bucket ORDER BY bucket ASC"

getPerpsMarketVolumeSince :: Connection -> Integer -> Text -> Integer -> IO Integer
getPerpsMarketVolumeSince conn chainId releaseRouter fromTimestamp = do
  rows <- query conn perpsMarketVolumeSinceSql
    (chainId, normalizeRouter releaseRouter, fromTimestamp)
  case rows of
    [Only (Just value)] -> pure $ scientificToInteger value
    _ -> pure 0

getPerpsMarketVolumeBuckets
  :: Connection
  -> Integer -- chain ID
  -> Text    -- release router
  -> Integer -- from timestamp
  -> Integer -- to timestamp
  -> Integer -- interval seconds
  -> IO [PerpsMarketVolumeBucketRow]
getPerpsMarketVolumeBuckets conn chainId releaseRouter fromTimestamp toTimestamp intervalSeconds =
  query
    conn
    perpsMarketVolumeBucketsSql
    (max 1 intervalSeconds, chainId, normalizeRouter releaseRouter, fromTimestamp, toTimestamp)

getPerpsOrderAccountSide :: Connection -> Integer -> Text -> Integer -> IO (Maybe (Text, Maybe Int))
getPerpsOrderAccountSide conn chainId orderRouter orderId = do
  rows <- query conn
    "SELECT account, side FROM perps_orders WHERE chain_id = ? AND order_router = ? AND order_id = ?"
    (chainId, normalizeRouter orderRouter, orderId)
  case rows of
    [(Just account, side)] -> pure $ Just (account, side)
    _ -> pure Nothing

insertPerpsExpiredCleanupActivityIfReady :: Connection -> Integer -> Text -> Integer -> IO ()
insertPerpsExpiredCleanupActivityIfReady conn chainId orderRouter orderId = do
  rows <- query conn
    "SELECT o.account, o.side, o.cleanup_actor, e.contract_address, e.tx_hash, e.block_number, e.block_hash, \
    \e.tx_index, e.log_index, e.timestamp \
    \FROM perps_orders o \
    \JOIN perps_events e ON e.chain_id = o.chain_id AND e.release_router = o.order_router AND e.order_id = o.order_id AND e.event_name = 'OrderFailed' \
    \WHERE o.chain_id = ? AND o.order_router = ? AND o.order_id = ? AND o.terminal_status = 'Expired / Cleaned up' \
    \AND o.account IS NOT NULL \
    \ORDER BY e.block_number DESC, e.log_index DESC LIMIT 1"
    (chainId, normalizeRouter orderRouter, orderId)
  case rows of
    [(Just account, side, actor, contractAddress, txHash, blockNumber, blockHash, txIndex, logIndex, timestamp)] ->
      insertPerpsActivity conn chainId orderRouter contractAddress (cleanupActivityKey txHash logIndex orderId) account
        "Cleaned up expired order" actor (Just orderId) side Nothing Nothing Nothing Nothing
        txHash blockNumber blockHash txIndex logIndex timestamp
        (object ["orderId" .= show orderId, "reason" .= ("Expired" :: Text), "actor" .= actor])
    _ -> pure ()
  where
    cleanupActivityKey :: Text -> Integer -> Integer -> Text
    cleanupActivityKey txHash logIndex orderId' =
      T.intercalate ":"
        [ T.toLower txHash
        , T.pack $ show logIndex
        , "cleanup"
        , T.pack $ show orderId'
        ]

getPerpsIndexerStatus :: Connection -> Integer -> Text -> Text -> IO (Maybe PerpsIndexerStatusRow)
getPerpsIndexerStatus conn chainId indexerName releaseRouter = do
  let scopedName = scopedIndexerName indexerName releaseRouter
  rows <- query conn
    "SELECT ?::text AS indexer_name, chain_id, release_router, last_indexed_block, last_indexed_block_hash \
    \FROM perps_indexer_state WHERE chain_id = ? AND indexer_name = ?"
    (indexerName, chainId, scopedName)
  case rows of
    [row] -> pure $ Just row
    _ -> pure Nothing

getPerpsIndexerLastBlock :: Connection -> Integer -> Text -> Text -> IO (Integer, Maybe Text)
getPerpsIndexerLastBlock conn chainId indexerName releaseRouter = do
  statusRow <- getPerpsIndexerStatus conn chainId indexerName releaseRouter
  pure $ case statusRow of
    Just row -> (pisLastIndexedBlock row, pisLastIndexedBlockHash row)
    Nothing -> (0, Nothing)

-- Replay limits are transaction-local so a pooled connection cannot leak an
-- administrative timeout into the long-running worker after rollback/commit.
-- This must run after BEGIN and before waiting for either advisory lock.
configurePerpsReplayTransaction :: Connection -> Int -> Int -> IO ()
configurePerpsReplayTransaction conn statementTimeoutMs lockTimeoutMs = do
  statementRows <-
    query
      conn
      "SELECT set_config('statement_timeout', ?, TRUE)"
      (Only $ T.pack (show statementTimeoutMs) <> "ms")
      :: IO [Only Text]
  lockRows <-
    query
      conn
      "SELECT set_config('lock_timeout', ?, TRUE)"
      (Only $ T.pack (show lockTimeoutMs) <> "ms")
      :: IO [Only Text]
  unless (length statementRows == 1 && length lockRows == 1) $
    fail "Could not configure the bounded Perps replay transaction timeouts"

-- The live indexer, its reorg recovery, and bounded replay all take this lock
-- first. Dataset-specific candle locks come afterwards, giving every writer a
-- single lock order and preventing a replay from racing cursor advancement.
lockPerpsIndexerTransaction :: Connection -> Integer -> Text -> Text -> IO ()
lockPerpsIndexerTransaction conn chainId indexerName releaseRouter = do
  rows <-
    query
      conn
      "SELECT 1::BIGINT FROM (SELECT pg_advisory_xact_lock(hashtextextended(?, ?))) locked"
      ("perps-indexer:" <> scopedIndexerName indexerName releaseRouter, chainId)
      :: IO [Only Integer]
  unless (length rows == 1) $
    fail "Could not acquire the Perps indexer transaction lock"

setPerpsIndexerState :: Connection -> Integer -> Text -> Text -> Integer -> Integer -> Maybe Text -> IO ()
setPerpsIndexerState conn chainId indexerName releaseRouter configuredStartBlock blockNumber blockHash =
  setPerpsIndexerStateWithTimestamp conn chainId indexerName releaseRouter configuredStartBlock blockNumber blockHash Nothing

-- Only a contiguous, canonical append may supply the block timestamp. Legacy
-- updates and reorg rewinds deliberately clear it rather than retain old proof.
setPerpsIndexerStateWithTimestamp :: Connection -> Integer -> Text -> Text -> Integer -> Integer -> Maybe Text -> Maybe Integer -> IO ()
setPerpsIndexerStateWithTimestamp conn chainId indexerName releaseRouter configuredStartBlock blockNumber blockHash blockTimestamp = do
  let scopedName = scopedIndexerName indexerName releaseRouter
  unless (configuredStartBlock > 0) $
    fail "Perps indexer configured start block must be positive"
  affected <- execute conn
    "INSERT INTO perps_indexer_state \
    \(indexer_name, chain_id, release_router, configured_start_block, last_indexed_block, last_indexed_block_hash, last_indexed_timestamp) \
    \VALUES (?, ?, ?, ?, ?, ?, ?) \
    \ON CONFLICT (indexer_name, chain_id) DO UPDATE SET \
    \ release_router = EXCLUDED.release_router,\
    \ configured_start_block = COALESCE(perps_indexer_state.configured_start_block, EXCLUDED.configured_start_block),\
    \ last_indexed_block = EXCLUDED.last_indexed_block,\
    \ last_indexed_block_hash = EXCLUDED.last_indexed_block_hash,\
    \ last_indexed_timestamp = EXCLUDED.last_indexed_timestamp,\
    \ updated_at = NOW() \
    \WHERE perps_indexer_state.configured_start_block IS NULL \
    \   OR perps_indexer_state.configured_start_block = EXCLUDED.configured_start_block"
    ( scopedName
    , chainId
    , normalizeRouter releaseRouter
    , configuredStartBlock
    , blockNumber
    , fmap T.toLower blockHash
    , blockTimestamp
    )
  unless (affected == 1) $
    fail "Immutable Perps indexer configured start block mismatch; refusing to mix release history"

scopedIndexerName :: Text -> Text -> Text
scopedIndexerName indexerName releaseRouter =
  indexerName <> ":" <> normalizeRouter releaseRouter

deletePerpsHistoryFromBlock :: Connection -> Integer -> Text -> Integer -> IO ()
deletePerpsHistoryFromBlock conn chainId releaseRouter blockNumber = do
  _ <- execute conn
    "DELETE FROM perps_usdc_transfers WHERE chain_id = ? AND release_router = ? AND block_number >= ?"
    (chainId, normalizeRouter releaseRouter, blockNumber)
  _ <- execute conn
    "DELETE FROM perps_account_activity WHERE chain_id = ? AND release_router = ? AND block_number >= ?"
    (chainId, normalizeRouter releaseRouter, blockNumber)
  _ <- execute conn
    "DELETE FROM perps_events WHERE chain_id = ? AND release_router = ? AND block_number >= ?"
    (chainId, normalizeRouter releaseRouter, blockNumber)
  _ <- execute conn
    "UPDATE perps_orders SET terminal_tx_hash = NULL, terminal_block_number = NULL, terminal_timestamp = NULL, \
    \terminal_status = 'Committed', failure_reason = NULL, execution_price = NULL, execution_vpi_usdc = NULL, \
    \execution_frozen_close_spread_usdc = NULL, execution_economics_version = NULL, execution_oracle_price = NULL, \
    \execution_oracle_frozen = NULL, \
    \oracle_min_publish_time = NULL, oracle_max_publish_time = NULL, oracle_derivation_version = NULL, \
    \execution_evidence_last_attempt_at = NULL, cleanup_actor = NULL, updated_at = NOW() \
    \WHERE chain_id = ? AND order_router = ? AND terminal_block_number >= ?"
    (chainId, normalizeRouter releaseRouter, blockNumber)
  _ <- execute conn
    "DELETE FROM perps_orders WHERE chain_id = ? AND order_router = ? AND commit_block_number >= ?"
    (chainId, normalizeRouter releaseRouter, blockNumber)
  pure ()
