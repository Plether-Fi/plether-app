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
  , PythUpdatePayloadRow (..)
  , promotePythPayloadSource
  , isAdmittedPythPayloadSource
  , isHistoricalRevealPayload
  , isHistoricalRevealPayloadSource
  , ensurePerpsKeeperSchema
  , tryPerpsKeeperLock
  , unlockPerpsKeeperLock
  , getPerpsKeeperLastIndexedBlock
  , setPerpsKeeperLastIndexedBlock
  , upsertPerpsKeeperOrderCommitted
  , markPerpsKeeperOrderExecuted
  , markPerpsKeeperOrderFailed
  , recordPerpsKeeperOrderAttempt
  , recordPerpsKeeperOrderError
  , recordPerpsKeeperOrderImmediateRetryError
  , getPendingPerpsKeeperOrders
  , PerpsKeeperOrderRow (..)
  , PerpsKeeperTerminalOrderRow (..)
  , getPerpsKeeperOrderById
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
  , getPendingPerpsLiquidationCandidate
  , markPerpsLiquidationCandidateChecked
  , recordPerpsLiquidationCandidatePending
  , recordPerpsLiquidationCandidateBroadcastAttempt
  , clearPerpsLiquidationCandidatePending
  , recordPerpsLiquidationCandidateError
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
  , deletePerpsHistoryFromBlock
  ) where

import Control.Monad (unless)
import Data.Aeson (Value, encode, object, (.=))
import qualified Data.ByteString.Lazy as LBS
import Data.Scientific (Scientific, base10Exponent, coefficient)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Int (Int64)
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
  )
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import Database.PostgreSQL.Simple.Internal (RowParser)
import Database.PostgreSQL.Simple.ToField (ToField (..))
import Database.PostgreSQL.Simple.ToRow (ToRow (..))
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
  pure ()

keeperLockId :: Int
keeperLockId = 421614485

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

getPendingPerpsLiquidationCandidate :: Connection -> Integer -> Text -> Int -> Int -> IO (Maybe PerpsLiquidationCandidateRow)
getPendingPerpsLiquidationCandidate conn chainId cfdEngine replacementSeconds broadcastRetrySeconds = do
  rows <-
    query
      conn
      "SELECT account, attempt_count, last_error, pending_tx_hash, pending_nonce, pending_sender, pending_raw_tx, \
      \pending_call_data, pending_value, pending_gas_limit, pending_max_priority_fee_per_gas, \
      \pending_max_fee_per_gas, \
      \COALESCE(pending_since <= NOW() - (? * INTERVAL '1 second'), FALSE), \
      \COALESCE(pending_last_broadcast_at <= NOW() - (? * INTERVAL '1 second'), TRUE) \
      \FROM perps_liquidation_candidates \
      \WHERE chain_id = ? AND cfd_engine = ? AND pending_tx_hash IS NOT NULL \
      \ORDER BY pending_since ASC, account ASC LIMIT 1"
      (max 1 replacementSeconds, max 1 broadcastRetrySeconds, chainId, normalizeRouter cfdEngine)
  pure $ case rows of
    candidate : _ -> Just candidate
    [] -> Nothing

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
    \ (release_router IS NOT NULL AND configured_start_block > 0))\
    \)"
  _ <- execute_ conn
    "ALTER TABLE perps_indexer_state ADD COLUMN IF NOT EXISTS release_router TEXT"
  _ <- execute_ conn
    "ALTER TABLE perps_indexer_state ADD COLUMN IF NOT EXISTS configured_start_block BIGINT"
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
  \      OR o.execution_economics_version IS DISTINCT FROM ?) \
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
  \o.oracle_derivation_version, o.cleanup_actor, a.activity_type, a.size_delta, a.price, a.vpi_usdc, a.pnl_usdc, \
  \COALESCE(o.terminal_block_number, o.commit_block_number, 0) AS sort_block \
  \FROM perps_orders o \
  \LEFT JOIN LATERAL (\
  \  SELECT e.log_index, e.block_hash \
  \  FROM perps_events e \
  \  WHERE e.chain_id = o.chain_id AND e.release_router = o.order_router \
  \    AND e.contract_address = o.order_router \
  \    AND e.tx_hash = o.terminal_tx_hash AND e.block_number = o.terminal_block_number AND e.order_id = o.order_id \
  \    AND e.event_name = CASE WHEN o.terminal_status = 'Executed' THEN 'OrderExecuted' ELSE 'OrderFailed' END \
  \  ORDER BY e.log_index ASC LIMIT 1\
  \) terminal_event ON TRUE \
  \LEFT JOIN LATERAL (\
  \  SELECT e.log_index \
  \  FROM perps_events e \
  \  WHERE e.chain_id = o.chain_id AND e.release_router = o.order_router \
  \    AND e.contract_address = o.order_router \
  \    AND e.tx_hash = o.terminal_tx_hash AND e.event_name IN ('OrderExecuted', 'OrderFailed') \
  \    AND e.log_index < terminal_event.log_index \
  \  ORDER BY e.log_index DESC LIMIT 1\
  \) previous_terminal_event ON terminal_event.log_index IS NOT NULL \
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
  \WHERE o.chain_id = ? AND o.order_router = ?"

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

getPerpsActivityByAccount :: Connection -> Integer -> Text -> Text -> Int -> Maybe (Integer, Integer) -> IO [PerpsActivityRow]
getPerpsActivityByAccount conn chainId releaseRouter account limit cursor = do
  case cursor of
    Nothing ->
      query conn baseQuery (chainId, normalizeRouter releaseRouter, T.toLower account, limit)
    Just (cursorBlock, cursorLogIndex) ->
      query conn cursorQuery (chainId, normalizeRouter releaseRouter, T.toLower account, cursorBlock, cursorBlock, cursorLogIndex, limit)
  where
    baseQuery :: Query
    baseQuery =
      "SELECT activity_type, release_router, contract_address, account, actor, order_id, side, price, size_delta, amount_usdc, pnl_usdc, \
      \tx_hash, block_number, timestamp, data, log_index \
      \FROM perps_account_activity \
      \WHERE chain_id = ? AND release_router = ? AND account = ? \
      \ORDER BY block_number DESC, log_index DESC LIMIT ?"

    cursorQuery :: Query
    cursorQuery =
      "SELECT activity_type, release_router, contract_address, account, actor, order_id, side, price, size_delta, amount_usdc, pnl_usdc, \
      \tx_hash, block_number, timestamp, data, log_index \
      \FROM perps_account_activity \
      \WHERE chain_id = ? AND release_router = ? AND account = ? \
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
setPerpsIndexerState conn chainId indexerName releaseRouter configuredStartBlock blockNumber blockHash = do
  let scopedName = scopedIndexerName indexerName releaseRouter
  unless (configuredStartBlock > 0) $
    fail "Perps indexer configured start block must be positive"
  affected <- execute conn
    "INSERT INTO perps_indexer_state \
    \(indexer_name, chain_id, release_router, configured_start_block, last_indexed_block, last_indexed_block_hash) \
    \VALUES (?, ?, ?, ?, ?, ?) \
    \ON CONFLICT (indexer_name, chain_id) DO UPDATE SET \
    \ release_router = EXCLUDED.release_router,\
    \ configured_start_block = COALESCE(perps_indexer_state.configured_start_block, EXCLUDED.configured_start_block),\
    \ last_indexed_block = EXCLUDED.last_indexed_block,\
    \ last_indexed_block_hash = EXCLUDED.last_indexed_block_hash,\
    \ updated_at = NOW() \
    \WHERE perps_indexer_state.configured_start_block IS NULL \
    \   OR perps_indexer_state.configured_start_block = EXCLUDED.configured_start_block"
    ( scopedName
    , chainId
    , normalizeRouter releaseRouter
    , configuredStartBlock
    , blockNumber
    , fmap T.toLower blockHash
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
