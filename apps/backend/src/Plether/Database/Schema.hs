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
  , getBasketSnapshots
  , getBasketSnapshotTimes
  , getLatestBasketSnapshot
  , getLatestBasketSnapshotTime
  , BasketSnapshotRow (..)
  , insertPythUpdatePayload
  , getPythUpdatePayloadForWindow
  , PythUpdatePayloadRow (..)
  , ensurePerpsKeeperSchema
  , tryPerpsKeeperLock
  , unlockPerpsKeeperLock
  , getPerpsKeeperLastIndexedBlock
  , setPerpsKeeperLastIndexedBlock
  , upsertPerpsOrderCommitted
  , markPerpsOrderExecuted
  , markPerpsOrderFailed
  , recordPerpsOrderAttempt
  , recordPerpsOrderError
  , getPendingPerpsOrders
  , PerpsOrderRow (..)
  ) where

import Data.Aeson (Value, encode)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import Database.PostgreSQL.Simple
  ( Connection
  , Only (..)
  , Query
  , execute
  , execute_
  , query
  , query_
  )
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
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
  ensurePerpsKeeperSchema conn
  pure ()

ensurePerpsKeeperSchema :: Connection -> IO ()
ensurePerpsKeeperSchema conn = do
  _ <- execute_ conn
    "CREATE TABLE IF NOT EXISTS perps_keeper_state (\
    \id INTEGER PRIMARY KEY DEFAULT 1,\
    \last_indexed_block BIGINT NOT NULL,\
    \updated_at TIMESTAMP DEFAULT NOW(),\
    \CONSTRAINT perps_keeper_state_single_row CHECK (id = 1)\
    \)"
  _ <- execute_ conn
    "INSERT INTO perps_keeper_state (id, last_indexed_block) \
    \VALUES (1, 0) ON CONFLICT (id) DO NOTHING"
  _ <- execute_ conn
    "CREATE TABLE IF NOT EXISTS perps_orders (\
    \order_id NUMERIC(20,0) PRIMARY KEY,\
    \account VARCHAR(42) NOT NULL,\
    \side INTEGER NOT NULL,\
    \commit_block BIGINT NOT NULL,\
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
    \updated_at TIMESTAMP DEFAULT NOW()\
    \)"
  _ <- execute_ conn
    "CREATE INDEX IF NOT EXISTS idx_perps_orders_pending \
    \ON perps_orders(order_id ASC) WHERE status = 'pending'"
  _ <- execute_ conn
    "CREATE INDEX IF NOT EXISTS idx_perps_orders_commit_block \
    \ON perps_orders(commit_block DESC)"
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
  _ <- execute conn
    "INSERT INTO perps_basket_snapshots \
    \(timestamp, interval_seconds, basket_price, component_prices, source) \
    \VALUES (?, ?, ?, ?, ?) \
    \ON CONFLICT (timestamp, interval_seconds) DO UPDATE SET \
    \basket_price = EXCLUDED.basket_price, \
    \component_prices = EXCLUDED.component_prices, \
    \source = EXCLUDED.source"
    (timestamp, intervalSeconds, basketPrice, encode components, source)
  pure ()

getBasketSnapshots
  :: Connection
  -> Integer -- from timestamp
  -> Integer -- to timestamp
  -> Integer -- interval seconds
  -> Int     -- limit
  -> IO [BasketSnapshotRow]
getBasketSnapshots conn fromTimestamp toTimestamp intervalSeconds limit = do
  query conn
    "SELECT timestamp, interval_seconds, basket_price, component_prices \
    \FROM perps_basket_snapshots \
    \WHERE timestamp >= ? AND timestamp <= ? AND interval_seconds = ? \
    \ORDER BY timestamp ASC LIMIT ?"
    (fromTimestamp, toTimestamp, intervalSeconds, limit)

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

insertPythUpdatePayload
  :: Connection
  -> Integer -- min publish time
  -> Integer -- max publish time
  -> Value   -- publish_times
  -> Value   -- update_data
  -> Integer -- fetched_at
  -> Text    -- source
  -> IO ()
insertPythUpdatePayload conn minPublishTime maxPublishTime publishTimes updateData fetchedAt source = do
  _ <- execute conn
    "INSERT INTO perps_pyth_update_payloads \
    \(min_publish_time, max_publish_time, publish_times, update_data, source, fetched_at) \
    \VALUES (?, ?, ?, ?, ?, ?) \
    \ON CONFLICT (min_publish_time, max_publish_time) DO UPDATE SET \
    \publish_times = EXCLUDED.publish_times, \
    \update_data = EXCLUDED.update_data, \
    \source = EXCLUDED.source, \
    \fetched_at = EXCLUDED.fetched_at"
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
    \WHERE min_publish_time >= ? AND max_publish_time <= ? \
    \ORDER BY min_publish_time ASC LIMIT 1"
    (minPublishTime, maxPublishTime)
  case rows of
    [row] -> pure $ Just row
    _ -> pure Nothing

data PerpsOrderRow = PerpsOrderRow
  { porOrderId :: Integer
  , porAccount :: Text
  , porSide :: Integer
  , porCommitBlock :: Integer
  , porCommitTime :: Integer
  , porCommitTxHash :: Text
  , porStatus :: Text
  , porAttemptCount :: Int
  , porLastError :: Maybe Text
  }
  deriving stock (Show, Generic)

instance FromRow PerpsOrderRow where
  fromRow =
    PerpsOrderRow
      <$> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field

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

getPerpsKeeperLastIndexedBlock :: Connection -> IO Integer
getPerpsKeeperLastIndexedBlock conn = do
  result <- query_ conn "SELECT last_indexed_block FROM perps_keeper_state WHERE id = 1" :: IO [Only Integer]
  case result of
    [Only block] -> pure block
    _ -> pure 0

setPerpsKeeperLastIndexedBlock :: Connection -> Integer -> IO ()
setPerpsKeeperLastIndexedBlock conn block = do
  _ <- execute conn
    "INSERT INTO perps_keeper_state (id, last_indexed_block, updated_at) \
    \VALUES (1, ?, NOW()) \
    \ON CONFLICT (id) DO UPDATE SET \
    \last_indexed_block = EXCLUDED.last_indexed_block, \
    \updated_at = NOW()"
    (Only block)
  pure ()

upsertPerpsOrderCommitted
  :: Connection
  -> Integer -- order_id
  -> Text    -- account
  -> Integer -- side
  -> Integer -- commit_block
  -> Integer -- commit_time
  -> Text    -- commit_tx_hash
  -> IO ()
upsertPerpsOrderCommitted conn orderId account side commitBlock commitTime commitTxHash = do
  _ <- execute conn
    "INSERT INTO perps_orders \
    \(order_id, account, side, commit_block, commit_time, commit_tx_hash, status) \
    \VALUES (?, ?, ?, ?, ?, ?, 'pending') \
    \ON CONFLICT (order_id) DO NOTHING"
    (orderId, T.toLower account, side, commitBlock, commitTime, T.toLower commitTxHash)
  pure ()

markPerpsOrderExecuted
  :: Connection
  -> Integer -- order_id
  -> Text    -- tx_hash
  -> Integer -- block_number
  -> Integer -- execution_price
  -> IO ()
markPerpsOrderExecuted conn orderId txHash blockNumber executionPrice = do
  _ <- execute conn
    "UPDATE perps_orders SET \
    \status = 'executed', \
    \execution_tx_hash = ?, \
    \execution_block = ?, \
    \execution_price = ?, \
    \last_error = NULL, \
    \updated_at = NOW() \
    \WHERE order_id = ?"
    (T.toLower txHash, blockNumber, executionPrice, orderId)
  pure ()

markPerpsOrderFailed
  :: Connection
  -> Integer -- order_id
  -> Text    -- tx_hash
  -> Integer -- block_number
  -> Integer -- failure_reason
  -> IO ()
markPerpsOrderFailed conn orderId txHash blockNumber failureReason = do
  _ <- execute conn
    "UPDATE perps_orders SET \
    \status = 'failed', \
    \failure_tx_hash = ?, \
    \failure_block = ?, \
    \failure_reason = ?, \
    \last_error = NULL, \
    \updated_at = NOW() \
    \WHERE order_id = ?"
    (T.toLower txHash, blockNumber, failureReason, orderId)
  pure ()

recordPerpsOrderAttempt :: Connection -> Integer -> IO ()
recordPerpsOrderAttempt conn orderId = do
  _ <- execute conn
    "UPDATE perps_orders SET \
    \attempt_count = attempt_count + 1, \
    \last_attempt_at = NOW(), \
    \updated_at = NOW() \
    \WHERE order_id = ?"
    (Only orderId)
  pure ()

recordPerpsOrderError :: Connection -> Integer -> Text -> IO ()
recordPerpsOrderError conn orderId err = do
  _ <- execute conn
    "UPDATE perps_orders SET \
    \last_error = ?, \
    \updated_at = NOW() \
    \WHERE order_id = ?"
    (err, orderId)
  pure ()

getPendingPerpsOrders :: Connection -> Int -> IO [PerpsOrderRow]
getPendingPerpsOrders conn limitRows =
  query conn
    "SELECT order_id, account, side, commit_block, commit_time, commit_tx_hash, \
    \status, attempt_count, last_error \
    \FROM perps_orders \
    \WHERE status = 'pending' \
    \AND (last_attempt_at IS NULL OR last_attempt_at < NOW() - INTERVAL '5 seconds') \
    \ORDER BY order_id ASC LIMIT ?"
    (Only limitRows)
