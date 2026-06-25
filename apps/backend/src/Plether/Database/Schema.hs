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
  , getLatestPythUpdatePayload
  , PythUpdatePayloadRow (..)
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
  , ensurePerpsHistorySchema
  , ensureTestnetFaucetSchema
  , TestnetFaucetClaimRow (..)
  , getTestnetFaucetClaim
  , beginTestnetFaucetClaim
  , markTestnetFaucetClaimSuccess
  , markTestnetFaucetClaimFailed
  , PerpsOrderRow (..)
  , PerpsActivityRow (..)
  , PerpsIndexerStatusRow (..)
  , insertPerpsEvent
  , upsertPerpsOrderCommitted
  , upsertPerpsOrderTerminal
  , insertPerpsActivity
  , getPerpsOrdersByAccount
  , getPerpsOrderById
  , getPerpsActivityByAccount
  , getPerpsMarketVolumeSince
  , getPerpsOrderAccountSide
  , insertPerpsExpiredCleanupActivityIfReady
  , getPerpsIndexerStatus
  , getPerpsIndexerLastBlock
  , setPerpsIndexerState
  , deletePerpsHistoryFromBlock
  ) where

import Data.Aeson (Value, encode, object, (.=))
import qualified Data.ByteString.Lazy as LBS
import Data.Scientific (Scientific, base10Exponent, coefficient)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Int (Int64)
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

ensureTestnetFaucetSchema :: Connection -> IO ()
ensureTestnetFaucetSchema conn = do
  _ <- execute_ conn
    "CREATE TABLE IF NOT EXISTS testnet_faucet_claims (\
    \address VARCHAR(42) NOT NULL,\
    \amount BIGINT NOT NULL,\
    \token_address VARCHAR(42) NOT NULL,\
    \tx_hash VARCHAR(66),\
    \status VARCHAR(16) NOT NULL,\
    \error TEXT,\
    \created_at TIMESTAMP DEFAULT NOW(),\
    \updated_at TIMESTAMP DEFAULT NOW(),\
    \PRIMARY KEY (address, token_address)\
    \)"
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
    "SELECT address, amount, token_address, tx_hash, status, error \
    \FROM testnet_faucet_claims WHERE address = ? AND token_address = ?"
    (T.toLower address, T.toLower tokenAddress)
  pure $ case rows of
    [row] -> Just row
    _ -> Nothing

beginTestnetFaucetClaim :: Connection -> Text -> Integer -> Text -> IO Bool
beginTestnetFaucetClaim conn address amount tokenAddress = do
  affected <- execute conn
    "INSERT INTO testnet_faucet_claims \
    \(address, amount, token_address, status, error, updated_at) \
    \VALUES (?, ?, ?, 'pending', NULL, NOW()) \
    \ON CONFLICT (address, token_address) DO UPDATE SET \
    \amount = EXCLUDED.amount,\
    \tx_hash = NULL,\
    \status = 'pending',\
    \error = NULL,\
    \updated_at = NOW() \
    \WHERE testnet_faucet_claims.status = 'failed'"
    (T.toLower address, amount, T.toLower tokenAddress)
  pure $ affected > (0 :: Int64)

markTestnetFaucetClaimSuccess :: Connection -> Text -> Text -> Text -> IO ()
markTestnetFaucetClaimSuccess conn address tokenAddress txHash = do
  _ <- execute conn
    "UPDATE testnet_faucet_claims SET \
    \tx_hash = ?, status = 'success', error = NULL, updated_at = NOW() \
    \WHERE address = ? AND token_address = ?"
    (txHash, T.toLower address, T.toLower tokenAddress)
  pure ()

markTestnetFaucetClaimFailed :: Connection -> Text -> Text -> Text -> IO ()
markTestnetFaucetClaimFailed conn address tokenAddress err = do
  _ <- execute conn
    "UPDATE testnet_faucet_claims SET \
    \status = 'failed', error = ?, updated_at = NOW() \
    \WHERE address = ? AND token_address = ?"
    (err, T.toLower address, T.toLower tokenAddress)
  pure ()

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

isHistoricalRevealPayload :: PythUpdatePayloadRow -> Bool
isHistoricalRevealPayload =
  isHistoricalRevealPayloadSource . puprSource

isHistoricalRevealPayloadSource :: Text -> Bool
isHistoricalRevealPayloadSource source =
  source
    `elem` [ "backend_hermes_historical"
           , "backend_hermes_reveal_backfill"
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
    \WHERE perps_pyth_update_payloads.source NOT IN ('backend_hermes_historical', 'backend_hermes_reveal_backfill') \
    \OR EXCLUDED.source IN ('backend_hermes_historical', 'backend_hermes_reveal_backfill')"
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
    \ORDER BY max_publish_time DESC LIMIT 1"
  case rows of
    [row] -> pure $ Just row
    _ -> pure Nothing

data PerpsKeeperOrderRow = PerpsKeeperOrderRow
  { pkorOrderId :: Integer
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

data PerpsKeeperTerminalOrderRow = PerpsKeeperTerminalOrderRow
  { pktoOrderId :: Integer
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
      <*> numericIntegerField
      <*> field
      <*> field
      <*> field

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
    "CREATE TABLE IF NOT EXISTS perps_keeper_orders (\
    \order_id BIGINT PRIMARY KEY,\
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
    \updated_at TIMESTAMP DEFAULT NOW()\
    \)"
  _ <- execute_ conn
    "ALTER TABLE perps_keeper_orders \
    \ALTER COLUMN order_id TYPE BIGINT USING order_id::bigint"
  _ <- execute_ conn
    "ALTER TABLE perps_keeper_orders \
    \ADD COLUMN IF NOT EXISTS commit_event_block BIGINT"
  _ <- execute_ conn
    "CREATE INDEX IF NOT EXISTS idx_perps_keeper_orders_pending \
    \ON perps_keeper_orders(order_id ASC) WHERE status = 'pending'"
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

upsertPerpsKeeperOrderCommitted
  :: Connection
  -> Integer -- order_id
  -> Text    -- account
  -> Integer -- side
  -> Integer -- commit_block
  -> Integer -- commit_event_block
  -> Integer -- commit_time
  -> Text    -- commit_tx_hash
  -> IO ()
upsertPerpsKeeperOrderCommitted conn orderId account side commitBlock commitEventBlock commitTime commitTxHash = do
  _ <- execute conn
    "INSERT INTO perps_keeper_orders \
    \(order_id, account, side, commit_block, commit_event_block, commit_time, commit_tx_hash, status) \
    \VALUES (?, ?, ?, ?, ?, ?, ?, 'pending') \
    \ON CONFLICT (order_id) DO UPDATE SET \
    \commit_event_block = COALESCE(perps_keeper_orders.commit_event_block, EXCLUDED.commit_event_block)"
    (orderId, T.toLower account, side, commitBlock, commitEventBlock, commitTime, T.toLower commitTxHash)
  pure ()

markPerpsKeeperOrderExecuted
  :: Connection
  -> Integer -- order_id
  -> Text    -- tx_hash
  -> Integer -- block_number
  -> Integer -- execution_price
  -> IO ()
markPerpsKeeperOrderExecuted conn orderId txHash blockNumber executionPrice = do
  _ <- execute conn
    "UPDATE perps_keeper_orders SET \
    \status = 'executed', \
    \execution_tx_hash = ?, \
    \execution_block = ?, \
    \execution_price = ?, \
    \last_error = NULL, \
    \updated_at = NOW() \
    \WHERE order_id = ?"
    (T.toLower txHash, blockNumber, executionPrice, orderId)
  pure ()

markPerpsKeeperOrderFailed
  :: Connection
  -> Integer -- order_id
  -> Text    -- tx_hash
  -> Integer -- block_number
  -> Integer -- failure_reason
  -> IO ()
markPerpsKeeperOrderFailed conn orderId txHash blockNumber failureReason = do
  _ <- execute conn
    "UPDATE perps_keeper_orders SET \
    \status = 'failed', \
    \failure_tx_hash = ?, \
    \failure_block = ?, \
    \failure_reason = ?, \
    \last_error = NULL, \
    \updated_at = NOW() \
    \WHERE order_id = ?"
    (T.toLower txHash, blockNumber, failureReason, orderId)
  pure ()

recordPerpsKeeperOrderAttempt :: Connection -> Integer -> IO ()
recordPerpsKeeperOrderAttempt conn orderId = do
  _ <- execute conn
    "UPDATE perps_keeper_orders SET \
    \attempt_count = attempt_count + 1, \
    \last_attempt_at = NOW(), \
    \updated_at = NOW() \
    \WHERE order_id = ?"
    (Only orderId)
  pure ()

recordPerpsKeeperOrderError :: Connection -> Integer -> Text -> IO ()
recordPerpsKeeperOrderError conn orderId err = do
  _ <- execute conn
    "UPDATE perps_keeper_orders SET \
    \last_error = ?, \
    \updated_at = NOW() \
    \WHERE order_id = ?"
    (err, orderId)
  pure ()

recordPerpsKeeperOrderImmediateRetryError :: Connection -> Integer -> Text -> IO ()
recordPerpsKeeperOrderImmediateRetryError conn orderId err = do
  _ <- execute conn
    "UPDATE perps_keeper_orders SET \
    \last_error = ?, \
    \last_attempt_at = NULL, \
    \updated_at = NOW() \
    \WHERE order_id = ?"
    (err, orderId)
  pure ()

getPendingPerpsKeeperOrders :: Connection -> Int -> IO [PerpsKeeperOrderRow]
getPendingPerpsKeeperOrders conn limitRows =
  query conn
    "SELECT order_id, account, side, commit_block, commit_time, commit_tx_hash, \
    \status, attempt_count, last_error \
    \FROM perps_keeper_orders \
    \WHERE status = 'pending' \
    \AND (last_attempt_at IS NULL OR last_attempt_at < NOW() - INTERVAL '5 seconds') \
    \ORDER BY order_id ASC LIMIT ?"
    (Only limitRows)

getPerpsKeeperOrderById :: Connection -> Integer -> Maybe Text -> IO (Maybe PerpsKeeperTerminalOrderRow)
getPerpsKeeperOrderById conn orderId mAccount = do
  rows <- case mAccount of
    Nothing ->
      query conn baseQuery (Only orderId)
    Just account ->
      query conn accountQuery (orderId, T.toLower account)
  pure $ case rows of
    row : _ -> Just row
    [] -> Nothing
  where
    baseSelect :: Query
    baseSelect =
      "SELECT order_id, account, side, commit_block, commit_event_block, commit_time, commit_tx_hash, status, \
      \execution_tx_hash, execution_block, execution_price, failure_tx_hash, failure_block, failure_reason \
      \FROM perps_keeper_orders \
      \WHERE order_id = ?"

    baseQuery :: Query
    baseQuery =
      baseSelect <> " LIMIT 1"

    accountQuery :: Query
    accountQuery =
      baseSelect <> " AND account = ? LIMIT 1"

data PerpsOrderRow = PerpsOrderRow
  { porOrderId :: Integer
  , porAccount :: Maybe Text
  , porSide :: Maybe Int
  , porCommitTxHash :: Maybe Text
  , porCommitBlockNumber :: Maybe Integer
  , porCommitTimestamp :: Maybe Integer
  , porTerminalTxHash :: Maybe Text
  , porTerminalBlockNumber :: Maybe Integer
  , porTerminalTimestamp :: Maybe Integer
  , porTerminalStatus :: Text
  , porFailureReason :: Maybe Text
  , porExecutionPrice :: Maybe Integer
  , porCleanupActor :: Maybe Text
  , porActivityType :: Maybe Text
  , porActivitySizeDelta :: Maybe Integer
  , porActivityPrice :: Maybe Integer
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
    <*> numericIntegerField
    <*> field
    <*> field
    <*> numericIntegerField
    <*> numericIntegerField
    <*> numericIntegerField
    <*> field

data PerpsActivityRow = PerpsActivityRow
  { parActivityType :: Text
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
    <*> numericIntegerField
    <*> numericIntegerField
    <*> numericIntegerField
    <*> numericIntegerField
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field

numericIntegerField :: RowParser (Maybe Integer)
numericIntegerField = fmap scientificToInteger <$> (field :: RowParser (Maybe Scientific))

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

ensurePerpsHistorySchema :: Connection -> IO ()
ensurePerpsHistorySchema conn = do
  _ <- execute_ conn
    "CREATE TABLE IF NOT EXISTS perps_events (\
    \id SERIAL PRIMARY KEY,\
    \chain_id BIGINT NOT NULL,\
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
    "CREATE INDEX IF NOT EXISTS idx_perps_events_account_block \
    \ON perps_events(account, block_number DESC, log_index DESC)"
  _ <- execute_ conn
    "CREATE INDEX IF NOT EXISTS idx_perps_events_order_id \
    \ON perps_events(chain_id, order_id)"
  _ <- execute_ conn
    "CREATE TABLE IF NOT EXISTS perps_orders (\
    \chain_id BIGINT NOT NULL,\
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
    \cleanup_actor TEXT,\
    \updated_at TIMESTAMP DEFAULT NOW(),\
    \PRIMARY KEY (chain_id, order_id)\
    \)"
  _ <- execute_ conn
    "CREATE INDEX IF NOT EXISTS idx_perps_orders_account_block \
    \ON perps_orders(account, COALESCE(terminal_block_number, commit_block_number) DESC)"
  _ <- execute_ conn
    "CREATE TABLE IF NOT EXISTS perps_account_activity (\
    \id SERIAL PRIMARY KEY,\
    \chain_id BIGINT NOT NULL,\
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
    "CREATE INDEX IF NOT EXISTS idx_perps_account_activity_account_block \
    \ON perps_account_activity(account, block_number DESC, log_index DESC)"
  _ <- execute_ conn
    "CREATE INDEX IF NOT EXISTS idx_perps_account_activity_chain_timestamp \
    \ON perps_account_activity(chain_id, timestamp DESC)"
  _ <- execute_ conn
    "CREATE TABLE IF NOT EXISTS perps_indexer_state (\
    \indexer_name TEXT NOT NULL,\
    \chain_id BIGINT NOT NULL,\
    \last_indexed_block BIGINT NOT NULL,\
    \last_indexed_block_hash TEXT,\
    \updated_at TIMESTAMP DEFAULT NOW(),\
    \PRIMARY KEY (indexer_name, chain_id)\
    \)"
  pure ()

insertPerpsEvent
  :: Connection
  -> Integer
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
insertPerpsEvent conn chainId contractAddress eventName txHash blockNumber blockHash txIndex logIndex timestamp account orderId side payload = do
  _ <- execute conn
    "INSERT INTO perps_events \
    \(chain_id, contract_address, event_name, tx_hash, block_number, block_hash, tx_index, log_index, timestamp, account, order_id, side, data) \
    \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) \
    \ON CONFLICT (chain_id, tx_hash, log_index) DO NOTHING"
    ( chainId
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
  -> Integer
  -> Text
  -> Int
  -> Text
  -> Integer
  -> Integer
  -> IO ()
upsertPerpsOrderCommitted conn chainId orderId account side txHash blockNumber timestamp = do
  _ <- execute conn
    "INSERT INTO perps_orders \
    \(chain_id, order_id, account, side, commit_tx_hash, commit_block_number, commit_timestamp, terminal_status) \
    \VALUES (?, ?, ?, ?, ?, ?, ?, 'Committed') \
    \ON CONFLICT (chain_id, order_id) DO UPDATE SET \
    \account = COALESCE(perps_orders.account, EXCLUDED.account),\
    \side = COALESCE(perps_orders.side, EXCLUDED.side),\
    \commit_tx_hash = COALESCE(perps_orders.commit_tx_hash, EXCLUDED.commit_tx_hash),\
    \commit_block_number = COALESCE(perps_orders.commit_block_number, EXCLUDED.commit_block_number),\
    \commit_timestamp = COALESCE(perps_orders.commit_timestamp, EXCLUDED.commit_timestamp),\
    \updated_at = NOW()"
    (chainId, orderId, T.toLower account, side, T.toLower txHash, blockNumber, timestamp)
  pure ()

upsertPerpsOrderTerminal
  :: Connection
  -> Integer
  -> Integer
  -> Text
  -> Maybe Text
  -> Maybe Integer
  -> Maybe Text
  -> Text
  -> Integer
  -> Integer
  -> IO ()
upsertPerpsOrderTerminal conn chainId orderId status failureReason executionPrice cleanupActor txHash blockNumber timestamp = do
  _ <- execute conn
    "INSERT INTO perps_orders \
    \(chain_id, order_id, terminal_tx_hash, terminal_block_number, terminal_timestamp, terminal_status, failure_reason, execution_price, cleanup_actor) \
    \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?) \
    \ON CONFLICT (chain_id, order_id) DO UPDATE SET \
    \terminal_tx_hash = EXCLUDED.terminal_tx_hash,\
    \terminal_block_number = EXCLUDED.terminal_block_number,\
    \terminal_timestamp = EXCLUDED.terminal_timestamp,\
    \terminal_status = EXCLUDED.terminal_status,\
    \failure_reason = EXCLUDED.failure_reason,\
    \execution_price = EXCLUDED.execution_price,\
    \cleanup_actor = EXCLUDED.cleanup_actor,\
    \updated_at = NOW()"
    ( chainId
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

insertPerpsActivity
  :: Connection
  -> Integer
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
insertPerpsActivity conn chainId eventKey account activityType actor orderId side price sizeDelta amountUsdc pnlUsdc txHash blockNumber blockHash txIndex logIndex timestamp payload = do
  _ <- execute conn
    "INSERT INTO perps_account_activity \
    \(chain_id, event_key, account, actor, activity_type, order_id, side, price, size_delta, amount_usdc, pnl_usdc, tx_hash, block_number, block_hash, tx_index, log_index, timestamp, data) \
    \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) \
    \ON CONFLICT (event_key) DO NOTHING"
    ( chainId
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

getPerpsOrdersByAccount :: Connection -> Integer -> Text -> Int -> Maybe (Integer, Integer) -> IO [PerpsOrderRow]
getPerpsOrdersByAccount conn chainId account limit cursor = do
  case cursor of
    Nothing ->
      query conn baseQuery (chainId, T.toLower account, limit)
    Just (cursorBlock, cursorOrderId) ->
      query conn cursorQuery (chainId, T.toLower account, cursorBlock, cursorBlock, cursorOrderId, limit)
  where
    baseSelect :: Query
    baseSelect =
      "SELECT o.order_id, o.account, o.side, o.commit_tx_hash, o.commit_block_number, o.commit_timestamp, \
      \o.terminal_tx_hash, o.terminal_block_number, o.terminal_timestamp, o.terminal_status, o.failure_reason, \
      \o.execution_price, o.cleanup_actor, a.activity_type, a.size_delta, a.price, a.pnl_usdc, \
      \COALESCE(o.terminal_block_number, o.commit_block_number, 0) AS sort_block \
      \FROM perps_orders o \
      \LEFT JOIN LATERAL (\
      \  SELECT activity_type, size_delta, price, pnl_usdc \
      \  FROM perps_account_activity a \
      \  WHERE a.chain_id = o.chain_id AND a.account = o.account AND a.tx_hash = o.terminal_tx_hash \
      \    AND a.activity_type IN ('Open', 'Close', 'Liquidated') \
      \  ORDER BY a.log_index ASC LIMIT 1\
      \) a ON TRUE \
      \WHERE o.chain_id = ? AND o.account = ?"

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

getPerpsOrderById :: Connection -> Integer -> Integer -> Maybe Text -> IO (Maybe PerpsOrderRow)
getPerpsOrderById conn chainId orderId mAccount = do
  rows <- case mAccount of
    Nothing ->
      query conn baseQuery (chainId, orderId)
    Just account ->
      query conn accountQuery (chainId, orderId, T.toLower account)
  pure $ case rows of
    row : _ -> Just row
    [] -> Nothing
  where
    baseSelect :: Query
    baseSelect =
      "SELECT o.order_id, o.account, o.side, o.commit_tx_hash, o.commit_block_number, o.commit_timestamp, \
      \o.terminal_tx_hash, o.terminal_block_number, o.terminal_timestamp, o.terminal_status, o.failure_reason, \
      \o.execution_price, o.cleanup_actor, a.activity_type, a.size_delta, a.price, a.pnl_usdc, \
      \COALESCE(o.terminal_block_number, o.commit_block_number, 0) AS sort_block \
      \FROM perps_orders o \
      \LEFT JOIN LATERAL (\
      \  SELECT activity_type, size_delta, price, pnl_usdc \
      \  FROM perps_account_activity a \
      \  WHERE a.chain_id = o.chain_id AND a.account = o.account AND a.tx_hash = o.terminal_tx_hash \
      \    AND a.activity_type IN ('Open', 'Close', 'Liquidated') \
      \  ORDER BY a.log_index ASC LIMIT 1\
      \) a ON TRUE \
      \WHERE o.chain_id = ? AND o.order_id = ?"

    baseQuery :: Query
    baseQuery =
      baseSelect <> " LIMIT 1"

    accountQuery :: Query
    accountQuery =
      baseSelect <> " AND o.account = ? LIMIT 1"

getPerpsActivityByAccount :: Connection -> Integer -> Text -> Int -> Maybe (Integer, Integer) -> IO [PerpsActivityRow]
getPerpsActivityByAccount conn chainId account limit cursor = do
  case cursor of
    Nothing ->
      query conn baseQuery (chainId, T.toLower account, limit)
    Just (cursorBlock, cursorLogIndex) ->
      query conn cursorQuery (chainId, T.toLower account, cursorBlock, cursorBlock, cursorLogIndex, limit)
  where
    baseQuery :: Query
    baseQuery =
      "SELECT activity_type, account, actor, order_id, side, price, size_delta, amount_usdc, pnl_usdc, \
      \tx_hash, block_number, timestamp, data, log_index \
      \FROM perps_account_activity \
      \WHERE chain_id = ? AND account = ? \
      \ORDER BY block_number DESC, log_index DESC LIMIT ?"

    cursorQuery :: Query
    cursorQuery =
      "SELECT activity_type, account, actor, order_id, side, price, size_delta, amount_usdc, pnl_usdc, \
      \tx_hash, block_number, timestamp, data, log_index \
      \FROM perps_account_activity \
      \WHERE chain_id = ? AND account = ? \
      \AND (block_number < ? OR (block_number = ? AND log_index < ?)) \
      \ORDER BY block_number DESC, log_index DESC LIMIT ?"

getPerpsMarketVolumeSince :: Connection -> Integer -> Integer -> IO Integer
getPerpsMarketVolumeSince conn chainId fromTimestamp = do
  rows <- query conn
    "SELECT FLOOR(COALESCE(SUM(ABS(size_delta) * price / 100000000000000000000), 0)) \
    \FROM perps_account_activity \
    \WHERE chain_id = ? \
    \AND timestamp >= ? \
    \AND activity_type IN ('Open', 'Close', 'Liquidated') \
    \AND size_delta IS NOT NULL \
    \AND price IS NOT NULL"
    (chainId, fromTimestamp)
  case rows of
    [Only (Just value)] -> pure $ scientificToInteger value
    _ -> pure 0

getPerpsOrderAccountSide :: Connection -> Integer -> Integer -> IO (Maybe (Text, Maybe Int))
getPerpsOrderAccountSide conn chainId orderId = do
  rows <- query conn
    "SELECT account, side FROM perps_orders WHERE chain_id = ? AND order_id = ?"
    (chainId, orderId)
  case rows of
    [(Just account, side)] -> pure $ Just (account, side)
    _ -> pure Nothing

insertPerpsExpiredCleanupActivityIfReady :: Connection -> Integer -> Integer -> IO ()
insertPerpsExpiredCleanupActivityIfReady conn chainId orderId = do
  rows <- query conn
    "SELECT o.account, o.side, o.cleanup_actor, e.tx_hash, e.block_number, e.block_hash, \
    \e.tx_index, e.log_index, e.timestamp \
    \FROM perps_orders o \
    \JOIN perps_events e ON e.chain_id = o.chain_id AND e.order_id = o.order_id AND e.event_name = 'OrderFailed' \
    \WHERE o.chain_id = ? AND o.order_id = ? AND o.terminal_status = 'Expired / Cleaned up' \
    \AND o.account IS NOT NULL \
    \ORDER BY e.block_number DESC, e.log_index DESC LIMIT 1"
    (chainId, orderId)
  case rows of
    [(Just account, side, actor, txHash, blockNumber, blockHash, txIndex, logIndex, timestamp)] ->
      insertPerpsActivity conn chainId (cleanupActivityKey txHash logIndex orderId) account
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

getPerpsIndexerStatus :: Connection -> Integer -> Text -> IO (Maybe PerpsIndexerStatusRow)
getPerpsIndexerStatus conn chainId indexerName = do
  rows <- query conn
    "SELECT indexer_name, chain_id, last_indexed_block, last_indexed_block_hash \
    \FROM perps_indexer_state WHERE chain_id = ? AND indexer_name = ?"
    (chainId, indexerName)
  case rows of
    [row] -> pure $ Just row
    _ -> pure Nothing

getPerpsIndexerLastBlock :: Connection -> Integer -> Text -> IO (Integer, Maybe Text)
getPerpsIndexerLastBlock conn chainId indexerName = do
  statusRow <- getPerpsIndexerStatus conn chainId indexerName
  pure $ case statusRow of
    Just row -> (pisLastIndexedBlock row, pisLastIndexedBlockHash row)
    Nothing -> (0, Nothing)

setPerpsIndexerState :: Connection -> Integer -> Text -> Integer -> Maybe Text -> IO ()
setPerpsIndexerState conn chainId indexerName blockNumber blockHash = do
  _ <- execute conn
    "INSERT INTO perps_indexer_state (indexer_name, chain_id, last_indexed_block, last_indexed_block_hash) \
    \VALUES (?, ?, ?, ?) \
    \ON CONFLICT (indexer_name, chain_id) DO UPDATE SET \
    \last_indexed_block = EXCLUDED.last_indexed_block,\
    \last_indexed_block_hash = EXCLUDED.last_indexed_block_hash,\
    \updated_at = NOW()"
    (indexerName, chainId, blockNumber, fmap T.toLower blockHash)
  pure ()

deletePerpsHistoryFromBlock :: Connection -> Integer -> Integer -> IO ()
deletePerpsHistoryFromBlock conn chainId blockNumber = do
  _ <- execute conn
    "DELETE FROM perps_account_activity WHERE chain_id = ? AND block_number >= ?"
    (chainId, blockNumber)
  _ <- execute conn
    "DELETE FROM perps_events WHERE chain_id = ? AND block_number >= ?"
    (chainId, blockNumber)
  _ <- execute conn
    "UPDATE perps_orders SET terminal_tx_hash = NULL, terminal_block_number = NULL, terminal_timestamp = NULL, \
    \terminal_status = 'Committed', failure_reason = NULL, execution_price = NULL, cleanup_actor = NULL, updated_at = NOW() \
    \WHERE chain_id = ? AND terminal_block_number >= ?"
    (chainId, blockNumber)
  _ <- execute conn
    "DELETE FROM perps_orders WHERE chain_id = ? AND commit_block_number >= ?"
    (chainId, blockNumber)
  pure ()
