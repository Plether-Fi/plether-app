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
  , getBasketSnapshots
  , getLatestBasketSnapshotTime
  , BasketSnapshotRow (..)
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
  pure ()

insertBasketSnapshot
  :: Connection
  -> Integer -- timestamp
  -> Integer -- interval_seconds
  -> Integer -- basket_price
  -> Value   -- component_prices
  -> IO ()
insertBasketSnapshot conn timestamp intervalSeconds basketPrice components = do
  _ <- execute conn
    "INSERT INTO perps_basket_snapshots \
    \(timestamp, interval_seconds, basket_price, component_prices) \
    \VALUES (?, ?, ?, ?) \
    \ON CONFLICT (timestamp, interval_seconds) DO UPDATE SET \
    \basket_price = EXCLUDED.basket_price, \
    \component_prices = EXCLUDED.component_prices, \
    \source = 'pyth_benchmarks'"
    (timestamp, intervalSeconds, basketPrice, encode components)
  pure ()

getBasketSnapshots
  :: Connection
  -> Integer -- from timestamp
  -> Integer -- to timestamp
  -> Int     -- limit
  -> IO [BasketSnapshotRow]
getBasketSnapshots conn fromTimestamp toTimestamp limit = do
  query conn
    "SELECT timestamp, interval_seconds, basket_price, component_prices \
    \FROM perps_basket_snapshots \
    \WHERE timestamp >= ? AND timestamp <= ? \
    \ORDER BY timestamp ASC LIMIT ?"
    (fromTimestamp, toTimestamp, limit)

getLatestBasketSnapshotTime :: Connection -> IO (Maybe Integer)
getLatestBasketSnapshotTime conn = do
  result <- query_ conn
    "SELECT timestamp FROM perps_basket_snapshots ORDER BY timestamp DESC LIMIT 1"
    :: IO [Only Integer]
  case result of
    [Only timestamp] -> pure $ Just timestamp
    _ -> pure Nothing
