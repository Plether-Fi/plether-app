module Plether.Database.VaultPerformance
  ( VaultPerformanceSnapshotRow (..)
  , ensureVaultPerformanceSchema
  , upsertVaultPerformanceSnapshot
  , getVaultPerformanceSnapshots
  ) where

import qualified Data.ByteString.Char8 as BS8
import Data.Text (Text)
import qualified Data.Text as T
import Database.PostgreSQL.Simple
  ( Connection
  , execute
  , execute_
  , query
  )
import Database.PostgreSQL.Simple.FromField
  ( ResultError (ConversionFailed)
  , returnError
  )
import Database.PostgreSQL.Simple.FromRow
  ( FromRow (..)
  , RowParser
  , field
  , fieldWith
  )
import Database.PostgreSQL.Simple.ToRow (ToRow (..))
import Text.Read (readMaybe)

-- | One coherent hourly observation of both tranches. The epoch timestamp is
-- the UTC boundary being sampled, while the block timestamp is the timestamp
-- of the last canonical block at or before that boundary.
data VaultPerformanceSnapshotRow = VaultPerformanceSnapshotRow
  { vpsChainId :: Integer
  , vpsHousePoolAddress :: Text
  , vpsSeniorVaultAddress :: Text
  , vpsJuniorVaultAddress :: Text
  , vpsEpochTimestamp :: Integer
  , vpsBlockNumber :: Integer
  , vpsBlockHash :: Text
  , vpsBlockTimestamp :: Integer
  , vpsMarkFresh :: Maybe Bool
  , vpsSeniorTotalAssets :: Integer
  , vpsSeniorTotalSupply :: Integer
  , vpsSeniorSharePriceWad :: Integer
  , vpsJuniorTotalAssets :: Integer
  , vpsJuniorTotalSupply :: Integer
  , vpsJuniorSharePriceWad :: Integer
  }
  deriving stock (Eq, Show)

instance FromRow VaultPerformanceSnapshotRow where
  fromRow =
    VaultPerformanceSnapshotRow
      <$> numericIntegerField
      <*> field
      <*> field
      <*> field
      <*> field
      <*> numericIntegerField
      <*> field
      <*> field
      <*> field
      <*> numericIntegerField
      <*> numericIntegerField
      <*> numericIntegerField
      <*> numericIntegerField
      <*> numericIntegerField
      <*> numericIntegerField

-- PostgreSQL's NUMERIC type is decoded by postgresql-simple as Scientific,
-- not Integer. Vault values use NUMERIC(78,0) so they can hold uint256-sized
-- quantities without truncation; reject fractional database values instead of
-- silently rounding them.
numericIntegerField :: RowParser Integer
numericIntegerField =
  fieldWith $ \column raw ->
    case raw >>= readMaybe . BS8.unpack of
      Just integer -> pure integer
      Nothing ->
        returnError
          ConversionFailed
          column
          "Vault performance NUMERIC value was not an integer"

instance ToRow VaultPerformanceSnapshotRow where
  toRow VaultPerformanceSnapshotRow {..} =
    toRow
      ( vpsChainId
      , normalizeAddress vpsHousePoolAddress
      , normalizeAddress vpsSeniorVaultAddress
      , normalizeAddress vpsJuniorVaultAddress
      , vpsEpochTimestamp
      , vpsBlockNumber
      , T.toLower $ T.strip vpsBlockHash
      , vpsBlockTimestamp
      , vpsMarkFresh
      , vpsSeniorTotalAssets
      , vpsSeniorTotalSupply
      , vpsSeniorSharePriceWad
      , vpsJuniorTotalAssets
      , vpsJuniorTotalSupply
      , vpsJuniorSharePriceWad
      )

ensureVaultPerformanceSchema :: Connection -> IO ()
ensureVaultPerformanceSchema conn = do
  _ <- execute_ conn
    "CREATE TABLE IF NOT EXISTS vault_performance_snapshots (\
    \chain_id NUMERIC(78,0) NOT NULL,\
    \house_pool_address VARCHAR(42) NOT NULL,\
    \senior_vault_address VARCHAR(42) NOT NULL,\
    \junior_vault_address VARCHAR(42) NOT NULL,\
    \epoch_timestamp BIGINT NOT NULL,\
    \block_number NUMERIC(78,0) NOT NULL,\
    \block_hash VARCHAR(66) NOT NULL,\
    \block_timestamp BIGINT NOT NULL,\
    \mark_fresh BOOLEAN NOT NULL,\
    \senior_total_assets NUMERIC(78,0) NOT NULL,\
    \senior_total_supply NUMERIC(78,0) NOT NULL,\
    \senior_share_price_wad NUMERIC(78,0) NOT NULL,\
    \junior_total_assets NUMERIC(78,0) NOT NULL,\
    \junior_total_supply NUMERIC(78,0) NOT NULL,\
    \junior_share_price_wad NUMERIC(78,0) NOT NULL,\
    \created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),\
    \updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),\
    \PRIMARY KEY (\
    \  chain_id, house_pool_address, senior_vault_address, junior_vault_address, epoch_timestamp\
    \),\
    \CHECK (chain_id > 0),\
    \CHECK (house_pool_address ~ '^0x[0-9a-f]{40}$'),\
    \CHECK (senior_vault_address ~ '^0x[0-9a-f]{40}$'),\
    \CHECK (junior_vault_address ~ '^0x[0-9a-f]{40}$'),\
    \CHECK (epoch_timestamp >= 0 AND epoch_timestamp % 3600 = 0),\
    \CHECK (block_timestamp >= 0),\
    \CHECK (block_timestamp <= epoch_timestamp),\
    \CHECK (block_number >= 0),\
    \CHECK (block_hash ~ '^0x[0-9a-f]{64}$'),\
    \CHECK (senior_total_assets >= 0),\
    \CHECK (senior_total_supply >= 0),\
    \CHECK (senior_share_price_wad >= 0),\
    \CHECK (junior_total_assets >= 0),\
    \CHECK (junior_total_supply >= 0),\
    \CHECK (junior_share_price_wad >= 0)\
    \)"
  -- Existing installations predate mark freshness. Leave legacy rows NULL so
  -- the indexer can distinguish and resample them; every new/upserted row
  -- carries an observed boolean.
  _ <- execute_ conn
    "ALTER TABLE vault_performance_snapshots \
    \ADD COLUMN IF NOT EXISTS mark_fresh BOOLEAN"
  _ <- execute_ conn
    "CREATE INDEX IF NOT EXISTS idx_vault_performance_deployment_epoch \
    \ON vault_performance_snapshots \
    \(chain_id, house_pool_address, senior_vault_address, junior_vault_address, epoch_timestamp DESC)"
  pure ()

-- | Idempotently publish or repair an hourly checkpoint. A canonical reorg or
-- corrected deployment identity replaces all sampled values atomically.
upsertVaultPerformanceSnapshot :: Connection -> VaultPerformanceSnapshotRow -> IO ()
upsertVaultPerformanceSnapshot conn row = do
  _ <- execute conn
    "INSERT INTO vault_performance_snapshots (\
    \chain_id, house_pool_address, senior_vault_address, junior_vault_address,\
    \epoch_timestamp, block_number, block_hash, block_timestamp, mark_fresh,\
    \senior_total_assets, senior_total_supply, senior_share_price_wad,\
    \junior_total_assets, junior_total_supply, junior_share_price_wad\
    \) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) \
    \ON CONFLICT (\
    \chain_id, house_pool_address, senior_vault_address, junior_vault_address, epoch_timestamp\
    \) DO UPDATE SET \
    \block_number = EXCLUDED.block_number,\
    \block_hash = EXCLUDED.block_hash,\
    \block_timestamp = EXCLUDED.block_timestamp,\
    \mark_fresh = EXCLUDED.mark_fresh,\
    \senior_total_assets = EXCLUDED.senior_total_assets,\
    \senior_total_supply = EXCLUDED.senior_total_supply,\
    \senior_share_price_wad = EXCLUDED.senior_share_price_wad,\
    \junior_total_assets = EXCLUDED.junior_total_assets,\
    \junior_total_supply = EXCLUDED.junior_total_supply,\
    \junior_share_price_wad = EXCLUDED.junior_share_price_wad,\
    \updated_at = NOW()"
    row
  pure ()

-- | Return the latest deployment-scoped checkpoints in chronological order.
-- The inner descending selection ensures the limit applies to the newest rows;
-- the outer ordering is the API/chart contract.
getVaultPerformanceSnapshots
  :: Connection
  -> Integer
  -> Text
  -> Text
  -> Text
  -> Int
  -> IO [VaultPerformanceSnapshotRow]
getVaultPerformanceSnapshots conn chainId housePool seniorVault juniorVault limit =
  query conn
    "SELECT chain_id, house_pool_address, senior_vault_address, junior_vault_address,\
    \epoch_timestamp, block_number, block_hash, block_timestamp, mark_fresh,\
    \senior_total_assets, senior_total_supply, senior_share_price_wad,\
    \junior_total_assets, junior_total_supply, junior_share_price_wad \
    \FROM (\
    \  SELECT chain_id, house_pool_address, senior_vault_address, junior_vault_address,\
    \  epoch_timestamp, block_number, block_hash, block_timestamp, mark_fresh,\
    \  senior_total_assets, senior_total_supply, senior_share_price_wad,\
    \  junior_total_assets, junior_total_supply, junior_share_price_wad \
    \  FROM vault_performance_snapshots \
    \  WHERE chain_id = ? AND house_pool_address = ? \
    \    AND senior_vault_address = ? AND junior_vault_address = ? \
    \  ORDER BY epoch_timestamp DESC LIMIT ?\
    \) AS latest ORDER BY epoch_timestamp ASC"
    ( chainId
    , normalizeAddress housePool
    , normalizeAddress seniorVault
    , normalizeAddress juniorVault
    , max 0 limit
    )

normalizeAddress :: Text -> Text
normalizeAddress = T.toLower . T.strip
