module Plether.Database.VaultActivity
  ( VaultActivityDeployment (..)
  , VaultActivityIndexerStateRow (..)
  , VaultHolderRow (..)
  , VaultRequestRow (..)
  , ensureVaultActivitySchema
  , tryLockVaultActivityIndexer
  , unlockVaultActivityIndexer
  , getVaultActivityIndexerState
  , setVaultActivityIndexerState
  , resetVaultActivityDeployment
  , insertVaultLogIdentityExact
  , insertVaultShareTransferExact
  , insertVaultRequestExact
  , recomputeVaultHolderBalance
  , getVaultHolders
  , countVaultHolders
  , getVaultRequests
  , countVaultRequests
  , countVaultEvents
  , getVaultRequestIds
  ) where

import Control.Monad (unless)
import qualified Data.ByteString.Char8 as BS8
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Database.PostgreSQL.Simple
  ( Connection
  , Only (..)
  , execute
  , execute_
  , query
  , query_
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
import Text.Read (readMaybe)

data VaultActivityDeployment = VaultActivityDeployment
  { vadChainId :: Integer
  , vadHousePool :: Text
  , vadSeniorVault :: Text
  , vadJuniorVault :: Text
  , vadDeploymentBlock :: Integer
  }
  deriving stock (Eq, Show)

data VaultActivityIndexerStateRow = VaultActivityIndexerStateRow
  { vaisLastIndexedBlock :: Integer
  , vaisLastIndexedBlockHash :: Maybe Text
  , vaisLastIndexedBlockTimestamp :: Integer
  , vaisSafeHeadBlock :: Integer
  , vaisSafeHeadBlockHash :: Maybe Text
  , vaisSafeHeadTimestamp :: Integer
  , vaisBackfillComplete :: Bool
  , vaisLastSuccessTimestamp :: Integer
  }
  deriving stock (Eq, Show)

instance FromRow VaultActivityIndexerStateRow where
  fromRow =
    VaultActivityIndexerStateRow
      <$> decimalIntegerField
      <*> field
      <*> field
      <*> decimalIntegerField
      <*> field
      <*> field
      <*> field
      <*> field

data VaultHolderRow = VaultHolderRow
  { vhrAddress :: Text
  , vhrShareBalance :: Integer
  }
  deriving stock (Eq, Show)

instance FromRow VaultHolderRow where
  fromRow = VaultHolderRow <$> field <*> decimalIntegerField

data VaultRequestRow = VaultRequestRow
  { vrrEventName :: Text
  , vrrVaultAddress :: Text
  , vrrController :: Text
  , vrrOwner :: Text
  , vrrRequestId :: Integer
  , vrrRawAmount :: Integer
  , vrrTxHash :: Text
  , vrrBlockNumber :: Integer
  , vrrBlockHash :: Text
  , vrrTxIndex :: Integer
  , vrrLogIndex :: Integer
  , vrrTimestamp :: Integer
  }
  deriving stock (Eq, Show)

instance FromRow VaultRequestRow where
  fromRow =
    VaultRequestRow
      <$> field
      <*> field
      <*> field
      <*> field
      <*> decimalIntegerField
      <*> decimalIntegerField
      <*> field
      <*> decimalIntegerField
      <*> field
      <*> decimalIntegerField
      <*> decimalIntegerField
      <*> field

decimalIntegerField :: RowParser Integer
decimalIntegerField =
  fieldWith $ \column raw ->
    case raw >>= readMaybe . BS8.unpack of
      Just integer -> pure integer
      Nothing ->
        returnError
          ConversionFailed
          column
          "Vault activity NUMERIC value was not an integer"

ensureVaultActivitySchema :: Connection -> IO ()
ensureVaultActivitySchema conn = do
  _ <- execute_ conn
    "CREATE TABLE IF NOT EXISTS vault_activity_indexer_state (\
    \chain_id NUMERIC(78,0) NOT NULL,\
    \house_pool_address VARCHAR(42) NOT NULL,\
    \senior_vault_address VARCHAR(42) NOT NULL,\
    \junior_vault_address VARCHAR(42) NOT NULL,\
    \deployment_block NUMERIC(78,0) NOT NULL,\
    \last_indexed_block NUMERIC(78,0) NOT NULL,\
    \last_indexed_block_hash VARCHAR(66),\
    \last_indexed_block_timestamp BIGINT NOT NULL DEFAULT 0,\
    \safe_head_block NUMERIC(78,0) NOT NULL DEFAULT 0,\
    \safe_head_block_hash VARCHAR(66),\
    \safe_head_timestamp BIGINT NOT NULL DEFAULT 0,\
    \backfill_complete BOOLEAN NOT NULL DEFAULT FALSE,\
    \last_success_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),\
    \updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),\
    \PRIMARY KEY (chain_id, house_pool_address, senior_vault_address, junior_vault_address, deployment_block),\
    \CHECK (chain_id > 0 AND deployment_block >= 0 AND last_indexed_block >= 0 AND safe_head_block >= 0),\
    \CHECK (house_pool_address ~ '^0x[0-9a-f]{40}$'),\
    \CHECK (senior_vault_address ~ '^0x[0-9a-f]{40}$'),\
    \CHECK (junior_vault_address ~ '^0x[0-9a-f]{40}$'),\
    \CHECK (last_indexed_block_hash IS NULL OR last_indexed_block_hash ~ '^0x[0-9a-f]{64}$'),\
    \CHECK (safe_head_block_hash IS NULL OR safe_head_block_hash ~ '^0x[0-9a-f]{64}$')\
    \)"
  _ <- execute_ conn
    "ALTER TABLE vault_activity_indexer_state ADD COLUMN IF NOT EXISTS \
    \last_indexed_block_timestamp BIGINT NOT NULL DEFAULT 0"
  _ <- execute_ conn
    "CREATE TABLE IF NOT EXISTS vault_canonical_logs (\
    \chain_id NUMERIC(78,0) NOT NULL,\
    \house_pool_address VARCHAR(42) NOT NULL,\
    \deployment_block NUMERIC(78,0) NOT NULL,\
    \vault_address VARCHAR(42) NOT NULL,\
    \event_name TEXT NOT NULL,\
    \tx_hash VARCHAR(66) NOT NULL,\
    \block_number NUMERIC(78,0) NOT NULL,\
    \block_hash VARCHAR(66) NOT NULL,\
    \tx_index NUMERIC(78,0) NOT NULL,\
    \log_index NUMERIC(78,0) NOT NULL,\
    \block_timestamp BIGINT NOT NULL,\
    \PRIMARY KEY (chain_id, house_pool_address, deployment_block, vault_address, tx_hash, log_index),\
    \CHECK (event_name IN ('Transfer', 'DepositRequest', 'RedeemRequest', 'DepositRequested')),\
    \CHECK (chain_id > 0 AND deployment_block >= 0 AND block_number >= 0 AND tx_index >= 0 AND log_index >= 0),\
    \CHECK (house_pool_address ~ '^0x[0-9a-f]{40}$' AND vault_address ~ '^0x[0-9a-f]{40}$'),\
    \CHECK (tx_hash ~ '^0x[0-9a-f]{64}$' AND block_hash ~ '^0x[0-9a-f]{64}$')\
    \)"
  _ <- execute_ conn
    "CREATE TABLE IF NOT EXISTS vault_share_transfers (\
    \chain_id NUMERIC(78,0) NOT NULL,\
    \house_pool_address VARCHAR(42) NOT NULL,\
    \deployment_block NUMERIC(78,0) NOT NULL,\
    \vault_address VARCHAR(42) NOT NULL,\
    \from_address VARCHAR(42) NOT NULL,\
    \to_address VARCHAR(42) NOT NULL,\
    \amount NUMERIC(78,0) NOT NULL,\
    \tx_hash VARCHAR(66) NOT NULL,\
    \block_number NUMERIC(78,0) NOT NULL,\
    \block_hash VARCHAR(66) NOT NULL,\
    \tx_index NUMERIC(78,0) NOT NULL,\
    \log_index NUMERIC(78,0) NOT NULL,\
    \block_timestamp BIGINT NOT NULL,\
    \PRIMARY KEY (chain_id, house_pool_address, deployment_block, vault_address, tx_hash, log_index),\
    \CHECK (chain_id > 0 AND deployment_block >= 0 AND amount >= 0 AND block_number >= 0 AND tx_index >= 0 AND log_index >= 0),\
    \CHECK (house_pool_address ~ '^0x[0-9a-f]{40}$' AND vault_address ~ '^0x[0-9a-f]{40}$'),\
    \CHECK (from_address ~ '^0x[0-9a-f]{40}$' AND to_address ~ '^0x[0-9a-f]{40}$'),\
    \CHECK (tx_hash ~ '^0x[0-9a-f]{64}$' AND block_hash ~ '^0x[0-9a-f]{64}$')\
    \)"
  _ <- execute_ conn
    "CREATE INDEX IF NOT EXISTS idx_vault_share_transfers_holder \
    \ON vault_share_transfers(chain_id, house_pool_address, deployment_block, vault_address, from_address, to_address)"
  _ <- execute_ conn
    "CREATE TABLE IF NOT EXISTS vault_holder_balances (\
    \chain_id NUMERIC(78,0) NOT NULL,\
    \house_pool_address VARCHAR(42) NOT NULL,\
    \deployment_block NUMERIC(78,0) NOT NULL,\
    \vault_address VARCHAR(42) NOT NULL,\
    \holder_address VARCHAR(42) NOT NULL,\
    \share_balance NUMERIC(78,0) NOT NULL,\
    \updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),\
    \PRIMARY KEY (chain_id, house_pool_address, deployment_block, vault_address, holder_address),\
    \CHECK (share_balance > 0),\
    \CHECK (house_pool_address ~ '^0x[0-9a-f]{40}$' AND vault_address ~ '^0x[0-9a-f]{40}$'),\
    \CHECK (holder_address ~ '^0x[0-9a-f]{40}$')\
    \)"
  _ <- execute_ conn
    "CREATE INDEX IF NOT EXISTS idx_vault_holder_balances_rank \
    \ON vault_holder_balances(chain_id, house_pool_address, deployment_block, vault_address, share_balance DESC, holder_address)"
  _ <- execute_ conn
    "CREATE TABLE IF NOT EXISTS vault_request_events (\
    \chain_id NUMERIC(78,0) NOT NULL,\
    \house_pool_address VARCHAR(42) NOT NULL,\
    \deployment_block NUMERIC(78,0) NOT NULL,\
    \vault_address VARCHAR(42) NOT NULL,\
    \event_name TEXT NOT NULL,\
    \controller_address VARCHAR(42) NOT NULL,\
    \owner_address VARCHAR(42) NOT NULL,\
    \request_id NUMERIC(78,0) NOT NULL,\
    \raw_amount NUMERIC(78,0) NOT NULL,\
    \tx_hash VARCHAR(66) NOT NULL,\
    \block_number NUMERIC(78,0) NOT NULL,\
    \block_hash VARCHAR(66) NOT NULL,\
    \tx_index NUMERIC(78,0) NOT NULL,\
    \log_index NUMERIC(78,0) NOT NULL,\
    \block_timestamp BIGINT NOT NULL,\
    \PRIMARY KEY (chain_id, house_pool_address, deployment_block, vault_address, tx_hash, log_index),\
    \CHECK (event_name IN ('DepositRequest', 'RedeemRequest', 'DepositRequested')),\
    \CHECK (chain_id > 0 AND deployment_block >= 0 AND request_id >= 0 AND raw_amount >= 0 AND block_number >= 0 AND tx_index >= 0 AND log_index >= 0),\
    \CHECK (house_pool_address ~ '^0x[0-9a-f]{40}$' AND vault_address ~ '^0x[0-9a-f]{40}$'),\
    \CHECK (controller_address ~ '^0x[0-9a-f]{40}$' AND owner_address ~ '^0x[0-9a-f]{40}$'),\
    \CHECK (tx_hash ~ '^0x[0-9a-f]{64}$' AND block_hash ~ '^0x[0-9a-f]{64}$')\
    \)"
  _ <- execute_ conn
    "CREATE INDEX IF NOT EXISTS idx_vault_request_events_recent \
    \ON vault_request_events(chain_id, house_pool_address, deployment_block, vault_address, block_number DESC, tx_index DESC, log_index DESC)"
  _ <- execute_ conn
    "CREATE INDEX IF NOT EXISTS idx_vault_request_events_controller \
    \ON vault_request_events(chain_id, house_pool_address, deployment_block, vault_address, controller_address, request_id DESC)"
  _ <- execute_ conn
    "CREATE INDEX IF NOT EXISTS idx_vault_request_events_owner \
    \ON vault_request_events(chain_id, house_pool_address, deployment_block, vault_address, owner_address, request_id DESC)"
  pure ()

tryLockVaultActivityIndexer :: Connection -> IO Bool
tryLockVaultActivityIndexer conn = do
  rows <- query_ conn "SELECT pg_try_advisory_lock(8612047532)" :: IO [Only Bool]
  pure $ rows == [Only True]

unlockVaultActivityIndexer :: Connection -> IO ()
unlockVaultActivityIndexer conn = do
  _ <- query_ conn "SELECT pg_advisory_unlock(8612047532)" :: IO [Only Bool]
  pure ()

getVaultActivityIndexerState
  :: Connection
  -> VaultActivityDeployment
  -> IO (Maybe VaultActivityIndexerStateRow)
getVaultActivityIndexerState conn deployment = do
  rows <- query conn
    "SELECT last_indexed_block::TEXT, last_indexed_block_hash, last_indexed_block_timestamp, \
    \safe_head_block::TEXT, safe_head_block_hash, \
    \safe_head_timestamp, backfill_complete, EXTRACT(EPOCH FROM last_success_at)::BIGINT \
    \FROM vault_activity_indexer_state WHERE chain_id = ? AND house_pool_address = ? \
    \AND senior_vault_address = ? AND junior_vault_address = ? AND deployment_block = ?"
    ( vadChainId deployment
    , address $ vadHousePool deployment
    , address $ vadSeniorVault deployment
    , address $ vadJuniorVault deployment
    , vadDeploymentBlock deployment
    )
  pure $ case rows of
    [row] -> Just row
    _ -> Nothing

setVaultActivityIndexerState
  :: Connection
  -> VaultActivityDeployment
  -> Integer
  -> Maybe Text
  -> Integer
  -> Integer
  -> Text
  -> Integer
  -> Bool
  -> IO ()
setVaultActivityIndexerState conn deployment indexedBlock indexedHash indexedTimestamp safeBlock safeHash safeTimestamp complete = do
  affected <- execute conn
    "INSERT INTO vault_activity_indexer_state (chain_id, house_pool_address, senior_vault_address, junior_vault_address, \
    \deployment_block, last_indexed_block, last_indexed_block_hash, last_indexed_block_timestamp, \
    \safe_head_block, safe_head_block_hash, \
    \safe_head_timestamp, backfill_complete, last_success_at, updated_at) \
    \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, NOW(), NOW()) \
    \ON CONFLICT (chain_id, house_pool_address, senior_vault_address, junior_vault_address, deployment_block) DO UPDATE SET \
    \last_indexed_block = EXCLUDED.last_indexed_block, \
    \last_indexed_block_hash = EXCLUDED.last_indexed_block_hash, \
    \last_indexed_block_timestamp = EXCLUDED.last_indexed_block_timestamp, safe_head_block = EXCLUDED.safe_head_block, \
    \safe_head_block_hash = EXCLUDED.safe_head_block_hash, safe_head_timestamp = EXCLUDED.safe_head_timestamp, \
    \backfill_complete = vault_activity_indexer_state.backfill_complete OR EXCLUDED.backfill_complete, \
    \last_success_at = NOW(), updated_at = NOW()"
    ( vadChainId deployment
    , address $ vadHousePool deployment
    , address $ vadSeniorVault deployment
    , address $ vadJuniorVault deployment
    , vadDeploymentBlock deployment
    , indexedBlock
    , fmap T.toLower indexedHash
    , indexedTimestamp
    , safeBlock
    , T.toLower safeHash
    , safeTimestamp
    , complete
    )
  unless (affected == 1) $ fail "Vault activity indexer deployment identity changed"

resetVaultActivityDeployment :: Connection -> VaultActivityDeployment -> IO ()
resetVaultActivityDeployment conn deployment = do
  let scope =
        ( vadChainId deployment
        , address $ vadHousePool deployment
        , vadDeploymentBlock deployment
        , address $ vadSeniorVault deployment
        , address $ vadJuniorVault deployment
        )
  _ <- execute conn
    "DELETE FROM vault_holder_balances WHERE chain_id = ? AND house_pool_address = ? AND deployment_block = ? AND vault_address IN (?, ?)"
    scope
  _ <- execute conn
    "DELETE FROM vault_share_transfers WHERE chain_id = ? AND house_pool_address = ? AND deployment_block = ? AND vault_address IN (?, ?)"
    scope
  _ <- execute conn
    "DELETE FROM vault_request_events WHERE chain_id = ? AND house_pool_address = ? AND deployment_block = ? AND vault_address IN (?, ?)"
    scope
  _ <- execute conn
    "DELETE FROM vault_canonical_logs WHERE chain_id = ? AND house_pool_address = ? AND deployment_block = ? AND vault_address IN (?, ?)"
    scope
  _ <- execute conn
    "DELETE FROM vault_activity_indexer_state WHERE chain_id = ? AND house_pool_address = ? \
    \AND senior_vault_address = ? AND junior_vault_address = ? AND deployment_block = ?"
    ( vadChainId deployment
    , address $ vadHousePool deployment
    , address $ vadSeniorVault deployment
    , address $ vadJuniorVault deployment
    , vadDeploymentBlock deployment
    )
  pure ()

insertVaultLogIdentityExact
  :: Connection
  -> VaultActivityDeployment
  -> Text
  -> Text
  -> Text
  -> Integer
  -> Text
  -> Integer
  -> Integer
  -> Integer
  -> IO ()
insertVaultLogIdentityExact conn deployment vault eventName txHash blockNumber blockHash txIndex logIndex timestamp = do
  affected <- execute conn
    "INSERT INTO vault_canonical_logs (chain_id, house_pool_address, deployment_block, vault_address, event_name, \
    \tx_hash, block_number, block_hash, tx_index, log_index, block_timestamp) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) \
    \ON CONFLICT (chain_id, house_pool_address, deployment_block, vault_address, tx_hash, log_index) DO UPDATE SET block_hash = EXCLUDED.block_hash \
    \WHERE vault_canonical_logs.event_name = EXCLUDED.event_name \
    \AND vault_canonical_logs.block_number = EXCLUDED.block_number AND vault_canonical_logs.block_hash = EXCLUDED.block_hash \
    \AND vault_canonical_logs.tx_index = EXCLUDED.tx_index AND vault_canonical_logs.block_timestamp = EXCLUDED.block_timestamp"
    ( vadChainId deployment, address $ vadHousePool deployment, vadDeploymentBlock deployment, address vault, eventName
    , T.toLower txHash, blockNumber, T.toLower blockHash, txIndex, logIndex, timestamp
    )
  unless (affected == 1) $ fail "Conflicting canonical vault log identity"

insertVaultShareTransferExact
  :: Connection
  -> VaultActivityDeployment
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
insertVaultShareTransferExact conn deployment vault fromAddress toAddress amount txHash blockNumber blockHash txIndex logIndex timestamp = do
  affected <- execute conn
    "INSERT INTO vault_share_transfers (chain_id, house_pool_address, deployment_block, vault_address, from_address, to_address, amount, \
    \tx_hash, block_number, block_hash, tx_index, log_index, block_timestamp) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) \
    \ON CONFLICT (chain_id, house_pool_address, deployment_block, vault_address, tx_hash, log_index) DO UPDATE SET block_hash = EXCLUDED.block_hash \
    \WHERE vault_share_transfers.from_address = EXCLUDED.from_address AND vault_share_transfers.to_address = EXCLUDED.to_address \
    \AND vault_share_transfers.amount = EXCLUDED.amount AND vault_share_transfers.block_number = EXCLUDED.block_number \
    \AND vault_share_transfers.block_hash = EXCLUDED.block_hash AND vault_share_transfers.tx_index = EXCLUDED.tx_index \
    \AND vault_share_transfers.block_timestamp = EXCLUDED.block_timestamp"
    ( vadChainId deployment, address $ vadHousePool deployment, vadDeploymentBlock deployment, address vault, address fromAddress, address toAddress
    , amount, T.toLower txHash, blockNumber, T.toLower blockHash, txIndex, logIndex, timestamp
    )
  unless (affected == 1) $ fail "Conflicting canonical vault Transfer log"

insertVaultRequestExact
  :: Connection
  -> VaultActivityDeployment
  -> VaultRequestRow
  -> IO ()
insertVaultRequestExact conn deployment row = do
  affected <- execute conn
    "INSERT INTO vault_request_events (chain_id, house_pool_address, deployment_block, vault_address, event_name, controller_address, \
    \owner_address, request_id, raw_amount, tx_hash, block_number, block_hash, tx_index, log_index, block_timestamp) \
    \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) \
    \ON CONFLICT (chain_id, house_pool_address, deployment_block, vault_address, tx_hash, log_index) DO UPDATE SET block_hash = EXCLUDED.block_hash \
    \WHERE vault_request_events.event_name = EXCLUDED.event_name \
    \AND vault_request_events.controller_address = EXCLUDED.controller_address \
    \AND vault_request_events.owner_address = EXCLUDED.owner_address \
    \AND vault_request_events.request_id = EXCLUDED.request_id AND vault_request_events.raw_amount = EXCLUDED.raw_amount \
    \AND vault_request_events.block_number = EXCLUDED.block_number AND vault_request_events.block_hash = EXCLUDED.block_hash \
    \AND vault_request_events.tx_index = EXCLUDED.tx_index AND vault_request_events.block_timestamp = EXCLUDED.block_timestamp"
    ( vadChainId deployment, address $ vadHousePool deployment, vadDeploymentBlock deployment, address $ vrrVaultAddress row, vrrEventName row
    , address $ vrrController row, address $ vrrOwner row, vrrRequestId row, vrrRawAmount row
    , T.toLower $ vrrTxHash row, vrrBlockNumber row, T.toLower $ vrrBlockHash row
    , vrrTxIndex row, vrrLogIndex row, vrrTimestamp row
    )
  unless (affected == 1) $ fail "Conflicting canonical vault request log"

recomputeVaultHolderBalance
  :: Connection
  -> VaultActivityDeployment
  -> Text
  -> Text
  -> IO ()
recomputeVaultHolderBalance conn deployment vault holder
  | address holder == zeroAddress = pure ()
  | otherwise = do
      rows <- query conn
        "SELECT COALESCE(SUM(delta), 0)::TEXT FROM (\
        \SELECT amount AS delta FROM vault_share_transfers WHERE chain_id = ? AND house_pool_address = ? AND deployment_block = ? \
        \AND vault_address = ? AND to_address = ? UNION ALL \
        \SELECT -amount AS delta FROM vault_share_transfers WHERE chain_id = ? AND house_pool_address = ? AND deployment_block = ? \
        \AND vault_address = ? AND from_address = ?) AS movements"
        ( vadChainId deployment, address $ vadHousePool deployment, vadDeploymentBlock deployment, address vault, address holder
        , vadChainId deployment, address $ vadHousePool deployment, vadDeploymentBlock deployment, address vault, address holder
        ) :: IO [Only Text]
      balance <- case rows of
        [Only raw] ->
          maybe (fail "Invalid holder balance aggregate") pure $
            (readMaybe (T.unpack raw) :: Maybe Integer)
        _ -> fail "Missing holder balance aggregate"
      if balance < 0
        then fail "Canonical vault transfers produce a negative holder balance"
        else if balance == 0
          then do
            _ <- execute conn
              "DELETE FROM vault_holder_balances WHERE chain_id = ? AND house_pool_address = ? AND deployment_block = ? AND vault_address = ? AND holder_address = ?"
              (vadChainId deployment, address $ vadHousePool deployment, vadDeploymentBlock deployment, address vault, address holder)
            pure ()
          else do
            _ <- execute conn
              "INSERT INTO vault_holder_balances (chain_id, house_pool_address, deployment_block, vault_address, holder_address, share_balance) \
              \VALUES (?, ?, ?, ?, ?, ?) ON CONFLICT (chain_id, house_pool_address, deployment_block, vault_address, holder_address) \
              \DO UPDATE SET share_balance = EXCLUDED.share_balance, updated_at = NOW()"
              (vadChainId deployment, address $ vadHousePool deployment, vadDeploymentBlock deployment, address vault, address holder, balance)
            pure ()

getVaultHolders :: Connection -> VaultActivityDeployment -> Text -> Int -> IO [VaultHolderRow]
getVaultHolders conn deployment vault limit =
  query conn
    "SELECT holder_address, share_balance::TEXT FROM vault_holder_balances \
    \WHERE chain_id = ? AND house_pool_address = ? AND deployment_block = ? AND vault_address = ? \
    \ORDER BY share_balance DESC, holder_address ASC LIMIT ?"
    (vadChainId deployment, address $ vadHousePool deployment, vadDeploymentBlock deployment, address vault, max 0 limit)

countVaultHolders :: Connection -> VaultActivityDeployment -> Text -> IO Int64
countVaultHolders conn deployment vault = do
  rows <- query conn
    "SELECT COUNT(*) FROM vault_holder_balances WHERE chain_id = ? AND house_pool_address = ? AND deployment_block = ? AND vault_address = ?"
    (vadChainId deployment, address $ vadHousePool deployment, vadDeploymentBlock deployment, address vault) :: IO [Only Int64]
  pure $ case rows of
    [Only count] -> count
    _ -> 0

getVaultRequests :: Connection -> VaultActivityDeployment -> Text -> Int -> IO [VaultRequestRow]
getVaultRequests conn deployment vault limit =
  query conn
    "SELECT event_name, vault_address, controller_address, owner_address, request_id::TEXT, raw_amount::TEXT, \
    \tx_hash, block_number::TEXT, block_hash, tx_index::TEXT, log_index::TEXT, block_timestamp \
    \FROM vault_request_events WHERE chain_id = ? AND house_pool_address = ? AND deployment_block = ? AND vault_address = ? \
    \AND event_name IN ('DepositRequest', 'RedeemRequest') \
    \ORDER BY block_number DESC, tx_index DESC, log_index DESC LIMIT ?"
    (vadChainId deployment, address $ vadHousePool deployment, vadDeploymentBlock deployment, address vault, max 0 limit)

countVaultRequests :: Connection -> VaultActivityDeployment -> Text -> IO Int64
countVaultRequests conn deployment vault = do
  rows <- query conn
    "SELECT COUNT(*) FROM vault_request_events WHERE chain_id = ? AND house_pool_address = ? AND deployment_block = ? AND vault_address = ? \
    \AND event_name IN ('DepositRequest', 'RedeemRequest')"
    (vadChainId deployment, address $ vadHousePool deployment, vadDeploymentBlock deployment, address vault) :: IO [Only Int64]
  pure $ case rows of
    [Only count] -> count
    _ -> 0

countVaultEvents :: Connection -> VaultActivityDeployment -> IO Int64
countVaultEvents conn deployment = do
  rows <- query conn
    "SELECT COUNT(*) FROM vault_canonical_logs WHERE chain_id = ? AND house_pool_address = ? AND deployment_block = ? AND vault_address IN (?, ?)"
    ( vadChainId deployment, address $ vadHousePool deployment, vadDeploymentBlock deployment
    , address $ vadSeniorVault deployment, address $ vadJuniorVault deployment
    ) :: IO [Only Int64]
  pure $ case rows of
    [Only count] -> count
    _ -> 0

getVaultRequestIds
  :: Connection
  -> VaultActivityDeployment
  -> Text
  -> Text
  -> Int
  -> Maybe Integer
  -> IO [Integer]
getVaultRequestIds conn deployment vault account limit cursor = do
  rows <- case cursor of
    Nothing -> query conn
      "SELECT DISTINCT request_id::TEXT FROM vault_request_events WHERE chain_id = ? AND house_pool_address = ? AND deployment_block = ? \
      \AND vault_address = ? AND (controller_address = ? OR owner_address = ?) ORDER BY request_id DESC LIMIT ?"
      (vadChainId deployment, address $ vadHousePool deployment, vadDeploymentBlock deployment, address vault, address account, address account, max 0 limit)
    Just requestId -> query conn
      "SELECT DISTINCT request_id::TEXT FROM vault_request_events WHERE chain_id = ? AND house_pool_address = ? AND deployment_block = ? \
      \AND vault_address = ? AND (controller_address = ? OR owner_address = ?) AND request_id < ? ORDER BY request_id DESC LIMIT ?"
      (vadChainId deployment, address $ vadHousePool deployment, vadDeploymentBlock deployment, address vault, address account, address account, requestId, max 0 limit)
  traverse parseRow rows
 where
  parseRow (Only raw) = maybe (fail "Invalid request ID aggregate") pure $ readMaybe $ T.unpack raw

address :: Text -> Text
address = T.toLower . T.strip

zeroAddress :: Text
zeroAddress = "0x0000000000000000000000000000000000000000"
