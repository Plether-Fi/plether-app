module Plether.Handlers.ProtectionHistory
  ( getProtectionHistory
  , getProtectionEvents
  , getProtectionExecution
  , protectionExecutionSql
  , parseProtectionCursor
  , validProtectionBook
  ) where

import Data.Aeson (Value, object, (.=))
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Database.PostgreSQL.Simple (Only (..), Query, query)
import Plether.Config (Config (..))
import Plether.Database (DbPool, withDb)
import qualified Plether.Perps.Manifest as Manifest
import Plether.Types (ApiError, ApiResponse, mkResponse)
import Text.Read (readMaybe)

parseProtectionCursor :: Text -> Maybe Integer
parseProtectionCursor value = do
  n <- readMaybe (T.unpack value)
  if n > 0 && n < 2 ^ (64 :: Int) && T.pack (show n) == value then Just n else Nothing

validProtectionBook :: Maybe Text -> Bool
validProtectionBook = maybe True ((== T.toLower Manifest.positionProtectionBookAddress) . T.toLower)

-- Whitelist fields: signed raw transactions must never enter the public API.
-- The UI matches the observed protection/order/status and expires this advisory report.
protectionExecutionSql :: Query
protectionExecutionSql =
  "SELECT o.checked_block, o.observation || jsonb_build_object(\
  \'checkedBlock',o.checked_block::text,'checkedBlockHash',o.checked_block_hash,\
  \'checkedAt',o.checked_at,'ageSeconds',GREATEST(0,EXTRACT(EPOCH FROM (clock_timestamp()-o.checked_at))),\
  \'transactionHash',t.transaction_hash,'transactionAction',t.action) \
  \FROM perps_protection_observations o LEFT JOIN LATERAL (\
  \SELECT transaction_hash,action FROM perps_protection_transactions \
  \WHERE chain_id=o.chain_id AND book=o.book AND protection_id=o.protection_id \
  \AND status IN ('pending','included') AND linked_order_id::text=o.observation->>'linkedOrderId' \
  \ORDER BY created_at DESC LIMIT 1) t ON TRUE \
  \WHERE o.chain_id=? AND o.book=? AND o.protection_id=?"

getProtectionExecution :: DbPool -> Config -> Integer -> IO (Either ApiError (ApiResponse Value))
getProtectionExecution pool cfg protectionId = withDb pool $ \conn -> do
  rows <- query conn protectionExecutionSql
    (cfgPerpsChainId cfg, T.toLower Manifest.positionProtectionBookAddress, protectionId) :: IO [(Integer, Value)]
  pure $ Right $ mkResponse (maybe 0 fst $ listToMaybe rows) (cfgPerpsChainId cfg) $ object
    [ "observation" .= (snd <$> listToMaybe rows) ]

getProtectionHistory :: DbPool -> Config -> Text -> Int -> Maybe Integer -> IO (Either ApiError (ApiResponse Value))
getProtectionHistory pool cfg account requestedLimit cursor = withDb pool $ \conn -> do
  let limit = max 1 $ min 100 requestedLimit
      chain = cfgPerpsChainId cfg
      book = T.toLower Manifest.positionProtectionBookAddress
  rows <- query conn
    "SELECT protection_id, snapshot || jsonb_build_object('updatedBlock',block_number::text) FROM (\
    \SELECT DISTINCT ON (protection_id) protection_id,snapshot,block_number FROM perps_protection_events \
    \WHERE chain_id=? AND book=? AND account=? AND (?::numeric IS NULL OR protection_id < ?) \
    \ORDER BY protection_id DESC,block_number DESC,log_index DESC) latest ORDER BY protection_id DESC LIMIT ?"
    (chain, book, T.toLower account, cursor, cursor, limit + 1) :: IO [(Integer, Value)]
  checkpoints <- query conn
    "SELECT block_number FROM perps_protection_checkpoints WHERE chain_id=? AND book=? ORDER BY block_number DESC LIMIT 1"
    (chain, book) :: IO [Only Integer]
  let page = take limit rows
      nextCursor = if length rows > limit then T.pack . show . fst <$> listToMaybe (reverse page) else Nothing
      indexedBlock = maybe 0 fromOnly $ listToMaybe checkpoints
  pure $ Right $ mkResponse indexedBlock chain $ object
    [ "protections" .= map snd page, "nextCursor" .= nextCursor, "indexedThroughBlock" .= show indexedBlock ]

getProtectionEvents :: DbPool -> Config -> Integer -> Int -> Maybe (Integer, Integer) -> IO (Either ApiError (ApiResponse Value))
getProtectionEvents pool cfg protectionId requestedLimit cursor = withDb pool $ \conn -> do
  let limit = max 1 $ min 100 requestedLimit
      chain = cfgPerpsChainId cfg
      book = T.toLower Manifest.positionProtectionBookAddress
  rows <- query conn
    "SELECT block_number,log_index,jsonb_build_object('event',event_name,'args',event_data,'blockNumber',block_number::text,\
    \'logIndex',log_index::text,'blockHash',block_hash,'transactionHash',transaction_hash) FROM perps_protection_events \
    \WHERE chain_id=? AND book=? AND protection_id=? AND (?::bigint IS NULL OR (block_number,log_index) < (?,?)) \
    \ORDER BY block_number DESC,log_index DESC LIMIT ?"
    (chain, book, protectionId, fst <$> cursor, fst <$> cursor, snd <$> cursor, limit + 1) :: IO [(Integer, Integer, Value)]
  let page = take limit rows
      nextCursor = if length rows > limit then (\(b,l,_) -> T.pack $ show b <> ":" <> show l) <$> listToMaybe (reverse page) else Nothing
  pure $ Right $ mkResponse (maybe 0 (\(b,_,_) -> b) $ listToMaybe rows) chain $ object
    [ "events" .= map (\(_,_,v) -> v) page, "nextCursor" .= nextCursor ]
