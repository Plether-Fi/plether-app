-- Display-only execution locators. A keeper hash never proves inclusion or a
-- terminal order; consumers must verify the exact canonical lifecycle receipt.
module Plether.Database.ExecutionProgress
  ( ExecutionProgress (..)
  , getExecutionProgress
  , executionProgressJson
  , executionProgressVersion
  , validExecutionVersion
  , parseExecutionConfirmations
  , readExecutionConfirmations
  ) where

import Control.Exception (try, throwIO)
import Data.Aeson (Value, object, (.=))
import Data.Char (isHexDigit)
import Data.Text (Text)
import qualified Data.Text as T
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import Plether.Config (Config (..))
import Plether.Logging (logWarnEvery)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

data ExecutionProgress = ExecutionProgress
  { epState :: Text
  , epOrderId :: Integer
  , epRouter :: Text
  , epAccount :: Text
  , epClientOrderId :: Text
  , epCommitTransaction :: Text
  , epTransaction :: Text
  }
  deriving stock (Eq, Show)

instance FromRow ExecutionProgress where
  fromRow = ExecutionProgress <$> field <*> field <*> field <*> field <*> field <*> field <*> field

-- Parse the same indexer range/default without adding configuration state or
-- silently lowering either confirmation requirement. Malformed configuration
-- disables this optional locator; canonical history continues normally.
parseExecutionConfirmations :: Int -> Maybe String -> Maybe Int
parseExecutionConfirmations keeper raw = do
  let text = maybe "1" id raw
  number <- readMaybe text :: Maybe Integer
  if null text || any (not . (`elem` ['0' .. '9'])) text || number < 0 || number > 10000
    then Nothing
    else Just $ max 1 $ max keeper (fromIntegral number)

readExecutionConfirmations :: Config -> IO (Maybe Int)
readExecutionConfirmations cfg = do
  raw <- lookupEnv "PERPS_INDEXER_CONFIRMATIONS"
  let parsed = parseExecutionConfirmations (cfgKeeperConfirmations cfg) raw
  case parsed of
    Nothing -> logWarnEvery 60 "execution_progress_invalid_confirmations" "Execution progress disabled for invalid confirmation configuration" []
    Just _ -> pure ()
  pure parsed

validExecutionVersion :: Text -> Bool
validExecutionVersion "none" = True
validExecutionVersion value
  | T.length value > 83 = False
  | otherwise = case T.splitOn ":" value of
      [hash, state] -> T.length hash == 66 && "0x" `T.isPrefixOf` hash
        && T.all isHexDigit (T.drop 2 hash) && state `elem` ["submitting", "submitted"]
      _ -> False

executionProgressVersion :: Maybe ExecutionProgress -> Text
executionProgressVersion Nothing = "none"
executionProgressVersion (Just progress) = epTransaction progress <> ":" <> epState progress

executionProgressJson :: Int -> ExecutionProgress -> Value
executionProgressJson confirmations progress = object
  [ "state" .= epState progress
  , "orderId" .= show (epOrderId progress)
  , "orderRouter" .= epRouter progress
  , "account" .= epAccount progress
  , "clientOrderId" .= epClientOrderId progress
  , "commitTxHash" .= epCommitTransaction progress
  , "transactionHash" .= epTransaction progress
  , "requiredConfirmations" .= confirmations
  ]

-- The outer Nothing means unavailable, not an authoritative clearing. Clients
-- must retain their last known locator through optional database outages.
getExecutionProgress :: Connection -> Integer -> Text -> Integer -> Maybe Text -> IO (Maybe (Maybe ExecutionProgress))
getExecutionProgress conn chain router order account = do
  result <- try $ query conn
    "SELECT p.state,o.order_id,lower(o.order_router),lower(o.account),lower(o.client_order_id),lower(o.commit_tx_hash),lower(p.tx_hash) \
    \FROM perps_orders o JOIN perps_keeper_orders k ON k.order_router=o.order_router AND k.order_id=o.order_id \
    \AND lower(k.account)=lower(o.account) AND lower(k.commit_tx_hash)=lower(o.commit_tx_hash) \
    \LEFT JOIN perps_keeper_broadcasts b ON b.order_router=o.order_router \
    \CROSS JOIN LATERAL(SELECT CASE WHEN o.order_id=ANY(b.target_order_ids) THEN b.tx_hash WHEN k.status='executed' THEN k.execution_tx_hash WHEN k.status='failed' THEN k.failure_tx_hash END AS tx_hash, \
    \CASE WHEN o.order_id=ANY(b.target_order_ids) AND b.submitted_at IS NULL THEN 'submitting' ELSE 'submitted' END AS state) p \
    \WHERE o.chain_id=? AND o.order_router=? AND o.order_id=? AND (?::text IS NULL OR lower(o.account)=?) \
    \AND lower(o.account) ~ '^0x[0-9a-f]{40}$' AND lower(o.client_order_id) ~ '^0x[0-9a-f]{64}$' \
    \AND lower(o.commit_tx_hash) ~ '^0x[0-9a-f]{64}$' AND lower(p.tx_hash) ~ '^0x[0-9a-f]{64}$' LIMIT 1"
    (chain, T.toLower router, order, T.toLower <$> account, T.toLower <$> account)
  case result of
    Right [progress] -> pure $ Just $ Just progress
    Right _ -> pure $ Just Nothing
    Left (err :: SqlError) | sqlState err `elem` ["42P01","42703"] -> do
      logWarnEvery 60 "execution_progress_schema_unavailable" "Optional execution progress schema unavailable; canonical history remains authoritative" []
      pure Nothing
    Left err -> throwIO err
