-- Advisory receipt correlation. This never submits a transaction or changes
-- authorization, accounting, keeper progress, or reconciliation state.
module Plether.AA.OrderDiagnostics
  ( recoverOrderDiagnostics, receiptOrder, claimOrderDiagnostics, completeOrderDiagnostic
  , executionFailureReason
  ) where

import Control.Monad (forM_, void)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.List (nub, sortOn)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.PostgreSQL.Simple (Connection, Only(..), execute, query)
import Plether.Config (Config(..))
import Plether.Database (DbPool, withDb)
import Plether.Ethereum.Abi (keccak256)
import Plether.Ethereum.Client (EthClient)
import qualified Plether.Ethereum.Contracts.Perps as Perps
import Plether.Ethereum.Rpc
import Plether.Logging (field, logInfo, logError)
import System.Timeout (timeout)

-- Nothing means malformed/ambiguous evidence. Just Nothing is a successfully
-- verified UserOperation with no order registration (for example a deposit).
-- Every EntryPoint event delimits its own execution, including operations from
-- other paymasters; matching only sponsored events would misattribute bundles.
receiptOrder :: Text -> Text -> Text -> TxReceipt -> Maybe (Maybe Integer)
receiptOrder lifecycle sender operation receipt
  | not (receiptSucceeded receipt) = Nothing
  | length indices /= length (nub indices) || any (<0) indices = Nothing
  | any (not . belongsToReceipt) ordered = Nothing
  | otherwise = case matches of
      [(previous, current)] ->
        let entries = [entry | entry <- ordered, rpcLogIndex entry > previous
              , rpcLogIndex entry < rpcLogIndex current
              , same (rpcLogAddress entry) lifecycle
              , take 1 (rpcLogTopics entry) == [Perps.intentRegisteredTopic]]
        in case entries of
          [] -> Just Nothing
          [entry]
            | length (rpcLogTopics entry) == 4
            , all ((==32) . BS.length) (rpcLogTopics entry)
            , Just Perps.IntentRegistered{Perps.poeOrderId=orderId, Perps.poeAccount=account} <- Perps.decodePerpsOrderEvent entry
            , orderId > 0, orderId < 2^(64 :: Int), same account sender -> Just $ Just orderId
          _ -> Nothing
      _ -> Nothing
 where
  same a b = T.toLower a == T.toLower b
  encoded = ("0x" <>) . TE.decodeUtf8 . B16.encode
  ordered = sortOn rpcLogIndex $ receiptLogs receipt
  indices = map rpcLogIndex ordered
  belongsToReceipt entry = same (rpcLogTxHash entry) (receiptTxHash receipt)
    && same (rpcLogBlockHash entry) (receiptBlockHash receipt)
    && rpcLogBlockNumber entry == receiptBlockNumber receipt
    && rpcLogTransactionIndex entry == receiptTransactionIndex receipt
  events = [entry | entry <- ordered
    , same (rpcLogAddress entry) "0x4337084d9e255ff0702461cf8895ce9e3b5ff108"
    , take 1 (rpcLogTopics entry) == [keccak256 "UserOperationEvent(bytes32,address,address,uint256,bool,uint256,uint256)"]]
  matches = [(previous, entry) | (previous, entry) <- zip (-1 : map rpcLogIndex events) events
    , [_topic, operationHash, account, paymaster] <- [rpcLogTopics entry]
    , all ((==32) . BS.length) [operationHash, account, paymaster]
    , BS.take 12 account == BS.replicate 12 0
    , same (encoded operationHash) operation, same (encoded $ BS.drop 12 account) sender
    , BS.length (rpcLogData entry) == 128
    , BS.take 32 (BS.drop 32 $ rpcLogData entry) == BS.replicate 31 0 <> BS.singleton 1]

-- Short autocommit claim, no transaction/connection is kept across RPC. A lost
-- worker resumes after thirty seconds. The timestamp is also a fencing token:
-- a slow worker cannot complete a claim that a newer worker has recovered.
claimOrderDiagnostics :: Connection -> Integer -> Text -> IO [(Text, Text, Text, Text, Integer, Text, Text)]
claimOrderDiagnostics conn chain router = query conn
  "WITH candidates AS (SELECT d.attempt_id FROM aa_attempt_diagnostics d JOIN aa_user_operation_events e ON e.user_operation_hash=d.operation_hash JOIN aa_sponsorship_authorizations a ON a.digest=e.digest AND a.client_key=d.client_key AND a.sender=d.sender WHERE d.chain_id=? AND d.deployment=? AND d.order_id IS NULL AND d.correlation_resolved_at IS NULL AND d.terminal_at IS NULL AND a.state='settled' AND e.success AND e.finalized_at IS NOT NULL AND (d.correlation_checked_at IS NULL OR d.correlation_checked_at < clock_timestamp()-interval '30 seconds') ORDER BY d.correlation_checked_at NULLS FIRST,d.created_at LIMIT 8 FOR UPDATE OF d SKIP LOCKED), claimed AS (UPDATE aa_attempt_diagnostics d SET correlation_checked_at=clock_timestamp() FROM candidates c WHERE d.attempt_id=c.attempt_id RETURNING d.*) SELECT d.attempt_id::text,d.sender,d.operation_hash,e.transaction_hash,e.block_number,e.block_hash,d.correlation_checked_at::text FROM claimed d JOIN aa_user_operation_events e ON e.user_operation_hash=d.operation_hash"
  (chain, T.toLower router)

completeOrderDiagnostic :: Connection -> Text -> Text -> Maybe Integer -> IO Bool
completeOrderDiagnostic conn attempt lease order = do
  rows <- query conn
    "UPDATE aa_attempt_diagnostics SET order_id=?,correlation_resolved_at=clock_timestamp(),stage=CASE WHEN ?::bigint IS NULL THEN stage ELSE 'committed' END,updated_at=clock_timestamp() WHERE attempt_id=?::uuid AND correlation_checked_at=?::timestamptz AND correlation_resolved_at IS NULL AND order_id IS NULL AND terminal_at IS NULL RETURNING attempt_id::text"
    (order, order, attempt, lease) :: IO [Only Text]
  pure $ not $ null rows

executionFailureReason :: Text -> Text
executionFailureReason err
  | "insufficient funds" `T.isInfixOf` T.toLower err = "KEEPER_INSUFFICIENT_FUNDS"
  | "timeout" `T.isInfixOf` T.toLower err = "KEEPER_RPC_TIMEOUT"
  | otherwise = "KEEPER_EXECUTION_FAILED"

recoverOrderDiagnostics :: Config -> DbPool -> EthClient -> IO ()
recoverOrderDiagnostics cfg pool client = case cfgPerpsOrderLifecycleBook cfg of
  Nothing -> pure ()
  Just lifecycle -> do
    candidates <- withDb pool $ \conn -> claimOrderDiagnostics conn (cfgPerpsChainId cfg) (cfgPerpsOrderRouter cfg)
    forM_ candidates $ \(attempt,sender,operation,transaction,blockNumber,blockHash,lease) -> void $ timeout 2_000_000 $ do
      receiptResult <- ethGetTransactionReceipt client transaction
      case receiptResult of
        Right (Just receipt)
          | T.toLower (receiptTxHash receipt) == transaction
          , receiptBlockNumber receipt == blockNumber
          , T.toLower (receiptBlockHash receipt) == blockHash
          , Just order <- receiptOrder lifecycle sender operation receipt -> do
              -- Recheck the exact canonical header; the reconciler supplied the
              -- safe/finalized evidence, not a latest-head keeper observation.
              header <- ethGetBlockByNumber client blockNumber
              case header of
                Right block | T.toLower (rpcBlockHash block) == blockHash -> do
                  changed <- withDb pool $ \conn -> completeOrderDiagnostic conn attempt lease order
                  case (changed,order) of
                    (True,Just _) -> logInfo "aa_order_committed" "Recovered sponsored order correlation"
                      [field "attempt_id" attempt, field "stage" ("committed" :: Text)]
                    _ -> pure ()
                _ -> pure ()
        _ -> pure ()
    -- Keeper errors are durable even if they preceded diagnostic materialization.
    -- Preserve their actual timestamp; never use today's readiness as the cause.
    withDb pool $ \conn -> do
      failures <- query conn
        "SELECT d.attempt_id::text,k.last_error,k.last_attempt_at::text FROM aa_attempt_diagnostics d JOIN perps_keeper_orders k ON k.order_router=d.deployment AND k.order_id=d.order_id AND k.account=d.sender WHERE d.chain_id=? AND d.deployment=? AND d.terminal_at IS NULL AND k.last_error IS NOT NULL AND k.last_attempt_at IS NOT NULL AND (d.execution_observed_at IS NULL OR d.execution_observed_at<k.last_attempt_at) ORDER BY k.last_attempt_at LIMIT 100"
        (cfgPerpsChainId cfg,T.toLower $ cfgPerpsOrderRouter cfg) :: IO [(Text,Text,Text)]
      forM_ failures $ \(attempt,err,observed) -> do
        let reason = executionFailureReason err
        changed <- execute conn
          "UPDATE aa_attempt_diagnostics SET reason=?,stage='execution_attempt_failed',execution_observed_at=?::timestamp,updated_at=clock_timestamp() WHERE attempt_id=?::uuid AND terminal_at IS NULL AND (execution_observed_at IS NULL OR execution_observed_at<?::timestamp)"
          (reason,observed,attempt,observed)
        if changed == 0 then pure () else logError "aa_order_execution_attempt_failed" "Recovered historical keeper execution issue"
          [field "attempt_id" attempt,field "stage" ("execution" :: Text),field "reason_code" reason]
