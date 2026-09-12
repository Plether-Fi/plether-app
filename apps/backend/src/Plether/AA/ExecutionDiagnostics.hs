-- Advisory, post-reconciliation trace enrichment. No signing/submission calls.
module Plether.AA.ExecutionDiagnostics (executionOutOfGas, gasUtilizationBps, recoverExecutionDiagnostics, claimExecutionDiagnostics, completeExecutionDiagnostic) where

import Control.Monad (forM_, unless, when)
import Data.Aeson (Value(..), object, toJSON, (.=))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Maybe (fromMaybe)
import Database.PostgreSQL.Simple (query, Only, Connection)
import Plether.Config (Config(..))
import qualified Plether.AA.Paymaster as Paymaster
import Plether.Ethereum.Abi (decodeUint256)
import Plether.Database (DbPool, withDb)
import Plether.Ethereum.Client (EthClient, rpcCall)
import Plether.Ethereum.Rpc
import Plether.Logging (field, logError, logInfo)
import System.Timeout (timeout)

entryPoint :: Text
entryPoint = "0x4337084d9e255ff0702461cf8895ce9e3b5ff108"

-- Total receipt gas includes verification, execution, PVG and any EntryPoint
-- penalties. Do not compare it to callGasLimit alone or describe it as call gas.
gasUtilizationBps :: Value -> Value -> Maybe Integer
gasUtilizationBps (Object operation) (Object event) = do
  op <- either (const Nothing) Just $ Paymaster.parsePackedUserOperation operation
  String raw <- KM.lookup "data" event
  bytes <- either (const Nothing) Just $ B16.decode $ TE.encodeUtf8 $ T.drop 2 raw
  if not ("0x" `T.isPrefixOf` raw) || BS.length bytes /= 128 then Nothing else do
    let used = decodeUint256 $ BS.drop 96 bytes
        total = Paymaster.puoCallGasLimit op + Paymaster.puoVerificationGasLimit op + Paymaster.puoPreVerificationGas op
          + fromMaybe 0 (Paymaster.puoPaymasterVerificationGasLimit op) + fromMaybe 0 (Paymaster.puoPaymasterPostOpGasLimit op)
    if total <= 0 then Nothing else Just $ used * 10_000 `div` total
gasUtilizationBps _ _ = Nothing

data Node = Node Text Text Text (Maybe Text) [Node]

-- Empty revert data or high gas utilization alone never proves out-of-gas.
-- Match ONE exact EntryPoint -> sender/calldata execution, then require an OOG
-- failure on a continuously reverted call path, excluding caught inner errors.
executionOutOfGas :: Text -> Text -> Value -> Bool
executionOutOfGas sender calldata value = case parseNode 0 value of
  Right root@(Node _ to _ Nothing _) | to == entryPoint -> case matching root of
    [node@(Node _ _ _ (Just _) _)] -> propagatedOog node
    _ -> False
  _ -> False
 where
  matching node@(Node from to input _ children) =
    [node | from == entryPoint, to == T.toLower sender, input == T.toLower calldata]
      ++ concatMap matching children
  propagatedOog (Node _ _ _ (Just err) children) =
    err == "out of gas" || "out of gas: " `T.isPrefixOf` err
      || any propagatedOog children
  propagatedOog _ = False

parseNode :: Int -> Value -> Either () Node
parseNode depth (Object fields) = do
  unless (depth <= 64) $ Left ()
  from <- text "from"
  to <- text "to"
  input <- text "input"
  err <- case KM.lookup "error" fields of
    Nothing -> Right Nothing
    Just (String s) | not (T.null s) -> Right $ Just $ T.toLower s
    _ -> Left ()
  children <- case KM.lookup "calls" fields of
    Nothing -> Right []
    Just (Array a) | length a <= 256 -> traverse (parseNode $ depth+1) $ toList a
    _ -> Left ()
  pure $ Node from to input err children
 where
  text key = case KM.lookup key fields of
    Just (String s) -> Right $ T.toLower s
    _ -> Left ()
parseNode _ _ = Left ()

recoverExecutionDiagnostics :: Config -> DbPool -> EthClient -> IO ()
recoverExecutionDiagnostics cfg pool client = do
  -- Reuse the existing diagnostic lease/resolution columns for failed operations
  -- (order correlation only claims successful ones). CAS completion deduplicates
  -- logs across API instances; no budget lock or DB connection spans trace RPC.
  claims <- withDb pool $ \conn -> claimExecutionDiagnostics conn (cfgPerpsChainId cfg) (cfgPerpsOrderRouter cfg)
  forM_ claims $ \(attempt,sender,calldata,tx,number,hash,lease) -> do
    _ <- timeout 4_000_000 $ do
      receiptResult <- ethGetTransactionReceipt client tx
      case receiptResult of
        Right (Just receipt)
          | receiptSucceeded receipt, receiptBlockNumber receipt == number
          , T.toLower (receiptTxHash receipt) == T.toLower tx
          , T.toLower (receiptBlockHash receipt) == T.toLower hash -> do
              traced <- rpcCall client "debug_traceTransaction" $ toJSONParams tx
              case traced of
                Right trace@(Object _) -> do
                  header <- ethGetBlockByNumber client number
                  case header of
                    Right block | T.toLower (rpcBlockHash block) == T.toLower hash -> do
                      let oog = executionOutOfGas sender calldata trace
                          reason = if oog then "USER_OPERATION_OUT_OF_GAS" else "USER_OPERATION_REVERTED" :: Text
                      completed <- withDb pool $ \conn -> completeExecutionDiagnostic conn attempt lease reason
                      when completed $
                        (if oog then logError else logInfo) "aa_execution_diagnosed" "Safely reconciled execution diagnostic"
                          [field "attempt_id" attempt,field "stage" ("execution" :: Text),field "reason_code" reason,
                           field "outcome" ("failure" :: Text)]
                    _ -> pure ()
                _ -> pure ()
        _ -> pure ()
    pure ()
 where
  toJSONParams tx = toJSON [String tx,object ["tracer" .= ("callTracer" :: Text),"timeout" .= ("3s" :: Text)]]

claimExecutionDiagnostics :: Connection -> Integer -> Text -> IO [(Text,Text,Text,Text,Integer,Text,Text)]
claimExecutionDiagnostics conn chain deployment = query conn
    "WITH candidates AS (SELECT d.attempt_id FROM aa_attempt_diagnostics d JOIN aa_user_operation_events e ON e.user_operation_hash=d.operation_hash JOIN aa_sponsorship_authorizations a ON a.digest=e.digest AND a.client_key=d.client_key AND a.sender=d.sender WHERE d.chain_id=? AND d.deployment=? AND d.attempt_id=(SELECT first_d.attempt_id FROM aa_attempt_diagnostics first_d WHERE first_d.operation_hash=d.operation_hash ORDER BY first_d.created_at,first_d.attempt_id LIMIT 1) AND d.stage='user_operation_reverted' AND d.correlation_resolved_at IS NULL AND a.state='settled' AND NOT e.success AND e.finalized_at IS NOT NULL AND (d.correlation_checked_at IS NULL OR d.correlation_checked_at<clock_timestamp()-interval '5 minutes') ORDER BY d.correlation_checked_at NULLS FIRST,d.created_at LIMIT 4 FOR UPDATE OF d SKIP LOCKED), claimed AS (UPDATE aa_attempt_diagnostics d SET correlation_checked_at=clock_timestamp() FROM candidates c WHERE d.attempt_id=c.attempt_id RETURNING d.*) SELECT d.attempt_id::text,d.sender,p.operation->>'callData',e.transaction_hash,e.block_number,e.block_hash,d.correlation_checked_at::text FROM claimed d JOIN aa_user_operation_events e ON e.user_operation_hash=d.operation_hash JOIN aa_preparations p ON p.authorization_digest=e.digest AND p.client_key=d.client_key AND p.sender=d.sender AND p.preparation_id=d.preparation_id"
    (chain,T.toLower deployment) :: IO [(Text,Text,Text,Text,Integer,Text,Text)]

completeExecutionDiagnostic :: Connection -> Text -> Text -> Text -> IO Bool
completeExecutionDiagnostic conn attempt lease reason = do
  unless (reason `elem` ["USER_OPERATION_OUT_OF_GAS","USER_OPERATION_REVERTED"]) $ fail "Invalid diagnostic reason"
  rows <- query conn
    "UPDATE aa_attempt_diagnostics SET reason=?,correlation_resolved_at=clock_timestamp(),updated_at=clock_timestamp() WHERE attempt_id=?::uuid AND correlation_checked_at=?::timestamptz AND correlation_resolved_at IS NULL AND stage='user_operation_reverted' RETURNING attempt_id::text"
    (reason,attempt,lease) :: IO [Only Text]
  pure $ not $ null rows
