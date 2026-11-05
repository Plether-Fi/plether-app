-- One durable transaction journal per router, owned by the order keeper's
-- session advisory lock. Unknown broadcasts are retried with identical bytes.
module Plether.Keeper.Broadcast (saveBroadcast, clearBroadcast, reconcileBroadcast) where

import Control.Monad (void, when)
import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import Database.PostgreSQL.Simple (Connection, Only (..), execute, query, withTransaction)
import Database.PostgreSQL.Simple.Types (Binary (..))
import Plether.Ethereum.Client (EthClient)
import Plether.Ethereum.Rpc
  ( TxReceipt, RpcBlock (..), ethGetTransactionReceipt, ethSendRawTransaction
  , ethChainId, ethSafeBlock, ethGetBlockByNumber, ethGetTransactionCountAtBlock )
import Plether.Ethereum.Transaction (SignedTransaction (..), Tx1559 (..), decodeSignedTransaction, rawTransactionHash)
import Plether.Logging (field, logErrorEvery, logWarn, logWarnEvery)

saveBroadcast :: Connection -> Text -> SignedTransaction -> IO ()
saveBroadcast conn router signed = void $ execute conn
  "INSERT INTO perps_keeper_broadcasts(order_router,tx_hash,raw_tx) VALUES (?,?,?)"
  (T.toLower router, signedTransactionHash signed, Binary $ signedRawTransaction signed)

clearBroadcast :: Connection -> Text -> Text -> IO ()
clearBroadcast conn router hash = void $ execute conn
  "DELETE FROM perps_keeper_broadcasts WHERE order_router=? AND tx_hash=?"
  (T.toLower router, hash)

-- False means no new nonce may be allocated. Missing receipts and send errors
-- alone never release a journal. A canonical safe-block nonce can prove that
-- another transaction consumed this signed transaction's nonce.
reconcileBroadcast :: Connection -> Text -> EthClient -> (TxReceipt -> IO ()) -> IO Bool
reconcileBroadcast conn router client applyReceipt = do
  rows <- query conn "SELECT tx_hash,raw_tx FROM perps_keeper_broadcasts WHERE order_router=?"
    (Only $ T.toLower router) :: IO [(Text, ByteString)]
  case rows of
    [] -> pure True
    [(hash, raw)] | rawTransactionHash raw == hash -> do
      receipt <- ethGetTransactionReceipt client hash
      case receipt of
        Right (Just mined) -> do
          withTransaction conn $ applyReceipt mined >> clearBroadcast conn router hash
          pure True
        Right Nothing -> do
          decoded <- decodeSignedTransaction raw
          case decoded of
            Left _ -> invalidJournal
            Right (tx, signer) -> do
              recovered <- recoverConsumedNonce hash raw tx signer
              if recovered then pure True else rebroadcast raw
        Left _ -> rebroadcast raw
    _ -> invalidJournal
 where
  invalidJournal = do
    logErrorEvery 60 "keeper_broadcast_invalid" "Keeper broadcast journal failed integrity validation" []
    pure False
  rebroadcast raw = do
    due <- execute conn "UPDATE perps_keeper_broadcasts SET last_broadcast_at=now() WHERE order_router=? AND last_broadcast_at < now()-interval '5 seconds'"
      (Only $ T.toLower router)
    when (due > 0) $ void $ ethSendRawTransaction client raw
    logWarnEvery 30 "keeper_broadcast_unresolved" "Keeper is reconciling its existing signed transaction" []
    pure False
  recoverConsumedNonce hash raw tx signer
    | T.toLower (txTo tx) /= T.toLower router = pure False
    | otherwise = do
        chain <- ethChainId client
        safe <- ethSafeBlock client
        case (chain, safe) of
          (Right chainId, Right block) | chainId == txChainId tx -> do
            nonce <- ethGetTransactionCountAtBlock client signer (rpcBlockNumber block)
            case nonce of
              Right consumed | consumed > txNonce tx -> do
                -- Recheck both the receipt and pinned block. RPC failures,
                -- a receipt appearing, or a reorg keep the journal intact.
                receipt <- ethGetTransactionReceipt client hash
                canonical <- ethGetBlockByNumber client (rpcBlockNumber block)
                case (receipt, canonical) of
                  (Right Nothing, Right verified) | rpcBlockHash verified == rpcBlockHash block -> do
                    let proof = object ["sender" .= signer, "nonce" .= txNonce tx,
                          "chainId" .= chainId, "safeBlockNumber" .= rpcBlockNumber block,
                          "safeBlockHash" .= rpcBlockHash block, "safeNonce" .= consumed]
                    changed <- withTransaction conn $ do
                      archived <- execute conn
                        "INSERT INTO perps_keeper_broadcast_recoveries(tx_hash,order_router,raw_tx,proof) SELECT tx_hash,order_router,raw_tx,?::jsonb FROM perps_keeper_broadcasts WHERE order_router=? AND tx_hash=? AND raw_tx=?"
                        (Aeson.encode proof, T.toLower router, hash, Binary raw)
                      if archived /= 1 then pure False else do
                        clearBroadcast conn router hash
                        pure True
                    when changed $ logWarn "keeper_broadcast_nonce_consumed"
                      "Archived an unmined keeper transaction whose nonce was consumed at a canonical safe block"
                      [field "transaction_hash" hash, field "signer" signer,
                       field "nonce" $ txNonce tx, field "safe_nonce" consumed,
                       field "safe_block" $ rpcBlockNumber block]
                    pure changed
                  _ -> pure False
              _ -> pure False
          _ -> pure False
