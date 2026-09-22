-- One durable transaction journal per router, owned by the order keeper's
-- session advisory lock. Unknown broadcasts are retried with identical bytes.
module Plether.Keeper.Broadcast (saveBroadcast, clearBroadcast, reconcileBroadcast) where

import Control.Monad (void, when)
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import Database.PostgreSQL.Simple (Connection, Only (..), execute, query, withTransaction)
import Database.PostgreSQL.Simple.Types (Binary (..))
import Plether.Ethereum.Client (EthClient)
import Plether.Ethereum.Rpc (TxReceipt, ethGetTransactionReceipt, ethSendRawTransaction)
import Plether.Ethereum.Transaction (SignedTransaction (..), rawTransactionHash)
import Plether.Logging (logErrorEvery, logWarnEvery)

saveBroadcast :: Connection -> Text -> SignedTransaction -> IO ()
saveBroadcast conn router signed = void $ execute conn
  "INSERT INTO perps_keeper_broadcasts(order_router,tx_hash,raw_tx) VALUES (?,?,?)"
  (T.toLower router, signedTransactionHash signed, Binary $ signedRawTransaction signed)

clearBroadcast :: Connection -> Text -> Text -> IO ()
clearBroadcast conn router hash = void $ execute conn
  "DELETE FROM perps_keeper_broadcasts WHERE order_router=? AND tx_hash=?"
  (T.toLower router, hash)

-- False means no new nonce may be allocated. A failed RPC, malformed journal,
-- or unknown receipt never grants permission to replace the saved transaction.
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
        _ -> do
          due <- execute conn "UPDATE perps_keeper_broadcasts SET last_broadcast_at=now() WHERE order_router=? AND last_broadcast_at < now()-interval '5 seconds'"
            (Only $ T.toLower router)
          when (due > 0) $ void $ ethSendRawTransaction client raw
          logWarnEvery 30 "keeper_broadcast_unresolved" "Keeper is reconciling its existing signed transaction" []
          pure False
    _ -> do
      logErrorEvery 60 "keeper_broadcast_invalid" "Keeper broadcast journal failed integrity validation" []
      pure False
