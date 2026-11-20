module Plether.Keeper.OracleWakeup
  ( notifyOracleWakeup
  , receiptMayAdvanceOracleMark
  ) where

import Control.Exception (IOException, bracket, try)
import Control.Monad (forM_, void, when)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Network.Socket as Socket
import qualified Network.Socket.ByteString as SocketBytes
import Plether.Config (Config (..))
import Plether.Ethereum.Contracts.Perps (isHistoricalOrderFill)
import Plether.Ethereum.Rpc (RpcLog (rpcLogAddress), TxReceipt (..))
import System.Timeout (timeout)

-- The keeper and updater share loopback in the consolidated Sepolia task.
-- This bounded hint only schedules the updater's existing on-chain health check.
notifyOracleWakeup :: Config -> TxReceipt -> IO ()
notifyOracleWakeup cfg receipt =
  when (cfgPerpsChainId cfg == 421614) $
    forM_ (cfgPerpsOrderLifecycleBook cfg) $ \book ->
      when (receiptMayAdvanceOracleMark book receipt) $
        void (try (timeout 50_000 sendWakeup) :: IO (Either IOException (Maybe ())))

sendWakeup :: IO ()
sendWakeup =
  bracket (Socket.socket Socket.AF_INET Socket.Datagram Socket.defaultProtocol) Socket.close $ \sock ->
    void $ SocketBytes.sendTo sock "plether-oracle-wake-v1" endpoint
  where
    endpoint = Socket.SockAddrInet 3017 (Socket.tupleToHostAddress (127, 0, 0, 1))

-- RPC decoding verifies the receipt and log identities. Core can skip its
-- mark write if the execution price is already covered by the engine mark.
receiptMayAdvanceOracleMark :: Text -> TxReceipt -> Bool
receiptMayAdvanceOracleMark book receipt =
  receiptSucceeded receipt && any fromLifecycleBook (receiptLogs receipt)
  where
    fromLifecycleBook entry =
      T.toLower (rpcLogAddress entry) == T.toLower book && isHistoricalOrderFill entry
