module Plether.Perps.Funding.Source
  ( validateSourceTransaction, sourceTransactionMatches, sourceReceiptStatus, sourceReceiptEvidence ) where

import Control.Monad (unless)
import Control.Monad.Trans.Except
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Plether.Ethereum.Client
import Plether.Ethereum.Rpc
import Plether.Perps.Funding.Types

validateSourceTransaction :: EthClient -> Value -> Text -> IO (Either Text ())
validateSourceTransaction client intent hash = runExceptT $ do
  chain <- rpc $ ethChainId client
  unless (Just chain == fieldInteger "sourceChainId" intent) $ throwE "SOURCE_CHAIN_MISMATCH"
  transaction <- rpc $ rpcCall client "eth_getTransactionByHash" $ toJSON [hash]
  either throwE pure $ sourceTransactionMatches intent hash transaction
  where rpc operation = ExceptT $ either (const $ Left "SOURCE_RPC_UNAVAILABLE") Right <$> operation

sourceTransactionMatches :: Value -> Text -> Value -> Either Text ()
sourceTransactionMatches intent hash transaction = do
  unless (transaction /= Null) $ Left "SOURCE_TRANSACTION_NOT_VISIBLE"
  owner <- maybe (Left "INVALID_INTENT_OWNER") Right $ fieldText "ownerAddress" intent
  let transactions = fromMaybe [] $ parseMaybe (withObject "intent" (.: "sourceTransactions")) intent :: [Value]
      bridges = filter ((== Just "bridge") . fieldText "kind") transactions
  planned <- case bridges of [one] -> Right one; _ -> Left "INVALID_SOURCE_PLAN"
  to <- maybe (Left "INVALID_SOURCE_PLAN") Right $ fieldText "to" planned
  input <- maybe (Left "INVALID_SOURCE_PLAN") Right $ fieldText "data" planned
  value <- maybe (Left "INVALID_SOURCE_PLAN") Right $ fieldText "value" planned
  amount <- case reads (T.unpack value) of [(n,"")] | n >= 0 -> Right n; _ -> Left "INVALID_SOURCE_PLAN"
  submittedValue <- maybe (Left "INVALID_SOURCE_TRANSACTION") (either (const $ Left "INVALID_SOURCE_TRANSACTION") Right . parseRpcQuantity "source value") $ fieldText "value" transaction
  unless (fmap T.toLower (fieldText "hash" transaction) == Just (T.toLower hash)
    && fmap T.toLower (fieldText "from" transaction) == Just owner
    && fmap T.toLower (fieldText "to" transaction) == Just (T.toLower to)
    && fmap T.toLower (fieldText "input" transaction) == Just (T.toLower input)
    && amount == submittedValue) $ Left "SOURCE_TRANSACTION_BINDING_MISMATCH"

-- The source receipt describes bridge submission, never destination margin.
sourceReceiptStatus :: EthClient -> Text -> IO (Either Text Text)
sourceReceiptStatus client hash = fmap (>>= maybe (Left "SOURCE_RECEIPT_UNAVAILABLE") Right . fieldText "sourceStatus") $ sourceReceiptEvidence client hash

sourceReceiptEvidence :: EthClient -> Text -> IO (Either Text Value)
sourceReceiptEvidence client hash = runExceptT $ do
  result <- rpc $ ethGetTransactionReceipt client hash
  case result of
    Nothing -> pure $ object ["sourceStatus" .= ("pending" :: Text),"sourceTerminal" .= False
      ,"sourceBlockNumber" .= Null,"sourceBlockHash" .= Null,"sourceConfirmations" .= (0 :: Integer)]
    Just receipt -> do
      canonical <- rpc $ ethGetBlockByNumber client $ receiptBlockNumber receipt
      headBlock <- rpc $ ethBlockNumber client
      unless (receiptTxHash receipt == hash && receiptBlockHash receipt == rpcBlockHash canonical) $ throwE "SOURCE_RECEIPT_REORGED"
      let confirmations = max 0 $ headBlock - receiptBlockNumber receipt + 1
          sourceStatus = if confirmations < 2 then "pending" else if receiptSucceeded receipt then "confirmed" else "reverted" :: Text
      pure $ object ["sourceStatus" .= sourceStatus,"sourceTerminal" .= (sourceStatus == "reverted")
        ,"sourceBlockNumber" .= show (receiptBlockNumber receipt),"sourceBlockHash" .= receiptBlockHash receipt
        ,"sourceConfirmations" .= confirmations]
  where rpc operation = ExceptT $ either (const $ Left "SOURCE_RPC_UNAVAILABLE") Right <$> operation
