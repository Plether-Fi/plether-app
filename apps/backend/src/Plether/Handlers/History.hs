module Plether.Handlers.History
  ( getHistory
  , getLeverageHistory
  , getLendingHistory
  ) where

import Data.Aeson (Value)
import Data.Text (Text)
import qualified Data.Text as T
import Plether.Config (Config (..))
import Plether.Database (DbPool, withDb)
import Plether.Database.Schema
  ( TransactionRow (..)
  , getTransactionCount
  , getTransactionsByUser
  )
import Plether.Ethereum.Client (EthClient, ethBlockNumber)
import Plether.Types (ApiError, ApiResponse, mkResponse, rpcErrorToApiError)
import Plether.Types.History

getHistory
  :: DbPool
  -> EthClient
  -> Config
  -> Text
  -> HistoryParams
  -> IO (Either ApiError (ApiResponse TransactionHistory))
getHistory pool client cfg userAddr params = do
  eBlockNum <- ethBlockNumber client
  case eBlockNum of
    Left err -> pure $ Left $ rpcErrorToApiError err
    Right blockNum -> do
      let offset = (hpPage params - 1) * hpLimit params
      txs <- withDb pool $ \conn ->
        getTransactionsByUser conn userAddr (hpTxType params) (hpSide params) (hpTxTypes params) (hpLimit params) offset
      total <- withDb pool $ \conn ->
        getTransactionCount conn userAddr (hpTxType params) (hpSide params) (hpTxTypes params)

      let transactions = map rowToTransaction txs
          pagination = Pagination
            { pagPage = hpPage params
            , pagLimit = hpLimit params
            , pagTotal = total
            , pagHasMore = offset + length txs < total
            }
          history = TransactionHistory
            { histTransactions = transactions
            , histPagination = pagination
            }

      pure $ Right $ mkResponse blockNum (cfgChainId cfg) history

getLeverageHistory
  :: DbPool
  -> EthClient
  -> Config
  -> Text
  -> HistoryParams
  -> IO (Either ApiError (ApiResponse TransactionHistory))
getLeverageHistory pool client cfg userAddr params =
  getHistory pool client cfg userAddr params
    { hpTxTypes = ["leverage_open", "leverage_close"]
    }

getLendingHistory
  :: DbPool
  -> EthClient
  -> Config
  -> Text
  -> HistoryParams
  -> IO (Either ApiError (ApiResponse TransactionHistory))
getLendingHistory pool client cfg userAddr params =
  getHistory pool client cfg userAddr params
    { hpTxTypes = ["supply", "withdraw", "borrow", "repay"]
    }

rowToTransaction :: TransactionRow -> Transaction
rowToTransaction TransactionRow {..} =
  Transaction
    { txId = trId
    , txHash = trTxHash
    , txBlockNumber = trBlockNumber
    , txTimestamp = trTimestamp
    , txUserAddress = trUserAddress
    , txType = textToTxType' trTxType
    , txSide = textToSide trSide
    , txStatus = textToStatus trStatus
    , txData = trData
    }

textToTxType' :: Text -> TransactionType
textToTxType' = \case
  "mint" -> TxMint
  "burn" -> TxBurn
  "swap" -> TxSwap
  "zap_buy" -> TxZapBuy
  "zap_sell" -> TxZapSell
  "stake" -> TxStake
  "unstake" -> TxUnstake
  "leverage_open" -> TxLeverageOpen
  "leverage_close" -> TxLeverageClose
  "supply" -> TxSupply
  "withdraw" -> TxWithdraw
  "borrow" -> TxBorrow
  "repay" -> TxRepay
  _ -> TxMint

textToSide :: Maybe Text -> Maybe TransactionSide
textToSide = \case
  Just "bear" -> Just SideBear
  Just "bull" -> Just SideBull
  _ -> Nothing

textToStatus :: Text -> TransactionStatus
textToStatus = \case
  "success" -> Success
  "failed" -> Failed
  "pending" -> Pending
  _ -> Success

