module Plether.Types.History
  ( Transaction (..)
  , TransactionType (..)
  , TransactionSide (..)
  , TransactionStatus (..)
  , TransactionData (..)
  , TransactionHistory (..)
  , Pagination (..)
  , HistoryParams (..)
  , defaultHistoryParams
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), Value, object, withText, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

data TransactionType
  = TxMint
  | TxBurn
  | TxSwap
  | TxZapBuy
  | TxZapSell
  | TxStake
  | TxUnstake
  | TxLeverageOpen
  | TxLeverageClose
  | TxSupply
  | TxWithdraw
  | TxBorrow
  | TxRepay
  deriving stock (Show, Eq, Generic)

instance ToJSON TransactionType where
  toJSON = \case
    TxMint -> "mint"
    TxBurn -> "burn"
    TxSwap -> "swap"
    TxZapBuy -> "zap_buy"
    TxZapSell -> "zap_sell"
    TxStake -> "stake"
    TxUnstake -> "unstake"
    TxLeverageOpen -> "leverage_open"
    TxLeverageClose -> "leverage_close"
    TxSupply -> "lending_supply"
    TxWithdraw -> "lending_withdraw"
    TxBorrow -> "lending_borrow"
    TxRepay -> "lending_repay"

instance FromJSON TransactionType where
  parseJSON = withText "TransactionType" $ \case
    "mint" -> pure TxMint
    "burn" -> pure TxBurn
    "swap" -> pure TxSwap
    "zap_buy" -> pure TxZapBuy
    "zap_sell" -> pure TxZapSell
    "stake" -> pure TxStake
    "unstake" -> pure TxUnstake
    "leverage_open" -> pure TxLeverageOpen
    "leverage_close" -> pure TxLeverageClose
    "lending_supply" -> pure TxSupply
    "lending_withdraw" -> pure TxWithdraw
    "lending_borrow" -> pure TxBorrow
    "lending_repay" -> pure TxRepay
    other -> fail $ "Unknown transaction type: " <> T.unpack other

txTypeToText :: TransactionType -> Text
txTypeToText = \case
  TxMint -> "mint"
  TxBurn -> "burn"
  TxSwap -> "swap"
  TxZapBuy -> "zap_buy"
  TxZapSell -> "zap_sell"
  TxStake -> "stake"
  TxUnstake -> "unstake"
  TxLeverageOpen -> "leverage_open"
  TxLeverageClose -> "leverage_close"
  TxSupply -> "lending_supply"
  TxWithdraw -> "lending_withdraw"
  TxBorrow -> "lending_borrow"
  TxRepay -> "lending_repay"

textToTxType :: Text -> Maybe TransactionType
textToTxType = \case
  "mint" -> Just TxMint
  "burn" -> Just TxBurn
  "swap" -> Just TxSwap
  "zap_buy" -> Just TxZapBuy
  "zap_sell" -> Just TxZapSell
  "stake" -> Just TxStake
  "unstake" -> Just TxUnstake
  "leverage_open" -> Just TxLeverageOpen
  "leverage_close" -> Just TxLeverageClose
  "lending_supply" -> Just TxSupply
  "lending_withdraw" -> Just TxWithdraw
  "lending_borrow" -> Just TxBorrow
  "lending_repay" -> Just TxRepay
  _ -> Nothing

data TransactionSide = SideBear | SideBull
  deriving stock (Show, Eq, Generic)

instance ToJSON TransactionSide where
  toJSON SideBear = "bear"
  toJSON SideBull = "bull"

instance FromJSON TransactionSide where
  parseJSON = withText "TransactionSide" $ \case
    "bear" -> pure SideBear
    "bull" -> pure SideBull
    other -> fail $ "Unknown side: " <> T.unpack other

data TransactionStatus = Success | Failed | Pending
  deriving stock (Show, Eq, Generic)

instance ToJSON TransactionStatus where
  toJSON Success = "success"
  toJSON Failed = "failed"
  toJSON Pending = "pending"

instance FromJSON TransactionStatus where
  parseJSON = withText "TransactionStatus" $ \case
    "success" -> pure Success
    "failed" -> pure Failed
    "pending" -> pure Pending
    other -> fail $ "Unknown status: " <> T.unpack other

data TransactionData = TransactionData
  { tdAmount :: Maybe Integer
  , tdAmountOut :: Maybe Integer
  , tdTokenIn :: Maybe Text
  , tdTokenOut :: Maybe Text
  , tdPrincipal :: Maybe Integer
  , tdLeverage :: Maybe Integer
  , tdCollateral :: Maybe Integer
  , tdDebt :: Maybe Integer
  }
  deriving stock (Show, Generic)

instance ToJSON TransactionData where
  toJSON TransactionData {..} =
    object $
      maybe [] (\v -> ["amount" .= show v]) tdAmount
        ++ maybe [] (\v -> ["amountOut" .= show v]) tdAmountOut
        ++ maybe [] (\v -> ["tokenIn" .= v]) tdTokenIn
        ++ maybe [] (\v -> ["tokenOut" .= v]) tdTokenOut
        ++ maybe [] (\v -> ["principal" .= show v]) tdPrincipal
        ++ maybe [] (\v -> ["leverage" .= show v]) tdLeverage
        ++ maybe [] (\v -> ["collateral" .= show v]) tdCollateral
        ++ maybe [] (\v -> ["debt" .= show v]) tdDebt

instance FromJSON TransactionData

data Transaction = Transaction
  { txId :: Integer
  , txHash :: Text
  , txBlockNumber :: Integer
  , txTimestamp :: Integer
  , txUserAddress :: Text
  , txType :: TransactionType
  , txSide :: Maybe TransactionSide
  , txStatus :: TransactionStatus
  , txData :: Value
  }
  deriving stock (Show, Generic)

instance ToJSON Transaction where
  toJSON Transaction {..} =
    object
      [ "id" .= txId
      , "hash" .= txHash
      , "blockNumber" .= txBlockNumber
      , "timestamp" .= txTimestamp
      , "userAddress" .= txUserAddress
      , "type" .= txType
      , "side" .= txSide
      , "status" .= txStatus
      , "data" .= txData
      ]

data Pagination = Pagination
  { pagPage :: Int
  , pagLimit :: Int
  , pagTotal :: Int
  , pagHasMore :: Bool
  }
  deriving stock (Show, Generic)

instance ToJSON Pagination where
  toJSON Pagination {..} =
    object
      [ "page" .= pagPage
      , "limit" .= pagLimit
      , "total" .= pagTotal
      , "hasMore" .= pagHasMore
      ]

data TransactionHistory = TransactionHistory
  { histTransactions :: [Transaction]
  , histPagination :: Pagination
  }
  deriving stock (Show, Generic)

instance ToJSON TransactionHistory where
  toJSON TransactionHistory {..} =
    object
      [ "transactions" .= histTransactions
      , "pagination" .= histPagination
      ]

data HistoryParams = HistoryParams
  { hpPage :: Int
  , hpLimit :: Int
  , hpTxType :: Maybe Text
  , hpSide :: Maybe Text
  , hpTxTypes :: [Text]
  }
  deriving stock (Show)

defaultHistoryParams :: HistoryParams
defaultHistoryParams =
  HistoryParams
    { hpPage = 1
    , hpLimit = 20
    , hpTxType = Nothing
    , hpSide = Nothing
    , hpTxTypes = []
    }
