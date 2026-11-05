module Plether.Insights.DivisionsFunding
  ( FundingScope (..)
  , FundingFlow (..)
  , DepositPayerProof (..)
  , ensureDivisionsFundingSchema
  , loadDivisionsFunding
  , fundingFlowsQuery
  , decodeDepositPayer
  ) where

import Control.Monad (forM, guard, void)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.Int (Int64)
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.PostgreSQL.Simple (Connection, Query, execute, execute_, query)
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import Database.PostgreSQL.Simple.ToField (toField)
import Plether.Database (DbPool, withDb)
import Plether.Ethereum.Abi (encodeAddress, encodeUint256, keccak256)
import Plether.Ethereum.Client (EthClient)
import Plether.Ethereum.Rpc (RpcLog (..), TxReceipt (..), ethGetTransactionReceipt)
import Plether.Utils.Address (isValidAddress)
import Text.Read (readMaybe)

data FundingScope = FundingScope
  { fsChainId :: Integer
  , fsRouter :: Text
  , fsToken :: Text
  , fsClearinghouse :: Text
  } deriving stock (Show, Eq)

data FundingFlow = FundingFlow
  { ffAccount :: Text
  , ffKind :: Text
  , ffAmount :: Text
  , ffEmitter :: Text
  , ffAsset :: Text
  , ffTxHash :: Text
  , ffBlockNumber :: Integer
  , ffBlockHash :: Text
  , ffTxIndex :: Integer
  , ffLogIndex :: Integer
  , ffTransferCount :: Int64
  , ffAssisted :: Bool
  , ffVerifiedPayer :: Bool
  } deriving stock (Show, Eq)

instance FromRow FundingFlow where
  fromRow = FundingFlow <$> field <*> field <*> field <*> field <*> field <*> field
    <*> field <*> field <*> field <*> field <*> field <*> field <*> field

data DepositPayerProof = DepositPayerProof
  { dppPayer :: Text
  , dppTransferLogIndex :: Integer
  } deriving stock (Show, Eq)

-- A proof is reusable only for the same indexed credit and canonical block
-- identity. Reorgs leave old proofs unreachable; they cannot authorize a credit
-- at the replacement block hash. No signed transactions or RPC bodies are kept.
ensureDivisionsFundingSchema :: Connection -> IO ()
ensureDivisionsFundingSchema conn = void $ execute_ conn
  "CREATE TABLE IF NOT EXISTS divisions_deposit_payers ( \
  \chain_id BIGINT NOT NULL, release_router TEXT NOT NULL, tx_hash TEXT NOT NULL, \
  \block_number BIGINT NOT NULL, block_hash TEXT NOT NULL, tx_index BIGINT NOT NULL, \
  \deposit_log_index BIGINT NOT NULL, account TEXT NOT NULL, token_address TEXT NOT NULL, \
  \clearinghouse TEXT NOT NULL, amount_assets NUMERIC(78,0) NOT NULL CHECK (amount_assets>0), \
  \payer TEXT NOT NULL, transfer_log_index BIGINT NOT NULL, \
  \PRIMARY KEY (chain_id,release_router,tx_hash,block_hash,deposit_log_index))"

-- Every flow still needs its indexed custody transfer. For depositFor, a
-- receipt-bound proof supplies the payer and exact transfer log instead of
-- assuming the beneficiary paid. DepositFor is metadata, never a second flow.
fundingFlowsQuery :: Query
fundingFlowsQuery =
  "SELECT LOWER(a.account), a.activity_type, a.amount_assets::text, \
  \LOWER(COALESCE(a.contract_address,'')), LOWER(COALESCE(a.data->>'asset','')), \
  \a.tx_hash,a.block_number,a.block_hash,a.tx_index,a.log_index, \
  \(SELECT COUNT(*) FROM perps_asset_transfers x WHERE x.chain_id=a.chain_id \
  \AND x.release_router=a.release_router AND x.tx_hash=a.tx_hash \
  \AND x.block_number=a.block_number AND x.block_hash=a.block_hash AND x.tx_index=a.tx_index \
  \AND LOWER(x.token_address)=LOWER(?) AND x.amount=a.amount_assets \
  \AND LOWER(x.from_address)=CASE WHEN a.activity_type='Deposit' THEN COALESCE(p.payer,LOWER(a.account)) ELSE LOWER(?) END \
  \AND LOWER(x.to_address)=CASE WHEN a.activity_type='Deposit' THEN LOWER(?) ELSE LOWER(a.account) END \
  \AND (p.payer IS NULL OR x.log_index=p.transfer_log_index)), \
  \EXISTS (SELECT 1 FROM aa_close_assistance g WHERE g.verified \
  \AND g.chain_id=a.chain_id AND g.router=a.release_router AND g.account=a.account \
  \AND g.transaction_hash=a.tx_hash AND g.block_number=a.block_number \
  \AND g.block_hash=a.block_hash AND g.deposit_log_index=a.log_index \
  \AND g.amount_assets=a.amount_assets), p.payer IS NOT NULL \
  \FROM perps_account_activity a LEFT JOIN divisions_deposit_payers p \
  \ON a.activity_type='Deposit' AND p.chain_id=a.chain_id AND p.release_router=a.release_router \
  \AND p.tx_hash=a.tx_hash AND p.block_number=a.block_number AND p.block_hash=a.block_hash \
  \AND p.tx_index=a.tx_index AND p.deposit_log_index=a.log_index AND p.account=LOWER(a.account) \
  \AND p.token_address=LOWER(a.data->>'asset') AND p.clearinghouse=LOWER(a.contract_address) \
  \AND p.amount_assets=a.amount_assets \
  \WHERE a.chain_id=? AND a.release_router=? AND a.block_number>? AND a.block_number<=? \
  \AND a.activity_type IN ('Deposit','Withdraw') ORDER BY a.block_number,a.tx_index,a.log_index"

loadDivisionsFunding
  :: EthClient -> DbPool -> FundingScope -> Integer -> Integer
  -> IO (Maybe [(Text, Text, Integer, Bool)])
loadDivisionsFunding client pool scope baseline upper = do
  let readFlows = withDb pool $ \conn -> query conn fundingFlowsQuery
        (fsToken scope, fsClearinghouse scope, fsClearinghouse scope,
         fsChainId scope, fsRouter scope, baseline, upper)
  initial <- readFlows
  -- Only fetch missing payer evidence. A receipt can disambiguate equal
  -- depositFor amounts by their log positions. Verified ordinary flows,
  -- withdrawals and cached proofs need no additional RPC request.
  repaired <- forM initial $ \flow ->
    if validFlow scope flow && ffKind flow == "Deposit"
        && ffTransferCount flow /= 1 && not (ffVerifiedPayer flow)
      then do
        receipt <- ethGetTransactionReceipt client (ffTxHash flow)
        case receipt of
          Right (Just value) | Just proof <- decodeDepositPayer flow value -> do
            withDb pool $ \conn -> storeDepositPayerProof conn (fsChainId scope) (fsRouter scope) flow proof
            pure True
          _ -> pure False
      else pure False
  flows <- if or repaired then readFlows else pure initial
  pure $ traverse (parseFlow scope) flows

validFlow :: FundingScope -> FundingFlow -> Bool
validFlow scope FundingFlow {..} =
  maybe False (> 0) (readMaybe (T.unpack ffAmount) :: Maybe Integer)
    && ffEmitter == T.toLower (fsClearinghouse scope)
    && ffAsset == T.toLower (fsToken scope)
    && (ffKind == "Deposit" || ffKind == "Withdraw")
    && (not ffAssisted || ffKind == "Deposit")

parseFlow :: FundingScope -> FundingFlow -> Maybe (Text, Text, Integer, Bool)
parseFlow scope flow@FundingFlow {..} = do
  guard $ validFlow scope flow && ffTransferCount == 1
  amount <- readMaybe (T.unpack ffAmount)
  pure (ffAccount, ffKind, amount, ffAssisted)

-- The pinned clearinghouse emits Transfer, Deposit, DepositFor consecutively.
-- Bind by log position as well as beneficiary/amount so two equal deposits in
-- one transaction cannot reuse the same custody transfer or payer annotation.
decodeDepositPayer :: FundingFlow -> TxReceipt -> Maybe DepositPayerProof
decodeDepositPayer FundingFlow {..} receipt = do
  guard $ ffKind == "Deposit" && ffLogIndex > 0 && receiptSucceeded receipt
  guard $ all isValidAddress [ffAccount, ffEmitter, ffAsset]
  guard $ same (receiptTxHash receipt) ffTxHash
    && receiptBlockNumber receipt == ffBlockNumber
    && same (receiptBlockHash receipt) ffBlockHash
    && receiptTransactionIndex receipt == ffTxIndex
  amount <- readMaybe (T.unpack ffAmount)
  guard $ amount > 0 && amount < 2 ^ (256 :: Int)
  let logs = receiptLogs receipt
      indices = map rpcLogIndex logs
      identity logEntry = same (rpcLogTxHash logEntry) ffTxHash
        && rpcLogBlockNumber logEntry == ffBlockNumber
        && same (rpcLogBlockHash logEntry) ffBlockHash
        && rpcLogTransactionIndex logEntry == ffTxIndex
      one index = case filter ((== index) . rpcLogIndex) logs of
        [value] -> Just value
        _ -> Nothing
      matches logEntry emitter topics = same (rpcLogAddress logEntry) emitter
        && rpcLogTopics logEntry == topics && rpcLogData logEntry == encodeUint256 amount
      topic = keccak256 . TE.encodeUtf8
  guard $ length indices == length (nub indices) && all identity logs
  deposit <- one ffLogIndex
  guard $ matches deposit ffEmitter
    [topic "Deposit(address,address,uint256)", encodeAddress ffAccount, encodeAddress ffAsset]
  metadata <- one (ffLogIndex + 1)
  payerWord <- case rpcLogTopics metadata of
    [signature, payer, beneficiary]
      | signature == topic "DepositFor(address,address,uint256)"
      , beneficiary == encodeAddress ffAccount
      , BS.length payer == 32, BS.take 12 payer == BS.replicate 12 0 -> Just payer
    _ -> Nothing
  guard $ same (rpcLogAddress metadata) ffEmitter && rpcLogData metadata == encodeUint256 amount
    && payerWord /= BS.replicate 32 0
  transfer <- one (ffLogIndex - 1)
  guard $ matches transfer ffAsset
    [topic "Transfer(address,address,uint256)", payerWord, encodeAddress ffEmitter]
  let payer = "0x" <> T.takeEnd 40 (TE.decodeUtf8 $ B16.encode payerWord)
  pure $ DepositPayerProof payer (ffLogIndex - 1)
  where
    same a b = T.toLower a == T.toLower b

storeDepositPayerProof :: Connection -> Integer -> Text -> FundingFlow -> DepositPayerProof -> IO ()
storeDepositPayerProof conn chain router FundingFlow {..} DepositPayerProof {..} = void $ execute conn
  "INSERT INTO divisions_deposit_payers \
  \(chain_id,release_router,tx_hash,block_number,block_hash,tx_index,deposit_log_index, \
  \account,token_address,clearinghouse,amount_assets,payer,transfer_log_index) \
  \VALUES (?,?,?,?,?,?,?,?,?,?,?::numeric,?,?) ON CONFLICT DO NOTHING"
  [ toField chain, toField router, toField (T.toLower ffTxHash), toField ffBlockNumber
  , toField (T.toLower ffBlockHash), toField ffTxIndex, toField ffLogIndex
  , toField ffAccount, toField ffAsset, toField ffEmitter, toField ffAmount
  , toField dppPayer, toField dppTransferLogIndex
  ]
