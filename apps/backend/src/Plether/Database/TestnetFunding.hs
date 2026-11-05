{-# LANGUAGE TemplateHaskell #-}
module Plether.Database.TestnetFunding
  ( FundingClaim (..), FundingTransaction (..), ensureMarginFundingSchema
  , reserveMarginFunding, getMarginFunding, nextMarginFunding
  , submittedFundingTransaction, hasSubmittedFundingTransaction
  , persistFundingTransaction, finishFundingTransaction, failMarginFunding
  ) where

import Control.Monad (void)
import Data.Int (Int64)
import Data.String (fromString)
import Data.Text (Text)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Language.Haskell.TH (runIO)
import Language.Haskell.TH.Syntax (addDependentFile, lift)

-- No Show instances: transaction rows include signed payloads.
data FundingClaim = FundingClaim
  { fcId :: Text, fcOwner :: Text, fcAddress :: Text, fcToken :: Text
  , fcChain :: Integer, fcClearinghouse :: Text, fcSigner :: Text, fcRelease :: Text
  , fcAmount :: Integer, fcPhase :: Text, fcStatus :: Text, fcReason :: Maybe Text
  , fcDepositHash :: Maybe Text, fcDepositBlock :: Maybe Integer
  }
instance FromRow FundingClaim where
  fromRow = FundingClaim <$> field <*> field <*> field <*> field <*> field <*> field <*> field
    <*> field <*> field <*> field <*> field <*> field <*> field <*> field

data FundingTransaction = FundingTransaction
  { ftId :: Int64, ftClaim :: Text, ftPhase :: Text, ftSigner :: Text, ftNonce :: Integer
  , ftTarget :: Text, ftCalldata :: Text, ftHash :: Text, ftRaw :: Text
  }
instance FromRow FundingTransaction where
  fromRow = FundingTransaction <$> field <*> field <*> field <*> field <*> field
    <*> field <*> field <*> field <*> field

schema :: Query
schema = fromString $(do
  let path = "config/migrations/testnet-margin-funding-v1.sql"
  addDependentFile path
  runIO (readFile path) >>= lift)

ensureMarginFundingSchema :: Connection -> IO ()
ensureMarginFundingSchema conn = void $ execute_ conn schema

-- Callers hold the shared faucet signer lock. The legacy allocation and new
-- claim are committed together; an old binary sees an unknown, non-retryable
-- marker, rather than a failed wallet mint that could grant another allocation.
reserveMarginFunding :: Connection -> FundingClaim -> IO (Maybe FundingClaim)
reserveMarginFunding conn claim = withTransaction conn $ do
  allocated <- execute conn
    "INSERT INTO testnet_faucet_claims(address,token_address,amount,status,delivery_mode) VALUES (?,?,?,'margin_reserved','margin') ON CONFLICT DO NOTHING"
    (fcAddress claim, fcToken claim, fcAmount claim)
  if allocated == 1 then do
    void $ execute conn
      "INSERT INTO testnet_margin_funding(owner_address,address,token_address,chain_id,clearinghouse,signer,release_id,amount) VALUES (?,?,?,?,?,?,?,?)"
      (fcOwner claim,fcAddress claim,fcToken claim,fcChain claim,fcClearinghouse claim,fcSigner claim,fcRelease claim,fcAmount claim)
    else pure ()
  rows <- query conn
    "SELECT id::text FROM testnet_margin_funding WHERE address=? AND token_address=?"
    (fcAddress claim,fcToken claim) :: IO [Only Text]
  case rows of
    [Only identifier] -> do
      -- Only explicit user retries release a known failure; mint is never
      -- repeated after success, and deposit retries refresh exact allowance.
      void $ execute conn
        "UPDATE testnet_margin_funding SET status='pending',reason=NULL,phase=CASE WHEN phase='deposit' THEN 'approval' ELSE phase END,updated_at=NOW() WHERE id=?::uuid AND status='failed'"
        (Only identifier)
      getMarginFunding conn identifier
    _ -> pure Nothing

claimSelect :: Query
claimSelect = "SELECT id::text,owner_address,address,token_address,chain_id,clearinghouse,signer,release_id,amount,phase,status,reason,deposit_tx_hash,deposit_block_number FROM testnet_margin_funding "

getMarginFunding :: Connection -> Text -> IO (Maybe FundingClaim)
getMarginFunding conn identifier = first <$> query conn (claimSelect <> "WHERE id=?::uuid") (Only identifier)

nextMarginFunding :: Connection -> IO (Maybe FundingClaim)
nextMarginFunding conn = first <$> query_ conn (claimSelect <> "WHERE status='pending' ORDER BY created_at,id LIMIT 1")

submittedFundingTransaction :: Connection -> IO (Maybe FundingTransaction)
submittedFundingTransaction conn = first <$> query_ conn
  "SELECT id,claim_id::text,phase,signer,nonce,target,calldata,tx_hash,raw_tx FROM testnet_margin_funding_transactions WHERE status='submitted' ORDER BY id LIMIT 1"

hasSubmittedFundingTransaction :: Connection -> IO Bool
hasSubmittedFundingTransaction conn = maybe False (const True) <$> submittedFundingTransaction conn

persistFundingTransaction :: Connection -> FundingClaim -> Integer -> Text -> Text -> Text -> Text -> IO ()
persistFundingTransaction conn claim nonce target calldata txHash raw = void $ execute conn
  "INSERT INTO testnet_margin_funding_transactions(claim_id,phase,attempt,signer,nonce,target,calldata,tx_hash,raw_tx) SELECT ?::uuid,?,COALESCE(MAX(attempt),0)+1,?,?,?,?,?,? FROM testnet_margin_funding_transactions WHERE claim_id=?::uuid AND phase=?"
  (fcId claim,fcPhase claim,fcSigner claim,nonce,target,calldata,txHash,raw,fcId claim,fcPhase claim)

finishFundingTransaction :: Connection -> FundingClaim -> FundingTransaction -> Bool -> Integer -> Text -> IO ()
finishFundingTransaction conn claim tx succeeded block blockHash = withTransaction conn $ do
  changed <- execute conn
    "UPDATE testnet_margin_funding_transactions SET status=?,block_number=?,block_hash=? WHERE id=? AND claim_id=?::uuid AND phase=? AND tx_hash=? AND status='submitted'"
    (if succeeded then ("confirmed" :: Text) else "reverted",block,blockHash,ftId tx,fcId claim,fcPhase claim,ftHash tx)
  if changed /= 1 then fail "Funding transaction changed during finalization" else pure ()
  if not succeeded then failMarginFunding conn claim "failed" "TRANSACTION_REVERTED"
  else if ftPhase tx == "deposit" then do
    void $ execute conn
      "UPDATE testnet_margin_funding SET status='funded',reason=NULL,deposit_tx_hash=?,deposit_block_number=?,updated_at=NOW() WHERE id=?::uuid"
      (ftHash tx,block,fcId claim)
    void $ execute conn
      "UPDATE testnet_faucet_claims SET status='margin_funded',updated_at=NOW() WHERE address=? AND token_address=? AND delivery_mode='margin'"
      (fcAddress claim,fcToken claim)
  else void $ execute conn
    "UPDATE testnet_margin_funding SET phase=?,status='pending',reason=NULL,updated_at=NOW() WHERE id=?::uuid"
    (if ftPhase tx == "mint" then ("approval" :: Text) else "deposit",fcId claim)

failMarginFunding :: Connection -> FundingClaim -> Text -> Text -> IO ()
failMarginFunding conn claim status reason = void $ execute conn
  "UPDATE testnet_margin_funding SET status=?,reason=?,updated_at=NOW() WHERE id=?::uuid AND status <> 'funded'"
  (status,reason,fcId claim)

first :: [a] -> Maybe a
first [] = Nothing
first (value:_) = Just value
