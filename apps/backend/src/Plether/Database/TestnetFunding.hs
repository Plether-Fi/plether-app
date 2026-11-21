{-# LANGUAGE TemplateHaskell #-}
module Plether.Database.TestnetFunding
  ( FundingClaim (..), FundingTransaction (..), ensureMarginFundingSchema, requireKernelMarginFundingSchema
  , KernelFundingReservationFailure (..)
  , reserveMarginFunding, reserveKernelMarginFunding, getMarginFunding, getMarginFundingProgram, nextMarginFunding
  , nextMarginFundingForProgram
  , submittedFundingTransaction, hasSubmittedFundingTransaction
  , persistFundingTransaction, finishFundingTransaction, failMarginFunding
  ) where

import Control.Monad (unless, void, when)
import Data.Int (Int64)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
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

data KernelFundingReservationFailure
  = KernelFundingBindingChanged
  | KernelFundingAllocationExists
  deriving stock (Eq, Show)

schema :: Query
schema = fromString $(do
  let path = "config/migrations/testnet-margin-funding-v1.sql"
  addDependentFile path
  runIO (readFile path) >>= lift)

ensureMarginFundingSchema :: Connection -> IO ()
ensureMarginFundingSchema conn = void $ execute_ conn schema

-- The isolated API must not run writes to the shared funding schema during
-- startup. Verify the rolled-out column and allocation constraint instead.
requireKernelMarginFundingSchema :: Connection -> IO ()
requireKernelMarginFundingSchema conn = do
  [Only ready] <- query_ conn
    "SELECT EXISTS (SELECT 1 FROM pg_attribute\
    \ WHERE attrelid=to_regclass('testnet_margin_funding') AND attname='kernel_program'\
    \ AND atttypid='text'::regtype AND NOT attisdropped AND NOT attnotnull)\
    \ AND EXISTS (SELECT 1 FROM pg_index\
    \ WHERE indrelid=to_regclass('testnet_margin_funding')\
    \ AND indexrelid=to_regclass('testnet_margin_funding_kernel_owner_program')\
    \ AND indisunique AND indisvalid AND indisready AND indislive AND indnatts=3 AND indnkeyatts=3\
    \ AND pg_get_indexdef(indexrelid,1,true)='chain_id'\
    \ AND pg_get_indexdef(indexrelid,2,true)='kernel_program'\
    \ AND regexp_replace(pg_get_indexdef(indexrelid,3,true),'[[:space:]()]','','g')\
    \ IN ('lowerowner_address::text','lowerowner_address')\
    \ AND pg_get_expr(indpred,indrelid)='(kernel_program IS NOT NULL)')"
  unless ready $ fail "Kernel funding schema is missing or incompatible. Apply the reviewed margin funding migration before starting the canary."

-- Callers hold the shared faucet signer lock. The legacy allocation and new
-- claim are committed together; an old binary sees an unknown, non-retryable
-- marker, rather than a failed wallet mint that could grant another allocation.
reserveMarginFunding :: Connection -> FundingClaim -> IO (Maybe FundingClaim)
reserveMarginFunding conn claim = withTransaction conn $ do
  allocated <- reserveEligibility conn claim
  when allocated $ do
    void $ execute conn
      "INSERT INTO testnet_margin_funding(owner_address,address,token_address,chain_id,clearinghouse,signer,release_id,amount) VALUES (?,?,?,?,?,?,?,?)"
      (fcOwner claim,fcAddress claim,fcToken claim,fcChain claim,fcClearinghouse claim,fcSigner claim,fcRelease claim,fcAmount claim)
  rows <- query conn
    "SELECT id::text FROM testnet_margin_funding WHERE address=? AND token_address=?"
    (fcAddress claim,fcToken claim) :: IO [Only Text]
  case rows of
    [Only identifier] -> retryMarginFunding conn identifier
    _ -> pure Nothing

-- The address eligibility marker keeps wallet minting exclusive with direct
-- margin funding. The program-and-owner index also prevents two simultaneous
-- reservations for the same owner from allocating to different addresses.
reserveKernelMarginFunding :: Connection -> Text -> FundingClaim -> IO (Either KernelFundingReservationFailure FundingClaim)
reserveKernelMarginFunding conn program claim = withTransaction conn $ do
  allocated <- reserveEligibility conn claim
  let owner = T.toLower $ fcOwner claim
  when allocated $ do
    inserted <- execute conn
      "INSERT INTO testnet_margin_funding(owner_address,address,token_address,chain_id,clearinghouse,signer,release_id,amount,kernel_program) VALUES (?,?,?,?,?,?,?,?,?) ON CONFLICT DO NOTHING"
      (owner,fcAddress claim,fcToken claim,fcChain claim,fcClearinghouse claim,fcSigner claim,fcRelease claim,fcAmount claim,program)
    -- This transaction owns a new marker only when reserveEligibility succeeds.
    -- Remove it if the allocation lost a uniqueness race.
    when (inserted == 0) $ void $ execute conn
      "DELETE FROM testnet_faucet_claims WHERE address=? AND token_address=?"
      (fcAddress claim,fcToken claim)
  rows <- query conn (claimSelect <> "WHERE chain_id=? AND kernel_program=? AND lower(owner_address)=?")
    (fcChain claim, program, owner)
  case rows of
    [existing] | sameBinding existing -> do
      resumed <- retryMarginFunding conn $ fcId existing
      pure $ maybe (Left KernelFundingAllocationExists) Right resumed
    [_] -> pure $ Left KernelFundingBindingChanged
    _ -> pure $ Left KernelFundingAllocationExists
  where
    sameBinding existing =
      fcAddress existing == fcAddress claim && fcToken existing == fcToken claim
      && fcClearinghouse existing == fcClearinghouse claim && fcSigner existing == fcSigner claim
      && fcRelease existing == fcRelease claim && fcAmount existing == fcAmount claim

reserveEligibility :: Connection -> FundingClaim -> IO Bool
reserveEligibility conn claim = (== 1) <$> execute conn
  "INSERT INTO testnet_faucet_claims(address,token_address,amount,status,delivery_mode) VALUES (?,?,?,'margin_reserved','margin') ON CONFLICT DO NOTHING"
  (fcAddress claim, fcToken claim, fcAmount claim)

retryMarginFunding :: Connection -> Text -> IO (Maybe FundingClaim)
retryMarginFunding conn identifier = do
  -- Only explicit user retries release a known failure; mint is never repeated
  -- after success, and deposit retries refresh the exact allowance.
  void $ execute conn
    "UPDATE testnet_margin_funding SET status='pending',reason=NULL,phase=CASE WHEN phase='deposit' THEN 'approval' ELSE phase END,updated_at=NOW() WHERE id=?::uuid AND status='failed'"
    (Only identifier)
  getMarginFunding conn identifier

claimSelect :: Query
claimSelect = "SELECT id::text,owner_address,address,token_address,chain_id,clearinghouse,signer,release_id,amount,phase,status,reason,deposit_tx_hash,deposit_block_number FROM testnet_margin_funding "

getMarginFunding :: Connection -> Text -> IO (Maybe FundingClaim)
getMarginFunding conn identifier = first <$> query conn (claimSelect <> "WHERE id=?::uuid") (Only identifier)

getMarginFundingProgram :: Connection -> Text -> IO (Maybe Text)
getMarginFundingProgram conn identifier = do
  rows <- query conn "SELECT kernel_program FROM testnet_margin_funding WHERE id=?::uuid" (Only identifier)
  pure $ fromOnly =<< first rows

nextMarginFunding :: Connection -> IO (Maybe FundingClaim)
nextMarginFunding conn = first <$> query_ conn (claimSelect <> "WHERE status='pending' ORDER BY created_at,id LIMIT 1")

nextMarginFundingForProgram :: Connection -> Maybe Text -> IO (Maybe FundingClaim)
nextMarginFundingForProgram conn program = first <$> query conn
  (claimSelect <> "WHERE status='pending' AND kernel_program IS NOT DISTINCT FROM ? \
    \AND (reason IS DISTINCT FROM 'TRADING_ACCOUNT_VERIFICATION_UNAVAILABLE' OR updated_at <= NOW() - INTERVAL '15 seconds') \
    \ORDER BY created_at,id LIMIT 1")
  (Only program)

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
