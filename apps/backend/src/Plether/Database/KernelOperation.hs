module Plether.Database.KernelOperation
  ( KernelScope (..)
  , PaymentMode (..)
  , KernelPreparation (..)
  , StorePreparationResult (..)
  , AcceptOperationResult (..)
  , KernelOperation (..)
  , KernelInclusion (..)
  , KernelRetirement (..)
  , KernelCancellation (..)
  , KernelSponsorBudget (..)
  , KernelSponsorshipDraft (..)
  , KernelSponsorship (..)
  , ensureKernelOperationSchema
  , storeKernelPreparation
  , getKernelPreparationForClient
  , getKernelNonceScanCursor
  , storeKernelNonceScanCursor
  , acceptKernelOperation
  , acceptKernelEthOperationAtNonce
  , acceptKernelCancellationAnchor
  , getKernelOperationForClient
  , finalizeKernelOperation
  , retireKernelOperation
  , storeKernelCancellation
  , getKernelCancellation
  , getAcceptedKernelCancellation
  , acceptKernelCancellation
  , finalizeKernelCancellation
  , lockKernelSponsorScope
  , kernelSponsorshipReady
  , kernelSponsorshipStateIsEmpty
  , reserveKernelSponsorship
  , getKernelSponsorshipForClient
  , storeKernelSponsorshipSignature
  , settleKernelSponsorship
  , expireKernelSponsorshipsThrough
  ) where

import Control.Exception (Exception, throwIO, try)
import Control.Monad (void)
import Data.Aeson (Value (..), encode, object, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Pair)
import Data.Bits (shiftR)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple (Connection, Only (..), execute, execute_, query, query_, withTransaction)
import Database.PostgreSQL.Simple.FromField (ResultError (ConversionFailed), returnError)
import Database.PostgreSQL.Simple.FromRow (FromRow (..), RowParser, field, fieldWith)
import Database.PostgreSQL.Simple.ToField (Action, toField)
import Database.PostgreSQL.Simple.Transaction (withSavepoint)
import Database.PostgreSQL.Simple.Types (Query)
import Numeric (showHex)
import Text.Read (readMaybe)

data KernelScope = KernelScope
  { ksDeployment :: Text
  , ksChainId :: Integer
  , ksEntryPoint :: Text
  } deriving stock (Eq, Show)

instance FromRow KernelScope where
  fromRow = KernelScope <$> field <*> field <*> field

data PaymentMode = Sponsored | PayWithEth
  deriving stock (Eq, Show)

modeText :: PaymentMode -> Text
modeText Sponsored = "sponsored"
modeText PayWithEth = "eth"

modeField :: RowParser PaymentMode
modeField = fieldWith $ \column raw -> case raw of
  Just "sponsored" -> pure Sponsored
  Just "eth" -> pure PayWithEth
  _ -> returnError ConversionFailed column "Invalid Kernel payment mode"

-- The gateway validates and canonicalizes calldata, quantities and addresses.
-- Operation payloads and client credentials intentionally have no Show instance.
data KernelPreparation = KernelPreparation
  { kpScope :: KernelScope
  , kpSender :: Text
  , kpOwner :: Text
  , kpNonce :: Integer
  , kpId :: Text
  , kpClientKey :: Text
  , kpMode :: PaymentMode
  , kpUnsignedOperation :: Value
  , kpExpiresAt :: UTCTime
  -- Older rows have no anchor. Retain the original anchor across retries.
  , kpSafeBlock :: Maybe Integer
  } deriving stock (Eq)

instance FromRow KernelPreparation where
  fromRow = KernelPreparation <$> fromRow <*> field <*> field <*> integerField
    <*> field <*> field <*> modeField <*> field <*> field <*> field

data StorePreparationResult = PreparationStored | PreparationAlreadyStored | PreparationConflict
  deriving stock (Eq, Show)

data AcceptOperationResult
  = OperationAccepted
  | OperationReplay
  | OperationPreparationMissing
  | OperationPreparationExpired
  | OperationPreparationMismatch
  | OperationConflict
  | OperationLaneBusy
  | OperationAlreadyFinal
  deriving stock (Eq, Show)

data KernelOperation = KernelOperation
  { koPreparation :: KernelPreparation
  , koHash :: Text
  , koSignedOperation :: Value
  , koState :: Text
  , koTerminalEvidence :: Maybe Value
  , koCancellationOnly :: Bool
  }

instance FromRow KernelOperation where
  fromRow = KernelOperation <$> fromRow <*> field <*> field <*> field <*> field <*> field

-- Evidence is constructed only by chain-verifying callers. A bundled revert
-- still consumes the nonce and settles actual gas cost.
data KernelInclusion = KernelInclusion
  { kiSender :: Text
  , kiNonce :: Integer
  , kiPaymaster :: Maybe Text
  , kiTransactionHash :: Text
  , kiBlockHash :: Text
  , kiBlockNumber :: Integer
  , kiExecutionSuccess :: Bool
  , kiActualGasCost :: Integer
  } deriving stock (Eq, Show)

data KernelRetirement = KernelNonceConsumed
  { krSender :: Text
  , krNonce :: Integer
  , krObservedNonce :: Integer
  , krBlockNumber :: Integer
  , krBlockHash :: Text
  } deriving stock (Eq, Show)

data KernelCancellation = KernelCancellation
  { kcPreparationId :: Text
  , kcUnsignedOperation :: Value
  , kcExpiresAt :: UTCTime
  , kcSignedOperation :: Maybe Value
  , kcHash :: Maybe Text
  } deriving stock (Eq)

instance FromRow KernelCancellation where
  fromRow = KernelCancellation <$> field <*> field <*> field <*> field <*> field

data KernelSponsorBudget = KernelSponsorBudget
  { ksbMaxCostWei :: Integer
  , ksbAccountOutstandingWei :: Integer
  , ksbClientOutstandingWei :: Integer
  , ksbGlobalOutstandingWei :: Integer
  , ksbAccountHourlyWei :: Integer
  , ksbGlobalHourlyWei :: Integer
  , ksbGlobalDailyWei :: Integer
  } deriving stock (Eq, Show)

data KernelSponsorshipDraft = KernelSponsorshipDraft
  { ksdScope :: KernelScope
  , ksdPaymaster :: Text
  , ksdId :: Text
  , ksdPreparationId :: Text
  , ksdClientKey :: Text
  , ksdSender :: Text
  , ksdNonce :: Integer
  , ksdMaxCostWei :: Integer
  , ksdValidAfter :: Integer
  , ksdValidUntil :: Integer
  , ksdUnsignedOperation :: Value
  } deriving stock (Eq)

instance FromRow KernelSponsorshipDraft where
  fromRow = KernelSponsorshipDraft <$> fromRow <*> field <*> field <*> field <*> field
    <*> field <*> integerField <*> integerField <*> field <*> field <*> field

data KernelSponsorship = KernelSponsorship
  { kspDraft :: KernelSponsorshipDraft
  , kspSignature :: Maybe Text
  , kspUserOperationHash :: Maybe Text
  , kspStatus :: Text
  }

instance FromRow KernelSponsorship where
  fromRow = KernelSponsorship <$> fromRow <*> field <*> field <*> field

ensureKernelOperationSchema :: Connection -> IO ()
ensureKernelOperationSchema conn = withTransaction conn $ do
  -- Keep Kernel state separate from the deployed SimpleAccount ledger, including
  -- for profile startup against a shared PostgreSQL instance.
  void (query_ conn "SELECT 1::integer FROM (SELECT pg_advisory_xact_lock(621971928835702785)) AS kernel_schema" :: IO [Only Int])
  void $ execute_ conn
    "CREATE TABLE IF NOT EXISTS aa_kernel_preparations (\
    \ deployment TEXT NOT NULL CHECK (deployment <> ''), chain_id BIGINT NOT NULL CHECK (chain_id > 0),\
    \ entry_point TEXT NOT NULL CHECK (entry_point ~ '^0x[0-9a-f]{40}$'),\
    \ sender TEXT NOT NULL CHECK (sender ~ '^0x[0-9a-f]{40}$'),\
    \ owner_address TEXT NOT NULL CHECK (owner_address ~ '^0x[0-9a-f]{40}$'),\
    \ nonce NUMERIC(78,0) NOT NULL CHECK (nonce >= 0), preparation_id TEXT NOT NULL CHECK (preparation_id <> ''),\
    \ client_key TEXT NOT NULL CHECK (client_key <> ''), mode TEXT NOT NULL CHECK (mode IN ('sponsored','eth')),\
    \ unsigned_operation JSONB NOT NULL CHECK (jsonb_typeof(unsigned_operation)='object'),\
    \ expires_at TIMESTAMPTZ NOT NULL, created_at TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp(),\
    \ PRIMARY KEY (deployment,chain_id,entry_point,preparation_id),\
    \ UNIQUE (deployment,chain_id,entry_point,preparation_id,sender,nonce,client_key,mode))"
  void $ execute_ conn
    "ALTER TABLE aa_kernel_preparations ADD COLUMN IF NOT EXISTS prepared_safe_block BIGINT\
    \ CHECK (prepared_safe_block >= 0)"
  void $ execute_ conn
    "ALTER TABLE aa_kernel_preparations\
    \ ADD COLUMN IF NOT EXISTS nonce_scan_through_block BIGINT,\
    \ ADD COLUMN IF NOT EXISTS nonce_scan_through_hash TEXT CHECK (\
    \   (nonce_scan_through_block IS NULL AND nonce_scan_through_hash IS NULL) OR (\
    \   prepared_safe_block IS NOT NULL AND nonce_scan_through_block IS NOT NULL\
    \   AND nonce_scan_through_block >= prepared_safe_block AND nonce_scan_through_hash IS NOT NULL\
    \   AND nonce_scan_through_hash ~ '^0x[0-9a-f]{64}$'))"
  void $ execute_ conn
    "CREATE TABLE IF NOT EXISTS aa_kernel_operations (\
    \ deployment TEXT NOT NULL, chain_id BIGINT NOT NULL, entry_point TEXT NOT NULL,\
    \ sender TEXT NOT NULL, nonce NUMERIC(78,0) NOT NULL, preparation_id TEXT NOT NULL, client_key TEXT NOT NULL,\
    \ mode TEXT NOT NULL, user_operation_hash TEXT NOT NULL CHECK (user_operation_hash ~ '^0x[0-9a-f]{64}$'),\
    \ signed_operation JSONB NOT NULL CHECK (jsonb_typeof(signed_operation)='object'),\
    \ status TEXT NOT NULL DEFAULT 'pending' CHECK (status IN ('pending','included','nonce_consumed','sponsorship_expired')),\
    \ terminal_evidence JSONB, created_at TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp(),\
    \ PRIMARY KEY (deployment,chain_id,entry_point,user_operation_hash),\
    \ UNIQUE (deployment,chain_id,entry_point,preparation_id),\
    \ CHECK ((status='pending')=(terminal_evidence IS NULL)),\
    \ FOREIGN KEY (deployment,chain_id,entry_point,preparation_id,sender,nonce,client_key,mode)\
    \ REFERENCES aa_kernel_preparations (deployment,chain_id,entry_point,preparation_id,sender,nonce,client_key,mode))"
  void $ execute_ conn
    "ALTER TABLE aa_kernel_operations ADD COLUMN IF NOT EXISTS cancellation_only BOOLEAN NOT NULL DEFAULT FALSE\
    \ CHECK (NOT cancellation_only OR mode='eth')"
  void $ execute_ conn
    "CREATE UNIQUE INDEX IF NOT EXISTS aa_kernel_pending_nonce\
    \ ON aa_kernel_operations (deployment,chain_id,entry_point,sender,nonce) WHERE status='pending'"
  void $ execute_ conn "DROP INDEX IF EXISTS aa_kernel_pending_account"
  void $ execute_ conn
    "CREATE TABLE IF NOT EXISTS aa_kernel_cancellations (\
    \ deployment TEXT NOT NULL, chain_id BIGINT NOT NULL, entry_point TEXT NOT NULL,\
    \ original_hash TEXT NOT NULL, cancellation_preparation_id TEXT NOT NULL CHECK (cancellation_preparation_id <> ''),\
    \ unsigned_operation JSONB NOT NULL CHECK (jsonb_typeof(unsigned_operation)='object'),\
    \ expires_at TIMESTAMPTZ NOT NULL, created_at TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp(),\
    \ signed_operation JSONB CHECK (jsonb_typeof(signed_operation)='object'),\
    \ cancellation_hash TEXT CHECK (cancellation_hash ~ '^0x[0-9a-f]{64}$' AND cancellation_hash <> original_hash),\
    \ PRIMARY KEY (deployment,chain_id,entry_point,cancellation_preparation_id),\
    \ CHECK ((signed_operation IS NULL)=(cancellation_hash IS NULL)),\
    \ FOREIGN KEY (deployment,chain_id,entry_point,original_hash)\
    \ REFERENCES aa_kernel_operations (deployment,chain_id,entry_point,user_operation_hash))"
  void $ execute_ conn
    "CREATE UNIQUE INDEX IF NOT EXISTS aa_kernel_accepted_cancellation\
    \ ON aa_kernel_cancellations (deployment,chain_id,entry_point,original_hash) WHERE cancellation_hash IS NOT NULL"
  void $ execute_ conn
    "CREATE TABLE IF NOT EXISTS aa_kernel_sponsorships (\
    \ deployment TEXT NOT NULL CHECK (deployment <> ''), chain_id BIGINT NOT NULL CHECK (chain_id > 0),\
    \ entry_point TEXT NOT NULL CHECK (entry_point ~ '^0x[0-9a-f]{40}$'),\
    \ paymaster TEXT NOT NULL CHECK (paymaster ~ '^0x[0-9a-f]{40}$'),\
    \ authorization_id TEXT NOT NULL CHECK (authorization_id <> ''), preparation_id TEXT NOT NULL CHECK (preparation_id <> ''),\
    \ client_key TEXT NOT NULL CHECK (client_key <> ''), sender TEXT NOT NULL CHECK (sender ~ '^0x[0-9a-f]{40}$'),\
    \ nonce NUMERIC(78,0) NOT NULL CHECK (nonce >= 0), max_cost_wei NUMERIC(78,0) NOT NULL CHECK (max_cost_wei > 0),\
    \ valid_after BIGINT NOT NULL CHECK (valid_after >= 0), valid_until BIGINT NOT NULL CHECK (valid_until > valid_after),\
    \ unsigned_operation JSONB NOT NULL CHECK (jsonb_typeof(unsigned_operation)='object'),\
    \ signature TEXT, user_operation_hash TEXT CHECK (user_operation_hash ~ '^0x[0-9a-f]{64}$'),\
    \ status TEXT NOT NULL DEFAULT 'reserved' CHECK (status IN ('reserved','signed','settled','expired')),\
    \ actual_gas_cost NUMERIC(78,0), terminal_evidence JSONB, settled_at TIMESTAMPTZ,\
    \ created_at TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp(),\
    \ PRIMARY KEY (deployment,chain_id,entry_point,paymaster,authorization_id),\
    \ UNIQUE (deployment,chain_id,entry_point,paymaster,preparation_id),\
    \ UNIQUE (deployment,chain_id,entry_point,paymaster,user_operation_hash),\
    \ CHECK ((signature IS NULL)=(user_operation_hash IS NULL)),\
    \ CHECK (status NOT IN ('signed','settled') OR signature IS NOT NULL),\
    \ CHECK ((status='settled')=(terminal_evidence IS NOT NULL) AND (status='settled')=(settled_at IS NOT NULL)),\
    \ CHECK ((status='settled')=(actual_gas_cost IS NOT NULL)),\
    \ CHECK (actual_gas_cost >= 0 AND actual_gas_cost <= max_cost_wei))"
  void $ execute_ conn
    "CREATE INDEX IF NOT EXISTS aa_kernel_sponsorship_active ON aa_kernel_sponsorships\
    \ (deployment,chain_id,entry_point,paymaster,valid_until) WHERE status IN ('reserved','signed')"
  void $ execute_ conn
    "CREATE INDEX IF NOT EXISTS aa_kernel_sponsorship_settled ON aa_kernel_sponsorships\
    \ (deployment,chain_id,entry_point,paymaster,settled_at) WHERE status='settled'"

storeKernelPreparation :: Connection -> KernelPreparation -> IO StorePreparationResult
storeKernelPreparation conn prepared
  | not (matchesPaymentMode prepared) = pure PreparationConflict
  | otherwise = withTransaction conn $ do
      inserted <- execute conn
        "INSERT INTO aa_kernel_preparations\
        \ (deployment,chain_id,entry_point,sender,owner_address,nonce,preparation_id,client_key,mode,unsigned_operation,expires_at,prepared_safe_block)\
        \ VALUES (?,?,?,?,?,?,?,?,?,?::jsonb,?,?) ON CONFLICT DO NOTHING"
        (scopeParams (kpScope prepared) ++
          [ toField (kpSender prepared), toField (kpOwner prepared), toField (kpNonce prepared)
          , toField (kpId prepared), toField (kpClientKey prepared), toField (modeText $ kpMode prepared)
          , toField (jsonText $ kpUnsignedOperation prepared), toField (kpExpiresAt prepared), toField (kpSafeBlock prepared)
          ])
      if inserted == 1 then pure PreparationStored else do
        prior <- getKernelPreparationForClient conn (kpScope prepared) (kpId prepared) (kpClientKey prepared)
        pure $ case prior of
          Just stored | stored == prepared {kpExpiresAt = kpExpiresAt stored, kpSafeBlock = kpSafeBlock stored} ->
            PreparationAlreadyStored
          _ -> PreparationConflict

getKernelPreparationForClient :: Connection -> KernelScope -> Text -> Text -> IO (Maybe KernelPreparation)
getKernelPreparationForClient conn scope preparationId client = oneRow =<< query conn
  "SELECT deployment,chain_id,entry_point,sender,owner_address,nonce,preparation_id,client_key,mode,unsigned_operation,expires_at,prepared_safe_block\
  \ FROM aa_kernel_preparations\
  \ WHERE deployment=? AND chain_id=? AND entry_point=? AND preparation_id=? AND client_key=?"
  (scopeParams scope ++ [toField preparationId, toField client])

-- This progress belongs to the original preparation. Compare-and-swap lets
-- concurrent status requests advance it without overwriting a verified scan.
getKernelNonceScanCursor :: Connection -> KernelPreparation -> IO (Maybe (Integer, Text))
getKernelNonceScanCursor conn prepared = oneRow =<< query conn
  "SELECT nonce_scan_through_block,nonce_scan_through_hash FROM aa_kernel_preparations\
  \ WHERE deployment=? AND chain_id=? AND entry_point=? AND preparation_id=? AND client_key=?\
  \ AND prepared_safe_block=? AND nonce_scan_through_block IS NOT NULL"
  (scopeParams (kpScope prepared) ++
    [toField (kpId prepared), toField (kpClientKey prepared), toField (kpSafeBlock prepared)])

storeKernelNonceScanCursor
  :: Connection -> KernelPreparation -> Maybe (Integer, Text) -> (Integer, Text) -> IO Bool
storeKernelNonceScanCursor conn prepared expected (block, blockHash) = do
  updated <- execute conn
    "UPDATE aa_kernel_preparations SET nonce_scan_through_block=?,nonce_scan_through_hash=?\
    \ WHERE deployment=? AND chain_id=? AND entry_point=? AND preparation_id=? AND client_key=?\
    \ AND prepared_safe_block=? AND prepared_safe_block<=?\
    \ AND nonce_scan_through_block IS NOT DISTINCT FROM ? AND nonce_scan_through_hash IS NOT DISTINCT FROM ?"
    ([toField block, toField blockHash] ++ scopeParams (kpScope prepared) ++
      [toField (kpId prepared), toField (kpClientKey prepared), toField (kpSafeBlock prepared), toField block,
       toField (fst <$> expected), toField (snd <$> expected)])
  pure $ updated == 1

-- Commit acceptance before handing signed bytes to Alto. The durable lock is
-- shared by both modes and survives processes, tabs, and relay failures.
acceptKernelOperation :: Connection -> KernelScope -> Text -> Text -> Text -> Value -> IO AcceptOperationResult
acceptKernelOperation = acceptKernelOperationWith False Nothing

-- Only the allowlisted ETH gateway supplies a nonce attested on both recent
-- chains. Older ETH operations stay pending for settlement on their own nonce.
acceptKernelEthOperationAtNonce :: Connection -> KernelScope -> Integer -> Text -> Text -> Text -> Value -> IO AcceptOperationResult
acceptKernelEthOperationAtNonce conn scope nonce = acceptKernelOperationWith False (Just nonce) conn scope

-- Recover a signed ETH operation whose original submission was ambiguous.
-- The anchor holds the same account lane and cannot become relayable.
acceptKernelCancellationAnchor :: Connection -> KernelScope -> Text -> Text -> Text -> Value -> IO AcceptOperationResult
acceptKernelCancellationAnchor = acceptKernelOperationWith True Nothing

acceptKernelOperationWith :: Bool -> Maybe Integer -> Connection -> KernelScope -> Text -> Text -> Text -> Value -> IO AcceptOperationResult
acceptKernelOperationWith cancellationOnly currentNonce conn scope preparationId client operationHash signedOperation = withTransaction conn $ do
  preparation <- getKernelPreparationForClient conn scope preparationId client
  case preparation of
    Nothing -> pure OperationPreparationMissing
    Just prepared
      | cancellationOnly && kpMode prepared /= PayWithEth -> pure OperationPreparationMismatch
      | maybe False (\nonce -> kpMode prepared /= PayWithEth || kpNonce prepared /= nonce) currentNonce -> pure OperationPreparationMismatch
      | not (sameUnsignedOperation prepared signedOperation) -> pure OperationPreparationMismatch
      | otherwise -> do
          lockScope conn "account" scope (kpSender prepared)
          existing <- getKernelOperationForClientWithLock conn scope operationHash client
          case existing of
            Just op
              | kpId (koPreparation op) /= preparationId || koSignedOperation op /= signedOperation -> pure OperationConflict
              | koCancellationOnly op && not cancellationOnly -> pure OperationConflict
              | koState op /= "pending" -> pure OperationAlreadyFinal
              | cancellationOnly -> pure OperationReplay
              | otherwise -> do
                  cancellation <- getAcceptedKernelCancellation conn op
                  pure $ case cancellation of
                    Just _ -> OperationConflict
                    Nothing -> OperationReplay
            Nothing -> acceptNew prepared
 where
  acceptNew prepared
    | cancellationOnly = insertAccepted prepared
    | otherwise = do
        [Only now] <- query_ conn "SELECT clock_timestamp()"
        if kpExpiresAt prepared <= now then pure OperationPreparationExpired else do
          valid <- hasSponsorAuthorization conn prepared operationHash now
          if valid then insertAccepted prepared else pure OperationPreparationMismatch
  insertAccepted prepared = do
    [Only busy] <- query conn
      "SELECT EXISTS (SELECT 1 FROM aa_kernel_operations\
      \ WHERE deployment=? AND chain_id=? AND entry_point=? AND sender=? AND status='pending'\
      \ AND (?::numeric IS NULL OR mode<>'eth' OR nonce=?))"
      (scopeParams scope ++ [toField (kpSender prepared), toField currentNonce, toField currentNonce])
    if busy then pure OperationLaneBusy else do
      inserted <- execute conn
        "INSERT INTO aa_kernel_operations\
        \ (deployment,chain_id,entry_point,sender,nonce,preparation_id,client_key,mode,user_operation_hash,signed_operation,cancellation_only)\
        \ VALUES (?,?,?,?,?,?,?,?,?,?::jsonb,?) ON CONFLICT DO NOTHING"
        (scopeParams scope ++
          [ toField (kpSender prepared), toField (kpNonce prepared), toField preparationId, toField client
          , toField (modeText $ kpMode prepared), toField operationHash, toField (jsonText signedOperation), toField cancellationOnly
          ])
      pure $ if inserted == 1 then OperationAccepted else OperationConflict

getKernelOperationForClient :: Connection -> KernelScope -> Text -> Text -> IO (Maybe KernelOperation)
getKernelOperationForClient = findKernelOperation False

getKernelOperationForClientWithLock :: Connection -> KernelScope -> Text -> Text -> IO (Maybe KernelOperation)
getKernelOperationForClientWithLock = findKernelOperation True

findKernelOperation :: Bool -> Connection -> KernelScope -> Text -> Text -> IO (Maybe KernelOperation)
findKernelOperation lock conn scope operationHash client = oneRow =<< query conn
  (kernelOperationQuery <> (if lock then " FOR UPDATE OF o" else ""))
  (scopeParams scope ++ [toField operationHash, toField client])

kernelOperationQuery :: Query
kernelOperationQuery =
  "SELECT p.deployment,p.chain_id,p.entry_point,p.sender,p.owner_address,p.nonce,p.preparation_id,p.client_key,p.mode,p.unsigned_operation,p.expires_at,p.prepared_safe_block,\
  \ o.user_operation_hash,o.signed_operation,o.status,o.terminal_evidence,o.cancellation_only FROM aa_kernel_operations o\
  \ JOIN aa_kernel_preparations p USING (deployment,chain_id,entry_point,preparation_id)\
  \ WHERE o.deployment=? AND o.chain_id=? AND o.entry_point=? AND o.user_operation_hash=? AND o.client_key=?"

sameUnsignedOperation :: KernelPreparation -> Value -> Bool
sameUnsignedOperation prepared signedOperation =
  matchesPaymentMode prepared && sameOperationBody (kpUnsignedOperation prepared) signedOperation

sameOperationBody :: Value -> Value -> Bool
sameOperationBody unsignedOperation signedOperation = case (unsignedOperation, signedOperation) of
  (Object unsigned, Object signed) -> case KM.lookup "signature" signed of
    Just (String signature) | signature /= "0x" && signature /= "" ->
      KM.delete "signature" unsigned == KM.delete "signature" signed
    _ -> False
  _ -> False

matchesPaymentMode :: KernelPreparation -> Bool
matchesPaymentMode prepared = case kpUnsignedOperation prepared of
  Object operation -> case kpMode prepared of
    PayWithEth -> noPaymasterFields operation
    Sponsored -> case KM.lookup "paymaster" operation of
      Just (String _) -> all (`KM.member` operation)
        ["paymasterData", "paymasterVerificationGasLimit", "paymasterPostOpGasLimit"]
        && all (not . (`KM.member` operation)) ["paymasterAndData", "paymasterSignature"]
      _ -> False
  _ -> False

noPaymasterFields :: KM.KeyMap Value -> Bool
noPaymasterFields operation = all (not . (`KM.member` operation))
  ["paymaster", "paymasterData", "paymasterVerificationGasLimit", "paymasterPostOpGasLimit", "paymasterAndData", "paymasterSignature"]

hasSponsorAuthorization :: Connection -> KernelPreparation -> Text -> UTCTime -> IO Bool
hasSponsorAuthorization _ prepared _ _ | kpMode prepared == PayWithEth = pure True
hasSponsorAuthorization conn prepared operationHash now = case kpUnsignedOperation prepared of
  Object operation | Just (String paymaster) <- KM.lookup "paymaster" operation -> do
    lockKernelSponsorScope conn (kpScope prepared) paymaster
    [Only eligible] <- query conn
      "SELECT EXISTS (SELECT 1 FROM aa_kernel_sponsorships\
      \ WHERE deployment=? AND chain_id=? AND entry_point=? AND paymaster=?\
      \ AND preparation_id=? AND client_key=? AND sender=? AND nonce=?\
      \ AND user_operation_hash=? AND status='signed' AND to_timestamp(valid_until) > ?)"
      (scopeParams (kpScope prepared) ++
        [ toField paymaster, toField (kpId prepared), toField (kpClientKey prepared), toField (kpSender prepared)
        , toField (kpNonce prepared), toField operationHash, toField now
        ])
    pure eligible
  _ -> pure False

finalizeKernelOperation :: Connection -> KernelScope -> Text -> KernelInclusion -> IO Bool
finalizeKernelOperation conn scope operationHash inclusion
  | kiBlockNumber inclusion < 0 || kiActualGasCost inclusion < 0 = pure False
  | otherwise = do
      let evidence = jsonText $ inclusionValue inclusion
          -- A safe receipt can arrive after a nonce observation. Keep the richer
          -- evidence and never replace a previously recorded conflicting receipt.
      updated <- execute conn
        "UPDATE aa_kernel_operations SET status='included',terminal_evidence=?::jsonb\
        \ WHERE deployment=? AND chain_id=? AND entry_point=? AND user_operation_hash=? AND sender=? AND nonce=?\
        \ AND ((mode='eth' AND ? IS NULL) OR (mode='sponsored' AND signed_operation->>'paymaster'=?))\
        \ AND (status='pending' OR (status='nonce_consumed' AND NOT jsonb_exists(terminal_evidence,'cancellationHash'))\
        \ OR (status='included' AND terminal_evidence=?::jsonb))"
        ([toField evidence] ++ scopeParams scope ++
          [ toField operationHash, toField (kiSender inclusion), toField (kiNonce inclusion)
          , toField (kiPaymaster inclusion), toField (kiPaymaster inclusion), toField evidence
          ])
      pure $ updated == 1

retireKernelOperation :: Connection -> KernelScope -> Text -> KernelRetirement -> IO Bool
retireKernelOperation conn scope operationHash evidence
  | krObservedNonce evidence <= krNonce evidence = pure False
  | shiftR (krObservedNonce evidence) 64 /= shiftR (krNonce evidence) 64 = pure False
  | krBlockNumber evidence < 0 = pure False
  | otherwise = do
      let payload = jsonText $ object
            [ "observedNonce" .= show (krObservedNonce evidence)
            , "blockNumber" .= show (krBlockNumber evidence)
            , "blockHash" .= krBlockHash evidence
            ]
      updated <- execute conn
        "UPDATE aa_kernel_operations SET status='nonce_consumed',terminal_evidence=?::jsonb\
        \ WHERE deployment=? AND chain_id=? AND entry_point=? AND user_operation_hash=? AND sender=? AND nonce=?\
        \ AND mode='eth'\
        \ AND (status='pending' OR (status='nonce_consumed' AND terminal_evidence=?::jsonb))"
        ([toField payload] ++ scopeParams scope ++
          [toField operationHash, toField (krSender evidence), toField (krNonce evidence), toField payload])
      pure $ updated == 1

-- Cancellation drafts do not release the original lane. The same account
-- lock serializes cancellation writes with acceptance of original bytes.
storeKernelCancellation :: Connection -> KernelOperation -> KernelCancellation -> IO StorePreparationResult
storeKernelCancellation conn original cancellation
  | kcSignedOperation cancellation /= Nothing || kcHash cancellation /= Nothing = pure PreparationConflict
  | T.null preparationId || preparationId == kpId prepared = pure PreparationConflict
  | not (matchesCancellationOriginal prepared (kcUnsignedOperation cancellation)) = pure PreparationConflict
  | otherwise = withTransaction conn $ do
      current <- lockCancellationOriginal conn original
      case current of
        Just storedOriginal | koState storedOriginal == "pending" -> do
          prior <- getKernelCancellation conn storedOriginal preparationId
          case prior of
            Just stored | kcUnsignedOperation stored == kcUnsignedOperation cancellation -> pure PreparationAlreadyStored
            Just _ -> pure PreparationConflict
            Nothing -> storeNew storedOriginal
        _ -> pure PreparationConflict
 where
  prepared = koPreparation original
  preparationId = kcPreparationId cancellation
  storeNew storedOriginal = do
    accepted <- getAcceptedKernelCancellation conn storedOriginal
    case accepted of
      Just _ -> pure PreparationConflict
      Nothing -> do
        inserted <- execute conn
          "INSERT INTO aa_kernel_cancellations\
          \ (deployment,chain_id,entry_point,original_hash,cancellation_preparation_id,unsigned_operation,expires_at)\
          \ SELECT ?,?,?,?,?,?::jsonb,?::timestamptz WHERE ? > clock_timestamp() ON CONFLICT DO NOTHING"
          (scopeParams (kpScope prepared) ++
            [ toField (koHash original), toField preparationId, toField (jsonText $ kcUnsignedOperation cancellation)
            , toField (kcExpiresAt cancellation), toField (kcExpiresAt cancellation)
            ])
        pure $ if inserted == 1 then PreparationStored else PreparationConflict

getKernelCancellation :: Connection -> KernelOperation -> Text -> IO (Maybe KernelCancellation)
getKernelCancellation conn original preparationId = findKernelCancellation conn original (Just preparationId)

getAcceptedKernelCancellation :: Connection -> KernelOperation -> IO (Maybe KernelCancellation)
getAcceptedKernelCancellation conn original = findKernelCancellation conn original Nothing

findKernelCancellation :: Connection -> KernelOperation -> Maybe Text -> IO (Maybe KernelCancellation)
findKernelCancellation conn original preparationId = oneRow =<< query conn
  "SELECT c.cancellation_preparation_id,c.unsigned_operation,c.expires_at,c.signed_operation,c.cancellation_hash\
  \ FROM aa_kernel_cancellations c JOIN aa_kernel_operations o\
  \ ON (c.deployment,c.chain_id,c.entry_point,c.original_hash)=(o.deployment,o.chain_id,o.entry_point,o.user_operation_hash)\
  \ WHERE c.deployment=? AND c.chain_id=? AND c.entry_point=? AND c.original_hash=?\
  \ AND o.client_key=? AND o.mode='eth'\
  \ AND ((?::text IS NULL AND c.cancellation_hash IS NOT NULL) OR c.cancellation_preparation_id=?)"
  (originalParams original ++ [toField preparationId, toField preparationId])

acceptKernelCancellation :: Connection -> KernelOperation -> Text -> Text -> Value -> IO AcceptOperationResult
acceptKernelCancellation conn original preparationId cancellationHash signedOperation
  | koHash original == cancellationHash = pure OperationConflict
  | otherwise = withTransaction conn $ do
      current <- lockCancellationOriginal conn original
      case current of
        Nothing -> pure OperationPreparationMissing
        Just storedOriginal
          | koState storedOriginal /= "pending" -> pure OperationAlreadyFinal
          | otherwise -> acceptPending storedOriginal
 where
  acceptPending storedOriginal = do
    quote <- getKernelCancellation conn storedOriginal preparationId
    case quote of
      Nothing -> pure OperationPreparationMissing
      Just draft
        | not (sameOperationBody (kcUnsignedOperation draft) signedOperation) -> pure OperationPreparationMismatch
        | otherwise -> do
            accepted <- getAcceptedKernelCancellation conn storedOriginal
            case accepted of
              Just prior
                | kcPreparationId prior == preparationId
                , kcHash prior == Just cancellationHash
                , kcSignedOperation prior == Just signedOperation -> pure OperationReplay
                | otherwise -> pure OperationConflict
              Nothing -> persistSigned draft
  persistSigned draft = do
    updated <- execute conn
      "UPDATE aa_kernel_cancellations SET cancellation_hash=?,signed_operation=?::jsonb\
      \ WHERE deployment=? AND chain_id=? AND entry_point=? AND original_hash=? AND cancellation_preparation_id=?\
      \ AND expires_at > clock_timestamp()"
      ([toField cancellationHash, toField (jsonText signedOperation)] ++ scopeParams (kpScope $ koPreparation original) ++
        [toField (koHash original), toField (kcPreparationId draft)])
    pure $ if updated == 1 then OperationAccepted else OperationPreparationExpired

-- The on-chain winner is authoritative, including a cancellation whose
-- execution reverted after EntryPoint consumed the nonce.
finalizeKernelCancellation :: Connection -> KernelOperation -> Text -> KernelInclusion -> IO Bool
finalizeKernelCancellation conn original cancellationHash inclusion
  | kiPaymaster inclusion /= Nothing || kiBlockNumber inclusion < 0 || kiActualGasCost inclusion < 0 = pure False
  | otherwise = do
      let evidence = jsonText $ object $ ("cancellationHash" .= cancellationHash) : inclusionFields inclusion
      updated <- execute conn
        "UPDATE aa_kernel_operations o SET status='nonce_consumed',terminal_evidence=?::jsonb\
        \ FROM aa_kernel_cancellations c\
        \ WHERE (o.deployment,o.chain_id,o.entry_point,o.user_operation_hash)=(c.deployment,c.chain_id,c.entry_point,c.original_hash)\
        \ AND c.deployment=? AND c.chain_id=? AND c.entry_point=? AND c.original_hash=? AND o.client_key=?\
        \ AND c.cancellation_hash=? AND o.mode='eth' AND o.sender=? AND o.nonce=?\
        \ AND (o.status='pending' OR (o.status='nonce_consumed' AND\
        \ (NOT jsonb_exists(o.terminal_evidence,'cancellationHash') OR o.terminal_evidence=?::jsonb)))"
        ([toField evidence] ++ originalParams original ++
          [toField cancellationHash, toField (kiSender inclusion), toField (kiNonce inclusion), toField evidence])
      pure $ updated == 1

lockCancellationOriginal :: Connection -> KernelOperation -> IO (Maybe KernelOperation)
lockCancellationOriginal conn original
  | kpMode prepared /= PayWithEth = pure Nothing
  | otherwise = do
      lockScope conn "account" (kpScope prepared) (kpSender prepared)
      current <- getKernelOperationForClientWithLock conn (kpScope prepared) (koHash original) (kpClientKey prepared)
      pure $ case current of
        Just stored | koPreparation stored == prepared -> Just stored
        _ -> Nothing
 where
  prepared = koPreparation original

matchesCancellationOriginal :: KernelPreparation -> Value -> Bool
matchesCancellationOriginal prepared (Object operation) =
  kpMode prepared == PayWithEth
  && noPaymasterFields operation
  && KM.lookup "sender" operation == Just (String $ kpSender prepared)
  && KM.lookup "nonce" operation == Just (String $ "0x" <> T.pack (showHex (kpNonce prepared) ""))
matchesCancellationOriginal _ _ = False

originalParams :: KernelOperation -> [Action]
originalParams original =
  scopeParams (kpScope prepared) ++ [toField (koHash original), toField (kpClientKey prepared)]
 where
  prepared = koPreparation original

lockKernelSponsorScope :: Connection -> KernelScope -> Text -> IO ()
lockKernelSponsorScope conn = lockScope conn "sponsor"

lockScope :: Connection -> Text -> KernelScope -> Text -> IO ()
lockScope conn lockType scope identity = void (query conn
  "SELECT 1::integer FROM (SELECT pg_advisory_xact_lock(hashtextextended(\
  \ CAST(jsonb_build_array('plether-kernel',?::text,?::text,?::bigint,?::text,?::text) AS text),0))) AS scope_lock"
  ([toField lockType] ++ scopeParams scope ++ [toField identity]) :: IO [Only Int])

kernelSponsorshipReady :: Connection -> KernelScope -> Text -> Integer -> IO Bool
kernelSponsorshipReady conn scope paymaster maxAge = do
  [Only fresh] <- query conn
    "SELECT EXISTS (SELECT 1 FROM aa_kernel_reconciler_state\
    \ WHERE deployment=? AND chain_id=? AND entry_point=? AND paymaster=? AND paused_reason IS NULL\
    \ AND last_success_at >= clock_timestamp()-make_interval(secs => ?::double precision))"
    (scopeParams scope ++ [toField paymaster, toField maxAge])
  pure $ maxAge > 0 && fresh

kernelSponsorshipStateIsEmpty :: Connection -> KernelScope -> Text -> IO Bool
kernelSponsorshipStateIsEmpty conn scope paymaster = do
  [Only empty] <- query conn
    "SELECT NOT EXISTS (SELECT 1 FROM aa_kernel_sponsorships\
    \ WHERE deployment=? AND chain_id=? AND entry_point=? AND paymaster=?)"
    (scopeParams scope ++ [toField paymaster])
  pure empty

reserveKernelSponsorship
  :: Connection -> KernelSponsorBudget -> KernelSponsorshipDraft -> IO (Either Text KernelSponsorship)
reserveKernelSponsorship conn budget draft = withTransaction conn $ do
  let scope = ksdScope draft
      paymaster = ksdPaymaster draft
      cost = ksdMaxCostWei draft
  lockKernelSponsorScope conn scope paymaster
  ready <- kernelSponsorshipReady conn scope paymaster 120
  [Only valid] <- query conn "SELECT to_timestamp(?::bigint)>clock_timestamp()" (Only $ ksdValidUntil draft)
  if not ready then pure $ Left "SPONSORSHIP_NOT_READY"
  else if not valid then pure $ Left "SPONSORSHIP_EXPIRED"
  else if cost <= 0 || cost > ksbMaxCostWei budget then pure $ Left "PER_OPERATION_BUDGET_EXCEEDED"
  else do
    prior <- getKernelSponsorshipForClient conn scope paymaster (ksdId draft) (ksdClientKey draft)
    case prior of
      Just existing
        | kspDraft existing == draft && kspStatus existing `elem` ["reserved", "signed"] -> pure $ Right existing
        | otherwise -> pure $ Left "SPONSORSHIP_REQUEST_CONFLICT"
      Nothing -> reserveNew
 where
  reserveNew = do
    let scope = ksdScope draft
        paymaster = ksdPaymaster draft
        cost = ksdMaxCostWei draft
        sender = ksdSender draft
        client = ksdClientKey draft
    BudgetTotals accountActive clientActive globalActive accountHour globalHour globalDay <-
      kernelBudgetTotals conn scope paymaster sender client
    let denied
          | accountActive + cost > ksbAccountOutstandingWei budget = Just "ACCOUNT_OUTSTANDING_BUDGET_EXCEEDED"
          | clientActive + cost > ksbClientOutstandingWei budget = Just "CLIENT_OUTSTANDING_BUDGET_EXCEEDED"
          | globalActive + cost > ksbGlobalOutstandingWei budget = Just "GLOBAL_OUTSTANDING_BUDGET_EXCEEDED"
          | accountHour + accountActive + cost > ksbAccountHourlyWei budget = Just "ACCOUNT_HOURLY_BUDGET_EXCEEDED"
          | globalHour + globalActive + cost > ksbGlobalHourlyWei budget = Just "GLOBAL_HOURLY_BUDGET_EXCEEDED"
          | globalDay + globalActive + cost > ksbGlobalDailyWei budget = Just "GLOBAL_DAILY_BUDGET_EXCEEDED"
          | otherwise = Nothing
    case denied of
      Just reason -> pure $ Left reason
      Nothing -> do
        rows <- execute conn
          "INSERT INTO aa_kernel_sponsorships (deployment,chain_id,entry_point,paymaster,authorization_id,preparation_id,\
          \ client_key,sender,nonce,max_cost_wei,valid_after,valid_until,unsigned_operation)\
          \ VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?::jsonb) ON CONFLICT DO NOTHING"
          (scopeParams scope ++
            [ toField paymaster, toField (ksdId draft), toField (ksdPreparationId draft), toField client
            , toField sender, toField (ksdNonce draft), toField cost, toField (ksdValidAfter draft)
            , toField (ksdValidUntil draft), toField (jsonText $ ksdUnsignedOperation draft)
            ])
        if rows == 1
          then pure $ Right $ KernelSponsorship draft Nothing Nothing "reserved"
          else pure $ Left "SPONSORSHIP_REQUEST_CONFLICT"

getKernelSponsorshipForClient :: Connection -> KernelScope -> Text -> Text -> Text -> IO (Maybe KernelSponsorship)
getKernelSponsorshipForClient conn scope paymaster authorizationId client = oneRow =<< query conn
  "SELECT deployment,chain_id,entry_point,paymaster,authorization_id,preparation_id,client_key,sender,nonce,max_cost_wei,\
  \ valid_after,valid_until,unsigned_operation,signature,user_operation_hash,status\
  \ FROM aa_kernel_sponsorships\
  \ WHERE deployment=? AND chain_id=? AND entry_point=? AND paymaster=? AND authorization_id=? AND client_key=?"
  (scopeParams scope ++ [toField paymaster, toField authorizationId, toField client])

storeKernelSponsorshipSignature
  :: Connection -> KernelScope -> Text -> Text -> Text -> Text -> Text -> IO Bool
storeKernelSponsorshipSignature conn scope paymaster authorizationId client signature operationHash =
  withTransaction conn $ do
    lockKernelSponsorScope conn scope paymaster
    ready <- kernelSponsorshipReady conn scope paymaster 120
    if not ready then pure False else do
      changed <- execute conn
        "UPDATE aa_kernel_sponsorships SET signature=?,user_operation_hash=?,status='signed'\
        \ WHERE deployment=? AND chain_id=? AND entry_point=? AND paymaster=? AND authorization_id=? AND client_key=?\
        \ AND to_timestamp(valid_until)>clock_timestamp()\
        \ AND (status='reserved' OR (status='signed' AND signature=? AND user_operation_hash=?))"
        ([toField signature, toField operationHash] ++ scopeParams scope ++
          [toField paymaster, toField authorizationId, toField client, toField signature, toField operationHash])
      pure $ changed == 1

data KernelSettlementConflict = KernelSettlementConflict
  deriving stock (Show)

instance Exception KernelSettlementConflict

-- Called inside the reconciler transaction holding lockKernelSponsorScope.
-- An operation sent via another bundler may have no local relay record. A
-- conflicting local record rolls back the sponsorship update to the savepoint.
settleKernelSponsorship :: Connection -> KernelScope -> Text -> Text -> KernelInclusion -> IO Bool
settleKernelSponsorship conn scope paymaster operationHash inclusion
  | kiPaymaster inclusion /= Just paymaster = pure False
  | kiActualGasCost inclusion < 0 || kiBlockNumber inclusion < 0 = pure False
  | otherwise = either (const False) id <$> try @KernelSettlementConflict (withSavepoint conn settle)
  where
    settle = do
      let evidence = jsonText $ inclusionValue inclusion
      changed <- execute conn
        "UPDATE aa_kernel_sponsorships SET status='settled',actual_gas_cost=?,terminal_evidence=?::jsonb,\
        \ settled_at=COALESCE(settled_at,clock_timestamp())\
        \ WHERE deployment=? AND chain_id=? AND entry_point=? AND paymaster=? AND user_operation_hash=?\
        \ AND sender=? AND nonce=? AND max_cost_wei>=?\
        \ AND (status='signed' OR (status='settled' AND terminal_evidence=?::jsonb))"
        ([toField (kiActualGasCost inclusion), toField evidence] ++ scopeParams scope ++
          [ toField paymaster, toField operationHash, toField (kiSender inclusion), toField (kiNonce inclusion)
          , toField (kiActualGasCost inclusion), toField evidence
          ])
      if changed /= 1 then pure False else do
        finalized <- finalizeKernelOperation conn scope operationHash inclusion
        if finalized then pure True else do
          [Only present] <- query conn
            "SELECT EXISTS (SELECT 1 FROM aa_kernel_operations\
            \ WHERE deployment=? AND chain_id=? AND entry_point=? AND user_operation_hash=?)"
            (scopeParams scope ++ [toField operationHash])
          if present then throwIO KernelSettlementConflict else pure True

-- The EntryPoint treats validUntil as inclusive, and only a complete safe
-- scan permits releasing even unsigned reservations for this profile.
expireKernelSponsorshipsThrough :: Connection -> KernelScope -> Text -> Integer -> IO ()
expireKernelSponsorshipsThrough conn scope paymaster safeTimestamp = void $ execute conn
  "WITH expired AS (\
  \ UPDATE aa_kernel_sponsorships SET status='expired'\
  \ WHERE deployment=? AND chain_id=? AND entry_point=? AND paymaster=?\
  \ AND status IN ('reserved','signed') AND valid_until<?\
  \ RETURNING deployment,chain_id,entry_point,preparation_id,user_operation_hash,paymaster,valid_until)\
  \ UPDATE aa_kernel_operations operation SET status='sponsorship_expired',\
  \ terminal_evidence=jsonb_build_object('validUntil',expired.valid_until::text,'safeTimestamp',?::text)\
  \ FROM expired WHERE operation.deployment=expired.deployment AND operation.chain_id=expired.chain_id\
  \ AND operation.entry_point=expired.entry_point AND operation.preparation_id=expired.preparation_id\
  \ AND operation.user_operation_hash=expired.user_operation_hash AND operation.signed_operation->>'paymaster'=expired.paymaster\
  \ AND operation.status='pending' AND operation.mode='sponsored'"
  (scopeParams scope ++ [toField paymaster, toField safeTimestamp, toField (show safeTimestamp)])

data BudgetTotals = BudgetTotals Integer Integer Integer Integer Integer Integer

instance FromRow BudgetTotals where
  fromRow = BudgetTotals <$> integerField <*> integerField <*> integerField
    <*> integerField <*> integerField <*> integerField

kernelBudgetTotals :: Connection -> KernelScope -> Text -> Text -> Text -> IO BudgetTotals
kernelBudgetTotals conn scope paymaster sender client = do
  [totals] <- query conn
    "SELECT COALESCE(SUM(max_cost_wei) FILTER (WHERE sender=? AND status IN ('reserved','signed')),0),\
    \ COALESCE(SUM(max_cost_wei) FILTER (WHERE client_key=? AND status IN ('reserved','signed')),0),\
    \ COALESCE(SUM(max_cost_wei) FILTER (WHERE status IN ('reserved','signed')),0),\
    \ COALESCE(SUM(actual_gas_cost) FILTER (WHERE sender=? AND settled_at>=statement_timestamp()-interval '1 hour'),0),\
    \ COALESCE(SUM(actual_gas_cost) FILTER (WHERE settled_at>=statement_timestamp()-interval '1 hour'),0),\
    \ COALESCE(SUM(actual_gas_cost) FILTER (WHERE settled_at>=statement_timestamp()-interval '24 hours'),0)\
    \ FROM aa_kernel_sponsorships WHERE deployment=? AND chain_id=? AND entry_point=? AND paymaster=?\
    \ AND (status IN ('reserved','signed') OR (status='settled' AND settled_at>=statement_timestamp()-interval '24 hours'))"
    ([toField sender, toField client, toField sender] ++ scopeParams scope ++ [toField paymaster])
  pure totals

inclusionValue :: KernelInclusion -> Value
inclusionValue = object . inclusionFields

inclusionFields :: KernelInclusion -> [Pair]
inclusionFields evidence =
  [ "sender" .= kiSender evidence
  , "nonce" .= show (kiNonce evidence)
  , "paymaster" .= kiPaymaster evidence
  , "transactionHash" .= kiTransactionHash evidence
  , "blockHash" .= kiBlockHash evidence
  , "blockNumber" .= show (kiBlockNumber evidence)
  , "executionSuccess" .= kiExecutionSuccess evidence
  , "actualGasCost" .= show (kiActualGasCost evidence)
  ]

scopeParams :: KernelScope -> [Action]
scopeParams scope = [toField (ksDeployment scope), toField (ksChainId scope), toField (ksEntryPoint scope)]

integerField :: RowParser Integer
integerField = fieldWith $ \column raw -> case raw >>= readMaybe . BS8.unpack of
  Just integer -> pure integer
  Nothing -> returnError ConversionFailed column "Kernel operation numeric value is not an integer"

jsonText :: Value -> Text
jsonText = TE.decodeUtf8 . LBS.toStrict . encode

oneRow :: [a] -> IO (Maybe a)
oneRow [] = pure Nothing
oneRow [result] = pure $ Just result
oneRow _ = fail "Ambiguous Kernel operation database row"
