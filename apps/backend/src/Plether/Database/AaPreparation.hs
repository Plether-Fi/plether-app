module Plether.Database.AaPreparation
  ( PreparationClaim (..), claimPreparation, claimPreparationCompatible, claimPreparationCompatibleFenced, savePreparedOperation, releasePreparation, linkPreparation, linkPreparationDiagnostic, getPreparationStatus, bindPreparationDeployment ) where

import qualified Plether.Database.AaPreparationRecovery as Recovery
import Data.Aeson (Value, encode)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import Data.Maybe (isJust)
import qualified Data.Text.Encoding as TE
import Database.PostgreSQL.Simple

data PreparationClaim = PreparationFenceLost | PreparationBusy | PreparationConflict | PreparationExpired | PreparationDisabled | PreparationClaimed (Maybe Value)
  deriving stock (Eq, Show)

-- A short transaction owns only the lease, never an upstream RPC or KMS call.
-- The chosen operation is immutable once written, including across lease loss.
claimPreparation :: Connection -> Bool -> Text -> Text -> Text -> Text -> Text -> IO PreparationClaim
claimPreparation conn allowNew client sender identifier intent lease =
  claimPreparationCompatible conn allowNew client sender identifier intent [] lease

-- Compatibility is only for an already-persisted operation from a reviewed
-- prior gas policy. Empty work never acquires permission to use a retired policy.
claimPreparationCompatible :: Connection -> Bool -> Text -> Text -> Text -> Text -> [Text] -> Text -> IO PreparationClaim
claimPreparationCompatible conn allowNew client sender identifier intent priorIntents lease = withTransaction conn $
  claimPreparationUnlocked conn allowNew client sender identifier intent priorIntents lease

claimPreparationCompatibleFenced :: Connection -> Recovery.Fence -> Bool -> Text -> Text -> Text -> Text -> [Text] -> Text -> IO PreparationClaim
claimPreparationCompatibleFenced conn fence allowNew client sender identifier intent priorIntents lease = withTransaction conn $ do
  Recovery.recoveryLock conn
  valid <- Recovery.fenceValid conn fence
  if valid then claimPreparationUnlocked conn allowNew client sender identifier intent priorIntents lease else pure PreparationFenceLost

claimPreparationUnlocked :: Connection -> Bool -> Text -> Text -> Text -> Text -> [Text] -> Text -> IO PreparationClaim
claimPreparationUnlocked conn allowNew client sender identifier intent priorIntents lease = do
  if allowNew then do
    _ <- execute conn
      "INSERT INTO aa_preparations(client_key,sender,preparation_id,intent_hash) VALUES (?,?,?,?) ON CONFLICT DO NOTHING"
      (client, sender, identifier, intent)
    pure ()
  else pure ()
  rows <- query conn
    "SELECT intent_hash,operation,lease_until IS NULL OR lease_until < clock_timestamp(),expires_at>clock_timestamp() FROM aa_preparations WHERE client_key=? AND sender=? AND preparation_id=? FOR UPDATE"
    (client,sender,identifier) :: IO [(Text, Maybe Value, Bool, Bool)]
  case rows of
    [(stored, operation, available, fresh)]
      | stored /= intent && not (isJust operation && stored `elem` priorIntents) -> pure PreparationConflict
      | not fresh -> pure PreparationExpired
      | not available -> pure PreparationBusy
      | otherwise -> do
          _ <- execute conn
            "UPDATE aa_preparations SET lease_token=?,lease_until=clock_timestamp()+interval '60 seconds',updated_at=clock_timestamp() WHERE client_key=? AND sender=? AND preparation_id=?"
            (lease,client,sender,identifier)
          pure $ PreparationClaimed operation
    [] | not allowNew -> pure PreparationDisabled
    _ -> pure PreparationConflict

savePreparedOperation :: Connection -> Text -> Text -> Text -> Text -> Value -> IO Bool
savePreparedOperation conn client sender identifier lease operation = do
  affected <- execute conn
    "UPDATE aa_preparations SET operation=?::jsonb,updated_at=clock_timestamp() WHERE client_key=? AND sender=? AND preparation_id=? AND lease_token=? AND lease_until>clock_timestamp() AND operation IS NULL"
    (TE.decodeUtf8 $ LBS.toStrict $ encode operation,client,sender,identifier,lease)
  pure $ affected == 1

linkPreparation :: Connection -> Text -> Text -> Text -> Text -> Text -> IO Bool
linkPreparation conn client sender identifier lease digest = do
  affected <- execute conn
    "UPDATE aa_preparations SET authorization_digest=? WHERE client_key=? AND sender=? AND preparation_id=? AND lease_token=? AND lease_until>clock_timestamp() AND (authorization_digest IS NULL OR authorization_digest=?)"
    (digest,client,sender,identifier,lease,digest)
  pure $ affected == 1

releasePreparation :: Connection -> Text -> Text -> Text -> Text -> IO ()
releasePreparation conn client sender identifier lease = do
  _ <- execute conn
    "UPDATE aa_preparations SET lease_token=NULL,lease_until=NULL,updated_at=clock_timestamp() WHERE client_key=? AND sender=? AND preparation_id=? AND lease_token=?"
    (client,sender,identifier,lease)
  pure ()

-- Nullable correlation travels in the existing durable linkage write. It does
-- not participate in intent, digest, lease ownership or authorization identity.
-- Retain the first reference on retries, including retries from another process.
linkPreparationDiagnostic :: Connection -> Text -> Text -> Text -> Text -> Text -> Maybe Text -> Integer -> Text -> IO Bool
linkPreparationDiagnostic conn client sender identifier lease digest attempt chain deployment = do
  affected <- execute conn
    "UPDATE aa_preparations SET authorization_digest=?,diagnostic_attempt_id=COALESCE(diagnostic_attempt_id,?::uuid),diagnostic_chain_id=COALESCE(diagnostic_chain_id,?),diagnostic_deployment=COALESCE(diagnostic_deployment,?) WHERE client_key=? AND sender=? AND preparation_id=? AND lease_token=? AND lease_until>clock_timestamp() AND (authorization_digest IS NULL OR authorization_digest=?)"
    (digest,attempt,chain,deployment,client,sender,identifier,lease,digest)
  pure $ affected == 1

-- Read-only, client-bound projection. Never return operations, signatures, or client keys.
getPreparationStatus :: Connection -> Text -> Text -> Maybe Text -> Maybe Text -> IO (Maybe (Value, Maybe Value))
getPreparationStatus conn client sender identifier operationHash = do
  rows <- query conn
    "SELECT jsonb_build_object('authorizationState',COALESCE(a.state,'preparing'),\
    \'validUntil',a.valid_until::text,'userOperationHash',a.expected_user_operation_hash,\
    \'transactionHash',e.transaction_hash,'executionSuccess',e.success,'safeSettlement',e.finalized_at IS NOT NULL,\
    \'assisted',g.digest IS NOT NULL,'assistanceVerified',COALESCE(g.verified,FALSE),\
    \'preparationAvailable',p.operation IS NOT NULL AND p.expires_at>clock_timestamp(),\
    \'assistanceBlocked',EXISTS (SELECT 1 FROM aa_close_assistance other_g\
    \ JOIN aa_sponsorship_authorizations other_a USING(digest)\
    \ LEFT JOIN aa_user_operation_events other_e USING(digest)\
    \ WHERE other_g.account=p.sender AND other_g.router=COALESCE(g.router,p.diagnostic_deployment)\
    \ AND (other_a.state IN ('reserved','signed','submitted') OR (COALESCE(other_e.success,FALSE) AND NOT other_g.verified)))),p.operation\
    \ FROM aa_preparations p LEFT JOIN aa_sponsorship_authorizations a ON a.digest=p.authorization_digest AND a.client_key=p.client_key\
    \ LEFT JOIN aa_close_assistance g ON g.digest=a.digest\
    \ LEFT JOIN aa_user_operation_events e ON e.digest=a.digest AND e.user_operation_hash=a.expected_user_operation_hash\
    \ WHERE p.client_key=? AND p.sender=? AND ((?::text IS NOT NULL AND p.preparation_id=?)\
    \ OR (?::text IS NOT NULL AND a.expected_user_operation_hash=?)) ORDER BY p.expires_at DESC,p.preparation_id LIMIT 1"
    (client,sender,identifier,identifier,operationHash,operationHash) :: IO [(Value, Maybe Value)]
  case rows of
    [] -> pure Nothing
    [value] -> pure $ Just value
    _ -> fail "Ambiguous preparation locator"

-- Deployment locator is saved even if sponsorship is refused or its response is lost.
bindPreparationDeployment :: Connection -> Text -> Text -> Text -> Text -> Integer -> Text -> IO Bool
bindPreparationDeployment conn client sender identifier lease chain deployment = do
  changed <- execute conn
    "UPDATE aa_preparations SET diagnostic_chain_id=?,diagnostic_deployment=?\
    \ WHERE client_key=? AND sender=? AND preparation_id=? AND lease_token=? AND lease_until>clock_timestamp()\
    \ AND (diagnostic_deployment IS NULL OR diagnostic_deployment=?)"
    (chain,deployment,client,sender,identifier,lease,deployment)
  pure $ changed == 1
