-- Every mutating registry operation uses the sponsorship budget lock. Fences
-- are checked in the SAME transaction as reservation, not before it.
module Plether.Database.AaPreparationRecovery
  ( Scope (..), Fence (..), recoveryLock, beginPreparation, releasePreparation
  , fenceValid, bindDeployment, linkAuthorization, deliveryActive, retirePreparation
  , matchingPreparations, bindHistoricalAuthorizations, operationOutcomes, registryRetired, retirementReason, saveChallenge, saveChallengeAt, readChallenge
  , consumeChallenge, sessionOwner, sessionSubmissionClient
  ) where

import Control.Monad (void, forM_, when)
import Data.Aeson (Value(..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.Text.Encoding as TE
import Plether.Config (NativeAaConfig)
import qualified Plether.AA.Paymaster as Paymaster
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Database.PostgreSQL.Simple

data Scope = Scope { scopeChain :: Integer, scopePaymaster :: Text, scopeSender :: Text, scopeId :: Text }
  deriving stock (Eq, Show)
data Fence = Fence Scope Integer Text deriving stock (Eq, Show)

scopeTuple :: Scope -> (Integer,Text,Text,Text)
scopeTuple (Scope chain paymaster sender identifier) = (chain,paymaster,sender,identifier)

recoveryLock :: Connection -> IO ()
recoveryLock conn = do
  void $ execute_ conn "SET LOCAL lock_timeout='5s'"
  void $ execute_ conn "SET LOCAL statement_timeout='15s'"
  void (query_ conn "SELECT 1::int FROM pg_advisory_xact_lock(4338008421614)" :: IO [Only Int])

ensureRegistry :: Connection -> Scope -> IO ()
ensureRegistry conn scope = void $ execute conn
  "INSERT INTO aa_preparation_registry(chain_id,paymaster,sender,preparation_id) VALUES (?,?,?,?) ON CONFLICT DO NOTHING" (scopeTuple scope)

beginPreparation :: Connection -> Scope -> Text -> IO (Either Text Fence)
beginPreparation conn scope@(Scope chain paymaster sender identifier) token = withTransaction conn $ do
  recoveryLock conn
  ensureRegistry conn scope
  rows <- query conn "SELECT retired,generation,lease_until>clock_timestamp() FROM aa_preparation_registry WHERE chain_id=? AND paymaster=? AND sender=? AND preparation_id=? FOR UPDATE" (scopeTuple scope) :: IO [(Bool,Integer,Maybe Bool)]
  case rows of
    [(True,_,_)] -> pure $ Left "PREPARATION_RETIRED"
    [(_,_,Just True)] -> pure $ Left "PREPARATION_BUSY"
    [(False,generation,_)] -> do
      void $ execute conn "UPDATE aa_preparation_registry SET lease_token=?,lease_until=clock_timestamp()+interval '120 seconds',updated_at=clock_timestamp() WHERE chain_id=? AND paymaster=? AND sender=? AND preparation_id=?" (token,chain,paymaster,sender,identifier)
      pure $ Right $ Fence scope generation token
    _ -> fail "Invalid preparation registry"

releasePreparation :: Connection -> Fence -> IO ()
releasePreparation conn (Fence (Scope chain paymaster sender identifier) generation token) = void $ execute conn
  "UPDATE aa_preparation_registry SET lease_token=NULL,lease_until=NULL WHERE chain_id=? AND paymaster=? AND sender=? AND preparation_id=? AND generation=? AND lease_token=?"
  (chain,paymaster,sender,identifier,generation,token)

-- Caller must hold recoveryLock in an open transaction.
fenceValid :: Connection -> Fence -> IO Bool
fenceValid conn (Fence (Scope chain paymaster sender identifier) generation token) = do
  rows <- query conn "SELECT EXISTS (SELECT 1 FROM aa_preparation_registry WHERE chain_id=? AND paymaster=? AND sender=? AND preparation_id=? AND NOT retired AND generation=? AND lease_token=? AND lease_until>clock_timestamp())" (chain,paymaster,sender,identifier,generation,token)
  pure $ rows == [Only True]

-- Record the paymaster only while the exact preparation owns a live fence.
bindDeployment :: Connection -> Fence -> Text -> IO Bool
bindDeployment conn fence@(Fence (Scope _ paymaster sender identifier) _ _) client = withTransaction conn $ do
  recoveryLock conn
  valid <- fenceValid conn fence
  if not valid then pure False else do
    changed <- execute conn "UPDATE aa_preparations SET recovery_paymaster=? WHERE client_key=? AND sender=? AND preparation_id=? AND (recovery_paymaster IS NULL OR recovery_paymaster=?)" (paymaster,client,sender,identifier,paymaster)
    pure $ changed == 1

linkAuthorization :: Connection -> Fence -> Text -> IO ()
linkAuthorization conn (Fence (Scope chain paymaster sender identifier) _ _) digest = void $ execute conn
  "INSERT INTO aa_preparation_authorizations(chain_id,paymaster,sender,preparation_id,digest) VALUES (?,?,?,?,?) ON CONFLICT DO NOTHING" (chain,paymaster,sender,identifier,digest)

deliveryActive :: Connection -> Text -> IO Bool
deliveryActive conn digest = do
  rows <- query conn "SELECT NOT EXISTS (SELECT 1 FROM aa_preparation_authorizations a JOIN aa_preparation_registry r USING(chain_id,paymaster,sender,preparation_id) WHERE a.digest=? AND r.retired)" (Only digest)
  pure $ rows == [Only True]

registryRetired :: Connection -> Scope -> IO Bool
registryRetired conn scope = do
  rows <- query conn "SELECT retired FROM aa_preparation_registry WHERE chain_id=? AND paymaster=? AND sender=? AND preparation_id=?" (scopeTuple scope)
  pure $ rows == [Only True]

-- Never select one client arbitrarily. Limit+overflow is an unresolved outcome.
matchingPreparations :: Connection -> Scope -> Text -> IO [(Text,Maybe Text,Bool)]
matchingPreparations conn (Scope chain paymaster sender identifier) router = query conn
  "SELECT p.client_key,a.expected_user_operation_hash,(p.diagnostic_chain_id=? AND p.diagnostic_deployment=? AND p.recovery_paymaster=?) IS TRUE FROM aa_preparations p LEFT JOIN aa_sponsorship_authorizations a ON a.digest=p.authorization_digest WHERE p.sender=? AND p.preparation_id=? ORDER BY p.client_key LIMIT 21"
  (chain,router,paymaster,sender,identifier)

-- Old authorization payloads intentionally omitted paymaster/paymasterData.
-- Prove their deployment by recomputing the stored EIP-712 digest, never by
-- assigning the current paymaster merely because the account/ID matches.
-- Call only after owner-session verification. Unknown profiles stay unresolved.
bindHistoricalAuthorizations :: Connection -> NativeAaConfig -> Scope -> Text -> IO ()
bindHistoricalAuthorizations conn cfg scope@(Scope chain paymaster sender identifier) router = withTransaction conn $ do
  recoveryLock conn
  rows <- query conn
    "SELECT p.client_key,a.digest,a.valid_after,a.valid_until,a.max_cost_wei::text,a.operation FROM aa_preparations p JOIN aa_sponsorship_authorizations a ON a.digest=p.authorization_digest AND a.client_key=p.client_key AND a.sender=p.sender WHERE p.sender=? AND p.preparation_id=? AND p.recovery_paymaster IS NULL AND p.diagnostic_chain_id=? AND p.diagnostic_deployment=? ORDER BY p.client_key LIMIT 21"
    (sender,identifier,chain,router) :: IO [(Text,Text,Integer,Integer,Text,Value)]
  forM_ rows $ \(client,digest,validAfter,validUntil,cost,payload) -> case payload of
    Object fields | [(maxCost,"")] <- reads $ T.unpack cost,
      Right operation <- Paymaster.parsePackedUserOperation fields -> do
        let envelope = Paymaster.makeSponsorshipEnvelope cfg validAfter validUntil maxCost BS.empty
            verifiedDigest = "0x" <> TE.decodeUtf8 (B16.encode $ Paymaster.sponsorshipDigest operation envelope)
        when (Paymaster.puoSender operation == sender && verifiedDigest == digest && T.toLower (Paymaster.sePaymaster envelope) == paymaster) $ do
          ensureRegistry conn scope
          void $ execute conn "UPDATE aa_preparations SET recovery_paymaster=? WHERE client_key=? AND sender=? AND preparation_id=? AND recovery_paymaster IS NULL" (paymaster,client,sender,identifier)
          void $ execute conn "INSERT INTO aa_preparation_authorizations(chain_id,paymaster,sender,preparation_id,digest) VALUES (?,?,?,?,?) ON CONFLICT DO NOTHING" (chain,paymaster,sender,identifier,digest)
    _ -> pure ()

-- Bounded durable outcomes; only finalized events are presented as settled.
operationOutcomes :: Connection -> Scope -> IO [Value]
operationOutcomes conn (Scope _ _ sender identifier) = do
  rows <- query conn
    "SELECT DISTINCT jsonb_build_object('hash',a.expected_user_operation_hash,'state',CASE WHEN a.state='settled' AND e.finalized_at IS NULL THEN 'submitted' ELSE a.state END,'transactionHash',CASE WHEN e.finalized_at IS NOT NULL THEN e.transaction_hash END,'executionSuccess',CASE WHEN e.finalized_at IS NOT NULL THEN e.success END) FROM aa_preparations p JOIN aa_sponsorship_authorizations a ON a.digest=p.authorization_digest LEFT JOIN aa_user_operation_events e ON e.digest=a.digest WHERE p.sender=? AND p.preparation_id=? AND a.expected_user_operation_hash IS NOT NULL LIMIT 20"
    (sender,identifier) :: IO [Only Value]
  pure $ map fromOnly rows

retirePreparation :: Connection -> Scope -> Text -> IO (Either Text ())
retirePreparation conn scope router = withTransaction conn $ do
  recoveryLock conn
  ensureRegistry conn scope
  retired <- registryRetired conn scope
  if retired then pure $ Right () else do
    -- Conservative sender-wide liability/lease checks cover legacy reservations
    -- created before their preparation linkage was saved, including lost replies.
    reason <- retirementReason conn scope router
    case reason of
      Nothing -> do
        void $ execute conn "UPDATE aa_preparation_registry SET retired=TRUE,generation=generation+1,lease_token=NULL,lease_until=NULL,updated_at=clock_timestamp() WHERE chain_id=? AND paymaster=? AND sender=? AND preparation_id=?" (scopeTuple scope)
        pure $ Right ()
      Just why -> pure $ Left why

retirementReason :: Connection -> Scope -> Text -> IO (Maybe Text)
retirementReason conn (Scope chain paymaster sender identifier) router = do
  checks <- query conn
    "SELECT EXISTS (SELECT 1 FROM aa_preparation_registry WHERE sender=? AND lease_until>clock_timestamp()), EXISTS (SELECT 1 FROM aa_preparations WHERE sender=? AND lease_until>clock_timestamp()), EXISTS (SELECT 1 FROM aa_sponsorship_authorizations WHERE sender=? AND state IN ('reserved','signed','submitted')), EXISTS (SELECT 1 FROM aa_preparations WHERE sender=? AND preparation_id=? AND (diagnostic_chain_id IS DISTINCT FROM ? OR diagnostic_deployment IS DISTINCT FROM ? OR recovery_paymaster IS DISTINCT FROM ?)), EXISTS (SELECT 1 FROM aa_close_assistance g JOIN aa_sponsorship_authorizations a USING(digest) JOIN aa_user_operation_events e USING(digest) WHERE a.sender=? AND NOT g.verified AND a.state='settled' AND e.success), EXISTS (SELECT 1 FROM aa_sponsorship_authorizations a LEFT JOIN aa_user_operation_events e ON e.digest=a.digest WHERE a.sender=? AND a.state='settled' AND e.finalized_at IS NULL)"
    (sender,sender,sender,sender,identifier,chain,router,paymaster,sender,sender) :: IO [(Bool,Bool,Bool,Bool,Bool,Bool)]
  pure $ case checks of
    [(False,False,False,False,False,False)] -> Nothing
    [(_,_,_,True,_,_)] -> Just "RECOVERY_BINDING_UNRESOLVED"
    _ -> Just "RECOVERY_LIABILITY_PENDING"

saveChallenge :: Connection -> Scope -> Text -> Text -> Text -> IO ()
saveChallenge conn scope challenge owner message = do
  now <- floor <$> getPOSIXTime
  saveChallengeAt conn scope challenge owner message (now + 300)

saveChallengeAt :: Connection -> Scope -> Text -> Text -> Text -> Integer -> IO ()
saveChallengeAt conn (Scope chain paymaster sender identifier) challenge owner message expires = withTransaction conn $ do
  void $ execute_ conn "DELETE FROM aa_preparation_recovery_challenges WHERE expires_at<clock_timestamp()"
  void $ execute_ conn "DELETE FROM aa_preparation_recovery_sessions WHERE expires_at<clock_timestamp()"
  void $ execute conn "INSERT INTO aa_preparation_recovery_challenges(id,chain_id,paymaster,sender,preparation_id,owner,message,expires_at) VALUES (?,?,?,?,?,?,?,to_timestamp(?))" (challenge,chain,paymaster,sender,identifier,owner,message,expires)

readChallenge :: Connection -> Scope -> Text -> IO (Maybe (Text,Text))
readChallenge conn (Scope chain paymaster sender identifier) challenge = do
  rows <- query conn "SELECT owner,message FROM aa_preparation_recovery_challenges WHERE id=? AND chain_id=? AND paymaster=? AND sender=? AND preparation_id=? AND NOT consumed AND expires_at>clock_timestamp()" (challenge,chain,paymaster,sender,identifier)
  pure $ case rows of [value] -> Just value; _ -> Nothing

consumeChallenge :: Connection -> Scope -> Text -> Text -> Text -> IO Bool
consumeChallenge conn (Scope chain paymaster sender identifier) challenge owner tokenHash = withTransaction conn $ do
  changed <- execute conn "UPDATE aa_preparation_recovery_challenges SET consumed=TRUE WHERE id=? AND chain_id=? AND paymaster=? AND sender=? AND preparation_id=? AND owner=? AND NOT consumed AND expires_at>clock_timestamp()" (challenge,chain,paymaster,sender,identifier,owner)
  if changed /= 1 then pure False else do
    void $ execute conn "INSERT INTO aa_preparation_recovery_sessions(token_hash,chain_id,paymaster,sender,preparation_id,owner,expires_at) VALUES (?,?,?,?,?,?,clock_timestamp()+interval '15 minutes')" (tokenHash,chain,paymaster,sender,identifier,owner)
    pure True

sessionOwner :: Connection -> Scope -> Text -> IO (Maybe Text)
sessionOwner conn (Scope chain paymaster sender identifier) tokenHash = do
  rows <- query conn "SELECT owner FROM aa_preparation_recovery_sessions WHERE token_hash=? AND chain_id=? AND paymaster=? AND sender=? AND preparation_id=? AND expires_at>clock_timestamp()" (tokenHash,chain,paymaster,sender,identifier)
  pure $ case rows of [Only owner] -> Just owner; _ -> Nothing

-- A recovery session only selects the original namespace. Submission still
-- requires the exact sponsored hash and a real owner signature at the gateway.
sessionSubmissionClient :: Connection -> Text -> Text -> Text -> IO (Maybe Text)
sessionSubmissionClient conn paymaster tokenHash operationHash = do
  rows <- query conn
    "SELECT DISTINCT a.client_key FROM aa_preparation_recovery_sessions s JOIN aa_preparation_authorizations p USING(chain_id,paymaster,sender,preparation_id) JOIN aa_preparation_registry r USING(chain_id,paymaster,sender,preparation_id) JOIN aa_sponsorship_authorizations a ON a.digest=p.digest AND a.sender=s.sender AND a.owner=s.owner WHERE s.token_hash=? AND s.paymaster=? AND s.expires_at>clock_timestamp() AND NOT r.retired AND a.expected_user_operation_hash=? AND a.state IN ('signed','submitted') AND (SELECT count(*) FROM aa_preparations original WHERE original.sender=s.sender AND original.preparation_id=s.preparation_id)=1 LIMIT 2"
    (tokenHash,paymaster,operationHash)
  pure $ case rows of [Only client] -> Just client; _ -> Nothing
