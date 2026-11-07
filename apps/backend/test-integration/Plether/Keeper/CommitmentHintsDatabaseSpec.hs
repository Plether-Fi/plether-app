module Plether.Keeper.CommitmentHintsDatabaseSpec (commitmentHintsDatabaseSpec) where

import Control.Exception (bracket,finally)
import Control.Monad (void,forM_)
import Data.Aeson (object,(.=))
import qualified Data.ByteString.Char8 as B
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types (Identifier(..))
import Plether.Database.CommitmentHints
import Test.Hspec

commitmentHintsDatabaseSpec :: Text -> Spec
commitmentHintsDatabaseSpec url = describe "Durable advisory commitment hints" $ do
  it "deduplicates enqueue and repairs a lost notification from the old submitted journal" $ fixture url $ \c -> do
    seed c hashOne sender paymaster "submitted"
    recoverSubmittedHints c router paymaster (Just hashOne) `shouldReturn` 1
    recoverSubmittedHints c router paymaster (Just hashOne) `shouldReturn` 0
    query_ c "SELECT count(*)::bigint FROM perps_commitment_hints" `shouldReturn` [Only (1::Int64)]
    void $ execute_ c "DELETE FROM perps_commitment_hints"
    -- Simulate restart with no in-memory signal and an OLD submitted_at.
    void $ execute_ c "UPDATE aa_sponsorship_authorizations SET submitted_at=clock_timestamp()-interval '1 hour'"
    recoverSubmittedHints c router paymaster Nothing `shouldReturn` 1
  it "ignores unsigned, wrong-paymaster, unbound-chain and expired authorization candidates" $ fixture url $ \c -> do
    seed c hashOne sender paymaster "signed"
    seed c hashTwo sender stranger "submitted"
    seed c hashThree sender paymaster "submitted"
    void $ execute c "DELETE FROM aa_preparation_authorizations WHERE digest=?" (Only hashThree)
    recoverSubmittedHints c router paymaster Nothing `shouldReturn` 0
    void $ execute_ c "UPDATE aa_sponsorship_authorizations SET state='submitted',valid_until=extract(epoch FROM clock_timestamp())::bigint-100"
    recoverSubmittedHints c router paymaster Nothing `shouldReturn` 0
  it "fences stale workers after a lost lease and survives a worker restart" $ fixture url $ \c -> do
    claim <- oneClaim c
    claimCommitmentHints c router `shouldReturn` []
    void $ execute_ c "UPDATE perps_commitment_hints SET lease_until=clock_timestamp()-interval '1 second'"
    [newer] <- claimCommitmentHints c router
    hintGeneration newer `shouldSatisfy` (>hintGeneration claim)
    record c claim `shouldReturn` False
    record c newer `shouldReturn` True
  it "clears provisional data on reorg and only supplies recently reverified reveal candidates" $ fixture url $ \c -> do
    claim <- oneClaim c
    record c claim `shouldReturn` True
    loadProvisionalRevealCandidates c router `shouldReturn` [(1000,20)]
    loadProvisionalRevealCandidates c stranger `shouldReturn` []
    retryCommitmentHint c router claim True
    loadProvisionalRevealCandidates c router `shouldReturn` []
    query_ c "SELECT transaction_hash,verified_at::text FROM perps_commitment_hints" `shouldReturn` [(Nothing::Maybe Text,Nothing::Maybe Text)]
  it "does not prewarm stale or expired evidence and never changes authorization state" $ fixture url $ \c -> do
    claim <- oneClaim c
    record c claim `shouldReturn` True
    void $ execute_ c "UPDATE perps_commitment_hints SET verified_at=clock_timestamp()-interval '4 seconds'"
    loadProvisionalRevealCandidates c router `shouldReturn` []
    void $ execute_ c "UPDATE perps_commitment_hints SET verified_at=clock_timestamp(),expires_at=clock_timestamp()-interval '1 second'"
    loadProvisionalRevealCandidates c router `shouldReturn` []
    claimCommitmentHints c router `shouldReturn` []
    query_ c "SELECT state FROM aa_sponsorship_authorizations" `shouldReturn` [Only ("submitted"::Text)]
  it "retires an optimization once canonical execution finishes, while missing canonical rows keep tracking" $ fixture url $ \c -> do
    claim <- oneClaim c
    record c claim `shouldReturn` True
    void $ execute_ c "UPDATE perps_commitment_hints SET next_check=clock_timestamp()-interval '1 second'"
    [continued] <- claimCommitmentHints c router
    record c continued `shouldReturn` True
    void $ execute c "INSERT INTO perps_keeper_orders(order_router,order_id,account,status) VALUES(?,20,?,'executed')" (router,sender)
    claimCommitmentHints c router `shouldReturn` []
    loadProvisionalRevealCandidates c router `shouldReturn` []
    query_ c "SELECT status FROM perps_keeper_orders" `shouldReturn` [Only ("executed"::Text)]
  it "enforces the global active-work cap even when recovery runs repeatedly" $ fixture url $ \c -> do
    forM_ [1..140::Int] $ \i -> seed c ("0x"<>T.justifyRight 64 '0' (T.pack $ show i)) ("0x"<>T.justifyRight 40 '0' (T.pack $ show i)) paymaster "submitted"
    forM_ [1..8::Int] $ \_ -> void $ recoverSubmittedHints c router paymaster Nothing
    query_ c "SELECT count(*)::bigint FROM perps_commitment_hints" `shouldReturn` [Only (128::Int64)]
  it "caps each account independently and keeps fast tracking active between due checks" $ fixture url $ \c -> do
    forM_ [1..8::Int] $ \i -> seed c ("0x"<>T.justifyRight 64 '0' (T.pack $ show i)) sender paymaster "submitted"
    recoverSubmittedHints c router paymaster Nothing `shouldReturn` 5
    recoverSubmittedHints c router paymaster Nothing `shouldReturn` 0
    hasActiveCommitmentHints c router `shouldReturn` True
    void $ execute_ c "UPDATE perps_commitment_hints SET next_check=clock_timestamp()+interval '1 second'"
    claimCommitmentHints c router `shouldReturn` []
    hasActiveCommitmentHints c router `shouldReturn` True
    void $ execute_ c "UPDATE perps_commitment_hints SET expires_at=clock_timestamp()-interval '1 second'"
    hasActiveCommitmentHints c router `shouldReturn` False

record :: Connection -> HintClaim -> IO Bool
record c claim=withTransaction c $ recordProvisionalCommitment c router claim hashTwo 100 hashThree 20 1000 101 3
oneClaim :: Connection -> IO HintClaim
oneClaim c=do
  seed c hashOne sender paymaster "submitted"
  void $ recoverSubmittedHints c router paymaster Nothing
  [claim] <- claimCommitmentHints c router
  pure claim

-- Each test uses a fresh PostgreSQL schema, not application or production data.
fixture :: Text -> (Connection -> IO a) -> IO a
fixture url action=bracket (connectPostgreSQL $ B.pack $ T.unpack url) close $ \c -> do
  [Only identifier] <- query_ c "SELECT pg_backend_pid()" :: IO [Only Int]
  let schema=Identifier $ "commitment_hints_test_"<>T.pack(show identifier)
  void $ execute c "CREATE SCHEMA ?" (Only schema)
  finally (do
    void $ execute c "SET search_path TO ?" (Only schema)
    void $ execute_ c "CREATE TABLE aa_sponsorship_authorizations(digest TEXT PRIMARY KEY,expected_user_operation_hash TEXT UNIQUE,sender TEXT,valid_until BIGINT,state TEXT,submitted_at TIMESTAMPTZ DEFAULT clock_timestamp(),operation JSONB)"
    void $ execute_ c "CREATE TABLE aa_preparation_authorizations(chain_id BIGINT,paymaster TEXT,sender TEXT,digest TEXT)"
    void $ execute_ c "CREATE TABLE perps_keeper_orders(order_router TEXT,order_id BIGINT,account TEXT,status TEXT)"
    ensureCommitmentHintSchema c
    action c) (void $ execute c "DROP SCHEMA ? CASCADE" $ Only schema)

seed :: Connection -> Text -> Text -> Text -> Text -> IO ()
seed c hash account sponsor state=do
  void $ execute c "INSERT INTO aa_sponsorship_authorizations(digest,expected_user_operation_hash,sender,valid_until,state,operation) VALUES(?,?,?,extract(epoch FROM clock_timestamp())::bigint+120,?,?)"
    (hash,hash,account,state,object["paymaster".=sponsor])
  void $ execute c "INSERT INTO aa_preparation_authorizations(chain_id,paymaster,sender,digest) VALUES(421614,?,?,?)" (sponsor,account,hash)

router,sender,paymaster,stranger,hashOne,hashTwo,hashThree :: Text
router="0x"<>T.replicate 40 "1"
sender="0x"<>T.replicate 40 "2"
paymaster="0x"<>T.replicate 40 "3"
stranger="0x"<>T.replicate 40 "4"
hashOne="0x"<>T.replicate 64 "5"
hashTwo="0x"<>T.replicate 64 "6"
hashThree="0x"<>T.replicate 64 "7"
