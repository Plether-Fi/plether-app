-- Advisory work only. This module never changes sponsorship, keeper orders or
-- index cursors. Losing any row may cost latency, never authorization or funds.
module Plether.Database.CommitmentHints
  ( HintClaim(..), ensureCommitmentHintSchema, recoverSubmittedHints
  , claimCommitmentHints, hasActiveCommitmentHints, retryCommitmentHint, completeCommitmentHint
  , recordProvisionalCommitment, loadProvisionalRevealCandidates
  ) where

import Control.Monad (void)
import Data.Aeson (Value)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow (FromRow(..), field)

data HintClaim = HintClaim
  { hintHash :: Text, hintSender :: Text, hintTransaction :: Maybe Text
  , hintAttempts :: Int, hintGeneration :: Integer, hintLastHead :: Integer
  , hintWakeStage :: Int
  , hintSubmissionAgeMs :: Integer
  , hintOperation :: Value
  } deriving stock (Eq, Show)
instance FromRow HintClaim where
  fromRow = HintClaim <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

-- Startup invokes this off the API request path and retries on failure.
ensureCommitmentHintSchema :: Connection -> IO ()
ensureCommitmentHintSchema conn = bounded conn $ do
  void $ execute_ conn
    "CREATE TABLE IF NOT EXISTS perps_commitment_hints (\
    \chain_id BIGINT NOT NULL CHECK(chain_id=421614),router TEXT NOT NULL,\
    \operation_hash TEXT NOT NULL CHECK(operation_hash ~ '^0x[0-9a-f]{64}$'),\
    \sender TEXT NOT NULL CHECK(sender ~ '^0x[0-9a-f]{40}$'),\
    \state TEXT NOT NULL DEFAULT 'tracking' CHECK(state IN ('tracking','provisional','complete')),\
    \created_at TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp(),expires_at TIMESTAMPTZ NOT NULL,\
    \next_check TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp(),lease_until TIMESTAMPTZ,\
    \generation BIGINT NOT NULL DEFAULT 0,attempts INTEGER NOT NULL DEFAULT 0,\
    \transaction_hash TEXT,commit_event_block BIGINT,commit_block_hash TEXT,\
    \order_id BIGINT,commit_time BIGINT,verified_at TIMESTAMPTZ,notified_head BIGINT,wake_stage INTEGER NOT NULL DEFAULT 0,\
    \PRIMARY KEY(chain_id,router,operation_hash),\
    \CHECK(router ~ '^0x[0-9a-f]{40}$'),\
    \CHECK((state='provisional')=(verified_at IS NOT NULL)),\
    \CHECK(state<>'provisional' OR (order_id>0 AND commit_time>0 AND transaction_hash IS NOT NULL AND commit_event_block IS NOT NULL AND commit_block_hash IS NOT NULL)))"
  void $ execute_ conn "CREATE INDEX IF NOT EXISTS perps_commitment_hints_due ON perps_commitment_hints(chain_id,router,next_check) WHERE state<>'complete'"

bounded :: Connection -> IO a -> IO a
bounded conn action = withTransaction conn $ do
  void $ execute_ conn "SET LOCAL statement_timeout='1500ms'"
  void $ execute_ conn "SET LOCAL lock_timeout='200ms'"
  action

-- The existing submitted journal is the durable source of truth. An anti-join
-- repairs lost enqueue/NOTIFY deliveries and API crashes, including old first-
-- submission timestamps. No high-water cursor or browser assertion is trusted.
-- Admission is serialized separately from financial locks and globally capped.
recoverSubmittedHints :: Connection -> Text -> Text -> Maybe Text -> IO Int64
recoverSubmittedHints conn router paymaster preferred = bounded conn $ do
  void (query_ conn "SELECT 1::integer FROM pg_advisory_xact_lock(421614,17597)" :: IO [Only Int])
  void $ execute_ conn "DELETE FROM perps_commitment_hints WHERE expires_at<clock_timestamp()-interval '5 minutes'"
  capacity <- query conn "SELECT greatest(0,128-count(*))::integer FROM perps_commitment_hints WHERE chain_id=421614 AND router=? AND state<>'complete' AND expires_at>clock_timestamp()" (Only $ T.toLower router) :: IO [Only Int]
  let available = case capacity of [Only n] -> min 32 n; _ -> 0
  execute conn
    "WITH candidates AS (SELECT a.*,row_number() OVER(PARTITION BY a.sender ORDER BY (a.expected_user_operation_hash=?) DESC NULLS LAST,a.submitted_at,a.digest) AS sender_rank, \
    \(SELECT count(*) FROM perps_commitment_hints h WHERE h.chain_id=421614 AND h.router=? AND h.sender=a.sender AND h.state<>'complete' AND h.expires_at>clock_timestamp()) AS sender_active \
    \FROM aa_sponsorship_authorizations a \
    \WHERE a.state='submitted' AND a.expected_user_operation_hash IS NOT NULL \
    \AND a.valid_until>extract(epoch FROM clock_timestamp())-60 \
    \AND lower(a.operation->>'paymaster')=? \
    \AND EXISTS(SELECT 1 FROM aa_preparation_authorizations p WHERE p.digest=a.digest AND p.sender=a.sender AND p.chain_id=421614 AND p.paymaster=?) \
    \AND NOT EXISTS(SELECT 1 FROM perps_commitment_hints h WHERE h.chain_id=421614 AND h.router=? AND h.operation_hash=a.expected_user_operation_hash) \
    \) INSERT INTO perps_commitment_hints(chain_id,router,operation_hash,sender,expires_at) \
    \SELECT 421614,?,a.expected_user_operation_hash,a.sender,least(to_timestamp(a.valid_until+60),clock_timestamp()+interval '3 minutes') FROM candidates a \
    \WHERE a.sender_rank+a.sender_active<=5 ORDER BY (a.expected_user_operation_hash=?) DESC NULLS LAST,a.submitted_at,a.digest LIMIT ? \
    \ON CONFLICT DO NOTHING"
    (preferred,T.toLower router,T.toLower paymaster,T.toLower paymaster,T.toLower router,T.toLower router,preferred,available)

hasActiveCommitmentHints :: Connection -> Text -> IO Bool
hasActiveCommitmentHints conn router = do
  rows <- query conn "SELECT EXISTS(SELECT 1 FROM perps_commitment_hints WHERE chain_id=421614 AND router=? AND state<>'complete' AND expires_at>clock_timestamp())"
    (Only $ T.toLower router) :: IO [Only Bool]
  pure $ rows==[Only True]

-- A generation fences slow/stale workers; locks end before any network call.
claimCommitmentHints :: Connection -> Text -> IO [HintClaim]
claimCommitmentHints conn router = bounded conn $ do
  void $ execute conn
    "UPDATE perps_commitment_hints h SET state='complete',verified_at=NULL,lease_until=NULL WHERE h.chain_id=421614 AND h.router=? AND h.state='provisional' AND EXISTS(SELECT 1 FROM perps_keeper_orders k WHERE k.order_router=h.router AND k.order_id=h.order_id AND k.account=h.sender AND k.status IN ('executed','failed'))"
    (Only $ T.toLower router)
  void $ execute conn
    "UPDATE perps_commitment_hints SET state='complete',verified_at=NULL,lease_until=NULL WHERE chain_id=421614 AND router=? AND expires_at<=clock_timestamp() AND state<>'complete'"
    (Only $ T.toLower router)
  query conn
    "WITH due AS(SELECT operation_hash FROM perps_commitment_hints WHERE chain_id=421614 AND router=? AND state<>'complete' AND expires_at>clock_timestamp() AND next_check<=clock_timestamp() AND (lease_until IS NULL OR lease_until<clock_timestamp()) ORDER BY next_check,created_at LIMIT 4 FOR UPDATE SKIP LOCKED) \
    \UPDATE perps_commitment_hints h SET generation=h.generation+1,lease_until=clock_timestamp()+interval '15 seconds',attempts=h.attempts+1 \
    \FROM due WHERE h.chain_id=421614 AND h.router=? AND h.operation_hash=due.operation_hash \
    \RETURNING h.operation_hash,h.sender,h.transaction_hash,h.attempts,h.generation,coalesce(h.notified_head,-1),h.wake_stage,coalesce((SELECT greatest(0,extract(epoch FROM (clock_timestamp()-a.submitted_at))*1000)::bigint FROM aa_sponsorship_authorizations a WHERE a.expected_user_operation_hash=h.operation_hash),0),coalesce((SELECT a.operation FROM aa_sponsorship_authorizations a WHERE a.expected_user_operation_hash=h.operation_hash),'null'::jsonb)"
    (T.toLower router,T.toLower router)

retryCommitmentHint :: Connection -> Text -> HintClaim -> Bool -> IO ()
retryCommitmentHint conn router claim clearEvidence = bounded conn $ void $ execute conn
  "UPDATE perps_commitment_hints SET state='tracking',verified_at=NULL,transaction_hash=CASE WHEN ? THEN NULL ELSE transaction_hash END,order_id=NULL,commit_time=NULL,commit_event_block=NULL,commit_block_hash=NULL,notified_head=NULL,wake_stage=0,lease_until=NULL,next_check=clock_timestamp()+CASE WHEN created_at>clock_timestamp()-interval '10 seconds' THEN interval '250 milliseconds' ELSE interval '1 second' END WHERE chain_id=421614 AND router=? AND operation_hash=? AND generation=?"
  (clearEvidence,T.toLower router,hintHash claim,hintGeneration claim)

completeCommitmentHint :: Connection -> Text -> HintClaim -> IO ()
completeCommitmentHint conn router claim = bounded conn $ void $ execute conn
  "UPDATE perps_commitment_hints SET state='complete',verified_at=NULL,lease_until=NULL WHERE chain_id=421614 AND router=? AND operation_hash=? AND generation=?"
  (T.toLower router,hintHash claim,hintGeneration claim)

-- Notify only after this fenced evidence write. The caller emits the optional
-- wake in the SAME transaction; an old worker can never publish a newer claim.
recordProvisionalCommitment :: Connection -> Text -> HintClaim -> Text -> Integer -> Text -> Integer -> Integer -> Integer -> Int -> IO Bool
recordProvisionalCommitment conn router claim tx block hash order time headNumber stage = do
  rows <- query conn
    "UPDATE perps_commitment_hints SET state='provisional',transaction_hash=?,commit_event_block=?,commit_block_hash=?,order_id=?,commit_time=?,verified_at=clock_timestamp(),notified_head=?,wake_stage=?,lease_until=NULL,next_check=clock_timestamp()+CASE WHEN created_at>clock_timestamp()-interval '10 seconds' THEN interval '250 milliseconds' ELSE interval '1 second' END WHERE chain_id=421614 AND router=? AND operation_hash=? AND generation=? AND expires_at>clock_timestamp() RETURNING operation_hash"
    (T.toLower tx,block,T.toLower hash,order,time,headNumber,stage,T.toLower router,hintHash claim,hintGeneration claim) :: IO [Only Text]
  pure $ not (null rows)

-- Signed Pyth payload prewarming only; this row is never execution authority.
-- Freshness bounds the effect of a reorg or unavailable revalidation provider.
loadProvisionalRevealCandidates :: Connection -> Text -> IO [(Integer,Integer)]
loadProvisionalRevealCandidates conn router = query conn
  "SELECT commit_time,order_id FROM perps_commitment_hints WHERE chain_id=421614 AND router=? AND state='provisional' AND verified_at>clock_timestamp()-interval '3 seconds' AND expires_at>clock_timestamp() ORDER BY commit_time,order_id LIMIT 20"
  (Only $ T.toLower router)
