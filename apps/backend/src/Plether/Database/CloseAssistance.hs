module Plether.Database.CloseAssistance
  ( CloseAssistanceReservation (..)
  , ensureCloseAssistanceSchema
  , closeAssistanceReservationReason
  , closeAssistanceReservationAllowed
  , insertCloseAssistanceReservation
  , getCloseAssistanceReservation
  , confirmCloseAssistance
  ) where

import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T
import Database.PostgreSQL.Simple

data CloseAssistanceReservation = CloseAssistanceReservation
  { carRouter :: Text
  , carAccount :: Text
  , carClientOrderId :: Text
  , carRequestHash :: Text
  , carLens :: Text
  , carAmountUsdc :: Integer
  } deriving stock (Eq, Show)

ensureCloseAssistanceSchema :: Connection -> IO ()
ensureCloseAssistanceSchema conn = do
  void $ execute_ conn
    "CREATE TABLE IF NOT EXISTS aa_close_assistance (\
    \digest VARCHAR(66) PRIMARY KEY REFERENCES aa_sponsorship_authorizations(digest),\
    \chain_id BIGINT NOT NULL DEFAULT 421614 CHECK(chain_id=421614),\
    \router VARCHAR(42) NOT NULL, account VARCHAR(42) NOT NULL, client_order_id VARCHAR(66) NOT NULL,\
    \request_hash VARCHAR(66) NOT NULL, lens VARCHAR(42) NOT NULL,\
    \amount_usdc BIGINT NOT NULL CHECK(amount_usdc BETWEEN 1 AND 200000),\
    \verified BOOLEAN NOT NULL DEFAULT FALSE, transaction_hash VARCHAR(66), block_number BIGINT,\
    \block_hash VARCHAR(66), deposit_log_index BIGINT, order_id BIGINT,\
    \CHECK(NOT verified OR (transaction_hash IS NOT NULL AND block_number IS NOT NULL\
    \ AND block_hash IS NOT NULL AND deposit_log_index IS NOT NULL AND order_id > 0)))"
  void $ execute_ conn
    "CREATE UNIQUE INDEX IF NOT EXISTS aa_close_assistance_verified_intent\
    \ ON aa_close_assistance(chain_id,router,account,client_order_id) WHERE verified"
  void $ execute_ conn
    "CREATE UNIQUE INDEX IF NOT EXISTS aa_close_assistance_verified_deposit\
    \ ON aa_close_assistance(chain_id,transaction_hash,deposit_log_index) WHERE verified"
  void $ execute_ conn
    "CREATE INDEX IF NOT EXISTS aa_close_assistance_account ON aa_close_assistance(chain_id,router,account)"

-- | Called under the existing global AA budget lock, in the authorization transaction.
-- Expiry is accepted only after the safe reconciler has marked the authorization expired.
closeAssistanceReservationAllowed :: Connection -> CloseAssistanceReservation -> IO Bool
closeAssistanceReservationAllowed conn r = do
  rows <- query conn
    "SELECT NOT EXISTS (SELECT 1 FROM aa_close_assistance g\
    \ JOIN aa_sponsorship_authorizations a USING(digest)\
    \ LEFT JOIN aa_user_operation_events e USING(digest)\
    \ WHERE g.router=? AND g.account=? AND (a.state IN ('reserved','signed','submitted')\
    \ OR (COALESCE(e.success,FALSE) AND (NOT g.verified OR g.client_order_id=?))))"
    (T.toLower $ carRouter r, T.toLower $ carAccount r, T.toLower $ carClientOrderId r) :: IO [Only Bool]
  pure $ rows == [Only True]

insertCloseAssistanceReservation :: Connection -> Text -> CloseAssistanceReservation -> IO ()
insertCloseAssistanceReservation conn digest r = void $ execute conn
  "INSERT INTO aa_close_assistance(digest,router,account,client_order_id,request_hash,lens,amount_usdc)\
  \ VALUES(?,?,?,?,?,?,?)"
  (T.toLower digest, T.toLower $ carRouter r, T.toLower $ carAccount r,
   T.toLower $ carClientOrderId r, T.toLower $ carRequestHash r, T.toLower $ carLens r, carAmountUsdc r)

getCloseAssistanceReservation :: Connection -> Text -> IO (Maybe CloseAssistanceReservation)
getCloseAssistanceReservation conn digest = do
  rows <- query conn
    "SELECT router,account,client_order_id,request_hash,lens,amount_usdc FROM aa_close_assistance WHERE digest=?"
    (Only $ T.toLower digest) :: IO [(Text,Text,Text,Text,Text,Integer)]
  pure $ case rows of
    [(router,account,clientId,requestHash,lens,amount)] -> Just $ CloseAssistanceReservation router account clientId requestHash lens amount
    _ -> Nothing

confirmCloseAssistance :: Connection -> Text -> Text -> Integer -> Text -> Integer -> Integer -> IO Bool
confirmCloseAssistance conn digest tx blockNumber blockHash depositIndex orderId = do
  count <- execute conn
    "UPDATE aa_close_assistance SET verified=TRUE,transaction_hash=?,block_number=?,block_hash=?,deposit_log_index=?,order_id=?\
    \ WHERE digest=? AND (NOT verified OR (transaction_hash=? AND block_hash=? AND deposit_log_index=? AND order_id=?))"
    (T.toLower tx,blockNumber,T.toLower blockHash,depositIndex,orderId,T.toLower digest,
     T.toLower tx,T.toLower blockHash,depositIndex,orderId)
  pure $ count == 1

-- Keep the compatibility predicate, while new callers expose distinct recovery reasons.
closeAssistanceReservationReason :: Connection -> CloseAssistanceReservation -> IO (Maybe Text)
closeAssistanceReservationReason conn r = do
  rows <- query conn
    "SELECT CASE WHEN BOOL_OR(COALESCE(e.success,FALSE) AND g.verified AND g.client_order_id=?)\
    \ THEN 'INTENT_ALREADY_COMMITTED' ELSE 'ASSISTANCE_RESERVATION_PENDING' END\
    \ FROM aa_close_assistance g JOIN aa_sponsorship_authorizations a USING(digest)\
    \ LEFT JOIN aa_user_operation_events e USING(digest) WHERE g.router=? AND g.account=?\
    \ AND (a.state IN ('reserved','signed','submitted') OR (COALESCE(e.success,FALSE) AND (NOT g.verified OR g.client_order_id=?)))\
    \ HAVING COUNT(*) > 0"
    (T.toLower $ carClientOrderId r,T.toLower $ carRouter r,T.toLower $ carAccount r,T.toLower $ carClientOrderId r) :: IO [Only Text]
  case rows of
    [] -> pure Nothing
    [Only reason] -> pure $ Just reason
    _ -> fail "Ambiguous assistance reservation state"
