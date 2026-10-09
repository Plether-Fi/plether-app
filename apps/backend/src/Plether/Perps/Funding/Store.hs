{-# LANGUAGE OverloadedStrings #-}

-- | Durable bridge funding records. Quotes and intent bindings are immutable;
-- only the reconciliation evidence in an intent may be updated.
module Plether.Perps.Funding.Store
  ( ensureFundingSchema
  , insertQuote
  , findQuote
  , createIntent
  , getIntent
  , findIntentByIdempotencyKey
  , updateIntent
  , listPendingIntents
  , listPendingIntentsForDeployment
  , getActiveTransaction
  , withFundingWorkerLock
  , setWorkerReadiness
  , isWorkerReady
  ) where

import Control.Exception (bracket_)
import Control.Monad (unless, void)
import Data.Aeson (Result (..), Value (..), object, encode, fromJSON)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isAlphaNum, isAscii)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Database.PostgreSQL.Simple

-- Keep these definitions in sync with config/migrations/perps-funding-v1.sql.
-- A transaction-scoped schema lock also makes concurrent process startup safe.
ensureFundingSchema :: Connection -> IO ()
ensureFundingSchema conn = withTransaction conn $ do
  void (query_ conn "SELECT 1::int FROM pg_advisory_xact_lock(20261008, 3)" :: IO [Only Int])
  void $ execute_ conn
    "CREATE TABLE IF NOT EXISTS perps_funding_quotes (\
    \id TEXT PRIMARY KEY CHECK (id ~ '^0x[0-9a-f]{64}$'),\
    \payload JSONB NOT NULL CHECK (jsonb_typeof(payload) = 'object' AND payload->>'quoteId' IS NOT DISTINCT FROM id),\
    \expires_at TIMESTAMPTZ NOT NULL,\
    \created_at TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp())"
  void $ execute_ conn
    "CREATE TABLE IF NOT EXISTS perps_funding_intents (\
    \id TEXT PRIMARY KEY CHECK (id ~ '^0x[0-9a-f]{64}$'),\
    \idempotency_key TEXT NOT NULL UNIQUE CHECK (idempotency_key ~ '^[-a-zA-Z0-9_:]{16,128}$'),\
    \quote_id TEXT NOT NULL UNIQUE REFERENCES perps_funding_quotes(id),\
    \creation_payload JSONB NOT NULL CHECK (jsonb_typeof(creation_payload) = 'object'),\
    \payload JSONB NOT NULL CHECK (jsonb_typeof(payload) = 'object' AND payload->>'intentId' IS NOT DISTINCT FROM id AND payload->>'quoteId' IS NOT DISTINCT FROM quote_id),\
    \status TEXT NOT NULL CHECK (status IN ('awaiting-source','bridging','received','depositing','confirmed','retryable','failed')),\
    \created_at TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp(),\
    \updated_at TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp(),\
    \last_polled_at TIMESTAMPTZ,\
    \CHECK (payload->>'status' IS NOT DISTINCT FROM status))"
  void $ execute_ conn
    "CREATE INDEX IF NOT EXISTS perps_funding_intents_poll_idx ON perps_funding_intents(last_polled_at NULLS FIRST, created_at, id)"
  void $ execute_ conn
    "CREATE TABLE IF NOT EXISTS perps_funding_worker_readiness (\
    \release_id TEXT NOT NULL CHECK (length(release_id)>0),\
    \chain_id NUMERIC(78,0) NOT NULL CHECK (chain_id>0),\
    \ready BOOLEAN NOT NULL,\
    \last_seen TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp(),\
    \PRIMARY KEY (release_id,chain_id))"

-- | Insert a quote once. A byte/key-order-independent retry is harmless, but a
-- reused identifier cannot change the route or extend the quote's lifetime.
insertQuote :: Connection -> Text -> Value -> Integer -> IO ()
insertQuote conn identifier payload expires = do
  unless (textField "quoteId" payload == Just identifier && numberField "expiresAt" payload == Just expires) $
    fail "Funding quote identifier or expiry does not match its payload"
  rows <- query conn
    "INSERT INTO perps_funding_quotes(id,payload,expires_at) VALUES (?,?::jsonb,to_timestamp(?)) \
    \ON CONFLICT (id) DO UPDATE SET id=EXCLUDED.id \
    \WHERE perps_funding_quotes.payload=EXCLUDED.payload AND perps_funding_quotes.expires_at=EXCLUDED.expires_at RETURNING id"
    (identifier, jsonText payload, expires) :: IO [Only Text]
  unless (rows == [Only identifier]) $ fail "Funding quote identifier conflict"

findQuote :: Connection -> Text -> IO (Maybe Value)
findQuote conn identifier = singleValue <$> (query conn
  "SELECT payload FROM perps_funding_quotes WHERE id=?" (Only identifier) :: IO [Only Value])

findIntentByIdempotencyKey :: Connection -> Text -> IO (Maybe Value)
findIntentByIdempotencyKey conn key = singleValue <$> (query conn
  "SELECT payload FROM perps_funding_intents WHERE idempotency_key=?" (Only key) :: IO [Only Value])

-- | Claim a quote exactly once. Idempotency compares the original request, not
-- mutable worker evidence. Server-generated intentId/createdAt/updatedAt may
-- differ on a retry; all other request fields must remain semantically equal.
-- A matching retry returns current state even if the quote has since expired.
createIntent :: Connection -> Text -> Text -> Text -> Value -> IO (Either Text Value)
createIntent conn key identifier quoteId payload
  | T.length key < 16 || T.length key > 128 || not (T.all validKeyCharacter key) = pure $ Left "INVALID_IDEMPOTENCY_KEY"
  | textField "intentId" payload /= Just identifier || textField "quoteId" payload /= Just quoteId =
      pure $ Left "INVALID_INTENT_BINDING"
  | textField "status" payload /= Just "awaiting-source" = pure $ Left "INVALID_INITIAL_STATUS"
  | otherwise = withTransaction conn $ do
      -- Serialize a key before taking a quote lock. The unique constraints are
      -- a second line of defense, including accidental intent identifier reuse.
      void (query conn "SELECT 1::int FROM pg_advisory_xact_lock(20261009, hashtext(?))" (Only key) :: IO [Only Int])
      existing <- query conn
        "SELECT quote_id,creation_payload,payload FROM perps_funding_intents WHERE idempotency_key=? FOR UPDATE"
        (Only key) :: IO [(Text, Value, Value)]
      case existing of
        [(storedQuote, original, current)]
          | storedQuote == quoteId && retryPayload original == retryPayload payload -> pure $ Right current
          | otherwise -> pure $ Left "IDEMPOTENCY_CONFLICT"
        [] -> claimQuote
        _ -> fail "Ambiguous funding idempotency key"
  where
    validKeyCharacter character = isAscii character && (isAlphaNum character || character `elem` ("-_:" :: String))
    claimQuote = do
      quotes <- query conn
        "SELECT payload,expires_at>clock_timestamp() FROM perps_funding_quotes WHERE id=? FOR UPDATE"
        (Only quoteId) :: IO [(Value, Bool)]
      case quotes of
        [] -> pure $ Left "QUOTE_NOT_FOUND"
        [(quoted, fresh)] -> do
          claimed <- query conn "SELECT id FROM perps_funding_intents WHERE quote_id=?" (Only quoteId) :: IO [Only Text]
          if not (null claimed) then pure $ Left "QUOTE_ALREADY_USED"
          else if not fresh then pure $ Left "QUOTE_EXPIRED"
          else if not (matchesQuote quoted payload) then pure $ Left "QUOTE_PAYLOAD_CONFLICT"
          else do
            updated <- payloadUpdatedAt payload
            inserted <- query conn
              "INSERT INTO perps_funding_intents(id,idempotency_key,quote_id,creation_payload,payload,status,updated_at) \
              \SELECT ?,?,?,?::jsonb,?::jsonb,'awaiting-source',COALESCE(?,clock_timestamp()) \
              \FROM perps_funding_quotes WHERE id=? AND expires_at>clock_timestamp() \
              \ON CONFLICT DO NOTHING RETURNING payload"
              (identifier, key, quoteId, jsonText payload, jsonText payload, updated, quoteId) :: IO [Only Value]
            case singleValue inserted of
              Just created -> pure $ Right created
              Nothing -> do
                -- The quote may have expired while waiting for its row lock.
                -- Recheck at insertion, not just at the initial SELECT.
                stillFresh <- query conn "SELECT expires_at>clock_timestamp() FROM perps_funding_quotes WHERE id=?"
                  (Only quoteId) :: IO [Only Bool]
                pure $ Left $ if stillFresh == [Only True] then "INTENT_ID_CONFLICT" else "QUOTE_EXPIRED"
        _ -> fail "Ambiguous funding quote"

getIntent :: Connection -> Text -> IO (Maybe Value)
getIntent conn identifier = singleValue <$> (query conn
  "SELECT payload FROM perps_funding_intents WHERE id=?" (Only identifier) :: IO [Only Value])

-- | Replace reconciliation state, preserving the complete original route,
-- beneficiary, transaction request and release snapshot. Only a small explicit
-- set of server-owned evidence fields can be added, removed or changed.
updateIntent :: Connection -> Text -> Value -> IO ()
updateIntent conn identifier payload = withTransaction conn $ do
  stored <- query conn "SELECT payload FROM perps_funding_intents WHERE id=? FOR UPDATE" (Only identifier) :: IO [Only Value]
  case stored of
    [Only original] -> do
      unless (immutablePayload original == immutablePayload payload && textField "intentId" payload == Just identifier) $
        fail "Cannot change an immutable funding intent binding"
      status <- maybe (fail "Funding intent status is missing") pure (textField "status" payload)
      updated <- payloadUpdatedAt payload
      void $ execute conn
        "UPDATE perps_funding_intents SET payload=?::jsonb,status=?,updated_at=COALESCE(?,clock_timestamp()) WHERE id=?"
        (jsonText payload, status, updated, identifier)
    [] -> fail "Funding intent does not exist"
    _ -> fail "Ambiguous funding intent"

-- | Reconcile every durable intent, including confirmed (reorgs / late split
-- transfers) and failed (late arrivals). Poll timestamps rotate the bounded
-- batch so unchanged or confirmed records cannot starve later records. Call
-- this on the connection protected by 'withFundingWorkerLock'.
listPendingIntents :: Connection -> Int -> IO [Value]
listPendingIntents conn = listPendingIntentsForDeployment conn (object [])

-- Filter before LIMIT so historical releases cannot suppress current readiness.
listPendingIntentsForDeployment :: Connection -> Value -> Int -> IO [Value]
listPendingIntentsForDeployment conn binding limit
  | limit <= 0 = pure []
  | otherwise = map fromOnly <$> (query conn
      "WITH candidates AS (SELECT id FROM perps_funding_intents WHERE payload @> ?::jsonb \
      \ORDER BY last_polled_at NULLS FIRST,created_at,id LIMIT ? FOR UPDATE SKIP LOCKED) \
      \UPDATE perps_funding_intents AS intents SET last_polled_at=clock_timestamp() \
      \FROM candidates WHERE intents.id=candidates.id RETURNING intents.payload"
      (jsonText binding, limit) :: IO [Only Value])

-- | Resume a persisted signed transaction before allocating another nonce.
-- This searches all intents rather than the current bounded polling batch.
-- Call while holding the global worker lock, and persist the signed transaction
-- before broadcasting it. Clear it only after reconciliation permits progress.
getActiveTransaction :: Connection -> IO (Maybe Value)
getActiveTransaction conn = singleValue <$> (query_ conn
  "SELECT payload FROM perps_funding_intents \
  \WHERE jsonb_typeof(payload->'signedRawTransaction')='string' \
  \ORDER BY updated_at,id LIMIT 1" :: IO [Only Value])

-- | One destination signer/worker at a time, across processes. This is a session
-- lock rather than a long SQL transaction: retain the same dedicated connection
-- through the whole action, including signing and broadcast, and do not share
-- it concurrently. PostgreSQL releases the lock if that connection is lost.
withFundingWorkerLock :: Connection -> IO a -> IO a
withFundingWorkerLock conn = bracket_ acquire release
  where
    acquire = void (query_ conn "SELECT 1::int FROM pg_advisory_lock(20261008, 4)" :: IO [Only Int])
    release = void (query_ conn "SELECT pg_advisory_unlock(20261008, 4)" :: IO [Only Bool])

-- | Publish readiness only after checking the configured release and signing
-- capacity. A stopped or disconnected executor automatically becomes unready.
setWorkerReadiness :: Connection -> Text -> Integer -> Bool -> IO ()
setWorkerReadiness conn releaseId chainId ready = void $ execute conn
  "INSERT INTO perps_funding_worker_readiness(release_id,chain_id,ready,last_seen) VALUES (?,?,?,clock_timestamp()) \
  \ON CONFLICT (release_id,chain_id) DO UPDATE SET ready=EXCLUDED.ready,last_seen=EXCLUDED.last_seen"
  (releaseId, chainId, ready)

-- | Config and quote creation fail closed without a fresh executor heartbeat
-- for the exact release and destination chain. Trust the database clock only.
isWorkerReady :: Connection -> Text -> Integer -> IO Bool
isWorkerReady conn releaseId chainId = do
  rows <- query conn
    "SELECT ready AND last_seen>clock_timestamp()-interval '60 seconds' \
    \FROM perps_funding_worker_readiness WHERE release_id=? AND chain_id=?"
    (releaseId, chainId) :: IO [Only Bool]
  pure $ rows == [Only True]

jsonText :: Value -> Text
jsonText = TE.decodeUtf8 . LBS.toStrict . encode

singleValue :: [Only Value] -> Maybe Value
singleValue [] = Nothing
singleValue [Only value] = Just value
singleValue _ = error "Funding store query returned more than one row for a unique key"

textField :: Text -> Value -> Maybe Text
textField key (Object fields) = case KM.lookup (Key.fromText key) fields of
  Just (String value) -> Just value
  _ -> Nothing
textField _ _ = Nothing

numberField :: Text -> Value -> Maybe Integer
numberField key (Object fields) = KM.lookup (Key.fromText key) fields >>= \value -> case fromJSON value of
  Success n -> Just n
  Error _ -> Nothing
numberField _ _ = Nothing

retryPayload :: Value -> Value
retryPayload = without ["intentId", "createdAt", "updatedAt"]

immutablePayload :: Value -> Value
immutablePayload = without
  [ "status", "updatedAt", "sourceTxHash", "depositTxHash", "depositBlockNumber"
  , "depositBlockHash", "creditedAmount", "lastError", "signedRawTransaction"
  , "sender", "nonce", "transactionKind", "transactionHash", "observationFromBlock"
  , "creditEvents", "scanFromBlock", "scanBoundaryHash", "bridgeStatus", "providerCheckedAt", "sourceStatus", "sourceTerminal", "sourceBlockNumber", "sourceBlockHash", "sourceConfirmations"
  ]

without :: [Key.Key] -> Value -> Value
without keys (Object fields) = Object $ foldr KM.delete fields keys
without _ value = value

matchesQuote :: Value -> Value -> Bool
matchesQuote (Object quoted) (Object intent) =
  all (\(key, value) -> KM.lookup key intent == Just value) (KM.toList quoted)
  && retryPayload (Object intent) == Object (KM.insert "status" (String "awaiting-source") (KM.delete "createdAt" $ KM.delete "updatedAt" quoted))
matchesQuote _ _ = False

-- API timestamps can be ISO-8601 strings or Unix seconds. Missing timestamps
-- use the database clock; malformed supplied timestamps are never ignored.
payloadUpdatedAt :: Value -> IO (Maybe UTCTime)
payloadUpdatedAt (Object fields) = case KM.lookup "updatedAt" fields of
  Nothing -> pure Nothing
  Just Null -> pure Nothing
  Just (Number n) -> pure $ Just $ posixSecondsToUTCTime (realToFrac n)
  Just value@(String _) -> case fromJSON value of
    Success time -> pure $ Just time
    Error _ -> fail "Invalid funding intent updatedAt timestamp"
  _ -> fail "Invalid funding intent updatedAt timestamp"
payloadUpdatedAt _ = fail "Funding intent payload must be an object"
