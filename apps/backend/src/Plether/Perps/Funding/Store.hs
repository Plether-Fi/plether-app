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
  , claimSourceRelay
  , getSourceRelay
  , invalidateSourceRelay
  , withFundingStateLock
  , setWorkerReadiness
  , isWorkerReady
  ) where

import Control.Exception (bracket_)
import Control.Monad (unless, void, when)
import Data.Aeson (Result (..), Value (..), object, encode, fromJSON)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isAlphaNum, isAscii)
import Data.Maybe (isJust)
import Text.Read (readMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Database.PostgreSQL.Simple

-- Keep this initial, undeployed schema in sync with perps-funding-v1.sql.
-- Existing receiver-based rows are rejected, never silently reinterpreted.
ensureFundingSchema :: Connection -> IO ()
ensureFundingSchema conn = withTransaction conn $ do
  void (query_ conn "SELECT 1::int FROM pg_advisory_xact_lock(20261008, 3)" :: IO [Only Int])
  void $ execute_ conn fundingSchema

fundingSchema :: Query
fundingSchema = "CREATE TABLE IF NOT EXISTS perps_funding_quotes (\n\
  \  id TEXT PRIMARY KEY CHECK (id ~ '^0x[0-9a-f]{64}$'),\n\
  \  payload JSONB NOT NULL CHECK (jsonb_typeof(payload) = 'object' AND payload->>'quoteId' IS NOT DISTINCT FROM id),\n\
  \  expires_at TIMESTAMPTZ NOT NULL,\n\
  \  created_at TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp()\n\
  \);\n\
  \CREATE TABLE IF NOT EXISTS perps_funding_intents (\n\
  \  id TEXT PRIMARY KEY CHECK (id ~ '^0x[0-9a-f]{64}$'),\n\
  \  idempotency_key TEXT NOT NULL UNIQUE CHECK (idempotency_key ~ '^[-a-zA-Z0-9_:]{16,128}$'),\n\
  \  quote_id TEXT NOT NULL UNIQUE REFERENCES perps_funding_quotes(id),\n\
  \  creation_payload JSONB NOT NULL CHECK (jsonb_typeof(creation_payload) = 'object'),\n\
  \  payload JSONB NOT NULL CHECK (jsonb_typeof(payload) = 'object' AND payload->>'intentId' IS NOT DISTINCT FROM id AND payload->>'quoteId' IS NOT DISTINCT FROM quote_id),\n\
  \  status TEXT NOT NULL,\n\
  \  created_at TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp(),\n\
  \  updated_at TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp(),\n\
  \  last_polled_at TIMESTAMPTZ,\n\
  \  CHECK (payload->>'status' IS NOT DISTINCT FROM status)\n\
  \);\n\
  \-- Receiver-based development rows cannot be interpreted as destination actions.\n\
  \-- This feature has not shipped; operators must explicitly reset/migrate such\n\
  \-- local data rather than having startup rewrite its meaning or discard funds.\n\
  \DO $$ BEGIN\n\
  \  IF EXISTS (SELECT 1 FROM perps_funding_quotes WHERE NOT COALESCE(payload->>'destinationMessageHash' ~ '^0x[0-9a-f]{64}$' AND payload->>'destinationMessage' ~ '^0x([0-9a-f]{2})+$', FALSE))\n\
  \    OR EXISTS (SELECT 1 FROM perps_funding_intents WHERE NOT COALESCE(payload->>'destinationMessageHash' ~ '^0x[0-9a-f]{64}$' AND payload->>'destinationMessage' ~ '^0x([0-9a-f]{2})+$', FALSE)) THEN\n\
  \    RAISE EXCEPTION 'Legacy funding rows require an explicit migration or development reset';\n\
  \  END IF;\n\
  \END $$;\n\
  \ALTER TABLE perps_funding_quotes DROP CONSTRAINT IF EXISTS perps_funding_quotes_destination_message_check;\n\
  \ALTER TABLE perps_funding_quotes ADD CONSTRAINT perps_funding_quotes_destination_message_check\n\
  \  CHECK (COALESCE(payload->>'destinationMessageHash' ~ '^0x[0-9a-f]{64}$' AND payload->>'destinationMessage' ~ '^0x([0-9a-f]{2})+$', FALSE));\n\
  \ALTER TABLE perps_funding_intents DROP CONSTRAINT IF EXISTS perps_funding_intents_status_check;\n\
  \ALTER TABLE perps_funding_intents ADD CONSTRAINT perps_funding_intents_status_check\n\
  \  CHECK (status IN ('awaiting-source','bridging','confirmed','needs-deposit','retryable','failed'));\n\
  \ALTER TABLE perps_funding_intents DROP CONSTRAINT IF EXISTS perps_funding_intents_destination_message_check;\n\
  \ALTER TABLE perps_funding_intents ADD CONSTRAINT perps_funding_intents_destination_message_check\n\
  \  CHECK (COALESCE(payload->>'destinationMessageHash' ~ '^0x[0-9a-f]{64}$' AND payload->>'destinationMessage' ~ '^0x([0-9a-f]{2})+$', FALSE));\n\
  \CREATE INDEX IF NOT EXISTS perps_funding_intents_poll_idx\n\
  \  ON perps_funding_intents(last_polled_at NULLS FIRST, created_at, id);\n\
  \CREATE TABLE IF NOT EXISTS perps_funding_source_relays (\n\
  \  relay_hash TEXT PRIMARY KEY CHECK (relay_hash ~ '^0x[0-9a-f]{64}$'),\n\
  \  intent_id TEXT NOT NULL REFERENCES perps_funding_intents(id),\n\
  \  origin_chain_id NUMERIC(78,0) NOT NULL CHECK (origin_chain_id>0),\n\
  \  source_spoke_pool TEXT NOT NULL CHECK (source_spoke_pool ~ '^0x[0-9a-f]{40}$'),\n\
  \  deposit_id NUMERIC(78,0) NOT NULL CHECK (deposit_id>=0),\n\
  \  evidence JSONB NOT NULL CHECK (jsonb_typeof(evidence)='object' AND evidence->>'relayHash' IS NOT DISTINCT FROM relay_hash),\n\
  \  canonical BOOLEAN NOT NULL DEFAULT TRUE,\n\
  \  orphaned_evidence JSONB NOT NULL DEFAULT '[]'::jsonb CHECK (jsonb_typeof(orphaned_evidence)='array'),\n\
  \  updated_at TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp()\n\
  \);\n\
  \-- Ownership of a relay hash is permanent, even when its evidence is orphaned.\n\
  \-- Deposit IDs may be reused after a positive source-chain reorg invalidation.\n\
  \CREATE UNIQUE INDEX IF NOT EXISTS perps_funding_source_relays_active_intent\n\
  \  ON perps_funding_source_relays(intent_id) WHERE canonical;\n\
  \CREATE UNIQUE INDEX IF NOT EXISTS perps_funding_source_relays_active_deposit\n\
  \  ON perps_funding_source_relays(origin_chain_id,source_spoke_pool,deposit_id) WHERE canonical;\n\
  \CREATE TABLE IF NOT EXISTS perps_funding_worker_readiness (\n\
  \  release_id TEXT NOT NULL CHECK (length(release_id)>0),\n\
  \  chain_id NUMERIC(78,0) NOT NULL CHECK (chain_id>0),\n\
  \  ready BOOLEAN NOT NULL,\n\
  \  last_seen TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp(),\n\
  \  PRIMARY KEY (release_id,chain_id)\n\
  \);"

-- | Insert a quote once. A byte/key-order-independent retry is harmless, but a
-- reused identifier cannot change the route or extend the quote's lifetime.
insertQuote :: Connection -> Text -> Value -> Integer -> IO ()
insertQuote conn identifier payload expires = do
  requireActionPayload payload
  unless (textField "quoteId" payload == Just identifier && numberField "expiresAt" payload == Just expires) $
    fail "Funding quote identifier or expiry does not match its payload"
  rows <- query conn
    "INSERT INTO perps_funding_quotes(id,payload,expires_at) VALUES (?,?::jsonb,to_timestamp(?)) \
    \ON CONFLICT (id) DO UPDATE SET id=EXCLUDED.id \
    \WHERE perps_funding_quotes.payload=EXCLUDED.payload AND perps_funding_quotes.expires_at=EXCLUDED.expires_at RETURNING id"
    (identifier, jsonText payload, expires) :: IO [Only Text]
  unless (rows == [Only identifier]) $ fail "Funding quote identifier conflict"

findQuote :: Connection -> Text -> IO (Maybe Value)
findQuote conn identifier = (query conn
  "SELECT payload FROM perps_funding_quotes WHERE id=?" (Only identifier) :: IO [Only Value]) >>= singleActionValue

findIntentByIdempotencyKey :: Connection -> Text -> IO (Maybe Value)
findIntentByIdempotencyKey conn key = (query conn
  "SELECT payload FROM perps_funding_intents WHERE idempotency_key=?" (Only key) :: IO [Only Value]) >>= singleActionValue

-- | Claim a quote exactly once. Idempotency compares the original request, not
-- mutable worker evidence. Server-generated intentId/createdAt/updatedAt may
-- differ on a retry; all other request fields must remain semantically equal.
-- A matching retry returns current state even if the quote has since expired.
createIntent :: Connection -> Text -> Text -> Text -> Value -> IO (Either Text Value)
createIntent conn key identifier quoteId payload
  | not (isActionPayload payload) = pure $ Left "LEGACY_FUNDING_PAYLOAD"
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
getIntent conn identifier = (query conn
  "SELECT payload FROM perps_funding_intents WHERE id=?" (Only identifier) :: IO [Only Value]) >>= singleActionValue

-- | Replace reconciliation state, preserving the complete original route,
-- beneficiary, transaction request and release snapshot. Only a small explicit
-- set of server-owned evidence fields can be added, removed or changed.
updateIntent :: Connection -> Text -> Value -> IO ()
updateIntent conn identifier payload = withTransaction conn $ do
  stored <- query conn "SELECT payload FROM perps_funding_intents WHERE id=? FOR UPDATE" (Only identifier) :: IO [Only Value]
  case stored of
    [Only original] -> do
      requireActionPayload payload
      activeRelay <- getSourceRelay conn identifier
      unless (sourceBindingMatches activeRelay payload) $ fail "Cannot change a claimed source relay or transaction"
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
-- this on the connection protected by 'withFundingStateLock'.
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

-- | Coordinate unsigned observer/API read-modify-write operations across
-- processes. Keep the same dedicated connection through the action; this is
-- state coordination only and owns no signer, nonce, key, or transaction lane.
withFundingStateLock :: Connection -> IO a -> IO a
withFundingStateLock conn = bracket_ acquire release
  where
    acquire = void (query_ conn "SELECT 1::int FROM pg_advisory_lock(20261008, 4)" :: IO [Only Int])
    release = void (query_ conn "SELECT pg_advisory_unlock(20261008, 4)" :: IO [Only Bool])

-- | Publish readiness only after verifying the pinned deployment and the
-- unsigned observer's RPC health. No signing capability or ETH is required.
setWorkerReadiness :: Connection -> Text -> Integer -> Bool -> IO ()
setWorkerReadiness conn releaseId chainId ready = void $ execute conn
  "INSERT INTO perps_funding_worker_readiness(release_id,chain_id,ready,last_seen) VALUES (?,?,?,clock_timestamp()) \
  \ON CONFLICT (release_id,chain_id) DO UPDATE SET ready=EXCLUDED.ready,last_seen=EXCLUDED.last_seen"
  (releaseId, chainId, ready)

-- | Config and quote creation fail closed without a fresh observer heartbeat
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

singleActionValue :: [Only Value] -> IO (Maybe Value)
singleActionValue rows = case singleValue rows of
  Nothing -> pure Nothing
  Just payload -> requireActionPayload payload >> pure (Just payload)

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
  [ "status", "updatedAt", "lastCheckedAt", "sourceTxHash", "sourceRelay"
  , "depositTxHash", "depositBlockNumber", "depositBlockHash", "depositLogIndices"
  , "creditedAmount", "lastError", "observationFromBlock", "creditEvents"
  , "scanFromBlock", "scanBoundaryHash", "bridgeStatus", "providerCheckedAt"
  , "sourceStatus", "sourceTerminal", "sourceBlockNumber", "sourceBlockHash"
  , "sourceLogIndex", "sourceConfirmations", "fillTxHash", "fillBlockNumber"
  , "fillBlockHash", "fillLogIndex", "fallbackTxHash", "fallbackBlockNumber"
  , "fallbackBlockHash", "fallbackAmount", "fallbackLogIndices"
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

-- | Claim a source event only after the observer has verified its canonical
-- block, receipt, relay contents and confirmation depth. Database uniqueness
-- prevents two intents claiming one relay. Hash ownership survives reorgs;
-- source deposit IDs and an intent may be rebound only after explicit orphaning.
claimSourceRelay :: Connection -> Text -> Value -> IO (Either Text ())
claimSourceRelay conn identifier evidence = case parseSourceRelay evidence of
  Left reason -> pure $ Left reason
  Right binding -> withTransaction conn $ do
    intents <- query conn "SELECT payload FROM perps_funding_intents WHERE id=? FOR UPDATE"
      (Only identifier) :: IO [Only Value]
    case intents of
      [] -> pure $ Left "INTENT_NOT_FOUND"
      [Only payload]
        | not (isActionPayload payload) -> pure $ Left "LEGACY_FUNDING_PAYLOAD"
        | numberField "sourceChainId" payload /= Just (relayOrigin binding) -> pure $ Left "SOURCE_CHAIN_MISMATCH"
        | textField "sourceTxHash" payload /= Just (relayTransaction binding) -> pure $ Left "SOURCE_TRANSACTION_BINDING_MISMATCH"
        | isJust (textField "messageHash" evidence)
          && textField "messageHash" evidence /= textField "destinationMessageHash" payload -> pure $ Left "SOURCE_MESSAGE_BINDING_MISMATCH"
        | otherwise -> do
            -- One origin identity lock orders competing intents before checking
            -- or reactivating a deposit ID. The unique indexes also protect
            -- callers outside this module from creating duplicate active claims.
            let identityKey = T.intercalate ":" [T.pack $ show $ relayOrigin binding, relayPool binding, T.pack $ show $ relayDeposit binding]
            void (query conn "SELECT 1::int FROM pg_advisory_xact_lock(20261010,hashtext(?))"
              (Only identityKey) :: IO [Only Int])
            active <- getSourceRelay conn identifier
            owners <- query conn "SELECT intent_id,evidence,canonical FROM perps_funding_source_relays WHERE relay_hash=?"
              (Only $ relayHash binding) :: IO [(Text,Value,Bool)]
            otherDeposits <- query conn
              "SELECT intent_id FROM perps_funding_source_relays WHERE origin_chain_id=? AND source_spoke_pool=? AND deposit_id=? AND canonical AND relay_hash<>?"
              (relayOrigin binding,relayPool binding,relayDeposit binding,relayHash binding) :: IO [Only Text]
            let failure = case () of
                  _ | maybe False ((/= Just (relayHash binding)) . textField "relayHash") active -> Just "SOURCE_RELAY_ALREADY_BOUND"
                    | not (null otherDeposits) -> Just "SOURCE_DEPOSIT_ALREADY_CLAIMED"
                    | otherwise -> case owners of
                        [(owner,_,_)] | owner /= identifier -> Just "SOURCE_RELAY_ALREADY_CLAIMED"
                        [(_,old,True)] | old /= evidence -> Just "SOURCE_RELAY_EVIDENCE_CONFLICT"
                        [(_,old,False)] | relayContents old /= relayContents evidence -> Just "SOURCE_RELAY_EVIDENCE_CONFLICT"
                        _ -> Nothing
            case failure of
              Just reason -> pure $ Left reason
              Nothing -> do
                changed <- case owners of
                  [] -> query conn
                    "INSERT INTO perps_funding_source_relays(relay_hash,intent_id,origin_chain_id,source_spoke_pool,deposit_id,evidence) VALUES (?,?,?,?,?,?::jsonb) ON CONFLICT DO NOTHING RETURNING relay_hash"
                    (relayHash binding,identifier,relayOrigin binding,relayPool binding,relayDeposit binding,jsonText evidence) :: IO [Only Text]
                  [(_,_,_)] -> query conn
                    "UPDATE perps_funding_source_relays SET canonical=TRUE,evidence=?::jsonb,updated_at=clock_timestamp() WHERE relay_hash=? AND intent_id=? RETURNING relay_hash"
                    (jsonText evidence,relayHash binding,identifier) :: IO [Only Text]
                  _ -> fail "Ambiguous source relay owner"
                if changed /= [Only $ relayHash binding] then pure $ Left "SOURCE_RELAY_ALREADY_CLAIMED"
                else do
                  -- Pin the relational claim and public intent snapshot in the
                  -- same transaction; readers never observe a half-bound intent.
                  let pinned = setPayloadFields
                        ([(key, maybe Null id $ valueField key evidence) | key <- sourceIdentityFields]
                          <> [("sourceRelay",evidence),("sourceStatus",String "confirmed"),("sourceTerminal",Bool False)]) payload
                  void $ execute conn "UPDATE perps_funding_intents SET payload=?::jsonb,updated_at=clock_timestamp() WHERE id=?"
                    (jsonText pinned,identifier)
                  pure $ Right ()
      _ -> fail "Ambiguous funding intent"

getSourceRelay :: Connection -> Text -> IO (Maybe Value)
getSourceRelay conn identifier = singleValue <$> (query conn
  "SELECT evidence FROM perps_funding_source_relays WHERE intent_id=? AND canonical"
  (Only identifier) :: IO [Only Value])

-- | Caller must have positive canonical-chain proof that the claimed source
-- occurrence was orphaned/reverted. Missing RPC data, timeout, a missing receipt
-- or a provider status alone does not authorize invalidation. Preserve the
-- relay's permanent owner and append the orphaned occurrence to its history.
-- Invalidation also retracts destination readiness atomically. Reread the intent
-- before saving further observations, just as after 'claimSourceRelay'.
invalidateSourceRelay :: Connection -> Text -> Text -> IO Bool
invalidateSourceRelay conn identifier hash = withTransaction conn $ do
  intents <- query conn "SELECT payload FROM perps_funding_intents WHERE id=? FOR UPDATE"
    (Only identifier) :: IO [Only Value]
  case intents of
    [] -> pure False
    [Only payload] -> do
      changed <- execute conn
        "UPDATE perps_funding_source_relays SET canonical=FALSE,orphaned_evidence=orphaned_evidence||jsonb_build_array(jsonb_build_object('evidence',evidence,'orphanedAt',clock_timestamp())),updated_at=clock_timestamp() WHERE intent_id=? AND relay_hash=? AND canonical"
        (identifier,hash)
      when (changed == 1) $ do
        let retracted = setPayloadFields
              ([(key,Null) | key <- ["sourceRelay","sourceBlockNumber","sourceBlockHash","sourceLogIndex"
                ,"depositTxHash","depositBlockNumber","depositBlockHash","depositLogIndices"
                ,"fillTxHash","fillBlockNumber","fillBlockHash","fillLogIndex"
                ,"fallbackTxHash","fallbackBlockNumber","fallbackBlockHash","fallbackAmount","fallbackLogIndices"
                ,"scanBoundaryHash","scanFromBlock"]]
              <> [("status",String "bridging"),("creditedAmount",String "0"),("creditEvents",Array mempty)
                 ,("sourceStatus",String "pending"),("sourceTerminal",Bool False),("sourceConfirmations",Number 0)
                 ,("lastCheckedAt",Number 0),("lastError",String "SOURCE_RELAY_ORPHANED")]) payload
        void $ execute conn "UPDATE perps_funding_intents SET payload=?::jsonb,status='bridging',updated_at=clock_timestamp() WHERE id=?"
          (jsonText retracted,identifier)
      pure $ changed == 1
    _ -> fail "Ambiguous funding intent"

data SourceRelayBinding = SourceRelayBinding
  { relayOrigin :: Integer
  , relayPool :: Text
  , relayDeposit :: Integer
  , relayHash :: Text
  , relayTransaction :: Text
  }

parseSourceRelay :: Value -> Either Text SourceRelayBinding
parseSourceRelay value = do
  origin <- maybe invalid Right $ numberField "originChainId" value
  unless (origin > 0 && origin < 2 ^ (256 :: Int)) invalid
  pool <- required "sourceSpokePool"
  unless (canonicalHex 20 pool && pool /= "0x" <> T.replicate 40 "0") invalid
  deposit <- natural "depositId"
  hash <- requiredHash "relayHash"
  transaction <- requiredHash "sourceTxHash"
  _ <- natural "sourceBlockNumber"
  _ <- requiredHash "sourceBlockHash"
  _ <- natural "sourceLogIndex"
  pure $ SourceRelayBinding origin pool deposit hash transaction
  where
    invalid = Left "INVALID_SOURCE_RELAY"
    required key = maybe invalid Right $ textField key value
    requiredHash key = do
      hash <- required key
      if canonicalHex 32 hash then Right hash else invalid
    natural :: Text -> Either Text Integer
    natural key = do
      raw <- required key
      case readMaybe (T.unpack raw) of
        Just number | number >= 0 && number < 2 ^ (256 :: Int) && raw == T.pack (show number) -> Right number
        _ -> invalid

sourceIdentityFields :: [Text]
sourceIdentityFields = ["sourceTxHash","sourceBlockNumber","sourceBlockHash","sourceLogIndex"]

sourceBindingMatches :: Maybe Value -> Value -> Bool
sourceBindingMatches Nothing payload = maybe True (== Null) $ valueField "sourceRelay" payload
sourceBindingMatches (Just evidence) payload = valueField "sourceRelay" payload == Just evidence
  && all (\key -> valueField key payload == valueField key evidence) sourceIdentityFields

-- Block/log identity can change when the same relay reappears after a proven
-- reorg; its economic/message/transaction identity cannot be rewritten.
relayContents :: Value -> Value
relayContents = without ["sourceBlockNumber","sourceBlockHash","sourceLogIndex"]

valueField :: Text -> Value -> Maybe Value
valueField key (Object fields) = KM.lookup (Key.fromText key) fields
valueField _ _ = Nothing

setPayloadFields :: [(Text,Value)] -> Value -> Value
setPayloadFields fields (Object payload) = Object $ foldr (\(key,value) -> KM.insert (Key.fromText key) value) payload fields
setPayloadFields _ _ = error "Funding payload must be an object"

canonicalHex :: Int -> Text -> Bool
canonicalHex bytes value = T.length value == 2 + bytes * 2 && T.take 2 value == "0x"
  && T.all (`elem` (['0'..'9'] <> ['a'..'f'])) (T.drop 2 value)

-- Destination message/hash are the explicit marker for the action-based
-- protocol. Receiver-based or signed-transaction development rows fail closed.
isActionPayload :: Value -> Bool
isActionPayload payload = case (textField "destinationMessage" payload,textField "destinationMessageHash" payload) of
  (Just message,Just hash) -> canonicalHex 32 hash && T.length message > 2 && even (T.length message)
    && T.take 2 message == "0x" && T.all (`elem` (['0'..'9'] <> ['a'..'f'])) (T.drop 2 message)
    && all (not . (`isJustField` payload)) ["receiver","receiverFactory","factoryCodeHash","intentSalt","signedRawTransaction","sender","nonce","transactionKind","transactionHash"]
  _ -> False
  where isJustField key value = isJust $ valueField key value

requireActionPayload :: Value -> IO ()
requireActionPayload payload = unless (isActionPayload payload) $ fail "Legacy or invalid funding payload; destination actions are required"
