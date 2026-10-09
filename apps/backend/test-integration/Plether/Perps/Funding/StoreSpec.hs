{-# LANGUAGE OverloadedStrings #-}

module Plether.Perps.Funding.StoreSpec (fundingStoreSpec) where

import Control.Concurrent.Async (concurrently)
import Control.Exception (bracket)
import Control.Monad (forM_, void)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Either (isLeft, isRight)
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.PostgreSQL.Simple
import Plether.Perps.Funding.Store
import Test.Hspec

-- This suite uses a private schema, and refuses a non-test database. It exercises
-- actual PostgreSQL locking/constraints rather than mocking SQL responses.
fundingStoreSpec :: Text -> Spec
fundingStoreSpec databaseUrl = around withStore $ describe "durable perps funding store" $ do
  it "requires a fresh observer heartbeat for the exact destination and release" $ \conn -> do
    isWorkerReady conn "release-v1" 42161 `shouldReturn` False
    setWorkerReadiness conn "release-v1" 42161 True
    isWorkerReady conn "release-v1" 42161 `shouldReturn` True
    isWorkerReady conn "release-v2" 42161 `shouldReturn` False
    isWorkerReady conn "release-v1" 1 `shouldReturn` False
    void $ execute_ conn "UPDATE perps_funding_worker_readiness SET last_seen=clock_timestamp()-interval '61 seconds'"
    isWorkerReady conn "release-v1" 42161 `shouldReturn` False
    setWorkerReadiness conn "release-v1" 42161 True
    isWorkerReady conn "release-v1" 42161 `shouldReturn` True
    setWorkerReadiness conn "release-v1" 42161 False
    isWorkerReady conn "release-v1" 42161 `shouldReturn` False

  it "retries identical quotes and rejects changes to immutable payload/expiry" $ \conn -> do
    insertQuote conn quoteId quote expiry
    insertQuote conn quoteId quote expiry
    findQuote conn quoteId `shouldReturn` Just quote
    insertQuote conn quoteId (put "minimumAmount" (String "1") quote) expiry `shouldThrow` anyIOException
    insertQuote conn quoteId (put "expiresAt" (Number $ fromInteger $ expiry + 1) quote) (expiry + 1) `shouldThrow` anyIOException
    findQuote conn quoteId `shouldReturn` Just quote

  it "returns latest state on an idempotent retry after quote expiry" $ \conn -> do
    insertQuote conn quoteId quote expiry
    createIntent conn key intentId quoteId intent `shouldReturn` Right intent
    let confirmed = put "status" (String "confirmed") intent
    updateIntent conn intentId confirmed
    void $ execute conn "UPDATE perps_funding_quotes SET expires_at=to_timestamp(1) WHERE id=?" (Only quoteId)
    createIntent conn key (identifier '3') quoteId (put "intentId" (String $ identifier '3') intent) `shouldReturn` Right confirmed
    createIntent conn key intentId quoteId (put "minimumAmount" (String "1") intent) `shouldReturn` Left "IDEMPOTENCY_CONFLICT"

  it "rejects a second idempotency key for the same quote" $ \conn -> do
    insertQuote conn quoteId quote expiry
    createIntent conn key intentId quoteId intent `shouldReturn` Right intent
    createIntent conn "another-key-000001" (identifier '3') quoteId (put "intentId" (String $ identifier '3') intent)
      `shouldReturn` Left "QUOTE_ALREADY_USED"

  it "checks expiry with the database clock before accepting a new intent" $ \conn -> do
    let expiredQuote = put "expiresAt" (Number 1) quote
    insertQuote conn quoteId expiredQuote 1
    createIntent conn key intentId quoteId (asIntent intentId expiredQuote) `shouldReturn` Left "QUOTE_EXPIRED"

  it "rejects changed quote bindings and unquoted additions" $ \conn -> do
    insertQuote conn quoteId quote expiry
    createIntent conn key intentId quoteId (put "destinationMessage" (String "0xabcd") intent) `shouldReturn` Left "QUOTE_PAYLOAD_CONFLICT"
    createIntent conn key intentId quoteId (put "extraRoute" (String "bad") intent) `shouldReturn` Left "QUOTE_PAYLOAD_CONFLICT"
    createIntent conn key intentId quoteId intent `shouldReturn` Right intent

  it "protects every original immutable binding on state updates" $ \conn -> do
    insertQuote conn quoteId quote expiry
    createIntent conn key intentId quoteId intent `shouldReturn` Right intent
    forM_ ["intentId", "quoteId", "ownerAddress", "beneficiary", "token", "clearinghouse", "destinationChainId", "releaseId", "sourceChainId", "sourceToken", "sourceAmount", "destinationSpokePool", "destinationSpokePoolImplementation", "destinationSpokePoolCodeHash", "destinationSpokePoolImplementationCodeHash", "multicallHandler", "multicallHandlerCodeHash", "destinationMessage", "destinationMessageHash", "sourceTransactions", "expiresAt"] $ \binding ->
      updateIntent conn intentId (put binding Null intent) `shouldThrow` anyIOException
    updateIntent conn intentId (put "unknownBinding" Null intent) `shouldThrow` anyIOException
    getIntent conn intentId `shouldReturn` Just intent

  it "rejects legacy quote rows rather than interpreting them as destination actions" $ \conn -> do
    let legacy = object ["quoteId" .= quoteId,"expiresAt" .= expiry]
    insertQuote conn quoteId legacy expiry `shouldThrow` anyIOException
    void $ execute_ conn "ALTER TABLE perps_funding_quotes DROP CONSTRAINT perps_funding_quotes_destination_message_check"
    void $ execute conn "INSERT INTO perps_funding_quotes(id,payload,expires_at) VALUES (?,jsonb_build_object('quoteId',?::text),to_timestamp(?))"
      (quoteId,quoteId,expiry)
    ensureFundingSchema conn `shouldThrow` (const True :: SqlError -> Bool)
    findQuote conn quoteId `shouldThrow` anyIOException
    query_ conn "SELECT count(*) FROM perps_funding_quotes" `shouldReturn` ([Only 1] :: [Only Int])

  it "atomically pins canonical source evidence and retries it without creating another claim" $ \conn -> do
    prepareRelayIntent conn quoteId intentId key
    claimSourceRelay conn intentId relay `shouldReturn` Right ()
    claimSourceRelay conn intentId relay `shouldReturn` Right ()
    getSourceRelay conn intentId `shouldReturn` Just relay
    current <- getIntent conn intentId
    (current >>= field "sourceRelay") `shouldBe` Just relay
    (current >>= field "sourceBlockHash") `shouldBe` field "sourceBlockHash" relay
    query_ conn "SELECT count(*) FROM perps_funding_source_relays" `shouldReturn` ([Only 1] :: [Only Int])

  it "allows needs-deposit proof while rejecting source identity changes or signer metadata" $ \conn -> do
    prepareRelayIntent conn quoteId intentId key
    claimSourceRelay conn intentId relay `shouldReturn` Right ()
    current <- requireIntent conn intentId
    let delivered = put "status" (String "needs-deposit") $ put "fillTxHash" (String $ identifier 'b') $
          put "fallbackTxHash" (String $ identifier 'b') $ put "fallbackAmount" (String "1000000") $
          put "fallbackLogIndices" (Array mempty) $ put "lastCheckedAt" (Number 123) current
    updateIntent conn intentId delivered
    getIntent conn intentId `shouldReturn` Just delivered
    forM_ ["sourceTxHash","sourceBlockNumber","sourceBlockHash","sourceLogIndex","sourceRelay"] $ \binding ->
      updateIntent conn intentId (put binding Null delivered) `shouldThrow` anyIOException
    forM_ ["signedRawTransaction","nonce","sender","transactionHash","transactionKind"] $ \removed ->
      updateIntent conn intentId (put removed (String "unused") delivered) `shouldThrow` anyIOException

  it "does not let updateIntent forge a source relay without an atomic claim" $ \conn -> do
    prepareRelayIntent conn quoteId intentId key
    current <- requireIntent conn intentId
    updateIntent conn intentId (put "sourceRelay" relay current) `shouldThrow` anyIOException
    getSourceRelay conn intentId `shouldReturn` Nothing

  it "rejects incorrect source chain, transaction, message, and malformed event locators" $ \conn -> do
    prepareRelayIntent conn quoteId intentId key
    claimSourceRelay conn intentId (put "originChainId" (Number 2) relay) `shouldReturn` Left "SOURCE_CHAIN_MISMATCH"
    claimSourceRelay conn intentId (put "sourceTxHash" (String $ identifier 'e') relay) `shouldReturn` Left "SOURCE_TRANSACTION_BINDING_MISMATCH"
    claimSourceRelay conn intentId (put "messageHash" (String $ identifier 'e') relay) `shouldReturn` Left "SOURCE_MESSAGE_BINDING_MISMATCH"
    forM_ ["-1","01","1e2","0x1"] $ \invalid ->
      claimSourceRelay conn intentId (put "depositId" (String invalid) relay) `shouldReturn` Left "INVALID_SOURCE_RELAY"
    getSourceRelay conn intentId `shouldReturn` Nothing

  it "keeps relay-hash ownership permanent even after canonical evidence is orphaned" $ \conn -> do
    prepareRelayIntent conn quoteId intentId key
    prepareRelayIntent conn (identifier '4') (identifier '5') "second-key-000001"
    claimSourceRelay conn intentId relay `shouldReturn` Right ()
    claimSourceRelay conn (identifier '5') relay `shouldReturn` Left "SOURCE_RELAY_ALREADY_CLAIMED"
    invalidateSourceRelay conn intentId (identifier 'a') `shouldReturn` True
    claimSourceRelay conn (identifier '5') relay `shouldReturn` Left "SOURCE_RELAY_ALREADY_CLAIMED"
    query_ conn "SELECT count(*) FROM perps_funding_source_relays WHERE NOT canonical" `shouldReturn` ([Only 1] :: [Only Int])

  it "permits deposit-ID reuse by a different relay only after positive orphan invalidation" $ \conn -> do
    prepareRelayIntent conn quoteId intentId key
    prepareRelayIntent conn (identifier '4') (identifier '5') "second-key-000001"
    let replacement = put "relayHash" (String $ identifier 'b') relay
    claimSourceRelay conn intentId relay `shouldReturn` Right ()
    claimSourceRelay conn (identifier '5') replacement `shouldReturn` Left "SOURCE_DEPOSIT_ALREADY_CLAIMED"
    invalidateSourceRelay conn intentId (identifier 'a') `shouldReturn` True
    claimSourceRelay conn (identifier '5') replacement `shouldReturn` Right ()
    getSourceRelay conn intentId `shouldReturn` Nothing
    getSourceRelay conn (identifier '5') `shouldReturn` Just replacement

  it "retracts destination readiness and preserves orphan history on source invalidation" $ \conn -> do
    prepareRelayIntent conn quoteId intentId key
    claimSourceRelay conn intentId relay `shouldReturn` Right ()
    current <- requireIntent conn intentId
    updateIntent conn intentId $ put "status" (String "confirmed") $ put "creditedAmount" (String "1000000") $
      put "depositTxHash" (String $ identifier 'b') $ put "fillTxHash" (String $ identifier 'b') current
    invalidateSourceRelay conn intentId (identifier 'a') `shouldReturn` True
    invalidateSourceRelay conn intentId (identifier 'a') `shouldReturn` False
    retracted <- requireIntent conn intentId
    field "status" retracted `shouldBe` Just (String "bridging")
    field "creditedAmount" retracted `shouldBe` Just (String "0")
    field "sourceTxHash" retracted `shouldBe` Just (String sourceTxHash)
    forM_ ["sourceRelay","depositTxHash","fillTxHash","fallbackTxHash"] $ \name -> field name retracted `shouldBe` Just Null
    history <- query_ conn "SELECT orphaned_evidence->0->'evidence',jsonb_array_length(orphaned_evidence) FROM perps_funding_source_relays" :: IO [(Value,Int)]
    history `shouldBe` [(relay,1)]

  it "reactivates the same owned relay after reappearance without losing orphan evidence" $ \conn -> do
    prepareRelayIntent conn quoteId intentId key
    claimSourceRelay conn intentId relay `shouldReturn` Right ()
    invalidateSourceRelay conn intentId (identifier 'a') `shouldReturn` True
    let reappeared = put "sourceBlockHash" (String $ identifier 'e') $ put "sourceBlockNumber" (String "101") relay
    claimSourceRelay conn intentId reappeared `shouldReturn` Right ()
    getSourceRelay conn intentId `shouldReturn` Just reappeared
    query_ conn "SELECT jsonb_array_length(orphaned_evidence) FROM perps_funding_source_relays" `shouldReturn` ([Only 1] :: [Only Int])

  it "allows a new relay for an intent only after the old claim is explicitly orphaned" $ \conn -> do
    prepareRelayIntent conn quoteId intentId key
    claimSourceRelay conn intentId relay `shouldReturn` Right ()
    let replacement = put "relayHash" (String $ identifier 'b') $ put "depositId" (String "43") relay
    claimSourceRelay conn intentId replacement `shouldReturn` Left "SOURCE_RELAY_ALREADY_BOUND"
    invalidateSourceRelay conn intentId (identifier 'a') `shouldReturn` True
    claimSourceRelay conn intentId replacement `shouldReturn` Right ()
    getSourceRelay conn intentId `shouldReturn` Just replacement
    query_ conn "SELECT count(*) FROM perps_funding_source_relays" `shouldReturn` ([Only 2] :: [Only Int])

  it "serializes different intents racing for the same relay" $ \conn -> do
    prepareRelayIntent conn quoteId intentId key
    prepareRelayIntent conn (identifier '4') (identifier '5') "second-key-000001"
    (left,right) <- concurrently
      (withConnection $ \other -> claimSourceRelay other intentId relay)
      (withConnection $ \other -> claimSourceRelay other (identifier '5') relay)
    length (filter isRight [left,right]) `shouldBe` 1
    filter isLeft [left,right] `shouldBe` [Left "SOURCE_RELAY_ALREADY_CLAIMED"]
    query_ conn "SELECT count(*) FROM perps_funding_source_relays" `shouldReturn` ([Only 1] :: [Only Int])

  it "serializes different relay hashes racing for the same canonical deposit ID" $ \conn -> do
    prepareRelayIntent conn quoteId intentId key
    prepareRelayIntent conn (identifier '4') (identifier '5') "second-key-000001"
    let replacement = put "relayHash" (String $ identifier 'b') relay
    (left,right) <- concurrently
      (withConnection $ \other -> claimSourceRelay other intentId relay)
      (withConnection $ \other -> claimSourceRelay other (identifier '5') replacement)
    length (filter isRight [left,right]) `shouldBe` 1
    filter isLeft [left,right] `shouldBe` [Left "SOURCE_DEPOSIT_ALREADY_CLAIMED"]
    query_ conn "SELECT count(*) FROM perps_funding_source_relays WHERE canonical" `shouldReturn` ([Only 1] :: [Only Int])

  it "rotates bounded batches and keeps confirmed and needs-deposit intents for late arrivals/reorgs" $ \conn -> do
    let secondQuoteId = identifier '4'
        thirdQuoteId = identifier '6'
        secondIntentId = identifier '5'
        thirdIntentId = identifier '7'
    forM_ [(quoteId, intentId, key, "confirmed"), (secondQuoteId, secondIntentId, "second-key-000001", "needs-deposit"), (thirdQuoteId, thirdIntentId, "third-key-0000001", "bridging")] $ \(qid, iid, idempotency, status) -> do
      let quoted = put "quoteId" (String qid) quote
          initial = asIntent iid quoted
      insertQuote conn qid quoted expiry
      createIntent conn idempotency iid qid initial `shouldReturn` Right initial
      updateIntent conn iid (put "status" (String status) initial)
    first <- listPendingIntents conn 1
    second <- listPendingIntents conn 1
    third <- listPendingIntents conn 1
    sort (map (field "intentId") (first <> second <> third)) `shouldBe` sort (map (Just . String) [intentId, secondIntentId, thirdIntentId])
    listPendingIntents conn 0 `shouldReturn` []

  it "does not let another release occupy a bounded deployment polling batch" $ \conn -> do
    insertQuote conn quoteId quote expiry
    createIntent conn key intentId quoteId intent `shouldReturn` Right intent
    listPendingIntentsForDeployment conn (object ["releaseId" .= ("other-release" :: Text)]) 1 `shouldReturn` []
    listPendingIntentsForDeployment conn (object []) 1 `shouldReturn` [intent]

  it "serializes concurrent retries of the same key and returns the same stored identity" $ \conn -> do
    insertQuote conn quoteId quote expiry
    (left, right) <- concurrently
      (withConnection $ \other -> createIntent other key intentId quoteId intent)
      (withConnection $ \other -> createIntent other key (identifier '3') quoteId (put "intentId" (String $ identifier '3') intent))
    left `shouldSatisfy` isRight
    right `shouldBe` left
    query_ conn "SELECT count(*) FROM perps_funding_intents" `shouldReturn` ([Only 1] :: [Only Int])

  it "serializes different keys racing for the same quote" $ \conn -> do
    insertQuote conn quoteId quote expiry
    (left, right) <- concurrently
      (withConnection $ \other -> createIntent other key intentId quoteId intent)
      (withConnection $ \other -> createIntent other "another-key-000001" (identifier '3') quoteId (put "intentId" (String $ identifier '3') intent))
    length (filter isRight [left, right]) `shouldBe` 1
    filter isLeft [left, right] `shouldBe` [Left "QUOTE_ALREADY_USED"]
    query_ conn "SELECT count(*) FROM perps_funding_intents" `shouldReturn` ([Only 1] :: [Only Int])

  it "holds the funding state lock and releases it when the action throws" $ \conn -> do
    let available = withConnection $ \other -> bracket
          (query_ other "SELECT pg_try_advisory_lock(20261008, 4)" :: IO [Only Bool])
          (\held -> if held == [Only True]
            then void (query_ other "SELECT pg_advisory_unlock(20261008, 4)" :: IO [Only Bool])
            else pure ())
          pure
    withFundingStateLock conn (available `shouldReturn` [Only False])
    withFundingStateLock conn (ioError $ userError "expected test exception") `shouldThrow` anyIOException
    available `shouldReturn` [Only True]
  where
    withConnection action = bracket (connectPostgreSQL $ TE.encodeUtf8 databaseUrl) close $ \conn -> do
      void $ execute_ conn "SET search_path=perps_funding_store_spec"
      action conn
    withStore action = bracket (connectPostgreSQL $ TE.encodeUtf8 databaseUrl) close $ \conn -> do
      names <- query_ conn "SELECT current_database()" :: IO [Only Text]
      unlessTest names
      void $ execute_ conn "DROP SCHEMA IF EXISTS perps_funding_store_spec CASCADE"
      void $ execute_ conn "CREATE SCHEMA perps_funding_store_spec"
      void $ execute_ conn "SET search_path=perps_funding_store_spec"
      ensureFundingSchema conn
      action conn
    unlessTest [Only name] | "_test" `T.isSuffixOf` name || name == "plether_critical_path" = pure ()
    unlessTest _ = fail "Funding store integration tests require a database ending in _test or named plether_critical_path"

identifier :: Char -> Text
identifier character = "0x" <> T.replicate 64 (T.singleton character)

quoteId, intentId, key :: Text
quoteId = identifier '1'
intentId = identifier '2'
key = "funding-test-000001"

expiry :: Integer
expiry = 4102444800

quote :: Value
quote = object
  [ "quoteId" .= quoteId, "expiresAt" .= expiry, "sourceChainId" .= (1 :: Int)
  , "sourceToken" .= ("0xsource" :: Text), "sourceAmount" .= ("1000000" :: Text)
  , "destinationChainId" .= (42161 :: Int), "token" .= ("0xtoken" :: Text)
  , "ownerAddress" .= ("0xowner" :: Text), "beneficiary" .= ("0xbeneficiary" :: Text)
  , "clearinghouse" .= ("0xclearinghouse" :: Text)
  , "destinationSpokePool" .= ("0xdestinationpool" :: Text), "releaseId" .= ("release-v1" :: Text)
  , "destinationSpokePoolImplementation" .= ("0ximplementation" :: Text)
  , "destinationSpokePoolCodeHash" .= identifier 'c', "destinationSpokePoolImplementationCodeHash" .= identifier 'd'
  , "multicallHandler" .= ("0xhandler" :: Text), "multicallHandlerCodeHash" .= identifier 'e'
  , "destinationMessage" .= ("0x0102" :: Text), "destinationMessageHash" .= identifier 'f'
  , "estimatedAmount" .= ("1000000" :: Text)
  , "minimumAmount" .= ("990000" :: Text), "provider" .= ("test-provider" :: Text)
  , "sourceTransactions" .= ([] :: [Value])
  ]

intent :: Value
intent = asIntent intentId quote

asIntent :: Text -> Value -> Value
asIntent identifierValue = put "intentId" (String identifierValue) . put "status" (String "awaiting-source")

put :: Text -> Value -> Value -> Value
put keyName value (Object fields) = Object $ KM.insert (Key.fromText keyName) value fields
put _ _ _ = error "Test fixture is not an object"

field :: Text -> Value -> Maybe Value
field keyName (Object fields) = KM.lookup (Key.fromText keyName) fields
field _ _ = Nothing


sourceTxHash :: Text
sourceTxHash = identifier '9'

relay :: Value
relay = object
  ["originChainId" .= (1 :: Integer),"sourceSpokePool" .= ("0x" <> T.replicate 40 "1")
  ,"depositId" .= ("42" :: Text),"relayHash" .= identifier 'a',"sourceTxHash" .= sourceTxHash
  ,"sourceBlockNumber" .= ("100" :: Text),"sourceBlockHash" .= identifier 'c',"sourceLogIndex" .= ("2" :: Text)
  ,"messageHash" .= identifier 'f']

prepareRelayIntent :: Connection -> Text -> Text -> Text -> IO ()
prepareRelayIntent conn qid iid idempotency = do
  let quoted = put "quoteId" (String qid) quote
      initial = asIntent iid quoted
  insertQuote conn qid quoted expiry
  createIntent conn idempotency iid qid initial `shouldReturn` Right initial
  updateIntent conn iid $ put "sourceTxHash" (String sourceTxHash) initial

requireIntent :: Connection -> Text -> IO Value
requireIntent conn iid = getIntent conn iid >>= maybe (fail "Expected durable intent") pure
