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
  it "requires a fresh ready heartbeat for the exact destination and release" $ \conn -> do
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
    createIntent conn key intentId quoteId (put "receiver" (String "0xbad") intent) `shouldReturn` Left "QUOTE_PAYLOAD_CONFLICT"
    createIntent conn key intentId quoteId (put "extraRoute" (String "bad") intent) `shouldReturn` Left "QUOTE_PAYLOAD_CONFLICT"
    createIntent conn key intentId quoteId intent `shouldReturn` Right intent

  it "protects every original immutable binding on state updates" $ \conn -> do
    insertQuote conn quoteId quote expiry
    createIntent conn key intentId quoteId intent `shouldReturn` Right intent
    forM_ ["intentId", "quoteId", "ownerAddress", "beneficiary", "receiver", "token", "clearinghouse", "destinationChainId", "releaseId", "sourceChainId", "sourceToken", "sourceAmount", "receiverFactory", "intentSalt", "sourceTransactions", "expiresAt"] $ \binding ->
      updateIntent conn intentId (put binding Null intent) `shouldThrow` anyIOException
    updateIntent conn intentId (put "unknownBinding" Null intent) `shouldThrow` anyIOException
    getIntent conn intentId `shouldReturn` Just intent

  it "persists signed transaction evidence and clears it after canonical reconciliation" $ \conn -> do
    insertQuote conn quoteId quote expiry
    createIntent conn key intentId quoteId intent `shouldReturn` Right intent
    let signed = put "signedRawTransaction" (String "0x1234") $ put "transactionKind" (String "flush") $
          put "transactionHash" (String "0xabc") $ put "nonce" (String "4") $ put "sender" (String "0xsender") $
          put "status" (String "depositing") intent
    updateIntent conn intentId signed
    getActiveTransaction conn `shouldReturn` Just signed
    let confirmed = put "status" (String "confirmed") $ put "depositTxHash" (String "0xabc") $
          put "creditEvents" (Array mempty) $ put "scanFromBlock" (String "123") $ put "signedRawTransaction" Null signed
    updateIntent conn intentId confirmed
    getActiveTransaction conn `shouldReturn` Nothing
    getIntent conn intentId `shouldReturn` Just confirmed

  it "rotates bounded batches and keeps confirmed and failed intents for late arrivals/reorgs" $ \conn -> do
    let secondQuoteId = identifier '4'
        thirdQuoteId = identifier '6'
        secondIntentId = identifier '5'
        thirdIntentId = identifier '7'
    forM_ [(quoteId, intentId, key, "confirmed"), (secondQuoteId, secondIntentId, "second-key-000001", "failed"), (thirdQuoteId, thirdIntentId, "third-key-0000001", "bridging")] $ \(qid, iid, idempotency, status) -> do
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

  it "holds the global signer lock and releases it when the action throws" $ \conn -> do
    let available = withConnection $ \other -> bracket
          (query_ other "SELECT pg_try_advisory_lock(20261008, 4)" :: IO [Only Bool])
          (\held -> if held == [Only True]
            then void (query_ other "SELECT pg_advisory_unlock(20261008, 4)" :: IO [Only Bool])
            else pure ())
          pure
    withFundingWorkerLock conn (available `shouldReturn` [Only False])
    withFundingWorkerLock conn (ioError $ userError "expected test exception") `shouldThrow` anyIOException
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
  , "clearinghouse" .= ("0xclearinghouse" :: Text), "receiver" .= ("0xreceiver" :: Text)
  , "receiverFactory" .= ("0xfactory" :: Text), "releaseId" .= ("release-v1" :: Text)
  , "intentSalt" .= quoteId, "estimatedAmount" .= ("1000000" :: Text)
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
