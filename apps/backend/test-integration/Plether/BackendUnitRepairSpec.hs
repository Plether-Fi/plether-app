module Plether.BackendUnitRepairSpec (backendUnitRepairSpec) where

import Control.Exception (SomeException, bracket, displayException, finally, try)
import Control.Monad (forM_, void)
import Data.Aeson (Value, object, (.=))
import qualified Data.ByteString.Char8 as BS8
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Unique (hashUnique, newUnique)
import GHC.Clock (getMonotonicTimeNSec)
import Database.PostgreSQL.Simple
  ( Connection, Only (..), close, connectPostgreSQL, execute, execute_, query, query_ )
import Database.PostgreSQL.Simple.Types (Query (..))
import Plether.Database.AaSponsorship (ensureAaSponsorshipSchema)
import Plether.Database.Schema
  ( PerpsOrderRow (..), TestnetFaucetClaimRow (..)
  , ensurePerpsHistorySchema, getPerpsOrdersByAccount
  , ensureTestnetFaucetSchema, beginTestnetFaucetClaim, getTestnetFaucetClaim
  , markTestnetFaucetClaimSubmitted, markTestnetFaucetClaimSuccess, markTestnetFaucetClaimReverted )
import Plether.Database.Insights.Registration (openRegistrationIfConfigured)
import Plether.Database.VaultPerformance
  ( VaultPerformanceSnapshotRow (..), ensureVaultPerformanceSchema
  , getVaultPerformanceSnapshots, upsertVaultPerformanceSnapshot )
import Test.Hspec

-- Every example owns a fresh schema on the explicitly supplied test database.
-- No public/shared tables are truncated or changed.
backendUnitRepairSpec :: Text -> Spec
backendUnitRepairSpec databaseUrl = describe "backend unit audit PostgreSQL behavior" $ do
  describe "native AA startup schema verification" $ do
    it "accepts the actual runtime bootstrap and an idempotent second startup" $
      withSchema databaseUrl $ \conn -> do
        ensureAaSponsorshipSchema conn
        ensureAaSponsorshipSchema conn
        rows <- query_ conn "SELECT issuance_paused FROM aa_sponsorship_control" :: IO [Only Bool]
        rows `shouldBe` [Only True]

    it "accepts the static bootstrap before running the actual startup verifier" $
      withSchema databaseUrl $ \conn -> do
        installStaticSchema conn
        ensureAaSponsorshipSchema conn

    it "accepts valid startup when an unrelated schema has a same-named index" $
      withSchema databaseUrl $ \conn -> do
        ensureAaSponsorshipSchema conn
        withSchema databaseUrl $ \other -> do
          void $ execute_ other "CREATE TABLE unrelated (request_key VARCHAR(66),state VARCHAR(16))"
          void $ execute_ other "CREATE UNIQUE INDEX idx_aa_sponsorship_active_request_key ON unrelated(request_key) WHERE state IN ('reserved','signed','submitted')"
          ensureAaSponsorshipSchema conn

    forM_ aaDrifts $ \(name, mutation, message) ->
      it ("fails actual startup after isolated " <> name <> " drift") $
        withSchema databaseUrl $ \conn -> do
          ensureAaSponsorshipSchema conn
          void $ execute_ conn mutation
          expectFailureContaining message $ ensureAaSponsorshipSchema conn

  describe "vault performance persistence" $ do
    it "isolates vault-only redeployments and returns the newest limited rows chronologically" $
      withSchema databaseUrl $ \conn -> do
        ensureVaultPerformanceSchema conn
        mapM_ (upsertVaultPerformanceSnapshot conn . vaultRow) [0..3]
        let redeployed = (vaultRow 3) {vpsSeniorVaultAddress = address "9", vpsJuniorVaultAddress = address "a"}
        upsertVaultPerformanceSnapshot conn redeployed
        getVaultPerformanceSnapshots conn 421614 (address "1") (address "2") (address "3") 2
          `shouldReturn` map vaultRow [2,3]
        getVaultPerformanceSnapshots conn 421614 (address "1") (address "9") (address "a") 2
          `shouldReturn` [redeployed]
        getVaultPerformanceSnapshots conn 1 (address "1") (address "2") (address "3") 2
          `shouldReturn` []

    it "atomically upserts all fields at one identity without duplicating its row" $
      withSchema databaseUrl $ \conn -> do
        ensureVaultPerformanceSchema conn
        upsertVaultPerformanceSnapshot conn $ vaultRow 0
        let replacement = (vaultRow 0)
              { vpsBlockNumber = 999, vpsBlockHash = hash "b", vpsBlockTimestamp = 3598
              , vpsMarkFresh = Just False, vpsSeniorTotalAssets = 201, vpsSeniorLockedAssets = Just 202
              , vpsJuniorLockedAssets = Just 203, vpsSeniorTotalSupply = 204, vpsSeniorSharePriceWad = 205
              , vpsJuniorTotalAssets = 206, vpsJuniorTotalSupply = 207, vpsJuniorSharePriceWad = 208 }
        upsertVaultPerformanceSnapshot conn replacement
        upsertVaultPerformanceSnapshot conn replacement
        getVaultPerformanceSnapshots conn 421614 (address "1") (address "2") (address "3") 10
          `shouldReturn` [replacement]
        (query_ conn "SELECT COUNT(*) FROM vault_performance_snapshots" :: IO [Only Int64])
          `shouldReturn` [Only 1]

    it "preserves uint256-sized NUMERIC quantities through the real row codecs" $
      withSchema databaseUrl $ \conn -> do
        ensureVaultPerformanceSchema conn
        let maximumUint256 = 2 ^ (256 :: Integer) - 1
            large = (vaultRow 0) {vpsSeniorTotalAssets = maximumUint256, vpsJuniorSharePriceWad = maximumUint256}
        upsertVaultPerformanceSnapshot conn large
        getVaultPerformanceSnapshots conn 421614 (address "1") (address "2") (address "3") 10
          `shouldReturn` [large]

    it "enforces hourly boundaries and canonical block identities in PostgreSQL" $
      withSchema databaseUrl $ \conn -> do
        ensureVaultPerformanceSchema conn
        upsertVaultPerformanceSnapshot conn $ vaultRow 0
        forM_
          [ (vaultRow 1) {vpsEpochTimestamp = 7201}
          , (vaultRow 1) {vpsBlockHash = "not-a-block-hash"}
          , (vaultRow 1) {vpsBlockTimestamp = 7201}
          ] $ \invalid -> do
            failed <- try $ upsertVaultPerformanceSnapshot conn invalid :: IO (Either SomeException ())
            failed `shouldSatisfy` either (const True) (const False)
        getVaultPerformanceSnapshots conn 421614 (address "1") (address "2") (address "3") 10
          `shouldReturn` [vaultRow 0]

    it "migrates legacy freshness and locked-asset columns and repairs the sampled row" $
      withSchema databaseUrl $ \conn -> do
        ensureVaultPerformanceSchema conn
        upsertVaultPerformanceSnapshot conn $ vaultRow 0
        void $ execute_ conn "ALTER TABLE vault_performance_snapshots DROP COLUMN mark_fresh, DROP COLUMN senior_locked_assets, DROP COLUMN junior_locked_assets"
        ensureVaultPerformanceSchema conn
        getVaultPerformanceSnapshots conn 421614 (address "1") (address "2") (address "3") 10
          `shouldReturn` [(vaultRow 0) {vpsMarkFresh = Nothing, vpsSeniorLockedAssets = Nothing, vpsJuniorLockedAssets = Nothing}]
        upsertVaultPerformanceSnapshot conn $ vaultRow 0
        getVaultPerformanceSnapshots conn 421614 (address "1") (address "2") (address "3") 10
          `shouldReturn` [vaultRow 0]

  describe "durable faucet claim transitions" $
    it "persists the exact transaction and finalizes only its matching durable hash" $
      withSchema databaseUrl $ \conn -> do
        ensureTestnetFaucetSchema conn
        beginTestnetFaucetClaim conn account 100 (address "6") `shouldReturn` True
        markTestnetFaucetClaimSubmitted conn account (address "6") (hash "a") "0x02abcd"
          `shouldReturn` True
        submitted <- getTestnetFaucetClaim conn account (address "6")
        fmap tfcStatus submitted `shouldBe` Just "submitted"
        fmap tfcTxHash submitted `shouldBe` Just (Just $ hash "a")
        fmap tfcRawTx submitted `shouldBe` Just (Just "0x02abcd")
        markTestnetFaucetClaimSuccess conn account (address "6") (hash "b") 123 `shouldReturn` False
        markTestnetFaucetClaimReverted conn account (address "6") (hash "b") "wrong transaction"
          `shouldReturn` False
        unchanged <- getTestnetFaucetClaim conn account (address "6")
        fmap tfcStatus unchanged `shouldBe` Just "submitted"
        fmap tfcRawTx unchanged `shouldBe` Just (Just "0x02abcd")
        markTestnetFaucetClaimSuccess conn account (address "6") (hash "a") 123 `shouldReturn` True
        completed <- getTestnetFaucetClaim conn account (address "6")
        fmap tfcStatus completed `shouldBe` Just "success"
        fmap tfcMintBlockNumber completed `shouldBe` Just (Just 123)
        fmap tfcRawTx completed `shouldBe` Just Nothing
        beginTestnetFaucetClaim conn account 100 (address "6") `shouldReturn` False

  describe "registration opening persistence" $
    it "records the requested privacy version and opening time together and rejects conflicts" $
      withSchema databaseUrl $ \conn -> do
        void $ execute_ conn "CREATE TABLE insights_competitions (slug TEXT PRIMARY KEY, registration_open_timestamp BIGINT, registration_close_timestamp BIGINT, privacy_notice_version TEXT, updated_at TIMESTAMPTZ, finalized BOOLEAN NOT NULL)"
        void $ execute_ conn "INSERT INTO insights_competitions VALUES ('registration',NULL,FLOOR(EXTRACT(EPOCH FROM NOW()))::BIGINT+3600,NULL,NOW(),FALSE)"
        openRegistrationIfConfigured conn "registration" "privacy-v1" `shouldReturn` True
        (query_ conn "SELECT privacy_notice_version,registration_open_timestamp IS NOT NULL FROM insights_competitions" :: IO [(Text,Bool)])
          `shouldReturn` [("privacy-v1",True)]
        openRegistrationIfConfigured conn "registration" "privacy-v1" `shouldReturn` True
        openRegistrationIfConfigured conn "registration" "privacy-v2" `shouldReturn` False
        (query_ conn "SELECT privacy_notice_version FROM insights_competitions" :: IO [Only Text])
          `shouldReturn` [Only "privacy-v1"]

  describe "Perps history relational projection" $
    it "matches each batched order to its own preceding activity and excludes other deployments and V1 rows" $
      withSchema databaseUrl $ \conn -> do
        ensurePerpsHistorySchema conn
        insertOrder conn router 1 (Just "client-1")
        insertOrder conn router 2 (Just "client-2")
        insertOrder conn router 3 Nothing
        insertOrder conn router 4 (Just "client-4")
        insertOrder conn (address "9") 9 (Just "other-release")
        insertFinalization conn 1 12
        insertFinalization conn 2 22
        insertFinalization conn 4 32
        insertActivity conn router "open-1" 11 101 (-17)
        insertActivity conn router "open-2" 21 102 (-23)
        insertActivity conn (address "9") "wrong-router" 10 999 999
        orders <- getPerpsOrdersByAccount conn 421614 router account 10 Nothing
        map porOrderId orders `shouldBe` [4,2,1]
        map porActivityPrice orders `shouldBe` [Nothing,Just 102,Just 101]
        map porActivityVpiAssets orders `shouldBe` [Nothing,Just (-23),Just (-17)]
        map porTerminalBlockHash orders `shouldBe` replicate 3 (Just $ hash "a")
        otherChain <- getPerpsOrdersByAccount conn 1 router account 10 Nothing
        map porOrderId otherChain `shouldBe` []

withSchema :: Text -> (Connection -> IO a) -> IO a
withSchema databaseUrl action = bracket (connectPostgreSQL $ TE.encodeUtf8 databaseUrl) close $ \conn -> do
  unique <- hashUnique <$> newUnique
  timestamp <- getMonotonicTimeNSec
  let schema = "bu_repair_" <> T.pack (show timestamp) <> "_" <> T.pack (show $ abs unique)
      sql = Query . TE.encodeUtf8
  void $ execute_ conn $ sql $ "CREATE SCHEMA " <> schema
  (do
    void $ execute_ conn $ sql $ "SET search_path TO " <> schema <> ", public"
    action conn) `finally` do
      void $ execute_ conn "SET search_path TO public"
      void $ execute_ conn $ sql $ "DROP SCHEMA " <> schema <> " CASCADE"

installStaticSchema :: Connection -> IO ()
installStaticSchema conn = do
  schema <- BS8.readFile "schema.sql"
  let sql = BS8.unlines $ filter (not . BS8.isPrefixOf "\\") $ BS8.lines schema
  void $ execute_ conn $ Query sql

expectFailureContaining :: String -> IO () -> Expectation
expectFailureContaining message action = do
  outcome <- try action :: IO (Either SomeException ())
  case outcome of
    Left err -> displayException err `shouldContain` message
    Right () -> expectationFailure $ "Startup accepted drift; expected " <> message

aaDrifts :: [(String, Query, String)]
aaDrifts =
  [ ("row security", "ALTER TABLE aa_reconciler_cursor ENABLE ROW LEVEL SECURITY", "table durability/row-security fingerprint mismatch")
  , ("column type", "ALTER TABLE aa_sponsorship_authorizations ALTER COLUMN sender TYPE VARCHAR(41)", "schema column fingerprint mismatch")
  , ("foreign key action", "ALTER TABLE aa_sponsorship_ledger DROP CONSTRAINT aa_sponsorship_ledger_digest_fkey; ALTER TABLE aa_sponsorship_ledger ADD CONSTRAINT aa_sponsorship_ledger_digest_fkey FOREIGN KEY (digest) REFERENCES aa_sponsorship_authorizations(digest) ON DELETE CASCADE", "schema key/foreign-key fingerprint mismatch")
  , ("same-name check", "ALTER TABLE aa_sponsorship_ledger DROP CONSTRAINT aa_ledger_invariants_ck; ALTER TABLE aa_sponsorship_ledger ADD CONSTRAINT aa_ledger_invariants_ck CHECK (entry_type IN ('reserve','release','actual_charge') AND amount_wei >= -1)", "schema check-constraint fingerprint mismatch")
  , ("same-name index", "DROP INDEX idx_aa_sponsorship_sender_state; CREATE INDEX idx_aa_sponsorship_sender_state ON aa_sponsorship_authorizations(owner,state)", "schema index fingerprint mismatch")
  , ("active request predicate", "DROP INDEX idx_aa_sponsorship_active_request_key; CREATE UNIQUE INDEX idx_aa_sponsorship_active_request_key ON aa_sponsorship_authorizations(request_key) WHERE state IN ('reserved','signed','submitted','settled')", "active request-key index predicate mismatch")
  , ("default", "ALTER TABLE aa_sponsorship_authorizations ALTER COLUMN created_at DROP DEFAULT", "schema default fingerprint mismatch")
  , ("null-distinct uniqueness", "ALTER TABLE aa_sponsorship_ledger DROP CONSTRAINT aa_sponsorship_ledger_digest_entry_type_key; ALTER TABLE aa_sponsorship_ledger ADD CONSTRAINT aa_sponsorship_ledger_digest_entry_type_key UNIQUE NULLS NOT DISTINCT(digest,entry_type)", "schema key/foreign-key fingerprint mismatch")
  ]

vaultRow :: Integer -> VaultPerformanceSnapshotRow
vaultRow index = VaultPerformanceSnapshotRow
  { vpsChainId = 421614, vpsHousePoolAddress = address "1", vpsSeniorVaultAddress = address "2", vpsJuniorVaultAddress = address "3"
  , vpsEpochTimestamp = (index+1)*3600, vpsBlockNumber = index+100, vpsBlockHash = hash "a", vpsBlockTimestamp = (index+1)*3600-1
  , vpsMarkFresh = Just True, vpsSeniorTotalAssets = 101, vpsSeniorLockedAssets = Just 102
  , vpsJuniorLockedAssets = Just 103, vpsSeniorTotalSupply = 104, vpsSeniorSharePriceWad = 105
  , vpsJuniorTotalAssets = 106, vpsJuniorTotalSupply = 107, vpsJuniorSharePriceWad = 108 }

address, hash :: Text -> Text
address digit = "0x" <> T.replicate 40 digit
hash digit = "0x" <> T.replicate 64 digit

router, account :: Text
router = address "4"
account = address "5"

insertOrder :: Connection -> Text -> Integer -> Maybe Text -> IO ()
insertOrder conn release orderId clientId = void $ execute conn
  "INSERT INTO perps_orders(chain_id,order_router,order_id,account,side,client_order_id,terminal_tx_hash,terminal_block_number,terminal_timestamp,terminal_status,receipt_economics) VALUES(421614,?,?,?,?,?,'batch-tx',100,1700000000,'Executed','{}')"
  (release,orderId,account,(0 :: Int),clientId)

insertFinalization :: Connection -> Integer -> Integer -> IO ()
insertFinalization conn orderId logIndex = void $ execute conn
  "INSERT INTO perps_events(chain_id,release_router,contract_address,event_name,tx_hash,block_number,block_hash,tx_index,log_index,timestamp,account,order_id,data) VALUES(421614,?,?,'OrderFinalized','batch-tx',100,?,0,?,1700000000,?,?,'{}')"
  (router,router,hash "a",logIndex,account,orderId)

insertActivity :: Connection -> Text -> Text -> Integer -> Integer -> Integer -> IO ()
insertActivity conn release key logIndex price vpi = void $ execute conn
  "INSERT INTO perps_account_activity(chain_id,release_router,event_key,account,activity_type,side,price,size_delta,pnl_assets,tx_hash,block_number,block_hash,tx_index,log_index,timestamp,data) VALUES(421614,?, ?,?,'Open',0,?,100,0,'batch-tx',100,?,0,?,1700000000,?)"
  (release,key,account,price,hash "a",logIndex,object ["vpiAssets" .= T.pack (show vpi)] :: Value)
