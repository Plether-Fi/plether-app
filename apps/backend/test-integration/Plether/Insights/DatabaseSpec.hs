module Plether.Insights.DatabaseSpec
  ( insightsDatabaseSpec
  ) where

import Control.Exception (bracket, finally)
import Control.Monad (void, forM_)
import Data.Aeson (Value (..), encode, object, (.=), eitherDecodeFileStrict', decode)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Base16 as Base16
import Data.IORef (newIORef)
import Data.Foldable (toList)
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.HTTP.Types (status200)
import Network.Wai (strictRequestBody, responseLBS)
import Network.Wai.Handler.Warp (testWithApplication)
import Plether.Config (PerpsCandleWriteMode (..))
import Plether.Perps.HistoryIndexer
  ( enrichSettlementReceipts, defaultPerpsAddresses, PerpsAddresses (..), PerpsIndexerConfig (..), PerpsIndexerMode (..),
    parseReplayLogEntry, parsePerpsLog, RpcLog (..), ParsedPerpsLog (..) )
import Data.List (find, sort)
import Data.Maybe (isJust)
import Data.Pool (destroyAllResources)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TextEncoding
import Data.Time (UTCTime, addUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Database.PostgreSQL.Simple (Connection, Only (..), execute, query, query_)
import Plether.Database.AaSponsorship (ensureAaSponsorshipSchema)
import Plether.Database.CloseAssistance
import Plether.Database (DbPool, newDbPool, withDb)
import Plether.Database.Insights
  ( AccountSnapshotInput (..)
  , CompetitionRow (..)
  , LeaderboardRow (..)
  , InsightsActivityRow (..)
  , SnapshotKind (..)
  , ensureInsightsSchema
  , getCompetitionLeaderboard
  , getCompetitionWallet
  , getCompetitionWalletActivity
  , getCurrentCompetition
  , hasCompleteAccountSnapshotBatch
  , insertManualAdjustment
  , materializeFinalizedStandings
  , publishAccountSnapshotBatch
  , refreshCompetitionIntegrityFlags
  , setCompetitionBoundaryBlocks
  , seedCompetition
  , stageCompetitionParticipantWalletRemap
  )
import Plether.Database.Insights.Registration (ensureRegistrationSchema)
import Plether.Database.Schema
  ( ensurePerpsHistorySchema
  , ensureTestnetFaucetSchema
  , deletePerpsHistoryFromBlock
  , insertPerpsEvent
  , insertPerpsActivity
  , insertPerpsUsdcTransfer
  , setPerpsIndexerState
  )
import Plether.Perps.IndexerFormat (PerpsIndexerFormat (..), indexerName)
import Plether.Insights.Competition
  ( CompetitionReleaseManifest (..)
  , CompetitionRules (..)
  , EquitySnapshot (..)
  , july2026Competition
  , september2026Competition
  , september2026ReleaseManifest
  )
import Test.Hspec

insightsDatabaseSpec :: Text -> Spec
insightsDatabaseSpec databaseUrl =
  describe "Plether Insights PostgreSQL lifecycle" $ do
    it "backfills settlement receipts idempotently without changing activity or scores" $
      withInsightsDatabase databaseUrl $ \pool -> do
        decoded <- eitherDecodeFileStrict' "../../scripts/fixtures/insights-close-waiver.json"
        fixture <- either fail pure decoded
        receipt <- case fixture of
          Object value | Just (Object receipt) <- KM.lookup "receipt" value -> pure receipt
          _ -> fail "Invalid fixture"
        let text key = case KM.lookup key receipt of Just (String value) -> value; _ -> ""
            tx = text "transactionHash"
            blockHash = text "blockHash"
            blockNumber = 309072000
            timestamp = 1789456609
        logs <- case KM.lookup "logs" receipt of
          Just (Array entries) -> either (fail . T.unpack) pure $ traverse parseReplayLogEntry (toList entries)
          _ -> fail "Missing logs"
        let finalized = [(entry, oid, account, receiptHash, economics, payload)
              | entry <- logs, Just (ParsedOrderFinalized oid account _ receiptHash _ _ _ _ _ economics payload) <- [parsePerpsLog entry]]
        withDb pool $ \conn -> forM_ finalized $ \(entry, oid, account, receiptHash, economics, payload) -> do
          void $ execute conn
            "INSERT INTO perps_orders (chain_id, order_router, order_id, account, terminal_tx_hash, terminal_block_number, \
            \terminal_timestamp, terminal_status, receipt_hash, receipt_economics) VALUES (?, ?, ?, ?, ?, ?, ?, 'Executed', ?, ?::jsonb)"
            (fixtureChain, fixtureRouter, oid, account, tx, blockNumber, timestamp, receiptHash, encode economics)
          insertPerpsEvent conn fixtureChain fixtureRouter (rlAddress entry) "OrderFinalized" tx blockNumber blockHash
            (rlTxIndex entry) (rlLogIndex entry) timestamp (Just account) (Just oid) Nothing payload
        let app request respond = do
              body <- strictRequestBody request
              let method = case decode body of Just (Object o) -> KM.lookup "method" o; _ -> Nothing
                  result = if method == Just (String "eth_getTransactionReceipt") then Object receipt else
                    object ["number" .= ("0x126c1080" :: Text), "hash" .= blockHash, "timestamp" .= ("0x6aa00000" :: Text)]
              respond $ responseLBS status200 [("Content-Type", "application/json")]
                (encode $ object ["jsonrpc" .= ("2.0" :: Text), "id" .= (1 :: Int), "result" .= result])
        testWithApplication (pure app) $ \port -> do
          manager <- newManager defaultManagerSettings
          req <- newIORef 1
          let cfg = PerpsIndexerConfig
                { picRpcUrls = ["http://127.0.0.1:" <> T.pack (show port)], picRpcAuthToken = Nothing,
                  picChainId = fixtureChain, picAddresses = defaultPerpsAddresses {paOrderRouter = fixtureRouter},
                  picStartBlock = 1, picConfirmations = 0, picBatchSize = 20, picPollIntervalMicros = 1000000,
                  picIndexerName = indexerName BoundedV2, picMode = PerpsIndexerOnce,
                  picCandleWriteMode = PerpsCandleWritesOff, picCandleLatenessSeconds = 0,
                  picDeploymentEnvironment = Nothing }
          enrichSettlementReceipts manager pool cfg req
          enrichSettlementReceipts manager pool cfg req
        withDb pool $ \conn -> do
          markers <- query conn "SELECT settlement_evidence_version FROM perps_orders WHERE chain_id = ? AND order_router = ?"
            (fixtureChain, fixtureRouter) :: IO [Only (Maybe Int)]
          markers `shouldBe` [Only (Just 1)]
          events <- query conn "SELECT data->>'protocolFeeCollectedUsdc' FROM perps_events WHERE chain_id = ? AND release_router = ? AND event_name = 'ActionChargeSettled'"
            (fixtureChain, fixtureRouter) :: IO [Only Text]
          events `shouldBe` [Only "222311115"]
          counts <- query conn "SELECT COUNT(*) FROM perps_account_activity WHERE chain_id = ? AND release_router = ?"
            (fixtureChain, fixtureRouter) :: IO [Only Int]
          counts `shouldBe` [Only 0]

    it "binds breakdowns to individual executions and invalidates receipt evidence on reorg" $
      withInsightsDatabase databaseUrl $ \pool -> withDb pool $ \conn -> do
        insertParticipant conn walletA "trader-a"
        setCompetitionBoundaryBlocks conn competitionSlug
          (Just (startBlock, startHash, baselineHash)) (Just (finalBlock, finalHash))
        publishAccountSnapshotBatch conn [snapshot walletA SnapshotStart baselineBlock baselineHash baselineTimestamp bankroll]
        publishAccountSnapshotBatch conn [snapshot walletA SnapshotLive liveBlock liveHash liveTimestamp bankroll]
        let tx = hashText "breakdown-tx"
            receiptHash = hashText "breakdown-receipt"
            receipt = object ["executionFeeUsdc" .= ("1000000" :: Text)]
        forM_ [(1, 10, 20), (2, 30, 40)] $ \(oid, activityIndex, terminalIndex) -> do
          insertPerpsActivity conn fixtureChain fixtureRouter fixtureRouter ("breakdown-" <> T.pack (show oid))
            walletA "Close" Nothing Nothing (Just 1) (Just 100000000) (Just 1000000000000000000) Nothing (Just 0)
            tx liveBlock liveHash 0 activityIndex liveTimestamp (object [])
          void $ execute conn
            "INSERT INTO perps_orders (chain_id, order_router, order_id, account, terminal_tx_hash, terminal_block_number, \
            \terminal_timestamp, terminal_status, receipt_hash, receipt_economics, settlement_evidence_version) \
            \VALUES (?, ?, ?, ?, ?, ?, ?, 'Executed', ?, ?::jsonb, 1)"
            (fixtureChain, fixtureRouter, oid :: Integer, walletA, tx, liveBlock, liveTimestamp, receiptHash, encode receipt)
          insertPerpsEvent conn fixtureChain fixtureRouter fixtureRouter "OrderFinalized" tx liveBlock liveHash 0 terminalIndex
            liveTimestamp (Just walletA) (Just oid) Nothing
            (object ["receiptHash" .= receiptHash, "status" .= (2 :: Int), "terminalReason" .= ("Executed" :: Text)])
          insertPerpsEvent conn fixtureChain fixtureRouter fixtureRouter "ActionChargeSettled" tx liveBlock liveHash 0 (activityIndex - 1)
            liveTimestamp (Just walletA) (Just oid) Nothing
            (object ["assessedUsdc" .= ("1000000" :: Text), "recoveredUsdc" .= ("900000" :: Text), "waivedUsdc" .= ("100000" :: Text)])
        rows <- getCompetitionWalletActivity conn competitionSlug walletA 20
        let orderId row = case iarExecution row of
              Just (Object execution) -> KM.lookup "orderId" execution
              _ -> Nothing
        map orderId rows `shouldBe` [Just (String "2"), Just (String "1")]
        -- A second position event in one terminal interval makes the match ambiguous.
        insertPerpsActivity conn fixtureChain fixtureRouter fixtureRouter "breakdown-ambiguous"
          walletA "Open" Nothing Nothing (Just 1) (Just 100000000) (Just 1000000000000000000) Nothing Nothing
          tx liveBlock liveHash 0 31 liveTimestamp (object [])
        ambiguous <- getCompetitionWalletActivity conn competitionSlug walletA 20
        map orderId ambiguous `shouldBe` [Nothing, Nothing, Just (String "1")]
        -- Canonical block identity, not merely tx/account, is required.
        void $ execute conn "UPDATE perps_events SET block_hash = ? WHERE chain_id = ? AND release_router = ? AND event_name = 'OrderFinalized'"
          (hashText "other-fork", fixtureChain, fixtureRouter)
        mismatched <- getCompetitionWalletActivity conn competitionSlug walletA 20
        map iarExecution mismatched `shouldBe` replicate 3 Nothing
        deletePerpsHistoryFromBlock conn fixtureChain fixtureRouter liveBlock
        markers <- query conn "SELECT settlement_evidence_version, receipt_economics FROM perps_orders WHERE chain_id = ? AND order_router = ? ORDER BY order_id"
          (fixtureChain, fixtureRouter) :: IO [(Maybe Int, Maybe Value)]
        markers `shouldBe` [(Nothing, Nothing), (Nothing, Nothing)]
        getCompetitionWalletActivity conn competitionSlug walletA 20 `shouldReturn` []

    it "binds pending v1.2.3 during trading without changing the roster or schedule" $
      withPendingV123Competition databaseUrl $ \conn rules -> do
        insertParticipant conn walletA "existing-registration"
        before <- requireCompetition conn competitionSlug
        seedRelease conn rules september2026ReleaseManifest
        after <- requireCompetition conn competitionSlug
        icrReleaseReady after `shouldBe` True
        icrReleaseRouter after `shouldBe` crmOrderRouter september2026ReleaseManifest
        icrStartTimestamp after `shouldBe` icrStartTimestamp before
        icrScoreCutoffTimestamp after `shouldBe` icrScoreCutoffTimestamp before
        rows <- query conn
          "SELECT wallet, trader_reference FROM insights_competition_participants WHERE competition_slug = ?"
          (Only competitionSlug) :: IO [(Text, Text)]
        rows `shouldBe` [(walletA, "existing-registration")]
        seedRelease conn rules september2026ReleaseManifest
        requireCompetition conn competitionSlug `shouldReturn` after

    it "rejects a different release during late binding" $
      withPendingV123Competition databaseUrl $ \conn rules -> do
        seedRelease conn rules (september2026ReleaseManifest {crmAccountLens = fixtureLens})
          `shouldThrow` anyIOException
        icrReleaseReady <$> requireCompetition conn competitionSlug `shouldReturn` False

    it "rejects late binding after baseline resolution" $
      withPendingV123Competition databaseUrl $ \conn rules -> do
        void $ execute conn "UPDATE insights_competitions SET start_block = 307400000 WHERE slug = ?" (Only competitionSlug)
        seedRelease conn rules september2026ReleaseManifest `shouldThrow` anyIOException
        icrReleaseReady <$> requireCompetition conn competitionSlug `shouldReturn` False

    it "rejects late binding when the stored schedule differs" $
      withPendingV123Competition databaseUrl $ \conn rules -> do
        seedRelease conn (rules {crStartAt = addUTCTime 1 $ crStartAt rules}) september2026ReleaseManifest
          `shouldThrow` anyIOException
        icrReleaseReady <$> requireCompetition conn competitionSlug `shouldReturn` False

    it "rejects late binding after scoring closes" $
      withPendingV123Competition databaseUrl $ \conn rules -> do
        let closed = rules
              { crNewRiskCutoffAt = crStartAt rules
              , crScoreCutoffAt = crStartAt rules
              }
        void $ execute conn
          "UPDATE insights_competitions SET new_risk_cutoff_timestamp = start_timestamp, score_cutoff_timestamp = start_timestamp WHERE slug = ?"
          (Only competitionSlug)
        seedRelease conn closed september2026ReleaseManifest `shouldThrow` anyIOException
        icrReleaseReady <$> requireCompetition conn competitionSlug `shouldReturn` False

    it "preserves finalized July data while selecting the configured September competition" $
      withInsightsDatabase databaseUrl $ \pool -> withDb pool $ \conn -> do
        void $ execute conn
          "UPDATE insights_competitions SET finalized = TRUE WHERE slug = ?"
          (Only $ crSlug july2026Competition)
        julyBefore <- requireCompetition conn $ crSlug july2026Competition

        -- A normal September restart must validate only that row and must not
        -- reinterpret or refresh finalized July history.
        ensureInsightsSchema
          conn testSeptemberRules fixtureChain fixtureRouter fixtureUsdc fixtureClearinghouse fixtureLens fixtureManifest
        julyAfter <- requireCompetition conn $ crSlug july2026Competition
        current <- getCurrentCompetition conn $ crSlug testSeptemberRules

        julyAfter `shouldBe` julyBefore
        fmap icrSlug current `shouldBe` Just (crSlug testSeptemberRules)
        fmap icrFinalized current `shouldBe` Just False

    it "preserves completed registrations while relaxing the September X-account age from 90 to 30 days" $
      withInsightsDatabase databaseUrl $ \pool -> withDb pool $ \conn -> do
        now <- getCurrentTime
        let currentRules = prelaunchAgeMigrationRules now
            legacyRules = currentRules {crMinimumXAccountAgeDays = Just 90}
            slug = crSlug currentRules
            registrationId = "11111111-1111-4111-8111-111111111111" :: Text
        void $ execute conn "DELETE FROM insights_competitions WHERE slug = ?" (Only slug)
        ensureInsightsSchema
          conn legacyRules fixtureChain fixtureRouter fixtureUsdc fixtureClearinghouse fixtureLens fixtureManifest
        ensureRegistrationSchema conn
        void $ execute conn
          "INSERT INTO insights_registration_applications (\
          \ registration_id, competition_slug, status, turnstile_token_digest,\
          \ email_key_version, email_nonce, email_ciphertext, email_tag, email_digest, email_masked,\
          \ x_user_id_digest, x_username, x_created_timestamp, x_identity_verified_at, x_follow_verified_at,\
          \ owner_wallet, trading_account, wallet_verification_block, wallet_verification_block_hash, wallet_verified_at,\
          \ rules_version, privacy_version, completed_at)\
          \ VALUES (?::uuid, ?, 'completed', decode(repeat('01', 32), 'hex'),\
          \ 'v1', decode(repeat('02', 12), 'hex'), decode('03', 'hex'), decode(repeat('04', 16), 'hex'),\
          \ decode(repeat('05', 32), 'hex'), 'f***@example.test',\
          \ decode(repeat('06', 32), 'hex'), 'fixture_trader', 0, NOW(), NOW(),\
          \ ?, ?, 1, ?, NOW(), ?, 'fixture-v1', NOW())"
          (registrationId, slug, walletA, walletB, hashText "registration-block", crRulesVersion currentRules)
        insertParticipantFor conn slug walletA registrationId

        ensureInsightsSchema
          conn currentRules fixtureChain fixtureRouter fixtureUsdc fixtureClearinghouse fixtureLens fixtureManifest

        migrated <- query conn
          "SELECT c.minimum_x_account_age_days, a.status, a.rules_version,\
          \ (SELECT COUNT(*) FROM insights_competition_participants p WHERE p.competition_slug = c.slug)\
          \ FROM insights_competitions c JOIN insights_registration_applications a\
          \ ON a.competition_slug = c.slug WHERE c.slug = ? AND a.registration_id = ?::uuid"
          (slug, registrationId) :: IO [(Maybe Int, Text, Text, Integer)]
        migrated `shouldBe` [(Just 30, "completed", crRulesVersion currentRules, 1)]

    it "fails closed instead of freezing scoreless legacy standings" $
      withInsightsDatabase databaseUrl $ \pool -> withDb pool $ \conn -> do
        insertParticipantFor conn (crSlug july2026Competition) walletA "legacy-a"
        void $ execute conn
          "UPDATE insights_competitions SET finalized = TRUE WHERE slug = ?"
          (Only $ crSlug july2026Competition)
        ensureInsightsSchema
          conn testSeptemberRules fixtureChain fixtureRouter fixtureUsdc fixtureClearinghouse fixtureLens fixtureManifest
          `shouldThrow` anyIOException

    it "blocks manual wallet remapping for a verified-registration roster" $
      withInsightsDatabase databaseUrl $ \pool -> withDb pool $ \conn -> do
        insertParticipant conn walletA "trader-a"
        void $ execute conn
          "UPDATE insights_competitions SET registration_close_timestamp = ?,\
          \ minimum_x_account_age_days = 30, target_x_handle = 'plether_fi' WHERE slug = ?"
          (startTimestamp - 1, competitionSlug)
        result <- stageCompetitionParticipantWalletRemap
          conn competitionSlug "trader-a" walletA walletB
        result `shouldBe`
          Left "The competition is missing, finalized, or uses verified first-party registration; wallet remaps are locked"

    it "preserves published standings during late-roster rebuilds and accepts an all-zero final batch" $
      withInsightsDatabase databaseUrl $ \pool -> withDb pool $ \conn -> do
        insertParticipant conn walletA "trader-a"
        setCompetitionBoundaryBlocks
          conn competitionSlug
          (Just (startBlock, startHash, baselineHash))
          (Just (finalBlock, finalHash))
        insertTrade conn walletA 103 3

        publishAccountSnapshotBatch conn
          [snapshot walletA SnapshotStart baselineBlock baselineHash baselineTimestamp bankroll]
        publishAccountSnapshotBatch conn
          [snapshot walletA SnapshotLive liveBlock liveHash liveTimestamp (bankroll + gain)]
        hasCompleteAccountSnapshotBatch conn competitionSlug SnapshotStart baselineBlock baselineHash
          `shouldReturn` True
        hasCompleteAccountSnapshotBatch conn competitionSlug SnapshotLive liveBlock liveHash
          `shouldReturn` True
        initialRows <- getCompetitionLeaderboard conn competitionSlug Nothing 20 0
        original <- requireWallet walletA initialRows
        ilrRank original `shouldBe` Just 1
        ilrFinalPnlUsdc original `shouldBe` Just gain
        ilrExecutedTrades original `shouldBe` 1
        activityBefore <- getCompetitionWalletActivity conn competitionSlug walletA 20
        activityBefore `shouldSatisfy` (not . null)

        insertParticipant conn walletB "trader-b"
        hasCompleteAccountSnapshotBatch conn competitionSlug SnapshotStart baselineBlock baselineHash
          `shouldReturn` False
        hasCompleteAccountSnapshotBatch conn competitionSlug SnapshotLive liveBlock liveHash
          `shouldReturn` False
        publishAccountSnapshotBatch conn
          [snapshot walletA SnapshotLive liveBlock liveHash liveTimestamp (bankroll + gain)]
          `shouldThrow` anyIOException
        during <- getCompetitionLeaderboard conn competitionSlug Nothing 20 0
        existing <- requireWallet walletA during
        existing `shouldBe` original
        newcomer <- requireWallet walletB during
        ilrRank newcomer `shouldBe` Nothing
        ilrFinalPnlUsdc newcomer `shouldBe` Nothing
        getCompetitionWallet conn competitionSlug walletA `shouldReturn` Just original
        getCompetitionWalletActivity conn competitionSlug walletA 20
          `shouldReturn` activityBefore

        publishAccountSnapshotBatch conn
          [ snapshot walletA SnapshotStart baselineBlock baselineHash baselineTimestamp bankroll
          , snapshot walletB SnapshotStart baselineBlock baselineHash baselineTimestamp 0
          ]
        -- A failed/delayed live capture must not erase existing scores after
        -- the expanded baseline has already been published.
        baselineOnly <- getCompetitionLeaderboard conn competitionSlug Nothing 20 0
        requireWallet walletA baselineOnly `shouldReturn` original
        waiting <- requireWallet walletB baselineOnly
        ilrFinalPnlUsdc waiting `shouldBe` Nothing
        publishAccountSnapshotBatch conn
          [ snapshot walletA SnapshotLive liveBlock liveHash liveTimestamp (bankroll + gain)
          , snapshot walletB SnapshotLive liveBlock liveHash liveTimestamp (bankroll + gain)
          ]
        hasCompleteAccountSnapshotBatch conn competitionSlug SnapshotStart baselineBlock baselineHash
          `shouldReturn` True
        hasCompleteAccountSnapshotBatch conn competitionSlug SnapshotLive liveBlock liveHash
          `shouldReturn` True
        rebuilt <- getCompetitionLeaderboard conn competitionSlug Nothing 20 0
        joined <- requireWallet walletB rebuilt
        ilrFinalPnlUsdc joined `shouldBe` Just (bankroll + gain)

        -- A successful exact lens read can legitimately return zero state for
        -- the entire roster. It must replace the earlier stateful batch.
        publishAccountSnapshotBatch conn
          [ snapshot walletA SnapshotFinal finalBlock finalHash finalTimestamp 0
          , snapshot walletB SnapshotFinal finalBlock finalHash finalTimestamp 0
          ]
        hasCompleteAccountSnapshotBatch conn competitionSlug SnapshotFinal finalBlock finalHash
          `shouldReturn` True
        rows <- getCompetitionLeaderboard conn competitionSlug Nothing 20 0
        sort (map ilrCurrentAccountValueUsdc rows) `shouldBe` [Just 0, Just 0]

    it "recaptures unversioned September snapshots without invalidating finalized results" $
      withInsightsDatabase databaseUrl $ \pool -> withDb pool $ \conn -> do
        insertParticipant conn walletA "trader-a"
        setCompetitionBoundaryBlocks conn competitionSlug
          (Just (startBlock, startHash, baselineHash)) (Just (finalBlock, finalHash))
        let corrected = snapshot walletA SnapshotStart baselineBlock baselineHash baselineTimestamp bankroll
            old = corrected { asiRawData = object ["pendingOrderCount" .= ("0" :: Text)] }
        publishAccountSnapshotBatch conn [old]
        hasCompleteAccountSnapshotBatch conn competitionSlug SnapshotStart baselineBlock baselineHash
          `shouldReturn` False
        publishAccountSnapshotBatch conn [corrected]
        hasCompleteAccountSnapshotBatch conn competitionSlug SnapshotStart baselineBlock baselineHash
          `shouldReturn` True
        publishAccountSnapshotBatch conn [old]
        void $ execute conn "UPDATE insights_competitions SET finalized = TRUE WHERE slug = ?" (Only competitionSlug)
        hasCompleteAccountSnapshotBatch conn competitionSlug SnapshotStart baselineBlock baselineHash
          `shouldReturn` True

    it "serves immutable materialized standings after a canonical history rebuild" $
      withInsightsDatabase databaseUrl $ \pool -> withDb pool $ \conn -> do
        insertParticipant conn walletA "trader-a"
        setCompetitionBoundaryBlocks conn competitionSlug
          (Just (startBlock, startHash, baselineHash)) (Just (finalBlock, finalHash))
        seedOfficialAllocation conn walletA 80 90 "immutable-a"
        publishAccountSnapshotBatch conn
          [snapshot walletA SnapshotStart baselineBlock baselineHash baselineTimestamp bankroll]
        publishAccountSnapshotBatch conn
          [snapshot walletA SnapshotFinal finalBlock finalHash finalTimestamp (bankroll + gain)]
        refreshCompetitionIntegrityFlags conn competitionSlug
        materializeFinalizedStandings conn competitionSlug `shouldReturn` Right 1
        void $ execute conn "UPDATE insights_competitions SET finalized = TRUE WHERE slug = ?" (Only competitionSlug)
        frozen <- getCompetitionLeaderboard conn competitionSlug Nothing 20 0
        frozenWallet <- getCompetitionWallet conn competitionSlug walletA

        deletePerpsHistoryFromBlock conn fixtureChain fixtureRouter 1
        afterRebuild <- getCompetitionLeaderboard conn competitionSlug Nothing 20 0
        afterWallet <- getCompetitionWallet conn competitionSlug walletA
        afterRebuild `shouldBe` frozen
        afterWallet `shouldBe` frozenWallet

    it "executes canonical cash-flow P&L, funding provenance, ranking, and strict asset filtering" $
      withInsightsDatabase databaseUrl $ \pool -> withDb pool $ \conn -> do
        mapM_ (uncurry $ insertParticipant conn)
          [(walletA, "trader-a"), (walletB, "trader-b"), (walletC, "trader-c")]
        setCompetitionBoundaryBlocks
          conn competitionSlug
          (Just (startBlock, startHash, baselineHash))
          Nothing

        -- A and C are officially prefunded before the canonical baseline.
        seedOfficialAllocation conn walletA 80 90 "prefund-a"
        seedOfficialAllocation conn walletC 81 91 "prefund-c"
        insertTransfer conn attacker walletC 1 70 7 "pre-mint-dust-in"
        insertTransfer conn walletC attacker 1 71 8 "pre-mint-dust-out"
        -- B has a zero baseline and exactly one official allocation before its
        -- first trade.
        seedOfficialAllocation conn walletB 101 102 "postfund-b"
        insertTrade conn walletB 103 3

        publishAccountSnapshotBatch conn
          [ snapshot walletA SnapshotStart baselineBlock baselineHash baselineTimestamp bankroll
          , snapshot walletB SnapshotStart baselineBlock baselineHash baselineTimestamp 0
          , snapshot walletC SnapshotStart baselineBlock baselineHash baselineTimestamp bankroll
          ]
        publishAccountSnapshotBatch conn
          [ snapshot walletA SnapshotLive liveBlock liveHash liveTimestamp (bankroll + gain)
          , snapshot walletB SnapshotLive liveBlock liveHash liveTimestamp (bankroll + gain)
          , snapshot walletC SnapshotLive liveBlock liveHash liveTimestamp (bankroll - loss)
          ]

        -- September ignores the legacy manual-adjustment mechanism entirely.
        adjustment <- insertManualAdjustment conn competitionSlug walletA (999 * usdcScale) "fixture" "integration"
        adjustment `shouldSatisfy` isJust
        initial <- getCompetitionLeaderboard conn competitionSlug Nothing 20 0
        a <- requireWallet walletA initial
        b <- requireWallet walletB initial
        c <- requireWallet walletC initial
        ilrFinalPnlUsdc a `shouldBe` Just gain
        ilrFinalPnlUsdc b `shouldBe` Just gain
        ilrFinalPnlUsdc c `shouldBe` Just (negate loss)
        ilrDepositsUsdc a `shouldBe` 0
        ilrDepositsUsdc b `shouldBe` bankroll
        ilrManualAdjustmentsUsdc a `shouldBe` 0
        ilrFundingIntegrityClear a `shouldBe` True
        ilrFundingIntegrityClear b `shouldBe` True
        ilrRank a `shouldBe` Just 1
        ilrRank b `shouldBe` Just 1
        ilrRank c `shouldBe` Just 3

        -- A reorg removes the canonical zero-address mint even though the
        -- faucet receipt remains. Receipt-only provenance must fail closed;
        -- replaying the exact canonical transfer restores it.
        void $ execute conn
          "DELETE FROM perps_usdc_transfers WHERE chain_id = ? AND release_router = ? AND tx_hash = ?"
          (fixtureChain, fixtureRouter, hashText "faprefund-c")
        refreshCompetitionIntegrityFlags conn competitionSlug
        afterStaleMint <- getCompetitionLeaderboard conn competitionSlug Nothing 20 0
        ilrFundingIntegrityClear (requireWalletUnsafe walletC afterStaleMint) `shouldBe` False
        insertMintTransfer conn walletC 81 "prefund-c"
        refreshCompetitionIntegrityFlags conn competitionSlug
        afterMintReplay <- getCompetitionLeaderboard conn competitionSlug Nothing 20 0
        ilrFundingIntegrityClear (requireWalletUnsafe walletC afterMintReplay) `shouldBe` True

        -- Positive third-party dust which remains outside the clearinghouse
        -- is non-blocking; it becomes blocking only when used by a Deposit.
        insertTransfer conn attacker walletC 1 106 6 "idle-dust"
        refreshCompetitionIntegrityFlags conn competitionSlug
        afterIdleDust <- getCompetitionLeaderboard conn competitionSlug Nothing 20 0
        ilrFundingIntegrityClear (requireWalletUnsafe walletC afterIdleDust) `shouldBe` True

        -- A same-tx/same-amount piggyback transfer destroys the one-to-one
        -- Deposit pairing and proves dust/substitute capital was used.
        insertPerpsUsdcTransfer conn fixtureChain fixtureRouter fixtureUsdc walletC fixtureClearinghouse bankroll
          (hashText "txprefund-c") 91 (hashText "blprefund-c") 0 2 (eventTimestamp 91)
        refreshCompetitionIntegrityFlags conn competitionSlug
        afterPiggyback <- getCompetitionLeaderboard conn competitionSlug Nothing 20 0
        ilrFundingIntegrityClear (requireWalletUnsafe walletC afterPiggyback) `shouldBe` False

        -- Missing and wrong asset provenance must be excluded from displayed
        -- cash flow and must independently block integrity eligibility.
        insertDeposit conn walletB 104 4 (Just fixtureClearinghouse) Nothing (7 * usdcScale) "missing-asset"
        insertDeposit conn walletB 105 5 (Just fixtureClearinghouse) (Just wrongAsset) (9 * usdcScale) "wrong-asset"
        refreshCompetitionIntegrityFlags conn competitionSlug
        afterMalformed <- getCompetitionLeaderboard conn competitionSlug Nothing 20 0
        malformed <- requireWallet walletB afterMalformed
        ilrDepositsUsdc malformed `shouldBe` bankroll
        ilrFinalPnlUsdc malformed `shouldBe` Just gain
        ilrFundingIntegrityClear malformed `shouldBe` False

        -- Moving the claimed mint away and replacing it with unrelated USDC
        -- cannot reuse the faucet entitlement to bless later capital.
        insertTransfer conn walletA attacker bankroll 85 6 "official-out"
        insertTransfer conn attacker walletA bankroll 86 7 "unofficial-in"
        refreshCompetitionIntegrityFlags conn competitionSlug
        afterSubstitution <- getCompetitionLeaderboard conn competitionSlug Nothing 20 0
        ilrFundingIntegrityClear (requireWalletUnsafe walletA afterSubstitution) `shouldBe` False


    it "subtracts verified assistance from PnL without treating it as extra bankroll, including late registration" $
      withInsightsDatabase databaseUrl $ \pool -> withDb pool $ \conn -> do
        seedOfficialAllocation conn walletA 80 90 "prefund-a"
        insertDeposit conn walletA 105 4 (Just fixtureClearinghouse) (Just fixtureUsdc) 198000 "assisted"
        let digest = "0x" <> T.replicate 64 "a"
            grant = CloseAssistanceReservation fixtureRouter walletA ("0x" <> T.replicate 64 "b") ("0x" <> T.replicate 64 "c") fixtureLens 198000
        void $ execute conn
          "INSERT INTO aa_sponsorship_authorizations(request_key,digest,sender,owner,nonce,valid_after,valid_until,max_cost_wei,client_key,operation,state) VALUES(?,?,?,?,0,1,2,1,?,'{}','settled') ON CONFLICT DO NOTHING"
          (digest,digest,walletA,walletA,digest)
        insertCloseAssistanceReservation conn digest grant
        -- Registration follows assistance: provenance is attached to the account and exact event.
        insertParticipant conn walletA "assisted-trader"
        setCompetitionBoundaryBlocks conn competitionSlug (Just (startBlock,startHash,baselineHash)) Nothing
        publishAccountSnapshotBatch conn [snapshot walletA SnapshotStart baselineBlock baselineHash baselineTimestamp bankroll]
        publishAccountSnapshotBatch conn [snapshot walletA SnapshotLive liveBlock liveHash liveTimestamp (bankroll + gain + 198000)]
        refreshCompetitionIntegrityFlags conn competitionSlug
        unverified <- getCompetitionLeaderboard conn competitionSlug Nothing 20 0
        ilrFundingIntegrityClear (requireWalletUnsafe walletA unverified) `shouldBe` False
        confirmCloseAssistance conn digest (hashText "txassisted") 105 (hashText "blassisted") 4 1 `shouldReturn` True
        refreshCompetitionIntegrityFlags conn competitionSlug
        verified <- getCompetitionLeaderboard conn competitionSlug Nothing 20 0
        let row = requireWalletUnsafe walletA verified
        ilrFundingIntegrityClear row `shouldBe` True
        ilrDepositsUsdc row `shouldBe` 198000
        ilrFinalPnlUsdc row `shouldBe` Just gain
        insertDeposit conn walletA 106 4 (Just fixtureClearinghouse) (Just fixtureUsdc) 1 "unrelated"
        refreshCompetitionIntegrityFlags conn competitionSlug
        unrelated <- getCompetitionLeaderboard conn competitionSlug Nothing 20 0
        ilrFundingIntegrityClear (requireWalletUnsafe walletA unrelated) `shouldBe` False
        void $ execute conn "DELETE FROM aa_close_assistance WHERE digest=?" (Only digest)
        void $ execute conn "DELETE FROM aa_sponsorship_authorizations WHERE digest=?" (Only digest)

seedRelease :: Connection -> CompetitionRules -> CompetitionReleaseManifest -> IO ()
seedRelease conn rules manifest =
  seedCompetition conn rules (crmChainId manifest) (crmOrderRouter manifest)
    (crmUsdc manifest) (crmMarginClearinghouse manifest) (crmAccountLens manifest) manifest

withPendingV123Competition :: Text -> (Connection -> CompetitionRules -> IO a) -> IO a
withPendingV123Competition databaseUrl action =
  withInsightsDatabase databaseUrl $ \pool -> withDb pool $ \conn -> do
    now <- getCurrentTime
    let rules = testSeptemberRules
          { crStartAt = addUTCTime (-3600) now
          , crNewRiskCutoffAt = addUTCTime 3600 now
          , crScoreCutoffAt = addUTCTime 3600 now
          }
    void $ execute conn "DELETE FROM insights_competitions WHERE slug = ?" (Only competitionSlug)
    seedRelease conn rules (september2026ReleaseManifest {crmReleaseId = "release-pending"})
    action conn rules

withInsightsDatabase :: Text -> (DbPool -> IO a) -> IO a
withInsightsDatabase databaseUrl action =
  bracket (newDbPool databaseUrl) destroyAllResources $ \pool -> do
    assertDedicatedDatabase pool
    prepareDatabase pool
    action pool `finally` cleanupDatabase pool

assertDedicatedDatabase :: DbPool -> IO ()
assertDedicatedDatabase pool = withDb pool $ \conn -> do
  names <- query_ conn "SELECT current_database()" :: IO [Only Text]
  case names of
    [Only name]
      | "critical_path" `T.isInfixOf` T.toLower name -> pure ()
    _ -> fail "Insights integration tests require a dedicated critical_path PostgreSQL database"

prepareDatabase :: DbPool -> IO ()
prepareDatabase pool = withDb pool $ \conn -> do
  ensureAaSponsorshipSchema conn
  ensureTestnetFaucetSchema conn
  ensurePerpsHistorySchema conn
  -- Install all tables using the historical rule first, then add the new
  -- versioned row. Registration metadata is disabled in this disposable clone
  -- so the fixture remains runnable after the real registration deadline.
  ensureInsightsSchema
    conn july2026Competition fixtureChain fixtureRouter fixtureUsdc fixtureClearinghouse fixtureLens fixtureManifest
  cleanupRows conn
  ensureInsightsSchema
    conn july2026Competition fixtureChain fixtureRouter fixtureUsdc fixtureClearinghouse fixtureLens fixtureManifest
  ensureInsightsSchema
    conn testSeptemberRules fixtureChain fixtureRouter fixtureUsdc fixtureClearinghouse fixtureLens fixtureManifest
  setPerpsIndexerState
    conn fixtureChain (indexerName LegacyV1) fixtureRouter 1 cursorBlock (Just cursorHash)
  setPerpsIndexerState
    conn fixtureChain (indexerName BoundedV2) fixtureRouter 1 cursorBlock (Just cursorHash)

cleanupDatabase :: DbPool -> IO ()
cleanupDatabase pool = withDb pool cleanupRows

cleanupRows :: Connection -> IO ()
cleanupRows conn = do
  void $ execute conn "DELETE FROM perps_events WHERE chain_id = ? AND release_router = ?" (fixtureChain, fixtureRouter)
  void $ execute conn "DELETE FROM perps_orders WHERE chain_id = ? AND order_router = ?" (fixtureChain, fixtureRouter)
  void $ execute conn
    "DELETE FROM insights_competitions WHERE slug IN (?, ?)"
    (crSlug july2026Competition, competitionSlug)
  void $ execute conn
    "DELETE FROM perps_account_activity WHERE chain_id = ? AND release_router = ?"
    (fixtureChain, fixtureRouter)
  void $ execute conn
    "DELETE FROM perps_usdc_transfers WHERE chain_id = ? AND release_router = ?"
    (fixtureChain, fixtureRouter)
  void $ execute conn
    "DELETE FROM testnet_faucet_claims WHERE token_address = ? AND address IN (?, ?, ?)"
    (fixtureUsdc, walletA, walletB, walletC)
  void $ execute conn
    "DELETE FROM perps_indexer_state WHERE chain_id = ? AND release_router = ?"
    (fixtureChain, fixtureRouter)

insertParticipant :: Connection -> Text -> Text -> IO ()
insertParticipant conn wallet reference =
  insertParticipantFor conn competitionSlug wallet reference

insertParticipantFor :: Connection -> Text -> Text -> Text -> IO ()
insertParticipantFor conn slug wallet reference =
  void $ execute conn
    "INSERT INTO insights_competition_participants\
    \ (competition_slug, wallet, trader_reference, alias) VALUES (?, ?, ?, ?)"
    (slug, wallet, reference, Just reference)

seedOfficialAllocation :: Connection -> Text -> Integer -> Integer -> Text -> IO ()
seedOfficialAllocation conn wallet mintBlock depositBlock suffix = do
  let faucetTx = hashText $ "fa" <> suffix
  void $ execute conn
    "INSERT INTO testnet_faucet_claims\
    \ (address, amount, token_address, tx_hash, mint_block_number, status)\
    \ VALUES (?, ?, ?, ?, ?, 'success')"
    (wallet, bankroll, fixtureUsdc, faucetTx, mintBlock)
  insertMintTransfer conn wallet mintBlock suffix
  insertDeposit
    conn wallet depositBlock 1 (Just fixtureClearinghouse) (Just fixtureUsdc) bankroll suffix

insertMintTransfer :: Connection -> Text -> Integer -> Text -> IO ()
insertMintTransfer conn wallet mintBlock suffix =
  insertPerpsUsdcTransfer
    conn fixtureChain fixtureRouter fixtureUsdc zeroAddress wallet bankroll (hashText $ "fa" <> suffix) mintBlock
    (hashText $ "mint" <> suffix) 0 0 (eventTimestamp mintBlock)

insertDeposit
  :: Connection
  -> Text
  -> Integer
  -> Integer
  -> Maybe Text
  -> Maybe Text
  -> Integer
  -> Text
  -> IO ()
insertDeposit conn wallet blockNumber logIndex emitter asset amount suffix =
  let txHash = hashText $ "tx" <> suffix
      blockHash = hashText $ "bl" <> suffix
   in do
    case (emitter, asset) of
      (Just emitterAddress, Just assetAddress)
        | T.toLower emitterAddress == T.toLower fixtureClearinghouse
        , T.toLower assetAddress == T.toLower fixtureUsdc ->
            insertPerpsUsdcTransfer conn fixtureChain fixtureRouter fixtureUsdc wallet fixtureClearinghouse amount
              txHash blockNumber blockHash 0 (max 0 $ logIndex - 1) (eventTimestamp blockNumber)
      _ -> pure ()
    insertPerpsActivity
      conn fixtureChain fixtureRouter (maybe wrongEmitter id emitter) ("insights:" <> suffix)
      wallet "Deposit" Nothing Nothing Nothing Nothing Nothing (Just amount) Nothing
      txHash blockNumber blockHash 0 logIndex
      (eventTimestamp blockNumber) (maybe (object []) (\value -> object ["asset" .= value]) asset)

insertTransfer :: Connection -> Text -> Text -> Integer -> Integer -> Integer -> Text -> IO ()
insertTransfer conn fromAddress toAddress amount blockNumber logIndex suffix =
  insertPerpsUsdcTransfer conn fixtureChain fixtureRouter fixtureUsdc fromAddress toAddress amount
    (hashText $ "tx" <> suffix) blockNumber (hashText $ "bl" <> suffix) 0 logIndex (eventTimestamp blockNumber)

insertTrade :: Connection -> Text -> Integer -> Integer -> IO ()
insertTrade conn wallet blockNumber logIndex =
  insertPerpsActivity
    conn fixtureChain fixtureRouter fixtureRouter "insights:trade-b" wallet "Open"
    Nothing Nothing (Just 1) (Just 100_000_000) (Just 1_000_000_000_000_000_000) Nothing Nothing
    (hashText "tx-trade") blockNumber (hashText "bl-trade") 0 logIndex
    (eventTimestamp blockNumber) (object [])

snapshot
  :: Text
  -> SnapshotKind
  -> Integer
  -> Text
  -> Integer
  -> Integer
  -> AccountSnapshotInput
snapshot wallet kind blockNumber blockHash timestamp value =
  AccountSnapshotInput
    { asiCompetitionSlug = competitionSlug
    , asiWallet = wallet
    , asiKind = kind
    , asiChainId = fixtureChain
    , asiReleaseRouter = fixtureRouter
    , asiAccountLensAddress = fixtureLens
    , asiBlockNumber = blockNumber
    , asiBlockHash = blockHash
    , asiTimestamp = timestamp
    , asiEquity = EquitySnapshot False 0 value 0
    , asiRawData = object
        [ "pendingOrderCount" .= ("0" :: Text)
        , "accountValuationVersion" .= ("full-account-v2" :: Text)
        ]
    }

requireCompetition :: Connection -> Text -> IO CompetitionRow
requireCompetition conn slug = do
  row <- getCurrentCompetition conn slug
  maybe (fail $ "missing competition " <> T.unpack slug) pure row

requireWallet :: Text -> [LeaderboardRow] -> IO LeaderboardRow
requireWallet wallet rows =
  maybe (fail $ "missing leaderboard wallet " <> T.unpack wallet) pure $
    find ((== wallet) . ilrWallet) rows

requireWalletUnsafe :: Text -> [LeaderboardRow] -> LeaderboardRow
requireWalletUnsafe wallet rows =
  case find ((== wallet) . ilrWallet) rows of
    Just row -> row
    Nothing -> error $ "missing leaderboard wallet " <> T.unpack wallet

testSeptemberRules :: CompetitionRules
testSeptemberRules =
  september2026Competition
    { crRegistrationClosesAt = Nothing
    , crMinimumXAccountAgeDays = Nothing
    , crTargetXHandle = Nothing
    }

prelaunchAgeMigrationRules :: UTCTime -> CompetitionRules
prelaunchAgeMigrationRules now =
  september2026Competition
    { crStartAt = addUTCTime 86_400 now
    , crNewRiskCutoffAt = addUTCTime 172_800 now
    , crScoreCutoffAt = addUTCTime 172_800 now
    , crResultsAt = addUTCTime 259_200 now
    , crPaymentDeadlineAt = addUTCTime 604_800 now
    , crRegistrationClosesAt = Just $ addUTCTime 3_600 now
    }

fixtureManifest :: CompetitionReleaseManifest
fixtureManifest =
  CompetitionReleaseManifest
    { crmReleaseId = competitionSlug
    , crmChainId = fixtureChain
    , crmUsdc = fixtureUsdc
    , crmOrderRouter = fixtureRouter
    , crmMarginClearinghouse = fixtureClearinghouse
    , crmAccountLens = fixtureLens
    , crmCfdEngine = "0xd100000000000000000000000000000000000001"
    , crmCfdEngineLens = "0xd200000000000000000000000000000000000002"
    , crmSettlementSidecar = "0xd300000000000000000000000000000000000003"
    , crmPletherOracle = "0xd400000000000000000000000000000000000004"
    , crmIndexerStartBlock = 1
    }

competitionSlug, fixtureRouter, fixtureUsdc, fixtureClearinghouse, fixtureLens :: Text
competitionSlug = crSlug testSeptemberRules
fixtureRouter = "0xa100000000000000000000000000000000000001"
fixtureUsdc = "0xa200000000000000000000000000000000000002"
fixtureClearinghouse = "0xa300000000000000000000000000000000000003"
fixtureLens = "0xa400000000000000000000000000000000000004"

walletA, walletB, walletC, wrongAsset, wrongEmitter, attacker, zeroAddress :: Text
walletA = "0xb100000000000000000000000000000000000001"
walletB = "0xb200000000000000000000000000000000000002"
walletC = "0xb300000000000000000000000000000000000003"
wrongAsset = "0xc100000000000000000000000000000000000001"
wrongEmitter = "0xc200000000000000000000000000000000000002"
attacker = "0xc300000000000000000000000000000000000003"
zeroAddress = "0x0000000000000000000000000000000000000000"

fixtureChain, baselineBlock, startBlock, liveBlock, finalBlock, cursorBlock :: Integer
fixtureChain = 421_614
baselineBlock = 100
startBlock = 101
liveBlock = 110
finalBlock = 120
cursorBlock = 200

baselineHash, startHash, liveHash, finalHash, cursorHash :: Text
baselineHash = hashText "baseline"
startHash = hashText "start"
liveHash = hashText "live"
finalHash = hashText "final"
cursorHash = hashText "cursor"

bankroll, gain, loss, usdcScale :: Integer
usdcScale = 1_000_000
bankroll = 100_000 * usdcScale
gain = 5_000 * usdcScale
loss = 1_000 * usdcScale

startTimestamp, baselineTimestamp, liveTimestamp, finalTimestamp :: Integer
startTimestamp = round $ utcTimeToPOSIXSeconds $ crStartAt testSeptemberRules
baselineTimestamp = startTimestamp - 1
liveTimestamp = startTimestamp + 1_000
finalTimestamp = round (utcTimeToPOSIXSeconds $ crScoreCutoffAt testSeptemberRules) - 1

eventTimestamp :: Integer -> Integer
eventTimestamp blockNumber
  | blockNumber <= baselineBlock = baselineTimestamp - (baselineBlock - blockNumber)
  | otherwise = startTimestamp + (blockNumber - startBlock)

hashText :: Text -> Text
hashText seed =
  let encoded = TextEncoding.decodeUtf8 $ Base16.encode $ TextEncoding.encodeUtf8 seed
   in "0x" <> T.take 64 (encoded <> T.replicate 64 "0")
