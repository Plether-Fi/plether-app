module Plether.Insights.DatabaseSpec
  ( insightsDatabaseSpec
  ) where

import Control.Exception (bracket, finally)
import Control.Monad (void)
import Data.Aeson (object, (.=))
import qualified Data.ByteString.Base16 as Base16
import Data.List (find, sort)
import Data.Maybe (isJust)
import Data.Pool (destroyAllResources)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TextEncoding
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Database.PostgreSQL.Simple (Connection, Only (..), execute, query_)
import Plether.Database (DbPool, newDbPool, withDb)
import Plether.Database.Insights
  ( AccountSnapshotInput (..)
  , CompetitionRow (..)
  , LeaderboardRow (..)
  , SnapshotKind (..)
  , ensureInsightsSchema
  , getCompetitionLeaderboard
  , getCompetitionWallet
  , getCurrentCompetition
  , hasCompleteAccountSnapshotBatch
  , insertManualAdjustment
  , materializeFinalizedStandings
  , publishAccountSnapshotBatch
  , refreshCompetitionIntegrityFlags
  , setCompetitionBoundaryBlocks
  , stageCompetitionParticipantWalletRemap
  )
import Plether.Database.Schema
  ( ensurePerpsHistorySchema
  , ensureTestnetFaucetSchema
  , deletePerpsHistoryFromBlock
  , insertPerpsActivity
  , insertPerpsUsdcTransfer
  , setPerpsIndexerState
  )
import Plether.Insights.Competition
  ( CompetitionReleaseManifest (..)
  , CompetitionRules (..)
  , EquitySnapshot (..)
  , july2026Competition
  , september2026Competition
  )
import Test.Hspec

insightsDatabaseSpec :: Text -> Spec
insightsDatabaseSpec databaseUrl =
  describe "Plether Insights PostgreSQL lifecycle" $ do
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

    it "invalidates incomplete late-roster batches, rebuilds them, and accepts an all-zero final batch" $
      withInsightsDatabase databaseUrl $ \pool -> withDb pool $ \conn -> do
        insertParticipant conn walletA "trader-a"
        setCompetitionBoundaryBlocks
          conn competitionSlug
          (Just (startBlock, startHash, baselineHash))
          (Just (finalBlock, finalHash))

        publishAccountSnapshotBatch conn
          [snapshot walletA SnapshotStart baselineBlock baselineHash baselineTimestamp bankroll]
        publishAccountSnapshotBatch conn
          [snapshot walletA SnapshotLive liveBlock liveHash liveTimestamp (bankroll + gain)]
        hasCompleteAccountSnapshotBatch conn competitionSlug SnapshotStart baselineBlock baselineHash
          `shouldReturn` True
        hasCompleteAccountSnapshotBatch conn competitionSlug SnapshotLive liveBlock liveHash
          `shouldReturn` True

        insertParticipant conn walletB "trader-b"
        hasCompleteAccountSnapshotBatch conn competitionSlug SnapshotStart baselineBlock baselineHash
          `shouldReturn` False
        hasCompleteAccountSnapshotBatch conn competitionSlug SnapshotLive liveBlock liveHash
          `shouldReturn` False

        publishAccountSnapshotBatch conn
          [ snapshot walletA SnapshotStart baselineBlock baselineHash baselineTimestamp bankroll
          , snapshot walletB SnapshotStart baselineBlock baselineHash baselineTimestamp 0
          ]
        publishAccountSnapshotBatch conn
          [ snapshot walletA SnapshotLive liveBlock liveHash liveTimestamp (bankroll + gain)
          , snapshot walletB SnapshotLive liveBlock liveHash liveTimestamp (bankroll + gain)
          ]
        hasCompleteAccountSnapshotBatch conn competitionSlug SnapshotStart baselineBlock baselineHash
          `shouldReturn` True
        hasCompleteAccountSnapshotBatch conn competitionSlug SnapshotLive liveBlock liveHash
          `shouldReturn` True

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
    conn fixtureChain "perps-history-costs-v1" fixtureRouter 1 cursorBlock (Just cursorHash)

cleanupDatabase :: DbPool -> IO ()
cleanupDatabase pool = withDb pool cleanupRows

cleanupRows :: Connection -> IO ()
cleanupRows conn = do
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
    , asiRawData = object ["pendingOrderCount" .= ("0" :: Text)]
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
