module Plether.Keeper.LiquidationMonitoringSpec (liquidationMonitoringSpec) where

import Control.Exception (bracket, finally)
import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T
import Database.PostgreSQL.Simple (Connection, Only (..), execute, execute_, query_, withTransaction)
import Plether.Database (DbPool, newDbPool, destroyDbPool, withDb)
import Plether.Database.Schema
import Plether.LiquidationWorker.Monitoring
import Test.Hspec

liquidationMonitoringSpec :: Text -> Spec
liquidationMonitoringSpec url = describe "Liquidation durable monitoring PostgreSQL" $ do
  it "preserves backlog age across repeat observations, failed reads and schema restart" $
    withFixture url $ \pool -> withDb pool $ \conn -> do
      seed conn
      observeRisk conn chain engine [(account, Just True)]
      ageRisk conn 90
      observeRisk conn chain engine [(account, Just True), (account, Nothing)]
      recordPerpsLiquidationCandidateError conn chain engine account "RPC unavailable"
      ensurePerpsLiquidationSchema conn
      health <- readBacklogHealth conn chain engine
      bhRiskCount health `shouldBe` 1
      bhOldestRiskSeconds health `shouldSatisfy` (>= 90)
      bhWithoutProgressSeconds health `shouldSatisfy` (>= 90)

  it "clears only verified resolved risk and starts a new age for a later episode" $
    withFixture url $ \pool -> withDb pool $ \conn -> do
      seed conn
      observeRisk conn chain engine [(account, Just True)]
      ageRisk conn 400
      observeRisk conn chain engine [(account, Just False)]
      health <- readBacklogHealth conn chain engine
      bhRiskCount health `shouldBe` 0
      bhOldestRiskSeconds health `shouldBe` 0
      bhWithoutProgressSeconds health `shouldBe` 0
      observeRisk conn chain engine [(account, Just True)]
      fresh <- readBacklogHealth conn chain engine
      bhOldestRiskSeconds fresh `shouldSatisfy` (< 5)

  it "a submission and successful generic checks cannot manufacture liquidation progress" $
    withFixture url $ \pool -> withDb pool $ \conn -> do
      seed conn
      observeRisk conn chain engine [(account, Just True)]
      ageRisk conn 90
      recordPerpsLiquidationCandidatePending conn chain engine account 1 account "tx" "raw" "data" 1 10000000 1 2
      markPerpsLiquidationCandidateChecked conn chain engine account
      health <- readBacklogHealth conn chain engine
      bhPendingCount health `shouldBe` 1
      bhWithoutProgressSeconds health `shouldSatisfy` (>= 90)

  it "deduplicates confirmed progress and scopes it to the chain and engine" $
    withFixture url $ \pool -> withDb pool $ \conn -> do
      seed conn
      observeRisk conn chain engine [(account, Just True)]
      ageRisk conn 180
      recordConfirmedProgress conn chain engine 100
      void $ execute conn
        "UPDATE perps_liquidation_state SET last_liquidation_progress_at = clock_timestamp() - interval '80 seconds' WHERE chain_id = ? AND cfd_engine = ?"
        (chain, engine)
      recordConfirmedProgress conn chain engine 100
      recordConfirmedProgress conn chain engine 99
      recordConfirmedProgress conn (chain + 1) engine 101
      repeated <- readBacklogHealth conn chain engine
      bhWithoutProgressSeconds repeated `shouldSatisfy` (>= 80)
      recordConfirmedProgress conn chain engine 101
      advanced <- readBacklogHealth conn chain engine
      bhWithoutProgressSeconds advanced `shouldSatisfy` (< 5)
      bhOldestRiskSeconds advanced `shouldSatisfy` (>= 180)

  it "retains progress and ages on a fresh database connection" $
    withFixture url $ \pool -> do
      withDb pool $ \conn -> do
        seed conn
        observeRisk conn chain engine [(account, Just True)]
        ageRisk conn 400
        recordConfirmedProgress conn chain engine 100
      bracket (newDbPool url) destroyDbPool $ \restarted -> withDb restarted $ \conn -> do
        health <- readBacklogHealth conn chain engine
        bhRiskCount health `shouldBe` 1
        bhOldestRiskSeconds health `shouldSatisfy` (>= 400)
        bhWithoutProgressSeconds health `shouldSatisfy` (< 5)

  it "distinguishes unseen accounts from an empty healthy queue" $
    withFixture url $ \pool -> withDb pool $ \conn -> do
      empty <- readBacklogHealth conn chain engine
      empty `shouldBe` BacklogHealth 0 0 0 0 0
      seed conn
      void $ execute conn
        "UPDATE perps_liquidation_candidates SET monitoring_first_seen_at = clock_timestamp() - interval '90 seconds' WHERE chain_id = ? AND cfd_engine = ?"
        (chain, engine)
      unknown <- readBacklogHealth conn chain engine
      bhOldestUncheckedSeconds unknown `shouldSatisfy` (>= 90)
      bhWithoutProgressSeconds unknown `shouldBe` 0
      observeRisk conn chain engine [(account, Just False)]
      checked <- readBacklogHealth conn chain engine
      bhOldestUncheckedSeconds checked `shouldSatisfy` (< 5)

  it "rolls back progress with failed receipt reconciliation" $
    withFixture url $ \pool -> withDb pool $ \conn -> do
      seed conn
      observeRisk conn chain engine [(account, Just True)]
      ageRisk conn 90
      (withTransaction conn $ do
        recordConfirmedProgress conn chain engine 100
        void $ execute_ conn "SELECT 1 / 0") `shouldThrow` anyException
      health <- readBacklogHealth conn chain engine
      bhWithoutProgressSeconds health `shouldSatisfy` (>= 90)

  it "removing a confirmed closed position removes it from the backlog" $
    withFixture url $ \pool -> withDb pool $ \conn -> do
      seed conn
      observeRisk conn chain engine [(account, Just True)]
      deletePerpsLiquidationCandidate conn chain engine account
      health <- readBacklogHealth conn chain engine
      health `shouldBe` BacklogHealth 0 0 0 0 0

withFixture :: Text -> (DbPool -> IO a) -> IO a
withFixture url action = bracket (newDbPool url) destroyDbPool $ \pool -> do
  withDb pool $ \conn -> do
    names <- query_ conn "SELECT current_database()" :: IO [Only Text]
    case names of
      [Only name] | "critical_path" `T.isInfixOf` name -> pure ()
      _ -> fail "Liquidation integration tests require a dedicated critical_path database"
    ensurePerpsLiquidationSchema conn
  cleanup pool
  action pool `finally` cleanup pool
  where
    cleanup pool = withDb pool $ \conn -> do
      void $ execute conn "DELETE FROM perps_liquidation_candidates WHERE chain_id IN (?, ?) AND cfd_engine = ?" (chain, chain + 1, engine)
      void $ execute conn "DELETE FROM perps_liquidation_state WHERE chain_id IN (?, ?) AND cfd_engine = ?" (chain, chain + 1, engine)

seed :: Connection -> IO ()
seed conn = upsertPerpsLiquidationCandidate conn chain engine account 10

ageRisk :: Connection -> Int -> IO ()
ageRisk conn seconds = void $ execute conn
  "UPDATE perps_liquidation_candidates SET risk_first_observed_at = clock_timestamp() - (? * interval '1 second') WHERE chain_id = ? AND cfd_engine = ?"
  (seconds, chain, engine)

chain :: Integer
chain = 999991

engine, account :: Text
engine = "0x1111111111111111111111111111111111111111"
account = "0x2222222222222222222222222222222222222222"
