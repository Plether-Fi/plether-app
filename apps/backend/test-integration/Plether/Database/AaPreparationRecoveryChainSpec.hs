{-# LANGUAGE OverloadedStrings #-}

module Plether.Database.AaPreparationRecoveryChainSpec (preparationRecoveryChainSpec) where

import Control.Exception (bracket, finally)
import Control.Monad (forM_, void)
import Data.Aeson (Value, object, (.=))
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.PostgreSQL.Simple
import qualified Plether.Database.AaPreparation as Preparation
import qualified Plether.Database.AaPreparationRecovery as Recovery
import Plether.Database.AaSponsorship (ensureAaSponsorshipSchema)
import Test.Hspec

preparationRecoveryChainSpec :: Text -> Spec
preparationRecoveryChainSpec databaseUrl = around withSchema $ describe "Arbitrum preparation recovery durability" $ do
  it "leaves an installation without the optional recovery schema untouched" $ \conn -> do
    Recovery.ensurePreparationRecoveryChainScope conn
    query_ conn "SELECT to_regclass('aa_preparation_registry') IS NULL" `shouldReturn` [Only True]

  it "migrates the legacy constraint and persists a fenced mainnet preparation across reconnection" $ \conn -> do
    installLegacy conn
    Recovery.beginPreparation conn mainnet "before-migration" `shouldThrow` checkViolation
    oldFence <- Recovery.beginPreparation conn sepolia "legacy-worker" >>= requireFence
    Recovery.releasePreparation conn oldFence
    void $ execute_ conn "UPDATE aa_preparation_registry SET retired=TRUE,generation=7"
    applyChainMigration conn
    applyChainMigration conn
    Recovery.ensurePreparationRecoveryChainScope conn
    assertHistoricalRetirement conn
    fence <- Recovery.beginPreparation conn mainnet "mainnet-worker" >>= requireFence
    Preparation.claimPreparationCompatibleFenced conn fence True client sender preparationId intentHash [] "preparation-worker"
      `shouldReturn` Preparation.PreparationClaimed Nothing
    Preparation.bindPreparationDeployment conn client sender preparationId "preparation-worker" 42161 router `shouldReturn` True
    Recovery.bindDeployment conn fence client `shouldReturn` True
    Preparation.savePreparedOperation conn client sender preparationId "preparation-worker" operation `shouldReturn` True
    Preparation.releasePreparation conn client sender preparationId "preparation-worker"
    Recovery.releasePreparation conn fence
    -- A fresh database connection sees the exact stored operation and its
    -- configured chain/deployment, while the retired Sepolia scope stays dead.
    withPeer $ \restarted -> do
      rows <- query restarted
        "SELECT operation,diagnostic_chain_id,diagnostic_deployment,recovery_paymaster FROM aa_preparations WHERE client_key=? AND sender=? AND preparation_id=?"
        (client,sender,preparationId) :: IO [(Value,Integer,Text,Text)]
      rows `shouldBe` [(operation,42161,router,paymaster)]
      Recovery.matchingPreparations restarted mainnet router `shouldReturn` [(client,Nothing,True)]
      Recovery.beginPreparation restarted sepolia "retired-retry" `shouldReturn` Left "PREPARATION_RETIRED"
      recovered <- Recovery.beginPreparation restarted mainnet "restarted-worker" >>= requireFence
      Recovery.releasePreparation restarted recovered
      assertHistoricalRetirement restarted
    Recovery.beginPreparation conn (mainnet {Recovery.scopeChain = 1}) "unsupported-chain" `shouldThrow` checkViolation

  it "upgrades an existing registry at runtime without rewriting historical attribution" $ \conn -> do
    installLegacy conn
    oldFence <- Recovery.beginPreparation conn sepolia "legacy-worker" >>= requireFence
    Recovery.releasePreparation conn oldFence
    void $ execute_ conn "UPDATE aa_preparation_registry SET retired=TRUE,generation=7"
    Recovery.ensurePreparationRecoveryChainScope conn
    Recovery.ensurePreparationRecoveryChainScope conn
    assertHistoricalRetirement conn
    fence <- Recovery.beginPreparation conn mainnet "mainnet-worker" >>= requireFence
    Recovery.releasePreparation conn fence
    Recovery.beginPreparation conn (mainnet {Recovery.scopeChain = 1}) "unsupported-chain" `shouldThrow` checkViolation
  where
    withPeer action = bracket (connectPostgreSQL $ TE.encodeUtf8 databaseUrl) close $ \conn -> do
      void $ execute_ conn "SET search_path=aa_preparation_chain_spec"
      action conn
    withSchema action = withPeer $ \conn -> do
      names <- query_ conn "SELECT current_database()" :: IO [Only Text]
      case names of
        [Only name] | "_test" `T.isSuffixOf` name || "critical_path" `T.isInfixOf` name -> pure ()
        _ -> fail "Recovery chain tests require a dedicated _test or critical_path database"
      void $ execute_ conn "DROP SCHEMA IF EXISTS aa_preparation_chain_spec CASCADE"
      void $ execute_ conn "CREATE SCHEMA aa_preparation_chain_spec"
      action conn `finally` void (execute_ conn "DROP SCHEMA aa_preparation_chain_spec CASCADE")

installLegacy :: Connection -> IO ()
installLegacy conn = do
  ensureAaSponsorshipSchema conn
  forM_ ["aa-preparation-v1.sql","aa-observability-v1.sql","aa-preparation-recovery-v1.sql"] $ \name -> do
    migration <- fromString <$> readFile ("config/migrations/" <> name)
    void $ execute_ conn migration

applyChainMigration :: Connection -> IO ()
applyChainMigration conn = do
  migration <- fromString <$> readFile "config/migrations/aa-preparation-recovery-chain-scope-v1.sql"
  void $ execute_ conn migration

assertHistoricalRetirement :: Connection -> Expectation
assertHistoricalRetirement conn = do
  rows <- query conn "SELECT chain_id,generation,retired FROM aa_preparation_registry WHERE chain_id=? AND preparation_id=?"
    (421614 :: Integer,preparationId) :: IO [(Integer,Integer,Bool)]
  rows `shouldBe` [(421614,7,True)]

requireFence :: Either Text Recovery.Fence -> IO Recovery.Fence
requireFence = either (fail . T.unpack) pure

checkViolation :: SqlError -> Bool
checkViolation = (== "23514") . sqlState

mainnet, sepolia :: Recovery.Scope
mainnet = Recovery.Scope 42161 paymaster sender preparationId
sepolia = Recovery.Scope 421614 paymaster sender preparationId

client, sender, paymaster, router, preparationId, intentHash :: Text
client = "0x" <> T.replicate 64 "1"
sender = "0x" <> T.replicate 40 "2"
paymaster = "0x" <> T.replicate 40 "3"
router = "0x" <> T.replicate 40 "4"
preparationId = "0x" <> T.replicate 64 "5"
intentHash = "0x" <> T.replicate 64 "6"

operation :: Value
operation = object ["sender" .= sender,"nonce" .= ("0x1" :: Text),"callData" .= ("0x1234" :: Text)]
