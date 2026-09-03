module Plether.Database.VaultPerformanceSpec (spec) where

import Test.Hspec

spec :: Spec
spec =
  describe "vault performance persistence" $ do
    it "isolates vault-only redeployments in both runtime and static primary keys" $ do
      runtime <- readFile "src/Plether/Database/VaultPerformance.hs"
      static <- readFile "schema.sql"
      let identity =
            "chain_id, house_pool_address, senior_vault_address, junior_vault_address, epoch_timestamp"
      runtime `shouldContain` identity
      runtime `shouldContain` "ON CONFLICT ("
      static `shouldContain` "senior_vault_address VARCHAR(42) NOT NULL"
      static `shouldContain` "junior_vault_address VARCHAR(42) NOT NULL"
      static `shouldContain` "PRIMARY KEY ("

    it "uses exact deployment identity and newest-first limiting for API reads" $ do
      runtime <- readFile "src/Plether/Database/VaultPerformance.hs"
      runtime `shouldContain` "AND senior_vault_address = ? AND junior_vault_address = ?"
      runtime `shouldContain` "ORDER BY epoch_timestamp DESC LIMIT ?"
      runtime `shouldContain` ") AS latest ORDER BY epoch_timestamp ASC"

    it "keeps hourly boundaries and canonical block identities constrained" $ do
      runtime <- readFile "src/Plether/Database/VaultPerformance.hs"
      static <- readFile "schema.sql"
      runtime `shouldContain` "epoch_timestamp >= 0 AND epoch_timestamp % 3600 = 0"
      runtime `shouldContain` "block_hash ~ '^0x[0-9a-f]{64}$'"
      static `shouldContain` "epoch_timestamp >= 0 AND epoch_timestamp % 3600 = 0"
      static `shouldContain` "block_hash ~ '^0x[0-9a-f]{64}$'"

    it "persists mark freshness and migrates legacy snapshot tables" $ do
      runtime <- readFile "src/Plether/Database/VaultPerformance.hs"
      static <- readFile "schema.sql"
      runtime `shouldContain` "mark_fresh BOOLEAN NOT NULL"
      runtime `shouldContain` "ADD COLUMN IF NOT EXISTS mark_fresh BOOLEAN"
      runtime `shouldContain` "mark_fresh = EXCLUDED.mark_fresh"
      static `shouldContain` "mark_fresh BOOLEAN NOT NULL"
