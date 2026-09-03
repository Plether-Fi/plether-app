module Plether.Handlers.VaultActivitySpec (spec) where

import Plether.Handlers.VaultActivity (vaultCoverageIsStale)
import Test.Hspec

spec :: Spec
spec = describe "vault activity freshness" $ do
  it "keeps normal independent polling drift quiet" $ do
    vaultCoverageIsStale 13 13 `shouldBe` False
    vaultCoverageIsStale 120 180 `shouldBe` False

  it "warns for meaningful indexing lag or an old successful poll" $ do
    vaultCoverageIsStale 121 0 `shouldBe` True
    vaultCoverageIsStale 0 181 `shouldBe` True
