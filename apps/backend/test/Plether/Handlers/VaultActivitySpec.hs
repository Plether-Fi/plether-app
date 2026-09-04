module Plether.Handlers.VaultActivitySpec (spec) where

import Plether.Handlers.VaultActivity (vaultCoverageIsStale, vaultRequestKind)
import Test.Hspec

spec :: Spec
spec = describe "vault activity freshness" $ do
  it "keeps normal independent polling drift quiet" $ do
    vaultCoverageIsStale 13 13 `shouldBe` False
    vaultCoverageIsStale 120 180 `shouldBe` False

  it "warns for meaningful indexing lag or an old successful poll" $ do
    vaultCoverageIsStale 121 0 `shouldBe` True
    vaultCoverageIsStale 0 181 `shouldBe` True

  it "classifies ordinary and claimable-deposit redemption requests as withdrawals" $ do
    vaultRequestKind "DepositRequest" `shouldBe` "deposit"
    vaultRequestKind "RedeemRequest" `shouldBe` "withdraw"
    vaultRequestKind "ClaimableDepositRedeemRequest" `shouldBe` "withdraw"
