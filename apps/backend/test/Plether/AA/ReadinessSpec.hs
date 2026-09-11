module Plether.AA.ReadinessSpec (spec) where
import Test.Hspec
import Plether.AA.Readiness

spec :: Spec
spec = describe "Readiness funding evidence" $ do
  it "subtracts outstanding liabilities before affordability" $
    classifyFunding 100 80 30 `shouldBe` Check "funding" "blocked" "KEEPER_INSUFFICIENT_FUNDS"
  it "warns below ten executions without inventing a hard blocker" $
    classifyFunding 100 0 20 `shouldBe` Check "funding" "ready" "FUNDING_LOW"
  it "does not treat an unknown estimate as zero cost" $
    classifyFunding 100 0 0 `shouldBe` Check "funding" "unknown" "FUNDING_UNVERIFIED"
  it "accepts exactly ten execution reserves" $
    classifyFunding 200 0 20 `shouldBe` Check "funding" "ready" "READY"
