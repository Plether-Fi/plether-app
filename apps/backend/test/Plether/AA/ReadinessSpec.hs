module Plether.AA.ReadinessSpec (spec) where
import Test.Hspec
import Plether.AA.Readiness
import Data.Aeson (Value(..))
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable (toList)

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
  it "never hides an unverified Alto executor behind a funded peer" $ do
    aggregateFunding "alto" [("ready","READY"),("unknown","FUNDING_UNVERIFIED")] `shouldBe` Check "alto" "unknown" "FUNDING_UNVERIFIED"
    aggregateFunding "alto" [("ready","READY"),("blocked","WORKER_INSUFFICIENT_FUNDS")] `shouldBe` Check "alto" "unknown" "FUNDING_LOW"
    aggregateFunding "alto" [("blocked","WORKER_INSUFFICIENT_FUNDS")] `shouldBe` Check "alto" "blocked" "WORKER_INSUFFICIENT_FUNDS"
    aggregateFunding "alto" [] `shouldBe` Check "alto" "unknown" "FUNDING_UNVERIFIED"
    aggregateFunding "alto" [("blocked","READY")] `shouldBe` Check "alto" "unknown" "FUNDING_UNVERIFIED"
  it "keeps LP, oracle updater and liquidation funding out of close blockers" $ do
    let ready = Check "keeper" "ready" "READY"
        oracle = Check "oracle" "ready" "READY"
        workers = [Check c "blocked" "WORKER_INSUFFICIENT_FUNDS" | c <- ["lp_settlement","liquidation","oracle","protection"]]
        snapshot = snapshotWithWorkers 1000 True [] ready (oracle,oracle,oracle) workers
        action key = case snapshot of
          Object o | Just (Object actions) <- KM.lookup "actions" o, Just (Array xs) <- KM.lookup key actions -> toList xs
          _ -> []
        blocked (Object o) = KM.lookup "status" o == Just (String "blocked")
        blocked _ = False
    any blocked (action "close") `shouldBe` False
    any blocked (action "open") `shouldBe` False
    any blocked (action "deposit") `shouldBe` False
    any blocked (action "protection") `shouldBe` False -- cancellation/retry must remain available
