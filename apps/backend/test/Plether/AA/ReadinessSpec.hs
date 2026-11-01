module Plether.AA.ReadinessSpec (spec) where
import Test.Hspec
import Plether.AA.Readiness
import Data.Aeson (Value(..), object, toJSON, (.=))
import Data.Text (Text)
import qualified Data.Aeson.KeyMap as KM

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
        expected component status reason = object
          ["component" .= (component :: Text), "status" .= (status :: Text), "reason" .= (reason :: Text)]
        keeperCheck = expected "keeper" "ready" "READY"
        oracleCheck = expected "oracle" "ready" "READY"
    case snapshot of
      Object fields -> do
        KM.lookup "workers" fields `shouldBe` Just (toJSON
          [expected c "blocked" "WORKER_INSUFFICIENT_FUNDS" | c <- ["lp_settlement","liquidation","oracle","protection"]])
        case KM.lookup "actions" fields of
          Just (Object actions) -> do
            KM.keys actions `shouldMatchList` ["deposit","open","close","protection"]
            KM.lookup "deposit" actions `shouldBe` Just (toJSON ([] :: [Value]))
            KM.lookup "open" actions `shouldBe` Just (toJSON [keeperCheck,oracleCheck])
            KM.lookup "close" actions `shouldBe` Just (toJSON [keeperCheck,oracleCheck])
            KM.lookup "protection" actions `shouldBe` Just (toJSON
              [keeperCheck,oracleCheck,expected "protection" "unknown" "WORKER_INSUFFICIENT_FUNDS"])
          other -> expectationFailure $ "missing or malformed action evidence: " <> show other
      other -> expectationFailure $ "missing readiness object: " <> show other
