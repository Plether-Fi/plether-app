{-# LANGUAGE OverloadedStrings #-}

module Plether.Handlers.InsightsOracleSpec (spec) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Plether.Database.Schema (classifyOracleAdmissionError)
import Plether.Handlers.InsightsOracle
  ( AdmittedBucketAgg (..)
  , OracleHealthBucket (..)
  , RejectionBucketAgg (..)
  , buildOracleHealthBuckets
  , classifyOracleSliceStatus
  , computeComponentConfidenceBps
  )
import Test.Hspec

spec :: Spec
spec = describe "Plether.Handlers.InsightsOracle" $ do
  it "classifies oracle rejection messages into sanitized canonical reason codes" $ do
    fst (classifyOracleAdmissionError "component publish times diverged by 7s, over 5s policy")
      `shouldBe` "publish_time_divergence"
    fst (classifyOracleAdmissionError "latest payload is 22s old, over 15s policy")
      `shouldBe` "stale_price"
    fst (classifyOracleAdmissionError "latest payload publish time is 8s in the future")
      `shouldBe` "future_skew"
    fst (classifyOracleAdmissionError "Pyth rejected Hermes payload before cache promotion: revert")
      `shouldBe` "onchain_pyth_rejected"
    fst (classifyOracleAdmissionError "Pyth basket price is outside the immutable v1 display domain")
      `shouldBe` "price_domain_invalid"
    fst (classifyOracleAdmissionError "Pyth signed prices did not match Hermes metadata: mismatch")
      `shouldBe` "metadata_mismatch"
    fst (classifyOracleAdmissionError "HTTP 429 rate limited https://secret-url/token=123")
      `shouldBe` "hermes_rate_limited"
    -- Never leaks raw URLs or tokens in sanitized detail
    let (_, safeDetail) = classifyOracleAdmissionError "HTTP 429 rate limited https://secret-url/token=123"
    safeDetail `shouldSatisfy` (not . T.isInfixOf "secret-url")

  it "classifies chronological oracle slice statuses deterministically" $ do
    classifyOracleSliceStatus 12 0 0 0 `shouldBe` "healthy"
    classifyOracleSliceStatus 12 1 0 0 `shouldBe` "rejected"
    classifyOracleSliceStatus 0 0 4 0 `shouldBe` "stale"
    classifyOracleSliceStatus 10 0 0 2 `shouldBe` "degraded"
    classifyOracleSliceStatus 0 0 0 0 `shouldBe` "idle"

  it "builds fixed-count chronological oracle health buckets with idle gaps preserved" $ do
    let adm =
          Map.fromList
            [ (0, AdmittedBucketAgg 10 10 0 1 2 0)
            , (2, AdmittedBucketAgg 8 6 2 4 5 1)
            ]
        rej =
          Map.fromList
            [ (1, RejectionBucketAgg 2 0)
            , (3, RejectionBucketAgg 0 3)
            ]
        buckets = buildOracleHealthBuckets 1000 120 5 adm rej
    length buckets `shouldBe` 5
    map ohbStatus buckets `shouldBe` ["healthy", "rejected", "degraded", "stale", "idle"]
    map ohbAdmittedCount buckets `shouldBe` [10, 0, 8, 0, 0]
    map ohbRejectedCount buckets `shouldBe` [0, 2, 0, 0, 0]
    map ohbStaleCount buckets `shouldBe` [0, 0, 0, 3, 0]

  it "computes FX component confidence ratios in basis points" $ do
    computeComponentConfidenceBps 117500000 117500 `shouldBe` Just 10
    computeComponentConfidenceBps 100000000 0 `shouldBe` Just 0
    computeComponentConfidenceBps 0 100 `shouldBe` Nothing
