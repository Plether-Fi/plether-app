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
  , computeChangeFromBaseBps
  , computeComponentConfidenceBps
  , computeComponentContribution
  , computeDisplayDeltaBps
  , computeEffectiveShareBps
  , parseOracleUpdateIdParam
  , settledOrderSideLabel
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

  it "computes DXY basket component contribution, effective share, and drift vs base" $ do
    -- At basePrice, EUR/USD (5760 bps) contributes exactly 57_600_000 (0.57600000)
    computeComponentContribution 117500000 5760 117500000 `shouldBe` Just 57600000
    -- If EUR/USD moves +10% above base, contribution rises by 10%
    computeComponentContribution 129250000 5760 117500000 `shouldBe` Just 63360000
    computeComponentContribution 0 5760 117500000 `shouldBe` Nothing
    -- Effective share bps out of total basket contribution
    computeEffectiveShareBps 57600000 100000000 `shouldBe` Just 5760
    computeEffectiveShareBps 63360000 105760000 `shouldBe` Just 5991
    computeEffectiveShareBps 57600000 0 `shouldBe` Nothing
    -- Change vs base price in signed bps
    computeChangeFromBaseBps 117500000 117500000 `shouldBe` Just 0
    computeChangeFromBaseBps 129250000 117500000 `shouldBe` Just 1000
    computeChangeFromBaseBps 105750000 117500000 `shouldBe` Just (-1000)
    computeChangeFromBaseBps 0 117500000 `shouldBe` Nothing

  it "computes display plDXY price delta in basis points across updates" $ do
    -- raw 100_000_000 -> display 100_000_000; raw 99_000_000 -> display 101_000_000 (+100 bps)
    computeDisplayDeltaBps 99000000 100000000 `shouldBe` Just 100
    computeDisplayDeltaBps 101000000 100000000 `shouldBe` Just (-100)
    computeDisplayDeltaBps 200000000 100000000 `shouldBe` Nothing

  it "parses oracle update id path parameter safely" $ do
    parseOracleUpdateIdParam "latest" `shouldBe` Just Nothing
    parseOracleUpdateIdParam " LATEST " `shouldBe` Just Nothing
    parseOracleUpdateIdParam "42105" `shouldBe` Just (Just 42105)
    parseOracleUpdateIdParam "0" `shouldBe` Nothing
    parseOracleUpdateIdParam "-5" `shouldBe` Nothing
    parseOracleUpdateIdParam "abc" `shouldBe` Nothing

  it "maps perps_orders integer side codes to long/short labels" $ do
    settledOrderSideLabel (Just 0) `shouldBe` Just "long"
    settledOrderSideLabel (Just 1) `shouldBe` Just "short"
    settledOrderSideLabel (Just 2) `shouldBe` Nothing
    settledOrderSideLabel Nothing `shouldBe` Nothing
