module Plether.Handlers.InsightsExecutionSpec (spec) where

import Data.Aeson (object, (.=))
import Plether.Handlers.InsightsExecution
  ( DetectedKeeperEvent (..)
  , KeeperAggregate (..)
  , LatencyStats (..)
  , aggregateKeepers
  , computeLatencyPercentiles
  , extractTxFrom
  , rateBps
  )
import Test.Hspec

spec :: Spec
spec = describe "Plether.Handlers.InsightsExecution" $ do
  it "computes latency percentiles and ignores empty samples without faking zeros" $ do
    computeLatencyPercentiles [] `shouldBe` LatencyStats 0 Nothing Nothing Nothing Nothing
    computeLatencyPercentiles [12, 2, 4, 8, 6, 10, 20, 14, 16, 18]
      `shouldBe` LatencyStats 10 (Just 10) (Just 20) (Just 20) (Just 20)

  it "returns Nothing for rates with zero denominator" $ do
    rateBps 0 0 `shouldBe` Nothing
    rateBps 95 100 `shouldBe` Just 9500
    rateBps 1 3 `shouldBe` Just 3333

  it "aggregates permissionless keepers identically across roles with share, latency, and bounties" $ do
    let keeperA = "0x1111111111111111111111111111111111111111"
        keeperB = "0x2222222222222222222222222222222222222222"
        events =
          [ DetectedKeeperEvent "order_execution" keeperA "0xa1" 100 1000 (Just 3) Nothing
          , DetectedKeeperEvent "order_execution" keeperA "0xa2" 105 1020 (Just 5) Nothing
          , DetectedKeeperEvent "liquidation" keeperA "0xa3" 110 1040 Nothing (Just 500000)
          , DetectedKeeperEvent "lp_settlement" keeperB "0xb1" 108 1030 (Just 4) Nothing
          ]
        aggregated = aggregateKeepers events
    length aggregated `shouldBe` 2
    case aggregated of
      [firstKeeper, secondKeeper] -> do
        kaAddress firstKeeper `shouldBe` keeperA
        kaRoles firstKeeper `shouldBe` ["liquidation", "order_execution"]
        kaExecutions24h firstKeeper `shouldBe` 3
        kaShareBps firstKeeper `shouldBe` Just 7500
        kaLatencyP50Seconds firstKeeper `shouldBe` Just 3
        kaLatencyP95Seconds firstKeeper `shouldBe` Just 5
        kaBountiesEarnedAssets firstKeeper `shouldBe` 500000
        kaLastActiveBlock firstKeeper `shouldBe` 110

        kaAddress secondKeeper `shouldBe` keeperB
        kaRoles secondKeeper `shouldBe` ["lp_settlement"]
        kaExecutions24h secondKeeper `shouldBe` 1
        kaShareBps secondKeeper `shouldBe` Just 2500
      _ -> expectationFailure "Expected exactly two aggregated keepers"

  it "extracts and normalizes valid Ethereum tx.from addresses" $ do
    extractTxFrom (object ["from" .= ("0xAbCdEf0123456789012345678901234567890123" :: String)])
      `shouldBe` Just "0xabcdef0123456789012345678901234567890123"
    extractTxFrom (object ["from" .= ("not-an-address" :: String)]) `shouldBe` Nothing
    extractTxFrom (object []) `shouldBe` Nothing
