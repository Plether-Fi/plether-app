module Plether.Handlers.InsightsExecutionSpec (spec) where

import Data.Aeson (object, (.=))
import Plether.Handlers.InsightsExecution
  ( DetectedKeeperEvent (..)
  , KeeperAggregate (..)
  , LatencyStats (..)
  , RawKeeperTx (..)
  , aggregateKeepers
  , computeLatencyPercentiles
  , extractTxFrom
  , mergeLpSettlementTxs
  , parseHexInteger
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

  it "merges canonical LP settlement logs with keeper journal entries without double-counting" $ do
    let journal =
          [ RawKeeperTx "lp_settlement" "0xAAA1" 0 1700000100 (Just 4) Nothing
          ]
        canonical =
          [ RawKeeperTx "lp_settlement" "0xaaa1" 500 1700000090 Nothing Nothing
          , RawKeeperTx "lp_settlement" "0xbbb2" 520 1700000200 Nothing Nothing
          ]
        merged = mergeLpSettlementTxs journal canonical
    length merged `shouldBe` 2
    case merged of
      [externalTx, overlapTx] -> do
        rktTxHash externalTx `shouldBe` "0xbbb2"
        rktBlockNumber externalTx `shouldBe` 520
        rktTxHash overlapTx `shouldBe` "0xaaa1"
        rktBlockNumber overlapTx `shouldBe` 500
        rktLatencySeconds overlapTx `shouldBe` Just 4
        rktTimestamp overlapTx `shouldBe` 1700000100
      _ -> expectationFailure "Expected merged canonical + journal LP settlements"

  it "parses hex integers from RPC log blockTimestamp fields" $ do
    parseHexInteger "0x67000010" `shouldBe` Just 1728053264
    parseHexInteger "0x0" `shouldBe` Just 0
    parseHexInteger "not-hex" `shouldBe` Nothing
    parseHexInteger "" `shouldBe` Nothing
