module Plether.Handlers.InsightsExecutionSpec (spec) where

import Data.Aeson (object, (.=))
import qualified Data.Text as T
import Plether.Handlers.InsightsExecution
  ( DetectedKeeperEvent (..)
  , KeeperAggregate (..)
  , KeeperOrderAggregateRow (..)
  , LatencyStats (..)
  , OrderReliabilityBucket (..)
  , ProtocolLatencyBenchmark (..)
  , RawKeeperTx (..)
  , aggregateKeepers
  , buildLatencyBenchmark
  , buildTimelineBuckets
  , classifyTxKind
  , computeLatencyPercentileRank
  , computeLatencyPercentiles
  , extractTxFrom
  , isValidTxHash
  , mergeKeeperFleetAggregates
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

  it "validates 32-byte 0x-prefixed transaction hashes" $ do
    isValidTxHash (T.pack ("0x" <> replicate 64 'a')) `shouldBe` True
    isValidTxHash (T.pack ("0x" <> replicate 64 'F')) `shouldBe` True
    isValidTxHash (T.pack ("0x" <> replicate 63 'a')) `shouldBe` False
    isValidTxHash (T.pack ("0x" <> replicate 64 'g')) `shouldBe` False
    isValidTxHash "not-a-hash" `shouldBe` False

  it "builds chronological zero-filled order reliability time buckets with exact timestamps" $ do
    let buckets = buildTimelineBuckets 1700003600 3600 3 [(0, 10, 10, 0, 0, 0), (2, 5, 3, 1, 1, 0)]
    buckets
      `shouldBe` [ OrderReliabilityBucket 1700000000 1700001200 10 10 0 0 0
                 , OrderReliabilityBucket 1700001200 1700002400 0 0 0 0 0
                 , OrderReliabilityBucket 1700002400 1700003600 5 3 1 1 0
                 ]

  it "classifies single finalization, batch finalization, LP settlement, and liquidation transactions" $ do
    classifyTxKind 3 False False `shouldBe` "batch_finalization"
    classifyTxKind 1 False False `shouldBe` "order_finalization"
    classifyTxKind 0 True False `shouldBe` "lp_settlement"
    classifyTxKind 0 False True `shouldBe` "liquidation"
    classifyTxKind 0 False False `shouldBe` "protocol_tx"

  it "merges full-24h SQL order aggregates with liquidations, protections, and LP settlements without truncating roles" $ do
    let keeperA = "0x1111111111111111111111111111111111111111"
        keeperB = "0x2222222222222222222222222222222222222222"
        orderAggs =
          [ KeeperOrderAggregateRow keeperA 6590 (Just 3) (Just 6) 2000 1700005000
          ]
        nonOrderEvents =
          [ DetectedKeeperEvent "liquidation" keeperB "0xliq1" 1900 1700004000 Nothing (Just 44550000)
          , DetectedKeeperEvent "protection" keeperA "0xprot1" 1950 1700004500 Nothing Nothing
          , DetectedKeeperEvent "lp_settlement" keeperB "0xlp1" 1850 1700003500 (Just 4) Nothing
          ]
        merged = mergeKeeperFleetAggregates orderAggs nonOrderEvents 6600
    length merged `shouldBe` 2
    case merged of
      [firstK, secondK] -> do
        kaAddress firstK `shouldBe` keeperA
        kaRoles firstK `shouldBe` ["order_execution", "protection"]
        kaExecutions24h firstK `shouldBe` 6591
        kaShareBps firstK `shouldBe` Just 9986
        kaLatencyP50Seconds firstK `shouldBe` Just 3
        kaLatencyP95Seconds firstK `shouldBe` Just 6
        kaBountiesEarnedAssets firstK `shouldBe` 0
        kaLastActiveBlock firstK `shouldBe` 2000

        kaAddress secondK `shouldBe` keeperB
        kaRoles secondK `shouldBe` ["liquidation", "lp_settlement"]
        kaExecutions24h secondK `shouldBe` 2
        kaLatencyP50Seconds secondK `shouldBe` Just 4
        kaBountiesEarnedAssets secondK `shouldBe` 44550000
      _ -> expectationFailure "Expected two merged keeper aggregates"

  it "computes mid-rank empirical latency percentiles and signed stage benchmarks" $ do
    computeLatencyPercentileRank [] 2 `shouldBe` Nothing
    let samples = [1, 2, 2, 2, 2, 3, 5]
    computeLatencyPercentileRank samples 1 `shouldBe` Just 7
    computeLatencyPercentileRank samples 2 `shouldBe` Just 43
    computeLatencyPercentileRank samples 3 `shouldBe` Just 79
    computeLatencyPercentileRank samples 5 `shouldBe` Just 93
    computeLatencyPercentileRank samples 25 `shouldBe` Just 99
    let bench = buildLatencyBenchmark "7d" samples [-1, 0, 1, 1, 2] [1, 1, 2, 2, 3]
    plbWindowLabel bench `shouldBe` "7d"
    plbSampleCount bench `shouldBe` 7
    plbMinSeconds bench `shouldBe` Just 1
    plbP50Seconds bench `shouldBe` Just 2
    plbP95Seconds bench `shouldBe` Just 5
    plbP50CommitToOracleSeconds bench `shouldBe` Just 1
    plbP50OracleToFinalizeSeconds bench `shouldBe` Just 2
