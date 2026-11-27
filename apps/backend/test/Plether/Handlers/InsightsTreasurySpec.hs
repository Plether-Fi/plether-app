{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Plether.Handlers.InsightsTreasurySpec (spec) where

import Data.Aeson (object, (.=))
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Plether.Ethereum.Abi (encodeAddress, encodeUint256)
import Plether.Handlers.InsightsTreasury
  ( DailyRevenuePoint (..)
  , PendingTreasuryRotation (..)
  , buildDailyRevenueSeries
  , computeAnnualizedRunRate7d
  , computeEffectiveFeeBps
  , decodeAddressWord
  , decodePendingTreasury
  , decodeRiskProtocolShareBps
  , extractManifestTreasuryAddress
  )
import Test.Hspec

spec :: Spec
spec = describe "Plether.Handlers.InsightsTreasury" $ do
  it "decodes non-zero Ethereum addresses from 32-byte ABI words and rejects zero address" $ do
    let addr = "0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B"
    decodeAddressWord (encodeAddress addr)
      `shouldBe` Just "0x5a71a4094ec81165ada48aa4c27da48ec27e0d6b"
    decodeAddressWord (encodeAddress "0x0000000000000000000000000000000000000000")
      `shouldBe` Nothing
    decodeAddressWord (BS.replicate 16 0) `shouldBe` Nothing

  it "decodes pendingProtocolTreasury() 3-word ABI responses" $ do
    let nextTreasury = "0x1111111111111111111111111111111111111111"
        payload = BS.concat [encodeAddress nextTreasury, encodeUint256 1_796_000_000, encodeUint256 1_797_000_000]
    decodePendingTreasury payload
      `shouldBe` Just (PendingTreasuryRotation nextTreasury 1_796_000_000 1_797_000_000)
    let zeroPayload = BS.concat [encodeAddress "0x0000000000000000000000000000000000000000", encodeUint256 0, encodeUint256 0]
    decodePendingTreasury zeroPayload `shouldBe` Nothing

  it "decodes keeperShareBps and protocolShareBps from 10-word CfdEngine.riskParams()" $ do
    let words10 =
          BS.concat
            [ encodeUint256 10_000_000_000_000_000
            , encodeUint256 400_000_000_000_000_000
            , encodeUint256 10
            , encodeUint256 20
            , encodeUint256 300
            , encodeUint256 500
            , encodeUint256 1_000_000
            , encodeUint256 10
            , encodeUint256 5000
            , encodeUint256 1500
            ]
    decodeRiskProtocolShareBps words10 `shouldBe` Just (Just 5000, Just 1500)
    decodeRiskProtocolShareBps (BS.replicate 64 0) `shouldBe` Nothing

  it "extracts deploymentTimeProtocolTreasury from the embedded release manifest" $ do
    let mVal =
          object
            [ "release"
                .= object
                  [ "deploymentTimeProtocolTreasury"
                      .= ("0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B" :: Text)
                  ]
            ]
    extractManifestTreasuryAddress mVal
      `shouldBe` Just "0x5a71a4094ec81165ada48aa4c27da48ec27e0d6b"

  it "computes effective fee bps and 7d annualized run-rate cleanly" $ do
    computeEffectiveFeeBps 400_000 1_000_000_000 `shouldBe` Just 4.0
    computeEffectiveFeeBps 0 0 `shouldBe` Nothing
    computeAnnualizedRunRate7d 7_000_000 `shouldBe` Just 365_000_000
    computeAnnualizedRunRate7d 0 `shouldBe` Nothing

  it "builds chronological zero-filled daily revenue series with running cumulative fees" $ do
    let d0 = 1_795_564_800 :: Integer -- 2026-11-25
        d1 = d0 + 86400
        d2 = d0 + 2 * 86400
        ordMap = Map.fromList [(d0, (12, 10_000_000_000, 4_000_000)), (d2, (5, 5_000_000_000, 2_000_000))]
        caMap = Map.fromList [(d1, 200_000)]
        gasMap = Map.fromList [(d0, 1_000_000_000_000_000), (d2, 500_000_000_000_000)]
        series = buildDailyRevenueSeries d0 d2 10_000_000 ordMap caMap gasMap
    length series `shouldBe` 3
    map drpExecutionFeeAssets series `shouldBe` [4_000_000, 0, 2_000_000]
    map drpCumulativeExecutionFeeAssets series `shouldBe` [14_000_000, 14_000_000, 16_000_000]
    map drpCloseAssistanceAssets series `shouldBe` [0, 200_000, 0]
    map drpSponsoredGasWei series `shouldBe` [1_000_000_000_000_000, 0, 500_000_000_000_000]
