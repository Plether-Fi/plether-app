module Plether.Types.PerpsSpec (spec) where

import Data.Aeson (object, toJSON, (.=))
import Plether.Types.Perps
  ( BasketCandle (..)
  , BasketCurrentCandle (..)
  , BasketHistoryPoint (..)
  , basketCandlePageSpan
  , basketRangeSeconds
  , canonicalBasketCandleIntervals
  , hasExactBasketCandleQueryKeys
  , isAlignedBasketCandleCursor
  , isBasketCandleCursorWithinFutureBound
  )
import Test.Hspec

spec :: Spec
spec = do
  describe "basketRangeSeconds" $ do
    it "supports one year basket history ranges" $ do
      basketRangeSeconds "1y" `shouldBe` 365 * 24 * 60 * 60

  describe "BasketHistoryPoint JSON" $
    it "always serializes candle volume as a lossless decimal string" $ do
      toJSON
        BasketHistoryPoint
          { bhpTimestamp = 120
          , bhpBasketPrice = 101_660_000
          , bhpVolumeUsdc = 12_345_678
          , bhpComponents = Nothing
          }
        `shouldBe` object
          [ "timestamp" .= (120 :: Integer)
          , "basketPrice" .= ("101660000" :: String)
          , "volumeUsdc" .= ("12345678" :: String)
          ]

  describe "basket candle pagination" $ do
    it "supports only the seven canonical resolutions" $ do
      canonicalBasketCandleIntervals
        `shouldBe` [60, 180, 300, 900, 1800, 3600, 86_400]

    it "uses fixed five-hundred-bucket page boundaries" $ do
      basketCandlePageSpan 3600 `shouldBe` Just 1_800_000
      isAlignedBasketCandleCursor 3600 1_800_000 `shouldBe` True
      isAlignedBasketCandleCursor 3600 1_800_001 `shouldBe` False
      isAlignedBasketCandleCursor 120 60_000 `shouldBe` False

    it "allows exactly one page of clock skew beyond the backend clock" $ do
      isBasketCandleCursorWithinFutureBound 65_000 60 90_000 `shouldBe` True
      isBasketCandleCursorWithinFutureBound 65_000 60 120_000 `shouldBe` True
      isBasketCandleCursorWithinFutureBound 65_000 60 150_000 `shouldBe` False
      isBasketCandleCursorWithinFutureBound 90_000 60 120_000 `shouldBe` True
      isBasketCandleCursorWithinFutureBound 90_000 60 150_000 `shouldBe` False

    it "rejects unknown, missing, and duplicate query keys" $ do
      hasExactBasketCandleQueryKeys ["interval", "cursor"] ["cursor", "interval"]
        `shouldBe` True
      hasExactBasketCandleQueryKeys ["interval", "cursor"] ["interval"]
        `shouldBe` False
      hasExactBasketCandleQueryKeys ["interval", "cursor"] ["interval", "cursor", "limit"]
        `shouldBe` False
      hasExactBasketCandleQueryKeys ["interval", "cursor"] ["interval", "cursor", "cursor"]
        `shouldBe` False

  describe "BasketCandle JSON" $ do
    it "serializes raw OHLCV losslessly and derives completeness" $ do
      toJSON
        BasketCandle
          { bcTimestamp = 3600
          , bcRawOpenPrice = 101_000_000
          , bcRawHighPrice = 102_000_000
          , bcRawLowPrice = 100_000_000
          , bcRawClosePrice = 101_500_000
          , bcVolumeUsdc = Just 12_345_678
          , bcTradeCount = Just 4
          , bcSampleCount = 59
          , bcQuality = "observed"
          , bcRevision = 2
          , bcPriceComplete = True
          , bcVolumeComplete = False
          }
        `shouldBe` object
          [ "timestamp" .= (3600 :: Integer)
          , "rawOpenPrice" .= ("101000000" :: String)
          , "rawHighPrice" .= ("102000000" :: String)
          , "rawLowPrice" .= ("100000000" :: String)
          , "rawClosePrice" .= ("101500000" :: String)
          , "volumeUsdc" .= ("12345678" :: String)
          , "tradeCount" .= (4 :: Integer)
          , "sampleCount" .= (59 :: Integer)
          , "quality" .= ("observed" :: String)
          , "revision" .= (2 :: Integer)
          , "priceComplete" .= True
          , "volumeComplete" .= False
          , "complete" .= False
          ]

    it "preserves unknown current-candle volume as null" $ do
      let candle =
            BasketCandle
              { bcTimestamp = 3600
              , bcRawOpenPrice = 101_000_000
              , bcRawHighPrice = 101_000_000
              , bcRawLowPrice = 101_000_000
              , bcRawClosePrice = 101_000_000
              , bcVolumeUsdc = Nothing
              , bcTradeCount = Nothing
              , bcSampleCount = 1
              , bcQuality = "observed"
              , bcRevision = 1
              , bcPriceComplete = False
              , bcVolumeComplete = False
              }
      toJSON candle `shouldSatisfy` (== object
        [ "timestamp" .= (3600 :: Integer)
        , "rawOpenPrice" .= ("101000000" :: String)
        , "rawHighPrice" .= ("101000000" :: String)
        , "rawLowPrice" .= ("101000000" :: String)
        , "rawClosePrice" .= ("101000000" :: String)
        , "volumeUsdc" .= (Nothing :: Maybe String)
        , "tradeCount" .= (Nothing :: Maybe Integer)
        , "sampleCount" .= (1 :: Integer)
        , "quality" .= ("observed" :: String)
        , "revision" .= (1 :: Integer)
        , "priceComplete" .= False
        , "volumeComplete" .= False
        , "complete" .= False
        ])

    it "keeps current coverage metadata when the mutable row is absent" $ do
      toJSON
        BasketCurrentCandle
          { bccIntervalSeconds = 60
          , bccSeriesId = "dxy-v1"
          , bccConfigurationHash = "sha256:test"
          , bccDisplayPriceCap = 200_000_000
          , bccDatasetGeneration = 67_108_865
          , bccCoverageStart = Just 1_000
          , bccCoverageEnd = Just 2_000
          , bccFinalizedThrough = Just 1_900
          , bccCoverageComplete = True
          , bccCandle = Nothing
          }
        `shouldBe` object
          [ "intervalSeconds" .= (60 :: Integer)
          , "seriesId" .= ("dxy-v1" :: String)
          , "configurationHash" .= ("sha256:test" :: String)
          , "displayPriceCap" .= ("200000000" :: String)
          , "datasetGeneration" .= (67_108_865 :: Integer)
          , "coverageStart" .= (Just 1_000 :: Maybe Integer)
          , "coverageEnd" .= (Just 2_000 :: Maybe Integer)
          , "finalizedThrough" .= (Just 1_900 :: Maybe Integer)
          , "coverageComplete" .= True
          , "candle" .= (Nothing :: Maybe BasketCandle)
          ]
