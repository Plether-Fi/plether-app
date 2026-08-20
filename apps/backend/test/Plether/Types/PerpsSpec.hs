module Plether.Types.PerpsSpec (spec) where

import Data.Aeson (object, toJSON, (.=))
import Data.Either (isLeft)
import Plether.Types.Perps
  ( BasketCandle (..)
  , BasketCandlePage (..)
  , BasketCurrentCandle (..)
  , BasketHistoryParams (..)
  , BasketHistoryPoint (..)
  , basketCandlePageSpan
  , basketRangeSeconds
  , canonicalBasketCandleIntervals
  , hasExactBasketCandleQueryKeys
  , isAlignedBasketCandleCursor
  , isBasketCandleCursorWithinFutureBound
  , parseCanonicalPositiveInteger
  , parseBasketHistoryQueryParams
  )
import Test.Hspec

spec :: Spec
spec = do
  describe "basketRangeSeconds" $ do
    it "supports one year basket history ranges" $ do
      basketRangeSeconds "1y" `shouldBe` 365 * 24 * 60 * 60

  describe "basket history query validation" $ do
    it "accepts the exact required keys in either order" $ do
      parseBasketHistoryQueryParams
        ["interval", "range"]
        (Just "30d")
        (Just "3600")
        Nothing
        `shouldBe` Right
          BasketHistoryParams
            { bhpRange = "30d"
            , bhpIntervalSeconds = 3600
            , bhpIncludeComponents = False
            }

    it "accepts only canonical optional component booleans" $ do
      parseBasketHistoryQueryParams
        ["includeComponents", "range", "interval"]
        (Just "24h")
        (Just "3600")
        (Just "true")
        `shouldBe` Right
          BasketHistoryParams
            { bhpRange = "24h"
            , bhpIntervalSeconds = 3600
            , bhpIncludeComponents = True
            }
      parseBasketHistoryQueryParams
        ["range", "interval", "includeComponents"]
        (Just "7d")
        (Just "300")
        (Just "false")
        `shouldBe` Right
          BasketHistoryParams
            { bhpRange = "7d"
            , bhpIntervalSeconds = 300
            , bhpIncludeComponents = False
            }

    it "rejects missing, duplicate, and unknown query keys" $ do
      let rejects keys =
            parseBasketHistoryQueryParams keys (Just "30d") (Just "3600") Nothing
              `shouldSatisfy` isLeft
      rejects ["range"]
      rejects ["range", "interval", "range"]
      rejects ["range", "interval", "extra"]
      parseBasketHistoryQueryParams
        ["range", "interval", "includeComponents", "includeComponents"]
        (Just "24h")
        (Just "3600")
        (Just "true")
        `shouldSatisfy` isLeft

    it "rejects missing or noncanonical range values" $ do
      let rejects range =
            parseBasketHistoryQueryParams ["range", "interval"] range (Just "3600") Nothing
              `shouldSatisfy` isLeft
      mapM_ rejects [Nothing, Just "", Just "week", Just "30D", Just " 30d"]

    it "rejects missing, zero, leading-zero, signed, fractional, padded, and nonnumeric intervals" $ do
      let rejects interval =
            parseBasketHistoryQueryParams ["range", "interval"] (Just "30d") interval Nothing
              `shouldSatisfy` isLeft
      mapM_ rejects [Nothing, Just "", Just "0", Just "00", Just "03600", Just "-1", Just "+60", Just "60.0", Just " 60", Just "60 ", Just "abc"]

    it "rejects missing or noncanonical component booleans" $ do
      let rejects value =
            parseBasketHistoryQueryParams
              ["range", "interval", "includeComponents"]
              (Just "24h")
              (Just "3600")
              value
              `shouldSatisfy` isLeft
      mapM_ rejects [Nothing, Just "", Just "1", Just "yes", Just "TRUE", Just " true"]

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
    it "accepts only canonical positive decimal query values" $ do
      parseCanonicalPositiveInteger "3600" `shouldBe` Just 3600
      parseCanonicalPositiveInteger "1787400000" `shouldBe` Just 1_787_400_000
      parseCanonicalPositiveInteger "12345678901234567890"
        `shouldBe` Just 12_345_678_901_234_567_890
      mapM_
        (\value -> parseCanonicalPositiveInteger value `shouldBe` Nothing)
        [ ""
        , "0"
        , "00"
        , "03600"
        , "+3600"
        , "-3600"
        , "3600.0"
        , "36e2"
        , "3_600"
        , "3,600"
        , " 3600"
        , "3600 "
        , "\t3600"
        , "3600\t"
        , "３６００"
        , "seconds"
        , "123456789012345678901"
        ]

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

    it "preserves price-only candle volume as null" $ do
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
              , bcPriceComplete = True
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
        , "priceComplete" .= True
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
          , bccVolumeChainId = 421_614
          , bccVolumeRouter = "0x1111111111111111111111111111111111111111"
          , bccVolumeCoverageStart = Just 1_200
          , bccVolumeCoverageEnd = Just 1_800
          , bccVolumeFinalizedThrough = Just 1_800
          , bccVolumeCoverageComplete = True
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
          , "volumeChainId" .= (421_614 :: Integer)
          , "volumeRouter" .= ("0x1111111111111111111111111111111111111111" :: String)
          , "volumeCoverageStart" .= (Just 1_200 :: Maybe Integer)
          , "volumeCoverageEnd" .= (Just 1_800 :: Maybe Integer)
          , "volumeFinalizedThrough" .= (Just 1_800 :: Maybe Integer)
          , "volumeCoverageComplete" .= True
          , "datasetGeneration" .= (67_108_865 :: Integer)
          , "coverageStart" .= (Just 1_000 :: Maybe Integer)
          , "coverageEnd" .= (Just 2_000 :: Maybe Integer)
          , "finalizedThrough" .= (Just 1_900 :: Maybe Integer)
          , "coverageComplete" .= True
          , "candle" .= (Nothing :: Maybe BasketCandle)
          ]

    it "binds historical volume coverage to the exact chain and router" $ do
      toJSON
        BasketCandlePage
          { bcpIntervalSeconds = 60
          , bcpCursor = 30_000
          , bcpSeriesId = "dxy-v1"
          , bcpConfigurationHash = "sha256:test"
          , bcpDisplayPriceCap = 200_000_000
          , bcpVolumeChainId = 421_614
          , bcpVolumeRouter = "0x1111111111111111111111111111111111111111"
          , bcpVolumeCoverageStart = Nothing
          , bcpVolumeCoverageEnd = Nothing
          , bcpVolumeFinalizedThrough = Nothing
          , bcpVolumeCoverageComplete = False
          , bcpPreviousCursor = Nothing
          , bcpHasEarlier = False
          , bcpCoverageStart = Just 0
          , bcpCoverageEnd = Just 30_000
          , bcpFinalizedThrough = Just 30_000
          , bcpDatasetGeneration = 134_217_728
          , bcpCoverageComplete = True
          , bcpCandles = []
          }
        `shouldBe` object
          [ "intervalSeconds" .= (60 :: Integer)
          , "cursor" .= (30_000 :: Integer)
          , "seriesId" .= ("dxy-v1" :: String)
          , "configurationHash" .= ("sha256:test" :: String)
          , "displayPriceCap" .= ("200000000" :: String)
          , "volumeChainId" .= (421_614 :: Integer)
          , "volumeRouter" .= ("0x1111111111111111111111111111111111111111" :: String)
          , "volumeCoverageStart" .= (Nothing :: Maybe Integer)
          , "volumeCoverageEnd" .= (Nothing :: Maybe Integer)
          , "volumeFinalizedThrough" .= (Nothing :: Maybe Integer)
          , "volumeCoverageComplete" .= False
          , "previousCursor" .= (Nothing :: Maybe Integer)
          , "hasEarlier" .= False
          , "coverageStart" .= (Just 0 :: Maybe Integer)
          , "coverageEnd" .= (Just 30_000 :: Maybe Integer)
          , "finalizedThrough" .= (Just 30_000 :: Maybe Integer)
          , "datasetGeneration" .= (134_217_728 :: Integer)
          , "coverageComplete" .= True
          , "candles" .= ([] :: [BasketCandle])
          ]
