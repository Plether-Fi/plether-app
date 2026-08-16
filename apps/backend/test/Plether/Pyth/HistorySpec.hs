module Plether.Pyth.HistorySpec (spec) where

import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Aeson (Value (Null))
import Plether.Database.Candles (RollupCoverage (..), RollupKind (..), defaultBasketSeriesId)
import Plether.Pyth.Basket (BasketComponent (..), PythPricePoint (..), basketComponents)
import Plether.Pyth.History
  ( basketBackfillGridWindows
  , basketObservationId
  , decodeTradingViewCloseHistory
  , deriveEarliestBasketGridTimestamp
  , deriveBasketHistoryObservation
  , deriveTradingViewBasketHistory
  , filterTradingViewHistorySamplesForPersistence
  , legacyObservationId
  , minimumBasketHistoryPublicationEnd
  )
import Test.Hspec

spec :: Spec
spec = do
  describe "deriveEarliestBasketGridTimestamp" $ do
    it "aligns an arbitrary selected target to the first complete sampling bucket" $
      deriveEarliestBasketGridTimestamp 60 1_800_000 30 (Just 121)
        `shouldBe` Right 180

    it "accepts the Unix epoch as an explicit history target" $
      deriveEarliestBasketGridTimestamp 60 1_800_000 30 (Just 0)
        `shouldBe` Right 0

    it "falls back to the configured relative-day window without a target" $
      deriveEarliestBasketGridTimestamp 60 1_800_000 7 Nothing
        `shouldBe` Right (1_800_000 - 7 * 86_400)

  describe "basketBackfillGridWindows" $ do
    it "keeps an epoch-to-present grid bounded without dropping or repeating boundaries" $ do
      let windows = basketBackfillGridWindows 60 0 (2_880 * 60)
      windows
        `shouldBe`
          [ (0, 1_439 * 60)
          , (1_440 * 60, 2_879 * 60)
          , (2_880 * 60, 2_880 * 60)
          ]

    it "uses one window for an exact maximum-size grid and rejects invalid ranges" $ do
      basketBackfillGridWindows 60 120 (120 + 1_439 * 60)
        `shouldBe` [(120, 120 + 1_439 * 60)]
      basketBackfillGridWindows 60 180 120 `shouldBe` []
      basketBackfillGridWindows 0 0 60 `shouldBe` []

  describe "minimumBasketHistoryPublicationEnd" $ do
    it "waits for a full UTC daily bucket after an arbitrary target" $ do
      minimumBasketHistoryPublicationEnd 0 `shouldBe` 86_400
      minimumBasketHistoryPublicationEnd 1 `shouldBe` 172_800
      minimumBasketHistoryPublicationEnd (86_400 + 60) `shouldBe` 259_200

  describe "Pyth TradingView bulk history" $ do
    let componentClose component =
          fromInteger
            ( if bcInverted component
                then 10 ^ (16 :: Int) `div` bcBasePrice component
                else bcBasePrice component
            )
            / 100_000_000

    it "decodes matching close arrays and accepts an explicit no-data response" $ do
      decodeTradingViewCloseHistory
        (LBS8.pack "{\"s\":\"ok\",\"t\":[120,180],\"c\":[1.25,1.5]}")
        `shouldBe` Right [(120, 1.25), (180, 1.5)]
      decodeTradingViewCloseHistory (LBS8.pack "{\"s\":\"no_data\"}")
        `shouldBe` Right []

    it "rejects partial timestamp/close arrays and endpoint errors" $ do
      decodeTradingViewCloseHistory
        (LBS8.pack "{\"s\":\"ok\",\"t\":[120,180],\"c\":[1.25]}")
        `shouldSatisfy` isLeft
      decodeTradingViewCloseHistory
        (LBS8.pack "{\"s\":\"error\",\"errmsg\":\"unknown symbol\"}")
        `shouldSatisfy` isLeft

    it "carries fresh closes across the canonical minute grid" $ do
      let timestamps = [120, 240]
          series =
            [ (component, [(timestamp, componentClose component) | timestamp <- timestamps])
            | component <- basketComponents
            ]
      case deriveTradingViewBasketHistory 120 300 series of
        Left err -> expectationFailure $ "unexpected TradingView basket failure: " <> show err
        Right samples ->
          map (\(timestamp, _, _) -> timestamp) samples `shouldBe` [120, 180, 240]

    it "combines independently updating component grids by recent as-of close" $ do
      case basketComponents of
        [] -> expectationFailure "basket configuration unexpectedly has no components"
        firstComponent : remainingComponents -> do
          let independent =
                ( firstComponent
                , [ (300, componentClose firstComponent)
                  , (660, componentClose firstComponent)
                  , (780, componentClose firstComponent)
                  ]
                )
                  : [ ( component
                      , [ (360, componentClose component)
                        , (600, componentClose component)
                        , (720, componentClose component)
                        ]
                      )
                    | component <- remainingComponents
                    ]
          case deriveTradingViewBasketHistory 600 840 independent of
            Left err -> expectationFailure $ "unexpected as-of basket failure: " <> show err
            Right samples ->
              map (\(timestamp, _, _) -> timestamp) samples
                `shouldBe` [600, 660, 720, 780]

    it "seeds a window boundary from the bounded five-minute lookback" $ do
      let series =
            [ (component, [(300, componentClose component)])
            | component <- basketComponents
            ]
      case deriveTradingViewBasketHistory 600 720 series of
        Left err -> expectationFailure $ "unexpected boundary seed failure: " <> show err
        Right samples ->
          map (\(timestamp, _, _) -> timestamp) samples `shouldBe` [600]

    it "does not carry a stale component beyond five minutes" $ do
      case basketComponents of
        [] -> expectationFailure "basket configuration unexpectedly has no components"
        firstComponent : remainingComponents -> do
          let series =
                (firstComponent, [(300, componentClose firstComponent)])
                  : [ ( component
                      , [ (timestamp, componentClose component)
                        | timestamp <- [600, 660 .. 840]
                        ]
                      )
                    | component <- remainingComponents
                    ]
          case deriveTradingViewBasketHistory 600 900 series of
            Left err -> expectationFailure $ "unexpected stale-gap failure: " <> show err
            Right samples ->
              map (\(timestamp, _, _) -> timestamp) samples `shouldBe` [600]

    it "accepts a unanimous empty no-update window and rejects unaligned data" $ do
      deriveTradingViewBasketHistory
        120
        240
        [(component, []) | component <- basketComponents]
        `shouldBe` Right []
      deriveTradingViewBasketHistory
        120
        240
        [ (component, [(121, componentClose component)])
        | component <- basketComponents
        ]
        `shouldSatisfy` isLeft
      deriveTradingViewBasketHistory
        600
        720
        [ (component, [(240, componentClose component)])
        | component <- basketComponents
        ]
        `shouldSatisfy` isLeft

    it "rejects duplicate and out-of-lookback timestamps" $ do
      deriveTradingViewBasketHistory
        600
        720
        [ ( component
          , [ (600, componentClose component)
            , (600, componentClose component)
            ]
          )
        | component <- basketComponents
        ]
        `shouldSatisfy` isLeft

    it "persists only the missing prefix before trusted published coverage" $ do
      let samples =
            [ (540, 100, Null)
            , (600, 101, Null)
            , (660, 102, Null)
            , (1_260, 103, Null)
            ]
      filterTradingViewHistorySamplesForPersistence
        (Just trustedPriceCoverage)
        samples
        `shouldBe` Right [(540, 100, Null)]

    it "keeps the published boundary while coverage is incomplete or under maintenance" $ do
      let samples = [(540, 100, Null), (600, 101, Null)]
          prefixOnly = [(540, 100, Null)]
          repairingCoverage =
            trustedPriceCoverage
              { rcComplete = False
              , rcGeneration = 4
              , rcLastError = Just "bounded_admin_repair"
              , rcMaintenanceFrom = Just 600
              , rcMaintenanceTo = Just 1_200
              }
      filterTradingViewHistorySamplesForPersistence
        (Just trustedPriceCoverage {rcComplete = False, rcLastError = Just "price_watermark_gap"})
        samples
        `shouldBe` Right prefixOnly
      filterTradingViewHistorySamplesForPersistence
        (Just repairingCoverage)
        samples
        `shouldBe` Right prefixOnly

    it "persists the full target window without a structurally valid published boundary" $ do
      let samples = [(540, 100, Null), (600, 101, Null)]
      filterTradingViewHistorySamplesForPersistence Nothing samples
        `shouldBe` Right samples
      filterTradingViewHistorySamplesForPersistence
        (Just trustedPriceCoverage {rcDerivationVersion = "future"})
        samples
        `shouldSatisfy` isLeft
      filterTradingViewHistorySamplesForPersistence
        (Just trustedPriceCoverage {rcFinalizedThrough = rcCoverageStart trustedPriceCoverage})
        samples
        `shouldBe` Right samples

  describe "basketObservationId" $ do
    let eur = PythPricePoint "0x02" 117_500_000 100 (-8) 1_785_437_841
        yen = PythPricePoint "0x01" 638_000 20 (-8) 1_785_437_840

    it "is independent of feed ordering" $
      basketObservationId "plether-dxy-v1" [eur, yen]
        `shouldBe` basketObservationId "plether-dxy-v1" [yen, eur]

    it "binds every observation to its basket definition" $
      basketObservationId "plether-dxy-v1" [eur, yen]
        `shouldNotBe` basketObservationId "plether-dxy-v2" [eur, yen]

    it "changes when signed price metadata changes" $
      basketObservationId "plether-dxy-v1" [eur, yen]
        `shouldNotBe` basketObservationId "plether-dxy-v1" [eur {pppConfidence = 101}, yen]

  describe "legacy benchmark observation admission" $ do
    let points = zipWith historyPoint basketComponents [100 .. 105]

    it "uses the minimum component publish time after divergence validation" $
      case deriveBasketHistoryObservation 100 60 points of
        Left err -> expectationFailure $ "unexpected legacy admission failure: " <> show err
        Right (canonicalPublishTime, _, _, _) -> canonicalPublishTime `shouldBe` 100

    it "rejects a component set outside the signed-path divergence policy" $
      deriveBasketHistoryObservation
        100
        60
        (zipWith historyPoint basketComponents [100, 101, 102, 103, 104, 106])
        `shouldSatisfy` isLeft

    it "accepts both endpoints of the official inclusive benchmark window" $ do
      let lowerBoundary = map (historyPointAt 100) basketComponents
          upperBoundary = map (historyPointAt 160) basketComponents
      deriveBasketHistoryObservation 100 60 lowerBoundary `shouldSatisfy` isRight
      case deriveBasketHistoryObservation 100 60 upperBoundary of
        Left err -> expectationFailure $ "unexpected exact-end admission failure: " <> show err
        Right (canonicalPublishTime, _, _, _) -> canonicalPublishTime `shouldBe` 160

    it "rejects any component outside the requested inclusive window" $ do
      let beforeWindow = map (historyPointAt 99) basketComponents
          afterWindow = map (historyPointAt 161) basketComponents
          straddlesEnd = zipWith historyPoint basketComponents [159 .. 164]
      deriveBasketHistoryObservation 100 60 beforeWindow `shouldSatisfy` isLeft
      deriveBasketHistoryObservation 100 60 afterWindow `shouldSatisfy` isLeft
      deriveBasketHistoryObservation 100 60 straddlesEnd `shouldSatisfy` isLeft

    it "deduplicates sampling slots by feed times while permitting corrections" $ do
      let corrected = map (\point -> point {pppPrice = pppPrice point + 1}) points
      legacyObservationId "plether-dxy-v1" points
        `shouldBe` legacyObservationId "plether-dxy-v1" (reverse points)
      legacyObservationId "plether-dxy-v1" points
        `shouldBe` legacyObservationId "plether-dxy-v1" corrected
      legacyObservationId "plether-dxy-v1" points
        `shouldNotBe` legacyObservationId "plether-dxy-v2" points
      legacyObservationId "plether-dxy-v1" points
        `shouldNotBe`
          legacyObservationId
            "plether-dxy-v1"
            ((head points) {pppPublishTime = 99} : tail points)

trustedPriceCoverage :: RollupCoverage
trustedPriceCoverage =
  RollupCoverage
    { rcKind = PriceRollup
    , rcSeriesId = Just defaultBasketSeriesId
    , rcChainId = Nothing
    , rcReleaseRouter = Nothing
    , rcIntervalSeconds = 60
    , rcCoverageStart = Just 600
    , rcCoverageEnd = Just 1_200
    , rcFinalizedThrough = Just 1_140
    , rcGeneration = 3
    , rcComplete = True
    , rcDerivationVersion = "v1"
    , rcLastError = Nothing
    , rcMaintenanceFrom = Nothing
    , rcMaintenanceTo = Nothing
    }

historyPoint :: BasketComponent -> Integer -> PythPricePoint
historyPoint component publishTime =
  PythPricePoint
    { pppFeedId = bcFeedId component
    , pppPrice =
        if bcInverted component
          then 10 ^ (16 :: Int) `div` bcBasePrice component
          else bcBasePrice component
    , pppConfidence = 1
    , pppExponent = -8
    , pppPublishTime = publishTime
    }

historyPointAt :: Integer -> BasketComponent -> PythPricePoint
historyPointAt publishTime component = historyPoint component publishTime

isLeft :: Either a b -> Bool
isLeft value =
  case value of
    Left _ -> True
    Right _ -> False

isRight :: Either a b -> Bool
isRight = not . isLeft
