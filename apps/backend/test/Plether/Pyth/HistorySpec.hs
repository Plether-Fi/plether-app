module Plether.Pyth.HistorySpec (spec) where

import Plether.Pyth.Basket (BasketComponent (..), PythPricePoint (..), basketComponents)
import Plether.Pyth.History
  ( basketObservationId
  , deriveBasketHistoryObservation
  , legacyObservationId
  )
import Test.Hspec

spec :: Spec
spec = do
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
