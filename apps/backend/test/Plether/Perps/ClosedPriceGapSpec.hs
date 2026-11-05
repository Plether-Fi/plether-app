module Plether.Perps.ClosedPriceGapSpec (spec) where

import Plether.Perps.ClosedPriceGap
  ( validateClosedPriceGapEvidence
  , withinWeeklyFxClosure
  )
import Test.Hspec

spec :: Spec
spec = do
  describe "withinWeeklyFxClosure" $ do
    it "accepts a Sunday migration gap inside the frozen FX session" $
      withinWeeklyFxClosure 1_788_088_740 1_788_123_600 `shouldBe` True

    it "rejects a range whose approved deadline crosses the Sunday reopen" $
      withinWeeklyFxClosure 1_788_088_740 1_788_123_601 `shouldBe` False

    it "matches core boundaries for the November DST transition" $ do
      withinWeeklyFxClosure 1793394000 1793570400 `shouldBe` True
      withinWeeklyFxClosure 1793393999 1793570400 `shouldBe` False
      withinWeeklyFxClosure 1793394000 1793570401 `shouldBe` False

    it "matches core boundaries for the March DST transition" $ do
      withinWeeklyFxClosure 1772834400 1773003600 `shouldBe` True
      withinWeeklyFxClosure 1772834399 1773003600 `shouldBe` False
      withinWeeklyFxClosure 1772834400 1773003601 `shouldBe` False

    it "matches core boundaries for the standard-time weekend" $ do
      withinWeeklyFxClosure 1794002400 1794175200 `shouldBe` True
      withinWeeklyFxClosure 1794002399 1794175200 `shouldBe` False
      withinWeeklyFxClosure 1794002400 1794175201 `shouldBe` False

    it "matches core boundaries for the daylight-time weekend" $ do
      withinWeeklyFxClosure 1783112400 1783285200 `shouldBe` True
      withinWeeklyFxClosure 1783112399 1783285200 `shouldBe` False
      withinWeeklyFxClosure 1783112400 1783285201 `shouldBe` False

    it "rejects ordinary weekday downtime" $
      withinWeeklyFxClosure 1_788_300_000 1_788_303_600 `shouldBe` False

  describe "validateClosedPriceGapEvidence" $ do
    let coverageEnd = 1_788_088_740
        checkedThrough = 1_788_100_000
        recoverBefore = 1_788_123_600
        latestPublishTime = 1_787_950_799
        emptySixFeedHistory = replicate 6 []

    it "accepts unanimous no-update evidence and a predating signed state" $
      validateClosedPriceGapEvidence
        coverageEnd
        checkedThrough
        recoverBefore
        latestPublishTime
        emptySixFeedHistory
        `shouldBe` Right ()

    it "rejects any component update inside the gap" $
      validateClosedPriceGapEvidence
        coverageEnd
        checkedThrough
        recoverBefore
        latestPublishTime
        ([coverageEnd + 60] : replicate 5 [])
        `shouldSatisfy` isLeft

    it "rejects missing feed evidence, a non-predating payload, and a late run" $ do
      validateClosedPriceGapEvidence
        coverageEnd checkedThrough recoverBefore latestPublishTime (replicate 5 [])
        `shouldSatisfy` isLeft
      validateClosedPriceGapEvidence
        coverageEnd checkedThrough recoverBefore coverageEnd emptySixFeedHistory
        `shouldSatisfy` isLeft
      validateClosedPriceGapEvidence
        coverageEnd recoverBefore recoverBefore latestPublishTime emptySixFeedHistory
        `shouldSatisfy` isLeft

isLeft :: Either value result -> Bool
isLeft = either (const True) (const False)
