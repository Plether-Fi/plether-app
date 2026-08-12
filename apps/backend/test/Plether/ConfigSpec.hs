module Plether.ConfigSpec (spec) where

import Plether.Config
  ( PerpsCandleReadMode (..)
  , PerpsCandleWriteMode (..)
  , parsePerpsCandleReadIntervals
  , parsePerpsCandleReadMode
  , parsePerpsCandleWriteMode
  , perpsCandleRollupReadEnabled
  , validatePerpsCandleModeCombination
  )
import Test.Hspec

spec :: Spec
spec =
  describe "perps candle feature configuration" $ do
    it "accepts only explicit safe write modes" $ do
      parsePerpsCandleWriteMode "off" `shouldBe` Right PerpsCandleWritesOff
      parsePerpsCandleWriteMode "DUAL" `shouldBe` Right PerpsCandleWritesDual
      parsePerpsCandleWriteMode "on" `shouldSatisfy` isLeft

    it "accepts legacy, shadow, and rollup read modes" $ do
      parsePerpsCandleReadMode "legacy" `shouldBe` Right PerpsCandleReadsLegacy
      parsePerpsCandleReadMode " shadow " `shouldBe` Right PerpsCandleReadsShadow
      parsePerpsCandleReadMode "rollup" `shouldBe` Right PerpsCandleReadsRollup
      parsePerpsCandleReadMode "enabled" `shouldSatisfy` isLeft

    it "validates every rollout interval against the canonical set" $ do
      parsePerpsCandleReadIntervals "60, 300 3600,86400"
        `shouldBe` Right [60, 300, 3600, 86_400]
      parsePerpsCandleReadIntervals "" `shouldBe` Right []
      parsePerpsCandleReadIntervals "120" `shouldSatisfy` isLeft

    it "exposes only allowlisted intervals in explicit rollup mode" $ do
      perpsCandleRollupReadEnabled PerpsCandleReadsRollup True [60, 3600] 3600
        `shouldBe` True
      perpsCandleRollupReadEnabled PerpsCandleReadsRollup True [] 3600
        `shouldBe` False
      perpsCandleRollupReadEnabled PerpsCandleReadsLegacy True [3600] 3600
        `shouldBe` False
      perpsCandleRollupReadEnabled PerpsCandleReadsShadow True [3600] 3600
        `shouldBe` False
      perpsCandleRollupReadEnabled PerpsCandleReadsRollup True [120] 120
        `shouldBe` False
      perpsCandleRollupReadEnabled PerpsCandleReadsRollup False [3600] 3600
        `shouldBe` False

    it "requires dual writes before any rollup interval is allowlisted" $ do
      validatePerpsCandleModeCombination
        PerpsCandleWritesOff
        PerpsCandleReadsRollup
        [3600]
        True
        `shouldSatisfy` isLeft
      validatePerpsCandleModeCombination
        PerpsCandleWritesDual
        PerpsCandleReadsRollup
        [3600]
        True
        `shouldBe` Right ()
      validatePerpsCandleModeCombination
        PerpsCandleWritesOff
        PerpsCandleReadsRollup
        []
        True
        `shouldBe` Right ()
      validatePerpsCandleModeCombination
        PerpsCandleWritesOff
        PerpsCandleReadsLegacy
        [3600]
        True
        `shouldSatisfy` isLeft

    it "requires strict coverage whenever rollup read mode is selected" $ do
      validatePerpsCandleModeCombination
        PerpsCandleWritesDual
        PerpsCandleReadsRollup
        [3600]
        False
        `shouldSatisfy` isLeft
      validatePerpsCandleModeCombination
        PerpsCandleWritesOff
        PerpsCandleReadsLegacy
        []
        False
        `shouldBe` Right ()
  where
    isLeft (Left _) = True
    isLeft (Right _) = False
