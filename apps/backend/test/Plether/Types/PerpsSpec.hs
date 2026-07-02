module Plether.Types.PerpsSpec (spec) where

import Plether.Types.Perps (basketRangeSeconds)
import Test.Hspec

spec :: Spec
spec = do
  describe "basketRangeSeconds" $ do
    it "supports one year basket history ranges" $ do
      basketRangeSeconds "1y" `shouldBe` 365 * 24 * 60 * 60
