module Plether.Types.PerpsSpec (spec) where

import Data.Aeson (object, toJSON, (.=))
import Plether.Types.Perps (BasketHistoryPoint (..), basketRangeSeconds)
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
