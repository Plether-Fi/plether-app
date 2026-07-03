module Plether.Types.ProtocolSpec (spec) where

import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Plether.Types.Protocol (Constants (..))
import Test.Hspec

spec :: Spec
spec = do
  describe "Constants JSON" $ do
    it "serializes adverseConfidenceMultiplierBps as a string" $ do
      let constants =
            Constants
              { constMaxSlippage = 0.01
              , constMinLeverage = 1.1
              , constMaxLeverage = 10.0
              , constLiquidationLtv = 0.86
              , constAdverseConfidenceMultiplierBps = 30000
              }

      LBS.unpack (encode constants)
        `shouldContain` "\"adverseConfidenceMultiplierBps\":\"30000\""
