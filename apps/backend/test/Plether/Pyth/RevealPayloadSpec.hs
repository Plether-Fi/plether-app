module Plether.Pyth.RevealPayloadSpec (spec) where

import Plether.Pyth.RevealPayload (validatePublishTimes, validateRevealWindow)
import Test.Hspec

spec :: Spec
spec = do
  describe "validatePublishTimes" $ do
    it "accepts six feed publish times inside divergence policy" $ do
      validatePublishTimes [101, 102, 103, 104, 105, 106] `shouldBe` Right (101, 106)

    it "rejects component publish-time divergence over policy" $ do
      validatePublishTimes [101, 102, 103, 104, 105, 107]
        `shouldSatisfy` isLeft

  describe "validateRevealWindow" $ do
    it "accepts payloads inside T+1 to T+15" $ do
      validateRevealWindow 100 15 [101, 102, 103, 104, 105, 106]
        `shouldBe` Right (101, 106)

    it "rejects payloads before T+1" $ do
      validateRevealWindow 100 15 [100, 101, 102, 103, 104, 105]
        `shouldSatisfy` isLeft

    it "rejects payloads after T+15" $ do
      validateRevealWindow 100 15 [111, 112, 113, 114, 115, 116]
        `shouldSatisfy` isLeft

isLeft :: Either a b -> Bool
isLeft value =
  case value of
    Left _ -> True
    Right _ -> False
