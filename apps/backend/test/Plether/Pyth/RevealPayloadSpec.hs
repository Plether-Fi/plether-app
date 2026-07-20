module Plether.Pyth.RevealPayloadSpec (spec) where

import Plether.Pyth.RevealPayload
  ( validateLatestPublishTimes
  , validatePublishTimes
  , validateRevealWindow
  )
import Test.Hspec

spec :: Spec
spec = do
  describe "validatePublishTimes" $ do
    it "accepts six feed publish times inside divergence policy" $ do
      validatePublishTimes [101, 102, 103, 104, 105, 106] `shouldBe` Right (101, 106)

    it "rejects component publish-time divergence over policy" $ do
      validatePublishTimes [101, 102, 103, 104, 105, 107]
        `shouldSatisfy` isLeft

  describe "validateLatestPublishTimes" $ do
    it "accepts a fresh payload inside the configured age" $ do
      validateLatestPublishTimes 116 15 [101, 102, 103, 104, 105, 106]
        `shouldBe` Right (101, 106)

    it "rejects a payload whose oldest component is stale" $ do
      validateLatestPublishTimes 117 15 [101, 102, 103, 104, 105, 106]
        `shouldSatisfy` isLeft

    it "permits only bounded upstream clock skew" $ do
      validateLatestPublishTimes 100 15 (replicate 6 105)
        `shouldBe` Right (105, 105)
      validateLatestPublishTimes 100 15 (replicate 6 106)
        `shouldSatisfy` isLeft

  describe "validateRevealWindow" $ do
    it "accepts payloads starting at T+1 and ending inside T+15" $ do
      validateRevealWindow 100 15 [101, 102, 103, 104, 105, 106]
        `shouldBe` Right (101, 106)

    it "rejects payloads before T+1" $ do
      validateRevealWindow 100 15 [100, 101, 102, 103, 104, 105]
        `shouldSatisfy` isLeft

    it "rejects later in-window payloads that are not the first post-commit tick" $ do
      validateRevealWindow 100 15 [102, 103, 104, 105, 106]
        `shouldSatisfy` isLeft

    it "rejects the order 61 failure shape: a later tick inside the settlement window" $ do
      validateRevealWindow 1_782_120_343 15 (replicate 6 1_782_120_346)
        `shouldSatisfy` isLeft

    it "rejects payloads after T+15" $ do
      validateRevealWindow 100 15 [111, 112, 113, 114, 115, 116]
        `shouldSatisfy` isLeft

isLeft :: Either a b -> Bool
isLeft value =
  case value of
    Left _ -> True
    Right _ -> False
