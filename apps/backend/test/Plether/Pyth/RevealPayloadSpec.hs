module Plether.Pyth.RevealPayloadSpec (spec) where

import Plether.Pyth.RevealPayload
  ( PythPayloadAdmission (..)
  , classifyPythPayloadAdmission
  , validateLatestPublishTimes
  , validatePublishTimes
  , validateRevealWindow
  )
import Test.Hspec

spec :: Spec
spec = do
  describe "classifyPythPayloadAdmission" $ do
    it "admits the latest source only on the latest polling route" $
      classifyPythPayloadAdmission Nothing "backend_hermes_latest"
        `shouldBe` Right AdmitLatestPayload

    it "binds historical sources to the on-chain reveal bounds" $ do
      classifyPythPayloadAdmission
        (Just (101, 115))
        "backend_hermes_historical"
        `shouldBe` Right (AdmitHistoricalPayload 101 115)
      classifyPythPayloadAdmission
        (Just (201, 215))
        "backend_hermes_reveal_backfill"
        `shouldBe` Right (AdmitHistoricalPayload 201 215)

    it "rejects a latest-labelled payload on a historical reveal route" $
      classifyPythPayloadAdmission
        (Just (101, 115))
        "backend_hermes_latest"
        `shouldSatisfy` isLeft

    it "rejects a historical-labelled payload on the latest polling route" $
      classifyPythPayloadAdmission Nothing "backend_hermes_historical"
        `shouldSatisfy` isLeft

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
