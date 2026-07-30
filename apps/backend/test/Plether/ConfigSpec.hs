module Plether.ConfigSpec (spec) where

import Plether.Config
  ( defaultProtocolExplorerEnabled
  , validateProtocolExplorerEnabled
  )
import Test.Hspec

spec :: Spec
spec = do
  describe "protocol explorer rollout configuration" $ do
    it "defaults to disabled until an audited release is explicitly enabled" $
      defaultProtocolExplorerEnabled `shouldBe` False

    it "accepts explicit enabled and disabled values" $ do
      validateProtocolExplorerEnabled " true " `shouldBe` Right True
      validateProtocolExplorerEnabled "OFF" `shouldBe` Right False
      validateProtocolExplorerEnabled "1" `shouldBe` Right True
      validateProtocolExplorerEnabled "0" `shouldBe` Right False

    it "rejects ambiguous values instead of accidentally enabling the explorer" $
      validateProtocolExplorerEnabled "eventually" `shouldBe`
        Left
          "PROTOCOL_EXPLORER_ENABLED must be one of true, false, 1, 0, yes, no, on, or off"
