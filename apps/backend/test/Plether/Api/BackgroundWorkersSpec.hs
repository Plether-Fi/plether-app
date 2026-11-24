module Plether.Api.BackgroundWorkersSpec (spec) where

import Plether.Api.BackgroundWorkers (StartupPolicy (..), resolveStartupPolicy)
import Test.Hspec

spec :: Spec
spec = describe "isolated Kernel API background workers" $ do
  it "keeps the existing API workers on by default" $
    resolveStartupPolicy False Nothing Nothing
      `shouldBe` Right (StartupPolicy False True True)

  it "keeps a future non-canary Kernel API's existing services on" $
    resolveStartupPolicy True Nothing Nothing
      `shouldBe` Right (StartupPolicy False True True)

  it "requires an explicit disabled worker setting for the canary" $ do
    resolveStartupPolicy True (Just "true") Nothing
      `shouldBe` Left "KERNEL_CANARY_API_MODE requires API_BACKGROUND_WORKERS_ENABLED=false"
    resolveStartupPolicy True (Just "true") (Just "true")
      `shouldBe` Left "KERNEL_CANARY_API_MODE requires API_BACKGROUND_WORKERS_ENABLED=false"

  it "starts the Kernel canary without generic schema writes or workers" $
    resolveStartupPolicy True (Just "true") (Just "false")
      `shouldBe` Right (StartupPolicy True False False)

  it "rejects a canary without a Kernel gateway and malformed flags" $ do
    resolveStartupPolicy False (Just "true") (Just "false")
      `shouldBe` Left "KERNEL_CANARY_API_MODE requires an enabled Kernel gateway"
    resolveStartupPolicy True (Just "TRUE") (Just "false")
      `shouldBe` Left "KERNEL_CANARY_API_MODE must be exactly true or false"
    resolveStartupPolicy True (Just "true") (Just "FALSE")
      `shouldBe` Left "API_BACKGROUND_WORKERS_ENABLED must be exactly true or false"
