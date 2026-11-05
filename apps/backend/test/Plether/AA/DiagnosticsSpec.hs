module Plether.AA.DiagnosticsSpec (spec) where
import Data.Aeson (Value(String), object, (.=))
import Plether.AA.Diagnostics (parseBrowserStage, BrowserFailure(..))
import Test.Hspec

spec :: Spec
spec = describe "Attempt timeline input" $ do
  let attempt = "12345678-1234-4123-8123-123456789abc" :: String
  it "accepts a bounded advisory browser stage" $
    parseBrowserStage (object ["attemptId" .= attempt, "stage" .= String "wallet_approved"])
      `shouldBe` Just ("12345678-1234-4123-8123-123456789abc", "wallet_approved", Nothing)
  it "rejects backend provenance spoofing and arbitrary error payloads" $ do
    parseBrowserStage (object ["attemptId" .= attempt, "stage" .= String "submission_received"]) `shouldBe` Nothing
    parseBrowserStage (object ["attemptId" .= attempt, "stage" .= String "wallet_approved", "source" .= String "backend"]) `shouldBe` Nothing
    parseBrowserStage (object ["attemptId" .= attempt, "stage" .= String "wallet_approved", "signature" .= String "secret"]) `shouldBe` Nothing
    parseBrowserStage (object ["attemptId" .= attempt, "stage" .= String "raw wallet error"]) `shouldBe` Nothing
  it "rejects invalid identifiers" $
    parseBrowserStage (object ["attemptId" .= String "not-an-attempt", "stage" .= String "wallet_approved"]) `shouldBe` Nothing

  it "accepts only a complete safe failure pair on an interruption" $ do
    let report step reason = object ["attemptId" .= attempt, "stage" .= String "execution_interrupted",
          "failureStep" .= String step, "reasonCode" .= String reason]
    parseBrowserStage (report "wallet_approval" "WALLET_DECLINED") `shouldBe`
      Just ("12345678-1234-4123-8123-123456789abc", "execution_interrupted", Just $ BrowserFailure "wallet_approval" "WALLET_DECLINED")
    parseBrowserStage (report "private wallet data" "WALLET_DECLINED") `shouldBe` Nothing
    parseBrowserStage (report "wallet_approval" "private_signature") `shouldBe` Nothing
    parseBrowserStage (object ["attemptId" .= attempt, "stage" .= String "execution_interrupted", "failureStep" .= String "wallet_approval"]) `shouldBe` Nothing
    parseBrowserStage (object ["attemptId" .= attempt, "stage" .= String "wallet_approved", "failureStep" .= String "wallet_approval", "reasonCode" .= String "WALLET_DECLINED"]) `shouldBe` Nothing
    parseBrowserStage (object ["attemptId" .= attempt, "stage" .= String "execution_interrupted", "failureStep" .= String "wallet_approval", "reasonCode" .= String "WALLET_DECLINED", "message" .= String "secret"]) `shouldBe` Nothing
