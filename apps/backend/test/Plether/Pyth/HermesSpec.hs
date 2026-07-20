module Plether.Pyth.HermesSpec (spec) where

import qualified Data.Text as T
import Plether.Config
  ( defaultPythHermesUrl
  , defaultPythLatestMaxAgeSeconds
  , maxPythLatestMaxAgeSeconds
  , validatePythLatestMaxAgeSeconds
  )
import Plether.Pyth.Hermes
  ( isPermanentHermesConfigurationError
  , resolveHermesApiKey
  )
import Test.Hspec

spec :: Spec
spec = do
  describe "Pyth Hermes defaults" $ do
    it "uses the upgraded Hermes endpoint" $
      defaultPythHermesUrl `shouldBe` "https://pyth.dourolabs.app/hermes"

    it "keeps latest payloads below the oracle freshness window" $ do
      defaultPythLatestMaxAgeSeconds `shouldBe` 10
      maxPythLatestMaxAgeSeconds `shouldBe` 10

    it "accepts only whole-second ages with safety headroom" $ do
      validatePythLatestMaxAgeSeconds "1" `shouldBe` Right 1
      validatePythLatestMaxAgeSeconds "10" `shouldBe` Right 10
      validatePythLatestMaxAgeSeconds " 10 " `shouldBe` Right 10
      validatePythLatestMaxAgeSeconds "0" `shouldSatisfy` isLeft
      validatePythLatestMaxAgeSeconds "01" `shouldSatisfy` isLeft
      validatePythLatestMaxAgeSeconds "11" `shouldSatisfy` isLeft
      validatePythLatestMaxAgeSeconds "15" `shouldSatisfy` isLeft
      validatePythLatestMaxAgeSeconds "not-a-number" `shouldSatisfy` isLeft

  describe "resolveHermesApiKey" $ do
    it "rejects a missing key for the upgraded endpoint" $
      resolveHermesApiKey defaultPythHermesUrl Nothing
        `shouldSatisfy` isMissingKeyError

    it "rejects a blank key for normalized upgraded endpoint URLs" $
      resolveHermesApiKey " HTTPS://PYTH.DOUROLABS.APP/HERMES/ " (Just " \t ")
        `shouldSatisfy` isMissingKeyError

    it "strips a configured key" $
      resolveHermesApiKey defaultPythHermesUrl (Just "  secret-key  ")
        `shouldBe` Right (Just "secret-key")

    it "rejects the known legacy endpoint even when a key is configured" $
      resolveHermesApiKey "https://hermes.pyth.network/" (Just "secret-key")
        `shouldSatisfy` isLegacyEndpointError

    it "allows keyless custom Hermes-compatible endpoints" $
      resolveHermesApiKey "https://hermes.internal.example" Nothing
        `shouldBe` Right Nothing

  describe "isPermanentHermesConfigurationError" $ do
    it "classifies missing credentials and authorization failures" $ do
      isPermanentHermesConfigurationError "PYTH_API_KEY is required for the upgraded endpoint"
        `shouldBe` True
      isPermanentHermesConfigurationError "Hermes returned HTTP 403: not entitled"
        `shouldBe` True

    it "keeps rate limits and transient network failures retryable" $ do
      isPermanentHermesConfigurationError "Hermes returned HTTP 429; retry after 60s"
        `shouldBe` False
      isPermanentHermesConfigurationError "connection timed out"
        `shouldBe` False
  where
    isMissingKeyError result =
      case result of
        Left err -> "PYTH_API_KEY" `T.isInfixOf` err
        Right _ -> False

    isLegacyEndpointError result =
      case result of
        Left err -> "legacy" `T.isInfixOf` T.toLower err
        Right _ -> False

    isLeft result =
      case result of
        Left _ -> True
        Right _ -> False
