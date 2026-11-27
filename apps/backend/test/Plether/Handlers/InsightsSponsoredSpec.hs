module Plether.Handlers.InsightsSponsoredSpec (spec) where

import Data.Aeson (Value (Null), toJSON)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import Plether.Ethereum.Abi (encodeUint256)
import Plether.Handlers.InsightsSponsored
  ( checkBundlerEntryPoint
  , classifySponsorshipGate
  , computeRunwayHours
  , computeSuccessRateBps
  , computeUtilizationBps
  , decodeEntryPointBalance
  , decodePaymasterPaused
  , sanitizePublicReason
  )
import Test.Hspec

spec :: Spec
spec = describe "Plether.Handlers.InsightsSponsored" $ do
  it "sanitizes pause reasons and strips sensitive URLs, ARNs, and raw exception strings" $ do
    sanitizePublicReason "EntryPoint deposit dropped below AA_PAYMASTER_MIN_DEPOSIT_WEI"
      `shouldBe` "Paymaster EntryPoint deposit below minimum floor"
    sanitizePublicReason "bootstrap_fail_closed"
      `shouldBe` "Fail-closed bootstrap pause"
    sanitizePublicReason "RPC provider disagreement at block 123"
      `shouldBe` "RPC provider disagreement detected"
    sanitizePublicReason "arn:aws:kms:us-east-1:123456789012:key/secret-key-id failed https://rpc.internal/token"
      `shouldBe` "Safety pause active"
    sanitizePublicReason "MANUAL_MAINTENANCE"
      `shouldBe` "Operator safety control update"
    sanitizePublicReason "CUSTOM_GATE_HOLD"
      `shouldBe` "CUSTOM_GATE_HOLD"
    sanitizePublicReason ""
      `shouldBe` "Safety pause active"
    T.isInfixOf "arn:" (sanitizePublicReason "arn:aws:kms:abc") `shouldBe` False

  it "computes utilization bps, runway hours, and success rate bps without fabricating zeros" $ do
    computeUtilizationBps 25000 100000 `shouldBe` Just 2500
    computeUtilizationBps 10 0 `shouldBe` Nothing
    computeRunwayHours 200000000000000000 10000000000000000 `shouldBe` Just 480
    computeRunwayHours 200000000000000000 0 `shouldBe` Nothing
    computeSuccessRateBps 98 100 `shouldBe` Just 9800
    computeSuccessRateBps 0 0 `shouldBe` Nothing

  it "classifies sponsorship gate fail-closed across issuing, paused, disabled, and degraded states" $ do
    classifySponsorshipGate True True (Just False) (Just False) True (Just True) `shouldBe` "issuing"
    classifySponsorshipGate False True (Just False) (Just False) True (Just True) `shouldBe` "disabled"
    classifySponsorshipGate True False (Just False) (Just False) True (Just True) `shouldBe` "disabled"
    classifySponsorshipGate True True (Just True) (Just False) True (Just True) `shouldBe` "paused"
    classifySponsorshipGate True True (Just False) (Just True) True (Just True) `shouldBe` "paused"
    classifySponsorshipGate True True (Just False) (Just False) False (Just True) `shouldBe` "degraded"
    classifySponsorshipGate True True (Just False) (Just False) True (Just False) `shouldBe` "degraded"
    classifySponsorshipGate True True Nothing Nothing True (Just True) `shouldBe` "degraded"

  it "decodes EntryPoint balanceOf, Paymaster paused(), and Alto supported EntryPoints" $ do
    decodeEntryPointBalance (encodeUint256 250000000000000000)
      `shouldBe` Just 250000000000000000
    decodeEntryPointBalance (BS.replicate 16 0) `shouldBe` Nothing
    decodePaymasterPaused (encodeUint256 0) `shouldBe` Just False
    decodePaymasterPaused (encodeUint256 1) `shouldBe` Just True
    decodePaymasterPaused (encodeUint256 2) `shouldBe` Nothing
    let ep = "0x4337084d9e255ff0702461cf8895ce9e3b5ff108"
    checkBundlerEntryPoint ep (toJSON (["0x4337084D9e255ff0702461cf8895ce9e3b5ff108"] :: [Text]))
      `shouldBe` True
    checkBundlerEntryPoint ep (toJSON (["0x0000000000000000000000000000000000000001"] :: [Text]))
      `shouldBe` False
    checkBundlerEntryPoint ep Null `shouldBe` False
