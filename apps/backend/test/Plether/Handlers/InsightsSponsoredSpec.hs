module Plether.Handlers.InsightsSponsoredSpec (spec) where

import Data.Aeson (Value (Null), toJSON)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import Plether.Ethereum.Abi (encodeUint256)
import Plether.Handlers.InsightsSponsored
  ( checkBundlerEntryPoint
  , classifyAccountBytecode
  , classifyKernelHealth
  , classifySponsorshipGate
  , computeInclusionLatencySeconds
  , computeIntLatencyPercentiles
  , computeRunwayHours
  , computeSuccessRateBps
  , computeUtilizationBps
  , decodeEntryPointBalance
  , decodePaymasterPaused
  , parseLogBlockTimestamp
  , resolveInclusionEpoch
  , sanitizeControlEventReason
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

  it "never labels resume control events as Safety pause active" $ do
    sanitizeControlEventReason "resume" "https://rpc.internal/secret failed"
      `shouldBe` "Sponsorship issuance resumed"
    sanitizeControlEventReason "resume" ""
      `shouldBe` "Sponsorship issuance resumed"
    sanitizeControlEventReason "resume" "EntryPoint deposit dropped below minimum"
      `shouldBe` "Resumed after: Paymaster EntryPoint deposit below minimum floor"
    sanitizeControlEventReason "pause" "https://rpc.internal/secret failed"
      `shouldBe` "Safety pause active"

  it "computes on-chain UserOp inclusion latency from log blockTimestamp or perps_events timestamp" $ do
    parseLogBlockTimestamp (Just "0x67000010") `shouldBe` Just 1728053264
    parseLogBlockTimestamp (Just "0x0") `shouldBe` Nothing
    parseLogBlockTimestamp Nothing `shouldBe` Nothing
    resolveInclusionEpoch (Just "0x67000010") (Just 1728053200) `shouldBe` Just 1728053264
    resolveInclusionEpoch Nothing (Just 1728053200) `shouldBe` Just 1728053200
    resolveInclusionEpoch Nothing Nothing `shouldBe` Nothing
    computeInclusionLatencySeconds (Just 1728053264) 1728053261 `shouldBe` Just 3
    computeInclusionLatencySeconds (Just 1728053260) 1728053264 `shouldBe` Just 0
    computeInclusionLatencySeconds Nothing 1728053264 `shouldBe` Nothing
    computeIntLatencyPercentiles [1, 2, 2, 3, 4, 5, 6, 7, 8, 12] `shouldBe` (Just 4, Just 12)
    computeIntLatencyPercentiles [] `shouldBe` (Nothing, Nothing)

  it "classifies ZeroDev Kernel v4 Trading Account stack health fail-closed" $ do
    classifyKernelHealth True True True True True Nothing `shouldBe` "healthy"
    classifyKernelHealth True False True True True Nothing `shouldBe` "healthy"
    classifyKernelHealth False True True True True Nothing `shouldBe` "healthy"
    classifyKernelHealth False False True True True Nothing `shouldBe` "disabled"
    classifyKernelHealth True True True True True (Just "Safety pause active") `shouldBe` "paused"
    classifyKernelHealth True True False True True Nothing `shouldBe` "degraded"
    classifyKernelHealth True True True False True Nothing `shouldBe` "degraded"
    classifyKernelHealth True True True True False Nothing `shouldBe` "degraded"

  it "classifies Kernel smart account bytecode against pinned proxy code hash" $ do
    classifyAccountBytecode BS.empty "0x1234" `shouldBe` (False, Nothing, Nothing)
    let sample = BS.pack [0x60, 0x00, 0x60, 0x00]
        (deployed, mHash, mMismatch) = classifyAccountBytecode sample "0x0000000000000000000000000000000000000000000000000000000000000000"
    deployed `shouldBe` True
    mMismatch `shouldBe` Just False
    case mHash of
      Nothing -> expectationFailure "Expected non-empty code hash for deployed bytecode"
      Just h -> classifyAccountBytecode sample (T.toUpper h) `shouldBe` (True, Just h, Just True)
