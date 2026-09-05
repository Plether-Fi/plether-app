{-# LANGUAGE LambdaCase #-}

module Plether.AA.PaymasterSpec (spec) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Plether.AA.Paymaster
import Plether.AA.Gateway
  ( ownerAllowedForNativeCanary
  , nativeAccountRateClientKey
  , nativeMaxFeeAllowance
  , nativeStartupFailure
  , validateHardEconomicCaps
  )
import Plether.AA.Pimlico (ProxyFailure (..))
import Plether.Config (NativeAaConfig (..))
import Plether.Ethereum.Abi (keccak256)
import Test.Hspec

spec :: Spec
spec = do
  describe "Plether verifying-paymaster wire format" $ do
    it "matches the independent Solidity/viem EIP-712 fixture" $ do
      operation <- parseFixture
      let envelope =
            makeSponsorshipEnvelope
              fixtureConfig
              1_800_000_000
              1_900_000_000
              1_000_000_000_000_000
              (BS.replicate 65 0)
      hex (sponsorshipDigest operation envelope)
        `shouldBe` "0xd92042495de3ae32c76391a73aeb6bfaf515af2dd3da45c9a8921b5310cde1ea"

    it "encodes and decodes the exact 157-byte paymasterData envelope" $ do
      operation <- parseFixture
      let envelope =
            makeSponsorshipEnvelope
              fixtureConfig
              1_899_999_700
              1_900_000_000
              1_000_000_000_000_000
              dummyPaymasterSignature
          packed = applyPaymasterEnvelope operation envelope
      BS.length (maybe BS.empty id $ puoPaymasterData packed) `shouldBe` 157
      BS.length (paymasterAndData packed) `shouldBe` 209
      decodeSponsorshipEnvelope fixtureConfig packed `shouldBe` Right envelope

    it "pins the reviewed Sponsorship type hash" $ do
      hex sponsorshipTypeHash
        `shouldBe` "0x5835c142c681b663470a1a53c34b0ba256a8283b7b9f9560aadb85711d252918"

    it "accepts viem's unsigned pm_* shape with only stub paymaster gas fields" $ do
      let Object base = fixtureOperation
          unsigned =
            KM.insert "paymasterVerificationGasLimit" (String "0x186a0") $
              KM.insert "paymasterPostOpGasLimit" (String "0x0") base
      parsed <- expectRight $ parsePackedUserOperation unsigned
      puoSignature parsed `shouldBe` BS.empty
      puoPaymaster parsed `shouldBe` Nothing
      puoPaymasterVerificationGasLimit parsed `shouldBe` Just 100_000

    it "binds every gas field and paymaster signature in the final UserOperation hash" $ do
      operation <- parseFixture
      let envelope =
            makeSponsorshipEnvelope fixtureConfig 10 100 10_000 (BS.replicate 65 1)
          first = applyPaymasterEnvelope operation envelope
          second = applyPaymasterEnvelope operation envelope {seSignature = BS.replicate 65 2}
      userOperationHash first `shouldNotBe` userOperationHash second

    it "matches viem 2.45.2 for the EntryPoint v0.8 EIP-712 UserOperation hash" $ do
      operation <- parseFixture
      hex (userOperationHash operation)
        `shouldBe` "0x601c358f5253f485c0f347dd89325784d77e8341db4ef05d99c9d22152949939"

    it "matches the live EntryPoint v0.8 hash with the full paymaster signature" $ do
      operation <- parseFixture
      let envelope =
            makeSponsorshipEnvelope fixtureConfig 10 100 10_000 (BS.replicate 65 1)
      hex (userOperationHash $ applyPaymasterEnvelope operation envelope)
        `shouldBe` "0xa339c78eccd42a781c1436f05396b969959c8bc0e713f44da9c84ca3b7c54c58"

    it "reserves the EntryPoint v0.8 maximum gas liability and rejects inflated fields" $ do
      operation <- parseFixture
      let envelope =
            makeSponsorshipEnvelope fixtureConfig 10 100 2_000_000_000_000_000 BS.empty
      maximumUserOperationCost operation envelope
        `shouldBe` 1_930_000_000_000_000
      validateHardEconomicCaps operation `shouldBe` Right ()
      validateHardEconomicCaps operation {puoPreVerificationGas = 1_000_001}
        `shouldSatisfy` isLeft

  describe "native owner canary" $ do
    it "returns a fixed client-safe startup-attestation failure" $ do
      pfReason nativeStartupFailure `shouldBe` "SIGNER_UNAVAILABLE"
      pfMessage nativeStartupFailure `shouldBe` "Native sponsorship startup attestation failed"
      pfRetryable nativeStartupFailure `shouldBe` True

    it "allows every verified owner only with the explicit global rollout switch" $ do
      ownerAllowedForNativeCanary fixtureConfig "0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
        `shouldBe` True

    it "fails closed when neither a canary owner nor global rollout is configured" $ do
      let config = fixtureConfig {naaCanaryOwners = [], naaGlobalRolloutEnabled = False}
      ownerAllowedForNativeCanary config "0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
        `shouldBe` False

    it "normalizes deployed and counterfactual verified owner checks" $ do
      let allowed = "0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          config = fixtureConfig {naaCanaryOwners = [allowed], naaGlobalRolloutEnabled = False}
      ownerAllowedForNativeCanary config "0xAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
        `shouldBe` True
      ownerAllowedForNativeCanary config "0xbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
        `shouldBe` False

    it "uses one client-independent sentinel for the account-wide rate bucket" $
      nativeAccountRateClientKey
        `shouldBe` "0x0000000000000000000000000000000000000000000000000000000000000000"

    it "derives the fee ceiling only from the exact dual-agreed block base fee" $ do
      nativeMaxFeeAllowance 1 `shouldBe` 1_000_000_000
      nativeMaxFeeAllowance 2_000_000_000 `shouldBe` 6_000_000_000
      nativeMaxFeeAllowance 9_000_000_000 `shouldBe` 10_000_000_000

parseFixture :: IO PackedUserOperation
parseFixture =
  case fixtureOperation of
    Object operation -> expectRight $ parsePackedUserOperation operation
    _ -> error "fixture must be an object"

fixtureOperation :: Value
fixtureOperation =
  object
    [ "sender" .= ("0x2222222222222222222222222222222222222222" :: Text)
    , "nonce" .= ("0x7" :: Text)
    , "callData" .= ("0xdeadbeef" :: Text)
    , "callGasLimit" .= ("0x7a120" :: Text)
    , "verificationGasLimit" .= ("0x3d090" :: Text)
    , "preVerificationGas" .= ("0x124f8" :: Text)
    , "maxFeePerGas" .= ("0x77359400" :: Text)
    , "maxPriorityFeePerGas" .= ("0x3b9aca00" :: Text)
    ]

fixtureConfig :: NativeAaConfig
fixtureConfig =
  NativeAaConfig
    { naaProxyOriginToken = "secret"
    , naaAltoRpcUrl = "http://alto:4337"
    , naaSecurityRpcUrl = "https://secondary-rpc.example.invalid"
    , naaPaymasterAddress = "0x1111111111111111111111111111111111111111"
    , naaPaymasterCodeHash = "0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
    , naaPolicyId = "0x998b46b747647acb0e13177c7c5e2531452f3ac9c8b0cce56f2b0fdbfdf37781"
    , naaSignerAddress = "0x3333333333333333333333333333333333333333"
    , naaKmsKeyId = "alias/test"
    , naaAccountCodeHash = hex $ keccak256 $ decode "60006000f3"
    , naaSponsorshipEnabled = True
    , naaSubmissionEnabled = True
    , naaIpRateLimitPerMinute = 120
    , naaFinalRateLimitPerMinute = 6
    , naaAccountRateLimitPerMinute = 30
    , naaMaxRequestBytes = 262144
    , naaValiditySeconds = 300
    , naaVerificationGasLimit = 100_000
    , naaPostOpGasLimit = 40_000
    , naaMaxCostWei = 10_000_000_000_000_000
    , naaAccountOutstandingWei = 20_000_000_000_000_000
    , naaClientOutstandingWei = 20_000_000_000_000_000
    , naaGlobalOutstandingWei = 100_000_000_000_000_000
    , naaAccountHourlyWei = 30_000_000_000_000_000
    , naaGlobalHourlyWei = 100_000_000_000_000_000
    , naaGlobalDailyWei = 250_000_000_000_000_000
    , naaCanaryOwners = []
    , naaGlobalRolloutEnabled = True
    }

hex :: BS.ByteString -> Text
hex value = "0x" <> TE.decodeUtf8 (B16.encode value)

decode :: BS.ByteString -> BS.ByteString
decode value = either (const BS.empty) id $ B16.decode value

expectRight :: Show err => Either err value -> IO value
expectRight = \case
  Left err -> do
    expectationFailure $ show err
    error "unreachable"
  Right value -> pure value

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False
