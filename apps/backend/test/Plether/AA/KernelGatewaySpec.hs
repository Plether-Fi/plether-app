module Plether.AA.KernelGatewaySpec (spec) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import Data.Either (isLeft)
import Data.Functor.Identity (runIdentity)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Plether.AA.Gateway as Native
import Plether.AA.Kernel (KernelProfile (..), decodeKernelCalls, kernelEntryPoint, kernelFactoryArgs, kernelUserOperationHash)
import Plether.AA.KernelDeploymentFixture
  ( KernelDeploymentFixture (..), KernelGasFixture (..), loadKernelDeploymentFixture )
import Plether.AA.KernelGateway
  ( PrepareIntent (..), classifyKernelEstimate, kernelCanFund, kernelCancellationCallData, kernelFundingAdvice, kernelScope, maxPrefund
  , kernelCanaryAllowsSponsoredFollowUp, kernelOwnerAdmitted, kernelOwnerMaySubmit, minimumKernelReplacementFee, parseKernelCancellationOriginal, parseKernelPrepareIntent
  , kernelNonceScanRange, kernelOperationDeadline, locateKernelNonceTransition, parseKernelMaxSafeLag, parseOwnerAdmission
  , validateKernelEconomicCaps, validateKernelWithdrawalMode
  )
import qualified Plether.AA.Paymaster as Paymaster
import Plether.AA.Pimlico (SmartCall (..))
import qualified Plether.AA.Pimlico as Legacy
import qualified Plether.Database.KernelOperation as Db
import Plether.Ethereum.Abi (encodeAddress, encodeCall, encodeUint256)
import Test.Hspec

spec :: Spec
spec = do
  describe "Kernel safe-block lag configuration" $ do
    it "accepts the isolated Sepolia service setting and rejects out-of-range values" $ do
      fixture <- loadKernelDeploymentFixture
      let lower = fixtureMinimumSafeLag fixture
          upper = fixtureMaximumSafeLag fixture
      map (parseKernelMaxSafeLag . show) [lower, 600, upper] `shouldBe` map Right [lower, 600, upper]
      mapM_ (\raw -> parseKernelMaxSafeLag raw `shouldSatisfy` isLeft)
        [show (lower - 1), show (upper + 1), '0' : show upper]

  describe "Kernel gateway preparation intent" $ do
    it "requires the caller to choose the payer explicitly" $ do
      checkMode "sponsored" Db.Sponsored
      checkMode "eth" Db.PayWithEth
      mustReject (KM.delete "mode" $ request "sponsored")
      mustReject (request "auto")
    it "accepts paired factory arguments only for the pinned Kernel factory" $
      case kernelFactoryArgs profile owner of
        Left failure -> expectationFailure $ show failure
        Right factoryData -> do
          let fields = KM.insert "factory" (String $ kpFactory profile) $
                KM.insert "factoryData" (String factoryData) (request "eth")
          case parseKernelPrepareIntent profile fields of
            Left failure -> expectationFailure $ show failure
            Right intent -> piFactoryData intent `shouldBe` Just factoryData
          mustReject (KM.delete "factory" fields)
          mustReject (KM.delete "factoryData" fields)
          mustReject (KM.insert "factory" (String $ address 'a') fields)
          mustReject (KM.insert "factory" (String $ address '9') fields)
    it "rejects malformed or extra request fields before preparation" $ do
      let base = request "eth"
      mustReject (KM.insert "paymaster" (String $ address '9') base)
      mustReject (KM.insert "version" (Number 2) base)
      mustReject (KM.insert "preparationId" (String "0x0123") base)
      mustReject (KM.insert "sender" (String $ address '0') base)
      mustReject (KM.insert "callData" (String "0x") base)
      mustReject (KM.insert "stateOverrides" (Object KM.empty) base)

  describe "Kernel gateway gas funding" $ do
    it "uses the EntryPoint deposit before liquid ETH and reserves call value separately" $ do
      kernelCanFund 100 30 70 0 0 `shouldBe` True
      kernelCanFund 100 30 70 1 0 `shouldBe` False
      kernelCanFund 100 31 70 1 0 `shouldBe` True
      kernelCanFund 100 0 101 1 0 `shouldBe` False
    it "limits a deposit withdrawal to the deposit remaining after prefund" $ do
      kernelCanFund 100 0 101 0 1 `shouldBe` True
      kernelCanFund 100 1000 101 0 2 `shouldBe` False
      kernelCanFund 100 1000 99 0 1 `shouldBe` False
      kernelCanFund 0 0 2 0 2 `shouldBe` True
    it "includes paymaster verification gas only for a prepared sponsored operation" $ do
      checkQuote ethQuote 360000
      checkQuote sponsoredQuote 485000
    it "admits the recorded Nitro preverification gas on Kernel while preserving the legacy ceiling" $ do
      gas <- fixtureGas <$> loadKernelDeploymentFixture
      let quote = KM.insert "preVerificationGas" (quantity $ fixtureNitroPreVerificationGas gas) ethQuote
      case Paymaster.parsePackedUserOperation quote of
        Left reason -> expectationFailure $ T.unpack reason
        Right operation -> do
          validateKernelEconomicCaps operation `shouldBe` Right ()
          Native.validateHardEconomicCaps operation `shouldSatisfy` isLeft
    it "enforces every reviewed Kernel gas and fee boundary" $ do
      gas <- fixtureGas <$> loadKernelDeploymentFixture
      let units =
            [ ("callGasLimit", fixtureCallGas gas)
            , ("verificationGasLimit", fixtureVerificationGas gas)
            , ("preVerificationGas", fixturePreVerificationGas gas)
            , ("paymasterVerificationGasLimit", fixturePaymasterVerificationGas gas)
            , ("paymasterPostOpGasLimit", fixturePaymasterPostOpGas gas)
            ]
          caps = units <>
            [ ("maxFeePerGas", fixtureMaxFeePerGas gas)
            , ("maxPriorityFeePerGas", fixtureMaxPriorityFeePerGas gas)
            ]
          base = KM.insert "maxFeePerGas" (quantity $ fixtureMaxFeePerGas gas) sponsoredQuote
          setCap (field, cap) = KM.insert field (quantity cap)
          validate fields = Paymaster.parsePackedUserOperation fields >>= validateKernelEconomicCaps
      sum (map snd units) `shouldBe` fixtureAggregateGas gas
      validate (foldr setCap base caps) `shouldBe` Right ()
      mapM_ (\(field, cap) -> validate (setCap (field, cap + 1) base) `shouldSatisfy` isLeft) caps
    it "bounds ETH-paid prefund independently of the gas-unit caps" $ do
      gas <- fixtureGas <$> loadKernelDeploymentFixture
      let fee = fixtureMaxFeePerGas gas
          availableGasUnits = fixtureEthMaxPrefundWei gas `div` fee
          estimate gasUnits = Paymaster.parsePackedUserOperation $
            KM.insert "maxFeePerGas" (quantity fee) $
            KM.insert "callGasLimit" (quantity 1) $
            KM.insert "verificationGasLimit" (quantity 1) $
            KM.insert "preVerificationGas" (quantity $ gasUnits - 2) ethQuote
      (estimate availableGasUnits >>= validateKernelEconomicCaps) `shouldBe` Right ()
      (estimate (availableGasUnits + 1) >>= validateKernelEconomicCaps) `shouldSatisfy` isLeft
    it "classifies only the pinned ETH estimate prefund failure as insufficient gas" $ do
      let response code message = object ["error" .= object ["code" .= Number code, "message" .= String message]]
          pinned = "UserOperation reverted during simulation with reason: AA21 didn't pay prefund"
          reason mode = either Legacy.pfReason (const "success") . classifyKernelEstimate mode
      reason Db.PayWithEth (response (-32500) pinned) `shouldBe` "INSUFFICIENT_GAS_ETH"
      reason Db.Sponsored (response (-32500) pinned) `shouldBe` "SIMULATION_FAILED"
      reason Db.PayWithEth (response (-32521) pinned) `shouldBe` "SIMULATION_FAILED"
      reason Db.PayWithEth (response (-32500) (pinned <> " actual condition unknown")) `shouldBe` "SIMULATION_FAILED"
    it "reports the liquid top-up separately from gas prefund, EntryPoint deposit and call value" $ do
      kernelFundingAdvice 100 29 70 5 `shouldBe` Just (object
        ["maxNetworkGasWei" .= quantity 100, "balanceWei" .= quantity 29,
         "entryPointDepositWei" .= quantity 70, "callValueWei" .= quantity 5, "shortfallWei" .= quantity 6])
      kernelFundingAdvice 100 35 70 5 `shouldBe` Nothing
      kernelFundingAdvice 100 2 110 3 `shouldBe` Just (object
        ["maxNetworkGasWei" .= quantity 100, "balanceWei" .= quantity 2,
         "entryPointDepositWei" .= quantity 110, "callValueWei" .= quantity 3, "shortfallWei" .= quantity 1])

  describe "Kernel owner withdrawal policy" $ do
    it "keeps direct native and deposit withdrawals available with ETH-paid gas" $ do
      let native = SmartCall owner 1 BS.empty
          deposit = SmartCall (kpEntryPoint profile) 0 $
            encodeCall "withdrawTo(address,uint256)" [encodeAddress owner, encodeUint256 1]
      mapM_ (\call -> either (const False) id (validateKernelWithdrawalMode profile Db.PayWithEth owner [call])
        `shouldBe` True) [native, deposit]
      mapM_ (\call -> validateKernelWithdrawalMode profile Db.Sponsored owner [call]
        `shouldSatisfy` isLeft) [native, deposit]
    it "leaves ordinary sponsored operations to the protocol allowlist" $ do
      let protocolCall = SmartCall (address '7') 0 (BS.pack [1, 2, 3, 4])
      either (const True) id (validateKernelWithdrawalMode profile Db.Sponsored owner [protocolCall]) `shouldBe` False

  describe "Kernel preparation deadline" $ do
    it "requires time after the thirty-second submission margin" $ do
      let order = SmartCall (address '7') 0 $ encodeCall
            "commitOrder((bytes32,uint8,uint256,uint256,uint256,bool,(uint64,uint32,uint8,bytes32,uint256,uint256,uint256,uint256,uint256,uint256,uint256,uint256,uint32)))"
            (replicate 6 (encodeUint256 0) <> [encodeUint256 130] <> replicate 12 (encodeUint256 0))
      kernelOperationDeadline 99 [order] `shouldBe` Right (Just 130)
      either Legacy.pfReason (const "accepted") (kernelOperationDeadline 100 [order]) `shouldBe` "DEADLINE_TOO_CLOSE"
      kernelOperationDeadline 100 [] `shouldBe` Right Nothing

  describe "Kernel nonce recovery" $ do
    it "finds the nonce transition beyond the old rolling block window" $ do
      let nonceAt block = pure $ if block >= 185 then 8 else 7
      runIdentity (locateKernelNonceTransition nonceAt (Just 100) 5_000 7) `shouldBe` Just 185
      runIdentity (locateKernelNonceTransition nonceAt Nothing 5_000 7) `shouldBe` Just 185
    it "finds the first block even when the lane advances multiple times later" $ do
      let nonceAt block = pure $ if block >= 1_700 then 10 else if block >= 1_400 then 8 else 7
      runIdentity (locateKernelNonceTransition nonceAt (Just 1_500) 5_000 7) `shouldBe` Just 1_400
    it "leaves recovery unresolved if no history precedes the advanced nonce" $
      runIdentity (locateKernelNonceTransition (const $ pure 8) Nothing 5_000 7) `shouldBe` Nothing
    it "scans successive bounded log batches when historic state is pruned" $ do
      kernelNonceScanRange 100 Nothing 5_000 `shouldBe` Just (100, 2_099)
      kernelNonceScanRange 100 (Just 2_099) 5_000 `shouldBe` Just (2_100, 4_099)
      kernelNonceScanRange 100 (Just 4_099) 5_000 `shouldBe` Just (4_100, 5_000)
      kernelNonceScanRange 100 (Just 5_000) 5_000 `shouldBe` Nothing

  describe "Kernel canary owner admission" $ do
    it "opens sponsored follow-ups only for the matching two-wallet Arbitrum Sepolia canary" $ do
      let owners = [owner, address '7']
      kernelCanaryAllowsSponsoredFollowUp profile (Just owners) (reverse owners) `shouldBe` True
      kernelCanaryAllowsSponsoredFollowUp profile Nothing owners `shouldBe` False
      kernelCanaryAllowsSponsoredFollowUp profile (Just [owner]) [owner] `shouldBe` False
      kernelCanaryAllowsSponsoredFollowUp profile (Just owners) [owner, address '8'] `shouldBe` False
      kernelCanaryAllowsSponsoredFollowUp profile (Just [owner, owner]) [owner, owner] `shouldBe` False
      kernelCanaryAllowsSponsoredFollowUp profile { kpDeployment = "another-deployment" } (Just owners) owners `shouldBe` False
      kernelCanaryAllowsSponsoredFollowUp profile { kpChainId = 42161 } (Just owners) owners `shouldBe` False
    it "requires a nonempty explicit owner list before enabling either payment mode" $ do
      parseOwnerAdmission True False Nothing `shouldSatisfy` isLeft
      parseOwnerAdmission True False (Just "[]") `shouldSatisfy` isLeft
      parseOwnerAdmission False False Nothing `shouldBe` Right (Just [])
      let permitted = Just $ "[\"" <> T.unpack owner <> "\"]"
      parseOwnerAdmission True False permitted `shouldBe` Right (Just [owner])
      kernelOwnerAdmitted (Just [owner]) owner `shouldBe` True
      kernelOwnerAdmitted (Just [owner]) (address '7') `shouldBe` False
      kernelOwnerMaySubmit False (Just [owner]) (address '7') `shouldBe` False
      kernelOwnerMaySubmit True (Just [owner]) (address '7') `shouldBe` True
    it "requires a separate public switch to open broad ETH admission" $ do
      parseOwnerAdmission True True Nothing `shouldBe` Right Nothing
      kernelOwnerAdmitted Nothing (address '7') `shouldBe` True
      parseOwnerAdmission True True (Just "[]") `shouldSatisfy` isLeft
      parseOwnerAdmission True False (Just "[\"bad-address\"]") `shouldSatisfy` isLeft
      let repeated = Just $ "[\"" <> T.unpack owner <> "\",\"" <> T.unpack owner <> "\"]"
      parseOwnerAdmission True False repeated `shouldSatisfy` isLeft

  describe "Kernel cancellation quote" $ do
    it "encodes exactly one zero-value, empty-data call to the original account" $ do
      let sender = address '6'
          bytes = Base16.decode $ TE.encodeUtf8 $ T.drop 2 $ kernelCancellationCallData sender
      case bytes of
        Left failure -> expectationFailure failure
        Right calldata -> decodeKernelCalls calldata `shouldBe` Right [SmartCall sender 0 BS.empty]
    it "rounds both replacement fee bounds up and bumps even a zero fee" $
      map minimumKernelReplacementFee [0, 1, 101, 1000] `shouldBe` [1, 2, 112, 1100]
    it "binds a cancellation-only anchor to the exact stored ETH preparation and hash" $ do
      let prepared = Db.KernelPreparation (kernelScope profile) (address '6') owner 0 (hash '8') (hash '9')
            Db.PayWithEth (Object ethQuote) (posixSecondsToUTCTime 1) (Just 42)
          signature = hexText $ encodeUint256 1 <> encodeUint256 1 <> BS.singleton 27
          signed = KM.insert "signature" (String signature) ethQuote
      case Paymaster.parsePackedUserOperation signed of
        Left failure -> expectationFailure $ T.unpack failure
        Right operation -> case kernelUserOperationHash profile operation of
          Left failure -> expectationFailure $ show failure
          Right digest -> do
            let expected = hexText digest
            case parseKernelCancellationOriginal profile prepared expected signed of
              Left failure -> expectationFailure $ show failure
              Right matched -> Paymaster.puoObject matched `shouldBe` Paymaster.puoObject operation
            case parseKernelCancellationOriginal profile prepared (hash '0') signed of
              Left _ -> pure ()
              Right _ -> expectationFailure "accepted an unrelated operation hash"
            case parseKernelCancellationOriginal profile prepared expected (KM.insert "maxFeePerGas" (quantity 6) signed) of
              Left _ -> pure ()
              Right _ -> expectationFailure "accepted a mutation to the original fee"
            case parseKernelCancellationOriginal profile (prepared { Db.kpMode = Db.Sponsored }) expected signed of
              Left _ -> pure ()
              Right _ -> expectationFailure "accepted a sponsored cancellation-only anchor"
  where
    checkMode mode expected =
      case parseKernelPrepareIntent profile (request mode) of
        Left failure -> expectationFailure $ show failure
        Right intent -> do
          piId intent `shouldBe` hash '8'
          piMode intent `shouldBe` expected
          piSender intent `shouldBe` address '6'
          piFactoryData intent `shouldBe` Nothing

    mustReject fields =
      case parseKernelPrepareIntent profile fields of
        Left _ -> pure ()
        Right _ -> expectationFailure "accepted an invalid Kernel preparation intent"

    checkQuote fields expected =
      case Paymaster.parsePackedUserOperation fields of
        Left reason -> expectationFailure $ T.unpack reason
        Right operation -> maxPrefund operation `shouldBe` expected

request :: Text -> KM.KeyMap Value
request mode = KM.fromList
  [ ("version", Number 1)
  , ("preparationId", String $ hash '8')
  , ("mode", String mode)
  , ("sender", String $ address '6')
  , ("callData", String $ hexText kernelCall)
  ]

kernelCall :: ByteString
kernelCall = encodeCall "execute(bytes32,bytes)"
  [ BS.replicate 32 0
  , encodeUint256 64
  , encodeUint256 56
  , BS.drop 12 (encodeAddress $ address '7') <> encodeUint256 0 <> BS.pack [1, 2, 3, 4] <> BS.replicate 8 0
  ]

ethQuote, sponsoredQuote :: KM.KeyMap Value
ethQuote = KM.fromList
  [ ("sender", String $ address '6')
  , ("nonce", String "0x0")
  , ("callData", String $ hexText kernelCall)
  , ("callGasLimit", quantity 20000)
  , ("verificationGasLimit", quantity 50000)
  , ("preVerificationGas", quantity 2000)
  , ("maxFeePerGas", quantity 5)
  , ("maxPriorityFeePerGas", quantity 2)
  ]
sponsoredQuote = KM.union (KM.fromList
  [ ("paymaster", String $ address '9')
  , ("paymasterVerificationGasLimit", quantity 25000)
  , ("paymasterPostOpGasLimit", quantity 0)
  , ("paymasterData", String "0x01")
  ]) ethQuote

quantity :: Integer -> Value
quantity = String . Paymaster.canonicalQuantity

profile :: KernelProfile
profile = KernelProfile "kernel-sepolia-direct-v1" 421614 kernelEntryPoint (hash '1')
  (address '2') (hash '2') (address '3') (hash '3') (address '4') (hash '4') (hash '5')

owner :: Text
owner = address '8'

address, hash :: Char -> Text
address digit = "0x" <> T.replicate 40 (T.singleton digit)
hash digit = "0x" <> T.replicate 64 (T.singleton digit)

hexText :: ByteString -> Text
hexText = ("0x" <>) . TE.decodeUtf8 . Base16.encode
