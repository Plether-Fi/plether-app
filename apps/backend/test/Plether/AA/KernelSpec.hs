{-# LANGUAGE LambdaCase #-}

module Plether.AA.KernelSpec (spec) where

import Data.Aeson (Result (..), Value (..), fromJSON, toJSON)
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import Data.Either (isLeft)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Plether.AA.Kernel
import Plether.AA.KernelDeploymentFixture (fixtureProfile, loadKernelDeploymentFixture)
import Plether.AA.Pimlico (OwnedTradingAccountFailure (..), SmartCall (..))
import Plether.Ethereum.Abi (encodeAddress, encodeCall, encodeUint256, keccak256, selector)
import Plether.Ethereum.Client (RpcError)
import Test.Hspec

spec :: Spec
spec = do
  describe "Kernel deployment profile" $ do
    it "round-trips the exact reviewed deployment profile" $ do
      raw <- fixtureProfile <$> loadKernelDeploymentFixture
      case fromJSON raw :: Result KernelProfile of
        Error failure -> expectationFailure failure
        Success reviewed -> do
          toJSON reviewed `shouldBe` raw
          kpDeployment reviewed `shouldBe` "kernel-sepolia-direct-v1"
          case raw of
            Object fields -> do
              KM.size fields `shouldBe` 14
              mapM_ (\key -> assertInvalidProfile $ Object $ KM.delete key fields) (KM.keys fields)
              let legacy = KM.insert "staker" (String $ address 'a') $
                    KM.insert "stakerCodeHash" (String $ hash 'a') fields
              assertInvalidProfile $ Object legacy
            _ -> expectationFailure "expected an object for the deployment profile"
    it "accepts only the reviewed chain, version, and EntryPoint" $ do
      fromJSON (toJSON profile) `shouldBe` Success profile
      assertInvalidProfile $ toJSON $ profile {kpEntryPoint = address '1'}

  describe "Kernel factory initialization" $ do
    it "matches the v4 ABI for a single ECDSA root owner package" $ do
      kernelFactoryArgs profile owner `shouldBe` Right (hexText factoryData)
      decodeKernelFactoryOwner profile (hexText factoryData) `shouldBe` Right owner
    it "rejects changes to the initialization profile" $ do
      decodeKernelFactoryOwner profile (hexText $ replaceWord 36 1 factoryData) `shouldSatisfy` isLeft
      decodeKernelFactoryOwner (profile {kpValidator = address '9'}) (hexText factoryData) `shouldSatisfy` isLeft
      decodeKernelFactoryOwner profile (hexText $ factoryData <> BS.replicate 32 0) `shouldSatisfy` isLeft
      decodeKernelFactoryOwner profile (hexText legacyStakerData) `shouldSatisfy` isLeft

  describe "Kernel canonical account attestation" $ do
    it "attests factory derivation and the deployed owner at the selected block" $
      verifyAccount (Just approvedRoot) Nothing `shouldReturn` Right owner
    it "attests the same owner's counterfactual account before the first operation" $
      verifyAccount Nothing (Just owner) `shouldReturn` Right owner
    it "rejects factory data on a deployed account for fresh submission but allows exact-owner replay" $ do
      verifyAccount (Just approvedRoot) (Just owner) >>= (`shouldSatisfy` isLeft)
      verifyKernelAccountWithRpcForReplayAtBlock attestedProfile (accountRpc (Just approvedRoot)) 42 (address '6') (Just owner)
        `shouldReturn` Right owner
      verifyKernelAccountWithRpcForReplayAtBlock attestedProfile (accountRpcWithOwner (Just approvedRoot) "0x" (address '9')) 42 (address '6') (Just owner)
        >>= (`shouldSatisfy` isLeft)
    it "rejects a deployed account with a delegated root wallet" $
      verifyKernelAccountWithRpcAtBlock attestedProfile
        (accountRpcWithOwner (Just approvedRoot) ("0xef0100" <> T.drop 2 (address '9')) owner) 42 (address '6') Nothing
        >>= (`shouldSatisfy` isLeft)
    it "rejects a counterfactual account with code at the root wallet" $
      verifyKernelAccountWithRpcAtBlock attestedProfile
        (accountRpcWithOwner Nothing "0x01" owner) 42 (address '6') (Just owner)
        >>= (`shouldSatisfy` isLeft)
    it "rejects a deployed account when its active root differs from the reviewed root" $
      verifyAccount (Just $ BS.replicate 32 0) Nothing >>= (`shouldSatisfy` isLeft)
    it "rejects an unexpected Kernel factory runtime" $
      verifyKernelAccountWithRpcAtBlock (attestedProfile {kpFactoryCodeHash = hash '9'})
        (accountRpc Nothing) 42 (address '6') (Just owner)
        >>= (`shouldSatisfy` isLeft)
    it "rejects a factory bound to an unexpected Kernel implementation" $
      verifyKernelAccountWithRpcAtBlock attestedProfile (accountRpcWithImplementation (address '9') Nothing "0x" owner)
        42 (address '6') (Just owner)
        >>= (`shouldSatisfy` isLeft)

  describe "Kernel owned account resolution for onboarding" $ do
    it "derives the same address before and after deployment" $ do
      resolveOwnedKernelAccountWithRpcAtBlock attestedProfile (accountRpc Nothing) 42 owner `shouldReturn` Right (address '6')
      resolveOwnedKernelAccountWithRpcAtBlock attestedProfile (accountRpc $ Just approvedRoot) 42 owner `shouldReturn` Right (address '6')
    it "rejects a contract owner wallet" $
      resolveOwnedKernelAccountWithRpcAtBlock attestedProfile (accountRpcWithOwner Nothing "0x01" owner) 42 owner
        `shouldReturn` Left OwnerWalletIsContract
    it "rejects a deployed address whose root signer differs from the deriving owner" $
      resolveOwnedKernelAccountWithRpcAtBlock attestedProfile (accountRpcWithOwner (Just approvedRoot) "0x" (address '9')) 42 owner
        `shouldReturn` Left OwnedTradingAccountProofUnavailable

  describe "Kernel execution" $ do
    it "decodes canonical ERC-7579 single and batch executions for the shared Perps policy" $ do
      decodeKernelCalls singleCall `shouldBe` Right [SmartCall (address '7') 0 (fromHex "12345678")]
      decodeKernelCalls batchCalls `shouldBe` Right
        [SmartCall (address '7') 0 (fromHex "12345678"), SmartCall (address '8') 0 (fromHex "87654321")]
    it "rejects unsupported modes and malformed or padded calldata" $ do
      decodeKernelCalls (replaceWord 4 1 singleCall) `shouldSatisfy` isLeft
      decodeKernelCalls (batchCalls <> BS.replicate 32 0) `shouldSatisfy` isLeft
      decodeKernelCalls (BS.init singleCall <> BS.singleton 1) `shouldSatisfy` isLeft

  describe "Kernel payment and authorization" $ do
    it "decodes the full uint256 EntryPoint balance without a stake response" $ do
      let largest = 2 ^ (256 :: Int) - 1
      decodeKernelEntryPointDeposit (encodeUint256 0) `shouldBe` Right 0
      decodeKernelEntryPointDeposit (encodeUint256 largest) `shouldBe` Right largest
      decodeKernelEntryPointDeposit BS.empty `shouldSatisfy` isLeft
      decodeKernelEntryPointDeposit (BS.replicate 31 0) `shouldSatisfy` isLeft
      decodeKernelEntryPointDeposit (BS.replicate 160 0) `shouldSatisfy` isLeft
    it "matches Viem's EntryPoint 0.9 UserOperation hashes in both modes" $ do
      let parsed mode fields = parseKernelOperation profile mode KernelSubmit fields >>= kernelUserOperationHash profile . koPacked
      parsed KernelEthPaid ethPaidOperation `shouldBe` Right (fromHex "3dca3463f5d6fc8d17ce155f16e7c1b056c5697e99a7082ec0c532e4fd8cf17f")
      parsed KernelSponsored sponsoredOperation `shouldBe` Right (fromHex "1bc663d681cf1e1c7f4553592ae689775d9a944bcae12af911d73395d9bed230")
    it "checks the signed hash against the verified owner for both wallet signature forms" $ do
      mapM_ verifyOwner [rawOwnerSignature, personalOwnerSignature]
    it "enforces the saved gas mode, root nonce lane and reviewed envelope format" $ do
      let parse mode = parseKernelOperation profile mode KernelSubmit
      parse KernelSponsored ethPaidOperation `shouldSatisfy` isLeft
      parse KernelEthPaid sponsoredOperation `shouldSatisfy` isLeft
      parse KernelEthPaid (KM.insert "nonce" (String "0x10000000000000000") ethPaidOperation) `shouldSatisfy` isLeft
      parse KernelEthPaid (KM.insert "eip7702Auth" Null ethPaidOperation) `shouldSatisfy` isLeft
      parse KernelSponsored
        (KM.insert "paymasterData" (String $ hexText $ BS.take 149 sponsorEnvelope <> fromHex "22e325a297439656") sponsoredOperation)
        `shouldSatisfy` isLeft
  where
    assertInvalidProfile raw = case fromJSON raw :: Result KernelProfile of
      Error _ -> pure ()
      Success _ -> expectationFailure "accepted a profile outside the reviewed schema"
    verifyOwner signature =
      case parseKernelOperation profile KernelEthPaid KernelSubmit (KM.insert "signature" signature ethPaidOperation) of
        Left failure -> expectationFailure $ show failure
        Right operation -> do
          verifyKernelSignature profile owner (koPacked operation) `shouldReturn` Right ()
          verifyKernelSignature profile (address '9') (koPacked operation) >>= (`shouldSatisfy` isLeft)
    verifyAccount root = verifyKernelAccountWithRpcAtBlock attestedProfile (accountRpc root) 42 (address '6')

-- ABI, hash and signature vectors cross-checked independently with Viem.
ethPaidOperation :: KM.KeyMap Value
ethPaidOperation = KM.fromList
  [ ("sender", String $ address '6')
  , ("nonce", String "0x0")
  , ("callData", String $ hexText singleCall)
  , ("callGasLimit", String "0x5208")
  , ("verificationGasLimit", String "0xc350")
  , ("preVerificationGas", String "0x2710")
  , ("maxFeePerGas", String "0x20")
  , ("maxPriorityFeePerGas", String "0x2")
  , ("signature", rawOwnerSignature)
  ]

sponsoredOperation :: KM.KeyMap Value
sponsoredOperation = KM.union (KM.fromList
  [ ("factory", String $ kpFactory profile)
  , ("factoryData", String $ hexText factoryData)
  , ("paymaster", String $ address '9')
  , ("paymasterVerificationGasLimit", String "0x6590")
  , ("paymasterPostOpGasLimit", String "0x0")
  , ("paymasterData", String $ hexText sponsorEnvelope)
  ]) ethPaidOperation

sponsorEnvelope :: ByteString
sponsorEnvelope = BS.replicate 60 0 <> BS.replicate 32 0x55 <> BS.replicate 64 0x11 <> BS.singleton 27

rawOwnerSignature, personalOwnerSignature :: Value
rawOwnerSignature = String "0x1ab53b002cdeb5d339a76ef935ac3444b872d9b547671a1acf7db332cbec30985dec12eff10bea320e765deae7d93af2f3c73d7c786e5d17b175a680feef0f431c"
personalOwnerSignature = String "0x8bd8016e32b821b82d63ea9fb19cae7bb5ea11f329a76d2febf6e541164187677d806a7f4af609fcabbf0d01d51936f4acba392105d8131ceed957eb2d1e160d1b"

singleCall, batchCalls, factoryData, legacyStakerData :: ByteString
singleCall = fromHex $ T.concat
  [ "e9ae5c53"
  , "0000000000000000000000000000000000000000000000000000000000000000"
  , "0000000000000000000000000000000000000000000000000000000000000040"
  , "0000000000000000000000000000000000000000000000000000000000000038"
  , "7777777777777777777777777777777777777777000000000000000000000000"
  , "0000000000000000000000000000000000000000123456780000000000000000"
  ]
batchCalls = fromHex $ T.concat
  [ "e9ae5c53"
  , "0100000000000000000000000000000000000000000000000000000000000000"
  , "0000000000000000000000000000000000000000000000000000000000000040"
  , "00000000000000000000000000000000000000000000000000000000000001c0"
  , "0000000000000000000000000000000000000000000000000000000000000020"
  , "0000000000000000000000000000000000000000000000000000000000000002"
  , "0000000000000000000000000000000000000000000000000000000000000040"
  , "00000000000000000000000000000000000000000000000000000000000000e0"
  , "0000000000000000000000007777777777777777777777777777777777777777"
  , "0000000000000000000000000000000000000000000000000000000000000000"
  , "0000000000000000000000000000000000000000000000000000000000000060"
  , "0000000000000000000000000000000000000000000000000000000000000004"
  , "1234567800000000000000000000000000000000000000000000000000000000"
  , "0000000000000000000000008888888888888888888888888888888888888888"
  , "0000000000000000000000000000000000000000000000000000000000000000"
  , "0000000000000000000000000000000000000000000000000000000000000060"
  , "0000000000000000000000000000000000000000000000000000000000000004"
  , "8765432100000000000000000000000000000000000000000000000000000000"
  ]
factoryData = fromHex $ T.concat
  [ "0609747b"
  , "0000000000000000000000000000000000000000000000000000000000000040"
  , "0000000000000000000000000000000000000000000000000000000000000000"
  , "0000000000000000000000000000000000000000000000000000000000000001"
  , "0000000000000000000000000000000000000000000000000000000000000020"
  , "0000000000000000000000000000000000000000000000000000000000000001"
  , "0000000000000000000000004444444444444444444444444444444444444444"
  , "0000000000000000000000000000000000000000000000000000000000000080"
  , "00000000000000000000000000000000000000000000000000000000000000c0"
  , "0000000000000000000000000000000000000000000000000000000000000014"
  , "7e5f4552091a69125d5dfcb7b8c2659029395bdf000000000000000000000000"
  , "0000000000000000000000000000000000000000000000000000000000000000"
  ]
legacyStakerData = encodeCall "deployWithFactory(address,bytes)"
  [ encodeAddress $ kpFactory profile
  , encodeUint256 64
  , encodeUint256 $ fromIntegral $ BS.length factoryData
  , factoryData <> BS.replicate 28 0
  ]

profile :: KernelProfile
profile = KernelProfile "kernel-sepolia-direct-v1" 421614 kernelEntryPoint (hash '1')
  (address '2') (hash '2') (address '3') (hash '3') (address '4') (hash '4') (hash '5')

owner :: Text
owner = "0x7e5f4552091a69125d5dfcb7b8c2659029395bdf"

address, hash :: Char -> Text
address digit = "0x" <> T.replicate 40 (T.singleton digit)
hash digit = "0x" <> T.replicate 64 (T.singleton digit)

replaceWord :: Int -> Integer -> ByteString -> ByteString
replaceWord offset value bytes = BS.take offset bytes <> encodeUint256 value <> BS.drop (offset + 32) bytes

fromHex :: Text -> ByteString
fromHex = either error id . Base16.decode . TE.encodeUtf8

hexText :: ByteString -> Text
hexText = ("0x" <>) . TE.decodeUtf8 . Base16.encode

attestedProfile :: KernelProfile
attestedProfile = profile
  { kpEntryPointCodeHash = hashCode 1
  , kpFactoryCodeHash = hashCode 2
  , kpImplementationCodeHash = hashCode 3
  , kpValidatorCodeHash = hashCode 4
  , kpAccountCodeHash = hashCode 5
  }
  where
    hashCode = hexText . keccak256 . BS.singleton

approvedRoot :: ByteString
approvedRoot = BS.singleton 1 <> BS.drop 12 (encodeAddress $ kpValidator profile) <> BS.replicate 11 0

accountRpc :: Maybe ByteString -> Text -> Value -> IO (Either RpcError Value)
accountRpc root = accountRpcWithOwner root "0x" owner

accountRpcWithOwner :: Maybe ByteString -> Text -> Text -> Text -> Value -> IO (Either RpcError Value)
accountRpcWithOwner = accountRpcWithImplementation (kpImplementation profile)

accountRpcWithImplementation :: Text -> Maybe ByteString -> Text -> Text -> Text -> Value -> IO (Either RpcError Value)
accountRpcWithImplementation implementation root ownerCode signer method = \case
  Array args -> (Right . String) <$> lookupResult method (foldr (:) [] args)
  _ -> expectationFailure "expected attestation RPC arguments" >> pure (Right Null)
  where
    lookupResult :: Text -> [Value] -> IO Text
    lookupResult "eth_chainId" [] = pure "0x66eee"
    lookupResult "eth_getCode" [String target, String "0x2a"]
      | target == kpEntryPoint profile = pure "0x01"
      | target == kpFactory profile = pure "0x02"
      | target == kpImplementation profile = pure "0x03"
      | target == kpValidator profile = pure "0x04"
      | target == address '6' = pure $ maybe "0x" (const "0x05") root
      | target == owner = pure ownerCode
    lookupResult "eth_getStorageAt" [String target, String slot, String "0x2a"]
      | target == address '6'
      , slot == "0x360894a13ba1a3210667c828492db98dca3e2076cc3735a920a3ca505d382bbc" =
          pure $ hexText $ encodeAddress $ kpImplementation profile
      | target == address '6'
      , slot == "0xa3f0ad74e5423aebfd80d3ef4346578335a9a72aeaee59ff6cb3582b35133d50" = pure $ hash '0'
    lookupResult "eth_call" [Object call, String "0x2a"]
      | KM.lookup "to" call == Just (String $ kpFactory profile)
      , KM.lookup "data" call == Just (String $ hexText $ selector "UUPS()") =
          pure $ hexText $ encodeAddress implementation
      | KM.lookup "to" call == Just (String $ kpFactory profile)
      , KM.lookup "data" call == Just (String $ hexText $ selector "getAddress((uint256,address,bytes,bytes)[],uint256)" <> BS.drop 4 factoryData) =
          pure $ hexText $ encodeAddress $ address '6'
      | KM.lookup "to" call == Just (String $ address '6')
      , KM.lookup "data" call == Just (String $ hexText $ selector "root()")
      , Just liveRoot <- root = pure $ hexText liveRoot
      | KM.lookup "to" call == Just (String $ kpValidator profile)
      , KM.lookup "data" call == Just (String $ hexText $ encodeCall "ecdsaValidatorStorage(address)" [encodeAddress $ address '6']) =
          pure $ hexText $ encodeAddress signer
    lookupResult unexpected _ = expectationFailure ("unexpected attestation RPC " <> T.unpack unexpected) >> pure "0x"
