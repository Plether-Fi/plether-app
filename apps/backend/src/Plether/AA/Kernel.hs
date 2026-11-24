module Plether.AA.Kernel
  ( KernelProfile (..)
  , KernelGasMode (..)
  , KernelOperationPhase (..)
  , KernelOperation (..)
  , kernelEntryPoint
  , parseKernelOperation
  , parseKernelCallIntent
  , decodeKernelCalls
  , kernelFactoryArgs
  , decodeKernelFactoryOwner
  , decodeKernelEntryPointDeposit
  , kernelUserOperationHash
  , attestKernelStackAtBlock
  , verifyKernelAccountAtBlock
  , verifyKernelAccountWithRpcAtBlock
  , resolveOwnedKernelAccountAtBlock
  , resolveOwnedKernelAccountWithRpcAtBlock
  , verifyKernelSignature
  ) where

import Control.Concurrent.Async (concurrently, mapConcurrently)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object, withObject, (.:), (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import Data.Maybe (isJust)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Plether.AA.Paymaster as Paymaster
import Plether.AA.Pimlico (OwnedTradingAccountFailure (..), ProxyFailure, SmartCall (..), invalidParams, policyDenied, unavailable)
import Plether.Ethereum.Abi (decodeAddress, decodeUint256, encodeAddress, encodeCall, encodeUint256, keccak256, selector)
import Plether.Ethereum.Client (EthClient, RpcError, rpcCall)
import Plether.Ethereum.Transaction (recoverSignerAddress)

-- First deployment profile. Additional factories, validation modes, or chains
-- require a versioned profile and distinct reviewed encodings.
data KernelProfile = KernelProfile
  { kpDeployment :: Text
  , kpChainId :: Integer
  , kpEntryPoint :: Text
  , kpEntryPointCodeHash :: Text
  , kpFactory :: Text
  , kpFactoryCodeHash :: Text
  , kpImplementation :: Text
  , kpImplementationCodeHash :: Text
  , kpValidator :: Text
  , kpValidatorCodeHash :: Text
  , kpAccountCodeHash :: Text
  }
  deriving stock (Eq, Show)

kernelEntryPoint :: Text
kernelEntryPoint = "0x433709009b8330fda32311df1c2afa402ed8d009"

instance FromJSON KernelProfile where
  parseJSON = withObject "Kernel profile" $ \fields -> do
    let names =
          [ "deployment", "chainId", "entryPoint", "entryPointCodeHash"
          , "factory", "factoryCodeHash", "implementation", "implementationCodeHash"
          , "validator", "validatorCodeHash", "accountCodeHash"
          , "entryPointVersion", "accountVersion", "accountIndex"
          ]
    unless (Set.fromList (KM.keys fields) == Set.fromList names) $
      fail "Kernel profile keys do not match the supported schema"
    deployment <- fields .: "deployment"
    unless (not (T.null deployment) && T.length deployment <= 96 && T.all deploymentCharacter deployment) $
      fail "Kernel deployment id is invalid"
    chain <- fields .: "chainId"
    index <- fields .: "accountIndex"
    entryPointVersion <- fields .: "entryPointVersion"
    accountVersion <- fields .: "accountVersion"
    unless (chain == 421614 && index == (0 :: Integer)
        && entryPointVersion == ("0.9" :: Text) && accountVersion == ("kernel-v4-uups" :: Text)) $
      fail "Kernel profile must use Arbitrum Sepolia, EntryPoint 0.9 and Kernel v4 UUPS index 0"
    let pinnedAddress label = do
          raw <- fields .: label
          maybe (fail $ "Invalid Kernel address: " <> Key.toString label) pure (normalizeAddress raw)
        pinnedHash label = do
          raw <- fields .: label
          case hexBytes raw of
            Just value
              | BS.length value == 32
              , not (BS.all (== 0) value)
              , value /= keccak256 BS.empty -> pure $ T.toLower raw
            _ -> fail $ "Invalid Kernel code hash: " <> Key.toString label
    entryPoint <- pinnedAddress "entryPoint"
    unless (entryPoint == kernelEntryPoint) $ fail "Kernel profile has an unexpected EntryPoint address"
    KernelProfile deployment chain entryPoint
      <$> pinnedHash "entryPointCodeHash"
      <*> pinnedAddress "factory"
      <*> pinnedHash "factoryCodeHash"
      <*> pinnedAddress "implementation"
      <*> pinnedHash "implementationCodeHash"
      <*> pinnedAddress "validator"
      <*> pinnedHash "validatorCodeHash"
      <*> pinnedHash "accountCodeHash"
    where
      deploymentCharacter c = (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c `elem` ("._-" :: String)

instance ToJSON KernelProfile where
  toJSON profile =
    object
      [ "deployment" .= kpDeployment profile
      , "chainId" .= kpChainId profile
      , "entryPoint" .= kpEntryPoint profile
      , "entryPointCodeHash" .= kpEntryPointCodeHash profile
      , "factory" .= kpFactory profile
      , "factoryCodeHash" .= kpFactoryCodeHash profile
      , "implementation" .= kpImplementation profile
      , "implementationCodeHash" .= kpImplementationCodeHash profile
      , "validator" .= kpValidator profile
      , "validatorCodeHash" .= kpValidatorCodeHash profile
      , "accountCodeHash" .= kpAccountCodeHash profile
      , "entryPointVersion" .= ("0.9" :: Text)
      , "accountVersion" .= ("kernel-v4-uups" :: Text)
      , "accountIndex" .= (0 :: Integer)
      ]

data KernelGasMode = KernelSponsored | KernelEthPaid
  deriving stock (Eq, Show)

data KernelOperationPhase = KernelEstimate | KernelSubmit
  deriving stock (Eq, Show)

data KernelOperation = KernelOperation
  { koPacked :: Paymaster.PackedUserOperation
  , koCalls :: [SmartCall]
  , koFactoryOwner :: Maybe Text
  }
  deriving stock (Eq, Show)

parseKernelOperation
  :: KernelProfile -> KernelGasMode -> KernelOperationPhase -> KM.KeyMap Value
  -> Either ProxyFailure KernelOperation
parseKernelOperation profile mode phase fields = do
  unless (all (`Set.member` allowedOperationFields) (KM.keys fields)) $
    Left $ invalidParams "Kernel UserOperation contains unsupported fields"
  packed <- either (Left . invalidParams) Right $ Paymaster.parsePackedUserOperation fields
  unless (Paymaster.puoNonce packed < 2 ^ (64 :: Int)) $
    Left $ invalidParams "Kernel accepts only the root validator nonce key zero"
  when (phase == KernelSubmit && not (validOwnerSignature $ Paymaster.puoSignature packed)) $
    Left $ invalidParams "Kernel submission requires a canonical 65-byte ECDSA owner signature"
  validatePaymaster
  (calls, owner) <- parseKernelCallIntent profile (Paymaster.puoSender packed)
    (Paymaster.puoCallData packed) (Paymaster.puoFactory packed) (hexText <$> Paymaster.puoFactoryData packed)
  pure $ KernelOperation packed calls owner
  where
    hasPaymasterFields = any (`KM.member` fields) paymasterFields
    validatePaymaster
      | mode == KernelEthPaid =
          when hasPaymasterFields $ Left $ invalidParams "An ETH-paid operation cannot contain paymaster fields"
      | phase == KernelEstimate && not hasPaymasterFields = Right ()
      | otherwise = do
          unless (all (`KM.member` fields) paymasterFields) $
            Left $ invalidParams "A sponsored operation requires the complete paymaster envelope"
          envelope <- case KM.lookup "paymasterData" fields of
            Just (String value) | Just bytes <- hexBytes value -> Right bytes
            _ -> Left $ invalidParams "The paymaster data is malformed"
          unless (BS.length envelope == 157
              && not (paymasterSignatureMagic `BS.isSuffixOf` envelope)
              && hexText (BS.take 32 $ BS.drop 60 envelope) == kpAccountCodeHash profile) $
            Left $ invalidParams "The paymaster data does not match the reviewed Kernel envelope"
          case KM.lookup "paymaster" fields of
            Just (String value) | isJust (normalizeAddress value) -> Right ()
            _ -> Left $ invalidParams "The paymaster address is invalid"

parseKernelCallIntent
  :: KernelProfile -> Text -> ByteString -> Maybe Text -> Maybe Text
  -> Either ProxyFailure ([SmartCall], Maybe Text)
parseKernelCallIntent profile sender callData factory factoryData = do
  unless (isJust $ normalizeAddress sender) $ Left $ invalidParams "Kernel sender is invalid"
  calls <- decodeKernelCalls callData
  owner <- case (factory, factoryData) of
    (Nothing, Nothing) -> Right Nothing
    (Just address, Just arguments)
      | normalizeAddress address == Just (kpFactory profile) ->
          Just <$> decodeKernelFactoryOwner profile arguments
    _ -> Left $ invalidParams "Kernel factory fields do not match the deployment profile"
  pure (calls, owner)

allowedOperationFields :: Set.Set Key.Key
allowedOperationFields = Set.fromList $
  [ "sender", "nonce", "factory", "factoryData", "callData", "callGasLimit"
  , "verificationGasLimit", "preVerificationGas", "maxFeePerGas", "maxPriorityFeePerGas", "signature"
  ] <> paymasterFields

paymasterFields :: [Key.Key]
paymasterFields = ["paymaster", "paymasterVerificationGasLimit", "paymasterPostOpGasLimit", "paymasterData"]

-- On 0.9 this optional suffix changes the bytes covered by the owner signature.
-- Plether's v1 envelope signs the complete paymasterAndData.
paymasterSignatureMagic :: ByteString
paymasterSignatureMagic = BS.pack [0x22, 0xe3, 0x25, 0xa2, 0x97, 0x43, 0x96, 0x56]

kernelUserOperationHash :: KernelProfile -> Paymaster.PackedUserOperation -> Either ProxyFailure ByteString
kernelUserOperationHash profile packed = do
  when (paymasterSignatureMagic `BS.isSuffixOf` Paymaster.paymasterAndData packed) $
    Left $ invalidParams "The optional paymaster signature suffix is unsupported"
  _ <- parseKernelCallIntent profile (Paymaster.puoSender packed) (Paymaster.puoCallData packed)
    (Paymaster.puoFactory packed) (hexText <$> Paymaster.puoFactoryData packed)
  pure $ Paymaster.userOperationHashFor (kpChainId profile) (kpEntryPoint profile) packed

-- execute(bytes32,bytes) in default single (0x00) or batch (0x01) mode.
-- Decode the outer ABI and each inner call exactly before the shared Perps
-- policy examines selectors, recipients, and action semantics.
decodeKernelCalls :: ByteString -> Either ProxyFailure [SmartCall]
decodeKernelCalls bytes = do
  unless (BS.take 4 bytes == selector "execute(bytes32,bytes)") $
    Left $ policyDenied "Kernel action must use the reviewed execute method"
  let args = BS.drop 4 bytes
  mode <- wordAt args 0
  checkedWord args 32 64
  (payload, end) <- dynamicBytesAt args 64
  unless (end == BS.length args) $ Left malformedCall
  case BS.uncons mode of
    Just (0, rest) | BS.all (== 0) rest -> (: []) <$> decodeSingle payload
    Just (1, rest) | BS.all (== 0) rest -> decodeBatch payload
    _ -> Left $ policyDenied "Kernel execution mode is not approved"
  where
    decodeSingle payload = do
      unless (BS.length payload >= 52) $ Left malformedCall
      let target = hexText $ BS.take 20 payload
          value = decodeUint256 $ BS.take 32 $ BS.drop 20 payload
      pure $ SmartCall target value (BS.drop 52 payload)

    decodeBatch payload = do
      checkedWord payload 0 32
      count <- wordInteger payload 32
      unless (count `elem` [1, 2, 5]) $ Left $ policyDenied "Kernel batch call count is not approved"
      let elementCount = fromIntegral count
          entriesStart = 64
          dataStart = entriesStart + 32 * elementCount
      (calls, end) <- foldEntries payload entriesStart elementCount dataStart 0 []
      unless (end == BS.length payload) $ Left malformedCall
      pure $ reverse calls

    foldEntries payload entriesStart count tupleStart index accum
      | index == count = Right (accum, tupleStart)
      | otherwise = do
          checkedWord payload (entriesStart + 32 * index) (fromIntegral $ tupleStart - entriesStart)
          targetWord <- wordAt payload tupleStart
          unless (BS.all (== 0) $ BS.take 12 targetWord) $ Left malformedCall
          value <- wordInteger payload (tupleStart + 32)
          checkedWord payload (tupleStart + 64) 96
          (dataBytes, end) <- dynamicBytesAt payload (tupleStart + 96)
          foldEntries payload entriesStart count end (index + 1) (SmartCall (decodeAddress targetWord) value dataBytes : accum)

factoryType :: Text
factoryType = "((uint256,address,bytes,bytes)[],uint256)"

-- Deploy one index-zero Kernel directly with a type-1 ECDSA root owner package.
kernelFactoryArgs :: KernelProfile -> Text -> Either ProxyFailure Text
kernelFactoryArgs profile ownerAddress = do
  owner <- maybe (Left $ invalidParams "Kernel root owner is invalid") Right $ normalizeAddress ownerAddress
  _ <- maybe (Left $ invalidParams "Kernel validator is invalid") Right $ normalizeAddress (kpValidator profile)
  pure $ hexText $ encodeCall ("deploy" <> factoryType)
    [ encodeUint256 64
    , encodeUint256 0
    , encodeUint256 1
    , encodeUint256 32
    , encodeUint256 1
    , encodeAddress (kpValidator profile)
    , encodeUint256 128
    , encodeUint256 192
    , encodeUint256 20
    , BS.drop 12 (encodeAddress owner) <> BS.replicate 12 0
    , encodeUint256 0
    ]

decodeKernelFactoryOwner :: KernelProfile -> Text -> Either ProxyFailure Text
decodeKernelFactoryOwner profile raw = do
  bytes <- maybe (Left malformedFactory) Right $ hexBytes raw
  unless (BS.length bytes == 356 && BS.take 4 bytes == selector ("deploy" <> factoryType)) $
    Left malformedFactory
  let owner = hexText $ BS.take 20 $ BS.drop 292 bytes
  approved <- kernelFactoryArgs profile owner
  unless (hexText bytes == approved) $ Left malformedFactory
  pure owner
  where
    malformedFactory = invalidParams "Kernel factory initializer does not match the reviewed owner package"

type KernelRpc = Text -> Value -> IO (Either RpcError Value)

attestKernelStackAtBlock :: KernelProfile -> EthClient -> Integer -> IO (Either ProxyFailure ())
attestKernelStackAtBlock profile client = attestKernelStack profile (rpcCall client)

attestKernelStack :: KernelProfile -> KernelRpc -> Integer -> IO (Either ProxyFailure ())
attestKernelStack profile rpc block = runExceptT $ do
  when (block < 0) $ throwE $ invalidParams "Kernel attestation requires an explicit block number"
  let addresses =
        [ (kpEntryPoint profile, kpEntryPointCodeHash profile)
        , (kpFactory profile, kpFactoryCodeHash profile)
        , (kpImplementation profile, kpImplementationCodeHash profile)
        , (kpValidator profile, kpValidatorCodeHash profile)
        ]
  (chain, (code, implementation)) <- liftIO $ concurrently
    (rpc "eth_chainId" $ toJSON ([] :: [Value]))
    (concurrently (mapConcurrently (readCode rpc block . fst) addresses)
      (readWordCall rpc block (kpFactory profile) (selector "UUPS()")))
  unless (chain == Right (String $ Paymaster.canonicalQuantity $ kpChainId profile)) $
    throwE $ unavailable "KERNEL_ATTESTATION_FAILED" "Kernel RPC chain does not match the deployment profile"
  case traverse id code of
    Left failure -> throwE failure
    Right bytecodes ->
      unless (and $ zipWith pinnedCode bytecodes (map snd addresses)) $
        throwE $ unavailable "KERNEL_ATTESTATION_FAILED" "Kernel contract code does not match the deployment profile"
  factoryImplementation <- ExceptT $ pure implementation
  unless (factoryImplementation == encodeAddress (kpImplementation profile)) $
    throwE $ unavailable "KERNEL_ATTESTATION_FAILED" "Kernel factory implementation does not match the deployment profile"

-- Just owner is the counterfactual path and requires an undeployed account.
-- Nothing resolves the live root owner from the deployed account. Both paths
-- verify that the approved factory derives this address for that owner.
verifyKernelAccountAtBlock
  :: KernelProfile -> EthClient -> Integer -> Text -> Maybe Text -> IO (Either ProxyFailure Text)
verifyKernelAccountAtBlock profile client = verifyKernelAccountWithRpcAtBlock profile (rpcCall client)

verifyKernelAccountWithRpcAtBlock
  :: KernelProfile -> KernelRpc -> Integer -> Text -> Maybe Text -> IO (Either ProxyFailure Text)
verifyKernelAccountWithRpcAtBlock profile rpc block rawSender factoryOwner = runExceptT $ do
  when (block < 0) $ throwE $ invalidParams "Kernel attestation requires an explicit block number"
  sender <- maybe (throwE $ invalidParams "Kernel sender is invalid") pure $ normalizeAddress rawSender
  (stack, code) <- liftIO $ concurrently (attestKernelStack profile rpc block) (readCode rpc block sender)
  ExceptT $ pure stack
  bytecode <- ExceptT $ pure code
  owner <- case factoryOwner of
    Just rawOwner -> do
      unless (BS.null bytecode) $ throwE $ policyDenied "Kernel factory data may be used only for an undeployed account"
      maybe (throwE $ invalidParams "Kernel root owner is invalid") pure $ normalizeAddress rawOwner
    Nothing -> verifyLiveKernel profile rpc block sender bytecode
  ownerCode <- ExceptT $ readCode rpc block owner
  unless (BS.null ownerCode) $
    throwE $ policyDenied "The Kernel root owner must be a wallet without deployed code"
  derived <- deriveKernelAddress profile rpc block owner
  unless (derived == sender) $
    throwE $ policyDenied "The Kernel factory does not derive this sender for the verified owner"
  pure owner

-- Funding and registration start with the proven owner. A counterfactual
-- account only needs the pinned derivation. A deployed account must still
-- have that owner, the approved implementation and the approved validator.
resolveOwnedKernelAccountAtBlock
  :: KernelProfile -> EthClient -> Integer -> Text -> IO (Either OwnedTradingAccountFailure Text)
resolveOwnedKernelAccountAtBlock profile client = resolveOwnedKernelAccountWithRpcAtBlock profile (rpcCall client)

resolveOwnedKernelAccountWithRpcAtBlock
  :: KernelProfile -> KernelRpc -> Integer -> Text -> IO (Either OwnedTradingAccountFailure Text)
resolveOwnedKernelAccountWithRpcAtBlock profile rpc block rawOwner
  | block < 0 = pure $ Left OwnedTradingAccountProofUnavailable
  | otherwise = case normalizeAddress rawOwner of
    Nothing -> pure $ Left OwnedTradingAccountProofUnavailable
    Just owner -> do
      ownerCode <- readCode rpc block owner
      case ownerCode of
        Left _ -> pure $ Left OwnedTradingAccountProofUnavailable
        Right code | not (BS.null code) -> pure $ Left OwnerWalletIsContract
        Right _ -> do
          result <- runExceptT $ do
            ExceptT $ attestKernelStack profile rpc block
            sender <- deriveKernelAddress profile rpc block owner
            when (sender == owner) $ throwE $ policyDenied "The Kernel account must differ from its owner"
            deployed <- ExceptT $ readCode rpc block sender
            unless (BS.null deployed) $ do
              liveOwner <- verifyLiveKernel profile rpc block sender deployed
              unless (liveOwner == owner) $ throwE $ policyDenied "The Kernel account no longer belongs to this owner"
            pure sender
          pure $ either (const $ Left OwnedTradingAccountProofUnavailable) Right result

deriveKernelAddress :: KernelProfile -> KernelRpc -> Integer -> Text -> ExceptT ProxyFailure IO Text
deriveKernelAddress profile rpc block owner = do
  factoryArguments <- ExceptT $ pure $ kernelFactoryArgs profile owner
  lookupArguments <- case hexBytes factoryArguments of
    Just bytes -> pure (selector ("getAddress" <> factoryType) <> BS.drop 4 bytes)
    Nothing -> throwE $ invalidParams "Kernel factory arguments are invalid"
  derived <- ExceptT $ readWordCall rpc block (kpFactory profile) lookupArguments
  readAddressWord "Kernel factory address response is invalid" derived

verifyLiveKernel :: KernelProfile -> KernelRpc -> Integer -> Text -> ByteString -> ExceptT ProxyFailure IO Text
verifyLiveKernel profile rpc block sender bytecode = do
  unless (pinnedCode bytecode $ kpAccountCodeHash profile) $
    throwE $ policyDenied "Kernel account runtime does not match the deployment profile"
  ((implementation, beacon), (root, owner)) <- liftIO $ concurrently
    (concurrently (readSlot rpc block sender implementationSlot) (readSlot rpc block sender beaconSlot))
    (concurrently (readWordCall rpc block sender $ selector "root()")
      (readWordCall rpc block (kpValidator profile) $ encodeCall "ecdsaValidatorStorage(address)" [encodeAddress sender]))
  implementationWord <- ExceptT $ pure implementation
  beaconWord <- ExceptT $ pure beacon
  rootWord <- ExceptT $ pure root
  ownerWord <- ExceptT $ pure owner
  let expectedRoot = BS.singleton 1 <> BS.drop 12 (encodeAddress $ kpValidator profile) <> BS.replicate 11 0
  unless (implementationWord == encodeAddress (kpImplementation profile) && BS.all (== 0) beaconWord
      && rootWord == expectedRoot) $
    throwE $ policyDenied "Kernel account implementation or root validator does not match the deployment profile"
  readAddressWord "Kernel root owner response is invalid" ownerWord

readAddressWord :: Text -> ByteString -> ExceptT ProxyFailure IO Text
readAddressWord failure word = do
  unless (BS.all (== 0) $ BS.take 12 word) $ throwE $ policyDenied failure
  maybe (throwE $ policyDenied failure) pure $ normalizeAddress $ decodeAddress word

verifyKernelSignature
  :: KernelProfile -> Text -> Paymaster.PackedUserOperation -> IO (Either ProxyFailure ())
verifyKernelSignature profile rawOwner operation = runExceptT $ do
  owner <- maybe (throwE $ invalidParams "Kernel root owner is invalid") pure $ normalizeAddress rawOwner
  let signature = Paymaster.puoSignature operation
  unless (validOwnerSignature signature) $ throwE $ invalidParams "Kernel ECDSA owner signature is malformed"
  digest <- ExceptT $ pure $ kernelUserOperationHash profile operation
  let compact = BS.take 64 signature
      parity = fromIntegral (BS.last signature) - 27
      personalDigest = keccak256 $ "\x19\&Ethereum Signed Message:\n32" <> digest
  plain <- liftIO $ recoverSignerAddress digest compact parity
  unless (plain == Right owner) $ do
    personal <- liftIO $ recoverSignerAddress personalDigest compact parity
    unless (personal == Right owner) $ throwE $ policyDenied "Kernel operation signature does not match the verified owner"

validOwnerSignature :: ByteString -> Bool
validOwnerSignature bytes =
  BS.length bytes == 65 && BS.last bytes `elem` [27, 28]
    && r > 0 && r < secp256k1Order && s > 0 && s <= secp256k1Order `div` 2
  where
    r = decodeUint256 $ BS.take 32 bytes
    s = decodeUint256 $ BS.take 32 $ BS.drop 32 bytes
    secp256k1Order = 0xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141

readWordCall :: KernelRpc -> Integer -> Text -> ByteString -> IO (Either ProxyFailure ByteString)
readWordCall rpc block address calldata = do
  result <- rpc "eth_call" $ toJSON
    [object ["to" .= address, "data" .= hexText calldata], String $ Paymaster.canonicalQuantity block]
  pure $ case result of
    Right (String raw) | Just bytes <- hexBytes raw, BS.length bytes == 32 -> Right bytes
    _ -> Left $ unavailable "KERNEL_ATTESTATION_FAILED" "Kernel contract read failed"

decodeKernelEntryPointDeposit :: ByteString -> Either Text Integer
decodeKernelEntryPointDeposit bytes
  | BS.length bytes == 32 = Right $ decodeUint256 bytes
  | otherwise = Left "Kernel EntryPoint deposit response is noncanonical"

readCode :: KernelRpc -> Integer -> Text -> IO (Either ProxyFailure ByteString)
readCode rpc block address = do
  result <- rpc "eth_getCode" $ toJSON [address, Paymaster.canonicalQuantity block]
  pure $ case result of
    Right (String raw) | Just bytes <- hexBytes raw -> Right bytes
    _ -> Left $ unavailable "KERNEL_ATTESTATION_FAILED" "Kernel runtime code read failed"

readSlot :: KernelRpc -> Integer -> Text -> Text -> IO (Either ProxyFailure ByteString)
readSlot rpc block address slot = do
  result <- rpc "eth_getStorageAt" $ toJSON [address, slot, Paymaster.canonicalQuantity block]
  pure $ case result of
    Right (String raw) | Just bytes <- hexBytes raw, BS.length bytes == 32 -> Right bytes
    _ -> Left $ unavailable "KERNEL_ATTESTATION_FAILED" "Kernel account storage read failed"

implementationSlot, beaconSlot :: Text
implementationSlot = "0x360894a13ba1a3210667c828492db98dca3e2076cc3735a920a3ca505d382bbc"
beaconSlot = "0xa3f0ad74e5423aebfd80d3ef4346578335a9a72aeaee59ff6cb3582b35133d50"

pinnedCode :: ByteString -> Text -> Bool
pinnedCode bytecode expected = not (BS.null bytecode) && hexText (keccak256 bytecode) == expected

malformedCall :: ProxyFailure
malformedCall = invalidParams "Kernel execution calldata is not canonically encoded"

wordAt :: ByteString -> Int -> Either ProxyFailure ByteString
wordAt bytes offset
  | offset < 0 || offset > BS.length bytes - 32 = Left malformedCall
  | otherwise = Right $ BS.take 32 $ BS.drop offset bytes

wordInteger :: ByteString -> Int -> Either ProxyFailure Integer
wordInteger bytes offset = decodeUint256 <$> wordAt bytes offset

checkedWord :: ByteString -> Int -> Integer -> Either ProxyFailure ()
checkedWord bytes offset expected = do
  actual <- wordInteger bytes offset
  unless (actual == expected) $ Left malformedCall

dynamicBytesAt :: ByteString -> Int -> Either ProxyFailure (ByteString, Int)
dynamicBytesAt bytes offset = do
  lengthWord <- wordInteger bytes offset
  let remaining = BS.length bytes - offset - 32
      paddedWord = ((lengthWord + 31) `div` 32) * 32
  unless (paddedWord <= fromIntegral remaining) $ Left malformedCall
  let byteLength = fromIntegral lengthWord
      paddedLength = fromIntegral paddedWord
      payload = BS.take paddedLength $ BS.drop (offset + 32) bytes
  unless (BS.all (== 0) $ BS.drop byteLength payload) $ Left malformedCall
  pure (BS.take byteLength payload, offset + 32 + paddedLength)

normalizeAddress :: Text -> Maybe Text
normalizeAddress raw = do
  bytes <- hexBytes raw
  if BS.length bytes == 20 && not (BS.all (== 0) bytes)
    then Just (hexText bytes)
    else Nothing

hexBytes :: Text -> Maybe ByteString
hexBytes raw =
  T.stripPrefix "0x" raw >>= either (const Nothing) Just . Base16.decode . TE.encodeUtf8

hexText :: ByteString -> Text
hexText = ("0x" <>) . TE.decodeUtf8 . Base16.encode
