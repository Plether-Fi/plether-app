module Plether.AA.Paymaster
  ( PackedUserOperation (..)
  , SponsorshipEnvelope (..)
  , parsePackedUserOperation
  , applyPaymasterEnvelope
  , makeSponsorshipEnvelope
  , decodeSponsorshipEnvelope
  , sponsorshipDigest
  , userOperationHash
  , maximumUserOperationCost
  , paymasterAndData
  , paymasterDataHex
  , sponsorshipTypeHash
  , dummyPaymasterSignature
  , canonicalQuantity
  ) where

import Control.Monad (unless, when)
import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Plether.Config (NativeAaConfig (..))
import Plether.Ethereum.Abi (encodeAddress, encodeUint256, keccak256)
import Numeric (showHex)

entryPointAddress :: Text
entryPointAddress = "0x4337084d9e255ff0702461cf8895ce9e3b5ff108"

chainId :: Integer
chainId = 421614

data PackedUserOperation = PackedUserOperation
  { puoObject :: KM.KeyMap Value
  , puoSender :: Text
  , puoNonce :: Integer
  , puoFactory :: Maybe Text
  , puoFactoryData :: Maybe ByteString
  , puoCallData :: ByteString
  , puoCallGasLimit :: Integer
  , puoVerificationGasLimit :: Integer
  , puoPreVerificationGas :: Integer
  , puoMaxFeePerGas :: Integer
  , puoMaxPriorityFeePerGas :: Integer
  , puoPaymaster :: Maybe Text
  , puoPaymasterVerificationGasLimit :: Maybe Integer
  , puoPaymasterPostOpGasLimit :: Maybe Integer
  , puoPaymasterData :: Maybe ByteString
  , puoSignature :: ByteString
  }
  deriving stock (Eq, Show)

data SponsorshipEnvelope = SponsorshipEnvelope
  { sePaymaster :: Text
  , seVerificationGasLimit :: Integer
  , sePostOpGasLimit :: Integer
  , seValidUntil :: Integer
  , seValidAfter :: Integer
  , seMaxCost :: Integer
  , sePolicyId :: ByteString
  , seAccountCodeHash :: ByteString
  , seSignature :: ByteString
  }
  deriving stock (Eq, Show)

parsePackedUserOperation :: KM.KeyMap Value -> Either Text PackedUserOperation
parsePackedUserOperation operation = do
  sender <- requiredAddress "sender"
  nonce <- requiredQuantity "nonce"
  callData <- requiredHex "callData"
  callGasLimit <- requiredUint128 "callGasLimit"
  verificationGasLimit <- requiredUint128 "verificationGasLimit"
  preVerificationGas <- requiredQuantity "preVerificationGas"
  maxFeePerGas <- requiredUint128 "maxFeePerGas"
  maxPriorityFeePerGas <- requiredUint128 "maxPriorityFeePerGas"
  when (maxPriorityFeePerGas > maxFeePerGas) $
    Left "maxPriorityFeePerGas exceeds maxFeePerGas"
  signature <- fromMaybe BS.empty <$> optionalHex "signature"
  (factory, factoryData) <- optionalPair "factory" "factoryData" optionalAddress optionalHex
  (paymaster, paymasterVerificationGasLimit, paymasterPostOpGasLimit, paymasterData) <-
    parsePaymasterFields
  pure $
    PackedUserOperation
      operation
      sender
      nonce
      factory
      factoryData
      callData
      callGasLimit
      verificationGasLimit
      preVerificationGas
      maxFeePerGas
      maxPriorityFeePerGas
      paymaster
      paymasterVerificationGasLimit
      paymasterPostOpGasLimit
      paymasterData
      signature
 where
  key = Key.fromText
  requiredText name =
    case KM.lookup (key name) operation of
      Just (String value) -> Right value
      _ -> Left $ "UserOperation." <> name <> " must be a string"
  requiredAddress name = requiredText name >>= parseAddress name
  requiredHex name = requiredText name >>= parseHex name
  requiredQuantity name = requiredText name >>= parseQuantity name
  requiredUint128 name = do
    value <- requiredQuantity name
    if value < 2 ^ (128 :: Integer)
      then Right value
      else Left $ "UserOperation." <> name <> " exceeds uint128"
  optionalAddress name =
    case KM.lookup (key name) operation of
      Just (String value) -> Just <$> parseAddress name value
      Nothing -> Right Nothing
      _ -> Left $ "UserOperation." <> name <> " must be a string"
  optionalHex name =
    case KM.lookup (key name) operation of
      Just (String value) -> Just <$> parseHex name value
      Nothing -> Right Nothing
      _ -> Left $ "UserOperation." <> name <> " must be a string"
  optionalUint128 name =
    case KM.lookup (key name) operation of
      Nothing -> Right Nothing
      Just (String raw) -> do
        value <- parseQuantity name raw
        if value < 2 ^ (128 :: Integer)
          then Right $ Just value
          else Left $ "UserOperation." <> name <> " exceeds uint128"
      _ -> Left $ "UserOperation." <> name <> " must be a string"
  optionalPair leftName rightName parseLeft parseRight = do
    left <- parseLeft leftName
    right <- parseRight rightName
    unless (isJust left == isJust right) $
      Left $ "UserOperation." <> leftName <> " and " <> rightName <> " must be supplied together"
    pure (left, right)
  parsePaymasterFields = do
    paymaster <- optionalAddress "paymaster"
    verification <- optionalUint128 "paymasterVerificationGasLimit"
    postOp <- optionalUint128 "paymasterPostOpGasLimit"
    paymasterData <- optionalHex "paymasterData"
    unless (isJust verification == isJust postOp) $
      Left "both UserOperation paymaster gas limits must be supplied together"
    unless (isJust paymaster == isJust paymasterData) $
      Left "UserOperation paymaster and paymasterData must be supplied together"
    unless (not (isJust paymaster) || isJust verification) $
      Left "complete UserOperation paymaster fields are required when paymaster is present"
    pure (paymaster, verification, postOp, paymasterData)

makeSponsorshipEnvelope
  :: NativeAaConfig
  -> Integer
  -> Integer
  -> Integer
  -> ByteString
  -> SponsorshipEnvelope
makeSponsorshipEnvelope cfg validAfter validUntil maxCost signature =
  SponsorshipEnvelope
    { sePaymaster = naaPaymasterAddress cfg
    , seVerificationGasLimit = naaVerificationGasLimit cfg
    , sePostOpGasLimit = naaPostOpGasLimit cfg
    , seValidUntil = validUntil
    , seValidAfter = validAfter
    , seMaxCost = maxCost
    , sePolicyId = decodeFixed 32 $ naaPolicyId cfg
    , seAccountCodeHash = decodeFixed 32 $ naaAccountCodeHash cfg
    , seSignature = signature
    }

decodeSponsorshipEnvelope
  :: NativeAaConfig
  -> PackedUserOperation
  -> Either Text SponsorshipEnvelope
decodeSponsorshipEnvelope cfg operation = do
  paymaster <- maybe (Left "paymaster is required") Right $ puoPaymaster operation
  verification <- maybe (Left "paymasterVerificationGasLimit is required") Right $
    puoPaymasterVerificationGasLimit operation
  postOp <- maybe (Left "paymasterPostOpGasLimit is required") Right $
    puoPaymasterPostOpGasLimit operation
  bytes <- maybe (Left "paymasterData is required") Right $ puoPaymasterData operation
  unless (T.toLower paymaster == T.toLower (naaPaymasterAddress cfg)) $
    Left "UserOperation uses an unapproved paymaster"
  unless (verification == naaVerificationGasLimit cfg && postOp == naaPostOpGasLimit cfg) $
    Left "UserOperation paymaster gas limits do not match policy"
  unless (BS.length bytes == 157) $
    Left "Plether paymasterData must be exactly 157 bytes"
  let validUntil = bytesToInteger $ BS.take 6 bytes
      validAfter = bytesToInteger $ BS.take 6 $ BS.drop 6 bytes
      maxCost = bytesToInteger $ BS.take 16 $ BS.drop 12 bytes
      policyId = BS.take 32 $ BS.drop 28 bytes
      accountCodeHash = BS.take 32 $ BS.drop 60 bytes
      signature = BS.drop 92 bytes
  unless (validUntil > validAfter && validUntil > 0) $
    Left "Plether paymaster validity window is invalid"
  unless (validUntil - validAfter <= 600) $
    Left "Plether paymaster validity window exceeds policy"
  unless (maxCost > 0 && maxCost <= naaMaxCostWei cfg) $
    Left "Plether paymaster maximum cost exceeds policy"
  unless (policyId == decodeFixed 32 (naaPolicyId cfg)) $
    Left "Plether paymaster policy id does not match"
  unless (accountCodeHash == decodeFixed 32 (naaAccountCodeHash cfg)) $
    Left "Plether paymaster account code hash does not match"
  pure $
    SponsorshipEnvelope
      paymaster verification postOp validUntil validAfter maxCost policyId accountCodeHash signature

applyPaymasterEnvelope
  :: PackedUserOperation
  -> SponsorshipEnvelope
  -> PackedUserOperation
applyPaymasterEnvelope operation envelope =
  operation
    { puoObject = object'
    , puoPaymaster = Just $ T.toLower $ sePaymaster envelope
    , puoPaymasterVerificationGasLimit = Just $ seVerificationGasLimit envelope
    , puoPaymasterPostOpGasLimit = Just $ sePostOpGasLimit envelope
    , puoPaymasterData = Just $ envelopeBytes envelope
    }
 where
  object' =
    KM.insert "paymasterData" (String $ paymasterDataHex envelope) $
      KM.insert "paymasterPostOpGasLimit" (String $ canonicalQuantity $ sePostOpGasLimit envelope) $
        KM.insert "paymasterVerificationGasLimit" (String $ canonicalQuantity $ seVerificationGasLimit envelope) $
          KM.insert "paymaster" (String $ T.toLower $ sePaymaster envelope) (puoObject operation)

maximumUserOperationCost :: PackedUserOperation -> SponsorshipEnvelope -> Integer
maximumUserOperationCost operation envelope =
  ( puoCallGasLimit operation
      + puoVerificationGasLimit operation
      + puoPreVerificationGas operation
      + seVerificationGasLimit envelope
      + sePostOpGasLimit envelope
  )
    * puoMaxFeePerGas operation

sponsorshipDigest :: PackedUserOperation -> SponsorshipEnvelope -> ByteString
sponsorshipDigest operation envelope =
  keccak256 $ BS.pack [0x19, 0x01] <> domainSeparator envelope <> structHash
 where
  structHash =
    keccak256 $
      sponsorshipTypeHash
        <> encodeAddress (puoSender operation)
        <> encodeUint256 (puoNonce operation)
        <> keccak256 (initCode operation)
        <> keccak256 (puoCallData operation)
        <> accountGasLimits operation
        <> encodeUint256 (puoPreVerificationGas operation)
        <> gasFees operation
        <> encodeUint256 (seVerificationGasLimit envelope)
        <> encodeUint256 (sePostOpGasLimit envelope)
        <> encodeUint256 (seValidUntil envelope)
        <> encodeUint256 (seValidAfter envelope)
        <> encodeUint256 (seMaxCost envelope)
        <> sePolicyId envelope
        <> seAccountCodeHash envelope
        <> encodeAddress entryPointAddress

userOperationHash :: PackedUserOperation -> ByteString
userOperationHash operation =
  keccak256 $
    BS.pack [0x19, 0x01]
      <> entryPointDomainSeparator
      <> packedHash
 where
  packedHash =
    keccak256 $
      packedUserOperationTypeHash
        <> encodeAddress (puoSender operation)
        <> encodeUint256 (puoNonce operation)
        <> keccak256 (initCode operation)
        <> keccak256 (puoCallData operation)
        <> accountGasLimits operation
        <> encodeUint256 (puoPreVerificationGas operation)
        <> gasFees operation
        <> keccak256 (paymasterAndData operation)

-- EntryPoint v0.8 hashes PackedUserOperation as EIP-712 data.  This domain is
-- deliberately separate from the verifying-paymaster sponsorship domain.
entryPointDomainSeparator :: ByteString
entryPointDomainSeparator =
  keccak256 $
    eip712DomainTypeHash
      <> keccak256 "ERC4337"
      <> keccak256 "1"
      <> encodeUint256 chainId
      <> encodeAddress entryPointAddress

packedUserOperationTypeHash :: ByteString
packedUserOperationTypeHash =
  keccak256
    "PackedUserOperation(address sender,uint256 nonce,bytes initCode,bytes callData,bytes32 accountGasLimits,uint256 preVerificationGas,bytes32 gasFees,bytes paymasterAndData)"

paymasterDataHex :: SponsorshipEnvelope -> Text
paymasterDataHex = encodeHex . envelopeBytes

dummyPaymasterSignature :: ByteString
dummyPaymasterSignature = BS.replicate 65 0

canonicalQuantity :: Integer -> Text
canonicalQuantity value = "0x" <> T.pack (showHex value "")

domainSeparator :: SponsorshipEnvelope -> ByteString
domainSeparator envelope =
  keccak256 $
    eip712DomainTypeHash
      <> keccak256 "PletherVerifyingPaymaster"
      <> keccak256 "1"
      <> encodeUint256 chainId
      <> encodeAddress (sePaymaster envelope)

accountGasLimits :: PackedUserOperation -> ByteString
accountGasLimits operation =
  integerToFixed 16 (puoVerificationGasLimit operation)
    <> integerToFixed 16 (puoCallGasLimit operation)

gasFees :: PackedUserOperation -> ByteString
gasFees operation =
  integerToFixed 16 (puoMaxPriorityFeePerGas operation)
    <> integerToFixed 16 (puoMaxFeePerGas operation)

initCode :: PackedUserOperation -> ByteString
initCode operation =
  case (puoFactory operation, puoFactoryData operation) of
    (Just factory, Just factoryData) -> decodeFixed 20 factory <> factoryData
    _ -> BS.empty

paymasterAndData :: PackedUserOperation -> ByteString
paymasterAndData operation =
  case
    ( puoPaymaster operation
    , puoPaymasterVerificationGasLimit operation
    , puoPaymasterPostOpGasLimit operation
    , puoPaymasterData operation
    )
  of
    (Just paymaster, Just verification, Just postOp, Just bytes) ->
      decodeFixed 20 paymaster
        <> integerToFixed 16 verification
        <> integerToFixed 16 postOp
        <> bytes
    _ -> BS.empty

envelopeBytes :: SponsorshipEnvelope -> ByteString
envelopeBytes envelope =
  integerToFixed 6 (seValidUntil envelope)
    <> integerToFixed 6 (seValidAfter envelope)
    <> integerToFixed 16 (seMaxCost envelope)
    <> sePolicyId envelope
    <> seAccountCodeHash envelope
    <> seSignature envelope

eip712DomainTypeHash :: ByteString
eip712DomainTypeHash =
  keccak256 "EIP712Domain(string name,string version,uint256 chainId,address verifyingContract)"

sponsorshipTypeHash :: ByteString
sponsorshipTypeHash =
  decodeFixed 32 "0x5835c142c681b663470a1a53c34b0ba256a8283b7b9f9560aadb85711d252918"

parseAddress :: Text -> Text -> Either Text Text
parseAddress name raw =
  let value = T.toLower $ T.strip raw
   in if isFixedHex 20 value
        then Right value
        else Left $ "UserOperation." <> name <> " must be an address"

parseHex :: Text -> Text -> Either Text ByteString
parseHex name raw =
  let value = T.toLower $ T.strip raw
   in if T.isPrefixOf "0x" value && even (T.length $ T.drop 2 value)
        then case B16.decode $ TE.encodeUtf8 $ T.drop 2 value of
          Right bytes -> Right bytes
          Left _ -> invalid
        else invalid
 where
  invalid = Left $ "UserOperation." <> name <> " must be canonical hex bytes"

parseQuantity :: Text -> Text -> Either Text Integer
parseQuantity name raw =
  let value = T.toLower $ T.strip raw
      digits = T.drop 2 value
   in if
        T.isPrefixOf "0x" value
          && not (T.null digits)
          && T.length digits <= 64
          && T.all isHexChar digits
          && (T.length digits == 1 || T.head digits /= '0')
        then Right $ T.foldl' (\total digit -> total * 16 + hexDigit digit) 0 digits
        else Left $ "UserOperation." <> name <> " must be a canonical quantity"

isFixedHex :: Int -> Text -> Bool
isFixedHex bytes value =
  T.isPrefixOf "0x" value
    && T.length (T.drop 2 value) == bytes * 2
    && T.all isHexChar (T.drop 2 value)

decodeFixed :: Int -> Text -> ByteString
decodeFixed size value =
  case B16.decode $ TE.encodeUtf8 $ T.drop 2 $ T.toLower value of
    Right bytes | BS.length bytes == size -> bytes
    _ -> BS.replicate size 0

encodeHex :: ByteString -> Text
encodeHex bytes = "0x" <> TE.decodeUtf8 (B16.encode bytes)

integerToFixed :: Int -> Integer -> ByteString
integerToFixed width value =
  BS.replicate (width - BS.length raw) 0 <> raw
 where
  raw
    | value == 0 = BS.empty
    | otherwise = BS.reverse $ BS.unfoldr step value
  step 0 = Nothing
  step remaining = Just (fromIntegral $ remaining `mod` 256, remaining `div` 256)

bytesToInteger :: ByteString -> Integer
bytesToInteger = BS.foldl' (\total byte -> total * 256 + fromIntegral byte) 0

isHexChar :: Char -> Bool
isHexChar char =
  (char >= '0' && char <= '9')
    || (char >= 'a' && char <= 'f')
    || (char >= 'A' && char <= 'F')

hexDigit :: Char -> Integer
hexDigit char
  | char >= '0' && char <= '9' = fromIntegral $ fromEnum char - fromEnum '0'
  | char >= 'a' && char <= 'f' = fromIntegral $ fromEnum char - fromEnum 'a' + 10
  | otherwise = fromIntegral $ fromEnum char - fromEnum 'A' + 10
