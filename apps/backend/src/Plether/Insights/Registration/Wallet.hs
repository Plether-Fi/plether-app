module Plether.Insights.Registration.Wallet
  ( walletChallengeLifetimeSeconds
  , renderWalletChallenge
  , recoverPersonalSignAddress
  ) where

import Control.Exception (bracket)
import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Word (Word8)
import Foreign
  ( Ptr
  , alloca
  , allocaBytes
  , castPtr
  , peek
  , poke
  )
import Foreign.C (CInt (..), CSize (..), CUInt (..))
import Plether.Ethereum.Abi (keccak256)
import Plether.Utils.Address (isValidAddress)

data SecpCtx
data SecpPubKey
data SecpRecoverableSig

walletChallengeLifetimeSeconds :: Integer
walletChallengeLifetimeSeconds = 300

renderWalletChallenge
  :: Text
  -> Text
  -> Integer
  -> Text
  -> Text
  -> Integer
  -> Integer
  -> Either Text Text
renderWalletChallenge canonicalOrigin competitionSlug chainId ownerAddress nonce issuedAt expiresAt
  | not (isCanonicalAddress normalizedOwner) || normalizedOwner == zeroAddress = Left "Wallet address is invalid"
  | T.length nonce < 8 || not (T.all isAsciiAlphaNumeric nonce) = Left "Wallet challenge nonce is invalid"
  | expiresAt <= issuedAt = Left "Wallet challenge expiry is invalid"
  | otherwise =
      Right $
        domain
          <> " wants you to sign in with your Ethereum account:\n"
          <> normalizedOwner
          <> "\n\nRegister for Plether competition "
          <> competitionSlug
          <> ". This request will not trigger a blockchain transaction.\n\nURI: "
          <> canonicalOrigin
          <> "/competitions/"
          <> competitionSlug
          <> "/register\nVersion: 1\nChain ID: "
          <> T.pack (show chainId)
          <> "\nNonce: "
          <> nonce
          <> "\nIssued At: "
          <> renderTimestamp issuedAt
          <> "\nExpiration Time: "
          <> renderTimestamp expiresAt
  where
    normalizedOwner = T.toLower $ T.strip ownerAddress
    domain = maybe canonicalOrigin id $ T.stripPrefix "https://" canonicalOrigin

renderTimestamp :: Integer -> Text
renderTimestamp =
  T.pack
    . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"
    . posixSecondsToUTCTime
    . fromInteger

recoverPersonalSignAddress :: Text -> Text -> IO (Either Text Text)
recoverPersonalSignAddress message signatureText =
  case decodeSignature signatureText of
    Left err -> pure $ Left err
    Right (compactSignature, recoveryId) ->
      recoverAddress (personalMessageDigest message) compactSignature recoveryId

personalMessageDigest :: Text -> ByteString
personalMessageDigest message =
  let messageBytes = TE.encodeUtf8 message
      prefix =
        BS.cons 0x19 "Ethereum Signed Message:\n"
          <> TE.encodeUtf8 (T.pack $ show $ BS.length messageBytes)
   in keccak256 $ prefix <> messageBytes

decodeSignature :: Text -> Either Text (ByteString, Int)
decodeSignature raw = do
  if raw /= T.strip raw || raw /= T.toLower raw || T.take 2 raw /= "0x"
    then Left "Wallet signature must be canonical hex"
    else pure ()
  let normalized = T.drop 2 raw
  bytes <- case B16.decode $ TE.encodeUtf8 normalized of
    Left _ -> Left "Wallet signature must be canonical hex"
    Right decoded -> Right decoded
  if BS.length bytes /= 65
    then Left "Wallet signature must contain exactly 65 bytes"
    else pure ()
  let compact = BS.take 64 bytes
      r = bytesToInteger $ BS.take 32 compact
      s = bytesToInteger $ BS.drop 32 compact
      v = BS.index bytes 64
  if r <= 0 || r >= secp256k1Order || s <= 0 || s > secp256k1HalfOrder
    then Left "Wallet signature is not canonical"
    else case v of
      0 -> Right (compact, 0)
      1 -> Right (compact, 1)
      27 -> Right (compact, 0)
      28 -> Right (compact, 1)
      _ -> Left "Wallet signature recovery id is invalid"

recoverAddress :: ByteString -> ByteString -> Int -> IO (Either Text Text)
recoverAddress digest compactSignature recoveryId
  | BS.length digest /= 32 || BS.length compactSignature /= 64 =
      pure $ Left "Wallet signature input is invalid"
  | recoveryId < 0 || recoveryId > 1 = pure $ Left "Wallet signature recovery id is invalid"
  | otherwise =
      withSecpContext $ \context ->
        allocaBytes 65 $ \recoverableSignaturePtr ->
          BS.useAsCString compactSignature $ \compactPtr -> do
            parsed <-
              c_secp256k1_ecdsa_recoverable_signature_parse_compact
                context
                recoverableSignaturePtr
                (castPtr compactPtr)
                (fromIntegral recoveryId)
            if not $ isSuccess parsed
              then pure $ Left "Wallet signature is invalid"
              else
                allocaBytes 64 $ \publicKeyPtr ->
                  BS.useAsCString digest $ \digestPtr -> do
                    recovered <-
                      c_secp256k1_ecdsa_recover
                        context
                        publicKeyPtr
                        recoverableSignaturePtr
                        (castPtr digestPtr)
                    if not $ isSuccess recovered
                      then pure $ Left "Wallet signature is invalid"
                      else serializeAddress context publicKeyPtr

serializeAddress :: Ptr SecpCtx -> Ptr SecpPubKey -> IO (Either Text Text)
serializeAddress context publicKeyPtr =
  alloca $ \lengthPtr ->
    allocaBytes 65 $ \outputPtr -> do
      poke lengthPtr 65
      serialized <-
        c_secp256k1_ec_pubkey_serialize
          context
          (castPtr outputPtr)
          lengthPtr
          publicKeyPtr
          secp256k1EcUncompressed
      if not $ isSuccess serialized
        then pure $ Left "Wallet public key could not be serialized"
        else do
          outputLength <- peek lengthPtr
          publicKey <- BS.packCStringLen (outputPtr, fromIntegral outputLength)
          let publicKeyBody =
                if BS.length publicKey == 65 && BS.head publicKey == 0x04
                  then BS.drop 1 publicKey
                  else publicKey
              digest = keccak256 publicKeyBody
              addressBytes = BS.drop (BS.length digest - 20) digest
          pure $ Right $ "0x" <> TE.decodeUtf8 (B16.encode addressBytes)

withSecpContext :: (Ptr SecpCtx -> IO a) -> IO a
withSecpContext =
  bracket
    (c_secp256k1_context_create secp256k1ContextVerify)
    c_secp256k1_context_destroy

isSuccess :: CInt -> Bool
isSuccess 1 = True
isSuccess _ = False

bytesToInteger :: ByteString -> Integer
bytesToInteger = BS.foldl' (\acc byte -> acc * 256 + fromIntegral byte) 0

secp256k1Order :: Integer
secp256k1Order = 0xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141

secp256k1HalfOrder :: Integer
secp256k1HalfOrder = secp256k1Order `div` 2

secp256k1ContextVerify :: CUInt
secp256k1ContextVerify = 0x0101

secp256k1EcUncompressed :: CUInt
secp256k1EcUncompressed = 0x0002

foreign import ccall safe "secp256k1.h secp256k1_context_create"
  c_secp256k1_context_create :: CUInt -> IO (Ptr SecpCtx)

foreign import ccall safe "secp256k1.h secp256k1_context_destroy"
  c_secp256k1_context_destroy :: Ptr SecpCtx -> IO ()

foreign import ccall safe "secp256k1_recovery.h secp256k1_ecdsa_recoverable_signature_parse_compact"
  c_secp256k1_ecdsa_recoverable_signature_parse_compact ::
    Ptr SecpCtx ->
    Ptr SecpRecoverableSig ->
    Ptr Word8 ->
    CInt ->
    IO CInt

foreign import ccall safe "secp256k1_recovery.h secp256k1_ecdsa_recover"
  c_secp256k1_ecdsa_recover ::
    Ptr SecpCtx ->
    Ptr SecpPubKey ->
    Ptr SecpRecoverableSig ->
    Ptr Word8 ->
    IO CInt

foreign import ccall safe "secp256k1.h secp256k1_ec_pubkey_serialize"
  c_secp256k1_ec_pubkey_serialize ::
    Ptr SecpCtx ->
    Ptr Word8 ->
    Ptr CSize ->
    Ptr SecpPubKey ->
    CUInt ->
    IO CInt

zeroAddress :: Text
zeroAddress = "0x0000000000000000000000000000000000000000"

isCanonicalAddress :: Text -> Bool
isCanonicalAddress address =
  T.length address == 42
    && T.take 2 address == "0x"
    && T.all isLowerHex (T.drop 2 address)
    && isValidAddress address
  where
    isLowerHex character = character `elem` ['0' .. '9'] || character `elem` ['a' .. 'f']

isAsciiAlphaNumeric :: Char -> Bool
isAsciiAlphaNumeric character =
  character `elem` ['0' .. '9']
    || character `elem` ['a' .. 'z']
    || character `elem` ['A' .. 'Z']
