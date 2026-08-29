module Plether.Insights.Registration.Crypto
  ( EncryptedValue (..)
  , encryptValue
  , decryptValue
  , normalizeEmail
  , emailLookupDigest
  , secretDigest
  , generateOpaqueToken
  , generateHexToken
  , generateUuidV4
  , uuidV4FromDigest
  , generatePkcePair
  , base64UrlNoPad
  , constantTimeEqual
  , registrationFieldAad
  ) where

import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types
  ( AEAD
  , AEADMode (AEAD_GCM)
  , AuthTag (..)
  , aeadInit
  , aeadSimpleDecrypt
  , aeadSimpleEncrypt
  , cipherInit
  )
import Crypto.Error (CryptoFailable (..))
import Crypto.Hash (Digest, SHA256, hash)
import Crypto.MAC.HMAC (HMAC, hmac)
import Crypto.Random (getRandomBytes)
import Data.Bits ((.&.), (.|.))
import Data.ByteArray (constEq, convert)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base64 as B64
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

-- | The key version is authenticated both as GCM AAD and as an explicit row
-- column.  Callers add competition/application/field identity to 'aad' so an
-- encrypted value cannot be transplanted into another row or field.
data EncryptedValue = EncryptedValue
  { evKeyVersion :: Text
  , evNonce :: ByteString
  , evCiphertext :: ByteString
  , evTag :: ByteString
  }
  deriving stock (Eq)

instance Show EncryptedValue where
  show value =
    "EncryptedValue {evKeyVersion = "
      <> show (evKeyVersion value)
      <> ", evNonce = <redacted>, evCiphertext = <redacted>, evTag = <redacted>}"

encryptValue
  :: Text
  -> ByteString
  -> ByteString
  -> ByteString
  -> IO (Either Text EncryptedValue)
encryptValue keyVersion key aad plaintext = do
  nonce <- getRandomBytes 12
  pure $ do
    cipher <- initCipher key
    aead <- initAead cipher nonce
    let authenticatedData = envelopeAad keyVersion aad
        (tag, ciphertext) = aeadSimpleEncrypt aead authenticatedData plaintext 16
    Right
      EncryptedValue
        { evKeyVersion = keyVersion
        , evNonce = nonce
        , evCiphertext = ciphertext
        , evTag = convert tag
        }

decryptValue :: ByteString -> ByteString -> EncryptedValue -> Either Text ByteString
decryptValue key aad value = do
  cipher <- initCipher key
  aead <- initAead cipher $ evNonce value
  if BS.length (evTag value) /= 16
    then Left "Encrypted value authentication tag is invalid"
    else
      maybe
        (Left "Encrypted value authentication failed")
        Right
        ( aeadSimpleDecrypt
            aead
            (envelopeAad (evKeyVersion value) aad)
            (evCiphertext value)
            (AuthTag $ convert $ evTag value)
        )

initCipher :: ByteString -> Either Text AES256
initCipher key =
  case cipherInit key of
    CryptoPassed cipher -> Right cipher
    CryptoFailed _ -> Left "Encryption key must be exactly 32 bytes"

initAead :: AES256 -> ByteString -> Either Text (AEAD AES256)
initAead cipher nonce =
  case aeadInit AEAD_GCM cipher nonce of
    CryptoPassed aead -> Right aead
    CryptoFailed _ -> Left "Encrypted value nonce is invalid"

envelopeAad :: Text -> ByteString -> ByteString
envelopeAad keyVersion aad =
  "plether-registration-envelope-v1\0"
    <> TE.encodeUtf8 keyVersion
    <> "\0"
    <> aad

normalizeEmail :: Text -> Text
normalizeEmail = T.toCaseFold . T.strip

emailLookupDigest :: ByteString -> Text -> ByteString
emailLookupDigest key email =
  keyedDigest key $ "email-v1\0" <> TE.encodeUtf8 (normalizeEmail email)

secretDigest :: ByteString -> Text -> ByteString -> ByteString
secretDigest key domain value =
  keyedDigest key $ "secret-v1\0" <> TE.encodeUtf8 domain <> "\0" <> value

keyedDigest :: ByteString -> ByteString -> ByteString
keyedDigest key message = convert (hmac key message :: HMAC SHA256)

generateOpaqueToken :: Int -> IO Text
generateOpaqueToken byteCount = base64UrlNoPad <$> getRandomBytes byteCount

-- | Fixed-entropy, ASCII-alphanumeric challenge material suitable for the
-- EIP-4361 nonce grammar.  Unlike base64url it can never contain '-' or '_'.
generateHexToken :: Int -> IO Text
generateHexToken byteCount = TE.decodeUtf8 . B16.encode <$> getRandomBytes byteCount

generateUuidV4 :: IO Text
generateUuidV4 = do
  randomBytes <- getRandomBytes 16
  maybe (fail "Could not construct registration UUID") pure $ uuidV4FromDigest randomBytes

-- | Turn a secret, collision-resistant digest into an RFC 4122 v4-shaped UUID.
-- This lets Turnstile retries reuse the same idempotency key without storing or
-- exposing the provider token. The source must remain a keyed digest rather
-- than attacker-controlled bytes.
uuidV4FromDigest :: ByteString -> Maybe Text
uuidV4FromDigest digest
  | BS.length digest < 16 = Nothing
  | otherwise =
      let source = BS.take 16 digest
          versioned =
            BS.take 6 source
              <> BS.singleton ((BS.index source 6 .&. 0x0f) .|. 0x40)
              <> BS.take 1 (BS.drop 7 source)
              <> BS.singleton ((BS.index source 8 .&. 0x3f) .|. 0x80)
              <> BS.drop 9 source
          hex = TE.decodeUtf8 $ B16.encode versioned
       in Just $
            T.intercalate
              "-"
              [ T.take 8 hex
              , T.take 4 $ T.drop 8 hex
              , T.take 4 $ T.drop 12 hex
              , T.take 4 $ T.drop 16 hex
              , T.drop 20 hex
              ]

generatePkcePair :: IO (Text, Text)
generatePkcePair = do
  verifier <- generateOpaqueToken 32
  let challenge = base64UrlNoPad $ convert (hash (TE.encodeUtf8 verifier) :: Digest SHA256)
  pure (verifier, challenge)

base64UrlNoPad :: ByteString -> Text
base64UrlNoPad =
  TE.decodeUtf8
    . BS.takeWhile (/= 61)
    . BS.map translate
    . B64.encode
  where
    translate 43 = 45 -- + -> -
    translate 47 = 95 -- / -> _
    translate byte = byte

constantTimeEqual :: ByteString -> ByteString -> Bool
constantTimeEqual left right = BS.length left == BS.length right && constEq left right

registrationFieldAad :: Text -> Text -> Text -> ByteString
registrationFieldAad competitionSlug applicationId fieldName =
  "competition\0"
    <> TE.encodeUtf8 competitionSlug
    <> "\0application\0"
    <> TE.encodeUtf8 applicationId
    <> "\0field\0"
    <> TE.encodeUtf8 fieldName
