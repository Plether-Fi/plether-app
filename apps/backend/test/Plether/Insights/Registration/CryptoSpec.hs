module Plether.Insights.Registration.CryptoSpec (spec) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Plether.Insights.Registration.Crypto
import Test.Hspec

spec :: Spec
spec = do
  describe "AES-256-GCM registration envelopes" $ do
    it "round-trips with a fresh 96-bit nonce and 128-bit tag" $ do
      encrypted <- encryptValue "v1" keyV1 rowAad plaintext
      case encrypted of
        Left err -> expectationFailure $ T.unpack err
        Right value -> do
          evKeyVersion value `shouldBe` "v1"
          BS.length (evNonce value) `shouldBe` 12
          BS.length (evTag value) `shouldBe` 16
          evCiphertext value `shouldNotBe` plaintext
          decryptValue keyV1 rowAad value `shouldBe` Right plaintext

    it "authenticates row AAD, key version, ciphertext, and tag" $ do
      Right value <- encryptValue "v1" keyV1 rowAad plaintext
      decryptValue keyV1 "competition/other/application/field/email" value
        `shouldSatisfy` isLeft
      decryptValue keyV1 rowAad value {evKeyVersion = "v2"}
        `shouldSatisfy` isLeft
      decryptValue keyV1 rowAad value {evCiphertext = tamper $ evCiphertext value}
        `shouldSatisfy` isLeft
      decryptValue keyV1 rowAad value {evTag = tamper $ evTag value}
        `shouldSatisfy` isLeft

    it "supports explicit key rotation without allowing cross-version decryption" $ do
      Right oldValue <- encryptValue "v1" keyV1 rowAad plaintext
      Right newValue <- encryptValue "v2" keyV2 rowAad plaintext
      decryptValue keyV1 rowAad oldValue `shouldBe` Right plaintext
      decryptValue keyV2 rowAad newValue `shouldBe` Right plaintext
      decryptValue keyV2 rowAad oldValue `shouldSatisfy` isLeft
      decryptValue keyV1 rowAad newValue `shouldSatisfy` isLeft

    it "rejects non-256-bit keys and malformed authentication tags" $ do
      invalid <- encryptValue "v1" (BS.replicate 31 0) rowAad plaintext
      invalid `shouldBe` Left "Encryption key must be exactly 32 bytes"
      Right value <- encryptValue "v1" keyV1 rowAad plaintext
      decryptValue keyV1 rowAad value {evTag = BS.take 15 $ evTag value}
        `shouldBe` Left "Encrypted value authentication tag is invalid"

    it "redacts all encrypted bytes from Show output" $ do
      Right value <- encryptValue "v1" keyV1 rowAad plaintext
      let rendered = show value
      rendered `shouldContain` "evKeyVersion = \"v1\""
      rendered `shouldContain` "evCiphertext = <redacted>"
      rendered `shouldNotContain` T.unpack (TE.decodeUtf8 plaintext)

  describe "registration digests" $ do
    it "normalizes email case and surrounding whitespace before HMAC lookup" $ do
      normalizeEmail "  Alice@Example.COM  " `shouldBe` "alice@example.com"
      emailLookupDigest digestKey "  Alice@Example.COM  "
        `shouldBe` emailLookupDigest digestKey "alice@example.com"

    it "matches stable versioned HMAC-SHA256 vectors" $ do
      hex (emailLookupDigest digestKey "Alice@Example.COM")
        `shouldBe` "73f11d3b67cc3e2629614a1b5873dbd8fb9edc1ae094dd3b488de351683f6f3e"
      hex (secretDigest digestKey "session" "opaque-token")
        `shouldBe` "ed9108c6ef93feb0fb7231edd7243f602328beddf3cd9215e51abe10d82b0a7e"

    it "domain-separates email and opaque-secret digests" $ do
      emailLookupDigest digestKey "opaque-token"
        `shouldNotBe` secretDigest digestKey "session" "opaque-token"
      secretDigest digestKey "session" "opaque-token"
        `shouldNotBe` secretDigest digestKey "csrf" "opaque-token"
      BS.length (emailLookupDigest digestKey "alice@example.com") `shouldBe` 32

    it "compares same-length digests without accepting prefixes" $ do
      constantTimeEqual "same" "same" `shouldBe` True
      constantTimeEqual "same" "different" `shouldBe` False
      constantTimeEqual "same" "same-suffix" `shouldBe` False

  describe "opaque registration identifiers" $ do
    it "generates 256-bit URL-safe session tokens without padding" $ do
      token <- generateOpaqueToken 32
      T.length token `shouldBe` 43
      token `shouldSatisfy` T.all isBase64UrlCharacter
      token `shouldNotSatisfy` T.isInfixOf "="

    it "generates RFC 4122 version-4 UUID-shaped references" $ do
      uuid <- generateUuidV4
      T.length uuid `shouldBe` 36
      map (T.index uuid) [8, 13, 18, 23] `shouldBe` "----"
      T.index uuid 14 `shouldBe` '4'
      T.index uuid 19 `shouldSatisfy` (`elem` ("89ab" :: String))
      uuid `shouldSatisfy` T.all (\character -> character == '-' || isLowerHex character)

    it "derives a stable secret Turnstile retry UUID from a keyed digest" $ do
      let firstDigest = secretDigest digestKey "turnstile-idempotency" "provider-token-a"
          secondDigest = secretDigest digestKey "turnstile-idempotency" "provider-token-b"
          firstUuid = uuidV4FromDigest firstDigest
      firstUuid `shouldBe` uuidV4FromDigest firstDigest
      firstUuid `shouldNotBe` uuidV4FromDigest secondDigest
      uuidV4FromDigest (BS.replicate 15 0) `shouldBe` Nothing
      case firstUuid of
        Nothing -> expectationFailure "expected UUID from SHA-256 digest"
        Just uuid -> do
          T.length uuid `shouldBe` 36
          T.index uuid 14 `shouldBe` '4'
          T.index uuid 19 `shouldSatisfy` (`elem` ("89ab" :: String))

    it "generates a URL-safe 256-bit PKCE verifier and S256 challenge" $ do
      (verifier, challenge) <- generatePkcePair
      T.length verifier `shouldBe` 43
      T.length challenge `shouldBe` 43
      verifier `shouldSatisfy` T.all isBase64UrlCharacter
      challenge `shouldSatisfy` T.all isBase64UrlCharacter
      verifier `shouldNotBe` challenge

    it "encodes base64url without padding" $ do
      base64UrlNoPad "f" `shouldBe` "Zg"
      base64UrlNoPad (BS.pack [0xfb, 0xef, 0xff]) `shouldBe` "--__"

keyV1 :: BS.ByteString
keyV1 = BS.replicate 32 0x11

keyV2 :: BS.ByteString
keyV2 = BS.replicate 32 0x22

digestKey :: BS.ByteString
digestKey = BS.pack [0 .. 31]

rowAad :: BS.ByteString
rowAad = "competition/testnet-trading-2026-09/application/registration-1/field/email"

plaintext :: BS.ByteString
plaintext = "alice@example.com"

hex :: BS.ByteString -> T.Text
hex = TE.decodeUtf8 . B16.encode

tamper :: BS.ByteString -> BS.ByteString
tamper bytes
  | BS.null bytes = "x"
  | otherwise = BS.cons (BS.head bytes + 1) (BS.tail bytes)

isBase64UrlCharacter :: Char -> Bool
isBase64UrlCharacter character =
  character == '-'
    || character == '_'
    || character `elem` ['0' .. '9']
    || character `elem` ['a' .. 'z']
    || character `elem` ['A' .. 'Z']

isLowerHex :: Char -> Bool
isLowerHex character = character `elem` ['0' .. '9'] || character `elem` ['a' .. 'f']

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False
