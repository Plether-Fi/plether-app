module Plether.AA.KmsSpec (spec) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Plether.AA.Kms
  ( canonicalRequest
  , assembleRecoverableSignature
  , normalizeLowS
  , parseKmsDerSignature
  , parseKmsPublicKey
  )
import Plether.Ethereum.Transaction (ethereumAddressFromPublicKey, recoverSignerAddress)
import Test.Hspec

spec :: Spec
spec = do
  describe "AWS KMS secp256k1 encoding" $ do
    it "strictly parses canonical DER scalars" $ do
      parseKmsDerSignature (decode "3006020101020102") `shouldBe` Right (1, 2)

    it "rejects negative, padded, truncated, and trailing DER values" $ do
      parseKmsDerSignature (decode "3006020180020102") `shouldSatisfy` isLeft
      parseKmsDerSignature (decode "300702020001020102") `shouldSatisfy` isLeft
      parseKmsDerSignature (decode "30060201010201") `shouldSatisfy` isLeft
      parseKmsDerSignature (decode "300602010102010200") `shouldSatisfy` isLeft

    it "normalizes high-s signatures" $ do
      normalizeLowS (curveOrder - 2) `shouldBe` 2
      normalizeLowS 2 `shouldBe` 2

    it "accepts only canonical secp256k1 SubjectPublicKeyInfo" $ do
      let publicKey = decode $
            "0479be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798"
              <> "483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8"
          spki = decode "3056301006072a8648ce3d020106052b8104000a034200" <> publicKey
      parseKmsPublicKey spki `shouldBe` Right publicKey
      ethereumAddressFromPublicKey publicKey
        `shouldBe` Right "0x7e5f4552091a69125d5dfcb7b8c2659029395bdf"
      parseKmsPublicKey (BS.cons 0 spki) `shouldSatisfy` isLeft

    it "derives the Ethereum parity from a compact KMS-style signature" $ do
      let digest = decode "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"
          compact = decode $
            "a951b0cf98bd51c614c802a65a418fa42482dc5c45c9394e39c0d98773c51cd5"
              <> "30104fdc36d91582b5757e1de73d982e803cc14d75e82c65daf924e38d27d834"
      recoverSignerAddress digest compact 1
        `shouldReturn` Right "0x7e5f4552091a69125d5dfcb7b8c2659029395bdf"
      wrongParity <- recoverSignerAddress digest compact 0
      wrongParity
        `shouldNotBe` Right "0x7e5f4552091a69125d5dfcb7b8c2659029395bdf"

    it "encodes both Ethereum recovery parities as a 65-byte 27/28 signature" $ do
      let compact = BS.replicate 64 1
      fmap BS.last (assembleRecoverableSignature compact 0) `shouldBe` Right 27
      fmap BS.last (assembleRecoverableSignature compact 1) `shouldBe` Right 28
      fmap BS.length (assembleRecoverableSignature compact 1) `shouldBe` Right 65
      assembleRecoverableSignature compact 2 `shouldSatisfy` isLeft
      assembleRecoverableSignature (BS.replicate 63 1) 0 `shouldSatisfy` isLeft

  describe "SigV4 canonical request" $ do
    it "commits every security-sensitive KMS header and exact body" $ do
      canonicalRequest
        "kms.eu-central-1.amazonaws.com"
        "20260902T120000Z"
        "session-token"
        "TrentService.Sign"
        "{}"
        `shouldBe`
          "POST\n/\n\ncontent-type:application/x-amz-json-1.1\n\
          \host:kms.eu-central-1.amazonaws.com\n\
          \x-amz-date:20260902T120000Z\n\
          \x-amz-security-token:session-token\n\
          \x-amz-target:TrentService.Sign\n\n\
          \content-type;host;x-amz-date;x-amz-security-token;x-amz-target\n\
          \44136fa355b3678a1146ad16f7e8649e94fb4fc21fe77e8310c060f61caaff8a"
 where
  isLeft (Left _) = True
  isLeft _ = False

decode :: BS.ByteString -> BS.ByteString
decode value = either (const BS.empty) id $ B16.decode value

curveOrder :: Integer
curveOrder = 0xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141
