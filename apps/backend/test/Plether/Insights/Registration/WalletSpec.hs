module Plether.Insights.Registration.WalletSpec (spec) where

import Control.Monad (replicateM_)
import qualified Data.Text as T
import Plether.Insights.Registration.Crypto (generateHexToken)
import Plether.Insights.Registration.Wallet
import Test.Hspec

spec :: Spec
spec = do
  describe "renderWalletChallenge" $ do
    it "renders the exact viem-compatible September registration message" $ do
      renderWalletChallenge
        canonicalOrigin
        competitionSlug
        chainId
        owner
        nonce
        issuedAt
        expiresAt
        `shouldBe` Right expectedMessage
      walletChallengeLifetimeSeconds `shouldBe` 300

    it "normalizes a checksummed owner address to canonical lowercase" $ do
      let mixedCaseOwner = "0x7E5F4552091A69125D5DFCB7B8C2659029395BDF"
      renderWalletChallenge canonicalOrigin competitionSlug chainId mixedCaseOwner nonce issuedAt expiresAt
        `shouldBe` Right expectedMessage

    it "rejects zero or malformed owners, unsafe nonces, and non-forward expiries" $ do
      renderWalletChallenge canonicalOrigin competitionSlug chainId zeroOwner nonce issuedAt expiresAt
        `shouldBe` Left "Wallet address is invalid"
      renderWalletChallenge canonicalOrigin competitionSlug chainId "0x1234" nonce issuedAt expiresAt
        `shouldBe` Left "Wallet address is invalid"
      renderWalletChallenge canonicalOrigin competitionSlug chainId owner "short" issuedAt expiresAt
        `shouldBe` Left "Wallet challenge nonce is invalid"
      renderWalletChallenge canonicalOrigin competitionSlug chainId owner "invalid-nonce" issuedAt expiresAt
        `shouldBe` Left "Wallet challenge nonce is invalid"
      renderWalletChallenge canonicalOrigin competitionSlug chainId owner nonce issuedAt issuedAt
        `shouldBe` Left "Wallet challenge expiry is invalid"

    it "always accepts freshly generated canonical hex nonces" $
      replicateM_ 64 $ do
        generatedNonce <- generateHexToken 16
        T.length generatedNonce `shouldBe` 32
        generatedNonce `shouldSatisfy` T.all isLowerHex
        renderWalletChallenge canonicalOrigin competitionSlug chainId owner generatedNonce issuedAt expiresAt
          `shouldBe` Right (T.replace nonce generatedNonce expectedMessage)

  describe "recoverPersonalSignAddress" $ do
    it "recovers the supplied viem personal_sign vector to the expected owner" $ do
      recovered <- recoverPersonalSignAddress expectedMessage validSignature
      recovered `shouldBe` Right owner

    it "accepts the normalized v=0 representation of the same low-s signature" $ do
      recovered <- recoverPersonalSignAddress expectedMessage validSignatureV0
      recovered `shouldBe` Right owner

    it "does not recover the expected owner when the signed message domain changes" $ do
      recovered <- recoverPersonalSignAddress (T.replace canonicalOrigin "https://evil.example" expectedMessage) validSignature
      recovered `shouldNotBe` Right owner

    it "rejects the malleable high-s counterpart" $ do
      recovered <- recoverPersonalSignAddress expectedMessage highSSignature
      recovered `shouldBe` Left "Wallet signature is not canonical"

    it "rejects zero-r, zero-s, and invalid recovery-id variants" $ do
      recoverPersonalSignAddress expectedMessage zeroRSignature
        `shouldReturn` Left "Wallet signature is not canonical"
      recoverPersonalSignAddress expectedMessage zeroSSignature
        `shouldReturn` Left "Wallet signature is not canonical"
      recoverPersonalSignAddress expectedMessage invalidVSignature
        `shouldReturn` Left "Wallet signature recovery id is invalid"

    it "rejects non-hex and non-65-byte signatures before recovery" $ do
      recoverPersonalSignAddress expectedMessage "0xnot-hex"
        `shouldReturn` Left "Wallet signature must be canonical hex"
      recoverPersonalSignAddress expectedMessage "0x00"
        `shouldReturn` Left "Wallet signature must contain exactly 65 bytes"
      recoverPersonalSignAddress expectedMessage (T.drop 2 validSignature)
        `shouldReturn` Left "Wallet signature must be canonical hex"
      recoverPersonalSignAddress expectedMessage (T.toUpper validSignature)
        `shouldReturn` Left "Wallet signature must be canonical hex"
      recoverPersonalSignAddress expectedMessage (" " <> validSignature)
        `shouldReturn` Left "Wallet signature must be canonical hex"

canonicalOrigin :: T.Text
canonicalOrigin = "https://insights.plether.com"

competitionSlug :: T.Text
competitionSlug = "testnet-trading-2026-09"

chainId :: Integer
chainId = 421_614

owner :: T.Text
owner = "0x7e5f4552091a69125d5dfcb7b8c2659029395bdf"

zeroOwner :: T.Text
zeroOwner = "0x0000000000000000000000000000000000000000"

nonce :: T.Text
nonce = "0123456789abcdef"

issuedAt :: Integer
issuedAt = 1_788_220_800

expiresAt :: Integer
expiresAt = 1_788_221_100

expectedMessage :: T.Text
expectedMessage =
  "insights.plether.com wants you to sign in with your Ethereum account:\n"
    <> owner
    <> "\n\nRegister for Plether competition testnet-trading-2026-09. This request will not trigger a blockchain transaction.\n\n"
    <> "URI: https://insights.plether.com/competitions/testnet-trading-2026-09/register\n"
    <> "Version: 1\n"
    <> "Chain ID: 421614\n"
    <> "Nonce: 0123456789abcdef\n"
    <> "Issued At: 2026-09-01T00:00:00Z\n"
    <> "Expiration Time: 2026-09-01T00:05:00Z"

validSignature :: T.Text
validSignature =
  "0x0353d3cf6c06d07e9ab5e9540322bc484b04ac890415f2cf89e4f13ef41a97896e5ec787786075a76dc4f920b543d4c67b091d2bcf04b1ce9e73a21a3c1954b51b"

validSignatureV0 :: T.Text
validSignatureV0 = T.dropEnd 2 validSignature <> "00"

invalidVSignature :: T.Text
invalidVSignature = T.dropEnd 2 validSignature <> "02"

highSSignature :: T.Text
highSSignature =
  "0x0353d3cf6c06d07e9ab5e9540322bc484b04ac890415f2cf89e4f13ef41a978991a13878879f8a58923b06df4abc2b383fa5bfbae043ee6d215ebc72941cec8c1c"

zeroRSignature :: T.Text
zeroRSignature =
  "0x00000000000000000000000000000000000000000000000000000000000000006e5ec787786075a76dc4f920b543d4c67b091d2bcf04b1ce9e73a21a3c1954b51b"

zeroSSignature :: T.Text
zeroSSignature =
  "0x0353d3cf6c06d07e9ab5e9540322bc484b04ac890415f2cf89e4f13ef41a978900000000000000000000000000000000000000000000000000000000000000001b"

isLowerHex :: Char -> Bool
isLowerHex character = character `elem` ['0' .. '9'] || character `elem` ['a' .. 'f']
