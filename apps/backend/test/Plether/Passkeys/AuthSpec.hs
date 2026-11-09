module Plether.Passkeys.AuthSpec (spec, signed, claims, header) where

import Crypto.Hash (SHA256 (..))
import Crypto.Number.Serialize (i2ospOf_)
import Crypto.PubKey.ECC.ECDSA (PrivateKey, PublicKey (..), Signature (..), sign)
import Crypto.PubKey.ECC.Generate (generate)
import Crypto.PubKey.ECC.Types (CurveName (SEC_p256r1), Point (..), getCurveByName)
import Data.Aeson (Value (..), encode, object, (.=))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as B64
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Plether.Passkeys.Auth (AuthFailure (..), parseKeys, verifyToken)
import Test.Hspec

claims :: Value
claims = object ["iss" .= ("privy.io" :: Text), "aud" .= ("public-app" :: Text),
  "sub" .= ("did:privy:user-a" :: Text), "iat" .= (900 :: Int), "exp" .= (1100 :: Int)]

header :: Value
header = object ["alg" .= ("ES256" :: Text), "kid" .= ("test-key" :: Text)]

signed :: PrivateKey -> Value -> Value -> IO BS.ByteString
signed private h c = do
  let content = B64.encodeUnpadded (LBS.toStrict $ encode h) <> "." <> B64.encodeUnpadded (LBS.toStrict $ encode c)
  Signature r s <- sign private SHA256 content
  pure $ content <> "." <> B64.encodeUnpadded (i2ospOf_ 32 r <> i2ospOf_ 32 s)

spec :: Spec
spec = describe "Privy passkey-name authentication" $ do
  it "verifies a real ES256 signature and its app/issuer/expiry claims" $ do
    (public, private) <- generate $ getCurveByName SEC_p256r1
    token <- signed private header claims
    verifyToken [("test-key", public)] "public-app" 1000 token `shouldBe` Right "did:privy:user-a"
    verifyToken [("test-key", public)] "other-app" 1000 token `shouldBe` Left InvalidToken
    verifyToken [("test-key", public)] "public-app" 1100 token `shouldBe` Left InvalidToken

  it "rejects modified content, unknown keys, unsigned algorithms, critical extensions and malformed tokens" $ do
    (public, private) <- generate $ getCurveByName SEC_p256r1
    (other, _) <- generate $ getCurveByName SEC_p256r1
    token <- signed private header claims
    verifyToken [("test-key", other)] "public-app" 1000 token `shouldBe` Left InvalidToken
    verifyToken [] "public-app" 1000 token `shouldBe` Left InvalidToken
    let verifyBad value = signed private value claims >>= \t -> verifyToken [("test-key", public)] "public-app" 1000 t `shouldBe` Left InvalidToken
    verifyBad $ object ["alg" .= ("none" :: Text), "kid" .= ("test-key" :: Text)]
    verifyBad $ object ["alg" .= ("ES256" :: Text), "kid" .= ("unknown" :: Text)]
    verifyBad $ object ["alg" .= ("ES256" :: Text), "kid" .= ("test-key" :: Text), "crit" .= (["b64"] :: [Text])]
    mapM_ (\bad -> verifyToken [("test-key", public)] "public-app" 1000 bad `shouldBe` Left InvalidToken)
      ["", "a.b.c", token <> ".extra", BS.replicate 8193 65]
    let parts = BS.split 46 token
    modified <- signed private header $ object ["sub" .= ("did:privy:other" :: Text)]
    verifyToken [("test-key", public)] "public-app" 1000 (parts !! 0 <> "." <> BS.split 46 modified !! 1 <> "." <> parts !! 2) `shouldBe` Left InvalidToken

  it "rejects wrong issuer, future issuance/not-before, missing claims and invalid subjects" $ do
    (public, private) <- generate $ getCurveByName SEC_p256r1
    let Object original = claims
        invalid = [ KM.insert "iss" (String "other") original, KM.insert "iat" (Number 2000) original,
          KM.insert "nbf" (Number 2000) original, KM.delete "exp" original,
          KM.insert "sub" (String "not-a-privy-user") original ]
    mapM_ (\c -> signed private header (Object c) >>= \token -> verifyToken [("test-key", public)] "public-app" 1000 token `shouldBe` Left InvalidToken) invalid

  it "loads a P-256 JWKS and rejects invalid points or algorithms" $ do
    (public@(PublicKey _ point), private) <- generate $ getCurveByName SEC_p256r1
    let Point x y = point
        jwk = object ["kty" .= ("EC" :: Text), "crv" .= ("P-256" :: Text), "alg" .= ("ES256" :: Text),
          "use" .= ("sig" :: Text), "kid" .= ("test-key" :: Text),
          "x" .= TE.decodeUtf8 (B64.encodeUnpadded $ i2ospOf_ 32 x), "y" .= TE.decodeUtf8 (B64.encodeUnpadded $ i2ospOf_ 32 y)]
    let Right parsed = parseKeys $ LBS.toStrict $ encode $ object ["keys" .= [jwk]]
    token <- signed private header claims
    verifyToken parsed "public-app" 1000 token `shouldBe` verifyToken [("test-key", public)] "public-app" 1000 token
    parseKeys "{\"keys\":[]}" `shouldSatisfy` either (const True) (const False)
    let Object fields = jwk
    mapM_ (\key -> parseKeys (LBS.toStrict $ encode $ object ["keys" .= [Object key]]) `shouldSatisfy` either (const True) (const False))
      [KM.insert "alg" (String "HS256") fields, KM.insert "x" (String "invalid") fields,
       KM.insert "x" (String $ TE.decodeUtf8 $ B64.encodeUnpadded $ BS.replicate 32 0) $ KM.insert "y" (String $ TE.decodeUtf8 $ B64.encodeUnpadded $ BS.replicate 32 0) fields]
