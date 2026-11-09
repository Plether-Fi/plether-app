module Plether.Passkeys.Auth
  ( AuthFailure (..), VerifierCache, newVerifierCache, verifyWithCache, verifyToken, parseKeys ) where

import Control.Concurrent.MVar (MVar, modifyMVar, newMVar)
import Control.Exception (try)
import Control.Monad (guard)
import Crypto.Hash (SHA256 (..))
import Crypto.Number.Serialize (os2ip)
import Crypto.PubKey.ECC.ECDSA (PublicKey (..), Signature (..), verify)
import Crypto.PubKey.ECC.Prim (isPointValid)
import Crypto.PubKey.ECC.Types (CurveName (SEC_p256r1), Point (..), getCurveByName)
import Data.Aeson (Value, eitherDecodeStrict', withObject, (.:), (.:?))
import Data.Aeson.Types (parseEither)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as B64
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.HTTP.Client
  ( HttpException, Manager, brReadSome, parseRequest, redirectCount, responseBody
  , responseStatus, responseTimeout, responseTimeoutMicro, withResponse )
import Network.HTTP.Types.Status (statusCode)

data AuthFailure = InvalidToken | AuthUnavailable deriving (Eq, Show)

parseKeys :: BS.ByteString -> Either String [(Text, PublicKey)]
parseKeys bytes = do
  value <- eitherDecodeStrict' bytes
  keys <- parseEither (withObject "JWKS" (.: "keys")) value :: Either String [Value]
  if null keys || length keys > 10 then Left "Invalid key set" else traverse parseKey keys
  where
    parseKey = parseEither $ withObject "JWK" $ \key -> do
      kty <- key .: "kty"
      curve <- key .: "crv"
      alg <- key .: "alg"
      use <- key .: "use"
      kid <- key .: "kid"
      x <- key .: "x"
      y <- key .: "y"
      if (kty, curve, alg, use) /= (("EC", "P-256", "ES256", "sig") :: (Text, Text, Text, Text))
        then fail "Unsupported key"
        else case (B64.decodeUnpadded $ TE.encodeUtf8 x, B64.decodeUnpadded $ TE.encodeUtf8 y) of
          (Right xb, Right yb) | BS.length xb == 32 && BS.length yb == 32 -> do
            let c = getCurveByName SEC_p256r1
                point = Point (os2ip xb) (os2ip yb)
            if isPointValid c point then pure (kid, PublicKey c point) else fail "Invalid point"
          _ -> fail "Invalid coordinates"

verifyToken :: [(Text, PublicKey)] -> Text -> Integer -> BS.ByteString -> Either AuthFailure Text
verifyToken keys appId now token = maybe (Left InvalidToken) Right $ do
  guard $ BS.length token <= 8192
  [header, payload, encodedSignature] <- pure $ BS.split 46 token
  headerJson <- either (const Nothing) Just $ B64.decodeUnpadded header >>= eitherDecodeStrict'
  (alg, kid, critical) <- either (const Nothing) Just $ parseEither
    (withObject "header" $ \o -> (,,) <$> o .: "alg" <*> o .: "kid" <*> o .:? "crit") headerJson
  guard $ alg == ("ES256" :: Text) && (critical :: Maybe [Text]) == Nothing
  key <- lookup kid keys
  signature <- either (const Nothing) Just $ B64.decodeUnpadded encodedSignature
  guard $ BS.length signature == 64
  let sig = Signature (os2ip $ BS.take 32 signature) (os2ip $ BS.drop 32 signature)
  guard $ verify SHA256 key sig (header <> "." <> payload)
  claims <- either (const Nothing) Just $ B64.decodeUnpadded payload >>= eitherDecodeStrict'
  (issuer, audience, subject, expires, issued, notBefore) <- either (const Nothing) Just $ parseEither
    (withObject "claims" $ \o -> (,,,,,) <$> o .: "iss" <*> o .: "aud" <*> o .: "sub"
      <*> o .: "exp" <*> o .: "iat" <*> o .:? "nbf") claims
  guard $ issuer == ("privy.io" :: Text) && audience == appId
  guard $ (expires :: Integer) > now && (issued :: Integer) <= now + 30 && issued < expires
  guard $ maybe True (<= now + 30) (notBefore :: Maybe Integer)
  guard $ "did:privy:" `T.isPrefixOf` subject && T.length subject > 10 && T.length subject <= 256
  pure subject

newtype VerifierCache = VerifierCache (MVar (Maybe (Integer, Either AuthFailure [(Text, PublicKey)])))

newVerifierCache :: IO VerifierCache
newVerifierCache = VerifierCache <$> newMVar Nothing

verifyWithCache :: VerifierCache -> Manager -> Text -> BS.ByteString -> IO (Either AuthFailure Text)
verifyWithCache (VerifierCache cache) manager appId token = do
  now <- floor <$> getPOSIXTime
  -- The URL is derived only from trusted configuration, never JWT jku/x5u.
  if T.null appId || T.length appId > 128 || T.any (\c -> not $ c `elem` (['a'..'z'] <> ['A'..'Z'] <> ['0'..'9'] <> "_-")) appId
    then pure $ Left AuthUnavailable
    else if BS.length token > 8192 || length (BS.split 46 token) /= 3
      then pure $ Left InvalidToken
      else do
        keys <- modifyMVar cache $ \existing -> case existing of
          Just (untilTime, value) | now < untilTime -> pure (existing, value)
          _ -> do
            result <- try @HttpException $ do
              request <- parseRequest $ "https://auth.privy.io/api/v1/apps/" <> T.unpack appId <> "/jwks.json"
              withResponse (request { redirectCount = 0, responseTimeout = responseTimeoutMicro 5000000 }) manager $ \response -> do
                bytes <- LBS.toStrict <$> brReadSome (responseBody response) 65537
                pure $ if statusCode (responseStatus response) == 200 && BS.length bytes <= 65536
                  then parseKeys bytes else Left "Unavailable key set"
            case result of
              Right (Right value) -> pure (Just (now + 60, Right value), Right value)
              _ -> pure (Just (now + 5, Left AuthUnavailable), Left AuthUnavailable)
        pure $ keys >>= \value -> verifyToken value appId now token
