module Plether.AA.PreparationRecovery
  ( parseRecoveryRequest, parseRecoveryOrigins, requiredText, renderChallenge, randomToken, tokenHash
  , PreparationClientRecovery (..), selectPreparationClient
  ) where

import Control.Monad (unless)
import Crypto.Hash (Digest, SHA256, hash)
import Crypto.Random (getRandomBytes)
import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as B16
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.URI (URI (..), URIAuth (..), parseURI)
import Text.Read (readMaybe)
import qualified Plether.AA.Pimlico as Legacy
import Plether.Database.AaPreparationRecovery (Scope (..))

-- The canonical hosted origin stays HTTPS. Additional exact origins are an
-- explicit operator choice; unencrypted loopback is supported only on Sepolia.
parseRecoveryOrigins :: Integer -> Maybe Text -> Text -> Either Text [Text]
parseRecoveryOrigins chain primary extra = do
  let additional = if T.null extra then [] else map T.strip $ T.splitOn "," extra
  unless (length additional <= 8 && length additional == length (nub additional)) $
    Left "Recovery requires at most eight distinct additional origins"
  case primary of
    Nothing -> do
      unless (null additional) $ Left "Additional recovery origins require a canonical HTTPS origin"
      pure []
    Just canonical -> do
      unless (validOrigin False canonical) $ Left "Recovery origin must be an exact HTTPS origin"
      unless (all (validOrigin $ chain == 421614) additional && canonical `notElem` additional) $
        Left "Additional recovery origins must be exact HTTPS origins or explicitly allowed Sepolia loopback HTTP origins"
      pure $ canonical : additional
 where
  validOrigin allowLoopback value
    | T.length value > 256 || T.strip value /= value = False
    | otherwise = case parseURI $ T.unpack value of
        Just uri | Just auth <- uriAuthority uri ->
          let host = uriRegName auth
              port = uriPort auth
              validPort = case port of
                "" -> True
                ':':first:rest -> first `elem` ['1'..'9'] && all (`elem` ['0'..'9']) rest
                  && maybe False (\number -> number >= 1 && number <= 65535) (readMaybe (first:rest) :: Maybe Int)
                _ -> False
              httpsHost = not (null host) && all (`elem` (['a'..'z'] <> ['0'..'9'] <> ".-")) host
              validScheme = (uriScheme uri == "https:" && httpsHost)
                || (allowLoopback && uriScheme uri == "http:" && host `elem` ["localhost", "127.0.0.1", "[::1]"])
           in uriUserInfo auth == "" && uriPath uri == "" && uriQuery uri == "" && uriFragment uri == "" && validPort && validScheme
        _ -> False

parseRecoveryRequest :: Text -> [Text] -> [Value] -> Either Legacy.ProxyFailure (Scope, KM.KeyMap Value)
parseRecoveryRequest paymaster extra [Object fields] = do
  unless (all (\key -> K.toText key `elem` (["version","chainId","sender","preparationId"] <> extra)) $ KM.keys fields) $
    Left $ Legacy.invalidParams "Unknown recovery field"
  unless (KM.lookup "version" fields == Just (Number 1) && KM.lookup "chainId" fields == Just (String "0x66eee")) $
    Left $ Legacy.invalidParams "Unsupported recovery version or chain"
  sender <- requiredText "sender" 42 fields >>= canonicalHex 20
  identifier <- requiredText "preparationId" 66 fields >>= canonicalHex 32
  pure (Scope 421614 (T.toLower paymaster) sender identifier, fields)
 where
  canonicalHex bytes value
    | T.length value == 2+bytes*2 && T.take 2 value == "0x" && T.all (`elem` (['0'..'9'] <> ['a'..'f'])) (T.drop 2 value) = Right value
    | otherwise = Left $ Legacy.invalidParams "Invalid recovery locator"
parseRecoveryRequest _ _ _ = Left $ Legacy.invalidParams "Recovery requires one versioned locator"

requiredText :: Text -> Int -> KM.KeyMap Value -> Either Legacy.ProxyFailure Text
requiredText key limit fields = case KM.lookup (K.fromText key) fields of
  Just (String value) | not (T.null value) && T.length value <= limit -> Right value
  _ -> Left $ Legacy.invalidParams $ "Invalid recovery " <> key

renderChallenge :: Text -> Scope -> Text -> Text -> Integer -> Text
renderChallenge origin (Scope chain paymaster sender identifier) owner nonce expires = T.intercalate "\n"
  [ "Plether Trading Account Recovery", "Origin: " <> origin
  , "Chain ID: " <> T.pack (show chain), "Paymaster: " <> paymaster
  , "Trading Account: " <> sender, "Preparation ID: " <> identifier, "Owner: " <> owner
  , "Purpose: Recover this saved attempt and allow retiring it only after backend safety checks."
  , "This message does not authorize a blockchain transaction or spending."
  , "Nonce: " <> nonce, "Expires At (Unix seconds): " <> T.pack (show expires) ]

randomToken :: IO Text
randomToken = TE.decodeUtf8 . B16.encode <$> (getRandomBytes 32 :: IO ByteString)

tokenHash :: Text -> Text
tokenHash token = TE.decodeUtf8 $ B16.encode (convert (hash $ TE.encodeUtf8 token :: Digest SHA256) :: ByteString)

-- A verified missing preparation can retry its original ID in the current
-- namespace. A known historical match must retain its original namespace.
data PreparationClientRecovery = ClientAllowed Text | ClientProofRequired Text | ClientAmbiguous
  deriving stock (Eq, Show)

selectPreparationClient :: Text -> Bool -> [(Text,Maybe Text,Bool)] -> PreparationClientRecovery
selectPreparationClient current supplied matches = case matches of
  [] -> if supplied then ClientProofRequired current else ClientAllowed current
  [(client,_,_)] | client == current && not supplied -> ClientAllowed current
  [(client,_,True)] -> ClientProofRequired client
  _ -> ClientAmbiguous
