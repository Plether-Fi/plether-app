module Plether.AA.PreparationRecovery
  ( parseRecoveryRequest, requiredText, renderChallenge, randomToken, tokenHash
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
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Plether.AA.Pimlico as Legacy
import Plether.Database.AaPreparationRecovery (Scope (..))

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
