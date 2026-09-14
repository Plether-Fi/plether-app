-- | Read-only, operation-scoped credentials. Never use these for issuance or
-- submission. Database recovery authorization/expiry must still be checked.
module Plether.AA.RecoveryCapability (issue, verify) where

import Crypto.Hash (SHA256)
import Crypto.MAC.HMAC (HMAC, hmac)
import Data.ByteArray (convert, constEq)
import qualified Data.ByteString.Base16 as B16
import Data.Char (isHexDigit)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Text.Read (readMaybe)

issue :: Text -> Text -> Integer -> Text -> Text -> Text
issue secret deployment now operation client =
  let payload = T.intercalate "." ["v1", T.toLower operation, client, T.pack $ show $ now + 604800]
  in payload <> "." <> mac secret deployment payload

verify :: Text -> Text -> Integer -> Text -> Text -> Maybe Text
verify secret deployment now operation token = case T.splitOn "." token of
  ["v1", op, client, expires, signature]
    | validKey op && validKey client && op == T.toLower operation
    , Just deadline <- readMaybe (T.unpack expires)
    , deadline > now && deadline <= now + 604800
    , T.length signature == 64
    , TE.encodeUtf8 signature `constEq` TE.encodeUtf8 (mac secret deployment $ T.intercalate "." ["v1",op,client,expires])
      -> Just client
  _ -> Nothing
 where
  validKey value = T.length value == 66 && T.take 2 value == "0x" && T.all isHexDigit (T.drop 2 value)

mac :: Text -> Text -> Text -> Text
mac secret deployment payload = TE.decodeUtf8 $ B16.encode $ convert
  (hmac (TE.encodeUtf8 secret) (TE.encodeUtf8 $ "plether:aa:recovery:421614:alto:" <> deployment <> ":" <> payload) :: HMAC SHA256)
