module Plether.AA.ClientKey
  ( pseudonymousClientKey
  , pseudonymousAccountKey
  ) where

import Crypto.Hash (SHA256)
import Crypto.MAC.HMAC (HMAC, hmac)
import Data.ByteArray (convert)
import qualified Data.ByteString.Base16 as B16
import Data.Text (Text)
import qualified Data.Text.Encoding as TE

pseudonymousClientKey :: Text -> Text -> Text
pseudonymousClientKey secret clientIp =
  pseudonym secret $ "client-ip:" <> clientIp

pseudonymousAccountKey :: Text -> Text -> Text
pseudonymousAccountKey secret account =
  pseudonym secret $ "account:" <> account

pseudonym :: Text -> Text -> Text
pseudonym secret value =
  "0x" <> TE.decodeUtf8 (B16.encode digest)
 where
  digest = convert (hmac (TE.encodeUtf8 secret) (TE.encodeUtf8 value) :: HMAC SHA256)
