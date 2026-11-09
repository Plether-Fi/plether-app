module Plether.Handlers.PasskeyNames (registerPasskeyNameRoutes, registerRoutes) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value, eitherDecode, object, withObject, (.:), (.=))
import Data.Aeson.Types (parseEither)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as LT
import Network.HTTP.Client (Manager)
import Network.HTTP.Types.Status (Status, status400, status401, status429, status503)
import Plether.AA.Pimlico (readBoundedRequestBody)
import Plether.Database (DbPool, withDb)
import Plether.Passkeys.Auth (AuthFailure (..), VerifierCache, verifyWithCache)
import qualified Plether.Passkeys.Names as Names
import System.Environment (lookupEnv)
import Web.Scotty (ActionM, ScottyM, get, header, json, put, queryParamMaybe, request, setHeader, status)

registerPasskeyNameRoutes :: VerifierCache -> DbPool -> Manager -> ScottyM ()
registerPasskeyNameRoutes cache pool manager = do
  let authenticate token = do
        appId <- maybe "" T.pack <$> lookupEnv "PRIVY_APP_ID"
        verifyWithCache cache manager appId token
  registerRoutes authenticate (withDb pool . flip Names.listNames)
    (\user credential name -> withDb pool $ \connection -> Names.saveName connection user credential name)

registerRoutes :: (BS.ByteString -> IO (Either AuthFailure Text))
  -> (Text -> IO [(Text, Text)]) -> (Text -> Text -> Text -> IO Bool) -> ScottyM ()
registerRoutes authenticate list save = do
  get "/api/user/passkey-names" $ authorized $ \user -> do
    expected <- queryParamMaybe "userId"
    if expected /= Just user then failure status401 "The signed-in account changed. Reopen the wallet modal."
    else do
      names <- liftIO $ list user
      json $ object ["userId" .= user, "names" .= [object ["credentialId" .= credential, "name" .= name] | (credential, name) <- names]]
  put "/api/user/passkey-names" $ authorized $ \user -> do
    contentType <- header "Content-Type"
    req <- request
    bounded <- liftIO $ readBoundedRequestBody 16384 req
    let parsed = do
          bytes <- either (const $ Left "body") Right bounded
          value <- eitherDecode bytes :: Either String Value
          parseEither (withObject "name" $ \o -> (,,) <$> o .: "userId" <*> o .: "credentialId" <*> o .: "name") value
    case parsed of
      Right (expected, _, _) | expected /= user -> failure status401 "The signed-in account changed. Reopen the wallet modal."
      Right (_, credential, name) | maybe False ((== "application/json") . T.toLower . T.strip . T.takeWhile (/= ';') . LT.toStrict) contentType
        && Names.validCredential credential && Names.validName name -> do
          saved <- liftIO $ save user credential (T.strip name)
          if saved then json (object ["userId" .= user, "name" .= T.strip name])
            else failure status429 "Too many named passkeys. Clear an unused name first."
      _ -> failure status400 "Use a valid passkey and a name of at most 64 characters."
  where
    failure :: Status -> Text -> ActionM ()
    failure code message = status code >> json (object ["error" .= message])
    authorized action = do
      setHeader "Cache-Control" "no-store"
      setHeader "Vary" "Authorization"
      supplied <- header "Authorization"
      case supplied >>= T.stripPrefix "Bearer " . LT.toStrict of
        Nothing -> failure status401 "Sign in to manage passkey names."
        Just token -> do
          result <- liftIO $ authenticate $ TE.encodeUtf8 token
          case result of
            Right user -> action user
            Left InvalidToken -> failure status401 "Sign in again to manage passkey names."
            Left AuthUnavailable -> failure status503 "Passkey names are temporarily unavailable. Try again."
