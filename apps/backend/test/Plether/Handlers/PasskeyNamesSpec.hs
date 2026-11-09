module Plether.Handlers.PasskeyNamesSpec (spec) where

import Control.Concurrent.Async (concurrently)
import Control.Exception (bracket)
import Crypto.PubKey.ECC.Generate (generate)
import Crypto.PubKey.ECC.Types (CurveName (SEC_p256r1), getCurveByName)
import Data.Aeson (Value (..), encode, object, (.=))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Database.PostgreSQL.Simple (Only (..), close, connectPostgreSQL, execute)
import Network.HTTP.Client (RequestBody (..), defaultManagerSettings, httpLbs, method, newManager, parseRequest, requestBody, requestHeaders, responseBody, responseHeaders, responseStatus)
import Network.HTTP.Types.Status (statusCode)
import Network.Wai.Handler.Warp (testWithApplication)
import Plether.Handlers.PasskeyNames (registerRoutes)
import Plether.Passkeys.Auth (verifyToken)
import Plether.Passkeys.AuthSpec (claims, header, signed)
import qualified Plether.Passkeys.Names as Names
import System.Environment (lookupEnv)
import Test.Hspec
import Web.Scotty (scottyApp)

spec :: Spec
spec = describe "Passkey-name API" $ do
  it "authenticates real tokens, isolates accounts, persists per-key edits and clears names" $ do
    (public, private) <- generate $ getCurveByName SEC_p256r1
    first <- signed private header claims
    let Object original = claims
    second <- signed private header $ Object $ KM.insert "sub" (String "did:privy:user-b") original
    store <- newIORef Map.empty
    let list user = map (\((_, key), name) -> (key, name)) . filter ((== user) . fst . fst) . Map.toList <$> readIORef store
        save user key name = atomicModifyIORef' store $ \rows ->
          (if name == "" then Map.delete (user, key) rows else Map.insert (user, key) name rows, True)
    application <- scottyApp $ registerRoutes (pure . verifyToken [("test-key", public)] "public-app" 1000) list save
    manager <- newManager defaultManagerSettings
    testWithApplication (pure application) $ \port -> do
      let call token verb suffix value = do
            base <- parseRequest $ "http://127.0.0.1:" <> show port <> "/api/user/passkey-names" <> suffix
            httpLbs (base { method = verb, requestHeaders = [("Authorization", "Bearer " <> token), ("Content-Type", "application/json")], requestBody = RequestBodyLBS $ encode value }) manager
          putName token user key name = call token "PUT" "" $ object ["userId" .= (user :: Text), "credentialId" .= (key :: Text), "name" .= (name :: Text)]
      written <- putName first "did:privy:user-a" "key-one" "YubiKey"
      statusCode (responseStatus written) `shouldBe` 200
      _ <- putName first "did:privy:user-a" "key-two" "Phone"
      own <- call first "GET" "?userId=did:privy:user-a" Null
      responseBody own `shouldSatisfy` BS8.isInfixOf "YubiKey" . LBS.toStrict
      lookup "Cache-Control" (responseHeaders own) `shouldBe` Just "no-store"
      other <- call second "GET" "?userId=did:privy:user-b" Null
      responseBody other `shouldBe` "{\"names\":[],\"userId\":\"did:privy:user-b\"}"
      forbidden <- putName second "did:privy:user-a" "key-one" "Hacked"
      statusCode (responseStatus forbidden) `shouldBe` 401
      invalid <- call "a.b.c" "GET" "?userId=did:privy:user-a" Null
      statusCode (responseStatus invalid) `shouldBe` 401
      _ <- putName first "did:privy:user-a" "key-one" ""
      readIORef store `shouldReturn` Map.singleton ("did:privy:user-a", "key-two") "Phone"
      badName <- putName first "did:privy:user-a" "key-one" "bad\nname"
      statusCode (responseStatus badName) `shouldBe` 400
      oversized <- call first "PUT" "" $ String $ T.replicate 17000 "a"
      statusCode (responseStatus oversized) `shouldBe` 400

  it "validates names and opaque credential identifiers" $ do
    Names.validName "YubiKey USB-C" `shouldBe` True
    Names.validName "" `shouldBe` True
    Names.validName (T.replicate 65 "x") `shouldBe` False
    Names.validName "bad\NULname" `shouldBe` False
    Names.validCredential "test_key-123" `shouldBe` True
    Names.validCredential "" `shouldBe` False
    Names.validCredential "../key" `shouldBe` False

  it "stores edits in PostgreSQL without overwriting another account or another passkey" $ do
    configured <- lookupEnv "PASSKEY_NAMES_TEST_DATABASE_URL"
    case configured of
      Nothing -> pendingWith "Set PASSKEY_NAMES_TEST_DATABASE_URL to an isolated test database"
      Just url -> bracket (connectPostgreSQL $ BS8.pack url) close $ \connection -> do
        Names.ensureSchema connection
        Names.ensureSchema connection
        Names.saveName connection "did:privy:db-a" "key1" "YubiKey" `shouldReturn` True
        Names.saveName connection "did:privy:db-a" "key2" "Phone" `shouldReturn` True
        Names.saveName connection "did:privy:db-b" "key1" "Other user" `shouldReturn` True
        Names.saveName connection "did:privy:db-a" "key1" "Renamed" `shouldReturn` True
        Names.listNames connection "did:privy:db-a" `shouldReturn` [("key1", "Renamed"), ("key2", "Phone")]
        Names.listNames connection "did:privy:db-b" `shouldReturn` [("key1", "Other user")]
        Names.saveName connection "did:privy:db-a" "key1" "" `shouldReturn` True
        Names.listNames connection "did:privy:db-a" `shouldReturn` [("key2", "Phone")]

  it "bounds storage per account and preserves simultaneous edits from two connections" $ do
    configured <- lookupEnv "PASSKEY_NAMES_TEST_DATABASE_URL"
    case configured of
      Nothing -> pendingWith "Set PASSKEY_NAMES_TEST_DATABASE_URL to an isolated test database"
      Just url -> bracket (connectPostgreSQL $ BS8.pack url) close $ \first ->
        bracket (connectPostgreSQL $ BS8.pack url) close $ \second -> do
          _ <- concurrently (Names.ensureSchema first) (Names.ensureSchema second)
          _ <- execute first "DELETE FROM passkey_names WHERE privy_user_id = ?" (Only ("did:privy:quota-test" :: Text))
          mapM_ (\n -> Names.saveName first "did:privy:quota-test" (T.pack $ show (n :: Int)) "Name" `shouldReturn` True) [1..200]
          Names.saveName first "did:privy:quota-test" "201" "Extra" `shouldReturn` False
          Names.saveName first "did:privy:quota-test" "1" "Rename at limit" `shouldReturn` True
          Names.saveName first "did:privy:quota-test" "1" "" `shouldReturn` True
          Names.saveName first "did:privy:quota-test" "201" "Extra" `shouldReturn` True
          result <- concurrently
            (Names.saveName first "did:privy:concurrent-test" "laptop" "Laptop")
            (Names.saveName second "did:privy:concurrent-test" "yubikey" "YubiKey")
          result `shouldBe` (True, True)
          Names.listNames first "did:privy:concurrent-test" `shouldReturn` [("laptop", "Laptop"), ("yubikey", "YubiKey")]

  it "syncs a signed-in user's name through the real HTTP route and PostgreSQL to a second client" $ do
    configured <- lookupEnv "PASSKEY_NAMES_TEST_DATABASE_URL"
    case configured of
      Nothing -> pendingWith "Set PASSKEY_NAMES_TEST_DATABASE_URL to an isolated test database"
      Just url -> bracket (connectPostgreSQL $ BS8.pack url) close $ \connection -> do
        Names.ensureSchema connection
        (public, private) <- generate $ getCurveByName SEC_p256r1
        let Object original = claims
            user = "did:privy:http-db-user" :: Text
        token <- signed private header $ Object $ KM.insert "sub" (String user) original
        application <- scottyApp $ registerRoutes
          (pure . verifyToken [("test-key", public)] "public-app" 1000)
          (Names.listNames connection) (Names.saveName connection)
        laptop <- newManager defaultManagerSettings
        phone <- newManager defaultManagerSettings
        testWithApplication (pure application) $ \port -> do
          let urlBase = "http://127.0.0.1:" <> show port <> "/api/user/passkey-names"
          base <- parseRequest urlBase
          written <- httpLbs (base { method = "PUT", requestHeaders = [("Authorization", "Bearer " <> token), ("Content-Type", "application/json")],
            requestBody = RequestBodyLBS $ encode $ object ["userId" .= user, "credentialId" .= ("hardware-key" :: Text), "name" .= ("YubiKey" :: Text)] }) laptop
          statusCode (responseStatus written) `shouldBe` 200
          readRequest <- parseRequest $ urlBase <> "?userId=" <> T.unpack user
          received <- httpLbs (readRequest { requestHeaders = [("Authorization", "Bearer " <> token)] }) phone
          statusCode (responseStatus received) `shouldBe` 200
          responseBody received `shouldSatisfy` BS8.isInfixOf "YubiKey" . LBS.toStrict
          Names.listNames connection user `shouldReturn` [("hardware-key", "YubiKey")]
