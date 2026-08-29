module Plether.Insights.Registration.ConfigSpec (spec) where

import Control.Exception (bracket)
import Control.Monad (forM_)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Plether.Insights.Registration.Config
  ( RegistrationConfig (..)
  , loadRegistrationConfig
  )
import System.Environment (lookupEnv, setEnv, unsetEnv)
import Test.Hspec

spec :: Spec
spec = do
  describe "loadRegistrationConfig" $ do
    it "stays absent when neither provisioning nor activation is enabled" $ do
      withRegistrationEnv [] $ do
        result <- loadRegistrationConfig
        result `shouldSatisfy` isDisabled
      withRegistrationEnv
        [ ("INSIGHTS_REGISTRATION_PROVISIONED", "false")
        , ("INSIGHTS_REGISTRATION_ENABLED", "false")
        ]
        $ do
          result <- loadRegistrationConfig
          result `shouldSatisfy` isDisabled

    it "validates both provisioning and activation flags strictly" $ do
      withRegistrationEnv [("INSIGHTS_REGISTRATION_ENABLED", "enabled")] $
        do
          result <- loadRegistrationConfig
          fmap (const ()) result
            `shouldBe` Left "INSIGHTS_REGISTRATION_ENABLED must be a boolean"
      withRegistrationEnv [("INSIGHTS_REGISTRATION_PROVISIONED", "ready")] $ do
        result <- loadRegistrationConfig
        fmap (const ()) result
          `shouldBe` Left "INSIGHTS_REGISTRATION_PROVISIONED must be a boolean"
      withRegistrationEnv [("INSIGHTS_REGISTRATION_PROVISIONED", " true ")] $ do
        result <- loadRegistrationConfig
        fmap (const ()) result
          `shouldBe` Left "INSIGHTS_REGISTRATION_PROVISIONED must be a boolean"

    it "refuses activation unless the service is explicitly provisioned" $ do
      let environment = filter ((/= "INSIGHTS_REGISTRATION_PROVISIONED") . fst) validEnvironment
      withRegistrationEnv environment $ do
        result <- loadRegistrationConfig
        fmap (const ()) result
          `shouldBe` Left "INSIGHTS_REGISTRATION_ENABLED=true requires INSIGHTS_REGISTRATION_PROVISIONED=true"
      withRegistrationEnv (replaceEnv "INSIGHTS_REGISTRATION_PROVISIONED" "false" validEnvironment) $ do
        result <- loadRegistrationConfig
        fmap (const ()) result
          `shouldBe` Left "INSIGHTS_REGISTRATION_ENABLED=true requires INSIGHTS_REGISTRATION_PROVISIONED=true"

    it "loads provisioned configuration without activating public routes" $
      withRegistrationEnv (replaceEnv "INSIGHTS_REGISTRATION_ENABLED" "false" validEnvironment) $ do
        result <- loadRegistrationConfig
        case result of
          Right (Just config) -> rcActivationEnabled config `shouldBe` False
          other -> expectationFailure $ "expected provisioned inactive configuration, got " <> show other

    it "loads a canonical enabled configuration with conservative defaults" $
      withRegistrationEnv validEnvironment $ do
        result <- loadRegistrationConfig
        case result of
          Right (Just config) -> do
            rcActivationEnabled config `shouldBe` True
            rcPublicOrigin config `shouldBe` "https://insights.plether.com"
            rcOriginTokenNext config `shouldBe` Nothing
            rcTurnstileExpectedAction config `shouldBe` "competition_registration"
            rcXCallbackCompetitionSlug config `shouldBe` "testnet-trading-2026-09"
            rcXTargetHandle config `shouldBe` "plether_fi"
            rcActiveEmailKeyVersion config `shouldBe` "v2"
            Map.keys (rcEmailKeys config) `shouldBe` ["v1", "v2"]
            BS.length (rcEmailKeys config Map.! "v1") `shouldBe` 32
            BS.length (rcEmailKeys config Map.! "v2") `shouldBe` 32
            BS.length (rcLookupHmacKey config) `shouldBe` 32
            rcSessionTtlSeconds config `shouldBe` 1800
            rcIpRateLimitPerMinute config `shouldBe` 10
            rcSessionRateLimitPerMinute config `shouldBe` 30
            rcMinimumXAccountAgeDays config `shouldBe` 90
          other -> expectationFailure $ "expected an enabled configuration, got " <> show other

    it "accepts one distinct strong next edge token for zero-downtime rotation" $
      withRegistrationEnv
        (("INSIGHTS_REGISTRATION_ORIGIN_TOKEN_NEXT", validOriginTokenNext) : validEnvironment)
        $ do
          result <- loadRegistrationConfig
          case result of
            Right (Just config) ->
              rcOriginTokenNext config `shouldBe` Just (BS.pack $ map (fromIntegral . fromEnum) validOriginTokenNext)
            other -> expectationFailure $ "expected overlap token configuration, got " <> show other

    it "rejects an edge-token overlap that repeats the current secret" $
      withRegistrationEnv
        (("INSIGHTS_REGISTRATION_ORIGIN_TOKEN_NEXT", validOriginToken) : validEnvironment)
        $ do
          result <- loadRegistrationConfig
          expectErrorContaining
            "duplicate edge tokens"
            "must differ from INSIGHTS_REGISTRATION_ORIGIN_TOKEN"
            result

    it "never includes registration secrets or email keys in Show output" $
      withRegistrationEnv validEnvironment $ do
        result <- loadRegistrationConfig
        case result of
          Right (Just config) -> do
            let rendered = show config
            rendered `shouldNotContain` validOriginToken
            rendered `shouldNotContain` validTurnstileSecret
            rendered `shouldNotContain` validXClientSecret
            rendered `shouldNotContain` strongEmailKeyBase64
            rendered `shouldNotContain` secondEmailKeyBase64
            rendered `shouldNotContain` strongHmacKeyBase64
            rendered `shouldContain` "<redacted keyring>"
          other -> expectationFailure $ "expected an enabled configuration, got " <> show other

    it "rejects non-canonical or potentially ambiguous public origins" $ do
      forM_
        [ "http://insights.plether.com"
        , "https://Insights.plether.com"
        , "https://insights.plether.com:443"
        , "https://insights.plether.com/register"
        , "https://insights.plether.com?next=evil"
        ]
        $ \origin ->
          withRegistrationEnv (replaceEnv "INSIGHTS_REGISTRATION_PUBLIC_ORIGIN" origin validEnvironment) $ do
            result <- loadRegistrationConfig
            result `shouldSatisfy` hasErrorContaining "canonical lowercase HTTPS origin"

    it "pins the registration and Turnstile host to the reviewed production hostname" $ do
      withRegistrationEnv
        (replaceEnv "INSIGHTS_REGISTRATION_PUBLIC_ORIGIN" "https://preview.plether.com" validEnvironment)
        $ do
          result <- loadRegistrationConfig
          result `shouldSatisfy` hasErrorContaining "must equal https://insights.plether.com"
      withRegistrationEnv
        (replaceEnv "TURNSTILE_EXPECTED_HOSTNAME" "preview.plether.com" validEnvironment)
        $ do
          result <- loadRegistrationConfig
          result `shouldSatisfy` hasErrorContaining "must equal insights.plether.com"

    it "pins the OAuth callback to the exact origin and competition callback path" $ do
      forM_
        [ "https://evil.example/api/insights/v1/competitions/testnet-trading-2026-09/registrations/x/callback"
        , "https://insights.plether.com/api/insights/v1/competitions/Testnet/registrations/x/callback"
        , "https://insights.plether.com/api/insights/v1/competitions/testnet-trading-2026-09/registrations/x/callback?next=evil"
        , "https://insights.plether.com/competitions/testnet-trading-2026-09/register"
        ]
        $ \callback ->
          withRegistrationEnv (replaceEnv "X_OAUTH_CALLBACK_URL" callback validEnvironment) $ do
            result <- loadRegistrationConfig
            result `shouldSatisfy` hasErrorContaining "exact canonical Insights origin"

    it "rejects missing key versions, short keys, whitespace-bearing secrets, and non-canonical limits" $ do
      let invalidCases =
            [ ( "missing active email key"
              , replaceEnv "INSIGHTS_REGISTRATION_EMAIL_KEY_VERSION" "v3" validEnvironment
              , "is absent"
              )
            , ( "short encryption key"
              , replaceEnv "INSIGHTS_REGISTRATION_EMAIL_KEYS_JSON" "{\"v1\":\"AA==\"}" validEnvironment
              , "exactly 32 bytes"
              )
            , ( "short lookup key"
              , replaceEnv "INSIGHTS_REGISTRATION_EMAIL_HMAC_KEY_BASE64" "AA==" validEnvironment
              , "at least 32 bytes"
              )
            , ( "origin secret with whitespace"
              , replaceEnv "INSIGHTS_REGISTRATION_ORIGIN_TOKEN" "origin token 0123456789abcdef0123456789abcdef" validEnvironment
              , "must not contain whitespace"
              )
            , ( "zero-padded session TTL"
              , ("INSIGHTS_REGISTRATION_SESSION_TTL_SECONDS", "0300") : validEnvironment
              , "whole number between 300 and 3600"
              )
            ]
      forM_ invalidCases $ \(label, environment, message) ->
        withRegistrationEnv environment $ do
          result <- loadRegistrationConfig
          expectErrorContaining label message result

    it "rejects repeated-byte key material and low-diversity origin or X secrets" $ do
      let weakSecretCases =
            [ ( "repeated-byte AES email key"
              , replaceEnv
                  "INSIGHTS_REGISTRATION_EMAIL_KEYS_JSON"
                  ("{\"v1\":\"" <> repeatedByteKeyBase64 <> "\"}")
                  validEnvironment
              )
            , ( "repeated-byte email lookup HMAC key"
              , replaceEnv "INSIGHTS_REGISTRATION_EMAIL_HMAC_KEY_BASE64" repeatedByteKeyBase64 validEnvironment
              )
            , ( "low-diversity edge-origin token"
              , replaceEnv "INSIGHTS_REGISTRATION_ORIGIN_TOKEN" (concat $ replicate 8 "abcd") validEnvironment
              )
            , ( "low-diversity X OAuth client secret"
              , replaceEnv "X_OAUTH_CLIENT_SECRET" (replicate 16 'x') validEnvironment
              )
            , ( "low-diversity Turnstile secret"
              , replaceEnv "TURNSTILE_SECRET_KEY" (replicate 16 't') validEnvironment
              )
            ]
      forM_ weakSecretCases $ \(label, environment) ->
        withRegistrationEnv environment $ do
          result <- loadRegistrationConfig
          expectErrorContaining label "generated from strong random material" result

    it "requires Turnstile credentials to contain at least 16 strong bytes" $
      withRegistrationEnv (replaceEnv "TURNSTILE_SECRET_KEY" "A2b4C6d8" validEnvironment) $ do
        result <- loadRegistrationConfig
        expectErrorContaining "short Turnstile secret" "must contain at least 16 bytes" result

    it "requires distinct material for every AES version and a separate HMAC key" $ do
      let duplicateKeyring =
            "{\"v1\":\""
              <> strongEmailKeyBase64
              <> "\",\"v2\":\""
              <> strongEmailKeyBase64
              <> "\"}"
      withRegistrationEnv (replaceEnv "INSIGHTS_REGISTRATION_EMAIL_KEYS_JSON" duplicateKeyring validEnvironment) $ do
        result <- loadRegistrationConfig
        expectErrorContaining
          "duplicate AES versions"
          "must use distinct key material for every version"
          result
      withRegistrationEnv (replaceEnv "INSIGHTS_REGISTRATION_EMAIL_HMAC_KEY_BASE64" strongEmailKeyBase64 validEnvironment) $ do
        result <- loadRegistrationConfig
        expectErrorContaining
          "HMAC key reused as AES key"
          "must differ from every email encryption key"
          result

    it "rejects placeholder origins, Turnstile settings, and X credentials" $ do
      let placeholderCases =
            [ ( "public origin"
              , replaceEnv "INSIGHTS_REGISTRATION_PUBLIC_ORIGIN" "https://prod.example.com" validEnvironment
              )
            , ( "Turnstile secret"
              , replaceEnv "TURNSTILE_SECRET_KEY" "replace-me-now" validEnvironment
              )
            , ( "Turnstile hostname"
              , replaceEnv "TURNSTILE_EXPECTED_HOSTNAME" "captcha.example" validEnvironment
              )
            , ( "X OAuth client ID"
              , replaceEnv "X_OAUTH_CLIENT_ID" "your-secret-client-id" validEnvironment
              )
            , ( "X OAuth client secret"
              , replaceEnv "X_OAUTH_CLIENT_SECRET" "change-me-9T4nQ7vLs2" validEnvironment
              )
            , ( "target X handle"
              , replaceEnv "X_TARGET_HANDLE" "placeholder" validEnvironment
              )
            , ( "target X numeric ID"
              , replaceEnv "X_TARGET_USER_ID" "123456789" validEnvironment
              )
            ]
      forM_ placeholderCases $ \(label, environment) ->
        withRegistrationEnv environment $ do
          result <- loadRegistrationConfig
          expectErrorContaining label "must not contain an example or placeholder value" result

    it "allows only the fixed competition_registration Turnstile action" $ do
      forM_ ["login", "competition-registration", " competition_registration"] $ \action ->
        withRegistrationEnv (replaceEnv "TURNSTILE_EXPECTED_ACTION" action validEnvironment) $ do
          result <- loadRegistrationConfig
          expectErrorContaining
            ("Turnstile action " <> show action)
            "TURNSTILE_EXPECTED_ACTION must equal competition_registration"
            result

validEnvironment :: [(String, String)]
validEnvironment =
  [ ("INSIGHTS_REGISTRATION_PROVISIONED", "true")
  , ("INSIGHTS_REGISTRATION_ENABLED", "true")
  , ("INSIGHTS_REGISTRATION_PUBLIC_ORIGIN", "https://insights.plether.com")
  , ("INSIGHTS_REGISTRATION_ORIGIN_TOKEN", validOriginToken)
  , ("TURNSTILE_SECRET_KEY", validTurnstileSecret)
  , ("TURNSTILE_EXPECTED_HOSTNAME", "insights.plether.com")
  , ("X_OAUTH_CLIENT_ID", "TjA4bGh6QnV8z2Mf")
  , ("X_OAUTH_CLIENT_SECRET", validXClientSecret)
  , ( "X_OAUTH_CALLBACK_URL"
    , "https://insights.plether.com/api/insights/v1/competitions/testnet-trading-2026-09/registrations/x/callback"
    )
  , ("X_TARGET_USER_ID", "1738274910263847562")
  , ( "INSIGHTS_REGISTRATION_EMAIL_KEYS_JSON"
    , "{\"v1\":\""
        <> strongEmailKeyBase64
        <> "\",\"v2\":\""
        <> secondEmailKeyBase64
        <> "\"}"
    )
  , ("INSIGHTS_REGISTRATION_EMAIL_KEY_VERSION", "v2")
  , ("INSIGHTS_REGISTRATION_EMAIL_HMAC_KEY_BASE64", strongHmacKeyBase64)
  , ("INSIGHTS_REGISTRATION_RULES_VERSION", "2026-09-v1")
  , ("INSIGHTS_REGISTRATION_PRIVACY_VERSION", "2026-09-v1")
  ]

validOriginToken :: String
validOriginToken = "K7m2Q9vL4xR8pC3nW6tY1sD5fH0jB7uE"

validOriginTokenNext :: String
validOriginTokenNext = "R4v8M2qW7nK1sC9xT6pL3hD0fY5jB8uG"

validTurnstileSecret :: String
validTurnstileSecret = "0x4F7q9Nz3Lp8Rc2Vm6Tk1Ws5Y"

validXClientSecret :: String
validXClientSecret = "Qp7Vn2Ls9Kx4Rm8Tz6Bc"

strongEmailKeyBase64 :: String
strongEmailKeyBase64 = "AAECAwQFBgcICQoLDA0ODxAREhMUFRYXGBkaGxwdHh8="

secondEmailKeyBase64 :: String
secondEmailKeyBase64 = "QEFCQ0RFRkdISUpLTE1OT1BRUlNUVVZXWFlaW1xdXl8="

strongHmacKeyBase64 :: String
strongHmacKeyBase64 = "ICEiIyQlJicoKSorLC0uLzAxMjM0NTY3ODk6Ozw9Pj8="

repeatedByteKeyBase64 :: String
repeatedByteKeyBase64 = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="

registrationEnvironmentNames :: [String]
registrationEnvironmentNames =
  [ "INSIGHTS_REGISTRATION_PROVISIONED"
  , "INSIGHTS_REGISTRATION_ENABLED"
  , "INSIGHTS_REGISTRATION_PUBLIC_ORIGIN"
  , "INSIGHTS_REGISTRATION_ORIGIN_TOKEN"
  , "INSIGHTS_REGISTRATION_ORIGIN_TOKEN_NEXT"
  , "TURNSTILE_SECRET_KEY"
  , "TURNSTILE_EXPECTED_HOSTNAME"
  , "TURNSTILE_EXPECTED_ACTION"
  , "X_OAUTH_CLIENT_ID"
  , "X_OAUTH_CLIENT_SECRET"
  , "X_OAUTH_CALLBACK_URL"
  , "X_TARGET_USER_ID"
  , "X_TARGET_HANDLE"
  , "INSIGHTS_REGISTRATION_EMAIL_KEYS_JSON"
  , "INSIGHTS_REGISTRATION_EMAIL_KEY_VERSION"
  , "INSIGHTS_REGISTRATION_EMAIL_HMAC_KEY_BASE64"
  , "INSIGHTS_REGISTRATION_SESSION_TTL_SECONDS"
  , "INSIGHTS_REGISTRATION_IP_RATE_LIMIT_PER_MINUTE"
  , "INSIGHTS_REGISTRATION_SESSION_RATE_LIMIT_PER_MINUTE"
  , "INSIGHTS_REGISTRATION_RULES_VERSION"
  , "INSIGHTS_REGISTRATION_PRIVACY_VERSION"
  ]

withRegistrationEnv :: [(String, String)] -> IO a -> IO a
withRegistrationEnv environment action =
  bracket capture restore $ \_ -> do
    mapM_ unsetEnv registrationEnvironmentNames
    mapM_ (uncurry setEnv) environment
    action
  where
    capture = mapM (\name -> do value <- lookupEnv name; pure (name, value)) registrationEnvironmentNames
    restore saved = forM_ saved $ \(name, value) -> maybe (unsetEnv name) (setEnv name) value

replaceEnv :: String -> String -> [(String, String)] -> [(String, String)]
replaceEnv name value environment = (name, value) : filter ((/= name) . fst) environment

hasErrorContaining :: String -> Either String a -> Bool
hasErrorContaining expected result = case result of
  Left message -> T.pack expected `T.isInfixOf` T.pack message
  Right _ -> False

isDisabled :: Either String (Maybe RegistrationConfig) -> Bool
isDisabled (Right Nothing) = True
isDisabled _ = False

expectErrorContaining :: String -> String -> Either String a -> Expectation
expectErrorContaining label expected result
  | hasErrorContaining expected result = pure ()
  | otherwise = expectationFailure $ label <> ": expected an error containing " <> show expected
