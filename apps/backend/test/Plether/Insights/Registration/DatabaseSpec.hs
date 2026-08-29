module Plether.Insights.Registration.DatabaseSpec (spec) where

import Data.List (isInfixOf)
import System.Directory (doesFileExist)
import Test.Hspec

spec :: Spec
spec = do
  describe "RegistrationSessionRow SQL projection contract" $ do
    it "keeps csrf_digest in the same slot in registrationSessionSelect and FromRow" $ do
      sourcePath <- locateRegistrationSource
      source <- normalizeWhitespace <$> readFile sourcePath
      source
        `shouldSatisfy` isInfixOf
          ( normalizeWhitespace
              "EXTRACT(EPOCH FROM s.expires_at)::BIGINT, s.csrf_digest, s.csrf_key_version, s.csrf_nonce, s.csrf_ciphertext, s.csrf_tag, s.oauth_error_code, a.email_masked, a.x_username"
          )
      source
        `shouldSatisfy` isInfixOf
          ( normalizeWhitespace
              "expiresTimestamp <- field csrfDigest <- field csrfKeyVersion <- field csrfNonce <- field csrfCiphertext <- field csrfTag <- field oauthErrorCode <- field emailMasked <- field xUsername <- field"
          )
      source
        `shouldSatisfy` isInfixOf
          ( normalizeWhitespace
              "rsrSessionExpiresTimestamp = expiresTimestamp , rsrCsrfDigest = csrfDigest , rsrCsrfEncrypted = EncryptedValue csrfKeyVersion csrfNonce csrfCiphertext csrfTag , rsrOauthErrorCode = oauthErrorCode , rsrEmailMasked = emailMasked"
          )

    it "keeps wallet ownership proof independent from release history" $ do
      sourcePath <- locateRegistrationSource
      source <- normalizeWhitespace <$> readFile sourcePath
      source
        `shouldNotSatisfy` isInfixOf
          (normalizeWhitespace "perps_indexer_state")
      source
        `shouldNotSatisfy` isInfixOf
          (normalizeWhitespace "perps_account_activity")
      source
        `shouldSatisfy` isInfixOf
          (normalizeWhitespace "owner_wallet = ?, trading_account = ?, wallet_verification_block = ?, wallet_verification_block_hash = ?")
      source
        `shouldSatisfy` isInfixOf
          (normalizeWhitespace "INSERT INTO insights_competition_participants (competition_slug, wallet, trader_reference, alias, eligibility_status)")

    it "publishes the exact privacy version in the same transaction that opens registration" $ do
      sourcePath <- locateRegistrationSource
      source <- normalizeWhitespace <$> readFile sourcePath
      source
        `shouldSatisfy` isInfixOf
          ( normalizeWhitespace
              "SET registration_open_timestamp = FLOOR(EXTRACT(EPOCH FROM NOW()))::BIGINT, privacy_notice_version = ?, updated_at = NOW()"
          )
      source
        `shouldSatisfy` isInfixOf
          (normalizeWhitespace "AND (privacy_notice_version IS NULL OR privacy_notice_version = ?)")
      source
        `shouldSatisfy` isInfixOf
          (normalizeWhitespace "AND privacy_notice_version = ? AND NOW() >= TO_TIMESTAMP(registration_open_timestamp)")

  describe "static registration schema" $
    it "mirrors all private registration tables and privacy constraints" $ do
      schemaPath <- locateSchema
      schema <- normalizeWhitespace <$> readFile schemaPath
      mapM_
        (\needle -> schema `shouldSatisfy` isInfixOf (normalizeWhitespace needle))
        [ "CREATE TABLE IF NOT EXISTS insights_registration_competition_config"
        , "CREATE TABLE IF NOT EXISTS insights_registration_applications"
        , "CREATE TABLE IF NOT EXISTS insights_registration_sessions"
        , "CREATE TABLE IF NOT EXISTS insights_registration_rate_limits"
        , "status <> 'completed' OR ( completed_at IS NOT NULL AND rules_version IS NOT NULL AND privacy_version IS NOT NULL"
        ]

locateRegistrationSource :: IO FilePath
locateRegistrationSource = go candidates
  where
    candidates =
      [ "src/Plether/Database/Insights/Registration.hs"
      , "apps/backend/src/Plether/Database/Insights/Registration.hs"
      ]
    go [] = expectationFailure "could not locate the registration database source" >> pure ""
    go (candidate : remaining) = do
      exists <- doesFileExist candidate
      if exists then pure candidate else go remaining

locateSchema :: IO FilePath
locateSchema = go candidates
  where
    candidates = ["schema.sql", "apps/backend/schema.sql"]
    go [] = expectationFailure "could not locate the backend schema" >> pure ""
    go (candidate : remaining) = do
      exists <- doesFileExist candidate
      if exists then pure candidate else go remaining

normalizeWhitespace :: String -> String
normalizeWhitespace = unwords . words . filter (/= '\\')
