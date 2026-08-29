module Plether.Insights.Registration.Rotation
  ( RegistrationEmailRotationResult (..)
  , rotateRegistrationEmails
  , registrationKeyReferencePreflight
  , registrationKeyReferenceTotal
  ) where

import Control.Monad (forM)
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Plether.Database (DbPool, withDb)
import qualified Plether.Database.Insights.Registration as Db
import Plether.Insights.Registration.Config (RegistrationConfig (..))
import Plether.Insights.Registration.Crypto
  ( EncryptedValue (..)
  , decryptValue
  , encryptValue
  , registrationFieldAad
  )

data RegistrationEmailRotationResult = RegistrationEmailRotationResult
  { rerrRotatedEmails :: Integer
  , rerrOldKeyReferences :: Db.RegistrationKeyReferenceCounts
  }
  deriving stock (Show, Eq)

-- | Re-encrypt every indefinite email envelope from one explicit old version
-- to the configured active version.  The exact initial count is an operator
-- compare-and-swap guard; each database write is itself version-selective, so
-- an interrupted run is safe to resume with a fresh preflight/count.
--
-- Errors deliberately contain counts/retry instructions only.  Plaintext,
-- application IDs, ciphertext, and row-level failures never leave this module.
rotateRegistrationEmails
  :: DbPool
  -> RegistrationConfig
  -> Text
  -> Integer
  -> IO (Either Text RegistrationEmailRotationResult)
rotateRegistrationEmails pool config oldVersion expectedCount
  | expectedCount < 0 = pure $ Left "Expected email count must be a non-negative integer"
  | oldVersion == activeVersion = pure $ Left "Old key version must differ from the configured active key version"
  | otherwise =
      case (Map.lookup oldVersion keyring, Map.lookup activeVersion keyring) of
        (Nothing, _) -> pure $ Left "Old key version is not present in the configured keyring"
        (_, Nothing) -> pure $ Left "Configured active key version is not present in the keyring"
        (Just oldKey, Just activeKey) -> do
          initialCount <- withDb pool $ \connection ->
            Db.countRegistrationEmailsByKeyVersion connection oldVersion
          if initialCount /= expectedCount
            then pure $ Left "Email rotation preflight count changed; rerun the count-only preflight"
            else rotateBatches oldKey activeKey 0
  where
    keyring = rcEmailKeys config
    activeVersion = rcActiveEmailKeyVersion config
    batchSize = 100

    rotateBatches oldKey activeKey rotated = do
      rows <- withDb pool $ \connection ->
        Db.listRegistrationEmailsForRotation connection oldVersion batchSize
      if null rows
        then finish rotated
        else do
          encryptedBatch <-
            forM rows $ \row -> do
              let envelope = Db.rerEncryptedEmail row
                  aad =
                    registrationFieldAad
                      (Db.rerCompetitionSlug row)
                      (Db.rerApplicationId row)
                      "email"
              pure $ do
                if evKeyVersion envelope /= oldVersion
                  then Left "Email rotation source version changed; rerun the count-only preflight"
                  else pure ()
                plaintext <-
                  either
                    (const $ Left "Email rotation could not authenticate an old envelope")
                    Right
                    (decryptValue oldKey aad envelope)
                Right (Db.rerApplicationId row, aad, plaintext)
          case sequence encryptedBatch of
            Left err -> pure $ Left err
            Right plaintextRows -> do
              replacements <-
                forM plaintextRows $ \(applicationId, aad, plaintext) -> do
                  encrypted <- encryptValue activeVersion activeKey aad plaintext
                  pure $
                    either
                      (const $ Left "Email rotation could not create an active-key envelope")
                      (\replacement -> Right (applicationId, replacement))
                      encrypted
              case sequence replacements of
                Left err -> pure $ Left err
                Right replacementRows -> do
                  updated <- withDb pool $ \connection ->
                    Db.reencryptRegistrationEmails connection oldVersion replacementRows
                  let expectedBatch = fromIntegral $ length replacementRows :: Int64
                  if updated /= expectedBatch
                    then pure $ Left "Email rotation state changed during a batch; rerun the count-only preflight"
                    else rotateBatches oldKey activeKey $ rotated + fromIntegral updated

    finish rotated = do
      remaining <- withDb pool $ \connection ->
        Db.countRegistrationEmailsByKeyVersion connection oldVersion
      references <- registrationKeyReferencePreflight pool oldVersion
      pure $
        if remaining /= 0 || rotated /= expectedCount
          then Left "Email rotation did not reach the exact expected postcondition; rerun the count-only preflight"
          else Right $ RegistrationEmailRotationResult rotated references

registrationKeyReferencePreflight
  :: DbPool
  -> Text
  -> IO Db.RegistrationKeyReferenceCounts
registrationKeyReferencePreflight pool keyVersion =
  withDb pool $ \connection -> Db.countRegistrationKeyReferences connection keyVersion

registrationKeyReferenceTotal :: Db.RegistrationKeyReferenceCounts -> Integer
registrationKeyReferenceTotal counts =
  Db.rkrcEmail counts
    + Db.rkrcXUserId counts
    + Db.rkrcXAccess counts
    + Db.rkrcCsrf counts
    + Db.rkrcPkce counts
    + Db.rkrcWalletMessage counts
