module Main (main) where

import Control.Exception (SomeException, displayException, try)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Plether.AA.Kms
  ( PaymasterSigner (..)
  , newKmsPaymasterSigner
  )
import Plether.Database (newDbPool, withDb)
import Plether.Database.AaSponsorship
  ( ensureAaSponsorshipSchema
  , resumeAaIssuance
  )
import Plether.Ethereum.Abi (keccak256)
import Plether.Ethereum.Transaction (recoverSignerAddress)
import Plether.Logging (field, logError, logInfo)
import System.Environment (getArgs, lookupEnv)
import System.Exit (exitFailure)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["attest-kms"] -> attestKms
    "resume-issuance" : options -> resumeIssuance options
    _ ->
      fatal
        "aa_admin_command_invalid"
        "usage: plether-aa-admin attest-kms | resume-issuance --expected-reason REASON --operator-note NOTE"

attestKms :: IO ()
attestKms = do
  keyId <- requireEnv "AA_PAYMASTER_KMS_KEY_ID"
  configuredSigner <- requireEnv "AA_PAYMASTER_SIGNER_ADDRESS"
  manager <- newManager tlsManagerSettings
  signerResult <- newKmsPaymasterSigner manager keyId configuredSigner
  signer <- either (fatal "aa_admin_kms_attestation_failed") pure signerResult
  signatureResult <- psSignDigest signer kmsAttestationDigest
  signature <- either (fatal "aa_admin_kms_attestation_failed") pure signatureResult
  parity <-
    case BS.unsnoc signature of
      Just (compact, recoveryByte)
        | BS.length compact == 64
        , recoveryByte `elem` [27, 28]
        , scalarS compact <= curveOrder `div` 2 -> do
            let recoveryParity = fromIntegral recoveryByte - 27
            recovered <- recoverSignerAddress kmsAttestationDigest compact recoveryParity
            case recovered of
              Right address | T.toLower address == T.toLower (psAddress signer) ->
                pure recoveryParity
              _ -> fatal "aa_admin_kms_attestation_failed" "KMS test signature did not recover to the attested signer"
      _ -> fatal "aa_admin_kms_attestation_failed" "KMS returned a non-canonical recoverable signature"
  logInfo
    "aa_admin_kms_attested"
    "Production KMS signer passed public-key and fixed-digest signing attestation"
    [ field "signer_address" $ psAddress signer
    , field "signature_length" $ BS.length signature
    , field "recovery_parity" parity
    ]

resumeIssuance :: [String] -> IO ()
resumeIssuance options = do
  (expectedReason, operatorNote) <-
    either (fatal "aa_admin_command_invalid") pure $ parseResumeOptions options
  databaseUrl <- requireEnv "DATABASE_URL"
  pool <- newDbPool databaseUrl
  schemaResult <- try @SomeException $ withDb pool ensureAaSponsorshipSchema
  case schemaResult of
    Left err ->
      fatal
        "aa_admin_schema_invalid"
        ("Could not validate the AA sponsorship schema: " <> T.pack (displayException err))
    Right () -> pure ()
  resumed <- withDb pool $ \conn -> resumeAaIssuance conn expectedReason operatorNote
  case resumed of
    Left reason -> fatal "aa_admin_resume_refused" reason
    Right () ->
      logInfo
        "aa_admin_issuance_resumed"
        "An operator explicitly cleared the AA issuance circuit breaker"
        [ field "previous_reason" expectedReason
        , field "operator_note_length" $ T.length $ T.strip operatorNote
        ]

parseResumeOptions :: [String] -> Either Text (Text, Text)
parseResumeOptions = go Nothing Nothing
 where
  go expected note [] = case (expected, note) of
    (Just expectedReason, Just operatorNote)
      | T.null expectedReason -> Left "--expected-reason must not be blank"
      | T.length expectedReason > 512 -> Left "--expected-reason must not exceed 512 characters"
      | T.null (T.strip operatorNote) -> Left "--operator-note must not be blank"
      | T.length (T.strip operatorNote) > 512 -> Left "--operator-note must not exceed 512 characters"
      | otherwise -> Right (expectedReason, operatorNote)
    _ -> Left "both --expected-reason and --operator-note are required"
  go Nothing note ("--expected-reason" : value : rest) =
    go (Just $ T.pack value) note rest
  go expected Nothing ("--operator-note" : value : rest) =
    go expected (Just $ T.pack value) rest
  go _ _ _ = Left "resume-issuance received duplicate, missing, or unsupported options"

requireEnv :: String -> IO Text
requireEnv name = do
  value <- fmap (T.strip . T.pack) <$> lookupEnv name
  case value of
    Just configured | not (T.null configured) -> pure configured
    _ -> fatal "aa_admin_configuration_invalid" $ T.pack name <> " is required"

fatal :: Text -> Text -> IO a
fatal eventName reason = do
  logError eventName "AA administration command failed" [field "error" reason]
  exitFailure

kmsAttestationDigest :: ByteString
kmsAttestationDigest = keccak256 "Plether-AA-KMS-Attestation/v1"

scalarS :: ByteString -> Integer
scalarS = BS.foldl' (\total byte -> total * 256 + fromIntegral byte) 0 . BS.drop 32

curveOrder :: Integer
curveOrder = 0xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141
