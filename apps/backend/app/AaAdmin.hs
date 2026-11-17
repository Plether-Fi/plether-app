module Main (main) where

import qualified Plether.Perps.Manifest as Manifest

import Control.Exception (SomeException, displayException, try)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Int (Int64)
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
  ( AaIssuanceResumeRequest (..)
  , ensureAaSponsorshipSchema
  , resumeAaIssuance
  )
import Plether.Ethereum.Abi (keccak256)
import Plether.Ethereum.Transaction (recoverSignerAddress)
import Plether.Logging (field, logError, logInfo)
import System.Environment (getArgs, lookupEnv)
import System.Exit (exitFailure)
import Text.Read (readMaybe)

main :: IO ()
main = do
  Manifest.requireDeployedRelease
  args <- getArgs
  case args of
    ["attest-kms"] -> attestKms
    "resume-issuance" : options -> resumeIssuance options
    _ ->
      fatal
        "aa_admin_command_invalid"
        "usage: plether-aa-admin attest-kms | resume-issuance --expected-pause-event-id ID --paymaster ADDRESS --expected-reason REASON --operator-note NOTE"

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
  request <-
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
  resumed <- withDb pool $ \conn -> resumeAaIssuance conn request
  case resumed of
    Left reason -> fatal "aa_admin_resume_refused" reason
    Right () ->
      logInfo
        "aa_admin_issuance_resumed"
        "An operator explicitly cleared the AA issuance circuit breaker"
        [ field "previous_reason" $ airExpectedReason request
        , field "previous_pause_event_id" $ T.pack $ show $ airExpectedPauseEventId request
        , field "operator_note_length" $ T.length $ T.strip $ airOperatorNote request
        ]

parseResumeOptions :: [String] -> Either Text AaIssuanceResumeRequest
parseResumeOptions options = do
  pairs <- collect [] options
  expectedReason <- required "--expected-reason" pairs
  operatorNote <- required "--operator-note" pairs
  generationText <- required "--expected-pause-event-id" pairs
  paymaster <- required "--paymaster" pairs
  generation <- case readMaybe generationText :: Maybe Int64 of
    Just value | value > 0 && show value == generationText -> Right value
    _ -> Left "--expected-pause-event-id must be a canonical positive 64-bit integer"
  let normalizedPaymaster = T.toLower $ T.pack paymaster
  if T.null (T.pack expectedReason) || length expectedReason > 512
    then Left "--expected-reason must contain 1-512 characters"
    else if T.null (T.strip $ T.pack operatorNote) || T.length (T.strip $ T.pack operatorNote) > 512
      then Left "--operator-note must contain 1-512 characters"
      else if T.length normalizedPaymaster /= 42 || T.take 2 normalizedPaymaster /= "0x"
        || not (T.all (`elem` ("0123456789abcdef" :: String)) $ T.drop 2 normalizedPaymaster)
        || T.drop 2 normalizedPaymaster == T.replicate 40 "0"
        then Left "--paymaster must be a nonzero Ethereum address"
        else Right AaIssuanceResumeRequest
          { airExpectedPauseEventId = generation
          , airExpectedReason = T.pack expectedReason
          , airChainId = Manifest.releaseChainId
          , airPaymaster = normalizedPaymaster
          , airOperatorNote = T.pack operatorNote
          }
 where
  collect found [] = Right found
  collect found (flag : value : rest)
    | flag `elem` ["--expected-reason", "--operator-note", "--expected-pause-event-id", "--paymaster"]
    , lookup flag found == Nothing = collect ((flag, value) : found) rest
  collect _ _ = Left "resume-issuance received duplicate, missing, or unsupported options"
  required flag pairs = maybe (Left $ T.pack flag <> " is required") Right $ lookup flag pairs

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
