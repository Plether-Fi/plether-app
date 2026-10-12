-- Local test adapter: exercise the same deadline parser/cap/relay gate as Gateway.
-- No network, signer credentials, database, or production configuration.
{-# LANGUAGE PackageImports #-}
import qualified "base16-bytestring" Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BS
import Plether.AA.Pimlico (capSponsorshipDeadline, validateSubmissionHeadroom)
import System.Environment (getArgs)

main :: IO ()
main = do
  [now, expiry, calldata] <- getArgs
  bytes <- either fail pure $ B16.decode $ BS.pack calldata
  bounded <- either (fail . show) pure $ capSponsorshipDeadline (read expiry) bytes
  either (fail . show) pure $ validateSubmissionHeadroom (read now) bounded bytes
  print bounded
