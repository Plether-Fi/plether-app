module Main (main) where

import Data.Aeson (eitherDecodeFileStrict')
import qualified Plether.Perps.CandleAdmin as CandleAdmin
import System.Environment (getArgs, withArgs)

-- This separate test executable accepts an explicit profile. The production
-- executable has no profile argument or runtime guard-bypass switch.
main :: IO ()
main = do
  arguments <- getArgs
  case arguments of
    profilePath : command -> do
      profile <- eitherDecodeFileStrict' profilePath >>= either fail pure
      withArgs command $ CandleAdmin.mainWithManifest profile
    [] -> fail "Candle integration driver requires an explicit deployment profile"
