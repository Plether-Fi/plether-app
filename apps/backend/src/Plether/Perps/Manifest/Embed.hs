{-# LANGUAGE TemplateHaskell #-}

-- | Build-time manifest access. Missing or malformed fields fail compilation.
-- PLETHER_PERPS_RELEASE_MANIFEST selects a complete artifact only at compilation;
-- changing that build variable requires a clean rebuild. Runtime environment
-- variables cannot change the embedded release identity.
module Plether.Perps.Manifest.Embed (manifestText, manifestInteger) where

import Data.Aeson (Result (..), Value (..), eitherDecodeFileStrict', fromJSON)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Char (isHexDigit)
import Language.Haskell.TH (Exp, Q, runIO)
import Language.Haskell.TH.Syntax (addDependentFile, lift)
import System.Directory (doesFileExist, makeAbsolute)
import System.Environment (lookupEnv)

manifestField :: [String] -> Q Value
manifestField fields = do
  -- Cabal runs from apps/backend locally; Docker copies the same source
  -- manifest beneath its build directory.
  let repositoryPath = "../../config/perps/arbitrum-sepolia-v2.json"
      dockerPath = "config/perps/arbitrum-sepolia-v2.json"
  inRepository <- runIO $ doesFileExist repositoryPath
  selected <- runIO $ lookupEnv "PLETHER_PERPS_RELEASE_MANIFEST"
  relativePath <- case selected of
    Nothing -> pure $ if inRepository then repositoryPath else dockerPath
    Just "" -> fail "PLETHER_PERPS_RELEASE_MANIFEST must not be empty"
    Just configured -> pure configured
  path <- runIO $ makeAbsolute relativePath
  addDependentFile path
  decoded <- runIO $ eitherDecodeFileStrict' path
  root <- either fail pure decoded
  walk root fields
 where
  walk value [] = pure value
  walk (Object object) (key : rest) =
    maybe (fail $ "Missing release manifest field: " <> show fields)
      (`walk` rest) (KeyMap.lookup (Key.fromString key) object)
  walk _ _ = fail $ "Invalid release manifest path: " <> show fields

manifestText :: [String] -> Q Exp
manifestText fields = do
  value <- manifestField fields
  case fromJSON value :: Result String of
    Error failure -> fail failure
    Success text
      | null text -> fail $ "Empty release manifest field: " <> show fields
      | last fields == "address" && not (validHex 40 text) ->
          fail $ "Invalid release contract address: " <> show fields
      | last fields == "runtimeCodeHash" && not (validHex 64 text) ->
          fail $ "Invalid release runtime code hash: " <> show fields
      | otherwise -> lift text
 where
  validHex digits text = take 2 text == "0x" && length text == digits + 2
    && all isHexDigit (drop 2 text) && any (/= '0') (drop 2 text)

manifestInteger :: [String] -> Q Exp
manifestInteger fields = do
  value <- manifestField fields
  case fromJSON value :: Result Integer of
    Error failure -> fail failure
    Success number
      | fields == ["network", "chainId"] && number `notElem` [42161, 421614] ->
          fail "The perps release must target Arbitrum One (42161) or Arbitrum Sepolia (421614)"
      | number <= 0 -> fail $ "Release manifest integer must be positive: " <> show fields
      | otherwise -> lift number
