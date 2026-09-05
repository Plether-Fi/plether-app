{-# LANGUAGE TemplateHaskell #-}

-- | Build-time manifest access. Missing or malformed fields fail compilation;
-- no runtime file loading or environment-dependent release identity is needed.
module Plether.Perps.Manifest.Embed (manifestText, manifestInteger) where

import Data.Aeson (Result (..), Value (..), eitherDecodeFileStrict', fromJSON)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Language.Haskell.TH (Exp, Q, runIO)
import Language.Haskell.TH.Syntax (addDependentFile, lift)
import System.Directory (doesFileExist, makeAbsolute)

manifestField :: [String] -> Q Value
manifestField fields = do
  -- Cabal runs from apps/backend locally; Docker copies the same source
  -- manifest beneath its build directory.
  let repositoryPath = "../../config/perps/arbitrum-sepolia-v2.json"
      dockerPath = "config/perps/arbitrum-sepolia-v2.json"
  inRepository <- runIO $ doesFileExist repositoryPath
  path <- runIO $ makeAbsolute $ if inRepository then repositoryPath else dockerPath
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
    Success text -> lift text

manifestInteger :: [String] -> Q Exp
manifestInteger fields = do
  value <- manifestField fields
  case fromJSON value :: Result Integer of
    Error failure -> fail failure
    Success number -> lift number
