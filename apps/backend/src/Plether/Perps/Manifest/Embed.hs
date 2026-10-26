{-# LANGUAGE TemplateHaskell #-}

-- | Build-time manifest access. Missing or malformed fields fail compilation;
-- no runtime file loading or environment-dependent release identity is needed.
module Plether.Perps.Manifest.Embed
  ( manifestText, manifestInteger, manifestSettlementText, settlementTokenField ) where

import Data.Aeson (Result (..), Value (..), eitherDecodeFileStrict', fromJSON)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Language.Haskell.TH (Exp, Q, runIO)
import Language.Haskell.TH.Syntax (addDependentFile, lift)
import System.Directory (doesFileExist, makeAbsolute)

manifestRoot :: Q Value
manifestRoot = do
  -- Cabal runs from apps/backend locally; Docker copies the same source
  -- manifest beneath its build directory.
  let repositoryPath = "../../config/perps/arbitrum-sepolia-v3.json"
      dockerPath = "config/perps/arbitrum-sepolia-v3.json"
  inRepository <- runIO $ doesFileExist repositoryPath
  path <- runIO $ makeAbsolute $ if inRepository then repositoryPath else dockerPath
  addDependentFile path
  decoded <- runIO $ eitherDecodeFileStrict' path
  root <- either fail pure decoded
  phase <- either fail pure $ fieldAt root ["release", "phase"]
  case fromJSON phase :: Result String of
    Success value | value `elem` ["deployed", "seeded", "active"] -> pure root
    _ -> fail "Backend requires a deployed perps release; prepared manifests cannot be activated"

fieldAt :: Value -> [String] -> Either String Value
fieldAt value [] = Right value
fieldAt (Object object) (key : rest) =
  maybe (Left $ "Missing release manifest field: " <> key)
    (`fieldAt` rest) (KeyMap.lookup (Key.fromString key) object)
fieldAt _ fields = Left $ "Invalid release manifest path: " <> show fields

manifestField :: [String] -> Q Value
manifestField fields = manifestRoot >>= either fail pure . (`fieldAt` fields)

-- | Legacy releases use mockUsdc. A present but invalid canonical entry must
-- fail closed rather than silently selecting a different token.
settlementTokenField :: Value -> String -> Either String Value
settlementTokenField root field = do
  contracts <- fieldAt root ["contracts"]
  schema <- fieldAt root ["schemaVersion"]
  schemaVersion <- case fromJSON schema :: Result Integer of
    Success number | number `elem` [1, 2, 3, 4] -> Right number
    _ -> Left "Unsupported release manifest schema"
  let present fieldKey = case contracts of
        Object object -> KeyMap.member (Key.fromString fieldKey) object
        _ -> False
      schema4 = schemaVersion == 4
      key = if schema4 then "settlementToken" else "mockUsdc"
      hasMetadata = case root of
        Object object -> KeyMap.member (Key.fromString "settlementToken") object
        _ -> False
  if (schema4 && present "mockUsdc") || (not schema4 && (present "settlementToken" || hasMetadata))
    then Left "Settlement token key conflicts with release schema"
    else pure ()
  value <- fieldAt contracts [key, field]
  case fromJSON value :: Result String of
    Success text | not (null text) -> Right value
    _ -> Left $ "Invalid settlement token " <> field

manifestSettlementText :: String -> Q Exp
manifestSettlementText field = do
  root <- manifestRoot
  value <- either fail pure $ settlementTokenField root field
  case fromJSON value :: Result String of
    Error failure -> fail failure
    Success text -> lift text

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
