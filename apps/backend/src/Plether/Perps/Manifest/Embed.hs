{-# LANGUAGE TemplateHaskell #-}

-- | Embed the canonical future release. Prepared metadata is buildable but can
-- never authorize perps requests or workers. Null pins become internal inactive
-- sentinels only; they are never written back to the release document.
module Plether.Perps.Manifest.Embed
  ( manifestText, manifestInteger, manifestSettlementText, manifestIsDeployed
  , settlementTokenField, releaseIsDeployed
  ) where

import Control.Monad (unless, forM_)
import Data.Aeson (Result (..), Value (..), eitherDecodeFileStrict', fromJSON)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Char (isHexDigit)
import qualified Data.Text as T
import Language.Haskell.TH (Exp, Q, runIO)
import Language.Haskell.TH.Syntax (addDependentFile, lift)
import System.Directory (doesFileExist, makeAbsolute)

manifestRoot :: Q Value
manifestRoot = do
  let repositoryPath = "../../config/perps/mock-usdg-prepared.json"
      dockerPath = "config/perps/mock-usdg-prepared.json"
  inRepository <- runIO $ doesFileExist repositoryPath
  path <- runIO $ makeAbsolute $ if inRepository then repositoryPath else dockerPath
  addDependentFile path
  decoded <- runIO $ eitherDecodeFileStrict' path
  root <- either fail pure decoded
  _ <- either fail pure $ releaseIsDeployed root
  pure root

fieldAt :: Value -> [String] -> Either String Value
fieldAt value [] = Right value
fieldAt (Object object) (key : rest) =
  maybe (Left $ "Missing release manifest field: " <> key)
    (`fieldAt` rest) (KeyMap.lookup (Key.fromString key) object)
fieldAt _ fields = Left $ "Invalid release manifest path: " <> show fields

-- | Schema four is a breaking contract. No historical token-key adapters.
settlementTokenField :: Value -> String -> Either String Value
settlementTokenField root field = do
  schema <- fieldAt root ["schemaVersion"]
  unless (schema == Number 4) $ Left "Backend requires release schemaVersion 4"
  contracts <- fieldAt root ["contracts"]
  case contracts of
    Object object | KeyMap.member (Key.fromString "mockUsdc") object ->
      Left "Historical token entries cannot activate the current backend"
    _ -> pure ()
  fieldAt contracts ["settlementToken", field]

releaseIsDeployed :: Value -> Either String Bool
releaseIsDeployed root = do
  _ <- settlementTokenField root "address"
  forM_ [("name", String "Mock USDG"), ("symbol", String "USDG"), ("decimals", Number 6),
         ("isMock", Bool True), ("supportsReceiveWithAuthorization", Bool False), ("supportsBridgeFunding", Bool False)] $ \(key, expected) -> do
    actual <- fieldAt root ["settlementToken", key]
    unless (actual == expected) $ Left $ "Invalid MockUSDG metadata: " <> key
  chain <- fieldAt root ["network", "chainId"]
  unless (chain == Number 421614) $ Left "Unsupported perps chain"
  interface <- fieldAt root ["orderInterfaceVersion"]
  unless (interface == Number 3) $ Left "Unsupported perps order interface"
  phase <- fieldAt root ["release", "phase"]
  deployed <- case phase of
    String "prepared" -> Right False
    String value | value `elem` ["deployed", "seeded", "active"] -> Right True
    _ -> Left "Invalid perps release phase"
  contracts <- fieldAt root ["contracts"]
  forM_ ["settlementToken", "marginClearinghouse", "cfdEngine", "terminalNavBookV2", "cfdEnginePlanner", "cfdEngineSettlementSidecar", "cfdEngineAdmin", "housePoolRedemptionMathSidecar", "housePool", "cfdEngineProtocolLens", "seniorVault", "juniorVault", "cfdEngineAccountLens", "cfdEngineLens", "cfdEngineOpenQuoter", "cfdOrderPolicyEvaluator", "cfdClosePreview", "orderRouterExecutionSidecar", "orderRecoverySidecar", "pletherOracle", "orderRouterLiquidationBatchSidecar", "orderLifecycleBook", "orderRouter", "orderRouterAdmin", "positionProtectionBook", "perpsPublicLens", "settlementMonitorLens", "settlementMonitorLensSidecar", "emergencyPauseCoordinator"] $ \name -> do
    _ <- fieldAt root ["contracts", name, "address"]
    pure ()
  case contracts of
    Object entries -> forM_ (KeyMap.toList entries) $ \(key, _) ->
      forM_ [("address", 40), ("runtimeCodeHash", 64)] $ \(field, width) -> do
        value <- fieldAt root ["contracts", Key.toString key, field]
        unless (if deployed then validPin width value else value == Null) $
          Left $ "Invalid " <> Key.toString key <> " " <> field <> " for release phase"
    _ -> Left "Invalid contract pins"
  forM_ [["release", "deploymentBlock"], ["integration", "volumeHistoryStartTimestamp"]] $ \path -> do
    value <- fieldAt root path
    unless (if deployed then case fromJSON value :: Result Integer of {Success n -> n > 0; _ -> False} else value == Null) $
      Left $ "Invalid release boundary: " <> show path
  forM_ [["release", "version"], ["integration", "aaManifestVersion"], ["release", "sourceCommit"]] $ \path -> do
    value <- fieldAt root path
    unless (if deployed then case value of {String text -> not (T.null text); _ -> False} else value == Null) $
      Left $ "Invalid release identity: " <> show path
  pure deployed
 where
  validPin width (String value) =
    T.length value == width + 2 && T.take 2 value == "0x"
      && T.all isHexDigit (T.drop 2 value) && T.any (/= '0') (T.drop 2 value)
  validPin _ _ = False

manifestIsDeployed :: Q Exp
manifestIsDeployed = manifestRoot >>= either fail lift . releaseIsDeployed

manifestSettlementText :: String -> Q Exp
manifestSettlementText field = manifestText ["contracts", "settlementToken", field]

manifestText :: [String] -> Q Exp
manifestText fields = do
  root <- manifestRoot
  value <- either fail pure $ fieldAt root fields
  case value of
    String text -> lift $ T.unpack text
    Null -> case reverse fields of
      "address" : _ -> lift ("0x" <> replicate 40 '0')
      "runtimeCodeHash" : _ -> lift ("0x" <> replicate 64 '0')
      _ -> lift ("" :: String)
    _ -> fail $ "Invalid manifest string: " <> show fields

manifestInteger :: [String] -> Q Exp
manifestInteger fields = do
  root <- manifestRoot
  value <- either fail pure $ fieldAt root fields
  case value of
    Null -> lift (0 :: Integer)
    _ -> case fromJSON value :: Result Integer of
      Error failure -> fail failure
      Success number -> lift number
