{-# LANGUAGE TemplateHaskell #-}

-- | The checked-in release manifest is the single source of deployed identities.
module Plether.Perps.Manifest
  ( releaseChainId
  , releaseDeploymentBlock
  , releaseVolumeHistoryStartTimestamp
  , releaseAaManifestVersion
  , mockUsdcAddress
  , mockUsdcCodeHash
  , marginClearinghouseAddress
  , marginClearinghouseCodeHash
  , cfdEngineAddress
  , cfdEngineCodeHash
  , housePoolAddress
  , housePoolCodeHash
  , seniorVaultAddress
  , seniorVaultCodeHash
  , juniorVaultAddress
  , juniorVaultCodeHash
  , cfdOrderPolicyEvaluatorAddress
  , cfdOrderPolicyEvaluatorCodeHash
  , orderLifecycleBookAddress
  , orderLifecycleBookCodeHash
  , orderRouterAddress
  , orderRouterCodeHash
  , positionProtectionBookAddress
  , positionProtectionBookCodeHash
  , perpsPublicLensAddress
  , perpsPublicLensCodeHash
  , cfdEngineLensAddress
  , cfdEngineLensCodeHash
  , cfdEngineAccountLensAddress
  , cfdEngineAccountLensCodeHash
  , cfdEngineSettlementSidecarAddress
  , cfdEngineSettlementSidecarCodeHash
  , pletherOracleAddress
  , pletherOracleCodeHash
  , settlementMonitorLensAddress
  , settlementMonitorLensCodeHash
  , settlementMonitorLensSidecarAddress
  , settlementMonitorLensSidecarCodeHash
  ) where

import Data.Text (Text)
import Plether.Perps.Manifest.Embed (manifestInteger, manifestText)

releaseChainId :: Integer
releaseChainId = $(manifestInteger ["network", "chainId"])

releaseDeploymentBlock :: Integer
releaseDeploymentBlock = $(manifestInteger ["release", "deploymentBlock"])

releaseVolumeHistoryStartTimestamp :: Integer
releaseVolumeHistoryStartTimestamp = $(manifestInteger ["integration", "volumeHistoryStartTimestamp"])

releaseAaManifestVersion :: Text
releaseAaManifestVersion = $(manifestText ["integration", "aaManifestVersion"])

mockUsdcAddress :: Text
mockUsdcAddress = $(manifestText ["contracts", "mockUsdc", "address"])

mockUsdcCodeHash :: Text
mockUsdcCodeHash = $(manifestText ["contracts", "mockUsdc", "runtimeCodeHash"])

marginClearinghouseAddress :: Text
marginClearinghouseAddress = $(manifestText ["contracts", "marginClearinghouse", "address"])

marginClearinghouseCodeHash :: Text
marginClearinghouseCodeHash = $(manifestText ["contracts", "marginClearinghouse", "runtimeCodeHash"])

cfdEngineAddress :: Text
cfdEngineAddress = $(manifestText ["contracts", "cfdEngine", "address"])

cfdEngineCodeHash :: Text
cfdEngineCodeHash = $(manifestText ["contracts", "cfdEngine", "runtimeCodeHash"])

housePoolAddress :: Text
housePoolAddress = $(manifestText ["contracts", "housePool", "address"])

housePoolCodeHash :: Text
housePoolCodeHash = $(manifestText ["contracts", "housePool", "runtimeCodeHash"])

seniorVaultAddress :: Text
seniorVaultAddress = $(manifestText ["contracts", "seniorVault", "address"])

seniorVaultCodeHash :: Text
seniorVaultCodeHash = $(manifestText ["contracts", "seniorVault", "runtimeCodeHash"])

juniorVaultAddress :: Text
juniorVaultAddress = $(manifestText ["contracts", "juniorVault", "address"])

juniorVaultCodeHash :: Text
juniorVaultCodeHash = $(manifestText ["contracts", "juniorVault", "runtimeCodeHash"])

cfdOrderPolicyEvaluatorAddress :: Text
cfdOrderPolicyEvaluatorAddress = $(manifestText ["contracts", "cfdOrderPolicyEvaluator", "address"])

cfdOrderPolicyEvaluatorCodeHash :: Text
cfdOrderPolicyEvaluatorCodeHash = $(manifestText ["contracts", "cfdOrderPolicyEvaluator", "runtimeCodeHash"])

orderLifecycleBookAddress :: Text
orderLifecycleBookAddress = $(manifestText ["contracts", "orderLifecycleBook", "address"])

orderLifecycleBookCodeHash :: Text
orderLifecycleBookCodeHash = $(manifestText ["contracts", "orderLifecycleBook", "runtimeCodeHash"])

orderRouterAddress :: Text
orderRouterAddress = $(manifestText ["contracts", "orderRouter", "address"])

orderRouterCodeHash :: Text
orderRouterCodeHash = $(manifestText ["contracts", "orderRouter", "runtimeCodeHash"])

positionProtectionBookAddress :: Text
positionProtectionBookAddress = $(manifestText ["contracts", "positionProtectionBook", "address"])

positionProtectionBookCodeHash :: Text
positionProtectionBookCodeHash = $(manifestText ["contracts", "positionProtectionBook", "runtimeCodeHash"])

perpsPublicLensAddress :: Text
perpsPublicLensAddress = $(manifestText ["contracts", "perpsPublicLens", "address"])

perpsPublicLensCodeHash :: Text
perpsPublicLensCodeHash = $(manifestText ["contracts", "perpsPublicLens", "runtimeCodeHash"])

cfdEngineLensAddress :: Text
cfdEngineLensAddress = $(manifestText ["contracts", "cfdEngineLens", "address"])

cfdEngineLensCodeHash :: Text
cfdEngineLensCodeHash = $(manifestText ["contracts", "cfdEngineLens", "runtimeCodeHash"])

cfdEngineAccountLensAddress :: Text
cfdEngineAccountLensAddress = $(manifestText ["contracts", "cfdEngineAccountLens", "address"])

cfdEngineAccountLensCodeHash :: Text
cfdEngineAccountLensCodeHash = $(manifestText ["contracts", "cfdEngineAccountLens", "runtimeCodeHash"])

cfdEngineSettlementSidecarAddress :: Text
cfdEngineSettlementSidecarAddress = $(manifestText ["contracts", "cfdEngineSettlementSidecar", "address"])

cfdEngineSettlementSidecarCodeHash :: Text
cfdEngineSettlementSidecarCodeHash = $(manifestText ["contracts", "cfdEngineSettlementSidecar", "runtimeCodeHash"])

pletherOracleAddress :: Text
pletherOracleAddress = $(manifestText ["contracts", "pletherOracle", "address"])

pletherOracleCodeHash :: Text
pletherOracleCodeHash = $(manifestText ["contracts", "pletherOracle", "runtimeCodeHash"])

settlementMonitorLensAddress :: Text
settlementMonitorLensAddress = $(manifestText ["contracts", "settlementMonitorLens", "address"])

settlementMonitorLensCodeHash :: Text
settlementMonitorLensCodeHash = $(manifestText ["contracts", "settlementMonitorLens", "runtimeCodeHash"])

settlementMonitorLensSidecarAddress :: Text
settlementMonitorLensSidecarAddress = $(manifestText ["contracts", "settlementMonitorLensSidecar", "address"])

settlementMonitorLensSidecarCodeHash :: Text
settlementMonitorLensSidecarCodeHash = $(manifestText ["contracts", "settlementMonitorLensSidecar", "runtimeCodeHash"])
