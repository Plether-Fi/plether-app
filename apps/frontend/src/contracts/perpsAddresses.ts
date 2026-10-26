import type { Address } from 'viem'
import release from '../../../../config/perps/arbitrum-sepolia-v3.json'
import { parsePerpsReleaseConfiguration, settlementTokenContract } from './perpsRelease'

export const PERPS_RELEASE = parsePerpsReleaseConfiguration(release)
export const PERPS_SETTLEMENT_TOKEN = PERPS_RELEASE.settlementToken
export const PERPS_SETTLEMENT_TOKEN_LABEL = PERPS_RELEASE.schemaVersion === 4
  ? PERPS_SETTLEMENT_TOKEN.name
  : PERPS_SETTLEMENT_TOKEN.symbol
export const PERPS_SETTLEMENT_TOKEN_CONTRACT = settlementTokenContract(PERPS_RELEASE)

/** Active perps graph, pinned to the deployed v1.3.1 release. */
export const PERPS_CLOSE_PREVIEW_ADDRESS = PERPS_RELEASE.contracts.cfdClosePreview.address

export const PERPS_ARBITRUM_SEPOLIA_CHAIN_ID = 421614 as const
export const PERPS_ARBITRUM_SEPOLIA_DEPLOYMENT_BLOCK = PERPS_RELEASE.release.deploymentBlock

export type PerpsContractAddresses = typeof PERPS_ARBITRUM_SEPOLIA

export const PERPS_ARBITRUM_SEPOLIA = {
  pyth: PERPS_RELEASE.release.pyth,
  usdc: PERPS_SETTLEMENT_TOKEN_CONTRACT.address,
  marginClearinghouse: PERPS_RELEASE.contracts.marginClearinghouse.address,
  cfdEngine: PERPS_RELEASE.contracts.cfdEngine.address,
  terminalNavBookV2: PERPS_RELEASE.contracts.terminalNavBookV2.address,
  cfdEnginePlanner: PERPS_RELEASE.contracts.cfdEnginePlanner.address,
  cfdEngineSettlementSidecar: PERPS_RELEASE.contracts.cfdEngineSettlementSidecar.address,
  cfdEngineAdmin: PERPS_RELEASE.contracts.cfdEngineAdmin.address,
  housePoolRedemptionMathSidecar: PERPS_RELEASE.contracts.housePoolRedemptionMathSidecar.address,
  housePool: PERPS_RELEASE.contracts.housePool.address,
  cfdEngineProtocolLens: PERPS_RELEASE.contracts.cfdEngineProtocolLens.address,
  seniorVault: PERPS_RELEASE.contracts.seniorVault.address,
  juniorVault: PERPS_RELEASE.contracts.juniorVault.address,
  cfdEngineAccountLens: PERPS_RELEASE.contracts.cfdEngineAccountLens.address,
  cfdEngineLens: PERPS_RELEASE.contracts.cfdEngineLens.address,
  cfdEngineOpenQuoter: PERPS_RELEASE.contracts.cfdEngineOpenQuoter.address,
  policyEvaluator: PERPS_RELEASE.contracts.cfdOrderPolicyEvaluator.address,
  cfdClosePreview: PERPS_RELEASE.contracts.cfdClosePreview.address,
  orderRouterExecutionSidecar: PERPS_RELEASE.contracts.orderRouterExecutionSidecar.address,
  orderRecoverySidecar: PERPS_RELEASE.contracts.orderRecoverySidecar.address,
  pletherOracle: PERPS_RELEASE.contracts.pletherOracle.address,
  orderRouterLiquidationBatchSidecar: PERPS_RELEASE.contracts.orderRouterLiquidationBatchSidecar.address,
  orderLifecycleBook: PERPS_RELEASE.contracts.orderLifecycleBook.address,
  orderRouter: PERPS_RELEASE.contracts.orderRouter.address,
  orderRouterAdmin: PERPS_RELEASE.contracts.orderRouterAdmin.address,
  positionProtectionBook: PERPS_RELEASE.contracts.positionProtectionBook.address,
  perpsPublicLens: PERPS_RELEASE.contracts.perpsPublicLens.address,
  settlementMonitorLens: PERPS_RELEASE.contracts.settlementMonitorLens.address,
  settlementMonitorLensSidecar: PERPS_RELEASE.contracts.settlementMonitorLensSidecar.address,
  emergencyPauseCoordinator: PERPS_RELEASE.contracts.emergencyPauseCoordinator.address,
} satisfies Record<string, Address>
