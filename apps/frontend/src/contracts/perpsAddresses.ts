import type { Address } from 'viem'
import release from '../../../../config/perps/arbitrum-sepolia-v3.json'

/** Active perps graph, pinned to the deployed v1.3.0 release. */
export const PERPS_CLOSE_PREVIEW_ADDRESS = release.contracts.cfdClosePreview.address as Address

export const PERPS_ARBITRUM_SEPOLIA_CHAIN_ID = 421614 as const
export const PERPS_ARBITRUM_SEPOLIA_DEPLOYMENT_BLOCK = release.release.deploymentBlock

export type PerpsContractAddresses = typeof PERPS_ARBITRUM_SEPOLIA

export const PERPS_ARBITRUM_SEPOLIA = {
  pyth: release.release.pyth as Address,
  usdc: release.contracts.mockUsdc.address as Address,
  marginClearinghouse: release.contracts.marginClearinghouse.address as Address,
  cfdEngine: release.contracts.cfdEngine.address as Address,
  terminalNavBookV2: release.contracts.terminalNavBookV2.address as Address,
  cfdEnginePlanner: release.contracts.cfdEnginePlanner.address as Address,
  cfdEngineSettlementSidecar: release.contracts.cfdEngineSettlementSidecar.address as Address,
  cfdEngineAdmin: release.contracts.cfdEngineAdmin.address as Address,
  housePoolRedemptionMathSidecar: release.contracts.housePoolRedemptionMathSidecar.address as Address,
  housePool: release.contracts.housePool.address as Address,
  cfdEngineProtocolLens: release.contracts.cfdEngineProtocolLens.address as Address,
  seniorVault: release.contracts.seniorVault.address as Address,
  juniorVault: release.contracts.juniorVault.address as Address,
  cfdEngineAccountLens: release.contracts.cfdEngineAccountLens.address as Address,
  cfdEngineLens: release.contracts.cfdEngineLens.address as Address,
  cfdEngineOpenQuoter: release.contracts.cfdEngineOpenQuoter.address as Address,
  policyEvaluator: release.contracts.cfdOrderPolicyEvaluator.address as Address,
  cfdClosePreview: release.contracts.cfdClosePreview.address as Address,
  orderRouterExecutionSidecar: release.contracts.orderRouterExecutionSidecar.address as Address,
  orderRecoverySidecar: release.contracts.orderRecoverySidecar.address as Address,
  pletherOracle: release.contracts.pletherOracle.address as Address,
  orderRouterLiquidationBatchSidecar: release.contracts.orderRouterLiquidationBatchSidecar.address as Address,
  orderLifecycleBook: release.contracts.orderLifecycleBook.address as Address,
  orderRouter: release.contracts.orderRouter.address as Address,
  orderRouterAdmin: release.contracts.orderRouterAdmin.address as Address,
  positionProtectionBook: release.contracts.positionProtectionBook.address as Address,
  perpsPublicLens: release.contracts.perpsPublicLens.address as Address,
  settlementMonitorLens: release.contracts.settlementMonitorLens.address as Address,
  settlementMonitorLensSidecar: release.contracts.settlementMonitorLensSidecar.address as Address,
  emergencyPauseCoordinator: release.contracts.emergencyPauseCoordinator.address as Address,
} satisfies Record<string, Address>
