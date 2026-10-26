import { zeroAddress, type Address } from 'viem'
import release from '../../../../config/perps/mock-usdg-prepared.json'
import { parsePerpsReleaseConfiguration, settlementTokenContract } from './perpsRelease'

export const PERPS_RELEASE = parsePerpsReleaseConfiguration(release)
export const PERPS_RELEASE_IS_DEPLOYED = PERPS_RELEASE.isDeployed
export const PERPS_SETTLEMENT_TOKEN = PERPS_RELEASE.settlementToken
export const PERPS_SETTLEMENT_TOKEN_LABEL = PERPS_SETTLEMENT_TOKEN.symbol
export const PERPS_SETTLEMENT_TOKEN_CONTRACT = settlementTokenContract(PERPS_RELEASE)

// Null prepared pins use a non-deployment sentinel only inside disabled hooks.
// Public configuration keeps nulls; no request or transaction may use this graph
// until PERPS_RELEASE_IS_DEPLOYED becomes true after verified activation.
function contractAddress(name: string): Address { return PERPS_RELEASE.contracts[name].address ?? zeroAddress }
export const PERPS_CLOSE_PREVIEW_ADDRESS = contractAddress('cfdClosePreview')

export const PERPS_ARBITRUM_SEPOLIA_CHAIN_ID = 421614 as const
export const PERPS_ARBITRUM_SEPOLIA_DEPLOYMENT_BLOCK = PERPS_RELEASE.release.deploymentBlock ?? 0

export type PerpsContractAddresses = typeof PERPS_ARBITRUM_SEPOLIA

export const PERPS_ARBITRUM_SEPOLIA = {
  pyth: PERPS_RELEASE.release.pyth,
  settlementToken: contractAddress('settlementToken'),
  marginClearinghouse: contractAddress('marginClearinghouse'),
  cfdEngine: contractAddress('cfdEngine'),
  terminalNavBookV2: contractAddress('terminalNavBookV2'),
  cfdEnginePlanner: contractAddress('cfdEnginePlanner'),
  cfdEngineSettlementSidecar: contractAddress('cfdEngineSettlementSidecar'),
  cfdEngineAdmin: contractAddress('cfdEngineAdmin'),
  housePoolRedemptionMathSidecar: contractAddress('housePoolRedemptionMathSidecar'),
  housePool: contractAddress('housePool'),
  cfdEngineProtocolLens: contractAddress('cfdEngineProtocolLens'),
  seniorVault: contractAddress('seniorVault'),
  juniorVault: contractAddress('juniorVault'),
  cfdEngineAccountLens: contractAddress('cfdEngineAccountLens'),
  cfdEngineLens: contractAddress('cfdEngineLens'),
  cfdEngineOpenQuoter: contractAddress('cfdEngineOpenQuoter'),
  policyEvaluator: contractAddress('cfdOrderPolicyEvaluator'),
  cfdClosePreview: contractAddress('cfdClosePreview'),
  orderRouterExecutionSidecar: contractAddress('orderRouterExecutionSidecar'),
  orderRecoverySidecar: contractAddress('orderRecoverySidecar'),
  pletherOracle: contractAddress('pletherOracle'),
  orderRouterLiquidationBatchSidecar: contractAddress('orderRouterLiquidationBatchSidecar'),
  orderLifecycleBook: contractAddress('orderLifecycleBook'),
  orderRouter: contractAddress('orderRouter'),
  orderRouterAdmin: contractAddress('orderRouterAdmin'),
  positionProtectionBook: contractAddress('positionProtectionBook'),
  perpsPublicLens: contractAddress('perpsPublicLens'),
  settlementMonitorLens: contractAddress('settlementMonitorLens'),
  settlementMonitorLensSidecar: contractAddress('settlementMonitorLensSidecar'),
  emergencyPauseCoordinator: contractAddress('emergencyPauseCoordinator'),
} satisfies Record<string, Address>
