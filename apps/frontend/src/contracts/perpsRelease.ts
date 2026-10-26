import { isAddress, type Address, type Hex } from 'viem'

export interface SettlementTokenMetadata {
  name: string
  symbol: string
  decimals: 6
  isMock: boolean
  supportsReceiveWithAuthorization: boolean
  supportsBridgeFunding: boolean
}

interface ContractPin { address: Address; runtimeCodeHash: Hex; artifact?: string }
export interface PerpsReleaseConfiguration {
  schemaVersion: number
  network: { chainId: number }
  release: { phase: string; deploymentBlock: number; pyth: Address }
  contracts: Record<string, ContractPin>
  settlementToken: SettlementTokenMetadata
}

function record(value: unknown, label: string): Record<string, unknown> {
  if (!value || typeof value !== 'object' || Array.isArray(value)) throw new Error(`Invalid perps ${label}`)
  return value as Record<string, unknown>
}
function address(value: unknown, label: string): Address {
  if (typeof value !== 'string' || !isAddress(value) || /^0x0{40}$/i.test(value)) throw new Error(`Invalid perps ${label} address`)
  return value
}

/** A prepared manifest cannot silently replace the active deployment. */
export function parsePerpsReleaseConfiguration(value: unknown): PerpsReleaseConfiguration {
  const source = record(value, 'release manifest')
  const schemaVersion = source.schemaVersion
  if (typeof schemaVersion !== 'number' || ![1, 2, 3, 4].includes(schemaVersion)) throw new Error('Unsupported perps release schema')
  const network = record(source.network, 'network')
  if (network.chainId !== 421614) throw new Error('Perps release must use Arbitrum Sepolia')
  const release = record(source.release, 'release')
  if (!['deployed', 'seeded', 'active'].includes(String(release.phase))) throw new Error('Perps release is not deployed')
  if (!Number.isSafeInteger(release.deploymentBlock) || Number(release.deploymentBlock) <= 0) throw new Error('Perps deployment block is missing')
  const contracts = Object.fromEntries(Object.entries(record(source.contracts, 'contracts')).map(([key, value]) => {
    const pin = record(value, key)
    if (typeof pin.runtimeCodeHash !== 'string' || !/^0x[0-9a-f]{64}$/i.test(pin.runtimeCodeHash) || /^0x0{64}$/i.test(pin.runtimeCodeHash)) throw new Error(`Invalid perps ${key} runtime hash`)
    return [key, { ...pin, address: address(pin.address, key), runtimeCodeHash: pin.runtimeCodeHash as Hex, ...(typeof pin.artifact === 'string' ? { artifact: pin.artifact } : {}) }] as const
  }))
  for (const name of ['perpsPublicLens', 'marginClearinghouse', 'orderRouter', 'orderRouterAdmin', 'cfdEngine',
    'cfdEnginePlanner', 'cfdEngineSettlementSidecar', 'cfdEngineAdmin', 'housePool', 'seniorVault', 'juniorVault',
    'pletherOracle', 'cfdEngineLens', 'cfdEngineAccountLens', 'orderLifecycleBook', 'cfdOrderPolicyEvaluator', 'positionProtectionBook']) {
    if (!Object.hasOwn(contracts, name)) throw new Error(`Missing perps ${name} contract`)
  }
  if (schemaVersion === 4) {
    if (source.orderInterfaceVersion !== 3) throw new Error('MockUSDG release requires order interface version 3')
    for (const name of ['terminalNavBookV2', 'housePoolRedemptionMathSidecar', 'cfdEngineProtocolLens', 'cfdEngineOpenQuoter',
      'cfdClosePreview', 'orderRouterExecutionSidecar', 'orderRecoverySidecar', 'orderRouterLiquidationBatchSidecar',
      'settlementMonitorLens', 'settlementMonitorLensSidecar', 'emergencyPauseCoordinator']) {
      if (!Object.hasOwn(contracts, name)) throw new Error(`Missing perps ${name} contract`)
    }
  }
  let settlementToken: SettlementTokenMetadata
  if (schemaVersion === 4) {
    if ('mockUsdc' in contracts) throw new Error('Schema 4 cannot contain the legacy mockUsdc contract')
    if (!Object.hasOwn(contracts, 'settlementToken') || contracts.settlementToken.artifact !== 'script/DeployPerpsArbitrumSepolia.s.sol:MockUSDG') throw new Error('Schema 4 requires the MockUSDG artifact')
    const metadata = record(source.settlementToken, 'settlement token metadata')
    if (metadata.name !== 'Mock USDG' || metadata.symbol !== 'USDG' || metadata.decimals !== 6 ||
        metadata.isMock !== true || metadata.supportsReceiveWithAuthorization !== false || metadata.supportsBridgeFunding !== false) {
      throw new Error('Invalid MockUSDG metadata or capabilities')
    }
    settlementToken = metadata as unknown as SettlementTokenMetadata
  } else {
    // Historical releases retain their actual USDC identity; metadata cannot relabel them.
    if (!Object.hasOwn(contracts, 'mockUsdc') || 'settlementToken' in contracts || source.settlementToken !== undefined) throw new Error('Historical release requires its mockUsdc contract')
    settlementToken = { name: 'Mock USDC', symbol: 'USDC', decimals: 6, isMock: true, supportsReceiveWithAuthorization: false, supportsBridgeFunding: false }
  }
  return {
    schemaVersion,
    network: { chainId: network.chainId },
    release: { phase: String(release.phase), deploymentBlock: Number(release.deploymentBlock), pyth: address(release.pyth, 'Pyth') },
    contracts,
    settlementToken,
  }
}

export function settlementTokenContract(release: PerpsReleaseConfiguration): ContractPin {
  return release.contracts[release.schemaVersion === 4 ? 'settlementToken' : 'mockUsdc']
}
