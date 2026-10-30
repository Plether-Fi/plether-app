import { isAddress, type Address, type Hex } from 'viem'

export interface SettlementTokenMetadata {
  name: string
  symbol: string
  decimals: 6
  isMock: boolean
  supportsReceiveWithAuthorization: boolean
  supportsBridgeFunding: boolean
}
export interface ContractPin { address: Address | null; runtimeCodeHash: Hex | null; artifact?: string }
export interface PerpsReleaseConfiguration {
  schemaVersion: 4
  isDeployed: boolean
  network: { chainId: number }
  release: { phase: string; deploymentBlock: number | null; pyth: Address; version: string | null }
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

/** The breaking interface only accepts canonical releases. Prepared pins stay null. */
export function parsePerpsReleaseConfiguration(value: unknown): PerpsReleaseConfiguration {
  const source = record(value, 'release manifest')
  if (source.schemaVersion !== 4) throw new Error('Perps requires canonical release schema 4')
  if (source.orderInterfaceVersion !== 3) throw new Error('Perps release requires order interface version 3')
  const network = record(source.network, 'network')
  if (network.chainId !== 421614) throw new Error('Perps release must use Arbitrum Sepolia')
  const release = record(source.release, 'release')
  const prepared = release.phase === 'prepared'
  if (!prepared && !['deployed', 'seeded', 'active'].includes(String(release.phase))) throw new Error('Invalid perps release phase')
  if (prepared ? release.deploymentBlock !== null : !Number.isSafeInteger(release.deploymentBlock) || Number(release.deploymentBlock) <= 0) throw new Error('Invalid perps deployment block')
  const contracts = Object.fromEntries<ContractPin>(Object.entries(record(source.contracts, 'contracts')).map(([key, value]): [string, ContractPin] => {
    const pin = record(value, key)
    const artifact = typeof pin.artifact === 'string' ? { artifact: pin.artifact } : {}
    if (prepared) {
      if (pin.address !== null || pin.runtimeCodeHash !== null) throw new Error(`Prepared perps ${key} must have null deployment pins`)
      return [key, { ...artifact, address: null, runtimeCodeHash: null }]
    }
    if (typeof pin.runtimeCodeHash !== 'string' || !/^0x[0-9a-f]{64}$/i.test(pin.runtimeCodeHash) || /^0x0{64}$/i.test(pin.runtimeCodeHash)) throw new Error(`Invalid perps ${key} runtime hash`)
    return [key, { ...artifact, address: address(pin.address, key), runtimeCodeHash: pin.runtimeCodeHash as Hex }]
  }))
  const expectedContracts = ['settlementToken', 'perpsPublicLens', 'marginClearinghouse', 'orderRouter', 'orderRouterAdmin', 'cfdEngine',
    'cfdEnginePlanner', 'cfdEngineSettlementSidecar', 'cfdEngineAdmin', 'housePool', 'seniorVault', 'juniorVault',
    'pletherOracle', 'cfdEngineLens', 'cfdEngineAccountLens', 'orderLifecycleBook', 'cfdOrderPolicyEvaluator', 'positionProtectionBook',
    'terminalNavBookV2', 'housePoolRedemptionMathSidecar', 'cfdEngineProtocolLens', 'cfdEngineOpenQuoter', 'cfdClosePreview',
    'orderRouterExecutionSidecar', 'orderRecoverySidecar', 'orderRouterLiquidationBatchSidecar', 'settlementMonitorLens',
    'settlementMonitorLensSidecar', 'emergencyPauseCoordinator']
  if (Object.keys(contracts).length !== expectedContracts.length) throw new Error('Perps contract graph has unsupported entries')
  for (const name of expectedContracts) if (!Object.hasOwn(contracts, name)) throw new Error(`Missing perps ${name} contract`)
  if (record(source.contracts, 'contracts').settlementToken === undefined || record(record(source.contracts, 'contracts').settlementToken, 'settlementToken').artifact !== 'script/DeployPerpsArbitrumSepolia.s.sol:MockUSDG') throw new Error('Schema 4 requires the MockUSDG artifact')
  const metadata = record(source.settlementToken, 'USDG token metadata')
  if (metadata.name !== 'Mock USDG' || metadata.symbol !== 'USDG' || metadata.decimals !== 6 ||
      metadata.isMock !== true || metadata.supportsReceiveWithAuthorization !== false || metadata.supportsBridgeFunding !== false) throw new Error('Invalid MockUSDG metadata or capabilities')
  return {
    schemaVersion: 4,
    isDeployed: !prepared,
    network: { chainId: network.chainId },
    release: { phase: String(release.phase), deploymentBlock: prepared ? null : Number(release.deploymentBlock), pyth: address(release.pyth, 'Pyth'), version: typeof release.version === 'string' ? release.version : null },
    contracts,
    settlementToken: metadata as unknown as SettlementTokenMetadata,
  }
}

export function settlementTokenContract(release: PerpsReleaseConfiguration): ContractPin {
  return release.contracts.settlementToken
}
