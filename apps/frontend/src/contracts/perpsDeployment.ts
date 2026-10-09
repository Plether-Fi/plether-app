import { isAddress, isAddressEqual, zeroAddress, type Address, type Hex } from 'viem'

export const PERPS_CONTRACT_KEYS = [
  'pyth', 'usdc', 'perpsPublicLens', 'marginClearinghouse', 'orderRouter',
  'orderRouterAdmin', 'cfdEngine', 'cfdEnginePlanner', 'cfdEngineSettlementSidecar',
  'cfdEngineAdmin', 'housePool', 'seniorVault', 'juniorVault', 'pletherOracle',
  'cfdEngineLens', 'cfdEngineAccountLens', 'orderLifecycleBook', 'policyEvaluator',
  'positionProtectionBook',
] as const

export type PerpsContractAddresses = Record<typeof PERPS_CONTRACT_KEYS[number], Address>
export type PerpsChainId = 42161 | 421614
const PROTECTION_CODE_HASH_KEYS = ['orderRouter', 'orderLifecycleBook', 'positionProtectionBook'] as const

export interface PerpsDeployment {
  schemaVersion: 1
  releaseId: string
  chainId: PerpsChainId
  deploymentBlock: number
  contracts: PerpsContractAddresses
  runtimeCodeHashes: Record<typeof PROTECTION_CODE_HASH_KEYS[number], Hex>
  closePreview: {
    address: Address
    runtimeCodeHash: Hex
    chainId: PerpsChainId
    cfdEngine: Address
    orderRouter: Address
    policyEvaluator: Address
  }
}

const MANIFEST_BINDING_KEYS = [
  'usdc', 'marginClearinghouse', 'cfdEngine', 'orderRouter', 'orderLifecycleBook',
  'positionProtectionBook', 'policyEvaluator',
] as const
export type PerpsDeploymentManifestBindings = { chainId: number }
  & Pick<PerpsContractAddresses, typeof MANIFEST_BINDING_KEYS[number]>

/** Keep AA identity, ordinary reads, and transaction targets on one reviewed graph. */
export function isPerpsManifestForDeployment(
  manifest: PerpsDeploymentManifestBindings,
  deployment: PerpsDeployment,
): boolean {
  return manifest.chainId === deployment.chainId
    && MANIFEST_BINDING_KEYS.every(key => isAddress(manifest[key]) && isAddressEqual(manifest[key], deployment.contracts[key]))
}

function invalid(message: string): never {
  throw new Error(`Invalid perps deployment configuration: ${message}`)
}

function object(value: unknown, field: string, keys: readonly string[]): Record<string, unknown> {
  if (!value || typeof value !== 'object' || Array.isArray(value)) invalid(`${field} must be an object`)
  const result = value as Record<string, unknown>
  if (Object.keys(result).length !== keys.length || Object.keys(result).some(key => !keys.includes(key))) {
    invalid(`${field} must contain exactly ${keys.join(', ')}`)
  }
  return result
}

function address(value: unknown, field: string): Address {
  if (typeof value !== 'string' || !isAddress(value) || isAddressEqual(value, zeroAddress)) {
    invalid(`${field} must be a nonzero address`)
  }
  return value
}

function codeHash(value: unknown, field: string): Hex {
  if (typeof value !== 'string' || !/^0x[0-9a-f]{64}$/i.test(value) || /^0x0{64}$/i.test(value)) {
    invalid(`${field} must be a nonzero runtime code hash`)
  }
  return value.toLowerCase() as Hex
}

/** Complete build-owned release configuration. No individual address overrides or fallbacks. */
export function parsePerpsDeployment(value: unknown): PerpsDeployment {
  const config = object(value, 'deployment', [
    'schemaVersion', 'releaseId', 'chainId', 'deploymentBlock', 'contracts', 'runtimeCodeHashes', 'closePreview',
  ])
  if (config.schemaVersion !== 1) invalid('schemaVersion must equal 1')
  if (typeof config.releaseId !== 'string' || !/^[a-zA-Z0-9][a-zA-Z0-9._-]{0,127}$/.test(config.releaseId)) {
    invalid('releaseId must be a nonempty release identifier')
  }
  if (config.chainId !== 42161 && config.chainId !== 421614) invalid('chainId must be 42161 or 421614')
  if (typeof config.deploymentBlock !== 'number' || !Number.isSafeInteger(config.deploymentBlock) || config.deploymentBlock < 1) {
    invalid('deploymentBlock must be a positive safe integer')
  }
  const rawContracts = object(config.contracts, 'contracts', PERPS_CONTRACT_KEYS)
  const contracts = {} as PerpsContractAddresses
  for (const key of PERPS_CONTRACT_KEYS) contracts[key] = address(rawContracts[key], `contracts.${key}`)
  const rawHashes = object(config.runtimeCodeHashes, 'runtimeCodeHashes', PROTECTION_CODE_HASH_KEYS)
  const runtimeCodeHashes = {} as PerpsDeployment['runtimeCodeHashes']
  for (const key of PROTECTION_CODE_HASH_KEYS) runtimeCodeHashes[key] = codeHash(rawHashes[key], `runtimeCodeHashes.${key}`)
  const rawPreview = object(config.closePreview, 'closePreview', [
    'address', 'runtimeCodeHash', 'chainId', 'cfdEngine', 'orderRouter', 'policyEvaluator',
  ])
  if (rawPreview.chainId !== config.chainId) invalid('closePreview.chainId must match the deployment')
  const closePreview: PerpsDeployment['closePreview'] = {
    address: address(rawPreview.address, 'closePreview.address'),
    runtimeCodeHash: codeHash(rawPreview.runtimeCodeHash, 'closePreview.runtimeCodeHash'),
    chainId: config.chainId,
    cfdEngine: address(rawPreview.cfdEngine, 'closePreview.cfdEngine'),
    orderRouter: address(rawPreview.orderRouter, 'closePreview.orderRouter'),
    policyEvaluator: address(rawPreview.policyEvaluator, 'closePreview.policyEvaluator'),
  }
  for (const key of ['cfdEngine', 'orderRouter', 'policyEvaluator'] as const) {
    if (!isAddressEqual(closePreview[key], contracts[key])) invalid(`closePreview.${key} must match the deployment`)
  }
  if (isAddressEqual(closePreview.address, contracts.policyEvaluator)) {
    invalid('closePreview must not alias the execution evaluator')
  }
  return {
    schemaVersion: 1,
    releaseId: config.releaseId,
    chainId: config.chainId,
    deploymentBlock: config.deploymentBlock,
    contracts,
    runtimeCodeHashes,
    closePreview,
  }
}

/** An unset variable preserves the shipped release; malformed or partial overrides fail closed. */
export function resolvePerpsDeployment(value: unknown, defaultDeployment: PerpsDeployment): PerpsDeployment {
  if (value === undefined) return defaultDeployment
  return parsePerpsDeploymentJson(value)
}

/** Also usable by the build pipeline before compiling the frontend. */
export function parsePerpsDeploymentJson(value: unknown): PerpsDeployment {
  if (typeof value !== 'string' || !value.trim()) invalid('VITE_PERPS_DEPLOYMENT_JSON must be a complete JSON object')
  let parsed: unknown
  try {
    parsed = JSON.parse(value)
  } catch {
    invalid('VITE_PERPS_DEPLOYMENT_JSON is not valid JSON')
  }
  return parsePerpsDeployment(parsed)
}
