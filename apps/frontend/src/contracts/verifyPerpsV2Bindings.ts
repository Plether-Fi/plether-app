import { isAddress, isAddressEqual, keccak256, type Address, type Hex, type PublicClient } from 'viem'
import {
  PERPS_CFD_ENGINE_ABI,
  PERPS_ORDER_LIFECYCLE_BOOK_ABI,
  PERPS_ORDER_ROUTER_ABI,
  PERPS_POSITION_PROTECTION_BOOK_ABI,
  PERPS_PUBLIC_LENS_ABI,
} from './abis'
import { PERPS_ACTIVE_DEPLOYMENT, PERPS_CONTRACTS } from './perpsAddresses'
import { isPerpsManifestForDeployment, type PerpsDeployment } from './perpsDeployment'
import type { PerpsAaDeploymentManifest } from '../perps-aa/manifest'

function requireSameAddress(
  label: string,
  actual: Address,
  expected: Address
): void {
  if (!isAddressEqual(actual, expected)) {
    throw new Error(
      `${label} binding mismatch: expected ${expected}, received ${actual}`
    )
  }
}

/** Call only for prospective closes, after the existing graph verification. */
export async function verifyClosePreviewDeployment(
  client: PublicClient,
  manifest: PerpsAaDeploymentManifest,
  blockNumber: bigint,
  deployment: PerpsDeployment = PERPS_ACTIVE_DEPLOYMENT,
): Promise<Address> {
  const pin = deployment.closePreview
  if (!isAddress(pin.address) || !/^0x[0-9a-f]{64}$/i.test(pin.runtimeCodeHash)) {
    throw new Error('Close review is unavailable: preview deployment configuration is invalid.')
  }
  if (manifest.chainId !== deployment.chainId || pin.chainId !== deployment.chainId || await client.getChainId() !== deployment.chainId) {
    throw new Error(`Close review requires the active perps deployment on chain ${String(deployment.chainId)}.`)
  }
  requireSameAddress('Close preview Engine', manifest.cfdEngine, pin.cfdEngine)
  requireSameAddress('Close preview Router', manifest.orderRouter, pin.orderRouter)
  requireSameAddress('Close preview execution evaluator', manifest.policyEvaluator, pin.policyEvaluator)
  if (isAddressEqual(pin.address, manifest.policyEvaluator)) {
    throw new Error('Close review is unavailable: preview aliases the execution evaluator.')
  }
  const code = await client.getCode({ address: pin.address, blockNumber })
  if (!code || code === '0x' || keccak256(code) !== pin.runtimeCodeHash.toLowerCase()) {
    throw new Error('Close review is unavailable: preview bytecode does not match the verified deployment.')
  }
  return pin.address
}

export async function verifyProtectionDeployment(
  client: PublicClient, manifest: PerpsAaDeploymentManifest, blockNumber?: bigint,
  deployment: PerpsDeployment = PERPS_ACTIVE_DEPLOYMENT,
): Promise<void> {
  if (!isPerpsManifestForDeployment(manifest, deployment) || await client.getChainId() !== deployment.chainId) {
    throw new Error('TP/SL requires the active reviewed perps deployment')
  }
  for (const [key, address] of [['positionProtectionBook', manifest.positionProtectionBook], ['orderRouter', manifest.orderRouter], ['orderLifecycleBook', manifest.orderLifecycleBook]] as const) {
    requireSameAddress(`TP/SL ${key}`, address, deployment.contracts[key])
    const code = await client.getCode({ address, blockNumber })
    if (!code || keccak256(code) !== deployment.runtimeCodeHashes[key].toLowerCase()) throw new Error(`TP/SL ${key} bytecode does not match ${deployment.releaseId}`)
  }
}

/**
 * Verifies the immutable V2 graph at one coherent block. Any mismatch blocks
 * order preparation before a client intent is journaled or signed.
 */
export async function verifyPerpsV2DeploymentBindings(
  client: PublicClient,
  manifest: PerpsAaDeploymentManifest
): Promise<{
  positionProtectionBook: Address
  blockNumber: bigint
  block: { number: bigint; hash: Hex; timestamp: bigint }
}> {
  if (!isPerpsManifestForDeployment(manifest, PERPS_ACTIVE_DEPLOYMENT)) {
    throw new Error('Perps manifest does not match the active reviewed deployment')
  }
  const [block, chainId] = await Promise.all([client.getBlock({ blockTag: 'latest' }), client.getChainId()])
  if (chainId !== PERPS_ACTIVE_DEPLOYMENT.chainId) throw new Error('Perps RPC chain does not match the active deployment')
  const blockNumber = block.number
  const [
    routerEngine,
    routerLifecycleBook,
    routerPolicyEvaluator,
    positionProtectionBook,
    lifecycleRouter,
    lifecycleEngine,
    lifecycleClearinghouse,
    lifecycleHousePool,
    engineClearinghouse,
    enginePool,
    lensEngine,
    lensRouter,
    lensHousePool,
    protectionRouter,
  ] = await Promise.all([
    client.readContract({
      address: manifest.orderRouter,
      abi: PERPS_ORDER_ROUTER_ABI,
      functionName: 'engine',
      blockNumber,
    }),
    client.readContract({
      address: manifest.orderRouter,
      abi: PERPS_ORDER_ROUTER_ABI,
      functionName: 'lifecycleBook',
      blockNumber,
    }),
    client.readContract({
      address: manifest.orderRouter,
      abi: PERPS_ORDER_ROUTER_ABI,
      functionName: 'policyEvaluator',
      blockNumber,
    }),
    client.readContract({
      address: manifest.orderRouter,
      abi: PERPS_ORDER_ROUTER_ABI,
      functionName: 'positionProtectionBook',
      blockNumber,
    }),
    client.readContract({
      address: manifest.orderLifecycleBook,
      abi: PERPS_ORDER_LIFECYCLE_BOOK_ABI,
      functionName: 'ROUTER',
      blockNumber,
    }),
    client.readContract({
      address: manifest.orderLifecycleBook,
      abi: PERPS_ORDER_LIFECYCLE_BOOK_ABI,
      functionName: 'ENGINE',
      blockNumber,
    }),
    client.readContract({
      address: manifest.orderLifecycleBook,
      abi: PERPS_ORDER_LIFECYCLE_BOOK_ABI,
      functionName: 'CLEARINGHOUSE',
      blockNumber,
    }),
    client.readContract({
      address: manifest.orderLifecycleBook,
      abi: PERPS_ORDER_LIFECYCLE_BOOK_ABI,
      functionName: 'HOUSE_POOL',
      blockNumber,
    }),
    client.readContract({
      address: manifest.cfdEngine,
      abi: PERPS_CFD_ENGINE_ABI,
      functionName: 'clearinghouse',
      blockNumber,
    }),
    client.readContract({
      address: manifest.cfdEngine,
      abi: PERPS_CFD_ENGINE_ABI,
      functionName: 'pool',
      blockNumber,
    }),
    client.readContract({
      address: PERPS_CONTRACTS.perpsPublicLens,
      abi: PERPS_PUBLIC_LENS_ABI,
      functionName: 'ENGINE',
      blockNumber,
    }),
    client.readContract({
      address: PERPS_CONTRACTS.perpsPublicLens,
      abi: PERPS_PUBLIC_LENS_ABI,
      functionName: 'ORDER_ROUTER',
      blockNumber,
    }),
    client.readContract({
      address: PERPS_CONTRACTS.perpsPublicLens,
      abi: PERPS_PUBLIC_LENS_ABI,
      functionName: 'HOUSE_POOL',
      blockNumber,
    }),
    // The router's returned book is checked against this manifest address below,
    // so its reverse binding can be read in the same parallel group.
    client.readContract({
      address: manifest.positionProtectionBook,
      abi: PERPS_POSITION_PROTECTION_BOOK_ABI,
      functionName: 'ROUTER',
      blockNumber,
    }),
  ])

  requireSameAddress('Router Engine', routerEngine, manifest.cfdEngine)
  requireSameAddress(
    'Router lifecycle Book',
    routerLifecycleBook,
    manifest.orderLifecycleBook
  )
  requireSameAddress(
    'Router policy evaluator',
    routerPolicyEvaluator,
    manifest.policyEvaluator
  )
  requireSameAddress(
    'Router position-protection Book',
    positionProtectionBook,
    manifest.positionProtectionBook
  )
  requireSameAddress('Lifecycle Router', lifecycleRouter, manifest.orderRouter)
  requireSameAddress('Lifecycle Engine', lifecycleEngine, manifest.cfdEngine)
  requireSameAddress(
    'Lifecycle Clearinghouse',
    lifecycleClearinghouse,
    manifest.marginClearinghouse
  )
  requireSameAddress(
    'Lifecycle HousePool',
    lifecycleHousePool,
    PERPS_CONTRACTS.housePool
  )
  requireSameAddress(
    'Engine Clearinghouse',
    engineClearinghouse,
    manifest.marginClearinghouse
  )
  requireSameAddress('Engine Pool', enginePool, PERPS_CONTRACTS.housePool)
  requireSameAddress('Public lens Engine', lensEngine, manifest.cfdEngine)
  requireSameAddress('Public lens Router', lensRouter, manifest.orderRouter)
  requireSameAddress(
    'Public lens HousePool',
    lensHousePool,
    PERPS_CONTRACTS.housePool
  )

  requireSameAddress(
    'Position-protection Router',
    protectionRouter,
    manifest.orderRouter
  )

  return { positionProtectionBook, blockNumber, block }
}
