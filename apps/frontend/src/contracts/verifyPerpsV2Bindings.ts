import { isAddressEqual, type Address, type PublicClient } from 'viem'
import {
  PERPS_CFD_ENGINE_ABI,
  PERPS_ORDER_LIFECYCLE_BOOK_ABI,
  PERPS_ORDER_ROUTER_ABI,
  PERPS_POSITION_PROTECTION_BOOK_ABI,
  PERPS_PUBLIC_LENS_ABI,
} from './abis'
import { PERPS_ARBITRUM_SEPOLIA } from './perpsAddresses'
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

/**
 * Verifies the immutable V2 graph at one coherent block. Any mismatch blocks
 * order preparation before a client intent is journaled or signed.
 */
export async function verifyPerpsV2DeploymentBindings(
  client: PublicClient,
  manifest: PerpsAaDeploymentManifest
): Promise<{ positionProtectionBook: Address; blockNumber: bigint }> {
  if (!manifest.orderLifecycleBook || !manifest.policyEvaluator) {
    throw new Error(
      'Bounded V2 orders require a reviewed perps-aa v2 deployment manifest'
    )
  }

  const block = await client.getBlock({ blockTag: 'latest' })
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
      address: PERPS_ARBITRUM_SEPOLIA.perpsPublicLens,
      abi: PERPS_PUBLIC_LENS_ABI,
      functionName: 'ENGINE',
      blockNumber,
    }),
    client.readContract({
      address: PERPS_ARBITRUM_SEPOLIA.perpsPublicLens,
      abi: PERPS_PUBLIC_LENS_ABI,
      functionName: 'ORDER_ROUTER',
      blockNumber,
    }),
    client.readContract({
      address: PERPS_ARBITRUM_SEPOLIA.perpsPublicLens,
      abi: PERPS_PUBLIC_LENS_ABI,
      functionName: 'HOUSE_POOL',
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
    PERPS_ARBITRUM_SEPOLIA.housePool
  )
  requireSameAddress(
    'Engine Clearinghouse',
    engineClearinghouse,
    manifest.marginClearinghouse
  )
  requireSameAddress('Engine Pool', enginePool, PERPS_ARBITRUM_SEPOLIA.housePool)
  requireSameAddress('Public lens Engine', lensEngine, manifest.cfdEngine)
  requireSameAddress('Public lens Router', lensRouter, manifest.orderRouter)
  requireSameAddress(
    'Public lens HousePool',
    lensHousePool,
    PERPS_ARBITRUM_SEPOLIA.housePool
  )

  const protectionRouter = await client.readContract({
    address: positionProtectionBook,
    abi: PERPS_POSITION_PROTECTION_BOOK_ABI,
    functionName: 'ROUTER',
    blockNumber,
  })
  requireSameAddress(
    'Position-protection Router',
    protectionRouter,
    manifest.orderRouter
  )

  return { positionProtectionBook, blockNumber }
}
