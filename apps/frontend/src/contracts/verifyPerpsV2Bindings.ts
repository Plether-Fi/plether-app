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
import { PERPS_CONFIG_SCHEMA_HASH, PERPS_RECEIPT_TYPEHASH } from './perpsProtection'

export async function verifyPerpsReceiptSchema(client: PublicClient, book: Address, blockNumber: bigint): Promise<void> {
  const [configSchema, receiptType] = await Promise.all([
    client.readContract({ address: book, abi: PERPS_ORDER_LIFECYCLE_BOOK_ABI, functionName: 'CONFIG_SCHEMA_HASH', blockNumber }),
    client.readContract({ address: book, abi: PERPS_ORDER_LIFECYCLE_BOOK_ABI, functionName: 'RECEIPT_TYPEHASH', blockNumber }),
  ])
  if (configSchema !== PERPS_CONFIG_SCHEMA_HASH || receiptType !== PERPS_RECEIPT_TYPEHASH) {
    throw new Error('This deployment does not support V3 protection receipts. A complete perps stack deployment is required.')
  }
}

export async function verifyProtectionRetryBindings(client: PublicClient, blockNumber: bigint): Promise<void> {
  const addresses = PERPS_ARBITRUM_SEPOLIA
  const [book, lifecycle, router, engine] = await Promise.all([
    client.readContract({ address: addresses.orderRouter, abi: PERPS_ORDER_ROUTER_ABI, functionName: 'positionProtectionBook', blockNumber }),
    client.readContract({ address: addresses.orderRouter, abi: PERPS_ORDER_ROUTER_ABI, functionName: 'lifecycleBook', blockNumber }),
    client.readContract({ address: addresses.positionProtectionBook, abi: PERPS_POSITION_PROTECTION_BOOK_ABI, functionName: 'ROUTER', blockNumber }),
    client.readContract({ address: addresses.positionProtectionBook, abi: PERPS_POSITION_PROTECTION_BOOK_ABI, functionName: 'ENGINE', blockNumber }),
  ])
  requireSameAddress('Router protection Book', book, addresses.positionProtectionBook)
  requireSameAddress('Router lifecycle Book', lifecycle, addresses.orderLifecycleBook)
  requireSameAddress('Protection Router', router, addresses.orderRouter)
  requireSameAddress('Protection Engine', engine, addresses.cfdEngine)
  await verifyPerpsReceiptSchema(client, lifecycle, blockNumber)
}

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
  const block = await client.getBlock({ blockTag: 'latest' })
  const blockNumber = block.number
  await verifyPerpsReceiptSchema(client, manifest.orderLifecycleBook, blockNumber)
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
  requireSameAddress(
    'Router position-protection Book',
    positionProtectionBook,
    PERPS_ARBITRUM_SEPOLIA.positionProtectionBook
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
