import { isAddress, isAddressEqual, keccak256, stringToHex, type Address, type Hex, type PublicClient } from 'viem'
import {
  PERPS_CFD_ENGINE_ABI,
  PERPS_ORDER_LIFECYCLE_BOOK_ABI,
  PERPS_ORDER_ROUTER_ABI,
  PERPS_POSITION_PROTECTION_BOOK_ABI,
  PERPS_PUBLIC_LENS_ABI,
} from './abis'
import { PERPS_ARBITRUM_SEPOLIA, PERPS_RELEASE as release, PERPS_RELEASE_IS_DEPLOYED } from './perpsAddresses'
import type { PerpsAaDeploymentManifest } from '../perps-aa/manifest'

export const ORDER_V3_INTENT_TYPEHASH = keccak256(stringToHex('PletherOrderIntentV3(uint256 chainId,address router,address account,bytes32 clientOrderId,uint8 side,uint256 sizeDelta,uint256 marginDelta,uint256 targetPrice,bool isClose,uint64 submitBy,uint32 executionWindowSeconds,uint8 allowedExecutionModes,bytes32 expectedConfigHash,uint256 maxExecutionBountyAssets,uint256 maxExecutionNotionalAssets,uint256 maxGrossAccountDebitAssets,uint256 maxActionChargeAssets,uint256 maxExplicitFeesAssets,uint256 maxPostPositionSize,uint256 minPostSettlementBalanceAssets,uint256 minPostPositionEquityAssets,uint32 maxPostLeverageBps)'))

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
): Promise<Address> {
  if (!PERPS_RELEASE_IS_DEPLOYED) throw new Error('The perps release is prepared and unavailable.')
  const pin = release.contracts.cfdClosePreview
  if (!pin.address || !isAddress(pin.address) || !pin.runtimeCodeHash) throw new Error('Close review deployment pins are missing.')
  if (manifest.chainId !== release.network.chainId || await client.getChainId() !== release.network.chainId) throw new Error('Close review requires Arbitrum Sepolia.')
  requireSameAddress('Close preview Engine', manifest.cfdEngine, PERPS_ARBITRUM_SEPOLIA.cfdEngine)
  requireSameAddress('Close preview Router', manifest.orderRouter, PERPS_ARBITRUM_SEPOLIA.orderRouter)
  requireSameAddress('Close preview execution evaluator', manifest.policyEvaluator, PERPS_ARBITRUM_SEPOLIA.policyEvaluator)
  if (isAddressEqual(pin.address, manifest.policyEvaluator)) {
    throw new Error('Close review is unavailable: preview aliases the execution evaluator.')
  }
  const code = await client.getCode({ address: pin.address, blockNumber })
  if (!code || code === '0x' || keccak256(code) !== pin.runtimeCodeHash.toLowerCase()) {
    throw new Error('Close review is unavailable: preview bytecode does not match the verified deployment.')
  }
  return pin.address
}

export async function verifyProtectionDeployment(client: PublicClient, manifest: PerpsAaDeploymentManifest, blockNumber?: bigint): Promise<void> {
  if (!PERPS_RELEASE_IS_DEPLOYED) throw new Error('The perps release is prepared and unavailable.')
  if (manifest.chainId !== release.network.chainId || await client.getChainId() !== release.network.chainId) throw new Error('TP/SL requires the reviewed Arbitrum Sepolia release')
  for (const [key, address] of [['positionProtectionBook', manifest.positionProtectionBook], ['orderRouter', manifest.orderRouter], ['orderLifecycleBook', manifest.orderLifecycleBook]] as const) {
    const contract = release.contracts[key]
    if (!contract.address || !contract.runtimeCodeHash) throw new Error('Protection deployment pins are missing.')
    requireSameAddress(`TP/SL ${key}`, address, contract.address)
    const code = await client.getCode({ address, blockNumber })
    if (!code || keccak256(code) !== contract.runtimeCodeHash.toLowerCase()) throw new Error(`TP/SL ${key} bytecode does not match ${release.release.version ?? 'the reviewed release'}`)
  }
}

/**
 * Verifies the immutable V3 graph at one coherent block. Any mismatch blocks
 * order preparation before a client intent is journaled or signed.
 */
export async function verifyPerpsV3DeploymentBindings(
  client: PublicClient,
  manifest: PerpsAaDeploymentManifest
): Promise<{
  positionProtectionBook: Address
  blockNumber: bigint
  block: { number: bigint; hash: Hex; timestamp: bigint }
}> {
  if (!PERPS_RELEASE_IS_DEPLOYED) throw new Error('The perps release is prepared and unavailable.')
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
    protectionRouter,
    intentTypehash,
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
    // The router's returned book is checked against this manifest address below,
    // so its reverse binding can be read in the same parallel group.
    client.readContract({
      address: manifest.positionProtectionBook,
      abi: PERPS_POSITION_PROTECTION_BOOK_ABI,
      functionName: 'ROUTER',
      blockNumber,
    }),
    client.readContract({ address: manifest.orderLifecycleBook, abi: PERPS_ORDER_LIFECYCLE_BOOK_ABI, functionName: 'INTENT_TYPEHASH', blockNumber }),
  ])

  if (intentTypehash !== ORDER_V3_INTENT_TYPEHASH) throw new Error('Lifecycle Book does not implement the V3 signed order interface')
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

  requireSameAddress(
    'Position-protection Router',
    protectionRouter,
    manifest.orderRouter
  )

  return { positionProtectionBook, blockNumber, block }
}
