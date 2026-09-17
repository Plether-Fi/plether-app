import type { PerpsActionKind } from '@plether-fi/perps-aa-client'
import { type Address, type Hex, type PublicClient } from 'viem'
import { PERPS_CFD_ENGINE_ABI, PERPS_ORDER_LIFECYCLE_BOOK_ABI, PERPS_POSITION_PROTECTION_BOOK_ABI } from '../contracts/abis/Perps'
import type { PerpsAaDeploymentManifest } from './manifest'

export interface ReviewedActionStateInput {
  action: PerpsActionKind
  clientOrderId?: Hex
}

/** Capture stored position/protection state, not price-derived views that drift
 * each block. Batch estimation separately revalidates current execution policy. */
export async function readReviewedActionState(client: PublicClient, manifest: PerpsAaDeploymentManifest, account: Address, input: ReviewedActionStateInput): Promise<string> {
  const tracksPosition = ['place-order', 'place-protected-order', 'create-protection', 'replace-protection', 'cancel-protection', 'add-margin'].includes(input.action)
  if (!tracksPosition && !input.clientOrderId) return 'v1:no-position-binding'
  const blockNumber = await client.getBlockNumber({ cacheTime: 0 })
  const [position, protectionId, intent] = await Promise.all([
    tracksPosition ? client.readContract({ address: manifest.cfdEngine, abi: PERPS_CFD_ENGINE_ABI, functionName: 'positions', args: [account], blockNumber }) : undefined,
    tracksPosition ? client.readContract({ address: manifest.positionProtectionBook, abi: PERPS_POSITION_PROTECTION_BOOK_ABI, functionName: 'activePositionProtectionId', args: [account], blockNumber }) : undefined,
    input.clientOrderId ? client.readContract({ address: manifest.orderLifecycleBook, abi: PERPS_ORDER_LIFECYCLE_BOOK_ABI, functionName: 'clientIntent', args: [account, input.clientOrderId], blockNumber }) : undefined,
  ])
  if (intent && intent.orderId !== 0n) throw new Error('This client intent was already committed. Check order activity before a fresh review.')
  const protection = protectionId && protectionId !== 0n ? await client.readContract({ address: manifest.positionProtectionBook,
    abi: PERPS_POSITION_PROTECTION_BOOK_ABI, functionName: 'getPositionProtection', args: [protectionId], blockNumber }) : undefined
  return JSON.stringify({ version: 1, position, protectionId, protection, intent }, (_key, value: unknown) => typeof value === 'bigint' ? value.toString() : value)
}
