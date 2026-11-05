import { formatUnits } from 'viem'
import type { ProtectionDraft } from '../contracts/positionProtection'
import type { SponsoredOperation } from './operationStore'

/** Editable inputs only. Never reuse old calldata, quotes, deadlines or signatures. */
export interface SavedOrderDraft {
  version: 1
  direction: 'long' | 'short'
  orderQuantity: string
  leverage: number
  slippage: number
  reduceOnly: boolean
  fullClose: boolean
  maxOpen: boolean
  protectionEnabled: boolean
  protection: ProtectionDraft
}

export function restoredOrderDraft(operation: SponsoredOperation): { draft: Partial<SavedOrderDraft>; incomplete: boolean } {
  const saved: Partial<SavedOrderDraft> | undefined = operation.orderDraft
  if (saved?.version === 1 && (saved.direction === 'long' || saved.direction === 'short')
    && typeof saved.orderQuantity === 'string' && saved.orderQuantity.length <= 100
    && typeof saved.leverage === 'number' && Number.isFinite(saved.leverage) && saved.leverage > 0
    && typeof saved.slippage === 'number' && Number.isFinite(saved.slippage) && saved.slippage >= 0
    && [saved.reduceOnly, saved.fullClose, saved.maxOpen, saved.protectionEnabled].every(value => typeof value === 'boolean')
    && saved.protection && ['price', 'percent'].includes(saved.protection.mode)
    && typeof saved.protection.takeProfit === 'string' && typeof saved.protection.stopLoss === 'string') {
    return { draft: saved, incomplete: false }
  }
  const order = operation.orderRequestV3
  return { draft: order ? {
    direction: order.side === 0 ? 'long' : order.side === 1 ? 'short' : undefined,
    orderQuantity: /^\d+$/.test(order.sizeDelta) ? formatUnits(BigInt(order.sizeDelta), 18) : '',
    reduceOnly: order.isClose,
  } : {}, incomplete: true }
}
