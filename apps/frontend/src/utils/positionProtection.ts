import { formatUnits } from 'viem'
import { protectionParamsFromInputs, type ProtectionDraft } from '../contracts/positionProtection'
import type { PerpsDirection } from './perps'

export interface ProtectionPriceContext {
  direction: PerpsDirection
  rawMark?: bigint
  cap?: bigint
}

export function protectionPrice(rawPrice?: bigint, cap?: bigint): string {
  if (rawPrice === undefined || rawPrice <= 0n || cap === undefined || rawPrice >= cap) return '—'
  const [whole, decimals = ''] = formatUnits(cap - rawPrice, 8).split('.')
  return `${whole}.${decimals.padEnd(4, '0')}`
}

export function protectionDistance(rawPrice: bigint, rawMark?: bigint, cap?: bigint): string | undefined {
  if (!rawPrice || rawMark === undefined || cap === undefined || cap <= rawMark) return undefined
  const change = rawMark - rawPrice
  const percent = (change < 0n ? -change : change) * 10_000n / (cap - rawMark)
  return `${change < 0n ? '−' : '+'}${(percent / 100n).toString()}.${(percent % 100n).toString().padStart(2, '0')}% from current price`
}

export function convertProtectionInputMode(draft: ProtectionDraft, mode: ProtectionDraft['mode'], context: ProtectionPriceContext): ProtectionDraft {
  if (draft.mode === mode) return draft
  if (!draft.takeProfit && !draft.stopLoss) return { ...draft, mode }
  const params = protectionParamsFromInputs({ ...draft, ...context, rawMark: context.rawMark ?? 0n, cap: context.cap ?? 0n })
  const cap = context.cap ?? 0n
  const displayedMark = cap - (context.rawMark ?? 0n)
  const convert = (raw: bigint) => {
    if (!raw) return ''
    if (mode === 'price') return formatUnits(cap - raw, 8)
    const delta = cap - raw - displayedMark
    const percent = (delta < 0n ? -delta : delta) * 1_000_000n / displayedMark
    if (!percent) throw new Error('This price change is too small to express as a percentage. Keep price input.')
    return formatUnits(percent, 4)
  }
  const converted = { mode, takeProfit: convert(params.takeProfitTriggerPrice), stopLoss: convert(params.stopLossTriggerPrice) }
  const roundTrip = protectionParamsFromInputs({ ...converted, direction: context.direction, rawMark: context.rawMark ?? 0n, cap })
  if (roundTrip.takeProfitTriggerPrice !== params.takeProfitTriggerPrice || roundTrip.stopLossTriggerPrice !== params.stopLossTriggerPrice) {
    throw new Error('These prices need more precision than % change supports. Keep price input to preserve them.')
  }
  return converted
}

export function protectionStatusLabel(status?: number): string {
  switch (status) {
    case 1: return 'Waiting for position'
    case 2: return 'Active'
    case 3: return 'Close queued'
    case 4: return 'Closed'
    case 5: return 'Not completed'
    case 6: return 'Removed'
    case 7: return 'Liquidated'
    case 8: return 'Close delayed'
    case 0: case undefined: return 'Not set'
    default: return 'Unknown state'
  }
}

export function protectionStateDescription(status: number): string {
  switch (status) {
    case 1: return 'Triggers are waiting for the opening order to fill. They are not active yet.'
    case 2: return 'The first eligible trigger queues a full-position close. The other trigger is then cancelled.'
    case 3: return 'A trigger was reached and a close order was queued. Execution has not completed yet.'
    case 4: return 'This TP/SL closed the protected position. Neither trigger remains active.'
    case 5: return 'This protection ended without a successful TP/SL close. Check your position and the failure details below.'
    case 6: return 'TP/SL was removed. Removing it did not close a position or cancel its opening order.'
    case 7: return 'The protected position was liquidated. These TP/SL triggers are no longer active.'
    case 8: return 'A close attempt failed, but the original trigger still applies. The protection has not finished.'
    default: return 'No automatic exit triggers are active for this record.'
  }
}
