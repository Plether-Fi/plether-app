import { parseAbi, parseUnits, type Address } from 'viem'
import type { PositionProtectionParams } from '@plether/perps-aa-client'
import type { PerpsDirection } from '../utils/perps'

export type { PositionProtectionParams }
export interface ProtectionDraft { mode: 'price' | 'percent'; takeProfit: string; stopLoss: string }
export const EMPTY_PROTECTION_DRAFT: ProtectionDraft = { mode: 'price', takeProfit: '', stopLoss: '' }
export const PROTECTION_STATUS = ['None', 'PendingOpen', 'Armed', 'Triggered', 'Executed', 'Failed', 'Cancelled', 'Liquidated', 'Latched'] as const
export const PROTECTION_LEG = ['None', 'TakeProfit', 'StopLoss'] as const
export const PROTECTION_CONFIG_ABI = parseAbi([
  'function positionProtectionTriggerBountyUsdc() view returns (uint256)',
  'function closeOrderExecutionBountyUsdc() view returns (uint256)',
])
export const PROTECTION_RELEASE_ENABLED = import.meta.env.VITE_PERPS_POSITION_PROTECTION_ENABLED === 'true'

export interface PositionProtection extends PositionProtectionParams {
  protectionId: bigint
  parentOrderId: bigint
  linkedOrderId: bigint
  account: Address
  side: number
  size: bigint
  triggerBountyUsdc: bigint
  executionBountyUsdc: bigint
  armedAt: bigint
  armedBlock: bigint
  triggerMarkPrice: bigint
  triggerPublishTime: bigint
  triggeredLeg: number
  status: number
}

export interface PersistedProtectionIntent {
  version: 1
  book: Address
  protectionId?: string
  takeProfitTriggerPrice: string
  stopLossTriggerPrice: string
}

export function persistProtectionIntent(book: Address, params: PositionProtectionParams, protectionId?: bigint): PersistedProtectionIntent {
  return { version: 1, book, protectionId: protectionId?.toString(), takeProfitTriggerPrice: params.takeProfitTriggerPrice.toString(), stopLossTriggerPrice: params.stopLossTriggerPrice.toString() }
}

export function validateProtectionParams(params: PositionProtectionParams, direction: PerpsDirection, rawMark: bigint, cap: bigint): void {
  const { takeProfitTriggerPrice: tp, stopLossTriggerPrice: sl } = params
  if (cap <= 0n || rawMark <= 0n || rawMark >= cap) throw new Error('A valid current market price is required')
  if (tp === 0n && sl === 0n) throw new Error('Enter a take-profit or stop-loss trigger')
  for (const price of [tp, sl]) if (price < 0n || price >= cap) throw new Error('Trigger price must be between zero and the price cap')
  if (tp !== 0n && (direction === 'long' ? tp >= rawMark : tp <= rawMark)) {
    throw new Error(`Take profit must be ${direction === 'long' ? 'above' : 'below'} the current displayed price`)
  }
  if (sl !== 0n && (direction === 'long' ? sl <= rawMark : sl >= rawMark)) {
    throw new Error(`Stop loss must be ${direction === 'long' ? 'below' : 'above'} the current displayed price`)
  }
}

/** UI prices are dollar-oriented; the Book consumes the inverse basket price. */
export function protectionParamsFromInputs(input: {
  takeProfit: string; stopLoss: string; mode: 'price' | 'percent'; direction: PerpsDirection; rawMark: bigint; cap: bigint
}): PositionProtectionParams {
  const displayedMark = input.cap - input.rawMark
  function price(value: string, leg: 'tp' | 'sl'): bigint {
    if (!value.trim()) return 0n
    const decimals = input.mode === 'price' ? 8 : 4
    if (!new RegExp(`^\\d+(?:\\.\\d{1,${decimals.toString()}})?$`).test(value) || Number(value) <= 0) {
      throw new Error(`Enter a positive ${input.mode === 'price' ? 'price (up to 8 decimals)' : 'percentage (up to 4 decimals)'}`)
    }
    const amount = parseUnits(value, decimals)
    const increase = (input.direction === 'long') === (leg === 'tp')
    const display = input.mode === 'price' ? amount : displayedMark + (increase ? 1n : -1n) * displayedMark * amount / 1_000_000n
    if (display <= 0n || display >= input.cap) throw new Error('Trigger price must be between zero and the price cap')
    return input.cap - display
  }
  const params = { takeProfitTriggerPrice: price(input.takeProfit, 'tp'), stopLossTriggerPrice: price(input.stopLoss, 'sl') }
  validateProtectionParams(params, input.direction, input.rawMark, input.cap)
  return params
}

export function parsePositionProtection(value: unknown): PositionProtection | undefined {
  if (!value || typeof value !== 'object') return undefined
  const keys = ['protectionId', 'parentOrderId', 'linkedOrderId', 'account', 'side', 'size', 'takeProfitTriggerPrice', 'stopLossTriggerPrice', 'triggerBountyUsdc', 'executionBountyUsdc', 'armedAt', 'armedBlock', 'triggerMarkPrice', 'triggerPublishTime', 'triggeredLeg', 'status'] as const
  const record = Object.fromEntries(keys.map((key, index) => [key, Array.isArray(value) ? value[index] : (value as Record<string, unknown>)[key]]))
  if (typeof record.protectionId !== 'bigint' || record.protectionId === 0n) return undefined
  return record as unknown as PositionProtection
}
