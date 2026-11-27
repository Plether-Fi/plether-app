import { formatPlDxyQuantity, type AccountProtectionSnapshot } from '../api'
import type { StatusTone } from '../components/StatusPill'
import { formatCompactAssets, formatPrice } from './format'

const PROTECTION_STATUS_CODES: Record<string, string> = {
  '0': 'None',
  '1': 'PendingOpen',
  '2': 'Armed',
  '3': 'Triggered',
  '4': 'Executed',
  '5': 'Failed',
  '6': 'Cancelled',
  '7': 'Liquidated',
  '8': 'Latched',
}

const TRIGGERED_LEG_CODES: Record<string, string> = {
  '0': 'None',
  '1': 'TakeProfit',
  '2': 'StopLoss',
}

function readRawField(item: AccountProtectionSnapshot, keys: string[]): string | null {
  for (const key of keys) {
    const value = item[key]
    if (typeof value === 'string' && value.trim().length > 0) return value.trim()
    if (typeof value === 'number' && Number.isFinite(value)) return String(value)
  }
  return null
}

function nonZero(value: string | null): string | null {
  if (!value || value === '0') return null
  return value
}

export interface NormalizedAccountProtection {
  protectionId: string
  parentOrderId: string | null
  linkedOrderId: string | null
  status: string
  triggeredLeg: string | null
  takeProfitTriggerPrice: string | null
  stopLossTriggerPrice: string | null
  triggerMarkPrice: string | null
  triggerBountyAssets: string | null
  armedBlock: string | null
  updatedBlock: string | null
}

export function normalizeAccountProtection(
  item: AccountProtectionSnapshot,
  fallbackId = '—',
): NormalizedAccountProtection {
  const rawStatus = readRawField(item, ['statusName', 'status', 'state'])
  const status = rawStatus ? (PROTECTION_STATUS_CODES[rawStatus] ?? rawStatus) : 'Unknown'
  const rawLeg = readRawField(item, ['triggeredLegName', 'triggeredLeg'])
  const mappedLeg = rawLeg ? (TRIGGERED_LEG_CODES[rawLeg] ?? rawLeg) : null
  const legacyKind = readRawField(item, ['kind', 'protectionType', 'triggerKind'])
  const legacyPrice = nonZero(readRawField(item, ['triggerPrice', 'price']))

  let takeProfitTriggerPrice = nonZero(
    readRawField(item, ['takeProfitTriggerPrice', 'takeProfitPrice']),
  )
  let stopLossTriggerPrice = nonZero(readRawField(item, ['stopLossTriggerPrice', 'stopLossPrice']))

  if (!takeProfitTriggerPrice && legacyKind?.toLowerCase() === 'takeprofit') {
    takeProfitTriggerPrice = legacyPrice
  }
  if (!stopLossTriggerPrice && legacyKind?.toLowerCase() === 'stoploss') {
    stopLossTriggerPrice = legacyPrice
  }

  const triggeredLeg = mappedLeg && mappedLeg !== 'None' ? mappedLeg : null

  return {
    protectionId: readRawField(item, ['protectionId', 'id']) ?? fallbackId,
    parentOrderId: nonZero(readRawField(item, ['parentOrderId'])),
    linkedOrderId: nonZero(readRawField(item, ['linkedOrderId'])),
    status,
    triggeredLeg,
    takeProfitTriggerPrice,
    stopLossTriggerPrice,
    triggerMarkPrice: nonZero(readRawField(item, ['triggerMarkPrice'])),
    triggerBountyAssets: nonZero(readRawField(item, ['triggerBountyAssets'])),
    armedBlock: nonZero(readRawField(item, ['armedBlock'])),
    updatedBlock: nonZero(readRawField(item, ['updatedBlock', 'blockNumber'])),
  }
}

export function protectionLifecycleTone(status: string | null | undefined): StatusTone {
  const lower = (status ?? '').toLowerCase()
  if (lower === 'armed' || lower === 'active' || lower === 'pendingopen') return 'live'
  if (lower === 'executed' || lower === 'triggered') return 'ready'
  if (lower === 'latched' || lower === 'failed') return 'degraded'
  if (lower === 'liquidated') return 'blocked'
  return 'unknown'
}

export function isActivePositionProtectionStatus(status: string): boolean {
  const lower = status.toLowerCase()
  return (
    lower === 'armed' ||
    lower === 'active' ||
    lower === 'pendingopen' ||
    lower === 'triggered' ||
    lower === 'latched'
  )
}

function formatLeverageBps(raw: string): string {
  const num = Number(raw)
  if (!Number.isFinite(num)) return raw
  return `${(num / 10_000).toFixed(2)}x`
}

function shortHash(hash: string): string {
  if (hash.length <= 18) return hash
  return `${hash.slice(0, 10)}…${hash.slice(-6)}`
}

export function formatCheckValue(value: string | null, unit: string): string {
  if (value == null) return '—'
  switch (unit) {
    case 'seconds':
      return `${value}s`
    case 'price':
      return formatPrice(value)
    case 'usdg':
      return formatCompactAssets(value)
    case 'size':
      return formatPlDxyQuantity(value)
    case 'bps':
      return formatLeverageBps(value)
    case 'hash':
      return shortHash(value)
    default:
      return value
  }
}
