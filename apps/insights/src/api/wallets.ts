import { useQuery } from '@tanstack/react-query'
import { InsightsApiError } from './client'
import type { LiquidityNetworkMeta } from './liquidity'
import type { ApiErrorBody, TradeExecution, WalletActivity } from './types'

const API_ROOT = '/api/insights/v1'

export interface AccountLedgerPosition {
  market: string
  side: 'long' | 'short' | 'unknown'
  size: string
  marginAssets: string
  entryPrice: string
  unrealizedPnlAssets: string
  liquidatable: boolean
}

export interface AccountLedgerSnapshot {
  settlementBalanceAssets: string
  freeSettlementAssets: string
  activePositionMarginAssets: string
  otherLockedMarginAssets: string
  committedMarginAssets: string
  executionBountyReserveAssets: string
  traderClaimBalanceAssets: string
  pendingOrderCount: number
  accountEquityAssets: string
  freeBuyingPowerAssets: string
  netEquityAssets: string
  hasPosition: boolean
  position: AccountLedgerPosition | null
}

export interface AccountPerformanceStats {
  totalTrades: number
  opensCount: number
  closesCount: number
  liquidationsCount: number
  volumeAssets: string
  realizedPnlAssets: string
  activeDays: number
  firstActivityAt: string | null
  lastActivityAt: string | null
}

export interface AccountActivityRecord {
  id: string
  activityType: string
  orderId?: string | null
  side?: 'long' | 'short' | 'unknown' | null
  price?: string | null
  sizeDelta?: string | null
  amountAssets?: string | null
  pnlAssets?: string | null
  vpiAssets?: string | null
  frozenCloseSpreadAssets?: string | null
  oracleFrozen?: boolean | null
  latencySeconds?: number | null
  keeperBountyAssets?: string | null
  execution?: TradeExecution | null
  txHash: string
  blockNumber: string
  occurredAt: string
}

export interface AccountProtectionSnapshot {
  protectionId?: string
  status?: string
  kind?: string
  triggerPrice?: string
  orderPrice?: string
  closeBps?: number | string
  size?: string
  side?: string | number
  expiry?: string | number
  updatedBlock?: string
  [key: string]: unknown
}

export interface AccountVaultBalance {
  tranche: 'senior' | 'junior' | 'unknown'
  vaultAddress: string
  shareBalance: string
  unclaimedDepositShares: string
  withdrawalEscrowShares: string
  totalAttributedShares: string
  observedBlock: string
}

export interface AccountVaultRequestState {
  tranche: 'senior' | 'junior' | 'unknown'
  vaultAddress: string
  requestId: string
  pendingDepositAssets: string
  claimableDepositAssets: string
  claimableDepositShares: string
  refundableDepositAssets: string
  pendingRedeemShares: string
  refundableRedeemShares: string
  redeemRefundPending: boolean
  isActive: boolean
  observedBlock: string
}

export interface AccountVaultRequestEvent {
  tranche: 'senior' | 'junior' | 'unknown'
  vaultAddress: string
  eventName: string
  requestId: string
  rawAmount: string
  txHash: string
  blockNumber: string
  occurredAt: string
}

export interface AccountDivisionsWeek {
  weekStart: number
  closingBlock: string
  weekStartsAt: string | null
  weekClosesAt: string | null
  publishedAt: string | null
  displayName: string | null
  rank: number | null
  tier: string | null
  previousTier: string | null
  movement: string | null
  weeklyPnlAssets: string | null
  adjustedEquityAssets: string | null
  activeDays: number | null
  qualifying: boolean | null
}

export type BalanceTimeframe = '7d' | '30d' | '90d'

export interface BalanceHistoryPoint {
  occurredAt: string
  timestamp: number
  blockNumber: string
  balanceAssets: string
  netEquityAssets?: string | null
  eventType: string
  source: string
}

export interface WalletSummaryResponse {
  network: LiquidityNetworkMeta
  account: string
  observedAt: string
  identity: {
    discordUsername: string | null
    isDivisionsLinked: boolean
  }
  ledgerAvailable: boolean
  ledger: AccountLedgerSnapshot | null
  performance: AccountPerformanceStats
  balanceHistory?: BalanceHistoryPoint[]
  activity: AccountActivityRecord[]
  protections: {
    indexedThroughBlock: string
    items: AccountProtectionSnapshot[]
  }
  lp: {
    balances: AccountVaultBalance[]
    requestStates: AccountVaultRequestState[]
    requestEvents: AccountVaultRequestEvent[]
  }
  divisionsHistory: AccountDivisionsWeek[]
}

interface Envelope<T> {
  data: T
}

async function fetchWalletJson<T>(path: string, signal?: AbortSignal): Promise<T> {
  const response = await fetch(`${API_ROOT}${path}`, {
    headers: { Accept: 'application/json' },
    signal,
  })
  if (!response.ok) {
    let body: ApiErrorBody | undefined
    try {
      body = (await response.json()) as ApiErrorBody
    } catch {
      body = undefined
    }
    const message =
      body?.error?.message ?? body?.message ?? `Request failed (${String(response.status)})`
    throw new InsightsApiError(message, response.status, body?.error?.code)
  }
  const parsed = (await response.json()) as T | Envelope<T>
  if (typeof parsed === 'object' && parsed !== null && 'data' in parsed) {
    return parsed.data
  }
  return parsed
}

export function useWalletSummary(address: string | undefined, limit = 50) {
  const normalized = address?.trim().toLowerCase()
  return useQuery({
    queryKey: ['insights', 'wallets', normalized, limit] as const,
    enabled: Boolean(normalized),
    queryFn: ({ signal }) =>
      fetchWalletJson<WalletSummaryResponse>(
        `/wallets/${encodeURIComponent(normalized ?? '')}/summary?limit=${String(limit)}`,
        signal,
      ),
    staleTime: 15_000,
    refetchInterval: 15_000,
    refetchIntervalInBackground: false,
    retry(failureCount, error) {
      if (error instanceof InsightsApiError && (error.status === 404 || error.status === 400)) {
        return false
      }
      return failureCount < 2
    },
  })
}

export function formatVaultShares(raw: string | null | undefined, decimals = 9): string {
  if (raw == null || !/^\d+$/.test(raw)) return 'Unavailable'
  const value = BigInt(raw)
  const scale = 10n ** BigInt(decimals)
  const whole = value / scale
  const frac = ((value % scale) / 10n ** BigInt(Math.max(0, decimals - 2)))
    .toString()
    .padStart(2, '0')
  return `${whole.toLocaleString('en-US')}.${frac} shares`
}

export function formatPlDxyQuantity(raw: string | null | undefined): string {
  if (raw == null || !/^\d+$/.test(raw)) return 'Unavailable'
  const value = BigInt(raw)
  const scale = 10n ** 18n
  const whole = value / scale
  const frac = ((value % scale) / 10n ** 16n).toString().padStart(2, '0')
  return `${whole.toLocaleString('en-US')}.${frac} plDXY`
}

export function accountActivityToWalletActivity(row: AccountActivityRecord): WalletActivity {
  return {
    id: row.id,
    type: row.activityType,
    occurredAt: row.occurredAt,
    market: 'plDXY-PERP',
    side: row.side === 'long' || row.side === 'short' ? row.side : null,
    size: row.amountAssets ?? null,
    sizeDelta: row.sizeDelta ?? null,
    price: row.price ?? null,
    pnl: row.pnlAssets ?? null,
    execution: row.execution ?? null,
    executionFee: row.execution?.receipt.executionFeeAssets ?? null,
    vpi: row.vpiAssets ?? null,
    txHash: row.txHash,
  }
}

export const BALANCE_TIMEFRAME_DAYS: Record<BalanceTimeframe, number> = {
  '7d': 7,
  '30d': 30,
  '90d': 90,
}

/**
 * Slices a chronological 90d balance history to the requested window (7d/30d/90d),
 * carrying forward the latest canonical point at or before the window boundary so
 * accounts with quiet periods still display an accurate baseline rather than a zero.
 */
export function sliceBalanceHistory(
  points: BalanceHistoryPoint[],
  timeframe: BalanceTimeframe,
  nowUnix?: number,
): BalanceHistoryPoint[] {
  if (points.length === 0) return []
  const sorted = [...points].sort((a, b) => a.timestamp - b.timestamp)
  const latestTs = nowUnix ?? sorted[sorted.length - 1].timestamp
  const windowStart = latestTs - BALANCE_TIMEFRAME_DAYS[timeframe] * 86_400

  let baseline: BalanceHistoryPoint | null = null
  const inWindow: BalanceHistoryPoint[] = []

  for (const pt of sorted) {
    if (pt.timestamp < windowStart) {
      baseline = pt
    } else {
      inWindow.push(pt)
    }
  }

  const result: BalanceHistoryPoint[] = []
  if (baseline) {
    result.push({
      ...baseline,
      timestamp: windowStart,
      occurredAt: new Date(windowStart * 1000).toISOString(),
    })
  }
  result.push(...inWindow)

  // If the account only had a baseline before the window and a single carried point,
  // extend horizontally to latestTs so the SVG renders a flat balance line.
  if (result.length === 1 && result[0].timestamp < latestTs) {
    result.push({
      ...result[0],
      timestamp: latestTs,
      occurredAt: new Date(latestTs * 1000).toISOString(),
      eventType: 'Carried',
    })
  }

  return result
}
