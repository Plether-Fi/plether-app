import { useQuery } from '@tanstack/react-query'
import { InsightsApiError } from './client'
import type { LiquidityNetworkMeta } from './liquidity'
import type { ApiErrorBody, TradeExecution, WalletActivity } from './types'

const API_ROOT = '/api/insights/v1'
const LOW_ETH_WEI_THRESHOLD = 10_000_000_000_000_000n // 0.01 ETH

export interface OrderReliabilityCohort {
  windowSeconds: number
  totalOrders: number
  executedOrders: number
  expiredOrders: number
  otherFailedOrders: number
  pendingOrders: number
  activeAccounts: number
  affectedAccounts: number
  repeatAffectedAccounts: number
  fillRateBps: number | null
  expiryRateBps: number | null
}

export interface LatencyDistribution {
  sampleCount: number
  p50: number | null
  p95: number | null
  p99: number | null
  max: number | null
}

export interface ExecutionQualityResponse {
  network: LiquidityNetworkMeta
  observedAt: string
  cohortEndAt: string
  volume24hAssets: string
  pendingOrderQueue: number
  orderReliability: OrderReliabilityCohort[]
  executionLatency: {
    seconds: LatencyDistribution
    blocks: LatencyDistribution
  }
  protections: {
    indexedThroughBlock: string
    activeProtections: number
    triggered24h: number
    created24h: number
    cancelled24h: number
    expired24h: number
  }
  liquidations: {
    indexedThroughBlock: string
    updatedAt: string | null
    watchedCandidates: number
    liquidations24h: number
    bountiesPaid24hAssets: string
  }
}

export interface ProtocolTradeRecord {
  id: string
  activityType: string
  account: string
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

export interface ExecutionTradesResponse {
  network: LiquidityNetworkMeta
  trades: ProtocolTradeRecord[]
}

export type KeeperRoleId =
  | 'order_execution'
  | 'lp_settlement'
  | 'protection'
  | 'liquidation'
  | 'oracle'

export interface DetectedKeeperRecord {
  address: string
  roles: string[]
  executions24h: number
  shareBps: number | null
  latencyP50Seconds: number | null
  latencyP95Seconds: number | null
  ethBalanceWei: string | null
  bountiesEarnedAssets: string
  lastActiveBlock: string
  lastActiveAt: string
}

export interface KeepersStatusResponse {
  network: LiquidityNetworkMeta
  observedAt: string
  summary: {
    activeKeepers24h: number
    executions24h: number
    latencyP50Seconds: number | null
    latencyP95Seconds: number | null
    bountiesPaid24hAssets: string
  }
  keepers: DetectedKeeperRecord[]
}

export interface KeeperActivityRecord {
  role: string
  keeperAddress: string
  txHash: string
  blockNumber: string
  occurredAt: string
  latencySeconds: number | null
  bountyAssets: string | null
}

export interface KeepersActivityResponse {
  network: LiquidityNetworkMeta
  observedAt: string
  activity: KeeperActivityRecord[]
}

interface Envelope<T> {
  data: T
}

async function fetchExecutionJson<T>(path: string, signal?: AbortSignal): Promise<T> {
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

function shouldRetryEndpoint(failureCount: number, error: Error): boolean {
  if (error instanceof InsightsApiError && error.status === 404) return false
  return failureCount < 2
}

export function useExecutionQuality() {
  return useQuery({
    queryKey: ['insights', 'execution', 'quality'] as const,
    queryFn: ({ signal }) => fetchExecutionJson<ExecutionQualityResponse>('/execution/quality', signal),
    staleTime: 15_000,
    refetchInterval: 15_000,
    refetchIntervalInBackground: false,
    retry: shouldRetryEndpoint,
  })
}

export function useExecutionTrades(limit = 30) {
  return useQuery({
    queryKey: ['insights', 'execution', 'trades', limit] as const,
    queryFn: ({ signal }) =>
      fetchExecutionJson<ExecutionTradesResponse>(`/execution/trades?limit=${String(limit)}`, signal),
    staleTime: 15_000,
    refetchInterval: 15_000,
    refetchIntervalInBackground: false,
    retry: shouldRetryEndpoint,
  })
}

export function useKeepersStatus() {
  return useQuery({
    queryKey: ['insights', 'keepers', 'status'] as const,
    queryFn: ({ signal }) => fetchExecutionJson<KeepersStatusResponse>('/keepers/status', signal),
    staleTime: 15_000,
    refetchInterval: 15_000,
    refetchIntervalInBackground: false,
    retry: shouldRetryEndpoint,
  })
}

export function useKeepersActivity(limit = 50) {
  return useQuery({
    queryKey: ['insights', 'keepers', 'activity', limit] as const,
    queryFn: ({ signal }) =>
      fetchExecutionJson<KeepersActivityResponse>(`/keepers/activity?limit=${String(limit)}`, signal),
    staleTime: 15_000,
    refetchInterval: 15_000,
    refetchIntervalInBackground: false,
    retry: shouldRetryEndpoint,
  })
}

export function formatEthWei(wei: string | null | undefined): string {
  if (wei == null || !/^\d+$/.test(wei)) return 'Unavailable'
  const raw = BigInt(wei)
  const divisor = 10n ** 14n // 4 decimal places
  const scaled = (raw + divisor / 2n) / divisor
  const whole = scaled / 10_000n
  const frac = (scaled % 10_000n).toString().padStart(4, '0')
  return `${whole.toLocaleString('en-US')}.${frac} ETH`
}

export function isLowEthBalance(wei: string | null | undefined): boolean {
  if (wei == null || !/^\d+$/.test(wei)) return false
  return BigInt(wei) < LOW_ETH_WEI_THRESHOLD
}

export function formatRateBps(bps: number | null | undefined): string {
  if (bps == null || !Number.isFinite(bps)) return '—'
  return `${(bps / 100).toFixed(2)}%`
}

export function tradeToWalletActivity(trade: ProtocolTradeRecord): WalletActivity {
  return {
    id: trade.id,
    type: trade.activityType,
    occurredAt: trade.occurredAt,
    market: 'plDXY-PERP',
    side: trade.side === 'long' || trade.side === 'short' ? trade.side : null,
    size: trade.amountAssets ?? null,
    sizeDelta: trade.sizeDelta ?? null,
    price: trade.price ?? null,
    pnl: trade.pnlAssets ?? null,
    execution: trade.execution ?? null,
    executionFee: trade.execution?.receipt.executionFeeAssets ?? null,
    vpi: trade.vpiAssets ?? null,
    txHash: trade.txHash,
  }
}
