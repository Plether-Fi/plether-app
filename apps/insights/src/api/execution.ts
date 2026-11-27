import { useQuery, useQueryClient } from '@tanstack/react-query'
import { InsightsApiError } from './client'
import type { LiquidityNetworkMeta } from './liquidity'
import type { ApiErrorBody, TradeExecution, WalletActivity } from './types'

const API_ROOT = '/api/insights/v1'
const LOW_ETH_WEI_THRESHOLD = 10_000_000_000_000_000n // 0.01 ETH

export interface OrderReliabilityBucket {
  bucketStart: string
  bucketEnd: string
  totalOrders: number
  executedOrders: number
  expiredOrders: number
  otherFailedOrders: number
  pendingOrders: number
}

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
  timeline?: OrderReliabilityBucket[]
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

export interface TransactionDetailsResponse {
  network: LiquidityNetworkMeta
  txHash: string
  blockNumber: string
  occurredAt: string
  executorAddress?: string | null
  events: ProtocolTradeRecord[]
}

export interface TransactionRouteState {
  trade?: ProtocolTradeRecord
  chainId?: number | null
  releaseVersion?: string | null
  traderLabel?: string | null
  from?: string | null
  fromLabel?: string | null
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

export interface KeeperRoleBreakdown {
  role: string
  executions24h: number
  shareBps: number | null
  latencyP50Seconds: number | null
  latencyP95Seconds: number | null
  bountiesEarnedAssets: string
}

export interface KeeperDetailResponse {
  network: LiquidityNetworkMeta
  observedAt: string
  keeper: {
    address: string
    roles: string[]
    executions24h: number
    shareBps: number | null
    latency: LatencyDistribution
    ethBalanceWei: string | null
    bountiesEarnedAssets: string
    lastActiveBlock: string | null
    lastActiveAt: string | null
  }
  roleBreakdown: KeeperRoleBreakdown[]
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

async function fetchTransactionWithFallback(
  txHash: string,
  signal?: AbortSignal,
): Promise<TransactionDetailsResponse> {
  const normalized = txHash.toLowerCase()
  try {
    return await fetchExecutionJson<TransactionDetailsResponse>(`/tx/${normalized}`, signal)
  } catch (error) {
    // If the dedicated /tx/:txHash route is not yet deployed on the backend
    // (plain 404 without a structured NOT_FOUND API error), fall back to
    // checking recent protocol trades so direct refreshes of recent fills work.
    if (error instanceof InsightsApiError && error.status === 404 && !error.code) {
      const recent = await fetchExecutionJson<ExecutionTradesResponse>(
        '/execution/trades?limit=100',
        signal,
      )
      const matching = recent.trades.filter((row) => row.txHash.toLowerCase() === normalized)
      if (matching.length > 0) {
        const first = matching[0]
        return {
          network: recent.network,
          txHash: normalized,
          blockNumber: first.blockNumber,
          occurredAt: first.occurredAt,
          executorAddress: null,
          events: matching,
        }
      }
    }
    throw error
  }
}

export function useTransaction(txHash: string | undefined) {
  const normalized = txHash?.trim().toLowerCase() ?? ''
  const valid = /^0x[0-9a-f]{64}$/.test(normalized)
  return useQuery({
    queryKey: ['insights', 'tx', normalized] as const,
    queryFn: ({ signal }) => fetchTransactionWithFallback(normalized, signal),
    enabled: valid,
    staleTime: 60_000,
    retry: shouldRetryEndpoint,
  })
}

export function extractLogIndex(eventId: string | null | undefined): string | null {
  if (!eventId) return null
  const colonIdx = eventId.lastIndexOf(':')
  if (colonIdx < 0) return null
  const suffix = eventId.slice(colonIdx + 1)
  return /^\d+$/.test(suffix) ? suffix : null
}

export function transactionPath(txHash: string, eventId?: string | null): string {
  const logIndex = extractLogIndex(eventId)
  const base = `/tx/${txHash.toLowerCase()}`
  return logIndex ? `${base}?log=${logIndex}` : base
}

export const keeperKeys = {
  status: ['insights', 'keepers', 'status'] as const,
  activity: (limit: number) => ['insights', 'keepers', 'activity', limit] as const,
  detail: (address: string, limit = 50) =>
    ['insights', 'keepers', 'detail', address.toLowerCase(), limit] as const,
}

export function useKeepersStatus() {
  return useQuery({
    queryKey: keeperKeys.status,
    queryFn: ({ signal }) => fetchExecutionJson<KeepersStatusResponse>('/keepers/status', signal),
    staleTime: 15_000,
    refetchInterval: 15_000,
    refetchIntervalInBackground: false,
    retry: shouldRetryEndpoint,
  })
}

export function useKeepersActivity(limit = 50) {
  return useQuery({
    queryKey: keeperKeys.activity(limit),
    queryFn: ({ signal }) =>
      fetchExecutionJson<KeepersActivityResponse>(`/keepers/activity?limit=${String(limit)}`, signal),
    staleTime: 15_000,
    refetchInterval: 15_000,
    refetchIntervalInBackground: false,
    retry: shouldRetryEndpoint,
  })
}

function computePercentile(sorted: number[], pct: number): number | null {
  if (sorted.length === 0) return null
  const idx = Math.min(sorted.length - 1, Math.floor((sorted.length * pct + 99) / 100) - 1)
  return sorted[Math.max(0, idx)]
}

async function fetchKeeperDetailWithFallback(
  address: string,
  limit: number,
  signal?: AbortSignal,
): Promise<KeeperDetailResponse> {
  const normalized = address.toLowerCase()
  try {
    return await fetchExecutionJson<KeeperDetailResponse>(
      `/keepers/${normalized}?limit=${String(limit)}`,
      signal,
    )
  } catch (error) {
    // Before /keepers/:address is deployed on the backend, synthesize cleanly from
    // the live /keepers/status and /keepers/activity feeds without faking missing values.
    if (error instanceof InsightsApiError && error.status === 404 && !error.code) {
      const [statusResp, activityResp] = await Promise.all([
        fetchExecutionJson<KeepersStatusResponse>('/keepers/status', signal),
        fetchExecutionJson<KeepersActivityResponse>('/keepers/activity?limit=100', signal),
      ])
      const matchKeeper = statusResp.keepers.find((k) => k.address.toLowerCase() === normalized)
      const matchingEvents = activityResp.activity.filter(
        (ev) => ev.keeperAddress.toLowerCase() === normalized,
      )
      const latencies = matchingEvents
        .map((ev) => ev.latencySeconds)
        .filter((v): v is number => v != null && v >= 0)
        .sort((a, b) => a - b)

      const roles = matchKeeper?.roles ?? Array.from(new Set(matchingEvents.map((e) => e.role))).sort()
      const totalExecs = matchKeeper?.executions24h ?? matchingEvents.length
      const roleBreakdown: KeeperRoleBreakdown[] = roles.map((role) => {
        const rEvents = matchingEvents.filter((ev) => ev.role === role)
        const rLat = rEvents
          .map((ev) => ev.latencySeconds)
          .filter((v): v is number => v != null && v >= 0)
          .sort((a, b) => a - b)
        const rBounty = rEvents.reduce((sum, ev) => sum + (ev.bountyAssets ? BigInt(ev.bountyAssets) : 0n), 0n)
        return {
          role,
          executions24h: rEvents.length,
          shareBps:
            matchingEvents.length > 0
              ? Math.round((rEvents.length * 10000) / matchingEvents.length)
              : null,
          latencyP50Seconds: computePercentile(rLat, 50),
          latencyP95Seconds: computePercentile(rLat, 95),
          bountiesEarnedAssets: rBounty.toString(),
        }
      })

      const latestEvent = matchingEvents.at(0)

      return {
        network: statusResp.network,
        observedAt: statusResp.observedAt,
        keeper: {
          address: normalized,
          roles,
          executions24h: totalExecs,
          shareBps: matchKeeper?.shareBps ?? null,
          latency: {
            sampleCount: latencies.length,
            p50: matchKeeper?.latencyP50Seconds ?? computePercentile(latencies, 50),
            p95: matchKeeper?.latencyP95Seconds ?? computePercentile(latencies, 95),
            p99: computePercentile(latencies, 99),
            max: latencies.length > 0 ? latencies[latencies.length - 1] : null,
          },
          ethBalanceWei: matchKeeper?.ethBalanceWei ?? null,
          bountiesEarnedAssets: matchKeeper?.bountiesEarnedAssets ?? '0',
          lastActiveBlock: matchKeeper?.lastActiveBlock ?? latestEvent?.blockNumber ?? null,
          lastActiveAt: matchKeeper?.lastActiveAt ?? latestEvent?.occurredAt ?? null,
        },
        roleBreakdown,
        activity: matchingEvents.slice(0, limit),
      }
    }
    throw error
  }
}

export function useKeeperDetail(address: string | undefined, limit = 50) {
  const queryClient = useQueryClient()
  const staticPreview = queryClient.getDefaultOptions().queries?.refetchInterval === false
  const normalized = address?.trim().toLowerCase() ?? ''
  const valid = /^0x[0-9a-f]{40}$/.test(normalized)

  return useQuery({
    queryKey: keeperKeys.detail(normalized, limit),
    queryFn: ({ signal }) => fetchKeeperDetailWithFallback(normalized, limit, signal),
    enabled: valid,
    staleTime: staticPreview ? Infinity : 15_000,
    refetchInterval: staticPreview ? false : 15_000,
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
