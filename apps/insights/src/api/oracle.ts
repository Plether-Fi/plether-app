import { useQuery } from '@tanstack/react-query'
import { InsightsApiError } from './client'
import type { LiquidityNetworkMeta } from './liquidity'
import type { ApiErrorBody } from './types'

const API_ROOT = '/api/insights/v1'

export type OracleSliceStatus = 'healthy' | 'degraded' | 'stale' | 'rejected' | 'idle'

export interface OracleHealthBucket {
  bucketStart: string
  bucketEnd: string
  status: OracleSliceStatus
  admittedCount: number
  latestCount: number
  revealCount: number
  rejectedCount: number
  staleCount: number
  highSpreadCount: number
  maxSpreadSeconds: number | null
  maxLagSeconds: number | null
}

export interface OraclePolicyFlags {
  closeOnly: boolean
  oracleFrozen: boolean
  isFadWindow: boolean
  maxStalenessSeconds: number
  requireStoredMark: boolean
  allowAnyStoredMark: boolean
}

export interface OracleAdmissionCheck {
  key: string
  label: string
  status: 'passed' | 'warning' | 'failed' | 'unknown'
  threshold: string
  currentValue: string
  rejections24h: number
  description: string
}

export interface OracleBasketComponent {
  symbol: string
  feedSymbol: string
  feedId: string
  price: string
  rawPrice: string
  confidence: string
  confidenceBps: number | null
  confidenceExceeded: boolean
  exponent: number | null
  publishTime: string | null
  driftFromMinSeconds: number | null
  weightBps: number | null
  basePrice?: string | null
  contribution?: string | null
  effectiveShareBps?: number | null
  changeFromBaseBps?: number | null
  inverted: boolean
}

export interface OracleUpdateSummary {
  id: string
  minPublishTime: string
  maxPublishTime: string
  fetchedAt: string
  source: string
  basketPrice: string | null
  spreadSeconds: number
  lagSeconds: number
  ordersSettledCount: number
}

export interface OracleUpdateSettledOrder {
  orderId: string
  account: string
  side: string | null
  status: string
  txHash: string | null
  executionPrice: string | null
  executionOraclePrice: string | null
  commitTimestamp: string | null
  terminalTimestamp: string | null
}

export interface OracleUpdateDetailResponse {
  network: LiquidityNetworkMeta
  observedAt: string
  update: {
    id: string
    source: string
    status: string
    minPublishTime: string
    maxPublishTime: string
    fetchedAt: string
    spreadSeconds: number
    lagSeconds: number
    vaaCount: number
    basketPrice: string | null
    displayPriceCap: string
    reconstructedBasketPrice: string | null
    reconstructionDiffRaw: number | null
    previousBasketPrice: string | null
    displayDeltaBps: number | null
    maxComponentConfidenceBps: number | null
    totalTargetWeightBps: number
    totalEffectiveShareBps: number
  }
  navigation: {
    previousId: string | null
    nextId: string | null
    previousMaxPublishTime: string | null
    nextMaxPublishTime: string | null
  }
  thresholds: {
    maxComponentDivergenceSeconds: number
    orderStalenessLimitSeconds: number | null
    liquidationStalenessLimitSeconds: number | null
    maxConfidenceRatioBps: number | null
    adverseConfidenceMultiplierBps: number | null
    displayPriceCap: string
  }
  admissionChecks: OracleAdmissionCheck[]
  components: OracleBasketComponent[]
  settledOrders: OracleUpdateSettledOrder[]
}

export interface OracleRejectionBreakdown {
  reasonCode: string
  outcome: string
  count24h: number
  lastOccurredAt: string
}

export interface OracleRejectionEvent {
  id: string
  occurredAt: string
  route: string
  outcome: string
  reasonCode: string
  minPublishTime: string | null
  maxPublishTime: string | null
  spreadSeconds: number | null
  ageSeconds: number | null
  detail: string
}

export interface OracleHealthResponse {
  network: LiquidityNetworkMeta
  observedAt: string
  overallStatus: string
  regime: string
  live: {
    latestUpdateId?: string | null
    basketPrice: string | null
    onChainMarkPrice: string | null
    onChainMarkTime: string | null
    latestPublishTime: string | null
    minPublishTime: string | null
    fetchedAt: string | null
    source: string | null
    ageSeconds: number | null
    spreadSeconds: number | null
    maxComponentConfidenceBps: number | null
    oracleFrozen: boolean | null
    fadWindow: boolean | null
    tradingActive: boolean | null
    openPolicy: OraclePolicyFlags | null
    closePolicy: OraclePolicyFlags | null
  }
  thresholds: {
    maxComponentDivergenceSeconds: number
    orderStalenessLimitSeconds: number | null
    liquidationStalenessLimitSeconds: number | null
    maxConfidenceRatioBps: number | null
    adverseConfidenceMultiplierBps: number | null
    displayPriceCap: string
  }
  summary24h: {
    admittedUpdates: number
    latestAdmittedUpdates: number
    revealAdmittedUpdates: number
    rejectedUpdates: number
    staleMarketClosedPolls: number
    admissionRateBps: number | null
    maxSpreadSeconds: number | null
    maxLagSeconds: number | null
  }
  windows: {
    window1h: {
      windowSeconds: number
      bucketSeconds: number
      timeline: OracleHealthBucket[]
    }
    window24h: {
      windowSeconds: number
      bucketSeconds: number
      timeline: OracleHealthBucket[]
    }
  }
  admissionChecks: OracleAdmissionCheck[]
  components: OracleBasketComponent[]
  recentUpdates?: OracleUpdateSummary[]
  rejectionBreakdown: OracleRejectionBreakdown[]
  recentRejections: OracleRejectionEvent[]
}

interface Envelope<T> {
  data: T
}

export async function fetchOracleHealth(signal?: AbortSignal): Promise<OracleHealthResponse> {
  const response = await fetch(`${API_ROOT}/oracle/health`, {
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
  const parsed = (await response.json()) as OracleHealthResponse | Envelope<OracleHealthResponse>
  if ('data' in parsed) {
    return parsed.data
  }
  return parsed
}

export function useOracleHealth() {
  return useQuery({
    queryKey: ['insights', 'oracle', 'health'] as const,
    queryFn: ({ signal }) => fetchOracleHealth(signal),
    staleTime: 15_000,
    refetchInterval: 15_000,
    refetchIntervalInBackground: false,
    retry(failureCount, error) {
      if (error instanceof InsightsApiError && error.status === 404) return false
      return failureCount < 2
    },
  })
}

export function isValidOracleUpdateId(value: string): boolean {
  const trimmed = value.trim().toLowerCase()
  return trimmed === 'latest' || (/^\d+$/.test(trimmed) && trimmed !== '0')
}

export function oracleUpdatePath(updateId?: string | null): string {
  const trimmed = updateId?.trim()
  if (trimmed && isValidOracleUpdateId(trimmed)) {
    return `/oracle/updates/${trimmed.toLowerCase()}`
  }
  return '/oracle/updates/latest'
}

export async function fetchOracleUpdateDetail(
  id: string,
  signal?: AbortSignal,
): Promise<OracleUpdateDetailResponse> {
  const normalized = id.trim().toLowerCase()
  const response = await fetch(`${API_ROOT}/oracle/updates/${encodeURIComponent(normalized)}`, {
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
  const parsed = (await response.json()) as
    | OracleUpdateDetailResponse
    | Envelope<OracleUpdateDetailResponse>
  if ('data' in parsed) {
    return parsed.data
  }
  return parsed
}

export function useOracleUpdateDetail(id: string) {
  const normalized = id.trim().toLowerCase()
  const isValid = isValidOracleUpdateId(normalized)
  return useQuery({
    queryKey: ['insights', 'oracle', 'update', normalized] as const,
    queryFn: ({ signal }) => fetchOracleUpdateDetail(normalized, signal),
    staleTime: normalized === 'latest' ? 15_000 : 60_000,
    ...(isValid ? {} : { enabled: false }),
    retry(failureCount, error) {
      if (
        error instanceof InsightsApiError &&
        (error.status === 400 || error.status === 404)
      ) {
        return false
      }
      return failureCount < 2
    },
  })
}
