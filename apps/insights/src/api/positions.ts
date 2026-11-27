import { useQuery, useQueryClient } from '@tanstack/react-query'
import { InsightsApiError } from './client'
import type { OrderProtectionsPayload } from './execution'
import type { LiquidityNetworkMeta } from './liquidity'
import type { ApiErrorBody } from './types'

const API_ROOT = '/api/insights/v1'
const PLDXY_PRICE_CAP = 200_000_000

export type PositionStatus = 'open' | 'closed' | 'liquidated'
export type PositionSide = 'long' | 'short' | 'unknown'

export interface ActiveExposureSnapshot {
  positionId: string
  market: string
  side: PositionSide
  size: string
  notionalAssets: string
  marginAssets: string
  entryPrice: string
  markPrice: string
  liquidationPrice: string | null
  distanceToLiquidationBps: number | null
  closestLiquidationBps: number | null
  effectiveLeverageX100: number
  unrealizedPnlAssets: string
  marginUtilizationBps: number | null
  liquidatable: boolean
  openedAt: string
  durationSeconds: number
  mfePnlAssets: string
  maePnlAssets: string
}

export interface WalletExposureSummary {
  totalPositions: number
  openPositions: number
  closedPositions: number
  liquidatedPositions: number
  winCount: number
  lossCount: number
  winRateBps: number | null
  longPositions: number
  shortPositions: number
  longNotionalAssets: string
  shortNotionalAssets: string
  longNotionalShareBps: number | null
  shortNotionalShareBps: number | null
  longRealizedPnlAssets: string
  shortRealizedPnlAssets: string
  longHoldSeconds: number
  shortHoldSeconds: number
  avgEntryLeverageX100: number | null
  maxLeverageX100: number
  currentLeverageX100: number
  avgHoldSeconds: number | null
  medianHoldSeconds: number | null
  avgNotionalAssets: string | null
  peakNotionalAssets: string
  avgMfeAssets: string | null
  avgMaeAssets: string | null
  tightestLiquidationBps: number | null
}

export interface ExposureTimelinePoint {
  timestamp: number
  occurredAt: string
  blockNumber: string
  signedNotionalAssets: string
  absNotionalAssets: string
  marginAssets: string
  leverageX100: number
  side: 'long' | 'short' | 'flat' | 'unknown'
  eventType: string
  positionId: string | null
}

export interface PositionLifecycleSummary {
  positionId: string
  market: string
  status: PositionStatus
  side: PositionSide
  openedAt: string
  closedAt: string | null
  durationSeconds: number
  entryPrice: string
  exitPrice: string | null
  currentSize: string
  peakSize: string
  peakNotionalAssets: string
  peakMarginAssets: string
  entryLeverageX100: number
  maxLeverageX100: number
  currentLeverageX100: number
  realizedPnlAssets: string
  unrealizedPnlAssets: string
  netPnlAssets: string
  roiBps: number | null
  totalFeesAssets: string
  totalVpiAssets: string
  totalKeeperBountyAssets: string
  mfePnlAssets: string
  mfeBps: number
  mfePrice: string | null
  mfeOccurredAt: string | null
  maePnlAssets: string
  maeBps: number
  maePrice: string | null
  maeOccurredAt: string | null
  liquidationPrice: string | null
  closestLiquidationBps: number | null
  closestLiquidationAt: string | null
  currentLiquidationBps: number | null
  liquidatable: boolean
  fillCount: number
  scaleInCount: number
  partialCloseCount: number
  marginAddCount: number
  openTxHash: string
  closeTxHash: string | null
  openOrderId: string | null
}

export interface PositionLifecycleDetail extends PositionLifecycleSummary {
  captureRatioBps: number | null
  remainingMarginAssets: string
  entryCostAssets: string
}

export interface PositionPricePoint {
  timestamp: number
  occurredAt: string
  rawPrice: string
  rawHigh: string
  rawLow: string
  unrealizedPnlAssets: string
  positionSize: string
  leverageX100: number
  liquidationPrice: string | null
  distanceToLiquidationBps: number | null
}

export interface PositionStepEvent {
  id: string
  action: string
  activityType: string
  orderId?: string | null
  txHash: string
  blockNumber: string
  timestamp: number
  occurredAt: string
  price?: string | null
  sizeDelta: string
  resultingSize: string
  resultingEntryPrice: string
  marginDeltaAssets: string
  resultingMarginAssets: string
  resultingNotionalAssets: string
  resultingLeverageX100: number
  resultingLiquidationPrice?: string | null
  pnlAssets?: string | null
  feeAssets?: string | null
  vpiAssets?: string | null
  keeperBountyAssets?: string | null
  orderProtections?: OrderProtectionsPayload | null
}

export interface PositionProtectionEvent {
  protectionId: string
  blockNumber: string
  logIndex: number
  txHash: string
  eventName: string
  eventData: Record<string, unknown>
  snapshot: Record<string, unknown>
}

export interface WalletExposureResponse {
  network: LiquidityNetworkMeta
  account: string
  observedAt: string
  currentMarkPrice: string | null
  markObservedAt: string | null
  maintenanceMarginBps: number
  activeExposure: ActiveExposureSnapshot | null
  summary: WalletExposureSummary
  exposureTimeline: ExposureTimelinePoint[]
  positions: PositionLifecycleSummary[]
}

export interface PositionDetailResponse {
  network: LiquidityNetworkMeta
  account: string
  observedAt: string
  identity: {
    discordUsername: string | null
  }
  currentMarkPrice: string | null
  markObservedAt: string | null
  maintenanceMarginBps: number
  candleIntervalSeconds: number
  position: PositionLifecycleDetail
  priceSeries: PositionPricePoint[]
  events: PositionStepEvent[]
  protections: PositionProtectionEvent[]
}

interface Envelope<T> {
  data: T
}

async function fetchPositionsJson<T>(path: string, signal?: AbortSignal): Promise<T> {
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

export interface GlobalOpenPositionRecord {
  account: string
  discordUsername: string | null
  positionId: string
  market: string
  side: PositionSide
  size: string
  notionalAssets: string
  marginAssets: string
  entryPrice: string
  markPrice: string
  liquidationPrice: string | null
  distanceToLiquidationBps: number | null
  effectiveLeverageX100: number
  unrealizedPnlAssets: string
  openedAt: string
  durationSeconds: number
}

export interface GlobalMarketExposureSummary {
  longOpenInterestAssets: string
  shortOpenInterestAssets: string
  totalOpenInterestAssets: string
  netSkewAssets: string
  longShareBps: number | null
  shortShareBps: number | null
  openPositionCount: number
  longCount: number
  shortCount: number
  avgLeverageX100: number | null
}

export interface GlobalMarketExposureResponse {
  network: LiquidityNetworkMeta
  observedAt: string
  currentMarkPrice: string | null
  markObservedAt: string | null
  summary: GlobalMarketExposureSummary
  positions: GlobalOpenPositionRecord[]
}

export function useMarketExposure() {
  const queryClient = useQueryClient()
  const staticPreview = queryClient.getDefaultOptions().queries?.refetchInterval === false
  return useQuery({
    queryKey: ['insights', 'market', 'exposure'] as const,
    enabled: !staticPreview,
    queryFn: ({ signal }) =>
      fetchPositionsJson<GlobalMarketExposureResponse>('/market/exposure', signal),
    staleTime: staticPreview ? Infinity : 15_000,
    refetchInterval: staticPreview ? false : 15_000,
    refetchIntervalInBackground: false,
    retry(failureCount, error) {
      if (error instanceof InsightsApiError && (error.status === 404 || error.status === 400)) {
        return false
      }
      return failureCount < 2
    },
  })
}

export function positionPath(address: string, positionId: string): string {
  return `/wallets/${encodeURIComponent(address.toLowerCase())}/positions/${encodeURIComponent(positionId)}`
}

export function useWalletExposure(address: string | undefined) {
  const queryClient = useQueryClient()
  const staticPreview = queryClient.getDefaultOptions().queries?.refetchInterval === false
  const normalized = address?.trim().toLowerCase()
  return useQuery({
    queryKey: ['insights', 'wallets', normalized, 'exposure'] as const,
    enabled: Boolean(normalized) && !staticPreview,
    queryFn: ({ signal }) =>
      fetchPositionsJson<WalletExposureResponse>(
        `/wallets/${encodeURIComponent(normalized ?? '')}/exposure`,
        signal,
      ),
    staleTime: staticPreview ? Infinity : 15_000,
    refetchInterval: staticPreview ? false : 15_000,
    refetchIntervalInBackground: false,
    retry(failureCount, error) {
      if (error instanceof InsightsApiError && (error.status === 404 || error.status === 400)) {
        return false
      }
      return failureCount < 2
    },
  })
}

export function useWalletPositionDetail(
  address: string | undefined,
  positionId: string | undefined,
) {
  const queryClient = useQueryClient()
  const staticPreview = queryClient.getDefaultOptions().queries?.refetchInterval === false
  const normalized = address?.trim().toLowerCase()
  const trimmedPos = positionId?.trim()
  return useQuery({
    queryKey: ['insights', 'wallets', normalized, 'positions', trimmedPos] as const,
    enabled: Boolean(normalized) && Boolean(trimmedPos) && !staticPreview,
    queryFn: ({ signal }) =>
      fetchPositionsJson<PositionDetailResponse>(
        `/wallets/${encodeURIComponent(normalized ?? '')}/positions/${encodeURIComponent(trimmedPos ?? '')}`,
        signal,
      ),
    staleTime: staticPreview ? Infinity : 15_000,
    refetchInterval: staticPreview ? false : 15_000,
    refetchIntervalInBackground: false,
    retry(failureCount, error) {
      if (error instanceof InsightsApiError && (error.status === 404 || error.status === 400)) {
        return false
      }
      return failureCount < 2
    },
  })
}

export function formatLeverageX100(levX100: number | null | undefined): string {
  if (levX100 == null || !Number.isFinite(levX100) || levX100 <= 0) return '—'
  const val = levX100 / 100
  return val >= 100 ? `${val.toFixed(0)}×` : `${val.toFixed(val < 10 ? 2 : 1)}×`
}

export function formatDurationCompact(seconds: number | null | undefined): string {
  if (seconds == null || !Number.isFinite(seconds) || seconds < 0) return '—'
  const total = Math.floor(seconds)
  if (total < 60) return `${String(total)}s`
  const days = Math.floor(total / 86_400)
  const hours = Math.floor((total % 86_400) / 3_600)
  const mins = Math.floor((total % 3_600) / 60)
  if (days > 0) return hours > 0 ? `${String(days)}d ${String(hours)}h` : `${String(days)}d`
  if (hours > 0) return mins > 0 ? `${String(hours)}h ${String(mins)}m` : `${String(hours)}h`
  return `${String(mins)}m`
}

export function rawToDisplayPrice(raw: string | null | undefined): number | null {
  if (!raw) return null
  const trimmed = raw.trim()
  if (!/^\d+$/.test(trimmed)) return null
  const num = Number(trimmed)
  if (!Number.isFinite(num) || num <= 0 || num > PLDXY_PRICE_CAP) return null
  return (PLDXY_PRICE_CAP - num) / 1e8
}

export function formatPositionBpsPercent(bps: number | null | undefined, signed = false): string {
  if (bps == null || !Number.isFinite(bps)) return '—'
  const pct = bps / 100
  const prefix = signed && pct > 0 ? '+' : ''
  return `${prefix}${pct.toFixed(2)}%`
}

export function classifyLiquidationBuffer(bps: number | null | undefined): {
  text: string
  label: string
  tone: 'positive' | 'warning' | 'negative' | 'neutral'
} {
  if (bps == null || !Number.isFinite(bps)) {
    return { text: '—', label: 'Unavailable', tone: 'neutral' }
  }
  const text = `${(bps / 100).toFixed(2)}%`
  if (bps <= 50) return { text, label: 'Critical', tone: 'negative' }
  if (bps <= 200) return { text, label: 'Tight', tone: 'warning' }
  return { text, label: 'Safe', tone: 'positive' }
}

export function describeDirectionalBias(longShareBps: number | null | undefined): {
  label: string
  tone: 'positive' | 'negative' | 'neutral'
} {
  if (longShareBps == null || !Number.isFinite(longShareBps)) {
    return { label: 'No exposure yet', tone: 'neutral' }
  }
  if (longShareBps >= 6000) return { label: 'Long biased', tone: 'positive' }
  if (longShareBps <= 4000) return { label: 'Short biased', tone: 'negative' }
  return { label: 'Balanced', tone: 'neutral' }
}
