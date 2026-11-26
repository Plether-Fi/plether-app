import { useQuery } from '@tanstack/react-query'
import { InsightsApiError } from './client'
import type { ApiErrorBody } from './types'

const API_ROOT = '/api/insights/v1'

export interface LiquidityNetworkMeta {
  chainId: number
  releaseVersion: string
  settlementAssetSymbol: string
}

export interface DecodedMask {
  raw: string
  activeBits: number[]
}

export type SettlementReadiness =
  | 'ready'
  | 'ready_with_warnings'
  | 'incomplete'
  | 'blocked'
  | 'faulted'
  | 'paused'

export interface LpSettlementObservation {
  epoch: number
  observedBlock: string
  observedAt: string
  readiness: SettlementReadiness
  executionPath: 'no_matured_work' | 'cached_mark' | 'atomic_oracle_refresh' | 'unknown'
  healthState: 'healthy' | 'degraded' | 'faulted' | 'unknown'
  observationComplete: boolean | null
  hasMaturedWork: boolean | null
  paused: boolean | null
  masks: {
    operationalBlocker: DecodedMask
    warning: DecodedMask
    dependencyFailure: DecodedMask
    criticalFault: DecodedMask
  }
}

export interface LpSettlementSummary24h {
  observedEpochs24h: number
  confirmedSettlements24h: number
  failedOrReverted24h: number
  replacedAttempts24h: number
  deferredEntryEpochs24h: number
  backlogEpochs24h: number
}

export interface LpSettlementRecord {
  epoch: number
  cutoffEpoch: string | null
  status: string
  executionPath: 'no_matured_work' | 'cached_mark' | 'atomic_oracle_refresh' | 'unknown'
  txHash: string | null
  receiptBlock: string | null
  createdAt: string
  confirmedAt: string | null
  confirmationLatencySeconds: number | null
  replacementCount: number
  seniorDepositAssets: string | null
  seniorRedeemAssets: string | null
  juniorDepositAssets: string | null
  juniorRedeemAssets: string | null
  seniorBacklog: boolean | null
  juniorBacklog: boolean | null
  entriesDeferred: boolean | null
}

export interface LiquiditySettlementsResponse {
  network: LiquidityNetworkMeta
  monitorAddress: string
  latestObservation: LpSettlementObservation | null
  summary: LpSettlementSummary24h
  recentSettlements: LpSettlementRecord[]
}

export interface SnapshotFreshnessWindow {
  expectedHours: number
  observedHours: number
  freshHours: number
  staleHours: number
  latestEpochTimestamp: number | null
  latestBlockNumber: string | null
  latestMarkFresh: boolean | null
}

export interface ReturnReportAudit {
  startBlockNumber: string
  endBlockNumber: string
  sourceCommit: string
  schemaVersion: number
  updatedAt: string
  sourceBlockCount: number | null
  reportedReturnPp: number | null
  unexplainedPp: number | null
  netReturnUsdg: string | null
  unexplainedUsdg: string | null
  liquidationFeesUsdg: string | null
  notesCount: number
}

export interface VaultActivityIndexerHealth {
  lastIndexedBlock: string
  safeHeadBlock: string
  lagBlocks: number
  lagSeconds: number
  pollAgeSeconds: number
  backfillComplete: boolean
}

export interface VaultDepositAttributionHealth {
  confirmedThroughBlock: string
  pollAgeSeconds: number
  backfillComplete: boolean
}

export interface LiquidityForensicsResponse {
  network: LiquidityNetworkMeta
  observedAt: string
  snapshotFreshness: {
    window7d: SnapshotFreshnessWindow
    window30d: SnapshotFreshnessWindow
  }
  attributionWorker: {
    indexedThroughBlock: string | null
    recentReports: ReturnReportAudit[]
  }
  vaultIndexers: {
    activity: VaultActivityIndexerHealth | null
    depositAttribution: VaultDepositAttributionHealth | null
  }
}

interface Envelope<T> {
  data: T
}

async function fetchInsightsJson<T>(path: string, signal?: AbortSignal): Promise<T> {
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

export function getLiquiditySettlements(signal?: AbortSignal): Promise<LiquiditySettlementsResponse> {
  return fetchInsightsJson<LiquiditySettlementsResponse>('/liquidity/settlements', signal)
}

export function getLiquidityForensics(signal?: AbortSignal): Promise<LiquidityForensicsResponse> {
  return fetchInsightsJson<LiquidityForensicsResponse>('/liquidity/forensics', signal)
}

function shouldRetryEndpoint(failureCount: number, error: Error): boolean {
  if (error instanceof InsightsApiError && error.status === 404) return false
  return failureCount < 2
}

export function useLiquiditySettlements() {
  return useQuery({
    queryKey: ['insights', 'liquidity', 'settlements'] as const,
    queryFn: ({ signal }) => getLiquiditySettlements(signal),
    staleTime: 30_000,
    refetchInterval: 30_000,
    refetchIntervalInBackground: false,
    retry: shouldRetryEndpoint,
  })
}

export function useLiquidityForensics() {
  return useQuery({
    queryKey: ['insights', 'liquidity', 'forensics'] as const,
    queryFn: ({ signal }) => getLiquidityForensics(signal),
    staleTime: 30_000,
    refetchInterval: 30_000,
    refetchIntervalInBackground: false,
    retry: shouldRetryEndpoint,
  })
}
