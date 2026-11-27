import { useQuery, useQueryClient } from '@tanstack/react-query'
import type { StatusTone } from '../components/StatusPill'
import { InsightsApiError } from './client'
import type { ApiErrorBody } from './types'

const API_ROOT = '/api/insights/v1'
const WEI_PER_ETH = 1_000_000_000_000_000_000n

export type SponsoredStack = 'native' | 'kernel' | 'none'
export type SponsoredGateStatus = 'issuing' | 'paused' | 'disabled' | 'degraded'
export type SponsoredRolloutMode = 'global' | 'canary' | 'disabled'
export type SponsoredOpStatusFilter = 'all' | 'succeeded' | 'reverted'

export interface SponsoredNetworkMeta {
  chainId: number
  releaseVersion: string
  settlementAssetSymbol: string
}

export interface SponsoredGateState {
  status: SponsoredGateStatus
  sponsorshipEnabled: boolean
  submissionEnabled: boolean
  rolloutMode: SponsoredRolloutMode
  issuancePaused: boolean | null
  pausedReason: string | null
  paymasterOnchainPaused: boolean | null
  updatedAt: string | null
}

export interface SponsoredPaymasterHealth {
  entryPointDepositWei: string | null
  minDepositWei: string | null
  freeHeadroomWei: string | null
  depositHealthy: boolean | null
  burn24hWei: string | null
  estimatedRunwayHours: number | null
  altoFundingState: string
  altoFundingReason: string
}

export interface SponsoredBudgetGauge {
  usedWei: string
  capWei: string
  utilizationBps: number | null
}

export interface SponsoredBudgets {
  maxCostPerOpWei: string
  accountHourlyCapWei: string
  globalOutstanding: SponsoredBudgetGauge
  globalHourly: SponsoredBudgetGauge
  globalDaily: SponsoredBudgetGauge
}

export interface SponsoredReconcilerState {
  safeBlock: string | null
  headBlock: string | null
  blocksBehind: number | null
  lastSuccessAt: string | null
  lagSeconds: number | null
  maxSafeLagSeconds: number | null
  fresh: boolean
}

export interface SponsoredBundlerState {
  reachable: boolean
  entryPointSupported: boolean
  status: string
}

export interface SponsoredWindowMetrics {
  totalOps: number
  succeededOps: number
  revertedOps: number
  successRateBps: number | null
  gasSpentWei: string
  gasEfficiencyBps: number | null
}

export interface SponsoredFunnel24h {
  reserved: number
  signed: number
  submitted: number
  settled: number
  expired: number
  cancelled: number
}

export interface SponsoredExecutionSummary {
  window1h: SponsoredWindowMetrics
  window24h: SponsoredWindowMetrics
  window7d: SponsoredWindowMetrics
  latencyP50Seconds: number | null
  latencyP95Seconds: number | null
  funnel24h: SponsoredFunnel24h
  activeUnfinalizedCount: number
  oldestUnfinalizedAgeSeconds: number | null
}

export interface SponsoredCloseAssistance {
  available: boolean
  verifiedCount24h: number
  verifiedCount7d: number
  totalAssisted7dAssets: string
  pendingUnverifiedCount: number
  verificationRateBps: number | null
}

export interface SponsoredStatusResponse {
  network: SponsoredNetworkMeta
  observedAt: string
  configured: boolean
  stack: SponsoredStack
  entryPointVersion: string | null
  entryPointAddress: string | null
  paymasterAddress: string | null
  gate: SponsoredGateState
  paymaster: SponsoredPaymasterHealth
  budgets: SponsoredBudgets | null
  reconciler: SponsoredReconcilerState
  bundler: SponsoredBundlerState
  execution: SponsoredExecutionSummary
  closeAssistance: SponsoredCloseAssistance
}

export interface SponsoredOperationRecord {
  userOpHash: string
  txHash: string
  sender: string
  blockNumber: string
  success: boolean
  actualGasCostWei: string
  maxCostWei: string
  efficiencyBps: number | null
  latencySeconds: number | null
  occurredAt: string
  finalizedAt: string
}

export interface SponsoredControlEvent {
  id: number
  action: string
  reason: string
  createdAt: string
}

export interface SponsoredOperationsResponse {
  network: SponsoredNetworkMeta
  observedAt: string
  configured: boolean
  stack: SponsoredStack
  operations: SponsoredOperationRecord[]
  controlEvents: SponsoredControlEvent[]
}

interface Envelope<T> {
  data: T
}

async function fetchSponsoredJson<T>(path: string, signal?: AbortSignal): Promise<T> {
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

export function getSponsoredStatus(signal?: AbortSignal): Promise<SponsoredStatusResponse> {
  return fetchSponsoredJson<SponsoredStatusResponse>('/sponsored/status', signal)
}

export function getSponsoredOperations(
  status: SponsoredOpStatusFilter = 'all',
  limit = 50,
  signal?: AbortSignal,
): Promise<SponsoredOperationsResponse> {
  const params = new URLSearchParams({ limit: String(limit) })
  if (status !== 'all') params.set('status', status)
  return fetchSponsoredJson<SponsoredOperationsResponse>(
    `/sponsored/operations?${params.toString()}`,
    signal,
  )
}

function shouldRetrySponsored(failureCount: number, error: Error): boolean {
  if (error instanceof InsightsApiError && error.status === 404) return false
  return failureCount < 2
}

export function useSponsoredStatus() {
  const queryClient = useQueryClient()
  const staticPreview = queryClient.getDefaultOptions().queries?.refetchInterval === false
  return useQuery({
    queryKey: ['insights', 'sponsored', 'status'] as const,
    queryFn: ({ signal }) => getSponsoredStatus(signal),
    enabled: !staticPreview,
    staleTime: staticPreview ? Infinity : 15_000,
    refetchInterval: staticPreview ? false : 15_000,
    refetchIntervalInBackground: false,
    retry: shouldRetrySponsored,
  })
}

export function useSponsoredOperations(status: SponsoredOpStatusFilter = 'all', limit = 50) {
  const queryClient = useQueryClient()
  const staticPreview = queryClient.getDefaultOptions().queries?.refetchInterval === false
  return useQuery({
    queryKey: ['insights', 'sponsored', 'operations', status, limit] as const,
    queryFn: ({ signal }) => getSponsoredOperations(status, limit, signal),
    enabled: !staticPreview,
    staleTime: staticPreview ? Infinity : 15_000,
    refetchInterval: staticPreview ? false : 15_000,
    refetchIntervalInBackground: false,
    retry: shouldRetrySponsored,
  })
}

export function formatSponsoredEthWei(
  wei: string | null | undefined,
  decimals = 4,
): string {
  if (wei == null) return 'Unavailable'
  const trimmed = wei.trim()
  if (!/^\d+$/.test(trimmed)) return 'Unavailable'

  const value = BigInt(trimmed)
  const scale = 10n ** BigInt(decimals)
  const scaled = (value * scale + WEI_PER_ETH / 2n) / WEI_PER_ETH
  if (value > 0n && scaled === 0n) {
    return `< 0.${'0'.repeat(Math.max(0, decimals - 1))}1 ETH`
  }
  const whole = scaled / scale
  const frac = scaled % scale
  const wholeText = new Intl.NumberFormat('en-US', { maximumFractionDigits: 0 }).format(whole)
  return `${wholeText}.${frac.toString().padStart(decimals, '0')} ETH`
}

export function formatBpsPercent(bps: number | null | undefined): string {
  if (bps == null || !Number.isFinite(bps)) return '—'
  return `${(bps / 100).toFixed(2)}%`
}

export function formatRunwayHours(hours: number | null | undefined): string {
  if (hours == null || !Number.isFinite(hours) || hours < 0) return 'No 24h burn'
  if (hours < 24) return `${String(hours)}h`
  const days = Math.floor(hours / 24)
  if (days < 7) {
    const remHours = hours % 24
    return remHours > 0 ? `${String(days)}d ${String(remHours)}h` : `${String(days)}d`
  }
  return `${days.toLocaleString('en-US')}d`
}

export function formatDurationSeconds(seconds: number | null | undefined): string {
  if (seconds == null || !Number.isFinite(seconds) || seconds < 0) return 'Unavailable'
  const s = Math.round(seconds)
  if (s < 60) return `${String(s)}s`
  if (s < 3_600) {
    const mins = Math.floor(s / 60)
    const rem = s % 60
    return rem > 0 ? `${String(mins)}m ${String(rem)}s` : `${String(mins)}m`
  }
  const hrs = Math.floor(s / 3_600)
  const remMins = Math.floor((s % 3_600) / 60)
  return remMins > 0 ? `${String(hrs)}h ${String(remMins)}m` : `${String(hrs)}h`
}

export function budgetGaugeTone(bps: number | null | undefined): StatusTone {
  if (bps == null || !Number.isFinite(bps)) return 'unknown'
  if (bps >= 8_500) return 'blocked'
  if (bps >= 6_000) return 'degraded'
  return 'live'
}

export function deriveSponsoredBanner(data: SponsoredStatusResponse): {
  tone: StatusTone
  label: string
  note: string
} {
  if (!data.configured) {
    return {
      tone: 'unknown',
      label: 'Sponsorship not configured',
      note: 'No native or Kernel paymaster sponsorship backend is active on this deployment',
    }
  }

  const stackLabel =
    data.stack === 'kernel'
      ? `Kernel AA · EntryPoint v${data.entryPointVersion ?? '0.9'}`
      : `Native AA · EntryPoint v${data.entryPointVersion ?? '0.8'}`

  if (data.gate.status === 'disabled') {
    return {
      tone: 'unknown',
      label: 'Sponsorship disabled',
      note: `${stackLabel} · Paymaster issuance flag is turned off`,
    }
  }

  if (data.gate.status === 'paused') {
    const detail = data.gate.paymasterOnchainPaused
      ? 'Paymaster contract paused on-chain'
      : data.gate.pausedReason ?? 'Issuance kill-switch engaged'
    return {
      tone: 'blocked',
      label: 'Sponsorship paused',
      note: `${stackLabel} · ${detail}`,
    }
  }

  if (data.paymaster.depositHealthy === false) {
    return {
      tone: 'blocked',
      label: 'Low paymaster deposit',
      note: `${stackLabel} · EntryPoint balance is below safety floor or outstanding liability`,
    }
  }

  if (data.gate.status === 'degraded' || !data.reconciler.fresh || !data.bundler.reachable) {
    const reasons: string[] = []
    if (!data.reconciler.fresh) reasons.push('AA reconciler lag')
    if (!data.bundler.reachable) reasons.push('Alto bundler unreachable')
    else if (!data.bundler.entryPointSupported) reasons.push('EntryPoint unsupported on bundler')
    return {
      tone: 'degraded',
      label: 'Sponsorship degraded',
      note: `${stackLabel} · ${reasons.length > 0 ? reasons.join(' · ') : 'Verification telemetry incomplete'}`,
    }
  }

  if (data.paymaster.altoFundingState === 'blocked') {
    return {
      tone: 'blocked',
      label: 'Alto executor low ETH',
      note: `${stackLabel} · Bundler executor reports ${data.paymaster.altoFundingReason}`,
    }
  }

  if (data.paymaster.altoFundingReason === 'FUNDING_LOW') {
    return {
      tone: 'degraded',
      label: 'Issuing · Alto ETH low',
      note: `${stackLabel} · Rollout: ${data.gate.rolloutMode}`,
    }
  }

  return {
    tone: 'live',
    label: data.gate.rolloutMode === 'canary' ? 'Issuing (Canary)' : 'Issuing live',
    note: `${stackLabel} · Reconciler fresh & Alto bundler ready`,
  }
}

export function altoFundingPill(
  state: string,
  reason: string,
): { tone: StatusTone; label: string } {
  if (state === 'blocked' || reason === 'WORKER_INSUFFICIENT_FUNDS') {
    return { tone: 'blocked', label: 'Insufficient ETH' }
  }
  if (reason === 'FUNDING_LOW') {
    return { tone: 'degraded', label: 'Funding low' }
  }
  if (state === 'ready' && reason === 'READY') {
    return { tone: 'live', label: 'Ready' }
  }
  return { tone: 'unknown', label: 'Unverified' }
}
