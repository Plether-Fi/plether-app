import { useQuery } from '@tanstack/react-query'
import { InsightsApiError } from './client'
import type { LiquidityNetworkMeta } from './liquidity'
import type { ApiErrorBody } from './types'

const API_ROOT = '/api/insights/v1'

export type KernelStackStatus = 'healthy' | 'degraded' | 'paused' | 'disabled' | 'unconfigured'

export interface KernelStackProfile {
  deployment: string
  accountVersion: string
  entryPointVersion: string
  entryPoint: string
  entryPointCodeHash: string
  factory: string
  factoryCodeHash: string
  implementation: string
  implementationCodeHash: string
  validator: string
  validatorCodeHash: string
  accountCodeHash: string
  paymaster: string | null
  policyId: string | null
}

export interface KernelGatewayControls {
  ethModeEnabled: boolean
  sponsorshipEnabled: boolean
  protectionCommitsEnabled: boolean
  ownerAdmissionMode: 'canary' | 'open'
  admittedOwnerCount: number | null
}

export interface KernelPipelineHealth {
  bundlerReachable: boolean
  bundlerEntryPointSupported: boolean
  bundlerStatus: string
  reconcilerSafeBlock: string | null
  reconcilerHeadBlock: string | null
  reconcilerBlocksBehind: number | null
  reconcilerLagSeconds: number | null
  reconcilerMaxSafeLagSeconds: number
  reconcilerFresh: boolean
  reconcilerPausedReason: string | null
}

export interface KernelAdoptionMetrics {
  uniqueAccounts: number
  uniqueOwners: number
  activeAccounts24h: number
  totalPreparations: number
  totalOperations: number
  operations24h: number
  sponsoredOpsTotal: number
  ethOpsTotal: number
  sponsoredOps24h: number
  ethOps24h: number
  cancellationOpsTotal: number
  pendingOpsCount: number
  oldestPendingAgeSeconds: number | null
}

export interface KernelLifecycleMetrics {
  includedSucceeded: number
  includedReverted: number
  pending: number
  nonceConsumed: number
  sponsorshipExpired: number
  cancellationsPrepared: number
  cancellationsAccepted: number
  successRateBps: number | null
  totalGasCostWei: string
  sponsoredGasCostWei: string
  ethGasCostWei: string
}

export interface KernelSmartAccountItem {
  sender: string
  ownerAddress: string
  discordUsername: string | null
  sponsoredCount: number
  ethCount: number
  includedCount: number
  pendingCount: number
  retiredCount: number
  highestNonce: string
  lastMode: 'sponsored' | 'eth'
  firstSeenAt: string
  lastActivityAt: string
}

export interface KernelOperationItem {
  userOpHash: string
  sender: string
  ownerAddress: string
  mode: 'sponsored' | 'eth'
  cancellationOnly: boolean
  nonce: string
  status: 'pending' | 'included' | 'nonce_consumed' | 'sponsorship_expired'
  executionSuccess: boolean | null
  txHash: string | null
  blockNumber: string | null
  actualGasCostWei: string | null
  cancellationHash: string | null
  createdAt: string
}

export interface TradingAccountsOverviewResponse {
  network: LiquidityNetworkMeta
  observedAt: string
  configured: boolean
  status: KernelStackStatus
  profile: KernelStackProfile | null
  controls: KernelGatewayControls | null
  pipeline: KernelPipelineHealth | null
  adoption: KernelAdoptionMetrics
  lifecycle: KernelLifecycleMetrics
  accounts: KernelSmartAccountItem[]
  operations: KernelOperationItem[]
}

interface Envelope<T> {
  data: T
}

async function fetchTradingAccountsJson<T>(path: string, signal?: AbortSignal): Promise<T> {
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

export type KernelPaymentMode = 'sponsored' | 'eth'

export interface KernelAccountOnchainState {
  rpcAvailable: boolean
  deployed: boolean
  codeHash: string | null
  codeHashMatchesPinned: boolean | null
  ethBalanceWei: string | null
  entryPointNonce: string | null
  dbHighestNonce: string | null
  nonceSynced: boolean | null
}

export interface KernelAccountSponsorshipQuota {
  enabled: boolean
  hourlyUsedWei: string
  hourlyCapWei: string
  hourlyUtilizationBps: number | null
  outstandingUsedWei: string
  outstandingCapWei: string
  outstandingUtilizationBps: number | null
  maxCostPerOpWei: string
  activeReservedCount: number
}

export interface KernelAccountDetailSummary {
  totalPreparations: number
  unsubmittedPreparations: number
  totalOperations: number
  operations24h: number
  sponsoredCount: number
  ethCount: number
  cancellationOpsCount: number
  includedSucceeded: number
  includedReverted: number
  pendingCount: number
  nonceConsumed: number
  sponsorshipExpired: number
  cancellationsPrepared: number
  cancellationsAccepted: number
  successRateBps: number | null
  totalGasCostWei: string
  sponsoredGasCostWei: string
  ethGasCostWei: string
  oldestPendingAgeSeconds: number | null
}

export interface KernelAccountPendingOp {
  userOpHash: string
  preparationId: string
  mode: KernelPaymentMode
  cancellationOnly: boolean
  nonce: string
  ageSeconds: number
  preparedSafeBlock: string | null
  nonceScanThroughBlock: string | null
  sponsorshipValidUntil: string | null
  reservedMaxCostWei: string | null
  cancellationPrepared: boolean
  cancellationHash: string | null
  createdAt: string
}

export interface KernelAccountOperationItem {
  userOpHash: string
  preparationId: string
  mode: KernelPaymentMode
  cancellationOnly: boolean
  nonce: string
  status: 'pending' | 'included' | 'nonce_consumed' | 'sponsorship_expired'
  executionSuccess: boolean | null
  reservedMaxCostWei: string | null
  actualGasCostWei: string | null
  txHash: string | null
  blockNumber: string | null
  cancellationHash: string | null
  createdAt: string
}

export interface KernelAccountCancellationItem {
  cancellationPreparationId: string
  originalHash: string
  cancellationHash: string | null
  nonce: string
  accepted: boolean
  expiresAt: string
  createdAt: string
}

export interface KernelAccountUnsubmittedPrep {
  preparationId: string
  mode: KernelPaymentMode
  nonce: string
  preparedSafeBlock: string | null
  nonceScanThroughBlock: string | null
  expired: boolean
  expiresAt: string
  createdAt: string
}

export interface TradingAccountDetailResponse {
  network: LiquidityNetworkMeta
  observedAt: string
  configured: boolean
  sender: string
  found: boolean
  ownerAddress: string | null
  discordUsername: string | null
  firstSeenAt: string | null
  lastActivityAt: string | null
  profile: {
    deployment: string
    accountVersion: string
    entryPointVersion: string
    entryPoint: string
    accountCodeHash: string
    paymaster: string | null
  } | null
  onchain: KernelAccountOnchainState
  sponsorship: KernelAccountSponsorshipQuota | null
  summary: KernelAccountDetailSummary
  pendingOperations: KernelAccountPendingOp[]
  operations: KernelAccountOperationItem[]
  cancellations: KernelAccountCancellationItem[]
  unsubmittedPreparations: KernelAccountUnsubmittedPrep[]
}

export function useTradingAccountsOverview() {
  return useQuery({
    queryKey: ['insights', 'trading-accounts', 'overview'] as const,
    queryFn: ({ signal }) =>
      fetchTradingAccountsJson<TradingAccountsOverviewResponse>(
        '/trading-accounts/overview',
        signal,
      ),
    staleTime: 15_000,
    refetchInterval: 20_000,
    refetchIntervalInBackground: false,
    retry(failureCount, error) {
      if (error instanceof InsightsApiError && (error.status === 404 || error.status === 400)) {
        return false
      }
      return failureCount < 2
    },
  })
}

export function useTradingAccountDetail(address: string) {
  const normalized = address.trim().toLowerCase()
  const isValid = /^0x[0-9a-f]{40}$/.test(normalized)
  return useQuery({
    queryKey: ['insights', 'trading-accounts', 'detail', normalized] as const,
    queryFn: ({ signal }) =>
      fetchTradingAccountsJson<TradingAccountDetailResponse>(
        `/trading-accounts/${encodeURIComponent(normalized)}`,
        signal,
      ),
    ...(isValid ? {} : { enabled: false }),
    staleTime: 10_000,
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
