import { useQuery } from '@tanstack/react-query'
import { InsightsApiError } from './client'
import type { LiquidityNetworkMeta } from './liquidity'
import type { ApiErrorBody } from './types'

const API_ROOT = '/api/insights/v1'

export interface PendingTreasuryRotation {
  treasury: string
  activationTime: string
  lockedUntil: string | null
}

export interface TreasuryIdentity {
  address: string | null
  addressSource: 'onchain' | 'manifest' | 'unavailable'
  deploymentTreasuryAddress: string | null
  lockedUntil: string | null
  isLocked: boolean
  pendingRotation: PendingTreasuryRotation | null
}

export interface TreasuryFeePolicy {
  executionFeeBps: number | null
  liquidationProtocolShareBps: number | null
  liquidationKeeperShareBps: number | null
  juniorMaintenanceFeeAprBps: number | null
  juniorMaintenanceFeeRecipient: string | null
  pendingMaintenanceFeeShares: string | null
  pendingMaintenanceFeeAssets: string | null
}

export interface TreasuryHoldings {
  totalUsdgAssets: string | null
  clearinghouseSettlementAssets: string | null
  clearinghouseFreeAssets: string | null
  clearinghouseClaimAssets: string | null
  clearinghouseHasPosition: boolean | null
  walletUsdgAssets: string | null
  walletEthWei: string | null
  juniorVaultShares: string | null
  juniorVaultAssets: string | null
  seniorVaultShares: string | null
  seniorVaultAssets: string | null
  pendingMaintenanceFeeAssets: string | null
}

export interface TreasuryWindowSummary {
  executedOrders: number
  volumeAssets: string
  executionFeeAssessedAssets: string
  executionFeeCreditedAssets: string
  effectiveFeeBps: number | null
  frozenSpreadAssessedAssets: string
  keeperBountyAssets: string
  liquidationCount: number
  liquidationKeeperBountyAssets: string
  closeAssistanceCount: number
  closeAssistanceUsdgAssets: string
  sponsoredOpsCount: number
  sponsoredGasSpentWei: string
  netUsdgSurplusAssets: string
}

export interface DailyRevenuePoint {
  dayStartUnix: number
  dayUtc: string
  executedOrders: number
  volumeAssets: string
  executionFeeAssets: string
  cumulativeExecutionFeeAssets: string
  closeAssistanceAssets: string
  sponsoredGasWei: string
}

export interface TopFeeOrderRow {
  orderId: string
  txHash: string
  account: string
  side: string
  volumeAssets: string
  executionFeeAssets: string
  effectiveFeeBps: number | null
  blockNumber: string
  occurredAt: string
}

export interface TreasuryMovementRow {
  kind: string
  direction: string
  amountAssets: string
  counterparty: string | null
  txHash: string
  blockNumber: string
  occurredAt: string
}

export interface TreasuryOverviewResponse {
  blockNumber: string | null
  network: LiquidityNetworkMeta
  observedAt: string
  rpcAvailable: boolean
  treasury: TreasuryIdentity
  feePolicy: TreasuryFeePolicy
  holdings: TreasuryHoldings
  profitOverview: {
    annualizedExecutionFeeRunRate7dAssets: string | null
    window24h: TreasuryWindowSummary
    window7d: TreasuryWindowSummary
    window30d: TreasuryWindowSummary
    allTime: TreasuryWindowSummary
  }
  dailySeries: DailyRevenuePoint[]
  recentFeeOrders: TopFeeOrderRow[]
  treasuryMovements: TreasuryMovementRow[]
}

export async function fetchTreasuryOverview(signal?: AbortSignal): Promise<TreasuryOverviewResponse> {
  const response = await fetch(`${API_ROOT}/treasury/overview`, {
    signal,
    headers: { Accept: 'application/json' },
  })
  const raw = await response.text()
  let parsed: unknown = null
  if (raw) {
    try {
      parsed = JSON.parse(raw)
    } catch {
      parsed = null
    }
  }
  if (!response.ok) {
    const body = parsed as ApiErrorBody | null
    throw new InsightsApiError(
      body?.error?.message ?? `Request failed (${String(response.status)})`,
      response.status,
      body?.error?.code,
    )
  }
  const envelope = parsed as { data?: Omit<TreasuryOverviewResponse, 'blockNumber'>; blockNumber?: string | number | null } | null
  if (envelope?.data === undefined) {
    throw new InsightsApiError('Malformed API response', response.status)
  }
  return {
    ...envelope.data,
    blockNumber: envelope.blockNumber != null ? String(envelope.blockNumber) : null,
  }
}

export function useTreasuryOverview() {
  return useQuery({
    queryKey: ['insights', 'treasury', 'overview'],
    queryFn: ({ signal }) => fetchTreasuryOverview(signal),
    staleTime: 15_000,
    refetchInterval: 30_000,
  })
}
