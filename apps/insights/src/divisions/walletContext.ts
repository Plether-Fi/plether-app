import { useQuery, useQueryClient } from '@tanstack/react-query'
import { InsightsApiError } from '../api/client'
import type { LiveStanding } from './query'
import type { Tier } from './rules'

export interface DivisionsWalletHistoryRecord {
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

export interface DivisionsCutoffPosition {
  market: string
  side: 'long' | 'short' | 'unknown'
  size: string
  marginAssets: string
  entryPrice: string
  unrealizedPnlAssets: string
  liquidatable: boolean
}

export interface DivisionsCutoffSnapshot {
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
  pendingCarryAssets: string
  economicEquityAssets: string
  hasPosition: boolean
  position: DivisionsCutoffPosition | null
}

export interface DivisionsEquityBridge {
  startingEquityAssets: string
  depositsAssets: string
  withdrawalsAssets: string
  adjustedEquityAssets: string
  cutoffEquityAssets: string
  weeklyPnlAssets: string
  pendingCarryAssets: string
}

export interface DivisionsWeekActivity {
  tradesCount: number
  opensCount: number
  closesCount: number
  liquidationsCount: number
  volumeAssets: string
  realizedPnlAssets: string
  depositsAssets: string
  withdrawalsAssets: string
}

export interface DivisionsWeekContext {
  weekStart: number
  weekStartsAt: string
  weekClosesAt: string
  baselineBlock: string
  cutoffBlock: string
  cutoffAt: string
  releaseVersion: string
  assetSymbol: 'USDC' | 'USDG'
  activeWeekdays: [boolean, boolean, boolean, boolean, boolean]
  activity: DivisionsWeekActivity
  equityBridge: DivisionsEquityBridge | null
  cutoffSnapshot: DivisionsCutoffSnapshot | null
}

export interface DivisionsWalletContextResponse {
  network: {
    chainId: number
    releaseVersion: string
    settlementAssetSymbol: string
  }
  account: string
  observedAt: string
  identity: {
    discordUsername: string | null
    isDivisionsLinked: boolean
  }
  liveWeek: DivisionsWeekContext | null
  publishedWeek: DivisionsWeekContext | null
  history: DivisionsWalletHistoryRecord[]
}

interface Envelope<T> {
  data: T
}

async function fetchDivisionsWalletContext(
  address: string,
  signal?: AbortSignal,
): Promise<DivisionsWalletContextResponse> {
  const response = await fetch(
    `/api/insights/v1/divisions/wallets/${encodeURIComponent(address.toLowerCase())}`,
    {
      signal,
      headers: { Accept: 'application/json' },
    },
  )
  if (!response.ok) {
    throw new InsightsApiError(
      `Divisions wallet context unavailable (${String(response.status)})`,
      response.status,
    )
  }
  const parsed = (await response.json()) as
    | DivisionsWalletContextResponse
    | Envelope<DivisionsWalletContextResponse>
  if ('data' in parsed) {
    return parsed.data
  }
  return parsed
}

export function useDivisionsWalletContext(address: string | undefined) {
  const queryClient = useQueryClient()
  const staticPreview = queryClient.getDefaultOptions().queries?.refetchInterval === false
  const normalized = address?.trim().toLowerCase()
  return useQuery({
    queryKey: ['insights', 'divisions', 'wallet-context', normalized] as const,
    enabled: Boolean(normalized) && !staticPreview,
    queryFn: ({ signal }) => fetchDivisionsWalletContext(normalized ?? '', signal),
    staleTime: staticPreview ? Infinity : 30_000,
    refetchInterval: staticPreview ? false : 60_000,
    refetchIntervalInBackground: false,
    retry(failureCount, error) {
      if (error instanceof InsightsApiError && (error.status === 404 || error.status === 400)) {
        return false
      }
      return failureCount < 1
    },
  })
}

export interface LiveTierProjection {
  totalRanked: number
  profitableCount: number
  projectedTier: Tier
  percentileAmongProfitable: number | null
  nextTier: Tier | null
  bpsBehindNextTier: number | null
}

const PROFITABLE_TIER_CUTOFFS: readonly { tier: Tier; percent: number }[] = [
  { tier: 'Diamond', percent: 5 },
  { tier: 'Platinum', percent: 10 },
  { tier: 'Gold', percent: 20 },
  { tier: 'Silver', percent: 50 },
]

/**
 * Computes provisional tier projection and exact bps gap to the next tier cutoff
 * from the live Divisions standings using the canonical profitable-trader percentiles.
 */
export function computeLiveTierProjection(
  standings: LiveStanding[] | undefined,
  targetAddress: string,
): LiveTierProjection | null {
  if (!standings || standings.length === 0) return null
  const lowerTarget = targetAddress.toLowerCase()
  const row = standings.find((s) => s.address.toLowerCase() === lowerTarget)
  if (!row) return null

  const ranked = standings.filter((s) => s.rank != null)
  const profitable = ranked
    .filter((s) => (s.weeklyReturnBps ?? 0) > 0)
    .sort((a, b) => (a.rank ?? Number.MAX_SAFE_INTEGER) - (b.rank ?? Number.MAX_SAFE_INTEGER))

  const totalRanked = ranked.length
  const profitableCount = profitable.length
  const bps = row.weeklyReturnBps

  if (row.rank == null || bps == null || bps <= 0 || profitableCount === 0) {
    return {
      totalRanked,
      profitableCount,
      projectedTier: 'Iron',
      percentileAmongProfitable: null,
      nextTier: 'Bronze',
      bpsBehindNextTier: bps != null && bps <= 0 ? Math.abs(bps) + 1 : null,
    }
  }

  let projectedTier: Tier = 'Bronze'
  let currentCutoffIdx = -1
  for (let i = 0; i < PROFITABLE_TIER_CUTOFFS.length; i += 1) {
    const cutoff = PROFITABLE_TIER_CUTOFFS[i]
    const maxRank = Math.floor((profitableCount * cutoff.percent + 99) / 100)
    if (row.rank <= maxRank) {
      projectedTier = cutoff.tier
      currentCutoffIdx = i
      break
    }
  }

  const percentileAmongProfitable = Math.min(100, (row.rank / profitableCount) * 100)

  let nextTier: Tier | null = null
  let bpsBehindNextTier: number | null = null
  const nextCutoffIndex =
    currentCutoffIdx === -1
      ? PROFITABLE_TIER_CUTOFFS.length - 1
      : currentCutoffIdx > 0
        ? currentCutoffIdx - 1
        : null

  if (nextCutoffIndex !== null) {
    const targetCutoff = PROFITABLE_TIER_CUTOFFS[nextCutoffIndex]
    nextTier = targetCutoff.tier
    const targetRankSlot = Math.max(
      1,
      Math.floor((profitableCount * targetCutoff.percent + 99) / 100),
    )
    const boundaryTrader = profitable.at(targetRankSlot - 1)
    if (boundaryTrader?.weeklyReturnBps != null && boundaryTrader.weeklyReturnBps >= bps) {
      bpsBehindNextTier = Math.max(1, boundaryTrader.weeklyReturnBps - bps)
    }
  }

  return {
    totalRanked,
    profitableCount,
    projectedTier,
    percentileAmongProfitable,
    nextTier,
    bpsBehindNextTier,
  }
}
