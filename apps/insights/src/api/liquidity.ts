import { useQuery, useQueryClient } from '@tanstack/react-query'
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
  labels?: string[]
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

export interface EpochTrancheSnapshot {
  totalAssets: string
  lockedAssets: string | null
  totalSupply: string
  sharePriceWad: string
  sharePriceDeltaBps: number | null
}

export interface EpochVaultSnapshot {
  epochTimestamp: number
  blockNumber: string
  blockTimestamp: number
  blockUtc: string
  markFresh: boolean | null
  senior: EpochTrancheSnapshot
  junior: EpochTrancheSnapshot
}

export interface LiquidityEpochResponse {
  network: LiquidityNetworkMeta
  monitorAddress: string
  epoch: number
  epochTimestamp: number
  epochUtc: string
  previousEpoch: number | null
  nextEpoch: number | null
  latestObservation: LpSettlementObservation | null
  observations: LpSettlementObservation[]
  settlements: LpSettlementRecord[]
  vaultSnapshot: EpochVaultSnapshot | null
  coveringReport: ReturnReportAudit | null
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

export function getLiquidityEpoch(
  epoch: string | number,
  signal?: AbortSignal,
): Promise<LiquidityEpochResponse> {
  return fetchInsightsJson<LiquidityEpochResponse>(
    `/liquidity/epochs/${encodeURIComponent(String(epoch))}`,
    signal,
  )
}

async function fetchLiquidityEpochWithFallback(
  epoch: string,
  signal?: AbortSignal,
): Promise<LiquidityEpochResponse> {
  try {
    return await getLiquidityEpoch(epoch, signal)
  } catch (error) {
    // Before /liquidity/epochs/:epoch is deployed on the backend, hydrate recent
    // epochs from /liquidity/settlements so clicking any settled epoch works immediately.
    if (error instanceof InsightsApiError && error.status === 404 && !error.code) {
      const targetEpoch = Number(epoch)
      const settlementsResp = await getLiquiditySettlements(signal)
      const matchingTxs = settlementsResp.recentSettlements.filter(
        (row) => row.epoch === targetEpoch,
      )
      const matchingObs =
        settlementsResp.latestObservation?.epoch === targetEpoch
          ? settlementsResp.latestObservation
          : null
      if (matchingTxs.length > 0 || matchingObs) {
        const knownEpochs = Array.from(
          new Set([
            ...settlementsResp.recentSettlements.map((r) => r.epoch),
            ...(settlementsResp.latestObservation ? [settlementsResp.latestObservation.epoch] : []),
          ]),
        ).sort((a, b) => a - b)
        const idx = knownEpochs.indexOf(targetEpoch)
        const epochTs = targetEpoch * 3600
        return {
          network: settlementsResp.network,
          monitorAddress: settlementsResp.monitorAddress,
          epoch: targetEpoch,
          epochTimestamp: epochTs,
          epochUtc: new Date(epochTs * 1000).toISOString(),
          previousEpoch: idx > 0 ? knownEpochs[idx - 1] : null,
          nextEpoch: idx >= 0 && idx < knownEpochs.length - 1 ? knownEpochs[idx + 1] : null,
          latestObservation: matchingObs,
          observations: matchingObs ? [matchingObs] : [],
          settlements: matchingTxs,
          vaultSnapshot: null,
          coveringReport: null,
        }
      }
    }
    throw error
  }
}

export function formatSharePriceWad(wad: string | null | undefined, symbol = 'USDG'): string {
  if (!wad) return '—'
  try {
    const raw = BigInt(wad)
    const sign = raw < 0n ? '-' : ''
    const abs = raw < 0n ? -raw : raw
    const whole = abs / 1_000_000_000_000_000_000n
    const frac = ((abs % 1_000_000_000_000_000_000n) / 100_000_000_000_000n)
      .toString()
      .padStart(4, '0')
    return `${sign}${whole.toLocaleString('en-US')}.${frac} ${symbol}`
  } catch {
    return '—'
  }
}

function shouldRetryEndpoint(failureCount: number, error: Error): boolean {
  if (error instanceof InsightsApiError && (error.status === 404 || error.status === 400)) return false
  return failureCount < 2
}

export interface PoolEfficiencyBucket {
  bucketStart: string
  bucketEnd: string
  volumeAssets: string
  feesAssets: string
  tradesCount: number
  poolTvlAssets: string | null
  seniorTvlAssets: string | null
  juniorTvlAssets: string | null
  turnoverX100: number | null
  annualizedTurnoverDailyX100: number | null
  takeRateBpsX100: number | null
  longOiAssets: string
  shortOiAssets: string
  grossOiAssets: string
  netSkewAssets: string
  grossOiToPoolBps: number | null
  netSkewToPoolBps: number | null
}

export interface PoolEfficiencySummary {
  poolTvlAssets: string | null
  seniorTvlAssets: string | null
  juniorTvlAssets: string | null
  volume24hAssets: string
  volume7dAssets: string
  volume30dAssets: string
  fees24hAssets: string
  fees7dAssets: string
  turnover24hX100: number | null
  turnover7dAvgDailyX100: number | null
  turnover30dAvgDailyX100: number | null
  effectiveTakeRate7dBpsX100: number | null
  annualizedFeeYield7dBps: number | null
  currentLongOiAssets: string
  currentShortOiAssets: string
  currentGrossOiAssets: string
  currentNetSkewAssets: string
  grossOiToPoolBps: number | null
  netSkewToPoolBps: number | null
  netSkewToJuniorBps: number | null
}

export interface LiquidityEfficiencyResponse {
  network: LiquidityNetworkMeta
  observedAt: string
  summary: PoolEfficiencySummary
  series7d: PoolEfficiencyBucket[]
  series30d: PoolEfficiencyBucket[]
}

export function getLiquidityEfficiency(signal?: AbortSignal): Promise<LiquidityEfficiencyResponse> {
  return fetchInsightsJson<LiquidityEfficiencyResponse>('/liquidity/efficiency', signal)
}

export function formatTurnoverX100(x100: number | null | undefined): string {
  if (x100 == null || !Number.isFinite(x100)) return '—'
  const val = x100 / 100
  if (val >= 100) return `${val.toFixed(1)}×`
  return `${val.toFixed(2)}×`
}

export function formatPoolBpsPercent(bps: number | null | undefined, digits = 1): string {
  if (bps == null || !Number.isFinite(bps)) return '—'
  return `${(bps / 100).toFixed(digits)}%`
}

export function formatTakeRateBpsX100(bpsX100: number | null | undefined): string {
  if (bpsX100 == null || !Number.isFinite(bpsX100)) return '—'
  return `${(bpsX100 / 100).toFixed(2)} bps`
}

export interface LiquiditySolvencySnapshot {
  poolTotalAssets: string | null
  poolRawAssets: string | null
  excessAssets: string | null
  terminalDeficitAssets: string | null
  freeSettlementAssets: string | null
  withdrawalReserveAssets: string | null
  effectiveSolvencyAssets: string | null
  solvencyCoverageBps: number | null
  freeLiquidityShareBps: number | null
  maxDirectionalLiabilityAssets: string | null
  totalTraderClaimsAssets: string | null
  terminalLpPriceDeltaAssets: string | null
  markPrice: string | null
  markTime: number | null
  bookVersion: number | null
  hasOpenPositions: boolean | null
  degradedMode: boolean | null
  seniorImpaired: boolean | null
  canIncreaseRisk: boolean | null
  canAcceptOrdinaryDeposits: boolean | null
  canSettleDepositEntries: boolean | null
  seniorReservationsWithinLimits: boolean | null
}

export interface LiquidityWaterfallSnapshot {
  seniorPrincipalAssets: string | null
  juniorPrincipalAssets: string | null
  seniorHighWaterMarkAssets: string | null
  seniorHwmCoverageBps: number | null
  seniorImpairmentAssets: string | null
  protectedSeniorExposureAssets: string | null
  maxSeniorWithdrawAssets: string | null
  maxJuniorWithdrawAssets: string | null
  seniorDepositCapacityAssets: string | null
  reservedSeniorDepositAssets: string | null
  unassignedAssets: string | null
  pendingRecapitalizationAssets: string | null
  pendingTradingRevenueAssets: string | null
  positiveVpiAssets: string | null
  negativeVpiAssets: string | null
  netVpiAccruedAssets: string | null
  carryIndex: string | null
  carryTimestamp: number | null
}

export interface LiquidityPolicySnapshot {
  seniorRateBps: number | null
  seniorFrozenLpFeeBps: number | null
  juniorFrozenLpFeeBps: number | null
  poolConfigActivationTime: number | null
  pendingPoolConfigTimelockActive: boolean
  secondsUntilPoolConfigActivation: number | null
}

export interface TrancheVaultEscrowSnapshot {
  pendingDepositAssets: string | null
  pendingRedeemShares: string | null
  depositClaimShares: string | null
  withdrawalEscrowAssets: string | null
}

export interface SettlementCustodyAudit {
  healthState: 'healthy' | 'degraded' | 'faulted' | 'unknown'
  healthStateCode: number
  criticalFaults: DecodedMask
  dependencyFailures: DecodedMask
  custodyBalanced: boolean | null
  seniorEscrowBalanced: boolean | null
  juniorEscrowBalanced: boolean | null
  navBookCapMatchesEngine: boolean | null
  pool: {
    rawAssets: string
    accountedAssets: string
    custodyDeficitAssets: string
    custodySurplusAssets: string
  }
  escrowAudit: {
    senior: {
      requiredAssetEscrow: string
      actualAssetEscrow: string
      requiredShareEscrow: string
      actualShareEscrow: string
    }
    junior: {
      requiredAssetEscrow: string
      actualAssetEscrow: string
      requiredShareEscrow: string
      actualShareEscrow: string
    }
  }
  navBookParity: {
    bookCapPrice: string
    engineCapPrice: string
    bookActiveCurveCount: number
    bookTotalLots: string
    bookTotalEntryCostAssetAtoms: string
    bookTotalEffectiveCapAssetAtoms: string
    engineLongOpenInterest: string
    engineShortOpenInterest: string
    engineLongEntryNotional: string
    engineShortEntryNotional: string
    sizeQuantum: string
  }
}

export interface LiquiditySolvencyResponse {
  network: LiquidityNetworkMeta
  observedAt: string
  observedBlock: string
  solvency: LiquiditySolvencySnapshot
  waterfall: LiquidityWaterfallSnapshot
  policy: LiquidityPolicySnapshot
  vaultEscrows: {
    senior: TrancheVaultEscrowSnapshot
    junior: TrancheVaultEscrowSnapshot
  }
  custodyAudit: SettlementCustodyAudit | null
}

export function getLiquiditySolvency(signal?: AbortSignal): Promise<LiquiditySolvencyResponse> {
  return fetchInsightsJson<LiquiditySolvencyResponse>('/liquidity/solvency', signal)
}

export const liquidityKeys = {
  settlements: ['insights', 'liquidity', 'settlements'] as const,
  forensics: ['insights', 'liquidity', 'forensics'] as const,
  efficiency: ['insights', 'liquidity', 'efficiency'] as const,
  solvency: ['insights', 'liquidity', 'solvency'] as const,
  epoch: (epoch: string | number) => ['insights', 'liquidity', 'epoch', String(epoch)] as const,
}

export function useLiquiditySolvency() {
  const queryClient = useQueryClient()
  const staticPreview = queryClient.getDefaultOptions().queries?.refetchInterval === false
  const hasSeededData = queryClient.getQueryData(liquidityKeys.solvency) !== undefined
  return useQuery({
    queryKey: liquidityKeys.solvency,
    queryFn: ({ signal }) => getLiquiditySolvency(signal),
    enabled: !staticPreview || hasSeededData,
    staleTime: staticPreview ? Infinity : 15_000,
    refetchInterval: staticPreview ? false : 15_000,
    refetchOnMount: !staticPreview,
    refetchOnWindowFocus: !staticPreview,
    refetchIntervalInBackground: false,
    retry: shouldRetryEndpoint,
  })
}

export function useLiquidityEfficiency() {
  const queryClient = useQueryClient()
  const staticPreview = queryClient.getDefaultOptions().queries?.refetchInterval === false
  return useQuery({
    queryKey: liquidityKeys.efficiency,
    queryFn: ({ signal }) => getLiquidityEfficiency(signal),
    staleTime: staticPreview ? Infinity : 30_000,
    refetchInterval: staticPreview ? false : 30_000,
    refetchOnMount: !staticPreview,
    refetchOnWindowFocus: !staticPreview,
    refetchIntervalInBackground: false,
    retry: shouldRetryEndpoint,
  })
}

export function useLiquiditySettlements() {
  const queryClient = useQueryClient()
  const staticPreview = queryClient.getDefaultOptions().queries?.refetchInterval === false
  return useQuery({
    queryKey: liquidityKeys.settlements,
    queryFn: ({ signal }) => getLiquiditySettlements(signal),
    staleTime: staticPreview ? Infinity : 30_000,
    refetchInterval: staticPreview ? false : 30_000,
    refetchOnMount: !staticPreview,
    refetchOnWindowFocus: !staticPreview,
    refetchIntervalInBackground: false,
    retry: shouldRetryEndpoint,
  })
}

export function useLiquidityForensics() {
  const queryClient = useQueryClient()
  const staticPreview = queryClient.getDefaultOptions().queries?.refetchInterval === false
  return useQuery({
    queryKey: liquidityKeys.forensics,
    queryFn: ({ signal }) => getLiquidityForensics(signal),
    staleTime: staticPreview ? Infinity : 30_000,
    refetchInterval: staticPreview ? false : 30_000,
    refetchOnMount: !staticPreview,
    refetchOnWindowFocus: !staticPreview,
    refetchIntervalInBackground: false,
    retry: shouldRetryEndpoint,
  })
}

export function useLiquidityEpoch(epoch: string | undefined) {
  const queryClient = useQueryClient()
  const staticPreview = queryClient.getDefaultOptions().queries?.refetchInterval === false
  const valid = epoch != null && /^\d+$/.test(epoch)

  return useQuery({
    queryKey: liquidityKeys.epoch(epoch ?? ''),
    queryFn: ({ signal }) => fetchLiquidityEpochWithFallback(epoch ?? '', signal),
    enabled: valid,
    staleTime: staticPreview ? Infinity : 30_000,
    refetchInterval: staticPreview ? false : 30_000,
    refetchIntervalInBackground: false,
    retry: shouldRetryEndpoint,
  })
}
