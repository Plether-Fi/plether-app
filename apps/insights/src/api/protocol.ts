import { useQuery, useQueryClient } from '@tanstack/react-query'
import type { StatusTone } from '../components/StatusPill'
import { formatAssets, formatCompactAssets } from '../utils/format'
import { InsightsApiError } from './client'
import type { ApiErrorBody } from './types'

const API_ROOT = '/api/insights/v1'
const COMPACT_ASSET_THRESHOLD = 100_000_000_000n // 100,000 USDG in 1e6 units
const WAD_SCALE = 1_000_000_000_000_000_000n

export interface ProtocolNetworkMeta {
  chainId: number
  releaseVersion: string
  settlementAssetSymbol: string
}

export interface ProtocolReleaseMetadata {
  version: string | null
  date: string | null
  phase: string | null
  deploymentBlock: string | null
  sourceCommit: string | null
  forgeVersion: string | null
  manifestSha256: string | null
  bundleSha256: string | null
  pythAddress: string | null
  schemaVersion: number | null
  orderInterfaceVersion: number | null
  aaManifestVersion: string | null
}

export interface ProtocolSettlementToken {
  name: string | null
  symbol: string | null
  decimals: number | null
  isMock: boolean | null
  address: string
}

export interface ProtocolSafetyStatus {
  available: boolean
  phase: string | null
  lastMarkPrice: string | null
  lastMarkTime: string | null
  tradingActive: boolean | null
  withdrawalLive: boolean | null
  lpEpochSettlementPaused: boolean | null
  oracleFrozen: boolean | null
  fadWindow: boolean | null
  housePoolPaused: boolean | null
  degradedMode: boolean | null
}

export interface ProtocolVerificationSummary {
  totalContracts: number
  verifiedCount: number
  mismatchCount: number
  uncheckedCount: number
  matchedParameters: number
  driftedParameters: number
  unavailableParameters: number
}

export type ProtocolParameterCategory = 'trader' | 'liquidation' | 'lp' | 'oracle'
export type ProtocolParameterUnit = 'bps' | 'wad' | 'assets' | 'seconds' | 'count'
export type ProtocolParameterStatus = 'match' | 'drifted' | 'live_only' | 'unavailable'

export interface ProtocolParameterRecord {
  key: string
  category: ProtocolParameterCategory
  label: string
  unit: ProtocolParameterUnit
  contract: string
  description: string
  manifestDefault: string | null
  liveValue: string | null
  status: ProtocolParameterStatus
  minBound?: string | null
  maxBound?: string | null
}

export interface ParameterRangeVisual {
  minRaw: string
  maxRaw: string
  minLabel: string
  maxLabel: string
  livePercent: number | null
  defaultPercent: number | null
}

export type ProtocolContractCategory = 'core' | 'orders' | 'liquidity' | 'oracle' | 'safety'

export interface ProtocolContractRecord {
  key: string
  name: string
  category: ProtocolContractCategory
  role: string
  address: string
  expectedCodeHash: string
  actualCodeHash: string | null
  verified: boolean | null
  sourcePath?: string | null
  usdgBalanceAssets?: string | null
}

export interface ProtocolTvlBucket {
  key: string
  label: string
  detail: string
  assets: string
  shareBps: number | null
  addressCount: number
}

export interface ProtocolTvlContractHolding {
  key: string
  name: string
  category: ProtocolContractCategory
  address: string
  assets: string
  shareBps: number | null
}

export interface ProtocolTvlKernelAccount {
  address: string
  assets: string
  shareBps: number | null
}

export interface ProtocolTvlSummary {
  available: boolean
  isComplete: boolean
  verifiedAtBlock: string
  settlementAssetSymbol: string
  totalTvlAssets?: string
  protocolContractsTvlAssets?: string
  lpVaultsTvlAssets?: string
  housePoolAssets?: string
  seniorVaultAssets?: string
  juniorVaultAssets?: string
  marginClearinghouseAssets?: string
  otherContractsAssets?: string
  kernelTradingAccountsAssets?: string
  totalTraderMarginAssets?: string
  contractsTracked?: number
  contractsWithBalance?: number
  contractsFailedReads?: number
  kernelAccountsAvailable?: boolean
  kernelAccountsTracked?: number
  kernelAccountsFunded?: number
  kernelAccountsFailedReads?: number
  buckets?: ProtocolTvlBucket[]
  contractHoldings?: ProtocolTvlContractHolding[]
  topKernelAccounts?: ProtocolTvlKernelAccount[]
}

export const PROTOCOL_CORE_GITHUB_REPO = 'Plether-Fi/plether-core'
export const PROTOCOL_CORE_GITHUB_URL = `https://github.com/${PROTOCOL_CORE_GITHUB_REPO}`
const PROTOCOL_CORE_DEFAULT_BRANCH = 'master'
const GIT_COMMIT_SHA_RE = /^[0-9a-f]{7,40}$/i

/**
 * Verified Solidity source paths in Plether-Fi/plether-core for each pinned
 * perps protocol contract key.
 */
export const PROTOCOL_CONTRACT_SOURCE_PATHS: Readonly<Record<string, string>> = {
  cfdEngine: 'packages/perps/src/CfdEngine.sol',
  marginClearinghouse: 'packages/perps/src/MarginClearinghouse.sol',
  terminalNavBookV2: 'packages/perps/src/TerminalNavBookV2.sol',
  cfdEnginePlanner: 'packages/perps/src/CfdEnginePlanner.sol',
  cfdEnginePlanningReader: 'packages/perps/src/CfdEnginePlanningReader.sol',
  cfdEngineSettlementSidecar: 'packages/perps/src/CfdEngineSettlementSidecar.sol',
  cfdEngineOpenQuoter: 'packages/perps/src/CfdEngineOpenQuoter.sol',
  cfdClosePreview: 'packages/perps/src/CfdClosePreview.sol',
  orderRouter: 'packages/perps/src/OrderRouter.sol',
  orderLifecycleBook: 'packages/perps/src/OrderLifecycleBook.sol',
  cfdOrderPolicyEvaluator: 'packages/perps/src/CfdOrderPolicyEvaluator.sol',
  positionProtectionBook: 'packages/perps/src/PositionProtectionBook.sol',
  orderRouterExecutionSidecar: 'packages/perps/src/OrderRouterExecutionSidecar.sol',
  orderRecoverySidecar: 'packages/perps/src/OrderRecoverySidecar.sol',
  orderRouterLiquidationBatchSidecar: 'packages/perps/src/OrderRouterLiquidationBatchSidecar.sol',
  housePool: 'packages/perps/src/HousePool.sol',
  seniorVault: 'packages/perps/src/TrancheVault.sol',
  juniorVault: 'packages/perps/src/TrancheVault.sol',
  housePoolRedemptionMathSidecar: 'packages/perps/src/HousePoolRedemptionMathSidecar.sol',
  pletherOracle: 'packages/perps/src/PletherOracle.sol',
  perpsPublicLens: 'packages/perps/src/PerpsPublicLens.sol',
  cfdEngineLens: 'packages/perps/src/CfdEngineLens.sol',
  cfdEngineAccountLens: 'packages/perps/src/CfdEngineAccountLens.sol',
  cfdEngineProtocolLens: 'packages/perps/src/CfdEngineProtocolLens.sol',
  settlementMonitorLens: 'packages/perps/src/SettlementMonitorLens.sol',
  settlementMonitorLensSidecar: 'packages/perps/src/SettlementMonitorLensSidecar.sol',
  emergencyPauseCoordinator: 'packages/perps/src/EmergencyPauseCoordinator.sol',
  cfdEngineAdmin: 'packages/perps/src/CfdEngineAdmin.sol',
  orderRouterAdmin: 'packages/perps/src/OrderRouterAdmin.sol',
}

const MOCK_SETTLEMENT_TOKEN_SOURCE_PATH = 'script/DeployPerpsArbitrumSepolia.s.sol'

function normalizeGitCommitRef(sourceCommit: string | null | undefined): string | null {
  if (!sourceCommit) return null
  const trimmed = sourceCommit.trim()
  return GIT_COMMIT_SHA_RE.test(trimmed) ? trimmed : null
}

export function protocolCommitGitHubUrl(sourceCommit: string | null | undefined): string | null {
  const commit = normalizeGitCommitRef(sourceCommit)
  return commit ? `${PROTOCOL_CORE_GITHUB_URL}/commit/${commit}` : null
}

export function protocolTreeGitHubUrl(sourceCommit: string | null | undefined): string {
  const commit = normalizeGitCommitRef(sourceCommit)
  return commit
    ? `${PROTOCOL_CORE_GITHUB_URL}/tree/${commit}/packages/perps/src`
    : `${PROTOCOL_CORE_GITHUB_URL}/tree/${PROTOCOL_CORE_DEFAULT_BRANCH}/packages/perps/src`
}

export function protocolContractGitHubUrl(
  contract: Pick<ProtocolContractRecord, 'key' | 'sourcePath'>,
  sourceCommit: string | null | undefined,
  options?: { settlementTokenIsMock?: boolean | null },
): { url: string; path: string; ref: string } | null {
  const explicitPath = contract.sourcePath?.trim().replace(/^\/+/, '') ?? ''
  const path =
    explicitPath !== ''
      ? explicitPath
      : contract.key === 'settlementToken'
        ? options?.settlementTokenIsMock === true
          ? MOCK_SETTLEMENT_TOKEN_SOURCE_PATH
          : null
        : (PROTOCOL_CONTRACT_SOURCE_PATHS[contract.key] ?? null)

  if (!path) return null

  const ref = normalizeGitCommitRef(sourceCommit) ?? PROTOCOL_CORE_DEFAULT_BRANCH
  return {
    url: `${PROTOCOL_CORE_GITHUB_URL}/blob/${ref}/${path}`,
    path,
    ref,
  }
}

export interface ProtocolReleaseHistoryItem {
  version: string
  status: string
  date: string
  deploymentBlock?: string | null
  summary: string
}

export interface ProtocolManifestResponse {
  network: ProtocolNetworkMeta
  observedAt: string
  rpcAvailable: boolean
  verifiedAtBlock: string
  release: ProtocolReleaseMetadata
  settlementToken: ProtocolSettlementToken
  safety: ProtocolSafetyStatus
  tvl?: ProtocolTvlSummary
  verification: ProtocolVerificationSummary
  parameters: ProtocolParameterRecord[]
  contracts: ProtocolContractRecord[]
  releaseHistory: ProtocolReleaseHistoryItem[]
}

interface Envelope<T> {
  data: T
}

async function fetchProtocolJson<T>(path: string, signal?: AbortSignal): Promise<T> {
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

export function getProtocolManifest(signal?: AbortSignal): Promise<ProtocolManifestResponse> {
  return fetchProtocolJson<ProtocolManifestResponse>('/protocol/manifest', signal)
}

function shouldRetryProtocol(failureCount: number, error: Error): boolean {
  if (error instanceof InsightsApiError && error.status === 404) return false
  return failureCount < 2
}

export function useProtocolManifest() {
  const queryClient = useQueryClient()
  const staticPreview = queryClient.getDefaultOptions().queries?.refetchInterval === false
  return useQuery({
    queryKey: ['insights', 'protocol', 'manifest'] as const,
    queryFn: ({ signal }) => getProtocolManifest(signal),
    enabled: !staticPreview,
    staleTime: staticPreview ? Infinity : 60_000,
    refetchInterval: staticPreview ? false : 60_000,
    refetchIntervalInBackground: false,
    retry: shouldRetryProtocol,
  })
}

export function formatProtocolParamValue(
  raw: string | null | undefined,
  unit: ProtocolParameterUnit,
): string {
  if (raw == null) return 'Unavailable'
  const trimmed = raw.trim()
  if (trimmed === '' || !/^\d+$/.test(trimmed)) return 'Unavailable'

  switch (unit) {
    case 'bps': {
      const bps = Number(trimmed)
      if (!Number.isSafeInteger(bps)) return 'Unavailable'
      const pct = (bps / 100).toFixed(2)
      const formattedBps = bps.toLocaleString('en-US')
      return `${pct}% (${formattedBps} bps)`
    }
    case 'wad': {
      const wad = BigInt(trimmed)
      const hundredthsPct = (wad * 10_000n + WAD_SCALE / 2n) / WAD_SCALE
      const whole = hundredthsPct / 100n
      const frac = hundredthsPct % 100n
      return `${whole.toString()}.${frac.toString().padStart(2, '0')}%`
    }
    case 'assets': {
      const units = BigInt(trimmed)
      return units >= COMPACT_ASSET_THRESHOLD
        ? formatCompactAssets(trimmed)
        : formatAssets(trimmed)
    }
    case 'seconds': {
      const total = Number(trimmed)
      if (!Number.isSafeInteger(total)) return 'Unavailable'
      if (total < 60) return `${String(total)}s`
      if (total % 86_400 === 0) {
        const days = total / 86_400
        const hours = total / 3_600
        return `${String(days)}d (${String(hours)}h)`
      }
      if (total % 3_600 === 0) {
        return `${String(total / 3_600)}h`
      }
      if (total % 60 === 0) {
        return `${String(total / 60)}m`
      }
      const mins = Math.floor(total / 60)
      const secs = total % 60
      return `${String(mins)}m ${String(secs)}s`
    }
    case 'count': {
      const count = Number(trimmed)
      if (!Number.isSafeInteger(count)) return 'Unavailable'
      return count.toLocaleString('en-US')
    }
  }
}

export function parameterImpactHint(param: ProtocolParameterRecord): string | null {
  const value = param.liveValue ?? param.manifestDefault
  if (!value || !/^\d+$/.test(value)) return null
  const num = Number(value)
  if (!Number.isSafeInteger(num) || num <= 0) return null

  if (param.key === 'initialMarginBps') {
    const maxLev = Math.floor(10_000 / num)
    return maxLev > 0 ? `Up to ${maxLev.toLocaleString('en-US')}× nominal leverage` : null
  }
  if (param.key === 'fadMarginBps') {
    const maxLev = Math.floor(10_000 / num)
    return maxLev > 0 ? `Caps weekend leverage at ~${maxLev.toLocaleString('en-US')}×` : null
  }
  return null
}

const DEFAULT_PARAM_BOUNDS: Partial<Record<string, { min: string; max: string }>> = {
  initialMarginBps: { min: '0', max: '100' },
  maintenanceMarginBps: { min: '0', max: '50' },
  fadMarginBps: { min: '0', max: '1000' },
  executionFeeBps: { min: '0', max: '25' },
  vpiFactorWad: { min: '0', max: '50000000000000000' },
  maxSkewRatioWad: { min: '0', max: '1000000000000000000' },
  baseCarryBps: { min: '0', max: '2000' },
  frozenCloseSpreadBps: { min: '0', max: '200' },
  minimumOpenNotionalAssets: { min: '0', max: '5000000000' },
  maxPendingOrders: { min: '0', max: '20' },
  liquidationChargeBps: { min: '0', max: '100' },
  minimumLiquidationChargeAssets: { min: '0', max: '10000000' },
  keeperShareBps: { min: '0', max: '10000' },
  protocolShareBps: { min: '0', max: '10000' },
  positionProtectionTriggerBountyAssets: { min: '0', max: '1000000' },
  maxSeniorExposureAssets: { min: '0', max: '100000000000000' },
  maxSeniorShareBps: { min: '0', max: '10000' },
  settlementBufferBps: { min: '0', max: '200' },
  lpEpochDurationSeconds: { min: '0', max: '14400' },
  timelockDelaySeconds: { min: '0', max: '259200' },
  basketMaxConfidenceRatioBps: { min: '0', max: '50' },
  adverseConfidenceMultiplierBps: { min: '0', max: '10000' },
  orderExecutionStalenessLimitSeconds: { min: '0', max: '60' },
  liquidationStalenessLimitSeconds: { min: '0', max: '120' },
  maxExecutionWindowSeconds: { min: '0', max: '120' },
}

export function formatCompactParamBound(
  raw: string,
  unit: ProtocolParameterUnit,
): string {
  if (!/^\d+$/.test(raw)) return '—'
  switch (unit) {
    case 'bps': {
      const bps = Number(raw)
      if (!Number.isSafeInteger(bps)) return '—'
      const pct = bps / 100
      return Number.isInteger(pct) ? `${String(pct)}%` : `${pct.toFixed(2)}%`
    }
    case 'wad':
      return formatProtocolParamValue(raw, 'wad')
    case 'assets': {
      const units = BigInt(raw)
      if (units === 0n) return '0 USDG'
      return units >= 1_000_000_000n ? formatCompactAssets(raw) : formatAssets(raw)
    }
    case 'seconds':
      return formatProtocolParamValue(raw, 'seconds')
    case 'count':
      return Number(raw).toLocaleString('en-US')
  }
}

function parseNonNegBigInt(raw: string | null | undefined): bigint | null {
  if (raw == null) return null
  const trimmed = raw.trim()
  if (!/^\d+$/.test(trimmed)) return null
  return BigInt(trimmed)
}

function ratioPercent(value: bigint, minVal: bigint, maxVal: bigint): number {
  if (maxVal <= minVal) return 0
  const clamped = value < minVal ? minVal : value > maxVal ? maxVal : value
  const scaled = ((clamped - minVal) * 1000n) / (maxVal - minVal)
  return Number(scaled) / 10
}

export function resolveParameterRange(
  param: ProtocolParameterRecord,
): ParameterRangeVisual {
  const configured = DEFAULT_PARAM_BOUNDS[param.key]
  const liveBig = parseNonNegBigInt(param.liveValue)
  const defaultBig = parseNonNegBigInt(param.manifestDefault)

  let minBig = parseNonNegBigInt(param.minBound) ?? (configured ? BigInt(configured.min) : 0n)
  let maxBig =
    parseNonNegBigInt(param.maxBound)
    ?? (configured
      ? BigInt(configured.max)
      : (() => {
          const ref = liveBig ?? defaultBig ?? 100n
          return ref > 0n ? ref * 2n : 100n
        })())

  if (liveBig != null && liveBig < minBig) minBig = liveBig
  if (defaultBig != null && defaultBig < minBig) minBig = defaultBig
  if (liveBig != null && liveBig > maxBig) maxBig = liveBig
  if (defaultBig != null && defaultBig > maxBig) maxBig = defaultBig
  if (maxBig <= minBig) maxBig = minBig + 1n

  const minRaw = minBig.toString()
  const maxRaw = maxBig.toString()

  return {
    minRaw,
    maxRaw,
    minLabel: formatCompactParamBound(minRaw, param.unit),
    maxLabel: formatCompactParamBound(maxRaw, param.unit),
    livePercent: liveBig != null ? ratioPercent(liveBig, minBig, maxBig) : null,
    defaultPercent: defaultBig != null ? ratioPercent(defaultBig, minBig, maxBig) : null,
  }
}

export function shortHash(hash: string | null | undefined): string {
  if (!hash || hash === '0x') return hash === '0x' ? 'Empty (0x)' : 'Unavailable'
  const trimmed = hash.trim()
  if (trimmed.length <= 16) return trimmed
  return `${trimmed.slice(0, 8)}…${trimmed.slice(-6)}`
}

export function deriveProtocolHealthBanner(data: ProtocolManifestResponse): {
  tone: StatusTone
  label: string
  note: string
} {
  const { verification, safety, network, rpcAvailable } = data
  const pauseActive =
    safety.oracleFrozen === true
    || safety.housePoolPaused === true
    || safety.lpEpochSettlementPaused === true
    || safety.tradingActive === false
    || safety.phase === 'paused'

  if (!rpcAvailable) {
    return {
      tone: 'stale',
      label: 'RPC verification unavailable',
      note: 'Live on-chain bytecode and parameter reads could not reach RPC — showing manifest baseline only',
    }
  }

  if (verification.mismatchCount > 0) {
    return {
      tone: 'blocked',
      label: 'Bytecode mismatch detected',
      note: `${String(verification.mismatchCount)} of ${String(verification.totalContracts)} pinned contracts differ on-chain`,
    }
  }

  if (pauseActive) {
    return {
      tone: 'blocked',
      label: 'Safety pause active',
      note: 'One or more emergency pause or oracle freeze controls are engaged on-chain',
    }
  }

  if (safety.degradedMode === true) {
    return {
      tone: 'degraded',
      label: 'Degraded mode active',
      note: 'CfdEngine degraded mode is engaged on-chain',
    }
  }

  if (verification.driftedParameters > 0) {
    return {
      tone: 'degraded',
      label: 'Parameter drift detected',
      note: `${String(verification.driftedParameters)} live parameter${verification.driftedParameters === 1 ? '' : 's'} differ from Core ${network.releaseVersion} defaults`,
    }
  }

  if (verification.uncheckedCount > 0 || verification.unavailableParameters > 0) {
    return {
      tone: 'stale',
      label: 'Partially verified',
      note: `${String(verification.verifiedCount)}/${String(verification.totalContracts)} contracts verified · ${String(verification.unavailableParameters)} parameter reads unavailable`,
    }
  }

  return {
    tone: 'live',
    label: `Verified · ${String(verification.verifiedCount)}/${String(verification.totalContracts)} bytecode match`,
    note: `${String(verification.matchedParameters)} parameters aligned with Core ${network.releaseVersion}`,
  }
}

export function deriveProtocolStateSummary(safety: ProtocolSafetyStatus): {
  label: string
  tone: 'positive' | 'negative' | 'warning' | 'neutral'
  detail: string
} {
  if (!safety.available) {
    return {
      label: 'Unavailable',
      tone: 'neutral',
      detail: 'PerpsPublicLens status unreachable',
    }
  }
  if (safety.oracleFrozen) {
    return {
      label: 'Oracle frozen',
      tone: 'negative',
      detail: 'Pyth DXY basket feed frozen on-chain',
    }
  }
  if (safety.phase === 'paused' || safety.tradingActive === false || safety.housePoolPaused) {
    return {
      label: 'Paused',
      tone: 'negative',
      detail: 'Emergency pause control engaged',
    }
  }
  if (safety.degradedMode) {
    return {
      label: 'Degraded',
      tone: 'warning',
      detail: 'CfdEngine running in degraded mode',
    }
  }
  if (safety.lpEpochSettlementPaused) {
    return {
      label: 'LP paused',
      tone: 'warning',
      detail: 'LP epoch settlement paused; trading active',
    }
  }
  if (safety.fadWindow) {
    return {
      label: 'FAD window',
      tone: 'warning',
      detail: 'Weekend FX closure margin active',
    }
  }
  const phaseTitle = safety.phase
    ? safety.phase.charAt(0).toUpperCase() + safety.phase.slice(1)
    : 'Active'
  return {
    label: phaseTitle,
    tone: safety.phase === 'active' ? 'positive' : 'neutral',
    detail: 'Trading, withdrawals & LP epochs live',
  }
}
