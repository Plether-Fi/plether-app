import type {
  ApiErrorBody,
  Competition,
  InsightsStatus,
  CurrentReleaseResponse,
  HousePoolResponse,
  KeeperResponse,
  KeepersResponse,
  LeaderboardResponse,
  ParameterChangesResponse,
  ParametersResponse,
  ProtocolEnvelope,
  ProtocolOrderResponse,
  ProtocolOverviewResponse,
  ProtocolTransactionResponse,
  ProtocolTransactionsResponse,
  ProtocolWalletActivity,
  ProtocolWalletResponse,
  ProtocolWalletSummary,
  ProtocolWalletsResponse,
  TrancheHistoryResponse,
  TrancheResponse,
  WalletActivity,
  WalletResponse,
} from './types'

export const DEFAULT_COMPETITION_SLUG = 'testnet-trading-2026'
const API_ROOT = '/api/insights/v1'
const PERPS_PRICE_DECIMALS = 8
const PLDXY_PRICE_CAP = 2n * 10n ** BigInt(PERPS_PRICE_DECIMALS)

interface ApiEnvelope<T> {
  data: T
  meta?: { blockNumber?: number; chainId?: number }
}

interface WirePrize {
  place: number
  amount?: string
  amountUsdc?: string
}

interface WireCompetition {
  id?: string
  slug: string
  name: string
  chainId?: string | number
  status?: Competition['status']
  phase?: string
  startsAt?: string
  startAt?: string
  tradingCutoffAt?: string
  scoreCutoffAt?: string
  resultsAt: string
  startingBalance?: string
  startingBalanceUsdc?: string
  pnlEligibilityThreshold?: string
  minimumProfitUsdc?: string
  minActiveDays?: number
  minimumActiveDays?: number
  prizes: WirePrize[]
  latestIndexedBlock?: number | null
  latestIndexedAt?: string | null
  participantCount?: number
  eligibleCount?: number
}

interface WireStanding {
  rank?: number | null
  address?: string
  wallet?: string
  displayName?: string | null
  alias?: string | null
  pnl?: string | null
  finalPnlUsdc?: string | null
  realizedPnl?: string | null
  realizedPnlUsdc?: string | null
  roiBps?: number | null
  volume?: string
  volumeUsdc?: string
  trades?: number
  executedTrades?: number
  activeDays: number
  liquidations: number
  prizePlace?: number | null
  prizePlaces?: number[]
  prizeAmountUsdc?: string | null
  eligible?: boolean
  prizeEligible?: boolean
  eligibilityReasons?: string[]
  eligibilityStatus?: string
  eligibilityReason?: string
  meetsProfitRequirement?: boolean
  meetsActiveDaysRequirement?: boolean
  scoreAvailable?: boolean
  currentAccountValueUsdc?: string
  equity?: string
  position?: WirePosition | null
}

interface WirePosition {
  market?: string
  side?: 'long' | 'short' | number | null
  sideCode?: string | number | null
  size?: string | null
  sizeDelta?: string | null
  margin?: string | null
  marginUsdc?: string | null
  entryPrice?: string | null
  markPrice?: string | null
  unrealizedPnl?: string | null
  unrealizedPnlUsdc?: string | null
  liquidatable?: boolean | null
}

interface WireActivity {
  id?: string
  type?: string
  activityType?: string
  occurredAt: string
  market?: string | null
  side?: 'long' | 'short' | number | null
  size?: string | null
  sizeDelta?: string | null
  amountUsdc?: string | null
  price?: string | null
  pnl?: string | null
  pnlUsdc?: string | null
  executionFee?: string | null
  executionFeeUsdc?: string | null
  protocolFee?: string | null
  protocolFeeUsdc?: string | null
  vpi?: string | null
  vpiUsdc?: string | null
  vpiDeltaUsdc?: string | null
  txHash?: string | null
  blockNumber?: string
  logIndex?: number
}

interface WireLeaderboardResponse {
  competition: WireCompetition
  standings: WireStanding[]
  nextCursor: string | null
  provisional: boolean
}

interface WireWalletResponse {
  competition: WireCompetition
  wallet: WireStanding
  activity?: WireActivity[] | null
}

interface WireDataStatus {
  participantCount?: number
  eligibleCount?: number
  indexedThroughBlock?: string
  indexerUpdatedAt?: string
  latestIndexedBlock?: number | null
  latestIndexedAt?: string | null
  healthy?: boolean
}

interface WireStatusResponse {
  competition?: WireCompetition
  status?: WireDataStatus
  healthy?: boolean
  latestIndexedBlock?: number | null
  latestIndexedAt?: string | null
  chainId?: number
}

export class InsightsApiError extends Error {
  readonly status: number
  readonly code: string | undefined

  constructor(message: string, status: number, code?: string) {
    super(message)
    this.name = 'InsightsApiError'
    this.status = status
    this.code = code
  }
}

async function request<T>(path: string, signal?: AbortSignal): Promise<T> {
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
    const message = body?.error?.message ?? body?.message ?? `Request failed (${String(response.status)})`
    throw new InsightsApiError(message, response.status, body?.error?.code)
  }

  const body = (await response.json()) as T | ApiEnvelope<T>
  if (typeof body === 'object' && body !== null && 'data' in body) {
    return body.data
  }
  return body
}

export async function getCurrentCompetition(signal?: AbortSignal): Promise<Competition> {
  const response = await request<WireCompetition | { competition: WireCompetition }>('/competitions/current', signal)
  const rawCompetition = 'competition' in response ? response.competition : response
  return normalizeCompetition(rawCompetition)
}

export interface LeaderboardParams {
  limit?: number
  cursor?: string
  search?: string
  signal?: AbortSignal
}

export function getLeaderboard(
  slug: string,
  { limit = 50, cursor, search, signal }: LeaderboardParams = {},
): Promise<LeaderboardResponse> {
  const query = new URLSearchParams({ limit: String(limit) })
  if (cursor) query.set('cursor', cursor)
  if (search) query.set('search', search)
  return request<WireLeaderboardResponse>(
    `/competitions/${encodeURIComponent(slug)}/leaderboard?${query.toString()}`,
    signal,
  ).then((response) => ({
    competition: normalizeCompetition(response.competition),
    standings: response.standings.map((standing) => normalizeStanding(standing, response.competition)),
    nextCursor: response.nextCursor,
    provisional: response.provisional,
  }))
}

export async function getWallet(slug: string, address: string, signal?: AbortSignal): Promise<WalletResponse> {
  const response = await request<WireWalletResponse>(
    `/competitions/${encodeURIComponent(slug)}/wallets/${encodeURIComponent(address)}`,
    signal,
  )
  const standing = normalizeStanding(response.wallet, response.competition)
  const activity = response.activity?.map(normalizeActivity) ?? null
  return {
    competition: normalizeCompetition(response.competition),
    wallet: {
      ...standing,
      realizedPnl:
        response.wallet.realizedPnl
        ?? response.wallet.realizedPnlUsdc
        ?? sumRealizedPnl(activity),
      equity: response.wallet.equity ?? response.wallet.currentAccountValueUsdc ?? null,
      position: normalizePosition(response.wallet.position),
    },
    activity,
  }
}

export async function getStatus(signal?: AbortSignal): Promise<InsightsStatus> {
  const response = await request<WireStatusResponse>('/status', signal)
  return normalizeStatus(response)
}

export function getCurrentProtocolRelease(signal?: AbortSignal): Promise<CurrentReleaseResponse> {
  return request<CurrentReleaseResponse>('/protocol/releases/current', signal)
}

function protocolPath(releaseId: string, suffix: string): string {
  return `/protocol/releases/${encodeURIComponent(releaseId)}${suffix}`
}

export function getProtocolOverview(releaseId: string, signal?: AbortSignal): Promise<ProtocolOverviewResponse> {
  return request<ProtocolOverviewResponse>(protocolPath(releaseId, '/overview'), signal)
}

export interface ProtocolTransactionsParams {
  actionType?: string
  outcome?: string
  address?: string
  account?: string
  keeper?: string
  contract?: string
  transactionHash?: string
  from?: string
  to?: string
  limit?: number
  cursor?: string
  signal?: AbortSignal
}

export interface CursorPaginationParams {
  limit?: number
  cursor?: string
  signal?: AbortSignal
}

export type TrancheHistoryParams = CursorPaginationParams

export interface KeeperDetailParams extends CursorPaginationParams {
  window?: string
}

export interface KeepersParams extends CursorPaginationParams {
  window?: string
}

export interface ProtocolWalletsParams extends CursorPaginationParams {
  window?: string
}

export interface ProtocolWalletDetailParams extends CursorPaginationParams {
  window?: string
}

export type ParameterChangesParams = CursorPaginationParams

export function getProtocolTransactions(
  releaseId: string,
  params: ProtocolTransactionsParams = {},
): Promise<ProtocolTransactionsResponse> {
  const query = new URLSearchParams()
  query.set('limit', String(params.limit ?? 50))
  for (const key of ['actionType', 'outcome', 'address', 'account', 'keeper', 'contract', 'transactionHash', 'from', 'to', 'cursor'] as const) {
    const value = params[key]
    if (value) query.set(key, value)
  }
  return request<ProtocolTransactionsResponse>(
    `${protocolPath(releaseId, '/transactions')}?${query.toString()}`,
    params.signal,
  )
}

export function getProtocolTransaction(
  releaseId: string,
  txHash: string,
  signal?: AbortSignal,
): Promise<ProtocolTransactionResponse> {
  return request<ProtocolTransactionResponse>(
    protocolPath(releaseId, `/transactions/${encodeURIComponent(txHash)}`),
    signal,
  )
}

export function getProtocolOrder(
  releaseId: string,
  orderId: string,
  signal?: AbortSignal,
): Promise<ProtocolOrderResponse> {
  return request<ProtocolOrderResponse>(
    protocolPath(releaseId, `/orders/${encodeURIComponent(orderId)}`),
    signal,
  )
}

export function getHousePool(releaseId: string, signal?: AbortSignal): Promise<HousePoolResponse> {
  return request<HousePoolResponse>(protocolPath(releaseId, '/house-pool'), signal)
}

export function getTranche(
  releaseId: string,
  tranche: string,
  signal?: AbortSignal,
): Promise<TrancheResponse> {
  return request<TrancheResponse>(
    protocolPath(releaseId, `/tranches/${encodeURIComponent(tranche)}`),
    signal,
  )
}

export function getTrancheHistory(
  releaseId: string,
  tranche: string,
  paramsOrSignal: TrancheHistoryParams | AbortSignal = {},
): Promise<TrancheHistoryResponse> {
  const params = normalizeCursorPaginationParams(paramsOrSignal)
  const query = cursorPaginationQuery(params, 500)
  return request<TrancheHistoryResponse>(
    `${protocolPath(releaseId, `/tranches/${encodeURIComponent(tranche)}/history`)}?${query.toString()}`,
    params.signal,
  )
}

export function getKeepers(
  releaseId: string,
  params?: KeepersParams,
): Promise<KeepersResponse>
export function getKeepers(
  releaseId: string,
  window: string,
  pagination?: CursorPaginationParams | AbortSignal,
): Promise<KeepersResponse>
export function getKeepers(
  releaseId: string,
  paramsOrWindow: KeepersParams | string = {},
  paginationOrSignal: CursorPaginationParams | AbortSignal = {},
): Promise<KeepersResponse> {
  const params = typeof paramsOrWindow === 'string'
    ? { ...normalizeCursorPaginationParams(paginationOrSignal), window: paramsOrWindow }
    : paramsOrWindow
  const query = new URLSearchParams({
    window: params.window ?? '7d',
    limit: String(params.limit ?? 100),
  })
  if (params.cursor) query.set('cursor', params.cursor)
  return request<KeepersResponse>(
    `${protocolPath(releaseId, '/keepers')}?${query.toString()}`,
    params.signal,
  )
}

export function getKeeper(
  releaseId: string,
  address: string,
  params?: KeeperDetailParams,
): Promise<KeeperResponse>
export function getKeeper(
  releaseId: string,
  address: string,
  window: string,
  pagination?: CursorPaginationParams | AbortSignal,
): Promise<KeeperResponse>
export function getKeeper(
  releaseId: string,
  address: string,
  paramsOrWindow: KeeperDetailParams | string = {},
  paginationOrSignal: CursorPaginationParams | AbortSignal = {},
): Promise<KeeperResponse> {
  const params = typeof paramsOrWindow === 'string'
    ? { ...normalizeCursorPaginationParams(paginationOrSignal), window: paramsOrWindow }
    : paramsOrWindow
  const query = new URLSearchParams({
    window: params.window ?? '7d',
    limit: String(params.limit ?? 100),
  })
  if (params.cursor) query.set('cursor', params.cursor)
  return request<KeeperResponse>(
    `${protocolPath(releaseId, `/keepers/${encodeURIComponent(address)}`)}?${query.toString()}`,
    params.signal,
  )
}

export function getProtocolWallets(
  releaseId: string,
  params: ProtocolWalletsParams = {},
): Promise<ProtocolWalletsResponse> {
  const query = new URLSearchParams({
    window: params.window ?? '7d',
    limit: String(params.limit ?? 100),
  })
  if (params.cursor) query.set('cursor', params.cursor)
  return request<unknown>(
    `${protocolPath(releaseId, '/wallets')}?${query.toString()}`,
    params.signal,
  ).then(normalizeProtocolWalletsResponse)
}

export function getProtocolWallet(
  releaseId: string,
  address: string,
  params: ProtocolWalletDetailParams = {},
): Promise<ProtocolWalletResponse> {
  const query = new URLSearchParams({
    window: params.window ?? '7d',
    limit: String(params.limit ?? 100),
  })
  if (params.cursor) query.set('cursor', params.cursor)
  return request<unknown>(
    `${protocolPath(releaseId, `/wallets/${encodeURIComponent(address)}`)}?${query.toString()}`,
    params.signal,
  ).then(normalizeProtocolWalletResponse)
}

export function getParameters(releaseId: string, signal?: AbortSignal): Promise<ParametersResponse> {
  return request<ParametersResponse>(protocolPath(releaseId, '/parameters'), signal)
}

export function getParameterChanges(
  releaseId: string,
  params?: ParameterChangesParams,
): Promise<ParameterChangesResponse>
export function getParameterChanges(
  releaseId: string,
  limit?: number,
  signal?: AbortSignal,
): Promise<ParameterChangesResponse>
export function getParameterChanges(
  releaseId: string,
  paramsOrLimit: ParameterChangesParams | number = {},
  legacySignal?: AbortSignal,
): Promise<ParameterChangesResponse> {
  const params = typeof paramsOrLimit === 'number'
    ? { limit: paramsOrLimit, signal: legacySignal }
    : { ...paramsOrLimit, signal: paramsOrLimit.signal ?? legacySignal }
  const query = cursorPaginationQuery(params, 200)
  return request<ParameterChangesResponse>(
    `${protocolPath(releaseId, '/parameter-changes')}?${query.toString()}`,
    params.signal,
  )
}

function normalizeCursorPaginationParams(
  paramsOrSignal: CursorPaginationParams | AbortSignal,
): CursorPaginationParams {
  return isAbortSignal(paramsOrSignal) ? { signal: paramsOrSignal } : paramsOrSignal
}

function isAbortSignal(value: CursorPaginationParams | AbortSignal): value is AbortSignal {
  return 'aborted' in value && 'addEventListener' in value
}

function cursorPaginationQuery(params: CursorPaginationParams, defaultLimit: number): URLSearchParams {
  const query = new URLSearchParams({ limit: String(params.limit ?? defaultLimit) })
  if (params.cursor) query.set('cursor', params.cursor)
  return query
}

function normalizeProtocolWalletsResponse(value: unknown): ProtocolWalletsResponse {
  const root = wireRecord(value)
  const payload = wireRecord(root.wallets ?? root.operationalWallets)
  const rawItems = wireArray(payload.items ?? payload.wallets)
  return {
    ...protocolEnvelope(root),
    wallets: {
      window: wireString(payload.window) ?? '7d',
      windowStart: wireTimestamp(payload.windowStart),
      windowEnd: wireTimestamp(payload.windowEnd),
      definition: normalizeOperationalWalletDefinition(payload.definition),
      items: rawItems.map(normalizeProtocolWalletSummary),
      nextCursor: wireString(payload.nextCursor),
      oracleUpdaterIdentityAvailable: wireBoolean(
        payload.oracleUpdaterIdentityAvailable
        ?? payload.oracleUpdaterPublished,
      ),
      oracleUpdaterActivityAttributable: wireBoolean(
        payload.oracleUpdaterActivityAttributable,
      ),
      totalTrackedWalletCount: wireString(
        payload.totalTrackedWalletCount
        ?? payload.totalWalletCount,
      ),
      totalAtRiskWalletCount: wireString(payload.totalAtRiskWalletCount),
      units: wireStringRecord(payload.units),
    },
  }
}

function normalizeProtocolWalletResponse(value: unknown): ProtocolWalletResponse {
  const root = wireRecord(value)
  const payload = wireRecord(root.wallet ?? root.operationalWallet)
  const activityPayload = wireRecord(payload.activity)
  const rawActivity = wireArray(
    Array.isArray(payload.activity)
      ? payload.activity
      : activityPayload.items ?? payload.actions ?? payload.transactions,
  )
  const activity = rawActivity.map(normalizeProtocolWalletActivity)
  const summary = normalizeProtocolWalletSummary(payload)
  return {
    ...protocolEnvelope(root),
    wallet: {
      ...summary,
      lastActivityTransactionHash:
        summary.lastActivityTransactionHash
        ?? activity.find((item) => item.transactionHash !== null)?.transactionHash
        ?? null,
      activity,
      nextCursor: wireString(payload.nextCursor ?? activityPayload.nextCursor),
    },
  }
}

function normalizeProtocolWalletSummary(value: unknown): ProtocolWalletSummary {
  const raw = wireRecord(value)
  const balances = wireRecord(raw.balances ?? raw.balance)
  const costs = wireRecord(raw.observedCosts ?? raw.costs ?? raw.activitySummary)
  const runway = wireRecord(raw.runway)
  const lastActivity = wireRecord(raw.lastActivity)
  const rawRoles = Array.isArray(raw.roles)
    ? raw.roles
    : raw.role === undefined || raw.role === null
      ? []
      : [raw.role]
  return {
    address: wireString(raw.address ?? raw.wallet) ?? '',
    roles: rawRoles
      .map((role) => wireString(role))
      .filter((role): role is string => role !== null),
    roleSources: wireArray(raw.roleSources).map(wireRecord),
    status: wireString(raw.status ?? runway.status) ?? 'unknown',
    nativeBalanceWei: wireString(
      raw.nativeBalanceWei
      ?? raw.balanceWei
      ?? balances.nativeBalanceWei
      ?? balances.nativeWei,
    ),
    observedGasCostWei: wireString(
      raw.observedGasCostWei
      ?? raw.gasCostWei
      ?? costs.observedGasCostWei
      ?? costs.gasCostWei,
    ),
    observedTransactionNativeValueWei: wireString(
      raw.observedTransactionNativeValueWei
      ?? raw.transactionNativeValueWei
      ?? costs.observedTransactionNativeValueWei
      ?? costs.transactionNativeValueWei,
    ),
    observedActionCount: wireString(
      raw.observedActionCount
      ?? raw.actionCount
      ?? costs.observedActionCount
      ?? costs.actionCount,
    ),
    observedTransactionCount: wireString(
      raw.observedTransactionCount
      ?? raw.transactionCount
      ?? costs.observedTransactionCount
      ?? costs.transactionCount,
    ),
    medianObservedSuccessfulOperationalTransactionGrossNativeSpendWei: wireString(
      raw.medianObservedSuccessfulOperationalTransactionGrossNativeSpendWei
      ?? costs.medianObservedSuccessfulOperationalTransactionGrossNativeSpendWei
      ?? runway.medianObservedSuccessfulOperationalTransactionGrossNativeSpendWei
      ?? raw.medianObservedSuccessfulOperationalTransactionNativeOutlayWei
      ?? costs.medianObservedSuccessfulOperationalTransactionNativeOutlayWei
      ?? runway.medianObservedSuccessfulOperationalTransactionNativeOutlayWei
      ?? raw.medianObservedSuccessfulActionNativeOutlayWei
      ?? costs.medianObservedSuccessfulActionNativeOutlayWei
      ?? runway.medianObservedSuccessfulActionNativeOutlayWei,
    ),
    estimatedTransactionsAtObservedGrossSpend: wireString(
      raw.estimatedTransactionsAtObservedGrossSpend
      ?? runway.estimatedTransactionsAtObservedGrossSpend
      ?? raw.estimatedTransactionsRemaining
      ?? runway.estimatedTransactionsRemaining
      ?? raw.actionsRemainingEstimate
      ?? raw.estimatedActionsRemaining
      ?? runway.actionsRemainingEstimate
      ?? runway.estimatedActionsRemaining,
    ),
    runwayFormula: normalizeRunwayFormula(raw.runwayFormula, runway, raw.formulaIdentifier),
    lastActivityTimestamp: wireTimestamp(
      raw.lastActivityTimestamp
      ?? raw.lastActivityAt
      ?? raw.lastSuccessfulActionAt
      ?? costs.lastActivityTimestamp
      ?? lastActivity.timestamp
      ?? lastActivity.occurredAt,
    ),
    lastActivityTransactionHash: wireString(
      raw.lastActivityTransactionHash
      ?? raw.lastTransactionHash
      ?? lastActivity.transactionHash
      ?? lastActivity.txHash,
    ),
    evidence: wireRecord(raw.evidence),
    availability: wireAvailability(raw.availability),
    raw,
  }
}

function normalizeProtocolWalletActivity(value: unknown, index: number): ProtocolWalletActivity {
  const raw = wireRecord(value)
  const receipt = wireRecord(raw.receipt)
  const transactionHash = wireString(raw.transactionHash ?? raw.txHash)
  const actionEvidence = wireRecord(raw.evidence)
  const transactionEvidence = wireRecord(raw.transactionEvidence)
  const transactionAvailability = wireAvailability(raw.transactionAvailability)
  const actionAvailability = wireAvailability(raw.availability)
  return {
    activityId:
      wireString(raw.activityId ?? raw.actionId ?? raw.id)
      ?? `${transactionHash ?? 'wallet-activity'}:${String(index)}`,
    transactionHash,
    timestamp: wireTimestamp(raw.timestamp ?? raw.occurredAt ?? raw.blockTimestamp),
    actionType: wireString(raw.actionType ?? raw.type ?? raw.activityType) ?? 'protocol_action',
    outcome: wireString(raw.outcome ?? raw.status) ?? 'success',
    gasCostWei: wireString(raw.gasCostWei ?? receipt.gasCostWei),
    nativeValueWei: wireString(
      raw.nativeValueWei
      ?? raw.transactionNativeValueWei
      ?? receipt.nativeValueWei,
    ),
    evidence:
      Object.keys(actionEvidence).length > 0
      || Object.keys(transactionEvidence).length > 0
      || wireString(raw.evidence) !== null
      || wireString(raw.transactionEvidence) !== null
        ? {
            action: raw.evidence ?? null,
            transaction: raw.transactionEvidence ?? null,
          }
        : { level: 'unavailable' },
    availability: [...actionAvailability, ...transactionAvailability],
    raw,
  }
}

function normalizeOperationalWalletDefinition(value: unknown): Record<string, unknown> {
  const definition = wireRecord(value)
  if (Object.keys(definition).length > 0) return definition
  const text = wireString(value)
  return {
    trackedIdentity:
      text
      ?? 'A public, release-scoped protocol wallet whose native-token balance can affect liveness.',
    interpretation:
      'Operational transaction capacity is a conservative gross-spend diagnostic; refunds are not netted, and it is not a time estimate, net cost, or profit calculation.',
  }
}

function normalizeRunwayFormula(
  rawFormula: unknown,
  runway: Record<string, unknown>,
  rawFormulaIdentifier: unknown,
): Record<string, unknown> | null {
  const formula = wireRecord(rawFormula)
  const source = Object.keys(formula).length > 0 ? formula : runway
  const normalized = {
    formulaIdentifier:
      wireString(source.formulaIdentifier ?? rawFormulaIdentifier),
    calculationVersion: wireString(source.calculationVersion),
    releaseCalculationVersion: wireString(source.releaseCalculationVersion),
    estimateKind: wireString(source.estimateKind),
    expression: wireString(source.expression ?? source.formula),
    sampleCount: wireString(source.sampleCount),
  }
  return Object.values(normalized).some((item) => item !== null) ? normalized : null
}

function protocolEnvelope(root: Record<string, unknown>): ProtocolEnvelope {
  const confirmedBlock = wireRecord(root.confirmedBlock)
  return {
    releaseId: wireString(root.releaseId) ?? '',
    chainId: wireString(root.chainId) ?? '',
    confirmedBlock: {
      number: wireString(confirmedBlock.number) ?? '0',
      hash: wireString(confirmedBlock.hash) ?? '',
      timestamp: wireTimestamp(confirmedBlock.timestamp) ?? 0,
    },
    indexerTimestamp: wireTimestamp(root.indexerTimestamp),
    calculationVersion: wireString(root.calculationVersion) ?? '',
    evidence: wireRecord(root.evidence),
    availability: wireAvailability(root.availability),
  }
}

function wireRecord(value: unknown): Record<string, unknown> {
  return typeof value === 'object' && value !== null && !Array.isArray(value)
    ? value as Record<string, unknown>
    : {}
}

function wireArray(value: unknown): unknown[] {
  return Array.isArray(value) ? value : []
}

function wireString(value: unknown): string | null {
  return typeof value === 'string' || typeof value === 'number' || typeof value === 'bigint'
    ? String(value)
    : null
}

function wireBoolean(value: unknown): boolean | null {
  return typeof value === 'boolean' ? value : null
}

function wireTimestamp(value: unknown): number | null {
  if (typeof value === 'string' && !/^\d+(?:\.\d+)?$/.test(value)) {
    const parsedDate = Date.parse(value)
    return Number.isNaN(parsedDate) ? null : Math.floor(parsedDate / 1000)
  }
  const numeric = Number(value)
  return Number.isFinite(numeric) && numeric > 0 ? numeric : null
}

function wireAvailability(value: unknown): { field: string; reason: string }[] {
  return wireArray(value).flatMap((item) => {
    const record = wireRecord(item)
    const field = wireString(record.field)
    const reason = wireString(record.reason)
    return field !== null && reason !== null ? [{ field, reason }] : []
  })
}

function wireStringRecord(value: unknown): Record<string, string> {
  return Object.fromEntries(
    Object.entries(wireRecord(value)).flatMap(([key, item]) => {
      const normalized = wireString(item)
      return normalized === null ? [] : [[key, normalized]]
    }),
  )
}

function normalizeCompetition(raw: WireCompetition): Competition {
  return {
    id: raw.id ?? raw.slug,
    slug: raw.slug,
    name: raw.name,
    status: raw.status ?? normalizePhase(raw.phase),
    startsAt: raw.startsAt ?? raw.startAt ?? '',
    tradingCutoffAt: raw.tradingCutoffAt ?? raw.scoreCutoffAt ?? '',
    resultsAt: raw.resultsAt,
    startingBalance: raw.startingBalance ?? raw.startingBalanceUsdc ?? '0',
    pnlEligibilityThreshold:
      raw.pnlEligibilityThreshold ?? raw.minimumProfitUsdc ?? '0',
    minActiveDays: raw.minActiveDays ?? raw.minimumActiveDays ?? 0,
    prizes: raw.prizes.map((prize) => ({
      place: prize.place,
      amount: prize.amount ?? prize.amountUsdc ?? '0',
    })),
    latestIndexedBlock: raw.latestIndexedBlock ?? null,
    latestIndexedAt: raw.latestIndexedAt ?? null,
    participantCount: raw.participantCount,
    eligibleCount: raw.eligibleCount,
  }
}

function normalizePhase(phase: string | undefined): Competition['status'] {
  switch (phase) {
    case 'upcoming': return 'scheduled'
    case 'live': return 'live'
    case 'final': return 'final'
    case 'review': return 'review'
    case 'provisional_results': return 'review'
    default: return 'scheduled'
  }
}

function normalizeStanding(raw: WireStanding, competition: WireCompetition): WalletResponse['wallet'] {
  const reasons = raw.eligibilityReasons ? [...raw.eligibilityReasons] : []
  const minimumDays = competition.minActiveDays ?? competition.minimumActiveDays ?? 5
  if (raw.scoreAvailable === false) reasons.push('Awaiting a finalized account snapshot')
  if (raw.scoreAvailable !== false && raw.meetsProfitRequirement === false) {
    reasons.push('Below the +1% net P&L threshold')
  }
  if (raw.meetsActiveDaysRequirement === false) {
    reasons.push(`${String(raw.activeDays)} of ${String(minimumDays)} active days`)
  }
  if (raw.eligibilityStatus === 'pending') reasons.push('Integrity review pending')
  if (raw.eligibilityStatus === 'under_review') reasons.push('Integrity review in progress')
  if (raw.eligibilityStatus === 'ineligible') {
    reasons.push(raw.eligibilityReason ?? 'Not eligible after integrity review')
  }

  return {
    rank: raw.rank ?? null,
    address: raw.address ?? raw.wallet ?? '',
    displayName: raw.displayName ?? raw.alias ?? null,
    pnl: raw.pnl ?? raw.finalPnlUsdc ?? null,
    realizedPnl: raw.realizedPnl ?? raw.realizedPnlUsdc ?? '0',
    roiBps: raw.roiBps ?? null,
    volume: raw.volume ?? raw.volumeUsdc ?? '0',
    trades: raw.trades ?? raw.executedTrades ?? 0,
    activeDays: raw.activeDays,
    liquidations: raw.liquidations,
    prizePlace: raw.prizePlace ?? null,
    prizePlaces: raw.prizePlaces ?? [],
    prizeAmountUsdc: raw.prizeAmountUsdc ?? null,
    eligible: raw.eligible ?? raw.prizeEligible ?? false,
    eligibilityStatus: raw.eligibilityStatus ?? ((raw.eligible ?? raw.prizeEligible) ? 'eligible' : 'pending'),
    eligibilityReasons: [...new Set(reasons)],
    equity: raw.equity ?? raw.currentAccountValueUsdc ?? null,
    position: normalizePosition(raw.position),
  }
}

function normalizeActivity(raw: WireActivity): WalletActivity {
  const activityType = raw.type ?? raw.activityType ?? 'activity'
  const side = normalizeSide(raw.side)
  const sizeDelta = raw.sizeDelta ?? null
  const priceRaw = raw.price ?? null
  return {
    id: raw.id ?? `${raw.txHash ?? 'activity'}:${String(raw.logIndex ?? 0)}`,
    type: activityType,
    occurredAt: raw.occurredAt,
    market: raw.market ?? (['Open', 'Close', 'Liquidated'].includes(activityType) ? 'DXY' : null),
    side,
    size: raw.size ?? sizeDeltaToNotionalUsdc(sizeDelta, priceRaw) ?? raw.amountUsdc ?? null,
    sizeDelta,
    price: normalizePrice(priceRaw),
    pnl: raw.pnl ?? raw.pnlUsdc ?? null,
    executionFee:
      raw.executionFee
      ?? raw.executionFeeUsdc
      ?? raw.protocolFee
      ?? raw.protocolFeeUsdc
      ?? null,
    vpi: raw.vpi ?? raw.vpiUsdc ?? raw.vpiDeltaUsdc ?? null,
    txHash: raw.txHash ?? null,
  }
}

function sumRealizedPnl(activity: WalletActivity[] | null): string {
  if (!activity) return '0'
  try {
    return activity.reduce(
      (total, item) => total + (item.pnl !== null && /^-?\d+$/.test(item.pnl) ? BigInt(item.pnl) : 0n),
      0n,
    ).toString()
  } catch {
    return '0'
  }
}

function normalizePosition(raw: WirePosition | null | undefined): WalletResponse['wallet']['position'] {
  if (!raw) return null
  const side = normalizeSide(raw.side ?? raw.sideCode)
  const sizeDelta = raw.sizeDelta ?? null
  const entryPriceRaw = raw.entryPrice ?? null
  return {
    market: raw.market ?? 'plDXY Perp',
    side,
    size: raw.size ?? sizeDeltaToNotionalUsdc(sizeDelta, entryPriceRaw),
    sizeDelta,
    margin: raw.margin ?? raw.marginUsdc ?? null,
    entryPrice: normalizePrice(entryPriceRaw),
    markPrice: normalizePrice(raw.markPrice),
    unrealizedPnl: raw.unrealizedPnl ?? raw.unrealizedPnlUsdc ?? null,
    liquidatable: raw.liquidatable ?? null,
  }
}

function normalizeSide(value: string | number | null | undefined): 'long' | 'short' | null {
  if (value === 'long' || value === 0 || value === '0') return 'long'
  if (value === 'short' || value === 1 || value === '1') return 'short'
  return null
}

function sizeDeltaToNotionalUsdc(
  sizeDelta: string | null | undefined,
  price: string | null | undefined,
): string | null {
  if (sizeDelta == null || price == null || !/^-?\d+$/.test(sizeDelta) || !/^\d+$/.test(price)) return null
  try {
    const absoluteSize = BigInt(sizeDelta) < 0n ? -BigInt(sizeDelta) : BigInt(sizeDelta)
    return ((absoluteSize * BigInt(price)) / 100_000_000_000_000_000_000n).toString()
  } catch {
    return null
  }
}

function normalizePrice(value: string | null | undefined): string | null {
  if (value == null) return null
  if (!/^-?\d+$/.test(value)) return value
  try {
    const basketPrice = BigInt(value)
    if (basketPrice <= 0n || basketPrice >= PLDXY_PRICE_CAP) return null
    return formatIntegerUnits((PLDXY_PRICE_CAP - basketPrice).toString(), PERPS_PRICE_DECIMALS)
  } catch {
    return null
  }
}

function normalizeStatus(response: WireStatusResponse): InsightsStatus {
  const raw: WireDataStatus = response.status ?? response
  const latestIndexedBlock =
    raw.latestIndexedBlock ?? parseOptionalNumber(raw.indexedThroughBlock)
  const latestIndexedAt = raw.latestIndexedAt ?? raw.indexerUpdatedAt ?? null
  const indexedRecently = latestIndexedAt !== null
    && Date.now() - new Date(latestIndexedAt).getTime() < 5 * 60_000
  return {
    healthy: raw.healthy ?? (latestIndexedBlock !== null && indexedRecently),
    latestIndexedBlock,
    latestIndexedAt,
    chainId: response.chainId ?? parseOptionalNumber(response.competition?.chainId) ?? undefined,
    participantCount: raw.participantCount,
    eligibleCount: raw.eligibleCount,
  }
}

function parseOptionalNumber(value: string | number | null | undefined): number | null {
  if (value == null) return null
  const parsed = Number(value)
  return Number.isFinite(parsed) ? parsed : null
}

function formatIntegerUnits(value: string | null | undefined, decimals: number): string | null {
  if (value == null || !/^-?\d+$/.test(value)) return null
  const negative = value.startsWith('-')
  const digits = negative ? value.slice(1) : value
  const padded = digits.padStart(decimals + 1, '0')
  const whole = padded.slice(0, -decimals)
  const fraction = padded.slice(-decimals).replace(/0+$/, '')
  return `${negative ? '-' : ''}${whole}${fraction ? `.${fraction}` : ''}`
}
