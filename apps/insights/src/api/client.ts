import type {
  ApiErrorBody,
  Competition,
  InsightsStatus,
  LeaderboardResponse,
  RegistrationSession,
  WalletActivity,
  WalletChallenge,
  WalletResponse,
} from './types'

export const DEFAULT_COMPETITION_SLUG = 'testnet-trading-2026-09'
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
  releaseReady?: boolean
  registration?: {
    status: 'upcoming' | 'open' | 'closed'
    opensAt: string
    closesAt: string
    minimumXAccountAgeDays: number
    targetXHandle: string
    rulesVersion: string
    privacyVersion: string
  }
  fxSessionBoundaryUtc?: string
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
  activityStatus?: 'live' | 'omitted_after_finalization'
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
  readonly retryAfterSeconds: number | null

  constructor(message: string, status: number, code?: string, retryAfterSeconds: number | null = null) {
    super(message)
    this.name = 'InsightsApiError'
    this.status = status
    this.code = code
    this.retryAfterSeconds = retryAfterSeconds
  }
}

async function parseResponse<T>(response: Response): Promise<T> {
  if (!response.ok) {
    let body: ApiErrorBody | undefined
    try {
      body = (await response.json()) as ApiErrorBody
    } catch {
      body = undefined
    }
    const message = body?.error?.message ?? body?.message ?? `Request failed (${String(response.status)})`
    const retryAfter = response.headers.get('Retry-After')
    const parsedRetryAfter = retryAfter === null ? null : Number.parseInt(retryAfter, 10)
    throw new InsightsApiError(
      message,
      response.status,
      body?.error?.code,
      parsedRetryAfter !== null && Number.isFinite(parsedRetryAfter) ? parsedRetryAfter : null,
    )
  }

  const body = (await response.json()) as T | ApiEnvelope<T>
  if (typeof body === 'object' && body !== null && 'data' in body) {
    return body.data
  }
  return body
}

async function request<T>(path: string, signal?: AbortSignal): Promise<T> {
  const response = await fetch(`${API_ROOT}${path}`, {
    headers: { Accept: 'application/json' },
    signal,
  })
  return parseResponse<T>(response)
}

async function registrationRequest<T>(
  path: string,
  options: {
    method?: 'GET' | 'POST'
    body?: Record<string, unknown>
    csrfToken?: string
    signal?: AbortSignal
  } = {},
): Promise<T> {
  const headers = new Headers({ Accept: 'application/json' })
  if (options.body) headers.set('Content-Type', 'application/json')
  if (options.csrfToken) headers.set('X-Registration-CSRF', options.csrfToken)

  const response = await fetch(`${API_ROOT}${path}`, {
    method: options.method ?? 'GET',
    credentials: 'include',
    headers,
    body: options.body ? JSON.stringify(options.body) : undefined,
    signal: options.signal,
  })
  return parseResponse<T>(response)
}

function registrationBase(slug: string): string {
  return `/competitions/${encodeURIComponent(slug)}/registrations`
}

function unwrapRegistration(
  response: RegistrationSession | { registration: RegistrationSession },
): RegistrationSession {
  return 'registration' in response ? response.registration : response
}

export async function getRegistrationSession(
  slug: string,
  signal?: AbortSignal,
): Promise<RegistrationSession> {
  const response = await registrationRequest<RegistrationSession | { registration: RegistrationSession }>(
    `${registrationBase(slug)}/session`,
    { signal },
  )
  return unwrapRegistration(response)
}

export async function createRegistrationSession(
  slug: string,
  turnstileToken: string,
  signal?: AbortSignal,
): Promise<RegistrationSession> {
  const response = await registrationRequest<RegistrationSession | { registration: RegistrationSession }>(
    `${registrationBase(slug)}/session`,
    { method: 'POST', body: { turnstileToken }, signal },
  )
  return unwrapRegistration(response)
}

export async function createXAuthorization(slug: string, csrfToken: string): Promise<string> {
  const response = await registrationRequest<{ authorizationUrl: string }>(
    `${registrationBase(slug)}/x/authorize`,
    { method: 'POST', csrfToken, body: {} },
  )
  return response.authorizationUrl
}

export async function confirmXFollow(slug: string, csrfToken: string): Promise<RegistrationSession> {
  const response = await registrationRequest<RegistrationSession | { registration: RegistrationSession }>(
    `${registrationBase(slug)}/x/follow`,
    { method: 'POST', csrfToken, body: {} },
  )
  return unwrapRegistration(response)
}

export function createWalletChallenge(
  slug: string,
  csrfToken: string,
  ownerAddress: string,
): Promise<WalletChallenge> {
  return registrationRequest<WalletChallenge>(
    `${registrationBase(slug)}/wallet/challenge`,
    { method: 'POST', csrfToken, body: { ownerAddress } },
  )
}

export async function verifyRegistrationWallet(
  slug: string,
  csrfToken: string,
  ownerAddress: string,
  signature: string,
): Promise<RegistrationSession> {
  const response = await registrationRequest<RegistrationSession | { registration: RegistrationSession }>(
    `${registrationBase(slug)}/wallet/verify`,
    { method: 'POST', csrfToken, body: { ownerAddress, signature } },
  )
  return unwrapRegistration(response)
}

export async function completeRegistration(
  slug: string,
  csrfToken: string,
  rulesVersion: string,
  privacyVersion: string,
): Promise<RegistrationSession> {
  const response = await registrationRequest<RegistrationSession | { registration: RegistrationSession }>(
    `${registrationBase(slug)}/complete`,
    {
      method: 'POST',
      csrfToken,
      body: { acceptRules: true, acceptPrivacy: true, rulesVersion, privacyVersion },
    },
  )
  return unwrapRegistration(response)
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
    activityStatus: response.activityStatus ?? 'live',
  }
}

export async function getStatus(signal?: AbortSignal): Promise<InsightsStatus> {
  const response = await request<WireStatusResponse>('/status', signal)
  return normalizeStatus(response)
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
    releaseReady: raw.releaseReady,
    registration: raw.registration,
    fxSessionBoundaryUtc: raw.fxSessionBoundaryUtc,
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
