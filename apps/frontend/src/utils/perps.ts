import { formatUnits, isHex, parseUnits, type Hex } from 'viem'
import { defaultApiBaseUrl } from '../api/client'
import { PERPS_DECIMALS, PERPS_POSITION_SIZE_TO_USDC_SCALE, PERPS_SIDE, type PerpsSide } from '../contracts/perpsConstants'

export type PerpsDirection = 'long' | 'short'
export type PerpsOracleFreshness = 'fresh' | 'checking' | 'market-closed' | 'stale'
export const PERPS_DXY_PRICE_CAP = 2n * 10n ** BigInt(PERPS_DECIMALS.PRICE)

export function cleanNumericInput(value: string): string {
  return value.replaceAll(' ', '').replaceAll(',', '.')
}

export function parsePerpsUsdc(value: string): bigint {
  try {
    const cleaned = cleanNumericInput(value)
    if (!cleaned || cleaned === '.') return 0n
    return parseUnits(cleaned, PERPS_DECIMALS.USDC)
  } catch {
    return 0n
  }
}

export function formatPerpsNumber(value: number, maxDecimals = 2, minDecimals = 0): string {
  if (!Number.isFinite(value)) return '--'

  return value.toLocaleString('en-US', {
    minimumFractionDigits: minDecimals,
    maximumFractionDigits: maxDecimals,
  }).replaceAll(',', ' ')
}

export function formatPerpsUsdc(amount: bigint | undefined, maxDecimals = 2): string {
  if (amount === undefined) return '--'
  return formatPerpsNumber(Number(formatUnits(amount, PERPS_DECIMALS.USDC)), maxDecimals)
}

export function formatPerpsUsdcFloor(amount: bigint | undefined, maxDecimals = 2): string {
  if (amount === undefined) return '--'

  const decimals = Math.max(0, Math.min(maxDecimals, PERPS_DECIMALS.USDC))
  const scale = 10n ** BigInt(PERPS_DECIMALS.USDC - decimals)
  const flooredAmount = (amount / scale) * scale
  return formatPerpsNumber(Number(formatUnits(flooredAmount, PERPS_DECIMALS.USDC)), decimals)
}

export function formatSignedPerpsUsdc(amount: bigint | undefined, maxDecimals = 2): string {
  if (amount === undefined) return '--'
  const sign = amount < 0n ? '-' : amount > 0n ? '+' : ''
  const absolute = amount < 0n ? -amount : amount
  return `${sign}${formatPerpsUsdc(absolute, maxDecimals)}`
}

export function formatPerpsPrice(price: bigint | undefined, decimals = 4): string {
  if (price === undefined || price === 0n) return '--'
  return formatPerpsNumber(Number(formatUnits(price, PERPS_DECIMALS.PRICE)), decimals, decimals)
}

export function oraclePriceToDisplayDxyPrice(rawOraclePrice: bigint | undefined): bigint | undefined {
  if (rawOraclePrice === undefined || rawOraclePrice === 0n) return undefined
  return PERPS_DXY_PRICE_CAP > rawOraclePrice ? PERPS_DXY_PRICE_CAP - rawOraclePrice : 0n
}

export function formatDisplayDxyPrice(rawOraclePrice: bigint | undefined, decimals = 4): string {
  return formatPerpsPrice(oraclePriceToDisplayDxyPrice(rawOraclePrice), decimals)
}

export function displayDxyPriceToOraclePrice(displayDxyPrice: bigint | undefined): bigint | undefined {
  if (displayDxyPrice === undefined || displayDxyPrice === 0n) return undefined
  return PERPS_DXY_PRICE_CAP > displayDxyPrice ? PERPS_DXY_PRICE_CAP - displayDxyPrice : 0n
}

export function directionToPerpsSide(direction: PerpsDirection): PerpsSide {
  return direction === 'long' ? PERPS_SIDE.BULL : PERPS_SIDE.BEAR
}

export function perpsSideToDirection(side: number | bigint | undefined): PerpsDirection {
  return Number(side ?? PERPS_SIDE.BULL) === PERPS_SIDE.BEAR ? 'short' : 'long'
}

export function perpsSideLabel(side: number | bigint | undefined): string {
  return perpsSideToDirection(side) === 'long' ? 'Long' : 'Short'
}

export function notionalUsdcToSizeDelta(notionalUsdc: bigint, oraclePrice: bigint): bigint {
  if (oraclePrice === 0n) return 0n
  return ((notionalUsdc * PERPS_POSITION_SIZE_TO_USDC_SCALE) + oraclePrice - 1n) / oraclePrice
}

export function sizeDeltaToNotionalUsdc(sizeDelta: bigint | undefined, oraclePrice: bigint | undefined): bigint | undefined {
  if (sizeDelta === undefined || oraclePrice === undefined) return undefined
  return (sizeDelta * oraclePrice) / PERPS_POSITION_SIZE_TO_USDC_SCALE
}

export function dxyExposureFromContractNotional(
  contractNotionalUsdc: bigint,
  rawOraclePrice: bigint | undefined
): bigint | undefined {
  if (contractNotionalUsdc <= 0n) return 0n
  const displayDxyPrice = oraclePriceToDisplayDxyPrice(rawOraclePrice)
  if (rawOraclePrice === undefined || rawOraclePrice <= 0n || displayDxyPrice === undefined || displayDxyPrice <= 0n) {
    return undefined
  }

  const sizeDelta = notionalUsdcToSizeDelta(contractNotionalUsdc, rawOraclePrice)
  return sizeDeltaToNotionalUsdc(sizeDelta, displayDxyPrice)
}

function applyBps(price: bigint, bps: number, mode: 'up' | 'down'): bigint {
  const bpsInt = BigInt(Math.round(bps * 100))
  const denominator = 10_000n
  if (mode === 'up') return (price * (denominator + bpsInt)) / denominator
  return (price * (denominator - bpsInt)) / denominator
}

export function getPerpsTargetPrice({
  direction,
  isClose,
  oraclePrice,
  slippagePercent,
}: {
  direction: PerpsDirection
  isClose: boolean
  oraclePrice: bigint
  slippagePercent: number
}): bigint {
  if (!Number.isFinite(slippagePercent)) return 0n
  const mode = isClose
    ? direction === 'long' ? 'up' : 'down'
    : direction === 'long' ? 'down' : 'up'
  return applyBps(oraclePrice, slippagePercent, mode)
}

export interface PerpsPythUpdatePayload {
  updateData: Hex[]
  fetchedAt: number
  publishTimes: number[]
}

interface BackendPythUpdateResponse {
  data?: {
    updateData?: string[]
    fetchedAt?: number
    publishTimes?: number[]
  }
}

interface BackendRevealPayloadResponse {
  data?: {
    updateData?: string[]
    fetchedAt?: number
    publishTimes?: number[]
    minPublishTime?: number
    maxPublishTime?: number
    source?: string
  }
}

interface BackendErrorResponse {
  error?: {
    code?: string
    message?: string
    details?: {
      retryAfter?: string | null
    }
  }
}

const HERMES_DEFAULT_RATE_LIMIT_MS = 60_000
const HERMES_HISTORICAL_CACHE_LIMIT = 32
let hermesRateLimitUntil = 0
const historicalPythCache = new Map<number, PerpsPythUpdatePayload>()
const historicalPythInFlight = new Map<number, Promise<PerpsPythUpdatePayload>>()

function normalizeHex(value: string): Hex {
  return isHex(value) ? value : `0x${value}`
}

function sleep(ms: number): Promise<void> {
  return new Promise((resolve) => {
    window.setTimeout(resolve, ms)
  })
}

function isPythUpdateNotFoundError(error: unknown): boolean {
  const message = error instanceof Error ? error.message.toLowerCase() : String(error).toLowerCase()
  return message.includes('404') && message.includes('update data not found')
}

function describeRetryAfter(value: string | null): string {
  if (!value) return 'Wait about 60 seconds before retrying.'

  const seconds = Number(value)
  if (Number.isFinite(seconds) && seconds > 0) {
    return `Retry after ${Math.ceil(seconds).toString()}s.`
  }

  const retryAt = Date.parse(value)
  if (Number.isFinite(retryAt)) {
    const waitSeconds = Math.max(1, Math.ceil((retryAt - Date.now()) / 1000))
    return `Retry after ${waitSeconds.toString()}s.`
  }

  return 'Wait about 60 seconds before retrying.'
}

function retryAfterMs(value: string | null): number {
  if (!value) return HERMES_DEFAULT_RATE_LIMIT_MS

  const seconds = Number(value)
  if (Number.isFinite(seconds) && seconds > 0) {
    return Math.ceil(seconds * 1000)
  }

  const retryAt = Date.parse(value)
  if (Number.isFinite(retryAt)) {
    return Math.max(1_000, retryAt - Date.now())
  }

  return HERMES_DEFAULT_RATE_LIMIT_MS
}

function cacheHistoricalPythPayload(publishTime: number, payload: PerpsPythUpdatePayload): void {
  historicalPythCache.set(publishTime, payload)
  if (historicalPythCache.size <= HERMES_HISTORICAL_CACHE_LIMIT) return

  const firstKey = historicalPythCache.keys().next().value
  if (firstKey !== undefined) historicalPythCache.delete(firstKey)
}

function perpsApiUrl(path: string): URL {
  const apiBase = defaultApiBaseUrl()
  const normalizedBase = apiBase.endsWith('/') ? apiBase.slice(0, -1) : apiBase
  return new URL(`${normalizedBase}${path}`, window.location.origin)
}

function pythRateLimitError(retryAfter: string | null): Error {
  hermesRateLimitUntil = Date.now() + retryAfterMs(retryAfter)
  return new Error(
    `Hermes rate limit reached. ${describeRetryAfter(retryAfter)} Public Hermes is shared and can temporarily block requests; a reveal order may expire before the public endpoint unblocks.`
  )
}

async function parseBackendPythError(response: Response): Promise<Error> {
  const parsed = await response.json().catch(() => undefined) as BackendErrorResponse | undefined
  const code = parsed?.error?.code
  const message = parsed?.error?.message
  const retryAfter = parsed?.error?.details?.retryAfter ?? response.headers.get('retry-after')

  if (response.status === 429 || code === 'RATE_LIMITED') {
    return pythRateLimitError(retryAfter)
  }

  return new Error(message ?? `Pyth update request failed: ${response.status.toString()}`)
}

async function parseRevealPayloadError(response: Response, orderId: bigint): Promise<Error> {
  const parsed = await response.json().catch(() => undefined) as BackendErrorResponse | undefined
  const message = parsed?.error?.message

  return new Error(
    message ??
      `Reveal payload unavailable for order ${orderId.toString()}. Keep the basket worker running and retry before the order expires.`
  )
}

async function fetchPerpsPythUpdatePayloadUncached(publishTime?: number): Promise<PerpsPythUpdatePayload> {
  const now = Date.now()
  if (now < hermesRateLimitUntil) {
    throw new Error(
      `Hermes rate limit reached. Retry after ${Math.ceil((hermesRateLimitUntil - now) / 1000).toString()}s. Public Hermes is shared and can temporarily block requests; a reveal order may expire before the public endpoint unblocks.`
    )
  }

  const requestUrl = perpsApiUrl('/perps/pyth/update')
  if (publishTime !== undefined) {
    requestUrl.searchParams.set('publishTime', String(publishTime))
  }

  let response: Response
  try {
    response = await fetch(requestUrl)
  } catch (error) {
    throw new Error(
      `Could not fetch Pyth update data from the backend. Check that the backend is running and configured with PYTH_HERMES_URL. ${
        error instanceof Error ? error.message : ''
      }`.trim()
    )
  }

  if (!response.ok) {
    throw await parseBackendPythError(response)
  }

  const payload = await response.json() as BackendPythUpdateResponse
  const updates = payload.data?.updateData
  if (!updates?.length) {
    throw new Error('Backend did not return Pyth update data')
  }

  return {
    updateData: updates.map(normalizeHex),
    fetchedAt: payload.data?.fetchedAt ?? Math.floor(Date.now() / 1000),
    publishTimes: payload.data?.publishTimes ?? [],
  }
}

export async function fetchPerpsPythUpdatePayload(publishTime?: number): Promise<PerpsPythUpdatePayload> {
  if (publishTime === undefined) {
    return fetchPerpsPythUpdatePayloadUncached()
  }

  const cached = historicalPythCache.get(publishTime)
  if (cached) return cached

  const inFlight = historicalPythInFlight.get(publishTime)
  if (inFlight) return inFlight

  const request = fetchPerpsPythUpdatePayloadUncached(publishTime)
    .then((payload) => {
      cacheHistoricalPythPayload(publishTime, payload)
      return payload
    })
    .finally(() => {
      historicalPythInFlight.delete(publishTime)
    })

  historicalPythInFlight.set(publishTime, request)
  return request
}

export async function fetchPerpsPythUpdatePayloadForWindow(
  minPublishTime: number,
  maxPublishTime: number
): Promise<PerpsPythUpdatePayload> {
  let lastNotFound: Error | undefined
  const maxAttempts = 2
  // Pyth parsePriceFeedUpdatesUnique expects the first unique update after commit.
  // Newer updates can be inside the settlement window but still fail uniqueness.
  const publishTimeCandidates = Array.from(
    { length: Math.max(0, maxPublishTime - minPublishTime + 1) },
    (_, index) => minPublishTime + index
  )

  for (let attempt = 0; attempt < maxAttempts; attempt += 1) {
    if (attempt > 0) {
      await sleep(750)
    }

    for (const publishTime of publishTimeCandidates) {
      try {
        const payload = await fetchPerpsPythUpdatePayload(publishTime)
        const returnedMinPublishTime = payload.publishTimes.length ? Math.min(...payload.publishTimes) : undefined
        const returnedMaxPublishTime = payload.publishTimes.length ? Math.max(...payload.publishTimes) : undefined
        if (
          returnedMinPublishTime !== undefined &&
          returnedMaxPublishTime !== undefined &&
          returnedMinPublishTime >= minPublishTime &&
          returnedMaxPublishTime <= maxPublishTime
        ) {
          return payload
        }
      } catch (error) {
        if (isPythUpdateNotFoundError(error)) {
          lastNotFound = error instanceof Error ? error : new Error(String(error))
          continue
        }
        throw error
      }
    }
  }

  throw new Error(
    `Hermes did not return Pyth update data for the valid reveal window ${minPublishTime.toString()} to ${maxPublishTime.toString()}. ${
      lastNotFound?.message ?? ''
    }`.trim()
  )
}

export async function fetchPerpsRevealPayload(
  orderId: bigint,
  minPublishTime: number,
  maxPublishTime: number
): Promise<PerpsPythUpdatePayload> {
  const requestUrl = perpsApiUrl(`/perps/orders/${orderId.toString()}/reveal-payload`)
  requestUrl.searchParams.set('minPublishTime', String(minPublishTime))
  requestUrl.searchParams.set('maxPublishTime', String(maxPublishTime))

  let response: Response
  try {
    response = await fetch(requestUrl)
  } catch (error) {
    throw new Error(
      `Could not fetch cached reveal payload from the backend. Check that the backend and plether-basket-worker are running. ${
        error instanceof Error ? error.message : ''
      }`.trim()
    )
  }

  if (!response.ok) {
    throw await parseRevealPayloadError(response, orderId)
  }

  const payload = await response.json() as BackendRevealPayloadResponse
  const updates = payload.data?.updateData
  if (!updates?.length) {
    throw new Error(`Cached reveal payload for order ${orderId.toString()} did not include Pyth update data`)
  }

  return {
    updateData: updates.map(normalizeHex),
    fetchedAt: payload.data?.fetchedAt ?? Math.floor(Date.now() / 1000),
    publishTimes: payload.data?.publishTimes ?? [],
  }
}

export async function fetchPerpsPythUpdateData(): Promise<Hex[]> {
  return (await fetchPerpsPythUpdatePayload()).updateData
}
