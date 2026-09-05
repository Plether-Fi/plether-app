import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest'
import {
  PERPS_CANDLE_CURRENT_FRESHNESS_SLO_MS,
  PERPS_CANDLE_CURRENT_POLL_INTERVAL_MS,
} from './candlePolicy'

// @ts-expect-error -- The Cloudflare Pages worker is shipped as a plain JavaScript artifact.
import worker, { getPublicPerpsCacheKey, getPublicPerpsCachePolicy } from '../../public/_worker.js'

class MemoryCache {
  readonly responses = new Map<string, Response>()
  readonly put = vi.fn(async (request: Request, response: Response) => {
    this.responses.set(request.url, response.clone())
  })

  async match(request: Request): Promise<Response | undefined> {
    return this.responses.get(request.url)?.clone()
  }
}

class DeferredPutCache {
  readonly responses = new Map<string, Response>()
  readonly putStarted: Promise<void>
  readonly put: ReturnType<typeof vi.fn>
  private readonly allowPut: () => void

  constructor() {
    let signalPutStarted!: () => void
    let allowPut!: () => void
    this.putStarted = new Promise<void>((resolve) => {
      signalPutStarted = resolve
    })
    const putAllowed = new Promise<void>((resolve) => {
      allowPut = resolve
    })
    this.allowPut = allowPut
    this.put = vi.fn(async (request: Request, response: Response) => {
      signalPutStarted()
      await putAllowed
      this.responses.set(request.url, response.clone())
    })
  }

  releasePut(): void {
    this.allowPut()
  }

  async match(request: Request): Promise<Response | undefined> {
    return this.responses.get(request.url)?.clone()
  }
}

function jsonResponse(): Response {
  return new Response(JSON.stringify({ data: { ok: true }, meta: {} }), {
    status: 200,
    headers: { 'Content-Type': 'application/json' },
  })
}

const CANDLE_CONFIGURATION_HASH = `sha256:${'a'.repeat(64)}`

type CandleIdentity = {
  intervalSeconds: number
  seriesId: string
  configurationHash: string
  displayPriceCap: string
  volumeChainId: number
  volumeRouter: string
  volumeCoverageStart: number | null
  volumeCoverageEnd: number | null
  volumeFinalizedThrough: number | null
  volumeCoverageComplete: boolean
  datasetGeneration: number
  coverageStart: number
  coverageEnd: number
  finalizedThrough: number
}

function candleIdentity(
  datasetGeneration = 7,
  overrides: Partial<CandleIdentity> = {}
): CandleIdentity {
  return {
    intervalSeconds: 300,
    seriesId: 'dxy-v1',
    configurationHash: CANDLE_CONFIGURATION_HASH,
    displayPriceCap: '200000000',
    volumeChainId: 421_614,
    volumeRouter: '0x1111111111111111111111111111111111111111',
    volumeCoverageStart: 1_799_700_000,
    volumeCoverageEnd: 1_800_000_000,
    volumeFinalizedThrough: 1_800_000_000,
    volumeCoverageComplete: true,
    datasetGeneration,
    coverageStart: 1_799_700_000,
    coverageEnd: 1_800_000_000,
    finalizedThrough: 1_800_000_000,
    ...overrides,
  }
}

function candleResponse(
  identity: CandleIdentity,
  data: Record<string, unknown> = {},
  init: ResponseInit = {}
): Response {
  const headers = new Headers(init.headers)
  if (!headers.has('Content-Type')) headers.set('Content-Type', 'application/json')
  return new Response(JSON.stringify({
    data: {
      ...identity,
      coverageComplete: true,
      ...data,
    },
    meta: {},
  }), {
    ...init,
    headers,
  })
}

function currentCandleResponse(identity = candleIdentity()): Response {
  return candleResponse(identity, { candle: null })
}

function candlePageResponse(identity = candleIdentity()): Response {
  return candleResponse(identity, {
    cursor: 1_800_000_000,
    candles: [],
  })
}

function isCurrentCandleFetch(input: unknown): boolean {
  return new URL(String(input)).pathname.endsWith('/perps/basket/candles/current')
}

function executionContext() {
  const promises: Promise<unknown>[] = []
  return {
    promises,
    waitUntil(promise: Promise<unknown>) {
      promises.push(promise)
    },
  }
}

function workerEnv(overrides: Record<string, unknown> = {}) {
  return {
    BACKEND_URL: 'https://backend.example',
    ASSETS: { fetch: vi.fn() },
    ...overrides,
  }
}

function requestWithHeaders(url: string, headers: Record<string, string>): Request {
  return {
    method: 'GET',
    url,
    headers: new Headers(headers),
  } as unknown as Request
}

beforeEach(() => {
  vi.stubGlobal('caches', { default: new MemoryCache() })
})

afterEach(() => {
  vi.restoreAllMocks()
  vi.unstubAllGlobals()
})

describe('Perps public edge-cache allowlist', () => {
  it('allows only anonymous GETs for exact public market-data routes', () => {
    const latest = new Request('https://app.example/api/perps/v1/perps/basket/latest')
    const history = new Request(
      'https://app.example/api/perps/v1/perps/basket/history?includeComponents=true&interval=3600&range=24h'
    )

    expect(getPublicPerpsCachePolicy(latest)).toMatchObject({ freshSeconds: 5 })
    expect(getPublicPerpsCachePolicy(history)).toMatchObject({ freshSeconds: 60 })
    for (const query of [
      'range=24h&interval=60',
      'range=24h&interval=180',
      'range=7d&interval=300',
      'range=7d&interval=900',
      'range=30d&interval=1800',
      'range=30d&interval=3600',
      'range=1y&interval=86400',
    ]) {
      expect(getPublicPerpsCachePolicy(new Request(
        `https://app.example/api/perps/v1/perps/basket/history?${query}`
      ))).toMatchObject({ freshSeconds: 2, staleWhileRevalidateSeconds: 2 })
    }
    expect(getPublicPerpsCachePolicy(new Request(
      'https://app.example/api/perps/v1/perps/basket/history?range=24h&interval=3600&includeComponents=true'
    ))).toMatchObject({ freshSeconds: 60, staleWhileRevalidateSeconds: 300 })
    expect(getPublicPerpsCachePolicy(new Request(latest, { method: 'POST' }))).toBeUndefined()
    expect(getPublicPerpsCachePolicy(requestWithHeaders(latest.url, {
      Authorization: 'Bearer private',
    }))).toBeUndefined()
    expect(getPublicPerpsCachePolicy(requestWithHeaders(latest.url, {
      Cookie: 'session=private',
    }))).toBeUndefined()
    expect(getPublicPerpsCachePolicy(requestWithHeaders(latest.url, {
      Range: 'bytes=0-100',
    }))).toBeUndefined()
    expect(getPublicPerpsCachePolicy(new Request(
      'https://app.example/api/perps/v1/perps/orders/1/reveal-payload'
    ))).toBeUndefined()
    for (const path of ['/aa/pimlico', '/aa/rpc']) {
      expect(getPublicPerpsCachePolicy(new Request(
        `https://app.example/api/perps/v1${path}`
      ))).toBeUndefined()
    }
  })

  it('rejects cache-key amplification through unsupported public query parameters', () => {
    expect(getPublicPerpsCachePolicy(new Request(
      'https://app.example/api/perps/v1/perps/basket/latest?nonce=1'
    ))).toBeUndefined()
    expect(getPublicPerpsCachePolicy(new Request(
      'https://app.example/api/perps/v1/perps/basket/history?range=24h&interval=61'
    ))).toBeUndefined()
    expect(getPublicPerpsCachePolicy(new Request(
      'https://app.example/api/perps/v1/perps/basket/history?range=24h&range=7d'
    ))).toBeUndefined()
    expect(getPublicPerpsCachePolicy(new Request(
      'https://app.example/api/perps/v1/perps/basket/history?range=1y&interval=60&includeComponents=true'
    ))).toBeUndefined()
    expect(getPublicPerpsCachePolicy(new Request(
      'https://app.example/api/perps/v1/perps/basket/history?range=24h&interval=300&includeComponents=true'
    ))).toBeUndefined()
  })

  it('canonicalizes equivalent history query ordering to one cache key', () => {
    const first = new URL(
      'https://app.example/api/perps/v1/perps/basket/history?range=24h&interval=3600&includeComponents=true'
    )
    const second = new URL(
      'https://app.example/api/perps/v1/perps/basket/history?includeComponents=true&interval=3600&range=24h'
    )

    expect(getPublicPerpsCacheKey(first)).toBe(getPublicPerpsCacheKey(second))
    expect(getPublicPerpsCacheKey(new URL(
      'https://app.example/api/perps/v1/perps/basket/history?range=24h&interval=60&includeComponents=false'
    ))).toBe(getPublicPerpsCacheKey(new URL(
      'https://app.example/api/perps/v1/perps/basket/history?range=24h&interval=60'
    )))
  })

  it('allows only canonical, bounded candle page shapes', () => {
    vi.spyOn(Date, 'now').mockReturnValue(1_800_000_100_000)

    for (const interval of ['60', '180', '300', '900', '1800', '3600', '86400']) {
      const policy = getPublicPerpsCachePolicy(new Request(
        `https://app.example/api/perps/v1/perps/basket/candles/current?interval=${interval}`
      ))
      expect(policy).toMatchObject({ freshSeconds: 2, staleWhileRevalidateSeconds: 2 })
      expect(
        ((policy?.freshSeconds ?? 0) + (policy?.staleWhileRevalidateSeconds ?? 0)) * 1_000 +
          PERPS_CANDLE_CURRENT_POLL_INTERVAL_MS
      ).toBeLessThanOrEqual(PERPS_CANDLE_CURRENT_FRESHNESS_SLO_MS)
    }

    const closedPage = new Request(
      'https://app.example/api/perps/v1/perps/basket/candles?cursor=1800000000&interval=300'
    )
    const activePage = new Request(
      'https://app.example/api/perps/v1/perps/basket/candles?interval=300&cursor=1800150000'
    )
    const clockSkewPage = new Request(
      'https://app.example/api/perps/v1/perps/basket/candles?interval=300&cursor=1800300000'
    )
    expect(getPublicPerpsCachePolicy(closedPage)).toMatchObject({
      freshSeconds: 300,
      staleWhileRevalidateSeconds: 3600,
    })
    expect(getPublicPerpsCachePolicy(activePage)).toMatchObject({
      freshSeconds: 2,
      staleWhileRevalidateSeconds: 2,
    })
    expect(getPublicPerpsCachePolicy(clockSkewPage)).toMatchObject({
      freshSeconds: 2,
      staleWhileRevalidateSeconds: 2,
    })

    for (const url of [
      'https://app.example/api/perps/v1/perps/basket/candles/current',
      'https://app.example/api/perps/v1/perps/basket/candles/current?interval=61',
      'https://app.example/api/perps/v1/perps/basket/candles/current?interval=300&nonce=1',
      'https://app.example/api/perps/v1/perps/basket/candles/current?interval=300&interval=300',
      'https://app.example/api/perps/v1/perps/basket/candles?interval=300',
      'https://app.example/api/perps/v1/perps/basket/candles?interval=300&cursor=0',
      'https://app.example/api/perps/v1/perps/basket/candles?interval=300&cursor=01800000000',
      'https://app.example/api/perps/v1/perps/basket/candles?interval=300&cursor=1800000001',
      'https://app.example/api/perps/v1/perps/basket/candles?interval=300&cursor=1800450000',
      'https://app.example/api/perps/v1/perps/basket/candles?interval=300&cursor=1800000000&limit=500',
      'https://app.example/api/perps/v1/perps/basket/candles?interval=300&interval=300&cursor=1800000000',
      'https://app.example/api/perps/v1/perps/basket/candles?interval=300&cursor=1800000000&cursor=1800000000',
    ]) {
      expect(getPublicPerpsCachePolicy(new Request(url))).toBeUndefined()
    }
  })

  it('canonicalizes candle cache keys without admitting arbitrary parameters', () => {
    const first = new URL(
      'https://app.example/api/perps/v1/perps/basket/candles?cursor=1800000000&interval=300'
    )
    const second = new URL(
      'https://app.example/api/perps/v1/perps/basket/candles?interval=300&cursor=1800000000'
    )

    expect(getPublicPerpsCacheKey(first)).toBe(
      'https://app.example/api/perps/v1/perps/basket/candles?interval=300&cursor=1800000000'
    )
    expect(getPublicPerpsCacheKey(first)).toBe(getPublicPerpsCacheKey(second))
  })
})

describe('Perps public edge caching', () => {
  it('serves repeat public reads from the edge cache', async () => {
    const fetchMock = vi.fn(async () => jsonResponse())
    vi.stubGlobal('fetch', fetchMock)
    const request = new Request('https://app.example/api/perps/v1/perps/basket/latest')
    const firstContext = executionContext()

    const first = await worker.fetch(request, workerEnv(), firstContext)
    await Promise.all(firstContext.promises)
    const second = await worker.fetch(request, workerEnv(), executionContext())

    expect(first.headers.get('X-Plether-Edge-Cache')).toBe('MISS')
    expect(second.headers.get('X-Plether-Edge-Cache')).toBe('HIT')
    expect(second.headers.get('Cache-Control')).toContain('s-maxage=5')
    expect(fetchMock).toHaveBeenCalledTimes(1)
    expect(String(fetchMock.mock.calls[0]?.[0])).toBe(
      'https://backend.example/api/perps/basket/latest'
    )
    await expect(first.json()).resolves.toMatchObject({ data: { ok: true } })
    await expect(second.json()).resolves.toMatchObject({ data: { ok: true } })
  })

  it('keeps concurrent misses request-scoped while cache storage completes', async () => {
    const cache = new DeferredPutCache()
    vi.stubGlobal('caches', { default: cache })
    const fetchMock = vi.fn(async () => jsonResponse())
    vi.stubGlobal('fetch', fetchMock)
    const request = new Request('https://app.example/api/perps/v1/perps/basket/latest')
    const firstContext = executionContext()
    const secondContext = executionContext()

    const firstPromise = worker.fetch(request, workerEnv(), firstContext)
    await cache.putStarted
    const first = await firstPromise
    const second = await worker.fetch(request, workerEnv(), secondContext)

    expect(first.headers.get('X-Plether-Edge-Cache')).toBe('MISS')
    expect(second.headers.get('X-Plether-Edge-Cache')).toBe('MISS')
    expect(fetchMock).toHaveBeenCalledTimes(2)

    cache.releasePut()
    await Promise.all([...firstContext.promises, ...secondContext.promises])
    const third = await worker.fetch(request, workerEnv(), executionContext())
    expect(third.headers.get('X-Plether-Edge-Cache')).toBe('HIT')
    expect(fetchMock).toHaveBeenCalledTimes(2)
  })

  it('serves stale data when a background refresh fails', async () => {
    const now = vi.spyOn(Date, 'now').mockReturnValue(10_000)
    const fetchMock = vi.fn()
      .mockResolvedValueOnce(jsonResponse())
      .mockResolvedValueOnce(new Response(
        JSON.stringify({ error: { message: 'temporarily unavailable' } }),
        {
          status: 503,
          headers: { 'Content-Type': 'application/json' },
        }
      ))
    vi.stubGlobal('fetch', fetchMock)
    const request = new Request('https://app.example/api/perps/v1/perps/basket/latest')
    const firstContext = executionContext()

    await worker.fetch(request, workerEnv(), firstContext)
    await Promise.all(firstContext.promises)
    now.mockReturnValue(16_000)

    const staleContext = executionContext()
    const stale = await worker.fetch(request, workerEnv(), staleContext)
    await Promise.all(staleContext.promises)

    expect(stale.headers.get('X-Plether-Edge-Cache')).toBe('STALE')
    await expect(stale.json()).resolves.toMatchObject({ data: { ok: true } })
    expect(fetchMock).toHaveBeenCalledTimes(2)
  })

  it('does not serve rollup-compatible history beyond the live freshness window', async () => {
    const now = vi.spyOn(Date, 'now').mockReturnValue(10_000)
    const fetchMock = vi.fn()
      .mockResolvedValueOnce(new Response(JSON.stringify({ data: { version: 1 } }), {
        headers: { 'Content-Type': 'application/json' },
      }))
      .mockResolvedValueOnce(new Response(JSON.stringify({ data: { version: 2 } }), {
        headers: { 'Content-Type': 'application/json' },
      }))
    vi.stubGlobal('fetch', fetchMock)
    const request = new Request(
      'https://app.example/api/perps/v1/perps/basket/history?range=30d&interval=3600'
    )
    const seedContext = executionContext()

    const seeded = await worker.fetch(request, workerEnv(), seedContext)
    await Promise.all(seedContext.promises)
    now.mockReturnValue(14_001)
    const refreshed = await worker.fetch(request, workerEnv(), executionContext())

    expect(seeded.headers.get('Cache-Control')).toContain('s-maxage=2')
    expect(seeded.headers.get('Cache-Control')).toContain('stale-while-revalidate=2')
    expect(refreshed.headers.get('X-Plether-Edge-Cache')).toBe('MISS')
    await expect(refreshed.json()).resolves.toMatchObject({ data: { version: 2 } })
    expect(fetchMock).toHaveBeenCalledTimes(2)
  })

  it('keeps concurrent candle cache misses and identity probes request-scoped', async () => {
    vi.spyOn(Date, 'now').mockReturnValue(1_800_000_000_000)
    const resolvePages: Array<(response: Response) => void> = []
    const fetchMock = vi.fn((input: unknown) => {
      if (isCurrentCandleFetch(input)) return Promise.resolve(currentCandleResponse())
      return new Promise<Response>((resolve) => {
        resolvePages.push(resolve)
      })
    })
    vi.stubGlobal('fetch', fetchMock)
    const firstRequest = new Request(
      'https://app.example/api/perps/v1/perps/basket/candles?cursor=1800000000&interval=300'
    )
    const secondRequest = new Request(
      'https://app.example/api/perps/v1/perps/basket/candles?interval=300&cursor=1800000000'
    )

    const firstPromise = worker.fetch(firstRequest, workerEnv(), executionContext())
    const secondPromise = worker.fetch(secondRequest, workerEnv(), executionContext())
    await vi.waitFor(() => expect(fetchMock).toHaveBeenCalledTimes(4))
    for (const resolvePage of resolvePages) resolvePage(candlePageResponse())
    const [first, second] = await Promise.all([firstPromise, secondPromise])

    expect(first.headers.get('X-Plether-Edge-Cache')).toBe('MISS')
    expect(second.headers.get('X-Plether-Edge-Cache')).toBe('MISS')
    await expect(first.json()).resolves.toMatchObject({ data: { datasetGeneration: 7 } })
    await expect(second.json()).resolves.toMatchObject({ data: { datasetGeneration: 7 } })
    expect(fetchMock.mock.calls.filter(([input]) => isCurrentCandleFetch(input))).toHaveLength(2)
    expect(fetchMock.mock.calls.filter(([input]) => !isCurrentCandleFetch(input))).toHaveLength(2)
  })

  it('keeps concurrent stale candle refreshes request-scoped', async () => {
    const now = vi.spyOn(Date, 'now').mockReturnValue(1_800_000_000_000)
    const resolveRefreshes: Array<(response: Response) => void> = []
    const fetchMock = vi.fn()
      .mockResolvedValueOnce(jsonResponse())
      .mockImplementation(() => new Promise<Response>((resolve) => {
        resolveRefreshes.push(resolve)
      }))
    vi.stubGlobal('fetch', fetchMock)
    const request = new Request(
      'https://app.example/api/perps/v1/perps/basket/candles/current?interval=300'
    )

    await worker.fetch(request, workerEnv(), executionContext())
    now.mockReturnValue(1_800_000_003_000)
    const firstContext = executionContext()
    const secondContext = executionContext()
    const [first, second] = await Promise.all([
      worker.fetch(request, workerEnv(), firstContext),
      worker.fetch(request, workerEnv(), secondContext),
    ])

    expect(first.headers.get('X-Plether-Edge-Cache')).toBe('STALE')
    expect(second.headers.get('X-Plether-Edge-Cache')).toBe('STALE')
    await vi.waitFor(() => expect(fetchMock).toHaveBeenCalledTimes(3))
    for (const resolveRefresh of resolveRefreshes) resolveRefresh(jsonResponse())
    await Promise.all([...firstContext.promises, ...secondContext.promises])
    expect(fetchMock).toHaveBeenCalledTimes(3)
  })

  it('returns a controlled 503 when the origin fetch throws', async () => {
    const consoleError = vi.spyOn(console, 'error').mockImplementation(() => undefined)
    const fetchMock = vi.fn().mockRejectedValue(new Error('origin unavailable'))
    vi.stubGlobal('fetch', fetchMock)

    const response = await worker.fetch(
      new Request('https://app.example/api/perps/v1/perps/basket/latest?secret=hidden'),
      workerEnv(),
      executionContext()
    )

    expect(response.status).toBe(503)
    expect(response.headers.get('Cache-Control')).toBe('no-store')
    expect(response.headers.get('Retry-After')).toBe('1')
    await expect(response.json()).resolves.toEqual({
      error: {
        code: 'EDGE_PROXY_UNAVAILABLE',
        message: 'The edge proxy is temporarily unavailable.',
      },
    })
    expect(consoleError).toHaveBeenCalledOnce()
    const logEntry = String(consoleError.mock.calls[0]?.[0])
    expect(logEntry).toContain('origin unavailable')
    expect(logEntry).toContain('/api/perps/v1/perps/basket/latest')
    expect(logEntry).not.toContain('secret=hidden')
  })

  it('never serves current-candle cache entries beyond the freshness budget', async () => {
    const now = vi.spyOn(Date, 'now').mockReturnValue(1_800_000_000_000)
    const fetchMock = vi.fn()
      .mockResolvedValueOnce(candleResponse(candleIdentity(), { version: 1 }))
      .mockResolvedValueOnce(candleResponse(candleIdentity(), { version: 2 }))
    vi.stubGlobal('fetch', fetchMock)
    const request = new Request(
      'https://app.example/api/perps/v1/perps/basket/candles/current?interval=300'
    )
    const seedContext = executionContext()

    await worker.fetch(request, workerEnv(), seedContext)
    await Promise.all(seedContext.promises)
    now.mockReturnValue(1_800_000_004_001)
    const refreshed = await worker.fetch(request, workerEnv(), executionContext())

    expect(refreshed.headers.get('X-Plether-Edge-Cache')).toBe('MISS')
    await expect(refreshed.json()).resolves.toMatchObject({ data: { version: 2 } })
    expect(fetchMock).toHaveBeenCalledTimes(2)
  })

  it('never serves active-page entries after their live reuse window', async () => {
    const now = vi.spyOn(Date, 'now').mockReturnValue(1_800_000_100_000)
    const fetchMock = vi.fn(async (input: unknown) => isCurrentCandleFetch(input)
      ? currentCandleResponse()
      : candlePageResponse())
    vi.stubGlobal('fetch', fetchMock)
    const request = new Request(
      'https://app.example/api/perps/v1/perps/basket/candles?interval=300&cursor=1800150000'
    )
    const seedContext = executionContext()

    await worker.fetch(request, workerEnv(), seedContext)
    await Promise.all(seedContext.promises)
    now.mockReturnValue(1_800_000_104_001)
    const refreshed = await worker.fetch(request, workerEnv(), executionContext())

    expect(refreshed.headers.get('X-Plether-Edge-Cache')).toBe('MISS')
    expect(fetchMock.mock.calls.filter(([input]) => !isCurrentCandleFetch(input))).toHaveLength(2)
  })

  it('does not share-cache credential-bearing candle requests', async () => {
    vi.spyOn(Date, 'now').mockReturnValue(1_800_000_000_000)
    const fetchMock = vi.fn(async () => jsonResponse())
    vi.stubGlobal('fetch', fetchMock)
    const request = new Request(
      'https://app.example/api/perps/v1/perps/basket/candles?interval=300&cursor=1800000000',
      { headers: { Authorization: 'Bearer private' } }
    )

    const first = await worker.fetch(request, workerEnv(), executionContext())
    const second = await worker.fetch(request, workerEnv(), executionContext())

    expect(first.headers.get('X-Plether-Edge-Cache')).toBeNull()
    expect(second.headers.get('X-Plether-Edge-Cache')).toBeNull()
    expect(fetchMock).toHaveBeenCalledTimes(2)
    const headers = fetchMock.mock.calls[0]?.[1]?.headers as Headers
    expect(headers.get('Authorization')).toBe('Bearer private')
    expect(fetchMock.mock.calls[0]?.[1]?.cf).toBeUndefined()
  })

  it('never caches order or reveal traffic', async () => {
    const fetchMock = vi.fn(async () => jsonResponse())
    vi.stubGlobal('fetch', fetchMock)
    const request = new Request(
      'https://app.example/api/perps/v1/perps/orders/1/reveal-payload?minPublishTime=1&maxPublishTime=2'
    )

    const first = await worker.fetch(request, workerEnv(), executionContext())
    const second = await worker.fetch(request, workerEnv(), executionContext())

    expect(first.headers.get('X-Plether-Edge-Cache')).toBeNull()
    expect(second.headers.get('X-Plether-Edge-Cache')).toBeNull()
    expect(fetchMock).toHaveBeenCalledTimes(2)
  })

  it('does not cache origin responses marked private', async () => {
    const fetchMock = vi.fn(async () => new Response(
      JSON.stringify({ data: { private: true } }),
      {
        status: 200,
        headers: {
          'Cache-Control': 'private',
          'Content-Type': 'application/json',
        },
      }
    ))
    vi.stubGlobal('fetch', fetchMock)
    const request = new Request('https://app.example/api/perps/v1/perps/market/stats')

    const first = await worker.fetch(request, workerEnv(), executionContext())
    const second = await worker.fetch(request, workerEnv(), executionContext())

    expect(first.headers.get('X-Plether-Edge-Cache')).toBeNull()
    expect(second.headers.get('X-Plether-Edge-Cache')).toBeNull()
    expect(fetchMock).toHaveBeenCalledTimes(2)
  })

  it.each([
    ['private', { 'Cache-Control': 'private' }],
    ['no-store', { 'Cache-Control': 'no-store' }],
  ])('does not cache candle responses protected by %s', async (
    _label,
    protectedHeaders
  ) => {
    vi.spyOn(Date, 'now').mockReturnValue(1_800_000_000_000)
    const fetchMock = vi.fn(async (input: unknown) => {
      if (isCurrentCandleFetch(input)) return currentCandleResponse()
      return candleResponse(candleIdentity(), {
        cursor: 1_800_000_000,
        candles: [],
      }, {
        headers: protectedHeaders,
      })
    })
    vi.stubGlobal('fetch', fetchMock)
    const request = new Request(
      'https://app.example/api/perps/v1/perps/basket/candles?interval=300&cursor=1800000000'
    )

    const first = await worker.fetch(request, workerEnv(), executionContext())
    const second = await worker.fetch(request, workerEnv(), executionContext())

    expect(first.headers.get('X-Plether-Edge-Cache')).toBeNull()
    expect(second.headers.get('X-Plether-Edge-Cache')).toBeNull()
    expect(fetchMock).toHaveBeenCalledTimes(4)
    expect(fetchMock.mock.calls[0]?.[1]?.cf).toBeUndefined()
  })

  it('does not cache candle responses carrying Set-Cookie', async () => {
    vi.spyOn(Date, 'now').mockReturnValue(1_800_000_000_000)
    const fetchMock = vi.fn(async (input: unknown) => {
      if (isCurrentCandleFetch(input)) return currentCandleResponse()
      const response = candlePageResponse()
      vi.spyOn(response.headers, 'has').mockImplementation(
        (name) => name.toLowerCase() === 'set-cookie' || Headers.prototype.has.call(response.headers, name)
      )
      return response
    })
    vi.stubGlobal('fetch', fetchMock)
    const request = new Request(
      'https://app.example/api/perps/v1/perps/basket/candles?interval=300&cursor=1800000000'
    )

    const first = await worker.fetch(request, workerEnv(), executionContext())
    const second = await worker.fetch(request, workerEnv(), executionContext())

    expect(first.headers.get('X-Plether-Edge-Cache')).toBeNull()
    expect(second.headers.get('X-Plether-Edge-Cache')).toBeNull()
    expect(fetchMock).toHaveBeenCalledTimes(4)
  })

  it('makes generation A cache entries unreachable after the current identity advances to B', async () => {
    vi.spyOn(Date, 'now').mockReturnValue(1_800_000_000_000)
    let generation = 7
    const fetchMock = vi.fn(async (input: unknown) => isCurrentCandleFetch(input)
      ? currentCandleResponse(candleIdentity(generation))
      : candlePageResponse(candleIdentity(generation)))
    vi.stubGlobal('fetch', fetchMock)
    const url =
      'https://app.example/api/perps/v1/perps/basket/candles?interval=300&cursor=1800000000'

    const first = await worker.fetch(new Request(url), workerEnv(), executionContext())
    generation = 8
    const advanced = await worker.fetch(new Request(url), workerEnv(), executionContext())
    const subsequent = await worker.fetch(new Request(url), workerEnv(), executionContext())

    expect(first.headers.get('X-Plether-Edge-Cache')).toBe('MISS')
    expect(advanced.headers.get('X-Plether-Edge-Cache')).toBe('MISS')
    expect(subsequent.headers.get('X-Plether-Edge-Cache')).toBe('HIT')
    await expect(advanced.json()).resolves.toMatchObject({
      data: { datasetGeneration: 8 },
    })
    await expect(subsequent.json()).resolves.toMatchObject({
      data: { datasetGeneration: 8 },
    })
    expect(fetchMock).toHaveBeenCalledTimes(5)
    const probeHeaders = fetchMock.mock.calls[0]?.[1]?.headers as Headers
    expect(probeHeaders.get('Cache-Control')).toBe('no-store')
  })

  it('makes old-chain volume cache entries unreachable with the same router address', async () => {
    vi.spyOn(Date, 'now').mockReturnValue(1_800_000_000_000)
    let volumeChainId = 421_614
    const identity = () => candleIdentity(7, { volumeChainId })
    const fetchMock = vi.fn(async (input: unknown) => isCurrentCandleFetch(input)
      ? currentCandleResponse(identity())
      : candlePageResponse(identity()))
    vi.stubGlobal('fetch', fetchMock)
    const url =
      'https://app.example/api/perps/v1/perps/basket/candles?interval=300&cursor=1800000000'

    const first = await worker.fetch(new Request(url), workerEnv(), executionContext())
    volumeChainId = 1
    const replaced = await worker.fetch(new Request(url), workerEnv(), executionContext())
    const subsequent = await worker.fetch(new Request(url), workerEnv(), executionContext())

    expect(first.headers.get('X-Plether-Edge-Cache')).toBe('MISS')
    expect(replaced.headers.get('X-Plether-Edge-Cache')).toBe('MISS')
    expect(subsequent.headers.get('X-Plether-Edge-Cache')).toBe('HIT')
    await expect(replaced.json()).resolves.toMatchObject({
      data: { datasetGeneration: 7, volumeChainId },
    })
    expect(fetchMock).toHaveBeenCalledTimes(5)
  })

  it('keeps a closed page on the active TTL until current-router volume finalizes it', async () => {
    vi.spyOn(Date, 'now').mockReturnValue(1_800_000_000_000)
    let volumeFinalizedThrough = 1_799_970_000
    const identity = () => candleIdentity(7, { volumeFinalizedThrough })
    const fetchMock = vi.fn(async (input: unknown) => isCurrentCandleFetch(input)
      ? currentCandleResponse(identity())
      : candleResponse(identity(), {
          cursor: 1_800_000_000,
          candles: [{
            timestamp: 1_799_999_700,
            volumeComplete: volumeFinalizedThrough >= 1_800_000_000,
          }],
        }))
    vi.stubGlobal('fetch', fetchMock)
    const url =
      'https://app.example/api/perps/v1/perps/basket/candles?interval=300&cursor=1800000000'

    const unfinished = await worker.fetch(
      new Request(url),
      workerEnv(),
      executionContext(),
    )
    volumeFinalizedThrough = 1_800_000_000
    const finalized = await worker.fetch(
      new Request(url),
      workerEnv(),
      executionContext(),
    )
    const finalizedHit = await worker.fetch(
      new Request(url),
      workerEnv(),
      executionContext(),
    )

    expect(unfinished.headers.get('Cache-Control')).toContain('s-maxage=2')
    expect(finalized.headers.get('X-Plether-Edge-Cache')).toBe('MISS')
    expect(finalized.headers.get('Cache-Control')).toContain('s-maxage=300')
    expect(finalizedHit.headers.get('X-Plether-Edge-Cache')).toBe('HIT')
    await expect(finalized.json()).resolves.toMatchObject({
      data: { candles: [{ volumeComplete: true }] },
    })
    expect(fetchMock.mock.calls.filter(([input]) => !isCurrentCandleFetch(input)))
      .toHaveLength(2)
  })

  it('retains an immutable pre-volume page while the current-router watermark advances', async () => {
    vi.spyOn(Date, 'now').mockReturnValue(1_800_000_000_000)
    let volumeCoverageEnd = 1_800_300_000
    let volumeFinalizedThrough = 1_800_000_000
    const identity = () => candleIdentity(7, {
      coverageStart: 1_799_400_000,
      volumeCoverageStart: 1_800_000_000,
      volumeCoverageEnd,
      volumeFinalizedThrough,
    })
    const fetchMock = vi.fn(async (input: unknown) => isCurrentCandleFetch(input)
      ? currentCandleResponse(identity())
      : candleResponse(identity(), {
          cursor: 1_799_850_000,
          candles: [{ timestamp: 1_799_849_700, volumeComplete: false }],
        }))
    vi.stubGlobal('fetch', fetchMock)
    const url =
      'https://app.example/api/perps/v1/perps/basket/candles?interval=300&cursor=1799850000'

    const seeded = await worker.fetch(new Request(url), workerEnv(), executionContext())
    volumeCoverageEnd = 1_800_600_000
    volumeFinalizedThrough = 1_800_300_000
    const hit = await worker.fetch(new Request(url), workerEnv(), executionContext())

    expect(seeded.headers.get('Cache-Control')).toContain('s-maxage=300')
    expect(hit.headers.get('X-Plether-Edge-Cache')).toBe('HIT')
    expect(fetchMock.mock.calls.filter(([input]) => !isCurrentCandleFetch(input)))
      .toHaveLength(1)
  })

  it('long-caches a finalized page that straddles the current-router coverage start', async () => {
    vi.spyOn(Date, 'now').mockReturnValue(1_800_000_000_000)
    const identity = candleIdentity(7, {
      volumeCoverageStart: 1_799_900_100,
      volumeCoverageEnd: 1_800_000_000,
      volumeFinalizedThrough: 1_800_000_000,
    })
    const fetchMock = vi.fn(async (input: unknown) => isCurrentCandleFetch(input)
      ? currentCandleResponse(identity)
      : candlePageResponse(identity))
    vi.stubGlobal('fetch', fetchMock)
    const url =
      'https://app.example/api/perps/v1/perps/basket/candles?interval=300&cursor=1800000000'

    const seeded = await worker.fetch(new Request(url), workerEnv(), executionContext())
    const hit = await worker.fetch(new Request(url), workerEnv(), executionContext())

    expect(seeded.headers.get('Cache-Control')).toContain('s-maxage=300')
    expect(hit.headers.get('X-Plether-Edge-Cache')).toBe('HIT')
    expect(fetchMock.mock.calls.filter(([input]) => !isCurrentCandleFetch(input)))
      .toHaveLength(1)
  })

  it('caches against the shared later published price target boundary', async () => {
    vi.spyOn(Date, 'now').mockReturnValue(1_800_000_000_000)
    // Physical storage may begin earlier; both public endpoints intentionally
    // expose this latest published boundary so their page-state proofs agree.
    const publishedIdentity = candleIdentity(7, {
      coverageStart: 1_799_900_100,
    })
    const fetchMock = vi.fn(async (input: unknown) => isCurrentCandleFetch(input)
      ? currentCandleResponse(publishedIdentity)
      : candlePageResponse(publishedIdentity))
    vi.stubGlobal('fetch', fetchMock)
    const url =
      'https://app.example/api/perps/v1/perps/basket/candles?interval=300&cursor=1800000000'

    const seeded = await worker.fetch(new Request(url), workerEnv(), executionContext())
    const hit = await worker.fetch(new Request(url), workerEnv(), executionContext())

    expect(seeded.headers.get('X-Plether-Edge-Cache')).toBe('MISS')
    expect(hit.headers.get('X-Plether-Edge-Cache')).toBe('HIT')
    expect(fetchMock.mock.calls.filter(([input]) => !isCurrentCandleFetch(input)))
      .toHaveLength(1)
  })

  it('keeps a wall-clock-closed terminal price page active until price catches up', async () => {
    vi.spyOn(Date, 'now').mockReturnValue(1_800_000_000_000)
    let coverageEnd = 1_799_970_000
    let finalizedThrough = 1_799_970_000
    let volumeCoverageEnd = 1_799_970_000
    let volumeFinalizedThrough = 1_799_970_000
    const identity = () => candleIdentity(7, {
      coverageEnd,
      finalizedThrough,
      volumeCoverageEnd,
      volumeFinalizedThrough,
    })
    const fetchMock = vi.fn(async (input: unknown) => isCurrentCandleFetch(input)
      ? currentCandleResponse(identity())
      : candlePageResponse(identity()))
    vi.stubGlobal('fetch', fetchMock)
    const url =
      'https://app.example/api/perps/v1/perps/basket/candles?interval=300&cursor=1800000000'

    const terminal = await worker.fetch(new Request(url), workerEnv(), executionContext())
    coverageEnd = 1_800_000_000
    finalizedThrough = 1_800_000_000
    volumeCoverageEnd = 1_800_000_000
    volumeFinalizedThrough = 1_800_000_000
    const completed = await worker.fetch(new Request(url), workerEnv(), executionContext())
    const hit = await worker.fetch(new Request(url), workerEnv(), executionContext())

    expect(terminal.headers.get('Cache-Control')).toContain('s-maxage=2')
    expect(completed.headers.get('X-Plether-Edge-Cache')).toBe('MISS')
    expect(completed.headers.get('Cache-Control')).toContain('s-maxage=300')
    expect(hit.headers.get('X-Plether-Edge-Cache')).toBe('HIT')
    expect(fetchMock.mock.calls.filter(([input]) => !isCurrentCandleFetch(input)))
      .toHaveLength(2)
  })

  it('makes an active-page cache entry unreachable when the page becomes closed', async () => {
    const now = vi.spyOn(Date, 'now').mockReturnValue(1_800_000_100_000)
    let coverageEnd = 1_800_000_000
    let finalizedThrough = 1_800_000_000
    let volumeCoverageEnd = 1_800_000_000
    let volumeFinalizedThrough = 1_800_000_000
    const identity = () => candleIdentity(7, {
      coverageEnd,
      finalizedThrough,
      volumeCoverageEnd,
      volumeFinalizedThrough,
    })
    const fetchMock = vi.fn(async (input: unknown) => isCurrentCandleFetch(input)
      ? currentCandleResponse(identity())
      : candlePageResponse(identity()))
    vi.stubGlobal('fetch', fetchMock)
    const url =
      'https://app.example/api/perps/v1/perps/basket/candles?interval=300&cursor=1800150000'

    const active = await worker.fetch(new Request(url), workerEnv(), executionContext())
    now.mockReturnValue(1_800_150_000_000)
    coverageEnd = 1_800_150_000
    finalizedThrough = 1_800_150_000
    volumeCoverageEnd = 1_800_150_000
    volumeFinalizedThrough = 1_800_150_000
    const newlyClosed = await worker.fetch(new Request(url), workerEnv(), executionContext())
    const closedHit = await worker.fetch(new Request(url), workerEnv(), executionContext())

    expect(active.headers.get('X-Plether-Edge-Cache')).toBe('MISS')
    expect(active.headers.get('Cache-Control')).toContain('s-maxage=2')
    expect(newlyClosed.headers.get('X-Plether-Edge-Cache')).toBe('MISS')
    expect(newlyClosed.headers.get('Cache-Control')).toContain('s-maxage=300')
    expect(closedHit.headers.get('X-Plether-Edge-Cache')).toBe('HIT')
    expect(fetchMock.mock.calls.filter(([input]) => !isCurrentCandleFetch(input))).toHaveLength(2)
  })

  it('does not serve a cached generation when the authoritative identity probe fails', async () => {
    vi.spyOn(Date, 'now').mockReturnValue(1_800_000_000_000)
    const cache = new MemoryCache()
    vi.stubGlobal('caches', { default: cache })
    let probeCount = 0
    let pageCount = 0
    const fetchMock = vi.fn(async (input: unknown) => {
      if (isCurrentCandleFetch(input)) {
        probeCount += 1
        return probeCount === 1
          ? currentCandleResponse(candleIdentity(7))
          : new Response('unavailable', { status: 503 })
      }
      pageCount += 1
      return candlePageResponse(candleIdentity(pageCount === 1 ? 7 : 8))
    })
    vi.stubGlobal('fetch', fetchMock)
    const url =
      'https://app.example/api/perps/v1/perps/basket/candles?interval=300&cursor=1800000000'

    const seeded = await worker.fetch(new Request(url), workerEnv(), executionContext())
    const uncached = await worker.fetch(new Request(url), workerEnv(), executionContext())

    expect(seeded.headers.get('X-Plether-Edge-Cache')).toBe('MISS')
    expect(uncached.headers.get('X-Plether-Edge-Cache')).toBeNull()
    expect(uncached.headers.get('Cache-Control')).toBe('no-store')
    await expect(uncached.json()).resolves.toMatchObject({
      data: { datasetGeneration: 8 },
    })
    expect(cache.put).toHaveBeenCalledTimes(1)
    expect(fetchMock).toHaveBeenCalledTimes(4)
    expect(fetchMock.mock.calls.every(([, init]) =>
      (init as RequestInit | undefined)?.redirect === 'manual'
    )).toBe(true)
  })

  it('returns but never caches a page whose identity races the probe', async () => {
    vi.spyOn(Date, 'now').mockReturnValue(1_800_000_000_000)
    const cache = new MemoryCache()
    vi.stubGlobal('caches', { default: cache })
    const fetchMock = vi.fn(async (input: unknown) => isCurrentCandleFetch(input)
      ? currentCandleResponse(candleIdentity(7))
      : candlePageResponse(candleIdentity(8)))
    vi.stubGlobal('fetch', fetchMock)
    const url =
      'https://app.example/api/perps/v1/perps/basket/candles?interval=300&cursor=1800000000'

    const first = await worker.fetch(new Request(url), workerEnv(), executionContext())
    const second = await worker.fetch(new Request(url), workerEnv(), executionContext())

    expect(first.headers.get('X-Plether-Edge-Cache')).toBeNull()
    expect(second.headers.get('X-Plether-Edge-Cache')).toBeNull()
    expect(first.headers.get('Cache-Control')).toBe('no-store')
    expect(second.headers.get('Cache-Control')).toBe('no-store')
    await expect(second.json()).resolves.toMatchObject({
      data: { datasetGeneration: 8 },
    })
    expect(cache.put).not.toHaveBeenCalled()
    expect(fetchMock).toHaveBeenCalledTimes(4)
  })

  it.each([
    ['/api/perps/v1/aa/pimlico', '/api/aa/pimlico'],
    ['/api/perps/v1/aa/rpc', '/api/aa/rpc'],
  ])('authenticates only exact AA route %s', async (path, backendPath) => {
    const fetchMock = vi.fn(async () => jsonResponse())
    vi.stubGlobal('fetch', fetchMock)
    const request = new Request(`https://app.example${path}`, {
      method: 'POST',
      headers: { 'X-Plether-AA-Proxy-Token': 'browser-spoof' },
      body: '{}',
    })

    const response = await worker.fetch(
      request,
      workerEnv({ AA_PROXY_ORIGIN_TOKEN: 'trusted-origin-token' }),
      executionContext()
    )
    const forwardedUrl = fetchMock.mock.calls[0]?.[0] as URL
    const forwardedHeaders = fetchMock.mock.calls[0]?.[1]?.headers as Headers

    expect(response.headers.get('X-Plether-Edge-Cache')).toBeNull()
    expect(forwardedUrl.pathname).toBe(backendPath)
    expect(forwardedHeaders.get('X-Plether-AA-Proxy-Token'))
      .toBe('trusted-origin-token')
  })

  it.each([
    '/api/perps/v1/aa/pimlico',
    '/api/perps/v1/aa/rpc',
  ])('never follows an authenticated origin redirect for %s', async (path) => {
    const fetchMock = vi.fn(async () => new Response(null, {
      status: 302,
      headers: { Location: 'https://attacker.example/collect' },
    }))
    vi.stubGlobal('fetch', fetchMock)

    const response = await worker.fetch(new Request(
      `https://app.example${path}`,
      {
        method: 'POST',
        headers: { 'X-Plether-AA-Proxy-Token': 'browser-spoof' },
        body: '{}',
      }
    ), workerEnv({ AA_PROXY_ORIGIN_TOKEN: 'trusted-origin-token' }), executionContext())

    expect(response.status).toBe(302)
    expect(fetchMock).toHaveBeenCalledOnce()
    const [forwardedUrl, init] = fetchMock.mock.calls[0] as unknown as [URL, RequestInit]
    expect(forwardedUrl.origin).toBe('https://backend.example')
    expect(init.redirect).toBe('manual')
    expect((init.headers as Headers).get('X-Plether-AA-Proxy-Token'))
      .toBe('trusted-origin-token')
  })

  it.each([
    '/api/perps/v1/aa/pimlico',
    '/api/perps/v1/aa/rpc',
  ])('fails closed without origin authentication for %s', async (path) => {
    const fetchMock = vi.fn(async () => jsonResponse())
    vi.stubGlobal('fetch', fetchMock)

    const response = await worker.fetch(new Request(
      `https://app.example${path}`,
      { method: 'POST', body: '{}' }
    ), workerEnv(), executionContext())

    expect(response.status).toBe(502)
    expect(fetchMock).not.toHaveBeenCalled()
  })

  it('strips a spoofed AA token from non-AA perps requests', async () => {
    const fetchMock = vi.fn(async () => jsonResponse())
    vi.stubGlobal('fetch', fetchMock)

    await worker.fetch(new Request(
      'https://app.example/api/perps/v1/perps/market/stats',
      { headers: { 'X-Plether-AA-Proxy-Token': 'browser-spoof' } }
    ), workerEnv({ AA_PROXY_ORIGIN_TOKEN: 'trusted-origin-token' }), executionContext())

    const forwardedHeaders = fetchMock.mock.calls[0]?.[1]?.headers as Headers
    expect(forwardedHeaders.has('X-Plether-AA-Proxy-Token')).toBe(false)
  })

  it('does not authenticate near-miss AA paths', async () => {
    const fetchMock = vi.fn(async () => jsonResponse())
    vi.stubGlobal('fetch', fetchMock)

    await worker.fetch(new Request(
      'https://app.example/api/perps/v1/aa/rpc/extra',
      { headers: { 'X-Plether-AA-Proxy-Token': 'browser-spoof' } }
    ), workerEnv({ AA_PROXY_ORIGIN_TOKEN: 'trusted-origin-token' }), executionContext())

    const forwardedHeaders = fetchMock.mock.calls[0]?.[1]?.headers as Headers
    expect(forwardedHeaders.has('X-Plether-AA-Proxy-Token')).toBe(false)
  })

  it('forces the deployment manifest asset to no-store', async () => {
    const assetFetch = vi.fn(async () => new Response('{}', {
      headers: {
        'Cache-Control': 'public, max-age=3600',
        'X-Plether-Edge-Cache': 'HIT',
        'X-Plether-Edge-Cached-At': '123',
      },
    }))

    const response = await worker.fetch(
      new Request('https://app.example/perps-aa-manifest.json'),
      workerEnv({ ASSETS: { fetch: assetFetch } }),
      executionContext()
    )

    expect(response.headers.get('Cache-Control')).toBe('no-store')
    expect(response.headers.has('X-Plether-Edge-Cache')).toBe(false)
    expect(response.headers.has('X-Plether-Edge-Cached-At')).toBe(false)
    expect(assetFetch).toHaveBeenCalledOnce()
  })
})
