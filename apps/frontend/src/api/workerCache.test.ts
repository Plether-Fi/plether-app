import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest'

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

function jsonResponse(): Response {
  return new Response(JSON.stringify({ data: { ok: true }, meta: {} }), {
    status: 200,
    headers: { 'Content-Type': 'application/json' },
  })
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
      'range=7d&interval=300',
      'range=30d&interval=3600',
      'range=1y&interval=86400',
      'range=24h&interval=3600&includeComponents=true',
    ]) {
      expect(getPublicPerpsCachePolicy(new Request(
        `https://app.example/api/perps/v1/perps/basket/history?${query}`
      ))).toMatchObject({ freshSeconds: 60 })
    }
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
    expect(getPublicPerpsCachePolicy(new Request(
      'https://app.example/api/perps/v1/aa/pimlico'
    ))).toBeUndefined()
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

  it('keeps AA origin authentication isolated from the public cache path', async () => {
    const fetchMock = vi.fn(async () => jsonResponse())
    vi.stubGlobal('fetch', fetchMock)
    const request = new Request('https://app.example/api/perps/v1/aa/pimlico', {
      method: 'POST',
      headers: { 'X-Plether-AA-Proxy-Token': 'browser-spoof' },
      body: '{}',
    })

    const response = await worker.fetch(
      request,
      workerEnv({ AA_PROXY_ORIGIN_TOKEN: 'trusted-origin-token' }),
      executionContext()
    )
    const forwardedHeaders = fetchMock.mock.calls[0]?.[1]?.headers as Headers

    expect(response.headers.get('X-Plether-Edge-Cache')).toBeNull()
    expect(forwardedHeaders.get('X-Plether-AA-Proxy-Token')).toBe('trusted-origin-token')
  })
})
