import { afterEach, describe, expect, it, vi } from 'vitest'

// @ts-expect-error -- the Pages Worker is deployed as a standalone JavaScript entry point.
import worker from '../public/_worker.js'

const apiRequest = () => new Request('https://insights.plether.com/api/insights/v1/status?fresh=true')
const assets = { fetch: vi.fn().mockResolvedValue(new Response('asset')) }

type ProxyFetchOptions = {
  cf?: {
    cacheEverything: boolean
    cacheTtlByStatus: Record<string, number>
  }
}

function proxiedFetchOptions(fetchMock: ReturnType<typeof vi.fn>): ProxyFetchOptions {
  return fetchMock.mock.calls[0]?.[1] as ProxyFetchOptions
}

afterEach(() => {
  vi.unstubAllGlobals()
  vi.clearAllMocks()
})

describe('Cloudflare Pages Worker', () => {
  it('returns a controlled error when the backend binding is missing', async () => {
    const fetchMock = vi.fn()
    vi.stubGlobal('fetch', fetchMock)

    const response = await worker.fetch(apiRequest(), { ASSETS: assets })

    expect(response.status).toBe(502)
    await expect(response.json()).resolves.toEqual({
      error: {
        code: 'backend_not_configured',
        message: 'Insights backend is not configured.',
      },
    })
    expect(response.headers.get('content-security-policy')).toContain("default-src 'self'")
    expect(response.headers.get('cache-control')).toBe('no-store')
    expect(fetchMock).not.toHaveBeenCalled()
  })

  it.each([
    'http://internal-alb.example.com',
    'not a URL',
    'https://api.example.com/base-path',
    'https://user:password@api.example.com',
  ])('fails closed for invalid backend binding %s', async (configuredOrigin) => {
    const fetchMock = vi.fn()
    vi.stubGlobal('fetch', fetchMock)

    const response = await worker.fetch(apiRequest(), {
      INSIGHTS_BACKEND_URL: configuredOrigin,
      ASSETS: assets,
    })

    expect(response.status).toBe(502)
    await expect(response.json()).resolves.toEqual({
      error: {
        code: 'backend_configuration_invalid',
        message: 'Insights backend configuration is invalid.',
      },
    })
    expect(response.headers.get('cache-control')).toBe('no-store')
    expect(fetchMock).not.toHaveBeenCalled()
  })

  it('proxies only a valid HTTPS origin and preserves backend response metadata', async () => {
    const fetchMock = vi.fn().mockResolvedValue(
      Response.json(
        { healthy: true },
        { headers: { 'Cache-Control': 'no-store' } },
      ),
    )
    vi.stubGlobal('fetch', fetchMock)

    const response = await worker.fetch(apiRequest(), {
      INSIGHTS_BACKEND_URL: 'https://backend.example.com/',
      ASSETS: assets,
    })

    expect(fetchMock).toHaveBeenCalledOnce()
    expect(String(fetchMock.mock.calls[0]?.[0])).toBe(
      'https://backend.example.com/api/insights/v1/status?fresh=true',
    )
    expect(proxiedFetchOptions(fetchMock).cf).toEqual({
      cacheEverything: true,
      cacheTtlByStatus: {
        '200-299': 30,
        '300-599': -1,
      },
    })
    expect(response.headers.get('cache-control')).toBe('no-store')
    expect(response.headers.get('x-content-type-options')).toBe('nosniff')
    await expect(response.json()).resolves.toEqual({ healthy: true })
  })

  it('does not forward browser credentials to the public backend', async () => {
    const fetchMock = vi.fn().mockResolvedValue(Response.json({ ok: true }))
    vi.stubGlobal('fetch', fetchMock)

    await worker.fetch(
      new Request(
        'https://insights.plether.com/api/insights/v1/protocol/releases/current',
        {
          headers: {
            Accept: 'application/json',
            Authorization: 'Bearer secret-sentinel',
            Cookie: 'session=secret-sentinel',
            'Proxy-Authorization': 'Basic secret-sentinel',
            'Cf-Access-Jwt-Assertion': 'secret-sentinel',
          },
        },
      ),
      {
        INSIGHTS_BACKEND_URL: 'https://backend.example.com',
        ASSETS: assets,
      },
    )

    const proxiedHeaders = fetchMock.mock.calls[0]?.[1]?.headers as Headers
    expect(proxiedHeaders.get('accept')).toBe('application/json')
    expect(proxiedHeaders.get('authorization')).toBeNull()
    expect(proxiedHeaders.get('cookie')).toBeNull()
    expect(proxiedHeaders.get('proxy-authorization')).toBeNull()
    expect(proxiedHeaders.get('cf-access-jwt-assertion')).toBeNull()
  })

  it('sanitizes rejected upstream requests and marks them no-store', async () => {
    const fetchMock = vi.fn().mockRejectedValue(
      new Error('secret provider hostname and TLS diagnostics'),
    )
    vi.stubGlobal('fetch', fetchMock)

    const response = await worker.fetch(apiRequest(), {
      INSIGHTS_BACKEND_URL: 'https://backend.example.com',
      ASSETS: assets,
    })

    expect(response.status).toBe(502)
    expect(response.headers.get('cache-control')).toBe('no-store')
    expect(response.headers.get('x-content-type-options')).toBe('nosniff')
    await expect(response.json()).resolves.toEqual({
      error: {
        code: 'backend_unavailable',
        message: 'Insights backend is temporarily unavailable.',
      },
    })
  })

  it.each([
    ['/api/insights/v1/status', 30],
    ['/api/insights/v1/competitions/current', 60],
    ['/api/insights/v1/competitions/summer-2026/leaderboard?limit=50', 15],
    ['/api/insights/v1/competitions/summer-2026/wallets/0x1234', 15],
    ['/api/insights/v1/protocol/releases/current', 5],
    ['/api/insights/v1/protocol/releases/release-1/overview', 15],
    ['/api/insights/v1/protocol/releases/release-1/house-pool', 15],
    ['/api/insights/v1/protocol/releases/release-1/keepers?window=7d', 15],
    ['/api/insights/v1/protocol/releases/release-1/keepers/0xabc?window=30d', 15],
    ['/api/insights/v1/protocol/releases/release-1/wallets?window=24h', 15],
    ['/api/insights/v1/protocol/releases/release-1/wallets/0xabc?window=30d', 15],
    ['/api/insights/v1/protocol/releases/release-1/parameters', 15],
    ['/api/insights/v1/protocol/releases/release-1/orders/42', 15],
    ['/api/insights/v1/protocol/releases/release-1/tranches/senior', 15],
    ['/api/insights/v1/protocol/releases/release-1/transactions?actionType=liquidation', 10],
    ['/api/insights/v1/protocol/releases/release-1/transactions/0xabc', 15],
    ['/api/insights/v1/protocol/releases/release-1/tranches/senior/history', 30],
    ['/api/insights/v1/protocol/releases/release-1/parameter-changes?limit=200', 30],
  ])('edge-caches successful public reads for %s for %i seconds', async (path, cacheTtl) => {
    const fetchMock = vi.fn().mockResolvedValue(Response.json({ ok: true }))
    vi.stubGlobal('fetch', fetchMock)

    await worker.fetch(new Request(`https://insights.plether.com${path}`), {
      INSIGHTS_BACKEND_URL: 'https://backend.example.com',
      ASSETS: assets,
    })

    expect(proxiedFetchOptions(fetchMock).cf).toEqual({
      cacheEverything: true,
      cacheTtlByStatus: {
        '200-299': cacheTtl,
        '300-599': -1,
      },
    })
  })

  it('uses the same cache policy for HEAD reads', async () => {
    const fetchMock = vi.fn().mockResolvedValue(new Response(null))
    vi.stubGlobal('fetch', fetchMock)

    await worker.fetch(
      new Request('https://insights.plether.com/api/insights/v1/status', { method: 'HEAD' }),
      {
        INSIGHTS_BACKEND_URL: 'https://backend.example.com',
        ASSETS: assets,
      },
    )

    expect(proxiedFetchOptions(fetchMock).cf?.cacheTtlByStatus['200-299']).toBe(30)
  })

  it('marks a disabled explorer response as no-store at the edge', async () => {
    const fetchMock = vi.fn().mockResolvedValue(
      Response.json(
        { error: { code: 'NOT_FOUND', message: 'Protocol explorer is disabled' } },
        { status: 404 },
      ),
    )
    vi.stubGlobal('fetch', fetchMock)

    const response = await worker.fetch(
      new Request(
        'https://insights.plether.com/api/insights/v1/protocol/releases/release-1/overview',
      ),
      {
        INSIGHTS_BACKEND_URL: 'https://backend.example.com',
        ASSETS: assets,
      },
    )

    expect(response.status).toBe(404)
    expect(proxiedFetchOptions(fetchMock).cf?.cacheTtlByStatus['300-599']).toBe(-1)
    expect(response.headers.get('cache-control')).toBe('no-store')
  })

  it('overrides cacheable backend headers on non-2xx responses', async () => {
    const fetchMock = vi.fn().mockResolvedValue(
      Response.json(
        { error: { code: 'upstream_unavailable' } },
        {
          status: 503,
          headers: { 'Cache-Control': 'public, max-age=3600' },
        },
      ),
    )
    vi.stubGlobal('fetch', fetchMock)

    const response = await worker.fetch(apiRequest(), {
      INSIGHTS_BACKEND_URL: 'https://backend.example.com',
      ASSETS: assets,
    })

    expect(response.status).toBe(503)
    expect(response.headers.get('cache-control')).toBe('no-store')
  })

  it.each([
    ['POST', '/api/insights/v1/status'],
    ['POST', '/api/insights/v1/protocol/releases/release-1/transactions'],
    ['GET', '/api/insights/v1/competitions/summer-2026/activity'],
  ])('does not enable edge caching for %s %s', async (method, path) => {
    const fetchMock = vi.fn().mockResolvedValue(
      Response.json(
        { ok: true },
        { headers: { 'Cache-Control': 'public, max-age=3600' } },
      ),
    )
    vi.stubGlobal('fetch', fetchMock)

    const response = await worker.fetch(new Request(`https://insights.plether.com${path}`, { method }), {
      INSIGHTS_BACKEND_URL: 'https://backend.example.com',
      ASSETS: assets,
    })

    expect(proxiedFetchOptions(fetchMock).cf).toBeUndefined()
    if (method !== 'GET' && method !== 'HEAD') {
      expect(response.headers.get('cache-control')).toBe('no-store')
    }
  })

  it('applies immutable caching and security headers to hashed assets', async () => {
    const response = await worker.fetch(
      new Request('https://insights.plether.com/assets/app-HASH.js'),
      { ASSETS: assets },
    )

    expect(response.headers.get('cache-control')).toBe('public, max-age=31536000, immutable')
    expect(response.headers.get('x-frame-options')).toBe('DENY')
  })
})
