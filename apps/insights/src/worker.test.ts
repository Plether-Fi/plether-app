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

  it.each([
    ['/api/insights/v1/status', 30],
    ['/api/insights/v1/competitions/current', 60],
    ['/api/insights/v1/competitions/summer-2026/leaderboard?limit=50', 15],
    ['/api/insights/v1/competitions/summer-2026/wallets/0x1234', 15],
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

  it.each([
    ['POST', '/api/insights/v1/status'],
    ['GET', '/api/insights/v1/competitions/summer-2026/activity'],
  ])('does not enable edge caching for %s %s', async (method, path) => {
    const fetchMock = vi.fn().mockResolvedValue(Response.json({ ok: true }))
    vi.stubGlobal('fetch', fetchMock)

    await worker.fetch(new Request(`https://insights.plether.com${path}`, { method }), {
      INSIGHTS_BACKEND_URL: 'https://backend.example.com',
      ASSETS: assets,
    })

    expect(proxiedFetchOptions(fetchMock).cf).toBeUndefined()
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
