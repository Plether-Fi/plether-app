// @vitest-environment node

import { afterEach, describe, expect, it, vi } from 'vitest'

// @ts-expect-error -- the Pages Worker is deployed as a standalone JavaScript entry point.
import worker from '../public/_worker.js'

const apiRequest = () => new Request('https://insights.plether.com/api/insights/v1/status?fresh=true')
const assets = { fetch: vi.fn().mockResolvedValue(new Response('asset')) }

type ProxyFetchOptions = {
  cache?: RequestCache
  cf?: {
    cacheEverything: boolean
    cacheTtlByStatus: Record<string, number>
  }
  headers?: Headers
  redirect?: RequestRedirect
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

  it('does not cache current competition metadata across registration boundaries', async () => {
    const fetchMock = vi.fn().mockResolvedValue(Response.json({ ok: true }))
    vi.stubGlobal('fetch', fetchMock)

    const response = await worker.fetch(new Request('https://insights.plether.com/api/insights/v1/competitions/current'), {
      INSIGHTS_BACKEND_URL: 'https://backend.example.com',
      ASSETS: assets,
    })

    expect(proxiedFetchOptions(fetchMock).cf).toBeUndefined()
    expect(proxiedFetchOptions(fetchMock).cache).toBe('no-store')
    expect(proxiedFetchOptions(fetchMock).headers?.get('cache-control')).toBe(
      'no-store',
    )
    expect(response.headers.get('cache-control')).toBe('no-store')
  })

  it('uses the same cache policy for HEAD reads', async () => {
    const fetchMock = vi.fn().mockResolvedValue(new Response(null))
    vi.stubGlobal('fetch', fetchMock)

    await worker.fetch(
      new Request('https://insights.plether.com/api/insights/v1/status', { method: 'HEAD' }),
      {
        INSIGHTS_BACKEND_URL: 'https://backend.example.com',
        INSIGHTS_REGISTRATION_PUBLIC_ORIGIN: 'https://insights.plether.com',
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
    expect(proxiedFetchOptions(fetchMock).cache).toBe('no-store')
  })

  it.each<[string, string, boolean]>([
    ['Cookie', '__Host-plether_registration=opaque-session', false],
    ['Authorization', 'Bearer browser-token', false],
    ['Range', 'bytes=0-99', true],
  ])(
    'bypasses public edge caching when %s is present',
    async (headerName, headerValue, forwarded) => {
      const fetchMock = vi.fn().mockResolvedValue(Response.json({ ok: true }))
      vi.stubGlobal('fetch', fetchMock)

      await worker.fetch(
        new Request(
          'https://insights.plether.com/api/insights/v1/competitions/current',
          { headers: { [headerName]: headerValue } },
        ),
        {
          INSIGHTS_BACKEND_URL: 'https://backend.example.com',
          ASSETS: assets,
        },
      )

      const options = proxiedFetchOptions(fetchMock)
      expect(options.cf).toBeUndefined()
      expect(options.cache).toBe('no-store')
      expect(options.headers?.has(headerName)).toBe(forwarded)
    },
  )

  it('fails closed when the registration origin secret is missing', async () => {
    const fetchMock = vi.fn()
    vi.stubGlobal('fetch', fetchMock)

    const response = await worker.fetch(
      new Request(
        'https://insights.plether.com/api/insights/v1/competitions/testnet-trading-2026-09/registrations/session',
        {
          method: 'POST',
          headers: { Origin: 'https://insights.plether.com' },
        },
      ),
      {
        INSIGHTS_BACKEND_URL: 'https://backend.example.com',
        INSIGHTS_REGISTRATION_PUBLIC_ORIGIN: 'https://insights.plether.com',
        ASSETS: assets,
      },
    )

    expect(response.status).toBe(502)
    expect(response.headers.get('cache-control')).toBe('private, no-store, max-age=0')
    await expect(response.json()).resolves.toEqual({
      error: {
        code: 'registration_proxy_not_configured',
        message: 'Insights registration proxy is not configured.',
      },
    })
    expect(fetchMock).not.toHaveBeenCalled()
  })

  it('authenticates registration requests at the edge and forwards session context', async () => {
    const fetchMock = vi.fn().mockResolvedValue(Response.json({ ok: true }))
    vi.stubGlobal('fetch', fetchMock)

    await worker.fetch(
      new Request(
        'https://insights.plether.com/api/insights/v1/competitions/testnet-trading-2026-09/registrations/session',
        {
          method: 'POST',
          headers: {
            Cookie: '__Host-plether_registration=opaque-session',
            Origin: 'https://insights.plether.com',
            'X-Registration-CSRF': 'csrf-value',
            'X-Plether-Registration-Origin': 'browser-supplied-value',
            'X-Forwarded-Host': 'attacker.example.com',
            'X-Forwarded-Proto': 'http',
          },
        },
      ),
      {
        INSIGHTS_BACKEND_URL: 'https://backend.example.com',
        INSIGHTS_REGISTRATION_PUBLIC_ORIGIN: 'https://insights.plether.com',
        INSIGHTS_REGISTRATION_ORIGIN_TOKEN: 'trusted-pages-secret',
        ASSETS: assets,
      },
    )

    const options = proxiedFetchOptions(fetchMock)
    expect(options.cf).toBeUndefined()
    expect(options.cache).toBe('no-store')
    expect(options.redirect).toBe('manual')
    expect(options.headers?.get('cache-control')).toBe('no-store')
    expect(options.headers?.get('pragma')).toBe('no-cache')
    expect(options.headers?.get('cookie')).toBe(
      '__Host-plether_registration=opaque-session',
    )
    expect(options.headers?.get('origin')).toBe('https://insights.plether.com')
    expect(options.headers?.get('x-registration-csrf')).toBe('csrf-value')
    expect(options.headers?.get('x-plether-registration-origin')).toBe(
      'trusted-pages-secret',
    )
    expect(options.headers?.get('x-forwarded-host')).toBe('insights.plether.com')
    expect(options.headers?.get('x-forwarded-proto')).toBe('https')
  })

  it.each([
    ['a sibling Plether origin', { Origin: 'https://app.sepolia.plether.com' }],
    ['an opaque origin', { Origin: 'null' }],
    ['a missing mutation origin', { 'Sec-Fetch-Site': 'same-origin' }],
  ])('rejects %s before injecting the registration credential', async (_, headers) => {
    const fetchMock = vi.fn()
    vi.stubGlobal('fetch', fetchMock)

    const response = await worker.fetch(
      new Request(
        'https://insights.plether.com/api/insights/v1/competitions/testnet-trading-2026-09/registrations/x/authorize',
        { method: 'POST', headers },
      ),
      {
        INSIGHTS_BACKEND_URL: 'https://backend.example.com',
        INSIGHTS_REGISTRATION_PUBLIC_ORIGIN: 'https://insights.plether.com',
        INSIGHTS_REGISTRATION_ORIGIN_TOKEN: 'trusted-pages-secret',
        ASSETS: assets,
      },
    )

    expect(response.status).toBe(403)
    expect(response.headers.get('cache-control')).toBe('private, no-store, max-age=0')
    expect(fetchMock).not.toHaveBeenCalled()
  })

  it.each([
    ['Fetch Metadata', { 'Sec-Fetch-Site': 'same-origin' }],
    [
      'a same-origin Referer',
      { Referer: 'https://insights.plether.com/competitions/testnet-trading-2026-09/register' },
    ],
  ])('accepts a same-origin registration GET proven by %s', async (_, headers) => {
    const fetchMock = vi.fn().mockResolvedValue(Response.json({ ok: true }))
    vi.stubGlobal('fetch', fetchMock)

    const response = await worker.fetch(
      new Request(
        'https://insights.plether.com/api/insights/v1/competitions/testnet-trading-2026-09/registrations/session',
        { headers },
      ),
      {
        INSIGHTS_BACKEND_URL: 'https://backend.example.com',
        INSIGHTS_REGISTRATION_PUBLIC_ORIGIN: 'https://insights.plether.com',
        INSIGHTS_REGISTRATION_ORIGIN_TOKEN: 'trusted-pages-secret',
        ASSETS: assets,
      },
    )

    expect(response.status).toBe(200)
    expect(proxiedFetchOptions(fetchMock).headers?.get('origin')).toBe(
      'https://insights.plether.com',
    )
  })

  it.each([
    ['cross-site Fetch Metadata', { 'Sec-Fetch-Site': 'cross-site' }],
    [
      'a sibling-site Referer',
      { Referer: 'https://app.sepolia.plether.com/competition' },
    ],
    ['no browser provenance', {}],
  ])('rejects an Origin-less registration GET with %s', async (_, headers) => {
    const fetchMock = vi.fn()
    vi.stubGlobal('fetch', fetchMock)

    const response = await worker.fetch(
      new Request(
        'https://insights.plether.com/api/insights/v1/competitions/testnet-trading-2026-09/registrations/session',
        { headers },
      ),
      {
        INSIGHTS_BACKEND_URL: 'https://backend.example.com',
        INSIGHTS_REGISTRATION_PUBLIC_ORIGIN: 'https://insights.plether.com',
        INSIGHTS_REGISTRATION_ORIGIN_TOKEN: 'trusted-pages-secret',
        ASSETS: assets,
      },
    )

    expect(response.status).toBe(403)
    expect(fetchMock).not.toHaveBeenCalled()
  })

  it('preserves the fixed callback redirect and multiple cookies while forcing no-store', async () => {
    const backendHeaders = new Headers({
      'Cache-Control': 'public, max-age=300',
      Location:
        'https://insights.plether.com/competitions/testnet-trading-2026-09/register',
    })
    backendHeaders.append(
      'Set-Cookie',
      '__Host-plether_registration=rotated; Path=/; Secure; HttpOnly; SameSite=Lax',
    )
    backendHeaders.append(
      'Set-Cookie',
      '__Host-plether_oauth=cleared; Path=/; Secure; HttpOnly; SameSite=Lax; Max-Age=0',
    )
    const fetchMock = vi.fn().mockResolvedValue(
      new Response(null, {
        status: 303,
        headers: backendHeaders,
      }),
    )
    vi.stubGlobal('fetch', fetchMock)

    const response = await worker.fetch(
      new Request(
        'https://insights.plether.com/api/insights/v1/competitions/testnet-trading-2026-09/registrations/x/callback?code=provider-code&state=opaque',
        { headers: { Origin: 'https://x.com', 'Sec-Fetch-Site': 'cross-site' } },
      ),
      {
        INSIGHTS_BACKEND_URL: 'https://backend.example.com',
        INSIGHTS_REGISTRATION_PUBLIC_ORIGIN: 'https://insights.plether.com',
        INSIGHTS_REGISTRATION_ORIGIN_TOKEN: 'trusted-pages-secret',
        ASSETS: assets,
      },
    )

    expect(response.status).toBe(303)
    expect(response.headers.get('location')).toBe(
      'https://insights.plether.com/competitions/testnet-trading-2026-09/register',
    )
    expect(response.headers.get('set-cookie')).toContain(
      '__Host-plether_registration=rotated',
    )
    expect(response.headers.get('set-cookie')).toContain(
      '__Host-plether_oauth=cleared',
    )
    const responseCookies = (
      response.headers as Headers & { getSetCookie: () => string[] }
    ).getSetCookie()
    expect(responseCookies).toHaveLength(2)
    expect(responseCookies[0]).toContain('__Host-plether_registration=rotated')
    expect(responseCookies[1]).toContain('__Host-plether_oauth=cleared')
    expect(response.headers.get('cache-control')).toBe('private, no-store, max-age=0')
    expect(response.headers.get('pragma')).toBe('no-cache')
    expect(response.headers.get('referrer-policy')).toBe('no-referrer')
    expect(proxiedFetchOptions(fetchMock).headers?.get('origin')).toBe(
      'https://insights.plether.com',
    )
  })

  it.each([
    'https://attacker.example/register',
    'https://insights.plether.com/competitions/testnet-trading-2026-09/register?code=reflected',
    'https://insights.plether.com/competitions/another-slug/register',
  ])('rejects an unsafe registration callback redirect to %s', async (location) => {
    const fetchMock = vi.fn().mockResolvedValue(
      new Response(null, { status: 303, headers: { Location: location } }),
    )
    vi.stubGlobal('fetch', fetchMock)

    const response = await worker.fetch(
      new Request(
        'https://insights.plether.com/api/insights/v1/competitions/testnet-trading-2026-09/registrations/x/callback?code=provider-code&state=opaque',
      ),
      {
        INSIGHTS_BACKEND_URL: 'https://backend.example.com',
        INSIGHTS_REGISTRATION_PUBLIC_ORIGIN: 'https://insights.plether.com',
        INSIGHTS_REGISTRATION_ORIGIN_TOKEN: 'trusted-pages-secret',
        ASSETS: assets,
      },
    )

    expect(response.status).toBe(502)
    expect(response.headers.get('cache-control')).toBe('private, no-store, max-age=0')
    await expect(response.json()).resolves.toEqual({
      error: {
        code: 'registration_redirect_invalid',
        message: 'Insights registration backend returned an invalid redirect.',
      },
    })
  })

  it('rejects registration on Pages preview hosts before exposing the origin credential', async () => {
    const fetchMock = vi.fn()
    vi.stubGlobal('fetch', fetchMock)

    const response = await worker.fetch(
      new Request(
        'https://deployment.plether-insights.pages.dev/api/insights/v1/competitions/testnet-trading-2026-09/registrations/session',
        { method: 'POST' },
      ),
      {
        INSIGHTS_BACKEND_URL: 'https://backend.example.com',
        INSIGHTS_REGISTRATION_PUBLIC_ORIGIN: 'https://insights.plether.com',
        INSIGHTS_REGISTRATION_ORIGIN_TOKEN: 'trusted-pages-secret',
        ASSETS: assets,
      },
    )

    expect(response.status).toBe(403)
    expect(response.headers.get('cache-control')).toBe('private, no-store, max-age=0')
    await expect(response.json()).resolves.toEqual({
      error: {
        code: 'registration_origin_not_allowed',
        message: 'Registration is not available on this origin.',
      },
    })
    expect(fetchMock).not.toHaveBeenCalled()
  })

  it('never forwards a browser-supplied registration origin credential elsewhere', async () => {
    const fetchMock = vi.fn().mockResolvedValue(Response.json({ ok: true }))
    vi.stubGlobal('fetch', fetchMock)

    await worker.fetch(
      new Request('https://insights.plether.com/api/insights/v1/status', {
        headers: { 'X-Plether-Registration-Origin': 'browser-supplied-value' },
      }),
      {
        INSIGHTS_BACKEND_URL: 'https://backend.example.com',
        INSIGHTS_REGISTRATION_ORIGIN_TOKEN: 'trusted-pages-secret',
        ASSETS: assets,
      },
    )

    expect(
      proxiedFetchOptions(fetchMock).headers?.has('x-plether-registration-origin'),
    ).toBe(false)
  })

  it('applies immutable caching and security headers to hashed assets', async () => {
    const response = await worker.fetch(
      new Request('https://insights.plether.com/assets/app-HASH.js'),
      { ASSETS: assets },
    )

    expect(response.headers.get('cache-control')).toBe('public, max-age=31536000, immutable')
    expect(response.headers.get('x-frame-options')).toBe('DENY')
    const csp = response.headers.get('content-security-policy') ?? ''
    expect(csp).toContain('https://challenges.cloudflare.com')
    expect(csp).toContain('wss://relay.walletconnect.com')
    expect(csp).not.toContain('wasm-unsafe-eval')
    expect(csp).not.toContain('https://*.')
  })
})
