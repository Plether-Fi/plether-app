// @vitest-environment node

import { afterEach, describe, expect, it, vi } from 'vitest'

// @ts-expect-error -- the Pages Worker is deployed as a standalone JavaScript entry point.
import worker, { resetOgFontCache, setSvgToPngRenderer } from '../public/_worker.js'

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
  setSvgToPngRenderer(null)
  resetOgFontCache()
  vi.unstubAllGlobals()
  vi.clearAllMocks()
})

describe('Cloudflare Pages Worker', () => {
  it('serves the mutable Divisions publication without retaining browser caches', async () => {
    const response = await worker.fetch(new Request('https://insights.plether.com/divisions/current.json'), { ASSETS: assets })
    expect(response.headers.get('cache-control')).toBe('no-store')
    expect(response.status).toBe(200)
  })
  it('returns 404 when Pages serves the SPA for a missing Divisions publication', async () => {
    const htmlAssets = { fetch: vi.fn().mockResolvedValue(new Response('<html>SPA</html>', { headers: { 'Content-Type': 'text/html; charset=utf-8' } })) }
    const response = await worker.fetch(new Request('https://insights.plether.com/divisions/current.json'), { ASSETS: htmlAssets })
    expect(response.status).toBe(404)
    expect(response.headers.get('cache-control')).toBe('no-store')
    expect(htmlAssets.fetch).toHaveBeenCalledTimes(1)
  })
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
    ['/api/insights/v1/overview', 15],
    ['/api/insights/v1/execution/quality', 15],
    ['/api/insights/v1/execution/trades?limit=25', 15],
    ['/api/insights/v1/keepers/status', 15],
    ['/api/insights/v1/keepers/activity?limit=25', 15],
    [`/api/insights/v1/keepers/0x${'b'.repeat(40)}`, 15],
    ['/api/insights/v1/sponsored/status', 15],
    ['/api/insights/v1/sponsored/operations?limit=50', 15],
    ['/api/insights/v1/liquidity/settlements', 30],
    ['/api/insights/v1/liquidity/forensics', 30],
    ['/api/insights/v1/liquidity/epochs/498776', 30],
    ['/api/insights/v1/protocol/manifest', 60],
    ['/api/insights/v1/infra/status', 15],
    [`/api/insights/v1/wallets/0x${'a'.repeat(40)}/summary`, 15],
    ['/api/insights/v1/divisions/stats', 15],
    ['/api/insights/v1/divisions/traders', 15],
    ['/api/insights/v1/divisions/live', 15],
    ['/api/insights/v1/divisions/current', 15],
    ['/api/insights/v1/divisions/history?week=1791752400', 15],
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
    ['POST', '/api/insights/v1/overview'],
    ['GET', '/api/insights/v1/wallets/not-an-address/summary'],
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

      // The anonymous request must be cacheable, otherwise a missing bypass
      // guard would be indistinguishable from an endpoint with no cache TTL.
      await worker.fetch(apiRequest(), {
        INSIGHTS_BACKEND_URL: 'https://backend.example.com',
        ASSETS: assets,
      })
      expect(proxiedFetchOptions(fetchMock).cf?.cacheTtlByStatus['200-299']).toBe(30)
      expect(proxiedFetchOptions(fetchMock).cache).toBeUndefined()
      fetchMock.mockClear()

      await worker.fetch(
        new Request(
          apiRequest().url,
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

  it('proxies Divisions registration with the same origin protection', async () => {
    const fetchMock = vi.fn().mockResolvedValue(Response.json({ registration: {} }))
    vi.stubGlobal('fetch', fetchMock)
    const url = 'https://insights.plether.com/api/insights/v1/divisions/registrations/session'
    const env = {
      INSIGHTS_BACKEND_URL: 'https://backend.example.com',
      INSIGHTS_REGISTRATION_PUBLIC_ORIGIN: 'https://insights.plether.com',
      INSIGHTS_REGISTRATION_ORIGIN_TOKEN: 'trusted-pages-secret',
      ASSETS: assets,
    }
    const response = await worker.fetch(new Request(url, {
      method: 'POST', headers: { Origin: 'https://insights.plether.com' },
    }), env)
    expect(response.status).toBe(200)
    expect(response.headers.get('cache-control')).toBe('private, no-store, max-age=0')
    expect(proxiedFetchOptions(fetchMock).headers?.get('x-plether-registration-origin')).toBe('trusted-pages-secret')

    fetchMock.mockClear()
    const rejected = await worker.fetch(new Request(url, {
      method: 'POST', headers: { Origin: 'https://app.sepolia.plether.com' },
    }), env)
    expect(rejected.status).toBe(403)
    expect(fetchMock).not.toHaveBeenCalled()
  })

  it('permits only the canonical Discord callback redirect', async () => {
    const fetchMock = vi.fn().mockResolvedValue(new Response(null, {
      status: 303, headers: { Location: 'https://insights.plether.com/register' },
    }))
    vi.stubGlobal('fetch', fetchMock)
    const url = 'https://insights.plether.com/api/insights/v1/divisions/registrations/discord/callback?code=provider-code&state=opaque'
    const env = {
      INSIGHTS_BACKEND_URL: 'https://backend.example.com',
      INSIGHTS_REGISTRATION_PUBLIC_ORIGIN: 'https://insights.plether.com',
      INSIGHTS_REGISTRATION_ORIGIN_TOKEN: 'trusted-pages-secret',
      ASSETS: assets,
    }
    expect((await worker.fetch(new Request(url, { headers: { 'Sec-Fetch-Site': 'cross-site' } }), env)).status).toBe(303)
    fetchMock.mockResolvedValue(new Response(null, {
      status: 303, headers: { Location: 'https://insights.plether.com/register?code=leaked' },
    }))
    expect((await worker.fetch(new Request(url), env)).status).toBe(502)
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

  it.each([
    ['POST', 'session'],
    ['GET', 'x/callback?code=provider-code&state=opaque'],
  ])('rejects preview-host registration %s %s before exposing the origin credential', async (method, path) => {
    const fetchMock = vi.fn().mockResolvedValue(Response.json({ ok: true }))
    vi.stubGlobal('fetch', fetchMock)

    const response = await worker.fetch(
      new Request(
        `https://deployment.plether-insights.pages.dev/api/insights/v1/competitions/testnet-trading-2026-09/registrations/${path}`,
        // Supply valid browser provenance so only the preview-host binding
        // rejects this request. OAuth callbacks bypass that provenance gate.
        { method, headers: { Origin: 'https://insights.plether.com' } },
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
    expect(csp).toContain('connect-src \'self\' https://auth.privy.io')
    expect(csp).toContain('https://*.rpc.privy.systems')
    expect(csp).toContain('https://explorer-api.walletconnect.com')
    expect(csp).toContain('wss://relay.walletconnect.com')
    expect(csp).toContain('https://verify.walletconnect.com')
    expect(csp).toContain('https://verify.walletconnect.org')
    expect(csp).not.toContain('web3modal.org')
    expect(csp).not.toContain('fonts.reown.com')
    expect(csp).not.toContain('secure.walletconnect.org')
    expect(csp).not.toContain('wasm-unsafe-eval')
    expect(csp.match(/https:\/\/\*\.[^; ]+/g)).toEqual(['https://*.rpc.privy.systems'])
  })

  it.each([520, 522, 524, 525])('retries an idempotent GET once on Cloudflare origin status %i', async (originStatus) => {
    const fetchMock = vi.fn()
      .mockResolvedValueOnce(new Response('origin error', { status: originStatus }))
      .mockResolvedValueOnce(Response.json({ healthy: true }))
    vi.stubGlobal('fetch', fetchMock)

    const response = await worker.fetch(apiRequest(), {
      INSIGHTS_BACKEND_URL: 'https://backend.example.com',
      ASSETS: assets,
    })

    expect(response.status).toBe(200)
    await expect(response.json()).resolves.toEqual({ healthy: true })
    expect(fetchMock).toHaveBeenCalledTimes(2)
  })

  it('retries an idempotent GET once on transport failure and recovers', async () => {
    const fetchMock = vi.fn()
      .mockRejectedValueOnce(new Error('socket hang up'))
      .mockResolvedValueOnce(Response.json({ healthy: true }))
    vi.stubGlobal('fetch', fetchMock)

    const response = await worker.fetch(apiRequest(), {
      INSIGHTS_BACKEND_URL: 'https://backend.example.com',
      ASSETS: assets,
    })

    expect(response.status).toBe(200)
    await expect(response.json()).resolves.toEqual({ healthy: true })
    expect(fetchMock).toHaveBeenCalledTimes(2)
  })

  it('returns a controlled 503 with security headers when both origin attempts fail', async () => {
    vi.spyOn(console, 'error').mockImplementation(() => undefined)
    const fetchMock = vi.fn().mockRejectedValue(new Error('origin timeout'))
    vi.stubGlobal('fetch', fetchMock)

    const response = await worker.fetch(apiRequest(), {
      INSIGHTS_BACKEND_URL: 'https://backend.example.com',
      ASSETS: assets,
    })

    expect(response.status).toBe(503)
    expect(response.headers.get('cache-control')).toBe('no-store')
    expect(response.headers.get('retry-after')).toBe('1')
    expect(response.headers.get('x-frame-options')).toBe('DENY')
    await expect(response.json()).resolves.toEqual({
      error: {
        code: 'edge_proxy_unavailable',
        message: 'The edge proxy is temporarily unavailable.',
      },
    })
    expect(fetchMock).toHaveBeenCalledTimes(2)
  })

  it('never retries OAuth callbacks or POST mutations', async () => {
    vi.spyOn(console, 'error').mockImplementation(() => undefined)
    const fetchMock = vi.fn().mockRejectedValue(new Error('origin reset'))
    vi.stubGlobal('fetch', fetchMock)
    const env = {
      INSIGHTS_BACKEND_URL: 'https://backend.example.com',
      INSIGHTS_REGISTRATION_PUBLIC_ORIGIN: 'https://insights.plether.com',
      INSIGHTS_REGISTRATION_ORIGIN_TOKEN: 'trusted-pages-secret',
      ASSETS: assets,
    }

    const callbackResponse = await worker.fetch(
      new Request(
        'https://insights.plether.com/api/insights/v1/divisions/registrations/discord/callback?code=provider-code&state=opaque',
        { headers: { 'Sec-Fetch-Site': 'cross-site' } },
      ),
      env,
    )
    const postResponse = await worker.fetch(
      new Request(
        'https://insights.plether.com/api/insights/v1/divisions/registrations/session',
        { method: 'POST', headers: { Origin: 'https://insights.plether.com' } },
      ),
      env,
    )

    expect(callbackResponse.status).toBe(503)
    expect(postResponse.status).toBe(503)
    expect(fetchMock).toHaveBeenCalledTimes(2)
  })
})

describe('OpenGraph thumbnails and HTML meta rewriting', () => {
  const wallet = '0xfe2c91e9d193a5de484f6b4a7cca3dc05a3533c2'
  const txHash = `0x${'a'.repeat(64)}`
  const tradeId = `${txHash}:12`
  const baseHtml = `<!doctype html><html><head><meta charset="UTF-8" /><meta name="description" content="old" /><meta property="og:title" content="Plether Insights" /><meta name="twitter:card" content="summary" /><title>Old</title></head><body><div id="root"></div></body></html>`

  const spaAssets = () => ({
    fetch: vi.fn(async (input: RequestInfo | URL) => {
      const target = typeof input === 'string' ? input : input instanceof URL ? input.toString() : input.url
      const pathname = new URL(target).pathname
      if (pathname === '/og/default.png') {
        return new Response(new Uint8Array([0x89, 0x50, 0x4e, 0x47]), {
          status: 200,
          headers: { 'Content-Type': 'image/png' },
        })
      }
      if (pathname.startsWith('/og/') && pathname.endsWith('.ttf')) {
        return new Response(new Uint8Array([0, 1, 0, 0]), { status: 200 })
      }
      if (pathname === '/divisions/current.json') {
        return Response.json({
          chainId: 421614,
          week: { startsAt: '2026-11-16T00:00:00Z' },
          standings: [
            {
              address: wallet,
              displayName: 'stan_trader',
              rank: 2,
              tier: 'Gold',
              movement: 'promoted',
              weeklyPnlAssets: '420000000',
              adjustedEquityAssets: '1000000000',
              activeDays: 4,
            },
          ],
        })
      }
      return new Response(baseHtml, {
        status: 200,
        headers: { 'Content-Type': 'text/html; charset=utf-8' },
      })
    }),
  })

  it('rewrites wallet, trade, position, and Divisions routes to dynamic og:image cards', async () => {
    const assetEnv = { ASSETS: spaAssets() }

    const walletRes = await worker.fetch(new Request(`https://insights.plether.com/wallets/${wallet}`), assetEnv)
    const walletHtml = await walletRes.text()
    expect(walletHtml).toContain(`property="og:image" content="https://insights.plether.com/og/wallet/${wallet}.png"`)
    expect(walletHtml).toContain('name="twitter:card" content="summary_large_image"')
    expect(walletHtml).not.toContain('content="summary"')

    const tradeRes = await worker.fetch(
      new Request(`https://insights.plether.com/wallets/${wallet}?trade=${encodeURIComponent(tradeId)}`),
      assetEnv,
    )
    const tradeHtml = await tradeRes.text()
    expect(tradeHtml).toContain(
      `property="og:image" content="https://insights.plether.com/og/wallet/${wallet}.png?trade=${encodeURIComponent(tradeId)}"`,
    )
    expect(tradeHtml).toContain('Closed Position PnL')

    const posRes = await worker.fetch(
      new Request(`https://insights.plether.com/wallets/${wallet}?card=position`),
      assetEnv,
    )
    const posHtml = await posRes.text()
    expect(posHtml).toContain(`property="og:image" content="https://insights.plether.com/og/wallet/${wallet}.png?card=position"`)

    const divRes = await worker.fetch(
      new Request(`https://insights.plether.com/divisions/wallets/${wallet}`),
      assetEnv,
    )
    const divHtml = await divRes.text()
    expect(divHtml).toContain(`property="og:image" content="https://insights.plether.com/og/divisions/${wallet}.png"`)

    // Invalid trade param must never be reflected into og:image.
    const badTradeRes = await worker.fetch(
      new Request(`https://insights.plether.com/wallets/${wallet}?trade=../../evil`),
      assetEnv,
    )
    const badTradeHtml = await badTradeRes.text()
    expect(badTradeHtml).toContain(`property="og:image" content="https://insights.plether.com/og/wallet/${wallet}.png"`)
    expect(badTradeHtml).not.toContain('evil')

    // Non-wallet routes get the static branded default card.
    const execRes = await worker.fetch(new Request('https://insights.plether.com/execution'), assetEnv)
    const execHtml = await execRes.text()
    expect(execHtml).toContain('property="og:image" content="https://insights.plether.com/og/default.png"')
    expect(execHtml).toContain('Execution Quality')
  })

  it('renders wallet account, closed trade, live position, and Divisions PNG thumbnails via the SVG renderer', async () => {
    const renderedSvgs: string[] = []
    setSvgToPngRenderer(async (svg: string) => {
      renderedSvgs.push(svg)
      return new Uint8Array([0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a])
    })

    const fetchMock = vi.fn(async (input: RequestInfo | URL) => {
      const target = String(input)
      if (target.includes(`/wallets/${wallet}/summary`)) {
        return Response.json({
          data: {
            network: { chainId: 421614, releaseVersion: 'v2.0.0' },
            account: wallet,
            identity: { discordUsername: 'stan_trader', isDivisionsLinked: true },
            ledgerAvailable: true,
            ledger: {
              netEquityAssets: '1250000000',
              hasPosition: true,
              position: {
                market: 'plDXY-PERP',
                side: 'long',
                size: '250000000000000000000',
                entryPrice: '104500000000000000000',
                marginAssets: '200000000',
                unrealizedPnlAssets: '55000000',
                liquidatable: false,
              },
            },
            performance: {
              totalTrades: 14,
              volumeAssets: '9500000000',
              realizedPnlAssets: '420500000',
              activeDays: 4,
            },
            balanceHistory: [
              { balanceAssets: '1000000000' },
              { balanceAssets: '1250000000' },
            ],
            activity: [
              {
                id: tradeId,
                activityType: 'Close',
                side: 'long',
                price: '104523000000000000000',
                sizeDelta: '250000000000000000000',
                amountAssets: '26130750000',
                pnlAssets: '1450250000',
                occurredAt: '2026-11-26T14:22:00Z',
                txHash,
              },
            ],
            divisionsHistory: [{ tier: 'Gold', rank: 2 }],
          },
        })
      }
      if (target.includes('/divisions/live')) {
        return Response.json({
          data: {
            chainId: 421614,
            week: { startsAt: '2026-11-23T00:00:00Z' },
            standings: [
              {
                address: wallet,
                displayName: 'stan_trader',
                rank: 1,
                weeklyReturn: 0.2835,
                weeklyPnlAssets: '420500000',
                activeDays: 3,
              },
            ],
          },
        })
      }
      return new Response(null, { status: 404 })
    })
    vi.stubGlobal('fetch', fetchMock)

    const env = {
      INSIGHTS_BACKEND_URL: 'https://backend.example.com',
      ASSETS: spaAssets(),
    }

    const acctPng = await worker.fetch(new Request(`https://insights.plether.com/og/wallet/${wallet}.png`), env)
    expect(acctPng.status).toBe(200)
    expect(acctPng.headers.get('content-type')).toBe('image/png')
    expect(acctPng.headers.get('cache-control')).toBe('public, max-age=60, s-maxage=60')
    expect(renderedSvgs[0]).toContain('ALL-TIME REALIZED PNL')
    expect(renderedSvgs[0]).toContain('LIFE-LONG BALANCE')
    expect(renderedSvgs[0]).toContain('+420.50 USDG')
    expect(renderedSvgs[0]).not.toContain('$')

    const tradePng = await worker.fetch(
      new Request(`https://insights.plether.com/og/wallet/${wallet}.png?trade=${encodeURIComponent(tradeId)}`),
      env,
    )
    expect(tradePng.status).toBe(200)
    expect(renderedSvgs[1]).toContain('CLOSED LONG')
    expect(renderedSvgs[1]).toContain('+1,450.25 USDG')

    const posPng = await worker.fetch(
      new Request(`https://insights.plether.com/og/wallet/${wallet}.png?card=position`),
      env,
    )
    expect(posPng.status).toBe(200)
    expect(renderedSvgs[2]).toContain('OPEN LONG')
    expect(renderedSvgs[2]).toContain('+55.00 USDG')

    const divPng = await worker.fetch(new Request(`https://insights.plether.com/og/divisions/${wallet}.png`), env)
    expect(divPng.status).toBe(200)
    expect(renderedSvgs[3]).toContain('RANK #1')
    expect(renderedSvgs[3]).toContain('GOLD DIVISION')

    const headRes = await worker.fetch(
      new Request(`https://insights.plether.com/og/wallet/${wallet}.png`, { method: 'HEAD' }),
      env,
    )
    expect(headRes.status).toBe(200)
    expect(headRes.headers.get('content-type')).toBe('image/png')
    expect(await headRes.text()).toBe('')
  })

  it('falls back cleanly to /og/default.png when backend is unavailable', async () => {
    setSvgToPngRenderer(async () => new Uint8Array([1, 2, 3]))
    vi.stubGlobal('fetch', vi.fn().mockResolvedValue(new Response(null, { status: 500 })))

    const res = await worker.fetch(
      new Request(`https://insights.plether.com/og/wallet/${wallet}.png`),
      {
        INSIGHTS_BACKEND_URL: 'https://backend.example.com',
        ASSETS: spaAssets(),
      },
    )
    expect(res.status).toBe(200)
    expect(res.headers.get('content-type')).toBe('image/png')
    expect(Array.from(new Uint8Array(await res.arrayBuffer()))).toEqual([0x89, 0x50, 0x4e, 0x47])
  })

  it('rewrites OG metadata cleanly for /tx/:txHash routes and validates optional ?log param', async () => {
    const env = {
      INSIGHTS_BACKEND_URL: 'https://backend.example.com',
      ASSETS: spaAssets(),
    }

    const res = await worker.fetch(
      new Request(`https://insights.plether.com/tx/${txHash}?log=42`),
      env,
    )
    expect(res.status).toBe(200)
    const html = await res.text()
    expect(html).toContain(`Transaction 0xaaaa...aaaa — Plether Insights`)
    expect(html).toContain(`content="https://insights.plether.com/tx/${txHash}?log=42"`)

    const unsafeRes = await worker.fetch(
      new Request(`https://insights.plether.com/tx/${txHash}?log=evil<script>`),
      env,
    )
    const unsafeHtml = await unsafeRes.text()
    expect(unsafeHtml).toContain(`content="https://insights.plether.com/tx/${txHash}"`)
    expect(unsafeHtml).not.toContain('evil')
  })

  it('rewrites OG metadata for /protocol, /sponsored, and /infra', async () => {
    const env = {
      INSIGHTS_BACKEND_URL: 'https://backend.example.com',
      ASSETS: spaAssets(),
    }

    const res = await worker.fetch(new Request('https://insights.plether.com/protocol'), env)
    expect(res.status).toBe(200)
    const html = await res.text()
    expect(html).toContain('Protocol Contracts &amp; Parameters — Plether Insights')
    expect(html).toContain('content="https://insights.plether.com/protocol"')

    const sponsoredRes = await worker.fetch(new Request('https://insights.plether.com/sponsored'), env)
    expect(sponsoredRes.status).toBe(200)
    const sponsoredHtml = await sponsoredRes.text()
    expect(sponsoredHtml).toContain('Sponsored Transactions &amp; Paymaster Health — Plether Insights')
    expect(sponsoredHtml).toContain('content="https://insights.plether.com/sponsored"')

    const infraRes = await worker.fetch(new Request('https://insights.plether.com/infra'), env)
    expect(infraRes.status).toBe(200)
    const infraHtml = await infraRes.text()
    expect(infraHtml).toContain('AWS Infrastructure &amp; Action Readiness — Plether Insights')
    expect(infraHtml).toContain('content="https://insights.plether.com/infra"')

    const epochRes = await worker.fetch(
      new Request('https://insights.plether.com/liquidity/epochs/498776'),
      env,
    )
    expect(epochRes.status).toBe(200)
    const epochHtml = await epochRes.text()
    expect(epochHtml).toContain('LP Epoch #498776 — Plether Insights')
    expect(epochHtml).toContain('content="https://insights.plether.com/liquidity/epochs/498776"')

    const keeperAddr = `0x5a71${'0'.repeat(32)}0d6b`
    const keeperRes = await worker.fetch(
      new Request(`https://insights.plether.com/keepers/${keeperAddr}`),
      env,
    )
    expect(keeperRes.status).toBe(200)
    const keeperHtml = await keeperRes.text()
    expect(keeperHtml).toContain('Keeper 0x5a71...0d6b — Plether Insights')
    expect(keeperHtml).toContain(`content="https://insights.plether.com/keepers/${keeperAddr}"`)
  })
})
