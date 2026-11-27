import {
  buildDefaultOgSvg,
  buildOgCardSvg,
  selectDivisionOgCardData,
  selectWalletOgCardData,
} from '../src/og/ogCardSvg.js';
import {
  isValidTradeIdParam,
  parseOgImageRoute,
  resolveRouteOgMeta,
  rewriteHtmlOgTags,
} from '../src/og/ogMeta.js';

const API_PREFIX = '/api/insights/v1';
const OG_CACHE_TTL_SECONDS = 60;
const OG_ORIGIN_TIMEOUT_MS = 3_000;
const OG_FONT_ASSET_PATHS = [
  '/og/uncut-sans-500.ttf',
  '/og/uncut-sans-700.ttf',
  '/og/jetbrains-mono-700.ttf',
];

let svgToPngRenderer = null;
let cachedFontBuffersPromise = null;

export function setSvgToPngRenderer(renderer) {
  svgToPngRenderer = typeof renderer === 'function' ? renderer : null;
}

export function resetOgFontCache() {
  cachedFontBuffersPromise = null;
}
const REGISTRATION_PATH_PATTERN =
  /^\/api\/insights\/v1\/(?:competitions\/[^/]+\/registrations|divisions\/registrations)(?:\/|$)/;
const REGISTRATION_CALLBACK_PATH_PATTERN =
  /^\/api\/insights\/v1\/(?:competitions\/[^/]+\/registrations\/x|divisions\/registrations\/discord)\/callback$/;
const REGISTRATION_ORIGIN_HEADER = 'X-Plether-Registration-Origin';
const ORIGIN_READ_TIMEOUT_MS = 4_000;
const MAX_ORIGIN_READ_ATTEMPTS = 2;
const RETRYABLE_EDGE_ORIGIN_STATUSES = new Set([520, 522, 524, 525]);

const SECURITY_HEADERS = {
  'X-Frame-Options': 'DENY',
  'X-Content-Type-Options': 'nosniff',
  'Strict-Transport-Security': 'max-age=31536000; includeSubDomains',
  'Referrer-Policy': 'strict-origin-when-cross-origin',
  'Permissions-Policy': 'camera=(), microphone=(), geolocation=(), payment=(), usb=()',
  'Content-Security-Policy':
    "default-src 'self'; script-src 'self' https://challenges.cloudflare.com; style-src 'self' 'unsafe-inline'; connect-src 'self' https://auth.privy.io https://*.rpc.privy.systems https://explorer-api.walletconnect.com https://arb1.arbitrum.io https://sepolia-rollup.arbitrum.io wss://relay.walletconnect.com wss://relay.walletconnect.org wss://www.walletlink.org; img-src 'self' data: blob: https://auth.privy.io https://explorer-api.walletconnect.com; font-src 'self' data:; child-src https://auth.privy.io https://verify.walletconnect.com https://verify.walletconnect.org; frame-src 'self' blob: https://challenges.cloudflare.com https://auth.privy.io https://verify.walletconnect.com https://verify.walletconnect.org; worker-src 'self' blob:; object-src 'none'; base-uri 'self'; form-action 'self'; frame-ancestors 'none'",
};

const API_CACHE_TTLS = {
  status: 30,
  leaderboard: 15,
  wallet: 15,
  pulse: 15,
  liquidity: 30,
  manifest: 60,
};

function resolvePublicApiCacheTtl(method, pathname) {
  if (method !== 'GET' && method !== 'HEAD') {
    return null;
  }

  const apiPath = pathname.slice(API_PREFIX.length);

  if (apiPath === '/status') {
    return API_CACHE_TTLS.status;
  }

  if (
    apiPath === '/overview' ||
    apiPath === '/execution/quality' ||
    apiPath === '/execution/trades' ||
    apiPath === '/keepers/status' ||
    apiPath === '/keepers/activity' ||
    /^\/keepers\/0x[0-9a-fA-F]{40}$/.test(apiPath) ||
    apiPath === '/sponsored/status' ||
    apiPath === '/sponsored/operations' ||
    apiPath === '/oracle/health' ||
    apiPath === '/treasury/overview' ||
    apiPath === '/infra/status'
  ) {
    return API_CACHE_TTLS.pulse;
  }

  if (
    apiPath === '/liquidity/settlements' ||
    apiPath === '/liquidity/forensics' ||
    /^\/liquidity\/epochs\/\d+$/.test(apiPath)
  ) {
    return API_CACHE_TTLS.liquidity;
  }

  if (apiPath === '/protocol/manifest') {
    return API_CACHE_TTLS.manifest;
  }

  if (
    /^\/wallets\/0x[0-9a-fA-F]{40}\/summary$/.test(apiPath) ||
    /^\/wallets\/0x[0-9a-fA-F]{40}\/exposure$/.test(apiPath) ||
    /^\/wallets\/0x[0-9a-fA-F]{40}\/positions\/[a-zA-Z0-9_-]{1,80}$/.test(apiPath)
  ) {
    return API_CACHE_TTLS.wallet;
  }

  if (/^\/tx\/0x[0-9a-fA-F]{64}$/.test(apiPath)) {
    return API_CACHE_TTLS.manifest;
  }

  if (apiPath === '/divisions/stats' || apiPath === '/divisions/traders' || apiPath === '/divisions/live' || apiPath === '/divisions/current' || apiPath === '/divisions/history') {
    return API_CACHE_TTLS.leaderboard;
  }

  if (/^\/competitions\/[^/]+\/leaderboard$/.test(apiPath)) {
    return API_CACHE_TTLS.leaderboard;
  }

  if (/^\/competitions\/[^/]+\/wallets\/[^/]+$/.test(apiPath)) {
    return API_CACHE_TTLS.wallet;
  }

  return null;
}

function resolveApiCacheTtl(request, pathname) {
  // Session cookies and authorization credentials must never become part of
  // an anonymous edge-cache entry. Range requests also bypass the cache so a
  // partial response cannot poison a full-response cache key.
  if (
    request.headers.has('Cookie') ||
    request.headers.has('Authorization') ||
    request.headers.has('Range')
  ) {
    return null;
  }

  return resolvePublicApiCacheTtl(request.method, pathname);
}

function isAnonymousPublicApiPath(method, pathname) {
  if (method !== 'GET' && method !== 'HEAD') return false;
  if (pathname === `${API_PREFIX}/competitions/current`) return true;
  return resolvePublicApiCacheTtl(method, pathname) !== null;
}

function isRegistrationPath(pathname) {
  return REGISTRATION_PATH_PATTERN.test(pathname);
}

function isRegistrationCallback(method, pathname) {
  return method === 'GET' && REGISTRATION_CALLBACK_PATH_PATTERN.test(pathname);
}

function hasSameOriginReferer(headers, publicOrigin) {
  const referer = headers.get('Referer');
  if (!referer) return false;

  try {
    return new URL(referer).origin === publicOrigin;
  } catch {
    return false;
  }
}

function registrationBrowserOriginAllowed(request, pathname, publicOrigin) {
  // X returns by top-level cross-site navigation and commonly omits Origin.
  // OAuth state, the one-time session cookie, and the edge secret authenticate
  // that one callback route; no other registration request gets this exception.
  if (isRegistrationCallback(request.method, pathname)) return true;

  const suppliedOrigin = request.headers.get('Origin');
  if (suppliedOrigin !== null) return suppliedOrigin === publicOrigin;

  // Browsers commonly omit Origin from same-origin GET. Fetch Metadata is the
  // strongest fallback; Referer supports older clients without allowing a
  // sibling Plether origin to drive the trusted registration proxy.
  return (
    request.method === 'GET' &&
    (request.headers.get('Sec-Fetch-Site') === 'same-origin' ||
      hasSameOriginReferer(request.headers, publicOrigin))
  );
}

function createOriginTimeoutError(timeoutMs) {
  const error = new Error(`Origin request timed out after ${timeoutMs}ms`);
  error.name = 'TimeoutError';
  return error;
}

async function fetchWithBoundedTimeout(url, options, timeoutMs, callerSignal) {
  if (!Number.isFinite(timeoutMs) || timeoutMs <= 0) {
    return fetch(url, options);
  }

  const controller = new AbortController();
  let timedOut = false;
  const onCallerAbort = () => controller.abort(callerSignal.reason);
  if (callerSignal) {
    if (callerSignal.aborted) {
      controller.abort(callerSignal.reason);
    } else {
      callerSignal.addEventListener('abort', onCallerAbort, { once: true });
    }
  }

  const timer = setTimeout(() => {
    timedOut = true;
    controller.abort(createOriginTimeoutError(timeoutMs));
  }, timeoutMs);

  try {
    return await fetch(url, {
      ...options,
      signal: controller.signal,
    });
  } catch (error) {
    if (timedOut) {
      throw createOriginTimeoutError(timeoutMs);
    }
    throw error;
  } finally {
    clearTimeout(timer);
    if (callerSignal) {
      callerSignal.removeEventListener('abort', onCallerAbort);
    }
  }
}

export async function fetchOriginWithResilience(
  url,
  options,
  { timeoutMs, maxAttempts = 1, callerSignal } = {},
) {
  const attempts = Math.max(1, maxAttempts);
  for (let attempt = 1; attempt <= attempts; attempt += 1) {
    try {
      const response = await fetchWithBoundedTimeout(
        url,
        options,
        timeoutMs,
        callerSignal,
      );
      if (
        attempt < attempts &&
        RETRYABLE_EDGE_ORIGIN_STATUSES.has(response.status) &&
        !callerSignal?.aborted
      ) {
        await response.body?.cancel().catch(() => undefined);
        continue;
      }
      return response;
    } catch (error) {
      if (attempt >= attempts || callerSignal?.aborted) {
        throw error;
      }
    }
  }
}

function isRetryableInsightsRead(method, pathname) {
  if (method !== 'GET' && method !== 'HEAD') return false;
  return !isRegistrationCallback(method, pathname);
}

function backendUnavailableError(pathname) {
  return applyResponseHeaders(
    Response.json(
      {
        error: {
          code: 'edge_proxy_unavailable',
          message: 'The edge proxy is temporarily unavailable.',
        },
      },
      {
        status: 503,
        headers: {
          'Cache-Control': 'no-store',
          'Retry-After': '1',
        },
      },
    ),
    pathname,
  );
}

function apiFetchOptions(request, headers, cacheTtl, forceNoStore) {
  const options = {
    method: request.method,
    headers,
    body: request.body,
    redirect: 'manual',
  };

  if (forceNoStore) {
    options.cache = 'no-store';
  } else if (cacheTtl !== null) {
    options.cf = {
      cacheEverything: true,
      cacheTtlByStatus: {
        '200-299': cacheTtl,
        '300-599': -1,
      },
    };
  }

  return options;
}

function applyResponseHeaders(response, pathname) {
  const headers = new Headers(response.headers);

  for (const [name, value] of Object.entries(SECURITY_HEADERS)) {
    headers.set(name, value);
  }

  if (pathname.startsWith('/assets/')) {
    headers.set('Cache-Control', 'public, max-age=31536000, immutable');
  } else if (pathname === '/favicon.svg') {
    headers.set('Cache-Control', 'public, max-age=86400');
  } else if (pathname === '/og/default.png') {
    headers.set('Cache-Control', 'public, max-age=3600');
  } else if (pathname.startsWith('/og/') && pathname.endsWith('.png')) {
    headers.set('Cache-Control', `public, max-age=${String(OG_CACHE_TTL_SECONDS)}, s-maxage=${String(OG_CACHE_TTL_SECONDS)}`);
  } else if (pathname === '/divisions/current.json' || pathname === `${API_PREFIX}/competitions/current`) {
    headers.set('Cache-Control', 'no-store');
  } else if (isRegistrationPath(pathname)) {
    // Registration responses may contain session state or rotate an HttpOnly
    // cookie. Never let a browser, intermediary, or Cloudflare cache retain
    // them, including GET status and OAuth callback responses.
    headers.set('Cache-Control', 'private, no-store, max-age=0');
    headers.set('Pragma', 'no-cache');
    headers.set('Referrer-Policy', 'no-referrer');
  }

  return new Response(response.body, {
    status: response.status,
    statusText: response.statusText,
    headers,
  });
}

function backendConfigurationError(code, message, pathname) {
  return applyResponseHeaders(
    Response.json({ error: { code, message } }, { status: 502 }),
    pathname,
  );
}

function registrationOriginError(pathname) {
  return applyResponseHeaders(
    Response.json(
      {
        error: {
          code: 'registration_origin_not_allowed',
          message: 'Registration is not available on this origin.',
        },
      },
      { status: 403 },
    ),
    pathname,
  );
}

function registrationRedirectAllowed(response, pathname, publicOrigin) {
  if (response.status < 300 || response.status >= 400) return true;

  if (pathname === `${API_PREFIX}/divisions/registrations/discord/callback`) {
    const location = response.headers.get('Location');
    if (response.status !== 303 || !location) return false;
    try {
      const target = new URL(location);
      return target.origin === publicOrigin && target.username === '' &&
        target.password === '' && target.pathname === '/register' &&
        target.search === '' && target.hash === '';
    } catch {
      return false;
    }
  }

  const callbackMatch = pathname.match(
    /^\/api\/insights\/v1\/competitions\/([^/]+)\/registrations\/x\/callback$/,
  );
  const location = response.headers.get('Location');
  if (response.status !== 303 || !callbackMatch || !location) return false;

  try {
    const target = new URL(location);
    return (
      target.origin === publicOrigin &&
      target.username === '' &&
      target.password === '' &&
      target.pathname === `/competitions/${callbackMatch[1]}/register` &&
      target.search === '' &&
      target.hash === ''
    );
  } catch {
    return false;
  }
}

function resolveBackendOrigin(configuredOrigin) {
  if (typeof configuredOrigin !== 'string' || configuredOrigin.length === 0) {
    return null;
  }

  try {
    const origin = new URL(configuredOrigin);
    const isHttpsOrigin =
      configuredOrigin === configuredOrigin.trim() &&
      origin.protocol === 'https:' &&
      origin.username === '' &&
      origin.password === '' &&
      origin.pathname === '/' &&
      origin.search === '' &&
      origin.hash === '';

    return isHttpsOrigin ? origin.origin : null;
  } catch {
    return null;
  }
}

async function loadOgFontBuffers(env, originUrl) {
  if (!cachedFontBuffersPromise) {
    cachedFontBuffersPromise = Promise.all(
      OG_FONT_ASSET_PATHS.map(async (assetPath) => {
        try {
          const res = await env.ASSETS.fetch(new URL(assetPath, originUrl));
          if (!res.ok) return null;
          return new Uint8Array(await res.arrayBuffer());
        } catch {
          return null;
        }
      }),
    )
      .then((buffers) => buffers.filter((buf) => buf !== null && buf.byteLength > 0))
      .catch(() => []);
  }
  const buffers = await cachedFontBuffersPromise;
  if (buffers.length === 0) {
    cachedFontBuffersPromise = null;
  }
  return buffers;
}

async function fetchBackendJsonForOg(env, backendOrigin, apiPath) {
  const backendUrl = new URL(apiPath, backendOrigin);
  const headers = new Headers({
    Accept: 'application/json',
    Host: backendUrl.hostname,
  });
  if (env.ALB_ORIGIN_SECRET) {
    headers.set('X-Plether-Origin-Secret', env.ALB_ORIGIN_SECRET);
  }
  try {
    const res = await fetchOriginWithResilience(
      backendUrl,
      {
        method: 'GET',
        headers,
        redirect: 'manual',
        cf: {
          cacheEverything: true,
          cacheTtlByStatus: {
            '200-299': OG_CACHE_TTL_SECONDS,
            '300-599': -1,
          },
        },
      },
      { timeoutMs: OG_ORIGIN_TIMEOUT_MS, maxAttempts: 1 },
    );
    if (!res.ok) return null;
    return await res.json();
  } catch {
    return null;
  }
}

async function serveDefaultOgImage(env, requestUrl, pathname, method) {
  try {
    const fallback = await env.ASSETS.fetch(new URL('/og/default.png', requestUrl));
    if (fallback.ok && fallback.headers.get('Content-Type')?.toLowerCase().includes('image/png')) {
      const headers = new Headers(fallback.headers);
      headers.set('Content-Type', 'image/png');
      return applyResponseHeaders(
        new Response(method === 'HEAD' ? null : fallback.body, {
          status: 200,
          headers,
        }),
        pathname,
      );
    }
  } catch {
    // Fall through to SVG-rendered fallback below if renderer is registered.
  }

  if (svgToPngRenderer) {
    try {
      const fontBuffers = await loadOgFontBuffers(env, requestUrl);
      const pngBytes = await svgToPngRenderer(buildDefaultOgSvg(), fontBuffers);
      return applyResponseHeaders(
        new Response(method === 'HEAD' ? null : pngBytes, {
          status: 200,
          headers: { 'Content-Type': 'image/png' },
        }),
        pathname,
      );
    } catch {
      // Fall through to 404.
    }
  }

  return applyResponseHeaders(new Response(null, { status: 404 }), pathname);
}

async function handleOgImageRequest(request, env, ctx, url, ogRoute) {
  if (request.method !== 'GET' && request.method !== 'HEAD') {
    return applyResponseHeaders(new Response(null, { status: 405 }), url.pathname);
  }

  const cache = typeof caches !== 'undefined' ? caches?.default ?? null : null;
  const cacheKeyUrl = new URL(url.pathname, url.origin);
  const tradeParam = url.searchParams.get('trade');
  const cardParam = url.searchParams.get('card');
  if (ogRoute.kind === 'wallet') {
    if (isValidTradeIdParam(tradeParam)) {
      cacheKeyUrl.searchParams.set('trade', tradeParam);
    } else if (cardParam === 'position') {
      cacheKeyUrl.searchParams.set('card', 'position');
    }
  }
  const cacheKey = new Request(cacheKeyUrl.toString(), { method: 'GET' });

  if (cache) {
    const cached = await cache.match(cacheKey);
    if (cached) {
      return request.method === 'HEAD'
        ? new Response(null, { status: cached.status, headers: cached.headers })
        : cached;
    }
  }

  const configuredOrigin = env.INSIGHTS_BACKEND_URL ?? env.BACKEND_URL;
  const backendOrigin = resolveBackendOrigin(configuredOrigin);
  if (!backendOrigin || !svgToPngRenderer) {
    return serveDefaultOgImage(env, request.url, url.pathname, request.method);
  }

  let cardData = null;
  if (ogRoute.kind === 'wallet') {
    const summaryPayload = await fetchBackendJsonForOg(
      env,
      backendOrigin,
      `${API_PREFIX}/wallets/${ogRoute.account}/summary?limit=30`,
    );
    cardData = selectWalletOgCardData(summaryPayload, {
      tradeId: isValidTradeIdParam(tradeParam) ? tradeParam : null,
      cardMode: cardParam === 'position' ? 'position' : null,
    });
  } else if (ogRoute.kind === 'divisions') {
    const [livePayload, summaryPayload] = await Promise.all([
      fetchBackendJsonForOg(env, backendOrigin, `${API_PREFIX}/divisions/live`),
      fetchBackendJsonForOg(env, backendOrigin, `${API_PREFIX}/wallets/${ogRoute.account}/summary?limit=5`),
    ]);
    let publishedPayload = null;
    try {
      const pubRes = await env.ASSETS.fetch(new URL('/divisions/current.json', request.url));
      if (pubRes.ok && !pubRes.headers.get('Content-Type')?.toLowerCase().includes('text/html')) {
        publishedPayload = await pubRes.json();
      }
    } catch {
      publishedPayload = null;
    }
    cardData = selectDivisionOgCardData(ogRoute.account, livePayload, publishedPayload, summaryPayload);
  }

  if (!cardData) {
    return serveDefaultOgImage(env, request.url, url.pathname, request.method);
  }

  try {
    const fontBuffers = await loadOgFontBuffers(env, request.url);
    const svg = buildOgCardSvg(cardData);
    const pngBytes = await svgToPngRenderer(svg, fontBuffers);
    const response = applyResponseHeaders(
      new Response(pngBytes, {
        status: 200,
        headers: { 'Content-Type': 'image/png' },
      }),
      url.pathname,
    );
    if (cache) {
      const putPromise = cache.put(cacheKey, response.clone()).catch(() => undefined);
      if (ctx && typeof ctx.waitUntil === 'function') {
        ctx.waitUntil(putPromise);
      } else {
        await putPromise;
      }
    }
    return request.method === 'HEAD'
      ? new Response(null, { status: 200, headers: response.headers })
      : response;
  } catch {
    return serveDefaultOgImage(env, request.url, url.pathname, request.method);
  }
}

export default {
  async fetch(request, env, ctx) {
    const url = new URL(request.url);

    const ogRoute = parseOgImageRoute(url.pathname);
    if (ogRoute) {
      return handleOgImageRequest(request, env, ctx, url, ogRoute);
    }

    if (url.pathname === API_PREFIX || url.pathname.startsWith(`${API_PREFIX}/`)) {
      const configuredOrigin = env.INSIGHTS_BACKEND_URL ?? env.BACKEND_URL;
      if (!configuredOrigin) {
        return backendConfigurationError(
          'backend_not_configured',
          'Insights backend is not configured.',
          url.pathname,
        );
      }

      const origin = resolveBackendOrigin(configuredOrigin);
      if (!origin) {
        return backendConfigurationError(
          'backend_configuration_invalid',
          'Insights backend configuration is invalid.',
          url.pathname,
        );
      }

      const backendUrl = new URL(url.pathname + url.search, origin);
      const headers = new Headers(request.headers);
      const registrationRequest = isRegistrationPath(url.pathname);

      // A browser must never be able to choose the credential trusted by the
      // backend. Strip it on every API request and add the Pages secret only
      // for the tightly scoped registration namespace.
      headers.delete('X-Plether-Origin-Secret');
      headers.delete(REGISTRATION_ORIGIN_HEADER);
      if (env.ALB_ORIGIN_SECRET) {
        headers.set('X-Plether-Origin-Secret', env.ALB_ORIGIN_SECRET);
      }
      if (registrationRequest) {
        const registrationOrigin = resolveBackendOrigin(
          env.INSIGHTS_REGISTRATION_PUBLIC_ORIGIN,
        );
        if (!registrationOrigin) {
          return backendConfigurationError(
            'registration_public_origin_not_configured',
            'Insights registration public origin is not configured.',
            url.pathname,
          );
        }
        if (url.origin !== registrationOrigin) {
          return registrationOriginError(url.pathname);
        }
        if (
          !registrationBrowserOriginAllowed(
            request,
            url.pathname,
            registrationOrigin,
          )
        ) {
          return registrationOriginError(url.pathname);
        }
        if (!env.INSIGHTS_REGISTRATION_ORIGIN_TOKEN) {
          return backendConfigurationError(
            'registration_proxy_not_configured',
            'Insights registration proxy is not configured.',
            url.pathname,
          );
        }

        headers.set(
          REGISTRATION_ORIGIN_HEADER,
          env.INSIGHTS_REGISTRATION_ORIGIN_TOKEN,
        );
        const canonicalUrl = new URL(registrationOrigin);
        headers.set('X-Forwarded-Host', canonicalUrl.host);
        headers.set('X-Forwarded-Proto', canonicalUrl.protocol.slice(0, -1));
        // Never forward a sibling-site or provider-supplied Origin. The backend
        // receives only the canonical browser origin that this Worker bound to
        // the request before injecting its private edge credential.
        headers.set('Origin', registrationOrigin);
        // Bypass a previously populated intermediary cache as well as
        // preventing storage of this request. The response is rewritten below
        // too, providing defense in depth for OAuth callbacks and sessions.
        headers.set('Cache-Control', 'no-store');
        headers.set('Pragma', 'no-cache');
      }

      headers.set('Host', backendUrl.hostname);
      if (!registrationRequest) headers.delete('Origin');

      const cacheTtl = resolveApiCacheTtl(request, url.pathname);
      if (isAnonymousPublicApiPath(request.method, url.pathname)) {
        // These endpoints are deliberately anonymous. Even when a request is
        // forced to bypass cache above, do not disclose unrelated registration
        // cookies or bearer credentials to their handlers.
        headers.delete('Cookie');
        headers.delete('Authorization');
      }
      const forceNoStore = cacheTtl === null;
      if (forceNoStore) {
        // The allowlist above is exhaustive. Mutations, current-competition
        // metadata, credentialed/range reads, and new API routes must bypass
        // both default fetch caching and any intermediary cache lookup.
        headers.set('Cache-Control', 'no-store');
        headers.set('Pragma', 'no-cache');
      }

      const retryableRead = isRetryableInsightsRead(request.method, url.pathname);
      let response;
      try {
        response = await fetchOriginWithResilience(
          backendUrl,
          apiFetchOptions(request, headers, cacheTtl, forceNoStore),
          {
            timeoutMs: retryableRead ? ORIGIN_READ_TIMEOUT_MS : undefined,
            maxAttempts: retryableRead ? MAX_ORIGIN_READ_ATTEMPTS : 1,
            callerSignal: request.signal,
          },
        );
      } catch (error) {
        console.error(JSON.stringify({
          message: 'Insights Cloudflare Worker origin fetch failed',
          method: request.method,
          path: url.pathname,
          error: error instanceof Error ? error.message : 'Unknown error',
        }));
        return backendUnavailableError(url.pathname);
      }

      if (
        registrationRequest &&
        !registrationRedirectAllowed(
          response,
          url.pathname,
          env.INSIGHTS_REGISTRATION_PUBLIC_ORIGIN,
        )
      ) {
        return backendConfigurationError(
          'registration_redirect_invalid',
          'Insights registration backend returned an invalid redirect.',
          url.pathname,
        );
      }

      return applyResponseHeaders(response, url.pathname);
    }

    let response = await env.ASSETS.fetch(request);
    // Pages serves index.html with 200 for missing assets. A missing weekly
    // publication must be a 404 so the UI can show its awaiting state.
    if (url.pathname === '/divisions/current.json' && response.ok &&
      response.headers.get('Content-Type')?.toLowerCase().includes('text/html')) {
      response = new Response(null, { status: 404 });
    }
    if (response.status === 404 && !url.pathname.includes('.')) {
      response = await env.ASSETS.fetch(new URL('/', request.url));
    }
    if (
      (request.method === 'GET' || request.method === 'HEAD') &&
      response.ok &&
      response.headers.get('Content-Type')?.toLowerCase().includes('text/html')
    ) {
      const html = await response.text();
      const rewritten = rewriteHtmlOgTags(html, resolveRouteOgMeta(url));
      const headers = new Headers(response.headers);
      headers.delete('Content-Length');
      response = new Response(request.method === 'HEAD' ? null : rewritten, {
        status: response.status,
        statusText: response.statusText,
        headers,
      });
    }
    return applyResponseHeaders(response, url.pathname);
  },
};
