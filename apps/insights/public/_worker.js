const API_PREFIX = '/api/insights/v1';

const SECURITY_HEADERS = {
  'X-Frame-Options': 'DENY',
  'X-Content-Type-Options': 'nosniff',
  'Strict-Transport-Security': 'max-age=31536000; includeSubDomains',
  'Referrer-Policy': 'strict-origin-when-cross-origin',
  'Permissions-Policy': 'camera=(), microphone=(), geolocation=(), payment=(), usb=()',
  'Content-Security-Policy':
    "default-src 'self'; script-src 'self'; style-src 'self' 'unsafe-inline'; connect-src 'self'; img-src 'self' data:; font-src 'self' data:; object-src 'none'; base-uri 'self'; form-action 'self'; frame-ancestors 'none'",
};

const API_CACHE_TTLS = {
  status: 30,
  currentCompetition: 60,
  leaderboard: 15,
  wallet: 15,
};

function resolveApiCacheTtl(method, pathname) {
  if (method !== 'GET' && method !== 'HEAD') {
    return null;
  }

  const apiPath = pathname.slice(API_PREFIX.length);

  if (apiPath === '/status') {
    return API_CACHE_TTLS.status;
  }

  if (apiPath === '/competitions/current') {
    return API_CACHE_TTLS.currentCompetition;
  }

  if (/^\/competitions\/[^/]+\/leaderboard$/.test(apiPath)) {
    return API_CACHE_TTLS.leaderboard;
  }

  if (/^\/competitions\/[^/]+\/wallets\/[^/]+$/.test(apiPath)) {
    return API_CACHE_TTLS.wallet;
  }

  return null;
}

function apiFetchOptions(request, headers, cacheTtl) {
  const options = {
    method: request.method,
    headers,
    body: request.body,
    redirect: 'manual',
  };

  if (cacheTtl !== null) {
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

export default {
  async fetch(request, env) {
    const url = new URL(request.url);

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
      headers.set('Host', backendUrl.hostname);
      headers.delete('Origin');

      const cacheTtl = resolveApiCacheTtl(request.method, url.pathname);

      const response = await fetch(backendUrl, apiFetchOptions(request, headers, cacheTtl));

      return applyResponseHeaders(response, url.pathname);
    }

    let response = await env.ASSETS.fetch(request);
    if (response.status === 404 && !url.pathname.includes('.')) {
      response = await env.ASSETS.fetch(new URL('/', request.url));
    }
    return applyResponseHeaders(response, url.pathname);
  },
};
