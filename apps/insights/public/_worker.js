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
  protocolBootstrap: 5,
  protocolLiveState: 15,
  protocolActivity: 10,
  // The backend currently exposes a shallow confirmed head. Keep canonical
  // transaction details close to live-state TTLs so a reorg rewind cannot
  // leave an orphaned detail page cached for an hour.
  protocolConfirmedDetail: 15,
  protocolHistory: 30,
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

  if (apiPath === '/protocol/releases/current') {
    return API_CACHE_TTLS.protocolBootstrap;
  }

  if (/^\/protocol\/releases\/[^/]+\/(overview|house-pool|keepers|wallets|parameters|orders\/[^/]+|keepers\/[^/]+|wallets\/[^/]+|tranches\/[^/]+)$/.test(apiPath)) {
    return API_CACHE_TTLS.protocolLiveState;
  }

  if (/^\/protocol\/releases\/[^/]+\/transactions$/.test(apiPath)) {
    return API_CACHE_TTLS.protocolActivity;
  }

  if (/^\/protocol\/releases\/[^/]+\/transactions\/[^/]+$/.test(apiPath)) {
    return API_CACHE_TTLS.protocolConfirmedDetail;
  }

  if (/^\/protocol\/releases\/[^/]+\/(tranches\/[^/]+\/history|parameter-changes)$/.test(apiPath)) {
    return API_CACHE_TTLS.protocolHistory;
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

function applyResponseHeaders(response, pathname, method = 'GET') {
  const headers = new Headers(response.headers);

  for (const [name, value] of Object.entries(SECURITY_HEADERS)) {
    headers.set(name, value);
  }

  if (
    method !== 'GET'
    && method !== 'HEAD'
  ) {
    headers.set('Cache-Control', 'no-store');
  } else if (response.status < 200 || response.status >= 300) {
    headers.set('Cache-Control', 'no-store');
  } else if (pathname.startsWith('/assets/')) {
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

function backendConfigurationError(code, message, pathname, method = 'GET') {
  return applyResponseHeaders(
    Response.json({ error: { code, message } }, { status: 502 }),
    pathname,
    method,
  );
}

function publicApiRequestHeaders(request, backendUrl) {
  const headers = new Headers();
  for (const name of [
    'Accept',
    'Accept-Encoding',
    'Content-Type',
    'If-Modified-Since',
    'If-None-Match',
    'User-Agent',
  ]) {
    const value = request.headers.get(name);
    if (value !== null) {
      headers.set(name, value);
    }
  }
  headers.set('Host', backendUrl.hostname);
  return headers;
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
          request.method,
        );
      }

      const origin = resolveBackendOrigin(configuredOrigin);
      if (!origin) {
        return backendConfigurationError(
          'backend_configuration_invalid',
          'Insights backend configuration is invalid.',
          url.pathname,
          request.method,
        );
      }

      const backendUrl = new URL(url.pathname + url.search, origin);
      const headers = publicApiRequestHeaders(request, backendUrl);

      const cacheTtl = resolveApiCacheTtl(request.method, url.pathname);

      let response;
      try {
        response = await fetch(backendUrl, apiFetchOptions(request, headers, cacheTtl));
      } catch {
        return backendConfigurationError(
          'backend_unavailable',
          'Insights backend is temporarily unavailable.',
          url.pathname,
          request.method,
        );
      }

      return applyResponseHeaders(response, url.pathname, request.method);
    }

    let response = await env.ASSETS.fetch(request);
    if (response.status === 404 && !url.pathname.includes('.')) {
      response = await env.ASSETS.fetch(new URL('/', request.url));
    }
    return applyResponseHeaders(response, url.pathname, request.method);
  },
};
