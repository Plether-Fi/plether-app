const API_PREFIX = '/api/insights/v1';
const REGISTRATION_PATH_PATTERN =
  /^\/api\/insights\/v1\/competitions\/[^/]+\/registrations(?:\/|$)/;
const REGISTRATION_CALLBACK_PATH_PATTERN =
  /^\/api\/insights\/v1\/competitions\/[^/]+\/registrations\/x\/callback$/;
const REGISTRATION_ORIGIN_HEADER = 'X-Plether-Registration-Origin';

const SECURITY_HEADERS = {
  'X-Frame-Options': 'DENY',
  'X-Content-Type-Options': 'nosniff',
  'Strict-Transport-Security': 'max-age=31536000; includeSubDomains',
  'Referrer-Policy': 'strict-origin-when-cross-origin',
  'Permissions-Policy': 'camera=(), microphone=(), geolocation=(), payment=(), usb=()',
  'Content-Security-Policy':
    "default-src 'self'; script-src 'self' https://challenges.cloudflare.com; style-src 'self' 'unsafe-inline'; connect-src 'self' https://api.web3modal.org https://relay.walletconnect.org https://rpc.walletconnect.org https://verify.walletconnect.org https://sepolia-rollup.arbitrum.io wss://relay.walletconnect.com wss://relay.walletconnect.org; img-src 'self' data: blob: https://api.web3modal.org https://secure.walletconnect.org; font-src 'self' data: https://fonts.reown.com; frame-src 'self' blob: https://challenges.cloudflare.com https://secure.walletconnect.org https://verify.walletconnect.org; worker-src 'self' blob:; object-src 'none'; base-uri 'self'; form-action 'self'; frame-ancestors 'none'",
};

const API_CACHE_TTLS = {
  status: 30,
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

function resolvePublicApiCacheTtl(method, pathname) {
  if (method !== 'GET' && method !== 'HEAD') {
    return null;
  }

  const apiPath = pathname.slice(API_PREFIX.length);

  if (apiPath === '/status') {
    return API_CACHE_TTLS.status;
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

function applyResponseHeaders(response, pathname, method = 'GET') {
  const headers = new Headers(response.headers);

  for (const [name, value] of Object.entries(SECURITY_HEADERS)) {
    headers.set(name, value);
  }

  if (isRegistrationPath(pathname)) {
    // Registration responses may contain session state or rotate an HttpOnly
    // cookie. Never let a browser, intermediary, or Cloudflare cache retain
    // them, including GET status and OAuth callback responses.
    headers.set('Cache-Control', 'private, no-store, max-age=0');
    headers.set('Pragma', 'no-cache');
    headers.set('Referrer-Policy', 'no-referrer');
  } else if (
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
  } else if (pathname === `${API_PREFIX}/competitions/current`) {
    headers.set('Cache-Control', 'no-store');
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
    'Range',
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
      const registrationRequest = isRegistrationPath(url.pathname);
      const headers = registrationRequest
        ? new Headers(request.headers)
        : publicApiRequestHeaders(request, backendUrl);

      // A browser must never be able to choose the credential trusted by the
      // backend. Strip it on every API request and add the Pages secret only
      // for the tightly scoped registration namespace.
      headers.delete(REGISTRATION_ORIGIN_HEADER);
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

      let response;
      try {
        response = await fetch(
          backendUrl,
          apiFetchOptions(request, headers, cacheTtl, forceNoStore),
        );
      } catch {
        return backendConfigurationError(
          'backend_unavailable',
          'Insights backend is temporarily unavailable.',
          url.pathname,
          request.method,
        );
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

      return applyResponseHeaders(response, url.pathname, request.method);
    }

    let response = await env.ASSETS.fetch(request);
    if (response.status === 404 && !url.pathname.includes('.')) {
      response = await env.ASSETS.fetch(new URL('/', request.url));
    }
    return applyResponseHeaders(response, url.pathname, request.method);
  },
};
