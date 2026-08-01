const ROUTES = {
  '/api/perps/v1/': 'SEPOLIA_BACKEND_URL',
  '/api/spot/v1/': 'MAINNET_BACKEND_URL',
};
const AA_PROXY_PATH = '/api/perps/v1/aa/pimlico';
const AA_PROXY_AUTH_HEADER = 'X-Plether-AA-Proxy-Token';
const EDGE_CACHE_STORED_AT_HEADER = 'X-Plether-Edge-Cached-At';
const EDGE_CACHE_STATUS_HEADER = 'X-Plether-Edge-Cache';

const PUBLIC_PERPS_CACHE_POLICIES = {
  '/api/perps/v1/perps/basket/latest': {
    freshSeconds: 5,
    staleWhileRevalidateSeconds: 10,
  },
  '/api/perps/v1/perps/basket/history': {
    freshSeconds: 60,
    staleWhileRevalidateSeconds: 5 * 60,
  },
  '/api/perps/v1/perps/market/stats': {
    freshSeconds: 30,
    staleWhileRevalidateSeconds: 2 * 60,
  },
};
const PUBLIC_HISTORY_QUERY_KEYS = new Set(['range', 'interval', 'includeComponents']);
const PUBLIC_HISTORY_VARIANTS = new Set([
  '24h:60:false',
  '7d:300:false',
  '30d:3600:false',
  '1y:86400:false',
  '24h:3600:true',
]);

function hasHeader(request, name) {
  const value = request.headers.get(name);
  return value !== null && value !== '';
}

function hasSafeHistoryQuery(url) {
  for (const key of url.searchParams.keys()) {
    if (!PUBLIC_HISTORY_QUERY_KEYS.has(key)) return false;
    if (url.searchParams.getAll(key).length !== 1) return false;
  }

  const range = url.searchParams.get('range');
  const interval = url.searchParams.get('interval');
  const includeComponents = url.searchParams.get('includeComponents');

  if (range === null || interval === null) return false;
  if (includeComponents !== null && includeComponents !== 'true' && includeComponents !== 'false') {
    return false;
  }

  return PUBLIC_HISTORY_VARIANTS.has(
    `${range}:${interval}:${includeComponents === 'true' ? 'true' : 'false'}`
  );
}

/**
 * Only anonymous, idempotent Perps market-data reads are eligible for shared
 * caching. Keeping this as an exact allowlist makes new endpoints private by
 * default, including orders, reveal payloads, users and AA operations.
 */
export function getPublicPerpsCachePolicy(request, url = new URL(request.url)) {
  if (request.method !== 'GET') return undefined;
  if (
    hasHeader(request, 'Authorization') ||
    hasHeader(request, 'Cookie') ||
    hasHeader(request, 'Range')
  ) return undefined;

  const policy = PUBLIC_PERPS_CACHE_POLICIES[url.pathname];
  if (!policy) return undefined;

  if (url.pathname === '/api/perps/v1/perps/basket/history') {
    return hasSafeHistoryQuery(url) ? policy : undefined;
  }

  return url.search === '' ? policy : undefined;
}

export function getPublicPerpsCacheKey(url) {
  const cacheUrl = new URL(url.origin + url.pathname);

  for (const key of ['range', 'interval']) {
    const value = url.searchParams.get(key);
    if (value !== null) cacheUrl.searchParams.set(key, value);
  }
  if (url.searchParams.get('includeComponents') === 'true') {
    cacheUrl.searchParams.set('includeComponents', 'true');
  }

  return cacheUrl.toString();
}

function responseCanBePubliclyCached(response) {
  const cacheControl = response.headers.get('Cache-Control') ?? '';
  const contentType = response.headers.get('Content-Type') ?? '';
  const vary = response.headers.get('Vary') ?? '';

  return (
    response.status === 200 &&
    contentType.toLowerCase().includes('application/json') &&
    !response.headers.has('Set-Cookie') &&
    !/(?:^|,)\s*(?:private|no-store|no-cache)\b/i.test(cacheControl) &&
    (vary === '' || /^accept-encoding$/i.test(vary.trim()))
  );
}

function clientCacheResponse(response, policy, status) {
  const headers = new Headers(response.headers);
  headers.set(
    'Cache-Control',
    `public, max-age=0, s-maxage=${policy.freshSeconds}, stale-while-revalidate=${policy.staleWhileRevalidateSeconds}`
  );
  headers.set(EDGE_CACHE_STATUS_HEADER, status);
  headers.delete(EDGE_CACHE_STORED_AT_HEADER);

  return new Response(response.body, {
    status: response.status,
    statusText: response.statusText,
    headers,
  });
}

function storedCacheResponse(response, policy) {
  const headers = new Headers(response.headers);
  const storageSeconds = policy.freshSeconds + policy.staleWhileRevalidateSeconds;
  headers.set('Cache-Control', `public, max-age=0, s-maxage=${storageSeconds}`);
  headers.set(EDGE_CACHE_STORED_AT_HEADER, String(Date.now()));

  return new Response(response.body, {
    status: response.status,
    statusText: response.statusText,
    headers,
  });
}

function runInBackground(context, promise) {
  const guardedPromise = promise.catch(() => undefined);
  if (context?.waitUntil) {
    context.waitUntil(guardedPromise);
  } else {
    void guardedPromise;
  }
}

async function refreshStoredPublicResponse(fetchBackend, cache, cacheKey, policy) {
  const response = await fetchBackend();
  if (!responseCanBePubliclyCached(response)) {
    await response.body?.cancel().catch(() => undefined);
    return;
  }

  await cache.put(cacheKey, storedCacheResponse(response, policy));
}

async function fetchPublicResponse(request, url, policy, fetchBackend, context) {
  const cache = globalThis.caches?.default;
  if (!cache) return fetchBackend();

  const requestCacheControl = request.headers.get('Cache-Control') ?? '';
  if (/\bno-store\b/i.test(requestCacheControl)) return fetchBackend();

  const cacheKeyUrl = getPublicPerpsCacheKey(url);
  const cacheKey = new Request(cacheKeyUrl, { method: 'GET' });
  const shouldRevalidate = /\bno-cache\b/i.test(requestCacheControl);
  const cached = shouldRevalidate ? undefined : await cache.match(cacheKey);

  if (cached) {
    const storedAt = Number(cached.headers.get(EDGE_CACHE_STORED_AT_HEADER));
    if (Number.isFinite(storedAt) && storedAt > 0) {
      const ageSeconds = Math.max(0, Date.now() - storedAt) / 1000;
      if (ageSeconds <= policy.freshSeconds) {
        return clientCacheResponse(cached, policy, 'HIT');
      }

      runInBackground(
        context,
        refreshStoredPublicResponse(fetchBackend, cache, cacheKey, policy)
      );
      return clientCacheResponse(cached, policy, 'STALE');
    }
  }

  const response = await fetchBackend();
  if (!responseCanBePubliclyCached(response)) return response;

  runInBackground(
    context,
    cache.put(cacheKey, storedCacheResponse(response.clone(), policy))
  );
  return clientCacheResponse(response, policy, 'MISS');
}

export default {
  async fetch(request, env, context) {
    const url = new URL(request.url);

    for (const [prefix, envKey] of Object.entries(ROUTES)) {
      if (url.pathname.startsWith(prefix) || url.pathname === prefix.slice(0, -1)) {
        const origin = env.BACKEND_URL ?? env[envKey];
        if (!origin) return new Response('Backend not configured', { status: 502 });

        const backendPath = '/api' + url.pathname.slice(prefix.length - 1);
        const backendUrl = new URL(backendPath + url.search, origin);

        const headers = new Headers(request.headers);
        // Never trust or forward a browser-supplied origin-auth token. The
        // backend may trust CF-Connecting-IP for AA rate limiting only after
        // this Worker-to-origin credential has been verified.
        headers.delete(AA_PROXY_AUTH_HEADER);
        if (url.pathname === AA_PROXY_PATH) {
          if (!env.AA_PROXY_ORIGIN_TOKEN) {
            return new Response('AA proxy authentication not configured', {
              status: 502,
            });
          }
          headers.set(AA_PROXY_AUTH_HEADER, env.AA_PROXY_ORIGIN_TOKEN);
        }
        headers.set('Host', backendUrl.hostname);
        headers.delete('Origin');

        const fetchBackend = () => fetch(backendUrl, {
          method: request.method,
          headers,
          body: request.body,
        });
        const cachePolicy = getPublicPerpsCachePolicy(request, url);

        return cachePolicy
          ? fetchPublicResponse(request, url, cachePolicy, fetchBackend, context)
          : fetchBackend();
      }
    }

    const response = await env.ASSETS.fetch(request);
    if (response.status === 404 && !url.pathname.includes('.')) {
      return env.ASSETS.fetch(new URL('/', request.url));
    }
    return response;
  },
};
