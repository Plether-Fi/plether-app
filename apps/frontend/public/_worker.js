const ROUTES = {
  '/api/perps/v1/': 'SEPOLIA_BACKEND_URL',
  '/api/spot/v1/': 'MAINNET_BACKEND_URL',
};
const AA_PROXY_PATHS = new Set([
  '/api/perps/v1/aa/pimlico',
  '/api/perps/v1/aa/rpc',
]);
const AA_PROXY_AUTH_HEADER = 'X-Plether-AA-Proxy-Token';
const FAUCET_PROXY_PATH = '/api/perps/v1/testnet/faucet';
const FAUCET_PROXY_AUTH_HEADER = 'X-Plether-Faucet-Proxy-Token';
const PERPS_AA_MANIFEST_PATH = '/perps-aa-manifest.json';
const BASKET_HISTORY_PATH = '/api/perps/basket/history';
const VAULT_HISTORY_PATH = '/api/perps/v1/perps/vaults/history';
const CANDLE_PAGE_SIZE = 500;
const EDGE_CACHE_STORED_AT_HEADER = 'X-Plether-Edge-Cached-At';
const EDGE_CACHE_STATUS_HEADER = 'X-Plether-Edge-Cache';
const EDGE_ORIGIN_TIMING_METRIC = 'plether_edge_origin';
const EDGE_CACHE_TIMING_METRIC = 'plether_edge_cache';
const CACHE_SERVED_STATUSES = new Set([
  'HIT',
  'REVALIDATED',
  'STALE',
  'UPDATING',
]);

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
  [VAULT_HISTORY_PATH]: {
    freshSeconds: 60,
    staleWhileRevalidateSeconds: 5 * 60,
  },
};
const CANDLE_CURRENT_CACHE_POLICY = {
  // The browser polls every five seconds. Keeping edge reuse to at most four
  // seconds bounds the composed origin-to-next-poll age below the ten-second
  // live-candle SLO instead of budgeting each cache layer independently.
  freshSeconds: 2,
  staleWhileRevalidateSeconds: 2,
};
const ROLLUP_COMPAT_HISTORY_CACHE_POLICY = {
  // This shape can be served by the rollup compatibility reader. Bound edge
  // reuse so a cached success cannot hide the backend's stale-coverage failure
  // for longer than the native live-candle budget.
  freshSeconds: 2,
  staleWhileRevalidateSeconds: 2,
};
const CANDLE_ACTIVE_PAGE_CACHE_POLICY = {
  // Active pages gain the just-finalized candle at interval boundaries, so
  // they share the live freshness budget rather than the immutable-page TTL.
  freshSeconds: 2,
  staleWhileRevalidateSeconds: 2,
};
const CANDLE_CLOSED_PAGE_CACHE_POLICY = {
  freshSeconds: 5 * 60,
  staleWhileRevalidateSeconds: 60 * 60,
};
const CANDLE_INTERVALS = new Set([
  '60',
  '180',
  '300',
  '900',
  '1800',
  '3600',
  '86400',
]);
const CANDLES_PATH = '/api/perps/v1/perps/basket/candles';
const CURRENT_CANDLE_PATH = '/api/perps/v1/perps/basket/candles/current';
const PUBLIC_HISTORY_QUERY_KEYS = new Set(['range', 'interval', 'includeComponents']);
const PUBLIC_HISTORY_VARIANTS = new Set([
  // Live-volume refreshes use a 24-hour window at the active resolution.
  '24h:60:false',
  '24h:180:false',
  '24h:300:false',
  '24h:900:false',
  '24h:1800:false',
  '24h:3600:false',
  '24h:86400:false',
  // Main TradingView history requests are resolution-capped in the datafeed.
  '7d:60:false',
  '7d:180:false',
  '7d:300:false',
  '7d:900:false',
  '7d:1800:false',
  '30d:300:false',
  '30d:900:false',
  '30d:1800:false',
  '30d:3600:false',
  '1y:3600:false',
  '1y:86400:false',
  // Component-rich snapshots are only needed for the 24-hour comparison rail.
  '24h:3600:true',
]);

function withEdgeFetchTiming(response, durationMs) {
  const measuredDuration = Number.isFinite(durationMs)
    ? Math.max(0, durationMs)
    : 0;
  const timedResponse = new Response(response.body, response);
  const cloudflareCacheStatus = response.headers
    .get('CF-Cache-Status')
    ?.toUpperCase();
  const workerCacheStatus = response.headers
    .get(EDGE_CACHE_STATUS_HEADER)
    ?.toUpperCase();
  const servedFromCache =
    CACHE_SERVED_STATUSES.has(cloudflareCacheStatus) ||
    CACHE_SERVED_STATUSES.has(workerCacheStatus);

  if (servedFromCache) timedResponse.headers.delete('Server-Timing');
  timedResponse.headers.append(
    'Server-Timing',
    `${servedFromCache ? EDGE_CACHE_TIMING_METRIC : EDGE_ORIGIN_TIMING_METRIC};dur=${measuredDuration.toFixed(3)}`,
  );
  return timedResponse;
}

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

function hasExactQuery(url, requiredKeys) {
  const allowedKeys = new Set(requiredKeys);
  for (const key of url.searchParams.keys()) {
    if (!allowedKeys.has(key)) return false;
    if (url.searchParams.getAll(key).length !== 1) return false;
  }

  return requiredKeys.every((key) => url.searchParams.getAll(key).length === 1);
}

function getCandleCachePolicy(url) {
  if (url.pathname === CURRENT_CANDLE_PATH) {
    if (!hasExactQuery(url, ['interval'])) return undefined;
    return CANDLE_INTERVALS.has(url.searchParams.get('interval'))
      ? CANDLE_CURRENT_CACHE_POLICY
      : undefined;
  }

  if (url.pathname !== CANDLES_PATH) return undefined;
  if (!hasExactQuery(url, ['interval', 'cursor'])) return undefined;

  const intervalText = url.searchParams.get('interval');
  const cursorText = url.searchParams.get('cursor');
  if (!CANDLE_INTERVALS.has(intervalText) || !/^[1-9]\d*$/.test(cursorText)) {
    return undefined;
  }

  const interval = Number(intervalText);
  const cursor = Number(cursorText);
  const pageSpan = interval * CANDLE_PAGE_SIZE;
  if (!Number.isSafeInteger(cursor) || cursor % pageSpan !== 0) return undefined;

  const nowSeconds = Math.floor(Date.now() / 1000);
  // Permit one additional fixed page for modest browser/backend clock skew.
  // The backend applies the same strict one-page grace bound.
  const latestAllowedCursor = Math.ceil(nowSeconds / pageSpan) * pageSpan + pageSpan;
  if (cursor > latestAllowedCursor) return undefined;

  const currentBucketStart = Math.floor(nowSeconds / interval) * interval;
  return cursor <= currentBucketStart
    ? CANDLE_CLOSED_PAGE_CACHE_POLICY
    : CANDLE_ACTIVE_PAGE_CACHE_POLICY;
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
  if (!policy) return getCandleCachePolicy(url);

  if (url.pathname === '/api/perps/v1/perps/basket/history') {
    if (!hasSafeHistoryQuery(url)) return undefined;
    return url.searchParams.get('includeComponents') === 'true'
      ? policy
      : ROLLUP_COMPAT_HISTORY_CACHE_POLICY;
  }

  if (url.pathname === VAULT_HISTORY_PATH) {
    if (!hasExactQuery(url, ['range', 'interval'])) return undefined;
    return url.searchParams.get('range') === '7d' &&
      url.searchParams.get('interval') === '3600'
      ? policy
      : undefined;
  }

  return url.search === '' ? policy : undefined;
}

export function getPublicPerpsCacheKey(url) {
  const cacheUrl = new URL(url.origin + url.pathname);

  for (const key of ['range', 'interval', 'cursor']) {
    const value = url.searchParams.get(key);
    if (value !== null) cacheUrl.searchParams.set(key, value);
  }
  if (url.searchParams.get('includeComponents') === 'true') {
    cacheUrl.searchParams.set('includeComponents', 'true');
  }

  return cacheUrl.toString();
}

function parseCandleDatasetIdentity(payload, expectedInterval) {
  const data = payload?.data;
  if (!data || typeof data !== 'object' || data.coverageComplete !== true) {
    return undefined;
  }

  const {
    intervalSeconds,
    seriesId,
    configurationHash,
    displayPriceCap,
    volumeChainId,
    volumeRouter,
    volumeCoverageStart,
    volumeCoverageEnd,
    volumeFinalizedThrough,
    volumeCoverageComplete,
    datasetGeneration,
    coverageStart,
    coverageEnd,
    finalizedThrough,
  } = data;
  const validPriceCoverage =
    Number.isSafeInteger(coverageStart) &&
    Number.isSafeInteger(coverageEnd) &&
    Number.isSafeInteger(finalizedThrough) &&
    coverageStart >= 0 &&
    coverageStart < coverageEnd &&
    finalizedThrough >= coverageStart &&
    finalizedThrough <= coverageEnd &&
    coverageStart % expectedInterval === 0 &&
    coverageEnd % expectedInterval === 0 &&
    finalizedThrough % expectedInterval === 0;
  const validVolumeCoverage = volumeCoverageComplete === true
    ? Number.isSafeInteger(volumeCoverageStart) &&
      Number.isSafeInteger(volumeCoverageEnd) &&
      Number.isSafeInteger(volumeFinalizedThrough) &&
      volumeCoverageStart >= 0 &&
      volumeCoverageStart < volumeCoverageEnd &&
      volumeFinalizedThrough >= volumeCoverageStart &&
      volumeFinalizedThrough <= volumeCoverageEnd &&
      volumeCoverageStart % expectedInterval === 0 &&
      volumeCoverageEnd % expectedInterval === 0 &&
      volumeFinalizedThrough % expectedInterval === 0
    : volumeCoverageComplete === false &&
      volumeCoverageStart === null &&
      volumeCoverageEnd === null &&
      volumeFinalizedThrough === null;
  if (
    intervalSeconds !== expectedInterval ||
    typeof seriesId !== 'string' ||
    seriesId.length === 0 ||
    seriesId.length > 256 ||
    typeof configurationHash !== 'string' ||
    !/^sha256:[0-9a-f]{64}$/.test(configurationHash) ||
    typeof displayPriceCap !== 'string' ||
    !/^[1-9]\d*$/.test(displayPriceCap) ||
    !Number.isSafeInteger(volumeChainId) ||
    volumeChainId <= 0 ||
    typeof volumeRouter !== 'string' ||
    !/^0x[0-9a-fA-F]{40}$/.test(volumeRouter) ||
    !Number.isSafeInteger(datasetGeneration) ||
    datasetGeneration <= 0 ||
    !validPriceCoverage ||
    !validVolumeCoverage
  ) {
    return undefined;
  }

  return {
    intervalSeconds,
    seriesId,
    configurationHash,
    displayPriceCap,
    volumeChainId,
    volumeRouter: volumeRouter.toLowerCase(),
    volumeCoverageStart,
    volumeCoverageEnd,
    volumeFinalizedThrough,
    volumeCoverageComplete,
    datasetGeneration,
    coverageStart,
    coverageEnd,
    finalizedThrough,
  };
}

function getCandlePageState(identity, cursor, pricePageState) {
  if (pricePageState === 'active') return 'active';

  // A page wholly before the published price boundary stays empty under this
  // boundary. Include it in the key so an earlier extension cannot reuse it.
  if (cursor <= identity.coverageStart) {
    return `closed-before-price-${identity.coverageStart}`;
  }
  // A terminal or only-partly-finalized price page can gain ordinary live rows
  // without a generation bump (for example after a weekend). It must retain
  // the live TTL until price is finalized through the full page.
  if (
    cursor > identity.coverageEnd ||
    cursor > identity.finalizedThrough
  ) return 'active';

  if (!identity.volumeCoverageComplete) return 'active';

  if (cursor <= identity.volumeCoverageStart) {
    // Include the boundary so a future backwards extension cannot reuse the
    // formerly-null page under the same internal key.
    return `closed-before-volume-${identity.coverageStart}-${identity.volumeCoverageStart}`;
  }
  if (
    cursor <= identity.volumeCoverageEnd &&
    cursor <= identity.volumeFinalizedThrough
  ) {
    // Rows before the current-router boundary are permanently nullable under
    // this key; every row at/after it is now volume-finalized.
    return `closed-volume-finalized-${identity.coverageStart}-${identity.volumeCoverageStart}`;
  }
  return 'active';
}

function candleDatasetIdentitiesEqual(left, right) {
  return (
    left.intervalSeconds === right.intervalSeconds &&
    left.seriesId === right.seriesId &&
    left.configurationHash === right.configurationHash &&
    left.displayPriceCap === right.displayPriceCap &&
    left.volumeChainId === right.volumeChainId &&
    left.volumeRouter === right.volumeRouter &&
    left.datasetGeneration === right.datasetGeneration
  );
}

function getCandleVariantCacheKey(url, identity, pageState) {
  const cacheUrl = new URL(getPublicPerpsCacheKey(url));
  // These parameters exist only on the Worker's internal Cache API key. The
  // browser and backend continue to use the stable public query contract.
  cacheUrl.searchParams.set('__plether_page_state', pageState);
  cacheUrl.searchParams.set('__plether_series', identity.seriesId);
  cacheUrl.searchParams.set('__plether_configuration', identity.configurationHash);
  cacheUrl.searchParams.set('__plether_display_cap', identity.displayPriceCap);
  cacheUrl.searchParams.set('__plether_volume_chain', String(identity.volumeChainId));
  cacheUrl.searchParams.set('__plether_volume_router', identity.volumeRouter);
  cacheUrl.searchParams.set('__plether_generation', String(identity.datasetGeneration));
  return cacheUrl.toString();
}

async function probeCandleDatasetIdentity(variant) {
  try {
    const response = await variant.fetchIdentity();
    if (
      response.status !== 200 ||
      !response.headers.get('Content-Type')?.toLowerCase().includes('application/json')
    ) {
      await response.body?.cancel().catch(() => undefined);
      return undefined;
    }

    const payload = await response.json();
    return parseCandleDatasetIdentity(payload, variant.intervalSeconds);
  } catch {
    return undefined;
  }
}

async function responseMatchesCandleDatasetIdentity(
  response,
  identity,
  expectedPageState,
  cursor,
  pricePageState,
) {
  try {
    const payload = await response.clone().json();
    const responseIdentity = parseCandleDatasetIdentity(
      payload,
      identity.intervalSeconds,
    );
    return responseIdentity !== undefined &&
      candleDatasetIdentitiesEqual(responseIdentity, identity) &&
      getCandlePageState(responseIdentity, cursor, pricePageState) === expectedPageState;
  } catch {
    return false;
  }
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

function noStoreResponse(response) {
  const headers = new Headers(response.headers);
  headers.set('Cache-Control', 'no-store');
  headers.delete(EDGE_CACHE_STATUS_HEADER);
  headers.delete(EDGE_CACHE_STORED_AT_HEADER);
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

async function refreshStoredPublicResponse(
  fetchBackend,
  cache,
  cacheKey,
  policy,
  responseMatchesVariant,
) {
  const response = await fetchBackend();
  const matchesVariant = responseMatchesVariant === undefined ||
    await responseMatchesVariant(response);
  if (!responseCanBePubliclyCached(response) || !matchesVariant) {
    await response.body?.cancel().catch(() => undefined);
    return;
  }

  await cache
    .put(cacheKey, storedCacheResponse(response, policy))
    .catch(() => undefined);
}

async function fetchPublicResponse(
  request,
  url,
  policy,
  fetchBackend,
  context,
  candleVariant,
) {
  const cache = globalThis.caches?.default;
  let effectivePolicy = policy;
  const fetchUncachedCandlePage = async () => noStoreResponse(
    await (candleVariant?.fetchUncachedPage ?? fetchBackend)(),
  );
  if (!cache) return fetchBackend();

  const requestCacheControl = request.headers.get('Cache-Control') ?? '';
  let cacheKeyUrl = getPublicPerpsCacheKey(url);
  let responseMatchesVariant;
  if (candleVariant !== undefined) {
    // Never consult a historical-page cache entry until the origin has supplied
    // the authoritative identity for this exact interval. A failed or malformed
    // probe deliberately degrades to an uncached origin page. Candle rewrites,
    // generation changes, and coverage-watermark publications commit
    // atomically at the origin, so this probe is the request's state
    // linearization point; a later commit belongs to the next request.
    const identity = await probeCandleDatasetIdentity(candleVariant);
    if (identity === undefined) return fetchUncachedCandlePage();
    const pageState = getCandlePageState(
      identity,
      candleVariant.cursor,
      candleVariant.pricePageState,
    );
    effectivePolicy = pageState === 'active'
      ? CANDLE_ACTIVE_PAGE_CACHE_POLICY
      : CANDLE_CLOSED_PAGE_CACHE_POLICY;
    cacheKeyUrl = getCandleVariantCacheKey(
      url,
      identity,
      pageState,
    );
    responseMatchesVariant = (response) =>
      responseMatchesCandleDatasetIdentity(
        response,
        identity,
        pageState,
        candleVariant.cursor,
        candleVariant.pricePageState,
      );
  }
  if (/\bno-store\b/i.test(requestCacheControl)) {
    return candleVariant === undefined
      ? fetchBackend()
      : fetchUncachedCandlePage();
  }
  const cacheKey = new Request(cacheKeyUrl, { method: 'GET' });
  const shouldRevalidate =
    /\bno-cache\b/i.test(requestCacheControl) ||
    /(?:^|,)\s*max-age\s*=\s*0\b/i.test(requestCacheControl);
  const cached = shouldRevalidate ? undefined : await cache.match(cacheKey);

  if (cached) {
    const storedAt = Number(cached.headers.get(EDGE_CACHE_STORED_AT_HEADER));
    if (Number.isFinite(storedAt) && storedAt > 0) {
      const ageSeconds = Math.max(0, Date.now() - storedAt) / 1000;
      if (ageSeconds <= effectivePolicy.freshSeconds) {
        return clientCacheResponse(cached, effectivePolicy, 'HIT');
      }

      const maximumReusableAgeSeconds =
        effectivePolicy.freshSeconds + effectivePolicy.staleWhileRevalidateSeconds;
      if (ageSeconds <= maximumReusableAgeSeconds) {
        runInBackground(
          context,
          refreshStoredPublicResponse(
            fetchBackend,
            cache,
            cacheKey,
            effectivePolicy,
            responseMatchesVariant,
          )
        );
        return clientCacheResponse(cached, effectivePolicy, 'STALE');
      }
    }
  }

  // Keep every origin fetch and Response within the request that created it.
  // Cloudflare Workers cannot share live I/O objects across request contexts;
  // the Cache API is the safe cross-request reuse boundary.
  const response = await fetchBackend();
  const matchesVariant = responseMatchesVariant === undefined ||
    await responseMatchesVariant(response);
  const cacheable = responseCanBePubliclyCached(response) && matchesVariant;
  if (cacheable) {
    runInBackground(
      context,
      cache
        .put(cacheKey, storedCacheResponse(response.clone(), effectivePolicy))
        .catch(() => undefined),
    );
    return clientCacheResponse(response, effectivePolicy, 'MISS');
  }
  return responseMatchesVariant !== undefined && !matchesVariant
    ? noStoreResponse(response)
    : response;
}

const requestHandler = {
  async fetch(request, env, context) {
    const url = new URL(request.url);

    for (const [prefix, envKey] of Object.entries(ROUTES)) {
      if (url.pathname.startsWith(prefix) || url.pathname === prefix.slice(0, -1)) {
        const origin = env.BACKEND_URL ?? env[envKey];
        if (!origin) return new Response('Backend not configured', { status: 502 });

        const backendPath = '/api' + url.pathname.slice(prefix.length - 1);
        const backendUrl = new URL(backendPath + url.search, origin);

        const headers = new Headers(request.headers);
        // Never trust or forward browser-supplied origin-auth tokens. The
        // backend may trust CF-Connecting-IP for protected endpoints only
        // after the matching Worker-to-origin credential has been verified.
        headers.delete(AA_PROXY_AUTH_HEADER);
        headers.delete(FAUCET_PROXY_AUTH_HEADER);
        if (AA_PROXY_PATHS.has(url.pathname)) {
          if (!env.AA_PROXY_ORIGIN_TOKEN) {
            return new Response('AA proxy authentication not configured', {
              status: 502,
            });
          }
          headers.set(AA_PROXY_AUTH_HEADER, env.AA_PROXY_ORIGIN_TOKEN);
        }
        if (url.pathname === FAUCET_PROXY_PATH) {
          if (!env.FAUCET_PROXY_ORIGIN_TOKEN) {
            return new Response('Faucet proxy authentication not configured', {
              status: 502,
            });
          }
          const cloudflareClientIp = request.headers.get('CF-Connecting-IP');
          headers.set(
            FAUCET_PROXY_AUTH_HEADER,
            env.FAUCET_PROXY_ORIGIN_TOKEN,
          );
          headers.delete('CF-Connecting-IP');
          if (cloudflareClientIp) {
            headers.set('CF-Connecting-IP', cloudflareClientIp);
          }
        }

        headers.set('Host', backendUrl.hostname);
        headers.delete('Origin');

        const isBasketHistory = backendPath === BASKET_HISTORY_PATH;
        const cachePolicy = getPublicPerpsCachePolicy(request, url);
        const isCandleRead =
          url.pathname === CANDLES_PATH || url.pathname === CURRENT_CANDLE_PATH;
        const shouldExposeEdgeTiming =
          isBasketHistory || (cachePolicy !== undefined && isCandleRead);
        if (cachePolicy) {
          backendUrl.search = new URL(getPublicPerpsCacheKey(url)).search;
        }
        const fetchOptions = {
          method: request.method,
          headers,
          body: request.body,
          // The origin credential must never be replayed to a Location target.
          // Return a 3xx response to the browser instead of letting fetch follow
          // it with the authenticated request headers.
          redirect: 'manual',
        };

        const fetchBackend = () => fetch(backendUrl, fetchOptions);
        let candleVariant;
        if (url.pathname === CANDLES_PATH && cachePolicy !== undefined) {
          const intervalText = url.searchParams.get('interval');
          const identityUrl = new URL(`${backendUrl.pathname}/current`, backendUrl.origin);
          identityUrl.searchParams.set('interval', intervalText);
          const identityHeaders = new Headers(headers);
          identityHeaders.set('Cache-Control', 'no-store');
          identityHeaders.set('Pragma', 'no-cache');
          identityHeaders.delete('If-Modified-Since');
          identityHeaders.delete('If-None-Match');
          candleVariant = {
            intervalSeconds: Number(intervalText),
            cursor: Number(url.searchParams.get('cursor')),
            pricePageState: cachePolicy === CANDLE_CLOSED_PAGE_CACHE_POLICY
              ? 'closed'
              : 'active',
            fetchIdentity: () => fetch(identityUrl, {
              method: 'GET',
              headers: identityHeaders,
              redirect: 'manual',
            }),
            fetchUncachedPage: () => {
              const uncachedHeaders = new Headers(headers);
              uncachedHeaders.set('Cache-Control', 'no-store');
              uncachedHeaders.set('Pragma', 'no-cache');
              uncachedHeaders.delete('If-Modified-Since');
              uncachedHeaders.delete('If-None-Match');
              return fetch(backendUrl, {
                ...fetchOptions,
                headers: uncachedHeaders,
              });
            },
          };
        }
        const fetchStartedAt = shouldExposeEdgeTiming ? performance.now() : null;
        const response = cachePolicy
          ? await fetchPublicResponse(
              request,
              url,
              cachePolicy,
              fetchBackend,
              context,
              candleVariant,
            )
          : await fetchBackend();

        if (!shouldExposeEdgeTiming) return response;
        return withEdgeFetchTiming(response, performance.now() - fetchStartedAt);
      }
    }

    const response = await env.ASSETS.fetch(request);
    if (url.pathname === PERPS_AA_MANIFEST_PATH) {
      return noStoreResponse(response);
    }
    if (response.status === 404 && !url.pathname.includes('.')) {
      return env.ASSETS.fetch(new URL('/', request.url));
    }
    return response;
  },
};

export default {
  async fetch(request, env, context) {
    try {
      return await requestHandler.fetch(request, env, context);
    } catch (error) {
      console.error(JSON.stringify({
        message: 'Cloudflare Worker request failed',
        method: request.method,
        path: new URL(request.url).pathname,
        error: error instanceof Error ? error.message : 'Unknown error',
      }));
      return Response.json(
        {
          error: {
            code: 'EDGE_PROXY_UNAVAILABLE',
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
      );
    }
  },
};
