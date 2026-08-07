const ROUTES = {
  '/api/perps/v1/': 'SEPOLIA_BACKEND_URL',
  '/api/spot/v1/': 'MAINNET_BACKEND_URL',
};
const AA_PROXY_PATH = '/api/perps/v1/aa/pimlico';
const AA_PROXY_AUTH_HEADER = 'X-Plether-AA-Proxy-Token';
const BASKET_HISTORY_PATH = '/api/perps/basket/history';
const EDGE_ORIGIN_TIMING_METRIC = 'plether_edge_origin';

function withEdgeOriginTiming(response, durationMs) {
  const measuredDuration = Number.isFinite(durationMs)
    ? Math.max(0, durationMs)
    : 0;
  const timedResponse = new Response(response.body, response);
  timedResponse.headers.append(
    'Server-Timing',
    `${EDGE_ORIGIN_TIMING_METRIC};dur=${measuredDuration.toFixed(3)}`,
  );
  return timedResponse;
}

export default {
  async fetch(request, env) {
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

        const shouldMeasureOrigin = backendPath === BASKET_HISTORY_PATH;
        const originFetchStartedAt = shouldMeasureOrigin
          ? performance.now()
          : null;
        const response = await fetch(backendUrl, {
          method: request.method,
          headers,
          body: request.body,
        });
        if (!shouldMeasureOrigin) return response;
        return withEdgeOriginTiming(
          response,
          performance.now() - originFetchStartedAt,
        );
      }
    }

    const response = await env.ASSETS.fetch(request);
    if (response.status === 404 && !url.pathname.includes('.')) {
      return env.ASSETS.fetch(new URL('/', request.url));
    }
    return response;
  },
};
