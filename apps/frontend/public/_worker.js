const ROUTES = {
  '/api/sepolia_v1/': 'SEPOLIA_BACKEND_URL',
  '/api/v1/': 'MAINNET_BACKEND_URL',
};

export default {
  async fetch(request, env) {
    const url = new URL(request.url);

    for (const [prefix, envKey] of Object.entries(ROUTES)) {
      if (url.pathname.startsWith(prefix) || url.pathname === prefix.slice(0, -1)) {
        const origin = env[envKey];
        if (!origin) return new Response('Backend not configured', { status: 502 });

        const backendPath = '/api' + url.pathname.slice(prefix.length - 1);
        const backendUrl = new URL(backendPath + url.search, origin);

        const headers = new Headers(request.headers);
        headers.set('Host', backendUrl.hostname);
        headers.delete('Origin');

        return fetch(backendUrl, {
          method: request.method,
          headers,
          body: request.body,
        });
      }
    }

    const response = await env.ASSETS.fetch(request);
    if (response.status === 404 && !url.pathname.includes('.')) {
      return env.ASSETS.fetch(new URL('/', request.url));
    }
    return response;
  },
};
