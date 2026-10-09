import assert from 'node:assert/strict';
import { afterEach, describe, it, mock } from 'node:test';

import worker from '../public/_worker.js';

const REQUEST_URL =
  'https://app.plether.com/api/perps/v1/perps/basket/history?range=7d&interval=300';
const CLOSED_CANDLE_URL =
  'https://app.plether.com/api/perps/v1/perps/basket/candles?cursor=1800000000&interval=300';
const ACTIVE_CANDLE_URL =
  'https://app.plether.com/api/perps/v1/perps/basket/candles?interval=300&cursor=1800150000';
const CURRENT_CANDLE_URL =
  'https://app.plether.com/api/perps/v1/perps/basket/candles/current?interval=300';
const VAULT_HISTORY_URL =
  'https://app.plether.com/api/perps/v1/perps/vaults/history?interval=3600&range=7d';

afterEach(() => {
  mock.restoreAll();
});

function mockOriginFetch(response, durationMs) {
  const fetchMock = mock.method(globalThis, 'fetch', async () => response);
  const times = [1_000, 1_000 + durationMs];
  mock.method(performance, 'now', () => times.shift());
  return fetchMock;
}

function workerEnv() {
  return {
    SEPOLIA_BACKEND_URL: 'https://sepolia-api.plether.test',
    ASSETS: {
      fetch() {
        throw new Error('API requests must not reach the asset binding');
      },
    },
  };
}

describe('Trading readiness and diagnostics proxy authentication', () => {
  it('passes recovery credentials only to AA and preserves the uncached response credential', async () => {
    const fetchMock = mockOriginFetch(new Response('{}', { headers: { 'Cache-Control': 'no-store', 'X-Plether-AA-Recovery': 'scoped-response' } }), 1);
    const response = await worker.fetch(new Request('https://app.plether.com/api/perps/v1/aa/rpc', {
      method: 'POST', body: '{}', headers: { 'X-Plether-AA-Recovery': 'scoped-request', 'X-Plether-AA-Preparation-Recovery': 'preparation-session' },
    }), { ...workerEnv(), AA_PROXY_ORIGIN_TOKEN: 'trusted' });
    assert.equal(fetchMock.mock.calls[0].arguments[1].headers.get('X-Plether-AA-Recovery'), 'scoped-request');
    assert.equal(response.headers.get('X-Plether-AA-Recovery'), 'scoped-response');
    assert.equal(fetchMock.mock.calls[0].arguments[1].headers.get('X-Plether-AA-Preparation-Recovery'), 'preparation-session');
    assert.equal(response.headers.get('Cache-Control'), 'no-store');
    await worker.fetch(new Request('https://app.plether.com/api/perps/v1/health', {
      headers: { 'X-Plether-AA-Recovery': 'scoped-request', 'X-Plether-AA-Preparation-Recovery': 'preparation-session' },
    }), workerEnv());
    assert.equal(fetchMock.mock.calls[1].arguments[1].headers.has('X-Plether-AA-Recovery'), false);
    assert.equal(fetchMock.mock.calls[1].arguments[1].headers.has('X-Plether-AA-Preparation-Recovery'), false);
  });
  it('forwards bounded diagnostic POSTs through the authenticated, uncached proxy', async () => {
    const fetchMock = mockOriginFetch(new Response('{}', { headers: { 'Cache-Control': 'no-store' } }), 1);
    const body = JSON.stringify({ attemptId: '12345678-1234-4123-8123-123456789abc', stage: 'wallet_approved' });
    const response = await worker.fetch(new Request('https://app.plether.com/api/perps/v1/aa/diagnostics', {
      method: 'POST', body, headers: { 'Content-Type': 'application/json', 'CF-Connecting-IP': '192.0.2.1' },
    }), { ...workerEnv(), AA_PROXY_ORIGIN_TOKEN: 'trusted' });
    const options = fetchMock.mock.calls[0].arguments[1];
    assert.equal(options.method, 'POST');
    assert.equal(options.headers.get('X-Plether-AA-Proxy-Token'), 'trusted');
    assert.equal(response.headers.get('Cache-Control'), 'no-store');
    assert.equal(options.cf, undefined);
  });
  for (const path of ['readiness', 'aa/diagnostics?attemptId=12345678-1234-4123-8123-123456789abc']) {
    it(`authenticates and never edge-caches ${path}`, async () => {
      const fetchMock = mockOriginFetch(new Response('{}', { headers: { 'Cache-Control': 'no-store' } }), 1);
      const response = await worker.fetch(new Request(`https://app.plether.com/api/perps/v1/${path}`, {
        headers: { 'X-Plether-AA-Proxy-Token': 'untrusted-browser-token', 'CF-Connecting-IP': '192.0.2.1' },
      }), { ...workerEnv(), AA_PROXY_ORIGIN_TOKEN: 'trusted-test-token' });
      assert.equal(response.status, 200);
      const options = fetchMock.mock.calls[0].arguments[1];
      assert.equal(options.headers.get('X-Plether-AA-Proxy-Token'), 'trusted-test-token');
      assert.equal(options.cf, undefined);
      assert.equal(response.headers.get('Cache-Control'), 'no-store');
    });
    it(`rejects ${path} without a proxy secret`, async () => {
      const fetchMock = mockOriginFetch(new Response('{}'), 1);
      const response = await worker.fetch(new Request(`https://app.plether.com/api/perps/v1/${path}`), workerEnv());
      assert.equal(response.status, 502);
      assert.equal(fetchMock.mock.callCount(), 0);
    });
  }
});

describe('Cloudflare API proxy history caching and Server-Timing', () => {
  it('caches anonymous public history briefly and preserves an origin response', async () => {
    const originResponse = new Response('history payload', {
      headers: {
        'Content-Type': 'application/json',
        'X-Origin-Request': 'history-123',
      },
    });
    const fetchMock = mockOriginFetch(originResponse, 37.4564);

    const response = await worker.fetch(new Request(REQUEST_URL), workerEnv());

    assert.equal(fetchMock.mock.callCount(), 1);
    assert.equal(
      fetchMock.mock.calls[0].arguments[0].href,
      'https://sepolia-api.plether.test/api/perps/basket/history?range=7d&interval=300',
    );
    const fetchOptions = fetchMock.mock.calls[0].arguments[1];
    assert.equal(fetchOptions.headers.has('Authorization'), false);
    assert.equal(fetchOptions.headers.has('Cookie'), false);
    // Origin responses are admitted only after response headers are inspected
    // by the Worker's manual Cache API path. Forced subrequest caching would
    // override private/no-store/Set-Cookie safeguards before that inspection.
    assert.equal(fetchOptions.cf, undefined);
    assert.equal(response.status, 200);
    assert.equal(response.headers.get('Content-Type'), 'application/json');
    assert.equal(response.headers.get('X-Origin-Request'), 'history-123');
    assert.equal(
      response.headers.get('Server-Timing'),
      'plether_edge_origin;dur=37.456',
    );
    assert.equal(await response.text(), 'history payload');
  });

  it('appends plether_edge_origin to existing origin timing metrics', async () => {
    const originResponse = new Response('{"ok":true}', {
      headers: {
        'Server-Timing': 'snapshots;dur=18.125, volume;dur=21.500',
      },
    });
    mockOriginFetch(originResponse, 302.1);

    const response = await worker.fetch(new Request(REQUEST_URL), workerEnv());

    assert.equal(
      response.headers.get('Server-Timing'),
      'snapshots;dur=18.125, volume;dur=21.500, plether_edge_origin;dur=302.100',
    );
    assert.equal(await response.text(), '{"ok":true}');
  });

  for (const cacheStatus of ['HIT', 'REVALIDATED', 'STALE', 'UPDATING']) {
    it(`marks ${cacheStatus} responses without exposing stale backend timings`, async () => {
      const cachedResponse = new Response('{"cached":true}', {
        headers: {
          'CF-Cache-Status': cacheStatus,
          'Server-Timing': 'plether_app;dur=80.000, plether_db_snapshots;dur=50.000',
        },
      });
      mockOriginFetch(cachedResponse, 3.25);

      const response = await worker.fetch(new Request(REQUEST_URL), workerEnv());

      assert.equal(
        response.headers.get('Server-Timing'),
        'plether_edge_cache;dur=3.250',
      );
      assert.equal(response.headers.get('CF-Cache-Status'), cacheStatus);
      assert.equal(await response.text(), '{"cached":true}');
    });
  }

  it('does not cache non-GET history requests', async () => {
    const originResponse = new Response('{"ok":true}');
    const fetchMock = mockOriginFetch(originResponse, 18.5);

    const response = await worker.fetch(
      new Request(REQUEST_URL, {
        method: 'POST',
        body: '{}',
        headers: {
          Authorization: 'Bearer backend-token',
          Cookie: 'session=backend-session',
        },
      }),
      workerEnv(),
    );

    const fetchOptions = fetchMock.mock.calls[0].arguments[1];
    assert.equal(fetchOptions.cf, undefined);
    assert.equal(
      fetchOptions.headers.get('Authorization'),
      'Bearer backend-token',
    );
    assert.equal(fetchOptions.headers.get('Cookie'), 'session=backend-session');
    assert.equal(
      response.headers.get('Server-Timing'),
      'plether_edge_origin;dur=18.500',
    );
  });

  it('does not share-cache credential-bearing history requests', async () => {
    const originResponse = new Response('{"ok":true}');
    const fetchMock = mockOriginFetch(originResponse, 14.25);

    const response = await worker.fetch(
      new Request(REQUEST_URL, {
        headers: {
          Authorization: 'Bearer backend-token',
          Cookie: 'session=backend-session',
        },
      }),
      workerEnv(),
    );

    const fetchOptions = fetchMock.mock.calls[0].arguments[1];
    assert.equal(fetchOptions.cf, undefined);
    assert.equal(
      fetchOptions.headers.get('Authorization'),
      'Bearer backend-token',
    );
    assert.equal(fetchOptions.headers.get('Cookie'), 'session=backend-session');
    assert.equal(
      response.headers.get('Server-Timing'),
      'plether_edge_origin;dur=14.250',
    );
  });

  it('does not add timing to unrelated API responses', async () => {
    const originResponse = new Response('{"ok":true}');
    const fetchMock = mockOriginFetch(originResponse, 12.5);

    const response = await worker.fetch(
      new Request('https://app.plether.com/api/perps/v1/perps/basket/latest'),
      workerEnv(),
    );

    assert.equal(response.headers.get('Server-Timing'), null);
    assert.equal(fetchMock.mock.calls[0].arguments[1].cf, undefined);
  });
});

describe('Cloudflare API proxy vault history caching', () => {
  for (const range of ['7d', '30d']) it(`caches the exact anonymous ${range} query under a canonical key`, async () => {
    mock.method(Date, 'now', () => 1_800_000_000_000);
    const cacheMatch = mock.fn(async () => undefined);
    const cachePut = mock.fn(async () => undefined);
    const originalCaches = Object.getOwnPropertyDescriptor(globalThis, 'caches');
    Object.defineProperty(globalThis, 'caches', {
      configurable: true,
      value: {
        default: {
          match: cacheMatch,
          put: cachePut,
        },
      },
    });
    const fetchMock = mock.method(globalThis, 'fetch', async () => new Response(
      '{"data":{"complete":true}}',
      { headers: { 'Content-Type': 'application/json' } },
    ));
    const backgroundWork = [];

    let response;
    try {
      response = await worker.fetch(
        new Request(VAULT_HISTORY_URL.replace('7d', range)),
        workerEnv(),
        { waitUntil: (promise) => backgroundWork.push(promise) },
      );
      await Promise.all(backgroundWork);
    } finally {
      if (originalCaches === undefined) delete globalThis.caches;
      else Object.defineProperty(globalThis, 'caches', originalCaches);
    }

    const canonicalUrl =
      `https://app.plether.com/api/perps/v1/perps/vaults/history?range=${range}&interval=3600`;
    assert.equal(fetchMock.mock.callCount(), 1);
    assert.equal(
      fetchMock.mock.calls[0].arguments[0].href,
      `https://sepolia-api.plether.test/api/perps/vaults/history?range=${range}&interval=3600`,
    );
    assert.equal(fetchMock.mock.calls[0].arguments[1].cf, undefined);
    assert.equal(cacheMatch.mock.callCount(), 1);
    assert.equal(cacheMatch.mock.calls[0].arguments[0].url, canonicalUrl);
    assert.equal(cachePut.mock.callCount(), 1);
    assert.equal(cachePut.mock.calls[0].arguments[0].url, canonicalUrl);
    assert.equal(
      cachePut.mock.calls[0].arguments[1].headers.get('Cache-Control'),
      'public, max-age=0, s-maxage=360',
    );
    assert.equal(
      response.headers.get('Cache-Control'),
      'public, max-age=0, s-maxage=60, stale-while-revalidate=300',
    );
    assert.equal(response.headers.get('X-Plether-Edge-Cache'), 'MISS');
    assert.equal(await response.text(), '{"data":{"complete":true}}');
  });

  it('does not share-cache vault history with missing, duplicate, extra, or unsupported queries', async () => {
    const cacheMatch = mock.fn(async () => undefined);
    const cachePut = mock.fn(async () => undefined);
    const originalCaches = Object.getOwnPropertyDescriptor(globalThis, 'caches');
    Object.defineProperty(globalThis, 'caches', {
      configurable: true,
      value: {
        default: {
          match: cacheMatch,
          put: cachePut,
        },
      },
    });
    const fetchMock = mock.method(globalThis, 'fetch', async () => new Response(
      '{"data":{}}',
      { headers: { 'Content-Type': 'application/json' } },
    ));
    const urls = [
      'https://app.plether.com/api/perps/v1/perps/vaults/history?interval=3600',
      'https://app.plether.com/api/perps/v1/perps/vaults/history?range=90d&interval=3600',
      'https://app.plether.com/api/perps/v1/perps/vaults/history?range=7d&interval=300',
      'https://app.plether.com/api/perps/v1/perps/vaults/history?range=7d&interval=3600&cursor=1',
      'https://app.plether.com/api/perps/v1/perps/vaults/history?range=7d&range=7d&interval=3600',
    ];

    const responses = [];
    try {
      for (const url of urls) {
        responses.push(await worker.fetch(new Request(url), workerEnv()));
      }
    } finally {
      if (originalCaches === undefined) delete globalThis.caches;
      else Object.defineProperty(globalThis, 'caches', originalCaches);
    }

    assert.equal(fetchMock.mock.callCount(), urls.length);
    assert.equal(cacheMatch.mock.callCount(), 0);
    assert.equal(cachePut.mock.callCount(), 0);
    for (const response of responses) {
      assert.equal(response.headers.get('X-Plether-Edge-Cache'), null);
    }
  });

  it('does not share-cache credential-bearing or non-GET vault history requests', async () => {
    const cacheMatch = mock.fn(async () => undefined);
    const cachePut = mock.fn(async () => undefined);
    const originalCaches = Object.getOwnPropertyDescriptor(globalThis, 'caches');
    Object.defineProperty(globalThis, 'caches', {
      configurable: true,
      value: {
        default: {
          match: cacheMatch,
          put: cachePut,
        },
      },
    });
    const fetchMock = mock.method(globalThis, 'fetch', async () => new Response(
      '{"data":{}}',
      { headers: { 'Content-Type': 'application/json' } },
    ));

    let authenticatedResponse;
    let postResponse;
    try {
      authenticatedResponse = await worker.fetch(
        new Request(VAULT_HISTORY_URL, {
          headers: {
            Authorization: 'Bearer private-token',
            Cookie: 'session=private-session',
          },
        }),
        workerEnv(),
      );
      postResponse = await worker.fetch(
        new Request(VAULT_HISTORY_URL, {
          method: 'POST',
          body: '{}',
        }),
        workerEnv(),
      );
    } finally {
      if (originalCaches === undefined) delete globalThis.caches;
      else Object.defineProperty(globalThis, 'caches', originalCaches);
    }

    assert.equal(fetchMock.mock.callCount(), 2);
    assert.equal(cacheMatch.mock.callCount(), 0);
    assert.equal(cachePut.mock.callCount(), 0);
    assert.equal(
      fetchMock.mock.calls[0].arguments[1].headers.get('Authorization'),
      'Bearer private-token',
    );
    assert.equal(
      fetchMock.mock.calls[0].arguments[1].headers.get('Cookie'),
      'session=private-session',
    );
    assert.equal(authenticatedResponse.headers.get('X-Plether-Edge-Cache'), null);
    assert.equal(postResponse.headers.get('X-Plether-Edge-Cache'), null);
  });
});

describe('Cloudflare API proxy candle caching', () => {
  for (const status of [200, 503]) {
    it(`preserves exact origin candle-clock evidence on a no-store ${status} current response`, async () => {
      mock.method(Date, 'now', () => 1_800_000_100_000);
      const cacheMatch = mock.fn(async () => new Response('{"cached":true}', {
        status: 200,
        headers: {
          'X-Plether-Candle-Validated-At': '1799999999',
        },
      }));
      const cachePut = mock.fn(async () => undefined);
      const originalCaches = Object.getOwnPropertyDescriptor(globalThis, 'caches');
      Object.defineProperty(globalThis, 'caches', {
        configurable: true,
        value: {
          default: {
            match: cacheMatch,
            put: cachePut,
          },
        },
      });
      const originResponse = new Response('{"data":{"coverageComplete":true}}', {
        status,
        headers: {
          'Content-Type': 'application/json',
          'X-Plether-Candle-Validated-At': '1800000100',
        },
      });
      const fetchMock = mockOriginFetch(originResponse, 17.25);

      let response;
      try {
        response = await worker.fetch(
          new Request(CURRENT_CANDLE_URL, {
            headers: {
              'Cache-Control': 'no-store',
              Pragma: 'no-cache',
            },
          }),
          workerEnv(),
        );
      } finally {
        if (originalCaches === undefined) delete globalThis.caches;
        else Object.defineProperty(globalThis, 'caches', originalCaches);
      }

      assert.equal(fetchMock.mock.callCount(), 1);
      assert.equal(cacheMatch.mock.callCount(), 0);
      assert.equal(cachePut.mock.callCount(), 0);
      assert.equal(
        fetchMock.mock.calls[0].arguments[1].headers.get('Cache-Control'),
        'no-store',
      );
      assert.equal(response.status, status);
      assert.equal(
        response.headers.get('X-Plether-Candle-Validated-At'),
        '1800000100',
      );
      assert.equal(
        response.headers.get('Server-Timing'),
        'plether_edge_origin;dur=17.250',
      );
    });
  }

  for (const [kind, requestUrl] of [
    ['page', ACTIVE_CANDLE_URL],
    ['current', CURRENT_CANDLE_URL],
  ]) {
    it(`replaces stale origin timing with edge cache timing for a cached candle ${kind}`, async () => {
      mock.method(Date, 'now', () => 1_800_000_100_000);
      const cachedResponse = new Response('{"cached":true}', {
        headers: {
          'Content-Type': 'application/json',
          'X-Plether-Edge-Cache': 'HIT',
          'Server-Timing': 'plether_app;dur=80.000, plether_db_candles;dur=50.000',
        },
      });
      mockOriginFetch(cachedResponse, 2.75);

      const response = await worker.fetch(new Request(requestUrl), workerEnv());

      assert.equal(response.headers.get('Server-Timing'), 'plether_edge_cache;dur=2.750');
      assert.equal(await response.text(), '{"cached":true}');
    });
  }

  it('routes candle reads without forcing pre-validation subrequest caching', async () => {
    mock.method(Date, 'now', () => 1_800_000_100_000);
    const fetchMock = mock.method(globalThis, 'fetch', async () => new Response(
      '{"data":[]}',
      { headers: { 'Content-Type': 'application/json' } },
    ));

    await worker.fetch(new Request(CURRENT_CANDLE_URL), workerEnv());
    await worker.fetch(new Request(ACTIVE_CANDLE_URL), workerEnv());
    await worker.fetch(new Request(CLOSED_CANDLE_URL), workerEnv());

    assert.equal(
      fetchMock.mock.calls[0].arguments[0].href,
      'https://sepolia-api.plether.test/api/perps/basket/candles/current?interval=300',
    );
    assert.equal(
      fetchMock.mock.calls[1].arguments[0].href,
      'https://sepolia-api.plether.test/api/perps/basket/candles?interval=300&cursor=1800150000',
    );
    assert.equal(fetchMock.mock.calls[0].arguments[1].cf, undefined);
    assert.equal(fetchMock.mock.calls[1].arguments[1].cf, undefined);
    assert.equal(fetchMock.mock.calls[2].arguments[1].cf, undefined);
  });

  it('bypasses shared caching for credential-bearing candle requests', async () => {
    mock.method(Date, 'now', () => 1_800_000_000_000);
    const fetchMock = mock.method(globalThis, 'fetch', async () => new Response(
      '{"data":[]}',
      { headers: { 'Content-Type': 'application/json' } },
    ));

    await worker.fetch(new Request(CLOSED_CANDLE_URL, {
      headers: {
        Authorization: 'Bearer backend-token',
        Cookie: 'session=backend-session',
      },
    }), workerEnv());

    const fetchOptions = fetchMock.mock.calls[0].arguments[1];
    assert.equal(fetchOptions.cf, undefined);
    assert.equal(fetchOptions.headers.get('Authorization'), 'Bearer backend-token');
    assert.equal(fetchOptions.headers.get('Cookie'), 'session=backend-session');
  });
});

describe('Cloudflare Sepolia faucet proxy authentication', () => {
  const faucetUrl = 'https://app.plether.com/api/perps/v1/testnet/faucet';
  const faucetHeader = 'X-Plether-Faucet-Proxy-Token';

  it('injects the trusted token only on the exact faucet path and preserves the Cloudflare IP', async () => {
    const fetchMock = mock.method(globalThis, 'fetch', async () => new Response('{}'));
    const request = new Request(`${faucetUrl}?source=welcome`, {
      method: 'POST',
      headers: {
        [faucetHeader]: 'browser-spoof',
        'CF-Connecting-IP': '203.0.113.8',
        'Content-Type': 'application/json',
      },
      body: JSON.stringify({
        address: '0x1111111111111111111111111111111111111111',
        confirmationMode: 'async',
      }),
    });

    await worker.fetch(request, {
      ...workerEnv(),
      FAUCET_PROXY_ORIGIN_TOKEN: 'trusted-faucet-origin-token',
    });

    assert.equal(fetchMock.mock.callCount(), 1);
    assert.equal(
      fetchMock.mock.calls[0].arguments[0].href,
      'https://sepolia-api.plether.test/api/testnet/faucet?source=welcome',
    );
    const forwardedHeaders = fetchMock.mock.calls[0].arguments[1].headers;
    assert.equal(
      forwardedHeaders.get(faucetHeader),
      'trusted-faucet-origin-token',
    );
    assert.equal(forwardedHeaders.get('CF-Connecting-IP'), '203.0.113.8');
  });

  it('fails closed without the faucet Pages secret', async () => {
    const fetchMock = mock.method(globalThis, 'fetch', async () => new Response('{}'));
    const response = await worker.fetch(new Request(faucetUrl, {
      method: 'POST',
      headers: { 'CF-Connecting-IP': '203.0.113.8' },
      body: '{}',
    }), workerEnv());

    assert.equal(response.status, 502);
    assert.equal(fetchMock.mock.callCount(), 0);
  });

  it('removes caller-supplied faucet credentials from unrelated API routes', async () => {
    const fetchMock = mock.method(globalThis, 'fetch', async () => new Response('{}'));
    await worker.fetch(new Request(
      'https://app.plether.com/api/perps/v1/user/0x1111111111111111111111111111111111111111/dashboard',
      { headers: { [faucetHeader]: 'browser-spoof' } },
    ), {
      ...workerEnv(),
      FAUCET_PROXY_ORIGIN_TOKEN: 'trusted-faucet-origin-token',
    });

    assert.equal(fetchMock.mock.callCount(), 1);
    const forwardedHeaders = fetchMock.mock.calls[0].arguments[1].headers;
    assert.equal(forwardedHeaders.has(faucetHeader), false);
  });

  it('does not inject the credential into a faucet-like subpath', async () => {
    const fetchMock = mock.method(globalThis, 'fetch', async () => new Response('{}'));
    await worker.fetch(new Request(`${faucetUrl}/status`, {
      headers: { [faucetHeader]: 'browser-spoof' },
    }), {
      ...workerEnv(),
      FAUCET_PROXY_ORIGIN_TOKEN: 'trusted-faucet-origin-token',
    });

    const forwardedHeaders = fetchMock.mock.calls[0].arguments[1].headers;
    assert.equal(forwardedHeaders.has(faucetHeader), false);
  });
});

describe('Bridge funding uses its own uncached origin', () => {
  const fundingUrl = 'https://app.plether.com/api/perps/funding';
  const fundingEnv = () => ({
    ...workerEnv(),
    PERPS_FUNDING_BACKEND_URL: 'https://funding-api.plether.test',
    BACKEND_URL: 'https://legacy-api.plether.test',
    AA_PROXY_ORIGIN_TOKEN: 'aa-secret',
    FAUCET_PROXY_ORIGIN_TOKEN: 'faucet-secret',
  });
  const post = (body = '{}', headers = {}) => ({
    method: 'POST', body, headers: { 'Content-Type': 'application/json', ...headers },
  });

  it('stays disabled without a dedicated origin even if legacy backends are configured', async () => {
    const fetchMock = mock.method(globalThis, 'fetch', async () => new Response('{}'));
    const env = fundingEnv();
    delete env.PERPS_FUNDING_BACKEND_URL;
    const response = await worker.fetch(new Request(`${fundingUrl}/config`), env);
    assert.equal(response.status, 503);
    assert.equal(response.headers.get('Cache-Control'), 'no-store');
    assert.equal((await response.json()).error.code, 'FUNDING_DISABLED');
    assert.equal(fetchMock.mock.callCount(), 0);
  });

  for (const [path, method] of [
    ['/config', 'GET'], ['/quotes', 'POST'], ['/intents', 'POST'],
    ['/intents/12345678-1234-4123-8123-123456789abc', 'GET'],
    ['/intents/intent_123/source', 'POST'], ['/intents/intent_123/retry', 'POST'],
  ]) it(`preserves the full ${method} funding path ${path}`, async () => {
    const fetchMock = mock.method(globalThis, 'fetch', async () => new Response('{}'));
    const response = await worker.fetch(new Request(`${fundingUrl}${path}`, method === 'POST' ? post() : {}), fundingEnv());
    assert.equal(response.status, 200);
    assert.equal(fetchMock.mock.calls[0].arguments[0].href, `https://funding-api.plether.test/api/perps/funding${path}`);
    const options = fetchMock.mock.calls[0].arguments[1];
    assert.equal(options.method, method);
    if (method === 'POST') assert.equal(new TextDecoder().decode(options.body), '{}');
    assert.equal(options.redirect, 'manual');
    assert.equal(options.cache, 'no-store');
    assert.equal(options.cf, undefined);
    assert.equal(options.headers.get('Cache-Control'), 'no-store');
    assert.equal(response.headers.get('Cache-Control'), 'no-store');
  });

  for (const [path, init, status] of [
    ['', {}, 404], ['/unknown', {}, 404], ['/intents/a/delete', post(), 404],
    ['/intents/a%2Fb', {}, 404], ['/intents/' + 'a'.repeat(129), {}, 404],
    ['/config', post(), 405], ['/quotes', {}, 405], ['/config?secret=value', {}, 400],
    ['/quotes', post('{}', { 'Content-Type': 'text/plain' }), 415],
    ['/quotes', post('{}', { 'Content-Encoding': 'gzip' }), 415],
  ]) it(`rejects unsupported funding request ${init.method ?? 'GET'} ${path}`, async () => {
    const fetchMock = mock.method(globalThis, 'fetch', async () => new Response('{}'));
    const response = await worker.fetch(new Request(`${fundingUrl}${path}`, init), fundingEnv());
    assert.equal(response.status, status);
    assert.equal(response.headers.get('Cache-Control'), 'no-store');
    assert.equal(fetchMock.mock.callCount(), 0);
  });

  it('bounds streamed and declared request bodies without contacting the backend', async () => {
    const fetchMock = mock.method(globalThis, 'fetch', async () => new Response('{}'));
    for (const init of [post('a'.repeat(16 * 1024 + 1)), post('{}', { 'Content-Length': String(16 * 1024 + 1) })]) {
      const response = await worker.fetch(new Request(`${fundingUrl}/quotes`, init), fundingEnv());
      assert.equal(response.status, 413);
    }
    assert.equal(fetchMock.mock.callCount(), 0);
    const response = await worker.fetch(new Request(`${fundingUrl}/quotes`, post(' '.repeat(16 * 1024))), fundingEnv());
    assert.equal(response.status, 200);
    assert.equal(fetchMock.mock.calls[0].arguments[1].body.byteLength, 16 * 1024);
  });

  it('never reads or writes shared cache and strips unrelated origin credentials', async () => {
    const originalCaches = Object.getOwnPropertyDescriptor(globalThis, 'caches');
    const match = mock.fn(async () => { throw new Error('Funding must not read public cache'); });
    const put = mock.fn(async () => { throw new Error('Funding must not write public cache'); });
    Object.defineProperty(globalThis, 'caches', { configurable: true, value: { default: { match, put } } });
    const unrelatedCredentials = {
      'X-Plether-AA-Proxy-Token': 'browser-aa',
      'X-Plether-Faucet-Proxy-Token': 'browser-faucet',
      'X-Plether-AA-Recovery': 'browser-recovery',
      'X-Plether-AA-Preparation-Recovery': 'browser-preparation',
    };
    const fetchMock = mock.method(globalThis, 'fetch', async () => new Response('{}', {
      headers: {
        ...unrelatedCredentials, 'Cache-Control': 'public, max-age=3600',
        'CDN-Cache-Control': 'public, max-age=3600', 'Cloudflare-CDN-Cache-Control': 'public, max-age=3600',
        ETag: 'stale-intent', 'X-Plether-Edge-Cache': 'HIT',
      },
    }));
    try {
      const response = await worker.fetch(new Request(`${fundingUrl}/intents/intent_123`, {
        headers: { ...unrelatedCredentials, Authorization: 'Bearer funding-user', Cookie: 'session=funding-user', 'If-None-Match': 'stale-intent' },
      }), fundingEnv());
      const options = fetchMock.mock.calls[0].arguments[1];
      for (const name of Object.keys(unrelatedCredentials)) {
        assert.equal(options.headers.has(name), false);
        assert.equal(response.headers.has(name), false);
      }
      assert.equal(options.headers.get('Authorization'), 'Bearer funding-user');
      assert.equal(options.headers.get('Cookie'), 'session=funding-user');
      assert.equal(options.headers.has('If-None-Match'), false);
      for (const name of ['Cache-Control', 'CDN-Cache-Control', 'Cloudflare-CDN-Cache-Control']) {
        assert.equal(response.headers.get(name), 'no-store');
      }
      assert.equal(response.headers.has('ETag'), false);
      assert.equal(response.headers.has('X-Plether-Edge-Cache'), false);
      assert.equal(match.mock.callCount(), 0);
      assert.equal(put.mock.callCount(), 0);
    } finally {
      if (originalCaches === undefined) delete globalThis.caches;
      else Object.defineProperty(globalThis, 'caches', originalCaches);
    }
  });

  it('returns redirects without replaying credentials to the target', async () => {
    const fetchMock = mock.method(globalThis, 'fetch', async () => new Response(null, {
      status: 307, headers: { Location: 'https://provider.example/collect' },
    }));
    const response = await worker.fetch(new Request(`${fundingUrl}/config`, {
      headers: { Authorization: 'Bearer funding-user' },
    }), fundingEnv());
    assert.equal(response.status, 307);
    assert.equal(fetchMock.mock.callCount(), 1);
    assert.equal(fetchMock.mock.calls[0].arguments[1].redirect, 'manual');
    assert.equal(response.headers.get('Cache-Control'), 'no-store');
  });
});

it('keeps the complete funding namespace on the explicit local API proxy target', async () => {
  const { loadConfigFromFile } = await import('vite');
  const previousTarget = process.env.VITE_API_PROXY_TARGET;
  process.env.VITE_API_PROXY_TARGET = 'http://127.0.0.1:4399';
  try {
    const loaded = await loadConfigFromFile({ command: 'serve', mode: 'test' }, undefined, undefined, 'silent', undefined, 'runner');
    const funding = loaded.config.server.proxy['^/api/perps/funding(?:/|$)'];
    assert.equal(funding.target, 'http://127.0.0.1:4399');
    assert.equal(funding.rewrite, undefined);
    assert.equal(funding.followRedirects, false);
    const callbacks = {};
    funding.configure({ on: (event, callback) => { callbacks[event] = callback; } });
    const headers = new Headers({
      'X-Plether-AA-Proxy-Token': 'aa-secret', 'X-Plether-Faucet-Proxy-Token': 'faucet-secret',
      'X-Plether-AA-Recovery': 'recovery', 'X-Plether-AA-Preparation-Recovery': 'preparation',
    });
    callbacks.proxyReq({ removeHeader: (key) => headers.delete(key), setHeader: (key, value) => headers.set(key, value) });
    assert.equal(headers.get('Cache-Control'), 'no-store');
    for (const key of ['X-Plether-AA-Proxy-Token', 'X-Plether-Faucet-Proxy-Token', 'X-Plether-AA-Recovery', 'X-Plether-AA-Preparation-Recovery']) {
      assert.equal(headers.has(key), false);
    }
    const response = { headers: { 'cache-control': 'public' } };
    callbacks.proxyRes(response);
    assert.equal(response.headers['cache-control'], 'no-store');
  } finally {
    if (previousTarget === undefined) delete process.env.VITE_API_PROXY_TARGET;
    else process.env.VITE_API_PROXY_TARGET = previousTarget;
  }
});


it('rejects a malformed explicit perps release before starting a build', async () => {
  const { loadConfigFromFile } = await import('vite');
  const previousDeployment = process.env.VITE_PERPS_DEPLOYMENT_JSON;
  process.env.VITE_PERPS_DEPLOYMENT_JSON = '{}';
  try {
    await assert.rejects(
      loadConfigFromFile({ command: 'build', mode: 'production' }, undefined, undefined, 'silent', undefined, 'runner'),
      /Invalid perps deployment configuration/,
    );
  } finally {
    if (previousDeployment === undefined) delete process.env.VITE_PERPS_DEPLOYMENT_JSON;
    else process.env.VITE_PERPS_DEPLOYMENT_JSON = previousDeployment;
  }
});
