import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { afterEach, describe, it, mock } from 'node:test';

import worker, { fetchOriginWithResilience, readJsonWithTimeout } from '../public/_worker.js';

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

describe('Wallet content security policy', () => {
  it('permits Privy authentication and external wallets without legacy AppKit services', () => {
    const headers = readFileSync(new URL('../public/_headers', import.meta.url), 'utf8');
    const policy = headers.split('\n').find(line => line.trim().startsWith('Content-Security-Policy:'));
    assert.ok(policy);
    const directives = Object.fromEntries(policy.slice(policy.indexOf(':') + 1).split(';').map(value => {
      const [name, ...sources] = value.trim().split(/\s+/);
      return [name, sources];
    }));
    assert.ok(directives['connect-src'].includes('https://auth.privy.io'));
    assert.ok(directives['connect-src'].includes('https://*.rpc.privy.systems'));
    assert.ok(directives['connect-src'].includes('https://explorer-api.walletconnect.com'));
    assert.ok(directives['connect-src'].includes('wss://relay.walletconnect.com'));
    assert.ok(directives['connect-src'].includes('https://eu.i.posthog.com'));
    for (const name of ['child-src', 'frame-src']) {
      for (const source of ['https://auth.privy.io', 'https://verify.walletconnect.com', 'https://verify.walletconnect.org']) {
        assert.ok(directives[name].includes(source), `${name} must permit ${source}`);
      }
    }
    assert.ok(directives['script-src'].includes('https://challenges.cloudflare.com'));
    assert.equal(directives['script-src'].includes('https:'), false);
    assert.doesNotMatch(policy, /web3modal\.org|fonts\.reown\.com|secure\.walletconnect\.org/);
  });
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
  it('authenticates browser RPC at the origin without exposing a node credential or caching responses', async () => {
    const fetchMock = mockOriginFetch(new Response('{"jsonrpc":"2.0","id":1,"result":"0x66eee"}', { headers: { 'Cache-Control': 'no-store' } }), 1);
    const response = await worker.fetch(new Request('https://app.plether.com/api/perps/v1/rpc', {
      method: 'POST', body: '{"jsonrpc":"2.0","id":1,"method":"eth_chainId","params":[]}',
      headers: { 'X-Plether-AA-Proxy-Token': 'forged', 'CF-Connecting-IP': '192.0.2.1' },
    }), { ...workerEnv(), AA_PROXY_ORIGIN_TOKEN: 'trusted-origin-token' });
    assert.equal(fetchMock.mock.calls[0].arguments[0].href, 'https://sepolia-api.plether.test/api/rpc');
    const options = fetchMock.mock.calls[0].arguments[1];
    assert.equal(options.headers.get('X-Plether-AA-Proxy-Token'), 'trusted-origin-token');
    assert.equal(options.headers.get('CF-Connecting-IP'), '192.0.2.1');
    assert.equal(options.redirect, 'manual');
    assert.equal(options.cf, undefined);
    assert.equal(response.headers.get('X-Plether-AA-Proxy-Token'), null);
    assert.equal(response.headers.get('Cache-Control'), 'no-store');
  });
  it('routes Kernel AA to the exact authenticated origin path with recovery credentials', async () => {
    const fetchMock = mockOriginFetch(new Response('{}', { headers: { 'Cache-Control': 'no-store' } }), 1);
    const url = 'https://app.plether.com/api/perps/v1/aa/kernel/rpc';
    const headers = {
      'X-Plether-AA-Proxy-Token': 'browser-token',
      'X-Plether-AA-Recovery': 'kernel-recovery',
      'CF-Connecting-IP': '192.0.2.1',
    };
    const response = await worker.fetch(new Request(url, { method: 'POST', body: '{}', headers }), {
      ...workerEnv(), AA_PROXY_ORIGIN_TOKEN: 'legacy-only',
      KERNEL_CANARY_AA_PROXY_ORIGIN_TOKEN: 'kernel-only',
    });
    const [origin, options] = fetchMock.mock.calls[0].arguments;
    assert.equal(String(origin), 'https://sepolia-api.plether.test/api/aa/kernel/rpc');
    assert.equal(options.method, 'POST');
    assert.equal(options.headers.get('X-Plether-AA-Proxy-Token'), 'kernel-only');
    assert.equal(options.headers.get('X-Plether-AA-Recovery'), 'kernel-recovery');
    assert.equal(response.headers.get('Cache-Control'), 'no-store');

    const unavailable = await worker.fetch(new Request(url, { method: 'POST', body: '{}', headers }), {
      ...workerEnv(), AA_PROXY_ORIGIN_TOKEN: 'legacy-only',
    });
    assert.equal(unavailable.status, 502);
    assert.equal(fetchMock.mock.callCount(), 1);

    const legacy = await worker.fetch(new Request('https://app.plether.com/api/perps/v1/aa/rpc', {
      method: 'POST', body: '{}', headers,
    }), {
      ...workerEnv(), AA_PROXY_ORIGIN_TOKEN: 'legacy-only',
      KERNEL_CANARY_AA_PROXY_ORIGIN_TOKEN: 'kernel-only',
    });
    assert.equal(legacy.status, 200);
    assert.equal(fetchMock.mock.calls[1].arguments[1].headers.get('X-Plether-AA-Proxy-Token'), 'legacy-only');
  });
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

  it('rejects vault history with missing, duplicate, extra, or unsupported queries at the edge', async () => {
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

    assert.equal(fetchMock.mock.callCount(), 0);
    assert.equal(cacheMatch.mock.callCount(), 0);
    assert.equal(cachePut.mock.callCount(), 0);
    for (const response of responses) {
      assert.equal(response.status, 400);
      assert.equal(response.headers.get('Cache-Control'), 'no-store');
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

  for (const path of [
    '/api/perps/v1/testnet/margin-funding',
    '/api/perps/v1/testnet/margin-funding/a6415ba2-5668-4ba5-a058-f627bd1b938d',
  ]) {
    it(`authenticates the bounded direct funding path ${path}`, async () => {
      const fetchMock = mock.method(globalThis, 'fetch', async () => new Response('{}'));
      await worker.fetch(new Request('https://app.plether.com' + path, {
        method: path.endsWith('margin-funding') ? 'POST' : 'GET',
        headers: { [faucetHeader]: 'browser-spoof', 'CF-Connecting-IP': '203.0.113.8' },
      }), { ...workerEnv(), FAUCET_PROXY_ORIGIN_TOKEN: 'trusted-faucet-origin-token' });
      const [url, options] = fetchMock.mock.calls[0].arguments;
      assert.equal(url.href, 'https://sepolia-api.plether.test/api' + path.slice('/api/perps/v1'.length));
      assert.equal(options.headers.get(faucetHeader), 'trusted-faucet-origin-token');
      assert.equal(options.headers.get('CF-Connecting-IP'), '203.0.113.8');
    });
  }

  it('does not send credentials to arbitrary funding subpaths', async () => {
    const fetchMock = mock.method(globalThis, 'fetch', async () => new Response('{}'));
    await worker.fetch(new Request('https://app.plether.com/api/perps/v1/testnet/margin-funding/not-a-claim', {
      headers: { [faucetHeader]: 'browser-spoof' },
    }), { ...workerEnv(), FAUCET_PROXY_ORIGIN_TOKEN: 'trusted-faucet-origin-token' });
    assert.equal(fetchMock.mock.calls[0].arguments[1].headers.has(faucetHeader), false);
  });

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

describe('Cloudflare API proxy origin timeout and retry resilience', () => {
  it('retries an idempotent GET once after a transport error and succeeds on the second attempt', async () => {
    let attempts = 0;
    const fetchMock = mock.method(globalThis, 'fetch', async () => {
      attempts += 1;
      if (attempts === 1) throw new Error('socket hang up');
      return new Response('{"status":"ok"}', {
        status: 200,
        headers: { 'Content-Type': 'application/json' },
      });
    });

    const response = await worker.fetch(
      new Request('https://app.plether.com/api/perps/v1/health'),
      workerEnv(),
    );

    assert.equal(response.status, 200);
    assert.equal(await response.text(), '{"status":"ok"}');
    assert.equal(fetchMock.mock.callCount(), 2);
    assert.ok(fetchMock.mock.calls[0].arguments[1].signal instanceof AbortSignal);
    assert.ok(fetchMock.mock.calls[1].arguments[1].signal instanceof AbortSignal);
  });

  for (const edgeOriginStatus of [520, 522, 524, 525]) {
    it(`retries an idempotent GET once on Cloudflare origin status ${edgeOriginStatus}`, async () => {
      let attempts = 0;
      const fetchMock = mock.method(globalThis, 'fetch', async () => {
        attempts += 1;
        if (attempts === 1) {
          return new Response('origin error', { status: edgeOriginStatus });
        }
        return new Response('{"recovered":true}', {
          status: 200,
          headers: { 'Content-Type': 'application/json' },
        });
      });

      const response = await worker.fetch(
        new Request('https://app.plether.com/api/perps/v1/perps/basket/latest'),
        workerEnv(),
      );

      assert.equal(response.status, 200);
      assert.equal(await response.text(), '{"recovered":true}');
      assert.equal(fetchMock.mock.callCount(), 2);
    });
  }

  it('does not retry a normal origin 503 response', async () => {
    const fetchMock = mock.method(globalThis, 'fetch', async () => new Response(
      '{"error":{"code":"NETWORK_ERROR","message":"backend busy"}}',
      { status: 503, headers: { 'Content-Type': 'application/json' } },
    ));

    const response = await worker.fetch(
      new Request('https://app.plether.com/api/perps/v1/perps/basket/latest'),
      workerEnv(),
    );

    assert.equal(response.status, 503);
    assert.equal(fetchMock.mock.callCount(), 1);
  });

  it('aborts a stalled origin fetch after timeoutMs and succeeds on the retry attempt', async () => {
    let attempts = 0;
    const fetchMock = mock.method(globalThis, 'fetch', (_url, options) => {
      attempts += 1;
      if (attempts === 1) {
        return new Promise((_resolve, reject) => {
          options.signal.addEventListener('abort', () => {
            reject(options.signal.reason);
          }, { once: true });
        });
      }
      return Promise.resolve(new Response('{"ok":true}', { status: 200 }));
    });

    const response = await fetchOriginWithResilience(
      new URL('https://sepolia-api.plether.test/api/health'),
      { method: 'GET', redirect: 'manual' },
      { timeoutMs: 25, maxAttempts: 2 },
    );

    assert.equal(response.status, 200);
    assert.equal(await response.text(), '{"ok":true}');
    assert.equal(fetchMock.mock.callCount(), 2);
  });

  it('retries read-only POST /api/perps/v1/rpc calls on transport error with the full request body', async () => {
    const bodyText = JSON.stringify([
      { jsonrpc: '2.0', id: 1, method: 'eth_chainId', params: [] },
      { jsonrpc: '2.0', id: 2, method: 'eth_call', params: [{ to: '0x1111111111111111111111111111111111111111', data: '0x' }, 'latest'] },
    ]);
    const observedBodies = [];
    let attempts = 0;
    const fetchMock = mock.method(globalThis, 'fetch', async (_url, options) => {
      attempts += 1;
      observedBodies.push(new TextDecoder().decode(options.body));
      if (attempts === 1) throw new Error('connection reset');
      return new Response('[{"jsonrpc":"2.0","id":1,"result":"0x66eee"}]', {
        status: 200,
        headers: { 'Content-Type': 'application/json', 'Cache-Control': 'no-store' },
      });
    });

    const response = await worker.fetch(
      new Request('https://app.plether.com/api/perps/v1/rpc', {
        method: 'POST',
        body: bodyText,
        headers: {
          'Content-Type': 'application/json',
          'CF-Connecting-IP': '192.0.2.1',
        },
      }),
      { ...workerEnv(), AA_PROXY_ORIGIN_TOKEN: 'trusted-origin-token' },
    );

    assert.equal(response.status, 200);
    assert.equal(fetchMock.mock.callCount(), 2);
    assert.deepEqual(observedBodies, [bodyText, bodyText]);
  });

  it('never retries POST /api/perps/v1/rpc containing eth_sendRawTransaction', async () => {
    mock.method(console, 'error', () => undefined);
    const fetchMock = mock.method(globalThis, 'fetch', async () => {
      throw new Error('connection reset');
    });

    const response = await worker.fetch(
      new Request('https://app.plether.com/api/perps/v1/rpc', {
        method: 'POST',
        body: JSON.stringify([
          { jsonrpc: '2.0', id: 1, method: 'eth_chainId', params: [] },
          { jsonrpc: '2.0', id: 2, method: 'eth_sendRawTransaction', params: ['0x01'] },
        ]),
        headers: {
          'Content-Type': 'application/json',
          'CF-Connecting-IP': '192.0.2.1',
        },
      }),
      { ...workerEnv(), AA_PROXY_ORIGIN_TOKEN: 'trusted-origin-token' },
    );

    assert.equal(response.status, 503);
    assert.equal(fetchMock.mock.callCount(), 1);
    assert.equal(fetchMock.mock.calls[0].arguments[1].signal, undefined);
  });

  it('never retries or applies a short read timeout to long-poll order wait or write endpoints', async () => {
    mock.method(console, 'error', () => undefined);
    const fetchMock = mock.method(globalThis, 'fetch', async () => {
      throw new Error('upstream closed');
    });

    const waitResponse = await worker.fetch(
      new Request('https://app.plether.com/api/perps/v1/perps/orders/42/wait'),
      workerEnv(),
    );
    const faucetResponse = await worker.fetch(
      new Request('https://app.plether.com/api/perps/v1/testnet/faucet', {
        method: 'POST',
        body: '{}',
        headers: { 'CF-Connecting-IP': '203.0.113.8' },
      }),
      { ...workerEnv(), FAUCET_PROXY_ORIGIN_TOKEN: 'trusted-faucet-origin-token' },
    );
    const aaResponse = await worker.fetch(
      new Request('https://app.plether.com/api/perps/v1/aa/rpc', {
        method: 'POST',
        body: '{}',
      }),
      { ...workerEnv(), AA_PROXY_ORIGIN_TOKEN: 'trusted-origin-token' },
    );

    assert.equal(waitResponse.status, 503);
    assert.equal(faucetResponse.status, 503);
    assert.equal(aaResponse.status, 503);
    assert.equal(fetchMock.mock.callCount(), 3);
    for (const call of fetchMock.mock.calls) {
      assert.equal(call.arguments[1].signal, undefined);
    }
  });

  it('retries a failed candle identity probe once and falls back to an uncached page if both probe attempts fail', async () => {
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
    const fetchMock = mock.method(globalThis, 'fetch', async (url) => {
      if (url.pathname.endsWith('/candles/current')) {
        throw new Error('probe timed out');
      }
      return new Response('{"data":{"candles":[]}}', {
        status: 200,
        headers: { 'Content-Type': 'application/json' },
      });
    });

    let response;
    try {
      response = await worker.fetch(new Request(CLOSED_CANDLE_URL), workerEnv());
    } finally {
      if (originalCaches === undefined) delete globalThis.caches;
      else Object.defineProperty(globalThis, 'caches', originalCaches);
    }

    assert.equal(response.status, 200);
    assert.equal(response.headers.get('Cache-Control'), 'no-store');
    assert.equal(response.headers.get('X-Plether-Edge-Cache'), null);
    assert.equal(fetchMock.mock.callCount(), 3);
    assert.ok(fetchMock.mock.calls[0].arguments[0].pathname.endsWith('/candles/current'));
    assert.ok(fetchMock.mock.calls[1].arguments[0].pathname.endsWith('/candles/current'));
    assert.ok(fetchMock.mock.calls[2].arguments[0].pathname.endsWith('/candles'));
  });

  it('bounds buffering and disables retry for oversized chunked POST /api/perps/v1/rpc bodies', async () => {
    const chunkA = new Uint8Array(40_000).fill(97); // 'a'
    const chunkB = new Uint8Array(30_000).fill(98); // 'b' -> exceeds 65,536
    const chunkC = new Uint8Array(10_000).fill(99); // 'c' -> within 1-chunk HWM
    const chunkD = new Uint8Array(10_000).fill(100); // 'd' -> must not be pulled before forwarding
    const chunkE = new Uint8Array(10_000).fill(101); // 'e' -> must not be pulled before forwarding
    let pulledPastLimitBeforeFetch = false;
    let fetchInvoked = false;
    let pulledCount = 0;
    const chunks = [chunkA, chunkB, chunkC, chunkD, chunkE];
    const bodyStream = new ReadableStream({
      pull(controller) {
        const next = chunks.shift();
        if (!next) {
          controller.close();
          return;
        }
        pulledCount += 1;
        if (pulledCount >= 4 && !fetchInvoked) {
          pulledPastLimitBeforeFetch = true;
        }
        controller.enqueue(next);
      },
    });

    let forwardedTotalBytes = 0;
    const fetchMock = mock.method(globalThis, 'fetch', async (_url, options) => {
      fetchInvoked = true;
      assert.equal(options.signal, undefined);
      const reader = options.body.getReader();
      while (true) {
        const { done, value } = await reader.read();
        if (done) break;
        forwardedTotalBytes += value.byteLength;
      }
      return new Response('{"error":"too large"}', { status: 413 });
    });

    const response = await worker.fetch(
      new Request('https://app.plether.com/api/perps/v1/rpc', {
        method: 'POST',
        body: bodyStream,
        duplex: 'half',
        headers: {
          'Content-Type': 'application/json',
          'CF-Connecting-IP': '192.0.2.1',
        },
      }),
      { ...workerEnv(), AA_PROXY_ORIGIN_TOKEN: 'trusted-origin-token' },
    );

    assert.equal(response.status, 413);
    assert.equal(fetchMock.mock.callCount(), 1);
    assert.equal(pulledPastLimitBeforeFetch, false);
    assert.equal(forwardedTotalBytes, 100_000);
  });

  it('aborts and cancels a stalled JSON body read after timeoutMs', async () => {
    let streamCancelled = false;
    const stalledResponse = new Response(
      new ReadableStream({
        cancel() {
          streamCancelled = true;
        },
      }),
      { status: 200, headers: { 'Content-Type': 'application/json' } },
    );

    await assert.rejects(
      () => readJsonWithTimeout(stalledResponse, 25),
      (error) => error instanceof Error && error.name === 'TimeoutError',
    );
    assert.equal(streamCancelled, true);
  });
});
