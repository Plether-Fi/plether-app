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
  it('caches the exact anonymous seven-day query under a canonical key', async () => {
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
        new Request(VAULT_HISTORY_URL),
        workerEnv(),
        { waitUntil: (promise) => backgroundWork.push(promise) },
      );
      await Promise.all(backgroundWork);
    } finally {
      if (originalCaches === undefined) delete globalThis.caches;
      else Object.defineProperty(globalThis, 'caches', originalCaches);
    }

    const canonicalUrl =
      'https://app.plether.com/api/perps/v1/perps/vaults/history?range=7d&interval=3600';
    assert.equal(fetchMock.mock.callCount(), 1);
    assert.equal(
      fetchMock.mock.calls[0].arguments[0].href,
      'https://sepolia-api.plether.test/api/perps/vaults/history?range=7d&interval=3600',
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
      'https://app.plether.com/api/perps/v1/perps/vaults/history?range=30d&interval=3600',
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
