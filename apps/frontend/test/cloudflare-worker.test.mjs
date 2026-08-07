import assert from 'node:assert/strict';
import { afterEach, describe, it, mock } from 'node:test';

import worker from '../public/_worker.js';

const REQUEST_URL =
  'https://app.plether.com/api/perps/v1/perps/basket/history?range=7d&interval=300';

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
  it('caches public history briefly and preserves an origin response', async () => {
    const originResponse = new Response('history payload', {
      headers: {
        'Content-Type': 'application/json',
        'X-Origin-Request': 'history-123',
      },
    });
    const fetchMock = mockOriginFetch(originResponse, 37.4564);

    const response = await worker.fetch(
      new Request(REQUEST_URL, {
        headers: {
          Authorization: 'Bearer browser-token',
          Cookie: 'session=browser-session',
        },
      }),
      workerEnv(),
    );

    assert.equal(fetchMock.mock.callCount(), 1);
    assert.equal(
      fetchMock.mock.calls[0].arguments[0].href,
      'https://sepolia-api.plether.test/api/perps/basket/history?range=7d&interval=300',
    );
    const fetchOptions = fetchMock.mock.calls[0].arguments[1];
    assert.equal(fetchOptions.headers.has('Authorization'), false);
    assert.equal(fetchOptions.headers.has('Cookie'), false);
    assert.deepEqual(fetchOptions.cf, {
      cacheEverything: true,
      cacheTtlByStatus: {
        '200-299': 30,
        '300-599': -1,
      },
    });
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
