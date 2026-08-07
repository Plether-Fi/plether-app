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

describe('Cloudflare API proxy Server-Timing', () => {
  it('adds plether_edge_origin while preserving the origin response', async () => {
    const originResponse = new Response('history payload', {
      status: 206,
      statusText: 'Partial Content',
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
    assert.equal(response.status, 206);
    assert.equal(response.statusText, 'Partial Content');
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

  it('does not add timing to unrelated API responses', async () => {
    const originResponse = new Response('{"ok":true}');
    mockOriginFetch(originResponse, 12.5);

    const response = await worker.fetch(
      new Request('https://app.plether.com/api/perps/v1/perps/basket/latest'),
      workerEnv(),
    );

    assert.equal(response.headers.get('Server-Timing'), null);
  });
});
