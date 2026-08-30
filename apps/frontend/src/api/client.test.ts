import { Result } from 'better-result';
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';

import {
  PlethApiClient,
  PlethApiError,
  TESTNET_FAUCET_TIMEOUT_MESSAGE,
  TESTNET_FAUCET_TIMEOUT_MS,
  TESTNET_FAUCET_UPGRADE_REQUIRED_MESSAGE,
  apiScopeToApiPath,
  getScopedApiBaseUrl,
  isUpstreamApiError,
  testnetFaucetErrorMessage,
} from './client';

const analyticsMock = vi.hoisted(() => ({
  captureFrontendLog: vi.fn(),
}));

vi.mock('../analytics/client', () => ({
  captureFrontendLog: analyticsMock.captureFrontendLog,
}));

beforeEach(() => {
  vi.clearAllMocks();
});

afterEach(() => {
  vi.useRealTimers();
  vi.unstubAllGlobals();
});

describe('apiScopeToApiPath', () => {
  it('pins spot to the spot API namespace', () => {
    expect(apiScopeToApiPath('spot')).toBe('/api/spot/v1');
  });

  it('pins perps to the perps API namespace', () => {
    expect(apiScopeToApiPath('perps')).toBe('/api/perps/v1');
  });
});

describe('getScopedApiBaseUrl', () => {
  it('keeps spot on mainnet even when a testnet chain would route elsewhere', () => {
    const expectedSpotBaseUrl = import.meta.env.VITE_API_URL as string | undefined ?? '/api/spot/v1';

    expect(apiScopeToApiPath('spot')).toBe('/api/spot/v1');
    expect(getScopedApiBaseUrl('spot')).toBe(expectedSpotBaseUrl);
  });

  it('keeps perps on the testnet backend scope', () => {
    const expectedPerpsBaseUrl = import.meta.env.VITE_API_URL as string | undefined ?? '/api/perps/v1';

    expect(apiScopeToApiPath('perps')).toBe('/api/perps/v1');
    expect(getScopedApiBaseUrl('perps')).toBe(expectedPerpsBaseUrl);
  });
});

describe('isUpstreamApiError', () => {
  it('matches RPC and network API errors', () => {
    expect(isUpstreamApiError(new PlethApiError('RPC_ERROR', 'execution reverted', 400))).toBe(true);
    expect(isUpstreamApiError(new PlethApiError('NETWORK_ERROR', 'node unreachable'))).toBe(true);
  });

  it('matches service-unavailable HTTP statuses', () => {
    expect(isUpstreamApiError(new PlethApiError('INTERNAL_ERROR', 'bad gateway', 502))).toBe(true);
    expect(isUpstreamApiError(new PlethApiError('INTERNAL_ERROR', 'unavailable', 503))).toBe(true);
    expect(isUpstreamApiError(new PlethApiError('INTERNAL_ERROR', 'timeout', 504))).toBe(true);
  });

  it('does not match user input errors', () => {
    expect(isUpstreamApiError(new PlethApiError('INVALID_ADDRESS', 'invalid address', 400))).toBe(false);
    expect(isUpstreamApiError(new PlethApiError('INVALID_AMOUNT', 'invalid amount', 400))).toBe(false);
  });
});

describe('API request timeouts', () => {
  it('keeps the faucet client deadline below the ALB idle timeout', () => {
    expect(TESTNET_FAUCET_TIMEOUT_MS).toBe(65_000);
    expect(TESTNET_FAUCET_TIMEOUT_MS).toBeLessThan(75_000);
  });

  it('allows a faucet claim to finish after the default 30-second API timeout', async () => {
    vi.useFakeTimers();
    vi.stubGlobal('fetch', vi.fn(() =>
      new Promise<Response>((resolve) => {
        setTimeout(() => {
          resolve(new Response(JSON.stringify({
            data: {
              address: '0x1111111111111111111111111111111111111111',
              amount: '100000000000',
              token: '0x2222222222222222222222222222222222222222',
              txHash: '0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa',
              status: 'minted',
            },
            meta: {
              blockNumber: 1,
              chainId: 421614,
              cached: false,
            },
          }), {
            status: 200,
            headers: { 'Content-Type': 'application/json' },
          }));
        }, 31_000);
      })
    ));
    const client = new PlethApiClient({
      baseUrl: '/api',
      timeout: 30_000,
    });

    const pendingClaim = client.claimTestnetFaucet(
      '0x1111111111111111111111111111111111111111'
    );
    expect(fetch).toHaveBeenCalledWith(
      '/api/testnet/faucet',
      expect.objectContaining({
        body: JSON.stringify({
          address: '0x1111111111111111111111111111111111111111',
          confirmationMode: 'async',
        }),
      })
    );
    await vi.advanceTimersByTimeAsync(31_000);
    const result = await pendingClaim;

    expect(Result.isError(result)).toBe(false);
    if (Result.isError(result)) return;
    expect(result.value.data.status).toBe('minted');
    expect(analyticsMock.captureFrontendLog).toHaveBeenCalledWith(
      'info',
      'frontend api request completed',
      expect.objectContaining({
        component: 'api_client',
        operation: 'claim_testnet_faucet',
        outcome: 'success',
        duration_ms: 31_000,
        timeout_ms: TESTNET_FAUCET_TIMEOUT_MS,
      })
    );
  });

  it('returns an actionable faucet message and structured timeout diagnostics', async () => {
    vi.useFakeTimers();
    vi.stubGlobal('fetch', vi.fn((
      _input: RequestInfo | URL,
      init?: RequestInit
    ) => new Promise<Response>((_resolve, reject) => {
      init?.signal?.addEventListener('abort', () => {
        reject(init.signal?.reason);
      });
    })));
    const client = new PlethApiClient({ baseUrl: '/api' });

    const pendingClaim = client.claimTestnetFaucet(
      '0x1111111111111111111111111111111111111111'
    );
    await vi.advanceTimersByTimeAsync(TESTNET_FAUCET_TIMEOUT_MS);
    const result = await pendingClaim;

    expect(Result.isError(result)).toBe(true);
    if (!Result.isError(result)) return;
    expect(result.error).toMatchObject({
      code: 'NETWORK_ERROR',
      message: TESTNET_FAUCET_TIMEOUT_MESSAGE,
    });
    expect(analyticsMock.captureFrontendLog).toHaveBeenCalledWith(
      'error',
      'frontend api request failed',
      expect.objectContaining({
        component: 'api_client',
        operation: 'claim_testnet_faucet',
        outcome: 'failure',
        error_category: 'timeout',
        reason_code: 'client_timeout',
        duration_ms: TESTNET_FAUCET_TIMEOUT_MS,
        timeout_ms: TESTNET_FAUCET_TIMEOUT_MS,
      })
    );
  });
});

describe('faucet compatibility errors', () => {
  it('parses UPGRADE_REQUIRED and presents an explicit refresh message', async () => {
    vi.stubGlobal('fetch', vi.fn(async () => new Response(JSON.stringify({
      error: {
        code: 'UPGRADE_REQUIRED',
        message: 'legacy compatibility response',
      },
    }), {
      status: 426,
      headers: { 'Content-Type': 'application/json' },
    })));
    const client = new PlethApiClient({ baseUrl: '/api' });

    const result = await client.claimTestnetFaucet(
      '0x1111111111111111111111111111111111111111'
    );

    expect(Result.isError(result)).toBe(true);
    if (!Result.isError(result)) return;
    expect(result.error).toMatchObject({
      code: 'UPGRADE_REQUIRED',
      status: 426,
    });
    expect(testnetFaucetErrorMessage(result.error))
      .toBe(TESTNET_FAUCET_UPGRADE_REQUIRED_MESSAGE);
  });
});

describe('Perps query requests', () => {
  it('forwards caller cancellation without recording an API failure', async () => {
    const fetchMock = vi.fn((
      _input: RequestInfo | URL,
      init?: RequestInit
    ) => new Promise<Response>((_resolve, reject) => {
      init?.signal?.addEventListener('abort', () => {
        reject(init.signal?.reason);
      });
    }));
    vi.stubGlobal('fetch', fetchMock);
    const client = new PlethApiClient({ baseUrl: '/api' });
    const controller = new AbortController();

    const pendingRequest = client.getPerpsBasketLatest(controller.signal);
    controller.abort();

    await expect(pendingRequest).rejects.toMatchObject({ name: 'AbortError' });
    expect(analyticsMock.captureFrontendLog).not.toHaveBeenCalled();
  });

  it('forwards cancellation while an error response body is being decoded', async () => {
    let rejectBody: ((reason?: unknown) => void) | undefined;
    const fetchMock = vi.fn(async () => ({
      ok: false,
      status: 502,
      statusText: 'Bad Gateway',
      headers: new Headers({ 'Content-Type': 'application/json' }),
      json: () => new Promise<unknown>((_resolve, reject) => {
        rejectBody = reject;
      }),
    }) as Response);
    vi.stubGlobal('fetch', fetchMock);
    const client = new PlethApiClient({ baseUrl: '/api' });
    const controller = new AbortController();

    const pendingRequest = client.getPerpsBasketLatest(controller.signal);
    await vi.waitFor(() => expect(rejectBody).toBeDefined());
    controller.abort();
    rejectBody?.(controller.signal.reason);

    await expect(pendingRequest).rejects.toMatchObject({ name: 'AbortError' });
    expect(analyticsMock.captureFrontendLog).not.toHaveBeenCalled();
  });

  it('does not add a JSON content type to bodyless GET requests', async () => {
    const fetchMock = vi.fn(async () => new Response(JSON.stringify({
      data: {
        timestamp: 1,
        basketPrice: '100000000',
        components: [],
        generatedAt: 1,
        source: 'test',
      },
      meta: {
        blockNumber: 0,
        chainId: 421614,
        cached: false,
      },
    }), {
      status: 200,
      headers: { 'Content-Type': 'application/json' },
    }));
    vi.stubGlobal('fetch', fetchMock);
    const client = new PlethApiClient({ baseUrl: '/api' });

    await client.getPerpsBasketLatest();

    const init = fetchMock.mock.calls[0]?.[1] as RequestInit | undefined;
    expect(new Headers(init?.headers).has('Content-Type')).toBe(false);
    expect(init?.credentials).toBe('omit');
  });

  it('requests fixed candle pages and the mutable current candle anonymously', async () => {
    const fetchMock = vi.fn(async (input: RequestInfo | URL) => new Response(JSON.stringify({
      data: String(input).includes('/current')
        ? {
            intervalSeconds: 300,
            seriesId: 'dxy-v1',
            configurationHash: 'sha256:test-configuration',
            displayPriceCap: '200000000',
            volumeChainId: 421_614,
            volumeRouter: '0x1111111111111111111111111111111111111111',
            volumeCoverageStart: 1_650_000_000,
            volumeCoverageEnd: 1_700_000_000,
            volumeFinalizedThrough: 1_700_000_000,
            volumeCoverageComplete: true,
            datasetGeneration: 7,
            coverageStart: 1_600_000_000,
            coverageEnd: 1_700_000_000,
            coverageComplete: true,
            finalizedThrough: 1_700_000_000,
            candle: null,
          }
        : {
            intervalSeconds: 300,
            cursor: 1_700_100_000,
            seriesId: 'dxy-v1',
            configurationHash: 'sha256:test-configuration',
            displayPriceCap: '200000000',
            volumeChainId: 421_614,
            volumeRouter: '0x1111111111111111111111111111111111111111',
            volumeCoverageStart: 1_650_000_000,
            volumeCoverageEnd: 1_700_100_000,
            volumeFinalizedThrough: 1_700_000_000,
            volumeCoverageComplete: true,
            previousCursor: 1_699_950_000,
            hasEarlier: true,
            coverageStart: 1_600_000_000,
            coverageEnd: 1_700_100_000,
            coverageComplete: true,
            finalizedThrough: 1_700_000_000,
            datasetGeneration: 7,
            candles: [],
          },
      meta: { blockNumber: 0, chainId: 421614, cached: false },
    }), {
      status: 200,
      headers: { 'Content-Type': 'application/json' },
    }));
    vi.stubGlobal('fetch', fetchMock);
    const client = new PlethApiClient({ baseUrl: '/api' });
    const controller = new AbortController();

    await client.getPerpsBasketCandles(300, 1_700_100_000, controller.signal);
    await client.getPerpsBasketCurrentCandle(300, controller.signal);

    expect(fetchMock.mock.calls.map(([input]) => String(input))).toEqual([
      '/api/perps/basket/candles?interval=300&cursor=1700100000',
      '/api/perps/basket/candles/current?interval=300',
    ]);
    for (const [, init] of fetchMock.mock.calls) {
      expect(init).toMatchObject({ credentials: 'omit', signal: expect.any(AbortSignal) });
      expect(new Headers((init as RequestInit | undefined)?.headers).has('Content-Type')).toBe(false);
    }
  });

  it('requests the fixed seven-day vault history anonymously', async () => {
    const fetchMock = vi.fn(async () => new Response(JSON.stringify({
      data: {
        range: '7d',
        intervalSeconds: 3600,
        deployment: {
          chainId: 421_614,
          housePool: '0x1111111111111111111111111111111111111111',
          seniorVault: '0x2222222222222222222222222222222222222222',
          juniorVault: '0x3333333333333333333333333333333333333333',
        },
        coverage: { start: null, end: null, complete: false },
        senior: { apy7d: null, return7d: null, points: [] },
        junior: { apy7d: null, return7d: null, points: [] },
      },
      meta: { blockNumber: 0, chainId: 421614, cached: false },
    }), {
      status: 200,
      headers: { 'Content-Type': 'application/json' },
    }));
    vi.stubGlobal('fetch', fetchMock);
    const client = new PlethApiClient({ baseUrl: '/api' });
    const controller = new AbortController();

    await client.getPerpsVaultHistory(controller.signal);

    expect(fetchMock).toHaveBeenCalledWith(
      '/api/perps/vaults/history?range=7d&interval=3600',
      expect.objectContaining({
        credentials: 'omit',
        signal: expect.any(AbortSignal),
      })
    );
    const init = fetchMock.mock.calls[0]?.[1] as RequestInit | undefined;
    expect(new Headers(init?.headers).has('Content-Type')).toBe(false);
  });

  it('can force candle generation recovery through HTTP revalidation', async () => {
    const fetchMock = vi.fn(async () => new Response(JSON.stringify({
      data: {},
      meta: { blockNumber: 0, chainId: 421614, cached: false },
    }), {
      status: 200,
      headers: { 'Content-Type': 'application/json' },
    }));
    vi.stubGlobal('fetch', fetchMock);
    const client = new PlethApiClient({ baseUrl: '/api' });

    await client.getPerpsBasketCandles(60, 90_000, undefined, true);
    await client.getPerpsBasketCurrentCandle(60, undefined, true);

    for (const [, init] of fetchMock.mock.calls) {
      expect(init?.cache).toBe('no-cache');
      expect(init?.credentials).toBe('omit');
    }
  });
});
