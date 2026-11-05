import { focusManager, QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { act, renderHook, waitFor } from '@testing-library/react';
import { Result } from 'better-result';
import type { PropsWithChildren } from 'react';
import { afterEach, describe, expect, it, vi } from 'vitest';

import { perpsApi, PlethApiError } from './client';
import {
  apiQueryKeys,
  usePerpsVaultHistory,
  VAULT_HISTORY_QUERY_POLICY,
} from './hooks';
import type { ApiResponse, VaultHistory } from './types';

const completeHistory: ApiResponse<VaultHistory> = {
  data: {
    range: '7d',
    intervalSeconds: 3600,
    deployment: {
      chainId: 421_614,
      housePool: '0x1111111111111111111111111111111111111111',
      seniorVault: '0x2222222222222222222222222222222222222222',
      juniorVault: '0x3333333333333333333333333333333333333333',
    },
    coverage: {
      start: 1_700_000_000,
      end: 1_700_604_800,
      complete: true,
    },
    senior: {
      apy7d: 0.0524,
      return7d: 0.00098,
      points: [{
        timestamp: 1_700_000_000,
        blockNumber: '1000000',
        markFresh: true,
        sharePrice: '1007500000000000000',
        totalAssets: '402670000000000',
        totalSupply: '399673697270',
      }],
    },
    junior: {
      apy7d: -0.125,
      return7d: -0.00256,
      points: [{
        timestamp: 1_700_000_000,
        blockNumber: '1000000',
        markFresh: true,
        sharePrice: '643400000000000000',
        totalAssets: '102920000000000',
        totalSupply: '159962698165',
      }],
    },
  },
  meta: {
    cached: false,
    blockNumber: 1_000_000,
    chainId: 421_614,
  },
};

function createWrapper(queryClient: QueryClient) {
  return function Wrapper({ children }: PropsWithChildren) {
    return <QueryClientProvider client={queryClient}>{children}</QueryClientProvider>;
  };
}

afterEach(() => {
  vi.restoreAllMocks();
});

describe('usePerpsVaultHistory', () => {
  it('shares the two-tranche query and automatically refreshes both consumers after sixty seconds', async () => {
    vi.useFakeTimers()
    focusManager.setFocused(true)
    const refreshedHistory = {
      ...completeHistory,
      data: { ...completeHistory.data, senior: { ...completeHistory.data.senior, apy7d: 0.06 } },
    }
    const getHistory = vi.spyOn(perpsApi, 'getPerpsVaultHistory')
      .mockResolvedValueOnce(Result.ok(completeHistory))
      .mockResolvedValue(Result.ok(refreshedHistory))
    const queryClient = new QueryClient({ defaultOptions: { queries: { retry: false } } })
    const { result, unmount } = renderHook(() => ({
      senior: usePerpsVaultHistory(), junior: usePerpsVaultHistory(),
    }), { wrapper: createWrapper(queryClient) })

    try {
      await act(async () => { await vi.advanceTimersByTimeAsync(0) })
      expect(result.current.senior.data).toEqual(completeHistory)
      expect(result.current.junior.data).toEqual(completeHistory)
      expect(apiQueryKeys.perps.vaultHistory()).toEqual(['perps', 'perps', 'vaultHistory', '7d', 3600])
      expect(VAULT_HISTORY_QUERY_POLICY).toEqual({ staleTime: 60_000, refetchInterval: 60_000 })
      expect(getHistory).toHaveBeenCalledTimes(1)

      await act(async () => { await vi.advanceTimersByTimeAsync(59_999) })
      expect(getHistory).toHaveBeenCalledTimes(1)
      await act(async () => { await vi.advanceTimersByTimeAsync(1) })
      expect(getHistory).toHaveBeenCalledTimes(2)
      // TanStack notifies React on a subsequent timer after the fetch resolves.
      await act(async () => { await vi.advanceTimersByTimeAsync(1) })
      expect(result.current.senior.data).toEqual(refreshedHistory)
      expect(result.current.junior.data).toEqual(refreshedHistory)
    } finally {
      unmount()
      queryClient.clear()
      focusManager.setFocused(undefined)
      vi.useRealTimers()
    }
  })

  it('requests and caches thirty-day assets separately from seven-day performance', async () => {
    const getHistory = vi.spyOn(perpsApi, 'getPerpsVaultHistory').mockResolvedValue(Result.ok(completeHistory));
    const queryClient = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    const { result } = renderHook(() => usePerpsVaultHistory('30d'), { wrapper: createWrapper(queryClient) });
    await waitFor(() => expect(result.current.isSuccess).toBe(true));
    expect(getHistory).toHaveBeenCalledWith(expect.any(AbortSignal), '30d');
    expect(queryClient.getQueryData(apiQueryKeys.perps.vaultHistory('30d'))).toEqual(completeHistory);
    expect(queryClient.getQueryData(apiQueryKeys.perps.vaultHistory())).toBeUndefined();
    queryClient.clear();
  });

  it('keeps the last complete deployment response when a refresh fails', async () => {
    const getHistory = vi.spyOn(perpsApi, 'getPerpsVaultHistory')
      .mockResolvedValueOnce(Result.ok(completeHistory))
      .mockResolvedValue(Result.err(new PlethApiError('NETWORK_ERROR', 'refresh failed')));
    const queryClient = new QueryClient({
      defaultOptions: { queries: { retry: false } },
    });
    const { result } = renderHook(() => usePerpsVaultHistory(), {
      wrapper: createWrapper(queryClient),
    });
    await waitFor(() => expect(result.current.isSuccess).toBe(true));

    await act(async () => {
      await result.current.refetch();
    });

    expect(getHistory).toHaveBeenCalledTimes(3);
    expect(result.current.data).toEqual(completeHistory);

    queryClient.clear();
  });

  it('replaces cached history when the backend reports a new deployment identity', async () => {
    const nextDeployment: ApiResponse<VaultHistory> = {
      ...completeHistory,
      data: {
        ...completeHistory.data,
        deployment: {
          ...completeHistory.data.deployment,
          housePool: '0x4444444444444444444444444444444444444444',
        },
        coverage: { start: null, end: null, complete: false },
        senior: { apy7d: null, return7d: null, points: [] },
        junior: { apy7d: null, return7d: null, points: [] },
      },
    };
    const getHistory = vi.spyOn(perpsApi, 'getPerpsVaultHistory')
      .mockResolvedValueOnce(Result.ok(completeHistory))
      .mockResolvedValueOnce(Result.ok(nextDeployment));
    const queryClient = new QueryClient({
      defaultOptions: { queries: { retry: false } },
    });
    const { result } = renderHook(() => usePerpsVaultHistory(), {
      wrapper: createWrapper(queryClient),
    });
    await waitFor(() => expect(result.current.isSuccess).toBe(true));
    expect(getHistory).toHaveBeenCalledTimes(1);

    let refreshedData: ApiResponse<VaultHistory> | undefined;
    await act(async () => {
      refreshedData = (await result.current.refetch()).data;
    });

    expect(getHistory).toHaveBeenCalledTimes(2);
    expect(refreshedData).toEqual(nextDeployment);
    expect(queryClient.getQueryData(apiQueryKeys.perps.vaultHistory()))
      .toEqual(nextDeployment);

    queryClient.clear();
  });

  it('accepts a successful incomplete response for the same deployment', async () => {
    const incompleteRefresh: ApiResponse<VaultHistory> = {
      ...completeHistory,
      data: {
        ...completeHistory.data,
        coverage: { start: null, end: null, complete: false },
        senior: { apy7d: null, return7d: null, points: [] },
        junior: { apy7d: null, return7d: null, points: [] },
      },
    };
    const getHistory = vi.spyOn(perpsApi, 'getPerpsVaultHistory')
      .mockResolvedValueOnce(Result.ok(completeHistory))
      .mockResolvedValueOnce(Result.ok(incompleteRefresh));
    const queryClient = new QueryClient({
      defaultOptions: { queries: { retry: false } },
    });
    const { result } = renderHook(() => usePerpsVaultHistory(), {
      wrapper: createWrapper(queryClient),
    });
    await waitFor(() => expect(result.current.isSuccess).toBe(true));
    expect(result.current.data).toEqual(completeHistory);

    await act(async () => {
      await result.current.refetch();
    });

    expect(getHistory).toHaveBeenCalledTimes(2);
    await waitFor(() => expect(result.current.data).toEqual(incompleteRefresh));
    expect(queryClient.getQueryData(apiQueryKeys.perps.vaultHistory()))
      .toEqual(incompleteRefresh);

    queryClient.clear();
  });
});
