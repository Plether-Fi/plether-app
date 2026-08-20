/**
 * React Query hooks for the Plether API
 *
 * These hooks wrap the API client with TanStack Query for
 * automatic caching, refetching, and state management.
 */

import { useQuery, useInfiniteQuery } from '@tanstack/react-query';
import { useEffect, useState, useCallback, useRef, useSyncExternalStore } from 'react';
import { Result } from 'better-result';
import { perpsApi, spotApi, PlethApiError } from './client';
import { PERPS_CANDLE_CURRENT_POLL_INTERVAL_MS } from './candlePolicy';
import type {
  Side,
  ZapDirection,
  TradeFrom,
  HistoryParams,
  AllowancesParams,
  ApiResponse,
  PricesMessage,
  WebSocketMessage,
  BasketHistoryRange,
  PerpsCandleIntervalSeconds,
} from './types';

// =============================================================================
// Query Keys
// =============================================================================

const SPOT_API_SCOPE = 'spot';
const PERPS_API_SCOPE = 'perps';

export const apiQueryKeys = {
  protocol: {
    all: () => ['protocol', SPOT_API_SCOPE] as const,
    status: () => [...apiQueryKeys.protocol.all(), 'status'] as const,
    config: () => [...apiQueryKeys.protocol.all(), 'config'] as const,
  },
  perps: {
    all: () => ['perps', PERPS_API_SCOPE] as const,
    basketLatest: () => [...apiQueryKeys.perps.all(), 'basketLatest'] as const,
    basketHistoryAll: () => [...apiQueryKeys.perps.all(), 'basketHistory'] as const,
    basketHistory: (range: BasketHistoryRange, intervalSeconds: number, includeComponents = false) =>
      [...apiQueryKeys.perps.basketHistoryAll(), range, intervalSeconds, includeComponents] as const,
    basketCandlesAll: () => [...apiQueryKeys.perps.all(), 'basketCandles'] as const,
    basketCandles: (intervalSeconds: PerpsCandleIntervalSeconds, cursor: number) =>
      [...apiQueryKeys.perps.basketCandlesAll(), intervalSeconds, cursor] as const,
    basketCurrentCandle: (intervalSeconds: PerpsCandleIntervalSeconds) =>
      [...apiQueryKeys.perps.basketCandlesAll(), 'current', intervalSeconds] as const,
    marketStats: () => [...apiQueryKeys.perps.all(), 'marketStats'] as const,
  },
  user: {
    all: (address: string) => ['user', SPOT_API_SCOPE, address] as const,
    dashboard: (address: string) => [...apiQueryKeys.user.all(address), 'dashboard'] as const,
    balances: (address: string) => [...apiQueryKeys.user.all(address), 'balances'] as const,
    positions: (address: string) => [...apiQueryKeys.user.all(address), 'positions'] as const,
    allowances: (address: string, params?: AllowancesParams) =>
      [...apiQueryKeys.user.all(address), 'allowances', params] as const,
    history: (address: string, params?: HistoryParams) =>
      [...apiQueryKeys.user.all(address), 'history', params] as const,
    leverageHistory: (address: string, params?: { side?: Side }) =>
      [...apiQueryKeys.user.all(address), 'leverageHistory', params] as const,
    lendingHistory: (address: string, params?: { side?: Side }) =>
      [...apiQueryKeys.user.all(address), 'lendingHistory', params] as const,
  },
  quotes: {
    all: () => ['quotes', SPOT_API_SCOPE] as const,
    mint: (amount: string) => [...apiQueryKeys.quotes.all(), 'mint', amount] as const,
    burn: (amount: string) => [...apiQueryKeys.quotes.all(), 'burn', amount] as const,
    zap: (direction: ZapDirection, amount: string) =>
      [...apiQueryKeys.quotes.all(), 'zap', direction, amount] as const,
    trade: (from: TradeFrom, amount: string) =>
      [...apiQueryKeys.quotes.all(), 'trade', from, amount] as const,
    leverage: (side: Side, principal: string, leverage: string) =>
      [...apiQueryKeys.quotes.all(), 'leverage', side, principal, leverage] as const,
  },
} as const;

// =============================================================================
// Helper to unwrap Result
// =============================================================================

function unwrapResult<T>(result: Result<ApiResponse<T>, PlethApiError>): ApiResponse<T> {
  if (Result.isError(result)) {
    throw result.error;
  }
  return result.value;
}

function retryTransientFailureOnce(failureCount: number, error: unknown): boolean {
  const status = (error as { status?: number }).status;
  if (status !== undefined && status >= 400 && status < 500) return false;
  return failureCount < 1;
}

// =============================================================================
// Protocol Hooks
// =============================================================================

export function useProtocolStatus() {
  return useQuery({
    queryKey: apiQueryKeys.protocol.status(),
    queryFn: async () => unwrapResult(await spotApi.getProtocolStatus()),
    staleTime: 30_000,
    refetchInterval: (query) => query.state.status === 'error' ? false : 30_000,
    retryDelay: (attemptIndex) => Math.min(1000 * 2 ** attemptIndex, 10_000),
  });
}

export function useProtocolConfig() {
  return useQuery({
    queryKey: apiQueryKeys.protocol.config(),
    queryFn: async () => unwrapResult(await spotApi.getProtocolConfig()),
    staleTime: 60 * 60 * 1000,
  });
}

const GENERIC_HISTORY_STALE_MS = 60 * 1000;
const GENERIC_HISTORY_REFETCH_MS = 60 * 1000;
const GENERIC_HISTORY_ERROR_REFETCH_MS = 2 * 60 * 1000;
const COMPONENT_HISTORY_REFETCH_MS = 5 * 60 * 1000;

export function perpsBasketHistoryQueryPolicy(
  range: BasketHistoryRange,
  intervalSeconds: number,
  includeComponents: boolean
) {
  const isBoundedComponentHistory =
    includeComponents && range === '24h' && intervalSeconds === 60 * 60;
  return isBoundedComponentHistory
    ? {
        staleTimeMs: COMPONENT_HISTORY_REFETCH_MS,
        refetchIntervalMs: COMPONENT_HISTORY_REFETCH_MS,
        errorRefetchIntervalMs: COMPONENT_HISTORY_REFETCH_MS,
        retryTransientFailure: false,
      }
    : {
        staleTimeMs: GENERIC_HISTORY_STALE_MS,
        refetchIntervalMs: GENERIC_HISTORY_REFETCH_MS,
        errorRefetchIntervalMs: GENERIC_HISTORY_ERROR_REFETCH_MS,
        retryTransientFailure: true,
      };
}

export function usePerpsBasketHistory(
  range: BasketHistoryRange = '7d',
  intervalSeconds = 60 * 60,
  includeComponents = false
) {
  const policy = perpsBasketHistoryQueryPolicy(range, intervalSeconds, includeComponents);
  return useQuery({
    queryKey: apiQueryKeys.perps.basketHistory(range, intervalSeconds, includeComponents),
    queryFn: async ({ signal }) => unwrapResult(await perpsApi.getPerpsBasketHistory(
      range,
      intervalSeconds,
      includeComponents,
      signal
    )),
    staleTime: policy.staleTimeMs,
    refetchInterval: (query) => query.state.status === 'error'
      ? policy.errorRefetchIntervalMs
      : policy.refetchIntervalMs,
    retry: policy.retryTransientFailure ? retryTransientFailureOnce : false,
  });
}

export function usePerpsBasketLatest() {
  return useQuery({
    queryKey: apiQueryKeys.perps.basketLatest(),
    queryFn: async ({ signal }) => unwrapResult(await perpsApi.getPerpsBasketLatest(signal)),
    staleTime: 5 * 1000,
    refetchInterval: (query) => query.state.status === 'error' ? 15 * 1000 : 5 * 1000,
    retry: retryTransientFailureOnce,
    retryDelay: (attemptIndex) => Math.min(1000 * 2 ** attemptIndex, 10_000),
  });
}

export function usePerpsBasketCurrentCandle(intervalSeconds: PerpsCandleIntervalSeconds) {
  return useQuery({
    queryKey: apiQueryKeys.perps.basketCurrentCandle(intervalSeconds),
    queryFn: async ({ signal }) => unwrapResult(
      await perpsApi.getPerpsBasketCurrentCandle(intervalSeconds, signal)
    ),
    staleTime: 0,
    refetchInterval: (query) => query.state.status === 'error'
      ? 15 * 1000
      : PERPS_CANDLE_CURRENT_POLL_INTERVAL_MS,
    retry: retryTransientFailureOnce,
  });
}

export function usePerpsMarketStats() {
  return useQuery({
    queryKey: apiQueryKeys.perps.marketStats(),
    queryFn: async ({ signal }) => unwrapResult(await perpsApi.getPerpsMarketStats(signal)),
    staleTime: 30 * 1000,
    refetchInterval: (query) => query.state.status === 'error' ? 60 * 1000 : 30 * 1000,
    retry: retryTransientFailureOnce,
    retryDelay: (attemptIndex) => Math.min(1000 * 2 ** attemptIndex, 10_000),
  });
}

// =============================================================================
// User Hooks
// =============================================================================

export function useUserDashboard(address: string | undefined) {
  return useQuery({
    queryKey: apiQueryKeys.user.dashboard(address ?? ''),
    queryFn: async () => {
      if (!address) throw new Error('Address required');
      return unwrapResult(await spotApi.getUserDashboard(address));
    },
    enabled: !!address,
    staleTime: 12_000,
    refetchInterval: 12_000,
  });
}

export function useUserBalances(address: string | undefined) {
  return useQuery({
    queryKey: apiQueryKeys.user.balances(address ?? ''),
    queryFn: async () => {
      if (!address) throw new Error('Address required');
      return unwrapResult(await spotApi.getUserBalances(address));
    },
    enabled: !!address,
    staleTime: 12_000,
    refetchInterval: 12_000,
  });
}

export function useUserPositions(address: string | undefined) {
  return useQuery({
    queryKey: apiQueryKeys.user.positions(address ?? ''),
    queryFn: async () => {
      if (!address) throw new Error('Address required');
      return unwrapResult(await spotApi.getUserPositions(address));
    },
    enabled: !!address,
    staleTime: 12_000,
    refetchInterval: 12_000,
  });
}

export function useUserAllowances(address: string | undefined, params?: AllowancesParams) {
  return useQuery({
    queryKey: apiQueryKeys.user.allowances(address ?? '', params),
    queryFn: async () => {
      if (!address) throw new Error('Address required');
      return unwrapResult(await spotApi.getUserAllowances(address, params));
    },
    enabled: !!address,
    staleTime: 5 * 60 * 1000,
  });
}

// =============================================================================
// Quote Hooks
// =============================================================================

export function useMintQuote(amount: string | undefined) {
  return useQuery({
    queryKey: apiQueryKeys.quotes.mint(amount ?? ''),
    queryFn: async () => {
      if (!amount) throw new Error('Amount required');
      return unwrapResult(await spotApi.getMintQuote(amount));
    },
    enabled: !!amount && amount !== '0',
    staleTime: 5_000,
  });
}

export function useBurnQuote(amount: string | undefined) {
  return useQuery({
    queryKey: apiQueryKeys.quotes.burn(amount ?? ''),
    queryFn: async () => {
      if (!amount) throw new Error('Amount required');
      return unwrapResult(await spotApi.getBurnQuote(amount));
    },
    enabled: !!amount && amount !== '0',
    staleTime: 5_000,
  });
}

export function useZapQuote(direction: ZapDirection, amount: string | undefined) {
  return useQuery({
    queryKey: apiQueryKeys.quotes.zap(direction, amount ?? ''),
    queryFn: async () => {
      if (!amount) throw new Error('Amount required');
      return unwrapResult(await spotApi.getZapQuote(direction, amount));
    },
    enabled: !!amount && amount !== '0',
    staleTime: 5_000,
  });
}

export function useTradeQuote(from: TradeFrom, amount: string | undefined) {
  return useQuery({
    queryKey: apiQueryKeys.quotes.trade(from, amount ?? ''),
    queryFn: async () => {
      if (!amount) throw new Error('Amount required');
      return unwrapResult(await spotApi.getTradeQuote(from, amount));
    },
    enabled: !!amount && amount !== '0',
    staleTime: 5_000,
  });
}

export function useLeverageQuote(
  side: Side,
  principal: string | undefined,
  leverage: string | undefined
) {
  return useQuery({
    queryKey: apiQueryKeys.quotes.leverage(side, principal ?? '', leverage ?? ''),
    queryFn: async () => {
      if (!principal || !leverage) throw new Error('Principal and leverage required');
      return unwrapResult(await spotApi.getLeverageQuote(side, principal, leverage));
    },
    enabled: !!principal && principal !== '0' && !!leverage,
    staleTime: 5_000,
  });
}

// =============================================================================
// History Hooks (with infinite query for pagination)
// =============================================================================

export function useTransactionHistory(address: string | undefined, params?: HistoryParams) {
  return useInfiniteQuery({
    queryKey: apiQueryKeys.user.history(address ?? '', params),
    queryFn: async ({ pageParam }) => {
      if (!address) throw new Error('Address required');
      const result = await spotApi.getTransactionHistory(address, {
        ...params,
        page: pageParam,
      });
      return unwrapResult(result);
    },
    getNextPageParam: (lastPage) => {
      if (lastPage.data.pagination.hasMore) {
        return lastPage.data.pagination.page + 1;
      }
      return undefined;
    },
    initialPageParam: 1,
    enabled: !!address,
    staleTime: 60_000,
  });
}

export function useLeverageHistory(address: string | undefined, params?: { side?: Side }) {
  return useInfiniteQuery({
    queryKey: apiQueryKeys.user.leverageHistory(address ?? '', params),
    queryFn: async ({ pageParam }) => {
      if (!address) throw new Error('Address required');
      const result = await spotApi.getLeverageHistory(address, {
        ...params,
        page: pageParam,
      });
      return unwrapResult(result);
    },
    getNextPageParam: (lastPage) => {
      if (lastPage.data.pagination.hasMore) {
        return lastPage.data.pagination.page + 1;
      }
      return undefined;
    },
    initialPageParam: 1,
    enabled: !!address,
    staleTime: 60_000,
  });
}

export function useLendingHistory(address: string | undefined, params?: { side?: Side }) {
  return useInfiniteQuery({
    queryKey: apiQueryKeys.user.lendingHistory(address ?? '', params),
    queryFn: async ({ pageParam }) => {
      if (!address) throw new Error('Address required');
      const result = await spotApi.getLendingHistory(address, {
        ...params,
        page: pageParam,
      });
      return unwrapResult(result);
    },
    getNextPageParam: (lastPage) => {
      if (lastPage.data.pagination.hasMore) {
        return lastPage.data.pagination.page + 1;
      }
      return undefined;
    },
    initialPageParam: 1,
    enabled: !!address,
    staleTime: 60_000,
  });
}

// =============================================================================
// WebSocket Hooks
// =============================================================================

export function useWebSocketPrices(enabled = true) {
  const [prices, setPrices] = useState<PricesMessage['data'] | null>(null);

  useEffect(() => {
    if (!enabled) return;

    spotApi.connectWebSocket();
    const unsubscribe = spotApi.subscribeToPrices(setPrices);

    return () => {
      unsubscribe();
    };
  }, [enabled]);

  return prices;
}

function subscribeToConnection(callback: () => void) {
  return spotApi.onMessage(callback);
}

function getConnectionSnapshot() {
  return spotApi.isWebSocketConnected;
}

export function useWebSocketConnection(address?: string) {
  const isConnected = useSyncExternalStore(subscribeToConnection, getConnectionSnapshot);
  const [lastMessage, setLastMessage] = useState<WebSocketMessage | null>(null);
  const addressRef = useRef(address);

  useEffect(() => {
    addressRef.current = address;
  }, [address]);

  const connect = useCallback(() => {
    spotApi.connectWebSocket(addressRef.current);
  }, []);

  const disconnect = useCallback(() => {
    spotApi.disconnectWebSocket();
  }, []);

  const subscribe = useCallback((userAddress: string) => {
    spotApi.subscribeToUser(userAddress);
  }, []);

  const unsubscribe = useCallback(() => {
    spotApi.unsubscribeFromUser();
  }, []);

  useEffect(() => {
    const unsub = spotApi.onMessage(setLastMessage);
    return unsub;
  }, []);

  return {
    isConnected,
    lastMessage,
    connect,
    disconnect,
    subscribe,
    unsubscribe,
  };
}
