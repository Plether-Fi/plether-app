/**
 * React Query hooks for the Plether API
 *
 * These hooks wrap the API client with TanStack Query for
 * automatic caching, refetching, and state management.
 */

import { useQuery, useInfiniteQuery, useQueryClient } from '@tanstack/react-query';
import { useEffect, useState, useCallback, useRef, useSyncExternalStore } from 'react';
import { useChainId } from 'wagmi';
import { Result } from 'better-result';
import { plethApi, PlethApiError } from './client';
import type {
  Side,
  ZapDirection,
  TradeFrom,
  HistoryParams,
  AllowancesParams,
  ApiResponse,
  PricesMessage,
  WebSocketMessage,
} from './types';

// =============================================================================
// Query Keys
// =============================================================================

let currentChainId: number | undefined;

export const apiQueryKeys = {
  protocol: {
    all: () => ['protocol', currentChainId] as const,
    status: () => [...apiQueryKeys.protocol.all(), 'status'] as const,
    config: () => [...apiQueryKeys.protocol.all(), 'config'] as const,
  },
  user: {
    all: (address: string) => ['user', currentChainId, address] as const,
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
    all: () => ['quotes', currentChainId] as const,
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

// =============================================================================
// Protocol Hooks
// =============================================================================

export function useProtocolStatus() {
  return useQuery({
    queryKey: apiQueryKeys.protocol.status(),
    queryFn: async () => unwrapResult(await plethApi.getProtocolStatus()),
    staleTime: 30_000,
    refetchInterval: (query) => query.state.status === 'error' ? false : 30_000,
    retryDelay: (attemptIndex) => Math.min(1000 * 2 ** attemptIndex, 10_000),
  });
}

export function useProtocolConfig() {
  return useQuery({
    queryKey: apiQueryKeys.protocol.config(),
    queryFn: async () => unwrapResult(await plethApi.getProtocolConfig()),
    staleTime: 60 * 60 * 1000,
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
      return unwrapResult(await plethApi.getUserDashboard(address));
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
      return unwrapResult(await plethApi.getUserBalances(address));
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
      return unwrapResult(await plethApi.getUserPositions(address));
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
      return unwrapResult(await plethApi.getUserAllowances(address, params));
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
      return unwrapResult(await plethApi.getMintQuote(amount));
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
      return unwrapResult(await plethApi.getBurnQuote(amount));
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
      return unwrapResult(await plethApi.getZapQuote(direction, amount));
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
      return unwrapResult(await plethApi.getTradeQuote(from, amount));
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
      return unwrapResult(await plethApi.getLeverageQuote(side, principal, leverage));
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
      const result = await plethApi.getTransactionHistory(address, {
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
      const result = await plethApi.getLeverageHistory(address, {
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
      const result = await plethApi.getLendingHistory(address, {
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
// Chain Sync
// =============================================================================

export function useApiChainSync(): void {
  const chainId = useChainId();
  const queryClient = useQueryClient();
  const prevChainId = useRef(chainId);

  useEffect(() => {
    if (chainId === prevChainId.current) return;
    prevChainId.current = chainId;
    currentChainId = chainId;
    plethApi.setChainId(chainId);
    void queryClient.invalidateQueries();
  }, [chainId, queryClient]);

  useEffect(() => {
    currentChainId = chainId;
    plethApi.setChainId(chainId);
  }, []); // eslint-disable-line react-hooks/exhaustive-deps
}

// =============================================================================
// WebSocket Hooks
// =============================================================================

export function useWebSocketPrices(enabled = true) {
  const [prices, setPrices] = useState<PricesMessage['data'] | null>(null);

  useEffect(() => {
    if (!enabled) return;

    plethApi.connectWebSocket();
    const unsubscribe = plethApi.subscribeToPrices(setPrices);

    return () => {
      unsubscribe();
    };
  }, [enabled]);

  return prices;
}

function subscribeToConnection(callback: () => void) {
  return plethApi.onMessage(callback);
}

function getConnectionSnapshot() {
  return plethApi.isWebSocketConnected;
}

export function useWebSocketConnection(address?: string) {
  const isConnected = useSyncExternalStore(subscribeToConnection, getConnectionSnapshot);
  const [lastMessage, setLastMessage] = useState<WebSocketMessage | null>(null);
  const addressRef = useRef(address);

  useEffect(() => {
    addressRef.current = address;
  }, [address]);

  const connect = useCallback(() => {
    plethApi.connectWebSocket(addressRef.current);
  }, []);

  const disconnect = useCallback(() => {
    plethApi.disconnectWebSocket();
  }, []);

  const subscribe = useCallback((userAddress: string) => {
    plethApi.subscribeToUser(userAddress);
  }, []);

  const unsubscribe = useCallback(() => {
    plethApi.unsubscribeFromUser();
  }, []);

  useEffect(() => {
    const unsub = plethApi.onMessage(setLastMessage);
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
