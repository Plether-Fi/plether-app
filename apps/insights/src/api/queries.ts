import { useInfiniteQuery, useQuery } from '@tanstack/react-query'
import {
  getCurrentCompetition,
  getCurrentProtocolRelease,
  getHousePool,
  getKeeper,
  getKeepers,
  getLeaderboard,
  getParameterChanges,
  getParameters,
  getProtocolOrder,
  getProtocolOverview,
  getProtocolTransaction,
  getProtocolTransactions,
  getProtocolWallet,
  getProtocolWallets,
  getStatus,
  getTranche,
  getTrancheHistory,
  getWallet,
  type ProtocolTransactionsParams,
} from './client'

export const queryKeys = {
  competition: ['insights', 'competition', 'current'] as const,
  leaderboard: (slug: string, search: string) => ['insights', 'leaderboard', slug, search] as const,
  wallet: (slug: string, address: string) => ['insights', 'wallet', slug, address] as const,
  status: ['insights', 'status'] as const,
  release: ['insights', 'protocol', 'release', 'current'] as const,
  overview: (releaseId: string) => ['insights', 'protocol', releaseId, 'overview'] as const,
  transactions: (releaseId: string, filters: string) => ['insights', 'protocol', releaseId, 'transactions', filters] as const,
  transaction: (releaseId: string, txHash: string) => ['insights', 'protocol', releaseId, 'transaction', txHash] as const,
  order: (releaseId: string, orderId: string) => ['insights', 'protocol', releaseId, 'order', orderId] as const,
  housePool: (releaseId: string) => ['insights', 'protocol', releaseId, 'house-pool'] as const,
  tranche: (releaseId: string, tranche: string) => ['insights', 'protocol', releaseId, 'tranche', tranche] as const,
  trancheHistory: (releaseId: string, tranche: string, limit = 500) =>
    ['insights', 'protocol', releaseId, 'tranche', tranche, 'history', limit] as const,
  keepers: (releaseId: string, window: string, limit = 100) =>
    ['insights', 'protocol', releaseId, 'keepers', window, limit] as const,
  keeper: (releaseId: string, address: string, window: string, limit = 100) =>
    ['insights', 'protocol', releaseId, 'keeper', address.toLowerCase(), window, limit] as const,
  protocolWallets: (releaseId: string, window: string, limit = 100) =>
    ['insights', 'protocol', releaseId, 'wallets', window, limit] as const,
  protocolWallet: (releaseId: string, address: string, window: string, limit = 100) =>
    ['insights', 'protocol', releaseId, 'wallet', address.toLowerCase(), window, limit] as const,
  parameters: (releaseId: string) => ['insights', 'protocol', releaseId, 'parameters'] as const,
  parameterChanges: (releaseId: string, limit = 200) =>
    ['insights', 'protocol', releaseId, 'parameter-changes', limit] as const,
}

export function useCurrentCompetition() {
  return useQuery({
    queryKey: queryKeys.competition,
    queryFn: ({ signal }) => getCurrentCompetition(signal),
    staleTime: 60_000,
  })
}

export function useLeaderboard(slug: string, search: string) {
  return useInfiniteQuery({
    queryKey: queryKeys.leaderboard(slug, search),
    queryFn: ({ pageParam, signal }) =>
      getLeaderboard(slug, { limit: 50, cursor: pageParam, search: search || undefined, signal }),
    initialPageParam: undefined as string | undefined,
    getNextPageParam: (lastPage) => lastPage.nextCursor ?? undefined,
    staleTime: 15_000,
  })
}

export function useWallet(slug: string, address: string) {
  return useQuery({
    queryKey: queryKeys.wallet(slug, address.toLowerCase()),
    queryFn: ({ signal }) => getWallet(slug, address, signal),
    enabled: slug.length > 0 && address.length > 0,
    staleTime: 15_000,
  })
}

export function useInsightsStatus() {
  return useQuery({
    queryKey: queryKeys.status,
    queryFn: ({ signal }) => getStatus(signal),
    refetchInterval: 60_000,
    refetchIntervalInBackground: false,
    staleTime: 60_000,
  })
}

export function useCurrentProtocolRelease() {
  return useQuery({
    queryKey: queryKeys.release,
    queryFn: ({ signal }) => getCurrentProtocolRelease(signal),
    staleTime: 5_000,
    refetchInterval: 30_000,
    refetchIntervalInBackground: false,
  })
}

export function useProtocolOverview(releaseId: string) {
  return useQuery({
    queryKey: queryKeys.overview(releaseId),
    queryFn: ({ signal }) => getProtocolOverview(releaseId, signal),
    enabled: releaseId.length > 0,
    staleTime: 15_000,
  })
}

export function useProtocolTransactions(
  releaseId: string,
  params: Omit<ProtocolTransactionsParams, 'signal' | 'cursor'>,
  filterKey: string,
) {
  return useInfiniteQuery({
    queryKey: queryKeys.transactions(releaseId, filterKey),
    queryFn: ({ pageParam, signal }) =>
      getProtocolTransactions(releaseId, { ...params, cursor: pageParam, signal }),
    initialPageParam: undefined as string | undefined,
    getNextPageParam: (lastPage) => lastPage.transactions.nextCursor ?? undefined,
    enabled: releaseId.length > 0,
    staleTime: 10_000,
  })
}

export function useProtocolTransaction(releaseId: string, txHash: string) {
  return useQuery({
    queryKey: queryKeys.transaction(releaseId, txHash),
    queryFn: ({ signal }) => getProtocolTransaction(releaseId, txHash, signal),
    enabled: releaseId.length > 0 && txHash.length > 0,
    staleTime: 15_000,
  })
}

export function useProtocolOrder(releaseId: string, orderId: string) {
  return useQuery({
    queryKey: queryKeys.order(releaseId, orderId),
    queryFn: ({ signal }) => getProtocolOrder(releaseId, orderId, signal),
    enabled: releaseId.length > 0 && orderId.length > 0,
    staleTime: 15_000,
  })
}

export function useHousePool(releaseId: string) {
  return useQuery({
    queryKey: queryKeys.housePool(releaseId),
    queryFn: ({ signal }) => getHousePool(releaseId, signal),
    enabled: releaseId.length > 0,
    staleTime: 15_000,
  })
}

export function useTranche(releaseId: string, tranche: string) {
  return useQuery({
    queryKey: queryKeys.tranche(releaseId, tranche),
    queryFn: ({ signal }) => getTranche(releaseId, tranche, signal),
    enabled: releaseId.length > 0 && (tranche === 'senior' || tranche === 'junior'),
    staleTime: 15_000,
  })
}

export function useTrancheHistory(releaseId: string, tranche: string, limit = 500) {
  return useInfiniteQuery({
    queryKey: queryKeys.trancheHistory(releaseId, tranche, limit),
    queryFn: ({ pageParam, signal }) =>
      getTrancheHistory(releaseId, tranche, { limit, cursor: pageParam, signal }),
    initialPageParam: undefined as string | undefined,
    getNextPageParam: (lastPage) => lastPage.history.nextCursor ?? undefined,
    enabled: releaseId.length > 0 && (tranche === 'senior' || tranche === 'junior'),
    staleTime: 30_000,
  })
}

export function useKeepers(releaseId: string, window: string, limit = 100) {
  return useInfiniteQuery({
    queryKey: queryKeys.keepers(releaseId, window, limit),
    queryFn: ({ pageParam, signal }) =>
      getKeepers(releaseId, { window, limit, cursor: pageParam, signal }),
    initialPageParam: undefined as string | undefined,
    getNextPageParam: (lastPage) => lastPage.keepers.nextCursor ?? undefined,
    enabled: releaseId.length > 0,
    staleTime: 30_000,
  })
}

export function useKeeper(releaseId: string, address: string, window: string, limit = 100) {
  return useInfiniteQuery({
    queryKey: queryKeys.keeper(releaseId, address, window, limit),
    queryFn: ({ pageParam, signal }) =>
      getKeeper(releaseId, address, { window, limit, cursor: pageParam, signal }),
    initialPageParam: undefined as string | undefined,
    getNextPageParam: (lastPage) => lastPage.keeper.nextCursor ?? undefined,
    enabled: releaseId.length > 0 && address.length > 0,
    staleTime: 30_000,
  })
}

export function useProtocolWallets(releaseId: string, window: string, limit = 100) {
  return useInfiniteQuery({
    queryKey: queryKeys.protocolWallets(releaseId, window, limit),
    queryFn: ({ pageParam, signal }) =>
      getProtocolWallets(releaseId, { window, limit, cursor: pageParam, signal }),
    initialPageParam: undefined as string | undefined,
    getNextPageParam: (lastPage) => lastPage.wallets.nextCursor ?? undefined,
    enabled: releaseId.length > 0,
    staleTime: 15_000,
  })
}

export function useProtocolWallet(
  releaseId: string,
  address: string,
  window: string,
  limit = 100,
) {
  return useInfiniteQuery({
    queryKey: queryKeys.protocolWallet(releaseId, address, window, limit),
    queryFn: ({ pageParam, signal }) =>
      getProtocolWallet(releaseId, address, { window, limit, cursor: pageParam, signal }),
    initialPageParam: undefined as string | undefined,
    getNextPageParam: (lastPage) => lastPage.wallet.nextCursor ?? undefined,
    enabled: releaseId.length > 0 && address.length > 0,
    staleTime: 15_000,
  })
}

export function useParameters(releaseId: string) {
  return useQuery({
    queryKey: queryKeys.parameters(releaseId),
    queryFn: ({ signal }) => getParameters(releaseId, signal),
    enabled: releaseId.length > 0,
    staleTime: 30_000,
  })
}

export function useParameterChanges(releaseId: string, limit = 200) {
  return useInfiniteQuery({
    queryKey: queryKeys.parameterChanges(releaseId, limit),
    queryFn: ({ pageParam, signal }) =>
      getParameterChanges(releaseId, { limit, cursor: pageParam, signal }),
    initialPageParam: undefined as string | undefined,
    getNextPageParam: (lastPage) => lastPage.parameterChanges.nextCursor ?? undefined,
    enabled: releaseId.length > 0,
    staleTime: 30_000,
  })
}
