import { useInfiniteQuery, useQuery } from '@tanstack/react-query'
import {
  getCurrentCompetition,
  getLeaderboard,
  getStatus,
  getWallet,
} from './client'

export const queryKeys = {
  competition: ['insights', 'competition', 'current'] as const,
  leaderboard: (slug: string, search: string) => ['insights', 'leaderboard', slug, search] as const,
  wallet: (slug: string, address: string) => ['insights', 'wallet', slug, address] as const,
  status: ['insights', 'status'] as const,
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
