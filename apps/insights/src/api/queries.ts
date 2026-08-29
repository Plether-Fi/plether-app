import { useInfiniteQuery, useQuery } from '@tanstack/react-query'
import {
  getCurrentCompetition,
  getLeaderboard,
  getRegistrationSession,
  getStatus,
  getWallet,
} from './client'

export const queryKeys = {
  competition: ['insights', 'competition', 'current'] as const,
  leaderboard: (slug: string, search: string) => ['insights', 'leaderboard', slug, search] as const,
  wallet: (slug: string, address: string) => ['insights', 'wallet', slug, address] as const,
  registration: (slug: string) => ['insights', 'registration', slug] as const,
  status: ['insights', 'status'] as const,
}

export function useCurrentCompetition() {
  return useQuery({
    queryKey: queryKeys.competition,
    queryFn: ({ signal }) => getCurrentCompetition(signal),
    // Registration opens at deployment time and closes on a half-open UTC
    // boundary. Keep this metadata out of long-lived client caches so the CTA
    // cannot remain open while the mutation endpoints are already closed.
    staleTime: 0,
    refetchInterval: (query) => query.state.data?.registration?.status === 'open' ? 1_000 : 30_000,
    refetchIntervalInBackground: false,
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

export function useRegistrationSession(slug: string) {
  return useQuery({
    queryKey: queryKeys.registration(slug),
    queryFn: ({ signal }) => getRegistrationSession(slug, signal),
    enabled: slug.length > 0,
    retry: false,
    staleTime: 0,
  })
}
