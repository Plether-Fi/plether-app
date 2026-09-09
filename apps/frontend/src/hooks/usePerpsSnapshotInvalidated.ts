import { useCallback, useSyncExternalStore } from 'react'
import { useQueryClient, type QueryKey } from '@tanstack/react-query'
import { hashFn } from 'wagmi/query'

/** Invalidated snapshots must not authorize risk-sensitive actions while refetching. */
export function usePerpsSnapshotInvalidated(queryKey: QueryKey): boolean {
  const client = useQueryClient()
  const cache = client.getQueryCache()
  const queryHash = hashFn(queryKey)
  const subscribe = useCallback((onChange: () => void) => cache.subscribe(onChange), [cache])
  // Exact key filters rehash the key with every candidate's hash function.
  // Unrelated API queries use JSON.stringify, which cannot serialize contract bigint args.
  const snapshot = useCallback(() => cache.get(queryHash)?.state.isInvalidated ?? false, [cache, queryHash])
  return useSyncExternalStore(subscribe, snapshot, snapshot)
}

export function useInvalidatePerpsSnapshot(queryKey: QueryKey) {
  const client = useQueryClient()
  const queryHash = hashFn(queryKey)
  return useCallback(() => client.invalidateQueries({
    predicate: (query) => query.queryHash === queryHash,
    refetchType: 'none',
  }), [client, queryHash])
}
