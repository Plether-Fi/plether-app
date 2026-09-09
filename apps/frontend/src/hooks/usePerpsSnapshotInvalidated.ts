import { useCallback, useSyncExternalStore } from 'react'
import { useQueryClient, type QueryKey } from '@tanstack/react-query'

/** Invalidated snapshots must not authorize risk-sensitive actions while refetching. */
export function usePerpsSnapshotInvalidated(queryKey: QueryKey): boolean {
  const client = useQueryClient()
  const cache = client.getQueryCache()
  const subscribe = useCallback((onChange: () => void) => cache.subscribe(onChange), [cache])
  const snapshot = useCallback(() => cache.find({ queryKey, exact: true })?.state.isInvalidated ?? false, [cache, queryKey])
  return useSyncExternalStore(subscribe, snapshot, snapshot)
}

export function useInvalidatePerpsSnapshot(queryKey: QueryKey) {
  const client = useQueryClient()
  return useCallback(() => client.invalidateQueries({ queryKey, exact: true, refetchType: 'none' }), [client, queryKey])
}
