import { act, renderHook } from '@testing-library/react'
import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import { hashFn } from '@wagmi/core/query'
import { type PropsWithChildren } from 'react'
import { expect, it } from 'vitest'
import { useInvalidatePerpsSnapshot, usePerpsSnapshotInvalidated } from '../usePerpsSnapshotInvalidated'

it('tracks explicit invalidation until a successful replacement snapshot, including bigint keys', async () => {
  const client = new QueryClient({ defaultOptions: { queries: { queryKeyHashFn: hashFn } } })
  const queryKey = ['readContracts', { args: [100n] }]
  client.setQueryData(queryKey, 'snapshot')
  const wrapper = ({ children }: PropsWithChildren) => <QueryClientProvider client={client}>{children}</QueryClientProvider>
  const { result } = renderHook(() => usePerpsSnapshotInvalidated(queryKey), { wrapper })
  expect(result.current).toBe(false)
  await act(async () => { await client.invalidateQueries({ queryKey, refetchType: 'none' }) })
  expect(result.current).toBe(true)
  act(() => { client.setQueryData(queryKey, 'new snapshot') })
  expect(result.current).toBe(false)
  client.clear()
})

it('reads and invalidates bigint snapshots in a mixed cache without touching other queries', async () => {
  const client = new QueryClient()
  client.setQueryData(['market'], 'unrelated')
  const queryKey = ['readContracts', { args: [100n] }]
  const otherKey = ['readContracts', { args: [200n] }]
  await client.fetchQuery({ queryKey, queryKeyHashFn: hashFn, queryFn: () => 'snapshot' })
  await client.fetchQuery({ queryKey: otherKey, queryKeyHashFn: hashFn, queryFn: () => 'other account' })
  const wrapper = ({ children }: PropsWithChildren) => <QueryClientProvider client={client}>{children}</QueryClientProvider>
  const { result, rerender } = renderHook(({ key }) => ({
    invalidated: usePerpsSnapshotInvalidated(key),
    invalidate: useInvalidatePerpsSnapshot(key),
  }), { wrapper, initialProps: { key: queryKey } })
  expect(result.current.invalidated).toBe(false)
  await act(async () => { await result.current.invalidate() })
  expect(result.current.invalidated).toBe(true)
  expect(client.getQueryCache().get(hashFn(['market']))?.state.isInvalidated).toBe(false)
  expect(client.getQueryCache().get(hashFn(otherKey))?.state.isInvalidated).toBe(false)
  rerender({ key: otherKey })
  expect(result.current.invalidated).toBe(false)
  rerender({ key: queryKey })
  expect(result.current.invalidated).toBe(true)
  await act(async () => {
    await client.fetchQuery({ queryKey, queryKeyHashFn: hashFn, queryFn: () => 'new snapshot' })
  })
  expect(result.current.invalidated).toBe(false)
  client.clear()
})
