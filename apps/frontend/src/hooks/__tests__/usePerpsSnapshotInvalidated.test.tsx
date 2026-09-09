import { act, renderHook } from '@testing-library/react'
import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import { hashFn } from '@wagmi/core/query'
import { type PropsWithChildren } from 'react'
import { expect, it } from 'vitest'
import { usePerpsSnapshotInvalidated } from '../usePerpsSnapshotInvalidated'

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
