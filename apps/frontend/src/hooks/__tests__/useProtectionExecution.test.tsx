import { type PropsWithChildren } from 'react'
import { renderHook, waitFor } from '@testing-library/react'
import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest'
import { useProtectionExecution } from '../useProtectionHistory'

const mocks = vi.hoisted(() => ({
  account: '0x1111111111111111111111111111111111111111' as string | undefined,
  book: '0x63973Eb0B5a862dfc95348D4d575FC55C9546F04', fetch: vi.fn(),
}))
vi.mock('../../perps-aa', () => ({ usePerpsIdentity: () => ({ accountAddress: mocks.account, manifest: { positionProtectionBook: mocks.book } }) }))
function wrapper({ children }: PropsWithChildren) {
  return <QueryClientProvider client={queryClient}>{children}</QueryClientProvider>
}
let queryClient: QueryClient
const observation = { protectionId: '7', account: '0x1111111111111111111111111111111111111111', linkedOrderId: '19', protectionStatus: 8,
  reason: 'queue-congested', checkedBlock: '120', checkedBlockHash: '0x' + 'ab'.repeat(32), checkedAt: '2026-09-06T15:00:00Z', ageSeconds: 0 }
describe('protection execution query', () => {
  beforeEach(() => {
    queryClient = new QueryClient({ defaultOptions: { queries: { retry: false, gcTime: 0 } } })
    mocks.account = observation.account
    mocks.fetch.mockReset().mockResolvedValue(new Response(JSON.stringify({ data: { observation } }), { status: 200 }))
    vi.stubGlobal('fetch', mocks.fetch)
  })
  afterEach(() => { queryClient.clear(); vi.unstubAllGlobals() })
  it('fetches the release-scoped status without caching and validates the payload', async () => {
    const { result } = renderHook(() => useProtectionExecution(7n, true), { wrapper })
    await waitFor(() => { expect(result.current.isSuccess).toBe(true) })
    expect(mocks.fetch).toHaveBeenCalledWith(expect.stringContaining(`/perps/protections/7/execution?book=${mocks.book}`), expect.objectContaining({ cache: 'no-store', signal: expect.any(AbortSignal) }))
    expect(result.current.data?.observation).toEqual(observation)
  })
  it('does not fetch when the account is unconfirmed or the state is not eligible', () => {
    mocks.account = undefined
    const first = renderHook(() => useProtectionExecution(7n, true), { wrapper })
    expect(first.result.current.isFetching).toBe(false)
    first.unmount()
    mocks.account = observation.account
    renderHook(() => useProtectionExecution(7n, false), { wrapper })
    expect(mocks.fetch).not.toHaveBeenCalled()
  })
  it('does not carry an execution report into another account', async () => {
    const { result, rerender } = renderHook(() => useProtectionExecution(7n, true), { wrapper })
    await waitFor(() => { expect(result.current.isSuccess).toBe(true) })
    mocks.account = '0x2222222222222222222222222222222222222222'
    mocks.fetch.mockImplementation(() => new Promise(() => undefined))
    rerender()
    expect(result.current.data).toBeUndefined()
  })
})
