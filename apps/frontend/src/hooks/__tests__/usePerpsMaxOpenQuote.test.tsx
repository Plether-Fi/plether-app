import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import { renderHook, waitFor, act, cleanup } from '@testing-library/react'
import { useState, type PropsWithChildren } from 'react'
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest'
import { usePerpsMaxOpenQuote } from '../usePerpsMaxOpenQuote'

const mocks = vi.hoisted(() => ({ quotePerpsMaxOpen: vi.fn(), client: { readContract: vi.fn(), getBlockNumber: vi.fn() } }))
vi.mock('wagmi', () => ({ usePublicClient: () => mocks.client }))
vi.mock('../../contracts/perpsMaxOpenQuote', () => ({ quotePerpsMaxOpen: mocks.quotePerpsMaxOpen }))
type Input = Parameters<typeof usePerpsMaxOpenQuote>[0]
const account = '0x00000000000000000000000000000000000000a1' as const
const input: Input = { enabled: true, account, side: 1, availableAssets: 100_000_000n,
  oraclePrice: 100_000_000n, publishTime: 2_000_000_000, protectionRewardsAssets: 400_000n,
  selectedMaxLeverageBps: 100_000 }
const quantum = 100n * 10n ** 18n
const preview = { valid: true, invalidReason: 0, postSize: 9n * quantum, postEquityAssets: 99_000_000n }
const quote = { maxSizeDelta: 9n * quantum, preview, limitingReason: 9 }
const successful = { marginDelta: 99_400_000n, quote }
function queryClient() {
  return new QueryClient({ defaultOptions: { queries: { retry: false, gcTime: 0 } } })
}
function withClient(client: QueryClient) {
  return ({ children }: PropsWithChildren) => <QueryClientProvider client={client}>{children}</QueryClientProvider>
}
function Wrapper({ children }: PropsWithChildren) {
  const [client] = useState(queryClient)
  return <QueryClientProvider client={client}>{children}</QueryClientProvider>
}
function deferred<T>() {
  let resolve!: (value: T) => void
  const promise = new Promise<T>(done => { resolve = done })
  return { promise, resolve }
}

beforeEach(() => {
  mocks.client.readContract.mockReset()
  mocks.client.getBlockNumber.mockReset()
  mocks.quotePerpsMaxOpen.mockReset().mockResolvedValue(successful)
})
afterEach(() => {
  cleanup()
  vi.useRealTimers()
})

it('requests the leverage-constrained maximum without separate block or configuration RPCs', async () => {
  const { result } = renderHook(() => usePerpsMaxOpenQuote(input), { wrapper: Wrapper })
  await waitFor(() => expect(result.current.quote).toEqual(quote))
  expect(result.current.marginDelta).toBe(99_400_000n)
  expect(mocks.quotePerpsMaxOpen).toHaveBeenCalledExactlyOnceWith(mocks.client, {
    account, side: 1, availableAssets: 100_000_000n, oraclePrice: 100_000_000n,
    protectionRewardsAssets: 400_000n, selectedMaxLeverageBps: 100_000,
    signal: expect.any(AbortSignal),
  })
  expect(mocks.client.readContract).not.toHaveBeenCalled()
  expect(mocks.client.getBlockNumber).not.toHaveBeenCalled()
})

it.each<Partial<Input>>([
  { account: '0x00000000000000000000000000000000000000a2' },
  { side: 0 }, { availableAssets: 99_000_000n }, { oraclePrice: 100_001_000n },
  { protectionRewardsAssets: 0n }, { selectedMaxLeverageBps: 330_000 },
])('discards an earlier maximum when a risk input changes: %s', async override => {
  const client = queryClient()
  const pending = deferred<typeof successful>()
  const { result, rerender } = renderHook(props => usePerpsMaxOpenQuote(props), {
    initialProps: input, wrapper: withClient(client),
  })
  await waitFor(() => expect(result.current.quote).toEqual(quote))
  mocks.quotePerpsMaxOpen.mockReturnValueOnce(pending.promise)
  rerender({ ...input, ...override })
  expect(result.current.quote).toBeUndefined()
  expect(result.current.isPending).toBe(true)
  await act(async () => { pending.resolve(successful) })
  await waitFor(() => expect(result.current.quote).toEqual(quote))
  expect(mocks.quotePerpsMaxOpen).toHaveBeenCalledTimes(2)
})

it('keeps a cached maximum when only the oracle publication time changes', async () => {
  const { result, rerender } = renderHook(props => usePerpsMaxOpenQuote(props), { initialProps: input, wrapper: Wrapper })
  await waitFor(() => expect(result.current.quote).toEqual(quote))
  rerender({ ...input, publishTime: input.publishTime! + 15 })
  expect(result.current.quote).toEqual(quote)
  expect(result.current.isPending).toBe(false)
  expect(result.current.isFetching).toBe(false)
  expect(mocks.quotePerpsMaxOpen).toHaveBeenCalledTimes(1)
})

it('does not cancel an in-flight calculation for a timestamp-only update', async () => {
  const pending = deferred<typeof successful>()
  mocks.quotePerpsMaxOpen.mockReturnValueOnce(pending.promise)
  const { result, rerender } = renderHook(props => usePerpsMaxOpenQuote(props), { initialProps: input, wrapper: Wrapper })
  const signal: AbortSignal = mocks.quotePerpsMaxOpen.mock.calls[0][1].signal
  rerender({ ...input, publishTime: input.publishTime! + 15 })
  expect(signal.aborted).toBe(false)
  expect(mocks.quotePerpsMaxOpen).toHaveBeenCalledTimes(1)
  await act(async () => { pending.resolve(successful) })
  await waitFor(() => expect(result.current.quote).toEqual(quote))
})

it('shares a fresh calculation between equivalent consumers and a remount', async () => {
  const client = queryClient()
  const Wrapper = withClient(client)
  const first = renderHook(() => usePerpsMaxOpenQuote(input), { wrapper: Wrapper })
  await waitFor(() => expect(first.result.current.quote).toEqual(quote))
  const second = renderHook(() => usePerpsMaxOpenQuote({ ...input, publishTime: input.publishTime! + 1 }), { wrapper: Wrapper })
  expect(second.result.current.quote).toEqual(quote)
  first.unmount()
  second.unmount()
  const remounted = renderHook(() => usePerpsMaxOpenQuote(input), { wrapper: Wrapper })
  expect(remounted.result.current.quote).toEqual(quote)
  expect(mocks.quotePerpsMaxOpen).toHaveBeenCalledTimes(1)
})

it('revalidates a cached result after its freshness window', async () => {
  vi.useFakeTimers({ toFake: ['Date'] })
  const client = queryClient()
  const Wrapper = withClient(client)
  const first = renderHook(() => usePerpsMaxOpenQuote(input), { wrapper: Wrapper })
  await waitFor(() => expect(first.result.current.quote).toEqual(quote))
  first.unmount()
  vi.setSystemTime(Date.now() + 10_001)
  const pending = deferred<typeof successful>()
  mocks.quotePerpsMaxOpen.mockReturnValueOnce(pending.promise)
  const next = renderHook(() => usePerpsMaxOpenQuote(input), { wrapper: Wrapper })
  expect(next.result.current.isFetching).toBe(true)
  expect(mocks.quotePerpsMaxOpen).toHaveBeenCalledTimes(2)
  await act(async () => { pending.resolve(successful) })
})

it('continues refreshing chain state every 15 seconds without changing quote identity', async () => {
  vi.useFakeTimers()
  const { result } = renderHook(() => usePerpsMaxOpenQuote(input), { wrapper: Wrapper })
  await act(async () => { await vi.advanceTimersByTimeAsync(1) })
  expect(result.current.quote).toEqual(quote)
  await act(async () => { await vi.advanceTimersByTimeAsync(15_000) })
  expect(mocks.quotePerpsMaxOpen).toHaveBeenCalledTimes(2)
})

it('exposes background invalidation as fetching so Max remains gated until refreshed', async () => {
  const client = queryClient()
  const { result } = renderHook(() => usePerpsMaxOpenQuote(input), { wrapper: withClient(client) })
  await waitFor(() => expect(result.current.quote).toEqual(quote))
  const pending = deferred<typeof successful>()
  mocks.quotePerpsMaxOpen.mockReturnValueOnce(pending.promise)
  let refresh!: Promise<void>
  act(() => { refresh = client.invalidateQueries({ queryKey: ['perps', 'max-open'] }) })
  await waitFor(() => expect(result.current.isFetching).toBe(true))
  expect(result.current.isPending).toBe(false)
  expect(result.current.quote).toEqual(quote)
  await act(async () => { pending.resolve(successful); await refresh })
  await waitFor(() => expect(result.current.isFetching).toBe(false))
})

describe('unavailable inputs and capacity', () => {
  it.each<Partial<Input>>([{ enabled: false }, { account: undefined }, { availableAssets: undefined },
    { oraclePrice: 0n }, { publishTime: undefined }, { publishTime: 0 }, { protectionRewardsAssets: undefined },
    { selectedMaxLeverageBps: 0 }, { selectedMaxLeverageBps: NaN },
    { selectedMaxLeverageBps: Number.MAX_SAFE_INTEGER + 1 },
  ])('disables quoting for %s', override => {
    const { result } = renderHook(() => usePerpsMaxOpenQuote({ ...input, ...override }), { wrapper: Wrapper })
    expect(result.current.quote).toBeUndefined()
    expect(result.current.isPending).toBe(false)
    expect(mocks.quotePerpsMaxOpen).not.toHaveBeenCalled()
  })

  it('hides a cached quote when required oracle readiness is lost', async () => {
    const { result, rerender } = renderHook(props => usePerpsMaxOpenQuote(props), { initialProps: input, wrapper: Wrapper })
    await waitFor(() => expect(result.current.quote).toEqual(quote))
    rerender({ ...input, publishTime: undefined })
    expect(result.current.quote).toBeUndefined()
    expect(result.current.marginDelta).toBeUndefined()
    expect(result.current.isFetching).toBe(false)
    expect(mocks.quotePerpsMaxOpen).toHaveBeenCalledTimes(1)
  })

  it('returns no capacity when rewards exhaust the balance', async () => {
    mocks.quotePerpsMaxOpen.mockResolvedValueOnce({ marginDelta: 0n, quote: undefined })
    const { result } = renderHook(() => usePerpsMaxOpenQuote({ ...input, availableAssets: 600_000n }), { wrapper: Wrapper })
    await waitFor(() => expect(result.current.marginDelta).toBe(0n))
    expect(result.current.quote).toBeUndefined()
  })

  it('preserves zero capacity at the selected leverage', async () => {
    mocks.quotePerpsMaxOpen.mockResolvedValueOnce({ ...successful, quote: { ...quote, maxSizeDelta: 0n } })
    const { result } = renderHook(() => usePerpsMaxOpenQuote({ ...input, selectedMaxLeverageBps: 10_000 }), { wrapper: Wrapper })
    await waitFor(() => expect(result.current.quote?.maxSizeDelta).toBe(0n))
    expect(result.current.error).toBeNull()
  })

  it('preserves protocol zero-capacity diagnostics', async () => {
    mocks.quotePerpsMaxOpen.mockResolvedValueOnce({ ...successful,
      quote: { ...quote, maxSizeDelta: 0n, preview: { valid: false, invalidReason: 9 } },
    })
    const { result } = renderHook(() => usePerpsMaxOpenQuote(input), { wrapper: Wrapper })
    await waitFor(() => expect(result.current.quote?.maxSizeDelta).toBe(0n))
    expect(result.current.quote?.preview.invalidReason).toBe(9)
    expect(result.current.error).toBeNull()
  })

  it.each(['RPC unavailable', 'CfdEngineLens__QuoteSearchLimitExceeded()'])('surfaces %s without an approximate maximum', async message => {
    const error = new Error(message)
    mocks.quotePerpsMaxOpen.mockRejectedValue(error)
    const { result } = renderHook(() => usePerpsMaxOpenQuote(input), { wrapper: Wrapper })
    await waitFor(() => expect(result.current.error).toBe(error))
    expect(result.current.quote).toBeUndefined()
  })

  it('discards a successful quote after a failed refresh', async () => {
    const client = queryClient()
    const { result } = renderHook(() => usePerpsMaxOpenQuote(input), { wrapper: withClient(client) })
    await waitFor(() => expect(result.current.quote).toEqual(quote))
    const error = new Error('Refresh failed')
    mocks.quotePerpsMaxOpen.mockRejectedValue(error)
    await act(async () => { await client.invalidateQueries({ queryKey: ['perps', 'max-open'] }) })
    await waitFor(() => expect(result.current.error).toBe(error))
    expect(result.current.quote).toBeUndefined()
    expect(result.current.marginDelta).toBeUndefined()
  })
})
