import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import { renderHook, waitFor, act } from '@testing-library/react'
import type { PropsWithChildren } from 'react'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { usePerpsMaxOpenQuote } from '../usePerpsMaxOpenQuote'

const mocks = vi.hoisted(() => ({ readContract: vi.fn(), getBlockNumber: vi.fn() }))
vi.mock('wagmi', () => ({ usePublicClient: () => mocks }))
const account = '0x00000000000000000000000000000000000000a1' as const
const input = { enabled: true, account, side: 1, availableUsdc: 100_000_000n,
  oraclePrice: 100_000_000n, publishTime: 2_000_000_000, protectionRewardsUsdc: 400_000n,
  selectedMaxLeverageBps: 100_000 }
const quantum = 100n * 10n ** 18n
const preview = (size: bigint, equity = 99_000_000n) => ({ valid: true, invalidReason: 0,
  postSize: size, postEquityUsdc: equity })
const quote = { maxSizeDelta: 7900n * quantum, preview: preview(7900n * quantum), limitingReason: 9 }
function wrapper({ children }: PropsWithChildren) {
  const client = new QueryClient({ defaultOptions: { queries: { retry: false, gcTime: 0 } } })
  return <QueryClientProvider client={client}>{children}</QueryClientProvider>
}

beforeEach(() => {
  mocks.getBlockNumber.mockReset().mockResolvedValue(42n)
  mocks.readContract.mockReset().mockImplementation(async ({ functionName, args }) => {
    if (functionName === 'maxOpenOrderExecutionBountyUsdc') return 200_000n
    if (functionName === 'CAP_PRICE') return 200_000_000n
    if (functionName === 'quoteMaxOpen') return quote
    if (functionName === 'previewOpen') return preview(args[2])
    throw new Error(`Unexpected read ${functionName}`)
  })
})

it('finds the largest lot within selected leverage after fees and reward reservations', async () => {
  const { result } = renderHook(() => usePerpsMaxOpenQuote(input), { wrapper })
  await waitFor(() => expect(result.current.quote?.maxSizeDelta).toBe(9n * quantum))
  expect(result.current.marginDelta).toBe(99_400_000n)
  expect(mocks.readContract).toHaveBeenCalledWith(expect.objectContaining({
    functionName: 'quoteMaxOpen', args: [account, 1, 99_400_000n, 100_000_000n, 2_000_000_000n],
  }))
  for (const [request] of mocks.readContract.mock.calls) expect(request.blockNumber).toBe(42n)
})

it('recalculates when selected leverage changes and discards the earlier maximum', async () => {
  const { result, rerender } = renderHook(props => usePerpsMaxOpenQuote(props), { initialProps: input, wrapper })
  await waitFor(() => expect(result.current.quote?.maxSizeDelta).toBe(9n * quantum))
  rerender({ ...input, selectedMaxLeverageBps: 330_000 })
  expect(result.current.quote).toBeUndefined()
  await waitFor(() => expect(result.current.quote?.maxSizeDelta).toBe(32n * quantum))
})

it('includes existing position exposure and equity in the leverage limit', async () => {
  const read = mocks.readContract.getMockImplementation()!
  mocks.readContract.mockImplementation(async request => {
    if (request.functionName === 'quoteMaxOpen') return { ...quote, preview: preview(quote.maxSizeDelta + 5n * quantum, 199_000_000n) }
    if (request.functionName === 'previewOpen') return preview(request.args[2] + 5n * quantum, 199_000_000n)
    return read(request)
  })
  const { result } = renderHook(() => usePerpsMaxOpenQuote(input), { wrapper })
  await waitFor(() => expect(result.current.quote?.maxSizeDelta).toBe(14n * quantum))
})

it('retains the protocol maximum when it is below the leverage limit', async () => {
  const read = mocks.readContract.getMockImplementation()!
  mocks.readContract.mockImplementation(async request => request.functionName === 'quoteMaxOpen'
    ? { ...quote, maxSizeDelta: 5n * quantum, preview: preview(5n * quantum) } : read(request))
  const { result } = renderHook(() => usePerpsMaxOpenQuote(input), { wrapper })
  await waitFor(() => expect(result.current.quote?.maxSizeDelta).toBe(5n * quantum))
  expect(mocks.readContract.mock.calls.some(([request]) => request.functionName === 'previewOpen')).toBe(false)
})

describe('unavailable inputs and capacity', () => {
  it.each([{ enabled: false }, { account: undefined }, { availableUsdc: undefined },
    { oraclePrice: 0n }, { publishTime: undefined }, { publishTime: 0 }, { protectionRewardsUsdc: undefined },
    { selectedMaxLeverageBps: 0 }, { selectedMaxLeverageBps: NaN },
  ])('disables quoting for %s', override => {
    const { result } = renderHook(() => usePerpsMaxOpenQuote({ ...input, ...override }), { wrapper })
    expect(result.current.quote).toBeUndefined()
    expect(result.current.isPending).toBe(false)
    expect(mocks.readContract).not.toHaveBeenCalled()
  })

  it('returns no capacity when rewards exhaust the balance', async () => {
    const { result } = renderHook(() => usePerpsMaxOpenQuote({ ...input, availableUsdc: 600_000n }), { wrapper })
    await waitFor(() => expect(result.current.marginDelta).toBe(0n))
    expect(result.current.quote).toBeUndefined()
    expect(mocks.readContract.mock.calls.some(([request]) => request.functionName === 'quoteMaxOpen')).toBe(false)
  })

  it('returns zero when selected leverage cannot fund one lot', async () => {
    const { result } = renderHook(() => usePerpsMaxOpenQuote({ ...input, selectedMaxLeverageBps: 10_000 }), { wrapper })
    await waitFor(() => expect(result.current.quote?.maxSizeDelta).toBe(0n))
    expect(result.current.error).toBeNull()
  })

  it('preserves protocol zero-capacity diagnostics', async () => {
    const read = mocks.readContract.getMockImplementation()!
    mocks.readContract.mockImplementation(async request => request.functionName === 'quoteMaxOpen'
      ? { ...quote, maxSizeDelta: 0n, preview: { valid: false, invalidReason: 9 } } : read(request))
    const { result } = renderHook(() => usePerpsMaxOpenQuote(input), { wrapper })
    await waitFor(() => expect(result.current.quote?.maxSizeDelta).toBe(0n))
    expect(result.current.quote?.preview.invalidReason).toBe(9)
    expect(result.current.error).toBeNull()
  })

  it.each(['RPC unavailable', 'CfdEngineLens__QuoteSearchLimitExceeded()'])('surfaces %s without an approximate maximum', async message => {
    const error = new Error(message)
    mocks.readContract.mockRejectedValue(error)
    const { result } = renderHook(() => usePerpsMaxOpenQuote(input), { wrapper })
    await waitFor(() => expect(result.current.error).toBe(error))
    expect(result.current.quote).toBeUndefined()
  })

  it('discards a successful quote after a failed refresh', async () => {
    const client = new QueryClient({ defaultOptions: { queries: { retry: false, gcTime: 0 } } })
    const { result } = renderHook(() => usePerpsMaxOpenQuote(input), {
      wrapper: ({ children }) => <QueryClientProvider client={client}>{children}</QueryClientProvider>,
    })
    await waitFor(() => expect(result.current.quote?.maxSizeDelta).toBe(9n * quantum))
    const error = new Error('Refresh failed')
    mocks.readContract.mockRejectedValue(error)
    await act(async () => { await client.invalidateQueries() })
    await waitFor(() => expect(result.current.error).toBe(error))
    expect(result.current.quote).toBeUndefined()
  })
})
