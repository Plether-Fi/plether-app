import { renderHook } from '@testing-library/react'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { usePerpsMaxOpenQuote } from '../usePerpsMaxOpenQuote'
import { PERPS_ARBITRUM_SEPOLIA } from '../../contracts/perpsAddresses'

const mocks = vi.hoisted(() => ({
  useReadContracts: vi.fn(),
  bounty: {} as Record<string, unknown>,
  quote: {} as Record<string, unknown>,
}))
vi.mock('wagmi', () => ({ useReadContracts: mocks.useReadContracts }))
const account = '0x00000000000000000000000000000000000000a1' as const
const input = { enabled: true, account, side: 1, availableUsdc: 100_000_000n,
  oraclePrice: 99_000_000n, publishTime: 2_000_000_000, protectionRewardsUsdc: 400_000n }
const quote = { maxSizeDelta: 790_000n * 10n ** 18n, preview: { valid: true }, limitingReason: 9 }
const success = (result: unknown) => ({ data: [{ status: 'success', result }], isLoading: false, isFetching: false })

beforeEach(() => {
  mocks.bounty = success(200_000n)
  mocks.quote = success(quote)
  mocks.useReadContracts.mockReset().mockImplementation((request) =>
    request.contracts[0]?.functionName === 'maxOpenOrderExecutionBountyUsdc' ? mocks.bounty : mocks.quote)
})

it('quotes the account and side using oracle price/time and margin after router/protection rewards', () => {
  const { result } = renderHook(() => usePerpsMaxOpenQuote(input))
  expect(result.current.quote).toBe(quote)
  expect(result.current.marginDelta).toBe(99_400_000n)
  expect(mocks.useReadContracts).toHaveBeenLastCalledWith(expect.objectContaining({
    contracts: [expect.objectContaining({
      address: PERPS_ARBITRUM_SEPOLIA.cfdEngineLens, functionName: 'quoteMaxOpen',
      args: [account, 1, 99_400_000n, 99_000_000n, 2_000_000_000n],
    })], query: expect.objectContaining({ enabled: true, retry: false }),
  }))
})

it('keys the quote on changes to account, side, balance, oracle and protection budget', () => {
  const { rerender } = renderHook((props) => usePerpsMaxOpenQuote(props), { initialProps: input })
  rerender({ ...input, side: 0, availableUsdc: 80_000_000n, oraclePrice: 98_000_000n,
    publishTime: 2_000_000_001, protectionRewardsUsdc: 0n })
  expect(mocks.useReadContracts).toHaveBeenLastCalledWith(expect.objectContaining({
    contracts: [expect.objectContaining({ args: [account, 0, 79_800_000n, 98_000_000n, 2_000_000_001n] })],
  }))
})

describe('unavailable inputs and capacity', () => {
  it.each([{ enabled: false }, { account: undefined }, { availableUsdc: undefined },
    { oraclePrice: 0n }, { publishTime: undefined }, { publishTime: 0 }, { protectionRewardsUsdc: undefined },
  ])('disables quoting and discards cached data for %s', (override) => {
    const { result } = renderHook(() => usePerpsMaxOpenQuote({ ...input, ...override }))
    expect(result.current.quote).toBeUndefined()
    expect(result.current.isPending).toBe(false)
    expect(mocks.useReadContracts).toHaveBeenLastCalledWith(expect.objectContaining({ contracts: [], query: expect.objectContaining({ enabled: false }) }))
  })

  it('waits for bounty configuration before quoting', () => {
    mocks.bounty = { isLoading: true, isFetching: true }
    const { result } = renderHook(() => usePerpsMaxOpenQuote(input))
    expect(result.current.isPending).toBe(true)
    expect(result.current.quote).toBeUndefined()
  })

  it('returns no capacity when rewards exhaust the available balance', () => {
    const { result } = renderHook(() => usePerpsMaxOpenQuote({ ...input, availableUsdc: 600_000n }))
    expect(result.current.marginDelta).toBe(0n)
    expect(result.current.quote).toBeUndefined()
    expect(result.current.isPending).toBe(false)
  })

  it('preserves zero-capacity diagnostics separately from a failed read', () => {
    mocks.quote = success({ ...quote, maxSizeDelta: 0n, preview: { valid: false, invalidReason: 9 } })
    const { result } = renderHook(() => usePerpsMaxOpenQuote(input))
    expect(result.current.quote?.maxSizeDelta).toBe(0n)
    expect(result.current.error).toBeUndefined()
  })

  it.each(['RPC unavailable', 'CfdEngineLens__QuoteSearchLimitExceeded()'])('surfaces %s without returning an approximate maximum', (message) => {
    const error = new Error(message)
    mocks.quote = { data: [{ status: 'failure', error }] }
    const { result } = renderHook(() => usePerpsMaxOpenQuote(input))
    expect(result.current.error).toBe(error)
    expect(result.current.quote).toBeUndefined()
  })

  it('does not reuse a successful quote after a failed refresh', () => {
    const error = new Error('Refresh failed')
    mocks.quote = { ...success(quote), isError: true, error }
    const { result } = renderHook(() => usePerpsMaxOpenQuote(input))
    expect(result.current.error).toBe(error)
    expect(result.current.quote).toBeUndefined()
  })
})
