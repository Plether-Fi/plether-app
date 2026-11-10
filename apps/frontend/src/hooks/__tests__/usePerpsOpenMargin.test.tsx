import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import { act, renderHook, waitFor } from '@testing-library/react'
import type { PropsWithChildren } from 'react'
import { beforeEach, expect, it, vi } from 'vitest'
import { usePerpsOpenMargin } from '../usePerpsOpenMargin'

const mocks = vi.hoisted(() => ({ quote: vi.fn(), client: {} }))
vi.mock('wagmi', () => ({ usePublicClient: () => mocks.client }))
vi.mock('../../contracts/perpsOpenMargin', () => ({ quotePerpsOpenMargin: mocks.quote }))

const input = { enabled: true, account: '0x00000000000000000000000000000000000000a1' as const,
  side: 0, sizeDelta: 1_000n * 10n ** 18n, marginBudget: 500_000_000n,
  oraclePrice: 100_000_000n, selectedMaxLeverageBps: 100_000 }
const preview = { valid: true, marginDeltaAssets: 110_500_000n }
function wrapper() {
  const client = new QueryClient({ defaultOptions: { queries: { retry: false, gcTime: 0 } } })
  return ({ children }: PropsWithChildren) => <QueryClientProvider client={client}>{children}</QueryClientProvider>
}

beforeEach(() => { mocks.quote.mockReset().mockResolvedValue(preview) })

it('keeps review pending until a funded canonical quote arrives', async () => {
  let resolve!: (value: typeof preview) => void
  mocks.quote.mockReturnValueOnce(new Promise(done => { resolve = done }))
  const { result } = renderHook(() => usePerpsOpenMargin(input), { wrapper: wrapper() })
  expect(result.current).toMatchObject({ preview: undefined, isPending: true })
  await act(async () => { resolve(preview) })
  await waitFor(() => expect(result.current).toMatchObject({ preview, isPending: false }))
  expect(mocks.quote).toHaveBeenCalledWith(mocks.client, expect.objectContaining({
    account: input.account, sizeDelta: input.sizeDelta, marginBudget: input.marginBudget,
    oraclePrice: input.oraclePrice, selectedMaxLeverageBps: input.selectedMaxLeverageBps, signal: expect.any(AbortSignal),
  }))
})

it.each([
  { sizeDelta: 500n * 10n ** 18n }, { marginBudget: 400_000_000n }, { side: 1 },
  { selectedMaxLeverageBps: 50_000 }, { oraclePrice: 99_000_000n },
  { account: '0x00000000000000000000000000000000000000a2' as const },
])('does not use a quote from earlier risk inputs: %s', async override => {
  const { result, rerender } = renderHook(props => usePerpsOpenMargin(props), { initialProps: input, wrapper: wrapper() })
  await waitFor(() => expect(result.current.preview).toBe(preview))
  mocks.quote.mockReturnValueOnce(new Promise(() => {}))
  rerender({ ...input, ...override })
  expect(result.current).toMatchObject({ preview: undefined, isPending: true })
})

it('does not quote before account funding is available', () => {
  const { result } = renderHook(() => usePerpsOpenMargin({ ...input, marginBudget: undefined }), { wrapper: wrapper() })
  expect(result.current).toMatchObject({ preview: undefined, isPending: true })
  expect(mocks.quote).not.toHaveBeenCalled()
})

it('propagates a quote failure without returning a previous margin', async () => {
  const error = new Error('RPC unavailable')
  mocks.quote.mockRejectedValue(error)
  const { result } = renderHook(() => usePerpsOpenMargin(input), { wrapper: wrapper() })
  await waitFor(() => expect(result.current.error).toBe(error))
  expect(result.current.preview).toBeUndefined()
  expect(result.current.isPending).toBe(false)
})
