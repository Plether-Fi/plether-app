import { act, cleanup, renderHook } from '@testing-library/react'
import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import type { PropsWithChildren } from 'react'
import { afterEach, describe, expect, it, vi } from 'vitest'
import { useLeaderboard, useWallet } from './queries'

const mocks = vi.hoisted(() => ({ getLeaderboard: vi.fn(), getWallet: vi.fn() }))
vi.mock('./client', () => mocks)

afterEach(() => {
  cleanup()
  vi.useRealTimers()
  vi.resetAllMocks()
})

function useTestLeaderboard() {
  const query = useLeaderboard('testnet-trading-2026-09', '')
  return query.data?.pages[0].standings[0]?.pnl
}

function useTestWallet() {
  return useWallet('testnet-trading-2026-09', '0x1111111111111111111111111111111111111111').data?.wallet.pnl
}

describe('standings refresh', () => {
  it.each([
    ['leaderboard', useTestLeaderboard, mocks.getLeaderboard],
    ['wallet', useTestWallet, mocks.getWallet],
  ] as const)('recovers %s data without navigation or reload', async (_kind, useData, fetchData) => {
    vi.useFakeTimers()
    const client = new QueryClient({ defaultOptions: { queries: { retry: false, gcTime: Infinity } } })
    const wrapper = ({ children }: PropsWithChildren) => <QueryClientProvider client={client}>{children}</QueryClientProvider>
    mocks.getLeaderboard
      .mockResolvedValueOnce({ standings: [{ pnl: null }], nextCursor: null })
      .mockResolvedValue({ standings: [{ pnl: '1000000' }], nextCursor: null })
    mocks.getWallet.mockResolvedValueOnce({ wallet: { pnl: null } }).mockResolvedValue({ wallet: { pnl: '1000000' } })
    const { result, unmount } = renderHook(useData, { wrapper })
    await act(async () => { await vi.advanceTimersByTimeAsync(1) })
    expect(result.current).toBeNull()
    await act(async () => { await vi.advanceTimersByTimeAsync(15_000) })
    expect(result.current).toBe('1000000')
    expect(fetchData).toHaveBeenCalledTimes(2)
    unmount()
    client.clear()
  })
})
