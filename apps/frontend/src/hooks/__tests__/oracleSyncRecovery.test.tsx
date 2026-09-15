import { act, renderHook } from '@testing-library/react'
import { afterEach, beforeEach, expect, it, vi } from 'vitest'
import { encodeErrorResult, parseAbi } from 'viem'
import { usePerpsOrderPreparation } from '../usePerpsOrderPreparation'
import { prepared } from '../../test/fixtures/preparedOrder'
// @ts-expect-error The standalone Node worker intentionally has no TypeScript declaration.
import { createOracleWorker } from '../../../scripts/perps-oracle-worker.mjs'

vi.mock('../../analytics/client', () => ({ captureAnalyticsEvent: vi.fn() }))
beforeEach(() => { vi.useFakeTimers(); vi.setSystemTime(new Date('2026-09-15T12:00:00Z')) })
afterEach(() => { vi.useRealTimers(); vi.restoreAllMocks() })

it('recovers a review after the worker repairs independently stored Pyth feeds', async () => {
  const tick = BigInt(Math.floor(Date.now() / 1000))
  let storedTime = tick - 5n
  const markTime = tick // Historical execution advanced this without touching stored feeds.
  let blockNumber = 10n
  const feedIds = Array.from({ length: 6 }, (_, index) => `0x${(index + 1).toString().padStart(64, '0')}`)
  const publicClient = {
    getBlock: vi.fn(async () => ({ number: blockNumber, timestamp: tick })),
    readContract: vi.fn(async ({ functionName }: { functionName: string }) => {
      if (functionName === 'getProtocolStatus') return { lastMarkTime: markTime, lastMarkPrice: 100_000_000n }
      if (functionName === 'getPriceUnsafe') return { publishTime: storedTime }
      if (functionName === 'getUpdateFee') return 1n
      throw new Error(`Unexpected ${functionName}`)
    }),
    getBalance: vi.fn(async () => 1000n),
    simulateContract: vi.fn(async () => ({ request: {} })),
    waitForTransactionReceipt: vi.fn(async () => {
      storedTime = tick; blockNumber++
      return { status: 'success', blockNumber }
    }),
  }
  const prepare = vi.fn(async () => {
    const block = await publicClient.getBlock()
    if (storedTime < markTime) throw new Error('Review wrapper', { cause: {
      data: encodeErrorResult({ abi: parseAbi(['error PletherOracle__PriceOutOfOrder(uint64,uint64)']),
        errorName: 'PletherOracle__PriceOutOfOrder', args: [storedTime, markTime] }),
    } })
    return { ...prepared(), reviewedBlockNumber: block.number }
  })
  const candidate = { key: 'unchanged-draft', input: { size: 100n, margin: 20_000_000n } }
  const view = renderHook(() => usePerpsOrderPreparation({ candidate, identityKey: 'account', mode: 'review', prepare }))
  await act(async () => { await vi.advanceTimersByTimeAsync(0) })
  expect(view.result.current.recoveringOracle).toBe(true)
  expect(view.result.current.ready).toBe(false)
  const walletClient = { writeContract: vi.fn(async () => '0x1234') }
  const iterate = createOracleWorker({ publicClient, walletClient, account: { address: '0x1234' },
    feeds: { pyth: '0x1234', feedIds }, log: vi.fn(), logEvery: vi.fn(),
    fetchPayload: async () => ({ updateData: ['0xabcd'], publishTimes: Array(6).fill(Number(tick)) }),
  })
  await iterate()
  await act(async () => { await vi.advanceTimersByTimeAsync(2000) })
  expect(view.result.current.ready).toBe(true)
  expect(view.result.current.result?.reviewedBlockNumber).toBe(11n)
  expect(view.result.current.key).toBe(candidate.key)
  expect(candidate.input).toEqual({ size: 100n, margin: 20_000_000n })
  expect(prepare).toHaveBeenCalledTimes(2)
  expect(walletClient.writeContract).toHaveBeenCalledTimes(1) // Only the worker repair, no user submission.
  expect(markTime).toBe(tick)
})
