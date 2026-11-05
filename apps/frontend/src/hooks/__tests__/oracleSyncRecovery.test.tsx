import { act, renderHook } from '@testing-library/react'
import { afterEach, beforeEach, expect, it, vi } from 'vitest'
import { encodeErrorResult, parseAbi } from 'viem'
import { usePerpsOrderPreparation } from '../usePerpsOrderPreparation'
import { prepared } from '../../test/fixtures/preparedOrder'
import { createOracleSyncError } from '../../test/fixtures/oracleSyncError'
// @ts-expect-error The standalone Node worker intentionally has no TypeScript declaration.
import { createOracleWorker } from '../../../scripts/perps-oracle-worker.mjs'

vi.mock('../../analytics/client', () => ({ captureAnalyticsEvent: vi.fn() }))
beforeEach(() => { vi.useFakeTimers(); vi.setSystemTime(new Date('2026-09-15T12:00:00Z')) })
afterEach(() => { vi.useRealTimers(); vi.restoreAllMocks() })

it.each([true, false])('recovers a review after the worker refreshes the accepted basket (legacy ABI=%s)', async legacyAbi => {
  const tick = BigInt(Math.floor(Date.now() / 1000))
  let acceptedTime = tick - 5n
  const markTime = tick // Historical execution advanced this independently of the accepted live basket.
  let blockNumber = 10n
  const publicClient = {
    getBlock: vi.fn(async () => ({ number: blockNumber, timestamp: tick })),
    readContract: vi.fn(async ({ functionName }: { functionName: string }) => {
      if (functionName === 'getProtocolStatus') return { lastMarkTime: markTime, lastMarkPrice: 100_000_000n, oracleFrozen: false }
      if (functionName === 'getAcceptedBasketMark') return [true, 100_000_000n, acceptedTime]
      if (functionName === 'getLatestPrice') {
        if (acceptedTime < markTime) throw createOracleSyncError(false,
          encodeErrorResult({ abi: parseAbi(['error PletherOracle__PriceOutOfOrder(uint64,uint64)']),
            errorName: 'PletherOracle__PriceOutOfOrder', args: [acceptedTime, markTime] }))
        return { price: 100_000_000n, markPrice: 100_000_000n, publishTime: acceptedTime }
      }
      if (functionName === 'getUpdateFee') return 1n
      throw new Error(`Unexpected ${functionName}`)
    }),
    getBalance: vi.fn(async () => 1000n),
    simulateContract: vi.fn(async () => ({ request: {} })),
    waitForTransactionReceipt: vi.fn(async () => {
      acceptedTime = tick; blockNumber++
      return { status: 'success', blockNumber }
    }),
  }
  const prepare = vi.fn(async () => {
    const block = await publicClient.getBlock()
    if (acceptedTime < markTime) throw new Error('Review wrapper', { cause: createOracleSyncError(legacyAbi,
      encodeErrorResult({ abi: parseAbi(['error PletherOracle__PriceOutOfOrder(uint64,uint64)']),
        errorName: 'PletherOracle__PriceOutOfOrder', args: [acceptedTime, markTime] })) })
    return { ...prepared(), reviewedBlockNumber: block.number }
  })
  const candidate = { key: 'unchanged-draft', input: { size: 100n, margin: 20_000_000n } }
  const view = renderHook(() => usePerpsOrderPreparation({ candidate, identityKey: 'account', mode: 'review', prepare }))
  await act(async () => { await vi.advanceTimersByTimeAsync(0) })
  expect(view.result.current.recoveringOracle).toBe(true)
  expect(view.result.current.ready).toBe(false)
  const walletClient = { writeContract: vi.fn(async () => '0x1234') }
  const iterate = createOracleWorker({ publicClient, walletClient, account: { address: '0x1234' },
    log: vi.fn(), logEvery: vi.fn(),
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

vi.mock('../../perps-aa/deadlineClock', () => ({ deadlineNow: () => Date.now(), refreshDeadlineClock: async () => ({ now: () => Date.now() }), observeDeadlineResponse: () => {} }))
