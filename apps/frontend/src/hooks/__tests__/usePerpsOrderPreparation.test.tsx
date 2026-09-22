import { StrictMode, type ReactNode } from 'react'
import { act, renderHook } from '@testing-library/react'
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest'
import type { PreparedPerpsOrderV2 } from '../../contracts/perpsOrderV2'
import { ORACLE_RECOVERY_UNAVAILABLE, orderPreparationKey, usePerpsOrderPreparation } from '../usePerpsOrderPreparation'
import { preparationFailure } from '../../utils/perpsPreparationDiagnostics'
import { createOracleSyncError } from '../../test/fixtures/oracleSyncError'
import { getPerpsErrorMessage } from '../../utils/perpsErrors'

const analytics = vi.hoisted(() => vi.fn())
vi.mock('../../analytics/client', () => ({ captureAnalyticsEvent: analytics }))
import { prepared } from '../../test/fixtures/preparedOrder'
function deferred<T>() {
  let resolve!: (value: T) => void
  let reject!: (error: Error) => void
  const promise = new Promise<T>((yes, no) => { resolve = yes; reject = no })
  return { promise, resolve, reject }
}
async function advance(ms: number) { await act(async () => { await vi.advanceTimersByTimeAsync(ms) }) }
type Input = { quantity: bigint }
type Mode = 'background' | 'review' | 'inactive'
function setup(mode: Mode = 'background', prepare = vi.fn<(input: Input) => Promise<PreparedPerpsOrderV2>>().mockImplementation(async () => prepared())) {
  const initialProps = { mode, key: 'one', identityKey: 'account-one' }
  const view = renderHook((props: typeof initialProps) => usePerpsOrderPreparation({
    ...props, prepare, candidate: { key: props.key, input: { quantity: 100n } },
  }), { initialProps })
  return { ...view, prepare, update: (props: Partial<typeof initialProps>) => view.rerender({ ...initialProps, ...props }) }
}

describe('order preparation lifecycle', () => {
  beforeEach(() => { vi.useFakeTimers(); vi.setSystemTime(new Date('2026-09-08T12:00:00Z')); analytics.mockClear() })
  afterEach(() => { vi.useRealTimers(); vi.restoreAllMocks() })

  it('records safe failure details through the normalized error cause', async () => {
    const cause = preparationFailure(new Error('execution reverted with private RPC data'), 'commit_simulation', 'commitOrder')
    const error = new Error('Commit reverted before creating an order', { cause })
    const view = setup('review', vi.fn().mockRejectedValue(error))
    await advance(0)
    expect(view.result.current.error).toBe(error)
    expect(view.result.current.supportReference).toMatch(/^tx-[\da-f-]{36}$/)
    expect(analytics).toHaveBeenCalledWith('transaction failed', expect.objectContaining({ support_reference: view.result.current.supportReference }))
    expect(analytics).toHaveBeenCalledWith('perps order preparation finished', {
      surface: 'perps', duration_ms: 0, reason_code: 'cold', error_category: 'preparation_failed',
      error_code: 'undecoded_revert', stage: 'commit_simulation', contract_function: 'commitOrder', support_reference: view.result.current.supportReference,
    })
  })

  it('distinguishes timeouts from late rejected work and records only one failure', async () => {
    const pending = deferred<PreparedPerpsOrderV2>()
    setup('review', vi.fn().mockReturnValue(pending.promise))
    await advance(30_000)
    await act(async () => { pending.reject(new Error('execution reverted')) })
    expect(analytics.mock.calls.filter(([name]) => name === 'perps order preparation finished')).toEqual([
      ['perps order preparation finished', { surface: 'perps', duration_ms: 30_000, reason_code: 'cold',
        error_category: 'preparation_failed', error_code: 'timeout', stage: 'preparation_timeout', support_reference: expect.stringMatching(/^tx-[\da-f-]{36}$/) }],
    ])
  })

  it('does not attach failure codes to obsolete requests or successful retries', async () => {
    const pending = deferred<PreparedPerpsOrderV2>()
    const view = setup('review', vi.fn().mockReturnValueOnce(pending.promise).mockResolvedValue(prepared()))
    view.update({ mode: 'review', key: 'two' })
    await advance(0)
    await act(async () => { pending.reject(preparationFailure(new Error('execution reverted'), 'context_read', 'getLatestPrice') as Error) })
    const events = analytics.mock.calls.filter(([name]) => name === 'perps order preparation finished').map(([, properties]) => properties)
    expect(events.map(properties => properties.error_category)).toEqual(['none', 'cancelled'])
    expect(events.every(properties => !('error_code' in properties) && !('stage' in properties))).toBe(true)
  })

  it('debounces edits and reuses a warm order immediately when review opens', async () => {
    const view = setup()
    await advance(499)
    expect(view.prepare).not.toHaveBeenCalled()
    view.update({ key: 'two' })
    await advance(499)
    expect(view.prepare).not.toHaveBeenCalled()
    await advance(1)
    const result = view.result.current.result
    expect(view.prepare).toHaveBeenCalledTimes(1)
    view.update({ key: 'two', mode: 'review' })
    expect(view.result.current.ready).toBe(true)
    expect(view.result.current.result).toBe(result)
    expect(view.prepare).toHaveBeenCalledTimes(1)
    expect(analytics).toHaveBeenCalledWith('perps review ready', expect.objectContaining({ reason_code: 'completed_reuse', duration_ms: 0 }))
  })

  it('starts cold review without debounce and joins a matching pending background request', async () => {
    const pending = deferred<PreparedPerpsOrderV2>()
    const view = setup('background', vi.fn().mockReturnValue(pending.promise))
    await advance(500)
    await advance(2000)
    view.update({ mode: 'review' })
    expect(view.prepare).toHaveBeenCalledTimes(1)
    await advance(1000)
    expect(view.result.current.slow).toBe(true)
    await act(async () => { pending.resolve(prepared()) })
    expect(view.result.current.ready).toBe(true)
    expect(analytics).toHaveBeenCalledWith('perps review ready', expect.objectContaining({ reason_code: 'pending_reuse' }))
    view.unmount()
    const cold = setup('review')
    await advance(0)
    expect(cold.prepare).toHaveBeenCalledTimes(1)
    expect(cold.result.current.ready).toBe(true)
  })

  it('does not reuse old results or continuously warm an untouched ticket', async () => {
    const view = setup()
    await advance(500)
    await advance(10_000)
    expect(view.prepare).toHaveBeenCalledTimes(1)
    view.update({ mode: 'review' })
    expect(view.result.current.ready).toBe(false)
    await advance(0)
    expect(view.prepare).toHaveBeenCalledTimes(2)
  })

  it('queues only the latest background candidate and ignores old completions', async () => {
    const pending = deferred<PreparedPerpsOrderV2>()
    const prepare = vi.fn().mockReturnValueOnce(pending.promise).mockImplementation(async () => prepared())
    const view = setup('background', prepare)
    await advance(500)
    view.update({ key: 'two' })
    await advance(600)
    view.update({ key: 'three' })
    await advance(600)
    expect(prepare).toHaveBeenCalledTimes(1)
    await act(async () => { pending.resolve(prepared()) })
    expect(view.result.current.result).toBeUndefined()
    await advance(500)
    expect(prepare).toHaveBeenCalledTimes(2)
    expect(view.result.current.key).toBe('three')
  })

  it('does not wait for obsolete background work when review opens', async () => {
    const pending = deferred<PreparedPerpsOrderV2>()
    const view = setup('background', vi.fn().mockReturnValueOnce(pending.promise).mockImplementation(async () => prepared()))
    await advance(500)
    view.update({ mode: 'review', key: 'two' })
    await advance(0)
    const fresh = view.result.current.result
    await act(async () => { pending.resolve(prepared(60, 999n)) })
    expect(view.result.current.result).toBe(fresh)
    expect(view.prepare).toHaveBeenCalledTimes(2)
  })

  it('keeps background failures quiet and retries once on review, then requires manual retry', async () => {
    const view = setup('background', vi.fn().mockRejectedValue(new Error('offline')))
    await advance(500)
    expect(view.result.current.status).toBe('error')
    view.update({ mode: 'review' })
    await advance(0)
    expect(view.prepare).toHaveBeenCalledTimes(2)
    await advance(60_000)
    expect(view.prepare).toHaveBeenCalledTimes(2)
    act(() => { view.result.current.retry() })
    await advance(0)
    expect(view.prepare).toHaveBeenCalledTimes(3)
  })

  it('refreshes at forty-five seconds remaining and retains the last displayed terms', async () => {
    const pending = deferred<PreparedPerpsOrderV2>()
    const view = setup('review', vi.fn().mockResolvedValueOnce(prepared(55)).mockReturnValueOnce(pending.promise).mockImplementation(async () => prepared(55)))
    await advance(0)
    const original = view.result.current.result
    await advance(10_000)
    expect(view.result.current.ready).toBe(false)
    expect(view.result.current.refreshing).toBe(true)
    expect(view.result.current.result).toBe(original)
    await act(async () => { pending.resolve(prepared(55, 30_000_000n)) })
    expect(view.result.current.previous).toBe(original)
    expect(view.result.current.result?.request.marginDelta).toBe(30_000_000n)
    expect(view.result.current.ready).toBe(true)
    await advance(10_000)
    expect(view.prepare).toHaveBeenCalledTimes(3)
  })

  it('stops on refresh failure or an already expiring response', async () => {
    const view = setup('review', vi.fn().mockResolvedValueOnce(prepared(55)).mockRejectedValue(new Error('offline')))
    await advance(10_000)
    expect(view.result.current.status).toBe('error')
    expect(view.result.current.ready).toBe(false)
    await advance(60_000)
    expect(view.prepare).toHaveBeenCalledTimes(2)
    view.unmount()
    const expired = setup('review', vi.fn().mockResolvedValue(prepared(10)))
    await advance(60_000)
    expect(expired.result.current.status).toBe('error')
    expect(expired.prepare).toHaveBeenCalledTimes(1)
  })

  it('times out from the original request start and ignores a late result', async () => {
    const pending = deferred<PreparedPerpsOrderV2>()
    const view = setup('background', vi.fn().mockReturnValue(pending.promise))
    await advance(500)
    await advance(2000)
    view.update({ mode: 'review' })
    await advance(28_000)
    expect(view.result.current.status).toBe('error')
    expect(view.prepare.mock.calls[0][1].aborted).toBe(true)
    await act(async () => { pending.resolve(prepared()) })
    expect(view.result.current.ready).toBe(false)
  })

  it('invalidates results on identity changes, cancellation, and commit start', async () => {
    const pending = deferred<PreparedPerpsOrderV2>()
    const view = setup('review', vi.fn().mockReturnValueOnce(pending.promise).mockImplementation(async () => prepared()))
    await advance(0)
    view.update({ mode: 'review', identityKey: 'account-two' })
    await advance(0)
    const fresh = view.result.current.result
    await act(async () => { pending.resolve(prepared(60, 999n)) })
    expect(view.result.current.result).toBe(fresh)
    view.update({ mode: 'inactive', identityKey: 'account-two' })
    await advance(60_000)
    expect(view.result.current.result).toBeUndefined()
    expect(view.prepare).toHaveBeenCalledTimes(2)
  })

  it('pauses while hidden and rechecks freshness when returning', async () => {
    let visibility: DocumentVisibilityState = 'hidden'
    vi.spyOn(document, 'visibilityState', 'get').mockImplementation(() => visibility)
    const view = setup()
    await advance(1000)
    expect(view.prepare).not.toHaveBeenCalled()
    act(() => { visibility = 'visible'; document.dispatchEvent(new Event('visibilitychange')) })
    await advance(500)
    view.update({ mode: 'review' })
    act(() => { visibility = 'hidden'; document.dispatchEvent(new Event('visibilitychange')) })
    await advance(60_000)
    expect(view.prepare).toHaveBeenCalledTimes(1)
    expect(view.result.current.ready).toBe(false)
    act(() => { visibility = 'visible'; document.dispatchEvent(new Event('visibilitychange')) })
    await advance(0)
    expect(view.prepare).toHaveBeenCalledTimes(2)
    expect(view.result.current.ready).toBe(true)
  })

  it('recovers after StrictMode effect cleanup and ignores work after unmount', async () => {
    const prepare = vi.fn().mockImplementation(async () => prepared())
    const view = renderHook(() => usePerpsOrderPreparation({ mode: 'review', identityKey: 'one', candidate: { key: 'one', input: {} }, prepare }), {
      wrapper: ({ children }: { children: ReactNode }) => <StrictMode>{children}</StrictMode>,
    })
    await advance(0)
    expect(view.result.current.ready).toBe(true)
    view.unmount()
    const count = prepare.mock.calls.length
    await advance(60_000)
    expect(prepare).toHaveBeenCalledTimes(count)
  })

  it('rechecks changed account context without changing the frozen order', async () => {
    const prepare = vi.fn().mockImplementation(async () => prepared())
    const input = { quantity: 100n }
    const view = renderHook(({ contextKey }) => usePerpsOrderPreparation({ mode: 'review', identityKey: 'account',
      contextKey, candidate: { key: 'frozen', input }, prepare }), { initialProps: { contextKey: 'balance-one' } })
    await advance(0)
    const before = view.result.current.result
    view.rerender({ contextKey: 'balance-two' })
    expect(view.result.current.ready).toBe(false)
    await advance(0)
    expect(view.result.current.previous).toBe(before)
    expect(prepare).toHaveBeenCalledTimes(2)
    expect(prepare).toHaveBeenLastCalledWith(input, expect.any(AbortSignal))
  })

  it('refreshes changed context on returning from a briefly hidden tab even inside the reuse window', async () => {
    let visibility: DocumentVisibilityState = 'visible'
    vi.spyOn(document, 'visibilityState', 'get').mockImplementation(() => visibility)
    const prepare = vi.fn().mockImplementation(async () => prepared())
    const view = renderHook(({ contextKey }) => usePerpsOrderPreparation({ mode: 'review', identityKey: 'account',
      contextKey, candidate: { key: 'frozen', input: {} }, prepare }), { initialProps: { contextKey: 'one' } })
    await advance(0)
    act(() => { visibility = 'hidden'; document.dispatchEvent(new Event('visibilitychange')) })
    view.rerender({ contextKey: 'two' })
    await advance(1000)
    expect(prepare).toHaveBeenCalledTimes(1)
    act(() => { visibility = 'visible'; document.dispatchEvent(new Event('visibilitychange')) })
    await advance(0)
    expect(prepare).toHaveBeenCalledTimes(2)
    expect(view.result.current.ready).toBe(true)
  })

  it('keeps keys value-based without conflating bigint, strings, null, or unlimited slippage', () => {
    expect(orderPreparationKey({ size: 1n, slippage: Infinity })).toBe(orderPreparationKey({ size: 1n, slippage: Infinity }))
    expect(new Set([1n, '1', 1, Infinity, null].map(orderPreparationKey)).size).toBe(5)
    expect(orderPreparationKey({ size: 1n, slippage: Infinity })).toBe(orderPreparationKey({ slippage: Infinity, size: 1n }))
  })
})

const syncError = (legacyAbi = false) => {
  const cause = createOracleSyncError(legacyAbi)
  return new Error(getPerpsErrorMessage(cause, 'review'), { cause })
}

describe('oracle recovery during review', () => {
  beforeEach(() => { vi.useFakeTimers(); vi.setSystemTime(new Date('2026-09-08T12:00:00Z')); analytics.mockClear() })
  afterEach(() => { vi.useRealTimers(); vi.restoreAllMocks() })

  it.each([true, false])('retries the full preparation and preserves the reviewed input (legacy ABI=%s)', async legacyAbi => {
    const prepare = vi.fn().mockRejectedValueOnce(syncError(legacyAbi)).mockImplementation(async () => prepared())
    const view = setup('review', prepare)
    await advance(0)
    expect(view.result.current.recoveringOracle).toBe(true)
    expect(view.result.current.ready).toBe(false)
    await advance(1999)
    expect(prepare).toHaveBeenCalledTimes(1)
    await advance(1)
    expect(prepare).toHaveBeenCalledTimes(2)
    expect(prepare.mock.calls[1][0]).toBe(prepare.mock.calls[0][0])
    expect(view.result.current.ready).toBe(true)
    expect(view.result.current.recoveringOracle).toBe(false)
    expect(analytics).toHaveBeenCalledWith('perps oracle recovery', expect.objectContaining({ reason_code: 'succeeded', duration_ms: 2000 }))
  })
  it.each([true, false])('exhausts the original budget and manual retry starts another budget (legacy ABI=%s)', async legacyAbi => {
    const view = setup('review', vi.fn().mockRejectedValue(syncError(legacyAbi)))
    await advance(30_000)
    expect(view.prepare).toHaveBeenCalledTimes(15)
    expect(view.result.current.error).toMatchObject({ message: ORACLE_RECOVERY_UNAVAILABLE })
    expect(view.result.current.ready).toBe(false)
    await advance(10_000)
    expect(view.prepare).toHaveBeenCalledTimes(15)
    act(() => { view.result.current.retry() })
    await advance(0)
    expect(view.prepare).toHaveBeenCalledTimes(16)
    expect(view.result.current.recoveringOracle).toBe(true)
  })
  it('pauses while hidden and resumes without resetting the deadline', async () => {
    const view = setup('review', vi.fn().mockRejectedValue(syncError()))
    await advance(0)
    vi.spyOn(document, 'visibilityState', 'get').mockReturnValue('hidden')
    act(() => { document.dispatchEvent(new Event('visibilitychange')) })
    await advance(20_000)
    expect(view.prepare).toHaveBeenCalledTimes(1)
    vi.spyOn(document, 'visibilityState', 'get').mockReturnValue('visible')
    act(() => { document.dispatchEvent(new Event('visibilitychange')) })
    await advance(0)
    expect(view.prepare).toHaveBeenCalledTimes(2)
    await advance(10_000)
    expect(view.result.current.error).toMatchObject({ message: ORACLE_RECOVERY_UNAVAILABLE })
  })
  it('expires while hidden and does not silently restart on visibility', async () => {
    const view = setup('review', vi.fn().mockRejectedValue(syncError()))
    await advance(0)
    vi.spyOn(document, 'visibilityState', 'get').mockReturnValue('hidden')
    act(() => { document.dispatchEvent(new Event('visibilitychange')) })
    await advance(30_000)
    vi.spyOn(document, 'visibilityState', 'get').mockReturnValue('visible')
    act(() => { document.dispatchEvent(new Event('visibilitychange')) })
    await advance(0)
    expect(view.prepare).toHaveBeenCalledTimes(1)
    expect(view.result.current.status).toBe('error')
  })
  it('keeps background errors quiet but recovers a request joined by review', async () => {
    const pending = deferred<PreparedPerpsOrderV2>()
    const view = setup('background', vi.fn().mockReturnValueOnce(pending.promise).mockImplementation(async () => prepared()))
    await advance(500)
    view.update({ mode: 'review' })
    await act(async () => { pending.reject(syncError()) })
    expect(view.result.current.recoveringOracle).toBe(true)
    await advance(2000)
    expect(view.result.current.ready).toBe(true)
  })
  it('never retries a background-only synchronization failure', async () => {
    const view = setup('background', vi.fn().mockRejectedValue(syncError()))
    await advance(30_000)
    expect(view.prepare).toHaveBeenCalledTimes(1)
    expect(view.result.current.recoveringOracle).toBe(false)
  })
  it('cancels queued recovery on modal close and identity changes', async () => {
    const view = setup('review', vi.fn().mockRejectedValue(syncError()))
    await advance(0)
    view.update({ mode: 'inactive' })
    await advance(2000)
    expect(view.prepare).toHaveBeenCalledTimes(1)
    view.update({ mode: 'review', identityKey: 'account-two' })
    await advance(0)
    expect(view.result.current.identityKey).toBe('account-two')
    expect(view.prepare).toHaveBeenCalledTimes(2)
    view.unmount()
    await advance(2000)
    expect(view.prepare).toHaveBeenCalledTimes(2)
  })
  it('ignores a late retry result after inputs change', async () => {
    const pending = deferred<PreparedPerpsOrderV2>()
    const view = setup('review', vi.fn().mockRejectedValueOnce(syncError()).mockReturnValueOnce(pending.promise).mockImplementation(async () => prepared()))
    await advance(2000)
    view.update({ key: 'new-input' })
    await advance(0)
    const fresh = view.result.current.result
    await act(async () => { pending.resolve(prepared(60, 99n)) })
    expect(view.result.current.result).toBe(fresh)
    expect(view.result.current.ready).toBe(true)
  })
  it('retains prior terms during recovery and surfaces updated terms after success', async () => {
    const view = setup('review', vi.fn().mockResolvedValueOnce(prepared(55)).mockRejectedValueOnce(syncError()).mockImplementation(async () => prepared(60, 30_000_000n)))
    await advance(0)
    const previous = view.result.current.result
    await advance(10_000)
    expect(view.result.current.result).toBe(previous)
    expect(view.result.current.ready).toBe(false)
    await advance(2000)
    expect(view.result.current.previous).toBe(previous)
    expect(view.result.current.result?.request.marginDelta).toBe(30_000_000n)
    expect(view.result.current.ready).toBe(true)
  })
  it('rebuilds changed account context before resuming hidden recovery', async () => {
    const prepare = vi.fn().mockRejectedValueOnce(syncError()).mockImplementation(async () => prepared())
    const view = renderHook(({ contextKey }) => usePerpsOrderPreparation({
      candidate: { key: 'draft', input: { quantity: 100n } }, identityKey: 'account',
      contextKey, mode: 'review', prepare,
    }), { initialProps: { contextKey: 'before' } })
    await advance(0)
    vi.spyOn(document, 'visibilityState', 'get').mockReturnValue('hidden')
    act(() => { document.dispatchEvent(new Event('visibilitychange')) })
    view.rerender({ contextKey: 'after' })
    await advance(5000)
    expect(prepare).toHaveBeenCalledTimes(1)
    vi.spyOn(document, 'visibilityState', 'get').mockReturnValue('visible')
    act(() => { document.dispatchEvent(new Event('visibilitychange')) })
    await advance(0)
    expect(view.result.current.contextKey).toBe('after')
    expect(view.result.current.ready).toBe(true)
    expect(prepare).toHaveBeenCalledTimes(2)
  })
  it('keeps the recovery deadline when market polling changes context repeatedly', async () => {
    const prepare = vi.fn().mockRejectedValue(syncError())
    const view = renderHook(({ contextKey }) => usePerpsOrderPreparation({
      candidate: { key: 'draft', input: { quantity: 100n } }, identityKey: 'account',
      contextKey, mode: 'review', prepare,
    }), { initialProps: { contextKey: '0' } })
    await advance(0)
    for (let i = 1; i <= 5; i++) {
      await advance(5000)
      view.rerender({ contextKey: String(i) })
      await advance(0)
    }
    await advance(5000)
    expect(view.result.current.error).toMatchObject({ message: ORACLE_RECOVERY_UNAVAILABLE })
    expect(view.result.current.ready).toBe(false)
    expect(analytics.mock.calls.filter(call => call[0] === 'perps oracle recovery' && call[1].reason_code === 'started')).toHaveLength(1)
    expect(analytics).toHaveBeenCalledWith('perps oracle recovery', expect.objectContaining({ reason_code: 'exhausted', duration_ms: 30_000 }))
    const attempts = prepare.mock.calls.length
    view.rerender({ contextKey: 'after-exhaustion' })
    await advance(5000)
    expect(prepare).toHaveBeenCalledTimes(attempts)
    expect(view.result.current.error).toMatchObject({ message: ORACLE_RECOVERY_UNAVAILABLE })
    expect(view.result.current.matches).toBe(true)
    act(() => { view.result.current.retry() })
    await advance(0)
    expect(prepare).toHaveBeenCalledTimes(attempts + 1)
  })
  it('stops recovery immediately on an unrelated error', async () => {
    const view = setup('review', vi.fn().mockRejectedValueOnce(syncError()).mockRejectedValue(new Error('insufficient margin')))
    await advance(30_000)
    expect(view.prepare).toHaveBeenCalledTimes(2)
    expect(view.result.current.error).toMatchObject({ message: 'insufficient margin' })
  })
})

vi.mock('../../perps-aa/deadlineClock', () => ({ deadlineNow: () => Date.now(), refreshDeadlineClock: async () => ({ now: () => Date.now() }), observeDeadlineResponse: () => {} }))
