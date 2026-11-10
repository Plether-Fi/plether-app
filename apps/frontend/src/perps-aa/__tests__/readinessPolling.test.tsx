import { act, cleanup, fireEvent, render, renderHook, screen } from '@testing-library/react'
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest'
import { TradingStatus } from '../../components/TradingStatus'
import { currentReadiness, readinessBlocker, readinessChecks, refreshReadiness, useTradingReadiness, type ReadinessSnapshot } from '../readiness'

vi.mock('../../analytics/readiness', () => ({ reportReadiness: vi.fn() }))

const epoch = 1_700_000_000_000
let phase = 0
let paused = false
// Model the backend's ten-second sampling period, with three-second probes.
function response() {
  const now = epoch + phase + performance.now()
  const observedAt = epoch + Math.floor((now - epoch - 3_000) / 10_000) * 10_000
  const check = paused
    ? { component: 'paymaster', status: 'blocked' as const, reason: 'PAYMASTER_PAUSED' }
    : { component: 'sponsorship', status: 'ready' as const, reason: 'READY' }
  const value: ReadinessSnapshot = { version: 1, observedAt, expiresAt: observedAt + 15_000,
    enforcementEnabled: false, actions: { deposit: [check], open: [check], close: [check], protection: [check] } }
  return new Response(JSON.stringify(value), { headers: { Date: new Date(now).toUTCString() } })
}
const fetcher = vi.fn(async () => response())
beforeEach(async () => {
  vi.useFakeTimers({ toFake: ['Date', 'performance', 'setTimeout', 'clearTimeout', 'setInterval', 'clearInterval'] })
  // Polling/freshness must use the response clock even with a wrong device clock.
  vi.setSystemTime(epoch + 86_400_000)
  phase = 0
  paused = false
  fetcher.mockReset().mockImplementation(async () => response())
  vi.stubGlobal('fetch', fetcher)
  await refreshReadiness(true)
})
afterEach(() => {
  cleanup()
  vi.useRealTimers()
  vi.restoreAllMocks()
  vi.unstubAllGlobals()
})
async function tick(milliseconds = 1_000) {
  await act(async () => { await vi.advanceTimersByTimeAsync(milliseconds) })
}

describe('readiness refresh scheduling', () => {
  it.each([0, 1, 2, 3, 4, 5, 6, 7, 8, 9])('stays fresh for a minute with a %i-second polling phase', async seconds => {
    phase = seconds * 1_000
    await refreshReadiness(true)
    fetcher.mockClear()
    const first = renderHook(() => useTradingReadiness(true))
    const second = renderHook(() => useTradingReadiness(true))
    for (let i = 0; i < 60; i++) {
      // Check the old observation while a refresh is pending, too: never hide
      // an expiry behind React batching an immediate test response.
      act(() => { vi.advanceTimersByTime(1_000) })
      expect(readinessChecks(first.result.current, 'open')[0].status).toBe('ready')
      await act(async () => { await Promise.resolve() })
      expect(readinessChecks(second.result.current, 'open')[0].status).toBe('ready')
    }
    // Subscribers share polling; no request per render or per consumer.
    expect(fetcher.mock.calls.length).toBeGreaterThan(6)
    expect(fetcher.mock.calls.length).toBeLessThanOrEqual(30)
  })

  it('deduplicates in-flight refreshes and keeps fresh status while waiting', async () => {
    let resolve!: (response: Response) => void
    fetcher.mockImplementationOnce(() => new Promise<Response>(done => { resolve = done }))
    const pending = refreshReadiness(true)
    expect(refreshReadiness(true)).toBe(pending)
    expect(readinessChecks(currentReadiness(), 'open')[0].status).toBe('ready')
    resolve(response())
    await pending
    expect(fetcher).toHaveBeenCalledTimes(2) // initial observation and this request
  })

  it('shows a real outage, expires blockers on time, and backs off failed requests', async () => {
    paused = true
    await refreshReadiness(true)
    const { result } = renderHook(() => useTradingReadiness(true))
    expect(readinessBlocker(result.current, 'open')?.reason).toBe('PAYMASTER_PAUSED')
    // A request can remain pending, but must not extend the previous expiry.
    let reject!: (error: Error) => void
    fetcher.mockImplementationOnce(() => new Promise<Response>((_, fail) => { reject = fail }))
    const pending = refreshReadiness(true)
    await tick(4_000)
    expect(readinessChecks(result.current, 'open')[0].status).toBe('unknown')
    expect(readinessBlocker(result.current, 'open')).toBeUndefined()
    await act(async () => { reject(new Error('offline')); await pending })
    expect(currentReadiness()).toBeUndefined()
    fetcher.mockRejectedValue(new Error('offline'))
    fetcher.mockClear()
    await tick(5_000)
    expect(fetcher).not.toHaveBeenCalled()
    await tick(1_000)
    expect(fetcher).toHaveBeenCalledTimes(1)
    await tick(9_000)
    expect(fetcher).toHaveBeenCalledTimes(1)
  })

  it('does not keep retrying an already-expired cached response every second', async () => {
    const stale = response()
    const value = await stale.json()
    fetcher.mockImplementation(async () => new Response(JSON.stringify(value), { headers: { Date: new Date(epoch + 30_000).toUTCString() } }))
    await refreshReadiness(true)
    renderHook(() => useTradingReadiness(true))
    fetcher.mockClear()
    await tick(9_000)
    expect(fetcher).not.toHaveBeenCalled()
    expect(readinessChecks(currentReadiness(), 'open')[0].status).toBe('unknown')
    await tick(1_000)
    expect(fetcher).toHaveBeenCalledTimes(1)
  })

  it('clears a fresh blocker on fetch failure and allows an explicit recovery refresh', async () => {
    paused = true
    await refreshReadiness(true)
    expect(readinessBlocker(currentReadiness(), 'open')?.reason).toBe('PAYMASTER_PAUSED')
    fetcher.mockRejectedValueOnce(new Error('offline'))
    await refreshReadiness(true)
    expect(currentReadiness()).toBeUndefined()
    expect(readinessBlocker(currentReadiness(), 'open')).toBeUndefined()
    const calls = fetcher.mock.calls.length
    await refreshReadiness()
    expect(fetcher).toHaveBeenCalledTimes(calls)
    paused = false
    await refreshReadiness(true)
    expect(readinessChecks(currentReadiness(), 'open')[0].status).toBe('ready')
  })

  it('does not poll for a disabled consumer', async () => {
    renderHook(() => useTradingReadiness(false))
    fetcher.mockClear()
    await tick(20_000)
    expect(fetcher).not.toHaveBeenCalled()
  })

  it('pauses polling while hidden and stops when the last consumer unmounts', async () => {
    const visibility = vi.spyOn(document, 'visibilityState', 'get').mockReturnValue('hidden')
    const { unmount } = renderHook(() => useTradingReadiness(true))
    fetcher.mockClear()
    await tick(20_000)
    expect(fetcher).not.toHaveBeenCalled()
    visibility.mockReturnValue('visible')
    await act(async () => { document.dispatchEvent(new Event('visibilitychange')) })
    expect(fetcher).toHaveBeenCalledTimes(1)
    unmount()
    await tick(20_000)
    expect(fetcher).toHaveBeenCalledTimes(1)
  })
})

describe('trading status panel', () => {
  it('retains the same panel and the user toggle across healthy, blocked and expired observations', async () => {
    const { container } = render(<TradingStatus />)
    const panel = container.querySelector('details')!
    expect(panel.open).toBe(false)
    await act(async () => {
      panel.open = true
      fireEvent(panel, new Event('toggle'))
      await vi.advanceTimersByTimeAsync(0)
    })
    paused = true
    await act(async () => { await refreshReadiness(true) })
    expect(screen.getByText('Trading status — temporarily unavailable')).toBeInTheDocument()
    expect(container.querySelector('details')).toBe(panel)
    expect(panel.open).toBe(true)
    await act(async () => {
      panel.open = false
      fireEvent(panel, new Event('toggle'))
      await vi.advanceTimersByTimeAsync(0)
    })
    paused = false
    await act(async () => { await refreshReadiness(true) })
    expect(screen.getByText('Trading status — ready')).toBeInTheDocument()
    expect(panel.open).toBe(false)
    let resolve!: (response: Response) => void
    fetcher.mockImplementationOnce(() => new Promise<Response>(done => { resolve = done }))
    await tick(4_000)
    expect(screen.getByText('Trading status — unverified')).toBeInTheDocument()
    expect(screen.getByText('Trading status is unavailable. Existing transaction security checks still apply.')).toBeInTheDocument()
    expect(container.querySelector('details')).toBe(panel)
    expect(panel.open).toBe(false)
    await act(async () => { resolve(response()) })
  })
})
