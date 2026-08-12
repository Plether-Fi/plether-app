import { describe, expect, it, vi } from 'vitest'
import { QueryClient } from '@tanstack/react-query'
import { apiQueryKeys, type BasketHistory, type BasketLatest } from '../api'
import {
  PletherDxyDatafeed,
  TRADINGVIEW_RESOLUTIONS,
  basketPointsToTradingViewBars,
  chartIntervalForTradingViewResolution,
  historyRangeForRequest,
  secondsForTradingViewResolution,
  tradingViewResolutionForInterval,
  type PletherChartDataSource,
} from './pletherDatafeed'
import type { TradingViewBar, TradingViewSymbolInfo } from './types'

const history: BasketHistory = {
  range: '24h',
  intervalSeconds: 60,
  source: 'pyth_benchmarks',
  generatedAt: 200,
  latestPrice: '97000000',
  changePct: null,
  points: [
    { timestamp: 60, basketPrice: '98000000', volumeUsdc: '1250000' },
    { timestamp: 90, basketPrice: '99000000', volumeUsdc: '2500000' },
    { timestamp: 119, basketPrice: '97000000' },
    { timestamp: 120, basketPrice: '96000000', volumeUsdc: '4000000' },
  ],
}

const latest: BasketLatest = {
  timestamp: 180,
  basketPrice: '95000000',
  components: [],
  generatedAt: 181,
  source: 'database',
}

function dataSource(overrides: Partial<PletherChartDataSource> = {}): PletherChartDataSource {
  return {
    getHistory: async () => history,
    getLatest: async () => undefined,
    ...overrides,
  }
}

function deferredValue<T>() {
  let resolve!: (value: T) => void
  const promise = new Promise<T>((resolvePromise) => {
    resolve = resolvePromise
  })
  return { promise, resolve }
}

describe('Plether TradingView datafeed', () => {
  it('maps the parent intervals and supports additional native minute resolutions', () => {
    expect(tradingViewResolutionForInterval('1m')).toBe('1')
    expect(tradingViewResolutionForInterval('5m')).toBe('5')
    expect(tradingViewResolutionForInterval('1h')).toBe('60')
    expect(tradingViewResolutionForInterval('1d')).toBe('1D')
    expect(chartIntervalForTradingViewResolution('1')).toBe('1m')
    expect(chartIntervalForTradingViewResolution('5')).toBe('5m')
    expect(chartIntervalForTradingViewResolution('60')).toBe('1h')
    expect(chartIntervalForTradingViewResolution('1D')).toBe('1d')
    expect(chartIntervalForTradingViewResolution('15')).toBeUndefined()
    expect(secondsForTradingViewResolution('3')).toBe(180)
    expect(secondsForTradingViewResolution('15')).toBe(900)
    expect(secondsForTradingViewResolution('30')).toBe(1_800)
    expect(secondsForTradingViewResolution('1D')).toBe(86_400)
  })

  it('aligns initial ranges with the existing chart queries and expands for older requests', () => {
    const now = 100 * 24 * 60 * 60

    expect(historyRangeForRequest(now - 60 * 60, now, 300, '1', now)).toBe('24h')
    expect(historyRangeForRequest(now - 60 * 60, now, 300, '3', now)).toBe('7d')
    expect(historyRangeForRequest(now - 60 * 60, now, 60, '5', now)).toBe('7d')
    expect(historyRangeForRequest(now - 60 * 60, now, 2_000, '5', now)).toBe('7d')
    expect(historyRangeForRequest(now - 60 * 60, now, 60, '15', now)).toBe('7d')
    expect(historyRangeForRequest(now - 60 * 60, now, 60, '30', now)).toBe('7d')
    expect(historyRangeForRequest(now - 60 * 60, now, 60, '60', now)).toBe('30d')
    expect(historyRangeForRequest(now - 60 * 60, now, 300, '60', now)).toBe('30d')
    expect(historyRangeForRequest(now - 60 * 60, now, 10, '1D', now)).toBe('1y')
    expect(historyRangeForRequest(now - 60 * 60, now, 100, '1D', now)).toBe('1y')
    expect(
      historyRangeForRequest(now - 60 * 24 * 60 * 60, now - 59 * 24 * 60 * 60, 100, '1', now)
    ).toBe('7d')
    expect(
      historyRangeForRequest(now - 60 * 24 * 60 * 60, now - 59 * 24 * 60 * 60, 24, '60', now)
    ).toBe('1y')
  })

  it('declares an OHLCV symbol with USDC volume precision', async () => {
    const feed = new PletherDxyDatafeed({ dataSource: dataSource() })

    const symbolInfo = await new Promise<TradingViewSymbolInfo>((resolve, reject) => {
      feed.resolveSymbol('PLDXY', resolve, reject)
    })

    expect(symbolInfo.visible_plots_set).toBe('ohlcv')
    expect(symbolInfo.volume_precision).toBe(2)
    expect(symbolInfo.supported_resolutions).toEqual(TRADINGVIEW_RESOLUTIONS)
    expect(symbolInfo.intraday_multipliers).toEqual(['1', '3', '5', '15', '30', '60'])
    expect(symbolInfo.daily_multipliers).toEqual(['1'])
  })

  it('converts Pyth samples and bucketed micro-USDC into plDXY OHLCV bars', () => {
    const bars = basketPointsToTradingViewBars(history.points, '1')

    expect(bars).toHaveLength(2)
    expect(bars[0]).toMatchObject({
      time: 60_000,
      open: 1.02,
      high: 1.03,
      low: 1.01,
      close: 1.03,
      volume: 3.75,
    })
    expect(bars[1]).toMatchObject({
      time: 120_000,
      open: 1.03,
      high: 1.04,
      low: 1.03,
      close: 1.04,
      volume: 4,
    })
  })

  it('zero-fills missing or invalid volume without dropping valid candles', () => {
    const bars = basketPointsToTradingViewBars([
      { timestamp: 60, basketPrice: '98000000' },
      { timestamp: 120, basketPrice: '97000000', volumeUsdc: '-1' },
      { timestamp: 180, basketPrice: '96000000', volumeUsdc: 'not-a-number' },
    ], '1')

    expect(bars.map((bar) => bar.volume)).toEqual([0, 0, 0])
  })

  it('preserves source-bucket volume when the oracle mark replaces its price sample', () => {
    const bars = basketPointsToTradingViewBars(
      [{ timestamp: 90, basketPrice: '97000000', volumeUsdc: '0' }],
      '1',
      [{ timestamp: 119, basketPrice: '98000000', volumeUsdc: '7500000' }]
    )

    expect(bars).toHaveLength(1)
    expect(bars[0].volume).toBe(7.5)
  })

  it('serves historical bars through the callback API', async () => {
    const feed = new PletherDxyDatafeed({ dataSource: dataSource() })

    const bars = await new Promise<TradingViewBar[]>((resolve, reject) => {
      feed.getBars(
        {} as TradingViewSymbolInfo,
        '1',
        { from: 0, to: 181, countBack: 100, firstDataRequest: true },
        (result) => resolve(result),
        reject
      )
    })

    expect(bars.map((bar) => bar.time)).toEqual([60_000, 120_000])
  })

  it('delivers chart bars asynchronously without exposing retained mutable objects', async () => {
    const feed = new PletherDxyDatafeed({
      dataSource: dataSource({ getLatest: async () => latest }),
      pollIntervalMs: 60_000,
    })
    let synchronous = true

    try {
      const bars = await new Promise<TradingViewBar[]>((resolve, reject) => {
        feed.getBars(
          {} as TradingViewSymbolInfo,
          '1',
          { from: 0, to: 181, countBack: 100, firstDataRequest: true },
          (result) => {
            expect(synchronous).toBe(false)
            resolve(result)
          },
          reject
        )
        synchronous = false
      })
      const lastBar = bars.at(-1)
      expect(lastBar).toBeDefined()
      if (lastBar) lastBar.close = 999

      const liveBar = await new Promise<TradingViewBar>((resolve) => {
        feed.subscribeBars(
          {} as TradingViewSymbolInfo,
          '1',
          resolve,
          'mutable-bar-listener',
          () => undefined
        )
      })
      expect(liveBar.close).toBe(1.05)
    } finally {
      feed.destroy()
    }
  })

  it('aborts an in-flight subscription request when the listener unsubscribes', async () => {
    let requestSignal: AbortSignal | undefined
    const feed = new PletherDxyDatafeed({
      dataSource: dataSource({
        getLatest: (signal) => new Promise((_resolve, reject) => {
          requestSignal = signal
          signal?.addEventListener('abort', () => reject(signal.reason), { once: true })
        }),
      }),
      pollIntervalMs: 60_000,
    })

    try {
      feed.subscribeBars(
        {} as TradingViewSymbolInfo,
        '1',
        () => undefined,
        'cancelled-listener',
        () => undefined
      )
      await vi.waitFor(() => expect(requestSignal).toBeDefined())

      feed.unsubscribeBars('cancelled-listener')

      expect(requestSignal?.aborted).toBe(true)
    } finally {
      feed.destroy()
    }
  })

  it('cancels a listener locally without aborting a shared React Query request', async () => {
    const queryClient = new QueryClient({
      defaultOptions: { queries: { retry: false } },
    })
    const pendingResponse = deferredValue<Response>()
    const latestResponse = {
      data: latest,
      meta: { blockNumber: 1, cached: false, chainId: 421_614 },
    }
    let transportSignal: AbortSignal | undefined
    const fetchSpy = vi.spyOn(globalThis, 'fetch').mockImplementation((_input, init) => {
      transportSignal = init?.signal as AbortSignal | undefined
      return pendingResponse.promise
    })
    const feed = new PletherDxyDatafeed({ queryClient, pollIntervalMs: 60_000 })

    try {
      feed.subscribeBars(
        {} as TradingViewSymbolInfo,
        '1',
        () => undefined,
        'shared-query-listener',
        () => undefined
      )
      await vi.waitFor(() => expect(transportSignal).toBeDefined())

      const sharedConsumer = queryClient.fetchQuery({
        queryKey: apiQueryKeys.perps.basketLatest(),
        queryFn: async (): Promise<typeof latestResponse> => {
          throw new Error('The in-flight query should be reused')
        },
        retry: false,
      })

      feed.unsubscribeBars('shared-query-listener')

      expect(transportSignal?.aborted).toBe(false)
      pendingResponse.resolve(new Response(JSON.stringify(latestResponse), {
        headers: { 'Content-Type': 'application/json' },
      }))

      await expect(sharedConsumer).resolves.toEqual(latestResponse)
      expect(queryClient.getQueryState(apiQueryKeys.perps.basketLatest())?.status).toBe('success')
    } finally {
      pendingResponse.resolve(new Response(JSON.stringify(latestResponse), {
        headers: { 'Content-Type': 'application/json' },
      }))
      feed.destroy()
      queryClient.clear()
      fetchSpy.mockRestore()
    }
  })

  it('does not start a historical request while the document is hidden', async () => {
    const visibilitySpy = vi.spyOn(document, 'visibilityState', 'get').mockReturnValue('hidden')
    const getHistory = vi.fn(async () => history)
    const getLatest = vi.fn(async () => latest)
    const feed = new PletherDxyDatafeed({
      dataSource: dataSource({ getHistory, getLatest }),
    })

    try {
      const message = await new Promise<string>((resolve, reject) => {
        feed.getBars(
          {} as TradingViewSymbolInfo,
          '1',
          { from: 0, to: 181, countBack: 100, firstDataRequest: true },
          () => reject(new Error('Hidden requests must not return bars')),
          resolve
        )
      })

      expect(message).toContain('hidden')
      expect(getHistory).not.toHaveBeenCalled()
      expect(getLatest).not.toHaveBeenCalled()
    } finally {
      feed.destroy()
      visibilitySpy.mockRestore()
    }
  })

  it('aborts an in-flight historical request when the document becomes hidden', async () => {
    let visibilityState: DocumentVisibilityState = 'visible'
    const visibilitySpy = vi.spyOn(document, 'visibilityState', 'get').mockImplementation(
      () => visibilityState
    )
    let historySignal: AbortSignal | undefined
    let latestSignal: AbortSignal | undefined
    const feed = new PletherDxyDatafeed({
      dataSource: dataSource({
        getHistory: (_range, _intervalSeconds, signal) => new Promise((_resolve, reject) => {
          historySignal = signal
          signal?.addEventListener('abort', () => reject(signal.reason), { once: true })
        }),
        getLatest: (signal) => new Promise((_resolve, reject) => {
          latestSignal = signal
          signal?.addEventListener('abort', () => reject(signal.reason), { once: true })
        }),
      }),
    })

    try {
      const errorMessage = new Promise<string>((resolve, reject) => {
        feed.getBars(
          {} as TradingViewSymbolInfo,
          '1',
          { from: 0, to: 181, countBack: 100, firstDataRequest: true },
          () => reject(new Error('An aborted history request must not return bars')),
          resolve
        )
      })
      await vi.waitFor(() => {
        expect(historySignal).toBeDefined()
        expect(latestSignal).toBeDefined()
      })

      visibilityState = 'hidden'
      document.dispatchEvent(new Event('visibilitychange'))

      await expect(errorMessage).resolves.toBeTruthy()
      expect(historySignal?.aborted).toBe(true)
      expect(latestSignal?.aborted).toBe(true)
    } finally {
      feed.destroy()
      visibilitySpy.mockRestore()
    }
  })

  it('reuses React Query basket data instead of issuing duplicate initial requests', async () => {
    const queryClient = new QueryClient({
      defaultOptions: { queries: { retry: false } },
    })
    const meta = { blockNumber: 1, cached: false, chainId: 421_614 }
    queryClient.setQueryData(apiQueryKeys.perps.basketHistory('7d', 60), {
      data: history,
      meta,
    })
    queryClient.setQueryData(apiQueryKeys.perps.basketLatest(), {
      data: latest,
      meta,
    })
    const feed = new PletherDxyDatafeed({ queryClient })

    const bars = await new Promise<TradingViewBar[]>((resolve, reject) => {
      feed.getBars(
        {} as TradingViewSymbolInfo,
        '1',
        { from: 0, to: 181, countBack: 100, firstDataRequest: true },
        (result) => resolve(result),
        reject
      )
    })

    expect(bars.map((bar) => bar.time)).toEqual([60_000, 120_000, 180_000])
    queryClient.clear()
  })

  it('does not let older pagination replace the current bar used for live updates', async () => {
    const feed = new PletherDxyDatafeed({
      dataSource: dataSource({ getLatest: async () => latest }),
      pollIntervalMs: 60_000,
    })

    try {
      const requestBars = (to: number) =>
        new Promise<TradingViewBar[]>((resolve, reject) => {
          feed.getBars(
            {} as TradingViewSymbolInfo,
            '1',
            { from: 0, to, countBack: 100, firstDataRequest: true },
            (result) => resolve(result),
            reject
          )
        })

      await requestBars(181)
      await requestBars(100)

      const liveBar = await new Promise<TradingViewBar>((resolve) => {
        feed.subscribeBars(
          {} as TradingViewSymbolInfo,
          '1',
          resolve,
          'pagination-listener',
          () => undefined
        )
      })

      expect(liveBar).toMatchObject({
        time: 180_000,
        open: 1.04,
        high: 1.05,
        low: 1.04,
        close: 1.05,
      })
    } finally {
      feed.destroy()
    }
  })

  it('pushes the latest basket value to live subscribers', async () => {
    const feed = new PletherDxyDatafeed({
      dataSource: dataSource({ getLatest: async () => latest }),
      pollIntervalMs: 60_000,
    })

    try {
      const bar = await new Promise<TradingViewBar>((resolve) => {
        feed.subscribeBars(
          {} as TradingViewSymbolInfo,
          '1',
          resolve,
          'listener',
          () => undefined
        )
      })

      expect(bar).toMatchObject({
        time: 180_000,
        open: 1.05,
        high: 1.05,
        low: 1.05,
        close: 1.05,
        volume: 0,
      })
    } finally {
      feed.destroy()
    }
  })

  it('resets cached REST bars when the authoritative oracle mark is older', async () => {
    const newerLatest = { ...latest, timestamp: 360 }
    const onResetCacheNeeded = vi.fn()
    const onHistoryGap = vi.fn()
    const feed = new PletherDxyDatafeed({
      dataSource: dataSource({ getLatest: async () => newerLatest }),
      pollIntervalMs: 60_000,
      onHistoryGap,
    })

    try {
      const ticks: TradingViewBar[] = []
      await new Promise<void>((resolve) => {
        feed.subscribeBars(
          {} as TradingViewSymbolInfo,
          '1',
          (bar) => {
            ticks.push(bar)
            resolve()
          },
          'older-oracle-listener',
          onResetCacheNeeded
        )
      })

      feed.setOracleMark({ timestamp: 180, basketPrice: '94000000' })
      await new Promise((resolve) => setTimeout(resolve, 0))

      expect(onResetCacheNeeded).toHaveBeenCalledOnce()
      expect(onHistoryGap).toHaveBeenCalledOnce()
      expect(ticks).toHaveLength(1)
    } finally {
      feed.destroy()
    }
  })

  it('updates live bucket volume cumulatively without adding repeated polling values', async () => {
    let now = 0
    let historyRequest = 0
    const dateNow = vi.spyOn(Date, 'now').mockImplementation(() => now)
    const getHistory = vi.fn(async (): Promise<BasketHistory> => ({
      ...history,
      points: [{
        timestamp: 180,
        basketPrice: '95000000',
        volumeUsdc: historyRequest++ === 0 ? '10500000' : '12750000',
      }],
    }))
    const feed = new PletherDxyDatafeed({
      dataSource: dataSource({ getHistory, getLatest: async () => latest }),
      pollIntervalMs: 1,
    })

    try {
      const volumes: number[] = []
      let resolveInitialVolume!: () => void
      let resolveUpdatedVolume!: () => void
      const initialVolume = new Promise<void>((resolve) => {
        resolveInitialVolume = resolve
      })
      const updatedVolume = new Promise<void>((resolve) => {
        resolveUpdatedVolume = resolve
      })

      feed.subscribeBars(
        {} as TradingViewSymbolInfo,
        '1',
        (bar) => {
          const volume = bar.volume ?? -1
          volumes.push(volume)
          if (volume === 10.5) resolveInitialVolume()
          if (volume === 12.75) resolveUpdatedVolume()
        },
        'volume-listener',
        () => undefined
      )

      await initialVolume
      now = 60_001
      await updatedVolume

      expect(volumes[0]).toBe(0)
      expect(volumes).toContain(10.5)
      expect(volumes.at(-1)).toBe(12.75)
      expect(getHistory).toHaveBeenCalledTimes(2)
    } finally {
      dateNow.mockRestore()
      feed.destroy()
    }
  })

  it('keeps a shared volume refresh alive when its initiating listener unsubscribes', async () => {
    const pendingHistory = deferredValue<BasketHistory>()
    let historySignal: AbortSignal | undefined
    const getLatest = vi.fn(async () => latest)
    const feed = new PletherDxyDatafeed({
      dataSource: dataSource({
        getHistory: (_range, _intervalSeconds, signal) => {
          historySignal = signal
          return pendingHistory.promise
        },
        getLatest,
      }),
      pollIntervalMs: 60_000,
    })
    const volumeHistory: BasketHistory = {
      ...history,
      points: [{ timestamp: 180, basketPrice: '95000000', volumeUsdc: '10500000' }],
    }

    try {
      feed.subscribeBars(
        {} as TradingViewSymbolInfo,
        '1',
        () => undefined,
        'volume-owner',
        () => undefined
      )
      await vi.waitFor(() => expect(historySignal).toBeDefined())

      let resolveFollowerReady!: () => void
      let resolveFollowerVolume!: () => void
      const followerReady = new Promise<void>((resolve) => {
        resolveFollowerReady = resolve
      })
      const followerVolume = new Promise<void>((resolve) => {
        resolveFollowerVolume = resolve
      })
      feed.subscribeBars(
        {} as TradingViewSymbolInfo,
        '1',
        (bar) => {
          resolveFollowerReady()
          if (bar.volume === 10.5) resolveFollowerVolume()
        },
        'volume-follower',
        () => undefined
      )
      await followerReady

      feed.unsubscribeBars('volume-owner')

      expect(historySignal?.aborted).toBe(false)
      pendingHistory.resolve(volumeHistory)
      await followerVolume
    } finally {
      pendingHistory.resolve(volumeHistory)
      feed.destroy()
    }
  })

  it('retries a live volume refresh promptly after a failed refresh', async () => {
    let historyRequest = 0
    const getHistory = vi.fn(async (): Promise<BasketHistory> => {
      historyRequest += 1
      if (historyRequest === 1) throw new Error('temporary history failure')
      return {
        ...history,
        points: [{ timestamp: 180, basketPrice: '95000000', volumeUsdc: '10500000' }],
      }
    })
    const feed = new PletherDxyDatafeed({
      dataSource: dataSource({ getHistory, getLatest: async () => latest }),
      pollIntervalMs: 1,
    })

    try {
      await new Promise<void>((resolve) => {
        feed.subscribeBars(
          {} as TradingViewSymbolInfo,
          '1',
          (bar) => {
            if (bar.volume === 10.5) resolve()
          },
          'failed-volume-listener',
          () => undefined
        )
      })

      expect(getHistory).toHaveBeenCalledTimes(2)
    } finally {
      feed.destroy()
    }
  })

  it('keeps live prices flowing during volume refresh and never reapplies a stale price', async () => {
    const historyRequest = deferredValue<BasketHistory>()
    const feed = new PletherDxyDatafeed({
      dataSource: dataSource({
        getHistory: () => historyRequest.promise,
        getLatest: async () => latest,
      }),
      pollIntervalMs: 60_000,
    })

    try {
      const bars: TradingViewBar[] = []
      let resolveFirstBar!: () => void
      let resolveVolumeCorrection!: () => void
      const firstBar = new Promise<void>((resolve) => {
        resolveFirstBar = resolve
      })
      const volumeCorrection = new Promise<void>((resolve) => {
        resolveVolumeCorrection = resolve
      })

      feed.subscribeBars(
        {} as TradingViewSymbolInfo,
        '1',
        (bar) => {
          bars.push(bar)
          if (bars.length === 1) resolveFirstBar()
          if (bar.volume === 10.5) resolveVolumeCorrection()
        },
        'slow-volume-listener',
        () => undefined
      )

      await firstBar
      expect(bars.at(-1)?.close).toBe(1.05)

      feed.setOracleMark({ timestamp: 180, basketPrice: '94000000' })
      await new Promise((resolve) => setTimeout(resolve, 0))
      expect(bars.at(-1)?.close).toBe(1.06)

      historyRequest.resolve({
        ...history,
        points: [{ timestamp: 180, basketPrice: '95000000', volumeUsdc: '10500000' }],
      })
      await volumeCorrection

      expect(bars.at(-1)).toMatchObject({ close: 1.06, volume: 10.5 })
    } finally {
      feed.destroy()
    }
  })

  it('does not apply a pending volume correction after a listener changes resolution', async () => {
    const minuteHistory = deferredValue<BasketHistory>()
    const threeMinuteHistory = deferredValue<BasketHistory>()
    const feed = new PletherDxyDatafeed({
      dataSource: dataSource({
        getHistory: (_range, intervalSeconds) => (
          intervalSeconds === 60 ? minuteHistory.promise : threeMinuteHistory.promise
        ),
        getLatest: async () => latest,
      }),
      pollIntervalMs: 60_000,
    })

    try {
      await new Promise<void>((resolve) => {
        feed.subscribeBars(
          {} as TradingViewSymbolInfo,
          '1',
          () => resolve(),
          'reused-listener',
          () => undefined
        )
      })

      const threeMinuteBars: TradingViewBar[] = []
      await new Promise<void>((resolve) => {
        feed.subscribeBars(
          {} as TradingViewSymbolInfo,
          '3',
          (bar) => {
            threeMinuteBars.push(bar)
            resolve()
          },
          'reused-listener',
          () => undefined
        )
      })

      minuteHistory.resolve({
        ...history,
        points: [{ timestamp: 180, basketPrice: '95000000', volumeUsdc: '10500000' }],
      })
      await new Promise((resolve) => setTimeout(resolve, 0))

      expect(threeMinuteBars.map((bar) => bar.volume)).toEqual([0])
    } finally {
      feed.destroy()
    }
  })

  it('does not let older pagination postpone the current volume refresh', async () => {
    vi.useFakeTimers({ toFake: ['setInterval', 'clearInterval'] })
    let now = 0
    const dateNow = vi.spyOn(Date, 'now').mockImplementation(() => now)
    const currentHistory: BasketHistory = {
      ...history,
      points: [{ timestamp: 180, basketPrice: '95000000', volumeUsdc: '10500000' }],
    }
    const getHistory = vi.fn(async () => currentHistory)
    const feed = new PletherDxyDatafeed({
      dataSource: dataSource({ getHistory, getLatest: async () => latest }),
      pollIntervalMs: 5_000,
    })

    try {
      const initialVolume = new Promise<void>((resolve) => {
        feed.subscribeBars(
          {} as TradingViewSymbolInfo,
          '1',
          (bar) => {
            if (bar.volume === 10.5) resolve()
          },
          'pagination-volume-listener',
          () => undefined
        )
      })
      await initialVolume
      expect(getHistory).toHaveBeenCalledOnce()

      now = 59_000
      await new Promise<TradingViewBar[]>((resolve, reject) => {
        feed.getBars(
          {} as TradingViewSymbolInfo,
          '1',
          { from: 0, to: 100, countBack: 100, firstDataRequest: false },
          resolve,
          reject
        )
      })
      expect(getHistory).toHaveBeenCalledTimes(2)

      now = 60_001
      await vi.advanceTimersByTimeAsync(5_000)
      expect(getHistory).toHaveBeenCalledTimes(3)
    } finally {
      feed.destroy()
      dateNow.mockRestore()
      vi.useRealTimers()
    }
  })

  it('resets the TradingView cache when polling detects missing candle buckets', async () => {
    const values = [latest, { ...latest, timestamp: 360 }]
    let requestIndex = 0
    const onResetCacheNeeded = vi.fn()
    const onHistoryGap = vi.fn()
    const feed = new PletherDxyDatafeed({
      dataSource: dataSource({
        getLatest: async () => values[Math.min(requestIndex++, values.length - 1)],
      }),
      pollIntervalMs: 1,
      onHistoryGap,
    })

    try {
      await new Promise<void>((resolve) => {
        let updates = 0
        feed.subscribeBars(
          {} as TradingViewSymbolInfo,
          '1',
          () => {
            updates += 1
            if (updates === 2) resolve()
          },
          'gap-listener',
          onResetCacheNeeded
        )
      })

      expect(onResetCacheNeeded).toHaveBeenCalledOnce()
      expect(onHistoryGap).toHaveBeenCalledOnce()
    } finally {
      feed.destroy()
    }
  })
})
