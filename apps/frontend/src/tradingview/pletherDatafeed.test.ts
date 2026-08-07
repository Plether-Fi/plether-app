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
    expect(symbolInfo.intraday_multipliers).toEqual(['1', '5', '60'])
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

  it('reuses React Query basket data instead of issuing duplicate initial requests', async () => {
    const queryClient = new QueryClient({
      defaultOptions: { queries: { retry: false } },
    })
    const meta = { blockNumber: 1, cached: false, chainId: 421_614 }
    queryClient.setQueryData(apiQueryKeys.perps.basketHistory('1y', 60), {
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
