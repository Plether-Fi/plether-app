import { describe, expect, it, vi } from 'vitest'
import { QueryClient } from '@tanstack/react-query'
import { apiQueryKeys, type BasketHistory, type BasketLatest } from '../api'
import {
  PletherDxyDatafeed,
  basketPointsToTradingViewBars,
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
    { timestamp: 60, basketPrice: '98000000' },
    { timestamp: 90, basketPrice: '99000000' },
    { timestamp: 119, basketPrice: '97000000' },
    { timestamp: 120, basketPrice: '96000000' },
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

describe('Plether TradingView datafeed', () => {
  it('maps the existing UI intervals to TradingView resolutions', () => {
    expect(tradingViewResolutionForInterval('1m')).toBe('1')
    expect(tradingViewResolutionForInterval('5m')).toBe('5')
    expect(tradingViewResolutionForInterval('1h')).toBe('60')
    expect(tradingViewResolutionForInterval('1d')).toBe('1D')
    expect(secondsForTradingViewResolution('1D')).toBe(86_400)
  })

  it('aligns initial ranges with the existing chart queries and expands for older requests', () => {
    const now = 100 * 24 * 60 * 60

    expect(historyRangeForRequest(now - 60 * 60, now, 300, '1', now)).toBe('24h')
    expect(historyRangeForRequest(now - 60 * 60, now, 60, '5', now)).toBe('7d')
    expect(historyRangeForRequest(now - 60 * 60, now, 2_000, '5', now)).toBe('7d')
    expect(historyRangeForRequest(now - 60 * 60, now, 60, '60', now)).toBe('30d')
    expect(historyRangeForRequest(now - 60 * 60, now, 300, '60', now)).toBe('30d')
    expect(historyRangeForRequest(now - 60 * 60, now, 10, '1D', now)).toBe('1y')
    expect(historyRangeForRequest(now - 60 * 60, now, 100, '1D', now)).toBe('1y')
    expect(
      historyRangeForRequest(now - 60 * 24 * 60 * 60, now - 59 * 24 * 60 * 60, 24, '60', now)
    ).toBe('1y')
  })

  it('declares an OHLC-only symbol without misleading volume support', async () => {
    const feed = new PletherDxyDatafeed({ dataSource: dataSource() })

    const symbolInfo = await new Promise<TradingViewSymbolInfo>((resolve, reject) => {
      feed.resolveSymbol('PLDXY', resolve, reject)
    })

    expect(symbolInfo.visible_plots_set).toBe('ohlc')
  })

  it('converts Pyth basket samples into reversed plDXY OHLC bars', () => {
    const bars = basketPointsToTradingViewBars(history.points, '1')

    expect(bars).toHaveLength(2)
    expect(bars[0]).toMatchObject({
      time: 60_000,
      open: 1.02,
      high: 1.03,
      low: 1.01,
      close: 1.03,
    })
    expect(bars[1]).toMatchObject({
      time: 120_000,
      open: 1.03,
      high: 1.04,
      low: 1.03,
      close: 1.04,
    })
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
      })
    } finally {
      feed.destroy()
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
