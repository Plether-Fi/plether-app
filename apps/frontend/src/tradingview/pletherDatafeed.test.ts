import { describe, expect, it, vi } from 'vitest'
import { QueryClient } from '@tanstack/react-query'
import {
  apiQueryKeys,
  type BasketHistory,
  type BasketLatest,
  type PerpsBasketCandle,
  type PerpsBasketCandlePage,
  type PerpsBasketCurrentCandle,
  type PerpsCandleIntervalSeconds,
} from '../api'
import {
  PletherDxyDatafeed,
  PERPS_CANDLE_MAX_HISTORY_PAGES,
  TRADINGVIEW_RESOLUTIONS,
  basketPointsToTradingViewBars,
  chartIntervalForTradingViewResolution,
  historyRangeForRequest,
  isPerpsCandleApiEnabled,
  candlePageCursorForRequest,
  perpsBasketCandlesToTradingViewBars,
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

function deferredValue<T>() {
  let resolve!: (value: T) => void
  const promise = new Promise<T>((resolvePromise) => {
    resolve = resolvePromise
  })
  return { promise, resolve }
}

const CANDLE_IDENTITY = {
  seriesId: 'dxy-v1',
  configurationHash: 'sha256:test-configuration',
  displayPriceCap: '200000000',
  volumeChainId: 421_614,
  volumeRouter: '0x1111111111111111111111111111111111111111',
} as const

const CURRENT_VOLUME_COVERAGE = {
  volumeCoverageStart: 0,
  volumeCoverageEnd: 64_800,
  volumeFinalizedThrough: 64_800,
  volumeCoverageComplete: true,
} as const

function rawCandle(
  timestamp: number,
  overrides: Partial<PerpsBasketCandle> = {}
): PerpsBasketCandle {
  return {
    timestamp,
    rawOpenPrice: '98000000',
    rawHighPrice: '101000000',
    rawLowPrice: '97000000',
    rawClosePrice: '99000000',
    volumeUsdc: '1250000',
    tradeCount: 2,
    sampleCount: 3,
    quality: 'observed',
    revision: 1,
    priceComplete: true,
    volumeComplete: true,
    complete: true,
    ...overrides,
  }
}

function candlePage(
  cursor: number,
  candles: PerpsBasketCandle[],
  overrides: Partial<PerpsBasketCandlePage> = {}
): PerpsBasketCandlePage {
  const intervalSeconds = overrides.intervalSeconds ?? 60
  const defaultVolumeEnd = Math.ceil(64_800 / intervalSeconds) * intervalSeconds
  return {
    intervalSeconds: 60,
    cursor,
    ...CANDLE_IDENTITY,
    previousCursor: cursor > 30_000 ? cursor - 30_000 : null,
    hasEarlier: cursor > 30_000,
    coverageStart: 0,
    coverageEnd: 64_800,
    coverageComplete: true,
    finalizedThrough: 64_800,
    volumeCoverageStart: 0,
    volumeCoverageEnd: defaultVolumeEnd,
    volumeFinalizedThrough: defaultVolumeEnd,
    volumeCoverageComplete: true,
    datasetGeneration: 7,
    candles,
    ...overrides,
  }
}

function currentCandle(
  intervalSeconds: PerpsCandleIntervalSeconds = 60,
  overrides: Partial<PerpsBasketCurrentCandle> = {}
): PerpsBasketCurrentCandle {
  const defaultVolumeEnd = Math.ceil(64_800 / intervalSeconds) * intervalSeconds
  return {
    intervalSeconds,
    ...CANDLE_IDENTITY,
    coverageStart: 0,
    coverageEnd: 64_800,
    coverageComplete: true,
    finalizedThrough: 64_800,
    volumeCoverageStart: 0,
    volumeCoverageEnd: defaultVolumeEnd,
    volumeFinalizedThrough: defaultVolumeEnd,
    volumeCoverageComplete: true,
    datasetGeneration: 7,
    candle: null,
    ...overrides,
  }
}

function dataSource(overrides: Partial<PletherChartDataSource> = {}): PletherChartDataSource {
  return {
    getHistory: async () => history,
    getLatest: async () => undefined,
    getCurrentCandle: async (intervalSeconds) => currentCandle(intervalSeconds),
    ...overrides,
  }
}

describe('Plether TradingView datafeed', () => {
  it('keeps the candle API behind an explicit opt-in flag', () => {
    expect(isPerpsCandleApiEnabled(undefined)).toBe(false)
    expect(isPerpsCandleApiEnabled('false')).toBe(false)
    expect(isPerpsCandleApiEnabled('0')).toBe(false)
    expect(isPerpsCandleApiEnabled('true')).toBe(true)
    expect(isPerpsCandleApiEnabled('1')).toBe(true)
  })

  it('aligns fixed pages and converts raw backend OHLCV without rebuilding candles', () => {
    expect(candlePageCursorForRequest(65_000, 60)).toBe(90_000)
    expect(perpsBasketCandlesToTradingViewBars(
      [rawCandle(64_920)],
      60,
      CANDLE_IDENTITY.displayPriceCap
    )).toEqual([{
      time: 64_920_000,
      open: 1.02,
      high: 1.03,
      low: 0.99,
      close: 1.01,
      volume: 1.25,
    }])
  })

  it('preserves unknown rollup volume instead of displaying it as proven zero', () => {
    expect(perpsBasketCandlesToTradingViewBars([
      rawCandle(64_920, { volumeUsdc: null, tradeCount: null, volumeComplete: false }),
    ], 60, CANDLE_IDENTITY.displayPriceCap)).toEqual([{
      time: 64_920_000,
      open: 1.02,
      high: 1.03,
      low: 0.99,
      close: 1.01,
    }])
  })

  it('publishes accepted per-interval volume transitions and suppresses duplicates', async () => {
    const onVolumeCoverageChange = vi.fn()
    const pages = [
      candlePage(90_000, [rawCandle(64_920, {
        volumeUsdc: null,
        tradeCount: null,
        volumeComplete: false,
      })], {
        hasEarlier: false,
        previousCursor: null,
        volumeCoverageStart: null,
        volumeCoverageEnd: null,
        volumeFinalizedThrough: null,
        volumeCoverageComplete: false,
        datasetGeneration: 134_217_728,
      }),
      candlePage(90_000, [rawCandle(64_920)], {
        hasEarlier: false,
        previousCursor: null,
        datasetGeneration: 134_217_731,
      }),
      candlePage(90_000, [rawCandle(64_920)], {
        hasEarlier: false,
        previousCursor: null,
        datasetGeneration: 134_217_731,
      }),
    ]
    const getCandlePage = vi.fn(async () => pages.shift() ?? pages[0])
    const feed = new PletherDxyDatafeed({
      dataSource: dataSource({ getCandlePage }),
      onVolumeCoverageChange,
      useCandleApi: true,
    })
    const load = () => new Promise<TradingViewBar[]>((resolve, reject) => {
      feed.getBars(
        {} as TradingViewSymbolInfo,
        '1',
        { from: 0, to: 65_000, countBack: 1, firstDataRequest: false },
        resolve,
        reject
      )
    })

    try {
      await load()
      await load()
      await load()

      expect(onVolumeCoverageChange.mock.calls).toEqual([
        [{ intervalSeconds: 60, state: 'unavailable' }],
        [{ intervalSeconds: 60, state: 'available' }],
      ])
    } finally {
      feed.destroy()
    }
  })

  it('tracks volume coverage independently when the chart interval changes', async () => {
    const onVolumeCoverageChange = vi.fn()
    const getCandlePage = vi.fn(async (intervalSeconds: PerpsCandleIntervalSeconds, cursor: number) => (
      intervalSeconds === 60
        ? candlePage(cursor, [], {
            hasEarlier: false,
            previousCursor: null,
            volumeCoverageStart: null,
            volumeCoverageEnd: null,
            volumeFinalizedThrough: null,
            volumeCoverageComplete: false,
            datasetGeneration: 134_217_728,
          })
        : candlePage(cursor, [], {
            intervalSeconds,
            hasEarlier: false,
            previousCursor: null,
            coverageEnd: 64_800,
            finalizedThrough: 64_800,
            volumeCoverageEnd: 64_800,
            volumeFinalizedThrough: 64_800,
            datasetGeneration: 134_217_731,
          })
    ))
    const feed = new PletherDxyDatafeed({
      dataSource: dataSource({ getCandlePage }),
      onVolumeCoverageChange,
      useCandleApi: true,
    })
    const load = (resolution: '1' | '5') => new Promise<TradingViewBar[]>((resolve, reject) => {
      feed.getBars(
        {} as TradingViewSymbolInfo,
        resolution,
        { from: 0, to: 65_000, countBack: 1, firstDataRequest: false },
        resolve,
        reject
      )
    })

    try {
      await load('1')
      await load('5')

      expect(onVolumeCoverageChange.mock.calls).toEqual([
        [{ intervalSeconds: 60, state: 'unavailable' }],
        [{ intervalSeconds: 300, state: 'available' }],
      ])
    } finally {
      feed.destroy()
    }
  })

  it('publishes volume coverage from an accepted current-candle response', async () => {
    const onVolumeCoverageChange = vi.fn()
    const getCurrentCandle = vi.fn(async () => currentCandle(900, {
      volumeCoverageStart: null,
      volumeCoverageEnd: null,
      volumeFinalizedThrough: null,
      volumeCoverageComplete: false,
      datasetGeneration: 134_217_728,
    }))
    const feed = new PletherDxyDatafeed({
      dataSource: dataSource({ getCurrentCandle }),
      onVolumeCoverageChange,
      pollIntervalMs: 60_000,
      useCandleApi: true,
    })

    try {
      feed.subscribeBars(
        {} as TradingViewSymbolInfo,
        '15',
        vi.fn(),
        'volume-current',
        vi.fn()
      )
      await vi.waitFor(() => {
        expect(onVolumeCoverageChange).toHaveBeenCalledWith({
          intervalSeconds: 900,
          state: 'unavailable',
        })
      })
    } finally {
      feed.destroy()
    }
  })

  it('does not publish malformed, stale, or rejected mixed-identity volume metadata', async () => {
    const onVolumeCoverageChange = vi.fn()
    const responses = new Map<number, PerpsBasketCandlePage>([
      [90_000, candlePage(90_000, [rawCandle(64_920, {
        volumeUsdc: null,
        tradeCount: null,
        volumeComplete: false,
      })], {
        volumeCoverageStart: null,
        volumeCoverageEnd: null,
        volumeFinalizedThrough: null,
        volumeCoverageComplete: false,
        datasetGeneration: 134_217_728,
      })],
      [60_000, candlePage(60_000, [rawCandle(59_940)], {
        hasEarlier: false,
        previousCursor: null,
        volumeRouter: '0x2222222222222222222222222222222222222222',
        datasetGeneration: 134_217_731,
      })],
    ])
    const mixedFeed = new PletherDxyDatafeed({
      dataSource: dataSource({
        getCandlePage: async (_interval, cursor) => {
          const response = responses.get(cursor)
          if (!response) throw new Error(`Unexpected cursor ${cursor.toString()}`)
          return response
        },
      }),
      onVolumeCoverageChange,
      useCandleApi: true,
    })
    const malformedFeed = new PletherDxyDatafeed({
      dataSource: dataSource({
        getCandlePage: async (_interval, cursor) => candlePage(cursor, [], {
          hasEarlier: false,
          previousCursor: null,
          volumeCoverageStart: null,
          volumeCoverageEnd: null,
          volumeFinalizedThrough: null,
          volumeCoverageComplete: true,
        }),
      }),
      onVolumeCoverageChange,
      useCandleApi: true,
    })
    const load = (feed: PletherDxyDatafeed, countBack: number) => new Promise<void>((resolve) => {
      feed.getBars(
        {} as TradingViewSymbolInfo,
        '1',
        { from: 0, to: 65_000, countBack, firstDataRequest: false },
        () => resolve(),
        () => resolve()
      )
    })

    try {
      await load(mixedFeed, 2)
      await load(malformedFeed, 1)
      expect(onVolumeCoverageChange).not.toHaveBeenCalled()
    } finally {
      mixedFeed.destroy()
      malformedFeed.destroy()
    }

    const staleUpdates = vi.fn()
    let response = candlePage(90_000, [], {
      hasEarlier: false,
      previousCursor: null,
      datasetGeneration: 134_217_731,
    })
    const staleFeed = new PletherDxyDatafeed({
      dataSource: dataSource({ getCandlePage: async () => response }),
      onVolumeCoverageChange: staleUpdates,
      useCandleApi: true,
    })
    try {
      await load(staleFeed, 1)
      response = candlePage(90_000, [], {
        hasEarlier: false,
        previousCursor: null,
        volumeCoverageStart: null,
        volumeCoverageEnd: null,
        volumeFinalizedThrough: null,
        volumeCoverageComplete: false,
        datasetGeneration: 134_217_728,
      })
      await load(staleFeed, 1)
      expect(staleUpdates.mock.calls).toEqual([
        [{ intervalSeconds: 60, state: 'available' }],
      ])
    } finally {
      staleFeed.destroy()
    }
  })

  it('walks fixed pages until countBack actual candles are collected across gaps', async () => {
    const getHistory = vi.fn(async () => history)
    const getLatest = vi.fn(async () => latest)
    const getCurrentCandle = vi.fn(async (intervalSeconds: PerpsCandleIntervalSeconds) => (
      currentCandle(intervalSeconds)
    ))
    const getCandlePage = vi.fn(async (_interval: number, cursor: number) => {
      if (cursor === 90_000) {
        return candlePage(cursor, [rawCandle(64_860), rawCandle(64_920)])
      }
      if (cursor === 60_000) {
        return candlePage(cursor, [rawCandle(30_000), rawCandle(59_940)], {
          hasEarlier: false,
          previousCursor: null,
        })
      }
      throw new Error(`Unexpected cursor ${cursor.toString()}`)
    })
    const feed = new PletherDxyDatafeed({
      dataSource: { getHistory, getLatest, getCandlePage, getCurrentCandle },
      useCandleApi: true,
    })

    try {
      const bars = await new Promise<TradingViewBar[]>((resolve, reject) => {
        feed.getBars(
          {} as TradingViewSymbolInfo,
          '1',
          { from: 64_800, to: 65_000, countBack: 4, firstDataRequest: true },
          resolve,
          reject
        )
      })

      expect(bars.map((bar) => bar.time)).toEqual([
        30_000_000,
        59_940_000,
        64_860_000,
        64_920_000,
      ])
      expect(getCandlePage.mock.calls.map(([, cursor]) => cursor)).toEqual([90_000, 60_000])
      expect(getHistory).not.toHaveBeenCalled()
      expect(getLatest).not.toHaveBeenCalled()
      expect(getCurrentCandle).toHaveBeenCalledOnce()
    } finally {
      feed.destroy()
    }
  })

  it('coalesces the first current candle into history before normal polling begins', async () => {
    vi.useFakeTimers({ toFake: ['setInterval', 'clearInterval'] })
    const getCandlePage = vi.fn(async (_interval: number, cursor: number) => candlePage(
      cursor,
      [rawCandle(64_920)],
      { hasEarlier: false, previousCursor: null }
    ))
    const getCurrentCandle = vi.fn(async () => currentCandle(60, {
      candle: rawCandle(64_980, { volumeUsdc: '10750000' }),
    }))
    const feed = new PletherDxyDatafeed({
      dataSource: dataSource({ getCandlePage, getCurrentCandle }),
      useCandleApi: true,
      pollIntervalMs: 60_000,
    })

    try {
      const bars = await new Promise<TradingViewBar[]>((resolve, reject) => {
        feed.getBars(
          {} as TradingViewSymbolInfo,
          '1',
          { from: 0, to: 65_000, countBack: 2, firstDataRequest: true },
          resolve,
          reject
        )
      })
      expect(bars.map((bar) => bar.time)).toEqual([64_920_000, 64_980_000])
      expect(bars.at(-1)?.volume).toBe(10.75)
      expect(getCurrentCandle).toHaveBeenCalledOnce()

      const onTick = vi.fn()
      feed.subscribeBars(
        {} as TradingViewSymbolInfo,
        '1',
        onTick,
        'primed-current-listener',
        () => undefined
      )
      await new Promise((resolve) => setTimeout(resolve, 0))

      expect(getCurrentCandle).toHaveBeenCalledOnce()
      expect(onTick).not.toHaveBeenCalled()

      await vi.advanceTimersByTimeAsync(60_000)
      expect(getCurrentCandle).toHaveBeenCalledTimes(2)
    } finally {
      feed.destroy()
      vi.useRealTimers()
    }
  })

  it('does not delay first history when the optional current-candle request fails', async () => {
    const getCandlePage = vi.fn(async (_interval: number, cursor: number) => candlePage(
      cursor,
      [rawCandle(64_920)],
      { hasEarlier: false, previousCursor: null }
    ))
    const getCurrentCandle = vi.fn(async () => {
      throw new Error('current endpoint unavailable')
    })
    const feed = new PletherDxyDatafeed({
      dataSource: dataSource({ getCandlePage, getCurrentCandle }),
      useCandleApi: true,
    })

    try {
      const bars = await new Promise<TradingViewBar[]>((resolve, reject) => {
        feed.getBars(
          {} as TradingViewSymbolInfo,
          '1',
          { from: 0, to: 65_000, countBack: 2, firstDataRequest: true },
          resolve,
          reject
        )
      })

      expect(bars.map((bar) => bar.time)).toEqual([64_920_000])
      expect(getCandlePage).toHaveBeenCalledOnce()
      expect(getCurrentCandle).toHaveBeenCalledOnce()
    } finally {
      feed.destroy()
    }
  })

  it('returns no data without requesting a daily page before known coverage', async () => {
    const now = vi.spyOn(Date, 'now').mockReturnValue(1_786_986_000_000)
    const intervalSeconds = 86_400 as const
    const currentCursor = 1_814_400_000
    const inceptionCursor = 1_771_200_000
    const coverageStart = 1_767_225_600
    const coverageEnd = 1_786_924_800
    const getCandlePage = vi.fn(async (interval: number, cursor: number) => {
      expect(interval).toBe(intervalSeconds)
      if (cursor === currentCursor) {
        return candlePage(cursor, [rawCandle(1_786_838_400)], {
          intervalSeconds,
          coverageStart,
          coverageEnd,
          finalizedThrough: coverageEnd,
          previousCursor: inceptionCursor,
          hasEarlier: true,
        })
      }
      if (cursor === inceptionCursor) {
        return candlePage(cursor, [rawCandle(coverageStart)], {
          intervalSeconds,
          coverageStart,
          coverageEnd,
          finalizedThrough: coverageEnd,
          previousCursor: null,
          hasEarlier: false,
        })
      }
      throw new Error(`Unexpected pre-coverage cursor ${cursor.toString()}`)
    })
    const feed = new PletherDxyDatafeed({
      dataSource: dataSource({ getCandlePage }),
      useCandleApi: true,
    })
    const requestBars = (to: number) => new Promise<{
      bars: TradingViewBar[]
      metadata: { noData: boolean }
    }>((resolve, reject) => {
      feed.getBars(
        {} as TradingViewSymbolInfo,
        '1D',
        { from: 0, to, countBack: 300, firstDataRequest: true },
        (bars, metadata) => resolve({ bars, metadata }),
        reject
      )
    })

    try {
      const available = await requestBars(1_786_986_000)
      expect(available.bars.map((bar) => bar.time)).toEqual([
        coverageStart * 1_000,
        1_786_838_400_000,
      ])

      await expect(requestBars(coverageStart)).resolves.toEqual({
        bars: [],
        metadata: { noData: true },
      })
      expect(getCandlePage.mock.calls.map(([, cursor]) => cursor)).toEqual([
        currentCursor,
        inceptionCursor,
      ])
    } finally {
      feed.destroy()
      now.mockRestore()
    }
  })

  it('refreshes the known coverage boundary when the dataset generation advances', async () => {
    const now = vi.spyOn(Date, 'now').mockReturnValue(1_786_986_000_000)
    const intervalSeconds = 86_400 as const
    const currentCursor = 1_814_400_000
    const inceptionCursor = 1_771_200_000
    const originalCoverageStart = 1_767_225_600
    const expandedCoverageStart = originalCoverageStart - intervalSeconds
    const coverageEnd = 1_786_924_800
    let datasetGeneration = 7
    let coverageStart = originalCoverageStart
    const getCandlePage = vi.fn(async (interval: number, cursor: number) => {
      expect(interval).toBe(intervalSeconds)
      if (cursor === currentCursor) {
        return candlePage(cursor, [rawCandle(1_786_838_400)], {
          intervalSeconds,
          datasetGeneration,
          coverageStart,
          coverageEnd,
          finalizedThrough: coverageEnd,
          previousCursor: inceptionCursor,
          hasEarlier: true,
        })
      }
      if (cursor === inceptionCursor) {
        return candlePage(cursor, [rawCandle(coverageStart)], {
          intervalSeconds,
          datasetGeneration,
          coverageStart,
          coverageEnd,
          finalizedThrough: coverageEnd,
          previousCursor: null,
          hasEarlier: false,
        })
      }
      throw new Error(`Unexpected cursor ${cursor.toString()}`)
    })
    const feed = new PletherDxyDatafeed({
      dataSource: dataSource({ getCandlePage }),
      useCandleApi: true,
    })
    const requestBars = (to: number, countBack: number) => new Promise<TradingViewBar[]>(
      (resolve, reject) => {
        feed.getBars(
          {} as TradingViewSymbolInfo,
          '1D',
          { from: 0, to, countBack, firstDataRequest: true },
          resolve,
          reject
        )
      }
    )

    try {
      await expect(requestBars(1_786_986_000, 300)).resolves.toHaveLength(2)
      await expect(requestBars(originalCoverageStart, 300)).resolves.toEqual([])
      expect(getCandlePage).toHaveBeenCalledTimes(2)

      datasetGeneration = 8
      coverageStart = expandedCoverageStart
      await expect(requestBars(1_786_986_000, 1)).resolves.toHaveLength(1)
      await expect(requestBars(originalCoverageStart, 1)).resolves.toEqual([
        expect.objectContaining({ time: expandedCoverageStart * 1_000 }),
      ])

      expect(getCandlePage).toHaveBeenCalledTimes(4)
      expect(getCandlePage.mock.calls.at(-1)?.[1]).toBe(inceptionCursor)
    } finally {
      feed.destroy()
      now.mockRestore()
    }
  })

  it('does not reuse an interval/cursor React Query entry across Worker probes', async () => {
    const queryClient = new QueryClient({ defaultOptions: { queries: { retry: false } } })
    const cursor = 90_000
    const stalePage = candlePage(cursor, [rawCandle(64_920)], {
      datasetGeneration: 6,
      hasEarlier: false,
      previousCursor: null,
    })
    queryClient.setQueryData(apiQueryKeys.perps.basketCandles(60, cursor), {
      data: stalePage,
      meta: { blockNumber: 1, cached: true, chainId: 421_614 },
    })
    const freshPage = candlePage(cursor, [rawCandle(64_920)], {
      datasetGeneration: 7,
      hasEarlier: false,
      previousCursor: null,
    })
    const fetchSpy = vi.spyOn(globalThis, 'fetch').mockImplementation(async () => (
      new Response(JSON.stringify({
        data: freshPage,
        meta: { blockNumber: 2, cached: false, chainId: 421_614 },
      }), { headers: { 'Content-Type': 'application/json' } })
    ))
    const feed = new PletherDxyDatafeed({ queryClient, useCandleApi: true })
    const requestBars = () => new Promise<TradingViewBar[]>((resolve, reject) => {
      feed.getBars(
        {} as TradingViewSymbolInfo,
        '1',
        { from: 0, to: 65_000, countBack: 1, firstDataRequest: false },
        resolve,
        reject
      )
    })

    try {
      await expect(requestBars()).resolves.toHaveLength(1)
      await expect(requestBars()).resolves.toHaveLength(1)
      expect(fetchSpy).toHaveBeenCalledTimes(2)
      expect(String(fetchSpy.mock.calls[0]?.[0])).toContain(
        '/api/perps/v1/perps/basket/candles?interval=60&cursor=90000'
      )
    } finally {
      feed.destroy()
      queryClient.clear()
      fetchSpy.mockRestore()
    }
  })

  it('clamps a future TradingView range to the local current fixed page', async () => {
    const now = vi.spyOn(Date, 'now').mockReturnValue(65_000_000)
    const getCandlePage = vi.fn(async (_interval: number, cursor: number) => candlePage(
      cursor,
      [rawCandle(64_920)],
      { hasEarlier: false, previousCursor: null }
    ))
    const feed = new PletherDxyDatafeed({
      dataSource: dataSource({ getCandlePage }),
      useCandleApi: true,
    })

    try {
      const bars = await new Promise<TradingViewBar[]>((resolve, reject) => {
        feed.getBars(
          {} as TradingViewSymbolInfo,
          '1',
          { from: 0, to: 120_001, countBack: 1, firstDataRequest: true },
          resolve,
          reject
        )
      })

      expect(getCandlePage).toHaveBeenCalledWith(60, 90_000, expect.any(AbortSignal))
      expect(bars.map((bar) => bar.time)).toEqual([64_920_000])
    } finally {
      feed.destroy()
      now.mockRestore()
    }
  })

  it('caps one native history traversal at 24 fixed pages', async () => {
    const now = vi.spyOn(Date, 'now').mockReturnValue(750_000_000)
    const getCandlePage = vi.fn(async (_interval: number, cursor: number) => candlePage(
      cursor,
      [rawCandle(cursor - 60)],
      { previousCursor: cursor - 30_000, hasEarlier: true }
    ))
    const feed = new PletherDxyDatafeed({
      dataSource: dataSource({ getCandlePage }),
      useCandleApi: true,
    })

    try {
      const bars = await new Promise<TradingViewBar[]>((resolve, reject) => {
        feed.getBars(
          {} as TradingViewSymbolInfo,
          '1',
          { from: 0, to: 750_000, countBack: 1_000, firstDataRequest: true },
          resolve,
          reject
        )
      })

      expect(getCandlePage).toHaveBeenCalledTimes(PERPS_CANDLE_MAX_HISTORY_PAGES)
      expect(bars).toHaveLength(PERPS_CANDLE_MAX_HISTORY_PAGES)
      expect(getCandlePage.mock.calls.at(-1)?.[1]).toBe(60_000)
    } finally {
      feed.destroy()
      now.mockRestore()
    }
  })

  it('follows an aligned previousCursor jump across empty weekend pages', async () => {
    const getCandlePage = vi.fn(async (_interval: number, cursor: number) => {
      if (cursor === 90_000) {
        return candlePage(cursor, [rawCandle(64_920)], {
          // The API can skip the empty [30_000, 60_000) page.
          previousCursor: 30_000,
          hasEarlier: true,
        })
      }
      if (cursor === 30_000) {
        return candlePage(cursor, [rawCandle(29_940)], {
          previousCursor: null,
          hasEarlier: false,
        })
      }
      throw new Error(`Unexpected cursor ${cursor.toString()}`)
    })
    const feed = new PletherDxyDatafeed({
      dataSource: dataSource({ getCandlePage }),
      useCandleApi: true,
    })

    try {
      const bars = await new Promise<TradingViewBar[]>((resolve, reject) => {
        feed.getBars(
          {} as TradingViewSymbolInfo,
          '1',
          { from: 0, to: 65_000, countBack: 2, firstDataRequest: true },
          resolve,
          reject
        )
      })

      expect(getCandlePage.mock.calls.map(([, cursor]) => cursor)).toEqual([90_000, 30_000])
      expect(bars.map((bar) => bar.time)).toEqual([29_940_000, 64_920_000])
    } finally {
      feed.destroy()
    }
  })

  it('rejects a dataset generation change during fixed-page traversal', async () => {
    const clearCandlePageCache = vi.fn()
    const getCandlePage = vi.fn(async (_interval: number, cursor: number) => (
      cursor === 90_000
        ? candlePage(cursor, [rawCandle(64_920)])
        : candlePage(cursor, [rawCandle(59_940)], {
            datasetGeneration: 8,
            hasEarlier: false,
            previousCursor: null,
          })
    ))
    const feed = new PletherDxyDatafeed({
      dataSource: dataSource({
        getCandlePage,
        clearCandlePageCache,
      }),
      useCandleApi: true,
    })

    try {
      await expect(new Promise<TradingViewBar[]>((resolve, reject) => {
        feed.getBars(
          {} as TradingViewSymbolInfo,
          '1',
          { from: 0, to: 65_000, countBack: 2, firstDataRequest: true },
          resolve,
          reject
        )
      })).rejects.toBe('The Perps candle dataset changed while history was loading')
      expect(clearCandlePageCache).toHaveBeenCalledOnce()
    } finally {
      feed.destroy()
    }
  })

  it('rejects mixed basket identities during fixed-page traversal', async () => {
    const clearCandlePageCache = vi.fn()
    const getCandlePage = vi.fn(async (_interval: number, cursor: number) => (
      cursor === 90_000
        ? candlePage(cursor, [rawCandle(64_920)])
        : candlePage(cursor, [rawCandle(59_940)], {
            configurationHash: 'sha256:another-configuration',
            hasEarlier: false,
            previousCursor: null,
          })
    ))
    const feed = new PletherDxyDatafeed({
      dataSource: dataSource({
        getCandlePage,
        clearCandlePageCache,
      }),
      useCandleApi: true,
    })

    try {
      await expect(new Promise<TradingViewBar[]>((resolve, reject) => {
        feed.getBars(
          {} as TradingViewSymbolInfo,
          '1',
          { from: 0, to: 65_000, countBack: 2, firstDataRequest: true },
          resolve,
          reject
        )
      })).rejects.toBe('The Perps candle identity changed while history was loading')
      expect(clearCandlePageCache).toHaveBeenCalledWith(60)
    } finally {
      feed.destroy()
    }
  })

  it('recovers when history is the first request to observe a new basket identity', async () => {
    const nowSpy = vi.spyOn(Date, 'now').mockReturnValue(64_950_000)
    const clearCandlePageCache = vi.fn()
    const onHistoryGap = vi.fn()
    const nextIdentity = {
      seriesId: 'dxy-v2',
      configurationHash: 'sha256:next-configuration',
      displayPriceCap: '250000000',
      volumeRouter: CANDLE_IDENTITY.volumeRouter,
    } as const
    let activeIdentity: typeof CANDLE_IDENTITY | typeof nextIdentity = CANDLE_IDENTITY
    let activeGeneration = 7
    const getCandlePage = vi.fn(async (_interval: number, cursor: number) => candlePage(
      cursor,
      [rawCandle(64_920)],
      {
        ...activeIdentity,
        datasetGeneration: activeGeneration,
        hasEarlier: false,
        previousCursor: null,
      }
    ))
    const getCurrentCandle = vi.fn(async () => currentCandle(60, {
      ...activeIdentity,
      datasetGeneration: activeGeneration,
    }))
    const feed = new PletherDxyDatafeed({
      dataSource: dataSource({
        getCandlePage,
        getCurrentCandle,
        clearCandlePageCache,
      }),
      useCandleApi: true,
      onHistoryGap,
    })
    const requestBars = () => new Promise<TradingViewBar[]>((resolve, reject) => {
      feed.getBars(
        {} as TradingViewSymbolInfo,
        '1',
        { from: 0, to: 65_000, countBack: 1, firstDataRequest: true },
        resolve,
        reject
      )
    })

    try {
      await expect(requestBars()).resolves.toHaveLength(1)
      activeIdentity = nextIdentity
      activeGeneration = 1

      await expect(requestBars()).resolves.toHaveLength(1)
      await expect(requestBars()).resolves.toHaveLength(1)
      await vi.waitFor(() => expect(onHistoryGap).toHaveBeenCalledWith(60))

      // The transition request revalidates once; subsequent history requests
      // accept the stored v2 identity instead of entering a rejection loop.
      expect(getCandlePage).toHaveBeenCalledTimes(4)
      expect(clearCandlePageCache).toHaveBeenCalledWith(60)
    } finally {
      feed.destroy()
      nowSpy.mockRestore()
    }
  })

  it('never adopts an older identity from a closed historical page', async () => {
    const nowSpy = vi.spyOn(Date, 'now').mockReturnValue(64_950_000)
    const clearCandlePageCache = vi.fn()
    const onHistoryGap = vi.fn()
    const nextIdentity = {
      seriesId: 'dxy-v2',
      configurationHash: 'sha256:next-configuration',
      displayPriceCap: '250000000',
      volumeRouter: CANDLE_IDENTITY.volumeRouter,
    } as const
    let pageGeneration = 1
    let pageIdentity: typeof CANDLE_IDENTITY | typeof nextIdentity = nextIdentity
    const getCandlePage = vi.fn(async (_interval: number, cursor: number) => candlePage(
      cursor,
      [rawCandle(64_920)],
      {
        ...pageIdentity,
        datasetGeneration: pageGeneration,
        hasEarlier: false,
        previousCursor: null,
      }
    ))
    const feed = new PletherDxyDatafeed({
      dataSource: dataSource({
        getCandlePage,
        getCurrentCandle: async () => currentCandle(60, {
          ...nextIdentity,
          datasetGeneration: 1,
        }),
        clearCandlePageCache,
      }),
      useCandleApi: true,
      onHistoryGap,
    })
    const requestBars = () => new Promise<TradingViewBar[]>((resolve, reject) => {
      feed.getBars(
        {} as TradingViewSymbolInfo,
        '1',
        { from: 0, to: 65_000, countBack: 1, firstDataRequest: true },
        resolve,
        reject
      )
    })

    try {
      await expect(requestBars()).resolves.toHaveLength(1)
      pageIdentity = CANDLE_IDENTITY
      pageGeneration = 7
      nowSpy.mockReturnValue(120_000_000)

      await expect(requestBars()).rejects.toBe(
        'The Perps candle identity changed while history was loading'
      )
      expect(onHistoryGap).not.toHaveBeenCalled()

      pageIdentity = nextIdentity
      pageGeneration = 1
      nowSpy.mockReturnValue(64_950_000)
      await expect(requestBars()).resolves.toHaveLength(1)
      expect(onHistoryGap).not.toHaveBeenCalled()
      expect(clearCandlePageCache).toHaveBeenCalledWith(60)
    } finally {
      feed.destroy()
      nowSpy.mockRestore()
    }
  })

  it('rejects invalid display-price caps before accepting historical bars', async () => {
    const feed = new PletherDxyDatafeed({
      dataSource: dataSource({
        getCurrentCandle: async () => currentCandle(60, {
          displayPriceCap: '9007199254740992',
        }),
        getCandlePage: async (_interval, cursor) => candlePage(
          cursor,
          [rawCandle(64_920)],
          { displayPriceCap: '9007199254740992', hasEarlier: false, previousCursor: null }
        ),
      }),
      useCandleApi: true,
    })

    try {
      await expect(new Promise<TradingViewBar[]>((resolve, reject) => {
        feed.getBars(
          {} as TradingViewSymbolInfo,
          '1',
          { from: 0, to: 65_000, countBack: 1, firstDataRequest: true },
          resolve,
          reject
        )
      })).rejects.toBe('The Perps candle API returned an invalid display-price cap')
    } finally {
      feed.destroy()
    }
  })

  it('rejects a zero dataset generation before accepting historical bars', async () => {
    const feed = new PletherDxyDatafeed({
      dataSource: dataSource({
        getCurrentCandle: async () => currentCandle(60, { datasetGeneration: 0 }),
        getCandlePage: async (_interval, cursor) => candlePage(
          cursor,
          [rawCandle(64_920)],
          { datasetGeneration: 0, hasEarlier: false, previousCursor: null }
        ),
      }),
      useCandleApi: true,
    })

    try {
      await expect(new Promise<TradingViewBar[]>((resolve, reject) => {
        feed.getBars(
          {} as TradingViewSymbolInfo,
          '1',
          { from: 0, to: 65_000, countBack: 1, firstDataRequest: true },
          resolve,
          reject
        )
      })).rejects.toBe('The Perps candle API returned an invalid dataset generation')
    } finally {
      feed.destroy()
    }
  })

  it('tracks dataset generations independently for each candle interval', async () => {
    const getCandlePage = vi.fn(async (interval: number, cursor: number) => candlePage(
      cursor,
      [rawCandle(interval === 60 ? 64_920 : 64_800)],
      {
        intervalSeconds: interval as 60 | 300,
        datasetGeneration: interval === 60 ? 12 : 3,
        hasEarlier: false,
        previousCursor: null,
      }
    ))
    const feed = new PletherDxyDatafeed({
      dataSource: dataSource({
        getCandlePage,
        getCurrentCandle: async (intervalSeconds) => currentCandle(intervalSeconds, {
          datasetGeneration: intervalSeconds === 60 ? 12 : 3,
        }),
      }),
      useCandleApi: true,
    })

    const requestBars = (resolution: '1' | '5') => new Promise<TradingViewBar[]>(
      (resolve, reject) => {
        feed.getBars(
          {} as TradingViewSymbolInfo,
          resolution,
          { from: 0, to: 65_000, countBack: 1, firstDataRequest: true },
          resolve,
          reject
        )
      }
    )

    try {
      await expect(requestBars('1')).resolves.toHaveLength(1)
      await expect(requestBars('5')).resolves.toHaveLength(1)
    } finally {
      feed.destroy()
    }
  })

  it('polls only the full current-candle endpoint for v2 subscribers', async () => {
    const getHistory = vi.fn(async () => history)
    const getLatest = vi.fn(async () => latest)
    const getCandlePage = vi.fn()
    const getCurrentCandle = vi.fn(async () => ({
      intervalSeconds: 300 as const,
      ...CANDLE_IDENTITY,
      ...CURRENT_VOLUME_COVERAGE,
      datasetGeneration: 7,
      coverageStart: 30_000,
      coverageEnd: 64_800,
      coverageComplete: true,
      finalizedThrough: 64_800,
      candle: rawCandle(64_800, { volumeUsdc: '10750000' }),
    }))
    const feed = new PletherDxyDatafeed({
      dataSource: { getHistory, getLatest, getCandlePage, getCurrentCandle },
      useCandleApi: true,
      pollIntervalMs: 60_000,
    })

    try {
      const bar = await new Promise<TradingViewBar>((resolve) => {
        feed.subscribeBars(
          {} as TradingViewSymbolInfo,
          '5',
          resolve,
          'v2-current-listener',
          () => undefined
        )
      })

      expect(bar).toMatchObject({
        time: 64_800_000,
        open: 1.02,
        high: 1.03,
        low: 0.99,
        close: 1.01,
        volume: 10.75,
      })
      expect(getCurrentCandle).toHaveBeenCalledWith(300, expect.any(AbortSignal))
      expect(getHistory).not.toHaveBeenCalled()
      expect(getLatest).not.toHaveBeenCalled()
      expect(getCandlePage).not.toHaveBeenCalled()
    } finally {
      feed.destroy()
    }
  })

  it('resets one interval when current data transitions to a new volume router', async () => {
    const onResetCacheNeeded = vi.fn()
    const onHistoryGap = vi.fn()
    const clearCandlePageCache = vi.fn()
    const nextIdentity = {
      ...CANDLE_IDENTITY,
      volumeRouter: '0x2222222222222222222222222222222222222222',
    } as const
    let currentIdentity: typeof CANDLE_IDENTITY | typeof nextIdentity = CANDLE_IDENTITY
    const feed = new PletherDxyDatafeed({
      dataSource: {
        getHistory: async () => history,
        getLatest: async () => latest,
        getCandlePage: async (_interval, cursor) => candlePage(
          cursor,
          [rawCandle(64_920)],
          { hasEarlier: false, previousCursor: null }
        ),
        getCurrentCandle: async () => ({
          intervalSeconds: 60,
          ...currentIdentity,
          ...CURRENT_VOLUME_COVERAGE,
          // A new identity has an independent generation sequence.
          datasetGeneration: currentIdentity === CANDLE_IDENTITY ? 7 : 1,
          coverageStart: 30_000,
          coverageEnd: 64_920,
          coverageComplete: true,
          finalizedThrough: 64_860,
          candle: rawCandle(64_920),
        }),
        clearCandlePageCache,
      },
      useCandleApi: true,
      pollIntervalMs: 60_000,
      onHistoryGap,
    })

    try {
      await new Promise<TradingViewBar[]>((resolve, reject) => {
        feed.getBars(
          {} as TradingViewSymbolInfo,
          '1',
          { from: 0, to: 65_000, countBack: 1, firstDataRequest: false },
          resolve,
          reject
        )
      })
      currentIdentity = nextIdentity
      const liveBar = await new Promise<TradingViewBar>((resolve) => {
        feed.subscribeBars(
          {} as TradingViewSymbolInfo,
          '1',
          resolve,
          'identity-transition-listener',
          onResetCacheNeeded
        )
      })
      await vi.waitFor(() => {
        expect(onResetCacheNeeded).toHaveBeenCalledOnce()
        expect(onHistoryGap).toHaveBeenCalledWith(60)
      })

      expect(liveBar).toMatchObject({
        time: 64_920_000,
        open: 1.02,
        high: 1.03,
        low: 0.99,
        close: 1.01,
      })
      expect(clearCandlePageCache).toHaveBeenCalledWith(60)
    } finally {
      feed.destroy()
    }
  })

  it('rejects a mismatched current interval before emitting a realtime bar', async () => {
    let requestIndex = 0
    const getCurrentCandle = vi.fn(async () => ({
      intervalSeconds: requestIndex++ === 0 ? 300 as const : 60 as const,
      ...CANDLE_IDENTITY,
      ...CURRENT_VOLUME_COVERAGE,
      datasetGeneration: 7,
      coverageStart: 30_000,
      coverageEnd: 64_800,
      coverageComplete: true,
      finalizedThrough: 64_800,
      candle: rawCandle(64_800),
    }))
    const ticks: TradingViewBar[] = []
    const feed = new PletherDxyDatafeed({
      dataSource: {
        getHistory: async () => history,
        getLatest: async () => latest,
        getCurrentCandle,
      },
      useCandleApi: true,
      pollIntervalMs: 1,
    })

    try {
      await new Promise<void>((resolve) => {
        feed.subscribeBars(
          {} as TradingViewSymbolInfo,
          '1',
          (bar) => {
            ticks.push(bar)
            feed.unsubscribeBars('mismatched-current-listener')
            resolve()
          },
          'mismatched-current-listener',
          () => undefined
        )
      })

      expect(getCurrentCandle.mock.calls.length).toBeGreaterThanOrEqual(2)
      expect(ticks).toHaveLength(1)
    } finally {
      feed.destroy()
    }
  })

  it('rejects a zero current dataset generation before emitting a realtime bar', async () => {
    let requestIndex = 0
    const getCurrentCandle = vi.fn(async () => ({
      intervalSeconds: 60 as const,
      ...CANDLE_IDENTITY,
      ...CURRENT_VOLUME_COVERAGE,
      datasetGeneration: requestIndex++ === 0 ? 0 : 7,
      coverageStart: 30_000,
      coverageEnd: 64_800,
      coverageComplete: true,
      finalizedThrough: 64_800,
      candle: rawCandle(64_800),
    }))
    const ticks: TradingViewBar[] = []
    const feed = new PletherDxyDatafeed({
      dataSource: dataSource({ getCurrentCandle }),
      useCandleApi: true,
      pollIntervalMs: 1,
    })

    try {
      await new Promise<void>((resolve) => {
        feed.subscribeBars(
          {} as TradingViewSymbolInfo,
          '1',
          (bar) => {
            ticks.push(bar)
            feed.unsubscribeBars('zero-generation-listener')
            resolve()
          },
          'zero-generation-listener',
          () => undefined
        )
      })

      expect(getCurrentCandle.mock.calls.length).toBeGreaterThanOrEqual(2)
      expect(ticks).toHaveLength(1)
    } finally {
      feed.destroy()
    }
  })

  it('accepts null current candles while retaining their interval generation metadata', async () => {
    let requestIndex = 0
    const onResetCacheNeeded = vi.fn()
    const onHistoryGap = vi.fn()
    const getCurrentCandle = vi.fn(async () => ({
      intervalSeconds: 60 as const,
      ...CANDLE_IDENTITY,
      ...CURRENT_VOLUME_COVERAGE,
      datasetGeneration: 7,
      coverageStart: 30_000,
      coverageEnd: 64_800,
      coverageComplete: true,
      finalizedThrough: 64_800,
      candle: requestIndex++ === 0 ? null : rawCandle(64_800),
    }))
    const feed = new PletherDxyDatafeed({
      dataSource: {
        getHistory: async () => history,
        getLatest: async () => latest,
        getCurrentCandle,
      },
      useCandleApi: true,
      pollIntervalMs: 1,
      onHistoryGap,
    })

    try {
      await new Promise<void>((resolve) => {
        feed.subscribeBars(
          {} as TradingViewSymbolInfo,
          '1',
          () => {
            feed.unsubscribeBars('null-current-listener')
            resolve()
          },
          'null-current-listener',
          onResetCacheNeeded
        )
      })

      expect(getCurrentCandle.mock.calls.length).toBeGreaterThanOrEqual(2)
      expect(onResetCacheNeeded).not.toHaveBeenCalled()
      expect(onHistoryGap).not.toHaveBeenCalled()
    } finally {
      feed.destroy()
    }
  })

  it('resets and refetches history after a realtime candle jump without synthesizing bars', async () => {
    const candles = [rawCandle(64_800), rawCandle(65_400)]
    let requestIndex = 0
    const onResetCacheNeeded = vi.fn()
    const onHistoryGap = vi.fn()
    const clearCandlePageCache = vi.fn()
    const feed = new PletherDxyDatafeed({
      dataSource: {
        getHistory: async () => history,
        getLatest: async () => latest,
        getCurrentCandle: async () => ({
          intervalSeconds: 300,
          ...CANDLE_IDENTITY,
          ...CURRENT_VOLUME_COVERAGE,
          datasetGeneration: 7,
          coverageStart: 30_000,
          coverageEnd: 65_100,
          coverageComplete: true,
          finalizedThrough: 65_100,
          candle: candles[Math.min(requestIndex++, candles.length - 1)],
        }),
        clearCandlePageCache,
      },
      useCandleApi: true,
      pollIntervalMs: 1,
      onHistoryGap,
    })
    const ticks: TradingViewBar[] = []

    try {
      await new Promise<void>((resolve) => {
        feed.subscribeBars(
          {} as TradingViewSymbolInfo,
          '5',
          (bar) => {
            ticks.push(bar)
            if (ticks.length === 2) {
              feed.unsubscribeBars('jump-listener')
              resolve()
            }
          },
          'jump-listener',
          onResetCacheNeeded
        )
      })
      await vi.waitFor(() => {
        expect(onResetCacheNeeded).toHaveBeenCalledOnce()
        expect(onHistoryGap).toHaveBeenCalledWith(300)
      })

      expect(ticks.map((bar) => bar.time)).toEqual([64_800_000, 65_400_000])
      expect(clearCandlePageCache).toHaveBeenCalledWith(300)
    } finally {
      feed.destroy()
    }
  })

  it('revalidates a locally stale historical generation once before accepting it', async () => {
    let seeded = false
    const getCandlePage = vi.fn(async (
      _interval: number,
      cursor: number,
      _signal?: AbortSignal,
      revalidate = false
    ) => candlePage(cursor, [rawCandle(64_920)], {
      datasetGeneration: !seeded || revalidate ? 8 : 7,
      hasEarlier: false,
      previousCursor: null,
    }))
    const clearCandlePageCache = vi.fn()
    const feed = new PletherDxyDatafeed({
      dataSource: dataSource({
        getCandlePage,
        getCurrentCandle: async () => currentCandle(60, { datasetGeneration: 8 }),
        clearCandlePageCache,
      }),
      useCandleApi: true,
    })
    const requestBars = () => new Promise<TradingViewBar[]>((resolve, reject) => {
      feed.getBars(
        {} as TradingViewSymbolInfo,
        '1',
        { from: 0, to: 65_000, countBack: 1, firstDataRequest: true },
        resolve,
        reject
      )
    })

    try {
      await requestBars()
      seeded = true
      await expect(requestBars()).resolves.toHaveLength(1)

      expect(getCandlePage.mock.calls.at(-2)?.[3]).toBeUndefined()
      expect(getCandlePage.mock.calls.at(-1)?.[3]).toBe(true)
      expect(clearCandlePageCache).toHaveBeenCalledWith(60)
    } finally {
      feed.destroy()
    }
  })

  it('aborts an in-flight v2 current-candle poll on unsubscribe', async () => {
    let requestSignal: AbortSignal | undefined
    const feed = new PletherDxyDatafeed({
      dataSource: {
        getHistory: async () => history,
        getLatest: async () => latest,
        getCurrentCandle: (_interval, signal) => new Promise((_resolve, reject) => {
          requestSignal = signal
          signal?.addEventListener('abort', () => reject(signal.reason), { once: true })
        }),
      },
      useCandleApi: true,
      pollIntervalMs: 60_000,
    })

    try {
      feed.subscribeBars(
        {} as TradingViewSymbolInfo,
        '1',
        () => undefined,
        'v2-abort-listener',
        () => undefined
      )
      await vi.waitFor(() => expect(requestSignal).toBeDefined())

      feed.unsubscribeBars('v2-abort-listener')

      expect(requestSignal?.aborted).toBe(true)
    } finally {
      feed.destroy()
    }
  })

  it('cancels a v2 listener locally without aborting a shared current-candle query', async () => {
    const queryClient = new QueryClient({ defaultOptions: { queries: { retry: false } } })
    const pendingResponse = deferredValue<Response>()
    let transportSignal: AbortSignal | undefined
    const fetchSpy = vi.spyOn(globalThis, 'fetch').mockImplementation((_input, init) => {
      transportSignal = init?.signal as AbortSignal | undefined
      return pendingResponse.promise
    })
    const response = {
      data: {
        intervalSeconds: 60,
        ...CANDLE_IDENTITY,
        datasetGeneration: 7,
        coverageStart: 30_000,
        coverageEnd: 64_800,
        coverageComplete: true,
        finalizedThrough: 64_800,
        candle: rawCandle(64_800),
      },
      meta: { blockNumber: 1, cached: false, chainId: 421_614 },
    }
    const feed = new PletherDxyDatafeed({
      queryClient,
      useCandleApi: true,
      pollIntervalMs: 60_000,
    })

    try {
      feed.subscribeBars(
        {} as TradingViewSymbolInfo,
        '1',
        () => undefined,
        'v2-shared-listener',
        () => undefined
      )
      await vi.waitFor(() => expect(transportSignal).toBeDefined())
      const sharedConsumer = queryClient.fetchQuery({
        queryKey: apiQueryKeys.perps.basketCurrentCandle(60),
        queryFn: async () => { throw new Error('Expected the shared in-flight query') },
        retry: false,
      })

      feed.unsubscribeBars('v2-shared-listener')
      expect(transportSignal?.aborted).toBe(false)
      pendingResponse.resolve(new Response(JSON.stringify(response), {
        headers: { 'Content-Type': 'application/json' },
      }))
      await expect(sharedConsumer).resolves.toEqual(response)
    } finally {
      pendingResponse.resolve(new Response(JSON.stringify(response), {
        headers: { 'Content-Type': 'application/json' },
      }))
      feed.destroy()
      queryClient.clear()
      fetchSpy.mockRestore()
    }
  })

  it('pauses v2 current-candle polling while hidden and aborts it when visibility changes', async () => {
    let visibilityState: DocumentVisibilityState = 'hidden'
    const visibilitySpy = vi.spyOn(document, 'visibilityState', 'get').mockImplementation(
      () => visibilityState
    )
    let requestSignal: AbortSignal | undefined
    const getCurrentCandle = vi.fn((_interval: number, signal?: AbortSignal) =>
      new Promise<never>((_resolve, reject) => {
        requestSignal = signal
        signal?.addEventListener('abort', () => reject(signal.reason), { once: true })
      })
    )
    const feed = new PletherDxyDatafeed({
      dataSource: {
        getHistory: async () => history,
        getLatest: async () => latest,
        getCurrentCandle,
      },
      useCandleApi: true,
      pollIntervalMs: 60_000,
    })

    try {
      feed.subscribeBars(
        {} as TradingViewSymbolInfo,
        '1',
        () => undefined,
        'v2-visibility-listener',
        () => undefined
      )
      await new Promise((resolve) => setTimeout(resolve, 0))
      expect(getCurrentCandle).not.toHaveBeenCalled()

      visibilityState = 'visible'
      document.dispatchEvent(new Event('visibilitychange'))
      await vi.waitFor(() => expect(requestSignal).toBeDefined())

      visibilityState = 'hidden'
      document.dispatchEvent(new Event('visibilitychange'))
      expect(requestSignal?.aborted).toBe(true)
    } finally {
      feed.destroy()
      visibilitySpy.mockRestore()
    }
  })

  it('maps the parent intervals and supports additional native minute resolutions', () => {
    expect(tradingViewResolutionForInterval('1m')).toBe('1')
    expect(tradingViewResolutionForInterval('5m')).toBe('5')
    expect(tradingViewResolutionForInterval('15m')).toBe('15')
    expect(tradingViewResolutionForInterval('1h')).toBe('60')
    expect(tradingViewResolutionForInterval('1d')).toBe('1D')
    expect(chartIntervalForTradingViewResolution('1')).toBe('1m')
    expect(chartIntervalForTradingViewResolution('5')).toBe('5m')
    expect(chartIntervalForTradingViewResolution('15')).toBe('15m')
    expect(chartIntervalForTradingViewResolution('60')).toBe('1h')
    expect(chartIntervalForTradingViewResolution('1D')).toBe('1d')
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
    expect(symbolInfo.session).toBe('0000-0000:123456')
    expect(symbolInfo.timezone).toBe('Etc/UTC')
    expect(symbolInfo.has_empty_bars).toBe(false)
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
