import { Result } from 'better-result'
import type { QueryClient } from '@tanstack/react-query'
import {
  apiQueryKeys,
  perpsApi,
  type ApiResponse,
  type PerpsBasketCandle,
  type PerpsBasketCandlePage,
  type PerpsBasketCurrentCandle,
  type PerpsCandleIntervalSeconds,
} from '../api'
import {
  PERPS_CANDLE_CURRENT_POLL_INTERVAL_MS,
  PERPS_CANDLE_PAGE_BUCKETS,
} from '../api/candlePolicy'
import {
  parsePerpsDisplayPriceCap,
  perpsBasketCandleToChartCandle,
} from '../utils/dxyBasketChart'
import type { DxyBasketChartInterval } from '../components/dxyBasketChartConfig'
import type {
  TradingViewBar,
  TradingViewDatafeed,
  TradingViewDatafeedConfiguration,
  TradingViewResolution,
  TradingViewSearchResult,
  TradingViewSymbolInfo,
} from './types'

export const PLDXY_TRADINGVIEW_SYMBOL = 'PLETHER:PLDXY.P'
export const PLDXY_DIRECTIONAL_VOLUME_SYMBOL = 'PLETHER:PLDXY.DIRECTIONAL_VOLUME'
export const TRADINGVIEW_RESOLUTIONS: TradingViewResolution[] = [
  '1',
  '3',
  '5',
  '15',
  '30',
  '60',
  '1D',
]
export const TRADINGVIEW_FAVORITE_RESOLUTIONS: TradingViewResolution[] = ['5', '60', '1D']
export const PERPS_CANDLE_MAX_HISTORY_PAGES = 24

const MICRO_USDC_PER_USDC = 1_000_000n
const INITIAL_CURRENT_CANDLE_WAIT_MS = 250

const SYMBOL_INFO: TradingViewSymbolInfo = {
  name: 'plDXY.P',
  ticker: PLDXY_TRADINGVIEW_SYMBOL,
  description: 'plDXY Perpetual',
  type: 'futures',
  // The FX basket can publish partial Sunday buckets after the weekly market
  // opens. Saturday remains closed, and missing bars are never synthesized.
  session: '0000-0000:123456',
  timezone: 'Etc/UTC',
  exchange: 'Plether',
  listed_exchange: 'Plether',
  format: 'price',
  pricescale: 10_000,
  minmov: 1,
  has_intraday: true,
  intraday_multipliers: ['1', '3', '5', '15', '30', '60'],
  has_daily: true,
  daily_multipliers: ['1'],
  supported_resolutions: TRADINGVIEW_RESOLUTIONS,
  data_status: 'streaming',
  visible_plots_set: 'ohlcv',
  volume_precision: 2,
  has_empty_bars: false,
}

const DIRECTIONAL_VOLUME_SYMBOL_INFO: TradingViewSymbolInfo = {
  ...SYMBOL_INFO,
  name: 'plDXY.DirectionalVolume',
  ticker: PLDXY_DIRECTIONAL_VOLUME_SYMBOL,
  description: 'plDXY Directional Volume',
  pricescale: 100,
}

const DATAFEED_CONFIGURATION: TradingViewDatafeedConfiguration = {
  supported_resolutions: TRADINGVIEW_RESOLUTIONS,
  supports_marks: false,
  supports_timescale_marks: false,
  supports_time: false,
  exchanges: [{ value: 'Plether', name: 'Plether', desc: 'Plether' }],
  symbols_types: [{ name: 'Perpetual', value: 'futures' }],
}

export interface PletherChartDataSource {
  getCandlePage?: (
    intervalSeconds: PerpsCandleIntervalSeconds,
    cursor: number,
    signal?: AbortSignal,
    revalidate?: boolean
  ) => Promise<PerpsBasketCandlePage>
  getCurrentCandle?: (
    intervalSeconds: PerpsCandleIntervalSeconds,
    signal?: AbortSignal,
    revalidate?: boolean
  ) => Promise<PerpsBasketCurrentCandle>
  clearCandlePageCache?: (intervalSeconds?: PerpsCandleIntervalSeconds) => void
}

interface Subscription {
  listenerGuid: string
  resolution: TradingViewResolution
  seriesKind: TradingViewSeriesKind
  onTick: (bar: TradingViewBar) => void
  onResetCacheNeeded: () => void
  timer?: ReturnType<typeof setInterval>
  polling: boolean
  failureCount: number
  nextPollAt: number
  requestControllers: Set<AbortController>
  currentBar?: TradingViewBar
}

type TradingViewSeriesKind = 'price' | 'directional-volume'

interface CandleDatasetIdentity {
  seriesId: string
  configurationHash: string
  displayPriceCap: string
  volumeChainId: number
  volumeRouter: string
}

interface CandleCoverageBoundary {
  coverageStart: number
  datasetGeneration: number
}

interface PrimedCurrentCandle {
  bar: TradingViewBar
  primedAt: number
}

interface SharedRequest<T> {
  controller: AbortController
  promise: Promise<T>
  consumerCount: number
  settled: boolean
}

export interface PletherDxyDatafeedOptions {
  dataSource?: PletherChartDataSource
  queryClient?: QueryClient
  pollIntervalMs?: number
  onHistoryGap?: (intervalSeconds?: PerpsCandleIntervalSeconds) => void
  onVolumeCoverageChange?: (update: PletherVolumeCoverageUpdate) => void
}

export type PletherVolumeCoverageState = 'unknown' | 'available' | 'unavailable'

export interface PletherVolumeCoverageUpdate {
  intervalSeconds: PerpsCandleIntervalSeconds
  state: PletherVolumeCoverageState
}

async function fetchBasketCandlePage(
  intervalSeconds: PerpsCandleIntervalSeconds,
  cursor: number,
  signal?: AbortSignal,
  revalidate = false
): Promise<ApiResponse<PerpsBasketCandlePage>> {
  const result = await perpsApi.getPerpsBasketCandles(
    intervalSeconds,
    cursor,
    signal,
    revalidate
  )
  if (Result.isError(result)) throw result.error
  return result.value
}

async function fetchBasketCurrentCandle(
  intervalSeconds: PerpsCandleIntervalSeconds,
  signal?: AbortSignal,
  revalidate = false
): Promise<ApiResponse<PerpsBasketCurrentCandle>> {
  const result = await perpsApi.getPerpsBasketCurrentCandle(
    intervalSeconds,
    signal,
    revalidate
  )
  if (Result.isError(result)) throw result.error
  return result.value
}

function createApiDataSource(queryClient: QueryClient | undefined): PletherChartDataSource {
  const candlePageRequests = new Map<string, SharedRequest<PerpsBasketCandlePage>>()
  const currentCandleRequests = new Map<string, SharedRequest<PerpsBasketCurrentCandle>>()

  return {
    async getCandlePage(intervalSeconds, cursor, signal, revalidate = false) {
      // The Worker already caches fixed pages against an authoritative origin
      // identity and generation. Reusing a page locally under only
      // (interval,cursor) could bypass that probe after a correction, reorg, or
      // chart remount, so settled responses are never cached here. Price and
      // directional-volume reads can still share the exact same active probe.
      return await consumeSharedRequest(
        candlePageRequests,
        `${String(intervalSeconds)}:${String(cursor)}:${revalidate ? 'revalidate' : 'default'}`,
        signal,
        async (sharedSignal) => (
          await fetchBasketCandlePage(
            intervalSeconds,
            cursor,
            sharedSignal,
            revalidate
          )
        ).data
      )
    },
    async getCurrentCandle(intervalSeconds, signal, revalidate = false) {
      return await consumeSharedRequest(
        currentCandleRequests,
        `${String(intervalSeconds)}:${revalidate ? 'revalidate' : 'default'}`,
        signal,
        async (sharedSignal) => {
          if (!queryClient || revalidate) {
            return (
              await fetchBasketCurrentCandle(intervalSeconds, sharedSignal, revalidate)
            ).data
          }

          const response = await awaitWithAbort(
            queryClient.fetchQuery({
              queryKey: apiQueryKeys.perps.basketCurrentCandle(intervalSeconds),
              queryFn: ({ signal: querySignal }) => fetchBasketCurrentCandle(
                intervalSeconds,
                querySignal
              ),
              // The edge cache supplies the bounded reuse window. Keeping this
              // query immediately stale ensures every visible polling tick reaches
              // that boundary instead of skipping alternate ticks locally.
              staleTime: 0,
              retry: retryTransientFailureOnce,
            }),
            sharedSignal
          )
          return response.data
        }
      )
    },
    clearCandlePageCache(intervalSeconds) {
      const candleKey = apiQueryKeys.perps.basketCandlesAll()
      queryClient?.removeQueries({
        queryKey: candleKey,
        predicate: intervalSeconds === undefined
          ? undefined
          : (query) => {
              const suffix = query.queryKey.slice(candleKey.length)
              return suffix[0] === intervalSeconds || (
                suffix[0] === 'current' && suffix[1] === intervalSeconds
              )
            },
      })
    },
  }
}

function consumeSharedRequest<T>(
  requests: Map<string, SharedRequest<T>>,
  key: string,
  signal: AbortSignal | undefined,
  load: (signal: AbortSignal) => Promise<T>
): Promise<T> {
  if (signal?.aborted) return Promise.reject(abortReason(signal))

  let request = requests.get(key)
  if (!request) {
    const controller = new AbortController()
    const sharedRequest: SharedRequest<T> = {
      controller,
      promise: Promise.resolve().then(() => load(controller.signal)),
      consumerCount: 0,
      settled: false,
    }
    request = sharedRequest
    requests.set(key, sharedRequest)
    void sharedRequest.promise.then(
      () => {
        sharedRequest.settled = true
        if (requests.get(key) === sharedRequest) requests.delete(key)
      },
      () => {
        sharedRequest.settled = true
        if (requests.get(key) === sharedRequest) requests.delete(key)
      }
    )
  }

  request.consumerCount += 1
  const activeRequest = request
  return new Promise<T>((resolve, reject) => {
    let released = false
    const release = (): boolean => {
      if (released) return false
      released = true
      signal?.removeEventListener('abort', onAbort)
      activeRequest.consumerCount -= 1
      if (activeRequest.consumerCount === 0 && !activeRequest.settled) {
        if (requests.get(key) === activeRequest) requests.delete(key)
        activeRequest.controller.abort()
      }
      return true
    }
    const onAbort = () => {
      if (!release()) return
      reject(signal ? abortReason(signal) : new DOMException('The operation was aborted', 'AbortError'))
    }
    signal?.addEventListener('abort', onAbort, { once: true })
    void activeRequest.promise.then(
      (value) => {
        if (release()) resolve(value)
      },
      (error: unknown) => {
        if (release()) {
          reject(error instanceof Error ? error : new Error('Chart data request failed'))
        }
      }
    )
  })
}

function awaitWithAbort<T>(promise: Promise<T>, signal: AbortSignal | undefined): Promise<T> {
  if (!signal) return promise
  if (signal.aborted) return Promise.reject(abortReason(signal))

  return new Promise<T>((resolve, reject) => {
    const onAbort = () => {
      signal.removeEventListener('abort', onAbort)
      reject(abortReason(signal))
    }
    signal.addEventListener('abort', onAbort, { once: true })
    void promise.then(
      (value) => {
        signal.removeEventListener('abort', onAbort)
        resolve(value)
      },
      (error: unknown) => {
        signal.removeEventListener('abort', onAbort)
        reject(error instanceof Error ? error : new Error('Chart data request failed'))
      }
    )
  })
}

function settleWithin<T>(promise: Promise<T | undefined>, waitMs: number): Promise<T | undefined> {
  return new Promise((resolve) => {
    let settled = false
    const finish = (value: T | undefined) => {
      if (settled) return
      settled = true
      clearTimeout(timer)
      resolve(value)
    }
    const timer = setTimeout(() => {
      finish(undefined)
    }, waitMs)
    void promise.then(finish, () => {
      finish(undefined)
    })
  })
}

function abortReason(signal: AbortSignal): Error {
  return signal.reason instanceof Error
    ? signal.reason
    : new DOMException('The operation was aborted', 'AbortError')
}

function retryTransientFailureOnce(failureCount: number, error: unknown): boolean {
  if (isAbortError(error)) return false
  const status = (error as { status?: number }).status
  if (status !== undefined && status >= 400 && status < 500) return false
  return failureCount < 1
}

function isAbortError(error: unknown): boolean {
  return (error as { name?: string }).name === 'AbortError'
}

export function tradingViewResolutionForInterval(
  interval: DxyBasketChartInterval
): TradingViewResolution {
  const resolutions: Record<typeof interval, TradingViewResolution> = {
    '1m': '1',
    '5m': '5',
    '15m': '15',
    '1h': '60',
    '1d': '1D',
  }
  return resolutions[interval]
}

export function chartIntervalForTradingViewResolution(
  resolution: string
): DxyBasketChartInterval | undefined {
  const intervals: Partial<Record<TradingViewResolution, DxyBasketChartInterval>> = {
    '1': '1m',
    '5': '5m',
    '15': '15m',
    '60': '1h',
    '1D': '1d',
  }
  return intervals[resolution as TradingViewResolution]
}

export function secondsForTradingViewResolution(resolution: TradingViewResolution): number {
  const seconds: Record<TradingViewResolution, number> = {
    '1': 60,
    '3': 3 * 60,
    '5': 5 * 60,
    '15': 15 * 60,
    '30': 30 * 60,
    '60': 60 * 60,
    '1D': 24 * 60 * 60,
  }
  return seconds[resolution]
}

function candleIntervalForTradingViewResolution(
  resolution: TradingViewResolution
): PerpsCandleIntervalSeconds {
  return secondsForTradingViewResolution(resolution) as PerpsCandleIntervalSeconds
}

export function candlePageCursorForRequest(
  to: number,
  intervalSeconds: PerpsCandleIntervalSeconds
): number {
  const pageSeconds = intervalSeconds * PERPS_CANDLE_PAGE_BUCKETS
  if (!Number.isFinite(to) || to <= 0) return pageSeconds
  return Math.max(pageSeconds, Math.ceil(to / pageSeconds) * pageSeconds)
}

export function perpsBasketCandlesToTradingViewBars(
  candles: PerpsBasketCandle[],
  intervalSeconds: PerpsCandleIntervalSeconds,
  displayPriceCap: string
): TradingViewBar[] {
  const barsByTime = new Map<number, TradingViewBar>()

  for (const candle of candles) {
    if (candle.timestamp % intervalSeconds !== 0) continue
    const chartCandle = perpsBasketCandleToChartCandle(candle, displayPriceCap)
    if (!chartCandle) continue

    const volumeUsdc = parseOptionalMicroUsdc(candle.volumeUsdc)
    const bar: TradingViewBar = {
      time: chartCandle.timestamp * 1000,
      open: chartCandle.open,
      high: chartCandle.high,
      low: chartCandle.low,
      close: chartCandle.close,
      ...(volumeUsdc === undefined
        ? {}
        : { volume: microUsdcToHumanUsdc(volumeUsdc) }),
    }
    barsByTime.set(bar.time, bar)
  }

  return [...barsByTime.values()].sort((left, right) => left.time - right.time)
}

export function perpsBasketCandlesToDirectionalVolumeBars(
  candles: PerpsBasketCandle[],
  intervalSeconds: PerpsCandleIntervalSeconds
): TradingViewBar[] {
  const barsByTime = new Map<number, TradingViewBar>()

  for (const candle of candles) {
    if (candle.timestamp % intervalSeconds !== 0) continue
    const longFlow = parseOptionalMicroUsdc(candle.longFlowVolumeUsdc)
    const shortFlow = parseOptionalMicroUsdc(candle.shortFlowVolumeUsdc)
    if (longFlow === undefined || shortFlow === undefined) continue

    const longFlowUsdc = microUsdcToHumanUsdc(longFlow)
    const shortFlowUsdc = microUsdcToHumanUsdc(shortFlow)
    const bar: TradingViewBar = {
      time: candle.timestamp * 1000,
      open: longFlowUsdc,
      high: longFlowUsdc,
      low: longFlowUsdc,
      close: longFlowUsdc,
      volume: shortFlowUsdc,
    }
    barsByTime.set(bar.time, bar)
  }

  return [...barsByTime.values()].sort((left, right) => left.time - right.time)
}

function parseOptionalMicroUsdc(value: string | null | undefined): bigint | undefined {
  const normalized = value?.trim()
  if (!normalized || !/^\d+$/.test(normalized)) return undefined

  try {
    return BigInt(normalized)
  } catch {
    return undefined
  }
}

function microUsdcToHumanUsdc(value: bigint): number {
  const whole = Number(value / MICRO_USDC_PER_USDC)
  const fraction = Number(value % MICRO_USDC_PER_USDC) / Number(MICRO_USDC_PER_USDC)
  const volume = whole + fraction
  return Number.isFinite(volume) ? volume : 0
}

function errorMessage(error: unknown): string {
  return error instanceof Error ? error.message : 'Unable to load plDXY chart data'
}

function matchesSymbol(symbolName: string): boolean {
  const normalized = symbolName.trim().toUpperCase()
  return normalized === PLDXY_TRADINGVIEW_SYMBOL || normalized === 'PLDXY.P' || normalized === 'PLDXY'
}

function matchesDirectionalVolumeSymbol(symbolName: string | undefined): boolean {
  const normalized = symbolName?.trim().toUpperCase()
  return normalized === PLDXY_DIRECTIONAL_VOLUME_SYMBOL ||
    normalized === 'PLDXY.DIRECTIONALVOLUME'
}

function seriesKindForSymbol(symbolInfo: TradingViewSymbolInfo): TradingViewSeriesKind {
  return matchesDirectionalVolumeSymbol(symbolInfo.ticker) ||
    matchesDirectionalVolumeSymbol(symbolInfo.name)
    ? 'directional-volume'
    : 'price'
}

function searchResult(): TradingViewSearchResult {
  return {
    symbol: SYMBOL_INFO.name,
    full_name: PLDXY_TRADINGVIEW_SYMBOL,
    description: SYMBOL_INFO.description,
    exchange: SYMBOL_INFO.exchange,
    ticker: SYMBOL_INFO.ticker,
    type: SYMBOL_INFO.type,
  }
}

export class PletherDxyDatafeed implements TradingViewDatafeed {
  private readonly dataSource: PletherChartDataSource
  private readonly pollIntervalMs: number
  private readonly onHistoryGap:
    ((intervalSeconds?: PerpsCandleIntervalSeconds) => void) | undefined
  private readonly onVolumeCoverageChange:
    ((update: PletherVolumeCoverageUpdate) => void) | undefined
  private readonly subscriptions = new Map<string, Subscription>()
  private readonly lastBars = new Map<TradingViewResolution, TradingViewBar>()
  private readonly requestControllers = new Set<AbortController>()
  private readonly handleVisibilityChange = () => {
    if (document.visibilityState === 'hidden') {
      for (const subscription of this.subscriptions.values()) {
        this.stopSubscriptionTimer(subscription)
      }
      for (const controller of this.requestControllers) controller.abort()
      return
    }

    for (const [listenerGuid, subscription] of this.subscriptions) {
      this.startSubscriptionTimer(listenerGuid, subscription)
      void this.pollSubscription(listenerGuid)
    }
  }
  private readonly datasetGenerations = new Map<PerpsCandleIntervalSeconds, number>()
  private readonly candleDatasetIdentities = new Map<
    PerpsCandleIntervalSeconds,
    CandleDatasetIdentity
  >()
  private readonly candleCoverageBoundaries = new Map<
    PerpsCandleIntervalSeconds,
    CandleCoverageBoundary
  >()
  private readonly initializedCandleIntervals = new Set<PerpsCandleIntervalSeconds>()
  private readonly primedCurrentCandles = new Map<TradingViewResolution, PrimedCurrentCandle>()
  private readonly candleIntervalsNeedingRevalidation = new Set<PerpsCandleIntervalSeconds>()
  private readonly pendingCandleHistoryResets = new Set<PerpsCandleIntervalSeconds>()
  private readonly publishedVolumeCoverageStates = new Map<
    PerpsCandleIntervalSeconds,
    PletherVolumeCoverageState
  >()
  private destroyed = false

  constructor(options: PletherDxyDatafeedOptions = {}) {
    this.dataSource = options.dataSource ?? createApiDataSource(options.queryClient)
    this.pollIntervalMs = options.pollIntervalMs ?? PERPS_CANDLE_CURRENT_POLL_INTERVAL_MS
    this.onHistoryGap = options.onHistoryGap
    this.onVolumeCoverageChange = options.onVolumeCoverageChange
    if (typeof document !== 'undefined') {
      document.addEventListener('visibilitychange', this.handleVisibilityChange)
    }
  }

  onReady(callback: (configuration: TradingViewDatafeedConfiguration) => void): void {
    setTimeout(() => {
      callback(DATAFEED_CONFIGURATION)
    }, 0)
  }

  searchSymbols(
    userInput: string,
    _exchange: string,
    _symbolType: string,
    onResult: (results: TradingViewSearchResult[]) => void
  ): void {
    const query = userInput.trim().toLowerCase()
    const isMatch = !query || 'pldxy perpetual plether'.includes(query)
    setTimeout(() => {
      onResult(isMatch ? [searchResult()] : [])
    }, 0)
  }

  resolveSymbol(
    symbolName: string,
    onResolve: (symbolInfo: TradingViewSymbolInfo) => void,
    onError: (message: string) => void
  ): void {
    setTimeout(() => {
      if (matchesSymbol(symbolName)) {
        onResolve(SYMBOL_INFO)
      } else if (matchesDirectionalVolumeSymbol(symbolName)) {
        onResolve(DIRECTIONAL_VOLUME_SYMBOL_INFO)
      } else {
        onError(`Unknown symbol: ${symbolName}`)
      }
    }, 0)
  }

  getBars(
    symbolInfo: TradingViewSymbolInfo,
    resolution: TradingViewResolution,
    periodParams: { from: number; to: number; countBack: number; firstDataRequest: boolean },
    onResult: (bars: TradingViewBar[], metadata: { noData: boolean }) => void,
    onError: (message: string) => void
  ): void {
    if (!this.isDocumentVisible()) {
      setTimeout(() => {
        if (!this.destroyed) onError('Chart data loading is paused while this tab is hidden')
      }, 0)
      return
    }

    const seriesKind = seriesKindForSymbol(symbolInfo)
    void this.runRequest((signal) => this.loadCandleBars(resolution, periodParams, seriesKind, signal))
      .then((bars) => {
        setTimeout(() => {
          if (!this.destroyed) {
            onResult(bars.map((bar) => ({ ...bar })), { noData: bars.length === 0 })
          }
        }, 0)
      })
      .catch((error: unknown) => {
        setTimeout(() => {
          if (!this.destroyed) onError(errorMessage(error))
        }, 0)
      })
  }

  subscribeBars(
    symbolInfo: TradingViewSymbolInfo,
    resolution: TradingViewResolution,
    onTick: (bar: TradingViewBar) => void,
    listenerGuid: string,
    onResetCacheNeededCallback: () => void
  ): void {
    this.unsubscribeBars(listenerGuid)

    const seriesKind = seriesKindForSymbol(symbolInfo)
    const primedCurrent = seriesKind === 'price'
      ? this.consumePrimedCurrentCandle(resolution)
      : undefined

    const subscription: Subscription = {
      listenerGuid,
      resolution,
      seriesKind,
      onTick,
      onResetCacheNeeded: onResetCacheNeededCallback,
      polling: false,
      failureCount: 0,
      nextPollAt: 0,
      requestControllers: new Set(),
      currentBar: primedCurrent?.bar ?? (
        seriesKind === 'price' ? this.lastBars.get(resolution) : undefined
      ),
    }
    this.subscriptions.set(listenerGuid, subscription)
    if (this.isDocumentVisible()) {
      this.startSubscriptionTimer(listenerGuid, subscription)
      if (!primedCurrent) void this.pollSubscription(listenerGuid)
    }
  }

  unsubscribeBars(listenerGuid: string): void {
    const subscription = this.subscriptions.get(listenerGuid)
    if (!subscription) return

    this.stopSubscriptionTimer(subscription)
    for (const controller of subscription.requestControllers) controller.abort()
    subscription.requestControllers.clear()
    this.subscriptions.delete(listenerGuid)
  }

  destroy(): void {
    this.destroyed = true
    if (typeof document !== 'undefined') {
      document.removeEventListener('visibilitychange', this.handleVisibilityChange)
    }
    for (const listenerGuid of [...this.subscriptions.keys()]) {
      this.unsubscribeBars(listenerGuid)
    }
    for (const controller of this.requestControllers) controller.abort()
    this.requestControllers.clear()
  }

  private async loadCandleBars(
    resolution: TradingViewResolution,
    periodParams: {
      from: number
      to: number
      countBack: number
      firstDataRequest: boolean
    },
    seriesKind: TradingViewSeriesKind,
    signal?: AbortSignal
  ): Promise<TradingViewBar[]> {
    const getCandlePage = this.dataSource.getCandlePage
    if (!getCandlePage) {
      throw new Error('The Perps candle API data source is unavailable')
    }

    const intervalSeconds = candleIntervalForTradingViewResolution(resolution)
    const knownCoverageBoundary = this.candleCoverageBoundaries.get(intervalSeconds)
    if (
      knownCoverageBoundary &&
      knownCoverageBoundary.datasetGeneration === this.datasetGenerations.get(intervalSeconds) &&
      Number.isFinite(periodParams.to) &&
      periodParams.to <= knownCoverageBoundary.coverageStart
    ) {
      // TradingView asks again when the first response contains fewer bars than
      // countBack. Once the API has proved the series inception, an older
      // follow-up is a normal end-of-history condition rather than an API
      // error. Avoid requesting a fixed page that cannot intersect coverage.
      return []
    }
    const targetCount = Math.max(1, periodParams.countBack)
    const requestedToMs = periodParams.to * 1000
    const shouldPrimeCurrent = seriesKind === 'price' &&
      periodParams.firstDataRequest &&
      !this.initializedCandleIntervals.has(intervalSeconds) &&
      this.dataSource.getCurrentCandle !== undefined
    const currentCandleRequest = shouldPrimeCurrent
      ? this.dataSource.getCurrentCandle?.(
          intervalSeconds,
          signal,
          this.candleIntervalsNeedingRevalidation.has(intervalSeconds)
        ).catch(() => undefined)
      : undefined
    const barsByTime = new Map<number, TradingViewBar>()
    const visitedCursors = new Set<number>()
    const requestedCursor = candlePageCursorForRequest(periodParams.to, intervalSeconds)
    const localCurrentCursor = candlePageCursorForRequest(Date.now() / 1000, intervalSeconds)
    let cursor = Math.min(requestedCursor, localCurrentCursor)
    let pageRequestCount = 0
    let requestGeneration: number | undefined
    let requestIdentity: CandleDatasetIdentity | undefined
    let requestCoverageStart: number | undefined
    let requestVolumeCoverageState: PletherVolumeCoverageState | undefined
    let forceRevalidate = this.candleIntervalsNeedingRevalidation.has(intervalSeconds)

    while (
      barsByTime.size < targetCount &&
      pageRequestCount < PERPS_CANDLE_MAX_HISTORY_PAGES &&
      !visitedCursors.has(cursor)
    ) {
      visitedCursors.add(cursor)
      const loadPage = (revalidate: boolean) => {
        if (pageRequestCount >= PERPS_CANDLE_MAX_HISTORY_PAGES) {
          throw new Error('The Perps candle history request budget was exhausted')
        }
        pageRequestCount += 1
        return revalidate
          ? getCandlePage(intervalSeconds, cursor, signal, true)
          : getCandlePage(intervalSeconds, cursor, signal)
      }
      let page = await loadPage(forceRevalidate)
      this.validateCandlePage(page, intervalSeconds, cursor)
      let pageVolumeCoverageState = this.volumeCoverageState(page, intervalSeconds)
      let pageIdentity = this.candleIdentity(page)

      if (requestIdentity === undefined) {
        const knownIdentity = this.candleDatasetIdentities.get(intervalSeconds)
        if (knownIdentity && !this.candleIdentitiesEqual(knownIdentity, pageIdentity)) {
          // Only the fixed page containing the local wall clock may reveal a
          // newly activated basket definition authoritatively. A closed page
          // can legitimately belong to an older definition and must never
          // replace the live interval identity.
          this.prepareCandleRevalidation(intervalSeconds)
          const isCurrentContainingPage =
            cursor === candlePageCursorForRequest(Date.now() / 1000, intervalSeconds)
          if (!isCurrentContainingPage) {
            throw new Error('The Perps candle identity changed while history was loading')
          }
          if (!forceRevalidate) {
            const firstIdentity = pageIdentity
            page = await loadPage(true)
            forceRevalidate = true
            this.validateCandlePage(page, intervalSeconds, cursor)
            pageVolumeCoverageState = this.volumeCoverageState(page, intervalSeconds)
            pageIdentity = this.candleIdentity(page)
            if (!this.candleIdentitiesEqual(firstIdentity, pageIdentity)) {
              throw new Error('The Perps candle identity changed while history was loading')
            }
          }
          this.observeCandleIdentity(intervalSeconds, pageIdentity)
        }
        requestIdentity = pageIdentity
      } else if (!this.candleIdentitiesEqual(requestIdentity, pageIdentity)) {
        this.prepareCandleRevalidation(intervalSeconds)
        throw new Error('The Perps candle identity changed while history was loading')
      }

      const knownGeneration = this.datasetGenerations.get(intervalSeconds)
      if (knownGeneration !== undefined && page.datasetGeneration < knownGeneration) {
        this.prepareCandleRevalidation(intervalSeconds)
        if (forceRevalidate) {
          this.requestCandleHistoryReset(intervalSeconds)
          throw new Error('The Perps candle API returned a stale dataset generation')
        }

        forceRevalidate = true
        page = await loadPage(true)
        this.validateCandlePage(page, intervalSeconds, cursor)
        pageVolumeCoverageState = this.volumeCoverageState(page, intervalSeconds)
        const revalidatedIdentity = this.candleIdentity(page)
        if (!this.candleIdentitiesEqual(pageIdentity, revalidatedIdentity)) {
          // A rejected mixed page is never authoritative for interval state.
          // Keep the previously established live identity and merely force a
          // clean refetch on the next request.
          this.prepareCandleRevalidation(intervalSeconds)
          throw new Error('The Perps candle identity changed while history was loading')
        }
        if (page.datasetGeneration < knownGeneration) {
          this.requestCandleHistoryReset(intervalSeconds)
          throw new Error('The Perps candle API returned a stale dataset generation')
        }
      }

      requestCoverageStart ??= this.candleCoverageStart(page)
      if (requestVolumeCoverageState === undefined) {
        requestVolumeCoverageState = pageVolumeCoverageState
      } else if (requestVolumeCoverageState !== pageVolumeCoverageState) {
        this.prepareCandleRevalidation(intervalSeconds)
        throw new Error('The Perps candle volume coverage changed while history was loading')
      }
      if (requestGeneration === undefined) {
        requestGeneration = page.datasetGeneration
      } else if (requestGeneration !== page.datasetGeneration) {
        this.observeDatasetGeneration(
          intervalSeconds,
          Math.max(requestGeneration, page.datasetGeneration)
        )
        this.requestCandleHistoryReset(intervalSeconds)
        throw new Error('The Perps candle dataset changed while history was loading')
      }

      const pageBars = seriesKind === 'directional-volume'
        ? perpsBasketCandlesToDirectionalVolumeBars(page.candles, intervalSeconds)
        : perpsBasketCandlesToTradingViewBars(
            page.candles,
            intervalSeconds,
            page.displayPriceCap
          )
      for (const bar of pageBars) {
        if (bar.time < requestedToMs) barsByTime.set(bar.time, bar)
      }

      const previousCursor = page.previousCursor
      if (
        barsByTime.size >= targetCount ||
        !page.hasEarlier ||
        previousCursor === null ||
        previousCursor <= 0 ||
        previousCursor >= cursor
      ) {
        break
      }
      cursor = previousCursor
    }

    if (requestGeneration !== undefined && requestIdentity !== undefined) {
      this.observeCandleIdentity(intervalSeconds, requestIdentity)
      this.observeDatasetGeneration(intervalSeconds, requestGeneration)
      if (requestCoverageStart !== undefined) {
        this.candleCoverageBoundaries.set(intervalSeconds, {
          coverageStart: requestCoverageStart,
          datasetGeneration: requestGeneration,
        })
      }
      // Every page used by this response was checked against one generation.
      // A future older-page request still detects and repairs an old edge entry.
      this.candleIntervalsNeedingRevalidation.delete(intervalSeconds)
      if (requestVolumeCoverageState !== undefined) {
        this.publishVolumeCoverageState(intervalSeconds, requestVolumeCoverageState)
      }
    }
    let primedCurrentBar: TradingViewBar | undefined
    if (currentCandleRequest) {
      const currentResponse = await settleWithin(
        currentCandleRequest,
        INITIAL_CURRENT_CANDLE_WAIT_MS
      )
      if (currentResponse && requestGeneration !== undefined && requestIdentity !== undefined) {
        try {
          this.validateCurrentCandleResponse(currentResponse, intervalSeconds)
          const currentVolumeCoverageState = this.volumeCoverageState(
            currentResponse,
            intervalSeconds
          )
          const currentIdentity = this.candleIdentity(currentResponse)
          if (
            currentResponse.datasetGeneration === requestGeneration &&
            this.candleIdentitiesEqual(currentIdentity, requestIdentity)
          ) {
            this.publishVolumeCoverageState(intervalSeconds, currentVolumeCoverageState)
            const currentBar = currentResponse.candle
              ? (seriesKind === 'directional-volume'
                  ? perpsBasketCandlesToDirectionalVolumeBars(
                      [currentResponse.candle],
                      intervalSeconds
                    )
                  : perpsBasketCandlesToTradingViewBars(
                      [currentResponse.candle],
                      intervalSeconds,
                      currentResponse.displayPriceCap
                    )).at(0)
              : undefined
            if (currentBar && currentBar.time < requestedToMs) {
              barsByTime.set(currentBar.time, currentBar)
              primedCurrentBar = currentBar
            }
          }
        } catch {
          // Finalized history remains usable when the optional live snapshot is
          // malformed or changes generation while the first page is loading.
          // The normal subscription poll will revalidate and recover it.
        }
      }
    }
    if (seriesKind === 'price') this.initializedCandleIntervals.add(intervalSeconds)

    const bars = [...barsByTime.values()]
      .sort((left, right) => left.time - right.time)
      .slice(-targetCount)
    const lastBar = bars.at(-1)
    const previousLastBar = seriesKind === 'price' ? this.lastBars.get(resolution) : undefined
    if (
      seriesKind === 'price' &&
      lastBar &&
      (!previousLastBar || lastBar.time >= previousLastBar.time)
    ) {
      this.lastBars.set(resolution, { ...lastBar })
    }
    if (
      seriesKind === 'price' &&
      primedCurrentBar &&
      bars.some((bar) => bar.time === primedCurrentBar.time)
    ) {
      this.primedCurrentCandles.set(resolution, {
        bar: { ...primedCurrentBar },
        primedAt: Date.now(),
      })
    }
    return bars
  }

  private consumePrimedCurrentCandle(
    resolution: TradingViewResolution
  ): PrimedCurrentCandle | undefined {
    const primed = this.primedCurrentCandles.get(resolution)
    this.primedCurrentCandles.delete(resolution)
    if (!primed || Date.now() - primed.primedAt > this.pollIntervalMs) return undefined
    return primed
  }

  private async pollSubscription(listenerGuid: string): Promise<void> {
    const subscription = this.subscriptions.get(listenerGuid)
    if (!subscription || subscription.polling) return
    if (Date.now() < subscription.nextPollAt) return

    subscription.polling = true
    try {
      await this.pollCurrentCandle(listenerGuid, subscription)
    } catch (error) {
      const current = this.subscriptions.get(listenerGuid)
      if (current && !isAbortError(error)) {
        current.failureCount += 1
        current.nextPollAt = Date.now() + Math.min(
          60_000,
          this.pollIntervalMs * 3 * (2 ** (current.failureCount - 1))
        )
      }
      // Keep the last bar visible; repeated failures back off to one minute.
    } finally {
      const current = this.subscriptions.get(listenerGuid)
      if (current) current.polling = false
    }
  }

  private async pollCurrentCandle(
    listenerGuid: string,
    subscription: Subscription
  ): Promise<void> {
    const getCurrentCandle = this.dataSource.getCurrentCandle
    if (!getCurrentCandle) {
      throw new Error('The Perps current-candle API data source is unavailable')
    }

    const intervalSeconds = candleIntervalForTradingViewResolution(subscription.resolution)
    const loadCurrent = (revalidate: boolean) => this.runRequest(
      (signal) => revalidate
        ? getCurrentCandle(intervalSeconds, signal, true)
        : getCurrentCandle(intervalSeconds, signal),
      subscription
    )
    let currentResponse = await loadCurrent(
      this.candleIntervalsNeedingRevalidation.has(intervalSeconds)
    )
    this.validateCurrentCandleResponse(currentResponse, intervalSeconds)
    let currentVolumeCoverageState = this.volumeCoverageState(
      currentResponse,
      intervalSeconds
    )
    let currentIdentity = this.candleIdentity(currentResponse)
    let didRevalidate = this.candleIntervalsNeedingRevalidation.has(intervalSeconds)
    const knownIdentity = this.candleDatasetIdentities.get(intervalSeconds)
    if (knownIdentity && !this.candleIdentitiesEqual(knownIdentity, currentIdentity)) {
      this.dataSource.clearCandlePageCache?.(intervalSeconds)
      if (this.subscriptions.get(listenerGuid) !== subscription) return
      currentResponse = await loadCurrent(true)
      didRevalidate = true
      this.validateCurrentCandleResponse(currentResponse, intervalSeconds)
      currentVolumeCoverageState = this.volumeCoverageState(
        currentResponse,
        intervalSeconds
      )
      currentIdentity = this.candleIdentity(currentResponse)
    }
    let identityChanged = this.observeCandleIdentity(intervalSeconds, currentIdentity)

    const knownGeneration = this.datasetGenerations.get(intervalSeconds)
    if (
      knownGeneration !== undefined &&
      currentResponse.datasetGeneration < knownGeneration
    ) {
      this.dataSource.clearCandlePageCache?.(intervalSeconds)
      if (didRevalidate) {
        this.requestCandleHistoryReset(intervalSeconds)
        throw new Error('The Perps candle API returned a stale dataset generation')
      }
      if (this.subscriptions.get(listenerGuid) !== subscription) return
      currentResponse = await loadCurrent(true)
      didRevalidate = true
      this.validateCurrentCandleResponse(currentResponse, intervalSeconds)
      currentVolumeCoverageState = this.volumeCoverageState(
        currentResponse,
        intervalSeconds
      )
      currentIdentity = this.candleIdentity(currentResponse)
      identityChanged = this.observeCandleIdentity(intervalSeconds, currentIdentity) ||
        identityChanged
      const revalidatedKnownGeneration = this.datasetGenerations.get(intervalSeconds)
      if (
        revalidatedKnownGeneration !== undefined &&
        currentResponse.datasetGeneration < revalidatedKnownGeneration
      ) {
        this.requestCandleHistoryReset(intervalSeconds)
        throw new Error('The Perps candle API returned a stale dataset generation')
      }
    }

    const current = this.subscriptions.get(listenerGuid)
    if (!current) return

    const generationAdvanced = this.observeDatasetGeneration(
      intervalSeconds,
      currentResponse.datasetGeneration
    )
    this.candleIntervalsNeedingRevalidation.delete(intervalSeconds)
    this.publishVolumeCoverageState(intervalSeconds, currentVolumeCoverageState)
    current.failureCount = 0
    current.nextPollAt = 0
    const candle = currentResponse.candle
    if (!candle) return

    const bar = (current.seriesKind === 'directional-volume'
      ? perpsBasketCandlesToDirectionalVolumeBars([candle], intervalSeconds)
      : perpsBasketCandlesToTradingViewBars(
          [candle],
          intervalSeconds,
          currentResponse.displayPriceCap
        )).at(0)
    if (!bar || (current.currentBar && bar.time < current.currentBar.time)) return
    const intervalMilliseconds = intervalSeconds * 1000
    if (
      !identityChanged &&
      !generationAdvanced &&
      current.currentBar &&
      bar.time > current.currentBar.time + intervalMilliseconds
    ) {
      // Polling intentionally pauses in hidden tabs and backs off on failures.
      // Refetch finalized history when time jumped; do not invent gap candles.
      this.requestCandleHistoryReset(intervalSeconds)
    }
    current.currentBar = { ...bar }
    if (current.seriesKind === 'price') {
      this.lastBars.set(current.resolution, { ...bar })
    }
    this.scheduleTick(current, bar)
  }

  private validateCandlePage(
    page: PerpsBasketCandlePage,
    intervalSeconds: PerpsCandleIntervalSeconds,
    cursor: number
  ): void {
    if (page.intervalSeconds !== intervalSeconds || page.cursor !== cursor) {
      throw new Error('The Perps candle API returned a mismatched page')
    }
    if (!Number.isSafeInteger(page.datasetGeneration) || page.datasetGeneration <= 0) {
      throw new Error('The Perps candle API returned an invalid dataset generation')
    }
    this.candleIdentity(page)
    if (!page.coverageComplete) {
      throw new Error('The Perps candle API returned incomplete page coverage')
    }
    this.candleCoverageStart(page)

    const pageSpan = intervalSeconds * PERPS_CANDLE_PAGE_BUCKETS
    const previousCursor = page.previousCursor
    if (
      (previousCursor === null && page.hasEarlier) ||
      (previousCursor !== null && (
        !page.hasEarlier ||
        !Number.isSafeInteger(previousCursor) ||
        previousCursor <= 0 ||
        previousCursor >= cursor ||
        previousCursor % pageSpan !== 0
      ))
    ) {
      throw new Error('The Perps candle API returned invalid page pagination')
    }

    const pageStart = cursor - pageSpan
    if (page.candles.some((candle) => (
      candle.timestamp < pageStart || candle.timestamp >= cursor
    ))) {
      throw new Error('The Perps candle API returned a candle outside its page')
    }
  }

  private validateCurrentCandleResponse(
    response: PerpsBasketCurrentCandle,
    intervalSeconds: PerpsCandleIntervalSeconds
  ): void {
    if (response.intervalSeconds !== intervalSeconds) {
      throw new Error('The Perps candle API returned a mismatched current interval')
    }
    if (!Number.isSafeInteger(response.datasetGeneration) || response.datasetGeneration <= 0) {
      throw new Error('The Perps candle API returned an invalid dataset generation')
    }
    this.candleIdentity(response)
    if (!response.coverageComplete) {
      throw new Error('The Perps candle API returned incomplete current coverage')
    }
  }

  private volumeCoverageState(
    response: Pick<
      PerpsBasketCandlePage | PerpsBasketCurrentCandle,
      'volumeCoverageStart' | 'volumeCoverageEnd' |
      'volumeFinalizedThrough' | 'volumeCoverageComplete'
    >,
    intervalSeconds: PerpsCandleIntervalSeconds
  ): PletherVolumeCoverageState {
    if (typeof response.volumeCoverageComplete !== 'boolean') {
      throw new Error('The Perps candle API returned an invalid volume coverage state')
    }

    const bounds = [
      response.volumeCoverageStart,
      response.volumeCoverageEnd,
      response.volumeFinalizedThrough,
    ]
    for (const bound of bounds) {
      if (
        bound !== null &&
        (!Number.isSafeInteger(bound) || bound < 0 || bound % intervalSeconds !== 0)
      ) {
        throw new Error('The Perps candle API returned invalid volume coverage bounds')
      }
    }

    if (!response.volumeCoverageComplete) {
      if (bounds.some((bound) => bound !== null)) {
        throw new Error('The Perps candle API returned populated unavailable volume bounds')
      }
      return 'unavailable'
    }

    const [coverageStart, coverageEnd, finalizedThrough] = bounds
    if (
      coverageStart === null ||
      coverageEnd === null ||
      finalizedThrough === null ||
      coverageStart >= coverageEnd ||
      finalizedThrough < coverageStart ||
      finalizedThrough > coverageEnd
    ) {
      throw new Error('The Perps candle API returned incomplete volume coverage bounds')
    }
    return 'available'
  }

  private publishVolumeCoverageState(
    intervalSeconds: PerpsCandleIntervalSeconds,
    state: PletherVolumeCoverageState
  ): void {
    if (
      this.destroyed ||
      this.publishedVolumeCoverageStates.get(intervalSeconds) === state
    ) return

    this.publishedVolumeCoverageStates.set(intervalSeconds, state)
    this.onVolumeCoverageChange?.({ intervalSeconds, state })
  }

  private observeDatasetGeneration(
    intervalSeconds: PerpsCandleIntervalSeconds,
    datasetGeneration: number
  ): boolean {
    if (!Number.isSafeInteger(datasetGeneration) || datasetGeneration <= 0) {
      throw new Error('The Perps candle API returned an invalid dataset generation')
    }
    const previousGeneration = this.datasetGenerations.get(intervalSeconds)
    if (previousGeneration === undefined) {
      this.datasetGenerations.set(intervalSeconds, datasetGeneration)
      return false
    }
    if (datasetGeneration === previousGeneration) return false
    if (datasetGeneration < previousGeneration) {
      throw new Error('The Perps candle API returned a stale dataset generation')
    }

    this.datasetGenerations.set(intervalSeconds, datasetGeneration)
    this.requestCandleHistoryReset(intervalSeconds)
    return true
  }

  private candleIdentity(
    response: Pick<
      PerpsBasketCandlePage | PerpsBasketCurrentCandle,
      'seriesId' | 'configurationHash' | 'displayPriceCap' |
      'volumeChainId' | 'volumeRouter'
    >
  ): CandleDatasetIdentity {
    const seriesId = response.seriesId.trim()
    const configurationHash = response.configurationHash.trim()
    const volumeRouter = typeof response.volumeRouter === 'string'
      ? response.volumeRouter.trim().toLowerCase()
      : ''
    if (
      !seriesId ||
      !configurationHash ||
      !Number.isSafeInteger(response.volumeChainId) ||
      response.volumeChainId <= 0 ||
      !/^0x[0-9a-f]{40}$/.test(volumeRouter)
    ) {
      throw new Error('The Perps candle API returned an invalid dataset identity')
    }
    if (parsePerpsDisplayPriceCap(response.displayPriceCap) === undefined) {
      throw new Error('The Perps candle API returned an invalid display-price cap')
    }
    return {
      seriesId,
      configurationHash,
      displayPriceCap: response.displayPriceCap,
      volumeChainId: response.volumeChainId,
      volumeRouter,
    }
  }

  private candleCoverageStart(
    response: Pick<PerpsBasketCandlePage, 'coverageStart'>
  ): number {
    const coverageStart = response.coverageStart
    if (
      typeof coverageStart !== 'number' ||
      !Number.isSafeInteger(coverageStart) ||
      coverageStart < 0
    ) {
      throw new Error('The Perps candle API returned an invalid coverage start')
    }
    return coverageStart
  }

  private candleIdentitiesEqual(
    left: CandleDatasetIdentity,
    right: CandleDatasetIdentity
  ): boolean {
    return left.seriesId === right.seriesId &&
      left.configurationHash === right.configurationHash &&
      left.displayPriceCap === right.displayPriceCap &&
      left.volumeChainId === right.volumeChainId &&
      left.volumeRouter === right.volumeRouter
  }

  private observeCandleIdentity(
    intervalSeconds: PerpsCandleIntervalSeconds,
    identity: CandleDatasetIdentity
  ): boolean {
    const previousIdentity = this.candleDatasetIdentities.get(intervalSeconds)
    if (previousIdentity === undefined) {
      this.candleDatasetIdentities.set(intervalSeconds, identity)
      return false
    }
    if (this.candleIdentitiesEqual(previousIdentity, identity)) return false

    this.candleDatasetIdentities.set(intervalSeconds, identity)
    this.datasetGenerations.delete(intervalSeconds)
    this.requestCandleHistoryReset(intervalSeconds)
    return true
  }

  private requestCandleHistoryReset(intervalSeconds: PerpsCandleIntervalSeconds): void {
    this.candleCoverageBoundaries.delete(intervalSeconds)
    this.initializedCandleIntervals.delete(intervalSeconds)
    if (this.pendingCandleHistoryResets.has(intervalSeconds)) return
    this.pendingCandleHistoryResets.add(intervalSeconds)
    this.prepareCandleRevalidation(intervalSeconds)

    for (const resolution of TRADINGVIEW_RESOLUTIONS) {
      if (secondsForTradingViewResolution(resolution) !== intervalSeconds) continue
      this.lastBars.delete(resolution)
      this.primedCurrentCandles.delete(resolution)
    }
    for (const subscription of this.subscriptions.values()) {
      if (secondsForTradingViewResolution(subscription.resolution) !== intervalSeconds) continue
      subscription.currentBar = undefined
      this.scheduleCacheReset(subscription)
    }

    setTimeout(() => {
      this.pendingCandleHistoryResets.delete(intervalSeconds)
      if (!this.destroyed) this.onHistoryGap?.(intervalSeconds)
    }, 0)
  }

  private prepareCandleRevalidation(intervalSeconds: PerpsCandleIntervalSeconds): void {
    this.candleCoverageBoundaries.delete(intervalSeconds)
    this.candleIntervalsNeedingRevalidation.add(intervalSeconds)
    this.dataSource.clearCandlePageCache?.(intervalSeconds)
  }

  private isDocumentVisible(): boolean {
    return typeof document === 'undefined' || document.visibilityState !== 'hidden'
  }

  private startSubscriptionTimer(
    listenerGuid: string,
    subscription: Subscription
  ): void {
    if (subscription.timer !== undefined || !this.isDocumentVisible()) return
    subscription.timer = setInterval(() => {
      void this.pollSubscription(listenerGuid)
    }, this.pollIntervalMs)
  }

  private stopSubscriptionTimer(subscription: Subscription): void {
    if (subscription.timer === undefined) return
    clearInterval(subscription.timer)
    subscription.timer = undefined
  }

  private scheduleTick(subscription: Subscription, bar: TradingViewBar): void {
    const snapshot = { ...bar }
    setTimeout(() => {
      if (
        !this.destroyed &&
        this.subscriptions.get(subscription.listenerGuid) === subscription
      ) {
        subscription.onTick({ ...snapshot })
      }
    }, 0)
  }

  private scheduleCacheReset(subscription: Subscription): void {
    setTimeout(() => {
      if (
        !this.destroyed &&
        this.subscriptions.get(subscription.listenerGuid) === subscription
      ) {
        subscription.onResetCacheNeeded()
      }
    }, 0)
  }

  private async runRequest<T>(
    operation: (signal: AbortSignal) => Promise<T>,
    subscription?: Subscription
  ): Promise<T> {
    const controller = new AbortController()
    if (this.destroyed) controller.abort()
    this.requestControllers.add(controller)
    subscription?.requestControllers.add(controller)
    try {
      return await awaitWithAbort(operation(controller.signal), controller.signal)
    } finally {
      this.requestControllers.delete(controller)
      subscription?.requestControllers.delete(controller)
    }
  }
}
