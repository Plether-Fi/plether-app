import { Result } from 'better-result'
import type { QueryClient } from '@tanstack/react-query'
import {
  apiQueryKeys,
  perpsApi,
  type ApiResponse,
  type BasketHistory,
  type BasketHistoryPoint,
  type BasketHistoryRange,
  type BasketLatest,
} from '../api'
import {
  alignBasketPointsToOracleMark,
  buildCandles,
  oracleNumberToDisplayDxyPrice,
  type OracleMarkPoint,
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

const MICRO_USDC_PER_USDC = 1_000_000n
const LIVE_VOLUME_REFRESH_MS = 60_000

const SYMBOL_INFO: TradingViewSymbolInfo = {
  name: 'plDXY.P',
  ticker: PLDXY_TRADINGVIEW_SYMBOL,
  description: 'plDXY Perpetual',
  type: 'futures',
  session: '24x7',
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
  getHistory: (
    range: BasketHistoryRange,
    intervalSeconds: number,
    signal?: AbortSignal
  ) => Promise<BasketHistory>
  getLatest: (signal?: AbortSignal) => Promise<BasketLatest | undefined>
}

interface Subscription {
  listenerGuid: string
  resolution: TradingViewResolution
  onTick: (bar: TradingViewBar) => void
  onResetCacheNeeded: () => void
  timer?: ReturnType<typeof setInterval>
  polling: boolean
  failureCount: number
  nextPollAt: number
  requestControllers: Set<AbortController>
  currentBar?: TradingViewBar
}

interface VolumeSnapshot {
  byTime: Map<number, number>
}

export interface PletherDxyDatafeedOptions {
  dataSource?: PletherChartDataSource
  queryClient?: QueryClient
  pollIntervalMs?: number
  oracleMark?: OracleMarkPoint
  onHistoryGap?: () => void
}

async function fetchBasketHistory(
  range: BasketHistoryRange,
  intervalSeconds: number,
  signal?: AbortSignal
): Promise<ApiResponse<BasketHistory>> {
  const result = await perpsApi.getPerpsBasketHistory(
    range,
    intervalSeconds,
    false,
    signal
  )
  if (Result.isError(result)) throw result.error
  return result.value
}

async function fetchBasketLatest(signal?: AbortSignal): Promise<ApiResponse<BasketLatest>> {
  const result = await perpsApi.getPerpsBasketLatest(signal)
  if (Result.isError(result)) throw result.error
  return result.value
}

function createApiDataSource(queryClient: QueryClient | undefined): PletherChartDataSource {
  return {
    async getHistory(range, intervalSeconds, signal) {
      if (!queryClient) return (await fetchBasketHistory(range, intervalSeconds, signal)).data

      const response = await awaitWithAbort(
        queryClient.fetchQuery({
          queryKey: apiQueryKeys.perps.basketHistory(range, intervalSeconds),
          queryFn: ({ signal: querySignal }) => fetchBasketHistory(
            range,
            intervalSeconds,
            querySignal
          ),
          staleTime: 60_000,
          retry: retryTransientFailureOnce,
        }),
        signal
      )
      return response.data
    },
    async getLatest(signal) {
      if (!queryClient) return (await fetchBasketLatest(signal)).data

      const response = await awaitWithAbort(
        queryClient.fetchQuery({
          queryKey: apiQueryKeys.perps.basketLatest(),
          queryFn: ({ signal: querySignal }) => fetchBasketLatest(querySignal),
          staleTime: 5_000,
          retry: retryTransientFailureOnce,
        }),
        signal
      )
      return response.data
    },
  }
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

export function historyRangeForRequest(
  from: number,
  to: number,
  countBack: number,
  resolution: TradingViewResolution,
  now = Math.floor(Date.now() / 1000)
): BasketHistoryRange {
  const minimumSeconds: Record<TradingViewResolution, number> = {
    '1': 24 * 60 * 60,
    '3': 7 * 24 * 60 * 60,
    '5': 7 * 24 * 60 * 60,
    '15': 7 * 24 * 60 * 60,
    '30': 7 * 24 * 60 * 60,
    '60': 30 * 24 * 60 * 60,
    '1D': 365 * 24 * 60 * 60,
  }
  // Fine-grained bars are intentionally available for shorter lookback
  // windows. Without this cap, scrolling a one-minute chart far enough back
  // would ask the backend to materialize up to a year of minute snapshots.
  const maximumSeconds: Record<TradingViewResolution, number> = {
    '1': 7 * 24 * 60 * 60,
    '3': 7 * 24 * 60 * 60,
    '5': 30 * 24 * 60 * 60,
    '15': 30 * 24 * 60 * 60,
    '30': 30 * 24 * 60 * 60,
    '60': 365 * 24 * 60 * 60,
    '1D': 365 * 24 * 60 * 60,
  }
  const countBackSeconds = Math.max(0, countBack) * secondsForTradingViewResolution(resolution)
  const earliestRequestedTime = Math.min(from, to - countBackSeconds)
  const requestedSeconds = Math.min(
    maximumSeconds[resolution],
    Math.max(
      minimumSeconds[resolution],
      Math.max(0, to - from),
      countBackSeconds,
      Math.max(0, now - earliestRequestedTime)
    )
  )

  if (requestedSeconds <= 24 * 60 * 60) return '24h'
  if (requestedSeconds <= 7 * 24 * 60 * 60) return '7d'
  if (requestedSeconds <= 30 * 24 * 60 * 60) return '30d'
  return '1y'
}

export function basketPointsToTradingViewBars(
  points: BasketHistoryPoint[],
  resolution: TradingViewResolution,
  volumePoints: BasketHistoryPoint[] = points
): TradingViewBar[] {
  const intervalSeconds = secondsForTradingViewResolution(resolution)
  const chartPoints = points
    .map((point) => ({
      timestamp: point.timestamp,
      price: oracleNumberToDisplayDxyPrice(Number(point.basketPrice) / 1e8),
    }))
    .filter((point) => point.timestamp > 0 && point.price > 0)

  const volumeByTimestamp = new Map<number, bigint>()
  const seenVolumeTimestamps = new Set<number>()
  for (const point of volumePoints) {
    if (point.timestamp <= 0 || seenVolumeTimestamps.has(point.timestamp)) continue

    seenVolumeTimestamps.add(point.timestamp)
    const bucketTimestamp = Math.floor(point.timestamp / intervalSeconds) * intervalSeconds
    const currentVolume = volumeByTimestamp.get(bucketTimestamp) ?? 0n
    volumeByTimestamp.set(
      bucketTimestamp,
      currentVolume + parseMicroUsdc(point.volumeUsdc)
    )
  }

  return buildCandles(chartPoints, intervalSeconds).map((candle) => ({
    time: candle.timestamp * 1000,
    open: candle.open,
    high: candle.high,
    low: candle.low,
    close: candle.close,
    volume: microUsdcToHumanUsdc(volumeByTimestamp.get(candle.timestamp) ?? 0n),
  }))
}

function parseMicroUsdc(value: string | undefined): bigint {
  const normalized = value?.trim()
  if (!normalized || !/^\d+$/.test(normalized)) return 0n

  try {
    return BigInt(normalized)
  } catch {
    return 0n
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

function barFromLivePoint(
  basketPrice: string,
  timestamp: number,
  resolution: TradingViewResolution,
  previousBar: TradingViewBar | undefined,
  volume: number | undefined
): TradingViewBar | undefined {
  const price = oracleNumberToDisplayDxyPrice(Number(basketPrice) / 1e8)
  if (!Number.isFinite(price) || price <= 0 || timestamp <= 0) return undefined

  const intervalMilliseconds = secondsForTradingViewResolution(resolution) * 1000
  const time = Math.floor((timestamp * 1000) / intervalMilliseconds) * intervalMilliseconds

  if (previousBar && time < previousBar.time) return undefined
  if (previousBar?.time === time) {
    return {
      ...previousBar,
      high: Math.max(previousBar.high, price),
      low: Math.min(previousBar.low, price),
      close: price,
      volume: volume ?? previousBar.volume ?? 0,
    }
  }

  const open = previousBar?.close ?? price
  return {
    time,
    open,
    high: Math.max(open, price),
    low: Math.min(open, price),
    close: price,
    volume: volume ?? 0,
  }
}

export class PletherDxyDatafeed implements TradingViewDatafeed {
  private readonly dataSource: PletherChartDataSource
  private readonly pollIntervalMs: number
  private readonly onHistoryGap: (() => void) | undefined
  private readonly subscriptions = new Map<string, Subscription>()
  private readonly lastBars = new Map<TradingViewResolution, TradingViewBar>()
  private readonly volumeSnapshots = new Map<TradingViewResolution, VolumeSnapshot>()
  private readonly volumeRefreshes = new Map<
    TradingViewResolution,
    Promise<VolumeSnapshot | undefined>
  >()
  private readonly liveVolumeRefreshedAt = new Map<TradingViewResolution, number>()
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
  private oracleMark?: OracleMarkPoint
  private destroyed = false

  constructor(options: PletherDxyDatafeedOptions = {}) {
    this.dataSource = options.dataSource ?? createApiDataSource(options.queryClient)
    this.pollIntervalMs = options.pollIntervalMs ?? 5_000
    this.oracleMark = options.oracleMark
    this.onHistoryGap = options.onHistoryGap
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
      } else {
        onError(`Unknown symbol: ${symbolName}`)
      }
    }, 0)
  }

  getBars(
    _symbolInfo: TradingViewSymbolInfo,
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

    void this.runRequest((signal) => this.loadBars(resolution, periodParams, signal))
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
    _symbolInfo: TradingViewSymbolInfo,
    resolution: TradingViewResolution,
    onTick: (bar: TradingViewBar) => void,
    listenerGuid: string,
    onResetCacheNeededCallback: () => void
  ): void {
    this.unsubscribeBars(listenerGuid)

    const subscription: Subscription = {
      listenerGuid,
      resolution,
      onTick,
      onResetCacheNeeded: onResetCacheNeededCallback,
      polling: false,
      failureCount: 0,
      nextPollAt: 0,
      requestControllers: new Set(),
      currentBar: this.lastBars.get(resolution),
    }
    this.subscriptions.set(listenerGuid, subscription)
    if (this.isDocumentVisible()) {
      this.startSubscriptionTimer(listenerGuid, subscription)
      void this.pollSubscription(listenerGuid)
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

  setOracleMark(oracleMark: OracleMarkPoint | undefined): void {
    this.oracleMark = oracleMark
    if (!oracleMark) return

    let needsHistoryReset = false
    for (const subscription of this.subscriptions.values()) {
      const markTime = this.barTimeForTimestamp(subscription.resolution, oracleMark.timestamp)
      if (subscription.currentBar && markTime < subscription.currentBar.time) {
        subscription.currentBar = undefined
        this.lastBars.delete(subscription.resolution)
        this.scheduleCacheReset(subscription)
        needsHistoryReset = true
        continue
      }
      this.emitLiveBar(subscription, oracleMark.basketPrice, oracleMark.timestamp)
    }
    if (needsHistoryReset) {
      setTimeout(() => {
        if (!this.destroyed) this.onHistoryGap?.()
      }, 0)
    }
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

  private async loadBars(
    resolution: TradingViewResolution,
    periodParams: { from: number; to: number; countBack: number },
    signal?: AbortSignal
  ): Promise<TradingViewBar[]> {
    const intervalSeconds = secondsForTradingViewResolution(resolution)
    const range = historyRangeForRequest(
      periodParams.from,
      periodParams.to,
      periodParams.countBack,
      resolution
    )
    const [history, latest] = await Promise.all([
      this.dataSource.getHistory(range, intervalSeconds, signal),
      this.dataSource.getLatest(signal).catch((error: unknown) => {
        if (isAbortError(error)) throw error
        return undefined
      }),
    ])
    const points = alignBasketPointsToOracleMark(history.points, latest, this.oracleMark)
    const bars = basketPointsToTradingViewBars(points, resolution, history.points)
      .filter((bar) => bar.time < periodParams.to * 1000)
      .slice(-Math.max(1, periodParams.countBack))

    this.rememberVolumeBars(resolution, bars)
    const livePoint = this.oracleMark ?? latest
    if (livePoint) {
      const liveBarTime = this.barTimeForTimestamp(resolution, livePoint.timestamp)
      if (bars.some((bar) => bar.time === liveBarTime)) {
        this.liveVolumeRefreshedAt.set(resolution, Date.now())
      }
    }

    const lastBar = bars.at(-1)
    const previousLastBar = this.lastBars.get(resolution)
    if (lastBar && (!previousLastBar || lastBar.time >= previousLastBar.time)) {
      this.lastBars.set(resolution, lastBar)
    }
    return bars
  }

  private async pollSubscription(listenerGuid: string): Promise<void> {
    const subscription = this.subscriptions.get(listenerGuid)
    if (!subscription || subscription.polling) return
    if (Date.now() < subscription.nextPollAt) return

    subscription.polling = true
    try {
      const latest = await this.runRequest(
        (signal) => this.dataSource.getLatest(signal),
        subscription
      )
      const current = this.subscriptions.get(listenerGuid)
      if (!current) return
      current.failureCount = 0
      current.nextPollAt = 0

      const livePoint = this.oracleMark ?? latest
      if (livePoint) {
        const barTime = this.barTimeForTimestamp(current.resolution, livePoint.timestamp)
        const cachedVolume = this.volumeSnapshots
          .get(current.resolution)
          ?.byTime.get(barTime)
        this.emitLiveBar(current, livePoint.basketPrice, livePoint.timestamp, cachedVolume)
        this.queueLiveVolumeRefresh(listenerGuid, current.resolution, barTime)
      }
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

  private emitLiveBar(
    subscription: Subscription,
    basketPrice: string,
    timestamp: number,
    volume?: number
  ): void {
    const previousBar = subscription.currentBar
    const bar = barFromLivePoint(
      basketPrice,
      timestamp,
      subscription.resolution,
      previousBar,
      volume
    )
    if (!bar) return

    const intervalMilliseconds = secondsForTradingViewResolution(subscription.resolution) * 1000
    if (previousBar && bar.time > previousBar.time + intervalMilliseconds) {
      this.scheduleCacheReset(subscription)
      setTimeout(() => {
        if (!this.destroyed) this.onHistoryGap?.()
      }, 0)
    }

    subscription.currentBar = bar
    this.lastBars.set(subscription.resolution, bar)
    this.scheduleTick(subscription, bar)
  }

  private rememberVolumeBars(
    resolution: TradingViewResolution,
    bars: TradingViewBar[]
  ): void {
    const previous = this.volumeSnapshots.get(resolution)
    const byTime = new Map(previous?.byTime)
    for (const bar of bars) {
      if (bar.volume !== undefined) byTime.set(bar.time, bar.volume)
    }
    this.volumeSnapshots.set(resolution, { byTime })
  }

  private barTimeForTimestamp(
    resolution: TradingViewResolution,
    timestamp: number
  ): number {
    const intervalMilliseconds = secondsForTradingViewResolution(resolution) * 1000
    return Math.floor((timestamp * 1000) / intervalMilliseconds) * intervalMilliseconds
  }

  private queueLiveVolumeRefresh(
    listenerGuid: string,
    resolution: TradingViewResolution,
    barTime: number
  ): void {
    let refresh = this.volumeRefreshes.get(resolution)
    if (!refresh) {
      const refreshedAt = this.liveVolumeRefreshedAt.get(resolution)
      if (refreshedAt !== undefined && Date.now() - refreshedAt < LIVE_VOLUME_REFRESH_MS) return

      const subscription = this.subscriptions.get(listenerGuid)
      if (!subscription) return
      refresh = this.runRequest(
        (signal) => this.dataSource.getHistory(
            '24h',
            secondsForTradingViewResolution(resolution),
            signal
          )
      )
        .then((history) => {
          const byTime = new Map<number, number>()
          for (const bar of basketPointsToTradingViewBars(history.points, resolution)) {
            byTime.set(bar.time, bar.volume ?? 0)
          }
          const snapshot = { byTime }
          this.volumeSnapshots.set(resolution, snapshot)
          this.liveVolumeRefreshedAt.set(resolution, Date.now())
          return snapshot
        })
        .catch(() => undefined)
        .finally(() => {
          this.volumeRefreshes.delete(resolution)
        })

      this.volumeRefreshes.set(resolution, refresh)
    }

    void refresh.then((snapshot) => {
      if (!snapshot) return
      const volume = snapshot.byTime.get(barTime)
      if (volume === undefined) return

      const subscription = this.subscriptions.get(listenerGuid)
      if (subscription?.resolution !== resolution || subscription.currentBar?.time !== barTime) return
      const currentBar = subscription.currentBar
      if (currentBar.volume === volume) return

      const correctedBar = { ...currentBar, volume }
      subscription.currentBar = correctedBar
      this.lastBars.set(resolution, correctedBar)
      this.scheduleTick(subscription, correctedBar)
    })
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
