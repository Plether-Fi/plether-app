import type { BasketComponentPrice, BasketHistoryPoint, BasketLatest, PerpsBasketCandle } from '../api'

export interface ChartPoint {
  timestamp: number
  price: number
}

export interface ChartCandle {
  timestamp: number
  open: number
  high: number
  low: number
  close: number
}

export interface OracleMarkPoint {
  timestamp: number
  basketPrice: string
}

const RAW_ORACLE_PRICE_SCALE = 100_000_000n
const LEGACY_RAW_ORACLE_PRICE_CAP = 2n * RAW_ORACLE_PRICE_SCALE
const MAX_SAFE_ORACLE_INTEGER = BigInt(Number.MAX_SAFE_INTEGER)

export function parsePerpsDisplayPriceCap(value: string): bigint | undefined {
  if (typeof value !== 'string' || !/^[1-9]\d*$/.test(value)) return undefined

  try {
    const displayPriceCap = BigInt(value)
    return displayPriceCap <= MAX_SAFE_ORACLE_INTEGER ? displayPriceCap : undefined
  } catch {
    return undefined
  }
}

function parseRawOraclePrice(value: string, displayPriceCap: bigint): bigint | undefined {
  if (!/^\d+$/.test(value)) return undefined

  try {
    const rawPrice = BigInt(value)
    return rawPrice > 0n && rawPrice < displayPriceCap ? rawPrice : undefined
  } catch {
    return undefined
  }
}

function rawOraclePriceToDisplay(rawPrice: bigint, displayPriceCap: bigint): number {
  return Number(displayPriceCap - rawPrice) / Number(RAW_ORACLE_PRICE_SCALE)
}

/**
 * Converts a raw-domain backend OHLC candle to displayed plDXY OHLC.
 * The raw-to-display transform is decreasing, so raw low becomes display high
 * and raw high becomes display low.
 */
export function perpsBasketCandleToChartCandle(
  candle: PerpsBasketCandle,
  displayPriceCapValue: string
): ChartCandle | undefined {
  if (!Number.isSafeInteger(candle.timestamp) || candle.timestamp <= 0) return undefined

  const displayPriceCap = parsePerpsDisplayPriceCap(displayPriceCapValue)
  if (displayPriceCap === undefined) return undefined

  const rawOpen = parseRawOraclePrice(candle.rawOpenPrice, displayPriceCap)
  const rawHigh = parseRawOraclePrice(candle.rawHighPrice, displayPriceCap)
  const rawLow = parseRawOraclePrice(candle.rawLowPrice, displayPriceCap)
  const rawClose = parseRawOraclePrice(candle.rawClosePrice, displayPriceCap)
  if (
    rawOpen === undefined ||
    rawHigh === undefined ||
    rawLow === undefined ||
    rawClose === undefined ||
    rawLow > rawOpen ||
    rawLow > rawClose ||
    rawOpen > rawHigh ||
    rawClose > rawHigh
  ) {
    return undefined
  }

  return {
    timestamp: candle.timestamp,
    open: rawOraclePriceToDisplay(rawOpen, displayPriceCap),
    high: rawOraclePriceToDisplay(rawLow, displayPriceCap),
    low: rawOraclePriceToDisplay(rawHigh, displayPriceCap),
    close: rawOraclePriceToDisplay(rawClose, displayPriceCap),
  }
}

export function oracleNumberToDisplayDxyPrice(rawOraclePrice: number): number {
  if (!Number.isFinite(rawOraclePrice) || rawOraclePrice <= 0) return 0
  return Math.max(
    0,
    Number(LEGACY_RAW_ORACLE_PRICE_CAP) / Number(RAW_ORACLE_PRICE_SCALE) - rawOraclePrice
  )
}

function basketDisplayPrice(point: BasketHistoryPoint): number {
  return oracleNumberToDisplayDxyPrice(Number(point.basketPrice) / 1e8)
}

function componentKey(component: BasketComponentPrice): string {
  return component.feedId || component.symbol
}

function componentOraclePrice(component: BasketComponentPrice): number {
  return Number(component.price) / 1e8
}

function findHistoricalComponent(
  points: BasketHistoryPoint[],
  key: string,
  targetTimestamp: number,
  latestTimestamp: number
): BasketComponentPrice | undefined {
  let nearestComponent: BasketComponentPrice | undefined
  let nearestDistance = Number.POSITIVE_INFINITY

  for (const point of points) {
    if (point.timestamp >= latestTimestamp) continue

    const component = point.components?.find((item) => componentKey(item) === key)
    if (!component) continue

    const distance = Math.abs(point.timestamp - targetTimestamp)
    if (distance < nearestDistance) {
      nearestComponent = component
      nearestDistance = distance
    }
  }

  return nearestComponent
}

export function computeBasketDisplayPriceChange(
  historyPoints: BasketHistoryPoint[] | undefined,
  latest: BasketLatest | undefined
): number | undefined {
  if (!latest || !historyPoints?.length) return undefined

  const mergedPoints = mergeLatestBasketPoint(historyPoints, latest)
  const firstPoint = mergedPoints.at(0)
  const latestPoint = mergedPoints.at(-1)
  if (!firstPoint || !latestPoint || firstPoint.timestamp === latestPoint.timestamp) return undefined

  const firstPrice = basketDisplayPrice(firstPoint)
  const latestPrice = basketDisplayPrice(latestPoint)
  if (firstPrice <= 0) return undefined

  return (latestPrice - firstPrice) / firstPrice
}

export function computeBasketComponentPriceChanges(
  historyPoints: BasketHistoryPoint[] | undefined,
  latest: BasketLatest | undefined,
  windowSeconds = 24 * 60 * 60
): Partial<Record<string, number>> {
  if (!latest || !historyPoints?.length) return {}

  const points = [...mergeLatestBasketPoint(historyPoints, latest)].sort((left, right) => left.timestamp - right.timestamp)
  const latestPoint = points.at(-1)
  if (latestPoint?.timestamp !== latest.timestamp) return {}

  const targetTimestamp = latest.timestamp - windowSeconds
  const changes: Partial<Record<string, number>> = {}

  for (const latestComponent of latest.components) {
    const key = componentKey(latestComponent)
    const latestPrice = componentOraclePrice(latestComponent)
    if (!key || latestPrice <= 0) continue

    const historicalComponent = findHistoricalComponent(points, key, targetTimestamp, latest.timestamp)
    if (!historicalComponent) continue

    const historicalPrice = componentOraclePrice(historicalComponent)
    if (historicalPrice <= 0) continue

    changes[key] = (latestPrice - historicalPrice) / historicalPrice
  }

  return changes
}

export function mergeLatestBasketPoint(
  historyPoints: BasketHistoryPoint[],
  latest: BasketLatest | undefined
): BasketHistoryPoint[] {
  if (!latest) return historyPoints

  const lastPoint = historyPoints.at(-1)
  const livePoint: BasketHistoryPoint = {
    timestamp: latest.timestamp,
    basketPrice: latest.basketPrice,
    volumeUsdc: latest.timestamp === lastPoint?.timestamp ? lastPoint.volumeUsdc : '0',
    components: latest.components,
  }
  if (!lastPoint) return [livePoint]
  if (latest.timestamp < lastPoint.timestamp) return historyPoints
  if (latest.timestamp === lastPoint.timestamp) {
    return [...historyPoints.slice(0, -1), livePoint]
  }

  return [...historyPoints, livePoint]
}

export function alignBasketPointsToOracleMark(
  historyPoints: BasketHistoryPoint[],
  latest: BasketLatest | undefined,
  oracleMark: OracleMarkPoint | undefined
): BasketHistoryPoint[] {
  const points = mergeLatestBasketPoint(historyPoints, latest)
  if (!oracleMark || oracleMark.timestamp <= 0 || !oracleMark.basketPrice) return points

  const components = latest?.components ?? points.at(-1)?.components
  const replacedPoint = points.find((point) => point.timestamp === oracleMark.timestamp)
  const markPoint: BasketHistoryPoint = {
    timestamp: oracleMark.timestamp,
    basketPrice: oracleMark.basketPrice,
    volumeUsdc: replacedPoint?.volumeUsdc ?? '0',
    ...(components ? { components } : {}),
  }

  return [
    ...points.filter((point) => point.timestamp < oracleMark.timestamp),
    markPoint,
  ]
}

export function buildCandles(points: ChartPoint[], intervalSeconds: number): ChartCandle[] {
  const candles: ChartCandle[] = []
  let currentCandle: ChartCandle | undefined
  let previousClose: number | null = null

  const sortedPoints = [...points].sort((left, right) => left.timestamp - right.timestamp)

  for (const point of sortedPoints) {
    const timestamp = Math.floor(point.timestamp / intervalSeconds) * intervalSeconds

    if (currentCandle?.timestamp === timestamp) {
      currentCandle.high = Math.max(currentCandle.high, point.price)
      currentCandle.low = Math.min(currentCandle.low, point.price)
      currentCandle.close = point.price
      previousClose = point.price
      continue
    }

    if (currentCandle) {
      candles.push(currentCandle)
    }

    const open = previousClose ?? point.price
    currentCandle = {
      timestamp,
      open,
      high: Math.max(open, point.price),
      low: Math.min(open, point.price),
      close: point.price,
    }
    previousClose = point.price
  }

  if (currentCandle) {
    candles.push(currentCandle)
  }

  return candles
}
