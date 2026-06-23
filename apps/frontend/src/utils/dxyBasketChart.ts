import type { BasketHistoryPoint, BasketLatest } from '../api'

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

export function oracleNumberToDisplayDxyPrice(rawOraclePrice: number): number {
  if (!Number.isFinite(rawOraclePrice) || rawOraclePrice <= 0) return 0
  return Math.max(0, 2 - rawOraclePrice)
}

function basketDisplayPrice(point: BasketHistoryPoint): number {
  return oracleNumberToDisplayDxyPrice(Number(point.basketPrice) / 1e8)
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

export function mergeLatestBasketPoint(
  historyPoints: BasketHistoryPoint[],
  latest: BasketLatest | undefined
): BasketHistoryPoint[] {
  if (!latest) return historyPoints

  const livePoint: BasketHistoryPoint = {
    timestamp: latest.timestamp,
    basketPrice: latest.basketPrice,
    components: latest.components,
  }
  const lastPoint = historyPoints.at(-1)
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

  const components = latest?.components ?? points.at(-1)?.components ?? []
  const markPoint: BasketHistoryPoint = {
    timestamp: oracleMark.timestamp,
    basketPrice: oracleMark.basketPrice,
    components,
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
