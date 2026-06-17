import type { BasketHistoryRange } from '../api'

export type DxyBasketChartInterval = '1m' | '5m' | '1h' | '1d'

export const DXY_BASKET_CHART_INTERVALS: {
  value: DxyBasketChartInterval
  label: string
  range: BasketHistoryRange
  intervalSeconds: number
  ariaLabel: string
}[] = [
  { value: '1m', label: '1m', range: '7d', intervalSeconds: 60, ariaLabel: '1 minute interval' },
  { value: '5m', label: '5m', range: '7d', intervalSeconds: 5 * 60, ariaLabel: '5 minute interval' },
  { value: '1h', label: '1H', range: '7d', intervalSeconds: 60 * 60, ariaLabel: '1 hour interval' },
  { value: '1d', label: 'D', range: '30d', intervalSeconds: 24 * 60 * 60, ariaLabel: '1 day interval' },
]

export function basketRangeForChartInterval(interval: DxyBasketChartInterval): BasketHistoryRange {
  return DXY_BASKET_CHART_INTERVALS.find((item) => item.value === interval)?.range ?? '7d'
}

export function basketIntervalSecondsForChartInterval(interval: DxyBasketChartInterval): number {
  return DXY_BASKET_CHART_INTERVALS.find((item) => item.value === interval)?.intervalSeconds ?? 60 * 60
}

export function basketRequestIntervalSecondsForChartInterval(_interval: DxyBasketChartInterval): number {
  const requestIntervals: Record<DxyBasketChartInterval, number> = {
    '1m': 60,
    '5m': 60,
    '1h': 60,
    '1d': 60,
  }

  return requestIntervals[_interval]
}
