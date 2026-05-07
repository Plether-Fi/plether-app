import type { BasketHistoryRange } from '../api'

export type DxyBasketChartInterval = '5m' | '1h' | '1d'

export const DXY_BASKET_CHART_INTERVALS: {
  value: DxyBasketChartInterval
  label: string
  range: BasketHistoryRange
  ariaLabel: string
}[] = [
  { value: '5m', label: '5m', range: '24h', ariaLabel: '5 minute interval' },
  { value: '1h', label: '1H', range: '7d', ariaLabel: '1 hour interval' },
  { value: '1d', label: 'D', range: '30d', ariaLabel: '1 day interval' },
]

export function basketRangeForChartInterval(interval: DxyBasketChartInterval): BasketHistoryRange {
  return DXY_BASKET_CHART_INTERVALS.find((item) => item.value === interval)?.range ?? '7d'
}
