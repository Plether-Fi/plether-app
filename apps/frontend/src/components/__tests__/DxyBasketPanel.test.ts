import { describe, expect, it } from 'vitest'
import {
  basketDisplayPriceChange,
  buildCandles,
  mergeLatestBasketPoint,
  oracleNumberToDisplayDxyPrice,
} from '../../utils/dxyBasketChart'
import type { BasketComponentPrice, BasketHistoryPoint, BasketLatest } from '../../api'
import {
  DXY_BASKET_CHART_INTERVALS,
  basketRangeForChartInterval,
} from '../dxyBasketChartConfig'

const component: BasketComponentPrice = {
  symbol: 'EUR/USD',
  feedSymbol: 'EUR/USD',
  feedId: '0xfeed',
  price: '100000000',
  rawPrice: '100000',
  confidence: '1',
  exponent: -5,
  publishTime: 100,
  inverted: false,
  weightBps: 10000,
  basePrice: '100000000',
}

function historyPoint(timestamp: number, basketPrice: string): BasketHistoryPoint {
  return {
    timestamp,
    basketPrice,
    components: [component],
  }
}

function latestPoint(timestamp: number, basketPrice: string): BasketLatest {
  return {
    timestamp,
    basketPrice,
    components: [{ ...component, publishTime: timestamp + 1 }],
    generatedAt: timestamp + 2,
    source: 'database',
  }
}

describe('DXY basket chart display transform', () => {
  it('uses the intended default history window for each chart interval', () => {
    expect(Object.fromEntries(DXY_BASKET_CHART_INTERVALS.map((item) => [item.label, item.range]))).toEqual({
      '1m': '24h',
      '5m': '7d',
      '1H': '30d',
      D: '1y',
    })
    expect(basketRangeForChartInterval('1m')).toBe('24h')
    expect(basketRangeForChartInterval('5m')).toBe('7d')
    expect(basketRangeForChartInterval('1h')).toBe('30d')
    expect(basketRangeForChartInterval('1d')).toBe('1y')
  })

  it('plots raw basket prices as reversed DXY display prices', () => {
    expect(oracleNumberToDisplayDxyPrice(0.9831)).toBeCloseTo(1.0169, 8)
    expect(oracleNumberToDisplayDxyPrice(1)).toBeCloseTo(1, 8)
  })

  it('inverts candle high and low after reversing the raw basket prices', () => {
    const rawPoints = [
      { timestamp: 60, price: 0.98 },
      { timestamp: 90, price: 0.99 },
      { timestamp: 119, price: 0.97 },
    ]
    const displayPoints = rawPoints.map((point) => ({
      ...point,
      price: oracleNumberToDisplayDxyPrice(point.price),
    }))

    const [candle] = buildCandles(displayPoints, 60)

    expect(candle.open).toBeCloseTo(1.02, 8)
    expect(candle.high).toBeCloseTo(1.03, 8)
    expect(candle.low).toBeCloseTo(1.01, 8)
    expect(candle.close).toBeCloseTo(1.03, 8)
  })

  it('uses displayed DXY movement for percent-change direction', () => {
    const first = oracleNumberToDisplayDxyPrice(0.98)
    const latest = oracleNumberToDisplayDxyPrice(0.97)

    expect((latest - first) / first).toBeGreaterThan(0)
  })

  it('computes header percent change from the supplied history window', () => {
    const changePct = basketDisplayPriceChange(
      [historyPoint(60, '98000000'), historyPoint(120, '97000000')],
      latestPoint(180, '96000000')
    )

    expect(changePct).toBeCloseTo((1.04 - 1.02) / 1.02, 8)
  })

  it('replaces the current history bucket with the live latest point', () => {
    const merged = mergeLatestBasketPoint(
      [historyPoint(60, '98000000'), historyPoint(120, '97000000')],
      latestPoint(120, '96000000')
    )

    expect(merged).toHaveLength(2)
    expect(merged.at(-1)?.basketPrice).toBe('96000000')
    expect(merged.at(-1)?.components[0]?.publishTime).toBe(121)
  })

  it('appends the live latest point when it has moved into a new bucket', () => {
    const merged = mergeLatestBasketPoint(
      [historyPoint(60, '98000000'), historyPoint(120, '97000000')],
      latestPoint(180, '96000000')
    )

    expect(merged.map((point) => point.timestamp)).toEqual([60, 120, 180])
  })
})
