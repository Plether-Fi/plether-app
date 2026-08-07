import { describe, expect, it } from 'vitest'
import {
  alignBasketPointsToOracleMark,
  buildCandles,
  computeBasketDisplayPriceChange,
  computeBasketComponentPriceChanges,
  mergeLatestBasketPoint,
  oracleNumberToDisplayDxyPrice,
} from '../../utils/dxyBasketChart'
import type { BasketComponentPrice, BasketHistoryPoint, BasketLatest } from '../../api'
import {
  DEFAULT_DXY_BASKET_CHART_INTERVAL,
  DXY_BASKET_CHART_INTERVALS,
  basketRangeForChartInterval,
  basketRequestIntervalSecondsForChartInterval,
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
  it('opens on the five-day chart view by default', () => {
    expect(DEFAULT_DXY_BASKET_CHART_INTERVAL).toBe('5m')
    expect(basketRangeForChartInterval(DEFAULT_DXY_BASKET_CHART_INTERVAL)).toBe('7d')
  })

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

  it('requests only the resolution needed for each chart interval', () => {
    expect(basketRequestIntervalSecondsForChartInterval('1m')).toBe(60)
    expect(basketRequestIntervalSecondsForChartInterval('5m')).toBe(5 * 60)
    expect(basketRequestIntervalSecondsForChartInterval('1h')).toBe(60 * 60)
    expect(basketRequestIntervalSecondsForChartInterval('1d')).toBe(24 * 60 * 60)
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
    const changePct = computeBasketDisplayPriceChange(
      [historyPoint(60, '98000000'), historyPoint(120, '97000000')],
      latestPoint(180, '96000000')
    )

    expect(changePct).toBeCloseTo((1.04 - 1.02) / 1.02, 8)
  })

  it('does not report a 0% change when only the live latest point is available', () => {
    expect(computeBasketDisplayPriceChange([], latestPoint(180, '97000000'))).toBeUndefined()
    expect(computeBasketDisplayPriceChange(undefined, latestPoint(180, '97000000'))).toBeUndefined()
  })

  it('computes display percent change from history and the live latest point', () => {
    const change = computeBasketDisplayPriceChange(
      [historyPoint(60, '98000000')],
      latestPoint(180, '97000000')
    )

    expect(change).toBeCloseTo((1.03 - 1.02) / 1.02, 8)
  })

  it('computes component price changes from the 24h comparison point', () => {
    const latest = latestPoint(200_000, '97000000')
    latest.components = [{ ...component, price: '101000000' }]
    const changes = computeBasketComponentPriceChanges(
      [
        {
          timestamp: latest.timestamp - 25 * 60 * 60,
          basketPrice: '98000000',
          components: [{ ...component, price: '99000000' }],
        },
        {
          timestamp: latest.timestamp - 24 * 60 * 60,
          basketPrice: '98000000',
          components: [{ ...component, price: '100000000' }],
        },
        {
          timestamp: latest.timestamp - 60,
          basketPrice: '98000000',
          components: [{ ...component, price: '100500000' }],
        },
      ],
      latest
    )

    expect(changes[component.feedId]).toBeCloseTo(0.01, 8)
  })

  it('does not compute component price changes without historical component data', () => {
    const latest = latestPoint(200_000, '97000000')
    latest.components = [{ ...component, price: '101000000' }]

    expect(computeBasketComponentPriceChanges([], latest)).toEqual({})
    expect(computeBasketComponentPriceChanges([historyPoint(latest.timestamp, '97000000')], latest)).toEqual({})
  })

  it('replaces the current history bucket with the live latest point', () => {
    const merged = mergeLatestBasketPoint(
      [historyPoint(60, '98000000'), historyPoint(120, '97000000')],
      latestPoint(120, '96000000')
    )

    expect(merged).toHaveLength(2)
    expect(merged.at(-1)?.basketPrice).toBe('96000000')
    expect(merged.at(-1)?.components?.[0]?.publishTime).toBe(121)
  })

  it('appends the live latest point when it has moved into a new bucket', () => {
    const merged = mergeLatestBasketPoint(
      [historyPoint(60, '98000000'), historyPoint(120, '97000000')],
      latestPoint(180, '96000000')
    )

    expect(merged.map((point) => point.timestamp)).toEqual([60, 120, 180])
  })

  it('uses the on-chain mark as the current chart point', () => {
    const aligned = alignBasketPointsToOracleMark(
      [historyPoint(60, '98000000'), historyPoint(120, '97000000')],
      latestPoint(180, '96000000'),
      { timestamp: 150, basketPrice: '96500000' }
    )

    expect(aligned.map((point) => point.timestamp)).toEqual([60, 120, 150])
    expect(aligned.at(-1)?.basketPrice).toBe('96500000')
  })

  it('replaces a backend sample from the same timestamp with the on-chain mark', () => {
    const aligned = alignBasketPointsToOracleMark(
      [historyPoint(60, '98000000'), historyPoint(120, '97000000')],
      latestPoint(180, '96000000'),
      { timestamp: 120, basketPrice: '96500000' }
    )

    expect(aligned).toHaveLength(2)
    expect(aligned.at(-1)?.timestamp).toBe(120)
    expect(aligned.at(-1)?.basketPrice).toBe('96500000')
  })
})
