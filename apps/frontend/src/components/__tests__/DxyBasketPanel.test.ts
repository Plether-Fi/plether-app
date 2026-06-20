import { describe, expect, it } from 'vitest'
import { buildCandles, oracleNumberToDisplayDxyPrice } from '../../utils/dxyBasketChart'

describe('DXY basket chart display transform', () => {
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
})
