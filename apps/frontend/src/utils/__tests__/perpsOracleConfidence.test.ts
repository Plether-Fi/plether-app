import { describe, expect, it } from 'vitest'
import type { BasketComponentPrice, BasketLatest } from '../../api'
import {
  calculateAdverseOracleConfidenceSpreadPercent,
  calculateRawBasketOracleConfidenceSpreadPercent,
  formatAdverseConfidenceMultiplier,
  formatAdverseOracleConfidenceSpread,
  formatOracleConfidenceSpreadPercent,
} from '../perpsOracleConfidence'

const component: BasketComponentPrice = {
  symbol: 'EUR/USD',
  feedSymbol: 'EUR/USD',
  feedId: '0xfeed',
  price: '100000000',
  rawPrice: '100000000',
  confidence: '10000',
  exponent: -8,
  publishTime: 100,
  inverted: false,
  weightBps: 10000,
  basePrice: '100000000',
}

function latestBasket(overrides: Partial<BasketLatest> = {}): BasketLatest {
  return {
    timestamp: 100,
    basketPrice: '100000000',
    components: [component],
    generatedAt: 101,
    source: 'database',
    ...overrides,
  }
}

describe('adverse oracle confidence spread', () => {
  it('keeps the raw confidence spread unchanged at 10000 bps', () => {
    expect(calculateRawBasketOracleConfidenceSpreadPercent(latestBasket()))
      .toBeCloseTo(0.01, 8)
    expect(formatOracleConfidenceSpreadPercent(0.01)).toBe('~0.0100%')
    expect(calculateAdverseOracleConfidenceSpreadPercent(latestBasket(), '10000'))
      .toBeCloseTo(0.01, 8)
    expect(formatAdverseOracleConfidenceSpread(latestBasket(), '10000')).toBe('~0.0100%')
  })

  it('triples the displayed spread at 30000 bps', () => {
    expect(calculateAdverseOracleConfidenceSpreadPercent(latestBasket(), '30000'))
      .toBeCloseTo(0.03, 8)
    expect(formatAdverseConfidenceMultiplier('30000')).toBe('3x (30000 bps)')
    expect(formatAdverseOracleConfidenceSpread(latestBasket(), '30000')).toBe('~0.0300%')
  })

  it('returns unavailable for missing multiplier or invalid basket data', () => {
    expect(calculateAdverseOracleConfidenceSpreadPercent(latestBasket(), undefined)).toBeUndefined()
    expect(formatAdverseOracleConfidenceSpread(latestBasket({ basketPrice: '0' }), '10000')).toBeUndefined()
    expect(formatAdverseOracleConfidenceSpread(latestBasket({ components: [] }), '10000')).toBeUndefined()
  })
})
