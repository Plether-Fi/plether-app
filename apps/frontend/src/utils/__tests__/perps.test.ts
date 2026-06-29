import { describe, expect, it } from 'vitest'
import {
  adverseConfidenceBasketPrice,
  confidenceAdjustedBasketPrice,
  displayDxyPriceToOraclePrice,
  formatDisplayDxyPrice,
  oraclePriceToDisplayDxyPrice,
  perpsOracleFreshnessFromTimestamp,
  PERPS_DXY_PRICE_CAP,
} from '../perps'

describe('DXY display price helpers', () => {
  it('reverses raw basket oracle price around the 2.0 cap', () => {
    expect(oraclePriceToDisplayDxyPrice(98_310_000n)).toBe(101_690_000n)
    expect(formatDisplayDxyPrice(98_310_000n)).toBe('1.0169')
  })

  it('keeps 1.0 unchanged at the midpoint', () => {
    expect(oraclePriceToDisplayDxyPrice(100_000_000n)).toBe(100_000_000n)
    expect(formatDisplayDxyPrice(100_000_000n)).toBe('1.0000')
  })

  it('treats zero and undefined as unavailable display values', () => {
    expect(oraclePriceToDisplayDxyPrice(0n)).toBeUndefined()
    expect(oraclePriceToDisplayDxyPrice(undefined)).toBeUndefined()
    expect(formatDisplayDxyPrice(0n)).toBe('--')
    expect(formatDisplayDxyPrice(undefined)).toBe('--')
  })

  it('can convert a display DXY price back to raw oracle price for future inputs', () => {
    expect(displayDxyPriceToOraclePrice(101_690_000n)).toBe(98_310_000n)
    expect(displayDxyPriceToOraclePrice(PERPS_DXY_PRICE_CAP)).toBe(0n)
  })
})

describe('oracle confidence adjustment helpers', () => {
  const revealedComponents = [
    {
      rawPrice: '114019',
      confidence: '10',
      exponent: -5,
      inverted: false,
      weightBps: 5760,
      basePrice: '117500000',
    },
    {
      rawPrice: '161820',
      confidence: '40',
      exponent: -3,
      inverted: true,
      weightBps: 1360,
      basePrice: '638000',
    },
    {
      rawPrice: '132134',
      confidence: '16',
      exponent: -5,
      inverted: false,
      weightBps: 1190,
      basePrice: '134480000',
    },
    {
      rawPrice: '141891',
      confidence: '69',
      exponent: -5,
      inverted: true,
      weightBps: 910,
      basePrice: '72880000',
    },
    {
      rawPrice: '971291',
      confidence: '408',
      exponent: -5,
      inverted: true,
      weightBps: 420,
      basePrice: '10860000',
    },
    {
      rawPrice: '80896',
      confidence: '33',
      exponent: -5,
      inverted: true,
      weightBps: 360,
      basePrice: '126100000',
    },
  ]

  it('computes the adverse upper basket bound from Pyth confidence values', () => {
    expect(confidenceAdjustedBasketPrice(revealedComponents, 'basketUp')).toBe(97_086_667n)
  })

  it('chooses the upper basket bound for a short open', () => {
    expect(adverseConfidenceBasketPrice({
      components: revealedComponents,
      direction: 'short',
      isClose: false,
    })).toBe(97_086_667n)
  })
})

describe('perps oracle freshness helper', () => {
  it('marks recent on-chain oracle updates as fresh', () => {
    expect(perpsOracleFreshnessFromTimestamp({
      publishTime: 1_000n,
      isChecking: false,
      nowSeconds: 1_030,
      freshSeconds: 60,
    })).toEqual({
      freshness: 'fresh',
      publishTime: 1_000,
    })
  })

  it('marks old on-chain oracle updates as stale', () => {
    expect(perpsOracleFreshnessFromTimestamp({
      publishTime: 1_000,
      isChecking: false,
      nowSeconds: 1_061,
      freshSeconds: 60,
    })).toEqual({
      freshness: 'stale',
      publishTime: 1_000,
    })
  })

  it('reports checking while the on-chain read has not resolved', () => {
    expect(perpsOracleFreshnessFromTimestamp({
      publishTime: undefined,
      isChecking: true,
      nowSeconds: 1_000,
    })).toEqual({
      freshness: 'checking',
    })
  })

  it('does not treat zero as a real unix publish time', () => {
    expect(perpsOracleFreshnessFromTimestamp({
      publishTime: 0n,
      isChecking: false,
      nowSeconds: 1_000,
    })).toEqual({})
  })
})
