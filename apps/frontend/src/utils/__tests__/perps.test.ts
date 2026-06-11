import { describe, expect, it } from 'vitest'
import {
  displayDxyPriceToOraclePrice,
  formatDisplayDxyPrice,
  oraclePriceToDisplayDxyPrice,
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
