import { describe, expect, it } from 'vitest'
import { calculatePriceRisk, findLiquidationThreshold, formatLiquidationPrice, freeSettlementAfterCarry, type PerpsPriceRiskInputs } from '../perpsRisk'
import fixtures from './fixtures/perpsRisk.v1.2.2.json'

describe('canonical v1.2.2 price risk', () => {
  for (const fixture of fixtures.cases) {
    it(`matches Solidity: ${fixture.name}`, () => {
      const input = Object.fromEntries(Object.entries(fixture.input).map(([key, value]) => [key, key === 'side' ? value : BigInt(value)])) as unknown as PerpsPriceRiskInputs
      const result = findLiquidationThreshold(input)
      expect(result).toEqual(fixture.expected.status === 'boundary'
        ? { status: 'boundary', price: BigInt(fixture.expected.price!) }
        : { status: 'out-of-range' })
      if (result.status === 'boundary') {
        expect(calculatePriceRisk(input, result.price).liquidatable).toBe(true)
        const healthyTick = result.price + (input.side === 0 ? -1n : 1n)
        if (healthyTick >= 0n && healthyTick <= input.capPrice) expect(calculatePriceRisk(input, healthyTick).liquidatable).toBe(false)
      }
    })
  }

  it('keeps missing input distinct from a proven absent boundary', () => {
    expect(findLiquidationThreshold(undefined)).toEqual({ status: 'unavailable' })
  })
  it('formats valid endpoint thresholds and exact precision', () => {
    expect(formatLiquidationPrice(0n, 200_000_000n)).toBe('2.0000')
    expect(formatLiquidationPrice(200_000_000n, 200_000_000n)).toBe('0.0000')
    expect(formatLiquidationPrice(102_397_603n, 200_000_000n, 8)).toBe('0.97602397')
    expect(formatLiquidationPrice(97_597_597n, 200_000_000n, 8)).toBe('1.02402403')
  })
  it('reserves carry before offering free funds for margin additions', () => {
    expect(freeSettlementAfterCarry(750_000_000n, 20_000_000n)).toBe(730_000_000n)
    expect(freeSettlementAfterCarry(1n, 2n)).toBe(0n)
    expect(freeSettlementAfterCarry(2n, undefined)).toBeUndefined()
  })
})
