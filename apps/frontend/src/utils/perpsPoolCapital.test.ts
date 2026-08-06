import { describe, expect, it } from 'vitest'
import { calculatePerpsPoolCapital } from './perpsPoolCapital'

describe('calculatePerpsPoolCapital', () => {
  it('calculates the Junior and Senior principal shares', () => {
    expect(calculatePerpsPoolCapital({
      juniorPrincipalUsdc: 3_200_000n,
      seniorPrincipalUsdc: 6_800_000n,
      seniorHighWaterMarkUsdc: 6_800_000n,
    })).toEqual({
      isEmpty: false,
      isJuniorExhausted: false,
      juniorSharePercent: 32,
      seniorSharePercent: 68,
      seniorStatus: 'at-high-water-mark',
      seniorImpairmentUsdc: 0n,
    })
  })

  it('reports exhausted Junior capital without inventing an impairment', () => {
    expect(calculatePerpsPoolCapital({
      juniorPrincipalUsdc: 0n,
      seniorPrincipalUsdc: 6_800_000n,
      seniorHighWaterMarkUsdc: 6_800_000n,
    })).toMatchObject({
      isEmpty: false,
      isJuniorExhausted: true,
      juniorSharePercent: 0,
      seniorSharePercent: 100,
      seniorStatus: 'at-high-water-mark',
      seniorImpairmentUsdc: 0n,
    })
  })

  it('measures Senior impairment against the high-water mark', () => {
    expect(calculatePerpsPoolCapital({
      juniorPrincipalUsdc: 0n,
      seniorPrincipalUsdc: 5_900_000n,
      seniorHighWaterMarkUsdc: 6_800_000n,
    })).toMatchObject({
      isJuniorExhausted: true,
      seniorStatus: 'impaired',
      seniorImpairmentUsdc: 900_000n,
    })
  })

  it('keeps an empty pool distinct from missing contract data', () => {
    expect(calculatePerpsPoolCapital({
      juniorPrincipalUsdc: 0n,
      seniorPrincipalUsdc: 0n,
      seniorHighWaterMarkUsdc: 0n,
    })).toMatchObject({
      isEmpty: true,
      juniorSharePercent: 0,
      seniorSharePercent: 0,
    })

    expect(calculatePerpsPoolCapital({
      juniorPrincipalUsdc: undefined,
      seniorPrincipalUsdc: 0n,
      seniorHighWaterMarkUsdc: 0n,
    })).toBeUndefined()
  })
})
