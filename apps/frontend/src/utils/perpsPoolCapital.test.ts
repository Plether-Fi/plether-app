import { describe, expect, it } from 'vitest'
import { calculatePerpsPoolCapital } from './perpsPoolCapital'

describe('calculatePerpsPoolCapital', () => {
  it('calculates the Junior and Senior principal shares', () => {
    expect(calculatePerpsPoolCapital({
      juniorPrincipalAssets: 3_200_000n,
      seniorPrincipalAssets: 6_800_000n,
      seniorHighWaterMarkAssets: 6_800_000n,
    })).toEqual({
      isEmpty: false,
      isJuniorExhausted: false,
      juniorSharePercent: 32,
      seniorSharePercent: 68,
      seniorStatus: 'at-high-water-mark',
      seniorImpairmentAssets: 0n,
    })
  })

  it('reports exhausted Junior capital without inventing an impairment', () => {
    expect(calculatePerpsPoolCapital({
      juniorPrincipalAssets: 0n,
      seniorPrincipalAssets: 6_800_000n,
      seniorHighWaterMarkAssets: 6_800_000n,
    })).toMatchObject({
      isEmpty: false,
      isJuniorExhausted: true,
      juniorSharePercent: 0,
      seniorSharePercent: 100,
      seniorStatus: 'at-high-water-mark',
      seniorImpairmentAssets: 0n,
    })
  })

  it('measures Senior impairment against the high-water mark', () => {
    expect(calculatePerpsPoolCapital({
      juniorPrincipalAssets: 0n,
      seniorPrincipalAssets: 5_900_000n,
      seniorHighWaterMarkAssets: 6_800_000n,
    })).toMatchObject({
      isJuniorExhausted: true,
      seniorStatus: 'impaired',
      seniorImpairmentAssets: 900_000n,
    })
  })

  it('keeps an empty pool distinct from missing contract data', () => {
    expect(calculatePerpsPoolCapital({
      juniorPrincipalAssets: 0n,
      seniorPrincipalAssets: 0n,
      seniorHighWaterMarkAssets: 0n,
    })).toMatchObject({
      isEmpty: true,
      juniorSharePercent: 0,
      seniorSharePercent: 0,
    })

    expect(calculatePerpsPoolCapital({
      juniorPrincipalAssets: undefined,
      seniorPrincipalAssets: 0n,
      seniorHighWaterMarkAssets: 0n,
    })).toBeUndefined()
  })
})
