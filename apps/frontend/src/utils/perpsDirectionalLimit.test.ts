import { describe, expect, it } from 'vitest'
import { calculatePerpsDirectionalLimit } from './perpsDirectionalLimit'

const MAX_SKEW_RATIO_40_PERCENT = 400_000_000_000_000_000n

describe('calculatePerpsDirectionalLimit', () => {
  it('normalizes the configured 40% protocol cap to 100% frontend usage', () => {
    expect(calculatePerpsDirectionalLimit({
      longOpenInterestAssets: 600n,
      shortOpenInterestAssets: 200n,
      poolAssets: 1_000n,
      maxSkewRatio: MAX_SKEW_RATIO_40_PERCENT,
    })).toEqual({
      usagePercent: 100,
      side: 'long',
      netExposureAssets: 400n,
      limitAssets: 400n,
    })
  })

  it('reports long-heavy usage', () => {
    expect(calculatePerpsDirectionalLimit({
      longOpenInterestAssets: 548n,
      shortOpenInterestAssets: 200n,
      poolAssets: 1_000n,
      maxSkewRatio: MAX_SKEW_RATIO_40_PERCENT,
    })).toEqual({
      usagePercent: 87,
      side: 'long',
      netExposureAssets: 348n,
      limitAssets: 400n,
    })
  })

  it('reports short-heavy and balanced markets', () => {
    expect(calculatePerpsDirectionalLimit({
      longOpenInterestAssets: 200n,
      shortOpenInterestAssets: 440n,
      poolAssets: 1_000n,
      maxSkewRatio: MAX_SKEW_RATIO_40_PERCENT,
    })?.side).toBe('short')

    expect(calculatePerpsDirectionalLimit({
      longOpenInterestAssets: 200n,
      shortOpenInterestAssets: 200n,
      poolAssets: 1_000n,
      maxSkewRatio: MAX_SKEW_RATIO_40_PERCENT,
    })).toMatchObject({
      usagePercent: 0,
      side: 'balanced',
      netExposureAssets: 0n,
    })
  })

  it('returns undefined until every input and a positive limit are available', () => {
    expect(calculatePerpsDirectionalLimit({
      longOpenInterestAssets: undefined,
      shortOpenInterestAssets: 200n,
      poolAssets: 1_000n,
      maxSkewRatio: MAX_SKEW_RATIO_40_PERCENT,
    })).toBeUndefined()

    expect(calculatePerpsDirectionalLimit({
      longOpenInterestAssets: 600n,
      shortOpenInterestAssets: 200n,
      poolAssets: 1_000n,
      maxSkewRatio: 0n,
    })).toBeUndefined()
  })
})
