import { describe, expect, it } from 'vitest'
import { calculatePerpsDirectionalLimit } from './perpsDirectionalLimit'

const MAX_SKEW_RATIO_40_PERCENT = 400_000_000_000_000_000n

describe('calculatePerpsDirectionalLimit', () => {
  it('normalizes the configured 40% protocol cap to 100% frontend usage', () => {
    expect(calculatePerpsDirectionalLimit({
      longOpenInterestUsdc: 600n,
      shortOpenInterestUsdc: 200n,
      poolAssetsUsdc: 1_000n,
      maxSkewRatio: MAX_SKEW_RATIO_40_PERCENT,
    })).toEqual({
      usagePercent: 100,
      side: 'long',
      netExposureUsdc: 400n,
      limitUsdc: 400n,
    })
  })

  it('reports long-heavy usage', () => {
    expect(calculatePerpsDirectionalLimit({
      longOpenInterestUsdc: 548n,
      shortOpenInterestUsdc: 200n,
      poolAssetsUsdc: 1_000n,
      maxSkewRatio: MAX_SKEW_RATIO_40_PERCENT,
    })).toEqual({
      usagePercent: 87,
      side: 'long',
      netExposureUsdc: 348n,
      limitUsdc: 400n,
    })
  })

  it('reports short-heavy and balanced markets', () => {
    expect(calculatePerpsDirectionalLimit({
      longOpenInterestUsdc: 200n,
      shortOpenInterestUsdc: 440n,
      poolAssetsUsdc: 1_000n,
      maxSkewRatio: MAX_SKEW_RATIO_40_PERCENT,
    })?.side).toBe('short')

    expect(calculatePerpsDirectionalLimit({
      longOpenInterestUsdc: 200n,
      shortOpenInterestUsdc: 200n,
      poolAssetsUsdc: 1_000n,
      maxSkewRatio: MAX_SKEW_RATIO_40_PERCENT,
    })).toMatchObject({
      usagePercent: 0,
      side: 'balanced',
      netExposureUsdc: 0n,
    })
  })

  it('returns undefined until every input and a positive limit are available', () => {
    expect(calculatePerpsDirectionalLimit({
      longOpenInterestUsdc: undefined,
      shortOpenInterestUsdc: 200n,
      poolAssetsUsdc: 1_000n,
      maxSkewRatio: MAX_SKEW_RATIO_40_PERCENT,
    })).toBeUndefined()

    expect(calculatePerpsDirectionalLimit({
      longOpenInterestUsdc: 600n,
      shortOpenInterestUsdc: 200n,
      poolAssetsUsdc: 1_000n,
      maxSkewRatio: 0n,
    })).toBeUndefined()
  })
})
