import { describe, expect, it } from 'vitest'
import { calculatePerpsPoolCapacity, type PerpsPoolCapacityInputs } from './perpsPoolCapacity'

const ASSET = 10n ** 6n
const UNIT = 10n ** 18n
const PRICE = 10n ** 8n
const inputs: PerpsPoolCapacityInputs = {
  rawPrice: PRICE,
  capPrice: 2n * PRICE,
  poolAssets: 10_000n * ASSET,
  totalTraderClaimBalanceAssets: 0n,
  effectiveSolvencyAssets: 10_000n * ASSET,
  longOpenInterest: 1_000n * UNIT,
  shortOpenInterest: 1_000n * UNIT,
  longMaxProfitAssets: 1_000n * ASSET,
  shortMaxProfitAssets: 1_000n * ASSET,
  maxSkewRatio: 4n * 10n ** 17n,
  settlementBufferBps: 0n,
}

describe('calculatePerpsPoolCapacity', () => {
  it('allows long exposure within the larger existing short payout reserve', () => {
    const result = calculatePerpsPoolCapacity({
      ...inputs,
      rawPrice: 94_979_211n,
      poolAssets: 114_049_580_735_546n,
      effectiveSolvencyAssets: 114_049_580_735_546n,
      longOpenInterest: 115_767_900n * UNIT,
      shortOpenInterest: 108_276_900n * UNIT,
      longMaxProfitAssets: 110_153_929_844_291n,
      shortMaxProfitAssets: 113_674_807_953_677n,
      settlementBufferBps: 25n,
    })
    expect(result).toEqual({
      longSize: 3_802_100n * UNIT,
      shortSize: 86_000n * UNIT,
      longLimitingConstraint: 'backing',
      shortLimitingConstraint: 'backing',
    })
  })

  it('caps each side by directional headroom after accounting for existing imbalance', () => {
    expect(calculatePerpsPoolCapacity({ ...inputs, longOpenInterest: 2_000n * UNIT })).toEqual({
      longSize: 3_000n * UNIT,
      shortSize: 5_000n * UNIT,
      longLimitingConstraint: 'directional',
      shortLimitingConstraint: 'directional',
    })
  })

  it('deducts trader claims from both solvency backing and directional depth', () => {
    expect(calculatePerpsPoolCapacity({
      ...inputs,
      totalTraderClaimBalanceAssets: 5_000n * ASSET,
      effectiveSolvencyAssets: 5_000n * ASSET,
    })).toMatchObject({ longSize: 2_000n * UNIT, shortSize: 2_000n * UNIT })
    expect(calculatePerpsPoolCapacity({
      ...inputs,
      maxSkewRatio: 10n ** 18n,
      totalTraderClaimBalanceAssets: 5_000n * ASSET,
      effectiveSolvencyAssets: 5_000n * ASSET,
    })).toMatchObject({
      longSize: 4_000n * UNIT,
      shortSize: 4_000n * UNIT,
      longLimitingConstraint: 'backing',
      shortLimitingConstraint: 'backing',
    })
  })

  it('reserves the rounded-up liability buffer and rounds exposure down to 100-unit lots', () => {
    const result = calculatePerpsPoolCapacity({
      ...inputs,
      poolAssets: 1_111n * ASSET,
      effectiveSolvencyAssets: 1_111n * ASSET,
      maxSkewRatio: 10n ** 18n,
      settlementBufferBps: 100n,
    })
    expect(result?.longSize).toBe(100n * UNIT)
    // An extra 100 units would require 1,200 * 1.01 = 1,212 USDG.
    expect(result?.shortSize).toBe(100n * UNIT)
    expect(calculatePerpsPoolCapacity({
      ...inputs, poolAssets: 1_111n * ASSET - 1n, effectiveSolvencyAssets: 1_111n * ASSET - 1n,
      maxSkewRatio: 10n ** 18n, settlementBufferBps: 100n,
    })?.longSize).toBe(0n)
  })

  it('returns zero when existing obligations lack backing or claims exhaust the pool', () => {
    expect(calculatePerpsPoolCapacity({ ...inputs, effectiveSolvencyAssets: 999n * ASSET }))
      .toMatchObject({ longSize: 0n, shortSize: 0n })
    expect(calculatePerpsPoolCapacity({
      ...inputs, totalTraderClaimBalanceAssets: 10_001n * ASSET, effectiveSolvencyAssets: 0n,
    })).toMatchObject({ longSize: 0n, shortSize: 0n })
    expect(calculatePerpsPoolCapacity({
      ...inputs, poolAssets: 0n, effectiveSolvencyAssets: 0n,
      longMaxProfitAssets: 0n, shortMaxProfitAssets: 0n,
    })).toMatchObject({ longSize: 0n, shortSize: 0n })
  })

  it('keeps missing data and unusable prices distinct from zero capacity', () => {
    for (const key of Object.keys(inputs) as (keyof PerpsPoolCapacityInputs)[]) {
      expect(calculatePerpsPoolCapacity({ ...inputs, [key]: undefined })).toBeUndefined()
    }
    expect(calculatePerpsPoolCapacity({ ...inputs, rawPrice: 0n })).toBeUndefined()
    expect(calculatePerpsPoolCapacity({ ...inputs, capPrice: 0n })).toBeUndefined()
    expect(calculatePerpsPoolCapacity({ ...inputs, settlementBufferBps: -1n })).toBeUndefined()
  })

  it('clamps prices to the cap and handles zero additional short profit at that boundary', () => {
    expect(calculatePerpsPoolCapacity({ ...inputs, rawPrice: 3n * PRICE }))
      .toEqual(calculatePerpsPoolCapacity({ ...inputs, rawPrice: 2n * PRICE }))
    expect(calculatePerpsPoolCapacity({ ...inputs, rawPrice: 2n * PRICE }))
      .toMatchObject({ shortSize: 2_000n * UNIT, shortLimitingConstraint: 'directional' })
  })
})
