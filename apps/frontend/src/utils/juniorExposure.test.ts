import { describe, expect, it } from 'vitest'
import { calculateJuniorExposure } from './juniorExposure'

const assets = (value: number) => BigInt(value) * 10n ** 6n
const size = (value: number) => BigInt(value) * 10n ** 18n
const pool = {
  totalAssets: assets(1000), freeAssets: assets(400),
  seniorPrincipalAssets: assets(800), juniorPrincipalAssets: assets(200),
  seniorRateBps: 800n, markPrice: 80_000_000n,
  longOpenInterest: size(600), shortOpenInterest: size(100),
}

describe('calculateJuniorExposure', () => {
  it('uses the displayed index and isolates directional sensitivity from capital concentration', () => {
    expect(calculateJuniorExposure(pool)).toMatchObject({
      long: assets(720), short: assets(120), net: assets(600),
      juniorShare: 0.2, capitalMultiple: 5, grossExposureMultiple: 0.84,
      juniorLossPercentForOnePercentRise: 3, unavailableCashShare: 0.6,
      couponDragPercent: 32,
    })
  })

  it('reverses sensitivity for net SHORT and cancels balanced directional exposure', () => {
    expect(calculateJuniorExposure({ ...pool, longOpenInterest: size(100), shortOpenInterest: size(600) })
      .juniorLossPercentForOnePercentRise).toBe(-3)
    expect(calculateJuniorExposure({ ...pool, shortOpenInterest: size(600) })
      .juniorLossPercentForOnePercentRise).toBe(0)
  })

  it('increases sensitivity and coupon cost when the same pool has less Junior backing', () => {
    expect(calculateJuniorExposure({ ...pool, juniorPrincipalAssets: assets(100), seniorPrincipalAssets: assets(900) }))
      .toMatchObject({ capitalMultiple: 10, juniorLossPercentForOnePercentRise: 6, couponDragPercent: 72 })
  })

  it('keeps missing inputs and exhausted capital unavailable instead of reporting zero risk', () => {
    expect(calculateJuniorExposure({})).toMatchObject({
      net: undefined, capitalMultiple: undefined, couponDragPercent: undefined,
      unavailableCashShare: undefined, juniorLossPercentForOnePercentRise: undefined,
    })
    expect(calculateJuniorExposure({ ...pool, juniorPrincipalAssets: 0n })).toMatchObject({
      juniorShare: 0, capitalMultiple: undefined, couponDragPercent: undefined,
      juniorLossPercentForOnePercentRise: undefined,
    })
    expect(calculateJuniorExposure({ ...pool, totalAssets: 0n }).unavailableCashShare).toBeUndefined()
    expect(calculateJuniorExposure({ ...pool, markPrice: 200_000_000n })).toMatchObject({
      netPositionSize: size(500), juniorLossPercentForOnePercentRise: undefined,
    })
  })
})
