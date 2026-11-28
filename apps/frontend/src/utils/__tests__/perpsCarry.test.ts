import { describe, expect, it } from 'vitest'
import { calculatePendingCarryAssets } from '../perpsCarry'

const snapshot = {
  unsettledCarryAssets: 2_000_000n,
  maxProfitAssets: 100_000_000n,
  lastCarryIndex: 0n,
  currentCarryIndex: 50_000_000_000_000_000n, // 5% annual index growth (0.05e18)
}

describe('calculatePendingCarryAssets', () => {
  it('adds unpaid carry to one year of flat 5% carry on maxProfitAssets', () => {
    expect(calculatePendingCarryAssets(snapshot)).toBe(7_000_000n)
  })

  it.each([
    { maxProfitAssets: 0n },
    { currentCarryIndex: 0n },
    { lastCarryIndex: 50_000_000_000_000_000n },
  ])('retains unpaid carry without new accrual for %o', (overrides) => {
    expect(calculatePendingCarryAssets({ ...snapshot, ...overrides })).toBe(2_000_000n)
  })

  it('does not subtract unpaid carry when the current index is below the checkpoint', () => {
    expect(calculatePendingCarryAssets({ ...snapshot, lastCarryIndex: 10n ** 18n })).toBe(2_000_000n)
  })

  it('accrues from the delta between currentCarryIndex and lastCarryIndex', () => {
    expect(calculatePendingCarryAssets({
      ...snapshot, lastCarryIndex: 10_000_000_000_000_000n, currentCarryIndex: 35_000_000_000_000_000n,
    })).toBe(4_500_000n)
  })

  it('floors fractional USDG atoms instead of rounding up', () => {
    expect(calculatePendingCarryAssets({ ...snapshot, maxProfitAssets: 39n })).toBe(2_000_001n)
  })

  it.each(Object.keys(snapshot) as (keyof typeof snapshot)[])('returns unavailable for missing %s', (key) => {
    expect(calculatePendingCarryAssets({ ...snapshot, [key]: undefined })).toBeUndefined()
  })
})
