import { describe, expect, it } from 'vitest'
import { calculatePendingCarryAssets } from '../perpsCarry'

const snapshot = {
  unsettledCarryAssets: 2_000_000n,
  borrowBaseAssets: 100_000_000n,
  lastCarryIndex: 0n,
  sideCarryIndex: 0n,
  sideCarryTimestamp: 1_000n,
  sideBorrowBaseAssets: 50_000_000n,
  poolAssets: 100_000_000n,
  baseCarryBps: 500n,
  blockTimestamp: 1_000n + 31_536_000n,
}

describe('calculatePendingCarryAssets', () => {
  it('adds unpaid carry to one year at 50% utilization and a 5% base rate', () => {
    expect(calculatePendingCarryAssets(snapshot)).toBe(4_500_000n)
  })

  it.each([0n, 25_000_000n, 50_000_000n])('caps utilization at 100% with %s pool assets', (poolAssets) => {
    expect(calculatePendingCarryAssets({ ...snapshot, poolAssets })).toBe(7_000_000n)
  })

  it.each([
    { borrowBaseAssets: 0n },
    { sideBorrowBaseAssets: 0n },
    { sideBorrowBaseAssets: 0n, poolAssets: 0n },
    { sideBorrowBaseAssets: 1n }, // Utilization rounds down to zero.
    { baseCarryBps: 0n },
    { blockTimestamp: 1_000n },
    { blockTimestamp: 999n },
  ])('retains unpaid carry without new accrual for %o', (overrides) => {
    expect(calculatePendingCarryAssets({ ...snapshot, ...overrides })).toBe(2_000_000n)
  })

  it('retains historical indexed carry even when the current rate is zero', () => {
    expect(calculatePendingCarryAssets({
      ...snapshot, baseCarryBps: 0n, sideCarryIndex: 10n ** 16n,
    })).toBe(3_000_000n)
  })

  it('does not subtract unpaid carry when the current index is below the checkpoint', () => {
    expect(calculatePendingCarryAssets({ ...snapshot, lastCarryIndex: 10n ** 18n })).toBe(2_000_000n)
  })

  it('preserves fractional basis points in the utilized rate', () => {
    expect(calculatePendingCarryAssets({ ...snapshot, sideBorrowBaseAssets: 15_460_000n }))
      .toBe(2_773_000n) // 0.773% APR, not 0.77%.
  })

  it('floors fractional USDG atoms instead of rounding up', () => {
    expect(calculatePendingCarryAssets({ ...snapshot, borrowBaseAssets: 79n })).toBe(2_000_001n)
  })

  it.each(Object.keys(snapshot) as (keyof typeof snapshot)[])('returns unavailable for missing %s', (key) => {
    expect(calculatePendingCarryAssets({ ...snapshot, [key]: undefined })).toBeUndefined()
  })
})
