import { describe, expect, it } from 'vitest'
import { calculatePendingCarryUsdc } from '../perpsCarry'

const snapshot = {
  unsettledCarryUsdc: 2_000_000n,
  borrowBaseUsdc: 100_000_000n,
  lastCarryIndex: 0n,
  sideCarryIndex: 0n,
  sideCarryTimestamp: 1_000n,
  sideBorrowBaseUsdc: 50_000_000n,
  poolAssetsUsdc: 100_000_000n,
  baseCarryBps: 500n,
  blockTimestamp: 1_000n + 31_536_000n,
}

describe('calculatePendingCarryUsdc', () => {
  it('adds unpaid carry to one year at 50% utilization and a 5% base rate', () => {
    expect(calculatePendingCarryUsdc(snapshot)).toBe(4_500_000n)
  })

  it.each([0n, 25_000_000n, 50_000_000n])('caps utilization at 100% with %s pool assets', (poolAssetsUsdc) => {
    expect(calculatePendingCarryUsdc({ ...snapshot, poolAssetsUsdc })).toBe(7_000_000n)
  })

  it.each([
    { borrowBaseUsdc: 0n },
    { sideBorrowBaseUsdc: 0n },
    { sideBorrowBaseUsdc: 0n, poolAssetsUsdc: 0n },
    { sideBorrowBaseUsdc: 1n }, // Utilization rounds down to zero.
    { baseCarryBps: 0n },
    { blockTimestamp: 1_000n },
    { blockTimestamp: 999n },
  ])('retains unpaid carry without new accrual for %o', (overrides) => {
    expect(calculatePendingCarryUsdc({ ...snapshot, ...overrides })).toBe(2_000_000n)
  })

  it('retains historical indexed carry even when the current rate is zero', () => {
    expect(calculatePendingCarryUsdc({
      ...snapshot, baseCarryBps: 0n, sideCarryIndex: 10n ** 16n,
    })).toBe(3_000_000n)
  })

  it('does not subtract unpaid carry when the current index is below the checkpoint', () => {
    expect(calculatePendingCarryUsdc({ ...snapshot, lastCarryIndex: 10n ** 18n })).toBe(2_000_000n)
  })

  it('preserves fractional basis points in the utilized rate', () => {
    expect(calculatePendingCarryUsdc({ ...snapshot, sideBorrowBaseUsdc: 15_460_000n }))
      .toBe(2_773_000n) // 0.773% APR, not 0.77%.
  })

  it('floors fractional USDC atoms instead of rounding up', () => {
    expect(calculatePendingCarryUsdc({ ...snapshot, borrowBaseUsdc: 79n })).toBe(2_000_001n)
  })

  it.each(Object.keys(snapshot) as (keyof typeof snapshot)[])('returns unavailable for missing %s', (key) => {
    expect(calculatePendingCarryUsdc({ ...snapshot, [key]: undefined })).toBeUndefined()
  })
})
