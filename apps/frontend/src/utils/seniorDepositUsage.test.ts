import { describe, expect, it } from 'vitest'
import { calculateSeniorDepositUsage } from './seniorDepositUsage'

const state = {
  pendingSeniorPrincipalAssets: 70_000_000n,
  pendingJuniorPrincipalAssets: 50_000_000n,
  seniorHighWaterMarkAssets: 72_000_000n,
  reservedSeniorDepositAssets: 5_000_000n,
  maxSeniorExposureAssets: 100_000_000n,
  maxSeniorShareBps: 7_500n,
}

describe('calculateSeniorDepositUsage', () => {
  it('includes the protected high-water mark and pending reservations in both limits', () => {
    expect(calculateSeniorDepositUsage(state)).toEqual({
      protectedExposureAssets: 72_000_000n,
      committedExposureAssets: 77_000_000n,
      absoluteUsagePercent: 77,
      absoluteLimitExceeded: false,
      shareUsagePercent: 80.84,
      shareLimitExceeded: false,
    })
  })

  it('uses projected capital when it exceeds the stored protected balance', () => {
    expect(calculateSeniorDepositUsage({ ...state, pendingSeniorPrincipalAssets: 80_000_000n }))
      .toMatchObject({ protectedExposureAssets: 80_000_000n, committedExposureAssets: 85_000_000n })
  })

  it('detects a share limit breach even when the absolute cap has room', () => {
    expect(calculateSeniorDepositUsage({ ...state, pendingJuniorPrincipalAssets: 20_000_000n }))
      .toMatchObject({ absoluteUsagePercent: 77, shareUsagePercent: 105.84, shareLimitExceeded: true })
    // Comparing rounded percentages would hide this small breach.
    expect(calculateSeniorDepositUsage({ ...state,
      pendingSeniorPrincipalAssets: 75_000_001n, seniorHighWaterMarkAssets: 0n,
      reservedSeniorDepositAssets: 0n, pendingJuniorPrincipalAssets: 25_000_000n,
    })).toMatchObject({ shareUsagePercent: 100, shareLimitExceeded: true })
  })

  it('preserves over-limit usage and handles zero and unlimited caps', () => {
    expect(calculateSeniorDepositUsage({ ...state, maxSeniorExposureAssets: 50_000_000n }))
      .toMatchObject({ absoluteUsagePercent: 154, absoluteLimitExceeded: true })
    expect(calculateSeniorDepositUsage({ ...state, maxSeniorExposureAssets: 0n }))
      .toMatchObject({ absoluteUsagePercent: undefined, absoluteLimitExceeded: true })
    expect(calculateSeniorDepositUsage({ ...state, maxSeniorExposureAssets: 2n ** 256n - 1n }))
      .toMatchObject({ absoluteUsagePercent: undefined, absoluteLimitExceeded: false })
  })

  it('distinguishes an empty pool from missing live data', () => {
    expect(calculateSeniorDepositUsage({ ...state, pendingSeniorPrincipalAssets: 0n,
      pendingJuniorPrincipalAssets: 0n, seniorHighWaterMarkAssets: 0n, reservedSeniorDepositAssets: 0n,
    })).toMatchObject({ committedExposureAssets: 0n, absoluteUsagePercent: 0, shareUsagePercent: 0 })
    expect(calculateSeniorDepositUsage({ ...state, pendingSeniorPrincipalAssets: undefined })).toBeUndefined()
    expect(calculateSeniorDepositUsage({ ...state, pendingJuniorPrincipalAssets: undefined }))
      .toMatchObject({ absoluteUsagePercent: 77, shareUsagePercent: undefined })
  })

  it('normalizes Senior share against its cap and handles disabled share limits', () => {
    const current = { ...state, seniorHighWaterMarkAssets: 70_000_000n }
    // A 60% committed Senior share uses 80% of the 75% share cap.
    expect(calculateSeniorDepositUsage(current)).toMatchObject({ shareUsagePercent: 80 })
    expect(calculateSeniorDepositUsage({ ...current, maxSeniorShareBps: 0n }))
      .toMatchObject({ shareUsagePercent: undefined, shareLimitExceeded: true })
    expect(calculateSeniorDepositUsage({ ...current, maxSeniorShareBps: 10_000n }))
      .toMatchObject({ shareUsagePercent: undefined, shareLimitExceeded: false })
  })
})
