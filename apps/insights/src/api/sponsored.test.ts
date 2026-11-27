import { describe, expect, it } from 'vitest'
import {
  altoFundingPill,
  budgetGaugeTone,
  deriveSponsoredBanner,
  formatBpsPercent,
  formatDurationSeconds,
  formatSponsoredEthWei,
  formatRunwayHours,
  type SponsoredStatusResponse,
} from './sponsored'

function baseSponsoredStatus(
  overrides: Partial<SponsoredStatusResponse> = {},
): SponsoredStatusResponse {
  return {
    network: { chainId: 421614, releaseVersion: 'v2.0.0', settlementAssetSymbol: 'USDG' },
    observedAt: '2026-11-26T18:30:00Z',
    configured: true,
    stack: 'native',
    entryPointVersion: '0.8',
    entryPointAddress: '0x4337084d9e255ff0702461cf8895ce9e3b5ff108',
    paymasterAddress: '0x1111111111111111111111111111111111111111',
    gate: {
      status: 'issuing',
      sponsorshipEnabled: true,
      submissionEnabled: true,
      rolloutMode: 'global',
      issuancePaused: false,
      pausedReason: null,
      paymasterOnchainPaused: false,
      updatedAt: '2026-11-26T18:00:00Z',
    },
    paymaster: {
      entryPointDepositWei: '250000000000000000',
      minDepositWei: '50000000000000000',
      freeHeadroomWei: '248000000000000000',
      depositHealthy: true,
      burn24hWei: '12000000000000000',
      estimatedRunwayHours: 500,
      altoFundingState: 'ready',
      altoFundingReason: 'READY',
    },
    budgets: {
      maxCostPerOpWei: '5000000000000000',
      accountHourlyCapWei: '25000000000000000',
      globalOutstanding: { usedWei: '2000000000000000', capWei: '50000000000000000', utilizationBps: 400 },
      globalHourly: { usedWei: '3500000000000000', capWei: '100000000000000000', utilizationBps: 350 },
      globalDaily: { usedWei: '12000000000000000', capWei: '500000000000000000', utilizationBps: 240 },
    },
    reconciler: {
      safeBlock: '332900400',
      headBlock: '332900420',
      blocksBehind: 20,
      lastSuccessAt: '2026-11-26T18:29:55Z',
      lagSeconds: 5,
      maxSafeLagSeconds: 600,
      fresh: true,
    },
    bundler: {
      reachable: true,
      entryPointSupported: true,
      status: 'ready',
    },
    execution: {
      window1h: { totalOps: 14, succeededOps: 14, revertedOps: 0, successRateBps: 10000, gasSpentWei: '1400000000000000', gasEfficiencyBps: 3800 },
      window24h: { totalOps: 120, succeededOps: 118, revertedOps: 2, successRateBps: 9833, gasSpentWei: '12000000000000000', gasEfficiencyBps: 4100 },
      window7d: { totalOps: 810, succeededOps: 798, revertedOps: 12, successRateBps: 9851, gasSpentWei: '81000000000000000', gasEfficiencyBps: 4050 },
      latencyP50Seconds: 2,
      latencyP95Seconds: 6,
      funnel24h: { reserved: 1, signed: 0, submitted: 1, settled: 118, expired: 3, cancelled: 1 },
      activeUnfinalizedCount: 2,
      oldestUnfinalizedAgeSeconds: 18,
    },
    closeAssistance: {
      available: true,
      verifiedCount24h: 4,
      verifiedCount7d: 19,
      totalAssisted7dAssets: '3800000',
      pendingUnverifiedCount: 0,
      verificationRateBps: 10000,
    },
    ...overrides,
  }
}

describe('sponsored API formatters and health derivations', () => {
  it('formats wei amounts cleanly without losing small per-op precision or using $', () => {
    expect(formatSponsoredEthWei('250000000000000000', 4)).toBe('0.2500 ETH')
    expect(formatSponsoredEthWei('95000000000000', 6)).toBe('0.000095 ETH')
    expect(formatSponsoredEthWei('10000000000', 4)).toBe('< 0.0001 ETH')
    expect(formatSponsoredEthWei('0', 4)).toBe('0.0000 ETH')
    expect(formatSponsoredEthWei(null)).toBe('Unavailable')
    expect(formatSponsoredEthWei('bad')).toBe('Unavailable')
  })

  it('formats runway hours, bps percentages, and durations accurately', () => {
    expect(formatRunwayHours(null)).toBe('No 24h burn')
    expect(formatRunwayHours(14)).toBe('14h')
    expect(formatRunwayHours(50)).toBe('2d 2h')
    expect(formatRunwayHours(500)).toBe('20d')
    expect(formatBpsPercent(9833)).toBe('98.33%')
    expect(formatBpsPercent(null)).toBe('—')
    expect(formatDurationSeconds(5)).toBe('5s')
    expect(formatDurationSeconds(125)).toBe('2m 5s')
    expect(formatDurationSeconds(3660)).toBe('1h 1m')
    expect(formatDurationSeconds(null)).toBe('Unavailable')
  })

  it('maps budget gauge utilization and Alto funding states to status tones', () => {
    expect(budgetGaugeTone(1500)).toBe('live')
    expect(budgetGaugeTone(6500)).toBe('degraded')
    expect(budgetGaugeTone(9000)).toBe('blocked')
    expect(budgetGaugeTone(null)).toBe('unknown')

    expect(altoFundingPill('ready', 'READY')).toEqual({ tone: 'live', label: 'Ready' })
    expect(altoFundingPill('ready', 'FUNDING_LOW')).toEqual({ tone: 'degraded', label: 'Funding low' })
    expect(altoFundingPill('blocked', 'WORKER_INSUFFICIENT_FUNDS')).toEqual({
      tone: 'blocked',
      label: 'Insufficient ETH',
    })
    expect(altoFundingPill('unknown', 'FUNDING_UNVERIFIED')).toEqual({
      tone: 'unknown',
      label: 'Unverified',
    })
  })

  it('derives fail-closed sponsorship banners across live, paused, low deposit, degraded, and unconfigured states', () => {
    expect(deriveSponsoredBanner(baseSponsoredStatus())).toMatchObject({
      tone: 'live',
      label: 'Issuing live',
    })

    expect(
      deriveSponsoredBanner(
        baseSponsoredStatus({
          gate: { ...baseSponsoredStatus().gate, status: 'paused', pausedReason: 'Paymaster EntryPoint deposit below minimum floor' },
        }),
      ),
    ).toMatchObject({ tone: 'blocked', label: 'Sponsorship paused' })

    expect(
      deriveSponsoredBanner(
        baseSponsoredStatus({
          paymaster: { ...baseSponsoredStatus().paymaster, depositHealthy: false },
        }),
      ),
    ).toMatchObject({ tone: 'blocked', label: 'Low paymaster deposit' })

    expect(
      deriveSponsoredBanner(
        baseSponsoredStatus({
          reconciler: { ...baseSponsoredStatus().reconciler, fresh: false },
        }),
      ),
    ).toMatchObject({ tone: 'degraded', label: 'Sponsorship degraded' })

    expect(deriveSponsoredBanner(baseSponsoredStatus({ configured: false }))).toMatchObject({
      tone: 'unknown',
      label: 'Sponsorship not configured',
    })
  })
})
