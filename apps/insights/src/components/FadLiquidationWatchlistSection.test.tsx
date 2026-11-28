import { fireEvent, render, screen } from '@testing-library/react'
import { MemoryRouter } from 'react-router-dom'
import { describe, expect, it } from 'vitest'
import type { MarketFadRiskResponse } from '../api/positions'
import { FadLiquidationWatchlistSection } from './FadLiquidationWatchlistSection'

const SAMPLE_FAD_RISK: MarketFadRiskResponse = {
  network: {
    chainId: 421614,
    releaseVersion: 'v2.0.0',
    settlementAssetSymbol: 'USDG',
  },
  observedAt: '2026-11-26T21:50:00Z',
  observedBlock: '326500120',
  markPrice: '100000000',
  accountsScanned: 18,
  calendar: {
    regime: 'live',
    fadWindow: false,
    oracleFrozen: false,
    isNewYorkDst: false,
    nextFadStartsAt: 1795816200,
    nextFadStartsAtUtc: '2026-11-27T21:50:00Z',
    nextFreezeStartsAt: 1795816800,
    nextFreezeStartsAtUtc: '2026-11-27T22:00:00Z',
    nextReopenAt: 1795989600,
    nextReopenAtUtc: '2026-11-29T22:00:00Z',
    currentFadEndsAt: null,
    currentFadEndsAtUtc: null,
    secondsUntilNextFad: 86400,
    secondsUntilNextFreeze: 87000,
    fadRunwaySeconds: 3600,
    scheduledClosures: [],
    upcomingWindows: [
      {
        kind: 'weekend',
        label: 'Weekly FX Weekend Freeze',
        fadStartsAt: 1795816200,
        fadStartsAtUtc: '2026-11-27T21:50:00Z',
        freezeStartsAt: 1795816800,
        freezeStartsAtUtc: '2026-11-27T22:00:00Z',
        freezeEndsAt: 1795989600,
        freezeEndsAtUtc: '2026-11-29T22:00:00Z',
        fadEndsAt: 1795989900,
        fadEndsAtUtc: '2026-11-29T22:05:00Z',
      },
    ],
  },
  thresholds: {
    maintenanceMarginBps: 10,
    fadMarginBps: 300,
    maxFadLeverageX100: 3333,
    baseCarryBps: 500,
    fadMaxStaleness: 259200,
    engineMarkStalenessLimit: 90,
  },
  summary: {
    totalOpenPositions: 3,
    liquidatableNowCount: 0,
    wouldLiquidateCount: 1,
    criticalCount: 1,
    atRiskCount: 0,
    safeCount: 1,
    endangeredCount: 2,
    immediateLiquidationNotionalAssets: '100000000000',
    immediateLiquidationLongNotionalAssets: '100000000000',
    immediateLiquidationShortNotionalAssets: '0',
    totalEndangeredNotionalAssets: '150000000000',
    endangeredLongNotionalAssets: '100000000000',
    endangeredShortNotionalAssets: '50000000000',
    totalMarginTopUpNeededAssets: '850000000',
  },
  positions: [
    {
      account: '0x1111111111111111111111111111111111111111',
      discordUsername: 'highlev_alice',
      side: 'long',
      size: '100000000000000000000000',
      notionalAssets: '100000000000',
      marginAssets: '2200000000',
      traderClaimAssets: '0',
      unrealizedPnlAssets: '0',
      currentEquityAssets: '2200000000',
      projectedCarryToFadAssets: '50000000',
      projectedEquityAtFadAssets: '2150000000',
      normalRequiredMarginAssets: '100000000',
      fadRequiredMarginAssets: '3000000000',
      fadMarginBufferAssets: '-850000000',
      marginNeededToSurviveAssets: '850000000',
      effectiveLeverageX100: 4545,
      entryPrice: '100000000',
      markPrice: '100000000',
      normalLiquidationPrice: '102090000',
      fadLiquidationPrice: '100000000',
      distanceToFadLiquidationBps: 0,
      liquidatableNow: false,
      dangerStatus: 'would_liquidate_at_fad',
    },
    {
      account: '0x2222222222222222222222222222222222222222',
      discordUsername: 'tight_bob',
      side: 'short',
      size: '50000000000000000000000',
      notionalAssets: '50000000000',
      marginAssets: '1650000000',
      traderClaimAssets: '0',
      unrealizedPnlAssets: '0',
      currentEquityAssets: '1650000000',
      projectedCarryToFadAssets: '10000000',
      projectedEquityAtFadAssets: '1640000000',
      normalRequiredMarginAssets: '50000000',
      fadRequiredMarginAssets: '1500000000',
      fadMarginBufferAssets: '140000000',
      marginNeededToSurviveAssets: '0',
      effectiveLeverageX100: 3030,
      entryPrice: '100000000',
      markPrice: '100000000',
      normalLiquidationPrice: '96800000',
      fadLiquidationPrice: '99710000',
      distanceToFadLiquidationBps: 29,
      liquidatableNow: false,
      dangerStatus: 'critical',
    },
    {
      account: '0x3333333333333333333333333333333333333333',
      discordUsername: 'safe_carol',
      side: 'long',
      size: '25000000000000000000000',
      notionalAssets: '25000000000',
      marginAssets: '5000000000',
      traderClaimAssets: '0',
      unrealizedPnlAssets: '0',
      currentEquityAssets: '5000000000',
      projectedCarryToFadAssets: '5000000',
      projectedEquityAtFadAssets: '4995000000',
      normalRequiredMarginAssets: '25000000',
      fadRequiredMarginAssets: '750000000',
      fadMarginBufferAssets: '4245000000',
      marginNeededToSurviveAssets: '0',
      effectiveLeverageX100: 500,
      entryPrice: '100000000',
      markPrice: '100000000',
      normalLiquidationPrice: '119800000',
      fadLiquidationPrice: '116500000',
      distanceToFadLiquidationBps: 1650,
      liquidatableNow: false,
      dangerStatus: 'safe',
    },
  ],
}

describe('FadLiquidationWatchlistSection', () => {
  it('displays FAD countdown, margin step-up, endangered rows, and interactive filters', () => {
    render(
      <MemoryRouter>
        <FadLiquidationWatchlistSection
          data={SAMPLE_FAD_RISK}
          isLoading={false}
          isError={false}
        />
      </MemoryRouter>,
    )

    expect(screen.getByText('Next FAD in 1d')).toBeInTheDocument()
    expect(screen.getByText(/0\.10% → 3\.00% · 33\.3× max lev/i)).toBeInTheDocument()
    expect(screen.getByText('@highlev_alice')).toBeInTheDocument()
    expect(screen.getByText('@tight_bob')).toBeInTheDocument()
    // Safe position hidden by default on "Endangered" tab
    expect(screen.queryByText('@safe_carol')).not.toBeInTheDocument()
    expect(screen.getByText(/Add ≥ 850\.00 USDG/i)).toBeInTheDocument()

    // Switch to "All Open" tab -> safe_carol appears
    fireEvent.click(screen.getByRole('tab', { name: /All Open \(3\)/i }))
    expect(screen.getByText('@safe_carol')).toBeInTheDocument()

    // Filter by handle search
    const searchInput = screen.getByRole('searchbox')
    fireEvent.change(searchInput, { target: { value: 'alice' } })
    expect(screen.getByText('@highlev_alice')).toBeInTheDocument()
    expect(screen.queryByText('@tight_bob')).not.toBeInTheDocument()
  })

  it('renders clear all-safe state when no open positions are endangered', () => {
    const allSafe: MarketFadRiskResponse = {
      ...SAMPLE_FAD_RISK,
      summary: {
        ...SAMPLE_FAD_RISK.summary,
        totalOpenPositions: 1,
        wouldLiquidateCount: 0,
        criticalCount: 0,
        safeCount: 1,
        endangeredCount: 0,
        immediateLiquidationNotionalAssets: '0',
        immediateLiquidationLongNotionalAssets: '0',
        immediateLiquidationShortNotionalAssets: '0',
        totalEndangeredNotionalAssets: '0',
        endangeredLongNotionalAssets: '0',
        endangeredShortNotionalAssets: '0',
        totalMarginTopUpNeededAssets: '0',
      },
      positions: [SAMPLE_FAD_RISK.positions[2]],
    }

    render(
      <MemoryRouter>
        <FadLiquidationWatchlistSection
          data={allSafe}
          isLoading={false}
          isError={false}
        />
      </MemoryRouter>,
    )

    expect(
      screen.getByText('All open positions clear the FAD margin requirement'),
    ).toBeInTheDocument()
  })
})
