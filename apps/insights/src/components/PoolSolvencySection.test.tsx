import { render, screen } from '@testing-library/react'
import { describe, expect, it } from 'vitest'
import type { LiquiditySolvencyResponse } from '../api/liquidity'
import { PoolSolvencySection } from './PoolSolvencySection'

const HEALTHY_SOLVENCY: LiquiditySolvencyResponse = {
  network: {
    chainId: 421614,
    releaseVersion: 'v2.0.0',
    settlementAssetSymbol: 'USDG',
  },
  observedAt: '2026-11-28T14:00:00Z',
  observedBlock: '326500000',
  solvency: {
    poolTotalAssets: '1000000000000',
    poolRawAssets: '1000000000000',
    excessAssets: '120000000000',
    terminalDeficitAssets: '0',
    freeSettlementAssets: '740000000000',
    withdrawalReserveAssets: '260000000000',
    effectiveSolvencyAssets: '955000000000',
    solvencyCoverageBps: 45455,
    freeLiquidityShareBps: 7400,
    maxDirectionalLiabilityAssets: '220000000000',
    totalTraderClaimsAssets: '45000000000',
    terminalLpPriceDeltaAssets: '18500000000',
    markPrice: '101250000',
    markTime: 1795874400,
    bookVersion: 12,
    hasOpenPositions: true,
    degradedMode: false,
    seniorImpaired: false,
    canIncreaseRisk: true,
    canAcceptOrdinaryDeposits: true,
    canSettleDepositEntries: true,
    seniorReservationsWithinLimits: true,
  },
  waterfall: {
    seniorPrincipalAssets: '650000000000',
    juniorPrincipalAssets: '350000000000',
    seniorHighWaterMarkAssets: '650000000000',
    seniorHwmCoverageBps: 10000,
    seniorImpairmentAssets: '0',
    protectedSeniorExposureAssets: '650000000000',
    maxSeniorWithdrawAssets: '520000000000',
    maxJuniorWithdrawAssets: '220000000000',
    seniorDepositCapacityAssets: '400000000000',
    reservedSeniorDepositAssets: '25000000000',
    unassignedAssets: '0',
    pendingRecapitalizationAssets: '0',
    pendingTradingRevenueAssets: '14200000000',
    positiveVpiAssets: '9500000000',
    negativeVpiAssets: '3100000000',
    netVpiAccruedAssets: '6400000000',
    carryIndex: '1004200000000000000',
    carryTimestamp: 1795874400,
  },
  policy: {
    seniorRateBps: 800,
    seniorFrozenLpFeeBps: 50,
    juniorFrozenLpFeeBps: 150,
    poolConfigActivationTime: 0,
    pendingPoolConfigTimelockActive: false,
    secondsUntilPoolConfigActivation: null,
  },
  vaultEscrows: {
    senior: {
      pendingDepositAssets: '25000000000',
      pendingRedeemShares: '0',
      depositClaimShares: '10000000000000',
      withdrawalEscrowAssets: '0',
    },
    junior: {
      pendingDepositAssets: '5000000000',
      pendingRedeemShares: '0',
      depositClaimShares: '0',
      withdrawalEscrowAssets: '0',
    },
  },
  custodyAudit: {
    healthState: 'healthy',
    healthStateCode: 1,
    criticalFaults: { raw: '0', activeBits: [], labels: [] },
    dependencyFailures: { raw: '0', activeBits: [], labels: [] },
    custodyBalanced: true,
    seniorEscrowBalanced: true,
    juniorEscrowBalanced: true,
    navBookCapMatchesEngine: true,
    pool: {
      rawAssets: '1000000000000',
      accountedAssets: '1000000000000',
      custodyDeficitAssets: '0',
      custodySurplusAssets: '0',
    },
    escrowAudit: {
      senior: {
        requiredAssetEscrow: '25000000000',
        actualAssetEscrow: '25000000000',
        requiredShareEscrow: '10000000000000',
        actualShareEscrow: '10000000000000',
      },
      junior: {
        requiredAssetEscrow: '5000000000',
        actualAssetEscrow: '5000000000',
        requiredShareEscrow: '0',
        actualShareEscrow: '0',
      },
    },
    navBookParity: {
      bookCapPrice: '200000000',
      engineCapPrice: '200000000',
      bookActiveCurveCount: 4,
      bookTotalLots: '5000000',
      bookTotalEntryCostAssetAtoms: '500000000000',
      bookTotalEffectiveCapAssetAtoms: '1000000000000',
      engineLongOpenInterest: '300000000000000000000000',
      engineShortOpenInterest: '200000000000000000000000',
      engineLongEntryNotional: '300000000000',
      engineShortEntryNotional: '200000000000',
      sizeQuantum: '100000000000000000',
    },
  },
}

describe('PoolSolvencySection', () => {
  it('renders healthy solvency envelope, LP waterfall, and verified custody audit', () => {
    render(
      <PoolSolvencySection
        data={HEALTHY_SOLVENCY}
        isLoading={false}
        isError={false}
      />,
    )

    expect(screen.getByText('Solvency Healthy')).toBeInTheDocument()
    expect(screen.getByText('Terminal Deficit: 0')).toBeInTheDocument()
    expect(screen.getByText('Senior Unimpaired')).toBeInTheDocument()
    expect(screen.getByText('4.55×')).toBeInTheDocument()
    expect(screen.getByText('8.00% Senior APR')).toBeInTheDocument()
    expect(screen.getAllByText('Verified')).toHaveLength(4)
  })

  it('highlights degraded mode, custody deficit, timelock, and decoded critical fault labels', () => {
    const stressed: LiquiditySolvencyResponse = {
      ...HEALTHY_SOLVENCY,
      solvency: {
        ...HEALTHY_SOLVENCY.solvency,
        degradedMode: true,
        terminalDeficitAssets: '15000000000',
        seniorImpaired: true,
        canIncreaseRisk: false,
      },
      policy: {
        ...HEALTHY_SOLVENCY.policy,
        poolConfigActivationTime: 1796100000,
        pendingPoolConfigTimelockActive: true,
        secondsUntilPoolConfigActivation: 7200,
      },
      custodyAudit: {
        ...HEALTHY_SOLVENCY.custodyAudit!,
        healthState: 'faulted',
        custodyBalanced: false,
        criticalFaults: {
          raw: '2048',
          activeBits: [11],
          labels: ['Pool Custody Deficit'],
        },
        dependencyFailures: {
          raw: '256',
          activeBits: [8],
          labels: ['Oracle'],
        },
      },
    }

    render(
      <PoolSolvencySection data={stressed} isLoading={false} isError={false} />,
    )

    expect(screen.getByText('Degraded Mode')).toBeInTheDocument()
    expect(screen.getByText('Senior Impaired')).toBeInTheDocument()
    expect(screen.getByText('Risk Increase Blocked')).toBeInTheDocument()
    expect(screen.getByText(/Config Timelock · 2h/i)).toBeInTheDocument()
    expect(screen.getByText('Pool Custody Deficit')).toBeInTheDocument()
    expect(screen.getByText('Dep: Oracle')).toBeInTheDocument()
    expect(screen.getByText('Mismatch')).toBeInTheDocument()
  })
})
