import type { Meta, StoryObj } from '@storybook/react-vite'
import { MemoryRouter } from 'react-router-dom'
import type { LiquiditySolvencyResponse } from '../../../insights/src/api/liquidity'
import type { MarketFadRiskResponse } from '../../../insights/src/api/positions'
import { FadLiquidationWatchlistSection } from '../../../insights/src/components/FadLiquidationWatchlistSection'
import { PoolSolvencySection } from '../../../insights/src/components/PoolSolvencySection'
import './InsightsPositionBreakdown.css'

const SAMPLE_SOLVENCY: LiquiditySolvencyResponse = {
  network: {
    chainId: 421614,
    releaseVersion: 'v2.0.0',
    settlementAssetSymbol: 'USDG',
  },
  observedAt: '2026-11-28T14:00:00Z',
  observedBlock: '326500000',
  solvency: {
    poolTotalAssets: '1050000000000',
    poolRawAssets: '1050000000000',
    excessAssets: '145000000000',
    terminalDeficitAssets: '0',
    freeSettlementAssets: '790000000000',
    withdrawalReserveAssets: '260000000000',
    effectiveSolvencyAssets: '1008000000000',
    solvencyCoverageBps: 47727,
    freeLiquidityShareBps: 7524,
    maxDirectionalLiabilityAssets: '220000000000',
    totalTraderClaimsAssets: '42000000000',
    terminalLpPriceDeltaAssets: '21400000000',
    markPrice: '101240000',
    markTime: 1795874400,
    bookVersion: 14,
    hasOpenPositions: true,
    degradedMode: false,
    seniorImpaired: false,
    canIncreaseRisk: true,
    canAcceptOrdinaryDeposits: true,
    canSettleDepositEntries: true,
    seniorReservationsWithinLimits: true,
  },
  waterfall: {
    seniorPrincipalAssets: '700000000000',
    juniorPrincipalAssets: '350000000000',
    seniorHighWaterMarkAssets: '700000000000',
    seniorHwmCoverageBps: 10000,
    seniorImpairmentAssets: '0',
    protectedSeniorExposureAssets: '700000000000',
    maxSeniorWithdrawAssets: '560000000000',
    maxJuniorWithdrawAssets: '230000000000',
    seniorDepositCapacityAssets: '450000000000',
    reservedSeniorDepositAssets: '30000000000',
    unassignedAssets: '0',
    pendingRecapitalizationAssets: '0',
    pendingTradingRevenueAssets: '18600000000',
    positiveVpiAssets: '11200000000',
    negativeVpiAssets: '3800000000',
    netVpiAccruedAssets: '7400000000',
    carryIndex: '1004500000000000000',
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
      pendingDepositAssets: '30000000000',
      pendingRedeemShares: '0',
      depositClaimShares: '15000000000000',
      withdrawalEscrowAssets: '0',
    },
    junior: {
      pendingDepositAssets: '10000000000',
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
      rawAssets: '1050000000000',
      accountedAssets: '1050000000000',
      custodyDeficitAssets: '0',
      custodySurplusAssets: '0',
    },
    escrowAudit: {
      senior: {
        requiredAssetEscrow: '30000000000',
        actualAssetEscrow: '30000000000',
        requiredShareEscrow: '15000000000000',
        actualShareEscrow: '15000000000000',
      },
      junior: {
        requiredAssetEscrow: '10000000000',
        actualAssetEscrow: '10000000000',
        requiredShareEscrow: '0',
        actualShareEscrow: '0',
      },
    },
    navBookParity: {
      bookCapPrice: '200000000',
      engineCapPrice: '200000000',
      bookActiveCurveCount: 4,
      bookTotalLots: '6200000',
      bookTotalEntryCostAssetAtoms: '620000000000',
      bookTotalEffectiveCapAssetAtoms: '1240000000000',
      engineLongOpenInterest: '380000000000000000000000',
      engineShortOpenInterest: '240000000000000000000000',
      engineLongEntryNotional: '380000000000',
      engineShortEntryNotional: '240000000000',
      sizeQuantum: '100000000000000000',
    },
  },
}

const SAMPLE_FAD_RISK: MarketFadRiskResponse = {
  network: {
    chainId: 421614,
    releaseVersion: 'v2.0.0',
    settlementAssetSymbol: 'USDG',
  },
  observedAt: '2026-11-27T17:50:00Z',
  observedBlock: '326500120',
  markPrice: '101240000',
  accountsScanned: 24,
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
    secondsUntilNextFad: 14400,
    secondsUntilNextFreeze: 15000,
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
    totalOpenPositions: 5,
    liquidatableNowCount: 1,
    wouldLiquidateCount: 1,
    criticalCount: 1,
    atRiskCount: 1,
    safeCount: 1,
    endangeredCount: 4,
    immediateLiquidationNotionalAssets: '503720000000',
    immediateLiquidationLongNotionalAssets: '303720000000',
    immediateLiquidationShortNotionalAssets: '200000000000',
    totalEndangeredNotionalAssets: '753720000000',
    endangeredLongNotionalAssets: '433720000000',
    endangeredShortNotionalAssets: '320000000000',
    totalMarginTopUpNeededAssets: '6877600000',
  },
  positions: [
    {
      account: '0x1111111111111111111111111111111111111111',
      discordUsername: 'macro_whale',
      side: 'long',
      size: '300000000000000000000000',
      notionalAssets: '303720000000',
      marginAssets: '5200000000',
      traderClaimAssets: '0',
      unrealizedPnlAssets: '-1200000000',
      currentEquityAssets: '4000000000',
      projectedCarryToFadAssets: '42000000',
      projectedEquityAtFadAssets: '3958000000',
      normalRequiredMarginAssets: '303720000',
      fadRequiredMarginAssets: '9111600000',
      fadMarginBufferAssets: '-5153600000',
      marginNeededToSurviveAssets: '5153600000',
      effectiveLeverageX100: 7593,
      entryPrice: '100840000',
      markPrice: '101240000',
      normalLiquidationPrice: '102470000',
      fadLiquidationPrice: '101240000',
      distanceToFadLiquidationBps: 0,
      liquidatableNow: true,
      dangerStatus: 'liquidatable_now',
    },
    {
      account: '0x2222222222222222222222222222222222222222',
      discordUsername: 'fx_degen',
      side: 'short',
      size: '200000000000000000000000',
      notionalAssets: '200000000000',
      marginAssets: '4200000000',
      traderClaimAssets: '0',
      unrealizedPnlAssets: '100000000',
      currentEquityAssets: '4300000000',
      projectedCarryToFadAssets: '24000000',
      projectedEquityAtFadAssets: '4276000000',
      normalRequiredMarginAssets: '200000000',
      fadRequiredMarginAssets: '6000000000',
      fadMarginBufferAssets: '-1724000000',
      marginNeededToSurviveAssets: '1724000000',
      effectiveLeverageX100: 4651,
      entryPrice: '100740000',
      markPrice: '101240000',
      normalLiquidationPrice: '99190000',
      fadLiquidationPrice: '101240000',
      distanceToFadLiquidationBps: 0,
      liquidatableNow: false,
      dangerStatus: 'would_liquidate_at_fad',
    },
    {
      account: '0x3333333333333333333333333333333333333333',
      discordUsername: 'dxy_scalper',
      side: 'long',
      size: '130000000000000000000000',
      notionalAssets: '130000000000',
      marginAssets: '4350000000',
      traderClaimAssets: '0',
      unrealizedPnlAssets: '0',
      currentEquityAssets: '4350000000',
      projectedCarryToFadAssets: '15000000',
      projectedEquityAtFadAssets: '4335000000',
      normalRequiredMarginAssets: '130000000',
      fadRequiredMarginAssets: '3900000000',
      fadMarginBufferAssets: '435000000',
      marginNeededToSurviveAssets: '0',
      effectiveLeverageX100: 2989,
      entryPrice: '101240000',
      markPrice: '101240000',
      normalLiquidationPrice: '104400000',
      fadLiquidationPrice: '101570000',
      distanceToFadLiquidationBps: 33,
      liquidatableNow: false,
      dangerStatus: 'critical',
    },
    {
      account: '0x4444444444444444444444444444444444444444',
      discordUsername: 'delta_desk',
      side: 'short',
      size: '120000000000000000000000',
      notionalAssets: '120000000000',
      marginAssets: '4900000000',
      traderClaimAssets: '0',
      unrealizedPnlAssets: '0',
      currentEquityAssets: '4900000000',
      projectedCarryToFadAssets: '12000000',
      projectedEquityAtFadAssets: '4888000000',
      normalRequiredMarginAssets: '120000000',
      fadRequiredMarginAssets: '3600000000',
      fadMarginBufferAssets: '1288000000',
      marginNeededToSurviveAssets: '0',
      effectiveLeverageX100: 2449,
      entryPrice: '101240000',
      markPrice: '101240000',
      normalLiquidationPrice: '97100000',
      fadLiquidationPrice: '100180000',
      distanceToFadLiquidationBps: 107,
      liquidatableNow: false,
      dangerStatus: 'at_risk',
    },
    {
      account: '0x5555555555555555555555555555555555555555',
      discordUsername: 'hedged_vault',
      side: 'long',
      size: '80000000000000000000000',
      notionalAssets: '80992000000',
      marginAssets: '16000000000',
      traderClaimAssets: '0',
      unrealizedPnlAssets: '400000000',
      currentEquityAssets: '16400000000',
      projectedCarryToFadAssets: '9000000',
      projectedEquityAtFadAssets: '16391000000',
      normalRequiredMarginAssets: '80992000',
      fadRequiredMarginAssets: '2429760000',
      fadMarginBufferAssets: '13961240000',
      marginNeededToSurviveAssets: '0',
      effectiveLeverageX100: 494,
      entryPrice: '101740000',
      markPrice: '101240000',
      normalLiquidationPrice: '121600000',
      fadLiquidationPrice: '118200000',
      distanceToFadLiquidationBps: 1717,
      liquidatableNow: false,
      dangerStatus: 'safe',
    },
  ],
}

const meta: Meta = {
  title: 'Insights/Protocol Health & FAD Risk',
  parameters: {
    layout: 'fullscreen',
    backgrounds: { default: 'dark' },
  },
}

export default meta

export const ProtocolSolvencyHealthy: StoryObj = {
  render: () => (
    <MemoryRouter>
      <div className="min-h-screen bg-app-bg p-6 text-content-primary sm:p-10">
        <div className="mx-auto max-w-6xl">
          <PoolSolvencySection
            data={SAMPLE_SOLVENCY}
            isLoading={false}
            isError={false}
          />
        </div>
      </div>
    </MemoryRouter>
  ),
}

export const ProtocolSolvencyAlert: StoryObj = {
  render: () => (
    <MemoryRouter>
      <div className="min-h-screen bg-app-bg p-6 text-content-primary sm:p-10">
        <div className="mx-auto max-w-6xl">
          <PoolSolvencySection
            data={{
              ...SAMPLE_SOLVENCY,
              solvency: {
                ...SAMPLE_SOLVENCY.solvency,
                degradedMode: true,
                terminalDeficitAssets: '18500000000',
                seniorImpaired: true,
                canIncreaseRisk: false,
                solvencyCoverageBps: 11200,
              },
              policy: {
                ...SAMPLE_SOLVENCY.policy,
                pendingPoolConfigTimelockActive: true,
                secondsUntilPoolConfigActivation: 187200,
              },
              custodyAudit: {
                ...SAMPLE_SOLVENCY.custodyAudit!,
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
            }}
            isLoading={false}
            isError={false}
          />
        </div>
      </div>
    </MemoryRouter>
  ),
}

export const FadLiquidationWatchlist: StoryObj = {
  render: () => (
    <MemoryRouter>
      <div className="min-h-screen bg-app-bg p-6 text-content-primary sm:p-10">
        <div className="mx-auto max-w-6xl">
          <FadLiquidationWatchlistSection
            data={SAMPLE_FAD_RISK}
            isLoading={false}
            isError={false}
          />
        </div>
      </div>
    </MemoryRouter>
  ),
}
