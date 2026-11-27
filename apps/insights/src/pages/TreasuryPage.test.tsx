import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import { fireEvent, render, screen } from '@testing-library/react'
import { MemoryRouter } from 'react-router-dom'
import { afterEach, describe, expect, it, vi } from 'vitest'
import type { TreasuryOverviewResponse } from '../api/treasury'
import { TreasuryPage } from './TreasuryPage'

const treasuryFixture: TreasuryOverviewResponse = {
  blockNumber: '333150000',
  network: {
    chainId: 421614,
    releaseVersion: 'v2.0.0',
    settlementAssetSymbol: 'USDG',
  },
  observedAt: '2026-11-27T10:00:00Z',
  rpcAvailable: true,
  treasury: {
    address: '0x5a71a4094ec81165ada48aa4c27da48ec27e0d6b',
    addressSource: 'onchain',
    deploymentTreasuryAddress: '0x5a71a4094ec81165ada48aa4c27da48ec27e0d6b',
    lockedUntil: '2027-01-01T00:00:00Z',
    isLocked: true,
    pendingRotation: null,
  },
  feePolicy: {
    executionFeeBps: 4,
    liquidationProtocolShareBps: 0,
    liquidationKeeperShareBps: 5000,
    juniorMaintenanceFeeAprBps: 0,
    juniorMaintenanceFeeRecipient: '0x5a71a4094ec81165ada48aa4c27da48ec27e0d6b',
    pendingMaintenanceFeeShares: '0',
    pendingMaintenanceFeeAssets: '0',
  },
  holdings: {
    totalUsdgAssets: '15420000000',
    clearinghouseSettlementAssets: '12420000000',
    clearinghouseFreeAssets: '12420000000',
    clearinghouseClaimAssets: '0',
    clearinghouseHasPosition: false,
    walletUsdgAssets: '1000000000',
    walletEthWei: '1500000000000000000',
    juniorVaultShares: '1000000000000',
    juniorVaultAssets: '1000000000',
    seniorVaultShares: '1000000000000',
    seniorVaultAssets: '1000000000',
    pendingMaintenanceFeeAssets: '0',
  },
  profitOverview: {
    annualizedExecutionFeeRunRate7dAssets: '219000000000',
    window24h: {
      executedOrders: 650,
      volumeAssets: '1500000000000',
      executionFeeAssessedAssets: '600000000',
      executionFeeCreditedAssets: '580000000',
      effectiveFeeBps: 4.0,
      frozenSpreadAssessedAssets: '0',
      keeperBountyAssets: '130000000',
      liquidationCount: 2,
      liquidationKeeperBountyAssets: '45000000',
      closeAssistanceCount: 1,
      closeAssistanceUsdgAssets: '200000',
      sponsoredOpsCount: 820,
      sponsoredGasSpentWei: '4200000000000000',
      netUsdgSurplusAssets: '599800000',
    },
    window7d: {
      executedOrders: 4200,
      volumeAssets: '10500000000000',
      executionFeeAssessedAssets: '4200000000',
      executionFeeCreditedAssets: '4100000000',
      effectiveFeeBps: 4.0,
      frozenSpreadAssessedAssets: '0',
      keeperBountyAssets: '840000000',
      liquidationCount: 10,
      liquidationKeeperBountyAssets: '210000000',
      closeAssistanceCount: 3,
      closeAssistanceUsdgAssets: '600000',
      sponsoredOpsCount: 5400,
      sponsoredGasSpentWei: '28000000000000000',
      netUsdgSurplusAssets: '4199400000',
    },
    window30d: {
      executedOrders: 12400,
      volumeAssets: '31050000000000',
      executionFeeAssessedAssets: '12420000000',
      executionFeeCreditedAssets: '12100000000',
      effectiveFeeBps: 4.0,
      frozenSpreadAssessedAssets: '0',
      keeperBountyAssets: '2480000000',
      liquidationCount: 24,
      liquidationKeeperBountyAssets: '520000000',
      closeAssistanceCount: 5,
      closeAssistanceUsdgAssets: '1000000',
      sponsoredOpsCount: 15200,
      sponsoredGasSpentWei: '81000000000000000',
      netUsdgSurplusAssets: '12419000000',
    },
    allTime: {
      executedOrders: 12400,
      volumeAssets: '31050000000000',
      executionFeeAssessedAssets: '12420000000',
      executionFeeCreditedAssets: '12100000000',
      effectiveFeeBps: 4.0,
      frozenSpreadAssessedAssets: '0',
      keeperBountyAssets: '2480000000',
      liquidationCount: 24,
      liquidationKeeperBountyAssets: '520000000',
      closeAssistanceCount: 5,
      closeAssistanceUsdgAssets: '1000000',
      sponsoredOpsCount: 15200,
      sponsoredGasSpentWei: '81000000000000000',
      netUsdgSurplusAssets: '12419000000',
    },
  },
  dailySeries: [
    {
      dayStartUnix: 1795564800,
      dayUtc: '2026-11-25',
      executedOrders: 520,
      volumeAssets: '1200000000000',
      executionFeeAssets: '480000000',
      cumulativeExecutionFeeAssets: '11340000000',
      closeAssistanceAssets: '0',
      sponsoredGasWei: '3100000000000000',
    },
    {
      dayStartUnix: 1795651200,
      dayUtc: '2026-11-26',
      executedOrders: 610,
      volumeAssets: '1250000000000',
      executionFeeAssets: '500000000',
      cumulativeExecutionFeeAssets: '11840000000',
      closeAssistanceAssets: '200000',
      sponsoredGasWei: '3900000000000000',
    },
    {
      dayStartUnix: 1795737600,
      dayUtc: '2026-11-27',
      executedOrders: 650,
      volumeAssets: '1500000000000',
      executionFeeAssets: '600000000',
      cumulativeExecutionFeeAssets: '12440000000',
      closeAssistanceAssets: '0',
      sponsoredGasWei: '4200000000000000',
    },
  ],
  recentFeeOrders: [
    {
      orderId: '62301',
      txHash: '0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa',
      account: '0x1111111111111111111111111111111111111111',
      side: 'Short',
      volumeAssets: '50000000000',
      executionFeeAssets: '20000000',
      effectiveFeeBps: 4.0,
      blockNumber: '333149900',
      occurredAt: '2026-11-27T09:58:00Z',
    },
  ],
  treasuryMovements: [
    {
      kind: 'clearinghouse_withdraw',
      direction: 'outflow',
      amountAssets: '1000000000',
      counterparty: '0xb5b851f627d6df3cf3f096e23e20a58884d7b89a',
      txHash: '0xbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb',
      blockNumber: '333100000',
      occurredAt: '2026-11-26T18:00:00Z',
    },
  ],
}

vi.mock('../api/treasury', async () => {
  const actual = await vi.importActual<typeof import('../api/treasury')>('../api/treasury')
  return {
    ...actual,
    useTreasuryOverview: vi.fn(),
  }
})

vi.mock('../hooks/useProtocolContext', () => ({
  useProtocolContext: () => ({
    chain: { chainId: 421614, name: 'Arbitrum Sepolia', isTestnet: true },
    releaseVersion: 'v2.0.0',
    blockNumber: '333150000',
    observedAt: '2026-11-27T10:00:00Z',
  }),
}))

import { useTreasuryOverview } from '../api/treasury'

function renderPage() {
  const qc = new QueryClient({ defaultOptions: { queries: { retry: false } } })
  return render(
    <QueryClientProvider client={qc}>
      <MemoryRouter initialEntries={['/treasury']}>
        <TreasuryPage />
      </MemoryRouter>
    </QueryClientProvider>,
  )
}

describe('TreasuryPage', () => {
  afterEach(() => {
    vi.clearAllMocks()
  })

  it('renders treasury balance sheet, protocol profit ledger, fee policy, and audit trails', () => {
    vi.mocked(useTreasuryOverview).mockReturnValue({
      data: treasuryFixture,
      isLoading: false,
      isError: false,
      error: null,
      refetch: vi.fn(),
    } as unknown as ReturnType<typeof useTreasuryOverview>)

    renderPage()

    expect(screen.getByText(/Treasury Wallet & Revenue Ledger/i)).toBeInTheDocument()
    expect(screen.getByText('15,420.00 USDG')).toBeInTheDocument()
    expect(screen.getAllByText('12,420.00 USDG').length).toBeGreaterThan(0)
    expect(screen.getByText('219,000.00 USDG')).toBeInTheDocument()
    expect(screen.getByText('4 bps (0.04%)')).toBeInTheDocument()
    expect(screen.getByText('#62301')).toBeInTheDocument()
    expect(screen.getByText('Clearinghouse withdraw')).toBeInTheDocument()

    // Toggle revenue chart between 30d and 7d
    const btn7d = screen.getByRole('button', { name: '7d' })
    fireEvent.click(btn7d)
    expect(btn7d).toHaveAttribute('aria-pressed', 'true')
  })

  it('shows pending treasury rotation banner when a rotation is proposed', () => {
    vi.mocked(useTreasuryOverview).mockReturnValue({
      data: {
        ...treasuryFixture,
        treasury: {
          ...treasuryFixture.treasury,
          pendingRotation: {
            treasury: '0x9999999999999999999999999999999999999999',
            activationTime: '2026-12-01T00:00:00Z',
            lockedUntil: null,
          },
        },
      },
      isLoading: false,
      isError: false,
      error: null,
      refetch: vi.fn(),
    } as unknown as ReturnType<typeof useTreasuryOverview>)

    renderPage()
    expect(screen.getByText(/Pending Treasury Rotation Proposed/i)).toBeInTheDocument()
    expect(screen.getByText('Timelock active')).toBeInTheDocument()
  })
})
