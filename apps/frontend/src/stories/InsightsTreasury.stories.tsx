import type { Meta, StoryObj } from '@storybook/react-vite'
import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import { useMemo } from 'react'
import { MemoryRouter } from 'react-router-dom'
import type { DailyRevenuePoint, TreasuryOverviewResponse } from '../../../insights/src/api/treasury'
import { TreasuryPage } from '../../../insights/src/pages/TreasuryPage'
import './InsightsPositionBreakdown.css'

function buildSampleDailySeries(): DailyRevenuePoint[] {
  const startUnix = 1793232000 // 2026-10-29 UTC
  let cumulative = 240_000_000
  return Array.from({ length: 30 }, (_, i) => {
    const dayStartUnix = startUnix + i * 86400
    const dayUtc = new Date(dayStartUnix * 1000).toISOString().slice(0, 10)
    const wave = 340 + ((i * 73) % 290) + (i > 20 ? 180 : 0)
    const feeUsdg = wave * 1_000_000
    const volUsdg = feeUsdg * 2500
    cumulative += feeUsdg
    return {
      dayStartUnix,
      dayUtc,
      executedOrders: Math.round(wave * 1.4),
      volumeAssets: String(volUsdg),
      executionFeeAssets: String(feeUsdg),
      cumulativeExecutionFeeAssets: String(cumulative),
      closeAssistanceAssets: i % 6 === 0 ? '200000' : '0',
      sponsoredGasWei: String(BigInt(2_800 + (i % 7) * 420) * 1_000_000_000_000n),
    }
  })
}

const SAMPLE_SERIES = buildSampleDailySeries()
const TOTAL_FEES = SAMPLE_SERIES[SAMPLE_SERIES.length - 1]?.cumulativeExecutionFeeAssets ?? '16840000000'

const SAMPLE_TREASURY_OVERVIEW: TreasuryOverviewResponse = {
  blockNumber: '333154820',
  network: {
    chainId: 421614,
    releaseVersion: 'v2.0.0',
    settlementAssetSymbol: 'USDG',
  },
  observedAt: '2026-11-27T10:15:00Z',
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
    totalUsdgAssets: '19240500000',
    clearinghouseSettlementAssets: '15840500000',
    clearinghouseFreeAssets: '15840500000',
    clearinghouseClaimAssets: '0',
    clearinghouseHasPosition: false,
    walletUsdgAssets: '1400000000',
    walletEthWei: '2450000000000000000',
    juniorVaultShares: '1000000000000',
    juniorVaultAssets: '1000000000',
    seniorVaultShares: '1000000000000',
    seniorVaultAssets: '1000000000',
    pendingMaintenanceFeeAssets: '0',
  },
  profitOverview: {
    annualizedExecutionFeeRunRate7dAssets: '246530000000',
    window24h: {
      executedOrders: 912,
      volumeAssets: '1765000000000',
      executionFeeAssessedAssets: '706000000',
      executionFeeCreditedAssets: '689400000',
      effectiveFeeBps: 4.0,
      frozenSpreadAssessedAssets: '0',
      keeperBountyAssets: '182400000',
      liquidationCount: 2,
      liquidationKeeperBountyAssets: '44550000',
      closeAssistanceCount: 1,
      closeAssistanceUsdgAssets: '200000',
      sponsoredOpsCount: 1240,
      sponsoredGasSpentWei: '4350000000000000',
      netUsdgSurplusAssets: '705800000',
    },
    window7d: {
      executedOrders: 6180,
      volumeAssets: '11820000000000',
      executionFeeAssessedAssets: '4728000000',
      executionFeeCreditedAssets: '4610000000',
      effectiveFeeBps: 4.0,
      frozenSpreadAssessedAssets: '0',
      keeperBountyAssets: '1236000000',
      liquidationCount: 9,
      liquidationKeeperBountyAssets: '198000000',
      closeAssistanceCount: 2,
      closeAssistanceUsdgAssets: '400000',
      sponsoredOpsCount: 8420,
      sponsoredGasSpentWei: '29800000000000000',
      netUsdgSurplusAssets: '4727600000',
    },
    window30d: {
      executedOrders: 21420,
      volumeAssets: '41500000000000',
      executionFeeAssessedAssets: TOTAL_FEES,
      executionFeeCreditedAssets: String(BigInt(TOTAL_FEES) - 210_000_000n),
      effectiveFeeBps: 4.0,
      frozenSpreadAssessedAssets: '0',
      keeperBountyAssets: '4284000000',
      liquidationCount: 28,
      liquidationKeeperBountyAssets: '612000000',
      closeAssistanceCount: 5,
      closeAssistanceUsdgAssets: '1000000',
      sponsoredOpsCount: 29100,
      sponsoredGasSpentWei: '112400000000000000',
      netUsdgSurplusAssets: String(BigInt(TOTAL_FEES) - 1_000_000n),
    },
    allTime: {
      executedOrders: 21420,
      volumeAssets: '41500000000000',
      executionFeeAssessedAssets: TOTAL_FEES,
      executionFeeCreditedAssets: String(BigInt(TOTAL_FEES) - 210_000_000n),
      effectiveFeeBps: 4.0,
      frozenSpreadAssessedAssets: '0',
      keeperBountyAssets: '4284000000',
      liquidationCount: 28,
      liquidationKeeperBountyAssets: '612000000',
      closeAssistanceCount: 5,
      closeAssistanceUsdgAssets: '1000000',
      sponsoredOpsCount: 29100,
      sponsoredGasSpentWei: '112400000000000000',
      netUsdgSurplusAssets: String(BigInt(TOTAL_FEES) - 1_000_000n),
    },
  },
  dailySeries: SAMPLE_SERIES,
  recentFeeOrders: [
    {
      orderId: '62318',
      txHash: '0x6b3a1204c81e9012f4c3d9b2e7a8c1209f14a51b890c3d2a1b4f9012c34d5678',
      account: '0x7a1290f4c3b18e9d234567890abcdef123456789',
      side: 'Long',
      volumeAssets: '125000000000',
      executionFeeAssets: '50000000',
      effectiveFeeBps: 4.0,
      blockNumber: '333154812',
      occurredAt: '2026-11-27T10:14:22Z',
    },
    {
      orderId: '62315',
      txHash: '0x2d4e8901f7a23b4c5d6e7f8091a2b3c4d5e6f708192a3b4c5d6e7f8091a2b3c4',
      account: '0x3b88f1c9a0d27e4156980abcde1234567890abcd',
      side: 'Short',
      volumeAssets: '82500000000',
      executionFeeAssets: '33000000',
      effectiveFeeBps: 4.0,
      blockNumber: '333154790',
      occurredAt: '2026-11-27T10:12:48Z',
    },
    {
      orderId: '62311',
      txHash: '0x9f1e2d3c4b5a69788796a5b4c3d2e1f00112233445566778899aabbccddeeff0',
      account: '0x9c01d4e7f8a9b0c1d2e3f4a5b6c7d8e9f0a1b2c3',
      side: 'Long',
      volumeAssets: '60000000000',
      executionFeeAssets: '24000000',
      effectiveFeeBps: 4.0,
      blockNumber: '333154710',
      occurredAt: '2026-11-27T10:10:05Z',
    },
  ],
  treasuryMovements: [
    {
      kind: 'clearinghouse_withdraw',
      direction: 'outflow',
      amountAssets: '1400000000',
      counterparty: '0xb5b851f627d6df3cf3f096e23e20a58884d7b89a',
      txHash: '0x4444555566667777888899990000111122223333444455556666777788889999',
      blockNumber: '332900412',
      occurredAt: '2026-11-24T16:30:00Z',
    },
    {
      kind: 'clearinghouse_deposit',
      direction: 'inflow',
      amountAssets: '2000000000',
      counterparty: '0xb5b851f627d6df3cf3f096e23e20a58884d7b89a',
      txHash: '0x1111222233334444555566667777888899990000aaaabbbbccccddddeeeeffff',
      blockNumber: '323658900',
      occurredAt: '2026-10-28T14:00:00Z',
    },
  ],
}

function TreasuryStoryCanvas({ data }: { data: TreasuryOverviewResponse }) {
  const queryClient = useMemo(() => {
    const qc = new QueryClient({
      defaultOptions: {
        queries: {
          retry: false,
          staleTime: Infinity,
          refetchInterval: false,
          refetchOnMount: false,
          refetchOnWindowFocus: false,
          refetchOnReconnect: false,
        },
      },
    })
    qc.setQueryData(['insights', 'treasury', 'overview'], data)
    qc.setQueryData(['insights', 'status'], {
      chainId: 421614,
      releaseVersion: 'v2.0.0',
      latestIndexedBlock: '333154820',
      latestIndexedAt: '2026-11-27T10:15:00Z',
      healthy: true,
    })
    return qc
  }, [data])

  return (
    <QueryClientProvider client={queryClient}>
      <MemoryRouter initialEntries={['/treasury']}>
        <div className="min-h-screen bg-app-bg p-6 text-content-primary sm:p-8">
          <div className="mx-auto max-w-7xl">
            <TreasuryPage />
          </div>
        </div>
      </MemoryRouter>
    </QueryClientProvider>
  )
}

const meta: Meta<typeof TreasuryStoryCanvas> = {
  title: 'Insights/Treasury Overview',
  component: TreasuryStoryCanvas,
  parameters: {
    layout: 'fullscreen',
  },
}

export default meta
type Story = StoryObj<typeof TreasuryStoryCanvas>

export const LiveTreasuryAndProfitOverview: Story = {
  args: {
    data: SAMPLE_TREASURY_OVERVIEW,
  },
}
