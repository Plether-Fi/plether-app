import type { Meta, StoryObj } from '@storybook/react-vite'
import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import { useMemo } from 'react'
import { MemoryRouter, Route, Routes } from 'react-router-dom'
import { liquidityKeys, type LiquidityEpochResponse } from '../../../insights/src/api/liquidity'
import { LiquidityEpochPage } from '../../../insights/src/pages/LiquidityEpochPage'
import './InsightsPositionBreakdown.css'

const CLEAN_EPOCH: LiquidityEpochResponse = {
  network: {
    chainId: 421614,
    releaseVersion: 'v2.0.0',
    settlementAssetSymbol: 'USDG',
  },
  monitorAddress: '0x6b3a91c4e2d8f05a1b7c3e9d4f2a8b6c0e1d5f3a',
  epoch: 498776,
  epochTimestamp: 1795593600,
  epochUtc: '2026-11-26T00:00:00Z',
  previousEpoch: 498634,
  nextEpoch: 498777,
  latestObservation: {
    epoch: 498776,
    observedBlock: '332948900',
    observedAt: '2026-11-26T00:00:06Z',
    readiness: 'ready',
    executionPath: 'cached_mark',
    healthState: 'healthy',
    observationComplete: true,
    hasMaturedWork: true,
    paused: false,
    masks: {
      operationalBlocker: { raw: '0', activeBits: [] },
      warning: { raw: '0', activeBits: [] },
      dependencyFailure: { raw: '0', activeBits: [] },
      criticalFault: { raw: '0', activeBits: [] },
    },
  },
  observations: [],
  settlements: [
    {
      epoch: 498776,
      cutoffEpoch: '498775',
      status: 'confirmed_success',
      executionPath: 'cached_mark',
      txHash: '0x8a4e91c7d3b2f605a9e1c4b8d2f7a0e3c6b9d1f4a7e2c5b8d0f3a6e9c2b5d8f1',
      receiptBlock: '332948912',
      createdAt: '2026-11-26T00:00:08Z',
      confirmedAt: '2026-11-26T00:00:12Z',
      confirmationLatencySeconds: 4,
      replacementCount: 0,
      seniorDepositAssets: '48500000000',
      seniorRedeemAssets: '12000000000',
      juniorDepositAssets: '22000000000',
      juniorRedeemAssets: '5000000000',
      seniorBacklog: false,
      juniorBacklog: false,
      entriesDeferred: false,
    },
  ],
  vaultSnapshot: {
    epochTimestamp: 1795593600,
    blockNumber: '332948895',
    blockTimestamp: 1795593598,
    blockUtc: '2026-11-25T23:59:58Z',
    markFresh: true,
    senior: {
      totalAssets: '1425000000000',
      lockedAssets: '1380000000000',
      totalSupply: '1392000000000',
      sharePriceWad: '1023700000000000000',
      sharePriceDeltaBps: 8,
    },
    junior: {
      totalAssets: '612000000000',
      lockedAssets: '585000000000',
      totalSupply: '574000000000',
      sharePriceWad: '1066200000000000000',
      sharePriceDeltaBps: 34,
    },
  },
  coveringReport: {
    startBlockNumber: '332900000',
    endBlockNumber: '332950000',
    sourceCommit: '9f4a21c',
    schemaVersion: 3,
    updatedAt: '2026-11-26T00:12:00Z',
    sourceBlockCount: 674,
    reportedReturnPp: 1.42,
    unexplainedPp: 0,
    netReturnUsdg: '8420000000',
    unexplainedUsdg: '0',
    liquidationFeesUsdg: '640000000',
    notesCount: 0,
  },
}

const BACKLOG_EPOCH: LiquidityEpochResponse = {
  ...CLEAN_EPOCH,
  epoch: 498634,
  previousEpoch: 498609,
  nextEpoch: 498776,
  latestObservation: {
    ...CLEAN_EPOCH.latestObservation!,
    epoch: 498634,
    readiness: 'ready_with_warnings',
    executionPath: 'atomic_oracle_refresh',
    masks: {
      operationalBlocker: { raw: '0', activeBits: [] },
      warning: { raw: '4', activeBits: [2] },
      dependencyFailure: { raw: '0', activeBits: [] },
      criticalFault: { raw: '0', activeBits: [] },
    },
  },
  settlements: [
    {
      ...CLEAN_EPOCH.settlements[0],
      epoch: 498634,
      executionPath: 'atomic_oracle_refresh',
      confirmationLatencySeconds: 18,
      replacementCount: 1,
      seniorBacklog: true,
      entriesDeferred: true,
    },
  ],
  vaultSnapshot: {
    ...CLEAN_EPOCH.vaultSnapshot!,
    markFresh: false,
    junior: {
      ...CLEAN_EPOCH.vaultSnapshot!.junior,
      sharePriceDeltaBps: -18,
    },
  },
}

function EpochStoryHarness({ data }: { data: LiquidityEpochResponse }) {
  const client = useMemo(() => {
    const qc = new QueryClient({
      defaultOptions: {
        queries: {
          retry: false,
          staleTime: Infinity,
          refetchInterval: false,
        },
      },
    })
    qc.setQueryData(liquidityKeys.epoch(data.epoch), data)
    qc.setQueryData(['insights', 'status'], {
      healthy: true,
      latestIndexedBlock: 332948912,
      latestIndexedAt: data.epochUtc,
      chainId: data.network.chainId,
    })
    return qc
  }, [data])

  return (
    <QueryClientProvider client={client}>
      <MemoryRouter initialEntries={[`/liquidity/epochs/${String(data.epoch)}`]}>
        <div className="min-h-screen bg-app-bg px-6 py-8 text-content-primary">
          <div className="mx-auto max-w-7xl">
            <Routes>
              <Route path="/liquidity/epochs/:epoch" element={<LiquidityEpochPage />} />
            </Routes>
          </div>
        </div>
      </MemoryRouter>
    </QueryClientProvider>
  )
}

const meta: Meta<typeof EpochStoryHarness> = {
  title: 'Insights/Liquidity — LP Epoch Detail Page',
  component: EpochStoryHarness,
  parameters: {
    layout: 'fullscreen',
  },
}

export default meta

type Story = StoryObj<typeof EpochStoryHarness>

export const ConfirmedCleanEpoch: Story = {
  args: { data: CLEAN_EPOCH },
}

export const WarningWithBacklogAndGasBump: Story = {
  args: { data: BACKLOG_EPOCH },
}
