import type { Meta, StoryObj } from '@storybook/react-vite'
import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import { useMemo } from 'react'
import { MemoryRouter, Route, Routes } from 'react-router-dom'
import { keeperKeys, type KeeperDetailResponse } from '../../../insights/src/api/execution'
import { KeeperDetailPage } from '../../../insights/src/pages/KeeperDetailPage'
import './InsightsPositionBreakdown.css'

const KEEPER_ADDR = '0x5a71c4e9b2d8f031a6e4c9b7d2f5a8e1c4b70d6b'

const ACTIVE_KEEPER: KeeperDetailResponse = {
  network: {
    chainId: 421614,
    releaseVersion: 'v2.0.0',
    settlementAssetSymbol: 'USDG',
  },
  observedAt: '2026-11-26T22:35:00Z',
  keeper: {
    address: KEEPER_ADDR,
    roles: ['order_execution', 'lp_settlement', 'protection', 'liquidation'],
    executions24h: 1842,
    shareBps: 8890,
    latency: { sampleCount: 1830, p50: 2, p95: 4, p99: 7, max: 16 },
    ethBalanceWei: '142800000000000000',
    bountiesEarnedAssets: '1240000000',
    lastActiveBlock: '332948912',
    lastActiveAt: '2026-11-26T22:34:54Z',
  },
  roleBreakdown: [
    {
      role: 'order_execution',
      executions24h: 1796,
      shareBps: 9750,
      latencyP50Seconds: 2,
      latencyP95Seconds: 4,
      bountiesEarnedAssets: '0',
    },
    {
      role: 'lp_settlement',
      executions24h: 24,
      shareBps: 130,
      latencyP50Seconds: 4,
      latencyP95Seconds: 7,
      bountiesEarnedAssets: '0',
    },
    {
      role: 'protection',
      executions24h: 16,
      shareBps: 87,
      latencyP50Seconds: 3,
      latencyP95Seconds: 6,
      bountiesEarnedAssets: '320000000',
    },
    {
      role: 'liquidation',
      executions24h: 6,
      shareBps: 33,
      latencyP50Seconds: null,
      latencyP95Seconds: null,
      bountiesEarnedAssets: '920000000',
    },
  ],
  activity: [
    {
      role: 'order_execution',
      keeperAddress: KEEPER_ADDR,
      txHash: '0x91e4a7c2d5b8f031a6e4c9b7d2f5a8e1c4b70d6b91e4a7c2d5b8f031a6e4c9b7',
      blockNumber: '332948912',
      occurredAt: '2026-11-26T22:34:54Z',
      latencySeconds: 2,
      bountyAssets: null,
    },
    {
      role: 'liquidation',
      keeperAddress: KEEPER_ADDR,
      txHash: '0x42b8d1f9e3a6c502b7d4e9a1c3f6b8d2e5a7c0f342b8d1f9e3a6c502b7d4e9a1',
      blockNumber: '332948740',
      occurredAt: '2026-11-26T22:31:18Z',
      latencySeconds: null,
      bountyAssets: '215000000',
    },
    {
      role: 'lp_settlement',
      keeperAddress: KEEPER_ADDR,
      txHash: '0x7c3e5a9b1d4f6028c5e7a9b1d3f50729c4e6a8b07c3e5a9b1d4f6028c5e7a9b1',
      blockNumber: '332947200',
      occurredAt: '2026-11-26T22:00:05Z',
      latencySeconds: 4,
      bountyAssets: null,
    },
    {
      role: 'protection',
      keeperAddress: KEEPER_ADDR,
      txHash: '0x19f2b4d6e8a0c234567890abcdef1234567890ab19f2b4d6e8a0c234567890ab',
      blockNumber: '332946810',
      occurredAt: '2026-11-26T21:52:41Z',
      latencySeconds: 3,
      bountyAssets: '45000000',
    },
  ],
}

const LOW_ETH_KEEPER: KeeperDetailResponse = {
  ...ACTIVE_KEEPER,
  keeper: {
    ...ACTIVE_KEEPER.keeper,
    ethBalanceWei: '3200000000000000',
    executions24h: 48,
    shareBps: 420,
  },
}

function KeeperStoryHarness({ data }: { data: KeeperDetailResponse }) {
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
    qc.setQueryData(keeperKeys.detail(data.keeper.address, 50), data)
    qc.setQueryData(['insights', 'status'], {
      healthy: true,
      latestIndexedBlock: 332948912,
      latestIndexedAt: data.observedAt,
      chainId: data.network.chainId,
    })
    return qc
  }, [data])

  return (
    <QueryClientProvider client={client}>
      <MemoryRouter initialEntries={[`/keepers/${data.keeper.address}`]}>
        <div className="min-h-screen bg-app-bg px-6 py-8 text-content-primary">
          <div className="mx-auto max-w-7xl">
            <Routes>
              <Route path="/keepers/:address" element={<KeeperDetailPage />} />
            </Routes>
          </div>
        </div>
      </MemoryRouter>
    </QueryClientProvider>
  )
}

const meta: Meta<typeof KeeperStoryHarness> = {
  title: 'Insights/Keepers — Keeper Detail Page',
  component: KeeperStoryHarness,
  parameters: {
    layout: 'fullscreen',
  },
}

export default meta

type Story = StoryObj<typeof KeeperStoryHarness>

export const MultiRoleActiveKeeper: Story = {
  args: { data: ACTIVE_KEEPER },
}

export const LowEthRunwayKeeper: Story = {
  args: { data: LOW_ETH_KEEPER },
}
