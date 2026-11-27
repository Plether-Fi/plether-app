import { fireEvent, render, screen } from '@testing-library/react'
import { MemoryRouter, Route, Routes } from 'react-router-dom'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import type { KeeperDetailResponse } from '../api'
import { KeeperDetailPage } from './KeeperDetailPage'

const keeperQuery = vi.hoisted(() => vi.fn())
const protocolContext = vi.hoisted(() => vi.fn())

vi.mock('../api', async () => {
  const actual = await vi.importActual<typeof import('../api')>('../api')
  return {
    ...actual,
    useKeeperDetail: keeperQuery,
  }
})

vi.mock('../hooks/useProtocolContext', () => ({
  useProtocolContext: protocolContext,
}))

const KEEPER_ADDR = '0x5a71111111111111111111111111111111110d6b'

const SAMPLE_DETAIL: KeeperDetailResponse = {
  network: { chainId: 421614, releaseVersion: 'v2.0.0', settlementAssetSymbol: 'USDG' },
  observedAt: '2026-11-26T22:30:00Z',
  keeper: {
    address: KEEPER_ADDR,
    roles: ['liquidation', 'order_execution'],
    executions24h: 142,
    shareBps: 7100,
    latency: { sampleCount: 140, p50: 2, p95: 5, p99: 9, max: 14 },
    ethBalanceWei: '85000000000000000',
    bountiesEarnedAssets: '450000000',
    lastActiveBlock: '332948912',
    lastActiveAt: '2026-11-26T22:29:50Z',
  },
  roleBreakdown: [
    {
      role: 'order_execution',
      executions24h: 138,
      shareBps: 9718,
      latencyP50Seconds: 2,
      latencyP95Seconds: 5,
      bountiesEarnedAssets: '0',
    },
    {
      role: 'liquidation',
      executions24h: 4,
      shareBps: 282,
      latencyP50Seconds: null,
      latencyP95Seconds: null,
      bountiesEarnedAssets: '450000000',
    },
  ],
  activity: [
    {
      role: 'order_execution',
      keeperAddress: KEEPER_ADDR,
      txHash: `0x${'a'.repeat(64)}`,
      blockNumber: '332948912',
      occurredAt: '2026-11-26T22:29:50Z',
      latencySeconds: 2,
      bountyAssets: null,
    },
    {
      role: 'liquidation',
      keeperAddress: KEEPER_ADDR,
      txHash: `0x${'b'.repeat(64)}`,
      blockNumber: '332948800',
      occurredAt: '2026-11-26T22:25:00Z',
      latencySeconds: null,
      bountyAssets: '150000000',
    },
  ],
}

beforeEach(() => {
  protocolContext.mockReturnValue({
    chainId: 421614,
    chain: {
      chainId: 421614,
      name: 'Arbitrum Sepolia',
      shortName: 'Arb Sepolia',
      isTestnet: true,
      explorerBaseUrl: 'https://sepolia.arbiscan.io',
      tradeAppUrl: 'https://sepolia.plether.com',
      defaultAssetSymbol: 'USDG',
    },
    releaseVersion: 'v2.0.0',
    blockNumber: 332948912,
    observedAt: '2026-11-26T22:30:00Z',
  })
  keeperQuery.mockReturnValue({
    data: SAMPLE_DETAIL,
    isLoading: false,
    isError: false,
    error: null,
    refetch: vi.fn(),
  })
})

function renderAt(path: string) {
  return render(
    <MemoryRouter initialEntries={[path]}>
      <Routes>
        <Route path="/keepers/:address" element={<KeeperDetailPage />} />
      </Routes>
    </MemoryRouter>,
  )
}

describe('KeeperDetailPage', () => {
  it('renders keeper profile, ETH balance, role breakdown, and filterable executions linking to /tx', () => {
    renderAt(`/keepers/${KEEPER_ADDR}`)

    expect(screen.getByRole('heading', { name: '0x5a71…0d6b' })).toBeInTheDocument()
    expect(screen.getByText('0.0850 ETH')).toBeInTheDocument()
    expect(screen.getByText('71.00% of 24h protocol executions')).toBeInTheDocument()
    expect(screen.getByText('Role breakdown (24h)')).toBeInTheDocument()
    expect(screen.getByRole('link', { name: '0xaaaa…aaaa' })).toHaveAttribute(
      'href',
      `/tx/0x${'a'.repeat(64)}`,
    )
    expect(screen.getByRole('link', { name: '0xbbbb…bbbb' })).toHaveAttribute(
      'href',
      `/tx/0x${'b'.repeat(64)}`,
    )

    fireEvent.click(screen.getByRole('tab', { name: 'Liquidation' }))
    expect(screen.queryByRole('link', { name: '0xaaaa…aaaa' })).not.toBeInTheDocument()
    expect(screen.getByRole('link', { name: '0xbbbb…bbbb' })).toBeInTheDocument()
  })

  it('rejects invalid EVM addresses cleanly', () => {
    renderAt('/keepers/not-an-address')
    expect(screen.getAllByText('Invalid keeper address').length).toBeGreaterThanOrEqual(1)
  })
})
