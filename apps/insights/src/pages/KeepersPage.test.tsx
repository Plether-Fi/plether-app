import { fireEvent, render, screen, within } from '@testing-library/react'
import { MemoryRouter } from 'react-router-dom'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { InsightsApiError } from '../api/client'
import { resolvePerpsChain } from '../utils/chain'
import { KeepersPage } from './KeepersPage'

const protocolContext = vi.hoisted(() => vi.fn())
const keepersQuery = vi.hoisted(() => vi.fn())
const activityQuery = vi.hoisted(() => vi.fn())

vi.mock('../hooks/useProtocolContext', () => ({ useProtocolContext: protocolContext }))
vi.mock('../api', async (importOriginal) => ({
  ...(await importOriginal<typeof import('../api')>()),
  useKeepersStatus: keepersQuery,
  useKeepersActivity: activityQuery,
}))

const keeperOne = `0x${'1'.repeat(40)}`
const keeperTwo = `0x${'2'.repeat(40)}`

beforeEach(() => {
  protocolContext.mockReturnValue({
    chainId: 421614,
    chain: resolvePerpsChain(421614),
    releaseVersion: 'v2.0.0',
    blockNumber: '332879274',
    observedAt: '2026-11-26T11:37:00Z',
  })
  keepersQuery.mockReturnValue({
    data: {
      network: { chainId: 421614, releaseVersion: 'v2.0.0', settlementAssetSymbol: 'USDG' },
      observedAt: '2026-11-26T12:00:00Z',
      summary: {
        activeKeepers24h: 2,
        executions24h: 100,
        latencyP50Seconds: 3,
        latencyP95Seconds: 7,
        bountiesPaid24hAssets: '1500000',
      },
      keepers: [
        {
          address: keeperOne,
          roles: ['order_execution', 'liquidation'],
          executions24h: 75,
          shareBps: 7500,
          latencyP50Seconds: 3,
          latencyP95Seconds: 6,
          ethBalanceWei: '500000000000000000',
          bountiesEarnedAssets: '1500000',
          lastActiveBlock: '332879270',
          lastActiveAt: '2026-11-26T11:59:00Z',
        },
        {
          address: keeperTwo,
          roles: ['lp_settlement'],
          executions24h: 25,
          shareBps: 2500,
          latencyP50Seconds: 4,
          latencyP95Seconds: 8,
          ethBalanceWei: '5000000000000000',
          bountiesEarnedAssets: '0',
          lastActiveBlock: '332879260',
          lastActiveAt: '2026-11-26T11:55:00Z',
        },
      ],
    },
    isLoading: false,
    isError: false,
  })
  activityQuery.mockReturnValue({
    data: {
      network: { chainId: 421614, releaseVersion: 'v2.0.0', settlementAssetSymbol: 'USDG' },
      observedAt: '2026-11-26T12:00:00Z',
      activity: [
        {
          role: 'liquidation',
          keeperAddress: keeperOne,
          txHash: `0x${'a'.repeat(64)}`,
          blockNumber: '332879270',
          occurredAt: '2026-11-26T11:59:00Z',
          latencySeconds: 3,
          bountyAssets: '1500000',
        },
        {
          role: 'lp_settlement',
          keeperAddress: keeperTwo,
          txHash: `0x${'b'.repeat(64)}`,
          blockNumber: '332879260',
          occurredAt: '2026-11-26T11:55:00Z',
          latencySeconds: 4,
          bountyAssets: null,
        },
      ],
    },
    isLoading: false,
    isError: false,
  })
})

function show() {
  render(
    <MemoryRouter>
      <KeepersPage />
    </MemoryRouter>,
  )
}

describe('KeepersPage (Stage 2 Permissionless Keepers)', () => {
  it('displays every detected on-chain keeper identically with ETH balance, low-ETH flag, share, and filters', () => {
    show()
    expect(screen.getByText('2 active keepers (24h)')).toBeInTheDocument()
    expect(screen.getByText('3s / 7s')).toBeInTheDocument()

    const table = screen.getByRole('table', { name: /Permissionless on-chain keepers/ })
    expect(within(table).getByText('0.5000 ETH')).toBeInTheDocument()
    expect(within(table).getByText('0.0050 ETH')).toBeInTheDocument()
    expect(within(table).getByText('Low')).toBeInTheDocument()
    expect(within(table).getByText('75.00%')).toBeInTheDocument()
    expect(within(table).getByText('25.00%')).toBeInTheDocument()
    expect(screen.queryByText('Alto Bundler')).not.toBeInTheDocument()
    expect(screen.getAllByText('Indexed')).toHaveLength(4)
    expect(screen.getByText('Indexer pending')).toBeInTheDocument()

    const liqBtn = screen.getByRole('button', { name: 'Liquidation' })
    fireEvent.click(liqBtn)
    expect(liqBtn).toHaveAttribute('aria-pressed', 'true')
    expect(within(table).getByText('0.5000 ETH')).toBeInTheDocument()
    expect(within(table).queryByText('0.0050 ETH')).not.toBeInTheDocument()
  })

  it('fails closed cleanly when Stage 2 Keepers endpoints return 404', () => {
    const notFound = new InsightsApiError('Not found', 404)
    keepersQuery.mockReturnValue({ data: undefined, isLoading: false, isError: true, error: notFound })
    activityQuery.mockReturnValue({ data: undefined, isLoading: false, isError: true, error: notFound })
    show()
    expect(screen.getByText(/Connected backend does not serve Stage 2 Keepers endpoints yet/i)).toBeInTheDocument()
    expect(screen.getAllByText('Unavailable').length).toBeGreaterThanOrEqual(4)
  })
})
