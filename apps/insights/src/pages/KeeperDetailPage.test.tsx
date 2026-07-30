import { fireEvent, render, screen } from '@testing-library/react'
import { MemoryRouter, Route, Routes } from 'react-router-dom'
import type { ProtocolAction } from '../api'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { KeeperDetailPage } from './KeeperDetailPage'

const apiMocks = vi.hoisted(() => ({
  useCurrentProtocolRelease: vi.fn(),
  useKeeper: vi.fn(),
}))

vi.mock('../api', () => apiMocks)

const keeperAddress = '0x1111111111111111111111111111111111111111'

beforeEach(() => {
  vi.clearAllMocks()
  apiMocks.useCurrentProtocolRelease.mockReturnValue({
    data: { releaseId: 'release-1' },
    isLoading: false,
  })
  apiMocks.useKeeper.mockReturnValue(keeperQuery([keeperPage()]))
})

describe('KeeperDetailPage', () => {
  it('renames reward telemetry to the observable liquidation-bounty scope', () => {
    render(
      <MemoryRouter initialEntries={[`/keepers/${keeperAddress}?window=7d`]}>
        <Routes>
          <Route path="/keepers/:address" element={<KeeperDetailPage />} />
        </Routes>
      </MemoryRouter>,
    )

    expect(apiMocks.useKeeper).toHaveBeenCalledWith('release-1', keeperAddress, '7d')
    expect(screen.getByText('Observed Liquidation Bounties Usdc')).toBeInTheDocument()
    expect(screen.queryByText('Gross Rewards Usdc')).not.toBeInTheDocument()
    expect(screen.getByText(/sums of available exact receipt and transaction values and may be partial/)).toBeInTheDocument()
    expect(screen.getByText(/Mark-update and LP-maintenance categories remain unavailable/)).toBeInTheDocument()
  })

  it('queries an archived release independently and preserves it across keeper navigation', () => {
    apiMocks.useCurrentProtocolRelease.mockReturnValue({
      data: undefined,
      isLoading: true,
      isError: false,
    })

    render(
      <MemoryRouter initialEntries={[`/keepers/${keeperAddress}?release=release-archive&window=7d`]}>
        <Routes>
          <Route path="/keepers/:address" element={<KeeperDetailPage />} />
        </Routes>
      </MemoryRouter>,
    )

    expect(apiMocks.useKeeper).toHaveBeenLastCalledWith('release-archive', keeperAddress, '7d')
    expect(screen.queryByLabelText('Loading')).not.toBeInTheDocument()
    expect(screen.getByRole('link', { name: '← All keepers' })).toHaveAttribute(
      'href',
      '/keepers?release=release-archive&window=7d',
    )

    fireEvent.click(screen.getByRole('button', { name: '30d' }))

    expect(apiMocks.useKeeper).toHaveBeenLastCalledWith('release-archive', keeperAddress, '30d')
    expect(screen.getByRole('link', { name: '← All keepers' })).toHaveAttribute(
      'href',
      '/keepers?release=release-archive&window=30d',
    )
  })

  it('flattens anchored pages, deduplicates actions and availability, and loads more', () => {
    const fetchNextPage = vi.fn()
    const firstAction = protocolAction({ actionId: 'action-1', actionType: 'order_execution' })
    const secondAction = protocolAction({ actionId: 'action-2', actionType: 'liquidation', logIndex: '2' })
    const firstAvailability = {
      field: 'summary.totalGrossRewardsUsdc',
      reason: 'keeper_total_gross_rewards_unavailable',
    }
    apiMocks.useKeeper.mockReturnValue(keeperQuery([
      keeperPage({
        blockNumber: '123',
        actions: [firstAction],
        availability: [firstAvailability],
      }),
      keeperPage({
        blockNumber: '999',
        actions: [firstAction, secondAction],
        availability: [
          firstAvailability,
          { field: 'summary.nativeCosts.pythFeeWei', reason: 'pyth_fee_component_not_isolated' },
        ],
      }),
    ], { hasNextPage: true, fetchNextPage }))

    render(
      <MemoryRouter initialEntries={[`/keepers/${keeperAddress}?window=7d`]}>
        <Routes>
          <Route path="/keepers/:address" element={<KeeperDetailPage />} />
        </Routes>
      </MemoryRouter>,
    )

    expect(screen.getByText('Confirmed block').parentElement).toHaveTextContent('123')
    expect(screen.queryByText('999')).not.toBeInTheDocument()
    expect(screen.getByText(/2 unique actions loaded across 2 anchored pages/)).toBeInTheDocument()
    expect(screen.getByText('Order Execution')).toBeInTheDocument()
    expect(screen.getByText('Liquidation')).toBeInTheDocument()
    expect(screen.getByText('2 fields unavailable or incomplete')).toBeInTheDocument()

    fireEvent.click(screen.getByRole('button', { name: 'Load more keeper actions' }))
    expect(fetchNextPage).toHaveBeenCalledOnce()
  })
})

function keeperQuery(
  pages: ReturnType<typeof keeperPage>[],
  overrides: Record<string, unknown> = {},
) {
  return {
    data: { pages },
    isError: false,
    isLoading: false,
    hasNextPage: false,
    isFetchingNextPage: false,
    fetchNextPage: vi.fn(),
    refetch: vi.fn(),
    ...overrides,
  }
}

function keeperPage({
  blockNumber = '123',
  actions = [],
  availability = [{
    field: 'keeper.totalGrossRewardsUsdc',
    reason: 'keeper_total_gross_rewards_unavailable',
  }],
}: {
  blockNumber?: string
  actions?: ProtocolAction[]
  availability?: { field: string; reason: string }[]
} = {}) {
  return {
    releaseId: 'release-1',
    chainId: '421614',
    confirmedBlock: {
      number: blockNumber,
      hash: `0x${blockNumber.padStart(64, '0')}`,
      timestamp: 1_785_000_000,
    },
    indexerTimestamp: 1_785_000_010,
    calculationVersion: 'protocol-transparency-v1',
    evidence: {
      actions: 'confirmed_successful_log_actions',
    },
    availability,
    keeper: {
      address: keeperAddress,
      window: '7d',
      summary: {
        actionCount: '4',
        observedLiquidationRewardsUsdc: '1750000',
        grossRewardsUsdc: '1750000',
      },
      actions,
      nextCursor: null,
    },
  }
}

function protocolAction(overrides: Partial<ProtocolAction>): ProtocolAction {
  return {
    actionId: 'action',
    transactionHash: `0x${'1'.repeat(64)}`,
    blockNumber: '123',
    blockHash: `0x${'2'.repeat(64)}`,
    transactionIndex: '0',
    logIndex: '1',
    timestamp: 1_785_000_000,
    actionType: 'order_execution',
    outcome: 'success',
    account: null,
    keeper: keeperAddress,
    orderId: null,
    contractAddress: `0x${'3'.repeat(40)}`,
    data: {},
    evidence: { level: 'exact' },
    units: {},
    ...overrides,
  }
}
