import { fireEvent, render, screen, within } from '@testing-library/react'
import { MemoryRouter, Route, Routes } from 'react-router-dom'
import type { ProtocolAction, TrancheHistoryCheckpoint } from '../api'
import { buildTrancheHistoryCsv } from '../utils/trancheHistory'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { TranchePage } from './TranchePage'

const apiMocks = vi.hoisted(() => ({
  useCurrentProtocolRelease: vi.fn(),
  useTranche: vi.fn(),
  useTrancheHistory: vi.fn(),
}))

vi.mock('../api', () => apiMocks)

const deposit = protocolAction({
  actionId: 'deposit-1',
  actionType: 'tranche_deposit',
  orderId: '42',
  blockNumber: '10',
  timestamp: 1_785_000_000,
  data: {
    assets: '5000000',
    principalUsdc: '6000000',
    navUsdc: '6100000',
    assetsPerShare: '1010000',
    drawdownBps: '100',
    grossCoverageRatioBps: '12000',
  },
})

const unrelated = protocolAction({
  actionId: 'coupon-1',
  actionType: 'coupon_checkpoint',
  blockNumber: '11',
  timestamp: 1_785_000_100,
  data: {
    assets: '900000000',
    principalUsdc: '6200000',
    totalAssetsUsdc: '6300000',
    sharePriceUsdc: '1020000',
    drawdownBps: '75',
    coverageRatioBps: '12500',
  },
})

const withdraw = protocolAction({
  actionId: 'withdraw-1',
  actionType: 'tranche_withdraw',
  blockNumber: '12',
  timestamp: 1_785_000_200,
  data: {
    assets: '2000000',
    principalUsdc: '4000000',
    navUsdc: '4100000',
    assetsPerShare: '1005000',
    drawdownBps: '150',
    grossCoverageRatioBps: '11000',
  },
})

const snapshotCheckpoints = [
  trancheCheckpoint({
    blockNumber: '12',
    timestamp: 1_785_000_200,
    principalUsdc: '4000000',
    navUsdc: '4100000',
    shareSupply: '4000000000000000000',
    assetsPerShare: '1025000',
    drawdownUsdc: '1500000',
    coverageRatioBps: '11000',
  }),
  trancheCheckpoint({
    blockNumber: '11',
    timestamp: 1_785_000_100,
    principalUsdc: '6200000',
    navUsdc: '6300000',
    shareSupply: '6000000000000000000',
    assetsPerShare: '1050000',
    drawdownUsdc: '750000',
    coverageRatioBps: '12500',
  }),
  trancheCheckpoint({
    blockNumber: '10',
    timestamp: 1_785_000_000,
    principalUsdc: '6000000',
    navUsdc: '6100000',
    shareSupply: '6000000000000000000',
    assetsPerShare: '1016666',
    drawdownUsdc: '1000000',
    coverageRatioBps: '12000',
  }),
]

beforeEach(() => {
  vi.clearAllMocks()
  apiMocks.useCurrentProtocolRelease.mockReturnValue({
    data: { releaseId: 'release-1' },
    isLoading: false,
  })
  apiMocks.useTranche.mockReturnValue({
    data: {
      releaseId: 'release-1',
      chainId: '421614',
      confirmedBlock: {
        number: '123',
        hash: '0xblock',
        timestamp: 1_785_000_300,
      },
      indexerTimestamp: 1_785_000_310,
      calculationVersion: 'protocol-transparency-v1',
      evidence: { currentState: 'exact_historical_contract_read' },
      availability: [],
      tranche: {
        principalUsdc: '4000000',
        navUsdc: '4100000',
        assetsPerShare: '1005000',
        impairmentGapUsdc: '0',
      },
    },
    isError: false,
    isLoading: false,
  })
  apiMocks.useTrancheHistory.mockReturnValue(historyQuery([withdraw, unrelated, deposit]))
})

describe('TranchePage history charts', () => {
  it('builds chronological cumulative flows only from ERC-4626 deposit and withdrawal assets', () => {
    renderPage()

    const section = sectionNamed('Cumulative indexed net flows')
    const rows = within(section).getAllByRole('row')
    expect(rows).toHaveLength(3)
    expect(rows[1]).toHaveTextContent('5.00 USDC')
    expect(rows[1]).toHaveTextContent('tranche_deposit +data.assets')
    expect(rows[2]).toHaveTextContent('3.00 USDC')
    expect(rows[2]).toHaveTextContent('tranche_withdraw -data.assets')
    expect(section).not.toHaveTextContent('905.00 USDC')
  })

  it('renders sourced accessible visuals for every available history series', () => {
    renderPage()

    expect(screen.getByRole('img', { name: /Senior tranche principal/i })).toBeInTheDocument()
    expect(screen.getByRole('img', { name: /Senior tranche NAV/i })).toBeInTheDocument()
    expect(screen.getByRole('img', { name: /Senior tranche share price/i })).toBeInTheDocument()
    expect(screen.getByRole('img', { name: /Senior tranche drawdown/i })).toBeInTheDocument()
    expect(screen.getByRole('img', { name: /Senior tranche solvency coverage/i })).toBeInTheDocument()

    expect(sectionNamed('Principal and NAV history')).toHaveTextContent('history.checkpoints.navUsdc')
    expect(sectionNamed('Principal and NAV history')).toHaveTextContent('exact_historical_contract_read')
    expect(sectionNamed('Share-price history')).toHaveTextContent('history.checkpoints.assetsPerShare')
    expect(sectionNamed('Share-price history')).toHaveTextContent('derived_from_same_block_snapshots')
    expect(sectionNamed('Sparse checkpoint-page drawdown history')).toHaveTextContent('history.checkpoints.drawdownUsdc')
    expect(sectionNamed('Sparse checkpoint-page drawdown history')).toHaveTextContent('derived_from_sparse_range_end_snapshots')
    expect(sectionNamed('Sparse checkpoint-page drawdown history')).toHaveTextContent('not full-history')
    expect(sectionNamed('Sparse checkpoint-page drawdown history')).toHaveTextContent('instead of recomputing a continuous running peak')
    expect(sectionNamed('Solvency-coverage history')).toHaveTextContent('history.checkpoints.coverageRatioBps')
    expect(screen.getByRole('link', { name: 'Order #42' })).toHaveAttribute('href', '/orders/release-1/42')
  })

  it('sorts range-end snapshot checkpoints by block and exposes formula, scope, and block evidence', () => {
    renderPage()

    const navSection = sectionNamed('Principal and NAV history')
    const navText = navSection.textContent ?? ''
    expect(navText.indexOf('6.10 USDC')).toBeLessThan(navText.indexOf('6.30 USDC'))
    expect(navText.indexOf('6.30 USDC')).toBeLessThan(navText.indexOf('4.10 USDC'))
    expect(navSection).toHaveTextContent('tranche.senior')
    expect(navSection).toHaveTextContent(`0x${'2'.repeat(64)}`)

    const sharePriceSection = sectionNamed('Share-price history')
    expect(sharePriceSection).toHaveTextContent('formula protocol.tranche.range_end_checkpoint.v1')
    expect(sharePriceSection).toHaveTextContent('tranche totalAssetsUsdc * 1e18 / tranche totalSupply')
    expect(sharePriceSection).toHaveTextContent('USDC:6 per share:18')
  })

  it('shows a one-point table fallback without treating event assets as NAV', () => {
    apiMocks.useTrancheHistory.mockReturnValue(historyQuery([
      protocolAction({
        actionId: 'only-deposit',
        actionType: 'tranche_deposit',
        blockNumber: '20',
        data: { assets: '5000000' },
      }),
    ], []))

    renderPage()

    const flowSection = sectionNamed('Cumulative indexed net flows')
    expect(flowSection).toHaveTextContent('history unavailable')
    expect(flowSection).toHaveTextContent('One sourced checkpoint is available')
    expect(flowSection).toHaveTextContent('5.00 USDC')

    const navSection = sectionNamed('Principal and NAV history')
    expect(navSection).toHaveTextContent('Senior tranche NAV history unavailable')
    expect(navSection).toHaveTextContent("data.assets amount is not treated as tranche NAV")
    expect(within(navSection).queryByText('5.00 USDC')).not.toBeInTheDocument()
  })

  it('exports lifecycle actions and snapshot checkpoints as distinct, evidenced CSV rows', () => {
    const csv = buildTrancheHistoryCsv([deposit], [snapshotCheckpoints[0]])

    expect(csv).toContain('"recordType","timestamp","blockNumber","blockHash"')
    expect(csv).toContain('"action"')
    expect(csv).toContain('"tranche_deposit"')
    expect(csv).toContain('"snapshot_checkpoint"')
    expect(csv).toContain('"protocol.tranche.range_end_checkpoint.v1"')
    expect(csv).toContain('""scope"":""tranche.senior""')
    expect(csv).toContain('""navUsdc"":""exact_historical_contract_read""')
  })

  it('flattens independently sliced anchored pages, reports stream completion, and loads more history', () => {
    const fetchNextPage = vi.fn()
    const repeatedAvailability = {
      field: 'extendedLifecycleEvents',
      reason: 'current_release_event_decoder_unavailable',
    }
    apiMocks.useTrancheHistory.mockReturnValue({
      data: {
        pages: [
          historyPage({
            items: [deposit],
            checkpoints: [snapshotCheckpoints[2]],
            blockNumber: '777',
            availability: [repeatedAvailability],
            pagination: {
              actionsComplete: false,
              checkpointsComplete: false,
            },
            nextCursor: 'combined-page-2',
          }),
          historyPage({
            items: [withdraw],
            checkpoints: [snapshotCheckpoints[1], snapshotCheckpoints[0]],
            blockNumber: '999',
            availability: [
              repeatedAvailability,
              { field: 'beforeAfterState', reason: 'action_aligned_snapshots_unavailable' },
            ],
            pagination: {
              actionsComplete: false,
              checkpointsComplete: true,
            },
            nextCursor: 'combined-page-3',
          }),
        ],
      },
      isError: false,
      isLoading: false,
      hasNextPage: true,
      isFetchingNextPage: false,
      fetchNextPage,
      refetch: vi.fn(),
    })

    renderPage()

    const confirmedBlocks = screen.getAllByText('Confirmed block')
      .map((label) => label.parentElement?.textContent ?? '')
    expect(confirmedBlocks).toContain('Confirmed block 777')
    expect(confirmedBlocks).not.toContain('Confirmed block 999')
    expect(screen.getByText(/2 unique loaded actions and 3 unique loaded sparse checkpoints across 2 anchored API pages/)).toBeInTheDocument()
    const status = screen.getByLabelText('Tranche history load status')
    expect(status).toHaveTextContent('2 unique actions loaded · more action pages available')
    expect(status).toHaveTextContent('3 unique sparse checkpoints loaded · checkpoint pagination complete')
    expect(status).toHaveTextContent('does not make sparse checkpoints a continuous history')
    expect(screen.getByText('2 fields unavailable or incomplete')).toBeInTheDocument()
    expect(within(sectionNamed('Principal and NAV history')).getAllByText('6.30 USDC')).toHaveLength(1)

    fireEvent.click(screen.getByRole('button', { name: 'Load more tranche history' }))
    expect(fetchNextPage).toHaveBeenCalledOnce()
  })
})

function renderPage() {
  render(
    <MemoryRouter initialEntries={['/house-pool/senior']}>
      <Routes>
        <Route path="/house-pool/:tranche" element={<TranchePage />} />
      </Routes>
    </MemoryRouter>,
  )
}

function sectionNamed(name: string): HTMLElement {
  const heading = screen.getByRole('heading', { name })
  const section = heading.closest('section')
  if (section === null) throw new Error(`Section ${name} was not rendered`)
  return section
}

function historyQuery(
  items: ProtocolAction[],
  checkpoints: TrancheHistoryCheckpoint[] = snapshotCheckpoints,
) {
  return {
    data: {
      pages: [historyPage({ items, checkpoints })],
    },
    isError: false,
    isLoading: false,
    hasNextPage: false,
    isFetchingNextPage: false,
    fetchNextPage: vi.fn(),
    refetch: vi.fn(),
  }
}

function historyPage({
  items,
  checkpoints,
  blockNumber = '123',
  availability = [],
  nextCursor = null,
  pagination = {
    actionsComplete: true,
    checkpointsComplete: true,
  },
}: {
  items: ProtocolAction[]
  checkpoints: TrancheHistoryCheckpoint[]
  blockNumber?: string
  availability?: { field: string; reason: string }[]
  nextCursor?: string | null
  pagination?: {
    actionsComplete: boolean
    checkpointsComplete: boolean
  }
}) {
  return {
    releaseId: 'release-1',
    chainId: '421614',
    confirmedBlock: {
      number: blockNumber,
      hash: `0x${blockNumber.padStart(64, '0')}`,
      timestamp: 1_785_000_300,
    },
    indexerTimestamp: 1_785_000_310,
    calculationVersion: 'protocol-transparency-v1',
    evidence: { history: 'confirmed_log_actions' },
    availability,
    history: {
      tranche: 'senior',
      items,
      nextCursor,
      nextCursors: {
        combined: nextCursor,
        actions: pagination.actionsComplete ? null : `${nextCursor ?? 'action'}:actions`,
        checkpoints: pagination.checkpointsComplete ? null : `${nextCursor ?? 'checkpoint'}:checkpoints`,
      },
      pagination,
      checkpoints,
      csvColumns: [],
    },
  }
}

function trancheCheckpoint(
  overrides: Partial<TrancheHistoryCheckpoint>,
): TrancheHistoryCheckpoint {
  return {
    blockNumber: '1',
    blockHash: `0x${'2'.repeat(64)}`,
    timestamp: 1_785_000_000,
    principalUsdc: null,
    navUsdc: null,
    shareSupply: null,
    assetsPerShare: null,
    drawdownUsdc: null,
    impairmentGapUsdc: null,
    coverageRatioBps: null,
    calculationVersion: 'protocol-transparency-v1',
    formulaIdentifier: 'protocol.tranche.range_end_checkpoint.v1',
    formula: {
      assetsPerShare: 'tranche totalAssetsUsdc * 1e18 / tranche totalSupply',
      drawdownUsdc: 'maximum observed tranche NAV through this sparse checkpoint minus current tranche NAV',
      coverageRatioBps: 'HousePool accounted totalAssetsUsdc * 10000 / max(LONG maxProfitUsdc, SHORT maxProfitUsdc)',
    },
    evidence: {
      principalUsdc: 'exact_historical_contract_read',
      navUsdc: 'exact_historical_contract_read',
      shareSupply: 'exact_historical_contract_read',
      assetsPerShare: 'derived_from_same_block_snapshots',
      drawdownUsdc: 'derived_from_sparse_range_end_snapshots',
      coverageRatioBps: 'derived_from_same_block_snapshots',
    },
    sourceScopes: [
      { scope: 'tranche.senior', blockNumber: '1', blockHash: `0x${'2'.repeat(64)}` },
      { scope: 'house-pool.liquidity', blockNumber: '1', blockHash: `0x${'2'.repeat(64)}` },
      { scope: 'market.long', blockNumber: '1', blockHash: `0x${'2'.repeat(64)}` },
      { scope: 'market.short', blockNumber: '1', blockHash: `0x${'2'.repeat(64)}` },
    ],
    availability: [],
    units: {
      principalUsdc: 'USDC:6',
      navUsdc: 'USDC:6',
      shareSupply: 'shares:18',
      assetsPerShare: 'USDC:6 per share:18',
      drawdownUsdc: 'USDC:6',
      coverageRatioBps: 'basis_points',
    },
    ...overrides,
  }
}

function protocolAction(overrides: Partial<ProtocolAction>): ProtocolAction {
  return {
    actionId: 'action-1',
    transactionHash: `0x${'1'.repeat(64)}`,
    blockNumber: '1',
    blockHash: `0x${'2'.repeat(64)}`,
    transactionIndex: '0',
    logIndex: '0',
    timestamp: 1_785_000_000,
    actionType: 'unknown',
    outcome: 'success',
    account: null,
    keeper: null,
    orderId: null,
    contractAddress: `0x${'3'.repeat(40)}`,
    data: {},
    evidence: { level: 'exact' },
    units: {},
    ...overrides,
  }
}
