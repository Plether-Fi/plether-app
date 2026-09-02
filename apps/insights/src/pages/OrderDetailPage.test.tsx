import { render, screen, within } from '@testing-library/react'
import { MemoryRouter, Route, Routes } from 'react-router-dom'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { OrderDetailPage } from './OrderDetailPage'

const apiMocks = vi.hoisted(() => ({
  useCurrentProtocolRelease: vi.fn(),
  useProtocolOrder: vi.fn(),
}))

vi.mock('../api', () => apiMocks)

const commitmentHash = `0x${'1'.repeat(64)}`
const terminalHash = `0x${'2'.repeat(64)}`

beforeEach(() => {
  vi.clearAllMocks()
  apiMocks.useCurrentProtocolRelease.mockReturnValue({
    data: { releaseId: 'release-1' },
    isError: false,
    isLoading: false,
  })
  apiMocks.useProtocolOrder.mockReturnValue({
    data: orderResponse(),
    isError: false,
    isLoading: false,
  })
})

describe('OrderDetailPage', () => {
  it('renders the commitment, reveal range, terminal lifecycle, and canonical transaction links', () => {
    renderPage('/orders/release-archive/42')

    expect(apiMocks.useProtocolOrder).toHaveBeenCalledWith('release-archive', '42')
    expect(screen.getByRole('heading', { name: 'Order #42' })).toBeInTheDocument()

    const commitment = sectionNamed('Commitment')
    expect(rowNamed(commitment, 'Timestamp')).toHaveTextContent('1000')
    expect(rowNamed(commitment, 'Max Order Age Seconds')).toHaveTextContent('60')
    expect(rowNamed(commitment, 'Expiry Timestamp')).toHaveTextContent('1060')

    const reveal = sectionNamed('Reveal')
    expect(rowNamed(reveal, 'First Eligible Timestamp')).toHaveTextContent('1001')
    expect(rowNamed(reveal, 'Last Eligible Timestamp')).toHaveTextContent('1015')
    expect(rowNamed(reveal, 'Settlement Window Seconds')).toHaveTextContent('15')

    const terminal = sectionNamed('Terminal')
    expect(rowNamed(terminal, 'Status')).toHaveTextContent('executed')
    expect(rowNamed(terminal, 'Commit To Terminal Latency Seconds')).toHaveTextContent('13')
    expect(rowNamed(terminal, 'Reveal Ready To Terminal Latency Seconds')).toHaveTextContent('12')

    expect(screen.getByRole('link', { name: commitmentHash })).toHaveAttribute(
      'href',
      `/transactions/${commitmentHash}?release=release-archive`,
    )
    expect(screen.getByRole('link', { name: terminalHash })).toHaveAttribute(
      'href',
      `/transactions/${terminalHash}?release=release-archive`,
    )
  })

  it('labels before/after account and pool snapshots as block-level state impact', () => {
    renderPage('/orders/release-archive/42')

    const position = sectionNamed('Position before / after')
    expect(position).toHaveTextContent('terminal block − 1 and terminal block')
    expect(position).toHaveTextContent('labelled block-level')
    const sizes = within(position).getAllByText('Size')
    expect(sizes).toHaveLength(2)
    expect(sizes[0].closest('tr')).toHaveTextContent('10')
    expect(sizes[1].closest('tr')).toHaveTextContent('15')
    expect(within(position).getByText('After terminal block')).toBeInTheDocument()

    const housePool = sectionNamed('HousePool before / after')
    expect(housePool).toHaveTextContent('Accounting delta at block granularity')
    const freeUsdc = within(housePool).getAllByText('Free Usdc')
    expect(freeUsdc).toHaveLength(2)
    expect(freeUsdc[0].closest('tr')).toHaveTextContent('8.00 USDC')
    expect(freeUsdc[1].closest('tr')).toHaveTextContent('6.00 USDC')
    expect(within(housePool).getByText('After terminal block')).toBeInTheDocument()
  })

  it('renders the observed position change and keeper receipt economics with their evidence', () => {
    renderPage('/orders/release-archive/42')

    const positionChange = sectionNamed('Observed position change')
    expect(rowNamed(positionChange, 'Activity Type')).toHaveTextContent('Position Increased')
    expect(rowNamed(positionChange, 'Size Delta')).toHaveTextContent('5000000000000000000')
    expect(positionChange).toHaveTextContent('exact_confirmed_log_projection')

    const keeperEconomics = sectionNamed('Keeper transaction economics')
    expect(rowNamed(keeperEconomics, 'Gas Cost Wei')).toHaveTextContent('42000000000000')
    expect(rowNamed(keeperEconomics, 'Transaction Native Value Wei')).toHaveTextContent('1000000000000')
    expect(rowNamed(keeperEconomics, 'Provenance')).toHaveTextContent('exact_transaction_receipt')
    expect(rowNamed(keeperEconomics, 'Provenance')).toHaveTextContent('pythFeeWei')
    expect(rowNamed(keeperEconomics, 'Provenance')).toHaveTextContent('unavailable')
  })

  it('keeps archive-release deep links independent from current-release loading', () => {
    apiMocks.useCurrentProtocolRelease.mockReturnValue({
      data: undefined,
      isError: false,
      isLoading: true,
    })

    renderPage('/orders/release-archive/42')

    expect(apiMocks.useProtocolOrder).toHaveBeenCalledWith('release-archive', '42')
    expect(screen.getByRole('heading', { name: 'Order #42' })).toBeInTheDocument()
    expect(screen.queryByLabelText('Loading')).not.toBeInTheDocument()
  })

  it('resolves the current-release order alias before querying the lifecycle', () => {
    renderPage('/orders/current/42')

    expect(apiMocks.useProtocolOrder).toHaveBeenCalledWith('release-1', '42')
    expect(screen.getByRole('heading', { name: 'Order #42' })).toBeInTheDocument()
  })

  it('renders missing lifecycle evidence, snapshots, analysis, and transactions as unavailable', () => {
    apiMocks.useProtocolOrder.mockReturnValue({
      data: orderResponse({
        lifecycle: {
          commitment: {},
          reveal: {},
          terminal: {},
        },
        stateImpact: {
          position: {
            before: null,
            after: null,
          },
          housePool: {
            before: null,
            after: null,
          },
          provenance: 'unavailable',
        },
        economics: {},
        positionChange: null,
        keeperEconomics: null,
        liquidation: null,
        actions: [],
        transactions: {
          commitment: null,
          terminal: {},
        },
      }, [{
        field: 'reveal.pythComponentPublishTimes',
        reason: 'current_release_telemetry_missing',
      }, {
        field: 'stateImpact.position.before',
        reason: 'archive_state_unavailable',
      }]),
      isError: false,
      isLoading: false,
    })

    renderPage('/orders/release-archive/42')

    expect(screen.getByText('2 fields unavailable or incomplete')).toBeInTheDocument()
    expect(screen.getByText((_, element) =>
      element?.tagName === 'LI'
      && element.textContent?.includes('reveal.pythComponentPublishTimes') === true
      && element.textContent?.includes('Current Release Telemetry Missing') === true,
    )).toBeInTheDocument()
    expect(screen.getByText((_, element) =>
      element?.tagName === 'LI'
      && element.textContent?.includes('stateImpact.position.before') === true
      && element.textContent?.includes('Archive State Unavailable') === true,
    )).toBeInTheDocument()
    expect(screen.getAllByText('No data is available for this section.')).toHaveLength(4)
    expect(screen.getByText('Archive position state before the terminal block is unavailable.')).toBeInTheDocument()
    expect(screen.getByText('Archive position state after the terminal block is unavailable.')).toBeInTheDocument()
    expect(screen.getByText('Archive pool state before the terminal block is unavailable.')).toBeInTheDocument()
    expect(screen.getByText('Archive pool state after the terminal block is unavailable.')).toBeInTheDocument()
    expect(screen.getByText('No position-changing terminal activity was observed for this order.')).toBeInTheDocument()
    expect(screen.getByText('The terminal keeper transaction or its receipt economics are unavailable.')).toBeInTheDocument()
    expect(screen.getByText('This order did not terminate through liquidation.')).toBeInTheDocument()
    expect(screen.getByText('No matching successful or terminal onchain actions are indexed for this selection.')).toBeInTheDocument()
    expect(within(sectionNamed('Canonical transactions')).getAllByText('Unavailable')).toHaveLength(2)
  })
})

function renderPage(path: string) {
  return render(
    <MemoryRouter initialEntries={[path]}>
      <Routes>
        <Route path="/orders/:releaseId/:orderId" element={<OrderDetailPage />} />
      </Routes>
    </MemoryRouter>,
  )
}

function sectionNamed(name: string): HTMLElement {
  const heading = screen.getByRole('heading', { name })
  const section = heading.closest('section')
  if (!section) throw new Error(`Section ${name} not found`)
  return section
}

function rowNamed(section: HTMLElement, name: string): HTMLElement {
  const heading = within(section).getByText(name)
  const row = heading.closest('tr')
  if (!row) throw new Error(`Row ${name} not found`)
  return row
}

function orderResponse(
  orderOverrides: Record<string, unknown> = {},
  availability: { field: string; reason: string }[] = [],
) {
  return {
    releaseId: 'release-archive',
    chainId: '421614',
    confirmedBlock: {
      number: '123',
      hash: `0x${'3'.repeat(64)}`,
      timestamp: 1_785_000_000,
    },
    indexerTimestamp: 1_785_000_010,
    calculationVersion: 'protocol-transparency-v1',
    evidence: {
      lifecycle: 'exact_logs_plus_derived_latency',
      positionChange: orderOverrides.positionChange === null
        ? 'unavailable'
        : 'exact_confirmed_log_projection',
      stateImpact: 'block-level delta',
    },
    availability,
    order: {
      orderId: '42',
      lifecycle: {
        commitment: {
          transactionHash: commitmentHash,
          blockNumber: '100',
          timestamp: 1000,
          account: '0x4444444444444444444444444444444444444444',
          maxOrderAgeSeconds: '60',
          expiryTimestamp: 1060,
          evidence: {
            event: 'exact',
            expiryTimestamp: 'derived',
          },
        },
        reveal: {
          firstEligibleTimestamp: 1001,
          lastEligibleTimestamp: 1015,
          settlementWindowSeconds: '15',
          pythComponentPublishTimes: null,
          evidence: {
            firstEligibleTimestamp: 'derived',
            lastEligibleTimestamp: 'derived',
            pythComponentPublishTimes: 'unavailable',
          },
        },
        terminal: {
          transactionHash: terminalHash,
          blockNumber: '102',
          timestamp: 1013,
          status: 'executed',
          keeper: '0x5555555555555555555555555555555555555555',
          commitToTerminalLatencySeconds: '13',
          revealReadyToTerminalLatencySeconds: '12',
        },
      },
      stateImpact: {
        position: {
          before: {
            size: '10',
            marginUsdc: '10000000',
          },
          after: {
            size: '15',
            marginUsdc: '12000000',
          },
        },
        housePool: {
          before: {
            freeUsdc: '8000000',
            traderClaimsUsdc: '1000000',
          },
          after: {
            freeUsdc: '6000000',
            traderClaimsUsdc: '3000000',
          },
        },
        sourceBlocks: {
          before: { number: '101' },
          after: { number: '102' },
        },
        provenance: 'block-level delta',
      },
      positionChange: {
        activityType: 'Position Increased',
        sizeDelta: '5000000000000000000',
        price: '250000000000',
        pnlUsdc: '1250000',
        evidence: {
          activityType: 'exact_confirmed_log_projection',
          sizeDelta: 'exact_confirmed_log_projection',
          price: 'exact_confirmed_log_projection',
          pnlUsdc: 'exact_confirmed_log_projection',
        },
        units: {
          sizeDelta: 'position:18',
          price: 'indexPrice:8',
          pnlUsdc: 'USDC:6',
        },
      },
      keeperEconomics: {
        gasCostWei: '42000000000000',
        transactionNativeValueWei: '1000000000000',
        pythFeeWei: null,
        profitUsdc: null,
        formulaIdentifier: 'protocol.keeper.transaction_native_cost.v1',
        provenance: {
          gasCostWei: 'exact_transaction_receipt',
          transactionNativeValueWei: 'exact_transaction_input',
          pythFeeWei: 'unavailable',
          profitUsdc: 'unavailable',
        },
        units: {
          gasCostWei: 'wei',
          transactionNativeValueWei: 'wei',
          pythFeeWei: 'wei',
          profitUsdc: 'USDC:6',
        },
      },
      economics: {
        executionRewardUsdc: '500000',
      },
      liquidation: null,
      actions: [],
      transactions: {
        commitment: {
          transactionHash: commitmentHash,
        },
        terminal: {
          transactionHash: terminalHash,
        },
      },
      ...orderOverrides,
    },
  }
}
