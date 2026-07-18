import { fireEvent, render, screen, within } from '@testing-library/react'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { PerpsTradeTicket } from '../PerpsTradeTicket'
import { DOCS_LINKS } from '../../config/docs'

let mockReadContractsData: readonly {
  status: 'failure' | 'success'
  result?: unknown
}[] | undefined

const perpsTradingMocks = vi.hoisted(() => ({
  cleanupExpiredOrder: vi.fn(),
  commitOrder: vi.fn(),
  depositMargin: vi.fn(),
  executeOrder: vi.fn(),
  withdrawMargin: vi.fn(),
}))

vi.mock('@reown/appkit/react', () => ({
  createAppKit: vi.fn(),
  useAppKit: () => ({
    open: vi.fn(),
  }),
  useAppKitNetwork: () => ({
    switchNetwork: vi.fn(),
  }),
}))

vi.mock('wagmi', () => ({
  http: vi.fn(() => ({})),
  useAccount: () => ({
    address: '0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B',
    isConnected: true,
  }),
  useChainId: () => 421614,
  useReadContracts: () => ({
    data: mockReadContractsData,
    isFetching: false,
    isLoading: false,
  }),
  useSimulateContract: () => ({
    error: null,
    isFetching: false,
    isLoading: false,
  }),
  useSwitchChain: () => ({
    switchChain: vi.fn(),
  }),
}))

vi.mock('../../hooks', () => ({
  usePerpsTrading: () => perpsTradingMocks,
  useSwitchToArbitrumSepolia: () => ({
    switchToArbitrumSepolia: vi.fn(),
    isSwitching: false,
    switchError: null,
    clearSwitchError: vi.fn(),
  }),
  waitForPerpsOrderTerminal: vi.fn(),
}))

const latestBasket = {
  timestamp: 1_700_000_000,
  basketPrice: '100000000',
  components: [{
    symbol: 'EUR/USD',
    feedSymbol: 'EUR/USD',
    feedId: '0xfeed',
    price: '100000000',
    rawPrice: '100000000',
    confidence: '10000',
    exponent: -8,
    publishTime: 1_700_000_000,
    inverted: false,
    weightBps: 10_000,
    basePrice: '100000000',
  }],
  generatedAt: 1_700_000_001,
  source: 'database' as const,
}

const currentPosition = {
  exists: true,
  side: 0,
  direction: 'long' as const,
  size: 1_000n * 10n ** 18n,
  entryPrice: 100_000_000n,
  marginUsdc: 500_000_000n,
  unrealizedPnlUsdc: 0n,
  maintenanceMarginUsdc: 10_000_000n,
  liquidatable: false,
  estimatedNotionalUsdc: 1_000_000_000n,
  entryNotionalUsdc: 1_000_000_000n,
  dxyExposureUsdc: 1_000_000_000n,
}

function closePreviewTuple({
  frozenSpreadUsdc = 0n,
  frozenSpreadPaidUsdc = 0n,
  frozenSpreadWaivedUsdc = 0n,
}: {
  frozenSpreadUsdc?: bigint
  frozenSpreadPaidUsdc?: bigint
  frozenSpreadWaivedUsdc?: bigint
} = {}) {
  return [
    true,
    0,
    100_000_000n,
    500n * 10n ** 18n,
    0n,
    -1_000_000n,
    0n,
    40_000n,
    0n,
    0n,
    0n,
    0n,
    0n,
    0n,
    0n,
    500n * 10n ** 18n,
    250_000_000n,
    false,
    false,
    1_000_000_000n,
    500_000_000n,
    frozenSpreadUsdc,
    frozenSpreadPaidUsdc,
    frozenSpreadWaivedUsdc,
  ] as const
}

function renderCloseTicket({
  marketPhase,
  oracleFrozen,
}: {
  marketPhase: 'close-only' | 'open'
  oracleFrozen: boolean
}) {
  return render(
    <PerpsTradeTicket
      enableLiveTrading
      initialReviewOpen
      initialDirection="short"
      initialSize="500"
      oraclePriceRaw={100_000_000n}
      oraclePriceDisplay="1.0000"
      latestBasket={latestBasket}
      adverseConfidenceMultiplierBps="10000"
      oracleFrozen={oracleFrozen}
      availableToTradeRaw={1_000_000_000n}
      currentPosition={currentPosition}
      marketPhase={marketPhase}
    />
  )
}

function commitPreviewQueries() {
  const preview = screen.getByText('Commit Preview').parentElement
  expect(preview).not.toBeNull()
  return within(preview!)
}

describe('perps ticket oracle regime matrix', () => {
  beforeEach(() => {
    mockReadContractsData = [{
      status: 'success',
      result: closePreviewTuple(),
    }]
  })

  it.each([
    ['live', 'open'],
    ['FAD-only', 'close-only'],
  ] as const)('retains adverse confidence for a %s close', (_regime, marketPhase) => {
    renderCloseTicket({
      marketPhase,
      oracleFrozen: false,
    })

    const preview = commitPreviewQueries()
    expect(preview.getByText('~0.0100%')).toBeInTheDocument()
    expect(preview.getByText('0.1%')).toBeInTheDocument()
    expect(preview.queryByText('Waived')).not.toBeInTheDocument()
    expect(preview.queryByText('Estimated frozen close spread')).not.toBeInTheDocument()

    fireEvent.focus(preview.getByLabelText('Adverse oracle confidence spread info'))
    expect(screen.getByRole('tooltip')).toHaveTextContent(
      'It applies to opens and to close/reduce execution in live and FAD-only regimes.'
    )
    expect(screen.getByRole('link', { name: `Read: ${DOCS_LINKS.oracleConfidence.title}` }))
      .toHaveAttribute('href', DOCS_LINKS.oracleConfidence.href)
  })

  it('explains the dollar-oriented direction controls', () => {
    renderCloseTicket({
      marketPhase: 'open',
      oracleFrozen: false,
    })

    fireEvent.focus(screen.getByLabelText('Direction info'))

    expect(screen.getByRole('tooltip')).toHaveTextContent(
      'LONG USD benefits when the displayed price rises; SHORT USD benefits when it falls.'
    )
    expect(screen.getByRole('link', { name: `Read: ${DOCS_LINKS.direction.title}` }))
      .toHaveAttribute('href', DOCS_LINKS.direction.href)
  })

  it.each([
    {
      label: 'Contract notional',
      message: "The protocol's accounting size, calculated using the raw basket price.",
      docsLink: DOCS_LINKS.contractNotional,
    },
    {
      label: 'Maintenance margin',
      message: 'At or below this amount, the entire position can be liquidated.',
      docsLink: DOCS_LINKS.maintenanceMargin,
    },
    {
      label: 'Execution limit',
      message: 'It does not limit VPI, fees, carry, execution rewards, or a frozen-close spread.',
      docsLink: DOCS_LINKS.executionLimit,
    },
    {
      label: 'Estimated execution reward',
      message: 'It can still be paid if the order fails or expires.',
      docsLink: DOCS_LINKS.executionReward,
    },
  ])('explains $label in the commit preview', ({ label, message, docsLink }) => {
    renderCloseTicket({
      marketPhase: 'open',
      oracleFrozen: false,
    })

    const preview = commitPreviewQueries()
    fireEvent.focus(preview.getByLabelText(`${label} info`))

    expect(screen.getByRole('tooltip')).toHaveTextContent(message)
    expect(screen.getByRole('link', { name: `Read: ${docsLink.title}` }))
      .toHaveAttribute('href', docsLink.href)
  })

  it('explains the cost of manual finalization when that action becomes available', () => {
    render(
      <PerpsTradeTicket
        initialLifecycleState="selfExecuteAvailable"
        initialReviewOpen
        initialDirection="long"
        initialSize="1 000"
        initialOrderId={42n}
      />
    )

    fireEvent.focus(screen.getByLabelText('Manual finalization info'))

    expect(screen.getByRole('tooltip')).toHaveTextContent(
      'Unless marked Sponsored, manual finalization requires ETH for network gas and the Pyth update fee.'
    )
    expect(screen.getByRole('link', { name: `Read: ${DOCS_LINKS.manualFinalization.title}` }))
      .toHaveAttribute('href', DOCS_LINKS.manualFinalization.href)
  })

  it('waives adverse confidence and shows the lens frozen spread for an oracle-frozen close', () => {
    mockReadContractsData = [{
      status: 'success',
      result: closePreviewTuple({
        frozenSpreadUsdc: 12_345_678n,
        frozenSpreadPaidUsdc: 10_000_000n,
        frozenSpreadWaivedUsdc: 2_345_678n,
      }),
    }]

    renderCloseTicket({
      marketPhase: 'close-only',
      oracleFrozen: true,
    })

    const preview = commitPreviewQueries()
    expect(preview.queryByText('Adverse oracle confidence spread')).not.toBeInTheDocument()
    expect(preview.queryByText('Waived')).not.toBeInTheDocument()
    expect(preview.queryByText('~0.0100%')).not.toBeInTheDocument()
    expect(preview.getByText('Estimated frozen close spread')).toBeInTheDocument()
    expect(preview.getByText('12.3')).toBeInTheDocument()
    expect(preview.getByText('Exact')).toBeInTheDocument()

    fireEvent.click(screen.getByRole('button', { name: /Max slippage/ }))
    expect(screen.getByRole('button', { name: 'Exact' })).toBeInTheDocument()
    expect(screen.getByRole('button', { name: '0.05%' })).toBeInTheDocument()
    expect(screen.getByRole('button', { name: '0.1%' })).toBeInTheDocument()
    expect(screen.getByRole('button', { name: '0.25%' })).toBeInTheDocument()
    expect(screen.getByRole('button', { name: 'Infinity' })).toBeInTheDocument()
    expect(screen.queryByRole('button', { name: '0.5%' })).not.toBeInTheDocument()

    fireEvent.focus(preview.getByLabelText('Estimated frozen close spread info'))
    const tooltip = screen.getByRole('tooltip')
    expect(tooltip).toHaveTextContent('use this fixed LP-owned spread instead of the adverse-confidence price shift')
    expect(tooltip).toHaveTextContent('protect LPs from price uncertainty')
    expect(tooltip).toHaveTextContent('Wait until the market reopens to avoid this spread')
    expect(tooltip).not.toHaveTextContent('full close')
    expect(tooltip).not.toHaveTextContent('separate')
    expect(screen.getByRole('link', { name: `Read: ${DOCS_LINKS.frozenCloseSpread.title}` }))
      .toHaveAttribute('href', DOCS_LINKS.frozenCloseSpread.href)
    expect(tooltip).not.toHaveTextContent('12.3')
    expect(tooltip).not.toHaveTextContent('10.0')
    expect(tooltip).not.toHaveTextContent('2.3')
  })
})
