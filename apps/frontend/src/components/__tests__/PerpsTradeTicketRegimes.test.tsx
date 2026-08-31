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

vi.mock('../../perps-aa', async (importOriginal) => {
  const actual = await importOriginal<typeof import('../../perps-aa')>()
  const address = '0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B'
  return {
    ...actual,
    usePerpsIdentity: () => ({
      status: 'ready',
      ownerAddress: address,
      accountAddress: address,
      chainId: 421614,
      isAaManifestConfigured: false,
      sponsorshipEnabled: false,
      manifest: null,
      identity: null,
      proposedIdentity: null,
      changedIdentityFields: [],
      error: null,
      confirmIdentityAfterContinuityCheck: () => false,
      reloadIdentity: () => undefined,
    }),
  }
})

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
  vpiAccrued: 60_000_000n,
}

function closePreviewTuple({
  vpiDeltaUsdc = -1_000_000n,
  frozenSpreadUsdc = 0n,
  frozenSpreadPaidUsdc = 0n,
  frozenSpreadWaivedUsdc = 0n,
}: {
  vpiDeltaUsdc?: bigint
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
    vpiDeltaUsdc,
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

function closeTicket({
  enableLiveTrading = true,
  lifecycleState = 'preview',
  marketPhase,
  oraclePriceRaw = 100_000_000n,
  oracleFrozen,
  finalVpiUsdc,
  positionVpiAccrued = 60_000_000n,
  showCurrentPosition = true,
  size = '500',
}: {
  enableLiveTrading?: boolean
  lifecycleState?: 'executed' | 'preview'
  marketPhase: 'close-only' | 'open'
  oraclePriceRaw?: bigint
  oracleFrozen: boolean
  finalVpiUsdc?: bigint
  positionVpiAccrued?: bigint
  showCurrentPosition?: boolean
  size?: string
}) {
  return (
    <PerpsTradeTicket
      enableLiveTrading={enableLiveTrading}
      initialReviewOpen
      initialLifecycleState={lifecycleState}
      initialDirection="short"
      initialReduceOnly
      initialSize={size}
      initialOrderId={42n}
      initialCommittedSizeDelta={500n * 10n ** 18n}
      initialCommittedIsFullClose={lifecycleState === 'executed'}
      initialCommittedPositionVpiAccrued={
        lifecycleState === 'executed' ? positionVpiAccrued : undefined
      }
      initialFinalExecutionPrice={
        lifecycleState === 'executed' ? 100_000_000n : undefined
      }
      initialFinalExecutionOraclePrice={
        lifecycleState === 'executed' ? oraclePriceRaw : undefined
      }
      initialFinalExecutionOracleFrozen={
        lifecycleState === 'executed' ? oracleFrozen : undefined
      }
      initialFinalFrozenCloseSpreadUsdc={
        lifecycleState === 'executed' && oracleFrozen ? 12_345_678n : undefined
      }
      initialFinalExecutionEconomicsVersion={
        lifecycleState === 'executed' ? 1 : undefined
      }
      initialFinalVpiUsdc={
        lifecycleState === 'executed' ? finalVpiUsdc : undefined
      }
      oraclePriceRaw={oraclePriceRaw}
      oraclePriceDisplay="1.0000"
      latestBasket={latestBasket}
      adverseConfidenceMultiplierBps="10000"
      oracleFrozen={oracleFrozen}
      availableToTradeRaw={1_000_000_000n}
      currentPosition={showCurrentPosition
        ? { ...currentPosition, vpiAccrued: positionVpiAccrued }
        : undefined}
      marketPhase={marketPhase}
    />
  )
}

function renderCloseTicket(
  input: Parameters<typeof closeTicket>[0]
) {
  return render(closeTicket(input))
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

    const oracleConfidenceLabel = preview.getByText('Adverse oracle confidence spread')
    expect(oracleConfidenceLabel).toHaveClass('overflow-hidden', 'text-ellipsis', 'whitespace-nowrap')
    expect(oracleConfidenceLabel).toHaveAttribute('title', 'Adverse oracle confidence spread')

    fireEvent.focus(preview.getByLabelText('Adverse oracle confidence spread info'))
    expect(screen.getByRole('tooltip')).toHaveTextContent(
      'It applies to opens and to close/reduce execution in live and FAD-only regimes.'
    )
    expect(screen.getByRole('link', { name: `Read: ${DOCS_LINKS.oracleConfidence.title}` }))
      .toHaveAttribute('href', DOCS_LINKS.oracleConfidence.href)
  })

  it.each([
    {
      caseName: 'net-paid balance',
      positionVpiAccrued: 60_000_000n,
      positionBalance: 'Net paid 60.0 USDC',
    },
    {
      caseName: 'existing provisional credit',
      positionVpiAccrued: -40_000_000n,
      positionBalance: 'Provisional credit 40.0 USDC',
    },
  ])('shows the position VPI balance for a $caseName', ({
    positionVpiAccrued,
    positionBalance,
  }) => {
    renderCloseTicket({
      marketPhase: 'open',
      oracleFrozen: false,
      positionVpiAccrued,
    })

    const preview = commitPreviewQueries()
    expect(preview.getByLabelText(positionBalance)).toBeInTheDocument()
    expect(preview.queryByText('VPI allocated to reduction')).not.toBeInTheDocument()
    expect(preview.queryByText('Maximum eligible VPI credit')).not.toBeInTheDocument()

    const positionVpiLabel = preview.getByText('Position VPI balance')
    const positionVpiRow = positionVpiLabel.closest('div')
    expect(positionVpiRow).toHaveClass('flex-nowrap')
    expect(positionVpiLabel).toHaveClass('overflow-hidden', 'text-ellipsis', 'whitespace-nowrap')
    expect(positionVpiLabel).toHaveAttribute('title', 'Position VPI balance')
    expect(positionVpiRow?.querySelector('dd')).toHaveClass('whitespace-nowrap')

    const positionVpiInfo = preview.getByLabelText('Position VPI balance info')
    fireEvent.focus(positionVpiInfo)
    expect(screen.getByRole('tooltip')).toHaveTextContent(
      'A provisional credit has already been added to settlement, remains excluded from risk equity'
    )
  })

  it.each([
    ['charge', 1_250_000n, 'Pay 1.3 USDC'],
    ['credit', -1_250_000n, 'Credit 1.3 USDC'],
    ['zero adjustment', 0n, 'No VPI'],
  ] as const)('shows an estimated close VPI %s as an account action', (_case, vpiDeltaUsdc, expected) => {
    mockReadContractsData = [{
      status: 'success',
      result: closePreviewTuple({ vpiDeltaUsdc }),
    }]

    renderCloseTicket({
      marketPhase: 'open',
      oracleFrozen: false,
    })

    expect(screen.getAllByText('VPI')).toHaveLength(2)
    const preview = commitPreviewQueries()
    const vpiRow = preview.getByText('VPI').closest('div')
    if (vpiDeltaUsdc === 0n) {
      expect(vpiRow?.querySelector('dd')).toHaveTextContent(expected)
    } else {
      expect(within(vpiRow!).getByLabelText(expected)).toBeInTheDocument()
    }

    fireEvent.focus(preview.getByLabelText('VPI info'))
    expect(screen.getByRole('tooltip')).toHaveTextContent(
      'positive VPI is paid from the Margin Account and negative VPI is credited to the Margin Account settlement'
    )
    expect(screen.getByRole('tooltip')).toHaveTextContent(
      'not sent directly to the owner wallet'
    )
  })

  it('keeps close intent after a full-close refresh removes the live position', () => {
    const input = {
      enableLiveTrading: false,
      marketPhase: 'open' as const,
      oracleFrozen: false,
      size: '1000',
    }
    const { rerender } = renderCloseTicket(input)

    fireEvent.click(screen.getByRole('button', { name: 'Confirm Commit' }))
    rerender(closeTicket({ ...input, showCurrentPosition: false }))
    fireEvent.click(screen.getByRole('button', { name: 'Transaction Confirmed' }))
    fireEvent.click(screen.getByRole('button', { name: 'Auto Finalized' }))

    const finalResult = screen.getByText('Final Result').parentElement
    expect(finalResult).not.toBeNull()
    expect(screen.getByRole('button', { name: 'Replay celebration confetti' }))
      .toHaveTextContent('Long plDXY Perp position closed')
    expect(within(finalResult!).getByText('Target close exposure')).toBeInTheDocument()
    expect(within(finalResult!).getByText('Executed close exposure')).toBeInTheDocument()
    expect(within(finalResult!).queryByText('Margin posted')).not.toBeInTheDocument()
    expect(within(finalResult!).getByText('Position VPI before close')).toBeInTheDocument()
    expect(within(finalResult!).getByLabelText('Net paid 60.0 USDC')).toBeInTheDocument()
    expect(within(finalResult!).getByText('VPI')).toBeInTheDocument()
    expect(within(finalResult!).queryByText('VPI / Price impact')).not.toBeInTheDocument()
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
    expect(screen.getByRole('tooltip')).toHaveClass(
      'w-[320px]',
      'max-w-[calc(100vw-2rem)]',
      'whitespace-normal'
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
    expect(preview.queryByRole('button', { name: /Max slippage/ })).not.toBeInTheDocument()

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

  it('switches an already-mounted close to Exact when the oracle becomes frozen', () => {
    const liveInput = {
      marketPhase: 'close-only' as const,
      oracleFrozen: false,
    }
    const { rerender } = renderCloseTicket(liveInput)
    let preview = commitPreviewQueries()

    fireEvent.click(screen.getByRole('button', { name: /Max slippage/ }))
    fireEvent.click(screen.getByRole('button', { name: '0.25%' }))
    expect(preview.getByText('0.25%')).toBeInTheDocument()

    rerender(closeTicket({
      ...liveInput,
      oracleFrozen: true,
    }))
    preview = commitPreviewQueries()

    expect(preview.getByText('Exact')).toBeInTheDocument()
    expect(preview.queryByText('0.25%')).not.toBeInTheDocument()
    expect(preview.queryByRole('button', { name: /Max slippage/ })).not.toBeInTheDocument()

    rerender(closeTicket(liveInput))
    preview = commitPreviewQueries()

    expect(preview.getByText('0.25%')).toBeInTheDocument()
    expect(screen.getByRole('button', { name: /Max slippage/ })).toBeInTheDocument()
  })

  it('shows the frozen spread instead of an oracle confidence spread after execution', () => {
    mockReadContractsData = [{
      status: 'success',
      result: closePreviewTuple({
        frozenSpreadUsdc: 12_345_678n,
        frozenSpreadPaidUsdc: 10_000_000n,
        frozenSpreadWaivedUsdc: 2_345_678n,
      }),
    }]

    renderCloseTicket({
      lifecycleState: 'executed',
      marketPhase: 'close-only',
      oracleFrozen: true,
    })

    const finalResult = screen.getByText('Final Result').parentElement
    expect(finalResult).not.toBeNull()
    expect(within(finalResult!).queryByText(/Oracle confidence spread/i))
      .not.toBeInTheDocument()
    expect(within(finalResult!).getByText('Frozen close spread'))
      .toBeInTheDocument()
    expect(within(finalResult!).queryByText(/Estimated/i)).not.toBeInTheDocument()
    expect(within(finalResult!).getByText('12.3')).toBeInTheDocument()
  })

  it.each([
    ['charge', 12_345_678n, 'Paid 12.3 USDC'],
    ['credit', -12_345_678n, 'Credited 12.3 USDC'],
    ['zero adjustment', 0n, 'No VPI'],
  ] as const)('shows settled close VPI as a %s', (_case, finalVpiUsdc, expected) => {
    renderCloseTicket({
      lifecycleState: 'executed',
      marketPhase: 'open',
      oracleFrozen: false,
      finalVpiUsdc,
    })

    const finalResult = screen.getByText('Final Result').parentElement
    expect(finalResult).not.toBeNull()
    expect(screen.getByRole('button', { name: 'Replay celebration confetti' }))
      .toHaveTextContent('Long plDXY Perp position closed at 1.0000 USDC')
    expect(within(finalResult!).getByText('Position side')).toBeInTheDocument()
    expect(within(finalResult!).getByText('Long plDXY Perp')).toBeInTheDocument()
    expect(within(finalResult!).getByText('Target close exposure')).toBeInTheDocument()
    expect(within(finalResult!).getByText('Executed close exposure')).toBeInTheDocument()
    expect(within(finalResult!).queryByText('Margin posted')).not.toBeInTheDocument()
    expect(within(finalResult!).getByText('Position VPI before close')).toBeInTheDocument()
    expect(within(finalResult!).getByLabelText('Net paid 60.0 USDC')).toBeInTheDocument()
    const vpiRow = within(finalResult!).getByText('VPI').closest('div')
    if (finalVpiUsdc === 0n) {
      expect(vpiRow?.querySelector('dd')).toHaveTextContent(expected)
    } else {
      expect(within(vpiRow!).getByLabelText(expected)).toBeInTheDocument()
    }

    fireEvent.focus(within(finalResult!).getByLabelText('VPI info'))
    expect(screen.getByRole('tooltip')).toHaveTextContent(
      'credited VPI was added to the Margin Account settlement'
    )
  })

  it('keeps the committed slippage and execution limit when the live regime changes', () => {
    const frozenInput = {
      enableLiveTrading: false,
      marketPhase: 'close-only' as const,
      oracleFrozen: true,
      oraclePriceRaw: 100_000_000n,
    }
    const { rerender } = renderCloseTicket(frozenInput)

    fireEvent.click(screen.getByRole('button', { name: 'Confirm Commit' }))

    const dialog = screen.getByRole('dialog')
    expect(within(dialog).getByText('Waiting for wallet confirmation')).toBeInTheDocument()
    expect(within(dialog).getByText('Exact')).toBeInTheDocument()
    const committedLimit = within(dialog).getByText('Execution limit')
      .closest('div')?.querySelector('dd')?.textContent
    expect(committedLimit).toBeTruthy()

    rerender(closeTicket({
      ...frozenInput,
      marketPhase: 'open',
      oracleFrozen: false,
      oraclePriceRaw: 110_000_000n,
    }))

    expect(within(dialog).getByText('Exact')).toBeInTheDocument()
    expect(within(dialog).queryByText('0.1%')).not.toBeInTheDocument()
    expect(
      within(dialog).getByText('Execution limit')
        .closest('div')?.querySelector('dd')?.textContent
    ).toBe(committedLimit)
  })
})
