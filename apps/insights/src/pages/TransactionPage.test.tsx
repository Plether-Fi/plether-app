import { fireEvent, render, screen, within } from '@testing-library/react'
import { MemoryRouter, Route, Routes } from 'react-router-dom'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { InsightsApiError, type ProtocolTradeRecord, type TransactionDetailsResponse, type TransactionRouteState } from '../api'
import { TransactionPage } from './TransactionPage'

const txQuery = vi.fn()
const walletExposureQuery = vi.fn()

vi.mock('../api', async () => {
  const actual = await vi.importActual<typeof import('../api')>('../api')
  return {
    ...actual,
    useTransaction: (...args: unknown[]) => txQuery(...args),
    useWalletExposure: (...args: unknown[]) => walletExposureQuery(...args),
  }
})

vi.mock('../hooks/useProtocolContext', () => ({
  useProtocolContext: () => ({
    chainId: 421614,
    releaseVersion: 'v2.0.0',
    latestBlock: '332919707',
    latestIndexedAt: '2026-11-26T15:34:00Z',
    healthy: true,
    isLoading: false,
    isError: false,
  }),
}))

const sampleTxHash = `0x${'a'.repeat(64)}`
const sampleAccount = `0x${'1'.repeat(40)}`
const sampleKeeper = `0x${'9'.repeat(40)}`

const closeEvent: ProtocolTradeRecord = {
  id: `${sampleTxHash}:14`,
  activityType: 'Close',
  account: sampleAccount,
  orderId: '501',
  side: 'long',
  price: '101.25',
  sizeDelta: '500000000000000000000',
  amountAssets: '50000000000',
  pnlAssets: '1250000000',
  vpiAssets: '1250000',
  frozenCloseSpreadAssets: null,
  oracleFrozen: false,
  latencySeconds: 3,
  keeperBountyAssets: null,
  execution: {
    orderId: '501',
    protocolVersion: 'v1.2.3',
    status: 'complete',
    receipt: {
      realizedPnlAssets: '1250000000',
      executionFeeAssets: '2000000',
      vpiAssets: '1250000',
      carryAssets: '0',
      frozenSpreadAssets: '0',
      executionBountyAssets: '100000',
      preSettlementBalanceAssets: '10000000000',
      postSettlementBalanceAssets: '16246650000',
      preTraderClaimBalanceAssets: '0',
      postTraderClaimBalanceAssets: '0',
      executionNotionalAssets: '50625000000',
      postPositionSize: '0',
      postPositionMarginAssets: '0',
    },
    settlements: [],
  },
  txHash: sampleTxHash,
  blockNumber: '332919707',
  occurredAt: '2026-11-26T15:34:00Z',
}

const openEvent: ProtocolTradeRecord = {
  ...closeEvent,
  id: `${sampleTxHash}:8`,
  activityType: 'Open',
  orderId: '500',
  pnlAssets: null,
  execution: {
    ...closeEvent.execution!,
    orderId: '500',
    receipt: {
      ...closeEvent.execution!.receipt,
      realizedPnlAssets: '0',
      postPositionSize: '500000000000000000000',
      postPositionMarginAssets: '5000000000',
      preSettlementBalanceAssets: '15000000000',
      postSettlementBalanceAssets: '9996650000',
    },
  },
}

const sampleTxResponse: TransactionDetailsResponse = {
  network: { chainId: 421614, releaseVersion: 'v2.0.0', settlementAssetSymbol: 'USDG' },
  txHash: sampleTxHash,
  blockNumber: '332919707',
  occurredAt: '2026-11-26T15:34:00Z',
  executorAddress: sampleKeeper,
  events: [openEvent, closeEvent],
}

function renderTx(entry: string | { pathname: string; search?: string; state?: TransactionRouteState }) {
  render(
    <MemoryRouter initialEntries={[entry]}>
      <Routes>
        <Route path="/tx/:txHash" element={<TransactionPage />} />
      </Routes>
    </MemoryRouter>,
  )
}

beforeEach(() => {
  txQuery.mockReset()
  txQuery.mockReturnValue({
    data: sampleTxResponse,
    isLoading: false,
    isError: false,
  })
  walletExposureQuery.mockReset()
  walletExposureQuery.mockReturnValue({
    data: {
      positions: [
        {
          positionId: `pos-${sampleTxHash}:8`,
          market: 'plDXY-PERP',
          status: 'closed',
          side: 'long',
          openedAt: '2026-11-26T15:30:00Z',
          closedAt: '2026-11-26T15:34:00Z',
          durationSeconds: 240,
          entryPrice: '98800000',
          exitPrice: '98750000',
          currentSize: '0',
          peakSize: '500000000000000000000',
          peakNotionalAssets: '50000000000',
          peakMarginAssets: '10000000000',
          entryLeverageX100: 500,
          maxLeverageX100: 500,
          currentLeverageX100: 0,
          realizedPnlAssets: '1250000000',
          unrealizedPnlAssets: '0',
          netPnlAssets: '1250000000',
          roiBps: 1250,
          totalFeesAssets: '1500000',
          totalVpiAssets: '1250000',
          totalKeeperBountyAssets: '0',
          mfePnlAssets: '1500000000',
          mfeBps: 1500,
          mfePrice: '98600000',
          mfeOccurredAt: '2026-11-26T15:32:00Z',
          maePnlAssets: '-100000000',
          maeBps: -100,
          maePrice: '98900000',
          maeOccurredAt: '2026-11-26T15:31:00Z',
          liquidationPrice: null,
          closestLiquidationBps: 1800,
          closestLiquidationAt: '2026-11-26T15:31:00Z',
          currentLiquidationBps: null,
          liquidatable: false,
          fillCount: 2,
          scaleInCount: 0,
          partialCloseCount: 0,
          marginAddCount: 0,
          openTxHash: sampleTxHash,
          closeTxHash: sampleTxHash,
          openOrderId: '500',
        },
      ],
      exposureTimeline: [],
    },
    isLoading: false,
    isError: false,
  })
})

describe('TransactionPage', () => {
  it('renders full execution breakdown, account link, executor, and switches between multiple events', () => {
    renderTx(`/tx/${sampleTxHash}?log=14`)

    expect(screen.queryByText('Verified execution receipt')).not.toBeInTheDocument()
    expect(screen.getByText(sampleTxHash)).toBeInTheDocument()
    expect(screen.getByText('Protocol fee assessed')).toBeInTheDocument()
    expect(screen.getByText('Gross VPI charge')).toBeInTheDocument()
    expect(screen.getByText('Actual account change')).toBeInTheDocument()
    expect(screen.getByText('Fully closed')).toBeInTheDocument()
    expect(screen.getByRole('button', { name: 'Share PnL' })).toBeInTheDocument()
    expect(screen.getByRole('link', { name: '0x1111…1111' })).toHaveAttribute(
      'href',
      `/wallets/${sampleAccount}`,
    )
    expect(screen.getByRole('link', { name: '0x9999…9999' })).toHaveAttribute(
      'href',
      `/keepers/${sampleKeeper}`,
    )
    expect(
      screen.getByRole('link', { name: `View ${sampleKeeper} on Arbiscan` }),
    ).toHaveAttribute('href', `https://sepolia.arbiscan.io/address/${sampleKeeper}`)

    // Direct links to the matched position lifecycle
    const expectedPositionPath = `/wallets/${sampleAccount}/positions/${encodeURIComponent(`pos-${sampleTxHash}:8`)}`
    expect(screen.getByRole('link', { name: 'Position lifecycle →' })).toHaveAttribute(
      'href',
      expectedPositionPath,
    )
    expect(screen.getByRole('link', { name: 'View full position lifecycle →' })).toHaveAttribute(
      'href',
      expectedPositionPath,
    )

    // Verify no duplicate external Arbiscan URLs are rendered on the page
    const arbiscanHrefs = screen
      .getAllByRole('link')
      .map((el) => el.getAttribute('href') ?? '')
      .filter((href) => href.includes('arbiscan.io'))
    expect(arbiscanHrefs.length).toBeGreaterThanOrEqual(2)
    expect(new Set(arbiscanHrefs.map((u) => u.toLowerCase())).size).toBe(arbiscanHrefs.length)

    // Switch to the first event (#1 Open · log 8)
    fireEvent.click(screen.getByRole('button', { name: /#1 Open/i }))
    expect(screen.getByText('Open / increase')).toBeInTheDocument()
    expect(screen.queryByRole('button', { name: 'Share PnL' })).not.toBeInTheDocument()
    expect(screen.getByRole('button', { name: 'Share Open' })).toBeInTheDocument()
  })

  it('hydrates immediately from router state even if the backend tx endpoint returns 404', () => {
    txQuery.mockReturnValue({
      data: undefined,
      isLoading: false,
      isError: true,
      error: new InsightsApiError('Not found', 404),
    })

    renderTx({
      pathname: `/tx/${sampleTxHash}`,
      search: '?log=14',
      state: {
        trade: closeEvent,
        chainId: 421614,
        releaseVersion: 'v2.0.0',
        from: '/execution',
        fromLabel: 'Back to Execution',
      },
    })

    expect(screen.queryByText('Verified execution receipt')).not.toBeInTheDocument()
    expect(screen.getByText('Protocol fee assessed')).toBeInTheDocument()
    const breadcrumb = screen.getByRole('navigation', { name: 'Breadcrumb' })
    expect(within(breadcrumb).getByRole('link', { name: 'Execution' })).toHaveAttribute(
      'href',
      '/execution',
    )
  })

  it('rejects malformed tx hashes cleanly', () => {
    renderTx('/tx/not-a-tx-hash')
    expect(screen.getByText('Invalid transaction hash')).toBeInTheDocument()
  })

  it('renders an empty state with Arbiscan link when a valid hash is not indexed', () => {
    txQuery.mockReturnValue({
      data: undefined,
      isLoading: false,
      isError: true,
      error: new InsightsApiError('Not found', 404, 'NOT_FOUND'),
    })

    renderTx(`/tx/${sampleTxHash}`)
    expect(
      screen.getByText('No Plether Perps execution indexed for this transaction'),
    ).toBeInTheDocument()
    expect(screen.getByRole('link', { name: /Arbiscan/i })).toHaveAttribute(
      'href',
      `https://sepolia.arbiscan.io/tx/${sampleTxHash}`,
    )
  })

  it('renders protocol-level batch finalization manifest, Pyth oracle window, and event trace even with zero trade fills', () => {
    txQuery.mockReturnValue({
      data: {
        network: { chainId: 421614, releaseVersion: 'v2.0.0', settlementAssetSymbol: 'USDG' },
        txHash: sampleTxHash,
        txKind: 'batch_finalization',
        blockNumber: '333079617',
        occurredAt: '2026-11-27T06:22:10Z',
        executorAddress: sampleKeeper,
        batchSummary: {
          totalOrders: 2,
          executedOrders: 1,
          failedOrExpiredOrders: 1,
          uniqueAccounts: 2,
          minLatencySeconds: 2,
          medianLatencySeconds: 3,
          maxLatencySeconds: 4,
          totalVpiAssets: '12500000',
          oracleMinPublishTime: '2026-11-27T06:22:08Z',
          oracleMaxPublishTime: '2026-11-27T06:22:09Z',
          oracleSpreadSeconds: 1,
          oraclePrice: '1054110000000000000',
          oracleFrozen: false,
        },
        finalizedOrders: [
          {
            orderId: '901',
            account: sampleAccount,
            side: 'long',
            commitTxHash: `0x${'2'.repeat(64)}`,
            commitBlock: '333079609',
            commitAt: '2026-11-27T06:22:08Z',
            latencySeconds: 2,
            latencyBlocks: 8,
            terminalStatus: 'Executed',
            terminalReason: 'Executed',
            failureReason: null,
            failedConstraint: null,
            executionMode: 'live',
            executionPrice: '1054120000000000000',
            oraclePrice: '1054110000000000000',
            oracleFrozen: false,
            vpiAssets: '12500000',
            frozenCloseSpreadAssets: null,
          },
          {
            orderId: '902',
            account: `0x${'3'.repeat(40)}`,
            side: 'short',
            commitTxHash: `0x${'4'.repeat(64)}`,
            commitBlock: '333079580',
            commitAt: '2026-11-27T06:22:06Z',
            latencySeconds: 4,
            latencyBlocks: 37,
            terminalStatus: 'Expired',
            terminalReason: 'Expired',
            failureReason: '2',
            failedConstraint: 'SLIPPAGE_OR_TTL',
            executionMode: 'live',
            executionPrice: null,
            oraclePrice: '1054110000000000000',
            oracleFrozen: false,
            vpiAssets: null,
            frozenCloseSpreadAssets: null,
          },
        ],
        protocolEvents: [
          {
            logIndex: 11,
            contractAddress: `0x${'5'.repeat(40)}`,
            eventName: 'OrderFinalized',
            account: sampleAccount,
            orderId: '901',
            side: 'long',
            data: { status: '2', terminalReason: 'Executed' },
          },
        ],
        lpSettlement: null,
        events: [],
      },
      isLoading: false,
      isError: false,
      error: null,
    })

    renderTx(`/tx/${sampleTxHash}`)
    expect(screen.getByText('Batch finalization manifest (2 orders)')).toBeInTheDocument()
    expect(screen.getByText('1/2 executed')).toBeInTheDocument()
    expect(screen.getByText('SLIPPAGE_OR_TTL')).toBeInTheDocument()
    expect(screen.getByText('On-chain protocol event trace (1)')).toBeInTheDocument()
    expect(screen.getByText('OrderFinalized')).toBeInTheDocument()

    fireEvent.click(screen.getByRole('button', { name: 'JSON' }))
    expect(screen.getByText(/"terminalReason": "Executed"/)).toBeInTheDocument()
  })

  it('matches batch orders via execution.orderId when activity.orderId is null, shows Viewing ↑, and switches fill on Inspect fill click', () => {
    const scrollSpy = vi.fn()
    Element.prototype.scrollIntoView = scrollSpy

    txQuery.mockReturnValue({
      data: {
        ...sampleTxResponse,
        events: [
          { ...openEvent, orderId: null },
          { ...closeEvent, orderId: null },
        ],
        finalizedOrders: [
          {
            orderId: '500',
            account: sampleAccount,
            side: 'long',
            commitTxHash: null,
            commitBlock: '332919700',
            commitAt: '2026-11-26T15:33:55Z',
            latencySeconds: 2,
            latencyBlocks: 7,
            terminalStatus: 'Executed',
            terminalReason: 'Executed',
            failureReason: null,
            failedConstraint: null,
            executionMode: 'live',
            executionPrice: '94740000',
            oraclePrice: '94740000',
            oracleFrozen: false,
            vpiAssets: '1250000',
            frozenCloseSpreadAssets: null,
          },
          {
            orderId: '501',
            account: sampleAccount,
            side: 'long',
            commitTxHash: null,
            commitBlock: '332919701',
            commitAt: '2026-11-26T15:33:56Z',
            latencySeconds: 3,
            latencyBlocks: 6,
            terminalStatus: 'Executed',
            terminalReason: 'Executed',
            failureReason: null,
            failedConstraint: null,
            executionMode: 'live',
            executionPrice: '94700000',
            oraclePrice: '94700000',
            oracleFrozen: false,
            vpiAssets: '1250000',
            frozenCloseSpreadAssets: null,
          },
        ],
      },
      isLoading: false,
      isError: false,
    })

    renderTx(`/tx/${sampleTxHash}?log=14`)

    // Order #501 (Close, log 14) is active -> shows Viewing ↑, while Order #500 shows Inspect fill
    expect(screen.getByRole('button', { name: 'Viewing ↑' })).toBeInTheDocument()
    const inspectBtn = screen.getByRole('button', { name: 'Inspect fill' })
    fireEvent.click(inspectBtn)

    // Switches to Order #500 (Open) and scrolls back up to the breakdown
    expect(screen.getByText('Open / increase')).toBeInTheDocument()
    expect(scrollSpy).toHaveBeenCalledTimes(1)
  })
})
