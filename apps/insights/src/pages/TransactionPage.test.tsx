import { fireEvent, render, screen } from '@testing-library/react'
import { MemoryRouter, Route, Routes } from 'react-router-dom'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { InsightsApiError, type ProtocolTradeRecord, type TransactionDetailsResponse, type TransactionRouteState } from '../api'
import { TransactionPage } from './TransactionPage'

const txQuery = vi.fn()

vi.mock('../api', async () => {
  const actual = await vi.importActual<typeof import('../api')>('../api')
  return {
    ...actual,
    useTransaction: (...args: unknown[]) => txQuery(...args),
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
})

describe('TransactionPage', () => {
  it('renders full execution breakdown, account link, executor, and switches between multiple events', () => {
    renderTx(`/tx/${sampleTxHash}?log=14`)

    expect(screen.getByText('Verified execution receipt')).toBeInTheDocument()
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
      screen.getByRole('link', { name: `View ${sampleKeeper} on explorer` }),
    ).toHaveAttribute('href', `https://sepolia.arbiscan.io/address/${sampleKeeper}`)

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

    expect(screen.getByText('Verified execution receipt')).toBeInTheDocument()
    expect(screen.getByText('Protocol fee assessed')).toBeInTheDocument()
    expect(screen.getByRole('link', { name: '← Back to Execution' })).toHaveAttribute(
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
})
