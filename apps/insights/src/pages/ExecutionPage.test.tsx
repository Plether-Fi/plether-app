import { fireEvent, render, screen, within } from '@testing-library/react'
import { MemoryRouter } from 'react-router-dom'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { InsightsApiError } from '../api/client'
import { resolvePerpsChain } from '../utils/chain'
import { ExecutionPage } from './ExecutionPage'

const protocolContext = vi.hoisted(() => vi.fn())
const qualityQuery = vi.hoisted(() => vi.fn())
const tradesQuery = vi.hoisted(() => vi.fn())
const fadRiskQuery = vi.hoisted(() => vi.fn())

vi.mock('../hooks/useProtocolContext', () => ({ useProtocolContext: protocolContext }))
vi.mock('../api', async (importOriginal) => ({
  ...(await importOriginal<typeof import('../api')>()),
  useExecutionQuality: qualityQuery,
  useExecutionTrades: tradesQuery,
  useMarketFadRisk: fadRiskQuery,
}))

beforeEach(() => {
  fadRiskQuery.mockReturnValue({
    data: undefined,
    isLoading: false,
    isError: false,
    error: null,
    refetch: vi.fn(),
  })
  protocolContext.mockReturnValue({
    chainId: 421614,
    chain: resolvePerpsChain(421614),
    releaseVersion: 'v2.0.0',
    blockNumber: '332879274',
    observedAt: '2026-11-26T11:37:00Z',
  })
  qualityQuery.mockReturnValue({
    data: {
      network: { chainId: 421614, releaseVersion: 'v2.0.0', settlementAssetSymbol: 'USDG' },
      observedAt: '2026-11-26T12:00:00Z',
      cohortEndAt: '2026-11-26T11:58:00Z',
      volume24hAssets: '1250000000000',
      pendingOrderQueue: 2,
      orderReliability: [
        {
          windowSeconds: 3600,
          totalOrders: 50,
          executedOrders: 49,
          expiredOrders: 1,
          otherFailedOrders: 0,
          pendingOrders: 0,
          activeAccounts: 20,
          affectedAccounts: 1,
          repeatAffectedAccounts: 0,
          fillRateBps: 9800,
          expiryRateBps: 200,
        },
        {
          windowSeconds: 86400,
          totalOrders: 1000,
          executedOrders: 985,
          expiredOrders: 10,
          otherFailedOrders: 5,
          pendingOrders: 0,
          activeAccounts: 180,
          affectedAccounts: 7,
          repeatAffectedAccounts: 1,
          fillRateBps: 9850,
          expiryRateBps: 100,
        },
      ],
      executionLatency: {
        seconds: { sampleCount: 985, p50: 3, p95: 8, p99: 14, max: 22 },
        blocks: { sampleCount: 985, p50: 12, p95: 32, p99: 56, max: 88 },
      },
      protections: {
        indexedThroughBlock: '332879270',
        activeProtections: 42,
        triggered24h: 9,
        created24h: 18,
        cancelled24h: 4,
        expired24h: 1,
      },
      liquidations: {
        indexedThroughBlock: '332879274',
        updatedAt: '2026-11-26T12:00:00Z',
        watchedCandidates: 15,
        liquidations24h: 3,
        bountiesPaid24hAssets: '1500000',
      },
    },
    isLoading: false,
    isError: false,
  })
  tradesQuery.mockReturnValue({
    data: {
      network: { chainId: 421614, releaseVersion: 'v2.0.0', settlementAssetSymbol: 'USDG' },
      trades: [
        {
          id: '0x1:0',
          activityType: 'Open',
          account: `0x${'1'.repeat(40)}`,
          orderId: '101',
          side: 'long',
          price: '104500000000000000000',
          sizeDelta: '500000000000000000000',
          amountAssets: '50000000000',
          pnlAssets: null,
          vpiAssets: '1200000',
          latencySeconds: 3,
          execution: {
            orderId: '101',
            protocolVersion: 'v1.2.3',
            status: 'complete',
            receipt: {
              realizedPnlAssets: '0',
              executionFeeAssets: '250000',
              vpiAssets: '1200000',
              carryAssets: '0',
              frozenSpreadAssets: '0',
              executionBountyAssets: '100000',
              preSettlementBalanceAssets: '50000000000',
              postSettlementBalanceAssets: '49998450000',
              preTraderClaimBalanceAssets: '0',
              postTraderClaimBalanceAssets: '0',
              executionNotionalAssets: '50000000000',
              postPositionSize: '500000000000000000000',
              postPositionMarginAssets: '10000000000',
            },
            settlements: [],
          },
          txHash: `0x${'a'.repeat(64)}`,
          blockNumber: '332879270',
          occurredAt: '2026-11-26T11:59:00Z',
        },
        {
          id: '0x2:0',
          activityType: 'Liquidated',
          account: `0x${'2'.repeat(40)}`,
          side: 'short',
          amountAssets: '20000000000',
          pnlAssets: '-400000000',
          keeperBountyAssets: '500000',
          txHash: `0x${'b'.repeat(64)}`,
          blockNumber: '332879260',
          occurredAt: '2026-11-26T11:55:00Z',
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
      <ExecutionPage />
    </MemoryRouter>,
  )
}

describe('ExecutionPage (Stage 2)', () => {
  it('renders canonical 1h/24h cohorts, latency stats, and filters the protocol trade tape', () => {
    show()
    expect(screen.queryByText('Execution pipeline healthy')).not.toBeInTheDocument()
    expect(screen.getAllByText('98.50%').length).toBeGreaterThanOrEqual(2)
    expect(
      screen.getByRole('img', {
        name: /24 hours order outcomes: 985 executed, 10 expired, 5 failed/i,
      }),
    ).toBeInTheDocument()
    expect(
      screen.getByRole('img', {
        name: /Execution latency whisker \(seconds\): p50 3s, p95 8s, p99 14s, max 22s/i,
      }),
    ).toBeInTheDocument()
    expect(screen.getByText('42')).toBeInTheDocument()

    const tape = screen.getByRole('table', { name: /Recent protocol-wide executed trades/ })
    expect(within(tape).getByRole('link', { name: '0x1111…1111' })).toHaveAttribute(
      'href',
      `/wallets/0x${'1'.repeat(40)}`,
    )
    expect(within(tape).getByText('Liquidated')).toBeInTheDocument()

    // Trade with receipt links directly to the dedicated Transaction breakdown page and wallet exposure tab
    const breakdownLink = within(tape).getByRole('link', { name: 'View breakdown' })
    expect(breakdownLink).toHaveAttribute('href', `/tx/0x${'a'.repeat(64)}?log=0`)
    const detailsLink = within(tape).getByRole('link', { name: 'View details' })
    expect(detailsLink).toHaveAttribute('href', `/tx/0x${'b'.repeat(64)}?log=0`)
    const exposureLinks = within(tape).getAllByRole('link', { name: 'Exposure →' })
    expect(exposureLinks).toHaveLength(2)
    expect(exposureLinks[0]).toHaveAttribute('href', `/wallets/0x${'1'.repeat(40)}?tab=exposure`)
    expect(exposureLinks[1]).toHaveAttribute('href', `/wallets/0x${'2'.repeat(40)}?tab=exposure`)

    fireEvent.click(screen.getByRole('button', { name: 'Liquidations' }))
    expect(within(tape).queryByText('Open')).not.toBeInTheDocument()
    expect(within(tape).getByText('Liquidated')).toBeInTheDocument()
  })

  it('fails closed cleanly when Stage 2 backend endpoints return 404', () => {
    const notFound = new InsightsApiError('Not found', 404)
    qualityQuery.mockReturnValue({ data: undefined, isLoading: false, isError: true, error: notFound })
    tradesQuery.mockReturnValue({ data: undefined, isLoading: false, isError: true, error: notFound })
    show()
    expect(screen.getByText(/Connected backend does not serve Stage 2 Execution endpoints yet/i)).toBeInTheDocument()
    expect(screen.getAllByText('Unavailable').length).toBeGreaterThanOrEqual(2)
  })
})
