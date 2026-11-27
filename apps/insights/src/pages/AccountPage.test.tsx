import { fireEvent, render, screen, within } from '@testing-library/react'
import { MemoryRouter, Route, Routes } from 'react-router-dom'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { InsightsApiError } from '../api/client'
import { resolvePerpsChain } from '../utils/chain'
import { AccountPage } from './AccountPage'

const protocolContext = vi.hoisted(() => vi.fn())
const walletSummaryQuery = vi.hoisted(() => vi.fn())
const liveDivisionsQuery = vi.hoisted(() => vi.fn())
const divisionsQuery = vi.hoisted(() => vi.fn())

vi.mock('../hooks/useProtocolContext', () => ({ useProtocolContext: protocolContext }))
vi.mock('../api', async (importOriginal) => ({
  ...(await importOriginal<typeof import('../api')>()),
  useWalletSummary: walletSummaryQuery,
}))
vi.mock('../divisions/query', () => ({
  useLiveDivisions: liveDivisionsQuery,
  useDivisions: divisionsQuery,
}))

const testAccount = `0x${'a'.repeat(40)}`

beforeEach(() => {
  protocolContext.mockReturnValue({
    chainId: 421614,
    chain: resolvePerpsChain(421614),
    releaseVersion: 'v2.0.0',
    blockNumber: '332885000',
    observedAt: '2026-11-26T12:10:00Z',
  })
  liveDivisionsQuery.mockReturnValue({
    data: {
      status: 'ready',
      weekStartsAt: '2026-11-23T00:00:00Z',
      snapshotAt: '2026-11-26T12:05:00Z',
      standings: [
        {
          address: testAccount,
          discordUsername: 'alice_fx',
          rank: 3,
          weeklyReturnBps: 450,
          activeDays: 3,
        },
      ],
    },
    isLoading: false,
    isError: false,
  })
  divisionsQuery.mockReturnValue({
    data: {
      week: { startsAt: '2026-11-16T00:00:00Z', closesAt: '2026-11-23T00:00:00Z', publishesAt: '2026-11-23T00:05:00Z' },
      publishedAt: '2026-11-23T00:05:00Z',
      standings: [
        {
          address: testAccount,
          displayName: 'alice_fx',
          rank: 2,
          tier: 'gold',
          weeklyPnlAssets: '420000000',
          adjustedEquityAssets: '10420000000',
          activeDays: 4,
          qualifying: true,
          previousTier: 'silver',
          movement: 'promoted',
          nonQualifyingWeeks: 0,
        },
      ],
    },
    isLoading: false,
    isError: false,
  })
  walletSummaryQuery.mockReturnValue({
    data: {
      network: { chainId: 421614, releaseVersion: 'v2.0.0', settlementAssetSymbol: 'USDG' },
      account: testAccount,
      observedAt: '2026-11-26T12:10:00Z',
      identity: { discordUsername: 'alice_fx', isDivisionsLinked: true },
      ledgerAvailable: true,
      ledger: {
        settlementBalanceAssets: '1000000000',
        freeSettlementAssets: '800000000',
        activePositionMarginAssets: '200000000',
        otherLockedMarginAssets: '0',
        committedMarginAssets: '0',
        executionBountyReserveAssets: '500000',
        traderClaimBalanceAssets: '0',
        pendingOrderCount: 0,
        accountEquityAssets: '1050000000',
        freeBuyingPowerAssets: '800000000',
        netEquityAssets: '1050000000',
        hasPosition: true,
        position: {
          market: 'plDXY-PERP',
          side: 'long',
          size: '500000000000000000000',
          marginAssets: '200000000',
          entryPrice: '104500000000000000000',
          unrealizedPnlAssets: '50000000',
          liquidatable: false,
        },
      },
      performance: {
        totalTrades: 12,
        opensCount: 6,
        closesCount: 6,
        liquidationsCount: 0,
        volumeAssets: '500000000000',
        realizedPnlAssets: '420000000',
        activeDays: 4,
        firstActivityAt: '2026-11-18T10:00:00Z',
        lastActivityAt: '2026-11-26T11:50:00Z',
      },
      balanceHistory: [
        {
          occurredAt: '2026-11-18T10:00:00Z',
          timestamp: 1794996000,
          blockNumber: '331000000',
          balanceAssets: '580000000',
          eventType: 'Deposit',
          source: 'transfer',
        },
        {
          occurredAt: '2026-11-26T12:10:00Z',
          timestamp: 1795695000,
          blockNumber: '332885000',
          balanceAssets: '1000000000',
          netEquityAssets: '1050000000',
          eventType: 'Live',
          source: 'ledger',
        },
      ],
      activity: [
        {
          id: '0x1:0',
          activityType: 'Open',
          orderId: '501',
          side: 'long',
          price: '104500000000000000000',
          sizeDelta: '500000000000000000000',
          amountAssets: '50000000000',
          pnlAssets: null,
          vpiAssets: '1200000',
          latencySeconds: 3,
          execution: {
            orderId: '501',
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
          txHash: `0x${'1'.repeat(64)}`,
          blockNumber: '332884000',
          occurredAt: '2026-11-26T11:50:00Z',
        },
      ],
      protections: {
        indexedThroughBlock: '332884900',
        items: [
          {
            protectionId: '77',
            status: 'Active',
            kind: 'TakeProfit',
            triggerPrice: '106000000000000000000',
            updatedBlock: '332884500',
          },
        ],
      },
      lp: {
        balances: [
          {
            tranche: 'junior',
            vaultAddress: `0x${'b'.repeat(40)}`,
            shareBalance: '1250500000000',
            unclaimedDepositShares: '0',
            withdrawalEscrowShares: '0',
            totalAttributedShares: '1250500000000',
            observedBlock: '332884900',
          },
        ],
        requestStates: [
          {
            tranche: 'junior',
            vaultAddress: `0x${'b'.repeat(40)}`,
            requestId: '14',
            pendingDepositAssets: '500000000',
            claimableDepositAssets: '0',
            claimableDepositShares: '0',
            refundableDepositAssets: '0',
            pendingRedeemShares: '0',
            refundableRedeemShares: '0',
            redeemRefundPending: false,
            isActive: true,
            observedBlock: '332884900',
          },
        ],
        requestEvents: [],
      },
      divisionsHistory: [
        {
          weekStart: 1794787200,
          closingBlock: '330000000',
          weekStartsAt: '2026-11-16T00:00:00Z',
          weekClosesAt: '2026-11-23T00:00:00Z',
          publishedAt: '2026-11-23T00:05:00Z',
          displayName: 'alice_fx',
          rank: 2,
          tier: 'gold',
          previousTier: 'silver',
          movement: 'promoted',
          weeklyPnlAssets: '420000000',
          adjustedEquityAssets: '10420000000',
          activeDays: 4,
          qualifying: true,
        },
      ],
    },
    isLoading: false,
    isError: false,
  })
})

function renderAt(path: string) {
  render(
    <MemoryRouter initialEntries={[path]}>
      <Routes>
        <Route path="/wallets/:address" element={<AccountPage />} />
      </Routes>
    </MemoryRouter>,
  )
}

describe('AccountPage (Stage 3 Public Trading Account Inspector)', () => {
  it('renders live margin health, trades with PositionBreakdown receipts, protections, LP shares, and Divisions history', () => {
    renderAt(`/wallets/${testAccount}`)
    expect(screen.getByText('@alice_fx')).toBeInTheDocument()
    expect(screen.getByText('Live account telemetry')).toBeInTheDocument()
    expect(screen.getByText('LONG · 500.00 plDXY')).toBeInTheDocument()
    expect(screen.getByText('Wallet balance history')).toBeInTheDocument()
    expect(screen.getByLabelText('Collateral allocation')).toBeInTheDocument()
    expect(screen.getByText('20.0% locked')).toBeInTheDocument()
    expect(screen.getByRole('button', { name: '7D' })).toBeInTheDocument()
    expect(screen.getByRole('button', { name: '30D' })).toBeInTheDocument()
    expect(screen.getByRole('button', { name: '90D' })).toBeInTheDocument()

    // Open share card studio from header — Account Overview (with life-long balance chart) is primary, never duplicating Divisions Rank
    fireEvent.click(screen.getByRole('button', { name: 'Share card' }))
    expect(screen.getByText('Shareable social card')).toBeInTheDocument()
    expect(screen.getByRole('tab', { name: 'Account Overview' })).toHaveAttribute('aria-selected', 'true')
    expect(screen.getByRole('tab', { name: 'Open Position' })).toBeInTheDocument()
    expect(screen.queryByRole('tab', { name: 'Divisions Rank' })).not.toBeInTheDocument()
    fireEvent.click(screen.getByRole('button', { name: 'Close share card modal' }))

    // Switch to Trades & Receipts tab and verify receipt breakdown page link
    fireEvent.click(screen.getByRole('tab', { name: /Trades & Receipts/i }))
    const tradeTable = screen.getByRole('table', { name: /Trading Account activity/i })
    const breakdownLink = within(tradeTable).getByRole('link', { name: 'View breakdown' })
    expect(breakdownLink).toHaveAttribute('href', `/tx/0x${'1'.repeat(64)}?log=0`)

    // Switch to TP/SL Protections tab
    fireEvent.click(screen.getByRole('tab', { name: /TP\/SL Protections/i }))
    expect(screen.getByText('#77')).toBeInTheDocument()
    expect(screen.getByText('TakeProfit')).toBeInTheDocument()

    // Switch to LP Vault Activity tab
    fireEvent.click(screen.getByRole('tab', { name: /LP Vault Activity/i }))
    expect(screen.getAllByText('Junior Vault').length).toBeGreaterThanOrEqual(2)
    expect(screen.getAllByText('1,250.50 shares').length).toBeGreaterThanOrEqual(2)
    expect(screen.getByText('#14')).toBeInTheDocument()

    // Switch to Divisions History tab
    fireEvent.click(screen.getByRole('tab', { name: /Divisions History/i }))
    expect(screen.getByText('#2')).toBeInTheDocument()
    expect(screen.getByText('promoted')).toBeInTheDocument()
  })

  it('rejects invalid addresses cleanly without calling the inspector', () => {
    renderAt('/wallets/not-an-address')
    expect(screen.getByText('Invalid Trading Account address')).toBeInTheDocument()
  })

  it('fails closed gracefully when Stage 3 summary endpoint returns 404 while preserving live Divisions standing', () => {
    walletSummaryQuery.mockReturnValue({
      data: undefined,
      isLoading: false,
      isError: true,
      error: new InsightsApiError('Not found', 404),
    })
    renderAt(`/wallets/${testAccount}`)
    expect(screen.getByText(/Connected backend does not serve Stage 3 Account Summary yet/i)).toBeInTheDocument()
    expect(screen.getByText('@alice_fx')).toBeInTheDocument()
    expect(screen.getAllByText('Unavailable').length).toBeGreaterThanOrEqual(4)
  })
})
