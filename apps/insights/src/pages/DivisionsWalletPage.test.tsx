import { fireEvent, render, screen, within } from '@testing-library/react'
import { MemoryRouter, Route, Routes } from 'react-router-dom'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { DivisionsWalletPage } from './DivisionsWalletPage'
import { LeaderboardPage } from './LeaderboardPage'
import { WeeklyResultsPage } from './WeeklyResultsPage'
import { account, input } from '../test/divisions'
import { publishWeek } from '../divisions/engine'

const statsQuery = vi.hoisted(() => vi.fn())
const walletContextQuery = vi.hoisted(() => vi.fn())
const queries = vi.hoisted(() => ({ live: vi.fn(), publication: vi.fn(), history: vi.fn() }))
vi.mock('../divisions/query', () => ({ useDivisionsStats: statsQuery, useDivisions: queries.publication, useLiveDivisions: queries.live, useDivisionsHistory: queries.history }))
vi.mock('../divisions/walletContext', async () => {
  const actual = await vi.importActual<typeof import('../divisions/walletContext')>('../divisions/walletContext')
  return {
    ...actual,
    useDivisionsWalletContext: walletContextQuery,
  }
})
const address = account(1).address

beforeEach(() => {
  statsQuery.mockReturnValue({ data: { registeredWallets: 42 }, isError: false })
  walletContextQuery.mockReturnValue({ data: undefined, isLoading: false, isError: false })
  queries.publication.mockReturnValue({ data: publishWeek(input(0, [account(1)]), null) })
  queries.history.mockReturnValue({ data: { publication: publishWeek(input(0, [account(1)]), null), olderCursor: null, newerCursor: null } })
  queries.live.mockReturnValue({ data: {
    status: 'ready', weekStartsAt: '2026-10-18T21:00:00Z', snapshotAt: '2026-10-20T12:00:00Z', walletCount: 1,
    standings: [{ address, discordUsername: 'mellboi', rank: 2, weeklyReturnBps: -100, activeDays: 1 }],
  } })
})

function show(path = `/divisions/wallets/${address}`) {
  render(<MemoryRouter initialEntries={[path]}><Routes>
    <Route path="/divisions" element={<LeaderboardPage />} />
    <Route path="/results" element={<WeeklyResultsPage />} />
    <Route path="/divisions/wallets/:address" element={<DivisionsWalletPage />} />
  </Routes></MemoryRouter>)
}

describe('Divisions trader navigation', () => {
  it('keeps the prior contract badge and its original currency beside new live results', () => {
    const publication = publishWeek(input(0, [account(1)]), null)
    queries.publication.mockReturnValue({ data: { ...publication,
      scoringRelease: { router: `0x${'1'.repeat(40)}`, version: 'v1.3.1', assetSymbol: 'USDC' },
    } })
    show()
    expect(screen.getByText('Bronze')).toBeInTheDocument()
    expect(screen.getAllByText(/mock USDC$/).length).toBeGreaterThanOrEqual(2)
    expect(screen.getByText('-1.00%')).toBeInTheDocument()
  })

  it('places badges before wallets and opens the current trader view from either leaderboard', () => {
    show('/divisions')
    const liveTable = screen.getByRole('table', { name: /Provisional weekly ranks/ })
    expect(within(liveTable).getAllByRole('columnheader').map((header) => header.textContent)).toEqual([
      'Rank', 'Current badge', 'Wallet', 'Discord handle', 'Weekly return', 'Active days',
    ])
    fireEvent.click(within(liveTable).getByRole('link', { name: `View trader ${address}` }))
    expect(screen.getByRole('heading', { name: '@mellboi' })).toBeInTheDocument()
    expect(screen.getByText('Bronze')).toBeInTheDocument()
    expect(screen.getByText('-1.00%')).toBeInTheDocument()
    expect(screen.getByText('+10.00%')).toBeInTheDocument()
    expect(screen.getByText(/Week ending 16 Oct 2026/)).toBeInTheDocument()
    expect(screen.queryByText(/Cash-prize eligibility/)).not.toBeInTheDocument()
    fireEvent.click(screen.getByRole('link', { name: /Back to leaderboard/ }))
    fireEvent.click(screen.getByRole('link', { name: 'Last week’s results →' }))
    const publishedTable = screen.getByRole('table', { name: /Published weekly percentage/ })
    fireEvent.click(within(publishedTable).getByRole('link', { name: `View trader ${address}` }))
    expect(screen.getByRole('heading', { name: '@mellboi' })).toBeInTheDocument()
  })
  it('does not give a new trader an award from their live score', () => {
    queries.publication.mockReturnValue({ data: null })
    show()
    expect(screen.getByText('Not yet awarded')).toBeInTheDocument()
    expect(screen.getByText('No published award yet')).toBeInTheDocument()
    expect(screen.queryByText('Iron')).not.toBeInTheDocument()
  })
  it('preserves the award when live data is unavailable', () => {
    queries.live.mockReturnValue({ isError: true, error: new Error('offline'), refetch: vi.fn() })
    show()
    expect(screen.getByText('Bronze')).toBeInTheDocument()
    expect(screen.getByText('Live performance is unavailable')).toBeInTheDocument()
    expect(screen.getByText('+10.00%')).toBeInTheDocument()
  })
  it('does not mistake an unreadable publication for no award', () => {
    queries.publication.mockReturnValue({ isError: true, error: new Error('offline'), refetch: vi.fn() })
    show()
    expect(screen.getByText('Badge unavailable')).toBeInTheDocument()
    expect(screen.getByText('Published results are unavailable')).toBeInTheDocument()
    expect(screen.queryByText('Not yet awarded')).not.toBeInTheDocument()
    expect(screen.getByText('-1.00%')).toBeInTheDocument()
  })
  it('handles unknown wallets without showing another trader’s results', () => {
    show(`/divisions/wallets/${account(2).address}`)
    expect(screen.getByText('Trader not found')).toBeInTheDocument()
    expect(screen.queryByText('Bronze')).not.toBeInTheDocument()
    expect(screen.queryByText('-1.00%')).not.toBeInTheDocument()
  })
  it('validates the address before requesting data', () => {
    queries.live.mockClear()
    queries.publication.mockClear()
    show('/divisions/wallets/invalid')
    expect(screen.getByText('Invalid wallet address')).toBeInTheDocument()
    expect(queries.live).not.toHaveBeenCalled()
    expect(queries.publication).not.toHaveBeenCalled()
  })

  it('renders prominent Trading Account CTAs, live tier projection, cutoff balance/position snapshot, equity bridge, FX sessions, and multi-week history', () => {
    queries.live.mockReturnValue({
      data: {
        status: 'ready',
        weekStartsAt: '2026-11-22T21:00:00Z',
        snapshotAt: '2026-11-26T12:00:00Z',
        snapshotBlock: '332945120',
        walletCount: 4,
        standings: [
          { address: account(2).address, discordUsername: 'top', rank: 1, weeklyReturnBps: 2500, activeDays: 4 },
          { address, discordUsername: 'mellboi', rank: 2, weeklyReturnBps: 1500, activeDays: 3 },
          { address: account(3).address, discordUsername: 'third', rank: 3, weeklyReturnBps: 500, activeDays: 2 },
          { address: account(4).address, discordUsername: 'fourth', rank: 4, weeklyReturnBps: 100, activeDays: 1 },
        ],
      },
    })
    walletContextQuery.mockReturnValue({
      data: {
        network: { chainId: 421614, releaseVersion: 'v2.0.0', settlementAssetSymbol: 'USDG' },
        account: address,
        observedAt: '2026-11-26T12:00:00Z',
        identity: { discordUsername: 'mellboi', isDivisionsLinked: true },
        liveWeek: {
          weekStart: 1795381200,
          weekStartsAt: '2026-11-22T21:00:00Z',
          weekClosesAt: '2026-11-27T21:00:00Z',
          baselineBlock: '331200000',
          cutoffBlock: '332945120',
          cutoffAt: '2026-11-26T12:00:00Z',
          releaseVersion: 'v2.0.0',
          assetSymbol: 'USDG',
          activeWeekdays: [true, true, false, true, false],
          activity: {
            tradesCount: 7,
            opensCount: 4,
            closesCount: 3,
            liquidationsCount: 0,
            volumeAssets: '240000000000',
            realizedPnlAssets: '1100000000',
            depositsAssets: '2000000000',
            withdrawalsAssets: '0',
          },
          equityBridge: {
            startingEquityAssets: '8000000000',
            depositsAssets: '2000000000',
            withdrawalsAssets: '0',
            adjustedEquityAssets: '10000000000',
            cutoffEquityAssets: '11500000000',
            weeklyPnlAssets: '1500000000',
            pendingCarryAssets: '0',
          },
          cutoffSnapshot: {
            settlementBalanceAssets: '11100000000',
            freeSettlementAssets: '7100000000',
            activePositionMarginAssets: '4000000000',
            otherLockedMarginAssets: '0',
            committedMarginAssets: '0',
            executionBountyReserveAssets: '0',
            traderClaimBalanceAssets: '0',
            pendingOrderCount: 0,
            accountEquityAssets: '11500000000',
            freeBuyingPowerAssets: '7100000000',
            netEquityAssets: '11500000000',
            pendingCarryAssets: '0',
            economicEquityAssets: '11500000000',
            hasPosition: true,
            position: {
              market: 'plDXY-PERP',
              side: 'long',
              size: '500000000000000000000',
              marginAssets: '4000000000',
              entryPrice: '95200000',
              unrealizedPnlAssets: '400000000',
              liquidatable: false,
            },
          },
        },
        publishedWeek: {
          weekStart: 1794776400,
          weekStartsAt: '2026-11-15T21:00:00Z',
          weekClosesAt: '2026-11-20T21:00:00Z',
          baselineBlock: '329000000',
          cutoffBlock: '331100000',
          cutoffAt: '2026-11-20T21:00:00Z',
          releaseVersion: 'v2.0.0',
          assetSymbol: 'USDG',
          activeWeekdays: [true, false, true, true, false],
          activity: {
            tradesCount: 5,
            opensCount: 3,
            closesCount: 2,
            liquidationsCount: 0,
            volumeAssets: '120000000000',
            realizedPnlAssets: '1000000000',
            depositsAssets: '0',
            withdrawalsAssets: '0',
          },
          equityBridge: {
            startingEquityAssets: '10000000000',
            depositsAssets: '0',
            withdrawalsAssets: '0',
            adjustedEquityAssets: '10000000000',
            cutoffEquityAssets: '11000000000',
            weeklyPnlAssets: '1000000000',
            pendingCarryAssets: '0',
          },
          cutoffSnapshot: {
            settlementBalanceAssets: '11000000000',
            freeSettlementAssets: '11000000000',
            activePositionMarginAssets: '0',
            otherLockedMarginAssets: '0',
            committedMarginAssets: '0',
            executionBountyReserveAssets: '0',
            traderClaimBalanceAssets: '0',
            pendingOrderCount: 0,
            accountEquityAssets: '11000000000',
            freeBuyingPowerAssets: '11000000000',
            netEquityAssets: '11000000000',
            pendingCarryAssets: '0',
            economicEquityAssets: '11000000000',
            hasPosition: false,
            position: null,
          },
        },
        history: [
          {
            weekStart: 1794776400,
            closingBlock: '331100000',
            weekStartsAt: '2026-11-15T21:00:00Z',
            weekClosesAt: '2026-11-20T21:00:00Z',
            publishedAt: '2026-11-23T00:00:00Z',
            displayName: 'mellboi',
            rank: 1,
            tier: 'Silver',
            previousTier: 'Bronze',
            movement: 'promoted',
            weeklyPnlAssets: '1000000000',
            adjustedEquityAssets: '10000000000',
            activeDays: 3,
            qualifying: true,
          },
        ],
      },
      isLoading: false,
      isError: false,
    })

    show()
    expect(screen.getByRole('link', { name: 'Open Trading Account →' })).toHaveAttribute(
      'href',
      `/wallets/${address}`,
    )
    expect(screen.getByRole('link', { name: 'Market Exposure & Lifecycles →' })).toHaveAttribute(
      'href',
      `/wallets/${address}?tab=exposure`,
    )
    expect(screen.getByText('Tracking Silver')).toBeInTheDocument()
    expect(screen.getByText(/\+10\.00% to Gold/)).toBeInTheDocument()
    expect(screen.getByText('LONG open at snapshot')).toBeInTheDocument()
    expect(screen.getByText('Flat at cutoff')).toBeInTheDocument()
    expect(screen.getByText('Live equity accounting bridge')).toBeInTheDocument()
    expect(screen.getByText('Audited cutoff equity bridge')).toBeInTheDocument()
    expect(screen.getByText('Multi-week Divisions trajectory')).toBeInTheDocument()

    // Ensure no duplicate Arbiscan links on the page
    const arbiscanHrefs = screen
      .getAllByRole('link')
      .map((el) => el.getAttribute('href') ?? '')
      .filter((href) => href.includes('arbiscan.io'))
    expect(arbiscanHrefs).toHaveLength(1)
  })
})
