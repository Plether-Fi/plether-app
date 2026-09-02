import { render, screen } from '@testing-library/react'
import { MemoryRouter } from 'react-router-dom'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { LeaderboardPage } from './LeaderboardPage'

const apiMocks = vi.hoisted(() => ({
  useCurrentCompetition: vi.fn(),
  useInsightsStatus: vi.fn(),
  useLeaderboard: vi.fn(),
}))

vi.mock('../api', () => apiMocks)

beforeEach(() => {
  apiMocks.useCurrentCompetition.mockReturnValue({
    data: {
      id: 'competition-1',
      slug: 'testnet-trading-2026-09',
      name: 'September Testnet Trading Competition',
      status: 'live',
      startsAt: '2026-07-20T16:00:00Z',
      tradingCutoffAt: '2026-08-03T16:00:00Z',
      resultsAt: '2026-08-05T12:00:00Z',
      startingBalance: '100000000000',
      pnlEligibilityThreshold: '1000000',
      minActiveDays: 5,
      prizes: [
        { place: 1, amount: '600000000' },
        { place: 2, amount: '500000000' },
        { place: 3, amount: '400000000' },
        { place: 4, amount: '300000000' },
        { place: 5, amount: '200000000' },
      ],
      latestIndexedBlock: null,
      latestIndexedAt: null,
    },
    isError: false,
    isLoading: false,
  })
  apiMocks.useInsightsStatus.mockReturnValue({
    data: {
      participantCount: 358,
      latestIndexedBlock: 123,
      latestIndexedAt: '2026-07-20T12:00:00Z',
    },
  })
  apiMocks.useLeaderboard.mockReturnValue({
    data: { pages: [{ standings: [], provisional: false, nextCursor: null }] },
    hasNextPage: false,
    isError: false,
    isLoading: false,
  })
})

describe('LeaderboardPage', () => {
  it('fills competition metrics from the shared status query', () => {
    render(<MemoryRouter><LeaderboardPage /></MemoryRouter>)

    expect(screen.getByText('Registered traders').parentElement).toHaveTextContent('358')
    expect(screen.getByText('Starting balance').parentElement).toHaveTextContent('100,000.00 mock USDC')
    expect(screen.getByText('Minimum activity').parentElement).toHaveTextContent('5 active days')
    expect(screen.queryByText('Prize threshold')).not.toBeInTheDocument()
    expect(screen.getByText('Total prize pool').parentElement).toHaveTextContent('2,000.00 USDC')
    expect(screen.queryByText('5 active FX-session days')).not.toBeInTheDocument()
    expect(screen.queryByText('+1.00 mock USDC net P&L or better')).not.toBeInTheDocument()
    const prizeBreakdown = screen.getByRole('list', { name: 'Prize breakdown' })
    expect(prizeBreakdown).toHaveTextContent('#01600.00')
    expect(prizeBreakdown).toHaveTextContent('#02500.00')
    expect(prizeBreakdown).toHaveTextContent('#03400.00')
    expect(prizeBreakdown).toHaveTextContent('#04300.00')
    expect(prizeBreakdown).toHaveTextContent('#05200.00')
    expect(screen.getByRole('heading', { name: 'Be profitable over five days of trading' })).toBeInTheDocument()
    expect(screen.getByText(/Top 1,000 P&Ls/)).toBeInTheDocument()
    expect(screen.queryByText('$10')).not.toBeInTheDocument()
    expect(screen.queryByText('Example')).not.toBeInTheDocument()
  })
})
