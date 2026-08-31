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
    expect(screen.getByText('Prize pool').parentElement).toHaveTextContent('2,000.00 USDC')
    expect(screen.getByText('Starting balance').parentElement).toHaveTextContent('100,000.00 mock USDC')
    expect(screen.getByText('Prize threshold').parentElement).toHaveTextContent('+1.00 mock USDC')
    expect(screen.getByText('+1.00 mock USDC net P&L or better')).toBeInTheDocument()
    expect(screen.getByText(/600.00 \/ 500.00 \/ 400.00 \/ 300.00 \/ 200.00 USDC/i)).toBeInTheDocument()
    expect(screen.getByText(/for the top 5 eligible traders/i)).toBeInTheDocument()
  })
})
