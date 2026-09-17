import { render, screen, within } from '@testing-library/react'
import { MemoryRouter } from 'react-router-dom'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { LeaderboardPage } from './LeaderboardPage'

const apiMocks = vi.hoisted(() => ({
  useCurrentCompetition: vi.fn(),
  useInsightsStatus: vi.fn(),
  useLeaderboard: vi.fn(),
}))

vi.mock('../api', () => apiMocks)

vi.mock('../hooks/useUtcNow', () => ({
  useUtcNow: () => Date.parse('2026-09-17T12:00:00Z'),
}))

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
  it.each([
    ['2026-09-17T11:56:59Z', true],
    ['2026-09-17T11:57:00Z', false],
    ['2026-09-17T11:59:30Z', false],
  ])('uses actual snapshot time %s to mark delayed Insights without hiding standings', (snapshotAt, delayed) => {
    apiMocks.useLeaderboard.mockReturnValue({
      data: { pages: [{ standings: [{
        address: '0x1111111111111111111111111111111111111111',
        displayName: 'Alice', rank: 1, pnl: '0', roiBps: 0, volume: '0',
        trades: 0, activeDays: 0, eligible: false, eligibilityStatus: 'pending',
        prizePlace: null, prizeAmountUsdc: null, prizePlaces: [], snapshotAt,
      }], provisional: true, nextCursor: null }] },
      hasNextPage: false, isError: false, isLoading: false,
    })
    const { container } = render(<MemoryRouter><LeaderboardPage /></MemoryRouter>)
    expect(container.querySelector('time')).toHaveAttribute('dateTime', new Date(snapshotAt).toISOString())
    expect(screen.getAllByRole('row')).toHaveLength(2)
    expect(Boolean(screen.queryByText(/Insights update delayed/))).toBe(delayed)
  })

  it('retains cached standings when a background refresh fails', () => {
    apiMocks.useLeaderboard.mockReturnValue({
      data: { pages: [{ standings: [{
        address: '0x1111111111111111111111111111111111111111',
        displayName: 'Alice', rank: 1, pnl: '0', roiBps: 0, volume: '0',
        trades: 0, activeDays: 0, eligible: false, eligibilityStatus: 'pending',
        prizePlace: null, prizeAmountUsdc: null, prizePlaces: [],
        snapshotAt: '2026-09-17T11:59:30Z',
      }], provisional: true, nextCursor: null }] },
      hasNextPage: false, isError: true, error: new Error('Unavailable'), isLoading: false,
    })
    render(<MemoryRouter><LeaderboardPage /></MemoryRouter>)
    expect(screen.getAllByRole('row')).toHaveLength(2)
    expect(screen.getByText(/Could not refresh Insights/)).toBeInTheDocument()
  })

  it('labels unscored traders as updating instead of showing zero activity', () => {
    apiMocks.useLeaderboard.mockReturnValue({
      data: { pages: [{ standings: [{
        address: '0x1111111111111111111111111111111111111111',
        displayName: 'Alice', rank: null, pnl: null, roiBps: null, volume: '0',
        trades: 0, activeDays: 0, eligible: false, eligibilityStatus: 'pending',
        prizePlace: null, prizeAmountUsdc: null, prizePlaces: [],
      }], provisional: true, nextCursor: null }] },
      hasNextPage: false, isError: false, isLoading: false,
    })

    render(<MemoryRouter><LeaderboardPage /></MemoryRouter>)

    expect(screen.getByText(/Trading data is updating for some traders/)).toHaveAttribute('role', 'status')
    expect(screen.getByText('Updating trading data…')).toBeInTheDocument()
    const cells = within(screen.getAllByRole('row')[1]).getAllByRole('cell')
    expect(cells[4]).toHaveTextContent('—')
    expect(cells[5]).toHaveTextContent('—')
    expect(cells[6]).toHaveTextContent('— / 5')
    expect(screen.queryByText('0 active days · 0 trades')).not.toBeInTheDocument()
  })

  it.each([
    ['scheduled', 'Registered'],
    ['live', 'Registered'],
    ['ended', 'Awaiting prize review'],
    ['review', 'Awaiting prize review'],
  ])('shows the correct entry status during %s on desktop and mobile', (status, label) => {
    const current = apiMocks.useCurrentCompetition.getMockImplementation()?.()
    apiMocks.useCurrentCompetition.mockReturnValue({ ...current, data: { ...current.data, status } })
    apiMocks.useLeaderboard.mockReturnValue({
      data: { pages: [{ standings: [{
        address: '0x1111111111111111111111111111111111111111',
        displayName: 'Alice', rank: 1, pnl: '0', roiBps: 0, volume: '0',
        trades: 0, activeDays: 0, eligible: false, eligibilityStatus: 'pending',
        prizePlace: null, prizeAmountUsdc: null, prizePlaces: [],
      }], provisional: true, nextCursor: null }] },
      hasNextPage: false, isError: false, isLoading: false,
    })

    render(<MemoryRouter><LeaderboardPage /></MemoryRouter>)

    expect(screen.getAllByText(label)).toHaveLength(2)
    expect(screen.queryByText('Pending review')).not.toBeInTheDocument()
    expect(screen.getByText(/Prize eligibility is reviewed after the competition ends/)).toBeInTheDocument()
    if (label === 'Registered') {
      for (const badge of screen.getAllByText(label)) {
        expect(badge).not.toHaveClass('text-brand-peach')
        expect(badge).not.toHaveClass('text-positive')
      }
    }
  })

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
