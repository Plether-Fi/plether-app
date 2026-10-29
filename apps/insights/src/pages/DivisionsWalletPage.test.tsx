import { fireEvent, render, screen, within } from '@testing-library/react'
import { MemoryRouter, Route, Routes } from 'react-router-dom'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { DivisionsWalletPage } from './DivisionsWalletPage'
import { LeaderboardPage } from './LeaderboardPage'
import { WeeklyResultsPage } from './WeeklyResultsPage'
import { account, input } from '../test/divisions'
import { publishWeek } from '../divisions/engine'

const queries = vi.hoisted(() => ({ live: vi.fn(), publication: vi.fn(), history: vi.fn() }))
vi.mock('../divisions/query', () => ({ useDivisions: queries.publication, useLiveDivisions: queries.live, useDivisionsHistory: queries.history }))
const address = account(1).address

beforeEach(() => {
  queries.publication.mockReturnValue({ data: publishWeek(input(0, [account(1)]), null) })
  queries.history.mockReturnValue({ data: { publication: publishWeek(input(0, [account(1)]), null), olderCursor: null, newerCursor: null } })
  queries.live.mockReturnValue({ data: {
    status: 'ready', weekStartsAt: '2026-10-18T21:00:00Z', snapshotAt: '2026-10-20T12:00:00Z', walletCount: 1,
    standings: [{ address, discordUsername: 'mellboi', rank: 2, weeklyReturnBps: -100, activeDays: 1 }],
  } })
})

function show(path = `/divisions/wallets/${address}`) {
  render(<MemoryRouter initialEntries={[path]}><Routes>
    <Route path="/" element={<LeaderboardPage />} />
    <Route path="/results" element={<WeeklyResultsPage />} />
    <Route path="/divisions/wallets/:address" element={<DivisionsWalletPage />} />
  </Routes></MemoryRouter>)
}

describe('Divisions trader navigation', () => {
  it('places badges before wallets and opens the current trader view from either leaderboard', () => {
    show('/')
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
})
