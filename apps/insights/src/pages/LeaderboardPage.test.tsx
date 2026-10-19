import { fireEvent, render, screen, within } from '@testing-library/react'
import { MemoryRouter } from 'react-router-dom'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { LeaderboardPage } from './LeaderboardPage'
import { account, input } from '../test/divisions'
import { publishWeek } from '../divisions/engine'

const query = vi.hoisted(() => vi.fn())
const clock = vi.hoisted(() => ({ now: Date.parse('2026-10-12T12:00:00Z') }))
vi.mock('../divisions/query', () => ({ useDivisions: query }))
vi.mock('../hooks/useUtcNow', () => ({ useUtcNow: () => clock.now }))

beforeEach(() => {
  clock.now = Date.parse('2026-10-12T12:00:00Z')
  query.mockReturnValue({ data: null, isLoading: false, isError: false })
})

function show() { render(<MemoryRouter><LeaderboardPage /></MemoryRouter>) }

describe('Divisions leaderboard', () => {
  it('shows launch rules without pretending September scores are weekly results', () => {
    show()
    expect(screen.getByRole('heading', { name: 'Plether Divisions' })).toBeInTheDocument()
    expect(screen.getByText('First badges scheduled for Monday 19 October 2026')).toBeInTheDocument()
    expect(screen.getByText('The first climb starts here')).toBeInTheDocument()
    expect(screen.getByText(/2\+ active days/)).toBeInTheDocument()
    expect(screen.queryByText('Total prize pool')).not.toBeInTheDocument()
    expect(screen.queryByRole('link', { name: /Enter competition/ })).not.toBeInTheDocument()
    expect(screen.getByText(/Discord role linking is coming/)).toBeInTheDocument()
  })
  it('shows an awaiting state after the scheduled first publication when no snapshot exists', () => {
    clock.now = Date.parse('2026-10-19T12:00:00Z')
    show()
    expect(screen.getByText('Awaiting the first weekly publication')).toBeInTheDocument()
    expect(screen.getByText('Awaiting the first published snapshot')).toBeInTheDocument()
    expect(screen.getByText('Weekly results will appear here once the verified snapshot is published.')).toBeInTheDocument()
  })
  it('shows earned badges beside traders and filters the full published ranking', () => {
    const publication = publishWeek(input(0, [account(1, { displayName: 'Alice' }), account(2, { displayName: 'Bob' })]), null)
    query.mockReturnValue({ data: publication })
    show()
    const table = screen.getByRole('table')
    expect(within(table).getAllByRole('img', { name: 'Bronze' })).toHaveLength(2)
    expect(within(table).getByText('Alice')).toBeInTheDocument()
    expect(within(table).getAllByText('+10.00%')).toHaveLength(2)
    fireEvent.change(screen.getByRole('searchbox'), { target: { value: 'bob' } })
    expect(screen.queryByRole('table')?.textContent).not.toContain('Alice')
    expect(within(screen.getByRole('table')).getByText('Bob')).toBeInTheDocument()
    expect(screen.getByText('Alice, Bob')).toBeInTheDocument()
  })
  it('retains published results on a failed refresh and provides retry', () => {
    const refetch = vi.fn()
    query.mockReturnValue({ data: publishWeek(input(), null), isError: true, refetch })
    show()
    expect(screen.getByRole('table')).toBeInTheDocument()
    expect(screen.getByText(/Could not refresh Divisions/)).toHaveAttribute('role', 'status')
    fireEvent.click(screen.getByRole('button', { name: 'Retry' }))
    expect(refetch).toHaveBeenCalledOnce()
  })
  it('shows a blocking failure if no publication can be read', () => {
    query.mockReturnValue({ isError: true, error: new Error('offline'), refetch: vi.fn() })
    show()
    expect(screen.getByText('offline')).toBeInTheDocument()
    expect(screen.queryByText('The first climb starts here')).not.toBeInTheDocument()
  })
})
