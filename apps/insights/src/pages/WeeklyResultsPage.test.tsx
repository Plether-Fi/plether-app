import { fireEvent, render, screen, within } from '@testing-library/react'
import { MemoryRouter, useLocation } from 'react-router-dom'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { WeeklyResultsPage } from './WeeklyResultsPage'
import { account, input } from '../test/divisions'
import { publishWeek } from '../divisions/engine'

const historyQuery = vi.hoisted(() => vi.fn())
vi.mock('../divisions/query', () => ({ useDivisionsHistory: historyQuery }))
const first = publishWeek(input(0, []), null)
const latest = publishWeek(input(1, [account(1, { displayName: 'Alice' }), account(2, { displayName: 'Bob' })]), first)
const firstCursor = String(Date.parse(first.week.startsAt) / 1000)
const latestCursor = String(Date.parse(latest.week.startsAt) / 1000)

beforeEach(() => {
  historyQuery.mockImplementation((week: string | null) => ({ data: week === firstCursor
    ? { publication: first, olderCursor: null, newerCursor: latestCursor }
    : { publication: latest, olderCursor: firstCursor, newerCursor: null },
  }))
})

function Location() { return <output aria-label="Current URL">{useLocation().search}</output> }
function show(path = '/results') { render(<MemoryRouter initialEntries={[path]}><WeeklyResultsPage /><Location /></MemoryRouter>) }

describe('published weekly results', () => {
  it('opens the latest week, preserves its badges and filters wallet/Discord names', () => {
    show()
    expect(screen.getByRole('heading', { name: 'Last week’s results' })).toBeInTheDocument()
    expect(screen.getByRole('heading', { name: /Week ending 23 Oct 2026/ })).toBeInTheDocument()
    expect(screen.getByRole('button', { name: 'Newer week →' })).toBeDisabled()
    const table = screen.getByRole('table')
    expect(within(table).getAllByText('Bronze')).toHaveLength(2)
    fireEvent.change(screen.getByRole('searchbox'), { target: { value: '@BOB' } })
    expect(table).toHaveTextContent('Bob')
    expect(table).not.toHaveTextContent('Alice')
    expect(screen.getByText('Alice, Bob')).toBeInTheDocument()
    expect(within(table).getByRole('link', { name: `View trader ${account(2).address}` })).toHaveAttribute('href', `/divisions/wallets/${account(2).address}`)
  })
  it('navigates older and newer weeks with bookmarkable URLs, including an empty oldest week', () => {
    show()
    fireEvent.click(screen.getByRole('link', { name: '← Older week' }))
    expect(screen.getByLabelText('Current URL')).toHaveTextContent(`?week=${firstCursor}`)
    expect(screen.getByRole('heading', { name: /Week ending 16 Oct 2026/ })).toBeInTheDocument()
    expect(screen.getByText('No traders this week')).toBeInTheDocument()
    expect(screen.getByRole('button', { name: '← Older week' })).toBeDisabled()
    fireEvent.click(screen.getByRole('link', { name: 'Newer week →' }))
    expect(screen.getByLabelText('Current URL')).toHaveTextContent(`?week=${latestCursor}`)
    expect(screen.getByRole('heading', { name: /Week ending 23 Oct 2026/ })).toBeInTheDocument()
    fireEvent.click(screen.getByRole('link', { name: 'Latest results' }))
    expect(screen.getByRole('heading', { name: 'Last week’s results' })).toBeInTheDocument()
    expect(screen.getByLabelText('Current URL')).toBeEmptyDOMElement()
  })
  it('opens an older week directly and leaves navigation available when search finds nothing', () => {
    show(`/results?week=${firstCursor}`)
    expect(historyQuery).toHaveBeenCalledWith(firstCursor)
    expect(screen.getByText('No traders this week')).toBeInTheDocument()
    fireEvent.click(screen.getByRole('link', { name: 'Newer week →' }))
    fireEvent.change(screen.getByRole('searchbox'), { target: { value: 'missing' } })
    expect(screen.getByText('No traders found')).toBeInTheDocument()
    expect(screen.getByRole('link', { name: '← Older week' })).toBeInTheDocument()
  })
  it('retains saved results during a failed refresh and retries', () => {
    const refetch = vi.fn()
    historyQuery.mockReturnValue({ data: { publication: latest, olderCursor: firstCursor, newerCursor: null }, isError: true, refetch })
    show()
    expect(screen.getByRole('table')).toBeInTheDocument()
    expect(screen.getByText(/Could not refresh results/)).toHaveTextContent('Could not refresh results')
    fireEvent.click(screen.getByRole('button', { name: 'Retry' }))
    expect(refetch).toHaveBeenCalledOnce()
  })
  it('distinguishes an unavailable week from a history with no publications', () => {
    historyQuery.mockReturnValue({ isError: true, error: new Error('This week has not been published.'), refetch: vi.fn() })
    show('/results?week=0')
    expect(screen.getByText('Weekly results are unavailable')).toBeInTheDocument()
    expect(screen.getByRole('link', { name: 'Latest results' })).toHaveAttribute('href', '/results')
    expect(screen.queryByText('No published results yet')).not.toBeInTheDocument()
  })
  it('shows an explicit pre-publication state', () => {
    historyQuery.mockReturnValue({ data: { publication: null, olderCursor: null, newerCursor: null } })
    show()
    expect(screen.getByText('No published results yet')).toBeInTheDocument()
    expect(screen.queryByRole('navigation', { name: 'Results weeks' })).not.toBeInTheDocument()
  })
})
