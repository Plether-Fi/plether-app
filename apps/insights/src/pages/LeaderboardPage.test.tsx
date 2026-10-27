import { fireEvent, render, screen, within } from '@testing-library/react'
import { MemoryRouter } from 'react-router-dom'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { LeaderboardPage } from './LeaderboardPage'
import { account, input } from '../test/divisions'
import { publishWeek } from '../divisions/engine'

const query = vi.hoisted(() => vi.fn())
const liveQuery = vi.hoisted(() => vi.fn())
const clock = vi.hoisted(() => ({ now: Date.parse('2026-10-12T12:00:00Z') }))
vi.mock('../divisions/query', () => ({ useDivisions: query, useLiveDivisions: liveQuery }))
vi.mock('../hooks/useUtcNow', () => ({ useUtcNow: () => clock.now }))

beforeEach(() => {
  clock.now = Date.parse('2026-10-12T12:00:00Z')
  query.mockReturnValue({ data: null, isLoading: false, isError: false })
  liveQuery.mockReturnValue({ data: { status: 'pending', weekStartsAt: '2026-10-11T21:00:00Z' }, isLoading: false, isError: false })
})

function show() { render(<MemoryRouter><LeaderboardPage /></MemoryRouter>) }

describe('Divisions leaderboard', () => {
  it('shows launch rules without pretending September scores are weekly results', () => {
    show()
    expect(screen.getByRole('heading', { name: 'Plether Divisions' })).toBeInTheDocument()
    expect(screen.getByRole('heading', { name: 'Live weekly leaderboard' })).toBeInTheDocument()
    expect(screen.queryByRole('heading', { name: 'Current release wallets' })).not.toBeInTheDocument()
    expect(screen.getByText(/Live weekly returns include every wallet/)).toBeInTheDocument()
    expect(screen.queryByText(/2\+ active days/)).not.toBeInTheDocument()
    expect(screen.queryByText('Total prize pool')).not.toBeInTheDocument()
    expect(screen.queryByRole('link', { name: /Enter competition/ })).not.toBeInTheDocument()
    expect(screen.getByRole('link', { name: 'Link Discord and wallet' })).toHaveAttribute('href', '/register')
    expect(screen.getByText(/Discord role assignment is coming/)).toBeInTheDocument()
  })
  it('leads with live rankings when weekly results are unpublished', () => {
    clock.now = Date.parse('2026-10-19T12:00:00Z')
    liveQuery.mockReturnValue({ data: { status: 'ready', weekStartsAt: '2026-10-18T21:00:00Z', snapshotAt: '2026-10-19T12:00:00Z', snapshotBlock: '320000000', walletCount: 2, standings: [
      { address: '0x0000000000000000000000000000000000000001', rank: 1, weeklyReturnBps: 100, activeDays: 1 },
      { address: '0x0000000000000000000000000000000000000002', rank: 2, weeklyReturnBps: 50, activeDays: 1 },
    ] }, isLoading: false, isError: false })
    show()
    expect(screen.getByRole('heading', { name: 'Live weekly leaderboard' })).toBeInTheDocument()
    expect(screen.queryByRole('heading', { name: 'Current release wallets' })).not.toBeInTheDocument()
    expect(screen.queryByText('Awaiting the first weekly publication')).not.toBeInTheDocument()
    expect(screen.queryByRole('heading', { name: 'Weekly leaderboard' })).not.toBeInTheDocument()
    expect(screen.getByRole('table')).toHaveTextContent('0x0000…0001')
    expect(screen.getByRole('table')).toHaveTextContent('0x0000…0002')
  })
  it('shows public handles and earned badges without deriving tiers from live returns', () => {
    const publication = publishWeek(input(0, [account(1)]), null)
    query.mockReturnValue({ data: publication })
    liveQuery.mockReturnValue({ data: { status: 'ready', walletCount: 2, standings: [
      { address: account(2).address, discordUsername: null, rank: 1, weeklyReturnBps: 5000, activeDays: 5 },
      { address: account(1).address, discordUsername: 'mellboi', rank: 2, weeklyReturnBps: -100, activeDays: 0 },
    ] } })
    show()
    const table = screen.getByRole('table', { name: /Provisional weekly ranks/ })
    expect(within(table).getByRole('columnheader', { name: 'Discord handle' })).toBeInTheDocument()
    expect(within(table).getByRole('columnheader', { name: 'Current badge' })).toBeInTheDocument()
    const rows = within(table).getAllByRole('row')
    expect(rows[1]).toHaveTextContent('Not yet awarded')
    expect(rows[1]).not.toHaveTextContent('Iron')
    expect(rows[2]).toHaveTextContent('Bronze')
    expect(rows[2]).toHaveTextContent('@mellboi')
    expect(within(rows[1]).getByLabelText('Discord not linked')).toBeInTheDocument()
    fireEvent.change(screen.getByRole('searchbox'), { target: { value: '@MELLBOI' } })
    expect(within(table).getAllByRole('row')).toHaveLength(2)
    expect(table).toHaveTextContent('@mellboi')
  })
  it('shows Iron only after a completed week publishes it, regardless of current-week gains', () => {
    clock.now = Date.parse('2026-10-20T12:00:00Z')
    query.mockReturnValue({ data: publishWeek(input(0, [account(1, { closingEquityUsdc: '99000000000' })]), null) })
    liveQuery.mockReturnValue({ data: { status: 'ready', walletCount: 1, standings: [
      { address: account(1).address, rank: 1, weeklyReturnBps: 5000, activeDays: 2 },
    ] } })
    show()
    const table = screen.getByRole('table', { name: /Provisional weekly ranks/ })
    expect(table).toHaveTextContent('Iron')
    expect(table).not.toHaveTextContent('Bronze')
    expect(screen.getByText(/Last awarded for the week ending 16 Oct 2026/)).toBeInTheDocument()
  })
  it('finds published trader handles with an @ prefix', () => {
    query.mockReturnValue({ data: publishWeek(input(0, [account(1, { displayName: 'mellboi' }), account(2)]), null) })
    show()
    fireEvent.change(screen.getByRole('searchbox'), { target: { value: '@MELLBOI' } })
    const table = screen.getByRole('table', { name: 'Published weekly percentage P&L and division badges' })
    expect(table).toHaveTextContent('mellboi')
    expect(table).not.toHaveTextContent('Trader 2')
  })
  it('does not award any badge during a trader’s first week, even with a qualifying live score', () => {
    liveQuery.mockReturnValue({ data: { status: 'ready', walletCount: 1, standings: [
      { address: account(1).address, rank: 1, weeklyReturnBps: 5000, activeDays: 5 },
    ] } })
    show()
    const table = screen.getByRole('table', { name: /Provisional weekly ranks/ })
    expect(table).toHaveTextContent('Not yet awarded')
    expect(table).not.toHaveTextContent('Iron')
    expect(table).not.toHaveTextContent('Bronze')
  })
  it('pages through all live rankings and searches across pages', () => {
    liveQuery.mockReturnValue({ data: { status: 'ready', weekStartsAt: '2026-10-18T21:00:00Z', snapshotAt: '2026-10-19T12:00:00Z', snapshotBlock: '320000000', walletCount: 101, standings: Array.from({ length: 101 }, (_, index) => ({
      address: `0x${(index + 1).toString(16).padStart(40, '0')}`, rank: index + 1, weeklyReturnBps: 100, activeDays: 1,
    })) }, isLoading: false, isError: false })
    show()
    expect(screen.getByText('101 wallets · Page 1 of 2')).toBeInTheDocument()
    expect(within(screen.getByRole('table')).getAllByRole('row')).toHaveLength(101)
    fireEvent.click(screen.getByRole('button', { name: 'Next' }))
    expect(screen.getByText('101 wallets · Page 2 of 2')).toBeInTheDocument()
    expect(within(screen.getByRole('table')).getAllByRole('row')).toHaveLength(2)
    fireEvent.change(screen.getByRole('searchbox'), { target: { value: '0000000000000000000000000000000000000065' } })
    expect(screen.queryByText(/Page 2 of 2/)).not.toBeInTheDocument()
    expect(screen.getByRole('table')).toHaveTextContent('101')
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
  it('keeps live rankings available if no publication can be read', () => {
    query.mockReturnValue({ isError: true, error: new Error('offline'), refetch: vi.fn() })
    liveQuery.mockReturnValue({ data: { status: 'ready', weekStartsAt: '2026-10-18T21:00:00Z', snapshotAt: '2026-10-19T12:00:00Z', snapshotBlock: '320000000', walletCount: 1, standings: [
      { address: '0x0000000000000000000000000000000000000001', rank: 1, weeklyReturnBps: 100, activeDays: 1 },
    ] }, isLoading: false, isError: false })
    show()
    expect(screen.getByRole('heading', { name: 'Live weekly leaderboard' })).toBeInTheDocument()
    expect(screen.getByRole('table')).toHaveTextContent('0x0000…0001')
    expect(screen.queryByText('offline')).not.toBeInTheDocument()
    expect(screen.getByRole('table')).toHaveTextContent('Badge unavailable')
    expect(screen.getByRole('table')).not.toHaveTextContent('Not yet awarded')
  })
  it('shows provisional current-release ranks before any weekly publication', () => {
    liveQuery.mockReturnValue({ data: { status: 'ready', weekStartsAt: '2026-10-11T21:00:00Z', snapshotAt: '2026-10-12T12:00:00Z', snapshotBlock: '321', walletCount: 2, standings: [
      { address: '0x0000000000000000000000000000000000000001', rank: 1, weeklyReturnBps: 1250, activeDays: 2 },
      { address: '0x0000000000000000000000000000000000000002', rank: null, weeklyReturnBps: null, activeDays: 1 },
    ] }, isLoading: false, isError: false })
    show()
    const table = screen.getByRole('table', { name: /Provisional weekly ranks/ })
    expect(table).toHaveTextContent('+12.50%')
    expect(table).toHaveTextContent('0x0000…0002')
    expect(within(table).getByRole('link', { name: 'View 0x0000000000000000000000000000000000000001 on Arbiscan' })).toHaveAttribute('href', 'https://sepolia.arbiscan.io/address/0x0000000000000000000000000000000000000001')
    expect(within(table).getByRole('link', { name: 'View 0x0000000000000000000000000000000000000001 on Arbiscan' })).toHaveAttribute('target', '_blank')
    expect(screen.getByText(/2 wallets/)).toBeInTheDocument()
  })
})
