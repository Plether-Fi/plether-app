import { fireEvent, render, screen, within } from '@testing-library/react'
import { MemoryRouter } from 'react-router-dom'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { LeaderboardPage } from './LeaderboardPage'
import { account, input } from '../test/divisions'
import { publishWeek } from '../divisions/engine'

const query = vi.hoisted(() => vi.fn())
const walletsQuery = vi.hoisted(() => vi.fn())
const liveQuery = vi.hoisted(() => vi.fn())
const clock = vi.hoisted(() => ({ now: Date.parse('2026-10-12T12:00:00Z') }))
vi.mock('../divisions/query', () => ({ useDivisions: query, useTradedWallets: walletsQuery, useLiveDivisions: liveQuery }))
vi.mock('../hooks/useUtcNow', () => ({ useUtcNow: () => clock.now }))

beforeEach(() => {
  clock.now = Date.parse('2026-10-12T12:00:00Z')
  query.mockReturnValue({ data: null, isLoading: false, isError: false })
  liveQuery.mockReturnValue({ data: { status: 'pending', weekStartsAt: '2026-10-11T21:00:00Z' }, isLoading: false, isError: false })
  walletsQuery.mockReturnValue({ data: { traders: [], releaseRouter: '0x9b9ddc783780a14e4930dbbcd94d6356a82725e8', indexedThroughBlock: '1' }, isLoading: false, isError: false })
})

function show() { render(<MemoryRouter><LeaderboardPage /></MemoryRouter>) }

describe('Divisions leaderboard', () => {
  it('shows launch rules without pretending September scores are weekly results', () => {
    show()
    expect(screen.getByRole('heading', { name: 'Plether Divisions' })).toBeInTheDocument()
    expect(screen.getByText('Current release wallets')).toBeInTheDocument()
    expect(screen.getByText(/Live weekly returns include every wallet/)).toBeInTheDocument()
    expect(screen.queryByText(/2\+ active days/)).not.toBeInTheDocument()
    expect(screen.queryByText('Total prize pool')).not.toBeInTheDocument()
    expect(screen.queryByRole('link', { name: /Enter competition/ })).not.toBeInTheDocument()
    expect(screen.getByRole('link', { name: 'Link Discord and wallet' })).toHaveAttribute('href', '/register')
    expect(screen.getByText(/Discord role assignment is coming/)).toBeInTheDocument()
  })
  it('leads with current-release wallets when weekly rankings are unpublished', () => {
    clock.now = Date.parse('2026-10-19T12:00:00Z')
    walletsQuery.mockReturnValue({ data: { releaseRouter: '0x9b9ddc783780a14e4930dbbcd94d6356a82725e8', indexedThroughBlock: '320000000', traders: [
      { address: '0x0000000000000000000000000000000000000001', executedTrades: 3, lastTradeAt: '2026-10-18T20:00:00Z' },
      { address: '0x0000000000000000000000000000000000000002', executedTrades: 1, lastTradeAt: '2026-10-17T20:00:00Z' },
    ] }, isLoading: false, isError: false })
    show()
    expect(screen.getByRole('heading', { name: 'Current release wallets' })).toBeInTheDocument()
    expect(screen.queryByText('Awaiting the first weekly publication')).not.toBeInTheDocument()
    expect(screen.queryByRole('heading', { name: 'Weekly leaderboard' })).not.toBeInTheDocument()
    expect(screen.getByRole('table')).toHaveTextContent('0x0000…0001')
    expect(screen.getByRole('table')).toHaveTextContent('0x0000…0002')
  })
  it('pages through the complete traded-wallet roster', () => {
    walletsQuery.mockReturnValue({ data: { releaseRouter: '0x9b9ddc783780a14e4930dbbcd94d6356a82725e8', indexedThroughBlock: '320000000', traders: Array.from({ length: 101 }, (_, index) => ({
      address: `0x${(index + 1).toString(16).padStart(40, '0')}`, executedTrades: 1, lastTradeAt: '2026-10-18T20:00:00Z',
    })) }, isLoading: false, isError: false })
    show()
    expect(screen.getByText('101 current-release wallets · Page 1 of 2')).toBeInTheDocument()
    expect(within(screen.getByRole('table')).getAllByRole('row')).toHaveLength(101)
    fireEvent.click(screen.getByRole('button', { name: 'Next' }))
    expect(screen.getByText('101 current-release wallets · Page 2 of 2')).toBeInTheDocument()
    expect(within(screen.getByRole('table')).getAllByRole('row')).toHaveLength(2)
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
  it('keeps the live wallet list available if no publication can be read', () => {
    query.mockReturnValue({ isError: true, error: new Error('offline'), refetch: vi.fn() })
    show()
    expect(screen.getByRole('heading', { name: 'Current release wallets' })).toBeInTheDocument()
    expect(screen.queryByText('offline')).not.toBeInTheDocument()
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
    expect(screen.getByText(/2 wallets/)).toBeInTheDocument()
  })
})
