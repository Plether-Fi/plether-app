import { fireEvent, render, screen, within } from '@testing-library/react'
import { MemoryRouter } from 'react-router-dom'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { LeaderboardPage } from './LeaderboardPage'
import { account, input } from '../test/divisions'
import { publishWeek } from '../divisions/engine'

const query = vi.hoisted(() => vi.fn())
const walletsQuery = vi.hoisted(() => vi.fn())
const clock = vi.hoisted(() => ({ now: Date.parse('2026-10-12T12:00:00Z') }))
vi.mock('../divisions/query', () => ({ useDivisions: query, useTradedWallets: walletsQuery }))
vi.mock('../hooks/useUtcNow', () => ({ useUtcNow: () => clock.now }))

beforeEach(() => {
  clock.now = Date.parse('2026-10-12T12:00:00Z')
  query.mockReturnValue({ data: null, isLoading: false, isError: false })
  walletsQuery.mockReturnValue({ data: { traders: [], indexedThroughBlock: '1' }, isLoading: false, isError: false })
})

function show() { render(<MemoryRouter><LeaderboardPage /></MemoryRouter>) }

describe('Divisions leaderboard', () => {
  it('shows launch rules without pretending September scores are weekly results', () => {
    show()
    expect(screen.getByRole('heading', { name: 'Plether Divisions' })).toBeInTheDocument()
    expect(screen.getByText('All traded wallets')).toBeInTheDocument()
    expect(screen.getByText(/Follow trading activity across every traded wallet/)).toBeInTheDocument()
    expect(screen.queryByText(/2\+ active days/)).not.toBeInTheDocument()
    expect(screen.queryByText('Total prize pool')).not.toBeInTheDocument()
    expect(screen.queryByRole('link', { name: /Enter competition/ })).not.toBeInTheDocument()
    expect(screen.getByRole('link', { name: 'Link Discord and wallet' })).toHaveAttribute('href', '/register')
    expect(screen.getByText(/Discord role assignment is coming/)).toBeInTheDocument()
  })
  it('leads with all traded wallets when weekly rankings are unpublished', () => {
    clock.now = Date.parse('2026-10-19T12:00:00Z')
    walletsQuery.mockReturnValue({ data: { indexedThroughBlock: '320000000', traders: [
      { address: '0x0000000000000000000000000000000000000001', executedTrades: 3, lastTradeAt: '2026-10-18T20:00:00Z' },
      { address: '0x0000000000000000000000000000000000000002', executedTrades: 1, lastTradeAt: '2026-10-17T20:00:00Z' },
    ] }, isLoading: false, isError: false })
    show()
    expect(screen.getByRole('heading', { name: 'All traded wallets' })).toBeInTheDocument()
    expect(screen.queryByText('Awaiting the first weekly publication')).not.toBeInTheDocument()
    expect(screen.queryByRole('heading', { name: 'Weekly leaderboard' })).not.toBeInTheDocument()
    expect(screen.getByRole('table')).toHaveTextContent('0x0000…0001')
    expect(screen.getByRole('table')).toHaveTextContent('0x0000…0002')
  })
  it('pages through the complete traded-wallet roster', () => {
    walletsQuery.mockReturnValue({ data: { indexedThroughBlock: '320000000', traders: Array.from({ length: 101 }, (_, index) => ({
      address: `0x${(index + 1).toString(16).padStart(40, '0')}`, executedTrades: 1, lastTradeAt: '2026-10-18T20:00:00Z',
    })) }, isLoading: false, isError: false })
    show()
    expect(screen.getByText('101 traded wallets · Page 1 of 2')).toBeInTheDocument()
    expect(within(screen.getByRole('table')).getAllByRole('row')).toHaveLength(101)
    fireEvent.click(screen.getByRole('button', { name: 'Next' }))
    expect(screen.getByText('101 traded wallets · Page 2 of 2')).toBeInTheDocument()
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
    expect(screen.getByRole('heading', { name: 'All traded wallets' })).toBeInTheDocument()
    expect(screen.queryByText('offline')).not.toBeInTheDocument()
  })
})
