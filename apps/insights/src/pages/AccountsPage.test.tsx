import { fireEvent, render, screen, within } from '@testing-library/react'
import { MemoryRouter, Route, Routes } from 'react-router-dom'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { InsightsApiError, type WalletDirectoryResponse } from '../api'
import { resolvePerpsChain } from '../utils/chain'
import { AccountsPage } from './AccountsPage'

const protocolContext = vi.hoisted(() => vi.fn())
const walletsDirectoryQuery = vi.hoisted(() => vi.fn())

vi.mock('../hooks/useProtocolContext', () => ({ useProtocolContext: protocolContext }))
vi.mock('../api', async (importOriginal) => ({
  ...(await importOriginal<typeof import('../api')>()),
  useWalletsDirectory: walletsDirectoryQuery,
}))

const sampleDirectory: WalletDirectoryResponse = {
  network: { chainId: 421614, releaseVersion: 'v2.0.0', settlementAssetSymbol: 'USDG' },
  observedAt: '2026-11-27T14:00:00Z',
  currentMarkPrice: '94605602',
  markObservedAt: '2026-11-27T13:59:55Z',
  summary: {
    totalAccounts: 1420,
    accountsWithOpenPosition: 1100,
    longCount: 620,
    shortCount: 480,
    longOpenInterestAssets: '216585334940000',
    shortOpenInterestAssets: '173851227670000',
    totalOpenInterestAssets: '390436562610000',
    netSkewAssets: '42734107270000',
    avgLeverageX100: 117,
    active24hCount: 185,
    discordLinkedCount: 412,
    totalVolumeAssets: '950000000000000',
  },
  totalMatched: 2,
  limit: 50,
  offset: 0,
  items: [
    {
      account: `0x${'a'.repeat(40)}`,
      discordUsername: 'macro_alice',
      divisionsTier: 'Gold',
      openSide: 'long',
      openSize: '50000000000000000000000',
      openNotionalAssets: '47302801000',
      totalTrades: 18,
      opensCount: 9,
      closesCount: 9,
      liquidationsCount: 0,
      volumeAssets: '420000000000',
      realizedPnlAssets: '1250000000',
      firstActivityAt: '2026-11-10T10:00:00Z',
      lastActivityAt: '2026-11-27T13:45:00Z',
    },
    {
      account: `0x${'b'.repeat(40)}`,
      discordUsername: null,
      divisionsTier: null,
      openSide: 'short',
      openSize: '25000000000000000000000',
      openNotionalAssets: '23651400500',
      totalTrades: 4,
      opensCount: 2,
      closesCount: 2,
      liquidationsCount: 0,
      volumeAssets: '90000000000',
      realizedPnlAssets: '-320000000',
      firstActivityAt: '2026-11-20T10:00:00Z',
      lastActivityAt: '2026-11-27T12:15:00Z',
    },
  ],
}

beforeEach(() => {
  protocolContext.mockReturnValue({
    chainId: 421614,
    chain: resolvePerpsChain(421614),
    releaseVersion: 'v2.0.0',
    blockNumber: '332900500',
    observedAt: '2026-11-27T14:00:00Z',
  })
  walletsDirectoryQuery.mockReturnValue({
    data: sampleDirectory,
    isLoading: false,
    isError: false,
    error: null,
    refetch: vi.fn(),
  })
})

function renderAt(path = '/accounts') {
  return render(
    <MemoryRouter initialEntries={[path]}>
      <Routes>
        <Route path="/accounts" element={<AccountsPage />} />
      </Routes>
    </MemoryRouter>,
  )
}

describe('AccountsPage', () => {
  it('renders canonical OI KPIs, TraderIdentity rows, filter pills, and account/exposure drill-downs without $ prefixes', () => {
    const { container } = renderAt('/accounts?q=alice')
    expect(screen.getByRole('heading', { name: /Trading Accounts & Open Exposure/i })).toBeInTheDocument()
    expect(screen.getByText('1,420')).toBeInTheDocument()
    expect(screen.getAllByText(/Long 55\.5%/).length).toBeGreaterThanOrEqual(1)
    expect(screen.getAllByText(/Short 44\.5%/).length).toBeGreaterThanOrEqual(1)
    expect(container.textContent).not.toContain('$')

    // Search input hydrated from ?q=alice
    expect(screen.getByRole('searchbox')).toHaveValue('alice')

    const table = screen.getByRole('table', { name: /Active plDXY-PERP Trading Accounts/i })
    expect(within(table).getByText('@macro_alice')).toBeInTheDocument()
    expect(within(table).getByText('Gold')).toBeInTheDocument()
    expect(within(table).getByText('+1,250.00 USDG')).toBeInTheDocument()
    expect(within(table).getByText('-320.00 USDG')).toBeInTheDocument()

    // Filter pill updates query params
    fireEvent.click(screen.getByRole('button', { name: 'Open Position' }))
    expect(walletsDirectoryQuery).toHaveBeenLastCalledWith(
      expect.objectContaining({ filter: 'open', search: 'alice', sort: 'notional' }),
    )

    // Sort selector updates query params
    fireEvent.change(screen.getByLabelText('Sort'), { target: { value: 'pnl' } })
    expect(walletsDirectoryQuery).toHaveBeenLastCalledWith(
      expect.objectContaining({ filter: 'open', sort: 'pnl' }),
    )
  })

  it('fails closed cleanly when backend directory endpoint returns 404', () => {
    walletsDirectoryQuery.mockReturnValue({
      data: undefined,
      isLoading: false,
      isError: true,
      error: new InsightsApiError('Not found', 404),
      refetch: vi.fn(),
    })
    renderAt('/accounts')
    expect(screen.getByText('Trading Accounts directory awaiting backend deploy')).toBeInTheDocument()
  })
})
