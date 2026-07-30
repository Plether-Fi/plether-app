import { fireEvent, render, screen, waitFor } from '@testing-library/react'
import { MemoryRouter, useLocation } from 'react-router-dom'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { AppRoutes } from './App'

const apiMocks = vi.hoisted(() => ({
  useCurrentProtocolRelease: vi.fn(),
  useInsightsStatus: vi.fn(),
}))

vi.mock('./api', async (importOriginal) => ({
  ...await importOriginal<typeof import('./api')>(),
  ...apiMocks,
}))

vi.mock('./pages/OverviewPage', () => ({
  OverviewPage: () => <p>protocol-overview</p>,
}))

vi.mock('./pages/TransactionsPage', () => ({
  TransactionsPage: () => <p>protocol-transactions</p>,
}))

vi.mock('./pages/LeaderboardPage', () => ({
  LeaderboardPage: () => <p>competition-leaderboard</p>,
}))

vi.mock('./pages/ProtocolWalletsPage', () => ({
  ProtocolWalletsPage: () => <p>protocol-wallets</p>,
}))

vi.mock('./pages/ProtocolWalletDetailPage', () => ({
  ProtocolWalletDetailPage: () => <p>protocol-wallet-detail</p>,
}))

vi.mock('./pages/WalletPage', () => ({
  WalletPage: () => <p>competition-wallet</p>,
}))

beforeEach(() => {
  vi.clearAllMocks()
  apiMocks.useInsightsStatus.mockReturnValue({
    data: undefined,
    isLoading: false,
    isError: true,
  })
})

describe('Protocol Explorer rollout routing', () => {
  it('renders the explorer homepage and protocol chrome when enabled', () => {
    mockRelease(true)

    renderRoutes('/')

    expect(screen.getByText('protocol-overview')).toBeInTheDocument()
    expect(screen.getByRole('searchbox')).toBeInTheDocument()
    expect(screen.getByRole('link', { name: 'Transactions' })).toBeInTheDocument()
  })

  it('redirects the homepage and protocol deep links to the retained competition when disabled', async () => {
    mockRelease(false)

    const { unmount } = renderRoutes('/')
    await waitFor(() => {
      expect(screen.getByText('competition-leaderboard')).toBeInTheDocument()
      expect(screen.getByTestId('location')).toHaveTextContent(
        '/competitions/testnet-trading-2026',
      )
    })
    expect(screen.queryByRole('searchbox')).not.toBeInTheDocument()
    expect(screen.queryByRole('link', { name: 'Overview' })).not.toBeInTheDocument()

    unmount()
    renderRoutes('/transactions')
    await waitFor(() => {
      expect(screen.queryByText('protocol-transactions')).not.toBeInTheDocument()
      expect(screen.getByTestId('location')).toHaveTextContent(
        '/competitions/testnet-trading-2026',
      )
    })
  })

  it('does not mount explorer content before the bootstrap flag resolves', () => {
    apiMocks.useCurrentProtocolRelease.mockReturnValue({
      data: undefined,
      isLoading: true,
      isError: false,
    })

    renderRoutes('/')

    expect(screen.getByRole('status')).toHaveTextContent(
      'Loading Protocol Explorer configuration',
    )
    expect(screen.queryByText('protocol-overview')).not.toBeInTheDocument()
    expect(screen.queryByText('competition-leaderboard')).not.toBeInTheDocument()
    expect(screen.queryByRole('searchbox')).not.toBeInTheDocument()
  })

  it('keeps the requested explorer route and offers a retry when bootstrap fails', () => {
    const refetch = vi.fn()
    apiMocks.useCurrentProtocolRelease.mockReturnValue({
      data: undefined,
      error: new Error('release manifest request failed'),
      isLoading: false,
      isError: true,
      refetch,
    })

    renderRoutes('/transactions')

    expect(screen.getByRole('heading', {
      name: 'Protocol Explorer configuration unavailable',
    })).toBeInTheDocument()
    expect(screen.getByText('release manifest request failed')).toBeInTheDocument()
    expect(screen.getByTestId('location')).toHaveTextContent('/transactions')
    expect(screen.queryByText('protocol-transactions')).not.toBeInTheDocument()
    expect(screen.queryByText('competition-leaderboard')).not.toBeInTheDocument()

    fireEvent.click(screen.getByRole('button', { name: 'Try again' }))
    expect(refetch).toHaveBeenCalledOnce()
  })

  it('leaves explicit competition routes unchanged while disabled', () => {
    mockRelease(false)

    renderRoutes('/competitions/summer-2026')

    expect(screen.getByText('competition-leaderboard')).toBeInTheDocument()
    expect(screen.getByTestId('location')).toHaveTextContent(
      '/competitions/summer-2026',
    )
  })

  it('uses a separate protocol-wallet namespace and preserves the legacy competition redirect', async () => {
    mockRelease(true)

    const { unmount } = renderRoutes('/protocol-wallets')
    expect(screen.getByText('protocol-wallets')).toBeInTheDocument()

    unmount()
    const detail = renderRoutes('/protocol-wallets/0x1234?release=release-1')
    expect(screen.getByText('protocol-wallet-detail')).toBeInTheDocument()

    detail.unmount()
    renderRoutes('/wallets/0x1234')
    await waitFor(() => {
      expect(screen.getByTestId('location')).toHaveTextContent(
        '/competitions/testnet-trading-2026/wallets/0x1234',
      )
    })
  })
})

function mockRelease(explorerEnabled: boolean) {
  apiMocks.useCurrentProtocolRelease.mockReturnValue({
    data: {
      releaseId: 'release-1',
      explorerEnabled,
    },
    error: null,
    isLoading: false,
    isError: false,
    refetch: vi.fn(),
  })
}

function renderRoutes(path: string) {
  return render(
    <MemoryRouter initialEntries={[path]}>
      <AppRoutes />
      <Location />
    </MemoryRouter>,
  )
}

function Location() {
  const location = useLocation()
  return <p data-testid="location">{location.pathname}{location.search}</p>
}
