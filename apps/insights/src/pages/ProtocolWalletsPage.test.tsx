import { fireEvent, render, screen } from '@testing-library/react'
import { MemoryRouter } from 'react-router-dom'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { ProtocolWalletsPage } from './ProtocolWalletsPage'

const apiMocks = vi.hoisted(() => ({
  useCurrentProtocolRelease: vi.fn(),
  useProtocolWallets: vi.fn(),
}))

vi.mock('../api', () => apiMocks)

const walletAddress = '0x1111111111111111111111111111111111111111'

beforeEach(() => {
  vi.clearAllMocks()
  apiMocks.useCurrentProtocolRelease.mockReturnValue({
    data: { releaseId: 'release-current' },
    isLoading: false,
    isError: false,
  })
  apiMocks.useProtocolWallets.mockReturnValue({
    data: {
      pages: [walletsPage()],
    },
    isLoading: false,
    isError: false,
    hasNextPage: false,
    isFetchingNextPage: false,
    fetchNextPage: vi.fn(),
    refetch: vi.fn(),
  })
})

describe('ProtocolWalletsPage', () => {
  it('shows conservative funding diagnostics and explicitly marks unavailable updater telemetry', () => {
    renderPage('/protocol-wallets?release=release-archive&window=24h')

    expect(apiMocks.useProtocolWallets).toHaveBeenLastCalledWith('release-archive', '24h')
    expect(screen.getByText('Oracle updater wallet identity unavailable')).toBeInTheDocument()
    expect(screen.getByText('0.009000 ETH')).toBeInTheDocument()
    expect(screen.getByText('9')).toBeInTheDocument()
    expect(screen.getByText('Critical')).toBeInTheDocument()
    expect(screen.getByText('Loaded wallet rows')).toBeInTheDocument()
    expect(screen.getByText('Low-funding loaded rows')).toBeInTheDocument()
    expect(screen.getByText('Oracle updater activity attribution unavailable')).toBeInTheDocument()
    expect(screen.getByText('Role-address balance is not proof of protocol liveness')).toBeInTheDocument()
    expect(screen.getByText(/Native refunds are not netted/)).toBeInTheDocument()
    expect(screen.getByText('partial structured evidence')).toBeInTheDocument()
    expect(screen.getByRole('link', { name: '0x1111…1111' })).toHaveAttribute(
      'href',
      `/protocol-wallets/${walletAddress}?window=24h&release=release-archive`,
    )
    expect(screen.getByText(/without guessing an operator identity/)).toBeInTheDocument()
  })

  it('preserves release selection when changing fixed windows', () => {
    renderPage('/protocol-wallets?release=release-archive&window=24h')

    fireEvent.click(screen.getByRole('button', { name: '30d' }))

    expect(apiMocks.useProtocolWallets).toHaveBeenLastCalledWith('release-archive', '30d')
  })

  it('does not call a published updater unavailable when its row is on a later page', () => {
    const page = walletsPage()
    apiMocks.useProtocolWallets.mockReturnValue({
      data: {
        pages: [{
          ...page,
          wallets: {
            ...page.wallets,
            oracleUpdaterIdentityAvailable: true,
            totalTrackedWalletCount: '2',
            nextCursor: 'wallet-page-2',
          },
        }],
      },
      isLoading: false,
      isError: false,
      hasNextPage: true,
      isFetchingNextPage: false,
      fetchNextPage: vi.fn(),
      refetch: vi.fn(),
    })

    renderPage('/protocol-wallets?release=release-archive&window=24h')

    expect(screen.getByText('Published (not loaded)')).toBeInTheDocument()
    expect(screen.getByText('Tracked wallets').parentElement).toHaveTextContent('2')
    expect(screen.getByText('Low-funding loaded rows')).toBeInTheDocument()
    expect(screen.queryByText('Oracle updater wallet identity unavailable')).not.toBeInTheDocument()
  })
})

function renderPage(path: string) {
  return render(
    <MemoryRouter initialEntries={[path]}>
      <ProtocolWalletsPage />
    </MemoryRouter>,
  )
}

function walletsPage() {
  return {
    releaseId: 'release-archive',
    chainId: '421614',
    confirmedBlock: {
      number: '123',
      hash: `0x${'1'.repeat(64)}`,
      timestamp: 1_785_000_000,
    },
    indexerTimestamp: 1_785_000_010,
    calculationVersion: 'protocol-transparency-v1',
    evidence: { wallets: { level: 'mixed' } },
    availability: [{
      field: 'wallets.oracleUpdater',
      reason: 'oracle_updater_identity_not_published_by_current_release',
    }, {
      field: 'wallets.oracleUpdaterActivity',
      reason: 'oracle_updater_activity_not_attributable_current_release',
    }],
    wallets: {
      window: '24h',
      windowStart: 1_784_913_600,
      windowEnd: 1_785_000_000,
      definition: 'Public release-scoped operational wallets.',
      items: [{
        address: walletAddress,
        roles: ['governance_executor'],
        roleSources: [{ role: 'governance_executor', source: 'release_manifest' }],
        status: 'critical',
        nativeBalanceWei: '9000000000000000',
        observedGasCostWei: '3000000000000000',
        observedTransactionNativeValueWei: '1000000000000000',
        observedActionCount: '3',
        observedTransactionCount: '2',
        medianObservedSuccessfulOperationalTransactionGrossNativeSpendWei: '1000000000000000',
        estimatedTransactionsAtObservedGrossSpend: '9',
        runwayFormula: {
          formulaIdentifier: 'native_balance_div_median_outlay_v1',
        },
        lastActivityTimestamp: 1_784_999_900,
        lastActivityTransactionHash: null,
        evidence: {
          nativeBalance: { level: 'exact' },
          runway: { level: 'unavailable' },
        },
        availability: [],
        raw: { address: walletAddress },
      }],
      nextCursor: null,
      oracleUpdaterIdentityAvailable: null,
      oracleUpdaterActivityAttributable: null,
      totalTrackedWalletCount: null,
      totalAtRiskWalletCount: null,
      units: { nativeBalanceWei: 'wei' },
    },
  }
}
