import { fireEvent, render, screen } from '@testing-library/react'
import { MemoryRouter, Route, Routes } from 'react-router-dom'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { ProtocolWalletDetailPage } from './ProtocolWalletDetailPage'

const apiMocks = vi.hoisted(() => ({
  useCurrentProtocolRelease: vi.fn(),
  useProtocolWallet: vi.fn(),
}))

vi.mock('../api', () => apiMocks)

const walletAddress = '0x1111111111111111111111111111111111111111'
const transactionHash = `0x${'a'.repeat(64)}`

beforeEach(() => {
  vi.clearAllMocks()
  apiMocks.useCurrentProtocolRelease.mockReturnValue({
    data: { releaseId: 'release-current' },
    isLoading: false,
    isError: false,
  })
  apiMocks.useProtocolWallet.mockReturnValue({
    data: { pages: [walletPage()] },
    isLoading: false,
    isError: false,
    hasNextPage: false,
    isFetchingNextPage: false,
    fetchNextPage: vi.fn(),
    refetch: vi.fn(),
  })
})

describe('ProtocolWalletDetailPage', () => {
  it('shows exact balance, derived gross-spend diagnostic, role provenance, and transaction evidence', () => {
    renderPage(`/protocol-wallets/${walletAddress}?release=release-archive&window=7d`)

    expect(apiMocks.useProtocolWallet).toHaveBeenCalledWith(
      'release-archive',
      walletAddress,
      '7d',
    )
    expect(screen.getByText('Native balance').parentElement).toHaveTextContent('0.009000 ETH')
    expect(screen.getByText('Estimated txs at gross spend').parentElement).toHaveTextContent('9')
    expect(screen.getByText('Role provenance')).toBeInTheDocument()
    expect(screen.getByText(`${walletAddress} · Governance Executor`)).toBeInTheDocument()
    expect(screen.getByText('Transaction-capacity calculation')).toBeInTheDocument()
    expect(screen.getByText('partial structured evidence')).toBeInTheDocument()
    expect(screen.getByRole('link', { name: '0xaaaa…aaaa' })).toHaveAttribute(
      'href',
      `/transactions/${transactionHash}?release=release-archive`,
    )
    expect(screen.getByText('2 fields unavailable or incomplete')).toBeInTheDocument()
    expect(screen.getByText(/Refunds and returned native value are not netted/)).toBeInTheDocument()
  })

  it('preserves release selection when changing the detail window', () => {
    renderPage(`/protocol-wallets/${walletAddress}?release=release-archive&window=7d`)

    fireEvent.click(screen.getByRole('button', { name: '30d' }))

    expect(apiMocks.useProtocolWallet).toHaveBeenLastCalledWith(
      'release-archive',
      walletAddress,
      '30d',
    )
  })

  it('renders unattributable updater activity and gross spend as unavailable, not zero', () => {
    const page = walletPage()
    apiMocks.useProtocolWallet.mockReturnValue({
      data: {
        pages: [{
          ...page,
          availability: [{
            field: 'wallet.oracleUpdater.activity',
            reason: 'oracle_updater_activity_not_attributable_current_release',
          }, {
            field: 'wallet.oracleUpdater.nativeOutlay',
            reason: 'oracle_updater_native_outlay_not_attributable_current_release',
          }],
          wallet: {
            ...page.wallet,
            roles: ['oracle_updater'],
            roleSources: [{
              role: 'oracle_updater',
              source: 'release_manifest',
              evidence: 'checked_in_public_configuration',
            }],
            status: 'no_cost_baseline',
            observedGasCostWei: null,
            observedTransactionNativeValueWei: null,
            observedActionCount: null,
            observedTransactionCount: null,
            medianObservedSuccessfulOperationalTransactionGrossNativeSpendWei: null,
            estimatedTransactionsAtObservedGrossSpend: null,
            lastActivityTimestamp: null,
            lastActivityTransactionHash: null,
            activity: [],
          },
        }],
      },
      isLoading: false,
      isError: false,
      hasNextPage: false,
      isFetchingNextPage: false,
      fetchNextPage: vi.fn(),
      refetch: vi.fn(),
    })

    renderPage(`/protocol-wallets/${walletAddress}?release=release-archive&window=7d`)

    expect(screen.getByText('Oracle updater activity attribution unavailable')).toBeInTheDocument()
    expect(screen.getByText('Estimated txs at gross spend').parentElement).toHaveTextContent('Unavailable')
    expect(screen.getByText('Last successful activity').parentElement).toHaveTextContent(
      'Unavailable actions across Unavailable distinct transactions',
    )
    expect(screen.queryByText(/0 actions/)).not.toBeInTheDocument()
  })
})

function renderPage(path: string) {
  return render(
    <MemoryRouter initialEntries={[path]}>
      <Routes>
        <Route path="/protocol-wallets/:address" element={<ProtocolWalletDetailPage />} />
      </Routes>
    </MemoryRouter>,
  )
}

function walletPage() {
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
    evidence: { wallet: { level: 'mixed' } },
    availability: [{
      field: 'wallet.nativeRefunds',
      reason: 'native_refunds_not_netted_without_trace_or_telemetry',
    }],
    wallet: {
      address: walletAddress,
      roles: ['governance_executor'],
      roleSources: [{
        role: 'governance_executor',
        source: 'release_manifest',
        evidence: 'checked_in_public_configuration',
      }],
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
        calculationVersion: 'protocol-transparency-v1',
        expression: 'balance / median outlay',
        sampleCount: '2',
      },
      lastActivityTimestamp: 1_784_999_900,
      lastActivityTransactionHash: transactionHash,
      evidence: {
        level: 'mixed',
        roles: 'exact_release_manifest',
        runway: 'derived_v1',
      },
      availability: [],
      raw: { address: walletAddress, nativeBalanceWei: '9000000000000000' },
      activity: [{
        activityId: 'action-1',
        transactionHash,
        timestamp: 1_784_999_900,
        actionType: 'governance_execution',
        outcome: 'success',
        gasCostWei: '1000000000000000',
        nativeValueWei: null,
        evidence: {
          action: { level: 'partial' },
          transaction: { level: 'exact_receipt' },
        },
        availability: [{
          field: 'activity.nativeValueWei',
          reason: 'transaction_native_value_unavailable',
        }],
        raw: { actionId: 'action-1', transactionHash },
      }],
      nextCursor: null,
    },
  }
}
