import { render, screen } from '@testing-library/react'
import { MemoryRouter } from 'react-router-dom'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { OverviewPage } from './OverviewPage'

const apiMocks = vi.hoisted(() => ({
  useCurrentProtocolRelease: vi.fn(),
  useProtocolOverview: vi.fn(),
}))

vi.mock('../api', () => apiMocks)

beforeEach(() => {
  vi.clearAllMocks()
  apiMocks.useCurrentProtocolRelease.mockReturnValue({
    data: { releaseId: 'release-1' },
    isError: false,
    isLoading: false,
  })
  apiMocks.useProtocolOverview.mockReturnValue({
    data: {
      releaseId: 'release-1',
      chainId: '421614',
      confirmedBlock: {
        number: '123',
        hash: '0xblock',
        timestamp: 1_785_000_000,
      },
      indexerTimestamp: 1_785_000_010,
      calculationVersion: 'protocol-transparency-v1',
      evidence: { overview: 'confirmed' },
      availability: [],
      overview: {
        counts: {
          indexedActions24h: '17',
          liquidations24h: '2',
          nonSuccessOutcomes24h: '3',
          activeKeepers24h: '4',
          pendingOrders: '5',
          ordersOlderThanMaxOrderAge: '1',
        },
        housePool: null,
        protocolStatus: null,
        anomalies: [],
        anomalyEvaluation: 'complete',
        indexerLagBlocks: '2',
      },
    },
    isError: false,
    isLoading: false,
  })
})

describe('OverviewPage', () => {
  it('labels action and outcome counts with their 24-hour window', () => {
    render(
      <MemoryRouter>
        <OverviewPage />
      </MemoryRouter>,
    )

    const metric = screen.getByText('Indexed actions · 24h').parentElement
    expect(metric).toHaveTextContent('17')
    expect(metric).toHaveTextContent('2 liquidations · 3 non-success outcomes in the same window.')
  })

  it('does not show a green all-clear when anomaly inputs are incomplete', () => {
    const current = apiMocks.useProtocolOverview()
    apiMocks.useProtocolOverview.mockReturnValue({
      ...current,
      data: {
        ...current.data,
        availability: [{ field: 'housePool', reason: 'archive_state_unavailable' }],
        overview: {
          ...current.data.overview,
          anomalyEvaluation: 'partial',
        },
      },
    })

    render(
      <MemoryRouter>
        <OverviewPage />
      </MemoryRouter>,
    )

    expect(screen.getByText(/No anomaly is currently proven/)).toBeInTheDocument()
    expect(screen.queryByText('No configured anomaly threshold is currently breached.')).not.toBeInTheDocument()
  })

  it('identifies operational-wallet funding anomalies and links to the release detail', () => {
    const address = '0x1111111111111111111111111111111111111111'
    const current = apiMocks.useProtocolOverview()
    apiMocks.useProtocolOverview.mockReturnValue({
      ...current,
      data: {
        ...current.data,
        overview: {
          ...current.data.overview,
          anomalies: [{
            code: 'operational_wallet_gross_spend_capacity_critical',
            severity: 'critical',
            message: 'A public operational wallet has low native funding.',
            details: {
              address,
              role: 'oracle_updater',
              nativeBalanceWei: '9000000000000000',
              estimatedTransactionsAtObservedGrossSpend: '9',
            },
          }],
        },
      },
    })

    render(
      <MemoryRouter>
        <OverviewPage />
      </MemoryRouter>,
    )

    expect(screen.getByRole('link', { name: `Open operational wallet ${address}` })).toHaveAttribute(
      'href',
      `/protocol-wallets/${address}?release=release-1`,
    )
    expect(screen.getByText('Oracle Updater')).toBeInTheDocument()
    expect(screen.getByText('Balance 0.009000 ETH')).toHaveAttribute(
      'title',
      '9000000000000000 wei',
    )
    expect(screen.getByText('Est. txs at gross spend 9')).toBeInTheDocument()
  })
})
