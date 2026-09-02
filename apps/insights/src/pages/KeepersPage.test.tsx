import { fireEvent, render, screen } from '@testing-library/react'
import { MemoryRouter } from 'react-router-dom'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { KeepersPage } from './KeepersPage'

const apiMocks = vi.hoisted(() => ({
  useCurrentProtocolRelease: vi.fn(),
  useKeepers: vi.fn(),
}))

vi.mock('../api', () => apiMocks)

const keeperAddress = '0x1111111111111111111111111111111111111111'

beforeEach(() => {
  vi.clearAllMocks()
  apiMocks.useCurrentProtocolRelease.mockReturnValue({
    data: { releaseId: 'release-1' },
    isLoading: false,
  })
  apiMocks.useKeepers.mockReturnValue({
    data: {
      pages: [{
        releaseId: 'release-1',
        chainId: '421614',
        confirmedBlock: {
          number: '123',
          hash: '0xblock',
          timestamp: 1_785_000_000,
        },
        indexerTimestamp: 1_785_000_010,
        calculationVersion: 'protocol-transparency-v1',
        evidence: {
          rewards: 'exact_liquidation_terminal_events',
        },
        availability: [{
          field: 'keeper.totalGrossRewardsUsdc',
          reason: 'keeper_total_gross_rewards_unavailable',
        }],
        keepers: {
          window: '24h',
          definition: 'A successful permissionless protocol action sender.',
          activeKeeperCount: '1',
          actionCount: '3',
          backlogProcessed: '2',
          actionMix: {
            executions: '1',
            cleanups: '1',
            liquidations: '1',
          },
          latencySeconds: {
            commitToTerminalMedian: '8',
            commitToTerminalP90: '12',
            commitToTerminalP99: '14',
          },
          observedLiquidationRewardsUsdc: '2500000',
          totalGrossRewardsUsdc: null,
          nativeGasAndPythCosts: {
            gasCostWei: '1200000000000000',
            transactionNativeValueWei: '500000000000000',
            missingGasReceiptCount: '0',
            missingNativeValueCount: '0',
            nativeValueInterpretation: 'Exact transaction value; the Pyth component is not isolated.',
            profitUsdc: null,
          },
          observedRewardConcentration: {
            topOneShareBps: '10000',
            topThreeShareBps: '10000',
            slices: [{
              address: keeperAddress,
              observedLiquidationRewardsUsdc: '2500000',
            }],
          },
          keepers: [{
            address: keeperAddress,
            observedLiquidationRewardsUsdc: '2500000',
            actionCount: '3',
            executions: '1',
            cleanups: '1',
            liquidations: '1',
            nativeCosts: {
              gasCostWei: '1200000000000000',
            },
          }],
          nextCursor: null,
          units: {
            observedLiquidationRewardsUsdc: 'USDC:6',
          },
        },
      }],
    },
    isError: false,
    isLoading: false,
    hasNextPage: false,
    isFetchingNextPage: false,
    fetchNextPage: vi.fn(),
    refetch: vi.fn(),
  })
})

describe('KeepersPage', () => {
  it('labels liquidation-only telemetry without presenting it as total keeper rewards', () => {
    render(
      <MemoryRouter initialEntries={['/keepers?window=24h']}>
        <KeepersPage />
      </MemoryRouter>,
    )

    expect(apiMocks.useKeepers).toHaveBeenLastCalledWith('release-1', '24h')
    expect(screen.getAllByText('Observed liquidation bounties').length).toBeGreaterThan(0)
    expect(screen.getAllByText('2.50 USDC').length).toBeGreaterThan(0)
    expect(screen.getByText('Liquidation-bounty concentration')).toBeInTheDocument()
    expect(screen.getAllByText('0.001200 ETH').length).toBeGreaterThan(0)
    expect(screen.getByText(/This is not a view of total keeper earnings/)).toBeInTheDocument()
    expect(screen.queryByText(/^Gross rewards$/i)).not.toBeInTheDocument()
    expect(screen.getByText((_, element) =>
      element?.tagName === 'LI'
      && element.textContent?.includes('Keeper Total Gross Rewards Unavailable') === true,
    )).toBeInTheDocument()
  })

  it('keeps the selected activity window in the URL-backed query', () => {
    render(
      <MemoryRouter initialEntries={['/keepers?window=24h']}>
        <KeepersPage />
      </MemoryRouter>,
    )

    fireEvent.click(screen.getByRole('button', { name: '30d' }))

    expect(apiMocks.useKeepers).toHaveBeenLastCalledWith('release-1', '30d')
  })

  it('queries an archived release without waiting for current bootstrap and preserves it in keeper links', () => {
    apiMocks.useCurrentProtocolRelease.mockReturnValue({
      data: undefined,
      isLoading: true,
      isError: false,
    })

    render(
      <MemoryRouter initialEntries={['/keepers?release=release-archive&window=24h']}>
        <KeepersPage />
      </MemoryRouter>,
    )

    expect(apiMocks.useKeepers).toHaveBeenLastCalledWith('release-archive', '24h')
    expect(screen.getByRole('link', { name: '0x1111…1111' })).toHaveAttribute(
      'href',
      `/keepers/${keeperAddress}?release=release-archive&window=24h`,
    )
    expect(screen.queryByLabelText('Loading')).not.toBeInTheDocument()

    fireEvent.click(screen.getByRole('button', { name: '30d' }))

    expect(apiMocks.useKeepers).toHaveBeenLastCalledWith('release-archive', '30d')
    expect(screen.getByRole('link', { name: '0x1111…1111' })).toHaveAttribute(
      'href',
      `/keepers/${keeperAddress}?release=release-archive&window=30d`,
    )
  })

  it('flattens and deduplicates anchored pages and loads more keeper addresses', () => {
    const fetchNextPage = vi.fn()
    const firstPage = apiMocks.useKeepers().data.pages[0]
    apiMocks.useKeepers.mockClear()
    const secondAddress = '0x2222222222222222222222222222222222222222'
    apiMocks.useKeepers.mockReturnValue({
      data: {
        pages: [
          {
            ...firstPage,
            keepers: {
              ...firstPage.keepers,
              nextCursor: 'keeper-page-2',
            },
          },
          {
            ...firstPage,
            confirmedBlock: { ...firstPage.confirmedBlock, number: '999' },
            availability: [
              ...firstPage.availability,
              { field: 'keeper.revealReadyLatency', reason: 'oracle_publish_times_not_indexed' },
            ],
            keepers: {
              ...firstPage.keepers,
              activeKeeperCount: '999',
              keepers: [
                ...firstPage.keepers.keepers,
                {
                  address: secondAddress,
                  actionCount: '1',
                  executions: '1',
                  cleanups: '0',
                  liquidations: '0',
                  observedLiquidationRewardsUsdc: '0',
                  nativeCosts: { gasCostWei: '1000' },
                },
              ],
              nextCursor: null,
            },
          },
        ],
      },
      isError: false,
      isLoading: false,
      hasNextPage: true,
      isFetchingNextPage: false,
      fetchNextPage,
      refetch: vi.fn(),
    })

    render(
      <MemoryRouter initialEntries={['/keepers?window=24h']}>
        <KeepersPage />
      </MemoryRouter>,
    )

    expect(screen.getByText(/2 unique keeper addresses loaded across 2 anchored pages/)).toBeInTheDocument()
    expect(screen.getByText('Active keepers').parentElement).toHaveTextContent('1')
    expect(screen.getByText('Active keepers').parentElement).not.toHaveTextContent('999')
    expect(screen.getAllByRole('link', { name: '0x1111…1111' })).toHaveLength(1)
    expect(screen.getByRole('link', { name: '0x2222…2222' })).toBeInTheDocument()
    const confirmedBlocks = screen.getAllByText('Confirmed block')
      .map((label) => label.parentElement?.textContent ?? '')
    expect(confirmedBlocks).toContain('Confirmed block 123')
    expect(confirmedBlocks).not.toContain('Confirmed block 999')
    expect(screen.getByText('2 fields unavailable or incomplete')).toBeInTheDocument()

    fireEvent.click(screen.getByRole('button', { name: 'Load more keeper addresses' }))
    expect(fetchNextPage).toHaveBeenCalledOnce()
  })
})
