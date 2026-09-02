import { render, screen } from '@testing-library/react'
import { MemoryRouter } from 'react-router-dom'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { HousePoolPage } from './HousePoolPage'

const apiMocks = vi.hoisted(() => ({
  useCurrentProtocolRelease: vi.fn(),
  useHousePool: vi.fn(),
}))

vi.mock('../api', () => apiMocks)

beforeEach(() => {
  vi.clearAllMocks()
  apiMocks.useCurrentProtocolRelease.mockReturnValue({
    data: { releaseId: 'release-1' },
    isLoading: false,
  })
  apiMocks.useHousePool.mockReturnValue({
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
      evidence: {
        poolState: 'exact_historical_contract_read',
        boundedLiability: 'exact_historical_contract_read',
        coverageRatio: 'derived',
        solvencyHeadroom: 'derived',
        traderClaims: 'exact_historical_contract_read',
        badDebt: 'exact_historical_contract_read',
        protocolStatus: 'exact_historical_contract_read',
        governanceState: 'strict_exact_historical_admin_getter_reads_at_same_block',
      },
      availability: [],
      housePool: {
        accounting: {
          totalAssetsUsdc: '10000000',
          freeUsdc: '9000000',
        },
        boundedLiability: {
          longUsdc: '8000000',
          shortUsdc: '3000000',
          totalUsdc: '11000000',
        },
        grossCoverageRatioBps: '12500',
        solvencyHeadroomUsdc: '2000000',
        traderClaimsUsdc: '1500000',
        badDebtUsdc: '250000',
        protocolStatus: {
          phase: '1',
          oracleFrozen: false,
          fadWindow: true,
          tradingActive: true,
          withdrawalLive: false,
        },
        governanceState: [{
          definition: {
            key: 'house_pool.owner',
            sourceContract: 'house_pool',
            getter: 'owner()',
            description: 'Current HousePool owner.',
          },
          rawValue: '0x1111111111111111111111111111111111111111',
          formattedValue: '0x1111111111111111111111111111111111111111',
          sourceAddress: '0x2222222222222222222222222222222222222222',
          evidence: 'exact_historical_contract_read',
        }],
        coverageBasis: {
          numerator: 'min(raw USDC balance, accounted totalAssetsUsdc)',
        },
        waterfall: {},
      },
    },
    isError: false,
    isLoading: false,
  })
})

describe('HousePoolPage', () => {
  it('uses max(long, short) for bounded liability and exposes the coverage basis', () => {
    render(
      <MemoryRouter>
        <HousePoolPage />
      </MemoryRouter>,
    )

    expect(screen.getByText('Maximum bounded liability').parentElement).toHaveTextContent('8.00 USDC')
    expect(screen.getByText('Maximum bounded liability').parentElement).not.toHaveTextContent('11.00 USDC')
    expect(screen.getByText(/directional liabilities are not added together/i)).toBeInTheDocument()
    expect(screen.getByText('Gross coverage ratio').parentElement).toHaveTextContent('125.00%')
    expect(screen.getByText('Gross coverage ratio').parentElement).toHaveTextContent('min(raw USDC balance, accounted totalAssetsUsdc)')
    expect(screen.getByText('Outstanding trader claims').parentElement).toHaveTextContent('1.50 USDC')
    expect(screen.getByText('Accumulated bad debt').parentElement).toHaveTextContent('0.25 USDC')
  })

  it('renders same-block protocol and HousePool governance state with provenance', () => {
    render(
      <MemoryRouter>
        <HousePoolPage />
      </MemoryRouter>,
    )

    const protocolStatus = screen.getByRole('heading', {
      name: 'Protocol operating state',
    }).closest('section')
    expect(protocolStatus).not.toBeNull()
    expect(protocolStatus).toHaveTextContent('Oracle Frozen')
    expect(protocolStatus).toHaveTextContent('Fad Window')
    expect(protocolStatus).toHaveTextContent('Trading Active')
    expect(protocolStatus).toHaveTextContent('exact_historical_contract_read')

    const governanceState = screen.getByRole('heading', {
      name: 'Governance and dependency state',
    }).closest('section')
    expect(governanceState).not.toBeNull()
    expect(governanceState).toHaveTextContent('house_pool.owner')
    expect(governanceState).toHaveTextContent('owner()')
    expect(governanceState).toHaveTextContent('strict_exact_historical_admin_getter_reads_at_same_block')
    expect(governanceState).toHaveTextContent('exact_historical_contract_read')
  })
})
