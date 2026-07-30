import { fireEvent, render, screen, waitFor, within } from '@testing-library/react'
import { MemoryRouter } from 'react-router-dom'
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest'
import { ParametersPage } from './ParametersPage'

const apiMocks = vi.hoisted(() => ({
  useCurrentProtocolRelease: vi.fn(),
  useParameterChanges: vi.fn(),
  useParameters: vi.fn(),
}))

vi.mock('../api', () => apiMocks)

const storageKey = 'plether.insights.protocol.parameter-changes.seen.v1.release-1'
const governanceTxHash = `0x${'a'.repeat(64)}`

beforeEach(() => {
  vi.clearAllMocks()
  vi.stubGlobal('localStorage', memoryStorage())
  apiMocks.useCurrentProtocolRelease.mockReturnValue({
    data: { releaseId: 'release-1' },
    isLoading: false,
  })
  apiMocks.useParameters.mockReturnValue({
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
      evidence: {},
      availability: [],
      parameters: {
        current: [],
        pending: [],
        history: [{
          changeId: 'embedded-capped-history',
          parameterKey: 'embedded.shouldNotRender',
          status: 'executed',
          txHash: '0xembedded',
        }],
        parameterChangesPath:
          '/api/insights/v1/protocol/releases/release-1/parameter-changes',
        catalogVersion: 'protocol-parameters-v1',
      },
    },
    isError: false,
    isLoading: false,
  })
  apiMocks.useParameterChanges.mockReturnValue(parameterChangesQuery([
    parameterChangesPage({
      items: [{
        changeId: 'old-change',
        parameterKey: 'fees.protocolFeeBps',
        status: 'executed',
        txHash: '0xold',
      }, {
        changeId: 'new-dependency-change',
        parameterKey: 'dependencies.oracle',
        status: 'executed',
        txHash: governanceTxHash,
      }],
    }),
  ]))
})

afterEach(() => {
  vi.unstubAllGlobals()
})

describe('ParametersPage', () => {
  it('highlights governance changes not present during the previous view', async () => {
    localStorage.setItem(storageKey, JSON.stringify(['old-change']))

    render(
      <MemoryRouter initialEntries={['/parameters?view=history']}>
        <ParametersPage />
      </MemoryRouter>,
    )

    expect(screen.getByRole('status')).toHaveTextContent('1 governance change observed since your previous view.')
    expect(screen.getByText('New since last view')).toBeInTheDocument()
    expect(screen.getByText('dependencies.oracle')).toBeInTheDocument()
    await waitFor(() => {
      expect(JSON.parse(localStorage.getItem(storageKey) ?? '[]')).toEqual([
        'old-change',
        'new-dependency-change',
      ])
    })
  })

  it('establishes a baseline without marking all changes as new on first visit', async () => {
    render(
      <MemoryRouter initialEntries={['/parameters?view=history']}>
        <ParametersPage />
      </MemoryRouter>,
    )

    expect(screen.queryByRole('status')).not.toBeInTheDocument()
    expect(screen.queryByText('New since last view')).not.toBeInTheDocument()
    await waitFor(() => {
      expect(localStorage.getItem(storageKey)).not.toBeNull()
    })
  })

  it('preserves the release when linking governance history to a transaction', () => {
    render(
      <MemoryRouter initialEntries={['/parameters?view=history']}>
        <ParametersPage />
      </MemoryRouter>,
    )

    expect(screen.getByRole('link', { name: governanceTxHash })).toHaveAttribute(
      'href',
      `/transactions/${governanceTxHash}?release=release-1`,
    )
  })

  it('renders the full governance lifecycle without requiring raw evidence expansion', () => {
    apiMocks.useParameterChanges.mockReturnValue(parameterChangesQuery([
      parameterChangesPage({
        items: [{
          changeId: 'full-lifecycle',
          parameterKey: 'market.vpi_factor',
          status: 'executed',
          oldValue: '100',
          newValue: '125',
          proposer: '0x1111111111111111111111111111111111111111',
          executor: '0x2222222222222222222222222222222222222222',
          proposedAt: '1785000000',
          eta: '1785003600',
          executedAt: '1785007200',
          txHash: governanceTxHash,
        }],
      }),
    ]))

    render(
      <MemoryRouter initialEntries={['/parameters?view=history']}>
        <ParametersPage />
      </MemoryRouter>,
    )

    expect(screen.getByText('Old value').parentElement).toHaveTextContent('100')
    expect(screen.getByText('New value').parentElement).toHaveTextContent('125')
    expect(screen.getByText('Proposer').parentElement).toHaveTextContent('0x1111111111111111111111111111111111111111')
    expect(screen.getByText('Executor').parentElement).toHaveTextContent('0x2222222222222222222222222222222222222222')
    expect(screen.getByText('Proposed at').parentElement).not.toHaveTextContent('Unavailable')
    expect(screen.getByText('ETA').parentElement).not.toHaveTextContent('Unavailable')
    expect(screen.getByText('Executed at').parentElement).not.toHaveTextContent('Unavailable')
  })

  it('renders the machine-readable reason for an unavailable current value', () => {
    const current = apiMocks.useParameters()
    apiMocks.useParameters.mockReturnValue({
      ...current,
      data: {
        ...current.data,
        parameters: {
          ...current.data.parameters,
          current: [{
            definition: {
              key: 'oracle.maxAge',
              group: 'Oracle',
              displayUnit: 'seconds',
              sourceContract: 'pletherOracle',
              getter: 'maxAge()',
              description: 'Maximum accepted oracle age.',
              riskInterpretation: 'Higher permits older prices.',
              timelockPolicy: 'admin_timelock',
            },
            rawValue: null,
            formattedValue: null,
            effectiveBlock: '123',
            sourceAddress: '0x1111111111111111111111111111111111111111',
            evidence: 'unavailable',
            availability: [{
              field: 'oracle.maxAge',
              reason: 'archive_state_unavailable',
            }],
          }],
        },
      },
    })

    render(
      <MemoryRouter initialEntries={['/parameters?view=current']}>
        <ParametersPage />
      </MemoryRouter>,
    )

    expect(screen.getByLabelText('oracle.maxAge availability')).toHaveTextContent(
      'oracle.maxAge: archive_state_unavailable',
    )
  })

  it('shows scale, governance policy, source address, and safe documentation for current values', () => {
    const current = apiMocks.useParameters()
    const sourceAddress = '0x1111111111111111111111111111111111111111'
    apiMocks.useParameters.mockReturnValue({
      ...current,
      data: {
        ...current.data,
        parameters: {
          ...current.data.parameters,
          current: [{
            definition: {
              key: 'fees.protocol_fee_bps',
              group: 'Fees',
              rawScale: '10000',
              displayUnit: 'bps',
              sourceContract: 'housePool',
              getter: 'protocolFeeBps()',
              description: 'Protocol fee charged to a settlement.',
              riskInterpretation: 'Higher values increase trader cost.',
              mutability: 'governance',
              timelockPolicy: 'admin_timelock',
              documentationLink: '/methodology#protocol-parameters',
            },
            rawValue: '25',
            formattedValue: '25 bps',
            effectiveBlock: '123',
            sourceAddress,
            evidence: 'exact_historical_contract_read',
            availability: [],
          }],
        },
      },
    })

    render(
      <MemoryRouter initialEntries={['/parameters?view=current']}>
        <ParametersPage />
      </MemoryRouter>,
    )

    expect(screen.getByText('Raw scale:').parentElement).toHaveTextContent('10000')
    expect(screen.getByText('Display unit:').parentElement).toHaveTextContent('bps')
    expect(screen.getByText('Mutability:').parentElement).toHaveTextContent('Governance')
    expect(screen.getByText('Timelock:').parentElement).toHaveTextContent('Admin Timelock')
    expect(screen.getByText(sourceAddress)).toBeInTheDocument()
    expect(screen.getByRole('link', { name: 'Parameter documentation' })).toHaveAttribute(
      'href',
      '/methodology#protocol-parameters',
    )
  })

  it('does not turn an unsafe documentation URL into a link', () => {
    const current = apiMocks.useParameters()
    apiMocks.useParameters.mockReturnValue({
      ...current,
      data: {
        ...current.data,
        parameters: {
          ...current.data.parameters,
          current: [{
            definition: {
              key: 'oracle.max_age',
              group: 'Oracle',
              rawScale: '1',
              displayUnit: 'seconds',
              sourceContract: 'oracle',
              getter: 'maxAge()',
              description: 'Maximum accepted oracle age.',
              riskInterpretation: 'Higher permits older prices.',
              mutability: 'governance',
              timelockPolicy: 'admin_timelock',
              documentationLink: 'javascript:alert(1)',
            },
            rawValue: '30',
            formattedValue: '30 seconds',
            effectiveBlock: '123',
            sourceAddress: '0x1111111111111111111111111111111111111111',
            evidence: 'exact_historical_contract_read',
            availability: [],
          }],
        },
      },
    })

    render(
      <MemoryRouter initialEntries={['/parameters?view=current']}>
        <ParametersPage />
      </MemoryRouter>,
    )

    expect(screen.queryByRole('link', { name: 'Parameter documentation' })).not.toBeInTheDocument()
    expect(screen.getByText('Documentation unavailable')).toBeInTheDocument()
  })

  it('separates governance status from evidence and shows the confirmed-block countdown', () => {
    const current = apiMocks.useParameters()
    apiMocks.useParameters.mockReturnValue({
      ...current,
      data: {
        ...current.data,
        parameters: {
          ...current.data.parameters,
          pending: [{
            changeId: 'direct:risk:max_skew:1785003661',
            parameterKey: 'market.max_skew',
            status: 'pending',
            oldValue: null,
            newValue: '125',
            eta: '1785003661',
            countdownSeconds: '3661',
            evidence: {
              level: 'partial',
              activationTime: 'exact_historical_contract_read',
              proposer: 'unavailable',
            },
            provenance: 'correlated_confirmed_log_projection',
          }],
        },
      },
    })

    render(
      <MemoryRouter initialEntries={['/parameters?view=pending']}>
        <ParametersPage />
      </MemoryRouter>,
    )

    expect(screen.getByLabelText('Status: Pending')).toHaveTextContent('Pending')
    const evidence = screen.getByLabelText('Evidence details: partial')
    expect(evidence).toHaveTextContent('exact_historical_contract_read')
    expect(evidence).toHaveTextContent('"proposer": "unavailable"')
    expect(screen.getByText('1h 1m 1s remaining')).toBeInTheDocument()
    expect(screen.getByText('3,661 seconds at confirmed block')).toBeInTheDocument()
    expect(screen.getByText('Provenance').parentElement).toHaveTextContent(
      'correlated_confirmed_log_projection',
    )
  })

  it('uses the dedicated anchored history feed, deduplicates pages, and loads more', () => {
    const fetchNextPage = vi.fn()
    const repeatedAvailability = {
      field: 'parameterChanges.executor',
      reason: 'executor_unavailable',
    }
    apiMocks.useParameterChanges.mockReturnValue(parameterChangesQuery([
      parameterChangesPage({
        blockNumber: '555',
        availability: [repeatedAvailability],
        items: [{
          changeId: 'new-dependency-change',
          parameterKey: 'dependencies.oracle',
          status: 'executed',
          txHash: '0xnew',
        }],
      }),
      parameterChangesPage({
        blockNumber: '999',
        availability: [
          repeatedAvailability,
          { field: 'parameterChanges.proposer', reason: 'proposer_unavailable' },
        ],
        items: [{
          changeId: 'new-dependency-change',
          parameterKey: 'dependencies.oracle',
          status: 'executed',
          txHash: '0xnew',
        }, {
          changeId: 'older-change',
          parameterKey: 'risk.maxLeverage',
          status: 'executed',
          txHash: '0xolder',
        }],
      }),
    ], { hasNextPage: true, fetchNextPage }))
    localStorage.setItem(storageKey, JSON.stringify([]))

    render(
      <MemoryRouter initialEntries={['/parameters?view=history']}>
        <ParametersPage />
      </MemoryRouter>,
    )

    expect(screen.queryByText('embedded.shouldNotRender')).not.toBeInTheDocument()
    expect(screen.getByText('2 unique changes loaded across 2 anchored pages.')).toBeInTheDocument()
    expect(screen.getAllByText('dependencies.oracle')).toHaveLength(1)
    expect(screen.getByText('2 fields unavailable or incomplete')).toBeInTheDocument()
    const confirmedBlocks = screen.getAllByText('Confirmed block')
      .map((label) => label.parentElement?.textContent ?? '')
    expect(confirmedBlocks).toContain('Confirmed block 555')
    expect(confirmedBlocks).not.toContain('Confirmed block 999')
    expect(within(screen.getByText('risk.maxLeverage').parentElement!).queryByText('New since last view')).not.toBeInTheDocument()

    fireEvent.click(screen.getByRole('button', { name: 'Load more governance history' }))
    expect(fetchNextPage).toHaveBeenCalledOnce()
  })
})

function parameterChangesQuery(
  pages: ReturnType<typeof parameterChangesPage>[],
  overrides: Record<string, unknown> = {},
) {
  return {
    data: { pages },
    isError: false,
    isLoading: false,
    hasNextPage: false,
    isFetchingNextPage: false,
    fetchNextPage: vi.fn(),
    refetch: vi.fn(),
    ...overrides,
  }
}

function parameterChangesPage({
  items,
  blockNumber = '456',
  availability = [],
}: {
  items: Record<string, unknown>[]
  blockNumber?: string
  availability?: { field: string; reason: string }[]
}) {
  return {
    releaseId: 'release-1',
    chainId: '421614',
    confirmedBlock: {
      number: blockNumber,
      hash: `0x${blockNumber.padStart(64, '0')}`,
      timestamp: 1_785_000_000,
    },
    indexerTimestamp: 1_785_000_010,
    calculationVersion: 'protocol-transparency-v1',
    evidence: { changes: 'confirmed_governance_events' },
    availability,
    parameterChanges: {
      items,
      nextCursor: null,
    },
  }
}

function memoryStorage(): Storage {
  const values = new Map<string, string>()
  return {
    get length() {
      return values.size
    },
    clear() {
      values.clear()
    },
    getItem(key) {
      return values.get(key) ?? null
    },
    key(index) {
      return [...values.keys()][index] ?? null
    },
    removeItem(key) {
      values.delete(key)
    },
    setItem(key, value) {
      values.set(key, value)
    },
  }
}
