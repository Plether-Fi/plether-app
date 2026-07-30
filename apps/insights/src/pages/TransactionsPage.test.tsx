import { fireEvent, render, screen, waitFor } from '@testing-library/react'
import { MemoryRouter, Route, Routes, useLocation } from 'react-router-dom'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { TransactionsPage } from './TransactionsPage'

const apiMocks = vi.hoisted(() => ({
  useCurrentProtocolRelease: vi.fn(),
  useProtocolTransactions: vi.fn(),
}))

vi.mock('../api', () => apiMocks)

const account = '0x1111111111111111111111111111111111111111'
const keeper = '0x2222222222222222222222222222222222222222'
const transactionHash = `0x${'3'.repeat(64)}`

beforeEach(() => {
  vi.clearAllMocks()
  apiMocks.useCurrentProtocolRelease.mockReturnValue({
    data: { releaseId: 'release-1' },
    isError: false,
    isLoading: false,
  })
  apiMocks.useProtocolTransactions.mockReturnValue(transactionQuery())
})

describe('TransactionsPage', () => {
  it('hydrates every supported filter from the URL and keys the query by that URL state', () => {
    const search = [
      'release=release-archive',
      'outcome=pending',
      'actionType=order_commitment',
      `address=${account}`,
      `account=${account}`,
      `keeper=${keeper}`,
      'contract=0xcontract',
      `transactionHash=${transactionHash}`,
      'from=1785000000',
      'to=1786000000',
    ].join('&')

    renderPage(`/transactions?${search}`)

    expect(screen.getByLabelText('Release')).toHaveValue('release-archive')
    expect(screen.getByLabelText('Action type')).toHaveValue('order_commitment')
    expect(screen.getByLabelText('Action state')).toHaveValue('pending')
    expect(screen.getByLabelText('Account or keeper')).toHaveValue(account)
    expect(screen.getByLabelText('Trading account')).toHaveValue(account)
    expect(screen.getByLabelText('Keeper')).toHaveValue(keeper)
    expect(screen.getByLabelText('Contract')).toHaveValue('0xcontract')
    expect(screen.getByLabelText('Transaction hash')).toHaveValue(transactionHash)
    expect(screen.getByLabelText('From · Unix seconds')).toHaveValue('1785000000')
    expect(screen.getByLabelText('To · Unix seconds')).toHaveValue('1786000000')
    expect(apiMocks.useProtocolTransactions).toHaveBeenLastCalledWith(
      'release-archive',
      {
        actionType: 'order_commitment',
        outcome: 'pending',
        address: account,
        account,
        keeper,
        contract: '0xcontract',
        transactionHash,
        from: '1785000000',
        to: '1786000000',
        limit: 50,
      },
      search,
    )
  })

  it('writes trimmed filters to the URL and clears both the URL and query filters', async () => {
    renderPage('/transactions')

    fireEvent.change(screen.getByLabelText('Action type'), {
      target: { value: 'liquidation' },
    })
    fireEvent.change(screen.getByLabelText('Release'), {
      target: { value: '  release-archive  ' },
    })
    fireEvent.change(screen.getByLabelText('Action state'), {
      target: { value: 'success' },
    })
    fireEvent.change(screen.getByLabelText('Account or keeper'), {
      target: { value: '  0xabc  ' },
    })
    fireEvent.change(screen.getByLabelText('From · Unix seconds'), {
      target: { value: ' 1785000000 ' },
    })
    fireEvent.change(screen.getByLabelText('To · Unix seconds'), {
      target: { value: '1786000000 ' },
    })
    fireEvent.click(screen.getByRole('button', { name: 'Apply filters' }))

    const appliedSearch =
      'release=release-archive&actionType=liquidation&outcome=success&address=0xabc&from=1785000000&to=1786000000'
    await waitFor(() => {
      expect(screen.getByTestId('location')).toHaveTextContent(
        `/transactions?${appliedSearch}`,
      )
    })
    expect(apiMocks.useProtocolTransactions).toHaveBeenLastCalledWith(
      'release-archive',
      {
        actionType: 'liquidation',
        outcome: 'success',
        address: '0xabc',
        account: undefined,
        keeper: undefined,
        contract: undefined,
        transactionHash: undefined,
        from: '1785000000',
        to: '1786000000',
        limit: 50,
      },
      appliedSearch,
    )

    fireEvent.click(screen.getByRole('button', { name: 'Clear' }))

    await waitFor(() => {
      expect(screen.getByTestId('location')).toHaveTextContent(/^\/transactions$/)
    })
    expect(screen.getByLabelText('Release')).toHaveValue('current')
    expect(apiMocks.useProtocolTransactions).toHaveBeenLastCalledWith(
      'release-1',
      {
        actionType: undefined,
        outcome: undefined,
        address: undefined,
        account: undefined,
        keeper: undefined,
        contract: undefined,
        transactionHash: undefined,
        from: undefined,
        to: undefined,
        limit: 50,
      },
      '',
    )
  })

  it('flattens loaded pages, preserves action deep links, and requests the next page', () => {
    const fetchNextPage = vi.fn()
    apiMocks.useProtocolTransactions.mockReturnValue(transactionQuery({
      data: {
        pages: [
          transactionsResponse([protocolAction({
            actionId: 'action-1',
            actionType: 'order_commitment',
            orderId: '42',
          })]),
          transactionsResponse([protocolAction({
            actionId: 'action-2',
            actionType: 'liquidation',
            orderId: null,
          })]),
        ],
      },
      hasNextPage: true,
      fetchNextPage,
    }))

    renderPage('/transactions')

    expect(screen.getByText(/2 action rows loaded/)).toBeInTheDocument()
    expect(screen.getByRole('cell', { name: 'Order Commitment' })).toBeInTheDocument()
    expect(screen.getByRole('cell', { name: 'Liquidation' })).toBeInTheDocument()
    expect(screen.getByRole('link', { name: 'Order #42' })).toHaveAttribute(
      'href',
      '/orders/release-1/42',
    )
    expect(screen.getAllByRole('link', { name: '0x1111…1111' })[0]).toHaveAttribute(
      'href',
      `/transactions?release=release-1&account=${account}`,
    )
    expect(screen.getAllByRole('link', { name: '0x3333…3333' })[0]).toHaveAttribute(
      'href',
      `/transactions/${transactionHash}?release=release-1`,
    )

    fireEvent.click(screen.getByRole('button', { name: 'Load more' }))

    expect(fetchNextPage).toHaveBeenCalledOnce()
  })

  it('queries an explicit release without waiting for current-release resolution', () => {
    apiMocks.useCurrentProtocolRelease.mockReturnValue({
      data: undefined,
      isError: false,
      isLoading: true,
    })

    renderPage('/transactions?release=release-archive')

    expect(apiMocks.useProtocolTransactions).toHaveBeenCalledWith(
      'release-archive',
      expect.objectContaining({ limit: 50 }),
      'release=release-archive',
    )
    expect(screen.getByText('Confirmed activity')).toBeInTheDocument()
  })
})

function renderPage(path: string) {
  return render(
    <MemoryRouter initialEntries={[path]}>
      <Routes>
        <Route path="/transactions" element={<TransactionsPage />} />
      </Routes>
      <Location />
    </MemoryRouter>,
  )
}

function Location() {
  const location = useLocation()
  return <p data-testid="location">{location.pathname}{location.search}</p>
}

function transactionQuery(overrides: Record<string, unknown> = {}) {
  return {
    data: {
      pages: [transactionsResponse([])],
    },
    isLoading: false,
    isError: false,
    hasNextPage: false,
    isFetchingNextPage: false,
    fetchNextPage: vi.fn(),
    refetch: vi.fn(),
    ...overrides,
  }
}

function transactionsResponse(items: ReturnType<typeof protocolAction>[]) {
  return {
    releaseId: 'release-1',
    chainId: '421614',
    confirmedBlock: {
      number: '123',
      hash: `0x${'4'.repeat(64)}`,
      timestamp: 1_785_000_000,
    },
    indexerTimestamp: 1_785_000_010,
    calculationVersion: 'protocol-transparency-v1',
    evidence: { actions: 'confirmed' },
    availability: [],
    transactions: {
      items,
      nextCursor: null,
      filters: {},
    },
  }
}

function protocolAction(overrides: Record<string, unknown> = {}) {
  return {
    actionId: 'action',
    transactionHash,
    blockNumber: '123',
    blockHash: `0x${'4'.repeat(64)}`,
    transactionIndex: '1',
    logIndex: '2',
    timestamp: 1_785_000_000,
    actionType: 'order_execution',
    outcome: 'success',
    account,
    keeper,
    orderId: '42',
    contractAddress: '0x4444444444444444444444444444444444444444',
    data: {},
    evidence: { level: 'exact' },
    units: {},
    ...overrides,
  }
}
