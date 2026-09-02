import { fireEvent, render, screen } from '@testing-library/react'
import { MemoryRouter, useLocation } from 'react-router-dom'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { Layout } from './Layout'

const apiMocks = vi.hoisted(() => ({
  useInsightsStatus: vi.fn(),
}))

vi.mock('../api', async (importOriginal) => ({
  ...await importOriginal<typeof import('../api')>(),
  ...apiMocks,
}))

beforeEach(() => {
  vi.clearAllMocks()
  apiMocks.useInsightsStatus.mockReturnValue({
    data: {
      healthy: true,
      latestIndexedAt: 1_785_000_000,
      latestIndexedBlock: 123,
    },
    isLoading: false,
    isError: false,
  })
})

describe('Layout protocol search', () => {
  it.each([
    [`0x${'a'.repeat(64)}`, `/transactions/0x${'a'.repeat(64)}`],
    ['42', '/orders/release-1/42'],
    [`0x${'b'.repeat(40)}`, `/transactions?address=0x${'b'.repeat(40)}`],
  ])('routes %s to %s', (query, expectedLocation) => {
    render(
      <MemoryRouter>
        <Layout explorerEnabled protocolReleaseId="release-1"><Location /></Layout>
      </MemoryRouter>,
    )

    fireEvent.change(screen.getByRole('searchbox'), { target: { value: query } })
    fireEvent.click(screen.getByRole('button', { name: 'Search' }))

    expect(screen.getByTestId('location')).toHaveTextContent(expectedLocation)
  })

  it('hides protocol navigation and search when the explorer is disabled', () => {
    render(
      <MemoryRouter>
        <Layout><Location /></Layout>
      </MemoryRouter>,
    )

    expect(screen.queryByRole('searchbox')).not.toBeInTheDocument()
    expect(screen.queryByRole('link', { name: 'Overview' })).not.toBeInTheDocument()
    expect(screen.queryByRole('link', { name: 'Transactions' })).not.toBeInTheDocument()
    expect(screen.getByRole('link', { name: 'Leaderboard' })).toBeInTheDocument()
  })

  it('links operational wallets without replacing the competition wallet namespace', () => {
    render(
      <MemoryRouter>
        <Layout explorerEnabled protocolReleaseId="release-1"><Location /></Layout>
      </MemoryRouter>,
    )

    expect(screen.getByRole('link', { name: 'Wallets' })).toHaveAttribute(
      'href',
      '/protocol-wallets',
    )
  })
})

function Location() {
  const location = useLocation()
  return <p data-testid="location">{location.pathname}{location.search}</p>
}
