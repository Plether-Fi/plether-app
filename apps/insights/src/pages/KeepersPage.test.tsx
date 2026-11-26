import { fireEvent, render, screen, within } from '@testing-library/react'
import { MemoryRouter } from 'react-router-dom'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { resolvePerpsChain } from '../utils/chain'
import { KeepersPage } from './KeepersPage'

const protocolContext = vi.hoisted(() => vi.fn())
vi.mock('../hooks/useProtocolContext', () => ({ useProtocolContext: protocolContext }))

beforeEach(() => {
  protocolContext.mockReturnValue({
    chainId: 421614,
    chain: resolvePerpsChain(421614),
    releaseVersion: 'v2.0.0',
    blockNumber: '332879274',
    observedAt: '2026-11-26T11:37:00Z',
  })
})

describe('KeepersPage', () => {
  it('presents a neutral permissionless table without hardcoding Plether-operated signers', () => {
    render(
      <MemoryRouter>
        <KeepersPage />
      </MemoryRouter>,
    )

    expect(screen.getByRole('heading', { name: /Detected Keepers, Latency/ })).toBeInTheDocument()
    const table = screen.getByRole('table', { name: /Permissionless on-chain keepers/ })
    expect(within(table).getAllByRole('columnheader').map((header) => header.textContent)).toEqual([
      'Keeper address',
      'Observed roles',
      'Executions (24h)',
      'Share',
      'Latency (p50 / p95)',
      'ETH balance',
      'Bounties earned',
      'Last active',
    ])
    expect(screen.queryByText('Alto Bundler')).not.toBeInTheDocument()
  })

  it('filters role context across permissionless contract surfaces', () => {
    render(
      <MemoryRouter>
        <KeepersPage />
      </MemoryRouter>,
    )

    const liquidationButton = screen.getByRole('button', { name: 'Liquidation' })
    fireEvent.click(liquidationButton)
    expect(liquidationButton).toHaveAttribute('aria-pressed', 'true')
    expect(screen.getByText(/detected executing liquidation \(CfdEngine\)/i)).toBeInTheDocument()
  })
})
