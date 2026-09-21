import { fireEvent, render, screen } from '@testing-library/react'
import { afterEach, describe, expect, it, vi } from 'vitest'
import type { VaultHistory } from '../api'
import { VaultAssetChart } from './VaultAssetChart'

const end = 1_800_000_000
function history(): VaultHistory {
  const points = [0, 1, 2].map((index) => ({
    timestamp: end - (2 - index) * 3600,
    blockNumber: String(index + 1),
    markFresh: true,
    sharePrice: '1000000000000000000',
    totalAssets: String((100 + index * 10) * 1e6),
    totalSupply: '100000000000',
    lockedAssets: String((40 + index * 5) * 1e6),
  }))
  return {
    range: '7d', intervalSeconds: 3600,
    deployment: { chainId: 421614, housePool: '0x1', seniorVault: '0x2', juniorVault: '0x3' },
    coverage: { complete: false, start: points[0].timestamp, end },
    senior: { apy7d: null, return7d: null, points },
    junior: { apy7d: null, return7d: null, points: points.map((point) => ({ ...point, totalAssets: '200000000', lockedAssets: '80000000' })) },
  }
}

afterEach(() => vi.restoreAllMocks())

function freezeTime() {
  vi.spyOn(Date, 'now').mockReturnValue((end + 300) * 1000)
}

describe('VaultAssetChart', () => {
  it('shows both series for the selected vault and supports keyboard inspection', () => {
    freezeTime()
    render(<VaultAssetChart tranche="senior" history={history()} />)
    const chart = screen.getByRole('img', { name: /Senior Vault 7-day/ })
    fireEvent.focus(chart)
    expect(screen.getByRole('status')).toHaveTextContent('$120.00')
    expect(screen.getByRole('status')).toHaveTextContent('$50.00')
    fireEvent.keyDown(chart, { key: 'Home' })
    expect(screen.getByRole('status')).toHaveTextContent('$100.00')
    expect(screen.getByRole('status')).toHaveTextContent('$40.00')
    fireEvent.keyDown(chart, { key: 'ArrowRight' })
    expect(screen.getByRole('status')).toHaveTextContent('$110.00')
    fireEvent.keyDown(chart, { key: 'End' })
    expect(screen.getByRole('status')).toHaveTextContent('$120.00')
    expect(screen.getByRole('status')).toHaveTextContent('$50.00')
    fireEvent.keyDown(chart, { key: 'Escape' })
    expect(screen.queryByRole('status')).not.toBeInTheDocument()
  })

  it('uses junior values on the junior vault', () => {
    freezeTime()
    render(<VaultAssetChart tranche="junior" history={history()} />)
    fireEvent.focus(screen.getByRole('img'))
    expect(screen.getByRole('status')).toHaveTextContent('$200.00')
    expect(screen.getByRole('status')).toHaveTextContent('$80.00')
  })

  it('does not replace missing or stale locked observations with zero', () => {
    freezeTime()
    const data = history()
    data.senior.points[1].lockedAssets = null
    data.senior.points[2].markFresh = false
    render(<VaultAssetChart tranche="senior" history={data} />)
    expect(screen.getByText(/Locked unavailable/)).toBeInTheDocument()
    fireEvent.keyDown(screen.getByRole('img'), { key: 'ArrowLeft' })
    expect(screen.getByText(/Locked unavailable/)).toBeInTheDocument()
    expect(screen.getByText(/Partial history/)).toBeInTheDocument()
  })

  it('retains observations after refresh errors and rejects thirty-day data', () => {
    freezeTime()
    const data = history()
    const { rerender } = render(<VaultAssetChart tranche="senior" history={data} isError />)
    expect(screen.getByRole('img')).toBeInTheDocument()
    expect(screen.getByText(/History refresh is temporarily unavailable/)).toBeInTheDocument()
    rerender(<VaultAssetChart tranche="senior" history={{ ...data, range: '30d' }} />)
    expect(screen.queryByRole('img')).not.toBeInTheDocument()
    expect(screen.getByRole('status')).toHaveTextContent('Asset history is being collected')
  })

  it('excludes data outside the trailing seven-day window', () => {
    freezeTime()
    const data = history()
    data.senior.points = data.senior.points.map((point) => ({ ...point, timestamp: point.timestamp - 8 * 86400 }))
    render(<VaultAssetChart tranche="senior" history={data} />)
    expect(screen.queryByRole('img')).not.toBeInTheDocument()
  })
})
