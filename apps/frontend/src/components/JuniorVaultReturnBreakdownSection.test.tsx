import { render, screen } from '@testing-library/react'
import { describe, expect, it } from 'vitest'
import { JuniorVaultReturnBreakdownSection } from './JuniorVaultReturnBreakdown'
import { createVaultAssetHistory } from '../stories/fixtures/vaultAssetHistory'

describe('junior waterfall history integration', () => {
  it('uses the shared seven-day history and withholds a different range', () => {
    const history = createVaultAssetHistory()
    const { rerender } = render(<JuniorVaultReturnBreakdownSection history={history} />)
    expect(screen.getByRole('status')).toHaveTextContent('Component attribution is not available')
    expect(screen.queryByRole('group', { name: 'Return reporting period' })).not.toBeInTheDocument()
    rerender(<JuniorVaultReturnBreakdownSection history={{ ...history, range: '30d' }} />)
    expect(screen.getByRole('status')).toHaveTextContent('Complete, fresh diluted share-price history is not yet available')
  })

  it('withholds history from an unrelated vault deployment', () => {
    const history = createVaultAssetHistory()
    history.deployment.juniorVault = '0x0000000000000000000000000000000000000001'
    render(<JuniorVaultReturnBreakdownSection history={history} />)
    expect(screen.getByRole('status')).toHaveTextContent('Complete, fresh diluted share-price history is not yet available')
  })

  it('labels cached results after refresh failure', () => {
    render(<JuniorVaultReturnBreakdownSection history={createVaultAssetHistory()} isError />)
    expect(screen.getByText('Refresh failed. Showing the last received reporting period.')).toBeInTheDocument()
  })
})
