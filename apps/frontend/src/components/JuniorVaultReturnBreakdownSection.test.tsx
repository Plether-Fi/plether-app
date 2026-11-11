import { render, screen } from '@testing-library/react'
import { describe, expect, it } from 'vitest'
import { JuniorVaultReturnBreakdownSection } from './JuniorVaultReturnBreakdown'
import { createVaultAssetHistory } from '../stories/fixtures/vaultAssetHistory'
import { juniorReturnExample } from '../stories/fixtures/juniorReturnAttribution'

describe('junior waterfall history integration', () => {
  it('renders the backend breakdown and its note with a stale intermediate hour', () => {
    const history = createVaultAssetHistory()
    history.junior.points[106].markFresh = false
    const note = 'Pricing was temporarily stale during this period.'
    history.junior.returnAttribution = {
      ...structuredClone(juniorReturnExample),
      startTimestamp: history.junior.points[0].timestamp,
      endTimestamp: history.junior.points[168].timestamp,
      startDilutedSharePrice: history.junior.points[0].sharePrice,
      endDilutedSharePrice: history.junior.points[168].sharePrice,
      netReturnUsdg: '0',
      notes: [note],
    }
    for (const component of Object.values(history.junior.returnAttribution.components)) {
      component.amountUsdg = '0'
      component.contributionPp = 0
    }
    const { container } = render(<JuniorVaultReturnBreakdownSection history={history} />)
    expect(container.querySelectorAll('[data-bar]')).toHaveLength(9)
    expect(container.querySelector('[data-bar="netVpi"]')).toBeInTheDocument()
    expect(container.querySelector('[data-bar="total"]')).toHaveAttribute('data-end', '0')
    expect(screen.getByText(note)).toBeInTheDocument()
    expect(screen.queryByText(/Complete, fresh diluted share-price history/)).not.toBeInTheDocument()
  })

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
