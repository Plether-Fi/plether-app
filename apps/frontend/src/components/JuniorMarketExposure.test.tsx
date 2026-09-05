import { render, screen } from '@testing-library/react'
import { describe, expect, it } from 'vitest'
import { JuniorMarketExposure } from './JuniorMarketExposure'

const pool = {
  markPrice: 100_000_000n,
  longOpenInterest: 600n * 10n ** 18n,
  shortOpenInterest: 400n * 10n ** 18n,
  juniorPrincipalUsdc: 200n * 10n ** 6n,
  seniorPrincipalUsdc: 800n * 10n ** 6n,
}

describe('JuniorMarketExposure price availability', () => {
  it.each([
    { markFresh: true, oracleFrozen: true },
    { markFresh: false, oracleFrozen: false },
  ])('retains the ratio at the last price for %o', (pricing) => {
    render(<JuniorMarketExposure pool={{ ...pool, ...pricing }} />)
    expect(screen.getByText('1.00×')).toBeInTheDocument()
    expect(screen.getByText('At last market price')).toBeInTheDocument()
    expect(screen.getByText(/Market sensitivity is unavailable/)).toBeInTheDocument()
  })

  it('omits the last-price label when pricing is live', () => {
    render(<JuniorMarketExposure pool={{ ...pool, markFresh: true, oracleFrozen: false }} />)
    expect(screen.getByText('1.00×')).toBeInTheDocument()
    expect(screen.queryByText('At last market price')).not.toBeInTheDocument()
  })

  it('does not label missing price data as a last-price estimate', () => {
    render(<JuniorMarketExposure pool={{ ...pool, markPrice: undefined, oracleFrozen: true }} />)
    expect(screen.queryByText('1.00×')).not.toBeInTheDocument()
    expect(screen.queryByText('At last market price')).not.toBeInTheDocument()
  })
})
