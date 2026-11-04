import { fireEvent, render, screen } from '@testing-library/react'
import { describe, expect, it } from 'vitest'
import { JuniorVaultReturnBreakdown } from './JuniorVaultReturnBreakdown'
import { juniorReturnExample as attribution, juniorReturnExamplePeriod as period } from '../stories/fixtures/juniorReturnAttribution'

describe('Junior vault return waterfall', () => {
  it('renders the eight sources in order and a zero-based final total', () => {
    const { container } = render(<JuniorVaultReturnBreakdown period={period} attribution={attribution} />)
    const bars = [...container.querySelectorAll('[data-bar]')]
    expect(bars.map((bar) => bar.getAttribute('data-bar'))).toEqual([
      'netVpi', 'liquidationFees', 'realizedCarry', 'oracleSpread', 'settledTraderPnl',
      'openPositionValuation', 'seniorCoupon', 'maintenanceDilution', 'total',
    ])
    expect(bars.at(-1)).toHaveAttribute('data-start', '0')
    expect(screen.queryByText('Period return')).not.toBeInTheDocument()
    expect(screen.queryByText('Selected reporting period')).not.toBeInTheDocument()
    expect(screen.queryByText('APY equivalent')).not.toBeInTheDocument()
    expect(screen.getByRole('region')).toHaveClass('overflow-x-auto')
  })

  it('explains sources and shows percentage points and USDG on focus, tap and hover', () => {
    render(<JuniorVaultReturnBreakdown period={period} attribution={attribution} />)
    const oracle = screen.getByRole('button', { name: /^Oracle spread:/ })
    fireEvent.focus(oracle)
    expect(screen.getByRole('status')).toHaveTextContent('+0.028000 pp')
    expect(screen.getByRole('status')).toHaveTextContent('+370,000.00 USDG')
    expect(screen.getByRole('status')).toHaveTextContent('frozen-market close spread')
    fireEvent.keyDown(oracle, { key: 'Escape' })
    expect(screen.getByRole('status')).toHaveTextContent('Hover, tap or focus')
    fireEvent.click(screen.getByRole('button', { name: /^Maintenance dilution:/ }))
    expect(screen.getByRole('status')).toHaveTextContent('not a cash expense')
  })

  it('displays missing attribution without duplicating the shared performance metrics', () => {
    const { container } = render(<JuniorVaultReturnBreakdown period={period} />)
    expect(screen.queryByText('+0.523%')).not.toBeInTheDocument()
    expect(screen.getByRole('status')).toHaveTextContent('Component attribution is not available')
    expect(container.querySelector('[data-bar]')).toBeNull()
  })

  it('shows a reconciliation warning and a separate residual bar', () => {
    const changed = structuredClone(attribution)
    changed.components.oracleSpread.contributionPp = 0
    const { container } = render(<JuniorVaultReturnBreakdown period={period} attribution={changed} />)
    expect(screen.getByRole('alert')).toHaveTextContent('+0.02800000 pp')
    expect(container.querySelector('[data-bar="unexplained"]')).toBeInTheDocument()
  })

})
