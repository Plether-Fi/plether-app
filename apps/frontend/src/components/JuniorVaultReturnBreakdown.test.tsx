import { fireEvent, render, screen } from '@testing-library/react'
import { describe, expect, it, vi } from 'vitest'
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
    expect(screen.getByText('+14.56%')).toBeInTheDocument()
    expect(screen.getByText('14 days')).toBeInTheDocument()
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

  it('displays missing attribution honestly while retaining actual return', () => {
    const { container } = render(<JuniorVaultReturnBreakdown period={period} />)
    expect(screen.getByText('+0.523%')).toBeInTheDocument()
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

  it('changes period and explains compound annualization', async () => {
    const onRangeChange = vi.fn()
    render(<JuniorVaultReturnBreakdown period={period} attribution={attribution} range="7d" onRangeChange={onRangeChange} />)
    fireEvent.click(screen.getByRole('button', { name: '30 days' }))
    expect(onRangeChange).toHaveBeenCalledWith('30d')
    fireEvent.focus(screen.getByRole('button', { name: 'APY equivalent' }))
    expect(await screen.findByRole('tooltip')).toHaveTextContent('not a forecast')
    expect(screen.getByRole('tooltip')).toHaveTextContent('actual elapsed seconds')
  })
})
