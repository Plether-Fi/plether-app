import { fireEvent, render, screen, within } from '@testing-library/react'
import { describe, expect, it } from 'vitest'
import waiver from '../../../../scripts/fixtures/insights-close-waiver.json'
import rebate from '../../../../scripts/fixtures/insights-close-rebate.json'
import type { TradeExecution, WalletActivity } from '../api/types'
import { TradeBreakdown, TradeFee, TradeVpi } from './TradeBreakdown'

function item(execution?: TradeExecution): WalletActivity {
  return { id: 'trade', type: 'Close', occurredAt: '', market: 'DXY', side: 'long', size: null, sizeDelta: null,
    price: null, pnl: null, executionFee: '0', vpi: '0', txHash: null, execution }
}
describe('trade breakdown disclosure', () => {
  it('uses native keyboard-operable disclosure and exact amount controls', () => {
    const { container } = render(<TradeBreakdown item={item(rebate.execution as TradeExecution)} />)
    const summary = screen.getByText('Trade breakdown')
    expect(summary.tagName).toBe('SUMMARY')
    expect(summary.parentElement?.tagName).toBe('DETAILS')
    expect(summary.parentElement).not.toHaveAttribute('open')
    fireEvent.click(summary)
    fireEvent.click(screen.getByRole('checkbox', { name: 'Show exact amounts (6 decimals)' }))
    const feeRow = screen.getByText('Protocol fee assessed').parentElement!
    expect(within(feeRow).getByText('−866.453105 USDC')).toBeInTheDocument()
    expect(screen.getByText('+5,474.976305 USDC')).toBeInTheDocument()
    expect(container.textContent).toContain('Net rebate paid: 889.031888 USDC')
  })
  it('shows a confirmed fee waiver and effective fee without double counting', () => {
    render(<TradeBreakdown item={item(waiver.execution as TradeExecution)} />)
    expect(screen.getByText('Fee waived')).toBeInTheDocument()
    expect(screen.getByText('−7,200.21 USDC')).toBeInTheDocument()
    expect(screen.getByText(/Fee after waiver: 222.31 USDC/)).toBeInTheDocument()
  })
  it('never falls back to the ambiguous zero legacy fee', () => {
    const trade = item()
    render(<><span data-testid="fee"><TradeFee item={trade} /></span><span data-testid="vpi"><TradeVpi item={trade} /></span><TradeBreakdown item={trade} /></>)
    expect(screen.getByTestId('fee')).toHaveTextContent('—')
    expect(screen.getByTestId('vpi')).toHaveTextContent('—')
    expect(screen.getByText(/Breakdown unavailable/)).toBeInTheDocument()
    expect(screen.queryByText('0.00 USDC')).not.toBeInTheDocument()
  })
})
