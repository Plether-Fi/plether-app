import { fireEvent, render, screen, within } from '@testing-library/react'
import { describe, expect, it } from 'vitest'
import rebate from '../../../../scripts/fixtures/insights-close-rebate.json'
import type { TradeExecution, WalletActivity } from '../api/types'
import { ClosedPositionBreakdown } from './ClosedPositionBreakdown'

function item(execution: TradeExecution | null = rebate.execution as TradeExecution): WalletActivity {
  return { id: 'close', type: 'Close', occurredAt: '2026-09-15T02:27:59Z', market: 'DXY', side: 'long',
    size: '2166132764800', sizeDelta: '2194400000000000000000000', price: '1.01315', pnl: '4586170150',
    executionFee: '0', vpi: '-1755484993', txHash: rebate.receipt.transactionHash, execution }
}

describe('closed position breakdown', () => {
  it('opens a named dialog with execution context and exact receipt reconciliation', () => {
    render(<ClosedPositionBreakdown item={item()} />)
    expect(screen.queryByRole('dialog')).not.toBeInTheDocument()
    fireEvent.click(screen.getByRole('button', { name: 'View breakdown' }))
    const dialog = screen.getByRole('dialog', { name: 'Position breakdown' })
    expect(within(dialog).getByText('Fully closed')).toBeInTheDocument()
    expect(within(dialog).getByText('2,194,400.000000 plDXY')).toBeInTheDocument()
    expect(within(dialog).getByText('2,166,132.764800 USDC')).toBeInTheDocument()
    expect(within(dialog).getByText('−866.453105 USDC')).toBeInTheDocument()
    expect(within(dialog).getByText('+5,474.976305 USDC')).toBeInTheDocument()
    expect(within(dialog).getByRole('link')).toHaveAttribute('href', `https://sepolia.arbiscan.io/tx/${rebate.receipt.transactionHash}`)
    expect(within(dialog).queryByRole('checkbox')).not.toBeInTheDocument()
    fireEvent.click(within(dialog).getByRole('button', { name: 'Close position breakdown' }))
    expect(screen.queryByRole('dialog')).not.toBeInTheDocument()
  })

  it('distinguishes a partial close and shows the remaining position without adding its margin to profit', () => {
    const execution = structuredClone(rebate.execution) as TradeExecution
    execution.receipt.postPositionSize = '100000000000000000000'
    execution.receipt.postPositionMarginUsdc = '123456789'
    render(<ClosedPositionBreakdown item={item(execution)} />)
    fireEvent.click(screen.getByRole('button', { name: 'View breakdown' }))
    expect(screen.getByText('Partially closed')).toBeInTheDocument()
    expect(screen.getByText('100.000000 plDXY')).toBeInTheDocument()
    expect(screen.getByText('123.456789 USDC')).toBeInTheDocument()
    expect(screen.getByText('+5,474.976305 USDC')).toBeInTheDocument()
  })

  it('does not mistake missing evidence for a fully closed position or zero costs', () => {
    render(<ClosedPositionBreakdown item={item(null)} />)
    fireEvent.click(screen.getByRole('button', { name: 'View breakdown' }))
    expect(screen.getByText(/Breakdown unavailable/)).toBeInTheDocument()
    expect(screen.queryByText('Fully closed')).not.toBeInTheDocument()
    expect(screen.queryByText('0.000000 USDC')).not.toBeInTheDocument()
  })
})
