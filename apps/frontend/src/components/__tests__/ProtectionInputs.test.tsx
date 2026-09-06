import { useState } from 'react'
import { fireEvent, render, screen } from '@testing-library/react'
import { describe, expect, it } from 'vitest'
import { ProtectionInputs } from '../ProtectionInputs'
import { EMPTY_PROTECTION_DRAFT, protectionParamsFromInputs } from '../../contracts/positionProtection'

function Harness({ direction = 'long', rawMark = 100_000_000n }: { direction?: 'long' | 'short'; rawMark?: bigint }) {
  const [value, onChange] = useState(EMPTY_PROTECTION_DRAFT)
  const context = { direction, rawMark, cap: 200_000_000n }
  let result = ''
  try { result = JSON.stringify(protectionParamsFromInputs({ ...value, ...context }), (_, item: unknown) => typeof item === 'bigint' ? item.toString() : item) } catch { /* Incomplete input */ }
  return <><ProtectionInputs value={value} onChange={onChange} {...context} /><output data-testid="params">{result}</output></>
}

describe('ProtectionInputs paired fields', () => {
  it('shows the trigger explanation only in the info tooltip', () => {
    render(<Harness />)
    expect(screen.queryByText(/Set either trigger or both/)).not.toBeInTheDocument()
    fireEvent.focus(screen.getByLabelText('How take profit and stop loss work'))
    expect(screen.getByRole('tooltip')).toHaveTextContent('Set either trigger or both')
    expect(screen.getByRole('tooltip')).toHaveTextContent('not leveraged return')
  })
  it.each(['long', 'short'] as const)('calculates both ways for %s without altering the other trigger', direction => {
    render(<Harness direction={direction} />)
    expect(screen.queryByRole('group', { name: 'TP/SL input type' })).not.toBeInTheDocument()
    const tp = direction === 'long' ? '1.12345678' : '0.87654322'
    fireEvent.change(screen.getByLabelText('Take profit (USDC)'), { target: { value: tp } })
    expect(screen.getByLabelText('Take profit (%)')).toHaveValue('12.3456')
    fireEvent.change(screen.getByLabelText('Stop loss (%)'), { target: { value: '5' } })
    expect(screen.getByLabelText('Stop loss (USDC)')).toHaveValue(direction === 'long' ? '0.95' : '1.05')
    expect(screen.getByLabelText('Take profit (USDC)')).toHaveValue(tp)
    expect(screen.getByTestId('params')).toHaveTextContent(direction === 'long' ? '87654322' : '112345678')
    fireEvent.change(screen.getByLabelText('Take profit (%)'), { target: { value: '20' } })
    expect(screen.getByLabelText('Take profit (USDC)')).toHaveValue(direction === 'long' ? '1.2' : '0.8')
    fireEvent.change(screen.getByLabelText('Stop loss (USDC)'), { target: { value: '' } })
    expect(screen.getByLabelText('Stop loss (%)')).toHaveValue('')
  })

  it('keeps invalid input editable without displaying a stale calculation', () => {
    render(<Harness />)
    fireEvent.change(screen.getByLabelText('Take profit (%)'), { target: { value: '10' } })
    expect(screen.getByLabelText('Take profit (USDC)')).toHaveValue('1.1')
    fireEvent.change(screen.getByLabelText('Take profit (%)'), { target: { value: 'abc' } })
    expect(screen.getByLabelText('Take profit (%)')).toHaveValue('abc')
    expect(screen.getByLabelText('Take profit (USDC)')).toHaveValue('')
    expect(screen.getByLabelText('Take profit (%)')).toHaveAttribute('aria-invalid', 'true')
  })

  it('keeps both fields editable and calculates the entered percentage when the market loads', () => {
    const view = render(<Harness rawMark={0n} />)
    expect(screen.getByLabelText('Take profit (%)')).toBeEnabled()
    expect(screen.getByLabelText('Take profit (USDC)')).toBeEnabled()
    expect(screen.getByLabelText('Take profit (USDC)')).toHaveAttribute('placeholder', 'Price')
    expect(screen.getByLabelText('Stop loss (USDC)')).toHaveAttribute('placeholder', 'Price')
    expect(screen.getByLabelText('Take profit (%)')).toHaveAttribute('placeholder', 'Gain')
    expect(screen.getByLabelText('Stop loss (%)')).toHaveAttribute('placeholder', 'Loss')
    expect(screen.getByLabelText('Stop loss (%)')).toBeEnabled()
    expect(screen.getByLabelText('Stop loss (USDC)')).toBeEnabled()
    fireEvent.change(screen.getByLabelText('Take profit (%)'), { target: { value: '10' } })
    expect(screen.getByLabelText('Take profit (%)')).toHaveValue('10')
    expect(screen.getByLabelText('Take profit (USDC)')).toHaveValue('')
    expect(screen.getByText(/Waiting for the current price/)).toBeVisible()
    view.rerender(<Harness rawMark={100_000_000n} />)
    expect(screen.getByLabelText('Take profit (%)')).toHaveValue('10')
    expect(screen.getByLabelText('Take profit (USDC)')).toHaveValue('1.1')
  })
})
