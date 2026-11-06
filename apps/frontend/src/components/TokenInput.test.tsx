import { useState } from 'react'
import { fireEvent, render, screen } from '@testing-library/react'
import { describe, expect, it, vi } from 'vitest'
import { TokenInput } from './TokenInput'

const token = { symbol: 'USDG', decimals: 6 }

describe('TokenInput amount shortcuts', () => {
  it('fills percentages rounded down to token precision', () => {
    const onChange = vi.fn()
    render(<TokenInput value="" onChange={onChange} token={token} balance={10_000_003n} showAmountShortcuts />)

    for (const [label, amount] of [
      ['25%', '2.5'],
      ['50%', '5.000001'],
      ['75%', '7.500002'],
      ['Max', '10.000003'],
    ]) {
      fireEvent.click(screen.getByRole('button', { name: label, exact: true }))
      expect(onChange).toHaveBeenLastCalledWith(amount)
    }
    expect(screen.queryByRole('button', { name: 'MAX', exact: true })).not.toBeInTheDocument()
  })

  it.each([
    [1n, '0.000001'],
    [9_007_199_254_740_993n, '9007199254.740993'],
  ])('preserves the exact maximum for a balance of %s raw units', (balance, expected) => {
    const onChange = vi.fn()
    render(<TokenInput value="" onChange={onChange} token={token} balance={balance} showAmountShortcuts />)

    fireEvent.click(screen.getByRole('button', { name: 'Max', exact: true }))
    expect(onChange).toHaveBeenCalledWith(expected)
  })

  it.each([
    { balance: undefined, disabled: false },
    { balance: 0n, disabled: false },
    { balance: 1_000_000n, disabled: true },
  ])('disables unavailable shortcuts for %o', ({ balance, disabled }) => {
    const onChange = vi.fn()
    render(<TokenInput value="" onChange={onChange} token={token} balance={balance} disabled={disabled} showAmountShortcuts />)

    for (const button of screen.getAllByRole('button')) {
      expect(button).toBeDisabled()
      fireEvent.click(button)
    }
    expect(onChange).not.toHaveBeenCalled()
  })

  it('updates Max selection when the amount changes', () => {
    function InputWithState() {
      const [value, setValue] = useState('')
      return <TokenInput value={value} onChange={setValue} token={token} balance={10_000_000n} showAmountShortcuts />
    }
    render(<InputWithState />)

    fireEvent.click(screen.getByRole('button', { name: 'Max', exact: true }))
    expect(screen.getByRole('button', { name: 'Max', pressed: true })).toBeInTheDocument()
    fireEvent.change(screen.getByRole('textbox'), { target: { value: '3' } })
    expect(screen.getByRole('button', { name: 'Max', pressed: false })).toBeInTheDocument()
  })
})
