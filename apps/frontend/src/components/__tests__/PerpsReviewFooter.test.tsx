import { fireEvent, render, screen } from '@testing-library/react'
import { describe, expect, it, vi } from 'vitest'
import { PerpsReviewFooter } from '../PerpsReviewFooter'

const props = { preparing: true, refreshing: false, slow: false, changes: [], canConfirm: false, direction: 'long' as const, onConfirm: vi.fn(), onCancel: vi.fn(), onRetry: vi.fn() }
describe('commit review feedback', () => {
  it('explains loading and slow requests without moving focus', () => {
    const view = render(<PerpsReviewFooter {...props} />)
    expect(screen.getByRole('button', { name: 'Checking order…' })).toBeDisabled()
    expect(screen.getByRole('button', { name: 'Checking order…' })).toHaveAttribute('aria-busy', 'true')
    const cancel = screen.getByRole('button', { name: 'Cancel' })
    cancel.focus()
    view.rerender(<PerpsReviewFooter {...props} slow />)
    expect(screen.getByRole('status')).toHaveTextContent('Network is taking longer than usual.')
    expect(cancel).toHaveFocus()
  })
  it('keeps refreshes disabled and requires an explicit updated-order confirmation', () => {
    const onConfirm = vi.fn()
    const view = render(<PerpsReviewFooter {...props} refreshing onConfirm={onConfirm} />)
    fireEvent.click(screen.getByRole('button', { name: 'Updating review…' }))
    expect(onConfirm).not.toHaveBeenCalled()
    view.rerender(<PerpsReviewFooter {...props} preparing={false} canConfirm onConfirm={onConfirm}
      changes={[{ label: 'Required margin', before: '20 USDC', after: '20.000001 USDC' }]} />)
    expect(screen.getByRole('status')).toHaveTextContent('20 USDC → 20.000001 USDC')
    fireEvent.click(screen.getByRole('button', { name: 'Confirm updated order' }))
    expect(onConfirm).toHaveBeenCalledOnce()
  })
  it('keeps failed reviews disabled and exposes retry beside the error', () => {
    const onRetry = vi.fn()
    render(<PerpsReviewFooter {...props} preparing={false} error="Order checks failed." onRetry={onRetry} />)
    expect(screen.getByRole('status')).toHaveTextContent('Order checks failed.')
    expect(screen.getByRole('button', { name: 'Confirm Commit' })).toBeDisabled()
    fireEvent.click(screen.getByRole('button', { name: 'Retry review' }))
    expect(onRetry).toHaveBeenCalledOnce()
  })
})
