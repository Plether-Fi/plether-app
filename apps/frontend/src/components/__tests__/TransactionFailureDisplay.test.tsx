import { render, screen } from '@testing-library/react'
import { describe, expect, it } from 'vitest'
import { LoadingScreen } from '../ui/LoadingScreen'

describe('transaction error display', () => {
  it('shows failure details and explorer access even when no step was marked as failed', () => {
    render(<LoadingScreen steps={[]} errorMessage="The connection failed. Check account activity before retrying."
      supportReference="tx-7b2d5d87-5ddf-4eca-8c03-cd68c4af7281" errorCode="NETWORK_ERROR"
      transactionUrl="https://example.com/transaction" />)
    expect(screen.getByRole('alert')).toHaveTextContent('The connection failed.')
    expect(screen.getByText('tx-7b2d5d87-5ddf-4eca-8c03-cd68c4af7281')).toBeVisible()
    expect(screen.getByRole('link', { name: /Show transaction/ })).toHaveAttribute('href', 'https://example.com/transaction')
  })
})
