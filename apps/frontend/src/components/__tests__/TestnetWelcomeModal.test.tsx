import { render, screen } from '@testing-library/react'
import { describe, expect, it, vi } from 'vitest'
import { TestnetWelcomeModalView } from '../TestnetWelcomeModal'

describe('TestnetWelcomeModalView faucet recovery', () => {
  it('shows a recovered legacy claim without inventing a transaction link', () => {
    render(
      <TestnetWelcomeModalView
        isOpen
        walletAddress="0x18718947c3Ef215DEeaDdbB501CfdED63f95b3A5"
        claim={{
          address: '0x18718947c3ef215deeadb501cfded63f95b3a5',
          amount: '100000000000',
          token: '0xb15503d70b0eaa644dc6650d2a248762f7c5bce3',
          txHash: null,
          status: 'already_funded',
        }}
        onClose={vi.fn()}
        onWalletAddressChange={vi.fn()}
        onRequestFunds={vi.fn()}
      />
    )

    expect(screen.getByText('Mock USDC is already available for this wallet.')).toBeInTheDocument()
    expect(screen.queryByRole('link', { name: /^0x/ })).not.toBeInTheDocument()
    expect(screen.getByRole('button', { name: 'Deposit' })).toBeInTheDocument()
  })
})
