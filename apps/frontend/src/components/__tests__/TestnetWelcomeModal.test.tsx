import { fireEvent, render, screen } from '@testing-library/react'
import { describe, expect, it, vi } from 'vitest'

vi.mock('@reown/appkit/react', () => ({
  useAppKit: () => ({ open: vi.fn() }),
}))

vi.mock('../../config/wagmi', () => ({
  syncAppKitModalStyleOverrides: vi.fn(),
}))

import { TestnetWelcomeModalView } from '../TestnetWelcomeModal'
import { TESTNET_FAUCET_TIMEOUT_MESSAGE } from '../../api/client'

const walletAddress = '0x6b72fE6CC52201a1eb7892A813C6C10cCe62745c'

describe('TestnetWelcomeModalView wallet connection states', () => {
  it('prompts a disconnected user to connect their wallet without showing the address field', () => {
    const onConnectWallet = vi.fn()
    const onRequestFunds = vi.fn()

    render(
      <TestnetWelcomeModalView
        isOpen
        isWalletConnected={false}
        isTradingAccountRecipient
        walletAddress=""
        onClose={() => {}}
        onWalletAddressChange={() => {}}
        onConnectWallet={onConnectWallet}
        onRequestFunds={onRequestFunds}
      />
    )

    expect(screen.queryByRole('textbox')).not.toBeInTheDocument()
    expect(
      screen.queryByRole('button', { name: 'Get 100,000 mock USDC' })
    ).not.toBeInTheDocument()

    const connectButton = screen.getByRole('button', { name: 'Connect Wallet' })
    expect(connectButton).toHaveClass('bg-brand-orange')

    fireEvent.click(connectButton)

    expect(onConnectWallet).toHaveBeenCalledOnce()
    expect(onRequestFunds).not.toHaveBeenCalled()
  })

  it('shows the connected wallet address and the green funding action', () => {
    render(
      <TestnetWelcomeModalView
        isOpen
        isWalletConnected
        isTradingAccountRecipient
        walletAddress={walletAddress}
        onClose={() => {}}
        onWalletAddressChange={() => {}}
        onConnectWallet={() => {}}
        onRequestFunds={() => {}}
      />
    )

    expect(screen.getByRole('textbox')).toHaveValue(walletAddress)
    expect(screen.getByRole('textbox')).toBeDisabled()
    expect(screen.queryByRole('button', { name: 'Connect Wallet' })).not.toBeInTheDocument()

    const fundingButton = screen.getByRole('button', { name: 'Get 100,000 mock USDC' })
    expect(fundingButton).toHaveClass('bg-positive')
  })

  it('waits to show the field and green action until the Trading Account is ready', () => {
    render(
      <TestnetWelcomeModalView
        isOpen
        isWalletConnected
        isTradingAccountRecipient
        walletAddress=""
        onClose={() => {}}
        onWalletAddressChange={() => {}}
        onConnectWallet={() => {}}
        onRequestFunds={() => {}}
      />
    )

    expect(screen.queryByRole('textbox')).not.toBeInTheDocument()

    const preparingButton = screen.getByRole('button', { name: 'Preparing Trading Account' })
    expect(preparingButton).toBeDisabled()
    expect(preparingButton).toHaveClass('bg-surface-muted')
  })

  it('explains why the Trading Account could not be prepared', () => {
    render(
      <TestnetWelcomeModalView
        isOpen
        isWalletConnected
        isTradingAccountRecipient
        walletAddress=""
        recipientError="The connected chain does not match Arbitrum Sepolia."
        onClose={() => {}}
        onWalletAddressChange={() => {}}
        onConnectWallet={() => {}}
        onRequestFunds={() => {}}
      />
    )

    expect(
      screen.getByText('The connected chain does not match Arbitrum Sepolia.')
    ).toBeInTheDocument()
    expect(
      screen.getByRole('button', { name: 'Trading Account unavailable' })
    ).toBeDisabled()
  })

  it('shows a recovered legacy claim without inventing a transaction link', () => {
    render(
      <TestnetWelcomeModalView
        isOpen
        isWalletConnected
        walletAddress="0x18718947c3Ef215DEeaDdbB501CfdED63f95b3A5"
        claim={{
          address: '0x18718947c3ef215deeadb501cfded63f95b3a5',
          amount: '100000000000',
          token: '0x1647e41f49ED6D688936092B5a291c4B28106343',
          txHash: null,
          status: 'already_funded',
        }}
        onClose={vi.fn()}
        onWalletAddressChange={vi.fn()}
        onConnectWallet={vi.fn()}
        onRequestFunds={vi.fn()}
      />
    )

    expect(screen.getByText('Mock USDC is already available for this wallet.')).toBeInTheDocument()
    expect(screen.queryByRole('link', { name: /^0x/ })).not.toBeInTheDocument()
    expect(screen.getByRole('button', { name: 'Deposit' })).toBeInTheDocument()
  })

  it('keeps a submitted faucet transaction pending and rechecks it idempotently', () => {
    const onRequestFunds = vi.fn()
    const pendingTxHash = `0x${'a'.repeat(64)}`

    render(
      <TestnetWelcomeModalView
        isOpen
        isWalletConnected
        isTradingAccountRecipient
        walletAddress={walletAddress}
        claim={{
          address: walletAddress,
          amount: '100000000000',
          token: '0x1647e41f49ED6D688936092B5a291c4B28106343',
          txHash: pendingTxHash,
          status: 'submitted',
        }}
        onClose={vi.fn()}
        onWalletAddressChange={vi.fn()}
        onConnectWallet={vi.fn()}
        onRequestFunds={onRequestFunds}
      />
    )

    expect(
      screen.getByText('Faucet transaction submitted. Waiting for Arbitrum Sepolia confirmation.')
    ).toBeInTheDocument()
    expect(screen.queryByRole('button', { name: 'Deposit' })).not.toBeInTheDocument()
    expect(screen.getByRole('button', { name: 'Close' })).toBeInTheDocument()
    expect(screen.getByRole('link', { name: pendingTxHash })).toBeInTheDocument()

    const checkButton = screen.getByRole('button', { name: 'Check confirmation' })
    expect(checkButton).toBeEnabled()
    fireEvent.click(checkButton)
    expect(onRequestFunds).toHaveBeenCalledOnce()
  })

  it('shows an actionable faucet timeout and leaves retry available', () => {
    render(
      <TestnetWelcomeModalView
        isOpen
        isWalletConnected
        isTradingAccountRecipient
        walletAddress={walletAddress}
        submitError={TESTNET_FAUCET_TIMEOUT_MESSAGE}
        onClose={() => {}}
        onWalletAddressChange={() => {}}
        onConnectWallet={() => {}}
        onRequestFunds={() => {}}
      />
    )

    expect(screen.getByText(TESTNET_FAUCET_TIMEOUT_MESSAGE)).toBeInTheDocument()
    expect(
      screen.getByRole('button', { name: 'Get 100,000 mock USDC' })
    ).toBeEnabled()
  })
})
