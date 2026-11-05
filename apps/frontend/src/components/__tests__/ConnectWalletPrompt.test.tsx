import { fireEvent, render, screen } from '@testing-library/react'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { ConnectWalletPrompt } from '../ConnectWalletPrompt'

const wallet = vi.hoisted(() => ({
  connect: vi.fn(),
  ready: true,
  configured: true,
}))

vi.mock('../../wallet/useWalletActions', () => ({
  useWalletActions: () => wallet,
}))

describe('ConnectWalletPrompt', () => {
  beforeEach(() => {
    wallet.connect.mockClear()
    wallet.ready = true
    wallet.configured = true
  })

  it('opens wallet onboarding when it is ready', () => {
    render(<ConnectWalletPrompt />)

    fireEvent.click(screen.getByRole('button', { name: 'Connect Your Wallet' }))

    expect(wallet.connect).toHaveBeenCalledOnce()
    expect(screen.getByText('You can browse prices and protocol stats without connecting.')).toBeInTheDocument()
  })

  it('waits for wallet initialization before allowing a connection', () => {
    wallet.ready = false
    render(<ConnectWalletPrompt />)

    const button = screen.getByRole('button', { name: 'Connect Your Wallet' })
    expect(button).toBeDisabled()
    fireEvent.click(button)
    expect(wallet.connect).not.toHaveBeenCalled()
    expect(screen.getByRole('status')).toHaveTextContent('Preparing wallet connection…')
  })

  it('keeps browsing available when wallet connection is not configured', () => {
    wallet.configured = false
    render(<ConnectWalletPrompt />)

    const button = screen.getByRole('button', { name: 'Connect Your Wallet' })
    expect(button).toBeDisabled()
    fireEvent.click(button)
    expect(wallet.connect).not.toHaveBeenCalled()
    expect(screen.getByRole('status')).toHaveTextContent('Wallet connection is not configured for this deployment.')
    expect(screen.getByText('You can browse prices and protocol stats without connecting.')).toBeInTheDocument()
  })
})
