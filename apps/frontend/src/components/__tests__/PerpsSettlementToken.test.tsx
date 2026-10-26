import { fireEvent, render, screen } from '@testing-library/react'
import { describe, expect, it, vi } from 'vitest'

import { PERPS_SETTLEMENT_TOKEN, PERPS_SETTLEMENT_TOKEN_LABEL } from '../../contracts/perpsAddresses'

vi.mock('../../config/wagmi', () => ({ syncAppKitModalStyleOverrides: vi.fn() }))

import { PerpsClaimPanel } from '../PerpsClaimPanel'
import { PerpsReviewFooter } from '../PerpsReviewFooter'
import { TestnetWelcomeModalView } from '../TestnetWelcomeModal'
import { TokenAmount } from '../ui'

const account = '0x6b72fE6CC52201a1eb7892A813C6C10cCe62745c'

describe('USDG perps display', () => {
  it('labels perps balances from configuration without changing shared spot token defaults', () => {
    expect(PERPS_SETTLEMENT_TOKEN.name).toBe('Mock USDG')
    expect(PERPS_SETTLEMENT_TOKEN_LABEL).toBe('USDG')
    const view = render(<PerpsClaimPanel claimAssets="25.00" status="waiting" tradingAccountAddress={account} />)
    expect(screen.getByText('USDG')).toBeInTheDocument()
    expect(screen.queryByText('USDC')).not.toBeInTheDocument()

    view.rerender(<TokenAmount amount="25.00" />)
    expect(screen.getByText('USDC')).toBeInTheDocument()
  })

  it('uses the configured label in sponsored cost and carry explanations', () => {
    render(<PerpsReviewFooter preparing={false} refreshing={false} slow={false} changes={[]}
      canConfirm direction="long" sponsoredCloseAssets="0.20" depositCarryAssets="0.10" commitmentCarryAssets="0.05"
      onConfirm={vi.fn()} onCancel={vi.fn()} />)
    expect(screen.getByText(/Plether covers/)).toHaveTextContent('0.20 USDG')
    expect(screen.getByText(/Carry collected with assistance/)).toHaveTextContent('0.10 USDG')
    expect(screen.getByText(/Carry collected when/)).toHaveTextContent('0.05 USDG')
  })

  it.each(['idle', 'deposited'] as const)('allows another mint after a completed claim in %s phase', (fundingPhase) => {
    const onRequestFunds = vi.fn()
    render(<TestnetWelcomeModalView isOpen isWalletConnected isTradingAccountRecipient
      walletAddress={account} fundingPhase={fundingPhase}
      claim={{ address: account, amount: '100000000000', token: account, txHash: null, status: 'minted' }}
      onClose={vi.fn()} onWalletAddressChange={vi.fn()} onConnectWallet={vi.fn()} onRequestFunds={onRequestFunds} />)

    const mint = screen.getByRole('button', { name: 'Get 100,000 USDG' })
    expect(mint).toBeEnabled()
    expect(screen.getByText(/Your wallet needs Arbitrum Sepolia ETH/)).toHaveTextContent('Plether covers gas for the deposit.')
    fireEvent.click(mint)
    expect(onRequestFunds).toHaveBeenCalledOnce()
  })
})
