import { act, fireEvent, render, screen, waitFor } from '@testing-library/react'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { RegistrationWalletStep } from './RegistrationWalletStep'

const mocks = vi.hoisted(() => ({
  open: vi.fn(),
  switchChainAsync: vi.fn(),
  signMessageAsync: vi.fn(),
  createWalletChallenge: vi.fn(),
  verifyRegistrationWallet: vi.fn(),
  onVerified: vi.fn(),
  chainId: 1,
  address: '0x1111111111111111111111111111111111111111',
}))

vi.mock('@reown/appkit/react', () => ({ useAppKit: () => ({ open: mocks.open }) }))
vi.mock('wagmi', () => ({
  useConnection: () => ({ address: mocks.address, chainId: mocks.chainId, isConnected: true }),
  useSwitchChain: () => ({ mutateAsync: mocks.switchChainAsync }),
  useSignMessage: () => ({ mutateAsync: mocks.signMessageAsync }),
}))
vi.mock('../config/wagmi', () => ({ walletConnectionConfigured: true }))
vi.mock('../api', () => ({
  createWalletChallenge: mocks.createWalletChallenge,
  verifyRegistrationWallet: mocks.verifyRegistrationWallet,
  InsightsApiError: class InsightsApiError extends Error {},
}))

const registration = {
  status: 'in_progress' as const,
  csrfToken: 'csrf-token',
  expiresAt: '2026-08-28T12:00:00Z',
  steps: { xIdentity: 'verified' as const, xFollow: 'verified' as const, wallet: 'pending' as const, completed: false },
  requiredConsents: { rulesVersion: 'rules-v1', privacyVersion: 'privacy-v1' },
}

beforeEach(() => {
  vi.clearAllMocks()
  mocks.chainId = 1
  mocks.address = '0x1111111111111111111111111111111111111111'
  mocks.switchChainAsync.mockImplementation(() => {
    mocks.chainId = 421614
    return Promise.resolve({ id: 421614 })
  })
  mocks.createWalletChallenge.mockResolvedValue({ message: 'Sign this one-time message', expiresAt: '2026-08-28T11:05:00Z' })
  mocks.signMessageAsync.mockResolvedValue('0xsigned')
  mocks.verifyRegistrationWallet.mockResolvedValue({
    ...registration,
    steps: { ...registration.steps, wallet: 'verified' },
    wallet: {
      ownerAddress: '0x1111111111111111111111111111111111111111',
      tradingAccount: '0x2222222222222222222222222222222222222222',
    },
  })

})

describe('RegistrationWalletStep', () => {
  it('switches to Arbitrum Sepolia, signs the challenge, and verifies the owner', async () => {
    const view = render(<RegistrationWalletStep slug="testnet-trading-2026-09" registration={registration} onVerified={mocks.onVerified} />)

    fireEvent.click(screen.getByRole('button', { name: 'Switch to Arbitrum Sepolia' }))
    await waitFor(() => { expect(mocks.switchChainAsync).toHaveBeenCalledWith({ chainId: 421614 }) })
    expect(mocks.createWalletChallenge).not.toHaveBeenCalled()

    view.rerender(<RegistrationWalletStep slug="testnet-trading-2026-09" registration={registration} onVerified={mocks.onVerified} />)
    fireEvent.click(screen.getByRole('button', { name: 'Sign and verify wallet' }))

    await waitFor(() => { expect(mocks.onVerified).toHaveBeenCalledTimes(1) })
    expect(mocks.createWalletChallenge).toHaveBeenCalledWith(
      'testnet-trading-2026-09',
      'csrf-token',
      '0x1111111111111111111111111111111111111111',
    )
    expect(mocks.signMessageAsync).toHaveBeenCalledWith({ message: 'Sign this one-time message' })
    expect(mocks.verifyRegistrationWallet).toHaveBeenCalledWith(
      'testnet-trading-2026-09',
      'csrf-token',
      '0x1111111111111111111111111111111111111111',
      '0xsigned',
    )
  })

  it('abandons a signed challenge if the connected account changes', async () => {
    mocks.chainId = 421614
    let resolveSignature: ((signature: string) => void) | undefined
    mocks.signMessageAsync.mockReturnValue(new Promise<string>((resolve) => {
      resolveSignature = resolve
    }))
    const view = render(<RegistrationWalletStep slug="testnet-trading-2026-09" registration={registration} onVerified={mocks.onVerified} />)

    fireEvent.click(screen.getByRole('button', { name: 'Sign and verify wallet' }))
    await waitFor(() => { expect(mocks.signMessageAsync).toHaveBeenCalledTimes(1) })
    mocks.address = '0x3333333333333333333333333333333333333333'
    view.rerender(<RegistrationWalletStep slug="testnet-trading-2026-09" registration={registration} onVerified={mocks.onVerified} />)
    await act(async () => { resolveSignature?.('0xsigned') })

    expect(await screen.findByRole('alert')).toHaveTextContent('connected wallet or network changed')
    expect(mocks.verifyRegistrationWallet).not.toHaveBeenCalled()
  })

  it('canonicalizes an EIP-55 connector address before challenging and verifying', async () => {
    mocks.chainId = 421614
    mocks.address = '0x52908400098527886E0F7030069857D2E4169EE7'
    render(<RegistrationWalletStep slug="testnet-trading-2026-09" registration={registration} onVerified={mocks.onVerified} />)

    fireEvent.click(screen.getByRole('button', { name: 'Sign and verify wallet' }))

    await waitFor(() => { expect(mocks.onVerified).toHaveBeenCalledTimes(1) })
    expect(mocks.createWalletChallenge).toHaveBeenCalledWith(
      'testnet-trading-2026-09',
      'csrf-token',
      '0x52908400098527886e0f7030069857d2e4169ee7',
    )
    expect(mocks.verifyRegistrationWallet).toHaveBeenCalledWith(
      'testnet-trading-2026-09',
      'csrf-token',
      '0x52908400098527886e0f7030069857d2e4169ee7',
      '0xsigned',
    )
  })
})
