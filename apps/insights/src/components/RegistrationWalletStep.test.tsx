import { act, fireEvent, render, screen, waitFor } from '@testing-library/react'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { recoverMessageAddress } from 'viem'
import { privateKeyToAccount } from 'viem/accounts'
import { RegistrationWalletStep } from './RegistrationWalletStep'

const mocks = vi.hoisted(() => ({
  connect: vi.fn(),
  selectWallet: vi.fn(),
  configured: true,
  ready: true,
  isSelecting: false,
  isConnected: true,
  switchChainAsync: vi.fn(),
  signMessageAsync: vi.fn(),
  createWalletChallenge: vi.fn(),
  verifyRegistrationWallet: vi.fn(),
  onVerified: vi.fn(),
  chainId: 1,
  address: '0x1111111111111111111111111111111111111111',
}))

vi.mock('../wallet/useRegistrationWallet', () => ({
  useRegistrationWallet: () => ({
    configured: mocks.configured,
    ready: mocks.ready,
    isSelecting: mocks.isSelecting,
    connect: mocks.connect,
    selectWallet: mocks.selectWallet,
    wallets: [{ address: mocks.address, label: 'Privy wallet' }],
    error: null,
  }),
}))
vi.mock('wagmi', () => ({
  useConnection: () => ({ address: mocks.address, chainId: mocks.chainId, isConnected: mocks.isConnected }),
  useSwitchChain: () => ({ mutateAsync: mocks.switchChainAsync }),
  useSignMessage: () => ({ mutateAsync: mocks.signMessageAsync }),
}))
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
  mocks.configured = true
  mocks.ready = true
  mocks.isSelecting = false
  mocks.isConnected = true
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
    expect(mocks.signMessageAsync).toHaveBeenCalledWith({ account: mocks.address, message: 'Sign this one-time message' })
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


describe('Privy registration wallet controls', () => {
  it('keeps registration readable when Privy is not configured', () => {
    mocks.configured = false
    render(<RegistrationWalletStep slug="competition" registration={registration} onVerified={mocks.onVerified} />)
    expect(screen.getByRole('alert')).toHaveTextContent('Wallet connection is not configured')
    expect(screen.queryByRole('button', { name: 'Connect wallet' })).not.toBeInTheDocument()
  })

  it('waits for Privy wallets before enabling signing', () => {
    mocks.chainId = 421614
    mocks.ready = false
    render(<RegistrationWalletStep slug="competition" registration={registration} onVerified={mocks.onVerified} />)
    expect(screen.getByRole('status')).toHaveTextContent('Loading wallets')
    expect(screen.getByRole('button', { name: 'Sign and verify wallet' })).toBeDisabled()
    expect(mocks.createWalletChallenge).not.toHaveBeenCalled()
  })

  it('opens Privy for a disconnected external or social wallet', () => {
    mocks.isConnected = false
    render(<RegistrationWalletStep slug="competition" registration={registration} onVerified={mocks.onVerified} />)
    fireEvent.click(screen.getByRole('button', { name: 'Connect wallet' }))
    expect(mocks.connect).toHaveBeenCalledTimes(1)
  })

  it('waits for an explicitly selected owner to become active', () => {
    mocks.chainId = 421614
    mocks.isSelecting = true
    render(<RegistrationWalletStep slug="competition" registration={registration} onVerified={mocks.onVerified} />)
    expect(screen.getByRole('button', { name: 'Sign and verify wallet' })).toBeDisabled()
    expect(screen.getByRole('combobox', { name: 'Owner wallet' })).toBeEnabled()
    expect(screen.getByRole('button', { name: 'Use another wallet' })).toBeEnabled()
  })

  it('does not request a signature if the owner changes while fetching a challenge', async () => {
    mocks.chainId = 421614
    let resolveChallenge: ((challenge: { message: string }) => void) | undefined
    mocks.createWalletChallenge.mockReturnValue(new Promise<{ message: string }>((resolve) => { resolveChallenge = resolve }))
    const view = render(<RegistrationWalletStep slug="competition" registration={registration} onVerified={mocks.onVerified} />)
    fireEvent.click(screen.getByRole('button', { name: 'Sign and verify wallet' }))
    mocks.address = '0x3333333333333333333333333333333333333333'
    view.rerender(<RegistrationWalletStep slug="competition" registration={registration} onVerified={mocks.onVerified} />)
    await act(async () => { resolveChallenge?.({ message: 'Sign this one-time message' }) })
    expect(await screen.findByRole('alert')).toHaveTextContent('connected wallet or network changed')
    expect(mocks.signMessageAsync).not.toHaveBeenCalled()
    expect(mocks.verifyRegistrationWallet).not.toHaveBeenCalled()
  })

  it('abandons a signature when the wallet disconnects', async () => {
    mocks.chainId = 421614
    let resolveSignature: ((signature: string) => void) | undefined
    mocks.signMessageAsync.mockReturnValue(new Promise<string>((resolve) => { resolveSignature = resolve }))
    const view = render(<RegistrationWalletStep slug="competition" registration={registration} onVerified={mocks.onVerified} />)
    fireEvent.click(screen.getByRole('button', { name: 'Sign and verify wallet' }))
    await waitFor(() => { expect(mocks.signMessageAsync).toHaveBeenCalledTimes(1) })
    mocks.isConnected = false
    view.rerender(<RegistrationWalletStep slug="competition" registration={registration} onVerified={mocks.onVerified} />)
    await act(async () => { resolveSignature?.('0xsigned') })
    expect(await screen.findByRole('alert')).toHaveTextContent('connected wallet or network changed')
    expect(mocks.verifyRegistrationWallet).not.toHaveBeenCalled()
  })

  it('submits a canonical recoverable EOA signature for an embedded owner', async () => {
    // Anvil's public development account, used only to exercise the signing contract.
    const account = privateKeyToAccount('0xac0974bec39a17e36ba4a6b4d238ff944bacb478cbed5efcae784d7bf4f2ff80')
    const message = 'Sign this one-time message'
    const signature = await account.signMessage({ message })
    mocks.address = account.address
    mocks.chainId = 421614
    mocks.signMessageAsync.mockResolvedValue(signature.toUpperCase().replace('0X', '0x'))
    render(<RegistrationWalletStep slug="competition" registration={registration} onVerified={mocks.onVerified} />)
    fireEvent.click(screen.getByRole('button', { name: 'Sign and verify wallet' }))
    await waitFor(() => { expect(mocks.verifyRegistrationWallet).toHaveBeenCalledTimes(1) })
    const submittedSignature = mocks.verifyRegistrationWallet.mock.calls[0][3]
    expect(submittedSignature).toMatch(/^0x[0-9a-f]{130}$/)
    expect(await recoverMessageAddress({ message, signature: submittedSignature })).toBe(account.address)
  })
})
