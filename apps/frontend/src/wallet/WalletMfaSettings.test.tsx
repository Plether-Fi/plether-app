import { act, fireEvent, render, screen } from '@testing-library/react'
import { beforeEach, expect, it, vi } from 'vitest'
import { WalletMfaSettings } from './WalletMfaSettings'

const sdk = vi.hoisted(() => ({
  ready: true, authenticated: true, user: { id: 'public-test-user' } as { id: string } | null,
  wallet: { walletClientType: 'privy' } as { walletClientType: string } | undefined,
  show: vi.fn(),
}))
vi.mock('@privy-io/react-auth', () => ({
  usePrivy: () => sdk,
  useActiveWallet: () => ({ wallet: sdk.wallet }),
  useMfaEnrollment: () => ({ showMfaEnrollmentModal: sdk.show }),
}))

beforeEach(() => {
  sdk.ready = true
  sdk.authenticated = true
  sdk.user = { id: 'public-test-user' }
  sdk.wallet = { walletClientType: 'privy' }
  sdk.show.mockReset().mockResolvedValue(undefined)
})

it.each(['loading', 'signed-out', 'no-user', 'external', 'no-wallet'])('does not offer embedded-wallet MFA for %s', state => {
  if (state === 'loading') sdk.ready = false
  if (state === 'signed-out') sdk.authenticated = false
  if (state === 'no-user') sdk.user = null
  if (state === 'external') sdk.wallet = { walletClientType: 'metamask' }
  if (state === 'no-wallet') sdk.wallet = undefined
  render(<WalletMfaSettings />)
  expect(screen.queryByRole('button', { name: 'Wallet MFA settings' })).not.toBeInTheDocument()
  expect(sdk.show).not.toHaveBeenCalled()
})

it('opens the native MFA settings again after they are dismissed', async () => {
  // The SDK leaves its enrollment promise pending on a normal dismissal.
  sdk.show.mockImplementation(() => new Promise<void>(() => {}))
  render(<WalletMfaSettings />)
  await act(async () => { fireEvent.click(screen.getByRole('button', { name: 'Wallet MFA settings' })) })
  await act(async () => { fireEvent.click(screen.getByRole('button', { name: 'Wallet MFA settings' })) })
  expect(sdk.show).toHaveBeenCalledTimes(2)
  expect(screen.queryByRole('alert')).not.toBeInTheDocument()
})

it('shows an opening failure in the wallet modal and allows a retry', async () => {
  sdk.show.mockRejectedValueOnce(new Error('Offline settings service'))
  render(<WalletMfaSettings />)
  await act(async () => { fireEvent.click(screen.getByRole('button', { name: 'Wallet MFA settings' })) })
  expect(screen.getByRole('alert')).toHaveTextContent('Could not open wallet MFA settings. Try again.')
  await act(async () => { fireEvent.click(screen.getByRole('button', { name: 'Wallet MFA settings' })) })
  expect(sdk.show).toHaveBeenCalledTimes(2)
  expect(screen.queryByRole('alert')).not.toBeInTheDocument()
})
