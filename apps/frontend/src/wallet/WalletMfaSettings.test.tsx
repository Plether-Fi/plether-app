import { act, fireEvent, render, screen } from '@testing-library/react'
import { afterEach, beforeEach, expect, it, vi } from 'vitest'
import { WalletMfaSettings } from './WalletMfaSettings'
import * as passkeyOrigin from './passkeyOrigin'

const sdk = vi.hoisted(() => ({
  ready: true, authenticated: true, user: { id: 'public-test-user', mfaMethods: [], linkedAccounts: [] } as { id: string; mfaMethods: string[]; linkedAccounts: { type: string; credentialId: string; enrolledInMfa: boolean }[] } | null,
  wallet: { walletClientType: 'privy', address: '0x1234' } as { walletClientType: string; address: string } | undefined,
  show: vi.fn(), refresh: vi.fn(), prompt: vi.fn(), init: vi.fn(), submit: vi.fn(),
}))
vi.mock('@privy-io/react-auth', () => ({
  usePrivy: () => sdk,
  useActiveWallet: () => ({ wallet: sdk.wallet }),
  useUser: () => ({ refreshUser: sdk.refresh }),
  useMfa: () => ({ promptMfa: sdk.prompt }),
  useMfaEnrollment: () => ({ showMfaEnrollmentModal: sdk.show, initEnrollmentWithPasskey: sdk.init, submitEnrollmentWithPasskey: sdk.submit }),
}))

beforeEach(() => {
  sdk.ready = true
  sdk.authenticated = true
  sdk.user = { id: 'public-test-user', mfaMethods: [], linkedAccounts: [] }
  sdk.wallet = { walletClientType: 'privy', address: '0x1234' }
  sdk.show.mockReset().mockResolvedValue(undefined)
  sdk.refresh.mockReset().mockImplementation(async () => sdk.user)
  sdk.prompt.mockReset().mockResolvedValue(undefined)
  sdk.init.mockReset().mockResolvedValue(undefined)
  sdk.submit.mockReset().mockResolvedValue(undefined)
})
afterEach(() => { vi.restoreAllMocks() })

it.each(['loading', 'signed-out', 'no-user', 'external', 'no-wallet'])('does not offer embedded-wallet MFA for %s', state => {
  if (state === 'loading') sdk.ready = false
  if (state === 'signed-out') sdk.authenticated = false
  if (state === 'no-user') sdk.user = null
  if (state === 'external') sdk.wallet = { walletClientType: 'metamask', address: '0x5678' }
  if (state === 'no-wallet') sdk.wallet = undefined
  render(<WalletMfaSettings onAddPasskey={vi.fn()} />)
  expect(screen.queryByRole('button', { name: 'Wallet MFA settings' })).not.toBeInTheDocument()
  expect(screen.queryByRole('button', { name: 'Add passkey' })).not.toBeInTheDocument()
  expect(screen.queryByRole('region', { name: 'Registered passkeys' })).not.toBeInTheDocument()
  expect(sdk.show).not.toHaveBeenCalled()
})

it('offers passkey linking directly from the embedded-wallet actions', () => {
  vi.spyOn(passkeyOrigin, 'isPasskeyOriginSupported').mockReturnValue(true)
  const add = vi.fn()
  render(<WalletMfaSettings onAddPasskey={add} />)
  fireEvent.click(screen.getByRole('button', { name: 'Add passkey' }))
  expect(add).toHaveBeenCalledOnce()
  expect(sdk.show).not.toHaveBeenCalled()
  expect(sdk.prompt).not.toHaveBeenCalled()
})

it('does not start passkey linking on an unsupported origin', () => {
  vi.spyOn(passkeyOrigin, 'isPasskeyOriginSupported').mockReturnValue(false)
  const add = vi.fn()
  render(<WalletMfaSettings onAddPasskey={add} />)
  expect(screen.getByRole('button', { name: 'Add passkey' })).toBeDisabled()
  fireEvent.click(screen.getByRole('button', { name: 'Add passkey' }))
  expect(add).not.toHaveBeenCalled()
  expect(screen.getByText(/Passkeys need an HTTPS domain or localhost/)).toBeVisible()
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

const updateName = 'Enable additional passkeys for wallet MFA'
const mac = { type: 'passkey', credentialId: 'public-mac-key', enrolledInMfa: true }
const mobile = { type: 'passkey', credentialId: 'public-mobile-key', enrolledInMfa: false }
function withAdditionalPasskey() {
  sdk.user = { id: 'public-test-user', mfaMethods: ['passkey'], linkedAccounts: [{ ...mac }, { ...mobile }] }
}
function deferred() {
  let resolve!: () => void
  const promise = new Promise<void>(done => { resolve = done })
  return { promise, resolve }
}

it('includes registered passkeys and their individual enrollment status in the wallet actions', () => {
  withAdditionalPasskey()
  render(<WalletMfaSettings />)
  expect(screen.getByRole('list', { name: 'Registered passkeys' })).toBeVisible()
  expect(screen.getByText('Wallet MFA enabled')).toBeVisible()
  expect(screen.getByText('Not enabled for wallet MFA')).toBeVisible()
})

it.each(['not-enrolled', 'all-enrolled', 'no-passkeys'])('does not offer an unnecessary enrollment update (%s)', state => {
  withAdditionalPasskey()
  if (state === 'not-enrolled') sdk.user!.mfaMethods = []
  if (state === 'all-enrolled') sdk.user!.linkedAccounts = [{ ...mac }, { ...mobile, enrolledInMfa: true }]
  if (state === 'no-passkeys') sdk.user!.linkedAccounts = []
  render(<WalletMfaSettings />)
  expect(screen.queryByRole('button', { name: updateName })).not.toBeInTheDocument()
  expect(screen.getByRole('button', { name: 'Wallet MFA settings' })).toBeEnabled()
})

it('verifies existing MFA before adding the mobile key, keeping the Mac key and both login methods', async () => {
  withAdditionalPasskey()
  const verification = deferred()
  sdk.prompt.mockReturnValue(verification.promise)
  const add = vi.fn()
  render(<WalletMfaSettings onAddPasskey={add} />)
  fireEvent.click(screen.getByRole('button', { name: updateName }))
  expect(sdk.prompt).toHaveBeenCalledOnce()
  expect(sdk.init).not.toHaveBeenCalled()
  expect(sdk.submit).not.toHaveBeenCalled()
  expect(screen.getByRole('button', { name: 'Updating wallet MFA…' })).toBeDisabled()
  expect(screen.getByRole('button', { name: 'Wallet MFA settings' })).toBeDisabled()
  expect(screen.getByRole('button', { name: 'Add passkey' })).toBeDisabled()
  fireEvent.click(screen.getByRole('button', { name: 'Add passkey' }))
  expect(add).not.toHaveBeenCalled()
  fireEvent.click(screen.getByRole('button', { name: 'Updating wallet MFA…' }))
  await act(async () => { verification.resolve() })
  expect(sdk.prompt).toHaveBeenCalledOnce()
  expect(sdk.init).toHaveBeenCalledOnce()
  expect(sdk.submit).toHaveBeenCalledExactlyOnceWith(
    { credentialIds: [mac.credentialId, mobile.credentialId] }, { removeForLogin: false },
  )
  expect(screen.getByRole('status')).toHaveTextContent('Linked passkeys are enabled for wallet MFA')
  expect(screen.queryByRole('button', { name: updateName })).not.toBeInTheDocument()
})

it.each(['prompt', 'init', 'refresh', 'submit'] as const)('allows retry after %s fails, without reporting success', async stage => {
  withAdditionalPasskey()
  sdk[stage].mockRejectedValueOnce(new Error('Synthetic verification cancellation or service failure'))
  render(<WalletMfaSettings />)
  await act(async () => { fireEvent.click(screen.getByRole('button', { name: updateName })) })
  expect(screen.getByRole('alert')).toHaveTextContent('wallet MFA')
  expect(screen.queryByRole('status')).not.toBeInTheDocument()
  if (stage !== 'submit') expect(sdk.submit).not.toHaveBeenCalled()
  if (stage === 'prompt') expect(sdk.init).not.toHaveBeenCalled()
  await act(async () => { fireEvent.click(screen.getByRole('button', { name: updateName })) })
  expect(sdk.prompt).toHaveBeenCalledTimes(2)
  expect(screen.queryByRole('alert')).not.toBeInTheDocument()
  expect(screen.getByRole('status')).toBeVisible()
})

it.each(['signed-out', 'other-user', 'other-owner', 'dismissed', 'removed-key', 'unenrolled-mac', 'mfa-disabled', 'new-login-key'])('does not submit after the context changes during verification (%s)', async change => {
  withAdditionalPasskey()
  const verification = deferred()
  sdk.prompt.mockReturnValue(verification.promise)
  const view = render(<WalletMfaSettings />)
  fireEvent.click(screen.getByRole('button', { name: updateName }))
  if (change === 'signed-out') sdk.authenticated = false
  if (change === 'other-user') sdk.user = { ...sdk.user!, id: 'another-user' }
  if (change === 'other-owner') sdk.wallet = { walletClientType: 'privy', address: '0x5678' }
  if (change === 'removed-key') sdk.user = { ...sdk.user!, linkedAccounts: [{ ...mac }] }
  if (change === 'unenrolled-mac') sdk.user = { ...sdk.user!, linkedAccounts: [{ ...mac, enrolledInMfa: false }, { ...mobile }] }
  if (change === 'mfa-disabled') sdk.user = { ...sdk.user!, mfaMethods: [] }
  if (change === 'new-login-key') sdk.user = { ...sdk.user!, linkedAccounts: [...sdk.user!.linkedAccounts, { ...mobile, credentialId: 'new-key' }] }
  if (change === 'dismissed') view.unmount()
  else view.rerender(<WalletMfaSettings />)
  await act(async () => { verification.resolve() })
  expect(sdk.init).not.toHaveBeenCalled()
  expect(sdk.submit).not.toHaveBeenCalled()
  expect(screen.queryByRole('status')).not.toBeInTheDocument()
  if (change === 'other-owner' || change === 'other-user') expect(screen.getByRole('button', { name: updateName })).toBeEnabled()
})

it('rechecks the account after enrollment initialization', async () => {
  withAdditionalPasskey()
  const initialization = deferred()
  sdk.init.mockReturnValue(initialization.promise)
  const view = render(<WalletMfaSettings />)
  await act(async () => { fireEvent.click(screen.getByRole('button', { name: updateName })) })
  sdk.user = { ...sdk.user!, linkedAccounts: [{ ...mac }] }
  view.rerender(<WalletMfaSettings />)
  await act(async () => { initialization.resolve() })
  expect(sdk.submit).not.toHaveBeenCalled()
})

it('preserves keys enrolled elsewhere while verifying', async () => {
  withAdditionalPasskey()
  const verification = deferred()
  sdk.prompt.mockReturnValue(verification.promise)
  const view = render(<WalletMfaSettings />)
  fireEvent.click(screen.getByRole('button', { name: updateName }))
  sdk.user = { ...sdk.user!, linkedAccounts: [
    { ...mac }, { ...mobile },
    { type: 'passkey', credentialId: 'public-other-enrolled', enrolledInMfa: true },
  ] }
  view.rerender(<WalletMfaSettings />)
  await act(async () => { verification.resolve() })
  expect(sdk.submit).toHaveBeenCalledExactlyOnceWith(
    { credentialIds: [mac.credentialId, mobile.credentialId, 'public-other-enrolled'] }, { removeForLogin: false },
  )
})

it('preserves an MFA key enrolled on another device even before this tab receives the updated user', async () => {
  withAdditionalPasskey()
  sdk.refresh.mockResolvedValue({ ...sdk.user!, linkedAccounts: [
    { ...mac }, { ...mobile }, { ...mac, credentialId: 'public-server-mfa-key' },
  ] })
  render(<WalletMfaSettings />)
  await act(async () => { fireEvent.click(screen.getByRole('button', { name: updateName })) })
  expect(sdk.submit).toHaveBeenCalledExactlyOnceWith(
    { credentialIds: [mac.credentialId, mobile.credentialId, 'public-server-mfa-key'] }, { removeForLogin: false },
  )
})

it('does not re-enroll a passkey removed on another device while this tab was stale', async () => {
  withAdditionalPasskey()
  sdk.refresh.mockResolvedValue({ ...sdk.user!, linkedAccounts: [{ ...mac }] })
  render(<WalletMfaSettings />)
  await act(async () => { fireEvent.click(screen.getByRole('button', { name: updateName })) })
  expect(sdk.submit).not.toHaveBeenCalled()
  expect(screen.getByRole('alert')).toBeVisible()
})

it('does not submit if the account is signed out during the final server refresh', async () => {
  withAdditionalPasskey()
  const refresh = deferred()
  sdk.refresh.mockImplementation(async () => { await refresh.promise; return sdk.user })
  const view = render(<WalletMfaSettings />)
  await act(async () => { fireEvent.click(screen.getByRole('button', { name: updateName })) })
  expect(sdk.refresh).toHaveBeenCalledOnce()
  sdk.authenticated = false
  view.rerender(<WalletMfaSettings />)
  await act(async () => { refresh.resolve() })
  expect(sdk.submit).not.toHaveBeenCalled()
})

it('does not fall back to stale enrollment if the server refresh returns no user', async () => {
  withAdditionalPasskey()
  sdk.refresh.mockResolvedValue(undefined)
  render(<WalletMfaSettings />)
  await act(async () => { fireEvent.click(screen.getByRole('button', { name: updateName })) })
  expect(sdk.submit).not.toHaveBeenCalled()
  expect(screen.getByRole('alert')).toBeVisible()
})
