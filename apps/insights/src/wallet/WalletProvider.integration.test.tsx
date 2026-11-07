import * as passkeyOrigin from './passkeyOrigin'
import { useEffect, type ReactNode } from 'react'
import { act, cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react'
import { QueryClient } from '@tanstack/react-query'
import { beforeEach, afterEach, describe, expect, it, vi } from 'vitest'
import { createConfig } from '@privy-io/wagmi'
import { custom, recoverMessageAddress, stringToHex, type Address, type EIP1193Provider } from 'viem'
import { privateKeyToAccount } from 'viem/accounts'
import { arbitrumSepolia } from 'wagmi/chains'
import { useConnection, useSignMessage, type Config } from 'wagmi'
import type { ConnectedWallet } from '@privy-io/react-auth'
import { WalletProvider } from './WalletProvider'
import { useRegistrationWallet } from './useRegistrationWallet'
import { RegistrationWalletStep } from '../components/RegistrationWalletStep'

type Snapshot = { ready: boolean; authenticated: boolean; wallets: ConnectedWallet[]; user?: { id?: string; linkedAccounts: object[] } }
type Callbacks = Record<string, ((...input: never[]) => unknown) | undefined>
const auth = vi.hoisted(() => ({
  snapshot: { ready: true, authenticated: false, wallets: [] } as Snapshot,
  listeners: new Set<() => void>(),
  callbacks: new Map<string, Set<{ current: Callbacks }>>(),
  description: null as ReactNode,
  config: undefined as Config | undefined,
  createChallenge: vi.fn(),
  verifyWallet: vi.fn(),
  login: vi.fn(),
  initOAuth: vi.fn(),
  loginWithPasskey: vi.fn(),
  linkGoogle: vi.fn(),
  linkPasskey: vi.fn(),
  createWallet: vi.fn(),
  closePicker: vi.fn(),
}))
function updateAuth(patch: Partial<Snapshot>) {
  auth.snapshot = { ...auth.snapshot, ...patch }
  auth.listeners.forEach(listener => listener())
}
async function emit(event: string, callback: keyof Callbacks, ...input: unknown[]) {
  await act(async () => {
    await Promise.all([...auth.callbacks.get(event) ?? []].map(ref => ref.current[callback]?.(...input as never[])))
  })
}
vi.mock('@privy-io/react-auth', async () => {
  const { useSyncExternalStore, useRef, useEffect } = await import('react')
  const useSnapshot = () => useSyncExternalStore(listener => { auth.listeners.add(listener); return () => { auth.listeners.delete(listener) } }, () => auth.snapshot)
  function subscribe(event: string, callbacks: Callbacks = {}) {
    const ref = useRef(callbacks)
    ref.current = callbacks
    useEffect(() => {
      const group = auth.callbacks.get(event) ?? new Set()
      auth.callbacks.set(event, group)
      group.add(ref)
      return () => { group.delete(ref) }
    }, [event])
  }
  return {
    PrivyProvider: ({ children }: { children: ReactNode }) => children,
    usePrivy: () => useSnapshot(),
    useWallets: () => useSnapshot(),
    useConnectWallet: (callbacks: Callbacks) => {
      subscribe('connect', callbacks)
      return { connectWallet: (options: { description: ReactNode }) => { auth.description = options.description } }
    },
    useLinkAccount: (callbacks: Callbacks) => { subscribe('link', callbacks); return { linkGoogle: auth.linkGoogle, linkPasskey: auth.linkPasskey } },
    useLoginWithOAuth: () => ({ initOAuth: auth.initOAuth }),
    useLoginWithPasskey: (callbacks: Callbacks) => { subscribe('passkey', callbacks); return { loginWithPasskey: auth.loginWithPasskey } },
    useLogin: (callbacks: Callbacks) => { subscribe('login', callbacks); return { login: auth.login } },
    useConnectOrCreateWallet: (callbacks: Callbacks) => { subscribe('createOrConnect', callbacks); return {} },
    useCreateWallet: (callbacks: Callbacks) => { subscribe('create', callbacks); return { createWallet: auth.createWallet } },
  }
})
vi.mock('@privy-io/react-auth/ui', () => ({ usePletherWalletPickerClose: () => auth.closePicker }))
vi.mock('../config/wagmi', () => ({
  privyAppId: 'integration-privy',
  privyClientId: undefined,
  privyConfig: {},
  get wagmiConfig() { return auth.config },
}))
vi.mock('../api', () => ({
  createWalletChallenge: (...args: unknown[]) => auth.createChallenge(...args),
  verifyRegistrationWallet: (...args: unknown[]) => auth.verifyWallet(...args),
  getRegistrationSession: vi.fn(),
  InsightsApiError: class InsightsApiError extends Error {},
}))

const accounts = [1, 2].map(index => privateKeyToAccount(`0x${index.toString(16).padStart(64, '0')}`))
const message = 'Plether owner integration test: no transaction is submitted'
const storageKey = 'plether:privy:insights:owner:integration-privy'

function testWallet(index: number, embedded = false) {
  const account = accounts[index]
  const calls: string[] = []
  const listeners = new Map<string, Set<(value: unknown) => void>>()
  const provider = {
    on(event: string, listener: (value: unknown) => void) { const group = listeners.get(event) ?? new Set(); group.add(listener); listeners.set(event, group) },
    removeListener(event: string, listener: (value: unknown) => void) { listeners.get(event)?.delete(listener) },
    async request({ method, params }: { method: string; params?: readonly unknown[] }) {
      calls.push(method)
      if (method === 'eth_chainId') return '0x66eee'
      if (method === 'eth_accounts' || method === 'eth_requestAccounts') return [account.address]
      if (method === 'wallet_getPermissions' || method === 'wallet_requestPermissions') return [{ parentCapability: 'eth_accounts' }]
      if (method === 'personal_sign') {
        if (String(params?.[1]).toLowerCase() !== account.address.toLowerCase()) throw new Error('Provider account does not match requested signer')
        return account.signMessage({ message: { raw: String(params?.[0]) as `0x${string}` } })
      }
      throw new Error(`Unexpected wallet request: ${method}`)
    },
  } as unknown as EIP1193Provider
  return { calls, provider, wallet: {
    address: account.address, chainId: 'eip155:421614', type: 'ethereum', walletClientType: embedded ? 'privy' : 'metamask',
    connectorType: 'injected', meta: { id: embedded ? 'io.privy.wallet' : 'io.metamask', name: embedded ? 'Privy' : 'MetaMask' },
    getEthereumProvider: async () => provider,
  } as ConnectedWallet }
}

let sign: ReturnType<typeof useSignMessage>['mutateAsync']
let selected: ReturnType<typeof useRegistrationWallet>
function Probe() {
  const connection = useConnection()
  const actions = useRegistrationWallet()
  const { mutateAsync } = useSignMessage()
  useEffect(() => { sign = mutateAsync; selected = actions }, [mutateAsync, actions])
  return <>
    <p data-testid="account">{connection.isConnected ? connection.address : 'disconnected'}</p>
    <p data-testid="selecting">{String(actions.isSelecting)}</p>
    <button onClick={actions.connect}>Connect</button>
  </>
}
function tree(children: ReactNode = <Probe />) {
  return <WalletProvider queryClient={new QueryClient({ defaultOptions: { queries: { retry: false } } })}>{children}</WalletProvider>
}
async function expectAccount(address: Address) {
  await waitFor(() => { expect(screen.getByTestId('account')).toHaveTextContent(address) })
  await waitFor(() => { expect(screen.getByTestId('selecting')).toHaveTextContent('false') })
}
async function approve(wallet: ConnectedWallet) {
  await act(async () => { updateAuth({ wallets: [...auth.snapshot.wallets.filter(item => item.address !== wallet.address), wallet] }) })
  await emit('connect', 'onSuccess', { wallet } as never)
}

beforeEach(() => {
  auth.snapshot = { ready: true, authenticated: false, wallets: [] }
  auth.callbacks.clear()
  auth.listeners.clear()
  auth.description = null
  auth.createChallenge.mockReset()
  auth.verifyWallet.mockReset()
  auth.initOAuth.mockReset().mockResolvedValue(undefined)
  auth.loginWithPasskey.mockReset().mockResolvedValue(undefined)
  auth.login.mockReset()
  auth.createWallet.mockReset()
  auth.closePicker.mockReset().mockResolvedValue(undefined)
  sessionStorage.clear()
  window.history.replaceState({}, '', '/')
  auth.linkGoogle.mockReset()
  auth.linkPasskey.mockReset()
  auth.config = createConfig({ chains: [arbitrumSepolia], transports: { [arbitrumSepolia.id]: custom({ request: async () => { throw new Error('No RPC calls expected') } }) }, ssr: false, storage: null, multiInjectedProviderDiscovery: false })
})
afterEach(cleanup)

describe('Insights Privy/Wagmi integration', () => {
  it.each([false, true])('prevents unsupported-origin passkey requests (authenticated=%s)', async authenticated => {
    const origin = vi.spyOn(passkeyOrigin, 'isPasskeyOriginSupported').mockReturnValue(false)
    try {
      auth.snapshot = { ready: true, authenticated, wallets: [], user: authenticated ? { id: 'original-user', linkedAccounts: [] } : undefined }
      render(tree())
      fireEvent.click(screen.getByRole('button', { name: 'Connect' }))
      render(<>{auth.description}</>)
      const button = screen.getByRole('button', { name: authenticated ? 'Add passkey' : 'Log in with passkey' })
      expect(button).toBeDisabled()
      expect(screen.getByText('Passkeys need an HTTPS domain or localhost.')).toBeVisible()
      fireEvent.click(button)
      expect(auth.login).not.toHaveBeenCalled()
      expect(auth.linkPasskey).not.toHaveBeenCalled()
      expect(screen.getByRole('button', { name: authenticated ? 'Link Google account' : 'Log in with Google' })).toBeEnabled()
    } finally { origin.mockRestore() }
  })

  it.each(['Google'])('can retry %s login when closing the wallet picker fails', async method => {
    sessionStorage.setItem(storageKey, 'disconnected')
    auth.closePicker.mockRejectedValueOnce(new Error('Modal close failed'))
    render(tree())
    fireEvent.click(screen.getByRole('button', { name: 'Connect' }))
    const picker = render(<>{auth.description}</>)
    await act(async () => { fireEvent.click(screen.getByRole('button', { name: `Log in with ${method}` })) })
    picker.unmount()
    expect(auth.login).not.toHaveBeenCalled()
    expect(selected.error).toBe('Could not start sign-in. Try again.')

    fireEvent.click(screen.getByRole('button', { name: 'Connect' }))
    const retry = render(<>{auth.description}</>)
    await act(async () => { fireEvent.click(screen.getByRole('button', { name: `Log in with ${method}` })) })
    retry.unmount()
    expect(method === 'Google' ? auth.initOAuth : auth.loginWithPasskey).toHaveBeenCalledOnce()
  })

  it('keeps passkey failures inside the picker and allows an external-wallet fallback', async () => {
    sessionStorage.setItem(storageKey, 'disconnected')
    auth.loginWithPasskey.mockRejectedValueOnce(new Error('Offline credential service'))
    render(tree())
    fireEvent.click(screen.getByRole('button', { name: 'Connect' }))
    const initial = render(<>{auth.description}</>)
    await act(async () => { fireEvent.click(screen.getByRole('button', { name: 'Log in with passkey' })) })
    expect(selected.error).toBeNull()
    expect(auth.closePicker).not.toHaveBeenCalled()
    expect(screen.getByRole('alert')).toHaveTextContent('Could not sign in with passkey.')
    expect(screen.getByRole('button', { name: 'Log in with Email' })).toBeEnabled()
    expect(screen.getByRole('button', { name: 'Log in with Google' })).toBeEnabled()
    expect(screen.getByTestId('account')).toHaveTextContent('disconnected')
    const external = testWallet(0)
    await approve(external.wallet)
    initial.unmount()
    await expectAccount(external.wallet.address as Address)
    expect(selected.error).toBeNull()
  })

  it('cancels a pending passkey when the picker is dismissed and ignores late authentication', async () => {
    sessionStorage.setItem(storageKey, 'disconnected')
    let finish!: () => void
    auth.loginWithPasskey.mockImplementationOnce(() => new Promise<void>(resolve => { finish = resolve }))
    render(tree())
    fireEvent.click(screen.getByRole('button', { name: 'Connect' }))
    const picker = render(<>{auth.description}</>)
    await act(async () => { fireEvent.click(screen.getByRole('button', { name: 'Log in with passkey' })) })
    expect(screen.getByRole('button', { name: 'Log in with passkey' })).toBeDisabled()
    expect(auth.closePicker).not.toHaveBeenCalled()
    await emit('login', 'onError', 'exited_auth_flow')
    await emit('connect', 'onError', 'generic_connect_wallet_error')
    picker.unmount()
    const embedded = testWallet(1, true)
    await act(async () => { updateAuth({ wallets: [embedded.wallet] }) })
    await emit('passkey', 'onComplete', { user: { linkedAccounts: [{ type: 'wallet', chainType: 'ethereum', walletClientType: 'privy', address: embedded.wallet.address }] }, loginMethod: 'passkey' })
    await emit('create', 'onSuccess', { wallet: embedded.wallet })
    await act(async () => { finish() })
    expect(screen.getByTestId('account')).toHaveTextContent('disconnected')
    expect(selected.error).toBeNull()
    expect(auth.closePicker).not.toHaveBeenCalled()
  })

  it('accepts a native passkey alternative but ignores restored-session completion', async () => {
    sessionStorage.setItem(storageKey, 'disconnected')
    render(tree())
    fireEvent.click(screen.getByRole('button', { name: 'Connect' }))
    const picker = render(<>{auth.description}</>)
    await act(async () => { fireEvent.click(screen.getByRole('button', { name: 'Log in with Email' })) })
    picker.unmount()
    const embedded = testWallet(1, true)
    await act(async () => { updateAuth({ wallets: [embedded.wallet] }) })
    const user = { linkedAccounts: [{ type: 'wallet', chainType: 'ethereum', walletClientType: 'privy', address: embedded.wallet.address }] }
    await emit('login', 'onComplete', { user, loginMethod: 'passkey', wasAlreadyAuthenticated: true })
    expect(screen.getByTestId('account')).toHaveTextContent('disconnected')
    await emit('login', 'onComplete', { user, loginMethod: 'passkey', wasAlreadyAuthenticated: false })
    await expectAccount(embedded.wallet.address as Address)
  })

  it.each((['google', 'passkey'] as const).flatMap(method => (['failure', 'owner-change', 'session-change'] as const).map(outcome => ({ method, outcome }))))('handles $method wallet preparation $outcome without selecting a stale owner', async ({ method, outcome }) => {
    const embedded = testWallet(1, true)
    const external = testWallet(0)
    sessionStorage.setItem(storageKey, 'disconnected')
    let resolveCreation!: (wallet: ConnectedWallet) => void
    let rejectCreation!: (error: Error) => void
    auth.createWallet.mockImplementationOnce(() => new Promise<ConnectedWallet>((resolve, reject) => {
      resolveCreation = resolve
      rejectCreation = reject
    }))
    render(tree())
    fireEvent.click(screen.getByRole('button', { name: 'Connect' }))
    const picker = render(<>{auth.description}</>)
    await act(async () => { fireEvent.click(screen.getByRole('button', { name: method === 'google' ? 'Log in with Google' : 'Log in with passkey' })) })
    picker.unmount()
    const user = { id: 'google-user', linkedAccounts: [] }
    await emit(method === 'passkey' ? 'passkey' : 'login', 'onComplete', { loginMethod: method, user })
    expect(auth.createWallet).not.toHaveBeenCalled()
    await act(async () => { updateAuth({ authenticated: true, user }) })
    await waitFor(() => { expect(auth.createWallet).toHaveBeenCalledOnce() })
    if (outcome === 'owner-change') {
      fireEvent.click(screen.getByRole('button', { name: 'Connect' }))
      await approve(external.wallet)
      await expectAccount(external.wallet.address as Address)
    } else if (outcome === 'session-change') {
      await act(async () => { updateAuth({ user: { id: 'different-user', linkedAccounts: [] } }) })
    }
    await act(async () => { updateAuth({ wallets: [...auth.snapshot.wallets, embedded.wallet] }) })
    await emit('create', 'onSuccess', { wallet: embedded.wallet })
    await act(async () => {
      if (outcome === 'failure') rejectCreation(new Error('Offline wallet service'))
      else resolveCreation(embedded.wallet)
    })
    if (outcome === 'owner-change') await expectAccount(external.wallet.address as Address)
    else expect(screen.getByTestId('account')).toHaveTextContent('disconnected')
    if (outcome === 'failure') {
      expect(selected.error).toMatch(/wallet could not be prepared/)
      await act(async () => { updateAuth({ wallets: [] }) })
      auth.createWallet.mockResolvedValueOnce(embedded.wallet)
      fireEvent.click(screen.getByRole('button', { name: 'Connect' }))
      const retry = render(<>{auth.description}</>)
      await act(async () => { fireEvent.click(screen.getByRole('button', { name: 'Use email wallet' })) })
      retry.unmount()
      await act(async () => { updateAuth({ wallets: [embedded.wallet] }) })
      await expectAccount(embedded.wallet.address as Address)
      expect(method === 'google' ? auth.initOAuth : auth.loginWithPasskey).toHaveBeenCalledOnce()
    } else expect(selected.error).toBeNull()
  })

  it.each(['google', 'passkey'] as const)('waits for verified %s login before preparing a missing wallet', async method => {
    sessionStorage.setItem(storageKey, 'disconnected')
    render(tree())
    fireEvent.click(screen.getByRole('button', { name: 'Connect' }))
    const picker = render(<>{auth.description}</>)
    await act(async () => { fireEvent.click(screen.getByRole('button', { name: method === 'google' ? 'Log in with Google' : 'Log in with passkey' })) })
    picker.unmount()
    const embedded = testWallet(1, true)
    await act(async () => { updateAuth({ wallets: [embedded.wallet] }) })
    const created = { wallet: { address: embedded.wallet.address, chainType: 'ethereum' } }
    await emit('create', 'onSuccess', created)
    expect(screen.getByTestId('account')).toHaveTextContent('disconnected')
    const user = { id: 'verified-google-user', linkedAccounts: [] }
    auth.createWallet.mockResolvedValueOnce(embedded.wallet)
    await emit(method === 'passkey' ? 'passkey' : 'login', 'onComplete', { user, loginMethod: method })
    await act(async () => { updateAuth({ authenticated: true, user }) })
    await emit('create', 'onSuccess', created)
    await expectAccount(embedded.wallet.address as Address)
    expect(auth.createWallet).toHaveBeenCalledOnce()
  })

  it('does not link after the authenticated user changes while closing the picker', async () => {
    sessionStorage.setItem(storageKey, 'disconnected')
    auth.snapshot = { ready: true, authenticated: true, wallets: [], user: { id: 'original-user', linkedAccounts: [] } }
    let close!: () => void
    auth.closePicker.mockImplementationOnce(() => new Promise<void>(resolve => { close = resolve }))
    render(tree())
    fireEvent.click(screen.getByRole('button', { name: 'Connect' }))
    const picker = render(<>{auth.description}</>)
    await act(async () => { fireEvent.click(screen.getByRole('button', { name: 'Link Google account' })) })
    await act(async () => { updateAuth({ user: { id: 'different-user', linkedAccounts: [] } }); close() })
    picker.unmount()
    expect(auth.linkGoogle).not.toHaveBeenCalled()
    expect(screen.getByTestId('account')).toHaveTextContent('disconnected')
  })

  it('marks linked Google accounts while allowing additional passkeys', () => {
    auth.snapshot = { ready: true, authenticated: true, wallets: [], user: { id: 'original-user', linkedAccounts: [{ type: 'google_oauth' }, { type: 'passkey' }] } }
    render(tree())
    fireEvent.click(screen.getByRole('button', { name: 'Connect' }))
    render(<>{auth.description}</>)
    expect(screen.getByRole('button', { name: 'Google linked' })).toBeDisabled()
    expect(screen.getByRole('button', { name: 'Add passkey' })).toBeEnabled()
  })

  it.each(['google', 'passkey'] as const)('authenticates with %s and signs with the existing embedded wallet', async method => {
    const external = testWallet(0)
    const embedded = testWallet(1, true)
    auth.snapshot = { ready: true, authenticated: false, wallets: [external.wallet] }
    render(tree())
    await expectAccount(external.wallet.address as Address)
    fireEvent.click(screen.getByRole('button', { name: 'Connect' }))
    const picker = render(<>{auth.description}</>)
    await act(async () => { fireEvent.click(screen.getByRole('button', { name: method === 'google' ? 'Log in with Google' : 'Log in with passkey' })) })
    picker.unmount()
    if (method === 'google') {
      expect(auth.initOAuth).toHaveBeenCalledWith({ provider: 'google' })
      expect(auth.login).not.toHaveBeenCalled()
    } else expect(auth.loginWithPasskey).toHaveBeenCalledOnce()
    const user = { id: 'existing-user', linkedAccounts: [{ type: 'wallet', chainType: 'ethereum', walletClientType: 'privy', address: embedded.wallet.address }] }
    await act(async () => { updateAuth({ wallets: [external.wallet, embedded.wallet] }) })
    await emit('login', 'onComplete', { user, loginMethod: 'siwe' })
    await expectAccount(external.wallet.address as Address)
    await emit(method === 'passkey' ? 'passkey' : 'login', 'onComplete', { user, loginMethod: method })
    await expectAccount(embedded.wallet.address as Address)
    expect(auth.closePicker).toHaveBeenCalledOnce()
    let signature: `0x${string}` = '0x'
    await act(async () => { signature = await sign({ message }) })
    expect(await recoverMessageAddress({ message, signature })).toBe(embedded.wallet.address)
    expect(auth.createWallet).not.toHaveBeenCalled()
  })

  it.each(['google', 'passkey'] as const)('ignores canceled %s login and late completion', async method => {
    sessionStorage.setItem(storageKey, 'disconnected')
    render(tree())
    if (method === 'passkey') auth.loginWithPasskey.mockRejectedValueOnce({ privyErrorCode: 'passkey_not_allowed' })
    fireEvent.click(screen.getByRole('button', { name: 'Connect' }))
    const picker = render(<>{auth.description}</>)
    await act(async () => { fireEvent.click(screen.getByRole('button', { name: method === 'google' ? 'Log in with Google' : 'Log in with passkey' })) })
    picker.unmount()
    if (method === 'google') await emit('login', 'onError', 'exited_auth_flow')
    const embedded = testWallet(1, true)
    await act(async () => { updateAuth({ authenticated: true, wallets: [embedded.wallet] }) })
    await emit(method === 'passkey' ? 'passkey' : 'login', 'onComplete', { loginMethod: method, user: { linkedAccounts: [{ type: 'wallet', chainType: 'ethereum', walletClientType: 'privy', address: embedded.wallet.address }] } })
    await emit('create', 'onSuccess', { wallet: embedded.wallet })
    expect(screen.getByTestId('account')).toHaveTextContent('disconnected')
    expect(selected.error).toBeNull()
    expect(sessionStorage.getItem(`${storageKey}.google-login`)).toBeNull()
  })

  it.each(['google', 'passkey'] as const)('links %s without changing the owner, including SDK login and wallet-creation side effects', async method => {
    const external = testWallet(0)
    const embedded = testWallet(1, true)
    auth.snapshot = { ready: true, authenticated: true, wallets: [external.wallet, embedded.wallet], user: { id: 'original-user', linkedAccounts: [] } }
    sessionStorage.setItem(storageKey, external.wallet.address)
    render(tree())
    await expectAccount(external.wallet.address as Address)
    fireEvent.click(screen.getByRole('button', { name: 'Connect' }))
    const picker = render(<>{auth.description}</>)
    await act(async () => { fireEvent.click(screen.getByRole('button', { name: method === 'google' ? 'Link Google account' : 'Add passkey' })) })
    picker.unmount()
    expect(method === 'google' ? auth.linkGoogle : auth.linkPasskey).toHaveBeenCalledOnce()
    await emit('link', 'onSuccess', { user: auth.snapshot.user, linkMethod: method })
    await emit('login', 'onComplete', { user: { linkedAccounts: [{ type: 'wallet', chainType: 'ethereum', walletClientType: 'privy', address: embedded.wallet.address }] }, loginMethod: method })
    await emit('create', 'onSuccess', { wallet: embedded.wallet })
    await expectAccount(external.wallet.address as Address)
    let signature: `0x${string}` = '0x'
    await act(async () => { signature = await sign({ message }) })
    expect(await recoverMessageAddress({ message, signature })).toBe(external.wallet.address)
    expect(auth.login).not.toHaveBeenCalled()
    expect(auth.createWallet).not.toHaveBeenCalled()
  })

  it('keeps the selected owner when account linking conflicts, and cancels silently', async () => {
    const external = testWallet(0)
    auth.snapshot = { ready: true, authenticated: true, wallets: [external.wallet], user: { id: 'original-user', linkedAccounts: [] } }
    render(tree())
    await expectAccount(external.wallet.address as Address)
    await emit('link', 'onError', 'linked_to_another_user', { linkMethod: 'google' })
    expect(selected.error).toMatch(/already belong to another account/)
    await expectAccount(external.wallet.address as Address)
    await emit('link', 'onError', 'exited_link_flow', { linkMethod: 'passkey' })
    expect(selected.error).toBeNull()
  })

  it('finishes Google login after a full-page OAuth return to the same saved request', async () => {
    sessionStorage.setItem(storageKey, 'disconnected')
    const view = render(tree())
    fireEvent.click(screen.getByRole('button', { name: 'Connect' }))
    const picker = render(<>{auth.description}</>)
    await act(async () => { fireEvent.click(screen.getByRole('button', { name: 'Log in with Google' })) })
    picker.unmount()
    view.unmount()
    window.history.replaceState({}, '', '/?privy_oauth_provider=google&privy_oauth_code=test&privy_oauth_state=test')
    const embedded = testWallet(1, true)
    auth.snapshot = { ready: true, authenticated: true, wallets: [embedded.wallet] }
    render(tree())
    expect(screen.getByTestId('account')).toHaveTextContent('disconnected')
    await emit('login', 'onComplete', { user: { linkedAccounts: [{ type: 'wallet', chainType: 'ethereum', walletClientType: 'privy', address: embedded.wallet.address }] }, loginMethod: 'google' })
    await expectAccount(embedded.wallet.address as Address)
    expect(sessionStorage.getItem(`${storageKey}.google-login`)).toBeNull()
  })

  it('offers email above wallets and closes the picker before starting email login', async () => {
    let close!: () => void
    auth.closePicker.mockImplementation(() => new Promise<void>(resolve => { close = resolve }))
    render(tree())
    fireEvent.click(screen.getByRole('button', { name: 'Connect' }))
    render(<>{auth.description}</>)
    fireEvent.click(screen.getByRole('button', { name: 'Log in with Email' }))
    expect(auth.closePicker).toHaveBeenCalledOnce()
    expect(auth.login).not.toHaveBeenCalled()
    await act(async () => { close() })
    expect(auth.login).toHaveBeenCalledWith({ loginMethods: ['email'] })
    expect(auth.createWallet).not.toHaveBeenCalled()
  })

  it('uses the authenticated email wallet after an external owner without starting login again', async () => {
    const external = testWallet(0)
    const embedded = testWallet(1, true)
    auth.snapshot = { ready: true, authenticated: true, wallets: [external.wallet, embedded.wallet] }
    sessionStorage.setItem(storageKey, external.wallet.address)
    render(tree())
    await expectAccount(external.wallet.address as Address)
    fireEvent.click(screen.getByRole('button', { name: 'Connect' }))
    render(<>{auth.description}</>)
    fireEvent.click(screen.getByRole('button', { name: 'Use email wallet' }))
    await expectAccount(embedded.wallet.address as Address)
    expect(auth.closePicker).toHaveBeenCalledOnce()
    expect(auth.login).not.toHaveBeenCalled()
    expect(auth.createWallet).not.toHaveBeenCalled()
    let signature: `0x${string}` = '0x'
    await act(async () => { signature = await sign({ account: embedded.wallet.address as Address, message }) })
    expect(await recoverMessageAddress({ message, signature })).toBe(embedded.wallet.address)
  })

  it('waits for an authenticated linked email wallet to become available', async () => {
    const external = testWallet(0)
    const embedded = testWallet(1, true)
    auth.snapshot = { ready: true, authenticated: true, wallets: [external.wallet], user: { linkedAccounts: [
      { type: 'wallet', chainType: 'ethereum', walletClientType: 'privy', address: embedded.wallet.address },
    ] } }
    render(tree())
    await expectAccount(external.wallet.address as Address)
    fireEvent.click(screen.getByRole('button', { name: 'Connect' }))
    render(<>{auth.description}</>)
    fireEvent.click(screen.getByRole('button', { name: 'Use email wallet' }))
    await waitFor(() => { expect(screen.getByTestId('account')).toHaveTextContent('disconnected') })
    await act(async () => { updateAuth({ wallets: [external.wallet, embedded.wallet] }) })
    await expectAccount(embedded.wallet.address as Address)
    expect(auth.login).not.toHaveBeenCalled()
    expect(auth.createWallet).not.toHaveBeenCalled()
  })

  it('creates an email wallet for an authenticated user without one, after closing the picker', async () => {
    const external = testWallet(0)
    const embedded = testWallet(1, true)
    auth.snapshot = { ready: true, authenticated: true, wallets: [external.wallet] }
    auth.createWallet.mockResolvedValue({ address: embedded.wallet.address, chainType: 'ethereum' })
    render(tree())
    await expectAccount(external.wallet.address as Address)
    fireEvent.click(screen.getByRole('button', { name: 'Connect' }))
    render(<>{auth.description}</>)
    await act(async () => { fireEvent.click(screen.getByRole('button', { name: 'Use email wallet' })) })
    expect(auth.closePicker).toHaveBeenCalledOnce()
    expect(auth.createWallet).toHaveBeenCalledOnce()
    expect(auth.login).not.toHaveBeenCalled()
    await act(async () => { updateAuth({ wallets: [external.wallet, embedded.wallet] }) })
    await expectAccount(embedded.wallet.address as Address)
  })

  it('ignores email wallet creation that completes after another owner is chosen', async () => {
    const external = testWallet(0)
    const embedded = testWallet(1, true)
    let finishCreation!: (wallet: { address: string; chainType: string }) => void
    auth.snapshot = { ready: true, authenticated: true, wallets: [external.wallet] }
    auth.createWallet.mockImplementation(() => new Promise(resolve => { finishCreation = resolve }))
    render(tree())
    await expectAccount(external.wallet.address as Address)
    fireEvent.click(screen.getByRole('button', { name: 'Connect' }))
    render(<>{auth.description}</>)
    await act(async () => { fireEvent.click(screen.getByRole('button', { name: 'Use email wallet' })) })
    expect(auth.createWallet).toHaveBeenCalledOnce()
    act(() => { selected.selectWallet(external.wallet.address) })
    await act(async () => { updateAuth({ wallets: [external.wallet, embedded.wallet] }); finishCreation({ address: embedded.wallet.address, chainType: 'ethereum' }) })
    await emit('create', 'onSuccess', { wallet: { address: embedded.wallet.address, chainType: 'ethereum' } } as never)
    await expectAccount(external.wallet.address as Address)
  })

  it('reports failed authenticated email wallet creation without starting a login flow', async () => {
    const external = testWallet(0)
    auth.snapshot = { ready: true, authenticated: true, wallets: [external.wallet] }
    auth.createWallet.mockRejectedValue(new Error('Wallet creation unavailable'))
    render(tree())
    await expectAccount(external.wallet.address as Address)
    fireEvent.click(screen.getByRole('button', { name: 'Connect' }))
    render(<>{auth.description}</>)
    await act(async () => { fireEvent.click(screen.getByRole('button', { name: 'Use email wallet' })) })
    expect(selected.error).toBe('Could not prepare your email wallet. Try again.')
    expect(auth.login).not.toHaveBeenCalled()
    await expectAccount(external.wallet.address as Address)
  })

  it('switches from an external owner to the explicitly chosen verified email wallet', async () => {
    const external = testWallet(0)
    const embedded = testWallet(1, true)
    auth.snapshot = { ready: true, authenticated: false, wallets: [external.wallet] }
    render(tree())
    await expectAccount(external.wallet.address as Address)
    fireEvent.click(screen.getByRole('button', { name: 'Connect' }))
    render(<>{auth.description}</>)
    await act(async () => { fireEvent.click(screen.getByRole('button', { name: 'Log in with Email' })) })
    expect(auth.login).toHaveBeenCalledOnce()
    await emit('login', 'onComplete', { user: { linkedAccounts: [
      { type: 'wallet', chainType: 'ethereum', walletClientType: 'privy', address: embedded.wallet.address },
    ] }, loginMethod: 'email' } as never)
    await act(async () => { updateAuth({ authenticated: true, wallets: [external.wallet, embedded.wallet] }) })
    await expectAccount(embedded.wallet.address as Address)
  })

  it('connects an explicitly approved wallet and signs with its real provider', async () => {
    const owner = testWallet(0)
    render(tree())
    fireEvent.click(screen.getByRole('button', { name: 'Connect' }))
    await approve(owner.wallet)
    await expectAccount(owner.wallet.address as Address)
    let signature: `0x${string}` = '0x'
    await act(async () => { signature = await sign({ account: owner.wallet.address as Address, message }) })
    expect(await recoverMessageAddress({ message, signature })).toBe(owner.wallet.address)
  })

  it('replaces a saved unavailable owner on approval and reconnects it after remount', async () => {
    const owner = testWallet(1)
    sessionStorage.setItem(storageKey, accounts[0].address)
    const view = render(tree())
    fireEvent.click(screen.getByRole('button', { name: 'Connect' }))
    await approve(owner.wallet)
    await expectAccount(owner.wallet.address as Address)
    view.unmount()
    auth.config!.setState(state => ({ ...state, current: null, connections: new Map(), status: 'disconnected' }))
    render(tree())
    await expectAccount(owner.wallet.address as Address)
  })

  it('switches between accounts from the same extension and signs with the selected owner', async () => {
    const owner = testWallet(0)
    const other = testWallet(1)
    auth.snapshot = { ...auth.snapshot, wallets: [owner.wallet] }
    render(tree())
    await expectAccount(owner.wallet.address as Address)
    fireEvent.click(screen.getByRole('button', { name: 'Connect' }))
    await approve(other.wallet)
    await expectAccount(owner.wallet.address as Address)
    const stale = await auth.config!.state.connections.get(auth.config!.state.current!)!.connector.getProvider() as EIP1193Provider
    act(() => { selected.selectWallet(other.wallet.address) })
    await expect(stale.request({ method: 'personal_sign', params: [stringToHex(message), owner.wallet.address] })).rejects.toThrow('selected wallet changed')
    await expectAccount(other.wallet.address as Address)
    let signature: `0x${string}` = '0x'
    await act(async () => { signature = await sign({ account: other.wallet.address as Address, message }) })
    expect(await recoverMessageAddress({ message, signature })).toBe(other.wallet.address)
    expect(other.calls).toContain('personal_sign')
    expect(owner.calls).not.toContain('personal_sign')
  })

  it('invalidates a cached signer when its wallet disappears and reconnects after approval', async () => {
    const owner = testWallet(0)
    auth.snapshot = { ...auth.snapshot, wallets: [owner.wallet] }
    render(tree())
    await expectAccount(owner.wallet.address as Address)
    const stale = await auth.config!.state.connections.get(auth.config!.state.current!)!.connector.getProvider() as EIP1193Provider
    await act(async () => { updateAuth({ wallets: [] }) })
    await waitFor(() => { expect(screen.getByTestId('account')).toHaveTextContent('disconnected') })
    await expect(stale.request({ method: 'personal_sign', params: [stringToHex(message), owner.wallet.address] })).rejects.toThrow('selected wallet changed')
    fireEvent.click(screen.getByRole('button', { name: 'Connect' }))
    await approve(owner.wallet)
    await expectAccount(owner.wallet.address as Address)
    expect(owner.calls).not.toContain('personal_sign')
  })

  it('recovers the selected owner when an earlier connector finishes after switching', async () => {
    const owner = testWallet(0)
    const other = testWallet(1)
    let resolveEarlierProvider!: (provider: EIP1193Provider) => void
    const earlierProvider = new Promise<EIP1193Provider>(resolve => { resolveEarlierProvider = resolve })
    owner.wallet.getEthereumProvider = vi.fn(() => earlierProvider)
    sessionStorage.setItem(storageKey, owner.wallet.address)
    auth.snapshot = { ...auth.snapshot, wallets: [owner.wallet, other.wallet] }
    render(tree())
    await waitFor(() => { expect(owner.wallet.getEthereumProvider).toHaveBeenCalled() })
    act(() => { selected.selectWallet(other.wallet.address) })
    await expectAccount(other.wallet.address as Address)
    await act(async () => { resolveEarlierProvider(owner.provider) })
    await expectAccount(other.wallet.address as Address)
    let signature: `0x${string}` = '0x'
    await act(async () => { signature = await sign({ account: other.wallet.address as Address, message }) })
    expect(await recoverMessageAddress({ message, signature })).toBe(other.wallet.address)
  })

  it('does not let an earlier connect approval replace a later explicit owner choice', async () => {
    const owner = testWallet(0)
    const other = testWallet(1)
    sessionStorage.setItem(storageKey, owner.wallet.address)
    auth.snapshot = { ...auth.snapshot, wallets: [owner.wallet, other.wallet] }
    render(tree())
    await expectAccount(owner.wallet.address as Address)
    fireEvent.click(screen.getByRole('button', { name: 'Connect' }))
    act(() => { selected.selectWallet(other.wallet.address) })
    await expectAccount(other.wallet.address as Address)
    await act(async () => { updateAuth({ wallets: [owner.wallet] }) })
    await waitFor(() => { expect(screen.getByTestId('account')).toHaveTextContent('disconnected') })
    await emit('connect', 'onSuccess', { wallet: owner.wallet } as never)
    expect(screen.getByTestId('account')).toHaveTextContent('disconnected')
    expect(sessionStorage.getItem(storageKey)).toBe(other.wallet.address.toLowerCase())
  })

  it('stays disconnected after canceled email even if an embedded wallet appears later', async () => {
    render(tree())
    fireEvent.click(screen.getByRole('button', { name: 'Connect' }))
    render(<>{auth.description}</>)
    fireEvent.click(screen.getByRole('button', { name: 'Log in with Email' }))
    await emit('login', 'onError', 'exited_auth_flow' as never)
    const embedded = testWallet(0, true)
    await act(async () => { updateAuth({ wallets: [embedded.wallet] }) })
    await emit('create', 'onSuccess', { wallet: { address: embedded.wallet.address, chainType: 'ethereum' } } as never)
    await emit('connect', 'onSuccess', { wallet: embedded.wallet } as never)
    expect(screen.getByTestId('account')).toHaveTextContent('disconnected')
    expect(sessionStorage.getItem(storageKey)).toBe('disconnected')
  })

  it('does not restore an unavailable saved owner when email login is canceled', async () => {
    const embedded = testWallet(0, true)
    sessionStorage.setItem(storageKey, embedded.wallet.address)
    render(tree())
    fireEvent.click(screen.getByRole('button', { name: 'Connect' }))
    render(<>{auth.description}</>)
    fireEvent.click(screen.getByRole('button', { name: 'Log in with Email' }))
    await emit('login', 'onError', 'exited_auth_flow' as never)
    await act(async () => { updateAuth({ wallets: [embedded.wallet] }) })
    await emit('create', 'onSuccess', { wallet: { address: embedded.wallet.address, chainType: 'ethereum' } } as never)
    expect(screen.getByTestId('account')).toHaveTextContent('disconnected')
    expect(sessionStorage.getItem(storageKey)).toBe('disconnected')
  })

  it('waits for verified email and the created embedded wallet before connecting', async () => {
    render(tree())
    fireEvent.click(screen.getByRole('button', { name: 'Connect' }))
    render(<>{auth.description}</>)
    fireEvent.click(screen.getByRole('button', { name: 'Log in with Email' }))
    await emit('login', 'onComplete', { user: { linkedAccounts: [] }, loginMethod: 'email' } as never)
    expect(screen.getByTestId('account')).toHaveTextContent('disconnected')
    const embedded = testWallet(0, true)
    await act(async () => { updateAuth({ authenticated: true, wallets: [embedded.wallet] }) })
    await emit('create', 'onSuccess', { wallet: { address: embedded.wallet.address, chainType: 'ethereum' } } as never)
    await expectAccount(embedded.wallet.address as Address)
  })

  it('hands the actual selected-wallet signature to registration verification', async () => {
    const owner = testWallet(0)
    const registration = { status: 'in_progress' as const, csrfToken: 'csrf', expiresAt: '2026-11-06T12:00:00Z',
      steps: { xIdentity: 'verified' as const, xFollow: 'verified' as const, wallet: 'pending' as const, completed: false },
      requiredConsents: { rulesVersion: 'v1', privacyVersion: 'v1' } }
    const onVerified = vi.fn()
    auth.createChallenge.mockResolvedValue({ message })
    auth.verifyWallet.mockImplementation(async (_slug, _csrf, address, signature) => {
      expect(await recoverMessageAddress({ message, signature })).toBe(owner.wallet.address)
      expect(address).toBe(owner.wallet.address.toLowerCase())
      return { ...registration, steps: { ...registration.steps, wallet: 'verified' } }
    })
    render(tree(<RegistrationWalletStep slug="testnet" registration={registration} onVerified={onVerified} />))
    fireEvent.click(screen.getByRole('button', { name: 'Connect wallet' }))
    await approve(owner.wallet)
    await waitFor(() => { expect(screen.getByRole('button', { name: 'Sign and verify wallet' })).toBeEnabled() })
    fireEvent.click(screen.getByRole('button', { name: 'Sign and verify wallet' }))
    await waitFor(() => { expect(onVerified).toHaveBeenCalledOnce() })
    expect(auth.verifyWallet).toHaveBeenCalledOnce()
  })

  it('does not sign or submit a registration challenge after the connected owner disappears', async () => {
    const owner = testWallet(0)
    const registration = { status: 'in_progress' as const, csrfToken: 'csrf', expiresAt: '2026-11-06T12:00:00Z',
      steps: { xIdentity: 'verified' as const, xFollow: 'verified' as const, wallet: 'pending' as const, completed: false },
      requiredConsents: { rulesVersion: 'v1', privacyVersion: 'v1' } }
    let resolveChallenge!: (value: { message: string }) => void
    auth.createChallenge.mockImplementation(() => new Promise(resolve => { resolveChallenge = resolve }))
    render(tree(<RegistrationWalletStep slug="testnet" registration={registration} onVerified={vi.fn()} />))
    fireEvent.click(screen.getByRole('button', { name: 'Connect wallet' }))
    await approve(owner.wallet)
    await waitFor(() => { expect(screen.getByRole('button', { name: 'Sign and verify wallet' })).toBeEnabled() })
    fireEvent.click(screen.getByRole('button', { name: 'Sign and verify wallet' }))
    await waitFor(() => { expect(auth.createChallenge).toHaveBeenCalledOnce() })
    await act(async () => { updateAuth({ wallets: [] }) })
    await waitFor(() => { expect(screen.queryByText('Connected owner')).not.toBeInTheDocument() })
    await act(async () => { resolveChallenge({ message }) })
    expect(owner.calls).not.toContain('personal_sign')
    expect(auth.verifyWallet).not.toHaveBeenCalled()
    expect(screen.getByRole('alert')).toHaveTextContent('connected wallet or network changed')
  })
})
