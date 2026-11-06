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

type Snapshot = { ready: boolean; authenticated: boolean; wallets: ConnectedWallet[] }
type Callbacks = { onSuccess?: (input: { wallet: ConnectedWallet }) => unknown; onError?: (code: string) => unknown; onComplete?: (input: object) => unknown }
const auth = vi.hoisted(() => ({
  snapshot: { ready: true, authenticated: false, wallets: [] } as Snapshot,
  listeners: new Set<() => void>(),
  callbacks: new Map<string, Set<{ current: Callbacks }>>(),
  description: null as ReactNode,
  config: undefined as Config | undefined,
  createChallenge: vi.fn(),
  verifyWallet: vi.fn(),
}))
function updateAuth(patch: Partial<Snapshot>) {
  auth.snapshot = { ...auth.snapshot, ...patch }
  auth.listeners.forEach(listener => listener())
}
async function emit(event: string, callback: keyof Callbacks, input: never) {
  await act(async () => {
    await Promise.all([...auth.callbacks.get(event) ?? []].map(ref => ref.current[callback]?.(input)))
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
    usePrivy: () => ({ ...useSnapshot(), user: undefined }),
    useWallets: () => useSnapshot(),
    useConnectWallet: (callbacks: Callbacks) => {
      subscribe('connect', callbacks)
      return { connectWallet: (options: { description: ReactNode }) => { auth.description = options.description } }
    },
    useLogin: (callbacks: Callbacks) => { subscribe('login', callbacks); return { login: vi.fn() } },
    useConnectOrCreateWallet: (callbacks: Callbacks) => { subscribe('createOrConnect', callbacks); return {} },
    useCreateWallet: (callbacks: Callbacks) => { subscribe('create', callbacks); return {} },
  }
})
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
  sessionStorage.clear()
  auth.config = createConfig({ chains: [arbitrumSepolia], transports: { [arbitrumSepolia.id]: custom({ request: async () => { throw new Error('No RPC calls expected') } }) }, ssr: false, storage: null, multiInjectedProviderDiscovery: false })
})
afterEach(cleanup)

describe('Insights Privy/Wagmi integration', () => {
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
