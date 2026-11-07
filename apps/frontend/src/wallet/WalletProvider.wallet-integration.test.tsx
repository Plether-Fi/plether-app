import * as passkeyOrigin from './passkeyOrigin'
import { useEffect, useLayoutEffect, useRef, type ReactNode } from 'react'
import { act, cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react'
import { QueryClient } from '@tanstack/react-query'
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest'
import { getWalletClient } from '@wagmi/core'
import { useAccount } from 'wagmi'
import { verifyMessage, verifyTypedData, type Hex } from 'viem'
import { privateKeyToAccount } from 'viem/accounts'
import { MemoryRouter } from 'react-router-dom'
import type { ConnectedWallet } from '@privy-io/react-auth'
import type { WalletActions } from './useWalletActions'

type Callback = (...input: never[]) => unknown
type Callbacks = Record<string, Callback | undefined>
type PickerEntry = 'main' | 'native'
type ConnectionDescription = (close: () => Promise<void>) => ReactNode

const sdk = vi.hoisted(() => {
  vi.stubEnv('VITE_PRIVY_APP_ID', 'bridge-integration')
  return {
    ready: true,
    walletsReady: true,
    authenticated: false,
    user: null as { id?: string; linkedAccounts: unknown[] } | null,
    wallets: [] as ConnectedWallet[],
    nativeWallet: undefined as ConnectedWallet | undefined,
    events: new Map<string, Set<{ current: Callbacks }>>(),
    connect: vi.fn(),
    login: vi.fn(),
    initOAuth: vi.fn(),
    loginWithPasskey: vi.fn(),
    linkGoogle: vi.fn(),
    linkPasskey: vi.fn(),
    logout: vi.fn(),
    openNative: vi.fn(),
    closePicker: vi.fn(),
    closeNativePicker: vi.fn(),
    createWallet: vi.fn(),
    showMfa: vi.fn(),
    nativeDescription: undefined as ConnectionDescription | undefined,
    nativeAccountActions: null as ReactNode,
  }
})

// Mock the remote Privy authentication/modal boundary. The installed
// @privy-io/wagmi bridge, Wagmi connectors/actions, provider proxies, and viem
// signing/verification run unchanged so callback and connector races are real.
vi.mock('@privy-io/react-auth', () => {
  function useEvents(channel: string, callbacks: Callbacks = {}) {
    const ref = useRef(callbacks)
    useLayoutEffect(() => { ref.current = callbacks }, [callbacks])
    useEffect(() => {
      const handlers = sdk.events.get(channel) ?? new Set()
      sdk.events.set(channel, handlers)
      handlers.add(ref)
      return () => { handlers.delete(ref) }
    }, [channel])
  }
  return {
    PrivyProvider: ({ children }: { children: ReactNode }) => children,
    usePrivy: () => ({ ready: sdk.ready, authenticated: sdk.authenticated, user: sdk.user, logout: sdk.logout }),
    useWallets: () => ({ ready: sdk.walletsReady, wallets: sdk.wallets }),
    useModalStatus: () => ({ isOpen: false }),
    useActiveWallet: () => ({ wallet: sdk.nativeWallet, connect: sdk.openNative, setActiveWallet: vi.fn() }),
    useConnectWallet: (callbacks: Callbacks) => { useEvents('connect', callbacks); return { connectWallet: sdk.connect } },
    useConnectOrCreateWallet: (callbacks: Callbacks) => { useEvents('connectOrCreate', callbacks); return { connectOrCreateWallet: vi.fn() } },
    useLinkAccount: (callbacks: Callbacks) => { useEvents('link', callbacks); return { linkGoogle: sdk.linkGoogle, linkPasskey: sdk.linkPasskey } },
    useLoginWithOAuth: () => ({ initOAuth: sdk.initOAuth }),
    useLoginWithPasskey: (callbacks: Callbacks) => { useEvents('passkey', callbacks); return { loginWithPasskey: sdk.loginWithPasskey } },
    useLogin: (callbacks: Callbacks) => { useEvents('login', callbacks); return { login: sdk.login } },
    useCreateWallet: (callbacks: Callbacks) => { useEvents('create', callbacks); return { createWallet: sdk.createWallet } },
    useExportWallet: () => ({ exportWallet: vi.fn() }),
    useSetWalletRecovery: () => ({ setWalletRecovery: vi.fn() }),
    useMfaEnrollment: () => ({ showMfaEnrollmentModal: sdk.showMfa }),
  }
})
vi.mock('@privy-io/react-auth/ui', () => ({
  usePletherWalletPickerClose: () => sdk.closePicker,
  WalletsDialog: ({ connectionDescription, accountActions }: { connectionDescription: ConnectionDescription; accountActions: ReactNode }) => {
    sdk.nativeDescription = connectionDescription
    sdk.nativeAccountActions = accountActions
    return null
  },
}))
// Keep the real network-switch hook without loading unrelated trading hooks.
vi.mock('../hooks', async () => import('../hooks/useSwitchToArbitrumSepolia'))

import { config } from '../config/wagmi'
import { WalletProvider } from './WalletProvider'
import { useWalletActions } from './useWalletActions'
import { ConnectButton } from '../components/wallet/ConnectButton'

const selectionKey = 'plether.privy.bridge-integration.owner'
let actions: WalletActions
function Probe() {
  const value = useWalletActions()
  const account = useAccount()
  useEffect(() => { actions = value }, [value])
  return <output data-testid="connection">{account.status}:{account.address ?? ''}:{String(value.signerReady)}</output>
}

function mount(header = false) {
  const client = new QueryClient({ defaultOptions: { queries: { retry: false }, mutations: { retry: false } } })
  const tree = () => <WalletProvider queryClient={client}>
    <MemoryRouter initialEntries={['/vaults']}><Probe />{header ? <ConnectButton /> : null}</MemoryRouter>
  </WalletProvider>
  const view = render(tree())
  return { ...view, refresh: () => { view.rerender(tree()) } }
}

async function emit(channel: string, event: string, ...input: unknown[]) {
  await act(async () => {
    await Promise.all([...sdk.events.get(channel) ?? []].map(ref => ref.current[event]?.(...input as never[])))
  })
}

function testWallet(index: number, client = 'metamask', initialChainId = 421614) {
  // Synthetic public test keys; these accounts never contact a chain.
  const account = privateKeyToAccount(`0x${(index + 1).toString(16).padStart(64, '0')}` as Hex)
  const listeners = new Map<string, Set<(value: unknown) => void>>()
  let chainId = initialChainId
  let available = true
  const request = vi.fn(async ({ method, params = [] }: { method: string; params?: readonly unknown[] }) => {
    if (method === 'eth_chainId') return `0x${chainId.toString(16)}`
    if (method === 'eth_accounts' || method === 'eth_requestAccounts') return available ? [account.address] : []
    if (method === 'wallet_requestPermissions') return [{ parentCapability: 'eth_accounts' }]
    if (method === 'wallet_switchEthereumChain') {
      chainId = Number((params[0] as { chainId: string }).chainId)
      listeners.get('chainChanged')?.forEach(fn => { fn(`0x${chainId.toString(16)}`) })
      return null
    }
    if (method === 'personal_sign') {
      if (!available || String(params[1]).toLowerCase() !== account.address.toLowerCase()) throw new Error('Unauthorized test owner')
      return account.signMessage({ message: { raw: params[0] as Hex } })
    }
    if (method === 'eth_signTypedData_v4') {
      if (!available || String(params[0]).toLowerCase() !== account.address.toLowerCase()) throw new Error('Unauthorized test owner')
      return account.signTypedData(JSON.parse(params[1] as string))
    }
    throw new Error(`Unsupported test provider method: ${method}`)
  })
  const provider = {
    request,
    on(event: string, fn: (value: unknown) => void) {
      if (!listeners.has(event)) listeners.set(event, new Set())
      listeners.get(event)!.add(fn)
    },
    removeListener(event: string, fn: (value: unknown) => void) { listeners.get(event)?.delete(fn) },
  }
  const wallet = {
    address: account.address,
    chainId: `eip155:${initialChainId.toString()}`,
    chainType: 'ethereum',
    type: 'ethereum',
    walletClientType: client,
    connectorType: 'injected',
    meta: { id: client === 'privy' ? 'io.privy.wallet' : 'io.metamask', name: client },
    getEthereumProvider: vi.fn(async () => provider),
    disconnect: vi.fn(),
  } as unknown as ConnectedWallet
  return { wallet, account, provider, request, revoke: () => { available = false } }
}

async function expectConnected(address: string) {
  await waitFor(() => {
    expect(screen.getByTestId('connection')).toHaveTextContent(`connected:${address}:true`)
  })
}

async function expectDisconnected() {
  await waitFor(() => { expect(screen.getByTestId('connection')).toHaveTextContent('disconnected::false') })
}

function renderPicker(entry: PickerEntry = 'main') {
  if (entry === 'native') {
    expect(sdk.nativeDescription).toBeTypeOf('function')
    return render(<>{sdk.nativeDescription?.(sdk.closeNativePicker)}</>)
  }
  act(() => { actions.connect() })
  const options = sdk.connect.mock.lastCall?.[0] as { description: ReactNode }
  return render(<>{options.description}</>)
}

async function beginEmail() {
  const view = renderPicker()
  await act(async () => { fireEvent.click(screen.getByRole('button', { name: 'Log in with Email' })) })
  view.unmount()
}

beforeEach(() => {
  sessionStorage.clear()
  window.history.replaceState({}, '', '/')
  vi.clearAllMocks()
  sdk.events.clear()
  sdk.ready = true
  sdk.walletsReady = true
  sdk.authenticated = false
  sdk.user = null
  sdk.wallets = []
  sdk.nativeWallet = undefined
  sdk.initOAuth.mockReset().mockResolvedValue(undefined)
  sdk.loginWithPasskey.mockReset().mockResolvedValue(undefined)
  sdk.logout.mockResolvedValue(undefined)
  sdk.openNative.mockResolvedValue({})
  sdk.closePicker.mockResolvedValue(undefined)
  sdk.closeNativePicker.mockResolvedValue(undefined)
  sdk.createWallet.mockResolvedValue(undefined)
  sdk.nativeDescription = undefined
  sdk.nativeAccountActions = null
  sdk.showMfa.mockReset().mockResolvedValue(undefined)
  config._internal.connectors.setState([])
  config.setState(state => ({ ...state, connections: new Map(), current: null, status: 'disconnected' }))
})

afterEach(() => { cleanup() })

describe('WalletProvider and installed Privy/Wagmi bridge integration', () => {
  it('opens native MFA settings from the account dialog without changing its owner or signer', async () => {
    const embedded = testWallet(0, 'privy')
    const other = testWallet(1)
    sdk.authenticated = true
    sdk.user = { id: 'embedded-user', linkedAccounts: [] }
    sdk.wallets = [embedded.wallet, other.wallet]
    sdk.nativeWallet = embedded.wallet
    sessionStorage.setItem(selectionKey, embedded.account.address)
    sdk.showMfa.mockImplementation(() => new Promise<void>(() => {}))
    mount()
    await expectConnected(embedded.account.address)
    render(<>{sdk.nativeAccountActions}</>)
    const client = await getWalletClient(config)
    await act(async () => { fireEvent.click(screen.getByRole('button', { name: 'Wallet MFA settings' })) })
    expect(sdk.showMfa).toHaveBeenCalledOnce()
    expect(sdk.login).not.toHaveBeenCalled()
    expect(sdk.createWallet).not.toHaveBeenCalled()
    await emit('login', 'onComplete', { loginMethod: 'passkey', user: { linkedAccounts: [{ type: 'wallet', chainType: 'ethereum', walletClientType: 'privy', address: other.account.address }] } })
    await emit('create', 'onSuccess', { wallet: other.wallet })
    await expectConnected(embedded.account.address)
    const message = 'The selected embedded owner remains the signer while managing MFA'
    expect(await verifyMessage({ address: embedded.account.address, message, signature: await client.signMessage({ message }) })).toBe(true)
    expect(sessionStorage.getItem(selectionKey)).toBe(embedded.account.address)
  })

  it.each([false, true])('does not start passkey login/linking on an unsupported origin (authenticated=%s)', async authenticated => {
    const origin = vi.spyOn(passkeyOrigin, 'isPasskeyOriginSupported').mockReturnValue(false)
    try {
      sdk.authenticated = authenticated
      sdk.user = authenticated ? { id: 'original-user', linkedAccounts: [] } : null
      mount()
      renderPicker()
      const button = screen.getByRole('button', { name: authenticated ? 'Add passkey' : 'Log in with passkey' })
      expect(button).toBeDisabled()
      expect(screen.getByText('Passkeys need an HTTPS domain or localhost.')).toBeVisible()
      fireEvent.click(button)
      expect(sdk.login).not.toHaveBeenCalled()
      expect(sdk.linkPasskey).not.toHaveBeenCalled()
      expect(screen.getByRole('button', { name: authenticated ? 'Link Google account' : 'Log in with Google' })).toBeEnabled()
    } finally { origin.mockRestore() }
  })

  it.each(['Google'])('can retry %s sign-in after the connection picker fails to close', async method => {
    sessionStorage.setItem(selectionKey, 'disconnected')
    sdk.closePicker.mockRejectedValueOnce(new Error('Modal close failed'))
    mount()
    const first = renderPicker()
    await act(async () => { fireEvent.click(screen.getByRole('button', { name: `Log in with ${method}` })) })
    first.unmount()
    expect(sdk.login).not.toHaveBeenCalled()
    expect(actions.error).toBe('Could not start sign-in. Try again.')

    await expectDisconnected()
    const retry = renderPicker()
    await act(async () => { fireEvent.click(screen.getByRole('button', { name: `Log in with ${method}` })) })
    retry.unmount()
    expect(method === 'Google' ? sdk.initOAuth : sdk.loginWithPasskey).toHaveBeenCalledOnce()
  })

  it.each(['main', 'native'] as const)('keeps passkey failures inside the %s picker and allows an external-wallet fallback', async entry => {
    sessionStorage.setItem(selectionKey, 'disconnected')
    sdk.loginWithPasskey.mockRejectedValueOnce(new Error('Offline credential service'))
    const view = mount()
    const initial = renderPicker(entry)
    await act(async () => { fireEvent.click(screen.getByRole('button', { name: 'Log in with passkey' })) })
    expect(actions.error).toBeNull()
    expect(entry === 'main' ? sdk.closePicker : sdk.closeNativePicker).not.toHaveBeenCalled()
    expect(sdk.connect).toHaveBeenCalledTimes(entry === 'main' ? 1 : 0)
    expect(screen.getByRole('alert')).toHaveTextContent('Could not sign in with passkey.')
    expect(screen.getByRole('button', { name: 'Log in with Email' })).toBeEnabled()
    expect(screen.getByRole('button', { name: 'Log in with Google' })).toBeEnabled()
    await expectDisconnected()
    const external = testWallet(0)
    sdk.wallets = [external.wallet]
    view.refresh()
    await emit('connect', 'onSuccess', { wallet: external.wallet })
    initial.unmount()
    await expectConnected(external.account.address)
    expect(actions.error).toBeNull()
  })

  it('cancels a pending passkey when the picker is dismissed and ignores late authentication', async () => {
    sessionStorage.setItem(selectionKey, 'disconnected')
    let finish!: () => void
    sdk.loginWithPasskey.mockImplementationOnce(() => new Promise<void>(resolve => { finish = resolve }))
    const view = mount()
    const picker = renderPicker()
    await act(async () => { fireEvent.click(screen.getByRole('button', { name: 'Log in with passkey' })) })
    expect(screen.getByRole('button', { name: 'Log in with passkey' })).toBeDisabled()
    expect(sdk.closePicker).not.toHaveBeenCalled()
    await emit('login', 'onError', 'exited_auth_flow')
    await emit('connect', 'onError', 'generic_connect_wallet_error')
    picker.unmount()
    const embedded = testWallet(0, 'privy')
    sdk.wallets = [embedded.wallet]
    view.refresh()
    await emit('passkey', 'onComplete', { user: { linkedAccounts: [{ type: 'wallet', chainType: 'ethereum', walletClientType: 'privy', address: embedded.account.address }] }, loginMethod: 'passkey' })
    await emit('create', 'onSuccess', { wallet: embedded.wallet })
    await act(async () => { finish() })
    await expectDisconnected()
    expect(actions.error).toBeNull()
    expect(sdk.closePicker).not.toHaveBeenCalled()
  })

  it('accepts the native passkey alternative during email login but ignores already-authenticated callbacks', async () => {
    sessionStorage.setItem(selectionKey, 'disconnected')
    const embedded = testWallet(0, 'privy')
    const view = mount()
    const picker = renderPicker()
    await act(async () => { fireEvent.click(screen.getByRole('button', { name: 'Log in with Email' })) })
    picker.unmount()
    sdk.wallets = [embedded.wallet]
    view.refresh()
    const user = { linkedAccounts: [{ type: 'wallet', chainType: 'ethereum', walletClientType: 'privy', address: embedded.account.address }] }
    await emit('login', 'onComplete', { user, loginMethod: 'passkey', wasAlreadyAuthenticated: true })
    await expectDisconnected()
    await emit('login', 'onComplete', { user, loginMethod: 'passkey', wasAlreadyAuthenticated: false })
    await expectConnected(embedded.account.address)
  })

  it.each((['main', 'native'] as const).flatMap(entry => (['google', 'passkey'] as const).map(method => ({ entry, method }))))('starts $method directly from the $entry picker', async ({ entry, method }) => {
    sessionStorage.setItem(selectionKey, 'disconnected')
    mount()
    const picker = renderPicker(entry)
    await act(async () => { fireEvent.click(screen.getByRole('button', { name: method === 'google' ? 'Log in with Google' : 'Log in with passkey' })) })
    picker.unmount()
    expect(entry === 'main' ? sdk.closePicker : sdk.closeNativePicker).toHaveBeenCalledTimes(method === 'google' ? 1 : 0)
    if (method === 'google') expect(sdk.initOAuth).toHaveBeenCalledWith({ provider: 'google' })
    else expect(sdk.loginWithPasskey).toHaveBeenCalledOnce()
    expect(sdk.login).not.toHaveBeenCalled()
    await expectDisconnected()
  })

  it.each((['google', 'passkey'] as const).flatMap(method => (['failure', 'owner-change', 'session-change'] as const).map(outcome => ({ method, outcome }))))('handles $method wallet preparation $outcome without selecting a stale owner', async ({ method, outcome }) => {
    const embedded = testWallet(0, 'privy')
    const external = testWallet(1)
    sessionStorage.setItem(selectionKey, 'disconnected')
    let resolveCreation!: (wallet: ConnectedWallet) => void
    let rejectCreation!: (error: Error) => void
    sdk.createWallet.mockImplementationOnce(() => new Promise<ConnectedWallet>((resolve, reject) => {
      resolveCreation = resolve
      rejectCreation = reject
    }))
    const view = mount()
    const picker = renderPicker()
    await act(async () => { fireEvent.click(screen.getByRole('button', { name: method === 'google' ? 'Log in with Google' : 'Log in with passkey' })) })
    picker.unmount()
    const user = { id: 'google-user', linkedAccounts: [] }
    await emit(method === 'passkey' ? 'passkey' : 'login', 'onComplete', { loginMethod: method, user })
    expect(sdk.createWallet).not.toHaveBeenCalled()
    sdk.authenticated = true
    sdk.user = user
    view.refresh()
    await waitFor(() => { expect(sdk.createWallet).toHaveBeenCalledOnce() })
    if (outcome === 'owner-change') {
      renderPicker().unmount()
      sdk.wallets = [external.wallet]
      view.refresh()
      await emit('connect', 'onSuccess', { wallet: external.wallet })
      await expectConnected(external.account.address)
    } else if (outcome === 'session-change') {
      sdk.user = { id: 'different-user', linkedAccounts: [] }
      view.refresh()
    }
    sdk.wallets = [...sdk.wallets, embedded.wallet]
    view.refresh()
    await emit('create', 'onSuccess', { wallet: embedded.wallet })
    await act(async () => {
      if (outcome === 'failure') rejectCreation(new Error('Offline wallet service'))
      else resolveCreation(embedded.wallet)
    })
    if (outcome === 'owner-change') await expectConnected(external.account.address)
    else await expectDisconnected()
    if (outcome === 'failure') {
      expect(actions.error).toMatch(/wallet could not be prepared/)
      sdk.wallets = []
      view.refresh()
      sdk.createWallet.mockResolvedValueOnce(embedded.wallet)
      const retry = renderPicker()
      await act(async () => { fireEvent.click(screen.getByRole('button', { name: 'Use email wallet' })) })
      retry.unmount()
      sdk.wallets = [embedded.wallet]
      view.refresh()
      await expectConnected(embedded.account.address)
      expect(method === 'google' ? sdk.initOAuth : sdk.loginWithPasskey).toHaveBeenCalledOnce()
    } else expect(actions.error).toBeNull()
  })

  it.each(['google', 'passkey'] as const)('waits for verified %s login before selecting a newly created embedded wallet', async method => {
    const embedded = testWallet(0, 'privy')
    sessionStorage.setItem(selectionKey, 'disconnected')
    const view = mount()
    const picker = renderPicker()
    await act(async () => { fireEvent.click(screen.getByRole('button', { name: method === 'google' ? 'Log in with Google' : 'Log in with passkey' })) })
    picker.unmount()
    sdk.wallets = [embedded.wallet]
    view.refresh()
    await emit('create', 'onSuccess', { wallet: embedded.wallet })
    await expectDisconnected()
    const user = { id: 'verified-google-user', linkedAccounts: [] }
    sdk.createWallet.mockResolvedValueOnce(embedded.wallet)
    sdk.authenticated = true
    sdk.user = user
    await emit(method === 'passkey' ? 'passkey' : 'login', 'onComplete', { loginMethod: method, user })
    view.refresh()
    await emit('create', 'onSuccess', { wallet: embedded.wallet })
    await expectConnected(embedded.account.address)
    expect(sdk.createWallet).toHaveBeenCalledOnce()
  })

  it('does not start linking after the authenticated session changes while closing the picker', async () => {
    sdk.authenticated = true
    sdk.user = { id: 'original-user', linkedAccounts: [] }
    sessionStorage.setItem(selectionKey, 'disconnected')
    let close: () => void = () => { throw new Error('Close not requested') }
    sdk.closePicker.mockImplementationOnce(() => new Promise<void>(resolve => { close = resolve }))
    const view = mount()
    const picker = renderPicker()
    await act(async () => { fireEvent.click(screen.getByRole('button', { name: 'Link Google account' })) })
    sdk.user = { id: 'different-user', linkedAccounts: [] }
    view.refresh()
    await act(async () => { close() })
    picker.unmount()
    expect(sdk.linkGoogle).not.toHaveBeenCalled()
    await expectDisconnected()
  })

  it('marks an already-linked Google account and still allows adding another passkey', () => {
    sdk.authenticated = true
    sdk.user = { id: 'original-user', linkedAccounts: [{ type: 'google_oauth' }, { type: 'passkey' }] }
    mount()
    renderPicker()
    expect(screen.getByRole('button', { name: 'Google linked' })).toBeDisabled()
    expect(screen.getByRole('button', { name: 'Add passkey' })).toBeEnabled()
  })

  it.each(['google', 'passkey'] as const)('logs in with %s to the existing embedded wallet through the real signing bridge', async method => {
    const embedded = testWallet(0, 'privy')
    const external = testWallet(1)
    sdk.wallets = [external.wallet]
    sessionStorage.setItem(selectionKey, external.account.address)
    const view = mount()
    await expectConnected(external.account.address)
    const cached = await getWalletClient(config)
    const popup = renderPicker()
    await act(async () => { fireEvent.click(screen.getByRole('button', { name: method === 'google' ? 'Log in with Google' : 'Log in with passkey' })) })
    popup.unmount()
    if (method === 'google') {
      expect(sdk.initOAuth).toHaveBeenCalledWith({ provider: 'google' })
      expect(sdk.login).not.toHaveBeenCalled()
    } else expect(sdk.loginWithPasskey).toHaveBeenCalledOnce()
    if (method === 'google') expect(sdk.closePicker.mock.invocationCallOrder[0]).toBeLessThan(sdk.initOAuth.mock.invocationCallOrder[0])
    else expect(sdk.closePicker).not.toHaveBeenCalled()
    sdk.wallets = [external.wallet, embedded.wallet]
    view.refresh()
    const user = { id: 'same-privy-user', linkedAccounts: [{ type: 'wallet', chainType: 'ethereum', walletClientType: 'privy', address: embedded.account.address }] }
    await emit('login', 'onComplete', { user, loginMethod: 'siwe' })
    await expectConnected(external.account.address)
    await emit(method === 'passkey' ? 'passkey' : 'login', 'onComplete', { user, loginMethod: method })
    await expectConnected(embedded.account.address)
    expect(sdk.closePicker).toHaveBeenCalledOnce()
    await expect(cached.signMessage({ message: 'Old owner' })).rejects.toThrow(/selected wallet changed|not ready/)
    const client = await getWalletClient(config)
    const message = `Existing embedded wallet after ${method} login`
    expect(await verifyMessage({ address: embedded.account.address, message, signature: await client.signMessage({ message }) })).toBe(true)
    expect(sdk.createWallet).not.toHaveBeenCalled()
  })

  it.each(['google', 'passkey'] as const)('ignores canceled %s login and late wallet creation', async method => {
    sessionStorage.setItem(selectionKey, 'disconnected')
    const embedded = testWallet(0, 'privy')
    const view = mount()
    if (method === 'passkey') sdk.loginWithPasskey.mockRejectedValueOnce({ privyErrorCode: 'passkey_not_allowed' })
    const popup = renderPicker()
    await act(async () => { fireEvent.click(screen.getByRole('button', { name: method === 'google' ? 'Log in with Google' : 'Log in with passkey' })) })
    popup.unmount()
    if (method === 'google') await emit('login', 'onError', 'exited_auth_flow')
    sdk.wallets = [embedded.wallet]
    view.refresh()
    await emit(method === 'passkey' ? 'passkey' : 'login', 'onComplete', { loginMethod: method, user: { linkedAccounts: [{ type: 'wallet', chainType: 'ethereum', walletClientType: 'privy', address: embedded.account.address }] } })
    await emit('create', 'onSuccess', { wallet: embedded.wallet })
    await expectDisconnected()
    expect(actions.error).toBeNull()
    expect(sessionStorage.getItem(`${selectionKey}.google-login`)).toBeNull()
  })

  it.each(['main', 'native'] as const)('links Google and passkeys from the %s picker without changing the current owner or its signer', async entry => {
    const external = testWallet(0)
    const embedded = testWallet(1, 'privy')
    sdk.authenticated = true
    sdk.user = { id: 'original-user', linkedAccounts: [] }
    sdk.wallets = [external.wallet, embedded.wallet]
    sessionStorage.setItem(selectionKey, external.account.address)
    mount()
    await expectConnected(external.account.address)
    const client = await getWalletClient(config)
    for (const [label, method, spy] of [['Link Google account', 'google', sdk.linkGoogle], ['Add passkey', 'passkey', sdk.linkPasskey]] as const) {
      const popup = renderPicker(entry)
      await act(async () => { fireEvent.click(screen.getByRole('button', { name: label })) })
      popup.unmount()
      expect(spy).toHaveBeenCalledOnce()
      await emit('link', 'onSuccess', { user: sdk.user, linkMethod: method })
      // SDK login/creation events caused by linking cannot change trading owner.
      await emit('login', 'onComplete', { user: { linkedAccounts: [{ type: 'wallet', chainType: 'ethereum', walletClientType: 'privy', address: embedded.account.address }] }, loginMethod: method })
      await emit('create', 'onSuccess', { wallet: embedded.wallet })
      await expectConnected(external.account.address)
      const message = `Original owner after linking ${method}`
      expect(await verifyMessage({ address: external.account.address, message, signature: await client.signMessage({ message }) })).toBe(true)
    }
    expect(sdk.login).not.toHaveBeenCalled()
    expect(sdk.createWallet).not.toHaveBeenCalled()
  })

  it('keeps the owner on linking conflict and dismisses canceled linking silently', async () => {
    const external = testWallet(0)
    sdk.wallets = [external.wallet]
    sdk.authenticated = true
    sdk.user = { id: 'original-user', linkedAccounts: [] }
    mount()
    await expectConnected(external.account.address)
    const popup = renderPicker()
    await act(async () => { fireEvent.click(screen.getByRole('button', { name: 'Link Google account' })) })
    popup.unmount()
    await emit('link', 'onError', 'linked_to_another_user', { linkMethod: 'google' })
    expect(actions.error).toMatch(/already belong to another account/)
    await expectConnected(external.account.address)
    await emit('link', 'onError', 'exited_link_flow', { linkMethod: 'passkey' })
    expect(actions.error).toBeNull()
    await expectConnected(external.account.address)
  })

  it('resumes an explicit Google redirect after remount and consumes its saved intent', async () => {
    sessionStorage.setItem(selectionKey, 'disconnected')
    const first = mount()
    const popup = renderPicker()
    await act(async () => { fireEvent.click(screen.getByRole('button', { name: 'Log in with Google' })) })
    popup.unmount()
    expect(sessionStorage.getItem(`${selectionKey}.google-login`)).not.toBeNull()
    first.unmount()
    window.history.replaceState({}, '', '/?privy_oauth_provider=google&privy_oauth_code=test-code&privy_oauth_state=test-state')
    const embedded = testWallet(0, 'privy')
    sdk.wallets = [embedded.wallet]
    sdk.authenticated = true
    mount()
    await expectDisconnected()
    await emit('login', 'onComplete', { loginMethod: 'google', user: { linkedAccounts: [{ type: 'wallet', chainType: 'ethereum', walletClientType: 'privy', address: embedded.account.address }] } })
    await expectConnected(embedded.account.address)
    expect(sessionStorage.getItem(`${selectionKey}.google-login`)).toBeNull()
  })

  it.each(['main', 'native'] as const)('offers Log in with Email in the signed-out %s picker and waits for verified login before switching', async entry => {
    const email = testWallet(0, 'privy')
    const external = testWallet(1)
    sdk.wallets = entry === 'native' ? [external.wallet] : []
    sessionStorage.setItem(selectionKey, entry === 'native' ? external.account.address : 'disconnected')
    const view = mount()
    if (entry === 'native') await expectConnected(external.account.address)
    else await expectDisconnected()
    const popup = renderPicker(entry)
    expect(screen.queryByRole('button', { name: 'Use email wallet' })).not.toBeInTheDocument()
    await act(async () => { fireEvent.click(screen.getByRole('button', { name: 'Log in with Email', exact: true })) })
    popup.unmount()
    const close = entry === 'main' ? sdk.closePicker : sdk.closeNativePicker
    expect(close).toHaveBeenCalledOnce()
    expect(sdk.login).toHaveBeenCalledWith({ loginMethods: ['email'] })
    expect(close.mock.invocationCallOrder[0]).toBeLessThan(sdk.login.mock.invocationCallOrder[0])
    sdk.wallets = entry === 'native' ? [external.wallet, email.wallet] : [email.wallet]
    view.refresh()
    if (entry === 'native') await expectConnected(external.account.address)
    else await expectDisconnected()
    sdk.authenticated = true
    sdk.user = { linkedAccounts: [{ type: 'wallet', address: email.account.address, walletClientType: 'privy', chainType: 'ethereum' }] }
    await emit('login', 'onComplete', { user: sdk.user, loginMethod: 'email', loginAccount: { type: 'email' } })
    view.refresh()
    await expectConnected(email.account.address)
    const client = await getWalletClient(config)
    const message = 'Verified email owner from the connection popup'
    const signature = await client.signMessage({ message })
    expect(await verifyMessage({ address: email.account.address, message, signature })).toBe(true)
  })

  it.each([
    ['main', 'external-owner'], ['main', 'disconnected'], ['main', 'email-owner'],
    ['native', 'external-owner'], ['native', 'disconnected'], ['native', 'email-owner'],
  ] as const)('offers Use email wallet in an authenticated %s picker (%s) and activates its guarded signer', async (entry, state) => {
    const email = testWallet(0, 'privy')
    const external = testWallet(1)
    sdk.authenticated = true
    sdk.user = { linkedAccounts: [{ type: 'wallet', address: email.account.address, walletClientType: 'privy', chainType: 'ethereum' }] }
    sdk.wallets = [email.wallet, external.wallet]
    sessionStorage.setItem(selectionKey, state === 'external-owner' ? external.account.address
      : state === 'email-owner' ? email.account.address : 'disconnected')
    mount()
    let previousClient: Awaited<ReturnType<typeof getWalletClient>> | undefined
    if (state === 'external-owner') {
      await expectConnected(external.account.address)
      previousClient = await getWalletClient(config)
    } else if (state === 'email-owner') await expectConnected(email.account.address)
    else await expectDisconnected()
    const popup = renderPicker(entry)
    expect(screen.queryByRole('button', { name: 'Log in with Email' })).not.toBeInTheDocument()
    await act(async () => { fireEvent.click(screen.getByRole('button', { name: 'Use email wallet', exact: true })) })
    popup.unmount()
    await expectConnected(email.account.address)
    expect(sdk.login).not.toHaveBeenCalled()
    expect(entry === 'main' ? sdk.closePicker : sdk.closeNativePicker).toHaveBeenCalledOnce()
    expect(sessionStorage.getItem(selectionKey)).toBe(email.account.address.toLowerCase())
    if (previousClient) await expect(previousClient.signMessage({ message: 'Old external signer must not sign' })).rejects.toThrow(/selected wallet changed|not ready/)
    const client = await getWalletClient(config)
    const message = 'Email wallet explicitly selected in the connection popup'
    const signature = await client.signMessage({ message })
    expect(await verifyMessage({ address: email.account.address, message, signature })).toBe(true)
  })

  it.each(['main', 'native'] as const)('does not select the authenticated email wallet merely by opening or dismissing the %s picker', async entry => {
    const email = testWallet(0, 'privy')
    const external = testWallet(1)
    sdk.authenticated = true
    sdk.wallets = [email.wallet, external.wallet]
    sessionStorage.setItem(selectionKey, external.account.address.toLowerCase())
    mount()
    await expectConnected(external.account.address)
    const popup = renderPicker(entry)
    expect(screen.getByRole('button', { name: 'Use email wallet', exact: true })).toBeInTheDocument()
    await expectConnected(external.account.address)
    await emit('connect', 'onError', 'generic_connect_wallet_error')
    popup.unmount()
    await expectConnected(external.account.address)
    expect(sessionStorage.getItem(selectionKey)).toBe(external.account.address.toLowerCase())
  })

  it.each(['main', 'native'] as const)('prepares a missing embedded wallet from the authenticated %s picker without logging in again', async entry => {
    const external = testWallet(1)
    const email = testWallet(0, 'privy')
    sdk.authenticated = true
    sdk.user = { linkedAccounts: [] }
    sdk.wallets = [external.wallet]
    let resolveCreation: (wallet: ConnectedWallet) => void = () => { throw new Error('Wallet creation not started') }
    sdk.createWallet.mockImplementationOnce(() => new Promise(resolve => { resolveCreation = resolve }))
    sessionStorage.setItem(selectionKey, external.account.address)
    const view = mount()
    await expectConnected(external.account.address)
    const popup = renderPicker(entry)
    await act(async () => { fireEvent.click(screen.getByRole('button', { name: 'Use email wallet', exact: true })) })
    popup.unmount()
    expect(entry === 'main' ? sdk.closePicker : sdk.closeNativePicker).toHaveBeenCalledOnce()
    expect(sdk.login).not.toHaveBeenCalled()
    expect(sdk.createWallet).toHaveBeenCalledOnce()
    await expectConnected(external.account.address)
    sdk.wallets = [external.wallet, email.wallet]
    view.refresh()
    await act(async () => { resolveCreation(email.wallet) })
    await expectConnected(email.account.address)
  })

  it('connects after approval and signs recoverable personal and typed messages through Wagmi', async () => {
    const test = testWallet(0)
    const view = mount()
    await expectDisconnected()
    act(() => { actions.connect() })
    await emit('connect', 'onSuccess', { wallet: test.wallet })
    sdk.wallets = [test.wallet]
    view.refresh()
    await expectConnected(test.account.address)
    const client = await getWalletClient(config)
    const message = 'Plether owner integration test'
    const signature = await client.signMessage({ message })
    expect(await verifyMessage({ address: test.account.address, message, signature })).toBe(true)
    const typed = {
      domain: { name: 'Plether integration', version: '1', chainId: 421614 },
      types: { Owner: [{ name: 'owner', type: 'address' }] },
      primaryType: 'Owner', message: { owner: test.account.address },
    } as const
    const typedSignature = await client.signTypedData(typed)
    expect(await verifyTypedData({ ...typed, address: test.account.address, signature: typedSignature })).toBe(true)
  })

  it.each(['before', 'after'])('replaces an unavailable saved owner when approval arrives %s the SDK wallet list', async order => {
    const old = testWallet(0)
    const next = testWallet(1)
    sessionStorage.setItem(selectionKey, old.account.address)
    const view = mount()
    await expectDisconnected()
    act(() => { actions.connect() })
    if (order === 'after') { sdk.wallets = [next.wallet]; view.refresh() }
    await emit('connect', 'onSuccess', { wallet: next.wallet })
    sdk.wallets = [next.wallet]
    view.refresh()
    await expectConnected(next.account.address)
    expect(sessionStorage.getItem(selectionKey)).toBe(next.account.address.toLowerCase())
  })

  it('disconnects synchronously, revokes a cached signer, stays disconnected after remount, and can reconnect', async () => {
    const test = testWallet(0)
    sdk.wallets = [test.wallet]
    const view = mount()
    await expectConnected(test.account.address)
    const cachedClient = await getWalletClient(config)
    await act(async () => { await actions.disconnect() })
    await expectDisconnected()
    await expect(cachedClient.signMessage({ message: 'Must not sign after disconnect' })).rejects.toThrow(/selected wallet changed|not ready/)
    view.unmount()
    const nextView = mount()
    await expectDisconnected()
    act(() => { actions.connect() })
    await emit('connect', 'onSuccess', { wallet: test.wallet })
    nextView.refresh()
    await expectConnected(test.account.address)
  })

  it('preserves an active owner when another wallet connects and switches only on explicit selection', async () => {
    const first = testWallet(0)
    const next = testWallet(1)
    sdk.wallets = [first.wallet]
    const view = mount()
    await expectConnected(first.account.address)
    const cachedClient = await getWalletClient(config)
    act(() => { actions.connect() })
    sdk.wallets = [first.wallet, next.wallet]
    view.refresh()
    await emit('connect', 'onSuccess', { wallet: next.wallet })
    await expectConnected(first.account.address)
    act(() => { actions.selectWallet(next.account.address) })
    await expectConnected(next.account.address)
    await expect(cachedClient.signMessage({ message: 'Must not sign using old owner' })).rejects.toThrow(/selected wallet changed|not ready/)
    const client = await getWalletClient(config)
    const message = 'New owner selected'
    const signature = await client.signMessage({ message })
    expect(await verifyMessage({ address: next.account.address, message, signature })).toBe(true)
  })

  it('recovers the selected owner when an earlier connector finishes initializing after an owner change', async () => {
    const first = testWallet(0)
    const next = testWallet(1)
    let resolveProvider: (provider: typeof first.provider) => void = () => { throw new Error('Provider was not requested') }
    vi.mocked(first.wallet.getEthereumProvider).mockImplementationOnce(() => new Promise(resolve => { resolveProvider = resolve }))
    sessionStorage.setItem(selectionKey, first.account.address)
    sdk.wallets = [first.wallet, next.wallet]
    mount()
    await waitFor(() => { expect(first.wallet.getEthereumProvider).toHaveBeenCalled() })
    act(() => { actions.selectWallet(next.account.address) })
    await expectConnected(next.account.address)
    await act(async () => { resolveProvider(first.provider) })
    await expectConnected(next.account.address)
    const client = await getWalletClient(config)
    const message = 'Owner survives late connector initialization'
    const signature = await client.signMessage({ message })
    expect(await verifyMessage({ address: next.account.address, message, signature })).toBe(true)
    expect(first.request).not.toHaveBeenCalledWith(expect.objectContaining({ method: 'personal_sign' }))
  })

  it('waits for an explicit choice with multiple restored wallets and never falls back when the selected owner disappears', async () => {
    const first = testWallet(0)
    const next = testWallet(1)
    sdk.wallets = [first.wallet, next.wallet]
    const view = mount()
    await expectDisconnected()
    act(() => { actions.selectWallet(first.account.address) })
    await expectConnected(first.account.address)
    sdk.wallets = [next.wallet]
    view.refresh()
    await expectDisconnected()
    act(() => { actions.connect() })
    await emit('connect', 'onSuccess', { wallet: next.wallet })
    await expectConnected(next.account.address)
  })

  it('does not restore an unavailable saved owner if it reappears after canceled email login', async () => {
    const test = testWallet(0)
    sessionStorage.setItem(selectionKey, test.account.address)
    const view = mount()
    await expectDisconnected()
    await beginEmail()
    await emit('login', 'onError', 'exited_auth_flow')
    sdk.wallets = [test.wallet]
    view.refresh()
    await expectDisconnected()
    expect(sessionStorage.getItem(selectionKey)).toBe('disconnected')
  })

  it('keeps canceled external and email login disconnected despite late SDK success and wallet creation', async () => {
    sessionStorage.setItem(selectionKey, 'disconnected')
    const test = testWallet(0, 'privy')
    const view = mount()
    act(() => { actions.connect() })
    await emit('connect', 'onError', 'generic_connect_wallet_error')
    await emit('connect', 'onSuccess', { wallet: test.wallet })
    await beginEmail()
    await emit('login', 'onError', 'exited_auth_flow')
    sdk.wallets = [test.wallet]
    view.refresh()
    await emit('connect', 'onSuccess', { wallet: test.wallet })
    await emit('login', 'onComplete', { user: { linkedAccounts: [] }, loginMethod: 'email' })
    await emit('create', 'onSuccess', { wallet: test.wallet })
    await expectDisconnected()
    expect(actions.error).toBeNull()
    expect(test.request).not.toHaveBeenCalledWith(expect.objectContaining({ method: 'personal_sign' }))
  })

  it.each(['linked', 'created-later'])('connects a verified email wallet (%s) and logs out without reconnecting its cached session', async timing => {
    sessionStorage.setItem(selectionKey, 'disconnected')
    const test = testWallet(0, 'privy')
    const view = mount()
    await beginEmail()
    const linkedAccounts = timing === 'linked' ? [{ type: 'wallet', address: test.account.address, walletClientType: 'privy', chainType: 'ethereum' }] : []
    sdk.authenticated = true
    sdk.user = { linkedAccounts }
    await emit('login', 'onComplete', { user: sdk.user, loginMethod: 'email', loginAccount: { type: 'email' } })
    sdk.wallets = [test.wallet]
    view.refresh()
    if (timing === 'created-later') await emit('create', 'onSuccess', { wallet: test.wallet })
    await expectConnected(test.account.address)
    await act(async () => { await actions.disconnect() })
    view.refresh()
    await expectDisconnected()
    expect(sdk.logout).toHaveBeenCalledOnce()
  })

  it('does not reactivate a wallet when its native account dialog finishes after disconnect', async () => {
    const test = testWallet(0)
    sdk.wallets = [test.wallet]
    sdk.nativeWallet = test.wallet
    let closeDialog: (result: { wallet: ConnectedWallet }) => void = () => { throw new Error('Dialog not open') }
    sdk.openNative.mockImplementation(() => new Promise(resolve => { closeDialog = resolve }))
    mount()
    await expectConnected(test.account.address)
    let pending: Promise<void> | undefined
    act(() => { pending = actions.openAccount() })
    await act(async () => { await actions.disconnect() })
    await act(async () => { closeDialog({ wallet: test.wallet }); await pending })
    await expectDisconnected()
    expect(sessionStorage.getItem(selectionKey)).toBe('disconnected')
  })

  it('synchronizes a native wallet choice while the dialog is open and preserves it on close', async () => {
    const first = testWallet(0)
    const next = testWallet(1)
    sdk.wallets = [first.wallet, next.wallet]
    sdk.nativeWallet = first.wallet
    sessionStorage.setItem(selectionKey, first.account.address)
    let closeDialog: (result: { wallet: ConnectedWallet }) => void = () => { throw new Error('Dialog not open') }
    sdk.openNative.mockImplementation(() => new Promise(resolve => { closeDialog = resolve }))
    const view = mount()
    await expectConnected(first.account.address)
    let pending: Promise<void> | undefined
    act(() => { pending = actions.openAccount() })
    sdk.nativeWallet = next.wallet
    view.refresh()
    await expectConnected(next.account.address)
    await act(async () => { closeDialog({ wallet: next.wallet }); await pending })
    await expectConnected(next.account.address)
    expect(actions.modalOpen).toBe(false)
  })

  it('ignores a native dialog result after another explicit owner selection', async () => {
    const first = testWallet(0)
    const next = testWallet(1)
    sdk.wallets = [first.wallet, next.wallet]
    sdk.nativeWallet = first.wallet
    sessionStorage.setItem(selectionKey, first.account.address)
    let closeDialog: (result: { wallet: ConnectedWallet }) => void = () => { throw new Error('Dialog not open') }
    sdk.openNative.mockImplementation(() => new Promise(resolve => { closeDialog = resolve }))
    mount()
    await expectConnected(first.account.address)
    let pending: Promise<void> | undefined
    act(() => { pending = actions.openAccount() })
    act(() => { actions.selectWallet(next.account.address) })
    await expectConnected(next.account.address)
    await act(async () => { closeDialog({ wallet: first.wallet }); await pending })
    await expectConnected(next.account.address)
  })

  it('returns from an external owner through the connection picker without a header shortcut', async () => {
    const email = testWallet(0, 'privy')
    const external = testWallet(1)
    sessionStorage.setItem(selectionKey, 'disconnected')
    const view = mount(true)
    await beginEmail()
    sdk.authenticated = true
    sdk.user = { linkedAccounts: [{ type: 'wallet', address: email.account.address, walletClientType: 'privy', chainType: 'ethereum' }] }
    await emit('login', 'onComplete', { user: sdk.user, loginMethod: 'email', loginAccount: { type: 'email' } })
    sdk.wallets = [email.wallet]
    view.refresh()
    await expectConnected(email.account.address)
    expect(screen.queryByRole('button', { name: 'Use email wallet' })).not.toBeInTheDocument()

    act(() => { actions.connect() })
    sdk.wallets = [email.wallet, external.wallet]
    view.refresh()
    await emit('connect', 'onSuccess', { wallet: external.wallet })
    await expectConnected(email.account.address)
    act(() => { actions.selectWallet(external.account.address) })
    await expectConnected(external.account.address)
    const externalClient = await getWalletClient(config)
    const externalMessage = 'External owner can sign before selecting email'
    const externalSignature = await externalClient.signMessage({ message: externalMessage })
    expect(await verifyMessage({ address: external.account.address, message: externalMessage, signature: externalSignature })).toBe(true)

    expect(screen.queryByRole('button', { name: 'Use email wallet' })).not.toBeInTheDocument()
    const picker = renderPicker('native')
    await act(async () => { fireEvent.click(screen.getByRole('button', { name: 'Use email wallet' })) })
    picker.unmount()
    await expectConnected(email.account.address)
    await expect(externalClient.signMessage({ message: 'External signer must be revoked' })).rejects.toThrow(/selected wallet changed|not ready/)
    const emailClient = await getWalletClient(config)
    const emailMessage = 'Email owner selected through connection picker'
    const emailSignature = await emailClient.signMessage({ message: emailMessage })
    expect(await verifyMessage({ address: email.account.address, message: emailMessage, signature: emailSignature })).toBe(true)
    expect(screen.queryByRole('button', { name: 'Use email wallet' })).not.toBeInTheDocument()
  })

  it('switches the actual connected provider to Arbitrum Sepolia through the header network control', async () => {
    const test = testWallet(0, 'metamask', 1)
    sdk.wallets = [test.wallet]
    mount(true)
    await expectConnected(test.account.address)
    expect(config.state.chainId).toBe(1)
    fireEvent.click(screen.getByRole('button', { name: 'Switch wallet network to Arbitrum Sepolia' }))
    await waitFor(() => { expect(config.state.chainId).toBe(421614) })
    expect(test.request).toHaveBeenCalledWith({ method: 'wallet_switchEthereumChain', params: [{ chainId: '0x66eee' }] })
    await waitFor(() => { expect(screen.queryByRole('button', { name: 'Switch wallet network to Arbitrum Sepolia' })).not.toBeInTheDocument() })
    const client = await getWalletClient(config)
    expect(client.chain.id).toBe(421614)
    const message = 'Connected owner after switching network'
    const signature = await client.signMessage({ message })
    expect(await verifyMessage({ address: test.account.address, message, signature })).toBe(true)
  })
})
