import { useEffect, type ReactNode } from 'react'
import { act, fireEvent, render, screen, waitFor } from '@testing-library/react'
import { QueryClient } from '@tanstack/react-query'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import type { WalletActions } from './useWalletActions'

interface MockWallet {
  address: string
  walletClientType: string
  meta: { name: string }
  disconnect: () => void
}

const mocks = vi.hoisted(() => {
  vi.stubEnv('VITE_PRIVY_APP_ID', 'test-privy-app')
  return {
    ready: true,
    walletsReady: true,
    authenticated: false,
    modalOpen: false,
    activeAddress: undefined as string | undefined,
    wallets: [] as MockWallet[],
    connect: vi.fn(),
    connectExternal: vi.fn(),
    openNative: vi.fn(),
    closePicker: vi.fn(),
    setNative: vi.fn(),
    nativeWallet: undefined as (MockWallet & { type: 'ethereum' }) | undefined,
    logout: vi.fn(),
    exportWallet: vi.fn(),
    setRecovery: vi.fn(),
    callbacks: undefined as undefined | { onSuccess: (args: { wallet: { address: string } }) => void; onError: (code: string) => void },
    emailCallbacks: undefined as undefined | {
      onComplete: (input: { user: { linkedAccounts: { type: string; chainType: string; walletClientType: string; address: string }[] }; loginMethod: string | null }) => void
      onError: (code: string) => void
    },
    onWalletCreated: undefined as undefined | ((input: { wallet: { address: string; chainType: string } }) => void),
    selector: undefined as undefined | ((args: { wallets: MockWallet[] }) => { address: string } | undefined),
  }
})

vi.mock('@privy-io/react-auth', () => ({
  PrivyProvider: ({ children }: { children: ReactNode }) => children,
  useActiveWallet: () => ({ wallet: mocks.nativeWallet, connect: mocks.openNative, setActiveWallet: mocks.setNative }),
  usePrivy: () => ({ ready: mocks.ready, authenticated: mocks.authenticated, logout: mocks.logout }),
  useModalStatus: () => ({ isOpen: mocks.modalOpen }),
  useWallets: () => ({ ready: mocks.walletsReady, wallets: mocks.wallets }),
  useLinkAccount: () => ({ linkGoogle: vi.fn(), linkPasskey: vi.fn() }),
  useLoginWithOAuth: () => ({ initOAuth: vi.fn() }),
  useLoginWithPasskey: () => ({ loginWithPasskey: vi.fn() }),
  useLogin: (callbacks: typeof mocks.emailCallbacks) => {
    mocks.emailCallbacks = callbacks
    return { login: mocks.connect }
  },
  useConnectWallet: (callbacks: typeof mocks.callbacks) => {
    mocks.callbacks = callbacks
    return { connectWallet: mocks.connectExternal }
  },
  useCreateWallet: ({ onSuccess }: { onSuccess: typeof mocks.onWalletCreated }) => {
    mocks.onWalletCreated = onSuccess
    return { createWallet: vi.fn() }
  },
  useExportWallet: () => ({ exportWallet: mocks.exportWallet }),
  useSetWalletRecovery: () => ({ setWalletRecovery: mocks.setRecovery }),
}))
vi.mock('@privy-io/react-auth/ui', () => ({
  WalletsDialog: () => <div data-testid="native-wallet-dialog" />,
  usePletherWalletPickerClose: () => mocks.closePicker,
}))
vi.mock('@privy-io/wagmi', () => ({
  WagmiProvider: ({ children, setActiveWalletForWagmi }: { children: ReactNode; setActiveWalletForWagmi: typeof mocks.selector }) => {
    mocks.selector = setActiveWalletForWagmi
    return children
  },
}))
vi.mock('../config/wagmi', () => ({
  config: { setState: vi.fn() },
  walletChains: [{ id: 1 }, { id: 11155111 }, { id: 421614 }],
}))
vi.mock('../utils/deployment', () => ({ isSepoliaDeployment: () => true }))
vi.mock('wagmi', () => ({
  useAccount: () => ({ address: mocks.activeAddress, isConnecting: false, isReconnecting: false }),
  WagmiProvider: ({ children }: { children: ReactNode }) => children,
}))

import { WalletProvider } from './WalletProvider'
import { useWalletActions } from './useWalletActions'

let actions: WalletActions
function Probe() {
  const value = useWalletActions()
  useEffect(() => { actions = value }, [value])
  return <span data-testid="ready">{String(value.ready)}</span>
}

function tree() {
  return <WalletProvider queryClient={new QueryClient()}><Probe /></WalletProvider>
}

const ownerAddress = '0x1111111111111111111111111111111111111111'
const otherAddress = '0x2222222222222222222222222222222222222222'

describe('Privy wallet actions', () => {
  beforeEach(() => {
    vi.clearAllMocks()
    sessionStorage.clear()
  localStorage.clear()
    mocks.ready = true
    mocks.walletsReady = true
    mocks.authenticated = false
    mocks.modalOpen = false
    mocks.activeAddress = undefined
    mocks.wallets = []
    mocks.nativeWallet = undefined
    mocks.openNative.mockResolvedValue({})
    mocks.closePicker.mockResolvedValue(undefined)
    mocks.logout.mockResolvedValue(undefined)
    mocks.exportWallet.mockResolvedValue(undefined)
    mocks.setRecovery.mockResolvedValue(undefined)
  })

  it('connects an external wallet without requiring authenticated state', () => {
    render(tree())
    expect(actions.authenticated).toBe(false)
    expect(actions.ready).toBe(true)
    act(() => { actions.connect() })
    expect(mocks.connectExternal).toHaveBeenCalledOnce()
  })

  it('offers email above the wallet catalog and opens email login only on selection', async () => {
    render(tree())
    act(() => { actions.connect() })
    const options = mocks.connectExternal.mock.lastCall?.[0] as { description: ReactNode }
    render(<>{options.description}</>)
    expect(mocks.connect).not.toHaveBeenCalled()
    fireEvent.click(screen.getByRole('button', { name: 'Log in with Email' }))
    await waitFor(() => { expect(mocks.connect).toHaveBeenCalledOnce() })
    expect(mocks.connect).toHaveBeenCalledWith({ loginMethods: ['email'] })
  })

  it('exposes whether Privy authentication is still open', () => {
    const view = render(tree())
    expect(actions.modalOpen).toBe(false)
    mocks.modalOpen = true
    view.rerender(tree())
    expect(actions.modalOpen).toBe(true)
    mocks.modalOpen = false
    view.rerender(tree())
    expect(actions.modalOpen).toBe(false)
  })

  it.each(['external', 'email'])('replaces an unavailable saved owner after a successful %s connection', flow => {
    sessionStorage.setItem('plether.privy.test-privy-app.owner', otherAddress)
    const view = render(tree())
    act(() => { actions.connect() })
    if (flow === 'email') {
      const options = mocks.connectExternal.mock.lastCall?.[0] as { description: ReactNode }
      render(<>{options.description}</>)
      fireEvent.click(screen.getByRole('button', { name: 'Log in with Email' }))
      act(() => { mocks.emailCallbacks?.onComplete({ user: { linkedAccounts: [
        { type: 'wallet', chainType: 'ethereum', walletClientType: 'privy', address: ownerAddress },
      ] }, loginMethod: 'email' }) })
    } else {
      act(() => { mocks.callbacks?.onSuccess({ wallet: { address: ownerAddress } }) })
    }
    expect(sessionStorage.getItem('plether.privy.test-privy-app.owner')).toBe(ownerAddress)
    expect(mocks.selector?.({ wallets: [] })).toBeUndefined()
    expect(actions.signerReady).toBe(false)
    mocks.wallets = [{ address: ownerAddress, walletClientType: flow === 'email' ? 'privy' : 'metamask', meta: { name: 'Wallet' }, disconnect: vi.fn() }]
    mocks.activeAddress = ownerAddress
    view.rerender(tree())
    expect(mocks.selector?.({ wallets: mocks.wallets })?.address).toBe(ownerAddress)
    expect(actions.signerReady).toBe(true)
  })

  it('keeps an unavailable saved owner until an explicit connection succeeds', () => {
    sessionStorage.setItem('plether.privy.test-privy-app.owner', otherAddress)
    mocks.wallets = [{ address: ownerAddress, walletClientType: 'metamask', meta: { name: 'MetaMask' }, disconnect: vi.fn() }]
    render(tree())
    expect(mocks.selector?.({ wallets: mocks.wallets })).toBeUndefined()
    act(() => {
      actions.connect()
      mocks.callbacks?.onError('generic_connect_wallet_error')
      mocks.callbacks?.onSuccess({ wallet: { address: ownerAddress } })
    })
    expect(mocks.selector?.({ wallets: mocks.wallets })).toBeUndefined()
    expect(sessionStorage.getItem('plether.privy.test-privy-app.owner')).toBe(otherAddress)
  })

  it('preserves an available selected owner after explicitly connecting an additional wallet', () => {
    const owner = { address: ownerAddress, walletClientType: 'metamask', meta: { name: 'MetaMask' }, disconnect: vi.fn() }
    mocks.wallets = [owner]
    sessionStorage.setItem('plether.privy.test-privy-app.owner', ownerAddress)
    const view = render(tree())
    act(() => { actions.connect() })
    mocks.wallets = [owner, { ...owner, address: otherAddress }]
    view.rerender(tree())
    act(() => { mocks.callbacks?.onSuccess({ wallet: { address: otherAddress } }) })
    expect(mocks.selector?.({ wallets: mocks.wallets })?.address).toBe(ownerAddress)
    expect(sessionStorage.getItem('plether.privy.test-privy-app.owner')).toBe(ownerAddress)
  })

  it.each([false, true])('stays disconnected after email dismissal and late wallet callbacks (previously disconnected: %s)', async (disconnected) => {
    if (disconnected) sessionStorage.setItem('plether.privy.test-privy-app.owner', 'disconnected')
    const wallet = { address: ownerAddress, walletClientType: 'metamask', meta: { name: 'MetaMask' }, disconnect: vi.fn() }
    mocks.wallets = disconnected ? [wallet] : []
    const view = render(tree())
    act(() => { actions.connect() })
    const options = mocks.connectExternal.mock.lastCall?.[0] as { description: ReactNode }
    render(<>{options.description}</>)
    fireEvent.click(screen.getByRole('button', { name: 'Log in with Email' }))
    await waitFor(() => { expect(mocks.connect).toHaveBeenCalledOnce() })
    act(() => { mocks.emailCallbacks?.onError('exited_auth_flow') })
    mocks.wallets = [wallet]
    view.rerender(tree())
    act(() => {
      mocks.callbacks?.onSuccess({ wallet })
      mocks.emailCallbacks?.onComplete({ user: { linkedAccounts: [{ type: 'wallet', chainType: 'ethereum', walletClientType: 'privy', address: ownerAddress }] }, loginMethod: 'email' })
      mocks.onWalletCreated?.({ wallet: { address: ownerAddress, chainType: 'ethereum' } })
    })
    expect(mocks.selector?.({ wallets: mocks.wallets })).toBeUndefined()
    expect(actions.signerReady).toBe(false)
    expect(actions.error).toBeNull()
    expect(sessionStorage.getItem('plether.privy.test-privy-app.owner')).toBe('disconnected')
  })

  it('selects the authenticated email wallet after completion even when wallets arrive later', () => {
    const view = render(tree())
    act(() => { actions.connect() })
    const options = mocks.connectExternal.mock.lastCall?.[0] as { description: ReactNode }
    render(<>{options.description}</>)
    fireEvent.click(screen.getByRole('button', { name: 'Log in with Email' }))
    const user = { linkedAccounts: [{ type: 'wallet', chainType: 'ethereum', walletClientType: 'privy', address: ownerAddress }] }
    act(() => { mocks.callbacks?.onError('generic_connect_wallet_error') })
    act(() => { mocks.emailCallbacks?.onComplete({ user, loginMethod: null }) })
    expect(mocks.selector?.({ wallets: [] })).toBeUndefined()
    act(() => { mocks.emailCallbacks?.onComplete({ user, loginMethod: 'email' }) })
    expect(mocks.selector?.({ wallets: [] })).toBeUndefined()
    mocks.wallets = [
      { address: otherAddress, walletClientType: 'metamask', meta: { name: 'MetaMask' }, disconnect: vi.fn() },
      { address: ownerAddress, walletClientType: 'privy', meta: { name: 'Privy' }, disconnect: vi.fn() },
    ]
    view.rerender(tree())
    expect(mocks.selector?.({ wallets: mocks.wallets })?.address).toBe(ownerAddress)
  })

  it('waits for verified email login before accepting a newly created embedded wallet', () => {
    const view = render(tree())
    act(() => { actions.connect() })
    const options = mocks.connectExternal.mock.lastCall?.[0] as { description: ReactNode }
    render(<>{options.description}</>)
    fireEvent.click(screen.getByRole('button', { name: 'Log in with Email' }))
    act(() => { mocks.onWalletCreated?.({ wallet: { address: otherAddress, chainType: 'ethereum' } }) })
    expect(sessionStorage.getItem('plether.privy.test-privy-app.owner')).toBe('disconnected')
    act(() => {
      mocks.emailCallbacks?.onComplete({ user: { linkedAccounts: [] }, loginMethod: 'email' })
      mocks.onWalletCreated?.({ wallet: { address: ownerAddress, chainType: 'ethereum' } })
    })
    mocks.wallets = [{ address: ownerAddress, walletClientType: 'privy', meta: { name: 'Privy' }, disconnect: vi.fn() }]
    view.rerender(tree())
    expect(mocks.selector?.({ wallets: mocks.wallets })?.address).toBe(ownerAddress)
  })

  it.each(['generic_connect_wallet_error', 'exited_auth_flow', 'exited_link_flow'])('keeps popup dismissal silent (%s)', (code) => {
    render(tree())
    act(() => { actions.connect() })
    act(() => { mocks.callbacks?.onError(code) })
    expect(actions.error).toBeNull()
  })

  it('still reports actionable connection failures and clears them on dismissal', () => {
    render(tree())
    act(() => { actions.connect() })
    act(() => { mocks.callbacks?.onError('client_request_timeout') })
    expect(actions.error).toBe('Wallet connection could not be completed. Try again.')
    act(() => { mocks.callbacks?.onError('generic_connect_wallet_error') })
    expect(actions.error).toBeNull()
  })

  it('opens the native dialog with the trading owner and applies its wallet selection', async () => {
    const owner = { address: ownerAddress, walletClientType: 'metamask', meta: { name: 'MetaMask' }, disconnect: vi.fn(), type: 'ethereum' as const }
    const other = { address: otherAddress, walletClientType: 'rabby', meta: { name: 'Rabby' }, disconnect: vi.fn(), type: 'ethereum' as const }
    mocks.wallets = [owner, other]
    mocks.activeAddress = ownerAddress
    sessionStorage.setItem('plether.privy.test-privy-app.owner', ownerAddress)
    mocks.openNative.mockResolvedValue({ wallet: other })
    render(tree())
    expect(screen.getByTestId('native-wallet-dialog')).toBeInTheDocument()
    await act(async () => { await actions.openAccount() })
    expect(mocks.setNative).toHaveBeenCalledWith(owner)
    expect(mocks.openNative).toHaveBeenCalledOnce()
    expect(mocks.selector?.({ wallets: mocks.wallets })?.address).toBe(otherAddress)
    expect(actions.signerReady).toBe(false)
  })

  it('synchronizes a newly connected native wallet while the dialog is open', async () => {
    const owner = { address: ownerAddress, walletClientType: 'metamask', meta: { name: 'MetaMask' }, disconnect: vi.fn(), type: 'ethereum' as const }
    const other = { address: otherAddress, walletClientType: 'rabby', meta: { name: 'Rabby' }, disconnect: vi.fn(), type: 'ethereum' as const }
    let closeDialog: (value: object) => void = () => { throw new Error('Dialog not opened') }
    mocks.openNative.mockImplementation(() => new Promise(resolve => { closeDialog = resolve }))
    mocks.wallets = [owner]
    mocks.activeAddress = ownerAddress
    sessionStorage.setItem('plether.privy.test-privy-app.owner', ownerAddress)
    const view = render(tree())
    let pending: Promise<void> | undefined
    act(() => { pending = actions.openAccount() })
    expect(actions.modalOpen).toBe(true)
    mocks.wallets = [owner, other]
    mocks.nativeWallet = other
    view.rerender(tree())
    expect(mocks.selector?.({ wallets: mocks.wallets })?.address).toBe(otherAddress)
    await act(async () => { closeDialog({}); await pending })
    expect(actions.modalOpen).toBe(false)
    expect(mocks.selector?.({ wallets: mocks.wallets })?.address).toBe(otherAddress)
    expect(actions.error).toBeNull()
  })

  it('keeps closing native wallet details silent', async () => {
    render(tree())
    await act(async () => { await actions.openAccount() })
    expect(actions.error).toBeNull()
  })

  it('waits for both auth and wallet initialization before connecting', () => {
    mocks.walletsReady = false
    render(tree())
    expect(screen.getByTestId('ready')).toHaveTextContent('false')
    act(() => { actions.connect() })
    expect(mocks.connect).not.toHaveBeenCalled()
    expect(mocks.connectExternal).not.toHaveBeenCalled()
  })

  it('keeps the initial owner when another wallet connects and changes owner only on selection', () => {
    const owner = { address: ownerAddress, walletClientType: 'metamask', meta: { name: 'MetaMask' }, disconnect: vi.fn() }
    const other = { address: otherAddress, walletClientType: 'privy', meta: { name: 'Privy' }, disconnect: vi.fn() }
    mocks.wallets = [owner]
    mocks.activeAddress = ownerAddress
    const view = render(tree())
    act(() => { expect(mocks.selector?.({ wallets: mocks.wallets })?.address).toBe(ownerAddress) })
    mocks.wallets = [other, owner]
    act(() => { mocks.callbacks?.onSuccess({ wallet: other }) })
    view.rerender(tree())
    act(() => { expect(mocks.selector?.({ wallets: mocks.wallets })?.address).toBe(ownerAddress) })
    act(() => { actions.selectWallet(otherAddress) })
    expect(mocks.selector?.({ wallets: mocks.wallets })?.address).toBe(otherAddress)
    expect(actions.signerReady).toBe(false)
    expect(actions.ready).toBe(true)
    act(() => { actions.connect() })
    expect(mocks.connectExternal).toHaveBeenCalledOnce()
    mocks.activeAddress = otherAddress
    view.rerender(tree())
    expect(actions.signerReady).toBe(true)
  })

  it('opens external connection rather than the unauthenticated login flow for a social user', () => {
    mocks.authenticated = true
    render(tree())
    act(() => { actions.connect() })
    expect(mocks.connectExternal).toHaveBeenCalledOnce()
    expect(mocks.connect).not.toHaveBeenCalled()
  })

  it('clears the active signer on disconnect even when an injected wallet stays available', async () => {
    const owner = { address: ownerAddress, walletClientType: 'metamask', meta: { name: 'MetaMask' }, disconnect: vi.fn() }
    mocks.wallets = [owner]
    mocks.activeAddress = ownerAddress
    render(tree())
    act(() => { expect(mocks.selector?.({ wallets: mocks.wallets })?.address).toBe(ownerAddress) })
    await act(async () => { await actions.disconnect() })
    expect(owner.disconnect).toHaveBeenCalledOnce()
    expect(mocks.logout).not.toHaveBeenCalled()
    expect(mocks.selector?.({ wallets: mocks.wallets })).toBeUndefined()
    expect(sessionStorage.getItem('plether.privy.test-privy-app.owner')).toBe('disconnected')
  })

  it('ends the social session and only exports an authenticated embedded wallet', async () => {
    const embedded = { address: ownerAddress, walletClientType: 'privy', meta: { name: 'Privy' }, disconnect: vi.fn() }
    mocks.wallets = [embedded]
    mocks.activeAddress = ownerAddress
    mocks.authenticated = true
    render(tree())
    await act(async () => { await actions.exportWallet(otherAddress) })
    expect(mocks.exportWallet).not.toHaveBeenCalled()
    await act(async () => { await actions.exportWallet(ownerAddress) })
    expect(mocks.exportWallet).toHaveBeenCalledWith({ address: ownerAddress })
    await act(async () => { await actions.disconnect() })
    expect(mocks.logout).toHaveBeenCalledOnce()
  })
})
