import type { ReactNode } from 'react'
import { act, fireEvent, render, screen } from '@testing-library/react'
import { QueryClient } from '@tanstack/react-query'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { WalletProvider } from './WalletProvider'
import { useRegistrationWallet } from './useRegistrationWallet'

interface TestWallet {
  address: string
  walletClientType: string
  meta: { name: string }
}

const mocks = vi.hoisted(() => ({
  appId: 'test-privy-app' as string | undefined,
  ready: true,
  authenticated: false,
  walletsReady: true,
  isConnecting: false,
  isReconnecting: false,
  activeAddress: undefined as string | undefined,
  wallets: [] as TestWallet[],
  connect: vi.fn(),
  connectExternal: vi.fn(),
  selector: undefined as ((input: { wallets: TestWallet[] }) => TestWallet | undefined) | undefined,
  onError: undefined as ((code: string) => void) | undefined,
  onSuccess: undefined as ((input: { wallet: TestWallet }) => void) | undefined,
  onExternalSuccess: undefined as ((input: { wallet: TestWallet }) => void) | undefined,
}))

vi.mock('../config/wagmi', () => ({
  get privyAppId() { return mocks.appId },
  privyClientId: undefined,
  privyConfig: {},
  wagmiConfig: {},
}))
vi.mock('@privy-io/react-auth', () => ({
  PrivyProvider: ({ children }: { children: ReactNode }) => <div data-testid="privy-provider">{children}</div>,
  usePrivy: () => ({ ready: mocks.ready, authenticated: mocks.authenticated }),
  useWallets: () => ({ ready: mocks.walletsReady, wallets: mocks.wallets }),
  useConnectOrCreateWallet: ({ onSuccess }: { onSuccess: (input: { wallet: TestWallet }) => void }) => {
    mocks.onSuccess = onSuccess
    return { connectOrCreateWallet: mocks.connect }
  },
  useConnectWallet: ({ onSuccess, onError }: { onSuccess: (input: { wallet: TestWallet }) => void; onError: (code: string) => void }) => {
    mocks.onError = onError
    mocks.onExternalSuccess = onSuccess
    return { connectWallet: mocks.connectExternal }
  },
}))
vi.mock('@privy-io/wagmi', () => ({
  WagmiProvider: ({ children, setActiveWalletForWagmi }: {
    children: ReactNode
    setActiveWalletForWagmi: (input: { wallets: TestWallet[] }) => TestWallet | undefined
  }) => {
    mocks.selector = setActiveWalletForWagmi
    return <div data-testid="privy-wagmi-provider">{children}</div>
  },
}))
vi.mock('wagmi', () => ({
  WagmiProvider: ({ children }: { children: ReactNode }) => <div data-testid="readonly-wagmi-provider">{children}</div>,
  useConnection: () => ({ address: mocks.activeAddress, isConnecting: mocks.isConnecting, isReconnecting: mocks.isReconnecting }),
}))

const owner: TestWallet = { address: '0x1111111111111111111111111111111111111111', walletClientType: 'privy', meta: { name: 'Privy' } }
const other: TestWallet = { address: '0x2222222222222222222222222222222222222222', walletClientType: 'metamask', meta: { name: 'MetaMask' } }

function Controls() {
  const wallet = useRegistrationWallet()
  return <>
    <p data-testid="state">{JSON.stringify({ configured: wallet.configured, ready: wallet.ready, isSelecting: wallet.isSelecting })}</p>
    {wallet.error ? <p role="alert">{wallet.error}</p> : null}
    <button onClick={wallet.connect}>Connect</button>
    <button onClick={() => { wallet.selectWallet(other.address) }}>Choose other</button>
  </>
}

function Provider() {
  return <WalletProvider queryClient={new QueryClient()}><Controls /></WalletProvider>
}

beforeEach(() => {
  vi.clearAllMocks()
  window.sessionStorage.clear()
  mocks.appId = 'test-privy-app'
  mocks.ready = true
  mocks.authenticated = false
  mocks.walletsReady = true
  mocks.isConnecting = false
  mocks.isReconnecting = false
  mocks.activeAddress = undefined
  mocks.wallets = []
  mocks.selector = undefined
  mocks.onSuccess = undefined
  mocks.onExternalSuccess = undefined
})

describe('Insights Privy wallet provider', () => {
  it('renders read-only content without a configured app ID', () => {
    mocks.appId = undefined
    render(<Provider />)
    expect(screen.getByTestId('readonly-wagmi-provider')).toBeInTheDocument()
    expect(screen.queryByTestId('privy-provider')).not.toBeInTheDocument()
    expect(screen.getByTestId('state')).toHaveTextContent('"configured":false')
  })

  it('allows external connection without requiring Privy authentication', () => {
    render(<Provider />)
    fireEvent.click(screen.getByRole('button', { name: 'Connect' }))
    expect(mocks.connectExternal).toHaveBeenCalledTimes(1)
  })

  it('shows the email option before the wallet list without requiring wallet authentication', () => {
    render(<Provider />)
    fireEvent.click(screen.getByRole('button', { name: 'Connect' }))
    const options = mocks.connectExternal.mock.lastCall?.[0] as { description: ReactNode }
    render(<>{options.description}</>)
    expect(mocks.connect).not.toHaveBeenCalled()
    fireEvent.click(screen.getByRole('button', { name: 'Log in with Email' }))
    expect(mocks.connect).toHaveBeenCalledTimes(1)
  })

  it.each(['generic_connect_wallet_error', 'exited_auth_flow', 'exited_link_flow'])('keeps popup dismissal silent (%s)', (code) => {
    render(<Provider />)
    fireEvent.click(screen.getByRole('button', { name: 'Connect' }))
    act(() => { mocks.onError?.(code) })
    expect(screen.queryByRole('alert')).not.toBeInTheDocument()
  })

  it('retains actionable connection errors', () => {
    render(<Provider />)
    act(() => { mocks.onError?.('client_request_timeout') })
    expect(screen.getByRole('alert')).toHaveTextContent('Wallet connection could not be completed. Try again.')
  })

  it('does not open a connection modal before wallets finish loading', () => {
    mocks.walletsReady = false
    render(<Provider />)
    fireEvent.click(screen.getByRole('button', { name: 'Connect' }))
    expect(mocks.connect).not.toHaveBeenCalled()
    expect(screen.getByTestId('state')).toHaveTextContent('"ready":false')
  })

  it('opens the external connector for another wallet while social-authenticated', () => {
    mocks.authenticated = true
    mocks.wallets = [owner]
    const view = render(<Provider />)
    expect(mocks.selector?.({ wallets: [owner] })).toBe(owner)
    fireEvent.click(screen.getByRole('button', { name: 'Connect' }))
    expect(mocks.connectExternal).toHaveBeenCalledTimes(1)
    expect(mocks.connect).not.toHaveBeenCalled()
    mocks.wallets = [owner, other]
    view.rerender(<Provider />)
    act(() => { mocks.onExternalSuccess?.({ wallet: other }) })
    expect(mocks.selector?.({ wallets: mocks.wallets })).toBe(owner)
  })

  it('remembers the first sole owner when another wallet connects', () => {
    mocks.wallets = [owner]
    const view = render(<Provider />)
    expect(mocks.selector?.({ wallets: [owner] })).toBe(owner)
    mocks.wallets = [other, owner]
    view.rerender(<Provider />)
    expect(mocks.selector?.({ wallets: [other, owner] })).toBe(owner)
    expect(mocks.selector?.({ wallets: [other] })).toBeUndefined()
  })

  it('waits for an explicit choice when multiple wallets reconnect', () => {
    mocks.wallets = [owner, other]
    render(<Provider />)
    expect(mocks.selector?.({ wallets: mocks.wallets })).toBeUndefined()
    fireEvent.click(screen.getByRole('button', { name: 'Choose other' }))
    expect(mocks.selector?.({ wallets: mocks.wallets })).toBe(other)
    expect(screen.getByTestId('state')).toHaveTextContent('"isSelecting":true')
  })

  it('activates the first wallet returned by an explicit connection action', () => {
    mocks.wallets = [other]
    render(<Provider />)
    act(() => { mocks.onSuccess?.({ wallet: other }) })
    expect(mocks.selector?.({ wallets: mocks.wallets })).toBe(other)
    expect(window.sessionStorage.getItem('plether:privy:insights:owner:test-privy-app')).toBe(other.address)
  })

  it('does not switch an established owner after connecting another wallet', () => {
    mocks.wallets = [owner]
    const view = render(<Provider />)
    expect(mocks.selector?.({ wallets: [owner] })).toBe(owner)
    mocks.wallets = [owner, other]
    view.rerender(<Provider />)
    act(() => { mocks.onSuccess?.({ wallet: other }) })
    expect(mocks.selector?.({ wallets: mocks.wallets })).toBe(owner)
  })

  it('keeps the bridge disconnected until Privy and its wallets are ready', () => {
    mocks.wallets = [owner]
    mocks.ready = false
    const view = render(<Provider />)
    expect(mocks.selector?.({ wallets: [owner] })).toBeUndefined()
    mocks.ready = true
    mocks.walletsReady = false
    view.rerender(<Provider />)
    expect(mocks.selector?.({ wallets: [owner] })).toBeUndefined()
    mocks.walletsReady = true
    view.rerender(<Provider />)
    expect(mocks.selector?.({ wallets: [owner] })).toBe(owner)
  })

  it('gates signing during bridge reconnection while connection controls stay usable', () => {
    mocks.wallets = [owner]
    mocks.activeAddress = owner.address
    mocks.isReconnecting = true
    render(<Provider />)
    expect(screen.getByTestId('state')).toHaveTextContent('"ready":true')
    expect(screen.getByTestId('state')).toHaveTextContent('"isSelecting":true')
    fireEvent.click(screen.getByRole('button', { name: 'Connect' }))
    expect(mocks.connectExternal).toHaveBeenCalledTimes(1)
  })
})
