import { useCallback, useEffect, useLayoutEffect, useMemo, useRef, useState, type ReactNode } from 'react'
import {
  PrivyProvider,
  useConnectWallet,
  useCreateWallet,
  useLogin,
  usePrivy,
  useWallets,
  type ConnectedWallet,
} from '@privy-io/react-auth'
import { WagmiProvider as PrivyWagmiProvider } from '@privy-io/wagmi'
import { usePletherWalletPickerClose } from '@privy-io/react-auth/ui'
import { QueryClientProvider, type QueryClient } from '@tanstack/react-query'
import { WagmiProvider, useConnection } from 'wagmi'
import { privyAppId, privyClientId, privyConfig, wagmiConfig } from '../config/wagmi'
import { RegistrationWalletContext, type RegistrationWalletActions } from './context'
import { readSelectedWallet, saveSelectedWallet, selectedWallet } from './selection'
import { EmailLoginOption } from './EmailLoginOption'
import { createSigningGuard } from './signingGuard'

function WalletControls({ children, actions, expectedAddress, onStaleConnection }: {
  children: ReactNode
  actions: Omit<RegistrationWalletActions, 'isSelecting'>
  expectedAddress: string | undefined
  onStaleConnection: () => void
}) {
  const { address, isConnecting, isReconnecting } = useConnection()
  useEffect(() => {
    if (actions.ready && address && address.toLowerCase() !== expectedAddress?.toLowerCase()) {
      onStaleConnection()
    }
  }, [actions.ready, address, expectedAddress, onStaleConnection])
  const isSelecting = Boolean(isConnecting || isReconnecting || (
    expectedAddress && address?.toLowerCase() !== expectedAddress.toLowerCase()))

  return (
    <RegistrationWalletContext.Provider value={{ ...actions, isSelecting }}>
      {children}
    </RegistrationWalletContext.Provider>
  )
}

function ConnectedWalletProvider({ children, appId }: { children: ReactNode; appId: string }) {
  const { ready, authenticated, user } = usePrivy()
  const closeWalletPicker = usePletherWalletPickerClose()
  const { ready: walletsReady, wallets } = useWallets()
  const storageKey = `plether:privy:insights:owner:${appId}`
  const [selectedAddress, setSelectedAddress] = useState(() => readSelectedWallet(storageKey))
  const [signingGuard] = useState(() => createSigningGuard(selectedAddress))
  const [bridgeRevision, setBridgeRevision] = useState(0)
  const bridgeSelection = useMemo(() => ({ owner: selectedAddress, retry: bridgeRevision }), [selectedAddress, bridgeRevision])
  const initialOwner = useRef<string | null>(null)
  const emailIntent = useRef(false)
  const emailVerified = useRef(false)
  const connectionIntent = useRef(false)
  const [error, setError] = useState<string | null>(null)
  const expectedAddress = selectedWallet(wallets, selectedAddress === undefined ? signingGuard.selection : selectedAddress)?.address
  useLayoutEffect(() => {
    const availableOwner = selectedWallet(wallets, selectedAddress === null ? null : selectedAddress ?? initialOwner.current ?? undefined)
    signingGuard.setReady(ready && walletsReady && availableOwner !== undefined)
  }, [ready, walletsReady, wallets, selectedAddress, signingGuard])
  const resetStaleConnection = useCallback(() => {
    wagmiConfig.setState(state => ({ ...state, connections: new Map(), current: null, status: 'disconnected' }))
    setBridgeRevision(revision => revision + 1)
  }, [])
  const selectWallet = useCallback((address: string) => {
    signingGuard.select(address)
    connectionIntent.current = false
    emailIntent.current = false
    emailVerified.current = false
    setSelectedAddress(address.toLowerCase())
    saveSelectedWallet(storageKey, address)
    setError(null)
  }, [storageKey, signingGuard])
  const selectConnectedOwner = useCallback((address: string) => {
    const owner = selectedAddress === null ? null : selectedAddress ?? initialOwner.current
    if (!owner || !wallets.some(wallet => wallet.address.toLowerCase() === owner.toLowerCase())) {
      selectWallet(address)
    }
  }, [selectedAddress, selectWallet, wallets])
  const onConnected = useCallback(({ wallet }: { wallet: { address: string } }) => {
    if (!connectionIntent.current || emailIntent.current) return
    connectionIntent.current = false
    selectConnectedOwner(wallet.address)
  }, [selectConnectedOwner])
  const onConnectError = useCallback((code: string) => {
    connectionIntent.current = false
    // Closing Privy's wallet picker uses the same generic code as failures
    // already displayed inside the picker; don't persist those below the CTA.
    const dismissed = code === 'generic_connect_wallet_error'
      || code === 'exited_auth_flow' || code === 'exited_link_flow'
    setError(dismissed ? null : 'Wallet connection could not be completed. Try again.')
  }, [])
  const activateEmailWallet = (address: string) => {
    if (!emailIntent.current || !emailVerified.current) return
    emailIntent.current = false
    emailVerified.current = false
    // Email was explicitly chosen, so it may replace an external owner.
    selectWallet(address)
  }
  const { login } = useLogin({
    onComplete: ({ user, loginMethod }) => {
      if (!emailIntent.current || loginMethod !== 'email') return
      emailVerified.current = true
      const wallet = user.linkedAccounts.filter(account => account.type === 'wallet')
        .find(account => account.chainType === 'ethereum' && account.walletClientType === 'privy')
        ?? (user.wallet?.chainType === 'ethereum' && user.wallet.walletClientType === 'privy' ? user.wallet : undefined)
      if (wallet) activateEmailWallet(wallet.address)
    },
    onError: (code) => {
      if (!emailIntent.current) return
      emailIntent.current = false
      emailVerified.current = false
      onConnectError(code)
    },
  })
  const { createWallet } = useCreateWallet({ onSuccess: ({ wallet }) => {
    if (wallet.chainType === 'ethereum') activateEmailWallet(wallet.address)
  } })
  const { connectWallet } = useConnectWallet({ onSuccess: onConnected, onError: onConnectError })
  const openEmailWallet = async () => {
    connectionIntent.current = false
    emailVerified.current = false
    if (authenticated) {
      const embedded = wallets.find(wallet => wallet.walletClientType === 'privy')
        ?? user?.linkedAccounts.filter(account => account.type === 'wallet')
          .find(account => account.chainType === 'ethereum' && account.walletClientType === 'privy')
      if (embedded) {
        selectWallet(embedded.address)
        await closeWalletPicker()
        return
      }
    }
    const owner = selectedAddress === null ? null : selectedAddress ?? initialOwner.current
    if (!owner || !wallets.some(wallet => wallet.address.toLowerCase() === owner.toLowerCase())) {
      signingGuard.select(null)
      saveSelectedWallet(storageKey, null)
      setSelectedAddress(null)
    }
    emailIntent.current = true
    // An existing authenticated session needs wallet creation, not another
    // login flow (which Privy skips for an authenticated user).
    emailVerified.current = authenticated
    const revision = signingGuard.revision
    const isCurrentRequest = () => emailIntent.current && signingGuard.revision === revision
    await closeWalletPicker()
    if (!isCurrentRequest()) return
    if (!authenticated) {
      login({ loginMethods: ['email'] })
      return
    }
    try {
      const wallet = await createWallet()
      if (isCurrentRequest()) activateEmailWallet(wallet.address)
    } catch {
      if (!isCurrentRequest()) return
      emailIntent.current = false
      emailVerified.current = false
      setError('Could not prepare your email wallet. Try again.')
    }
  }
  const resolveActiveWallet = useCallback(({ wallets: connectedWallets }: { wallets: ConnectedWallet[] }) => {
    if (!ready || !walletsReady) return undefined
    const selection = bridgeSelection.owner === null ? null : bridgeSelection.owner ?? initialOwner.current ?? undefined
    const owner = selectedWallet(connectedWallets, selection)
    // The bridge calls this when its connection state changes. Remember the sole
    // initial wallet so another connection can never replace it automatically.
    if (owner && bridgeSelection.owner === undefined && !initialOwner.current) {
      initialOwner.current = owner.address
      signingGuard.select(owner.address)
      saveSelectedWallet(storageKey, owner.address)
    }
    return owner ? signingGuard.wrapWallet(owner) : undefined
  }, [ready, walletsReady, bridgeSelection, storageKey, signingGuard])

  const actions = {
    configured: true,
    ready: ready && walletsReady,
    connect: () => {
      if (!ready || !walletsReady) return
      setError(null)
      emailIntent.current = false
      emailVerified.current = false
      connectionIntent.current = true
      connectWallet({
        description: <EmailLoginOption label={authenticated ? 'Use email wallet' : 'Log in with Email'} onClick={() => { void openEmailWallet() }} />,
      })
    },
    selectWallet: (address: string) => {
      if (wallets.some((wallet) => wallet.address.toLowerCase() === address.toLowerCase())) selectWallet(address)
    },
    wallets: wallets.map((wallet) => ({
      address: wallet.address,
      label: wallet.walletClientType === 'privy' ? 'Privy wallet' : wallet.meta.name,
    })),
    error,
  }

  return (
    <PrivyWagmiProvider config={wagmiConfig} setActiveWalletForWagmi={resolveActiveWallet}>
      <WalletControls actions={actions} expectedAddress={expectedAddress} onStaleConnection={resetStaleConnection}>{children}</WalletControls>
    </PrivyWagmiProvider>
  )
}

export function WalletProvider({ children, queryClient }: { children: ReactNode; queryClient: QueryClient }) {
  if (!privyAppId) {
    return (
      <QueryClientProvider client={queryClient}>
        <WagmiProvider config={wagmiConfig} reconnectOnMount={false}>{children}</WagmiProvider>
      </QueryClientProvider>
    )
  }

  return (
    <PrivyProvider appId={privyAppId} clientId={privyClientId} config={privyConfig}>
      <QueryClientProvider client={queryClient}>
        <ConnectedWalletProvider appId={privyAppId}>{children}</ConnectedWalletProvider>
      </QueryClientProvider>
    </PrivyProvider>
  )
}
