import { useCallback, useRef, useState, type ReactNode } from 'react'
import {
  PrivyProvider,
  useConnectOrCreateWallet,
  useConnectWallet,
  usePrivy,
  useWallets,
  type ConnectedWallet,
} from '@privy-io/react-auth'
import { WagmiProvider as PrivyWagmiProvider } from '@privy-io/wagmi'
import { QueryClientProvider, type QueryClient } from '@tanstack/react-query'
import { WagmiProvider, useConnection } from 'wagmi'
import { privyAppId, privyClientId, privyConfig, wagmiConfig } from '../config/wagmi'
import { RegistrationWalletContext, type RegistrationWalletActions } from './context'
import { readSelectedWallet, saveSelectedWallet, selectedWallet } from './selection'
import { EmailLoginOption } from './EmailLoginOption'

function WalletControls({ children, actions, selectedAddress }: {
  children: ReactNode
  actions: Omit<RegistrationWalletActions, 'isSelecting'>
  selectedAddress: string | null
}) {
  const { address, isConnecting, isReconnecting } = useConnection()
  const isSelecting = Boolean(isConnecting || isReconnecting || (selectedAddress && actions.wallets.some((wallet) =>
    wallet.address.toLowerCase() === selectedAddress.toLowerCase(),
  ) && address?.toLowerCase() !== selectedAddress.toLowerCase()))

  return (
    <RegistrationWalletContext.Provider value={{ ...actions, isSelecting }}>
      {children}
    </RegistrationWalletContext.Provider>
  )
}

function ConnectedWalletProvider({ children, appId }: { children: ReactNode; appId: string }) {
  const { ready, authenticated } = usePrivy()
  const { ready: walletsReady, wallets } = useWallets()
  const storageKey = `plether:privy:insights:owner:${appId}`
  const [selectedAddress, setSelectedAddress] = useState(() => readSelectedWallet(storageKey))
  const initialOwner = useRef<string | null>(null)
  const [error, setError] = useState<string | null>(null)
  const selectWallet = useCallback((address: string) => {
    setSelectedAddress(address.toLowerCase())
    saveSelectedWallet(storageKey, address)
    setError(null)
  }, [storageKey])
  const onConnected = useCallback(({ wallet }: { wallet: { address: string } }) => {
    if (!selectedAddress && !initialOwner.current) selectWallet(wallet.address)
  }, [selectedAddress, selectWallet])
  const onConnectError = useCallback((code: string) => {
    // Closing Privy's wallet picker uses the same generic code as failures
    // already displayed inside the picker; don't persist those below the CTA.
    const dismissed = code === 'generic_connect_wallet_error'
      || code === 'exited_auth_flow' || code === 'exited_link_flow'
    setError(dismissed ? null : 'Wallet connection could not be completed. Try again.')
  }, [])
  const { connectOrCreateWallet } = useConnectOrCreateWallet({ onSuccess: onConnected, onError: onConnectError })
  const { connectWallet } = useConnectWallet({ onSuccess: onConnected, onError: onConnectError })
  const resolveActiveWallet = useCallback(({ wallets: connectedWallets }: { wallets: ConnectedWallet[] }) => {
    if (!ready || !walletsReady) return undefined
    const owner = selectedWallet(connectedWallets, selectedAddress ?? initialOwner.current)
    // The bridge calls this when its connection state changes. Remember the sole
    // initial wallet so another connection can never replace it automatically.
    if (owner && !selectedAddress && !initialOwner.current) {
      initialOwner.current = owner.address
      saveSelectedWallet(storageKey, owner.address)
    }
    return owner
  }, [ready, walletsReady, selectedAddress, storageKey])

  const actions = {
    configured: true,
    ready: ready && walletsReady,
    connect: () => {
      if (!ready || !walletsReady) return
      setError(null)
      connectWallet({
        description: authenticated ? '' : <EmailLoginOption onClick={() => { connectOrCreateWallet() }} />,
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
      <WalletControls actions={actions} selectedAddress={selectedAddress}>{children}</WalletControls>
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
