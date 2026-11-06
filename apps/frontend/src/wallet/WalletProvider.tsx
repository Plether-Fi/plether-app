import { useCallback, useEffect, useLayoutEffect, useMemo, useRef, useState, type ReactNode } from 'react'
import {
  PrivyProvider,
  useActiveWallet,
  useConnectWallet,
  useCreateWallet,
  useLogin,
  useExportWallet,
  usePrivy,
  useSetWalletRecovery,
  useWallets,
  type ConnectedWallet,
  type PrivyClientConfig,
} from '@privy-io/react-auth'
import { WagmiProvider } from '@privy-io/wagmi'
import { WalletsDialog } from '@privy-io/react-auth/ui'
import { QueryClientProvider, type QueryClient } from '@tanstack/react-query'
import { useAccount, WagmiProvider as ReadOnlyWagmiProvider } from 'wagmi'
import { config, walletChains } from '../config/wagmi'
import { isSepoliaDeployment } from '../utils/deployment'
import { WalletActionsContext, type WalletActions } from './useWalletActions'
import { readWalletSelection, selectedWallet, writeWalletSelection, type WalletSelection } from './selection'
import { createSigningGuard } from './signingGuard'
import { EmailLoginOption } from './EmailLoginOption'

function publicId(value: unknown): string | undefined {
  return typeof value === 'string' && value.trim().length > 0 ? value.trim() : undefined
}

const appId = publicId(import.meta.env.VITE_PRIVY_APP_ID)
const clientId = publicId(import.meta.env.VITE_PRIVY_CLIENT_ID)
const selectionKey = `plether.privy.${appId ?? 'unconfigured'}.owner`
const configuredChainId = Number(import.meta.env.VITE_DEFAULT_CHAIN_ID)
const defaultChain = walletChains.find(chain => chain.id === configuredChainId)
  ?? walletChains[isSepoliaDeployment() ? 2 : 0]

const privyConfig: PrivyClientConfig = {
  loginMethods: ['email'],
  appearance: {
    theme: '#250917',
    accentColor: '#FF572D',
    logo: '/logomark.svg',
    landingHeader: 'Connect',
    showWalletLoginFirst: false,
    walletChainType: 'ethereum-only',
    walletList: ['metamask', 'coinbase_wallet', 'rainbow', 'detected_ethereum_wallets', 'wallet_connect'],
  },
  supportedChains: [...walletChains],
  defaultChain,
  embeddedWallets: { ethereum: { createOnLogin: 'users-without-wallets' } },
}

function loadSelection(): WalletSelection {
  try {
    return readWalletSelection(window.sessionStorage, selectionKey)
  } catch {
    return undefined
  }
}

function saveSelection(selection: string | null) {
  try {
    writeWalletSelection(window.sessionStorage, selectionKey, selection)
  } catch {
    // The active wallet still works if browser storage is unavailable.
  }
}

function SyncedWalletActions({
  actions,
  expectedAddress,
  onStaleConnection,
  children,
}: {
  actions: WalletActions
  expectedAddress: string | undefined
  onStaleConnection: () => void
  children: ReactNode
}) {
  const { address, isConnecting, isReconnecting } = useAccount()
  useEffect(() => {
    if (actions.ready && address && address.toLowerCase() !== expectedAddress?.toLowerCase()) {
      onStaleConnection()
    }
  }, [actions.ready, address, expectedAddress, onStaleConnection])
  // The bridge updates its connector asynchronously. Keep wallet controls
  // disabled while it is still exposing the previous owner's signer.
  const signerReady = actions.ready && expectedAddress !== undefined && !isConnecting && !isReconnecting
    && address?.toLowerCase() === expectedAddress.toLowerCase()
  return (
    <WalletActionsContext value={{ ...actions, signerReady }}>
      {children}
    </WalletActionsContext>
  )
}

function ConfiguredWalletProvider({ children }: { children: ReactNode }) {
  const { ready: authReady, authenticated, logout } = usePrivy()
  const { wallets, ready: walletsReady } = useWallets()
  const { wallet: nativeWallet, connect: openNativeWallets, setActiveWallet: setNativeWallet } = useActiveWallet()
  const [accountOpen, setAccountOpen] = useState(false)
  const nativeWalletRef = useRef(nativeWallet)
  useLayoutEffect(() => { nativeWalletRef.current = nativeWallet }, [nativeWallet])
  const { exportWallet } = useExportWallet()
  const { setWalletRecovery } = useSetWalletRecovery()
  const [selection, setSelection] = useState<WalletSelection>(loadSelection)
  const [signingGuard] = useState(() => createSigningGuard(selection))
  const [bridgeRevision, setBridgeRevision] = useState(0)
  const connectionIntent = useRef<number | undefined>(undefined)
  const emailIntent = useRef<number | undefined>(undefined)
  const emailVerified = useRef(false)
  const bridgeSelection = useMemo(() => ({ owner: selection, retry: bridgeRevision }), [selection, bridgeRevision])
  const [error, setError] = useState<string | null>(null)
  const ready = authReady && walletsReady
  const availableOwner = selectedWallet(wallets, selection)?.address
  useLayoutEffect(() => {
    signingGuard.setReady(ready && availableOwner !== undefined)
  }, [ready, availableOwner, signingGuard])

  const updateSelection = useCallback((address: string | null) => {
    // Update signing authority synchronously, before React or the bridge catches up.
    signingGuard.select(address)
    saveSelection(address)
    setSelection(address?.toLowerCase() ?? null)
    setBridgeRevision(signingGuard.revision)
  }, [signingGuard])

  const resetStaleConnection = useCallback(() => {
    config.setState(state => ({ ...state, connections: new Map(), current: null, status: 'disconnected' }))
    setBridgeRevision(revision => revision + 1)
  }, [])

  const chooseWallet = useCallback((address: string) => {
    if (!ready || !wallets.some(wallet => wallet.address.toLowerCase() === address.toLowerCase())) return
    setError(null)
    updateSelection(address)
  }, [ready, wallets, updateSelection])

  useEffect(() => {
    if (accountOpen && nativeWallet?.type === 'ethereum') chooseWallet(nativeWallet.address)
  }, [accountOpen, nativeWallet, chooseWallet])

  const onWalletConnected = ({ wallet }: { wallet: { address: string } }) => {
    if (connectionIntent.current !== signingGuard.revision) return
    connectionIntent.current = undefined
    // Connecting an additional wallet never changes an existing owner. The
    // account panel is the only place to switch an established selection.
    if (signingGuard.selection === null || signingGuard.selection === undefined) {
      updateSelection(wallet.address)
    }
  }
  const onConnectionError = (code: string) => {
    connectionIntent.current = undefined
    // Privy reports closing its wallet picker as generic_connect_wallet_error.
    // The SDK also handles generic connection failures inside that picker.
    const dismissed = code === 'generic_connect_wallet_error'
      || code === 'exited_auth_flow' || code === 'exited_link_flow'
    setError(dismissed ? null : 'Wallet connection could not be completed. Try again.')
  }
  const activateEmailWallet = (address: string) => {
    if (!emailVerified.current || emailIntent.current !== signingGuard.revision) return
    emailIntent.current = undefined
    emailVerified.current = false
    if (signingGuard.selection === null || signingGuard.selection === undefined) updateSelection(address)
  }
  const { login } = useLogin({
    onComplete: ({ user, loginMethod }) => {
      // Privy also emits login callbacks while restoring an existing session.
      // Only a completed, explicitly requested email login can select a wallet.
      if (emailIntent.current !== signingGuard.revision || loginMethod !== 'email') return
      emailVerified.current = true
      const wallet = user.linkedAccounts.filter(account => account.type === 'wallet')
        .find(account => account.chainType === 'ethereum' && account.walletClientType === 'privy')
        ?? (user.wallet?.chainType === 'ethereum' ? user.wallet : undefined)
      if (wallet) activateEmailWallet(wallet.address)
    },
    onError: (code) => {
      if (emailIntent.current === undefined) return
      emailIntent.current = undefined
      emailVerified.current = false
      onConnectionError(code)
    },
  })
  // Automatic embedded-wallet creation may finish after email authentication.
  useCreateWallet({ onSuccess: ({ wallet }) => {
    if (wallet.chainType === 'ethereum') activateEmailWallet(wallet.address)
  } })
  const { connectWallet } = useConnectWallet({ onSuccess: onWalletConnected, onError: onConnectionError })

  const selectForWagmi = useCallback(({ wallets: connectedWallets }: { wallets: ConnectedWallet[] }) => {
    if (!ready) return undefined
    const selected = selectedWallet(connectedWallets, bridgeSelection.owner)
    if (selected && bridgeSelection.owner === undefined && signingGuard.selection === undefined) {
      updateSelection(selected.address)
    }
    return selected ? signingGuard.wrapWallet(selected) : undefined
  }, [ready, bridgeSelection, signingGuard, updateSelection])

  const actions: WalletActions = {
    configured: true,
    ready,
    signerReady: false,
    authenticated,
    wallets: wallets.map(wallet => ({
      address: wallet.address,
      name: wallet.walletClientType === 'privy' ? 'Social wallet' : wallet.meta.name,
      embedded: wallet.walletClientType === 'privy',
    })),
    error,
    connect: () => {
      if (!ready) return
      setError(null)
      emailIntent.current = undefined
      emailVerified.current = false
      connectionIntent.current = signingGuard.revision
      connectWallet({
        description: authenticated ? '' : <EmailLoginOption onClick={() => {
          connectionIntent.current = undefined
          // Prevent a wallet appearing during login/cancellation from being
          // automatically restored by the bridge, including after a reload.
          if (signingGuard.selection === undefined) updateSelection(null)
          emailIntent.current = signingGuard.revision
          emailVerified.current = false
          login({ loginMethods: ['email'] })
        }} />,
      })
    },
    openAccount: async () => {
      if (!ready || accountOpen) return
      setError(null)
      const owner = selectedWallet(wallets, signingGuard.selection)
      if (owner) setNativeWallet(owner)
      setAccountOpen(true)
      try {
        const result = await openNativeWallets()
        const chosen = result.wallet ?? nativeWalletRef.current
        if (chosen?.type === 'ethereum') chooseWallet(chosen.address)
      } catch {
        setError('Could not open wallet details. Try again.')
      } finally {
        setAccountOpen(false)
      }
    },
    selectWallet: chooseWallet,
    disconnect: async () => {
      if (!ready) return
      setError(null)
      connectionIntent.current = undefined
      emailIntent.current = undefined
      emailVerified.current = false
      // Clear the active signer even when an injected wallet cannot revoke
      // its site permission programmatically. Persist that choice on reload.
      updateSelection(null)
      try {
        wallets.forEach(wallet => { wallet.disconnect() })
        if (authenticated) await logout()
      } catch {
        setError('Could not finish signing out. Try again.')
      }
    },
    exportWallet: async (address) => {
      if (!ready || !authenticated) return
      if (!wallets.some(wallet => wallet.walletClientType === 'privy' && wallet.address.toLowerCase() === address.toLowerCase())) return
      setError(null)
      try {
        await exportWallet({ address })
      } catch {
        setError('Could not open wallet export. Try again.')
      }
    },
    setRecovery: async () => {
      if (!ready || !authenticated) return
      setError(null)
      try {
        await setWalletRecovery()
      } catch {
        setError('Could not open wallet recovery settings. Try again.')
      }
    },
    clearError: () => { setError(null) },
  }

  return (
    <WagmiProvider config={config} setActiveWalletForWagmi={selectForWagmi}>
      <SyncedWalletActions
        actions={actions}
        expectedAddress={availableOwner}
        onStaleConnection={resetStaleConnection}
      >
        <WalletsDialog networks={walletChains.filter(chain => chain.id !== 31337).map(chain => ({ id: chain.id }))} />
        {children}
      </SyncedWalletActions>
    </WagmiProvider>
  )
}

export function WalletProvider({ children, queryClient }: { children: ReactNode; queryClient: QueryClient }) {
  if (!appId) {
    return (
      <QueryClientProvider client={queryClient}>
        <ReadOnlyWagmiProvider config={config} reconnectOnMount={false}>
          {children}
        </ReadOnlyWagmiProvider>
      </QueryClientProvider>
    )
  }

  return (
    <PrivyProvider appId={appId} clientId={clientId} config={privyConfig}>
      <QueryClientProvider client={queryClient}>
        <ConfiguredWalletProvider>{children}</ConfiguredWalletProvider>
      </QueryClientProvider>
    </PrivyProvider>
  )
}
