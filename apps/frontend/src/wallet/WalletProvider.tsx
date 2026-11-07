import { useCallback, useEffect, useLayoutEffect, useMemo, useRef, useState, type ReactNode } from 'react'
import {
  PrivyProvider,
  useActiveWallet,
  useConnectWallet,
  useCreateWallet,
  useLogin,
  useLoginWithOAuth,
  useLoginWithPasskey,
  useLinkAccount,
  useModalStatus,
  useExportWallet,
  usePrivy,
  useSetWalletRecovery,
  useWallets,
  type ConnectedWallet,
  type PrivyClientConfig,
} from '@privy-io/react-auth'
import { WagmiProvider } from '@privy-io/wagmi'
import { WalletsDialog, usePletherWalletPickerClose } from '@privy-io/react-auth/ui'
import { QueryClientProvider, type QueryClient } from '@tanstack/react-query'
import { useAccount, WagmiProvider as ReadOnlyWagmiProvider } from 'wagmi'
import { config, walletChains } from '../config/wagmi'
import { isSepoliaDeployment } from '../utils/deployment'
import { WalletActionsContext, type WalletActions } from './useWalletActions'
import { readWalletSelection, selectedWallet, writeWalletSelection, type WalletSelection } from './selection'
import { createSigningGuard } from './signingGuard'
import { EmailLoginOption } from './EmailLoginOption'
import { WalletMfaSettings } from './WalletMfaSettings'
import { isPasskeyOriginSupported } from './passkeyOrigin'
import { clearGoogleLoginIntent, isGoogleLoginReturn, saveGoogleLoginIntent } from './googleLoginIntent'

type LoginMethod = 'email' | 'google' | 'passkey'

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
  loginMethods: ['email', 'google', 'passkey'],
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
  const { ready: authReady, authenticated, user, logout } = usePrivy()
  const closeWalletPicker = usePletherWalletPickerClose()
  const { isOpen: authModalOpen } = useModalStatus()
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
  const [googleReturn] = useState(() => isGoogleLoginReturn(selectionKey, selection))
  const loginIntent = useRef<number | undefined>(googleReturn ? signingGuard.revision : undefined)
  const accountIntent = useRef<{ revision: number } | undefined>(undefined)
  const loginVerified = useRef(false)
  const loginStarted = useRef(googleReturn)
  const passkeyPickerClose = useRef<(() => void | Promise<void>) | null>(null)
  const headlessLogin = useRef<'google' | 'passkey' | null>(googleReturn ? 'google' : null)
  const [embeddedWalletRequest, setEmbeddedWalletRequest] = useState<{ userId: string; revision: number } | null>(null)
  const embeddedWalletCreation = useRef<{ userId: string; revision: number } | null>(null)
  const bridgeSelection = useMemo(() => ({ owner: selection, retry: bridgeRevision }), [selection, bridgeRevision])
  const [error, setError] = useState<string | null>(null)
  const ready = authReady && walletsReady
  const session = useRef({ ready, authenticated, userId: user?.id })
  useLayoutEffect(() => { session.current = { ready, authenticated, userId: user?.id } }, [ready, authenticated, user?.id])
  const availableOwner = selectedWallet(wallets, selection)?.address
  useLayoutEffect(() => {
    signingGuard.setReady(ready && availableOwner !== undefined)
  }, [ready, availableOwner, signingGuard])

  const updateSelection = useCallback((address: string | null) => {
    clearGoogleLoginIntent(selectionKey)
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

  const chooseNativeWallet = useCallback((address: string) => {
    const intent = accountIntent.current
    if (intent?.revision !== signingGuard.revision) return
    chooseWallet(address)
    intent.revision = signingGuard.revision
  }, [chooseWallet, signingGuard])

  useEffect(() => {
    if (accountOpen && nativeWallet?.type === 'ethereum') chooseNativeWallet(nativeWallet.address)
  }, [accountOpen, nativeWallet, chooseNativeWallet])

  const selectConnectedOwner = (address: string) => {
    const owner = signingGuard.selection
    // Preserve an available owner when connecting an additional wallet. A
    // successful explicit connection can replace an owner no longer available.
    if (typeof owner !== 'string' || !wallets.some(wallet => wallet.address.toLowerCase() === owner)) {
      updateSelection(address)
    }
  }
  const onWalletConnected = ({ wallet }: { wallet: { address: string } }) => {
    if (connectionIntent.current !== signingGuard.revision) return
    connectionIntent.current = undefined
    selectConnectedOwner(wallet.address)
  }
  const onConnectionError = (code: string) => {
    if (headlessLogin.current === 'passkey') {
      loginIntent.current = undefined
      loginVerified.current = false
      passkeyPickerClose.current = null
      clearGoogleLoginIntent(selectionKey)
    }
    connectionIntent.current = undefined
    // Privy reports closing its wallet picker as generic_connect_wallet_error.
    // The SDK also handles generic connection failures inside that picker.
    const dismissed = code === 'generic_connect_wallet_error'
      || code === 'exited_auth_flow' || code === 'exited_link_flow'
    setError(dismissed ? null : 'Wallet connection could not be completed. Try again.')
  }
  const activateEmbeddedWallet = useCallback((address: string) => {
    if (!loginVerified.current || loginIntent.current !== signingGuard.revision) return
    loginIntent.current = undefined
    loginVerified.current = false
    headlessLogin.current = null
    clearGoogleLoginIntent(selectionKey)
    updateSelection(address)
    const close = passkeyPickerClose.current
    passkeyPickerClose.current = null
    void close?.()
  }, [signingGuard, updateSelection])
  const loginCallbacks: NonNullable<Parameters<typeof useLogin>[0]> = {
    onComplete: ({ user, loginMethod, wasAlreadyAuthenticated }) => {
      // Privy also emits login callbacks while restoring an existing session.
      // The native UI also offers passkeys alongside other login methods.
      // Accept its completed supported method, never a restored session or SIWE.
      if (loginIntent.current !== signingGuard.revision || wasAlreadyAuthenticated
        || !['email', 'google', 'passkey'].some(method => method === loginMethod)) return
      if (headlessLogin.current && loginMethod !== headlessLogin.current) return
      clearGoogleLoginIntent(selectionKey)
      loginVerified.current = true
      const wallet = user.linkedAccounts.filter(account => account.type === 'wallet')
        .find(account => account.chainType === 'ethereum' && account.walletClientType === 'privy')
        ?? (user.wallet?.chainType === 'ethereum' && user.wallet.walletClientType === 'privy' ? user.wallet : undefined)
      if (wallet) activateEmbeddedWallet(wallet.address)
      else if (loginMethod === 'google' || (loginMethod === 'passkey' && headlessLogin.current === 'passkey')) setEmbeddedWalletRequest({ userId: user.id, revision: signingGuard.revision })
    },
    onError: (code) => {
      // Headless passkey errors also arrive here without the original error.
      // Its promise handles cancellation and failures with the full context.
      // Closing a previous picker can report cancellation of an earlier login.
      if (!loginStarted.current || (headlessLogin.current === 'passkey' && !['exited_auth_flow'].includes(code))) return
      if (loginIntent.current === undefined) return
      loginIntent.current = undefined
      loginVerified.current = false
      passkeyPickerClose.current = null
      clearGoogleLoginIntent(selectionKey)
      onConnectionError(code)
    },
  }
  const { login } = useLogin(loginCallbacks)
  const { loginWithPasskey } = useLoginWithPasskey({ onComplete: loginCallbacks.onComplete })
  // Automatic embedded-wallet creation may finish after authentication.
  const { createWallet } = useCreateWallet({ onSuccess: ({ wallet }) => {
    // Explicit headless wallet creation is correlated through its own promise.
    // Global SDK creation events can arrive after the selected owner changes.
    if (headlessLogin.current || embeddedWalletCreation.current) return
    if (wallet.chainType === 'ethereum') activateEmbeddedWallet(wallet.address)
  } })
  const { initOAuth } = useLoginWithOAuth()
  useEffect(() => {
    const request = embeddedWalletRequest
    if (!request || embeddedWalletCreation.current) return
    if (loginIntent.current !== request.revision || signingGuard.revision !== request.revision || !loginVerified.current) return
    // Headless login does not automatically provision an embedded wallet.
    // Wait until the SDK has installed the verified user's session first.
    if (!authenticated || user?.id !== request.userId || !walletsReady) return
    embeddedWalletCreation.current = request
    void createWallet().then(wallet => {
      if (loginIntent.current === request.revision && signingGuard.revision === request.revision
        && session.current.authenticated && session.current.userId === request.userId) activateEmbeddedWallet(wallet.address)
    }).catch(() => {
      if (loginIntent.current !== request.revision || signingGuard.revision !== request.revision
        || !session.current.authenticated || session.current.userId !== request.userId) return
      loginIntent.current = undefined
      loginVerified.current = false
      setError('Signed in, but your wallet could not be prepared. Choose Use email wallet to retry.')
    }).finally(() => {
      if (embeddedWalletCreation.current !== request) return
      embeddedWalletCreation.current = null
      setEmbeddedWalletRequest(pending => pending === request ? null : pending ? { ...pending } : null)
    })
  }, [embeddedWalletRequest, authenticated, user?.id, walletsReady, createWallet, signingGuard, activateEmbeddedWallet])
  const { connectWallet } = useConnectWallet({ onSuccess: onWalletConnected, onError: onConnectionError })

  const { linkGoogle, linkPasskey } = useLinkAccount({
    onSuccess: () => { setError(null) },
    onError: (code: string, details) => {
      if (details.linkMethod !== 'google' && details.linkMethod !== 'passkey') return
      setError(code === 'exited_link_flow' || code === 'exited_auth_flow' ? null
        : 'Could not link this sign-in method. It may already belong to another account. Try again from the original account.')
    },
  })

  const handleLink = async (method: 'google' | 'passkey', close: () => void | Promise<void>) => {
    if (method === 'passkey' && !isPasskeyOriginSupported()) return
    if (!ready || !authenticated) return
    connectionIntent.current = undefined
    loginIntent.current = undefined
    loginVerified.current = false
    headlessLogin.current = null
    accountIntent.current = undefined
    setAccountOpen(false)
    clearGoogleLoginIntent(selectionKey)
    setError(null)
    const revision = signingGuard.revision
    const ownerId = user?.id
    try {
      await close()
      if (signingGuard.revision !== revision || !ownerId || !session.current.ready
        || !session.current.authenticated || session.current.userId !== ownerId) return
      if (method === 'google') linkGoogle()
      else linkPasskey()
    } catch { setError('Could not open account linking. Try again.') }
  }

  const handleEmbeddedWallet = async (close: () => void | Promise<void>, method: LoginMethod = 'email') => {
    if (method === 'passkey' && !isPasskeyOriginSupported()) return
    if (!ready) return
    connectionIntent.current = undefined
    accountIntent.current = undefined
    setAccountOpen(false)
    loginIntent.current = undefined
    loginVerified.current = false
    loginStarted.current = false
    passkeyPickerClose.current = null
    headlessLogin.current = method === 'google' || method === 'passkey' ? method : null
    clearGoogleLoginIntent(selectionKey)
    setError(null)
    const embedded = authenticated ? wallets.find(wallet => wallet.walletClientType === 'privy') : undefined
    if (embedded) {
      chooseWallet(embedded.address)
      await close()
      return
    }
    if (signingGuard.selection === undefined || !selectedWallet(wallets, signingGuard.selection)) updateSelection(null)
    const intent = signingGuard.revision
    loginIntent.current = intent
    loginVerified.current = authenticated
    try {
      if (method !== 'passkey' || authenticated) await close()
      if (loginIntent.current !== intent || signingGuard.revision !== intent) return
      if (authenticated) {
        const linked = user?.linkedAccounts.filter(account => account.type === 'wallet')
          .find(account => account.chainType === 'ethereum' && account.walletClientType === 'privy')
        if (linked) activateEmbeddedWallet(linked.address)
        else {
          const wallet = await createWallet()
          activateEmbeddedWallet(wallet.address)
        }
      } else {
        loginStarted.current = true
        saveGoogleLoginIntent(selectionKey, signingGuard.selection)
        if (method === 'google') await initOAuth({ provider: 'google' })
        else if (method === 'passkey') {
          passkeyPickerClose.current = close
          await loginWithPasskey()
        } else login({ loginMethods: [method] })
      }
    } catch (error) {
      if (loginIntent.current === intent) {
        loginIntent.current = undefined
        loginVerified.current = false
        clearGoogleLoginIntent(selectionKey)
        const cancelled = method === 'passkey' && error !== null && typeof error === 'object'
          && (('privyErrorCode' in error && error.privyErrorCode === 'passkey_not_allowed')
            || ('name' in error && (error.name === 'NotAllowedError' || error.name === 'AbortError')))
        if (method === 'passkey') {
          setError(null)
          passkeyPickerClose.current = null
          connectionIntent.current = signingGuard.revision
          if (!cancelled) return 'Could not sign in with passkey. Try again or use email or Google.'
        } else setError(authenticated ? 'Could not prepare your email wallet. Try again.' : 'Could not start sign-in. Try again.')
      }
    }
  }

  const connectionDescription = (close: () => void | Promise<void>) => (
    <EmailLoginOption label={authenticated ? 'Use email wallet' : 'Log in with Email'}
      onClick={() => { void handleEmbeddedWallet(close) }}
      options={authenticated ? [
        { label: user?.linkedAccounts.some(account => account.type === 'google_oauth') ? 'Google linked' : 'Link Google account',
          disabled: user?.linkedAccounts.some(account => account.type === 'google_oauth'), icon: 'google', onClick: () => { void handleLink('google', close) } },
        { label: 'Add passkey', kind: 'passkey-link', onClick: () => { void handleLink('passkey', close) } },
      ] : [
        { label: 'Log in with Google', icon: 'google', onClick: () => { void handleEmbeddedWallet(close, 'google') } },
        { label: 'Log in with passkey', kind: 'passkey-login', onClick: () => handleEmbeddedWallet(close, 'passkey') },
      ]} />
  )

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
    modalOpen: authModalOpen || accountOpen,
    wallets: wallets.map(wallet => ({
      address: wallet.address,
      name: wallet.walletClientType === 'privy' ? 'Social wallet' : wallet.meta.name,
      embedded: wallet.walletClientType === 'privy',
    })),
    error,
    connect: () => {
      if (!ready) return
      accountIntent.current = undefined
      setAccountOpen(false)
      setError(null)
      loginIntent.current = undefined
      loginVerified.current = false
      clearGoogleLoginIntent(selectionKey)
      connectionIntent.current = signingGuard.revision
      connectWallet({
        description: connectionDescription(closeWalletPicker),
      })
    },
    openAccount: async () => {
      if (!ready || accountIntent.current) return
      setError(null)
      const intent = { revision: signingGuard.revision }
      accountIntent.current = intent
      const owner = selectedWallet(wallets, signingGuard.selection)
      if (owner) setNativeWallet(owner)
      setAccountOpen(true)
      try {
        const result = await openNativeWallets()
        const chosen = result.wallet ?? nativeWalletRef.current
        if (accountIntent.current === intent && chosen?.type === 'ethereum') chooseNativeWallet(chosen.address)
      } catch {
        if (accountIntent.current === intent) setError('Could not open wallet details. Try again.')
      } finally {
        if (accountIntent.current === intent) {
          accountIntent.current = undefined
          setAccountOpen(false)
        }
      }
    },
    selectWallet: chooseWallet,
    disconnect: async () => {
      if (!ready) return
      setError(null)
      connectionIntent.current = undefined
      loginIntent.current = undefined
      accountIntent.current = undefined
      setAccountOpen(false)
      loginVerified.current = false
      clearGoogleLoginIntent(selectionKey)
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
        <WalletsDialog networks={walletChains.filter(chain => chain.id !== 31337).map(chain => ({ id: chain.id }))}
          connectionDescription={connectionDescription} accountActions={<WalletMfaSettings />} />
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
