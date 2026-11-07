import { useCallback, useEffect, useLayoutEffect, useMemo, useRef, useState, type ReactNode } from 'react'
import {
  PrivyProvider,
  useConnectWallet,
  useCreateWallet,
  useLogin,
  useLoginWithOAuth,
  useLoginWithPasskey,
  useLinkAccount,
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
import { isPasskeyOriginSupported } from './passkeyOrigin'
import { createSigningGuard } from './signingGuard'
import { clearGoogleLoginIntent, isGoogleLoginReturn, saveGoogleLoginIntent } from './googleLoginIntent'

type LoginMethod = 'email' | 'google' | 'passkey'

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
  const [googleReturn] = useState(() => isGoogleLoginReturn(storageKey, selectedAddress))
  const loginIntent = useRef(googleReturn)
  const loginVerified = useRef(false)
  const loginStarted = useRef(googleReturn)
  const passkeyPickerClose = useRef<(() => void | Promise<void>) | null>(null)
  const headlessLogin = useRef<'google' | 'passkey' | null>(googleReturn ? 'google' : null)
  const [embeddedWalletRequest, setEmbeddedWalletRequest] = useState<{ userId: string; revision: number } | null>(null)
  const embeddedWalletCreation = useRef<{ userId: string; revision: number } | null>(null)
  const connectionIntent = useRef(false)
  const [error, setError] = useState<string | null>(null)
  const session = useRef({ ready: ready && walletsReady, authenticated, userId: user?.id })
  useLayoutEffect(() => {
    session.current = { ready: ready && walletsReady, authenticated, userId: user?.id }
  }, [ready, walletsReady, authenticated, user?.id])
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
    clearGoogleLoginIntent(storageKey)
    signingGuard.select(address)
    connectionIntent.current = false
    loginIntent.current = false
    loginVerified.current = false
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
    if (!connectionIntent.current || loginIntent.current) return
    connectionIntent.current = false
    selectConnectedOwner(wallet.address)
  }, [selectConnectedOwner])
  const onConnectError = useCallback((code: string) => {
    if (headlessLogin.current === 'passkey') {
      loginIntent.current = false
      loginVerified.current = false
      passkeyPickerClose.current = null
      clearGoogleLoginIntent(storageKey)
    }
    connectionIntent.current = false
    // Closing Privy's wallet picker uses the same generic code as failures
    // already displayed inside the picker; don't persist those below the CTA.
    const dismissed = code === 'generic_connect_wallet_error'
      || code === 'exited_auth_flow' || code === 'exited_link_flow'
    setError(dismissed ? null : 'Wallet connection could not be completed. Try again.')
  }, [storageKey])
  const activateEmbeddedWallet = useCallback((address: string) => {
    if (!loginIntent.current || !loginVerified.current) return
    loginIntent.current = false
    loginVerified.current = false
    headlessLogin.current = null
    // Authentication was explicitly chosen, so it may replace an external owner.
    selectWallet(address)
    const close = passkeyPickerClose.current
    passkeyPickerClose.current = null
    void close?.()
  }, [selectWallet])
  const loginCallbacks: NonNullable<Parameters<typeof useLogin>[0]> = {
    onComplete: ({ user, loginMethod, wasAlreadyAuthenticated }) => {
      if (!loginIntent.current || wasAlreadyAuthenticated
        || !['email', 'google', 'passkey'].some(method => method === loginMethod)) return
      if (headlessLogin.current && loginMethod !== headlessLogin.current) return
      clearGoogleLoginIntent(storageKey)
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
      if (!loginIntent.current) return
      loginIntent.current = false
      loginVerified.current = false
      passkeyPickerClose.current = null
      clearGoogleLoginIntent(storageKey)
      onConnectError(code)
    },
  }
  const { login } = useLogin(loginCallbacks)
  const { loginWithPasskey } = useLoginWithPasskey({ onComplete: loginCallbacks.onComplete })
  const { createWallet } = useCreateWallet({ onSuccess: ({ wallet }) => {
    if (headlessLogin.current || embeddedWalletCreation.current) return
    if (wallet.chainType === 'ethereum') activateEmbeddedWallet(wallet.address)
  } })
  const { initOAuth } = useLoginWithOAuth()
  useEffect(() => {
    const request = embeddedWalletRequest
    if (!request || embeddedWalletCreation.current) return
    if (!loginIntent.current || signingGuard.revision !== request.revision || !loginVerified.current) return
    if (!authenticated || user?.id !== request.userId || !walletsReady) return
    embeddedWalletCreation.current = request
    void createWallet().then(wallet => {
      if (loginIntent.current && signingGuard.revision === request.revision
        && session.current.authenticated && session.current.userId === request.userId) activateEmbeddedWallet(wallet.address)
    }).catch(() => {
      if (!loginIntent.current || signingGuard.revision !== request.revision
        || !session.current.authenticated || session.current.userId !== request.userId) return
      loginIntent.current = false
      loginVerified.current = false
      setError('Signed in, but your wallet could not be prepared. Choose Use email wallet to retry.')
    }).finally(() => {
      if (embeddedWalletCreation.current !== request) return
      embeddedWalletCreation.current = null
      setEmbeddedWalletRequest(pending => pending === request ? null : pending ? { ...pending } : null)
    })
  }, [embeddedWalletRequest, authenticated, user?.id, walletsReady, createWallet, signingGuard, activateEmbeddedWallet])
  const { connectWallet } = useConnectWallet({ onSuccess: onConnected, onError: onConnectError })
  const { linkGoogle, linkPasskey } = useLinkAccount({
    onSuccess: () => { setError(null) },
    onError: (code: string, details) => {
      if (details.linkMethod !== 'google' && details.linkMethod !== 'passkey') return
      setError(code === 'exited_link_flow' || code === 'exited_auth_flow' ? null
        : 'Could not link this sign-in method. It may already belong to another account. Try again from the original account.')
    },
  })
  const openLink = async (method: 'google' | 'passkey') => {
    if (method === 'passkey' && !isPasskeyOriginSupported()) return
    if (!ready || !walletsReady || !authenticated) return
    connectionIntent.current = false
    loginIntent.current = false
    loginVerified.current = false
    headlessLogin.current = null
    clearGoogleLoginIntent(storageKey)
    setError(null)
    const revision = signingGuard.revision
    const ownerId = user?.id
    try {
      await closeWalletPicker()
      if (signingGuard.revision !== revision || !ownerId || !session.current.ready
        || !session.current.authenticated || session.current.userId !== ownerId) return
      if (method === 'google') linkGoogle()
      else linkPasskey()
    } catch { setError('Could not open account linking. Try again.') }
  }
  const openEmbeddedWallet = async (method: LoginMethod = 'email') => {
    if (method === 'passkey' && !isPasskeyOriginSupported()) return
    connectionIntent.current = false
    loginVerified.current = false
    loginStarted.current = false
    passkeyPickerClose.current = null
    headlessLogin.current = method === 'google' || method === 'passkey' ? method : null
    clearGoogleLoginIntent(storageKey)
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
    loginIntent.current = true
    // An existing authenticated session needs wallet creation, not another
    // login flow (which Privy skips for an authenticated user).
    loginVerified.current = authenticated
    const revision = signingGuard.revision
    const isCurrentRequest = () => loginIntent.current && signingGuard.revision === revision
    try {
      if (method !== 'passkey' || authenticated) await closeWalletPicker()
      if (!isCurrentRequest()) return
      if (!authenticated) {
        loginStarted.current = true
        saveGoogleLoginIntent(storageKey, signingGuard.selection)
        if (method === 'google') await initOAuth({ provider: 'google' })
        else if (method === 'passkey') {
          passkeyPickerClose.current = closeWalletPicker
          await loginWithPasskey()
        } else login({ loginMethods: [method] })
        return
      }
      const wallet = await createWallet()
      if (isCurrentRequest()) activateEmbeddedWallet(wallet.address)
    } catch (error) {
      if (!isCurrentRequest()) return
      loginIntent.current = false
      loginVerified.current = false
      clearGoogleLoginIntent(storageKey)
      const cancelled = method === 'passkey' && error !== null && typeof error === 'object'
        && (('privyErrorCode' in error && error.privyErrorCode === 'passkey_not_allowed')
          || ('name' in error && (error.name === 'NotAllowedError' || error.name === 'AbortError')))
      if (method === 'passkey') {
        setError(null)
        passkeyPickerClose.current = null
        connectionIntent.current = true
        if (!cancelled) return 'Could not sign in with passkey. Try again or use email or Google.'
      } else setError(authenticated ? 'Could not prepare your email wallet. Try again.' : 'Could not start sign-in. Try again.')
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

  const connectionDescription = () => (
    <EmailLoginOption label={authenticated ? 'Use email wallet' : 'Log in with Email'} onClick={() => { void openEmbeddedWallet() }}
      options={authenticated ? [
        { label: user?.linkedAccounts.some(account => account.type === 'google_oauth') ? 'Google linked' : 'Link Google account',
          disabled: user?.linkedAccounts.some(account => account.type === 'google_oauth'), icon: 'google', onClick: () => { void openLink('google') } },
        { label: 'Add passkey', kind: 'passkey-link', onClick: () => { void openLink('passkey') } },
      ] : [
        { label: 'Log in with Google', icon: 'google', onClick: () => { void openEmbeddedWallet('google') } },
        { label: 'Log in with passkey', kind: 'passkey-login', onClick: () => openEmbeddedWallet('passkey') },
      ]} />
  )

  const actions = {
    configured: true,
    ready: ready && walletsReady,
    connect: () => {
      if (!ready || !walletsReady) return
      setError(null)
      loginIntent.current = false
      loginVerified.current = false
      clearGoogleLoginIntent(storageKey)
      connectionIntent.current = true
      connectWallet({
        description: connectionDescription(),
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
