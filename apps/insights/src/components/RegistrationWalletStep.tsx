import { useEffect, useRef, useState } from 'react'
import type { Address } from 'viem'
import { useConnection, useSignMessage, useSwitchChain } from 'wagmi'
import { arbitrumSepolia } from 'wagmi/chains'
import {
  createWalletChallenge,
  getRegistrationSession,
  verifyRegistrationWallet,
  type RegistrationSession,
} from '../api'
import { useRegistrationWallet } from '../wallet/useRegistrationWallet'
import { shortAddress } from '../utils/format'
import { registrationErrorMessage, registrationNeedsRecovery } from '../utils/registration'

const BUTTON_CLASS = 'border border-brand-orange bg-brand-orange px-4 py-2.5 text-sm font-semibold text-content-primary transition-colors hover:bg-brand-peach hover:text-app-bg disabled:cursor-not-allowed disabled:opacity-50'

function chainIdOrZero(value: number | undefined): number {
  return value ?? 0
}

export function RegistrationWalletStep({
  slug,
  registration,
  onVerified,
}: {
  slug: string
  registration: RegistrationSession
  onVerified: (registration: RegistrationSession) => void
}) {
  const { address, chainId: connectedChainId, isConnected } = useConnection()
  const canonicalAddress = address?.toLowerCase() as Address | undefined
  const chainId = chainIdOrZero(connectedChainId)
  const wallet = useRegistrationWallet()
  const { mutateAsync: switchChain } = useSwitchChain()
  const { mutateAsync: signMessage } = useSignMessage()
  const [pending, setPending] = useState(false)
  const [error, setError] = useState<string | null>(null)
  const [needsRecovery, setNeedsRecovery] = useState(false)

  async function recoverSession() {
    setPending(true)
    try {
      onVerified(await getRegistrationSession(slug))
      setNeedsRecovery(false)
    } catch {
      setNeedsRecovery(true)
      setError('Your session could not be checked. Check it again before retrying wallet verification.')
    } finally {
      setPending(false)
    }
  }
  const walletContext = useRef({ address, chainId, isConnected, ready: wallet.ready, isSelecting: wallet.isSelecting })

  useEffect(() => {
    walletContext.current = { address, chainId, isConnected, ready: wallet.ready, isSelecting: wallet.isSelecting }
  }, [address, chainId, isConnected, wallet.ready, wallet.isSelecting])

  async function verifyWallet() {
    if (!canonicalAddress || !isConnected || !wallet.ready || wallet.isSelecting) return
    setError(null)
    setPending(true)
    try {
      if (chainId !== arbitrumSepolia.id) {
        await switchChain({ chainId: arbitrumSepolia.id })
        return
      }
      const challenge = await createWalletChallenge(slug, registration.csrfToken, canonicalAddress)
      const assertCurrentWallet = () => {
        const currentWallet = walletContext.current
        if (
          !currentWallet.isConnected
          || !currentWallet.ready
          || currentWallet.isSelecting
          || currentWallet.chainId !== arbitrumSepolia.id
          || currentWallet.address?.toLowerCase() !== canonicalAddress
        ) {
          throw new Error('The connected wallet or network changed. Start wallet verification again.')
        }
      }
      assertCurrentWallet()
      const signature = await signMessage({ account: canonicalAddress, message: challenge.message })
      assertCurrentWallet()
      const nextRegistration = await verifyRegistrationWallet(
        slug,
        registration.csrfToken,
        canonicalAddress,
        signature.toLowerCase(),
      )
      onVerified(nextRegistration)
    } catch (caught) {
      setError(registrationErrorMessage(caught))
      if (registrationNeedsRecovery(caught)) await recoverSession()
    } finally {
      setPending(false)
    }
  }

  if (!wallet.configured) {
    return (
      <p className="border border-brand-orange/40 bg-brand-orange/10 p-3 text-sm text-brand-peach" role="alert">
        Wallet connection is not configured for this deployment.
      </p>
    )
  }

  return (
    <div className="space-y-4">
      <p className="text-sm leading-6 text-content-secondary">
        Connect the owner wallet you will use for Plether. You will sign a five-minute, one-time message; this does not submit a transaction or spend funds.
      </p>

      {!wallet.ready ? <p className="text-sm text-content-secondary" role="status">Loading wallets…</p> : null}

      {wallet.wallets.length > 0 ? (
        <div className="space-y-2">
          <label className="block text-xs font-semibold uppercase tracking-wider text-content-tertiary" htmlFor="registration-wallet-owner">Owner wallet</label>
          <select
            id="registration-wallet-owner"
            className="w-full border border-brand-border/25 bg-app-bg px-3 py-2.5 text-sm text-content-primary"
            value={isConnected && canonicalAddress ? canonicalAddress : ''}
            disabled={pending || needsRecovery || !wallet.ready}
            onChange={(event) => { wallet.selectWallet(event.target.value) }}
          >
            <option value="" disabled>Choose a wallet</option>
            {wallet.wallets.map((choice) => (
              <option key={choice.address.toLowerCase()} value={choice.address.toLowerCase()}>{choice.label} · {choice.address}</option>
            ))}
          </select>
          <p className="text-xs text-content-tertiary">Choose the same owner wallet when opening the trading app.</p>
        </div>
      ) : null}

      {isConnected && address ? (
        <div className="flex flex-col gap-3 border border-brand-border/25 bg-app-bg/50 p-4 sm:flex-row sm:items-center sm:justify-between">
          <div>
            <p className="text-xs font-semibold uppercase tracking-wider text-content-tertiary">Connected owner</p>
            <p className="mt-1 font-mono text-sm text-content-primary" title={address}>{shortAddress(address)}</p>
            <p className="mt-1 text-xs text-content-tertiary">
              {chainId === arbitrumSepolia.id ? 'Arbitrum Sepolia' : 'Network switch required'}
            </p>
          </div>
          <button type="button" className={BUTTON_CLASS} disabled={pending || needsRecovery || !wallet.ready || wallet.isSelecting} onClick={() => { void verifyWallet() }}>
            {pending ? 'Check your wallet…' : chainId === arbitrumSepolia.id ? 'Sign and verify wallet' : 'Switch to Arbitrum Sepolia'}
          </button>
        </div>
      ) : (
        <button type="button" className={BUTTON_CLASS} disabled={!wallet.ready || pending || needsRecovery} onClick={wallet.connect}>
          Connect wallet
        </button>
      )}

      {isConnected ? (
        <button type="button" className={BUTTON_CLASS} disabled={pending || needsRecovery || !wallet.ready} onClick={wallet.connect}>
          Use another wallet
        </button>
      ) : null}

      {needsRecovery ? <button type="button" className={BUTTON_CLASS} disabled={pending} onClick={() => { void recoverSession() }}>Check registration session</button> : null}
      {wallet.error ? <p className="border border-brand-orange/40 bg-brand-orange/10 p-3 text-sm text-brand-peach" role="alert">{wallet.error}</p> : null}
      {error ? <p className="border border-brand-orange/40 bg-brand-orange/10 p-3 text-sm text-brand-peach" role="alert">{error}</p> : null}
    </div>
  )
}
