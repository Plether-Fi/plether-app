import { useAppKit } from '@reown/appkit/react'
import { useEffect, useRef, useState } from 'react'
import { useConnection, useSignMessage, useSwitchChain } from 'wagmi'
import { arbitrumSepolia } from 'wagmi/chains'
import {
  createWalletChallenge,
  verifyRegistrationWallet,
  type RegistrationSession,
} from '../api'
import { walletConnectionConfigured } from '../config/wagmi'
import { shortAddress } from '../utils/format'
import { registrationErrorMessage } from '../utils/registration'

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
  const canonicalAddress = address?.toLowerCase()
  const chainId = chainIdOrZero(connectedChainId)
  const { open } = useAppKit()
  const { mutateAsync: switchChain } = useSwitchChain()
  const { mutateAsync: signMessage } = useSignMessage()
  const [pending, setPending] = useState(false)
  const [error, setError] = useState<string | null>(null)
  const walletContext = useRef({ address, chainId })

  useEffect(() => {
    walletContext.current = { address, chainId }
  }, [address, chainId])

  async function verifyWallet() {
    if (!canonicalAddress) return
    setError(null)
    setPending(true)
    try {
      if (chainId !== arbitrumSepolia.id) {
        await switchChain({ chainId: arbitrumSepolia.id })
        return
      }
      const challenge = await createWalletChallenge(slug, registration.csrfToken, canonicalAddress)
      const signature = await signMessage({ message: challenge.message })
      const currentWallet = walletContext.current
      if (
        currentWallet.chainId !== arbitrumSepolia.id
        || currentWallet.address?.toLowerCase() !== canonicalAddress
      ) {
        throw new Error('The connected wallet or network changed. Start wallet verification again.')
      }
      const nextRegistration = await verifyRegistrationWallet(
        slug,
        registration.csrfToken,
        canonicalAddress,
        signature,
      )
      onVerified(nextRegistration)
    } catch (caught) {
      setError(registrationErrorMessage(caught))
    } finally {
      setPending(false)
    }
  }

  if (!walletConnectionConfigured) {
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

      {isConnected && address ? (
        <div className="flex flex-col gap-3 border border-brand-border/25 bg-app-bg/50 p-4 sm:flex-row sm:items-center sm:justify-between">
          <div>
            <p className="text-xs font-semibold uppercase tracking-wider text-content-tertiary">Connected owner</p>
            <p className="mt-1 font-mono text-sm text-content-primary" title={address}>{shortAddress(address)}</p>
            <p className="mt-1 text-xs text-content-tertiary">
              {chainId === arbitrumSepolia.id ? 'Arbitrum Sepolia' : 'Network switch required'}
            </p>
          </div>
          <button type="button" className={BUTTON_CLASS} disabled={pending} onClick={() => { void verifyWallet() }}>
            {pending ? 'Check your wallet…' : chainId === arbitrumSepolia.id ? 'Sign and verify wallet' : 'Switch to Arbitrum Sepolia'}
          </button>
        </div>
      ) : (
        <button type="button" className={BUTTON_CLASS} onClick={() => { void open({ view: 'Connect' }) }}>
          Connect wallet
        </button>
      )}

      {error ? <p className="border border-brand-orange/40 bg-brand-orange/10 p-3 text-sm text-brand-peach" role="alert">{error}</p> : null}
    </div>
  )
}
