import { useEffect, useRef, useState } from 'react'
import { Result } from 'better-result'
import { isAddress } from 'viem'
import { useAccount } from 'wagmi'
import { plethApi } from '../api'
import type { TestnetFaucetClaim } from '../api/types'
import { useSettingsStore } from '../stores/settingsStore'
import { Button, Input, Modal } from './ui'

export function TestnetWelcomeModal() {
  const { address: connectedAddress } = useAccount()
  const dismissed = useSettingsStore((s) => s.sepoliaWelcomeDismissed)
  const dismiss = useSettingsStore((s) => s.dismissSepoliaWelcome)
  const [walletAddress, setWalletAddress] = useState(connectedAddress ?? '')
  const [fieldError, setFieldError] = useState<string | null>(null)
  const [submitError, setSubmitError] = useState<string | null>(null)
  const [claim, setClaim] = useState<TestnetFaucetClaim | null>(null)
  const [isSubmitting, setIsSubmitting] = useState(false)
  const previousConnectedAddressRef = useRef<string | undefined>(connectedAddress)

  useEffect(() => {
    if (!connectedAddress) return

    setWalletAddress((currentAddress) => {
      const previousConnectedAddress = previousConnectedAddressRef.current
      const shouldUseConnectedAddress =
        currentAddress.trim().length === 0 ||
        (previousConnectedAddress !== undefined && currentAddress === previousConnectedAddress)

      return shouldUseConnectedAddress ? connectedAddress : currentAddress
    })
    previousConnectedAddressRef.current = connectedAddress
  }, [connectedAddress])

  async function requestFunds() {
    const trimmedAddress = walletAddress.trim()
    setSubmitError(null)

    if (!isAddress(trimmedAddress)) {
      setFieldError('Enter a valid wallet address.')
      return
    }

    setFieldError(null)
    setIsSubmitting(true)
    const result = await plethApi.claimTestnetFaucet(trimmedAddress)
    setIsSubmitting(false)

    if (Result.isError(result)) {
      setSubmitError(result.error.message)
      return
    }

    setClaim(result.value.data)
  }

  return (
    <Modal
      isOpen={!dismissed}
      onClose={dismiss}
      title="Welcome to Plether on Sepolia"
      size="lg"
      bodyClassName="p-0"
    >
      <div className="space-y-5 p-6 text-sm text-content-secondary">
        <p>
          This is a testnet version of Plether Perps, built for trying deposits, trades, and order
          execution without real funds.
        </p>
        <p>
          Enter your wallet address and we will send you 100,000 mock USDC on Arbitrum Sepolia to
          start testing. Testnet balances and positions have no real-world value and could be reset
          at any time.
        </p>
        <p>
          Use a wallet that supports Arbitrum Sepolia. You will need that network for trading.
        </p>
        <p>
          Thanks for being here early. This is a safe testnet environment, so try things freely.
          Nothing here has real-world value, and every bit of feedback helps.
        </p>

        <Input
          label="Wallet address"
          value={walletAddress}
          onChange={(event) => {
            setWalletAddress(event.target.value)
            setFieldError(null)
            setSubmitError(null)
            setClaim(null)
          }}
          placeholder="0x..."
          error={fieldError ?? undefined}
          spellCheck={false}
          autoComplete="off"
        />

        {submitError ? (
          <p className="border border-brand-orange/40 bg-brand-orange/10 px-4 py-3 text-sm text-brand-orange">
            {submitError}
          </p>
        ) : null}

        {claim ? (
          <div className="space-y-2 border border-positive/40 bg-positive/10 px-4 py-3 text-sm text-content-primary">
            <p className="font-medium">
              {claim.status === 'already_claimed'
                ? 'Mock USDC already claimed for this wallet.'
                : 'Mock USDC sent. You are ready to start testing.'}
            </p>
            <a
              href={`https://sepolia.arbiscan.io/tx/${claim.txHash}`}
              target="_blank"
              rel="noreferrer"
              className="break-all text-positive hover:underline"
            >
              {claim.txHash}
            </a>
          </div>
        ) : null}
      </div>

      <div className="flex flex-col gap-3 border-t border-brand-border/30 px-6 py-4 sm:flex-row">
        <Button
          type="button"
          onClick={() => { void requestFunds() }}
          isLoading={isSubmitting}
          disabled={!!claim}
          className="w-full"
        >
          Get 100,000 mock USDC
        </Button>
        <Button
          type="button"
          variant="secondary"
          onClick={dismiss}
          className="w-full"
        >
          {claim ? 'Continue' : 'Maybe later'}
        </Button>
      </div>
    </Modal>
  )
}
