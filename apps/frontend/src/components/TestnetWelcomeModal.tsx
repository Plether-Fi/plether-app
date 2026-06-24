import { useState } from 'react'
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
      title="Welcome to Sepolia (testnet)"
      size="lg"
      bodyClassName="p-0"
    >
      <div className="space-y-5 p-6 text-sm text-cyber-text-secondary">
        <p>
          This deployment uses testnet contracts and mock assets. Balances and positions here
          have no real-world value.
        </p>
        <p>
          Enter a wallet address to receive 100,000 mock USDC for testing Plether flows on
          Sepolia.
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
          <p className="border border-cyber-electric-fuchsia/40 bg-cyber-electric-fuchsia/10 px-4 py-3 text-sm text-cyber-electric-fuchsia">
            {submitError}
          </p>
        ) : null}

        {claim ? (
          <div className="space-y-2 border border-cyber-neon-green/40 bg-cyber-neon-green/10 px-4 py-3 text-sm text-cyber-text-primary">
            <p className="font-medium">
              {claim.status === 'already_claimed'
                ? 'This wallet has already claimed mock USDC.'
                : 'Mock USDC request submitted.'}
            </p>
            <a
              href={`https://sepolia.etherscan.io/tx/${claim.txHash}`}
              target="_blank"
              rel="noreferrer"
              className="break-all text-cyber-neon-green hover:underline"
            >
              {claim.txHash}
            </a>
          </div>
        ) : null}
      </div>

      <div className="flex flex-col gap-3 border-t border-cyber-border-glow/30 px-6 py-4 sm:flex-row">
        <Button
          type="button"
          onClick={() => { void requestFunds() }}
          isLoading={isSubmitting}
          disabled={!!claim}
          className="w-full"
        >
          Send mock USDC
        </Button>
        <Button
          type="button"
          variant="secondary"
          onClick={dismiss}
          className="w-full"
        >
          {claim ? 'Continue' : 'Cancel'}
        </Button>
      </div>
    </Modal>
  )
}
