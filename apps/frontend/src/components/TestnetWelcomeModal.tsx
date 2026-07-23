import { useEffect, useRef, useState } from 'react'
import { useAppKit } from '@reown/appkit/react'
import { Result } from 'better-result'
import { isAddress } from 'viem'
import { useAccount } from 'wagmi'
import { useNavigate } from 'react-router-dom'
import { perpsApi } from '../api'
import type { TestnetFaucetClaim } from '../api/types'
import { syncAppKitModalStyleOverrides } from '../config/wagmi'
import { usePerpsUiStore } from '../stores/perpsUiStore'
import { useSettingsStore } from '../stores/settingsStore'
import { usePerpsIdentity } from '../perps-aa'
import { Button, Input, Modal } from './ui'

interface TestnetWelcomeModalViewProps {
  isOpen: boolean
  isWalletConnected: boolean
  walletAddress: string
  fieldError?: string
  submitError?: string
  recipientError?: string
  claim?: TestnetFaucetClaim | null
  isSubmitting?: boolean
  isTradingAccountRecipient?: boolean
  onClose: () => void
  onConnectWallet: () => void
  onWalletAddressChange: (address: string) => void
  onRequestFunds: () => void
  onDeposit?: () => void
}

export function TestnetWelcomeModalView({
  isOpen,
  isWalletConnected,
  walletAddress,
  fieldError,
  submitError,
  recipientError,
  claim,
  isSubmitting = false,
  isTradingAccountRecipient = false,
  onClose,
  onConnectWallet,
  onWalletAddressChange,
  onRequestFunds,
  onDeposit,
}: TestnetWelcomeModalViewProps) {
  const activeClaim = isWalletConnected ? claim : null
  const handleSecondaryAction = activeClaim && onDeposit ? onDeposit : onClose
  const isRecipientReady =
    isWalletConnected &&
    (!isTradingAccountRecipient || walletAddress.trim().length > 0) &&
    !recipientError
  const isPreparingRecipient =
    isWalletConnected &&
    isTradingAccountRecipient &&
    !isRecipientReady &&
    !recipientError

  return (
    <Modal
      isOpen={isOpen}
      onClose={onClose}
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
          {!isWalletConnected
            ? `Connect your wallet to continue. Once connected, we will fill your ${isTradingAccountRecipient ? 'Plether Trading Account' : 'wallet'} address so you can request 100,000 mock USDC on Arbitrum Sepolia.`
            : recipientError
              ? 'Your wallet is connected, but Plether could not prepare the Trading Account address that should receive the test funds.'
            : isRecipientReady
              ? `Your ${isTradingAccountRecipient ? 'Plether Trading Account' : 'wallet'} address is shown below. We will send it 100,000 mock USDC on Arbitrum Sepolia to start testing.`
              : 'Your wallet is connected. Plether is preparing the Trading Account address that will receive the test funds.'}
        </p>
        <p>
          Testnet balances and positions have no real-world value and could be reset at any time.
          {isTradingAccountRecipient
            ? ' Eligible trader actions are gas-sponsored while sponsorship is available.'
            : ' Use a wallet that supports Arbitrum Sepolia because it will pay network gas.'}
        </p>
        <p>
          Thanks for being here early. This is a safe testnet environment, so try things freely.
          Nothing here has real-world value, and every bit of feedback helps.
        </p>

        {isRecipientReady ? (
          <Input
            label={isTradingAccountRecipient ? 'Trading Account address' : 'Wallet address'}
            value={walletAddress}
            onChange={(event) => {
              onWalletAddressChange(event.target.value)
            }}
            placeholder="0x..."
            disabled={isTradingAccountRecipient}
            error={fieldError}
            spellCheck={false}
            autoComplete="off"
          />
        ) : null}

        {recipientError ? (
          <p className="border border-brand-orange/40 bg-brand-orange/10 px-4 py-3 text-sm text-brand-orange">
            {recipientError}
          </p>
        ) : null}

        {isRecipientReady && submitError ? (
          <p className="border border-brand-orange/40 bg-brand-orange/10 px-4 py-3 text-sm text-brand-orange">
            {submitError}
          </p>
        ) : null}

        {activeClaim ? (
          <div className="space-y-2 border border-positive/40 bg-positive/10 px-4 py-3 text-sm text-content-primary">
            <p className="font-medium">
              {activeClaim.status === 'already_funded'
                ? `Mock USDC is already available for this ${isTradingAccountRecipient ? 'Trading Account' : 'wallet'}.`
                : activeClaim.status === 'already_claimed'
                  ? `Mock USDC was already claimed for this ${isTradingAccountRecipient ? 'Trading Account' : 'wallet'}.`
                  : `Mock USDC minted to your ${isTradingAccountRecipient ? 'Trading Account' : 'wallet'}.`}
            </p>
            <p className="text-content-secondary">
              {isTradingAccountRecipient
                ? 'Next, use the sponsored deposit flow to move those funds into the Trading Account’s Margin Account before placing orders.'
                : 'Next, deposit those funds into the exchange margin account before placing orders.'}
            </p>
            {isTradingAccountRecipient ? (
              <p className="text-content-secondary">
                Plether sponsors eligible perps network gas, so the owner wallet and Trading Account do not need Arbitrum Sepolia ETH for the sponsored journey.
              </p>
            ) : (
              <p className="text-content-secondary">
                You also need some Arbitrum Sepolia ETH to pay transaction fees.{' '}
                <a
                  href="https://www.alchemy.com/faucets/arbitrum-sepolia"
                  target="_blank"
                  rel="noreferrer"
                  className="text-positive hover:underline"
                >
                  Get Arbitrum Sepolia ETH from Alchemy.
                </a>
              </p>
            )}
            {activeClaim.txHash ? (
              <a
                href={`https://sepolia.arbiscan.io/tx/${activeClaim.txHash}`}
                target="_blank"
                rel="noreferrer"
                className="break-all text-positive hover:underline"
              >
                {activeClaim.txHash}
              </a>
            ) : null}
          </div>
        ) : null}
      </div>

      <div className="flex flex-col gap-3 border-t border-brand-border/30 px-6 py-4 sm:flex-row">
        <Button
          type="button"
          variant={!isWalletConnected ? 'danger' : isRecipientReady ? 'primary' : 'secondary'}
          onClick={isWalletConnected ? onRequestFunds : onConnectWallet}
          isLoading={isRecipientReady ? isSubmitting : isPreparingRecipient}
          disabled={
            isWalletConnected &&
            (!isRecipientReady || !!activeClaim)
          }
          className="w-full"
        >
          {isRecipientReady ? (
            'Get 100,000 mock USDC'
          ) : isWalletConnected ? (
            recipientError ? 'Trading Account unavailable' : 'Preparing Trading Account'
          ) : (
            <>
              <span aria-hidden="true" className="material-symbols-outlined text-xl">
                account_balance_wallet
              </span>
              Connect Wallet
            </>
          )}
        </Button>
        <Button
          type="button"
          variant="secondary"
          onClick={handleSecondaryAction}
          className="w-full"
        >
          {activeClaim ? 'Deposit' : 'Maybe later'}
        </Button>
      </div>
    </Modal>
  )
}

export function TestnetWelcomeModal() {
  const { address: connectedAddress, isConnected } = useAccount()
  const { open } = useAppKit()
  const perpsIdentity = usePerpsIdentity()
  const faucetRecipient = perpsIdentity.isAaManifestConfigured
    ? perpsIdentity.accountAddress
    : connectedAddress
  const navigate = useNavigate()
  const dismissed = useSettingsStore((s) => s.sepoliaWelcomeDismissed)
  const dismiss = useSettingsStore((s) => s.dismissSepoliaWelcome)
  const requestMarginAction = usePerpsUiStore((s) => s.requestMarginAction)
  const [walletAddress, setWalletAddress] = useState(faucetRecipient ?? '')
  const [fieldError, setFieldError] = useState<string | null>(null)
  const [submitError, setSubmitError] = useState<string | null>(null)
  const [claim, setClaim] = useState<TestnetFaucetClaim | null>(null)
  const [isSubmitting, setIsSubmitting] = useState(false)
  const previousConnectedAddressRef = useRef<string | undefined>(faucetRecipient)
  const displayedWalletAddress = perpsIdentity.isAaManifestConfigured
    ? faucetRecipient ?? ''
    : walletAddress
  const displayedClaim =
    claim?.address.toLowerCase() === displayedWalletAddress.toLowerCase() ? claim : null
  const recipientError =
    isConnected &&
    perpsIdentity.isAaManifestConfigured &&
    perpsIdentity.status !== 'ready' &&
    perpsIdentity.status !== 'loading' &&
    perpsIdentity.status !== 'disconnected'
      ? perpsIdentity.error?.message ??
        'The Trading Account configuration needs attention before testing can continue.'
      : undefined

  useEffect(() => {
    if (!faucetRecipient) return

    setWalletAddress((currentAddress) => {
      const previousConnectedAddress = previousConnectedAddressRef.current
      const shouldUseConnectedAddress =
        currentAddress.trim().length === 0 ||
        (previousConnectedAddress !== undefined && currentAddress === previousConnectedAddress)

      return shouldUseConnectedAddress ? faucetRecipient : currentAddress
    })
    previousConnectedAddressRef.current = faucetRecipient
  }, [faucetRecipient])

  async function requestFunds() {
    const trimmedAddress = displayedWalletAddress.trim()
    setSubmitError(null)

    if (!isAddress(trimmedAddress)) {
      setFieldError('Enter a valid wallet address.')
      return
    }

    setFieldError(null)
    setIsSubmitting(true)
    const result = await perpsApi.claimTestnetFaucet(trimmedAddress)
    setIsSubmitting(false)

    if (Result.isError(result)) {
      setSubmitError(result.error.message)
      return
    }

    setClaim(result.value.data)
  }

  return (
    <TestnetWelcomeModalView
      isOpen={!dismissed}
      isWalletConnected={isConnected}
      onClose={dismiss}
      onConnectWallet={() => {
        setFieldError(null)
        setSubmitError(null)
        setClaim(null)
        syncAppKitModalStyleOverrides()
        void open()
        syncAppKitModalStyleOverrides()
      }}
      walletAddress={displayedWalletAddress}
      fieldError={fieldError ?? undefined}
      submitError={submitError ?? undefined}
      recipientError={recipientError}
      claim={displayedClaim}
      isSubmitting={isSubmitting}
      isTradingAccountRecipient={perpsIdentity.isAaManifestConfigured}
      onWalletAddressChange={(nextAddress) => {
        if (perpsIdentity.isAaManifestConfigured) return
        setWalletAddress(nextAddress)
        setFieldError(null)
        setSubmitError(null)
        setClaim(null)
      }}
      onRequestFunds={() => { void requestFunds() }}
      onDeposit={() => {
        dismiss()
        requestMarginAction('deposit')
        void navigate('/')
      }}
    />
  )
}
