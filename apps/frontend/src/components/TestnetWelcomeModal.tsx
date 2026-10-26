import { PERPS_RELEASE_IS_DEPLOYED, PERPS_SETTLEMENT_TOKEN, PERPS_SETTLEMENT_TOKEN_LABEL } from '../contracts/perpsAddresses'
import { useEffect, useRef, useState } from 'react'
import { isAddress } from 'viem'
import { useAccount } from 'wagmi'
import { useNavigate } from 'react-router-dom'
import type { TestnetFaucetClaim } from '../api/types'
import { openAppKit } from '../config/wagmi'
import { usePerpsUiStore } from '../stores/perpsUiStore'
import { useSettingsStore } from '../stores/settingsStore'
import { usePerpsIdentity } from '../perps-aa'
import { useTestnetFunding, type TestnetFundingPhase } from '../hooks/useTestnetFunding'
import { Button, Input, Modal } from './ui'

const DIRECT_MOCK_MINT = PERPS_SETTLEMENT_TOKEN.isMock && PERPS_SETTLEMENT_TOKEN.name === 'Mock USDG'
const MOCK_TOKEN_LABEL = PERPS_SETTLEMENT_TOKEN_LABEL

interface TestnetWelcomeModalViewProps {
  isOpen: boolean
  isWalletConnected: boolean
  walletAddress: string
  fieldError?: string
  submitError?: string
  recipientError?: string
  claim?: TestnetFaucetClaim | null
  isSubmitting?: boolean
  fundingPhase?: TestnetFundingPhase
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
  fundingPhase = 'idle',
  isTradingAccountRecipient = false,
  onClose,
  onConnectWallet,
  onWalletAddressChange,
  onRequestFunds,
  onDeposit,
}: TestnetWelcomeModalViewProps) {
  const activeClaim = isWalletConnected ? claim : null
  const isPendingClaim = activeClaim?.status === 'submitted'
  const isCompletedClaim = !!activeClaim && !isPendingClaim
  const isAutomaticFunding = isTradingAccountRecipient && isSubmitting
  const isDeposited = fundingPhase === 'deposited'
  const handleSecondaryAction = isCompletedClaim && !isSubmitting && onDeposit ? onDeposit : onClose
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
      footer={
        <div className="flex flex-col gap-3 sm:flex-row">
          <Button
            type="button"
            variant={!isWalletConnected ? 'danger' : isRecipientReady ? 'primary' : 'secondary'}
            onClick={isWalletConnected ? onRequestFunds : onConnectWallet}
            isLoading={isRecipientReady ? isSubmitting : isPreparingRecipient}
            disabled={
              isWalletConnected &&
              (!isRecipientReady || (!DIRECT_MOCK_MINT && (isDeposited || (!!activeClaim && !isPendingClaim && !submitError))))
            }
            className="w-full"
          >
            {isRecipientReady ? (
              fundingPhase === 'waiting' ? `Waiting for ${MOCK_TOKEN_LABEL}`
                : fundingPhase === 'depositing' ? `Depositing ${MOCK_TOKEN_LABEL}`
                : isDeposited && !DIRECT_MOCK_MINT ? `${MOCK_TOKEN_LABEL} deposited`
                : isPendingClaim ? 'Check confirmation' : `Get 100,000 ${MOCK_TOKEN_LABEL}`
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
            {isDeposited ? 'Start trading' : isCompletedClaim && !isSubmitting ? 'Deposit' : activeClaim || isSubmitting ? 'Close' : 'Maybe later'}
          </Button>
        </div>
      }
    >
      <div className="space-y-5 panel-padding text-sm text-content-secondary">
        <p>Try deposits and trades on Arbitrum Sepolia with test USDG.</p>
        <p>
          {!isWalletConnected
            ? 'Connect your wallet to get 100,000 USDG.'
            : recipientError
              ? 'We could not prepare your Trading Account. Please refresh and try again.'
            : isRecipientReady
              ? `We will send 100,000 USDG to your ${isTradingAccountRecipient ? 'Trading Account and deposit it into your Margin Account' : 'wallet'}. Confirm in your wallet when prompted.`
              : 'Preparing your Trading Account to receive USDG.'}
        </p>
        {DIRECT_MOCK_MINT ? (
          <p>
            You can get more USDG anytime. Your wallet needs Arbitrum Sepolia ETH for the mint transaction.
            {isTradingAccountRecipient ? ' Plether covers gas for the deposit.' : ''}
          </p>
        ) : null}
        <p>Test funds have no real value. Balances and positions may be reset.</p>

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
          <div
            className={`space-y-2 border px-4 py-3 text-sm text-content-primary ${
              isPendingClaim
                ? 'border-brand-orange/40 bg-brand-orange/10'
                : 'border-positive/40 bg-positive/10'
            }`}
          >
            <p className="font-medium">
              {isDeposited
                ? `${MOCK_TOKEN_LABEL} deposited into your Margin Account. You are ready to trade.`
                : isPendingClaim
                ? 'Faucet transaction submitted. Waiting for Arbitrum Sepolia confirmation.'
                : activeClaim.status === 'already_funded'
                ? `${MOCK_TOKEN_LABEL} is already available for this ${isTradingAccountRecipient ? 'Trading Account' : 'wallet'}.`
                : activeClaim.status === 'already_claimed'
                  ? `${MOCK_TOKEN_LABEL} was already claimed for this ${isTradingAccountRecipient ? 'Trading Account' : 'wallet'}.`
                  : `${MOCK_TOKEN_LABEL} minted to your ${isTradingAccountRecipient ? 'Trading Account' : 'wallet'}.`}
            </p>
            <p className="text-content-secondary">
              {isDeposited
                ? 'Your sponsored deposit is confirmed.'
                : isAutomaticFunding
                ? fundingPhase === 'depositing'
                  ? `Depositing ${MOCK_TOKEN_LABEL} into your Margin Account. Confirm in your wallet if prompted.`
                  : `Waiting for ${MOCK_TOKEN_LABEL} to arrive. Your sponsored deposit will start automatically.`
                : isPendingClaim
                ? 'Use “Check confirmation” to refresh the transaction status.'
                : 'Deposit your USDG into your Margin Account to start trading.'}
            </p>
            {!isPendingClaim && isTradingAccountRecipient ? (
              <p className="text-content-secondary">
                Plether covers gas for eligible trades. Wallet transfers need Arbitrum Sepolia ETH for gas.
              </p>
            ) : !isPendingClaim ? (
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
            ) : null}
            {activeClaim.txHash ? (
              <a
                href={`https://sepolia.arbiscan.io/tx/${activeClaim.txHash}`}
                target="_blank"
                rel="noreferrer"
                className={`break-all hover:underline ${isPendingClaim ? 'text-brand-orange' : 'text-positive'}`}
              >
                {activeClaim.txHash}
              </a>
            ) : null}
          </div>
        ) : null}
      </div>

    </Modal>
  )
}

export function TestnetWelcomeModal() {
  return PERPS_RELEASE_IS_DEPLOYED ? <LiveTestnetWelcomeModal /> : null
}

function LiveTestnetWelcomeModal() {
  const { address: connectedAddress, isConnected } = useAccount()
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
  const previousConnectedAddressRef = useRef<string | undefined>(faucetRecipient)
  const displayedWalletAddress = perpsIdentity.isAaManifestConfigured
    ? faucetRecipient ?? ''
    : walletAddress
  const { claim, phase, error: submitError, requestFunds: fundAccount } = useTestnetFunding(
    displayedWalletAddress, isConnected
  )
  const isSubmitting = phase === 'claiming' || phase === 'waiting' || phase === 'depositing'
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
    if (!isAddress(trimmedAddress)) {
      setFieldError('Enter a valid wallet address.')
      return
    }
    setFieldError(null)
    await fundAccount(trimmedAddress)
  }

  return (
    <TestnetWelcomeModalView
      isOpen={!dismissed}
      isWalletConnected={isConnected}
      onClose={dismiss}
      onConnectWallet={() => {
        setFieldError(null)
        void openAppKit()
      }}
      walletAddress={displayedWalletAddress}
      fieldError={fieldError ?? undefined}
      submitError={submitError ?? undefined}
      recipientError={recipientError}
      claim={displayedClaim}
      isSubmitting={isSubmitting}
      fundingPhase={phase}
      isTradingAccountRecipient={perpsIdentity.isAaManifestConfigured}
      onWalletAddressChange={(nextAddress) => {
        if (perpsIdentity.isAaManifestConfigured) return
        setWalletAddress(nextAddress)
        setFieldError(null)
      }}
      onRequestFunds={() => { void requestFunds() }}
      onDeposit={() => {
        dismiss()
        if (phase !== 'deposited') requestMarginAction('deposit')
        void navigate('/')
      }}
    />
  )
}
