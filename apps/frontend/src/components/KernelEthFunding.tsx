import { useQuery } from '@tanstack/react-query'
import { useId, useRef, useState } from 'react'
import { formatEther, formatUnits, type Hex } from 'viem'
import { arbitrumSepolia } from 'viem/chains'
import { useChainId, usePublicClient, useWalletClient } from 'wagmi'
import { PERPS_SETTLEMENT_TOKEN_LABEL } from '../contracts/perpsAddresses'
import { kernelQuoteIsFresh } from '../hooks/usePerpsGasReview'
import { useSwitchToArbitrumSepolia } from '../hooks/useSwitchToArbitrumSepolia'
import { readGasPaymentPreference, useGasPaymentPreference } from '../perps-aa/gasPaymentPreference'
import { KernelObservedPendingError } from '../perps-aa/errors'
import {
  buildKernelEthWithdrawal, confirmKernelEthTopUp, parseKernelEthAmount, readKernelEthFunds,
  sendKernelEthTopUp, verifyKernelEthRecipient, type KernelEthWithdrawalSource,
} from '../perps-aa/kernelEthFunding'
import type { KernelActionQuote } from '../perps-aa/kernelRuntime'
import type { KernelPerpsAaDeploymentManifest } from '../perps-aa/manifest'
import type { KernelPerpsAaSmartAccountRuntime } from '../perps-aa/runtimeContext'
import { getExplorerTxUrl } from '../utils/explorer'
import { errorField, transactionErrorRecords, transactionFailureCode } from '../utils/transactionFailure'
import { Button, Input } from './ui'

type FundingAction = 'topup' | KernelEthWithdrawalSource

export function KernelEthFunding({ runtime, manifest }: {
  runtime: KernelPerpsAaSmartAccountRuntime
  manifest: KernelPerpsAaDeploymentManifest
}) {
  const scope = runtime.kernel.scope
  const id = useId()
  const client = usePublicClient({ chainId: arbitrumSepolia.id })
  const { data: walletClient } = useWalletClient({ chainId: arbitrumSepolia.id })
  const isCorrectChain = useChainId() === arbitrumSepolia.id
  const { switchToArbitrumSepolia, isSwitching, switchError } = useSwitchToArbitrumSepolia()
  const { mode, revision } = useGasPaymentPreference(runtime.gasPayment.scope)
  const inFlight = useRef(false)
  const [busy, setBusy] = useState(false)
  const [form, setForm] = useState<FundingAction>()
  const [amountText, setAmountText] = useState('')
  const [reviewedWithdrawal, setReviewedWithdrawal] = useState<{ quote: KernelActionQuote; revision: string }>()
  const [topUpHash, setTopUpHash] = useState<Hex>()
  const [notice, setNotice] = useState<{ message: string; error?: boolean }>()
  const balances = useQuery({
    queryKey: ['kernel-eth-funding', scope.deployment, scope.chainId, scope.sender],
    queryFn: () => readKernelEthFunds(runtime, client),
    enabled: Boolean(client),
    retry: false,
    refetchInterval: 10_000,
  })
  const snapshot = balances.data
  const amount = parseKernelEthAmount(amountText)
  const quote = mode === 'eth' && reviewedWithdrawal?.revision === revision ? reviewedWithdrawal.quote : undefined
  const withdrawal = form !== undefined && form !== 'topup'
  const readyForWithdrawal = mode === 'eth' && runtime.gasPayment.readiness.eth
  const canContinue = isCorrectChain && !balances.isError

  function isCurrentEthPreference() {
    const current = readGasPaymentPreference(runtime.gasPayment.scope)
    return current.mode === 'eth' && current.revision === revision
  }

  function choose(next?: FundingAction) {
    setForm(next)
    setAmountText('')
    setReviewedWithdrawal(undefined)
    setNotice(undefined)
  }

  async function run(action: () => Promise<void>) {
    if (inFlight.current) return
    inFlight.current = true
    setBusy(true)
    setNotice(undefined)
    try {
      await action()
    } catch (cause) {
      if (cause instanceof KernelObservedPendingError) {
        choose()
        setNotice({ message: cause.message })
      } else {
        setNotice({ message: cause instanceof Error ? cause.message : 'Unable to complete this action.', error: true })
      }
    } finally {
      setBusy(false)
      inFlight.current = false
    }
  }

  function requestTopUp() {
    if (!snapshot || !client || !walletClient || !canContinue || amount === undefined) return
    void run(async () => {
      let hash: Hex
      try {
        hash = await sendKernelEthTopUp(runtime, client, walletClient, amount)
      } catch (cause) {
        const errorNames = Array.from(transactionErrorRecords(cause), record => errorField(record, 'name'))
        if (transactionFailureCode(cause) === 'WALLET_DECLINED' || errorNames.includes('UserRejectedRequestError')) {
          setNotice({ message: 'You declined the ETH transfer in your wallet.' })
          return
        }
        if (errorNames.includes('FeeCapTooLowError')) {
          throw new Error('Arbitrum Sepolia rejected the gas fee cap. Review a fresh ETH transfer.', { cause })
        }
        throw new Error('The ETH transfer could not be verified. Check your wallet before retrying.', { cause })
      }
      setTopUpHash(hash)
      setForm(undefined)
      let confirmed: Hex
      try {
        confirmed = await confirmKernelEthTopUp(client, hash, runtime.ownerAddress, snapshot.address, amount)
      } catch (cause) {
        throw new Error('Check the ETH transfer in the explorer before retrying.', { cause })
      }
      setTopUpHash(confirmed)
      await balances.refetch()
      setNotice({ message: 'ETH transfer confirmed. Reopen your transaction review for a fresh gas quote.' })
    })
  }

  function reviewWithdrawal() {
    if (!snapshot || !withdrawal || !canContinue || !readyForWithdrawal || !isCurrentEthPreference() ||
      amount === undefined || amount > snapshot[form]) return
    void run(async () => {
      const action = buildKernelEthWithdrawal(runtime, form, amount)
      const reviewed = await runtime.kernel.quoteAction(action)
      if (isCurrentEthPreference() && reviewed.mode === 'eth') setReviewedWithdrawal({ quote: reviewed, revision })
    })
  }

  function confirmWithdrawal() {
    if (!snapshot || !withdrawal || !canContinue || !readyForWithdrawal || !isCurrentEthPreference() ||
      amount === undefined || amount > snapshot[form] || quote?.mode !== 'eth') return
    void run(async () => {
      if (!kernelQuoteIsFresh(quote)) {
        setReviewedWithdrawal(undefined)
        throw new Error('The gas quote expired. Review the withdrawal again.')
      }
      await runtime.kernel.execute({ manifest, runtime, action: buildKernelEthWithdrawal(runtime, form, amount),
        ownerAddress: runtime.ownerAddress, kernelQuote: quote })
      choose()
      await balances.refetch()
      setNotice({ message: 'ETH withdrawal confirmed.' })
    })
  }

  return (
    <section aria-labelledby={`${id}-title`} aria-busy={busy} className="space-y-3 border border-brand-border/30 bg-app-bg/30 p-4">
      <h3 id={`${id}-title`} className="text-sm font-semibold text-content-primary">ETH for gas</h3>
      {snapshot ? <>
        {balances.isError && <p role="status" className="text-sm text-warning">Showing the last verified balances. Wait for verification to resume before continuing.</p>}
        <dl className="space-y-1 text-sm text-content-secondary">
          <div className="flex justify-between gap-3"><dt>Trading Account ETH</dt><dd>{formatEther(snapshot.account)} ETH</dd></div>
          {snapshot.entrypoint > 0n && <div className="flex justify-between gap-3"><dt>EntryPoint ETH deposit</dt><dd>{formatEther(snapshot.entrypoint)} ETH</dd></div>}
          <div className="flex justify-between gap-3"><dt>Perps margin</dt><dd>{snapshot.margin === undefined ? 'Unavailable' : `${formatUnits(snapshot.margin, 6)} ${PERPS_SETTLEMENT_TOKEN_LABEL}`}</dd></div>
        </dl>
        <div className="space-y-1 border-t border-brand-border/20 pt-3 text-sm text-content-secondary">
          <p>ETH funding address on Arbitrum Sepolia</p>
          <p className="break-all font-mono text-xs text-content-primary">{snapshot.address}</p>
        </div>
        <div className="flex flex-wrap gap-2">
          <Button size="sm" disabled={busy} onClick={() => { choose('topup') }}>Add ETH</Button>
          <Button size="sm" variant="secondary" disabled={busy} onClick={() => {
            void run(async () => {
              const recipient = await verifyKernelEthRecipient(runtime)
              await navigator.clipboard.writeText(recipient.address)
              setNotice({ message: 'Funding address copied.' })
            })
          }}>Copy funding address</Button>
          {snapshot.account > 0n && <Button size="sm" variant="secondary" disabled={busy} onClick={() => { choose('account') }}>Withdraw ETH</Button>}
          {snapshot.entrypoint > 0n && <Button size="sm" variant="secondary" disabled={busy} onClick={() => { choose('entrypoint') }}>Withdraw EntryPoint deposit</Button>}
        </div>
        {form !== undefined && <div className="space-y-3 border-t border-brand-border/20 pt-3">
          <Input label={form === 'topup' ? 'ETH to add' : 'ETH to withdraw'} inputMode="decimal" maxLength={80} value={amountText} disabled={busy}
            onChange={event => { setAmountText(event.target.value); setReviewedWithdrawal(undefined); setNotice(undefined) }} />
          {form === 'topup'
            ? <p className="text-xs text-content-secondary">Your Owner Wallet pays for the transfer. The ETH goes to the Trading Account address above.</p>
            : <p className="text-xs text-content-secondary">
                The withdrawal goes to your Owner Wallet. {mode === 'eth' && (form === 'entrypoint'
                  ? 'Leave enough in the EntryPoint deposit for the maximum gas charge.'
                  : 'Leave enough ETH for gas if the EntryPoint deposit cannot cover the maximum.')}
              </p>}
          {quote && amount !== undefined && <dl className="space-y-1 text-sm text-content-secondary">
            <div className="flex justify-between gap-3"><dt>Withdraw to Owner Wallet</dt><dd>{formatEther(amount)} ETH</dd></div>
            <div className="flex flex-wrap justify-between gap-x-3"><dt>Recipient</dt><dd className="break-all font-mono text-xs">{runtime.ownerAddress}</dd></div>
            <div className="flex justify-between gap-3"><dt>Network gas</dt><dd>Up to {formatEther(quote.maxNetworkGasWei)} ETH</dd></div>
          </dl>}
          {withdrawal && !readyForWithdrawal && <p role="status" className="text-sm text-warning">{mode === 'sponsored'
            ? 'Turn off Gas sponsorship in Trading Account activity to withdraw ETH.'
            : 'ETH payments are currently unavailable.'}</p>}
          {!isCorrectChain && <div className="space-y-2">
            <p role="status" className="text-sm text-warning">Use Arbitrum Sepolia to {form === 'topup' ? 'send' : 'withdraw'} ETH.</p>
            <Button size="sm" variant="secondary" disabled={busy || isSwitching} onClick={() => { void switchToArbitrumSepolia() }}>Switch to Arbitrum Sepolia</Button>
            {switchError && <p role="alert" className="text-sm text-warning">{switchError}</p>}
          </div>}
          <div className="flex flex-wrap gap-2">
            {form === 'topup'
              ? <Button size="sm" disabled={busy || !canContinue || !walletClient || amount === undefined} onClick={requestTopUp}>Send ETH from Owner Wallet</Button>
              : quote
                ? <Button size="sm" disabled={busy || !canContinue || !readyForWithdrawal || amount === undefined || amount > snapshot[form]} onClick={confirmWithdrawal}>Continue to wallet</Button>
                : <Button size="sm" disabled={busy || !canContinue || !readyForWithdrawal || amount === undefined || amount > snapshot[form]} onClick={reviewWithdrawal}>Review withdrawal</Button>}
            <Button size="sm" variant="secondary" disabled={busy} onClick={() => { choose() }}>Cancel</Button>
          </div>
        </div>}
      </> : <p role="status" className="text-sm text-content-secondary">{balances.isError
        ? 'Trading Account ETH balances are temporarily unavailable.'
        : 'Verifying the Trading Account funding address…'}</p>}
      {busy && <p role="status" className="text-sm text-content-secondary">Check your wallet if approval is requested.</p>}
      {notice && <p role={notice.error ? 'alert' : 'status'} className={`text-sm ${notice.error ? 'text-warning' : 'text-content-secondary'}`}>{notice.message}</p>}
      {topUpHash && <a href={getExplorerTxUrl(scope.chainId, topUpHash)} target="_blank" rel="noopener noreferrer" className="text-sm text-brand-peach underline">View ETH transfer</a>}
    </section>
  )
}
