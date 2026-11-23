import { formatEther } from 'viem'
import type { PerpsGasReviewView } from '../hooks/usePerpsGasReview'
import type { KernelActionQuote } from '../perps-aa/kernelRuntime'
import { findKernelGasFundingError } from '../perps-aa/kernelRpc'
import { Button } from './ui'

export function PerpsGasReview({ gas, quote, error, showSettings = true, onOpenSettings }: {
  gas: PerpsGasReviewView
  quote?: KernelActionQuote
  error?: unknown
  showSettings?: boolean
  onOpenSettings?: () => void
}) {
  if (!gas) return null
  const mode = quote?.mode ?? gas.preference.mode
  const payFromWallet = mode === 'eth' && (quote ? quote.payer === 'wallet' : gas.paysEthFromWallet)
  const fundingError = mode === 'eth' && !payFromWallet ? findKernelGasFundingError(error) : undefined
  const funding = fundingError?.funding
  return (
    <div className="space-y-2 text-sm text-content-secondary">
      <dl className="space-y-1">
        <div className="flex justify-between gap-3">
          <dt>Network gas</dt>
          <dd className="text-right font-semibold text-content-primary">{mode === 'sponsored' ? 'Sponsored by Plether' : payFromWallet ? 'Pay from wallet' : 'Pay with ETH'}</dd>
        </div>
        {mode === 'eth' && quote && !funding ? (
          <div className="flex justify-between gap-3">
            <dt>Maximum network gas</dt>
            <dd className="text-right font-semibold text-content-primary">{formatEther(quote.maxNetworkGasWei)} ETH</dd>
          </div>
        ) : null}
        {funding && (
          <>
            <div className="flex justify-between gap-3">
              <dt>Estimated maximum network gas</dt>
              <dd className="text-right font-semibold text-content-primary">{formatEther(funding.maxNetworkGasWei)} ETH</dd>
            </div>
            <div className="flex justify-between gap-3">
              <dt>Trading Account ETH</dt>
              <dd>{formatEther(funding.balanceWei)} ETH</dd>
            </div>
            {funding.entryPointDepositWei > 0n && (
              <div className="flex justify-between gap-3">
                <dt>EntryPoint ETH deposit</dt>
                <dd>{formatEther(funding.entryPointDepositWei)} ETH</dd>
              </div>
            )}
          </>
        )}
      </dl>
      {payFromWallet && quote && <p>Sign the Trading Account action, then confirm the gas payment in your Owner Wallet.</p>}
      {funding && <p role="status">Add at least {formatEther(funding.shortfallWei)} ETH, then review a fresh gas quote.</p>}
      {!gas.available ? <p role="status">{mode === 'sponsored' ? 'Gas sponsorship is currently unavailable.' : 'Paying gas with ETH is currently unavailable.'}</p> : null}
      {showSettings ? (
        <button type="button" className="font-semibold text-brand-peach underline underline-offset-4" onClick={onOpenSettings ?? gas.openActivity}>
          {fundingError ? 'Add ETH' : 'Gas settings'}
        </button>
      ) : null}
    </div>
  )
}

export function KernelGasApprovalButtons({ confirm, cancel, disabled = false }: { confirm: () => void; cancel: () => void; disabled?: boolean }) {
  return (
    <div className="flex flex-wrap gap-3">
      <Button type="button" variant="secondary" onClick={cancel}>Cancel gas review</Button>
      <Button type="button" disabled={disabled} onClick={confirm}>Continue to wallet</Button>
    </div>
  )
}
