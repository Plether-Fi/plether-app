import { useEffect, useState } from 'react'
import { formatEther, type Hex } from 'viem'
import { Button } from '../components/ui/Button'
import { deadlineNow, refreshDeadlineClock } from './deadlineClock'
import {
  discardUnsentKernelAttempt,
  discardUnsentKernelCancellation,
  isKernelAttemptPending,
  kernelAttemptCanCancel,
  kernelAttemptCanDiscard,
  kernelAttemptCanRetry,
  kernelCancellationCanRetry,
  kernelCancellationCanDiscard,
  refreshKernelAttempt,
  retryKernelAttempt,
  retryKernelCancellation,
  signAndSubmitKernelCancellation,
  type KernelOperationAttempt,
} from './kernelOperationJournal'
import { KernelRpcError, type KernelPreparedCancellation, type KernelPreparedOperation } from './kernelRpc'

function readRecoveryTime(): number | undefined {
  try {
    return deadlineNow()
  } catch {
    return undefined
  }
}

export function KernelAttemptRecoveryControls({ attempt, quoteCancellation, signPrepared }: {
  attempt: KernelOperationAttempt
  quoteCancellation?: (original: KernelPreparedOperation, signature: Hex) => Promise<KernelPreparedCancellation>
  signPrepared?: (prepared: KernelPreparedOperation) => Promise<Hex>
}) {
  const [quote, setQuote] = useState<KernelPreparedCancellation>()
  const [busy, setBusy] = useState(false)
  const [notice, setNotice] = useState<string>()
  const [now, setNow] = useState(readRecoveryTime)
  const pending = isKernelAttemptPending(attempt)

  useEffect(() => {
    if (!pending) return
    const controller = new AbortController()
    const update = () => {
      if (!controller.signal.aborted) setNow(readRecoveryTime())
    }
    void refreshDeadlineClock(controller.signal).then(update, update)
    const timer = globalThis.setInterval(update, 1_000)
    return () => {
      controller.abort()
      globalThis.clearInterval(timer)
    }
  }, [pending])

  if (!pending) return null
  const canQuote = quoteCancellation !== undefined && signPrepared !== undefined && kernelAttemptCanCancel(attempt)
  const reviewed = canQuote && quote?.cancelsUserOperationHash === attempt.prepared.userOperationHash ? quote : undefined
  const quoteExpired = reviewed !== undefined && now !== undefined && now >= reviewed.expiresAt * 1_000

  const run = async (action: () => Promise<void>) => {
    setBusy(true)
    setNotice(undefined)
    try {
      await action()
    } catch (error) {
      setNotice(error instanceof KernelRpcError && error.reason === 'INSUFFICIENT_GAS_ETH'
        ? 'Add ETH to the Trading Account to pay the cancellation fee.'
        : 'The gateway could not complete this request. The original attempt remains in activity.')
    } finally {
      setBusy(false)
    }
  }

  return (
    <div className="space-y-3 border-t border-brand-border/20 pt-3" aria-busy={busy}>
      {reviewed && signPrepared ? (
        <div className="space-y-2 bg-surface-muted p-3 text-sm text-content-secondary">
          <p className="font-semibold text-content-primary">Review cancellation</p>
          <dl className="flex flex-wrap justify-between gap-x-3 gap-y-1">
            <dt>Maximum network gas</dt>
            <dd className="font-semibold text-content-primary">{formatEther(reviewed.maxNetworkGasWei)} ETH</dd>
          </dl>
          <p>Cancellation uses ETH from this Trading Account. The original operation may still confirm first.</p>
          {reviewed.operation.factory && <p>The quote includes deployment of this Trading Account.</p>}
          {quoteExpired && <p>This cancellation quote expired. Go back to review a fresh fee.</p>}
          <div className="flex flex-wrap gap-2">
            <Button type="button" size="sm" variant="danger" disabled={busy || now === undefined || quoteExpired} onClick={() => {
              void run(async () => {
                await signAndSubmitKernelCancellation(attempt, reviewed, signPrepared)
                setQuote(undefined)
                setNotice('Cancellation submitted. Wait for onchain confirmation.')
              })
            }}>Approve cancellation</Button>
            <Button type="button" size="sm" variant="ghost" disabled={busy} onClick={() => { setQuote(undefined) }}>Back</Button>
          </div>
        </div>
      ) : (
        <div className="flex flex-wrap gap-2">
          {now === undefined && (attempt.phase === 'signed' || attempt.cancellation?.phase === 'signed') &&
            <Button type="button" size="sm" variant="secondary" disabled={busy} onClick={() => {
              void run(async () => { setNow((await refreshDeadlineClock()).now()) })
            }}>Refresh quote status</Button>}
          {attempt.phase !== 'signed' && <Button type="button" size="sm" variant="secondary" disabled={busy} onClick={() => {
            void run(async () => {
              const result = await refreshKernelAttempt(attempt)
              if (isKernelAttemptPending(result)) setNotice('This operation is still pending.')
            })
          }}>Check status</Button>}
          {kernelAttemptCanRetry(attempt, now) && <Button type="button" size="sm" variant="secondary" disabled={busy} onClick={() => {
            void run(async () => {
              await retryKernelAttempt(attempt)
              setNotice('The signed operation was sent again with its original gas payer and limits.')
            })
          }}>Resend signed operation</Button>}
          {canQuote && <Button type="button" size="sm" variant="secondary" disabled={busy} onClick={() => {
            void run(async () => {
              setQuote(await quoteCancellation(attempt.prepared, attempt.signature))
              setNow(readRecoveryTime())
            })
          }}>Review cancellation</Button>}
          {kernelCancellationCanRetry(attempt, now) && <Button type="button" size="sm" variant="secondary" disabled={busy} onClick={() => {
            void run(async () => {
              await retryKernelCancellation(attempt)
              setNotice('The signed cancellation was sent again with its original fees.')
            })
          }}>Resend signed cancellation</Button>}
          {kernelAttemptCanDiscard(attempt, now) && (
            <Button type="button" size="sm" variant="secondary" disabled={busy} onClick={() => {
              void run(() => discardUnsentKernelAttempt(attempt))
            }}>Discard unsent operation</Button>
          )}
          {kernelCancellationCanDiscard(attempt, now) && (
            <Button type="button" size="sm" variant="secondary" disabled={busy} onClick={() => {
              void run(() => discardUnsentKernelCancellation(attempt))
            }}>Discard unsent cancellation</Button>
          )}
        </div>
      )}
      {attempt.cancellation && <p className="text-xs text-content-secondary">Waiting for the original operation or its cancellation to be confirmed onchain.</p>}
      {notice && <p role="status" className="text-xs text-content-secondary">{notice}</p>}
    </div>
  )
}
