import { useEffect, useState } from 'react'
import type { PositionProtection } from '../contracts/positionProtection'
import { PERPS_TERMINAL_REASON_LABELS } from '../contracts/perpsOrderV2'
import { currentProtectionObservation, PROTECTION_EXECUTION_COPY, type ProtectionExecutionReport } from '../utils/protectionExecution'
import { getExplorerTxUrl } from '../utils/explorer'

export function ProtectionExecutionNotice({ protection, report, loading, error, onRefresh }: {
  protection: PositionProtection
  report?: ProtectionExecutionReport
  loading?: boolean
  error?: boolean
  onRefresh?: () => void
}) {
  const [now, setNow] = useState(Date.now)
  useEffect(() => {
    const timer = window.setInterval(() => { setNow(Date.now()) }, 1_000)
    return () => { window.clearInterval(timer) }
  }, [])
  const observation = error ? undefined : currentProtectionObservation(report, protection, now)
  const copy = observation ? PROTECTION_EXECUTION_COPY[observation.reason] : undefined
  const transaction = observation?.transactionHash
  const unavailableTitle = loading ? 'Checking automatic execution…' : error ? 'Unable to refresh execution status' : report?.observation ? 'Execution status is out of date' : 'Automatic execution status unavailable'
  const title = transaction
    ? observation.transactionAction === 'prune' ? 'Queue cleanup awaiting confirmation' : observation.transactionAction === 'retry' ? 'Retry awaiting confirmation' : 'Trigger transaction awaiting confirmation'
    : copy?.title ?? unavailableTitle
  const body = transaction
    ? 'The worker is waiting for this transaction to confirm. This does not yet mean your position has closed.'
    : copy?.body ?? (loading ? 'Reading the latest worker check. Your on-chain TP/SL is shown above.' : 'There is no recent worker check matching this close attempt. We cannot confirm that automatic execution is running. Your on-chain TP/SL is shown above.')
  return <section aria-label="Automatic execution status" className="border border-brand-border/20 p-3">
    <div role="status" aria-live="polite">
      <p className={`text-xs font-semibold ${!observation || ['operator-required', 'execution-disabled', 'check-failed', 'oracle-unavailable'].includes(observation.reason) ? 'text-[#F7D977]' : 'text-content-primary'}`}>{title}</p>
      <p className="mt-1 text-xs leading-5 text-content-secondary">{body}</p>
    </div>
    {observation ? <div className="mt-2 space-y-1 text-xs text-content-secondary">
      {observation.outcomeReason !== undefined ? <p>Last close outcome: {PERPS_TERMINAL_REASON_LABELS[observation.outcomeReason] ?? 'Unknown failure'}</p> : null}
      <p>Last worker check: <time dateTime={observation.checkedAt}>{new Date(observation.checkedAt).toLocaleTimeString()}</time> · advisory, not an execution guarantee</p>
      {transaction ? <a className="inline-block underline underline-offset-4" href={getExplorerTxUrl(421614, transaction)} target="_blank" rel="noopener noreferrer">View pending transaction ↗</a> : null}
    </div> : null}
    {!loading && onRefresh ? <button type="button" className="mt-2 text-xs text-content-secondary underline underline-offset-4" onClick={onRefresh}>Refresh execution status</button> : null}
  </section>
}
