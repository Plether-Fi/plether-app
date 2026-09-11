import { readinessChecks, readinessMessage, refreshReadiness, useTradingReadiness, type ReadinessAction } from '../perps-aa/readiness'

export function TradingStatus({ action = 'open' }: { action?: ReadinessAction }) {
  const snapshot = useTradingReadiness(true)
  const checks = readinessChecks(snapshot, action)
  const issues = checks.filter(c => c.status !== 'ready' || c.reason === 'FUNDING_LOW')
  const blocked = issues.some(c => c.status === 'blocked')
  return <details key={issues.length ? 'issue' : 'healthy'} open={issues.length > 0} className="border border-border p-3 text-sm" aria-live="polite">
    <summary className="cursor-pointer font-semibold">Trading status — {blocked ? 'temporarily unavailable' : issues.length ? 'unverified' : 'ready'}</summary>
    <div className="mt-2 space-y-2 text-content-secondary">
      {issues.length ? [...new Set(issues.map(c => c.reason))].map(reason => <p key={reason}>{readinessMessage(reason)}</p>) : <p>No known blocker. Readiness does not guarantee execution.</p>}
      {blocked && !snapshot?.enforcementEnabled && <p>Observation mode: this status does not prevent signing.</p>}
      <p>{snapshot ? `Last checked: ${new Date(snapshot.observedAt).toLocaleTimeString()}.` : 'No fresh observation.'} <button type="button" className="underline" onClick={() => void refreshReadiness(true)}>Refresh</button></p>
      <p>Current service status does not explain historical transaction failures.</p>
    </div>
  </details>
}
