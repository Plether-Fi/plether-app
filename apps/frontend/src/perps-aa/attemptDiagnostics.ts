/** First-party, advisory telemetry. No payloads, signatures, addresses or raw errors. */
export type AttemptStage = 'wallet_requested' | 'wallet_approved' | 'wallet_declined' |
  'wallet_interrupted' | 'signed_operation_saved' | 'submission_requested' |
  'submission_acknowledged' | 'submission_failed' | 'deadline_elapsed' |
  'execution_interrupted' | 'safe_expiry_verified'

const sent = new Set<string>()
export function reportAttemptStage(attemptId: string, stage: AttemptStage): void {
  if (!/^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/i.test(attemptId)) return
  const key = `${attemptId}:${stage}`
  if (sent.has(key)) return
  if (sent.size >= 200) {
    const oldest = sent.values().next().value
    if (oldest !== undefined) sent.delete(oldest)
  }
  sent.add(key)
  try {
    // Never wait for telemetry on the signing/submission path. keepalive allows
    // a small in-flight report to finish while the user switches to the wallet.
    void fetch('/api/perps/v1/aa/diagnostics', {
      method: 'POST', credentials: 'same-origin', cache: 'no-store', keepalive: true,
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ attemptId, stage }), signal: AbortSignal.timeout(5_000),
    }).catch(() => { /* Missing telemetry is not evidence of a rejected trade. */ })
  } catch { /* Diagnostic failures must never affect a trade. */ }
}
