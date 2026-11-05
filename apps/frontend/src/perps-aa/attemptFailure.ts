/** Closed telemetry vocabulary. Never inspect messages, stacks, URLs or payloads. */
export const failureSteps = [
  'preflight', 'review_read', 'preparation_journal', 'review_clock', 'review_deadline',
  'recovery_check', 'sponsorship', 'prepared_payload_check', 'pre_sign_journal',
  'readiness_check', 'review_revalidation', 'signing_clock', 'signing_deadline',
  'wallet_approval', 'signed_payload_check', 'signed_journal', 'submission_clock',
  'submission_deadline', 'submission_journal', 'submission', 'confirmation',
] as const
export type FailureStep = typeof failureSteps[number]
export const failureReasons = [
  'UNKNOWN', 'REQUEST_ABORTED', 'REQUEST_TIMEOUT', 'WALLET_DECLINED', 'WALLET_DISCONNECTED',
  'NETWORK_ERROR', 'REVIEW_CHANGED', 'PREPARED_PAYLOAD_CHANGED', 'PREPARATION_UNUSABLE',
  'OPERATION_STORE_UNAVAILABLE', 'INVALID_ORDER_DEADLINE', 'DEADLINE_TOO_CLOSE',
  'SPONSOR_UNAVAILABLE', 'SPONSOR_REQUEST_TIMEOUT', 'RATE_LIMITED', 'SPONSOR_BUDGET_EXCEEDED',
  'SIMULATION_FAILED', 'POLICY_DENIED', 'PAYMASTER_PAUSED', 'ACCOUNT_NOT_TRUSTED',
  'ACCOUNT_DEPLOYMENT_PENDING', 'INSUFFICIENT_FREE_EQUITY', 'MUST_CLOSE_OPPOSING',
  'EXECUTION_GAS_CAP_EXCEEDED', 'RESTART_ESTIMATION', 'SECURITY_ATTESTATION_UNAVAILABLE',
  'SUBMISSION_PAUSED', 'SPONSORSHIP_NOT_AUTHORIZED', 'DATABASE_UNAVAILABLE',
  'BUNDLER_UNAVAILABLE', 'SUBMISSION_OUTCOME_UNKNOWN', 'SUBMISSION_HASH_MISMATCH',
  'RECEIPT_TIMEOUT', 'USER_OPERATION_REVERTED', 'READINESS_UNAVAILABLE',
  'OPEN_EXECUTION_UNAVAILABLE', 'PROTECTION_TRIGGER_UNAVAILABLE',
] as const
export type FailureReason = typeof failureReasons[number]
export interface AttemptFailure { failureStep: FailureStep; reasonCode: FailureReason }

/** Adds a diagnostic code without changing recovery's existing Error classification. */
export class AttemptFailureError extends Error {
  readonly diagnosticCode: FailureReason
  constructor(message: string, diagnosticCode: FailureReason) {
    super(message)
    this.diagnosticCode = diagnosticCode
  }
}

export function safeAttemptFailure(value: AttemptFailure | undefined): AttemptFailure | undefined {
  if (value && failureSteps.includes(value.failureStep) && failureReasons.includes(value.reasonCode)) {
    return { failureStep: value.failureStep, reasonCode: value.reasonCode }
  }
  return undefined
}

export function classifyAttemptFailure(failureStep: FailureStep, error: unknown): AttemptFailure {
  let reasonCode: FailureReason = 'UNKNOWN'
  try {
    const seen = new Set<object>()
    let current = error
    for (let depth = 0; depth < 8 && current && typeof current === 'object'; depth++) {
      if (seen.has(current)) break
      seen.add(current)
      const value = current as Record<string, unknown>
      const data = value.data && typeof value.data === 'object' ? value.data as Record<string, unknown> : undefined
      const code = value.diagnosticCode ?? value.reason ?? data?.reason
      if (typeof code === 'string' && failureReasons.includes(code as FailureReason) && code !== 'UNKNOWN') {
        reasonCode = code as FailureReason
        if (code !== 'SUBMISSION_OUTCOME_UNKNOWN') break
      }
      if (failureStep === 'wallet_approval' && value.code === 4001) { reasonCode = 'WALLET_DECLINED'; break }
      if (failureStep === 'wallet_approval' && (value.code === 4900 || value.code === 4901)) { reasonCode = 'WALLET_DISCONNECTED'; break }
      if (value.terminalStatus === 'receipt-timeout') { reasonCode = 'RECEIPT_TIMEOUT'; break }
      if (value.terminalStatus === 'execution-reverted') { reasonCode = 'USER_OPERATION_REVERTED'; break }
      if (value.name === 'AbortError') { reasonCode = 'REQUEST_ABORTED'; break }
      if (value.name === 'TimeoutError') { reasonCode = 'REQUEST_TIMEOUT'; break }
      if (value.name === 'HttpRequestError' || value.name === 'WebSocketRequestError') reasonCode = 'NETWORK_ERROR'
      current = value.cause
    }
  } catch { /* Even hostile provider objects cannot interfere with the original failure. */ }
  return { failureStep, reasonCode }
}
