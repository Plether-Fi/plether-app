import { captureAnalyticsEvent, captureFrontendLog } from './client'
import { BUILD_COMMIT } from '../config/buildInfo'
import { getPerpsContractErrorCode } from '../utils/perpsErrors'
import { errorField, TRANSACTION_FAILURE_MESSAGES, readableTransactionMessage, transactionErrorRecords, transactionFailureCode } from '../utils/transactionFailure'

export interface TransactionFailureContext {
  surface: 'perps' | 'wallet' | 'vault'
  action: string
  stage?: string
  attemptId?: string
}
export interface TransactionFailure {
  supportReference: string
  errorCode: string
  message: string
}
const references = new WeakMap<object, TransactionFailure>()
const emitted = new Set<string>()
const referenceCodes = new Map<string, string>()
export const isTransactionReference = (value: string | undefined): value is string =>
  value !== undefined && /^(?:tx-)?[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/i.test(value)

export function reportTransactionFailure(error: unknown, message: string | undefined, context: TransactionFailureContext): TransactionFailure {
  const records = [...transactionErrorRecords(error)]
  const existing = records.map(record => references.get(record)).find(value => value !== undefined)
  const reference = existing?.supportReference ?? (isTransactionReference(context.attemptId) ? context.attemptId : `tx-${crypto.randomUUID()}`)
  const classifiedCode = existing?.errorCode ?? getPerpsContractErrorCode(error) ?? transactionFailureCode(error)
  const code = classifiedCode === 'UNKNOWN' ? referenceCodes.get(reference) ?? classifiedCode : classifiedCode
  referenceCodes.set(reference, code)
  const oldestReference = referenceCodes.keys().next().value
  if (referenceCodes.size > 1000 && oldestReference !== undefined) referenceCodes.delete(oldestReference)
  const safeMessage = readableTransactionMessage(message?.replace(/\s*Support reference: (?:tx-)?[0-9a-f-]{36}/gi, '').replace(/\s*Transaction: 0x[0-9a-f]{64}/gi, ''))
  const failure = {
    supportReference: reference,
    errorCode: code,
    message: (safeMessage === TRANSACTION_FAILURE_MESSAGES.UNKNOWN || ['NETWORK_ERROR', 'REQUEST_TIMEOUT', 'RECEIPT_TIMEOUT', 'WALLET_DECLINED', 'WALLET_DISCONNECTED'].includes(code)) ? TRANSACTION_FAILURE_MESSAGES[code] ?? safeMessage : safeMessage,
  }
  for (const record of records) references.set(record, failure)
  const eventKey = `${reference}:${failure.errorCode}`
  if (!emitted.has(eventKey)) {
    emitted.add(eventKey)
    const oldest = emitted.values().next().value
    if (emitted.size > 1000 && oldest !== undefined) emitted.delete(oldest)
    const properties = {
      surface: context.surface, action_kind: context.action, stage: context.stage ?? 'transaction',
      support_reference: reference, attempt_id: reference.startsWith('tx-') ? undefined : reference,
      reason_code: failure.errorCode, error_code: failure.errorCode, build_commit: BUILD_COMMIT,
    }
    try {
      captureAnalyticsEvent('transaction failed', properties)
      captureFrontendLog(failure.errorCode === 'WALLET_DECLINED' ? 'warn' : 'error', 'Transaction failed', properties)
    } catch { /* Diagnostics must never interfere with recovery or error presentation. */ }
  }
  return failure
}

export function reportedTransactionError(error: unknown, message: string, context: TransactionFailureContext): Error {
  const failure = reportTransactionFailure(error, message, context)
  // Blockchain references stay local; PostHog receives only the random support ID.
  let hash: string | undefined
  for (const record of transactionErrorRecords(error)) {
    const value = errorField(record, 'transactionHash')
    if (typeof value === 'string' && /^0x[\da-f]{64}$/i.test(value)) { hash = value; break }
    const text = errorField(record, 'message')
    if (typeof text === 'string') hash ??= /(?:Failed tx|Failed transaction|Transaction hash): (0x[\da-f]{64})\b/i.exec(text)?.[1]
  }
  const wrapped = new Error(`${failure.message}${hash ? `\nTransaction: ${hash}` : ''}\n\nSupport reference: ${failure.supportReference}`, { cause: error })
  references.set(wrapped, failure)
  return wrapped
}
