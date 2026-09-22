/** Public failure copy and bounded inspection of error envelopes, never request payloads. */
export const TRANSACTION_FAILURE_MESSAGES: Record<string, string> = {
  DEADLINE_TOO_CLOSE: 'Too little time remained to submit this transaction after approval. Plether did not send this request. Check Trading Account activity for any earlier submission, then review the order again when recovery is complete.',
  INVALID_ORDER_DEADLINE: 'The order review expired. Check account activity, then review a fresh order.',
  WALLET_DECLINED: 'You declined the request in your wallet. Review the transaction again when you are ready.',
  WALLET_DISCONNECTED: 'The wallet disconnected. Reconnect it and check account activity before trying again.',
  NETWORK_ERROR: 'The connection failed. Check account activity to see whether the transaction was received before trying again.',
  REQUEST_TIMEOUT: 'The request timed out. Check account activity before retrying; it may still be processing.',
  RECEIPT_TIMEOUT: 'Confirmation is taking longer than expected. Check account activity before submitting another transaction.',
  REQUEST_ABORTED: 'The request was interrupted. Check account activity before trying again.',
  INSUFFICIENT_FUNDS: 'There are not enough funds for this transaction. Check your token balance and the network gas balance.',
  INSUFFICIENT_FREE_EQUITY: 'Not enough available trading collateral. Reduce the order size or add collateral.',
  MUST_CLOSE_OPPOSING: 'Close your existing position before opening one in the opposite direction.',
  RATE_LIMITED: 'Too many requests are in progress. Wait a moment, check account activity, and try again.',
  SPONSOR_BUDGET_EXCEEDED: 'Sponsored gas capacity is temporarily unavailable. Try again later.',
  SPONSOR_UNAVAILABLE: 'Gas sponsorship is temporarily unavailable. Check account activity before trying again.',
  SPONSOR_REQUEST_TIMEOUT: 'Gas sponsorship timed out. Check account activity before preparing another transaction.',
  PAYMASTER_PAUSED: 'Gas sponsorship is temporarily paused. Try again when service resumes.',
  POLICY_DENIED: 'This action is not eligible for sponsored gas. Review the action or contact support with your support reference.',
  SIMULATION_FAILED: 'The transaction could not pass the checks before submission. Refresh account state and review the action.',
  EXECUTION_GAS_CAP_EXCEEDED: 'This action exceeds the sponsored gas limit. Reduce its size or contact support.',
  ACCOUNT_NOT_TRUSTED: 'Your Trading Account could not be verified. Refresh the app and check account activity.',
  ACCOUNT_DEPLOYMENT_PENDING: 'Your Trading Account is still being activated. Wait for confirmation before trying again.',
  PREPARATION_UNUSABLE: 'This transaction preparation is no longer valid. Check account activity, then review a fresh transaction.',
  REVIEW_CHANGED: 'The order details changed during approval. Review the latest order before trying again.',
  PREPARED_PAYLOAD_CHANGED: 'The prepared transaction changed. Check account activity and review a fresh transaction.',
  OPERATION_STORE_UNAVAILABLE: 'The app could not save transaction recovery information. Keep this tab open and check account activity before retrying.',
  SECURITY_ATTESTATION_UNAVAILABLE: 'Transaction verification is temporarily unavailable. Check account activity and try again later.',
  SUBMISSION_PAUSED: 'Transaction submission is temporarily paused. Check account activity and try again later.',
  SPONSORSHIP_NOT_AUTHORIZED: 'The gas authorization could not be verified. Check account activity before preparing another transaction.',
  DATABASE_UNAVAILABLE: 'The transaction service is temporarily unavailable. Check account activity before trying again.',
  BUNDLER_UNAVAILABLE: 'The transaction relay is unavailable. Check account activity before retrying; your request may still be processing.',
  SUBMISSION_OUTCOME_UNKNOWN: 'We could not confirm whether this transaction was received. Check account activity before trying again.',
  SUBMISSION_HASH_MISMATCH: 'The transaction response could not be verified. Check account activity before trying again.',
  USER_OPERATION_REVERTED: 'The transaction was included onchain but could not complete. Refresh account state and review the action.',
  CONTRACT_REVERTED: 'The contract rejected this transaction. Refresh account state and review the action before trying again.',
  READINESS_UNAVAILABLE: 'Trading readiness could not be checked. Refresh the app and try again later.',
  OPEN_EXECUTION_UNAVAILABLE: 'Opening positions is currently unavailable. Refresh market status before trying again.',
  PROTECTION_TRIGGER_UNAVAILABLE: 'TP/SL execution is currently unavailable. Refresh your position and check its protection status.',
  LANE_BUSY: 'Another Trading Account action is still in progress. Check account activity before starting a new one.',
  TRADING_ACCOUNT_UNAVAILABLE: 'Your Trading Account is unavailable. Reconnect your wallet and refresh account state.',
  INVALID_AMOUNT: 'Enter an amount greater than zero and within the available balance.',
  MANIFEST_NOT_CONFIGURED: 'Trading is not configured for this network. Check the selected network or contact support.',
  MANIFEST_UNAVAILABLE: 'The trading configuration could not be loaded. Refresh and try again later.',
  MANIFEST_MISMATCH: 'The trading configuration changed. Refresh the app before reviewing another action.',
  IDENTITY_NOT_READY: 'Your wallet connection is not ready. Reconnect the wallet before trying again.',
  SPONSORSHIP_DISABLED: 'Sponsored transactions are currently disabled. Try again when service resumes.',
  RUNTIME_UNAVAILABLE: 'The wallet service is not ready. Reconnect your wallet and refresh the app.',
  OWNER_AUTHORIZATION_UNAVAILABLE: 'Wallet authorization is unavailable. Reconnect the owner wallet.',
  OWNER_AUTHORIZATION_FAILED: 'Wallet authorization failed. Reconnect the owner wallet and review the request.',
  ACTION_BUILD_FAILED: 'The app could not prepare this action. Refresh account state and review it again.',
  BROWSER_COORDINATION_UNAVAILABLE: 'The app could not coordinate this action across browser tabs. Check account activity before retrying.',
  OPERATION_EXPIRED: 'This transaction preparation expired. Check account activity before reviewing a fresh action.',
  OPERATION_DROPPED: 'The transaction relay stopped tracking this request. Check account activity before trying again.',
  OPERATION_REPLACED: 'Another request replaced this transaction. Check account activity for its outcome.',
  UNKNOWN: 'We could not determine why this transaction failed. Check account activity before retrying, or contact support with your support reference.',
}

export function errorField(value: object, key: string): unknown {
  try { return (value as Record<string, unknown>)[key] } catch { return undefined }
}

export function* transactionErrorRecords(error: unknown): Generator<object> {
  const seen = new Set<object>()
  function* visit(value: unknown, depth: number): Generator<object> {
    if (!value || typeof value !== 'object' || depth > 7 || seen.has(value) || seen.size >= 64) return
    seen.add(value)
    yield value
    for (const key of ['cause', 'error', 'data', 'originalError']) yield* visit(errorField(value, key), depth + 1)
    const details = errorField(value, 'details')
    if (typeof details === 'string' && details.length <= 16_384) {
      try { yield* visit(JSON.parse(details) as unknown, depth + 1) } catch { /* Not a JSON response. */ }
    }
  }
  yield* visit(error, 0)
}

export function transactionFailureCode(error: unknown): string {
  let fallback = 'UNKNOWN'
  for (const record of transactionErrorRecords(error)) {
    const reason = errorField(record, 'diagnosticCode') ?? errorField(record, 'reason')
    if (typeof reason === 'string' && Object.hasOwn(TRANSACTION_FAILURE_MESSAGES, reason) && reason !== 'UNKNOWN') {
      if (reason !== 'SUBMISSION_OUTCOME_UNKNOWN') return reason
      fallback = reason
    }
    const code = errorField(record, 'code')
    const name = errorField(record, '_tag') ?? errorField(record, 'name')
    if (code === 4001 || name === 'UserRejectedError') return 'WALLET_DECLINED'
    if (code === 4900 || code === 4901) return 'WALLET_DISCONNECTED'
    if (name === 'TimeoutError') fallback = 'REQUEST_TIMEOUT'
    if (name === 'AbortError') fallback = 'REQUEST_ABORTED'
    if (name === 'HttpRequestError' || name === 'NetworkError') fallback = 'NETWORK_ERROR'
    if (name === 'InsufficientFundsError') return 'INSUFFICIENT_FUNDS'
    if (name === 'ContractRevertError' || name === 'TransactionRevertError' || errorField(record, 'terminalStatus') === 'execution-reverted') fallback = 'CONTRACT_REVERTED'
    const status = errorField(record, 'terminalStatus')
    if (status === 'receipt-timeout') fallback = 'RECEIPT_TIMEOUT'
    if (status === 'signature-declined') return 'WALLET_DECLINED'
    if (status === 'expired') fallback = 'OPERATION_EXPIRED'
    if (status === 'dropped') fallback = 'OPERATION_DROPPED'
    if (status === 'replaced') fallback = 'OPERATION_REPLACED'
    if (status === 'submission-unknown' || status === 'signed-not-submitted') fallback = 'SUBMISSION_OUTCOME_UNKNOWN'
  }
  if (fallback !== 'UNKNOWN') return fallback
  const text = typeof error === 'string' ? error : error && typeof error === 'object'
    ? errorField(error, 'message') : undefined
  if (typeof text !== 'string') return fallback
  // Only output a closed classification; the provider's text never becomes telemetry.
  if (/user (rejected|denied|cancelled|canceled)|rejected by user|signature declined/i.test(text)) return 'WALLET_DECLINED'
  if (/insufficient (funds|token|balance)|not enough.*(funds|balance|gas)/i.test(text)) return 'INSUFFICIENT_FUNDS'
  if (/timeout|timed out/i.test(text)) return 'REQUEST_TIMEOUT'
  if (/network|failed to fetch|http request failed/i.test(text)) return 'NETWORK_ERROR'
  if (/revert/i.test(text)) return 'CONTRACT_REVERTED'
  return fallback
}

/** Keep useful application copy, but never print a provider diagnostic dump. */
export function readableTransactionMessage(message: string | undefined): string {
  if (!message || /^(unknown error|transaction failed|unknown reason)$/i.test(message.trim()) || message.length > 600 || /\S{120}/.test(message)
    || /request arguments:|request body:|raw call arguments:|\bviem@|https?:\/\/|\b0x[\da-f]{40,}|\b(signature|calldata|paymasterData)\s*[:=]/i.test(message)) {
    return TRANSACTION_FAILURE_MESSAGES.UNKNOWN
  }
  return message
}
