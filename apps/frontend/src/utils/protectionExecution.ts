import type { PositionProtection } from '../contracts/positionProtection'

export const PROTECTION_EXECUTION_REASONS = ['monitoring', 'trigger-ready', 'retry-ready', 'oracle-frozen', 'oracle-unavailable', 'pending-orders', 'queue-congested', 'queue-cleanup', 'operator-required', 'execution-disabled', 'check-failed', 'inactive'] as const
export type ProtectionExecutionReason = typeof PROTECTION_EXECUTION_REASONS[number]
export const PROTECTION_EXECUTION_MAX_AGE_SECONDS = 60
export interface ProtectionExecutionObservation {
  protectionId: string
  account: string
  linkedOrderId: string
  protectionStatus: number
  reason: ProtectionExecutionReason
  checkedBlock: string
  checkedBlockHash: string
  checkedAt: string
  ageSeconds: number
  outcomeReason?: number
  transactionHash?: string | null
  transactionAction?: 'trigger' | 'retry' | 'prune' | null
}
export interface ProtectionExecutionReport {
  observation: ProtectionExecutionObservation | null
  receivedAt: number
}

export function parseProtectionExecutionReport(value: unknown): ProtectionExecutionReport {
  const fail = () => { throw new Error('Execution status is temporarily unavailable') }
  if (!value || typeof value !== 'object' || !('observation' in value)) return fail()
  if (value.observation === null) return { observation: null, receivedAt: Date.now() }
  const row = value.observation
  if (!row || typeof row !== 'object') return fail()
  const record = row as Record<string, unknown>
  for (const field of ['protectionId', 'linkedOrderId', 'checkedBlock']) {
    if (typeof record[field] !== 'string' || !/^(0|[1-9]\d*)$/.test(record[field])) return fail()
  }
  if (typeof record.account !== 'string' || !/^0x[\da-f]{40}$/i.test(record.account) ||
      typeof record.checkedBlockHash !== 'string' || !/^0x[\da-f]{64}$/i.test(record.checkedBlockHash) ||
      typeof record.checkedAt !== 'string' || !Number.isFinite(Date.parse(record.checkedAt)) ||
      typeof record.ageSeconds !== 'number' || !Number.isFinite(record.ageSeconds) || record.ageSeconds < 0 ||
      !Number.isInteger(record.protectionStatus) || Number(record.protectionStatus) < 0 || Number(record.protectionStatus) > 8 ||
      !PROTECTION_EXECUTION_REASONS.includes(record.reason as ProtectionExecutionReason)) return fail()
  if (record.outcomeReason !== undefined && (!Number.isInteger(record.outcomeReason) || Number(record.outcomeReason) < 0)) return fail()
  if (record.transactionHash != null && (typeof record.transactionHash !== 'string' || !/^0x[\da-f]{64}$/i.test(record.transactionHash))) return fail()
  if (record.transactionAction != null && (typeof record.transactionAction !== 'string' || !['trigger', 'retry', 'prune'].includes(record.transactionAction))) return fail()
  if (Boolean(record.transactionHash) !== Boolean(record.transactionAction)) return fail()
  return { observation: record as unknown as ProtectionExecutionObservation, receivedAt: Date.now() }
}

export function currentProtectionObservation(report: ProtectionExecutionReport | undefined, protection: PositionProtection, now: number): ProtectionExecutionObservation | undefined {
  if (!report) return undefined
  const row = report.observation
  if (row?.protectionId !== protection.protectionId.toString() ||
      row.account.toLowerCase() !== protection.account.toLowerCase() || row.linkedOrderId !== protection.linkedOrderId.toString() ||
      row.protectionStatus !== protection.status || row.ageSeconds + Math.max(0, now - report.receivedAt) / 1000 >= PROTECTION_EXECUTION_MAX_AGE_SECONDS) return undefined
  if (protection.status === 8 && ['monitoring', 'trigger-ready', 'oracle-frozen', 'inactive'].includes(row.reason)) return undefined
  if (protection.status === 2 && ['retry-ready', 'pending-orders', 'queue-congested', 'queue-cleanup', 'operator-required', 'inactive'].includes(row.reason)) return undefined
  return row
}

export const PROTECTION_EXECUTION_COPY: Record<ProtectionExecutionReason, { title: string; body: string }> = {
  monitoring: { title: 'Watching your trigger prices', body: 'The worker checked the market and did not find an eligible TP/SL trigger at its last check.' },
  'trigger-ready': { title: 'Preparing the close', body: 'A trigger was reached. The worker is preparing a close transaction; it is not confirmed yet.' },
  'retry-ready': { title: 'Preparing a retry', body: 'The last close expired and the retry checks passed. A new close is not confirmed yet.' },
  'oracle-frozen': { title: 'Waiting for live prices', body: 'New TP/SL triggers are paused while the oracle is frozen. Your existing trigger prices remain set.' },
  'oracle-unavailable': { title: 'Waiting for a usable oracle price', body: 'The worker cannot proceed without an accepted oracle update. It will check again automatically.' },
  'pending-orders': { title: 'Waiting for your pending orders', body: 'Another order on this account must finish before the worker can retry this close.' },
  'queue-congested': { title: 'Waiting for the execution queue', body: 'The queue is currently too busy to retry within the order expiry window. The worker will check again.' },
  'queue-cleanup': { title: 'Clearing an expired queue entry', body: 'An expired order at the head of the queue must be removed before retrying. Your close is not requeued yet.' },
  'operator-required': { title: 'Operator review required', body: 'The last attempt failed for a reason that is not automatically retried. The original trigger remains binding; operator intervention is needed.' },
  'execution-disabled': { title: 'Automatic execution is paused', body: 'The worker is in monitoring-only mode and will not submit triggers or retries. This does not remove your on-chain TP/SL.' },
  'check-failed': { title: 'Execution check did not complete', body: 'The worker could not finish its checks or prepare the transaction. A retry is not confirmed. It will check again.' },
  inactive: { title: 'Waiting for updated contract state', body: 'The worker observed a different protection state. The on-chain status above remains the source of truth.' },
}
