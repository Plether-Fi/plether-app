import { captureFrontendLog } from '../analytics/client'

type RecoveryStage = 'awaiting_safe_head' | 'receipt_check_failed' |
  'protocol_check_failed' | 'coordination_failed' | 'inclusion_persistence_failed' |
  'canonical_receipt_recovered' | 'canonical_receipt_unverified'

const lastReported = new Map<string, number>()

/** Log bounded diagnostics, never RPC URLs, error bodies or signed operations. */
export function reportRecoveryDiagnostic(input: {
  operationKey: string
  stage: RecoveryStage
  attemptId?: string
  safeBlockNumber?: bigint
  includedBlockNumber?: bigint
}): void {
  try {
    const key = `${input.attemptId ?? input.operationKey}:${input.stage}`
    const now = Date.now()
    const previous = lastReported.get(key)
    if (previous !== undefined && now - previous < 60_000) return
    if (lastReported.size >= 100) {
      const oldest = lastReported.keys().next().value
      if (oldest !== undefined) lastReported.delete(oldest)
    }
    lastReported.set(key, now)
    captureFrontendLog(
      input.stage === 'awaiting_safe_head' || input.stage === 'canonical_receipt_recovered' ? 'info' : 'warn',
      'Sponsored operation recovery',
      {
        component: 'sponsored_operation_recovery',
        operation: 'reconcile',
        reason_code: input.stage,
        attempt_id: input.attemptId,
        recovery_source: input.stage.startsWith('canonical_receipt_') ? 'transaction_hint' : undefined,
        safe_block_number: input.safeBlockNumber?.toString(),
        included_block_number: input.includedBlockNumber?.toString(),
      }
    )
  } catch {
    // Diagnostics cannot interrupt receipt reconciliation or lane release.
  }
}
