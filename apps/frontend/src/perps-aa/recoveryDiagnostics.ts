import { captureFrontendLog } from '../analytics/client'

type RecoveryStage = 'awaiting_safe_head' | 'receipt_check_failed' |
  'protocol_check_failed' | 'coordination_failed' | 'inclusion_persistence_failed'

const lastReported = new Map<string, number>()

/** Log bounded diagnostics, never RPC URLs, error bodies or signed operations. */
export function reportRecoveryDiagnostic(input: {
  operationKey: string
  stage: RecoveryStage
  safeBlockNumber?: bigint
  includedBlockNumber?: bigint
}): void {
  try {
    const key = `${input.operationKey}:${input.stage}`
    const now = Date.now()
    const previous = lastReported.get(key)
    if (previous !== undefined && now - previous < 60_000) return
    if (lastReported.size >= 100) {
      const oldest = lastReported.keys().next().value
      if (oldest !== undefined) lastReported.delete(oldest)
    }
    lastReported.set(key, now)
    captureFrontendLog(
      input.stage === 'awaiting_safe_head' ? 'info' : 'warn',
      'Sponsored operation recovery',
      {
        component: 'sponsored_operation_recovery',
        operation: 'reconcile',
        reason_code: input.stage,
        safe_block_number: input.safeBlockNumber?.toString(),
        included_block_number: input.includedBlockNumber?.toString(),
      }
    )
  } catch {
    // Diagnostics cannot interrupt receipt reconciliation or lane release.
  }
}
