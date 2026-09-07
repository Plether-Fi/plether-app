import { afterEach, describe, expect, it, vi } from 'vitest'
import { captureFrontendLog } from '../../analytics/client'
import { reportRecoveryDiagnostic } from '../recoveryDiagnostics'

vi.mock('../../analytics/client', () => ({ captureFrontendLog: vi.fn() }))

afterEach(() => { vi.useRealTimers(); vi.clearAllMocks() })

describe('recovery diagnostics', () => {
  it('logs safe-head evidence without operation identifiers and throttles repeated checks', () => {
    vi.useFakeTimers()
    const diagnostic = {
      operationKey: 'private-operation-identifier',
      stage: 'awaiting_safe_head' as const,
      safeBlockNumber: 100n, includedBlockNumber: 110n,
    }
    reportRecoveryDiagnostic(diagnostic)
    reportRecoveryDiagnostic(diagnostic)
    expect(captureFrontendLog).toHaveBeenCalledOnce()
    expect(captureFrontendLog).toHaveBeenCalledWith('info', 'Sponsored operation recovery', {
      component: 'sponsored_operation_recovery', operation: 'reconcile',
      reason_code: 'awaiting_safe_head', safe_block_number: '100', included_block_number: '110',
    })
    vi.advanceTimersByTime(60_000)
    reportRecoveryDiagnostic(diagnostic)
    expect(captureFrontendLog).toHaveBeenCalledTimes(2)
  })

  it('does not interrupt recovery when logging fails', () => {
    vi.mocked(captureFrontendLog).mockImplementationOnce(() => { throw new Error('logging unavailable') })
    expect(() => reportRecoveryDiagnostic({ operationKey: 'second', stage: 'receipt_check_failed' }))
      .not.toThrow()
  })
})
