import { beforeEach, describe, expect, it, vi } from 'vitest'
import { HttpRequestError } from 'viem'
import { reportTransactionFailure, reportedTransactionError } from './transactionErrors'
import { captureAnalyticsEvent, captureFrontendLog, sanitizeAnalyticsProperties } from './client'

vi.mock('./client', async importOriginal => ({
  ...await importOriginal<typeof import('./client')>(), captureAnalyticsEvent: vi.fn(), captureFrontendLog: vi.fn(),
}))
const context = { surface: 'perps' as const, action: 'commit', stage: 'submission' }
beforeEach(() => { vi.clearAllMocks() })

describe('transaction failure reporting', () => {
  it('correlates wrapped HTTP deadline refusals with the sponsored attempt without exposing payloads', () => {
    const attemptId = crypto.randomUUID()
    const original = new HttpRequestError({ url: 'https://private.example/secret', status: 400,
      body: { signature: 'secret-signature', account: `0x${'12'.repeat(20)}` },
      details: JSON.stringify({ data: { reason: 'DEADLINE_TOO_CLOSE' } }),
    })
    const first = reportTransactionFailure(original, undefined, { ...context, attemptId })
    const wrapped = reportedTransactionError(new Error('wrapper', { cause: original }), 'Approval took too long.', context)
    expect(wrapped.message).toContain(`Support reference: ${attemptId}`)
    expect(first.errorCode).toBe('DEADLINE_TOO_CLOSE')
    expect(first.message).toContain('Too little time remained')
    expect(captureAnalyticsEvent).toHaveBeenCalledOnce()
    expect(captureAnalyticsEvent).toHaveBeenCalledWith('transaction failed', expect.objectContaining({
      support_reference: attemptId, attempt_id: attemptId, reason_code: 'DEADLINE_TOO_CLOSE', stage: 'submission',
    }))
    expect(JSON.stringify(vi.mocked(captureAnalyticsEvent).mock.calls)).not.toMatch(/secret|0x12|private\.example/)
    expect(captureFrontendLog).toHaveBeenCalledOnce()
  })

  it('keeps a transaction hash in the UI only and reports new failures with different references', () => {
    const hash = `0x${'ab'.repeat(32)}`
    const error = Object.assign(new Error('provider dump'), { transactionHash: hash })
    const result = reportedTransactionError(error, 'Confirmation failed.', context)
    expect(result.message).toContain(`Transaction: ${hash}`)
    expect(result.message).toMatch(/Support reference: tx-[\da-f-]{36}/)
    const next = reportTransactionFailure(new Error('other failure'), undefined, context)
    expect(result.message).not.toContain(next.supportReference)
    expect(JSON.stringify(vi.mocked(captureAnalyticsEvent).mock.calls)).not.toContain(hash)
  })

  it('does not duplicate a known failure when the recovery store only knows that it failed', () => {
    const attemptId = crypto.randomUUID()
    reportTransactionFailure({ code: 4001 }, undefined, { ...context, attemptId })
    const stored = reportTransactionFailure({ terminalStatus: 'signature-declined' }, undefined, { ...context, attemptId })
    reportTransactionFailure({ reason: 'UNKNOWN' }, undefined, { ...context, attemptId })
    expect(stored.errorCode).toBe('WALLET_DECLINED')
    expect(captureAnalyticsEvent).toHaveBeenCalledOnce()
  })

  it('keeps a single support reference when a reported error crosses another UI boundary', () => {
    const error = reportedTransactionError(new Error('Failed to fetch'), 'Failed to fetch', context)
    const wrapped = reportedTransactionError(error, error.message, context)
    expect(wrapped.message.match(/Support reference:/g)).toHaveLength(1)
    expect(wrapped.message).toBe(error.message)
    expect(captureAnalyticsEvent).toHaveBeenCalledOnce()
  })

  it('records a changed recovery outcome for the same attempt', () => {
    const attemptId = crypto.randomUUID()
    reportTransactionFailure({ reason: 'SUBMISSION_OUTCOME_UNKNOWN' }, undefined, { ...context, attemptId })
    reportTransactionFailure({ terminalStatus: 'execution-reverted' }, undefined, { ...context, attemptId })
    expect(captureAnalyticsEvent).toHaveBeenCalledTimes(2)
  })

  it('survives telemetry failures and hostile or cyclic provider objects', () => {
    vi.mocked(captureAnalyticsEvent).mockImplementationOnce(() => { throw new Error('offline') })
    const error = Object.create(null) as Record<string, unknown>
    error.cause = error
    Object.defineProperty(error, 'reason', { get() { throw new Error('hostile getter') } })
    expect(() => reportTransactionFailure(error, 'HTTP request failed. Request body: secret', context)).not.toThrow()
    expect(reportTransactionFailure(undefined, 'Raw Call Arguments: secret', context).message).not.toContain('secret')
  })

  it('allows random support references through the real analytics filter but rejects arbitrary identifiers', () => {
    const support_reference = `tx-${crypto.randomUUID()}`
    expect(sanitizeAnalyticsProperties({ support_reference, build_commit: 'abc123', signature: 'secret' }))
      .toEqual({ support_reference, build_commit: 'abc123' })
    expect(sanitizeAnalyticsProperties({ support_reference: 'https://private.example/key' })).toEqual({})
  })
})
