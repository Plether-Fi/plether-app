import { describe, expect, it } from 'vitest'
import { AttemptFailureError, classifyAttemptFailure } from '../attemptFailure'
import { BundlerRequestError } from '../errors'

describe('safe interrupted-attempt classification', () => {
  it('retains allowlisted nested gateway reasons without their payload', () => {
    expect(classifyAttemptFailure('submission', { cause: { data: {
      reason: 'SECURITY_ATTESTATION_UNAVAILABLE', signature: 'secret', message: 'private URL',
    } } })).toEqual({ failureStep: 'submission', reasonCode: 'SECURITY_ATTESTATION_UNAVAILABLE' })
  })
  it('distinguishes explicit wallet refusal from generic wallet errors', () => {
    expect(classifyAttemptFailure('wallet_approval', { cause: { code: 4001 } }).reasonCode).toBe('WALLET_DECLINED')
    expect(classifyAttemptFailure('wallet_approval', new Error('User rejected private calldata')).reasonCode).toBe('UNKNOWN')
    expect(classifyAttemptFailure('submission', { code: 4001 }).reasonCode).toBe('UNKNOWN')
    expect(classifyAttemptFailure('wallet_approval', { code: 4900 }).reasonCode).toBe('WALLET_DISCONNECTED')
  })
  it('reports transport timeouts while preserving the submission error itself', () => {
    const error = new BundlerRequestError({ message: 'private URL', retryable: false,
      reason: 'SUBMISSION_OUTCOME_UNKNOWN', terminalStatus: 'submission-unknown',
      cause: new DOMException('secret', 'TimeoutError') })
    expect(classifyAttemptFailure('submission', error).reasonCode).toBe('REQUEST_TIMEOUT')
    expect(error.terminalStatus).toBe('submission-unknown')
    expect(classifyAttemptFailure('confirmation', new BundlerRequestError({ message: 'secret', retryable: false,
      terminalStatus: 'receipt-timeout' })).reasonCode).toBe('RECEIPT_TIMEOUT')
  })
  it('never exports arbitrary labels, messages, cycles or throwing provider properties', () => {
    const cyclic: { cause?: unknown; reason: string } = { reason: 'private_token' }; cyclic.cause = cyclic
    for (const error of [cyclic, 'private signature', new Error('secret'), { get reason() { throw new Error('secret') } }]) {
      expect(classifyAttemptFailure('sponsorship', error)).toEqual({ failureStep: 'sponsorship', reasonCode: 'UNKNOWN' })
    }
  })
  it('records locally diagnosed review changes without matching error text', () => {
    expect(classifyAttemptFailure('review_revalidation', new AttemptFailureError('private details', 'REVIEW_CHANGED')))
      .toEqual({ failureStep: 'review_revalidation', reasonCode: 'REVIEW_CHANGED' })
  })
})
