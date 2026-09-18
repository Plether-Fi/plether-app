import { asSponsorRequestError, isDefinitiveSponsorshipRefusal, isRecoveryUnauthorized, submissionFailureReason } from './errors'

describe('recovery and submission metadata', () => {
  it('recognizes nested authorization denial without guessing from error text', () => {
    expect(isRecoveryUnauthorized({ cause: { data: { reason: 'RECOVERY_HASH_NOT_AUTHORIZED' } } })).toBe(true)
    expect(isRecoveryUnauthorized(new Error('RECOVERY_HASH_NOT_AUTHORIZED'))).toBe(false)
  })
  it('preserves safe submission reasons but never arbitrary provider data', () => {
    expect(submissionFailureReason({ cause: { data: { reason: 'SECURITY_ATTESTATION_UNAVAILABLE' } } })).toBe('SECURITY_ATTESTATION_UNAVAILABLE')
    expect(submissionFailureReason({ data: { reason: 'https://private/key?secret=123' } })).toBe('SUBMISSION_OUTCOME_UNKNOWN')
    expect(submissionFailureReason(new Error('signed payload'))).toBe('SUBMISSION_OUTCOME_UNKNOWN')
  })
})

describe('asSponsorRequestError', () => {
  it('distinguishes explicit simulation refusals from an ambiguous provider outage', () => {
    for (const reason of ['INSUFFICIENT_FREE_EQUITY','INVALID_ORDER_DEADLINE','SIMULATION_FAILED','MUST_CLOSE_OPPOSING']) expect(isDefinitiveSponsorshipRefusal(reason)).toBe(true)
    expect(isDefinitiveSponsorshipRefusal('BUNDLER_UNAVAILABLE')).toBe(false)
  })
  it('collects JSON-RPC policy metadata through viem error wrappers', () => {
    const rpcCause = Object.assign(new Error('rate limited'), {
      data: {
        reason: 'RATE_LIMITED',
        retryable: true,
        callIndex: 2,
      },
    })
    const viemWrapper = Object.assign(new Error('Limit exceeded'), {
      code: -32005,
      data: undefined,
      cause: rpcCause,
    })

    expect(asSponsorRequestError(viemWrapper)).toMatchObject({
      reason: 'RATE_LIMITED',
      retryable: true,
      callIndex: 2,
      rpcCode: -32005,
    })
  })
})
