import { describe, expect, it } from 'vitest'
import { encodeErrorResult, parseAbi } from 'viem'
import { COMMIT_UNDECODED_FALLBACK_MESSAGE } from '../perpsErrors'
import { getPreparationFailureProperties, preparationFailure, withPreparationStep } from '../perpsPreparationDiagnostics'

describe('preparation diagnostics privacy', () => {
  it('retains the inner failing step through UI normalization and decodes only the ABI name', async () => {
    const raw = Object.assign(new Error('reverted for user@example.com with signed payload'), {
      data: encodeErrorResult({ abi: parseAbi(['error OrderRouter__CommitValidation(uint8 code)']),
        errorName: 'OrderRouter__CommitValidation', args: [11] }),
    })
    let wrapped: unknown
    try {
      await withPreparationStep('review_validation', undefined, () =>
        withPreparationStep('order_assessment', 'assessOrder', () => Promise.reject(raw)))
    } catch (cause) {
      expect(cause).toBe(raw)
      wrapped = new Error(COMMIT_UNDECODED_FALLBACK_MESSAGE, { cause })
    }
    expect(getPreparationFailureProperties(wrapped)).toEqual({
      error_code: 'OrderRouter__CommitValidation', stage: 'order_assessment', contract_function: 'assessOrder',
    })
  })

  it('never emits arbitrary error names, messages, functions or RPC payloads', () => {
    const raw = { errorName: 'wallet@example.com', functionName: 'secret payload', message: 'execution reverted',
      data: '0x12345678', args: ['Bearer secret'] }
    expect(getPreparationFailureProperties(preparationFailure(raw, 'commit_simulation', 'commitOrder'))).toEqual({
      error_code: 'undecoded_revert', stage: 'commit_simulation', contract_function: 'commitOrder',
    })
  })

  it.each([
    [new Error('HTTP request failed. https://private.example/key'), 'network_failure'],
    [new Error('Request timed out for user@example.com'), 'timeout'],
    [new Error(COMMIT_UNDECODED_FALLBACK_MESSAGE), 'undecoded_revert'],
    [new Error('private failure details'), 'unknown'],
    [Object.assign(new Error('amount 123'), { name: 'PerpsOrderFundingShortfallError' }), 'funding_shortfall'],
    [Object.assign(new Error('exact leverage'), { name: 'PerpsOrderReviewError', reason: 'leverage' }), 'review_leverage'],
  ])('classifies without forwarding exception text: %s', (error, errorCode) => {
    expect(getPreparationFailureProperties(error)).toEqual({ stage: 'preflight', error_code: errorCode })
  })

  it('handles primitive rejections, cycles and throwing getters without throwing', async () => {
    const cycle: { cause?: unknown } = {}
    cycle.cause = cycle
    const malformed = Object.defineProperty({}, 'cause', { get() { throw new Error('secret') } })
    expect(getPreparationFailureProperties(cycle).error_code).toBe('unknown')
    expect(getPreparationFailureProperties(malformed).error_code).toBe('unknown')
    await expect(withPreparationStep('context_read', 'getLatestPrice', () => Promise.reject('execution reverted')))
      .rejects.toSatisfy((error: unknown) => getPreparationFailureProperties(error).error_code === 'undecoded_revert')
  })
})
