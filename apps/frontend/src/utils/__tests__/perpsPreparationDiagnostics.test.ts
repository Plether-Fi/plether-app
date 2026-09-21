import { describe, expect, it } from 'vitest'
import { encodeErrorResult, parseAbi } from 'viem'
import { createOracleSyncError } from '../../test/fixtures/oracleSyncError'
import { COMMIT_UNDECODED_FALLBACK_MESSAGE } from '../perpsErrors'
import { getPreparationDebugContext, getPreparationFailureProperties, preparationFailure, withPreparationStep } from '../perpsPreparationDiagnostics'

describe('preparation diagnostics privacy', () => {
  it.each([true, false])('classifies a real oracle error without leaking its payload (legacy ABI=%s)', legacyAbi => {
    const cause = preparationFailure(createOracleSyncError(legacyAbi), 'context_read', 'getLatestPrice')
    const wrapped = new Error('Review wrapper', { cause })
    expect(getPreparationFailureProperties(wrapped)).toEqual({
      error_code: 'PletherOracle__PriceOutOfOrder', stage: 'context_read', contract_function: 'getLatestPrice',
    })
  })
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

  it('retains close RPC context locally and sends only a bounded sample label', async () => {
    const context = { chainId: 421614, address: 'lens-address', account: 'private-account', blockNumber: 123n, blockHash: 'block-hash', samplePrice: 102_500_000n, assessmentPoint: 'limit' as const }
    const failure = Object.assign(new Error('private RPC request'), { data: '0x4e487b710000000000000000000000000000000000000000000000000000000000000011' })
    const error = await withPreparationStep('order_assessment', 'previewClose', () => Promise.reject(failure), context).catch(cause => new Error('Review failed', { cause }))
    expect(getPreparationDebugContext(error)).toEqual({ context, error: failure })
    expect(getPreparationFailureProperties(error)).toEqual({ stage: 'order_assessment', contract_function: 'previewClose', error_code: 'arithmetic_panic', assessment_point: 'limit' })
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
    [Object.assign(new Error('private position state'), { name: 'PerpsOrderPositionConflictError' }), 'MUST_CLOSE_OPPOSING'],
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
