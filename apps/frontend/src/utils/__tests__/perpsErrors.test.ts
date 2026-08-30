import { describe, expect, it } from 'vitest'
import { encodeErrorResult, parseAbi } from 'viem'
import {
  getPerpsCloseInvalidReasonMessage,
  getPerpsErrorMessage,
  getPerpsOpenRevertMessage,
  getPerpsOrderFailureMessage,
} from '../perpsErrors'

const PERPS_TEST_ERROR_ABI = parseAbi([
  'error EnforcedPause()',

  'error MarginClearinghouse__ZeroAmount()',
  'error MarginClearinghouse__InsufficientBalance()',
  'error MarginClearinghouse__InsufficientFreeEquity()',
  'error MarginClearinghouse__InsufficientUsdcForSettlement()',

  'error OrderRouter__ZeroSize()',
  'error OrderRouter__CommitValidation(uint8 code)',
  'error OrderRouter__PredictableOpenInvalid(uint8 code)',
  'error OrderRouter__CloseWithPositiveMargin()',
  'error OrderRouter__NoQueuedPosition()',
  'error OrderRouter__SideMismatch()',
  'error OrderRouter__SizeExceedsQueued()',
  'error OrderRouter__TooManyPendingOrders()',
  'error OrderRouter__DegradedMode()',
  'error OrderRouter__CloseOnlyWindow()',
  'error OrderRouter__NotInSeedLifecycle()',
  'error OrderRouter__VaultRiskBlocked()',
  'error OrderRouter__EmptyPythUpdateData()',
  'error OrderRouter__InsufficientPythFee()',
  'error OrderRouter__OraclePriceTooStale()',
  'error OrderRouter__OracleConfidenceTooWide()',
  'error OrderRouter__OraclePublishTimesDiverged()',
  'error OrderRouter__InvalidOraclePrice()',
  'error OrderRouter__MarkPriceOutOfOrder()',
  'error OrderRouter__MevDetected()',
  'error OrderRouter__NoOrdersToExecute()',
  'error OrderRouter__OrderNotQueueHead()',
  'error OrderRouter__BatchBeforeQueueHead()',
  'error OrderRouter__BatchOrderNotCommitted()',
  'error OrderRouter__OrderNotPending()',
  'error OrderRouter__InsufficientGas()',
  'error OrderRouter__MarginQueueCorrupt()',
  'error OrderRouter__AccountQueueCorrupt()',
  'error OrderRouter__GlobalQueueCorrupt()',
  'error OrderRouter__Unauthorized()',

  'error PletherOracle__MissingUpdateData()',
  'error PletherOracle__InsufficientFee(uint256 provided,uint256 required)',
  'error PletherOracle__PriceOutOfOrder(uint64 publishTime,uint64 lastMarkTime)',
  'error PletherOracle__StalePrice(uint8 mode, bytes32 feedId, uint256 publishTime, uint256 maxStaleness, uint256 currentTimestamp)',
  'error PletherOracle__InvalidPrice(bytes32 feedId, int64 price)',
  'error PletherOracle__ConfidenceTooWide(bytes32 feedId, uint64 confidence, int64 price, uint256 maxConfidenceBps)',
  'error PletherOracle__PublishTimeDivergence(uint8 mode,uint256 minPublishTime,uint256 maxPublishTime,uint256 maxDivergence)',
  'error PletherOracle__ZeroBasketPrice()',

  'error CfdEngine__TypedOrderFailure(uint8 failureCategory,uint8 failureCode,bool isClose)',
  'error CfdEngine__MustCloseOpposingPosition()',
  'error CfdEngine__DegradedMode()',
  'error CfdEngine__PositionTooSmall()',
  'error CfdEngine__SkewTooHigh()',
  'error CfdEngine__MarginDrainedByFees()',
  'error CfdEngine__InsufficientInitialMargin()',
  'error CfdEngine__PoolSolvencyExceeded()',
  'error CfdEngine__InsufficientPoolLiquidity()',
  'error CfdEngine__CloseSizeExceedsPosition()',
  'error CfdEngine__DustPosition()',
  'error CfdEngine__PartialCloseUnderwaterCarry()',
  'error CfdEngine__NoOpenPosition()',
  'error CfdEngine__WithdrawBlockedByOpenPosition()',
  'error CfdEngine__MarkPriceStale()',
  'error CfdEngine__MarkPriceOutOfOrder()',
  'error CfdEngine__InsufficientCloseOrderBountyBacking()',
])

const ZERO_FEED_ID = '0x0000000000000000000000000000000000000000000000000000000000000000'
const FEED_ID = '0x1111111111111111111111111111111111111111111111111111111111111111'

function encodedErrorMessage(errorName: string, args: readonly unknown[], action: Parameters<typeof getPerpsErrorMessage>[1] = 'commit'): string {
  const data = encodeErrorResult({
    abi: PERPS_TEST_ERROR_ABI,
    errorName,
    args,
  })
  return getPerpsErrorMessage({ cause: { raw: data } }, action)
}

describe('getPerpsErrorMessage', () => {
  it('preserves instrumented commit receipt diagnostics', () => {
    const message = 'Commit reverted after wallet confirmation, but the receipt did not include decodable revert data. Failed tx: 0x123.'

    expect(getPerpsErrorMessage(new Error(message), 'commit')).toBe(message)
  })

  it('describes commit-block timing guard as retryable reveal-not-ready state', () => {
    const data = encodeErrorResult({
      abi: PERPS_TEST_ERROR_ABI,
      errorName: 'OrderRouter__MevDetected',
      args: [],
    })

    const message = getPerpsErrorMessage({ cause: { raw: data } }, 'execute')

    expect(message).toBe('Reveal is not ready yet. Wait a few seconds and retry self-execute.')
    expect(message).not.toContain('failed')
  })

  it('describes historical Pyth parse rejection separately from expired price data', () => {
    const data = encodeErrorResult({
      abi: PERPS_TEST_ERROR_ABI,
      errorName: 'PletherOracle__StalePrice',
      args: [0, ZERO_FEED_ID, 1_781_096_515n, 60n, 1_781_096_515n],
    })

    const message = getPerpsErrorMessage({ cause: { raw: data } }, 'execute')

    expect(message).toContain('Historical Pyth update was rejected')
    expect(message).toContain('not expired')
    expect(message).toContain('unique historical tick after commit')
    expect(message).not.toContain('expired before the transaction landed')
  })

  it('still describes genuinely stale Pyth data as expired', () => {
    const data = encodeErrorResult({
      abi: PERPS_TEST_ERROR_ABI,
      errorName: 'PletherOracle__StalePrice',
      args: [0, FEED_ID, 1_781_096_400n, 60n, 1_781_096_515n],
    })

    const message = getPerpsErrorMessage({ cause: { raw: data } }, 'execute')

    expect(message).toContain('Pyth price data expired before the transaction landed')
    expect(message).toContain('age: 115s')
    expect(message).toContain('limit: 60s')
  })

  it.each([
    ['EnforcedPause', [], 'router is paused'],
    ['MarginClearinghouse__ZeroAmount', [], 'greater than zero'],
    ['MarginClearinghouse__InsufficientBalance', [], 'balance is too low'],
    ['MarginClearinghouse__InsufficientFreeEquity', [], 'Not enough free margin'],
    ['MarginClearinghouse__InsufficientUsdcForSettlement', [], 'does not have enough USDC settlement'],

    ['OrderRouter__ZeroSize', [], 'Order size must be greater than zero'],
    ['OrderRouter__CommitValidation', [11], 'below the minimum executable size'],
    ['OrderRouter__PredictableOpenInvalid', [4], 'Market skew is too high'],
    ['OrderRouter__PredictableOpenInvalid', [8], '100 plDXY increments'],
    ['OrderRouter__CloseWithPositiveMargin', [], 'cannot add margin'],
    ['OrderRouter__NoQueuedPosition', [], 'no queued or live position'],
    ['OrderRouter__SideMismatch', [], 'does not match your current position'],
    ['OrderRouter__SizeExceedsQueued', [], 'larger than the current queued position'],
    ['OrderRouter__TooManyPendingOrders', [], 'too many pending orders'],
    ['OrderRouter__DegradedMode', [], 'market is degraded'],
    ['OrderRouter__CloseOnlyWindow', [], 'market is close-only'],
    ['OrderRouter__NotInSeedLifecycle', [], 'LP pool is still in seed lifecycle'],
    ['OrderRouter__VaultRiskBlocked', [], 'LP vault is blocking'],
    ['OrderRouter__EmptyPythUpdateData', [], 'Missing Pyth price update data', 'execute'],
    ['OrderRouter__InsufficientPythFee', [], 'Pyth update fee changed', 'execute'],
    ['OrderRouter__OraclePriceTooStale', [], 'Pyth price data expired', 'execute'],
    ['OrderRouter__OracleConfidenceTooWide', [], 'Pyth confidence is too wide', 'execute'],
    ['OrderRouter__OraclePublishTimesDiverged', [], 'component prices are not aligned', 'execute'],
    ['OrderRouter__InvalidOraclePrice', [], 'invalid basket price', 'execute'],
    ['OrderRouter__MarkPriceOutOfOrder', [], 'older than the stored mark', 'execute'],
    ['OrderRouter__NoOrdersToExecute', [], 'no longer pending', 'execute'],
    ['OrderRouter__OrderNotQueueHead', [], 'blocked by an earlier pending order', 'execute'],
    ['OrderRouter__BatchBeforeQueueHead', [], 'Batch execution target is before the queue head', 'execute'],
    ['OrderRouter__BatchOrderNotCommitted', [], 'has not been committed yet', 'execute'],
    ['OrderRouter__OrderNotPending', [], 'no longer pending', 'execute'],
    ['OrderRouter__InsufficientGas', [], 'needs more gas', 'execute'],
    ['OrderRouter__MarginQueueCorrupt', [], 'order queue is inconsistent', 'execute'],
    ['OrderRouter__AccountQueueCorrupt', [], 'order queue is inconsistent', 'execute'],
    ['OrderRouter__GlobalQueueCorrupt', [], 'order queue is inconsistent', 'execute'],
    ['OrderRouter__Unauthorized', [], 'not authorized'],

    ['PletherOracle__MissingUpdateData', [], 'Missing Pyth price update data', 'execute'],
    ['PletherOracle__InsufficientFee', [1n, 2n], 'Pyth update fee changed', 'execute'],
    ['PletherOracle__PriceOutOfOrder', [1n, 2n], 'older than the stored mark', 'execute'],
    ['PletherOracle__InvalidPrice', [FEED_ID, 0n], 'invalid basket price', 'execute'],
    ['PletherOracle__ConfidenceTooWide', [FEED_ID, 1n, 1n, 2n], 'Pyth confidence is too wide', 'execute'],
    ['PletherOracle__PublishTimeDivergence', [0, 1n, 10n, 5n], 'component prices are not aligned', 'execute'],
    ['PletherOracle__ZeroBasketPrice', [], 'invalid basket price', 'execute'],

    ['CfdEngine__TypedOrderFailure', [0, 4, false], 'Market skew is too high', 'execute'],
    ['CfdEngine__TypedOrderFailure', [0, 2, true], 'remaining position would be too small', 'execute'],
    ['CfdEngine__MustCloseOpposingPosition', [], 'opposing position'],
    ['CfdEngine__DegradedMode', [], 'market is degraded'],
    ['CfdEngine__PositionTooSmall', [], 'below the minimum size'],
    ['CfdEngine__SkewTooHigh', [], 'Market skew is too high'],
    ['CfdEngine__MarginDrainedByFees', [], 'drain the margin'],
    ['CfdEngine__InsufficientInitialMargin', [], 'Initial margin is too low'],
    ['CfdEngine__PoolSolvencyExceeded', [], 'LP pool does not have enough'],
    ['CfdEngine__InsufficientPoolLiquidity', [], 'LP pool does not have enough'],
    ['CfdEngine__CloseSizeExceedsPosition', [], 'larger than the current position'],
    ['CfdEngine__DustPosition', [], 'remaining position would be too small'],
    ['CfdEngine__PartialCloseUnderwaterCarry', [], 'partial close would leave the position underwater'],
    ['CfdEngine__NoOpenPosition', [], 'no open position'],
    ['CfdEngine__WithdrawBlockedByOpenPosition', [], 'Withdrawal is blocked'],
    ['CfdEngine__MarkPriceStale', [], 'mark price is stale'],
    ['CfdEngine__MarkPriceOutOfOrder', [], 'out-of-order mark price'],
    ['CfdEngine__InsufficientCloseOrderBountyBacking', [], 'not enough margin backing'],
  ] satisfies Array<[string, readonly unknown[], string, Parameters<typeof getPerpsErrorMessage>[1]?]>)(
    'maps %s to explicit copy',
    (errorName, args, expected, action = 'commit') => {
      expect(encodedErrorMessage(errorName, args, action)).toContain(expected)
    }
  )

  it.each([
    [0, 'order is valid'],
    [1, 'opposing position'],
    [2, 'market is degraded'],
    [3, 'below the minimum size'],
    [4, 'Market skew is too high'],
    [5, 'drain the margin'],
    [6, 'Initial margin is too low'],
    [7, 'LP pool does not have enough'],
  ])('maps open revert code %i', (code, expected) => {
    expect(getPerpsOpenRevertMessage(code)).toContain(expected)
  })

  it.each([
    [0, 'expired before it could be revealed'],
    [1, 'close-only'],
    [2, 'slippage'],
    [3, 'internal panic'],
    [4, 'liquidated'],
    [5, 'rejected the order'],
  ])('maps order failure reason %i', (reason, expected) => {
    expect(getPerpsOrderFailureMessage(reason)).toContain(expected)
  })

  it.each([
    [0, 'close order is valid'],
    [1, 'No current position'],
    [2, 'Reduce size is invalid'],
    [3, 'remaining position would be too small'],
    [4, 'partial close would leave the position underwater'],
  ])('maps close invalid reason %i', (reason, expected) => {
    expect(getPerpsCloseInvalidReasonMessage(reason)).toContain(expected)
  })
})
