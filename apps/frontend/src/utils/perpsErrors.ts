import { decodeErrorResult, parseAbi } from 'viem'

type PerpsAction = 'approve' | 'deposit' | 'withdraw' | 'addPositionMargin' | 'commit' | 'execute'

const PERPS_ERROR_ABI = parseAbi([
  'error EnforcedPause()',

  'error MarginClearinghouse__NotOperator()',
  'error MarginClearinghouse__NotAccountOwner()',
  'error MarginClearinghouse__ZeroAmount()',
  'error MarginClearinghouse__InsufficientBalance()',
  'error MarginClearinghouse__InsufficientFreeEquity()',
  'error MarginClearinghouse__InsufficientUsdcForSettlement()',
  'error MarginClearinghouse__InsufficientAssetToSeize()',
  'error MarginClearinghouse__InvalidMarginBucket()',
  'error MarginClearinghouse__ReservationAlreadyExists()',
  'error MarginClearinghouse__ReservationNotActive()',
  'error MarginClearinghouse__IncompleteReservationCoverage()',
  'error MarginClearinghouse__ReservationLedgerActive()',
  'error MarginClearinghouse__InsufficientBucketMargin()',

  'error OrderRouter__ZeroSize()',
  'error OrderRouter__CommitValidation(uint8 code)',
  'error OrderRouter__InvalidPletherOracle()',
  'error OrderRouter__EmptyPythUpdateData()',
  'error OrderRouter__InsufficientPythFee()',
  'error OrderRouter__InvalidEngineLens()',
  'error OrderRouter__InvalidOraclePrice()',
  'error OrderRouter__MarkPriceOutOfOrder()',
  'error OrderRouter__OraclePriceTooStale()',
  'error OrderRouter__OracleConfidenceTooWide()',
  'error OrderRouter__MevDetected()',
  'error OrderRouter__OraclePublishTimesDiverged()',
  'error OrderRouter__NoOrdersToExecute()',
  'error OrderRouter__OrderNotQueueHead()',
  'error OrderRouter__BatchBeforeQueueHead()',
  'error OrderRouter__BatchOrderNotCommitted()',
  'error OrderRouter__OrderNotPending()',
  'error OrderRouter__MarginQueueCorrupt()',
  'error OrderRouter__AccountQueueCorrupt()',
  'error OrderRouter__GlobalQueueCorrupt()',
  'error OrderRouter__NotInSeedLifecycle()',
  'error OrderRouter__VaultRiskBlocked()',
  'error OrderRouter__CloseWithPositiveMargin()',
  'error OrderRouter__NoQueuedPosition()',
  'error OrderRouter__SideMismatch()',
  'error OrderRouter__SizeExceedsQueued()',
  'error OrderRouter__InsufficientFreeEquity()',
  'error OrderRouter__TooManyPendingOrders()',
  'error OrderRouter__Unauthorized()',
  'error OrderRouter__DegradedMode()',
  'error OrderRouter__CloseOnlyWindow()',
  'error OrderRouter__InsufficientGas()',
  'error OrderRouter__PredictableOpenInvalid(uint8 code)',

  'error PletherOracle__MissingUpdateData()',
  'error PletherOracle__InsufficientFee(uint256 provided,uint256 required)',
  'error PletherOracle__PriceOutOfOrder(uint64 publishTime,uint64 lastMarkTime)',
  'error PletherOracle__StalePrice(uint8 mode, bytes32 feedId, uint256 publishTime, uint256 maxStaleness, uint256 currentTimestamp)',
  'error PletherOracle__InvalidPrice(bytes32 feedId, int64 price)',
  'error PletherOracle__ConfidenceTooWide(bytes32 feedId, uint64 confidence, int64 price, uint256 maxConfidenceBps)',
  'error PletherOracle__PublishTimeDivergence(uint8 mode,uint256 minPublishTime,uint256 maxPublishTime,uint256 maxDivergence)',
  'error PletherOracle__ZeroBasketPrice()',

  'error CfdEngine__TypedOrderFailure(uint8 failureCategory,uint8 failureCode,bool isClose)',
  'error CfdEngine__NotAccountOwner()',
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

const OPEN_REVERT_MESSAGES: Record<number, string> = {
  0: 'The order is valid.',
  1: 'You have an opposing position. Close or reduce it before opening this side.',
  2: 'The market is degraded, so new risk cannot be opened right now.',
  3: 'The resulting position would be below the minimum size.',
  4: 'Market skew is too high for this side right now.',
  5: 'Fees and price impact would drain the margin for this order.',
  6: 'Initial margin is too low for this order. Lower leverage or reduce size.',
  7: 'The LP pool does not have enough solvency buffer for this order.',
}

const COMMIT_VALIDATION_MESSAGES: Record<number, string> = {
  11: 'Order size is below the minimum executable size.',
}

const CLOSE_REVERT_MESSAGES: Record<number, string> = {
  0: 'The close order is valid.',
  1: 'Reduce size is larger than the current position.',
  2: 'The remaining position would be too small. Reduce less or close the full position.',
  3: 'This partial close would leave the position underwater. Reduce less or close the full position.',
}

const CLOSE_INVALID_REASON_MESSAGES: Record<number, string> = {
  0: 'The close order is valid.',
  1: 'No current position to reduce or close.',
  2: 'Reduce size is invalid for the current position.',
  3: 'The remaining position would be too small. Reduce less or close the full position.',
  4: 'This partial close would leave the position underwater. Reduce less or close the full position.',
}

export const PERPS_ORDER_FAILURE_MESSAGES: Record<number, string> = {
  0: 'The order expired before it could be revealed. Create a fresh order.',
  1: 'The market switched to close-only before execution.',
  2: 'Execution exceeded your slippage setting. Increase slippage or retry with a fresh order.',
  3: 'The engine hit an internal panic while settling this order.',
  4: 'The account was liquidated before this order executed.',
  5: 'The engine rejected the order during reveal.',
}

function getNestedString(error: unknown, keys: string[], depth = 0): string | undefined {
  if (!error || typeof error !== 'object' || depth > 6) return undefined
  const record = error as Record<string, unknown>
  for (const key of keys) {
    const value = record[key]
    if (typeof value === 'string' && value) return value
  }
  for (const value of Object.values(record)) {
    const nested = getNestedString(value, keys, depth + 1)
    if (nested) return nested
  }
  return undefined
}

function getNestedArgs(error: unknown, depth = 0): readonly unknown[] | undefined {
  if (!error || typeof error !== 'object' || depth > 6) return undefined
  const record = error as Record<string, unknown>
  if (Array.isArray(record.args)) return record.args
  for (const value of Object.values(record)) {
    const nested = getNestedArgs(value, depth + 1)
    if (nested) return nested
  }
  return undefined
}

function extractRevertData(error: unknown, depth = 0): string | undefined {
  if (!error || depth > 6) return undefined
  if (typeof error === 'string') return error.startsWith('0x') ? error : undefined
  if (typeof error !== 'object') return undefined

  const record = error as Record<string, unknown>
  for (const key of ['data', 'raw']) {
    const value = record[key]
    if (typeof value === 'string' && value.startsWith('0x')) return value
    if (value && typeof value === 'object') {
      const nested = extractRevertData(value, depth + 1)
      if (nested) return nested
    }
  }

  for (const value of Object.values(record)) {
    const nested = extractRevertData(value, depth + 1)
    if (nested) return nested
  }
  return undefined
}

function decodePerpsError(error: unknown): { name?: string; args?: readonly unknown[] } {
  const data = extractRevertData(error)
  if (data) {
    try {
      const decoded = decodeErrorResult({ abi: PERPS_ERROR_ABI, data: data as `0x${string}` })
      return { name: decoded.errorName, args: decoded.args }
    } catch {
      // Fall back to viem's decoded metadata below.
    }
  }

  return {
    name: getNestedString(error, ['errorName']),
    args: getNestedArgs(error),
  }
}

function argNumber(args: readonly unknown[] | undefined, index = 0): number | undefined {
  const value = args?.[index]
  if (typeof value === 'number') return value
  if (typeof value === 'bigint') return Number(value)
  if (typeof value === 'string') return Number(value)
  return undefined
}

function argString(args: readonly unknown[] | undefined, index = 0): string | undefined {
  const value = args?.[index]
  if (typeof value === 'string') return value
  if (typeof value === 'bigint' || typeof value === 'number') return String(value)
  return undefined
}

function formatUnixTime(seconds: number | undefined): string | undefined {
  if (seconds === undefined || !Number.isFinite(seconds) || seconds <= 0) return undefined
  return new Date(seconds * 1000).toLocaleString(undefined, {
    month: 'short',
    day: '2-digit',
    hour: '2-digit',
    minute: '2-digit',
    second: '2-digit',
  })
}

function formatStalePriceMessage(args: readonly unknown[] | undefined): string {
  const feedId = argString(args, 1)
  const publishTime = argNumber(args, 2)
  const maxStaleness = argNumber(args, 3)
  const currentTimestamp = argNumber(args, 4)
  const publishLabel = formatUnixTime(publishTime)
  const currentLabel = formatUnixTime(currentTimestamp)
  const ageSeconds = publishTime === undefined || currentTimestamp === undefined
    ? undefined
    : currentTimestamp - publishTime

  if (
    publishLabel === undefined ||
    currentLabel === undefined ||
    ageSeconds === undefined ||
    maxStaleness === undefined
  ) {
    return 'Pyth price data expired before the transaction landed. Retry self-execute and confirm promptly.'
  }

  const zeroFeedId = feedId === '0x0000000000000000000000000000000000000000000000000000000000000000'
  if (zeroFeedId && ageSeconds <= maxStaleness) {
    return `Historical Pyth update was rejected for this order's reveal window. The oracle could not parse a unique historical tick after commit, even though the data was not expired. Router check time: ${currentLabel} (${currentTimestamp}); oracle max publish bound: ${publishLabel} (${publishTime}); decoded bound age: ${ageSeconds}s; staleness limit: ${maxStaleness}s. The app will retry with exact historical Hermes data when possible; if this repeats, wait for the order to expire, clean it up, and create a fresh order.`
  }

  if (ageSeconds <= maxStaleness) {
    return `Oracle returned a stale-price error, but the decoded timestamps are inconsistent. Decoded publish time: ${publishLabel} (${publishTime}); decoded chain check time: ${currentLabel} (${currentTimestamp}); decoded age: ${ageSeconds}s; limit: ${maxStaleness}s. Retry self-execute; if this repeats, send this line to the team.`
  }

  return `Pyth price data expired before the transaction landed. Price publish time: ${publishLabel}; chain check time: ${currentLabel}; age: ${ageSeconds}s; limit: ${maxStaleness}s. Retry self-execute and confirm promptly.`
}

function messageForDecodedError(name: string | undefined, args: readonly unknown[] | undefined): string | undefined {
  switch (name) {
    case 'EnforcedPause':
      return 'The router is paused. Try again after the protocol is unpaused.'
    case 'MarginClearinghouse__ZeroAmount':
      return 'Amount must be greater than zero.'
    case 'MarginClearinghouse__InsufficientBalance':
      return 'Your margin account balance is too low for this withdrawal.'
    case 'MarginClearinghouse__InsufficientFreeEquity':
    case 'OrderRouter__InsufficientFreeEquity':
      return 'Not enough free margin. Remember that committed orders also reserve keeper bounty.'
    case 'MarginClearinghouse__InsufficientUsdcForSettlement':
      return 'The margin account does not have enough USDC settlement balance.'
    case 'OrderRouter__ZeroSize':
      return 'Order size must be greater than zero.'
    case 'OrderRouter__CommitValidation': {
      const code = argNumber(args)
      return COMMIT_VALIDATION_MESSAGES[code ?? -1] ?? `Order commit failed validation${code === undefined ? '' : ` (${code})`}.`
    }
    case 'OrderRouter__PredictableOpenInvalid': {
      const code = argNumber(args)
      return OPEN_REVERT_MESSAGES[code ?? -1] ?? `This open order is invalid right now${code === undefined ? '' : ` (${code})`}.`
    }
    case 'CfdEngine__TypedOrderFailure': {
      const code = argNumber(args, 1)
      const isClose = args?.[2] === true
      const message = isClose ? CLOSE_REVERT_MESSAGES[code ?? -1] : OPEN_REVERT_MESSAGES[code ?? -1]
      return message ?? `The engine rejected this ${isClose ? 'close' : 'open'} order${code === undefined ? '' : ` (${code})`}.`
    }
    case 'CfdEngine__NotAccountOwner':
      return 'This wallet can only add margin to its own position.'
    case 'CfdEngine__MustCloseOpposingPosition':
      return OPEN_REVERT_MESSAGES[1]
    case 'CfdEngine__DegradedMode':
      return OPEN_REVERT_MESSAGES[2]
    case 'CfdEngine__PositionTooSmall':
      return OPEN_REVERT_MESSAGES[3]
    case 'CfdEngine__SkewTooHigh':
      return OPEN_REVERT_MESSAGES[4]
    case 'CfdEngine__MarginDrainedByFees':
      return OPEN_REVERT_MESSAGES[5]
    case 'CfdEngine__InsufficientInitialMargin':
      return OPEN_REVERT_MESSAGES[6]
    case 'CfdEngine__PoolSolvencyExceeded':
    case 'CfdEngine__InsufficientPoolLiquidity':
      return OPEN_REVERT_MESSAGES[7]
    case 'CfdEngine__CloseSizeExceedsPosition':
      return CLOSE_REVERT_MESSAGES[1]
    case 'CfdEngine__DustPosition':
      return CLOSE_REVERT_MESSAGES[2]
    case 'CfdEngine__PartialCloseUnderwaterCarry':
      return CLOSE_REVERT_MESSAGES[3]
    case 'CfdEngine__NoOpenPosition':
      return 'There is no open position for this account.'
    case 'CfdEngine__WithdrawBlockedByOpenPosition':
      return 'Withdrawal is blocked while this account has an open position.'
    case 'CfdEngine__MarkPriceStale':
      return 'The engine mark price is stale. Refresh the oracle mark before committing.'
    case 'CfdEngine__MarkPriceOutOfOrder':
      return 'The engine rejected an out-of-order mark price. Refresh and retry.'
    case 'CfdEngine__InsufficientCloseOrderBountyBacking':
      return 'There is not enough margin backing to reserve the close-order execution bounty.'
    case 'OrderRouter__CloseWithPositiveMargin':
      return 'Close/reduce orders cannot add margin.'
    case 'OrderRouter__NoQueuedPosition':
      return 'There is no queued or live position to reduce.'
    case 'OrderRouter__SideMismatch':
      return 'Reduce-only side does not match your current position.'
    case 'OrderRouter__SizeExceedsQueued':
      return 'Reduce size is larger than the current queued position.'
    case 'OrderRouter__TooManyPendingOrders':
      return 'You already have too many pending orders. Wait for one to execute or expire.'
    case 'OrderRouter__DegradedMode':
      return 'The market is degraded. New positions cannot be opened right now.'
    case 'OrderRouter__CloseOnlyWindow':
      return 'The market is close-only right now. You can reduce or close positions, but not open new risk.'
    case 'OrderRouter__NotInSeedLifecycle':
      return 'The LP pool is still in seed lifecycle. Trading is not open yet.'
    case 'OrderRouter__VaultRiskBlocked':
      return 'The LP vault is blocking additional risk right now.'
    case 'OrderRouter__EmptyPythUpdateData':
    case 'PletherOracle__MissingUpdateData':
      return 'Missing Pyth price update data. Retry execution.'
    case 'OrderRouter__InsufficientPythFee':
    case 'PletherOracle__InsufficientFee':
      return 'The Pyth update fee changed. Retry execution to quote a fresh fee.'
    case 'OrderRouter__OraclePriceTooStale':
      return 'Pyth price data expired before the transaction landed. Retry self-execute and confirm promptly.'
    case 'PletherOracle__StalePrice':
      return formatStalePriceMessage(args)
    case 'OrderRouter__OracleConfidenceTooWide':
    case 'PletherOracle__ConfidenceTooWide':
      return 'Pyth confidence is too wide right now. Retry when the oracle update is cleaner.'
    case 'OrderRouter__OraclePublishTimesDiverged':
    case 'PletherOracle__PublishTimeDivergence':
      return 'Pyth component prices are not aligned closely enough. Retry in a few seconds.'
    case 'OrderRouter__InvalidOraclePrice':
    case 'PletherOracle__InvalidPrice':
    case 'PletherOracle__ZeroBasketPrice':
      return 'The oracle returned an invalid basket price.'
    case 'OrderRouter__MarkPriceOutOfOrder':
    case 'PletherOracle__PriceOutOfOrder':
      return 'The oracle update is older than the stored mark. Refresh the mark or retry with newer Pyth data.'
    case 'OrderRouter__MevDetected':
      return 'Reveal is not ready yet. Wait a few seconds and retry self-execute.'
    case 'OrderRouter__NoOrdersToExecute':
      return 'This order is no longer pending. It likely expired or was already processed; your reserved margin has been released.'
    case 'OrderRouter__OrderNotQueueHead':
      return 'This order is blocked by an earlier pending order. Execute the earlier order first.'
    case 'OrderRouter__BatchBeforeQueueHead':
      return 'Batch execution target is before the queue head.'
    case 'OrderRouter__BatchOrderNotCommitted':
      return 'This order has not been committed yet.'
    case 'OrderRouter__OrderNotPending':
      return 'This order is no longer pending. Refresh account state.'
    case 'OrderRouter__InsufficientGas':
      return 'Execution needs more gas than the wallet supplied. Retry with a higher gas limit.'
    case 'OrderRouter__MarginQueueCorrupt':
    case 'OrderRouter__AccountQueueCorrupt':
    case 'OrderRouter__GlobalQueueCorrupt':
      return 'The order queue is inconsistent. Refresh and contact the team if this persists.'
    case 'OrderRouter__Unauthorized':
      return 'This wallet is not authorized for that router action.'
    default:
      return undefined
  }
}

function fallbackMessage(action: PerpsAction): string {
  switch (action) {
    case 'approve':
      return 'USDC approval failed. Check the wallet message and retry.'
    case 'deposit':
      return 'Deposit failed. Check USDC balance, allowance, and wallet gas.'
    case 'withdraw':
      return 'Withdraw failed. Check free margin and pending orders.'
    case 'addPositionMargin':
      return 'Add position margin failed. Check free margin, open position state, and wallet gas.'
    case 'commit':
      return 'Commit reverted before creating an order, but the RPC did not return a contract error. Refresh account state and check pending orders, free margin, market state, and slippage.'
    case 'execute':
      return 'Self-execute failed. Retry with fresh Pyth data; the previous update may have expired.'
  }
}

function messageForRawError(lowerMessage: string): string | undefined {
  if (lowerMessage.includes('orderrouter__toomanypendingorders') || lowerMessage.includes('orderrouter__too_many_pending_orders')) {
    return 'You already have too many pending orders. Execute or let existing orders expire/clean up before committing a new order.'
  }
  if (lowerMessage.includes('orderrouter__insufficientfreeequity')) {
    return 'Not enough free margin. Remember that committed orders also reserve keeper bounty.'
  }
  if (lowerMessage.includes('orderrouter__closeonlywindow')) {
    return 'The market is close-only right now. You can reduce or close positions, but not open new risk.'
  }
  if (lowerMessage.includes('orderrouter__degradedmode')) {
    return 'The market is degraded. New positions cannot be opened right now.'
  }
  if (lowerMessage.includes('orderrouter__predictableopeninvalid')) {
    return 'The open order is predictably invalid right now. Lower leverage, reduce size, or try the opposite side if skew is saturated.'
  }
  if (lowerMessage.includes('cfdengine__insufficientinitialmargin')) {
    return OPEN_REVERT_MESSAGES[6]
  }
  if (lowerMessage.includes('cfdengine__skewtoohigh')) {
    return OPEN_REVERT_MESSAGES[4]
  }
  if (lowerMessage.includes('cfdengine__poolsolvencyexceeded') || lowerMessage.includes('cfdengine__insufficientpoolliquidity')) {
    return OPEN_REVERT_MESSAGES[7]
  }
  if (lowerMessage.includes('cfdengine__mustcloseopposingposition')) {
    return OPEN_REVERT_MESSAGES[1]
  }
  if (lowerMessage.includes('cfdengine__positiontoosmall')) {
    return OPEN_REVERT_MESSAGES[3]
  }
  return undefined
}

export function getPerpsOrderFailureMessage(reason: number | undefined): string {
  if (reason === undefined) return 'Order failed during reveal.'
  return PERPS_ORDER_FAILURE_MESSAGES[reason] ?? `Order failed during reveal. Reason code: ${reason}.`
}

export function getPerpsOpenRevertMessage(code: number | undefined): string {
  return OPEN_REVERT_MESSAGES[code ?? -1] ?? `This open order is invalid right now${code === undefined ? '' : ` (${code})`}.`
}

export function getPerpsCloseInvalidReasonMessage(reason: number | undefined): string {
  return CLOSE_INVALID_REASON_MESSAGES[reason ?? -1] ?? `This reduce/close order is invalid right now${reason === undefined ? '' : ` (${reason})`}.`
}

export function getPerpsErrorMessage(error: unknown, action: PerpsAction): string {
  const decoded = decodePerpsError(error)
  const decodedMessage = messageForDecodedError(decoded.name, decoded.args)
  if (decodedMessage) return decodedMessage

  const rawMessage = getNestedString(error, ['shortMessage', 'message']) ?? (typeof error === 'string' ? error : '')
  const lower = rawMessage.toLowerCase()
  if (lower.includes('commit reverted after wallet confirmation')) {
    return rawMessage
  }
  const rawDecodedMessage = messageForRawError(lower)
  if (rawDecodedMessage) return rawDecodedMessage

  if (lower.includes('user rejected') || lower.includes('user denied') || lower.includes('rejected the request')) {
    return 'Transaction rejected in wallet.'
  }
  if (lower.includes('max fee per gas less than block base fee')) {
    return 'Gas price was below the current base fee. Retry; the app will request a higher gas fee.'
  }
  if (lower.includes('insufficient funds')) {
    return 'Not enough ETH for gas on Arbitrum Sepolia.'
  }
  if (
    lower.includes('could not fetch pyth update data from the backend') ||
    lower.includes('pyth update request failed') ||
    lower.includes('backend did not return pyth update data') ||
    lower.includes('hermes rate limit reached')
  ) {
    return rawMessage
  }
  if (lower.includes('network') || lower.includes('fetch')) {
    return rawMessage
      ? `Network request failed: ${rawMessage}`
      : 'Network request failed. Check RPC/Hermes connectivity and retry.'
  }
  if (lower.includes('transaction receipt') && lower.includes('could not be found')) {
    return 'Transaction was submitted, but confirmation timed out. Check the explorer before retrying.'
  }
  if (lower.includes('reverted') && action === 'execute') {
    return 'Self-execute reverted after wallet confirmation. Retry with fresh Pyth data; the previous update may have expired.'
  }
  if (lower.includes('reverted')) {
    return fallbackMessage(action)
  }

  return rawMessage && rawMessage !== 'Transaction failed' ? rawMessage : fallbackMessage(action)
}
