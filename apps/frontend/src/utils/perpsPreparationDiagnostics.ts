import { COMMIT_UNDECODED_FALLBACK_MESSAGE, getPerpsContractErrorCode } from './perpsErrors'

export type PreparationStage = 'preflight' | 'deployment_verification' | 'context_read'
  | 'max_size_quote' | 'order_assessment' | 'review_validation' | 'funding_check'
  | 'commit_simulation' | 'preparation_timeout' | 'review_freshness'

export type PreparationFunction = 'maxOrderAge' | 'currentExecutionConfigHash'
  | 'openOrderExecutionBountyBps' | 'minOpenOrderExecutionBountyUsdc'
  | 'maxOpenOrderExecutionBountyUsdc' | 'closeOrderExecutionBountyUsdc'
  | 'lastMarkPrice' | 'CAP_PRICE' | 'totalAssets' | 'getLatestPrice'
  | 'activePositionProtectionId' | 'getPendingOrders' | 'maxPendingOrders'
  | 'getFreeBuyingPowerUsdc' | 'positionProtectionTriggerBountyUsdc' | 'getPosition'
  | 'assessOrder' | 'quoteMaxOpen' | 'previewOpen' | 'previewClose' | 'previewSponsoredClose' | 'executeBatch'
  | 'commitOrder' | 'commitOpenOrderWithProtection'

export interface PreparationDebugContext {
  chainId: number
  address: string
  account: string
  blockNumber: bigint
  blockHash: string
  samplePrice: bigint
  assessmentPoint: 'current' | 'midpoint' | 'limit'
}
interface FailureLocation { stage: PreparationStage; contract_function?: PreparationFunction; debug?: PreparationDebugContext }

// Keep local metadata outside Error properties. Existing error subclasses and
// their funding/review data must survive unchanged for the UI and max-size search.
const locations = new WeakMap<object, FailureLocation>()

export function preparationFailure(error: unknown, stage: PreparationStage, contractFunction?: PreparationFunction, debug?: PreparationDebugContext): unknown {
  const failure = error !== null && typeof error === 'object'
    ? error : new Error(typeof error === 'string' ? error : 'Order preparation failed', { cause: error })
  if (!locations.has(failure)) locations.set(failure, { stage, contract_function: contractFunction, debug })
  return failure
}

export async function withPreparationStep<T>(stage: PreparationStage, contractFunction: PreparationFunction | undefined, run: () => Promise<T>, debug?: PreparationDebugContext): Promise<T> {
  try {
    return await run()
  } catch (error) {
    throw preparationFailure(error, stage, contractFunction, debug)
  }
}

/** Local debugger/support inspection only. Never pass this object to analytics. */
export function getPreparationDebugContext(error: unknown): { context: PreparationDebugContext; error: object } | undefined {
  const seen = new Set<object>()
  for (let current = error, depth = 0; current && typeof current === 'object' && depth < 8; depth++) {
    if (seen.has(current)) break
    seen.add(current)
    const debug = locations.get(current)?.debug
    if (debug) return { context: debug, error: current }
    current = (current as { cause?: unknown }).cause
  }
}

/** Classify locally, emitting only constants and ABI-allowlisted names. */
export function getPreparationFailureProperties(error: unknown): { error_code: string; stage: PreparationStage; contract_function?: PreparationFunction; assessment_point?: PreparationDebugContext['assessmentPoint'] } {
  let location: FailureLocation | undefined
  let code: string | undefined
  let fallbackCode: string | undefined
  let current = error
  const seen = new Set<unknown>()
  try {
    for (let depth = 0; current && depth < 8 && !seen.has(current); depth++) {
      seen.add(current)
      code ??= getPerpsContractErrorCode(current)
      if (typeof current !== 'object') break
      location ??= locations.get(current)
      const record = current as { name?: unknown; message?: unknown; cause?: unknown; reason?: unknown }
      if (record.name === 'PerpsOrderFundingShortfallError') fallbackCode = 'funding_shortfall'
      else if (record.name === 'PerpsOrderReviewError') fallbackCode = record.reason === 'leverage' ? 'review_leverage' : 'review_validation'
      else if (record.name === 'AbortError') fallbackCode ??= 'aborted'
      const message = typeof record.message === 'string' ? record.message.toLowerCase() : ''
      if (message === COMMIT_UNDECODED_FALLBACK_MESSAGE.toLowerCase() || message.includes('revert')) fallbackCode ??= 'undecoded_revert'
      else if (/timeout|timed out|took too long/.test(message)) fallbackCode ??= 'timeout'
      else if (/network|fetch|http request/.test(message)) fallbackCode ??= 'network_failure'
      current = record.cause
    }
  } catch {
    // Telemetry must not turn malformed third-party errors into a UI failure.
  }
  return { ...(location?.contract_function ? { contract_function: location.contract_function } : {}),
    ...(location?.debug ? { assessment_point: location.debug.assessmentPoint } : {}), stage: location?.stage ?? 'preflight',
    error_code: code ?? (location?.stage === 'preparation_timeout' ? 'timeout'
      : location?.stage === 'review_freshness' ? 'review_expired' : fallbackCode ?? 'unknown') }
}
