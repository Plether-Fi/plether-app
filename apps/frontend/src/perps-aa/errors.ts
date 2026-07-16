export type StableSponsorReason =
  | 'RESTART_ESTIMATION'
  | 'RATE_LIMITED'
  | 'SPONSOR_BUDGET_EXCEEDED'
  | 'SIMULATION_FAILED'
  | 'SPONSOR_UNAVAILABLE'
  | 'POLICY_DENIED'
  | 'PAYMASTER_PAUSED'
  | 'ACCOUNT_NOT_TRUSTED'
  | 'UNKNOWN'
  | (string & {})

export type UserOperationTerminalStatus =
  | 'execution-reverted'
  | 'dropped'
  | 'replaced'
  | 'expired'
  | 'receipt-timeout'

export class SponsorRequestError extends Error {
  readonly reason: StableSponsorReason
  readonly retryable: boolean
  readonly callIndex?: number
  readonly rpcCode?: number
  override readonly cause: unknown

  constructor(input: {
    reason: StableSponsorReason
    message: string
    retryable: boolean
    callIndex?: number
    rpcCode?: number
    cause?: unknown
  }) {
    super(input.message)
    this.name = 'SponsorRequestError'
    this.reason = input.reason
    this.retryable = input.retryable
    this.callIndex = input.callIndex
    this.rpcCode = input.rpcCode
    this.cause = input.cause
  }
}

export class BundlerRequestError extends Error {
  readonly retryable: boolean
  readonly terminalStatus?: UserOperationTerminalStatus
  readonly replacementUserOperationHash?: string
  override readonly cause: unknown

  constructor(input: {
    message: string
    retryable: boolean
    terminalStatus?: UserOperationTerminalStatus
    replacementUserOperationHash?: string
    cause?: unknown
  }) {
    super(input.message)
    this.name = 'BundlerRequestError'
    this.retryable = input.retryable
    this.terminalStatus = input.terminalStatus
    this.replacementUserOperationHash = input.replacementUserOperationHash
    this.cause = input.cause
  }
}

function causeOf(value: unknown): unknown {
  if (!value || typeof value !== 'object') return undefined
  return (value as { cause?: unknown }).cause
}

function recordOf(value: unknown): Record<string, unknown> | undefined {
  return value && typeof value === 'object'
    ? value as Record<string, unknown>
    : undefined
}

function sponsorMetadata(error: unknown): {
  reason?: StableSponsorReason
  retryable?: boolean
  callIndex?: number
  rpcCode?: number
} {
  let current = error
  const seen = new Set<object>()
  let reason: StableSponsorReason | undefined
  let retryable: boolean | undefined
  let callIndex: number | undefined
  let rpcCode: number | undefined

  for (let depth = 0; depth < 8 && current !== undefined; depth += 1) {
    const record = recordOf(current)
    if (!record || seen.has(record)) break
    seen.add(record)
    const data = recordOf(record.data)
    const currentReason = data?.reason ?? record.reason
    const currentRetryable = data?.retryable ?? record.retryable
    const currentCallIndex = data?.callIndex ?? record.callIndex
    const currentRpcCode = record.code ?? record.rpcCode
    if (reason === undefined && typeof currentReason === 'string') {
      reason = currentReason as StableSponsorReason
    }
    if (retryable === undefined && typeof currentRetryable === 'boolean') {
      retryable = currentRetryable
    }
    if (callIndex === undefined && typeof currentCallIndex === 'number') {
      callIndex = currentCallIndex
    }
    if (rpcCode === undefined && typeof currentRpcCode === 'number') {
      rpcCode = currentRpcCode
    }
    current = record.cause
  }
  return {
    ...(reason !== undefined ? { reason } : {}),
    ...(retryable !== undefined ? { retryable } : {}),
    ...(callIndex !== undefined ? { callIndex } : {}),
    ...(rpcCode !== undefined ? { rpcCode } : {}),
  }
}

function walkCauses<T>(
  error: unknown,
  predicate: (value: unknown) => value is T
): T | undefined {
  let current = error
  const seen = new Set<object>()

  for (let depth = 0; depth < 8 && current !== undefined; depth += 1) {
    if (predicate(current)) return current
    if (!current || typeof current !== 'object' || seen.has(current)) return undefined
    seen.add(current)
    current = causeOf(current)
  }

  return undefined
}

export function asSponsorRequestError(error: unknown): SponsorRequestError {
  if (error instanceof SponsorRequestError) return error
  const metadata = sponsorMetadata(error)

  return new SponsorRequestError({
    reason: metadata.reason ?? 'UNKNOWN',
    message: error instanceof Error ? error.message : String(error),
    retryable: metadata.retryable ?? false,
    callIndex: metadata.callIndex,
    rpcCode: metadata.rpcCode,
    cause: error,
  })
}

export function findSponsorRequestError(error: unknown): SponsorRequestError | undefined {
  return walkCauses(
    error,
    (value): value is SponsorRequestError => value instanceof SponsorRequestError
  )
}

export function findBundlerRequestError(error: unknown): BundlerRequestError | undefined {
  return walkCauses(
    error,
    (value): value is BundlerRequestError => value instanceof BundlerRequestError
  )
}

export function sponsorReasonMessage(error: SponsorRequestError): string {
  switch (error.reason) {
    case 'RESTART_ESTIMATION':
      return 'The gas estimate changed. Plether is preparing a fresh sponsored transaction.'
    case 'RATE_LIMITED':
      return 'Too many sponsored requests are in progress. Wait a moment and retry.'
    case 'SPONSOR_BUDGET_EXCEEDED':
      return 'Sponsored gas capacity is temporarily unavailable. Retry later or contact support.'
    case 'SIMULATION_FAILED':
      return 'The sponsored transaction did not pass simulation. Refresh account state and retry.'
    case 'PAYMASTER_PAUSED':
      return 'Plether gas sponsorship is temporarily paused.'
    case 'POLICY_DENIED':
      return 'This action is not eligible for sponsored network gas.'
    case 'ACCOUNT_NOT_TRUSTED':
      return 'The configured Trading Account could not be verified.'
    case 'SPONSOR_UNAVAILABLE':
      return 'Plether gas sponsorship is temporarily unavailable. Your action was not sent.'
    default:
      return 'Plether could not sponsor this transaction. Your action was not sent.'
  }
}
