import { TaggedError, matchErrorPartial } from 'better-result'

const ERROR_SELECTORS: Record<string, string> = {
  // LeverageRouter errors
  '0x80fd91ce': 'Swap output too low - try increasing slippage',
  '0x0fe775ae': 'Slippage exceeds maximum (1%)',
  '0x82793d5a': 'Curve price query failed',
  '0x02e83603': 'Splitter not active',
  '0x5ea9fe39': 'Transaction deadline expired',
  '0x50285b92': 'Leverage must be greater than 1x',
  '0x3431b67c': 'Principal amount is zero',
  '0x970faf44': 'Collateral amount is zero',
  '0xbcc16562': 'Invalid address (zero)',
  '0xf4d5ee4b': 'Not authorized - approve Morpho first',

  // ZapRouter errors
  '0x1e82378b': 'Swap output too low - try increasing slippage',
  '0xaaf6a5c7': 'Slippage exceeds maximum (1%)',
  '0x1b18b763': 'Curve price query failed',
  '0x7ec11d1d': 'Splitter not active',
  '0x97a66649': 'Transaction deadline expired',
  '0x238f1686': 'BEAR price above cap - market conditions unfavorable',
  '0xd7f35b86': 'Solvency check failed',
  '0xf471c493': 'Amount is zero',
  '0xe0479991': 'Invalid address (zero)',

  // Splitter errors
  '0x583fe571': 'Amount is zero',
  '0x1d2a620e': 'Burn would return zero USDC',
  '0x0d6ff717': 'Protocol is in liquidation mode',
  '0x2e686365': 'Insufficient liquidity in yield adapter',
  '0xe2cbebc9': 'Protocol is insolvent',

  // Oracle errors
  '0x08744fae': 'Price data is stale',
  '0xbb02674c': 'Price exceeds cap - liquidation triggered',
  '0xc4a1093a': 'Oracle price is stale',
  '0xd67540f3': 'Invalid oracle price',

  // ERC20 errors
  '0xe450d38c': 'Insufficient token balance',
  '0xfb8f41b2': 'Insufficient token allowance',

  // Common errors
  '0xd93c0665': 'Contract is paused',
  '0x3ee5aeb5': 'Reentrancy detected',
}

function decodeErrorSelector(data: string): string | null {
  if (!data || data.length < 10) return null
  const selector = data.slice(0, 10).toLowerCase()
  return ERROR_SELECTORS[selector] ?? null
}

export class UserRejectedError extends TaggedError('UserRejectedError')<{
  message: string
}>() {
  constructor() {
    super({ message: 'Transaction rejected by user' })
  }
}

export class InsufficientFundsError extends TaggedError('InsufficientFundsError')<{
  type: 'gas' | 'token' | 'allowance'
  message: string
}>() {
  constructor(args: { type: 'gas' | 'token' | 'allowance' }) {
    const messages = {
      gas: 'Insufficient funds for gas',
      token: 'Insufficient token balance',
      allowance: 'Insufficient token allowance',
    }
    super({ type: args.type, message: messages[args.type] })
  }
}

export class ContractRevertError extends TaggedError('ContractRevertError')<{
  reason: string
  message: string
}>() {
  constructor(args: { reason?: string }) {
    const reason = args.reason ?? 'Unknown reason'
    super({ reason, message: reason })
  }
}

export class NetworkError extends TaggedError('NetworkError')<{
  message: string
  cause?: unknown
}>() {
  constructor(args?: { cause?: unknown }) {
    super({ message: 'Network error - please try again', cause: args?.cause })
  }
}

export class TimeoutError extends TaggedError('TimeoutError')<{
  message: string
}>() {
  constructor() {
    super({ message: 'Transaction timed out' })
  }
}

export class UnknownTransactionError extends TaggedError('UnknownTransactionError')<{
  message: string
  cause: unknown
}>() {
  constructor(args: { cause: unknown }) {
    const msg = extractMessage(args.cause)
    super({ message: msg, cause: args.cause })
  }
}

export type TransactionError =
  | UserRejectedError
  | InsufficientFundsError
  | ContractRevertError
  | NetworkError
  | TimeoutError
  | UnknownTransactionError

function extractMessage(error: unknown): string {
  if (!error) return 'Transaction failed'

  const errorObj = error as {
    shortMessage?: string
    message?: string
    cause?: { shortMessage?: string; message?: string }
  }

  if (errorObj.shortMessage) return truncateMessage(errorObj.shortMessage)
  if (errorObj.cause?.shortMessage) return truncateMessage(errorObj.cause.shortMessage)
  if (errorObj.message) return truncateMessage(errorObj.message)
  if (typeof error === 'string') return truncateMessage(error)

  return 'Transaction failed'
}

function truncateMessage(message: string): string {
  return message.length > 100 ? message.slice(0, 100) + '...' : message
}

const USER_REJECTION_PATTERNS = [
  'user rejected',
  'user denied',
  'user cancelled',
  'user canceled',
  'rejected the request',
  'rejected by user',
  'request rejected',
  'transaction was rejected',
  'signature request was rejected',
  'action_rejected',
]

function isUserRejection(message: string): boolean {
  const lower = message.toLowerCase()
  return USER_REJECTION_PATTERNS.some((pattern) => lower.includes(pattern))
}

function extractErrorData(error: unknown): string | null {
  if (!error || typeof error !== 'object') return null

  const err = error as Record<string, unknown>

  // Try common viem error data locations
  const candidates = [
    err.data,
    (err.cause as Record<string, unknown>)?.data,
    (err.cause as Record<string, unknown>)?.cause,
    ((err.cause as Record<string, unknown>)?.cause as Record<string, unknown>)?.data,
    err.error,
    (err.error as Record<string, unknown>)?.data,
  ]

  for (const candidate of candidates) {
    if (typeof candidate === 'string' && candidate.startsWith('0x')) {
      return candidate
    }
  }

  // Check for nested error.data structure
  if (err.cause && typeof err.cause === 'object') {
    const cause = err.cause as Record<string, unknown>
    if (cause.error && typeof cause.error === 'object') {
      const innerError = cause.error as Record<string, unknown>
      if (typeof innerError.data === 'string') {
        return innerError.data
      }
    }
  }

  return null
}

export function parseTransactionError(error: unknown): TransactionError {
  if (!error) return new UnknownTransactionError({ cause: error })

  const errorObj = error as {
    shortMessage?: string
    message?: string
    cause?: { shortMessage?: string; message?: string }
  }

  const message =
    errorObj.shortMessage ??
    errorObj.cause?.shortMessage ??
    errorObj.message ??
    (typeof error === 'string' ? error : '')

  if (isUserRejection(message)) {
    return new UserRejectedError()
  }

  if (message.includes('insufficient funds')) {
    return new InsufficientFundsError({ type: 'gas' })
  }

  if (message.includes('insufficient allowance') || message.includes('ERC20: insufficient allowance')) {
    return new InsufficientFundsError({ type: 'allowance' })
  }

  if (
    message.includes('transfer amount exceeds balance') ||
    message.includes('ERC20: transfer amount exceeds balance')
  ) {
    return new InsufficientFundsError({ type: 'token' })
  }

  // Try to decode custom error selector from revert data
  const errorData = extractErrorData(error)
  if (errorData) {
    const decodedError = decodeErrorSelector(errorData)
    if (decodedError) {
      return new ContractRevertError({ reason: decodedError })
    }
  }

  const revertMatch = /execution reverted: (.+?)(?:\n|$|")/i.exec(message)
  if (revertMatch) {
    return new ContractRevertError({ reason: revertMatch[1].trim() })
  }

  if (message.includes('execution reverted')) {
    return new ContractRevertError({ reason: 'Transaction reverted' })
  }

  if (message.includes('network') || message.includes('disconnected')) {
    return new NetworkError({ cause: error })
  }

  if (message.includes('timeout')) {
    return new TimeoutError()
  }

  return new UnknownTransactionError({ cause: error })
}

export function getErrorMessage(error: TransactionError): string {
  return matchErrorPartial(
    error,
    {
      UserRejectedError: (e) => e.message,
      InsufficientFundsError: (e) => e.message,
      ContractRevertError: (e) => e.message,
      NetworkError: (e) => e.message,
      TimeoutError: (e) => e.message,
    },
    (e) => e.message
  )
}
