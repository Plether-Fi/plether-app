import { TaggedError, matchErrorPartial } from 'better-result'

const ERROR_SELECTORS: Record<string, string> = {
  // LeverageRouter errors
  '0x02e83603': 'Splitter not active',
  '0x0fe775ae': 'Slippage exceeds maximum (1%)',
  '0x3431b67c': 'Principal amount is zero',
  '0x50285b92': 'Leverage must be greater than 1x',
  '0x5ea9fe39': 'Transaction deadline expired',
  '0x80fd91ce': 'Swap output too low - try increasing slippage',
  '0x82793d5a': 'Curve price query failed',
  '0x970faf44': 'Collateral amount is zero',
  '0xbcc16562': 'Invalid address (zero)',
  '0xf4d5ee4b': 'Not authorized - approve Morpho first',

  // ZapRouter errors
  '0x1b18b763': 'Curve price query failed',
  '0x1e82378b': 'Swap output too low - try increasing slippage',
  '0x238f1686': 'BEAR price above cap - market conditions unfavorable',
  '0x7ec11d1d': 'Splitter not active',
  '0x97a66649': 'Transaction deadline expired',
  '0xaaf6a5c7': 'Slippage exceeds maximum (1%)',
  '0xd7f35b86': 'Solvency check failed',
  '0xe0479991': 'Invalid address (zero)',
  '0xf471c493': 'Amount is zero',

  // Splitter errors
  '0x0d6ff717': 'Protocol is in liquidation mode',
  '0x1d2a620e': 'Burn would return zero USDC',
  '0x2e686365': 'Insufficient liquidity in yield adapter',
  '0x4a4a2ca4': 'Protocol not in liquidation',
  '0x4f2ab8e9': 'No yield surplus to harvest',
  '0x583fe571': 'Amount is zero',
  '0x71e7e49a': 'No pending proposal',
  '0x8b871441': 'Invalid CAP value',
  '0xa573750e': 'Yield adapter not configured',
  '0xad473ba8': 'Harvest below threshold',
  '0xafc4799e': 'Adapter withdrawal failed',
  '0xc17870e6': 'Timelock not expired',
  '0xc43f1901': 'Cannot rescue core assets',
  '0xcf144b7e': 'Invalid address (zero)',
  '0xe0146c26': 'Adapter migration lost funds',
  '0xe2cbebc9': 'Protocol is insolvent',
  '0xe901888d': 'Governance locked - 7 day cooldown after unpause',
  '0xfe41b1b8': 'Operation requires pause',

  // Oracle errors
  '0x08744fae': 'Price data is stale',
  '0x090bc858': 'Sequencer grace period active',
  '0x2bb5b1b3': 'Invalid component price feed',
  '0x4e630da3': 'No pending proposal',
  '0x593fc1f4': 'Max deviation is zero',
  '0x5f0ad3d9': 'Invalid address (zero)',
  '0x6eb3202d': 'Array lengths mismatch',
  '0x92d64d06': 'Invalid oracle price',
  '0x9e0bffb6': 'Timelock not expired',
  '0x9f4269de': 'Invalid underlying oracle price',
  '0xb1596697': 'Base price is zero',
  '0xbb02674c': 'Price exceeds cap - liquidation triggered',
  '0xc4a1093a': 'Oracle price is stale',
  '0xd536a664': 'Invalid address (zero)',
  '0xd67540f3': 'Invalid oracle price',
  '0xdfe9bde3': 'L2 sequencer is down',
  '0xe4b0ed2b': 'Curve pool already set',
  '0xfbe44393': 'Price deviation exceeds threshold',

  // MorphoAdapter errors
  '0x10a713ca': 'Invalid address (zero)',
  '0x55e52988': 'Caller is not Splitter',
  '0xac2d6075': 'Cannot rescue underlying asset',
  '0xb0e1b4f9': 'Invalid Morpho market',

  // FlashLoan errors
  '0x31473b82': 'Invalid flash loan operation',
  '0x4ea8ea0b': 'Flash loan not self-initiated',
  '0xeb9e1f78': 'Invalid flash loan lender',

  // ERC4626 errors
  '0x284ff667': 'Exceeds maximum mint amount',
  '0x79012fb2': 'Exceeds maximum deposit amount',
  '0xb94abeec': 'Exceeds maximum redeem amount - position may be locked or insufficient liquidity',
  '0xfe9cceec': 'Exceeds maximum withdraw amount',

  // ERC20 errors
  '0x94280d62': 'Invalid spender address',
  '0x96c6fd1e': 'Invalid sender address',
  '0xe450d38c': 'Insufficient token balance',
  '0xe602df05': 'Invalid approver address',
  '0xec442f05': 'Invalid receiver address',
  '0xfb8f41b2': 'Insufficient token allowance',

  // ERC2612/ERC3156 errors
  '0x4b800e46': 'Invalid permit signer',
  '0x62791302': 'Permit signature expired',
  '0x678c5b00': 'Invalid flash loan receiver',
  '0x752d88c0': 'Invalid account nonce',
  '0xb5a7db92': 'Unsupported token for flash loan',
  '0xfd9a7609': 'Exceeds maximum flash loan amount',

  // Signature errors
  '0xd78bce0c': 'Invalid signature S value',
  '0xf645eedf': 'Invalid ECDSA signature',
  '0xfce698f7': 'Invalid signature length',

  // Common errors
  '0x118cdaa7': 'Not authorized - caller is not owner',
  '0x1e4fbdf7': 'Invalid owner address',
  '0x305a27a9': 'String too long',
  '0x3ee5aeb5': 'Reentrancy detected',
  '0x3f84e836': 'Caller is not Splitter',
  '0x5274afe7': 'Token transfer failed',
  '0x7b63ec3e': 'Invalid address (zero)',
  '0x8dfc202b': 'Contract should be paused',
  '0xb3512b0c': 'Invalid short string',
  '0xd93c0665': 'Contract is paused',
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
  rawData?: string
}>() {
  constructor(args: { reason?: string; rawData?: string }) {
    const reason = args.reason ?? 'Unknown reason'
    super({ reason, message: reason, rawData: args.rawData })
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

  let msg = 'Transaction failed'
  if (errorObj.shortMessage) msg = errorObj.shortMessage
  else if (errorObj.cause?.shortMessage) msg = errorObj.cause.shortMessage
  else if (errorObj.message) msg = errorObj.message
  else if (typeof error === 'string') msg = error

  if (msg.length > 100) {
    return msg.slice(0, 100) + '...'
  }
  return msg
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
  const cause = err.cause as Record<string, unknown> | undefined
  const nestedCause = cause?.cause as Record<string, unknown> | undefined
  const errorObj = err.error as Record<string, unknown> | undefined
  const candidates = [
    err.data,
    cause?.data,
    cause?.cause,
    nestedCause?.data,
    err.error,
    errorObj?.data,
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
      return new ContractRevertError({ reason: decodedError, rawData: errorData })
    }
    // Unknown selector - show full error data
    const selector = errorData.slice(0, 10)
    return new ContractRevertError({
      reason: `Unknown error ${selector}`,
      rawData: errorData
    })
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
