import { TaggedError, matchErrorPartial } from 'better-result'

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

  if (message.includes('User rejected') || message.includes('user rejected')) {
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
