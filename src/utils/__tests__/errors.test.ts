import { describe, it, expect } from 'vitest'
import {
  parseTransactionError,
  getErrorMessage,
  UserRejectedError,
  InsufficientFundsError,
  ContractRevertError,
  NetworkError,
  TimeoutError,
  UnknownTransactionError,
} from '../errors'

describe('parseTransactionError', () => {
  describe('UserRejectedError', () => {
    it('parses "User rejected" message', () => {
      const error = parseTransactionError({ message: 'User rejected the request' })
      expect(error._tag).toBe('UserRejectedError')
      expect(error.message).toBe('Transaction rejected by user')
    })

    it('parses "user rejected" (lowercase)', () => {
      const error = parseTransactionError({ message: 'user rejected transaction' })
      expect(error._tag).toBe('UserRejectedError')
    })

    it('parses shortMessage with User rejected', () => {
      const error = parseTransactionError({ shortMessage: 'User rejected' })
      expect(error._tag).toBe('UserRejectedError')
    })

    it('parses nested cause with User rejected', () => {
      const error = parseTransactionError({
        cause: { shortMessage: 'User rejected the request' },
      })
      expect(error._tag).toBe('UserRejectedError')
    })
  })

  describe('InsufficientFundsError', () => {
    it('parses "insufficient funds" as gas type', () => {
      const error = parseTransactionError({ message: 'insufficient funds for gas' })
      expect(error._tag).toBe('InsufficientFundsError')
      if (error._tag === 'InsufficientFundsError') {
        expect(error.type).toBe('gas')
        expect(error.message).toBe('Insufficient funds for gas')
      }
    })

    it('parses "insufficient allowance" as allowance type', () => {
      const error = parseTransactionError({ message: 'insufficient allowance' })
      expect(error._tag).toBe('InsufficientFundsError')
      if (error._tag === 'InsufficientFundsError') {
        expect(error.type).toBe('allowance')
        expect(error.message).toBe('Insufficient token allowance')
      }
    })

    it('parses "ERC20: insufficient allowance" as allowance type', () => {
      const error = parseTransactionError({ message: 'ERC20: insufficient allowance' })
      expect(error._tag).toBe('InsufficientFundsError')
      if (error._tag === 'InsufficientFundsError') {
        expect(error.type).toBe('allowance')
      }
    })

    it('parses "transfer amount exceeds balance" as token type', () => {
      const error = parseTransactionError({ message: 'transfer amount exceeds balance' })
      expect(error._tag).toBe('InsufficientFundsError')
      if (error._tag === 'InsufficientFundsError') {
        expect(error.type).toBe('token')
        expect(error.message).toBe('Insufficient token balance')
      }
    })

    it('parses "ERC20: transfer amount exceeds balance" as token type', () => {
      const error = parseTransactionError({ message: 'ERC20: transfer amount exceeds balance' })
      expect(error._tag).toBe('InsufficientFundsError')
      if (error._tag === 'InsufficientFundsError') {
        expect(error.type).toBe('token')
      }
    })
  })

  describe('ContractRevertError', () => {
    it('parses "execution reverted: <reason>"', () => {
      const error = parseTransactionError({
        message: 'execution reverted: Slippage exceeded\n',
      })
      expect(error._tag).toBe('ContractRevertError')
      if (error._tag === 'ContractRevertError') {
        expect(error.reason).toBe('Slippage exceeded')
      }
    })

    it('parses revert with reason ending in quote', () => {
      const error = parseTransactionError({
        message: 'execution reverted: Amount too low"',
      })
      expect(error._tag).toBe('ContractRevertError')
      if (error._tag === 'ContractRevertError') {
        expect(error.reason).toBe('Amount too low')
      }
    })

    it('parses generic "execution reverted" without reason', () => {
      const error = parseTransactionError({ message: 'execution reverted' })
      expect(error._tag).toBe('ContractRevertError')
      if (error._tag === 'ContractRevertError') {
        expect(error.reason).toBe('Transaction reverted')
      }
    })

    it('constructs with no reason provided', () => {
      const error = new ContractRevertError({})
      expect(error.reason).toBe('Unknown reason')
      expect(error.message).toBe('Unknown reason')
    })
  })

  describe('NetworkError', () => {
    it('parses "network" error message', () => {
      const error = parseTransactionError({ message: 'network request failed' })
      expect(error._tag).toBe('NetworkError')
      expect(error.message).toBe('Network error - please try again')
    })

    it('parses "disconnected" error message', () => {
      const error = parseTransactionError({ message: 'disconnected from chain' })
      expect(error._tag).toBe('NetworkError')
    })

    it('preserves cause in NetworkError', () => {
      const originalError = { message: 'network error' }
      const error = parseTransactionError(originalError)
      expect(error._tag).toBe('NetworkError')
      if (error._tag === 'NetworkError') {
        expect(error.cause).toBe(originalError)
      }
    })
  })

  describe('TimeoutError', () => {
    it('parses "timeout" error message', () => {
      const error = parseTransactionError({ message: 'Request timeout' })
      expect(error._tag).toBe('TimeoutError')
      expect(error.message).toBe('Transaction timed out')
    })
  })

  describe('UnknownTransactionError', () => {
    it('returns UnknownTransactionError for null/undefined', () => {
      const error1 = parseTransactionError(null)
      expect(error1._tag).toBe('UnknownTransactionError')

      const error2 = parseTransactionError(undefined)
      expect(error2._tag).toBe('UnknownTransactionError')
    })

    it('returns UnknownTransactionError for unrecognized error', () => {
      const error = parseTransactionError({ message: 'Something weird happened' })
      expect(error._tag).toBe('UnknownTransactionError')
    })

    it('extracts shortMessage for unknown error', () => {
      const error = parseTransactionError({ shortMessage: 'Brief error' })
      expect(error._tag).toBe('UnknownTransactionError')
      expect(error.message).toBe('Brief error')
    })

    it('extracts nested cause.shortMessage for unknown error', () => {
      const error = parseTransactionError({
        cause: { shortMessage: 'Nested brief error' },
      })
      expect(error._tag).toBe('UnknownTransactionError')
      expect(error.message).toBe('Nested brief error')
    })

    it('extracts message for unknown error', () => {
      const error = parseTransactionError({ message: 'Full error message' })
      expect(error._tag).toBe('UnknownTransactionError')
      expect(error.message).toBe('Full error message')
    })

    it('handles string errors', () => {
      const error = parseTransactionError('Simple string error')
      expect(error._tag).toBe('UnknownTransactionError')
      expect(error.message).toBe('Simple string error')
    })

    it('truncates long messages to 100 characters', () => {
      const longMessage = 'A'.repeat(150)
      const error = parseTransactionError({ message: longMessage })
      expect(error.message.length).toBe(103)
      expect(error.message).toBe('A'.repeat(100) + '...')
    })

    it('returns default message for empty error object', () => {
      const error = parseTransactionError({})
      expect(error._tag).toBe('UnknownTransactionError')
      expect(error.message).toBe('Transaction failed')
    })

    it('preserves cause in UnknownTransactionError', () => {
      const originalError = { message: 'Original error' }
      const error = parseTransactionError(originalError)
      expect(error._tag).toBe('UnknownTransactionError')
      if (error._tag === 'UnknownTransactionError') {
        expect(error.cause).toBe(originalError)
      }
    })
  })
})

describe('getErrorMessage', () => {
  it('returns message for UserRejectedError', () => {
    const error = new UserRejectedError()
    expect(getErrorMessage(error)).toBe('Transaction rejected by user')
  })

  it('returns message for InsufficientFundsError (gas)', () => {
    const error = new InsufficientFundsError({ type: 'gas' })
    expect(getErrorMessage(error)).toBe('Insufficient funds for gas')
  })

  it('returns message for InsufficientFundsError (token)', () => {
    const error = new InsufficientFundsError({ type: 'token' })
    expect(getErrorMessage(error)).toBe('Insufficient token balance')
  })

  it('returns message for InsufficientFundsError (allowance)', () => {
    const error = new InsufficientFundsError({ type: 'allowance' })
    expect(getErrorMessage(error)).toBe('Insufficient token allowance')
  })

  it('returns message for ContractRevertError', () => {
    const error = new ContractRevertError({ reason: 'Custom revert reason' })
    expect(getErrorMessage(error)).toBe('Custom revert reason')
  })

  it('returns message for NetworkError', () => {
    const error = new NetworkError()
    expect(getErrorMessage(error)).toBe('Network error - please try again')
  })

  it('returns message for TimeoutError', () => {
    const error = new TimeoutError()
    expect(getErrorMessage(error)).toBe('Transaction timed out')
  })

  it('returns message for UnknownTransactionError', () => {
    const error = new UnknownTransactionError({ cause: new Error('Test') })
    expect(getErrorMessage(error)).toBe('Test')
  })
})

describe('Error class constructors', () => {
  it('UserRejectedError constructs with correct tag and message', () => {
    const error = new UserRejectedError()
    expect(error._tag).toBe('UserRejectedError')
    expect(error.message).toBe('Transaction rejected by user')
  })

  it('InsufficientFundsError constructs with all types', () => {
    const gasError = new InsufficientFundsError({ type: 'gas' })
    expect(gasError.type).toBe('gas')
    expect(gasError._tag).toBe('InsufficientFundsError')

    const tokenError = new InsufficientFundsError({ type: 'token' })
    expect(tokenError.type).toBe('token')

    const allowanceError = new InsufficientFundsError({ type: 'allowance' })
    expect(allowanceError.type).toBe('allowance')
  })

  it('ContractRevertError constructs with reason', () => {
    const error = new ContractRevertError({ reason: 'Custom reason' })
    expect(error._tag).toBe('ContractRevertError')
    expect(error.reason).toBe('Custom reason')
    expect(error.message).toBe('Custom reason')
  })

  it('NetworkError constructs with optional cause', () => {
    const withoutCause = new NetworkError()
    expect(withoutCause._tag).toBe('NetworkError')
    expect(withoutCause.cause).toBeUndefined()

    const originalError = new Error('Connection failed')
    const withCause = new NetworkError({ cause: originalError })
    expect(withCause.cause).toBe(originalError)
  })

  it('TimeoutError constructs with correct message', () => {
    const error = new TimeoutError()
    expect(error._tag).toBe('TimeoutError')
    expect(error.message).toBe('Transaction timed out')
  })

  it('UnknownTransactionError extracts message from cause', () => {
    const error = new UnknownTransactionError({ cause: { message: 'Extracted message' } })
    expect(error._tag).toBe('UnknownTransactionError')
    expect(error.message).toBe('Extracted message')
  })
})
