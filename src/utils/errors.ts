export function parseTransactionError(error: unknown): string {
  if (!error) return 'Transaction failed'

  const errorObj = error as { shortMessage?: string; message?: string; cause?: { shortMessage?: string; message?: string } }

  // Wagmi/viem errors often have shortMessage
  if (errorObj.shortMessage) {
    return cleanErrorMessage(errorObj.shortMessage)
  }

  // Check cause for nested errors
  if (errorObj.cause?.shortMessage) {
    return cleanErrorMessage(errorObj.cause.shortMessage)
  }

  if (errorObj.message) {
    return cleanErrorMessage(errorObj.message)
  }

  if (typeof error === 'string') {
    return cleanErrorMessage(error)
  }

  return 'Transaction failed'
}

function cleanErrorMessage(message: string): string {
  // User rejected
  if (message.includes('User rejected') || message.includes('user rejected')) {
    return 'Transaction rejected by user'
  }

  // Insufficient funds
  if (message.includes('insufficient funds')) {
    return 'Insufficient funds for gas'
  }

  // Insufficient allowance
  if (message.includes('insufficient allowance') || message.includes('ERC20: insufficient allowance')) {
    return 'Insufficient token allowance'
  }

  // Insufficient balance
  if (message.includes('transfer amount exceeds balance') || message.includes('ERC20: transfer amount exceeds balance')) {
    return 'Insufficient token balance'
  }

  // Execution reverted with reason
  const revertMatch = /execution reverted: (.+?)(?:\n|$|")/i.exec(message)
  if (revertMatch) {
    return revertMatch[1].trim()
  }

  // Generic revert
  if (message.includes('execution reverted')) {
    return 'Transaction reverted'
  }

  // Network errors
  if (message.includes('network') || message.includes('disconnected')) {
    return 'Network error - please try again'
  }

  // Timeout
  if (message.includes('timeout')) {
    return 'Transaction timed out'
  }

  // Truncate long messages
  if (message.length > 100) {
    return message.slice(0, 100) + '...'
  }

  return message
}
