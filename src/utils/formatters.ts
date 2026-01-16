import { formatUnits, parseUnits } from 'viem'

/**
 * Format a bigint amount with decimals to a human-readable string
 */
export function formatAmount(
  amount: bigint,
  decimals: number,
  maxDecimals = 4
): string {
  const formatted = formatUnits(amount, decimals)
  const num = parseFloat(formatted)

  if (num === 0) return '0'
  if (num < 0.0001) return '< 0.0001'

  return num.toLocaleString('en-US', {
    minimumFractionDigits: 0,
    maximumFractionDigits: maxDecimals,
  })
}

/**
 * Format USD value
 */
export function formatUsd(amount: bigint, decimals = 6): string {
  const num = parseFloat(formatUnits(amount, decimals))

  return new Intl.NumberFormat('en-US', {
    style: 'currency',
    currency: 'USD',
    minimumFractionDigits: 2,
    maximumFractionDigits: 2,
  }).format(num)
}

/**
 * Format percentage
 */
export function formatPercent(value: number, decimals = 2): string {
  return `${value.toFixed(decimals)}%`
}

/**
 * Format address to shortened form (0x1234...5678)
 */
export function formatAddress(address: string): string {
  if (!address) return ''
  return `${address.slice(0, 6)}...${address.slice(-4)}`
}

/**
 * Parse a string input to bigint with decimals
 */
export function parseAmount(value: string, decimals: number): bigint {
  try {
    // Remove any commas
    const cleanValue = value.replace(/,/g, '')
    if (!cleanValue || cleanValue === '.') return 0n
    return parseUnits(cleanValue, decimals)
  } catch {
    return 0n
  }
}

/**
 * Format health factor with color indication
 */
export function getHealthFactorColor(healthFactor: number): string {
  if (healthFactor >= 1.5) return 'text-green-500'
  if (healthFactor >= 1.2) return 'text-yellow-500'
  return 'text-red-500'
}

/**
 * Format timestamp to readable date
 */
export function formatDate(timestamp: number): string {
  return new Date(timestamp * 1000).toLocaleDateString('en-US', {
    month: 'short',
    day: 'numeric',
    year: 'numeric',
    hour: '2-digit',
    minute: '2-digit',
  })
}
