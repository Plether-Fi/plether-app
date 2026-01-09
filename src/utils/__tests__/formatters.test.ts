import { describe, it, expect } from 'vitest'
import {
  formatAmount,
  formatUsd,
  formatPercent,
  formatAddress,
  parseAmount,
  getHealthFactorColor,
  formatDate,
} from '../formatters'

describe('formatAmount', () => {
  it('formats zero as "0"', () => {
    expect(formatAmount(0n, 18)).toBe('0')
  })

  it('formats small amounts with < 0.0001 notation', () => {
    expect(formatAmount(1n, 18)).toBe('< 0.0001')
    expect(formatAmount(99999999999999n, 18)).toBe('< 0.0001')
  })

  it('formats amounts with correct decimals', () => {
    expect(formatAmount(1000000000000000000n, 18)).toBe('1')
    expect(formatAmount(1500000000000000000n, 18)).toBe('1.5')
    expect(formatAmount(1234500000000000000n, 18)).toBe('1.2345')
  })

  it('respects maxDecimals parameter', () => {
    expect(formatAmount(1234567890000000000n, 18, 2)).toBe('1.23')
    expect(formatAmount(1234567890000000000n, 18, 6)).toBe('1.234568')
  })

  it('handles USDC amounts with 6 decimals', () => {
    expect(formatAmount(1000000n, 6)).toBe('1')
    expect(formatAmount(1500000n, 6)).toBe('1.5')
    expect(formatAmount(100000000n, 6)).toBe('100')
  })

  it('formats large amounts with locale separators', () => {
    expect(formatAmount(1000000000000000000000n, 18)).toBe('1,000')
    expect(formatAmount(1000000000000000000000000n, 18)).toBe('1,000,000')
  })
})

describe('formatUsd', () => {
  it('formats USDC amounts as USD', () => {
    expect(formatUsd(1000000n, 6)).toBe('$1.00')
    expect(formatUsd(1500000n, 6)).toBe('$1.50')
    expect(formatUsd(100000000n, 6)).toBe('$100.00')
  })

  it('formats zero as $0.00', () => {
    expect(formatUsd(0n, 6)).toBe('$0.00')
  })

  it('formats large amounts with separators', () => {
    expect(formatUsd(1000000000000n, 6)).toBe('$1,000,000.00')
  })
})

describe('formatPercent', () => {
  it('formats percentages with default 2 decimals', () => {
    expect(formatPercent(12.345)).toBe('12.35%')
    expect(formatPercent(0)).toBe('0.00%')
    expect(formatPercent(100)).toBe('100.00%')
  })

  it('respects decimals parameter', () => {
    expect(formatPercent(12.3456, 1)).toBe('12.3%')
    expect(formatPercent(12.3456, 4)).toBe('12.3456%')
  })
})

describe('formatAddress', () => {
  it('truncates address to 0x1234...5678 format', () => {
    expect(formatAddress('0x1234567890abcdef1234567890abcdef12345678'))
      .toBe('0x1234...5678')
  })

  it('returns empty string for empty input', () => {
    expect(formatAddress('')).toBe('')
  })
})

describe('parseAmount', () => {
  it('parses string to bigint with 18 decimals', () => {
    expect(parseAmount('1', 18)).toBe(1000000000000000000n)
    expect(parseAmount('1.5', 18)).toBe(1500000000000000000n)
    expect(parseAmount('0.1', 18)).toBe(100000000000000000n)
  })

  it('parses string to bigint with 6 decimals (USDC)', () => {
    expect(parseAmount('1', 6)).toBe(1000000n)
    expect(parseAmount('100', 6)).toBe(100000000n)
    expect(parseAmount('0.000001', 6)).toBe(1n)
  })

  it('handles commas in input', () => {
    expect(parseAmount('1,000', 18)).toBe(1000000000000000000000n)
    expect(parseAmount('1,000,000', 6)).toBe(1000000000000n)
  })

  it('returns 0n for invalid input', () => {
    expect(parseAmount('', 18)).toBe(0n)
    expect(parseAmount('.', 18)).toBe(0n)
    expect(parseAmount('abc', 18)).toBe(0n)
  })
})

describe('getHealthFactorColor', () => {
  it('returns green for healthy positions (>= 1.5)', () => {
    expect(getHealthFactorColor(1.5)).toBe('text-green-500')
    expect(getHealthFactorColor(2.0)).toBe('text-green-500')
    expect(getHealthFactorColor(10)).toBe('text-green-500')
  })

  it('returns yellow for at-risk positions (1.2 - 1.5)', () => {
    expect(getHealthFactorColor(1.2)).toBe('text-yellow-500')
    expect(getHealthFactorColor(1.3)).toBe('text-yellow-500')
    expect(getHealthFactorColor(1.49)).toBe('text-yellow-500')
  })

  it('returns red for dangerous positions (< 1.2)', () => {
    expect(getHealthFactorColor(1.19)).toBe('text-red-500')
    expect(getHealthFactorColor(1.0)).toBe('text-red-500')
    expect(getHealthFactorColor(0.5)).toBe('text-red-500')
  })
})

describe('formatDate', () => {
  it('formats unix timestamp to readable date', () => {
    const timestamp = 1704067200
    const result = formatDate(timestamp)
    expect(result).toContain('Jan')
    expect(result).toContain('2024')
  })
})
