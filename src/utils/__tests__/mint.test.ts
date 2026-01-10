import { describe, it, expect } from 'vitest'
import { parseUnits } from 'viem'
import {
  calculatePairAmount,
  calculateUsdcAmount,
  calculateOutputDisplay,
  getMinBalance,
} from '../mint'

describe('calculatePairAmount', () => {
  describe('mint mode - USDC to pair conversion', () => {
    it('converts 100 USDC to 50 pairs (18 decimals)', () => {
      expect(calculatePairAmount('100', 'mint')).toBe(50n * 10n ** 18n)
    })

    it('converts 2 USDC to 1 pair', () => {
      expect(calculatePairAmount('2', 'mint')).toBe(1n * 10n ** 18n)
    })

    it('converts 1 USDC to 0.5 pairs', () => {
      expect(calculatePairAmount('1', 'mint')).toBe(5n * 10n ** 17n)
    })

    it('handles decimal USDC input', () => {
      expect(calculatePairAmount('10.5', 'mint')).toBe(parseUnits('5.25', 18))
    })

    it('returns 0n for empty input', () => {
      expect(calculatePairAmount('', 'mint')).toBe(0n)
    })

    it('returns 0n for invalid input', () => {
      expect(calculatePairAmount('abc', 'mint')).toBe(0n)
    })
  })

  describe('redeem mode - pair amount (no conversion)', () => {
    it('uses input directly as pair amount (18 decimals)', () => {
      expect(calculatePairAmount('50', 'redeem')).toBe(50n * 10n ** 18n)
    })

    it('handles decimal pair input', () => {
      expect(calculatePairAmount('1.5', 'redeem')).toBe(parseUnits('1.5', 18))
    })
  })
})

describe('calculateUsdcAmount', () => {
  it('converts 100 USDC to correct bigint', () => {
    expect(calculateUsdcAmount('100')).toBe(100n * 10n ** 6n)
  })

  it('converts 0.01 USDC (1 cent) correctly', () => {
    expect(calculateUsdcAmount('0.01')).toBe(10000n)
  })

  it('handles large amounts', () => {
    expect(calculateUsdcAmount('1000000')).toBe(1000000n * 10n ** 6n)
  })

  it('returns 0n for empty input', () => {
    expect(calculateUsdcAmount('')).toBe(0n)
  })

  it('returns 0n for invalid input', () => {
    expect(calculateUsdcAmount('abc')).toBe(0n)
  })
})

describe('calculateOutputDisplay', () => {
  describe('mint mode - shows pairs received', () => {
    it('100 USDC shows 50.0000 pairs', () => {
      expect(calculateOutputDisplay('100', 'mint')).toBe('50.0000')
    })

    it('1 USDC shows 0.5000 pairs', () => {
      expect(calculateOutputDisplay('1', 'mint')).toBe('0.5000')
    })

    it('empty input shows 0.0000', () => {
      expect(calculateOutputDisplay('', 'mint')).toBe('0.0000')
    })
  })

  describe('redeem mode - shows USDC received', () => {
    it('50 pairs shows 100.00 USDC', () => {
      expect(calculateOutputDisplay('50', 'redeem')).toBe('100.00')
    })

    it('1 pair shows 2.00 USDC', () => {
      expect(calculateOutputDisplay('1', 'redeem')).toBe('2.00')
    })

    it('0.5 pairs shows 1.00 USDC', () => {
      expect(calculateOutputDisplay('0.5', 'redeem')).toBe('1.00')
    })
  })
})

describe('getMinBalance', () => {
  it('returns BEAR balance when BEAR < BULL', () => {
    const bearBalance = 100n * 10n ** 18n
    const bullBalance = 200n * 10n ** 18n
    expect(getMinBalance(bearBalance, bullBalance)).toBe(bearBalance)
  })

  it('returns BULL balance when BULL < BEAR', () => {
    const bearBalance = 200n * 10n ** 18n
    const bullBalance = 100n * 10n ** 18n
    expect(getMinBalance(bearBalance, bullBalance)).toBe(bullBalance)
  })

  it('returns either when equal', () => {
    const balance = 100n * 10n ** 18n
    expect(getMinBalance(balance, balance)).toBe(balance)
  })

  it('returns 0 when one balance is 0', () => {
    expect(getMinBalance(0n, 100n * 10n ** 18n)).toBe(0n)
    expect(getMinBalance(100n * 10n ** 18n, 0n)).toBe(0n)
  })
})

describe('CRITICAL: pair vs USDC amount difference', () => {
  it('pair amount and USDC amount are different for same input', () => {
    const input = '100'
    const pairAmount = calculatePairAmount(input, 'mint')
    const usdcAmount = calculateUsdcAmount(input)

    expect(pairAmount).toBe(50n * 10n ** 18n)
    expect(usdcAmount).toBe(100n * 10n ** 6n)
    expect(pairAmount).not.toBe(usdcAmount)
  })
})
