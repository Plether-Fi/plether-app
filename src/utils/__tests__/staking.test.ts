import { describe, it, expect } from 'vitest'
import { parseUnits } from 'viem'
import {
  ASSET_DECIMALS,
  SHARE_DECIMALS,
  getStakingDecimals,
  parseStakingAmount,
  formatStakingBalance,
  isInsufficientStakingBalance,
} from '../staking'

describe('staking decimal constants', () => {
  it('asset decimals is 18', () => {
    expect(ASSET_DECIMALS).toBe(18)
  })

  it('share decimals is 21 (18 + 3 offset)', () => {
    expect(SHARE_DECIMALS).toBe(21)
  })

  it('offset is exactly 3 for 1000x inflation protection', () => {
    expect(SHARE_DECIMALS - ASSET_DECIMALS).toBe(3)
  })
})

describe('getStakingDecimals', () => {
  it('returns 18 for stake mode', () => {
    expect(getStakingDecimals('stake')).toBe(18)
  })

  it('returns 21 for unstake mode', () => {
    expect(getStakingDecimals('unstake')).toBe(21)
  })
})

describe('parseStakingAmount', () => {
  describe('stake mode (18 decimals)', () => {
    it('parses 100 tokens correctly', () => {
      expect(parseStakingAmount('100', 'stake')).toBe(100n * 10n ** 18n)
    })

    it('parses 1 token correctly', () => {
      expect(parseStakingAmount('1', 'stake')).toBe(1n * 10n ** 18n)
    })

    it('parses decimal amount correctly', () => {
      expect(parseStakingAmount('1.5', 'stake')).toBe(parseUnits('1.5', 18))
    })

    it('parses small decimal amount', () => {
      expect(parseStakingAmount('0.001', 'stake')).toBe(1n * 10n ** 15n)
    })

    it('returns 0n for empty input', () => {
      expect(parseStakingAmount('', 'stake')).toBe(0n)
    })

    it('returns 0n for invalid input', () => {
      expect(parseStakingAmount('abc', 'stake')).toBe(0n)
    })
  })

  describe('unstake mode (21 decimals)', () => {
    it('parses 100 shares correctly', () => {
      expect(parseStakingAmount('100', 'unstake')).toBe(100n * 10n ** 21n)
    })

    it('parses 1 share correctly', () => {
      expect(parseStakingAmount('1', 'unstake')).toBe(1n * 10n ** 21n)
    })

    it('parses decimal share amount correctly', () => {
      expect(parseStakingAmount('1.5', 'unstake')).toBe(parseUnits('1.5', 21))
    })

    it('parses small decimal share amount', () => {
      expect(parseStakingAmount('0.001', 'unstake')).toBe(1n * 10n ** 18n)
    })
  })

  describe('stake vs unstake difference', () => {
    it('same input produces 1000x different values', () => {
      const stakeAmount = parseStakingAmount('100', 'stake')
      const unstakeAmount = parseStakingAmount('100', 'unstake')

      expect(stakeAmount).toBe(100n * 10n ** 18n)
      expect(unstakeAmount).toBe(100n * 10n ** 21n)
      expect(unstakeAmount).toBe(stakeAmount * 1000n)
    })
  })
})

describe('formatStakingBalance', () => {
  it('formats staked shares (21 decimals) correctly', () => {
    const shares = 1000n * 10n ** 21n
    expect(formatStakingBalance(shares, 'unstake')).toBe('1000')
  })

  it('formats token balance (18 decimals) correctly', () => {
    const tokens = 1000n * 10n ** 18n
    expect(formatStakingBalance(tokens, 'stake')).toBe('1000')
  })

  it('formats fractional amounts correctly', () => {
    const shares = parseUnits('1.5', 21)
    expect(formatStakingBalance(shares, 'unstake')).toBe('1.5')
  })
})

describe('isInsufficientStakingBalance', () => {
  describe('stake mode', () => {
    it('returns false when amount is within token balance', () => {
      const tokenBalance = 100n * 10n ** 18n
      expect(isInsufficientStakingBalance('50', 'stake', tokenBalance, 0n)).toBe(false)
    })

    it('returns true when amount exceeds token balance', () => {
      const tokenBalance = 100n * 10n ** 18n
      expect(isInsufficientStakingBalance('150', 'stake', tokenBalance, 0n)).toBe(true)
    })

    it('returns false at exact balance', () => {
      const tokenBalance = 100n * 10n ** 18n
      expect(isInsufficientStakingBalance('100', 'stake', tokenBalance, 0n)).toBe(false)
    })

    it('returns false for empty input', () => {
      expect(isInsufficientStakingBalance('', 'stake', 100n * 10n ** 18n, 0n)).toBe(false)
    })
  })

  describe('unstake mode', () => {
    it('returns false when amount is within staked balance', () => {
      const stakedBalance = 100n * 10n ** 21n
      expect(isInsufficientStakingBalance('50', 'unstake', 0n, stakedBalance)).toBe(false)
    })

    it('returns true when amount exceeds staked balance', () => {
      const stakedBalance = 100n * 10n ** 21n
      expect(isInsufficientStakingBalance('150', 'unstake', 0n, stakedBalance)).toBe(true)
    })

    it('uses 21 decimals for comparison', () => {
      const stakedBalance = 100n * 10n ** 21n
      expect(isInsufficientStakingBalance('100', 'unstake', 0n, stakedBalance)).toBe(false)
      expect(isInsufficientStakingBalance('100.001', 'unstake', 0n, stakedBalance)).toBe(true)
    })
  })
})
