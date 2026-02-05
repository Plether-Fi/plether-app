import { describe, it, expect } from 'vitest'
import { getMinBalance } from '../mint'

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
