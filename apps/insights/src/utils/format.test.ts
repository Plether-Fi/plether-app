import { describe, expect, it } from 'vitest'
import { formatCompactUsdc, formatCountdown, formatRoi, formatSignedUsdc, formatUsdc, isWalletAddress, xProfileUrl } from './format'

describe('USDC formatting', () => {
  it('keeps six-decimal integer units lossless and rounds for display', () => {
    expect(formatUsdc('100000000000')).toBe('100,000.00 USDC')
    expect(formatUsdc('1000999')).toBe('1.00 USDC')
    expect(formatUsdc('-1000500')).toBe('-1.00 USDC')
  })

  it('adds a sign only for positive P&L', () => {
    expect(formatSignedUsdc('1000000000')).toBe('+1,000.00 USDC')
    expect(formatSignedUsdc('999999999')).toBe('+1,000.00 USDC')
    expect(formatSignedUsdc('313177984')).toBe('+313.18 USDC')
    expect(formatSignedUsdc('0')).toBe('0.00 USDC')
    expect(formatSignedUsdc('-1000000')).toBe('-1.00 USDC')
  })

  it('uses compact notation for table values', () => {
    expect(formatCompactUsdc('1250000000')).toBe('1.25K USDC')
    expect(formatCompactUsdc('1200000000000')).toBe('1.20M USDC')
    expect(formatCompactUsdc('999999999')).toBe('1,000.00 USDC')
  })
})

describe('other public value formatting', () => {
  it('formats countdowns and clamps completed countdowns at zero', () => {
    const target = '2026-09-13T21:00:00Z'
    expect(formatCountdown(target, Date.parse('2026-09-10T20:59:58Z'))).toBe('3d 00h 00m 02s')
    expect(formatCountdown(target, Date.parse('2026-09-13T21:00:01Z'))).toBe('0d 00h 00m 00s')
  })

  it('formats basis points as a signed percentage', () => {
    expect(formatRoi(100)).toBe('+1.00%')
    expect(formatRoi(-25)).toBe('-0.25%')
  })

  it('accepts only complete EVM addresses', () => {
    expect(isWalletAddress(`0x${'a'.repeat(40)}`)).toBe(true)
    expect(isWalletAddress(`0x${'a'.repeat(39)}`)).toBe(false)
  })

  it('builds X profile links only for valid public usernames', () => {
    expect(xProfileUrl('@plether')).toBe('https://x.com/plether')
    expect(xProfileUrl('Plether_Fi')).toBe('https://x.com/Plether_Fi')
    expect(xProfileUrl('not a username')).toBeNull()
  })
})
