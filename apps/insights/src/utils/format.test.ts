import { describe, expect, it } from 'vitest'
import { formatCompactAssets, formatCountdown, formatRoi, formatSignedAssets, formatAssets, isWalletAddress, xProfileUrl } from './format'

describe('assets formatting', () => {
  it('keeps six-decimal integer units lossless and rounds for display', () => {
    expect(formatAssets('100000000000')).toBe('100,000.00 assets')
    expect(formatAssets('1000999')).toBe('1.00 assets')
    expect(formatAssets('-1000500')).toBe('-1.00 assets')
  })

  it('adds a sign only for positive P&L', () => {
    expect(formatSignedAssets('1000000000')).toBe('+1,000.00 assets')
    expect(formatSignedAssets('999999999')).toBe('+1,000.00 assets')
    expect(formatSignedAssets('313177984')).toBe('+313.18 assets')
    expect(formatSignedAssets('0')).toBe('0.00 assets')
    expect(formatSignedAssets('-1000000')).toBe('-1.00 assets')
  })

  it('uses compact notation for table values', () => {
    expect(formatCompactAssets('1250000000')).toBe('1.25K assets')
    expect(formatCompactAssets('1200000000000')).toBe('1.20M assets')
    expect(formatCompactAssets('999999999')).toBe('1,000.00 assets')
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
