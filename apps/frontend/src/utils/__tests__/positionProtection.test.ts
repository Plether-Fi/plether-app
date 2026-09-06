import { describe, expect, it } from 'vitest'
import { convertProtectionInputMode, protectionDistance, protectionPrice, protectionStatusLabel } from '../positionProtection'

const context = { direction: 'long' as const, rawMark: 100_000_000n, cap: 200_000_000n }
describe('TP/SL presentation', () => {
  it('preserves the full precision of a trigger price', () => {
    expect(protectionPrice(87_654_321n, context.cap)).toBe('1.12345679')
    expect(protectionPrice(90_000_000n, context.cap)).toBe('1.1000')
    expect(protectionPrice(0n, context.cap)).toBe('—')
    expect(protectionPrice(90_000_000n)).toBe('—')
  })
  it('shows signed distance from the displayed mark', () => {
    expect(protectionDistance(90_000_000n, context.rawMark, context.cap)).toBe('+10.00% from current price')
    expect(protectionDistance(105_000_000n, context.rawMark, context.cap)).toBe('−5.00% from current price')
  })
  it.each(['long', 'short'] as const)('preserves %s trigger values through input mode changes', direction => {
    const draft = { mode: 'price' as const, takeProfit: direction === 'long' ? '1.1' : '0.9', stopLoss: direction === 'long' ? '0.95' : '1.05' }
    const percent = convertProtectionInputMode(draft, 'percent', { ...context, direction })
    expect(percent).toEqual({ mode: 'percent', takeProfit: '10', stopLoss: '5' })
    expect(convertProtectionInputMode(percent, 'price', { ...context, direction })).toEqual(draft)
  })
  it('does not silently round prices when switching to percent', () => {
    expect(() => convertProtectionInputMode({ mode: 'price', takeProfit: '1.12345678', stopLoss: '' }, 'percent', context)).toThrow('precision')
  })
  it('allows changing modes while both fields are empty, even before the mark loads', () => {
    expect(convertProtectionInputMode({ mode: 'price', takeProfit: '', stopLoss: '' }, 'percent', { direction: 'long' })).toEqual({ mode: 'percent', takeProfit: '', stopLoss: '' })
  })
  it('uses plain-language status names', () => {
    expect(protectionStatusLabel(8)).toBe('Close delayed')
    expect(protectionStatusLabel(3)).toBe('Close queued')
    expect(protectionStatusLabel(1)).toBe('Waiting for position')
  })
})
