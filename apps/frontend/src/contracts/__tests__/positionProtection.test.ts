import { describe, expect, it } from 'vitest'
import { protectionParamsFromInputs, protectionReturnPercent, validateProtectionParams } from '../positionProtection'

describe('SL/TP displayed price conversion', () => {
  const context = { rawMark: 80_000_000n, cap: 200_000_000n } // display $1.20; deliberately not $1.00
  it('inverts long dollar-oriented trigger prices', () => {
    expect(protectionParamsFromInputs({ ...context, direction: 'long', mode: 'price', takeProfit: '1.32', stopLoss: '1.08' })).toEqual({ takeProfitTriggerPrice: 68_000_000n, stopLossTriggerPrice: 92_000_000n })
  })
  it.each(['long', 'short'] as const)('uses gross return on margin from entry for %s percentages', direction => {
    const position = { entryPrice: 80_000_000n, size: 1_000n * 10n ** 18n, marginUsdc: 100_000_000n }
    const params = protectionParamsFromInputs({ ...context, position, direction, mode: 'percent', takeProfit: '50', stopLoss: '90' })
    expect(params).toEqual(direction === 'long'
      ? { takeProfitTriggerPrice: 75_000_000n, stopLossTriggerPrice: 89_000_000n }
      : { takeProfitTriggerPrice: 85_000_000n, stopLossTriggerPrice: 71_000_000n })
    expect(protectionReturnPercent(params.takeProfitTriggerPrice, direction, position)).toBe(500_000n)
    expect(protectionReturnPercent(params.stopLossTriggerPrice, direction, position)).toBe(-900_000n)
    // A changed mark validates trigger direction but does not move the return target.
    expect(protectionParamsFromInputs({ ...context, rawMark: 81_000_000n, position, direction, mode: 'percent', takeProfit: '50', stopLoss: '90' })).toEqual(params)
  })
  it('places a 90% short loss before 1.03 liquidation instead of at 1.91', () => {
    const params = protectionParamsFromInputs({ rawMark: 99_500_000n, cap: context.cap, direction: 'short', mode: 'percent', takeProfit: '', stopLoss: '90',
      position: { entryPrice: 99_500_000n, size: 10_000n * 10n ** 18n, marginUsdc: 250_000_000n }, liquidationPrice: 97_000_000n })
    expect(params.stopLossTriggerPrice).toBe(97_250_000n) // Displayed 1.0275; loss = 225 USDC.
  })
  it.each(['long', 'short'] as const)('rejects %s stops at or beyond liquidation in either input mode', direction => {
    const input = { rawMark: 100_000_000n, cap: context.cap, direction, takeProfit: '',
      position: { entryPrice: 100_000_000n, size: 1_000n * 10n ** 18n, marginUsdc: 100_000_000n },
      liquidationPrice: direction === 'long' ? 108_000_000n : 92_000_000n }
    for (const stopLoss of ['80', '90']) expect(() => protectionParamsFromInputs({ ...input, mode: 'percent', stopLoss })).toThrow('liquidation price')
    for (const stopLoss of direction === 'long' ? ['0.92', '0.91'] : ['1.08', '1.09']) expect(() => protectionParamsFromInputs({ ...input, mode: 'price', stopLoss })).toThrow('liquidation price')
    expect(() => protectionParamsFromInputs({ ...input, mode: 'percent', stopLoss: '79' })).not.toThrow()
  })
  it('requires position data for return percentages without preventing absolute prices', () => {
    const input = { ...context, direction: 'long' as const, takeProfit: '10', stopLoss: '' }
    expect(() => protectionParamsFromInputs({ ...input, mode: 'percent' })).toThrow('Waiting for position')
    expect(() => protectionParamsFromInputs({ ...input, mode: 'percent', position: { entryPrice: context.rawMark, size: 1n, marginUsdc: 0n } })).toThrow('Waiting for position')
    expect(() => protectionParamsFromInputs({ ...input, mode: 'price', takeProfit: '1.3' })).not.toThrow()
  })
  it('supports a stop that locks in a gain and a breakeven stop', () => {
    const input = { ...context, direction: 'long' as const, rawMark: 70_000_000n, mode: 'percent' as const, takeProfit: '',
      position: { entryPrice: 80_000_000n, size: 1_000n * 10n ** 18n, marginUsdc: 100_000_000n } }
    expect(protectionParamsFromInputs({ ...input, stopLoss: '-50' }).stopLossTriggerPrice).toBe(75_000_000n)
    expect(protectionParamsFromInputs({ ...input, stopLoss: '0' }).stopLossTriggerPrice).toBe(80_000_000n)
  })
  it('rejects crossed legs, precision loss, and cap boundaries', () => {
    expect(() => validateProtectionParams({ takeProfitTriggerPrice: 80_000_000n, stopLossTriggerPrice: 0n }, 'long', context.rawMark, context.cap)).toThrow('above')
    for (const takeProfit of ['2', '0', '1.200000001', '-1', '1e2']) {
      expect(() => protectionParamsFromInputs({ ...context, direction: 'long', mode: 'price', takeProfit, stopLoss: '' })).toThrow()
    }
  })
})
