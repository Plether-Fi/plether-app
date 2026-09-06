import { describe, expect, it } from 'vitest'
import { protectionParamsFromInputs, validateProtectionParams } from '../positionProtection'

describe('SL/TP displayed price conversion', () => {
  const context = { rawMark: 80_000_000n, cap: 200_000_000n } // display $1.20; deliberately not $1.00
  it('inverts long dollar-oriented trigger prices', () => {
    expect(protectionParamsFromInputs({ ...context, direction: 'long', mode: 'price', takeProfit: '1.32', stopLoss: '1.08' })).toEqual({ takeProfitTriggerPrice: 68_000_000n, stopLossTriggerPrice: 92_000_000n })
  })
  it('uses unlevered price movement for percent inputs in either direction', () => {
    expect(protectionParamsFromInputs({ ...context, direction: 'long', mode: 'percent', takeProfit: '10', stopLoss: '10' })).toEqual({ takeProfitTriggerPrice: 68_000_000n, stopLossTriggerPrice: 92_000_000n })
    expect(protectionParamsFromInputs({ ...context, direction: 'short', mode: 'percent', takeProfit: '10', stopLoss: '' })).toEqual({ takeProfitTriggerPrice: 92_000_000n, stopLossTriggerPrice: 0n })
  })
  it('rejects crossed legs, precision loss, and cap boundaries', () => {
    expect(() => validateProtectionParams({ takeProfitTriggerPrice: 80_000_000n, stopLossTriggerPrice: 0n }, 'long', context.rawMark, context.cap)).toThrow('above')
    for (const takeProfit of ['2', '0', '1.200000001', '-1', '1e2']) {
      expect(() => protectionParamsFromInputs({ ...context, direction: 'long', mode: 'price', takeProfit, stopLoss: '' })).toThrow()
    }
  })
})
