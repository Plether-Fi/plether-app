import { describe, expect, it } from 'vitest'
import type { PerpsOrderHistoryRow } from '../hooks/usePerpsHistory'
import { getPerpsOrderFailureContext } from './perpsOrderFailure'

const order: PerpsOrderHistoryRow = {
  orderId: 10725n,
  account: '0x1234',
  clientOrderId: '0x5678',
  time: '--',
  market: 'plDXY Perp',
  side: 'Long',
  type: 'Order',
  price: '1.0206',
  size: '--',
  status: 'Failed',
  terminalReason: 'Slippage',
  executionPriceRaw: 97_936_552n,
  receiptEconomics: { executionBountyUsdc: '200000', executionFeeUsdc: '0' },
}

describe('order failure context', () => {
  it('reveals a price-limit breach hidden by four-decimal rounding and uses receipt charges', () => {
    const result = getPerpsOrderFailureContext(order, { direction: 'long', isClose: false, limit: 1.02063 })
    expect(result.limit).toBe('1.02063000')
    expect(result.attemptedPrice).toBe('1.02063448')
    expect(result.explanation).toContain('execution price of 1.02063448 was above your maximum acceptable price of 1.02063000 by 0.00000448')
    expect(result.reward).toBe('0.2 USDC')
    expect(result.executionFee).toBe('0 USDC')
    expect(result.nextStep).toContain('cannot be retried or finalized manually')
  })

  it('explains the minimum price protection for a failed long close', () => {
    const result = getPerpsOrderFailureContext(order, { direction: 'long', isClose: true, limit: 1.02064 })
    expect(result.explanation).toContain('execution price of 1.02063448 was below your minimum acceptable price of 1.02064000 by 0.00000552')
  })

  it('shows the known execution price without inventing a missing limit', () => {
    const result = getPerpsOrderFailureContext(order, {})
    expect(result.explanation).toContain('execution price of 1.02063448')
    expect(result.explanation).toContain('committed price limit is unavailable')
  })

  it('shows the committed limit when execution evidence is missing', () => {
    const result = getPerpsOrderFailureContext({ ...order, executionPriceRaw: undefined }, { direction: 'short', isClose: false, limit: 1.02064 })
    expect(result.explanation).toContain('minimum acceptable price was 1.02064000')
    expect(result.explanation).toContain('attempted execution price is not available yet')
  })

  it('does not claim a numerical breach that the recorded prices do not show', () => {
    const result = getPerpsOrderFailureContext(order, { direction: 'long', isClose: false, limit: 1.021 })
    expect(result.explanation).toContain('recorded execution price was 1.02063448')
    expect(result.explanation).not.toContain('was above')
  })

  it.each([
    ['long', false, 'Maximum'],
    ['short', false, 'Minimum'],
    ['long', true, 'Minimum'],
    ['short', true, 'Maximum'],
  ] as const)('labels the price boundary for %s, close=%s', (direction, isClose, label) => {
    expect(getPerpsOrderFailureContext(order, { direction, isClose }).limitLabel).toBe(`${label} acceptable price`)
  })

  it('does not invent a price or zero charges when receipt evidence is missing', () => {
    const result = getPerpsOrderFailureContext({ ...order, executionPriceRaw: 0n, receiptEconomics: undefined }, {})
    expect(result.attemptedPrice).toBe('Not available')
    expect(result.limit).toBe('Not available')
    expect(result.reward).toBe('Not yet available')
    expect(result.executionFee).toBe('Not yet available')
    expect(result.limitLabel).toBe('Committed price limit')
  })

  it('explains continuing exposure after a failed close', () => {
    const result = getPerpsOrderFailureContext(order, { isClose: true })
    expect(result.outcome).toContain('did not reduce or close your position')
    expect(result.outcome).toContain('does not remove your existing exposure')
  })

  it.each(['AccountLiquidated', 'Account liquidated'])('does not promise released margin after %s', (terminalReason) => {
    const result = getPerpsOrderFailureContext({ ...order, terminalReason }, {})
    expect(result.outcome).toContain('Liquidation changed the account separately')
    expect(result.outcome).not.toContain('was released')
    expect(result.rewardExplanation).toContain('protocol treasury')
  })

  it('keeps unknown failures factual and points to the transaction', () => {
    const result = getPerpsOrderFailureContext({ ...order, terminalReason: 'Unknown (99)' }, { limit: null })
    expect(result.explanation).toContain('does not explain the cause')
    expect(result.nextStep).toContain('finalization transaction')
    expect(result.limit).toBe('No price limit')
  })
})
