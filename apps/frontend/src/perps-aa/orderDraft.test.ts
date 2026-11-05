import { describe, expect, it } from 'vitest'
import { restoredOrderDraft, type SavedOrderDraft } from './orderDraft'
import type { SponsoredOperation } from './operationStore'

const draft: SavedOrderDraft = { version: 1, direction: 'short', orderQuantity: '125.25', leverage: 3,
  slippage: 0.5, reduceOnly: false, fullClose: false, maxOpen: false, protectionEnabled: true,
  protection: { mode: 'percent', takeProfit: '10', stopLoss: '5' } }
describe('editable order restoration', () => {
  it('preserves editable inputs and protection settings', () => {
    expect(restoredOrderDraft({ orderDraft: draft } as SponsoredOperation)).toEqual({ draft, incomplete: false })
  })
  it('restores only known legacy inputs without carrying old execution bounds', () => {
    const operation = { orderRequestV3: { side: 1, sizeDelta: '125250000000000000000', isClose: true,
      submitBy: '1', executionWindowSeconds: 60, clientOrderId: 'old-id', targetPrice: '999' } } as SponsoredOperation
    expect(restoredOrderDraft(operation)).toEqual({ draft: { direction: 'short', orderQuantity: '125.25', reduceOnly: true }, incomplete: true })
  })
  it('handles missing or malformed optional draft metadata', () => {
    expect(restoredOrderDraft({} as SponsoredOperation)).toEqual({ draft: {}, incomplete: true })
    expect(restoredOrderDraft({ orderDraft: { ...draft, protection: null } } as unknown as SponsoredOperation).incomplete).toBe(true)
  })
})
