import { describe, expect, it } from 'vitest'
import { prepared } from '../../test/fixtures/preparedOrder'
import { perpsReviewChanges } from '../perpsReviewChanges'

describe('refreshed review changes', () => {
  it('shows a changed bounty ceiling even when the current quote is unchanged', () => {
    const before = prepared()
    const after = structuredClone(before)
    before.request.bounds.maxExecutionBountyUsdc = 120_251n
    after.request.bounds.maxExecutionBountyUsdc = 120_252n
    expect(perpsReviewChanges(before, after)).toEqual([
      { label: 'Maximum execution reward', before: '0.120251 USDC', after: '0.120252 USDC' },
    ])
  })
  it('ignores refreshed IDs, block metadata, and deadlines', () => {
    const before = prepared()
    const after = structuredClone(before)
    after.reviewedBlockNumber++
    after.reviewedBlockHash = '0x99'
    after.request.clientOrderId = '0x99'
    after.protection.submitBy++
    after.request.bounds.submitBy++
    expect(perpsReviewChanges(before, after)).toEqual([])
  })
  it('shows full precision for one-unit price and funding changes', () => {
    const before = prepared()
    const after = structuredClone(before)
    after.request.targetPrice++
    after.request.marginDelta++
    expect(perpsReviewChanges(before, after)).toEqual([
      { label: 'Execution limit', before: '1 USDC', after: '0.99999999 USDC' },
      { label: 'Required margin', before: '20 USDC', after: '20.000001 USDC' },
    ])
  })
  it('highlights a changed Max quantity at full precision', () => {
    const before = prepared()
    const after = structuredClone(before)
    before.request.sizeDelta = 300n * 10n ** 18n
    after.request.sizeDelta = before.request.sizeDelta + 1n
    expect(perpsReviewChanges(before, after)).toEqual([
      { label: 'Order quantity', before: '300 plDXY', after: '300.000000000000000001 plDXY' },
    ])
  })
  it('identifies changed execution rules, protection prices, and rewards', () => {
    const before = prepared()
    const after = structuredClone(before)
    after.protection.executionMode = 2
    after.request.bounds.expectedConfigHash = '0x99'
    after.executionBountyUsdc++
    after.positionProtection = { book: after.account, params: { takeProfitTriggerPrice: 95_000_000n, stopLossTriggerPrice: 110_000_000n }, triggerBountyUsdc: 200_000n, executionBountyUsdc: 200_000n }
    expect(perpsReviewChanges(before, after).map(change => change.label)).toEqual([
      'Execution reward', 'Execution conditions', 'Execution configuration', 'Take profit', 'Stop loss', 'Protection trigger reward', 'Protection execution reward',
    ])
  })
})
