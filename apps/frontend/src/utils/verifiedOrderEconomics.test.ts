import type { OrderReceiptV5 } from '@plether-fi/perps-aa-client'
import { describe, expect, it } from 'vitest'
import { closeOrder180Receipt } from './__fixtures__/closeOrder180'
import { derivePerpsCloseReconciliation } from './perpsCloseReconciliation'
import { economicsFromVerifiedOrderReceipt } from './verifiedOrderEconomics'

function receipt(): OrderReceiptV5 {
  const known = closeOrder180Receipt
  return {
    orderId: 180n,
    account: '0x1111111111111111111111111111111111111111',
    clientOrderId: `0x${'12'.repeat(32)}`,
    intentHash: `0x${'34'.repeat(32)}`,
    expectedConfigHash: `0x${'56'.repeat(32)}`,
    observedConfigHash: `0x${'56'.repeat(32)}`,
    status: 2, reason: 1, executionMode: 1,
    executor: '0x2222222222222222222222222222222222222222',
    priceSource: 1, executionPrice: 99_000_000n, neutralMarkPrice: 98_999_000n,
    poolDepthAssets: 1_000_000n, oraclePublishTime: 1_790_000_000n, priceReachedEngine: true,
    bountyAssets: BigInt(known.executionBountyAssets),
    bountyRecipient: '0x2222222222222222222222222222222222222222',
    bountyDisposition: 0,
    failure: { selector: '0x00000000', category: 0, code: 0, constraint: 0,
      actual: 0n, limit: 0n, revertDataHash: `0x${'00'.repeat(32)}` },
    economics: {
      executionNotionalAssets: BigInt(known.executionNotionalAssets),
      realizedPnlAssets: BigInt(known.realizedPnlAssets),
      vpiAssets: BigInt(known.vpiAssets),
      carryAssets: BigInt(known.carryAssets),
      executionFeeAssets: BigInt(known.executionFeeAssets),
      frozenSpreadAssets: BigInt(known.frozenSpreadAssets),
      actionChargeAssessedAssets: BigInt(known.actionChargeAssessedAssets),
      actionChargeCollectedAssets: BigInt(known.actionChargeCollectedAssets),
      grossAccountDebitAssets: BigInt(known.grossAccountDebitAssets),
      preSettlementBalanceAssets: BigInt(known.preSettlementBalanceAssets),
      postSettlementBalanceAssets: BigInt(known.postSettlementBalanceAssets),
      preTraderClaimBalanceAssets: BigInt(known.preTraderClaimBalanceAssets),
      postTraderClaimBalanceAssets: BigInt(known.postTraderClaimBalanceAssets),
      postPositionSize: BigInt(known.postPositionSize),
      postPositionMarginAssets: BigInt(known.postPositionMarginAssets),
      postPositionEquityAssets: BigInt(known.postPositionEquityAssets),
      postLeverageBps: BigInt(known.postLeverageBps),
      close: { postFreeSettlementAssets: 0n, safeMarginReleaseAssets: 0n,
        actionChargeFromReleasedMarginAssets: 0n, netReleasedMarginAssets: 0n,
        priceLossAssets: 0n, pricePnlClaimConsumedAssets: 0n, pricePnlPledgeConsumedAssets: 0n,
        priceLossWrittenOffAssets: 0n, actionChargeWithheldAssets: 0n, actionChargeWaivedAssets: 0n },
    },
    commitment: { carryCollectedAssets: BigInt(known.commitmentCarryAssets), carryOutstandingAssets: 0n,
      bountyFromFreeAssets: 0n, settlementBeforeAssets: 0n, settlementAfterAssets: 0n, freeSettlementAfterAssets: 0n },
    bounty: { bountyEntitlementAssets: 0n, bountyPaidAssets: 0n, bountyRefundedAssets: 0n,
      bountyForfeitedAssets: 0n, discrepancy: 0 },
    timing: { submitBy: 1_790_000_000n, executionWindowSeconds: 60,
      commitTimestamp: 1_790_000_000n, executionDeadline: 1_790_000_060n },
  }
}

describe('verified order receipt economics', () => {
  it('matches the known order-180 receipt fields and reconciles commitment carry once', () => {
    const projected = economicsFromVerifiedOrderReceipt(receipt())
    const expected = Object.fromEntries(Object.entries(closeOrder180Receipt).filter(([key]) =>
      !['totalPositionVpiAssets', 'positionLifetimeNetResultAssets', 'positionLifetimeTradesResultAssets',
        'positionLifetimeAccountAdjustmentAssets'].includes(key)))
    expect(projected.receiptEconomics).toEqual(expected)
    expect(projected).toMatchObject({ vpiAssetsRaw: 4_636_317_700n,
      frozenCloseSpreadAssetsRaw: 0n, executionEconomicsVersion: 2 })
    expect(derivePerpsCloseReconciliation(projected.receiptEconomics)).toMatchObject({
      commitmentCarryAssets: 70_278n, carryAssets: 660n,
      netCloseResultAssets: -5_102_516_914n, actualAccountChangeAssets: -5_102_516_914n,
      settlementAdjustmentAssets: 0n,
    })
  })

  it('preserves signed credits, carry and equity, and separates bounty from its accounting breakdown', () => {
    const verified = receipt()
    verified.bountyAssets = 123n
    verified.bounty.bountyPaidAssets = 45n
    verified.economics.realizedPnlAssets = -789n
    verified.economics.vpiAssets = -123_456n
    verified.economics.carryAssets = -22n
    verified.economics.postPositionEquityAssets = -456n
    const projected = economicsFromVerifiedOrderReceipt(verified)
    expect(projected.vpiAssetsRaw).toBe(-123_456n)
    expect(projected.receiptEconomics).toMatchObject({
      executionBountyAssets: '123', realizedPnlAssets: '-789', vpiAssets: '-123456',
      carryAssets: '-22', postPositionEquityAssets: '-456',
    })
  })

  it('does not invent aggregate or oracle evidence from receipt price fields', () => {
    const projected = economicsFromVerifiedOrderReceipt(receipt())
    expect(Object.keys(projected).sort()).toEqual([
      'executionEconomicsVersion', 'frozenCloseSpreadAssetsRaw', 'receiptEconomics', 'vpiAssetsRaw',
    ])
    expect(projected.receiptEconomics).not.toHaveProperty('totalPositionVpiAssets')
    expect(projected.receiptEconomics).not.toHaveProperty('positionLifetimeNetResultAssets')
    expect(projected).not.toHaveProperty('executionOraclePriceRaw')
    expect(projected).not.toHaveProperty('oracleMinPublishTimeRaw')
  })
})
