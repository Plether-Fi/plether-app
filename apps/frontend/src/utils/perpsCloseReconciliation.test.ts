import { describe, expect, it } from 'vitest'
import type { PerpsOrderReceiptEconomics } from '../hooks/usePerpsHistory'
import { derivePerpsCloseReconciliation } from './perpsCloseReconciliation'
import { closeOrder14Receipt } from './__fixtures__/closeOrder14'
import { closeSettlementAdjustmentReceipt } from './__fixtures__/closeSettlementAdjustment'

const ASSETS = 1_000_000n

function receipt(
  overrides: Partial<PerpsOrderReceiptEconomics> = {}
): PerpsOrderReceiptEconomics {
  return {
    executionNotionalAssets: (1_000n * ASSETS).toString(),
    executionBountyAssets: '0',
    realizedPnlAssets: (30n * ASSETS).toString(),
    vpiAssets: (2n * ASSETS).toString(),
    carryAssets: (4n * ASSETS).toString(),
    executionFeeAssets: (1n * ASSETS).toString(),
    frozenSpreadAssets: '0',
    actionChargeAssessedAssets: (7n * ASSETS).toString(),
    actionChargeCollectedAssets: '0',
    grossAccountDebitAssets: '0',
    preSettlementBalanceAssets: (100n * ASSETS).toString(),
    postSettlementBalanceAssets: (123n * ASSETS).toString(),
    preTraderClaimBalanceAssets: '0',
    postTraderClaimBalanceAssets: '0',
    postPositionSize: '0',
    postPositionMarginAssets: '0',
    postPositionEquityAssets: '0',
    postLeverageBps: '0',
    ...overrides,
  }
}

describe('derivePerpsCloseReconciliation', () => {
  it('keeps a screenshot-sized settlement difference unclassified and reports the actual account change', () => {
    const result = derivePerpsCloseReconciliation(closeSettlementAdjustmentReceipt)!
    expect(result).toMatchObject({
      netCloseResultAssets: -13_228_098_000n,
      settlementAdjustmentAssets: 5_956_960_000n,
      actualAccountChangeAssets: -7_271_138_000n,
      marginAccountChangeAssets: -7_271_138_000n,
      traderClaimChangeAssets: 0n,
    })
    expect(result.netCloseResultAssets + result.settlementAdjustmentAssets)
      .toBe(result.marginAccountChangeAssets + result.traderClaimChangeAssets)
  })

  it('reconciles order 14 without treating VPI netting or withheld profit as a waiver', () => {
    expect(derivePerpsCloseReconciliation(closeOrder14Receipt)).toMatchObject({
      frozenSpreadAssessedAssets: 5_455_562n,
      frozenSpreadChargedAssets: 5_455_562n,
      frozenSpreadWaivedAssets: 0n,
      netCloseResultAssets: -4_731_125n,
      marginAccountChangeAssets: -4_731_125n,
      traderClaimChangeAssets: 0n,
      settlementAdjustmentAssets: 0n,
    })
  })

  it('reconciles a profitable close paid immediately', () => {
    expect(derivePerpsCloseReconciliation(receipt())).toMatchObject({
      realizedPnlAssets: 30n * ASSETS,
      vpiAssets: 2n * ASSETS,
      carryAssets: 4n * ASSETS,
      executionFeeAssets: 1n * ASSETS,
      netCloseResultAssets: 23n * ASSETS,
      actualAccountChangeAssets: 23n * ASSETS,
      marginAccountChangeAssets: 23n * ASSETS,
      traderClaimChangeAssets: 0n,
      settlementAdjustmentAssets: 0n,
    })
  })

  it('includes the finalized execution reward in the exact close result', () => {
    expect(derivePerpsCloseReconciliation(receipt({
      executionNotionalAssets: '9925809400',
      executionBountyAssets: '200000',
      realizedPnlAssets: '-52382100',
      vpiAssets: '667142',
      carryAssets: '42',
      executionFeeAssets: '3970323',
      actionChargeAssessedAssets: '4637507',
      actionChargeCollectedAssets: '4637507',
      grossAccountDebitAssets: '57219607',
      preSettlementBalanceAssets: '14994820546',
      postSettlementBalanceAssets: '14937600939',
    }))).toMatchObject({
      executionBountyAssets: 200_000n,
      netCloseResultAssets: -57_219_607n,
      marginAccountChangeAssets: -57_219_607n,
      traderClaimChangeAssets: 0n,
      settlementAdjustmentAssets: 0n,
    })
  })

  it('reconciles VPI rebates and a newly created trader claim', () => {
    expect(derivePerpsCloseReconciliation(receipt({
      vpiAssets: (-2n * ASSETS).toString(),
      actionChargeAssessedAssets: (3n * ASSETS).toString(),
      postSettlementBalanceAssets: (100n * ASSETS).toString(),
      postTraderClaimBalanceAssets: (27n * ASSETS).toString(),
    }))).toMatchObject({
      netCloseResultAssets: 27n * ASSETS,
      actualAccountChangeAssets: 27n * ASSETS,
      marginAccountChangeAssets: 0n,
      traderClaimChangeAssets: 27n * ASSETS,
    })
  })

  it('reconciles a loss that consumes account funds and an existing claim', () => {
    expect(derivePerpsCloseReconciliation(receipt({
      realizedPnlAssets: (-90n * ASSETS).toString(),
      actionChargeCollectedAssets: (7n * ASSETS).toString(),
      grossAccountDebitAssets: (97n * ASSETS).toString(),
      preSettlementBalanceAssets: (80n * ASSETS).toString(),
      postSettlementBalanceAssets: '0',
      preTraderClaimBalanceAssets: (17n * ASSETS).toString(),
      postTraderClaimBalanceAssets: '0',
    }))).toMatchObject({
      netCloseResultAssets: -97n * ASSETS,
      actualAccountChangeAssets: -97n * ASSETS,
      marginAccountChangeAssets: -80n * ASSETS,
      traderClaimChangeAssets: -17n * ASSETS,
      settlementAdjustmentAssets: 0n,
    })
  })

  it('reports a terminal settlement adjustment without assigning a cause', () => {
    expect(derivePerpsCloseReconciliation(receipt({
      realizedPnlAssets: (-100n * ASSETS).toString(),
      actionChargeCollectedAssets: (7n * ASSETS).toString(),
      grossAccountDebitAssets: (100n * ASSETS).toString(),
      preSettlementBalanceAssets: (80n * ASSETS).toString(),
      postSettlementBalanceAssets: '0',
      preTraderClaimBalanceAssets: (20n * ASSETS).toString(),
      postTraderClaimBalanceAssets: '0',
    }))).toMatchObject({
      netCloseResultAssets: -107n * ASSETS,
      actualAccountChangeAssets: -100n * ASSETS,
      marginAccountChangeAssets: -80n * ASSETS,
      traderClaimChangeAssets: -20n * ASSETS,
      settlementAdjustmentAssets: 7n * ASSETS,
    })
  })

  it('derives fully paid and partially waived frozen spread from collection priority', () => {
    const fullyPaid = derivePerpsCloseReconciliation(receipt({
      frozenSpreadAssets: (5n * ASSETS).toString(),
      actionChargeAssessedAssets: (12n * ASSETS).toString(),
      postSettlementBalanceAssets: (118n * ASSETS).toString(),
    }))
    expect(fullyPaid).toMatchObject({
      frozenSpreadAssessedAssets: 5n * ASSETS,
      frozenSpreadChargedAssets: 5n * ASSETS,
      frozenSpreadWaivedAssets: 0n,
      netCloseResultAssets: 18n * ASSETS,
    })

    const partiallyWaived = derivePerpsCloseReconciliation(receipt({
      realizedPnlAssets: (-92n * ASSETS).toString(),
      frozenSpreadAssets: (5n * ASSETS).toString(),
      actionChargeAssessedAssets: (12n * ASSETS).toString(),
      actionChargeCollectedAssets: (8n * ASSETS).toString(),
      grossAccountDebitAssets: (100n * ASSETS).toString(),
      preSettlementBalanceAssets: (100n * ASSETS).toString(),
      postSettlementBalanceAssets: '0',
    }))
    expect(partiallyWaived).toMatchObject({
      frozenSpreadAssessedAssets: 5n * ASSETS,
      frozenSpreadChargedAssets: 1n * ASSETS,
      frozenSpreadWaivedAssets: 4n * ASSETS,
      netCloseResultAssets: -100n * ASSETS,
      settlementAdjustmentAssets: 0n,
    })
    expect(
      partiallyWaived!.frozenSpreadChargedAssets + partiallyWaived!.frozenSpreadWaivedAssets
    ).toBe(partiallyWaived!.frozenSpreadAssessedAssets)
  })

  it('shows remaining and released margin only from an authoritative snapshot', () => {
    const partial = receipt({
      postPositionSize: (500n * 10n ** 18n).toString(),
      postPositionMarginAssets: (250n * ASSETS).toString(),
    })
    expect(derivePerpsCloseReconciliation(partial)).toMatchObject({
      postPositionMarginAssets: 250n * ASSETS,
      releasedPositionMarginAssets: undefined,
    })
    expect(derivePerpsCloseReconciliation(partial, {
      preExecutionPositionMarginAssets: 500n * ASSETS,
    })).toMatchObject({
      releasedPositionMarginAssets: 250n * ASSETS,
    })
    expect(derivePerpsCloseReconciliation(partial, {
      preExecutionPositionMarginAssets: 200n * ASSETS,
    })).toMatchObject({
      releasedPositionMarginAssets: undefined,
    })
  })

  it.each([
    { pnl: 2n, collected: 6n, waived: 0n, net: -6n },
    { pnl: 2n, collected: 4n, waived: 2n, net: -4n },
    { pnl: 10n, collected: 0n, waived: 0n, net: 2n },
  ])('separates VPI credit, profit withholding and actual waiver (case %#)', ({ pnl, collected, waived, net }) => {
    expect(derivePerpsCloseReconciliation(receipt({
      realizedPnlAssets: (pnl * ASSETS).toString(),
      vpiAssets: (-2n * ASSETS).toString(),
      frozenSpreadAssets: (5n * ASSETS).toString(),
      actionChargeAssessedAssets: (8n * ASSETS).toString(),
      actionChargeCollectedAssets: (collected * ASSETS).toString(),
      grossAccountDebitAssets: (collected * ASSETS).toString(),
      postSettlementBalanceAssets: ((100n + net) * ASSETS).toString(),
    }))).toMatchObject({
      frozenSpreadChargedAssets: (5n - waived) * ASSETS,
      frozenSpreadWaivedAssets: waived * ASSETS,
      netCloseResultAssets: net * ASSETS,
      settlementAdjustmentAssets: 0n,
    })
  })

  it.each([
    { vpi: -10n, assessed: 0n, collected: 0n, net: 30n },
    { vpi: -12n, assessed: 0n, collected: 4n, net: 32n },
    { vpi: -8n, assessed: 2n, collected: 4n, net: 28n },
  ])('handles net rebates and already-realized carry (case %#)', ({ vpi, assessed, collected, net }) => {
    expect(derivePerpsCloseReconciliation(receipt({
      vpiAssets: (vpi * ASSETS).toString(),
      frozenSpreadAssets: (5n * ASSETS).toString(),
      actionChargeAssessedAssets: (assessed * ASSETS).toString(),
      actionChargeCollectedAssets: (collected * ASSETS).toString(),
      grossAccountDebitAssets: (collected * ASSETS).toString(),
      postSettlementBalanceAssets: ((100n + net) * ASSETS).toString(),
    }))).toMatchObject({
      frozenSpreadChargedAssets: 5n * ASSETS,
      frozenSpreadWaivedAssets: 0n,
      netCloseResultAssets: net * ASSETS,
      settlementAdjustmentAssets: 0n,
    })
  })

  it('does not invent a cause when receipt balances do not support the calculated rebate', () => {
    expect(derivePerpsCloseReconciliation(receipt({
      vpiAssets: (-12n * ASSETS).toString(),
      frozenSpreadAssets: (5n * ASSETS).toString(),
      actionChargeAssessedAssets: '0',
      postSettlementBalanceAssets: (131n * ASSETS).toString(),
    }))).toBeUndefined()
  })

  it.each([
    undefined,
    receipt({ executionBountyAssets: undefined }),
    receipt({ realizedPnlAssets: undefined }),
    receipt({ realizedPnlAssets: 'not-a-number' }),
    receipt({ actionChargeCollectedAssets: (8n * ASSETS).toString() }),
    receipt({ carryAssets: '-1' }),
    receipt({ frozenSpreadAssets: (7n * ASSETS).toString() }),
    receipt({ executionBountyAssets: (8n * ASSETS).toString() }),
    receipt({ postPositionMarginAssets: (1n * ASSETS).toString() }),
    receipt({
      postPositionSize: (1n * 10n ** 18n).toString(),
      postPositionMarginAssets: (1n * ASSETS).toString(),
      postPositionEquityAssets: '-1',
    }),
    receipt({ postSettlementBalanceAssets: (122n * ASSETS).toString() }),
    receipt({
      realizedPnlAssets: (-100n * ASSETS).toString(),
      preSettlementBalanceAssets: (80n * ASSETS).toString(),
      postSettlementBalanceAssets: '0',
      postPositionSize: (1n * 10n ** 18n).toString(),
      postPositionMarginAssets: (1n * ASSETS).toString(),
    }),
  ])('rejects missing, malformed, legacy, and inconsistent receipt evidence', (value) => {
    expect(derivePerpsCloseReconciliation(value)).toBeUndefined()
  })
})
