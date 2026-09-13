import { describe, expect, it } from 'vitest'
import type { PerpsOrderReceiptEconomics } from '../hooks/usePerpsHistory'
import { derivePerpsCloseReconciliation } from './perpsCloseReconciliation'
import { closeOrder14Receipt } from './__fixtures__/closeOrder14'

const USDC = 1_000_000n

function receipt(
  overrides: Partial<PerpsOrderReceiptEconomics> = {}
): PerpsOrderReceiptEconomics {
  return {
    executionNotionalUsdc: (1_000n * USDC).toString(),
    executionBountyUsdc: '0',
    realizedPnlUsdc: (30n * USDC).toString(),
    vpiUsdc: (2n * USDC).toString(),
    carryUsdc: (4n * USDC).toString(),
    executionFeeUsdc: (1n * USDC).toString(),
    frozenSpreadUsdc: '0',
    actionChargeAssessedUsdc: (7n * USDC).toString(),
    actionChargeCollectedUsdc: '0',
    grossAccountDebitUsdc: '0',
    preSettlementBalanceUsdc: (100n * USDC).toString(),
    postSettlementBalanceUsdc: (123n * USDC).toString(),
    preTraderClaimBalanceUsdc: '0',
    postTraderClaimBalanceUsdc: '0',
    postPositionSize: '0',
    postPositionMarginUsdc: '0',
    postPositionEquityUsdc: '0',
    postLeverageBps: '0',
    ...overrides,
  }
}

describe('derivePerpsCloseReconciliation', () => {
  it('reconciles order 14 without treating VPI netting or withheld profit as a waiver', () => {
    expect(derivePerpsCloseReconciliation(closeOrder14Receipt)).toMatchObject({
      frozenSpreadAssessedUsdc: 5_455_562n,
      frozenSpreadChargedUsdc: 5_455_562n,
      frozenSpreadWaivedUsdc: 0n,
      netCloseResultUsdc: -4_731_125n,
      marginAccountChangeUsdc: -4_731_125n,
      traderClaimChangeUsdc: 0n,
      uncoveredLossUsdc: 0n,
    })
  })

  it('reconciles a profitable close paid immediately', () => {
    expect(derivePerpsCloseReconciliation(receipt())).toMatchObject({
      realizedPnlUsdc: 30n * USDC,
      vpiUsdc: 2n * USDC,
      carryUsdc: 4n * USDC,
      executionFeeUsdc: 1n * USDC,
      netCloseResultUsdc: 23n * USDC,
      marginAccountChangeUsdc: 23n * USDC,
      traderClaimChangeUsdc: 0n,
      uncoveredLossUsdc: 0n,
    })
  })

  it('includes the finalized execution reward in the exact close result', () => {
    expect(derivePerpsCloseReconciliation(receipt({
      executionNotionalUsdc: '9925809400',
      executionBountyUsdc: '200000',
      realizedPnlUsdc: '-52382100',
      vpiUsdc: '667142',
      carryUsdc: '42',
      executionFeeUsdc: '3970323',
      actionChargeAssessedUsdc: '4637507',
      actionChargeCollectedUsdc: '4637507',
      grossAccountDebitUsdc: '57219607',
      preSettlementBalanceUsdc: '14994820546',
      postSettlementBalanceUsdc: '14937600939',
    }))).toMatchObject({
      executionBountyUsdc: 200_000n,
      netCloseResultUsdc: -57_219_607n,
      marginAccountChangeUsdc: -57_219_607n,
      traderClaimChangeUsdc: 0n,
      uncoveredLossUsdc: 0n,
    })
  })

  it('reconciles VPI rebates and a newly created trader claim', () => {
    expect(derivePerpsCloseReconciliation(receipt({
      vpiUsdc: (-2n * USDC).toString(),
      actionChargeAssessedUsdc: (3n * USDC).toString(),
      postSettlementBalanceUsdc: (100n * USDC).toString(),
      postTraderClaimBalanceUsdc: (27n * USDC).toString(),
    }))).toMatchObject({
      netCloseResultUsdc: 27n * USDC,
      marginAccountChangeUsdc: 0n,
      traderClaimChangeUsdc: 27n * USDC,
    })
  })

  it('reconciles a loss that consumes account funds and an existing claim', () => {
    expect(derivePerpsCloseReconciliation(receipt({
      realizedPnlUsdc: (-90n * USDC).toString(),
      actionChargeCollectedUsdc: (7n * USDC).toString(),
      grossAccountDebitUsdc: (97n * USDC).toString(),
      preSettlementBalanceUsdc: (80n * USDC).toString(),
      postSettlementBalanceUsdc: '0',
      preTraderClaimBalanceUsdc: (17n * USDC).toString(),
      postTraderClaimBalanceUsdc: '0',
    }))).toMatchObject({
      netCloseResultUsdc: -97n * USDC,
      marginAccountChangeUsdc: -80n * USDC,
      traderClaimChangeUsdc: -17n * USDC,
      uncoveredLossUsdc: 0n,
    })
  })

  it('records only the terminal shortfall as uncovered loss', () => {
    expect(derivePerpsCloseReconciliation(receipt({
      realizedPnlUsdc: (-100n * USDC).toString(),
      actionChargeCollectedUsdc: (7n * USDC).toString(),
      grossAccountDebitUsdc: (100n * USDC).toString(),
      preSettlementBalanceUsdc: (80n * USDC).toString(),
      postSettlementBalanceUsdc: '0',
      preTraderClaimBalanceUsdc: (20n * USDC).toString(),
      postTraderClaimBalanceUsdc: '0',
    }))).toMatchObject({
      netCloseResultUsdc: -107n * USDC,
      marginAccountChangeUsdc: -80n * USDC,
      traderClaimChangeUsdc: -20n * USDC,
      uncoveredLossUsdc: 7n * USDC,
    })
  })

  it('derives fully paid and partially waived frozen spread from collection priority', () => {
    const fullyPaid = derivePerpsCloseReconciliation(receipt({
      frozenSpreadUsdc: (5n * USDC).toString(),
      actionChargeAssessedUsdc: (12n * USDC).toString(),
      postSettlementBalanceUsdc: (118n * USDC).toString(),
    }))
    expect(fullyPaid).toMatchObject({
      frozenSpreadAssessedUsdc: 5n * USDC,
      frozenSpreadChargedUsdc: 5n * USDC,
      frozenSpreadWaivedUsdc: 0n,
      netCloseResultUsdc: 18n * USDC,
    })

    const partiallyWaived = derivePerpsCloseReconciliation(receipt({
      realizedPnlUsdc: (-92n * USDC).toString(),
      frozenSpreadUsdc: (5n * USDC).toString(),
      actionChargeAssessedUsdc: (12n * USDC).toString(),
      actionChargeCollectedUsdc: (8n * USDC).toString(),
      grossAccountDebitUsdc: (100n * USDC).toString(),
      preSettlementBalanceUsdc: (100n * USDC).toString(),
      postSettlementBalanceUsdc: '0',
    }))
    expect(partiallyWaived).toMatchObject({
      frozenSpreadAssessedUsdc: 5n * USDC,
      frozenSpreadChargedUsdc: 1n * USDC,
      frozenSpreadWaivedUsdc: 4n * USDC,
      netCloseResultUsdc: -100n * USDC,
      uncoveredLossUsdc: 0n,
    })
    expect(
      partiallyWaived!.frozenSpreadChargedUsdc + partiallyWaived!.frozenSpreadWaivedUsdc
    ).toBe(partiallyWaived!.frozenSpreadAssessedUsdc)
  })

  it('shows remaining and released margin only from an authoritative snapshot', () => {
    const partial = receipt({
      postPositionSize: (500n * 10n ** 18n).toString(),
      postPositionMarginUsdc: (250n * USDC).toString(),
    })
    expect(derivePerpsCloseReconciliation(partial)).toMatchObject({
      postPositionMarginUsdc: 250n * USDC,
      releasedPositionMarginUsdc: undefined,
    })
    expect(derivePerpsCloseReconciliation(partial, {
      preExecutionPositionMarginUsdc: 500n * USDC,
    })).toMatchObject({
      releasedPositionMarginUsdc: 250n * USDC,
    })
    expect(derivePerpsCloseReconciliation(partial, {
      preExecutionPositionMarginUsdc: 200n * USDC,
    })).toMatchObject({
      releasedPositionMarginUsdc: undefined,
    })
  })

  it.each([
    { pnl: 2n, collected: 6n, waived: 0n, net: -6n },
    { pnl: 2n, collected: 4n, waived: 2n, net: -4n },
    { pnl: 10n, collected: 0n, waived: 0n, net: 2n },
  ])('separates VPI credit, profit withholding and actual waiver (case %#)', ({ pnl, collected, waived, net }) => {
    expect(derivePerpsCloseReconciliation(receipt({
      realizedPnlUsdc: (pnl * USDC).toString(),
      vpiUsdc: (-2n * USDC).toString(),
      frozenSpreadUsdc: (5n * USDC).toString(),
      actionChargeAssessedUsdc: (8n * USDC).toString(),
      actionChargeCollectedUsdc: (collected * USDC).toString(),
      grossAccountDebitUsdc: (collected * USDC).toString(),
      postSettlementBalanceUsdc: ((100n + net) * USDC).toString(),
    }))).toMatchObject({
      frozenSpreadChargedUsdc: (5n - waived) * USDC,
      frozenSpreadWaivedUsdc: waived * USDC,
      netCloseResultUsdc: net * USDC,
      uncoveredLossUsdc: 0n,
    })
  })

  it.each([
    { vpi: -10n, assessed: 0n, collected: 0n, net: 30n },
    { vpi: -12n, assessed: 0n, collected: 4n, net: 32n },
    { vpi: -8n, assessed: 2n, collected: 4n, net: 28n },
  ])('handles net rebates and already-realized carry (case %#)', ({ vpi, assessed, collected, net }) => {
    expect(derivePerpsCloseReconciliation(receipt({
      vpiUsdc: (vpi * USDC).toString(),
      frozenSpreadUsdc: (5n * USDC).toString(),
      actionChargeAssessedUsdc: (assessed * USDC).toString(),
      actionChargeCollectedUsdc: (collected * USDC).toString(),
      grossAccountDebitUsdc: (collected * USDC).toString(),
      postSettlementBalanceUsdc: ((100n + net) * USDC).toString(),
    }))).toMatchObject({
      frozenSpreadChargedUsdc: 5n * USDC,
      frozenSpreadWaivedUsdc: 0n,
      netCloseResultUsdc: net * USDC,
      uncoveredLossUsdc: 0n,
    })
  })

  it('does not invent a cause when receipt balances do not support the calculated rebate', () => {
    expect(derivePerpsCloseReconciliation(receipt({
      vpiUsdc: (-12n * USDC).toString(),
      frozenSpreadUsdc: (5n * USDC).toString(),
      actionChargeAssessedUsdc: '0',
      postSettlementBalanceUsdc: (131n * USDC).toString(),
    }))).toBeUndefined()
  })

  it.each([
    undefined,
    receipt({ executionBountyUsdc: undefined }),
    receipt({ realizedPnlUsdc: undefined }),
    receipt({ realizedPnlUsdc: 'not-a-number' }),
    receipt({ actionChargeCollectedUsdc: (8n * USDC).toString() }),
    receipt({ carryUsdc: '-1' }),
    receipt({ frozenSpreadUsdc: (7n * USDC).toString() }),
    receipt({ executionBountyUsdc: (8n * USDC).toString() }),
    receipt({ postPositionMarginUsdc: (1n * USDC).toString() }),
    receipt({
      postPositionSize: (1n * 10n ** 18n).toString(),
      postPositionMarginUsdc: (1n * USDC).toString(),
      postPositionEquityUsdc: '-1',
    }),
    receipt({ postSettlementBalanceUsdc: (122n * USDC).toString() }),
    receipt({
      realizedPnlUsdc: (-100n * USDC).toString(),
      preSettlementBalanceUsdc: (80n * USDC).toString(),
      postSettlementBalanceUsdc: '0',
      postPositionSize: (1n * 10n ** 18n).toString(),
      postPositionMarginUsdc: (1n * USDC).toString(),
    }),
  ])('rejects missing, malformed, legacy, and inconsistent receipt evidence', (value) => {
    expect(derivePerpsCloseReconciliation(value)).toBeUndefined()
  })
})
