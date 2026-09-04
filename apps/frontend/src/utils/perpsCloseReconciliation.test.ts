import { describe, expect, it } from 'vitest'
import type { PerpsOrderReceiptEconomics } from '../hooks/usePerpsHistory'
import { derivePerpsCloseReconciliation } from './perpsCloseReconciliation'

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
    actionChargeAssessedUsdc: (6n * USDC).toString(),
    actionChargeCollectedUsdc: (6n * USDC).toString(),
    grossAccountDebitUsdc: (7n * USDC).toString(),
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
      actionChargeAssessedUsdc: (4n * USDC).toString(),
      actionChargeCollectedUsdc: (4n * USDC).toString(),
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
      actionChargeAssessedUsdc: (11n * USDC).toString(),
      actionChargeCollectedUsdc: (11n * USDC).toString(),
      postSettlementBalanceUsdc: (118n * USDC).toString(),
    }))
    expect(fullyPaid).toMatchObject({
      frozenSpreadAssessedUsdc: 5n * USDC,
      frozenSpreadPaidUsdc: 5n * USDC,
      frozenSpreadWaivedUsdc: 0n,
      netCloseResultUsdc: 18n * USDC,
    })

    const partiallyWaived = derivePerpsCloseReconciliation(receipt({
      realizedPnlUsdc: (-92n * USDC).toString(),
      frozenSpreadUsdc: (5n * USDC).toString(),
      actionChargeAssessedUsdc: (11n * USDC).toString(),
      actionChargeCollectedUsdc: (8n * USDC).toString(),
      preSettlementBalanceUsdc: (100n * USDC).toString(),
      postSettlementBalanceUsdc: '0',
    }))
    expect(partiallyWaived).toMatchObject({
      frozenSpreadAssessedUsdc: 5n * USDC,
      frozenSpreadPaidUsdc: 2n * USDC,
      frozenSpreadWaivedUsdc: 3n * USDC,
      netCloseResultUsdc: -101n * USDC,
      uncoveredLossUsdc: 1n * USDC,
    })
    expect(
      partiallyWaived!.frozenSpreadPaidUsdc + partiallyWaived!.frozenSpreadWaivedUsdc
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
    undefined,
    receipt({ executionBountyUsdc: undefined }),
    receipt({ realizedPnlUsdc: undefined }),
    receipt({ realizedPnlUsdc: 'not-a-number' }),
    receipt({ actionChargeCollectedUsdc: (7n * USDC).toString() }),
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
