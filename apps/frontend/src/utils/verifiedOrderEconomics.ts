import type { OrderReceiptV5 } from '@plether-fi/perps-aa-client'
import type { PerpsOrderHistoryRow } from '../hooks/usePerpsHistory'

/**
 * Project an already authenticated SDK receipt into the existing history shape.
 * Call only after decodeVerifiedOrderFinalized and canonical receipt verification.
 * Lifetime aggregates and traced oracle evidence remain the indexer's concern.
 */
export function economicsFromVerifiedOrderReceipt(
  receipt: OrderReceiptV5,
): Pick<PerpsOrderHistoryRow, 'receiptEconomics' | 'vpiAssetsRaw' | 'frozenCloseSpreadAssetsRaw' | 'executionEconomicsVersion'> {
  const economics = receipt.economics
  return {
    receiptEconomics: {
      executionNotionalAssets: economics.executionNotionalAssets.toString(),
      executionBountyAssets: receipt.bountyAssets.toString(),
      realizedPnlAssets: economics.realizedPnlAssets.toString(),
      vpiAssets: economics.vpiAssets.toString(),
      carryAssets: economics.carryAssets.toString(),
      commitmentCarryAssets: receipt.commitment.carryCollectedAssets.toString(),
      executionFeeAssets: economics.executionFeeAssets.toString(),
      frozenSpreadAssets: economics.frozenSpreadAssets.toString(),
      actionChargeAssessedAssets: economics.actionChargeAssessedAssets.toString(),
      actionChargeCollectedAssets: economics.actionChargeCollectedAssets.toString(),
      grossAccountDebitAssets: economics.grossAccountDebitAssets.toString(),
      preSettlementBalanceAssets: economics.preSettlementBalanceAssets.toString(),
      postSettlementBalanceAssets: economics.postSettlementBalanceAssets.toString(),
      preTraderClaimBalanceAssets: economics.preTraderClaimBalanceAssets.toString(),
      postTraderClaimBalanceAssets: economics.postTraderClaimBalanceAssets.toString(),
      postPositionSize: economics.postPositionSize.toString(),
      postPositionMarginAssets: economics.postPositionMarginAssets.toString(),
      postPositionEquityAssets: economics.postPositionEquityAssets.toString(),
      postLeverageBps: economics.postLeverageBps.toString(),
    },
    vpiAssetsRaw: economics.vpiAssets,
    frozenCloseSpreadAssetsRaw: economics.frozenSpreadAssets,
    // Existing UI receipt-economics shape version, not the V5 hashing domain.
    executionEconomicsVersion: 2,
  }
}
