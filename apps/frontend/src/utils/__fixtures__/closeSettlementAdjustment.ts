import type { PerpsOrderReceiptEconomics } from '../../hooks/usePerpsHistory'

// Synthetic receipt matching the screenshot's rounded outcome, not onchain evidence.
export const closeSettlementAdjustmentReceipt: PerpsOrderReceiptEconomics = {
  executionNotionalAssets: '15521922030000',
  executionBountyAssets: '200000',
  realizedPnlAssets: '-7270734000',
  vpiAssets: '-251810000',
  carryAssets: '204000',
  executionFeeAssets: '6208770000',
  frozenSpreadAssets: '0',
  actionChargeAssessedAssets: '5957164000',
  actionChargeCollectedAssets: '204000',
  grossAccountDebitAssets: '7271138000',
  preSettlementBalanceAssets: '7271138000',
  postSettlementBalanceAssets: '0',
  preTraderClaimBalanceAssets: '0',
  postTraderClaimBalanceAssets: '0',
  postPositionSize: '0',
  postPositionMarginAssets: '0',
  postPositionEquityAssets: '0',
  postLeverageBps: '0',
}
