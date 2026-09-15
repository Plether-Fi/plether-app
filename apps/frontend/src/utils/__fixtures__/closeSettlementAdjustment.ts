import type { PerpsOrderReceiptEconomics } from '../../hooks/usePerpsHistory'

// Synthetic receipt matching the screenshot's rounded outcome, not onchain evidence.
export const closeSettlementAdjustmentReceipt: PerpsOrderReceiptEconomics = {
  executionNotionalUsdc: '15521922030000',
  executionBountyUsdc: '200000',
  realizedPnlUsdc: '-7270734000',
  vpiUsdc: '-251810000',
  carryUsdc: '204000',
  executionFeeUsdc: '6208770000',
  frozenSpreadUsdc: '0',
  actionChargeAssessedUsdc: '5957164000',
  actionChargeCollectedUsdc: '204000',
  grossAccountDebitUsdc: '7271138000',
  preSettlementBalanceUsdc: '7271138000',
  postSettlementBalanceUsdc: '0',
  preTraderClaimBalanceUsdc: '0',
  postTraderClaimBalanceUsdc: '0',
  postPositionSize: '0',
  postPositionMarginUsdc: '0',
  postPositionEquityUsdc: '0',
  postLeverageBps: '0',
}
