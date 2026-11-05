import type { PerpsOrderReceiptEconomics } from '../../hooks/usePerpsHistory'

// Core v1.2.3 Sepolia order 14, executed 2026-09-13. Public finalized receipt
// economics only: a VPI credit offsets charges and 8,657 units of PnL are withheld.
export const closeOrder14Receipt = {
  actionChargeAssessedAssets: '4539782',
  actionChargeCollectedAssets: '4531125',
  carryAssets: '34',
  executionBountyAssets: '200000',
  executionFeeAssets: '436444',
  executionNotionalAssets: '1091112451',
  frozenSpreadAssets: '5455562',
  grossAccountDebitAssets: '4731125',
  postLeverageBps: '49946',
  postPositionEquityAssets: '4091179534',
  postPositionMarginAssets: '4091017412',
  postPositionSize: '20600000000000000000000',
  postSettlementBalanceAssets: '11050221384',
  postTraderClaimBalanceAssets: '0',
  preSettlementBalanceAssets: '11054952509',
  preTraderClaimBalanceAssets: '0',
  realizedPnlAssets: '8657',
  vpiAssets: '-1352258',
} satisfies PerpsOrderReceiptEconomics
