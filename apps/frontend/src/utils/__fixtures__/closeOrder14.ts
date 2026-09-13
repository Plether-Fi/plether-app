import type { PerpsOrderReceiptEconomics } from '../../hooks/usePerpsHistory'

// Core v1.2.3 Sepolia order 14, executed 2026-09-13. Public finalized receipt
// economics only: a VPI credit offsets charges and 8,657 units of PnL are withheld.
export const closeOrder14Receipt = {
  actionChargeAssessedUsdc: '4539782',
  actionChargeCollectedUsdc: '4531125',
  carryUsdc: '34',
  executionBountyUsdc: '200000',
  executionFeeUsdc: '436444',
  executionNotionalUsdc: '1091112451',
  frozenSpreadUsdc: '5455562',
  grossAccountDebitUsdc: '4731125',
  postLeverageBps: '49946',
  postPositionEquityUsdc: '4091179534',
  postPositionMarginUsdc: '4091017412',
  postPositionSize: '20600000000000000000000',
  postSettlementBalanceUsdc: '11050221384',
  postTraderClaimBalanceUsdc: '0',
  preSettlementBalanceUsdc: '11054952509',
  preTraderClaimBalanceUsdc: '0',
  realizedPnlUsdc: '8657',
  vpiUsdc: '-1352258',
} satisfies PerpsOrderReceiptEconomics
