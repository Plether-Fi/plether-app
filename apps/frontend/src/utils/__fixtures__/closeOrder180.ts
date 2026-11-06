import type { PerpsOrderReceiptEconomics } from '../../hooks/usePerpsHistory'

// Core v2.0.0 Arbitrum Sepolia order 180, executed 2026-11-06.
// OrderFinalized in transaction
// 0x0c2a03a4e6e92ee5ea898f711a0b855907f51f9640e0dd383b95d1523a97c715.
// commitment.carryCollectedAssets is 70,278 atoms; economics.carryAssets
// includes that debit, while preSettlementBalanceAssets is already after it.
export const closeOrder180Receipt = {
  executionNotionalAssets: '1206377452451',
  executionBountyAssets: '0',
  realizedPnlAssets: '16352426',
  vpiAssets: '4636317700',
  carryAssets: '70938',
  commitmentCarryAssets: '70278',
  executionFeeAssets: '482550980',
  frozenSpreadAssets: '0',
  actionChargeAssessedAssets: '5118939618',
  actionChargeCollectedAssets: '5102587192',
  grossAccountDebitAssets: '5102587192',
  preSettlementBalanceAssets: '108307607989',
  postSettlementBalanceAssets: '103205091075',
  preTraderClaimBalanceAssets: '0',
  postTraderClaimBalanceAssets: '0',
  postPositionSize: '1252200000000000000000000',
  postPositionMarginAssets: '48312820667',
  postPositionEquityAssets: '48329174399',
  postLeverageBps: '249637',
  totalPositionVpiAssets: '-4636687985',
  positionLifetimeAccountAdjustmentAssets: '-70278',
  positionLifetimeNetResultAssets: '3205091075',
  positionLifetimeTradesResultAssets: '3205161353',
} satisfies PerpsOrderReceiptEconomics
