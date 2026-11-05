import type { PerpsOrderReceiptEconomics } from '../hooks/usePerpsHistory'

export interface PerpsCloseReconciliation {
  executionNotionalAssets: bigint
  executionBountyAssets: bigint
  realizedPnlAssets: bigint
  vpiAssets: bigint
  totalPositionVpiAssets?: bigint
  lifetime?: {
    netResultAssets: bigint
    previousTradesResultAssets: bigint
    betweenTradesAccountChangeAssets: bigint
  }
  carryAssets: bigint
  executionFeeAssets: bigint
  frozenSpreadAssessedAssets: bigint
  frozenSpreadChargedAssets: bigint
  frozenSpreadWaivedAssets: bigint
  netCloseResultAssets: bigint
  actualAccountChangeAssets: bigint
  marginAccountChangeAssets: bigint
  traderClaimChangeAssets: bigint
  settlementAdjustmentAssets: bigint
  postPositionSize: bigint
  postPositionMarginAssets: bigint
  releasedPositionMarginAssets?: bigint
}

export interface PerpsCloseReconciliationOptions {
  preExecutionPositionMarginAssets?: bigint
}

function parseReceiptInteger(value: string | undefined): bigint | undefined {
  if (value === undefined || value === '' || !/^-?\d+$/.test(value)) return undefined
  try {
    return BigInt(value)
  } catch {
    return undefined
  }
}

function clamp(value: bigint, minimum: bigint, maximum: bigint): bigint {
  if (value < minimum) return minimum
  if (value > maximum) return maximum
  return value
}

/**
 * Builds an exact executed-close reconciliation from receipt evidence.
 * Returns undefined for legacy, incomplete, malformed, or inconsistent receipts.
 */
export function derivePerpsCloseReconciliation(
  receipt: PerpsOrderReceiptEconomics | undefined,
  options: PerpsCloseReconciliationOptions = {}
): PerpsCloseReconciliation | undefined {
  if (receipt === undefined) return undefined

  const executionNotionalAssets = parseReceiptInteger(receipt.executionNotionalAssets)
  const executionBountyAssets = parseReceiptInteger(receipt.executionBountyAssets)
  const realizedPnlAssets = parseReceiptInteger(receipt.realizedPnlAssets)
  const vpiAssets = parseReceiptInteger(receipt.vpiAssets)
  const carryAssets = parseReceiptInteger(receipt.carryAssets)
  const executionFeeAssets = parseReceiptInteger(receipt.executionFeeAssets)
  const frozenSpreadAssessedAssets = parseReceiptInteger(receipt.frozenSpreadAssets)
  const actionChargeAssessedAssets = parseReceiptInteger(receipt.actionChargeAssessedAssets)
  const actionChargeCollectedAssets = parseReceiptInteger(receipt.actionChargeCollectedAssets)
  const grossAccountDebitAssets = parseReceiptInteger(receipt.grossAccountDebitAssets)
  const preSettlementBalanceAssets = parseReceiptInteger(receipt.preSettlementBalanceAssets)
  const postSettlementBalanceAssets = parseReceiptInteger(receipt.postSettlementBalanceAssets)
  const preTraderClaimBalanceAssets = parseReceiptInteger(receipt.preTraderClaimBalanceAssets)
  const postTraderClaimBalanceAssets = parseReceiptInteger(receipt.postTraderClaimBalanceAssets)
  const postPositionSize = parseReceiptInteger(receipt.postPositionSize)
  const postPositionMarginAssets = parseReceiptInteger(receipt.postPositionMarginAssets)
  const postPositionEquityAssets = parseReceiptInteger(receipt.postPositionEquityAssets)
  const postLeverageBps = parseReceiptInteger(receipt.postLeverageBps)

  if (
    executionNotionalAssets === undefined ||
    executionBountyAssets === undefined ||
    realizedPnlAssets === undefined ||
    vpiAssets === undefined ||
    carryAssets === undefined ||
    executionFeeAssets === undefined ||
    frozenSpreadAssessedAssets === undefined ||
    actionChargeAssessedAssets === undefined ||
    actionChargeCollectedAssets === undefined ||
    grossAccountDebitAssets === undefined ||
    preSettlementBalanceAssets === undefined ||
    postSettlementBalanceAssets === undefined ||
    preTraderClaimBalanceAssets === undefined ||
    postTraderClaimBalanceAssets === undefined ||
    postPositionSize === undefined ||
    postPositionMarginAssets === undefined ||
    postPositionEquityAssets === undefined ||
    postLeverageBps === undefined
  ) {
    return undefined
  }

  const unsignedValues = [
    executionNotionalAssets,
    executionBountyAssets,
    carryAssets,
    executionFeeAssets,
    frozenSpreadAssessedAssets,
    actionChargeAssessedAssets,
    actionChargeCollectedAssets,
    grossAccountDebitAssets,
    preSettlementBalanceAssets,
    postSettlementBalanceAssets,
    preTraderClaimBalanceAssets,
    postTraderClaimBalanceAssets,
    postPositionSize,
    postPositionMarginAssets,
    postLeverageBps,
  ]
  if (unsignedValues.some((value) => value < 0n)) return undefined
  if (executionNotionalAssets === 0n) return undefined
  if (executionBountyAssets > grossAccountDebitAssets) return undefined
  // Core v1.2.3 _buildCloseAssessment reports net charges, not gross spread.
  // Negative VPI can offset spread, fees and carry. Already-realized carry can
  // still be collected even when the remaining action is a rebate.
  const netActionChargeAssets = vpiAssets + carryAssets + executionFeeAssets + frozenSpreadAssessedAssets
  if (actionChargeAssessedAssets !== (netActionChargeAssets > 0n ? netActionChargeAssets : 0n)) {
    return undefined
  }
  if (actionChargeCollectedAssets > (
    actionChargeAssessedAssets > carryAssets ? actionChargeAssessedAssets : carryAssets
  )) return undefined
  if (
    postPositionSize === 0n &&
    (postPositionMarginAssets !== 0n || postPositionEquityAssets !== 0n || postLeverageBps !== 0n)
  ) {
    return undefined
  }
  if (postPositionSize > 0n && postPositionEquityAssets < 0n) return undefined

  // Collection excludes charges withheld from positive price PnL. The gap is
  // a waiver only after accounting for that withholding. Realized carry is
  // present in both receipt charge fields and cancels out in this difference.
  const uncollectedChargeAssets = actionChargeAssessedAssets - actionChargeCollectedAssets
    - (realizedPnlAssets > 0n ? realizedPnlAssets : 0n)
  const effectiveSpreadAssets = clamp(
    frozenSpreadAssessedAssets + (vpiAssets < 0n ? vpiAssets : 0n),
    0n,
    frozenSpreadAssessedAssets
  )
  const frozenSpreadWaivedAssets = clamp(uncollectedChargeAssets, 0n, effectiveSpreadAssets)
  // Economic charge, NOT Core's cash-revenue `frozenSpreadPaidAssets`: VPI is
  // displayed separately below, so subtracting the rebate here would count it twice.
  const frozenSpreadChargedAssets = frozenSpreadAssessedAssets - frozenSpreadWaivedAssets
  const netCloseResultAssets = realizedPnlAssets
    - vpiAssets
    - carryAssets
    - executionFeeAssets
    - frozenSpreadChargedAssets
    - executionBountyAssets
  const marginAccountChangeAssets = postSettlementBalanceAssets - preSettlementBalanceAssets
  const traderClaimChangeAssets = postTraderClaimBalanceAssets - preTraderClaimBalanceAssets
  const actualAccountChangeAssets = marginAccountChangeAssets + traderClaimChangeAssets
  const settlementAdjustmentAssets = actualAccountChangeAssets - netCloseResultAssets

  if (netCloseResultAssets >= 0n) {
    if (
      actualAccountChangeAssets !== netCloseResultAssets ||
      marginAccountChangeAssets < 0n ||
      traderClaimChangeAssets < 0n ||
      (marginAccountChangeAssets > 0n && traderClaimChangeAssets > 0n)
    ) {
      return undefined
    }
  } else {
    if (
      settlementAdjustmentAssets < 0n ||
      marginAccountChangeAssets > 0n ||
      traderClaimChangeAssets > 0n ||
      (settlementAdjustmentAssets > 0n && postPositionSize !== 0n)
    ) {
      return undefined
    }
  }

  const lifetimeNet = parseReceiptInteger(receipt.positionLifetimeNetResultAssets)
  const lifetimeTrades = parseReceiptInteger(receipt.positionLifetimeTradesResultAssets)
  const lifetimeAdjustment = parseReceiptInteger(receipt.positionLifetimeAccountAdjustmentAssets)
  const lifetime = lifetimeNet !== undefined && lifetimeTrades !== undefined && lifetimeAdjustment !== undefined
    && lifetimeNet === lifetimeTrades + lifetimeAdjustment
    ? {
        netResultAssets: lifetimeNet,
        previousTradesResultAssets: lifetimeTrades - actualAccountChangeAssets,
        betweenTradesAccountChangeAssets: lifetimeAdjustment,
      }
    : undefined

  const preExecutionPositionMarginAssets = options.preExecutionPositionMarginAssets
  const releasedPositionMarginAssets =
    preExecutionPositionMarginAssets !== undefined &&
    preExecutionPositionMarginAssets >= 0n &&
    preExecutionPositionMarginAssets >= postPositionMarginAssets
      ? preExecutionPositionMarginAssets - postPositionMarginAssets
      : undefined

  return {
    executionNotionalAssets,
    executionBountyAssets,
    realizedPnlAssets,
    vpiAssets,
    totalPositionVpiAssets: parseReceiptInteger(receipt.totalPositionVpiAssets),
    lifetime,
    carryAssets,
    executionFeeAssets,
    frozenSpreadAssessedAssets,
    frozenSpreadChargedAssets,
    frozenSpreadWaivedAssets,
    netCloseResultAssets,
    actualAccountChangeAssets,
    marginAccountChangeAssets,
    traderClaimChangeAssets,
    settlementAdjustmentAssets,
    postPositionSize,
    postPositionMarginAssets,
    releasedPositionMarginAssets,
  }
}
