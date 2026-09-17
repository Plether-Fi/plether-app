import type { PerpsOrderReceiptEconomics } from '../hooks/usePerpsHistory'

export interface PerpsCloseReconciliation {
  executionNotionalUsdc: bigint
  executionBountyUsdc: bigint
  realizedPnlUsdc: bigint
  vpiUsdc: bigint
  carryUsdc: bigint
  executionFeeUsdc: bigint
  frozenSpreadAssessedUsdc: bigint
  frozenSpreadChargedUsdc: bigint
  frozenSpreadWaivedUsdc: bigint
  netCloseResultUsdc: bigint
  actualAccountChangeUsdc: bigint
  marginAccountChangeUsdc: bigint
  traderClaimChangeUsdc: bigint
  settlementAdjustmentUsdc: bigint
  postPositionSize: bigint
  postPositionMarginUsdc: bigint
  releasedPositionMarginUsdc?: bigint
}

export interface PerpsCloseReconciliationOptions {
  preExecutionPositionMarginUsdc?: bigint
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

  const executionNotionalUsdc = parseReceiptInteger(receipt.executionNotionalUsdc)
  const executionBountyUsdc = parseReceiptInteger(receipt.executionBountyUsdc)
  const realizedPnlUsdc = parseReceiptInteger(receipt.realizedPnlUsdc)
  const vpiUsdc = parseReceiptInteger(receipt.vpiUsdc)
  const carryUsdc = parseReceiptInteger(receipt.carryUsdc)
  const executionFeeUsdc = parseReceiptInteger(receipt.executionFeeUsdc)
  const frozenSpreadAssessedUsdc = parseReceiptInteger(receipt.frozenSpreadUsdc)
  const actionChargeAssessedUsdc = parseReceiptInteger(receipt.actionChargeAssessedUsdc)
  const actionChargeCollectedUsdc = parseReceiptInteger(receipt.actionChargeCollectedUsdc)
  const grossAccountDebitUsdc = parseReceiptInteger(receipt.grossAccountDebitUsdc)
  const preSettlementBalanceUsdc = parseReceiptInteger(receipt.preSettlementBalanceUsdc)
  const postSettlementBalanceUsdc = parseReceiptInteger(receipt.postSettlementBalanceUsdc)
  const preTraderClaimBalanceUsdc = parseReceiptInteger(receipt.preTraderClaimBalanceUsdc)
  const postTraderClaimBalanceUsdc = parseReceiptInteger(receipt.postTraderClaimBalanceUsdc)
  const postPositionSize = parseReceiptInteger(receipt.postPositionSize)
  const postPositionMarginUsdc = parseReceiptInteger(receipt.postPositionMarginUsdc)
  const postPositionEquityUsdc = parseReceiptInteger(receipt.postPositionEquityUsdc)
  const postLeverageBps = parseReceiptInteger(receipt.postLeverageBps)

  if (
    executionNotionalUsdc === undefined ||
    executionBountyUsdc === undefined ||
    realizedPnlUsdc === undefined ||
    vpiUsdc === undefined ||
    carryUsdc === undefined ||
    executionFeeUsdc === undefined ||
    frozenSpreadAssessedUsdc === undefined ||
    actionChargeAssessedUsdc === undefined ||
    actionChargeCollectedUsdc === undefined ||
    grossAccountDebitUsdc === undefined ||
    preSettlementBalanceUsdc === undefined ||
    postSettlementBalanceUsdc === undefined ||
    preTraderClaimBalanceUsdc === undefined ||
    postTraderClaimBalanceUsdc === undefined ||
    postPositionSize === undefined ||
    postPositionMarginUsdc === undefined ||
    postPositionEquityUsdc === undefined ||
    postLeverageBps === undefined
  ) {
    return undefined
  }

  const unsignedValues = [
    executionNotionalUsdc,
    executionBountyUsdc,
    carryUsdc,
    executionFeeUsdc,
    frozenSpreadAssessedUsdc,
    actionChargeAssessedUsdc,
    actionChargeCollectedUsdc,
    grossAccountDebitUsdc,
    preSettlementBalanceUsdc,
    postSettlementBalanceUsdc,
    preTraderClaimBalanceUsdc,
    postTraderClaimBalanceUsdc,
    postPositionSize,
    postPositionMarginUsdc,
    postLeverageBps,
  ]
  if (unsignedValues.some((value) => value < 0n)) return undefined
  if (executionNotionalUsdc === 0n) return undefined
  if (executionBountyUsdc > grossAccountDebitUsdc) return undefined
  // Core v1.2.3 _buildCloseAssessment reports net charges, not gross spread.
  // Negative VPI can offset spread, fees and carry. Already-realized carry can
  // still be collected even when the remaining action is a rebate.
  const netActionChargeUsdc = vpiUsdc + carryUsdc + executionFeeUsdc + frozenSpreadAssessedUsdc
  if (actionChargeAssessedUsdc !== (netActionChargeUsdc > 0n ? netActionChargeUsdc : 0n)) {
    return undefined
  }
  if (actionChargeCollectedUsdc > (
    actionChargeAssessedUsdc > carryUsdc ? actionChargeAssessedUsdc : carryUsdc
  )) return undefined
  if (
    postPositionSize === 0n &&
    (postPositionMarginUsdc !== 0n || postPositionEquityUsdc !== 0n || postLeverageBps !== 0n)
  ) {
    return undefined
  }
  if (postPositionSize > 0n && postPositionEquityUsdc < 0n) return undefined

  // Collection excludes charges withheld from positive price PnL. The gap is
  // a waiver only after accounting for that withholding. Realized carry is
  // present in both receipt charge fields and cancels out in this difference.
  const uncollectedChargeUsdc = actionChargeAssessedUsdc - actionChargeCollectedUsdc
    - (realizedPnlUsdc > 0n ? realizedPnlUsdc : 0n)
  const effectiveSpreadUsdc = clamp(
    frozenSpreadAssessedUsdc + (vpiUsdc < 0n ? vpiUsdc : 0n),
    0n,
    frozenSpreadAssessedUsdc
  )
  const frozenSpreadWaivedUsdc = clamp(uncollectedChargeUsdc, 0n, effectiveSpreadUsdc)
  // Economic charge, NOT Core's cash-revenue `frozenSpreadPaidUsdc`: VPI is
  // displayed separately below, so subtracting the rebate here would count it twice.
  const frozenSpreadChargedUsdc = frozenSpreadAssessedUsdc - frozenSpreadWaivedUsdc
  const netCloseResultUsdc = realizedPnlUsdc
    - vpiUsdc
    - carryUsdc
    - executionFeeUsdc
    - frozenSpreadChargedUsdc
    - executionBountyUsdc
  const marginAccountChangeUsdc = postSettlementBalanceUsdc - preSettlementBalanceUsdc
  const traderClaimChangeUsdc = postTraderClaimBalanceUsdc - preTraderClaimBalanceUsdc
  const actualAccountChangeUsdc = marginAccountChangeUsdc + traderClaimChangeUsdc
  const settlementAdjustmentUsdc = actualAccountChangeUsdc - netCloseResultUsdc

  if (netCloseResultUsdc >= 0n) {
    if (
      actualAccountChangeUsdc !== netCloseResultUsdc ||
      marginAccountChangeUsdc < 0n ||
      traderClaimChangeUsdc < 0n ||
      (marginAccountChangeUsdc > 0n && traderClaimChangeUsdc > 0n)
    ) {
      return undefined
    }
  } else {
    if (
      settlementAdjustmentUsdc < 0n ||
      marginAccountChangeUsdc > 0n ||
      traderClaimChangeUsdc > 0n ||
      (settlementAdjustmentUsdc > 0n && postPositionSize !== 0n)
    ) {
      return undefined
    }
  }

  const preExecutionPositionMarginUsdc = options.preExecutionPositionMarginUsdc
  const releasedPositionMarginUsdc =
    preExecutionPositionMarginUsdc !== undefined &&
    preExecutionPositionMarginUsdc >= 0n &&
    preExecutionPositionMarginUsdc >= postPositionMarginUsdc
      ? preExecutionPositionMarginUsdc - postPositionMarginUsdc
      : undefined

  return {
    executionNotionalUsdc,
    executionBountyUsdc,
    realizedPnlUsdc,
    vpiUsdc,
    carryUsdc,
    executionFeeUsdc,
    frozenSpreadAssessedUsdc,
    frozenSpreadChargedUsdc,
    frozenSpreadWaivedUsdc,
    netCloseResultUsdc,
    actualAccountChangeUsdc,
    marginAccountChangeUsdc,
    traderClaimChangeUsdc,
    settlementAdjustmentUsdc,
    postPositionSize,
    postPositionMarginUsdc,
    releasedPositionMarginUsdc,
  }
}
